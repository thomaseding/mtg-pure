{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.ActivateCast (
  activateAbility,
  castSpell,
) where

import safe Control.Exception (assert)
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Util (AndLike (..))
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd.Api (
  doesZoneObjectExist,
  getHasPriority,
  getPlayer,
  newObjectId,
  pay,
  performElections,
 )
import safe MtgPure.Engine.Legality (Legality (..), legalityToMaybe, maybeToLegality)
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromRO,
  get,
  gets,
  modify,
 )
import safe MtgPure.Engine.Prompt (
  ActivateAbility,
  CastSpell,
  InvalidCastSpell (..),
  PriorityAction (ActivateAbility, CastSpell),
  Prompt' (..),
  SomeActivatedAbility (..),
 )
import safe MtgPure.Engine.State (
  AnyElected (..),
  Elected (..),
  GameState (..),
  Magic,
  Pending,
  PendingReady (..),
  electedObject_controller,
  electedObject_cost,
  logCall,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsCardList (containsCard)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTKind (
  OTInstant,
  OTSorcery,
  OTSpell,
 )
import safe MtgPure.Model.Object.OTN (OT0, OT1)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (
  ObjectId,
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectN (ObjectN (O1))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.ToObjectN.Classes (ToObject2' (..), ToObject6' (..))
import safe MtgPure.Model.Object.VisitObjectN (VisitObjectN (promoteIdToObjectN))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  AnyCard (..),
  Card (..),
  CardFacet (..),
  Cost (..),
  Effect (..),
  Elect (..),
  IsSpecificCard (singSpecificCard),
  SpecificCard (..),
  WithThis (..),
  WithThisActivated,
  WithThisOneShot,
  YourCard (..),
 )
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (asCard, oToZO1, toZO0)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZO, ZOPlayer, ZoneObject (..))

type Legality' = Maybe ()

newtype ActivateAbilityReqs = ActivateAbilityReqs
  { activateAbilityReqs_hasPriority :: Bool
  }
  deriving (Eq, Ord, Show, Typeable)

-- Unfortunately pattern synonyms won't contribute to exhaustiveness checking.
-- pattern ActivateAbilityReqs_Satisfied :: ActivateAbilityReqs
-- pattern ActivateAbilityReqs_Satisfied =
--   ActivateAbilityReqs
--     { activateAbilityReqs_hasPriority = True
--     }

getActivateAbilityReqs :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m ActivateAbilityReqs
getActivateAbilityReqs oPlayer = logCall 'getActivateAbilityReqs do
  hasPriority <- fromPublic $ getHasPriority oPlayer
  pure
    ActivateAbilityReqs
      { activateAbilityReqs_hasPriority = hasPriority -- (116.2a)
      }

newtype CastSpellReqs = CastSpellReqs
  { castSpellReqs_hasPriority :: Bool
  }
  deriving (Eq, Ord, Show, Typeable)

-- Unfortunately pattern synonyms won't contribute to exhaustiveness checking.
pattern CastSpellReqs_Satisfied :: CastSpellReqs
pattern CastSpellReqs_Satisfied =
  CastSpellReqs
    { castSpellReqs_hasPriority = True
    }

getCastSpellReqs :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m CastSpellReqs
getCastSpellReqs oPlayer = logCall 'getCastSpellReqs do
  hasPriority <- fromPublic $ getHasPriority oPlayer
  pure
    CastSpellReqs
      { castSpellReqs_hasPriority = hasPriority -- (116.2a)
      }

data CastMeta (ot :: Type) :: Type where
  CastMeta ::
    { castMeta_Elected ::
        Object 'OTPlayer ->
        Card ot ->
        CardFacet ot ->
        Cost ot ->
        PendingReady 'Pre (Effect 'OneShot) ot ->
        Elected 'Pre ot
    , castMeta_effect :: CardFacet ot -> WithThisOneShot ot
    , castMeta_cost :: CardFacet ot -> Cost ot
    } ->
    CastMeta ot

instantCastMeta :: CastMeta OTInstant
instantCastMeta =
  CastMeta
    { castMeta_Elected = ElectedInstant
    , castMeta_effect = instant_effect
    , castMeta_cost = instant_cost
    }

sorceryCastMeta :: CastMeta OTSorcery
sorceryCastMeta =
  CastMeta
    { castMeta_Elected = ElectedSorcery
    , castMeta_effect = sorcery_effect
    , castMeta_cost = sorcery_cost
    }

lensedThis :: (IsZone zone, IsObjectType a) => ObjectId -> ZO zone (OT1 a)
lensedThis = ZO singZone . O1 . idToObject . UntypedObject DefaultObjectDiscriminant

castSpell :: forall m. Monad m => Object 'OTPlayer -> PriorityAction CastSpell -> Magic 'Private 'RW m Legality
castSpell oCaster = logCall 'castSpell \case
  CastSpell zoSpell -> goSpell zoSpell
 where
  goSpell :: forall zone. IsZO zone OTSpell => ZO zone OTSpell -> Magic 'Private 'RW m Legality
  goSpell zoSpell = do
    st <- fromRO get
    reqs <- fromRO $ getCastSpellReqs oCaster
    player <- fromRO $ getPlayer oCaster
    let opaque = mkOpaqueGameState st
        prompt = magicPrompt st
        hand = playerHand player
        --zoCaster = oToZO1 oCaster
        --
        invalid :: (ZO zone OTSpell -> InvalidCastSpell) -> Magic 'Private 'RW m Legality
        invalid ex = do
          lift $ exceptionInvalidCastSpell prompt opaque oCaster $ ex zoSpell
          pure Illegal

    case reqs of
      CastSpellReqs{castSpellReqs_hasPriority = False} -> invalid CastSpell_NoPriority
      CastSpellReqs
        { castSpellReqs_hasPriority = True
        } -> assert (reqs == CastSpellReqs_Satisfied) case singZone @zone of
          SZBattlefield -> invalid undefined
          SZExile -> invalid undefined
          SZLibrary -> invalid undefined
          SZStack -> invalid undefined
          SZGraveyard -> invalid undefined
          SZHand -> do
            mCard <- fromRO $ gets $ Map.lookup (toZO0 zoSpell) . magicHandCards
            case mCard of
              Nothing -> invalid CastSpell_NotInZone
              Just anyCard -> do
                case containsCard (asCard zoSpell) hand of
                  False -> invalid CastSpell_NotOwned
                  True -> case anyCard of
                    AnyCard card -> case card of
                      Card{} -> castSpellCard oCaster card

castSpellCard ::
  forall ot m.
  Monad m =>
  Object 'OTPlayer ->
  Card ot ->
  Magic 'Private 'RW m Legality
castSpellCard oCaster card = logCall 'castSpellCard case card of
  Card _name yourCard -> goYourCard yourCard
 where
  goInvalid :: Magic 'Private 'RW m Legality
  goInvalid = do
    -- TODO: prompt error
    pure Illegal

  goYourCard :: IsSpecificCard ot => YourCard ot -> Magic 'Private 'RW m Legality
  goYourCard = \case
    YourArtifact{} -> goInvalid
    YourArtifactCreature{} -> goInvalid
    YourArtifactLand{} -> goInvalid
    YourCreature{} -> goInvalid
    YourEnchantment{} -> goInvalid
    YourEnchantmentCreature{} -> goInvalid
    YourLand{} -> goInvalid
    YourPlaneswalker{} -> goInvalid
    --
    YourInstant cont -> goSpell cont
    YourSorcery cont -> goSpell cont

  goSpell ::
    IsSpecificCard ot =>
    (ZOPlayer -> Elect 'Pre (CardFacet ot) ot) ->
    Magic 'Private 'RW m Legality
  goSpell casterToElectFacet = do
    let electFacet = casterToElectFacet $ oToZO1 oCaster
    spellId <- newObjectId
    let zoSpell = toZO0 @ 'ZStack spellId

        goElectFacet :: Elect 'Pre (CardFacet ot) ot -> Magic 'Private 'RW m Legality
        goElectFacet elect = do
          seedStackEntryTargets zoSpell
          maybeToLegality <$> performElections zoSpell goFacet elect

        goFacet :: CardFacet ot -> Magic 'Private 'RW m Legality'
        goFacet facet = case facet of
          ArtifactLandFacet{} -> undefined -- TODO: Not a spell
          LandFacet{} -> undefined -- TODO: Not a spell
          --
          ArtifactFacet{} -> goFacet' facet undefined
          ArtifactCreatureFacet{} -> goFacet' facet undefined
          CreatureFacet{} -> goFacet' facet undefined
          EnchantmentFacet{} -> goFacet' facet undefined
          EnchantmentCreatureFacet{} -> goFacet' facet undefined
          PlaneswalkerFacet{} -> goFacet' facet undefined
          --
          InstantFacet{} -> goFacet' facet instantCastMeta
          SorceryFacet{} -> goFacet' facet sorceryCastMeta

        goFacet' :: CardFacet ot -> CastMeta ot -> Magic 'Private 'RW m Legality'
        goFacet' facet meta = playPendingSpell
          zoSpell
          (castMeta_cost meta facet)
          (castMeta_effect meta facet)
          \cost effect -> do
            case singSpecificCard @ot of
              InstantCard ->
                legalityToMaybe <$> do
                  payElectedAndPutOnStack @ 'Cast zoSpell $
                    castMeta_Elected meta oCaster card facet cost effect
              SorceryCard ->
                legalityToMaybe <$> do
                  payElectedAndPutOnStack @ 'Cast zoSpell $
                    castMeta_Elected meta oCaster card facet cost effect
              _ -> undefined

    goElectFacet electFacet

-- TODO: Generalize for TriggeredAbility as well. Prolly make an AbilityMeta type that is analogous to CastMeta.
-- NOTE: A TriggeredAbility is basically the same as an ActivatedAbility that the game activates automatically.
-- TODO: This needs to validate ActivateAbilityReqs
activateAbility :: forall m. Monad m => Object 'OTPlayer -> PriorityAction ActivateAbility -> Magic 'Private 'RW m Legality
activateAbility oPlayer = logCall 'activateAbility \case
  ActivateAbility someActivated -> case someActivated of
    SomeActivatedAbility zoThis withThisActivated -> do
      goSomeActivatedAbility zoThis withThisActivated
 where
  logCall' s = logCall ('activateAbility, s :: String)

  goSomeActivatedAbility ::
    forall zone ot' ot.
    (IsZO zone ot', IsZO zone ot) =>
    ZO zone ot' ->
    WithThisActivated zone ot ->
    Magic 'Private 'RW m Legality
  goSomeActivatedAbility zoThis' withThisActivated = logCall' "goSomeActivatedAbility" do
    zoExists' <- fromRO $ doesZoneObjectExist zoThis'
    pure () -- TODO: Check that `zoThis'` actually has the `withThisActivated`
    _reqs <- fromRO $ getActivateAbilityReqs oPlayer -- TODO: validate reqs
    let oThis = promoteIdToObjectN @ot $ getObjectId zoThis'
        zoThis = ZO (singZone @zone) oThis
        thisId = getObjectId zoThis

        invalid :: m () -> Magic 'Private 'RW m Legality
        invalid complain = do
          () <- lift complain
          pure Illegal

        goWithThisActivated :: Magic 'Private 'RW m Legality
        goWithThisActivated = case withThisActivated of
          T1 thisToElectActivated -> do
            goThisToElectAbility thisToElectActivated (lensedThis thisId)
          T2 thisToElectActivated -> do
            goThisToElectAbility thisToElectActivated (lensedThis thisId, lensedThis thisId)

        goThisToElectAbility ::
          (this -> Elect 'Pre (ActivatedAbility zone ot) ot) ->
          this ->
          Magic 'Private 'RW m Legality
        goThisToElectAbility thisToElectAbility this = logCall' "goThisToElectAbility" do
          abilityId <- newObjectId
          let zoAbility = toZO0 @ 'ZStack abilityId
          goElectActivated zoAbility $ thisToElectAbility this

        goElectActivated :: ZO 'ZStack OT0 -> Elect 'Pre (ActivatedAbility zone ot) ot -> Magic 'Private 'RW m Legality
        goElectActivated zoAbility elect = logCall' "goElectedActivated" do
          seedStackEntryTargets zoAbility
          maybeToLegality <$> performElections zoAbility (goActivated zoAbility) elect

        goActivated :: ZO 'ZStack OT0 -> ActivatedAbility zone ot -> Magic 'Private 'RW m Legality'
        goActivated zoAbility activated = logCall' "goActivated" do
          legalityToMaybe <$> do
            let isController = True -- TODO
                abilityExists = True -- TODO
            prompt <- fromRO $ gets magicPrompt
            case (zoExists', isController, abilityExists) of
              (False, _, _) -> invalid $ exceptionZoneObjectDoesNotExist prompt zoThis'
              (_, False, _) -> invalid undefined
              (_, _, False) -> invalid undefined
              (True, True, True) ->
                maybeToLegality <$> do
                  playPendingAbility
                    zoAbility
                    (activated_cost activated)
                    (activated_effect activated)
                    \cost effect ->
                      legalityToMaybe <$> do
                        payElectedAndPutOnStack @ 'Activate zoAbility $
                          ElectedActivatedAbility oPlayer zoThis cost effect

    goWithThisActivated

playPendingSpell ::
  forall m ot x.
  (AndLike x, AndLike (Maybe x)) =>
  Monad m =>
  ZO 'ZStack OT0 ->
  Cost ot ->
  WithThisOneShot ot ->
  (Cost ot -> Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)) ->
  Magic 'Private 'RW m (Maybe x)
playPendingSpell _zoStack cost withThisElectEffect cont = logCall 'playPendingSpell do
  goWithThis withThisElectEffect
 where
  goWithThis = \case
    T1 thisToElectEffect -> do
      thisId <- newObjectId
      goElectEffect $ thisToElectEffect (lensedThis thisId)
    T2 thisToElectEffect -> do
      thisId <- newObjectId
      goElectEffect $ thisToElectEffect (lensedThis thisId, lensedThis thisId)

  goElectEffect = cont cost . Pending

playPendingAbility ::
  forall m ot x.
  (AndLike x, AndLike (Maybe x)) =>
  Monad m =>
  ZO 'ZStack OT0 ->
  Cost ot ->
  Elect 'Post (Effect 'OneShot) ot ->
  (Cost ot -> Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m (Maybe x)) ->
  Magic 'Private 'RW m (Maybe x)
playPendingAbility _zoStack cost electEffect cont = logCall 'playPendingAbility do
  cont cost $ Pending electEffect

data ActivateCast = Activate | Cast

class PayElected (ac :: ActivateCast) (ot :: Type) where
  payElectedAndPutOnStack :: Monad m => ZO 'ZStack OT0 -> Elected 'Pre ot -> Magic 'Private 'RW m Legality

instance PayElected 'Activate ot where
  payElectedAndPutOnStack zo =
    logCall 'payElectedAndPutOnStack $
      payElectedAndPutOnStack' zo $
        StackAbility . ZO SZStack . toObject2' . idToObject @ 'OTActivatedAbility . UntypedObject DefaultObjectDiscriminant

instance PayElected 'Cast OTInstant where
  payElectedAndPutOnStack zo =
    logCall 'payElectedAndPutOnStack $
      payElectedAndPutOnStack' zo $
        StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTInstant . UntypedObject DefaultObjectDiscriminant

instance PayElected 'Cast OTSorcery where
  payElectedAndPutOnStack zo =
    logCall 'payElectedAndPutOnStack $
      payElectedAndPutOnStack' zo $
        StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTSorcery . UntypedObject DefaultObjectDiscriminant

payElectedAndPutOnStack' ::
  forall ot m.
  Monad m =>
  ZO 'ZStack OT0 ->
  (ObjectId -> StackObject) ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStack' zoStack idToStackObject elected = logCall 'payElectedAndPutOnStack' do
  let stackId = getObjectId zoStack
      stackItem = idToStackObject stackId
  modify \st ->
    st
      { magicStack = Stack $ stackItem : unStack (magicStack st)
      , magicStackEntryElectedMap = Map.insert zoStack (AnyElected elected) $ magicStackEntryElectedMap st
      }
  legality <- pay (electedObject_controller elected) $ electedObject_cost elected
  case legality of
    Legal -> pure Legal
    Illegal -> do
      -- TODO: GC stack object stuff if Illegal
      -- XXX: GC actually might not be necessary since this is ultimately bracketed by rewindIllegal
      pure Illegal

seedStackEntryTargets :: Monad m => ZO 'ZStack OT0 -> Magic 'Private 'RW m ()
seedStackEntryTargets zoStack = logCall 'seedStackEntryTargets do
  modify \st ->
    st
      { magicStackEntryTargetsMap = Map.insert zoStack [] $ magicStackEntryTargetsMap st
      }
