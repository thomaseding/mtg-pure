{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Replace case with maybe" #-}

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
  requiresTargets,
  resolveOneShot,
 )
import safe MtgPure.Engine.Legality (Legality (..), legalityToMaybe, maybeToLegality)
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromRO,
  get,
  gets,
  internalFromRW,
  local,
  modify,
 )
import safe MtgPure.Engine.Prompt (
  ActivateAbility,
  CastSpell,
  EnactInfo (enactInfo_couldAddMana),
  InternalLogicError (..),
  InvalidCastSpell (..),
  PriorityAction (ActivateAbility, CastSpell),
  Prompt' (..),
  SomeActivatedAbility (..),
 )
import safe MtgPure.Engine.Resolve (resolveManaAbility)
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
  withHeadlessPrompt,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsCardList (containsCard)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTN (OT0, OT1, OTN)
import safe MtgPure.Model.Object.OTNAliases (
  OTArtifact,
  OTArtifactCreature,
  OTCreature,
  OTEnchantment,
  OTEnchantmentCreature,
  OTInstant,
  OTPlaneswalker,
  OTSorcery,
  OTSpell,
 )
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
  YourCardFacet (..),
 )
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (AsSpell', asCard, oToZO1, toZO0)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOT, IsZO, ZO, ZOPlayer, ZoneObject (..))

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
    { castMeta_effect :: Maybe (CardFacet ot -> WithThisOneShot ot)
    , castMeta_cost :: CardFacet ot -> Cost ot
    } ->
    CastMeta ot

artifactCastMeta :: CastMeta OTArtifact
artifactCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = artifact_cost
    }

artifactCreatureCastMeta :: CastMeta OTArtifactCreature
artifactCreatureCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = artifactCreature_cost
    }

creatureCastMeta :: CastMeta OTCreature
creatureCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = creature_cost
    }

enchantmentCastMeta :: CastMeta OTEnchantment
enchantmentCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = enchantment_cost
    }

enchantmentCreatureCastMeta :: CastMeta OTEnchantmentCreature
enchantmentCreatureCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = enchantmentCreature_cost
    }

instantCastMeta :: CastMeta OTInstant
instantCastMeta =
  CastMeta
    { castMeta_effect = Just instant_effect
    , castMeta_cost = instant_cost
    }

planeswalkerCastMeta :: CastMeta OTPlaneswalker
planeswalkerCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = planeswalker_cost
    }

sorceryCastMeta :: CastMeta OTSorcery
sorceryCastMeta =
  CastMeta
    { castMeta_effect = Just sorcery_effect
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
                    AnyCard1 card -> case card of
                      Card _name yourCard -> castYourSpellCard oCaster card yourCard
                    AnyCard2{} -> undefined

castYourSpellCard ::
  forall ot m x.
  (ot ~ OTN x, Monad m) =>
  Object 'OTPlayer ->
  Card ot ->
  YourCardFacet ot ->
  Magic 'Private 'RW m Legality
castYourSpellCard oCaster card = logCall 'castYourSpellCard \case
  YourLand{} -> goInvalid
  YourArtifactLand{} -> goInvalid
  --
  YourInstant cont -> goSpell cont
  YourSorcery cont -> goSpell cont
  --
  YourArtifact cont -> goSpell $ ElectCard . cont
  YourArtifactCreature cont -> goSpell $ ElectCard . cont
  YourCreature cont -> goSpell $ ElectCard . cont
  YourEnchantment cont -> goSpell $ ElectCard . cont
  YourEnchantmentCreature cont -> goSpell $ ElectCard . cont
  YourPlaneswalker cont -> goSpell $ ElectCard . cont
 where
  goInvalid :: Magic 'Private 'RW m Legality
  goInvalid = do
    -- TODO: prompt error
    pure Illegal

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
          ArtifactFacet{} -> goFacet' facet artifactCastMeta
          ArtifactCreatureFacet{} -> goFacet' facet artifactCreatureCastMeta
          CreatureFacet{} -> goFacet' facet creatureCastMeta
          EnchantmentFacet{} -> goFacet' facet enchantmentCastMeta
          EnchantmentCreatureFacet{} -> goFacet' facet enchantmentCreatureCastMeta
          InstantFacet{} -> goFacet' facet instantCastMeta
          PlaneswalkerFacet{} -> goFacet' facet planeswalkerCastMeta
          SorceryFacet{} -> goFacet' facet sorceryCastMeta

        goFacet' :: CardFacet ot -> CastMeta ot -> Magic 'Private 'RW m Legality'
        goFacet' facet meta = do
          let anyCard = AnyCard1 card
              goPay cost mEffect = do
                legalityToMaybe <$> case singSpecificCard @ot of
                  ArtifactLandCard{} -> error $ show CantHappenByConstruction
                  LandCard{} -> error $ show CantHappenByConstruction
                  --
                  ArtifactCard{} ->
                    payElectedAndPutOnStack @ 'Cast @ot zoSpell $
                      ElectedSpell oCaster anyCard facet cost mEffect
                  ArtifactCreatureCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoSpell $
                      ElectedSpell oCaster anyCard facet cost mEffect
                  CreatureCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoSpell $
                      ElectedSpell oCaster anyCard facet cost mEffect
                  EnchantmentCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoSpell $
                      ElectedSpell oCaster anyCard facet cost mEffect
                  EnchantmentCreatureCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoSpell $
                      ElectedSpell oCaster anyCard facet cost mEffect
                  InstantCard -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoSpell $
                      ElectedSpell oCaster anyCard facet cost mEffect
                  PlaneswalkerCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoSpell $
                      ElectedSpell oCaster anyCard facet cost mEffect
                  SorceryCard -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoSpell $
                      ElectedSpell oCaster anyCard facet cost mEffect
          case castMeta_effect meta of
            Just facetToEffect -> do
              playPendingOneShot zoSpell (castMeta_cost meta facet) (facetToEffect facet) goPay
            Nothing -> do
              playPendingPermanent zoSpell (castMeta_cost meta facet) goPay

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
    pure () -- TODO: Check that `zoThis'` actually has the `withThisActivated` (including intrinsic abilities)
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
          This1 thisToElectActivated -> do
            goThisToElectAbility thisToElectActivated (lensedThis thisId)
          This2 thisToElectActivated -> do
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
              (True, True, True) -> do
                let goPay cost effect = do
                      let elected = ElectedActivatedAbility oPlayer zoThis cost effect
                      fmap legalityToMaybe $
                        fromRO (isPendingManaEffect effect) >>= \case
                          True -> payElectedManaAbilityAndResolve elected
                          False -> payElectedAndPutOnStack @ 'Activate @ot zoAbility elected
                let cost = activated_cost activated
                    effect = activated_effect activated
                maybeToLegality <$> playPendingAbility zoAbility cost effect goPay

    goWithThisActivated

-- (605.1a)
isPendingManaEffect :: Monad m => Pending (Effect 'OneShot) ot -> Magic 'Private 'RO m Bool
isPendingManaEffect (Pending effect) = logCall 'isPendingManaEffect do
  requiresTargets effect >>= \case
    True -> pure False
    False -> internalFromRW goGameResult $ local withHeadlessPrompt do
      mEnactInfo <- resolveOneShot zoStack effect
      pure case mEnactInfo of
        Nothing -> False -- XXX: It could still be a mana ability despite failing to be legal at the moment.
        Just enactInfo -> enactInfo_couldAddMana enactInfo
 where
  zoStack :: ZO 'ZStack OT0
  zoStack = error $ show ManaAbilitiesDontHaveTargetsSoNoZoShouldBeNeeded

  goGameResult _ = pure False -- It would be really weird if this code path actually gets hit.

playPendingOneShot ::
  forall m ot x.
  (AndLike x, AndLike (Maybe x)) =>
  Monad m =>
  ZO 'ZStack OT0 ->
  Cost ot ->
  WithThisOneShot ot ->
  (Cost ot -> Maybe (Pending (Effect 'OneShot) ot) -> Magic 'Private 'RW m (Maybe x)) ->
  Magic 'Private 'RW m (Maybe x)
playPendingOneShot _zoStack cost withThisElectEffect cont = logCall 'playPendingOneShot do
  goWithThis withThisElectEffect
 where
  goWithThis = \case
    This1 thisToElectEffect -> do
      thisId <- newObjectId
      goElectEffect $ thisToElectEffect (lensedThis thisId)
    This2 thisToElectEffect -> do
      thisId <- newObjectId
      goElectEffect $ thisToElectEffect (lensedThis thisId, lensedThis thisId)

  goElectEffect = cont cost . Just . Pending

playPendingPermanent ::
  forall m ot x.
  (AndLike x, AndLike (Maybe x)) =>
  Monad m =>
  ZO 'ZStack OT0 ->
  Cost ot ->
  (Cost ot -> Maybe (Pending (Effect 'OneShot) ot) -> Magic 'Private 'RW m (Maybe x)) ->
  Magic 'Private 'RW m (Maybe x)
playPendingPermanent _zoStack cost cont = logCall 'playPendingPermanent do
  cont cost Nothing

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

payElected ::
  forall ot m.
  Monad m =>
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElected elected = logCall 'payElected do
  pay (electedObject_controller elected) $ electedObject_cost elected

payElectedManaAbilityAndResolve ::
  forall ot m.
  (IsOT ot, Monad m) =>
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedManaAbilityAndResolve elected = logCall 'payElectedManaAbilityAndResolve do
  payElected elected >>= \case
    Illegal -> pure Illegal
    Legal -> resolveManaAbility elected

data ActivateCast = Activate | Cast

payElectedAndPutOnStack ::
  forall (ac :: ActivateCast) ot m.
  (PayElected ac ot ot, Monad m) =>
  ZO 'ZStack OT0 ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStack zo elected = logCall 'payElectedAndPutOnStack do
  payElectedAndPutOnStack' @ac @ot @ot zo elected

-- NOTE: `otdummy` exists to circumvent `The constraint ‘A’ is no smaller than the instance head ‘B’` issues
class ot ~ otdummy => PayElected (ac :: ActivateCast) (ot :: Type) (otdummy :: Type) where
  payElectedAndPutOnStack' :: Monad m => ZO 'ZStack OT0 -> Elected 'Pre ot -> Magic 'Private 'RW m Legality

instance IsOT ot => PayElected 'Activate ot ot where
  payElectedAndPutOnStack' = payElectedAndPutOnStackAbility @ot

instance PayElected 'Cast OTArtifact OTArtifact where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTArtifact

instance PayElected 'Cast OTArtifactCreature OTArtifactCreature where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTCreature

instance PayElected 'Cast OTCreature OTCreature where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTCreature

instance PayElected 'Cast OTEnchantment OTEnchantment where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTEnchantment

instance PayElected 'Cast OTEnchantmentCreature OTEnchantmentCreature where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTCreature

instance PayElected 'Cast OTInstant OTInstant where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTInstant

instance PayElected 'Cast OTPlaneswalker OTPlaneswalker where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTPlaneswalker

instance PayElected 'Cast OTSorcery OTSorcery where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTSorcery

payElectedAndPutOnStackAbility ::
  forall ot m.
  (IsOT ot, Monad m) =>
  ZO 'ZStack OT0 ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStackAbility zo = payElectedAndPutOnStackImpl zo \i ->
  let toObjectActivatedTriggered' = toObject2'
      o = idToObject @ 'OTActivatedAbility $ UntypedObject DefaultObjectDiscriminant i
   in StackAbility $ ZO SZStack $ toObjectActivatedTriggered' o

payElectedAndPutOnStackSpell ::
  forall a ot m.
  (IsObjectType a, AsSpell' a, IsOT ot, Monad m) =>
  ZO 'ZStack OT0 ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStackSpell zo = payElectedAndPutOnStackImpl zo \i ->
  let toObjectSpell' = toObject6'
      o = idToObject @a $ UntypedObject DefaultObjectDiscriminant i
   in StackSpell $ ZO SZStack $ toObjectSpell' o

payElectedAndPutOnStackImpl ::
  forall ot m.
  (IsOT ot, Monad m) =>
  ZO 'ZStack OT0 ->
  (ObjectId -> StackObject) ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStackImpl zoStack idToStackObject elected = do
  let stackId = getObjectId zoStack
      stackItem = idToStackObject stackId
  case stackItem of
    StackAbility{} -> pure ()
    StackSpell{} -> pure () -- TODO: Remove original zoSpell (if not an ability) from its original zone (e.g. remove from hand).
  modify \st ->
    st
      { magicStack = Stack $ stackItem : unStack (magicStack st)
      , magicStackEntryElectedMap = Map.insert zoStack (AnyElected elected) $ magicStackEntryElectedMap st
      }
  payElected elected

seedStackEntryTargets :: Monad m => ZO 'ZStack OT0 -> Magic 'Private 'RW m ()
seedStackEntryTargets zoStack = logCall 'seedStackEntryTargets do
  modify \st ->
    st
      { magicStackEntryTargetsMap = Map.insert zoStack [] $ magicStackEntryTargetsMap st
      }
