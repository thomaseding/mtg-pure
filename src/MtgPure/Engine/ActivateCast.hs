{-# LANGUAGE Safe #-}
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
import safe Data.Functor ((<&>))
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
  removeHandCard,
  resolveElected,
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
  ActivateResult (..),
  AnyElected (..),
  CastSpell,
  Elected (..),
  Ev,
  InternalLogicError (..),
  InvalidCastSpell (..),
  Pending,
  PendingReady (..),
  PriorityAction (ActivateAbility, CastSpell),
  Prompt' (..),
  ResolveElected (..),
  SomeActivatedAbility (..),
  electedObject_controller,
  electedObject_cost,
 )
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  logCall,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsCardList (containsCard)
import safe MtgPure.Model.Mana.IsManaAbility (isManaAbility)
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTN (OT0, OTN)
import safe MtgPure.Model.Object.OTNAliases (
  OTNArtifact,
  OTNArtifactCreature,
  OTNCreature,
  OTNEnchantment,
  OTNEnchantmentCreature,
  OTNInstant,
  OTNPlaneswalker,
  OTNSorcery,
  OTNSpell,
 )
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (
  ObjectId,
  UntypedObject (..),
  getObjectId,
  pattern DefaultObjectDiscriminant,
 )
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.PromoteIdToObjectN (promoteIdToObjectN)
import safe MtgPure.Model.Object.ToObjectN.Classes (ToObject2' (..), ToObject6' (..))
import safe MtgPure.Model.PhaseStep (PhaseStep (..))
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
  WithThisActivated,
  WithThisOneShot,
  YourCardFacet (..),
 )
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (AsSpell', asCard, oToZO1, reifyWithThis, toZO0, zo0ToCard)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO, ZOPlayer, ZoneObject (..))

type Legality' = Maybe ()

data Speed = InstantSpeed | SorcerySpeed
  deriving (Eq, Ord, Show, Typeable)

getAbilitySpeed :: Monad m => IsZO zone ot => SomeActivatedAbility zone ot -> Magic 'Private 'RO m Speed
getAbilitySpeed _ = pure InstantSpeed -- TODO: activate only as a sorcery

getSpellSpeed :: Monad m => IsZone zone => ZO zone OTNSpell -> Magic 'Private 'RO m Speed
getSpellSpeed _ = pure SorcerySpeed -- TODO: Instants, flash, etc.

speedIsAllowable :: Monad m => Speed -> Magic 'Private 'RO m Bool
speedIsAllowable speed = do
  -- TODO: check if a split second spell is on the stack
  phaseStep <- gets magicPhaseStep
  pure case speed of
    InstantSpeed -> True
    SorcerySpeed -> case phaseStep of
      PSPreCombatMainPhase -> True
      PSPostCombatMainPhase -> True
      _ -> False

_canActivateAbilityNow :: Monad m => Object 'OTPlayer -> IsZO zone ot => SomeActivatedAbility zone ot -> Magic 'Private 'RO m Bool
_canActivateAbilityNow oPlayer ability = logCall '_canActivateAbilityNow do
  hasPriority <- fromPublic $ getHasPriority oPlayer
  speed <- fromRO $ getAbilitySpeed ability
  speedIsAllowable speed <&> (&& hasPriority)

_canPlaySpellNow :: Monad m => Object 'OTPlayer -> IsZone zone => ZO zone OTNSpell -> Magic 'Private 'RO m Bool
_canPlaySpellNow oPlayer = logCall '_canPlaySpellNow \zoSpell -> do
  hasPriority <- fromPublic $ getHasPriority oPlayer
  speed <- fromRO $ getSpellSpeed zoSpell
  speedIsAllowable speed <&> (&& hasPriority)

-- XXX: obsolete; replace with `canActivateAbilityNow`
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

-- XXX: obsolete; replace with `canCastSpellNow`
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

artifactCastMeta :: CastMeta OTNArtifact
artifactCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = artifact_cost
    }

artifactCreatureCastMeta :: CastMeta OTNArtifactCreature
artifactCreatureCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = artifactCreature_cost
    }

creatureCastMeta :: CastMeta OTNCreature
creatureCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = creature_cost
    }

enchantmentCastMeta :: CastMeta OTNEnchantment
enchantmentCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = enchantment_cost
    }

enchantmentCreatureCastMeta :: CastMeta OTNEnchantmentCreature
enchantmentCreatureCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = enchantmentCreature_cost
    }

instantCastMeta :: CastMeta OTNInstant
instantCastMeta =
  CastMeta
    { castMeta_effect = Just instant_effect
    , castMeta_cost = instant_cost
    }

planeswalkerCastMeta :: CastMeta OTNPlaneswalker
planeswalkerCastMeta =
  CastMeta
    { castMeta_effect = Nothing
    , castMeta_cost = planeswalker_cost
    }

sorceryCastMeta :: CastMeta OTNSorcery
sorceryCastMeta =
  CastMeta
    { castMeta_effect = Just sorcery_effect
    , castMeta_cost = sorcery_cost
    }

castSpell :: forall m. Monad m => Object 'OTPlayer -> PriorityAction CastSpell -> Magic 'Private 'RW m Legality
castSpell oCaster = logCall 'castSpell \case
  CastSpell zoSpell -> goSpell zoSpell
 where
  goSpell :: forall zone. IsZO zone OTNSpell => ZO zone OTNSpell -> Magic 'Private 'RW m Legality
  goSpell zoSpell = do
    st <- fromRO get
    reqs <- fromRO $ getCastSpellReqs oCaster
    player <- fromRO $ getPlayer oCaster
    let opaque = mkOpaqueGameState st
        prompt = magicPrompt st
        hand = playerHand player
        --zoCaster = oToZO1 oCaster
        --
        invalid :: (ZO zone OTNSpell -> InvalidCastSpell) -> Magic 'Private 'RW m Legality
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
                      Card _name yourCard -> castYourSpellCard zoSpell oCaster card yourCard
                    AnyCard2{} -> undefined

castYourSpellCard ::
  forall zone ot m x.
  (ot ~ OTN x, IsZO zone OTNSpell, Monad m) =>
  ZO zone OTNSpell ->
  Object 'OTPlayer ->
  Card ot ->
  YourCardFacet ot ->
  Magic 'Private 'RW m Legality
castYourSpellCard zoSpell oCaster card = logCall 'castYourSpellCard \case
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
    stackId <- newObjectId
    let zoStack = toZO0 @ 'ZStack stackId

        goElectFacet :: Elect 'Pre (CardFacet ot) ot -> Magic 'Private 'RW m Legality
        goElectFacet elect = do
          seedStackEntryTargets zoStack
          maybeToLegality <$> performElections zoStack goFacet elect

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
                    payElectedAndPutOnStack @ 'Cast @ot zoStack $
                      ElectedSpell zoSpell oCaster anyCard facet cost mEffect
                  ArtifactCreatureCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoStack $
                      ElectedSpell zoSpell oCaster anyCard facet cost mEffect
                  CreatureCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoStack $
                      ElectedSpell zoSpell oCaster anyCard facet cost mEffect
                  EnchantmentCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoStack $
                      ElectedSpell zoSpell oCaster anyCard facet cost mEffect
                  EnchantmentCreatureCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoStack $
                      ElectedSpell zoSpell oCaster anyCard facet cost mEffect
                  InstantCard -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoStack $
                      ElectedSpell zoSpell oCaster anyCard facet cost mEffect
                  PlaneswalkerCard{} -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoStack $
                      ElectedSpell zoSpell oCaster anyCard facet cost mEffect
                  SorceryCard -> do
                    payElectedAndPutOnStack @ 'Cast @ot zoStack $
                      ElectedSpell zoSpell oCaster anyCard facet cost mEffect
          case castMeta_effect meta of
            Just facetToEffect -> do
              playPendingOneShot zoStack (castMeta_cost meta facet) (facetToEffect facet) goPay
            Nothing -> do
              playPendingPermanent zoStack (castMeta_cost meta facet) goPay

    goElectFacet electFacet

-- TODO: Generalize for TriggeredAbility as well. Prolly make an AbilityMeta type that is analogous to CastMeta.
-- NOTE: A TriggeredAbility is basically the same as an ActivatedAbility that the game activates automatically.
-- TODO: This needs to validate ActivateAbilityReqs
activateAbility :: forall m. Monad m => Object 'OTPlayer -> PriorityAction ActivateAbility -> Magic 'Private 'RW m ActivateResult
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
    Magic 'Private 'RW m ActivateResult
  goSomeActivatedAbility zoThis' withThisActivated = logCall' "goSomeActivatedAbility" do
    zoExists' <- fromRO $ doesZoneObjectExist zoThis'
    pure () -- TODO: Check that `zoThis'` actually has the `withThisActivated` (including intrinsic abilities)
    _reqs <- fromRO $ getActivateAbilityReqs oPlayer -- TODO: validate reqs
    let oThis = promoteIdToObjectN @ot $ getObjectId zoThis'
        zoThis = ZO (singZone @zone) oThis
        some = SomeActivatedAbility zoThis withThisActivated
        thisId = getObjectId zoThis

        invalid :: m () -> Magic 'Private 'RW m ActivateResult
        invalid complain = do
          () <- lift complain
          pure IllegalActivation

        goWithThisActivated :: Magic 'Private 'RW m ActivateResult
        goWithThisActivated = do
          abilityId <- newObjectId
          let zoAbility = toZO0 @ 'ZStack abilityId
          goElectActivated zoAbility $ reifyWithThis thisId withThisActivated

        goElectActivated :: ZO 'ZStack OT0 -> Elect 'Pre (ActivatedAbility zone ot) ot -> Magic 'Private 'RW m ActivateResult
        goElectActivated zoAbility elect = logCall' "goElectedActivated" do
          seedStackEntryTargets zoAbility
          performElections zoAbility (goActivated zoAbility) elect <&> \case
            Nothing -> IllegalActivation
            Just result -> result

        goActivated :: ZO 'ZStack OT0 -> ActivatedAbility zone ot -> Magic 'Private 'RW m (Maybe ActivateResult)
        goActivated zoAbility activated = logCall' "goActivated" do
          goActivated' zoAbility activated <&> \case
            IllegalActivation -> Nothing
            result -> Just result

        goActivated' :: ZO 'ZStack OT0 -> ActivatedAbility zone ot -> Magic 'Private 'RW m ActivateResult
        goActivated' zoAbility activated = logCall' "goActivated'" do
          let isController = True -- TODO
              abilityExists = True -- TODO
          prompt <- fromRO $ gets magicPrompt
          case (zoExists', isController, abilityExists) of
            (False, _, _) -> invalid $ exceptionZoneObjectDoesNotExist prompt zoThis'
            (_, False, _) -> invalid undefined
            (_, _, False) -> invalid undefined
            (True, True, True) -> do
              let goPay cost effect = do
                    let elected = ElectedActivatedAbility some oPlayer zoThis cost effect
                        isManaAbility' = isManaAbility withThisActivated
                    case isManaAbility' of
                      True -> do
                        legality <- payElectedManaAbilityAndResolve elected
                        pure case legality of
                          ResolvedManaAbility evs -> ActivatedManaAbility evs
                          CantPay -> IllegalActivation
                      False -> do
                        legality <- payElectedAndPutOnStack @ 'Activate @ot zoAbility elected
                        pure case legality of
                          Legal -> ActivatedNonManaAbility
                          Illegal -> IllegalActivation
              let cost = activated_cost activated
                  effect = activated_effect activated
              playPendingAbility zoAbility cost effect goPay

    goWithThisActivated

playPendingOneShot ::
  forall m ot x.
  (AndLike x, AndLike (Maybe x), IsZO 'ZStack ot) =>
  Monad m =>
  ZO 'ZStack OT0 ->
  Cost ot ->
  WithThisOneShot ot ->
  (Cost ot -> Maybe (Pending (Effect 'OneShot) ot) -> Magic 'Private 'RW m (Maybe x)) ->
  Magic 'Private 'RW m (Maybe x)
playPendingOneShot _zoStack cost withThisElectEffect cont = logCall 'playPendingOneShot do
  thisId <- newObjectId
  goElectEffect $ reifyWithThis thisId withThisElectEffect
 where
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
  Monad m =>
  ZO 'ZStack OT0 ->
  Cost ot ->
  Elect 'Post (Effect 'OneShot) ot ->
  (Cost ot -> Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m x) ->
  Magic 'Private 'RW m x
playPendingAbility _zoStack cost electEffect cont = logCall 'playPendingAbility do
  pure () -- TODO: other stuff?
  cont cost $ Pending electEffect

payElected ::
  forall ot m.
  Monad m =>
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElected elected = logCall 'payElected do
  pay (electedObject_controller elected) $ electedObject_cost elected

data ResolvedManaAbility :: Type where
  CantPay :: ResolvedManaAbility
  ResolvedManaAbility :: [Ev] -> ResolvedManaAbility

payElectedManaAbilityAndResolve ::
  forall ot m.
  (IsOTN ot, Monad m) =>
  Elected 'Pre ot ->
  Magic 'Private 'RW m ResolvedManaAbility
payElectedManaAbilityAndResolve elected = logCall 'payElectedManaAbilityAndResolve do
  payElected elected >>= \case
    Illegal -> pure CantPay
    Legal -> do
      let zoStack = error $ show ManaAbilitiesDontHaveTargetsSoNoZoShouldBeNeeded
      resolveElected zoStack elected <&> \case
        ResolvedEffect evs -> ResolvedManaAbility evs
        Fizzled -> error $ show (undefined :: InternalLogicError) -- mana abilities don't have targets
        PermanentResolved -> error $ show (undefined :: InternalLogicError) -- mana abilities are abilities

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

instance IsOTN ot => PayElected 'Activate ot ot where
  payElectedAndPutOnStack' = payElectedAndPutOnStackAbility @ot

instance PayElected 'Cast OTNArtifact OTNArtifact where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTArtifact

instance PayElected 'Cast OTNArtifactCreature OTNArtifactCreature where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTCreature

instance PayElected 'Cast OTNCreature OTNCreature where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTCreature

instance PayElected 'Cast OTNEnchantment OTNEnchantment where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTEnchantment

instance PayElected 'Cast OTNEnchantmentCreature OTNEnchantmentCreature where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTCreature

instance PayElected 'Cast OTNInstant OTNInstant where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTInstant

instance PayElected 'Cast OTNPlaneswalker OTNPlaneswalker where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTPlaneswalker

instance PayElected 'Cast OTNSorcery OTNSorcery where
  payElectedAndPutOnStack' = payElectedAndPutOnStackSpell @ 'OTSorcery

payElectedAndPutOnStackAbility ::
  forall ot m.
  (IsOTN ot, Monad m) =>
  ZO 'ZStack OT0 ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStackAbility zo = payElectedAndPutOnStackImpl zo \i ->
  let toObjectActivatedTriggered' = toObject2'
      o = idToObject @ 'OTActivatedAbility $ UntypedObject DefaultObjectDiscriminant i
   in StackAbility $ ZO SZStack $ toObjectActivatedTriggered' o

payElectedAndPutOnStackSpell ::
  forall a ot m.
  (IsObjectType a, AsSpell' a, IsOTN ot, Monad m) =>
  ZO 'ZStack OT0 ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStackSpell zo = payElectedAndPutOnStackImpl zo \i ->
  let toObjectSpell' = toObject6'
      o = idToObject @a $ UntypedObject DefaultObjectDiscriminant i
   in StackSpell $ ZO SZStack $ toObjectSpell' o

payElectedAndPutOnStackImpl ::
  forall ot m.
  (IsOTN ot, Monad m) =>
  ZO 'ZStack OT0 ->
  (ObjectId -> StackObject) ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStackImpl zoStack idToStackObject elected = do
  pure () -- Assume the caller uses rewindIllegal, so no need to manually clean up game state here when illegal.
  let stackId = getObjectId zoStack
      stackItem = idToStackObject stackId
  case stackItem of
    StackAbility{} -> pure ()
    StackSpell{} -> removeFromSourceZone
  modify \st ->
    st
      { magicStack = Stack $ stackItem : unStack (magicStack st)
      , magicStackEntryElectedMap = Map.insert zoStack (AnyElected elected) $ magicStackEntryElectedMap st
      , magicOwnershipMap = Map.insert stackId (electedObject_controller elected) $ magicOwnershipMap st
      }
  payElected elected
 where
  removeFromSourceZone =
    case elected of
      ElectedActivatedAbility{} -> undefined -- should not be possible
      ElectedSpell zoSource _ _ _ _ _ -> do
        let go :: forall zone. IsZone zone => ZO zone OTNSpell -> ZO zone OT0 -> Magic 'Private 'RW m ()
            go _ zoSource0 = case singZone @zone of
              SZHand -> do
                removedCard <- removeHandCard (electedObject_controller elected) $ zo0ToCard zoSource0
                case removedCard of
                  Nothing -> undefined -- cant happen? ... if it can legitly happen, return Illegal instead
                  Just _ -> pure ()
              _ -> undefined -- TODO: singZone
        go zoSource $ toZO0 zoSource

seedStackEntryTargets :: Monad m => ZO 'ZStack OT0 -> Magic 'Private 'RW m ()
seedStackEntryTargets zoStack = logCall 'seedStackEntryTargets do
  modify \st ->
    st
      { magicStackEntryTargetsMap = Map.insert zoStack [] $ magicStackEntryTargetsMap st
      }
