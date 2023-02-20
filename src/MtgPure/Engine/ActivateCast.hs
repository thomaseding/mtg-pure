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
import safe MtgPure.Model.ElectStage (ElectStage (..))
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
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  AnyCard (..),
  Card (..),
  CardFacet (..),
  CardFacet' (..),
  Cost (..),
  Effect (..),
  Elect (..),
  ElectOT (unElectOT),
  IsSpecificCard (singSpecificCard),
  SpecificCard (..),
  WithThisActivated,
  WithThisOneShot,
 )
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (AsSpell', asCard, reifyWithThis, toZO0, zo0ToCard)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsOTN, IsZO, ZO, ZoneObject (..))

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
    { castMeta_effect :: Maybe (CardFacet' ot -> WithThisOneShot ot)
    , castMeta_cost :: CardFacet' ot -> Cost ot
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
  CastSpell zoSpellCard -> goSpell zoSpellCard
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
          SZExile -> invalid undefined -- TODO: [Misthollow Griffin]
          SZLibrary -> invalid undefined -- TODO: [Panglacial Wurm]
          SZStack -> invalid undefined
          SZGraveyard -> invalid undefined -- TODO: [Gravecrawler]
          SZHand -> do
            mCard <- fromRO $ gets $ Map.lookup (toZO0 zoSpell) . magicHandCards
            case mCard of
              Nothing -> invalid CastSpell_NotInZone
              Just anyCard -> do
                case containsCard (asCard zoSpell) hand of
                  False -> invalid CastSpell_NotOwned
                  True -> case anyCard of
                    AnyCard1 card -> case card of
                      Card{} -> do
                        stackId <- newObjectId
                        let zoStack = toZO0 @ 'ZStack stackId
                        modify \st' ->
                          st'
                            { magicControllerMap = Map.insert stackId oCaster $ magicControllerMap st'
                            , magicOwnerMap = Map.insert stackId oCaster $ magicOwnerMap st'
                            }
                        castSpellCard zoStack zoSpell oCaster card
                    AnyCard2{} -> undefined

castSpellCard ::
  forall zone ot m x.
  (ot ~ OTN x, IsZO zone OTNSpell, IsSpecificCard ot, Monad m) =>
  ZO 'ZStack OT0 ->
  ZO zone OTNSpell ->
  Object 'OTPlayer ->
  Card ot ->
  Magic 'Private 'RW m Legality
castSpellCard zoStack zoSpellCard oCaster card = logCall 'castSpellCard case card of
  Card _name electIntrinsic -> goElectIntrinsic electIntrinsic
 where
  goInvalid :: Magic 'Private 'RW m Legality'
  goInvalid = do
    -- TODO: prompt error
    pure Nothing

  goElectIntrinsic ::
    IsSpecificCard ot =>
    Elect 'IntrinsicStage (CardFacet ot) ot ->
    Magic 'Private 'RW m Legality
  goElectIntrinsic electIntrinsic = do
    mFacet <- fromRO $ performElections zoStack (pure . Just) electIntrinsic
    case mFacet of
      Nothing -> pure Illegal
      Just facet -> maybeToLegality <$> goFacet facet

  goFacet :: CardFacet ot -> Magic 'Private 'RW m Legality'
  goFacet facet = case facet of
    ArtifactLandFacet{} -> goInvalid
    LandFacet{} -> goInvalid
    --
    InstantFacet{} -> go $ instant_spec facet
    SorceryFacet{} -> go $ sorcery_spec facet
    --
    ArtifactFacet{} -> goPerm $ artifact_spec facet
    ArtifactCreatureFacet{} -> goPerm $ artifactCreature_spec facet
    CreatureFacet{} -> goPerm $ creature_spec facet
    EnchantmentFacet{} -> goPerm $ enchantment_spec facet
    EnchantmentCreatureFacet{} -> goPerm $ enchantmentCreature_spec facet
    PlaneswalkerFacet{} -> goPerm $ planeswalker_spec facet
   where
    go = goElectFacet' facet
    goPerm = go . ElectCardFacet'

  goElectFacet' :: CardFacet ot -> Elect 'TargetStage (CardFacet' ot) ot -> Magic 'Private 'RW m Legality'
  goElectFacet' facet elect = do
    seedStackEntryTargets zoStack
    performElections zoStack (goFacet' facet) elect

  goFacet' :: CardFacet ot -> CardFacet' ot -> Magic 'Private 'RW m Legality'
  goFacet' facet facet' = case facet' of
    ArtifactLandFacet'{} -> undefined -- TODO: Not a spell
    LandFacet'{} -> undefined -- TODO: Not a spell
    --
    ArtifactFacet'{} -> go artifactCastMeta
    ArtifactCreatureFacet'{} -> go artifactCreatureCastMeta
    CreatureFacet'{} -> go creatureCastMeta
    EnchantmentFacet'{} -> go enchantmentCastMeta
    EnchantmentCreatureFacet'{} -> go enchantmentCreatureCastMeta
    InstantFacet'{} -> go instantCastMeta
    PlaneswalkerFacet'{} -> go planeswalkerCastMeta
    SorceryFacet'{} -> go sorceryCastMeta
   where
    go = goCommonFacet' facet facet'

  goCommonFacet' :: CardFacet ot -> CardFacet' ot -> CastMeta ot -> Magic 'Private 'RW m Legality'
  goCommonFacet' facet facet' meta = do
    let anyCard = AnyCard1 card
    let goPay cost mEffect = do
          let electedSpell =
                ElectedSpell
                  { electedSpell_originalSource = zoSpellCard
                  , electedSpell_controller = oCaster
                  , electedSpell_card = anyCard
                  , electedSpell_facet = facet
                  , electedSpell_facet' = facet'
                  , electedSpell_cost = cost
                  , electedSpell_effect = mEffect
                  }
          legalityToMaybe <$> case singSpecificCard @ot of
            ArtifactLandCard{} -> error $ show CantHappenByConstruction
            LandCard{} -> error $ show CantHappenByConstruction
            --
            ArtifactCard{} ->
              payElectedAndPutOnStack @ 'Cast @ot zoStack electedSpell
            ArtifactCreatureCard{} -> do
              payElectedAndPutOnStack @ 'Cast @ot zoStack electedSpell
            CreatureCard{} -> do
              payElectedAndPutOnStack @ 'Cast @ot zoStack electedSpell
            EnchantmentCard{} -> do
              payElectedAndPutOnStack @ 'Cast @ot zoStack electedSpell
            EnchantmentCreatureCard{} -> do
              payElectedAndPutOnStack @ 'Cast @ot zoStack electedSpell
            InstantCard -> do
              payElectedAndPutOnStack @ 'Cast @ot zoStack electedSpell
            PlaneswalkerCard{} -> do
              payElectedAndPutOnStack @ 'Cast @ot zoStack electedSpell
            SorceryCard -> do
              payElectedAndPutOnStack @ 'Cast @ot zoStack electedSpell
    case castMeta_effect meta of
      Just facetToEffect -> do
        playPendingOneShot zoStack (castMeta_cost meta facet') (facetToEffect facet') goPay
      Nothing -> do
        playPendingPermanent zoStack (castMeta_cost meta facet') goPay

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
    let zoThis = ZO (singZone @zone) oThis
    let some = SomeActivatedAbility zoThis withThisActivated
    let thisId = getObjectId zoThis

    let invalid :: m () -> Magic 'Private 'RW m ActivateResult
        invalid complain = do
          () <- lift complain
          pure IllegalActivation

        goWithThisActivated :: Magic 'Private 'RW m ActivateResult
        goWithThisActivated = do
          abilityId <- newObjectId
          let zoAbility = toZO0 @ 'ZStack abilityId
          goElectActivated zoAbility $ unElectOT $ reifyWithThis thisId withThisActivated

        goElectActivated :: ZO 'ZStack OT0 -> Elect 'TargetStage (ActivatedAbility zone ot) ot -> Magic 'Private 'RW m ActivateResult
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
          let abilityExists = True -- TODO
          prompt <- fromRO $ gets magicPrompt
          case (zoExists', isController, abilityExists) of
            (False, _, _) -> invalid $ exceptionZoneObjectDoesNotExist prompt zoThis'
            (_, False, _) -> invalid undefined
            (_, _, False) -> invalid undefined
            (True, True, True) -> do
              let goPay cost effect = do
                    let elected =
                          ElectedActivatedAbility
                            { electedActivatedAbility_ability = some
                            , electedActivatedAbility_controller = oPlayer
                            , electedActivatedAbility_this = zoThis
                            , electedActivatedAbility_cost = cost
                            , electedActivatedAbility_effect = effect
                            }
                    let isManaAbility' = isManaAbility withThisActivated
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
              let effect = activated_effect activated
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
playPendingOneShot zoStack cost withThisElectEffect cont = logCall 'playPendingOneShot do
  thisId <- newObjectId
  let electTargets = reifyWithThis thisId withThisElectEffect
  mElectResolve <- performElections zoStack (pure . Just) electTargets
  case mElectResolve of
    Nothing -> pure Nothing
    Just electResolve -> goElectEffect electResolve
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
  Elect 'ResolveStage (Effect 'OneShot) ot ->
  (Cost ot -> Pending (Effect 'OneShot) ot -> Magic 'Private 'RW m x) ->
  Magic 'Private 'RW m x
playPendingAbility _zoStack cost electEffect cont = logCall 'playPendingAbility do
  pure () -- TODO: other stuff?
  cont cost $ Pending electEffect

payElected ::
  forall ot m.
  Monad m =>
  Elected 'TargetStage ot ->
  Magic 'Private 'RW m Legality
payElected elected = logCall 'payElected do
  pay (electedObject_controller elected) $ electedObject_cost elected

data ResolvedManaAbility :: Type where
  CantPay :: ResolvedManaAbility
  ResolvedManaAbility :: [Ev] -> ResolvedManaAbility

payElectedManaAbilityAndResolve ::
  forall ot m.
  (IsOTN ot, Monad m) =>
  Elected 'TargetStage ot ->
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
  Elected 'TargetStage ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStack zo elected = logCall 'payElectedAndPutOnStack do
  payElectedAndPutOnStack' @ac @ot @ot zo elected

-- NOTE: `dummyOT` exists to circumvent `The constraint ‘A’ is no smaller than the instance head ‘B’` issues
class ot ~ dummyOT => PayElected (ac :: ActivateCast) (ot :: Type) (dummyOT :: Type) where
  payElectedAndPutOnStack' :: Monad m => ZO 'ZStack OT0 -> Elected 'TargetStage ot -> Magic 'Private 'RW m Legality

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
  Elected 'TargetStage ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStackAbility zo = payElectedAndPutOnStackImpl zo \i ->
  let toObjectActivatedTriggered' = toObject2'
      o = idToObject @ 'OTActivatedAbility $ UntypedObject DefaultObjectDiscriminant i
   in StackAbility $ ZO SZStack $ toObjectActivatedTriggered' o

payElectedAndPutOnStackSpell ::
  forall a ot m.
  (IsObjectType a, AsSpell' a, IsOTN ot, Monad m) =>
  ZO 'ZStack OT0 ->
  Elected 'TargetStage ot ->
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
  Elected 'TargetStage ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStackImpl zoStack idToStackObject elected = do
  pure () -- Assume the caller uses rewindIllegal, so no need to manually clean up game state here when illegal.
  let stackId = getObjectId zoStack
  let stackItem = idToStackObject stackId
  case stackItem of
    StackAbility{} -> pure ()
    StackSpell{} -> removeFromSourceZone
  modify \st ->
    assert (Map.lookup stackId (magicControllerMap st) == Just (electedObject_controller elected)) $
      assert (Map.lookup stackId (magicOwnerMap st) == Just (electedObject_controller elected)) $
        st
          { magicStack = Stack $ stackItem : unStack (magicStack st)
          , magicStackEntryElectedMap = Map.insert zoStack (AnyElected elected) $ magicStackEntryElectedMap st
          }
  payElected elected
 where
  removeFromSourceZone =
    case elected of
      ElectedActivatedAbility{} -> undefined -- should not be possible
      ElectedSpell{electedSpell_originalSource = zoSource} -> do
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
