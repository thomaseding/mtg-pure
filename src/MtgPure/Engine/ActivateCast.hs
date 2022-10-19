{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.ActivateCast (
  askActivateAbility,
  activateAbility,
  castSpell,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Trans.Except (throwE)
import safe Control.Monad.Util (AndLike (..))
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe Data.Typeable (Typeable)
import safe MtgPure.Engine.Fwd.Api (
  gainPriority,
  getHasPriority,
  logCall,
  newObjectId,
  pay,
  performElections,
  rewindIllegal,
  runMagicCont,
 )
import safe MtgPure.Engine.Legality (Legality (..), legalityToMaybe, maybeToLegality)
import safe MtgPure.Engine.Monad (fromPublic, fromRO, get, gets, modify)
import safe MtgPure.Engine.Prompt (ActivateAbility (..), CastSpell (..), Prompt' (..), SomeActivatedAbility (..))
import safe MtgPure.Engine.State (
  AnyElected (..),
  Elected (..),
  GameState (..),
  Magic,
  MagicCont,
  Pending,
  PendingReady (..),
  StackEntry (..),
  electedObject_controller,
  electedObject_cost,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.Object (IsObjectType (..), OT0, OT1, Object, ObjectType (..))
import safe MtgPure.Model.ObjectId (ObjectId)
import safe MtgPure.Model.ObjectN (ObjectN (O1))
import safe MtgPure.Model.ObjectType.Kind (
  OTInstant,
  OTSorcery,
 )
import safe MtgPure.Model.PrePost (PrePost (..))
import safe MtgPure.Model.Recursive (
  ActivatedAbility (..),
  Card (..),
  CardFacet (..),
  Cost (..),
  Effect (..),
  Elect (..),
  IsSpecificCard (singSpecificCard),
  Some (..),
  SomeTerm (..),
  SpecificCard (..),
  WithThis (..),
  WithThisActivated,
  WithThisOneShot,
  YourCard (..),
 )
import safe MtgPure.Model.Stack (Stack (..), StackObject (..))
import safe MtgPure.Model.ToObjectN.Classes (ToObject2' (..), ToObject6' (..))
import safe MtgPure.Model.Zone (IsZone (..), SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZO, ZOPlayer, ZoneObject (..))
import safe MtgPure.Model.ZoneObject.Convert (oToZO1, toZO0)

type Legality' = Maybe ()

newtype ActivateAbilityReqs = ActivateAbilityReqs
  { activateAbilityReqs_hasPriority :: Bool
  }
  deriving (Eq, Ord, Show, Typeable)

-- Unfortunately pattern synonyms won't contribute to exhaustiveness checking.
pattern ActivateAbilityReqs_Satisfied :: ActivateAbilityReqs
pattern ActivateAbilityReqs_Satisfied =
  ActivateAbilityReqs
    { activateAbilityReqs_hasPriority = True
    }

getActivateAbilityReqs :: Monad m => Object 'OTPlayer -> Magic 'Private 'RO m ActivateAbilityReqs
getActivateAbilityReqs oPlayer = logCall 'getActivateAbilityReqs do
  hasPriority <- fromPublic $ getHasPriority oPlayer
  pure
    ActivateAbilityReqs
      { activateAbilityReqs_hasPriority = hasPriority -- (116.2a)
      }

askActivateAbility :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askActivateAbility oPlayer = logCall 'askActivateAbility do
  reqs <- lift $ fromRO $ getActivateAbilityReqs oPlayer
  case reqs of
    ActivateAbilityReqs_Satisfied -> do
      st <- lift $ fromRO get
      let opaque = mkOpaqueGameState st
          prompt = magicPrompt st
      mActivate <- lift $ lift $ promptActivateAbility prompt opaque oPlayer
      case mActivate of
        Nothing -> pure ()
        Just special -> do
          isLegal <- lift $ rewindIllegal $ activateAbility oPlayer special
          throwE case isLegal of
            True -> gainPriority oPlayer -- (117.3c)
            False -> runMagicCont (either id id) $ askActivateAbility oPlayer
    _ -> pure ()

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

castSpell :: forall m. Monad m => Object 'OTPlayer -> CastSpell -> Magic 'Private 'RW m Legality
castSpell oCaster (CastSpell someCard) = logCall 'castSpell case someCard of
  Some6a (SomeArtifact card) -> undefined card
  Some6b (SomeCreature card) -> undefined card
  Some6c (SomeEnchantment card) -> undefined card
  Some6d (SomeInstant card) -> castSpell' oCaster card instantCastMeta
  Some6e (SomePlaneswalker card) -> undefined card
  Some6f (SomeSorcery card) -> castSpell' oCaster card sorceryCastMeta
  Some6ab (SomeArtifactCreature card) -> undefined card
  Some6bc (SomeEnchantmentCreature card) -> undefined card

lensedThis :: (IsZone zone, IsObjectType a) => ObjectId -> ZO zone (OT1 a)
lensedThis = ZO singZone . O1 . idToObject

castSpell' ::
  forall ot m.
  Monad m =>
  Object 'OTPlayer ->
  Card ot ->
  CastMeta ot ->
  Magic 'Private 'RW m Legality
castSpell' oCaster card meta = logCall 'castSpell' case card of
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
        goElectFacet = fmap maybeToLegality . performElections andM zoSpell goFacet

        goFacet :: CardFacet ot -> Magic 'Private 'RW m Legality'
        goFacet facet = case facet of
          ArtifactLandFacet{} -> undefined -- TODO: Not a spell
          LandFacet{} -> undefined -- TODO: Not a spell
          --
          ArtifactFacet{} -> goFacet' facet
          ArtifactCreatureFacet{} -> goFacet' facet
          CreatureFacet{} -> goFacet' facet
          EnchantmentFacet{} -> goFacet' facet
          EnchantmentCreatureFacet{} -> goFacet' facet
          PlaneswalkerFacet{} -> goFacet' facet
          --
          InstantFacet{} -> goFacet' facet
          SorceryFacet{} -> goFacet' facet

        goFacet' :: CardFacet ot -> Magic 'Private 'RW m Legality'
        goFacet' facet = playPendingSpell
          zoSpell
          (castMeta_cost meta facet)
          (castMeta_effect meta facet)
          \cost effect -> do
            case singSpecificCard @ot of
              SorceryCard ->
                legalityToMaybe <$> do
                  payElectedAndPutOnStack @ 'Cast $
                    castMeta_Elected meta oCaster card facet cost effect
              _ -> undefined

    goElectFacet electFacet

-- TODO: Generalize for TriggeredAbility as well. Prolly make an AbilityMeta type that is analogous to CastMeta.
-- NOTE: A TriggeredAbility is basically the same as an ActivatedAbility that the game activates automatically.
activateAbility :: forall m. Monad m => Object 'OTPlayer -> ActivateAbility -> Magic 'Private 'RW m Legality
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
    pure () -- TODO: Check that `zoThis'` actually has the `withThisActivated` and is in the correct zone
    let zoThis = error "TODO: do_some_sort_of_cast" zoThis'
    goWithThis zoThis
   where
    goWithThis ::
      ZO zone ot ->
      Magic 'Private 'RW m Legality
    goWithThis zoThis = logCall' "goWithThis" case withThisActivated of
      T1 thisToElectActivated -> do
        thisId <- newObjectId
        goLegality thisToElectActivated (lensedThis thisId)
      T2 thisToElectActivated -> do
        thisId <- newObjectId
        goLegality thisToElectActivated (lensedThis thisId, lensedThis thisId)
     where
      goLegality ::
        forall this.
        (this -> Elect 'Pre (ActivatedAbility zone ot) ot) ->
        this ->
        Magic 'Private 'RW m Legality
      goLegality thisToElectAbility this = logCall' "goLegality" do
        abilityId <- newObjectId
        let zoAbility = toZO0 @ 'ZStack abilityId

            goElectActivated :: Elect 'Pre (ActivatedAbility zone ot) ot -> Magic 'Private 'RW m Legality
            goElectActivated =
              logCall' "goElectedActivated" $
                fmap maybeToLegality . performElections andM zoAbility goActivated

            goActivated :: ActivatedAbility zone ot -> Magic 'Private 'RW m Legality'
            goActivated activated = logCall' "goActivated" do
              legalityToMaybe <$> do
                let isThisInCorrectZone = True -- TODO
                    isController = True -- TODO
                case (isThisInCorrectZone, isController) of
                  (False, _) -> pure Illegal -- TODO prompt complaint
                  (_, False) -> pure Illegal -- TODO prompt complaint
                  (True, True) ->
                    maybeToLegality <$> do
                      playPendingAbility
                        zoAbility
                        (activated_cost activated)
                        (activated_effect activated)
                        \cost effect ->
                          legalityToMaybe <$> do
                            payElectedAndPutOnStack @ 'Activate $
                              ElectedActivatedAbility oPlayer zoThis cost effect

        goElectActivated $ thisToElectAbility this

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

setElectedObject_cost ::
  Elected pEffect ot ->
  Cost ot ->
  Elected pEffect ot
setElectedObject_cost elected cost = case elected of
  ElectedActivatedAbility{} -> elected{electedActivatedAbility_cost = cost}
  ElectedInstant{} -> elected{electedInstant_cost = cost}
  ElectedSorcery{} -> elected{electedSorcery_cost = cost}

data ActivateCast = Activate | Cast

class PayElected (ac :: ActivateCast) (ot :: Type) where
  payElectedAndPutOnStack :: Monad m => Elected 'Pre ot -> Magic 'Private 'RW m Legality

instance PayElected 'Activate ot where
  payElectedAndPutOnStack =
    logCall 'payElectedAndPutOnStack $
      payElectedAndPutOnStack' $
        StackAbility . ZO SZStack . toObject2' . idToObject @ 'OTActivatedAbility

instance PayElected 'Cast OTInstant where
  payElectedAndPutOnStack =
    logCall 'payElectedAndPutOnStack $
      payElectedAndPutOnStack' $
        StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTInstant

instance PayElected 'Cast OTSorcery where
  payElectedAndPutOnStack =
    logCall 'payElectedAndPutOnStack $
      payElectedAndPutOnStack' $
        StackSpell . ZO SZStack . toObject6' . idToObject @ 'OTSorcery

payElectedAndPutOnStack' ::
  forall ot m.
  Monad m =>
  (ObjectId -> StackObject) ->
  Elected 'Pre ot ->
  Magic 'Private 'RW m Legality
payElectedAndPutOnStack' idToStackObject elected = logCall 'payElectedAndPutOnStack' do
  stackId <- newObjectId
  let stackItem = idToStackObject stackId

      goCost :: Monad m => Cost ot -> Magic 'Private 'RW m Legality
      goCost cost =
        pay (electedObject_controller elected) cost >>= \case
          Illegal -> pure Illegal
          Legal -> do
            let entry =
                  StackEntry
                    { stackEntryTargets = []
                    , stackEntryElected = AnyElected $ setElectedObject_cost elected cost
                    }
            modify \st ->
              st
                { magicStack = Stack $ stackItem : unStack (magicStack st)
                , magicStackEntryMap = Map.insert (toZO0 stackId) entry $ magicStackEntryMap st
                }
            pure Legal

  legality <- goCost $ electedObject_cost elected
  prompt <- fromRO $ gets magicPrompt
  lift $ promptDebugMessage prompt $ show ("pay and put on stack", legality)
  case legality of
    Legal -> pure Legal
    Illegal -> pure Illegal -- TODO: GC stack object stuff if Illegal
