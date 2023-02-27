{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Enact (
  enact,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Util (untilJust)
import safe qualified Data.Foldable as F
import safe Data.Functor ((<&>))
import safe qualified Data.List as List
import safe Data.List.NonEmpty (NonEmpty (..))
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Set as Set
import safe MtgPure.Engine.Fwd.Api (
  caseOf,
  findPermanent,
  findPlayer,
  getAlivePlayers,
  getPermanent,
  getPlayer,
  ownerOf,
  performElections,
  pushGraveyardCard,
  pushHandCard,
  removeLibraryCard,
  rewindNothing,
  setPermanent,
  setPlayer,
  zosSatisfying,
 )
import safe MtgPure.Engine.Monad (fromPublic, fromPublicRO, fromRO, gets)
import safe MtgPure.Engine.Orphans (mapManaPool)
import safe MtgPure.Engine.Prompt (
  AnyElected (..),
  CardCount (..),
  CardIndex (..),
  Elected (..),
  Ev (..),
  InternalLogicError (..),
  Prompt' (..),
  SomeActivatedAbility (..),
  SourceZO (..),
  TriggerTime (..),
 )
import safe MtgPure.Engine.PutOntoBattlefield (putOntoBattlefield)
import safe MtgPure.Engine.State (GameState (..), Magic, logCall, mkOpaqueGameState)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.ElectStage (ElectStage (..))
import safe MtgPure.Model.IsCardList (IsCardList (..), popCard)
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.Mana (freezeMana)
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Object.OTNAliases (OTNDamageSource)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (GetObjectId (getUntypedObject), getObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Card (CoCard)
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Permanent (Permanent (..), Tapped (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (
  CardCharacteristic (..),
  Effect (..),
  Elect,
  Requirement (..),
  WithLinkedObject,
  fromSomeOT,
 )
import MtgPure.Model.Stack (Stack (..), stackObjectToZo0)
import safe MtgPure.Model.Supertype (Supertype)
import safe qualified MtgPure.Model.Supertype as Ty
import safe MtgPure.Model.Variable (forceVars)
import safe MtgPure.Model.Zone (SZone (SZLibrary), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (
  getWithLinkedObjectRequirements,
  reifyWithLinkedObject,
  toZO0,
  uoToON,
  zo0ToPermanent,
  zo1ToO,
 )
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
  ZO,
  ZOCreaturePlayerPlaneswalker,
  ZOPermanent,
  ZOPlayer,
  ZoneObject (..),
 )

-- XXX: Is it appropriate to trigger these at the end of resolution instead of as they occur?
-- Replacement effects need to be done immediately... but will they be encoded as `EvListener`?
-- Maybe this is solved by triggering in both cases and have the `EvListener` decide whether to act.
-- XXX: Replacement effects don't just listen... they also modify the event, so perhaps a different
-- mechanism is needed.
-- XXX: A replacement effect prolly is an `EvListener` that returns a `Maybe (SourceZO, Effect)`.
triggerEvListeners :: Monad m => TriggerTime -> [Ev] -> Magic 'Private 'RW m ()
triggerEvListeners time evs = logCall 'triggerEvListeners do
  listeners <- fromRO $ gets magicListeners
  F.for_ evs \ev -> do
    F.for_ (Map.elems listeners) \listener -> do
      listener time ev

-- | This triggers registered event listeners automatically in addition to returning the events.
--
-- XXX: Does this really need a non-unit return type? I suppose it simplifies ad-hoc event listening
-- by not requiring localized event listener registration, which might be worth the return type.
enact :: Monad m => Maybe SourceZO -> Effect 'OneShot -> Magic 'Private 'RW m [Ev]
enact mSource effect = logCall 'enact do
  evs <- enact' mSource effect
  triggerEvListeners AfterResolution evs
  pure evs

-- XXX: This might need to return `Maybe [ev]` because elections might fail in a recursive
-- call and might need to rewind state. Need to distinguish [] from Nothing.
enact' :: Monad m => Maybe SourceZO -> Effect 'OneShot -> Magic 'Private 'RW m [Ev]
enact' mSource = logCall 'enact' \case
  AddMana oPlayer mana -> addMana' mSource oPlayer mana
  AddToBattlefield{} -> undefined
  CounterAbility{} -> undefined
  CounterSpell{} -> undefined
  DealDamage oSource oVictim damage -> dealDamage' oSource oVictim damage
  Destroy oPerm -> destroy' mSource oPerm
  DrawCards oPlayer amount -> drawCards' mSource amount $ zo1ToO oPlayer
  EffectCase case_ -> caseOf (enact' mSource) case_
  EffectContinuous{} -> undefined
  EndTheTurn -> undefined
  Exile{} -> undefined
  GainLife{} -> undefined
  LoseLife{} -> undefined
  PutOntoBattlefield oPlayer zo -> putOntoBattlefield' (zo1ToO oPlayer) zo
  Sacrifice{} -> undefined
  SearchLibrary searcher searchee cont -> searchLibrary' mSource (zo1ToO searcher) (zo1ToO searchee) cont
  Sequence effects -> mconcat <$> mapM (enact' mSource) effects
  ShuffleLibrary oPlayer -> shuffleLibrary' mSource $ zo1ToO oPlayer
  Tap oPerm -> tap' mSource oPerm
  Untap oPerm -> untap' mSource oPerm
  WithList{} -> undefined

addMana' :: Monad m => Maybe SourceZO -> ZOPlayer -> ManaPool 'NonSnow -> Magic 'Private 'RW m [Ev]
addMana' mSource oPlayer mana = logCall 'addMana' do
  fromRO (findPlayer $ zo1ToO oPlayer) >>= \case
    Nothing -> pure () -- Don't complain. This can naturally happen if a player in a multiplayer game loses before `enact'` resolves.
    Just player -> do
      isSnow <- case mSource of
        Nothing -> pure False
        Just (SourceZO zoStack) -> do
          let zoStack0 = toZO0 $ getObjectId zoStack
          entry <- fromRO $ gets $ Map.lookup zoStack0 . magicStackEntryElectedMap
          case entry of
            Nothing -> assert False $ pure False -- XXX: Can this happen?
            Just (AnyElected elected) -> case elected of
              ElectedActivatedAbility{electedActivatedAbility_ability = someZo} -> goAbilitySource someZo
              ElectedSpell{electedSpell_character = character} -> pure $ goSpell character
      let mana' =
            playerMana player + case isSnow of
              True -> mempty{poolSnow = mapManaPool freezeMana mana}
              False -> mempty{poolNonSnow = mana}
      setPlayer (zo1ToO oPlayer) player{playerMana = mana'}
  pure mempty
 where
  goSupertypes :: [Supertype ot] -> Bool
  goSupertypes = any \case
    Ty.Snow -> True
    _ -> False

  goSpell character = case character of
    InstantCharacteristic{instant_supertypes = supertypes} -> goSupertypes supertypes
    SorceryCharacteristic{sorcery_supertypes = supertypes} -> goSupertypes supertypes
    _ -> assert False False

  goAbilitySource someZo = case someZo of
    SomeActivatedAbility{someActivatedZO = zo} -> do
      let i = getObjectId zo
      let zoPerm0 = zo0ToPermanent $ toZO0 i
      fromRO (findPermanent zoPerm0) <&> \case
        Nothing -> False
        Just perm -> (`any` permanentSupertypes perm) \ty -> fromSomeOT ty \case
          Ty.Snow -> True
          _ -> False

dealDamage' ::
  Monad m =>
  ZO zone OTNDamageSource ->
  ZOCreaturePlayerPlaneswalker ->
  Damage var ->
  Magic 'Private 'RW m [Ev]
dealDamage' _oSource oVictim (forceVars -> Damage damage) = logCall 'dealDamage' case damage of
  0 -> pure mempty -- (120.8)
  _ -> do
    fromRO (findPermanent $ zo0ToPermanent $ toZO0 oVictim) >>= \case
      Nothing -> pure ()
      Just perm -> do
        pure () -- TODO: indestructible
        -- XXX: What happens if damage is dealt to a permanent that is both a creature and a planeswalker?
        case permanentCreature perm of
          Nothing -> pure ()
          Just{} -> do
            setPermanent
              (zo0ToPermanent $ toZO0 oVictim)
              $ Just
                perm
                  { permanentCreatureDamage = (damage +) <$> permanentCreatureDamage perm
                  }
        case permanentPlaneswalker perm of
          Nothing -> pure ()
          Just{} -> undefined
    oPlayers <- fromPublicRO getAlivePlayers
    M.forM_ oPlayers \oPlayer -> case getObjectId oVictim == getObjectId oPlayer of
      False -> pure ()
      True -> do
        player <- fromRO $ getPlayer oPlayer
        let life = unLife $ playerLife player
        setPlayer oPlayer player{playerLife = Life $ life - damage}
    pure mempty

destroy' ::
  Monad m =>
  Maybe SourceZO ->
  ZOPermanent ->
  Magic 'Private 'RW m [Ev]
destroy' _mSource oPerm = logCall 'destroy' do
  fromRO (findPermanent oPerm) >>= \case
    Nothing -> pure ()
    Just perm -> do
      pure () -- TODO: indestructible
      case permanentCard perm of
        Right _anyToken -> pure ()
        Left anyCard -> do
          owner <- fromRO $ ownerOf oPerm
          M.void $ pushGraveyardCard owner anyCard
      setPermanent oPerm Nothing
  pure mempty

putOntoBattlefield' ::
  (IsZO zone ot, CoPermanent ot, Monad m) =>
  Object 'OTPlayer ->
  ZO zone ot ->
  Magic 'Private 'RW m [Ev]
putOntoBattlefield' oPlayer zo = logCall 'putOntoBattlefield' do
  rewindNothing (putOntoBattlefield oPlayer zo) >>= \case
    Nothing -> pure mempty
    Just zoPerm -> pure [EvEntersBattlefield zoPerm]

searchLibrary' ::
  forall ot m.
  (CoCard ot, IsOTN ot, Monad m) =>
  Maybe SourceZO ->
  Object 'OTPlayer ->
  Object 'OTPlayer ->
  WithLinkedObject (Elect 'ResolveStage (Effect 'OneShot)) 'ZLibrary ot ->
  Magic 'Private 'RW m [Ev]
searchLibrary' mSource oSearcher oSearchee zoLibToElectEffect = logCall 'searchLibrary' do
  fromRO (findPlayer oSearchee) >>= \case
    Nothing -> pure mempty -- Don't complain. This can naturally happen if a player in a multiplayer game loses before `enact'` resolves.
    Just searchee -> do
      let library = playerLibrary searchee
      case library of
        Library zoLibs -> do
          let req = RAnd $ getWithLinkedObjectRequirements zoLibToElectEffect
          zoCandidates <- fromRO (zosSatisfying @ 'ZLibrary @ot req)
          let iCandidates = Set.fromList $ map getObjectId zoCandidates
          let zoLibs' = filter (\zo -> getObjectId zo `Set.member` iCandidates) zoLibs
          case zoLibs' of
            [] -> pure mempty
            zo : zos -> do
              prompt <- fromRO $ gets magicPrompt
              opaque <- fromRO $ gets mkOpaqueGameState
              zoLib <- untilJust \attempt -> do
                zoLib <- M.lift $ promptPickZO prompt attempt opaque oSearcher $ zo :| zos
                case zoLib `elem` zoLibs' of
                  True -> pure $ Just zoLib
                  False -> pure Nothing
              let library' = Library $ filter (/= zoLib) zoLibs
              setPlayer oSearchee searchee{playerLibrary = library'}
              let uLib = getUntypedObject zoLib
              let zoLib' = ZO SZLibrary $ uoToON @ot uLib
              let electEffect = reifyWithLinkedObject zoLib' zoLibToElectEffect
              stack <- fromRO $ gets magicStack
              case stack of
                Stack [] -> undefined -- XXX: Can't happen?
                Stack (top : _) -> do
                  let top' = stackObjectToZo0 top
                  let go = fmap Just . enact' mSource
                  mEvs <- rewindNothing $ performElections top' go electEffect
                  setPlayer oSearchee searchee{playerLibrary = library}
                  pure case mEvs of
                    Nothing -> []
                    Just evs -> evs

shuffleLibrary' :: Monad m => Maybe SourceZO -> Object 'OTPlayer -> Magic 'Private 'RW m [Ev]
shuffleLibrary' _mSource oPlayer = logCall 'shuffleLibrary' do
  prompt <- fromRO $ gets magicPrompt
  player <- fromRO $ getPlayer oPlayer
  let library = fromCardList $ playerLibrary player
      count = length library
      ordered = [0 .. count - 1]
  ordering <- untilJust \attempt -> fromPublic $ fromRO do
    ordering <- M.lift $ promptShuffle prompt attempt (CardCount count) oPlayer
    case List.sort (map unCardIndex ordering) == ordered of
      True -> pure $ Just ordering
      False -> do
        M.lift $ exceptionInvalidShuffle prompt (CardCount count) ordering
        pure Nothing
  let library' = map snd $ List.sortOn fst $ zip ordering library
  setPlayer oPlayer $ player{playerLibrary = toCardList library'}
  pure mempty

drawCard' :: Monad m => Maybe SourceZO -> Object 'OTPlayer -> Magic 'Private 'RW m [Ev]
drawCard' _mSource oPlayer = logCall 'drawCard' do
  player <- fromRO $ getPlayer oPlayer
  let library = playerLibrary player
  case popCard library of
    Nothing -> setPlayer oPlayer player{playerDrewFromEmptyLibrary = True}
    Just (libCard, _) -> do
      mCard <- removeLibraryCard oPlayer libCard
      let card = case mCard of
            Nothing -> error $ show CantHappenByConstruction
            Just c -> c
      M.void $ pushHandCard oPlayer card
  pure mempty

drawCards' :: Monad m => Maybe SourceZO -> Int -> Object 'OTPlayer -> Magic 'Private 'RW m [Ev]
drawCards' mSource n oPlayer = logCall 'drawCards' do
  fmap mconcat $ M.replicateM n $ drawCard' mSource oPlayer

untap' :: (CoPermanent ot, Monad m) => Maybe SourceZO -> ZO 'ZBattlefield ot -> Magic 'Private 'RW m [Ev]
untap' mSource zoPerm' = logCall 'untap' do
  let zoPerm = zo0ToPermanent $ toZO0 zoPerm'
  perm <- fromRO $ getPermanent zoPerm
  setPermanent zoPerm $ Just perm{permanentTapped = Untapped}
  case permanentTapped perm of
    Untapped -> pure []
    Tapped -> pure [EvUntapped mSource zoPerm]

tap' :: (CoPermanent ot, Monad m) => Maybe SourceZO -> ZO 'ZBattlefield ot -> Magic 'Private 'RW m [Ev]
tap' mSource zoPerm' = logCall 'tap' do
  let zoPerm = zo0ToPermanent $ toZO0 zoPerm'
  perm <- fromRO $ getPermanent zoPerm
  setPermanent zoPerm $ Just perm{permanentTapped = Tapped}
  case permanentTapped perm of
    Tapped -> pure []
    Untapped -> pure [EvTapped mSource zoPerm]
