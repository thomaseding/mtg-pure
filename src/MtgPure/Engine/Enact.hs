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

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Util (untilJust)
import safe qualified Data.Foldable as F
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Engine.Fwd.Api (
  caseOf,
  findPermanent,
  findPlayer,
  getAlivePlayers,
  getPermanent,
  getPlayer,
  pushHandCard,
  removeLibraryCard,
  setPermanent,
  setPlayer,
 )
import safe MtgPure.Engine.Monad (fromPublic, fromPublicRO, fromRO, gets)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.Prompt (
  CardCount (..),
  CardIndex (..),
  Ev (..),
  InternalLogicError (..),
  Prompt' (..),
  SourceZO (..),
  TriggerTime (..),
 )
import safe MtgPure.Engine.State (GameState (..), Magic, logCall)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsCardList (IsCardList (..), popCard)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Mana.Snow (Snow (..))
import safe MtgPure.Model.Object.OTNAliases (OTNDamageSource)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (getObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Permanent (Permanent (..), Tapped (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Effect (..))
import safe MtgPure.Model.Variable (forceVars)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent, zo1ToO)
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO, ZOCreaturePlayerPlaneswalker, ZOPermanent, ZOPlayer)

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

enact :: Monad m => Maybe SourceZO -> Effect 'OneShot -> Magic 'Private 'RW m [Ev]
enact mSource effect = logCall 'enact do
  evs <- enact' mSource effect
  triggerEvListeners AfterResolution evs
  pure evs

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
  PutOntoBattlefield{} -> undefined
  Sacrifice{} -> undefined
  SearchLibrary{} -> undefined
  Sequence effects -> mconcat <$> mapM (enact' mSource) effects
  ShuffleLibrary oPlayer -> shuffleLibrary' mSource $ zo1ToO oPlayer
  Tap oPerm -> tap' mSource oPerm
  Untap oPerm -> untap' mSource oPerm
  WithList{} -> undefined

addMana' :: Monad m => Maybe SourceZO -> ZOPlayer -> ManaPool 'NonSnow -> Magic 'Private 'RW m [Ev]
addMana' _mSource oPlayer mana = logCall 'addMana' do
  fromRO (findPlayer $ zo1ToO oPlayer) >>= \case
    Nothing -> pure () -- Don't complain. This can naturally happen if a player loses before `enact'` resolves.
    Just player -> do
      let mana' = playerMana player + mempty{poolNonSnow = mana}
      setPlayer (zo1ToO oPlayer) player{playerMana = mana'}
  pure mempty

dealDamage' ::
  Monad m =>
  ZO zone OTNDamageSource ->
  ZOCreaturePlayerPlaneswalker ->
  Damage var ->
  Magic 'Private 'RW m [Ev]
dealDamage' _oSource oVictim (forceVars -> Damage damage) = logCall 'dealDamage' do
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
    Just _perm -> do
      pure () -- TODO: indestructible
      setPermanent oPerm Nothing
  pure mempty

shuffleLibrary' :: Monad m => Maybe SourceZO -> Object 'OTPlayer -> Magic 'Private 'RW m [Ev]
shuffleLibrary' _mSource oPlayer = logCall 'shuffleLibrary' do
  prompt <- fromRO $ gets magicPrompt
  player <- fromRO $ getPlayer oPlayer
  let library = fromCardList $ playerLibrary player
      count = length library
      ordered = [0 .. count - 1]
  ordering <- untilJust \attempt -> fromPublic $ fromRO do
    ordering <- promptShuffle prompt attempt (CardCount count) oPlayer
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
