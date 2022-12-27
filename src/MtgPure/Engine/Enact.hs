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
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Util (untilJust)
import safe qualified Data.List as List
import safe MtgPure.Engine.Fwd.Api (
  allPlayers,
  caseOf,
  findPermanent,
  findPlayer,
  getPermanent,
  getPlayer,
  pushHandCard,
  removeLibraryCard,
  setPermanent,
  setPlayer,
 )
import safe MtgPure.Engine.Monad (fromPublicRO, fromRO, gets)
import safe MtgPure.Engine.Prompt (
  CardCount (..),
  CardIndex (..),
  InternalLogicError (..),
  Prompt' (..),
 )
import safe MtgPure.Engine.State (GameState (..), Magic, logCall)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.EffectType (EffectType (..))
import safe MtgPure.Model.IsCardList (IsCardList (..), popCard)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana (Snow (..))
import safe MtgPure.Model.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Object (Object)
import safe MtgPure.Model.ObjectId (getObjectId)
import safe MtgPure.Model.ObjectType (ObjectType (..))
import safe MtgPure.Model.ObjectType.Kind (OTDamageSource, OTPermanent)
import safe MtgPure.Model.Permanent (Permanent (..), Tapped (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Effect (..))
import safe MtgPure.Model.Variable (forceVars)
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject (ZO, ZOCreaturePlayerPlaneswalker, ZOPlayer)
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent, zo1ToO)

enact :: Monad m => Effect 'OneShot -> Magic 'Private 'RW m ()
enact = logCall 'enact \case
  AddMana oPlayer mana -> addMana' oPlayer mana
  DealDamage oSource oVictim damage -> dealDamage' oSource oVictim damage
  DrawCards oPlayer amount -> drawCards' amount $ zo1ToO oPlayer
  EffectCase case_ -> caseOf enact case_
  Sequence effects -> mapM_ enact effects
  ShuffleLibrary oPlayer -> shuffleLibrary' $ zo1ToO oPlayer
  Tap oPerm -> M.void $ tap' oPerm
  Untap oPerm -> M.void $ untap' oPerm
  _ -> undefined

addMana' :: Monad m => ZOPlayer -> ManaPool 'NonSnow -> Magic 'Private 'RW m ()
addMana' oPlayer mana = logCall 'addMana' do
  fromRO (findPlayer $ zo1ToO oPlayer) >>= \case
    Nothing -> pure ()
    Just player -> do
      let mana' = playerMana player + mempty{poolNonSnow = mana}
      setPlayer (zo1ToO oPlayer) player{playerMana = mana'}

dealDamage' ::
  Monad m =>
  ZO zone OTDamageSource ->
  ZOCreaturePlayerPlaneswalker ->
  Damage var ->
  Magic 'Private 'RW m ()
dealDamage' _oSource oVictim (forceVars -> Damage damage) = logCall 'dealDamage' do
  fromRO (findPermanent $ zo0ToPermanent $ toZO0 oVictim) >>= \case
    Nothing -> pure ()
    Just perm -> do
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
        Just () -> undefined
  oPlayers <- fromPublicRO allPlayers
  M.forM_ oPlayers \oPlayer -> case getObjectId oVictim == getObjectId oPlayer of
    False -> pure ()
    True -> do
      player <- fromRO $ getPlayer oPlayer
      let life = unLife $ playerLife player
      setPlayer oPlayer player{playerLife = Life $ life - damage}

shuffleLibrary' :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
shuffleLibrary' oPlayer = logCall 'shuffleLibrary' do
  prompt <- fromRO $ gets magicPrompt
  player <- fromRO $ getPlayer oPlayer
  let library = fromCardList $ playerLibrary player
      count = length library
      ordered = [0 .. count - 1]
  ordering <- lift $
    untilJust \attempt -> do
      ordering <- promptShuffle prompt attempt (CardCount count) oPlayer
      case List.sort (map unCardIndex ordering) == ordered of
        True -> pure $ Just ordering
        False -> do
          exceptionInvalidShuffle prompt (CardCount count) ordering
          pure Nothing
  let library' = map snd $ List.sortOn fst $ zip ordering library
  setPlayer oPlayer $ player{playerLibrary = toCardList library'}

drawCard' :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
drawCard' oPlayer = logCall 'drawCard' do
  player <- fromRO $ getPlayer oPlayer
  let library = playerLibrary player
  case popCard library of
    Nothing -> setPlayer oPlayer player{playerDrewFromEmptyLibrary = True}
    Just (libCard, _) -> do
      mCard <- removeLibraryCard oPlayer libCard
      let card = case mCard of
            Nothing -> error $ show ObjectIdExistsAndAlsoDoesNotExist
            Just c -> c
      M.void $ pushHandCard oPlayer card

drawCards' :: Monad m => Int -> Object 'OTPlayer -> Magic 'Private 'RW m ()
drawCards' n = logCall 'drawCards' $ M.replicateM_ n . drawCard'

untap' :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RW m Bool
untap' oPerm = logCall 'untap' do
  perm <- fromRO $ getPermanent oPerm
  setPermanent oPerm $ Just perm{permanentTapped = Untapped}
  pure $ permanentTapped perm /= Untapped

tap' :: Monad m => ZO 'ZBattlefield OTPermanent -> Magic 'Private 'RW m Bool
tap' oPerm = logCall 'tap' do
  perm <- fromRO $ getPermanent oPerm
  setPermanent oPerm $ Just perm{permanentTapped = Tapped}
  pure $ permanentTapped perm /= Tapped
