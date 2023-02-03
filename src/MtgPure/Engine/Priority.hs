{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Use join" #-}

module MtgPure.Engine.Priority (
  askPriorityAction,
  gainPriority,
  bailGainPriority,
  getHasPriority,
  getPlayerWithPriority,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Util (Attempt, Attempt' (..))
import safe Data.Functor ((<&>))
import safe qualified Data.Stream as Stream
import safe Data.Void (Void, absurd)
import safe MtgPure.Engine.Fwd.Api (
  activateAbility,
  castSpell,
  endTheTurn,
  getAPNAP,
  getAlivePlayerCount,
  performStateBasedActions,
  playLand,
  resolveTopOfStack,
  rewindIllegal,
  rewindIllegalActivation,
 )
import safe MtgPure.Engine.Monad (
  PriorityEnd,
  fromPublic,
  fromPublicRO,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  liftCont,
  magicContBail,
  modify,
 )
import safe MtgPure.Engine.Prompt (
  ActivateAbility,
  ActivateResult (..),
  CastSpell,
  Ev (EvEndTheTurn),
  PlayLand,
  PlayerCount (..),
  PriorityAction (..),
  Prompt' (promptPriorityAction),
  SpecialAction (..),
 )
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  MagicCont,
  ToPriorityEnd (toPriorityEnd),
  logCall,
  mkOpaqueGameState,
  runMagicCont,
 )
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))

gainPriority :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW Void m ()
gainPriority oPlayer = do
  logCall 'gainPriority do
    liftCont do
      PlayerCount n <- fromPublicRO getAlivePlayerCount
      ps <- fromRO $ Stream.take n . Stream.dropWhile (/= oPlayer) <$> getAPNAP
      modify \st -> st{magicPlayerOrderPriority = ps}
  endOrVoid <- liftCont $ runMagicCont runPriorityQueue
  case endOrVoid of
    Right v -> absurd v
    Left end -> case end of
      Left v -> absurd v
      Right () -> pure ()

bailGainPriority :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW PriorityEnd m a
bailGainPriority = magicContBail . toPriorityEnd . gainPriority

bailEndTheTurn :: Monad m => MagicCont 'Private 'RW PriorityEnd m ()
bailEndTheTurn = magicContBail $ toPriorityEnd endTheTurn

bailResolveTopOfStack :: Monad m => MagicCont 'Private 'RW PriorityEnd m Void
bailResolveTopOfStack = magicContBail $ fmap Left resolveTopOfStack

runPriorityQueue :: Monad m => MagicCont 'Private 'RW PriorityEnd m Void
runPriorityQueue = M.join $ logCall 'runPriorityQueue do
  playerOrder <- liftCont $ fromRO $ gets magicPlayerOrderPriority
  case playerOrder of
    [] -> pure bailResolveTopOfStack -- (117.4)
    oPlayer : oPlayers -> do
      liftCont performStateBasedActions -- (117.5)
      askPriorityAction oPlayer
      liftCont $ modify \st -> st{magicPlayerOrderPriority = oPlayers} -- (117.3d)
      pure runPriorityQueue

getPlayerWithPriority :: Monad m => Magic 'Public 'RO m (Maybe (Object 'OTPlayer))
getPlayerWithPriority = logCall 'getPlayerWithPriority do
  oPlayers <- internalFromPrivate $ gets magicPlayerOrderPriority
  pure case oPlayers of
    oPlayer : _ -> Just oPlayer
    [] -> Nothing

getHasPriority :: Monad m => Object 'OTPlayer -> Magic 'Public 'RO m Bool
getHasPriority oPlayer = logCall 'getHasPriority do
  getPlayerWithPriority <&> \case
    Nothing -> False
    Just p -> oPlayer == p

askPriorityAction :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW PriorityEnd m ()
askPriorityAction = logCall 'askPriorityAction $ askPriorityAction' $ Attempt 0

_passPriority :: Monad m => MagicCont 'Private 'RW PriorityEnd m Void
_passPriority = logCall '_passPriority do
  oPlayers <- liftCont $ fromRO $ gets magicPlayerOrderPriority
  let oPlayers' = case oPlayers of
        [] -> undefined -- error
        _ : ps -> ps
  liftCont $ modify \st -> st{magicPlayerOrderPriority = oPlayers'} -- (117.3d)
  runPriorityQueue

askPriorityAction' :: Monad m => Attempt -> Object 'OTPlayer -> MagicCont 'Private 'RW PriorityEnd m ()
askPriorityAction' attempt oPlayer = M.join $ logCall 'askPriorityAction' do
  st <- liftCont $ fromRO get
  let opaque = mkOpaqueGameState st
      prompt = magicPrompt st
  action <- liftCont $
    fromPublic $ fromRO do
      promptPriorityAction prompt attempt opaque oPlayer
  performPriorityActionCont oPlayer action >>= \case
    Pass -> pure $ pure ()
    TryAgain mAttempt -> do
      let succAttempt = (1 +) <$> attempt
          attempt' = case mAttempt of
            Nothing -> succAttempt
            Just a@(Attempt n) -> case n >= 0 of
              False -> succAttempt
              True -> a
      pure $ askPriorityAction' attempt' oPlayer

-- No need to encode success results, since the continuation is hijacked in such a case.
data PriorityActionResult where
  Pass :: PriorityActionResult
  TryAgain :: Maybe Attempt -> PriorityActionResult

performPriorityActionCont ::
  forall m.
  Monad m =>
  Object 'OTPlayer ->
  PriorityAction () ->
  MagicCont 'Private 'RW PriorityEnd m PriorityActionResult
performPriorityActionCont oPlayer action = logCall 'performPriorityActionCont do
  go action
 where
  tryAgainOnFail :: MagicCont 'Private 'RW PriorityEnd m () -> MagicCont 'Private 'RW PriorityEnd m PriorityActionResult
  tryAgainOnFail m = m >> pure (TryAgain Nothing)

  go :: PriorityAction a -> MagicCont 'Private 'RW PriorityEnd m PriorityActionResult
  go x = case x of
    ActivateAbility{} -> tryAgainOnFail $ activateAbilityCont oPlayer x -- (117.1b) (117.1d)
    AskPriorityActionAgain mAttempt -> pure $ TryAgain mAttempt
    CastSpell{} -> tryAgainOnFail $ castSpellCont oPlayer x -- (117.1a)
    Concede{} -> undefined -- TODO: concede
    PassPriority -> pure Pass
    SpecialAction y -> tryAgainOnFail $ specialActionCont oPlayer y -- (117.1c)
    PriorityAction y -> go y

activateAbilityCont ::
  Monad m =>
  Object 'OTPlayer ->
  PriorityAction ActivateAbility ->
  MagicCont 'Private 'RW PriorityEnd m ()
activateAbilityCont oPlayer activate = logCall 'activateAbilityCont do
  result <- liftCont $ rewindIllegalActivation $ activateAbility oPlayer activate
  case result of
    IllegalActivation -> pure ()
    ActivatedNonManaAbility -> goNormal
    ActivatedManaAbility evs -> do
      case any isEndTheTurn evs of
        True -> bailEndTheTurn
        False -> goNormal
 where
  goNormal = bailGainPriority oPlayer -- (117.3c)
  isEndTheTurn = \case
    EvEndTheTurn{} -> True
    _ -> False

castSpellCont :: Monad m => Object 'OTPlayer -> PriorityAction CastSpell -> MagicCont 'Private 'RW PriorityEnd m ()
castSpellCont oPlayer cast = logCall 'castSpellCont do
  isLegal <- liftCont $ rewindIllegal $ castSpell oPlayer cast
  case isLegal of
    True -> bailGainPriority oPlayer -- (117.3c)
    False -> pure ()

playLandCont :: Monad m => Object 'OTPlayer -> SpecialAction PlayLand -> MagicCont 'Private 'RW PriorityEnd m ()
playLandCont oPlayer special = logCall 'playLandCont do
  isLegal <- liftCont $ rewindIllegal $ playLand oPlayer special
  case isLegal of
    True -> bailGainPriority oPlayer -- (117.3c)
    False -> pure ()

-- (117.1c)
specialActionCont :: Monad m => Object 'OTPlayer -> SpecialAction a -> MagicCont 'Private 'RW PriorityEnd m ()
specialActionCont oPlayer action = logCall 'specialActionCont case action of
  PlayLand{} -> playLandCont oPlayer action
