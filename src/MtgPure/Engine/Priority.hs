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
  getHasPriority,
  getPlayerWithPriority,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Util (Attempt, Attempt' (..))
import safe Data.Functor ((<&>))
import safe qualified Data.Stream as Stream
import safe Data.Void (Void, absurd)
import safe MtgPure.Engine.Fwd.Api (
  activateAbility,
  castSpell,
  getAPNAP,
  getAlivePlayerCount,
  performStateBasedActions,
  playLand,
  resolveTopOfStack,
  rewindIllegal,
  rewindIllegalActivation,
 )
import safe MtgPure.Engine.Monad (
  fromPublic,
  fromPublicRO,
  fromRO,
  get,
  gets,
  internalFromPrivate,
  liftCont,
  magicCont,
  modify,
 )
import safe MtgPure.Engine.Prompt (
  ActivateAbility,
  ActivateResult (..),
  CastSpell,
  EnactInfo (..),
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
  logCall,
  logCallRec,
  mkOpaqueGameState,
  runMagicCont,
 )
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))

gainPriority :: Monad m => Object 'OTPlayer -> Magic 'Private 'RW m ()
gainPriority oPlayer = do
  logCall 'gainPriority do
    PlayerCount n <- fromPublicRO getAlivePlayerCount
    ps <- fromRO $ Stream.take n . Stream.dropWhile (/= oPlayer) <$> getAPNAP
    modify \st -> st{magicPlayerOrderPriority = ps}
  runMagicCont (either id absurd) runPriorityQueue

runPriorityQueue :: Monad m => MagicCont 'Private 'RW m () Void
runPriorityQueue = do
  goVoid <- logCall 'runPriorityQueue do
    liftCont (fromRO $ gets magicPlayerOrderPriority) >>= \case
      [] -> pure resolveTopOfStack -- (117.4)
      oPlayer : oPlayers -> do
        liftCont performStateBasedActions -- (117.5)
        askPriorityAction oPlayer
        liftCont $ modify \st -> st{magicPlayerOrderPriority = oPlayers} -- (117.3d)
        pure runPriorityQueue
  goVoid

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

askPriorityAction :: Monad m => Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askPriorityAction = logCall 'askPriorityAction $ askPriorityAction' $ Attempt 0

askPriorityAction' :: Monad m => Attempt -> Object 'OTPlayer -> MagicCont 'Private 'RW m () ()
askPriorityAction' attempt oPlayer = logCallRec 'askPriorityAction' do
  st <- liftCont $ fromRO get
  let opaque = mkOpaqueGameState st
      prompt = magicPrompt st
  action <- liftCont $
    fromPublic $ fromRO do
      promptPriorityAction prompt attempt opaque oPlayer
  performPriorityActionCont oPlayer action >>= \case
    Pass -> pure ()
    TryAgain mAttempt ->
      let succAttempt = (1 +) <$> attempt
          attempt' = case mAttempt of
            Nothing -> succAttempt
            Just a@(Attempt n) -> case n >= 0 of
              False -> succAttempt
              True -> a
       in askPriorityAction' attempt' oPlayer

-- No need to encode success results, since the continuation is hijacked in such a case.
data PriorityActionResult where
  Pass :: PriorityActionResult
  TryAgain :: Maybe Attempt -> PriorityActionResult

performPriorityActionCont ::
  forall m.
  Monad m =>
  Object 'OTPlayer ->
  PriorityAction () ->
  MagicCont 'Private 'RW m () PriorityActionResult
performPriorityActionCont oPlayer action = logCall 'performPriorityActionCont do
  let f :: MagicCont 'Private 'RW m () () -> MagicCont 'Private 'RW m () PriorityActionResult
      f m = m >> pure (TryAgain Nothing)

      go :: PriorityAction a -> MagicCont 'Private 'RW m () PriorityActionResult
      go x = case x of
        ActivateAbility{} -> f $ activateAbilityCont oPlayer x -- (117.1b) (117.1d)
        AskPriorityActionAgain mAttempt -> pure $ TryAgain mAttempt
        CastSpell{} -> f $ castSpellCont oPlayer x -- (117.1a)
        Concede{} -> undefined -- TODO: concede
        PassPriority -> pure Pass
        SpecialAction y -> f $ specialActionCont oPlayer y -- (117.1c)
        PriorityAction y -> go y
  go action

activateAbilityCont :: Monad m => Object 'OTPlayer -> PriorityAction ActivateAbility -> MagicCont 'Private 'RW m () ()
activateAbilityCont oPlayer activate = logCall 'activateAbilityCont do
  result <- liftCont $ rewindIllegalActivation $ activateAbility oPlayer activate
  let goNormal = magicCont $ gainPriority oPlayer -- (117.3c)
  case result of
    IllegalActivation -> pure ()
    ActivatedNonManaAbility -> goNormal
    ActivatedManaAbility info -> case enactInfo_endTheTurn info of
      False -> goNormal
      True -> undefined -- TODO: end the turn

castSpellCont :: Monad m => Object 'OTPlayer -> PriorityAction CastSpell -> MagicCont 'Private 'RW m () ()
castSpellCont oPlayer cast = logCall 'castSpellCont do
  isLegal <- liftCont $ rewindIllegal $ castSpell oPlayer cast
  case isLegal of
    True -> magicCont $ gainPriority oPlayer -- (117.3c)
    False -> pure ()

playLandCont :: Monad m => Object 'OTPlayer -> SpecialAction PlayLand -> MagicCont 'Private 'RW m () ()
playLandCont oPlayer special = logCall 'playLandCont do
  isLegal <- liftCont $ rewindIllegal $ playLand oPlayer special
  case isLegal of
    True -> magicCont $ gainPriority oPlayer -- (117.3c)
    False -> pure ()

-- (117.1c)
specialActionCont :: Monad m => Object 'OTPlayer -> SpecialAction a -> MagicCont 'Private 'RW m () ()
specialActionCont oPlayer action = logCall 'specialActionCont case action of
  PlayLand{} -> playLandCont oPlayer action
