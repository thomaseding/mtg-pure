{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant flip" #-}

module MtgPure.Engine.Pay (
  pay,
) where

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Data.Functor ((<&>))
import safe MtgPure.Engine.Fwd.Api (
  enact,
  findPlayer,
  getPermanent,
  pickOneZO,
  pushGraveyardCard,
  satisfies,
  setPermanent,
  setPlayer,
  zosSatisfying,
 )
import safe MtgPure.Engine.Legality (Legality (..), toLegality)
import safe MtgPure.Engine.Monad (fromPublic, fromRO)
import safe MtgPure.Engine.PayMana (payManaCost)
import safe MtgPure.Engine.Prompt (SourceZO (..))
import safe MtgPure.Engine.State (
  Magic,
  logCall,
 )
import safe MtgPure.Model.Combinators (isTapped)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectN_ (ObjectN' (..))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Cost (..), Effect (..), Requirement (..))
import safe MtgPure.Model.Zone (SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (oToZO1, toZO0, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZoneObject (..))

pay :: Monad m => Object 'OTPlayer -> Cost ot -> Magic 'Private 'RW m Legality
pay oPlayer = logCall 'pay do
  -- TODO: allow player to activate mana abilities iff the payment requires a mana cost.
  -- Prolly actually should be done on each mana payment attempt instead of ahead of time,
  -- since it might require user elections that have not yet been determined, including branching choices.
  pure ()
  payRec oPlayer

payRec :: Monad m => Object 'OTPlayer -> Cost ot -> Magic 'Private 'RW m Legality
payRec oPlayer = logCall 'payRec \case
  AndCosts costs -> payAndCosts oPlayer costs
  CostCase{} -> undefined
  DiscardRandomCost{} -> undefined
  ExileCost{} -> undefined
  LoyaltyCost{} -> undefined
  ManaCost manaCost -> payManaCost oPlayer manaCost
  OrCosts costs -> payOrCosts oPlayer costs
  PayLife life -> payLife oPlayer $ Life life
  SacrificeCost reqs -> paySacrificeCost oPlayer $ RAnd reqs
  TapCost reqs -> payTapCost oPlayer $ RAnd reqs

payLife :: Monad m => Object 'OTPlayer -> Life -> Magic 'Private 'RW m Legality
payLife oPlayer life = logCall 'payLife do
  fromRO (findPlayer oPlayer) >>= \case
    Nothing -> pure Illegal
    Just player -> do
      let life' = playerLife player - life
      case life' < 0 of
        True -> pure Illegal
        False -> do
          setPlayer oPlayer player{playerLife = life'}
          pure Legal

paySacrificeCost ::
  (Monad m, IsZO 'ZBattlefield ot, CoPermanent ot) =>
  Object 'OTPlayer ->
  Requirement 'ZBattlefield ot ->
  Magic 'Private 'RW m Legality
paySacrificeCost oPlayer req = logCall 'paySacrificeCost do
  fromRO (zosSatisfying req') >>= (\zos -> fromPublic $ pickOneZO oPlayer zos) >>= \case
    Nothing -> pure Illegal
    Just zo -> do
      let zoPerm = zo0ToPermanent $ toZO0 zo
      perm <- fromRO $ getPermanent zoPerm
      setPermanent zoPerm Nothing
      case permanentCard perm of
        Left card -> M.void $ pushGraveyardCard oPlayer card
        Right _token -> pure ()
      pure Legal
 where
  req' =
    RAnd
      [ ControlledBy $ ZO SZBattlefield $ O1 oPlayer
      , req
      ]

payTapCost ::
  (Monad m, IsZO 'ZBattlefield ot, CoPermanent ot) =>
  Object 'OTPlayer ->
  Requirement 'ZBattlefield ot ->
  Magic 'Private 'RW m Legality
payTapCost oPlayer req = logCall 'payTapCost do
  fromRO (zosSatisfying req') >>= (\zos -> fromPublic $ pickOneZO oPlayer zos) >>= \case
    Nothing -> pure Illegal
    Just zo -> do
      let zoPerm = zo0ToPermanent $ toZO0 zo
          zoPlayer = oToZO1 @ 'ZBattlefield oPlayer
      M.void $ enact (Just $ SourceZO zoPlayer) $ Tap zoPerm
      fromRO $ satisfies zoPerm isTapped <&> toLegality
 where
  req' =
    RAnd
      [ ControlledBy $ ZO SZBattlefield $ O1 oPlayer
      , Not isTapped
      , req
      ]

payAndCosts :: Monad m => Object 'OTPlayer -> [Cost ot] -> Magic 'Private 'RW m Legality
payAndCosts oPlayer = logCall 'payAndCosts \case
  [] -> pure Legal
  cost : costs ->
    payRec oPlayer cost >>= \case
      Illegal -> pure Illegal
      Legal -> payAndCosts oPlayer costs

payOrCosts :: Monad m => Object 'OTPlayer -> [Cost ot] -> Magic 'Private 'RW m Legality
payOrCosts oPlayer = logCall 'payOrCosts \case
  [] -> pure Illegal
  cost : _costs ->
    payRec oPlayer cost >>= \case
      Legal -> pure Legal -- TODO: Offer other choices through prompt
      Illegal -> undefined
