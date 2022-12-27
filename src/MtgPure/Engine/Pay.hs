{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

module MtgPure.Engine.Pay (
  pay,
) where

import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Util (untilJust)
import safe Data.Functor ((<&>))
import safe Data.List.NonEmpty (NonEmpty (..))
import safe Data.Monoid (First (..))
import safe MtgPure.Engine.Fwd.Api (
  enact,
  findPlayer,
  satisfies,
  setPlayer,
  zosSatisfying,
 )
import safe MtgPure.Engine.Legality (Legality (..), toLegality)
import safe MtgPure.Engine.Monad (fromRO, gets)
import safe MtgPure.Engine.Prompt (Prompt' (..))
import safe MtgPure.Engine.State (GameState (..), Magic, logCall, mkOpaqueGameState)
import safe MtgPure.Model.Mana (IsManaNoVar, IsSnow, Mana)
import safe MtgPure.Model.ManaCost (ManaCost (..))
import safe MtgPure.Model.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Object (Object)
import safe MtgPure.Model.ObjectN (ObjectN (..))
import safe MtgPure.Model.ObjectType (ObjectType (..))
import safe MtgPure.Model.ObjectType.Permanent (CoPermanent)
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Cost (..), Effect (..), Requirement (..))
import safe MtgPure.Model.Variable (ForceVars (forceVars), Var (..))
import safe MtgPure.Model.Zone (SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject (IsZO, ZoneObject (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)
import safe MtgPure.ModelCombinators (isTapped)

pay :: Monad m => Object 'OTPlayer -> Cost ot -> Magic 'Private 'RW m Legality
pay oPlayer = logCall 'pay \case
  AndCosts costs -> payAndCosts oPlayer costs
  ManaCost manaCost -> payManaCost oPlayer manaCost
  OrCosts costs -> payOrCosts oPlayer costs
  TapCost reqs -> payTapCost oPlayer $ RAnd reqs
  _ -> undefined

class FindMana manas var | manas -> var where
  findMana ::
    manas ->
    ( forall snow color.
      (IsSnow snow, IsManaNoVar snow color) =>
      Mana var snow color ->
      Maybe x
    ) ->
    Maybe x

instance FindMana (ManaCost var) var where
  findMana (ManaCost' w u b r g c x s) f =
    getFirst $ mconcat $ map First [f w, f u, f b, f r, f g, f c, f x, f s]

instance IsSnow snow => FindMana (ManaPool snow) 'NoVar where
  findMana (ManaPool w u b r g c) f =
    getFirst $ mconcat $ map First [f w, f u, f b, f r, f g, f c]

instance FindMana CompleteManaPool 'NoVar where
  findMana pool f =
    getFirst $ mconcat $ map First [poolNonSnow pool `findMana` f, poolSnow pool `findMana` f]

-- TODO: Give this a legit impl.
-- TODO: Need to prompt for generic mana payments
payManaCost :: Monad m => Object 'OTPlayer -> ManaCost 'Var -> Magic 'Private 'RW m Legality
payManaCost oPlayer (forceVars -> cost) = logCall 'payManaCost do
  fromRO (findPlayer oPlayer) >>= \case
    Nothing -> pure Illegal
    Just player -> do
      let pool = poolNonSnow $ playerMana player
          w = poolWhite pool - costWhite cost
          u = poolBlue pool - costBlue cost
          b = poolBlack pool - costBlack cost
          r = poolRed pool - costRed cost
          g = poolGreen pool - costGreen cost
          c = poolColorless pool - costColorless cost
          pool' = ManaPool w u b r g c
          isBad mana = case mana < 0 of
            True -> Nothing
            False -> Just ()
      case findMana pool' isBad of
        Just () -> pure Illegal
        Nothing -> do
          setPlayer oPlayer player{playerMana = (playerMana player){poolNonSnow = pool'}}
          pure Legal

payTapCost ::
  (Monad m, IsZO 'ZBattlefield ot, CoPermanent ot) =>
  Object 'OTPlayer ->
  Requirement 'ZBattlefield ot ->
  Magic 'Private 'RW m Legality
payTapCost oPlayer req = logCall 'payTapCost do
  prompt <- fromRO $ gets magicPrompt
  fromRO (zosSatisfying req') >>= \case
    [] -> do
      lift $ promptDebugMessage prompt $ show ("payTapCost no zos satisfying" :: String)
      pure Illegal
    zos@(zosHead : zosTail) -> do
      opaque <- fromRO $ gets mkOpaqueGameState
      zo <- lift $
        untilJust \attempt -> do
          zo <- promptPickZO prompt attempt opaque oPlayer $ zosHead :| zosTail
          pure case zo `elem` zos of
            False -> Nothing
            True -> Just zo
      let oPerm = zo0ToPermanent $ toZO0 zo
      shouldBeUntapped <- fromRO $ satisfies oPerm isTapped <&> toLegality
      lift $ promptDebugMessage prompt $ show ("payTapCost tapping..." :: String, shouldBeUntapped)
      enact $ Tap oPerm
      shouldBeTapped <- fromRO $ satisfies oPerm isTapped <&> toLegality
      lift $ promptDebugMessage prompt $ show ("...payTapCost done" :: String, shouldBeTapped)
      fromRO $ satisfies oPerm isTapped <&> toLegality
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
    pay oPlayer cost >>= \case
      Illegal -> pure Illegal
      Legal -> payAndCosts oPlayer costs

payOrCosts :: Monad m => Object 'OTPlayer -> [Cost ot] -> Magic 'Private 'RW m Legality
payOrCosts oPlayer = logCall 'payOrCosts \case
  [] -> pure Illegal
  cost : _costs ->
    pay oPlayer cost >>= \case
      Legal -> pure Legal -- TODO: Offer other choices
      Illegal -> undefined
