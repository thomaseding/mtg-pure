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

import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Util (untilJust)
import safe Data.Functor ((<&>))
import safe Data.Monoid (First (..))
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
import safe MtgPure.Engine.Monad (fromRO, gets)
import safe MtgPure.Engine.Prompt (Prompt' (..))
import safe MtgPure.Engine.State (GameState (..), Magic, logCall, mkOpaqueGameState)
import safe MtgPure.Model.CountMana (countMana)
import safe MtgPure.Model.GenericMana (GenericMana (..))
import safe MtgPure.Model.Mana (IsManaNoVar, IsSnow, Mana)
import safe MtgPure.Model.ManaCost (ManaCost (..))
import safe MtgPure.Model.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectN_ (ObjectN' (..))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Cost (..), Effect (..), Requirement (..))
import safe MtgPure.Model.Variable (ForceVars (forceVars), Var (..))
import safe MtgPure.Model.Zone (SZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0, zo0ToPermanent)
import safe MtgPure.Model.ZoneObject.ZoneObject (IsZO, ZoneObject (..))
import safe MtgPure.ModelCombinators (isTapped)

pay :: Monad m => Object 'OTPlayer -> Cost ot -> Magic 'Private 'RW m Legality
pay oPlayer = logCall 'pay \case
  AndCosts costs -> payAndCosts oPlayer costs
  CostCase{} -> undefined
  DiscardRandomCost{} -> undefined
  LoyaltyCost{} -> undefined
  ManaCost manaCost -> payManaCost oPlayer manaCost
  OrCosts costs -> payOrCosts oPlayer costs
  PayLife{} -> undefined
  SacrificeCost reqs -> paySacrificeCost oPlayer $ RAnd reqs
  TapCost reqs -> payTapCost oPlayer $ RAnd reqs

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

-- TODO: snow costs and snow payments
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
            True -> Just ()
            False -> Nothing
      case findMana pool' isBad of
        Just () -> pure Illegal
        Nothing -> do
          let avail = countMana pool'
              generic = countMana $ costGeneric cost
              generic' = GenericMana' generic
          case generic > avail of
            True -> pure Illegal
            False -> do
              payment <- case generic of
                0 -> pure mempty
                _ -> do
                  prompt <- fromRO $ gets magicPrompt
                  opaque <- fromRO $ gets mkOpaqueGameState
                  untilJust \attempt -> do
                    fullPayment <- M.lift $ promptPayGeneric prompt attempt opaque oPlayer generic'
                    let payment = poolNonSnow fullPayment
                    case countMana payment == generic of
                      True -> pure $ Just payment
                      False -> do
                        M.lift $ exceptionInvalidGenericManaPayment prompt generic' fullPayment
                        pure Nothing
              let pool'' = pool' - payment
              setPlayer oPlayer player{playerMana = (playerMana player){poolNonSnow = pool''}}
              pure Legal

paySacrificeCost ::
  (Monad m, IsZO 'ZBattlefield ot, CoPermanent ot) =>
  Object 'OTPlayer ->
  Requirement 'ZBattlefield ot ->
  Magic 'Private 'RW m Legality
paySacrificeCost oPlayer req = logCall 'paySacrificeCost do
  fromRO (zosSatisfying req' >>= pickOneZO oPlayer) >>= \case
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
  fromRO (zosSatisfying req' >>= pickOneZO oPlayer) >>= \case
    Nothing -> pure Illegal
    Just zo -> do
      let zoPerm = zo0ToPermanent $ toZO0 zo
      M.void $ enact $ Tap zoPerm
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
