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

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
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
import safe MtgPure.Engine.Monad (fromPublic, fromRO, gets)
import safe MtgPure.Engine.Orphans (mapManaPool)
import safe MtgPure.Engine.Prompt (Prompt' (..), SourceZO (..))
import safe MtgPure.Engine.State (
  GameState (..),
  Magic,
  logCall,
  mkOpaqueGameState,
 )
import safe MtgPure.Model.Combinators (isTapped)
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.CountMana (countMana)
import safe MtgPure.Model.Mana.Mana (IsManaNoVar, Mana (..), freezeMana, thawMana)
import safe MtgPure.Model.Mana.ManaCost (DynamicManaCost (..), HybridManaCost (..), ManaCost (..), PhyrexianManaCost (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPayment (..), ManaPool (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (IsSnow, Snow (..))
import safe MtgPure.Model.Mana.ToManaPool (ToManaPool (..), toCompleteManaPool)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectN_ (ObjectN' (..))
import safe MtgPure.Model.Object.ObjectType (ObjectType (..))
import safe MtgPure.Model.Object.Singleton.Permanent (CoPermanent)
import safe MtgPure.Model.Permanent (Permanent (..))
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Recursive (Cost (..), Effect (..), Requirement (..))
import safe MtgPure.Model.Variable (ForceVars (forceVars), Var (..))
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

class FindMana manas var | manas -> var where
  findMana ::
    manas ->
    ( forall snow color.
      (IsSnow snow, IsManaNoVar snow color) =>
      Mana var snow color ->
      Maybe x
    ) ->
    Maybe x

instance IsSnow snow => FindMana (ManaPool snow) 'NoVar where
  findMana (ManaPool w u b r g c) f =
    getFirst $ mconcat $ map First [f w, f u, f b, f r, f g, f c]

instance FindMana CompleteManaPool 'NoVar where
  findMana pool f =
    getFirst $ mconcat $ map First [poolNonSnow pool `findMana` f, poolSnow pool `findMana` f]

payLife :: Monad m => Object 'OTPlayer -> Life -> Magic 'Private 'RW m Legality
payLife oPlayer life = logCall 'payLife do
  fromRO (findPlayer oPlayer) >>= \case
    Nothing -> pure Illegal
    Just player -> do
      let life' = playerLife player - life
      case life' < 0 of
        True -> pure Illegal
        False -> do
          setPlayer oPlayer $ player{playerLife = life'}
          pure Legal

hasEnoughFixedMana :: CompleteManaPool -> ManaCost 'NoVar -> Bool
hasEnoughFixedMana completePool cost = isManaNonNegative pool
 where
  nonSnow = poolNonSnow completePool
  snow = poolSnow completePool
  pool = (nonSnow <> mapManaPool thawMana snow) - extractFixedPayment' cost

isManaNonNegative :: FindMana manas 'NoVar => manas -> Bool
isManaNonNegative pool = case findMana pool isBad of
  Nothing -> True
  Just () -> False
 where
  isBad mana = case mana < 0 of
    True -> Just ()
    False -> Nothing

extractFixedPayment :: ManaCost 'NoVar -> CompleteManaPool
extractFixedPayment = toCompleteManaPool . extractFixedPayment'

extractFixedPayment' :: ManaCost 'NoVar -> ManaPool 'NonSnow
extractFixedPayment' cost = ManaPool w u b r g c
 where
  w = costWhite cost
  u = costBlue cost
  b = costBlack cost
  r = costRed cost
  g = costGreen cost
  c = costColorless cost

-- TODO: Prolly allow payment to be aborted by user.
-- [Pact of Negation] is an example of a card with a mandatory cost that cannot be aborted.
-- While players are not obligated to activate mana abilities, they must pay the cost with any
-- mana in their pools if possible. This is easy enough for fixed mana costs, but for dynamic.
-- I don't think MTG has any cards that require dynamic mana costs.
-- I suppose the best way to handle this is to accept an argument for Mandatory/Optional.
-- Game engine needs to search for at least one solution. If no solutions are found, then the payment
-- is regardless of whether it is mandatory or optional.
--
-- TODO: Auto pay generic costs when there is only one solution.
-- TODO: Auto pay snow costs when there is only one solution.
-- TODO: Auto pay hybrid costs when there is only one solution.
-- TODO: Auto pay phyrexian costs when there is only one solution.
promptPayForDynamic ::
  Monad m =>
  Object 'OTPlayer ->
  CompleteManaPool ->
  DynamicManaCost 'NoVar ->
  Magic 'Private 'RW m ManaPayment
promptPayForDynamic oPlayer avail dyn = do
  prompt <- fromRO $ gets magicPrompt
  opaque <- fromRO $ gets mkOpaqueGameState
  untilJust \attempt -> do
    payment <- fromPublic $ fromRO $ promptPayDynamicMana prompt attempt opaque oPlayer dyn
    case isPaymentCompatible payment dyn of
      False -> pure Nothing
      True -> case isManaNonNegative $ avail - paymentMana payment of
        False -> pure Nothing
        True -> pure $ Just payment -- TODO: check life payment

toPayment :: CompleteManaPool -> ManaPayment
toPayment pool = mempty{paymentMana = pool}

-- TODO: Handle X2 hybrid costs
possiblePaymentsHybrid ::
  ToManaPool 'NonSnow (ManaSymbol mt1) =>
  ToManaPool 'NonSnow (ManaSymbol mt2) =>
  ManaSymbol mt1 ->
  ManaSymbol mt2 ->
  Mana 'NoVar 'NonSnow mth ->
  [ManaPayment]
possiblePaymentsHybrid sym1 sym2 (Mana n) = case n <= 0 of
  True -> [mempty]
  False -> do
    p <-
      map
        toPayment
        [ mempty{poolNonSnow = toManaPool sym1}
        , mempty{poolNonSnow = toManaPool sym2}
        , mempty{poolSnow = mapManaPool freezeMana $ toManaPool sym1}
        , mempty{poolSnow = mapManaPool freezeMana $ toManaPool sym2}
        ]
    q <- possiblePaymentsHybrid sym1 sym2 $ Mana $ n - 1
    pure $ p <> q

possiblePaymentsPhyrexian ::
  forall mt mtp.
  ToManaPool 'NonSnow (ManaSymbol mt) =>
  ManaSymbol mt ->
  Mana 'NoVar 'NonSnow mtp ->
  [ManaPayment]
possiblePaymentsPhyrexian sym (Mana n) = case n <= 0 of
  True -> [mempty]
  False -> do
    p <-
      [ toPayment $ mempty{poolNonSnow = toManaPool sym}
        , toPayment $ mempty{poolSnow = mapManaPool freezeMana $ toManaPool sym}
        , mempty{paymentLife = 2}
        ]
    q <- possiblePaymentsPhyrexian sym $ Mana $ n - 1
    pure $ p <> q

class PossiblePayments cost where
  possiblePayments :: cost -> [ManaPayment]

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTHybridBG) where
  possiblePayments = possiblePaymentsHybrid B G

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianWhite) where
  possiblePayments = possiblePaymentsPhyrexian W

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianBlue) where
  possiblePayments = possiblePaymentsPhyrexian U

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianBlack) where
  possiblePayments = possiblePaymentsPhyrexian B

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianRed) where
  possiblePayments = possiblePaymentsPhyrexian R

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianGreen) where
  possiblePayments = possiblePaymentsPhyrexian G

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianColorless) where
  possiblePayments = possiblePaymentsPhyrexian C

instance PossiblePayments (Mana 'NoVar 'Snow 'MTGeneric) where
  possiblePayments (Mana n) = case n <= 0 of
    True -> [mempty]
    False -> do
      p <-
        [ toPayment $ mempty{poolSnow = mapManaPool freezeMana $ toManaPool W}
          , toPayment $ mempty{poolSnow = mapManaPool freezeMana $ toManaPool U}
          , toPayment $ mempty{poolSnow = mapManaPool freezeMana $ toManaPool B}
          , toPayment $ mempty{poolSnow = mapManaPool freezeMana $ toManaPool R}
          , toPayment $ mempty{poolSnow = mapManaPool freezeMana $ toManaPool G}
          , toPayment $ mempty{poolSnow = mapManaPool freezeMana $ toManaPool C}
          ]
      q <- possiblePayments @(Mana 'NoVar 'Snow 'MTGeneric) $ Mana $ n - 1
      pure $ p <> q

instance PossiblePayments (HybridManaCost 'NoVar) where
  possiblePayments hy = do
    bg <- possiblePayments $ hybridBG hy
    pure bg -- <> rg <> gw <> wb <> ur <> ...

instance PossiblePayments (PhyrexianManaCost 'NoVar) where
  possiblePayments phy = do
    w <- possiblePayments $ phyrexianWhite phy
    u <- possiblePayments $ phyrexianBlue phy
    b <- possiblePayments $ phyrexianBlack phy
    r <- possiblePayments $ phyrexianRed phy
    g <- possiblePayments $ phyrexianGreen phy
    c <- possiblePayments $ phyrexianColorless phy
    pure $ w <> u <> b <> r <> g <> c

instance PossiblePayments (DynamicManaCost 'NoVar) where
  possiblePayments dyn = do
    case costGeneric dyn of
      0 -> pure ()
      _ -> error "caller should zero out generic cost before calling possiblePayments to avoid big search tree"
    s <- possiblePayments $ costSnow dyn
    hy <- possiblePayments $ costHybrid dyn
    phy <- possiblePayments $ costPhyrexian dyn
    pure $ s <> hy <> phy

isPaymentCompatible :: ManaPayment -> DynamicManaCost 'NoVar -> Bool
isPaymentCompatible payment cost = not $ null do
  candidate <- possiblePayments cost'
  let remaining = payment - candidate
  M.guard $ isManaNonNegative $ paymentMana remaining
  M.guard $ countMana (paymentMana remaining) == countMana generic
  pure () -- TODO: check life payment
  pure candidate
 where
  cost' = cost{costGeneric = 0}
  generic = costGeneric cost

payManaCost :: Monad m => Object 'OTPlayer -> ManaCost 'Var -> Magic 'Private 'RW m Legality
payManaCost oPlayer (forceVars -> cost) = logCall 'payManaCost do
  fromRO (findPlayer oPlayer) >>= \case
    Nothing -> pure Illegal
    Just player -> do
      let pool = playerMana player
      case hasEnoughFixedMana pool cost of
        False -> pure Illegal
        True -> do
          let fixedPayment = extractFixedPayment cost
              avail = pool - fixedPayment
              dyn = costDynamic cost
          case countMana dyn > countMana avail of
            True -> pure Illegal
            False -> do
              -- XXX: This auto switching needs more smarts for life payments
              dynPayment <- case countMana dyn of
                0 -> pure mempty
                _ -> promptPayForDynamic oPlayer avail dyn
              let pool' = pool - fixedPayment - paymentMana dynPayment
              setPlayer
                oPlayer
                player
                  { playerMana = pool'
                  , playerLife =
                      let life = playerLife player - paymentLife dynPayment
                       in assert (life > 0) life
                  }
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
      Legal -> pure Legal -- TODO: Offer other choices
      Illegal -> undefined
