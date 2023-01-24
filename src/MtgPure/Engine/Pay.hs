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
  playerCanPayManaCost,
  CanPayManaCost (..),
  PossiblePayments,
  possiblePayments,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe Control.Monad.Util (untilJust)
import safe Data.Functor ((<&>))
import safe qualified Data.List as List
import safe Data.Monoid (First (..))
import safe GHC.Stack (HasCallStack)
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
import safe MtgPure.Model.Mana.Mana (IsManaNoVar, Mana (..), freezeMana)
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
hasEnoughFixedMana completePool cost = isEachManaNonNegative nonSnow'' && isEachManaNonNegative snow''
 where
  nonSnow = poolNonSnow completePool
  snow = poolSnow completePool
  (nonSnow', snow') = extractFixedPayment' nonSnow cost
  nonSnow'' = nonSnow - nonSnow'
  snow'' = snow - snow'

isEachManaNonNegative :: FindMana manas 'NoVar => manas -> Bool
isEachManaNonNegative pool = case findMana pool isBad of
  Nothing -> True
  Just () -> False
 where
  isBad mana = case mana < 0 of
    True -> Just ()
    False -> Nothing

extractFixedPayment :: ManaPool 'NonSnow -> ManaCost 'NoVar -> CompleteManaPool
extractFixedPayment maxNonSnow = toCompleteManaPool . extractFixedPayment' maxNonSnow

extractFixedPayment' :: ManaPool 'NonSnow -> ManaCost 'NoVar -> (ManaPool 'NonSnow, ManaPool 'Snow)
extractFixedPayment' maxNonSNow cost = (ManaPool w u b r g c, ManaPool sw su sb sr sg sc)
 where
  mw = poolWhite maxNonSNow
  mu = poolBlue maxNonSNow
  mb = poolBlack maxNonSNow
  mr = poolRed maxNonSNow
  mg = poolGreen maxNonSNow
  mc = poolColorless maxNonSNow
  cw = costWhite cost
  cu = costBlue cost
  cb = costBlack cost
  cr = costRed cost
  cg = costGreen cost
  cc = costColorless cost
  w = min mw cw
  u = min mu cu
  b = min mb cb
  r = min mr cr
  g = min mg cg
  c = min mc cc
  sw = freezeMana $ cw - w
  su = freezeMana $ cu - u
  sb = freezeMana $ cb - b
  sr = freezeMana $ cr - r
  sg = freezeMana $ cg - g
  sc = freezeMana $ cc - c

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
  -- | This is allowed to return duplicates.
  possiblePaymentsImpl :: HasCallStack => cost -> [ManaPayment]

possiblePayments :: HasCallStack => PossiblePayments cost => cost -> [ManaPayment]
possiblePayments = map head . List.group . List.sort . possiblePaymentsImpl

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTHybridBG) where
  possiblePaymentsImpl = possiblePaymentsHybrid B G

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianWhite) where
  possiblePaymentsImpl = possiblePaymentsPhyrexian W

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianBlue) where
  possiblePaymentsImpl = possiblePaymentsPhyrexian U

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianBlack) where
  possiblePaymentsImpl = possiblePaymentsPhyrexian B

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianRed) where
  possiblePaymentsImpl = possiblePaymentsPhyrexian R

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianGreen) where
  possiblePaymentsImpl = possiblePaymentsPhyrexian G

instance PossiblePayments (Mana 'NoVar 'NonSnow 'MTPhyrexianColorless) where
  possiblePaymentsImpl = possiblePaymentsPhyrexian C

instance PossiblePayments (Mana 'NoVar 'Snow 'MTGeneric) where
  possiblePaymentsImpl (Mana n) = case n <= 0 of
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
      q <- possiblePaymentsImpl @(Mana 'NoVar 'Snow 'MTGeneric) $ Mana $ n - 1
      pure $ p <> q

instance PossiblePayments (HybridManaCost 'NoVar) where
  possiblePaymentsImpl hy = do
    bg <- possiblePaymentsImpl $ hybridBG hy
    pure bg -- <> rg <> gw <> wb <> ur <> ...

instance PossiblePayments (PhyrexianManaCost 'NoVar) where
  possiblePaymentsImpl phy = do
    w <- possiblePaymentsImpl $ phyrexianWhite phy
    u <- possiblePaymentsImpl $ phyrexianBlue phy
    b <- possiblePaymentsImpl $ phyrexianBlack phy
    r <- possiblePaymentsImpl $ phyrexianRed phy
    g <- possiblePaymentsImpl $ phyrexianGreen phy
    c <- possiblePaymentsImpl $ phyrexianColorless phy
    pure $ w <> u <> b <> r <> g <> c

instance PossiblePayments (DynamicManaCost 'NoVar) where
  possiblePaymentsImpl dyn = do
    case costGeneric dyn of
      0 -> pure ()
      _ -> error "caller should zero out generic cost before calling possiblePayments to avoid big search tree"
    s <- possiblePaymentsImpl $ costSnow dyn
    hy <- possiblePaymentsImpl $ costHybrid dyn
    phy <- possiblePaymentsImpl $ costPhyrexian dyn
    pure $ s <> hy <> phy

isPaymentCompatible :: ManaPayment -> DynamicManaCost 'NoVar -> Bool
isPaymentCompatible payment fullDyn = not $ null do
  candidate <- possiblePayments dyn
  let remaining = payment - candidate
  M.guard $ isEachManaNonNegative $ paymentMana remaining
  M.guard $ countMana (paymentMana remaining) == countMana generic
  pure () -- TODO: check life payment
  pure candidate
 where
  dyn = fullDyn{costGeneric = 0}
  generic = costGeneric fullDyn

class PayGenericUnambiguously pool where
  payGenericUnambiguouslyImpl :: (pool -> CompleteManaPool) -> Int -> pool -> Maybe CompleteManaPool

instance PayGenericUnambiguously CompleteManaPool where
  payGenericUnambiguouslyImpl _go generic pool =
    let nonSnow = payGenericUnambiguouslyImpl goNonSnow generic $ poolNonSnow pool
        snow = payGenericUnambiguouslyImpl goSnow generic $ poolSnow pool
     in case (nonSnow, snow) of
          (Nothing, Nothing) -> Nothing
          (Just _, Just _) -> Nothing
          (Just x, Nothing) -> Just x
          (Nothing, Just x) -> Just x
   where
    goNonSnow x = pool{poolNonSnow = x}
    goSnow x = pool{poolSnow = x}

instance PayGenericUnambiguously (ManaPool snow) where
  payGenericUnambiguouslyImpl go generic pool
    | poolWhite pool >= x && pool{poolWhite = 0} == mempty = Just $ go mempty{poolWhite = x}
    | poolBlue pool >= x && pool{poolBlue = 0} == mempty = Just $ go mempty{poolBlue = x}
    | poolBlack pool >= x && pool{poolBlack = 0} == mempty = Just $ go mempty{poolBlack = x}
    | poolRed pool >= x && pool{poolRed = 0} == mempty = Just $ go mempty{poolRed = x}
    | poolGreen pool >= x && pool{poolGreen = 0} == mempty = Just $ go mempty{poolGreen = x}
    | poolColorless pool >= x && pool{poolColorless = 0} == mempty = Just $ go mempty{poolColorless = x}
    | otherwise = Nothing
   where
    x :: Mana 'NoVar snow mt
    x = fromIntegral generic

payGenericUnambiguously :: Int -> CompleteManaPool -> Maybe CompleteManaPool
payGenericUnambiguously generic pool = case generic == countMana pool of
  True -> Just pool
  False -> payGenericUnambiguouslyImpl id generic pool

data CanPayManaCost where
  CantPayMana :: CanPayManaCost
  -- | Returns `Just dynPayment` iff there is exactly one payment solution.
  -- This is the full dynamic mana cost, including the generic mana.
  -- Returns `Nothing` if there are multiple solutions.
  CanPayMana :: Maybe ManaPayment -> CanPayManaCost

getUniqueElem :: Eq a => [a] -> Maybe a
getUniqueElem = \case
  [] -> Nothing
  xs -> case all (== head xs) xs of
    True -> Just $ head xs
    False -> Nothing

-- NOTE: This code would be a lot simpler if generic mana was included in the generated payments.
-- However that would make the search space much larger, and special casing generic mana to be
-- handled at the end prevents giant trees. Imagine if X was 20 or something. I can live with a
-- large search space for the other dynamic mana costs, since real world cards don't have variable
-- costs for those. (e.g. no cards have hybrid mana costs for X, or at least it is rare). If such
-- cards exist and cause performance problems, we can revisit this to special case them or update
-- the algorithm to handle them. (e.g. bust out a third-party discrete constraint solver).
playerCanPayManaCost :: HasCallStack => Player -> ManaCost 'NoVar -> CanPayManaCost
playerCanPayManaCost player cost = case hasEnoughFixedMana pool cost of
  False -> CantPayMana
  True -> do
    case countMana fullDyn > countMana avail of
      True -> CantPayMana
      False -> do
        case dynNoGenericSolutions of
          [] -> CantPayMana
          [paymentNoGeneric] -> CanPayMana $ toFullDynPayment paymentNoGeneric
          paymentsNoGeneric ->
            CanPayMana $
              let fullDynPayments = map toFullDynPayment paymentsNoGeneric
               in -- While the paymentsNoGeneric are unique, the fullDynPayments may not be.
                  M.join $ getUniqueElem fullDynPayments
 where
  pool = playerMana player
  fixedPayment = extractFixedPayment (poolNonSnow pool) cost
  avail = pool - fixedPayment
  fullDyn = costDynamic cost
  generic = costGeneric fullDyn
  dynNoGeneric = fullDyn{costGeneric = 0}
  dynNoGenericCandidates = possiblePayments dynNoGeneric
  dynNoGenericSolutions = filter isSolution dynNoGenericCandidates
  isSolution candidate =
    let remaining = avail - paymentMana candidate
     in case isEachManaNonNegative remaining && countMana remaining >= countMana generic of
          False -> False
          True -> True -- TODO: check life payment
  toFullDynPayment payment =
    let avail' = avail - paymentMana payment
     in case generic of
          0 -> Just payment
          _ -> case payGenericUnambiguously (countMana generic) avail' of
            Nothing -> Nothing
            Just x -> Just $ payment <> mempty{paymentMana = x}

payManaCost :: Monad m => Object 'OTPlayer -> ManaCost 'Var -> Magic 'Private 'RW m Legality
payManaCost oPlayer (forceVars -> cost) = logCall 'payManaCost do
  fromRO (findPlayer oPlayer) >>= \case
    Nothing -> pure Illegal
    Just player -> case playerCanPayManaCost player cost of
      CantPayMana -> pure Illegal
      CanPayMana mDynSolution -> do
        let pool = playerMana player
            fixedPayment = extractFixedPayment (poolNonSnow pool) cost
            avail = pool - fixedPayment
            fullDyn = costDynamic cost
        dynPayment <- case mDynSolution of
          Just solution -> pure solution
          Nothing -> promptPayForCompatibleDynamic oPlayer avail fullDyn
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

-- TODO: Prolly allow payment to be aborted by user.
-- [Pact of Negation] is an example of a card with a mandatory cost that cannot be aborted.
-- While players are not obligated to activate mana abilities, they must pay the cost with any
-- mana in their pools if possible. This is easy enough for fixed mana costs, but for dynamic.
-- I don't think MTG has any cards that require dynamic mana costs.
-- I suppose the best way to handle this is to accept an argument for Mandatory/Optional.
-- Game engine needs to search for at least one solution. If no solutions are found, then the payment
-- is aborted regardless of whether it is mandatory or optional.
promptPayForCompatibleDynamic ::
  Monad m =>
  Object 'OTPlayer ->
  CompleteManaPool ->
  DynamicManaCost 'NoVar ->
  Magic 'Private 'RW m ManaPayment
promptPayForCompatibleDynamic oPlayer avail dyn = do
  prompt <- fromRO $ gets magicPrompt
  opaque <- fromRO $ gets mkOpaqueGameState
  untilJust \attempt -> do
    payment <- fromPublic $ fromRO $ promptPayDynamicMana prompt attempt opaque oPlayer dyn
    case isPaymentCompatible payment dyn of
      False -> pure Nothing
      True -> case isEachManaNonNegative $ avail - paymentMana payment of
        False -> pure Nothing
        True -> pure $ Just payment -- TODO: check life payment

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
