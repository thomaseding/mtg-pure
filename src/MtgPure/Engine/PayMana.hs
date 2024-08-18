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

module MtgPure.Engine.PayMana (
  isEachManaNonNegative,
  extractFixedPayment,
  playerCanPayManaCost,
  CanPayManaCost (..),
  PossiblePayments,
  possiblePayments,
  isPaymentCompatible,
  payManaCost,
) where

import safe Control.Exception (assert)
import safe qualified Control.Monad as M
import safe Control.Monad.Access (Visibility (..))
import safe qualified Control.Monad.Access as A
import safe qualified Control.Monad.Trans as M
import safe Control.Monad.Util (untilJust)
import safe qualified Data.List as List
import safe Data.Monoid (First (..))
import safe GHC.Stack (HasCallStack)
import safe MtgPure.Engine.Fwd.Api (findPlayer, getPlayer, setPlayer)
import safe MtgPure.Engine.Legality (Legality (..))
import safe MtgPure.Engine.Monad (fromPublic, fromRO, gets)
import safe MtgPure.Engine.Orphans (mapManaPool)
import safe MtgPure.Engine.Prompt (promptPayDynamicMana)
import safe MtgPure.Engine.State (Magic, logCall, magicPrompt, mkOpaqueGameState)
import safe MtgPure.Model.Mana.CountMana (countMana)
import safe MtgPure.Model.Mana.Mana (IsManaNoVar, Mana (..), freezeMana)
import safe MtgPure.Model.Mana.ManaCost (
  DynamicManaCost (..),
  HybridManaCost (..),
  ManaCost (..),
  PhyrexianManaCost (..),
 )
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPayment (..), ManaPool (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (IsSnow, Snow (..))
import safe MtgPure.Model.Mana.ToManaPool (ToManaPool (..), toCompleteManaPool)
import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Variable (ForceVars (..), Var (..))

class FindMana manas var | manas -> var where
  findMana ::
    manas ->
    ( forall snow color.
      (IsSnow snow, IsManaNoVar snow color) =>
      Mana var snow color ->
      Maybe x
    ) ->
    Maybe x

instance (IsSnow snow) => FindMana (ManaPool snow) 'NoVar where
  findMana ::
    (IsSnow snow) =>
    ManaPool snow ->
    ( forall (snow1 :: Snow) (color :: ManaType).
      (IsSnow snow1, IsManaNoVar snow1 color) =>
      Mana 'NoVar snow1 color ->
      Maybe x
    ) ->
    Maybe x
  findMana (ManaPool w u b r g c) f =
    getFirst $ mconcat $ map First [f w, f u, f b, f r, f g, f c]

instance FindMana CompleteManaPool 'NoVar where
  findMana ::
    CompleteManaPool ->
    ( forall (snow :: Snow) (color :: ManaType).
      (IsSnow snow, IsManaNoVar snow color) =>
      Mana 'NoVar snow color ->
      Maybe x
    ) ->
    Maybe x
  findMana pool f =
    getFirst $ mconcat $ map First [poolNonSnow pool `findMana` f, poolSnow pool `findMana` f]

hasEnoughFixedMana :: CompleteManaPool -> ManaCost 'NoVar -> Bool
hasEnoughFixedMana completePool cost = isEachManaNonNegative nonSnow'' && isEachManaNonNegative snow''
 where
  nonSnow = poolNonSnow completePool
  snow = poolSnow completePool
  (nonSnow', snow') = extractFixedPayment' nonSnow cost
  nonSnow'' = nonSnow - nonSnow'
  snow'' = snow - snow'

isEachManaNonNegative :: (FindMana manas 'NoVar) => manas -> Bool
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
  mw = poolW maxNonSNow
  mu = poolU maxNonSNow
  mb = poolB maxNonSNow
  mr = poolR maxNonSNow
  mg = poolG maxNonSNow
  mc = poolC maxNonSNow
  cw = costW cost
  cu = costU cost
  cb = costB cost
  cr = costR cost
  cg = costG cost
  cc = costC cost
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

data PartialManaPayment = PartialManaPayment ManaPayment (Mana 'NoVar 'NonSnow 'Ty1)
  deriving (Eq, Ord, Show)

instance Semigroup PartialManaPayment where
  (<>) :: PartialManaPayment -> PartialManaPayment -> PartialManaPayment
  PartialManaPayment p1 m1 <> PartialManaPayment p2 m2 = PartialManaPayment (p1 <> p2) (m1 + m2)

instance Monoid PartialManaPayment where
  mempty :: PartialManaPayment
  mempty = PartialManaPayment mempty 0

toPayment :: CompleteManaPool -> PartialManaPayment
toPayment pool = PartialManaPayment mempty{paymentMana = pool} 0

possiblePaymentsHybrid ::
  (ToManaPool 'NonSnow (ManaSymbol mt1)) =>
  (ToManaPool 'NonSnow (ManaSymbol mt2)) =>
  ManaSymbol mt1 ->
  ManaSymbol mt2 ->
  Mana 'NoVar 'NonSnow mth ->
  [PartialManaPayment]
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

possiblePaymentsHybrid2 ::
  (ToManaPool 'NonSnow (ManaSymbol mt)) =>
  ManaSymbol mt ->
  Mana 'NoVar 'NonSnow mth ->
  [PartialManaPayment]
possiblePaymentsHybrid2 sym (Mana n) = case n <= 0 of
  True -> [mempty]
  False -> do
    p <-
      map
        toPayment
        [ mempty{poolNonSnow = toManaPool sym}
        , mempty{poolSnow = mapManaPool freezeMana $ toManaPool sym}
        ]
        ++ [PartialManaPayment mempty 2]
    q <- possiblePaymentsHybrid2 sym $ Mana $ n - 1
    pure $ p <> q

possiblePaymentsPhyrexian ::
  forall mt mtp.
  (ToManaPool 'NonSnow (ManaSymbol mt)) =>
  ManaSymbol mt ->
  Mana 'NoVar 'NonSnow mtp ->
  [PartialManaPayment]
possiblePaymentsPhyrexian sym (Mana n) = case n <= 0 of
  True -> [mempty]
  False -> do
    p <-
      [ PartialManaPayment mempty{paymentLife = 2} 0
        , toPayment $ mempty{poolNonSnow = toManaPool sym}
        , toPayment $ mempty{poolSnow = mapManaPool freezeMana $ toManaPool sym}
        ]
    q <- possiblePaymentsPhyrexian sym $ Mana $ n - 1
    pure $ p <> q

class PossiblePayments cost where
  -- | This is allowed to return duplicates.
  possiblePaymentsImpl :: (HasCallStack) => cost -> [PartialManaPayment]

possiblePayments :: (HasCallStack) => (PossiblePayments cost) => cost -> [PartialManaPayment]
possiblePayments = map head . List.group . List.sort . possiblePaymentsImpl

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyWU) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyWU -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid W U

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyUB) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyUB -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid U B

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyBR) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyBR -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid B R

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyRG) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyRG -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid R G

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyGW) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyGW -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid G W

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyWB) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyWB -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid W B

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyUR) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyUR -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid U R

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyBG) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyBG -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid B G

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyRW) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyRW -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid R W

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyGU) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyGU -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid G U

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyW2) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyW2 -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid2 W

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyU2) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyU2 -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid2 U

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyB2) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyB2 -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid2 B

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyR2) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyR2 -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid2 R

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyG2) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyG2 -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid2 G

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyC2) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyC2 -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsHybrid2 C

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyPW) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyPW -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsPhyrexian W

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyPU) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyPU -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsPhyrexian U

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyPB) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyPB -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsPhyrexian B

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyPR) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyPR -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsPhyrexian R

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyPG) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyPG -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsPhyrexian G

instance PossiblePayments (Mana 'NoVar 'NonSnow 'TyPC) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'TyPC -> [PartialManaPayment]
  possiblePaymentsImpl = possiblePaymentsPhyrexian C

singleSnowPayments :: [PartialManaPayment]
singleSnowPayments =
  [ toPayment $ mempty{poolSnow = toManaPool SW}
  , toPayment $ mempty{poolSnow = toManaPool SU}
  , toPayment $ mempty{poolSnow = toManaPool SB}
  , toPayment $ mempty{poolSnow = toManaPool SR}
  , toPayment $ mempty{poolSnow = toManaPool SG}
  , toPayment $ mempty{poolSnow = toManaPool SC}
  ]

instance PossiblePayments (Mana 'NoVar 'NonSnow 'Ty1) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'NonSnow 'Ty1 -> [PartialManaPayment]
  possiblePaymentsImpl n = assert (n >= 0) [PartialManaPayment mempty n]

instance PossiblePayments (Mana 'NoVar 'Snow 'Ty1) where
  possiblePaymentsImpl :: (HasCallStack) => Mana 'NoVar 'Snow 'Ty1 -> [PartialManaPayment]
  possiblePaymentsImpl (Mana n) = case n <= 0 of
    True -> assert (n == 0) [mempty]
    False -> do
      p <- singleSnowPayments
      q <- possiblePaymentsImpl @(Mana 'NoVar 'Snow 'Ty1) $ Mana $ n - 1
      pure $ p <> q

instance PossiblePayments (HybridManaCost 'NoVar) where
  possiblePaymentsImpl :: (HasCallStack) => HybridManaCost 'NoVar -> [PartialManaPayment]
  possiblePaymentsImpl hy = do
    wu <- possiblePaymentsImpl $ hybridWU hy
    ub <- possiblePaymentsImpl $ hybridUB hy
    br <- possiblePaymentsImpl $ hybridBR hy
    rg <- possiblePaymentsImpl $ hybridRG hy
    gw <- possiblePaymentsImpl $ hybridGW hy
    wb <- possiblePaymentsImpl $ hybridWB hy
    ur <- possiblePaymentsImpl $ hybridUR hy
    bg <- possiblePaymentsImpl $ hybridBG hy
    rw <- possiblePaymentsImpl $ hybridRW hy
    gu <- possiblePaymentsImpl $ hybridGU hy
    w2 <- possiblePaymentsImpl $ hybridW2 hy
    u2 <- possiblePaymentsImpl $ hybridU2 hy
    b2 <- possiblePaymentsImpl $ hybridB2 hy
    r2 <- possiblePaymentsImpl $ hybridR2 hy
    g2 <- possiblePaymentsImpl $ hybridG2 hy
    c2 <- possiblePaymentsImpl $ hybridC2 hy
    pure $ wu <> ub <> br <> rg <> gw <> wb <> ur <> bg <> rw <> gu <> w2 <> u2 <> b2 <> r2 <> g2 <> c2

instance PossiblePayments (PhyrexianManaCost 'NoVar) where
  possiblePaymentsImpl :: (HasCallStack) => PhyrexianManaCost 'NoVar -> [PartialManaPayment]
  possiblePaymentsImpl phy = do
    w <- possiblePaymentsImpl $ phyrexianW phy
    u <- possiblePaymentsImpl $ phyrexianU phy
    b <- possiblePaymentsImpl $ phyrexianB phy
    r <- possiblePaymentsImpl $ phyrexianR phy
    g <- possiblePaymentsImpl $ phyrexianG phy
    c <- possiblePaymentsImpl $ phyrexianC phy
    pure $ w <> u <> b <> r <> g <> c

instance PossiblePayments (DynamicManaCost 'NoVar) where
  possiblePaymentsImpl :: (HasCallStack) => DynamicManaCost 'NoVar -> [PartialManaPayment]
  possiblePaymentsImpl dyn = do
    s <- possiblePaymentsImpl $ costSnow dyn
    hy <- possiblePaymentsImpl $ costHybrid dyn
    phy <- possiblePaymentsImpl $ costPhyrexian dyn
    pure $ PartialManaPayment mempty (costGeneric dyn) <> s <> hy <> phy

isPaymentCompatible :: ManaPayment -> DynamicManaCost 'NoVar -> Bool
isPaymentCompatible payment dyn = not $ null do
  PartialManaPayment candidate generic <- possiblePayments dyn
  let remaining = payment - candidate
  M.guard $ isEachManaNonNegative $ paymentMana remaining
  M.guard $ countMana (paymentMana remaining) == countMana generic
  M.guard $ paymentLife remaining == 0
  pure candidate

class PayGenericUnambiguously pool where
  payGenericUnambiguouslyImpl :: (pool -> CompleteManaPool) -> Int -> pool -> Maybe CompleteManaPool

instance PayGenericUnambiguously CompleteManaPool where
  payGenericUnambiguouslyImpl :: (CompleteManaPool -> CompleteManaPool) -> Int -> CompleteManaPool -> Maybe CompleteManaPool
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
  payGenericUnambiguouslyImpl :: (ManaPool snow -> CompleteManaPool) -> Int -> ManaPool snow -> Maybe CompleteManaPool
  payGenericUnambiguouslyImpl go generic pool
    | poolW pool >= x && pool{poolW = 0} == mempty = Just $ go mempty{poolW = x}
    | poolU pool >= x && pool{poolU = 0} == mempty = Just $ go mempty{poolU = x}
    | poolB pool >= x && pool{poolB = 0} == mempty = Just $ go mempty{poolB = x}
    | poolR pool >= x && pool{poolR = 0} == mempty = Just $ go mempty{poolR = x}
    | poolG pool >= x && pool{poolG = 0} == mempty = Just $ go mempty{poolG = x}
    | poolC pool >= x && pool{poolC = 0} == mempty = Just $ go mempty{poolC = x}
    | otherwise = Nothing
   where
    x :: Mana 'NoVar snow mt
    x = fromIntegral generic

payGenericUnambiguously :: Int -> CompleteManaPool -> Maybe CompleteManaPool
payGenericUnambiguously generic pool = case generic of
  0 -> Just mempty
  _ -> case generic == countMana pool of
    True -> Just pool
    False -> payGenericUnambiguouslyImpl id generic pool

data CanPayManaCost where
  CantPayMana :: CanPayManaCost
  -- | Returns `Just dynPayment` iff there is exactly one payment solution.
  -- This is the full dynamic mana cost, including the generic mana.
  -- Returns `Nothing` if there are multiple solutions.
  CanPayMana :: Maybe ManaPayment -> CanPayManaCost

getUniqueElem :: (Eq a) => [a] -> Maybe a
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
playerCanPayManaCost :: (HasCallStack) => Player -> ManaCost 'NoVar -> CanPayManaCost
playerCanPayManaCost player cost = case hasEnoughFixedMana pool cost of
  False -> CantPayMana
  True -> do
    case countMana dyn > countMana avail of
      True -> CantPayMana
      False -> do
        case dynSolutions of
          [] -> CantPayMana
          [partialPayment] -> CanPayMana $ toFullDynPayment partialPayment
          partialPayments ->
            CanPayMana $
              let fullDynPayments = map toFullDynPayment partialPayments
               in -- While the partialPayments are unique, the fullDynPayments may not be.
                  M.join $ getUniqueElem fullDynPayments
 where
  pool = playerMana player
  fixedPayment = extractFixedPayment (poolNonSnow pool) cost
  avail = pool - fixedPayment
  dyn = costDynamic cost
  dynCandidates = possiblePayments dyn
  dynSolutions = filter isSolution dynCandidates
  isSolution (PartialManaPayment candidate generic) =
    let remaining = avail - paymentMana candidate
        hasMana = isEachManaNonNegative remaining && countMana remaining >= countMana generic
        hasLife = playerLife player > paymentLife candidate || paymentLife candidate == 0
     in hasMana && hasLife
  toFullDynPayment (PartialManaPayment payment generic) =
    let avail' = avail - paymentMana payment
     in case payGenericUnambiguously (countMana generic) avail' of
          Nothing -> Nothing
          Just x -> Just $ payment <> mempty{paymentMana = x}

payManaCost :: (Monad m) => Object 'OTPlayer -> ManaCost 'Var -> Magic 'Private 'A.RW m Legality
payManaCost oPlayer (forceVars -> cost) = logCall 'payManaCost do
  fromRO (findPlayer oPlayer) >>= \case
    Nothing -> pure Illegal
    Just player -> case playerCanPayManaCost player cost of
      CantPayMana -> pure Illegal
      CanPayMana mDynSolution -> do
        let pool = playerMana player
            fixedPayment = extractFixedPayment (poolNonSnow pool) cost
            avail = pool - fixedPayment
            dyn = costDynamic cost
        dynPayment <- case mDynSolution of
          Just solution -> pure solution
          Nothing -> fromRO $ promptPayForCompatibleDynamic oPlayer avail dyn
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
  (Monad m) =>
  Object 'OTPlayer ->
  CompleteManaPool ->
  DynamicManaCost 'NoVar ->
  Magic 'Private 'A.RO m ManaPayment
promptPayForCompatibleDynamic oPlayer avail dyn = do
  player <- getPlayer oPlayer
  prompt <- gets magicPrompt
  opaque <- gets mkOpaqueGameState
  untilJust \attempt -> do
    payment <- fromPublic $ M.lift $ promptPayDynamicMana prompt attempt opaque oPlayer dyn
    let hasMana = isEachManaNonNegative $ avail - paymentMana payment
        hasLife = playerLife player > paymentLife payment || paymentLife payment == 0
        isCompatible = isPaymentCompatible payment dyn
    pure case hasMana && hasLife && isCompatible of
      False -> Nothing
      True -> Just payment
