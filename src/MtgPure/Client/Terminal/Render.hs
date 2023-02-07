{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Redundant fmap" #-}
{-# HLINT ignore "Use fromRight" #-}

module MtgPure.Client.Terminal.Render (
  printGameState,
) where

import Ansi.AnsiString (
  Rgb,
  Sgr (..),
  dullBlack,
  dullBlue,
  dullRed,
  dullWhite,
  dullYellow,
  vividGreen,
  vividWhite,
  vividYellow,
 )
import safe Ansi.Box (
  Box (..),
  FixedOrRatio (..),
  addPopup,
  drawBoxIO,
  fromAbsolute,
 )
import safe Control.Monad.Access (ReadWrite (..), Visibility (..))
import safe qualified Control.Monad.Trans as M
import safe Data.Functor ((<&>))
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe qualified Data.Maybe as Maybe
import safe qualified Data.Traversable as T
import safe MtgPure.Client.Terminal.Monad (Terminal)
import safe MtgPure.Engine.Fwd.Api (
  allControlledPermanentsOf,
  findHandCard,
  getAlivePlayers,
  getPermanent,
  getPlayer,
 )
import safe MtgPure.Engine.Monad (fromRO, gets, internalFromPrivate)
import safe MtgPure.Engine.Orphans ()
import safe MtgPure.Engine.State (GameState (..), Magic, OpaqueGameState, queryMagic)
import safe MtgPure.Model.CardName (CardName (..), HasCardName, getCardName)
import safe MtgPure.Model.Creature (Creature (..))
import safe MtgPure.Model.Damage (Damage' (..))
import safe MtgPure.Model.Graveyard (Graveyard (..))
import safe MtgPure.Model.Hand (Hand (..))
import safe MtgPure.Model.Library (Library (..))
import safe MtgPure.Model.Life (Life (..))
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Mana.ManaType (ManaType (..))
import safe MtgPure.Model.Mana.Snow (IsSnow (..), Snow (..))
import safe MtgPure.Model.Object.OTNAliases (OTNCard, OTNPermanent)
import safe MtgPure.Model.Object.Object (Object)
import safe MtgPure.Model.Object.ObjectId (GetObjectId, ObjectId (..), getObjectId)
import safe MtgPure.Model.Object.ObjectType (ObjectType (OTPlayer))
import safe MtgPure.Model.Permanent (Permanent (..), Tapped (..))
import safe MtgPure.Model.PhaseStep (prettyPhaseStep)
import safe MtgPure.Model.Player (Player (..))
import safe MtgPure.Model.Power (Power (..))
import safe MtgPure.Model.Recursive (AnyCard)
import safe MtgPure.Model.Stack (Stack (..))
import safe MtgPure.Model.Toughness (Toughness (..))
import safe MtgPure.Model.Variable (Var (..))
import safe MtgPure.Model.Zone (Zone (..))
import safe MtgPure.Model.ZoneObject.Convert (toZO0)
import safe MtgPure.Model.ZoneObject.ZoneObject (ZO)

--------------------------------------------------------------------------------

getPlayerName :: Object 'OTPlayer -> String
getPlayerName o = case getObjectId o of
  ObjectId i -> case i of
    1 -> "[1] Alice"
    2 -> "[2] Bob"
    _ -> "[?] UnknownPlayer" ++ show i

idAndName :: (GetObjectId i, HasCardName n) => i -> n -> String
idAndName i n = "[" ++ show i' ++ "] " ++ n'
 where
  ObjectId i' = getObjectId i
  CardName n' = getCardName n

getHandCardName :: ZO 'ZHand OTNCard -> Magic 'Public 'RO Terminal String
getHandCardName zo = do
  let zo0 = toZO0 zo
  handCards <- internalFromPrivate $ gets magicHandCards
  case Map.lookup zo0 handCards of
    Nothing -> pure $ "IllegalHandCard@" ++ show zo
    Just anyCard -> pure $ idAndName zo anyCard

getGraveyardCardName :: ZO 'ZGraveyard OTNCard -> Magic 'Public 'RO Terminal String
getGraveyardCardName zo = do
  let zo0 = toZO0 zo
  graveyardCards <- internalFromPrivate $ gets magicGraveyardCards
  case Map.lookup zo0 graveyardCards of
    Nothing -> pure $ "IllegalGraveyardCard@" ++ show zo
    Just anyCard -> pure $ idAndName zo anyCard

getExiledCardName :: ZO 'ZExile OTNCard -> Magic 'Public 'RO Terminal String
getExiledCardName zo = do
  let zo0 = toZO0 zo
  exileCards <- internalFromPrivate $ gets magicExiledCards
  case Map.lookup zo0 exileCards of
    Nothing -> pure $ "IllegalExiledCard@" ++ show zo
    Just anyCard -> pure $ idAndName zo anyCard

-- TODO: Expose sufficient Public API to avoid need for `internalFromPrivate`
printGameState :: OpaqueGameState Terminal -> Maybe String -> Terminal ()
printGameState opaque mPopup = queryMagic opaque do
  box <- mkEverythingBox
  let w = 100
  let h = max (fromAbsolute $ boxH box) $ maybe 0 (\s -> length (lines s) + 4) mPopup
  let box' = case mPopup of
        Nothing -> box
        Just popup -> addPopup popup box
  M.liftIO $ drawBoxIO w h box'

--------------------------------------------------------------------------------

-- TODO: Want collapsible boxes, so users can hide/show zones.
-- TODO: Want scrollable boxes, so users can scroll through large zones.
mkEverythingBox :: Magic 'Public 'RO Terminal Box
mkEverythingBox = do
  turnBox <- mkTurnBox
  stackBox <- mkStackBox
  priorityBox <- mkPriorityBox
  oPlayers <- getAlivePlayers
  playerBoxes <- T.for oPlayers buildPlayerBox
  pure
    Box
      { boxText = ""
      , boxClipper = take
      , boxX = Fixed 0
      , boxY = Fixed 0
      , boxW = Ratio 1
      , boxH = Absolute $ 3 + maximum (fromAbsolute . boxH <$> playerBoxes)
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = [turnBox, stackBox, priorityBox] ++ playerBoxes
      }

--------------------------------------------------------------------------------

mkTurnBox :: Magic 'Public 'RO Terminal Box
mkTurnBox = do
  turn <- internalFromPrivate $ gets magicCurrentTurn
  let dayNight = "Day" -- TODO
  phaseStep <- internalFromPrivate $ gets $ prettyPhaseStep . magicPhaseStep
  let text = "Turn " ++ show turn ++ " - " ++ dayNight ++ " - " ++ phaseStep
  pure
    Box
      { boxText = text
      , boxClipper = take
      , boxX = Center
      , boxY = Fixed 0
      , boxW = Ratio 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = [SgrTrueColorFg vividWhite]
      , boxKidsPre = []
      , boxKidsPost = []
      }

--------------------------------------------------------------------------------

mkStackBox :: Magic 'Public 'RO Terminal Box
mkStackBox = do
  Stack stack <- internalFromPrivate $ gets magicStack
  let text = "Stack=" ++ show stack
  pure
    Box
      { boxText = text
      , boxClipper = take
      , boxX = Center
      , boxY = Fixed 1
      , boxW = Ratio 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = [SgrTrueColorBg dullBlue]
      , boxKidsPre = []
      , boxKidsPost = []
      }

mkPriorityBox :: Magic 'Public 'RO Terminal Box
mkPriorityBox = do
  priorityOrder <- internalFromPrivate $ gets magicPlayerOrderPriority
  let text =
        "Priority=" ++ case priorityOrder of
          [] -> "N/A"
          p : _ -> "[" ++ show (unObjectId $ getObjectId p) ++ "]"
  pure
    Box
      { boxText = text
      , boxClipper = take
      , boxX = Center
      , boxY = Fixed 2
      , boxW = Ratio 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = [SgrTrueColorBg dullBlue]
      , boxKidsPre = []
      , boxKidsPost = []
      }

--------------------------------------------------------------------------------

buildPlayerBox :: Object 'OTPlayer -> Magic 'Public 'RO Terminal Box
buildPlayerBox oPlayer = do
  player <- internalFromPrivate $ getPlayer oPlayer
  perms <- allControlledPermanentsOf oPlayer
  let i = getObjectId oPlayer
  let pool = playerMana player
  let hand = playerHand player
  let hudOffset = 0
  let hudBox = mkPlayerHudBox hudOffset player
  let poolOffset = hudOffset + fromAbsolute (boxH hudBox)
  let poolBox = mkCompleteManaPoolBox poolOffset pool
  let handOffset = poolOffset + fromAbsolute (boxH poolBox)
  handBox <- mkHandBox oPlayer handOffset hand
  let graveyardOffset = handOffset + fromAbsolute (boxH handBox)
  graveyardBox <- mkGraveyardBox graveyardOffset $ playerGraveyard player
  let exileOffset = graveyardOffset + fromAbsolute (boxH graveyardBox)
  exileBox <- mkExileBox exileOffset [] -- TODO
  let permOffset = exileOffset + fromAbsolute (boxH exileBox)
  permBox <- mkBattlefieldBox permOffset perms
  let totalHeight = permOffset + fromAbsolute (boxH permBox)
  pure
    Box
      { boxText = ""
      , boxClipper = take
      , boxX = Ratio case i of
          ObjectId 1 -> 0
          ObjectId 2 -> 0.5
          _ -> error "Only two players supported"
      , boxY = Fixed 3
      , boxW = Ratio 0.5
      , boxH = Absolute totalHeight
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = [hudBox, poolBox, handBox, graveyardBox, exileBox, permBox]
      }

--------------------------------------------------------------------------------

mkPlayerHudBox :: Int -> Player -> Box
mkPlayerHudBox y player = box
 where
  name = getPlayerName $ playerObject player
  poison = 0 -- TODO: Add Poison to Player
  life = unLife $ playerLife player
  library = length $ unLibrary $ playerLibrary player
  text = name ++ " Poison=" ++ show poison ++ " Life=" ++ show life ++ " Library=" ++ show library
  box =
    Box
      { boxText = text
      , boxClipper = take
      , boxX = Fixed 0
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

--------------------------------------------------------------------------------

manaWidth :: Int
manaWidth = 5

mkCompleteManaPoolBox :: Int -> CompleteManaPool -> Box
mkCompleteManaPoolBox y pool =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Fixed 0
    , boxY = Fixed y
    , boxW = Ratio 1
    , boxH = Absolute 2
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = [mkManaPoolBox nonSnow 0, mkManaPoolBox snow 1]
    }
 where
  nonSnow = poolNonSnow pool
  snow = poolSnow pool

mkManaPoolBox :: forall snow. IsSnow snow => ManaPool snow -> Int -> Box
mkManaPoolBox pool y =
  Box
    { boxText = ""
    , boxClipper = take
    , boxX = Fixed 0
    , boxY = Fixed y
    , boxW = Ratio 1
    , boxH = Absolute 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = [mkManaLegendBox desc, mkManaC c, mkManaW w, mkManaU u, mkManaB b, mkManaR r, mkManaG g]
    }
 where
  ManaPool{poolW = w, poolU = u, poolB = b, poolR = r, poolG = g, poolC = c} = pool
  desc = case litSnow @snow of
    NonSnow -> "Norm"
    Snow -> "Snow"

legendWidth :: Int
legendWidth = 5

mkManaLegendBox :: String -> Box
mkManaLegendBox desc =
  Box
    { boxText = desc
    , boxClipper = take
    , boxX = Fixed 0
    , boxY = Fixed 0
    , boxW = Fixed legendWidth
    , boxH = Absolute 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

padLeft :: Int -> String -> String
padLeft n s = replicate (n - length s) ' ' <> s

manaClipper :: Int -> String -> String
manaClipper n s = padLeft (manaWidth - 1) $ manaClipper' n s

manaClipper' :: Int -> String -> String
manaClipper' n s
  | amount <= 9999 = s
  | amount <= 99999 = take (n - 1) (decimal2 $ show (amount `div` 1000)) <> "k"
  | amount <= 999999 = take (n - 1) (show (amount `div` 1000)) <> "k"
  | amount <= 9999999 = take (n - 1) (decimal $ show (amount `div` 100000)) <> "m"
  | amount <= 99999999 = take (n - 1) (decimal2 $ show (amount `div` 1000000)) <> "m"
  | amount <= 999999999 = take (n - 1) (show (amount `div` 100000)) <> "m"
  | otherwise = "+++"
 where
  amount = read s
  decimal = \case
    c : cs -> c : '.' : cs
    [] -> []
  decimal2 = \case
    c1 : c2 : cs -> c1 : c2 : '.' : cs
    cs -> cs

mkManaBox :: Rgb -> Rgb -> Int -> String -> Box
mkManaBox fg bg relX text =
  Box
    { boxText = text
    , boxClipper = manaClipper
    , boxX = Fixed $ manaWidth * relX + legendWidth
    , boxY = Fixed 0
    , boxW = Fixed $ manaWidth - 1
    , boxH = Absolute 1
    , boxBackground = Nothing
    , boxColorCommands = [SgrTrueColorFg fg, SgrTrueColorBg bg]
    , boxKidsPre = []
    , boxKidsPost = []
    }

mkManaC :: Mana 'NoVar snow 'TyC -> Box
mkManaC (Mana x) = mkManaBox dullBlack dullWhite 0 $ show x

mkManaW :: Mana 'NoVar snow 'TyW -> Box
mkManaW (Mana x) = mkManaBox dullBlack vividYellow 1 $ show x

mkManaU :: Mana 'NoVar snow 'TyU -> Box
mkManaU (Mana x) = mkManaBox vividWhite dullBlue 2 $ show x

mkManaB :: Mana 'NoVar snow 'TyB -> Box
mkManaB (Mana x) = mkManaBox dullYellow dullBlack 3 $ show x

mkManaR :: Mana 'NoVar snow 'TyR -> Box
mkManaR (Mana x) = mkManaBox vividWhite dullRed 4 $ show x

mkManaG :: Mana 'NoVar snow 'TyG -> Box
mkManaG (Mana x) = mkManaBox dullBlack vividGreen 5 $ show x

--------------------------------------------------------------------------------

mkEmptyObjectBox :: Int -> Box
mkEmptyObjectBox y =
  Box
    { boxText = "[ ] -"
    , boxClipper = take
    , boxX = Fixed 1
    , boxY = Fixed y
    , boxW = Ratio 1
    , boxH = Absolute 1
    , boxBackground = Nothing
    , boxColorCommands = []
    , boxKidsPre = []
    , boxKidsPost = []
    }

--------------------------------------------------------------------------------

mkHandBox :: Object 'OTPlayer -> Int -> Hand -> Magic 'Public 'RO Terminal Box
mkHandBox oPlayer y (Hand cards) = do
  kids <- T.for (zip [1 ..] cards) $ uncurry $ mkHandCardBox oPlayer
  let kids' = case kids of
        [] -> [mkEmptyObjectBox 1]
        _ -> kids
  pure
    Box
      { boxText = "Hand"
      , boxClipper = take
      , boxX = Fixed 0
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Absolute $ 1 + length kids'
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = kids'
      }

dropWhileNotPrefix :: String -> String -> String
dropWhileNotPrefix prefix str = case dropWhile (not . List.isPrefixOf prefix) $ List.tails str of
  [] -> []
  s : _ -> s

takeBalanced :: String -> String
takeBalanced = takeBalanced' 0

takeBalanced' :: Int -> String -> String
takeBalanced' _ [] = []
takeBalanced' n (c : cs)
  | c == '(' = c : takeBalanced' (n + 1) cs
  | c == ')' && n > 0 = c : takeBalanced' (n - 1) cs
  | c == ')' = []
  | otherwise = c : takeBalanced' n cs

-- TODO: Use MtgPure/Model/Mana/PrintedManaCost.hs
hackyGetManaCost :: AnyCard -> Maybe String
hackyGetManaCost card = case dropWhileNotPrefix "toManaCost" $ show card of
  "" -> Nothing
  s -> Just case takeBalanced $ takeWhile (/= ' ') $ drop 11 s of
    s'@('(' : _) -> s'
    s' -> '(' : s' <> ")"

mkHandCardBox :: Object 'OTPlayer -> Int -> ZO 'ZHand OTNCard -> Magic 'Public 'RO Terminal Box
mkHandCardBox oPlayer y zoCard = do
  card <-
    internalFromPrivate $
      fromRO $
        findHandCard oPlayer zoCard <&> \case
          Nothing -> error "mkHandCardBox: card not found"
          Just c -> c
  name <- getHandCardName zoCard
  let mCost = hackyGetManaCost card
  let text = case mCost of
        Nothing -> name
        Just cost -> name <> " " <> cost
  pure
    Box
      { boxText = text
      , boxClipper = take
      , boxX = Fixed 1
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

--------------------------------------------------------------------------------

mkGraveyardBox :: Int -> Graveyard -> Magic 'Public 'RO Terminal Box
mkGraveyardBox y (Graveyard cards) = do
  kids <- T.for (zip [1 ..] cards) $ uncurry mkGraveyardCardBox
  let kids' = case kids of
        [] -> [mkEmptyObjectBox 1]
        _ -> kids
  pure
    Box
      { boxText = "Graveyard"
      , boxClipper = take
      , boxX = Fixed 0
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Absolute $ 1 + length kids'
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = kids'
      }

mkGraveyardCardBox :: Int -> ZO 'ZGraveyard OTNCard -> Magic 'Public 'RO Terminal Box
mkGraveyardCardBox y card = do
  name <- getGraveyardCardName card
  pure
    Box
      { boxText = name
      , boxClipper = take
      , boxX = Fixed 1
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

--------------------------------------------------------------------------------

mkExileBox :: Int -> [ZO 'ZExile OTNCard] -> Magic 'Public 'RO Terminal Box
mkExileBox y cards = do
  kids <- T.for (zip [1 ..] cards) $ uncurry mkExileCardBox
  let kids' = case kids of
        [] -> [mkEmptyObjectBox 1]
        _ -> kids
  pure
    Box
      { boxText = "Exile"
      , boxClipper = take
      , boxX = Fixed 0
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Absolute $ 1 + length kids'
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = kids'
      }

mkExileCardBox :: Int -> ZO 'ZExile OTNCard -> Magic 'Public 'RO Terminal Box
mkExileCardBox y card = do
  name <- getExiledCardName card
  pure
    Box
      { boxText = name
      , boxClipper = take
      , boxX = Fixed 1
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Absolute 1
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }

--------------------------------------------------------------------------------

mkBattlefieldBox :: Int -> [ZO 'ZBattlefield OTNPermanent] -> Magic 'Public 'RO Terminal Box
mkBattlefieldBox y cards = do
  kids <- T.for (zip [1 ..] cards) $ uncurry mkBattlefieldCardBox
  let kids' = case kids of
        [] -> [mkEmptyObjectBox 1]
        _ -> kids
  pure
    Box
      { boxText = "Battlefield"
      , boxClipper = take
      , boxX = Fixed 0
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Absolute $ 1 + length kids'
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = kids'
      }

mkBattlefieldCardBox :: Int -> ZO 'ZBattlefield OTNPermanent -> Magic 'Public 'RO Terminal Box
mkBattlefieldCardBox y zoPerm = do
  perm <- internalFromPrivate $ getPermanent zoPerm
  let name = idAndName zoPerm $ permanentCard perm
  let mCreature = permanentCreature perm
  let Damage damage = permanentCreatureDamage perm
  let pt = case mCreature of
        Nothing -> ""
        Just creature ->
          let Power power = creaturePower creature
              Toughness toughness = creatureToughness creature
           in show power ++ "/" ++ show toughness ++ "-" ++ show damage
  let hasSummoningSickness = permanentSummoningSickness perm && Maybe.isJust mCreature
  let status = case (permanentTapped perm, hasSummoningSickness) of
        (Tapped, True) -> "T"
        (Tapped, False) -> "T"
        (Untapped, True) -> "S"
        (Untapped, False) -> "U"
  let text =
        status ++ " " ++ name ++ case pt of
          "" -> ""
          _ -> " " ++ pt
  pure
    Box
      { boxText = text
      , boxClipper = take
      , boxX = Fixed 1
      , boxY = Fixed y
      , boxW = Ratio 1
      , boxH = Fixed 1
      , boxBackground = Nothing
      , boxColorCommands = []
      , boxKidsPre = []
      , boxKidsPost = []
      }
