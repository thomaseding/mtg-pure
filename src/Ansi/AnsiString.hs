{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}

-- TODO: Needs a `parseAnsi :: String -> AnsiString`
module Ansi.AnsiString (
  Layer (..),
  Rgb (..),
  Csi (..),
  Sgr (..),
  AnsiChar (..),
  AnsiString (..),
  ToAnsiString (..),
  AnsiToString (..),
  Lines (..),
  dropAnsi,
  parseAnsi,
  takeAnsi,
  isMovementCsi,
  dullBlack,
  dullBlue,
  dullMagenta,
  dullRed,
  dullWhite,
  dullYellow,
  vividBlack,
  vividBlue,
  vividCyan,
  vividGreen,
  vividMagenta,
  vividRed,
  vividWhite,
  vividYellow,
) where

import safe Control.Exception (assert)
import safe qualified Data.Char as Char
import safe Data.Functor ((<&>))
import safe Data.List (intercalate)
import safe Data.Maybe (mapMaybe)
import safe Data.String (IsString (..))
import safe GHC.Word (Word8)
import safe Text.Read (readMaybe)

data Rgb = Rgb
  { rgb_r :: Word8
  , rgb_g :: Word8
  , rgb_b :: Word8
  }
  deriving (Eq, Ord, Show)

data Csi where
  CsiSetNextLine :: Csi
  CsiSetPrevLine :: Csi
  -- | 0-based coordinates. Negative values move the cursor left.
  CsiSetRelCursorX :: Int -> Csi
  -- | 0-based coordinates. Negative values move the cursor up.
  CsiSetRelCursorY :: Int -> Csi
  -- | 0-based coordinates. Must be non-negative.
  CsiSetAbsCursorX :: Int -> Csi
  -- | 0-based coordinates. Must be non-negative.
  CsiSetAbsCursorXY :: Int -> Int -> Csi
  deriving (Eq, Ord)

instance Show Csi where
  show :: Csi -> String
  show = \case
    CsiSetNextLine -> "[+]"
    CsiSetPrevLine -> "[-]"
    CsiSetRelCursorX x -> "[r=" ++ show x ++ ",]"
    CsiSetRelCursorY y -> "[r=," ++ show y ++ "]"
    CsiSetAbsCursorX x -> "[a=" ++ show x ++ ",]"
    CsiSetAbsCursorXY x y -> "[a=" ++ show x ++ "," ++ show y ++ "]"

isMovementCsi :: Csi -> Bool
isMovementCsi = \case
  CsiSetNextLine -> True
  CsiSetPrevLine -> True
  CsiSetRelCursorX n -> n /= 0
  CsiSetRelCursorY n -> n /= 0
  CsiSetAbsCursorX{} -> True
  CsiSetAbsCursorXY{} -> True

data Layer = Fg | Bg
  deriving (Eq, Ord, Show)

data Sgr where
  SgrReset :: Sgr
  SgrTrueColor :: Layer -> Rgb -> Sgr
  deriving (Eq, Ord)

instance Show Sgr where
  show :: Sgr -> String
  show = \case
    SgrReset -> "[reset]"
    SgrTrueColor Fg (Rgb r g b) -> "[fg=" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "]"
    SgrTrueColor Bg (Rgb r g b) -> "[bg=" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "]"

data AnsiChar where
  AnsiChar :: Char -> AnsiChar
  AnsiCsi :: Csi -> AnsiChar
  AnsiSgr :: Sgr -> AnsiChar
  deriving (Eq, Ord)

instance Show AnsiChar where
  show :: AnsiChar -> String
  show = \case
    AnsiChar c -> show c
    AnsiCsi csi -> show csi
    AnsiSgr sgr -> show sgr

newtype AnsiString = AnsiString {unAnsiString :: [AnsiChar]}
  deriving (Eq, Ord)

instance Show AnsiString where
  show :: AnsiString -> String
  show (AnsiString cs) = concatMap show cs

instance IsString AnsiString where
  fromString :: String -> AnsiString
  fromString = AnsiString . map AnsiChar

instance Semigroup AnsiString where
  (<>) :: AnsiString -> AnsiString -> AnsiString
  AnsiString x <> AnsiString y = AnsiString $ x ++ y

instance Monoid AnsiString where
  mempty :: AnsiString
  mempty = AnsiString []

class ToAnsiString a where
  toAnsiString :: a -> AnsiString

instance (ToAnsiString a) => ToAnsiString [a] where
  toAnsiString :: (ToAnsiString a) => [a] -> AnsiString
  toAnsiString = mconcat . map toAnsiString

instance ToAnsiString Csi where
  toAnsiString :: Csi -> AnsiString
  toAnsiString = AnsiString . pure . AnsiCsi

instance ToAnsiString Sgr where
  toAnsiString :: Sgr -> AnsiString
  toAnsiString = AnsiString . pure . AnsiSgr

instance ToAnsiString AnsiChar where
  toAnsiString :: AnsiChar -> AnsiString
  toAnsiString = AnsiString . pure

instance ToAnsiString Char where
  toAnsiString :: Char -> AnsiString
  toAnsiString = toAnsiString . AnsiChar

instance ToAnsiString String where
  toAnsiString :: String -> AnsiString
  toAnsiString = fromString

instance ToAnsiString AnsiString where
  toAnsiString :: AnsiString -> AnsiString
  toAnsiString = id

class AnsiToString a where
  ansiToString :: a -> String

instance AnsiToString String where
  ansiToString :: String -> String
  ansiToString = id

-- TODO: Does extra work need to be done for Int values outside of Word8?
-- Do I need to chain a bunch of relative movement commands?
instance AnsiToString Csi where
  ansiToString :: Csi -> String
  ansiToString = \case
    CsiSetNextLine -> "\ESC[E"
    CsiSetPrevLine -> "\ESC[F"
    CsiSetRelCursorX x -> case compare x 0 of
      EQ -> ""
      GT -> "\ESC[" <> show x <> "C"
      LT -> "\ESC[" <> show (negate x) <> "D"
    CsiSetRelCursorY y -> case compare y 0 of
      EQ -> ""
      GT -> case y of
        1 -> "\ESC[B"
        _ -> "\ESC[" <> show y <> "B" -- missing +1?
      LT -> case negate y of
        1 -> "\ESC[A"
        y' -> "\ESC[" <> show y' <> "A" -- missing +1?
    CsiSetAbsCursorX x -> assert (x >= 0) case x + 1 of
      1 -> "\ESC[G"
      x' -> "\ESC[" <> show x' <> "G"
    CsiSetAbsCursorXY x y -> assert (x >= 0) $ assert (y >= 0) case (x + 1, y + 1) of
      (1, 1) -> "\ESC[;H"
      (1, y') -> "\ESC[" <> show y' <> ";H"
      (x', 1) -> "\ESC[;" <> show x' <> "H"
      (x', y') -> "\ESC[" <> show y' <> ";" <> show x' <> "H"

instance AnsiToString Sgr where
  ansiToString :: Sgr -> String
  ansiToString = \case
    SgrReset -> "\ESC[0m"
    SgrTrueColor Fg (Rgb r g b) ->
      "\ESC[38;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"
    SgrTrueColor Bg (Rgb r g b) ->
      "\ESC[48;2;" <> show r <> ";" <> show g <> ";" <> show b <> "m"

instance AnsiToString AnsiChar where
  ansiToString :: AnsiChar -> String
  ansiToString = \case
    AnsiChar c -> [c]
    AnsiCsi csi -> ansiToString csi
    AnsiSgr sgr -> ansiToString sgr

instance AnsiToString [AnsiChar] where
  ansiToString :: [AnsiChar] -> String
  ansiToString = \case
    AnsiSgr (SgrTrueColor Fg (Rgb r1 g1 b1)) : AnsiSgr (SgrTrueColor Bg (Rgb r2 g2 b2)) : cs ->
      let x = "\ESC[38;2;" <> show r1 <> ";" <> show g1 <> ";" <> show b1 <> ";48;2;" <> show r2 <> ";" <> show g2 <> ";" <> show b2 <> "m"
       in x ++ ansiToString cs
    AnsiSgr (SgrTrueColor Bg (Rgb r2 g2 b2)) : AnsiSgr (SgrTrueColor Fg (Rgb r1 g1 b1)) : cs ->
      let x = "\ESC[38;2;" <> show r1 <> ";" <> show g1 <> ";" <> show b1 <> ";48;2;" <> show r2 <> ";" <> show g2 <> ";" <> show b2 <> "m"
       in x ++ ansiToString cs
    c : cs -> ansiToString c ++ ansiToString cs
    [] -> []

instance AnsiToString AnsiString where
  ansiToString :: AnsiString -> String
  ansiToString = ansiToString . unAnsiString

parseSemiList :: String -> Either String (Char, [Int], String)
parseSemiList = \case
  "" -> Left "[empty-str]"
  str -> case break Char.isAlpha str of
    (_, []) -> Left str
    (pre, command : post) ->
      do
        ns <-
          mapM goRead $
            lines $
              pre <&> \case
                ';' -> '\n'
                c -> c
        pure (command, ns, post)
 where
  goRead = \case
    "" -> Right 1
    s -> case readMaybe s of
      Nothing -> Left s
      Just z -> Right z

parseAnsi :: String -> Either String AnsiString
parseAnsi = fmap AnsiString . parseAnsi'

parseAnsi' :: String -> Either String [AnsiChar]
parseAnsi' = \case
  "" -> pure []
  '\ESC' : '[' : cs ->
    parseSemiList cs >>= \case
      ('E', [], cs') -> (AnsiCsi CsiSetNextLine :) <$> parseAnsi' cs'
      ('F', [], cs') -> (AnsiCsi CsiSetPrevLine :) <$> parseAnsi' cs'
      ('G', [], cs') -> (AnsiCsi (CsiSetAbsCursorX 1) :) <$> parseAnsi' cs'
      ('G', [n], cs') -> (AnsiCsi (CsiSetAbsCursorX $ n - 1) :) <$> parseAnsi' cs'
      ('H', [x], cs') -> (AnsiCsi (CsiSetAbsCursorXY x 1) :) <$> parseAnsi' cs'
      ('H', [x, y], cs') -> (AnsiCsi (CsiSetAbsCursorXY x y) :) <$> parseAnsi' cs'
      ('m', [0], cs') -> (AnsiSgr SgrReset :) <$> parseAnsi' cs'
      ('m', [38, 2, r, g, b], cs') -> (AnsiSgr (SgrTrueColor Fg $ rgb r g b) :) <$> parseAnsi' cs'
      ('m', [48, 2, r, g, b], cs') -> (AnsiSgr (SgrTrueColor Bg $ rgb r g b) :) <$> parseAnsi' cs'
      ('m', [38, 2, r1, g1, b1, 48, 2, r2, g2, b2], cs') ->
        ([AnsiSgr (SgrTrueColor Fg $ rgb r1 g1 b1), AnsiSgr (SgrTrueColor Bg $ rgb r2 g2 b2)] ++) <$> parseAnsi' cs'
      ('m', [48, 2, r1, g1, b1, 38, 2, r2, g2, b2], cs') ->
        ([AnsiSgr (SgrTrueColor Bg $ rgb r1 g1 b1), AnsiSgr (SgrTrueColor Fg $ rgb r2 g2 b2)] ++) <$> parseAnsi' cs'
      _ -> Left cs
  '\ESC' : cs -> Left cs
  c : cs -> (AnsiChar c :) <$> parseAnsi' cs
 where
  rgb r g b = Rgb (fromIntegral r) (fromIntegral g) (fromIntegral b)

dullBlack :: Rgb
dullBlack = Rgb 0 0 0

dullBlue :: Rgb
dullBlue = Rgb 0 0 150

dullMagenta :: Rgb
dullMagenta = Rgb 150 0 150

dullRed :: Rgb
dullRed = Rgb 150 0 0

dullWhite :: Rgb
dullWhite = Rgb 150 150 150

dullYellow :: Rgb
dullYellow = Rgb 0 150 150

vividCyan :: Rgb
vividCyan = Rgb 80 255 255

vividGreen :: Rgb
vividGreen = Rgb 0 255 0

vividBlack :: Rgb
vividBlack = Rgb 150 150 150

vividBlue :: Rgb
vividBlue = Rgb 0 0 255

vividMagenta :: Rgb
vividMagenta = Rgb 255 0 255

vividRed :: Rgb
vividRed = Rgb 255 0 0

vividWhite :: Rgb
vividWhite = Rgb 255 255 255

vividYellow :: Rgb
vividYellow = Rgb 0 255 255

class Lines a where
  toLines :: a -> [a]
  fromLines :: [a] -> a

instance Lines String where
  toLines :: String -> [String]
  toLines = lines

  fromLines :: [String] -> String
  fromLines = unlines

instance Lines [AnsiChar] where
  toLines :: [AnsiChar] -> [[AnsiChar]]
  toLines cs = case span (/= AnsiChar '\n') cs of
    (pre, _ : post) -> pre : toLines post
    (pre, []) -> [pre]

  fromLines :: [[AnsiChar]] -> [AnsiChar]
  fromLines cs = intercalate [AnsiChar '\n'] cs ++ [AnsiChar '\n']

instance Lines AnsiString where
  toLines :: AnsiString -> [AnsiString]
  toLines = map AnsiString . toLines . unAnsiString

  fromLines :: [AnsiString] -> AnsiString
  fromLines = AnsiString . fromLines . map unAnsiString

dropAnsi :: AnsiString -> String
dropAnsi (AnsiString cs) = mapMaybe go cs
 where
  go = \case
    AnsiChar c -> Just c
    _ -> Nothing

takeAnsi :: Int -> AnsiString -> AnsiString
takeAnsi n (AnsiString cs) = AnsiString $ takeAnsi' n cs

takeAnsi' :: Int -> [AnsiChar] -> [AnsiChar]
takeAnsi' = \case
  0 -> id
  n -> \case
    c@AnsiChar{} : cs -> c : takeAnsi' (n - 1) cs
    x : cs -> x : takeAnsi' n cs
    [] -> []
