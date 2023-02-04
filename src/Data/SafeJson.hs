{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use second" #-}

module Data.SafeJson (
  SafeValue (..),
  parseSafeJson,
  readSafeJsonFile,
  ToSafeJson (..),
  FromSafeJson (..),
  store,
  fetch,
) where

import safe Control.Exception (evaluate)
import safe qualified Control.Monad as M
import safe qualified Data.Char as Char
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (fromMaybe)
import safe Data.Typeable (Typeable)
import safe Text.Parsec (
  Parsec,
  anyChar,
  char,
  digit,
  eof,
  hexDigit,
  many,
  many1,
  manyTill,
  noneOf,
  oneOf,
  optionMaybe,
  parse,
  sepBy,
  string,
  try,
  (<|>),
 )

data SafeValue :: Type where
  SafeString :: String -> SafeValue
  SafeNumber :: Double -> SafeValue
  SafeBool :: Bool -> SafeValue
  SafeNull :: SafeValue
  SafeArray :: [SafeValue] -> SafeValue
  SafeObject :: Map.Map String SafeValue -> SafeValue
  deriving (Eq, Ord, Show, Typeable)

readSafeJsonFile :: FromSafeJson a => FilePath -> IO (Either String a)
readSafeJsonFile path = do
  contents <- readFile path
  M.void $ evaluate $ length contents
  pure $ parseSafeJson path contents

parseSafeJson :: FromSafeJson a => FilePath -> String -> Either String a
parseSafeJson path str = case parse safeJson path str of
  Left err -> Left $ show err
  Right value -> fromSafeJson value

safeJson :: Parsec String () SafeValue
safeJson = do
  skipSpace
  value <- jsonValue
  skipSpace
  eof
  pure value

jsonValue :: Parsec String () SafeValue
jsonValue = do
  SafeString <$> jsonString
    <|> SafeNumber <$> jsonNumber
    <|> SafeBool <$> jsonBool
    <|> SafeNull <$ jsonNull
    <|> SafeArray <$> jsonArray
    <|> SafeObject <$> jsonObject

jsonString :: Parsec String () String
jsonString = do
  char '"'
  str <- many jsonChar
  char '"'
  pure str

jsonChar :: Parsec String () Char
jsonChar = noneOf "\"\\" <|> jsonEscape

jsonEscape :: Parsec String () Char
jsonEscape = do
  char '\\'
  c <- anyChar
  case c of
    '"' -> pure '"'
    '\\' -> pure '\\'
    '/' -> pure '/'
    'b' -> pure '\b'
    'f' -> pure '\f'
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    'u' -> jsonUnicode
    _ -> fail "invalid escape character"

jsonUnicode :: Parsec String () Char
jsonUnicode = do
  a <- hex <$> hexDigit
  b <- hex <$> hexDigit
  c <- hex <$> hexDigit
  d <- hex <$> hexDigit
  pure $ Char.chr $ 0x1000 * a + 0x100 * b + 0x10 * c + d

hex :: Char -> Int
hex c
  | Char.isDigit c = Char.digitToInt c
  | otherwise = 10 + Char.ord (Char.toLower c) - Char.ord 'a'

jsonNumber :: Parsec String () Double
jsonNumber = do
  sign <- optionMaybe $ char '-'
  int <- jsonInt
  frac <- optionMaybe jsonFrac
  ex <- optionMaybe jsonExp
  pure $ read $ maybe "" pure sign ++ int ++ fromMaybe "" frac ++ fromMaybe "" ex

jsonInt :: Parsec String () String
jsonInt = do
  int <- many1 digit
  if head int == '0' && length int > 1
    then fail "leading zero"
    else pure int

jsonFrac :: Parsec String () String
jsonFrac = do
  M.void $ char '.'
  frac <- many1 digit
  if last frac == '0'
    then fail "trailing zero"
    else pure frac

jsonExp :: Parsec String () String
jsonExp = do
  e <- oneOf "eE"
  sign <- optionMaybe $ oneOf "+-"
  ex <- many1 digit
  pure $ e : maybe "" pure sign ++ ex

jsonBool :: Parsec String () Bool
jsonBool = True <$ string "true" <|> False <$ string "false"

jsonNull :: Parsec String () ()
jsonNull = M.void $ string "null"

jsonArray :: Parsec String () [SafeValue]
jsonArray = do
  char '['
  skipSpace
  values <- jsonValue `sepBy` try (skipSpace >> char ',' >> skipSpace)
  skipSpace
  char ']'
  pure values

jsonObject :: Parsec String () (Map.Map String SafeValue)
jsonObject = do
  char '{'
  skipSpace
  pairs <- jsonPair `sepBy` try (skipSpace >> char ',' >> skipSpace)
  skipSpace
  char '}'
  pure $ Map.fromList pairs

jsonPair :: Parsec String () (String, SafeValue)
jsonPair = do
  key <- jsonString
  skipSpace
  char ':'
  skipSpace
  value <- jsonValue
  pure (key, value)

skipSpace :: Parsec String () ()
skipSpace = do
  skipSpaceImpl
  optionMaybe skipComment >>= \case
    Nothing -> pure ()
    Just () -> skipSpace

skipSpaceImpl :: Parsec String () ()
skipSpaceImpl = do
  M.void $ many $ oneOf " \t\r\n"

skipComment :: Parsec String () ()
skipComment = do
  M.void $ char '/'
  skipSingleLineCommentImpl <|> skipMultiLineCommentImpl

skipMultiLineCommentImpl :: Parsec String () ()
skipMultiLineCommentImpl = do
  M.void $ char '*'
  M.void $ manyTill anyChar $ try $ string "*/"

skipSingleLineCommentImpl :: Parsec String () ()
skipSingleLineCommentImpl = do
  M.void $ char '/'
  M.void $ many $ noneOf "\n"

class ToSafeJson a where
  toSafeJson :: a -> SafeValue

class FromSafeJson a where
  fromSafeJson :: SafeValue -> Either String a

store :: ToSafeJson a => String -> a -> (String, SafeValue)
store k v = (k, toSafeJson v)

fetch :: (FromSafeJson a) => Map.Map String SafeValue -> String -> Either String a
fetch m k = case Map.lookup k m of
  Nothing -> Left $ "missing key: " ++ k
  Just v -> fromSafeJson v

instance ToSafeJson SafeValue where
  toSafeJson = id

instance FromSafeJson SafeValue where
  fromSafeJson = Right

instance ToSafeJson String where
  toSafeJson = SafeString

instance FromSafeJson String where
  fromSafeJson = \case
    SafeString s -> Right s
    _ -> Left "expected string"

instance ToSafeJson Double where
  toSafeJson = SafeNumber

instance FromSafeJson Double where
  fromSafeJson = \case
    SafeNumber n -> Right n
    _ -> Left "expected number"

instance ToSafeJson Bool where
  toSafeJson = SafeBool

instance FromSafeJson Bool where
  fromSafeJson = \case
    SafeBool b -> Right b
    _ -> Left "expected bool"

instance ToSafeJson () where
  toSafeJson = const SafeNull

instance FromSafeJson () where
  fromSafeJson = \case
    SafeNull -> Right ()
    _ -> Left "expected null"

instance (ToSafeJson a, ToSafeJson b) => ToSafeJson (a, b) where
  toSafeJson (a, b) = SafeArray [toSafeJson a, toSafeJson b]

instance (FromSafeJson a, FromSafeJson b) => FromSafeJson (a, b) where
  fromSafeJson = \case
    SafeArray [a, b] -> (,) <$> fromSafeJson a <*> fromSafeJson b
    _ -> Left "expected array of length 2"

instance (ToSafeJson a, ToSafeJson b, ToSafeJson c) => ToSafeJson (a, b, c) where
  toSafeJson (a, b, c) = SafeArray [toSafeJson a, toSafeJson b, toSafeJson c]

instance (FromSafeJson a, FromSafeJson b, FromSafeJson c) => FromSafeJson (a, b, c) where
  fromSafeJson = \case
    SafeArray [a, b, c] -> (,,) <$> fromSafeJson a <*> fromSafeJson b <*> fromSafeJson c
    _ -> Left "expected array of length 3"

instance (ToSafeJson a, ToSafeJson b, ToSafeJson c, ToSafeJson d) => ToSafeJson (a, b, c, d) where
  toSafeJson (a, b, c, d) = SafeArray [toSafeJson a, toSafeJson b, toSafeJson c, toSafeJson d]

instance (FromSafeJson a, FromSafeJson b, FromSafeJson c, FromSafeJson d) => FromSafeJson (a, b, c, d) where
  fromSafeJson = \case
    SafeArray [a, b, c, d] -> (,,,) <$> fromSafeJson a <*> fromSafeJson b <*> fromSafeJson c <*> fromSafeJson d
    _ -> Left "expected array of length 4"

instance (ToSafeJson a, ToSafeJson b, ToSafeJson c, ToSafeJson d, ToSafeJson e) => ToSafeJson (a, b, c, d, e) where
  toSafeJson (a, b, c, d, e) = SafeArray [toSafeJson a, toSafeJson b, toSafeJson c, toSafeJson d, toSafeJson e]

instance (FromSafeJson a, FromSafeJson b, FromSafeJson c, FromSafeJson d, FromSafeJson e) => FromSafeJson (a, b, c, d, e) where
  fromSafeJson = \case
    SafeArray [a, b, c, d, e] -> (,,,,) <$> fromSafeJson a <*> fromSafeJson b <*> fromSafeJson c <*> fromSafeJson d <*> fromSafeJson e
    _ -> Left "expected array of length 5"

instance ToSafeJson a => ToSafeJson [(String, a)] where
  toSafeJson = SafeObject . Map.fromList . map (\(k, v) -> (k, toSafeJson v))

instance FromSafeJson a => FromSafeJson [(String, a)] where
  fromSafeJson = \case
    SafeObject m -> mapM (\(k, v) -> (,) k <$> fromSafeJson v) $ Map.toList m
    _ -> Left "expected object"

instance (ToSafeJson a) => ToSafeJson (Maybe a) where
  toSafeJson = \case
    Nothing -> SafeNull
    Just x -> toSafeJson x

instance (FromSafeJson a) => FromSafeJson (Maybe a) where
  fromSafeJson = \case
    SafeNull -> Right Nothing
    x -> Just <$> fromSafeJson x

instance ToSafeJson Char where
  toSafeJson = SafeString . pure

instance FromSafeJson Char where
  fromSafeJson = \case
    SafeString [c] -> Right c
    _ -> Left "expected string of length 1"

instance FromSafeJson Int where
  fromSafeJson = \case
    SafeNumber n ->
      let i = truncate n
       in if fromIntegral i == n
            then Right i
            else Left "expected integer"
    _ -> Left "expected number"

instance ToSafeJson Int where
  toSafeJson = SafeNumber . fromIntegral

instance FromSafeJson Integer where
  fromSafeJson = \case
    SafeNumber n ->
      let i = truncate n
       in if fromIntegral i == n
            then Right i
            else Left "expected integer"
    _ -> Left "expected number"

instance ToSafeJson Integer where
  toSafeJson = SafeNumber . fromIntegral

instance FromSafeJson Float where
  fromSafeJson = \case
    SafeNumber n -> Right $ realToFrac n
    _ -> Left "expected number"

instance ToSafeJson Float where
  toSafeJson = SafeNumber . realToFrac

instance ToSafeJson a => ToSafeJson (Map.Map String a) where
  toSafeJson = SafeObject . Map.map toSafeJson

instance FromSafeJson a => FromSafeJson (Map.Map String a) where
  fromSafeJson = \case
    SafeObject m -> traverse fromSafeJson m
    _ -> Left "expected object"

instance ToSafeJson a => ToSafeJson (Map.Map Int a) where
  toSafeJson = toSafeJson . Map.mapKeys show

instance FromSafeJson a => FromSafeJson (Map.Map Int a) where
  fromSafeJson = fmap (Map.mapKeys goRead) . fromSafeJson
   where
    goRead :: String -> Int
    goRead s = case reads s of
      [(i, "")] -> i
      _ -> error "expected int"

instance ToSafeJson a => ToSafeJson (Map.Map Integer a) where
  toSafeJson = toSafeJson . Map.mapKeys show

instance FromSafeJson a => FromSafeJson (Map.Map Integer a) where
  fromSafeJson = fmap (Map.mapKeys goRead) . fromSafeJson
   where
    goRead :: String -> Integer
    goRead s = case reads s of
      [(i, "")] -> i
      _ -> error "expected int"
