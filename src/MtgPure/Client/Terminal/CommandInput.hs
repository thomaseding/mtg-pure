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
{-# HLINT ignore "Use <$>" #-}

module MtgPure.Client.Terminal.CommandInput (
  CommandAbilityIndex (..),
  CommandInput (..),
  CommandAliases (..),
  defaultCommandAliases,
  validateCommandAliases,
  unValidatedCommandAliases,
  runParseCommandInput,
) where

import safe qualified Control.Monad as M
import safe qualified Data.Char as Char
import safe Data.Kind (Type)
import safe qualified Data.Set as Set
import safe MtgPure.Model.BasicLandType (BasicLandType (..))
import safe MtgPure.Model.Object.ObjectId (ObjectId (..))
import safe Text.Parsec (
  ParseError,
  anyChar,
  char,
  choice,
  digit,
  eof,
  many,
  many1,
  optionMaybe,
  optional,
  parse,
  parserFail,
  satisfy,
  space,
  try,
  (<|>),
 )
import safe Text.Parsec.String (Parser)

data CommandAbilityIndex :: Type where
  CIAbilityIndex :: Int -> CommandAbilityIndex
  CIManaAbility :: Maybe BasicLandType -> CommandAbilityIndex
  CIInferManaAbility :: CommandAbilityIndex

instance Show CommandAbilityIndex where
  show = \case
    CIAbilityIndex n -> show n
    CIManaAbility (Just landType) -> case landType of
      Plains -> "W"
      Island -> "U"
      Swamp -> "B"
      Mountain -> "R"
      Forest -> "G"
    CIManaAbility Nothing -> "C"
    CIInferManaAbility -> "*"

data CommandInput :: Type where
  CIQuit :: CommandInput
  CIConcede :: CommandInput
  CIAskAgain :: CommandInput
  CIHelp :: Maybe String -> CommandInput
  CIExamineAbility :: ObjectId -> CommandAbilityIndex -> CommandInput
  CIExamineObject :: ObjectId -> CommandInput
  CIPass :: CommandInput
  CIActivateAbility :: ObjectId -> CommandAbilityIndex -> [ObjectId] -> CommandInput
  CICastSpell :: ObjectId -> [ObjectId] -> CommandInput
  CIPlayLand :: ObjectId -> [ObjectId] -> CommandInput

instance Show CommandInput where
  show = \case
    CIQuit -> quit
    CIConcede -> concede
    CIAskAgain -> askAgain
    CIHelp Nothing -> help
    CIHelp (Just n) -> help ++ " " ++ show n
    CIExamineAbility objectId abilityIndex -> examine ++ " " ++ showId objectId ++ " " ++ show abilityIndex
    CIExamineObject objectId -> examine ++ " " ++ showId objectId
    CIPass -> pass
    CIActivateAbility objectId abilityIndex extras -> activateAbility ++ " " ++ showId objectId ++ " " ++ show abilityIndex ++ showExtras extras
    CICastSpell spellId extras -> castSpell ++ " " ++ showId spellId ++ showExtras extras
    CIPlayLand landId extras -> playLand ++ " " ++ showId landId ++ showExtras extras
   where
    showId = show . unObjectId
    showExtras = \case
      [] -> ""
      xs -> " " ++ unwords (map showId xs)
    go = \case
      [] -> error "CommandInput.show.go: empty list"
      s : _ -> case s of
        "" -> error "CommandInput.show.go: empty string"
        c : cs -> Char.toUpper c : cs
    ca = unValidatedCommandAliases defaultCommandAliases
    askAgain = "AskAgain"
    quit = go $ caQuit ca
    concede = go $ caConcede ca
    help = go $ caHelp ca
    examine = go $ caExamine ca
    pass = go $ caPass ca
    activateAbility = go $ caActivateAbility ca
    castSpell = go $ caCastSpell ca
    playLand = go $ caPlayLand ca

data CommandAliases = CommandAliases
  { caQuit :: [String]
  , caConcede :: [String]
  , caHelp :: [String]
  , caExamine :: [String]
  , caPass :: [String]
  , caActivateAbility :: [String]
  , caCastSpell :: [String]
  , caPlayLand :: [String]
  }

newtype ValidatedCommandAliases = Validated CommandAliases

unValidatedCommandAliases :: ValidatedCommandAliases -> CommandAliases
unValidatedCommandAliases (Validated ca) = ca

isCommandAliasValid :: String -> Bool
isCommandAliasValid = all cond
 where
  cond c = not (Char.isSpace c) && c /= '.' && c /= '#'

areCommandAliasesValid :: CommandAliases -> Bool
areCommandAliasesValid ca =
  and
    [ go $ caQuit ca
    , go $ caConcede ca
    , go $ caHelp ca
    , go $ caExamine ca
    , go $ caPass ca
    , go $ caActivateAbility ca
    , go $ caCastSpell ca
    , go $ caPlayLand ca
    , unique
    ]
 where
  go = \case
    [] -> False
    s -> all isCommandAliasValid s
  aliases = concatMap ($ ca) [caQuit, caConcede, caHelp, caExamine, caPass, caActivateAbility, caCastSpell, caPlayLand]
  loweredAliases = map (map Char.toLower) aliases
  unique = length loweredAliases == length (Set.fromList loweredAliases)

validateCommandAliases :: CommandAliases -> Maybe ValidatedCommandAliases
validateCommandAliases ca = case areCommandAliasesValid ca of
  True -> Just $ Validated ca
  False -> Nothing

defaultCommandAliases :: ValidatedCommandAliases
defaultCommandAliases = case mValidated of
  Nothing -> error "CommandInput.defaultCommandAliases: impossible"
  Just validated -> validated
 where
  mValidated =
    validateCommandAliases
      CommandAliases
        { caQuit = ["quit"]
        , caConcede = ["concede"]
        , caHelp = ["help", "?"]
        , caExamine = ["examine", "look", "+"]
        , caPass = ["pass", "p", "0"]
        , caActivateAbility = ["activateAbility", "activate", "1"]
        , caCastSpell = ["castSpell", "cast", "2"]
        , caPlayLand = ["playLand", "3"]
        }

ciChar :: Char -> Parser Char
ciChar c = char (Char.toLower c) <|> char (Char.toUpper c)

ciString :: String -> Parser String
ciString = try . mapM ciChar

dotSpace :: Parser ()
dotSpace = M.void $ char '.' <|> space

dotSpaces :: Parser ()
dotSpaces = M.void $ many dotSpace

dotSpaceAfterDigit :: String -> Parser ()
dotSpaceAfterDigit s = case reverse s of
  [] -> parserFail "dotSpaceAfterDigit: empty string"
  c : _ -> case Char.isDigit c of
    True -> dotSpace
    False -> pure ()

parseCommandHeader :: [String] -> Parser ()
parseCommandHeader headers = do
  s <- choice $ map (try . ciString) headers
  dotSpaceAfterDigit s

parseCompositeHelp :: CommandAliases -> Parser CommandInput
parseCompositeHelp ca = do
  parseCommandHeader $ caHelp ca
  dotSpaces
  s <- many1 $ satisfy Char.isAlphaNum
  pure $ CIHelp $ Just s

parseSimpleHelp :: CommandAliases -> Parser CommandInput
parseSimpleHelp ca = do
  parseCommandHeader $ caHelp ca
  pure $ CIHelp Nothing

parseQuit :: CommandAliases -> Parser CommandInput
parseQuit ca = do
  parseCommandHeader $ caQuit ca
  pure CIQuit

parseConcede :: CommandAliases -> Parser CommandInput
parseConcede ca = do
  parseCommandHeader $ caConcede ca
  pure CIConcede

parseExamine :: CommandAliases -> Parser CommandInput
parseExamine ca = do
  parseCommandHeader $ caExamine ca
  dotSpaces
  objectId <- ObjectId . read <$> many1 digit
  dotSpaces
  mAbilityIndex <- optionMaybe parseAbilityIndex
  pure case mAbilityIndex of
    Nothing -> CIExamineObject objectId
    Just abilityIndex -> CIExamineAbility objectId abilityIndex

parsePass :: CommandAliases -> Parser CommandInput
parsePass ca = do
  parseCommandHeader $ caPass ca
  pure CIPass

parseAbilityIndex :: Parser CommandAbilityIndex
parseAbilityIndex = do
  let goMana s a = try do
        s' <- ciString s
        dotSpaceAfterDigit s'
        pure a
  choice
    [ goMana "W" $ CIManaAbility $ Just Plains
    , goMana "U" $ CIManaAbility $ Just Island
    , goMana "B" $ CIManaAbility $ Just Swamp
    , goMana "R" $ CIManaAbility $ Just Mountain
    , goMana "G" $ CIManaAbility $ Just Forest
    , goMana "C" $ CIManaAbility Nothing
    , goMana "*" CIInferManaAbility
    , goMana "-1" $ CIManaAbility $ Just Plains
    , goMana "-2" $ CIManaAbility $ Just Island
    , goMana "-3" $ CIManaAbility $ Just Swamp
    , goMana "-4" $ CIManaAbility $ Just Mountain
    , goMana "-5" $ CIManaAbility $ Just Forest
    , goMana "-6" $ CIManaAbility Nothing
    , goMana "-7" CIInferManaAbility
    , CIAbilityIndex . read <$> many1 digit
    ]

parseActivateAbility :: CommandAliases -> Parser CommandInput
parseActivateAbility ca = do
  parseCommandHeader $ caActivateAbility ca
  dotSpaces
  objId <- ObjectId . read <$> many1 digit
  dotSpaces
  abilityIndex <- parseAbilityIndex
  targetIds <- parseTargets
  pure $ CIActivateAbility objId abilityIndex targetIds

parseCastSpell :: CommandAliases -> Parser CommandInput
parseCastSpell ca = do
  parseCommandHeader $ caCastSpell ca
  dotSpaces
  objId <- ObjectId . read <$> many1 digit
  targetIds <- parseTargets
  pure $ CICastSpell objId targetIds

parsePlayLand :: CommandAliases -> Parser CommandInput
parsePlayLand ca = do
  parseCommandHeader $ caPlayLand ca
  dotSpaces
  objId <- ObjectId . read <$> many1 digit
  targetIds <- parseTargets
  pure $ CIPlayLand objId targetIds

parseTargets :: Parser [ObjectId]
parseTargets = many $ try do
  dotSpaces
  ObjectId . read <$> many1 digit

parseCommandInput' :: Parser CommandInput -> Parser CommandInput
parseCommandInput' parser = try do
  command <- parser
  dotSpaces
  optional comment
  eof
  pure command

parseCommandInput :: CommandAliases -> Parser CommandInput
parseCommandInput ca = do
  choice $
    map
      parseCommandInput'
      [ parseCompositeHelp ca
      , parseSimpleHelp ca
      , parseQuit ca
      , parseConcede ca
      , parseExamine ca
      , parsePass ca
      , parseActivateAbility ca
      , parseCastSpell ca
      , parsePlayLand ca
      ]

comment :: Parser ()
comment = do
  M.void $ char '#'
  M.void $ many anyChar

runParseCommandInput :: ValidatedCommandAliases -> String -> Either ParseError CommandInput
runParseCommandInput (Validated ca) s = parse (parseCommandInput ca) "(repl)" s'
 where
  s' = s ++ " "
