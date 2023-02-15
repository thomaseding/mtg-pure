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
  CommandAliases (..),
  defaultCommandAliases,
  validateCommandAliases,
  unValidatedCommandAliases,
  CommandAbilityIndex (..),
  CIPriorityAction (..),
  CIAttack (..),
  CIBlock (..),
  runParseCIPriorityAction,
  runParseCIAttack,
  runParseCIBlock,
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

data CIPriorityAction :: Type where
  CIActivateAbility :: ObjectId -> CommandAbilityIndex -> [ObjectId] -> CIPriorityAction
  CIAskAgain :: CIPriorityAction
  CICastSpell :: ObjectId -> [ObjectId] -> CIPriorityAction
  CIConcede :: CIPriorityAction
  CIExamineAbility :: ObjectId -> CommandAbilityIndex -> CIPriorityAction
  CIExamineObject :: ObjectId -> CIPriorityAction
  CIHelp :: Maybe String -> CIPriorityAction
  CIPass :: CIPriorityAction -- TODO: Augment this to allow passing to a phase or step.
  CIPlayLand :: ObjectId -> [ObjectId] -> CIPriorityAction
  CIQuit :: CIPriorityAction

data CIAttack :: Type where
  CIAttack :: [ObjectId] -> CIAttack
  deriving (Show)

data CIBlock :: Type where
  CIBlock :: [(ObjectId {-attacker-}, ObjectId {-blocker-})] -> CIBlock
  deriving (Show)

instance Show CIPriorityAction where
  show = \case
    CIActivateAbility objectId abilityIndex extras -> activateAbility ++ " " ++ showId objectId ++ " " ++ show abilityIndex ++ showExtras extras
    CIAskAgain -> askAgain
    CICastSpell spellId extras -> castSpell ++ " " ++ showId spellId ++ showExtras extras
    CIConcede -> concede
    CIExamineAbility objectId abilityIndex -> examine ++ " " ++ showId objectId ++ " " ++ show abilityIndex
    CIExamineObject objectId -> examine ++ " " ++ showId objectId
    CIHelp (Just n) -> help ++ " " ++ show n
    CIHelp Nothing -> help
    CIPass -> pass
    CIPlayLand landId extras -> playLand ++ " " ++ showId landId ++ showExtras extras
    CIQuit -> quit
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
    activateAbility = go $ caActivateAbility ca
    askAgain = "CIAskAgain"
    castSpell = go $ caCastSpell ca
    concede = go $ caConcede ca
    examine = go $ caExamine ca
    help = go $ caHelp ca
    pass = go $ caPass ca
    playLand = go $ caPlayLand ca
    quit = go $ caQuit ca

data CommandAliases = CommandAliases
  { ca_ :: ()
  , caActivateAbility :: [String]
  , caCastSpell :: [String]
  , caConcede :: [String]
  , caExamine :: [String]
  , caHelp :: [String]
  , caPass :: [String]
  , caPlayLand :: [String]
  , caQuit :: [String]
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
    [ go $ caActivateAbility ca
    , go $ caCastSpell ca
    , go $ caConcede ca
    , go $ caExamine ca
    , go $ caHelp ca
    , go $ caPass ca
    , go $ caPlayLand ca
    , go $ caQuit ca
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
        { ca_ = ()
        , caActivateAbility = ["activateAbility", "activate", "1"]
        , caCastSpell = ["castSpell", "cast", "2"]
        , caConcede = ["concede"]
        , caExamine = ["examine", "look", "+"]
        , caHelp = ["help", "h", "?", "--help", "-h", "-?", "/help", "/h", "/?"]
        , caPass = ["pass", "p", "0"]
        , caPlayLand = ["playLand", "3"]
        , caQuit = ["quit"]
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

dotSpaceAfterAlphaNum :: String -> Parser ()
dotSpaceAfterAlphaNum s = case reverse s of
  [] -> parserFail "dotSpaceAfterAlphaNum: empty string"
  c : _ -> case Char.isAlphaNum c of
    True -> dotSpace
    False -> pure ()

parseCommandHeader' :: [String] -> Parser String
parseCommandHeader' headers = do
  choice $ map (try . ciString) headers

parseCommandHeader :: [String] -> Parser ()
parseCommandHeader headers = do
  s <- parseCommandHeader' headers
  dotSpaceAfterDigit s

parseCompositeHelp :: CommandAliases -> Parser CIPriorityAction
parseCompositeHelp ca = do
  s <- parseCommandHeader' $ caHelp ca
  dotSpaceAfterAlphaNum s
  dotSpaces
  topic <- many1 $ satisfy Char.isAlphaNum
  pure $ CIHelp $ Just topic

parseSimpleHelp :: CommandAliases -> Parser CIPriorityAction
parseSimpleHelp ca = do
  parseCommandHeader $ caHelp ca
  pure $ CIHelp Nothing

parseQuit :: CommandAliases -> Parser CIPriorityAction
parseQuit ca = do
  parseCommandHeader $ caQuit ca
  pure CIQuit

parseConcede :: CommandAliases -> Parser CIPriorityAction
parseConcede ca = do
  parseCommandHeader $ caConcede ca
  pure CIConcede

parseExamine :: CommandAliases -> Parser CIPriorityAction
parseExamine ca = do
  parseCommandHeader $ caExamine ca
  dotSpaces
  objectId <- ObjectId . read <$> many1 digit
  dotSpaces
  mAbilityIndex <- optionMaybe parseAbilityIndex
  pure case mAbilityIndex of
    Nothing -> CIExamineObject objectId
    Just abilityIndex -> CIExamineAbility objectId abilityIndex

parsePass :: CommandAliases -> Parser CIPriorityAction
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

parseActivateAbility :: CommandAliases -> Parser CIPriorityAction
parseActivateAbility ca = do
  parseCommandHeader $ caActivateAbility ca
  dotSpaces
  objId <- ObjectId . read <$> many1 digit
  dotSpaces
  abilityIndex <- parseAbilityIndex
  targetIds <- parseTargets
  pure $ CIActivateAbility objId abilityIndex targetIds

parseCastSpell :: CommandAliases -> Parser CIPriorityAction
parseCastSpell ca = do
  parseCommandHeader $ caCastSpell ca
  dotSpaces
  objId <- ObjectId . read <$> many1 digit
  targetIds <- parseTargets
  pure $ CICastSpell objId targetIds

parsePlayLand :: CommandAliases -> Parser CIPriorityAction
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

comment :: Parser ()
comment = do
  M.void $ char '#'
  M.void $ many anyChar

parseCommandInput :: Parser a -> Parser a
parseCommandInput parser = try do
  command <- parser
  dotSpaces
  optional comment
  eof
  pure command

parseCIPriorityAction :: CommandAliases -> Parser CIPriorityAction
parseCIPriorityAction ca = do
  choice $
    map
      parseCommandInput
      [ parseActivateAbility ca
      , parseCastSpell ca
      , parseCompositeHelp ca
      , parseConcede ca
      , parseExamine ca
      , parsePass ca
      , parsePlayLand ca
      , parseQuit ca
      , parseSimpleHelp ca
      ]

parseCIAttack :: Parser CIAttack
parseCIAttack = parseCommandInput do
  dotSpaces
  attackerIds <- many $ try do
    dotSpaces
    ObjectId . read <$> many1 digit
  pure $ CIAttack attackerIds

parseCIBlock :: Parser CIBlock
parseCIBlock = parseCommandInput do
  dotSpaces
  attackerBlockerPairs <- many $ try do
    dotSpaces
    attackerId <- ObjectId . read <$> many1 digit
    dotSpaces
    blockerId <- ObjectId . read <$> many1 digit
    pure (attackerId, blockerId)
  pure $ CIBlock attackerBlockerPairs

runParseCIPriorityAction :: ValidatedCommandAliases -> String -> Either ParseError CIPriorityAction
runParseCIPriorityAction (Validated ca) s = parse (parseCIPriorityAction ca) "(repl)" s'
 where
  s' = s ++ " "

runParseCIAttack :: String -> Either ParseError CIAttack
runParseCIAttack s = parse parseCIAttack "(repl)" s'
 where
  s' = s ++ " "

runParseCIBlock :: String -> Either ParseError CIBlock
runParseCIBlock s = parse parseCIBlock "(repl)" s'
 where
  s' = s ++ " "
