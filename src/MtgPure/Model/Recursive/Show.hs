{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use if" #-}

module MtgPure.Model.Recursive.Show (
  CardDepth,
  DataCombinators (..),
  ShowOptions (..),
  defaultShowOptions,
  runEnvM,
  runEnvMWith,
  showCard,
  showToken,
  showSetCard,
  showSetToken,
  showCardWith,
  showTokenWith,
  showSetCardWith,
  showSetTokenWith,
  showAnyCardWith,
  showAnyTokenWith,
) where

import safe qualified Control.Monad.State.Strict as State
import safe Data.Kind (Type)
import safe qualified Data.List as List
import safe qualified Data.Map.Strict as Map
import safe Data.Maybe (catMaybes, fromMaybe)
import safe Data.Nat (NatList (..))
import safe Data.Proxy (Proxy (Proxy))
import safe Data.String (IsString (..))
import safe Data.Typeable (TypeRep, Typeable, typeOf, typeRep, typeRepArgs)
import safe MtgPure.Model.ArtifactType (ArtifactType)
import safe MtgPure.Model.BasicLandType (BasicLandType)
import safe MtgPure.Model.CardName (CardName (CardName), HasCardName (..))
import safe MtgPure.Model.Color (Color (..))
import safe MtgPure.Model.Colors (Colors (..))
import safe MtgPure.Model.CreatureType (CreatureType)
import safe MtgPure.Model.Damage (Damage, Damage' (..))
import safe MtgPure.Model.Defense (Defense (..))
import safe MtgPure.Model.LandType (LandType (..))
import safe MtgPure.Model.Loyalty (Loyalty)
import safe MtgPure.Model.Mana.Mana (Mana (..))
import safe MtgPure.Model.Mana.ManaCost (
  DynamicManaCost (..),
  HybridManaCost (..),
  ManaCost (..),
  PhyrexianManaCost (..),
 )
import safe MtgPure.Model.Mana.ManaPool (CompleteManaPool (..), ManaPool (..))
import safe MtgPure.Model.Mana.ManaSymbol (ManaSymbol (..))
import safe MtgPure.Model.Object.IsObjectType (IsObjectType (..))
import safe MtgPure.Model.Object.OTN (
  OTN (..),
 )
import safe MtgPure.Model.Object.OTNAliases (
  OTNAbility,
  OTNActivatedAbility,
  OTNActivatedOrTriggeredAbility,
  OTNAny,
  OTNArtifact,
  OTNArtifactLand,
  OTNBattle,
  OTNCard,
  OTNCreature,
  OTNCreaturePlaneswalker,
  OTNCreaturePlayer,
  OTNCreaturePlayerPlaneswalker,
  OTNDamageSource,
  OTNEmblem,
  OTNEnchantment,
  OTNInstant,
  OTNLand,
  OTNNonArtifactPermanent,
  OTNNonCreature,
  OTNNonCreaturePermanent,
  OTNNonEnchantmentPermanent,
  OTNNonLandPermanent,
  OTNNonPlaneswalkerPermanent,
  OTNPermanent,
  OTNPlaneswalker,
  OTNPlayer,
  OTNPlayerPlaneswalker,
  OTNSorcery,
  OTNSpell,
  OTNStaticAbility,
  OTNTriggeredAbility,
 )
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (
  ObjectId (ObjectId),
  UntypedObject (..),
  getObjectId,
 )
import safe MtgPure.Model.Object.ObjectN (ObjectN)
import safe MtgPure.Model.Object.ViewObjectN (viewOTN')
import safe MtgPure.Model.Object.VisitObjectN (visitObjectN')
import safe MtgPure.Model.Power (Power)
import safe MtgPure.Model.PrettyType (PrettyType (..))
import safe MtgPure.Model.Recursive (
  Ability (..),
  ActivatedAbility (..),
  AnyCard (..),
  AnyToken (..),
  BattleType (..),
  Card (..),
  CardCharacteristic (..),
  CardSpec (..),
  Case (..),
  Condition (..),
  Cost (..),
  Effect (..),
  Elect (..),
  Else (..),
  Enchant (..),
  EnchantmentType (..),
  EntersStatic (..),
  Event,
  EventListener,
  EventListener' (..),
  IsUser (..),
  List (..),
  Requirement (..),
  SetCard,
  SetToken,
  SomeZone (..),
  StaticAbility (..),
  Token (..),
  TriggeredAbility (..),
  WithLinkedObject (..),
  WithList (..),
  WithMaskedObject (..),
  WithMaskedObjects (..),
  WithThis (..),
  WithThisAbility (..),
  WithThisActivated,
  WithThisOneShot,
  WithThisStatic,
  WithThisTriggered,
  WithThisZ (..),
 )
import safe MtgPure.Model.Recursive.Tree (
  BuildTree,
  Tree (..),
  TreeConfig (..),
  buildTree,
 )
import safe MtgPure.Model.Supertype (Supertype (..))
import safe MtgPure.Model.TimePoint (TimePoint (..))
import safe MtgPure.Model.Toughness (Toughness)
import safe MtgPure.Model.Variable (
  Var (..),
  Variable (..),
  VariableId,
  VariableId' (..),
  getVariableId,
 )
import safe MtgPure.Model.Zone (IsZone (..), Zone (..))
import safe MtgPure.Model.ZoneObject.ZoneObject (
  IsOTN,
  IsZO,
  ZO,
  ZoneObject (..),
 )
import safe Prelude hiding (showList)

----------------------------------------

defaultDepthLimit :: Maybe Int
defaultDepthLimit = Nothing

-- | @buildTree@ always constructs the full tree; card-depth truncation happens
-- in the renderer via the 'EnvM' 'cardDepth' seeded by 'runEnvM'.
fullTreeConfig :: TreeConfig
fullTreeConfig = treeConfigForDepth Nothing

showViaTree :: (BuildTree a) => (Tree a -> EnvM Doc) -> a -> EnvM Doc
showViaTree render = render . buildTree fullTreeConfig

runShowWith :: ShowOptions -> (a -> EnvM Doc) -> a -> String
runShowWith opts render = runEnvMWith opts defaultDepthLimit . render

runShow :: (a -> EnvM Doc) -> a -> String
runShow = runShowWith defaultShowOptions

treeShow :: (BuildTree a) => (Tree a -> EnvM Doc) -> a -> String
treeShow render = runShow (showViaTree render)

-- | Render a value with explicit 'ShowOptions'.
showCardWith :: ShowOptions -> Card ot -> String
showCardWith opts = runShowWith opts showCard

showTokenWith :: ShowOptions -> Token ot -> String
showTokenWith opts = runShowWith opts showToken

showSetCardWith :: ShowOptions -> SetCard ot -> String
showSetCardWith opts = runShowWith opts showSetCard

showSetTokenWith :: ShowOptions -> SetToken ot -> String
showSetTokenWith opts = runShowWith opts showSetToken

showAnyCardWith :: ShowOptions -> AnyCard -> String
showAnyCardWith opts = runShowWith opts showAnyCard

showAnyTokenWith :: ShowOptions -> AnyToken -> String
showAnyTokenWith opts = runShowWith opts showAnyToken

instance Show (Ability zone ot) where
  show :: Ability zone ot -> String
  show = treeShow showTreeAbility

instance Show (ActivatedAbility zone ot) where
  show :: ActivatedAbility zone ot -> String
  show = treeShow showTreeActivatedAbility

instance Show AnyCard where
  show :: AnyCard -> String
  show = runShow showAnyCard

instance Show AnyToken where
  show :: AnyToken -> String
  show = runShow showAnyToken

instance Show BattleType where
  show :: BattleType -> String
  show = treeShow showTreeBattleType

instance Show (Card ot) where
  show :: Card ot -> String
  show = runShow showCard

instance Show (CardCharacteristic ot) where
  show :: CardCharacteristic ot -> String
  show = treeShow showTreeCardCharacteristic

instance Show (CardSpec ot) where
  show :: CardSpec ot -> String
  show = treeShow showTreeCardSpec

instance Show CompleteManaPool where
  show :: CompleteManaPool -> String
  show = runShow showCompleteManaPool

instance Show Condition where
  show :: Condition -> String
  show = treeShow showTreeCondition

instance Show Cost where
  show :: Cost -> String
  show = treeShow showTreeCost

instance Show (DynamicManaCost var) where
  show :: DynamicManaCost var -> String
  show = runShow showDynamicManaCost

instance Show (Effect ef) where
  show :: Effect ef -> String
  show = treeShow showTreeEffect

instance Show (Elect s el ot) where
  show :: Elect s el ot -> String
  show = treeShow showTreeElect

instance Show (EnchantmentType ot) where
  show :: EnchantmentType ot -> String
  show = treeShow showTreeEnchantmentType

instance Show EventListener where
  show :: EventListener -> String
  show = treeShow showTreeEventListener

instance Show (HybridManaCost var) where
  show :: HybridManaCost var -> String
  show = runShow showHybridManaCost

instance Show (ManaCost var) where
  show :: ManaCost var -> String
  show = runShow showManaCost

instance Show (ManaPool snow) where
  show :: ManaPool snow -> String
  show = runShow showManaPool

instance Show (PhyrexianManaCost var) where
  show :: PhyrexianManaCost var -> String
  show = runShow showPhyrexianManaCost

instance Show (Requirement zone ot) where
  show :: Requirement zone ot -> String
  show = treeShow showTreeRequirement

instance Show (SetCard ot) where
  show :: SetCard ot -> String
  show = runShow showSetCard

instance Show (SetToken ot) where
  show :: SetToken ot -> String
  show = runShow showSetToken

instance Show (StaticAbility zone ot) where
  show :: StaticAbility zone ot -> String
  show = treeShow showTreeStaticAbility

instance Show (Token ot) where
  show :: Token ot -> String
  show = runShow showToken

instance Show (TriggeredAbility zone ot) where
  show :: TriggeredAbility zone ot -> String
  show = treeShow showTreeTriggeredAbility

instance (IsZO zone ot) => Show (WithMaskedObject (Elect s e) zone ot) where
  show :: (IsZO zone ot) => WithMaskedObject (Elect s e) zone ot -> String
  show = treeShow (showTreeWithMaskedObject showTreeElect "obj")

instance (IsOTN ot) => Show (SomeZone WithThisAbility ot) where
  show :: (IsOTN ot) => SomeZone WithThisAbility ot -> String
  show = treeShow (showTreeSomeZoneWithThisAbility "this")

instance (IsOTN ot) => Show (SomeZone (WithThisZ ActivatedAbility) ot) where
  show :: (IsOTN ot) => SomeZone (WithThisZ ActivatedAbility) ot -> String
  show = treeShow (showTreeSomeZone (showTreeWithThisZ showTreeActivatedAbility "this"))

instance (IsOTN ot) => Show (SomeZone (WithThisZ StaticAbility) ot) where
  show :: (IsOTN ot) => SomeZone (WithThisZ StaticAbility) ot -> String
  show = treeShow (showTreeSomeZone (showTreeWithThisZ showTreeStaticAbility "this"))

instance (IsOTN ot) => Show (SomeZone (WithThisZ TriggeredAbility) ot) where
  show :: (IsOTN ot) => SomeZone (WithThisZ TriggeredAbility) ot -> String
  show = treeShow (showTreeSomeZone (showTreeWithThisZ showTreeTriggeredAbility "this"))

instance (IsZO zone ot) => Show (WithThis (Ability zone) zone ot) where
  show :: (IsZO zone ot) => WithThis (Ability zone) zone ot -> String
  show = treeShow (showTreeWithThis showTreeAbility "this")

instance (IsZO zone ot) => Show (WithThisAbility zone ot) where
  show :: (IsZO zone ot) => WithThisAbility zone ot -> String
  show = treeShow (showTreeWithThisAbility "this")

instance (IsZO zone ot) => Show (WithThisActivated zone ot) where
  show :: (IsZO zone ot) => WithThisActivated zone ot -> String
  show = treeShow (showTreeWithThis (\case TreeElectOT e -> showTreeElect e) "this")

instance (IsZO 'ZStack ot) => Show (WithThisOneShot ot) where
  show :: (IsZO 'ZStack ot) => WithThisOneShot ot -> String
  show = treeShow (showTreeWithThis showTreeElect "this")

instance (IsZO zone ot) => Show (WithThisStatic zone ot) where
  show :: (IsZO zone ot) => WithThisStatic zone ot -> String
  show = treeShow (showTreeWithThis showTreeStaticAbility "this")

instance (IsZO zone ot) => Show (WithThisTriggered zone ot) where
  show :: (IsZO zone ot) => WithThisTriggered zone ot -> String
  show = treeShow (showTreeWithThis showTreeTriggeredAbility "this")

----------------------------------------

tryLitMana :: Mana var snow mt -> Maybe Int
tryLitMana = \case
  Mana x -> Just x
  VariableMana{} -> Nothing
  SumMana{} -> Nothing

litMana :: Mana 'NoVar snow mt -> Int
litMana = \case
  Mana x -> x

----------------------------------------
-- Render tree.
--
-- Structural renderers build a `Doc`: a faithful AST of the Haskell expression
-- being shown (applications, lambdas, records, lists, type applications, and
-- atoms). The `Doc` itself carries no surface-syntax decisions. A single
-- `layout` pass flattens it to a stream of atoms, and that pass is the ONLY
-- place that considers parentheses, `$`, block arguments, and multiline
-- formatting -- all derived from the `Doc`'s shape (see `needsParens`).
-- Object/variable references stay as atoms so id-remapping and used/wildcard
-- analysis happen in the final `[Doc] -> String` pass.

data Doc :: Type where
  -- | A rendered token: a name, number, mana string, etc. Kept as a single
  -- unit; a Haskell application is a 'DApp', not a baked-in @DString "Foo x"@.
  -- (A pre-rendered token /may/ contain spaces -- e.g. @toManaCost (W,W)@ or a
  -- @show@n enum -- as long as it holds no object/variable atom; `needsParens`
  -- then parenthesizes it by the presence of a space.)
  DString :: String -> Doc
  -- | An object reference: its id and the generation that disambiguates
  -- shadowed binders. Rendered (and remapped) in the final pass.
  DObject :: ObjectId -> Generation -> Doc
  -- | A variable reference. Rendered (and remapped) in the final pass.
  DVariable :: VariableId -> Doc
  -- | A flat sequence of docs. The concatenation vehicle for `layout`'s output
  -- (it replaces the old @Items@ @DList@); 'Semigroup'/'Monoid' go through it.
  DSeq :: [Doc] -> Doc
  -- | Application: @head arg1 arg2 ...@.
  DApp :: Doc -> [Doc] -> Doc
  -- | @\\binder -> body@. The binder already includes the leading backslash.
  DLam :: Doc -> Doc -> Doc
  -- | Record constructor. Single-line: positional like 'DApp'. Multiline (D):
  -- record syntax with each field on its own (leading-comma) line.
  DRec :: Doc -> [(String, Doc)] -> Doc
  -- | @[d1, d2, ...]@.
  DList :: [Doc] -> Doc
  -- | Type application @head \@ty1 \@ty2 ... arg1 ...@: each @ty@ atom is kept as
  -- its own child 'Doc' (not flattened into the head) so the smart-aliasing pass
  -- (E) can match and replace it structurally, and so `layout` can parenthesize
  -- @\@(ty)@ when the type token needs it. The first list is the @\@ty@ type
  -- arguments (e.g. @masked \@ot \@'ZGraveyard@); the second the value arguments.
  DTypeApp :: String -> [Doc] -> [Doc] -> Doc
  -- | A piece of a destructured object binder, used by (F)
  -- 'NoDataCombinators' to rebuild coerced object references with constructors
  -- (see 'ObjectPart'). Rendered (and numbered like the object's 'DObject'
  -- atom) in the final pass.
  DObjectPart :: ObjectPart -> ObjectId -> Generation -> Doc
  deriving (Eq)

-- | Which piece of a destructured object binder a 'DObjectPart' atom is. Under
-- (F) 'NoDataCombinators' a binder renders as @name\@(ZO sng\<n\> objN\<n\>)@
-- and a use-site that views the object at a wider type rebuilds it as
-- @ZO sng\<n\> (ON\<k\>\<letter\> (... objN\<n\>))@ -- constructors only, no
-- @toZO\<n\>@\/@asFoo@ coercion functions.
data ObjectPart :: Type where
  -- | The @\@(ZO sng\<n\> objN\<n\>)@ as-pattern suffix on the binder name.
  -- Renders as the empty string unless some use-site coerces this binder
  -- (i.e. some 'ObjectPartSing'\/'ObjectPartPayload' atom shares its id and
  -- generation), so an undestructured binder stays a bare name.
  ObjectPartPattern :: ObjectPart
  -- | The bound @SingZone zone@ payload, rendered @sng\<n\>@.
  ObjectPartSing :: ObjectPart
  -- | The bound @ObjectN ot@ payload, rendered @objN\<n\>@.
  ObjectPartPayload :: ObjectPart
  deriving (Eq, Typeable)

instance IsString Doc where
  fromString :: String -> Doc
  fromString = DString

instance Semigroup Doc where
  (<>) :: Doc -> Doc -> Doc
  DSeq xs <> DSeq ys = DSeq (xs <> ys)
  DSeq xs <> y = DSeq (xs <> [y])
  x <> DSeq ys = DSeq (x : ys)
  x <> y = DSeq [x, y]

instance Monoid Doc where
  mempty :: Doc
  mempty = DSeq []

dintercalate :: Doc -> [Doc] -> Doc
dintercalate sep = mconcat . List.intersperse sep

-- | Does this 'Doc' need surrounding parens when it appears as a function
-- argument? Purely a function of shape -- no flag is stored anywhere. A
-- pre-rendered 'DString' token needs them iff it contains a space (a multi-word
-- token like @Damage 3@ or @toManaCost (W,W)@); a lone token or an already
-- self-bracketed one (@(W,W)@) does not.
needsParens :: Doc -> Bool
needsParens = \case
  DApp _ args -> not (null args)
  DTypeApp _ tys args -> not (null args) || not (null tys)
  DLam{} -> True
  DRec{} -> True
  DString s -> ' ' `elem` s
  DObject{} -> False
  DObjectPart{} -> False
  DVariable{} -> False
  DSeq{} -> False
  DList{} -> False

parenIf :: Bool -> Doc -> Doc
parenIf True d = "(" <> d <> ")"
parenIf False d = d

-- | Application with a plain-string head.
dApp :: String -> [Doc] -> Doc
dApp hd = DApp (fromString hd)

-- | An atomic bare name (no arguments), e.g. @Flying@ or a quoted string.
dName :: String -> Doc
dName s = DApp (fromString s) []

-- | @head \@ty arg1 ...@ keeping @ty@ as a matchable child 'Doc'. See 'DTypeApp'.
dTypeApp :: String -> Doc -> [Doc] -> Doc
dTypeApp hd ty = DTypeApp hd [ty]

-- | Like 'dTypeApp' but with several @\@ty@ type arguments, e.g.
-- @masked \@ot \@'ZGraveyard@.
dTypeApps :: String -> [Doc] -> [Doc] -> Doc
dTypeApps = DTypeApp

-- | The @\@zone@ type application(s) to append after a @masked@/@maskeds@
-- @\@ot@. Empty for the zones the cards leave implicit -- battlefield and stack,
-- which are inferred -- otherwise the explicit @\@'ZGraveyard@ / @\@'ZExile@ /
-- etc. that the source spells out for off-battlefield targets.
zoneTypeArgs :: forall zone. (IsZone zone) => [Doc]
zoneTypeArgs = case litZone @zone of
  ZBattlefield -> []
  ZStack -> []
  z@ZExile -> [zoneAtom z]
  z@ZGraveyard -> [zoneAtom z]
  z@ZHand -> [zoneAtom z]
  z@ZLibrary -> [zoneAtom z]
 where
  zoneAtom z = DString ('\'' : show z)

-- | @\\<name> -> body@ where @name@ is an already-rendered doc (e.g. an object).
dLam :: Doc -> Doc -> Doc
dLam binder = DLam ("\\" <> binder)

-- | @(name :: ty)@: a type-annotated lambda binder. Used by (F) below
-- 'HighDataCombinators' to pin the existential mask\/zone types on the raw
-- constructors' lambdas -- the job the @\@ty@ applications do on the
-- combinator forms.
dAnnBinder :: Doc -> Doc -> Doc
dAnnBinder nm ty = "(" <> nm <> " :: " <> ty <> ")"

dNewline :: Int -> Doc
dNewline n = fromString $ "\n" <> replicate n ' '

-- | Walk a 'Doc' to a flat 'Doc' (atoms and 'DSeq' only) with all parens, @$@,
-- block arguments, and multiline formatting placed. This is the only phase that
-- makes those surface-syntax decisions; each is derived from the input 'Doc'
-- shape (see 'needsParens'). @ind@ is the column to indent continuation lines
-- to.
layout :: ShowOptions -> Int -> Doc -> Doc
layout opts ind = \case
  d@DString{} -> d
  d@DObject{} -> d
  d@DObjectPart{} -> d
  d@DVariable{} -> d
  d@DSeq{} -> d
  DList kids
    | showOptions_multiline opts && any (isMultilineChild opts ind) kids ->
        layoutList opts ind kids
    | otherwise ->
        "[" <> dintercalate ", " (map (layout opts ind) kids) <> "]"
  DApp hd [] -> layout opts ind hd
  DApp hd kids -> hd <> layoutArgs opts ind kids
  DTypeApp hd tys kids ->
    let hdItems =
          fromString hd
            <> mconcat [" @" <> parenIf (needsParens ty) (layout opts ind ty) | ty <- tys]
     in case kids of
          [] -> hdItems
          _ -> hdItems <> layoutArgs opts ind kids
  DLam binder body -> layoutLam opts ind binder body
  DRec hd fields
    | showOptions_multiline opts -> layoutRec opts ind hd fields
    | otherwise -> hd <> layoutArgs opts ind (map snd fields)

layoutArgs :: ShowOptions -> Int -> [Doc] -> Doc
layoutArgs opts ind kids =
  mconcat (map (layoutArgParen opts ind) (init kids)) <> layoutArgLast opts ind (last kids)

layoutArgParen :: ShowOptions -> Int -> Doc -> Doc
layoutArgParen opts ind k = " " <> parenIf (needsParens k) (layout opts ind k)

layoutArgLast :: ShowOptions -> Int -> Doc -> Doc
layoutArgLast opts ind k
  | not (needsParens k) = " " <> laid
  | showOptions_blockArguments opts && isBlockDoc opts k = " " <> laid
  | otherwise = " $ " <> laid
 where
  laid = layout opts ind k

-- | Docs that can be a trailing block argument (drop @$@ under (C)).
isBlockDoc :: ShowOptions -> Doc -> Bool
isBlockDoc _ DLam{} = True
isBlockDoc opts DRec{} = showOptions_multiline opts
isBlockDoc _ _ = False

layoutLam :: ShowOptions -> Int -> Doc -> Doc -> Doc
layoutLam opts ind binder body
  | showOptions_multiline opts =
      binder <> " ->" <> dNewline ind' <> layout opts ind' body
  | otherwise = binder <> " -> " <> layout opts ind body
 where
  ind' = ind + 2

layoutRec :: ShowOptions -> Int -> Doc -> [(String, Doc)] -> Doc
layoutRec opts ind hd fields =
  hd <> mconcat (zipWith field [0 :: Int ..] fields) <> dNewline ind2 <> "}"
 where
  ind2 = ind + 2
  field i (name, val) =
    dNewline ind2
      <> (if i == 0 then "{ " else ", ")
      <> fromString (name <> " = ")
      <> layout opts (ind2 + 2) val

-- | Multiline list (D): each element on its own leading-comma line, mirroring
-- the record layout. The whole list drops onto its own set of lines so a value
-- like @land_abilities =@ is followed by an aligned @[ ... , ... ]@ block. The
-- leading newline leaves a trailing space on the preceding @=@ line, stripped
-- by 'stripTrailingSpaces' in the final pass.
layoutList :: ShowOptions -> Int -> [Doc] -> Doc
layoutList opts ind kids =
  mconcat (zipWith element [0 :: Int ..] kids) <> dNewline ind <> "]"
 where
  element i k =
    dNewline ind
      <> (if i == 0 then "[ " else ", ")
      <> layout opts (ind + 2) k

-- | Whether a list element renders across multiple lines, in which case the
-- whole list switches to the multiline 'layoutList' form.
isMultilineChild :: ShowOptions -> Int -> Doc -> Bool
isMultilineChild opts ind = docHasNewline . layout opts (ind + 2)

docHasNewline :: Doc -> Bool
docHasNewline = \case
  DString s -> '\n' `elem` s
  DObject{} -> False
  DObjectPart{} -> False
  DVariable{} -> False
  DSeq ds -> any docHasNewline ds
  DApp hd kids -> docHasNewline hd || any docHasNewline kids
  DLam binder body -> docHasNewline binder || docHasNewline body
  DRec hd fields -> docHasNewline hd || any (docHasNewline . snd) fields
  DList kids -> any docHasNewline kids
  DTypeApp _ tys kids -> any docHasNewline tys || any docHasNewline kids

-- | Drop spaces that immediately precede a newline. The multiline list/record
-- layouts can leave a trailing space on the line before a broken-out block;
-- trailing whitespace is never meaningful in the rendered output.
stripTrailingSpaces :: String -> String
stripTrailingSpaces = foldr step ""
 where
  step ' ' acc@('\n' : _) = acc
  step c acc = c : acc

----------------------------------------
-- Smart aliasing (E): a self-contained `Doc -> Doc` pass, working entirely in
-- `Doc` space.
--
-- Independent of any particular renderer: it walks a finished 'Doc' and, at
-- every sub-'Doc', replaces it with the 'aliasMap' value keyed by that exact
-- sub-'Doc'. The map is built once by rendering each imported alias two ways --
-- its verbose form (the key 'Doc') and its shorthand name (the value 'Doc') --
-- so a card's @masked \@(OTN \'[...])@ type atom, which 'dTypeApp' keeps as its
-- own child 'Doc', matches the key and is swapped for @OTNPermanent@. Because
-- the value 'Doc' is a space-free 'DString', 'layout' emits it without the
-- wrapping parens automatically -- no string surgery.
--
-- 'runEnvMWith' runs the pass over the whole 'Doc' when
-- 'showOptions_smartAliasing' is set. Replacement is iterated to a fixed point
-- (bounded by 'maxAliasIterations') so chained aliases compose.

-- | Rewrite a 'Doc' by replacing matching sub-'Doc's until a fixed point.
aliasDoc :: Doc -> Doc
aliasDoc = go maxAliasIterations
 where
  go n d
    | d' == d = d
    | n <= 0 = error "logic error: cyclic aliasMap"
    | otherwise = go (n - 1) d'
   where
    d' = rewriteDoc d

-- | One bottom-up rewrite pass: recurse into children, then look the (rewritten)
-- node up in 'aliasMap'.
rewriteDoc :: Doc -> Doc
rewriteDoc d = fromMaybe descended (lookup descended aliasMap)
 where
  descended = case d of
    DString{} -> d
    DObject{} -> d
    DObjectPart{} -> d
    DVariable{} -> d
    DSeq ds -> DSeq (map rewriteDoc ds)
    DApp hd kids -> DApp hd (map rewriteDoc kids)
    DLam binder body -> DLam binder (rewriteDoc body)
    DRec hd fields -> DRec hd (map (fmap rewriteDoc) fields)
    DList kids -> DList (map rewriteDoc kids)
    DTypeApp hd tys kids -> DTypeApp hd (map rewriteDoc tys) (map rewriteDoc kids)

-- | The @(keyDoc, valueDoc)@ replacements: each alias's verbose type atom mapped
-- to its shorthand-name atom. Aliases that 'prettyType' already renders in
-- shorthand (the two/three-tag ones with a dedicated 'PrettyType' branch) lack
-- the @OTN \'[@ prefix, never appear verbose, and are dropped here.
aliasMap :: [(Doc, Doc)]
aliasMap =
  [ (typeAtom pretty, typeAtom name)
  | (name, pretty) <- aliasSpecs
  , "OTN '[" `List.isPrefixOf` pretty
  ]

-- | The 'Doc' a type atom renders to for a given 'prettyType' string -- the same
-- 'DString' 'showTypeOf' produces. Parenthesization of @\@(ty)@ is decided later
-- by 'layout' (via 'needsParens', i.e. iff the token contains a space).
typeAtom :: String -> Doc
typeAtom = DString

-- | Every alias we can collapse, as @(OTN-name, prettyType\@OTN-alias)@. The
-- second element is the alias rendered with smart-aliasing off and every other
-- option on; type rendering is option-independent, so 'prettyType' (the same
-- primitive 'showTypeOf' uses) is that rendering.
aliasSpecs :: [(String, String)]
aliasSpecs =
  [ ("OTNPermanent", prettyType @OTNPermanent)
  , ("OTNSpell", prettyType @OTNSpell)
  , ("OTNCard", prettyType @OTNCard)
  , ("OTNDamageSource", prettyType @OTNDamageSource)
  , ("OTNNonCreature", prettyType @OTNNonCreature)
  , ("OTNNonArtifactPermanent", prettyType @OTNNonArtifactPermanent)
  , ("OTNNonCreaturePermanent", prettyType @OTNNonCreaturePermanent)
  , ("OTNNonEnchantmentPermanent", prettyType @OTNNonEnchantmentPermanent)
  , ("OTNNonLandPermanent", prettyType @OTNNonLandPermanent)
  , ("OTNNonPlaneswalkerPermanent", prettyType @OTNNonPlaneswalkerPermanent)
  , ("OTNAbility", prettyType @OTNAbility)
  , ("OTNActivatedOrTriggeredAbility", prettyType @OTNActivatedOrTriggeredAbility)
  , ("OTNArtifactLand", prettyType @OTNArtifactLand)
  , -- Single-tag @OTN '[ 'OTFoo]@ wrappers collapse to their @OTNFoo@ shorthand.
    ("OTNActivatedAbility", prettyType @OTNActivatedAbility)
  , ("OTNArtifact", prettyType @OTNArtifact)
  , ("OTNBattle", prettyType @OTNBattle)
  , ("OTNCreature", prettyType @OTNCreature)
  , ("OTNEmblem", prettyType @OTNEmblem)
  , ("OTNEnchantment", prettyType @OTNEnchantment)
  , ("OTNInstant", prettyType @OTNInstant)
  , ("OTNLand", prettyType @OTNLand)
  , ("OTNPlaneswalker", prettyType @OTNPlaneswalker)
  , ("OTNPlayer", prettyType @OTNPlayer)
  , ("OTNSorcery", prettyType @OTNSorcery)
  , ("OTNStaticAbility", prettyType @OTNStaticAbility)
  , ("OTNTriggeredAbility", prettyType @OTNTriggeredAbility)
  ]

-- | Replacement recursion bound. Direct atom swaps reach a fixed point in one
-- pass; exceeding this signals a malformed (cyclic) 'aliasMap'.
maxAliasIterations :: Int
maxAliasIterations = 32

----------------------------------------

type CardDepth = Maybe Int

type Generation = Int

-- | (F) How much of the card-authoring combinator vocabulary the renderer may
-- use when spelling out data, from constructors-only up to everything the
-- hand-written cards use. See 'showOptions_dataCombinators'.
data DataCombinators :: Type where
  -- | Constructors only. @toColors@\/@manaCost@\/@toManaCost@\/@toManaPool@
  -- render as their record constructors (no @mempty@ shortcuts), and even the
  -- object coercions are spelled with constructors: a binder whose object some
  -- use-site views at a wider type gains a @name\@(ZO sng\<n\> objN\<n\>)@
  -- as-pattern, and the coerced use rebuilds the wider @ObjectN@ from the
  -- bound payloads with the @ON\<k\>\<letter\>@ nesting constructors, e.g.
  -- @ZO sng3 (ON9a (... (ON2a objN3)))@ -- no @toZO\<n\>@\/@asFoo@ functions.
  NoDataCombinators :: DataCombinators
  -- | Raw structural constructors, but the highly-compacting leaf helpers
  -- stay: @toColors@, @manaCost@\/@toManaCost@, @toManaPool@, and the
  -- @asFoo@\/@toZO\<n\>@ object coercions render as in 'HighDataCombinators'.
  LowDataCombinators :: DataCombinators
  -- | The full authoring vocabulary, as the hand-written cards use it.
  HighDataCombinators :: DataCombinators
  deriving (Eq, Ord, Show, Typeable)

-- | Knobs controlling how a value is rendered. 'defaultShowOptions' (used by
-- the 'Show' instances) enables every cleanup option -- the fully-cleaned,
-- multiline, block-argument, wildcarded rendering -- and leaves (F)
-- 'showOptions_dataCombinators' at 'HighDataCombinators'.
data ShowOptions = ShowOptions
  { showOptions_wildcardUnusedVars :: Bool
  -- ^ (B) Render unused bindings as @_@. Wildcarded bindings do not consume
  -- a remap slot (object/variable ids are always remapped so the first
  -- occurrence of each is numbered @1,2,3,...@ / @x,y,z,...@ in left-DFS order,
  -- with no gaps).
  , showOptions_blockArguments :: Bool
  -- ^ (C) Omit the superfluous @$@ before a trailing lambda/record argument.
  , showOptions_multiline :: Bool
  -- ^ (D) Break record fields and lambda bodies onto indented lines.
  , showOptions_smartAliasing :: Bool
  -- ^ (E) Collapse the verbose type atoms in @\@Ty@ applications to the
  -- shorthand @OTN@ aliases they stand for, e.g.
  -- @masked \@(OTN \'[\'OTArtifact, \'OTBattle, \'OTCreature, \'OTEnchantment, \'OTLand, \'OTPlaneswalker])@
  -- becomes @masked \@OTNPermanent@. A standalone 'Doc'-space pass; see
  -- 'aliasDoc' / 'aliasMap'.
  , showOptions_dataCombinators :: DataCombinators
  -- ^ (F) Below 'HighDataCombinators', build data with raw constructors
  -- instead of the authoring combinators:
  -- @masked@\/@maskeds@\/@linked@\/@thisObject@ and the ability combinators
  -- (@activated@\/@static@\/@triggered@) render as their
  -- @MaskedN@\/@MaskedsN@\/@LinkedN@\/@ThisN@\/@SomeZone@+@WithThis*@
  -- constructors, with the existential mask\/zone types pinned by @::@
  -- annotations on the lambda binders instead of @\@ty@ applications (types may
  -- still use aliases). 'NoDataCombinators' additionally drops the compacting
  -- leaf\/coercion helpers ('LowDataCombinators' keeps them); see
  -- 'DataCombinators'. Below 'HighDataCombinators',
  -- 'showOptions_smartAliasing' is forced off via 'massageOptions' (and
  -- 'NoDataCombinators' also forces 'showOptions_wildcardUnusedVars' off: a
  -- wildcarded binder cannot carry the @\@(ZO ...)@ as-pattern -- @_\@pat@ is
  -- not valid Haskell).
  }

defaultShowOptions :: ShowOptions
defaultShowOptions =
  ShowOptions
    { showOptions_wildcardUnusedVars = True
    , showOptions_blockArguments = True
    , showOptions_multiline = True
    , showOptions_smartAliasing = True
    , showOptions_dataCombinators = HighDataCombinators
    }

-- | Normalize a 'ShowOptions' to the combination actually rendered: below
-- 'HighDataCombinators', (F) forces 'showOptions_smartAliasing' off (the alias
-- pass exists to shorten the @\@ty@ atoms the combinator forms emit; raw
-- constructors keep their types verbatim), and 'NoDataCombinators' forces
-- 'showOptions_wildcardUnusedVars' off (its destructured binders cannot render
-- as @_@). Applied by 'runEnvMWith', so every render sees the massaged options.
massageOptions :: ShowOptions -> ShowOptions
massageOptions opts = case showOptions_dataCombinators opts of
  NoDataCombinators ->
    opts{showOptions_smartAliasing = False, showOptions_wildcardUnusedVars = False}
  LowDataCombinators -> opts{showOptions_smartAliasing = False}
  HighDataCombinators -> opts

data Env = Env
  { nextObjectId :: ObjectId
  , nextVariableId :: VariableId
  , originalObjectRep :: Map.Map ObjectId TypeRep
  , currentGeneration :: Generation
  , objectGenerations :: Map.Map ObjectId Generation
  , objectNames :: Map.Map ObjectId String
  , cardDepth :: CardDepth
  , showOptions :: ShowOptions
  }

mkEnv :: ShowOptions -> CardDepth -> Env
mkEnv opts depth =
  Env
    { nextObjectId = ObjectId 1
    , nextVariableId = VariableId 0
    , originalObjectRep = mempty
    , currentGeneration = 0
    , objectGenerations = mempty
    , objectNames = mempty
    , cardDepth = max 0 <$> depth
    , showOptions = opts
    }

newtype EnvM a = EnvM {unEnvM :: State.State Env a}
  deriving (Functor)

instance Applicative EnvM where
  pure :: a -> EnvM a
  pure = EnvM . pure

  (<*>) :: EnvM (a -> b) -> EnvM a -> EnvM b
  EnvM f <*> EnvM a = EnvM $ f <*> a

instance Monad EnvM where
  (>>=) :: EnvM a -> (a -> EnvM b) -> EnvM b
  EnvM a >>= f = EnvM $ a >>= unEnvM . f

-- | Render a built `Doc` to a 'String' at the given depth.
runEnvM :: CardDepth -> EnvM Doc -> String
runEnvM = runEnvMWith defaultShowOptions

runEnvMWith :: ShowOptions -> CardDepth -> EnvM Doc -> String
runEnvMWith (massageOptions -> opts) depth docM =
  stripTrailingSpaces $ concat $ State.evalState (unEnvM atomStrsM) $ mkEnv opts depth
 where
  aliasPass = if showOptions_smartAliasing opts then aliasDoc else id
  atomStrsM :: EnvM [String]
  atomStrsM = do
    atoms <- flattenDoc . layout opts 0 . aliasPass <$> docM
    let used = getUsed atoms
        objRemap = mkObjRemap opts used atoms
        varRemap = mkVarRemap opts used atoms
    mapM (showAtom used objRemap varRemap) atoms
  -- The used/unused treatment shared by object and variable atoms: an unused
  -- binding renders as @_@ under (B), or keeps its name behind an @_@ prefix
  -- otherwise.
  nameOrWildcard :: Bool -> String -> String
  nameOrWildcard isUsed name
    | isUsed = name
    | showOptions_wildcardUnusedVars opts = "_"
    | otherwise = '_' : name
  showAtom :: Used -> Map.Map ObjectId Int -> Map.Map VariableId Int -> Doc -> EnvM String
  showAtom used objRemap varRemap = \case
    DString s -> pure s
    DObject i@(ObjectId n) g -> do
      prefix <- getObjectNamePrefix i
      let name = prefix ++ show (Map.findWithDefault n i objRemap)
      pure $ nameOrWildcard (Map.findWithDefault False (i, g) (usedObjects used)) name
    DObjectPart part i@(ObjectId n) g -> do
      let num = show (Map.findWithDefault n i objRemap)
      pure case part of
        ObjectPartPattern -> case Map.findWithDefault False (i, g) (coercedObjects used) of
          True -> "@(ZO sng" ++ num ++ " objN" ++ num ++ ")"
          False -> ""
        ObjectPartSing -> "sng" ++ num
        ObjectPartPayload -> "objN" ++ num
    DVariable vid@(VariableId i) -> do
      let name = varNames !! Map.findWithDefault i vid varRemap
      pure $ nameOrWildcard (Map.findWithDefault False vid (usedVariables used)) name
    _ -> error "logic error: flattenDoc emitted a non-atom"

-- | Flatten a laid-out 'Doc' to its stream of atoms
-- ('DString'/'DObject'/'DObjectPart'/'DVariable').
flattenDoc :: Doc -> [Doc]
flattenDoc = \case
  d@DString{} -> [d]
  d@DObject{} -> [d]
  d@DObjectPart{} -> [d]
  d@DVariable{} -> [d]
  DSeq ds -> concatMap flattenDoc ds
  DApp hd kids -> flattenDoc hd <> concatMap flattenDoc kids
  DLam binder body -> flattenDoc binder <> flattenDoc body
  DRec hd fields -> flattenDoc hd <> concatMap (flattenDoc . snd) fields
  DList kids -> concatMap flattenDoc kids
  DTypeApp hd tys kids -> DString hd : concatMap flattenDoc tys <> concatMap flattenDoc kids

-- | Remap object ids to @1,2,3,...@ by first-occurrence (left-DFS) order,
-- skipping objects that render as wildcards (B) so the remaining ids stay
-- gapless.
mkObjRemap :: ShowOptions -> Used -> [Doc] -> Map.Map ObjectId Int
mkObjRemap opts used items = Map.fromList $ zip kept [1 ..]
 where
  kept = List.nub [i | DObject i g <- items, not (skip i g)]
  skip i g =
    showOptions_wildcardUnusedVars opts
      && not (Map.findWithDefault False (i, g) (usedObjects used))

-- | Remap variable ids to @0,1,2,...@ (indices into 'varNames') by
-- first-occurrence order, skipping variables that render as wildcards (B).
mkVarRemap :: ShowOptions -> Used -> [Doc] -> Map.Map VariableId Int
mkVarRemap opts used items = Map.fromList $ zip kept [0 ..]
 where
  kept = List.nub [vid | DVariable vid <- items, not (skip vid)]
  skip vid =
    showOptions_wildcardUnusedVars opts
      && not (Map.findWithDefault False vid (usedVariables used))

type UsedObjects = Map.Map (ObjectId, Generation) Bool

type UsedVariables = Map.Map VariableId Bool

data Used = Used
  { usedObjects :: UsedObjects
  , usedVariables :: UsedVariables
  , coercedObjects :: Map.Map (ObjectId, Generation) Bool
  -- ^ Objects some use-site rebuilds from a binder's destructured payloads
  -- under (F) 'NoDataCombinators'; their binders render the
  -- @\@(ZO sng\<n\> objN\<n\>)@ as-pattern (see 'ObjectPartPattern').
  }

getUsed :: [Doc] -> Used
getUsed = flip foldr empty \item used -> case item of
  DObject i g ->
    used
      { usedObjects =
          Map.insertWith (\_ _ -> True) (i, g) False $
            usedObjects used
      }
  DObjectPart part i g -> case part of
    ObjectPartPattern -> used
    _ -> used{coercedObjects = Map.insert (i, g) True $ coercedObjects used}
  DVariable var ->
    used
      { usedVariables =
          Map.insertWith (\_ _ -> True) var False $
            usedVariables used
      }
  _ -> used
 where
  empty = Used mempty mempty mempty

-- TODO: Make this better now that variables can be procured through various abstract means.
varNames :: [String]
varNames = "x" : "y" : "z" : map f [0 ..]
 where
  f :: Int -> String
  f n = "var" ++ show n

getVarName :: Variable a -> Doc
getVarName = DVariable . getVariableId

getObjectName :: Object a -> EnvM Doc
getObjectName (Object _ (UntypedObject _ i)) = EnvM do
  gens <- State.gets objectGenerations
  case Map.lookup i gens of
    Nothing -> pure $ DObject i (-1) -- Object is an unbound variable. Can happen when walking past a variable binding before showing the rest of the tree.
    Just g -> pure $ DObject i g

-- | Register an object that already exists in a `Tree` binder (rather than
-- minting a fresh one via 'newObjectN'): assign it a fresh generation and its
-- lambda-parameter name so later uses resolve correctly. Mirrors the state
-- effects of 'newObjectN' (whose fresh id we do not need -- the `Tree` carries
-- the id already, numbered to match 'newObjectN' by starting at 1).
registerTreeObject ::
  forall zone ot. (Typeable (ObjectN ot)) => String -> ZO zone ot -> EnvM ()
registerTreeObject name (ZO _ objN) = EnvM $ State.modify' \st ->
  let i = getObjectId objN
   in st
        { originalObjectRep = Map.insert i (typeOf objN) $ originalObjectRep st
        , currentGeneration = currentGeneration st + 1
        , objectGenerations = Map.insert i (currentGeneration st) $ objectGenerations st
        , objectNames = Map.insert i name $ objectNames st
        }

treeConfigForDepth :: CardDepth -> TreeConfig
treeConfigForDepth depth =
  TreeConfig{treeConfig_ = (), treeConfig_maxCardDepth = depth}

pluralize :: EnvM Doc -> EnvM Doc
pluralize = fmap (appendToken "s")

-- | Append a literal suffix to the last atom of a 'Doc', preserving structure
-- (so a coerced object like @asPermanent obj@ pluralizes to @asPermanent objs@,
-- not a broken sequence).
appendToken :: String -> Doc -> Doc
appendToken t = \case
  DString s -> DString (s <> t)
  DObject i g -> DSeq [DObject i g, DString t]
  DVariable v -> DSeq [DVariable v, DString t]
  DSeq ds | not (null ds) -> DSeq (init ds <> [appendToken t (last ds)])
  DApp hd kids
    | null kids -> DApp (appendToken t hd) []
    | otherwise -> DApp hd (init kids <> [appendToken t (last kids)])
  d -> d <> DString t

lenseList :: List x -> x
lenseList = \case
  List [x] -> x
  _ -> error "logic error: should not happen by construction"

getObjectNamePrefix :: ObjectId -> EnvM String
getObjectNamePrefix i = EnvM do
  State.gets (Map.findWithDefault "unboundVariable" i . objectNames)

-- | The (F) 'showOptions_dataCombinators' level in effect for this render.
getDataCombinators :: EnvM DataCombinators
getDataCombinators = EnvM do
  State.gets (showOptions_dataCombinators . showOptions)

-- | The @ON\<k\>\<letter\>@ constructor chain (outermost first) that widens an
-- @ObjectN@ from the source type to the target type, or 'Nothing' when no such
-- chain exists (e.g. an @OT0@ source, or an unrelated pair). Both arguments are
-- @ObjectN otn@ 'TypeRep's. Each @ON\<k\>\<letter\>@ nesting constructor embeds
-- the @ObjectN@ that omits slot @\<letter\>@ of its @k@ object types, so the
-- chain is found by deleting, one at a time, the target slots the source lacks.
objectNChain :: TypeRep -> TypeRep -> Maybe [String]
objectNChain sourceRep targetRep = case (objNSlots sourceRep, objNSlots targetRep) of
  (Just source, Just target)
    | not (null source)
    , length source < length target ->
        go source target
  _ -> Nothing
 where
  go :: [TypeRep] -> [TypeRep] -> Maybe [String]
  go source current
    | length current == length source = case current == source of
        True -> Just []
        False -> Nothing
    | otherwise = case List.findIndex (`notElem` source) current of
        Nothing -> Nothing
        Just idx ->
          let consName = "ON" ++ show (length current) ++ [['a' ..] !! idx]
              rest = take idx current ++ drop (idx + 1) current
           in (consName :) <$> go source rest

-- | The promoted @[OT]@ slot list of an @ObjectN otn@ 'TypeRep', or 'Nothing'
-- if the rep does not decompose as expected. Tolerates the kind argument that
-- 'typeRepArgs' may or may not surface for the poly-kinded @OTN@ and the
-- promoted list constructors.
objNSlots :: TypeRep -> Maybe [TypeRep]
objNSlots rep = case typeRepArgs rep of
  [otn] -> Just $ listSlots $ otkOf otn
  _ -> Nothing
 where
  otkOf otn = case typeRepArgs otn of
    [otk] -> otk
    [_kind, otk] -> otk
    _ -> otn
  listSlots :: TypeRep -> [TypeRep]
  listSlots r = case typeRepArgs r of
    [_kind, hd, tl] -> hd : listSlots tl
    [hd, tl] -> hd : listSlots tl
    _ -> []

showListM :: (a -> EnvM Doc) -> [a] -> EnvM Doc
showListM f xs = DList <$> mapM f xs

----------------------------------------

showAnyCard :: AnyCard -> EnvM Doc
showAnyCard = showViaTree showTreeAnyCard

showAnyToken :: AnyToken -> EnvM Doc
showAnyToken = showViaTree showTreeAnyToken

showArtifactType :: ArtifactType -> EnvM Doc
showArtifactType = pure . DString . show

showArtifactTypes :: [ArtifactType] -> EnvM Doc
showArtifactTypes = showListM showArtifactType

showBasicLandType :: BasicLandType -> EnvM Doc
showBasicLandType = pure . DString . show

showBattleType :: BattleType -> EnvM Doc
showBattleType = \case
  Seige -> pure $ DString "Seige"

showBattleTypes :: [BattleType] -> EnvM Doc
showBattleTypes = showListM showBattleType

showCard :: Card ot -> EnvM Doc
showCard = showViaTree showTreeCard

showCardImpl :: (HasCardName name) => String -> name -> EnvM Doc -> EnvM Doc
showCardImpl consName (getCardName -> CardName name) cont = do
  depth <- EnvM $ State.gets cardDepth
  EnvM $ State.modify' \st -> st{cardDepth = subtract 1 <$> depth}
  case depth of
    Just 0 -> pure $ dName $ consName <> " " <> show name <> " ..."
    _ -> cont

showColor :: Color -> EnvM Doc
showColor = pure . DString . show

class ShowColors colors where
  showColors :: colors -> EnvM Doc

instance ShowColors [Color] where
  showColors :: [Color] -> EnvM Doc
  showColors = showListM showColor

instance ShowColors Colors where
  showColors :: Colors -> EnvM Doc
  showColors colors = do
    combinators <- getDataCombinators
    pure case combinators of
      NoDataCombinators -> dApp "Colors" [slot w, slot u, slot b, slot r, slot g]
      LowDataCombinators -> DApp "toColors" [DString arg]
      HighDataCombinators -> DApp "toColors" [DString arg]
   where
    Colors w u b r g = colors
    slot :: (Show sym) => Maybe sym -> Doc
    slot = \case
      Nothing -> DString "Nothing"
      Just sym -> DString $ "Just " <> show sym
    syms =
      List.intercalate "," $
        catMaybes
          [show <$> w, show <$> u, show <$> b, show <$> r, show <$> g]
    arg = case syms of
      [_] -> syms
      _ -> "(" <> syms <> ")"

showCompleteManaPool :: CompleteManaPool -> EnvM Doc
showCompleteManaPool complete = do
  sSnow <- showManaPool snow
  sNonSnow <- showManaPool nonSnow
  pure $ DApp "CompleteManaPool" [sSnow, sNonSnow]
 where
  CompleteManaPool
    { poolSnow = snow
    , poolNonSnow = nonSnow
    } = complete

showCreatureType :: CreatureType -> EnvM Doc
showCreatureType = pure . DString . show

showCreatureTypes :: [CreatureType] -> EnvM Doc
showCreatureTypes = showListM showCreatureType

showDamage :: Damage var -> EnvM Doc
showDamage = \case
  Damage n -> pure $ DString $ "Damage " ++ show n
  VariableDamage var -> pure $ DApp "VariableDamage" [getVarName var]

showDefense :: Defense -> EnvM Doc
showDefense = pure . DString . show

showLandType :: LandType -> EnvM Doc
showLandType landType = case landType of
  BasicLand basic -> do
    sBasic <- showBasicLandType basic
    pure $ DApp "BasicLand" [sBasic]
  Desert -> sLandType
  Gate -> sLandType
  Lair -> sLandType
  Locus -> sLandType
  Mine -> sLandType
  PowerPlant -> sLandType
  Tower -> sLandType
  Urzas -> sLandType
 where
  sLandType = pure $ DString $ show landType

showLandTypes :: [LandType] -> EnvM Doc
showLandTypes = showListM showLandType

showLoyalty :: Loyalty -> EnvM Doc
showLoyalty = pure . DString . show

showMana :: Mana var snow a -> EnvM Doc
showMana = \case
  Mana x -> pure $ DString $ show x
  VariableMana var -> pure $ DApp "VariableMana" [getVarName var]
  SumMana x y -> do
    sX <- showMana x
    sY <- showMana y
    pure $ DApp "SumMana" [sX, sY]

-- | Like 'showMana', but for (F) 'NoDataCombinators': a literal
-- renders as the explicit @Mana@ constructor instead of a bare numeral (which
-- would need the @Num@ instance to type-check).
showManaRaw :: Mana var snow a -> EnvM Doc
showManaRaw = \case
  Mana x -> pure $ DString $ "Mana " <> show x
  VariableMana var -> pure $ DApp "VariableMana" [getVarName var]
  SumMana x y -> do
    sX <- showManaRaw x
    sY <- showManaRaw y
    pure $ DApp "SumMana" [sX, sY]

-- | Raw-constructor form of a 'PhyrexianManaCost' for (F)
-- 'NoDataCombinators': the full record, no @mempty@ shortcut.
showPhyrexianManaCostRaw :: PhyrexianManaCost var -> EnvM Doc
showPhyrexianManaCostRaw cost =
  recD
    "PhyrexianManaCost"
    [ ("phyrexianW", showManaRaw w)
    , ("phyrexianU", showManaRaw u)
    , ("phyrexianB", showManaRaw b)
    , ("phyrexianR", showManaRaw r)
    , ("phyrexianG", showManaRaw g)
    , ("phyrexianC", showManaRaw c)
    ]
 where
  PhyrexianManaCost
    { phyrexianW = w
    , phyrexianU = u
    , phyrexianB = b
    , phyrexianR = r
    , phyrexianG = g
    , phyrexianC = c
    } = cost

showHybridManaCostRaw :: HybridManaCost var -> EnvM Doc
showHybridManaCostRaw cost =
  recD
    "HybridManaCost"
    [ ("hybridWU", showManaRaw wu)
    , ("hybridUB", showManaRaw ub)
    , ("hybridBR", showManaRaw br)
    , ("hybridRG", showManaRaw rg)
    , ("hybridGW", showManaRaw gw)
    , ("hybridWB", showManaRaw wb)
    , ("hybridUR", showManaRaw ur)
    , ("hybridBG", showManaRaw bg)
    , ("hybridRW", showManaRaw rw)
    , ("hybridGU", showManaRaw gu)
    , ("hybridW2", showManaRaw w2)
    , ("hybridU2", showManaRaw u2)
    , ("hybridB2", showManaRaw b2)
    , ("hybridR2", showManaRaw r2)
    , ("hybridG2", showManaRaw g2)
    , ("hybridC2", showManaRaw c2)
    ]
 where
  HybridManaCost
    { hybridWU = wu
    , hybridUB = ub
    , hybridBR = br
    , hybridRG = rg
    , hybridGW = gw
    , hybridWB = wb
    , hybridUR = ur
    , hybridBG = bg
    , hybridRW = rw
    , hybridGU = gu
    , hybridW2 = w2
    , hybridU2 = u2
    , hybridB2 = b2
    , hybridR2 = r2
    , hybridG2 = g2
    , hybridC2 = c2
    } = cost

showDynamicManaCostRaw :: DynamicManaCost var -> EnvM Doc
showDynamicManaCostRaw cost =
  recD
    "DynamicManaCost"
    [ ("costGeneric", showManaRaw x)
    , ("costSnow", showManaRaw s)
    , ("costHybrid", showHybridManaCostRaw hy)
    , ("costPhyrexian", showPhyrexianManaCostRaw phy)
    ]
 where
  DynamicManaCost
    { costGeneric = x
    , costSnow = s
    , costHybrid = hy
    , costPhyrexian = phy
    } = cost

-- | Raw-constructor form of a 'ManaCost' for (F)
-- 'NoDataCombinators': the @ManaCost'@ record replaces the
-- @manaCost@\/@toManaCost@ combinators.
showManaCostRaw :: ManaCost var -> EnvM Doc
showManaCostRaw cost =
  recD
    "ManaCost'"
    [ ("costW", showManaRaw w)
    , ("costU", showManaRaw u)
    , ("costB", showManaRaw b)
    , ("costR", showManaRaw r)
    , ("costG", showManaRaw g)
    , ("costC", showManaRaw c)
    , ("costDynamic", showDynamicManaCostRaw dyn)
    ]
 where
  ManaCost'
    { costW = w
    , costU = u
    , costB = b
    , costR = r
    , costG = g
    , costC = c
    , costDynamic = dyn
    } = cost

-- | Raw-constructor form of a 'ManaPool' for (F)
-- 'NoDataCombinators': the record replaces the @toManaPool@
-- combinator.
showManaPoolRaw :: ManaPool snow -> EnvM Doc
showManaPoolRaw pool =
  recD
    "ManaPool"
    [ ("poolW", showManaRaw w)
    , ("poolU", showManaRaw u)
    , ("poolB", showManaRaw b)
    , ("poolR", showManaRaw r)
    , ("poolG", showManaRaw g)
    , ("poolC", showManaRaw c)
    ]
 where
  ManaPool
    { poolW = w
    , poolU = u
    , poolB = b
    , poolR = r
    , poolG = g
    , poolC = c
    } = pool

showPhyrexianManaCost :: PhyrexianManaCost var -> EnvM Doc
showPhyrexianManaCost cost = do
  combinators <- getDataCombinators
  let PhyrexianManaCost
        { phyrexianW = w
        , phyrexianU = u
        , phyrexianB = b
        , phyrexianR = r
        , phyrexianG = g
        , phyrexianC = c
        } = cost
  case (combinators, cost == mempty) of
    (NoDataCombinators, _) -> showPhyrexianManaCostRaw cost
    (_, True) -> pure $ DString "mempty"
    (_, False) -> do
      sW <- showMana w
      sU <- showMana u
      sB <- showMana b
      sR <- showMana r
      sG <- showMana g
      sC <- showMana c
      pure $ DApp "PhyrexianManaCost" [sW, sU, sB, sR, sG, sC]

showHybridManaCost :: HybridManaCost var -> EnvM Doc
showHybridManaCost cost = do
  combinators <- getDataCombinators
  let HybridManaCost
        { hybridBG = bg
        } = cost
  case (combinators, cost == mempty) of
    (NoDataCombinators, _) -> showHybridManaCostRaw cost
    (_, True) -> pure $ DString "mempty"
    (_, False) -> do
      sBG <- showMana bg
      pure $ DApp "HybridManaCost" [sBG]

showDynamicManaCost :: DynamicManaCost var -> EnvM Doc
showDynamicManaCost cost = do
  combinators <- getDataCombinators
  let DynamicManaCost
        { costSnow = s
        , costGeneric = x
        , costHybrid = hy
        , costPhyrexian = phy
        } = cost
  case (combinators, cost == mempty) of
    (NoDataCombinators, _) -> showDynamicManaCostRaw cost
    (_, True) -> pure $ DString "mempty"
    -- Always emit the raw @DynamicManaCost@ constructor: an only-generic
    -- shortcut to the bare @Mana@ (@showMana x@) does not type-check where a
    -- @DynamicManaCost@ is expected (e.g. the @costDynamic@ field of a rendered
    -- @ManaCost'@). Field order is @costGeneric@ then @costSnow@ (see the record).
    (_, False) -> do
      sX <- showMana x
      sS <- showMana s
      sHy <- showHybridManaCost hy
      sPhy <- showPhyrexianManaCost phy
      pure $ DApp "DynamicManaCost" [sX, sS, sHy, sPhy]

-- | A bare 'ManaCost' value renders as @toManaCost \<arg\>@ (the 'Show' instance
-- uses this). Card costs render via @manaCost \<arg\>@; see 'showTreeCost'.
-- Under (F) 'NoDataCombinators' both render the @ManaCost'@ record instead
-- ('showManaCostRaw').
showManaCost :: ManaCost var -> EnvM Doc
showManaCost cost = do
  combinators <- getDataCombinators
  case combinators of
    NoDataCombinators -> showManaCostRaw cost
    LowDataCombinators -> pure $ dApp "toManaCost" [showManaCostArg cost]
    HighDataCombinators -> pure $ dApp "toManaCost" [showManaCostArg cost]

-- | The argument to @manaCost@/@toManaCost@ for a cost: @0@, a lone token like
-- @G@, or a tuple like @(3, R)@ / @(VariableMana \@'NonSnow \@'Ty1 x, G, G)@.
-- Literal components render as repeated 'ManaSymbol's (or @(sym, n)@ for large
-- counts) and the generic slot as a bare count; a variable (non-literal)
-- component renders as the raw @VariableMana@ constructor with its slot's type
-- applications -- exactly how the cards are hand-written (see @manaCost@).
showManaCostArg :: ManaCost var -> Doc
showManaCostArg cost =
  case atoms of
    [] -> DString "0"
    [a] -> a
    _ -> "(" <> dintercalate ", " atoms <> ")"
 where
  ManaCost'
    { costW = w
    , costU = u
    , costB = b
    , costR = r
    , costG = g
    , costC = c
    , costDynamic = dyn
    } = cost
  DynamicManaCost
    { costGeneric = x
    , costSnow = s
    , costHybrid = hybrid
    , costPhyrexian = phyrexian
    } = dyn
  HybridManaCost
    { hybridWU = wu
    , hybridUB = ub
    , hybridBR = br
    , hybridRG = rg
    , hybridGW = gw
    , hybridWB = wb
    , hybridUR = ur
    , hybridBG = bg
    , hybridRW = rw
    , hybridGU = gu
    , hybridW2 = w2
    , hybridU2 = u2
    , hybridB2 = b2
    , hybridR2 = r2
    , hybridG2 = g2
    , hybridC2 = c2
    } = hybrid
  PhyrexianManaCost
    { phyrexianW = pw
    , phyrexianU = pu
    , phyrexianB = pb
    , phyrexianR = pr
    , phyrexianG = pg
    , phyrexianC = pc
    } = phyrexian
  atoms =
    genericAtoms x
      ++ symAtoms W w
      ++ symAtoms U u
      ++ symAtoms B b
      ++ symAtoms R r
      ++ symAtoms G g
      ++ symAtoms C c
      ++ symAtoms S s
      ++ symAtoms WU wu
      ++ symAtoms UB ub
      ++ symAtoms BR br
      ++ symAtoms RG rg
      ++ symAtoms GW gw
      ++ symAtoms WB wb
      ++ symAtoms UR ur
      ++ symAtoms BG bg
      ++ symAtoms RW rw
      ++ symAtoms GU gu
      ++ symAtoms W2 w2
      ++ symAtoms U2 u2
      ++ symAtoms B2 b2
      ++ symAtoms R2 r2
      ++ symAtoms G2 g2
      ++ symAtoms C2 c2
      ++ symAtoms PW pw
      ++ symAtoms PU pu
      ++ symAtoms PB pb
      ++ symAtoms PR pr
      ++ symAtoms PG pg
      ++ symAtoms PC pc
  -- The generic slot is a bare count (@3@), or -- when variable -- the raw
  -- @VariableMana \@'NonSnow \@'Ty1@ constructor.
  genericAtoms :: Mana v sn mt -> [Doc]
  genericAtoms = \case
    Mana 0 -> []
    Mana n -> [DString (show n)]
    VariableMana var -> [manaVar "'NonSnow" "'Ty1" var]
    SumMana m1 m2 -> genericAtoms m1 ++ genericAtoms m2
  -- A colored/snow/hybrid/phyrexian slot is repeated 'ManaSymbol's, or (in the
  -- unusual case of a variable in such a slot) the raw @VariableMana@ term.
  symAtoms :: ManaSymbol smt -> Mana v sn mt -> [Doc]
  symAtoms sym m = case tryLitMana m of
    Just 0 -> []
    Just n
      | n < 10 -> replicate n (DString (show sym))
      | otherwise -> [DString ("(" ++ show sym ++ ", " ++ show n ++ ")")]
    Nothing -> variableAtoms m
  variableAtoms :: Mana v sn mt -> [Doc]
  variableAtoms = \case
    Mana _ -> []
    VariableMana var -> [DApp (DString "VariableMana") [getVarName var]]
    SumMana m1 m2 -> variableAtoms m1 ++ variableAtoms m2
  -- Built as a 'DSeq' (not a 'DApp') so the @\@ty@ prefix and the variable atom
  -- keep their space once flattened: this atom sits inside the tuple 'DSeq',
  -- which 'layout' does not descend into. The trailing 'DVariable' still gets
  -- id-remapped.
  manaVar snow mt var =
    DString ("VariableMana @" ++ snow ++ " @" ++ mt ++ " ") <> getVarName var

showManaPool :: ManaPool snow -> EnvM Doc
showManaPool pool = do
  combinators <- getDataCombinators
  case combinators of
    NoDataCombinators -> showManaPoolRaw pool
    LowDataCombinators -> showManaPoolCombinator pool
    HighDataCombinators -> showManaPoolCombinator pool

-- | The @toManaPool \<arg\>@ rendering of a 'ManaPool' (all but (F)
-- 'NoDataCombinators').
showManaPoolCombinator :: ManaPool snow -> EnvM Doc
showManaPoolCombinator pool = pure $ DString $ "toManaPool " ++ sManas
 where
  ManaPool
    { poolW = w
    , poolU = u
    , poolB = b
    , poolR = r
    , poolG = g
    , poolC = c
    } = pool
  syms sym mana = case litMana mana of
    0 -> []
    n
      | n < 10 -> replicate n $ show sym
      | otherwise -> ["(" ++ show sym ++ "," ++ show n ++ ")"]
  manas = concat [syms W w, syms U u, syms B b, syms R r, syms G g, syms C c]
  sManas = case manas of
    [m] -> m
    -- No 2-tuple @ToManaPool@ instance exists (it is commented out); the
    -- cards pad a two-mana pool to a 3-tuple with @()@, e.g. @(U, U, ())@.
    [m1, m2] -> "(" ++ m1 ++ ", " ++ m2 ++ ", ())"
    _ -> "(" ++ List.intercalate ", " manas ++ ")"

showObject :: Object a -> EnvM Doc
showObject = getObjectName

showObjectNImpl ::
  (IsObjectType a) => TypeRep -> Doc -> Object a -> EnvM Doc
showObjectNImpl objNRef prefix obj = do
  let i = objectToId obj
  sObj <- showObject obj
  combinators <- getDataCombinators
  EnvM (State.gets $ Map.lookup i . originalObjectRep) >>= \case
    Nothing -> pure sObj -- Object is an unbound variable. Can happen when walking past a variable binding before showing the rest of the tree.
    Just originalRep -> case originalRep == objNRef of
      True -> pure sObj
      False -> pure case (combinators, sObj) of
        -- Constructors only (F): rebuild the wider @ObjectN@ from the binder's
        -- destructured payloads, e.g. @ZO sng3 (ON9a (... objN3))@. The binder
        -- renders the matching @\@(ZO sng3 objN3)@ as-pattern (see 'objBinder').
        (NoDataCombinators, DObject i' g)
          | Just chain <- objectNChain originalRep objNRef ->
              dApp
                "ZO"
                [ DObjectPart ObjectPartSing i' g
                , foldr (\consName inner -> dApp consName [inner]) (DObjectPart ObjectPartPayload i' g) chain
                ]
        _ -> DApp prefix [sObj]

-- | The @asFoo@ coercions the hand-written cards use for the 'ObjectN' types
-- that have one, keyed by the type's 'TypeRep'. Types without an entry render
-- with the positional @toZO\<n\>@ coercion picked by 'showObjectN'.
objectCoercions :: [(TypeRep, Doc)]
objectCoercions =
  [ (objNRep @OTNCreaturePlaneswalker, "asCreaturePlaneswalker")
  , (objNRep @OTNCreaturePlayer, "asCreaturePlayer")
  , (objNRep @OTNPlayerPlaneswalker, "asPlayerPlaneswalker")
  , (objNRep @OTNCreaturePlayerPlaneswalker, "asCreaturePlayerPlaneswalker")
  , (objNRep @OTNPermanent, "asPermanent")
  , (objNRep @OTNSpell, "asSpell")
  , (objNRep @OTNDamageSource, "asDamageSource")
  , (objNRep @OTNAny, "asAny")
  ]
 where
  objNRep :: forall ot. (Typeable (ObjectN ot)) => TypeRep
  objNRep = typeRep (Proxy @(ObjectN ot))

-- | Render an 'ObjectN' as its object atom behind the coercion the cards
-- would write: the matching @asFoo@ from 'objectCoercions' when there is one,
-- the given positional @toZO\<n\>@ otherwise. Under (F) 'NoDataCombinators'
-- the coercion is instead rebuilt from constructors ('showObjectNImpl'); the
-- @toZO\<n\>@ prefix computed here survives only as its fallback for sources
-- no constructor chain can widen (e.g. @OT0@). 'showObjectNImpl' drops the
-- coercion entirely when the object is used at its original type.
showObjectNAs :: (Typeable (ObjectN ot)) => String -> ObjectN ot -> EnvM Doc
showObjectNAs toZO objN = do
  combinators <- getDataCombinators
  let prefix = case combinators of
        NoDataCombinators -> fromString toZO
        LowDataCombinators -> fromMaybe (fromString toZO) (lookup rep objectCoercions)
        HighDataCombinators -> fromMaybe (fromString toZO) (lookup rep objectCoercions)
  visitObjectN' (showObjectNImpl rep prefix) objN
 where
  rep = typeOf objN

showObjectN :: forall zone ot. (IsZO zone ot) => ObjectN ot -> EnvM Doc
showObjectN objN' = viewOTN' objN' go
 where
  go :: ObjectN (OTN otk) -> OTN otk -> EnvM Doc
  go objN = \case
    OT0 -> pure $ DString $ "toZO0 " ++ show (getObjectId objN)
    OT1 -> showObjectNAs "toZO1" objN
    OT2 -> showObjectNAs "toZO2" objN
    OT3 -> showObjectNAs "toZO3" objN
    OT4 -> showObjectNAs "toZO4" objN
    OT5 -> showObjectNAs "toZO5" objN
    OT6 -> showObjectNAs "toZO6" objN
    OT7 -> showObjectNAs "toZO7" objN
    OT8 -> showObjectNAs "toZO8" objN
    OT9 -> showObjectNAs "toZO9" objN
    OT10 -> showObjectNAs "toZO10" objN
    OT11 -> showObjectNAs "toZO11" objN
    OT12 -> showObjectNAs "toZO12" objN
    OT13 -> showObjectNAs "toZO13" objN

showPower :: Power -> EnvM Doc
showPower = pure . DString . show

showSetCard :: SetCard ot -> EnvM Doc
showSetCard = showViaTree showTreeSetCard

showSetToken :: SetToken ot -> EnvM Doc
showSetToken = showViaTree showTreeSetToken

showSupertypes :: [Supertype ot] -> EnvM Doc
showSupertypes = showListM showSupertype

showSupertype :: Supertype ot -> EnvM Doc
showSupertype = \case
  Basic -> pure $ DString "Basic"
  Legendary -> pure $ DString "Legendary"
  Snow -> pure $ DString "Snow"
  Tribal tys -> do
    sTys <- showCreatureTypes tys
    pure $ DApp "Tribal" [sTys]
  World -> pure $ DString "World"

showTimePoint :: TimePoint p -> EnvM Doc
showTimePoint = pure . DString . show

showToken :: Token ot -> EnvM Doc
showToken = showViaTree showTreeToken

showToughness :: Toughness -> EnvM Doc
showToughness = pure . DString . show

showTypeOf :: forall a. (PrettyType a) => Proxy a -> EnvM Doc
showTypeOf _ = pure $ DString $ prettyType @a

showZoneObject :: forall zone ot. (IsZO zone ot) => ZO zone ot -> EnvM Doc
showZoneObject = \case
  ZO _ objN -> showObjectN @zone objN

showZoneObjects :: forall zone ot. (IsZO zone ot) => List (ZO zone ot) -> EnvM Doc
showZoneObjects (lenseList -> zo) = pluralize $ showZoneObject zo

----------------------------------------
-- Tree renderers (build `Doc`s; see `layout`).
--
-- These walk the first-order `Tree` produced by `buildTree` instead of the
-- continuation-based DSL. Bound objects are read from `TreeZO` nodes (and
-- registered via 'registerTreeObject' so their names/generations resolve)
-- rather than minted with 'newObjectN'; the ids already match because
-- `Tree`'s builder numbers objects from 1 like the old walker did. Leaf values
-- (mana, colors, types, ...) are stored raw in the `Tree`, so the existing leaf
-- renderers above are reused directly.

-- | Application whose argument docs are produced in 'EnvM'.
app :: String -> [EnvM Doc] -> EnvM Doc
app hd ms = dApp hd <$> sequence ms

-- | Application with a computed (Doc) head, e.g. @SetCard s r@.
appI :: EnvM Doc -> [EnvM Doc] -> EnvM Doc
appI hdM ms = DApp <$> hdM <*> sequence ms

-- | Record constructor whose field values are produced in 'EnvM'.
recD :: String -> [(String, EnvM Doc)] -> EnvM Doc
recD hd fields = DRec (fromString hd) <$> traverse (\(n, m) -> (,) n <$> m) fields

-- | Register a binder object and return its binder 'Doc' (no backslash).
bindObj :: (IsZO zone ot) => String -> ZO zone ot -> EnvM Doc
bindObj memo zo = do
  registerTreeObject memo zo
  objBinder zo

-- | The binder 'Doc' for an already-registered object: its name, plus --
-- under (F) 'NoDataCombinators' -- the @\@(ZO sng\<n\> objN\<n\>)@ as-pattern
-- exposing the payloads that coerced use-sites rebuild with constructors
-- ('showObjectNImpl'). The as-pattern atom renders as the empty string when no
-- use-site coerces this binder, so undestructured binders stay bare names.
objBinder :: (IsZO zone ot) => ZO zone ot -> EnvM Doc
objBinder zo = do
  nm <- showZoneObject zo
  combinators <- getDataCombinators
  pure case (combinators, nm) of
    (NoDataCombinators, DObject i g) -> nm <> DObjectPart ObjectPartPattern i g
    _ -> nm

treeVarName :: Tree (Variable a) -> Doc
treeVarName = \case
  TreeVariable _ vid -> DVariable vid

treeCardName :: Tree (Card ot) -> CardName
treeCardName = \case
  TreeCard name _ -> name
  TreeDoubleSidedCard c1 c2 -> treeCardName c1 <> " // " <> treeCardName c2
  TreeSplitCard c1 c2 _ -> treeCardName c1 <> " // " <> treeCardName c2

showTreeList :: (Tree a -> EnvM Doc) -> Tree [a] -> EnvM Doc
showTreeList f = \case
  TreeList xs -> DList <$> mapM f xs

proxyOfZo :: Tree (ZO zone ot) -> Proxy ot
proxyOfZo _ = Proxy

proxyOfReqs :: Tree [Requirement zone ot] -> Proxy ot
proxyOfReqs _ = Proxy

-- | The rendered @ZO zone ot@ type of a binder, used by (F) below
-- 'HighDataCombinators' as a 'dAnnBinder' annotation.
zoTypeOf :: forall zone ot. (IsZO zone ot) => Tree (ZO zone ot) -> Doc
zoTypeOf _ = DString $ prettyType @(ZO zone ot)

-- | Like 'zoTypeOf' but for a plural (@maskeds@-style) binder of type
-- @List (ZO zone ot)@.
listZoTypeOf :: forall zone ot. (IsZO zone ot) => Tree (ZO zone ot) -> Doc
listZoTypeOf _ = DString $ "List (" <> prettyType @(ZO zone ot) <> ")"

showTreeZoneObject :: (IsZO zone ot) => Tree (ZO zone ot) -> EnvM Doc
showTreeZoneObject = \case
  TreeZO zo -> showZoneObject zo

showTreeAbility :: Tree (Ability zone ot) -> EnvM Doc
showTreeAbility = \case
  TreeActivated ability -> app "Activated" [showTreeElect ability]
  TreeStatic ability -> app "Static" [showTreeStaticAbility ability]
  TreeTriggered ability -> app "Triggered" [showTreeTriggeredAbility ability]

showTreeAnyCard :: Tree AnyCard -> EnvM Doc
showTreeAnyCard = \case
  TreeAnyCard1 card -> app "AnyCard1" [showTreeCard card]
  TreeAnyCard2 card -> app "AnyCard2" [showTreeCard card]

showTreeAnyToken :: Tree AnyToken -> EnvM Doc
showTreeAnyToken = \case
  TreeAnyToken token -> app "AnyToken" [showTreeToken token]

showTreeActivatedAbility :: Tree (ActivatedAbility zone ot) -> EnvM Doc
showTreeActivatedAbility = \case
  TreeAbility cost effect ->
    recD
      "Ability"
      [ ("activated_cost", showTreeCost cost)
      , ("activated_effect", showTreeElect effect)
      ]
  TreeCycling cost -> app "Cycling" [showTreeCost cost]

showTreeBattleType :: Tree BattleType -> EnvM Doc
showTreeBattleType = \case
  TreeSeige -> pure $ dName "Seige"

showTreeCard :: Tree (Card ot) -> EnvM Doc
showTreeCard tree = case tree of
  TreeCard name elect ->
    showCardImpl "Card" name $ app "Card" [pure $ dName $ show name, showTreeElect elect]
  TreeDoubleSidedCard card1 card2 ->
    showCardImpl "DoubleSidedCard" (treeCardName tree) $
      app "DoubleSidedCard" [showTreeCard card1, showTreeCard card2]
  TreeSplitCard card1 card2 splitAbilities ->
    showCardImpl "SplitCard" (treeCardName tree) $
      app
        "SplitCard"
        [ showTreeCard card1
        , showTreeCard card2
        , showTreeList (showTreeSomeZone showTreeAbility) splitAbilities
        ]

showTreeCardCharacteristic :: Tree (CardCharacteristic ot) -> EnvM Doc
showTreeCardCharacteristic = \case
  TreeArtifactCharacteristic colors sups artTypes spec ->
    recD
      "ArtifactCharacteristic"
      [ ("artifact_colors", showColors colors)
      , ("artifact_supertypes", showSupertypes sups)
      , ("artifact_artifactTypes", showArtifactTypes artTypes)
      , ("artifact_spec", showTreeCardSpec spec)
      ]
  TreeArtifactCreatureCharacteristic colors sups artTypes creatTypes power toughness spec ->
    recD
      "ArtifactCreatureCharacteristic"
      [ ("artifactCreature_colors", showColors colors)
      , ("artifactCreature_supertypes", showSupertypes sups)
      , ("artifactCreature_artifactTypes", showArtifactTypes artTypes)
      , ("artifactCreature_creatureTypes", showCreatureTypes creatTypes)
      , ("artifactCreature_power", showPower power)
      , ("artifactCreature_toughness", showToughness toughness)
      , ("artifactCreature_spec", showTreeCardSpec spec)
      ]
  TreeArtifactLandCharacteristic sups artTypes landTypes spec ->
    recD
      "ArtifactLandCharacteristic"
      [ ("artifactLand_supertypes", showSupertypes sups)
      , ("artifactLand_artifactTypes", showArtifactTypes artTypes)
      , ("artifactLand_landTypes", showLandTypes landTypes)
      , ("artifactLand_spec", showTreeCardSpec spec)
      ]
  TreeBattleCharacteristic colors sups battleTypes defense spec ->
    recD
      "BattleCharacteristic"
      [ ("battle_colors", showColors colors)
      , ("battle_supertypes", showSupertypes sups)
      , ("battle_battleTypes", showBattleTypes battleTypes)
      , ("battle_defense", showDefense defense)
      , ("battle_spec", showTreeCardSpec spec)
      ]
  TreeCreatureCharacteristic colors sups creatureTypes power toughness spec ->
    recD
      "CreatureCharacteristic"
      [ ("creature_colors", showColors colors)
      , ("creature_supertypes", showSupertypes sups)
      , ("creature_creatureTypes", showCreatureTypes creatureTypes)
      , ("creature_power", showPower power)
      , ("creature_toughness", showToughness toughness)
      , ("creature_spec", showTreeCardSpec spec)
      ]
  TreeEnchantmentCharacteristic colors sups enchTypes spec ->
    recD
      "EnchantmentCharacteristic"
      [ ("enchantment_colors", showColors colors)
      , ("enchantment_supertypes", showSupertypes sups)
      , ("enchantment_enchantmentTypes", showTreeEnchantmentTypes enchTypes)
      , ("enchantment_spec", showTreeCardSpec spec)
      ]
  TreeEnchantmentCreatureCharacteristic colors sups creatTypes enchTypes power toughness spec ->
    recD
      "EnchantmentCreatureCharacteristic"
      [ ("enchantmentCreature_colors", showColors colors)
      , ("enchantmentCreature_supertypes", showSupertypes sups)
      , ("enchantmentCreature_creatureTypes", showCreatureTypes creatTypes)
      , ("enchantmentCreature_enchantmentTypes", showTreeEnchantmentTypes enchTypes)
      , ("enchantmentCreature_power", showPower power)
      , ("enchantmentCreature_toughness", showToughness toughness)
      , ("enchantmentCreature_spec", showTreeCardSpec spec)
      ]
  TreeInstantCharacteristic colors sups spec ->
    recD
      "InstantCharacteristic"
      [ ("instant_colors", showColors colors)
      , ("instant_supertypes", showSupertypes sups)
      , ("instant_spec", showTreeElect spec)
      ]
  TreeLandCharacteristic sups landTypes spec ->
    recD
      "LandCharacteristic"
      [ ("land_supertypes", showSupertypes sups)
      , ("land_landTypes", showLandTypes landTypes)
      , ("land_spec", showTreeCardSpec spec)
      ]
  TreePlaneswalkerCharacteristic colors sups spec ->
    recD
      "PlaneswalkerCharacteristic"
      [ ("planeswalker_colors", showColors colors)
      , ("planeswalker_supertypes", showSupertypes sups)
      , ("planeswalker_spec", showTreeCardSpec spec)
      ]
  TreeSorceryCharacteristic colors sups spec ->
    recD
      "SorceryCharacteristic"
      [ ("sorcery_colors", showColors colors)
      , ("sorcery_supertypes", showSupertypes sups)
      , ("sorcery_spec", showTreeElect spec)
      ]

showTreeCardSpec :: Tree (CardSpec ot) -> EnvM Doc
showTreeCardSpec = \case
  TreeArtifactSpec cost abilities ->
    recD
      "ArtifactSpec"
      [ ("artifact_cost", showTreeCost cost)
      , ("artifact_abilities", abils abilities)
      ]
  TreeArtifactCreatureSpec cost artAbils creatAbils bothAbils ->
    recD
      "ArtifactCreatureSpec"
      [ ("artifactCreature_cost", showTreeCost cost)
      , ("artifactCreature_artifactAbilities", abils artAbils)
      , ("artifactCreature_creatureAbilities", abils creatAbils)
      , ("artifactCreature_artifactCreatureAbilities", abils bothAbils)
      ]
  TreeArtifactLandSpec artAbils landAbils bothAbils ->
    recD
      "ArtifactLandSpec"
      [ ("artifactLand_artifactAbilities", abils artAbils)
      , ("artifactLand_landAbilities", abils landAbils)
      , ("artifactLand_artifactLandAbilities", abils bothAbils)
      ]
  TreeBattleSpec cost abilities ->
    recD "BattleSpec" [("battle_cost", showTreeCost cost), ("battle_abilities", abils abilities)]
  TreeCreatureSpec cost abilities ->
    recD "CreatureSpec" [("creature_cost", showTreeCost cost), ("creature_abilities", abils abilities)]
  TreeEnchantmentSpec cost abilities ->
    recD
      "EnchantmentSpec"
      [("enchantment_cost", showTreeCost cost), ("enchantment_abilities", abils abilities)]
  TreeEnchantmentCreatureSpec cost creatAbils enchAbils bothAbils ->
    recD
      "EnchantmentCreatureSpec"
      [ ("enchantmentCreature_cost", showTreeCost cost)
      , ("enchantmentCreature_creatureAbilities", abils creatAbils)
      , ("enchantmentCreature_enchantmentAbilities", abils enchAbils)
      , ("enchantmentCreature_enchantmentCreatureAbilities", abils bothAbils)
      ]
  TreeInstantSpec cost abilities oneShot ->
    recD
      "InstantSpec"
      [ ("instant_cost", showTreeCost cost)
      , ("instant_abilities", abils abilities)
      , ("instant_effect", showTreeWithThis showTreeElect "this" oneShot)
      ]
  TreeLandSpec abilities -> recD "LandSpec" [("land_abilities", abils abilities)]
  TreePlaneswalkerSpec cost loyalty abilities ->
    recD
      "PlaneswalkerSpec"
      [ ("planeswalker_cost", showTreeCost cost)
      , ("planeswalker_loyalty", showLoyalty loyalty)
      , ("planeswalker_abilities", abils abilities)
      ]
  TreeSorcerySpec cost abilities oneShot ->
    recD
      "SorcerySpec"
      [ ("sorcery_cost", showTreeCost cost)
      , ("sorcery_abilities", abils abilities)
      , ("sorcery_effect", showTreeWithThis showTreeElect "this" oneShot)
      ]
 where
  abils :: (IsOTN ot') => Tree [SomeZone WithThisAbility ot'] -> EnvM Doc
  abils = showTreeList (showTreeSomeZoneWithThisAbility "this")

showTreeCase :: (Tree x -> EnvM Doc) -> Tree (Case x) -> EnvM Doc
showTreeCase showX = \case
  TreeCaseFin fin natList ->
    recD
      "CaseFin"
      [ ("caseFin", pure $ treeVarName fin)
      , ("ofFin", showTreeNatList showX natList)
      ]

showTreeCondition :: Tree Condition -> EnvM Doc
showTreeCondition = \case
  TreeCAnd conds -> app "CAnd" [showTreeConditions conds]
  TreeCNot cond -> app "CNot" [showTreeCondition cond]
  TreeCOr conds -> app "COr" [showTreeConditions conds]
  TreeSatisfies objN reqs ->
    -- XXX: Keeps the historical double space after "Satisfies".
    appI (pure "Satisfies ") [showTreeZoneObject objN, showTreeRequirements reqs]

showTreeConditions :: Tree [Condition] -> EnvM Doc
showTreeConditions = showTreeList showTreeCondition

showTreeCost :: Tree Cost -> EnvM Doc
showTreeCost = \case
  TreeAndCosts costs -> app "AndCosts" [showTreeList showTreeCost costs]
  TreeCostCase case_ -> app "CostCase" [showTreeCase showTreeCost case_]
  TreeDiscardRandomCost amount -> app "DiscardRandomCost" [pure $ dName $ show amount]
  TreeExileCost reqs -> app "ExileCost" [showTreeRequirements reqs]
  TreeLoyaltyCost zoPlaneswalker loyalty ->
    app "LoyaltyCost" [showTreeZoneObject zoPlaneswalker, showLoyalty loyalty]
  TreeManaCost cost -> do
    combinators <- getDataCombinators
    case combinators of
      NoDataCombinators -> app "ManaCost" [showManaCostRaw cost]
      LowDataCombinators -> pure $ dApp "manaCost" [showManaCostArg cost]
      HighDataCombinators -> pure $ dApp "manaCost" [showManaCostArg cost]
  TreeOrCosts costs -> app "OrCosts" [showTreeList showTreeCost costs]
  TreePayLife amount -> app "PayLife" [pure $ dName $ show amount]
  TreeSacrificeCost reqs -> do
    sTy <- showTypeOf (proxyOfReqs reqs)
    reqsDoc <- showTreeRequirements reqs
    pure $ dTypeApp "SacrificeCost" sTy [reqsDoc]
  TreeTapCost reqs -> app "TapCost" [showTreeRequirements reqs]

showTreeEffect :: Tree (Effect e) -> EnvM Doc
showTreeEffect = \case
  TreeAddMana player mana -> app "AddMana" [showTreeZoneObject player, showManaPool mana]
  TreeAddToBattlefield player token ->
    app "AddToBattlefield" [showTreeZoneObject player, showTreeToken token]
  TreeCantBeRegenerated creature -> app "CantBeRegenerated" [showTreeZoneObject creature]
  TreeChangeTo before after -> app "ChangeTo" [showTreeZoneObject before, showTreeCard after]
  TreeCounterAbility obj -> app "CounterAbility" [showTreeZoneObject obj]
  TreeCounterSpell obj -> app "CounterSpell" [showTreeZoneObject obj]
  TreeDealDamage source victim damage ->
    app
      "DealDamage"
      [showTreeZoneObject source, showTreeZoneObject victim, showDamage damage]
  TreeDestroy obj -> app "Destroy" [showTreeZoneObject obj]
  TreeDrawCards player n -> app "DrawCards" [showTreeZoneObject player, pure $ dName $ show n]
  TreeEffectCase case_ -> app "EffectCase" [showTreeCase showTreeEffect case_]
  TreeEffectContinuous effect -> app "EffectContinuous" [showTreeEffect effect]
  TreeEndTheTurn -> pure $ dName "EndTheTurn"
  TreeExile obj -> app "Exile" [showTreeZoneObject obj]
  TreeGainAbility obj ability ->
    app "GainAbility" [showTreeZoneObject obj, showTreeWithThisAbility "this" ability]
  TreeGainControl player obj ->
    app "GainControl" [showTreeZoneObject player, showTreeZoneObject obj]
  TreeGainLife player n -> app "GainLife" [showTreeZoneObject player, pure $ dName $ show n]
  TreeLoseAbility obj ability ->
    app "LoseAbility" [showTreeZoneObject obj, showTreeWithThisAbility "this" ability]
  TreeLoseLife player n -> app "LoseLife" [showTreeZoneObject player, pure $ dName $ show n]
  TreePutOntoBattlefield player obj ->
    app "PutOntoBattlefield" [showTreeZoneObject player, showTreeZoneObject obj]
  TreeSacrifice player reqs ->
    app "Sacrifice" [showTreeZoneObject player, showTreeRequirements reqs]
  TreeSearchLibrary searcher searchee withCard ->
    app
      "SearchLibrary"
      [ showTreeZoneObject searcher
      , showTreeZoneObject searchee
      , showTreeWithLinkedObject showTreeElect "card" withCard
      ]
  TreeSequence effects -> app "Sequence" [showTreeEffects effects]
  TreeShuffleLibrary player -> app "ShuffleLibrary" [showTreeZoneObject player]
  TreeStatDelta creature power toughness ->
    app
      "StatDelta"
      [showTreeZoneObject creature, showPower power, showToughness toughness]
  TreeTap obj -> app "Tap" [showTreeZoneObject obj]
  TreeUntap obj -> app "Untap" [showTreeZoneObject obj]
  TreeUntil electEvent effect -> app "Until" [showTreeElect electEvent, showTreeEffect effect]
  TreeWithList withList -> app "WithList" [showTreeWithList showTreeEffect withList]

showTreeEffects :: Tree [Effect e] -> EnvM Doc
showTreeEffects = showTreeList showTreeEffect

showTreeElse :: Tree (Else s e ot) -> EnvM Doc
showTreeElse = \case
  TreeElseCost elect -> app "ElseCost" [showTreeElect elect]
  TreeElseEffect elect -> app "ElseEffect" [showTreeElect elect]
  TreeElseEvent -> pure $ dName "ElseEvent"

showTreeEnchant :: Tree (Enchant zone ot) -> EnvM Doc
showTreeEnchant = \case
  TreeEnchant withObj -> app "Enchant" [showTreeWithLinkedObject showTreeElect "enchanted" withObj]

showTreeEnchantmentType :: Tree (EnchantmentType ot) -> EnvM Doc
showTreeEnchantmentType = \case
  TreeAura enchant -> app "Aura" [showTreeEnchant enchant]

showTreeEnchantmentTypes :: Tree [EnchantmentType ot] -> EnvM Doc
showTreeEnchantmentTypes = showTreeList showTreeEnchantmentType

showTreeEntersStatic :: Tree (EntersStatic zone ot) -> EnvM Doc
showTreeEntersStatic = \case
  TreeEntersTapped -> pure $ dName "EntersTapped"

showTreeEvent :: Tree Event -> EnvM Doc
showTreeEvent = showTreeEventListener' \case TreeProxy -> pure $ dName "Proxy"

showTreeEventListener :: Tree EventListener -> EnvM Doc
showTreeEventListener = showTreeEventListener' showTreeElect

showTreeEventListener' ::
  (forall ot. Tree (x ot) -> EnvM Doc) ->
  Tree (EventListener' x) ->
  EnvM Doc
showTreeEventListener' showX = \case
  TreeBecomesTapped withObject ->
    app "BecomesTapped" [showTreeWithLinkedObject showX "perm" withObject]
  TreeEntersBattlefield withObject ->
    app "EntersBattlefield" [showTreeWithLinkedObject showX "perm" withObject]
  TreeEntersNonBattlefield withObject ->
    app "EntersNonBattlefield" [showTreeWithLinkedObject showX "perm" withObject]
  TreeEvents listeners -> app "Events" [showTreeList (showTreeEventListener' showX) listeners]
  TreeSpellIsCast withObject ->
    app "SpellIsCast" [showTreeWithLinkedObject showX "spell" withObject]
  TreeTimePoint timePoint oneShot ->
    app "TimePoint" [showTimePoint timePoint, showX oneShot]

showTreeElect :: Tree (Elect s el ot) -> EnvM Doc
showTreeElect = \case
  TreeActivePlayer (TreeZO player) treeBody -> do
    nm <- bindObj "active" player
    body <- showTreeElect treeBody
    pure $ dApp "ActivePlayer" [dLam nm body]
  TreeAll withObjects ->
    app "All" [showTreeWithMaskedObjects showTreeElect "obj" withObjects]
  TreeChoose treePlayer withObject ->
    app
      "Choose"
      [showTreeZoneObject treePlayer, showTreeWithMaskedObject showTreeElect "choose" withObject]
  TreeChooseOption treePlayer natList treeVar treeBody -> do
    playerDoc <- showTreeZoneObject treePlayer
    natListDoc <- showTreeNatList showTreeCondition natList
    body <- showTreeElect treeBody
    pure $ dApp "ChooseOption" [playerDoc, natListDoc, dLam (treeVarName treeVar) body]
  TreeElectCondition cond -> app "Condition" [showTreeCondition cond]
  TreeControllerOf treeZObj treePlayer treeBody ->
    goTreePlayerOf1 "ControllerOf" "controller" treeZObj treePlayer treeBody
  TreeElectCost cost -> app "Cost" [showTreeCost cost]
  TreeElectEffect effect -> app "Effect" [showTreeEffects effect]
  TreeElectActivated activated -> app "ElectActivated" [showTreeActivatedAbility activated]
  TreeElectCardFacet post -> app "ElectCardFacet" [showTreeCardCharacteristic post]
  TreeElectCardSpec post -> app "ElectCardSpec" [showTreeCardSpec post]
  TreeElectCase case_ -> app "ElectCase" [showTreeCase showTreeElect case_]
  TreeEndTargets elect -> app "EndTargets" [showTreeElect elect]
  TreeElectEvent event -> app "Event" [showTreeEvent event]
  TreeIf cond then_ else_ ->
    app "If" [showTreeCondition cond, showTreeElect then_, showTreeElse else_]
  TreeListen listener -> app "Listen" [showTreeEventListener listener]
  TreeOwnerOf treeZObj treePlayer treeBody ->
    goTreePlayerOf1 "OwnerOf" "owner" treeZObj treePlayer treeBody
  TreePlayerPays treePlayer cost treeVar treeBody -> do
    playerDoc <- showTreeZoneObject treePlayer
    costDoc <- showTreeCost cost
    body <- showTreeElect treeBody
    pure $ dApp "PlayerPays" [playerDoc, costDoc, dLam (treeVarName treeVar) body]
  TreeRandom withObject ->
    app "Random" [showTreeWithMaskedObject showTreeElect "rand" withObject]
  TreeTarget treePlayer withObject ->
    app
      "Target"
      [showTreeZoneObject treePlayer, showTreeWithMaskedObject showTreeElect "target" withObject]
  TreeVariableFromPower treeCreature treeVar treeBody -> do
    creatureDoc <- showTreeZoneObject treeCreature
    body <- showTreeElect treeBody
    pure $ dApp "VariableFromPower" [creatureDoc, dLam (treeVarName treeVar) body]
  TreeVariableInt treeVar treeBody -> do
    body <- showTreeElect treeBody
    pure $ dApp "VariableInt" [dLam (treeVarName treeVar) body]
  TreeYour (TreeZO player) treeBody -> do
    nm <- bindObj "you" player
    body <- showTreeElect treeBody
    pure $ dApp "Your" [dLam nm body]
 where
  goTreePlayerOf1 ::
    (IsZO zone OTNAny) =>
    String ->
    String ->
    Tree (ZO zone OTNAny) ->
    Tree (ZO 'ZBattlefield OTNPlayer) ->
    Tree (Elect s el ot) ->
    EnvM Doc
  goTreePlayerOf1 consName varName treeZObj (TreeZO player) treeBody = do
    let objId = case treeZObj of TreeZO z -> getObjectId z
    objPrefix <- getObjectNamePrefix objId
    nm <- bindObj (case objPrefix == "this" of True -> "you"; False -> varName) player
    zObjDoc <- showTreeZoneObject treeZObj
    body <- showTreeElect treeBody
    pure $ dApp consName [zObjDoc, dLam nm body]

showTreeNatList ::
  forall u n x.
  (IsUser u) => (Tree x -> EnvM Doc) -> Tree (NatList u n x) -> EnvM Doc
showTreeNatList showX = \case
  TreeLZ u x -> appI (pure $ fromString $ "LZ (" <> show u <> ")") [showX x]
  TreeLS u x xs ->
    appI (pure $ fromString $ "LS (" <> show u <> ")") [showX x, showTreeNatList showX xs]

showTreeRequirement :: Tree (Requirement zone ot) -> EnvM Doc
showTreeRequirement = \case
  TreeControlledBy obj -> app "ControlledBy" [showTreeZoneObject obj]
  TreeControlsA req -> app "ControlsA" [showTreeRequirement req]
  TreeHasAbility ability ->
    app "HasAbility" [showTreeSomeZoneWithThisAbility "this" ability]
  TreeHasLandType landType -> app "HasLandType" [showLandType landType]
  TreeIs objN -> app "Is" [showTreeZoneObject objN]
  TreeIsOpponentOf player -> app "IsOpponentOf" [showTreeZoneObject player]
  TreeIsTapped -> pure $ dName "IsTapped"
  TreeNot req -> app "Not" [showTreeRequirement req]
  TreeOfColors colors -> app "OfColors" [showColors colors]
  TreeOwnedBy obj -> app "OwnedBy" [showTreeZoneObject obj]
  TreeRAnd reqs -> app "RAnd" [showTreeRequirements reqs]
  TreeROr reqs -> app "ROr" [showTreeRequirements reqs]
  TreeReq2 reqsA reqsB -> app "Req2" [showTreeRequirements reqsA, showTreeRequirements reqsB]
  TreeReq3 reqsA reqsB reqsC ->
    app "Req3" [showTreeRequirements reqsA, showTreeRequirements reqsB, showTreeRequirements reqsC]
  TreeReq4 reqsA reqsB reqsC reqsD ->
    app
      "Req4"
      [ showTreeRequirements reqsA
      , showTreeRequirements reqsB
      , showTreeRequirements reqsC
      , showTreeRequirements reqsD
      ]
  TreeReq5 reqsA reqsB reqsC reqsD reqsE ->
    app
      "Req5"
      [ showTreeRequirements reqsA
      , showTreeRequirements reqsB
      , showTreeRequirements reqsC
      , showTreeRequirements reqsD
      , showTreeRequirements reqsE
      ]

showTreeRequirements :: Tree [Requirement zone ot] -> EnvM Doc
showTreeRequirements = showTreeList showTreeRequirement

showTreeSetCard :: Tree (SetCard ot) -> EnvM Doc
showTreeSetCard = \case
  TreeSetCard set rarity card ->
    appI (pure $ fromString $ "SetCard " <> show set <> " " <> show rarity) [showTreeCard card]

showTreeSetToken :: Tree (SetToken ot) -> EnvM Doc
showTreeSetToken = \case
  TreeSetToken set rarity token ->
    appI
      (pure $ fromString $ "SetToken " <> show set <> " " <> show rarity)
      [showTreeToken token]

showTreeSomeZone ::
  forall liftZOT ot.
  (forall zone. Tree (liftZOT zone ot) -> EnvM Doc) ->
  Tree (SomeZone liftZOT ot) ->
  EnvM Doc
showTreeSomeZone showM = \case
  TreeSomeZone x -> app "SomeZone" [showM x]
  TreeSomeZone2 x -> app "SomeZone2" [showM x]

showTreeStaticAbility :: Tree (StaticAbility zone ot) -> EnvM Doc
showTreeStaticAbility = \case
  TreeAs electListener -> app "As" [showTreeElect electListener]
  TreeBestow cost enchant -> app "Bestow" [showTreeElect cost, showTreeEnchant enchant]
  TreeCantBlock -> pure $ dName "CantBlock"
  TreeDefender -> pure $ dName "Defender"
  TreeEnters entersStatic -> app "Enters" [showTreeEntersStatic entersStatic]
  TreeFirstStrike -> pure $ dName "FirstStrike"
  TreeFlying -> pure $ dName "Flying"
  TreeFuse -> pure $ dName "Fuse"
  TreeHaste -> pure $ dName "Haste"
  TreeLandwalk reqs -> app "Landwalk" [showTreeRequirements reqs]
  TreePhasing -> pure $ dName "Phasing"
  TreeStaticContinuous continuous -> app "StaticContinuous" [showTreeElect continuous]
  TreeSuspend time cost -> app "Suspend" [pure $ dName $ show time, showTreeElect cost]
  TreeTrample -> pure $ dName "Trample"

showTreeToken :: Tree (Token ot) -> EnvM Doc
showTreeToken = \case
  TreeToken card -> app "Token" [showTreeCard card]

showTreeTriggeredAbility :: Tree (TriggeredAbility zone ot) -> EnvM Doc
showTreeTriggeredAbility = \case
  TreeWhen listener -> app "When" [showTreeElect listener]

showTreeWithLinkedObject ::
  forall liftOT zone ot.
  (IsZO zone ot) =>
  (forall ot'. Tree (liftOT ot') -> EnvM Doc) ->
  String ->
  Tree (WithLinkedObject liftOT zone ot) ->
  EnvM Doc
showTreeWithLinkedObject showM memo = \case
  TreeLinked1 reqs treeZo body -> go "Linked1" reqs treeZo body
  TreeLinked2 reqs treeZo body -> go "Linked2" reqs treeZo body
  TreeLinked3 reqs treeZo body -> go "Linked3" reqs treeZo body
  TreeLinked4 reqs treeZo body -> go "Linked4" reqs treeZo body
  TreeLinked5 reqs treeZo body -> go "Linked5" reqs treeZo body
 where
  go ::
    (IsZO zone ot) =>
    String ->
    Tree [Requirement zone ot] ->
    Tree (ZO zone ot) ->
    Tree (liftOT ot) ->
    EnvM Doc
  go consName reqs treeZo@(TreeZO zo) body = do
    combinators <- getDataCombinators
    sTy <- showTypeOf (proxyOfZo treeZo)
    reqsDoc <- showTreeRequirements reqs
    nm <- bindObj memo zo
    bodyDoc <- showM body
    pure case combinators of
      HighDataCombinators -> dTypeApp "linked" sTy [reqsDoc, dLam nm bodyDoc]
      _ -> dApp consName [reqsDoc, dLam (dAnnBinder nm (zoTypeOf treeZo)) bodyDoc]

showTreeWithList ::
  (Tree ret -> EnvM Doc) -> Tree (WithList ret zone ot) -> EnvM Doc
showTreeWithList showRet = \case
  TreeCountOf zos treeVar treeRet -> do
    zosDoc <- showZoneObjects zos
    retDoc <- showRet treeRet
    pure $ dApp "CountOf" [zosDoc, dLam (treeVarName treeVar) retDoc]
  TreeEach zos treeZo treeRet -> do
    zosDoc <- showZoneObjects zos
    nm <- case treeZo of TreeZO zo -> objBinder zo
    retDoc <- showRet treeRet
    pure $ dApp "Each" [zosDoc, dLam nm retDoc]
  TreeSuchThat reqs withList ->
    app "SuchThat" [showTreeRequirements reqs, showTreeWithList showRet withList]

showTreeWithMaskedObject ::
  forall liftOT zone ot.
  (IsZone zone) =>
  (Tree (liftOT ot) -> EnvM Doc) ->
  String ->
  Tree (WithMaskedObject liftOT zone ot) ->
  EnvM Doc
showTreeWithMaskedObject showM memo = \case
  TreeMasked1 reqs treeZo body -> go "Masked1" reqs treeZo body
  TreeMasked2 reqs treeZo body -> go "Masked2" reqs treeZo body
  TreeMasked3 reqs treeZo body -> go "Masked3" reqs treeZo body
  TreeMasked4 reqs treeZo body -> go "Masked4" reqs treeZo body
  TreeMasked5 reqs treeZo body -> go "Masked5" reqs treeZo body
  TreeMasked6 reqs treeZo body -> go "Masked6" reqs treeZo body
  TreeMasked7 reqs treeZo body -> go "Masked7" reqs treeZo body
 where
  go ::
    (IsZO zone ot') =>
    String ->
    Tree [Requirement zone ot'] ->
    Tree (ZO zone ot') ->
    Tree (liftOT ot) ->
    EnvM Doc
  go consName reqs treeZo@(TreeZO zo) body = do
    combinators <- getDataCombinators
    sTy <- showTypeOf (proxyOfZo treeZo)
    reqsDoc <- showTreeRequirements reqs
    nm <- bindObj memo zo
    bodyDoc <- showM body
    pure case combinators of
      HighDataCombinators -> dTypeApps "masked" (sTy : zoneTypeArgs @zone) [reqsDoc, dLam nm bodyDoc]
      _ -> dApp consName [reqsDoc, dLam (dAnnBinder nm (zoTypeOf treeZo)) bodyDoc]

showTreeWithMaskedObjects ::
  forall liftOT zone ot.
  (IsZone zone) =>
  (Tree (liftOT ot) -> EnvM Doc) ->
  String ->
  Tree (WithMaskedObjects liftOT zone ot) ->
  EnvM Doc
showTreeWithMaskedObjects showM memo = \case
  TreeMaskeds1 reqs treeZo body -> go "Maskeds1" reqs treeZo body
  TreeMaskeds2 reqs treeZo body -> go "Maskeds2" reqs treeZo body
  TreeMaskeds3 reqs treeZo body -> go "Maskeds3" reqs treeZo body
  TreeMaskeds4 reqs treeZo body -> go "Maskeds4" reqs treeZo body
  TreeMaskeds5 reqs treeZo body -> go "Maskeds5" reqs treeZo body
  TreeMaskeds6 reqs treeZo body -> go "Maskeds6" reqs treeZo body
  TreeMaskeds7 reqs treeZo body -> go "Maskeds7" reqs treeZo body
 where
  go ::
    (IsZO zone ot') =>
    String ->
    Tree [Requirement zone ot'] ->
    Tree (ZO zone ot') ->
    Tree (liftOT ot) ->
    EnvM Doc
  go consName reqs treeZo@(TreeZO zo) body = do
    combinators <- getDataCombinators
    sTy <- showTypeOf (proxyOfZo treeZo)
    reqsDoc <- showTreeRequirements reqs
    registerTreeObject memo zo
    nm <- pluralize (showZoneObject zo)
    bodyDoc <- showM body
    pure case combinators of
      HighDataCombinators -> dTypeApps "maskeds" (sTy : zoneTypeArgs @zone) [reqsDoc, dLam nm bodyDoc]
      _ -> dApp consName [reqsDoc, dLam (dAnnBinder nm (listZoTypeOf treeZo)) bodyDoc]

-- | @thisObject \\binder -> body@ (binder is one name, or a tuple). No @\@ot@
-- type application: 'thisObject'\'s first visible type parameter is the @zone@
-- (from @class AsWithThis zone ot@), so annotating @ot@ there would not
-- type-check. The hand-written cards likewise leave @ot@ to be inferred.
showTreeWithThis ::
  forall liftOT zone ot.
  (IsZO zone ot) =>
  (forall ot'. Tree (liftOT ot') -> EnvM Doc) ->
  String ->
  Tree (WithThis liftOT zone ot) ->
  EnvM Doc
showTreeWithThis = showTreeWithThisWrapped (\lam -> dApp "thisObject" [lam])

-- | 'showTreeWithThis' generalized over the head that wraps the
-- @\\binder -> body@ lambda: 'showTreeWithThis' uses @thisObject@; the ability
-- combinators use @activated \@zone@ / @static \@zone@ / @triggered \@zone@ (see
-- 'showTreeSomeZoneWithThisAbility'). Under (F) below 'HighDataCombinators'
-- the wrap head is ignored and the raw @ThisN@ constructor is emitted instead,
-- with each binder 'dAnnBinder'-annotated so the existential @zone@ (which the
-- combinators pin with @\@zone@) is pinned by the binder types.
showTreeWithThisWrapped ::
  forall liftOT zone ot.
  (IsZO zone ot) =>
  (Doc -> Doc) ->
  (forall ot'. Tree (liftOT ot') -> EnvM Doc) ->
  String ->
  Tree (WithThis liftOT zone ot) ->
  EnvM Doc
showTreeWithThisWrapped wrap showM memo tree = do
  combinators <- getDataCombinators
  let bind :: (IsZO zone ot'') => Tree (ZO zone ot'') -> EnvM Doc
      bind treeZo@(TreeZO zo) = do
        nm <- bindObj memo zo
        pure case combinators of
          HighDataCombinators -> nm
          _ -> dAnnBinder nm (zoTypeOf treeZo)
      withTy :: String -> [EnvM Doc] -> Tree (liftOT ot) -> EnvM Doc
      withTy consName binders body = do
        names <- sequence binders
        bodyDoc <- showM body
        let lam = dLam (dTuple names) bodyDoc
        pure case combinators of
          HighDataCombinators -> wrap lam
          _ -> dApp consName [lam]
  case tree of
    TreeThis1 a body -> withTy "This1" [bind a] body
    TreeThis2 a b body -> withTy "This2" [bind a, bind b] body
    TreeThis3 a b c body -> withTy "This3" [bind a, bind b, bind c] body
    TreeThis4 a b c d body -> withTy "This4" [bind a, bind b, bind c, bind d] body
    TreeThis5 a b c d e body -> withTy "This5" [bind a, bind b, bind c, bind d, bind e] body
    TreeThis6 a b c d e f body -> withTy "This6" [bind a, bind b, bind c, bind d, bind e, bind f] body
 where
  dTuple :: [Doc] -> Doc
  dTuple = \case
    [one] -> one
    many -> "(" <> dintercalate ", " many <> ")"

showTreeWithThisAbility ::
  String -> Tree (WithThisAbility zone ot) -> EnvM Doc
showTreeWithThisAbility memo = \case
  TreeWithThisActivated withThis ->
    -- The @WithThisActivated@ body is an @ElectOT@-wrapped @Elect@ (see
    -- @activated'@); emit the @ElectOT@ constructor rather than hiding it, so the
    -- rendered @thisObject \\this -> ElectOT $ ...@ type-checks.
    app "WithThisActivated" [showTreeWithThis (\case TreeElectOT e -> fmap (\d -> dApp "ElectOT" [d]) (showTreeElect e)) memo withThis]
  TreeWithThisStatic withThis ->
    app "WithThisStatic" [showTreeWithThis showTreeStaticAbility memo withThis]
  TreeWithThisTriggered withThis ->
    app "WithThisTriggered" [showTreeWithThis showTreeTriggeredAbility memo withThis]

-- | Render @SomeZone (WithThisAbility ...)@ as the authoring combinator it came
-- from: @activated \@zone@ / @static \@zone@ / @triggered \@zone@ (see
-- 'MtgPure.Model.Combinators'). The @SomeZone@ + @thisObject@ + (for activated)
-- @ElectOT@ desugaring is folded away to match how the cards are written. The
-- @\@zone@ is always emitted: an ability's zone is frequently not inferable
-- (e.g. an ability with no target to pin it, like a mana ability). (The tuple
-- @SomeZone2@ form cannot hold a @WithThisAbility@, so it does not arise here.)
--
-- Under (F) below 'HighDataCombinators' the desugaring is spelled out
-- instead: @SomeZone (WithThisActivated (This1 \\(this :: ZO ...) -> ...))@,
-- with the zone pinned by the annotated 'This1' binder rather than @\@zone@.
showTreeSomeZoneWithThisAbility ::
  forall ot.
  String ->
  Tree (SomeZone WithThisAbility ot) ->
  EnvM Doc
showTreeSomeZoneWithThisAbility memo tree = do
  combinators <- getDataCombinators
  case tree of
    TreeSomeZone x -> case combinators of
      HighDataCombinators -> ability x
      _ -> app "SomeZone" [showTreeWithThisAbility memo x]
 where
  ability :: forall zone. (IsZO zone ot) => Tree (WithThisAbility zone ot) -> EnvM Doc
  ability = \case
    TreeWithThisActivated withThis ->
      -- @activated@ wraps the body in @ElectOT@ itself, so show the inner @Elect@.
      showTreeWithThisWrapped (comb "activated") (\case TreeElectOT e -> showTreeElect e) memo withThis
    TreeWithThisStatic withThis ->
      showTreeWithThisWrapped (comb "static") showTreeStaticAbility memo withThis
    TreeWithThisTriggered withThis ->
      showTreeWithThisWrapped (comb "triggered") showTreeTriggeredAbility memo withThis
   where
    comb :: String -> Doc -> Doc
    comb name lam = dTypeApps name [DString ('\'' : show (litZone @zone))] [lam]

showTreeWithThisZ ::
  forall liftZOT zone ot.
  (forall ot'. Tree (liftZOT zone ot') -> EnvM Doc) ->
  String ->
  Tree (WithThisZ liftZOT zone ot) ->
  EnvM Doc
showTreeWithThisZ showM memo = \case
  TreeWithThisZ withThis -> app "WithThisZ" [showTreeWithThis showM memo withThis]
