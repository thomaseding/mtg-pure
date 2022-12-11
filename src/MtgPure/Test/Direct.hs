{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use Lit" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant pure" #-}

-- https://github.com/stoeffel/haskell-simple-dsl-examples/blob/master/ChatGADT.hs
module MtgPure.Test.Direct (
  module MtgPure.Test.Direct,
) where

import safe qualified Control.Monad.State.Strict as State
import safe Data.IORef (modifyIORef', newIORef, readIORef)
import safe Data.Kind (Type)
import safe qualified Data.Map.Strict as Map
import safe MtgPure.Test.Variabled (Env (..), EnvM, EnvShow (..), RS, Var (Lit, Var), VarID, runEnvM)
import safe qualified MtgPure.Test.VariabledMonad as X

data InterpM (dsl :: Type -> Type -> Type) (s :: Type) (a :: Type) :: Type where
  Interp :: dsl s a -> InterpM dsl s a
  Pure :: a -> InterpM dsl s a
  Then :: RS a => InterpM dsl s a -> InterpM dsl s b -> InterpM dsl s b
  Bind :: RS a => InterpM dsl s (Var s a) -> (Var s a -> InterpM dsl s b) -> InterpM dsl s b

instance X.Monad s (InterpM dsl) where
  pure = Pure
  (>>) = Then
  (>>=) = Bind

--https://www.reddit.com/r/haskell/comments/gxcxgl/comment/ftatasa/?utm_source=share&utm_medium=web2x&context=3
newtype I dsl s m = I {unI :: forall x. dsl s x -> m x}

--interpret :: Monad m => (forall s x. (dsl s x -> m x), InterpM dsl s a) -> m a
--interpret :: Monad m => (forall s x. (dsl s x -> m x, InterpM dsl s a)) -> m a
interpret :: Monad m => (forall s. (I dsl s m, InterpM dsl s a)) -> m a
interpret (go, m) = interpret' (unI go) m

interpret' :: Monad m => (forall x. dsl s x -> m x) -> InterpM dsl s a -> m a
interpret' go = \case
  Interp dsl -> go dsl
  Pure x -> pure x
  Then x y -> interpret' go x >> interpret' go y
  Bind x f -> interpret' go x >>= interpret' go . f

data StoreF (f :: Type -> Type) (a :: Type) :: Type where
  Get :: f String -> StoreF f (f String)
  Set :: f String -> f String -> StoreF f ()
  Print :: f String -> StoreF f ()

type StoreVar s a = StoreF (Var s) a

instance EnvShow (StoreVar s a) where
  envShow x = case x of
    Get key -> do
      sKey <- envShow key
      pure $ "Get (" <> sKey <> ")"
    Set key val -> do
      sKey <- envShow key
      sVal <- envShow val
      pure $ "Set (" <> sKey <> ") (" <> sVal <> ")"
    Print var -> do
      sVar <- envShow var
      pure $ "Print (" <> sVar <> ")"

instance Show (StoreVar s a) where
  show = runEnvM . envShow

newtype Store s a = Store {unStore :: StoreF (Var s) a}

type StoreM s = InterpM Store s

liftStore :: StoreVar s a -> StoreM s a
liftStore = Interp . Store

newVar :: EnvM (Var s a)
newVar = do
  i <- State.gets envVarID
  State.modify' \st -> st{envVarID = i + 1}
  pure $ Var i

instance EnvShow (Store s a) where
  envShow = \case
    Store store -> do
      sStore <- envShow store
      pure $ "Store (" <> sStore <> ")"

instance Show (Store s a) where
  show = runEnvM . envShow

instance EnvShow a => EnvShow (StoreM s a) where
  envShow = \case
    Interp dsl -> do
      sDsl <- envShow dsl
      pure $ "Interp (" <> sDsl <> ")"
    Pure a -> do
      sa <- envShow a
      pure $ "Pure (" <> sa <> ")"
    Then ma mb -> do
      sma <- envShow ma
      smb <- envShow mb
      pure $ "Then (" <> sma <> ") (" <> smb <> ")"
    Bind ma f -> do
      sma <- envShow ma
      va <- newVar
      sva <- envShow va
      let mb = f va
      smb <- envShow mb
      let sf = "\\" <> sva <> " -> " <> smb
      pure $ "Bind (" <> sma <> ") (" <> sf <> ")"

instance EnvShow a => Show (StoreM s a) where
  show = runEnvM . envShow

indirection :: Var s String -> Var s String -> StoreM s ()
indirection key val =
  liftStore (Set (Lit "x") (Lit "hello"))
    X.>> liftStore (Set (Lit "y") (Lit "world"))
    X.>> liftStore (Set (Lit "hello") (Lit "good"))
    X.>> liftStore (Set (Lit "world") (Lit "riddance"))
    X.>> liftStore (Get key) X.>>= \key' ->
      liftStore (Set key' val)
        X.>> liftStore (Get (Lit "x")) X.>>= \x ->
          liftStore (Get (Lit "y")) X.>>= \y ->
            liftStore (Get (Lit "hello")) X.>>= \hello ->
              liftStore (Get (Lit "world")) X.>>= \world ->
                liftStore (Print x)
                  X.>> liftStore (Print y)
                  X.>> liftStore (Print hello)
                  X.>> liftStore (Print world)

dsl2 :: StoreM s (Var s String)
dsl2 = Pure (Lit "hello") X.>>= \x -> X.pure x

interpretStore :: (forall s. StoreM s a) -> IO a
interpretStore dsl = do
  varMapRef <- newIORef (mempty :: Map.Map VarID String)
  s2sRef <- newIORef (mempty :: Map.Map String String)

  let readVar :: Var s String -> IO String
      readVar = \case
        Lit x -> pure x
        Var vid -> readVarID vid

      readVarID :: VarID -> IO String
      readVarID vid = do
        varMap <- readIORef varMapRef
        case Map.lookup vid varMap of
          Nothing -> error "internal logic error... forgot to hide Var constructor?"
          Just x -> pure x

      go :: Store s x -> IO x
      go (Store store) = case store of
        Get keyVar -> do
          varMap <- readIORef varMapRef
          s2s <- readIORef s2sRef
          key <- readVar keyVar
          let val = Map.findWithDefault "EMPTY" key s2s
              vid = Map.size varMap
          modifyIORef' varMapRef $ Map.insert vid val
          pure $ Var vid
        Set keyVar valVar -> do
          key <- readVar keyVar
          val <- readVar valVar
          modifyIORef' s2sRef $ Map.insert key val
        Print var -> case var of
          Lit x -> print ("Lit" :: String, show x)
          Var vid -> do
            x <- readVarID vid
            print ("Var" :: String, vid, show x)

  interpret (I go, dsl)

test :: IO ()
test = interpretStore $ indirection (Lit "x") (Lit "magic")
