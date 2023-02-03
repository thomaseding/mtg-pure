{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.PrePost (
  PrePost (..),
  SPrePost (..),
  IsPrePost (..),
) where

import safe Data.Kind (Type)
import safe Data.Typeable (Typeable)

data PrePost
  = Pre
  | Post
  deriving (Eq, Ord, Show, Typeable)

data SPrePost (p :: PrePost) :: Type where
  SPre :: SPrePost 'Pre
  SPost :: SPrePost 'Post
  deriving (Typeable)

class Typeable p => IsPrePost p where
  singPrePost :: SPrePost p

instance IsPrePost 'Pre where
  singPrePost = SPre

instance IsPrePost 'Post where
  singPrePost = SPost
