{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Mana.Snow (
  Snow (..),
  IsSnow (..),
) where

import safe Data.Typeable (Typeable)

data Snow = Snow | NonSnow
  deriving (Typeable)

class (Typeable snow) => IsSnow (snow :: Snow) where
  litSnow :: Snow

instance IsSnow 'Snow where
  litSnow = Snow

instance IsSnow 'NonSnow where
  litSnow = NonSnow
