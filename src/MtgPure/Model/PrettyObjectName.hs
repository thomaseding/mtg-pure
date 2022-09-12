{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module MtgPure.Model.PrettyObjectName
  ( PrettyObjectName (..),
  )
where

import Data.Inst
  ( Inst2,
    Inst3,
    Inst4,
    Inst5,
  )
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (typeRep)
import MtgPure.Model.IsObjectType (IsObjectType)
import MtgPure.Model.Object (Object)
import MtgPure.Model.ObjectN
  ( OCreaturePlaneswalker,
    OCreaturePlayer,
    OCreaturePlayerPlaneswalker,
    OPermanent,
    OPlayerPlaneswalker,
    ObjectN (..),
  )

class PrettyObjectName a where
  prettyObjectName :: Proxy a -> String

instance IsObjectType a => PrettyObjectName (Object a) where
  prettyObjectName = ('O' :) . drop prefix . show . typeRep
    where
      prefix = length "Object 'OT"

instance Inst2 IsObjectType a b => PrettyObjectName (ObjectN '(a, b)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OCreaturePlaneswalker) -> "OCreaturePlaneswalker"
        | rep == typeRep (Proxy @OCreaturePlayer) -> "OCreaturePlayer"
        | rep == typeRep (Proxy @OPlayerPlaneswalker) -> "OPlayerPlaneswalker"
        | otherwise ->
          "ObjectN '( "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ")"
    where
      rep = typeRep proxy

instance Inst3 IsObjectType a b c => PrettyObjectName (ObjectN '(a, b, c)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OCreaturePlayerPlaneswalker) -> "OCreaturePlayerPlaneswalker"
        | otherwise ->
          "ObjectN '( "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ")"
    where
      rep = typeRep proxy

instance Inst4 IsObjectType a b c d => PrettyObjectName (ObjectN '(a, b, c, d)) where
  prettyObjectName proxy =
    if
        | otherwise ->
          "ObjectN '( "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ")"
    where
      _rep = typeRep proxy

instance Inst5 IsObjectType a b c d e => PrettyObjectName (ObjectN '(a, b, c, d, e)) where
  prettyObjectName proxy =
    if
        | rep == typeRep (Proxy @OPermanent) -> "OPermanent"
        | otherwise ->
          "ObjectN '( "
            ++ show (typeRep (Proxy @a))
            ++ ", "
            ++ show (typeRep (Proxy @b))
            ++ ", "
            ++ show (typeRep (Proxy @c))
            ++ ", "
            ++ show (typeRep (Proxy @d))
            ++ ", "
            ++ show (typeRep (Proxy @e))
            ++ ")"
    where
      rep = typeRep proxy
