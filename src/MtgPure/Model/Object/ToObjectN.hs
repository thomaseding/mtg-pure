{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.ToObjectN (
  module MtgPure.Model.Object.ToObjectN.Classes,
  toObjectNAny,
) where

import safe MtgPure.Model.Object.OT (OT (..))
import safe MtgPure.Model.Object.OTNAliases (OTNAny)
import safe MtgPure.Model.Object.ObjectN (ObjectN)
import safe MtgPure.Model.Object.ToObjectN.Classes
import safe MtgPure.Model.Object.ToObjectN.Instances ()

toObjectNAny ::
  ToObject12
    ot
    'OTActivatedAbility
    'OTArtifact
    'OTCreature
    'OTEmblem
    'OTEnchantment
    'OTInstant
    'OTLand
    'OTPlaneswalker
    'OTPlayer
    'OTSorcery
    'OTStaticAbility
    'OTTriggeredAbility =>
  ObjectN ot ->
  ObjectN OTNAny
toObjectNAny = toObject12
