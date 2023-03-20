{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use const" #-}

module MtgPure.Model.Object.MapObjectN (
  mapObjectN,
) where

import safe MtgPure.Model.Object.IsObjectType (IsObjectType (singObjectType))
import safe MtgPure.Model.Object.OT (OT (OTLand))
import safe MtgPure.Model.Object.Object (Object (..))
import safe MtgPure.Model.Object.ObjectId (UntypedObject (..))
import safe MtgPure.Model.Object.ObjectN (ObjectN (..))

type OTArbitrary = 'OTLand

mapObjectN :: (forall a. IsObjectType a => Object a -> Object a) -> ObjectN ot -> ObjectN ot
mapObjectN f = \case
  O0 (UntypedObject d i) ->
    let o = Object (singObjectType @OTArbitrary) (UntypedObject d i)
        o' = f o
        Object _ (UntypedObject _ i') = o'
     in O0 (UntypedObject d i')
  --
  O1 x -> O1 $ f x
  --
  O2a x -> O2a $ f x
  O2b x -> O2b $ f x
  ON2a x -> ON2a $ mapObjectN f x
  ON2b x -> ON2b $ mapObjectN f x
  --
  O3a x -> O3a $ f x
  O3b x -> O3b $ f x
  O3c x -> O3c $ f x
  ON3a x -> ON3a $ mapObjectN f x
  ON3b x -> ON3b $ mapObjectN f x
  ON3c x -> ON3c $ mapObjectN f x
  --
  O4a x -> O4a $ f x
  O4b x -> O4b $ f x
  O4c x -> O4c $ f x
  O4d x -> O4d $ f x
  ON4a x -> ON4a $ mapObjectN f x
  ON4b x -> ON4b $ mapObjectN f x
  ON4c x -> ON4c $ mapObjectN f x
  ON4d x -> ON4d $ mapObjectN f x
  --
  O5a x -> O5a $ f x
  O5b x -> O5b $ f x
  O5c x -> O5c $ f x
  O5d x -> O5d $ f x
  O5e x -> O5e $ f x
  ON5a x -> ON5a $ mapObjectN f x
  ON5b x -> ON5b $ mapObjectN f x
  ON5c x -> ON5c $ mapObjectN f x
  ON5d x -> ON5d $ mapObjectN f x
  ON5e x -> ON5e $ mapObjectN f x
  --
  O6a x -> O6a $ f x
  O6b x -> O6b $ f x
  O6c x -> O6c $ f x
  O6d x -> O6d $ f x
  O6e x -> O6e $ f x
  O6f x -> O6f $ f x
  ON6a x -> ON6a $ mapObjectN f x
  ON6b x -> ON6b $ mapObjectN f x
  ON6c x -> ON6c $ mapObjectN f x
  ON6d x -> ON6d $ mapObjectN f x
  ON6e x -> ON6e $ mapObjectN f x
  ON6f x -> ON6f $ mapObjectN f x
  --
  O7a x -> O7a $ f x
  O7b x -> O7b $ f x
  O7c x -> O7c $ f x
  O7d x -> O7d $ f x
  O7e x -> O7e $ f x
  O7f x -> O7f $ f x
  O7g x -> O7g $ f x
  ON7a x -> ON7a $ mapObjectN f x
  ON7b x -> ON7b $ mapObjectN f x
  ON7c x -> ON7c $ mapObjectN f x
  ON7d x -> ON7d $ mapObjectN f x
  ON7e x -> ON7e $ mapObjectN f x
  ON7f x -> ON7f $ mapObjectN f x
  ON7g x -> ON7g $ mapObjectN f x
  --
  O8a x -> O8a $ f x
  O8b x -> O8b $ f x
  O8c x -> O8c $ f x
  O8d x -> O8d $ f x
  O8e x -> O8e $ f x
  O8f x -> O8f $ f x
  O8g x -> O8g $ f x
  O8h x -> O8h $ f x
  ON8a x -> ON8a $ mapObjectN f x
  ON8b x -> ON8b $ mapObjectN f x
  ON8c x -> ON8c $ mapObjectN f x
  ON8d x -> ON8d $ mapObjectN f x
  ON8e x -> ON8e $ mapObjectN f x
  ON8f x -> ON8f $ mapObjectN f x
  ON8g x -> ON8g $ mapObjectN f x
  ON8h x -> ON8h $ mapObjectN f x
  --
  O9a x -> O9a $ f x
  O9b x -> O9b $ f x
  O9c x -> O9c $ f x
  O9d x -> O9d $ f x
  O9e x -> O9e $ f x
  O9f x -> O9f $ f x
  O9g x -> O9g $ f x
  O9h x -> O9h $ f x
  O9i x -> O9i $ f x
  ON9a x -> ON9a $ mapObjectN f x
  ON9b x -> ON9b $ mapObjectN f x
  ON9c x -> ON9c $ mapObjectN f x
  ON9d x -> ON9d $ mapObjectN f x
  ON9e x -> ON9e $ mapObjectN f x
  ON9f x -> ON9f $ mapObjectN f x
  ON9g x -> ON9g $ mapObjectN f x
  ON9h x -> ON9h $ mapObjectN f x
  ON9i x -> ON9i $ mapObjectN f x
  --
  O10a x -> O10a $ f x
  O10b x -> O10b $ f x
  O10c x -> O10c $ f x
  O10d x -> O10d $ f x
  O10e x -> O10e $ f x
  O10f x -> O10f $ f x
  O10g x -> O10g $ f x
  O10h x -> O10h $ f x
  O10i x -> O10i $ f x
  O10j x -> O10j $ f x
  ON10a x -> ON10a $ mapObjectN f x
  ON10b x -> ON10b $ mapObjectN f x
  ON10c x -> ON10c $ mapObjectN f x
  ON10d x -> ON10d $ mapObjectN f x
  ON10e x -> ON10e $ mapObjectN f x
  ON10f x -> ON10f $ mapObjectN f x
  ON10g x -> ON10g $ mapObjectN f x
  ON10h x -> ON10h $ mapObjectN f x
  ON10i x -> ON10i $ mapObjectN f x
  ON10j x -> ON10j $ mapObjectN f x
  --
  O11a x -> O11a $ f x
  O11b x -> O11b $ f x
  O11c x -> O11c $ f x
  O11d x -> O11d $ f x
  O11e x -> O11e $ f x
  O11f x -> O11f $ f x
  O11g x -> O11g $ f x
  O11h x -> O11h $ f x
  O11i x -> O11i $ f x
  O11j x -> O11j $ f x
  O11k x -> O11k $ f x
  ON11a x -> ON11a $ mapObjectN f x
  ON11b x -> ON11b $ mapObjectN f x
  ON11c x -> ON11c $ mapObjectN f x
  ON11d x -> ON11d $ mapObjectN f x
  ON11e x -> ON11e $ mapObjectN f x
  ON11f x -> ON11f $ mapObjectN f x
  ON11g x -> ON11g $ mapObjectN f x
  ON11h x -> ON11h $ mapObjectN f x
  ON11i x -> ON11i $ mapObjectN f x
  ON11j x -> ON11j $ mapObjectN f x
  ON11k x -> ON11k $ mapObjectN f x
  --
  O12a x -> O12a $ f x
  O12b x -> O12b $ f x
  O12c x -> O12c $ f x
  O12d x -> O12d $ f x
  O12e x -> O12e $ f x
  O12f x -> O12f $ f x
  O12g x -> O12g $ f x
  O12h x -> O12h $ f x
  O12i x -> O12i $ f x
  O12j x -> O12j $ f x
  O12k x -> O12k $ f x
  O12l x -> O12l $ f x
  ON12a x -> ON12a $ mapObjectN f x
  ON12b x -> ON12b $ mapObjectN f x
  ON12c x -> ON12c $ mapObjectN f x
  ON12d x -> ON12d $ mapObjectN f x
  ON12e x -> ON12e $ mapObjectN f x
  ON12f x -> ON12f $ mapObjectN f x
  ON12g x -> ON12g $ mapObjectN f x
  ON12h x -> ON12h $ mapObjectN f x
  ON12i x -> ON12i $ mapObjectN f x
  ON12j x -> ON12j $ mapObjectN f x
  ON12k x -> ON12k $ mapObjectN f x
  ON12l x -> ON12l $ mapObjectN f x
