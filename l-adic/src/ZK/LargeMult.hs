module ZK.LargeMult
  ( largeMult,
  )
where

import Circuit
import Circuit.Language
import Data.Field.Galois (GaloisField)
import Data.Vector.Sized qualified as V
import Protolude

largeMult ::
  forall f n proxy.
  (GaloisField f) =>
  (Hashable f) =>
  (KnownNat n) =>
  proxy n ->
  ExprM f (Var Wire f 'TField)
largeMult _ = do
  start <- var_ <$> fieldInput Public "start"
  let p = product $ V.generate @n $ \i ->
        start + fromIntegral i
  fieldOutput "out" p
