module ZK.LargeMult
  ( largeMult,
    multNums
  )
where

import Circuit
import Circuit.Language
import Data.Field.Galois (GaloisField)
import Protolude
import GHC.Natural (Natural)
import qualified Data.Vector as V

largeMult ::
  forall f.
  (GaloisField f) =>
  (Hashable f) =>
  Natural ->
  ExprM f (Var Wire f 'TField)
largeMult n = do
  start <- var_ <$> fieldInput Public "start"
  let p = multNums n start
  fieldOutput "out" p

multNums :: Num a => Natural -> a -> a
multNums n start = product $ V.generate (fromIntegral n) $ \i ->
  start + fromIntegral i