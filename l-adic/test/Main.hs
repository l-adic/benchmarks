module Main (main) where

import Circom.R1CS (witnessFromCircomWitness)
import Circom.Solver (CircomProgram (..), mkCircomProgram, nativeGenWitness)
import Circuit
import Circuit.Language
import qualified Data.Map as Map
import qualified Data.Vector.Sized as V
import GHC.Natural (Natural)
import GHC.TypeNats (SNat, withKnownNat, withSomeSNat)
import Protolude
import R1CS (Witness (..))
import Test.Hspec
import Test.QuickCheck
import ZK.LargeMult (largeMult)

main :: IO ()
main = hspec $ do
  describe "LargeMult" $ do
    it "should compute large products" $
      property $
        \a (ArbNatural nat) ->
          withSomeSNat nat $ \(snat :: SNat n) ->
            withKnownNat snat $
              let BuilderState {bsVars, bsCircuit} = snd $ runCircuitBuilder (largeMult @BN128 snat)
                  program = mkCircomProgram bsVars bsCircuit
                  inputs = Map.fromList [("start", Simple a)]
                  expected = product $ V.generate @n $ \i ->
                    a + fromIntegral i
                  Witness w = witnessFromCircomWitness $ nativeGenWitness program inputs
               in lookupVar (cpVars program) "out" w === Just expected

newtype ArbNatural = ArbNatural Natural deriving (Show)

instance Arbitrary ArbNatural where
  arbitrary = ArbNatural . fromIntegral @Int . abs <$> arbitrary
