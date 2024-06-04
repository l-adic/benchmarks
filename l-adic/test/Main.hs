module Main (main) where

import Circom.R1CS (witnessFromCircomWitness)
import Circom.Solver (CircomProgram (..), mkCircomProgram, nativeGenWitness)
import Circuit
import Circuit.Language
import qualified Data.Map as Map
import GHC.Natural (Natural)
import Protolude
import R1CS (Witness (..))
import Test.Hspec
import Test.QuickCheck
import ZK.LargeMult (largeMult, multNums)

main :: IO ()
main = hspec $ do
  describe "LargeMult" $ do
    it "should accept valid factorizations" $
      property $
        \a (ArbNatural nat) ->
              let BuilderState {bsVars, bsCircuit} = snd $ runCircuitBuilder (largeMult @BN128 nat)
                  program = mkCircomProgram bsVars bsCircuit
                  inputs = Map.fromList [("start", Simple a)]
                  expected = multNums nat a
                  Witness w = witnessFromCircomWitness $ nativeGenWitness program inputs
               in lookupVar (cpVars program) "out" w === Just expected

newtype ArbNatural = ArbNatural Natural deriving (Show)

instance Arbitrary ArbNatural where
  arbitrary = ArbNatural . fromIntegral @Int . (+ 1) . abs <$> arbitrary