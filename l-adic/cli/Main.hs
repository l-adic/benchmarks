{-# LANGUAGE TemplateHaskell #-}

module Main where

import Circom.CLI (defaultMain)
import Circuit (BN128)
import Protolude
import ZK.LargeMult (largeMult)

main :: IO ()
main = defaultMain "large-mult" $ largeMult @BN128 1_000_000
