module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Web3Spec.Types.Sha3 (sha3Spec)
import Web3Spec.Types.Utils (utilsSpec)
import Web3Spec.Types.BigNumber (bigNumberSpec)
import Web3Spec.Encoding.Single (encodingSpec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  sha3Spec
  utilsSpec
  bigNumberSpec
  encodingSpec

