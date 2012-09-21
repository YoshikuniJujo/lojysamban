module Main where
import Test.HUnit

import TestUnif
import TestProlog2

main = runTestTT $ test [unif, prolog2]
