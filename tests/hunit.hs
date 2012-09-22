module Main where

import Test.HUnit
import TestLojysambanLib
import TestProlog2
import TestUnif

main = runTestTT $ test [lojysambanLib, prolog2, unif]
