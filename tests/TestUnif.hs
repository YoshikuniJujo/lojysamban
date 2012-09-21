module TestUnif (
	unif
) where

import Test.HUnit
import Unif

unif = "test of Unif" ~: test [
	merge [] [] ~?= Just ([] :: Result String String),
	merge unif_1a unif_1b ~?= unif_1r ]

unif_1a = [([Var "" "X", Var "" "Y"], Nothing)]
unif_1b = [([Var "" "X"], Just $ Con "x")]
unif_1r = Just [([Var "" "X", Var "" "Y"], Just (Con "x"))]
