module TestUnif (unif) where

import Test.HUnit
import Unif

unif = "test of Unif" ~: test [
	merge [] [] ~?= Just ([] :: Result String String),
	merge merge_1a merge_1b ~?= merge_1r,
	unification unif_1a unif_1b1 ~?= unif_1r1,
	unification unif_1a unif_1b2 ~?= Nothing,
	unification unif_1a unif_1b3 ~?= unif_1r3
 ]

merge_1a = [([Var "" "X", Var "" "Y"], Nothing)]
merge_1b = [([Var "" "X"], Just $ Con "x")]
merge_1r = Just [([Var "" "X", Var "" "Y"], Just (Con "x"))]

unif_1a = [Var "" "X", Var "" "X"]
unif_1b1 = [Con "x", Con "x"]
unif_1r1 = Just [([Var "" "X"], Just $ Con "x")]
unif_1b2 = [Con "x", Con "y"]
unif_1b3 = [Con "x", Var "" "Y"]
unif_1r3 = Just [([Var "" "X", Var "" "Y"], Just $ Con "x")]
