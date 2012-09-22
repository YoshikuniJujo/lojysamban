module TestUnif (unif) where

import Test.HUnit
import Unif

unif = "test of Unif" ~: test [testMerge, testUnification, testUnify]

testMerge = "test of merge" ~: test [
	merge [] [] ~?= Just ([] :: Result String String),
	merge merge_1a merge_1b ~?= merge_1r ]

testUnification = "test of unification" ~: test [
	unification unif_1a unif_1b1 ~?= unif_1r1,
	unification unif_1a unif_1b2 ~?= Nothing,
	unification unif_1a unif_1b3 ~?= unif_1r3 ]

testUnify = "test of unify" ~: test [
	unify vx vx ~?= Just [],
	unify cx cx ~?= Just ([] :: [(Term String String, Term String String)]),
	unify vx cx ~?= Just [(vx, cx)]
 ]

merge_1a = [([Var "" "X", Var "" "Y"], Nothing)]
merge_1b = [([Var "" "X"], Just $ Con "x")]
merge_1r = Just [([Var "" "X", Var "" "Y"], Just (Con "x"))]

vx = Var "" "X"
vy = Var "" "Y"
cx = Con "x"
cy = Con "y"

unif_1a = [vx, vx]
unif_1b1 = [cx, cx]
unif_1r1 = Just [([vx], Just cx)]
unif_1b2 = [cx, cy]
unif_1b3 = [cx, vy]
unif_1r3 = Just [([vx, vy], Just cx)]
