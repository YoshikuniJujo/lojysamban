module TestUnif (unif) where

import Test.HUnit
import Unif

unif = "test of Unif" ~: test [testMerge, testUnification]

testMerge = "test of merge" ~: test [
	merge [] [] ~?= Just ([] :: Result String String),
	merge merge_1a merge_1b ~?= merge_1r,
	merge merge_2a merge_2b ~?= merge_2r,
	merge merge_3a merge_3b ~?= merge_3r
 ]

testUnification = "test of unification" ~: test [
	unification unif_1a unif_1b1 ~?= unif_1r1,
	unification unif_1a unif_1b2 ~?= Nothing,
	unification unif_1a unif_1b3 ~?= unif_1r3 ]

{-
testUnify = "test of unify" ~: test [
	unify vx vx ~?= Just (vx, []),
	unify cx cx ~?= Just (cx, ([] :: Result String String)),
	unify vx cx ~?= Just (cx, [([vx], Just cx)])
 ]
-}

merge_1a = [([vx, vy], Nothing)]
merge_1b = [([vx], Just cx)]
merge_1r = Just [([vx, vy], Just cx)]

merge_2a = [([vx, vy], Nothing)]
merge_2b = [([vx, vz], Nothing)]
merge_2r = Just [([vx, vy, vz], Nothing)]

merge_3a = [([vx], Just cx), ([vz], Just cz)]
merge_3b = [([vy], Just cy), ([vz], Just cz)]
merge_3r = Just [([vz], Just cz), ([vx], Just cx), ([vy], Just cy)]

vx = Var "" "X"
vy = Var "" "Y"
vz = Var "" "Z"
cx = Con "x"
cy = Con "y"
cz = Con "z"

unif_1a = [vx, vx]
unif_1b1 = [cx, cx]
unif_1r1 = Just [([vx], Just cx)]
unif_1b2 = [cx, cy]
unif_1b3 = [cx, vy]
unif_1r3 = Just [([vx, vy], Just cx)]
