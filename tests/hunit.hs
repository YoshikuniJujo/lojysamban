{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import Test.HUnit

import Unif
import Prolog2

instance TwoD String where
	next = (++ "n")
	down = (++ "d")

fact1 = const [Con "likes", Con "wallace", Con "cheese"]
fact2 = const [Con "likes", Con "grommit", Con "cheese"]
fact3 = const [Con "likes", Con "wendolene", Con "sheep"]
fact4 sc = [Con "likes", Var sc "X", Var sc "Z"]
fact5 sc = [Con "likes", Var sc "Y", Var sc "Z"]
fact6 sc = [Con "du", Var sc "X", Var sc "X"]

rule1 = Rule fact1 [] [] []
rule2 = Rule fact2 [] [] []
rule3 = Rule fact3 [] [] []
rule4 = Rule (\sc -> [Con "friends", Var sc "X", Var sc "Y"]) []
	[fact4, fact5] [\sc -> [Con "du", Var sc "X", Var sc "Y"]]
rule5 = Rule fact6 [] [] []
rules = [rule1, rule2, rule3, rule4, rule5]

q1 = const [Con "friends", Con "wallace", Con "wallace"]
q2 = const [Con "friends", Con "wallace", Con "grommit"]

tests = test [
	merge [] [] ~?= Just ([] :: Result String String),
	merge [([Var "" "X", Var "" "Y"], Nothing)] [([Var "" "X"], Just $ Con "x")] ~?=
		Just [([Var "" "X", Var "" "Y"], Just (Con "x"))],
	ask "" [] fact1 rules ~?= [[]],
	ask "" [] q1 rules ~?= [],
	ask "" [] q2 rules ~?= [result1] ]

result1 = [
	([Var "dnnnd" "Z"], Just (Con "cheese")),
	([Var "dnnnd" "X"], Just (Con "wallace")),
	([Var "dnnnd" "Y"], Just (Con "grommit"))]
