{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestProlog2 (prolog2) where

import Test.HUnit
import Prolog2
import Unif

prolog2 = "test of Prolog2" ~: test [
	ask "" [] fact1 rules ~?= [[]],
	ask "" [] q1 rules ~?= [],
	ask "" [] q2 rules ~?= [prolog2_result1],
	ask "" [] patfuQ1 patfuRules ~?= patfuResult1 ]

prolog2_result1 = [
	([Var "dnnnd" "Z"], Just (Con "cheese")),
	([Var "dnnnd" "X"], Just (Con "wallace")),
	([Var "dnnnd" "Y"], Just (Con "grommit"))]

patfuResult1 = [[
	([Var "dnnd" "Q"], Just $ Con "jon.bois.jr."),
	([Var "dnn" "Who", Var "dnnd" "P"], Just $ Con "jon.bois.sr.")], [
	([Var "dnnnd" "Y", Var "dnnndndnnd" "Q"], Just $ Con "jon.bois.jr."),
	([Var "dnnn" "Who", Var "dnnnd" "X"], Just (Con "zeb")),
	([Var "dnnnd" "Z", Var "dnnndndnnd" "P"], Just $ Con "jon.bois.sr.")
 ]]

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
rule4s = Rule (\sc -> [Con "friends", Var sc "X", Var sc "Y"]) []
	[fact4, fact5] []
rule5 = Rule fact6 [] [] []
rules = [rule1, rule2, rule3, rule4, rule5]

q1 = const [Con "friends", Con "wallace", Con "wallace"]
q2 = const [Con "friends", Con "wallace", Con "grommit"]

patfuF1 sc = [Con "patfu", Con "zeb", Con "jon.bois.sr."]
patfuF2 sc = [Con "patfu", Con "jon.bois.sr.", Con "jon.bois.jr."]
patfuF3 sc = [Con "dzena", Var sc "P", Var sc "Q"]
patfuF4 sc = [Con "patfu", Var sc "P", Var sc "Q"]
patfuF5 sc = [Con "dzena", Var sc "X", Var sc "Y"]
patfuF6 sc = [Con "patfu", Var sc "X", Var sc "Z"]
patfuF7 sc = [Con "dzena", Var sc "Z", Var sc "Y"]

patfuR1 = Rule patfuF1 [] [] []
patfuR2 = Rule patfuF2 [] [] []
patfuR3 = Rule patfuF3 [] [patfuF4] []
patfuR4 = Rule patfuF5 [] [patfuF6, patfuF7] []

patfuQ1 sc = [Con "dzena", Var sc "Who", Con "jon.bois.jr."]

patfuRules = [patfuR1, patfuR2, patfuR3, patfuR4]
