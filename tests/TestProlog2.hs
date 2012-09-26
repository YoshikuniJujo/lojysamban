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
	ask "" [] patfuQ1 patfuRules ~?= patfuResult1,
	ask "" [] binxoQ2 binxoRules ~?= binxoR2,
	ask "" [] applyQ1 applyRules ~?= applyR1,
	ask "" [] test2Q4 test2Rules ~?= test2R4 ]

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

binxoRule1 = Rule (\sc -> [Is, Var sc "Y", Con "x"]) [] [] []
duRule = Rule fact6 [] [] []
binxoQ1 sc = [Con "du", Var sc "What", Var sc "Y"]

binxoFact1 sc = [Is, Var sc "Z", Con "x"]
binxoRule2 = Rule (\sc -> [Con "fun", Var sc "Z"]) [] [binxoFact1] []
binxoRules = [binxoRule2, duRule, duRule]
binxoQ2 sc = [Con "fun", Var sc "What"]
binxoR2 = [[([Var "dd" "Z", Var "d" "What"], Just (Con "x"))]]

applyFact1 sc = [Is, Var sc "X", ApplyOp (++) (Con "x") (Con "y")]
applyRule1 = Rule (\sc -> [Con "fun", Var sc "X"]) [] [applyFact1] []
applyRules = [applyRule1]
applyQ1 sc = [Con "fun", Var sc "What"]
applyR1 = [[([Var "dd" "X", Var "d" "What"], Just (Con "xy"))]]

countFact0 sc = [Con "zilkancu", List [], Con ""]
countFact1 sc = [Con "zilkancu", Cons (Var sc "stedu") (Var sc "rebla"),
	Var sc "da"]
countFact2 sc = [Con "zilkancu", Var sc "rebla", Var sc "de"]
countFact3 sc = [Is, Var sc "da", ApplyOp (++) (Var sc "de") (Con ".")]
countRule1 = Rule countFact0 [] [] []
countRule2 = Rule countFact1 [] [countFact2, countFact3] []
countRules = [countRule1, countRule2]
countQ1 sc = [Con "zilkancu", List [], Var sc "ma"]
countQ2 sc = [Con "zilkancu", List [Con "a", Con "b"], Var sc "ma"]

-- broduFact00 sc = [Con "du", Var sc "da", Var sc "da"]
-- broduFact0 sc = [Con "brodu", Var sc "di"]
-- broduFact1 sc = [Con "du", Var sc "di", List [Var sc "da", Var sc "de"]]
-- broduFact2 sc = [Con 

brodeFact0 sc = [Con "brode", Con "pa"]
brodeFact1 sc = [Con "brode", Con "re"]
brodeFact2 sc = [Con "brode", Con "ci"]
brodeFact3 sc = [Con "brode", Con "vo"]
pavbonvihiFact0 sc = [Con "pavyvonbi'i", List []]
pavbonvihiFact1 sc = [Con "pavyvonbi'i", Cons (Var sc "da") (Var sc "de")]
pavbonvihiFact2 sc = [Con "brode", Var sc "da"]
pavbonvihiFact3 sc = [Con "pavyvonbi'i", Var sc "de"]
pavbonvihiRule0 = Rule brodeFact0 [] [] []
pavbonvihiRule1 = Rule brodeFact1 [] [] []
pavbonvihiRule2 = Rule brodeFact2 [] [] []
pavbonvihiRule3 = Rule brodeFact3 [] [] []
pavbonvihiRule4 = Rule pavbonvihiFact0 [] [] []
pavbonvihiRule5 = Rule pavbonvihiFact1 [] [pavbonvihiFact2, pavbonvihiFact3] []
pavbonvihiRules = [
	pavbonvihiRule0,
	pavbonvihiRule1,
	pavbonvihiRule2,
	pavbonvihiRule4,
	pavbonvihiRule5
 ]
pavbonvihiQ0 sc = [Con "pavyvonbi'i", List [Con "pa", Con "re"]]

brodaFact0 sc = [Con "broda", Var sc "da", List []]
brodaFact1 sc = [Con "broda", Var sc "da", Cons (Var sc "de") (Var sc "di")]
brodaFact2 sc = [Con "du", Var sc "da", Var sc "de"]
brodaFact3 sc = [Con "broda", Var sc "da", Var sc "di"]
brodaRule0 = Rule brodaFact0 [] [] []
brodaRule1 = Rule brodaFact1 [] [brodaFact3] [brodaFact2]
brodaRules = [brodaRule0, brodaRule1, duRule]
brodaQ0 sc = [Con "broda", Con "y", List [Con "x", Con "y", Con "x"]]
brodaQ1 sc = [Con "broda", Con "a", List [Con "x", Con "y", Con "z"]]

datsihuFact0 sc = [Con "datsi'u", List [Var sc "da", Var sc "de"]]
datsihuFact1 sc = [Con "datsi'u", Cons (Var sc "da") (Var sc "de")]
datsihuFact2 sc = [Con "broda", Var sc "da", Var sc "de"]
datsihuFact3 sc = [Con "datsi'u", Var sc "de"]
datsihuRule0 = Rule datsihuFact0 [] [] []
datsihuRule1 = Rule datsihuFact1 [] [datsihuFact2, datsihuFact3] []
datsihuRules = [datsihuRule0, datsihuRule1] ++ brodaRules
datsihuQ0 sc = [Con "datsi'u", List [Con "x", Con "y", Con "z"]]
datsihuQ1 sc = [Con "datsi'u", List [Con "x", Con "y", Con "x"]]

broduFact0 sc = [Con "brodu", List [Var sc "DA", Var sc "DE", Var sc "DI"]]
broduFact2 sc = [Con "pavyvonbi'i", List [Var sc "DA", Var sc "DE", Var sc "DI"]]
broduFact3 sc = [Con "datsi'u", List [Var sc "DA", Var sc "DE", Var sc "DI"]]
broduRule0 = Rule broduFact0 [] [broduFact2, broduFact3] []
broduRules = broduRule0 : pavbonvihiRules ++ datsihuRules
broduQ1 sc = [Con "brodu", Var sc "ma"]

test2Fact0 sc = [Con "drata", Con "pa", Con "re"]
test2Fact1 sc = [Con "drata", Con "pa", Con "ci"]
test2Fact2 sc = [Con "drata", Con "re", Con "pa"]
test2Fact3 sc = [Con "drata", Con "re", Con "ci"]
test2Fact4 sc = [Con "drata", Con "ci", Con "pa"]
test2Fact5 sc = [Con "drata", Con "ci", Con "re"]

test2Rule0 = Rule test2Fact0 [] [] []
test2Rule1 = Rule test2Fact1 [] [] []
test2Rule2 = Rule test2Fact2 [] [] []
test2Rule3 = Rule test2Fact3 [] [] []
test2Rule4 = Rule test2Fact4 [] [] []
test2Rule5 = Rule test2Fact5 [] [] []

test2Fact6 sc = [Con "broda", Var sc "da", List []]
test2Fact7 sc = [Con "broda", Var sc "da",  Cons (Var sc "de") (Var sc "di")]
test2Fact8 sc = [Con "drata", Var sc "da", Var sc "de"]
test2Fact9 sc = [Con "broda", Var sc "da", Var sc "di"]

test2Rule6 = Rule test2Fact6 [] [] []
test2Rule7 = Rule test2Fact7 [] [test2Fact8, test2Fact9] []

test2Fact10 sc = [Con "datsi'u", List []]
test2Fact11 sc = [Con "datsi'u", Cons (Var sc "DA") (Var sc "DE")]
test2Fact12 sc = [Con "broda", Var sc "DA", Var sc "DE"]
test2Fact13 sc = [Con "datsi'u", Var sc "DE"]

test2Rule8 = Rule test2Fact10 [] [] []
test2Rule9 = Rule test2Fact11 [] [test2Fact12, test2Fact13] []

test2Fact14 sc = [Con "brode", List [Var sc "da", Var sc "de", Var sc "di"]]
test2Fact15 sc = [Con "datsi'u", List [Var sc "da", Var sc "de", Var sc "di"]]

test2Rule10 = Rule test2Fact14 [] [test2Fact15] []

test2Rules = [
	test2Rule0,
	test2Rule1,
	test2Rule2,
	test2Rule3,
	test2Rule4,
	test2Rule5,
	test2Rule6,
	test2Rule7,
	test2Rule8,
	test2Rule9,
	test2Rule10 ]
test2Q0 sc = [Con "brode", Var sc "ma"]
test2Q1 sc = [Con "broda", Con "ci", List [Con "pa", Con "re", Con "re"]]
test2Q2 sc = [Con "drata", Con "ci", Con "pa"]
test2Q3 sc = [Con "broda", Con "ci", List []]
test2Q4 sc = [Con "datsi'u", List [Con "pa", Con "re", Con "ci"]]
test2R4 = [[
 ([Var "dnnnnnnnnndndnnnnnnnnnddnnnnnnnd" "di",
	Var "dnnnnnnnnnddnnnnnnndndnnnnnnnd" "di",
	Var "dnnnnnnnnndndnnnnnnnnndndnnnnnnnnnd" "DE"],Just (List [])),
 ([Var "dnnnnnnnnndndnnnnnnnnnddnnnnnnnd" "de",
	Var "dnnnnnnnnnddnnnnnnndndnnnnnnnd" "de",
	Var "dnnnnnnnnndndnnnnnnnnndndnnnnnnnnnd" "DA",
	Var "dnnnnnnnnndndnnnnnnnnndndnnnnnnnnnddnnnnnnd" "da"],Just (Con "ci")),
 ([Var "dnnnnnnnnnddnnnnnnnd" "di",
	Var "dnnnnnnnnndndnnnnnnnnnd" "DE"],Just (List [Con "ci"])),
 ([Var "dnnnnnnnnnddnnnnnnnd" "de",Var "dnnnnnnnnndndnnnnnnnnnd" "DA",
	Var "dnnnnnnnnndndnnnnnnnnnddnnnnnnnd" "da",
	Var "dnnnnnnnnndndnnnnnnnnnddnnnnnnndndnnnnnnd" "da"],Just (Con "re")),
 ([Var "dnnnnnnnnnd" "DE"],Just (List [Con "re",Con "ci"])),
 ([Var "dnnnnnnnnnd" "DA",Var "dnnnnnnnnnddnnnnnnnd" "da",
	Var "dnnnnnnnnnddnnnnnnndndnnnnnnnd" "da",
	Var "dnnnnnnnnnddnnnnnnndndnnnnnnndndnnnnnnd" "da"],Just (Con "pa"))]]
