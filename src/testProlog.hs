{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Prolog2

instance TwoD String where
	next = (++ " next")
	down = (++ " down")

fact1, fact2 :: Fact String String
fact1 = const [Con "likes", Con "wallace", Con "cheese"]
fact2 = const [Con "likes", Con "grommit", Con "cheese"]
fact3 = const [Con "likes", Con "wendolene", Con "sheep"]
fact4 sc = [Con "likes", Var sc "X", Var sc "Z"]
fact5 sc = [Con "likes", Var sc "Y", Var sc "Z"]

rule1 :: Rule String String
rule1 = Rule fact1 [] [] []
rule2 = Rule fact2 [] [] []
rule25 = Rule fact3 [] [] []
rule3 = Rule (\sc -> [Con "friends", Var sc "X", Var sc "Y"])
--	[NotUnify (Var "" "X") (Var "" "Y")] [fact4, fact5] []
	[] [fact4, fact5] [\sc -> [Con "du", Var sc "X", Var sc "Y"]]
rule4 = Rule (\sc -> [Con "du", Var sc "D", Var sc "D"]) [] [] []
rules = [rule1, rule2, rule25, rule3, rule4]

simpleRule = [rule1, rule2, srule3]
srule3 = Rule (\sc -> [Con "friends", Var sc "X", Var sc "Y"])
	[] [\sc -> [Con "likes", Var sc "X", Var sc "Y"]] []

-- bug1 = ask "" (\sc -> [Con "friends", Var sc "Who", Var sc "What"]) simpleRule

q1, q2 :: Fact String String
q1 sc = [Con "likes", Var sc "X", Con "cheese"]
q2 sc = [Con "friends", Con "wallace", Con "grommit"]
q3 sc = [Con "friends", Con "wallace", Con "wallace"]
q4 sc = [Con "friends", Var sc "Who", Con "grommit"]
q5 sc = [Con "frineds", Var sc "X", Var sc "Y"]
q6 sc = [Con "frineds", Var sc "V", Var sc "W"]
q7 sc = [Con "likes", Var sc "Who", Var sc "What"]

rules2 = [rule21, rule22]
rule21 = rule4
rule22 = Rule (\sc -> [Con "brode", Var sc "da", Var sc "de"])
	[] [\sc -> [Con "du", Var sc "da", Con "cinfo"]
--		] []
		, \sc -> [Con "du", Var sc "de", Con "tirxu"]] []

patfuFact1 :: Fact String String
patfuFact1 sc = [Con "patfu", Con "zeb", Con "jon.bois.sr"]
patfuFact2 sc = [Con "patfu", Con "jon.bois.sr", Con "jon.bois.jr"]

patfuRule1 :: Rule String String
patfuRule1 = Rule patfuFact1 [] [] []
patfuRule2 = Rule patfuFact2 [] [] []
patfuRule3 = Rule (\sc -> [Con "dzena", Var sc "X", Var sc "Y"])
	[] [\sc -> [Con "patfu", Var sc "X", Var sc "Y"]] []
patfuRule4 = Rule (\sc -> [Con "dzena", Var sc "da", Var sc "de"])
	[] [	\sc -> [Con "patfu", Var sc "da", Var sc "di"],
		\sc -> [Con "dzena", Var sc "di", Var sc "de"]] []

patfuRules = [patfuRule1, patfuRule2, patfuRule3, patfuRule4]

duRule = Rule (\sc -> [Con "du", Var sc "X", Var sc "X"]) [] [] []
listFact sc = [Con "du", list1 sc, list2]
list1 sc = List [Var sc "X", Var sc "Y", Var sc "Z"]
list2 = List [Con "1", Con "2", Con "3"]
