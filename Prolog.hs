{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Prolog (
	ask,
	Fact,
	Unify(..),
	Rule(..),
	Term(..),
	TwoD(..)
) where

import Unif
import NotUnif
import Data.Maybe
import Control.Applicative

class TwoD td where
	next :: td -> td
	down :: td -> td

instance TwoD String where
	next = (++ " next")
	down = (++ " down")

type Fact sc s = sc -> [Term sc s]
type NotFact sc s = Fact sc s
data Unify sc s
	= Unify (Term sc s) (Term sc s)
	| NotUnify (Term sc s) (Term sc s)
	deriving Show
data Rule sc s = Rule (Fact sc s) [Unify sc s] [Fact sc s] [NotFact sc s]
--	deriving Show

fact1, fact2 :: Fact String String
fact1 = const [Con "likes", Con "wallace", Con "cheese"]
fact2 = const [Con "likes", Con "grommit", Con "cheese"]
fact3 = const [Con "likes", Con "wendolene", Con "sheep"]
fact4 sc = [Con "likes", Var sc "X", Var sc "Z"]
fact5 sc = [Con "likes", Var sc "Y", Var sc "Z"]

rule1 :: Rule String String
rule1 = Rule fact1 [] [] []
rule2 = Rule fact2 [] [] []
rule2_5 = Rule fact3 [] [] []
rule3 = Rule (\sc -> [Con "friends", Var sc "X", Var sc "Y"])
--	[NotUnify (Var "" "X") (Var "" "Y")] [fact4, fact5] []
	[] [fact4, fact5] [\sc -> [Con "du", Var sc "X", Var sc "Y"]]
rule4 = Rule (\sc -> [Con "du", Var sc "D", Var sc "D"]) [] [] []
rules = [rule1, rule2, rule2_5, rule3, rule4]

simpleRule = [rule1, rule2, srule3]
srule3 = Rule (\sc -> [Con "friends", Var sc "X", Var sc "Y"])
	[] [\sc -> [Con "likes", Var sc "X", Var sc "Y"]] []

bug1 = ask "" (\sc -> [Con "friends", Var sc "Who", Var sc "What"]) simpleRule

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

notAsk :: (TwoD sc, Eq sc, Eq s) => sc ->
	Fact sc s -> [Rule sc s] -> [Maybe [(Term sc s, Term sc s)]]
notAsk sc q rs = zipWith (\sc r -> notAskRule sc q r rs) (iterate next $ down sc) rs

notAskRule :: (TwoD sc, Eq sc, Eq s) => sc ->
	Fact sc s -> Rule sc s -> [Rule sc s] -> Maybe [(Term sc s, Term sc s)]
notAskRule sc q r@(Rule fact unify facts notFacts) rs = liftA concat $
	case match of
		Nothing -> Just []
		Just _ ->
			liftA2 (:) start $ maybeOut $ concat $
				map ((flip (notAsk sc) rs) . const . ($ sc)) facts
	where
	start = notUnification (q sc) $ fact sc
	match = unification [head $ q sc] [head $ fact sc]

maybeOut :: [Maybe a] -> Maybe [a]
maybeOut [] = Just []
maybeOut (Nothing : xs) = Nothing
maybeOut (Just x : xs) = maybe Nothing (Just . (x :)) $ maybeOut xs

ask :: (TwoD sc, Eq sc, Eq s) =>
	sc -> Fact sc s -> [Rule sc s] -> [Result sc s]
ask sc q rs =
	  concat $ zipWith (\sc r -> askRule sc q r rs) (iterate next $ down sc) rs

askRule :: (TwoD sc, Eq sc, Eq s) =>
	sc -> Fact sc s -> Rule sc s -> [Rule sc s] -> [Result sc s]
askRule sc q r@(Rule fact unify facts notFacts) rs =
	filter (flip checkAll nots) ret
--	ret
	where
	ret = foldl mergeM (maybeToList start) $
--		map ((flip (ask sc) rs) . const . ($ sc)) facts
		zipWith (\sc f -> ask sc f rs) (iterate next sc) $ map (const . ($ sc)) facts
--	start = foldl mergeM (unification q fact) $ map checkUnify unify
--	start = foldl cu (unification q fact) unify
	start = unification (q sc) (fact sc)
	nots = concat $ map ((flip (notAsk sc) rs) . const . ($sc)) notFacts
	cu rr u = do
		r <- rr
		checkUnify' r u

mergeM ff gg = do
	f <- ff
	g <- gg
	case merge f g of
		Nothing -> fail "not return"
		Just r -> return r

checkUnify :: (Eq sc, Eq s) => Unify sc s -> Maybe (Result sc s)
checkUnify (Unify t u) = unification [t] [u]
checkUnify (NotUnify t u) = maybe (Just []) (const Nothing) $ unification [t] [u]

checkUnify' :: (Eq sc, Eq s) => Result sc s -> Unify sc s -> Maybe (Result sc s)
checkUnify' r (Unify t u) = (`merge` r) =<< unification [t] [u]
checkUnify' r (NotUnify t u) = case (`merge` r) =<< unification [t] [u] of
	Just _ -> Nothing
	Nothing -> Just r
