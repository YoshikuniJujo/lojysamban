module Prolog (
	ask,
	Fact,
	Unify(..),
	Rule(..),
	Term(..)
) where

import Unif
import NotUnif
import Data.Maybe
import Control.Applicative

type Fact sc s = [Term sc s]
type NotFact sc s = Fact sc s
data Unify sc s
	= Unify (Term sc s) (Term sc s)
	| NotUnify (Term sc s) (Term sc s)
	deriving Show
data Rule sc s = Rule (Fact sc s) [Unify sc s] [Fact sc s] [NotFact sc s]
	deriving Show

fact1, fact2 :: Fact String String
fact1 = [Con "likes", Con "wallace", Con "cheese"]
fact2 = [Con "likes", Con "grommit", Con "cheese"]
fact3 = [Con "likes", Con "wendolene", Con "sheep"]
fact4 = [Con "likes", Var "" "X", Var "" "Z"]
fact5 = [Con "likes", Var "" "Y", Var "" "Z"]

rule1 :: Rule String String
rule1 = Rule fact1 [] [] []
rule2 = Rule fact2 [] [] []
rule2_5 = Rule fact3 [] [] []
rule3 = Rule [Con "friends", Var "" "X", Var "" "Y"]
--	[NotUnify (Var "" "X") (Var "" "Y")] [fact4, fact5] []
	[] [fact4, fact5] [[Con "du", Var "" "X", Var "" "Y"]]
rule4 = Rule [Con "du", Var "local" "D", Var "local" "D"] [] [] []
rules = [rule1, rule2, rule2_5, rule3, rule4]

q1, q2 :: Fact String String
q1 = [Con "likes", Var "" "X", Con "cheese"]
q2 = [Con "friends", Con "wallace", Con "grommit"]
q3 = [Con "friends", Con "wallace", Con "wallace"]
q4 = [Con "friends", Var "top" "Who", Con "grommit"]

notAsk :: (Eq sc, Eq s) =>
	Fact sc s -> [Rule sc s] -> [Maybe [(Term sc s, Term sc s)]]
notAsk q rs = map (flip (notAskRule q) rs) rs

notAskRule :: (Eq sc, Eq s) =>
	Fact sc s -> Rule sc s -> [Rule sc s] -> Maybe [(Term sc s, Term sc s)]
notAskRule q r@(Rule fact unify facts notFacts) rs = liftA concat $
	case match of
		Nothing -> Just []
		Just _ ->
			liftA2 (:) start $ maybeOut $ concat $ map (flip notAsk rs) facts
	where
	start = notUnification q fact
	match = unification [head q] [head fact]

maybeOut :: [Maybe a] -> Maybe [a]
maybeOut [] = Just []
maybeOut (Nothing : xs) = Nothing
maybeOut (Just x : xs) = maybe Nothing (Just . (x :)) $ maybeOut xs

ask :: (Eq sc, Eq s) => Fact sc s -> [Rule sc s] -> [Result sc s]
ask q rs = concatMap (flip (askRule q) rs) rs

askRule :: (Eq sc, Eq s) => Fact sc s -> Rule sc s -> [Rule sc s] -> [Result sc s]
askRule q r@(Rule fact unify facts notFacts) rs =
	filter (flip checkAll nots) ret
--	ret
	where
	ret = foldl mergeM (maybeToList start) $ map (flip ask rs) facts
--	start = foldl mergeM (unification q fact) $ map checkUnify unify
--	start = foldl cu (unification q fact) unify
	start = unification q fact
	nots = concat $ map (flip notAsk rs) notFacts
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
