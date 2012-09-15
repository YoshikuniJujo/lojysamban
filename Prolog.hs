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

type Fact sc s = sc -> [Term sc s]
type NotFact sc s = Fact sc s
data Unify sc s
	= Unify (Term sc s) (Term sc s)
	| NotUnify (Term sc s) (Term sc s)
	deriving Show
data Rule sc s = Rule (Fact sc s) [Unify sc s] [Fact sc s] [NotFact sc s]
--	deriving Show

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
