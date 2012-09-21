{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PrologTools (
	Term(..),
	merge,
	unification,
	Result,
	TwoD(..),
	Fact,
	NotFact,
	Rule(..),
	notAsk,
	checkAll
) where

import NotUnif
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

notAsk :: (TwoD sc, Eq sc, Eq s) => sc ->
	Fact sc s -> [Rule sc s] -> [Maybe [(Term sc s, Term sc s)]]
notAsk sc q rs = zipWith (\sc' r -> notAskRule sc' q r rs) (iterate next $ down sc) rs

notAskRule :: (TwoD sc, Eq sc, Eq s) => sc ->
	Fact sc s -> Rule sc s -> [Rule sc s] -> Maybe [(Term sc s, Term sc s)]
notAskRule sc q (Rule fact _ facts _) rs = liftA concat $
	case match of
		Nothing -> Just []
		Just _ ->
			liftA2 (:) start $ maybeOut $ concatMap
				(flip (notAsk sc) rs . const . ($ sc)) facts
	where
	start = notUnification (q sc) $ fact sc
	match = unification [head $ q sc] [head $ fact sc]

maybeOut :: [Maybe a] -> Maybe [a]
maybeOut [] = Just []
maybeOut (Nothing : _) = Nothing
maybeOut (Just x : xs) = fmap (x :) $ maybeOut xs
