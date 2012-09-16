{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PrologTools (
	TwoD(..),
	Fact,
	NotFact,
	Rule(..)
) where

import Unif

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
