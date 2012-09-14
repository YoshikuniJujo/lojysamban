module Unification (Term(..), unification, a, b, x, y, z, hoge) where

import Control.Applicative
import Data.List

data Term = Con String | Var String deriving (Eq, Show)

unification :: [Term] -> [Term] -> Maybe [([Term], Maybe Term)]
unification ts us = simplify =<< unifies ts us

unify :: Term -> Term -> Maybe (Maybe (Term, Term))
unify t u | t == u = Just Nothing
unify t@(Con _) u@(Con _) = Nothing
unify t u = Just $ Just (t, u)

unifies :: [Term] -> [Term] -> Maybe [(Term, Term)]
unifies [] [] = Just []
unifies (t : ts) (u : us) = case unify t u of
	Nothing -> Nothing
	Just Nothing -> unifies ts us
	Just (Just p) -> (p :) <$> unifies ts us
unifies _ _ = Nothing

-- before form is bellow
-- [(X, A), (A, Y), (B, Z), (hoge, B)] -- no (hoge, hage) or (B, B)
-- (Var _, Var _), (Con _, Var _), (Var _, Con _)
-- simplified form is bellow
-- [([X, A, Y], Nothing), ([Z, B], Just hoge)]

-- test data
a, b, x, y, hoge :: Term
a = Var "A"
b = Var "B"
x = Var "X"
y = Var "Y"
z = Var "Z"
hoge = Con "hoge"
before :: [(Term, Term)]
before = [(x, a), (a, y), (b, z), (hoge, b)]

simplify :: [(Term, Term)] -> Maybe [([Term], Maybe Term)]
simplify [] = Just []
simplify ((Con _, Con _) : _) = error "bad before data"
simplify ((t@(Var _), u@(Var _)) : ps) = case simplify ps of
	Nothing -> Nothing
	Just ps' -> case (lookupElem t ps', lookupElem u ps') of
		(Just (ts, Just v1), Just (us, Just v2))
			| v1 == v2 -> Just $ (union ts us, Just v1) :
				deleteElem t (deleteElem u ps')
			| otherwise -> Nothing
		(Just (ts, Just v1), Just (us, _)) ->
			Just $ (union ts us, Just v1) :
				deleteElem t (deleteElem u ps')
		(Just (ts, _), Just (us, v2)) ->
			Just $ (union ts us, v2) :
				deleteElem t (deleteElem u ps')
		(Just (ts, v1), _) -> Just $ (u : ts, v1) : deleteElem t ps'
		(_, Just (us, v2)) -> Just $ (t : us, v2) : deleteElem u ps'
		(_, _) -> Just $ ([t, u], Nothing) : ps'
simplify ((t@(Var _), u) : ps) = case simplify ps of
	Nothing -> Nothing
	Just ps' -> case lookupElem t ps' of
		Just (ts, Just v1)
			| u == v1 -> Just ps'
			| otherwise -> Nothing
		Just (ts, _) -> Just $ (ts, Just u) : deleteElem t ps'
		_ -> Just $ ([t], Just u) : ps'
simplify ((t, u) : ps) = case simplify ps of
	Nothing -> Nothing
	Just ps' -> case lookupElem u ps' of
		Just (us, Just v2)
			| t == v2 -> Just ps'
			| otherwise -> Nothing
		Just (us, _) -> Just $ (us, Just t) : deleteElem u ps'
		_ -> Just $ ([u], Just t) : ps'

deleteElem :: Eq a => a -> [([a], b)] -> [([a], b)]
deleteElem _ [] = []
deleteElem x ((xs, y) : ps)
	| x `elem` xs = ps
	| otherwise = (xs, y) : deleteElem x ps

lookupElem :: Eq a => a -> [([a], b)] -> Maybe ([a], b)
lookupElem _ [] = Nothing
lookupElem x ((xs, y) : ps)
	| x `elem` xs = Just (xs, y)
	| otherwise = lookupElem x ps
