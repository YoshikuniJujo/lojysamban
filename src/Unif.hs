module Unif (Term(..), Result, merge, unification, unify) where

import Data.List(intersect, union)
import Data.Maybe(catMaybes)
import Control.Applicative((<$>))
import Control.Monad(foldM)

data Term sc s =
	Con s | Var sc s | List [Term sc s] | Cons (Term sc s) (Term sc s)
	deriving (Eq, Show)

type Result sc s = [([Term sc s], Maybe (Term sc s))]

merge :: (Eq sc, Eq s) => Result sc s -> Result sc s -> Maybe (Result sc s)
merge [] uss = Just $ simplify2All uss
merge (tsv@(ts, v1) : tss) uss = case filterElems ts uss of
	[] -> merge tss $ tsv : uss
	ps -> do
		ret <- foldM mergeValue v1 $ map snd ps
		merge tss $ (foldr (union . fst) ts ps, ret) : deleteElems ts uss
	where
	filterElems xs = filter (not . null . intersect xs . fst)
	mergeValue x@(Just _) y@(Just _)
		| x == y = Just x
		| otherwise = Nothing
	mergeValue x@(Just _) _ = Just x
	mergeValue _ y = Just y
	deleteElems xs = filter $ \ys -> null $ intersect xs $ fst ys

unification :: (Eq sc, Eq s) => [Term sc s] -> [Term sc s] -> Maybe (Result sc s)
unification ts us = simplify2All <$> (simplify =<< unifies ts us)

unify :: (Eq sc, Eq s) =>
	Term sc s -> Term sc s -> Maybe [(Term sc s, Term sc s)]
unify t u | t == u = Just []
unify (Con _) (Con _) = Nothing
unify t u = Just [(t, u)]

unifies :: (Eq sc, Eq s) =>
	[Term sc s] -> [Term sc s] -> Maybe [(Term sc s, Term sc s)]
unifies [] [] = Just []
unifies (List ts1 : ts) (List us1 : us) = do
	r1 <- unifies ts1 us1
	r2 <- unifies ts us
	return $ r1 ++ r2
unifies (t : ts) (u : us) = case unify t u of
	Nothing -> Nothing
	Just [] -> unifies ts us
	Just [p] -> (p :) <$> unifies ts us
unifies _ _ = Nothing

-- before form is bellow
-- [(X, A), (A, Y), (B, Z), (hoge, B)] -- no (hoge, hage) or (B, B)
-- (Var _, Var _), (Con _, Var _), (Var _, Con _)
-- simplified form is bellow
-- [([X, A, Y], Nothing), ([Z, B], Just hoge)]

simplify2All :: (Eq sc, Eq s) => Result sc s -> Result sc s
simplify2All ps
	| checkSimple2 ps = ps
	| otherwise = simplify2All $ simplify2 ps

checkSimple2 :: (Eq sc, Eq s) => Result sc s -> Bool
checkSimple2 = notDup' . map snd

notDup' :: Eq a => [Maybe a] -> Bool
notDup' [] = True
notDup' (Nothing : xs) = notDup' xs
notDup' (Just x : xs)
	| x `elem` catMaybes xs = False
	| otherwise = notDup' xs

simplify2 :: (Eq sc, Eq s) => Result sc s -> Result sc s
simplify2 [] = []
simplify2 ((ts, v@(Just _)) : ps) = (maybe ts (ts ++) ts', v) : simplify2 ps'
	where
	ts' = lookupSnd v ps
	ps' = deleteSnd v ps
simplify2 (p : ps) = p : simplify2 ps

lookupSnd :: Eq b => b -> [(a, b)] -> Maybe a
lookupSnd x = lookup x . map (\(y, z) -> (z, y))

deleteSnd :: Eq b => b -> [(a, b)] -> [(a, b)]
deleteSnd x = filter ((/= x) . snd)

simplify :: (Eq sc, Eq s) =>
	[(Term sc s, Term sc s)] -> Maybe [([Term sc s], Maybe (Term sc s))]
simplify [] = Just []
simplify ((Con _, Con _) : _) = error "bad before data"
simplify ((t@(Var _ _), u@(Var _ _)) : ps) = case simplify ps of
	Nothing -> Nothing
	Just ps' -> case (lookupElem t ps', lookupElem u ps') of
		(Just (ts, Just v1), Just (us, Just v2))
			| v1 == v2 -> Just $ (ts `union` us, Just v1) :
				deleteElem t (deleteElem u ps')
			| otherwise -> Nothing
		(Just (ts, Just v1), Just (us, _)) ->
			Just $ (ts `union` us, Just v1) :
				deleteElem t (deleteElem u ps')
		(Just (ts, _), Just (us, v2)) ->
			Just $ (ts `union` us, v2) :
				deleteElem t (deleteElem u ps')
		(Just (ts, v1), _) -> Just $ (u : ts, v1) : deleteElem t ps'
		(_, Just (us, v2)) -> Just $ (t : us, v2) : deleteElem u ps'
		(_, _) -> Just $ ([t, u], Nothing) : ps'
simplify ((t@(Var _ _), u@(List us)) : ps) = case simplify ps of
	Nothing -> Nothing
	Just ps' -> case lookupElem t ps' of
		Just (_ts, Just (Con _)) -> Nothing
		Just (_ts, Just (List l)) -> case simplify =<< unifies l us of
			Nothing -> Nothing
			Just ret -> Just $ ([t], Just u) : ret ++ deleteElem t ps'
		Just (_ts, Just (Cons uh ut)) -> case simplify =<< unifies [head us, List $ tail us] [uh, ut] of
			Nothing -> Nothing
			Just ret -> Just $ ([t], Just u) : ret ++ deleteElem t ps'
		Just (ts, _) -> Just $ (ts, Just u) : deleteElem t ps'
		_ -> Just $ ([t], Just u) : ps'
simplify ((t@(List _), u@(Var _ _)) : ps) = simplify ((u, t) : ps)
simplify ((t@(Var _ _), u@(Cons uh ut)) : ps) = case simplify ps of
	Nothing -> Nothing
	Just ps' -> case lookupElem t ps' of
		Just (_, Just (Con _)) -> Nothing
		Just (_, Just (List (h:l))) -> case simplify =<< unifies [h, List l] [uh, ut] of
			Nothing -> Nothing
			Just ret -> Just $ ([t], Just u) : ret ++ deleteElem t ps'
		Just (ts, _) -> Just $ (ts, Just u) : deleteElem t ps'
		_ -> Just $ ([t], Just u) : ps'
simplify ((t@(Cons _ _), u@(Var _ _)) : ps) = simplify ((u, t) : ps)
simplify ((t@(Var _ _), u) : ps) = case simplify ps of
	Nothing -> Nothing
	Just ps' -> case lookupElem t ps' of
		Just (_, Just v1)
			| u == v1 -> Just ps'
			| otherwise -> Nothing
		Just (ts, _) -> Just $ (ts, Just u) : deleteElem t ps'
		_ -> Just $ ([t], Just u) : ps'
simplify ((t, u) : ps) = case simplify ps of
	Nothing -> Nothing
	Just ps' -> case lookupElem u ps' of
		Just (_, Just v2)
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
