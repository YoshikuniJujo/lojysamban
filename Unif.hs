module Unif (
	unification,
	merge,
	Term(..),
	checkSimple2,
	simplify2All,
	Result
) where

import Control.Applicative
import Data.List hiding (deleteBy)

data Term sc s = Con s | Var sc s deriving (Eq, Show)

type Result sc s = [([Term sc s], Maybe (Term sc s))]

merge :: (Eq sc, Eq s) => Result sc s -> Result sc s -> Maybe (Result sc s)
merge [] uss = Just $ simplify2All uss
merge (tsv@(ts, v1) : tss) uss = case us of
	[] -> merge tss $ tsv : uss
	(us, v2) : rest -> case mergeValue v1 v2 of
		Nothing -> Nothing
		Just v	| not $ allEqual $ v2 : map snd rest -> Nothing
			| otherwise -> merge tss $
				(foldr union (union ts us) $ map fst rest, v) : uss'
	where
	us = filterElems ts uss
	uss' = deleteElems' ts uss

allEqual :: Eq a => [a] -> Bool
allEqual [x] = True
allEqual (x : y : xs) = x == y && allEqual (y : xs)

mergeValue :: Eq a => Maybe a -> Maybe a -> Maybe (Maybe a)
mergeValue x@(Just _) y@(Just _)
	| x == y = Just x
	| otherwise = Nothing
mergeValue x@(Just _) _ = Just x
mergeValue _ y = Just y

unification :: (Eq sc, Eq s) => [Term sc s] -> [Term sc s] -> Maybe (Result sc s)
unification ts us = simplify2All <$> (simplify =<< unifies ts us)
-- unification ts us = (simplify =<< unifies ts us)

-- unify :: Term -> Term -> Maybe (Maybe (Term, Term))
unify t u | t == u = Just Nothing
unify t@(Con _) u@(Con _) = Nothing
unify t u = Just $ Just (t, u)

-- unifies :: [Term] -> [Term] -> Maybe [(Term, Term)]
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
-- a, b, x, y, hoge :: Term
a = Var "" "A"
b = Var "" "B"
x = Var "" "X"
y = Var "" "Y"
z = Var "" "Z"
hoge = Con "hoge"
-- before :: [(Term, Term)]
before = [(x, a), (a, y), (b, z), (hoge, b)]

simplify2All :: (Eq sc, Eq s) => Result sc s -> Result sc s
simplify2All ps
	| checkSimple2 ps = ps
	| otherwise = simplify2All $ simplify2 ps

checkSimple2 :: (Eq sc, Eq s) => Result sc s -> Bool
checkSimple2 = notDup . map snd

notDup :: Eq a => [a] -> Bool
notDup [] = True
notDup (x : xs)
	| x `elem` xs = False
	| otherwise = notDup xs

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

-- simplify :: [(Term, Term)] -> Maybe [([Term], Maybe Term)]
simplify [] = Just []
simplify ((Con _, Con _) : _) = error "bad before data"
simplify ((t@(Var _ _), u@(Var _ _)) : ps) = case simplify ps of
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
simplify ((t@(Var _ _), u) : ps) = case simplify ps of
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

deleteElems :: Eq a => [a] -> [([a], b)] -> [([a], b)]
deleteElems = deleteBy $ \x y -> not $ null $ intersect x y

deleteElems' :: Eq a => [a] -> [([a], b)] -> [([a], b)]
deleteElems' xs = filter $ \ys -> null $ intersect xs $ fst ys

deleteBy :: (a -> b -> Bool) -> a -> [(b, c)] -> [(b, c)]
deleteBy _ _ [] = []
deleteBy p x ((y, z) : ps)
	| p x y = ps
	| otherwise = (y, z) : deleteBy p x ps

deleteElem :: Eq a => a -> [([a], b)] -> [([a], b)]
deleteElem _ [] = []
deleteElem x ((xs, y) : ps)
	| x `elem` xs = ps
	| otherwise = (xs, y) : deleteElem x ps

filterElems :: Eq a => [a] -> [([a], b)] -> [([a], b)]
filterElems xs = filter (\ys -> not $ null $ intersect xs $ fst ys)

lookupElems :: Eq a => [a] -> [([a], b)] -> Maybe ([a], b)
lookupElems = lookupBy $ \x y -> not $ null $ intersect x y

lookupBy :: (a -> b -> Bool) -> a -> [(b, c)] -> Maybe (b, c)
lookupBy _ _ [] = Nothing
lookupBy p x ((y, z) :ps)
	| p x y = Just (y, z)
	| otherwise = lookupBy p x ps

lookupElem :: Eq a => a -> [([a], b)] -> Maybe ([a], b)
lookupElem _ [] = Nothing
lookupElem x ((xs, y) : ps)
	| x `elem` xs = Just (xs, y)
	| otherwise = lookupElem x ps