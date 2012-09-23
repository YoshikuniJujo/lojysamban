{-# LANGUAGE PatternGuards #-}

module Unif (Term(..), Result, merge, unification, unify) where

import Data.List(intersect, union)
import Control.Monad(foldM)

data Term sc s =
	Con s | Var sc s | List [Term sc s] | Cons (Term sc s) (Term sc s)
	deriving (Eq, Show)

type Result sc s = [([Term sc s], Maybe (Term sc s))]

merge :: (Eq sc, Eq s) => Result sc s -> Result sc s -> Maybe (Result sc s)
merge ps qs = foldM (flip merge1) qs ps

merge1 :: (Eq sc, Eq s) =>
	([Term sc s], Maybe (Term sc s)) -> Result sc s -> Maybe (Result sc s)
merge1 (ts, mv) r
	| [(us, mv')] <- filter (isDefFor ts) r = do
		(vv, m) <- case (mv, mv') of
			(Just v, Just v') -> do
				(vv', m) <- unify v v'
				return (Just vv', m)
			(v, Nothing) -> return (v, [])
			(_, v') -> return (v', [])
		merge m $ (ts `union` us, vv) `add` notSames
	| [] <- filter (isDefFor ts) r = return $ (ts, mv) `add` notSames
	| [(us1, mv'1), (us2, mv'2)] <- filter (isDefFor ts) r = do
		ret1 <- fun (us1, mv'1) (ts, mv) notSames
		fun (us2, mv'2) (ts `union` us1, mv) ret1
	| err <- filter (isDefFor ts) r = error $ show $ length err
	where
	notSames = filter (not . isDefFor ts) r
	fun (_, mv') (tsss, mvvv) hoge = do
		(vv, m) <- case (mvvv, mv') of
			(Just v, Just v') -> do
				(vv', m) <- unify v v'
				return (Just vv', m)
			(v, Nothing) -> return (v, [])
			(_, v') -> return (v', [])
		merge m $ (tsss, vv) `add` hoge

add :: (Eq sc, Eq s) =>
	([Term sc s], Maybe (Term sc s)) -> Result sc s -> Result sc s
add (ts, v@(Just _)) rs = (foldr union ts sames, v) : notSames
	where
	sames = map fst $ filter ((== v) . snd) rs
	notSames = filter ((/= v) . snd) rs
add r1 rs = r1 : rs

isDefFor :: (Eq sc, Eq s) => [Term sc s] -> ([Term sc s], Maybe (Term sc s)) -> Bool
isDefFor ts (us, _) = not $ null $ ts `intersect` us

unification :: (Eq sc, Eq s) => [Term sc s] -> [Term sc s] -> Maybe (Result sc s)
unification = unifies

unify :: (Eq sc, Eq s) => Term sc s -> Term sc s -> Maybe (Term sc s, Result sc s)
unify t u | t == u = Just (t, [])
unify (Con _) (Con _) = Nothing
unify t@(Var _ _) u@(Var _ _) = Just (t, [([t, u], Nothing)])
unify t@(Var _ _) u = Just (u, [([t], Just u)])
unify t u@(Var _ _) = Just (t, [([u], Just t)])

unifies :: (Eq sc, Eq s) => [Term sc s] -> [Term sc s] -> Maybe (Result sc s)
unifies [] [] = Just []
unifies (t : ts) (u : us) = do
	(_, ret) <- unify t u
	rets <- unifies ts us
	merge ret rets
unifies _ _ = Nothing
