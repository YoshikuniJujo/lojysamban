{-# LANGUAGE PatternGuards #-}

module Unif (Term(..), Result, merge, unification, unify, apply) where

import Data.List(intersect, union)
import Control.Monad(foldM)

data Term sc s
	= Con s | Var sc s | List [Term sc s] | Cons (Term sc s) (Term sc s)
	| ApplyOp (s -> s -> s) (Term sc s) (Term sc s)
	| Is

instance (Show sc, Show s) => Show (Term sc s) where
	show (Con x) = "Con " ++ show x
	show (Var sc x) = "Var " ++ show sc ++ " " ++ show x
	show (List ts) = "List " ++ show ts
	show (Cons h t) = "Cons (" ++ show h ++ ") (" ++ show t ++ ")"
	show (ApplyOp _ _ _) = "ApplyOp _ _ _"

instance (Eq sc, Eq s) => Eq (Term sc s) where
	Con x == Con y = x == y
	Var sc x == Var sc' y = sc == sc' && x == y
	List xs == List ys = xs == ys
	Cons h t == Cons i u = h == i && t == u
	ApplyOp _ _ _ == ApplyOp _ _ _ = error "can't compare applys"
	_ == _ = False

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
unify (List ts) (List us) = do
	rs <- unification ts us
	return (List $ map (flip lookupValue rs) ts, rs)
unify (Cons h t) (List (u : us)) = do
	rs <- unification [h, t] [u, List us]
	return (Cons (lookupValue h rs) (lookupValue t rs), rs)
unify (Cons h t) (List []) = Nothing
unify (Cons h1 t1) (Cons h2 t2) = do
	rs <- unification [h1, t1] [h2, t2]
	return (Cons (lookupValue h1 rs) (lookupValue t1 rs), rs)
unify t u@(Cons _ _) = unify u t
unify Is Is = Just (Is, [])
unify Is _ = Nothing
unify _ Is = Nothing
unify (Cons _ _) (Con _) = Nothing -- error "Cons with Con"
unify (Cons _ _) (Var _ _) = error "Cons with Var"
unify (Cons _ _) (List _) = error "Cons with List"
unify (Cons _ _) (ApplyOp _ _ _) = error "Cons with ApplyOpp"
unify (Cons _ _) Is = error "Cons with Is"
unify (Cons _ _) _ = error "Cons with _"
unify (List _) _ = error "List with _"
unify (ApplyOp _ _ _) (List _) = error "AppOp with List"
unify (Con _) (List _) = Nothing -- error "Con with List"
unify _ (List _) = error "_ with List"

unifies :: (Eq sc, Eq s) => [Term sc s] -> [Term sc s] -> Maybe (Result sc s)
unifies [] [] = Just []
unifies (t : ts) (u : us) = do
	(_, ret) <- unify t u
	rets <- unifies ts us
	merge ret rets
unifies _ _ = Nothing

lookupValue :: (Eq sc, Eq s) => Term sc s -> Result sc s -> Term sc s
lookupValue t rs =
	case f of
		[] -> t
		[(_, Nothing)] -> t
		[(_, Just t')] -> t'
	where
	f = filter ((t `elem`) . fst) rs

apply (ApplyOp op t u) rs
	| Con x <- lookupValue t rs, Con y <- lookupValue u rs =
		Con $ op x y
	| otherwise = error "cannot apply"
apply x rs = x
