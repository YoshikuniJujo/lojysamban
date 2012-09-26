{-# LANGUAGE PatternGuards #-}

module Unif (Result, Term(..), unification, merge, apply, lookupValue) where

import Data.List(intersect, union)
import Control.Applicative((<$>))
import Control.Arrow(second)
import Control.Monad(foldM)

type Result sc s = [([Term sc s], Maybe (Term sc s))]

data Term sc s
	= Var sc s | Con s | ApplyOp (s -> s -> s) (Term sc s) (Term sc s)
	| Is | List [Term sc s] | Cons (Term sc s) (Term sc s)

instance (Eq sc, Eq s) => Eq (Term sc s) where
	Var sc x == Var sc' y = sc == sc' && x == y
	Con x == Con y = x == y
	ApplyOp{} == ApplyOp{} = error "can't compare applys"
	Is == Is = True
	List xs == List ys = xs == ys
	Cons h t == Cons i u = h == i && t == u
	_ == _ = False

instance (Show sc, Show s) => Show (Term sc s) where
	show (Var sc x) = "(Var " ++ show sc ++ " " ++ show x ++ ")"
	show (Con x) = "(Con " ++ show x ++ ")"
	show (ApplyOp{}) = "(ApplyOp _ _ _)"
	show Is = "Is"
	show (List ts) = "(List " ++ show ts ++ ")"
	show (Cons h t) = "(Cons (" ++ show h ++ ") (" ++ show t ++ "))"

unification :: (Eq sc, Eq s) => [Term sc s] -> [Term sc s] -> Maybe (Result sc s)
unification [] [] = Just []
unification (t : ts) (u : us) = do
	(_, ret) <- unify t u
	merge ret =<< unification ts us
unification _ _ = Nothing

unify :: (Eq sc, Eq s) => Term sc s -> Term sc s -> Maybe (Term sc s, Result sc s)
unify t u | t == u = Just (t, [])
unify t@(Var _ _) u@(Var _ _) = Just (t, [([t, u], Nothing)])
unify t@(Var _ _) u = Just (u, [([t], Just u)])
unify t u@(Var _ _) = unify u t
unify (Con _) (Con _) = Nothing
unify (Con _) ApplyOp{} = error "unify Con{} ApplyOp{}: not implemented yet"
unify t@ApplyOp{} u@(Con _) = unify u t
unify (Con _) _ = Nothing
unify _ (Con _) = Nothing
unify ApplyOp{} ApplyOp{} = error "unify ApplyOp{} ApplyOP{}: not implemented yet"
unify ApplyOp{} _ = Nothing
unify _ ApplyOp{} = Nothing
unify Is _ = Nothing
unify _ Is = Nothing
unify (List ts) (List us) = do
	rs <- unification ts us
	return (List $ map (`lookupValue` rs) ts, rs)
unify (List []) Cons{} = Nothing
unify (List (t : ts)) (Cons hd tl) = do
	rs <- unification [t, List ts] [hd, tl]
	return (List (lookupValue t rs : map (`lookupValue` rs) ts), rs)
unify t@(Cons _ _) u@(List _) = unify u t
unify (Cons h1 t1) (Cons h2 t2) = do
	rs <- unification [h1, t1] [h2, t2]
	return (Cons (lookupValue h1 rs) (lookupValue t1 rs), rs)

simplifyResult :: (Eq sc, Eq s) => Result sc s -> Result sc s
simplifyResult rs = map (second $ fmap $ flip lookupValue rs) rs

merge :: (Eq sc, Eq s) => Result sc s -> Result sc s -> Maybe (Result sc s)
merge ps qs = simplifyResult <$> foldM (flip merge1) qs ps

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

lookupValue :: (Eq sc, Eq s) => Term sc s -> Result sc s -> Term sc s
lookupValue t@(Con _) _ = t
lookupValue t@(Var _ _) rs =
-- lookupValue t rs =
	case f of
		[] -> t
		[(_, Nothing)] -> t
		[(_, Just t')] -> t'
		_ -> error "cannot occur"
	where
	f = filter ((t `elem`) . fst) rs
lookupValue (List ts) rs = List $ map (`lookupValue` rs) ts
lookupValue (Cons h t) rs = Cons (lookupValue h rs) (lookupValue t rs)
lookupValue _ _ = error "lookupValue: not implemented"

apply :: (Eq s, Eq sc) => Term sc s -> Result sc s -> Term sc s
apply (ApplyOp op t u) rs
	| Con x <- lookupValue t rs, Con y <- lookupValue u rs =
		Con $ op x y
	| otherwise = error "cannot apply"
apply x rs = lookupValue x rs
