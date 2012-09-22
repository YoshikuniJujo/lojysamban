module Unif (Term(..), Result, merge, unification, unify) where

import Data.List(intersect, union)
import Data.Maybe
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

unify :: (Eq sc, Eq s) => Term sc s -> Term sc s -> Maybe (Result sc s)
unify t u | t == u = Just []
unify (Con _) (Con _) = Nothing
unify t@(Var _ _) u@(Var _ _) = Just [([t, u], Nothing)]
unify t@(Var _ _) u = Just [([t], Just u)]
unify t u@(Var _ _) = Just [([u], Just t)]

unifies :: (Eq sc, Eq s) => [Term sc s] -> [Term sc s] -> Maybe (Result sc s)
unifies [] [] = Just []
unifies (List ts1 : ts) (List us1 : us) = do
	r1 <- unifies ts1 us1
	r2 <- unifies ts us
	return $ r1 ++ r2
unifies (t : ts) (u : us) = case unify t u of
	Nothing -> Nothing
	Just [] -> unifies ts us
	Just [p] -> (p :) <$> unifies ts us
	_ -> error "yet"
unifies _ _ = Nothing

simplify :: (Eq sc, Eq s) => Result sc s -> Maybe (Result sc s)
simplify = (>>= simp) . sameCon

simp :: (Eq sc, Eq s) => Result sc s -> Maybe (Result sc s)
simp [] = Just []
simp (p@(ts, v) : rest) = do
	v' <- foldM unifMaybe v (map snd same)
	return $ (foldr (union) ts (map fst same), v') : nosm
	where
	same = filter (sameVar p) rest
	nosm = filter (not . sameVar p) rest

unifMaybe :: (Eq sc, Eq s) =>
	Maybe (Term sc s) -> Maybe (Term sc s) -> Maybe (Maybe (Term sc s))
unifMaybe t@(Just (Con _)) Nothing = Just t
unifMaybe Nothing u@(Just (Con _)) = Just u
unifMaybe t@(Just (Con _)) u@(Just (Con _))
	| t == u = Just t
	| otherwise = Nothing
unifMaybe _ _ = error "yet defined"

sameVar :: (Eq sc, Eq s) =>
	([Term sc s], Maybe (Term sc s)) -> ([Term sc s], Maybe (Term sc s)) -> Bool
sameVar (ts, _) (us, _) = not $ null $ intersect ts us

sameCon :: (Eq sc, Eq s) => Result sc s -> Maybe (Result sc s)
sameCon [] = Just []
sameCon ((_, Just (Var _ _)) : _) = error "can not occur"
sameCon ((ts, _) : _) | any isCon ts = error "can not occur"
sameCon ((ts, Just val) : rest) =
	((foldl union ts $ map fst $ filter ((== Just val) . snd) rest, Just val) :)
		<$> sameCon (filter ((/= Just val) . snd) rest)
sameCon (p : rest) = (p :) <$> sameCon rest

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

isCon :: Term sc s -> Bool
isCon (Con _) = True
isCon _ = False
