{-# LANGUAGE PatternGuards #-}

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
merge ps qs = foldM (flip merge1) qs ps

merge1 :: (Eq sc, Eq s) =>
	([Term sc s], Maybe (Term sc s)) -> Result sc s -> Maybe (Result sc s)
merge1 (ts, mv) r
	| [(us, mv')] <- filter (isDefFor ts) r = do
--	| (us, mv') : _ <- filter (isDefFor ts) r = do
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
--		let ret1' = error $ show r
		fun (us2, mv'2) (ts `union` us1, mv) ret1
	| err <- filter (isDefFor ts) r = error $ show $ length err
	where
--	[(us, mv')] = filter (isDefFor ts) r
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
unification ts us = simplify2All <$> (simplify =<< unifies ts us)

unify :: (Eq sc, Eq s) => Term sc s -> Term sc s -> Maybe (Term sc s, Result sc s)
unify t u | t == u = Just (t, [])
unify (Con _) (Con _) = Nothing
unify t@(Var _ _) u@(Var _ _) = Just (t, [([t, u], Nothing)])
unify t@(Var _ _) u = Just (u, [([t], Just u)])
unify t u@(Var _ _) = Just (t, [([u], Just t)])

unifies :: (Eq sc, Eq s) => [Term sc s] -> [Term sc s] -> Maybe (Result sc s)
unifies [] [] = Just []
unifies (List ts1 : ts) (List us1 : us) = do
	r1 <- unifies ts1 us1
	r2 <- unifies ts us
	return $ r1 ++ r2
unifies (t : ts) (u : us) = case unify t u of
	Nothing -> Nothing
	Just (_, []) -> unifies ts us
	Just (_, [p]) -> (p :) <$> unifies ts us
	_ -> error "yet"
unifies _ _ = Nothing

simplify :: (Eq sc, Eq s) => Result sc s -> Maybe (Result sc s)
simplify = (>>= simp) . sameCon

simp :: (Eq sc, Eq s) => Result sc s -> Maybe (Result sc s)
simp [] = Just []
simp (p@(ts, v) : rest) = do
	v' <- foldM unifMaybe v (map snd same)
	return $ (foldr (union . fst) ts same, v') : nosm
	where
	same = filter (sameVar p) rest
	nosm = filter (not . sameVar p) rest

unifMaybe :: (Eq sc, Eq s) =>
	Maybe (Term sc s) -> Maybe (Term sc s) -> Maybe (Maybe (Term sc s))
unifMaybe (Just t) (Just u) = (Just . fst) <$> unify t u
unifMaybe t Nothing = Just t
unifMaybe Nothing u = Just u

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
