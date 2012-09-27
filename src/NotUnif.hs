module NotUnif (
	merge,
	Result,
	unification,
	Term(..),
	checkAll,
	checkNot,
	deleteFromNot,
	notUnification,
	applyIs
) where

import Unif
import Control.Applicative
import Data.Maybe

checkAll :: (Eq sc, Eq s) =>
	[([Term sc s], Maybe (Term sc s))] -> [Maybe [(Term sc s, Term sc s)]]
		-> Bool
checkAll _ [] = True
checkAll r (Nothing : nots) = checkAll r nots
checkAll r (Just [] : nots) = checkAll r nots
checkAll r (Just n : nots) =
	checkNot r (deleteFromNot r n) && checkAll r nots

deleteFromNot :: (Eq sc, Eq s) =>
	[([Term sc s], Maybe (Term sc s))] -> [(Term sc s, Term sc s)] ->
		[(Term sc s, Term sc s)]
deleteFromNot _ [] = []
deleteFromNot r ((t@(Var _ _), u@(Var _ _)) : ps)
	| not $ any ((t `elem`) . fst) r = deleteFromNot r ps
	| not $ any ((u `elem`) . fst) r = deleteFromNot r ps
	| otherwise = (t, u) : deleteFromNot r ps
deleteFromNot r ((t, u) : ps) = (t, u) : deleteFromNot r ps

checkNot :: (Eq sc, Eq s) =>
	[([Term sc s], Maybe (Term sc s))] -> [(Term sc s, Term sc s)] -> Bool
checkNot _ [] = False
checkNot r ((t@(Var _ _), u@(Var _ _)) : ps)
	= not (any ((\vs -> t `elem` vs && u `elem` vs) . fst) r) ||
--		null (filter ((t `elem`) .fst) r) ||
--		null (filter ((u `elem`) .fst) r) ||
		checkNot r ps
checkNot r ((t@(Var _ _), u) : ps)
	= snd (head $ filter ((t `elem`) . fst) r) /= Just u || checkNot r ps
checkNot _ _ = error "bad"

notUnification :: (Eq sc, Eq s) =>
	[Term sc s] -> [Term sc s] -> Maybe [(Term sc s, Term sc s)]
notUnification ts us = simplify <$> notUnifies ts us

notUnify :: (Eq sc, Eq s) =>
	Term sc s -> Term sc s -> Maybe (Maybe (Term sc s, Term sc s))
notUnify t u | t == u = Nothing
notUnify (Con _) (Con _) = Just Nothing
notUnify t u = Just $ Just (t, u)

notUnifies :: (Eq sc, Eq s) =>
	[Term sc s] -> [Term sc s] -> Maybe [(Term sc s, Term sc s)]
notUnifies [] [] = Nothing
notUnifies [t] [u] = maybeToList <$> notUnify t u
notUnifies (t : ts) (u : us) = case notUnify t u of
	Nothing -> notUnifies ts us
	Just Nothing -> Just []
	Just (Just p) -> case notUnifies ts us of
		Nothing -> Just [p]
		Just [] -> Just []
		Just ps -> Just $ p : ps
notUnifies _ _ = Just []

-- simplify :: [(Term, Term)] -> [(Term, Term)]
simplify :: (Eq sc, Eq s) =>
	[(Term sc s, Term sc s)] -> [(Term sc s, Term sc s)]
simplify = checkSame . map (uncurry order)

-- checkSame :: [(Term, Term)] -> [(Term, Term)]
checkSame :: (Eq sc, Eq s) =>
	[(Term sc s, Term sc s)] -> [(Term sc s, Term sc s)]
checkSame [] = []
checkSame (p : ps) = mapMaybe (isSame p) ps ++ p : checkSame ps

-- isSame :: (Term, Term) -> (Term, Term) -> Maybe (Term, Term)
isSame :: Eq a => (a, a) -> (a, a) -> Maybe (a, a)
isSame (x, y) (z, w)
	| x == z = Just (y, w)
	| y == w = Just (x, z)
	| x == w = Just (y, z)
	| y == z = Just (x, w)
	| otherwise = Nothing

order :: Term sc s -> Term sc s -> (Term sc s, Term sc s)
order (Con _) (Con _) = error "not occur"
order t@(Var _ _) u@(Con _) = (t, u)
order t@(Con _) u@(Var _ _) = (u, t)
order t u = (t, u)
