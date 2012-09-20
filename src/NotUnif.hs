module NotUnif (
	checkAll,
	checkNot,
	deleteFromNot,
	notUnification
) where

import Unif
import Control.Applicative
import Data.Maybe

-- checkAll :: [([Term], Maybe Term)] -> [Maybe [(Term, Term)]] -> Bool
checkAll r [] = True
checkAll r (Nothing : nots) = checkAll r nots
checkAll r (Just [] : nots) = checkAll r nots
checkAll r (Just n : nots) =
	checkNot r (deleteFromNot r n) && checkAll r nots

-- deleteFromNot :: [([Term], Maybe Term)] -> [(Term, Term)] -> [(Term, Term)]
deleteFromNot _ [] = []
deleteFromNot r ((t@(Var _ _), u@(Var _ _)) : ps)
	| null $ filter ((t `elem`) . fst) r = deleteFromNot r ps
	| null $ filter ((u `elem`) . fst) r = deleteFromNot r ps
	| otherwise = (t, u) : deleteFromNot r ps
deleteFromNot r ((t, u) : ps) = (t, u) : deleteFromNot r ps

-- checkNot :: [([Term], Maybe Term)] -> [(Term, Term)] -> Bool
checkNot _ [] = False
checkNot r ((t@(Var _ _), u@(Var _ _)) : ps)
	= null (filter ((\vs -> t `elem` vs && u `elem` vs) . fst) r) ||
--		null (filter ((t `elem`) .fst) r) ||
--		null (filter ((u `elem`) .fst) r) ||
		checkNot r ps
checkNot r ((t@(Var _ _), u) : ps)
	= snd (head $ filter ((t `elem`) . fst) r) /= Just u || checkNot r ps

-- notUnification :: 
notUnification ts us = simplify <$> notUnifies ts us

-- notUnify :: Term -> Term -> Maybe (Maybe (Term, Term))
notUnify t u | t == u = Nothing
notUnify t@(Con _) u@(Con _) = Just Nothing
notUnify t u = Just $ Just (t, u)

-- notUnifies :: [Term] -> [Term] -> Maybe [(Term, Term)]
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
checkSame (p : ps) = catMaybes (map (isSame p) ps) ++ p : checkSame ps

-- isSame :: (Term, Term) -> (Term, Term) -> Maybe (Term, Term)
isSame :: Eq a => (a, a) -> (a, a) -> Maybe (a, a)
isSame (x, y) (z, w)
	| x == z = Just (y, w)
	| y == w = Just (x, z)
	| x == w = Just (y, z)
	| y == z = Just (x, w)
	| otherwise = Nothing

-- order :: Term -> Term -> (Term, Term)
order (Con _) (Con _) = error "not occur"
order t@(Var _ _) u@(Con _) = (t, u)
order t@(Con _) u@(Var _ _) = (u, t)
order t u = (t, u)
