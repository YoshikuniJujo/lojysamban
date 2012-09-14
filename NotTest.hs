import Unification
import Control.Applicative
import Data.Maybe

checkAll :: [([Term], Maybe Term)] -> [Maybe [(Term, Term)]] -> Bool
checkAll r [] = True
checkAll r (Nothing : nots) = checkAll r nots
checkAll r (Just n : nots) = checkNot r n && checkAll r nots

checkNot :: [([Term], Maybe Term)] -> [(Term, Term)] -> Bool
checkNot _ [] = False
checkNot r ((t@(Var _), u@(Var _)) : ps)
	= null (filter ((\vs -> t `elem` vs && u `elem` vs) . fst) r) ||
		checkNot r ps
checkNot r ((t@(Var _), u) : ps)
	= snd (head $ filter ((t `elem`) . fst) r) /= Just u || checkNot r ps

-- notUnification :: 
notUnification ts us = simplify <$> notUnifies ts us

notUnify :: Term -> Term -> Maybe (Maybe (Term, Term))
notUnify t u | t == u = Nothing
notUnify t@(Con _) u@(Con _) = Just Nothing
notUnify t u = Just $ Just (t, u)

notUnifies :: [Term] -> [Term] -> Maybe [(Term, Term)]
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

simplify :: [(Term, Term)] -> [(Term, Term)]
simplify = map (uncurry order)

order :: Term -> Term -> (Term, Term)
order (Con _) (Con _) = error "not occur"
order t@(Var _) u@(Con _) = (t, u)
order t@(Con _) u@(Var _) = (u, t)
order t u = (t, u)
