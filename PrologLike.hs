module PrologLike where

import Data.Maybe
import Data.List

ask :: FactRule -> [FactRule] -> Maybe [[(Term, Term)]]
ask (Fact (NA f) ts) facts = case ask (Fact f ts) facts of
	Nothing -> Just [[]]
	Just _ -> Nothing
ask (Fact (GOhA "du") ts) _ = if ts !! 0 == ts !! 1 then Just [] else Nothing
ask q@(Fact _ ts) facts =
	case filter isJust $ map (\(Fact _ ts') -> matchTerms ts ts') f' of
		[] -> askRule q facts
		ps -> Just $ map fromJust ps
	where
	f' = findFact q facts

findFact :: FactRule -> [FactRule] -> [FactRule]
findFact (Fact f _) = filter (isFactFor f)

isFactFor :: Function -> FactRule -> Bool
isFactFor f0 (Fact f1 _) = f0 == f1
isFactFor _ _ = False

askRule :: FactRule -> [FactRule] -> Maybe [[(Term, Term)]]
askRule q@(Fact f ts) facts = case findRule q facts of
	Nothing -> Nothing
	Just r	| not $ null $ filter isNothing $ map (flip ask facts) $
			apply ts r -> Nothing
		| otherwise -> case map (flip ask facts) $ apply ts r of
			[] -> Nothing
--		ps -> Just $ matchMatches $ map fromJust ps
			ps -> case matchMatches $ map fromJust ps of
				[] -> Nothing
				ret -> Just ret

matchMatches :: [[[(Term, Term)]]] -> [[(Term, Term)]]
matchMatches [x] = x
matchMatches (x : y : xs) = matchMatches $ matchMatch x y : xs

matchMatch :: [[(Term, Term)]] -> [[(Term, Term)]] -> [[(Term, Term)]]
matchMatch pss1 pss2 = map fromJust $ filter isJust $ zipAllWith match2 pss1 pss2

zipAllWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipAllWith _ [] _ = []
zipAllWith op (x : xs) ys = map (op x) ys ++ zipAllWith op xs ys

match2 :: [(Term, Term)] -> [(Term, Term)] -> Maybe [(Term, Term)]
match2 [] ps2 = Just ps2
match2 (p1@(t1, s1) : ps1) ps2 = case lookup t1 ps2 of
	Nothing -> (p1 :) `fmap` match2 ps1 ps2
	Just s2 -> if s1 == s2 then match2 ps1 ps2 else Nothing

findRule :: FactRule -> [FactRule] -> Maybe FactRule
findRule (Fact f _) rs = let ret = filter (isRuleFor f) rs in
	if null ret then Nothing else Just $ head ret

isRuleFor :: Function -> FactRule -> Bool
isRuleFor f0 (Rule (Fact f1 _) _) = f0 == f1
isRuleFor _ _ = False

apply :: [Term] -> FactRule -> [FactRule]
apply ts2 (Rule (Fact _ ts1) fs) = map (changeFactTerms ts1 ts2) fs

changeFactTerms :: [Term] -> [Term] -> FactRule -> FactRule
changeFactTerms ts1 ts2 (Fact f ts) = Fact f $ changeTerms ts1 ts2 ts

changeTerms :: [Term] -> [Term] -> [Term] -> [Term]
changeTerms [] _ ts = ts
changeTerms _ [] ts = ts
changeTerms (t1 : ts1) (t2 : ts2) ts =
	changeTerms ts1 ts2 $ changeTerm t1 t2 ts

changeTerm _ _ [] = []
changeTerm t1 t2 (t : ts)
	| t == t1 = t2 : changeTerm t1 t2 ts
	| otherwise = t : changeTerm t1 t2 ts

matchTerms :: [Term] -> [Term] -> Maybe [(Term, Term)]
matchTerms [] _ = Just []
matchTerms _ [] = Just []
matchTerms (t1 : t1s) (t2 : t2s) = case matchTerm t1 t2 of
	Nothing -> Nothing
	Just Nothing -> matchTerms t1s t2s
	Just (Just p) -> (p :) `fmap` matchTerms t1s t2s

matchTerm :: Term -> Term -> Maybe (Maybe (Term, Term))
matchTerm t1@(VKOhA _) t2 = Just $ Just (t1, t2)
matchTerm t1 t2@(VKOhA _) = Just $ Just (t1, t2)
matchTerm t1 t2
	| t1 == t2 = Just Nothing
	| otherwise = Nothing

data FactRule
	= Fact Function [Term]
	| Rule FactRule [FactRule]
	deriving (Show, Eq)

data Function
	= Brivla String
	| GOhA String
	| NA Function
	deriving (Show, Eq)

data Term
	= LA String
	| LO String
	| KOhA String
	| VKOhA String
	deriving (Show, Eq)

sampleQ = Fact (Brivla "nelci") [VKOhA "da", VKOhA "de"]
sampleQ2 = Fact (Brivla "pendo") [LA "ualeis", LA "gromit"]
sampleQ3 = Fact (Brivla "pendo") [LA "ualeis", LA "uendolen"]
sampleQ4 = Fact (Brivla "nelci") [LA "ualeis", LO "cirla"]
sampleQ5 = Fact (Brivla "pendo") [LA "ualeis", LA "ualeis"]
sampleQ6 = Fact (Brivla "pendo") [LO "cirla", LA "ualeis"]
sampleQ7 = Fact (GOhA "du") [LA "ualeis", LA "ualeis"]
sampleQ8 = Fact (NA $ GOhA "du") [LA "ualeis", LA "gromit"]

sample = [
	Fact (Brivla "nelci") [LA "ualeis",LO "cirla"],
	Fact (Brivla "nelci") [LA "gromit",LO "cirla"],
	Fact (Brivla "nelci") [LA "uendolen",LO "lanme"],
	Rule (Fact (Brivla "pendo") [VKOhA "da",VKOhA "de"]) [
		Fact (NA (GOhA "du")) [VKOhA "da",VKOhA "de"],
		Fact (Brivla "nelci") [VKOhA "da",VKOhA "di"],
		Fact (Brivla "nelci") [VKOhA "de",VKOhA "di"]]]
