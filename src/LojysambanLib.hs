{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}

module LojysambanLib (ask, readRules, end) where

import LojbanTools
import Prolog2 hiding (ask)
import qualified Prolog2 as P
import Language.Lojban.Parser hiding (LA, Brivla, KOhA, GOhA, NA, LerfuString, LI)
import qualified Language.Lojban.Parser as P
import Data.Maybe
import Data.Either
import Data.List
import Control.Arrow
import Control.Applicative

end = isFAhO
ask = question

isFAhO :: String -> Bool
isFAhO src
	| Right (TopText _ _ _ _ _ (Just (_, "fa'o"))) <- parse src = True
	| otherwise = False

readRules :: String -> [Rule Scope Atom]
readRules = map readSentence . getSentences . (\(Right p) -> p) . parse

readQuestion :: String -> Fact Scope Atom
readQuestion =
	(\(Left q) -> q) . readSentenceFact . either (error . show) id . parse

isCOhO :: String -> Bool
isCOhO = isCOhO' . either (error . show) id . parse

isCOhO' :: Sentence -> Bool
isCOhO' (TopText _ _ [VocativeSumti [(_, "co'o", _)] _ _] _ _ _) = True
isCOhO' _ = False

question :: String -> [Rule Scope Atom] -> Maybe String
question q r = if isCOhO q then Nothing else Just $ question' q r

question' :: String -> [Rule Scope Atom] -> String
question' = ask1 . readQuestion

ask1 :: Fact Scope Atom -> [Rule Scope Atom] -> String
ask1 q rules =
	let	answer = P.ask [] [] q rules
		answer2_1 = unwords $ intersperse ".ija" $ map unwords $ filter ((> 2) . length) $
			map (intersperse ".ije" . map showPair .
			filter (not . isMA . fst) . regularization . onlyTopVars) answer
		answer2 = unwords $ intersperse ".ija" $ map unwords $ filter ((> 2) . length) $
			map ((\ret -> "tu'e" : intersperse ".ije" ret ++ ["tu'u"]) .
				map showPair . filter (not . isMA . fst) .
					regularization . onlyTopVars) answer
--	print answer
--	print answer2_1
--	print answer2
--	putStr ".i "
		result1 = ".i " ++ case answer of
			[] -> "nago'i"
			_ -> case intersperse ".a" $ mapMaybe
				((showAtom <$>) . maValue) answer of
				[] -> if null answer2 then "go'i" else ""
				m -> unwords m
	in
	result1 ++ if (null answer2) then "" else
		if length answer == 1 then answer2_1 else answer2

showAtom :: Atom -> String
showAtom (LA n) = "la " ++ n
showAtom (LO n) = "lo " ++ n
showAtom (LI n) = "li " ++ show n
showAtom (ListA []) = "lo kunti"
showAtom (ListA ns) = unwords $ intersperse "ce'o" $ map showAtom ns
showAtom o = error $ "showAtom: " ++ show o

maValue :: Result Scope Atom -> Maybe Atom
maValue r = case filter (not . null . fst) $ map (first $ filter isMA) r of
	[] -> Nothing
	((_, tv) : _) -> flip (<$>) tv $ \tv' -> case tv' of
		Con v -> v
		List vs -> ListA $ map (\(Con v) -> v) vs
		o -> error $ "maValue: " ++ show o

isMA :: Term Scope Atom -> Bool
isMA (Var [_] (KOhA "ma")) = True
isMA _ = False

showPair :: (Term Scope Atom, Term Scope Atom) -> String
showPair (Var _ (KOhA k), Con (LO n)) = k ++ " du lo " ++ n
showPair (Var _ (LerfuString l), Con (LO n)) = l ++ " du lo " ++ n
showPair (Var _ (KOhA k), Con (LA n)) = k ++ " du la " ++ n
showPair (Var _ (LerfuString l), Con (LA n)) = l ++ " du la " ++ n
showPair (Var _ (KOhA k), Con (LI n)) = k ++ " du li " ++ show n
showPair (Var _ (LerfuString l), Con (LI n)) = l ++ " du li " ++ show n
showPair (Var _ (LerfuString l), List vs) = l ++ " du " ++ unwords (intersperse "ce'o" $ map showTerm vs)
showPair o = show o

showTerm :: Term Scope Atom -> String
showTerm (Con (LI n)) = "li " ++ show n
showTerm (Con (LA n)) = "la " ++ n
showTerm o = error $ "showTerm: " ++ show o

regularization :: Result sc s -> [(Term sc s, Term sc s)]
regularization [] = []
regularization ((_, Nothing) : rest) = regularization rest
regularization ((vars, Just val) : rest) = map (, val) vars ++ regularization rest

onlyTopVars :: Result Scope s -> Result Scope s
onlyTopVars = filter (not . null . fst) . map (first $ filter isTopVar)

isTopVar :: Term Scope s -> Bool
isTopVar (Var [_] _) = True
isTopVar _ = False

data Atom
	= LA String
	| LO String
	| KOhA String
	| Brivla String
	| GOhA String
	| LerfuString String
	| LI Int
	| ListA [Atom]
	deriving (Show, Eq)

type Scope = [Int]

instance TwoD [Int] where
	next (n : ns) = n + 1 : ns
	next _ = error "empty"
	down ns = 0 : ns

readSumti :: Scope -> Sumti -> Term Scope Atom
readSumti _ (P.LA (_, "la", _) _ _ ns _) = Con $ LA $ intercalate "." $ map snd3 ns
readSumti _ (P.LALE (_, "lo", _) _ (SelbriRelativeClauses (P.Brivla (_, "kunti", _) _) _) _ _) = List []
readSumti sc (P.LALE (_, "lo", _) _ (SelbriRelativeClauses (Linkargs (P.Brivla (_, "selzilvi'u", _) _) (BE (_, "be", _) _ s1 (Just (BEI (_, "bei", _) _ s2 _))
	_ _)) _) _ _) = Cons (readSumti sc s1) (readSumti sc s2)
readSumti _ (P.LALE (_, "lo", _) _ st _ _) = Con $ LO $ readSumtiTail st
readSumti sc (P.KOhA (_, k, _) _) = Var sc $ KOhA k
readSumti sc (P.LerfuString s _ _) = Var sc $ LerfuString $ concatMap snd3 s
readSumti _ (P.LI (_, "li", _) _ (Number ns _ _) _ _) = Con $ LI $ readNumber ns
readSumti sc (JoikEkSumti s ss) = List $ readSumti sc s : readCEhOTail sc ss
readSumti _ o = error $ show o

readCEhOTail :: Scope -> [(JoikJek, [Free], Sumti)] -> [Term Scope Atom]
readCEhOTail _ [] = []
readCEhOTail sc ((JOI _ (_, "ce'o", _) _, _, s) : rest) =
	readSumti sc s : readCEhOTail sc rest
readCEhOTail _ o = error $ "readCEhOTail: " ++ show o

readNumber :: [([String], String, [[([String], String)]])] -> Int
readNumber = readTen 0 . map snd3

paList :: [(String, Int)]
paList = [
	("no", 0), ("pa", 1), ("re", 2), ("ci", 3), ("vo", 4),
	("0", 0), ("1", 1), ("2", 2), ("3", 3), ("4", 4)]

readTen :: Int -> [String] -> Int
readTen = foldl (\r n -> r * 10 + fromJust (lookup n paList))

readSumtiTail :: SumtiTail -> String
readSumtiTail (SelbriRelativeClauses (P.Brivla (_, n, _) _) _) = n
readSumtiTail st = show st

readSelbriAtom :: Selbri -> Atom
readSelbriAtom (P.GOhA (_, n, _) _ _) = GOhA n
readSelbriAtom o = error $ "readSelbriAtom: " ++ show o

readSelbri :: Selbri -> Either (Term Scope Atom) (Term Scope Atom)
readSelbri (P.Brivla (_, n, _) _) = Left $ Con $ Brivla n
readSelbri (P.GOhA (_, n, _) _ _) = Left $ Con $ GOhA n
readSelbri (P.NA (_, "na", _) _ s) = Right $ Con $ readSelbriAtom s
readSelbri o = error $ "readSelbri: " ++ show o

readSentenceFact :: Sentence -> Either (Fact Scope Atom) (Fact Scope Atom)
readSentenceFact s@(TermsBridiTail{}) =
	either (\lf -> Left $ \sc -> lf : (h sc ++ t sc))
		(\rf -> Right $ \sc -> rf : (h sc ++ t sc)) f
	where
	h sc = map (readSumti sc) $ headTerms s
	f = readSelbri $ selbri $ bridiTail s
	t sc = map (readSumti sc) $ tailTerms $ bridiTail s
readSentenceFact (TopText _ _ _ _ (Just s) _) = readSentenceFact s
readSentenceFact o = error $ show o

readSentence :: Sentence -> Rule Scope Atom
readSentence s@(TermsBridiTail{}) = Rule (\sc -> f : h sc ++ t sc) [] [] []
	where
	h sc = map (readSumti sc) $ headTerms s
	Left f = readSelbri $ selbri $ bridiTail s
	t sc = map (readSumti sc) $ tailTerms $ bridiTail s
readSentence (IJoikJek s [r]) = Rule f [] (getRule r) (getNotRule r)
	where
	Left f = readSentenceFact s
readSentence o = error $ "readSentence: " ++ show o

getRule :: (Show s, Show t) =>
	(s, JoikJek, t, Maybe Sentence) -> [Fact Scope Atom]
getRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) =
	lefts $ readRule t
getRule o = error $ "getRule: " ++ show o

getNotRule :: (Show s, Show t) =>
	(s, JoikJek, t, Maybe Sentence) -> [Fact Scope Atom]
getNotRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) =
	rights $ readRule t
getNotRule o = error $ "getNotRule: " ++ show o

readRule :: Sentence -> [Either (Fact Scope Atom) (Fact Scope Atom)]
readRule t@(TUhE {}) = readTUhE t
readRule t = [readSentenceFact t]

readTUhE :: Sentence -> [Either (Fact Scope Atom) (Fact Scope Atom)]
readTUhE (TUhE _ _ _ t _ _) = map readSentenceFact $ getSentences t
readTUhE o = error $ show o
