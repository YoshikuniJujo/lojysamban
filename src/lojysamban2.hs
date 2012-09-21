{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Main where

import LojbanTools
import Prolog2
import Language.Lojban.Parser hiding (LA, Brivla, KOhA, GOhA, NA, LerfuString, LI)
import qualified Language.Lojban.Parser as P
import System.Environment
import Data.Maybe
import Data.Either
import Data.List
import Control.Monad
import Control.Arrow
import Control.Applicative
import System.IO

main :: IO ()
main = do
	args <- getArgs
	src <- case args of
		[] -> readFacts
		[fn] -> readFile fn
	let	Right p = parse src
		rules = map readSentence $ getSentences p
	whileJust getAsk $ flip ask1 rules

readFacts :: IO String
readFacts = do
	l <- getLine
	if "fa'o" `isInfixOf` l then return l else do
		ls <- readFacts
		return $ l ++ ls

whileJust :: IO (Maybe a) -> (a -> IO b) -> IO ()
whileJust test action = do
	p <- test
	case p of
		Just x -> action x >> whileJust test action
		Nothing -> return ()

getAsk :: IO (Maybe (Fact Scope Atom))
getAsk = do
	putStr ".i "
	hFlush stdout
	p <- (either (error . show) id . parse) `fmap` getLine
	let	Left q = readSentenceFact p
	return $ if isCOhO p then Nothing else Just q

isCOhO (TopText _ _ [VocativeSumti [(_, "co'o", _)] _ _] _ _ _) = True
isCOhO _ = False

ask1 :: Fact Scope Atom -> [Rule Scope Atom] -> IO ()
ask1 q rules = do
	let	answer = ask [] [] q rules
	let	answer2_1 = unwords $ intersperse ".ija" $ map unwords $ filter ((> 2) . length) $
			map ((\ret -> intersperse ".ije" ret) . map showPair . filter (not . isMA . fst) . regularization . onlyTopVars) answer
		answer2 = unwords $ intersperse ".ija" $ map unwords $ filter ((> 2) . length) $
			map ((\ret -> "tu'e" : intersperse ".ije" ret ++ ["tu'u"]) . map showPair . filter (not . isMA . fst) . regularization . onlyTopVars) answer
	putStr ".i "
	putStr $ case answer of
		[] -> "nago'i\n"
		_ -> case intersperse ".a" $ catMaybes $ (flip map) (map maValue answer) $ (showAtom <$>) of
			[] -> if null answer2 then "go'i\n" else ""
			m -> unwords m ++ "\n"
	if null answer2 then return () else
		if length answer == 1 then putStrLn answer2_1 else putStrLn answer2

showAtom :: Atom -> String
showAtom (LA n) = "la " ++ n
showAtom (LO n) = "lo " ++ n
showAtom (LI n) = "li " ++ show n

maValue :: Result Scope Atom -> Maybe Atom
maValue r = case filter (not . null . fst) $ map (first $ filter isMA) r of
	[] -> Nothing
	((_, tv) : _) -> flip (<$>) tv $ \tv' -> case tv' of
		Con v -> v
		List vs -> error $ show vs

isMA :: Term Scope Atom -> Bool
isMA (Var [_] (KOhA "ma")) = True
isMA _ = False

showAnswerAll a = if null a then "nago'i" else
	intercalate " .a " $ map showAnswer $ map (lookupMA . onlyTop) a

showPair :: (Term Scope Atom, Term Scope Atom) -> String
showPair (Var _ (KOhA k), Con (LO n)) = k ++ " du lo " ++ n
showPair (Var _ (LerfuString l), Con (LO n)) = l ++ " du lo " ++ n
showPair (Var _ (KOhA k), Con (LA n)) = k ++ " du la " ++ n
showPair (Var _ (LerfuString l), Con (LA n)) = l ++ " du la " ++ n
showPair (Var _ (KOhA k), Con (LI n)) = k ++ " du li " ++ show n
showPair (Var _ (LerfuString l), Con (LI n)) = l ++ " du li " ++ show n
showPair o = show o

regularization :: Result sc s -> [(Term sc s, Term sc s)]
regularization [] = []
regularization ((_, Nothing) : rest) = regularization rest
regularization ((vars, Just val) : rest) = map (, val) vars ++ regularization rest

onlyTopVars :: Result Scope s -> Result Scope s
onlyTopVars = filter (not . null . fst) . map (first $ filter isTopVar)

isTopVar :: Term Scope s -> Bool
isTopVar (Var [_] _) = True
isTopVar _ = False

lookupMA = map snd . filter ((Var "top" (KOhA "ma") `elem`) . fst)

showAnswer as = if null as then "go'i" else showLA $ head as

showLA (Just (Con (LA n))) = "la " ++ n
showLA (Just (Con (LO n))) = "lo " ++ n

onlyTop = filter (not . null . fst) .
	map (\(vars, val) -> (filter isTop vars, val))

isTop :: Term String s -> Bool
isTop (Var "top" _) = True
isTop _ = False

data Atom
	= LA String
	| LO String
	| KOhA String
	| Brivla String
	| GOhA String
	| LerfuString String
	| LI Int
	deriving (Show, Eq)

type Scope = [Int]

instance TwoD [Int] where
	next (n : ns) = n + 1 : ns
	down ns = 0 : ns

readSumti :: Scope -> Sumti -> Term Scope Atom
readSumti sc (P.LA (_, "la", _) _ _ ns _) = Con $ LA $ intercalate "." $ map snd3 ns
readSumti sc (P.LALE (_, "lo", _) _ st _ _) = Con $ LO $ readSumtiTail st
readSumti sc (P.KOhA (_, k, _) _) = Var sc $ KOhA k
readSumti sc (P.LerfuString s _ _) = Var sc $ LerfuString $ concatMap snd3 s
readSumti sc (P.LI (_, "li", _) _ (Number ns _ _) _ _) = Con $ LI $ readNumber ns
readSumti sc (JoikEkSumti s ss) = List $ readSumti sc s : readCEhOTail sc ss
readSumti _ o = error $ show o

readCEhOTail :: Scope -> [(JoikJek, [Free], Sumti)] -> [Term Scope Atom]
readCEhOTail sc [] = []
readCEhOTail sc ((JOI _ (_, "ce'o", _) _, _, s) : rest) =
	readSumti sc s : readCEhOTail sc rest

readNumber :: [([String], String, [[([String], String)]])] -> Int
readNumber = readTen 0 . map snd3

paList = [
	("no", 0), ("pa", 1), ("re", 2), ("ci", 3), ("vo", 4),
	("0", 0), ("1", 1), ("2", 2), ("3", 3), ("4", 4)]

readTen :: Int -> [String] -> Int
readTen ret [] = ret
readTen ret (n : rest) = readTen (ret * 10 + fromJust (lookup n paList)) rest

readSumtiTail :: SumtiTail -> String
readSumtiTail (SelbriRelativeClauses (P.Brivla (_, n, _) _) _) = n
readSumtiTail st = show st

readSelbriAtom (P.GOhA (_, n, _) _ _) = GOhA n

readSelbri :: Selbri -> Either (Term Scope Atom) (Term Scope Atom)
readSelbri (P.Brivla (_, n, _) _) = Left $ Con $ Brivla n
readSelbri (P.GOhA (_, n, _) _ _) = Left $ Con $ GOhA n
readSelbri (P.NA (_, "na", _) _ s) = Right $ Con $ readSelbriAtom s

readSentenceFact :: Sentence -> Either (Fact Scope Atom) (Fact Scope Atom)
readSentenceFact s@(TermsBridiTail _ _ _ _) =
	either (\lf -> Left $ \sc -> lf : (h sc ++ t sc))
		(\rf -> Right $ \sc -> rf : (h sc ++ t sc)) f
	where
	h sc = map (readSumti sc) $ headTerms s
	f = readSelbri $ selbri $ bridiTail s
	t sc = map (readSumti sc) $ tailTerms $ bridiTail s
readSentenceFact (TopText _ _ _ _ (Just s) _) = readSentenceFact s
readSentenceFact o = error $ show o

readSentence :: Sentence -> Rule Scope Atom
readSentence s@(TermsBridiTail _ _ _ _) = Rule (\sc -> f : h sc ++ t sc) [] [] []
	where
	h sc = map (readSumti sc) $ headTerms s
	Left f = readSelbri $ selbri $ bridiTail s
	t sc = map (readSumti sc) $ tailTerms $ bridiTail s
readSentence (IJoikJek s [r]) = Rule f [] (getRule r) (getNotRule r)
	where
	Left f = readSentenceFact s

getRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) =
	lefts $ readRule t
getNotRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) =
	rights $ readRule t

readRule t@(TUhE _ _ _ _ _ _) = readTUhE t
readRule t = [readSentenceFact t]

readTUhE (TUhE _ _ _ t _ _) = map readSentenceFact $ getSentences t
readTUhE o = error $ show o
