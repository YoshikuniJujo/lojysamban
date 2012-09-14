module Main where

import LojbanTools
import Prolog
import Language.Lojban.Parser hiding (LA, Brivla, KOhA, GOhA, NA)
import qualified Language.Lojban.Parser as P
import System.Environment
import Data.Maybe
import Data.Either
import Data.List

main :: IO ()
main = do
	[fn] <- getArgs
	src <- readFile fn
	let	Right p = parse src
		rules = map (readSentence "rules") $ getSentences p
	Left q <- (readSentenceFact "top" . either (error "bad") id . parse)
		`fmap` getLine
	let	answer = ask q rules
--	print (rules :: [Rule String Atom])
--	print (q :: Fact Scope Atom)
	putStrLn $ showAnswerAll answer

showAnswerAll a = if null a then "nago'i" else
	intercalate " .a " $ map showAnswer $ map (lookupMA . onlyTop) a

lookupMA = map snd . filter ((Var "top" (KOhA "ma") `elem`) . fst)

showAnswer as = if null as then "go'i" else showLA $ head as

showLA (Just (Con (LA n))) = "la " ++ n

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
	| NA Atom
	deriving (Show, Eq)

type Scope = String

readSumti :: Scope -> Sumti -> Term Scope Atom
readSumti sc (P.LA (_, "la", _) _ _ ns _) = Con $ LA $ concat $ map snd3 ns
readSumti sc (P.LALE (_, "lo", _) _ st _ _) = Con $ LO $ readSumtiTail st
readSumti sc (P.KOhA (_, k, _) _) = Var sc $ KOhA k

readSumtiTail :: SumtiTail -> String
readSumtiTail (SelbriRelativeClauses (P.Brivla (_, n, _) _) _) = n
readSumtiTail st = show st

readSelbriAtom (P.GOhA (_, n, _) _ _) = GOhA n

readSelbri :: Selbri -> Either (Term Scope Atom) (Term Scope Atom)
readSelbri (P.Brivla (_, n, _) _) = Left $ Con $ Brivla n
readSelbri (P.GOhA (_, n, _) _ _) = Left $ Con $ GOhA n
readSelbri (P.NA (_, "na", _) _ s) = Right $ Con $ readSelbriAtom s

readSentenceFact :: Scope -> Sentence -> Either (Fact Scope Atom) (Fact Scope Atom)
readSentenceFact sc s@(TermsBridiTail _ _ _ _) =
	either (Left . (: (h ++ t))) (Right . (: (h ++ t))) f
	where
	h = map (readSumti sc) $ headTerms s
	f = readSelbri $ selbri $ bridiTail s
	t = map (readSumti sc) $ tailTerms $ bridiTail s

readSentence :: Scope -> Sentence -> Rule Scope Atom
readSentence sc s@(TermsBridiTail _ _ _ _) = Rule (f : h ++ t) [] [] []
	where
	h = map (readSumti sc) $ headTerms s
	Left f = readSelbri $ selbri $ bridiTail s
	t = map (readSumti sc) $ tailTerms $ bridiTail s
readSentence sc (IJoikJek s [r]) = Rule f [] (getRule sc r) (getNotRule sc r)
	where
	Left f = readSentenceFact sc s

getRule sc (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) =
	lefts $ readTUhE sc t
getNotRule sc (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) =
	rights $ readTUhE sc t

readTUhE sc (TUhE _ _ _ t _ _) = map (readSentenceFact sc) $ getSentences t
