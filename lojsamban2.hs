module Main where

import LojbanTools
import Prolog
import Language.Lojban.Parser hiding (LA, Brivla, KOhA, GOhA, NA)
import qualified Language.Lojban.Parser as P
import System.Environment
import Data.Maybe

main :: IO ()
main = do
	[fn] <- getArgs
	src <- readFile fn
	let	Right p = parse src
		rules = map readSentence $ getSentences p
	q <- (readSentenceFact . either (error "bad") id . parse) `fmap` getLine
--	print (rules :: [Rule String Atom])
--	print (q :: Fact Scope Atom)
	print $ ask q rules

data Atom
	= LA String
	| LO String
	| KOhA String
	| Brivla String
	| GOhA String
	| NA Atom
	deriving (Show, Eq)

type Scope = String

readSumti :: Sumti -> Term Scope Atom
readSumti (P.LA (_, "la", _) _ _ ns _) = Con $ LA $ concat $ map snd3 ns
readSumti (P.LALE (_, "lo", _) _ st _ _) = Con $ LO $ show st
readSumti (P.KOhA (_, k, _) _) = Var "" $ KOhA k

readSelbriAtom (P.GOhA (_, n, _) _ _) = GOhA n

readSelbri :: Selbri -> Term Scope Atom
readSelbri (P.Brivla (_, n, _) _) = Con $ Brivla n
readSelbri (P.GOhA (_, n, _) _ _) = Con $ GOhA n
readSelbri (P.NA (_, "na", _) _ s) = Con $ NA $ readSelbriAtom s

readSentenceFact :: Sentence -> Fact Scope Atom
readSentenceFact s@(TermsBridiTail _ _ _ _) = (f : h ++ t)
	where
	h = map readSumti $ headTerms s
	f = readSelbri $ selbri $ bridiTail s
	t = map readSumti $ tailTerms $ bridiTail s

readSentence :: Sentence -> Rule Scope Atom
readSentence s@(TermsBridiTail _ _ _ _) = Rule (f : h ++ t) [] []
	where
	h = map readSumti $ headTerms s
	f = readSelbri $ selbri $ bridiTail s
	t = map readSumti $ tailTerms $ bridiTail s
readSentence (IJoikJek s [r]) = Rule (readSentenceFact s) [] (getRule r)

getRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) = readTUhE t

readTUhE (TUhE _ _ _ t _ _) = map readSentenceFact $ getSentences t
