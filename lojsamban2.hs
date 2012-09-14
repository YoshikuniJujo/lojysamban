module Main where

import LojbanTools
import Prolog
import Language.Lojban.Parser hiding (LA, Brivla, KOhA, GOhA, NA)
import qualified Language.Lojban.Parser as P
import System.Environment
import Data.Maybe
import Data.Either

main :: IO ()
main = do
	[fn] <- getArgs
	src <- readFile fn
	let	Right p = parse src
		rules = map readSentence $ getSentences p
	Left q <- (readSentenceFact . either (error "bad") id . parse) `fmap` getLine
	print (rules :: [Rule String Atom])
--	print (q :: Fact Scope Atom)
	putStrLn ""
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

readSelbri :: Selbri -> Either (Term Scope Atom) (Term Scope Atom)
readSelbri (P.Brivla (_, n, _) _) = Left $ Con $ Brivla n
readSelbri (P.GOhA (_, n, _) _ _) = Left $ Con $ GOhA n
readSelbri (P.NA (_, "na", _) _ s) = Right $ Con $ readSelbriAtom s

readSentenceFact :: Sentence -> Either (Fact Scope Atom) (Fact Scope Atom)
readSentenceFact s@(TermsBridiTail _ _ _ _) =
	either (Left . (: (h ++ t))) (Right . (: (h ++ t))) f
	where
	h = map readSumti $ headTerms s
	f = readSelbri $ selbri $ bridiTail s
	t = map readSumti $ tailTerms $ bridiTail s

readSentence :: Sentence -> Rule Scope Atom
readSentence s@(TermsBridiTail _ _ _ _) = Rule (f : h ++ t) [] [] []
	where
	h = map readSumti $ headTerms s
	Left f = readSelbri $ selbri $ bridiTail s
	t = map readSumti $ tailTerms $ bridiTail s
readSentence (IJoikJek s [r]) = Rule f [] (getRule r) (getNotRule r)
	where
	Left f = readSentenceFact s

getRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) = lefts $ readTUhE t
getNotRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) = rights $ readTUhE t

readTUhE (TUhE _ _ _ t _ _) = map readSentenceFact $ getSentences t
