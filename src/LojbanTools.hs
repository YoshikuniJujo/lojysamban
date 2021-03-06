module LojbanTools (
	getSentences,
	headTerms,
	bridiTail,
	selbri,
	tailTerms,
	snd3
) where

import Language.Lojban.Parser hiding (LA, Brivla, KOhA, GOhA, NA, LerfuString)
import Data.Maybe

getSentences :: Sentence -> [Sentence]
getSentences (TopText _ _ _ _ (Just t) _) = getSentences t
getSentences (IText_1 _ _ _ _ (Just t)) = getSentences t
getSentences (StatementI s1 ss) = s1 : mapMaybe (\(_, _, s) -> s) ss
getSentences tbt@(TermsBridiTail{}) = [tbt]
getSentences o = error $ "getSentences: " ++ show o

headTerms :: Sentence -> [Sumti]
headTerms (TermsBridiTail ts _ _ _) = ts
headTerms _ = []

bridiTail :: Sentence -> Sentence
bridiTail (TermsBridiTail _ _ _ bt) = bt
bridiTail o = o

selbri :: Sentence -> Selbri
selbri (Selbri s) = s
selbri (SelbriTailTerms s _ _ _) = s
selbri t = error $ show t -- P.Brivla ([], "", []) []

tailTerms :: Sentence -> [Sumti]
tailTerms (SelbriTailTerms _ ts _ _) = ts
tailTerms _ = []

{-
readSumtiTail :: SumtiTail -> String
readSumtiTail (SelbriRelativeClauses (P.Brivla (_, n, _) _) _) = n
readSumtiTail st = show st
-}

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
