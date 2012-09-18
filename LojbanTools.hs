module LojbanTools (
	getSentences,
	headTerms,
	bridiTail,
	selbri,
	tailTerms,
	snd3
) where

import Language.Lojban.Parser hiding (LA, Brivla, KOhA, GOhA, NA, LerfuString)
import qualified Language.Lojban.Parser as P
import System.Environment
import Data.Maybe
import Data.Either
import Data.List

getSentences :: Sentence -> [Sentence]
getSentences (IText_1 _ _ _ _ (Just t)) = getSentences t
getSentences (StatementI s1 ss) = s1 : catMaybes (map (\(_, _, s) -> s) ss)
getSentences tbt@(TermsBridiTail _ _ _ _) = [tbt]
getSentences o = error $ show o

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

readSumtiTail :: SumtiTail -> String
readSumtiTail (SelbriRelativeClauses (P.Brivla (_, n, _) _) _) = n
readSumtiTail st = show st

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
