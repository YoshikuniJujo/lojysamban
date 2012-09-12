import PrologLike
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
	let Right p = parse src
	let facts = map readSentence $ getSentences p
--	print facts
	q <- (readSentence . either (error "bad") id . parse) `fmap` getLine
	putStrLn $ case ask q facts of
		Just r -> fromMaybe "go'i" $ answerMa r
		Nothing -> "nago'i"
--	putStrLn $ if ask q facts then "go'i" else "nago'i"
	putStrLn $ show $ ask q facts
--	print q

answerMa :: [[(Term, Term)]] -> Maybe String
answerMa ps
	| null answers = Nothing
	| otherwise = Just $ intercalate " .e " $ catMaybes $ map answerMa1 ps
	where
	answers = catMaybes $ map answerMa1 ps

answerMa1 :: [(Term, Term)] -> Maybe String
answerMa1 ps = case lookup (VKOhA "ma") ps of
	Nothing -> Nothing
	Just (LA n) -> Just $ "la " ++ n
	Just (LO n) -> Just $ "lo " ++ n

getSentences :: Sentence -> [Sentence]
getSentences (IText_1 _ _ _ _ (Just t)) = getSentences t
getSentences (StatementI s1 ss) = s1 : catMaybes (map (\(_, _, s) -> s) ss)

headTerms :: Sentence -> [Sumti]
headTerms (TermsBridiTail ts _ _ _) = ts
headTerms _ = []

bridiTail :: Sentence -> Sentence
bridiTail (TermsBridiTail _ _ _ bt) = bt
bridiTail o = o

selbri :: Sentence -> Selbri
selbri (SelbriTailTerms s _ _ _) = s
selbri _ = P.Brivla ([], "", []) []

tailTerms :: Sentence -> [Sumti]
tailTerms (SelbriTailTerms _ ts _ _) = ts
tailTerms _ = []

function :: Selbri -> Function
function (P.Brivla (_, n, _) _) = Brivla n
function (P.GOhA (_, n, _) _ _) = GOhA n
function (P.NA (_, "na", _) _ s) = NA $ function s

readLALO :: Sumti -> Term
readLALO (P.LA (_, "la", _) _ _ ns _) = LA $ concat $ map ((++ ".") . snd3) ns
readLALO (P.LALE (_, "lo", _) _ st _ _) = LO $ readSumtiTail st
readLALO (P.KOhA (_, k@"ma", _) _) = VKOhA k
readLALO (P.KOhA (_, k@"da", _) _) = VKOhA k
readLALO (P.KOhA (_, k@"de", _) _) = VKOhA k
readLALO (P.KOhA (_, k@"di", _) _) = VKOhA k
readLALO (P.KOhA (_, k@"do", _) _) = VKOhA k
readLALO (P.KOhA (_, k, _) _) = KOhA k

readSumtiTail :: SumtiTail -> String
readSumtiTail (SelbriRelativeClauses (P.Brivla (_, n, _) _) _) = n
readSumtiTail st = show st

readSentence :: Sentence -> FactRule
readSentence s@(TermsBridiTail _ _ _ _) = Fact f $ h ++ t
	where
	h = map readLALO $ headTerms s
	f = function $ selbri $ bridiTail s
	t = map readLALO $ tailTerms $ bridiTail s
readSentence (IJoikJek s [r]) = Rule (readSentence s) (getRule r)

getRule (_, Jek _ _ (_, "ja", _) (Just (_, "nai", _)), _, Just t) = readTUhE t

readTUhE (TUhE _ _ _ t _ _) = map readSentence $ getSentences t

checkKOhA :: FactRule -> FactRule -> [(Term, Term)]
checkKOhA (Fact _ ts1) (Fact _ ts2) = zip ts1 ts2

{-
findFacts :: FactRule -> [FactRule] -> [FactRule]
findFacts (Fact f0 _) fs = filter (isFactFor f0) fs

isFactFor :: Function -> FactRule -> Bool
isFactFor f0 (Fact f1 _) = f0 == f1
isFactFor _ _ = False

ask :: FactRule -> [FactRule] -> Bool
ask (Fact (NA f) ts) facts = not $ ask (Fact f ts) facts
ask (Fact (GOhA "du") ts) _ = ts !! 0 == ts !! 1
ask q@(Fact f ts) facts
	| q `elem` facts = True
	| otherwise = case findRule q facts of
		Nothing -> False
		Just r -> and $ map (flip ask facts) $ apply ts r

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

changeTerm :: Term -> Term -> [Term] -> [Term]
changeTerm _ _ [] = []
changeTerm t1 t2 (t : ts)
	| t == t1 = t2 : changeTerm t1 t2 ts
	| otherwise = t : changeTerm t1 t2 ts

sampleRule = Rule (Fact (Brivla "pendo") [KOhA "da", KOhA "de"])
	[Fact (Brivla "nelci") [KOhA "da", KOhA "de"]]
-}

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y
