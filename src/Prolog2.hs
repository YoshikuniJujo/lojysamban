module Prolog2 (
	ask,
	Fact,
	Rule(..),
	Term(..),
	TwoD(..),
	Result
) where

import PrologTools
import Data.Maybe

ask :: (TwoD sc, Eq sc, Eq s) =>
	sc -> Result sc s -> Fact sc s -> [Rule sc s] -> [Result sc s]
ask sc ret q rs =
	concat $ zipWith (\sc' r -> askrule sc' ret q r rs) (iterate next $ down sc) rs

askrule :: (TwoD sc, Eq sc, Eq s) =>
	sc -> Result sc s -> Fact sc s -> Rule sc s -> [Rule sc s] -> [Result sc s]
askrule sc ret q (Rule fact _ facts notFacts) rs =
	filter (`checkAll` nots) ret'
	where
	ret' = foldl (\rets (sc', f) -> rets >>= \r' -> ask sc' r' f rs) r0 $
		zip (iterate next sc0) $ map (const . ($ sc0)) facts
	sc0 = down sc
	r0 = case q sc `unification` fact sc0 of
		Nothing -> []
		Just r0' -> maybeToList $ ret `merge` r0'
	nots = concatMap (flip (notAsk sc0) rs . const . ($sc0)) notFacts
