module Prolog2 (
	ask,
	Fact,
--	Unify(..),
	Rule(..),
	Term(..),
	TwoD(..)
) where

import PrologTools
import Unif
import Data.Maybe

ask :: (TwoD sc, Eq sc, Eq s) =>
	sc -> Result sc s -> Fact sc s -> [Rule sc s] -> [Result sc s]
ask sc ret q rs =
	concat $ zipWith (\sc r -> askrule sc ret q r rs) (iterate next $ down sc) rs

askrule :: (TwoD sc, Eq sc, Eq s) =>
	sc -> Result sc s -> Fact sc s -> Rule sc s -> [Rule sc s] -> [Result sc s]
askrule sc ret q r@(Rule fact _ facts notFacts) rs = do
	let r0 = case (q sc) `unification` (fact sc) of
		Nothing -> []
		Just r0' -> maybeToList $ ret `merge` r0'
	foldl (\rets f -> rets >>= \r -> ask sc r f rs) r0 $
		map (const . ($ sc)) facts