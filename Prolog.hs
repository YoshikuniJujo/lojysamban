module Prolog where

import Unif
import Data.Maybe
import Control.Applicative

type Fact sc s = [Term sc s]
data Rule sc s = Rule (Fact sc s) [Fact sc s]

fact1, fact2 :: Fact String String
fact1 = [Con "likes", Con "wallace", Con "cheese"]
fact2 = [Con "likes", Con "grommit", Con "cheese"]
fact4 = [Con "likes", Var "" "X", Var "" "Z"]
fact5 = [Con "likes", Var "" "Y", Var "" "Z"]

rule1 :: Rule String String
rule1 = Rule fact1 []
rule2 = Rule fact2 []
rule3 = Rule [Con "friends", Var "" "X", Var "" "Y"] [fact4, fact5]
rules = [rule1, rule2, rule3]

q1, q2 :: Fact String String
q1 = [Con "likes", Var "" "X", Con "cheese"]
q2 = [Con "friends", Con "wallace", Con "grommit"]

ask :: (Eq sc, Eq s) => Fact sc s -> [Rule sc s] -> [Result sc s]
ask q rs = concatMap (flip (askRule q) rs) rs

askRule :: (Eq sc, Eq s) => Fact sc s -> Rule sc s -> [Rule sc s] -> [Result sc s]
askRule q r@(Rule fact facts) rs =
	foldl mrg (maybeToList $ unification q fact) $ map (flip ask rs) facts
	where
	mrg ff gg = do
		f <- ff
		g <- gg
		case merge f g of
			Nothing -> fail "not return"
			Just r -> return r
