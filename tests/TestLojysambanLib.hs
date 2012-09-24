module TestLojysambanLib (lojysambanLib) where

import Test.HUnit
import Control.Applicative
import System.IO.Unsafe
import LojysambanLib
import Crypto.Hash.MD5

lojysambanLib = "test of LojysambanLib" ~: test [
	ask pendoQ1 pendo ~?= pendoA1,
	ask patfuQ1 patfu ~?= patfuA1,
	(hash . read . show) <$> ask skariQ1 skari ~?= skariA1,
	ask jminaQ1 jmina ~?= Just ".i li 3" ]

pendo = unsafePerformIO $ readRules <$> readFile "examples/pendo.jbo"
pendoQ1 = "la .ualeis. pendo ma"
pendoA1 = Just ".i la gromit"

patfu = unsafePerformIO $ readRules <$> readFile "examples/patfu.jbo"
patfuQ1 = "ma dzena la jon.bois.jr."
patfuA1 = Just ".i la jon.bois.sr .a la zeb"

skari = unsafePerformIO $ readRules <$> readFile "examples/skari.jbo"
skariQ1 = "alabam. bu toldu'o misisip. bu boi joji'as. bu boi " ++
	"tenesis. bu boi florid. bu"
-- skariA1 = Just $ read "\"\179Gn|a\240\tV\194A;\211\&3q\246\254\""
skariA1 = Just $ read "\"\143\194\159\133X9\170\178\v\238\225\253\\\"\160Z\196\""

cmima = unsafePerformIO $ readRules <$> readFile "examples/cmima.jbo"
cmimaQ1 = "ma cmima la .as. ce'o la .yb."

jmina = unsafePerformIO $ readRules <$> readFile "examples/jmina.jbo"
jminaQ1 = "ma broda"
