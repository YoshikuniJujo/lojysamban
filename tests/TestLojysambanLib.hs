module TestLojysambanLib (lojysambanLib) where

import Test.HUnit
import Control.Applicative
import System.IO.Unsafe
import LojysambanLib
import Crypto.Hash.MD5

main = maybe (return ()) putStrLn $ ask nonkanynacQ1 nonkanynac

lojysambanLib = "test of LojysambanLib" ~: test [
	ask pendoQ1 pendo ~?= pendoA1,
	ask patfuQ1 patfu ~?= patfuA1,
	(hash . read . show . drop 4) <$> ask skariQ1 skari ~?= skariA1,
	ask jminaQ1 jmina ~?= Just ".i li 3",
	ask nonkanynacQ2 nonkanynac ~?= Just ".i go'i",
	ask nonkanynacQ3 nonkanynac ~?= Just ".i nago'i",
	ask nonkanynacQ4 nonkanynac ~?= Just ".i nago'i"
 ]

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

nonkanynac = unsafePerformIO $ readRules <$> readFile "examples/nonkanynac.jbo"
nonkanynacQ1 =
	"livo ce'o lipa ce'o lirelo'o ce'o .abu ce'o " ++
	"lire ce'o licilo'o ce'o by. ce'o lipa ce'o " ++
	"lipalo'o ce'o dy. ce'o lici ce'o livolo'o ce'o " ++
	".ebu ce'o livo ce'o lipa ce'o lire " ++
	"nonkanyna'u ma"
nonkanynacQ2 = "la .iocikun. ce'o la .ituk. ce'o la manam. cu datsi'u"
nonkanynacQ3 = "la .iocikun. ce'o la .ituk. ce'o la .iocikun. cu datsi'u"
nonkanynacQ4 = "la .ituk. ce'o la .iocikun. ce'o la .iocikun. cu datsi'u"
