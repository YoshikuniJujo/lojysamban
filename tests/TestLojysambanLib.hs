module TestLojysambanLib (lojysambanLib) where

import Test.HUnit
import Control.Applicative
import System.IO.Unsafe
import LojysambanLib
import Crypto.Hash.MD5

pendo = unsafePerformIO $ readRules <$> readFile "examples/pendo.jbo"
pendoQ1 = "la .ualeis. pendo ma"
pendoA1 = Just ".i la gromit"

patfu = unsafePerformIO $ readRules <$> readFile "examples/patfu.jbo"
patfuQ1 = "ma dzena la jon.bois.jr."
patfuA1 = Just ".i la jon.bois.sr .a la zeb"

skari = unsafePerformIO $ readRules <$> readFile "examples/skari.jbo"
skariQ1 = "alabam. bu toldu'o misisip. bu boi joji'as. bu boi " ++
	"tenesis. bu boi florid. bu"
skariA1 = Just $ read "\"\179Gn|a\240\tV\194A;\211\&3q\246\254\""

lojysambanLib = "test of LojysambanLib" ~: test [
	ask pendoQ1 pendo ~?= pendoA1,
	ask patfuQ1 patfu ~?= patfuA1,
	(hash . read . show) <$> ask skariQ1 skari ~?= skariA1 ]
