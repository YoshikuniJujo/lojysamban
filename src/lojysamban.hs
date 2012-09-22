module Main where

import System.IO(hFlush, stdout)
import System.Environment(getArgs)
import System.Exit(exitFailure)
import Control.Applicative((<$>))
import Control.Arrow((&&&))
import Control.Monad.Tools(doWhile, doWhile_)
import LojysambanLib(ask, readRules, end)

main :: IO ()
main = do
	args <- getArgs
	rules <- readRules <$> case args of
		[] -> doWhile "" $ \s -> (id &&& not . end) . (s ++) <$> getLine
		[fp] -> readFile fp
		_ -> putStrLn "Usage: lojysamban [FILEPATH]" >> exitFailure
	doWhile_ $ do
		q <- putStr ".i " >> hFlush stdout >> getLine
		maybe (return False) ((>> return True) . putStrLn) $ ask q rules
