module Main where

import System.IO(hFlush, stdout)
import System.Environment(getArgs)
import Control.Monad.Tools(doWhile_)
import Data.List(isInfixOf)

import LojysambanLib(ask1, readQuestion, readRules, isCOhO)

main :: IO ()
main = do
	args <- getArgs
	src <- case args of
		[] -> readFacts
		[fn] -> readFile fn
		_ -> error "bad arguments"
	doWhile_ $ do
		l <- putStr ".i " >> hFlush stdout >> getLine
		if isCOhO l then return False else do
			ask1 (readQuestion l) (readRules src)
			return True

readFacts :: IO String
readFacts = do
	l <- getLine
	if "fa'o" `isInfixOf` l then return l else do
		ls <- readFacts
		return $ l ++ ls
