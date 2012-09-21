{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Main where

import LojysambanLib
import System.Environment
import System.IO
import Control.Monad.Tools

main :: IO ()
main = do
	args <- getArgs
	src <- case args of
		[] -> readFacts
		[fn] -> readFile fn
		_ -> error "bad arguments"
	let	rules = readRules src
	doWhile_ $ do
		putStr ".i "
		hFlush stdout
		l <- getLine
		let	p' = either (error . show) id $ parse l
		if isCOhO p' then return False else
			ask1 (readQuestion l) rules >> return True
