module Main (main) where

import Forths
import Control.Monad.Free

program :: Forth ()
program = do
    pushInt 3
    pushInt 2
    word "add"
    pushInt 100
    word "add"
    end

main = do
	putStrLn $ pretty program