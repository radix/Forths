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

program2 :: Forth ()
program2 = do
  pushInt 3
  pushInt 2
  word "add"
  pushString "abc"
  pushInt 1
  word "index"
  end


customWords :: Forth ()
customWords = do
  word ":"
  word "add5"
  pushInt 5
  word "add"
  word ";"
  pushInt 5
  word "add5"
  word "add5"
  end

main = do
  putStrLn $ pretty program
  putStrLn $ show $ eval program2 []
