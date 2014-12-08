{-# LANGUAGE DeriveFunctor #-}

module Forths (
    Forth,
    Expr,
    pushInt,
    word,
    end,
    pretty
    ) where

import Control.Monad.Free

data Expr cont
  = FInt Int cont
  | FString String cont
  | FWord String cont
  | End
  deriving (Show, Functor)

type Forth = Free Expr

-- lift the data constructors into free versions (?)

pushInt :: Int -> Forth ()
pushInt n = liftF $ FInt n ()
word :: String -> Forth ()
word w = liftF $ FWord w ()
end :: Forth ()
end = liftF $ End


pretty :: Forth t -> String
pretty (Free (FInt n cont)) = "push " ++ show n ++ ";\n" ++ pretty cont
pretty (Free (FString s cont)) = "push " ++ s ++ ";\n" ++ pretty cont
pretty (Free (FWord s cont)) = "call " ++ s ++ ";\n" ++ pretty cont
pretty (Free End) = "end.\n"

--main = do
--     putStrLn $ show $ program

{-
todo:

- parse the following into the example program:
    3 2 add 100 add
- function definition
- is there a way to define built-ins without having to be concerned about the
  tail of the stack?

-}
