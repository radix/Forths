{-# LANGUAGE DeriveFunctor #-}

module Forths (
    Forth,
    Expr(FInt, FString, FWord, End),
    pushInt,
    word,
    end,
    pretty,
    eval
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


eval :: Forth t -> [Int] -> [Int]
eval (Free (FInt n cont)) xs = eval cont (n : xs)
eval (Free End) xs = xs
eval (Free (FWord "add" cont)) (x:x2:xs) = eval cont (x + x2 : xs)
eval (Free (FWord word cont)) xs = error ("unhandled word " ++ word ++ " with stack " ++ show xs)

{-
todo:

- parser
- word definition
- side effects
- homogenous stack... could I use a free monad for that too?
-}
