{-# LANGUAGE DeriveFunctor #-}

module Forths (
    Forth,
    Expr(FInt, FString, FWord, End),
    pushInt,
    pushString,
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
pushString :: String -> Forth ()
pushString s = liftF $ FString s ()
word :: String -> Forth ()
word w = liftF $ FWord w ()
end :: Forth ()
end = liftF $ End


pretty :: Forth t -> String
pretty (Free (FInt n cont)) = "push " ++ show n ++ ";\n" ++ pretty cont
pretty (Free (FString s cont)) = "push " ++ s ++ ";\n" ++ pretty cont
pretty (Free (FWord s cont)) = "call " ++ s ++ ";\n" ++ pretty cont
pretty (Free End) = "end.\n"



-- INTERPRETER


-- Data types allowed on the stack
data FData
  = RInt Int
  | RString String
  deriving (Show)

eval :: Forth t -> [FData] -> [FData]
eval (Free (FInt n cont)) xs = eval cont (RInt n : xs)
eval (Free (FString s cont)) xs = eval cont (RString s : xs)
eval (Free (FWord "add" cont)) ((RInt x):(RInt x2):xs) = eval cont (RInt (x + x2) : xs)
eval (Free (FWord "index" cont)) ((RInt index):(RString string):xs) = eval cont (RString [string !! index] : xs)
eval (Free (FWord word cont)) xs = error ("unhandled word " ++ word ++ " with stack " ++ show xs)
eval (Free End) xs = xs

{-
todo:
- parser
- word definition / dictionary
- side effects
-}
