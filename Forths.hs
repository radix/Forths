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

-- Questions:

-- 1. I have been told that Expr is a Functor.  If a Functor is a
--    mapping between categories, what categories does Expr map between?
-- 2. I'm not actually sure why End is useful.
-- 3. This type allows very invalid things to be represented, such as
--    FInt 3 "hoobledy". Is there a way I can only allow further Exprs?

data Expr cont
  = FInt Int cont
  | FString String cont
  | FWord String cont
  | End
  deriving (Show, Functor)

type Forth = Free Expr

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


-- INTERPRETER (probably should be a separate module)


-- Data types allowed on the stack
data FData
  = RInt Int
  | RString String
  | RBool Bool
  deriving (Show, Eq)


-- todo: custom words will need errors
eval :: (Show t) => Forth t -> [FData] -> [FData]
eval (Free (FInt n cont)) xs = eval cont (RInt n : xs)
eval (Free (FString s cont)) xs = eval cont (RString s : xs)
eval (Free (FWord "eq?" cont)) (x:y:xs) = eval cont (RBool (x == y) : xs)
eval (Free (FWord "add" cont)) ((RInt x):(RInt y):xs) = eval cont (RInt (x + y) : xs)
eval (Free (FWord "index" cont)) ((RInt index):(RString string):xs) = eval cont (RString [string !! index] : xs)
eval (Free End) xs = xs
eval prog xs = error ("unhandled program " ++ show prog ++ " with stack " ++ show xs)

{-
todo:
- word definition / dictionary
- conditionals
- parser
- side effects
-}
