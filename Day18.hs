module Day18 where

import Data.Char
import Data.Maybe ( fromJust )

---------------------------------------------------------------------------

type Operator = Char

data Token = TNum Int | TOp Operator
             deriving (Eq, Show)

data Expr = ENum Int | EApp Operator Expr Expr
            deriving (Eq, Show)

type ExprStack = [Expr]
type OpStack = [Operator]
type Precedence = Int

ops = "+*()"
opTableQ1
  = [ ('+', 6)
    , ('*', 6)
    , ('(', 1)
    , (')', 1)
    , ('$', 0) ]
opTableQ2
  = [ ('+', 7)
    , ('*', 6)
    , ('(', 1)
    , (')', 1)
    , ('$', 0) ]
opFunctions
 = [ ( '+', (+) )
   , ( '*', (*) ) ]

---------------------------------------------------------------------------

getInput :: IO [String]
getInput
  = do
      content <- readFile "data/day18_input.txt"
      return $ lines content

precedenceQ1 :: Operator -> Precedence
precedenceQ1
  = fromJust . flip lookup opTableQ1


precedenceQ2 :: Operator -> Precedence
precedenceQ2
  = fromJust . flip lookup opTableQ2

supersedesBy :: (Operator -> Precedence) -> Operator -> Operator -> Bool
supersedesBy f a b
  = f a > f b

tokenise :: String -> [Token]
tokenise [] = []
tokenise (x : xs)
  | isSpace x = tokenise xs
  | x `elem` ops = TOp x : tokenise xs
  | otherwise    = TNum (read (x : restOfToken)) : tokenise xs'
  where
    (restOfToken, xs') = span isDigit xs

parseExp :: (Operator -> Precedence) -> String -> Expr
parseExp f x
  = y 
  where
   (y, _, _) = parse f (tokenise x) [] ['$']

parse :: (Operator -> Precedence) 
      -> [Token] -> ExprStack -> OpStack 
      -> (Expr, ExprStack, OpStack)
parse _ [] [e] os = (e, [], os)
parse f [] (e : es) (o : os)
  | o == '('  = (e, es, os)
  | otherwise = parse f [] (EApp o e' e : es') os
  where
    (e' : es') = es
parse f (TNum i : ts) es os
  = parse f ts (ENum i : es) os 
parse f ts@(TOp x : ts') es os@(o : os')
  | supersedesBy f x o || x == '(' = parse f ts' es (x : os)
  | x == ')'                       = parse f ts' (insideBrackets : esLeft) osLeft 
  | otherwise                      = parse f ts (EApp o e' e : es') os'
  where
    (e : e' : es') = es 
    (insideBrackets, esLeft, osLeft) = parse f [] es os

eval :: Expr -> Int
eval (ENum x) = x
eval (EApp o e e') = (fromJust $ lookup o opFunctions) (eval e) (eval e')

---------------------------------------------------------------------------

q1 = sum
   . map (eval . parseExp precedenceQ1)

q2 = sum
   . map (eval . parseExp precedenceQ2)

---------------------------------------------------------------------------

main :: IO ()
main = do
        content <- getInput
        print $ q1 content
        print $ q2 content

