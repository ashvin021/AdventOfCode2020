{-# LANGUAGE DeriveFunctor, DeriveFoldable, InstanceSigs, FlexibleInstances #-}

module Day11 where

import CellularAutomata
import Control.Comonad
import Control.Monad ( join )

------------------------------------------------------------

------------------------------------------------------------

getInput :: IO [String]
getInput
  = do
      contents <- readFile "data/day11_input.txt"
      return $ lines contents

emptyTape :: Tape Char
emptyTape = Tape (repeat '.') '.' (repeat '.')

getSquareFromString :: [String] -> Square Char
getSquareFromString = toSquare '.'

------------------------------------------------------------

q1rule :: Square Char -> Char
q1rule (Square (Tape (x : _) y (z : _)))
  | e == 'L' && null ns        = '#'
  | e == '#' && length ns >= 4 = 'L'
  | otherwise                  =  e
  where
    Tape (a : _) b  (c : _) = x
    Tape (d : _) e  (f : _) = y
    Tape (g : _) h  (i : _) = z
    ns = filter (=='#') [a,b,c,d,f,g,h,i]

q2rule :: Square Char -> Char
q2rule s@(Square (Tape left (Tape us e ds) right))
  | e == 'L' && null ns        = '#'
  | e == '#' && length ns >= 5 = 'L'
  | otherwise                  =  e
  where
    directionStream f s = map extract $ iterateTail f s
    ns    = filter (=='#') $ (join . join) [lr, ud, diags]
    lr    = map (take 1 . dropWhile (=='.') . take 16 . map point) [left, right]
    ud    = map (take 1 . dropWhile (=='.') . take 16) [us, ds]
    diags = map (take 1 . dropWhile (=='.') . take 16 . (`directionStream` s))
                          [ upSquare . lSquare, upSquare . rSquare
                          , dnSquare . lSquare, dnSquare . rSquare ]

------------------------------------------------------------

q1 = length
   . filter (=='#')
   . concat 
   . toListFromSquare

q f = head
    . map snd
    . dropWhile (uncurry (/=))
    . (\x -> zip x (tail x))
    . map (toFiniteSquare 98 95)
    . build f
    . getSquareFromString

------------------------------------------------------------

main :: IO ()
main = do
        contents <- getInput
        print $ length contents
        print $ length $ head contents
        let grid = q q1rule contents
        print grid
        print $ q1 grid
        let grid' = q q2rule contents
        print grid'
        print $ q1 grid'
        return ()
        


