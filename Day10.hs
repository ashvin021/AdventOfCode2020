module Day10 where

import Reusable
import Data.Map ( fromListWith, Map, (!) )
import Data.List ( sort )
import Data.List.HT ( groupBy )

------------------------------------------------------------

getInput :: IO [String]
getInput
  = do
     contents <- readFile "data/day10_input.txt"
     return $ lines contents

parseInput :: [String] -> [Int]
parseInput xs
  = 0 : maximum ns + 3 : ns
  where
    ns = map (read :: String -> Int) xs

frequency :: Ord a => [a] -> Map a Int
frequency xs
  = fromListWith (+) [ (x, 1) | x <- xs ]

------------------------------------------------------------

q1 :: [Int] -> Int
q1 xs 
  = (freqMap ! 1) * (freqMap ! 3)
  where
    freqMap = frequency $ zipWith (-) (tail ns) ns
    ns      = sort xs

fibs3 :: [Integer]
fibs3 = 1 : 1 : 2 : zipWith3 ((+) .: (+)) fibs3 (drop 1 fibs3) (drop 2 fibs3)

q2 :: [Int] -> Integer
q2 = product 
   . map (\x -> fibs3 !! (length x - 1)) 
   . groupBy (\x y -> y - x == 1)
   . sort 

------------------------------------------------------------

main :: IO ()
main = do
        contents <- getInput
        print $ q1 $ parseInput contents
        print $ q2 $ parseInput contents
