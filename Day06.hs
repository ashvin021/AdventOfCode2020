module Day06 where

import Criterion.Main
import Data.List.Split
import Data.Containers.ListUtils ( nubOrd )
import Data.List ( intersect )

------------------------------------------------------------

getInput :: IO [String]
getInput
  = do
     contents <- readFile "data/day06_input.txt"
     return $ lines contents

formInputPerGroup :: [String] -> [[String]]
formInputPerGroup = splitWhen (=="")

intersection :: Eq a => [[a]] -> [a]
intersection = foldr1 intersect

------------------------------------------------------------

q1 :: [String] -> Int
q1 = sum . map (length . nubOrd . concat) . formInputPerGroup

q2 :: [String] -> Int
q2 = sum . map (length . nubOrd . intersection) . formInputPerGroup

------------------------------------------------------------

main :: IO ()
main = do
        contents <- getInput
        print $ q1 contents
        print $ q2 contents
        defaultMain [
                bgroup "Day06" [ bench "Q1" $ whnf q1 contents
                                , bench "Q2" $ whnf q2 contents ]
                    ]
