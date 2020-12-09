module Day09 where

-- import Criterion.Main
import Data.IntSet (IntSet, insert, member, empty)
import Data.List.Split (divvy)
import qualified Data.Sequence as S

------------------------------------------------------------

------------------------------------------------------------

getInput :: IO [String]
getInput
  = do
     contents <- readFile "data/day09_input.txt"
     return $ lines contents


parseInput :: [String] -> [Int]
parseInput 
  = map (read :: String -> Int)

canSum2ToK :: [Int] -> Int -> Bool
canSum2ToK ns k
  = canSum2ToK' ns (empty :: IntSet)
  where
    canSum2ToK' :: [Int] -> IntSet -> Bool
    canSum2ToK' [] _ = False
    canSum2ToK' (x : xs) set
      | (k - x) `member` set = True
      | otherwise            = canSum2ToK' xs (insert x set)

adjacentSublists :: S.Seq Int -> S.Seq (S.Seq Int)
adjacentSublists
  = S.filter (not . null) . foldl1 (S.><) . fmap S.inits . S.tails

------------------------------------------------------------

q1 :: [String] -> Int
q1 = head . head 
   . dropWhile (\(x : xs) -> canSum2ToK xs x)
   . divvy 26 1 . reverse
   . parseInput

q2 :: [String] -> Int
q2 c = (\xs -> maximum xs + minimum xs)
     . foldl1 (S.><)
     . S.filter (\xs -> sum xs == k)
     . adjacentSublists
     . S.takeWhileL (<=k)
     . S.fromList
     $ parseInput c 
  where
    k = q1 c

------------------------------------------------------------

main :: IO ()
main = do
        contents <- getInput
        print $ q1 contents
        print $ q2 contents
