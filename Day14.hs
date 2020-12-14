module Day14 where

import Data.Bits
import Data.Char ( isAlphaNum )
import Data.Bifunctor ( second, bimap )
import Data.List.Split ( split, startsWith )
import qualified Data.Map as M

------------------------------------------------------------

type Mask = String
type Address = Int

------------------------------------------------------------

getInput :: IO String
getInput = readFile "data/day14_input.txt"
   
parseInput :: String -> [(Mask, [(Address, Int)])]
parseInput xs
  = map parseSection sections
  where
    sections = split (startsWith "mask") xs
    
    parseSection :: String -> (Mask, [(Address, Int)])
    parseSection xs'
      = (mask, zipWith (curry (bimap read read)) addresses values)
      where
        (mask : values) = map (dropWhile (not . isAlphaNum) . dropWhile (/='=')) 
                        $ lines xs'
        addresses       = map (takeWhile (/=']') . tail . dropWhile (/='[')) 
                        . drop 1 $ lines xs'

applyMask :: Mask -> Int -> Int
applyMask mask 
  = (.&. clearMask) . (.|. setMask)
  where
    toInt :: String -> Int
    toInt = foldl (\x y -> 2 * x + read [y]) 0

    setMask   = toInt $ map (\x -> if x == '1' then '1' else '0') mask
    clearMask = toInt $ map (\x -> if x == '0' then '0' else '1') mask

applyMask' :: Mask -> Address -> [Address]
applyMask' mask address
  = map (.|. prelimMask) xBits
  where
    toInt :: String -> Int
    toInt = foldl (\x y -> 2 * x + read [y]) 0
    
    prelimMask = (.&. clearMask) . (.|. setMask) $ address
    setMask    = toInt $ map (\x -> if x == '1' then '1' else '0') mask
    clearMask  = toInt $ map (\x -> if x == 'X' then '0' else '1') mask
    xIndices   = [ x | x <- [0..length mask - 1], reverse mask !! x == 'X']
    xBits      = getXBits xIndices

getXBits :: [Int] -> [Int]
getXBits ps
  = map (foldr (.|.) 0 . zipWith toBits (reverse ps) . addLeading0s . toBin) 
    numOfCombinations
  where
    numOfCombinations = [0..2 ^ length ps - 1]
    toBits x y = if y == 1 then bit x else 0
    toBin 0 = [0]
    toBin 1 = [1]
    toBin n = let (q, r) = n `divMod` 2 in toBin q ++ [r]
    addLeading0s bins
      | length bins == length ps = bins
      | otherwise                = addLeading0s (0 : bins)

applyMaskToVals :: (Mask, [(Address, Int)]) -> [(Address, Int)]
applyMaskToVals (mask, xs)
  = map (second (applyMask mask)) xs

decodeAddresses :: (Mask, [(Address, Int)]) -> [(Address, Int)]
decodeAddresses (mask, xs)
  = concatMap (\(k, v) -> applyMask' mask k `zip` repeat v) xs

addToMap :: M.Map Address Int -> (Address, Int) -> M.Map Address Int
addToMap = flip $ uncurry M.insert


------------------------------------------------------------

q1 = sum
   . foldl addToMap M.empty
   . concatMap applyMaskToVals
   . parseInput

q2 = sum
   . foldl addToMap M.empty
   . concatMap decodeAddresses
   . parseInput

------------------------------------------------------------

main :: IO ()
main = do
        content <- getInput
        print $ q1 content
        print $ q2 content
