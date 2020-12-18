module Day17 where

import CellularAutomata
import Control.Comonad
import Data.List ( delete )

---------------------------------------------------------------------------

getInput :: IO String
getInput = readFile "data/day17_input.txt"

getCubeFromStrings :: [[String]] -> Cube Char
getCubeFromStrings = toCube '.'

getHCubeFromStrings :: [[[String]]] -> HyperCube Char
getHCubeFromStrings = toHCube '.'

numberActiveCube :: FiniteCube Char -> Int
numberActiveCube = length . filter (=='#') 
                 . concat . concat . toListFromCube 

numberActiveHCube :: FiniteHyperCube Char -> Int
numberActiveHCube = length . filter (=='#') 
                  . concat . concat . concat . toListFromHCube 

conway3D :: Cube Char -> Char
conway3D c
  | e == '#' && l /= 2 && l /= 3 = '.'
  | e == '.' && l == 3           = '#'
  | otherwise                    = e
  where
    e  = extract c
    ns = filter (=='#') . delete e $ neighboursCube c
    l  = length ns


conway4D :: HyperCube Char -> Char
conway4D h
  | e == '#' && l /= 2 && l /= 3 = '.'
  | e == '.' && l == 3           = '#'
  | otherwise                    = e
  where
    e  = extract h
    ns = filter (=='#') . delete e $ neighboursHCube h
    l  = length ns

q1 = (!! 6)
   . map (toFiniteCube 20 20 20)
   . build conway3D
   . getCubeFromStrings
   . (:[])
   . lines

q2 = toFiniteHCube 20 20 20 20
   . (!! 6)
   . build conway4D
   . getHCubeFromStrings
   . (:[]) . (:[])
   . lines

---------------------------------------------------------------------------

main :: IO ()
main = do
        content <- getInput
--        print $ q1 content
        let hcube = q2 content
        print hcube
        print $ numberActiveHCube hcube
