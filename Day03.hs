module Day03 where

import Criterion.Main
import Data.Vector ( Vector, (!?), fromList )
import Data.Maybe ( fromJust, isNothing ) 

------------------------------------------------------------

data Coord = Tree
           | Square
           deriving (Eq, Show)

type Forest     = Vector (Vector Coord)
type ForestInfo = (Forest, Int) 

------------------------------------------------------------

getMapInfo :: IO [String]
getMapInfo
  = do
     contents <- readFile "data/day03_input.txt"
     return $ lines contents


moveBy :: Int -> Int -> (Int, Int) -> (Int, Int)
moveBy i j (o, o')
  = (o + i, o' + j)


parseMapInfo :: [String] -> ForestInfo
parseMapInfo rows
  = (vectorised, rowLength)
  where
    rowLength = (length . head) rows
    coordsRows = map (map (\x -> if x == '#' then Tree else Square)) rows
    vectorised = fromList $ map fromList coordsRows


findPath :: ForestInfo -> (Int, Int) -> (Int, Int) -> [Coord]
findPath f@(forest, rowLength) (o, o') toMoveBy
  | isNothing origin = []
  | otherwise        = fromJust origin : findPath f nextCoord toMoveBy
  where
    origin    = forest !? o' >>= (\x -> x !? (o `mod` rowLength)) 
    nextCoord = uncurry moveBy toMoveBy  (o, o')
  

------------------------------------------------------------

q1 p = length
     . filter (==Tree)
     . (\x -> findPath x (0, 0) p)

q2 c = product $ map (`q1` c) [(1,1), (3,1), (5,1), (7,1), (1,2)]

q1withParse p = q1 p . parseMapInfo

q2withParse   = q2 . parseMapInfo

------------------------------------------------------------

main :: IO ()
main
  = do
     content <- getMapInfo
     let forest = parseMapInfo content
     print $ q1 (3, 1) forest 
     print $ q2 forest
     defaultMain [
             bgroup "Day03" [ bench "Q1" $ whnf (q1 (3,1)) forest
                            , bench "Q1 w/parse"
                               $ whnf (q1withParse (3,1)) content
                            , bench "Q2" $ whnf q2 forest 
                            , bench "Q2 w/parse" 
                               $ whnf q2withParse content ]  
                 ]
     return ()
