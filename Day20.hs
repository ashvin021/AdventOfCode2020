module Day20 where

import Text.ParserCombinators.ReadP
import Data.Char ( isDigit )
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree ( Gr )
import Data.List ( transpose )
import Data.Maybe ( mapMaybe, fromJust )

---------------------------------------------------------------------------

data Transform = R0 | R1 | R2 | R3 | M1 | M2 | D1 | D2
     deriving (Eq, Show)

data Side = L | R | T | B
     deriving (Eq, Show)

data Puzzle = P [Tile] (Gr Tile Side) deriving Show

data Tile = Tile { tileId :: Int
                 , tileData :: [String]
                 , left :: String
                 , right :: String
                 , top :: String
                 , bottom :: String
                 , transformedWith :: [Transform]
                 } deriving Eq

instance Show Tile where
  show = show . tileData

---------------------------------------------------------------------------

parseInput :: ReadP a -> String -> a
parseInput p = fst . head . readP_to_S p

parseTile :: ReadP Tile
parseTile = do
        string "Tile"
        skipSpaces
        id <- read <$> many1 (satisfy isDigit)
        string ":\n"
        tData <- sepBy1 (many1 (char '.' +++ char '#')) (char '\n')
        string "\n\n"
        return Tile { tileId = id
                    , tileData = tData
                    , left = getSide L tData
                    , right = getSide R tData
                    , top = getSide T tData
                    , bottom = getSide B tData
                    , transformedWith = [] }


makePuzzle :: [Tile] -> Puzzle
makePuzzle (t : ts) 
  = makePuzzle' (P ts init)
  where
    init = insNode (tileId t, t) empty
    makePuzzle' (P [] gr) = P [] gr
    makePuzzle' (P ts gr) = makePuzzle' $ foldl insertTile (P [] gr) ts

insertTile :: Puzzle -> Tile -> Puzzle
insertTile (P ts board) t
  | null matches = P (t : ts) board 
  | otherwise    = P ts newBoard
  where
    edgeNodes = mapMaybe (lab board) . filter ((<4) . outdeg board) 
              $ nodes board
    tEdges    = zip [L, R, T, B] $ map ($t) [left, right, top, bottom]
    matches   = map sidesToTransform $ mapMaybe checkMatch edgeNodes
    newBoard  = insEdges (concatMap (\(x, y, s, s') -> [(tileId x, tileId y, s')
                                                       ,(tileId y, tileId x, s)]) matches)
              $ insNode (tileId t, (\(_, x, _, _) -> x) $ head matches) board


    checkMatch :: Tile -> Maybe (Side, Side, Bool, Tile, Tile)
    checkMatch t'
      | null cProd = Nothing
      | otherwise  = Just (head cProd)
      where
        t'Edges = zip [L, R, T, B] $ map ($t') [left, right, top, bottom]
        cProd   = [ (s , s', e == e', t', t) | (s , e ) <- tEdges
                                          , (s', e') <- t'Edges
                                          ,  e == e' || e == reverse e' ]

sidesToTransform :: (Side, Side, Bool, Tile, Tile) -> (Tile, Tile, Side, Side)
sidesToTransform (s, s', b, t', t)
  = (t', (transform t2 . transform t1) t, s, s') 
  where
    t1 = fromJust $ lookup (s, s') tMap
    t2 | not b            = R0
       | s == L || s == R = M2
       | otherwise        = M1


getSide :: Side -> [String] -> String
getSide T  = head 
getSide L  = map head 
getSide B  = last 
getSide R  = last . transpose 

transform :: Transform -> Tile -> Tile
transform R0 t = t
transform R1 t = (transform M1 . transform D1) t
transform R2 t = (transform M2 . transform M1) t
transform R3 t = (transform M2 . transform D1) t
transform M1 t = t { tileData = map reverse (tileData t)
                   , transformedWith = M1 : transformedWith t }
transform M2 t = t { tileData = reverse $ tileData t 
                   , transformedWith = M2 : transformedWith t }
transform D1 t = t { tileData = transpose $ tileData t 
                   , transformedWith = D1 : transformedWith t }
transform D2 t = (transform D1 . transform R2) t


q1 :: [Tile] -> Int
q1 ts
  = product $ map tileId cornerTiles
  where
    P _ board = makePuzzle ts
    cornerTiles = mapMaybe (lab board) . filter ((==2) . outdeg board)
                $ nodes board

main :: IO ()
main = do
        raw <- readFile "data/day20_input.txt"
        let tiles = parseInput (many1 parseTile <* eof) raw
        print $ q1 tiles



tMap = [((L, R), R0)
       ,((T, B), R0)
       ,((R, L), R0)
       ,((B, T), R0)
       ,((L, B), R1)
       ,((T, L), R1)
       ,((R, T), R1)
       ,((B, R), R1)
       ,((L, L), R2)
       ,((T, T), R2)
       ,((R, R), R2)
       ,((B, B), R2)
       ,((L, T), R3)
       ,((T, R), R3)
       ,((R, B), R3)
       ,((B, L), R3)]
