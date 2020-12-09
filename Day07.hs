module Day07 where

import Criterion.Main
import Data.List.Split 
import Text.Read ( readMaybe )
import Data.Maybe ( fromJust )
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.TransClos ( tc )
import Data.Graph.Inductive.NodeMap

------------------------------------------------------------

readInput :: IO [String]
readInput
  = do
     contents <- readFile "data/day07_input.txt"
     return $ lines contents


parseInputToGraph :: [String] -> (Gr String Int, NodeMap String)
parseInputToGraph xs
  = (mkGraph nodes edges, nodeMap)
  where
    (nodes, nodeMap) = (mkNodes new . map (unwords . take 2 . words)) xs
    edges            = concat 
                     $ zipWith makeEdges nodes (map getConnections xs)
    getConnections   = map (\ws -> ((unwords . drop 1) ws, head ws))
                     . filter (/=[]) 
                     . splitWhen (\x -> removeChars x == "bag") 
                     . drop 1 . dropWhile (/="contain") . words
    removeChars xs   = [ x | x <- xs, x `notElem` ".,s" ]

    makeEdges :: LNode String -> [(String, String)] -> [LEdge Int]
    makeEdges n cs
      = fromJust $ mkEdges nodeMap edgeData
      where
        edgeData = map (\(s, i) -> (snd n, s, i)) cs'
        cs'      = map fromJust
                 $ filter (/=Nothing)
                 $ map (\ps -> do i <- readMaybe (snd ps) :: Maybe Int
                                  return (fst ps, i)) cs


maxContained :: Gr String Int -> Node -> Int  
maxContained gr n
  = containedIn n - 1
  where
    containedIn :: Node -> Int
    containedIn n'
      = 1 + sum (map (\(_, node, label) -> label * containedIn node) 
          $ out gr n')    


------------------------------------------------------------

q1 :: (Gr String Int, NodeMap String) -> String -> Int
q1 (graph, nodeMap) target
  = length $ inn (tc graph) (fst $ mkNode_ nodeMap target)

q2 :: (Gr String Int, NodeMap String) -> String -> Int
q2 (graph, nodeMap) target
  = maxContained graph (fst $ mkNode_ nodeMap target)

------------------------------------------------------------

main :: IO ()
main = do
        content <- readInput
        let p = parseInputToGraph content
        print $ q1 p "shiny gold"
        print $ q2 p "shiny gold"
        defaultMain [
            bgroup "Day07" [ bench "Q1" $ whnf q1 p 
                           , bench "Q2" $ whnf q2 p 
                           , bench "Q1 with parse" 
                             $ nf (q1 . parseInputToGraph) content
                           , bench "Q2 with parse" 
                             $ nf (q2 . parseInputToGraph) content]
                    ]
