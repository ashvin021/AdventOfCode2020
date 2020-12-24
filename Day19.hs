module Day19 where

import Data.List ( isPrefixOf, (\\))
import Data.List.Split ( splitOn, splitWhen )
import Data.Char ( isDigit )
import qualified Data.IntMap as IM

---------------------------------------------------------------------------

data Rule = Rule  String
          | Index Int
          | And   Rule Rule
          | Or    Rule Rule
          deriving (Show)

type RuleMap = IM.IntMap Rule
type RuleIndex = Int

---------------------------------------------------------------------------

getInput :: IO String
getInput = readFile "data/day19_input.txt"


parseInput :: String -> (RuleMap, [String])
parseInput str
  = (ruleMap, xs)
  where
    (rStrings : xs : _ ) = splitWhen (=="") $ lines str  
    ruleMap              = parseRules rStrings


parseRules :: [String] -> RuleMap
parseRules 
  = IM.fromList . map (\x -> (index x, parseBody (body x)))
  where
    toInt = read :: String -> Int
    index = toInt . takeWhile (/=':')
    body  = drop 2 . dropWhile (/=':')


    parseBody :: String -> Rule
    parseBody xs@(x : _)
      | '|' `elem` xs  = Or (parseBody o) (parseBody o')
      | ' ' `elem` xs  = And (parseBody a) (parseBody a')
      | isDigit x      = Index $ toInt xs
      | otherwise      = Rule $ filter (/='"') xs
      where
        (o : o' : _) = splitOn " | " xs
        (a : a' : _) = splitOn " " xs
    

matchesQ1 :: RuleMap -> Int -> String -> Bool
matchesQ1 rm i x
  = (True, []) == matches' rm (Index i) x


matchesQ2 :: RuleMap -> Int -> String -> Bool
matchesQ2 rm 0 x
  = matches0 rm x
matchesQ2 rm i x
  = (True, []) == matches' rm (Index i) x


matches' :: RuleMap -> Rule -> String -> (Bool, String)
matches' _ (Rule xs) ys
  = (xs `isPrefixOf` ys, ys \\ xs)
matches' rm (Index i) xs
  = matches' rm (rm IM.! i) xs
matches' rm (And r r') ys
  = (b && b', ys'')
  where
    (b , ys' ) = matches' rm r ys
    (b', ys'') = matches' rm r' ys'
matches' rm (Or r r') ys
  | fst a     = a
  | fst a'    = a'
  | otherwise = a
  where
    (a, a') = (f r, f r')
    f = \x -> matches' rm x ys


-- Quite an ugly solution, but it works 
matches0 :: RuleMap -> String -> Bool
matches0 rm xs
  = elem (True, []) $ map (\x -> matches' rm x xs) rules
  where
    rule8matches = takeWhile (\x -> fst $ matches' rm x xs)
                 $ iterate (\x -> And x (Index 42)) (Index 42)
    rule11'      = takeWhile (\x -> (/=[]) . snd $ matches' rm x xs)
                 $ iterate (\x -> And (And (Index 42) x) (Index 31)) 
                   (And (Index 42) (Index 31))
    rules        = [ And x y | x <- rule8matches, y <- rule11' ]


---------------------------------------------------------------------------

q1 c
  = length $ filter (matchesQ1 im 0) xs
  where
    (im, xs) = parseInput c

q2 c
  = length $ filter (matchesQ2 im 0) xs
  where
    (im, xs) = parseInput c

---------------------------------------------------------------------------

main :: IO ()
main = do
        content <- getInput
        print $ q1 content
        print $ q2 content
