{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Day16 where

import Reusable ( intersection )
import Data.List.Split
import Data.Char
import Data.Function ( on )
import Data.Maybe ( catMaybes )
import Data.List ( transpose, (\\), isPrefixOf ) 
import Data.Bifunctor ( first )
import qualified Data.Map as M

---------------------------------------------------------------------------

type Constraint = (String, Int -> Bool)
type Ticket     = [Int]

data TicketData = TD { constraints :: [Constraint]
                     , myTicket :: Ticket
                     , otherTickets :: [Ticket] }

instance {-# OVERLAPPING #-} Eq Constraint where
  (==) = (==) `on` fst

instance {-# OVERLAPPING #-} Ord Constraint where
  compare = compare `on` fst

instance {-# OVERLAPPING #-} Show Constraint where
  show = show . fst

---------------------------------------------------------------------------

getInput :: IO String
getInput = readFile "data/day16_input.txt"

parseInput :: String -> TicketData
parseInput str
  = TD { constraints = constraints, myTicket = myT, otherTickets = otherTs }
  where
    (c : t : ts : _) = filter (/=[]) . splitWhen (=="") $ lines str
    constraints      = map parseConstraint c
    myT              = parseTicket (t !! 1)
    otherTs          = map parseTicket (tail ts)

parseTicket :: String -> Ticket
parseTicket = map read . splitWhen (==',')

parseConstraint :: String -> Constraint
parseConstraint xs
  = generateConstraint name nums nums'
  where
    getNums      = first ((\[x, y] -> (x, y)) . map read . splitWhen (=='-'))
                 . break isSpace . dropWhile (not . isDigit)
    name         = takeWhile (/=':') xs
    (nums , xs') = getNums xs
    (nums', _  ) = getNums xs'

generateConstraint :: String -> (Int, Int) -> (Int, Int) -> Constraint
generateConstraint name (a, b) (c, d)
  = (name, f)
  where
    f = \n -> (n >= a && n <= b) || (n >= c && n <= d)

---------------------------------------------------------------------------

invalidFields :: TicketData -> [Int]
invalidFields t
  = concatMap (filter isFieldInvalid) $ otherTickets t
  where
    isFieldInvalid = not . isFieldValid (constraints t)

validTickets :: TicketData -> [Ticket]
validTickets t 
  = filter (all $ isFieldValid (constraints t)) $ otherTickets t
    
isFieldValid :: [Constraint] -> Int -> Bool
isFieldValid cs = or . mapM snd cs

isFieldValid' :: [Constraint] -> Int -> [Maybe Constraint]
isFieldValid' cs n = map (`isValidFor` n) cs 

isValidFor :: Constraint -> Int -> Maybe Constraint
isValidFor c@(_, f) n
  | f n       = Just c
  | otherwise = Nothing

matchFields :: TicketData -> M.Map Constraint Int
matchFields t
  = matchField M.empty (constraints t)
  where
    fs = transpose (validTickets t)
    matchField :: M.Map Constraint Int -> [Constraint] 
               -> M.Map Constraint Int
    matchField m [] = m
    matchField m cs 
      = matchField newMap newCs
      where
        validCsPerField :: [[Maybe Constraint]]
        validCsPerField = map (intersection . map (isFieldValid' cs)) fs

        ((i, c) : _)    = filter ((==1) . length . snd)
                        $ zip [0..] (map catMaybes validCsPerField)

        newMap          = M.insert (head c) i m
        newCs           = cs \\ c

myDepartureValues :: TicketData -> M.Map Constraint Int -> [Int]
myDepartureValues t m
  = map (myT !!) $ M.elems departureFields
  where
    myT = myTicket t
    departureFields = M.filterWithKey (\k _ -> "departure" `isPrefixOf` fst k) m
    
---------------------------------------------------------------------------

q1 = sum
   . invalidFields
   . parseInput

q2 = product
   . (\t -> myDepartureValues t (matchFields t))
   . parseInput

---------------------------------------------------------------------------

main :: IO ()
main = do
        input <- getInput
        print $ q1 input
        print $ q2 input
