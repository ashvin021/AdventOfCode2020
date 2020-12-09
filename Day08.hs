module Day08 where

-- import Criterion.Main
import           Data.Maybe ( fromJust )
import           Text.Read ( read )
import qualified Data.IntMap as IM
import qualified Data.Map    as M

------------------------------------------------------------

data Command = Acc Int
             | Jmp Int
             | Nop Int
             deriving (Show, Eq)

type Instructions = IM.IntMap Command
type Accumulator = Int

------------------------------------------------------------

getInput :: IO [String]
getInput
  = do
     content <- readFile "data/day08_input.txt"
     return $ lines content
     
parseCommands :: [String] -> Instructions
parseCommands xs
  = IM.fromList instructions 
  where
    instructions = zip [0..] $ map parseCommand xs

    parseCommand :: String -> Command
    parseCommand xs'
      | ins == "acc" = Acc i
      | ins == "jmp" = Jmp i
      | otherwise    = Nop i
      where
        ins : arg : _  = words xs'
        i              = read (filter (/='+') arg) :: Int  


eval :: Instructions -> Maybe (Accumulator, Int) 
eval inst 
  = eval' [] 0 0
  where
    limit = length inst

    eval' :: [Int] -> Int -> Accumulator -> Maybe (Accumulator, Int)
    eval' alreadyRun next acc
      | next == limit           = Just (acc, 0)
      | next `elem` alreadyRun  = Just (acc, next)
      | Nothing      <- command = Nothing
      | Just (Acc x) <- command = eval' alreadyRun' (next + 1) (acc + x)
      | Just (Jmp x) <- command = eval' alreadyRun' (next + x) acc
      | Just (Nop _) <- command = eval' alreadyRun' (next + 1) acc
      where
        command     = inst IM.!? next
        alreadyRun' = next : alreadyRun

      
    
q1 :: Instructions -> Accumulator
q1 = fst . fromJust .  eval


q2 :: Instructions -> Accumulator
q2 inst
  = q2' 0 
  where
    q2' :: Int -> Accumulator
    q2' i
      | newInst == inst    = q2' (i + 1)
      | Just(x, 0) <-  acc = x
      | otherwise          = q2' (i + 1) 
      where
        acc        = eval newInst
        newInst    = IM.adjust toggle i inst
        toggle cmd = case cmd of (Jmp x) -> Nop x
                                 (Nop x) -> Jmp x
                                 _       -> cmd

------------------------------------------------------------

main :: IO ()
main = do
        contents <- getInput
        print $ (q1 . parseCommands) contents
        print $ (q2 . parseCommands) contents


