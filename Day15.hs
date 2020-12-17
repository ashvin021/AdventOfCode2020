module Day15 where

import Data.MemoTrie
import Data.Bool ( bool )
import Data.Maybe ( fromJust, isNothing )
import Data.Function ( fix )
import Control.Monad ( foldM, forM_ )
import Control.Monad.ST ( runST )
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.IntMap.Lazy as IM


---------------------------------------------------------------------------

startNumbers = [12,1,16,3,11,0]

q1Table = [ q1 n | n <- [1..] ]

q1 n
  | n <= length startNumbers = startNumbers !! (n - 1)
  | prev `notElem` prevList  = 0
  | otherwise                = (n - 1) - lastN
  where
    prevList = take (n - 2) q1Table
    prev     = q1Table !! (n - 2)
    lastN    = snd . last . filter ((==prev) . fst) $ zip prevList [1..]


q2Table = scanl (\m x -> IM.insert (memoQ2 x) x m) IM.empty [1..]
q2Table' f n = if n == 0 then IM.empty else IM.insert (memoQ2 n) n (f (n - 1))
q2TableMemo = memoFix q2Table'

q2 :: (Int -> Int) -> Int -> Int
q2 f n
  | n <= length startNumbers = startNumbers !! (n - 1)
  | isNothing lastN          = 0
  | otherwise                = (n - 1) - fromJust lastN
  where
    prev     = f (n - 1)
    lastN    = q2TableMemo (n - 2) IM.!? prev

memoQ2 = memoFix q2







---------------------------------------------------------------------------
-- This version is heavily inspired by the solution at cdParks/advent2020
-- Probably going to retry this later without mutable vectors

q2' :: Int -> Int
q2' n = runST $ do

  memo <- V.new (max n (length startNumbers))
  forM_ startMap (uncurry $ V.write memo)
  
  foldM (\prev currentTurn -> do
    lastN <- V.unsafeRead memo prev
    V.unsafeWrite memo prev currentTurn
    pure $ if lastN == 0 then 0 else currentTurn - lastN
        ) (last startNumbers) turnsLeft

  where
    startMap  = zip startNumbers [1..]
    turnsLeft = [length startNumbers .. pred n]

