module Day15 where

import Data.MemoTrie
import Data.Maybe ( fromJust, isNothing )
import Data.Function ( fix )
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

q2 :: (Int -> Int) -> Int -> Int
q2 f n
  | n <= length startNumbers = startNumbers !! (n - 1)
  | isNothing lastN          = 0
  | otherwise                = (n - 1) - fromJust lastN
  where
--    prevList = take 1 . dropWhile ((/=prev) . snd) $ zip desc (map f desc)
    prev     = f (n - 1)
--    lastN    = fst . head $ prevList
    lastN    = (q2Table !! (n - 2)) IM.!? prev
--    desc     = [n - 2, n - 3 .. 1]


memoQ2 = memoFix q2
