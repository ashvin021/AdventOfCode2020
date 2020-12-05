module Day05 where

import Criterion.Main

------------------------------------------------------------

type BoardingPass = String
type SeatPosition = (Int, Int)

getBoardingPassData :: IO [BoardingPass]
getBoardingPassData
  = do
     contents <- readFile "data/day05_input.txt"
     return $ lines contents

getPosition :: BoardingPass -> SeatPosition
getPosition p
  = (binToDec row, binToDec column)
  where
   toBinary      = map (\x -> x=='B'||x=='R') 
   (row, column) = splitAt 7 $ toBinary p
   binToDec      = foldl (\x y -> fromEnum y + 2*x) 0
   
getSeatID :: SeatPosition -> Int
getSeatID (row, column)
  = row * 8 + column

------------------------------------------------------------

q1 :: [BoardingPass] -> Int
q1 = maximum . map (getSeatID . getPosition) 

q2 :: [BoardingPass] -> Int
q2 ps = sum [min..max] - sum seatIDs
  where
    seatIDs    = map (getSeatID . getPosition) ps
    (max, min) = (maximum seatIDs, minimum seatIDs)

------------------------------------------------------------

main :: IO ()
main = do
        contents <- getBoardingPassData
        print $ q1 contents
        print $ q2 contents
        defaultMain [
                bgroup "Day 05" [ bench "Q1" $ nf q1 contents 
                                , bench "Q2" $ nf q2 contents ]
                    ]

