module Day2 where

import           Text.Regex.PCRE.Light
import           Data.Maybe ( fromJust )
import           Data.Bits  ( xor )
import           Criterion.Main
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8  as BSU
import qualified Data.Sequence         as Seq

data PasswordInfo = PasswordInfo { password :: B.ByteString
                                 , constrainedChar :: Char
                                 , constraints :: (Int, Int)
                                 } deriving (Show)

------------------------------------------------------------

parserR 
  = compile (BSU.fromString "(\\d{1,3})\\D(\\d{1,3})\\s(\\w):\\s(\\w*)$") []

------------------------------------------------------------

getPasswordInfo :: IO [B.ByteString]
getPasswordInfo 
  = do
     contents <- B.readFile "data/day2_input.txt"
     return $ C.split '\n' contents


parsePasswordInfo :: B.ByteString -> PasswordInfo
parsePasswordInfo p
  = PasswordInfo { password        = pass
                 , constrainedChar = head $ C.unpack c
                 , constraints     = (read (BSU.toString i) :: Int
                                     ,read (BSU.toString j) :: Int)
                 }
  where
    (_ : i : j : c : pass : _) = fromJust $ match parserR p []


isValidPassword :: PasswordInfo -> Bool
isValidPassword p 
  = lower <= i && i <= upper
 where
    (lower, upper) = constraints p
    i              = (length . filter (==constrainedChar p)
                             . BSU.toString . password) p 


isValidPassword' :: PasswordInfo -> Bool
isValidPassword' p
  = xor a b
  where
    seqPass = (Seq.fromList . BSU.toString . password) p
    (i, j)  = constraints p
    c       = constrainedChar p
    a       = Just c == (seqPass Seq.!? (i - 1))
    b       = Just c == (seqPass Seq.!? (j - 1))


------------------------------------------------------------

q1 = length
   . filter isValidPassword
   . map parsePasswordInfo

q2 = length
   . filter isValidPassword'
   . map parsePasswordInfo
------------------------------------------------------------

main :: IO ()
main = do
        bytestrings <-  getPasswordInfo
        print $ q1 bytestrings
        print $ q2 bytestrings
        defaultMain [
                bgroup "Day2" [ bench "Q1" $ nf q1 bytestrings
                              , bench "Q2" $ nf q2 bytestrings
                              ]
                    ]
        return ()
