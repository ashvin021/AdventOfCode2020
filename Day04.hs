module Day04 where

import Criterion.Main
import Data.List.Split
import Data.Map.Strict (Map, fromList, member, (!))
import Data.Bifunctor ( second )
import Text.Regex.PCRE.Light
import qualified Data.ByteString.UTF8  as B

------------------------------------------------------------

byrRegex = compile (B.fromString "^(192\\d|19[3-9]\\d|200[0-2])$") []
iyrRegex = compile (B.fromString "^(201\\d|2020)$") []
eyrRegex = compile (B.fromString "^(202\\d|2030)$") []
hgtRegex = compile (B.fromString "^((1[5-8]\\d|19[0-3])cm|(59|6\\d|7[0-6])in)$") []
hclRegex = compile (B.fromString "^#([0-9]|[a-f]){6}$") []
eclRegex = compile (B.fromString "^(amb|blu|brn|gry|grn|hzl|oth)$") []
pidRegex = compile (B.fromString "^\\d{9}$") []

------------------------------------------------------------

getPassportData :: IO [String]
getPassportData
  = do
     contents <- readFile "data/day04_input.txt"
     return $ lines contents

parsePassports :: [String] -> [Map String String]
parsePassports input
  = map toMap $ splitWhen (=="") input 
  where
    toMap l = fromList
            . map (second (drop 1) . break (==':'))
            $ concatMap words l


isValidField :: String -> Regex -> Bool
isValidField str r
  = (/=Nothing) $ match r (B.fromString str) []

isValidPassport' :: Map String String -> Bool
isValidPassport' p
  = all (\(k ,v) -> isValidField (p ! k) v) fieldsToCheck

------------------------------------------------------------

validPassportsQ1 c
  = filter (\p -> all ((`member` p) . fst) fieldsToCheck)
  $ parsePassports c

q1 = length . validPassportsQ1

q2 = length . filter isValidPassport' . validPassportsQ1

fieldsToCheck = [ ("byr", byrRegex)
                , ("iyr", iyrRegex)
                , ("eyr", eyrRegex)
                , ("hgt", hgtRegex)
                , ("hcl", hclRegex)
                , ("ecl", eclRegex)
                , ("pid", pidRegex) ]


------------------------------------------------------------

main :: IO ()
main
  = do
     contents <- getPassportData
     print $ q1 contents 
     print $ q2 contents
     defaultMain [
             bgroup "Day 4" [ bench "Q1" $ whnf q1 contents
                            , bench "Q2" $ whnf q2 contents ]
                 ]
     return ()
