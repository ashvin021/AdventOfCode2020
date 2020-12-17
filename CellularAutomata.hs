{-# LANGUAGE DeriveFunctor, DeriveFoldable, InstanceSigs, FlexibleInstances #-}

module CellularAutomata where

import Control.Comonad
import Data.Function.HT ( nest )
--  import Data.List (transpose)

------------------------------------------------------------

data Tape a = Tape [a] a [a]
              deriving (Functor, Foldable, Eq)

newtype Square a = Square { unSquare :: Tape (Tape a) }
                     deriving (Functor, Eq)

newtype Cube a = Cube { unCube :: Tape (Square a) }
                     deriving (Functor, Eq, Show)

type FiniteTape a = Tape a
type FiniteSquare a = Square a
type FiniteCube a = Cube a

------------------------------------------------------------

instance Comonad Tape where
  extract = point
  duplicate = genericMove lTape rTape  

instance Show a => Show (Tape a) where
  show (Tape xs y zs) = show $ reverse (take 30 xs) ++ y : take 30 zs

instance Comonad Square where
  extract (Square t) = (point . point) t
  duplicate = Square . fmap xSquare . ySquare

instance Show a => Show (Square a) where
  show (Square (Tape uss vs wss)) 
    = pretty 20 (reverse (take 20 uss)) ++ 
      show vs ++
      '\n' : pretty 20 (take 20 wss)

instance Comonad Cube where
  extract (Cube t) = ((point . point) . unSquare . point) t
  duplicate = Cube . fmap ((Square . fmap xCube) . yCube) . zCube

---------------------------------------------------------------------------

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (t a -> t a)
            -> (t a -> t a)
            -> t a
            -> Tape (t a)
genericMove f g e = Tape (iterateTail f e) e (iterateTail g e) 

point :: Tape a -> a
point (Tape _ x _) = x

prevs :: Tape a -> [a]
prevs (Tape us _ _) = us

nexts :: Tape a -> [a]
nexts (Tape _ _ vs) = vs

lTape :: Tape a -> Tape a
lTape (Tape (x : xs) y zs) = Tape xs x (y : zs)

rTape :: Tape a -> Tape a
rTape (Tape xs y (z : zs)) = Tape (y : xs) z zs

upSquare, dnSquare :: Square a -> Square a
upSquare (Square a) = Square (lTape a)
dnSquare (Square a) = Square (rTape a)

lSquare, rSquare :: Square a -> Square a
lSquare (Square a) = Square (fmap lTape a)
rSquare (Square a) = Square (fmap rTape a)

xSquare, ySquare :: Square a -> Tape (Square a) 
xSquare = genericMove lSquare rSquare
ySquare = genericMove upSquare dnSquare

lCube, rCube :: Cube a -> Cube a
lCube (Cube a) = Cube (fmap lSquare a)
rCube (Cube a) = Cube (fmap rSquare a)

upCube, dnCube :: Cube a -> Cube a
upCube (Cube a) = Cube (fmap upSquare a)
dnCube (Cube a) = Cube (fmap dnSquare a)

inCube, outCube :: Cube a -> Cube a
inCube  (Cube a) = Cube (lTape a)
outCube (Cube a) = Cube (rTape a)

xCube, yCube, zCube :: Cube a -> Tape (Cube a)
xCube = genericMove lCube rCube
yCube = genericMove upCube dnCube
zCube = genericMove inCube outCube

pretty :: Show a => Int -> [Tape a] -> String
pretty _ [] = ""
pretty 0 _  = ""
pretty n (t : ts)
  = show t ++ '\n' : pretty (n - 1) ts 

---------------------------------------------------------------------------

build :: Comonad w => (w a -> a) -> w a -> [] (w a)
build f = iterate (f <<=) 

---------------------------------------------------------------------------

emptyTape :: a -> Tape a
emptyTape x = Tape (repeat x) x (repeat x)

toFiniteTape :: Int -> Tape a -> FiniteTape a
toFiniteTape size (Tape xs y zs)
  = Tape (take eachSide xs) y (take eachSide zs)
  where
    eachSide
      | size `mod` 2 == 1 = size `div` 2
      | otherwise         = (size `div` 2) + 1

toFiniteSquare :: Int -> Int -> Square a -> FiniteSquare a
toFiniteSquare x y (Square t)
  = Square (toFiniteTape x <$> toFiniteTape y t)

toFiniteCube :: Int -> Int -> Int -> Cube a 
             -> FiniteCube a
toFiniteCube x y z (Cube t)
  = Cube (toFiniteSquare x y <$> toFiniteTape z t)

---------------------------------------------------------------------------

toListFromTape :: FiniteTape a -> [a]
toListFromTape (Tape xs y zs)
  = xs ++ y : zs

toListFromSquare :: FiniteSquare a -> [[a]]
toListFromSquare (Square t)
  = toListFromTape <$> toListFromTape t

toListFromCube :: FiniteCube a -> [[[a]]]
toListFromCube (Cube t)
  = toListFromSquare <$> toListFromTape t

---------------------------------------------------------------------------

toTape :: a -> [a] -> Tape a
toTape init list@(x : xs)
  = nest (length list `div` 2) rTape $ Tape (repeat init) x (xs ++ repeat init)

toSquare :: a -> [[a]] -> Square a
toSquare init 
  = Square . toTape (emptyTape init) . fmap (toTape init) 

toCube :: a -> [[[a]]] -> Cube a
toCube init list
  = Cube $ toSquare init <$> toTape (repeat $ repeat init) list

-- class Transposable f where
--  transpose :: f (f a) -> f (f a)  

-- instance Transposable [] where
--   transpose = Data.List.transpose

-- instance Transposable Tape where
--   transpose t = Tape uss vs wss
--     where
--       uss = fmap extract (iterate upSquare t)
--       vs  = fmap extract t
--       wss = fmap extract (iterate dnSquare t)

-- diag :: Tape (Tape a)  -> Tape (Tape (Tape (Tape a)))
-- diag = Tapes.transpose . fmap duplicate . duplicate
