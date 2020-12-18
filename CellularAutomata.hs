{-# LANGUAGE DeriveFunctor, DeriveFoldable, InstanceSigs, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, TypeOperators, TypeFamilies #-}

module CellularAutomata where

import Control.Comonad
import Data.Function.HT ( nest )
import GHC.Generics ( Generic )
import Data.MemoTrie
--  import Data.List (transpose)

------------------------------------------------------------

data Tape a = Tape [a] a [a]
              deriving (Functor, Foldable, Eq, Generic)

newtype Square a = Square { unSquare :: Tape (Tape a) }
                     deriving (Functor, Eq, Generic)

newtype Cube a = Cube { unCube :: Tape (Square a) }
                     deriving (Functor, Eq, Show, Generic)

newtype HyperCube a = HyperCube { unHyperCube :: Square (Square a) }
                                deriving (Functor, Eq, Show, Generic)

type FiniteTape a = Tape a
type FiniteSquare a = Square a
type FiniteCube a = Cube a
type FiniteHyperCube a = HyperCube a

------------------------------------------------------------

instance Comonad Tape where
  extract = point
  duplicate = genericMove lTape rTape  

instance Show a => Show (Tape a) where
  show (Tape xs y zs) = show $ reverse (take 30 xs) ++ y : take 30 zs

instance HasTrie a => HasTrie (Tape a) where
  newtype (Tape a :->: b) = TapeTrie { unTapeTrie :: Reg (Tape a) :->: b}
  trie = trieGeneric TapeTrie
  untrie = untrieGeneric unTapeTrie
  enumerate = enumerateGeneric unTapeTrie

instance Comonad Square where
  extract (Square t) = (point . point) t
  duplicate = Square . fmap xSquare . ySquare

instance Show a => Show (Square a) where
  show (Square (Tape uss vs wss)) 
    = pretty 20 (reverse (take 20 uss)) ++ 
      show vs ++
      '\n' : pretty 20 (take 20 wss)

instance HasTrie a => HasTrie (Square a) where
  newtype (Square a :->: b) = SquareTrie { unSquareTrie :: Reg (Square a) :->: b}
  trie = trieGeneric SquareTrie
  untrie = untrieGeneric unSquareTrie
  enumerate = enumerateGeneric unSquareTrie

instance Comonad Cube where
  extract (Cube t) = ((point . point) . unSquare . point) t
  duplicate = Cube . fmap ((Square . fmap xCube) . yCube) . zCube

instance HasTrie a => HasTrie (Cube a) where
  newtype (Cube a :->: b) = CubeTrie { unCubeTrie :: Reg (Cube a) :->: b}
  trie = trieGeneric CubeTrie
  untrie = untrieGeneric unCubeTrie
  enumerate = enumerateGeneric unCubeTrie

instance Comonad HyperCube where
  extract (HyperCube t) = (extract . extract) t
  duplicate = HyperCube . Square 
            . fmap (fmap (Square . fmap xHCube . yHCube) . zHCube) . wHCube

instance HasTrie a => HasTrie (HyperCube a) where
  newtype (HyperCube a :->: b) = HyperCubeTrie { unHyperCubeTrie :: Reg (HyperCube a) :->: b}
  trie = trieGeneric HyperCubeTrie
  untrie = untrieGeneric unHyperCubeTrie
  enumerate = enumerateGeneric unHyperCubeTrie

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

lHCube, rHCube, upHCube, dnHCube :: HyperCube a -> HyperCube a
lHCube  (HyperCube a) = HyperCube (fmap lSquare a)
rHCube  (HyperCube a) = HyperCube (fmap rSquare a)
upHCube (HyperCube a) = HyperCube (fmap upSquare a)
dnHCube (HyperCube a) = HyperCube (fmap dnSquare a)

inHCube, outHCube, wPosHCube, wNegHCube :: HyperCube a -> HyperCube a
inHCube   (HyperCube a) = HyperCube (lSquare a) 
outHCube  (HyperCube a) = HyperCube (rSquare a)
wPosHCube (HyperCube a) = HyperCube (upSquare a)
wNegHCube (HyperCube a) = HyperCube (dnSquare a)

xHCube, yHCube, zHCube, wHCube :: HyperCube a -> Tape (HyperCube a)
xHCube = genericMove lHCube rHCube
yHCube = genericMove upHCube dnHCube
zHCube = genericMove inHCube outHCube
wHCube = genericMove wPosHCube wNegHCube

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

emptySquare :: a -> Square a
emptySquare x = Square $ Tape tapes (emptyTape x) tapes
  where
    tapes = repeat $ emptyTape x

emptyCube :: a -> Cube a
emptyCube x = Cube $ Tape squares (emptySquare x) squares
  where
    squares = repeat $ emptySquare x

emptyHCube :: a -> HyperCube a
emptyHCube x = HyperCube $ emptySquare (emptySquare x)

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

toFiniteHCube :: Int -> Int -> Int -> Int -> HyperCube a
              -> FiniteHyperCube a
toFiniteHCube x y z w (HyperCube h)
  = HyperCube (toFiniteSquare x y <$> toFiniteSquare z w h)

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

toListFromHCube :: FiniteHyperCube a -> [[[[a]]]]
toListFromHCube (HyperCube h)
  = toListFromSquare $ fmap toListFromSquare h

---------------------------------------------------------------------------

toTape :: a -> [a] -> Tape a
toTape init list@(x : xs)
  = nest (length list `div` 2) rTape $ Tape (repeat init) x (xs ++ repeat init)

toSquare :: a -> [[a]] -> Square a
toSquare init 
  = Square . toTape (emptyTape init) . fmap (toTape init) 

toCube :: a -> [[[a]]] -> Cube a
toCube init 
  = Cube . toTape (emptySquare init) . fmap (toSquare init)

toHCube :: a -> [[[[a]]]] -> HyperCube a
toHCube init
  = HyperCube . toSquare (emptySquare init) . (fmap . fmap) (toSquare init)

---------------------------------------------------------------------------

neighboursTape :: Tape a -> [a]
neighboursTape (Tape (x : _) y (z : _))
  = [x, y, z]

neighboursSquare :: Square a -> [a]
neighboursSquare 
  = concatMap neighboursTape . neighboursTape . unSquare 

neighboursCube :: Cube a -> [a]
neighboursCube
  = concatMap neighboursSquare . neighboursTape . unCube

neighboursHCube :: HyperCube a -> [a]
neighboursHCube
  = concatMap neighboursSquare . neighboursSquare . unHyperCube


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
