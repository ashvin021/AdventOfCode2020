module Reusable where

import Data.List ( intersect )

(.:) = (.).(.)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

intersection :: Eq a => [[a]] -> [a]
intersection = foldr1 intersect
