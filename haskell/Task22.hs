{-# LANGUAGE MultiWayIf #-}

module Task22 ( t22 ) where
{-
Определите функцию ОБЬЕДИНЕНИЕ, формирующую обьединение двух множеств.
-}
inset :: Eq a => a -> [a] -> Bool
inset x [] = False
inset x (item: set) =
    if  | x == item -> True
        | otherwise -> x `inset` set

--union of two sets
t22 :: Eq a => [a] -> [a] -> [a]
t22 [] ySet = ySet
t22 (xItem: xSet) ySet =
    if  | xItem `inset` ySet -> (t22 xSet ySet)
        | otherwise -> xItem : (t22 xSet ySet)

main = print (t22 [1, 2, 3, 4, 5] [4, 5, 6, 7, 8, 1])
