{-# LANGUAGE MultiWayIf #-}

module Task21 ( t21 ) where
{-
Определитефункцию ПЕРЕСЕЧЕНИЕ, формирующую пересечение двух множеств, т.е. множество из их общих элементов.
-}
inset :: Eq a => a -> [a] -> Bool
inset x [] = False
inset x (item: set) =
    if  | x == item -> True
        | otherwise -> x `inset` set

--intersection of two sets
t21 :: Eq a => [a] -> [a] -> [a]
t21 [] _ = []
t21 (xItem: xSet) ySet =
    if  | xItem `inset` ySet -> xItem : (t21 xSet ySet)
        | otherwise -> (t21 xSet ySet)

main = print (t21 [1, 2, 3, 4, 5] [4, 5, 6, 7, 8, 1])
