{-# LANGUAGE MultiWayIf #-}

module Task8 ( t8 ) where
{-
Определите функцию (ЛУКОВИЦА n),
строящую N-уровневый вложенный список,
элементом которого на самом глубоком уровне является N.
-}
data Onion = Onion Int
             | OnionList [Onion]
             deriving (Show)

t8 :: Int -> Onion
t8 num = onion_grow num
    where
        onion_grow depth =
            if  | depth == 0 -> Onion num
                | otherwise -> OnionList [onion_grow (depth - 1)]


main = t8
