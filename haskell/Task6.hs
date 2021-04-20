module Task6 ( t6 ) where

t6 :: [(Int, Char)] -> [Char]
t6 [] = []
t6 ((count, symboil):lst_tail) = (take count $ repeat symboil) ++ (t6 lst_tail)

main = t6
