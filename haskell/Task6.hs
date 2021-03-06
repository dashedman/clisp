module Task6 ( t6 ) where
{-
Определите функцию, упаковывающую подсписки вида (M N) в последовательные дубликаты списка, 
где N - элемент списка, M - количество повторений.

Например,
[(4,’a’),(1,’b’),(2,’c’),(2,’a’),(1,’d’),(4,’e’)]
должен быть переведен в
[’a’,’a’,’a’,’a’,’b’,’c’,’c’,’a’,’a’,’d’,’e’,’e’,’e’,’e’].
-}
t6 :: [(Int, Char)] -> [Char]
t6 [] = []
t6 ((count, symboil):lst_tail) = (take count $ repeat symboil) ++ (t6 lst_tail)

main = t6
