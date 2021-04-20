{-# LANGUAGE MultiWayIf #-}

module Task11 ( t11_1, t11_2 ) where
{-
Определите функции, осуществляющие преобразование между видами (a b c) и (((а) b) с)
-}
data Cascade a = Cascade a
               | CascadeUp [Cascade a]
               deriving (Show)

t11_1 :: [a] -> Cascade a
t11_1 [] = CascadeUp []
t11_1 (x:lst) = gen_cascade lst (CascadeUp [ Cascade x ])
    where
        gen_cascade [] prev_casc = prev_casc
        gen_cascade (x:lst) prev_casc =
            gen_cascade lst (CascadeUp [prev_casc, Cascade x])

t11_2 :: Cascade a -> [a]
t11_2 (CascadeUp [ Cascade item ]) = [item]
t11_2 (CascadeUp [ cascade, Cascade item ]) = (t11_2 cascade) ++ [item]
main = t11_1 . t11_2
