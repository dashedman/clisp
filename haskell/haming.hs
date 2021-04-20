{-# LANGUAGE MultiWayIf #-}

module Haming (
    ham_encode, ham_decode
) where

{-------------------------------------------------------------------------------------------


Реализовать энкодер и декодер для кода Хемминга способного исправлять одну ошибку.

представление данных в виде [Bool]

type Bit = Bool
ham_encode :: [Bit] -> [Bit]
ham_decode :: [Bit] -> [Bit]


-------------------------------------------------------------------------------------------}


import Debug.Trace
debug = flip trace

type Bit = Bool

xor :: Bool -> Bool -> Bool
xor a b = not (a == b)

toStr :: Bit -> String
toStr b =
    if  | b -> "1"
        | otherwise -> "0"

toBit :: [Int] -> [Bit]
toBit [] = []
toBit (x:t) =
    if  | x == 0 -> False : toBit t
        | otherwise -> True : toBit t
toArr :: [Bit] -> [Int]
toArr [] = []
toArr (x:t) =
    if  | x -> 1 : toArr t
        | otherwise -> 0 : toArr t

get :: [Bit] -> Int -> Bit
get [] _ = False
get (bit:_) 1 = bit
get (bit: bitarray) index = get bitarray (index - 1)

set :: [Bit] -> Int -> Bit -> [Bit]
set [] _ _ = []
set (bit: bitarray) 1 new = new:bitarray
set (bit: bitarray) index new = bit:(set bitarray (index - 1) new)

calc_sum_on_intervals :: [Bit] -> Int -> Bit
calc_sum_on_intervals bitarray bit_num = interval_skiper 1 bit_num bitarray
    where
        size = bit_num

        interval_walker :: Int -> Int -> [Bit] -> Bit
        interval_walker index _ [] = False
        interval_walker index end_point (bit: bitarray) =
            if  | index == end_point -> interval_skiper index (end_point + size) (bit:bitarray)
                | interval_walker (index + 1) end_point bitarray -> (not bit) -- `debug` ((show index)++" "++(show end_point)++" "++(toStr bit)++" prev 1 "++(show (toArr bitarray)))
                | otherwise -> bit -- `debug` ((show index)++" "++(show end_point)++" "++(toStr bit)++" prev 0 "++(show (toArr bitarray)))

        interval_skiper :: Int -> Int -> [Bit] -> Bit
        interval_skiper _ _ [] = False
        interval_skiper index end_point (bit: bitarray) =
            if  | index == end_point -> interval_walker index (end_point + size) (bit:bitarray) -- `debug` ((show index)++" "++(show end_point)++" "++(toStr bit)++" skip e "++(show (toArr bitarray)))
                | otherwise -> interval_skiper (index + 1) end_point bitarray  -- `debug` ((show index)++" "++(show end_point)++" "++(toStr bit)++" skip   "++(show (toArr bitarray)))


calc_sums :: Int -> [Bit] -> [Bit]
calc_sums index bitarray =
    if  | index > length bitarray -> bitarray
        | otherwise -> calc_sums (index*2) (set bitarray index (calc_sum_on_intervals bitarray index))


extendCode :: Int -> Int -> [Bit] -> [Bit]
extendCode _ _ [] = []
extendCode exPoint index bitarray =
    if  | index == exPoint -> False : (extendCode (exPoint * 2) (index + 1) bitarray)
        | otherwise -> (head bitarray):(extendCode exPoint (index + 1) (tail bitarray))

ham_encode :: [Bit] -> [Bit]
ham_encode = (calc_sums 1) . (extendCode 1 1)


check_sums :: Int -> [Bit] -> (Int, Bit)
check_sums index bitarray = get_fix 1 0
    where
        get_fix index error_index =
            if  | index > length bitarray -> (error_index, False)
                | let cont = (get bitarray index) in cont /= (cont `xor` (calc_sum_on_intervals bitarray index)) -> get_fix (index*2) (error_index + index)
                | otherwise -> get_fix (index*2) error_index


truncateCode  :: Int -> Int -> [Bit] -> [Bit]
truncateCode _ _ [] = []
truncateCode exPoint index (bit:bitarray) =
    if  | index == exPoint -> (truncateCode (exPoint * 2) (index + 1) bitarray)
        | otherwise -> bit:(truncateCode exPoint (index + 1) bitarray)

ham_decode :: [Bit] -> [Bit]
ham_decode =
    (truncateCode 1 1) . (\bitarray ->
        let (index, bit) = (check_sums 1 bitarray) in
        if index == 0
        then bitarray
        else set bitarray index bit)
