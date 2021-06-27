module Primitives where

data Prim = U | I Int | D Double | B Bool | S String deriving (Eq, Show)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

strToPrim :: String -> Prim
strToPrim s = 
    if isInt d
    then I (round d)
    else D d
    where d = read s :: Double