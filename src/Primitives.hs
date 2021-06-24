module Primitives where

data Prim = U | I Int | D Double | B Bool | S String deriving (Eq, Show)

strToPrim :: String -> Prim
strToPrim s = 
    if floor d == ceiling d
    then I (round d)
    else D d
    where d = read s :: Double