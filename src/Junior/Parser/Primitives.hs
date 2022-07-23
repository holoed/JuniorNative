module Junior.Parser.Primitives where

data Prim = U | I !Int | D !Double | B !Bool | S !String | C !Char deriving (Eq, Show)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

strToPrim :: String -> Prim
strToPrim s = 
    if isInt d
    then I (round d)
    else D d
    where d = read s :: Double

primToStr :: Prim -> String
primToStr (I n) = show n
primToStr (D x) = show x
primToStr (B b) = show b
primToStr (S s) = s
primToStr (C c) = [c]
primToStr U = "()"