module InterpreterIntrinsics where

import Data.Map (fromList, (!)) 
import Interpreter (InterpreterEnv, Result(..))
import Primitives (Prim(..))

numInt :: Result
numInt = Instance (fromList [
       ("*", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x * y))))),
       ("+", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x + y))))),
       ("-", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x - y))))),
       ("fromInteger", Function(\(Value (I x)) -> return $ Value (I x)))
       ])

eqInt :: Result
eqInt = Instance (fromList [
       ("==", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (B (x == y)))))
       ])

binOp :: String -> Result
binOp op = Function(\(Instance inst) -> 
           let (Function f) = inst!op in
           return $ Function(\x -> 
           return $ Function (\y -> 
             do (Function f') <- f x
                f' y )))

env :: InterpreterEnv
env = fromList [
    ("numInt", numInt),
    ("eqInt", eqInt),
    ("fromInteger", Function(\(Instance num) -> return $ Function (\x -> 
       let (Function f) = num!"fromInteger" in f x))),
    ("+", binOp "+"),
    ("-", binOp "-"),
    ("*", binOp "*"),
    ("==", binOp "==")
 ]