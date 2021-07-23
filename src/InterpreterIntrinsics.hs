module InterpreterIntrinsics where

import Data.Map (fromList, (!))
import Interpreter (InterpreterEnv, Result(..))
import Primitives (Prim(..))
import Control.Monad (join)

applicativeList :: Result
applicativeList = Instance (fromList [
        ("pure", Function(\x -> return $ List [x]))
       ])

monadList :: Result
monadList = Instance (fromList [
       ("pure", let (Instance dict) = applicativeList in dict!"pure"),
        ("bind", Function (\list -> return $ Function(\f ->
                    let (List xs) = list in
                    let (Function g) = f in
                    (List . join) . ((\(List zs) -> zs) <$>) <$> sequence (g <$> xs)
               )))
       ])

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
    ("applicativeList", applicativeList),
    ("monadList", monadList),
    ("fromInteger", Function(\(Instance num) -> return $ Function (\x ->
       let (Function f) = num!"fromInteger" in f x))),
    ("+", binOp "+"),
    ("-", binOp "-"),
    ("*", binOp "*"),
    ("==", binOp "=="),
    ("[]", List []),
    (":", Function(\x -> return $ Function (\(List xs) -> return $ List (x:xs) ))),
    ("head", Function(\(List xs) -> return $ head xs)),
    ("tail", Function(\(List xs) -> return $ List (tail xs))),
    ("null", Function(\(List xs) -> return $ Value (B $ null xs))),
    ("pure", Function(\(Instance m) -> return $ Function (\x ->
       let (Function f) = m!"pure" in f x))),
    ("bind", Function(\(Instance m) -> return $
             Function(\x -> return $
             Function(\y ->
              let (Function bind) = m!"bind" in
                    do (Function g) <- bind x
                       g y ))))
 ]