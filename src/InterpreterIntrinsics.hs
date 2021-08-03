{-# LANGUAGE OverloadedStrings #-}
module InterpreterIntrinsics where

import Data.HashMap.Strict (fromList, (!), member, HashMap)
import InterpreterMonad (InterpreterEnv, Result(..), Prim(..))
import Control.Monad (join)

floatingDouble :: Result
floatingDouble = Instance (fromList [
        ("cos", Function(\(Value (D x)) -> return $ Value (D (cos x)))),
        ("sin", Function(\(Value (D x)) -> return $ Value (D (sin x))))
       ])

ordInt :: Result
ordInt = Instance (fromList [
        (">", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (B (x > y))))),
        ("<", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (B (x < y))))),
        (">=", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (B (x >= y))))),
        ("<=", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (B (x <= y))))),
        ("==", let (Instance inst) = eqInt in inst!"==")
       ])

ordDouble :: Result
ordDouble = Instance (fromList [
        (">", Function(\(Value (D x)) -> return $ Function (\(Value (D y)) -> return $ Value (B (x > y))))),
        ("<", Function(\(Value (D x)) -> return $ Function (\(Value (D y)) -> return $ Value (B (x < y))))),
        (">=", Function(\(Value (D x)) -> return $ Function (\(Value (D y)) -> return $ Value (B (x >= y))))),
        ("<=", Function(\(Value (D x)) -> return $ Function (\(Value (D y)) -> return $ Value (B (x <= y)))))
       ])

functorList :: Result
functorList = Instance (fromList [
   ("fmap", Function(\(Function f) -> return $ Function(\(List xs) -> List <$> mapM f xs)))
  ])

applicativeList :: Result
applicativeList = Instance (fromList [
        ("pure", Function(\x -> return $ List [x])),
        ("<*>", Function(\(List fs) -> return $ Function(\(List xs) ->
                let vs = sequence $ do (Function f) <- fs
                                       f <$> xs in
                List <$> vs)))
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

fractionalDouble :: Result
fractionalDouble = Instance (fromList [
       ("+", let (Instance inst) = numDouble in inst!"+"),
       ("-", let (Instance inst) = numDouble in inst!"-"),
       ("*", let (Instance inst) = numDouble in inst!"*"),
       ("/", Function(\(Value (D x)) -> return $ Function (\(Value (D y)) -> return $ Value (D (x / y))))),
       ("fromRational", Function(\(Value (D x)) -> return $ Value (D x))),
       ("fromInteger", let (Instance inst) = numDouble in inst!"fromInteger")
       ])

fractionalInt :: Result
fractionalInt = Instance (fromList [
       ("+", let (Instance inst) = numInt in inst!"+"),
       ("-", let (Instance inst) = numInt in inst!"-"),
       ("*", let (Instance inst) = numInt in inst!"*"),
       ("/", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x `div` y))))),
       ("fromRational", Function(\(Value (D x)) -> return $ Value (I $ truncate x))),
       ("fromInteger", let (Instance inst) = numInt in inst!"fromInteger")
       ])

numInt :: Result
numInt = Instance (fromList [
       ("*", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x * y))))),
       ("+", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x + y))))),
       ("-", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (I (x - y))))),
       ("fromInteger", Function(\(Value (I x)) -> return $ Value (I x)))
       ])

numDouble :: Result
numDouble = Instance (fromList [
       ("*", Function(\(Value (D x)) -> return $ Function (\(Value (D y)) -> return $ Value (D (x * y))))),
       ("+", Function(\(Value (D x)) -> return $ Function (\(Value (D y)) -> return $ Value (D (x + y))))),
       ("-", Function(\(Value (D x)) -> return $ Function (\(Value (D y)) -> return $ Value (D (x - y))))),
       ("fromInteger", Function(\(Value (I x)) -> return $ Value (D $ fromIntegral x)))
       ])

eqBool :: Result
eqBool = Instance (fromList [
       ("==", Function(\(Value (B x)) -> return $ Function (\(Value (B y)) -> return $ Value (B (x == y)))))
       ])

eqInt :: Result
eqInt = Instance (fromList [
       ("==", Function(\(Value (I x)) -> return $ Function (\(Value (I y)) -> return $ Value (B (x == y)))))
       ])

eqString :: Result
eqString = Instance (fromList [
       ("==", Function(\(Value (S x)) -> return $ Function (\(Value (S y)) -> return $ Value (B (x == y)))))
       ])

eqTuple2 :: Result
eqTuple2 = Function(\(Instance instA) -> return $ Function(\(Instance instB) -> return $ Instance(fromList [
   ("==", Function(\(Tuple [x1,y1]) -> return $ Function (\(Tuple [x2,y2]) ->
          let (Function f1) = instA ! "==" in
          let (Function g1) = instB ! "==" in
          let ret1 = do (Function f2) <- f1 x1
                        f2 x2 in
          let ret2 = do (Function g2) <- g1 y1
                        g2 y2 in
          do v@(Value (B b)) <- ret1
             if b then ret2
             else return v)))
  ])))

numTuple2op :: HashMap String Result -> HashMap String Result -> String -> Result
numTuple2op instA instB op =
       Function(\(Tuple [x1,y1]) -> return $ Function (\(Tuple [x2,y2]) ->
          let (Function f1) = instA ! op in
          let (Function g1) = instB ! op in
          let ret1 = do (Function f2) <- f1 x1
                        f2 x2 in
          let ret2 = do (Function g2) <- g1 y1
                        g2 y2 in
          do r1 <- ret1
             r2 <- ret2
             return $ Tuple [r1, r2]))

numTuple2 :: Result
numTuple2 = Function(\(Instance instA) -> return $ Function(\(Instance instB) -> return $ Instance(fromList [
   ("fromInteger", Function(\r -> return $ Tuple [r, r])),
   ("+", numTuple2op instA instB "+"),
   ("-", numTuple2op instA instB "-"),
   ("*", Function(\(Tuple [re1,im1]) -> return $ Function (\(Tuple [re2,im2]) ->
          let (Function f1) = instA ! "*" in
          let (Function h1) = instA ! "-" in
          let (Function j1) = instA ! "+" in
          let re1re2 = do (Function f2) <- f1 re1
                          f2 re2 in
          let im1im2 = do (Function f2) <- f1 im1
                          f2 im2 in
          let part1 =  do v <- re1re2
                          w <- im1im2
                          (Function h2) <- h1 v
                          h2 w in
          let re1im2 = do (Function f2) <- f1 re1
                          f2 im2 in
          let im1re2 = do (Function f2) <- f1 im1
                          f2 re2 in
          let part2 =  do v <- re1im2
                          w <- im1re2
                          (Function h2) <- j1 v
                          h2 w in
          do r1 <- part1
             r2 <- part2
             return $ Tuple [r1, r2])))
  ])))

binOp :: String -> Result
binOp op = Function(\(Instance inst) ->
           if not $ member op inst then error (op ++ " not found in instance") else
           let (Function f) = inst!op in
           return $ Function(\x ->
           return $ Function (\y ->
             do (Function f') <- f x
                f' y )))

env :: InterpreterEnv
env = (fromList [
    ("floatingDouble", floatingDouble),
    ("fractionalDouble", fractionalDouble),
    ("fractionalInt", fractionalInt),
    ("ordInt", ordInt),
    ("ordDouble", ordDouble),
    ("numInt", numInt),
    ("numDouble", numDouble),
    ("numTuple2", numTuple2),
    ("eqBool", eqBool),
    ("eqInt", eqInt),
    ("eqString", eqString),
    ("eqTuple2", eqTuple2),
    ("functorList", functorList),
    ("applicativeList", applicativeList),
    ("monadList", monadList),
    ("fromInteger", Function(\(Instance num) -> return $ Function (\x ->
       if not $ member "fromInteger" num then error "fromInteger not found" else
       let (Function f) = num!"fromInteger" in f x))),
    ("fromRational", Function(\(Instance num) -> return $ Function (\x ->
       if not $ member "fromRational" num then error "fromRational not found" else
       let (Function f) = num!"fromRational" in f x))),
    (">", binOp ">"),
    ("<", binOp "<"),
    (">=", binOp ">="),
    ("<=", binOp "<="),
    ("+", binOp "+"),
    ("-", binOp "-"),
    ("*", binOp "*"),
    ("/", binOp "/"),
    ("==", binOp "=="),
    ("[]", List []),
    (":", Function(\x -> return $ Function (\(List xs) -> return $ List (x:xs) ))),
    ("head", Function(\(List xs) -> return $ head xs)),
    ("tail", Function(\(List xs) -> return $ List (tail xs))),
    ("null", Function(\(List xs) -> return $ Value (B $ null xs))),
    ("&&", Function(\(Value (B x)) -> return $ Function(\(Value (B y)) -> return $ Value (B (x && y))))),
    ("||", Function(\(Value (B x)) -> return $ Function(\(Value (B y)) -> return $ Value (B (x || y))))),
    (".", Function(\(Function f) -> return $
                     Function(\(Function g) -> return $
                     Function(\x -> do y <- g x
                                       f y)))),
    ("fmap", Function(\(Instance f) -> return $
             Function(\x -> return $
             Function(\y ->
              let (Function fmap') = f!"fmap" in
                    do (Function g) <- fmap' x
                       g y )))),
    ("pure", Function(\(Instance m) -> return $ Function (\x ->
       let (Function f) = m!"pure" in f x))),
    ("<*>", Function(\(Instance f) -> return $
             Function(\x -> return $
             Function(\y ->
              let (Function ap) = f!"<*>" in
                    do (Function g) <- ap x
                       g y )))),
    ("bind", Function(\(Instance m) -> return $
             Function(\x -> return $
             Function(\y ->
              let (Function bind) = m!"bind" in
                    do (Function g) <- bind x
                       g y )))),
    ("toDouble", Function (\(Value (I n)) ->
           return $ Value (D $ fromIntegral n))),
    ("truncate", Function (\(Value (D x)) ->
           return $ Value (I $ truncate x))),
    ("cos", Function(\(Instance m) -> return $ Function (\x ->
       let (Function f) = m!"cos" in f x))),
    ("fst", Function(\(Tuple [x,_]) -> return x)),
    ("snd", Function(\(Tuple [_,y]) -> return y))], fromList [])
 