module RecursionSchemes where

import Fixpoint

-- Recursion Schemes

cata :: Functor f => (f a -> a) -> (Fix f -> a) -> (Fix f -> a)
cata psi f = psi . fmap f . out

cataRec :: Functor f => (f a -> a) -> (Fix f -> a)
cataRec psi = fix (cata psi)

ana :: Functor f => (a -> f a) -> (a -> Fix f) -> (a -> Fix f)
ana psi f = In . fmap f . psi

anaRec :: Functor f => (a -> f a) -> (a -> Fix f)
anaRec psi = fix (ana psi)
