module RecursionSchemes where

import Fixpoint ( Fix(..), fix )

-- Recursion Schemes

cata :: Functor f => (f a -> a) -> (Fix f -> a) -> (Fix f -> a)
cata psi f = psi . fmap f . out

cataRec :: Functor f => (f a -> a) -> (Fix f -> a)
cataRec psi = fix (cata psi)

ana :: Functor f => (a -> f a) -> (a -> Fix f) -> (a -> Fix f)
ana psi f = In . fmap f . psi

anaRec :: Functor f => (a -> f a) -> (a -> Fix f)
anaRec psi = fix (ana psi)

-- Monadic

cataM :: (Monad m, Traversable f) => (f a -> m a) -> (Fix f -> m a) -> Fix f -> m a
cataM psi f e = traverse f (out e) >>= psi

cataMRec :: (Monad m, Traversable f) => (f a -> m a) -> Fix f -> m a
cataMRec psi = fix (cataM psi)
