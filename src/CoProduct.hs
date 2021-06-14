{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
module CoProduct where

import Fixpoint ()

data (f :+: g) e = Inl (f e)
                 | Inr (g e)
                 deriving (Show, Eq, Functor, Traversable, Foldable)

liftAlg :: (Functor f, Functor g) => (f a -> a) -> (g a -> a) -> (f :+: g) a -> a
liftAlg alg1 _ (Inl e) = alg1 e
liftAlg _ alg2 (Inr e) = alg2 e
