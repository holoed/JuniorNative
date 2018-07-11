{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module Annotations where

import Fixpoint
import RecursionSchemes

data Ann x f a = Ann x (f a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

unwrap :: Functor f => Fix (Ann x f) -> Fix f
unwrap = cataRec alg
 where alg (Ann _ e)  = In e

wrap :: Functor f => x -> Fix f -> Fix (Ann x f)
wrap x = cataRec alg
  where alg e = In (Ann x e)
