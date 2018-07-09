{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
module CoProduct where

import Fixpoint

data (f :+: g) e = Inl (f e)
                 | Inr (g e)
                 deriving (Show, Eq, Functor, Traversable, Foldable)
