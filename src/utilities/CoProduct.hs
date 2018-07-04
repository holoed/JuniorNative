{-# LANGUAGE TypeOperators #-}
module CoProduct where

data (f :+: g) e = Inl (f e) | Inr (g e)

instance (Functor f , Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)
