{-# LANGUAGE UndecidableInstances #-}

module Fixpoint where

fix :: ((a -> b) -> (a -> b)) -> (a -> b)
fix f = f (fix f)

newtype Fix f = In { out :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show (In f) = "(" ++ show f ++ ")"

instance Eq (f (Fix f)) => Eq (Fix f) where
  In x == In y = x == y  
