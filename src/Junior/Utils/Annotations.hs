{-# LANGUAGE DeriveTraversable #-}

module Junior.Utils.Annotations where

import Junior.Utils.Fixpoint ( Fix(In) )
import Junior.Utils.RecursionSchemes ( cataRec, cataMRec )

data Ann x f a = Ann x (f a)
    deriving (Eq, Show, Functor, Foldable, Traversable)

unwrap :: Functor f => Fix (Ann x f) -> Fix f
unwrap = cataRec alg
 where alg (Ann _ e)  = In e

wrap :: Functor f => x -> Fix f -> Fix (Ann x f)
wrap x = cataRec alg
  where alg e = In (Ann x e)

mapAnn :: Functor f => (a -> b) -> Fix (Ann a f) -> Fix (Ann b f)
mapAnn f = cataRec alg
    where alg (Ann x e) = In (Ann (f x) e)

mapAnnM :: (Monad m, Traversable f) => (a -> m b) -> Fix (Ann a f) -> m (Fix (Ann b f))
mapAnnM f = cataMRec alg
    where alg (Ann x e) = do
           x' <- f x
           return $ In (Ann x' e)