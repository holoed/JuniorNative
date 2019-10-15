module Monads where

import Control.Monad.Trans
import Control.Monad.Identity
import qualified Control.Monad.Trans.RWS.Lazy as S
import qualified Control.Monad.Trans.Except as E

-- ReaderState Monad
type ReaderState r w s = S.RWST r w s (E.ExceptT String Identity)

ask :: Monoid w => ReaderState r w s r
ask = S.ask

local :: Monoid w => (r -> r) -> ReaderState r w s a -> ReaderState r w s a
local = S.local

get :: Monoid w => ReaderState r w s s
get = S.get

put :: Monoid w => s -> ReaderState r w s ()
put s = S.put s

modify :: Monoid w => (s -> s) -> ReaderState r w s ()
modify f = get >>= (put . f)

throwError :: Monoid w => String -> ReaderState r w s a
throwError s = lift (E.throwE s)

run :: Monoid w => ReaderState r w s a -> r -> s -> Either String (s, w)
run m r s = runIdentity (E.runExceptT (S.execRWST m r s))

eval :: Monoid w => ReaderState r w s a -> r -> s -> Either String (a, w)
eval m r s = runIdentity (E.runExceptT (S.evalRWST m r s))
