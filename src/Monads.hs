module Monads where

import Control.Monad.Identity
import Control.Monad.Trans
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Monad.Trans.Except as E

-- ReaderState Monad
type ReaderState r s = R.ReaderT r (S.StateT s (E.ExceptT String Identity))

ask :: ReaderState r s r
ask = R.ask

local :: (r -> r) -> ReaderState r s a -> ReaderState r s a
local = R.local

get :: ReaderState r s s
get = lift S.get

put :: s -> ReaderState r s ()
put s = lift (S.put s)

throwError :: String -> ReaderState r s a
throwError s = lift (lift (E.throwE s))

run :: ReaderState r s a -> r -> s -> Either String s
run m r s = runIdentity (E.runExceptT (S.execStateT (R.runReaderT m r) s))
