module Monads where

import Location (PString)
import Control.Monad.Trans ( MonadTrans(lift) )
import Control.Monad.Identity ( Identity(runIdentity) )
import qualified Control.Monad.Trans.RWS.Lazy as S
import qualified Control.Monad.Trans.Except as E

-- ReaderState Monad
type ReaderWriterState r w s = S.RWST r w s (E.ExceptT PString Identity)

ask :: Monoid w => ReaderWriterState r w s r
ask = S.ask

local :: Monoid w => (r -> r) -> ReaderWriterState r w s a -> ReaderWriterState r w s a
local = S.local

get :: Monoid w => ReaderWriterState r w s s
get = S.get

put :: Monoid w => s -> ReaderWriterState r w s ()
put = S.put

modify :: Monoid w => (s -> s) -> ReaderWriterState r w s ()
modify f = get >>= (put . f)

throwError :: Monoid w => PString -> ReaderWriterState r w s a
throwError s = lift (E.throwE s)

catchError :: ReaderWriterState r w s a -> (PString -> ReaderWriterState r w s a) -> ReaderWriterState r w s a
catchError m f = S.RWST (\ r s -> E.catchE (S.runRWST m r s) (\e -> S.runRWST (f e) r s) )

listen :: Monoid w => ReaderWriterState r w s a -> ReaderWriterState r w s (a, w)
listen = S.listen

run :: Monoid w => ReaderWriterState r w s a -> r -> s -> Either PString (a, s, w)
run m r s = runIdentity (E.runExceptT (S.runRWST m r s))

eval :: Monoid w => ReaderWriterState r w s a -> r -> s -> Either PString (a, w)
eval m r s = runIdentity (E.runExceptT (S.evalRWST m r s))
