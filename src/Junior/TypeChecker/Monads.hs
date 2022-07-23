module Junior.TypeChecker.Monads where

import Control.Monad.Trans ( MonadTrans(lift) )
import Control.Monad.Identity ( Identity(runIdentity) )
import qualified Control.Monad.Trans.RWS.Lazy as S
import qualified Control.Monad.Trans.Except as E

type ErrorReaderWriterState e r w s = S.RWST r w s (E.ExceptT e Identity)

ask :: Monoid w => ErrorReaderWriterState e r w s r
ask = S.ask

local :: Monoid w => (r -> r) -> ErrorReaderWriterState e r w s a -> ErrorReaderWriterState e r w s a
local = S.local

get :: Monoid w => ErrorReaderWriterState e r w s s
get = S.get

put :: Monoid w => s -> ErrorReaderWriterState e r w s ()
put = S.put

modify :: Monoid w => (s -> s) -> ErrorReaderWriterState e r w s ()
modify f = get >>= (put . f)

throwError :: Monoid w => e -> ErrorReaderWriterState e r w s a
throwError s = lift (E.throwE s)

catchError :: ErrorReaderWriterState e r w s a -> (e -> ErrorReaderWriterState e r w s a) -> ErrorReaderWriterState e r w s a
catchError m f = S.RWST (\ r s -> E.catchE (S.runRWST m r s) (\e -> S.runRWST (f e) r s) )

listen :: Monoid w => ErrorReaderWriterState e r w s a -> ErrorReaderWriterState e r w s (a, w)
listen = S.listen

run :: Monoid w => ErrorReaderWriterState e r w s a -> r -> s -> Either e (a, s, w)
run m r s = runIdentity (E.runExceptT (S.runRWST m r s))

eval :: Monoid w => ErrorReaderWriterState e r w s a -> r -> s -> Either e (a, w)
eval m r s = runIdentity (E.runExceptT (S.evalRWST m r s))
