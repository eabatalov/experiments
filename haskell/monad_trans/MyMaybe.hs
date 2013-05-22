module MyMaybe(MyMaybeT(..)
	) where
import Control.Monad
import Control.Monad.Trans.Class

data MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MyMaybeT m) where
	return = returnMyMaybeT
	fail = failMyMaybeT
	(>>=) = bindMyMaybeT

returnMyMaybeT :: (Monad m) => a -> MyMaybeT m a
returnMyMaybeT a = MyMaybeT $ return $ Just a

failMyMaybeT :: (Monad m) => String -> MyMaybeT m a
failMyMaybeT _ = MyMaybeT $ return Nothing

bindMyMaybeT :: (Monad m) => MyMaybeT m a -> (a -> MyMaybeT m b) -> MyMaybeT m b
bindMyMaybeT mta k = MyMaybeT $ do
	let ma = runMyMaybeT mta
	maybeA <- ma
	case maybeA of
		Just a -> runMyMaybeT $ k a
		Nothing -> return Nothing

instance MonadTrans MyMaybeT where
	lift = liftMyMaybeT

liftMyMaybeT :: (Monad m) => m a -> MyMaybeT m a
liftMyMaybeT ma = MyMaybeT $ ma >>= return . Just
