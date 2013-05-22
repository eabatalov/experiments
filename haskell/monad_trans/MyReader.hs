{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MyReader(MyReaderT(..)
	) where
import Control.Monad
import Control.Monad.Reader

data MyReaderT env m a = MyReaderT { runMyReaderT :: env -> m a }

instance (Monad m) => Monad (MyReaderT env m) where
	(>>=) = bindMyReaderT
	return = returnMyReaderT

bindMyReaderT :: (Monad m) =>
	(MyReaderT env m) a -> (a -> (MyReaderT env m) b) -> (MyReaderT env m) b
bindMyReaderT mta k = MyReaderT $ \env -> do
	a <- runMyReaderT mta env
	b <- runMyReaderT (k a) env
	return b

returnMyReaderT :: (Monad m) => a -> MyReaderT env m a
returnMyReaderT a = MyReaderT $ \_ -> return a

instance (Monad m) => MonadReader env (MyReaderT env m) where
	ask = myAskT
	local = myLocalT
	reader = myReaderT

myAskT :: (Monad m) => MyReaderT env m env
myAskT = MyReaderT $ \env -> return env

myLocalT :: (Monad m) => (env -> env) -> MyReaderT env m a -> MyReaderT env m a
myLocalT fenv rt = MyReaderT $ \env -> runMyReaderT rt (fenv env)

myReaderT :: (Monad m) => (env -> a) -> MyReaderT env m a
myReaderT fenv = MyReaderT $ \env -> return (fenv env)

instance MonadTrans (MyReaderT env) where
	lift = liftMyReaderT

liftMyReaderT :: (Monad m) => m a -> MyReaderT env m a
liftMyReaderT ma = MyReaderT $ \env -> ma
