{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module MyState(MyStateT(..)
	) where
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Class
import Data.Monoid
import MyWriter
import MyReader

data MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

instance (Monad m) => Monad (MyStateT s m) where
	return = returnMyStateT
	(>>=) = bindMyStateT

returnMyStateT :: (Monad m) => a -> MyStateT s m a
returnMyStateT a = MyStateT $ \s -> return (a, s)

bindMyStateT :: (Monad m) =>
	MyStateT s m a -> (a -> MyStateT s m b) -> MyStateT s m b
bindMyStateT mta k = MyStateT $ \s -> do
	let ma = runMyStateT mta
	(a, s1) <- ma s
	let mb = runMyStateT (k a) s1
	mb

instance (Monad m) => MonadState s (MyStateT s m) where
	get = myGetT
	put = myPutT
	state = myStateT

myGetT :: (Monad m) => MyStateT s m s
myGetT = MyStateT $ \s -> return (s, s)

myPutT :: (Monad m) => s -> MyStateT s m ()
myPutT s = MyStateT $ \_ -> return ((), s)

myStateT :: (Monad m) => (s -> (a, s)) -> MyStateT s m a
myStateT fs = MyStateT $ \s -> return $ fs s

instance MonadTrans (MyStateT s) where
	lift = liftMyStateT

liftMyStateT :: (Monad m) => m a -> MyStateT s m a
liftMyStateT ma = MyStateT $ \s -> do
	a <- ma
	return (a, s)

{- USER CONVENIENCE INSTANCES -}
instance (Monoid w, MonadState s m) => MonadState s (MyWriterT w m) where
	get = lift get
	put = lift . put
	state = lift . state

instance (MonadState s m) => MonadState s (MyReaderT env m) where
	get = lift get
	put = lift . put
	state = lift . state
