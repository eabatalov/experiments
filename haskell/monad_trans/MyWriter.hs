{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module MyWriter(MyWriterT(..)
	) where
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans.Class
import MyReader

data MyWriterT w m a = MyWriterT { runMyWriterT :: m (a, w) }

instance (Monoid w, Monad m) => Monad (MyWriterT w m) where
	(>>=) = bindMyWriterT
	return = returnMyWriterT

bindMyWriterT :: (Monad m, Monoid w) =>
	MyWriterT w m a -> (a -> MyWriterT w m b) -> MyWriterT w m b
bindMyWriterT mta k = MyWriterT $ do
		let ma = runMyWriterT mta
		(a, w1) <- ma
		let mb = runMyWriterT $ k a
		(b, w2) <- mb
		return (b, w1 `mappend` w2)

returnMyWriterT :: (Monad m, Monoid w) => a -> MyWriterT w m a
returnMyWriterT a = MyWriterT $ return (a, mempty)

instance (Monad m, Monoid w) => MonadWriter w (MyWriterT w m) where
	writer = myWriterT
	tell = myTellT
	listen = myListenT
	pass = myPassT

myWriterT :: (Monad m, Monoid w) => (a, w) -> MyWriterT w m a
myWriterT = MyWriterT . return

myTellT :: (Monad m, Monoid w) => w -> MyWriterT w m ()
myTellT w = MyWriterT $ return ((), w)

myListenT :: (Monad m, Monoid w) => MyWriterT w m a -> MyWriterT w m (a, w)
myListenT mta = MyWriterT $ do
	let ma = runMyWriterT mta
	(a, w) <- ma
	return ((a, w), w)

myPassT :: (Monad m, Monoid w) => MyWriterT w m (a, w -> w) -> MyWriterT w m a
myPassT mta = MyWriterT $ do
	let ma = runMyWriterT mta
	((a, fw ), w) <- ma
	return (a, fw w)

instance (Monoid w) => MonadTrans (MyWriterT w) where
	lift = liftMyWriterT

liftMyWriterT :: (Monad m, Monoid w) => m a -> MyWriterT w m a
liftMyWriterT ma = MyWriterT $ do
	a <- ma
	return (a, mempty)

{- == USER CONVENIENCE INSTANCES -}
instance (Monoid w, MonadWriter w m) => MonadWriter w (MyReaderT env m) where
	writer = lift . writer
	tell = lift . tell
	listen = undefined
	pass = undefined
