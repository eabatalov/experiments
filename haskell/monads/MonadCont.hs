module MonadCont() where
import Control.Monad

class MyMonad m where
	(|>>=) :: m a -> (a -> m b) -> m b
	(|>>)  :: m a -> m b -> m b
	retrn :: a -> m a
	fail :: String -> m a
	ma |>> mb = ma |>>= (\_ -> mb)
	fail s = error s
infixl 0 |>>=
data MonadCont r a = Cont { runCont :: ((a -> r) -> r) }

instance MyMonad (MonadCont r) where
	retrn a = Cont (\cont -> cont a)
	ma |>>= k = Cont (\cont -> runCont ma (\a -> runCont (k a) cont))
	-- cont :: b -> r

sqr x = Cont $ \cont -> cont $ x * x

sumSqr :: (Num a) => a -> MonadCont r a
sumSqr x =
--	when (x < 5) (cont x) >>
	sqr x |>>= \a1 -> 
	sqr x |>>= \a2 ->
	retrn (a1 + a2)

cllCC :: ((a -> MonadCont r b) -> MonadCont r a) -> MonadCont r a
cllCC f = Cont $ \k -> runCont (f (\a -> Cont $ \_ -> k a)) k
-- k :: a -> r
-- k = id
-- runCont (f (\a -> Cont $ \_ -> a)) id

sumSqr' :: (Num a) => a -> MonadCont r a
sumSqr' x = cllCC $ \cont ->
	sqr x |>>= \a1 ->
	sqr x |>>= \a2 ->
	cont $ a1 + a2
-- cont :: (a -> MonadCont r b)
-- \cont -> ... :: ((a -> MonadCont r a) -> MonadCont r a)

whn :: (MyMonad m) => Bool -> m () -> m ()
whn b m = if b then m else retrn ()

sumSqr'' :: (Num a, Ord a) => a -> MonadCont r a
sumSqr'' x = cllCC $ \cont ->
	sqr x |>>= \a1 ->
	sqr x |>>= \a2 ->
	let sum = a1 + a2 in
	if sum > 29 then cont sum else retrn $ error "hui!"




