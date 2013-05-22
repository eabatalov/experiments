module MyState(MyState(MyState)
	, runMyState
	, putMyState
	, getMyState
	, modifyMyState
	)where
import Control.Monad
data MyState s a = MyState { runMyState :: (s -> (a, s))}

instance Monad (MyState s) where
	--(>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
	-- return :: a -> MyState s a
	-- fail :: String -> MyState s a
	st >>= k = MyState $ \s ->
		let (result, resultState) = (runMyState st s)
		in runMyState (k result) resultState
	return a = MyState (\s -> (a, s))
	fail str = MyState (\s -> (error str, s))

putMyState :: s -> MyState s ()
putMyState st = MyState $ \s -> ((), st)

getMyState :: MyState s s
getMyState = MyState (\s -> (s, s)) 

modifyMyState :: (s -> s) -> MyState s ()
modifyMyState f = MyState $ \s -> ((), f s)
