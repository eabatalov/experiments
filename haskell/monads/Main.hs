import MyState
import Interpreter
main = print $ runMyState computation []

computation = computation2

-- computation1 :: MyState [Int] String
computation1 = do
	initial <- getMyState
	putMyState (1:2:3:4:5:6:7:8:9:10:initial)
	modifyMyState $ map $ \x -> x * 10
	modifyMyState $ (++ [9, 8, 7, 6, 5, 4, 3, 2, 1])
	return "ok"

-- computation2 :: ComputationState Int
computation2 = do
	mkPushStr "Wow!"
	mkPushInt 5
	mkPushInt 5
	mkPushInt 5
	mkAdd
	mkSub
	mkPrint
	mkDrop
	mkPrint
	return "ok"
