module Interpreter(ComputationState
	, run
	, mkUnaryFunc
	, mkBinaryFunc
	, mkBinaryIntFunc
	, mkUnaryIntFunc
	, mkAdd
	, mkSub
	, mkDiv
	, mkMul
	, mkNeg
	, mkPushStr
	, mkPushInt
	, mkPrint
	, mkDrop
	)where
import Data.List
import System.IO.Unsafe(unsafePerformIO)
import MyState
data Data = Int Int | String String | None () deriving (Show)
type ComputationState a = MyState [Data] a
run :: ComputationState a -> [Data] -> (a, [Data])
run = runMyState

-- FunctionStackItem construction helpers
mkAdd = mkBinaryIntFunc (+)
mkSub = mkBinaryIntFunc (-)
mkMul = mkBinaryIntFunc (*)
mkDiv = mkBinaryIntFunc div
mkNeg = mkUnaryIntFunc (0-)
mkPrint = mkUnaryFunc (\top -> None (unsafePerformIO $ print top))
mkPushStr :: String -> ComputationState ()
mkPushInt :: Int -> ComputationState ()
mkPushStr = mkPush . String
mkPushInt = mkPush . Int
mkPush x = modifyMyState $ \st -> x:st
mkDrop = modifyMyState $ \st ->
	case st of
		(x:xs) -> xs
		xs -> xs

mkBinaryIntFunc :: (Int -> Int -> Int) -> ComputationState Data
mkBinaryIntFunc op = mkBinaryFunc op'
	where
		op' (Int x) (Int y) = Int (x `op` y)
		op' _ _ = error "binary int op: invalid operand types"

mkUnaryIntFunc :: (Int -> Int) -> ComputationState Data
mkUnaryIntFunc op = mkUnaryFunc op'
	where
		op' (Int x) = Int $ op x
		op' _= error "unary int op: invalid operand type"

mkUnaryFunc :: (Data -> Data) -> ComputationState Data
mkUnaryFunc op = do
	st <- getMyState
	case st of
		(x:xs) -> do
			let opResult = op x
			putMyState $ opResult:xs
			return opResult
		_ -> fail "Unary func: data stack is empty"

mkBinaryFunc :: (Data -> Data -> Data) -> ComputationState Data
mkBinaryFunc op = do
	st <- getMyState
	case st of
		(x:xs) -> do
			case xs of
				(y:xs) -> do
					let opResult = x `op` y
					putMyState $ opResult:xs
					return opResult
				_ -> fail "Binary func arg2: data stack is empty"
	 	_ -> fail "Binary func arg1: data stack is empty"
