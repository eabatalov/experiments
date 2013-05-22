module Interpreter(Stack(..)
	, DataStack
	, mkDataStack
	, FunctionStack
	, mkFunctionStack
	, FunctionStackItem(..)
	, DataStackItem(..)
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
	, mkPrint
	, mkDrop
	)where
import Data.List
import System.IO.Unsafe(unsafePerformIO)
data DataStackItem = Int Int | String String | None () deriving (Show)
type StackFunc = DataStack -> DataStack
data FunctionStackItem = FunctionStackItem {getFunc :: StackFunc}

type DataStack = Stack DataStackItem
type FunctionStack = Stack FunctionStackItem

run :: FunctionStack -> DataStack -> DataStack
run fs ds = foldl' app ds (getList fs)
	where app ds fi = (getFunc fi) ds
-- Helpers to construct computation
mkDataStack :: [DataStackItem] -> DataStack
mkDataStack = Stack

mkFunctionStack :: [FunctionStackItem] -> FunctionStack
mkFunctionStack = Stack
-- FunctionStackItem construction helpers
mkAdd = mkBinaryIntFuncItem (+)
mkSub = mkBinaryIntFuncItem (-)
mkMul = mkBinaryIntFuncItem (*)
mkDiv = mkBinaryIntFuncItem div
mkNeg = mkUnaryIntFuncItem (0-)
mkPrint = mkUnaryFuncItem (\top -> None (unsafePerformIO $ print top))
mkDrop = FunctionStackItem $
	\st -> case st of 
		(Stack []) -> st
		(Stack (x:xs)) -> Stack xs

mkBinaryFuncItem :: (DataStackItem -> DataStackItem -> DataStackItem) -> FunctionStackItem
mkBinaryFuncItem = FunctionStackItem . mkBinaryFunc
mkUnaryFuncItem :: (DataStackItem -> DataStackItem) -> FunctionStackItem
mkUnaryFuncItem = FunctionStackItem . mkUnaryFunc

mkBinaryIntFuncItem = FunctionStackItem . mkBinaryIntFunc
mkUnaryIntFuncItem = FunctionStackItem . mkUnaryIntFunc

mkBinaryIntFunc op = mkBinaryFunc op'
	where
		op' (Int x) (Int y) = Int (x `op` y)
		op' _ _ = error "binary int op: invalid operand types"

mkUnaryIntFunc op = mkUnaryFunc op'
	where
		op' (Int x) = Int $ op x
		op' _= error "unary int op: invalid operand type"

mkUnaryFunc :: (DataStackItem -> DataStackItem) -> StackFunc
mkUnaryFunc op st = case pop st of
	(Just top, popedSt) -> push popedSt (op top)
	(Nothing, _) -> error $ "Unary func: data stack is empty"

mkBinaryFunc :: (DataStackItem -> DataStackItem -> DataStackItem) -> StackFunc
mkBinaryFunc op st = case pop st of
	(Just top1, popedSt1) -> case pop popedSt1 of
		(Just top2, popedSt2) -> push popedSt2 (op top1 top2)
		(Nothing, _) -> error $ "Binary func arg2: data stack is empty"
	(Nothing, _) -> error $ "Binary func arg1: data stack is empty"

-- Usual Stack data structure
newtype Stack a = Stack { getList :: [a] } deriving (Show)

pop :: Stack a -> (Maybe a, Stack a)
pop st@(Stack ll) = if null ll then (Nothing, st) else (Just $ head ll, Stack $ tail ll)

push :: Stack a -> a -> Stack a
push (Stack ll) val = Stack (val : ll)

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False
