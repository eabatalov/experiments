import Interpreter
main = computation

computation :: IO ()
computation = print $ run funcs datas

funcs :: FunctionStack
funcs = mkFunctionStack [mkAdd, mkSub, mkPrint, mkDrop, mkPrint]

datas :: DataStack
datas = mkDataStack [Int 5, Int 5, Int 5, String "Wow!"]
