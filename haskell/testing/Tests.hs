-- file Tests.hs
module Tests where
import Test.QuickCheck(quickCheck)
import Data.List

main = do
	quickCheck (prop_idempotent :: [Int] -> Bool)
	
qsort :: Ord a => [a] -> [a]
qsort (x:xs) = qsort lxs ++ [x] ++ qsort rxs
	where
		lxs = filter (< x) xs
		rxs = filter (>= x) xs
qsort [] = []

prop_idempotent xs = qsort (qsort xs) == qsort xs

app :: [a] -> [a] -> [a]
app (x:xs) ys = x : (app xs ys)
app [] ys = ys
