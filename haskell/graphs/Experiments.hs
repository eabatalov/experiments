module Experiments
	where
import Data.List
-- SOMETHING
infiniteZeroOne :: [Int]
infiniteZeroOne = x
	where
		x = 0:y
		y = 1:x

-- DOUBLY LINKED LIST
data DLList a = DLNode (DLList a) a (DLList a) | Tail (DLList a) | Null

instance (Show a) => Show (DLList a) where
	show (Null) = "Null"
	show (Tail prev) = "Null"
	show (DLNode l a r) = show a  ++ ", " ++ show r

mkDLList' :: DLList a -> [a] -> DLList a
mkDLList' prev [] = Tail prev
mkDLList' prev (x:xs) = me
	where
		me = DLNode l x r
		l = prev
		r = mkDLList' me xs

mkDLList :: [a] -> DLList a
mkDLList = mkDLList' Null

fwdAndBk :: DLList a -> [a]
fwdAndBk Null = []
fwdAndBk tail@(Tail prev) = bk tail
fwdAndBk (DLNode l a r) = a:fwdAndBk r

bk :: DLList a -> [a]
bk Null = []
bk (Tail prev) = bk prev
bk (DLNode l a r) = a : bk l

list = mkDLList [1..5]

-- IMMUTABLE GRAPH
data GNode a = GNode { label :: a, edges :: [GNode a] }
data Graph a = Graph { name :: String, vertices :: [GNode a] }

instance (Show a) => Show (GNode a) where
	show node = (show . label) node ++ ": " ++ strEdges
		where strEdges = "[" ++ 
			foldl' (\str node -> str ++ " " ++ (show . label) node) [] (edges node)
			++ "]\n"

instance (Show a) => Show (Graph a) where
	show g = (show . name) g ++ "\n" ++
		foldl' (\str node -> str ++ show node) [] (vertices g)

graph :: Graph String
graph = Graph "graph" [v1, v2, v3, v4, v5, v6, v7, v8]
	where
		v1 = GNode { label = "v1", edges = [v2, v3, v4]}
		v2 = GNode "v2" [v1]
		v3 = GNode "v3" [v4]
		v4 = GNode "v4" [v5]
		v5 = GNode "v5" [v6]
		v6 = GNode "v6" [v7]
		v7 = GNode "v7" [v8]
		v8 = GNode "v8" [v1]
