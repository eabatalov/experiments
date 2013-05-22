module Graph
	where
import Data.List
import Data.Monoid
import Data.Ix
import Data.STRef
import Control.Monad
import Control.Monad.ST
import qualified Data.Array.ST as AM
import qualified Data.Array as A
import qualified Data.Array.IArray as AI
import qualified Data.Array.Unboxed as AI
import qualified Data.Map.Strict as Map

-- IMMUTABLE GRAPH
exGraph = mkGraph "Example"
	["1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "anton",
		"pavel", "mihail"]
	[("1", "2"),
	("2", "3"),
	("3", "4"),
	("4", "5"),
	("5", "6"),
	("6", "7"),
	("7", "8"),
	("8", "9"),
	("9", "10"),
	("10", "9"),
	("10", "1"),
	("1", "anton"),
	("1", "pavel"),
	("1", "mihail")]

data Graph vid = Graph {
	gName :: String,
	gVIdToIxMap :: Map.Map vid Int,
	gVIxToIdMap :: AI.Array Int vid,
	gAdjList :: A.Array Int (AI.UArray Int Int)
}

mkGraph :: (Ord vid) => String -> [vid] -> [(vid, vid)] -> Graph vid
mkGraph name vids edges = Graph {
		gName = name,
		gVIdToIxMap = vIdToIxMap,
		gVIxToIdMap = vIxToIdMap,
		gAdjList = adjList 
	} where
		vIdToIxList = ixizeList' vids
		vIdToIxMap = Map.fromList vIdToIxList
		vIxToIdMap = toIArray vids
		adjList = toIArray $ do
			(vId, vIx) <- vIdToIxList
			return $ toIArray $ do
				(v, w) <- edges
				guard $ v == vId
				return $ vIdToIxMap Map.! w

adjVertices :: (Ord vid) => Graph vid -> vid -> [vid]
adjVertices g vid = map (vIxToId g) (adjVertexIxs g vid)

adjVertexIxs :: (Ord vid) => Graph vid -> vid -> [Int]
adjVertexIxs g vid =
	let
		vix = vIdToIx g vid
		adjVertexIxs = (AI.elems $ gAdjList g AI.! vix)
	in adjVertexIxs

-- AUXILARY STUFF
instance (Show a) => Show (Graph a) where
	show g = "Graph " ++ gName g ++ ":\n" ++ vertices
		where vertices = do
			(vIx, v) <- AI.assocs $ gVIxToIdMap g
			let edges = do
				wIx <- AI.elems $ gAdjList g AI.! vIx
				let w = (gVIxToIdMap g) AI.! wIx
				showEdge v w ++ " "
			"Vertex " ++ (show v) ++ " " ++ edges ++ "\n"

toIArray :: (AI.IArray ar a) => [a] -> ar Int a
toIArray lst = AI.array (ixLow, ixHigh lst) $ ixizeList lst

lstShowIArray :: (Ix ix, Show a, AI.IArray ar a) => ar ix a -> String
lstShowIArray = show . AI.elems

showEdge v w = "(" ++ (show v) ++ " -> " ++ (show w) ++ ")"

ixLow = 0
ixHigh lst = (length lst - 1)
ixizeList lst = zip [ixLow..ixHigh lst] lst
ixizeList' = (map $ \(x, y) -> (y, x)) . ixizeList

vIdToIx :: (Ord vid) => Graph vid -> vid -> Int
vIdToIx g id = gVIdToIxMap g Map.! id
vIxToId :: Graph vid -> Int -> vid
vIxToId g ix = gVIxToIdMap g AI.! ix
