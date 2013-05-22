module GraphSearch
	where
import Graph
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.STRef
import qualified Data.Set as Set
import Data.List

class GraphSearchVertexList l where
	gsvlPut :: Int -> l -> l
	gsvlGet :: l -> (Maybe Int, l)
	gsvlIsEmpty :: l -> Bool

newtype VertexQueue = VertexQueue [Int]
enqueue :: Int -> VertexQueue -> VertexQueue
enqueue vIx (VertexQueue ixs) = VertexQueue $ vIx : ixs
dequeue :: VertexQueue -> (Maybe Int, VertexQueue)
dequeue (VertexQueue ixs) = helper [] ixs
	where
		helper acc (x:[]) = (Just x, VertexQueue acc)
		helper acc (x:xs) = helper (x:acc) xs
		helper _ [] = (Nothing, VertexQueue [])
vqIsEmpty (VertexQueue ixs) = null ixs

newtype VertexStack = VertexStack [Int]
push :: Int -> VertexStack -> VertexStack
push vIx (VertexStack ixs) = VertexStack $ vIx : ixs
pop :: VertexStack -> (Maybe Int, VertexStack)
pop (VertexStack []) = (Nothing, VertexStack [])
pop (VertexStack (ix:ixs)) = (Just ix, VertexStack ixs)
vsIsEmpty (VertexStack ixs) = null ixs

instance GraphSearchVertexList VertexQueue where
	gsvlPut = enqueue
	gsvlGet = dequeue
	gsvlIsEmpty = vqIsEmpty

instance GraphSearchVertexList VertexStack where
	gsvlPut = push
	gsvlGet = pop
	gsvlIsEmpty = vsIsEmpty

type GraphSearchCB vid acc = vid -> acc -> acc
data GraphSearchData vid acc vlist = GraphSearchData {
	gsdGraph :: Graph vid,
	gsdAcc :: acc,
	gsdVisitedSet :: Set.Set Int,
	gsdVertexList :: vlist
}

dfs :: (Ord vid) => Graph vid -> vid -> acc -> GraphSearchCB vid acc -> acc
dfs g start initAcc usrCallback = 
	let
		dfsData = GraphSearchData g initAcc visited gsvl
		startIx = vIdToIx g start
		gsvl = VertexStack [startIx]
		visited = Set.singleton startIx
	in gsHelper dfsData usrCallback

bfs :: (Ord vid) => Graph vid -> vid -> acc -> GraphSearchCB vid acc -> acc
bfs g start initAcc usrCallback = 
	let
		dfsData = GraphSearchData g initAcc visited gsvl
		startIx = vIdToIx g start
		gsvl = VertexQueue [startIx]
		visited = Set.singleton startIx
	in gsHelper dfsData usrCallback

gsHelper :: (Ord vid, GraphSearchVertexList vlist) =>
	GraphSearchData vid acc vlist -> GraphSearchCB vid acc -> acc
gsHelper gsd usrCB = fst $ runState gsGo gsd
	where
		gsGo = do
			gsd <- get
			if gsvlIsEmpty $ gsdVertexList gsd
				then return $ gsdAcc gsd
				else gsVisit >> gsGo
		gsVisit = modify $ \gsd ->
			let
				graph = gsdGraph gsd
				vlistOld = gsdVertexList gsd
				accOld = gsdAcc gsd
				visitedOld = gsdVisitedSet gsd
				--
				(Just vIx, vListNoV) = gsvlGet $ gsdVertexList gsd
				vId = vIxToId graph vIx
				accNew = usrCB vId accOld
				adjIxs = adjVertexIxs graph vId
				vxsToVisit = filterVisited visitedOld adjIxs
				visitedNew = foldr Set.insert visitedOld vxsToVisit
				vListNew = foldr gsvlPut vListNoV vxsToVisit
			in GraphSearchData {
				gsdGraph = graph,
				gsdAcc = accNew,
				gsdVisitedSet = visitedNew,
				gsdVertexList = vListNew
			}
		filterVisited :: Set.Set Int -> [Int] -> [Int]
		filterVisited visited vs = filter (flip Set.notMember visited) vs

bfsTest = bfs exGraph "1" [] (\v acc -> acc ++ [v])
dfsTest = dfs exGraph "1" [] (\v acc -> acc ++ [v])

-- OPTIMIZED GRAPH SEARCH WITH ST
data GraphSearchDataRefs s vid acc vlist = GraphSearchDataRefs {
	gsdGraphRef :: STRef s (Graph vid),
	gsdAccRef :: STRef s (acc),
	gsdVisitedSetRef :: STRef s (Set.Set Int),
	gsdVertexListRef :: STRef s (vlist)
}

dfsOpt :: (Ord vid) => Graph vid -> vid -> acc -> GraphSearchCB vid acc -> acc
dfsOpt g start initAcc usrCallback = 
	let
		dfsData = GraphSearchData g initAcc visited gsvl
		startIx = vIdToIx g start
		gsvl = VertexStack [startIx]
		visited = Set.singleton startIx
	in gsHelperOpt dfsData usrCallback

bfsOpt :: (Ord vid) => Graph vid -> vid -> acc -> GraphSearchCB vid acc -> acc
bfsOpt g start initAcc usrCallback = 
	let
		bfsData = GraphSearchData g initAcc visited gsvl
		startIx = vIdToIx g start
		gsvl = VertexQueue [startIx]
		visited = Set.singleton startIx
	in gsHelperOpt bfsData usrCallback

gsHelperOpt :: (GraphSearchVertexList vlist, Ord vid) =>
	GraphSearchData vid acc vlist -> GraphSearchCB vid acc -> acc
gsHelperOpt gsd usrCB = runST gsStart
	where
		gsStart = do
			initGraphRef <- newSTRef $ gsdGraph gsd
			initAccRef <- newSTRef $ gsdAcc gsd
			initVisitedSetRef <- newSTRef $ gsdVisitedSet gsd
			initVertexListRef <- newSTRef $ gsdVertexList gsd
			let gsd = GraphSearchDataRefs {
				gsdGraphRef = initGraphRef,
				gsdAccRef = initAccRef,
				gsdVisitedSetRef = initVisitedSetRef,
				gsdVertexListRef = initVertexListRef
			}
			gsGo gsd usrCB
		
		gsGo :: (GraphSearchVertexList vlist, Ord vid) =>
			GraphSearchDataRefs s vid acc vlist -> GraphSearchCB vid acc -> ST s acc
		gsGo gsd usrCB = do
			vertexList <- readSTRef $ gsdVertexListRef gsd
			if gsvlIsEmpty vertexList
				then readSTRef (gsdAccRef gsd) >>= return
				else gsVisit gsd usrCB >> gsGo gsd usrCB
		gsVisit ::(GraphSearchVertexList vlist, Ord vid) =>
			GraphSearchDataRefs s vid acc vlist -> GraphSearchCB vid acc -> ST s ()
		gsVisit gsd usrCB = do
			let graphRef = gsdGraphRef gsd
			let vertexListRef = gsdVertexListRef gsd
			let accRef = gsdAccRef gsd
			let visitedSetRef = gsdVisitedSetRef gsd
			graph <- readSTRef graphRef
			vlistOld <- readSTRef vertexListRef
			accOld <- readSTRef accRef
			let (Just vIx, vListNoV) = gsvlGet vlistOld
			let vId = vIxToId graph vIx
			visitedOld <- readSTRef visitedSetRef
			let accNew = usrCB vId accOld
			let adjIxs = adjVertexIxs graph vId
			let vxsToVisit = filterVisited visitedOld adjIxs
			let visitedNew = foldr Set.insert visitedOld vxsToVisit
			let vListNew = foldr gsvlPut vListNoV vxsToVisit
			writeSTRef accRef accNew
			writeSTRef visitedSetRef visitedNew
			writeSTRef vertexListRef vListNew
		filterVisited :: Set.Set Int -> [Int] -> [Int]
		filterVisited visited vs = filter (flip Set.notMember visited) vs

bfsOptTest = bfsOpt exGraph "1" [] (\v acc -> acc ++ [v])
dfsOptTest = dfsOpt exGraph "1" [] (\v acc -> acc ++ [v])

testAll = bfsTest == bfsOptTest && dfsTest == dfsOptTest

-- Small example to remember
foo :: a -> Int
foo a = runST $ do
	aRef <- newSTRef a
	bar aRef

bar :: STRef s a -> ST s Int
bar ref = do
	a <- readSTRef ref
	return 0
