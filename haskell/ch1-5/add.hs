import Data.List
--file add.hs
add a b = a + b
lastButOne xs = lastButOneIntern (length xs) xs
lastButOneIntern n xs | n == 2 = head xs
	| n > 2 = lastButOneIntern (n - 1) (tail xs)

data BookInfo = Book Int String [String]
	deriving (Show)

id (Book id _ _) = id

data Customer = Customer {
	  customerID :: Int
	, customerName :: String
	, customerAddress :: String
	} deriving (Show)

-- customer1 :: Customer
customer1 = Customer {
	  customerID = 1234
	, customerAddress = "Foo"
	, customerName = "Bar"
	}
-- cutomerFunc1 :: Customer -> Customer
customerIncId customer = Customer {
		  customerID = customerID customer + 1
		, customerName = customerName customer
		, customerAddress = customerAddress customer
	}

data List a = Cons { 
	lHead :: a, lTail ::  (List a)
	} | Nil deriving (Show)

-- Exerises
-- myLength :: Num t2 => [t1] -> t2
myLengthImpl res (x:xs) = myLengthImpl (res + 1) xs
myLengthImpl res [] = res
myLength xs = myLengthImpl 0 xs
-- mean :: (Fractional a) => [a] -> a
mean xs = sum xs / myLength xs

reverse_ :: [a] -> [a] -> [a]
reverse_ (x:xs) revs = reverse_ xs (x:revs)
reverse_ [] revs = revs
palindrom :: [a] -> [a]
palindrom xs = xs ++ reverse_ xs []

listElementAt :: Int -> [a] -> a
listElementAt 0 (x:xs) = x
listElementAt i (x:xs) = listElementAt (i - 1) xs
listElementAt _ [] = error "Index out of bounds"

listLastElem :: [a] -> a
listLastElem xs = listElementAt (length xs - 1) xs

listWithoutLastElem :: [a] -> (a, [a])
listWithoutLastElem (x:[]) = (x, [])
listWithoutLastElem (x:xs) = (lastEl, x:list)
	where (lastEl, list) = listWithoutLastElem xs
listWithoutLastElem [] = (error "!", [])

isPalindrome :: Eq a => [a] -> Bool
isPalindrome (x:y:[]) | x == y = True
					| otherwise = False
isPalindrome (x:xs) = x == y && isPalindrome ys
	where (y, ys) = listWithoutLastElem xs
isPalindrome [] = False

interspers :: a -> [[a]] -> [a]
interspers s (x:[]) = x
interspers s (x:xs) = x ++ [s] ++ interspers s xs
interspers _ [] = []

list = [[1..10], [1..3], [1..5], [1..7]]
listCh = ["foo", "bar", "baz", "quux"]

data Tree a = Node {
	val :: a,
	left :: Tree a,
	right :: Tree a
	} | Null deriving (Show)

tree = (Node 0 
	(Node 1 
		(Node 3 Null Null) Null) 
	(Node 2 Null Null)
	)

height :: Tree a -> Int
height (Node val left right) = 
	1 + max (height left) (height right)
height Null = 0

type Point = (Double, Double)
data Direction = DLeft | DRight | DLine deriving (Show)

turn :: Point -> Point -> Point -> Direction
turn p1@(x1, y1) p2@(x2, y2) p3@(x3, y3) 
	| x1 == x2 && x3 == x1 = DLine
	| x1 == x2 && x3 < x1 = DLeft
	| x1 == x2 && x3 > x1 = DRight
	| y12 x3 == y3 = DLine
	| y12 x3 > y3 = DRight
	| y12 x3 < y3 = DLeft
	where y12 x = -(y1 - y2) / (x2 - x1) * x - (x1 * y2 - x2 * y1) / (x2 - x1)
	
p1 = (0.0, 0.0)
p2 = (10.0, 10.0)
p3 = (10.0, 9.0)
p4 = (10.0, 11.0)
p5 = (20.0, 20.0)

pList :: [Point]
pList = [p1, p2, p3, p4, p5, p3, p5, p1, p2, p1, p5]

turns :: [Point] -> [Direction]
turns (p1:p2:p3:ps) = (turn p1 p2 p3) : turns (p2:(p3:ps))
turns _ = []

