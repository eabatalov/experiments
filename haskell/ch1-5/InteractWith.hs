import System.Environment (getArgs)
import Data.Foldable (foldl')
import Data.Char (digitToInt, isDigit)
import Data.List (groupBy)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile $ function input

-- myFunction = strsToFirstLineWords
myFunction = transpStr

main = mainWith myFunction
    where
    mainWith function = do
        args <- getArgs
        case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly 2 arguments needed"

isFoo :: a -> b -> a
a `isFoo` b = a

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = 
    case scd of
        [] -> suffSplitted
        _ -> scd : suffSplitted
    where 
    (scd, fldDirty) = break p xs
    suff = dropWhile p fldDirty
    suffSplitted = splitWith p suff

strsToFirstLineWords :: String -> String
strsToFirstLineWords str = 
    unlines $ map (head . words) $ lines str

transpStr :: String -> String
transpStr [] = []
transpStr (x1:x2:x3:x4:x5:x6:xs) = x6:x3:x2:x1:x5:x4:transpStr xs
transpStr (x1:x2:x3:x4:x5:[]) = x5:x2:x1:x3:x4:[]
transpStr (x1:x2:x3:x4:[]) = x4:x2:x1:x3:[]
transpStr (x1:x2:x3:[]) = x2:x1:x3:[]
transpStr xs@(x1:x2:[]) = xs
transpStr xs@(x:[]) = xs

-- foldr f acc (x1:x2:x3:[]) = f (foldl f acc xs) x1 =
-- f (f (foldl f acc xs) x2) x1 = f (f (f acc x3) x2) x1
-- foldl f acc (x1:x2:x3:[]) = foldl f (f acc x1) xs =
-- foldl f (f (f acc x1) x2) xs = f (f (f acc x1) x2) x3

asInt :: String -> Either String Int
asInt [] = Right 0
asInt lst@(x:xs) 
    | x == '-' = 
        case asInt xs of
            (Right val) -> Right $ -val
            (Left err) -> Left err
    | otherwise = foldl' step (Right 0) lst
        where 
        step :: Either String Int -> Char -> Either String Int
        step (Right acc) x 
            | isDigit x = Right $ 10 * acc + digitToInt x
            | otherwise = Left $ "Non-digit:" ++ [x]
        step (Left err) _ = Left err

concat :: [[a]] -> [a]
concat = foldr (\x acc -> x ++ acc) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) 
    | p x = x : takeWhile p xs
    | otherwise = []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' p = foldr (\x acc -> if not $ p x then [] else x:acc) []

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' cmp = foldl' helper []
    where 
--        helper :: [[a]] -> a -> [[a]]
        helper acc x 
            | null $ filter (cmp x) (map (\x -> head x) acc) = [x]:acc
            | otherwise = foldl'(\acc y -> if cmp x (head y) then ((x:y):acc) else y:acc) [] acc
        
