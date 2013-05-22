-- file IO.hs
module IO(getRecursiveContents) where
import Data.List(intercalate)
import Control.Monad(forM, sequence_)
import System.Directory(doesDirectoryExist, getDirectoryContents)
import System.FilePath((</>))
import System.IO.Unsafe(unsafeInterleaveIO)

main = do
    putStrLn "Greetings! What is your name?"
    inpStr <- getLine
    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
    putStrLn "Type a dir to list all its contents recursively!"
    topdir <- getLine
    recContents <- getRecursiveContents topdir
    putStrLn "Here it is:"
    sequence_ (map (putStrLn . show) recContents)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topPath = do
    isDirectory <- doesDirectoryExist topPath
    if not isDirectory
        then 
            return [topPath]
        else do
            topContents <- getDirectoryContents topPath
            let properNames = filter (\p -> p /= "." && p /= "..") topContents
            let topContentsFullPath = map (\p -> topPath </> p) properNames
            contentsRec <- unsafeInterleaveIO $ mapM getRecursiveContents topContentsFullPath
            return $ (topPath:topContentsFullPath) ++ (concat contentsRec)

mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' f xs = do
    let list = (map f xs) -- :: [m b]
    accM list []
    where 
        accM (mx:mxs) acc = mx >>= (\x -> accM mxs (x:acc))
        accM [] acc = return acc
