{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Transformers
	where
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Control.Monad.Writer.Lazy
import MyWriter
import MyReader
import MyMaybe
import MyState
import Control.Monad.Identity

data AppConfig = AppConfig { getConf :: String } deriving (Show)
data AppState = AppState { st1 :: String, st2 :: String } deriving (Show)
newtype AppResult = AppResult { rst :: String } deriving (Monoid, Show)
initConf = AppConfig "1"
initState = AppState "2" "3" 

{- == MY MONAD TRANSFORMERS == -}
type MyApp a = MyReaderT AppConfig (MyWriterT AppResult (MyStateT AppState Identity)) a
executeMyApp :: AppConfig -> AppState -> MyApp a -> Identity (a, AppResult)
executeMyApp config initialState action = do
	let writerT = runMyReaderT action config
	let stateT = runMyWriterT writerT
	(aAndAppRes, st) <- runMyStateT stateT initialState
	return aAndAppRes
-- class deriving style
mkMyApp :: MyApp ()
mkMyApp = do
	(AppConfig conf) <- ask
	(AppState st1 st2) <- get
	tell $ AppResult ("Result: " ++ conf ++ st1 ++ st2)
-- lift style
mkMyApp1 :: MyApp ()
mkMyApp1 = do
	(AppConfig conf) <- ask
	(AppState st1 st2) <- lift $ lift get
	lift $ tell $ AppResult ("Result: " ++ conf ++ st1 ++ st2)
	return ()

execMyInit app = executeMyApp initConf initState app

{- == STANDARD MONAD TRANSFORMERS -}
newtype App a = App { runAp ::
		ReaderT AppConfig (WriterT AppResult (StateT AppState IO)) a
	} deriving (Monad, 
		MonadIO,
		MonadState AppState,
		MonadWriter AppResult,
		MonadReader AppConfig)

executeApp :: AppConfig -> AppState -> App a -> IO (a, AppResult)
executeApp config initialState action = 
	runStateT (runWriterT (runReaderT (runAp action) config)) initialState >>=
	return . fst

app1 :: App ()
app1 = do
	put $ AppState "BIG" "BOOBIES"
	st <- get
	tell $ AppResult $ show st
	newSt1 <- liftIO getLine
	modify $ \st -> AppState newSt1 (st2 st)
	st <- get
	tell $ AppResult $ show st
	ask >>= flip when (tell $ AppResult "Config is empty") . (null . getConf)


execInit app = executeApp initConf initState app
