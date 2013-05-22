-- Write a parser of url encoded key value pairs.
-- Each key can occure multyple times
import Control.Monad
import Data.List

inputX = "foo=12354fsdmfnsd,mnsdc&dasdas=fantstish&hui=1&ruki=2&foo=4&goo=6"
input1 = "foo=12354fsdmfnsd,mnsdc"
inputXUrlData = [UrlData ("foo", "12354fsdmfnsd,mnsdc"),
	UrlData ("dasdas", "fantastish"),
	UrlData ("hui", "1"),
	UrlData ("ruki", "2"),
	UrlData ("foo", "4"),
	UrlData ("goo", "6")]
input1UrlData = UrlData ("foo", "12354fsdmfnsd,mnsdc")

newtype UrlData = UrlData { getData :: (String, String) } deriving (Eq)

instance Read UrlData where
	--ReadS :: String -> [(a, String)]
	--readsPrec :: Int -> ReadS UrlData
	readsPrec p = \s ->
		case break isEquals s of
			(key, '=':str)
				| not $ null key ->
					let (val, s) = break isAmp str
					in return $ (UrlData (key, val), s)
				| otherwise -> error "failed key"
			_ -> error "failed key-value structure"
		where
			isAmp = ('&' ==)
			isEquals = ('=' ==)
	--readList :: ReadS [UrlData]
	readList = \str -> do
		(val, str) <- readsPrec 0 str
		case str of
			"" -> return ([val], str)
			('&':str) -> readList str >>= \(xs, str) -> return (val:xs, str)
			_ -> error "invalid input"

instance Show UrlData where
	-- type ShowS = String -> String
	--showsPrec :: Int -> a -> ShowS
	showsPrec _ (UrlData (key, value)) = \s -> key ++ "=" ++ value  ++ s
	--showList :: [a] -> ShowS
	showList lst = \s -> init $ foldr (\ud acc -> (shows ud) . ('&':) . acc) id lst s
