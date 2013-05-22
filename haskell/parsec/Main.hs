module Main()
	where
import Text.ParserCombinators.Parsec
import Data.Char
import Data.List

data JValue = JInt Int |
	JString String |
	JArray [JValue] |
	JObject [(String, JValue)] deriving (Show)

parseJSON :: String -> Either ParseError JValue
parseJSON =	(parse jsonParser "no file") . skip
	where
		skip = filter (\ch -> not $ ch `elem` " \n\r" )

jsonParser :: GenParser Char () JValue
jsonParser =
	jsonObjectParser <|>
	jsonArrayParser <|>
	jsonStringParser <|>
	jsonIntParser <?>
	"Unknown JValue"

jsonIntParser :: GenParser Char () JValue
jsonIntParser =
	many1 (oneOf ['0'..'9']) >>= return . JInt . parseInt
	<?> "Couldn't parse JInt"
	where
		parseInt :: String -> Int
		parseInt = foldl' (\acc x -> 10 * acc + digitToInt x) 0

jsonStringParser :: GenParser Char () JValue
jsonStringParser = do
	char '\"'
	str <- many (noneOf ['\"'])
	char '\"'
	return (JString str)
	<?> "Couldn't parse JString"

jsonObjectParser :: GenParser Char () JValue
jsonObjectParser = do
	char '{'
	keyValue <- sepBy jsonKeyValueParser $ char ','
	char '}'
	return (JObject keyValue)
	<?> "Couldn't parse JObject"

jsonArrayParser :: GenParser Char () JValue
jsonArrayParser = do
	char '['
	arElems <- sepBy jsonParser (char ',')
	char ']'
	return (JArray arElems)
	<?> "Couldn't parse JArray"

jsonKeyValueParser :: GenParser Char () (String, JValue)
jsonKeyValueParser = do
	JString key <- jsonStringParser
	char ':'
	value <- jsonParser
	return (key, value)
	<?> "Couldn't parse json key-value pair"

{- TESTS -}
json1 = "\"hui\""
json2 = "123"
json3 = "[123, \"hui\", 345, \"foo\"]"
json4 = "{ \"a\" : 123, \"b\" : 345, \"c\" : \"foo\" }"
json5 = "\n { \"a\" :\n" ++ json4 ++ ", \"b\" : \n" ++ json3 ++ "}"

test = (show $ parseJSON json1)
	++ "\n" ++
	(show $ parseJSON json2)
	++ "\n" ++
	(show $ parseJSON json3)
	++ "\n" ++
	(show $ parseJSON json4)
	++ "\n" ++
	(show $ parseJSON json5)
