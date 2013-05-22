module SimpleJSON
	( JValue(..)
	, getString
	, getInt
	, getDouble
	, getBool
	, getObject
	, getArray
	, isNull)
	where
import Data.List (intercalate)

data JValue = JString String
	| JNumber Double
	| JBool Bool
	| JNull
	| JObject [(String, JValue)]
	| JArray [JValue]
	deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString str) = Just str
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _ = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject obj) = Just obj
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray ar) = Just ar
getArray _ = Nothing

isNull :: JValue -> Bool
isNull jv = jv == JNull

-- Serializers
serializeJValue :: JValue -> String
serializeJValue (JString s) = show s
serializeJValue (JNumber n) = show n
serializeJValue (JBool True) = "true"
serializeJValue (JBool False) = "false"
serializeJValue JNull = "null"

serializeJValue (JArray ar) = "[" ++ values ar ++ "]"
	where 
	values [] = ""
	values xs = intercalate ", " (map serializeJValue xs)
serializeJValue (JObject obj) = "{" ++ fields obj ++ "}"
	where
	fields [] = ""
	fields xs = intercalate ", " (map serializeField xs)
		where
			serializeField (f, jv) = show f ++ ": " ++ serializeJValue jv

printJValue :: JValue -> IO ()
printJValue = putStrLn . serializeJValue
