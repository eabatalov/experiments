{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
import Data.List
import Control.Monad.Error

class Table t where
	getTableName :: t -> String

type FieldSelector t = t -> String

data FieldOrConst t a where
	TableFieldInt :: t -> FieldSelector t -> FieldOrConst t Int
	TableFieldString :: t -> FieldSelector t -> FieldOrConst t String
	ConstantInt :: Int -> FieldOrConst t Int
	ConstantString :: String -> FieldOrConst t String

data Where t a where
	Where :: Where t a -> Where t a
	WhereFieldOrConst :: FieldOrConst t a -> Where t a
	Or :: Where t Bool -> Where t Bool -> Where t Bool
	And :: Where t Bool -> Where t Bool -> Where t Bool
	Not :: Where t Bool -> Where t Bool
	Eq :: (Eq a) => Where t a -> Where t a -> Where t Bool
	Ls :: (Ord a) => Where t a -> Where t a -> Where t Bool
	Gt :: (Ord a) => Where t a -> Where t a -> Where t Bool

data SQL t = SQLSelect {
	getSelectFields :: [FieldSelector t],
	getFrom :: t,
	getWhere :: Where t Bool
	}

{- === SYNTAX SHUGAR FOR QUERY HLINQ === -}
-- Autocast from numeric constant to constant int field
instance Num (Where t Int) where
	(+) = undefined
	(*) = undefined
	(-) = undefined
	negate = undefined
	abs = undefined
	signum = undefined
	fromInteger i = WhereFieldOrConst (ConstantInt $ fromIntegral i)
-- Necessary to implement for 
instance (Eq a) => Eq (Where t a) where
	(==) = undefined
instance Show (Where t a) where
	show = undefined
-- HLINQ operators

select :: (Table t) => [FieldSelector t] -> t -> Where t Bool -> SQL t
select = SQLSelect

from :: (Table a) => a -> a
from = id

string :: String -> Where t String
string = WhereFieldOrConst . ConstantString

{- === SIMPLE COMPILER === -}
compile :: (Table t) => SQL t -> String
compile (SQLSelect fls fm wh) = 
	"select (" ++ semicolonize (fieldsToSQLStrs fm fls) ++ ") " ++
	"from " ++ getTableName fm ++ " " ++
	"where (" ++ whereToSQLStr wh ++ ")"
	where
		semicolonize = foldr (\str acc -> str ++ (if acc /= "" then ", " else "") ++ acc) ""

whereToSQLStr :: Where t a -> String
whereToSQLStr (Where wh) = whereToSQLStr wh
whereToSQLStr (Not wh) = "(not " ++ whereToSQLStr wh ++ ")"
whereToSQLStr (WhereFieldOrConst fld) = fieldToSQLStr fld
whereToSQLStr (Or whL whR) = whereBinOp "||" whL whR
whereToSQLStr (And whL whR) = whereBinOp "&&" whL whR
whereToSQLStr (Eq flL flR) = whereBinOp "==" flL flR
whereToSQLStr (Ls flL flR) = whereBinOp "<" flL flR
whereToSQLStr (Gt flL flR) = whereBinOp ">" flL flR
whereBinOp opStr whL whR = "(" ++
	whereToSQLStr whL ++ " " ++ opStr ++ " " ++ whereToSQLStr whR
	++ ")"
	
fieldsToSQLStrs :: t -> [FieldSelector t] -> [String]
fieldsToSQLStrs from = foldr (\fs acc -> (fs from) : acc) []

fieldToSQLStr :: FieldOrConst t a -> String
fieldToSQLStr (ConstantString str) = "\"" ++ str ++ "\""
fieldToSQLStr (ConstantInt i) = show i
fieldToSQLStr (TableFieldInt t fs) = fs t
fieldToSQLStr (TableFieldString t fs) = fs t

{- === STRONGLY TYPED QUERY TEST === -}
data TableTestSchema = TableTestSchema {
	ttName_ :: String,
	ttfId_ :: String,
	ttfName_ :: String
	}
instance Table TableTestSchema where
	getTableName = ttName_
tableTest = TableTestSchema "TableTest" "id" "name"
ttfSomeConst = (\_ -> "\"Kotiunia up your ass!!!\"") :: TableTestSchema -> String
ttfId = WhereFieldOrConst $ TableFieldInt tableTest  ttfId_
ttfName = WhereFieldOrConst $ TableFieldString tableTest ttfName_

--failField :: Int -> String
data TableTestSchema2 = TableTestSchema2 {
	ttName_2 :: String,
	ttfId_2 :: String,
	ttfName_2 :: String
	}
instance Table TableTestSchema2 where
	getTableName = ttName_2
tableTest2 = TableTestSchema2 "TableTest2" "id" "name"
failField = WhereFieldOrConst $ TableFieldString 1 (\_ -> "Fail")
failName = WhereFieldOrConst $ TableFieldString tableTest2 ttfName_2

queryTest = (select [ttfId_, ttfSomeConst{-, ttfName_2-}])
	(from tableTest)
	(Where$ ((ttfId `Gt` 20) `Or` (ttfName `Eq` string "Moh")) {-`And` (failName `Eq` string "Fail")-})
