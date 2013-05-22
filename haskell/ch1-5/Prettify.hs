import showHex

data Doc = ToBeDefined deriving (Show)

string :: String -> Doc
string str = enclose '"' '"' . hcat . map oneChar

escapes :: [(Char, String)]
escapes :: zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
	where ch a b = (a, ['\\', b])

oneChar :: Char -> Doc
oneChar c = case lookup c escapes of
	Just r -> text r
	Nothing | mustEscape  -> hexEscape c
			| otherwise -> char c
	where 
	mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'
	hexEscape :: Int -> Doc

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat xs = undefined


