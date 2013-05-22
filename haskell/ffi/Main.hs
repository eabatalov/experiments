{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

main = print $ 
	"Here is c_sint(1.0):" ++ (show $ c_sin $ pi / 2)
	++ "\n" ++
	"Here is fastSin(1.0):" ++ (show $ fastSin $ pi / 2)

foreign import ccall unsafe "math.h sin"
	c_sin :: CDouble -> CDouble

fastSin :: Double -> Double
fastSin x = realToFrac $ c_sin $ realToFrac x
