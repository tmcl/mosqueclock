{-# LANGUAGE ForeignFunctionInterface #-}

module Mathematics.Fdlibm (sin, asin, cos, acos, tan, atan, atan2, pi) where

-- is this fully legit? my testing indicates so.
import Foreign.C.Types
import Prelude (Double, pi, ($), (.))

foreign import ccall unsafe "sin_fdlibm"
  sin_fdlibm :: CDouble -> CDouble

foreign import ccall unsafe "asin_fdlibm"
  asin_fdlibm :: CDouble -> CDouble

foreign import ccall unsafe "cos_fdlibm"
  cos_fdlibm :: CDouble -> CDouble

foreign import ccall unsafe "acos_fdlibm"
  acos_fdlibm :: CDouble -> CDouble

foreign import ccall unsafe "tan_fdlibm"
  tan_fdlibm :: CDouble -> CDouble

foreign import ccall unsafe "atan_fdlibm"
  atan_fdlibm :: CDouble -> CDouble

foreign import ccall unsafe "atan2_fdlibm"
  atan2_fdlibm :: CDouble -> CDouble -> CDouble

{-# INLINE unCDouble #-}
unCDouble :: CDouble -> Double
unCDouble (CDouble d) = d

{-# INLINE wrapCDouble #-}
wrapCDouble :: (CDouble -> CDouble) -> Double -> Double
wrapCDouble f = unCDouble . f . CDouble

sin :: Double -> Double
sin = wrapCDouble sin_fdlibm

asin :: Double -> Double
asin = wrapCDouble asin_fdlibm

cos :: Double -> Double
cos = wrapCDouble cos_fdlibm

acos :: Double -> Double
acos = wrapCDouble acos_fdlibm

tan :: Double -> Double
tan = wrapCDouble tan_fdlibm

atan :: Double -> Double
atan = wrapCDouble atan_fdlibm

atan2 :: Double -> Double -> Double
atan2 x y = unCDouble $ atan2_fdlibm (CDouble x) (CDouble y)
