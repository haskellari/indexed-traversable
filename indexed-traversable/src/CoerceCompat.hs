{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module CoerceCompat where

#if __GLASGOW_HASKELL__ >=708
import Data.Coerce (Coercible, coerce)
#else
import Unsafe.Coerce (unsafeCoerce)
#endif


#if __GLASGOW_HASKELL__ >=708
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
_ #. x = coerce x

(#..) :: Coercible b c => (b -> c) -> (i -> a -> b) -> (i -> a -> c)
_ #.. x = coerce x
#else
(#.) :: (b -> c) -> (a -> b) -> (a -> c)
_ #. x = unsafeCoerce x

(#..) :: (b -> c) -> (i -> a -> b) -> (i -> a -> c)
_ #.. x = unsafeCoerce x
#endif
infixr 9 #., #..
{-# INLINE (#.) #-}
{-# INLINE (#..) #-}