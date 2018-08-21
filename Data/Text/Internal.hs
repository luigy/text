{-# LANGUAGE CPP, DeriveDataTypeable, UnboxedTuples, MagicHash,
             BangPatterns, ForeignFunctionInterface, JavaScriptFFI, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes,
             GHCForeignImportPrim, MagicHash, FlexibleInstances #-}
module Data.Text.Internal
  ( module Data.Text.Internal
  , JSS.safe
  , JSS.firstf
  ) where

import Data.String (IsString(..))
import Data.Coerce
import Data.Data
import Data.Maybe
import Data.Binary
import Control.DeepSeq
import Data.Semigroup
import Data.Char
import qualified Data.JSString as JSS
import qualified Data.JSString.Internal as JSSS
import qualified Data.JSString.Internal.Type as JSS
import Data.JSString hiding (empty)
import qualified Data.Text.Array as A

import Data.Bits
import Data.Int (Int32, Int64)
-- import Data.Text.Internal.Unsafe.Char (ord)
import Data.Typeable (Typeable)
import GHC.Exts                       (ByteArray#, Char(..), ord#, andI#, (/=#), isTrue#, (==#), Int(..), Int#, isTrue#)
-- import qualified Data.Text.Array as A
import GHCJS.Marshal.Pure
import GHCJS.Marshal
import qualified GHC.CString                          as GHC


newtype Text = Text JSString
  deriving (PToJSVal, PFromJSVal, FromJSVal, ToJSVal)

instance Show Text where
  showsPrec p (Text jsstring) r = showsPrec p jsstring r

empty :: Text
empty = Text JSS.empty
{-# INLINE [1] empty #-}

empty_ :: Text
empty_ = Text JSS.empty
{-# NOINLINE empty_ #-}

foreign import javascript unsafe
  "String.fromCharCode.apply(null, $1['u8'])"
  js_decodeBytes :: ByteArray# -> JSString

foreign import javascript unsafe
  "h$textToString($1,$2,$3)"
  js_textToString :: ByteArray# -> Int# -> Int# -> JSString

-- | Construct a 'Text' without invisibly pinning its byte array in
-- memory if its length has dwindled to zero.
text :: A.Array -> Int -> Int -> Text
-- text arr off len | len == 0  = empty
--                  | otherwise = text_ arr off len
#if defined(ASSERTS)
text (A.Array ba _) (I# off#) (I# len#) | isTrue# (len# ==# 0#) = empty
                                      | otherwise = Text $ js_textToString ba off# len#
#else
text (A.Array ba) (I# off#) (I# len#) | isTrue# (len# ==# 0#) = empty
                                      | otherwise = Text $ js_textToString ba off# len#
#endif

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul :: Int -> Int -> Int
#if WORD_SIZE_IN_BITS == 64
mul a b = fromIntegral $ fromIntegral a `mul64` fromIntegral b
#else
mul a b = fromIntegral $ fromIntegral a `mul32` fromIntegral b
#endif
{-# INLINE mul #-}
infixl 7 `mul`

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul64 :: Int64 -> Int64 -> Int64
mul64 a b
  | a >= 0 && b >= 0 =  mul64_ a b
  | a >= 0           = -mul64_ a (-b)
  | b >= 0           = -mul64_ (-a) b
  | otherwise        =  mul64_ (-a) (-b)
{-# INLINE mul64 #-}
infixl 7 `mul64`

mul64_ :: Int64 -> Int64 -> Int64
mul64_ a b
  | ahi > 0 && bhi > 0 = error "overflow"
  | top > 0x7fffffff   = error "overflow"
  | total < 0          = error "overflow"
  | otherwise          = total
  where (# ahi, alo #) = (# a `shiftR` 32, a .&. 0xffffffff #)
        (# bhi, blo #) = (# b `shiftR` 32, b .&. 0xffffffff #)
        top            = ahi * blo + alo * bhi
        total          = (top `shiftL` 32) + alo * blo
{-# INLINE mul64_ #-}

-- | Checked multiplication.  Calls 'error' if the result would
-- overflow.
mul32 :: Int32 -> Int32 -> Int32
mul32 a b = case fromIntegral a * fromIntegral b of
              ab | ab < min32 || ab > max32 -> error "overflow"
                 | otherwise                -> fromIntegral ab
  where min32 = -0x80000000 :: Int64
        max32 =  0x7fffffff
{-# INLINE mul32 #-}
infixl 7 `mul32`

--------------------------------------
