{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, UnliftedFFITypes #-}
#endif
-- |
-- Module      : Data.Text.Unsafe
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- A module containing unsafe 'Text' operations, for very very careful
-- use in heavily tested code.
module Data.Text.Unsafe
    (
      inlineInterleaveST
    , inlinePerformIO
    , unsafeDupablePerformIO
    , Iter(..)
    , iter
    , iter_
    , reverseIter
    , reverseIter_
    , unsafeHead
    , unsafeTail
    , lengthWord16
    , takeWord16
    , dropWord16
    ) where

#if defined(ASSERTS)
import Control.Exception (assert)
#endif
import Data.Text.Internal.Encoding.Utf16 (chr2)
import Data.Text.Internal (Text(..))
import Data.Text.Internal.Unsafe (inlineInterleaveST, inlinePerformIO)
#ifdef __GHCJS__
import Data.Coerce
import qualified Data.JSString.Raw as JSS
import GHC.Exts (Int(..), Int#, chr#, int2Word#, Char(..))
import GHC.Word (Word16(..))
import Data.JSString (JSString)
#else
import Data.Text.Internal.Unsafe.Char (unsafeChr)
import qualified Data.Text.Array as A
#endif
import GHC.IO (unsafeDupablePerformIO)

-- | /O(1)/ A variant of 'head' for non-empty 'Text'. 'unsafeHead'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeHead :: Text -> Char
#ifndef __GHCJS__
unsafeHead (Text arr off _len)
    | m < 0xD800 || m > 0xDBFF = unsafeChr m
    | otherwise                = chr2 m n
    where m = A.unsafeIndex arr off
          n = A.unsafeIndex arr (off+1)
#else
unsafeRawHead :: JSString -> Char
unsafeRawHead x = C# (chr# (js_charCodeAt 0 x))
{-# INLINE unsafeRawHead #-}

unsafeHead = coerce unsafeRawHead
#endif
{-# INLINE unsafeHead #-}

-- | /O(1)/ A variant of 'tail' for non-empty 'Text'. 'unsafeTail'
-- omits the check for the empty case, so there is an obligation on
-- the programmer to provide a proof that the 'Text' is non-empty.
unsafeTail :: Text -> Text
#ifndef __GHCJS__
unsafeTail t@(Text arr off len) =
#if defined(ASSERTS)
    assert (d <= len) $
#endif
    Text arr (off+d) (len-d)
  where d = iter_ t 0
#else
unsafeTail = coerce JSS.rawTail
#endif
{-# INLINE unsafeTail #-}

data Iter = Iter {-# UNPACK #-} !Char {-# UNPACK #-} !Int

-- | /O(1)/ Iterate (unsafely) one step forwards through a UTF-16
-- array, returning the current character and the delta to add to give
-- the next offset to iterate at.
iter :: Text -> Int -> Iter
#ifndef __GHCJS__
iter (Text arr off _len) i
    | m < 0xD800 || m > 0xDBFF = Iter (unsafeChr m) 1
    | otherwise                = Iter (chr2 m n) 2
  where m = A.unsafeIndex arr j
        n = A.unsafeIndex arr k
        j = off + i
        k = j + 1
#else
iter (Text txt) i
    | m < 0xD800 || m > 0xDBFF = Iter (C# (chr# m#)) 1
    | otherwise                = Iter (chr2 m n) 2
  where m# = js_charCodeAt j txt
        n# = js_charCodeAt k txt
        m = W16# (int2Word# m#)
        n = W16# (int2Word# n#)
        j = off + i
        k = j + 1
        off = 0
#endif
{-# INLINE iter #-}

-- | /O(1)/ Iterate one step through a UTF-16 array, returning the
-- delta to add to give the next offset to iterate at.
iter_ :: Text -> Int -> Int
#ifndef __GHCJS__
iter_ (Text arr off _len) i | m < 0xD800 || m > 0xDBFF = 1
                            | otherwise                = 2
  where m = A.unsafeIndex arr (off+i)
#else
iter_ (Text txt) i | m < 0xD800 || m > 0xDBFF = 1
                   | otherwise                = 2
  where m# = js_charCodeAt i txt
        m = W16# (int2Word# m#)
#endif
{-# INLINE iter_ #-}

-- | /O(1)/ Iterate one step backwards through a UTF-16 array,
-- returning the current character and the delta to add (i.e. a
-- negative number) to give the next offset to iterate at.
reverseIter :: Text -> Int -> (Char,Int)
#ifndef __GHCJS__
reverseIter (Text arr off _len) i
    | m < 0xDC00 || m > 0xDFFF = (unsafeChr m, -1)
    | otherwise                = (chr2 n m,    -2)
  where m = A.unsafeIndex arr j
        n = A.unsafeIndex arr k
        j = off + i
        k = j - 1
#else
reverseIter (Text txt) i
    | m < 0xDC00 || m > 0xDFFF = (C# (chr# m#), -1)
    | otherwise                = (chr2 n m,     -2)
  where m# = js_charCodeAt j txt
        n# = js_charCodeAt k txt
        m = W16# (int2Word# m#)
        n = W16# (int2Word# n#)
        j = off + i
        k = j - 1
        off = 0
#endif
{-# INLINE reverseIter #-}

-- | /O(1)/ Iterate one step backwards through a UTF-16 array,
-- returning the delta to add (i.e. a negative number) to give the
-- next offset to iterate at.
--
-- @since 1.1.1.0
reverseIter_ :: Text -> Int -> Int
#ifndef __GHCJS__
reverseIter_ (Text arr off _len) i
    | m < 0xDC00 || m > 0xDFFF = -1
    | otherwise                = -2
  where m = A.unsafeIndex arr (off+i)
#else
reverseIter_ (Text txt) i
    | m < 0xDC00 || m > 0xDFFF = -1
    | otherwise                = -2
  where m# = js_charCodeAt (off + i) txt
        m = W16# (int2Word# m#)
        off = 0
#endif
{-# INLINE reverseIter_ #-}

-- | /O(1)/ Return the length of a 'Text' in units of 'Word16'.  This
-- is useful for sizing a target array appropriately before using
-- 'unsafeCopyToPtr'.
lengthWord16 :: Text -> Int
#ifndef __GHCJS__
lengthWord16 (Text _arr _off len) = len
#else
lengthWord16 (Text t) = js_length t
#endif
{-# INLINE lengthWord16 #-}

-- | /O(1)/ Unchecked take of 'k' 'Word16's from the front of a 'Text'.
takeWord16 :: Int -> Text -> Text
#ifndef __GHCJS__
takeWord16 k (Text arr off _len) = Text arr off k
#else
takeWord16 k (Text txt) = Text $ js_take k txt
#endif
{-# INLINE takeWord16 #-}

-- | /O(1)/ Unchecked drop of 'k' 'Word16's from the front of a 'Text'.
dropWord16 :: Int -> Text -> Text
#ifndef __GHCJS__
dropWord16 k (Text arr off len) = Text arr (off+k) (len-k)
#else
dropWord16 k (Text txt) = Text $ js_drop k txt
#endif
{-# INLINE dropWord16 #-}

#ifdef __GHCJS__
foreign import javascript unsafe
  "$2.charCodeAt($1)" js_charCodeAt :: Int -> JSString -> Int#

foreign import javascript unsafe
  "h$jsstringTake" js_take :: Int -> JSString -> JSString

foreign import javascript unsafe
  "h$jsstringDrop" js_drop :: Int -> JSString -> JSString

foreign import javascript unsafe
  "$1.length" js_length :: JSString -> Int
#endif
