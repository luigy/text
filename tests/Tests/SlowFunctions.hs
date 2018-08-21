{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, UnliftedFFITypes, MagicHash #-}
module Tests.SlowFunctions
    (
      indices
    , splitOn
    ) where

import qualified Data.Text as T
import Data.Text.Internal (Text(..))
import Data.Text.Unsafe (iter_, unsafeHead, unsafeTail)
#ifdef __GHCJS__
import Data.JSString (JSString)
#endif

indices :: T.Text              -- ^ Substring to search for (@needle@)
        -> T.Text              -- ^ Text to search in (@haystack@)
        -> [Int]
#ifdef __GHCJS__
indices needle@(Text njst) haystack@(Text hjst)
#else
indices needle@(Text _narr _noff nlen) haystack@(Text harr hoff hlen)
#endif
    | T.null needle = []
    | otherwise     = scan 0
  where
    scan i | i >= hlen = []
           | needle `T.isPrefixOf` t = i : scan (i+nlen)
           | otherwise = scan (i+d)
#ifdef __GHCJS__
           where t = Text $ js_substr1 i hjst
                 hlen = js_length hjst
                 nlen = js_length njst
#else
           where t = Text harr (hoff+i) (hlen-i)
#endif
                 d = iter_ haystack i

splitOn :: T.Text               -- ^ Text to split on
        -> T.Text               -- ^ Input text
        -> [T.Text]
splitOn pat src0
    | T.null pat  = error "splitOn: empty"
    | l == 1      = T.split (== (unsafeHead pat)) src0
    | otherwise   = go src0
  where
    l      = T.length pat
    go src = search 0 src
      where
        search !n !s
            | T.null s             = [src]      -- not found
            | pat `T.isPrefixOf` s = T.take n src : go (T.drop l s)
            | otherwise            = search (n+1) (unsafeTail s)

#ifdef __GHCJS__
foreign import javascript unsafe
  "$2.substr($1)" js_substr1 :: Int -> JSString -> JSString
foreign import javascript unsafe
  "$1.length" js_length :: JSString -> Int
#endif
