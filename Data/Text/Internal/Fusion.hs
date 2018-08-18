{-# LANGUAGE BangPatterns, MagicHash #-}

-- |
-- Module      : Data.Text.Internal.Fusion
-- Copyright   : (c) Tom Harper 2008-2009,
--               (c) Bryan O'Sullivan 2009-2010,
--               (c) Duncan Coutts 2009
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Text manipulation functions represented as fusible operations over
-- streams.
module Data.Text.Internal.Fusion
    (
    -- * Types
      Stream(..)
    , Step(..)

    -- * Creation and elimination
    , stream
    , unstream
    , reverseStream

    , Data.Text.Internal.Fusion.length

    -- * Transformations
    , Data.Text.Internal.Fusion.reverse

    -- * Construction
    -- ** Scans
    , reverseScanr

    -- ** Accumulating maps
    , Data.Text.Internal.Fusion.mapAccumL

    -- ** Generation and unfolding
    , unfoldrN

    -- * Indexing
    , Data.Text.Internal.Fusion.index
    , Data.Text.Internal.Fusion.findIndex
    , countChar
    ) where

import           GHC.Exts (Char(..), Int(..), chr#, Int#, isTrue#, (-#), (+#), (>=#))

import           Prelude hiding (length, reverse)
import           Data.Char

import           Data.JSString.Internal.Type (JSString(..))
import qualified Data.JSString.Internal.Type          as I
import           Data.JSString.Internal.Fusion.Types
import qualified Data.JSString.Internal.Fusion.Common as S

import           System.IO.Unsafe

import           GHCJS.Prim

import Data.Coerce
import Data.Text.Internal
import qualified Data.JSString.Internal.Fusion as JSS
import Data.JSString.Internal.Fusion.Types (Stream(..))

default(Int)

stream :: Text -> Stream Char
stream = coerce JSS.stream
{-# INLINE [0] stream #-}

reverseStream :: Text -> Stream Char
reverseStream = coerce JSS.reverseStream
{-# INLINE [0] reverseStream #-}

unstream :: Stream Char -> Text
unstream = coerce JSS.unstream
{-# INLINE [0] unstream #-}
{-# RULES "STREAM stream/unstream fusion" forall s. stream (unstream s) = s #-}

length :: Stream Char -> Int
length = S.lengthI
{-# INLINE [0] length #-}

reverse :: Stream Char -> Text
reverse = coerce JSS.reverse
{-# INLINE [0] reverse #-}

reverseScanr :: (Char -> Char -> Char) -> Char -> Stream Char -> Stream Char
reverseScanr = JSS.reverseScanr
{-# INLINE reverseScanr #-}

mapAccumL :: (a -> Char -> (a, Char)) -> a -> Stream Char -> (a, Text)
mapAccumL f z0 st =
  let (a, t) = JSS.mapAccumL f z0 st
  in (a, Text t)
{-# INLINE [0] mapAccumL #-}

unfoldrN :: Int -> (a -> Maybe (Char,a)) -> a -> Stream Char
unfoldrN = JSS.unfoldrN
{-# INLINE [0] unfoldrN #-}

index :: Stream Char -> Int -> Char
index = JSS.index
{-# INLINE [0] index #-}

findIndex :: (Char -> Bool) -> Stream Char -> Maybe Int
findIndex = JSS.findIndex
{-# INLINE [0] findIndex #-}

countChar :: Char -> Stream Char -> Int
countChar = JSS.countChar
{-# INLINE [0] countChar #-}
