{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, JavaScriptFFI,
    UnboxedTuples, DeriveDataTypeable, GHCForeignImportPrim,
    MagicHash, FlexibleInstances, BangPatterns, Rank2Types, CPP #-}

{- | Conversion between 'Data.Text.Text' and 'Data.JSString.JSString'

 -}

-- Moved from ghcjs-base with text-jsstring support.

module Data.JSString.Text
    ( textToJSString
    , textFromJSString
    , lazyTextToJSString
    , lazyTextFromJSString
    , textFromJSVal
    , lazyTextFromJSVal
    ) where

import GHCJS.Prim

import GHC.Exts (Any)

import Control.DeepSeq

import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL
import Data.JSString (JSString)

import GHCJS.Marshal.Pure
import Unsafe.Coerce

textToJSString :: T.Text -> JSString
textToJSString (T.Text txt) = txt
{-# INLINE textToJSString #-}

textFromJSString :: JSString -> T.Text
textFromJSString = T.Text
{-# INLINE textFromJSString #-}

lazyTextToJSString :: TL.Text -> JSString
lazyTextToJSString t = rnf t `seq` js_lazyTextToString (unsafeCoerce t)
{-# INLINE lazyTextToJSString #-}

lazyTextFromJSString :: JSString -> TL.Text
lazyTextFromJSString = TL.fromStrict . textFromJSString
{-# INLINE lazyTextFromJSString #-}

-- | returns the empty Text if not a string
textFromJSVal :: JSVal -> T.Text
textFromJSVal v = T.Text $ pFromJSVal v -- TODO check if val is a string?
{-# INLINE textFromJSVal #-}

-- | returns the empty Text if not a string
lazyTextFromJSVal :: JSVal -> TL.Text
lazyTextFromJSVal = TL.fromStrict . textFromJSVal
{-# INLINE lazyTextFromJSVal #-}

-- ----------------------------------------------------------------------------

foreign import javascript unsafe
  "h$lazyTextToString($1)"
  js_lazyTextToString :: Any -> JSString
