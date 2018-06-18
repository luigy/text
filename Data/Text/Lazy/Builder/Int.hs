-- Module:      Data.Text.Lazy.Builder.Int
-- Copyright:   (c) 2013 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD-style
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Portability: portable
--
-- Efficiently write an integral value to a 'Builder'.

module Data.Text.Lazy.Builder.Int
    (
      decimal
    , hexadecimal
    ) where

-- import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import qualified Data.JSString.Int as JSS
import Data.Text.Internal (Text (..))
import Data.Text.Lazy.Builder

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Monoid (mempty)
import qualified Data.ByteString.Unsafe as B
import Data.Text.Internal.Builder.Functions ((<>), i2d)
import Data.Text.Internal.Builder
import Data.Text.Internal.Builder.Int.Digits (digits)
import Data.Text.Array
import Data.Word (Word, Word8, Word16, Word32, Word64)
import GHC.Base (quotInt, remInt)
import GHC.Num (quotRemInteger)
import GHC.Types (Int(..))
import Control.Monad.ST
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

#ifdef  __GLASGOW_HASKELL__
# if defined(INTEGER_GMP)
import GHC.Integer.GMP.Internals (Integer(S#))
# elif defined(INTEGER_SIMPLE)
import GHC.Integer
# else
# error "You need to use either GMP or integer-simple."
# endif
#endif

#if defined(INTEGER_GMP) || defined(INTEGER_SIMPLE)
# define PAIR(a,b) (# a,b #)
#else
# define PAIR(a,b) (a,b)
#endif

decimal :: Integral a => a -> Builder
decimal n = fromText $ Text $ JSS.decimal n
