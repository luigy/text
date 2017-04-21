module Data.Text.Lazy.Builder.Int where

-- import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import qualified Data.JSString.Int as JSS
import Data.Text.Internal (Text (..))
import Data.Text.Lazy.Builder
import Data.Monoid

decimal :: Integral a => a -> Builder
decimal n = fromText $ Text $ JSS.decimal n
