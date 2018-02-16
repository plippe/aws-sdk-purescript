module Aws where

import Prelude
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.StrMap (StrMap)

newtype Metadata = Metadata (StrMap ServiceMetadata)

derive instance repGenericMetadata :: Generic Metadata _
instance decodeMetadata :: Decode Metadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

newtype ServiceMetadata = ServiceMetadata
  { name :: String
  , prefix :: NullOrUndefined String
  }

derive instance repGenericServiceMetadata :: Generic ServiceMetadata _
instance decodeServiceMetadata :: Decode ServiceMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
