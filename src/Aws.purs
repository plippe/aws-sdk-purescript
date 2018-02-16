module Aws where

import Prelude
import Data.Either (Either)
import Data.Foreign (Foreign)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.NullOrUndefined (NullOrUndefined, unNullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.StrMap (StrMap)
import Data.String.Regex (Regex, regex)
import Data.String.Regex.Flags (ignoreCase)

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

serviceMetadataFileRegex :: ServiceMetadata -> Either String Regex
serviceMetadataFileRegex (ServiceMetadata serviceMetadata) = pattern where
  prefix = fromMaybe serviceMetadata.name $ unNullOrUndefined serviceMetadata.prefix
  pattern = regex (prefix <> "-[0-9]{4}-[0-9]{2}-[0-9]{2}.normal.json") ignoreCase

newtype Service = Service
  { operations :: Foreign
  , shapes :: Foreign
  }

derive instance repGenericService :: Generic Service _
instance decodeService :: Decode Service where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
