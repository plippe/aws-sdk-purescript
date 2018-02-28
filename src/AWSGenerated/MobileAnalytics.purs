

-- | <p>Amazon Mobile Analytics is a service for collecting, visualizing, and understanding app usage data at scale.</p>
module AWS.MobileAnalytics where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "MobileAnalytics" :: String


-- | <p>The PutEvents operation records one or more events. You can have up to 1,500 unique custom events per app, any combination of up to 40 attributes and metrics per custom event, and any number of attribute or metric values.</p>
putEvents :: forall eff. PutEventsInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putEvents = Request.request serviceName "putEvents" 


-- | <p>An exception object returned when a request fails.</p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A JSON object representing a batch of unique event occurrences in your app.</p>
newtype Event = Event 
  { "EventType'" :: (String50Chars)
  , "Number" :: (ISO8601Timestamp)
  , "Session'" :: NullOrUndefined.NullOrUndefined (Session)
  , "Version'" :: NullOrUndefined.NullOrUndefined (String10Chars)
  , "Attributes'" :: NullOrUndefined.NullOrUndefined (MapOfStringToString)
  , "Metrics'" :: NullOrUndefined.NullOrUndefined (MapOfStringToNumber)
  }
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance decodeEvent :: Decode Event where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvent :: Encode Event where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventListDefinition = EventListDefinition (Array Event)
derive instance newtypeEventListDefinition :: Newtype EventListDefinition _
derive instance repGenericEventListDefinition :: Generic EventListDefinition _
instance showEventListDefinition :: Show EventListDefinition where
  show = genericShow
instance decodeEventListDefinition :: Decode EventListDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventListDefinition :: Encode EventListDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ISO8601Timestamp = ISO8601Timestamp String
derive instance newtypeISO8601Timestamp :: Newtype ISO8601Timestamp _
derive instance repGenericISO8601Timestamp :: Generic ISO8601Timestamp _
instance showISO8601Timestamp :: Show ISO8601Timestamp where
  show = genericShow
instance decodeISO8601Timestamp :: Decode ISO8601Timestamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeISO8601Timestamp :: Encode ISO8601Timestamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfStringToNumber = MapOfStringToNumber (StrMap.StrMap Number)
derive instance newtypeMapOfStringToNumber :: Newtype MapOfStringToNumber _
derive instance repGenericMapOfStringToNumber :: Generic MapOfStringToNumber _
instance showMapOfStringToNumber :: Show MapOfStringToNumber where
  show = genericShow
instance decodeMapOfStringToNumber :: Decode MapOfStringToNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfStringToNumber :: Encode MapOfStringToNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapOfStringToString = MapOfStringToString (StrMap.StrMap String0to1000Chars)
derive instance newtypeMapOfStringToString :: Newtype MapOfStringToString _
derive instance repGenericMapOfStringToString :: Generic MapOfStringToString _
instance showMapOfStringToString :: Show MapOfStringToString where
  show = genericShow
instance decodeMapOfStringToString :: Decode MapOfStringToString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapOfStringToString :: Encode MapOfStringToString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A container for the data needed for a PutEvent operation</p>
newtype PutEventsInput = PutEventsInput 
  { "Events'" :: (EventListDefinition)
  , "ClientContext'" :: (String)
  , "ClientContextEncoding'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePutEventsInput :: Newtype PutEventsInput _
derive instance repGenericPutEventsInput :: Generic PutEventsInput _
instance showPutEventsInput :: Show PutEventsInput where
  show = genericShow
instance decodePutEventsInput :: Decode PutEventsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventsInput :: Encode PutEventsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the session. Session information is required on ALL events.</p>
newtype Session = Session 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String50Chars)
  , "Duration'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StartTimestamp'" :: NullOrUndefined.NullOrUndefined (ISO8601Timestamp)
  , "StopTimestamp'" :: NullOrUndefined.NullOrUndefined (ISO8601Timestamp)
  }
derive instance newtypeSession :: Newtype Session _
derive instance repGenericSession :: Generic Session _
instance showSession :: Show Session where
  show = genericShow
instance decodeSession :: Decode Session where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSession :: Encode Session where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype String0to1000Chars = String0to1000Chars String
derive instance newtypeString0to1000Chars :: Newtype String0to1000Chars _
derive instance repGenericString0to1000Chars :: Generic String0to1000Chars _
instance showString0to1000Chars :: Show String0to1000Chars where
  show = genericShow
instance decodeString0to1000Chars :: Decode String0to1000Chars where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeString0to1000Chars :: Encode String0to1000Chars where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype String10Chars = String10Chars String
derive instance newtypeString10Chars :: Newtype String10Chars _
derive instance repGenericString10Chars :: Generic String10Chars _
instance showString10Chars :: Show String10Chars where
  show = genericShow
instance decodeString10Chars :: Decode String10Chars where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeString10Chars :: Encode String10Chars where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype String50Chars = String50Chars String
derive instance newtypeString50Chars :: Newtype String50Chars _
derive instance repGenericString50Chars :: Generic String50Chars _
instance showString50Chars :: Show String50Chars where
  show = genericShow
instance decodeString50Chars :: Decode String50Chars where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeString50Chars :: Encode String50Chars where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
