

-- | <p>Amazon Mobile Analytics is a service for collecting, visualizing, and understanding app usage data at scale.</p>
module AWS.MobileAnalytics where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "MobileAnalytics" :: String


-- | <p>The PutEvents operation records one or more events. You can have up to 1,500 unique custom events per app, any combination of up to 40 attributes and metrics per custom event, and any number of attribute or metric values.</p>
putEvents :: forall eff. PutEventsInput -> Aff (err :: AWS.RequestError | eff) Unit
putEvents = AWS.request serviceName "putEvents" 


-- | <p>An exception object returned when a request fails.</p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | <p>A JSON object representing a batch of unique event occurrences in your app.</p>
newtype Event = Event 
  { "EventType'" :: (String50Chars)
  , "Number" :: (ISO8601Timestamp)
  , "Session'" :: NullOrUndefined (Session)
  , "Version'" :: NullOrUndefined (String10Chars)
  , "Attributes'" :: NullOrUndefined (MapOfStringToString)
  , "Metrics'" :: NullOrUndefined (MapOfStringToNumber)
  }
derive instance newtypeEvent :: Newtype Event _


newtype EventListDefinition = EventListDefinition (Array Event)
derive instance newtypeEventListDefinition :: Newtype EventListDefinition _


newtype ISO8601Timestamp = ISO8601Timestamp String
derive instance newtypeISO8601Timestamp :: Newtype ISO8601Timestamp _


newtype MapOfStringToNumber = MapOfStringToNumber (Map String50Chars Number)
derive instance newtypeMapOfStringToNumber :: Newtype MapOfStringToNumber _


newtype MapOfStringToString = MapOfStringToString (Map String50Chars String0to1000Chars)
derive instance newtypeMapOfStringToString :: Newtype MapOfStringToString _


-- | <p>A container for the data needed for a PutEvent operation</p>
newtype PutEventsInput = PutEventsInput 
  { "Events'" :: (EventListDefinition)
  , "ClientContext'" :: (String)
  , "ClientContextEncoding'" :: NullOrUndefined (String)
  }
derive instance newtypePutEventsInput :: Newtype PutEventsInput _


-- | <p>Describes the session. Session information is required on ALL events.</p>
newtype Session = Session 
  { "Id'" :: NullOrUndefined (String50Chars)
  , "Duration'" :: NullOrUndefined (Number)
  , "StartTimestamp'" :: NullOrUndefined (ISO8601Timestamp)
  , "StopTimestamp'" :: NullOrUndefined (ISO8601Timestamp)
  }
derive instance newtypeSession :: Newtype Session _


newtype String0to1000Chars = String0to1000Chars String
derive instance newtypeString0to1000Chars :: Newtype String0to1000Chars _


newtype String10Chars = String10Chars String
derive instance newtypeString10Chars :: Newtype String10Chars _


newtype String50Chars = String50Chars String
derive instance newtypeString50Chars :: Newtype String50Chars _
