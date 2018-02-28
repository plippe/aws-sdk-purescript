

-- | <fullname>AWS Health</fullname> <p>The AWS Health API provides programmatic access to the AWS Health information that is presented in the <a href="https://phd.aws.amazon.com/phd/home#/">AWS Personal Health Dashboard</a>. You can get information about events that affect your AWS resources:</p> <ul> <li> <p> <a>DescribeEvents</a>: Summary information about events.</p> </li> <li> <p> <a>DescribeEventDetails</a>: Detailed information about one or more events.</p> </li> <li> <p> <a>DescribeAffectedEntities</a>: Information about AWS resources that are affected by one or more events.</p> </li> </ul> <p>In addition, these operations provide information about event types and summary counts of events or affected entities:</p> <ul> <li> <p> <a>DescribeEventTypes</a>: Information about the kinds of events that AWS Health tracks.</p> </li> <li> <p> <a>DescribeEventAggregates</a>: A count of the number of events that meet specified criteria.</p> </li> <li> <p> <a>DescribeEntityAggregates</a>: A count of the number of affected entities that meet specified criteria.</p> </li> </ul> <p>The Health API requires a Business or Enterprise support plan from <a href="http://aws.amazon.com/premiumsupport/">AWS Support</a>. Calling the Health API from an account that does not have a Business or Enterprise support plan causes a <code>SubscriptionRequiredException</code>. </p> <p>For authentication of requests, AWS Health uses the <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>See the <a href="http://docs.aws.amazon.com/health/latest/ug/what-is-aws-health.html">AWS Health User Guide</a> for information about how to use the API.</p> <p> <b>Service Endpoint</b> </p> <p>The HTTP endpoint for the AWS Health API is:</p> <ul> <li> <p>https://health.us-east-1.amazonaws.com </p> </li> </ul>
module AWS.Health where

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

serviceName = "Health" :: String


-- | <p>Returns a list of entities that have been affected by the specified events, based on the specified filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service. Events that have impact beyond that of the affected entities, or where the extent of impact is unknown, include at least one entity indicating this.</p> <p>At least one event ARN is required. Results are sorted by the <code>lastUpdatedTime</code> of the entity, starting with the most recent.</p>
describeAffectedEntities :: forall eff. DescribeAffectedEntitiesRequest -> Aff (exception :: EXCEPTION | eff) DescribeAffectedEntitiesResponse
describeAffectedEntities = Request.request serviceName "describeAffectedEntities" 


-- | <p>Returns the number of entities that are affected by each of the specified events. If no events are specified, the counts of all affected entities are returned.</p>
describeEntityAggregates :: forall eff. DescribeEntityAggregatesRequest -> Aff (exception :: EXCEPTION | eff) DescribeEntityAggregatesResponse
describeEntityAggregates = Request.request serviceName "describeEntityAggregates" 


-- | <p>Returns the number of events of each event type (issue, scheduled change, and account notification). If no filter is specified, the counts of all events in each category are returned.</p>
describeEventAggregates :: forall eff. DescribeEventAggregatesRequest -> Aff (exception :: EXCEPTION | eff) DescribeEventAggregatesResponse
describeEventAggregates = Request.request serviceName "describeEventAggregates" 


-- | <p>Returns detailed information about one or more specified events. Information includes standard event data (region, service, etc., as returned by <a>DescribeEvents</a>), a detailed event description, and possible additional metadata that depends upon the nature of the event. Affected entities are not included; to retrieve those, use the <a>DescribeAffectedEntities</a> operation.</p> <p>If a specified event cannot be retrieved, an error message is returned for that event.</p>
describeEventDetails :: forall eff. DescribeEventDetailsRequest -> Aff (exception :: EXCEPTION | eff) DescribeEventDetailsResponse
describeEventDetails = Request.request serviceName "describeEventDetails" 


-- | <p>Returns the event types that meet the specified filter criteria. If no filter criteria are specified, all event types are returned, in no particular order.</p>
describeEventTypes :: forall eff. DescribeEventTypesRequest -> Aff (exception :: EXCEPTION | eff) DescribeEventTypesResponse
describeEventTypes = Request.request serviceName "describeEventTypes" 


-- | <p>Returns information about events that meet the specified filter criteria. Events are returned in a summary form and do not include the detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the <a>DescribeEventDetails</a> and <a>DescribeAffectedEntities</a> operations.</p> <p>If no filter criteria are specified, all events are returned. Results are sorted by <code>lastModifiedTime</code>, starting with the most recent.</p>
describeEvents :: forall eff. DescribeEventsRequest -> Aff (exception :: EXCEPTION | eff) DescribeEventsResponse
describeEvents = Request.request serviceName "describeEvents" 


-- | <p>Information about an entity that is affected by a Health event.</p>
newtype AffectedEntity = AffectedEntity 
  { "EntityArn'" :: NullOrUndefined.NullOrUndefined (EntityArn')
  , "EventArn'" :: NullOrUndefined.NullOrUndefined (EventArn')
  , "EntityValue'" :: NullOrUndefined.NullOrUndefined (EntityValue')
  , "AwsAccountId'" :: NullOrUndefined.NullOrUndefined (AccountId')
  , "LastUpdatedTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StatusCode'" :: NullOrUndefined.NullOrUndefined (EntityStatusCode')
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagSet')
  }
derive instance newtypeAffectedEntity :: Newtype AffectedEntity _
derive instance repGenericAffectedEntity :: Generic AffectedEntity _
instance showAffectedEntity :: Show AffectedEntity where
  show = genericShow
instance decodeAffectedEntity :: Decode AffectedEntity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAffectedEntity :: Encode AffectedEntity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A range of dates and times that is used by the <a>EventFilter</a> and <a>EntityFilter</a> objects. If <code>from</code> is set and <code>to</code> is set: match items where the timestamp (<code>startTime</code>, <code>endTime</code>, or <code>lastUpdatedTime</code>) is between <code>from</code> and <code>to</code> inclusive. If <code>from</code> is set and <code>to</code> is not set: match items where the timestamp value is equal to or after <code>from</code>. If <code>from</code> is not set and <code>to</code> is set: match items where the timestamp value is equal to or before <code>to</code>.</p>
newtype DateTimeRange = DateTimeRange 
  { "From'" :: NullOrUndefined.NullOrUndefined (Number)
  , "To'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDateTimeRange :: Newtype DateTimeRange _
derive instance repGenericDateTimeRange :: Generic DateTimeRange _
instance showDateTimeRange :: Show DateTimeRange where
  show = genericShow
instance decodeDateTimeRange :: Decode DateTimeRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateTimeRange :: Encode DateTimeRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAffectedEntitiesRequest = DescribeAffectedEntitiesRequest 
  { "Filter'" :: (EntityFilter)
  , "Locale'" :: NullOrUndefined.NullOrUndefined (Locale')
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults')
  }
derive instance newtypeDescribeAffectedEntitiesRequest :: Newtype DescribeAffectedEntitiesRequest _
derive instance repGenericDescribeAffectedEntitiesRequest :: Generic DescribeAffectedEntitiesRequest _
instance showDescribeAffectedEntitiesRequest :: Show DescribeAffectedEntitiesRequest where
  show = genericShow
instance decodeDescribeAffectedEntitiesRequest :: Decode DescribeAffectedEntitiesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAffectedEntitiesRequest :: Encode DescribeAffectedEntitiesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAffectedEntitiesResponse = DescribeAffectedEntitiesResponse 
  { "Entities'" :: NullOrUndefined.NullOrUndefined (EntityList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeAffectedEntitiesResponse :: Newtype DescribeAffectedEntitiesResponse _
derive instance repGenericDescribeAffectedEntitiesResponse :: Generic DescribeAffectedEntitiesResponse _
instance showDescribeAffectedEntitiesResponse :: Show DescribeAffectedEntitiesResponse where
  show = genericShow
instance decodeDescribeAffectedEntitiesResponse :: Decode DescribeAffectedEntitiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAffectedEntitiesResponse :: Encode DescribeAffectedEntitiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEntityAggregatesRequest = DescribeEntityAggregatesRequest 
  { "EventArns'" :: NullOrUndefined.NullOrUndefined (EventArnsList)
  }
derive instance newtypeDescribeEntityAggregatesRequest :: Newtype DescribeEntityAggregatesRequest _
derive instance repGenericDescribeEntityAggregatesRequest :: Generic DescribeEntityAggregatesRequest _
instance showDescribeEntityAggregatesRequest :: Show DescribeEntityAggregatesRequest where
  show = genericShow
instance decodeDescribeEntityAggregatesRequest :: Decode DescribeEntityAggregatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEntityAggregatesRequest :: Encode DescribeEntityAggregatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEntityAggregatesResponse = DescribeEntityAggregatesResponse 
  { "EntityAggregates'" :: NullOrUndefined.NullOrUndefined (EntityAggregateList)
  }
derive instance newtypeDescribeEntityAggregatesResponse :: Newtype DescribeEntityAggregatesResponse _
derive instance repGenericDescribeEntityAggregatesResponse :: Generic DescribeEntityAggregatesResponse _
instance showDescribeEntityAggregatesResponse :: Show DescribeEntityAggregatesResponse where
  show = genericShow
instance decodeDescribeEntityAggregatesResponse :: Decode DescribeEntityAggregatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEntityAggregatesResponse :: Encode DescribeEntityAggregatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventAggregatesRequest = DescribeEventAggregatesRequest 
  { "Filter'" :: NullOrUndefined.NullOrUndefined (EventFilter)
  , "AggregateField'" :: (EventAggregateField')
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults')
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeEventAggregatesRequest :: Newtype DescribeEventAggregatesRequest _
derive instance repGenericDescribeEventAggregatesRequest :: Generic DescribeEventAggregatesRequest _
instance showDescribeEventAggregatesRequest :: Show DescribeEventAggregatesRequest where
  show = genericShow
instance decodeDescribeEventAggregatesRequest :: Decode DescribeEventAggregatesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventAggregatesRequest :: Encode DescribeEventAggregatesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventAggregatesResponse = DescribeEventAggregatesResponse 
  { "EventAggregates'" :: NullOrUndefined.NullOrUndefined (EventAggregateList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeEventAggregatesResponse :: Newtype DescribeEventAggregatesResponse _
derive instance repGenericDescribeEventAggregatesResponse :: Generic DescribeEventAggregatesResponse _
instance showDescribeEventAggregatesResponse :: Show DescribeEventAggregatesResponse where
  show = genericShow
instance decodeDescribeEventAggregatesResponse :: Decode DescribeEventAggregatesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventAggregatesResponse :: Encode DescribeEventAggregatesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventDetailsFailedSet = DescribeEventDetailsFailedSet (Array EventDetailsErrorItem)
derive instance newtypeDescribeEventDetailsFailedSet :: Newtype DescribeEventDetailsFailedSet _
derive instance repGenericDescribeEventDetailsFailedSet :: Generic DescribeEventDetailsFailedSet _
instance showDescribeEventDetailsFailedSet :: Show DescribeEventDetailsFailedSet where
  show = genericShow
instance decodeDescribeEventDetailsFailedSet :: Decode DescribeEventDetailsFailedSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventDetailsFailedSet :: Encode DescribeEventDetailsFailedSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventDetailsRequest = DescribeEventDetailsRequest 
  { "EventArns'" :: (EventArnList')
  , "Locale'" :: NullOrUndefined.NullOrUndefined (Locale')
  }
derive instance newtypeDescribeEventDetailsRequest :: Newtype DescribeEventDetailsRequest _
derive instance repGenericDescribeEventDetailsRequest :: Generic DescribeEventDetailsRequest _
instance showDescribeEventDetailsRequest :: Show DescribeEventDetailsRequest where
  show = genericShow
instance decodeDescribeEventDetailsRequest :: Decode DescribeEventDetailsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventDetailsRequest :: Encode DescribeEventDetailsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventDetailsResponse = DescribeEventDetailsResponse 
  { "SuccessfulSet'" :: NullOrUndefined.NullOrUndefined (DescribeEventDetailsSuccessfulSet)
  , "FailedSet'" :: NullOrUndefined.NullOrUndefined (DescribeEventDetailsFailedSet)
  }
derive instance newtypeDescribeEventDetailsResponse :: Newtype DescribeEventDetailsResponse _
derive instance repGenericDescribeEventDetailsResponse :: Generic DescribeEventDetailsResponse _
instance showDescribeEventDetailsResponse :: Show DescribeEventDetailsResponse where
  show = genericShow
instance decodeDescribeEventDetailsResponse :: Decode DescribeEventDetailsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventDetailsResponse :: Encode DescribeEventDetailsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventDetailsSuccessfulSet = DescribeEventDetailsSuccessfulSet (Array EventDetails)
derive instance newtypeDescribeEventDetailsSuccessfulSet :: Newtype DescribeEventDetailsSuccessfulSet _
derive instance repGenericDescribeEventDetailsSuccessfulSet :: Generic DescribeEventDetailsSuccessfulSet _
instance showDescribeEventDetailsSuccessfulSet :: Show DescribeEventDetailsSuccessfulSet where
  show = genericShow
instance decodeDescribeEventDetailsSuccessfulSet :: Decode DescribeEventDetailsSuccessfulSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventDetailsSuccessfulSet :: Encode DescribeEventDetailsSuccessfulSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventTypesRequest = DescribeEventTypesRequest 
  { "Filter'" :: NullOrUndefined.NullOrUndefined (EventTypeFilter)
  , "Locale'" :: NullOrUndefined.NullOrUndefined (Locale')
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults')
  }
derive instance newtypeDescribeEventTypesRequest :: Newtype DescribeEventTypesRequest _
derive instance repGenericDescribeEventTypesRequest :: Generic DescribeEventTypesRequest _
instance showDescribeEventTypesRequest :: Show DescribeEventTypesRequest where
  show = genericShow
instance decodeDescribeEventTypesRequest :: Decode DescribeEventTypesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventTypesRequest :: Encode DescribeEventTypesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventTypesResponse = DescribeEventTypesResponse 
  { "EventTypes'" :: NullOrUndefined.NullOrUndefined (EventTypeList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeEventTypesResponse :: Newtype DescribeEventTypesResponse _
derive instance repGenericDescribeEventTypesResponse :: Generic DescribeEventTypesResponse _
instance showDescribeEventTypesResponse :: Show DescribeEventTypesResponse where
  show = genericShow
instance decodeDescribeEventTypesResponse :: Decode DescribeEventTypesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventTypesResponse :: Encode DescribeEventTypesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventsRequest = DescribeEventsRequest 
  { "Filter'" :: NullOrUndefined.NullOrUndefined (EventFilter)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults')
  , "Locale'" :: NullOrUndefined.NullOrUndefined (Locale')
  }
derive instance newtypeDescribeEventsRequest :: Newtype DescribeEventsRequest _
derive instance repGenericDescribeEventsRequest :: Generic DescribeEventsRequest _
instance showDescribeEventsRequest :: Show DescribeEventsRequest where
  show = genericShow
instance decodeDescribeEventsRequest :: Decode DescribeEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventsRequest :: Encode DescribeEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventsResponse = DescribeEventsResponse 
  { "Events'" :: NullOrUndefined.NullOrUndefined (EventList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeEventsResponse :: Newtype DescribeEventsResponse _
derive instance repGenericDescribeEventsResponse :: Generic DescribeEventsResponse _
instance showDescribeEventsResponse :: Show DescribeEventsResponse where
  show = genericShow
instance decodeDescribeEventsResponse :: Decode DescribeEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventsResponse :: Encode DescribeEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of entities that are affected by one or more events. Returned by the <a>DescribeEntityAggregates</a> operation.</p>
newtype EntityAggregate = EntityAggregate 
  { "EventArn'" :: NullOrUndefined.NullOrUndefined (EventArn')
  , "Count'" :: NullOrUndefined.NullOrUndefined (Count')
  }
derive instance newtypeEntityAggregate :: Newtype EntityAggregate _
derive instance repGenericEntityAggregate :: Generic EntityAggregate _
instance showEntityAggregate :: Show EntityAggregate where
  show = genericShow
instance decodeEntityAggregate :: Decode EntityAggregate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityAggregate :: Encode EntityAggregate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityAggregateList = EntityAggregateList (Array EntityAggregate)
derive instance newtypeEntityAggregateList :: Newtype EntityAggregateList _
derive instance repGenericEntityAggregateList :: Generic EntityAggregateList _
instance showEntityAggregateList :: Show EntityAggregateList where
  show = genericShow
instance decodeEntityAggregateList :: Decode EntityAggregateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityAggregateList :: Encode EntityAggregateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The values to use to filter results from the <a>DescribeAffectedEntities</a> operation.</p>
newtype EntityFilter = EntityFilter 
  { "EventArns'" :: (EventArnList')
  , "EntityArns'" :: NullOrUndefined.NullOrUndefined (EntityArnList')
  , "EntityValues'" :: NullOrUndefined.NullOrUndefined (EntityValueList')
  , "LastUpdatedTimes'" :: NullOrUndefined.NullOrUndefined (DateTimeRangeList')
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagFilter')
  , "StatusCodes'" :: NullOrUndefined.NullOrUndefined (EntityStatusCodeList')
  }
derive instance newtypeEntityFilter :: Newtype EntityFilter _
derive instance repGenericEntityFilter :: Generic EntityFilter _
instance showEntityFilter :: Show EntityFilter where
  show = genericShow
instance decodeEntityFilter :: Decode EntityFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityFilter :: Encode EntityFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityList = EntityList (Array AffectedEntity)
derive instance newtypeEntityList :: Newtype EntityList _
derive instance repGenericEntityList :: Generic EntityList _
instance showEntityList :: Show EntityList where
  show = genericShow
instance decodeEntityList :: Decode EntityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityList :: Encode EntityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Summary information about an event, returned by the <a>DescribeEvents</a> operation. The <a>DescribeEventDetails</a> operation also returns this information, as well as the <a>EventDescription</a> and additional event metadata.</p>
newtype Event = Event 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (EventArn')
  , "Service'" :: NullOrUndefined.NullOrUndefined (Service')
  , "EventTypeCode'" :: NullOrUndefined.NullOrUndefined (EventTypeCode')
  , "EventTypeCategory'" :: NullOrUndefined.NullOrUndefined (EventTypeCategory')
  , "Region'" :: NullOrUndefined.NullOrUndefined (Region')
  , "AvailabilityZone'" :: NullOrUndefined.NullOrUndefined (AvailabilityZone')
  , "StartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdatedTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StatusCode'" :: NullOrUndefined.NullOrUndefined (EventStatusCode')
  }
derive instance newtypeEvent :: Newtype Event _
derive instance repGenericEvent :: Generic Event _
instance showEvent :: Show Event where
  show = genericShow
instance decodeEvent :: Decode Event where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvent :: Encode Event where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of events of each issue type. Returned by the <a>DescribeEventAggregates</a> operation.</p>
newtype EventAggregate = EventAggregate 
  { "AggregateValue'" :: NullOrUndefined.NullOrUndefined (AggregateValue')
  , "Count'" :: NullOrUndefined.NullOrUndefined (Count')
  }
derive instance newtypeEventAggregate :: Newtype EventAggregate _
derive instance repGenericEventAggregate :: Generic EventAggregate _
instance showEventAggregate :: Show EventAggregate where
  show = genericShow
instance decodeEventAggregate :: Decode EventAggregate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventAggregate :: Encode EventAggregate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventAggregateList = EventAggregateList (Array EventAggregate)
derive instance newtypeEventAggregateList :: Newtype EventAggregateList _
derive instance repGenericEventAggregateList :: Generic EventAggregateList _
instance showEventAggregateList :: Show EventAggregateList where
  show = genericShow
instance decodeEventAggregateList :: Decode EventAggregateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventAggregateList :: Encode EventAggregateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventArnsList = EventArnsList (Array EventArn')
derive instance newtypeEventArnsList :: Newtype EventArnsList _
derive instance repGenericEventArnsList :: Generic EventArnsList _
instance showEventArnsList :: Show EventArnsList where
  show = genericShow
instance decodeEventArnsList :: Decode EventArnsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventArnsList :: Encode EventArnsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The detailed description of the event. Included in the information returned by the <a>DescribeEventDetails</a> operation.</p>
newtype EventDescription = EventDescription 
  { "LatestDescription'" :: NullOrUndefined.NullOrUndefined (EventDescription')
  }
derive instance newtypeEventDescription :: Newtype EventDescription _
derive instance repGenericEventDescription :: Generic EventDescription _
instance showEventDescription :: Show EventDescription where
  show = genericShow
instance decodeEventDescription :: Decode EventDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDescription :: Encode EventDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed information about an event. A combination of an <a>Event</a> object, an <a>EventDescription</a> object, and additional metadata about the event. Returned by the <a>DescribeEventDetails</a> operation.</p>
newtype EventDetails = EventDetails 
  { "Event'" :: NullOrUndefined.NullOrUndefined (Event)
  , "EventDescription'" :: NullOrUndefined.NullOrUndefined (EventDescription)
  , "EventMetadata'" :: NullOrUndefined.NullOrUndefined (EventMetadata')
  }
derive instance newtypeEventDetails :: Newtype EventDetails _
derive instance repGenericEventDetails :: Generic EventDetails _
instance showEventDetails :: Show EventDetails where
  show = genericShow
instance decodeEventDetails :: Decode EventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDetails :: Encode EventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Error information returned when a <a>DescribeEventDetails</a> operation cannot find a specified event.</p>
newtype EventDetailsErrorItem = EventDetailsErrorItem 
  { "EventArn'" :: NullOrUndefined.NullOrUndefined (EventArn')
  , "ErrorName'" :: NullOrUndefined.NullOrUndefined (String)
  , "ErrorMessage'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEventDetailsErrorItem :: Newtype EventDetailsErrorItem _
derive instance repGenericEventDetailsErrorItem :: Generic EventDetailsErrorItem _
instance showEventDetailsErrorItem :: Show EventDetailsErrorItem where
  show = genericShow
instance decodeEventDetailsErrorItem :: Decode EventDetailsErrorItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDetailsErrorItem :: Encode EventDetailsErrorItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The values to use to filter results from the <a>DescribeEvents</a> and <a>DescribeEventAggregates</a> operations.</p>
newtype EventFilter = EventFilter 
  { "EventArns'" :: NullOrUndefined.NullOrUndefined (EventArnList')
  , "EventTypeCodes'" :: NullOrUndefined.NullOrUndefined (EventTypeList')
  , "Services'" :: NullOrUndefined.NullOrUndefined (ServiceList')
  , "Regions'" :: NullOrUndefined.NullOrUndefined (RegionList')
  , "AvailabilityZones'" :: NullOrUndefined.NullOrUndefined (AvailabilityZones')
  , "StartTimes'" :: NullOrUndefined.NullOrUndefined (DateTimeRangeList')
  , "EndTimes'" :: NullOrUndefined.NullOrUndefined (DateTimeRangeList')
  , "LastUpdatedTimes'" :: NullOrUndefined.NullOrUndefined (DateTimeRangeList')
  , "EntityArns'" :: NullOrUndefined.NullOrUndefined (EntityArnList')
  , "EntityValues'" :: NullOrUndefined.NullOrUndefined (EntityValueList')
  , "EventTypeCategories'" :: NullOrUndefined.NullOrUndefined (EventTypeCategoryList')
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagFilter')
  , "EventStatusCodes'" :: NullOrUndefined.NullOrUndefined (EventStatusCodeList')
  }
derive instance newtypeEventFilter :: Newtype EventFilter _
derive instance repGenericEventFilter :: Generic EventFilter _
instance showEventFilter :: Show EventFilter where
  show = genericShow
instance decodeEventFilter :: Decode EventFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventFilter :: Encode EventFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _
derive instance repGenericEventList :: Generic EventList _
instance showEventList :: Show EventList where
  show = genericShow
instance decodeEventList :: Decode EventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventList :: Encode EventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, <code>issue</code>), the service (for example, <code>EC2</code>), and the event type code (for example, <code>AWS_EC2_SYSTEM_MAINTENANCE_EVENT</code>).</p>
newtype EventType = EventType 
  { "Service'" :: NullOrUndefined.NullOrUndefined (Service')
  , "Code'" :: NullOrUndefined.NullOrUndefined (EventTypeCode')
  , "Category'" :: NullOrUndefined.NullOrUndefined (EventTypeCategory')
  }
derive instance newtypeEventType :: Newtype EventType _
derive instance repGenericEventType :: Generic EventType _
instance showEventType :: Show EventType where
  show = genericShow
instance decodeEventType :: Decode EventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventType :: Encode EventType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTypeCategoryList = EventTypeCategoryList (Array EventTypeCategory')
derive instance newtypeEventTypeCategoryList :: Newtype EventTypeCategoryList _
derive instance repGenericEventTypeCategoryList :: Generic EventTypeCategoryList _
instance showEventTypeCategoryList :: Show EventTypeCategoryList where
  show = genericShow
instance decodeEventTypeCategoryList :: Decode EventTypeCategoryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypeCategoryList :: Encode EventTypeCategoryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTypeCodeList = EventTypeCodeList (Array EventTypeCode')
derive instance newtypeEventTypeCodeList :: Newtype EventTypeCodeList _
derive instance repGenericEventTypeCodeList :: Generic EventTypeCodeList _
instance showEventTypeCodeList :: Show EventTypeCodeList where
  show = genericShow
instance decodeEventTypeCodeList :: Decode EventTypeCodeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypeCodeList :: Encode EventTypeCodeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The values to use to filter results from the <a>DescribeEventTypes</a> operation.</p>
newtype EventTypeFilter = EventTypeFilter 
  { "EventTypeCodes'" :: NullOrUndefined.NullOrUndefined (EventTypeCodeList)
  , "Services'" :: NullOrUndefined.NullOrUndefined (ServiceList')
  , "EventTypeCategories'" :: NullOrUndefined.NullOrUndefined (EventTypeCategoryList)
  }
derive instance newtypeEventTypeFilter :: Newtype EventTypeFilter _
derive instance repGenericEventTypeFilter :: Generic EventTypeFilter _
instance showEventTypeFilter :: Show EventTypeFilter where
  show = genericShow
instance decodeEventTypeFilter :: Decode EventTypeFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypeFilter :: Encode EventTypeFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTypeList = EventTypeList (Array EventType)
derive instance newtypeEventTypeList :: Newtype EventTypeList _
derive instance repGenericEventTypeList :: Generic EventTypeList _
instance showEventTypeList :: Show EventTypeList where
  show = genericShow
instance decodeEventTypeList :: Decode EventTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypeList :: Encode EventTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified pagination token (<code>nextToken</code>) is not valid.</p>
newtype InvalidPaginationToken = InvalidPaginationToken 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeInvalidPaginationToken :: Newtype InvalidPaginationToken _
derive instance repGenericInvalidPaginationToken :: Generic InvalidPaginationToken _
instance showInvalidPaginationToken :: Show InvalidPaginationToken where
  show = genericShow
instance decodeInvalidPaginationToken :: Decode InvalidPaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPaginationToken :: Encode InvalidPaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified locale is not supported.</p>
newtype UnsupportedLocale = UnsupportedLocale 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnsupportedLocale :: Newtype UnsupportedLocale _
derive instance repGenericUnsupportedLocale :: Generic UnsupportedLocale _
instance showUnsupportedLocale :: Show UnsupportedLocale where
  show = genericShow
instance decodeUnsupportedLocale :: Decode UnsupportedLocale where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedLocale :: Encode UnsupportedLocale where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountId' = AccountId' String
derive instance newtypeAccountId' :: Newtype AccountId' _
derive instance repGenericAccountId' :: Generic AccountId' _
instance showAccountId' :: Show AccountId' where
  show = genericShow
instance decodeAccountId' :: Decode AccountId' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId' :: Encode AccountId' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AggregateValue' = AggregateValue' String
derive instance newtypeAggregateValue' :: Newtype AggregateValue' _
derive instance repGenericAggregateValue' :: Generic AggregateValue' _
instance showAggregateValue' :: Show AggregateValue' where
  show = genericShow
instance decodeAggregateValue' :: Decode AggregateValue' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAggregateValue' :: Encode AggregateValue' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZone' = AvailabilityZone' String
derive instance newtypeAvailabilityZone' :: Newtype AvailabilityZone' _
derive instance repGenericAvailabilityZone' :: Generic AvailabilityZone' _
instance showAvailabilityZone' :: Show AvailabilityZone' where
  show = genericShow
instance decodeAvailabilityZone' :: Decode AvailabilityZone' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZone' :: Encode AvailabilityZone' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZones' = AvailabilityZones' (Array AvailabilityZone')
derive instance newtypeAvailabilityZones' :: Newtype AvailabilityZones' _
derive instance repGenericAvailabilityZones' :: Generic AvailabilityZones' _
instance showAvailabilityZones' :: Show AvailabilityZones' where
  show = genericShow
instance decodeAvailabilityZones' :: Decode AvailabilityZones' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZones' :: Encode AvailabilityZones' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Count' = Count' Int
derive instance newtypeCount' :: Newtype Count' _
derive instance repGenericCount' :: Generic Count' _
instance showCount' :: Show Count' where
  show = genericShow
instance decodeCount' :: Decode Count' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCount' :: Encode Count' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DateTimeRangeList' = DateTimeRangeList' (Array DateTimeRange)
derive instance newtypeDateTimeRangeList' :: Newtype DateTimeRangeList' _
derive instance repGenericDateTimeRangeList' :: Generic DateTimeRangeList' _
instance showDateTimeRangeList' :: Show DateTimeRangeList' where
  show = genericShow
instance decodeDateTimeRangeList' :: Decode DateTimeRangeList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateTimeRangeList' :: Encode DateTimeRangeList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityArn' = EntityArn' String
derive instance newtypeEntityArn' :: Newtype EntityArn' _
derive instance repGenericEntityArn' :: Generic EntityArn' _
instance showEntityArn' :: Show EntityArn' where
  show = genericShow
instance decodeEntityArn' :: Decode EntityArn' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityArn' :: Encode EntityArn' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityArnList' = EntityArnList' (Array EntityArn')
derive instance newtypeEntityArnList' :: Newtype EntityArnList' _
derive instance repGenericEntityArnList' :: Generic EntityArnList' _
instance showEntityArnList' :: Show EntityArnList' where
  show = genericShow
instance decodeEntityArnList' :: Decode EntityArnList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityArnList' :: Encode EntityArnList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityStatusCode' = EntityStatusCode' String
derive instance newtypeEntityStatusCode' :: Newtype EntityStatusCode' _
derive instance repGenericEntityStatusCode' :: Generic EntityStatusCode' _
instance showEntityStatusCode' :: Show EntityStatusCode' where
  show = genericShow
instance decodeEntityStatusCode' :: Decode EntityStatusCode' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityStatusCode' :: Encode EntityStatusCode' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityStatusCodeList' = EntityStatusCodeList' (Array EntityStatusCode')
derive instance newtypeEntityStatusCodeList' :: Newtype EntityStatusCodeList' _
derive instance repGenericEntityStatusCodeList' :: Generic EntityStatusCodeList' _
instance showEntityStatusCodeList' :: Show EntityStatusCodeList' where
  show = genericShow
instance decodeEntityStatusCodeList' :: Decode EntityStatusCodeList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityStatusCodeList' :: Encode EntityStatusCodeList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityValue' = EntityValue' String
derive instance newtypeEntityValue' :: Newtype EntityValue' _
derive instance repGenericEntityValue' :: Generic EntityValue' _
instance showEntityValue' :: Show EntityValue' where
  show = genericShow
instance decodeEntityValue' :: Decode EntityValue' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityValue' :: Encode EntityValue' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityValueList' = EntityValueList' (Array EntityValue')
derive instance newtypeEntityValueList' :: Newtype EntityValueList' _
derive instance repGenericEntityValueList' :: Generic EntityValueList' _
instance showEntityValueList' :: Show EntityValueList' where
  show = genericShow
instance decodeEntityValueList' :: Decode EntityValueList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityValueList' :: Encode EntityValueList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventAggregateField' = EventAggregateField' String
derive instance newtypeEventAggregateField' :: Newtype EventAggregateField' _
derive instance repGenericEventAggregateField' :: Generic EventAggregateField' _
instance showEventAggregateField' :: Show EventAggregateField' where
  show = genericShow
instance decodeEventAggregateField' :: Decode EventAggregateField' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventAggregateField' :: Encode EventAggregateField' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventArn' = EventArn' String
derive instance newtypeEventArn' :: Newtype EventArn' _
derive instance repGenericEventArn' :: Generic EventArn' _
instance showEventArn' :: Show EventArn' where
  show = genericShow
instance decodeEventArn' :: Decode EventArn' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventArn' :: Encode EventArn' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventArnList' = EventArnList' (Array EventArn')
derive instance newtypeEventArnList' :: Newtype EventArnList' _
derive instance repGenericEventArnList' :: Generic EventArnList' _
instance showEventArnList' :: Show EventArnList' where
  show = genericShow
instance decodeEventArnList' :: Decode EventArnList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventArnList' :: Encode EventArnList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventDescription' = EventDescription' String
derive instance newtypeEventDescription' :: Newtype EventDescription' _
derive instance repGenericEventDescription' :: Generic EventDescription' _
instance showEventDescription' :: Show EventDescription' where
  show = genericShow
instance decodeEventDescription' :: Decode EventDescription' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDescription' :: Encode EventDescription' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventMetadata' = EventMetadata' (StrMap.StrMap MetadataValue')
derive instance newtypeEventMetadata' :: Newtype EventMetadata' _
derive instance repGenericEventMetadata' :: Generic EventMetadata' _
instance showEventMetadata' :: Show EventMetadata' where
  show = genericShow
instance decodeEventMetadata' :: Decode EventMetadata' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventMetadata' :: Encode EventMetadata' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventStatusCode' = EventStatusCode' String
derive instance newtypeEventStatusCode' :: Newtype EventStatusCode' _
derive instance repGenericEventStatusCode' :: Generic EventStatusCode' _
instance showEventStatusCode' :: Show EventStatusCode' where
  show = genericShow
instance decodeEventStatusCode' :: Decode EventStatusCode' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventStatusCode' :: Encode EventStatusCode' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventStatusCodeList' = EventStatusCodeList' (Array EventStatusCode')
derive instance newtypeEventStatusCodeList' :: Newtype EventStatusCodeList' _
derive instance repGenericEventStatusCodeList' :: Generic EventStatusCodeList' _
instance showEventStatusCodeList' :: Show EventStatusCodeList' where
  show = genericShow
instance decodeEventStatusCodeList' :: Decode EventStatusCodeList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventStatusCodeList' :: Encode EventStatusCodeList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventType' = EventType' String
derive instance newtypeEventType' :: Newtype EventType' _
derive instance repGenericEventType' :: Generic EventType' _
instance showEventType' :: Show EventType' where
  show = genericShow
instance decodeEventType' :: Decode EventType' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventType' :: Encode EventType' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTypeCategory' = EventTypeCategory' String
derive instance newtypeEventTypeCategory' :: Newtype EventTypeCategory' _
derive instance repGenericEventTypeCategory' :: Generic EventTypeCategory' _
instance showEventTypeCategory' :: Show EventTypeCategory' where
  show = genericShow
instance decodeEventTypeCategory' :: Decode EventTypeCategory' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypeCategory' :: Encode EventTypeCategory' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTypeCategoryList' = EventTypeCategoryList' (Array EventTypeCategory')
derive instance newtypeEventTypeCategoryList' :: Newtype EventTypeCategoryList' _
derive instance repGenericEventTypeCategoryList' :: Generic EventTypeCategoryList' _
instance showEventTypeCategoryList' :: Show EventTypeCategoryList' where
  show = genericShow
instance decodeEventTypeCategoryList' :: Decode EventTypeCategoryList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypeCategoryList' :: Encode EventTypeCategoryList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTypeCode' = EventTypeCode' String
derive instance newtypeEventTypeCode' :: Newtype EventTypeCode' _
derive instance repGenericEventTypeCode' :: Generic EventTypeCode' _
instance showEventTypeCode' :: Show EventTypeCode' where
  show = genericShow
instance decodeEventTypeCode' :: Decode EventTypeCode' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypeCode' :: Encode EventTypeCode' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTypeList' = EventTypeList' (Array EventType')
derive instance newtypeEventTypeList' :: Newtype EventTypeList' _
derive instance repGenericEventTypeList' :: Generic EventTypeList' _
instance showEventTypeList' :: Show EventTypeList' where
  show = genericShow
instance decodeEventTypeList' :: Decode EventTypeList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTypeList' :: Encode EventTypeList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Locale' = Locale' String
derive instance newtypeLocale' :: Newtype Locale' _
derive instance repGenericLocale' :: Generic Locale' _
instance showLocale' :: Show Locale' where
  show = genericShow
instance decodeLocale' :: Decode Locale' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocale' :: Encode Locale' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults' = MaxResults' Int
derive instance newtypeMaxResults' :: Newtype MaxResults' _
derive instance repGenericMaxResults' :: Generic MaxResults' _
instance showMaxResults' :: Show MaxResults' where
  show = genericShow
instance decodeMaxResults' :: Decode MaxResults' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults' :: Encode MaxResults' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetadataKey' = MetadataKey' String
derive instance newtypeMetadataKey' :: Newtype MetadataKey' _
derive instance repGenericMetadataKey' :: Generic MetadataKey' _
instance showMetadataKey' :: Show MetadataKey' where
  show = genericShow
instance decodeMetadataKey' :: Decode MetadataKey' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadataKey' :: Encode MetadataKey' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetadataValue' = MetadataValue' String
derive instance newtypeMetadataValue' :: Newtype MetadataValue' _
derive instance repGenericMetadataValue' :: Generic MetadataValue' _
instance showMetadataValue' :: Show MetadataValue' where
  show = genericShow
instance decodeMetadataValue' :: Decode MetadataValue' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetadataValue' :: Encode MetadataValue' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken' = NextToken' String
derive instance newtypeNextToken' :: Newtype NextToken' _
derive instance repGenericNextToken' :: Generic NextToken' _
instance showNextToken' :: Show NextToken' where
  show = genericShow
instance decodeNextToken' :: Decode NextToken' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken' :: Encode NextToken' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Region' = Region' String
derive instance newtypeRegion' :: Newtype Region' _
derive instance repGenericRegion' :: Generic Region' _
instance showRegion' :: Show Region' where
  show = genericShow
instance decodeRegion' :: Decode Region' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegion' :: Encode Region' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegionList' = RegionList' (Array Region')
derive instance newtypeRegionList' :: Newtype RegionList' _
derive instance repGenericRegionList' :: Generic RegionList' _
instance showRegionList' :: Show RegionList' where
  show = genericShow
instance decodeRegionList' :: Decode RegionList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegionList' :: Encode RegionList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Service' = Service' String
derive instance newtypeService' :: Newtype Service' _
derive instance repGenericService' :: Generic Service' _
instance showService' :: Show Service' where
  show = genericShow
instance decodeService' :: Decode Service' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeService' :: Encode Service' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServiceList' = ServiceList' (Array Service')
derive instance newtypeServiceList' :: Newtype ServiceList' _
derive instance repGenericServiceList' :: Generic ServiceList' _
instance showServiceList' :: Show ServiceList' where
  show = genericShow
instance decodeServiceList' :: Decode ServiceList' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceList' :: Encode ServiceList' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagFilter' = TagFilter' (Array TagSet')
derive instance newtypeTagFilter' :: Newtype TagFilter' _
derive instance repGenericTagFilter' :: Generic TagFilter' _
instance showTagFilter' :: Show TagFilter' where
  show = genericShow
instance decodeTagFilter' :: Decode TagFilter' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagFilter' :: Encode TagFilter' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey' = TagKey' String
derive instance newtypeTagKey' :: Newtype TagKey' _
derive instance repGenericTagKey' :: Generic TagKey' _
instance showTagKey' :: Show TagKey' where
  show = genericShow
instance decodeTagKey' :: Decode TagKey' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey' :: Encode TagKey' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagSet' = TagSet' (StrMap.StrMap TagValue')
derive instance newtypeTagSet' :: Newtype TagSet' _
derive instance repGenericTagSet' :: Generic TagSet' _
instance showTagSet' :: Show TagSet' where
  show = genericShow
instance decodeTagSet' :: Decode TagSet' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagSet' :: Encode TagSet' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue' = TagValue' String
derive instance newtypeTagValue' :: Newtype TagValue' _
derive instance repGenericTagValue' :: Generic TagValue' _
instance showTagValue' :: Show TagValue' where
  show = genericShow
instance decodeTagValue' :: Decode TagValue' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue' :: Encode TagValue' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
