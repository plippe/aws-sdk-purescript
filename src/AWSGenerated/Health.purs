

-- | <fullname>AWS Health</fullname> <p>The AWS Health API provides programmatic access to the AWS Health information that is presented in the <a href="https://phd.aws.amazon.com/phd/home#/">AWS Personal Health Dashboard</a>. You can get information about events that affect your AWS resources:</p> <ul> <li> <p> <a>DescribeEvents</a>: Summary information about events.</p> </li> <li> <p> <a>DescribeEventDetails</a>: Detailed information about one or more events.</p> </li> <li> <p> <a>DescribeAffectedEntities</a>: Information about AWS resources that are affected by one or more events.</p> </li> </ul> <p>In addition, these operations provide information about event types and summary counts of events or affected entities:</p> <ul> <li> <p> <a>DescribeEventTypes</a>: Information about the kinds of events that AWS Health tracks.</p> </li> <li> <p> <a>DescribeEventAggregates</a>: A count of the number of events that meet specified criteria.</p> </li> <li> <p> <a>DescribeEntityAggregates</a>: A count of the number of affected entities that meet specified criteria.</p> </li> </ul> <p>The Health API requires a Business or Enterprise support plan from <a href="http://aws.amazon.com/premiumsupport/">AWS Support</a>. Calling the Health API from an account that does not have a Business or Enterprise support plan causes a <code>SubscriptionRequiredException</code>. </p> <p>For authentication of requests, AWS Health uses the <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>See the <a href="http://docs.aws.amazon.com/health/latest/ug/what-is-aws-health.html">AWS Health User Guide</a> for information about how to use the API.</p> <p> <b>Service Endpoint</b> </p> <p>The HTTP endpoint for the AWS Health API is:</p> <ul> <li> <p>https://health.us-east-1.amazonaws.com </p> </li> </ul>
module AWS.Health where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Health" :: String


-- | <p>Returns a list of entities that have been affected by the specified events, based on the specified filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service. Events that have impact beyond that of the affected entities, or where the extent of impact is unknown, include at least one entity indicating this.</p> <p>At least one event ARN is required. Results are sorted by the <code>lastUpdatedTime</code> of the entity, starting with the most recent.</p>
describeAffectedEntities :: forall eff. DescribeAffectedEntitiesRequest -> Aff (err :: AWS.RequestError | eff) DescribeAffectedEntitiesResponse
describeAffectedEntities = AWS.request serviceName "describeAffectedEntities" 


-- | <p>Returns the number of entities that are affected by each of the specified events. If no events are specified, the counts of all affected entities are returned.</p>
describeEntityAggregates :: forall eff. DescribeEntityAggregatesRequest -> Aff (err :: AWS.RequestError | eff) DescribeEntityAggregatesResponse
describeEntityAggregates = AWS.request serviceName "describeEntityAggregates" 


-- | <p>Returns the number of events of each event type (issue, scheduled change, and account notification). If no filter is specified, the counts of all events in each category are returned.</p>
describeEventAggregates :: forall eff. DescribeEventAggregatesRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventAggregatesResponse
describeEventAggregates = AWS.request serviceName "describeEventAggregates" 


-- | <p>Returns detailed information about one or more specified events. Information includes standard event data (region, service, etc., as returned by <a>DescribeEvents</a>), a detailed event description, and possible additional metadata that depends upon the nature of the event. Affected entities are not included; to retrieve those, use the <a>DescribeAffectedEntities</a> operation.</p> <p>If a specified event cannot be retrieved, an error message is returned for that event.</p>
describeEventDetails :: forall eff. DescribeEventDetailsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventDetailsResponse
describeEventDetails = AWS.request serviceName "describeEventDetails" 


-- | <p>Returns the event types that meet the specified filter criteria. If no filter criteria are specified, all event types are returned, in no particular order.</p>
describeEventTypes :: forall eff. DescribeEventTypesRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventTypesResponse
describeEventTypes = AWS.request serviceName "describeEventTypes" 


-- | <p>Returns information about events that meet the specified filter criteria. Events are returned in a summary form and do not include the detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the <a>DescribeEventDetails</a> and <a>DescribeAffectedEntities</a> operations.</p> <p>If no filter criteria are specified, all events are returned. Results are sorted by <code>lastModifiedTime</code>, starting with the most recent.</p>
describeEvents :: forall eff. DescribeEventsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventsResponse
describeEvents = AWS.request serviceName "describeEvents" 


-- | <p>Information about an entity that is affected by a Health event.</p>
newtype AffectedEntity = AffectedEntity 
  { "EntityArn'" :: NullOrUndefined (EntityArn')
  , "EventArn'" :: NullOrUndefined (EventArn')
  , "EntityValue'" :: NullOrUndefined (EntityValue')
  , "AwsAccountId'" :: NullOrUndefined (AccountId')
  , "LastUpdatedTime'" :: NullOrUndefined (Number)
  , "StatusCode'" :: NullOrUndefined (EntityStatusCode')
  , "Tags'" :: NullOrUndefined (TagSet')
  }
derive instance newtypeAffectedEntity :: Newtype AffectedEntity _


-- | <p>A range of dates and times that is used by the <a>EventFilter</a> and <a>EntityFilter</a> objects. If <code>from</code> is set and <code>to</code> is set: match items where the timestamp (<code>startTime</code>, <code>endTime</code>, or <code>lastUpdatedTime</code>) is between <code>from</code> and <code>to</code> inclusive. If <code>from</code> is set and <code>to</code> is not set: match items where the timestamp value is equal to or after <code>from</code>. If <code>from</code> is not set and <code>to</code> is set: match items where the timestamp value is equal to or before <code>to</code>.</p>
newtype DateTimeRange = DateTimeRange 
  { "From'" :: NullOrUndefined (Number)
  , "To'" :: NullOrUndefined (Number)
  }
derive instance newtypeDateTimeRange :: Newtype DateTimeRange _


newtype DescribeAffectedEntitiesRequest = DescribeAffectedEntitiesRequest 
  { "Filter'" :: (EntityFilter)
  , "Locale'" :: NullOrUndefined (Locale')
  , "NextToken'" :: NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined (MaxResults')
  }
derive instance newtypeDescribeAffectedEntitiesRequest :: Newtype DescribeAffectedEntitiesRequest _


newtype DescribeAffectedEntitiesResponse = DescribeAffectedEntitiesResponse 
  { "Entities'" :: NullOrUndefined (EntityList)
  , "NextToken'" :: NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeAffectedEntitiesResponse :: Newtype DescribeAffectedEntitiesResponse _


newtype DescribeEntityAggregatesRequest = DescribeEntityAggregatesRequest 
  { "EventArns'" :: NullOrUndefined (EventArnsList)
  }
derive instance newtypeDescribeEntityAggregatesRequest :: Newtype DescribeEntityAggregatesRequest _


newtype DescribeEntityAggregatesResponse = DescribeEntityAggregatesResponse 
  { "EntityAggregates'" :: NullOrUndefined (EntityAggregateList)
  }
derive instance newtypeDescribeEntityAggregatesResponse :: Newtype DescribeEntityAggregatesResponse _


newtype DescribeEventAggregatesRequest = DescribeEventAggregatesRequest 
  { "Filter'" :: NullOrUndefined (EventFilter)
  , "AggregateField'" :: (EventAggregateField')
  , "MaxResults'" :: NullOrUndefined (MaxResults')
  , "NextToken'" :: NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeEventAggregatesRequest :: Newtype DescribeEventAggregatesRequest _


newtype DescribeEventAggregatesResponse = DescribeEventAggregatesResponse 
  { "EventAggregates'" :: NullOrUndefined (EventAggregateList)
  , "NextToken'" :: NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeEventAggregatesResponse :: Newtype DescribeEventAggregatesResponse _


newtype DescribeEventDetailsFailedSet = DescribeEventDetailsFailedSet (Array EventDetailsErrorItem)
derive instance newtypeDescribeEventDetailsFailedSet :: Newtype DescribeEventDetailsFailedSet _


newtype DescribeEventDetailsRequest = DescribeEventDetailsRequest 
  { "EventArns'" :: (EventArnList')
  , "Locale'" :: NullOrUndefined (Locale')
  }
derive instance newtypeDescribeEventDetailsRequest :: Newtype DescribeEventDetailsRequest _


newtype DescribeEventDetailsResponse = DescribeEventDetailsResponse 
  { "SuccessfulSet'" :: NullOrUndefined (DescribeEventDetailsSuccessfulSet)
  , "FailedSet'" :: NullOrUndefined (DescribeEventDetailsFailedSet)
  }
derive instance newtypeDescribeEventDetailsResponse :: Newtype DescribeEventDetailsResponse _


newtype DescribeEventDetailsSuccessfulSet = DescribeEventDetailsSuccessfulSet (Array EventDetails)
derive instance newtypeDescribeEventDetailsSuccessfulSet :: Newtype DescribeEventDetailsSuccessfulSet _


newtype DescribeEventTypesRequest = DescribeEventTypesRequest 
  { "Filter'" :: NullOrUndefined (EventTypeFilter)
  , "Locale'" :: NullOrUndefined (Locale')
  , "NextToken'" :: NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined (MaxResults')
  }
derive instance newtypeDescribeEventTypesRequest :: Newtype DescribeEventTypesRequest _


newtype DescribeEventTypesResponse = DescribeEventTypesResponse 
  { "EventTypes'" :: NullOrUndefined (EventTypeList)
  , "NextToken'" :: NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeEventTypesResponse :: Newtype DescribeEventTypesResponse _


newtype DescribeEventsRequest = DescribeEventsRequest 
  { "Filter'" :: NullOrUndefined (EventFilter)
  , "NextToken'" :: NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined (MaxResults')
  , "Locale'" :: NullOrUndefined (Locale')
  }
derive instance newtypeDescribeEventsRequest :: Newtype DescribeEventsRequest _


newtype DescribeEventsResponse = DescribeEventsResponse 
  { "Events'" :: NullOrUndefined (EventList)
  , "NextToken'" :: NullOrUndefined (NextToken')
  }
derive instance newtypeDescribeEventsResponse :: Newtype DescribeEventsResponse _


-- | <p>The number of entities that are affected by one or more events. Returned by the <a>DescribeEntityAggregates</a> operation.</p>
newtype EntityAggregate = EntityAggregate 
  { "EventArn'" :: NullOrUndefined (EventArn')
  , "Count'" :: NullOrUndefined (Count')
  }
derive instance newtypeEntityAggregate :: Newtype EntityAggregate _


newtype EntityAggregateList = EntityAggregateList (Array EntityAggregate)
derive instance newtypeEntityAggregateList :: Newtype EntityAggregateList _


-- | <p>The values to use to filter results from the <a>DescribeAffectedEntities</a> operation.</p>
newtype EntityFilter = EntityFilter 
  { "EventArns'" :: (EventArnList')
  , "EntityArns'" :: NullOrUndefined (EntityArnList')
  , "EntityValues'" :: NullOrUndefined (EntityValueList')
  , "LastUpdatedTimes'" :: NullOrUndefined (DateTimeRangeList')
  , "Tags'" :: NullOrUndefined (TagFilter')
  , "StatusCodes'" :: NullOrUndefined (EntityStatusCodeList')
  }
derive instance newtypeEntityFilter :: Newtype EntityFilter _


newtype EntityList = EntityList (Array AffectedEntity)
derive instance newtypeEntityList :: Newtype EntityList _


-- | <p>Summary information about an event, returned by the <a>DescribeEvents</a> operation. The <a>DescribeEventDetails</a> operation also returns this information, as well as the <a>EventDescription</a> and additional event metadata.</p>
newtype Event = Event 
  { "Arn'" :: NullOrUndefined (EventArn')
  , "Service'" :: NullOrUndefined (Service')
  , "EventTypeCode'" :: NullOrUndefined (EventTypeCode')
  , "EventTypeCategory'" :: NullOrUndefined (EventTypeCategory')
  , "Region'" :: NullOrUndefined (Region')
  , "AvailabilityZone'" :: NullOrUndefined (AvailabilityZone')
  , "StartTime'" :: NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined (Number)
  , "LastUpdatedTime'" :: NullOrUndefined (Number)
  , "StatusCode'" :: NullOrUndefined (EventStatusCode')
  }
derive instance newtypeEvent :: Newtype Event _


-- | <p>The number of events of each issue type. Returned by the <a>DescribeEventAggregates</a> operation.</p>
newtype EventAggregate = EventAggregate 
  { "AggregateValue'" :: NullOrUndefined (AggregateValue')
  , "Count'" :: NullOrUndefined (Count')
  }
derive instance newtypeEventAggregate :: Newtype EventAggregate _


newtype EventAggregateList = EventAggregateList (Array EventAggregate)
derive instance newtypeEventAggregateList :: Newtype EventAggregateList _


newtype EventArnsList = EventArnsList (Array EventArn')
derive instance newtypeEventArnsList :: Newtype EventArnsList _


-- | <p>The detailed description of the event. Included in the information returned by the <a>DescribeEventDetails</a> operation.</p>
newtype EventDescription = EventDescription 
  { "LatestDescription'" :: NullOrUndefined (EventDescription')
  }
derive instance newtypeEventDescription :: Newtype EventDescription _


-- | <p>Detailed information about an event. A combination of an <a>Event</a> object, an <a>EventDescription</a> object, and additional metadata about the event. Returned by the <a>DescribeEventDetails</a> operation.</p>
newtype EventDetails = EventDetails 
  { "Event'" :: NullOrUndefined (Event)
  , "EventDescription'" :: NullOrUndefined (EventDescription)
  , "EventMetadata'" :: NullOrUndefined (EventMetadata')
  }
derive instance newtypeEventDetails :: Newtype EventDetails _


-- | <p>Error information returned when a <a>DescribeEventDetails</a> operation cannot find a specified event.</p>
newtype EventDetailsErrorItem = EventDetailsErrorItem 
  { "EventArn'" :: NullOrUndefined (EventArn')
  , "ErrorName'" :: NullOrUndefined (String)
  , "ErrorMessage'" :: NullOrUndefined (String)
  }
derive instance newtypeEventDetailsErrorItem :: Newtype EventDetailsErrorItem _


-- | <p>The values to use to filter results from the <a>DescribeEvents</a> and <a>DescribeEventAggregates</a> operations.</p>
newtype EventFilter = EventFilter 
  { "EventArns'" :: NullOrUndefined (EventArnList')
  , "EventTypeCodes'" :: NullOrUndefined (EventTypeList')
  , "Services'" :: NullOrUndefined (ServiceList')
  , "Regions'" :: NullOrUndefined (RegionList')
  , "AvailabilityZones'" :: NullOrUndefined (AvailabilityZones')
  , "StartTimes'" :: NullOrUndefined (DateTimeRangeList')
  , "EndTimes'" :: NullOrUndefined (DateTimeRangeList')
  , "LastUpdatedTimes'" :: NullOrUndefined (DateTimeRangeList')
  , "EntityArns'" :: NullOrUndefined (EntityArnList')
  , "EntityValues'" :: NullOrUndefined (EntityValueList')
  , "EventTypeCategories'" :: NullOrUndefined (EventTypeCategoryList')
  , "Tags'" :: NullOrUndefined (TagFilter')
  , "EventStatusCodes'" :: NullOrUndefined (EventStatusCodeList')
  }
derive instance newtypeEventFilter :: Newtype EventFilter _


newtype EventList = EventList (Array Event)
derive instance newtypeEventList :: Newtype EventList _


-- | <p>Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, <code>issue</code>), the service (for example, <code>EC2</code>), and the event type code (for example, <code>AWS_EC2_SYSTEM_MAINTENANCE_EVENT</code>).</p>
newtype EventType = EventType 
  { "Service'" :: NullOrUndefined (Service')
  , "Code'" :: NullOrUndefined (EventTypeCode')
  , "Category'" :: NullOrUndefined (EventTypeCategory')
  }
derive instance newtypeEventType :: Newtype EventType _


newtype EventTypeCategoryList = EventTypeCategoryList (Array EventTypeCategory')
derive instance newtypeEventTypeCategoryList :: Newtype EventTypeCategoryList _


newtype EventTypeCodeList = EventTypeCodeList (Array EventTypeCode')
derive instance newtypeEventTypeCodeList :: Newtype EventTypeCodeList _


-- | <p>The values to use to filter results from the <a>DescribeEventTypes</a> operation.</p>
newtype EventTypeFilter = EventTypeFilter 
  { "EventTypeCodes'" :: NullOrUndefined (EventTypeCodeList)
  , "Services'" :: NullOrUndefined (ServiceList')
  , "EventTypeCategories'" :: NullOrUndefined (EventTypeCategoryList)
  }
derive instance newtypeEventTypeFilter :: Newtype EventTypeFilter _


newtype EventTypeList = EventTypeList (Array EventType)
derive instance newtypeEventTypeList :: Newtype EventTypeList _


-- | <p>The specified pagination token (<code>nextToken</code>) is not valid.</p>
newtype InvalidPaginationToken = InvalidPaginationToken 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidPaginationToken :: Newtype InvalidPaginationToken _


-- | <p>The specified locale is not supported.</p>
newtype UnsupportedLocale = UnsupportedLocale 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedLocale :: Newtype UnsupportedLocale _


newtype AccountId' = AccountId' String
derive instance newtypeAccountId' :: Newtype AccountId' _


newtype AggregateValue' = AggregateValue' String
derive instance newtypeAggregateValue' :: Newtype AggregateValue' _


newtype AvailabilityZone' = AvailabilityZone' String
derive instance newtypeAvailabilityZone' :: Newtype AvailabilityZone' _


newtype AvailabilityZones' = AvailabilityZones' (Array AvailabilityZone')
derive instance newtypeAvailabilityZones' :: Newtype AvailabilityZones' _


newtype Count' = Count' Int
derive instance newtypeCount' :: Newtype Count' _


newtype DateTimeRangeList' = DateTimeRangeList' (Array DateTimeRange)
derive instance newtypeDateTimeRangeList' :: Newtype DateTimeRangeList' _


newtype EntityArn' = EntityArn' String
derive instance newtypeEntityArn' :: Newtype EntityArn' _


newtype EntityArnList' = EntityArnList' (Array EntityArn')
derive instance newtypeEntityArnList' :: Newtype EntityArnList' _


newtype EntityStatusCode' = EntityStatusCode' String
derive instance newtypeEntityStatusCode' :: Newtype EntityStatusCode' _


newtype EntityStatusCodeList' = EntityStatusCodeList' (Array EntityStatusCode')
derive instance newtypeEntityStatusCodeList' :: Newtype EntityStatusCodeList' _


newtype EntityValue' = EntityValue' String
derive instance newtypeEntityValue' :: Newtype EntityValue' _


newtype EntityValueList' = EntityValueList' (Array EntityValue')
derive instance newtypeEntityValueList' :: Newtype EntityValueList' _


newtype EventAggregateField' = EventAggregateField' String
derive instance newtypeEventAggregateField' :: Newtype EventAggregateField' _


newtype EventArn' = EventArn' String
derive instance newtypeEventArn' :: Newtype EventArn' _


newtype EventArnList' = EventArnList' (Array EventArn')
derive instance newtypeEventArnList' :: Newtype EventArnList' _


newtype EventDescription' = EventDescription' String
derive instance newtypeEventDescription' :: Newtype EventDescription' _


newtype EventMetadata' = EventMetadata' (Map MetadataKey' MetadataValue')
derive instance newtypeEventMetadata' :: Newtype EventMetadata' _


newtype EventStatusCode' = EventStatusCode' String
derive instance newtypeEventStatusCode' :: Newtype EventStatusCode' _


newtype EventStatusCodeList' = EventStatusCodeList' (Array EventStatusCode')
derive instance newtypeEventStatusCodeList' :: Newtype EventStatusCodeList' _


newtype EventType' = EventType' String
derive instance newtypeEventType' :: Newtype EventType' _


newtype EventTypeCategory' = EventTypeCategory' String
derive instance newtypeEventTypeCategory' :: Newtype EventTypeCategory' _


newtype EventTypeCategoryList' = EventTypeCategoryList' (Array EventTypeCategory')
derive instance newtypeEventTypeCategoryList' :: Newtype EventTypeCategoryList' _


newtype EventTypeCode' = EventTypeCode' String
derive instance newtypeEventTypeCode' :: Newtype EventTypeCode' _


newtype EventTypeList' = EventTypeList' (Array EventType')
derive instance newtypeEventTypeList' :: Newtype EventTypeList' _


newtype Locale' = Locale' String
derive instance newtypeLocale' :: Newtype Locale' _


newtype MaxResults' = MaxResults' Int
derive instance newtypeMaxResults' :: Newtype MaxResults' _


newtype MetadataKey' = MetadataKey' String
derive instance newtypeMetadataKey' :: Newtype MetadataKey' _


newtype MetadataValue' = MetadataValue' String
derive instance newtypeMetadataValue' :: Newtype MetadataValue' _


newtype NextToken' = NextToken' String
derive instance newtypeNextToken' :: Newtype NextToken' _


newtype Region' = Region' String
derive instance newtypeRegion' :: Newtype Region' _


newtype RegionList' = RegionList' (Array Region')
derive instance newtypeRegionList' :: Newtype RegionList' _


newtype Service' = Service' String
derive instance newtypeService' :: Newtype Service' _


newtype ServiceList' = ServiceList' (Array Service')
derive instance newtypeServiceList' :: Newtype ServiceList' _


newtype TagFilter' = TagFilter' (Array TagSet')
derive instance newtypeTagFilter' :: Newtype TagFilter' _


newtype TagKey' = TagKey' String
derive instance newtypeTagKey' :: Newtype TagKey' _


newtype TagSet' = TagSet' (Map TagKey' TagValue')
derive instance newtypeTagSet' :: Newtype TagSet' _


newtype TagValue' = TagValue' String
derive instance newtypeTagValue' :: Newtype TagValue' _
