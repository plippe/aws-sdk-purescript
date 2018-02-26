

-- | <fullname>AWS Health</fullname> <p>The AWS Health API provides programmatic access to the AWS Health information that is presented in the <a href="https://phd.aws.amazon.com/phd/home#/">AWS Personal Health Dashboard</a>. You can get information about events that affect your AWS resources:</p> <ul> <li> <p> <a>DescribeEvents</a>: Summary information about events.</p> </li> <li> <p> <a>DescribeEventDetails</a>: Detailed information about one or more events.</p> </li> <li> <p> <a>DescribeAffectedEntities</a>: Information about AWS resources that are affected by one or more events.</p> </li> </ul> <p>In addition, these operations provide information about event types and summary counts of events or affected entities:</p> <ul> <li> <p> <a>DescribeEventTypes</a>: Information about the kinds of events that AWS Health tracks.</p> </li> <li> <p> <a>DescribeEventAggregates</a>: A count of the number of events that meet specified criteria.</p> </li> <li> <p> <a>DescribeEntityAggregates</a>: A count of the number of affected entities that meet specified criteria.</p> </li> </ul> <p>The Health API requires a Business or Enterprise support plan from <a href="http://aws.amazon.com/premiumsupport/">AWS Support</a>. Calling the Health API from an account that does not have a Business or Enterprise support plan causes a <code>SubscriptionRequiredException</code>. </p> <p>For authentication of requests, AWS Health uses the <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>See the <a href="http://docs.aws.amazon.com/health/latest/ug/what-is-aws-health.html">AWS Health User Guide</a> for information about how to use the API.</p> <p> <b>Service Endpoint</b> </p> <p>The HTTP endpoint for the AWS Health API is:</p> <ul> <li> <p>https://health.us-east-1.amazonaws.com </p> </li> </ul>
module AWS.Health where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Health" :: String


-- | <p>Returns a list of entities that have been affected by the specified events, based on the specified filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service. Events that have impact beyond that of the affected entities, or where the extent of impact is unknown, include at least one entity indicating this.</p> <p>At least one event ARN is required. Results are sorted by the <code>lastUpdatedTime</code> of the entity, starting with the most recent.</p>
describeAffectedEntities :: forall eff. DescribeAffectedEntitiesRequest -> Aff (err :: AWS.RequestError | eff) DescribeAffectedEntitiesResponse
describeAffectedEntities = AWS.request serviceName "DescribeAffectedEntities" 


-- | <p>Returns the number of entities that are affected by each of the specified events. If no events are specified, the counts of all affected entities are returned.</p>
describeEntityAggregates :: forall eff. DescribeEntityAggregatesRequest -> Aff (err :: AWS.RequestError | eff) DescribeEntityAggregatesResponse
describeEntityAggregates = AWS.request serviceName "DescribeEntityAggregates" 


-- | <p>Returns the number of events of each event type (issue, scheduled change, and account notification). If no filter is specified, the counts of all events in each category are returned.</p>
describeEventAggregates :: forall eff. DescribeEventAggregatesRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventAggregatesResponse
describeEventAggregates = AWS.request serviceName "DescribeEventAggregates" 


-- | <p>Returns detailed information about one or more specified events. Information includes standard event data (region, service, etc., as returned by <a>DescribeEvents</a>), a detailed event description, and possible additional metadata that depends upon the nature of the event. Affected entities are not included; to retrieve those, use the <a>DescribeAffectedEntities</a> operation.</p> <p>If a specified event cannot be retrieved, an error message is returned for that event.</p>
describeEventDetails :: forall eff. DescribeEventDetailsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventDetailsResponse
describeEventDetails = AWS.request serviceName "DescribeEventDetails" 


-- | <p>Returns the event types that meet the specified filter criteria. If no filter criteria are specified, all event types are returned, in no particular order.</p>
describeEventTypes :: forall eff. DescribeEventTypesRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventTypesResponse
describeEventTypes = AWS.request serviceName "DescribeEventTypes" 


-- | <p>Returns information about events that meet the specified filter criteria. Events are returned in a summary form and do not include the detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the <a>DescribeEventDetails</a> and <a>DescribeAffectedEntities</a> operations.</p> <p>If no filter criteria are specified, all events are returned. Results are sorted by <code>lastModifiedTime</code>, starting with the most recent.</p>
describeEvents :: forall eff. DescribeEventsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventsResponse
describeEvents = AWS.request serviceName "DescribeEvents" 


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


-- | <p>A range of dates and times that is used by the <a>EventFilter</a> and <a>EntityFilter</a> objects. If <code>from</code> is set and <code>to</code> is set: match items where the timestamp (<code>startTime</code>, <code>endTime</code>, or <code>lastUpdatedTime</code>) is between <code>from</code> and <code>to</code> inclusive. If <code>from</code> is set and <code>to</code> is not set: match items where the timestamp value is equal to or after <code>from</code>. If <code>from</code> is not set and <code>to</code> is set: match items where the timestamp value is equal to or before <code>to</code>.</p>
newtype DateTimeRange = DateTimeRange 
  { "From'" :: NullOrUndefined (Number)
  , "To'" :: NullOrUndefined (Number)
  }


newtype DescribeAffectedEntitiesRequest = DescribeAffectedEntitiesRequest 
  { "Filter'" :: (EntityFilter)
  , "Locale'" :: NullOrUndefined (Locale')
  , "NextToken'" :: NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined (MaxResults')
  }


newtype DescribeAffectedEntitiesResponse = DescribeAffectedEntitiesResponse 
  { "Entities'" :: NullOrUndefined (EntityList)
  , "NextToken'" :: NullOrUndefined (NextToken')
  }


newtype DescribeEntityAggregatesRequest = DescribeEntityAggregatesRequest 
  { "EventArns'" :: NullOrUndefined (EventArnsList)
  }


newtype DescribeEntityAggregatesResponse = DescribeEntityAggregatesResponse 
  { "EntityAggregates'" :: NullOrUndefined (EntityAggregateList)
  }


newtype DescribeEventAggregatesRequest = DescribeEventAggregatesRequest 
  { "Filter'" :: NullOrUndefined (EventFilter)
  , "AggregateField'" :: (EventAggregateField')
  , "MaxResults'" :: NullOrUndefined (MaxResults')
  , "NextToken'" :: NullOrUndefined (NextToken')
  }


newtype DescribeEventAggregatesResponse = DescribeEventAggregatesResponse 
  { "EventAggregates'" :: NullOrUndefined (EventAggregateList)
  , "NextToken'" :: NullOrUndefined (NextToken')
  }


newtype DescribeEventDetailsFailedSet = DescribeEventDetailsFailedSet (Array EventDetailsErrorItem)


newtype DescribeEventDetailsRequest = DescribeEventDetailsRequest 
  { "EventArns'" :: (EventArnList')
  , "Locale'" :: NullOrUndefined (Locale')
  }


newtype DescribeEventDetailsResponse = DescribeEventDetailsResponse 
  { "SuccessfulSet'" :: NullOrUndefined (DescribeEventDetailsSuccessfulSet)
  , "FailedSet'" :: NullOrUndefined (DescribeEventDetailsFailedSet)
  }


newtype DescribeEventDetailsSuccessfulSet = DescribeEventDetailsSuccessfulSet (Array EventDetails)


newtype DescribeEventTypesRequest = DescribeEventTypesRequest 
  { "Filter'" :: NullOrUndefined (EventTypeFilter)
  , "Locale'" :: NullOrUndefined (Locale')
  , "NextToken'" :: NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined (MaxResults')
  }


newtype DescribeEventTypesResponse = DescribeEventTypesResponse 
  { "EventTypes'" :: NullOrUndefined (EventTypeList)
  , "NextToken'" :: NullOrUndefined (NextToken')
  }


newtype DescribeEventsRequest = DescribeEventsRequest 
  { "Filter'" :: NullOrUndefined (EventFilter)
  , "NextToken'" :: NullOrUndefined (NextToken')
  , "MaxResults'" :: NullOrUndefined (MaxResults')
  , "Locale'" :: NullOrUndefined (Locale')
  }


newtype DescribeEventsResponse = DescribeEventsResponse 
  { "Events'" :: NullOrUndefined (EventList)
  , "NextToken'" :: NullOrUndefined (NextToken')
  }


-- | <p>The number of entities that are affected by one or more events. Returned by the <a>DescribeEntityAggregates</a> operation.</p>
newtype EntityAggregate = EntityAggregate 
  { "EventArn'" :: NullOrUndefined (EventArn')
  , "Count'" :: NullOrUndefined (Count')
  }


newtype EntityAggregateList = EntityAggregateList (Array EntityAggregate)


-- | <p>The values to use to filter results from the <a>DescribeAffectedEntities</a> operation.</p>
newtype EntityFilter = EntityFilter 
  { "EventArns'" :: (EventArnList')
  , "EntityArns'" :: NullOrUndefined (EntityArnList')
  , "EntityValues'" :: NullOrUndefined (EntityValueList')
  , "LastUpdatedTimes'" :: NullOrUndefined (DateTimeRangeList')
  , "Tags'" :: NullOrUndefined (TagFilter')
  , "StatusCodes'" :: NullOrUndefined (EntityStatusCodeList')
  }


newtype EntityList = EntityList (Array AffectedEntity)


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


-- | <p>The number of events of each issue type. Returned by the <a>DescribeEventAggregates</a> operation.</p>
newtype EventAggregate = EventAggregate 
  { "AggregateValue'" :: NullOrUndefined (AggregateValue')
  , "Count'" :: NullOrUndefined (Count')
  }


newtype EventAggregateList = EventAggregateList (Array EventAggregate)


newtype EventArnsList = EventArnsList (Array EventArn')


-- | <p>The detailed description of the event. Included in the information returned by the <a>DescribeEventDetails</a> operation.</p>
newtype EventDescription = EventDescription 
  { "LatestDescription'" :: NullOrUndefined (EventDescription')
  }


-- | <p>Detailed information about an event. A combination of an <a>Event</a> object, an <a>EventDescription</a> object, and additional metadata about the event. Returned by the <a>DescribeEventDetails</a> operation.</p>
newtype EventDetails = EventDetails 
  { "Event'" :: NullOrUndefined (Event)
  , "EventDescription'" :: NullOrUndefined (EventDescription)
  , "EventMetadata'" :: NullOrUndefined (EventMetadata')
  }


-- | <p>Error information returned when a <a>DescribeEventDetails</a> operation cannot find a specified event.</p>
newtype EventDetailsErrorItem = EventDetailsErrorItem 
  { "EventArn'" :: NullOrUndefined (EventArn')
  , "ErrorName'" :: NullOrUndefined (String)
  , "ErrorMessage'" :: NullOrUndefined (String)
  }


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


newtype EventList = EventList (Array Event)


-- | <p>Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, <code>issue</code>), the service (for example, <code>EC2</code>), and the event type code (for example, <code>AWS_EC2_SYSTEM_MAINTENANCE_EVENT</code>).</p>
newtype EventType = EventType 
  { "Service'" :: NullOrUndefined (Service')
  , "Code'" :: NullOrUndefined (EventTypeCode')
  , "Category'" :: NullOrUndefined (EventTypeCategory')
  }


newtype EventTypeCategoryList = EventTypeCategoryList (Array EventTypeCategory')


newtype EventTypeCodeList = EventTypeCodeList (Array EventTypeCode')


-- | <p>The values to use to filter results from the <a>DescribeEventTypes</a> operation.</p>
newtype EventTypeFilter = EventTypeFilter 
  { "EventTypeCodes'" :: NullOrUndefined (EventTypeCodeList)
  , "Services'" :: NullOrUndefined (ServiceList')
  , "EventTypeCategories'" :: NullOrUndefined (EventTypeCategoryList)
  }


newtype EventTypeList = EventTypeList (Array EventType)


-- | <p>The specified pagination token (<code>nextToken</code>) is not valid.</p>
newtype InvalidPaginationToken = InvalidPaginationToken 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>The specified locale is not supported.</p>
newtype UnsupportedLocale = UnsupportedLocale 
  { "Message'" :: NullOrUndefined (String)
  }


newtype AccountId' = AccountId' String


newtype AggregateValue' = AggregateValue' String


newtype AvailabilityZone' = AvailabilityZone' String


newtype AvailabilityZones' = AvailabilityZones' (Array AvailabilityZone')


newtype Count' = Count' Int


newtype DateTimeRangeList' = DateTimeRangeList' (Array DateTimeRange)


newtype EntityArn' = EntityArn' String


newtype EntityArnList' = EntityArnList' (Array EntityArn')


newtype EntityStatusCode' = EntityStatusCode' String


newtype EntityStatusCodeList' = EntityStatusCodeList' (Array EntityStatusCode')


newtype EntityValue' = EntityValue' String


newtype EntityValueList' = EntityValueList' (Array EntityValue')


newtype EventAggregateField' = EventAggregateField' String


newtype EventArn' = EventArn' String


newtype EventArnList' = EventArnList' (Array EventArn')


newtype EventDescription' = EventDescription' String


newtype EventMetadata' = EventMetadata' (Map MetadataKey' MetadataValue')


newtype EventStatusCode' = EventStatusCode' String


newtype EventStatusCodeList' = EventStatusCodeList' (Array EventStatusCode')


newtype EventType' = EventType' String


newtype EventTypeCategory' = EventTypeCategory' String


newtype EventTypeCategoryList' = EventTypeCategoryList' (Array EventTypeCategory')


newtype EventTypeCode' = EventTypeCode' String


newtype EventTypeList' = EventTypeList' (Array EventType')


newtype Locale' = Locale' String


newtype MaxResults' = MaxResults' Int


newtype MetadataKey' = MetadataKey' String


newtype MetadataValue' = MetadataValue' String


newtype NextToken' = NextToken' String


newtype Region' = Region' String


newtype RegionList' = RegionList' (Array Region')


newtype Service' = Service' String


newtype ServiceList' = ServiceList' (Array Service')


newtype TagFilter' = TagFilter' (Array TagSet')


newtype TagKey' = TagKey' String


newtype TagSet' = TagSet' (Map TagKey' TagValue')


newtype TagValue' = TagValue' String
