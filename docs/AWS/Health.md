## Module AWS.Health

<fullname>AWS Health</fullname> <p>The AWS Health API provides programmatic access to the AWS Health information that is presented in the <a href="https://phd.aws.amazon.com/phd/home#/">AWS Personal Health Dashboard</a>. You can get information about events that affect your AWS resources:</p> <ul> <li> <p> <a>DescribeEvents</a>: Summary information about events.</p> </li> <li> <p> <a>DescribeEventDetails</a>: Detailed information about one or more events.</p> </li> <li> <p> <a>DescribeAffectedEntities</a>: Information about AWS resources that are affected by one or more events.</p> </li> </ul> <p>In addition, these operations provide information about event types and summary counts of events or affected entities:</p> <ul> <li> <p> <a>DescribeEventTypes</a>: Information about the kinds of events that AWS Health tracks.</p> </li> <li> <p> <a>DescribeEventAggregates</a>: A count of the number of events that meet specified criteria.</p> </li> <li> <p> <a>DescribeEntityAggregates</a>: A count of the number of affected entities that meet specified criteria.</p> </li> </ul> <p>The Health API requires a Business or Enterprise support plan from <a href="http://aws.amazon.com/premiumsupport/">AWS Support</a>. Calling the Health API from an account that does not have a Business or Enterprise support plan causes a <code>SubscriptionRequiredException</code>. </p> <p>For authentication of requests, AWS Health uses the <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>See the <a href="http://docs.aws.amazon.com/health/latest/ug/what-is-aws-health.html">AWS Health User Guide</a> for information about how to use the API.</p> <p> <b>Service Endpoint</b> </p> <p>The HTTP endpoint for the AWS Health API is:</p> <ul> <li> <p>https://health.us-east-1.amazonaws.com </p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `describeAffectedEntities`

``` purescript
describeAffectedEntities :: forall eff. DescribeAffectedEntitiesRequest -> Aff (err :: RequestError | eff) DescribeAffectedEntitiesResponse
```

<p>Returns a list of entities that have been affected by the specified events, based on the specified filter criteria. Entities can refer to individual customer resources, groups of customer resources, or any other construct, depending on the AWS service. Events that have impact beyond that of the affected entities, or where the extent of impact is unknown, include at least one entity indicating this.</p> <p>At least one event ARN is required. Results are sorted by the <code>lastUpdatedTime</code> of the entity, starting with the most recent.</p>

#### `describeEntityAggregates`

``` purescript
describeEntityAggregates :: forall eff. DescribeEntityAggregatesRequest -> Aff (err :: RequestError | eff) DescribeEntityAggregatesResponse
```

<p>Returns the number of entities that are affected by each of the specified events. If no events are specified, the counts of all affected entities are returned.</p>

#### `describeEventAggregates`

``` purescript
describeEventAggregates :: forall eff. DescribeEventAggregatesRequest -> Aff (err :: RequestError | eff) DescribeEventAggregatesResponse
```

<p>Returns the number of events of each event type (issue, scheduled change, and account notification). If no filter is specified, the counts of all events in each category are returned.</p>

#### `describeEventDetails`

``` purescript
describeEventDetails :: forall eff. DescribeEventDetailsRequest -> Aff (err :: RequestError | eff) DescribeEventDetailsResponse
```

<p>Returns detailed information about one or more specified events. Information includes standard event data (region, service, etc., as returned by <a>DescribeEvents</a>), a detailed event description, and possible additional metadata that depends upon the nature of the event. Affected entities are not included; to retrieve those, use the <a>DescribeAffectedEntities</a> operation.</p> <p>If a specified event cannot be retrieved, an error message is returned for that event.</p>

#### `describeEventTypes`

``` purescript
describeEventTypes :: forall eff. DescribeEventTypesRequest -> Aff (err :: RequestError | eff) DescribeEventTypesResponse
```

<p>Returns the event types that meet the specified filter criteria. If no filter criteria are specified, all event types are returned, in no particular order.</p>

#### `describeEvents`

``` purescript
describeEvents :: forall eff. DescribeEventsRequest -> Aff (err :: RequestError | eff) DescribeEventsResponse
```

<p>Returns information about events that meet the specified filter criteria. Events are returned in a summary form and do not include the detailed description, any additional metadata that depends on the event type, or any affected resources. To retrieve that information, use the <a>DescribeEventDetails</a> and <a>DescribeAffectedEntities</a> operations.</p> <p>If no filter criteria are specified, all events are returned. Results are sorted by <code>lastModifiedTime</code>, starting with the most recent.</p>

#### `AffectedEntity`

``` purescript
newtype AffectedEntity
  = AffectedEntity { "EntityArn'" :: NullOrUndefined (EntityArn'), "EventArn'" :: NullOrUndefined (EventArn'), "EntityValue'" :: NullOrUndefined (EntityValue'), "AwsAccountId'" :: NullOrUndefined (AccountId'), "LastUpdatedTime'" :: NullOrUndefined (Number), "StatusCode'" :: NullOrUndefined (EntityStatusCode'), "Tags'" :: NullOrUndefined (TagSet') }
```

<p>Information about an entity that is affected by a Health event.</p>

##### Instances
``` purescript
Newtype AffectedEntity _
```

#### `DateTimeRange`

``` purescript
newtype DateTimeRange
  = DateTimeRange { "From'" :: NullOrUndefined (Number), "To'" :: NullOrUndefined (Number) }
```

<p>A range of dates and times that is used by the <a>EventFilter</a> and <a>EntityFilter</a> objects. If <code>from</code> is set and <code>to</code> is set: match items where the timestamp (<code>startTime</code>, <code>endTime</code>, or <code>lastUpdatedTime</code>) is between <code>from</code> and <code>to</code> inclusive. If <code>from</code> is set and <code>to</code> is not set: match items where the timestamp value is equal to or after <code>from</code>. If <code>from</code> is not set and <code>to</code> is set: match items where the timestamp value is equal to or before <code>to</code>.</p>

##### Instances
``` purescript
Newtype DateTimeRange _
```

#### `DescribeAffectedEntitiesRequest`

``` purescript
newtype DescribeAffectedEntitiesRequest
  = DescribeAffectedEntitiesRequest { "Filter'" :: EntityFilter, "Locale'" :: NullOrUndefined (Locale'), "NextToken'" :: NullOrUndefined (NextToken'), "MaxResults'" :: NullOrUndefined (MaxResults') }
```

##### Instances
``` purescript
Newtype DescribeAffectedEntitiesRequest _
```

#### `DescribeAffectedEntitiesResponse`

``` purescript
newtype DescribeAffectedEntitiesResponse
  = DescribeAffectedEntitiesResponse { "Entities'" :: NullOrUndefined (EntityList), "NextToken'" :: NullOrUndefined (NextToken') }
```

##### Instances
``` purescript
Newtype DescribeAffectedEntitiesResponse _
```

#### `DescribeEntityAggregatesRequest`

``` purescript
newtype DescribeEntityAggregatesRequest
  = DescribeEntityAggregatesRequest { "EventArns'" :: NullOrUndefined (EventArnsList) }
```

##### Instances
``` purescript
Newtype DescribeEntityAggregatesRequest _
```

#### `DescribeEntityAggregatesResponse`

``` purescript
newtype DescribeEntityAggregatesResponse
  = DescribeEntityAggregatesResponse { "EntityAggregates'" :: NullOrUndefined (EntityAggregateList) }
```

##### Instances
``` purescript
Newtype DescribeEntityAggregatesResponse _
```

#### `DescribeEventAggregatesRequest`

``` purescript
newtype DescribeEventAggregatesRequest
  = DescribeEventAggregatesRequest { "Filter'" :: NullOrUndefined (EventFilter), "AggregateField'" :: EventAggregateField', "MaxResults'" :: NullOrUndefined (MaxResults'), "NextToken'" :: NullOrUndefined (NextToken') }
```

##### Instances
``` purescript
Newtype DescribeEventAggregatesRequest _
```

#### `DescribeEventAggregatesResponse`

``` purescript
newtype DescribeEventAggregatesResponse
  = DescribeEventAggregatesResponse { "EventAggregates'" :: NullOrUndefined (EventAggregateList), "NextToken'" :: NullOrUndefined (NextToken') }
```

##### Instances
``` purescript
Newtype DescribeEventAggregatesResponse _
```

#### `DescribeEventDetailsFailedSet`

``` purescript
newtype DescribeEventDetailsFailedSet
  = DescribeEventDetailsFailedSet (Array EventDetailsErrorItem)
```

##### Instances
``` purescript
Newtype DescribeEventDetailsFailedSet _
```

#### `DescribeEventDetailsRequest`

``` purescript
newtype DescribeEventDetailsRequest
  = DescribeEventDetailsRequest { "EventArns'" :: EventArnList', "Locale'" :: NullOrUndefined (Locale') }
```

##### Instances
``` purescript
Newtype DescribeEventDetailsRequest _
```

#### `DescribeEventDetailsResponse`

``` purescript
newtype DescribeEventDetailsResponse
  = DescribeEventDetailsResponse { "SuccessfulSet'" :: NullOrUndefined (DescribeEventDetailsSuccessfulSet), "FailedSet'" :: NullOrUndefined (DescribeEventDetailsFailedSet) }
```

##### Instances
``` purescript
Newtype DescribeEventDetailsResponse _
```

#### `DescribeEventDetailsSuccessfulSet`

``` purescript
newtype DescribeEventDetailsSuccessfulSet
  = DescribeEventDetailsSuccessfulSet (Array EventDetails)
```

##### Instances
``` purescript
Newtype DescribeEventDetailsSuccessfulSet _
```

#### `DescribeEventTypesRequest`

``` purescript
newtype DescribeEventTypesRequest
  = DescribeEventTypesRequest { "Filter'" :: NullOrUndefined (EventTypeFilter), "Locale'" :: NullOrUndefined (Locale'), "NextToken'" :: NullOrUndefined (NextToken'), "MaxResults'" :: NullOrUndefined (MaxResults') }
```

##### Instances
``` purescript
Newtype DescribeEventTypesRequest _
```

#### `DescribeEventTypesResponse`

``` purescript
newtype DescribeEventTypesResponse
  = DescribeEventTypesResponse { "EventTypes'" :: NullOrUndefined (EventTypeList), "NextToken'" :: NullOrUndefined (NextToken') }
```

##### Instances
``` purescript
Newtype DescribeEventTypesResponse _
```

#### `DescribeEventsRequest`

``` purescript
newtype DescribeEventsRequest
  = DescribeEventsRequest { "Filter'" :: NullOrUndefined (EventFilter), "NextToken'" :: NullOrUndefined (NextToken'), "MaxResults'" :: NullOrUndefined (MaxResults'), "Locale'" :: NullOrUndefined (Locale') }
```

##### Instances
``` purescript
Newtype DescribeEventsRequest _
```

#### `DescribeEventsResponse`

``` purescript
newtype DescribeEventsResponse
  = DescribeEventsResponse { "Events'" :: NullOrUndefined (EventList), "NextToken'" :: NullOrUndefined (NextToken') }
```

##### Instances
``` purescript
Newtype DescribeEventsResponse _
```

#### `EntityAggregate`

``` purescript
newtype EntityAggregate
  = EntityAggregate { "EventArn'" :: NullOrUndefined (EventArn'), "Count'" :: NullOrUndefined (Count') }
```

<p>The number of entities that are affected by one or more events. Returned by the <a>DescribeEntityAggregates</a> operation.</p>

##### Instances
``` purescript
Newtype EntityAggregate _
```

#### `EntityAggregateList`

``` purescript
newtype EntityAggregateList
  = EntityAggregateList (Array EntityAggregate)
```

##### Instances
``` purescript
Newtype EntityAggregateList _
```

#### `EntityFilter`

``` purescript
newtype EntityFilter
  = EntityFilter { "EventArns'" :: EventArnList', "EntityArns'" :: NullOrUndefined (EntityArnList'), "EntityValues'" :: NullOrUndefined (EntityValueList'), "LastUpdatedTimes'" :: NullOrUndefined (DateTimeRangeList'), "Tags'" :: NullOrUndefined (TagFilter'), "StatusCodes'" :: NullOrUndefined (EntityStatusCodeList') }
```

<p>The values to use to filter results from the <a>DescribeAffectedEntities</a> operation.</p>

##### Instances
``` purescript
Newtype EntityFilter _
```

#### `EntityList`

``` purescript
newtype EntityList
  = EntityList (Array AffectedEntity)
```

##### Instances
``` purescript
Newtype EntityList _
```

#### `Event`

``` purescript
newtype Event
  = Event { "Arn'" :: NullOrUndefined (EventArn'), "Service'" :: NullOrUndefined (Service'), "EventTypeCode'" :: NullOrUndefined (EventTypeCode'), "EventTypeCategory'" :: NullOrUndefined (EventTypeCategory'), "Region'" :: NullOrUndefined (Region'), "AvailabilityZone'" :: NullOrUndefined (AvailabilityZone'), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "LastUpdatedTime'" :: NullOrUndefined (Number), "StatusCode'" :: NullOrUndefined (EventStatusCode') }
```

<p>Summary information about an event, returned by the <a>DescribeEvents</a> operation. The <a>DescribeEventDetails</a> operation also returns this information, as well as the <a>EventDescription</a> and additional event metadata.</p>

##### Instances
``` purescript
Newtype Event _
```

#### `EventAggregate`

``` purescript
newtype EventAggregate
  = EventAggregate { "AggregateValue'" :: NullOrUndefined (AggregateValue'), "Count'" :: NullOrUndefined (Count') }
```

<p>The number of events of each issue type. Returned by the <a>DescribeEventAggregates</a> operation.</p>

##### Instances
``` purescript
Newtype EventAggregate _
```

#### `EventAggregateList`

``` purescript
newtype EventAggregateList
  = EventAggregateList (Array EventAggregate)
```

##### Instances
``` purescript
Newtype EventAggregateList _
```

#### `EventArnsList`

``` purescript
newtype EventArnsList
  = EventArnsList (Array EventArn')
```

##### Instances
``` purescript
Newtype EventArnsList _
```

#### `EventDescription`

``` purescript
newtype EventDescription
  = EventDescription { "LatestDescription'" :: NullOrUndefined (EventDescription') }
```

<p>The detailed description of the event. Included in the information returned by the <a>DescribeEventDetails</a> operation.</p>

##### Instances
``` purescript
Newtype EventDescription _
```

#### `EventDetails`

``` purescript
newtype EventDetails
  = EventDetails { "Event'" :: NullOrUndefined (Event), "EventDescription'" :: NullOrUndefined (EventDescription), "EventMetadata'" :: NullOrUndefined (EventMetadata') }
```

<p>Detailed information about an event. A combination of an <a>Event</a> object, an <a>EventDescription</a> object, and additional metadata about the event. Returned by the <a>DescribeEventDetails</a> operation.</p>

##### Instances
``` purescript
Newtype EventDetails _
```

#### `EventDetailsErrorItem`

``` purescript
newtype EventDetailsErrorItem
  = EventDetailsErrorItem { "EventArn'" :: NullOrUndefined (EventArn'), "ErrorName'" :: NullOrUndefined (String), "ErrorMessage'" :: NullOrUndefined (String) }
```

<p>Error information returned when a <a>DescribeEventDetails</a> operation cannot find a specified event.</p>

##### Instances
``` purescript
Newtype EventDetailsErrorItem _
```

#### `EventFilter`

``` purescript
newtype EventFilter
  = EventFilter { "EventArns'" :: NullOrUndefined (EventArnList'), "EventTypeCodes'" :: NullOrUndefined (EventTypeList'), "Services'" :: NullOrUndefined (ServiceList'), "Regions'" :: NullOrUndefined (RegionList'), "AvailabilityZones'" :: NullOrUndefined (AvailabilityZones'), "StartTimes'" :: NullOrUndefined (DateTimeRangeList'), "EndTimes'" :: NullOrUndefined (DateTimeRangeList'), "LastUpdatedTimes'" :: NullOrUndefined (DateTimeRangeList'), "EntityArns'" :: NullOrUndefined (EntityArnList'), "EntityValues'" :: NullOrUndefined (EntityValueList'), "EventTypeCategories'" :: NullOrUndefined (EventTypeCategoryList'), "Tags'" :: NullOrUndefined (TagFilter'), "EventStatusCodes'" :: NullOrUndefined (EventStatusCodeList') }
```

<p>The values to use to filter results from the <a>DescribeEvents</a> and <a>DescribeEventAggregates</a> operations.</p>

##### Instances
``` purescript
Newtype EventFilter _
```

#### `EventList`

``` purescript
newtype EventList
  = EventList (Array Event)
```

##### Instances
``` purescript
Newtype EventList _
```

#### `EventType`

``` purescript
newtype EventType
  = EventType { "Service'" :: NullOrUndefined (Service'), "Code'" :: NullOrUndefined (EventTypeCode'), "Category'" :: NullOrUndefined (EventTypeCategory') }
```

<p>Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, <code>issue</code>), the service (for example, <code>EC2</code>), and the event type code (for example, <code>AWS_EC2_SYSTEM_MAINTENANCE_EVENT</code>).</p>

##### Instances
``` purescript
Newtype EventType _
```

#### `EventTypeCategoryList`

``` purescript
newtype EventTypeCategoryList
  = EventTypeCategoryList (Array EventTypeCategory')
```

##### Instances
``` purescript
Newtype EventTypeCategoryList _
```

#### `EventTypeCodeList`

``` purescript
newtype EventTypeCodeList
  = EventTypeCodeList (Array EventTypeCode')
```

##### Instances
``` purescript
Newtype EventTypeCodeList _
```

#### `EventTypeFilter`

``` purescript
newtype EventTypeFilter
  = EventTypeFilter { "EventTypeCodes'" :: NullOrUndefined (EventTypeCodeList), "Services'" :: NullOrUndefined (ServiceList'), "EventTypeCategories'" :: NullOrUndefined (EventTypeCategoryList) }
```

<p>The values to use to filter results from the <a>DescribeEventTypes</a> operation.</p>

##### Instances
``` purescript
Newtype EventTypeFilter _
```

#### `EventTypeList`

``` purescript
newtype EventTypeList
  = EventTypeList (Array EventType)
```

##### Instances
``` purescript
Newtype EventTypeList _
```

#### `InvalidPaginationToken`

``` purescript
newtype InvalidPaginationToken
  = InvalidPaginationToken { "Message'" :: NullOrUndefined (String) }
```

<p>The specified pagination token (<code>nextToken</code>) is not valid.</p>

##### Instances
``` purescript
Newtype InvalidPaginationToken _
```

#### `UnsupportedLocale`

``` purescript
newtype UnsupportedLocale
  = UnsupportedLocale { "Message'" :: NullOrUndefined (String) }
```

<p>The specified locale is not supported.</p>

##### Instances
``` purescript
Newtype UnsupportedLocale _
```

#### `AccountId'`

``` purescript
newtype AccountId'
  = AccountId' String
```

##### Instances
``` purescript
Newtype AccountId' _
```

#### `AggregateValue'`

``` purescript
newtype AggregateValue'
  = AggregateValue' String
```

##### Instances
``` purescript
Newtype AggregateValue' _
```

#### `AvailabilityZone'`

``` purescript
newtype AvailabilityZone'
  = AvailabilityZone' String
```

##### Instances
``` purescript
Newtype AvailabilityZone' _
```

#### `AvailabilityZones'`

``` purescript
newtype AvailabilityZones'
  = AvailabilityZones' (Array AvailabilityZone')
```

##### Instances
``` purescript
Newtype AvailabilityZones' _
```

#### `Count'`

``` purescript
newtype Count'
  = Count' Int
```

##### Instances
``` purescript
Newtype Count' _
```

#### `DateTimeRangeList'`

``` purescript
newtype DateTimeRangeList'
  = DateTimeRangeList' (Array DateTimeRange)
```

##### Instances
``` purescript
Newtype DateTimeRangeList' _
```

#### `EntityArn'`

``` purescript
newtype EntityArn'
  = EntityArn' String
```

##### Instances
``` purescript
Newtype EntityArn' _
```

#### `EntityArnList'`

``` purescript
newtype EntityArnList'
  = EntityArnList' (Array EntityArn')
```

##### Instances
``` purescript
Newtype EntityArnList' _
```

#### `EntityStatusCode'`

``` purescript
newtype EntityStatusCode'
  = EntityStatusCode' String
```

##### Instances
``` purescript
Newtype EntityStatusCode' _
```

#### `EntityStatusCodeList'`

``` purescript
newtype EntityStatusCodeList'
  = EntityStatusCodeList' (Array EntityStatusCode')
```

##### Instances
``` purescript
Newtype EntityStatusCodeList' _
```

#### `EntityValue'`

``` purescript
newtype EntityValue'
  = EntityValue' String
```

##### Instances
``` purescript
Newtype EntityValue' _
```

#### `EntityValueList'`

``` purescript
newtype EntityValueList'
  = EntityValueList' (Array EntityValue')
```

##### Instances
``` purescript
Newtype EntityValueList' _
```

#### `EventAggregateField'`

``` purescript
newtype EventAggregateField'
  = EventAggregateField' String
```

##### Instances
``` purescript
Newtype EventAggregateField' _
```

#### `EventArn'`

``` purescript
newtype EventArn'
  = EventArn' String
```

##### Instances
``` purescript
Newtype EventArn' _
```

#### `EventArnList'`

``` purescript
newtype EventArnList'
  = EventArnList' (Array EventArn')
```

##### Instances
``` purescript
Newtype EventArnList' _
```

#### `EventDescription'`

``` purescript
newtype EventDescription'
  = EventDescription' String
```

##### Instances
``` purescript
Newtype EventDescription' _
```

#### `EventMetadata'`

``` purescript
newtype EventMetadata'
  = EventMetadata' (Map MetadataKey' MetadataValue')
```

##### Instances
``` purescript
Newtype EventMetadata' _
```

#### `EventStatusCode'`

``` purescript
newtype EventStatusCode'
  = EventStatusCode' String
```

##### Instances
``` purescript
Newtype EventStatusCode' _
```

#### `EventStatusCodeList'`

``` purescript
newtype EventStatusCodeList'
  = EventStatusCodeList' (Array EventStatusCode')
```

##### Instances
``` purescript
Newtype EventStatusCodeList' _
```

#### `EventType'`

``` purescript
newtype EventType'
  = EventType' String
```

##### Instances
``` purescript
Newtype EventType' _
```

#### `EventTypeCategory'`

``` purescript
newtype EventTypeCategory'
  = EventTypeCategory' String
```

##### Instances
``` purescript
Newtype EventTypeCategory' _
```

#### `EventTypeCategoryList'`

``` purescript
newtype EventTypeCategoryList'
  = EventTypeCategoryList' (Array EventTypeCategory')
```

##### Instances
``` purescript
Newtype EventTypeCategoryList' _
```

#### `EventTypeCode'`

``` purescript
newtype EventTypeCode'
  = EventTypeCode' String
```

##### Instances
``` purescript
Newtype EventTypeCode' _
```

#### `EventTypeList'`

``` purescript
newtype EventTypeList'
  = EventTypeList' (Array EventType')
```

##### Instances
``` purescript
Newtype EventTypeList' _
```

#### `Locale'`

``` purescript
newtype Locale'
  = Locale' String
```

##### Instances
``` purescript
Newtype Locale' _
```

#### `MaxResults'`

``` purescript
newtype MaxResults'
  = MaxResults' Int
```

##### Instances
``` purescript
Newtype MaxResults' _
```

#### `MetadataKey'`

``` purescript
newtype MetadataKey'
  = MetadataKey' String
```

##### Instances
``` purescript
Newtype MetadataKey' _
```

#### `MetadataValue'`

``` purescript
newtype MetadataValue'
  = MetadataValue' String
```

##### Instances
``` purescript
Newtype MetadataValue' _
```

#### `NextToken'`

``` purescript
newtype NextToken'
  = NextToken' String
```

##### Instances
``` purescript
Newtype NextToken' _
```

#### `Region'`

``` purescript
newtype Region'
  = Region' String
```

##### Instances
``` purescript
Newtype Region' _
```

#### `RegionList'`

``` purescript
newtype RegionList'
  = RegionList' (Array Region')
```

##### Instances
``` purescript
Newtype RegionList' _
```

#### `Service'`

``` purescript
newtype Service'
  = Service' String
```

##### Instances
``` purescript
Newtype Service' _
```

#### `ServiceList'`

``` purescript
newtype ServiceList'
  = ServiceList' (Array Service')
```

##### Instances
``` purescript
Newtype ServiceList' _
```

#### `TagFilter'`

``` purescript
newtype TagFilter'
  = TagFilter' (Array TagSet')
```

##### Instances
``` purescript
Newtype TagFilter' _
```

#### `TagKey'`

``` purescript
newtype TagKey'
  = TagKey' String
```

##### Instances
``` purescript
Newtype TagKey' _
```

#### `TagSet'`

``` purescript
newtype TagSet'
  = TagSet' (Map TagKey' TagValue')
```

##### Instances
``` purescript
Newtype TagSet' _
```

#### `TagValue'`

``` purescript
newtype TagValue'
  = TagValue' String
```

##### Instances
``` purescript
Newtype TagValue' _
```


