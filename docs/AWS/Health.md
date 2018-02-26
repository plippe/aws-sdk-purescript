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

#### `DateTimeRange`

``` purescript
newtype DateTimeRange
  = DateTimeRange { "From'" :: NullOrUndefined (Number), "To'" :: NullOrUndefined (Number) }
```

<p>A range of dates and times that is used by the <a>EventFilter</a> and <a>EntityFilter</a> objects. If <code>from</code> is set and <code>to</code> is set: match items where the timestamp (<code>startTime</code>, <code>endTime</code>, or <code>lastUpdatedTime</code>) is between <code>from</code> and <code>to</code> inclusive. If <code>from</code> is set and <code>to</code> is not set: match items where the timestamp value is equal to or after <code>from</code>. If <code>from</code> is not set and <code>to</code> is set: match items where the timestamp value is equal to or before <code>to</code>.</p>

#### `DescribeAffectedEntitiesRequest`

``` purescript
newtype DescribeAffectedEntitiesRequest
  = DescribeAffectedEntitiesRequest { "Filter'" :: EntityFilter, "Locale'" :: NullOrUndefined (Locale'), "NextToken'" :: NullOrUndefined (NextToken'), "MaxResults'" :: NullOrUndefined (MaxResults') }
```

#### `DescribeAffectedEntitiesResponse`

``` purescript
newtype DescribeAffectedEntitiesResponse
  = DescribeAffectedEntitiesResponse { "Entities'" :: NullOrUndefined (EntityList), "NextToken'" :: NullOrUndefined (NextToken') }
```

#### `DescribeEntityAggregatesRequest`

``` purescript
newtype DescribeEntityAggregatesRequest
  = DescribeEntityAggregatesRequest { "EventArns'" :: NullOrUndefined (EventArnsList) }
```

#### `DescribeEntityAggregatesResponse`

``` purescript
newtype DescribeEntityAggregatesResponse
  = DescribeEntityAggregatesResponse { "EntityAggregates'" :: NullOrUndefined (EntityAggregateList) }
```

#### `DescribeEventAggregatesRequest`

``` purescript
newtype DescribeEventAggregatesRequest
  = DescribeEventAggregatesRequest { "Filter'" :: NullOrUndefined (EventFilter), "AggregateField'" :: EventAggregateField', "MaxResults'" :: NullOrUndefined (MaxResults'), "NextToken'" :: NullOrUndefined (NextToken') }
```

#### `DescribeEventAggregatesResponse`

``` purescript
newtype DescribeEventAggregatesResponse
  = DescribeEventAggregatesResponse { "EventAggregates'" :: NullOrUndefined (EventAggregateList), "NextToken'" :: NullOrUndefined (NextToken') }
```

#### `DescribeEventDetailsFailedSet`

``` purescript
newtype DescribeEventDetailsFailedSet
  = DescribeEventDetailsFailedSet (Array EventDetailsErrorItem)
```

#### `DescribeEventDetailsRequest`

``` purescript
newtype DescribeEventDetailsRequest
  = DescribeEventDetailsRequest { "EventArns'" :: EventArnList', "Locale'" :: NullOrUndefined (Locale') }
```

#### `DescribeEventDetailsResponse`

``` purescript
newtype DescribeEventDetailsResponse
  = DescribeEventDetailsResponse { "SuccessfulSet'" :: NullOrUndefined (DescribeEventDetailsSuccessfulSet), "FailedSet'" :: NullOrUndefined (DescribeEventDetailsFailedSet) }
```

#### `DescribeEventDetailsSuccessfulSet`

``` purescript
newtype DescribeEventDetailsSuccessfulSet
  = DescribeEventDetailsSuccessfulSet (Array EventDetails)
```

#### `DescribeEventTypesRequest`

``` purescript
newtype DescribeEventTypesRequest
  = DescribeEventTypesRequest { "Filter'" :: NullOrUndefined (EventTypeFilter), "Locale'" :: NullOrUndefined (Locale'), "NextToken'" :: NullOrUndefined (NextToken'), "MaxResults'" :: NullOrUndefined (MaxResults') }
```

#### `DescribeEventTypesResponse`

``` purescript
newtype DescribeEventTypesResponse
  = DescribeEventTypesResponse { "EventTypes'" :: NullOrUndefined (EventTypeList), "NextToken'" :: NullOrUndefined (NextToken') }
```

#### `DescribeEventsRequest`

``` purescript
newtype DescribeEventsRequest
  = DescribeEventsRequest { "Filter'" :: NullOrUndefined (EventFilter), "NextToken'" :: NullOrUndefined (NextToken'), "MaxResults'" :: NullOrUndefined (MaxResults'), "Locale'" :: NullOrUndefined (Locale') }
```

#### `DescribeEventsResponse`

``` purescript
newtype DescribeEventsResponse
  = DescribeEventsResponse { "Events'" :: NullOrUndefined (EventList), "NextToken'" :: NullOrUndefined (NextToken') }
```

#### `EntityAggregate`

``` purescript
newtype EntityAggregate
  = EntityAggregate { "EventArn'" :: NullOrUndefined (EventArn'), "Count'" :: NullOrUndefined (Count') }
```

<p>The number of entities that are affected by one or more events. Returned by the <a>DescribeEntityAggregates</a> operation.</p>

#### `EntityAggregateList`

``` purescript
newtype EntityAggregateList
  = EntityAggregateList (Array EntityAggregate)
```

#### `EntityFilter`

``` purescript
newtype EntityFilter
  = EntityFilter { "EventArns'" :: EventArnList', "EntityArns'" :: NullOrUndefined (EntityArnList'), "EntityValues'" :: NullOrUndefined (EntityValueList'), "LastUpdatedTimes'" :: NullOrUndefined (DateTimeRangeList'), "Tags'" :: NullOrUndefined (TagFilter'), "StatusCodes'" :: NullOrUndefined (EntityStatusCodeList') }
```

<p>The values to use to filter results from the <a>DescribeAffectedEntities</a> operation.</p>

#### `EntityList`

``` purescript
newtype EntityList
  = EntityList (Array AffectedEntity)
```

#### `Event`

``` purescript
newtype Event
  = Event { "Arn'" :: NullOrUndefined (EventArn'), "Service'" :: NullOrUndefined (Service'), "EventTypeCode'" :: NullOrUndefined (EventTypeCode'), "EventTypeCategory'" :: NullOrUndefined (EventTypeCategory'), "Region'" :: NullOrUndefined (Region'), "AvailabilityZone'" :: NullOrUndefined (AvailabilityZone'), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "LastUpdatedTime'" :: NullOrUndefined (Number), "StatusCode'" :: NullOrUndefined (EventStatusCode') }
```

<p>Summary information about an event, returned by the <a>DescribeEvents</a> operation. The <a>DescribeEventDetails</a> operation also returns this information, as well as the <a>EventDescription</a> and additional event metadata.</p>

#### `EventAggregate`

``` purescript
newtype EventAggregate
  = EventAggregate { "AggregateValue'" :: NullOrUndefined (AggregateValue'), "Count'" :: NullOrUndefined (Count') }
```

<p>The number of events of each issue type. Returned by the <a>DescribeEventAggregates</a> operation.</p>

#### `EventAggregateList`

``` purescript
newtype EventAggregateList
  = EventAggregateList (Array EventAggregate)
```

#### `EventArnsList`

``` purescript
newtype EventArnsList
  = EventArnsList (Array EventArn')
```

#### `EventDescription`

``` purescript
newtype EventDescription
  = EventDescription { "LatestDescription'" :: NullOrUndefined (EventDescription') }
```

<p>The detailed description of the event. Included in the information returned by the <a>DescribeEventDetails</a> operation.</p>

#### `EventDetails`

``` purescript
newtype EventDetails
  = EventDetails { "Event'" :: NullOrUndefined (Event), "EventDescription'" :: NullOrUndefined (EventDescription), "EventMetadata'" :: NullOrUndefined (EventMetadata') }
```

<p>Detailed information about an event. A combination of an <a>Event</a> object, an <a>EventDescription</a> object, and additional metadata about the event. Returned by the <a>DescribeEventDetails</a> operation.</p>

#### `EventDetailsErrorItem`

``` purescript
newtype EventDetailsErrorItem
  = EventDetailsErrorItem { "EventArn'" :: NullOrUndefined (EventArn'), "ErrorName'" :: NullOrUndefined (String), "ErrorMessage'" :: NullOrUndefined (String) }
```

<p>Error information returned when a <a>DescribeEventDetails</a> operation cannot find a specified event.</p>

#### `EventFilter`

``` purescript
newtype EventFilter
  = EventFilter { "EventArns'" :: NullOrUndefined (EventArnList'), "EventTypeCodes'" :: NullOrUndefined (EventTypeList'), "Services'" :: NullOrUndefined (ServiceList'), "Regions'" :: NullOrUndefined (RegionList'), "AvailabilityZones'" :: NullOrUndefined (AvailabilityZones'), "StartTimes'" :: NullOrUndefined (DateTimeRangeList'), "EndTimes'" :: NullOrUndefined (DateTimeRangeList'), "LastUpdatedTimes'" :: NullOrUndefined (DateTimeRangeList'), "EntityArns'" :: NullOrUndefined (EntityArnList'), "EntityValues'" :: NullOrUndefined (EntityValueList'), "EventTypeCategories'" :: NullOrUndefined (EventTypeCategoryList'), "Tags'" :: NullOrUndefined (TagFilter'), "EventStatusCodes'" :: NullOrUndefined (EventStatusCodeList') }
```

<p>The values to use to filter results from the <a>DescribeEvents</a> and <a>DescribeEventAggregates</a> operations.</p>

#### `EventList`

``` purescript
newtype EventList
  = EventList (Array Event)
```

#### `EventType`

``` purescript
newtype EventType
  = EventType { "Service'" :: NullOrUndefined (Service'), "Code'" :: NullOrUndefined (EventTypeCode'), "Category'" :: NullOrUndefined (EventTypeCategory') }
```

<p>Metadata about a type of event that is reported by AWS Health. Data consists of the category (for example, <code>issue</code>), the service (for example, <code>EC2</code>), and the event type code (for example, <code>AWS_EC2_SYSTEM_MAINTENANCE_EVENT</code>).</p>

#### `EventTypeCategoryList`

``` purescript
newtype EventTypeCategoryList
  = EventTypeCategoryList (Array EventTypeCategory')
```

#### `EventTypeCodeList`

``` purescript
newtype EventTypeCodeList
  = EventTypeCodeList (Array EventTypeCode')
```

#### `EventTypeFilter`

``` purescript
newtype EventTypeFilter
  = EventTypeFilter { "EventTypeCodes'" :: NullOrUndefined (EventTypeCodeList), "Services'" :: NullOrUndefined (ServiceList'), "EventTypeCategories'" :: NullOrUndefined (EventTypeCategoryList) }
```

<p>The values to use to filter results from the <a>DescribeEventTypes</a> operation.</p>

#### `EventTypeList`

``` purescript
newtype EventTypeList
  = EventTypeList (Array EventType)
```

#### `InvalidPaginationToken`

``` purescript
newtype InvalidPaginationToken
  = InvalidPaginationToken { "Message'" :: NullOrUndefined (String) }
```

<p>The specified pagination token (<code>nextToken</code>) is not valid.</p>

#### `UnsupportedLocale`

``` purescript
newtype UnsupportedLocale
  = UnsupportedLocale { "Message'" :: NullOrUndefined (String) }
```

<p>The specified locale is not supported.</p>

#### `AccountId'`

``` purescript
newtype AccountId'
  = AccountId' String
```

#### `AggregateValue'`

``` purescript
newtype AggregateValue'
  = AggregateValue' String
```

#### `AvailabilityZone'`

``` purescript
newtype AvailabilityZone'
  = AvailabilityZone' String
```

#### `AvailabilityZones'`

``` purescript
newtype AvailabilityZones'
  = AvailabilityZones' (Array AvailabilityZone')
```

#### `Count'`

``` purescript
newtype Count'
  = Count' Int
```

#### `DateTimeRangeList'`

``` purescript
newtype DateTimeRangeList'
  = DateTimeRangeList' (Array DateTimeRange)
```

#### `EntityArn'`

``` purescript
newtype EntityArn'
  = EntityArn' String
```

#### `EntityArnList'`

``` purescript
newtype EntityArnList'
  = EntityArnList' (Array EntityArn')
```

#### `EntityStatusCode'`

``` purescript
newtype EntityStatusCode'
  = EntityStatusCode' String
```

#### `EntityStatusCodeList'`

``` purescript
newtype EntityStatusCodeList'
  = EntityStatusCodeList' (Array EntityStatusCode')
```

#### `EntityValue'`

``` purescript
newtype EntityValue'
  = EntityValue' String
```

#### `EntityValueList'`

``` purescript
newtype EntityValueList'
  = EntityValueList' (Array EntityValue')
```

#### `EventAggregateField'`

``` purescript
newtype EventAggregateField'
  = EventAggregateField' String
```

#### `EventArn'`

``` purescript
newtype EventArn'
  = EventArn' String
```

#### `EventArnList'`

``` purescript
newtype EventArnList'
  = EventArnList' (Array EventArn')
```

#### `EventDescription'`

``` purescript
newtype EventDescription'
  = EventDescription' String
```

#### `EventMetadata'`

``` purescript
newtype EventMetadata'
  = EventMetadata' (Map MetadataKey' MetadataValue')
```

#### `EventStatusCode'`

``` purescript
newtype EventStatusCode'
  = EventStatusCode' String
```

#### `EventStatusCodeList'`

``` purescript
newtype EventStatusCodeList'
  = EventStatusCodeList' (Array EventStatusCode')
```

#### `EventType'`

``` purescript
newtype EventType'
  = EventType' String
```

#### `EventTypeCategory'`

``` purescript
newtype EventTypeCategory'
  = EventTypeCategory' String
```

#### `EventTypeCategoryList'`

``` purescript
newtype EventTypeCategoryList'
  = EventTypeCategoryList' (Array EventTypeCategory')
```

#### `EventTypeCode'`

``` purescript
newtype EventTypeCode'
  = EventTypeCode' String
```

#### `EventTypeList'`

``` purescript
newtype EventTypeList'
  = EventTypeList' (Array EventType')
```

#### `Locale'`

``` purescript
newtype Locale'
  = Locale' String
```

#### `MaxResults'`

``` purescript
newtype MaxResults'
  = MaxResults' Int
```

#### `MetadataKey'`

``` purescript
newtype MetadataKey'
  = MetadataKey' String
```

#### `MetadataValue'`

``` purescript
newtype MetadataValue'
  = MetadataValue' String
```

#### `NextToken'`

``` purescript
newtype NextToken'
  = NextToken' String
```

#### `Region'`

``` purescript
newtype Region'
  = Region' String
```

#### `RegionList'`

``` purescript
newtype RegionList'
  = RegionList' (Array Region')
```

#### `Service'`

``` purescript
newtype Service'
  = Service' String
```

#### `ServiceList'`

``` purescript
newtype ServiceList'
  = ServiceList' (Array Service')
```

#### `TagFilter'`

``` purescript
newtype TagFilter'
  = TagFilter' (Array TagSet')
```

#### `TagKey'`

``` purescript
newtype TagKey'
  = TagKey' String
```

#### `TagSet'`

``` purescript
newtype TagSet'
  = TagSet' (Map TagKey' TagValue')
```

#### `TagValue'`

``` purescript
newtype TagValue'
  = TagValue' String
```


