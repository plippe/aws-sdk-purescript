## Module AWS.CostExplorer

<p>The Cost Explorer API allows you to programmatically query your cost and usage data. You can query for aggregated data such as total monthly costs or total daily usage. You can also query for granular data, such as the number of daily write operations for DynamoDB database tables in your production environment. </p> <p>Service Endpoint</p> <p>The Cost Explorer API provides the following endpoint:</p> <ul> <li> <p>https://ce.us-east-1.amazonaws.com</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `getCostAndUsage`

``` purescript
getCostAndUsage :: forall eff. GetCostAndUsageRequest -> Aff (err :: RequestError | eff) GetCostAndUsageResponse
```

<p>Retrieve cost and usage metrics for your account. You can specify which cost and usage-related metric, such as <code>BlendedCosts</code> or <code>UsageQuantity</code>, that you want the request to return. You can also filter and group your data by various dimensions, such as <code>SERVICE</code> or <code>AZ</code>, in a specific time range. See the <code>GetDimensionValues</code> action for a complete list of the valid dimensions. Master accounts in an organization have access to all member accounts.</p>

#### `getDimensionValues`

``` purescript
getDimensionValues :: forall eff. GetDimensionValuesRequest -> Aff (err :: RequestError | eff) GetDimensionValuesResponse
```

<p>You can use <code>GetDimensionValues</code> to retrieve all available filter values for a specific filter over a period of time. You can search the dimension values for an arbitrary string. </p>

#### `getReservationCoverage`

``` purescript
getReservationCoverage :: forall eff. GetReservationCoverageRequest -> Aff (err :: RequestError | eff) GetReservationCoverageResponse
```

<p>Retrieve the reservation coverage for your account. An organization's master account has access to the associated member accounts. For any time period, you can filter data about reservation usage by the following dimensions. </p> <ul> <li> <p>AZ</p> </li> <li> <p>INSTANCE_TYPE</p> </li> <li> <p>LINKED_ACCOUNT</p> </li> <li> <p>PLATFORM</p> </li> <li> <p>REGION</p> </li> <li> <p>TENANCY</p> </li> </ul> <p>To determine valid values for a dimension, use the <code>GetDimensionValues</code> operation. </p>

#### `getReservationUtilization`

``` purescript
getReservationUtilization :: forall eff. GetReservationUtilizationRequest -> Aff (err :: RequestError | eff) GetReservationUtilizationResponse
```

<p>You can retrieve the Reservation utilization for your account. Master accounts in an organization have access to their associated member accounts. You can filter data by dimensions in a time period. You can use <code>GetDimensionValues</code> to determine the possible dimension values. Currently, you can group only by <code>SUBSCRIPTION_ID</code>. </p>

#### `getTags`

``` purescript
getTags :: forall eff. GetTagsRequest -> Aff (err :: RequestError | eff) GetTagsResponse
```

<p>You can query for available tag keys and tag values for a specified period. You can search the tag values for an arbitrary string. </p>

#### `AttributeType`

``` purescript
newtype AttributeType
  = AttributeType String
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

#### `Attributes`

``` purescript
newtype Attributes
  = Attributes (Map AttributeType AttributeValue)
```

#### `BillExpirationException`

``` purescript
newtype BillExpirationException
  = BillExpirationException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The requested report expired. Update the date interval and try again.</p>

#### `Context`

``` purescript
newtype Context
  = Context String
```

#### `Coverage`

``` purescript
newtype Coverage
  = Coverage { "CoverageHours" :: NullOrUndefined (CoverageHours) }
```

<p>The amount of instance usage that a reservation covered.</p>

#### `CoverageByTime`

``` purescript
newtype CoverageByTime
  = CoverageByTime { "TimePeriod" :: NullOrUndefined (DateInterval), "Groups" :: NullOrUndefined (ReservationCoverageGroups), "Total" :: NullOrUndefined (Coverage) }
```

<p>Reservation coverage, in hours.</p>

#### `CoverageHours`

``` purescript
newtype CoverageHours
  = CoverageHours { "OnDemandHours" :: NullOrUndefined (OnDemandHours), "ReservedHours" :: NullOrUndefined (ReservedHours), "TotalRunningHours" :: NullOrUndefined (TotalRunningHours), "CoverageHoursPercentage" :: NullOrUndefined (CoverageHoursPercentage) }
```

<p>How long a running instance either used a reservation or was On-Demand.</p>

#### `CoverageHoursPercentage`

``` purescript
newtype CoverageHoursPercentage
  = CoverageHoursPercentage String
```

#### `CoveragesByTime`

``` purescript
newtype CoveragesByTime
  = CoveragesByTime (Array CoverageByTime)
```

#### `DataUnavailableException`

``` purescript
newtype DataUnavailableException
  = DataUnavailableException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The requested data is unavailable.</p>

#### `DateInterval`

``` purescript
newtype DateInterval
  = DateInterval { "Start" :: YearMonthDay, "End" :: YearMonthDay }
```

<p>The time period that you want the usage and costs for. </p>

#### `Dimension`

``` purescript
newtype Dimension
  = Dimension String
```

#### `DimensionValues`

``` purescript
newtype DimensionValues
  = DimensionValues { "Key" :: NullOrUndefined (Dimension), "Values" :: NullOrUndefined (Values) }
```

<p>The metadata that you can use to filter and group your results. You can use <code>GetDimensionValues</code> to find specific values.</p>

#### `DimensionValuesWithAttributes`

``` purescript
newtype DimensionValuesWithAttributes
  = DimensionValuesWithAttributes { "Value" :: NullOrUndefined (Value), "Attributes" :: NullOrUndefined (Attributes) }
```

<p>The metadata of a specific type that you can use to filter and group your results. You can use <code>GetDimensionValues</code> to find specific values.</p>

#### `DimensionValuesWithAttributesList`

``` purescript
newtype DimensionValuesWithAttributesList
  = DimensionValuesWithAttributesList (Array DimensionValuesWithAttributes)
```

#### `Entity`

``` purescript
newtype Entity
  = Entity String
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `Estimated`

``` purescript
newtype Estimated
  = Estimated Boolean
```

#### `Expression`

``` purescript
newtype Expression
  = Expression { "Or" :: NullOrUndefined (Expressions), "And" :: NullOrUndefined (Expressions), "Not" :: NullOrUndefined (Expression), "Dimensions" :: NullOrUndefined (DimensionValues), "Tags" :: NullOrUndefined (TagValues) }
```

<p>Use <code>Expression</code> to filter by cost or by usage. There are two patterns: </p> <ul> <li> <p>Simple dimension values - You can set the dimension name and values for the filters that you plan to use. For example, you can filter for <code>INSTANCE_TYPE==m4.xlarge OR INSTANCE_TYPE==c4.large</code>. The <code>Expression</code> for that looks like this.</p> <p> <code>{ "Dimensions": { "Key": "INSTANCE_TYPE", "Values": [ "m4.xlarge", “c4.large” ] } }</code> </p> <p>The list of dimension values are OR'd together to retrieve cost or usage data. You can create <code>Expression</code> and <code>DimensionValues</code> objects using either <code>with*</code> methods or <code>set*</code> methods in multiple lines. </p> </li> <li> <p>Compound dimension values with logical operations - You can use multiple <code>Expression</code> types and the logical operators <code>AND/OR/NOT</code> to create a list of one or more <code>Expression</code> objects. This allows you to filter on more advanced options. For example, you can filter on <code>((INSTANCE_TYPE == m4.large OR INSTANCE_TYPE == m3.large) OR (TAG.Type == Type1)) AND (USAGE_TYPE != DataTransfer)</code>. The <code>Expression</code> for that looks like this.</p> <p> <code>{ "And": [ {"Or": [ {"Dimensions": { "Key": "INSTANCE_TYPE", "Values": [ "m4.x.large", "c4.large" ] }}, {"Tag": { "Key": "TagName", "Values": ["Value1"] } } ]}, {"Not": {"dimensions": { "Key": "USAGE_TYPE", "Values": ["DataTransfer"] }}} ] } </code> </p> <note> <p>Because each <code>Expression</code> can have only one operator, the service returns an error if more than one is specified. The following example shows an Expression object that will create an error.</p> </note> <p> <code> { "And": [ ... ], "DimensionValues": { "Dimension": "USAGE_TYPE", "Values": [ "DataTransfer" ] } } </code> </p> </li> </ul>

#### `Expressions`

``` purescript
newtype Expressions
  = Expressions (Array Expression)
```

#### `GetCostAndUsageRequest`

``` purescript
newtype GetCostAndUsageRequest
  = GetCostAndUsageRequest { "TimePeriod" :: NullOrUndefined (DateInterval), "Granularity" :: NullOrUndefined (Granularity), "Filter" :: NullOrUndefined (Expression), "Metrics" :: NullOrUndefined (MetricNames), "GroupBy" :: NullOrUndefined (GroupDefinitions), "NextPageToken" :: NullOrUndefined (NextPageToken) }
```

#### `GetCostAndUsageResponse`

``` purescript
newtype GetCostAndUsageResponse
  = GetCostAndUsageResponse { "NextPageToken" :: NullOrUndefined (NextPageToken), "GroupDefinitions" :: NullOrUndefined (GroupDefinitions), "ResultsByTime" :: NullOrUndefined (ResultsByTime) }
```

#### `GetDimensionValuesRequest`

``` purescript
newtype GetDimensionValuesRequest
  = GetDimensionValuesRequest { "SearchString" :: NullOrUndefined (SearchString), "TimePeriod" :: DateInterval, "Dimension" :: Dimension, "Context" :: NullOrUndefined (Context), "NextPageToken" :: NullOrUndefined (NextPageToken) }
```

#### `GetDimensionValuesResponse`

``` purescript
newtype GetDimensionValuesResponse
  = GetDimensionValuesResponse { "DimensionValues" :: DimensionValuesWithAttributesList, "ReturnSize" :: PageSize, "TotalSize" :: PageSize, "NextPageToken" :: NullOrUndefined (NextPageToken) }
```

#### `GetReservationCoverageRequest`

``` purescript
newtype GetReservationCoverageRequest
  = GetReservationCoverageRequest { "TimePeriod" :: DateInterval, "GroupBy" :: NullOrUndefined (GroupDefinitions), "Granularity" :: NullOrUndefined (Granularity), "Filter" :: NullOrUndefined (Expression), "NextPageToken" :: NullOrUndefined (NextPageToken) }
```

<p>You can query for how much of your instance usage was covered by a reservation.</p>

#### `GetReservationCoverageResponse`

``` purescript
newtype GetReservationCoverageResponse
  = GetReservationCoverageResponse { "CoveragesByTime" :: CoveragesByTime, "Total" :: NullOrUndefined (Coverage), "NextPageToken" :: NullOrUndefined (NextPageToken) }
```

#### `GetReservationUtilizationRequest`

``` purescript
newtype GetReservationUtilizationRequest
  = GetReservationUtilizationRequest { "TimePeriod" :: DateInterval, "GroupBy" :: NullOrUndefined (GroupDefinitions), "Granularity" :: NullOrUndefined (Granularity), "Filter" :: NullOrUndefined (Expression), "NextPageToken" :: NullOrUndefined (NextPageToken) }
```

#### `GetReservationUtilizationResponse`

``` purescript
newtype GetReservationUtilizationResponse
  = GetReservationUtilizationResponse { "UtilizationsByTime" :: UtilizationsByTime, "Total" :: NullOrUndefined (ReservationAggregates), "NextPageToken" :: NullOrUndefined (NextPageToken) }
```

#### `GetTagsRequest`

``` purescript
newtype GetTagsRequest
  = GetTagsRequest { "SearchString" :: NullOrUndefined (SearchString), "TimePeriod" :: DateInterval, "TagKey" :: NullOrUndefined (TagKey), "NextPageToken" :: NullOrUndefined (NextPageToken) }
```

#### `GetTagsResponse`

``` purescript
newtype GetTagsResponse
  = GetTagsResponse { "NextPageToken" :: NullOrUndefined (NextPageToken), "Tags" :: TagList, "ReturnSize" :: PageSize, "TotalSize" :: PageSize }
```

#### `Granularity`

``` purescript
newtype Granularity
  = Granularity String
```

#### `Group`

``` purescript
newtype Group
  = Group { "Keys" :: NullOrUndefined (Keys), "Metrics" :: NullOrUndefined (Metrics) }
```

<p>One level of grouped data within the results.</p>

#### `GroupDefinition`

``` purescript
newtype GroupDefinition
  = GroupDefinition { "Type" :: NullOrUndefined (GroupDefinitionType), "Key" :: NullOrUndefined (GroupDefinitionKey) }
```

<p>Represents a group when you specify a group by criteria, or in the response to a query with a specific grouping.</p>

#### `GroupDefinitionKey`

``` purescript
newtype GroupDefinitionKey
  = GroupDefinitionKey String
```

#### `GroupDefinitionType`

``` purescript
newtype GroupDefinitionType
  = GroupDefinitionType String
```

#### `GroupDefinitions`

``` purescript
newtype GroupDefinitions
  = GroupDefinitions (Array GroupDefinition)
```

#### `Groups`

``` purescript
newtype Groups
  = Groups (Array Group)
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The pagination token is invalid. Try again without a pagination token.</p>

#### `Key`

``` purescript
newtype Key
  = Key String
```

#### `Keys`

``` purescript
newtype Keys
  = Keys (Array Key)
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>You made too many calls in a short period of time. Try again later.</p>

#### `MetricAmount`

``` purescript
newtype MetricAmount
  = MetricAmount String
```

#### `MetricName`

``` purescript
newtype MetricName
  = MetricName String
```

#### `MetricNames`

``` purescript
newtype MetricNames
  = MetricNames (Array MetricName)
```

#### `MetricUnit`

``` purescript
newtype MetricUnit
  = MetricUnit String
```

#### `MetricValue`

``` purescript
newtype MetricValue
  = MetricValue { "Amount" :: NullOrUndefined (MetricAmount), "Unit''" :: NullOrUndefined (MetricUnit) }
```

<p>The aggregated value for a metric.</p>

#### `Metrics`

``` purescript
newtype Metrics
  = Metrics (Map MetricName MetricValue)
```

#### `NextPageToken`

``` purescript
newtype NextPageToken
  = NextPageToken String
```

#### `OnDemandHours`

``` purescript
newtype OnDemandHours
  = OnDemandHours String
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

#### `PurchasedHours`

``` purescript
newtype PurchasedHours
  = PurchasedHours String
```

#### `ReservationAggregates`

``` purescript
newtype ReservationAggregates
  = ReservationAggregates { "UtilizationPercentage" :: NullOrUndefined (UtilizationPercentage), "PurchasedHours" :: NullOrUndefined (PurchasedHours), "TotalActualHours" :: NullOrUndefined (TotalActualHours), "UnusedHours" :: NullOrUndefined (UnusedHours) }
```

<p>The aggregated numbers for your RI usage.</p>

#### `ReservationCoverageGroup`

``` purescript
newtype ReservationCoverageGroup
  = ReservationCoverageGroup { "Attributes" :: NullOrUndefined (Attributes), "Coverage" :: NullOrUndefined (Coverage) }
```

<p>A group of reservations that share a set of attributes.</p>

#### `ReservationCoverageGroups`

``` purescript
newtype ReservationCoverageGroups
  = ReservationCoverageGroups (Array ReservationCoverageGroup)
```

#### `ReservationGroupKey`

``` purescript
newtype ReservationGroupKey
  = ReservationGroupKey String
```

#### `ReservationGroupValue`

``` purescript
newtype ReservationGroupValue
  = ReservationGroupValue String
```

#### `ReservationUtilizationGroup`

``` purescript
newtype ReservationUtilizationGroup
  = ReservationUtilizationGroup { "Key" :: NullOrUndefined (ReservationGroupKey), "Value" :: NullOrUndefined (ReservationGroupValue), "Attributes" :: NullOrUndefined (Attributes), "Utilization" :: NullOrUndefined (ReservationAggregates) }
```

<p>A group of RIs that share a set of attributes.</p>

#### `ReservationUtilizationGroups`

``` purescript
newtype ReservationUtilizationGroups
  = ReservationUtilizationGroups (Array ReservationUtilizationGroup)
```

#### `ReservedHours`

``` purescript
newtype ReservedHours
  = ReservedHours String
```

#### `ResultByTime`

``` purescript
newtype ResultByTime
  = ResultByTime { "TimePeriod" :: NullOrUndefined (DateInterval), "Total" :: NullOrUndefined (Metrics), "Groups" :: NullOrUndefined (Groups), "Estimated" :: NullOrUndefined (Estimated) }
```

<p>The result that is associated with a time period.</p>

#### `ResultsByTime`

``` purescript
newtype ResultsByTime
  = ResultsByTime (Array ResultByTime)
```

#### `SearchString`

``` purescript
newtype SearchString
  = SearchString String
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Entity)
```

#### `TagValues`

``` purescript
newtype TagValues
  = TagValues { "Key" :: NullOrUndefined (TagKey), "Values" :: NullOrUndefined (Values) }
```

<p>The values that are available for a tag.</p>

#### `TotalActualHours`

``` purescript
newtype TotalActualHours
  = TotalActualHours String
```

#### `TotalRunningHours`

``` purescript
newtype TotalRunningHours
  = TotalRunningHours String
```

#### `UnusedHours`

``` purescript
newtype UnusedHours
  = UnusedHours String
```

#### `UtilizationByTime`

``` purescript
newtype UtilizationByTime
  = UtilizationByTime { "TimePeriod" :: NullOrUndefined (DateInterval), "Groups" :: NullOrUndefined (ReservationUtilizationGroups), "Total" :: NullOrUndefined (ReservationAggregates) }
```

<p>The amount of utilization, in hours.</p>

#### `UtilizationPercentage`

``` purescript
newtype UtilizationPercentage
  = UtilizationPercentage String
```

#### `UtilizationsByTime`

``` purescript
newtype UtilizationsByTime
  = UtilizationsByTime (Array UtilizationByTime)
```

#### `Value`

``` purescript
newtype Value
  = Value String
```

#### `Values`

``` purescript
newtype Values
  = Values (Array Value)
```

#### `YearMonthDay`

``` purescript
newtype YearMonthDay
  = YearMonthDay String
```


