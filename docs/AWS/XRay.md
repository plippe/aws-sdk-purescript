## Module AWS.XRay

<p>AWS X-Ray provides APIs for managing debug traces and retrieving service maps and other data created by processing those traces.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchGetTraces`

``` purescript
batchGetTraces :: forall eff. BatchGetTracesRequest -> Aff (err :: RequestError | eff) BatchGetTracesResult
```

<p>Retrieves a list of traces specified by ID. Each trace is a collection of segment documents that originates from a single request. Use <code>GetTraceSummaries</code> to get a list of trace IDs.</p>

#### `getServiceGraph`

``` purescript
getServiceGraph :: forall eff. GetServiceGraphRequest -> Aff (err :: RequestError | eff) GetServiceGraphResult
```

<p>Retrieves a document that describes services that process incoming requests, and downstream services that they call as a result. Root services process incoming requests and make calls to downstream services. Root services are applications that use the AWS X-Ray SDK. Downstream services can be other applications, AWS resources, HTTP web APIs, or SQL databases.</p>

#### `getTraceGraph`

``` purescript
getTraceGraph :: forall eff. GetTraceGraphRequest -> Aff (err :: RequestError | eff) GetTraceGraphResult
```

<p>Retrieves a service graph for one or more specific trace IDs.</p>

#### `getTraceSummaries`

``` purescript
getTraceSummaries :: forall eff. GetTraceSummariesRequest -> Aff (err :: RequestError | eff) GetTraceSummariesResult
```

<p>Retrieves IDs and metadata for traces available for a specified time frame using an optional filter. To get the full traces, pass the trace IDs to <code>BatchGetTraces</code>.</p> <p>A filter expression can target traced requests that hit specific service nodes or edges, have errors, or come from a known user. For example, the following filter expression targets traces that pass through <code>api.example.com</code>:</p> <p> <code>service("api.example.com")</code> </p> <p>This filter expression finds traces that have an annotation named <code>account</code> with the value <code>12345</code>:</p> <p> <code>annotation.account = "12345"</code> </p> <p>For a full list of indexed fields and keywords that you can use in filter expressions, see <a href="http://docs.aws.amazon.com/xray/latest/devguide/xray-console-filters.html">Using Filter Expressions</a> in the <i>AWS X-Ray Developer Guide</i>.</p>

#### `putTelemetryRecords`

``` purescript
putTelemetryRecords :: forall eff. PutTelemetryRecordsRequest -> Aff (err :: RequestError | eff) PutTelemetryRecordsResult
```

<p>Used by the AWS X-Ray daemon to upload telemetry.</p>

#### `putTraceSegments`

``` purescript
putTraceSegments :: forall eff. PutTraceSegmentsRequest -> Aff (err :: RequestError | eff) PutTraceSegmentsResult
```

<p>Uploads segment documents to AWS X-Ray. The X-Ray SDK generates segment documents and sends them to the X-Ray daemon, which uploads them in batches. A segment document can be a completed segment, an in-progress segment, or an array of subsegments.</p> <p>Segments must include the following fields. For the full segment document schema, see <a href="http://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html">AWS X-Ray Segment Documents</a> in the <i>AWS X-Ray Developer Guide</i>.</p> <p class="title"> <b>Required Segment Document Fields</b> </p> <ul> <li> <p> <code>name</code> - The name of the service that handled the request.</p> </li> <li> <p> <code>id</code> - A 64-bit identifier for the segment, unique among segments in the same trace, in 16 hexadecimal digits.</p> </li> <li> <p> <code>trace_id</code> - A unique identifier that connects all segments and subsegments originating from a single client request.</p> </li> <li> <p> <code>start_time</code> - Time the segment or subsegment was created, in floating point seconds in epoch time, accurate to milliseconds. For example, <code>1480615200.010</code> or <code>1.480615200010E9</code>.</p> </li> <li> <p> <code>end_time</code> - Time the segment or subsegment was closed. For example, <code>1480615200.090</code> or <code>1.480615200090E9</code>. Specify either an <code>end_time</code> or <code>in_progress</code>.</p> </li> <li> <p> <code>in_progress</code> - Set to <code>true</code> instead of specifying an <code>end_time</code> to record that a segment has been started, but is not complete. Send an in progress segment when your application receives a request that will take a long time to serve, to trace the fact that the request was received. When the response is sent, send the complete segment to overwrite the in-progress segment.</p> </li> </ul> <p>A <code>trace_id</code> consists of three numbers separated by hyphens. For example, 1-58406520-a006649127e371903a2de979. This includes:</p> <p class="title"> <b>Trace ID Format</b> </p> <ul> <li> <p>The version number, i.e. <code>1</code>.</p> </li> <li> <p>The time of the original request, in Unix epoch time, in 8 hexadecimal digits. For example, 10:00AM December 2nd, 2016 PST in epoch time is <code>1480615200</code> seconds, or <code>58406520</code> in hexadecimal.</p> </li> <li> <p>A 96-bit identifier for the trace, globally unique, in 24 hexadecimal digits.</p> </li> </ul>

#### `Alias`

``` purescript
newtype Alias
  = Alias { "Name" :: NullOrUndefined (String), "Names" :: NullOrUndefined (AliasNames), "Type" :: NullOrUndefined (String) }
```

<p>An alias for an edge.</p>

#### `AliasList`

``` purescript
newtype AliasList
  = AliasList (Array Alias)
```

#### `AliasNames`

``` purescript
newtype AliasNames
  = AliasNames (Array String)
```

#### `AnnotationKey`

``` purescript
newtype AnnotationKey
  = AnnotationKey String
```

#### `AnnotationValue`

``` purescript
newtype AnnotationValue
  = AnnotationValue { "NumberValue" :: NullOrUndefined (NullableDouble), "BooleanValue" :: NullOrUndefined (NullableBoolean), "StringValue" :: NullOrUndefined (String) }
```

<p>Value of a segment annotation. Has one of three value types: Number, Boolean or String.</p>

#### `Annotations`

``` purescript
newtype Annotations
  = Annotations (Map AnnotationKey ValuesWithServiceIds)
```

#### `BackendConnectionErrors`

``` purescript
newtype BackendConnectionErrors
  = BackendConnectionErrors { "TimeoutCount" :: NullOrUndefined (NullableInteger), "ConnectionRefusedCount" :: NullOrUndefined (NullableInteger), "HTTPCode4XXCount" :: NullOrUndefined (NullableInteger), "HTTPCode5XXCount" :: NullOrUndefined (NullableInteger), "UnknownHostCount" :: NullOrUndefined (NullableInteger), "OtherCount" :: NullOrUndefined (NullableInteger) }
```

<p/>

#### `BatchGetTracesRequest`

``` purescript
newtype BatchGetTracesRequest
  = BatchGetTracesRequest { "TraceIds" :: TraceIdList, "NextToken" :: NullOrUndefined (String) }
```

#### `BatchGetTracesResult`

``` purescript
newtype BatchGetTracesResult
  = BatchGetTracesResult { "Traces" :: NullOrUndefined (TraceList), "UnprocessedTraceIds" :: NullOrUndefined (UnprocessedTraceIdList), "NextToken" :: NullOrUndefined (String) }
```

#### `EC2InstanceId`

``` purescript
newtype EC2InstanceId
  = EC2InstanceId String
```

#### `Edge`

``` purescript
newtype Edge
  = Edge { "ReferenceId" :: NullOrUndefined (NullableInteger), "StartTime" :: NullOrUndefined (Number), "EndTime" :: NullOrUndefined (Number), "SummaryStatistics" :: NullOrUndefined (EdgeStatistics), "ResponseTimeHistogram" :: NullOrUndefined (Histogram), "Aliases" :: NullOrUndefined (AliasList) }
```

<p>Information about a connection between two services.</p>

#### `EdgeList`

``` purescript
newtype EdgeList
  = EdgeList (Array Edge)
```

#### `EdgeStatistics`

``` purescript
newtype EdgeStatistics
  = EdgeStatistics { "OkCount" :: NullOrUndefined (NullableLong), "ErrorStatistics" :: NullOrUndefined (ErrorStatistics), "FaultStatistics" :: NullOrUndefined (FaultStatistics), "TotalCount" :: NullOrUndefined (NullableLong), "TotalResponseTime" :: NullOrUndefined (NullableDouble) }
```

<p>Response statistics for an edge.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `ErrorStatistics`

``` purescript
newtype ErrorStatistics
  = ErrorStatistics { "ThrottleCount" :: NullOrUndefined (NullableLong), "OtherCount" :: NullOrUndefined (NullableLong), "TotalCount" :: NullOrUndefined (NullableLong) }
```

<p>Information about requests that failed with a 4xx Client Error status code.</p>

#### `FaultStatistics`

``` purescript
newtype FaultStatistics
  = FaultStatistics { "OtherCount" :: NullOrUndefined (NullableLong), "TotalCount" :: NullOrUndefined (NullableLong) }
```

<p>Information about requests that failed with a 5xx Server Error status code.</p>

#### `FilterExpression`

``` purescript
newtype FilterExpression
  = FilterExpression String
```

#### `GetServiceGraphRequest`

``` purescript
newtype GetServiceGraphRequest
  = GetServiceGraphRequest { "StartTime" :: Number, "EndTime" :: Number, "NextToken" :: NullOrUndefined (String) }
```

#### `GetServiceGraphResult`

``` purescript
newtype GetServiceGraphResult
  = GetServiceGraphResult { "StartTime" :: NullOrUndefined (Number), "EndTime" :: NullOrUndefined (Number), "Services" :: NullOrUndefined (ServiceList), "NextToken" :: NullOrUndefined (String) }
```

#### `GetTraceGraphRequest`

``` purescript
newtype GetTraceGraphRequest
  = GetTraceGraphRequest { "TraceIds" :: TraceIdList, "NextToken" :: NullOrUndefined (String) }
```

#### `GetTraceGraphResult`

``` purescript
newtype GetTraceGraphResult
  = GetTraceGraphResult { "Services" :: NullOrUndefined (ServiceList), "NextToken" :: NullOrUndefined (String) }
```

#### `GetTraceSummariesRequest`

``` purescript
newtype GetTraceSummariesRequest
  = GetTraceSummariesRequest { "StartTime" :: Number, "EndTime" :: Number, "Sampling" :: NullOrUndefined (NullableBoolean), "FilterExpression" :: NullOrUndefined (FilterExpression), "NextToken" :: NullOrUndefined (String) }
```

#### `GetTraceSummariesResult`

``` purescript
newtype GetTraceSummariesResult
  = GetTraceSummariesResult { "TraceSummaries" :: NullOrUndefined (TraceSummaryList), "ApproximateTime" :: NullOrUndefined (Number), "TracesProcessedCount" :: NullOrUndefined (NullableLong), "NextToken" :: NullOrUndefined (String) }
```

#### `Histogram`

``` purescript
newtype Histogram
  = Histogram (Array HistogramEntry)
```

#### `HistogramEntry`

``` purescript
newtype HistogramEntry
  = HistogramEntry { "Value" :: NullOrUndefined (Number), "Count" :: NullOrUndefined (Int) }
```

<p>An entry in a histogram for a statistic. A histogram maps the range of observed values on the X axis, and the prevalence of each value on the Y axis.</p>

#### `Hostname`

``` purescript
newtype Hostname
  = Hostname String
```

#### `Http`

``` purescript
newtype Http
  = Http { "HttpURL" :: NullOrUndefined (String), "HttpStatus" :: NullOrUndefined (NullableInteger), "HttpMethod" :: NullOrUndefined (String), "UserAgent" :: NullOrUndefined (String), "ClientIp" :: NullOrUndefined (String) }
```

<p>Information about an HTTP request.</p>

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The request is missing required parameters or has invalid parameters.</p>

#### `NullableBoolean`

``` purescript
newtype NullableBoolean
  = NullableBoolean Boolean
```

#### `NullableDouble`

``` purescript
newtype NullableDouble
  = NullableDouble Number
```

#### `NullableInteger`

``` purescript
newtype NullableInteger
  = NullableInteger Int
```

#### `NullableLong`

``` purescript
newtype NullableLong
  = NullableLong Number
```

#### `PutTelemetryRecordsRequest`

``` purescript
newtype PutTelemetryRecordsRequest
  = PutTelemetryRecordsRequest { "TelemetryRecords" :: TelemetryRecordList, "EC2InstanceId" :: NullOrUndefined (EC2InstanceId), "Hostname" :: NullOrUndefined (Hostname), "ResourceARN" :: NullOrUndefined (ResourceARN) }
```

#### `PutTelemetryRecordsResult`

``` purescript
newtype PutTelemetryRecordsResult
  = PutTelemetryRecordsResult {  }
```

#### `PutTraceSegmentsRequest`

``` purescript
newtype PutTraceSegmentsRequest
  = PutTraceSegmentsRequest { "TraceSegmentDocuments" :: TraceSegmentDocumentList }
```

#### `PutTraceSegmentsResult`

``` purescript
newtype PutTraceSegmentsResult
  = PutTraceSegmentsResult { "UnprocessedTraceSegments" :: NullOrUndefined (UnprocessedTraceSegmentList) }
```

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

#### `Segment`

``` purescript
newtype Segment
  = Segment { "Id" :: NullOrUndefined (SegmentId), "Document" :: NullOrUndefined (SegmentDocument) }
```

<p>A segment from a trace that has been ingested by the X-Ray service. The segment can be compiled from documents uploaded with <a>PutTraceSegments</a>, or an <code>inferred</code> segment for a downstream service, generated from a subsegment sent by the service that called it.</p>

#### `SegmentDocument`

``` purescript
newtype SegmentDocument
  = SegmentDocument String
```

#### `SegmentId`

``` purescript
newtype SegmentId
  = SegmentId String
```

#### `SegmentList`

``` purescript
newtype SegmentList
  = SegmentList (Array Segment)
```

#### `Service`

``` purescript
newtype Service
  = Service { "ReferenceId" :: NullOrUndefined (NullableInteger), "Name" :: NullOrUndefined (String), "Names" :: NullOrUndefined (ServiceNames), "Root" :: NullOrUndefined (NullableBoolean), "AccountId" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String), "State" :: NullOrUndefined (String), "StartTime" :: NullOrUndefined (Number), "EndTime" :: NullOrUndefined (Number), "Edges" :: NullOrUndefined (EdgeList), "SummaryStatistics" :: NullOrUndefined (ServiceStatistics), "DurationHistogram" :: NullOrUndefined (Histogram), "ResponseTimeHistogram" :: NullOrUndefined (Histogram) }
```

<p>Information about an application that processed requests, users that made requests, or downstream services, resources and applications that an application used.</p>

#### `ServiceId`

``` purescript
newtype ServiceId
  = ServiceId { "Name" :: NullOrUndefined (String), "Names" :: NullOrUndefined (ServiceNames), "AccountId" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String) }
```

<p/>

#### `ServiceIds`

``` purescript
newtype ServiceIds
  = ServiceIds (Array ServiceId)
```

#### `ServiceList`

``` purescript
newtype ServiceList
  = ServiceList (Array Service)
```

#### `ServiceNames`

``` purescript
newtype ServiceNames
  = ServiceNames (Array String)
```

#### `ServiceStatistics`

``` purescript
newtype ServiceStatistics
  = ServiceStatistics { "OkCount" :: NullOrUndefined (NullableLong), "ErrorStatistics" :: NullOrUndefined (ErrorStatistics), "FaultStatistics" :: NullOrUndefined (FaultStatistics), "TotalCount" :: NullOrUndefined (NullableLong), "TotalResponseTime" :: NullOrUndefined (NullableDouble) }
```

<p>Response statistics for a service.</p>

#### `TelemetryRecord`

``` purescript
newtype TelemetryRecord
  = TelemetryRecord { "Number" :: Number, "SegmentsReceivedCount" :: NullOrUndefined (NullableInteger), "SegmentsSentCount" :: NullOrUndefined (NullableInteger), "SegmentsSpilloverCount" :: NullOrUndefined (NullableInteger), "SegmentsRejectedCount" :: NullOrUndefined (NullableInteger), "BackendConnectionErrors" :: NullOrUndefined (BackendConnectionErrors) }
```

<p/>

#### `TelemetryRecordList`

``` purescript
newtype TelemetryRecordList
  = TelemetryRecordList (Array TelemetryRecord)
```

#### `ThrottledException`

``` purescript
newtype ThrottledException
  = ThrottledException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The request exceeds the maximum number of requests per second.</p>

#### `Trace`

``` purescript
newtype Trace
  = Trace { "Id" :: NullOrUndefined (TraceId), "Duration" :: NullOrUndefined (NullableDouble), "Segments" :: NullOrUndefined (SegmentList) }
```

<p>A collection of segment documents with matching trace IDs.</p>

#### `TraceId`

``` purescript
newtype TraceId
  = TraceId String
```

#### `TraceIdList`

``` purescript
newtype TraceIdList
  = TraceIdList (Array TraceId)
```

#### `TraceList`

``` purescript
newtype TraceList
  = TraceList (Array Trace)
```

#### `TraceSegmentDocument`

``` purescript
newtype TraceSegmentDocument
  = TraceSegmentDocument String
```

#### `TraceSegmentDocumentList`

``` purescript
newtype TraceSegmentDocumentList
  = TraceSegmentDocumentList (Array TraceSegmentDocument)
```

#### `TraceSummary`

``` purescript
newtype TraceSummary
  = TraceSummary { "Id" :: NullOrUndefined (TraceId), "Duration" :: NullOrUndefined (NullableDouble), "ResponseTime" :: NullOrUndefined (NullableDouble), "HasFault" :: NullOrUndefined (NullableBoolean), "HasError" :: NullOrUndefined (NullableBoolean), "HasThrottle" :: NullOrUndefined (NullableBoolean), "IsPartial" :: NullOrUndefined (NullableBoolean), "Http" :: NullOrUndefined (Http), "Annotations" :: NullOrUndefined (Annotations), "Users" :: NullOrUndefined (TraceUsers), "ServiceIds" :: NullOrUndefined (ServiceIds) }
```

<p>Metadata generated from the segment documents in a trace.</p>

#### `TraceSummaryList`

``` purescript
newtype TraceSummaryList
  = TraceSummaryList (Array TraceSummary)
```

#### `TraceUser`

``` purescript
newtype TraceUser
  = TraceUser { "UserName" :: NullOrUndefined (String), "ServiceIds" :: NullOrUndefined (ServiceIds) }
```

<p>Information about a user recorded in segment documents.</p>

#### `TraceUsers`

``` purescript
newtype TraceUsers
  = TraceUsers (Array TraceUser)
```

#### `UnprocessedTraceIdList`

``` purescript
newtype UnprocessedTraceIdList
  = UnprocessedTraceIdList (Array TraceId)
```

#### `UnprocessedTraceSegment`

``` purescript
newtype UnprocessedTraceSegment
  = UnprocessedTraceSegment { "Id" :: NullOrUndefined (String), "ErrorCode" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String) }
```

<p>Information about a segment that failed processing.</p>

#### `UnprocessedTraceSegmentList`

``` purescript
newtype UnprocessedTraceSegmentList
  = UnprocessedTraceSegmentList (Array UnprocessedTraceSegment)
```

#### `ValueWithServiceIds`

``` purescript
newtype ValueWithServiceIds
  = ValueWithServiceIds { "AnnotationValue" :: NullOrUndefined (AnnotationValue), "ServiceIds" :: NullOrUndefined (ServiceIds) }
```

<p>Information about a segment annotation.</p>

#### `ValuesWithServiceIds`

``` purescript
newtype ValuesWithServiceIds
  = ValuesWithServiceIds (Array ValueWithServiceIds)
```


