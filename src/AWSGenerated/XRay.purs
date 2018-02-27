

-- | <p>AWS X-Ray provides APIs for managing debug traces and retrieving service maps and other data created by processing those traces.</p>
module AWS.XRay where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "XRay" :: String


-- | <p>Retrieves a list of traces specified by ID. Each trace is a collection of segment documents that originates from a single request. Use <code>GetTraceSummaries</code> to get a list of trace IDs.</p>
batchGetTraces :: forall eff. BatchGetTracesRequest -> Aff (err :: AWS.RequestError | eff) BatchGetTracesResult
batchGetTraces = AWS.request serviceName "BatchGetTraces" 


-- | <p>Retrieves a document that describes services that process incoming requests, and downstream services that they call as a result. Root services process incoming requests and make calls to downstream services. Root services are applications that use the AWS X-Ray SDK. Downstream services can be other applications, AWS resources, HTTP web APIs, or SQL databases.</p>
getServiceGraph :: forall eff. GetServiceGraphRequest -> Aff (err :: AWS.RequestError | eff) GetServiceGraphResult
getServiceGraph = AWS.request serviceName "GetServiceGraph" 


-- | <p>Retrieves a service graph for one or more specific trace IDs.</p>
getTraceGraph :: forall eff. GetTraceGraphRequest -> Aff (err :: AWS.RequestError | eff) GetTraceGraphResult
getTraceGraph = AWS.request serviceName "GetTraceGraph" 


-- | <p>Retrieves IDs and metadata for traces available for a specified time frame using an optional filter. To get the full traces, pass the trace IDs to <code>BatchGetTraces</code>.</p> <p>A filter expression can target traced requests that hit specific service nodes or edges, have errors, or come from a known user. For example, the following filter expression targets traces that pass through <code>api.example.com</code>:</p> <p> <code>service("api.example.com")</code> </p> <p>This filter expression finds traces that have an annotation named <code>account</code> with the value <code>12345</code>:</p> <p> <code>annotation.account = "12345"</code> </p> <p>For a full list of indexed fields and keywords that you can use in filter expressions, see <a href="http://docs.aws.amazon.com/xray/latest/devguide/xray-console-filters.html">Using Filter Expressions</a> in the <i>AWS X-Ray Developer Guide</i>.</p>
getTraceSummaries :: forall eff. GetTraceSummariesRequest -> Aff (err :: AWS.RequestError | eff) GetTraceSummariesResult
getTraceSummaries = AWS.request serviceName "GetTraceSummaries" 


-- | <p>Used by the AWS X-Ray daemon to upload telemetry.</p>
putTelemetryRecords :: forall eff. PutTelemetryRecordsRequest -> Aff (err :: AWS.RequestError | eff) PutTelemetryRecordsResult
putTelemetryRecords = AWS.request serviceName "PutTelemetryRecords" 


-- | <p>Uploads segment documents to AWS X-Ray. The X-Ray SDK generates segment documents and sends them to the X-Ray daemon, which uploads them in batches. A segment document can be a completed segment, an in-progress segment, or an array of subsegments.</p> <p>Segments must include the following fields. For the full segment document schema, see <a href="http://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html">AWS X-Ray Segment Documents</a> in the <i>AWS X-Ray Developer Guide</i>.</p> <p class="title"> <b>Required Segment Document Fields</b> </p> <ul> <li> <p> <code>name</code> - The name of the service that handled the request.</p> </li> <li> <p> <code>id</code> - A 64-bit identifier for the segment, unique among segments in the same trace, in 16 hexadecimal digits.</p> </li> <li> <p> <code>trace_id</code> - A unique identifier that connects all segments and subsegments originating from a single client request.</p> </li> <li> <p> <code>start_time</code> - Time the segment or subsegment was created, in floating point seconds in epoch time, accurate to milliseconds. For example, <code>1480615200.010</code> or <code>1.480615200010E9</code>.</p> </li> <li> <p> <code>end_time</code> - Time the segment or subsegment was closed. For example, <code>1480615200.090</code> or <code>1.480615200090E9</code>. Specify either an <code>end_time</code> or <code>in_progress</code>.</p> </li> <li> <p> <code>in_progress</code> - Set to <code>true</code> instead of specifying an <code>end_time</code> to record that a segment has been started, but is not complete. Send an in progress segment when your application receives a request that will take a long time to serve, to trace the fact that the request was received. When the response is sent, send the complete segment to overwrite the in-progress segment.</p> </li> </ul> <p>A <code>trace_id</code> consists of three numbers separated by hyphens. For example, 1-58406520-a006649127e371903a2de979. This includes:</p> <p class="title"> <b>Trace ID Format</b> </p> <ul> <li> <p>The version number, i.e. <code>1</code>.</p> </li> <li> <p>The time of the original request, in Unix epoch time, in 8 hexadecimal digits. For example, 10:00AM December 2nd, 2016 PST in epoch time is <code>1480615200</code> seconds, or <code>58406520</code> in hexadecimal.</p> </li> <li> <p>A 96-bit identifier for the trace, globally unique, in 24 hexadecimal digits.</p> </li> </ul>
putTraceSegments :: forall eff. PutTraceSegmentsRequest -> Aff (err :: AWS.RequestError | eff) PutTraceSegmentsResult
putTraceSegments = AWS.request serviceName "PutTraceSegments" 


-- | <p>An alias for an edge.</p>
newtype Alias = Alias 
  { "Name" :: NullOrUndefined (String)
  , "Names" :: NullOrUndefined (AliasNames)
  , "Type" :: NullOrUndefined (String)
  }
derive instance newtypeAlias :: Newtype Alias _


newtype AliasList = AliasList (Array Alias)
derive instance newtypeAliasList :: Newtype AliasList _


newtype AliasNames = AliasNames (Array String)
derive instance newtypeAliasNames :: Newtype AliasNames _


newtype AnnotationKey = AnnotationKey String
derive instance newtypeAnnotationKey :: Newtype AnnotationKey _


-- | <p>Value of a segment annotation. Has one of three value types: Number, Boolean or String.</p>
newtype AnnotationValue = AnnotationValue 
  { "NumberValue" :: NullOrUndefined (NullableDouble)
  , "BooleanValue" :: NullOrUndefined (NullableBoolean)
  , "StringValue" :: NullOrUndefined (String)
  }
derive instance newtypeAnnotationValue :: Newtype AnnotationValue _


newtype Annotations = Annotations (Map AnnotationKey ValuesWithServiceIds)
derive instance newtypeAnnotations :: Newtype Annotations _


-- | <p/>
newtype BackendConnectionErrors = BackendConnectionErrors 
  { "TimeoutCount" :: NullOrUndefined (NullableInteger)
  , "ConnectionRefusedCount" :: NullOrUndefined (NullableInteger)
  , "HTTPCode4XXCount" :: NullOrUndefined (NullableInteger)
  , "HTTPCode5XXCount" :: NullOrUndefined (NullableInteger)
  , "UnknownHostCount" :: NullOrUndefined (NullableInteger)
  , "OtherCount" :: NullOrUndefined (NullableInteger)
  }
derive instance newtypeBackendConnectionErrors :: Newtype BackendConnectionErrors _


newtype BatchGetTracesRequest = BatchGetTracesRequest 
  { "TraceIds" :: (TraceIdList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeBatchGetTracesRequest :: Newtype BatchGetTracesRequest _


newtype BatchGetTracesResult = BatchGetTracesResult 
  { "Traces" :: NullOrUndefined (TraceList)
  , "UnprocessedTraceIds" :: NullOrUndefined (UnprocessedTraceIdList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeBatchGetTracesResult :: Newtype BatchGetTracesResult _


newtype EC2InstanceId = EC2InstanceId String
derive instance newtypeEC2InstanceId :: Newtype EC2InstanceId _


-- | <p>Information about a connection between two services.</p>
newtype Edge = Edge 
  { "ReferenceId" :: NullOrUndefined (NullableInteger)
  , "StartTime" :: NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined (Number)
  , "SummaryStatistics" :: NullOrUndefined (EdgeStatistics)
  , "ResponseTimeHistogram" :: NullOrUndefined (Histogram)
  , "Aliases" :: NullOrUndefined (AliasList)
  }
derive instance newtypeEdge :: Newtype Edge _


newtype EdgeList = EdgeList (Array Edge)
derive instance newtypeEdgeList :: Newtype EdgeList _


-- | <p>Response statistics for an edge.</p>
newtype EdgeStatistics = EdgeStatistics 
  { "OkCount" :: NullOrUndefined (NullableLong)
  , "ErrorStatistics" :: NullOrUndefined (ErrorStatistics)
  , "FaultStatistics" :: NullOrUndefined (FaultStatistics)
  , "TotalCount" :: NullOrUndefined (NullableLong)
  , "TotalResponseTime" :: NullOrUndefined (NullableDouble)
  }
derive instance newtypeEdgeStatistics :: Newtype EdgeStatistics _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>Information about requests that failed with a 4xx Client Error status code.</p>
newtype ErrorStatistics = ErrorStatistics 
  { "ThrottleCount" :: NullOrUndefined (NullableLong)
  , "OtherCount" :: NullOrUndefined (NullableLong)
  , "TotalCount" :: NullOrUndefined (NullableLong)
  }
derive instance newtypeErrorStatistics :: Newtype ErrorStatistics _


-- | <p>Information about requests that failed with a 5xx Server Error status code.</p>
newtype FaultStatistics = FaultStatistics 
  { "OtherCount" :: NullOrUndefined (NullableLong)
  , "TotalCount" :: NullOrUndefined (NullableLong)
  }
derive instance newtypeFaultStatistics :: Newtype FaultStatistics _


newtype FilterExpression = FilterExpression String
derive instance newtypeFilterExpression :: Newtype FilterExpression _


newtype GetServiceGraphRequest = GetServiceGraphRequest 
  { "StartTime" :: (Number)
  , "EndTime" :: (Number)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetServiceGraphRequest :: Newtype GetServiceGraphRequest _


newtype GetServiceGraphResult = GetServiceGraphResult 
  { "StartTime" :: NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined (Number)
  , "Services" :: NullOrUndefined (ServiceList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetServiceGraphResult :: Newtype GetServiceGraphResult _


newtype GetTraceGraphRequest = GetTraceGraphRequest 
  { "TraceIds" :: (TraceIdList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetTraceGraphRequest :: Newtype GetTraceGraphRequest _


newtype GetTraceGraphResult = GetTraceGraphResult 
  { "Services" :: NullOrUndefined (ServiceList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetTraceGraphResult :: Newtype GetTraceGraphResult _


newtype GetTraceSummariesRequest = GetTraceSummariesRequest 
  { "StartTime" :: (Number)
  , "EndTime" :: (Number)
  , "Sampling" :: NullOrUndefined (NullableBoolean)
  , "FilterExpression" :: NullOrUndefined (FilterExpression)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetTraceSummariesRequest :: Newtype GetTraceSummariesRequest _


newtype GetTraceSummariesResult = GetTraceSummariesResult 
  { "TraceSummaries" :: NullOrUndefined (TraceSummaryList)
  , "ApproximateTime" :: NullOrUndefined (Number)
  , "TracesProcessedCount" :: NullOrUndefined (NullableLong)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetTraceSummariesResult :: Newtype GetTraceSummariesResult _


newtype Histogram = Histogram (Array HistogramEntry)
derive instance newtypeHistogram :: Newtype Histogram _


-- | <p>An entry in a histogram for a statistic. A histogram maps the range of observed values on the X axis, and the prevalence of each value on the Y axis.</p>
newtype HistogramEntry = HistogramEntry 
  { "Value" :: NullOrUndefined (Number)
  , "Count" :: NullOrUndefined (Int)
  }
derive instance newtypeHistogramEntry :: Newtype HistogramEntry _


newtype Hostname = Hostname String
derive instance newtypeHostname :: Newtype Hostname _


-- | <p>Information about an HTTP request.</p>
newtype Http = Http 
  { "HttpURL" :: NullOrUndefined (String)
  , "HttpStatus" :: NullOrUndefined (NullableInteger)
  , "HttpMethod" :: NullOrUndefined (String)
  , "UserAgent" :: NullOrUndefined (String)
  , "ClientIp" :: NullOrUndefined (String)
  }
derive instance newtypeHttp :: Newtype Http _


-- | <p>The request is missing required parameters or has invalid parameters.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


newtype NullableBoolean = NullableBoolean Boolean
derive instance newtypeNullableBoolean :: Newtype NullableBoolean _


newtype NullableDouble = NullableDouble Number
derive instance newtypeNullableDouble :: Newtype NullableDouble _


newtype NullableInteger = NullableInteger Int
derive instance newtypeNullableInteger :: Newtype NullableInteger _


newtype NullableLong = NullableLong Number
derive instance newtypeNullableLong :: Newtype NullableLong _


newtype PutTelemetryRecordsRequest = PutTelemetryRecordsRequest 
  { "TelemetryRecords" :: (TelemetryRecordList)
  , "EC2InstanceId" :: NullOrUndefined (EC2InstanceId)
  , "Hostname" :: NullOrUndefined (Hostname)
  , "ResourceARN" :: NullOrUndefined (ResourceARN)
  }
derive instance newtypePutTelemetryRecordsRequest :: Newtype PutTelemetryRecordsRequest _


newtype PutTelemetryRecordsResult = PutTelemetryRecordsResult 
  { 
  }
derive instance newtypePutTelemetryRecordsResult :: Newtype PutTelemetryRecordsResult _


newtype PutTraceSegmentsRequest = PutTraceSegmentsRequest 
  { "TraceSegmentDocuments" :: (TraceSegmentDocumentList)
  }
derive instance newtypePutTraceSegmentsRequest :: Newtype PutTraceSegmentsRequest _


newtype PutTraceSegmentsResult = PutTraceSegmentsResult 
  { "UnprocessedTraceSegments" :: NullOrUndefined (UnprocessedTraceSegmentList)
  }
derive instance newtypePutTraceSegmentsResult :: Newtype PutTraceSegmentsResult _


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _


-- | <p>A segment from a trace that has been ingested by the X-Ray service. The segment can be compiled from documents uploaded with <a>PutTraceSegments</a>, or an <code>inferred</code> segment for a downstream service, generated from a subsegment sent by the service that called it.</p>
newtype Segment = Segment 
  { "Id" :: NullOrUndefined (SegmentId)
  , "Document" :: NullOrUndefined (SegmentDocument)
  }
derive instance newtypeSegment :: Newtype Segment _


newtype SegmentDocument = SegmentDocument String
derive instance newtypeSegmentDocument :: Newtype SegmentDocument _


newtype SegmentId = SegmentId String
derive instance newtypeSegmentId :: Newtype SegmentId _


newtype SegmentList = SegmentList (Array Segment)
derive instance newtypeSegmentList :: Newtype SegmentList _


-- | <p>Information about an application that processed requests, users that made requests, or downstream services, resources and applications that an application used.</p>
newtype Service = Service 
  { "ReferenceId" :: NullOrUndefined (NullableInteger)
  , "Name" :: NullOrUndefined (String)
  , "Names" :: NullOrUndefined (ServiceNames)
  , "Root" :: NullOrUndefined (NullableBoolean)
  , "AccountId" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined (Number)
  , "Edges" :: NullOrUndefined (EdgeList)
  , "SummaryStatistics" :: NullOrUndefined (ServiceStatistics)
  , "DurationHistogram" :: NullOrUndefined (Histogram)
  , "ResponseTimeHistogram" :: NullOrUndefined (Histogram)
  }
derive instance newtypeService :: Newtype Service _


-- | <p/>
newtype ServiceId = ServiceId 
  { "Name" :: NullOrUndefined (String)
  , "Names" :: NullOrUndefined (ServiceNames)
  , "AccountId" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }
derive instance newtypeServiceId :: Newtype ServiceId _


newtype ServiceIds = ServiceIds (Array ServiceId)
derive instance newtypeServiceIds :: Newtype ServiceIds _


newtype ServiceList = ServiceList (Array Service)
derive instance newtypeServiceList :: Newtype ServiceList _


newtype ServiceNames = ServiceNames (Array String)
derive instance newtypeServiceNames :: Newtype ServiceNames _


-- | <p>Response statistics for a service.</p>
newtype ServiceStatistics = ServiceStatistics 
  { "OkCount" :: NullOrUndefined (NullableLong)
  , "ErrorStatistics" :: NullOrUndefined (ErrorStatistics)
  , "FaultStatistics" :: NullOrUndefined (FaultStatistics)
  , "TotalCount" :: NullOrUndefined (NullableLong)
  , "TotalResponseTime" :: NullOrUndefined (NullableDouble)
  }
derive instance newtypeServiceStatistics :: Newtype ServiceStatistics _


-- | <p/>
newtype TelemetryRecord = TelemetryRecord 
  { "Number" :: (Number)
  , "SegmentsReceivedCount" :: NullOrUndefined (NullableInteger)
  , "SegmentsSentCount" :: NullOrUndefined (NullableInteger)
  , "SegmentsSpilloverCount" :: NullOrUndefined (NullableInteger)
  , "SegmentsRejectedCount" :: NullOrUndefined (NullableInteger)
  , "BackendConnectionErrors" :: NullOrUndefined (BackendConnectionErrors)
  }
derive instance newtypeTelemetryRecord :: Newtype TelemetryRecord _


newtype TelemetryRecordList = TelemetryRecordList (Array TelemetryRecord)
derive instance newtypeTelemetryRecordList :: Newtype TelemetryRecordList _


-- | <p>The request exceeds the maximum number of requests per second.</p>
newtype ThrottledException = ThrottledException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeThrottledException :: Newtype ThrottledException _


-- | <p>A collection of segment documents with matching trace IDs.</p>
newtype Trace = Trace 
  { "Id" :: NullOrUndefined (TraceId)
  , "Duration" :: NullOrUndefined (NullableDouble)
  , "Segments" :: NullOrUndefined (SegmentList)
  }
derive instance newtypeTrace :: Newtype Trace _


newtype TraceId = TraceId String
derive instance newtypeTraceId :: Newtype TraceId _


newtype TraceIdList = TraceIdList (Array TraceId)
derive instance newtypeTraceIdList :: Newtype TraceIdList _


newtype TraceList = TraceList (Array Trace)
derive instance newtypeTraceList :: Newtype TraceList _


newtype TraceSegmentDocument = TraceSegmentDocument String
derive instance newtypeTraceSegmentDocument :: Newtype TraceSegmentDocument _


newtype TraceSegmentDocumentList = TraceSegmentDocumentList (Array TraceSegmentDocument)
derive instance newtypeTraceSegmentDocumentList :: Newtype TraceSegmentDocumentList _


-- | <p>Metadata generated from the segment documents in a trace.</p>
newtype TraceSummary = TraceSummary 
  { "Id" :: NullOrUndefined (TraceId)
  , "Duration" :: NullOrUndefined (NullableDouble)
  , "ResponseTime" :: NullOrUndefined (NullableDouble)
  , "HasFault" :: NullOrUndefined (NullableBoolean)
  , "HasError" :: NullOrUndefined (NullableBoolean)
  , "HasThrottle" :: NullOrUndefined (NullableBoolean)
  , "IsPartial" :: NullOrUndefined (NullableBoolean)
  , "Http" :: NullOrUndefined (Http)
  , "Annotations" :: NullOrUndefined (Annotations)
  , "Users" :: NullOrUndefined (TraceUsers)
  , "ServiceIds" :: NullOrUndefined (ServiceIds)
  }
derive instance newtypeTraceSummary :: Newtype TraceSummary _


newtype TraceSummaryList = TraceSummaryList (Array TraceSummary)
derive instance newtypeTraceSummaryList :: Newtype TraceSummaryList _


-- | <p>Information about a user recorded in segment documents.</p>
newtype TraceUser = TraceUser 
  { "UserName" :: NullOrUndefined (String)
  , "ServiceIds" :: NullOrUndefined (ServiceIds)
  }
derive instance newtypeTraceUser :: Newtype TraceUser _


newtype TraceUsers = TraceUsers (Array TraceUser)
derive instance newtypeTraceUsers :: Newtype TraceUsers _


newtype UnprocessedTraceIdList = UnprocessedTraceIdList (Array TraceId)
derive instance newtypeUnprocessedTraceIdList :: Newtype UnprocessedTraceIdList _


-- | <p>Information about a segment that failed processing.</p>
newtype UnprocessedTraceSegment = UnprocessedTraceSegment 
  { "Id" :: NullOrUndefined (String)
  , "ErrorCode" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnprocessedTraceSegment :: Newtype UnprocessedTraceSegment _


newtype UnprocessedTraceSegmentList = UnprocessedTraceSegmentList (Array UnprocessedTraceSegment)
derive instance newtypeUnprocessedTraceSegmentList :: Newtype UnprocessedTraceSegmentList _


-- | <p>Information about a segment annotation.</p>
newtype ValueWithServiceIds = ValueWithServiceIds 
  { "AnnotationValue" :: NullOrUndefined (AnnotationValue)
  , "ServiceIds" :: NullOrUndefined (ServiceIds)
  }
derive instance newtypeValueWithServiceIds :: Newtype ValueWithServiceIds _


newtype ValuesWithServiceIds = ValuesWithServiceIds (Array ValueWithServiceIds)
derive instance newtypeValuesWithServiceIds :: Newtype ValuesWithServiceIds _
