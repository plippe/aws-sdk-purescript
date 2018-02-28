

-- | <p>AWS X-Ray provides APIs for managing debug traces and retrieving service maps and other data created by processing those traces.</p>
module AWS.XRay where

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

serviceName = "XRay" :: String


-- | <p>Retrieves a list of traces specified by ID. Each trace is a collection of segment documents that originates from a single request. Use <code>GetTraceSummaries</code> to get a list of trace IDs.</p>
batchGetTraces :: forall eff. BatchGetTracesRequest -> Aff (exception :: EXCEPTION | eff) BatchGetTracesResult
batchGetTraces = Request.request serviceName "batchGetTraces" 


-- | <p>Retrieves a document that describes services that process incoming requests, and downstream services that they call as a result. Root services process incoming requests and make calls to downstream services. Root services are applications that use the AWS X-Ray SDK. Downstream services can be other applications, AWS resources, HTTP web APIs, or SQL databases.</p>
getServiceGraph :: forall eff. GetServiceGraphRequest -> Aff (exception :: EXCEPTION | eff) GetServiceGraphResult
getServiceGraph = Request.request serviceName "getServiceGraph" 


-- | <p>Retrieves a service graph for one or more specific trace IDs.</p>
getTraceGraph :: forall eff. GetTraceGraphRequest -> Aff (exception :: EXCEPTION | eff) GetTraceGraphResult
getTraceGraph = Request.request serviceName "getTraceGraph" 


-- | <p>Retrieves IDs and metadata for traces available for a specified time frame using an optional filter. To get the full traces, pass the trace IDs to <code>BatchGetTraces</code>.</p> <p>A filter expression can target traced requests that hit specific service nodes or edges, have errors, or come from a known user. For example, the following filter expression targets traces that pass through <code>api.example.com</code>:</p> <p> <code>service("api.example.com")</code> </p> <p>This filter expression finds traces that have an annotation named <code>account</code> with the value <code>12345</code>:</p> <p> <code>annotation.account = "12345"</code> </p> <p>For a full list of indexed fields and keywords that you can use in filter expressions, see <a href="http://docs.aws.amazon.com/xray/latest/devguide/xray-console-filters.html">Using Filter Expressions</a> in the <i>AWS X-Ray Developer Guide</i>.</p>
getTraceSummaries :: forall eff. GetTraceSummariesRequest -> Aff (exception :: EXCEPTION | eff) GetTraceSummariesResult
getTraceSummaries = Request.request serviceName "getTraceSummaries" 


-- | <p>Used by the AWS X-Ray daemon to upload telemetry.</p>
putTelemetryRecords :: forall eff. PutTelemetryRecordsRequest -> Aff (exception :: EXCEPTION | eff) PutTelemetryRecordsResult
putTelemetryRecords = Request.request serviceName "putTelemetryRecords" 


-- | <p>Uploads segment documents to AWS X-Ray. The X-Ray SDK generates segment documents and sends them to the X-Ray daemon, which uploads them in batches. A segment document can be a completed segment, an in-progress segment, or an array of subsegments.</p> <p>Segments must include the following fields. For the full segment document schema, see <a href="http://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html">AWS X-Ray Segment Documents</a> in the <i>AWS X-Ray Developer Guide</i>.</p> <p class="title"> <b>Required Segment Document Fields</b> </p> <ul> <li> <p> <code>name</code> - The name of the service that handled the request.</p> </li> <li> <p> <code>id</code> - A 64-bit identifier for the segment, unique among segments in the same trace, in 16 hexadecimal digits.</p> </li> <li> <p> <code>trace_id</code> - A unique identifier that connects all segments and subsegments originating from a single client request.</p> </li> <li> <p> <code>start_time</code> - Time the segment or subsegment was created, in floating point seconds in epoch time, accurate to milliseconds. For example, <code>1480615200.010</code> or <code>1.480615200010E9</code>.</p> </li> <li> <p> <code>end_time</code> - Time the segment or subsegment was closed. For example, <code>1480615200.090</code> or <code>1.480615200090E9</code>. Specify either an <code>end_time</code> or <code>in_progress</code>.</p> </li> <li> <p> <code>in_progress</code> - Set to <code>true</code> instead of specifying an <code>end_time</code> to record that a segment has been started, but is not complete. Send an in progress segment when your application receives a request that will take a long time to serve, to trace the fact that the request was received. When the response is sent, send the complete segment to overwrite the in-progress segment.</p> </li> </ul> <p>A <code>trace_id</code> consists of three numbers separated by hyphens. For example, 1-58406520-a006649127e371903a2de979. This includes:</p> <p class="title"> <b>Trace ID Format</b> </p> <ul> <li> <p>The version number, i.e. <code>1</code>.</p> </li> <li> <p>The time of the original request, in Unix epoch time, in 8 hexadecimal digits. For example, 10:00AM December 2nd, 2016 PST in epoch time is <code>1480615200</code> seconds, or <code>58406520</code> in hexadecimal.</p> </li> <li> <p>A 96-bit identifier for the trace, globally unique, in 24 hexadecimal digits.</p> </li> </ul>
putTraceSegments :: forall eff. PutTraceSegmentsRequest -> Aff (exception :: EXCEPTION | eff) PutTraceSegmentsResult
putTraceSegments = Request.request serviceName "putTraceSegments" 


-- | <p>An alias for an edge.</p>
newtype Alias = Alias 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Names" :: NullOrUndefined.NullOrUndefined (AliasNames)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAlias :: Newtype Alias _
derive instance repGenericAlias :: Generic Alias _
instance showAlias :: Show Alias where
  show = genericShow
instance decodeAlias :: Decode Alias where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlias :: Encode Alias where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasList = AliasList (Array Alias)
derive instance newtypeAliasList :: Newtype AliasList _
derive instance repGenericAliasList :: Generic AliasList _
instance showAliasList :: Show AliasList where
  show = genericShow
instance decodeAliasList :: Decode AliasList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasList :: Encode AliasList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasNames = AliasNames (Array String)
derive instance newtypeAliasNames :: Newtype AliasNames _
derive instance repGenericAliasNames :: Generic AliasNames _
instance showAliasNames :: Show AliasNames where
  show = genericShow
instance decodeAliasNames :: Decode AliasNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasNames :: Encode AliasNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AnnotationKey = AnnotationKey String
derive instance newtypeAnnotationKey :: Newtype AnnotationKey _
derive instance repGenericAnnotationKey :: Generic AnnotationKey _
instance showAnnotationKey :: Show AnnotationKey where
  show = genericShow
instance decodeAnnotationKey :: Decode AnnotationKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnnotationKey :: Encode AnnotationKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Value of a segment annotation. Has one of three value types: Number, Boolean or String.</p>
newtype AnnotationValue = AnnotationValue 
  { "NumberValue" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "BooleanValue" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "StringValue" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAnnotationValue :: Newtype AnnotationValue _
derive instance repGenericAnnotationValue :: Generic AnnotationValue _
instance showAnnotationValue :: Show AnnotationValue where
  show = genericShow
instance decodeAnnotationValue :: Decode AnnotationValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnnotationValue :: Encode AnnotationValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Annotations = Annotations (StrMap.StrMap ValuesWithServiceIds)
derive instance newtypeAnnotations :: Newtype Annotations _
derive instance repGenericAnnotations :: Generic Annotations _
instance showAnnotations :: Show Annotations where
  show = genericShow
instance decodeAnnotations :: Decode Annotations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAnnotations :: Encode Annotations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype BackendConnectionErrors = BackendConnectionErrors 
  { "TimeoutCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "ConnectionRefusedCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "HTTPCode4XXCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "HTTPCode5XXCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "UnknownHostCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "OtherCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeBackendConnectionErrors :: Newtype BackendConnectionErrors _
derive instance repGenericBackendConnectionErrors :: Generic BackendConnectionErrors _
instance showBackendConnectionErrors :: Show BackendConnectionErrors where
  show = genericShow
instance decodeBackendConnectionErrors :: Decode BackendConnectionErrors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackendConnectionErrors :: Encode BackendConnectionErrors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetTracesRequest = BatchGetTracesRequest 
  { "TraceIds" :: (TraceIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBatchGetTracesRequest :: Newtype BatchGetTracesRequest _
derive instance repGenericBatchGetTracesRequest :: Generic BatchGetTracesRequest _
instance showBatchGetTracesRequest :: Show BatchGetTracesRequest where
  show = genericShow
instance decodeBatchGetTracesRequest :: Decode BatchGetTracesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetTracesRequest :: Encode BatchGetTracesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetTracesResult = BatchGetTracesResult 
  { "Traces" :: NullOrUndefined.NullOrUndefined (TraceList)
  , "UnprocessedTraceIds" :: NullOrUndefined.NullOrUndefined (UnprocessedTraceIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBatchGetTracesResult :: Newtype BatchGetTracesResult _
derive instance repGenericBatchGetTracesResult :: Generic BatchGetTracesResult _
instance showBatchGetTracesResult :: Show BatchGetTracesResult where
  show = genericShow
instance decodeBatchGetTracesResult :: Decode BatchGetTracesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetTracesResult :: Encode BatchGetTracesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EC2InstanceId = EC2InstanceId String
derive instance newtypeEC2InstanceId :: Newtype EC2InstanceId _
derive instance repGenericEC2InstanceId :: Generic EC2InstanceId _
instance showEC2InstanceId :: Show EC2InstanceId where
  show = genericShow
instance decodeEC2InstanceId :: Decode EC2InstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEC2InstanceId :: Encode EC2InstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a connection between two services.</p>
newtype Edge = Edge 
  { "ReferenceId" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "SummaryStatistics" :: NullOrUndefined.NullOrUndefined (EdgeStatistics)
  , "ResponseTimeHistogram" :: NullOrUndefined.NullOrUndefined (Histogram)
  , "Aliases" :: NullOrUndefined.NullOrUndefined (AliasList)
  }
derive instance newtypeEdge :: Newtype Edge _
derive instance repGenericEdge :: Generic Edge _
instance showEdge :: Show Edge where
  show = genericShow
instance decodeEdge :: Decode Edge where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEdge :: Encode Edge where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EdgeList = EdgeList (Array Edge)
derive instance newtypeEdgeList :: Newtype EdgeList _
derive instance repGenericEdgeList :: Generic EdgeList _
instance showEdgeList :: Show EdgeList where
  show = genericShow
instance decodeEdgeList :: Decode EdgeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEdgeList :: Encode EdgeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response statistics for an edge.</p>
newtype EdgeStatistics = EdgeStatistics 
  { "OkCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "ErrorStatistics" :: NullOrUndefined.NullOrUndefined (ErrorStatistics)
  , "FaultStatistics" :: NullOrUndefined.NullOrUndefined (FaultStatistics)
  , "TotalCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "TotalResponseTime" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  }
derive instance newtypeEdgeStatistics :: Newtype EdgeStatistics _
derive instance repGenericEdgeStatistics :: Generic EdgeStatistics _
instance showEdgeStatistics :: Show EdgeStatistics where
  show = genericShow
instance decodeEdgeStatistics :: Decode EdgeStatistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEdgeStatistics :: Encode EdgeStatistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about requests that failed with a 4xx Client Error status code.</p>
newtype ErrorStatistics = ErrorStatistics 
  { "ThrottleCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "OtherCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "TotalCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  }
derive instance newtypeErrorStatistics :: Newtype ErrorStatistics _
derive instance repGenericErrorStatistics :: Generic ErrorStatistics _
instance showErrorStatistics :: Show ErrorStatistics where
  show = genericShow
instance decodeErrorStatistics :: Decode ErrorStatistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorStatistics :: Encode ErrorStatistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about requests that failed with a 5xx Server Error status code.</p>
newtype FaultStatistics = FaultStatistics 
  { "OtherCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "TotalCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  }
derive instance newtypeFaultStatistics :: Newtype FaultStatistics _
derive instance repGenericFaultStatistics :: Generic FaultStatistics _
instance showFaultStatistics :: Show FaultStatistics where
  show = genericShow
instance decodeFaultStatistics :: Decode FaultStatistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaultStatistics :: Encode FaultStatistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterExpression = FilterExpression String
derive instance newtypeFilterExpression :: Newtype FilterExpression _
derive instance repGenericFilterExpression :: Generic FilterExpression _
instance showFilterExpression :: Show FilterExpression where
  show = genericShow
instance decodeFilterExpression :: Decode FilterExpression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterExpression :: Encode FilterExpression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetServiceGraphRequest = GetServiceGraphRequest 
  { "StartTime" :: (Number)
  , "EndTime" :: (Number)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetServiceGraphRequest :: Newtype GetServiceGraphRequest _
derive instance repGenericGetServiceGraphRequest :: Generic GetServiceGraphRequest _
instance showGetServiceGraphRequest :: Show GetServiceGraphRequest where
  show = genericShow
instance decodeGetServiceGraphRequest :: Decode GetServiceGraphRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetServiceGraphRequest :: Encode GetServiceGraphRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetServiceGraphResult = GetServiceGraphResult 
  { "StartTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Services" :: NullOrUndefined.NullOrUndefined (ServiceList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetServiceGraphResult :: Newtype GetServiceGraphResult _
derive instance repGenericGetServiceGraphResult :: Generic GetServiceGraphResult _
instance showGetServiceGraphResult :: Show GetServiceGraphResult where
  show = genericShow
instance decodeGetServiceGraphResult :: Decode GetServiceGraphResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetServiceGraphResult :: Encode GetServiceGraphResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTraceGraphRequest = GetTraceGraphRequest 
  { "TraceIds" :: (TraceIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetTraceGraphRequest :: Newtype GetTraceGraphRequest _
derive instance repGenericGetTraceGraphRequest :: Generic GetTraceGraphRequest _
instance showGetTraceGraphRequest :: Show GetTraceGraphRequest where
  show = genericShow
instance decodeGetTraceGraphRequest :: Decode GetTraceGraphRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTraceGraphRequest :: Encode GetTraceGraphRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTraceGraphResult = GetTraceGraphResult 
  { "Services" :: NullOrUndefined.NullOrUndefined (ServiceList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetTraceGraphResult :: Newtype GetTraceGraphResult _
derive instance repGenericGetTraceGraphResult :: Generic GetTraceGraphResult _
instance showGetTraceGraphResult :: Show GetTraceGraphResult where
  show = genericShow
instance decodeGetTraceGraphResult :: Decode GetTraceGraphResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTraceGraphResult :: Encode GetTraceGraphResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTraceSummariesRequest = GetTraceSummariesRequest 
  { "StartTime" :: (Number)
  , "EndTime" :: (Number)
  , "Sampling" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "FilterExpression" :: NullOrUndefined.NullOrUndefined (FilterExpression)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetTraceSummariesRequest :: Newtype GetTraceSummariesRequest _
derive instance repGenericGetTraceSummariesRequest :: Generic GetTraceSummariesRequest _
instance showGetTraceSummariesRequest :: Show GetTraceSummariesRequest where
  show = genericShow
instance decodeGetTraceSummariesRequest :: Decode GetTraceSummariesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTraceSummariesRequest :: Encode GetTraceSummariesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTraceSummariesResult = GetTraceSummariesResult 
  { "TraceSummaries" :: NullOrUndefined.NullOrUndefined (TraceSummaryList)
  , "ApproximateTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "TracesProcessedCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetTraceSummariesResult :: Newtype GetTraceSummariesResult _
derive instance repGenericGetTraceSummariesResult :: Generic GetTraceSummariesResult _
instance showGetTraceSummariesResult :: Show GetTraceSummariesResult where
  show = genericShow
instance decodeGetTraceSummariesResult :: Decode GetTraceSummariesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTraceSummariesResult :: Encode GetTraceSummariesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Histogram = Histogram (Array HistogramEntry)
derive instance newtypeHistogram :: Newtype Histogram _
derive instance repGenericHistogram :: Generic Histogram _
instance showHistogram :: Show Histogram where
  show = genericShow
instance decodeHistogram :: Decode Histogram where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistogram :: Encode Histogram where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An entry in a histogram for a statistic. A histogram maps the range of observed values on the X axis, and the prevalence of each value on the Y axis.</p>
newtype HistogramEntry = HistogramEntry 
  { "Value" :: NullOrUndefined.NullOrUndefined (Number)
  , "Count" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeHistogramEntry :: Newtype HistogramEntry _
derive instance repGenericHistogramEntry :: Generic HistogramEntry _
instance showHistogramEntry :: Show HistogramEntry where
  show = genericShow
instance decodeHistogramEntry :: Decode HistogramEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistogramEntry :: Encode HistogramEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Hostname = Hostname String
derive instance newtypeHostname :: Newtype Hostname _
derive instance repGenericHostname :: Generic Hostname _
instance showHostname :: Show Hostname where
  show = genericShow
instance decodeHostname :: Decode Hostname where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostname :: Encode Hostname where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an HTTP request.</p>
newtype Http = Http 
  { "HttpURL" :: NullOrUndefined.NullOrUndefined (String)
  , "HttpStatus" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "HttpMethod" :: NullOrUndefined.NullOrUndefined (String)
  , "UserAgent" :: NullOrUndefined.NullOrUndefined (String)
  , "ClientIp" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHttp :: Newtype Http _
derive instance repGenericHttp :: Generic Http _
instance showHttp :: Show Http where
  show = genericShow
instance decodeHttp :: Decode Http where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttp :: Encode Http where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request is missing required parameters or has invalid parameters.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _
derive instance repGenericInvalidRequestException :: Generic InvalidRequestException _
instance showInvalidRequestException :: Show InvalidRequestException where
  show = genericShow
instance decodeInvalidRequestException :: Decode InvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequestException :: Encode InvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableBoolean = NullableBoolean Boolean
derive instance newtypeNullableBoolean :: Newtype NullableBoolean _
derive instance repGenericNullableBoolean :: Generic NullableBoolean _
instance showNullableBoolean :: Show NullableBoolean where
  show = genericShow
instance decodeNullableBoolean :: Decode NullableBoolean where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableBoolean :: Encode NullableBoolean where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableDouble = NullableDouble Number
derive instance newtypeNullableDouble :: Newtype NullableDouble _
derive instance repGenericNullableDouble :: Generic NullableDouble _
instance showNullableDouble :: Show NullableDouble where
  show = genericShow
instance decodeNullableDouble :: Decode NullableDouble where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableDouble :: Encode NullableDouble where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableInteger = NullableInteger Int
derive instance newtypeNullableInteger :: Newtype NullableInteger _
derive instance repGenericNullableInteger :: Generic NullableInteger _
instance showNullableInteger :: Show NullableInteger where
  show = genericShow
instance decodeNullableInteger :: Decode NullableInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableInteger :: Encode NullableInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableLong = NullableLong Number
derive instance newtypeNullableLong :: Newtype NullableLong _
derive instance repGenericNullableLong :: Generic NullableLong _
instance showNullableLong :: Show NullableLong where
  show = genericShow
instance decodeNullableLong :: Decode NullableLong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableLong :: Encode NullableLong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutTelemetryRecordsRequest = PutTelemetryRecordsRequest 
  { "TelemetryRecords" :: (TelemetryRecordList)
  , "EC2InstanceId" :: NullOrUndefined.NullOrUndefined (EC2InstanceId)
  , "Hostname" :: NullOrUndefined.NullOrUndefined (Hostname)
  , "ResourceARN" :: NullOrUndefined.NullOrUndefined (ResourceARN)
  }
derive instance newtypePutTelemetryRecordsRequest :: Newtype PutTelemetryRecordsRequest _
derive instance repGenericPutTelemetryRecordsRequest :: Generic PutTelemetryRecordsRequest _
instance showPutTelemetryRecordsRequest :: Show PutTelemetryRecordsRequest where
  show = genericShow
instance decodePutTelemetryRecordsRequest :: Decode PutTelemetryRecordsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutTelemetryRecordsRequest :: Encode PutTelemetryRecordsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutTelemetryRecordsResult = PutTelemetryRecordsResult Types.NoArguments
derive instance newtypePutTelemetryRecordsResult :: Newtype PutTelemetryRecordsResult _
derive instance repGenericPutTelemetryRecordsResult :: Generic PutTelemetryRecordsResult _
instance showPutTelemetryRecordsResult :: Show PutTelemetryRecordsResult where
  show = genericShow
instance decodePutTelemetryRecordsResult :: Decode PutTelemetryRecordsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutTelemetryRecordsResult :: Encode PutTelemetryRecordsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutTraceSegmentsRequest = PutTraceSegmentsRequest 
  { "TraceSegmentDocuments" :: (TraceSegmentDocumentList)
  }
derive instance newtypePutTraceSegmentsRequest :: Newtype PutTraceSegmentsRequest _
derive instance repGenericPutTraceSegmentsRequest :: Generic PutTraceSegmentsRequest _
instance showPutTraceSegmentsRequest :: Show PutTraceSegmentsRequest where
  show = genericShow
instance decodePutTraceSegmentsRequest :: Decode PutTraceSegmentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutTraceSegmentsRequest :: Encode PutTraceSegmentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutTraceSegmentsResult = PutTraceSegmentsResult 
  { "UnprocessedTraceSegments" :: NullOrUndefined.NullOrUndefined (UnprocessedTraceSegmentList)
  }
derive instance newtypePutTraceSegmentsResult :: Newtype PutTraceSegmentsResult _
derive instance repGenericPutTraceSegmentsResult :: Generic PutTraceSegmentsResult _
instance showPutTraceSegmentsResult :: Show PutTraceSegmentsResult where
  show = genericShow
instance decodePutTraceSegmentsResult :: Decode PutTraceSegmentsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutTraceSegmentsResult :: Encode PutTraceSegmentsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _
derive instance repGenericResourceARN :: Generic ResourceARN _
instance showResourceARN :: Show ResourceARN where
  show = genericShow
instance decodeResourceARN :: Decode ResourceARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceARN :: Encode ResourceARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A segment from a trace that has been ingested by the X-Ray service. The segment can be compiled from documents uploaded with <a>PutTraceSegments</a>, or an <code>inferred</code> segment for a downstream service, generated from a subsegment sent by the service that called it.</p>
newtype Segment = Segment 
  { "Id" :: NullOrUndefined.NullOrUndefined (SegmentId)
  , "Document" :: NullOrUndefined.NullOrUndefined (SegmentDocument)
  }
derive instance newtypeSegment :: Newtype Segment _
derive instance repGenericSegment :: Generic Segment _
instance showSegment :: Show Segment where
  show = genericShow
instance decodeSegment :: Decode Segment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegment :: Encode Segment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SegmentDocument = SegmentDocument String
derive instance newtypeSegmentDocument :: Newtype SegmentDocument _
derive instance repGenericSegmentDocument :: Generic SegmentDocument _
instance showSegmentDocument :: Show SegmentDocument where
  show = genericShow
instance decodeSegmentDocument :: Decode SegmentDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentDocument :: Encode SegmentDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SegmentId = SegmentId String
derive instance newtypeSegmentId :: Newtype SegmentId _
derive instance repGenericSegmentId :: Generic SegmentId _
instance showSegmentId :: Show SegmentId where
  show = genericShow
instance decodeSegmentId :: Decode SegmentId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentId :: Encode SegmentId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SegmentList = SegmentList (Array Segment)
derive instance newtypeSegmentList :: Newtype SegmentList _
derive instance repGenericSegmentList :: Generic SegmentList _
instance showSegmentList :: Show SegmentList where
  show = genericShow
instance decodeSegmentList :: Decode SegmentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegmentList :: Encode SegmentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an application that processed requests, users that made requests, or downstream services, resources and applications that an application used.</p>
newtype Service = Service 
  { "ReferenceId" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Names" :: NullOrUndefined.NullOrUndefined (ServiceNames)
  , "Root" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "AccountId" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  , "State" :: NullOrUndefined.NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Edges" :: NullOrUndefined.NullOrUndefined (EdgeList)
  , "SummaryStatistics" :: NullOrUndefined.NullOrUndefined (ServiceStatistics)
  , "DurationHistogram" :: NullOrUndefined.NullOrUndefined (Histogram)
  , "ResponseTimeHistogram" :: NullOrUndefined.NullOrUndefined (Histogram)
  }
derive instance newtypeService :: Newtype Service _
derive instance repGenericService :: Generic Service _
instance showService :: Show Service where
  show = genericShow
instance decodeService :: Decode Service where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeService :: Encode Service where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ServiceId = ServiceId 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Names" :: NullOrUndefined.NullOrUndefined (ServiceNames)
  , "AccountId" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServiceId :: Newtype ServiceId _
derive instance repGenericServiceId :: Generic ServiceId _
instance showServiceId :: Show ServiceId where
  show = genericShow
instance decodeServiceId :: Decode ServiceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceId :: Encode ServiceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServiceIds = ServiceIds (Array ServiceId)
derive instance newtypeServiceIds :: Newtype ServiceIds _
derive instance repGenericServiceIds :: Generic ServiceIds _
instance showServiceIds :: Show ServiceIds where
  show = genericShow
instance decodeServiceIds :: Decode ServiceIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceIds :: Encode ServiceIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServiceList = ServiceList (Array Service)
derive instance newtypeServiceList :: Newtype ServiceList _
derive instance repGenericServiceList :: Generic ServiceList _
instance showServiceList :: Show ServiceList where
  show = genericShow
instance decodeServiceList :: Decode ServiceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceList :: Encode ServiceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServiceNames = ServiceNames (Array String)
derive instance newtypeServiceNames :: Newtype ServiceNames _
derive instance repGenericServiceNames :: Generic ServiceNames _
instance showServiceNames :: Show ServiceNames where
  show = genericShow
instance decodeServiceNames :: Decode ServiceNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceNames :: Encode ServiceNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Response statistics for a service.</p>
newtype ServiceStatistics = ServiceStatistics 
  { "OkCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "ErrorStatistics" :: NullOrUndefined.NullOrUndefined (ErrorStatistics)
  , "FaultStatistics" :: NullOrUndefined.NullOrUndefined (FaultStatistics)
  , "TotalCount" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "TotalResponseTime" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  }
derive instance newtypeServiceStatistics :: Newtype ServiceStatistics _
derive instance repGenericServiceStatistics :: Generic ServiceStatistics _
instance showServiceStatistics :: Show ServiceStatistics where
  show = genericShow
instance decodeServiceStatistics :: Decode ServiceStatistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceStatistics :: Encode ServiceStatistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype TelemetryRecord = TelemetryRecord 
  { "Number" :: (Number)
  , "SegmentsReceivedCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "SegmentsSentCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "SegmentsSpilloverCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "SegmentsRejectedCount" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "BackendConnectionErrors" :: NullOrUndefined.NullOrUndefined (BackendConnectionErrors)
  }
derive instance newtypeTelemetryRecord :: Newtype TelemetryRecord _
derive instance repGenericTelemetryRecord :: Generic TelemetryRecord _
instance showTelemetryRecord :: Show TelemetryRecord where
  show = genericShow
instance decodeTelemetryRecord :: Decode TelemetryRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTelemetryRecord :: Encode TelemetryRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TelemetryRecordList = TelemetryRecordList (Array TelemetryRecord)
derive instance newtypeTelemetryRecordList :: Newtype TelemetryRecordList _
derive instance repGenericTelemetryRecordList :: Generic TelemetryRecordList _
instance showTelemetryRecordList :: Show TelemetryRecordList where
  show = genericShow
instance decodeTelemetryRecordList :: Decode TelemetryRecordList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTelemetryRecordList :: Encode TelemetryRecordList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request exceeds the maximum number of requests per second.</p>
newtype ThrottledException = ThrottledException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeThrottledException :: Newtype ThrottledException _
derive instance repGenericThrottledException :: Generic ThrottledException _
instance showThrottledException :: Show ThrottledException where
  show = genericShow
instance decodeThrottledException :: Decode ThrottledException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottledException :: Encode ThrottledException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A collection of segment documents with matching trace IDs.</p>
newtype Trace = Trace 
  { "Id" :: NullOrUndefined.NullOrUndefined (TraceId)
  , "Duration" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "Segments" :: NullOrUndefined.NullOrUndefined (SegmentList)
  }
derive instance newtypeTrace :: Newtype Trace _
derive instance repGenericTrace :: Generic Trace _
instance showTrace :: Show Trace where
  show = genericShow
instance decodeTrace :: Decode Trace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrace :: Encode Trace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TraceId = TraceId String
derive instance newtypeTraceId :: Newtype TraceId _
derive instance repGenericTraceId :: Generic TraceId _
instance showTraceId :: Show TraceId where
  show = genericShow
instance decodeTraceId :: Decode TraceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceId :: Encode TraceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TraceIdList = TraceIdList (Array TraceId)
derive instance newtypeTraceIdList :: Newtype TraceIdList _
derive instance repGenericTraceIdList :: Generic TraceIdList _
instance showTraceIdList :: Show TraceIdList where
  show = genericShow
instance decodeTraceIdList :: Decode TraceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceIdList :: Encode TraceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TraceList = TraceList (Array Trace)
derive instance newtypeTraceList :: Newtype TraceList _
derive instance repGenericTraceList :: Generic TraceList _
instance showTraceList :: Show TraceList where
  show = genericShow
instance decodeTraceList :: Decode TraceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceList :: Encode TraceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TraceSegmentDocument = TraceSegmentDocument String
derive instance newtypeTraceSegmentDocument :: Newtype TraceSegmentDocument _
derive instance repGenericTraceSegmentDocument :: Generic TraceSegmentDocument _
instance showTraceSegmentDocument :: Show TraceSegmentDocument where
  show = genericShow
instance decodeTraceSegmentDocument :: Decode TraceSegmentDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceSegmentDocument :: Encode TraceSegmentDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TraceSegmentDocumentList = TraceSegmentDocumentList (Array TraceSegmentDocument)
derive instance newtypeTraceSegmentDocumentList :: Newtype TraceSegmentDocumentList _
derive instance repGenericTraceSegmentDocumentList :: Generic TraceSegmentDocumentList _
instance showTraceSegmentDocumentList :: Show TraceSegmentDocumentList where
  show = genericShow
instance decodeTraceSegmentDocumentList :: Decode TraceSegmentDocumentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceSegmentDocumentList :: Encode TraceSegmentDocumentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Metadata generated from the segment documents in a trace.</p>
newtype TraceSummary = TraceSummary 
  { "Id" :: NullOrUndefined.NullOrUndefined (TraceId)
  , "Duration" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "ResponseTime" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "HasFault" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "HasError" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "HasThrottle" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "IsPartial" :: NullOrUndefined.NullOrUndefined (NullableBoolean)
  , "Http" :: NullOrUndefined.NullOrUndefined (Http)
  , "Annotations" :: NullOrUndefined.NullOrUndefined (Annotations)
  , "Users" :: NullOrUndefined.NullOrUndefined (TraceUsers)
  , "ServiceIds" :: NullOrUndefined.NullOrUndefined (ServiceIds)
  }
derive instance newtypeTraceSummary :: Newtype TraceSummary _
derive instance repGenericTraceSummary :: Generic TraceSummary _
instance showTraceSummary :: Show TraceSummary where
  show = genericShow
instance decodeTraceSummary :: Decode TraceSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceSummary :: Encode TraceSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TraceSummaryList = TraceSummaryList (Array TraceSummary)
derive instance newtypeTraceSummaryList :: Newtype TraceSummaryList _
derive instance repGenericTraceSummaryList :: Generic TraceSummaryList _
instance showTraceSummaryList :: Show TraceSummaryList where
  show = genericShow
instance decodeTraceSummaryList :: Decode TraceSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceSummaryList :: Encode TraceSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a user recorded in segment documents.</p>
newtype TraceUser = TraceUser 
  { "UserName" :: NullOrUndefined.NullOrUndefined (String)
  , "ServiceIds" :: NullOrUndefined.NullOrUndefined (ServiceIds)
  }
derive instance newtypeTraceUser :: Newtype TraceUser _
derive instance repGenericTraceUser :: Generic TraceUser _
instance showTraceUser :: Show TraceUser where
  show = genericShow
instance decodeTraceUser :: Decode TraceUser where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceUser :: Encode TraceUser where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TraceUsers = TraceUsers (Array TraceUser)
derive instance newtypeTraceUsers :: Newtype TraceUsers _
derive instance repGenericTraceUsers :: Generic TraceUsers _
instance showTraceUsers :: Show TraceUsers where
  show = genericShow
instance decodeTraceUsers :: Decode TraceUsers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTraceUsers :: Encode TraceUsers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnprocessedTraceIdList = UnprocessedTraceIdList (Array TraceId)
derive instance newtypeUnprocessedTraceIdList :: Newtype UnprocessedTraceIdList _
derive instance repGenericUnprocessedTraceIdList :: Generic UnprocessedTraceIdList _
instance showUnprocessedTraceIdList :: Show UnprocessedTraceIdList where
  show = genericShow
instance decodeUnprocessedTraceIdList :: Decode UnprocessedTraceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessedTraceIdList :: Encode UnprocessedTraceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a segment that failed processing.</p>
newtype UnprocessedTraceSegment = UnprocessedTraceSegment 
  { "Id" :: NullOrUndefined.NullOrUndefined (String)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "Message" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUnprocessedTraceSegment :: Newtype UnprocessedTraceSegment _
derive instance repGenericUnprocessedTraceSegment :: Generic UnprocessedTraceSegment _
instance showUnprocessedTraceSegment :: Show UnprocessedTraceSegment where
  show = genericShow
instance decodeUnprocessedTraceSegment :: Decode UnprocessedTraceSegment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessedTraceSegment :: Encode UnprocessedTraceSegment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnprocessedTraceSegmentList = UnprocessedTraceSegmentList (Array UnprocessedTraceSegment)
derive instance newtypeUnprocessedTraceSegmentList :: Newtype UnprocessedTraceSegmentList _
derive instance repGenericUnprocessedTraceSegmentList :: Generic UnprocessedTraceSegmentList _
instance showUnprocessedTraceSegmentList :: Show UnprocessedTraceSegmentList where
  show = genericShow
instance decodeUnprocessedTraceSegmentList :: Decode UnprocessedTraceSegmentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnprocessedTraceSegmentList :: Encode UnprocessedTraceSegmentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a segment annotation.</p>
newtype ValueWithServiceIds = ValueWithServiceIds 
  { "AnnotationValue" :: NullOrUndefined.NullOrUndefined (AnnotationValue)
  , "ServiceIds" :: NullOrUndefined.NullOrUndefined (ServiceIds)
  }
derive instance newtypeValueWithServiceIds :: Newtype ValueWithServiceIds _
derive instance repGenericValueWithServiceIds :: Generic ValueWithServiceIds _
instance showValueWithServiceIds :: Show ValueWithServiceIds where
  show = genericShow
instance decodeValueWithServiceIds :: Decode ValueWithServiceIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValueWithServiceIds :: Encode ValueWithServiceIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValuesWithServiceIds = ValuesWithServiceIds (Array ValueWithServiceIds)
derive instance newtypeValuesWithServiceIds :: Newtype ValuesWithServiceIds _
derive instance repGenericValuesWithServiceIds :: Generic ValuesWithServiceIds _
instance showValuesWithServiceIds :: Show ValuesWithServiceIds where
  show = genericShow
instance decodeValuesWithServiceIds :: Decode ValuesWithServiceIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValuesWithServiceIds :: Encode ValuesWithServiceIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
