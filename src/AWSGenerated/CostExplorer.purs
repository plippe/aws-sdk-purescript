

-- | <p>The Cost Explorer API allows you to programmatically query your cost and usage data. You can query for aggregated data such as total monthly costs or total daily usage. You can also query for granular data, such as the number of daily write operations for DynamoDB database tables in your production environment. </p> <p>Service Endpoint</p> <p>The Cost Explorer API provides the following endpoint:</p> <ul> <li> <p>https://ce.us-east-1.amazonaws.com</p> </li> </ul>
module AWS.CostExplorer where

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

serviceName = "CostExplorer" :: String


-- | <p>Retrieve cost and usage metrics for your account. You can specify which cost and usage-related metric, such as <code>BlendedCosts</code> or <code>UsageQuantity</code>, that you want the request to return. You can also filter and group your data by various dimensions, such as <code>SERVICE</code> or <code>AZ</code>, in a specific time range. See the <code>GetDimensionValues</code> action for a complete list of the valid dimensions. Master accounts in an organization have access to all member accounts.</p>
getCostAndUsage :: forall eff. GetCostAndUsageRequest -> Aff (exception :: EXCEPTION | eff) GetCostAndUsageResponse
getCostAndUsage = Request.request serviceName "getCostAndUsage" 


-- | <p>You can use <code>GetDimensionValues</code> to retrieve all available filter values for a specific filter over a period of time. You can search the dimension values for an arbitrary string. </p>
getDimensionValues :: forall eff. GetDimensionValuesRequest -> Aff (exception :: EXCEPTION | eff) GetDimensionValuesResponse
getDimensionValues = Request.request serviceName "getDimensionValues" 


-- | <p>Retrieve the reservation coverage for your account. An organization's master account has access to the associated member accounts. For any time period, you can filter data about reservation usage by the following dimensions. </p> <ul> <li> <p>AZ</p> </li> <li> <p>INSTANCE_TYPE</p> </li> <li> <p>LINKED_ACCOUNT</p> </li> <li> <p>PLATFORM</p> </li> <li> <p>REGION</p> </li> <li> <p>TENANCY</p> </li> </ul> <p>To determine valid values for a dimension, use the <code>GetDimensionValues</code> operation. </p>
getReservationCoverage :: forall eff. GetReservationCoverageRequest -> Aff (exception :: EXCEPTION | eff) GetReservationCoverageResponse
getReservationCoverage = Request.request serviceName "getReservationCoverage" 


-- | <p>You can retrieve the Reservation utilization for your account. Master accounts in an organization have access to their associated member accounts. You can filter data by dimensions in a time period. You can use <code>GetDimensionValues</code> to determine the possible dimension values. Currently, you can group only by <code>SUBSCRIPTION_ID</code>. </p>
getReservationUtilization :: forall eff. GetReservationUtilizationRequest -> Aff (exception :: EXCEPTION | eff) GetReservationUtilizationResponse
getReservationUtilization = Request.request serviceName "getReservationUtilization" 


-- | <p>You can query for available tag keys and tag values for a specified period. You can search the tag values for an arbitrary string. </p>
getTags :: forall eff. GetTagsRequest -> Aff (exception :: EXCEPTION | eff) GetTagsResponse
getTags = Request.request serviceName "getTags" 


newtype AttributeType = AttributeType String
derive instance newtypeAttributeType :: Newtype AttributeType _
derive instance repGenericAttributeType :: Generic AttributeType _
instance showAttributeType :: Show AttributeType where
  show = genericShow
instance decodeAttributeType :: Decode AttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeType :: Encode AttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Attributes = Attributes (StrMap.StrMap AttributeValue)
derive instance newtypeAttributes :: Newtype Attributes _
derive instance repGenericAttributes :: Generic Attributes _
instance showAttributes :: Show Attributes where
  show = genericShow
instance decodeAttributes :: Decode Attributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributes :: Encode Attributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested report expired. Update the date interval and try again.</p>
newtype BillExpirationException = BillExpirationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBillExpirationException :: Newtype BillExpirationException _
derive instance repGenericBillExpirationException :: Generic BillExpirationException _
instance showBillExpirationException :: Show BillExpirationException where
  show = genericShow
instance decodeBillExpirationException :: Decode BillExpirationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBillExpirationException :: Encode BillExpirationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Context = Context String
derive instance newtypeContext :: Newtype Context _
derive instance repGenericContext :: Generic Context _
instance showContext :: Show Context where
  show = genericShow
instance decodeContext :: Decode Context where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContext :: Encode Context where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The amount of instance usage that a reservation covered.</p>
newtype Coverage = Coverage 
  { "CoverageHours" :: NullOrUndefined.NullOrUndefined (CoverageHours)
  }
derive instance newtypeCoverage :: Newtype Coverage _
derive instance repGenericCoverage :: Generic Coverage _
instance showCoverage :: Show Coverage where
  show = genericShow
instance decodeCoverage :: Decode Coverage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCoverage :: Encode Coverage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Reservation coverage, in hours.</p>
newtype CoverageByTime = CoverageByTime 
  { "TimePeriod" :: NullOrUndefined.NullOrUndefined (DateInterval)
  , "Groups" :: NullOrUndefined.NullOrUndefined (ReservationCoverageGroups)
  , "Total" :: NullOrUndefined.NullOrUndefined (Coverage)
  }
derive instance newtypeCoverageByTime :: Newtype CoverageByTime _
derive instance repGenericCoverageByTime :: Generic CoverageByTime _
instance showCoverageByTime :: Show CoverageByTime where
  show = genericShow
instance decodeCoverageByTime :: Decode CoverageByTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCoverageByTime :: Encode CoverageByTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>How long a running instance either used a reservation or was On-Demand.</p>
newtype CoverageHours = CoverageHours 
  { "OnDemandHours" :: NullOrUndefined.NullOrUndefined (OnDemandHours)
  , "ReservedHours" :: NullOrUndefined.NullOrUndefined (ReservedHours)
  , "TotalRunningHours" :: NullOrUndefined.NullOrUndefined (TotalRunningHours)
  , "CoverageHoursPercentage" :: NullOrUndefined.NullOrUndefined (CoverageHoursPercentage)
  }
derive instance newtypeCoverageHours :: Newtype CoverageHours _
derive instance repGenericCoverageHours :: Generic CoverageHours _
instance showCoverageHours :: Show CoverageHours where
  show = genericShow
instance decodeCoverageHours :: Decode CoverageHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCoverageHours :: Encode CoverageHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CoverageHoursPercentage = CoverageHoursPercentage String
derive instance newtypeCoverageHoursPercentage :: Newtype CoverageHoursPercentage _
derive instance repGenericCoverageHoursPercentage :: Generic CoverageHoursPercentage _
instance showCoverageHoursPercentage :: Show CoverageHoursPercentage where
  show = genericShow
instance decodeCoverageHoursPercentage :: Decode CoverageHoursPercentage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCoverageHoursPercentage :: Encode CoverageHoursPercentage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CoveragesByTime = CoveragesByTime (Array CoverageByTime)
derive instance newtypeCoveragesByTime :: Newtype CoveragesByTime _
derive instance repGenericCoveragesByTime :: Generic CoveragesByTime _
instance showCoveragesByTime :: Show CoveragesByTime where
  show = genericShow
instance decodeCoveragesByTime :: Decode CoveragesByTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCoveragesByTime :: Encode CoveragesByTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested data is unavailable.</p>
newtype DataUnavailableException = DataUnavailableException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDataUnavailableException :: Newtype DataUnavailableException _
derive instance repGenericDataUnavailableException :: Generic DataUnavailableException _
instance showDataUnavailableException :: Show DataUnavailableException where
  show = genericShow
instance decodeDataUnavailableException :: Decode DataUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataUnavailableException :: Encode DataUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The time period that you want the usage and costs for. </p>
newtype DateInterval = DateInterval 
  { "Start" :: (YearMonthDay)
  , "End" :: (YearMonthDay)
  }
derive instance newtypeDateInterval :: Newtype DateInterval _
derive instance repGenericDateInterval :: Generic DateInterval _
instance showDateInterval :: Show DateInterval where
  show = genericShow
instance decodeDateInterval :: Decode DateInterval where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDateInterval :: Encode DateInterval where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Dimension = Dimension String
derive instance newtypeDimension :: Newtype Dimension _
derive instance repGenericDimension :: Generic Dimension _
instance showDimension :: Show Dimension where
  show = genericShow
instance decodeDimension :: Decode Dimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimension :: Encode Dimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The metadata that you can use to filter and group your results. You can use <code>GetDimensionValues</code> to find specific values.</p>
newtype DimensionValues = DimensionValues 
  { "Key" :: NullOrUndefined.NullOrUndefined (Dimension)
  , "Values" :: NullOrUndefined.NullOrUndefined (Values)
  }
derive instance newtypeDimensionValues :: Newtype DimensionValues _
derive instance repGenericDimensionValues :: Generic DimensionValues _
instance showDimensionValues :: Show DimensionValues where
  show = genericShow
instance decodeDimensionValues :: Decode DimensionValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionValues :: Encode DimensionValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The metadata of a specific type that you can use to filter and group your results. You can use <code>GetDimensionValues</code> to find specific values.</p>
newtype DimensionValuesWithAttributes = DimensionValuesWithAttributes 
  { "Value" :: NullOrUndefined.NullOrUndefined (Value)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypeDimensionValuesWithAttributes :: Newtype DimensionValuesWithAttributes _
derive instance repGenericDimensionValuesWithAttributes :: Generic DimensionValuesWithAttributes _
instance showDimensionValuesWithAttributes :: Show DimensionValuesWithAttributes where
  show = genericShow
instance decodeDimensionValuesWithAttributes :: Decode DimensionValuesWithAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionValuesWithAttributes :: Encode DimensionValuesWithAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionValuesWithAttributesList = DimensionValuesWithAttributesList (Array DimensionValuesWithAttributes)
derive instance newtypeDimensionValuesWithAttributesList :: Newtype DimensionValuesWithAttributesList _
derive instance repGenericDimensionValuesWithAttributesList :: Generic DimensionValuesWithAttributesList _
instance showDimensionValuesWithAttributesList :: Show DimensionValuesWithAttributesList where
  show = genericShow
instance decodeDimensionValuesWithAttributesList :: Decode DimensionValuesWithAttributesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionValuesWithAttributesList :: Encode DimensionValuesWithAttributesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Entity = Entity String
derive instance newtypeEntity :: Newtype Entity _
derive instance repGenericEntity :: Generic Entity _
instance showEntity :: Show Entity where
  show = genericShow
instance decodeEntity :: Decode Entity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntity :: Encode Entity where
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


newtype Estimated = Estimated Boolean
derive instance newtypeEstimated :: Newtype Estimated _
derive instance repGenericEstimated :: Generic Estimated _
instance showEstimated :: Show Estimated where
  show = genericShow
instance decodeEstimated :: Decode Estimated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEstimated :: Encode Estimated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Use <code>Expression</code> to filter by cost or by usage. There are two patterns: </p> <ul> <li> <p>Simple dimension values - You can set the dimension name and values for the filters that you plan to use. For example, you can filter for <code>INSTANCE_TYPE==m4.xlarge OR INSTANCE_TYPE==c4.large</code>. The <code>Expression</code> for that looks like this.</p> <p> <code>{ "Dimensions": { "Key": "INSTANCE_TYPE", "Values": [ "m4.xlarge", “c4.large” ] } }</code> </p> <p>The list of dimension values are OR'd together to retrieve cost or usage data. You can create <code>Expression</code> and <code>DimensionValues</code> objects using either <code>with*</code> methods or <code>set*</code> methods in multiple lines. </p> </li> <li> <p>Compound dimension values with logical operations - You can use multiple <code>Expression</code> types and the logical operators <code>AND/OR/NOT</code> to create a list of one or more <code>Expression</code> objects. This allows you to filter on more advanced options. For example, you can filter on <code>((INSTANCE_TYPE == m4.large OR INSTANCE_TYPE == m3.large) OR (TAG.Type == Type1)) AND (USAGE_TYPE != DataTransfer)</code>. The <code>Expression</code> for that looks like this.</p> <p> <code>{ "And": [ {"Or": [ {"Dimensions": { "Key": "INSTANCE_TYPE", "Values": [ "m4.x.large", "c4.large" ] }}, {"Tag": { "Key": "TagName", "Values": ["Value1"] } } ]}, {"Not": {"dimensions": { "Key": "USAGE_TYPE", "Values": ["DataTransfer"] }}} ] } </code> </p> <note> <p>Because each <code>Expression</code> can have only one operator, the service returns an error if more than one is specified. The following example shows an Expression object that will create an error.</p> </note> <p> <code> { "And": [ ... ], "DimensionValues": { "Dimension": "USAGE_TYPE", "Values": [ "DataTransfer" ] } } </code> </p> </li> </ul>
newtype Expression = Expression 
  { "Dimensions" :: NullOrUndefined.NullOrUndefined (DimensionValues)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagValues)
  }
derive instance newtypeExpression :: Newtype Expression _
derive instance repGenericExpression :: Generic Expression _
instance showExpression :: Show Expression where
  show = genericShow
instance decodeExpression :: Decode Expression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpression :: Encode Expression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Expressions = Expressions (Array Expression)
derive instance newtypeExpressions :: Newtype Expressions _
derive instance repGenericExpressions :: Generic Expressions _
instance showExpressions :: Show Expressions where
  show = genericShow
instance decodeExpressions :: Decode Expressions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpressions :: Encode Expressions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCostAndUsageRequest = GetCostAndUsageRequest 
  { "TimePeriod" :: NullOrUndefined.NullOrUndefined (DateInterval)
  , "Granularity" :: NullOrUndefined.NullOrUndefined (Granularity)
  , "Filter" :: NullOrUndefined.NullOrUndefined (Expression)
  , "Metrics" :: NullOrUndefined.NullOrUndefined (MetricNames)
  , "GroupBy" :: NullOrUndefined.NullOrUndefined (GroupDefinitions)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  }
derive instance newtypeGetCostAndUsageRequest :: Newtype GetCostAndUsageRequest _
derive instance repGenericGetCostAndUsageRequest :: Generic GetCostAndUsageRequest _
instance showGetCostAndUsageRequest :: Show GetCostAndUsageRequest where
  show = genericShow
instance decodeGetCostAndUsageRequest :: Decode GetCostAndUsageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCostAndUsageRequest :: Encode GetCostAndUsageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCostAndUsageResponse = GetCostAndUsageResponse 
  { "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  , "GroupDefinitions" :: NullOrUndefined.NullOrUndefined (GroupDefinitions)
  , "ResultsByTime" :: NullOrUndefined.NullOrUndefined (ResultsByTime)
  }
derive instance newtypeGetCostAndUsageResponse :: Newtype GetCostAndUsageResponse _
derive instance repGenericGetCostAndUsageResponse :: Generic GetCostAndUsageResponse _
instance showGetCostAndUsageResponse :: Show GetCostAndUsageResponse where
  show = genericShow
instance decodeGetCostAndUsageResponse :: Decode GetCostAndUsageResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCostAndUsageResponse :: Encode GetCostAndUsageResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDimensionValuesRequest = GetDimensionValuesRequest 
  { "SearchString" :: NullOrUndefined.NullOrUndefined (SearchString)
  , "TimePeriod" :: (DateInterval)
  , "Dimension" :: (Dimension)
  , "Context" :: NullOrUndefined.NullOrUndefined (Context)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  }
derive instance newtypeGetDimensionValuesRequest :: Newtype GetDimensionValuesRequest _
derive instance repGenericGetDimensionValuesRequest :: Generic GetDimensionValuesRequest _
instance showGetDimensionValuesRequest :: Show GetDimensionValuesRequest where
  show = genericShow
instance decodeGetDimensionValuesRequest :: Decode GetDimensionValuesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDimensionValuesRequest :: Encode GetDimensionValuesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDimensionValuesResponse = GetDimensionValuesResponse 
  { "DimensionValues" :: (DimensionValuesWithAttributesList)
  , "ReturnSize" :: (PageSize)
  , "TotalSize" :: (PageSize)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  }
derive instance newtypeGetDimensionValuesResponse :: Newtype GetDimensionValuesResponse _
derive instance repGenericGetDimensionValuesResponse :: Generic GetDimensionValuesResponse _
instance showGetDimensionValuesResponse :: Show GetDimensionValuesResponse where
  show = genericShow
instance decodeGetDimensionValuesResponse :: Decode GetDimensionValuesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDimensionValuesResponse :: Encode GetDimensionValuesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can query for how much of your instance usage was covered by a reservation.</p>
newtype GetReservationCoverageRequest = GetReservationCoverageRequest 
  { "TimePeriod" :: (DateInterval)
  , "GroupBy" :: NullOrUndefined.NullOrUndefined (GroupDefinitions)
  , "Granularity" :: NullOrUndefined.NullOrUndefined (Granularity)
  , "Filter" :: NullOrUndefined.NullOrUndefined (Expression)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  }
derive instance newtypeGetReservationCoverageRequest :: Newtype GetReservationCoverageRequest _
derive instance repGenericGetReservationCoverageRequest :: Generic GetReservationCoverageRequest _
instance showGetReservationCoverageRequest :: Show GetReservationCoverageRequest where
  show = genericShow
instance decodeGetReservationCoverageRequest :: Decode GetReservationCoverageRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReservationCoverageRequest :: Encode GetReservationCoverageRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetReservationCoverageResponse = GetReservationCoverageResponse 
  { "CoveragesByTime" :: (CoveragesByTime)
  , "Total" :: NullOrUndefined.NullOrUndefined (Coverage)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  }
derive instance newtypeGetReservationCoverageResponse :: Newtype GetReservationCoverageResponse _
derive instance repGenericGetReservationCoverageResponse :: Generic GetReservationCoverageResponse _
instance showGetReservationCoverageResponse :: Show GetReservationCoverageResponse where
  show = genericShow
instance decodeGetReservationCoverageResponse :: Decode GetReservationCoverageResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReservationCoverageResponse :: Encode GetReservationCoverageResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetReservationUtilizationRequest = GetReservationUtilizationRequest 
  { "TimePeriod" :: (DateInterval)
  , "GroupBy" :: NullOrUndefined.NullOrUndefined (GroupDefinitions)
  , "Granularity" :: NullOrUndefined.NullOrUndefined (Granularity)
  , "Filter" :: NullOrUndefined.NullOrUndefined (Expression)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  }
derive instance newtypeGetReservationUtilizationRequest :: Newtype GetReservationUtilizationRequest _
derive instance repGenericGetReservationUtilizationRequest :: Generic GetReservationUtilizationRequest _
instance showGetReservationUtilizationRequest :: Show GetReservationUtilizationRequest where
  show = genericShow
instance decodeGetReservationUtilizationRequest :: Decode GetReservationUtilizationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReservationUtilizationRequest :: Encode GetReservationUtilizationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetReservationUtilizationResponse = GetReservationUtilizationResponse 
  { "UtilizationsByTime" :: (UtilizationsByTime)
  , "Total" :: NullOrUndefined.NullOrUndefined (ReservationAggregates)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  }
derive instance newtypeGetReservationUtilizationResponse :: Newtype GetReservationUtilizationResponse _
derive instance repGenericGetReservationUtilizationResponse :: Generic GetReservationUtilizationResponse _
instance showGetReservationUtilizationResponse :: Show GetReservationUtilizationResponse where
  show = genericShow
instance decodeGetReservationUtilizationResponse :: Decode GetReservationUtilizationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReservationUtilizationResponse :: Encode GetReservationUtilizationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTagsRequest = GetTagsRequest 
  { "SearchString" :: NullOrUndefined.NullOrUndefined (SearchString)
  , "TimePeriod" :: (DateInterval)
  , "TagKey" :: NullOrUndefined.NullOrUndefined (TagKey)
  , "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  }
derive instance newtypeGetTagsRequest :: Newtype GetTagsRequest _
derive instance repGenericGetTagsRequest :: Generic GetTagsRequest _
instance showGetTagsRequest :: Show GetTagsRequest where
  show = genericShow
instance decodeGetTagsRequest :: Decode GetTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagsRequest :: Encode GetTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTagsResponse = GetTagsResponse 
  { "NextPageToken" :: NullOrUndefined.NullOrUndefined (NextPageToken)
  , "Tags" :: (TagList)
  , "ReturnSize" :: (PageSize)
  , "TotalSize" :: (PageSize)
  }
derive instance newtypeGetTagsResponse :: Newtype GetTagsResponse _
derive instance repGenericGetTagsResponse :: Generic GetTagsResponse _
instance showGetTagsResponse :: Show GetTagsResponse where
  show = genericShow
instance decodeGetTagsResponse :: Decode GetTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTagsResponse :: Encode GetTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Granularity = Granularity String
derive instance newtypeGranularity :: Newtype Granularity _
derive instance repGenericGranularity :: Generic Granularity _
instance showGranularity :: Show Granularity where
  show = genericShow
instance decodeGranularity :: Decode Granularity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGranularity :: Encode Granularity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One level of grouped data within the results.</p>
newtype Group = Group 
  { "Keys" :: NullOrUndefined.NullOrUndefined (Keys)
  , "Metrics" :: NullOrUndefined.NullOrUndefined (Metrics)
  }
derive instance newtypeGroup :: Newtype Group _
derive instance repGenericGroup :: Generic Group _
instance showGroup :: Show Group where
  show = genericShow
instance decodeGroup :: Decode Group where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroup :: Encode Group where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a group when you specify a group by criteria, or in the response to a query with a specific grouping.</p>
newtype GroupDefinition = GroupDefinition 
  { "Type" :: NullOrUndefined.NullOrUndefined (GroupDefinitionType)
  , "Key" :: NullOrUndefined.NullOrUndefined (GroupDefinitionKey)
  }
derive instance newtypeGroupDefinition :: Newtype GroupDefinition _
derive instance repGenericGroupDefinition :: Generic GroupDefinition _
instance showGroupDefinition :: Show GroupDefinition where
  show = genericShow
instance decodeGroupDefinition :: Decode GroupDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupDefinition :: Encode GroupDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupDefinitionKey = GroupDefinitionKey String
derive instance newtypeGroupDefinitionKey :: Newtype GroupDefinitionKey _
derive instance repGenericGroupDefinitionKey :: Generic GroupDefinitionKey _
instance showGroupDefinitionKey :: Show GroupDefinitionKey where
  show = genericShow
instance decodeGroupDefinitionKey :: Decode GroupDefinitionKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupDefinitionKey :: Encode GroupDefinitionKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupDefinitionType = GroupDefinitionType String
derive instance newtypeGroupDefinitionType :: Newtype GroupDefinitionType _
derive instance repGenericGroupDefinitionType :: Generic GroupDefinitionType _
instance showGroupDefinitionType :: Show GroupDefinitionType where
  show = genericShow
instance decodeGroupDefinitionType :: Decode GroupDefinitionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupDefinitionType :: Encode GroupDefinitionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupDefinitions = GroupDefinitions (Array GroupDefinition)
derive instance newtypeGroupDefinitions :: Newtype GroupDefinitions _
derive instance repGenericGroupDefinitions :: Generic GroupDefinitions _
instance showGroupDefinitions :: Show GroupDefinitions where
  show = genericShow
instance decodeGroupDefinitions :: Decode GroupDefinitions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupDefinitions :: Encode GroupDefinitions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Groups = Groups (Array Group)
derive instance newtypeGroups :: Newtype Groups _
derive instance repGenericGroups :: Generic Groups _
instance showGroups :: Show Groups where
  show = genericShow
instance decodeGroups :: Decode Groups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroups :: Encode Groups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pagination token is invalid. Try again without a pagination token.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Key = Key String
derive instance newtypeKey :: Newtype Key _
derive instance repGenericKey :: Generic Key _
instance showKey :: Show Key where
  show = genericShow
instance decodeKey :: Decode Key where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKey :: Encode Key where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Keys = Keys (Array Key)
derive instance newtypeKeys :: Newtype Keys _
derive instance repGenericKeys :: Generic Keys _
instance showKeys :: Show Keys where
  show = genericShow
instance decodeKeys :: Decode Keys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeys :: Encode Keys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You made too many calls in a short period of time. Try again later.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricAmount = MetricAmount String
derive instance newtypeMetricAmount :: Newtype MetricAmount _
derive instance repGenericMetricAmount :: Generic MetricAmount _
instance showMetricAmount :: Show MetricAmount where
  show = genericShow
instance decodeMetricAmount :: Decode MetricAmount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricAmount :: Encode MetricAmount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _
derive instance repGenericMetricName :: Generic MetricName _
instance showMetricName :: Show MetricName where
  show = genericShow
instance decodeMetricName :: Decode MetricName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricName :: Encode MetricName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricNames = MetricNames (Array MetricName)
derive instance newtypeMetricNames :: Newtype MetricNames _
derive instance repGenericMetricNames :: Generic MetricNames _
instance showMetricNames :: Show MetricNames where
  show = genericShow
instance decodeMetricNames :: Decode MetricNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricNames :: Encode MetricNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricUnit = MetricUnit String
derive instance newtypeMetricUnit :: Newtype MetricUnit _
derive instance repGenericMetricUnit :: Generic MetricUnit _
instance showMetricUnit :: Show MetricUnit where
  show = genericShow
instance decodeMetricUnit :: Decode MetricUnit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricUnit :: Encode MetricUnit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The aggregated value for a metric.</p>
newtype MetricValue = MetricValue 
  { "Amount" :: NullOrUndefined.NullOrUndefined (MetricAmount)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (MetricUnit)
  }
derive instance newtypeMetricValue :: Newtype MetricValue _
derive instance repGenericMetricValue :: Generic MetricValue _
instance showMetricValue :: Show MetricValue where
  show = genericShow
instance decodeMetricValue :: Decode MetricValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricValue :: Encode MetricValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Metrics = Metrics (StrMap.StrMap MetricValue)
derive instance newtypeMetrics :: Newtype Metrics _
derive instance repGenericMetrics :: Generic Metrics _
instance showMetrics :: Show Metrics where
  show = genericShow
instance decodeMetrics :: Decode Metrics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetrics :: Encode Metrics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextPageToken = NextPageToken String
derive instance newtypeNextPageToken :: Newtype NextPageToken _
derive instance repGenericNextPageToken :: Generic NextPageToken _
instance showNextPageToken :: Show NextPageToken where
  show = genericShow
instance decodeNextPageToken :: Decode NextPageToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextPageToken :: Encode NextPageToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OnDemandHours = OnDemandHours String
derive instance newtypeOnDemandHours :: Newtype OnDemandHours _
derive instance repGenericOnDemandHours :: Generic OnDemandHours _
instance showOnDemandHours :: Show OnDemandHours where
  show = genericShow
instance decodeOnDemandHours :: Decode OnDemandHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOnDemandHours :: Encode OnDemandHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _
derive instance repGenericPageSize :: Generic PageSize _
instance showPageSize :: Show PageSize where
  show = genericShow
instance decodePageSize :: Decode PageSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageSize :: Encode PageSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PurchasedHours = PurchasedHours String
derive instance newtypePurchasedHours :: Newtype PurchasedHours _
derive instance repGenericPurchasedHours :: Generic PurchasedHours _
instance showPurchasedHours :: Show PurchasedHours where
  show = genericShow
instance decodePurchasedHours :: Decode PurchasedHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePurchasedHours :: Encode PurchasedHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The aggregated numbers for your RI usage.</p>
newtype ReservationAggregates = ReservationAggregates 
  { "UtilizationPercentage" :: NullOrUndefined.NullOrUndefined (UtilizationPercentage)
  , "PurchasedHours" :: NullOrUndefined.NullOrUndefined (PurchasedHours)
  , "TotalActualHours" :: NullOrUndefined.NullOrUndefined (TotalActualHours)
  , "UnusedHours" :: NullOrUndefined.NullOrUndefined (UnusedHours)
  }
derive instance newtypeReservationAggregates :: Newtype ReservationAggregates _
derive instance repGenericReservationAggregates :: Generic ReservationAggregates _
instance showReservationAggregates :: Show ReservationAggregates where
  show = genericShow
instance decodeReservationAggregates :: Decode ReservationAggregates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservationAggregates :: Encode ReservationAggregates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A group of reservations that share a set of attributes.</p>
newtype ReservationCoverageGroup = ReservationCoverageGroup 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (Attributes)
  , "Coverage" :: NullOrUndefined.NullOrUndefined (Coverage)
  }
derive instance newtypeReservationCoverageGroup :: Newtype ReservationCoverageGroup _
derive instance repGenericReservationCoverageGroup :: Generic ReservationCoverageGroup _
instance showReservationCoverageGroup :: Show ReservationCoverageGroup where
  show = genericShow
instance decodeReservationCoverageGroup :: Decode ReservationCoverageGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservationCoverageGroup :: Encode ReservationCoverageGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservationCoverageGroups = ReservationCoverageGroups (Array ReservationCoverageGroup)
derive instance newtypeReservationCoverageGroups :: Newtype ReservationCoverageGroups _
derive instance repGenericReservationCoverageGroups :: Generic ReservationCoverageGroups _
instance showReservationCoverageGroups :: Show ReservationCoverageGroups where
  show = genericShow
instance decodeReservationCoverageGroups :: Decode ReservationCoverageGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservationCoverageGroups :: Encode ReservationCoverageGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservationGroupKey = ReservationGroupKey String
derive instance newtypeReservationGroupKey :: Newtype ReservationGroupKey _
derive instance repGenericReservationGroupKey :: Generic ReservationGroupKey _
instance showReservationGroupKey :: Show ReservationGroupKey where
  show = genericShow
instance decodeReservationGroupKey :: Decode ReservationGroupKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservationGroupKey :: Encode ReservationGroupKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservationGroupValue = ReservationGroupValue String
derive instance newtypeReservationGroupValue :: Newtype ReservationGroupValue _
derive instance repGenericReservationGroupValue :: Generic ReservationGroupValue _
instance showReservationGroupValue :: Show ReservationGroupValue where
  show = genericShow
instance decodeReservationGroupValue :: Decode ReservationGroupValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservationGroupValue :: Encode ReservationGroupValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A group of RIs that share a set of attributes.</p>
newtype ReservationUtilizationGroup = ReservationUtilizationGroup 
  { "Key" :: NullOrUndefined.NullOrUndefined (ReservationGroupKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (ReservationGroupValue)
  , "Attributes" :: NullOrUndefined.NullOrUndefined (Attributes)
  , "Utilization" :: NullOrUndefined.NullOrUndefined (ReservationAggregates)
  }
derive instance newtypeReservationUtilizationGroup :: Newtype ReservationUtilizationGroup _
derive instance repGenericReservationUtilizationGroup :: Generic ReservationUtilizationGroup _
instance showReservationUtilizationGroup :: Show ReservationUtilizationGroup where
  show = genericShow
instance decodeReservationUtilizationGroup :: Decode ReservationUtilizationGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservationUtilizationGroup :: Encode ReservationUtilizationGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservationUtilizationGroups = ReservationUtilizationGroups (Array ReservationUtilizationGroup)
derive instance newtypeReservationUtilizationGroups :: Newtype ReservationUtilizationGroups _
derive instance repGenericReservationUtilizationGroups :: Generic ReservationUtilizationGroups _
instance showReservationUtilizationGroups :: Show ReservationUtilizationGroups where
  show = genericShow
instance decodeReservationUtilizationGroups :: Decode ReservationUtilizationGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservationUtilizationGroups :: Encode ReservationUtilizationGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReservedHours = ReservedHours String
derive instance newtypeReservedHours :: Newtype ReservedHours _
derive instance repGenericReservedHours :: Generic ReservedHours _
instance showReservedHours :: Show ReservedHours where
  show = genericShow
instance decodeReservedHours :: Decode ReservedHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReservedHours :: Encode ReservedHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result that is associated with a time period.</p>
newtype ResultByTime = ResultByTime 
  { "TimePeriod" :: NullOrUndefined.NullOrUndefined (DateInterval)
  , "Total" :: NullOrUndefined.NullOrUndefined (Metrics)
  , "Groups" :: NullOrUndefined.NullOrUndefined (Groups)
  , "Estimated" :: NullOrUndefined.NullOrUndefined (Estimated)
  }
derive instance newtypeResultByTime :: Newtype ResultByTime _
derive instance repGenericResultByTime :: Generic ResultByTime _
instance showResultByTime :: Show ResultByTime where
  show = genericShow
instance decodeResultByTime :: Decode ResultByTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResultByTime :: Encode ResultByTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResultsByTime = ResultsByTime (Array ResultByTime)
derive instance newtypeResultsByTime :: Newtype ResultsByTime _
derive instance repGenericResultsByTime :: Generic ResultsByTime _
instance showResultsByTime :: Show ResultsByTime where
  show = genericShow
instance decodeResultsByTime :: Decode ResultsByTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResultsByTime :: Encode ResultsByTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchString = SearchString String
derive instance newtypeSearchString :: Newtype SearchString _
derive instance repGenericSearchString :: Generic SearchString _
instance showSearchString :: Show SearchString where
  show = genericShow
instance decodeSearchString :: Decode SearchString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchString :: Encode SearchString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Entity)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The values that are available for a tag.</p>
newtype TagValues = TagValues 
  { "Key" :: NullOrUndefined.NullOrUndefined (TagKey)
  , "Values" :: NullOrUndefined.NullOrUndefined (Values)
  }
derive instance newtypeTagValues :: Newtype TagValues _
derive instance repGenericTagValues :: Generic TagValues _
instance showTagValues :: Show TagValues where
  show = genericShow
instance decodeTagValues :: Decode TagValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValues :: Encode TagValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TotalActualHours = TotalActualHours String
derive instance newtypeTotalActualHours :: Newtype TotalActualHours _
derive instance repGenericTotalActualHours :: Generic TotalActualHours _
instance showTotalActualHours :: Show TotalActualHours where
  show = genericShow
instance decodeTotalActualHours :: Decode TotalActualHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTotalActualHours :: Encode TotalActualHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TotalRunningHours = TotalRunningHours String
derive instance newtypeTotalRunningHours :: Newtype TotalRunningHours _
derive instance repGenericTotalRunningHours :: Generic TotalRunningHours _
instance showTotalRunningHours :: Show TotalRunningHours where
  show = genericShow
instance decodeTotalRunningHours :: Decode TotalRunningHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTotalRunningHours :: Encode TotalRunningHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnusedHours = UnusedHours String
derive instance newtypeUnusedHours :: Newtype UnusedHours _
derive instance repGenericUnusedHours :: Generic UnusedHours _
instance showUnusedHours :: Show UnusedHours where
  show = genericShow
instance decodeUnusedHours :: Decode UnusedHours where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnusedHours :: Encode UnusedHours where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The amount of utilization, in hours.</p>
newtype UtilizationByTime = UtilizationByTime 
  { "TimePeriod" :: NullOrUndefined.NullOrUndefined (DateInterval)
  , "Groups" :: NullOrUndefined.NullOrUndefined (ReservationUtilizationGroups)
  , "Total" :: NullOrUndefined.NullOrUndefined (ReservationAggregates)
  }
derive instance newtypeUtilizationByTime :: Newtype UtilizationByTime _
derive instance repGenericUtilizationByTime :: Generic UtilizationByTime _
instance showUtilizationByTime :: Show UtilizationByTime where
  show = genericShow
instance decodeUtilizationByTime :: Decode UtilizationByTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUtilizationByTime :: Encode UtilizationByTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UtilizationPercentage = UtilizationPercentage String
derive instance newtypeUtilizationPercentage :: Newtype UtilizationPercentage _
derive instance repGenericUtilizationPercentage :: Generic UtilizationPercentage _
instance showUtilizationPercentage :: Show UtilizationPercentage where
  show = genericShow
instance decodeUtilizationPercentage :: Decode UtilizationPercentage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUtilizationPercentage :: Encode UtilizationPercentage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UtilizationsByTime = UtilizationsByTime (Array UtilizationByTime)
derive instance newtypeUtilizationsByTime :: Newtype UtilizationsByTime _
derive instance repGenericUtilizationsByTime :: Generic UtilizationsByTime _
instance showUtilizationsByTime :: Show UtilizationsByTime where
  show = genericShow
instance decodeUtilizationsByTime :: Decode UtilizationsByTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUtilizationsByTime :: Encode UtilizationsByTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Value = Value String
derive instance newtypeValue :: Newtype Value _
derive instance repGenericValue :: Generic Value _
instance showValue :: Show Value where
  show = genericShow
instance decodeValue :: Decode Value where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValue :: Encode Value where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Values = Values (Array Value)
derive instance newtypeValues :: Newtype Values _
derive instance repGenericValues :: Generic Values _
instance showValues :: Show Values where
  show = genericShow
instance decodeValues :: Decode Values where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValues :: Encode Values where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype YearMonthDay = YearMonthDay String
derive instance newtypeYearMonthDay :: Newtype YearMonthDay _
derive instance repGenericYearMonthDay :: Generic YearMonthDay _
instance showYearMonthDay :: Show YearMonthDay where
  show = genericShow
instance decodeYearMonthDay :: Decode YearMonthDay where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeYearMonthDay :: Encode YearMonthDay where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
