

-- | <p>Amazon CloudWatch monitors your Amazon Web Services (AWS) resources and the applications you run on AWS in real time. You can use CloudWatch to collect and track metrics, which are the variables you want to measure for your resources and applications.</p> <p>CloudWatch alarms send notifications or automatically change the resources you are monitoring based on rules that you define. For example, you can monitor the CPU usage and disk reads and writes of your Amazon EC2 instances. Then, use this data to determine whether you should launch additional instances to handle increased load. You can also use this data to stop under-used instances to save money.</p> <p>In addition to monitoring the built-in metrics that come with AWS, you can monitor your own custom metrics. With CloudWatch, you gain system-wide visibility into resource utilization, application performance, and operational health.</p>
module AWS.CloudWatch where

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

serviceName = "CloudWatch" :: String


-- | <p>Deletes the specified alarms. In the event of an error, no alarms are deleted.</p>
deleteAlarms :: forall eff. DeleteAlarmsInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteAlarms = Request.request serviceName "deleteAlarms" 


-- | <p>Deletes all dashboards that you specify. You may specify up to 100 dashboards to delete. If there is an error during this call, no dashboards are deleted.</p>
deleteDashboards :: forall eff. DeleteDashboardsInput -> Aff (exception :: EXCEPTION | eff) DeleteDashboardsOutput
deleteDashboards = Request.request serviceName "deleteDashboards" 


-- | <p>Retrieves the history for the specified alarm. You can filter the results by date range or item type. If an alarm name is not specified, the histories for all alarms are returned.</p> <p>CloudWatch retains the history of an alarm even if you delete the alarm.</p>
describeAlarmHistory :: forall eff. DescribeAlarmHistoryInput -> Aff (exception :: EXCEPTION | eff) DescribeAlarmHistoryOutput
describeAlarmHistory = Request.request serviceName "describeAlarmHistory" 


-- | <p>Retrieves the specified alarms. If no alarms are specified, all alarms are returned. Alarms can be retrieved by using only a prefix for the alarm name, the alarm state, or a prefix for any action.</p>
describeAlarms :: forall eff. DescribeAlarmsInput -> Aff (exception :: EXCEPTION | eff) DescribeAlarmsOutput
describeAlarms = Request.request serviceName "describeAlarms" 


-- | <p>Retrieves the alarms for the specified metric. To filter the results, specify a statistic, period, or unit.</p>
describeAlarmsForMetric :: forall eff. DescribeAlarmsForMetricInput -> Aff (exception :: EXCEPTION | eff) DescribeAlarmsForMetricOutput
describeAlarmsForMetric = Request.request serviceName "describeAlarmsForMetric" 


-- | <p>Disables the actions for the specified alarms. When an alarm's actions are disabled, the alarm actions do not execute when the alarm state changes.</p>
disableAlarmActions :: forall eff. DisableAlarmActionsInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
disableAlarmActions = Request.request serviceName "disableAlarmActions" 


-- | <p>Enables the actions for the specified alarms.</p>
enableAlarmActions :: forall eff. EnableAlarmActionsInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
enableAlarmActions = Request.request serviceName "enableAlarmActions" 


-- | <p>Displays the details of the dashboard that you specify.</p> <p>To copy an existing dashboard, use <code>GetDashboard</code>, and then use the data returned within <code>DashboardBody</code> as the template for the new dashboard when you call <code>PutDashboard</code> to create the copy.</p>
getDashboard :: forall eff. GetDashboardInput -> Aff (exception :: EXCEPTION | eff) GetDashboardOutput
getDashboard = Request.request serviceName "getDashboard" 


-- | <p>Gets statistics for the specified metric.</p> <p>The maximum number of data points returned from a single call is 1,440. If you request more than 1,440 data points, CloudWatch returns an error. To reduce the number of data points, you can narrow the specified time range and make multiple requests across adjacent time ranges, or you can increase the specified period. Data points are not returned in chronological order.</p> <p>CloudWatch aggregates data points based on the length of the period that you specify. For example, if you request statistics with a one-hour period, CloudWatch aggregates all data points with time stamps that fall within each one-hour period. Therefore, the number of values aggregated by CloudWatch is larger than the number of data points returned.</p> <p>CloudWatch needs raw data points to calculate percentile statistics. If you publish data using a statistic set instead, you can only retrieve percentile statistics for this data if one of the following conditions is true:</p> <ul> <li> <p>The SampleCount value of the statistic set is 1.</p> </li> <li> <p>The Min and the Max values of the statistic set are equal.</p> </li> </ul> <p>Amazon CloudWatch retains metric data as follows:</p> <ul> <li> <p>Data points with a period of less than 60 seconds are available for 3 hours. These data points are high-resolution metrics and are available only for custom metrics that have been defined with a <code>StorageResolution</code> of 1.</p> </li> <li> <p>Data points with a period of 60 seconds (1-minute) are available for 15 days.</p> </li> <li> <p>Data points with a period of 300 seconds (5-minute) are available for 63 days.</p> </li> <li> <p>Data points with a period of 3600 seconds (1 hour) are available for 455 days (15 months).</p> </li> </ul> <p>Data points that are initially published with a shorter period are aggregated together for long-term storage. For example, if you collect data using a period of 1 minute, the data remains available for 15 days with 1-minute resolution. After 15 days, this data is still available, but is aggregated and retrievable only with a resolution of 5 minutes. After 63 days, the data is further aggregated and is available with a resolution of 1 hour.</p> <p>CloudWatch started retaining 5-minute and 1-hour metric data as of July 9, 2016.</p> <p>For information about metrics and dimensions supported by AWS services, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CW_Support_For_AWS.html">Amazon CloudWatch Metrics and Dimensions Reference</a> in the <i>Amazon CloudWatch User Guide</i>.</p>
getMetricStatistics :: forall eff. GetMetricStatisticsInput -> Aff (exception :: EXCEPTION | eff) GetMetricStatisticsOutput
getMetricStatistics = Request.request serviceName "getMetricStatistics" 


-- | <p>Returns a list of the dashboards for your account. If you include <code>DashboardNamePrefix</code>, only those dashboards with names starting with the prefix are listed. Otherwise, all dashboards in your account are listed. </p>
listDashboards :: forall eff. ListDashboardsInput -> Aff (exception :: EXCEPTION | eff) ListDashboardsOutput
listDashboards = Request.request serviceName "listDashboards" 


-- | <p>List the specified metrics. You can use the returned metrics with <a>GetMetricStatistics</a> to obtain statistical data.</p> <p>Up to 500 results are returned for any one call. To retrieve additional results, use the returned token with subsequent calls.</p> <p>After you create a metric, allow up to fifteen minutes before the metric appears. Statistics about the metric, however, are available sooner using <a>GetMetricStatistics</a>.</p>
listMetrics :: forall eff. ListMetricsInput -> Aff (exception :: EXCEPTION | eff) ListMetricsOutput
listMetrics = Request.request serviceName "listMetrics" 


-- | <p>Creates a dashboard if it does not already exist, or updates an existing dashboard. If you update a dashboard, the entire contents are replaced with what you specify here.</p> <p>You can have up to 500 dashboards per account. All dashboards in your account are global, not region-specific.</p> <p>A simple way to create a dashboard using <code>PutDashboard</code> is to copy an existing dashboard. To copy an existing dashboard using the console, you can load the dashboard and then use the View/edit source command in the Actions menu to display the JSON block for that dashboard. Another way to copy a dashboard is to use <code>GetDashboard</code>, and then use the data returned within <code>DashboardBody</code> as the template for the new dashboard when you call <code>PutDashboard</code>.</p> <p>When you create a dashboard with <code>PutDashboard</code>, a good practice is to add a text widget at the top of the dashboard with a message that the dashboard was created by script and should not be changed in the console. This message could also point console users to the location of the <code>DashboardBody</code> script or the CloudFormation template used to create the dashboard.</p>
putDashboard :: forall eff. PutDashboardInput -> Aff (exception :: EXCEPTION | eff) PutDashboardOutput
putDashboard = Request.request serviceName "putDashboard" 


-- | <p>Creates or updates an alarm and associates it with the specified metric. Optionally, this operation can associate one or more Amazon SNS resources with the alarm.</p> <p>When this operation creates an alarm, the alarm state is immediately set to <code>INSUFFICIENT_DATA</code>. The alarm is evaluated and its state is set appropriately. Any actions associated with the state are then executed.</p> <p>When you update an existing alarm, its state is left unchanged, but the update completely overwrites the previous configuration of the alarm.</p> <p>If you are an IAM user, you must have Amazon EC2 permissions for some operations:</p> <ul> <li> <p> <code>iam:CreateServiceLinkedRole</code> for all alarms with EC2 actions</p> </li> <li> <p> <code>ec2:DescribeInstanceStatus</code> and <code>ec2:DescribeInstances</code> for all alarms on EC2 instance status metrics</p> </li> <li> <p> <code>ec2:StopInstances</code> for alarms with stop actions</p> </li> <li> <p> <code>ec2:TerminateInstances</code> for alarms with terminate actions</p> </li> <li> <p> <code>ec2:DescribeInstanceRecoveryAttribute</code> and <code>ec2:RecoverInstances</code> for alarms with recover actions</p> </li> </ul> <p>If you have read/write permissions for Amazon CloudWatch but not for Amazon EC2, you can still create an alarm, but the stop or terminate actions are not performed. However, if you are later granted the required permissions, the alarm actions that you created earlier are performed.</p> <p>If you are using an IAM role (for example, an EC2 instance profile), you cannot stop or terminate the instance using alarm actions. However, you can still see the alarm state and perform any other actions such as Amazon SNS notifications or Auto Scaling policies.</p> <p>If you are using temporary security credentials granted using AWS STS, you cannot stop or terminate an EC2 instance using alarm actions.</p> <p>You must create at least one stop, terminate, or reboot alarm using either the Amazon EC2 or CloudWatch consoles to create the <b>EC2ActionsAccess</b> IAM role. After this IAM role is created, you can create stop, terminate, or reboot alarms using a command-line interface or API.</p>
putMetricAlarm :: forall eff. PutMetricAlarmInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putMetricAlarm = Request.request serviceName "putMetricAlarm" 


-- | <p>Publishes metric data points to Amazon CloudWatch. CloudWatch associates the data points with the specified metric. If the specified metric does not exist, CloudWatch creates the metric. When CloudWatch creates a metric, it can take up to fifteen minutes for the metric to appear in calls to <a>ListMetrics</a>.</p> <p>Each <code>PutMetricData</code> request is limited to 40 KB in size for HTTP POST requests.</p> <p>Although the <code>Value</code> parameter accepts numbers of type <code>Double</code>, CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.</p> <p>You can use up to 10 dimensions per metric to further clarify what data the metric collects. For more information about specifying dimensions, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html">Publishing Metrics</a> in the <i>Amazon CloudWatch User Guide</i>.</p> <p>Data points with time stamps from 24 hours ago or longer can take at least 48 hours to become available for <a>GetMetricStatistics</a> from the time they are submitted.</p> <p>CloudWatch needs raw data points to calculate percentile statistics. If you publish data using a statistic set instead, you can only retrieve percentile statistics for this data if one of the following conditions is true:</p> <ul> <li> <p>The SampleCount value of the statistic set is 1</p> </li> <li> <p>The Min and the Max values of the statistic set are equal</p> </li> </ul>
putMetricData :: forall eff. PutMetricDataInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putMetricData = Request.request serviceName "putMetricData" 


-- | <p>Temporarily sets the state of an alarm for testing purposes. When the updated state differs from the previous value, the action configured for the appropriate state is invoked. For example, if your alarm is configured to send an Amazon SNS message when an alarm is triggered, temporarily changing the alarm state to <code>ALARM</code> sends an SNS message. The alarm returns to its actual state (often within seconds). Because the alarm state change happens quickly, it is typically only visible in the alarm's <b>History</b> tab in the Amazon CloudWatch console or through <a>DescribeAlarmHistory</a>.</p>
setAlarmState :: forall eff. SetAlarmStateInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
setAlarmState = Request.request serviceName "setAlarmState" 


newtype ActionPrefix = ActionPrefix String
derive instance newtypeActionPrefix :: Newtype ActionPrefix _
derive instance repGenericActionPrefix :: Generic ActionPrefix _
instance showActionPrefix :: Show ActionPrefix where
  show = genericShow
instance decodeActionPrefix :: Decode ActionPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionPrefix :: Encode ActionPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionsEnabled = ActionsEnabled Boolean
derive instance newtypeActionsEnabled :: Newtype ActionsEnabled _
derive instance repGenericActionsEnabled :: Generic ActionsEnabled _
instance showActionsEnabled :: Show ActionsEnabled where
  show = genericShow
instance decodeActionsEnabled :: Decode ActionsEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionsEnabled :: Encode ActionsEnabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlarmArn = AlarmArn String
derive instance newtypeAlarmArn :: Newtype AlarmArn _
derive instance repGenericAlarmArn :: Generic AlarmArn _
instance showAlarmArn :: Show AlarmArn where
  show = genericShow
instance decodeAlarmArn :: Decode AlarmArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmArn :: Encode AlarmArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlarmDescription = AlarmDescription String
derive instance newtypeAlarmDescription :: Newtype AlarmDescription _
derive instance repGenericAlarmDescription :: Generic AlarmDescription _
instance showAlarmDescription :: Show AlarmDescription where
  show = genericShow
instance decodeAlarmDescription :: Decode AlarmDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmDescription :: Encode AlarmDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the history of a specific alarm.</p>
newtype AlarmHistoryItem = AlarmHistoryItem 
  { "AlarmName" :: NullOrUndefined.NullOrUndefined (AlarmName)
  , "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "HistoryItemType" :: NullOrUndefined.NullOrUndefined (HistoryItemType)
  , "HistorySummary" :: NullOrUndefined.NullOrUndefined (HistorySummary)
  , "HistoryData" :: NullOrUndefined.NullOrUndefined (HistoryData)
  }
derive instance newtypeAlarmHistoryItem :: Newtype AlarmHistoryItem _
derive instance repGenericAlarmHistoryItem :: Generic AlarmHistoryItem _
instance showAlarmHistoryItem :: Show AlarmHistoryItem where
  show = genericShow
instance decodeAlarmHistoryItem :: Decode AlarmHistoryItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmHistoryItem :: Encode AlarmHistoryItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlarmHistoryItems = AlarmHistoryItems (Array AlarmHistoryItem)
derive instance newtypeAlarmHistoryItems :: Newtype AlarmHistoryItems _
derive instance repGenericAlarmHistoryItems :: Generic AlarmHistoryItems _
instance showAlarmHistoryItems :: Show AlarmHistoryItems where
  show = genericShow
instance decodeAlarmHistoryItems :: Decode AlarmHistoryItems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmHistoryItems :: Encode AlarmHistoryItems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlarmName = AlarmName String
derive instance newtypeAlarmName :: Newtype AlarmName _
derive instance repGenericAlarmName :: Generic AlarmName _
instance showAlarmName :: Show AlarmName where
  show = genericShow
instance decodeAlarmName :: Decode AlarmName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmName :: Encode AlarmName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlarmNamePrefix = AlarmNamePrefix String
derive instance newtypeAlarmNamePrefix :: Newtype AlarmNamePrefix _
derive instance repGenericAlarmNamePrefix :: Generic AlarmNamePrefix _
instance showAlarmNamePrefix :: Show AlarmNamePrefix where
  show = genericShow
instance decodeAlarmNamePrefix :: Decode AlarmNamePrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmNamePrefix :: Encode AlarmNamePrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlarmNames = AlarmNames (Array AlarmName)
derive instance newtypeAlarmNames :: Newtype AlarmNames _
derive instance repGenericAlarmNames :: Generic AlarmNames _
instance showAlarmNames :: Show AlarmNames where
  show = genericShow
instance decodeAlarmNames :: Decode AlarmNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmNames :: Encode AlarmNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsQueryErrorMessage = AwsQueryErrorMessage String
derive instance newtypeAwsQueryErrorMessage :: Newtype AwsQueryErrorMessage _
derive instance repGenericAwsQueryErrorMessage :: Generic AwsQueryErrorMessage _
instance showAwsQueryErrorMessage :: Show AwsQueryErrorMessage where
  show = genericShow
instance decodeAwsQueryErrorMessage :: Decode AwsQueryErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsQueryErrorMessage :: Encode AwsQueryErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _
derive instance repGenericComparisonOperator :: Generic ComparisonOperator _
instance showComparisonOperator :: Show ComparisonOperator where
  show = genericShow
instance decodeComparisonOperator :: Decode ComparisonOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparisonOperator :: Encode ComparisonOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DashboardArn = DashboardArn String
derive instance newtypeDashboardArn :: Newtype DashboardArn _
derive instance repGenericDashboardArn :: Generic DashboardArn _
instance showDashboardArn :: Show DashboardArn where
  show = genericShow
instance decodeDashboardArn :: Decode DashboardArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardArn :: Encode DashboardArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DashboardBody = DashboardBody String
derive instance newtypeDashboardBody :: Newtype DashboardBody _
derive instance repGenericDashboardBody :: Generic DashboardBody _
instance showDashboardBody :: Show DashboardBody where
  show = genericShow
instance decodeDashboardBody :: Decode DashboardBody where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardBody :: Encode DashboardBody where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DashboardEntries = DashboardEntries (Array DashboardEntry)
derive instance newtypeDashboardEntries :: Newtype DashboardEntries _
derive instance repGenericDashboardEntries :: Generic DashboardEntries _
instance showDashboardEntries :: Show DashboardEntries where
  show = genericShow
instance decodeDashboardEntries :: Decode DashboardEntries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardEntries :: Encode DashboardEntries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a specific dashboard.</p>
newtype DashboardEntry = DashboardEntry 
  { "DashboardName" :: NullOrUndefined.NullOrUndefined (DashboardName)
  , "DashboardArn" :: NullOrUndefined.NullOrUndefined (DashboardArn)
  , "LastModified" :: NullOrUndefined.NullOrUndefined (LastModified)
  , "Size" :: NullOrUndefined.NullOrUndefined (Size)
  }
derive instance newtypeDashboardEntry :: Newtype DashboardEntry _
derive instance repGenericDashboardEntry :: Generic DashboardEntry _
instance showDashboardEntry :: Show DashboardEntry where
  show = genericShow
instance decodeDashboardEntry :: Decode DashboardEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardEntry :: Encode DashboardEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DashboardErrorMessage = DashboardErrorMessage String
derive instance newtypeDashboardErrorMessage :: Newtype DashboardErrorMessage _
derive instance repGenericDashboardErrorMessage :: Generic DashboardErrorMessage _
instance showDashboardErrorMessage :: Show DashboardErrorMessage where
  show = genericShow
instance decodeDashboardErrorMessage :: Decode DashboardErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardErrorMessage :: Encode DashboardErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Some part of the dashboard data is invalid.</p>
newtype DashboardInvalidInputError = DashboardInvalidInputError 
  { "Message'" :: NullOrUndefined.NullOrUndefined (DashboardErrorMessage)
  , "DashboardValidationMessages'" :: NullOrUndefined.NullOrUndefined (DashboardValidationMessages)
  }
derive instance newtypeDashboardInvalidInputError :: Newtype DashboardInvalidInputError _
derive instance repGenericDashboardInvalidInputError :: Generic DashboardInvalidInputError _
instance showDashboardInvalidInputError :: Show DashboardInvalidInputError where
  show = genericShow
instance decodeDashboardInvalidInputError :: Decode DashboardInvalidInputError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardInvalidInputError :: Encode DashboardInvalidInputError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DashboardName = DashboardName String
derive instance newtypeDashboardName :: Newtype DashboardName _
derive instance repGenericDashboardName :: Generic DashboardName _
instance showDashboardName :: Show DashboardName where
  show = genericShow
instance decodeDashboardName :: Decode DashboardName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardName :: Encode DashboardName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DashboardNamePrefix = DashboardNamePrefix String
derive instance newtypeDashboardNamePrefix :: Newtype DashboardNamePrefix _
derive instance repGenericDashboardNamePrefix :: Generic DashboardNamePrefix _
instance showDashboardNamePrefix :: Show DashboardNamePrefix where
  show = genericShow
instance decodeDashboardNamePrefix :: Decode DashboardNamePrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardNamePrefix :: Encode DashboardNamePrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DashboardNames = DashboardNames (Array DashboardName)
derive instance newtypeDashboardNames :: Newtype DashboardNames _
derive instance repGenericDashboardNames :: Generic DashboardNames _
instance showDashboardNames :: Show DashboardNames where
  show = genericShow
instance decodeDashboardNames :: Decode DashboardNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardNames :: Encode DashboardNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified dashboard does not exist.</p>
newtype DashboardNotFoundError = DashboardNotFoundError 
  { "Message'" :: NullOrUndefined.NullOrUndefined (DashboardErrorMessage)
  }
derive instance newtypeDashboardNotFoundError :: Newtype DashboardNotFoundError _
derive instance repGenericDashboardNotFoundError :: Generic DashboardNotFoundError _
instance showDashboardNotFoundError :: Show DashboardNotFoundError where
  show = genericShow
instance decodeDashboardNotFoundError :: Decode DashboardNotFoundError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardNotFoundError :: Encode DashboardNotFoundError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error or warning for the operation.</p>
newtype DashboardValidationMessage = DashboardValidationMessage 
  { "DataPath" :: NullOrUndefined.NullOrUndefined (DataPath)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeDashboardValidationMessage :: Newtype DashboardValidationMessage _
derive instance repGenericDashboardValidationMessage :: Generic DashboardValidationMessage _
instance showDashboardValidationMessage :: Show DashboardValidationMessage where
  show = genericShow
instance decodeDashboardValidationMessage :: Decode DashboardValidationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardValidationMessage :: Encode DashboardValidationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DashboardValidationMessages = DashboardValidationMessages (Array DashboardValidationMessage)
derive instance newtypeDashboardValidationMessages :: Newtype DashboardValidationMessages _
derive instance repGenericDashboardValidationMessages :: Generic DashboardValidationMessages _
instance showDashboardValidationMessages :: Show DashboardValidationMessages where
  show = genericShow
instance decodeDashboardValidationMessages :: Decode DashboardValidationMessages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDashboardValidationMessages :: Encode DashboardValidationMessages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataPath = DataPath String
derive instance newtypeDataPath :: Newtype DataPath _
derive instance repGenericDataPath :: Generic DataPath _
instance showDataPath :: Show DataPath where
  show = genericShow
instance decodeDataPath :: Decode DataPath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataPath :: Encode DataPath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Encapsulates the statistical data that CloudWatch computes from metric data.</p>
newtype Datapoint = Datapoint 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "SampleCount" :: NullOrUndefined.NullOrUndefined (DatapointValue)
  , "Average" :: NullOrUndefined.NullOrUndefined (DatapointValue)
  , "Sum" :: NullOrUndefined.NullOrUndefined (DatapointValue)
  , "Minimum" :: NullOrUndefined.NullOrUndefined (DatapointValue)
  , "Maximum" :: NullOrUndefined.NullOrUndefined (DatapointValue)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (StandardUnit)
  , "ExtendedStatistics" :: NullOrUndefined.NullOrUndefined (DatapointValueMap)
  }
derive instance newtypeDatapoint :: Newtype Datapoint _
derive instance repGenericDatapoint :: Generic Datapoint _
instance showDatapoint :: Show Datapoint where
  show = genericShow
instance decodeDatapoint :: Decode Datapoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatapoint :: Encode Datapoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DatapointValue = DatapointValue Number
derive instance newtypeDatapointValue :: Newtype DatapointValue _
derive instance repGenericDatapointValue :: Generic DatapointValue _
instance showDatapointValue :: Show DatapointValue where
  show = genericShow
instance decodeDatapointValue :: Decode DatapointValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatapointValue :: Encode DatapointValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DatapointValueMap = DatapointValueMap (StrMap.StrMap DatapointValue)
derive instance newtypeDatapointValueMap :: Newtype DatapointValueMap _
derive instance repGenericDatapointValueMap :: Generic DatapointValueMap _
instance showDatapointValueMap :: Show DatapointValueMap where
  show = genericShow
instance decodeDatapointValueMap :: Decode DatapointValueMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatapointValueMap :: Encode DatapointValueMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Datapoints = Datapoints (Array Datapoint)
derive instance newtypeDatapoints :: Newtype Datapoints _
derive instance repGenericDatapoints :: Generic Datapoints _
instance showDatapoints :: Show Datapoints where
  show = genericShow
instance decodeDatapoints :: Decode Datapoints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatapoints :: Encode Datapoints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DatapointsToAlarm = DatapointsToAlarm Int
derive instance newtypeDatapointsToAlarm :: Newtype DatapointsToAlarm _
derive instance repGenericDatapointsToAlarm :: Generic DatapointsToAlarm _
instance showDatapointsToAlarm :: Show DatapointsToAlarm where
  show = genericShow
instance decodeDatapointsToAlarm :: Decode DatapointsToAlarm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatapointsToAlarm :: Encode DatapointsToAlarm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAlarmsInput = DeleteAlarmsInput 
  { "AlarmNames" :: (AlarmNames)
  }
derive instance newtypeDeleteAlarmsInput :: Newtype DeleteAlarmsInput _
derive instance repGenericDeleteAlarmsInput :: Generic DeleteAlarmsInput _
instance showDeleteAlarmsInput :: Show DeleteAlarmsInput where
  show = genericShow
instance decodeDeleteAlarmsInput :: Decode DeleteAlarmsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAlarmsInput :: Encode DeleteAlarmsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDashboardsInput = DeleteDashboardsInput 
  { "DashboardNames" :: (DashboardNames)
  }
derive instance newtypeDeleteDashboardsInput :: Newtype DeleteDashboardsInput _
derive instance repGenericDeleteDashboardsInput :: Generic DeleteDashboardsInput _
instance showDeleteDashboardsInput :: Show DeleteDashboardsInput where
  show = genericShow
instance decodeDeleteDashboardsInput :: Decode DeleteDashboardsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDashboardsInput :: Encode DeleteDashboardsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDashboardsOutput = DeleteDashboardsOutput Types.NoArguments
derive instance newtypeDeleteDashboardsOutput :: Newtype DeleteDashboardsOutput _
derive instance repGenericDeleteDashboardsOutput :: Generic DeleteDashboardsOutput _
instance showDeleteDashboardsOutput :: Show DeleteDashboardsOutput where
  show = genericShow
instance decodeDeleteDashboardsOutput :: Decode DeleteDashboardsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDashboardsOutput :: Encode DeleteDashboardsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAlarmHistoryInput = DescribeAlarmHistoryInput 
  { "AlarmName" :: NullOrUndefined.NullOrUndefined (AlarmName)
  , "HistoryItemType" :: NullOrUndefined.NullOrUndefined (HistoryItemType)
  , "StartDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndDate" :: NullOrUndefined.NullOrUndefined (Number)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAlarmHistoryInput :: Newtype DescribeAlarmHistoryInput _
derive instance repGenericDescribeAlarmHistoryInput :: Generic DescribeAlarmHistoryInput _
instance showDescribeAlarmHistoryInput :: Show DescribeAlarmHistoryInput where
  show = genericShow
instance decodeDescribeAlarmHistoryInput :: Decode DescribeAlarmHistoryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAlarmHistoryInput :: Encode DescribeAlarmHistoryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAlarmHistoryOutput = DescribeAlarmHistoryOutput 
  { "AlarmHistoryItems" :: NullOrUndefined.NullOrUndefined (AlarmHistoryItems)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAlarmHistoryOutput :: Newtype DescribeAlarmHistoryOutput _
derive instance repGenericDescribeAlarmHistoryOutput :: Generic DescribeAlarmHistoryOutput _
instance showDescribeAlarmHistoryOutput :: Show DescribeAlarmHistoryOutput where
  show = genericShow
instance decodeDescribeAlarmHistoryOutput :: Decode DescribeAlarmHistoryOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAlarmHistoryOutput :: Encode DescribeAlarmHistoryOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAlarmsForMetricInput = DescribeAlarmsForMetricInput 
  { "MetricName" :: (MetricName)
  , "Namespace" :: (Namespace)
  , "Statistic" :: NullOrUndefined.NullOrUndefined (Statistic)
  , "ExtendedStatistic" :: NullOrUndefined.NullOrUndefined (ExtendedStatistic)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (Dimensions)
  , "Period" :: NullOrUndefined.NullOrUndefined (Period)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (StandardUnit)
  }
derive instance newtypeDescribeAlarmsForMetricInput :: Newtype DescribeAlarmsForMetricInput _
derive instance repGenericDescribeAlarmsForMetricInput :: Generic DescribeAlarmsForMetricInput _
instance showDescribeAlarmsForMetricInput :: Show DescribeAlarmsForMetricInput where
  show = genericShow
instance decodeDescribeAlarmsForMetricInput :: Decode DescribeAlarmsForMetricInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAlarmsForMetricInput :: Encode DescribeAlarmsForMetricInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAlarmsForMetricOutput = DescribeAlarmsForMetricOutput 
  { "MetricAlarms" :: NullOrUndefined.NullOrUndefined (MetricAlarms)
  }
derive instance newtypeDescribeAlarmsForMetricOutput :: Newtype DescribeAlarmsForMetricOutput _
derive instance repGenericDescribeAlarmsForMetricOutput :: Generic DescribeAlarmsForMetricOutput _
instance showDescribeAlarmsForMetricOutput :: Show DescribeAlarmsForMetricOutput where
  show = genericShow
instance decodeDescribeAlarmsForMetricOutput :: Decode DescribeAlarmsForMetricOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAlarmsForMetricOutput :: Encode DescribeAlarmsForMetricOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAlarmsInput = DescribeAlarmsInput 
  { "AlarmNames" :: NullOrUndefined.NullOrUndefined (AlarmNames)
  , "AlarmNamePrefix" :: NullOrUndefined.NullOrUndefined (AlarmNamePrefix)
  , "StateValue" :: NullOrUndefined.NullOrUndefined (StateValue)
  , "ActionPrefix" :: NullOrUndefined.NullOrUndefined (ActionPrefix)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAlarmsInput :: Newtype DescribeAlarmsInput _
derive instance repGenericDescribeAlarmsInput :: Generic DescribeAlarmsInput _
instance showDescribeAlarmsInput :: Show DescribeAlarmsInput where
  show = genericShow
instance decodeDescribeAlarmsInput :: Decode DescribeAlarmsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAlarmsInput :: Encode DescribeAlarmsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAlarmsOutput = DescribeAlarmsOutput 
  { "MetricAlarms" :: NullOrUndefined.NullOrUndefined (MetricAlarms)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAlarmsOutput :: Newtype DescribeAlarmsOutput _
derive instance repGenericDescribeAlarmsOutput :: Generic DescribeAlarmsOutput _
instance showDescribeAlarmsOutput :: Show DescribeAlarmsOutput where
  show = genericShow
instance decodeDescribeAlarmsOutput :: Decode DescribeAlarmsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAlarmsOutput :: Encode DescribeAlarmsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Expands the identity of a metric.</p>
newtype Dimension = Dimension 
  { "Name" :: (DimensionName)
  , "Value" :: (DimensionValue)
  }
derive instance newtypeDimension :: Newtype Dimension _
derive instance repGenericDimension :: Generic Dimension _
instance showDimension :: Show Dimension where
  show = genericShow
instance decodeDimension :: Decode Dimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimension :: Encode Dimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents filters for a dimension.</p>
newtype DimensionFilter = DimensionFilter 
  { "Name" :: (DimensionName)
  , "Value" :: NullOrUndefined.NullOrUndefined (DimensionValue)
  }
derive instance newtypeDimensionFilter :: Newtype DimensionFilter _
derive instance repGenericDimensionFilter :: Generic DimensionFilter _
instance showDimensionFilter :: Show DimensionFilter where
  show = genericShow
instance decodeDimensionFilter :: Decode DimensionFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionFilter :: Encode DimensionFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionFilters = DimensionFilters (Array DimensionFilter)
derive instance newtypeDimensionFilters :: Newtype DimensionFilters _
derive instance repGenericDimensionFilters :: Generic DimensionFilters _
instance showDimensionFilters :: Show DimensionFilters where
  show = genericShow
instance decodeDimensionFilters :: Decode DimensionFilters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionFilters :: Encode DimensionFilters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionName = DimensionName String
derive instance newtypeDimensionName :: Newtype DimensionName _
derive instance repGenericDimensionName :: Generic DimensionName _
instance showDimensionName :: Show DimensionName where
  show = genericShow
instance decodeDimensionName :: Decode DimensionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionName :: Encode DimensionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionValue = DimensionValue String
derive instance newtypeDimensionValue :: Newtype DimensionValue _
derive instance repGenericDimensionValue :: Generic DimensionValue _
instance showDimensionValue :: Show DimensionValue where
  show = genericShow
instance decodeDimensionValue :: Decode DimensionValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionValue :: Encode DimensionValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Dimensions = Dimensions (Array Dimension)
derive instance newtypeDimensions :: Newtype Dimensions _
derive instance repGenericDimensions :: Generic Dimensions _
instance showDimensions :: Show Dimensions where
  show = genericShow
instance decodeDimensions :: Decode Dimensions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensions :: Encode Dimensions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableAlarmActionsInput = DisableAlarmActionsInput 
  { "AlarmNames" :: (AlarmNames)
  }
derive instance newtypeDisableAlarmActionsInput :: Newtype DisableAlarmActionsInput _
derive instance repGenericDisableAlarmActionsInput :: Generic DisableAlarmActionsInput _
instance showDisableAlarmActionsInput :: Show DisableAlarmActionsInput where
  show = genericShow
instance decodeDisableAlarmActionsInput :: Decode DisableAlarmActionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableAlarmActionsInput :: Encode DisableAlarmActionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableAlarmActionsInput = EnableAlarmActionsInput 
  { "AlarmNames" :: (AlarmNames)
  }
derive instance newtypeEnableAlarmActionsInput :: Newtype EnableAlarmActionsInput _
derive instance repGenericEnableAlarmActionsInput :: Generic EnableAlarmActionsInput _
instance showEnableAlarmActionsInput :: Show EnableAlarmActionsInput where
  show = genericShow
instance decodeEnableAlarmActionsInput :: Decode EnableAlarmActionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableAlarmActionsInput :: Encode EnableAlarmActionsInput where
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


newtype EvaluateLowSampleCountPercentile = EvaluateLowSampleCountPercentile String
derive instance newtypeEvaluateLowSampleCountPercentile :: Newtype EvaluateLowSampleCountPercentile _
derive instance repGenericEvaluateLowSampleCountPercentile :: Generic EvaluateLowSampleCountPercentile _
instance showEvaluateLowSampleCountPercentile :: Show EvaluateLowSampleCountPercentile where
  show = genericShow
instance decodeEvaluateLowSampleCountPercentile :: Decode EvaluateLowSampleCountPercentile where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluateLowSampleCountPercentile :: Encode EvaluateLowSampleCountPercentile where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EvaluationPeriods = EvaluationPeriods Int
derive instance newtypeEvaluationPeriods :: Newtype EvaluationPeriods _
derive instance repGenericEvaluationPeriods :: Generic EvaluationPeriods _
instance showEvaluationPeriods :: Show EvaluationPeriods where
  show = genericShow
instance decodeEvaluationPeriods :: Decode EvaluationPeriods where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluationPeriods :: Encode EvaluationPeriods where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtendedStatistic = ExtendedStatistic String
derive instance newtypeExtendedStatistic :: Newtype ExtendedStatistic _
derive instance repGenericExtendedStatistic :: Generic ExtendedStatistic _
instance showExtendedStatistic :: Show ExtendedStatistic where
  show = genericShow
instance decodeExtendedStatistic :: Decode ExtendedStatistic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedStatistic :: Encode ExtendedStatistic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtendedStatistics = ExtendedStatistics (Array ExtendedStatistic)
derive instance newtypeExtendedStatistics :: Newtype ExtendedStatistics _
derive instance repGenericExtendedStatistics :: Generic ExtendedStatistics _
instance showExtendedStatistics :: Show ExtendedStatistics where
  show = genericShow
instance decodeExtendedStatistics :: Decode ExtendedStatistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedStatistics :: Encode ExtendedStatistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FaultDescription = FaultDescription String
derive instance newtypeFaultDescription :: Newtype FaultDescription _
derive instance repGenericFaultDescription :: Generic FaultDescription _
instance showFaultDescription :: Show FaultDescription where
  show = genericShow
instance decodeFaultDescription :: Decode FaultDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFaultDescription :: Encode FaultDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDashboardInput = GetDashboardInput 
  { "DashboardName" :: (DashboardName)
  }
derive instance newtypeGetDashboardInput :: Newtype GetDashboardInput _
derive instance repGenericGetDashboardInput :: Generic GetDashboardInput _
instance showGetDashboardInput :: Show GetDashboardInput where
  show = genericShow
instance decodeGetDashboardInput :: Decode GetDashboardInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDashboardInput :: Encode GetDashboardInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDashboardOutput = GetDashboardOutput 
  { "DashboardArn" :: NullOrUndefined.NullOrUndefined (DashboardArn)
  , "DashboardBody" :: NullOrUndefined.NullOrUndefined (DashboardBody)
  , "DashboardName" :: NullOrUndefined.NullOrUndefined (DashboardName)
  }
derive instance newtypeGetDashboardOutput :: Newtype GetDashboardOutput _
derive instance repGenericGetDashboardOutput :: Generic GetDashboardOutput _
instance showGetDashboardOutput :: Show GetDashboardOutput where
  show = genericShow
instance decodeGetDashboardOutput :: Decode GetDashboardOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDashboardOutput :: Encode GetDashboardOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMetricStatisticsInput = GetMetricStatisticsInput 
  { "Namespace" :: (Namespace)
  , "MetricName" :: (MetricName)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (Dimensions)
  , "StartTime" :: (Number)
  , "EndTime" :: (Number)
  , "Period" :: (Period)
  , "Statistics" :: NullOrUndefined.NullOrUndefined (Statistics)
  , "ExtendedStatistics" :: NullOrUndefined.NullOrUndefined (ExtendedStatistics)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (StandardUnit)
  }
derive instance newtypeGetMetricStatisticsInput :: Newtype GetMetricStatisticsInput _
derive instance repGenericGetMetricStatisticsInput :: Generic GetMetricStatisticsInput _
instance showGetMetricStatisticsInput :: Show GetMetricStatisticsInput where
  show = genericShow
instance decodeGetMetricStatisticsInput :: Decode GetMetricStatisticsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMetricStatisticsInput :: Encode GetMetricStatisticsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMetricStatisticsOutput = GetMetricStatisticsOutput 
  { "Label" :: NullOrUndefined.NullOrUndefined (MetricLabel)
  , "Datapoints" :: NullOrUndefined.NullOrUndefined (Datapoints)
  }
derive instance newtypeGetMetricStatisticsOutput :: Newtype GetMetricStatisticsOutput _
derive instance repGenericGetMetricStatisticsOutput :: Generic GetMetricStatisticsOutput _
instance showGetMetricStatisticsOutput :: Show GetMetricStatisticsOutput where
  show = genericShow
instance decodeGetMetricStatisticsOutput :: Decode GetMetricStatisticsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMetricStatisticsOutput :: Encode GetMetricStatisticsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HistoryData = HistoryData String
derive instance newtypeHistoryData :: Newtype HistoryData _
derive instance repGenericHistoryData :: Generic HistoryData _
instance showHistoryData :: Show HistoryData where
  show = genericShow
instance decodeHistoryData :: Decode HistoryData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistoryData :: Encode HistoryData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HistoryItemType = HistoryItemType String
derive instance newtypeHistoryItemType :: Newtype HistoryItemType _
derive instance repGenericHistoryItemType :: Generic HistoryItemType _
instance showHistoryItemType :: Show HistoryItemType where
  show = genericShow
instance decodeHistoryItemType :: Decode HistoryItemType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistoryItemType :: Encode HistoryItemType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HistorySummary = HistorySummary String
derive instance newtypeHistorySummary :: Newtype HistorySummary _
derive instance repGenericHistorySummary :: Generic HistorySummary _
instance showHistorySummary :: Show HistorySummary where
  show = genericShow
instance decodeHistorySummary :: Decode HistorySummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistorySummary :: Encode HistorySummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request processing has failed due to some unknown error, exception, or failure.</p>
newtype InternalServiceFault = InternalServiceFault 
  { "Message" :: NullOrUndefined.NullOrUndefined (FaultDescription)
  }
derive instance newtypeInternalServiceFault :: Newtype InternalServiceFault _
derive instance repGenericInternalServiceFault :: Generic InternalServiceFault _
instance showInternalServiceFault :: Show InternalServiceFault where
  show = genericShow
instance decodeInternalServiceFault :: Decode InternalServiceFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceFault :: Encode InternalServiceFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Data was not syntactically valid JSON.</p>
newtype InvalidFormatFault = InvalidFormatFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidFormatFault :: Newtype InvalidFormatFault _
derive instance repGenericInvalidFormatFault :: Generic InvalidFormatFault _
instance showInvalidFormatFault :: Show InvalidFormatFault where
  show = genericShow
instance decodeInvalidFormatFault :: Decode InvalidFormatFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidFormatFault :: Encode InvalidFormatFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The next token specified is invalid.</p>
newtype InvalidNextToken = InvalidNextToken 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidNextToken :: Newtype InvalidNextToken _
derive instance repGenericInvalidNextToken :: Generic InvalidNextToken _
instance showInvalidNextToken :: Show InvalidNextToken where
  show = genericShow
instance decodeInvalidNextToken :: Decode InvalidNextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextToken :: Encode InvalidNextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Parameters were used together that cannot be used together.</p>
newtype InvalidParameterCombinationException = InvalidParameterCombinationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (AwsQueryErrorMessage)
  }
derive instance newtypeInvalidParameterCombinationException :: Newtype InvalidParameterCombinationException _
derive instance repGenericInvalidParameterCombinationException :: Generic InvalidParameterCombinationException _
instance showInvalidParameterCombinationException :: Show InvalidParameterCombinationException where
  show = genericShow
instance decodeInvalidParameterCombinationException :: Decode InvalidParameterCombinationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterCombinationException :: Encode InvalidParameterCombinationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of an input parameter is bad or out-of-range.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (AwsQueryErrorMessage)
  }
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _
derive instance repGenericInvalidParameterValueException :: Generic InvalidParameterValueException _
instance showInvalidParameterValueException :: Show InvalidParameterValueException where
  show = genericShow
instance decodeInvalidParameterValueException :: Decode InvalidParameterValueException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterValueException :: Encode InvalidParameterValueException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastModified = LastModified Number
derive instance newtypeLastModified :: Newtype LastModified _
derive instance repGenericLastModified :: Generic LastModified _
instance showLastModified :: Show LastModified where
  show = genericShow
instance decodeLastModified :: Decode LastModified where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastModified :: Encode LastModified where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The quota for alarms for this customer has already been reached.</p>
newtype LimitExceededFault = LimitExceededFault 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededFault :: Newtype LimitExceededFault _
derive instance repGenericLimitExceededFault :: Generic LimitExceededFault _
instance showLimitExceededFault :: Show LimitExceededFault where
  show = genericShow
instance decodeLimitExceededFault :: Decode LimitExceededFault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededFault :: Encode LimitExceededFault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDashboardsInput = ListDashboardsInput 
  { "DashboardNamePrefix" :: NullOrUndefined.NullOrUndefined (DashboardNamePrefix)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDashboardsInput :: Newtype ListDashboardsInput _
derive instance repGenericListDashboardsInput :: Generic ListDashboardsInput _
instance showListDashboardsInput :: Show ListDashboardsInput where
  show = genericShow
instance decodeListDashboardsInput :: Decode ListDashboardsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDashboardsInput :: Encode ListDashboardsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDashboardsOutput = ListDashboardsOutput 
  { "DashboardEntries" :: NullOrUndefined.NullOrUndefined (DashboardEntries)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDashboardsOutput :: Newtype ListDashboardsOutput _
derive instance repGenericListDashboardsOutput :: Generic ListDashboardsOutput _
instance showListDashboardsOutput :: Show ListDashboardsOutput where
  show = genericShow
instance decodeListDashboardsOutput :: Decode ListDashboardsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDashboardsOutput :: Encode ListDashboardsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListMetricsInput = ListMetricsInput 
  { "Namespace" :: NullOrUndefined.NullOrUndefined (Namespace)
  , "MetricName" :: NullOrUndefined.NullOrUndefined (MetricName)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (DimensionFilters)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListMetricsInput :: Newtype ListMetricsInput _
derive instance repGenericListMetricsInput :: Generic ListMetricsInput _
instance showListMetricsInput :: Show ListMetricsInput where
  show = genericShow
instance decodeListMetricsInput :: Decode ListMetricsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListMetricsInput :: Encode ListMetricsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListMetricsOutput = ListMetricsOutput 
  { "Metrics" :: NullOrUndefined.NullOrUndefined (Metrics)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListMetricsOutput :: Newtype ListMetricsOutput _
derive instance repGenericListMetricsOutput :: Generic ListMetricsOutput _
instance showListMetricsOutput :: Show ListMetricsOutput where
  show = genericShow
instance decodeListMetricsOutput :: Decode ListMetricsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListMetricsOutput :: Encode ListMetricsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxRecords = MaxRecords Int
derive instance newtypeMaxRecords :: Newtype MaxRecords _
derive instance repGenericMaxRecords :: Generic MaxRecords _
instance showMaxRecords :: Show MaxRecords where
  show = genericShow
instance decodeMaxRecords :: Decode MaxRecords where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxRecords :: Encode MaxRecords where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a specific metric.</p>
newtype Metric = Metric 
  { "Namespace" :: NullOrUndefined.NullOrUndefined (Namespace)
  , "MetricName" :: NullOrUndefined.NullOrUndefined (MetricName)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (Dimensions)
  }
derive instance newtypeMetric :: Newtype Metric _
derive instance repGenericMetric :: Generic Metric _
instance showMetric :: Show Metric where
  show = genericShow
instance decodeMetric :: Decode Metric where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetric :: Encode Metric where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an alarm.</p>
newtype MetricAlarm = MetricAlarm 
  { "AlarmName" :: NullOrUndefined.NullOrUndefined (AlarmName)
  , "AlarmArn" :: NullOrUndefined.NullOrUndefined (AlarmArn)
  , "AlarmDescription" :: NullOrUndefined.NullOrUndefined (AlarmDescription)
  , "AlarmConfigurationUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "ActionsEnabled" :: NullOrUndefined.NullOrUndefined (ActionsEnabled)
  , "OKActions" :: NullOrUndefined.NullOrUndefined (ResourceList)
  , "AlarmActions" :: NullOrUndefined.NullOrUndefined (ResourceList)
  , "InsufficientDataActions" :: NullOrUndefined.NullOrUndefined (ResourceList)
  , "StateValue" :: NullOrUndefined.NullOrUndefined (StateValue)
  , "StateReason" :: NullOrUndefined.NullOrUndefined (StateReason)
  , "StateReasonData" :: NullOrUndefined.NullOrUndefined (StateReasonData)
  , "StateUpdatedTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "MetricName" :: NullOrUndefined.NullOrUndefined (MetricName)
  , "Namespace" :: NullOrUndefined.NullOrUndefined (Namespace)
  , "Statistic" :: NullOrUndefined.NullOrUndefined (Statistic)
  , "ExtendedStatistic" :: NullOrUndefined.NullOrUndefined (ExtendedStatistic)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (Dimensions)
  , "Period" :: NullOrUndefined.NullOrUndefined (Period)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (StandardUnit)
  , "EvaluationPeriods" :: NullOrUndefined.NullOrUndefined (EvaluationPeriods)
  , "DatapointsToAlarm" :: NullOrUndefined.NullOrUndefined (DatapointsToAlarm)
  , "Threshold" :: NullOrUndefined.NullOrUndefined (Threshold)
  , "ComparisonOperator" :: NullOrUndefined.NullOrUndefined (ComparisonOperator)
  , "TreatMissingData" :: NullOrUndefined.NullOrUndefined (TreatMissingData)
  , "EvaluateLowSampleCountPercentile" :: NullOrUndefined.NullOrUndefined (EvaluateLowSampleCountPercentile)
  }
derive instance newtypeMetricAlarm :: Newtype MetricAlarm _
derive instance repGenericMetricAlarm :: Generic MetricAlarm _
instance showMetricAlarm :: Show MetricAlarm where
  show = genericShow
instance decodeMetricAlarm :: Decode MetricAlarm where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricAlarm :: Encode MetricAlarm where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricAlarms = MetricAlarms (Array MetricAlarm)
derive instance newtypeMetricAlarms :: Newtype MetricAlarms _
derive instance repGenericMetricAlarms :: Generic MetricAlarms _
instance showMetricAlarms :: Show MetricAlarms where
  show = genericShow
instance decodeMetricAlarms :: Decode MetricAlarms where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricAlarms :: Encode MetricAlarms where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricData = MetricData (Array MetricDatum)
derive instance newtypeMetricData :: Newtype MetricData _
derive instance repGenericMetricData :: Generic MetricData _
instance showMetricData :: Show MetricData where
  show = genericShow
instance decodeMetricData :: Decode MetricData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricData :: Encode MetricData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Encapsulates the information sent to either create a metric or add new values to be aggregated into an existing metric.</p>
newtype MetricDatum = MetricDatum 
  { "MetricName" :: (MetricName)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (Dimensions)
  , "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Value" :: NullOrUndefined.NullOrUndefined (DatapointValue)
  , "StatisticValues" :: NullOrUndefined.NullOrUndefined (StatisticSet)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (StandardUnit)
  , "StorageResolution" :: NullOrUndefined.NullOrUndefined (StorageResolution)
  }
derive instance newtypeMetricDatum :: Newtype MetricDatum _
derive instance repGenericMetricDatum :: Generic MetricDatum _
instance showMetricDatum :: Show MetricDatum where
  show = genericShow
instance decodeMetricDatum :: Decode MetricDatum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricDatum :: Encode MetricDatum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricLabel = MetricLabel String
derive instance newtypeMetricLabel :: Newtype MetricLabel _
derive instance repGenericMetricLabel :: Generic MetricLabel _
instance showMetricLabel :: Show MetricLabel where
  show = genericShow
instance decodeMetricLabel :: Decode MetricLabel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricLabel :: Encode MetricLabel where
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


newtype Metrics = Metrics (Array Metric)
derive instance newtypeMetrics :: Newtype Metrics _
derive instance repGenericMetrics :: Generic Metrics _
instance showMetrics :: Show Metrics where
  show = genericShow
instance decodeMetrics :: Decode Metrics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetrics :: Encode Metrics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An input parameter that is required is missing.</p>
newtype MissingRequiredParameterException = MissingRequiredParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (AwsQueryErrorMessage)
  }
derive instance newtypeMissingRequiredParameterException :: Newtype MissingRequiredParameterException _
derive instance repGenericMissingRequiredParameterException :: Generic MissingRequiredParameterException _
instance showMissingRequiredParameterException :: Show MissingRequiredParameterException where
  show = genericShow
instance decodeMissingRequiredParameterException :: Decode MissingRequiredParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingRequiredParameterException :: Encode MissingRequiredParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Namespace = Namespace String
derive instance newtypeNamespace :: Newtype Namespace _
derive instance repGenericNamespace :: Generic Namespace _
instance showNamespace :: Show Namespace where
  show = genericShow
instance decodeNamespace :: Decode Namespace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNamespace :: Encode Namespace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Period = Period Int
derive instance newtypePeriod :: Newtype Period _
derive instance repGenericPeriod :: Generic Period _
instance showPeriod :: Show Period where
  show = genericShow
instance decodePeriod :: Decode Period where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePeriod :: Encode Period where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutDashboardInput = PutDashboardInput 
  { "DashboardName" :: (DashboardName)
  , "DashboardBody" :: (DashboardBody)
  }
derive instance newtypePutDashboardInput :: Newtype PutDashboardInput _
derive instance repGenericPutDashboardInput :: Generic PutDashboardInput _
instance showPutDashboardInput :: Show PutDashboardInput where
  show = genericShow
instance decodePutDashboardInput :: Decode PutDashboardInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutDashboardInput :: Encode PutDashboardInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutDashboardOutput = PutDashboardOutput 
  { "DashboardValidationMessages" :: NullOrUndefined.NullOrUndefined (DashboardValidationMessages)
  }
derive instance newtypePutDashboardOutput :: Newtype PutDashboardOutput _
derive instance repGenericPutDashboardOutput :: Generic PutDashboardOutput _
instance showPutDashboardOutput :: Show PutDashboardOutput where
  show = genericShow
instance decodePutDashboardOutput :: Decode PutDashboardOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutDashboardOutput :: Encode PutDashboardOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutMetricAlarmInput = PutMetricAlarmInput 
  { "AlarmName" :: (AlarmName)
  , "AlarmDescription" :: NullOrUndefined.NullOrUndefined (AlarmDescription)
  , "ActionsEnabled" :: NullOrUndefined.NullOrUndefined (ActionsEnabled)
  , "OKActions" :: NullOrUndefined.NullOrUndefined (ResourceList)
  , "AlarmActions" :: NullOrUndefined.NullOrUndefined (ResourceList)
  , "InsufficientDataActions" :: NullOrUndefined.NullOrUndefined (ResourceList)
  , "MetricName" :: (MetricName)
  , "Namespace" :: (Namespace)
  , "Statistic" :: NullOrUndefined.NullOrUndefined (Statistic)
  , "ExtendedStatistic" :: NullOrUndefined.NullOrUndefined (ExtendedStatistic)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (Dimensions)
  , "Period" :: (Period)
  , "Unit''" :: NullOrUndefined.NullOrUndefined (StandardUnit)
  , "EvaluationPeriods" :: (EvaluationPeriods)
  , "DatapointsToAlarm" :: NullOrUndefined.NullOrUndefined (DatapointsToAlarm)
  , "Threshold" :: (Threshold)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "TreatMissingData" :: NullOrUndefined.NullOrUndefined (TreatMissingData)
  , "EvaluateLowSampleCountPercentile" :: NullOrUndefined.NullOrUndefined (EvaluateLowSampleCountPercentile)
  }
derive instance newtypePutMetricAlarmInput :: Newtype PutMetricAlarmInput _
derive instance repGenericPutMetricAlarmInput :: Generic PutMetricAlarmInput _
instance showPutMetricAlarmInput :: Show PutMetricAlarmInput where
  show = genericShow
instance decodePutMetricAlarmInput :: Decode PutMetricAlarmInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutMetricAlarmInput :: Encode PutMetricAlarmInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutMetricDataInput = PutMetricDataInput 
  { "Namespace" :: (Namespace)
  , "MetricData" :: (MetricData)
  }
derive instance newtypePutMetricDataInput :: Newtype PutMetricDataInput _
derive instance repGenericPutMetricDataInput :: Generic PutMetricDataInput _
instance showPutMetricDataInput :: Show PutMetricDataInput where
  show = genericShow
instance decodePutMetricDataInput :: Decode PutMetricDataInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutMetricDataInput :: Encode PutMetricDataInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceList = ResourceList (Array ResourceName)
derive instance newtypeResourceList :: Newtype ResourceList _
derive instance repGenericResourceList :: Generic ResourceList _
instance showResourceList :: Show ResourceList where
  show = genericShow
instance decodeResourceList :: Decode ResourceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceList :: Encode ResourceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _
derive instance repGenericResourceName :: Generic ResourceName _
instance showResourceName :: Show ResourceName where
  show = genericShow
instance decodeResourceName :: Decode ResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceName :: Encode ResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The named resource does not exist.</p>
newtype ResourceNotFound = ResourceNotFound 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFound :: Newtype ResourceNotFound _
derive instance repGenericResourceNotFound :: Generic ResourceNotFound _
instance showResourceNotFound :: Show ResourceNotFound where
  show = genericShow
instance decodeResourceNotFound :: Decode ResourceNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFound :: Encode ResourceNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetAlarmStateInput = SetAlarmStateInput 
  { "AlarmName" :: (AlarmName)
  , "StateValue" :: (StateValue)
  , "StateReason" :: (StateReason)
  , "StateReasonData" :: NullOrUndefined.NullOrUndefined (StateReasonData)
  }
derive instance newtypeSetAlarmStateInput :: Newtype SetAlarmStateInput _
derive instance repGenericSetAlarmStateInput :: Generic SetAlarmStateInput _
instance showSetAlarmStateInput :: Show SetAlarmStateInput where
  show = genericShow
instance decodeSetAlarmStateInput :: Decode SetAlarmStateInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetAlarmStateInput :: Encode SetAlarmStateInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Size = Size Number
derive instance newtypeSize :: Newtype Size _
derive instance repGenericSize :: Generic Size _
instance showSize :: Show Size where
  show = genericShow
instance decodeSize :: Decode Size where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSize :: Encode Size where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StandardUnit = StandardUnit String
derive instance newtypeStandardUnit :: Newtype StandardUnit _
derive instance repGenericStandardUnit :: Generic StandardUnit _
instance showStandardUnit :: Show StandardUnit where
  show = genericShow
instance decodeStandardUnit :: Decode StandardUnit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStandardUnit :: Encode StandardUnit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateReason = StateReason String
derive instance newtypeStateReason :: Newtype StateReason _
derive instance repGenericStateReason :: Generic StateReason _
instance showStateReason :: Show StateReason where
  show = genericShow
instance decodeStateReason :: Decode StateReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateReason :: Encode StateReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateReasonData = StateReasonData String
derive instance newtypeStateReasonData :: Newtype StateReasonData _
derive instance repGenericStateReasonData :: Generic StateReasonData _
instance showStateReasonData :: Show StateReasonData where
  show = genericShow
instance decodeStateReasonData :: Decode StateReasonData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateReasonData :: Encode StateReasonData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateValue = StateValue String
derive instance newtypeStateValue :: Newtype StateValue _
derive instance repGenericStateValue :: Generic StateValue _
instance showStateValue :: Show StateValue where
  show = genericShow
instance decodeStateValue :: Decode StateValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateValue :: Encode StateValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Statistic = Statistic String
derive instance newtypeStatistic :: Newtype Statistic _
derive instance repGenericStatistic :: Generic Statistic _
instance showStatistic :: Show Statistic where
  show = genericShow
instance decodeStatistic :: Decode Statistic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatistic :: Encode Statistic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a set of statistics that describes a specific metric. </p>
newtype StatisticSet = StatisticSet 
  { "SampleCount" :: (DatapointValue)
  , "Sum" :: (DatapointValue)
  , "Minimum" :: (DatapointValue)
  , "Maximum" :: (DatapointValue)
  }
derive instance newtypeStatisticSet :: Newtype StatisticSet _
derive instance repGenericStatisticSet :: Generic StatisticSet _
instance showStatisticSet :: Show StatisticSet where
  show = genericShow
instance decodeStatisticSet :: Decode StatisticSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatisticSet :: Encode StatisticSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Statistics = Statistics (Array Statistic)
derive instance newtypeStatistics :: Newtype Statistics _
derive instance repGenericStatistics :: Generic Statistics _
instance showStatistics :: Show Statistics where
  show = genericShow
instance decodeStatistics :: Decode Statistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatistics :: Encode Statistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageResolution = StorageResolution Int
derive instance newtypeStorageResolution :: Newtype StorageResolution _
derive instance repGenericStorageResolution :: Generic StorageResolution _
instance showStorageResolution :: Show StorageResolution where
  show = genericShow
instance decodeStorageResolution :: Decode StorageResolution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageResolution :: Encode StorageResolution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Threshold = Threshold Number
derive instance newtypeThreshold :: Newtype Threshold _
derive instance repGenericThreshold :: Generic Threshold _
instance showThreshold :: Show Threshold where
  show = genericShow
instance decodeThreshold :: Decode Threshold where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThreshold :: Encode Threshold where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TreatMissingData = TreatMissingData String
derive instance newtypeTreatMissingData :: Newtype TreatMissingData _
derive instance repGenericTreatMissingData :: Generic TreatMissingData _
instance showTreatMissingData :: Show TreatMissingData where
  show = genericShow
instance decodeTreatMissingData :: Decode TreatMissingData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTreatMissingData :: Encode TreatMissingData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
