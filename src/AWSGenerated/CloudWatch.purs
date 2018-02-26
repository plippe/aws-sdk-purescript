

-- | <p>Amazon CloudWatch monitors your Amazon Web Services (AWS) resources and the applications you run on AWS in real time. You can use CloudWatch to collect and track metrics, which are the variables you want to measure for your resources and applications.</p> <p>CloudWatch alarms send notifications or automatically change the resources you are monitoring based on rules that you define. For example, you can monitor the CPU usage and disk reads and writes of your Amazon EC2 instances. Then, use this data to determine whether you should launch additional instances to handle increased load. You can also use this data to stop under-used instances to save money.</p> <p>In addition to monitoring the built-in metrics that come with AWS, you can monitor your own custom metrics. With CloudWatch, you gain system-wide visibility into resource utilization, application performance, and operational health.</p>
module AWS.CloudWatch where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudWatch" :: String


-- | <p>Deletes the specified alarms. In the event of an error, no alarms are deleted.</p>
deleteAlarms :: forall eff. DeleteAlarmsInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteAlarms = AWS.request serviceName "DeleteAlarms" 


-- | <p>Deletes all dashboards that you specify. You may specify up to 100 dashboards to delete. If there is an error during this call, no dashboards are deleted.</p>
deleteDashboards :: forall eff. DeleteDashboardsInput -> Aff (err :: AWS.RequestError | eff) DeleteDashboardsOutput
deleteDashboards = AWS.request serviceName "DeleteDashboards" 


-- | <p>Retrieves the history for the specified alarm. You can filter the results by date range or item type. If an alarm name is not specified, the histories for all alarms are returned.</p> <p>CloudWatch retains the history of an alarm even if you delete the alarm.</p>
describeAlarmHistory :: forall eff. DescribeAlarmHistoryInput -> Aff (err :: AWS.RequestError | eff) DescribeAlarmHistoryOutput
describeAlarmHistory = AWS.request serviceName "DescribeAlarmHistory" 


-- | <p>Retrieves the specified alarms. If no alarms are specified, all alarms are returned. Alarms can be retrieved by using only a prefix for the alarm name, the alarm state, or a prefix for any action.</p>
describeAlarms :: forall eff. DescribeAlarmsInput -> Aff (err :: AWS.RequestError | eff) DescribeAlarmsOutput
describeAlarms = AWS.request serviceName "DescribeAlarms" 


-- | <p>Retrieves the alarms for the specified metric. To filter the results, specify a statistic, period, or unit.</p>
describeAlarmsForMetric :: forall eff. DescribeAlarmsForMetricInput -> Aff (err :: AWS.RequestError | eff) DescribeAlarmsForMetricOutput
describeAlarmsForMetric = AWS.request serviceName "DescribeAlarmsForMetric" 


-- | <p>Disables the actions for the specified alarms. When an alarm's actions are disabled, the alarm actions do not execute when the alarm state changes.</p>
disableAlarmActions :: forall eff. DisableAlarmActionsInput -> Aff (err :: AWS.RequestError | eff) Unit
disableAlarmActions = AWS.request serviceName "DisableAlarmActions" 


-- | <p>Enables the actions for the specified alarms.</p>
enableAlarmActions :: forall eff. EnableAlarmActionsInput -> Aff (err :: AWS.RequestError | eff) Unit
enableAlarmActions = AWS.request serviceName "EnableAlarmActions" 


-- | <p>Displays the details of the dashboard that you specify.</p> <p>To copy an existing dashboard, use <code>GetDashboard</code>, and then use the data returned within <code>DashboardBody</code> as the template for the new dashboard when you call <code>PutDashboard</code> to create the copy.</p>
getDashboard :: forall eff. GetDashboardInput -> Aff (err :: AWS.RequestError | eff) GetDashboardOutput
getDashboard = AWS.request serviceName "GetDashboard" 


-- | <p>Gets statistics for the specified metric.</p> <p>The maximum number of data points returned from a single call is 1,440. If you request more than 1,440 data points, CloudWatch returns an error. To reduce the number of data points, you can narrow the specified time range and make multiple requests across adjacent time ranges, or you can increase the specified period. Data points are not returned in chronological order.</p> <p>CloudWatch aggregates data points based on the length of the period that you specify. For example, if you request statistics with a one-hour period, CloudWatch aggregates all data points with time stamps that fall within each one-hour period. Therefore, the number of values aggregated by CloudWatch is larger than the number of data points returned.</p> <p>CloudWatch needs raw data points to calculate percentile statistics. If you publish data using a statistic set instead, you can only retrieve percentile statistics for this data if one of the following conditions is true:</p> <ul> <li> <p>The SampleCount value of the statistic set is 1.</p> </li> <li> <p>The Min and the Max values of the statistic set are equal.</p> </li> </ul> <p>Amazon CloudWatch retains metric data as follows:</p> <ul> <li> <p>Data points with a period of less than 60 seconds are available for 3 hours. These data points are high-resolution metrics and are available only for custom metrics that have been defined with a <code>StorageResolution</code> of 1.</p> </li> <li> <p>Data points with a period of 60 seconds (1-minute) are available for 15 days.</p> </li> <li> <p>Data points with a period of 300 seconds (5-minute) are available for 63 days.</p> </li> <li> <p>Data points with a period of 3600 seconds (1 hour) are available for 455 days (15 months).</p> </li> </ul> <p>Data points that are initially published with a shorter period are aggregated together for long-term storage. For example, if you collect data using a period of 1 minute, the data remains available for 15 days with 1-minute resolution. After 15 days, this data is still available, but is aggregated and retrievable only with a resolution of 5 minutes. After 63 days, the data is further aggregated and is available with a resolution of 1 hour.</p> <p>CloudWatch started retaining 5-minute and 1-hour metric data as of July 9, 2016.</p> <p>For information about metrics and dimensions supported by AWS services, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CW_Support_For_AWS.html">Amazon CloudWatch Metrics and Dimensions Reference</a> in the <i>Amazon CloudWatch User Guide</i>.</p>
getMetricStatistics :: forall eff. GetMetricStatisticsInput -> Aff (err :: AWS.RequestError | eff) GetMetricStatisticsOutput
getMetricStatistics = AWS.request serviceName "GetMetricStatistics" 


-- | <p>Returns a list of the dashboards for your account. If you include <code>DashboardNamePrefix</code>, only those dashboards with names starting with the prefix are listed. Otherwise, all dashboards in your account are listed. </p>
listDashboards :: forall eff. ListDashboardsInput -> Aff (err :: AWS.RequestError | eff) ListDashboardsOutput
listDashboards = AWS.request serviceName "ListDashboards" 


-- | <p>List the specified metrics. You can use the returned metrics with <a>GetMetricStatistics</a> to obtain statistical data.</p> <p>Up to 500 results are returned for any one call. To retrieve additional results, use the returned token with subsequent calls.</p> <p>After you create a metric, allow up to fifteen minutes before the metric appears. Statistics about the metric, however, are available sooner using <a>GetMetricStatistics</a>.</p>
listMetrics :: forall eff. ListMetricsInput -> Aff (err :: AWS.RequestError | eff) ListMetricsOutput
listMetrics = AWS.request serviceName "ListMetrics" 


-- | <p>Creates a dashboard if it does not already exist, or updates an existing dashboard. If you update a dashboard, the entire contents are replaced with what you specify here.</p> <p>You can have up to 500 dashboards per account. All dashboards in your account are global, not region-specific.</p> <p>A simple way to create a dashboard using <code>PutDashboard</code> is to copy an existing dashboard. To copy an existing dashboard using the console, you can load the dashboard and then use the View/edit source command in the Actions menu to display the JSON block for that dashboard. Another way to copy a dashboard is to use <code>GetDashboard</code>, and then use the data returned within <code>DashboardBody</code> as the template for the new dashboard when you call <code>PutDashboard</code>.</p> <p>When you create a dashboard with <code>PutDashboard</code>, a good practice is to add a text widget at the top of the dashboard with a message that the dashboard was created by script and should not be changed in the console. This message could also point console users to the location of the <code>DashboardBody</code> script or the CloudFormation template used to create the dashboard.</p>
putDashboard :: forall eff. PutDashboardInput -> Aff (err :: AWS.RequestError | eff) PutDashboardOutput
putDashboard = AWS.request serviceName "PutDashboard" 


-- | <p>Creates or updates an alarm and associates it with the specified metric. Optionally, this operation can associate one or more Amazon SNS resources with the alarm.</p> <p>When this operation creates an alarm, the alarm state is immediately set to <code>INSUFFICIENT_DATA</code>. The alarm is evaluated and its state is set appropriately. Any actions associated with the state are then executed.</p> <p>When you update an existing alarm, its state is left unchanged, but the update completely overwrites the previous configuration of the alarm.</p> <p>If you are an IAM user, you must have Amazon EC2 permissions for some operations:</p> <ul> <li> <p> <code>iam:CreateServiceLinkedRole</code> for all alarms with EC2 actions</p> </li> <li> <p> <code>ec2:DescribeInstanceStatus</code> and <code>ec2:DescribeInstances</code> for all alarms on EC2 instance status metrics</p> </li> <li> <p> <code>ec2:StopInstances</code> for alarms with stop actions</p> </li> <li> <p> <code>ec2:TerminateInstances</code> for alarms with terminate actions</p> </li> <li> <p> <code>ec2:DescribeInstanceRecoveryAttribute</code> and <code>ec2:RecoverInstances</code> for alarms with recover actions</p> </li> </ul> <p>If you have read/write permissions for Amazon CloudWatch but not for Amazon EC2, you can still create an alarm, but the stop or terminate actions are not performed. However, if you are later granted the required permissions, the alarm actions that you created earlier are performed.</p> <p>If you are using an IAM role (for example, an EC2 instance profile), you cannot stop or terminate the instance using alarm actions. However, you can still see the alarm state and perform any other actions such as Amazon SNS notifications or Auto Scaling policies.</p> <p>If you are using temporary security credentials granted using AWS STS, you cannot stop or terminate an EC2 instance using alarm actions.</p> <p>You must create at least one stop, terminate, or reboot alarm using either the Amazon EC2 or CloudWatch consoles to create the <b>EC2ActionsAccess</b> IAM role. After this IAM role is created, you can create stop, terminate, or reboot alarms using a command-line interface or API.</p>
putMetricAlarm :: forall eff. PutMetricAlarmInput -> Aff (err :: AWS.RequestError | eff) Unit
putMetricAlarm = AWS.request serviceName "PutMetricAlarm" 


-- | <p>Publishes metric data points to Amazon CloudWatch. CloudWatch associates the data points with the specified metric. If the specified metric does not exist, CloudWatch creates the metric. When CloudWatch creates a metric, it can take up to fifteen minutes for the metric to appear in calls to <a>ListMetrics</a>.</p> <p>Each <code>PutMetricData</code> request is limited to 40 KB in size for HTTP POST requests.</p> <p>Although the <code>Value</code> parameter accepts numbers of type <code>Double</code>, CloudWatch rejects values that are either too small or too large. Values must be in the range of 8.515920e-109 to 1.174271e+108 (Base 10) or 2e-360 to 2e360 (Base 2). In addition, special values (for example, NaN, +Infinity, -Infinity) are not supported.</p> <p>You can use up to 10 dimensions per metric to further clarify what data the metric collects. For more information about specifying dimensions, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html">Publishing Metrics</a> in the <i>Amazon CloudWatch User Guide</i>.</p> <p>Data points with time stamps from 24 hours ago or longer can take at least 48 hours to become available for <a>GetMetricStatistics</a> from the time they are submitted.</p> <p>CloudWatch needs raw data points to calculate percentile statistics. If you publish data using a statistic set instead, you can only retrieve percentile statistics for this data if one of the following conditions is true:</p> <ul> <li> <p>The SampleCount value of the statistic set is 1</p> </li> <li> <p>The Min and the Max values of the statistic set are equal</p> </li> </ul>
putMetricData :: forall eff. PutMetricDataInput -> Aff (err :: AWS.RequestError | eff) Unit
putMetricData = AWS.request serviceName "PutMetricData" 


-- | <p>Temporarily sets the state of an alarm for testing purposes. When the updated state differs from the previous value, the action configured for the appropriate state is invoked. For example, if your alarm is configured to send an Amazon SNS message when an alarm is triggered, temporarily changing the alarm state to <code>ALARM</code> sends an SNS message. The alarm returns to its actual state (often within seconds). Because the alarm state change happens quickly, it is typically only visible in the alarm's <b>History</b> tab in the Amazon CloudWatch console or through <a>DescribeAlarmHistory</a>.</p>
setAlarmState :: forall eff. SetAlarmStateInput -> Aff (err :: AWS.RequestError | eff) Unit
setAlarmState = AWS.request serviceName "SetAlarmState" 


newtype ActionPrefix = ActionPrefix String


newtype ActionsEnabled = ActionsEnabled Boolean


newtype AlarmArn = AlarmArn String


newtype AlarmDescription = AlarmDescription String


-- | <p>Represents the history of a specific alarm.</p>
newtype AlarmHistoryItem = AlarmHistoryItem 
  { "AlarmName" :: NullOrUndefined (AlarmName)
  , "Number" :: NullOrUndefined (Number)
  , "HistoryItemType" :: NullOrUndefined (HistoryItemType)
  , "HistorySummary" :: NullOrUndefined (HistorySummary)
  , "HistoryData" :: NullOrUndefined (HistoryData)
  }


newtype AlarmHistoryItems = AlarmHistoryItems (Array AlarmHistoryItem)


newtype AlarmName = AlarmName String


newtype AlarmNamePrefix = AlarmNamePrefix String


newtype AlarmNames = AlarmNames (Array AlarmName)


newtype AwsQueryErrorMessage = AwsQueryErrorMessage String


newtype ComparisonOperator = ComparisonOperator String


newtype DashboardArn = DashboardArn String


newtype DashboardBody = DashboardBody String


newtype DashboardEntries = DashboardEntries (Array DashboardEntry)


-- | <p>Represents a specific dashboard.</p>
newtype DashboardEntry = DashboardEntry 
  { "DashboardName" :: NullOrUndefined (DashboardName)
  , "DashboardArn" :: NullOrUndefined (DashboardArn)
  , "LastModified" :: NullOrUndefined (LastModified)
  , "Size" :: NullOrUndefined (Size)
  }


newtype DashboardErrorMessage = DashboardErrorMessage String


-- | <p>Some part of the dashboard data is invalid.</p>
newtype DashboardInvalidInputError = DashboardInvalidInputError 
  { "Message'" :: NullOrUndefined (DashboardErrorMessage)
  , "DashboardValidationMessages'" :: NullOrUndefined (DashboardValidationMessages)
  }


newtype DashboardName = DashboardName String


newtype DashboardNamePrefix = DashboardNamePrefix String


newtype DashboardNames = DashboardNames (Array DashboardName)


-- | <p>The specified dashboard does not exist.</p>
newtype DashboardNotFoundError = DashboardNotFoundError 
  { "Message'" :: NullOrUndefined (DashboardErrorMessage)
  }


-- | <p>An error or warning for the operation.</p>
newtype DashboardValidationMessage = DashboardValidationMessage 
  { "DataPath" :: NullOrUndefined (DataPath)
  , "Message" :: NullOrUndefined (Message)
  }


newtype DashboardValidationMessages = DashboardValidationMessages (Array DashboardValidationMessage)


newtype DataPath = DataPath String


-- | <p>Encapsulates the statistical data that CloudWatch computes from metric data.</p>
newtype Datapoint = Datapoint 
  { "Number" :: NullOrUndefined (Number)
  , "SampleCount" :: NullOrUndefined (DatapointValue)
  , "Average" :: NullOrUndefined (DatapointValue)
  , "Sum" :: NullOrUndefined (DatapointValue)
  , "Minimum" :: NullOrUndefined (DatapointValue)
  , "Maximum" :: NullOrUndefined (DatapointValue)
  , "Unit''" :: NullOrUndefined (StandardUnit)
  , "ExtendedStatistics" :: NullOrUndefined (DatapointValueMap)
  }


newtype DatapointValue = DatapointValue Number


newtype DatapointValueMap = DatapointValueMap (Map ExtendedStatistic DatapointValue)


newtype Datapoints = Datapoints (Array Datapoint)


newtype DatapointsToAlarm = DatapointsToAlarm Int


newtype DeleteAlarmsInput = DeleteAlarmsInput 
  { "AlarmNames" :: (AlarmNames)
  }


newtype DeleteDashboardsInput = DeleteDashboardsInput 
  { "DashboardNames" :: (DashboardNames)
  }


newtype DeleteDashboardsOutput = DeleteDashboardsOutput 
  { 
  }


newtype DescribeAlarmHistoryInput = DescribeAlarmHistoryInput 
  { "AlarmName" :: NullOrUndefined (AlarmName)
  , "HistoryItemType" :: NullOrUndefined (HistoryItemType)
  , "StartDate" :: NullOrUndefined (Number)
  , "EndDate" :: NullOrUndefined (Number)
  , "MaxRecords" :: NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeAlarmHistoryOutput = DescribeAlarmHistoryOutput 
  { "AlarmHistoryItems" :: NullOrUndefined (AlarmHistoryItems)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeAlarmsForMetricInput = DescribeAlarmsForMetricInput 
  { "MetricName" :: (MetricName)
  , "Namespace" :: (Namespace)
  , "Statistic" :: NullOrUndefined (Statistic)
  , "ExtendedStatistic" :: NullOrUndefined (ExtendedStatistic)
  , "Dimensions" :: NullOrUndefined (Dimensions)
  , "Period" :: NullOrUndefined (Period)
  , "Unit''" :: NullOrUndefined (StandardUnit)
  }


newtype DescribeAlarmsForMetricOutput = DescribeAlarmsForMetricOutput 
  { "MetricAlarms" :: NullOrUndefined (MetricAlarms)
  }


newtype DescribeAlarmsInput = DescribeAlarmsInput 
  { "AlarmNames" :: NullOrUndefined (AlarmNames)
  , "AlarmNamePrefix" :: NullOrUndefined (AlarmNamePrefix)
  , "StateValue" :: NullOrUndefined (StateValue)
  , "ActionPrefix" :: NullOrUndefined (ActionPrefix)
  , "MaxRecords" :: NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeAlarmsOutput = DescribeAlarmsOutput 
  { "MetricAlarms" :: NullOrUndefined (MetricAlarms)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Expands the identity of a metric.</p>
newtype Dimension = Dimension 
  { "Name" :: (DimensionName)
  , "Value" :: (DimensionValue)
  }


-- | <p>Represents filters for a dimension.</p>
newtype DimensionFilter = DimensionFilter 
  { "Name" :: (DimensionName)
  , "Value" :: NullOrUndefined (DimensionValue)
  }


newtype DimensionFilters = DimensionFilters (Array DimensionFilter)


newtype DimensionName = DimensionName String


newtype DimensionValue = DimensionValue String


newtype Dimensions = Dimensions (Array Dimension)


newtype DisableAlarmActionsInput = DisableAlarmActionsInput 
  { "AlarmNames" :: (AlarmNames)
  }


newtype EnableAlarmActionsInput = EnableAlarmActionsInput 
  { "AlarmNames" :: (AlarmNames)
  }


newtype ErrorMessage = ErrorMessage String


newtype EvaluateLowSampleCountPercentile = EvaluateLowSampleCountPercentile String


newtype EvaluationPeriods = EvaluationPeriods Int


newtype ExtendedStatistic = ExtendedStatistic String


newtype ExtendedStatistics = ExtendedStatistics (Array ExtendedStatistic)


newtype FaultDescription = FaultDescription String


newtype GetDashboardInput = GetDashboardInput 
  { "DashboardName" :: (DashboardName)
  }


newtype GetDashboardOutput = GetDashboardOutput 
  { "DashboardArn" :: NullOrUndefined (DashboardArn)
  , "DashboardBody" :: NullOrUndefined (DashboardBody)
  , "DashboardName" :: NullOrUndefined (DashboardName)
  }


newtype GetMetricStatisticsInput = GetMetricStatisticsInput 
  { "Namespace" :: (Namespace)
  , "MetricName" :: (MetricName)
  , "Dimensions" :: NullOrUndefined (Dimensions)
  , "StartTime" :: (Number)
  , "EndTime" :: (Number)
  , "Period" :: (Period)
  , "Statistics" :: NullOrUndefined (Statistics)
  , "ExtendedStatistics" :: NullOrUndefined (ExtendedStatistics)
  , "Unit''" :: NullOrUndefined (StandardUnit)
  }


newtype GetMetricStatisticsOutput = GetMetricStatisticsOutput 
  { "Label" :: NullOrUndefined (MetricLabel)
  , "Datapoints" :: NullOrUndefined (Datapoints)
  }


newtype HistoryData = HistoryData String


newtype HistoryItemType = HistoryItemType String


newtype HistorySummary = HistorySummary String


-- | <p>Request processing has failed due to some unknown error, exception, or failure.</p>
newtype InternalServiceFault = InternalServiceFault 
  { "Message" :: NullOrUndefined (FaultDescription)
  }


-- | <p>Data was not syntactically valid JSON.</p>
newtype InvalidFormatFault = InvalidFormatFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The next token specified is invalid.</p>
newtype InvalidNextToken = InvalidNextToken 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Parameters were used together that cannot be used together.</p>
newtype InvalidParameterCombinationException = InvalidParameterCombinationException 
  { "Message'" :: NullOrUndefined (AwsQueryErrorMessage)
  }


-- | <p>The value of an input parameter is bad or out-of-range.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined (AwsQueryErrorMessage)
  }


newtype LastModified = LastModified Number


-- | <p>The quota for alarms for this customer has already been reached.</p>
newtype LimitExceededFault = LimitExceededFault 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype ListDashboardsInput = ListDashboardsInput 
  { "DashboardNamePrefix" :: NullOrUndefined (DashboardNamePrefix)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListDashboardsOutput = ListDashboardsOutput 
  { "DashboardEntries" :: NullOrUndefined (DashboardEntries)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListMetricsInput = ListMetricsInput 
  { "Namespace" :: NullOrUndefined (Namespace)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "Dimensions" :: NullOrUndefined (DimensionFilters)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListMetricsOutput = ListMetricsOutput 
  { "Metrics" :: NullOrUndefined (Metrics)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype MaxRecords = MaxRecords Int


newtype Message = Message String


-- | <p>Represents a specific metric.</p>
newtype Metric = Metric 
  { "Namespace" :: NullOrUndefined (Namespace)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "Dimensions" :: NullOrUndefined (Dimensions)
  }


-- | <p>Represents an alarm.</p>
newtype MetricAlarm = MetricAlarm 
  { "AlarmName" :: NullOrUndefined (AlarmName)
  , "AlarmArn" :: NullOrUndefined (AlarmArn)
  , "AlarmDescription" :: NullOrUndefined (AlarmDescription)
  , "AlarmConfigurationUpdatedTimestamp" :: NullOrUndefined (Number)
  , "ActionsEnabled" :: NullOrUndefined (ActionsEnabled)
  , "OKActions" :: NullOrUndefined (ResourceList)
  , "AlarmActions" :: NullOrUndefined (ResourceList)
  , "InsufficientDataActions" :: NullOrUndefined (ResourceList)
  , "StateValue" :: NullOrUndefined (StateValue)
  , "StateReason" :: NullOrUndefined (StateReason)
  , "StateReasonData" :: NullOrUndefined (StateReasonData)
  , "StateUpdatedTimestamp" :: NullOrUndefined (Number)
  , "MetricName" :: NullOrUndefined (MetricName)
  , "Namespace" :: NullOrUndefined (Namespace)
  , "Statistic" :: NullOrUndefined (Statistic)
  , "ExtendedStatistic" :: NullOrUndefined (ExtendedStatistic)
  , "Dimensions" :: NullOrUndefined (Dimensions)
  , "Period" :: NullOrUndefined (Period)
  , "Unit''" :: NullOrUndefined (StandardUnit)
  , "EvaluationPeriods" :: NullOrUndefined (EvaluationPeriods)
  , "DatapointsToAlarm" :: NullOrUndefined (DatapointsToAlarm)
  , "Threshold" :: NullOrUndefined (Threshold)
  , "ComparisonOperator" :: NullOrUndefined (ComparisonOperator)
  , "TreatMissingData" :: NullOrUndefined (TreatMissingData)
  , "EvaluateLowSampleCountPercentile" :: NullOrUndefined (EvaluateLowSampleCountPercentile)
  }


newtype MetricAlarms = MetricAlarms (Array MetricAlarm)


newtype MetricData = MetricData (Array MetricDatum)


-- | <p>Encapsulates the information sent to either create a metric or add new values to be aggregated into an existing metric.</p>
newtype MetricDatum = MetricDatum 
  { "MetricName" :: (MetricName)
  , "Dimensions" :: NullOrUndefined (Dimensions)
  , "Number" :: NullOrUndefined (Number)
  , "Value" :: NullOrUndefined (DatapointValue)
  , "StatisticValues" :: NullOrUndefined (StatisticSet)
  , "Unit''" :: NullOrUndefined (StandardUnit)
  , "StorageResolution" :: NullOrUndefined (StorageResolution)
  }


newtype MetricLabel = MetricLabel String


newtype MetricName = MetricName String


newtype Metrics = Metrics (Array Metric)


-- | <p>An input parameter that is required is missing.</p>
newtype MissingRequiredParameterException = MissingRequiredParameterException 
  { "Message'" :: NullOrUndefined (AwsQueryErrorMessage)
  }


newtype Namespace = Namespace String


newtype NextToken = NextToken String


newtype Period = Period Int


newtype PutDashboardInput = PutDashboardInput 
  { "DashboardName" :: (DashboardName)
  , "DashboardBody" :: (DashboardBody)
  }


newtype PutDashboardOutput = PutDashboardOutput 
  { "DashboardValidationMessages" :: NullOrUndefined (DashboardValidationMessages)
  }


newtype PutMetricAlarmInput = PutMetricAlarmInput 
  { "AlarmName" :: (AlarmName)
  , "AlarmDescription" :: NullOrUndefined (AlarmDescription)
  , "ActionsEnabled" :: NullOrUndefined (ActionsEnabled)
  , "OKActions" :: NullOrUndefined (ResourceList)
  , "AlarmActions" :: NullOrUndefined (ResourceList)
  , "InsufficientDataActions" :: NullOrUndefined (ResourceList)
  , "MetricName" :: (MetricName)
  , "Namespace" :: (Namespace)
  , "Statistic" :: NullOrUndefined (Statistic)
  , "ExtendedStatistic" :: NullOrUndefined (ExtendedStatistic)
  , "Dimensions" :: NullOrUndefined (Dimensions)
  , "Period" :: (Period)
  , "Unit''" :: NullOrUndefined (StandardUnit)
  , "EvaluationPeriods" :: (EvaluationPeriods)
  , "DatapointsToAlarm" :: NullOrUndefined (DatapointsToAlarm)
  , "Threshold" :: (Threshold)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "TreatMissingData" :: NullOrUndefined (TreatMissingData)
  , "EvaluateLowSampleCountPercentile" :: NullOrUndefined (EvaluateLowSampleCountPercentile)
  }


newtype PutMetricDataInput = PutMetricDataInput 
  { "Namespace" :: (Namespace)
  , "MetricData" :: (MetricData)
  }


newtype ResourceList = ResourceList (Array ResourceName)


newtype ResourceName = ResourceName String


-- | <p>The named resource does not exist.</p>
newtype ResourceNotFound = ResourceNotFound 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype SetAlarmStateInput = SetAlarmStateInput 
  { "AlarmName" :: (AlarmName)
  , "StateValue" :: (StateValue)
  , "StateReason" :: (StateReason)
  , "StateReasonData" :: NullOrUndefined (StateReasonData)
  }


newtype Size = Size Number


newtype StandardUnit = StandardUnit String


newtype StateReason = StateReason String


newtype StateReasonData = StateReasonData String


newtype StateValue = StateValue String


newtype Statistic = Statistic String


-- | <p>Represents a set of statistics that describes a specific metric. </p>
newtype StatisticSet = StatisticSet 
  { "SampleCount" :: (DatapointValue)
  , "Sum" :: (DatapointValue)
  , "Minimum" :: (DatapointValue)
  , "Maximum" :: (DatapointValue)
  }


newtype Statistics = Statistics (Array Statistic)


newtype StorageResolution = StorageResolution Int


newtype Threshold = Threshold Number


newtype TreatMissingData = TreatMissingData String
