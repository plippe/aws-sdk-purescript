

-- | <p>You can use Amazon CloudWatch Logs to monitor, store, and access your log files from Amazon EC2 instances, AWS CloudTrail, or other sources. You can then retrieve the associated log data from CloudWatch Logs using the CloudWatch console, CloudWatch Logs commands in the AWS CLI, CloudWatch Logs API, or CloudWatch Logs SDK.</p> <p>You can use CloudWatch Logs to:</p> <ul> <li> <p> <b>Monitor logs from EC2 instances in real-time</b>: You can use CloudWatch Logs to monitor applications and systems using log data. For example, CloudWatch Logs can track the number of errors that occur in your application logs and send you a notification whenever the rate of errors exceeds a threshold that you specify. CloudWatch Logs uses your log data for monitoring; so, no code changes are required. For example, you can monitor application logs for specific literal terms (such as "NullReferenceException") or count the number of occurrences of a literal term at a particular position in log data (such as "404" status codes in an Apache access log). When the term you are searching for is found, CloudWatch Logs reports the data to a CloudWatch metric that you specify.</p> </li> <li> <p> <b>Monitor AWS CloudTrail logged events</b>: You can create alarms in CloudWatch and receive notifications of particular API activity as captured by CloudTrail and use the notification to perform troubleshooting.</p> </li> <li> <p> <b>Archive log data</b>: You can use CloudWatch Logs to store your log data in highly durable storage. You can change the log retention setting so that any log events older than this setting are automatically deleted. The CloudWatch Logs agent makes it easy to quickly send both rotated and non-rotated log data off of a host and into the log service. You can then access the raw log data when you need it.</p> </li> </ul>
module AWS.CloudWatchLogs where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudWatchLogs" :: String


-- | <p>Associates the specified AWS Key Management Service (AWS KMS) customer master key (CMK) with the specified log group.</p> <p>Associating an AWS KMS CMK with a log group overrides any existing associations between the log group and a CMK. After a CMK is associated with a log group, all newly ingested data for the log group is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.</p> <p>Note that it can take up to 5 minutes for this operation to take effect.</p> <p>If you attempt to associate a CMK with a log group but the CMK does not exist or the CMK is disabled, you will receive an <code>InvalidParameterException</code> error. </p>
associateKmsKey :: forall eff. AssociateKmsKeyRequest -> Aff (err :: AWS.RequestError | eff) Unit
associateKmsKey = AWS.request serviceName "AssociateKmsKey" 


-- | <p>Cancels the specified export task.</p> <p>The task must be in the <code>PENDING</code> or <code>RUNNING</code> state.</p>
cancelExportTask :: forall eff. CancelExportTaskRequest -> Aff (err :: AWS.RequestError | eff) Unit
cancelExportTask = AWS.request serviceName "CancelExportTask" 


-- | <p>Creates an export task, which allows you to efficiently export data from a log group to an Amazon S3 bucket.</p> <p>This is an asynchronous call. If all the required information is provided, this operation initiates an export task and responds with the ID of the task. After the task has started, you can use <a>DescribeExportTasks</a> to get the status of the export task. Each account can only have one active (<code>RUNNING</code> or <code>PENDING</code>) export task at a time. To cancel an export task, use <a>CancelExportTask</a>.</p> <p>You can export logs from multiple log groups or multiple time ranges to the same S3 bucket. To separate out log data for each export task, you can specify a prefix to be used as the Amazon S3 key prefix for all exported objects.</p>
createExportTask :: forall eff. CreateExportTaskRequest -> Aff (err :: AWS.RequestError | eff) CreateExportTaskResponse
createExportTask = AWS.request serviceName "CreateExportTask" 


-- | <p>Creates a log group with the specified name.</p> <p>You can create up to 5000 log groups per account.</p> <p>You must use the following guidelines when naming a log group:</p> <ul> <li> <p>Log group names must be unique within a region for an AWS account.</p> </li> <li> <p>Log group names can be between 1 and 512 characters long.</p> </li> <li> <p>Log group names consist of the following characters: a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).</p> </li> </ul> <p>If you associate a AWS Key Management Service (AWS KMS) customer master key (CMK) with the log group, ingested data is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.</p> <p>If you attempt to associate a CMK with the log group but the CMK does not exist or the CMK is disabled, you will receive an <code>InvalidParameterException</code> error. </p>
createLogGroup :: forall eff. CreateLogGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
createLogGroup = AWS.request serviceName "CreateLogGroup" 


-- | <p>Creates a log stream for the specified log group.</p> <p>There is no limit on the number of log streams that you can create for a log group.</p> <p>You must use the following guidelines when naming a log stream:</p> <ul> <li> <p>Log stream names must be unique within the log group.</p> </li> <li> <p>Log stream names can be between 1 and 512 characters long.</p> </li> <li> <p>The ':' (colon) and '*' (asterisk) characters are not allowed.</p> </li> </ul>
createLogStream :: forall eff. CreateLogStreamRequest -> Aff (err :: AWS.RequestError | eff) Unit
createLogStream = AWS.request serviceName "CreateLogStream" 


-- | <p>Deletes the specified destination, and eventually disables all the subscription filters that publish to it. This operation does not delete the physical resource encapsulated by the destination.</p>
deleteDestination :: forall eff. DeleteDestinationRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteDestination = AWS.request serviceName "DeleteDestination" 


-- | <p>Deletes the specified log group and permanently deletes all the archived log events associated with the log group.</p>
deleteLogGroup :: forall eff. DeleteLogGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteLogGroup = AWS.request serviceName "DeleteLogGroup" 


-- | <p>Deletes the specified log stream and permanently deletes all the archived log events associated with the log stream.</p>
deleteLogStream :: forall eff. DeleteLogStreamRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteLogStream = AWS.request serviceName "DeleteLogStream" 


-- | <p>Deletes the specified metric filter.</p>
deleteMetricFilter :: forall eff. DeleteMetricFilterRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteMetricFilter = AWS.request serviceName "DeleteMetricFilter" 


-- | <p>Deletes a resource policy from this account. This revokes the access of the identities in that policy to put log events to this account.</p>
deleteResourcePolicy :: forall eff. DeleteResourcePolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteResourcePolicy = AWS.request serviceName "DeleteResourcePolicy" 


-- | <p>Deletes the specified retention policy.</p> <p>Log events do not expire if they belong to log groups without a retention policy.</p>
deleteRetentionPolicy :: forall eff. DeleteRetentionPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteRetentionPolicy = AWS.request serviceName "DeleteRetentionPolicy" 


-- | <p>Deletes the specified subscription filter.</p>
deleteSubscriptionFilter :: forall eff. DeleteSubscriptionFilterRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteSubscriptionFilter = AWS.request serviceName "DeleteSubscriptionFilter" 


-- | <p>Lists all your destinations. The results are ASCII-sorted by destination name.</p>
describeDestinations :: forall eff. DescribeDestinationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDestinationsResponse
describeDestinations = AWS.request serviceName "DescribeDestinations" 


-- | <p>Lists the specified export tasks. You can list all your export tasks or filter the results based on task ID or task status.</p>
describeExportTasks :: forall eff. DescribeExportTasksRequest -> Aff (err :: AWS.RequestError | eff) DescribeExportTasksResponse
describeExportTasks = AWS.request serviceName "DescribeExportTasks" 


-- | <p>Lists the specified log groups. You can list all your log groups or filter the results by prefix. The results are ASCII-sorted by log group name.</p>
describeLogGroups :: forall eff. DescribeLogGroupsRequest -> Aff (err :: AWS.RequestError | eff) DescribeLogGroupsResponse
describeLogGroups = AWS.request serviceName "DescribeLogGroups" 


-- | <p>Lists the log streams for the specified log group. You can list all the log streams or filter the results by prefix. You can also control how the results are ordered.</p> <p>This operation has a limit of five transactions per second, after which transactions are throttled.</p>
describeLogStreams :: forall eff. DescribeLogStreamsRequest -> Aff (err :: AWS.RequestError | eff) DescribeLogStreamsResponse
describeLogStreams = AWS.request serviceName "DescribeLogStreams" 


-- | <p>Lists the specified metric filters. You can list all the metric filters or filter the results by log name, prefix, metric name, or metric namespace. The results are ASCII-sorted by filter name.</p>
describeMetricFilters :: forall eff. DescribeMetricFiltersRequest -> Aff (err :: AWS.RequestError | eff) DescribeMetricFiltersResponse
describeMetricFilters = AWS.request serviceName "DescribeMetricFilters" 


-- | <p>Lists the resource policies in this account.</p>
describeResourcePolicies :: forall eff. DescribeResourcePoliciesRequest -> Aff (err :: AWS.RequestError | eff) DescribeResourcePoliciesResponse
describeResourcePolicies = AWS.request serviceName "DescribeResourcePolicies" 


-- | <p>Lists the subscription filters for the specified log group. You can list all the subscription filters or filter the results by prefix. The results are ASCII-sorted by filter name.</p>
describeSubscriptionFilters :: forall eff. DescribeSubscriptionFiltersRequest -> Aff (err :: AWS.RequestError | eff) DescribeSubscriptionFiltersResponse
describeSubscriptionFilters = AWS.request serviceName "DescribeSubscriptionFilters" 


-- | <p>Disassociates the associated AWS Key Management Service (AWS KMS) customer master key (CMK) from the specified log group.</p> <p>After the AWS KMS CMK is disassociated from the log group, AWS CloudWatch Logs stops encrypting newly ingested data for the log group. All previously ingested data remains encrypted, and AWS CloudWatch Logs requires permissions for the CMK whenever the encrypted data is requested.</p> <p>Note that it can take up to 5 minutes for this operation to take effect.</p>
disassociateKmsKey :: forall eff. DisassociateKmsKeyRequest -> Aff (err :: AWS.RequestError | eff) Unit
disassociateKmsKey = AWS.request serviceName "DisassociateKmsKey" 


-- | <p>Lists log events from the specified log group. You can list all the log events or filter the results using a filter pattern, a time range, and the name of the log stream.</p> <p>By default, this operation returns as many log events as can fit in 1 MB (up to 10,000 log events), or all the events found within the time range that you specify. If the results include a token, then there are more log events available, and you can get additional results by specifying the token in a subsequent call.</p>
filterLogEvents :: forall eff. FilterLogEventsRequest -> Aff (err :: AWS.RequestError | eff) FilterLogEventsResponse
filterLogEvents = AWS.request serviceName "FilterLogEvents" 


-- | <p>Lists log events from the specified log stream. You can list all the log events or filter using a time range.</p> <p>By default, this operation returns as many log events as can fit in a response size of 1MB (up to 10,000 log events). You can get additional log events by specifying one of the tokens in a subsequent call.</p>
getLogEvents :: forall eff. GetLogEventsRequest -> Aff (err :: AWS.RequestError | eff) GetLogEventsResponse
getLogEvents = AWS.request serviceName "GetLogEvents" 


-- | <p>Lists the tags for the specified log group.</p>
listTagsLogGroup :: forall eff. ListTagsLogGroupRequest -> Aff (err :: AWS.RequestError | eff) ListTagsLogGroupResponse
listTagsLogGroup = AWS.request serviceName "ListTagsLogGroup" 


-- | <p>Creates or updates a destination. A destination encapsulates a physical resource (such as an Amazon Kinesis stream) and enables you to subscribe to a real-time stream of log events for a different account, ingested using <a>PutLogEvents</a>. Currently, the only supported physical resource is a Kinesis stream belonging to the same account as the destination.</p> <p>Through an access policy, a destination controls what is written to its Kinesis stream. By default, <code>PutDestination</code> does not set any access policy with the destination, which means a cross-account user cannot call <a>PutSubscriptionFilter</a> against this destination. To enable this, the destination owner must call <a>PutDestinationPolicy</a> after <code>PutDestination</code>.</p>
putDestination :: forall eff. PutDestinationRequest -> Aff (err :: AWS.RequestError | eff) PutDestinationResponse
putDestination = AWS.request serviceName "PutDestination" 


-- | <p>Creates or updates an access policy associated with an existing destination. An access policy is an <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies_overview.html">IAM policy document</a> that is used to authorize claims to register a subscription filter against a given destination.</p>
putDestinationPolicy :: forall eff. PutDestinationPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
putDestinationPolicy = AWS.request serviceName "PutDestinationPolicy" 


-- | <p>Uploads a batch of log events to the specified log stream.</p> <p>You must include the sequence token obtained from the response of the previous call. An upload in a newly created log stream does not require a sequence token. You can also get the sequence token using <a>DescribeLogStreams</a>. If you call <code>PutLogEvents</code> twice within a narrow time period using the same value for <code>sequenceToken</code>, both calls may be successful, or one may be rejected.</p> <p>The batch of events must satisfy the following constraints:</p> <ul> <li> <p>The maximum batch size is 1,048,576 bytes, and this size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.</p> </li> <li> <p>None of the log events in the batch can be more than 2 hours in the future.</p> </li> <li> <p>None of the log events in the batch can be older than 14 days or the retention period of the log group.</p> </li> <li> <p>The log events in the batch must be in chronological ordered by their time stamp (the time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC).</p> </li> <li> <p>The maximum number of log events in a batch is 10,000.</p> </li> <li> <p>A batch of log events in a single request cannot span more than 24 hours. Otherwise, the operation fails.</p> </li> </ul>
putLogEvents :: forall eff. PutLogEventsRequest -> Aff (err :: AWS.RequestError | eff) PutLogEventsResponse
putLogEvents = AWS.request serviceName "PutLogEvents" 


-- | <p>Creates or updates a metric filter and associates it with the specified log group. Metric filters allow you to configure rules to extract metric data from log events ingested through <a>PutLogEvents</a>.</p> <p>The maximum number of metric filters that can be associated with a log group is 100.</p>
putMetricFilter :: forall eff. PutMetricFilterRequest -> Aff (err :: AWS.RequestError | eff) Unit
putMetricFilter = AWS.request serviceName "PutMetricFilter" 


-- | <p>Creates or updates a resource policy allowing other AWS services to put log events to this account, such as Amazon Route 53. An account can have up to 50 resource policies per region.</p>
putResourcePolicy :: forall eff. PutResourcePolicyRequest -> Aff (err :: AWS.RequestError | eff) PutResourcePolicyResponse
putResourcePolicy = AWS.request serviceName "PutResourcePolicy" 


-- | <p>Sets the retention of the specified log group. A retention policy allows you to configure the number of days for which to retain log events in the specified log group.</p>
putRetentionPolicy :: forall eff. PutRetentionPolicyRequest -> Aff (err :: AWS.RequestError | eff) Unit
putRetentionPolicy = AWS.request serviceName "PutRetentionPolicy" 


-- | <p>Creates or updates a subscription filter and associates it with the specified log group. Subscription filters allow you to subscribe to a real-time stream of log events ingested through <a>PutLogEvents</a> and have them delivered to a specific destination. Currently, the supported destinations are:</p> <ul> <li> <p>An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.</p> </li> <li> <p>A logical destination that belongs to a different account, for cross-account delivery.</p> </li> <li> <p>An Amazon Kinesis Firehose delivery stream that belongs to the same account as the subscription filter, for same-account delivery.</p> </li> <li> <p>An AWS Lambda function that belongs to the same account as the subscription filter, for same-account delivery.</p> </li> </ul> <p>There can only be one subscription filter associated with a log group. If you are updating an existing filter, you must specify the correct name in <code>filterName</code>. Otherwise, the call fails because you cannot associate a second filter with a log group.</p>
putSubscriptionFilter :: forall eff. PutSubscriptionFilterRequest -> Aff (err :: AWS.RequestError | eff) Unit
putSubscriptionFilter = AWS.request serviceName "PutSubscriptionFilter" 


-- | <p>Adds or updates the specified tags for the specified log group.</p> <p>To list the tags for a log group, use <a>ListTagsLogGroup</a>. To remove tags, use <a>UntagLogGroup</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/log-group-tagging.html">Tag Log Groups in Amazon CloudWatch Logs</a> in the <i>Amazon CloudWatch Logs User Guide</i>.</p>
tagLogGroup :: forall eff. TagLogGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
tagLogGroup = AWS.request serviceName "TagLogGroup" 


-- | <p>Tests the filter pattern of a metric filter against a sample of log event messages. You can use this operation to validate the correctness of a metric filter pattern.</p>
testMetricFilter :: forall eff. TestMetricFilterRequest -> Aff (err :: AWS.RequestError | eff) TestMetricFilterResponse
testMetricFilter = AWS.request serviceName "TestMetricFilter" 


-- | <p>Removes the specified tags from the specified log group.</p> <p>To list the tags for a log group, use <a>ListTagsLogGroup</a>. To add tags, use <a>UntagLogGroup</a>.</p>
untagLogGroup :: forall eff. UntagLogGroupRequest -> Aff (err :: AWS.RequestError | eff) Unit
untagLogGroup = AWS.request serviceName "UntagLogGroup" 


newtype AccessPolicy = AccessPolicy String


newtype Arn = Arn String


newtype AssociateKmsKeyRequest = AssociateKmsKeyRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "KmsKeyId'" :: (KmsKeyId)
  }


newtype CancelExportTaskRequest = CancelExportTaskRequest 
  { "TaskId'" :: (ExportTaskId)
  }


newtype CreateExportTaskRequest = CreateExportTaskRequest 
  { "TaskName'" :: NullOrUndefined (ExportTaskName)
  , "LogGroupName'" :: (LogGroupName)
  , "LogStreamNamePrefix'" :: NullOrUndefined (LogStreamName)
  , "From'" :: (Number)
  , "To'" :: (Number)
  , "Destination'" :: (ExportDestinationBucket)
  , "DestinationPrefix'" :: NullOrUndefined (ExportDestinationPrefix)
  }


newtype CreateExportTaskResponse = CreateExportTaskResponse 
  { "TaskId'" :: NullOrUndefined (ExportTaskId)
  }


newtype CreateLogGroupRequest = CreateLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "KmsKeyId'" :: NullOrUndefined (KmsKeyId)
  , "Tags'" :: NullOrUndefined (Tags)
  }


newtype CreateLogStreamRequest = CreateLogStreamRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamName'" :: (LogStreamName)
  }


-- | <p>The event was already logged.</p>
newtype DataAlreadyAcceptedException = DataAlreadyAcceptedException 
  { "ExpectedSequenceToken'" :: NullOrUndefined (SequenceToken)
  }


-- | <p>The number of days to retain the log events in the specified log group. Possible values are: 1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180, 365, 400, 545, 731, 1827, and 3653.</p>
newtype Days = Days Int


newtype DefaultValue = DefaultValue Number


newtype DeleteDestinationRequest = DeleteDestinationRequest 
  { "DestinationName'" :: (DestinationName)
  }


newtype DeleteLogGroupRequest = DeleteLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  }


newtype DeleteLogStreamRequest = DeleteLogStreamRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamName'" :: (LogStreamName)
  }


newtype DeleteMetricFilterRequest = DeleteMetricFilterRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterName'" :: (FilterName)
  }


newtype DeleteResourcePolicyRequest = DeleteResourcePolicyRequest 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  }


newtype DeleteRetentionPolicyRequest = DeleteRetentionPolicyRequest 
  { "LogGroupName'" :: (LogGroupName)
  }


newtype DeleteSubscriptionFilterRequest = DeleteSubscriptionFilterRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterName'" :: (FilterName)
  }


newtype Descending = Descending Boolean


newtype DescribeDestinationsRequest = DescribeDestinationsRequest 
  { "DestinationNamePrefix" :: NullOrUndefined (DestinationName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (DescribeLimit)
  }


newtype DescribeDestinationsResponse = DescribeDestinationsResponse 
  { "Destinations'" :: NullOrUndefined (Destinations)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeExportTasksRequest = DescribeExportTasksRequest 
  { "TaskId'" :: NullOrUndefined (ExportTaskId)
  , "StatusCode'" :: NullOrUndefined (ExportTaskStatusCode)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (DescribeLimit)
  }


newtype DescribeExportTasksResponse = DescribeExportTasksResponse 
  { "ExportTasks'" :: NullOrUndefined (ExportTasks)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeLimit = DescribeLimit Int


newtype DescribeLogGroupsRequest = DescribeLogGroupsRequest 
  { "LogGroupNamePrefix'" :: NullOrUndefined (LogGroupName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (DescribeLimit)
  }


newtype DescribeLogGroupsResponse = DescribeLogGroupsResponse 
  { "LogGroups'" :: NullOrUndefined (LogGroups)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeLogStreamsRequest = DescribeLogStreamsRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamNamePrefix'" :: NullOrUndefined (LogStreamName)
  , "OrderBy'" :: NullOrUndefined (OrderBy)
  , "Descending'" :: NullOrUndefined (Descending)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (DescribeLimit)
  }


newtype DescribeLogStreamsResponse = DescribeLogStreamsResponse 
  { "LogStreams'" :: NullOrUndefined (LogStreams)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeMetricFiltersRequest = DescribeMetricFiltersRequest 
  { "LogGroupName'" :: NullOrUndefined (LogGroupName)
  , "FilterNamePrefix'" :: NullOrUndefined (FilterName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (DescribeLimit)
  , "MetricName'" :: NullOrUndefined (MetricName)
  , "MetricNamespace'" :: NullOrUndefined (MetricNamespace)
  }


newtype DescribeMetricFiltersResponse = DescribeMetricFiltersResponse 
  { "MetricFilters'" :: NullOrUndefined (MetricFilters)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeResourcePoliciesRequest = DescribeResourcePoliciesRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (DescribeLimit)
  }


newtype DescribeResourcePoliciesResponse = DescribeResourcePoliciesResponse 
  { "ResourcePolicies'" :: NullOrUndefined (ResourcePolicies)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeSubscriptionFiltersRequest = DescribeSubscriptionFiltersRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterNamePrefix'" :: NullOrUndefined (FilterName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (DescribeLimit)
  }


newtype DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse 
  { "SubscriptionFilters'" :: NullOrUndefined (SubscriptionFilters)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents a cross-account destination that receives subscription log events.</p>
newtype Destination = Destination 
  { "DestinationName'" :: NullOrUndefined (DestinationName)
  , "TargetArn'" :: NullOrUndefined (TargetArn)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  , "AccessPolicy'" :: NullOrUndefined (AccessPolicy)
  , "Arn'" :: NullOrUndefined (Arn)
  , "CreationTime'" :: NullOrUndefined (Number)
  }


newtype DestinationArn = DestinationArn String


newtype DestinationName = DestinationName String


newtype Destinations = Destinations (Array Destination)


newtype DisassociateKmsKeyRequest = DisassociateKmsKeyRequest 
  { "LogGroupName'" :: (LogGroupName)
  }


-- | <p>The method used to distribute log data to the destination, which can be either random or grouped by log stream.</p>
newtype Distribution = Distribution String


newtype EventId = EventId String


newtype EventMessage = EventMessage String


newtype EventNumber = EventNumber Number


newtype EventsLimit = EventsLimit Int


newtype ExportDestinationBucket = ExportDestinationBucket String


newtype ExportDestinationPrefix = ExportDestinationPrefix String


-- | <p>Represents an export task.</p>
newtype ExportTask = ExportTask 
  { "TaskId'" :: NullOrUndefined (ExportTaskId)
  , "TaskName'" :: NullOrUndefined (ExportTaskName)
  , "LogGroupName'" :: NullOrUndefined (LogGroupName)
  , "From'" :: NullOrUndefined (Number)
  , "To'" :: NullOrUndefined (Number)
  , "Destination'" :: NullOrUndefined (ExportDestinationBucket)
  , "DestinationPrefix'" :: NullOrUndefined (ExportDestinationPrefix)
  , "Status'" :: NullOrUndefined (ExportTaskStatus)
  , "ExecutionInfo'" :: NullOrUndefined (ExportTaskExecutionInfo)
  }


-- | <p>Represents the status of an export task.</p>
newtype ExportTaskExecutionInfo = ExportTaskExecutionInfo 
  { "CreationTime'" :: NullOrUndefined (Number)
  , "CompletionTime'" :: NullOrUndefined (Number)
  }


newtype ExportTaskId = ExportTaskId String


newtype ExportTaskName = ExportTaskName String


-- | <p>Represents the status of an export task.</p>
newtype ExportTaskStatus = ExportTaskStatus 
  { "Code'" :: NullOrUndefined (ExportTaskStatusCode)
  , "Message'" :: NullOrUndefined (ExportTaskStatusMessage)
  }


newtype ExportTaskStatusCode = ExportTaskStatusCode String


newtype ExportTaskStatusMessage = ExportTaskStatusMessage String


newtype ExportTasks = ExportTasks (Array ExportTask)


newtype ExtractedValues = ExtractedValues (Map Token Value)


newtype FilterCount = FilterCount Int


newtype FilterLogEventsRequest = FilterLogEventsRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamNames'" :: NullOrUndefined (InputLogStreamNames)
  , "StartTime'" :: NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined (Number)
  , "FilterPattern'" :: NullOrUndefined (FilterPattern)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (EventsLimit)
  , "Interleaved'" :: NullOrUndefined (Interleaved)
  }


newtype FilterLogEventsResponse = FilterLogEventsResponse 
  { "Events'" :: NullOrUndefined (FilteredLogEvents)
  , "SearchedLogStreams'" :: NullOrUndefined (SearchedLogStreams)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype FilterName = FilterName String


-- | <p>A symbolic description of how CloudWatch Logs should interpret the data in each log event. For example, a log event may contain time stamps, IP addresses, strings, and so on. You use the filter pattern to specify what to look for in the log event message.</p>
newtype FilterPattern = FilterPattern String


-- | <p>Represents a matched event.</p>
newtype FilteredLogEvent = FilteredLogEvent 
  { "LogStreamName'" :: NullOrUndefined (LogStreamName)
  , "Number" :: NullOrUndefined (Number)
  , "Message'" :: NullOrUndefined (EventMessage)
  , "IngestionTime'" :: NullOrUndefined (Number)
  , "EventId'" :: NullOrUndefined (EventId)
  }


newtype FilteredLogEvents = FilteredLogEvents (Array FilteredLogEvent)


newtype GetLogEventsRequest = GetLogEventsRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamName'" :: (LogStreamName)
  , "StartTime'" :: NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined (Number)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined (EventsLimit)
  , "StartFromHead'" :: NullOrUndefined (StartFromHead)
  }


newtype GetLogEventsResponse = GetLogEventsResponse 
  { "Events'" :: NullOrUndefined (OutputLogEvents)
  , "NextForwardToken'" :: NullOrUndefined (NextToken)
  , "NextBackwardToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents a log event, which is a record of activity that was recorded by the application or resource being monitored.</p>
newtype InputLogEvent = InputLogEvent 
  { "Number" :: (Number)
  , "Message'" :: (EventMessage)
  }


newtype InputLogEvents = InputLogEvents (Array InputLogEvent)


newtype InputLogStreamNames = InputLogStreamNames (Array LogStreamName)


newtype Interleaved = Interleaved Boolean


-- | <p>The operation is not valid on the specified resource.</p>
newtype InvalidOperationException = InvalidOperationException 
  { 
  }


-- | <p>A parameter is specified incorrectly.</p>
newtype InvalidParameterException = InvalidParameterException 
  { 
  }


-- | <p>The sequence token is not valid.</p>
newtype InvalidSequenceTokenException = InvalidSequenceTokenException 
  { "ExpectedSequenceToken'" :: NullOrUndefined (SequenceToken)
  }


newtype KmsKeyId = KmsKeyId String


-- | <p>You have reached the maximum number of resources that can be created.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }


newtype ListTagsLogGroupRequest = ListTagsLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  }


newtype ListTagsLogGroupResponse = ListTagsLogGroupResponse 
  { "Tags'" :: NullOrUndefined (Tags)
  }


newtype LogEventIndex = LogEventIndex Int


-- | <p>Represents a log group.</p>
newtype LogGroup = LogGroup 
  { "LogGroupName'" :: NullOrUndefined (LogGroupName)
  , "CreationTime'" :: NullOrUndefined (Number)
  , "RetentionInDays'" :: NullOrUndefined (Days)
  , "MetricFilterCount'" :: NullOrUndefined (FilterCount)
  , "Arn'" :: NullOrUndefined (Arn)
  , "StoredBytes'" :: NullOrUndefined (StoredBytes)
  , "KmsKeyId'" :: NullOrUndefined (KmsKeyId)
  }


newtype LogGroupName = LogGroupName String


newtype LogGroups = LogGroups (Array LogGroup)


-- | <p>Represents a log stream, which is a sequence of log events from a single emitter of logs.</p>
newtype LogStream = LogStream 
  { "LogStreamName'" :: NullOrUndefined (LogStreamName)
  , "CreationTime'" :: NullOrUndefined (Number)
  , "FirstEventTimestamp'" :: NullOrUndefined (Number)
  , "LastEventTimestamp'" :: NullOrUndefined (Number)
  , "LastIngestionTime'" :: NullOrUndefined (Number)
  , "UploadSequenceToken'" :: NullOrUndefined (SequenceToken)
  , "Arn'" :: NullOrUndefined (Arn)
  , "StoredBytes'" :: NullOrUndefined (StoredBytes)
  }


newtype LogStreamName = LogStreamName String


newtype LogStreamSearchedCompletely = LogStreamSearchedCompletely Boolean


newtype LogStreams = LogStreams (Array LogStream)


-- | <p>Metric filters express how CloudWatch Logs would extract metric observations from ingested log events and transform them into metric data in a CloudWatch metric.</p>
newtype MetricFilter = MetricFilter 
  { "FilterName'" :: NullOrUndefined (FilterName)
  , "FilterPattern'" :: NullOrUndefined (FilterPattern)
  , "MetricTransformations'" :: NullOrUndefined (MetricTransformations)
  , "CreationTime'" :: NullOrUndefined (Number)
  , "LogGroupName'" :: NullOrUndefined (LogGroupName)
  }


-- | <p>Represents a matched event.</p>
newtype MetricFilterMatchRecord = MetricFilterMatchRecord 
  { "EventNumber'" :: NullOrUndefined (EventNumber)
  , "EventMessage'" :: NullOrUndefined (EventMessage)
  , "ExtractedValues'" :: NullOrUndefined (ExtractedValues)
  }


newtype MetricFilterMatches = MetricFilterMatches (Array MetricFilterMatchRecord)


newtype MetricFilters = MetricFilters (Array MetricFilter)


-- | <p>The name of the CloudWatch metric to which the monitored log information should be published. For example, you may publish to a metric called ErrorCount.</p>
newtype MetricName = MetricName String


newtype MetricNamespace = MetricNamespace String


-- | <p>Indicates how to transform ingested log events in to metric data in a CloudWatch metric.</p>
newtype MetricTransformation = MetricTransformation 
  { "MetricName'" :: (MetricName)
  , "MetricNamespace'" :: (MetricNamespace)
  , "MetricValue'" :: (MetricValue)
  , "DefaultValue'" :: NullOrUndefined (DefaultValue)
  }


newtype MetricTransformations = MetricTransformations (Array MetricTransformation)


-- | <p>The value to publish to the CloudWatch metric. For example, if you're counting the occurrences of a term like "Error", the value is "1" for each occurrence. If you're counting the bytes transferred, the value is the value in the log event.</p>
newtype MetricValue = MetricValue String


-- | <p>The token for the next set of items to return. The token expires after 24 hours.</p>
newtype NextToken = NextToken String


-- | <p>Multiple requests to update the same resource were in conflict.</p>
newtype OperationAbortedException = OperationAbortedException 
  { 
  }


newtype OrderBy = OrderBy String


-- | <p>Represents a log event.</p>
newtype OutputLogEvent = OutputLogEvent 
  { "Number" :: NullOrUndefined (Number)
  , "Message'" :: NullOrUndefined (EventMessage)
  , "IngestionTime'" :: NullOrUndefined (Number)
  }


newtype OutputLogEvents = OutputLogEvents (Array OutputLogEvent)


newtype PolicyDocument = PolicyDocument String


newtype PolicyName = PolicyName String


newtype PutDestinationPolicyRequest = PutDestinationPolicyRequest 
  { "DestinationName'" :: (DestinationName)
  , "AccessPolicy'" :: (AccessPolicy)
  }


newtype PutDestinationRequest = PutDestinationRequest 
  { "DestinationName'" :: (DestinationName)
  , "TargetArn'" :: (TargetArn)
  , "RoleArn'" :: (RoleArn)
  }


newtype PutDestinationResponse = PutDestinationResponse 
  { "Destination'" :: NullOrUndefined (Destination)
  }


newtype PutLogEventsRequest = PutLogEventsRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamName'" :: (LogStreamName)
  , "LogEvents'" :: (InputLogEvents)
  , "SequenceToken'" :: NullOrUndefined (SequenceToken)
  }


newtype PutLogEventsResponse = PutLogEventsResponse 
  { "NextSequenceToken'" :: NullOrUndefined (SequenceToken)
  , "RejectedLogEventsInfo'" :: NullOrUndefined (RejectedLogEventsInfo)
  }


newtype PutMetricFilterRequest = PutMetricFilterRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterName'" :: (FilterName)
  , "FilterPattern'" :: (FilterPattern)
  , "MetricTransformations'" :: (MetricTransformations)
  }


newtype PutResourcePolicyRequest = PutResourcePolicyRequest 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  }


newtype PutResourcePolicyResponse = PutResourcePolicyResponse 
  { "ResourcePolicy'" :: NullOrUndefined (ResourcePolicy)
  }


newtype PutRetentionPolicyRequest = PutRetentionPolicyRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "RetentionInDays'" :: (Days)
  }


newtype PutSubscriptionFilterRequest = PutSubscriptionFilterRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterName'" :: (FilterName)
  , "FilterPattern'" :: (FilterPattern)
  , "DestinationArn'" :: (DestinationArn)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  , "Distribution'" :: NullOrUndefined (Distribution)
  }


-- | <p>Represents the rejected events.</p>
newtype RejectedLogEventsInfo = RejectedLogEventsInfo 
  { "TooNewLogEventStartIndex'" :: NullOrUndefined (LogEventIndex)
  , "TooOldLogEventEndIndex'" :: NullOrUndefined (LogEventIndex)
  , "ExpiredLogEventEndIndex'" :: NullOrUndefined (LogEventIndex)
  }


-- | <p>The specified resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { 
  }


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }


newtype ResourcePolicies = ResourcePolicies (Array ResourcePolicy)


-- | <p>A policy enabling one or more entities to put logs to a log group in this account.</p>
newtype ResourcePolicy = ResourcePolicy 
  { "PolicyName'" :: NullOrUndefined (PolicyName)
  , "PolicyDocument'" :: NullOrUndefined (PolicyDocument)
  , "LastUpdatedTime'" :: NullOrUndefined (Number)
  }


newtype RoleArn = RoleArn String


-- | <p>Represents the search status of a log stream.</p>
newtype SearchedLogStream = SearchedLogStream 
  { "LogStreamName'" :: NullOrUndefined (LogStreamName)
  , "SearchedCompletely'" :: NullOrUndefined (LogStreamSearchedCompletely)
  }


newtype SearchedLogStreams = SearchedLogStreams (Array SearchedLogStream)


newtype SequenceToken = SequenceToken String


-- | <p>The service cannot complete the request.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { 
  }


newtype StartFromHead = StartFromHead Boolean


newtype StoredBytes = StoredBytes Number


-- | <p>Represents a subscription filter.</p>
newtype SubscriptionFilter = SubscriptionFilter 
  { "FilterName'" :: NullOrUndefined (FilterName)
  , "LogGroupName'" :: NullOrUndefined (LogGroupName)
  , "FilterPattern'" :: NullOrUndefined (FilterPattern)
  , "DestinationArn'" :: NullOrUndefined (DestinationArn)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  , "Distribution'" :: NullOrUndefined (Distribution)
  , "CreationTime'" :: NullOrUndefined (Number)
  }


newtype SubscriptionFilters = SubscriptionFilters (Array SubscriptionFilter)


newtype TagKey = TagKey String


newtype TagList = TagList (Array TagKey)


newtype TagLogGroupRequest = TagLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "Tags'" :: (Tags)
  }


newtype TagValue = TagValue String


newtype Tags = Tags (Map TagKey TagValue)


newtype TargetArn = TargetArn String


newtype TestEventMessages = TestEventMessages (Array EventMessage)


newtype TestMetricFilterRequest = TestMetricFilterRequest 
  { "FilterPattern'" :: (FilterPattern)
  , "LogEventMessages'" :: (TestEventMessages)
  }


newtype TestMetricFilterResponse = TestMetricFilterResponse 
  { "Matches'" :: NullOrUndefined (MetricFilterMatches)
  }


newtype Token = Token String


newtype UntagLogGroupRequest = UntagLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "Tags'" :: (TagList)
  }


newtype Value = Value String
