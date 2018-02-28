

-- | <p>You can use Amazon CloudWatch Logs to monitor, store, and access your log files from Amazon EC2 instances, AWS CloudTrail, or other sources. You can then retrieve the associated log data from CloudWatch Logs using the CloudWatch console, CloudWatch Logs commands in the AWS CLI, CloudWatch Logs API, or CloudWatch Logs SDK.</p> <p>You can use CloudWatch Logs to:</p> <ul> <li> <p> <b>Monitor logs from EC2 instances in real-time</b>: You can use CloudWatch Logs to monitor applications and systems using log data. For example, CloudWatch Logs can track the number of errors that occur in your application logs and send you a notification whenever the rate of errors exceeds a threshold that you specify. CloudWatch Logs uses your log data for monitoring; so, no code changes are required. For example, you can monitor application logs for specific literal terms (such as "NullReferenceException") or count the number of occurrences of a literal term at a particular position in log data (such as "404" status codes in an Apache access log). When the term you are searching for is found, CloudWatch Logs reports the data to a CloudWatch metric that you specify.</p> </li> <li> <p> <b>Monitor AWS CloudTrail logged events</b>: You can create alarms in CloudWatch and receive notifications of particular API activity as captured by CloudTrail and use the notification to perform troubleshooting.</p> </li> <li> <p> <b>Archive log data</b>: You can use CloudWatch Logs to store your log data in highly durable storage. You can change the log retention setting so that any log events older than this setting are automatically deleted. The CloudWatch Logs agent makes it easy to quickly send both rotated and non-rotated log data off of a host and into the log service. You can then access the raw log data when you need it.</p> </li> </ul>
module AWS.CloudWatchLogs where

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

serviceName = "CloudWatchLogs" :: String


-- | <p>Associates the specified AWS Key Management Service (AWS KMS) customer master key (CMK) with the specified log group.</p> <p>Associating an AWS KMS CMK with a log group overrides any existing associations between the log group and a CMK. After a CMK is associated with a log group, all newly ingested data for the log group is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.</p> <p>Note that it can take up to 5 minutes for this operation to take effect.</p> <p>If you attempt to associate a CMK with a log group but the CMK does not exist or the CMK is disabled, you will receive an <code>InvalidParameterException</code> error. </p>
associateKmsKey :: forall eff. AssociateKmsKeyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
associateKmsKey = Request.request serviceName "associateKmsKey" 


-- | <p>Cancels the specified export task.</p> <p>The task must be in the <code>PENDING</code> or <code>RUNNING</code> state.</p>
cancelExportTask :: forall eff. CancelExportTaskRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
cancelExportTask = Request.request serviceName "cancelExportTask" 


-- | <p>Creates an export task, which allows you to efficiently export data from a log group to an Amazon S3 bucket.</p> <p>This is an asynchronous call. If all the required information is provided, this operation initiates an export task and responds with the ID of the task. After the task has started, you can use <a>DescribeExportTasks</a> to get the status of the export task. Each account can only have one active (<code>RUNNING</code> or <code>PENDING</code>) export task at a time. To cancel an export task, use <a>CancelExportTask</a>.</p> <p>You can export logs from multiple log groups or multiple time ranges to the same S3 bucket. To separate out log data for each export task, you can specify a prefix to be used as the Amazon S3 key prefix for all exported objects.</p>
createExportTask :: forall eff. CreateExportTaskRequest -> Aff (exception :: EXCEPTION | eff) CreateExportTaskResponse
createExportTask = Request.request serviceName "createExportTask" 


-- | <p>Creates a log group with the specified name.</p> <p>You can create up to 5000 log groups per account.</p> <p>You must use the following guidelines when naming a log group:</p> <ul> <li> <p>Log group names must be unique within a region for an AWS account.</p> </li> <li> <p>Log group names can be between 1 and 512 characters long.</p> </li> <li> <p>Log group names consist of the following characters: a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).</p> </li> </ul> <p>If you associate a AWS Key Management Service (AWS KMS) customer master key (CMK) with the log group, ingested data is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.</p> <p>If you attempt to associate a CMK with the log group but the CMK does not exist or the CMK is disabled, you will receive an <code>InvalidParameterException</code> error. </p>
createLogGroup :: forall eff. CreateLogGroupRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
createLogGroup = Request.request serviceName "createLogGroup" 


-- | <p>Creates a log stream for the specified log group.</p> <p>There is no limit on the number of log streams that you can create for a log group.</p> <p>You must use the following guidelines when naming a log stream:</p> <ul> <li> <p>Log stream names must be unique within the log group.</p> </li> <li> <p>Log stream names can be between 1 and 512 characters long.</p> </li> <li> <p>The ':' (colon) and '*' (asterisk) characters are not allowed.</p> </li> </ul>
createLogStream :: forall eff. CreateLogStreamRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
createLogStream = Request.request serviceName "createLogStream" 


-- | <p>Deletes the specified destination, and eventually disables all the subscription filters that publish to it. This operation does not delete the physical resource encapsulated by the destination.</p>
deleteDestination :: forall eff. DeleteDestinationRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDestination = Request.request serviceName "deleteDestination" 


-- | <p>Deletes the specified log group and permanently deletes all the archived log events associated with the log group.</p>
deleteLogGroup :: forall eff. DeleteLogGroupRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteLogGroup = Request.request serviceName "deleteLogGroup" 


-- | <p>Deletes the specified log stream and permanently deletes all the archived log events associated with the log stream.</p>
deleteLogStream :: forall eff. DeleteLogStreamRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteLogStream = Request.request serviceName "deleteLogStream" 


-- | <p>Deletes the specified metric filter.</p>
deleteMetricFilter :: forall eff. DeleteMetricFilterRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteMetricFilter = Request.request serviceName "deleteMetricFilter" 


-- | <p>Deletes a resource policy from this account. This revokes the access of the identities in that policy to put log events to this account.</p>
deleteResourcePolicy :: forall eff. DeleteResourcePolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteResourcePolicy = Request.request serviceName "deleteResourcePolicy" 


-- | <p>Deletes the specified retention policy.</p> <p>Log events do not expire if they belong to log groups without a retention policy.</p>
deleteRetentionPolicy :: forall eff. DeleteRetentionPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteRetentionPolicy = Request.request serviceName "deleteRetentionPolicy" 


-- | <p>Deletes the specified subscription filter.</p>
deleteSubscriptionFilter :: forall eff. DeleteSubscriptionFilterRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteSubscriptionFilter = Request.request serviceName "deleteSubscriptionFilter" 


-- | <p>Lists all your destinations. The results are ASCII-sorted by destination name.</p>
describeDestinations :: forall eff. DescribeDestinationsRequest -> Aff (exception :: EXCEPTION | eff) DescribeDestinationsResponse
describeDestinations = Request.request serviceName "describeDestinations" 


-- | <p>Lists the specified export tasks. You can list all your export tasks or filter the results based on task ID or task status.</p>
describeExportTasks :: forall eff. DescribeExportTasksRequest -> Aff (exception :: EXCEPTION | eff) DescribeExportTasksResponse
describeExportTasks = Request.request serviceName "describeExportTasks" 


-- | <p>Lists the specified log groups. You can list all your log groups or filter the results by prefix. The results are ASCII-sorted by log group name.</p>
describeLogGroups :: forall eff. DescribeLogGroupsRequest -> Aff (exception :: EXCEPTION | eff) DescribeLogGroupsResponse
describeLogGroups = Request.request serviceName "describeLogGroups" 


-- | <p>Lists the log streams for the specified log group. You can list all the log streams or filter the results by prefix. You can also control how the results are ordered.</p> <p>This operation has a limit of five transactions per second, after which transactions are throttled.</p>
describeLogStreams :: forall eff. DescribeLogStreamsRequest -> Aff (exception :: EXCEPTION | eff) DescribeLogStreamsResponse
describeLogStreams = Request.request serviceName "describeLogStreams" 


-- | <p>Lists the specified metric filters. You can list all the metric filters or filter the results by log name, prefix, metric name, or metric namespace. The results are ASCII-sorted by filter name.</p>
describeMetricFilters :: forall eff. DescribeMetricFiltersRequest -> Aff (exception :: EXCEPTION | eff) DescribeMetricFiltersResponse
describeMetricFilters = Request.request serviceName "describeMetricFilters" 


-- | <p>Lists the resource policies in this account.</p>
describeResourcePolicies :: forall eff. DescribeResourcePoliciesRequest -> Aff (exception :: EXCEPTION | eff) DescribeResourcePoliciesResponse
describeResourcePolicies = Request.request serviceName "describeResourcePolicies" 


-- | <p>Lists the subscription filters for the specified log group. You can list all the subscription filters or filter the results by prefix. The results are ASCII-sorted by filter name.</p>
describeSubscriptionFilters :: forall eff. DescribeSubscriptionFiltersRequest -> Aff (exception :: EXCEPTION | eff) DescribeSubscriptionFiltersResponse
describeSubscriptionFilters = Request.request serviceName "describeSubscriptionFilters" 


-- | <p>Disassociates the associated AWS Key Management Service (AWS KMS) customer master key (CMK) from the specified log group.</p> <p>After the AWS KMS CMK is disassociated from the log group, AWS CloudWatch Logs stops encrypting newly ingested data for the log group. All previously ingested data remains encrypted, and AWS CloudWatch Logs requires permissions for the CMK whenever the encrypted data is requested.</p> <p>Note that it can take up to 5 minutes for this operation to take effect.</p>
disassociateKmsKey :: forall eff. DisassociateKmsKeyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
disassociateKmsKey = Request.request serviceName "disassociateKmsKey" 


-- | <p>Lists log events from the specified log group. You can list all the log events or filter the results using a filter pattern, a time range, and the name of the log stream.</p> <p>By default, this operation returns as many log events as can fit in 1 MB (up to 10,000 log events), or all the events found within the time range that you specify. If the results include a token, then there are more log events available, and you can get additional results by specifying the token in a subsequent call.</p>
filterLogEvents :: forall eff. FilterLogEventsRequest -> Aff (exception :: EXCEPTION | eff) FilterLogEventsResponse
filterLogEvents = Request.request serviceName "filterLogEvents" 


-- | <p>Lists log events from the specified log stream. You can list all the log events or filter using a time range.</p> <p>By default, this operation returns as many log events as can fit in a response size of 1MB (up to 10,000 log events). You can get additional log events by specifying one of the tokens in a subsequent call.</p>
getLogEvents :: forall eff. GetLogEventsRequest -> Aff (exception :: EXCEPTION | eff) GetLogEventsResponse
getLogEvents = Request.request serviceName "getLogEvents" 


-- | <p>Lists the tags for the specified log group.</p>
listTagsLogGroup :: forall eff. ListTagsLogGroupRequest -> Aff (exception :: EXCEPTION | eff) ListTagsLogGroupResponse
listTagsLogGroup = Request.request serviceName "listTagsLogGroup" 


-- | <p>Creates or updates a destination. A destination encapsulates a physical resource (such as an Amazon Kinesis stream) and enables you to subscribe to a real-time stream of log events for a different account, ingested using <a>PutLogEvents</a>. Currently, the only supported physical resource is a Kinesis stream belonging to the same account as the destination.</p> <p>Through an access policy, a destination controls what is written to its Kinesis stream. By default, <code>PutDestination</code> does not set any access policy with the destination, which means a cross-account user cannot call <a>PutSubscriptionFilter</a> against this destination. To enable this, the destination owner must call <a>PutDestinationPolicy</a> after <code>PutDestination</code>.</p>
putDestination :: forall eff. PutDestinationRequest -> Aff (exception :: EXCEPTION | eff) PutDestinationResponse
putDestination = Request.request serviceName "putDestination" 


-- | <p>Creates or updates an access policy associated with an existing destination. An access policy is an <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies_overview.html">IAM policy document</a> that is used to authorize claims to register a subscription filter against a given destination.</p>
putDestinationPolicy :: forall eff. PutDestinationPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putDestinationPolicy = Request.request serviceName "putDestinationPolicy" 


-- | <p>Uploads a batch of log events to the specified log stream.</p> <p>You must include the sequence token obtained from the response of the previous call. An upload in a newly created log stream does not require a sequence token. You can also get the sequence token using <a>DescribeLogStreams</a>. If you call <code>PutLogEvents</code> twice within a narrow time period using the same value for <code>sequenceToken</code>, both calls may be successful, or one may be rejected.</p> <p>The batch of events must satisfy the following constraints:</p> <ul> <li> <p>The maximum batch size is 1,048,576 bytes, and this size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.</p> </li> <li> <p>None of the log events in the batch can be more than 2 hours in the future.</p> </li> <li> <p>None of the log events in the batch can be older than 14 days or the retention period of the log group.</p> </li> <li> <p>The log events in the batch must be in chronological ordered by their time stamp (the time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC).</p> </li> <li> <p>The maximum number of log events in a batch is 10,000.</p> </li> <li> <p>A batch of log events in a single request cannot span more than 24 hours. Otherwise, the operation fails.</p> </li> </ul>
putLogEvents :: forall eff. PutLogEventsRequest -> Aff (exception :: EXCEPTION | eff) PutLogEventsResponse
putLogEvents = Request.request serviceName "putLogEvents" 


-- | <p>Creates or updates a metric filter and associates it with the specified log group. Metric filters allow you to configure rules to extract metric data from log events ingested through <a>PutLogEvents</a>.</p> <p>The maximum number of metric filters that can be associated with a log group is 100.</p>
putMetricFilter :: forall eff. PutMetricFilterRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putMetricFilter = Request.request serviceName "putMetricFilter" 


-- | <p>Creates or updates a resource policy allowing other AWS services to put log events to this account, such as Amazon Route 53. An account can have up to 50 resource policies per region.</p>
putResourcePolicy :: forall eff. PutResourcePolicyRequest -> Aff (exception :: EXCEPTION | eff) PutResourcePolicyResponse
putResourcePolicy = Request.request serviceName "putResourcePolicy" 


-- | <p>Sets the retention of the specified log group. A retention policy allows you to configure the number of days for which to retain log events in the specified log group.</p>
putRetentionPolicy :: forall eff. PutRetentionPolicyRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putRetentionPolicy = Request.request serviceName "putRetentionPolicy" 


-- | <p>Creates or updates a subscription filter and associates it with the specified log group. Subscription filters allow you to subscribe to a real-time stream of log events ingested through <a>PutLogEvents</a> and have them delivered to a specific destination. Currently, the supported destinations are:</p> <ul> <li> <p>An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.</p> </li> <li> <p>A logical destination that belongs to a different account, for cross-account delivery.</p> </li> <li> <p>An Amazon Kinesis Firehose delivery stream that belongs to the same account as the subscription filter, for same-account delivery.</p> </li> <li> <p>An AWS Lambda function that belongs to the same account as the subscription filter, for same-account delivery.</p> </li> </ul> <p>There can only be one subscription filter associated with a log group. If you are updating an existing filter, you must specify the correct name in <code>filterName</code>. Otherwise, the call fails because you cannot associate a second filter with a log group.</p>
putSubscriptionFilter :: forall eff. PutSubscriptionFilterRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putSubscriptionFilter = Request.request serviceName "putSubscriptionFilter" 


-- | <p>Adds or updates the specified tags for the specified log group.</p> <p>To list the tags for a log group, use <a>ListTagsLogGroup</a>. To remove tags, use <a>UntagLogGroup</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/log-group-tagging.html">Tag Log Groups in Amazon CloudWatch Logs</a> in the <i>Amazon CloudWatch Logs User Guide</i>.</p>
tagLogGroup :: forall eff. TagLogGroupRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
tagLogGroup = Request.request serviceName "tagLogGroup" 


-- | <p>Tests the filter pattern of a metric filter against a sample of log event messages. You can use this operation to validate the correctness of a metric filter pattern.</p>
testMetricFilter :: forall eff. TestMetricFilterRequest -> Aff (exception :: EXCEPTION | eff) TestMetricFilterResponse
testMetricFilter = Request.request serviceName "testMetricFilter" 


-- | <p>Removes the specified tags from the specified log group.</p> <p>To list the tags for a log group, use <a>ListTagsLogGroup</a>. To add tags, use <a>UntagLogGroup</a>.</p>
untagLogGroup :: forall eff. UntagLogGroupRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
untagLogGroup = Request.request serviceName "untagLogGroup" 


newtype AccessPolicy = AccessPolicy String
derive instance newtypeAccessPolicy :: Newtype AccessPolicy _
derive instance repGenericAccessPolicy :: Generic AccessPolicy _
instance showAccessPolicy :: Show AccessPolicy where
  show = genericShow
instance decodeAccessPolicy :: Decode AccessPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessPolicy :: Encode AccessPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _
derive instance repGenericArn :: Generic Arn _
instance showArn :: Show Arn where
  show = genericShow
instance decodeArn :: Decode Arn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArn :: Encode Arn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateKmsKeyRequest = AssociateKmsKeyRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "KmsKeyId'" :: (KmsKeyId)
  }
derive instance newtypeAssociateKmsKeyRequest :: Newtype AssociateKmsKeyRequest _
derive instance repGenericAssociateKmsKeyRequest :: Generic AssociateKmsKeyRequest _
instance showAssociateKmsKeyRequest :: Show AssociateKmsKeyRequest where
  show = genericShow
instance decodeAssociateKmsKeyRequest :: Decode AssociateKmsKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateKmsKeyRequest :: Encode AssociateKmsKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelExportTaskRequest = CancelExportTaskRequest 
  { "TaskId'" :: (ExportTaskId)
  }
derive instance newtypeCancelExportTaskRequest :: Newtype CancelExportTaskRequest _
derive instance repGenericCancelExportTaskRequest :: Generic CancelExportTaskRequest _
instance showCancelExportTaskRequest :: Show CancelExportTaskRequest where
  show = genericShow
instance decodeCancelExportTaskRequest :: Decode CancelExportTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelExportTaskRequest :: Encode CancelExportTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateExportTaskRequest = CreateExportTaskRequest 
  { "TaskName'" :: NullOrUndefined.NullOrUndefined (ExportTaskName)
  , "LogGroupName'" :: (LogGroupName)
  , "LogStreamNamePrefix'" :: NullOrUndefined.NullOrUndefined (LogStreamName)
  , "From'" :: (Number)
  , "To'" :: (Number)
  , "Destination'" :: (ExportDestinationBucket)
  , "DestinationPrefix'" :: NullOrUndefined.NullOrUndefined (ExportDestinationPrefix)
  }
derive instance newtypeCreateExportTaskRequest :: Newtype CreateExportTaskRequest _
derive instance repGenericCreateExportTaskRequest :: Generic CreateExportTaskRequest _
instance showCreateExportTaskRequest :: Show CreateExportTaskRequest where
  show = genericShow
instance decodeCreateExportTaskRequest :: Decode CreateExportTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateExportTaskRequest :: Encode CreateExportTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateExportTaskResponse = CreateExportTaskResponse 
  { "TaskId'" :: NullOrUndefined.NullOrUndefined (ExportTaskId)
  }
derive instance newtypeCreateExportTaskResponse :: Newtype CreateExportTaskResponse _
derive instance repGenericCreateExportTaskResponse :: Generic CreateExportTaskResponse _
instance showCreateExportTaskResponse :: Show CreateExportTaskResponse where
  show = genericShow
instance decodeCreateExportTaskResponse :: Decode CreateExportTaskResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateExportTaskResponse :: Encode CreateExportTaskResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLogGroupRequest = CreateLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "KmsKeyId'" :: NullOrUndefined.NullOrUndefined (KmsKeyId)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeCreateLogGroupRequest :: Newtype CreateLogGroupRequest _
derive instance repGenericCreateLogGroupRequest :: Generic CreateLogGroupRequest _
instance showCreateLogGroupRequest :: Show CreateLogGroupRequest where
  show = genericShow
instance decodeCreateLogGroupRequest :: Decode CreateLogGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLogGroupRequest :: Encode CreateLogGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLogStreamRequest = CreateLogStreamRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamName'" :: (LogStreamName)
  }
derive instance newtypeCreateLogStreamRequest :: Newtype CreateLogStreamRequest _
derive instance repGenericCreateLogStreamRequest :: Generic CreateLogStreamRequest _
instance showCreateLogStreamRequest :: Show CreateLogStreamRequest where
  show = genericShow
instance decodeCreateLogStreamRequest :: Decode CreateLogStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLogStreamRequest :: Encode CreateLogStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The event was already logged.</p>
newtype DataAlreadyAcceptedException = DataAlreadyAcceptedException 
  { "ExpectedSequenceToken'" :: NullOrUndefined.NullOrUndefined (SequenceToken)
  }
derive instance newtypeDataAlreadyAcceptedException :: Newtype DataAlreadyAcceptedException _
derive instance repGenericDataAlreadyAcceptedException :: Generic DataAlreadyAcceptedException _
instance showDataAlreadyAcceptedException :: Show DataAlreadyAcceptedException where
  show = genericShow
instance decodeDataAlreadyAcceptedException :: Decode DataAlreadyAcceptedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataAlreadyAcceptedException :: Encode DataAlreadyAcceptedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of days to retain the log events in the specified log group. Possible values are: 1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180, 365, 400, 545, 731, 1827, and 3653.</p>
newtype Days = Days Int
derive instance newtypeDays :: Newtype Days _
derive instance repGenericDays :: Generic Days _
instance showDays :: Show Days where
  show = genericShow
instance decodeDays :: Decode Days where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDays :: Encode Days where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefaultValue = DefaultValue Number
derive instance newtypeDefaultValue :: Newtype DefaultValue _
derive instance repGenericDefaultValue :: Generic DefaultValue _
instance showDefaultValue :: Show DefaultValue where
  show = genericShow
instance decodeDefaultValue :: Decode DefaultValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultValue :: Encode DefaultValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDestinationRequest = DeleteDestinationRequest 
  { "DestinationName'" :: (DestinationName)
  }
derive instance newtypeDeleteDestinationRequest :: Newtype DeleteDestinationRequest _
derive instance repGenericDeleteDestinationRequest :: Generic DeleteDestinationRequest _
instance showDeleteDestinationRequest :: Show DeleteDestinationRequest where
  show = genericShow
instance decodeDeleteDestinationRequest :: Decode DeleteDestinationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDestinationRequest :: Encode DeleteDestinationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLogGroupRequest = DeleteLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  }
derive instance newtypeDeleteLogGroupRequest :: Newtype DeleteLogGroupRequest _
derive instance repGenericDeleteLogGroupRequest :: Generic DeleteLogGroupRequest _
instance showDeleteLogGroupRequest :: Show DeleteLogGroupRequest where
  show = genericShow
instance decodeDeleteLogGroupRequest :: Decode DeleteLogGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLogGroupRequest :: Encode DeleteLogGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLogStreamRequest = DeleteLogStreamRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamName'" :: (LogStreamName)
  }
derive instance newtypeDeleteLogStreamRequest :: Newtype DeleteLogStreamRequest _
derive instance repGenericDeleteLogStreamRequest :: Generic DeleteLogStreamRequest _
instance showDeleteLogStreamRequest :: Show DeleteLogStreamRequest where
  show = genericShow
instance decodeDeleteLogStreamRequest :: Decode DeleteLogStreamRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLogStreamRequest :: Encode DeleteLogStreamRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteMetricFilterRequest = DeleteMetricFilterRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterName'" :: (FilterName)
  }
derive instance newtypeDeleteMetricFilterRequest :: Newtype DeleteMetricFilterRequest _
derive instance repGenericDeleteMetricFilterRequest :: Generic DeleteMetricFilterRequest _
instance showDeleteMetricFilterRequest :: Show DeleteMetricFilterRequest where
  show = genericShow
instance decodeDeleteMetricFilterRequest :: Decode DeleteMetricFilterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteMetricFilterRequest :: Encode DeleteMetricFilterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteResourcePolicyRequest = DeleteResourcePolicyRequest 
  { "PolicyName'" :: NullOrUndefined.NullOrUndefined (PolicyName)
  }
derive instance newtypeDeleteResourcePolicyRequest :: Newtype DeleteResourcePolicyRequest _
derive instance repGenericDeleteResourcePolicyRequest :: Generic DeleteResourcePolicyRequest _
instance showDeleteResourcePolicyRequest :: Show DeleteResourcePolicyRequest where
  show = genericShow
instance decodeDeleteResourcePolicyRequest :: Decode DeleteResourcePolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteResourcePolicyRequest :: Encode DeleteResourcePolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRetentionPolicyRequest = DeleteRetentionPolicyRequest 
  { "LogGroupName'" :: (LogGroupName)
  }
derive instance newtypeDeleteRetentionPolicyRequest :: Newtype DeleteRetentionPolicyRequest _
derive instance repGenericDeleteRetentionPolicyRequest :: Generic DeleteRetentionPolicyRequest _
instance showDeleteRetentionPolicyRequest :: Show DeleteRetentionPolicyRequest where
  show = genericShow
instance decodeDeleteRetentionPolicyRequest :: Decode DeleteRetentionPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRetentionPolicyRequest :: Encode DeleteRetentionPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSubscriptionFilterRequest = DeleteSubscriptionFilterRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterName'" :: (FilterName)
  }
derive instance newtypeDeleteSubscriptionFilterRequest :: Newtype DeleteSubscriptionFilterRequest _
derive instance repGenericDeleteSubscriptionFilterRequest :: Generic DeleteSubscriptionFilterRequest _
instance showDeleteSubscriptionFilterRequest :: Show DeleteSubscriptionFilterRequest where
  show = genericShow
instance decodeDeleteSubscriptionFilterRequest :: Decode DeleteSubscriptionFilterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSubscriptionFilterRequest :: Encode DeleteSubscriptionFilterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Descending = Descending Boolean
derive instance newtypeDescending :: Newtype Descending _
derive instance repGenericDescending :: Generic Descending _
instance showDescending :: Show Descending where
  show = genericShow
instance decodeDescending :: Decode Descending where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescending :: Encode Descending where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDestinationsRequest = DescribeDestinationsRequest 
  { "DestinationNamePrefix" :: NullOrUndefined.NullOrUndefined (DestinationName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (DescribeLimit)
  }
derive instance newtypeDescribeDestinationsRequest :: Newtype DescribeDestinationsRequest _
derive instance repGenericDescribeDestinationsRequest :: Generic DescribeDestinationsRequest _
instance showDescribeDestinationsRequest :: Show DescribeDestinationsRequest where
  show = genericShow
instance decodeDescribeDestinationsRequest :: Decode DescribeDestinationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDestinationsRequest :: Encode DescribeDestinationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDestinationsResponse = DescribeDestinationsResponse 
  { "Destinations'" :: NullOrUndefined.NullOrUndefined (Destinations)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeDestinationsResponse :: Newtype DescribeDestinationsResponse _
derive instance repGenericDescribeDestinationsResponse :: Generic DescribeDestinationsResponse _
instance showDescribeDestinationsResponse :: Show DescribeDestinationsResponse where
  show = genericShow
instance decodeDescribeDestinationsResponse :: Decode DescribeDestinationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDestinationsResponse :: Encode DescribeDestinationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeExportTasksRequest = DescribeExportTasksRequest 
  { "TaskId'" :: NullOrUndefined.NullOrUndefined (ExportTaskId)
  , "StatusCode'" :: NullOrUndefined.NullOrUndefined (ExportTaskStatusCode)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (DescribeLimit)
  }
derive instance newtypeDescribeExportTasksRequest :: Newtype DescribeExportTasksRequest _
derive instance repGenericDescribeExportTasksRequest :: Generic DescribeExportTasksRequest _
instance showDescribeExportTasksRequest :: Show DescribeExportTasksRequest where
  show = genericShow
instance decodeDescribeExportTasksRequest :: Decode DescribeExportTasksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeExportTasksRequest :: Encode DescribeExportTasksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeExportTasksResponse = DescribeExportTasksResponse 
  { "ExportTasks'" :: NullOrUndefined.NullOrUndefined (ExportTasks)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportTasksResponse :: Newtype DescribeExportTasksResponse _
derive instance repGenericDescribeExportTasksResponse :: Generic DescribeExportTasksResponse _
instance showDescribeExportTasksResponse :: Show DescribeExportTasksResponse where
  show = genericShow
instance decodeDescribeExportTasksResponse :: Decode DescribeExportTasksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeExportTasksResponse :: Encode DescribeExportTasksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLimit = DescribeLimit Int
derive instance newtypeDescribeLimit :: Newtype DescribeLimit _
derive instance repGenericDescribeLimit :: Generic DescribeLimit _
instance showDescribeLimit :: Show DescribeLimit where
  show = genericShow
instance decodeDescribeLimit :: Decode DescribeLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLimit :: Encode DescribeLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLogGroupsRequest = DescribeLogGroupsRequest 
  { "LogGroupNamePrefix'" :: NullOrUndefined.NullOrUndefined (LogGroupName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (DescribeLimit)
  }
derive instance newtypeDescribeLogGroupsRequest :: Newtype DescribeLogGroupsRequest _
derive instance repGenericDescribeLogGroupsRequest :: Generic DescribeLogGroupsRequest _
instance showDescribeLogGroupsRequest :: Show DescribeLogGroupsRequest where
  show = genericShow
instance decodeDescribeLogGroupsRequest :: Decode DescribeLogGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLogGroupsRequest :: Encode DescribeLogGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLogGroupsResponse = DescribeLogGroupsResponse 
  { "LogGroups'" :: NullOrUndefined.NullOrUndefined (LogGroups)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeLogGroupsResponse :: Newtype DescribeLogGroupsResponse _
derive instance repGenericDescribeLogGroupsResponse :: Generic DescribeLogGroupsResponse _
instance showDescribeLogGroupsResponse :: Show DescribeLogGroupsResponse where
  show = genericShow
instance decodeDescribeLogGroupsResponse :: Decode DescribeLogGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLogGroupsResponse :: Encode DescribeLogGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLogStreamsRequest = DescribeLogStreamsRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamNamePrefix'" :: NullOrUndefined.NullOrUndefined (LogStreamName)
  , "OrderBy'" :: NullOrUndefined.NullOrUndefined (OrderBy)
  , "Descending'" :: NullOrUndefined.NullOrUndefined (Descending)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (DescribeLimit)
  }
derive instance newtypeDescribeLogStreamsRequest :: Newtype DescribeLogStreamsRequest _
derive instance repGenericDescribeLogStreamsRequest :: Generic DescribeLogStreamsRequest _
instance showDescribeLogStreamsRequest :: Show DescribeLogStreamsRequest where
  show = genericShow
instance decodeDescribeLogStreamsRequest :: Decode DescribeLogStreamsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLogStreamsRequest :: Encode DescribeLogStreamsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLogStreamsResponse = DescribeLogStreamsResponse 
  { "LogStreams'" :: NullOrUndefined.NullOrUndefined (LogStreams)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeLogStreamsResponse :: Newtype DescribeLogStreamsResponse _
derive instance repGenericDescribeLogStreamsResponse :: Generic DescribeLogStreamsResponse _
instance showDescribeLogStreamsResponse :: Show DescribeLogStreamsResponse where
  show = genericShow
instance decodeDescribeLogStreamsResponse :: Decode DescribeLogStreamsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLogStreamsResponse :: Encode DescribeLogStreamsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMetricFiltersRequest = DescribeMetricFiltersRequest 
  { "LogGroupName'" :: NullOrUndefined.NullOrUndefined (LogGroupName)
  , "FilterNamePrefix'" :: NullOrUndefined.NullOrUndefined (FilterName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (DescribeLimit)
  , "MetricName'" :: NullOrUndefined.NullOrUndefined (MetricName)
  , "MetricNamespace'" :: NullOrUndefined.NullOrUndefined (MetricNamespace)
  }
derive instance newtypeDescribeMetricFiltersRequest :: Newtype DescribeMetricFiltersRequest _
derive instance repGenericDescribeMetricFiltersRequest :: Generic DescribeMetricFiltersRequest _
instance showDescribeMetricFiltersRequest :: Show DescribeMetricFiltersRequest where
  show = genericShow
instance decodeDescribeMetricFiltersRequest :: Decode DescribeMetricFiltersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMetricFiltersRequest :: Encode DescribeMetricFiltersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeMetricFiltersResponse = DescribeMetricFiltersResponse 
  { "MetricFilters'" :: NullOrUndefined.NullOrUndefined (MetricFilters)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeMetricFiltersResponse :: Newtype DescribeMetricFiltersResponse _
derive instance repGenericDescribeMetricFiltersResponse :: Generic DescribeMetricFiltersResponse _
instance showDescribeMetricFiltersResponse :: Show DescribeMetricFiltersResponse where
  show = genericShow
instance decodeDescribeMetricFiltersResponse :: Decode DescribeMetricFiltersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeMetricFiltersResponse :: Encode DescribeMetricFiltersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeResourcePoliciesRequest = DescribeResourcePoliciesRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (DescribeLimit)
  }
derive instance newtypeDescribeResourcePoliciesRequest :: Newtype DescribeResourcePoliciesRequest _
derive instance repGenericDescribeResourcePoliciesRequest :: Generic DescribeResourcePoliciesRequest _
instance showDescribeResourcePoliciesRequest :: Show DescribeResourcePoliciesRequest where
  show = genericShow
instance decodeDescribeResourcePoliciesRequest :: Decode DescribeResourcePoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResourcePoliciesRequest :: Encode DescribeResourcePoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeResourcePoliciesResponse = DescribeResourcePoliciesResponse 
  { "ResourcePolicies'" :: NullOrUndefined.NullOrUndefined (ResourcePolicies)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeResourcePoliciesResponse :: Newtype DescribeResourcePoliciesResponse _
derive instance repGenericDescribeResourcePoliciesResponse :: Generic DescribeResourcePoliciesResponse _
instance showDescribeResourcePoliciesResponse :: Show DescribeResourcePoliciesResponse where
  show = genericShow
instance decodeDescribeResourcePoliciesResponse :: Decode DescribeResourcePoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResourcePoliciesResponse :: Encode DescribeResourcePoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeSubscriptionFiltersRequest = DescribeSubscriptionFiltersRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterNamePrefix'" :: NullOrUndefined.NullOrUndefined (FilterName)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (DescribeLimit)
  }
derive instance newtypeDescribeSubscriptionFiltersRequest :: Newtype DescribeSubscriptionFiltersRequest _
derive instance repGenericDescribeSubscriptionFiltersRequest :: Generic DescribeSubscriptionFiltersRequest _
instance showDescribeSubscriptionFiltersRequest :: Show DescribeSubscriptionFiltersRequest where
  show = genericShow
instance decodeDescribeSubscriptionFiltersRequest :: Decode DescribeSubscriptionFiltersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSubscriptionFiltersRequest :: Encode DescribeSubscriptionFiltersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeSubscriptionFiltersResponse = DescribeSubscriptionFiltersResponse 
  { "SubscriptionFilters'" :: NullOrUndefined.NullOrUndefined (SubscriptionFilters)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeSubscriptionFiltersResponse :: Newtype DescribeSubscriptionFiltersResponse _
derive instance repGenericDescribeSubscriptionFiltersResponse :: Generic DescribeSubscriptionFiltersResponse _
instance showDescribeSubscriptionFiltersResponse :: Show DescribeSubscriptionFiltersResponse where
  show = genericShow
instance decodeDescribeSubscriptionFiltersResponse :: Decode DescribeSubscriptionFiltersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSubscriptionFiltersResponse :: Encode DescribeSubscriptionFiltersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a cross-account destination that receives subscription log events.</p>
newtype Destination = Destination 
  { "DestinationName'" :: NullOrUndefined.NullOrUndefined (DestinationName)
  , "TargetArn'" :: NullOrUndefined.NullOrUndefined (TargetArn)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "AccessPolicy'" :: NullOrUndefined.NullOrUndefined (AccessPolicy)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (Arn)
  , "CreationTime'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDestination :: Newtype Destination _
derive instance repGenericDestination :: Generic Destination _
instance showDestination :: Show Destination where
  show = genericShow
instance decodeDestination :: Decode Destination where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestination :: Encode Destination where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DestinationArn = DestinationArn String
derive instance newtypeDestinationArn :: Newtype DestinationArn _
derive instance repGenericDestinationArn :: Generic DestinationArn _
instance showDestinationArn :: Show DestinationArn where
  show = genericShow
instance decodeDestinationArn :: Decode DestinationArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinationArn :: Encode DestinationArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DestinationName = DestinationName String
derive instance newtypeDestinationName :: Newtype DestinationName _
derive instance repGenericDestinationName :: Generic DestinationName _
instance showDestinationName :: Show DestinationName where
  show = genericShow
instance decodeDestinationName :: Decode DestinationName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinationName :: Encode DestinationName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Destinations = Destinations (Array Destination)
derive instance newtypeDestinations :: Newtype Destinations _
derive instance repGenericDestinations :: Generic Destinations _
instance showDestinations :: Show Destinations where
  show = genericShow
instance decodeDestinations :: Decode Destinations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinations :: Encode Destinations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateKmsKeyRequest = DisassociateKmsKeyRequest 
  { "LogGroupName'" :: (LogGroupName)
  }
derive instance newtypeDisassociateKmsKeyRequest :: Newtype DisassociateKmsKeyRequest _
derive instance repGenericDisassociateKmsKeyRequest :: Generic DisassociateKmsKeyRequest _
instance showDisassociateKmsKeyRequest :: Show DisassociateKmsKeyRequest where
  show = genericShow
instance decodeDisassociateKmsKeyRequest :: Decode DisassociateKmsKeyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateKmsKeyRequest :: Encode DisassociateKmsKeyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The method used to distribute log data to the destination, which can be either random or grouped by log stream.</p>
newtype Distribution = Distribution String
derive instance newtypeDistribution :: Newtype Distribution _
derive instance repGenericDistribution :: Generic Distribution _
instance showDistribution :: Show Distribution where
  show = genericShow
instance decodeDistribution :: Decode Distribution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDistribution :: Encode Distribution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventId = EventId String
derive instance newtypeEventId :: Newtype EventId _
derive instance repGenericEventId :: Generic EventId _
instance showEventId :: Show EventId where
  show = genericShow
instance decodeEventId :: Decode EventId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventId :: Encode EventId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventMessage = EventMessage String
derive instance newtypeEventMessage :: Newtype EventMessage _
derive instance repGenericEventMessage :: Generic EventMessage _
instance showEventMessage :: Show EventMessage where
  show = genericShow
instance decodeEventMessage :: Decode EventMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventMessage :: Encode EventMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventNumber = EventNumber Number
derive instance newtypeEventNumber :: Newtype EventNumber _
derive instance repGenericEventNumber :: Generic EventNumber _
instance showEventNumber :: Show EventNumber where
  show = genericShow
instance decodeEventNumber :: Decode EventNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventNumber :: Encode EventNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventsLimit = EventsLimit Int
derive instance newtypeEventsLimit :: Newtype EventsLimit _
derive instance repGenericEventsLimit :: Generic EventsLimit _
instance showEventsLimit :: Show EventsLimit where
  show = genericShow
instance decodeEventsLimit :: Decode EventsLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventsLimit :: Encode EventsLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportDestinationBucket = ExportDestinationBucket String
derive instance newtypeExportDestinationBucket :: Newtype ExportDestinationBucket _
derive instance repGenericExportDestinationBucket :: Generic ExportDestinationBucket _
instance showExportDestinationBucket :: Show ExportDestinationBucket where
  show = genericShow
instance decodeExportDestinationBucket :: Decode ExportDestinationBucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportDestinationBucket :: Encode ExportDestinationBucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportDestinationPrefix = ExportDestinationPrefix String
derive instance newtypeExportDestinationPrefix :: Newtype ExportDestinationPrefix _
derive instance repGenericExportDestinationPrefix :: Generic ExportDestinationPrefix _
instance showExportDestinationPrefix :: Show ExportDestinationPrefix where
  show = genericShow
instance decodeExportDestinationPrefix :: Decode ExportDestinationPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportDestinationPrefix :: Encode ExportDestinationPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an export task.</p>
newtype ExportTask = ExportTask 
  { "TaskId'" :: NullOrUndefined.NullOrUndefined (ExportTaskId)
  , "TaskName'" :: NullOrUndefined.NullOrUndefined (ExportTaskName)
  , "LogGroupName'" :: NullOrUndefined.NullOrUndefined (LogGroupName)
  , "From'" :: NullOrUndefined.NullOrUndefined (Number)
  , "To'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Destination'" :: NullOrUndefined.NullOrUndefined (ExportDestinationBucket)
  , "DestinationPrefix'" :: NullOrUndefined.NullOrUndefined (ExportDestinationPrefix)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ExportTaskStatus)
  , "ExecutionInfo'" :: NullOrUndefined.NullOrUndefined (ExportTaskExecutionInfo)
  }
derive instance newtypeExportTask :: Newtype ExportTask _
derive instance repGenericExportTask :: Generic ExportTask _
instance showExportTask :: Show ExportTask where
  show = genericShow
instance decodeExportTask :: Decode ExportTask where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportTask :: Encode ExportTask where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the status of an export task.</p>
newtype ExportTaskExecutionInfo = ExportTaskExecutionInfo 
  { "CreationTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CompletionTime'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeExportTaskExecutionInfo :: Newtype ExportTaskExecutionInfo _
derive instance repGenericExportTaskExecutionInfo :: Generic ExportTaskExecutionInfo _
instance showExportTaskExecutionInfo :: Show ExportTaskExecutionInfo where
  show = genericShow
instance decodeExportTaskExecutionInfo :: Decode ExportTaskExecutionInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportTaskExecutionInfo :: Encode ExportTaskExecutionInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportTaskId = ExportTaskId String
derive instance newtypeExportTaskId :: Newtype ExportTaskId _
derive instance repGenericExportTaskId :: Generic ExportTaskId _
instance showExportTaskId :: Show ExportTaskId where
  show = genericShow
instance decodeExportTaskId :: Decode ExportTaskId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportTaskId :: Encode ExportTaskId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportTaskName = ExportTaskName String
derive instance newtypeExportTaskName :: Newtype ExportTaskName _
derive instance repGenericExportTaskName :: Generic ExportTaskName _
instance showExportTaskName :: Show ExportTaskName where
  show = genericShow
instance decodeExportTaskName :: Decode ExportTaskName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportTaskName :: Encode ExportTaskName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the status of an export task.</p>
newtype ExportTaskStatus = ExportTaskStatus 
  { "Code'" :: NullOrUndefined.NullOrUndefined (ExportTaskStatusCode)
  , "Message'" :: NullOrUndefined.NullOrUndefined (ExportTaskStatusMessage)
  }
derive instance newtypeExportTaskStatus :: Newtype ExportTaskStatus _
derive instance repGenericExportTaskStatus :: Generic ExportTaskStatus _
instance showExportTaskStatus :: Show ExportTaskStatus where
  show = genericShow
instance decodeExportTaskStatus :: Decode ExportTaskStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportTaskStatus :: Encode ExportTaskStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportTaskStatusCode = ExportTaskStatusCode String
derive instance newtypeExportTaskStatusCode :: Newtype ExportTaskStatusCode _
derive instance repGenericExportTaskStatusCode :: Generic ExportTaskStatusCode _
instance showExportTaskStatusCode :: Show ExportTaskStatusCode where
  show = genericShow
instance decodeExportTaskStatusCode :: Decode ExportTaskStatusCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportTaskStatusCode :: Encode ExportTaskStatusCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportTaskStatusMessage = ExportTaskStatusMessage String
derive instance newtypeExportTaskStatusMessage :: Newtype ExportTaskStatusMessage _
derive instance repGenericExportTaskStatusMessage :: Generic ExportTaskStatusMessage _
instance showExportTaskStatusMessage :: Show ExportTaskStatusMessage where
  show = genericShow
instance decodeExportTaskStatusMessage :: Decode ExportTaskStatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportTaskStatusMessage :: Encode ExportTaskStatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportTasks = ExportTasks (Array ExportTask)
derive instance newtypeExportTasks :: Newtype ExportTasks _
derive instance repGenericExportTasks :: Generic ExportTasks _
instance showExportTasks :: Show ExportTasks where
  show = genericShow
instance decodeExportTasks :: Decode ExportTasks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportTasks :: Encode ExportTasks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExtractedValues = ExtractedValues (StrMap.StrMap Value)
derive instance newtypeExtractedValues :: Newtype ExtractedValues _
derive instance repGenericExtractedValues :: Generic ExtractedValues _
instance showExtractedValues :: Show ExtractedValues where
  show = genericShow
instance decodeExtractedValues :: Decode ExtractedValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtractedValues :: Encode ExtractedValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterCount = FilterCount Int
derive instance newtypeFilterCount :: Newtype FilterCount _
derive instance repGenericFilterCount :: Generic FilterCount _
instance showFilterCount :: Show FilterCount where
  show = genericShow
instance decodeFilterCount :: Decode FilterCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterCount :: Encode FilterCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterLogEventsRequest = FilterLogEventsRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamNames'" :: NullOrUndefined.NullOrUndefined (InputLogStreamNames)
  , "StartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "FilterPattern'" :: NullOrUndefined.NullOrUndefined (FilterPattern)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (EventsLimit)
  , "Interleaved'" :: NullOrUndefined.NullOrUndefined (Interleaved)
  }
derive instance newtypeFilterLogEventsRequest :: Newtype FilterLogEventsRequest _
derive instance repGenericFilterLogEventsRequest :: Generic FilterLogEventsRequest _
instance showFilterLogEventsRequest :: Show FilterLogEventsRequest where
  show = genericShow
instance decodeFilterLogEventsRequest :: Decode FilterLogEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterLogEventsRequest :: Encode FilterLogEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterLogEventsResponse = FilterLogEventsResponse 
  { "Events'" :: NullOrUndefined.NullOrUndefined (FilteredLogEvents)
  , "SearchedLogStreams'" :: NullOrUndefined.NullOrUndefined (SearchedLogStreams)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeFilterLogEventsResponse :: Newtype FilterLogEventsResponse _
derive instance repGenericFilterLogEventsResponse :: Generic FilterLogEventsResponse _
instance showFilterLogEventsResponse :: Show FilterLogEventsResponse where
  show = genericShow
instance decodeFilterLogEventsResponse :: Decode FilterLogEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterLogEventsResponse :: Encode FilterLogEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterName = FilterName String
derive instance newtypeFilterName :: Newtype FilterName _
derive instance repGenericFilterName :: Generic FilterName _
instance showFilterName :: Show FilterName where
  show = genericShow
instance decodeFilterName :: Decode FilterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterName :: Encode FilterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A symbolic description of how CloudWatch Logs should interpret the data in each log event. For example, a log event may contain time stamps, IP addresses, strings, and so on. You use the filter pattern to specify what to look for in the log event message.</p>
newtype FilterPattern = FilterPattern String
derive instance newtypeFilterPattern :: Newtype FilterPattern _
derive instance repGenericFilterPattern :: Generic FilterPattern _
instance showFilterPattern :: Show FilterPattern where
  show = genericShow
instance decodeFilterPattern :: Decode FilterPattern where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterPattern :: Encode FilterPattern where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a matched event.</p>
newtype FilteredLogEvent = FilteredLogEvent 
  { "LogStreamName'" :: NullOrUndefined.NullOrUndefined (LogStreamName)
  , "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Message'" :: NullOrUndefined.NullOrUndefined (EventMessage)
  , "IngestionTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "EventId'" :: NullOrUndefined.NullOrUndefined (EventId)
  }
derive instance newtypeFilteredLogEvent :: Newtype FilteredLogEvent _
derive instance repGenericFilteredLogEvent :: Generic FilteredLogEvent _
instance showFilteredLogEvent :: Show FilteredLogEvent where
  show = genericShow
instance decodeFilteredLogEvent :: Decode FilteredLogEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilteredLogEvent :: Encode FilteredLogEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilteredLogEvents = FilteredLogEvents (Array FilteredLogEvent)
derive instance newtypeFilteredLogEvents :: Newtype FilteredLogEvents _
derive instance repGenericFilteredLogEvents :: Generic FilteredLogEvents _
instance showFilteredLogEvents :: Show FilteredLogEvents where
  show = genericShow
instance decodeFilteredLogEvents :: Decode FilteredLogEvents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilteredLogEvents :: Encode FilteredLogEvents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLogEventsRequest = GetLogEventsRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamName'" :: (LogStreamName)
  , "StartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (EventsLimit)
  , "StartFromHead'" :: NullOrUndefined.NullOrUndefined (StartFromHead)
  }
derive instance newtypeGetLogEventsRequest :: Newtype GetLogEventsRequest _
derive instance repGenericGetLogEventsRequest :: Generic GetLogEventsRequest _
instance showGetLogEventsRequest :: Show GetLogEventsRequest where
  show = genericShow
instance decodeGetLogEventsRequest :: Decode GetLogEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLogEventsRequest :: Encode GetLogEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetLogEventsResponse = GetLogEventsResponse 
  { "Events'" :: NullOrUndefined.NullOrUndefined (OutputLogEvents)
  , "NextForwardToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "NextBackwardToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetLogEventsResponse :: Newtype GetLogEventsResponse _
derive instance repGenericGetLogEventsResponse :: Generic GetLogEventsResponse _
instance showGetLogEventsResponse :: Show GetLogEventsResponse where
  show = genericShow
instance decodeGetLogEventsResponse :: Decode GetLogEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetLogEventsResponse :: Encode GetLogEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a log event, which is a record of activity that was recorded by the application or resource being monitored.</p>
newtype InputLogEvent = InputLogEvent 
  { "Number" :: (Number)
  , "Message'" :: (EventMessage)
  }
derive instance newtypeInputLogEvent :: Newtype InputLogEvent _
derive instance repGenericInputLogEvent :: Generic InputLogEvent _
instance showInputLogEvent :: Show InputLogEvent where
  show = genericShow
instance decodeInputLogEvent :: Decode InputLogEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLogEvent :: Encode InputLogEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputLogEvents = InputLogEvents (Array InputLogEvent)
derive instance newtypeInputLogEvents :: Newtype InputLogEvents _
derive instance repGenericInputLogEvents :: Generic InputLogEvents _
instance showInputLogEvents :: Show InputLogEvents where
  show = genericShow
instance decodeInputLogEvents :: Decode InputLogEvents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLogEvents :: Encode InputLogEvents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputLogStreamNames = InputLogStreamNames (Array LogStreamName)
derive instance newtypeInputLogStreamNames :: Newtype InputLogStreamNames _
derive instance repGenericInputLogStreamNames :: Generic InputLogStreamNames _
instance showInputLogStreamNames :: Show InputLogStreamNames where
  show = genericShow
instance decodeInputLogStreamNames :: Decode InputLogStreamNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputLogStreamNames :: Encode InputLogStreamNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Interleaved = Interleaved Boolean
derive instance newtypeInterleaved :: Newtype Interleaved _
derive instance repGenericInterleaved :: Generic Interleaved _
instance showInterleaved :: Show Interleaved where
  show = genericShow
instance decodeInterleaved :: Decode Interleaved where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInterleaved :: Encode Interleaved where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation is not valid on the specified resource.</p>
newtype InvalidOperationException = InvalidOperationException Types.NoArguments
derive instance newtypeInvalidOperationException :: Newtype InvalidOperationException _
derive instance repGenericInvalidOperationException :: Generic InvalidOperationException _
instance showInvalidOperationException :: Show InvalidOperationException where
  show = genericShow
instance decodeInvalidOperationException :: Decode InvalidOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOperationException :: Encode InvalidOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A parameter is specified incorrectly.</p>
newtype InvalidParameterException = InvalidParameterException Types.NoArguments
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The sequence token is not valid.</p>
newtype InvalidSequenceTokenException = InvalidSequenceTokenException 
  { "ExpectedSequenceToken'" :: NullOrUndefined.NullOrUndefined (SequenceToken)
  }
derive instance newtypeInvalidSequenceTokenException :: Newtype InvalidSequenceTokenException _
derive instance repGenericInvalidSequenceTokenException :: Generic InvalidSequenceTokenException _
instance showInvalidSequenceTokenException :: Show InvalidSequenceTokenException where
  show = genericShow
instance decodeInvalidSequenceTokenException :: Decode InvalidSequenceTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSequenceTokenException :: Encode InvalidSequenceTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KmsKeyId = KmsKeyId String
derive instance newtypeKmsKeyId :: Newtype KmsKeyId _
derive instance repGenericKmsKeyId :: Generic KmsKeyId _
instance showKmsKeyId :: Show KmsKeyId where
  show = genericShow
instance decodeKmsKeyId :: Decode KmsKeyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKmsKeyId :: Encode KmsKeyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have reached the maximum number of resources that can be created.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsLogGroupRequest = ListTagsLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  }
derive instance newtypeListTagsLogGroupRequest :: Newtype ListTagsLogGroupRequest _
derive instance repGenericListTagsLogGroupRequest :: Generic ListTagsLogGroupRequest _
instance showListTagsLogGroupRequest :: Show ListTagsLogGroupRequest where
  show = genericShow
instance decodeListTagsLogGroupRequest :: Decode ListTagsLogGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsLogGroupRequest :: Encode ListTagsLogGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsLogGroupResponse = ListTagsLogGroupResponse 
  { "Tags'" :: NullOrUndefined.NullOrUndefined (Tags)
  }
derive instance newtypeListTagsLogGroupResponse :: Newtype ListTagsLogGroupResponse _
derive instance repGenericListTagsLogGroupResponse :: Generic ListTagsLogGroupResponse _
instance showListTagsLogGroupResponse :: Show ListTagsLogGroupResponse where
  show = genericShow
instance decodeListTagsLogGroupResponse :: Decode ListTagsLogGroupResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsLogGroupResponse :: Encode ListTagsLogGroupResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogEventIndex = LogEventIndex Int
derive instance newtypeLogEventIndex :: Newtype LogEventIndex _
derive instance repGenericLogEventIndex :: Generic LogEventIndex _
instance showLogEventIndex :: Show LogEventIndex where
  show = genericShow
instance decodeLogEventIndex :: Decode LogEventIndex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogEventIndex :: Encode LogEventIndex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a log group.</p>
newtype LogGroup = LogGroup 
  { "LogGroupName'" :: NullOrUndefined.NullOrUndefined (LogGroupName)
  , "CreationTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "RetentionInDays'" :: NullOrUndefined.NullOrUndefined (Days)
  , "MetricFilterCount'" :: NullOrUndefined.NullOrUndefined (FilterCount)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (Arn)
  , "StoredBytes'" :: NullOrUndefined.NullOrUndefined (StoredBytes)
  , "KmsKeyId'" :: NullOrUndefined.NullOrUndefined (KmsKeyId)
  }
derive instance newtypeLogGroup :: Newtype LogGroup _
derive instance repGenericLogGroup :: Generic LogGroup _
instance showLogGroup :: Show LogGroup where
  show = genericShow
instance decodeLogGroup :: Decode LogGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogGroup :: Encode LogGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogGroupName = LogGroupName String
derive instance newtypeLogGroupName :: Newtype LogGroupName _
derive instance repGenericLogGroupName :: Generic LogGroupName _
instance showLogGroupName :: Show LogGroupName where
  show = genericShow
instance decodeLogGroupName :: Decode LogGroupName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogGroupName :: Encode LogGroupName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogGroups = LogGroups (Array LogGroup)
derive instance newtypeLogGroups :: Newtype LogGroups _
derive instance repGenericLogGroups :: Generic LogGroups _
instance showLogGroups :: Show LogGroups where
  show = genericShow
instance decodeLogGroups :: Decode LogGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogGroups :: Encode LogGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a log stream, which is a sequence of log events from a single emitter of logs.</p>
newtype LogStream = LogStream 
  { "LogStreamName'" :: NullOrUndefined.NullOrUndefined (LogStreamName)
  , "CreationTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "FirstEventTimestamp'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastEventTimestamp'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastIngestionTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UploadSequenceToken'" :: NullOrUndefined.NullOrUndefined (SequenceToken)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (Arn)
  , "StoredBytes'" :: NullOrUndefined.NullOrUndefined (StoredBytes)
  }
derive instance newtypeLogStream :: Newtype LogStream _
derive instance repGenericLogStream :: Generic LogStream _
instance showLogStream :: Show LogStream where
  show = genericShow
instance decodeLogStream :: Decode LogStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogStream :: Encode LogStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogStreamName = LogStreamName String
derive instance newtypeLogStreamName :: Newtype LogStreamName _
derive instance repGenericLogStreamName :: Generic LogStreamName _
instance showLogStreamName :: Show LogStreamName where
  show = genericShow
instance decodeLogStreamName :: Decode LogStreamName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogStreamName :: Encode LogStreamName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogStreamSearchedCompletely = LogStreamSearchedCompletely Boolean
derive instance newtypeLogStreamSearchedCompletely :: Newtype LogStreamSearchedCompletely _
derive instance repGenericLogStreamSearchedCompletely :: Generic LogStreamSearchedCompletely _
instance showLogStreamSearchedCompletely :: Show LogStreamSearchedCompletely where
  show = genericShow
instance decodeLogStreamSearchedCompletely :: Decode LogStreamSearchedCompletely where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogStreamSearchedCompletely :: Encode LogStreamSearchedCompletely where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogStreams = LogStreams (Array LogStream)
derive instance newtypeLogStreams :: Newtype LogStreams _
derive instance repGenericLogStreams :: Generic LogStreams _
instance showLogStreams :: Show LogStreams where
  show = genericShow
instance decodeLogStreams :: Decode LogStreams where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogStreams :: Encode LogStreams where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Metric filters express how CloudWatch Logs would extract metric observations from ingested log events and transform them into metric data in a CloudWatch metric.</p>
newtype MetricFilter = MetricFilter 
  { "FilterName'" :: NullOrUndefined.NullOrUndefined (FilterName)
  , "FilterPattern'" :: NullOrUndefined.NullOrUndefined (FilterPattern)
  , "MetricTransformations'" :: NullOrUndefined.NullOrUndefined (MetricTransformations)
  , "CreationTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LogGroupName'" :: NullOrUndefined.NullOrUndefined (LogGroupName)
  }
derive instance newtypeMetricFilter :: Newtype MetricFilter _
derive instance repGenericMetricFilter :: Generic MetricFilter _
instance showMetricFilter :: Show MetricFilter where
  show = genericShow
instance decodeMetricFilter :: Decode MetricFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricFilter :: Encode MetricFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a matched event.</p>
newtype MetricFilterMatchRecord = MetricFilterMatchRecord 
  { "EventNumber'" :: NullOrUndefined.NullOrUndefined (EventNumber)
  , "EventMessage'" :: NullOrUndefined.NullOrUndefined (EventMessage)
  , "ExtractedValues'" :: NullOrUndefined.NullOrUndefined (ExtractedValues)
  }
derive instance newtypeMetricFilterMatchRecord :: Newtype MetricFilterMatchRecord _
derive instance repGenericMetricFilterMatchRecord :: Generic MetricFilterMatchRecord _
instance showMetricFilterMatchRecord :: Show MetricFilterMatchRecord where
  show = genericShow
instance decodeMetricFilterMatchRecord :: Decode MetricFilterMatchRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricFilterMatchRecord :: Encode MetricFilterMatchRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricFilterMatches = MetricFilterMatches (Array MetricFilterMatchRecord)
derive instance newtypeMetricFilterMatches :: Newtype MetricFilterMatches _
derive instance repGenericMetricFilterMatches :: Generic MetricFilterMatches _
instance showMetricFilterMatches :: Show MetricFilterMatches where
  show = genericShow
instance decodeMetricFilterMatches :: Decode MetricFilterMatches where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricFilterMatches :: Encode MetricFilterMatches where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricFilters = MetricFilters (Array MetricFilter)
derive instance newtypeMetricFilters :: Newtype MetricFilters _
derive instance repGenericMetricFilters :: Generic MetricFilters _
instance showMetricFilters :: Show MetricFilters where
  show = genericShow
instance decodeMetricFilters :: Decode MetricFilters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricFilters :: Encode MetricFilters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The name of the CloudWatch metric to which the monitored log information should be published. For example, you may publish to a metric called ErrorCount.</p>
newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _
derive instance repGenericMetricName :: Generic MetricName _
instance showMetricName :: Show MetricName where
  show = genericShow
instance decodeMetricName :: Decode MetricName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricName :: Encode MetricName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricNamespace = MetricNamespace String
derive instance newtypeMetricNamespace :: Newtype MetricNamespace _
derive instance repGenericMetricNamespace :: Generic MetricNamespace _
instance showMetricNamespace :: Show MetricNamespace where
  show = genericShow
instance decodeMetricNamespace :: Decode MetricNamespace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricNamespace :: Encode MetricNamespace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates how to transform ingested log events in to metric data in a CloudWatch metric.</p>
newtype MetricTransformation = MetricTransformation 
  { "MetricName'" :: (MetricName)
  , "MetricNamespace'" :: (MetricNamespace)
  , "MetricValue'" :: (MetricValue)
  , "DefaultValue'" :: NullOrUndefined.NullOrUndefined (DefaultValue)
  }
derive instance newtypeMetricTransformation :: Newtype MetricTransformation _
derive instance repGenericMetricTransformation :: Generic MetricTransformation _
instance showMetricTransformation :: Show MetricTransformation where
  show = genericShow
instance decodeMetricTransformation :: Decode MetricTransformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricTransformation :: Encode MetricTransformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricTransformations = MetricTransformations (Array MetricTransformation)
derive instance newtypeMetricTransformations :: Newtype MetricTransformations _
derive instance repGenericMetricTransformations :: Generic MetricTransformations _
instance showMetricTransformations :: Show MetricTransformations where
  show = genericShow
instance decodeMetricTransformations :: Decode MetricTransformations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricTransformations :: Encode MetricTransformations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value to publish to the CloudWatch metric. For example, if you're counting the occurrences of a term like "Error", the value is "1" for each occurrence. If you're counting the bytes transferred, the value is the value in the log event.</p>
newtype MetricValue = MetricValue String
derive instance newtypeMetricValue :: Newtype MetricValue _
derive instance repGenericMetricValue :: Generic MetricValue _
instance showMetricValue :: Show MetricValue where
  show = genericShow
instance decodeMetricValue :: Decode MetricValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricValue :: Encode MetricValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The token for the next set of items to return. The token expires after 24 hours.</p>
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Multiple requests to update the same resource were in conflict.</p>
newtype OperationAbortedException = OperationAbortedException Types.NoArguments
derive instance newtypeOperationAbortedException :: Newtype OperationAbortedException _
derive instance repGenericOperationAbortedException :: Generic OperationAbortedException _
instance showOperationAbortedException :: Show OperationAbortedException where
  show = genericShow
instance decodeOperationAbortedException :: Decode OperationAbortedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationAbortedException :: Encode OperationAbortedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderBy = OrderBy String
derive instance newtypeOrderBy :: Newtype OrderBy _
derive instance repGenericOrderBy :: Generic OrderBy _
instance showOrderBy :: Show OrderBy where
  show = genericShow
instance decodeOrderBy :: Decode OrderBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderBy :: Encode OrderBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a log event.</p>
newtype OutputLogEvent = OutputLogEvent 
  { "Number" :: NullOrUndefined.NullOrUndefined (Number)
  , "Message'" :: NullOrUndefined.NullOrUndefined (EventMessage)
  , "IngestionTime'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeOutputLogEvent :: Newtype OutputLogEvent _
derive instance repGenericOutputLogEvent :: Generic OutputLogEvent _
instance showOutputLogEvent :: Show OutputLogEvent where
  show = genericShow
instance decodeOutputLogEvent :: Decode OutputLogEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputLogEvent :: Encode OutputLogEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutputLogEvents = OutputLogEvents (Array OutputLogEvent)
derive instance newtypeOutputLogEvents :: Newtype OutputLogEvents _
derive instance repGenericOutputLogEvents :: Generic OutputLogEvents _
instance showOutputLogEvents :: Show OutputLogEvents where
  show = genericShow
instance decodeOutputLogEvents :: Decode OutputLogEvents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputLogEvents :: Encode OutputLogEvents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyDocument = PolicyDocument String
derive instance newtypePolicyDocument :: Newtype PolicyDocument _
derive instance repGenericPolicyDocument :: Generic PolicyDocument _
instance showPolicyDocument :: Show PolicyDocument where
  show = genericShow
instance decodePolicyDocument :: Decode PolicyDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyDocument :: Encode PolicyDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyName = PolicyName String
derive instance newtypePolicyName :: Newtype PolicyName _
derive instance repGenericPolicyName :: Generic PolicyName _
instance showPolicyName :: Show PolicyName where
  show = genericShow
instance decodePolicyName :: Decode PolicyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyName :: Encode PolicyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutDestinationPolicyRequest = PutDestinationPolicyRequest 
  { "DestinationName'" :: (DestinationName)
  , "AccessPolicy'" :: (AccessPolicy)
  }
derive instance newtypePutDestinationPolicyRequest :: Newtype PutDestinationPolicyRequest _
derive instance repGenericPutDestinationPolicyRequest :: Generic PutDestinationPolicyRequest _
instance showPutDestinationPolicyRequest :: Show PutDestinationPolicyRequest where
  show = genericShow
instance decodePutDestinationPolicyRequest :: Decode PutDestinationPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutDestinationPolicyRequest :: Encode PutDestinationPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutDestinationRequest = PutDestinationRequest 
  { "DestinationName'" :: (DestinationName)
  , "TargetArn'" :: (TargetArn)
  , "RoleArn'" :: (RoleArn)
  }
derive instance newtypePutDestinationRequest :: Newtype PutDestinationRequest _
derive instance repGenericPutDestinationRequest :: Generic PutDestinationRequest _
instance showPutDestinationRequest :: Show PutDestinationRequest where
  show = genericShow
instance decodePutDestinationRequest :: Decode PutDestinationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutDestinationRequest :: Encode PutDestinationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutDestinationResponse = PutDestinationResponse 
  { "Destination'" :: NullOrUndefined.NullOrUndefined (Destination)
  }
derive instance newtypePutDestinationResponse :: Newtype PutDestinationResponse _
derive instance repGenericPutDestinationResponse :: Generic PutDestinationResponse _
instance showPutDestinationResponse :: Show PutDestinationResponse where
  show = genericShow
instance decodePutDestinationResponse :: Decode PutDestinationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutDestinationResponse :: Encode PutDestinationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutLogEventsRequest = PutLogEventsRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "LogStreamName'" :: (LogStreamName)
  , "LogEvents'" :: (InputLogEvents)
  , "SequenceToken'" :: NullOrUndefined.NullOrUndefined (SequenceToken)
  }
derive instance newtypePutLogEventsRequest :: Newtype PutLogEventsRequest _
derive instance repGenericPutLogEventsRequest :: Generic PutLogEventsRequest _
instance showPutLogEventsRequest :: Show PutLogEventsRequest where
  show = genericShow
instance decodePutLogEventsRequest :: Decode PutLogEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutLogEventsRequest :: Encode PutLogEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutLogEventsResponse = PutLogEventsResponse 
  { "NextSequenceToken'" :: NullOrUndefined.NullOrUndefined (SequenceToken)
  , "RejectedLogEventsInfo'" :: NullOrUndefined.NullOrUndefined (RejectedLogEventsInfo)
  }
derive instance newtypePutLogEventsResponse :: Newtype PutLogEventsResponse _
derive instance repGenericPutLogEventsResponse :: Generic PutLogEventsResponse _
instance showPutLogEventsResponse :: Show PutLogEventsResponse where
  show = genericShow
instance decodePutLogEventsResponse :: Decode PutLogEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutLogEventsResponse :: Encode PutLogEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutMetricFilterRequest = PutMetricFilterRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterName'" :: (FilterName)
  , "FilterPattern'" :: (FilterPattern)
  , "MetricTransformations'" :: (MetricTransformations)
  }
derive instance newtypePutMetricFilterRequest :: Newtype PutMetricFilterRequest _
derive instance repGenericPutMetricFilterRequest :: Generic PutMetricFilterRequest _
instance showPutMetricFilterRequest :: Show PutMetricFilterRequest where
  show = genericShow
instance decodePutMetricFilterRequest :: Decode PutMetricFilterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutMetricFilterRequest :: Encode PutMetricFilterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutResourcePolicyRequest = PutResourcePolicyRequest 
  { "PolicyName'" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "PolicyDocument'" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  }
derive instance newtypePutResourcePolicyRequest :: Newtype PutResourcePolicyRequest _
derive instance repGenericPutResourcePolicyRequest :: Generic PutResourcePolicyRequest _
instance showPutResourcePolicyRequest :: Show PutResourcePolicyRequest where
  show = genericShow
instance decodePutResourcePolicyRequest :: Decode PutResourcePolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutResourcePolicyRequest :: Encode PutResourcePolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutResourcePolicyResponse = PutResourcePolicyResponse 
  { "ResourcePolicy'" :: NullOrUndefined.NullOrUndefined (ResourcePolicy)
  }
derive instance newtypePutResourcePolicyResponse :: Newtype PutResourcePolicyResponse _
derive instance repGenericPutResourcePolicyResponse :: Generic PutResourcePolicyResponse _
instance showPutResourcePolicyResponse :: Show PutResourcePolicyResponse where
  show = genericShow
instance decodePutResourcePolicyResponse :: Decode PutResourcePolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutResourcePolicyResponse :: Encode PutResourcePolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRetentionPolicyRequest = PutRetentionPolicyRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "RetentionInDays'" :: (Days)
  }
derive instance newtypePutRetentionPolicyRequest :: Newtype PutRetentionPolicyRequest _
derive instance repGenericPutRetentionPolicyRequest :: Generic PutRetentionPolicyRequest _
instance showPutRetentionPolicyRequest :: Show PutRetentionPolicyRequest where
  show = genericShow
instance decodePutRetentionPolicyRequest :: Decode PutRetentionPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRetentionPolicyRequest :: Encode PutRetentionPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutSubscriptionFilterRequest = PutSubscriptionFilterRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "FilterName'" :: (FilterName)
  , "FilterPattern'" :: (FilterPattern)
  , "DestinationArn'" :: (DestinationArn)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "Distribution'" :: NullOrUndefined.NullOrUndefined (Distribution)
  }
derive instance newtypePutSubscriptionFilterRequest :: Newtype PutSubscriptionFilterRequest _
derive instance repGenericPutSubscriptionFilterRequest :: Generic PutSubscriptionFilterRequest _
instance showPutSubscriptionFilterRequest :: Show PutSubscriptionFilterRequest where
  show = genericShow
instance decodePutSubscriptionFilterRequest :: Decode PutSubscriptionFilterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutSubscriptionFilterRequest :: Encode PutSubscriptionFilterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the rejected events.</p>
newtype RejectedLogEventsInfo = RejectedLogEventsInfo 
  { "TooNewLogEventStartIndex'" :: NullOrUndefined.NullOrUndefined (LogEventIndex)
  , "TooOldLogEventEndIndex'" :: NullOrUndefined.NullOrUndefined (LogEventIndex)
  , "ExpiredLogEventEndIndex'" :: NullOrUndefined.NullOrUndefined (LogEventIndex)
  }
derive instance newtypeRejectedLogEventsInfo :: Newtype RejectedLogEventsInfo _
derive instance repGenericRejectedLogEventsInfo :: Generic RejectedLogEventsInfo _
instance showRejectedLogEventsInfo :: Show RejectedLogEventsInfo where
  show = genericShow
instance decodeRejectedLogEventsInfo :: Decode RejectedLogEventsInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRejectedLogEventsInfo :: Encode RejectedLogEventsInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException Types.NoArguments
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _
derive instance repGenericResourceAlreadyExistsException :: Generic ResourceAlreadyExistsException _
instance showResourceAlreadyExistsException :: Show ResourceAlreadyExistsException where
  show = genericShow
instance decodeResourceAlreadyExistsException :: Decode ResourceAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAlreadyExistsException :: Encode ResourceAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourcePolicies = ResourcePolicies (Array ResourcePolicy)
derive instance newtypeResourcePolicies :: Newtype ResourcePolicies _
derive instance repGenericResourcePolicies :: Generic ResourcePolicies _
instance showResourcePolicies :: Show ResourcePolicies where
  show = genericShow
instance decodeResourcePolicies :: Decode ResourcePolicies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourcePolicies :: Encode ResourcePolicies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A policy enabling one or more entities to put logs to a log group in this account.</p>
newtype ResourcePolicy = ResourcePolicy 
  { "PolicyName'" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "PolicyDocument'" :: NullOrUndefined.NullOrUndefined (PolicyDocument)
  , "LastUpdatedTime'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeResourcePolicy :: Newtype ResourcePolicy _
derive instance repGenericResourcePolicy :: Generic ResourcePolicy _
instance showResourcePolicy :: Show ResourcePolicy where
  show = genericShow
instance decodeResourcePolicy :: Decode ResourcePolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourcePolicy :: Encode ResourcePolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _
derive instance repGenericRoleArn :: Generic RoleArn _
instance showRoleArn :: Show RoleArn where
  show = genericShow
instance decodeRoleArn :: Decode RoleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleArn :: Encode RoleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the search status of a log stream.</p>
newtype SearchedLogStream = SearchedLogStream 
  { "LogStreamName'" :: NullOrUndefined.NullOrUndefined (LogStreamName)
  , "SearchedCompletely'" :: NullOrUndefined.NullOrUndefined (LogStreamSearchedCompletely)
  }
derive instance newtypeSearchedLogStream :: Newtype SearchedLogStream _
derive instance repGenericSearchedLogStream :: Generic SearchedLogStream _
instance showSearchedLogStream :: Show SearchedLogStream where
  show = genericShow
instance decodeSearchedLogStream :: Decode SearchedLogStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchedLogStream :: Encode SearchedLogStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchedLogStreams = SearchedLogStreams (Array SearchedLogStream)
derive instance newtypeSearchedLogStreams :: Newtype SearchedLogStreams _
derive instance repGenericSearchedLogStreams :: Generic SearchedLogStreams _
instance showSearchedLogStreams :: Show SearchedLogStreams where
  show = genericShow
instance decodeSearchedLogStreams :: Decode SearchedLogStreams where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchedLogStreams :: Encode SearchedLogStreams where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SequenceToken = SequenceToken String
derive instance newtypeSequenceToken :: Newtype SequenceToken _
derive instance repGenericSequenceToken :: Generic SequenceToken _
instance showSequenceToken :: Show SequenceToken where
  show = genericShow
instance decodeSequenceToken :: Decode SequenceToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSequenceToken :: Encode SequenceToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service cannot complete the request.</p>
newtype ServiceUnavailableException = ServiceUnavailableException Types.NoArguments
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartFromHead = StartFromHead Boolean
derive instance newtypeStartFromHead :: Newtype StartFromHead _
derive instance repGenericStartFromHead :: Generic StartFromHead _
instance showStartFromHead :: Show StartFromHead where
  show = genericShow
instance decodeStartFromHead :: Decode StartFromHead where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartFromHead :: Encode StartFromHead where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StoredBytes = StoredBytes Number
derive instance newtypeStoredBytes :: Newtype StoredBytes _
derive instance repGenericStoredBytes :: Generic StoredBytes _
instance showStoredBytes :: Show StoredBytes where
  show = genericShow
instance decodeStoredBytes :: Decode StoredBytes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStoredBytes :: Encode StoredBytes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a subscription filter.</p>
newtype SubscriptionFilter = SubscriptionFilter 
  { "FilterName'" :: NullOrUndefined.NullOrUndefined (FilterName)
  , "LogGroupName'" :: NullOrUndefined.NullOrUndefined (LogGroupName)
  , "FilterPattern'" :: NullOrUndefined.NullOrUndefined (FilterPattern)
  , "DestinationArn'" :: NullOrUndefined.NullOrUndefined (DestinationArn)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "Distribution'" :: NullOrUndefined.NullOrUndefined (Distribution)
  , "CreationTime'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeSubscriptionFilter :: Newtype SubscriptionFilter _
derive instance repGenericSubscriptionFilter :: Generic SubscriptionFilter _
instance showSubscriptionFilter :: Show SubscriptionFilter where
  show = genericShow
instance decodeSubscriptionFilter :: Decode SubscriptionFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionFilter :: Encode SubscriptionFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscriptionFilters = SubscriptionFilters (Array SubscriptionFilter)
derive instance newtypeSubscriptionFilters :: Newtype SubscriptionFilters _
derive instance repGenericSubscriptionFilters :: Generic SubscriptionFilters _
instance showSubscriptionFilters :: Show SubscriptionFilters where
  show = genericShow
instance decodeSubscriptionFilters :: Decode SubscriptionFilters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionFilters :: Encode SubscriptionFilters where
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


newtype TagList = TagList (Array TagKey)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagLogGroupRequest = TagLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "Tags'" :: (Tags)
  }
derive instance newtypeTagLogGroupRequest :: Newtype TagLogGroupRequest _
derive instance repGenericTagLogGroupRequest :: Generic TagLogGroupRequest _
instance showTagLogGroupRequest :: Show TagLogGroupRequest where
  show = genericShow
instance decodeTagLogGroupRequest :: Decode TagLogGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagLogGroupRequest :: Encode TagLogGroupRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tags = Tags (StrMap.StrMap TagValue)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetArn = TargetArn String
derive instance newtypeTargetArn :: Newtype TargetArn _
derive instance repGenericTargetArn :: Generic TargetArn _
instance showTargetArn :: Show TargetArn where
  show = genericShow
instance decodeTargetArn :: Decode TargetArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetArn :: Encode TargetArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestEventMessages = TestEventMessages (Array EventMessage)
derive instance newtypeTestEventMessages :: Newtype TestEventMessages _
derive instance repGenericTestEventMessages :: Generic TestEventMessages _
instance showTestEventMessages :: Show TestEventMessages where
  show = genericShow
instance decodeTestEventMessages :: Decode TestEventMessages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestEventMessages :: Encode TestEventMessages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestMetricFilterRequest = TestMetricFilterRequest 
  { "FilterPattern'" :: (FilterPattern)
  , "LogEventMessages'" :: (TestEventMessages)
  }
derive instance newtypeTestMetricFilterRequest :: Newtype TestMetricFilterRequest _
derive instance repGenericTestMetricFilterRequest :: Generic TestMetricFilterRequest _
instance showTestMetricFilterRequest :: Show TestMetricFilterRequest where
  show = genericShow
instance decodeTestMetricFilterRequest :: Decode TestMetricFilterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestMetricFilterRequest :: Encode TestMetricFilterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestMetricFilterResponse = TestMetricFilterResponse 
  { "Matches'" :: NullOrUndefined.NullOrUndefined (MetricFilterMatches)
  }
derive instance newtypeTestMetricFilterResponse :: Newtype TestMetricFilterResponse _
derive instance repGenericTestMetricFilterResponse :: Generic TestMetricFilterResponse _
instance showTestMetricFilterResponse :: Show TestMetricFilterResponse where
  show = genericShow
instance decodeTestMetricFilterResponse :: Decode TestMetricFilterResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestMetricFilterResponse :: Encode TestMetricFilterResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _
derive instance repGenericToken :: Generic Token _
instance showToken :: Show Token where
  show = genericShow
instance decodeToken :: Decode Token where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeToken :: Encode Token where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UntagLogGroupRequest = UntagLogGroupRequest 
  { "LogGroupName'" :: (LogGroupName)
  , "Tags'" :: (TagList)
  }
derive instance newtypeUntagLogGroupRequest :: Newtype UntagLogGroupRequest _
derive instance repGenericUntagLogGroupRequest :: Generic UntagLogGroupRequest _
instance showUntagLogGroupRequest :: Show UntagLogGroupRequest where
  show = genericShow
instance decodeUntagLogGroupRequest :: Decode UntagLogGroupRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUntagLogGroupRequest :: Encode UntagLogGroupRequest where
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
