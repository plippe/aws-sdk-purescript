## Module AWS.CloudWatchLogs

<p>You can use Amazon CloudWatch Logs to monitor, store, and access your log files from Amazon EC2 instances, AWS CloudTrail, or other sources. You can then retrieve the associated log data from CloudWatch Logs using the CloudWatch console, CloudWatch Logs commands in the AWS CLI, CloudWatch Logs API, or CloudWatch Logs SDK.</p> <p>You can use CloudWatch Logs to:</p> <ul> <li> <p> <b>Monitor logs from EC2 instances in real-time</b>: You can use CloudWatch Logs to monitor applications and systems using log data. For example, CloudWatch Logs can track the number of errors that occur in your application logs and send you a notification whenever the rate of errors exceeds a threshold that you specify. CloudWatch Logs uses your log data for monitoring; so, no code changes are required. For example, you can monitor application logs for specific literal terms (such as "NullReferenceException") or count the number of occurrences of a literal term at a particular position in log data (such as "404" status codes in an Apache access log). When the term you are searching for is found, CloudWatch Logs reports the data to a CloudWatch metric that you specify.</p> </li> <li> <p> <b>Monitor AWS CloudTrail logged events</b>: You can create alarms in CloudWatch and receive notifications of particular API activity as captured by CloudTrail and use the notification to perform troubleshooting.</p> </li> <li> <p> <b>Archive log data</b>: You can use CloudWatch Logs to store your log data in highly durable storage. You can change the log retention setting so that any log events older than this setting are automatically deleted. The CloudWatch Logs agent makes it easy to quickly send both rotated and non-rotated log data off of a host and into the log service. You can then access the raw log data when you need it.</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateKmsKey`

``` purescript
associateKmsKey :: forall eff. AssociateKmsKeyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Associates the specified AWS Key Management Service (AWS KMS) customer master key (CMK) with the specified log group.</p> <p>Associating an AWS KMS CMK with a log group overrides any existing associations between the log group and a CMK. After a CMK is associated with a log group, all newly ingested data for the log group is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.</p> <p>Note that it can take up to 5 minutes for this operation to take effect.</p> <p>If you attempt to associate a CMK with a log group but the CMK does not exist or the CMK is disabled, you will receive an <code>InvalidParameterException</code> error. </p>

#### `cancelExportTask`

``` purescript
cancelExportTask :: forall eff. CancelExportTaskRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Cancels the specified export task.</p> <p>The task must be in the <code>PENDING</code> or <code>RUNNING</code> state.</p>

#### `createExportTask`

``` purescript
createExportTask :: forall eff. CreateExportTaskRequest -> Aff (err :: RequestError | eff) CreateExportTaskResponse
```

<p>Creates an export task, which allows you to efficiently export data from a log group to an Amazon S3 bucket.</p> <p>This is an asynchronous call. If all the required information is provided, this operation initiates an export task and responds with the ID of the task. After the task has started, you can use <a>DescribeExportTasks</a> to get the status of the export task. Each account can only have one active (<code>RUNNING</code> or <code>PENDING</code>) export task at a time. To cancel an export task, use <a>CancelExportTask</a>.</p> <p>You can export logs from multiple log groups or multiple time ranges to the same S3 bucket. To separate out log data for each export task, you can specify a prefix to be used as the Amazon S3 key prefix for all exported objects.</p>

#### `createLogGroup`

``` purescript
createLogGroup :: forall eff. CreateLogGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a log group with the specified name.</p> <p>You can create up to 5000 log groups per account.</p> <p>You must use the following guidelines when naming a log group:</p> <ul> <li> <p>Log group names must be unique within a region for an AWS account.</p> </li> <li> <p>Log group names can be between 1 and 512 characters long.</p> </li> <li> <p>Log group names consist of the following characters: a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).</p> </li> </ul> <p>If you associate a AWS Key Management Service (AWS KMS) customer master key (CMK) with the log group, ingested data is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.</p> <p>If you attempt to associate a CMK with the log group but the CMK does not exist or the CMK is disabled, you will receive an <code>InvalidParameterException</code> error. </p>

#### `createLogStream`

``` purescript
createLogStream :: forall eff. CreateLogStreamRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a log stream for the specified log group.</p> <p>There is no limit on the number of log streams that you can create for a log group.</p> <p>You must use the following guidelines when naming a log stream:</p> <ul> <li> <p>Log stream names must be unique within the log group.</p> </li> <li> <p>Log stream names can be between 1 and 512 characters long.</p> </li> <li> <p>The ':' (colon) and '*' (asterisk) characters are not allowed.</p> </li> </ul>

#### `deleteDestination`

``` purescript
deleteDestination :: forall eff. DeleteDestinationRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified destination, and eventually disables all the subscription filters that publish to it. This operation does not delete the physical resource encapsulated by the destination.</p>

#### `deleteLogGroup`

``` purescript
deleteLogGroup :: forall eff. DeleteLogGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified log group and permanently deletes all the archived log events associated with the log group.</p>

#### `deleteLogStream`

``` purescript
deleteLogStream :: forall eff. DeleteLogStreamRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified log stream and permanently deletes all the archived log events associated with the log stream.</p>

#### `deleteMetricFilter`

``` purescript
deleteMetricFilter :: forall eff. DeleteMetricFilterRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified metric filter.</p>

#### `deleteResourcePolicy`

``` purescript
deleteResourcePolicy :: forall eff. DeleteResourcePolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a resource policy from this account. This revokes the access of the identities in that policy to put log events to this account.</p>

#### `deleteRetentionPolicy`

``` purescript
deleteRetentionPolicy :: forall eff. DeleteRetentionPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified retention policy.</p> <p>Log events do not expire if they belong to log groups without a retention policy.</p>

#### `deleteSubscriptionFilter`

``` purescript
deleteSubscriptionFilter :: forall eff. DeleteSubscriptionFilterRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified subscription filter.</p>

#### `describeDestinations`

``` purescript
describeDestinations :: forall eff. DescribeDestinationsRequest -> Aff (err :: RequestError | eff) DescribeDestinationsResponse
```

<p>Lists all your destinations. The results are ASCII-sorted by destination name.</p>

#### `describeExportTasks`

``` purescript
describeExportTasks :: forall eff. DescribeExportTasksRequest -> Aff (err :: RequestError | eff) DescribeExportTasksResponse
```

<p>Lists the specified export tasks. You can list all your export tasks or filter the results based on task ID or task status.</p>

#### `describeLogGroups`

``` purescript
describeLogGroups :: forall eff. DescribeLogGroupsRequest -> Aff (err :: RequestError | eff) DescribeLogGroupsResponse
```

<p>Lists the specified log groups. You can list all your log groups or filter the results by prefix. The results are ASCII-sorted by log group name.</p>

#### `describeLogStreams`

``` purescript
describeLogStreams :: forall eff. DescribeLogStreamsRequest -> Aff (err :: RequestError | eff) DescribeLogStreamsResponse
```

<p>Lists the log streams for the specified log group. You can list all the log streams or filter the results by prefix. You can also control how the results are ordered.</p> <p>This operation has a limit of five transactions per second, after which transactions are throttled.</p>

#### `describeMetricFilters`

``` purescript
describeMetricFilters :: forall eff. DescribeMetricFiltersRequest -> Aff (err :: RequestError | eff) DescribeMetricFiltersResponse
```

<p>Lists the specified metric filters. You can list all the metric filters or filter the results by log name, prefix, metric name, or metric namespace. The results are ASCII-sorted by filter name.</p>

#### `describeResourcePolicies`

``` purescript
describeResourcePolicies :: forall eff. DescribeResourcePoliciesRequest -> Aff (err :: RequestError | eff) DescribeResourcePoliciesResponse
```

<p>Lists the resource policies in this account.</p>

#### `describeSubscriptionFilters`

``` purescript
describeSubscriptionFilters :: forall eff. DescribeSubscriptionFiltersRequest -> Aff (err :: RequestError | eff) DescribeSubscriptionFiltersResponse
```

<p>Lists the subscription filters for the specified log group. You can list all the subscription filters or filter the results by prefix. The results are ASCII-sorted by filter name.</p>

#### `disassociateKmsKey`

``` purescript
disassociateKmsKey :: forall eff. DisassociateKmsKeyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Disassociates the associated AWS Key Management Service (AWS KMS) customer master key (CMK) from the specified log group.</p> <p>After the AWS KMS CMK is disassociated from the log group, AWS CloudWatch Logs stops encrypting newly ingested data for the log group. All previously ingested data remains encrypted, and AWS CloudWatch Logs requires permissions for the CMK whenever the encrypted data is requested.</p> <p>Note that it can take up to 5 minutes for this operation to take effect.</p>

#### `filterLogEvents`

``` purescript
filterLogEvents :: forall eff. FilterLogEventsRequest -> Aff (err :: RequestError | eff) FilterLogEventsResponse
```

<p>Lists log events from the specified log group. You can list all the log events or filter the results using a filter pattern, a time range, and the name of the log stream.</p> <p>By default, this operation returns as many log events as can fit in 1 MB (up to 10,000 log events), or all the events found within the time range that you specify. If the results include a token, then there are more log events available, and you can get additional results by specifying the token in a subsequent call.</p>

#### `getLogEvents`

``` purescript
getLogEvents :: forall eff. GetLogEventsRequest -> Aff (err :: RequestError | eff) GetLogEventsResponse
```

<p>Lists log events from the specified log stream. You can list all the log events or filter using a time range.</p> <p>By default, this operation returns as many log events as can fit in a response size of 1MB (up to 10,000 log events). You can get additional log events by specifying one of the tokens in a subsequent call.</p>

#### `listTagsLogGroup`

``` purescript
listTagsLogGroup :: forall eff. ListTagsLogGroupRequest -> Aff (err :: RequestError | eff) ListTagsLogGroupResponse
```

<p>Lists the tags for the specified log group.</p>

#### `putDestination`

``` purescript
putDestination :: forall eff. PutDestinationRequest -> Aff (err :: RequestError | eff) PutDestinationResponse
```

<p>Creates or updates a destination. A destination encapsulates a physical resource (such as an Amazon Kinesis stream) and enables you to subscribe to a real-time stream of log events for a different account, ingested using <a>PutLogEvents</a>. Currently, the only supported physical resource is a Kinesis stream belonging to the same account as the destination.</p> <p>Through an access policy, a destination controls what is written to its Kinesis stream. By default, <code>PutDestination</code> does not set any access policy with the destination, which means a cross-account user cannot call <a>PutSubscriptionFilter</a> against this destination. To enable this, the destination owner must call <a>PutDestinationPolicy</a> after <code>PutDestination</code>.</p>

#### `putDestinationPolicy`

``` purescript
putDestinationPolicy :: forall eff. PutDestinationPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates or updates an access policy associated with an existing destination. An access policy is an <a href="http://docs.aws.amazon.com/IAM/latest/UserGuide/policies_overview.html">IAM policy document</a> that is used to authorize claims to register a subscription filter against a given destination.</p>

#### `putLogEvents`

``` purescript
putLogEvents :: forall eff. PutLogEventsRequest -> Aff (err :: RequestError | eff) PutLogEventsResponse
```

<p>Uploads a batch of log events to the specified log stream.</p> <p>You must include the sequence token obtained from the response of the previous call. An upload in a newly created log stream does not require a sequence token. You can also get the sequence token using <a>DescribeLogStreams</a>. If you call <code>PutLogEvents</code> twice within a narrow time period using the same value for <code>sequenceToken</code>, both calls may be successful, or one may be rejected.</p> <p>The batch of events must satisfy the following constraints:</p> <ul> <li> <p>The maximum batch size is 1,048,576 bytes, and this size is calculated as the sum of all event messages in UTF-8, plus 26 bytes for each log event.</p> </li> <li> <p>None of the log events in the batch can be more than 2 hours in the future.</p> </li> <li> <p>None of the log events in the batch can be older than 14 days or the retention period of the log group.</p> </li> <li> <p>The log events in the batch must be in chronological ordered by their time stamp (the time the event occurred, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC).</p> </li> <li> <p>The maximum number of log events in a batch is 10,000.</p> </li> <li> <p>A batch of log events in a single request cannot span more than 24 hours. Otherwise, the operation fails.</p> </li> </ul>

#### `putMetricFilter`

``` purescript
putMetricFilter :: forall eff. PutMetricFilterRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates or updates a metric filter and associates it with the specified log group. Metric filters allow you to configure rules to extract metric data from log events ingested through <a>PutLogEvents</a>.</p> <p>The maximum number of metric filters that can be associated with a log group is 100.</p>

#### `putResourcePolicy`

``` purescript
putResourcePolicy :: forall eff. PutResourcePolicyRequest -> Aff (err :: RequestError | eff) PutResourcePolicyResponse
```

<p>Creates or updates a resource policy allowing other AWS services to put log events to this account, such as Amazon Route 53. An account can have up to 50 resource policies per region.</p>

#### `putRetentionPolicy`

``` purescript
putRetentionPolicy :: forall eff. PutRetentionPolicyRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Sets the retention of the specified log group. A retention policy allows you to configure the number of days for which to retain log events in the specified log group.</p>

#### `putSubscriptionFilter`

``` purescript
putSubscriptionFilter :: forall eff. PutSubscriptionFilterRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates or updates a subscription filter and associates it with the specified log group. Subscription filters allow you to subscribe to a real-time stream of log events ingested through <a>PutLogEvents</a> and have them delivered to a specific destination. Currently, the supported destinations are:</p> <ul> <li> <p>An Amazon Kinesis stream belonging to the same account as the subscription filter, for same-account delivery.</p> </li> <li> <p>A logical destination that belongs to a different account, for cross-account delivery.</p> </li> <li> <p>An Amazon Kinesis Firehose delivery stream that belongs to the same account as the subscription filter, for same-account delivery.</p> </li> <li> <p>An AWS Lambda function that belongs to the same account as the subscription filter, for same-account delivery.</p> </li> </ul> <p>There can only be one subscription filter associated with a log group. If you are updating an existing filter, you must specify the correct name in <code>filterName</code>. Otherwise, the call fails because you cannot associate a second filter with a log group.</p>

#### `tagLogGroup`

``` purescript
tagLogGroup :: forall eff. TagLogGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds or updates the specified tags for the specified log group.</p> <p>To list the tags for a log group, use <a>ListTagsLogGroup</a>. To remove tags, use <a>UntagLogGroup</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/log-group-tagging.html">Tag Log Groups in Amazon CloudWatch Logs</a> in the <i>Amazon CloudWatch Logs User Guide</i>.</p>

#### `testMetricFilter`

``` purescript
testMetricFilter :: forall eff. TestMetricFilterRequest -> Aff (err :: RequestError | eff) TestMetricFilterResponse
```

<p>Tests the filter pattern of a metric filter against a sample of log event messages. You can use this operation to validate the correctness of a metric filter pattern.</p>

#### `untagLogGroup`

``` purescript
untagLogGroup :: forall eff. UntagLogGroupRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes the specified tags from the specified log group.</p> <p>To list the tags for a log group, use <a>ListTagsLogGroup</a>. To add tags, use <a>UntagLogGroup</a>.</p>

#### `AccessPolicy`

``` purescript
newtype AccessPolicy
  = AccessPolicy String
```

##### Instances
``` purescript
Newtype AccessPolicy _
```

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

##### Instances
``` purescript
Newtype Arn _
```

#### `AssociateKmsKeyRequest`

``` purescript
newtype AssociateKmsKeyRequest
  = AssociateKmsKeyRequest { "LogGroupName'" :: LogGroupName, "KmsKeyId'" :: KmsKeyId }
```

##### Instances
``` purescript
Newtype AssociateKmsKeyRequest _
```

#### `CancelExportTaskRequest`

``` purescript
newtype CancelExportTaskRequest
  = CancelExportTaskRequest { "TaskId'" :: ExportTaskId }
```

##### Instances
``` purescript
Newtype CancelExportTaskRequest _
```

#### `CreateExportTaskRequest`

``` purescript
newtype CreateExportTaskRequest
  = CreateExportTaskRequest { "TaskName'" :: NullOrUndefined (ExportTaskName), "LogGroupName'" :: LogGroupName, "LogStreamNamePrefix'" :: NullOrUndefined (LogStreamName), "From'" :: Number, "To'" :: Number, "Destination'" :: ExportDestinationBucket, "DestinationPrefix'" :: NullOrUndefined (ExportDestinationPrefix) }
```

##### Instances
``` purescript
Newtype CreateExportTaskRequest _
```

#### `CreateExportTaskResponse`

``` purescript
newtype CreateExportTaskResponse
  = CreateExportTaskResponse { "TaskId'" :: NullOrUndefined (ExportTaskId) }
```

##### Instances
``` purescript
Newtype CreateExportTaskResponse _
```

#### `CreateLogGroupRequest`

``` purescript
newtype CreateLogGroupRequest
  = CreateLogGroupRequest { "LogGroupName'" :: LogGroupName, "KmsKeyId'" :: NullOrUndefined (KmsKeyId), "Tags'" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype CreateLogGroupRequest _
```

#### `CreateLogStreamRequest`

``` purescript
newtype CreateLogStreamRequest
  = CreateLogStreamRequest { "LogGroupName'" :: LogGroupName, "LogStreamName'" :: LogStreamName }
```

##### Instances
``` purescript
Newtype CreateLogStreamRequest _
```

#### `DataAlreadyAcceptedException`

``` purescript
newtype DataAlreadyAcceptedException
  = DataAlreadyAcceptedException { "ExpectedSequenceToken'" :: NullOrUndefined (SequenceToken) }
```

<p>The event was already logged.</p>

##### Instances
``` purescript
Newtype DataAlreadyAcceptedException _
```

#### `Days`

``` purescript
newtype Days
  = Days Int
```

<p>The number of days to retain the log events in the specified log group. Possible values are: 1, 3, 5, 7, 14, 30, 60, 90, 120, 150, 180, 365, 400, 545, 731, 1827, and 3653.</p>

##### Instances
``` purescript
Newtype Days _
```

#### `DefaultValue`

``` purescript
newtype DefaultValue
  = DefaultValue Number
```

##### Instances
``` purescript
Newtype DefaultValue _
```

#### `DeleteDestinationRequest`

``` purescript
newtype DeleteDestinationRequest
  = DeleteDestinationRequest { "DestinationName'" :: DestinationName }
```

##### Instances
``` purescript
Newtype DeleteDestinationRequest _
```

#### `DeleteLogGroupRequest`

``` purescript
newtype DeleteLogGroupRequest
  = DeleteLogGroupRequest { "LogGroupName'" :: LogGroupName }
```

##### Instances
``` purescript
Newtype DeleteLogGroupRequest _
```

#### `DeleteLogStreamRequest`

``` purescript
newtype DeleteLogStreamRequest
  = DeleteLogStreamRequest { "LogGroupName'" :: LogGroupName, "LogStreamName'" :: LogStreamName }
```

##### Instances
``` purescript
Newtype DeleteLogStreamRequest _
```

#### `DeleteMetricFilterRequest`

``` purescript
newtype DeleteMetricFilterRequest
  = DeleteMetricFilterRequest { "LogGroupName'" :: LogGroupName, "FilterName'" :: FilterName }
```

##### Instances
``` purescript
Newtype DeleteMetricFilterRequest _
```

#### `DeleteResourcePolicyRequest`

``` purescript
newtype DeleteResourcePolicyRequest
  = DeleteResourcePolicyRequest { "PolicyName'" :: NullOrUndefined (PolicyName) }
```

##### Instances
``` purescript
Newtype DeleteResourcePolicyRequest _
```

#### `DeleteRetentionPolicyRequest`

``` purescript
newtype DeleteRetentionPolicyRequest
  = DeleteRetentionPolicyRequest { "LogGroupName'" :: LogGroupName }
```

##### Instances
``` purescript
Newtype DeleteRetentionPolicyRequest _
```

#### `DeleteSubscriptionFilterRequest`

``` purescript
newtype DeleteSubscriptionFilterRequest
  = DeleteSubscriptionFilterRequest { "LogGroupName'" :: LogGroupName, "FilterName'" :: FilterName }
```

##### Instances
``` purescript
Newtype DeleteSubscriptionFilterRequest _
```

#### `Descending`

``` purescript
newtype Descending
  = Descending Boolean
```

##### Instances
``` purescript
Newtype Descending _
```

#### `DescribeDestinationsRequest`

``` purescript
newtype DescribeDestinationsRequest
  = DescribeDestinationsRequest { "DestinationNamePrefix" :: NullOrUndefined (DestinationName), "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (DescribeLimit) }
```

##### Instances
``` purescript
Newtype DescribeDestinationsRequest _
```

#### `DescribeDestinationsResponse`

``` purescript
newtype DescribeDestinationsResponse
  = DescribeDestinationsResponse { "Destinations'" :: NullOrUndefined (Destinations), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeDestinationsResponse _
```

#### `DescribeExportTasksRequest`

``` purescript
newtype DescribeExportTasksRequest
  = DescribeExportTasksRequest { "TaskId'" :: NullOrUndefined (ExportTaskId), "StatusCode'" :: NullOrUndefined (ExportTaskStatusCode), "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (DescribeLimit) }
```

##### Instances
``` purescript
Newtype DescribeExportTasksRequest _
```

#### `DescribeExportTasksResponse`

``` purescript
newtype DescribeExportTasksResponse
  = DescribeExportTasksResponse { "ExportTasks'" :: NullOrUndefined (ExportTasks), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeExportTasksResponse _
```

#### `DescribeLimit`

``` purescript
newtype DescribeLimit
  = DescribeLimit Int
```

##### Instances
``` purescript
Newtype DescribeLimit _
```

#### `DescribeLogGroupsRequest`

``` purescript
newtype DescribeLogGroupsRequest
  = DescribeLogGroupsRequest { "LogGroupNamePrefix'" :: NullOrUndefined (LogGroupName), "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (DescribeLimit) }
```

##### Instances
``` purescript
Newtype DescribeLogGroupsRequest _
```

#### `DescribeLogGroupsResponse`

``` purescript
newtype DescribeLogGroupsResponse
  = DescribeLogGroupsResponse { "LogGroups'" :: NullOrUndefined (LogGroups), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeLogGroupsResponse _
```

#### `DescribeLogStreamsRequest`

``` purescript
newtype DescribeLogStreamsRequest
  = DescribeLogStreamsRequest { "LogGroupName'" :: LogGroupName, "LogStreamNamePrefix'" :: NullOrUndefined (LogStreamName), "OrderBy'" :: NullOrUndefined (OrderBy), "Descending'" :: NullOrUndefined (Descending), "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (DescribeLimit) }
```

##### Instances
``` purescript
Newtype DescribeLogStreamsRequest _
```

#### `DescribeLogStreamsResponse`

``` purescript
newtype DescribeLogStreamsResponse
  = DescribeLogStreamsResponse { "LogStreams'" :: NullOrUndefined (LogStreams), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeLogStreamsResponse _
```

#### `DescribeMetricFiltersRequest`

``` purescript
newtype DescribeMetricFiltersRequest
  = DescribeMetricFiltersRequest { "LogGroupName'" :: NullOrUndefined (LogGroupName), "FilterNamePrefix'" :: NullOrUndefined (FilterName), "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (DescribeLimit), "MetricName'" :: NullOrUndefined (MetricName), "MetricNamespace'" :: NullOrUndefined (MetricNamespace) }
```

##### Instances
``` purescript
Newtype DescribeMetricFiltersRequest _
```

#### `DescribeMetricFiltersResponse`

``` purescript
newtype DescribeMetricFiltersResponse
  = DescribeMetricFiltersResponse { "MetricFilters'" :: NullOrUndefined (MetricFilters), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeMetricFiltersResponse _
```

#### `DescribeResourcePoliciesRequest`

``` purescript
newtype DescribeResourcePoliciesRequest
  = DescribeResourcePoliciesRequest { "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (DescribeLimit) }
```

##### Instances
``` purescript
Newtype DescribeResourcePoliciesRequest _
```

#### `DescribeResourcePoliciesResponse`

``` purescript
newtype DescribeResourcePoliciesResponse
  = DescribeResourcePoliciesResponse { "ResourcePolicies'" :: NullOrUndefined (ResourcePolicies), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeResourcePoliciesResponse _
```

#### `DescribeSubscriptionFiltersRequest`

``` purescript
newtype DescribeSubscriptionFiltersRequest
  = DescribeSubscriptionFiltersRequest { "LogGroupName'" :: LogGroupName, "FilterNamePrefix'" :: NullOrUndefined (FilterName), "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (DescribeLimit) }
```

##### Instances
``` purescript
Newtype DescribeSubscriptionFiltersRequest _
```

#### `DescribeSubscriptionFiltersResponse`

``` purescript
newtype DescribeSubscriptionFiltersResponse
  = DescribeSubscriptionFiltersResponse { "SubscriptionFilters'" :: NullOrUndefined (SubscriptionFilters), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeSubscriptionFiltersResponse _
```

#### `Destination`

``` purescript
newtype Destination
  = Destination { "DestinationName'" :: NullOrUndefined (DestinationName), "TargetArn'" :: NullOrUndefined (TargetArn), "RoleArn'" :: NullOrUndefined (RoleArn), "AccessPolicy'" :: NullOrUndefined (AccessPolicy), "Arn'" :: NullOrUndefined (Arn), "CreationTime'" :: NullOrUndefined (Number) }
```

<p>Represents a cross-account destination that receives subscription log events.</p>

##### Instances
``` purescript
Newtype Destination _
```

#### `DestinationArn`

``` purescript
newtype DestinationArn
  = DestinationArn String
```

##### Instances
``` purescript
Newtype DestinationArn _
```

#### `DestinationName`

``` purescript
newtype DestinationName
  = DestinationName String
```

##### Instances
``` purescript
Newtype DestinationName _
```

#### `Destinations`

``` purescript
newtype Destinations
  = Destinations (Array Destination)
```

##### Instances
``` purescript
Newtype Destinations _
```

#### `DisassociateKmsKeyRequest`

``` purescript
newtype DisassociateKmsKeyRequest
  = DisassociateKmsKeyRequest { "LogGroupName'" :: LogGroupName }
```

##### Instances
``` purescript
Newtype DisassociateKmsKeyRequest _
```

#### `Distribution`

``` purescript
newtype Distribution
  = Distribution String
```

<p>The method used to distribute log data to the destination, which can be either random or grouped by log stream.</p>

##### Instances
``` purescript
Newtype Distribution _
```

#### `EventId`

``` purescript
newtype EventId
  = EventId String
```

##### Instances
``` purescript
Newtype EventId _
```

#### `EventMessage`

``` purescript
newtype EventMessage
  = EventMessage String
```

##### Instances
``` purescript
Newtype EventMessage _
```

#### `EventNumber`

``` purescript
newtype EventNumber
  = EventNumber Number
```

##### Instances
``` purescript
Newtype EventNumber _
```

#### `EventsLimit`

``` purescript
newtype EventsLimit
  = EventsLimit Int
```

##### Instances
``` purescript
Newtype EventsLimit _
```

#### `ExportDestinationBucket`

``` purescript
newtype ExportDestinationBucket
  = ExportDestinationBucket String
```

##### Instances
``` purescript
Newtype ExportDestinationBucket _
```

#### `ExportDestinationPrefix`

``` purescript
newtype ExportDestinationPrefix
  = ExportDestinationPrefix String
```

##### Instances
``` purescript
Newtype ExportDestinationPrefix _
```

#### `ExportTask`

``` purescript
newtype ExportTask
  = ExportTask { "TaskId'" :: NullOrUndefined (ExportTaskId), "TaskName'" :: NullOrUndefined (ExportTaskName), "LogGroupName'" :: NullOrUndefined (LogGroupName), "From'" :: NullOrUndefined (Number), "To'" :: NullOrUndefined (Number), "Destination'" :: NullOrUndefined (ExportDestinationBucket), "DestinationPrefix'" :: NullOrUndefined (ExportDestinationPrefix), "Status'" :: NullOrUndefined (ExportTaskStatus), "ExecutionInfo'" :: NullOrUndefined (ExportTaskExecutionInfo) }
```

<p>Represents an export task.</p>

##### Instances
``` purescript
Newtype ExportTask _
```

#### `ExportTaskExecutionInfo`

``` purescript
newtype ExportTaskExecutionInfo
  = ExportTaskExecutionInfo { "CreationTime'" :: NullOrUndefined (Number), "CompletionTime'" :: NullOrUndefined (Number) }
```

<p>Represents the status of an export task.</p>

##### Instances
``` purescript
Newtype ExportTaskExecutionInfo _
```

#### `ExportTaskId`

``` purescript
newtype ExportTaskId
  = ExportTaskId String
```

##### Instances
``` purescript
Newtype ExportTaskId _
```

#### `ExportTaskName`

``` purescript
newtype ExportTaskName
  = ExportTaskName String
```

##### Instances
``` purescript
Newtype ExportTaskName _
```

#### `ExportTaskStatus`

``` purescript
newtype ExportTaskStatus
  = ExportTaskStatus { "Code'" :: NullOrUndefined (ExportTaskStatusCode), "Message'" :: NullOrUndefined (ExportTaskStatusMessage) }
```

<p>Represents the status of an export task.</p>

##### Instances
``` purescript
Newtype ExportTaskStatus _
```

#### `ExportTaskStatusCode`

``` purescript
newtype ExportTaskStatusCode
  = ExportTaskStatusCode String
```

##### Instances
``` purescript
Newtype ExportTaskStatusCode _
```

#### `ExportTaskStatusMessage`

``` purescript
newtype ExportTaskStatusMessage
  = ExportTaskStatusMessage String
```

##### Instances
``` purescript
Newtype ExportTaskStatusMessage _
```

#### `ExportTasks`

``` purescript
newtype ExportTasks
  = ExportTasks (Array ExportTask)
```

##### Instances
``` purescript
Newtype ExportTasks _
```

#### `ExtractedValues`

``` purescript
newtype ExtractedValues
  = ExtractedValues (Map Token Value)
```

##### Instances
``` purescript
Newtype ExtractedValues _
```

#### `FilterCount`

``` purescript
newtype FilterCount
  = FilterCount Int
```

##### Instances
``` purescript
Newtype FilterCount _
```

#### `FilterLogEventsRequest`

``` purescript
newtype FilterLogEventsRequest
  = FilterLogEventsRequest { "LogGroupName'" :: LogGroupName, "LogStreamNames'" :: NullOrUndefined (InputLogStreamNames), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "FilterPattern'" :: NullOrUndefined (FilterPattern), "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (EventsLimit), "Interleaved'" :: NullOrUndefined (Interleaved) }
```

##### Instances
``` purescript
Newtype FilterLogEventsRequest _
```

#### `FilterLogEventsResponse`

``` purescript
newtype FilterLogEventsResponse
  = FilterLogEventsResponse { "Events'" :: NullOrUndefined (FilteredLogEvents), "SearchedLogStreams'" :: NullOrUndefined (SearchedLogStreams), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype FilterLogEventsResponse _
```

#### `FilterName`

``` purescript
newtype FilterName
  = FilterName String
```

##### Instances
``` purescript
Newtype FilterName _
```

#### `FilterPattern`

``` purescript
newtype FilterPattern
  = FilterPattern String
```

<p>A symbolic description of how CloudWatch Logs should interpret the data in each log event. For example, a log event may contain time stamps, IP addresses, strings, and so on. You use the filter pattern to specify what to look for in the log event message.</p>

##### Instances
``` purescript
Newtype FilterPattern _
```

#### `FilteredLogEvent`

``` purescript
newtype FilteredLogEvent
  = FilteredLogEvent { "LogStreamName'" :: NullOrUndefined (LogStreamName), "Number" :: NullOrUndefined (Number), "Message'" :: NullOrUndefined (EventMessage), "IngestionTime'" :: NullOrUndefined (Number), "EventId'" :: NullOrUndefined (EventId) }
```

<p>Represents a matched event.</p>

##### Instances
``` purescript
Newtype FilteredLogEvent _
```

#### `FilteredLogEvents`

``` purescript
newtype FilteredLogEvents
  = FilteredLogEvents (Array FilteredLogEvent)
```

##### Instances
``` purescript
Newtype FilteredLogEvents _
```

#### `GetLogEventsRequest`

``` purescript
newtype GetLogEventsRequest
  = GetLogEventsRequest { "LogGroupName'" :: LogGroupName, "LogStreamName'" :: LogStreamName, "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "NextToken'" :: NullOrUndefined (NextToken), "Limit'" :: NullOrUndefined (EventsLimit), "StartFromHead'" :: NullOrUndefined (StartFromHead) }
```

##### Instances
``` purescript
Newtype GetLogEventsRequest _
```

#### `GetLogEventsResponse`

``` purescript
newtype GetLogEventsResponse
  = GetLogEventsResponse { "Events'" :: NullOrUndefined (OutputLogEvents), "NextForwardToken'" :: NullOrUndefined (NextToken), "NextBackwardToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetLogEventsResponse _
```

#### `InputLogEvent`

``` purescript
newtype InputLogEvent
  = InputLogEvent { "Number" :: Number, "Message'" :: EventMessage }
```

<p>Represents a log event, which is a record of activity that was recorded by the application or resource being monitored.</p>

##### Instances
``` purescript
Newtype InputLogEvent _
```

#### `InputLogEvents`

``` purescript
newtype InputLogEvents
  = InputLogEvents (Array InputLogEvent)
```

##### Instances
``` purescript
Newtype InputLogEvents _
```

#### `InputLogStreamNames`

``` purescript
newtype InputLogStreamNames
  = InputLogStreamNames (Array LogStreamName)
```

##### Instances
``` purescript
Newtype InputLogStreamNames _
```

#### `Interleaved`

``` purescript
newtype Interleaved
  = Interleaved Boolean
```

##### Instances
``` purescript
Newtype Interleaved _
```

#### `InvalidOperationException`

``` purescript
newtype InvalidOperationException
  = InvalidOperationException {  }
```

<p>The operation is not valid on the specified resource.</p>

##### Instances
``` purescript
Newtype InvalidOperationException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException {  }
```

<p>A parameter is specified incorrectly.</p>

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `InvalidSequenceTokenException`

``` purescript
newtype InvalidSequenceTokenException
  = InvalidSequenceTokenException { "ExpectedSequenceToken'" :: NullOrUndefined (SequenceToken) }
```

<p>The sequence token is not valid.</p>

##### Instances
``` purescript
Newtype InvalidSequenceTokenException _
```

#### `KmsKeyId`

``` purescript
newtype KmsKeyId
  = KmsKeyId String
```

##### Instances
``` purescript
Newtype KmsKeyId _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>You have reached the maximum number of resources that can be created.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListTagsLogGroupRequest`

``` purescript
newtype ListTagsLogGroupRequest
  = ListTagsLogGroupRequest { "LogGroupName'" :: LogGroupName }
```

##### Instances
``` purescript
Newtype ListTagsLogGroupRequest _
```

#### `ListTagsLogGroupResponse`

``` purescript
newtype ListTagsLogGroupResponse
  = ListTagsLogGroupResponse { "Tags'" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype ListTagsLogGroupResponse _
```

#### `LogEventIndex`

``` purescript
newtype LogEventIndex
  = LogEventIndex Int
```

##### Instances
``` purescript
Newtype LogEventIndex _
```

#### `LogGroup`

``` purescript
newtype LogGroup
  = LogGroup { "LogGroupName'" :: NullOrUndefined (LogGroupName), "CreationTime'" :: NullOrUndefined (Number), "RetentionInDays'" :: NullOrUndefined (Days), "MetricFilterCount'" :: NullOrUndefined (FilterCount), "Arn'" :: NullOrUndefined (Arn), "StoredBytes'" :: NullOrUndefined (StoredBytes), "KmsKeyId'" :: NullOrUndefined (KmsKeyId) }
```

<p>Represents a log group.</p>

##### Instances
``` purescript
Newtype LogGroup _
```

#### `LogGroupName`

``` purescript
newtype LogGroupName
  = LogGroupName String
```

##### Instances
``` purescript
Newtype LogGroupName _
```

#### `LogGroups`

``` purescript
newtype LogGroups
  = LogGroups (Array LogGroup)
```

##### Instances
``` purescript
Newtype LogGroups _
```

#### `LogStream`

``` purescript
newtype LogStream
  = LogStream { "LogStreamName'" :: NullOrUndefined (LogStreamName), "CreationTime'" :: NullOrUndefined (Number), "FirstEventTimestamp'" :: NullOrUndefined (Number), "LastEventTimestamp'" :: NullOrUndefined (Number), "LastIngestionTime'" :: NullOrUndefined (Number), "UploadSequenceToken'" :: NullOrUndefined (SequenceToken), "Arn'" :: NullOrUndefined (Arn), "StoredBytes'" :: NullOrUndefined (StoredBytes) }
```

<p>Represents a log stream, which is a sequence of log events from a single emitter of logs.</p>

##### Instances
``` purescript
Newtype LogStream _
```

#### `LogStreamName`

``` purescript
newtype LogStreamName
  = LogStreamName String
```

##### Instances
``` purescript
Newtype LogStreamName _
```

#### `LogStreamSearchedCompletely`

``` purescript
newtype LogStreamSearchedCompletely
  = LogStreamSearchedCompletely Boolean
```

##### Instances
``` purescript
Newtype LogStreamSearchedCompletely _
```

#### `LogStreams`

``` purescript
newtype LogStreams
  = LogStreams (Array LogStream)
```

##### Instances
``` purescript
Newtype LogStreams _
```

#### `MetricFilter`

``` purescript
newtype MetricFilter
  = MetricFilter { "FilterName'" :: NullOrUndefined (FilterName), "FilterPattern'" :: NullOrUndefined (FilterPattern), "MetricTransformations'" :: NullOrUndefined (MetricTransformations), "CreationTime'" :: NullOrUndefined (Number), "LogGroupName'" :: NullOrUndefined (LogGroupName) }
```

<p>Metric filters express how CloudWatch Logs would extract metric observations from ingested log events and transform them into metric data in a CloudWatch metric.</p>

##### Instances
``` purescript
Newtype MetricFilter _
```

#### `MetricFilterMatchRecord`

``` purescript
newtype MetricFilterMatchRecord
  = MetricFilterMatchRecord { "EventNumber'" :: NullOrUndefined (EventNumber), "EventMessage'" :: NullOrUndefined (EventMessage), "ExtractedValues'" :: NullOrUndefined (ExtractedValues) }
```

<p>Represents a matched event.</p>

##### Instances
``` purescript
Newtype MetricFilterMatchRecord _
```

#### `MetricFilterMatches`

``` purescript
newtype MetricFilterMatches
  = MetricFilterMatches (Array MetricFilterMatchRecord)
```

##### Instances
``` purescript
Newtype MetricFilterMatches _
```

#### `MetricFilters`

``` purescript
newtype MetricFilters
  = MetricFilters (Array MetricFilter)
```

##### Instances
``` purescript
Newtype MetricFilters _
```

#### `MetricName`

``` purescript
newtype MetricName
  = MetricName String
```

<p>The name of the CloudWatch metric to which the monitored log information should be published. For example, you may publish to a metric called ErrorCount.</p>

##### Instances
``` purescript
Newtype MetricName _
```

#### `MetricNamespace`

``` purescript
newtype MetricNamespace
  = MetricNamespace String
```

##### Instances
``` purescript
Newtype MetricNamespace _
```

#### `MetricTransformation`

``` purescript
newtype MetricTransformation
  = MetricTransformation { "MetricName'" :: MetricName, "MetricNamespace'" :: MetricNamespace, "MetricValue'" :: MetricValue, "DefaultValue'" :: NullOrUndefined (DefaultValue) }
```

<p>Indicates how to transform ingested log events in to metric data in a CloudWatch metric.</p>

##### Instances
``` purescript
Newtype MetricTransformation _
```

#### `MetricTransformations`

``` purescript
newtype MetricTransformations
  = MetricTransformations (Array MetricTransformation)
```

##### Instances
``` purescript
Newtype MetricTransformations _
```

#### `MetricValue`

``` purescript
newtype MetricValue
  = MetricValue String
```

<p>The value to publish to the CloudWatch metric. For example, if you're counting the occurrences of a term like "Error", the value is "1" for each occurrence. If you're counting the bytes transferred, the value is the value in the log event.</p>

##### Instances
``` purescript
Newtype MetricValue _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

<p>The token for the next set of items to return. The token expires after 24 hours.</p>

##### Instances
``` purescript
Newtype NextToken _
```

#### `OperationAbortedException`

``` purescript
newtype OperationAbortedException
  = OperationAbortedException {  }
```

<p>Multiple requests to update the same resource were in conflict.</p>

##### Instances
``` purescript
Newtype OperationAbortedException _
```

#### `OrderBy`

``` purescript
newtype OrderBy
  = OrderBy String
```

##### Instances
``` purescript
Newtype OrderBy _
```

#### `OutputLogEvent`

``` purescript
newtype OutputLogEvent
  = OutputLogEvent { "Number" :: NullOrUndefined (Number), "Message'" :: NullOrUndefined (EventMessage), "IngestionTime'" :: NullOrUndefined (Number) }
```

<p>Represents a log event.</p>

##### Instances
``` purescript
Newtype OutputLogEvent _
```

#### `OutputLogEvents`

``` purescript
newtype OutputLogEvents
  = OutputLogEvents (Array OutputLogEvent)
```

##### Instances
``` purescript
Newtype OutputLogEvents _
```

#### `PolicyDocument`

``` purescript
newtype PolicyDocument
  = PolicyDocument String
```

##### Instances
``` purescript
Newtype PolicyDocument _
```

#### `PolicyName`

``` purescript
newtype PolicyName
  = PolicyName String
```

##### Instances
``` purescript
Newtype PolicyName _
```

#### `PutDestinationPolicyRequest`

``` purescript
newtype PutDestinationPolicyRequest
  = PutDestinationPolicyRequest { "DestinationName'" :: DestinationName, "AccessPolicy'" :: AccessPolicy }
```

##### Instances
``` purescript
Newtype PutDestinationPolicyRequest _
```

#### `PutDestinationRequest`

``` purescript
newtype PutDestinationRequest
  = PutDestinationRequest { "DestinationName'" :: DestinationName, "TargetArn'" :: TargetArn, "RoleArn'" :: RoleArn }
```

##### Instances
``` purescript
Newtype PutDestinationRequest _
```

#### `PutDestinationResponse`

``` purescript
newtype PutDestinationResponse
  = PutDestinationResponse { "Destination'" :: NullOrUndefined (Destination) }
```

##### Instances
``` purescript
Newtype PutDestinationResponse _
```

#### `PutLogEventsRequest`

``` purescript
newtype PutLogEventsRequest
  = PutLogEventsRequest { "LogGroupName'" :: LogGroupName, "LogStreamName'" :: LogStreamName, "LogEvents'" :: InputLogEvents, "SequenceToken'" :: NullOrUndefined (SequenceToken) }
```

##### Instances
``` purescript
Newtype PutLogEventsRequest _
```

#### `PutLogEventsResponse`

``` purescript
newtype PutLogEventsResponse
  = PutLogEventsResponse { "NextSequenceToken'" :: NullOrUndefined (SequenceToken), "RejectedLogEventsInfo'" :: NullOrUndefined (RejectedLogEventsInfo) }
```

##### Instances
``` purescript
Newtype PutLogEventsResponse _
```

#### `PutMetricFilterRequest`

``` purescript
newtype PutMetricFilterRequest
  = PutMetricFilterRequest { "LogGroupName'" :: LogGroupName, "FilterName'" :: FilterName, "FilterPattern'" :: FilterPattern, "MetricTransformations'" :: MetricTransformations }
```

##### Instances
``` purescript
Newtype PutMetricFilterRequest _
```

#### `PutResourcePolicyRequest`

``` purescript
newtype PutResourcePolicyRequest
  = PutResourcePolicyRequest { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyDocument'" :: NullOrUndefined (PolicyDocument) }
```

##### Instances
``` purescript
Newtype PutResourcePolicyRequest _
```

#### `PutResourcePolicyResponse`

``` purescript
newtype PutResourcePolicyResponse
  = PutResourcePolicyResponse { "ResourcePolicy'" :: NullOrUndefined (ResourcePolicy) }
```

##### Instances
``` purescript
Newtype PutResourcePolicyResponse _
```

#### `PutRetentionPolicyRequest`

``` purescript
newtype PutRetentionPolicyRequest
  = PutRetentionPolicyRequest { "LogGroupName'" :: LogGroupName, "RetentionInDays'" :: Days }
```

##### Instances
``` purescript
Newtype PutRetentionPolicyRequest _
```

#### `PutSubscriptionFilterRequest`

``` purescript
newtype PutSubscriptionFilterRequest
  = PutSubscriptionFilterRequest { "LogGroupName'" :: LogGroupName, "FilterName'" :: FilterName, "FilterPattern'" :: FilterPattern, "DestinationArn'" :: DestinationArn, "RoleArn'" :: NullOrUndefined (RoleArn), "Distribution'" :: NullOrUndefined (Distribution) }
```

##### Instances
``` purescript
Newtype PutSubscriptionFilterRequest _
```

#### `RejectedLogEventsInfo`

``` purescript
newtype RejectedLogEventsInfo
  = RejectedLogEventsInfo { "TooNewLogEventStartIndex'" :: NullOrUndefined (LogEventIndex), "TooOldLogEventEndIndex'" :: NullOrUndefined (LogEventIndex), "ExpiredLogEventEndIndex'" :: NullOrUndefined (LogEventIndex) }
```

<p>Represents the rejected events.</p>

##### Instances
``` purescript
Newtype RejectedLogEventsInfo _
```

#### `ResourceAlreadyExistsException`

``` purescript
newtype ResourceAlreadyExistsException
  = ResourceAlreadyExistsException {  }
```

<p>The specified resource already exists.</p>

##### Instances
``` purescript
Newtype ResourceAlreadyExistsException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>The specified resource does not exist.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ResourcePolicies`

``` purescript
newtype ResourcePolicies
  = ResourcePolicies (Array ResourcePolicy)
```

##### Instances
``` purescript
Newtype ResourcePolicies _
```

#### `ResourcePolicy`

``` purescript
newtype ResourcePolicy
  = ResourcePolicy { "PolicyName'" :: NullOrUndefined (PolicyName), "PolicyDocument'" :: NullOrUndefined (PolicyDocument), "LastUpdatedTime'" :: NullOrUndefined (Number) }
```

<p>A policy enabling one or more entities to put logs to a log group in this account.</p>

##### Instances
``` purescript
Newtype ResourcePolicy _
```

#### `RoleArn`

``` purescript
newtype RoleArn
  = RoleArn String
```

##### Instances
``` purescript
Newtype RoleArn _
```

#### `SearchedLogStream`

``` purescript
newtype SearchedLogStream
  = SearchedLogStream { "LogStreamName'" :: NullOrUndefined (LogStreamName), "SearchedCompletely'" :: NullOrUndefined (LogStreamSearchedCompletely) }
```

<p>Represents the search status of a log stream.</p>

##### Instances
``` purescript
Newtype SearchedLogStream _
```

#### `SearchedLogStreams`

``` purescript
newtype SearchedLogStreams
  = SearchedLogStreams (Array SearchedLogStream)
```

##### Instances
``` purescript
Newtype SearchedLogStreams _
```

#### `SequenceToken`

``` purescript
newtype SequenceToken
  = SequenceToken String
```

##### Instances
``` purescript
Newtype SequenceToken _
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException {  }
```

<p>The service cannot complete the request.</p>

##### Instances
``` purescript
Newtype ServiceUnavailableException _
```

#### `StartFromHead`

``` purescript
newtype StartFromHead
  = StartFromHead Boolean
```

##### Instances
``` purescript
Newtype StartFromHead _
```

#### `StoredBytes`

``` purescript
newtype StoredBytes
  = StoredBytes Number
```

##### Instances
``` purescript
Newtype StoredBytes _
```

#### `SubscriptionFilter`

``` purescript
newtype SubscriptionFilter
  = SubscriptionFilter { "FilterName'" :: NullOrUndefined (FilterName), "LogGroupName'" :: NullOrUndefined (LogGroupName), "FilterPattern'" :: NullOrUndefined (FilterPattern), "DestinationArn'" :: NullOrUndefined (DestinationArn), "RoleArn'" :: NullOrUndefined (RoleArn), "Distribution'" :: NullOrUndefined (Distribution), "CreationTime'" :: NullOrUndefined (Number) }
```

<p>Represents a subscription filter.</p>

##### Instances
``` purescript
Newtype SubscriptionFilter _
```

#### `SubscriptionFilters`

``` purescript
newtype SubscriptionFilters
  = SubscriptionFilters (Array SubscriptionFilter)
```

##### Instances
``` purescript
Newtype SubscriptionFilters _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TagLogGroupRequest`

``` purescript
newtype TagLogGroupRequest
  = TagLogGroupRequest { "LogGroupName'" :: LogGroupName, "Tags'" :: Tags }
```

##### Instances
``` purescript
Newtype TagLogGroupRequest _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Map TagKey TagValue)
```

##### Instances
``` purescript
Newtype Tags _
```

#### `TargetArn`

``` purescript
newtype TargetArn
  = TargetArn String
```

##### Instances
``` purescript
Newtype TargetArn _
```

#### `TestEventMessages`

``` purescript
newtype TestEventMessages
  = TestEventMessages (Array EventMessage)
```

##### Instances
``` purescript
Newtype TestEventMessages _
```

#### `TestMetricFilterRequest`

``` purescript
newtype TestMetricFilterRequest
  = TestMetricFilterRequest { "FilterPattern'" :: FilterPattern, "LogEventMessages'" :: TestEventMessages }
```

##### Instances
``` purescript
Newtype TestMetricFilterRequest _
```

#### `TestMetricFilterResponse`

``` purescript
newtype TestMetricFilterResponse
  = TestMetricFilterResponse { "Matches'" :: NullOrUndefined (MetricFilterMatches) }
```

##### Instances
``` purescript
Newtype TestMetricFilterResponse _
```

#### `Token`

``` purescript
newtype Token
  = Token String
```

##### Instances
``` purescript
Newtype Token _
```

#### `UntagLogGroupRequest`

``` purescript
newtype UntagLogGroupRequest
  = UntagLogGroupRequest { "LogGroupName'" :: LogGroupName, "Tags'" :: TagList }
```

##### Instances
``` purescript
Newtype UntagLogGroupRequest _
```

#### `Value`

``` purescript
newtype Value
  = Value String
```

##### Instances
``` purescript
Newtype Value _
```


