## Module AWS.CloudWatchEvents

<p>Amazon CloudWatch Events helps you to respond to state changes in your AWS resources. When your resources change state, they automatically send events into an event stream. You can create rules that match selected events in the stream and route them to targets to take action. You can also use rules to take action on a pre-determined schedule. For example, you can configure rules to:</p> <ul> <li> <p>Automatically invoke an AWS Lambda function to update DNS entries when an event notifies you that Amazon EC2 instance enters the running state.</p> </li> <li> <p>Direct specific API records from CloudTrail to an Amazon Kinesis stream for detailed analysis of potential security or availability risks.</p> </li> <li> <p>Periodically invoke a built-in target to create a snapshot of an Amazon EBS volume.</p> </li> </ul> <p>For more information about the features of Amazon CloudWatch Events, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events">Amazon CloudWatch Events User Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `deleteRule`

``` purescript
deleteRule :: forall eff. DeleteRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified rule.</p> <p>You must remove all targets from a rule using <a>RemoveTargets</a> before you can delete the rule.</p> <p>When you delete a rule, incoming events might continue to match to the deleted rule. Please allow a short period of time for changes to take effect.</p>

#### `describeEventBus`

``` purescript
describeEventBus :: forall eff. DescribeEventBusRequest -> Aff (err :: RequestError | eff) DescribeEventBusResponse
```

<p>Displays the external AWS accounts that are permitted to write events to your account using your account's event bus, and the associated policy. To enable your account to receive events from other accounts, use <a>PutPermission</a>.</p>

#### `describeRule`

``` purescript
describeRule :: forall eff. DescribeRuleRequest -> Aff (err :: RequestError | eff) DescribeRuleResponse
```

<p>Describes the specified rule.</p>

#### `disableRule`

``` purescript
disableRule :: forall eff. DisableRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Disables the specified rule. A disabled rule won't match any events, and won't self-trigger if it has a schedule expression.</p> <p>When you disable a rule, incoming events might continue to match to the disabled rule. Please allow a short period of time for changes to take effect.</p>

#### `enableRule`

``` purescript
enableRule :: forall eff. EnableRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Enables the specified rule. If the rule does not exist, the operation fails.</p> <p>When you enable a rule, incoming events might not immediately start matching to a newly enabled rule. Please allow a short period of time for changes to take effect.</p>

#### `listRuleNamesByTarget`

``` purescript
listRuleNamesByTarget :: forall eff. ListRuleNamesByTargetRequest -> Aff (err :: RequestError | eff) ListRuleNamesByTargetResponse
```

<p>Lists the rules for the specified target. You can see which of the rules in Amazon CloudWatch Events can invoke a specific target in your account.</p>

#### `listRules`

``` purescript
listRules :: forall eff. ListRulesRequest -> Aff (err :: RequestError | eff) ListRulesResponse
```

<p>Lists your Amazon CloudWatch Events rules. You can either list all the rules or you can provide a prefix to match to the rule names.</p>

#### `listTargetsByRule`

``` purescript
listTargetsByRule :: forall eff. ListTargetsByRuleRequest -> Aff (err :: RequestError | eff) ListTargetsByRuleResponse
```

<p>Lists the targets assigned to the specified rule.</p>

#### `putEvents`

``` purescript
putEvents :: forall eff. PutEventsRequest -> Aff (err :: RequestError | eff) PutEventsResponse
```

<p>Sends custom events to Amazon CloudWatch Events so that they can be matched to rules.</p>

#### `putPermission`

``` purescript
putPermission :: forall eff. PutPermissionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Running <code>PutPermission</code> permits the specified AWS account to put events to your account's default <i>event bus</i>. CloudWatch Events rules in your account are triggered by these events arriving to your default event bus. </p> <p>For another account to send events to your account, that external account must have a CloudWatch Events rule with your account's default event bus as a target.</p> <p>To enable multiple AWS accounts to put events to your default event bus, run <code>PutPermission</code> once for each of these accounts.</p> <p>The permission policy on the default event bus cannot exceed 10KB in size.</p>

#### `putRule`

``` purescript
putRule :: forall eff. PutRuleRequest -> Aff (err :: RequestError | eff) PutRuleResponse
```

<p>Creates or updates the specified rule. Rules are enabled by default, or based on value of the state. You can disable a rule using <a>DisableRule</a>.</p> <p>When you create or update a rule, incoming events might not immediately start matching to new or updated rules. Please allow a short period of time for changes to take effect.</p> <p>A rule must contain at least an EventPattern or ScheduleExpression. Rules with EventPatterns are triggered when a matching event is observed. Rules with ScheduleExpressions self-trigger based on the given schedule. A rule can have both an EventPattern and a ScheduleExpression, in which case the rule triggers on matching events as well as on a schedule.</p> <p>Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, CloudWatch Events uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.</p>

#### `putTargets`

``` purescript
putTargets :: forall eff. PutTargetsRequest -> Aff (err :: RequestError | eff) PutTargetsResponse
```

<p>Adds the specified targets to the specified rule, or updates the targets if they are already associated with the rule.</p> <p>Targets are the resources that are invoked when a rule is triggered.</p> <p>You can configure the following as targets for CloudWatch Events:</p> <ul> <li> <p>EC2 instances</p> </li> <li> <p>AWS Lambda functions</p> </li> <li> <p>Streams in Amazon Kinesis Streams</p> </li> <li> <p>Delivery streams in Amazon Kinesis Firehose</p> </li> <li> <p>Amazon ECS tasks</p> </li> <li> <p>AWS Step Functions state machines</p> </li> <li> <p>Pipelines in Amazon Code Pipeline</p> </li> <li> <p>Amazon Inspector assessment templates</p> </li> <li> <p>Amazon SNS topics</p> </li> <li> <p>Amazon SQS queues</p> </li> <li> <p>The default event bus of another AWS account</p> </li> </ul> <p>Note that creating rules with built-in targets is supported only in the AWS Management Console.</p> <p>For some target types, <code>PutTargets</code> provides target-specific parameters. If the target is an Amazon Kinesis stream, you can optionally specify which shard the event goes to by using the <code>KinesisParameters</code> argument. To invoke a command on multiple EC2 instances with one rule, you can use the <code>RunCommandParameters</code> field.</p> <p>To be able to make API calls against the resources that you own, Amazon CloudWatch Events needs the appropriate permissions. For AWS Lambda and Amazon SNS resources, CloudWatch Events relies on resource-based policies. For EC2 instances, Amazon Kinesis streams, and AWS Step Functions state machines, CloudWatch Events relies on IAM roles that you specify in the <code>RoleARN</code> argument in <code>PutTargets</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/auth-and-access-control-cwe.html">Authentication and Access Control</a> in the <i>Amazon CloudWatch Events User Guide</i>.</p> <p>If another AWS account is in the same region and has granted you permission (using <code>PutPermission</code>), you can send events to that account by setting that account's event bus as a target of the rules in your account. To send the matched events to the other account, specify that account's event bus as the <code>Arn</code> when you run <code>PutTargets</code>. If your account sends events to another account, your account is charged for each sent event. Each event sent to antoher account is charged as a custom event. The account receiving the event is not charged. For more information on pricing, see <a href="https://aws.amazon.com/cloudwatch/pricing/">Amazon CloudWatch Pricing</a>.</p> <p>For more information about enabling cross-account events, see <a>PutPermission</a>.</p> <p> <b>Input</b>, <b>InputPath</b> and <b>InputTransformer</b> are mutually exclusive and optional parameters of a target. When a rule is triggered due to a matched event:</p> <ul> <li> <p>If none of the following arguments are specified for a target, then the entire event is passed to the target in JSON form (unless the target is Amazon EC2 Run Command or Amazon ECS task, in which case nothing from the event is passed to the target).</p> </li> <li> <p>If <b>Input</b> is specified in the form of valid JSON, then the matched event is overridden with this constant.</p> </li> <li> <p>If <b>InputPath</b> is specified in the form of JSONPath (for example, <code>$.detail</code>), then only the part of the event specified in the path is passed to the target (for example, only the detail part of the event is passed).</p> </li> <li> <p>If <b>InputTransformer</b> is specified, then one or more specified JSONPaths are extracted from the event and used as values in a template that you specify as the input to the target.</p> </li> </ul> <p>When you specify <code>Input</code>, <code>InputPath</code>, or <code>InputTransformer</code>, you must use JSON dot notation, not bracket notation.</p> <p>When you add targets to a rule and the associated rule triggers soon after, new or updated targets might not be immediately invoked. Please allow a short period of time for changes to take effect.</p> <p>This action can partially fail if too many requests are made at the same time. If that happens, <code>FailedEntryCount</code> is non-zero in the response and each entry in <code>FailedEntries</code> provides the ID of the failed target and the error code.</p>

#### `removePermission`

``` purescript
removePermission :: forall eff. RemovePermissionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Revokes the permission of another AWS account to be able to put events to your default event bus. Specify the account to revoke by the <code>StatementId</code> value that you associated with the account when you granted it permission with <code>PutPermission</code>. You can find the <code>StatementId</code> by using <a>DescribeEventBus</a>.</p>

#### `removeTargets`

``` purescript
removeTargets :: forall eff. RemoveTargetsRequest -> Aff (err :: RequestError | eff) RemoveTargetsResponse
```

<p>Removes the specified targets from the specified rule. When the rule is triggered, those targets are no longer be invoked.</p> <p>When you remove a target, when the associated rule triggers, removed targets might continue to be invoked. Please allow a short period of time for changes to take effect.</p> <p>This action can partially fail if too many requests are made at the same time. If that happens, <code>FailedEntryCount</code> is non-zero in the response and each entry in <code>FailedEntries</code> provides the ID of the failed target and the error code.</p>

#### `testEventPattern`

``` purescript
testEventPattern :: forall eff. TestEventPatternRequest -> Aff (err :: RequestError | eff) TestEventPatternResponse
```

<p>Tests whether the specified event pattern matches the provided event.</p> <p>Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, CloudWatch Events uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.</p>

#### `Action`

``` purescript
newtype Action
  = Action String
```

##### Instances
``` purescript
Newtype Action _
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

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException {  }
```

<p>There is concurrent modification on a rule or target.</p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `DeleteRuleRequest`

``` purescript
newtype DeleteRuleRequest
  = DeleteRuleRequest { "Name" :: RuleName }
```

##### Instances
``` purescript
Newtype DeleteRuleRequest _
```

#### `DescribeEventBusRequest`

``` purescript
newtype DescribeEventBusRequest
  = DescribeEventBusRequest {  }
```

##### Instances
``` purescript
Newtype DescribeEventBusRequest _
```

#### `DescribeEventBusResponse`

``` purescript
newtype DescribeEventBusResponse
  = DescribeEventBusResponse { "Name" :: NullOrUndefined (String), "Arn" :: NullOrUndefined (String), "Policy" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeEventBusResponse _
```

#### `DescribeRuleRequest`

``` purescript
newtype DescribeRuleRequest
  = DescribeRuleRequest { "Name" :: RuleName }
```

##### Instances
``` purescript
Newtype DescribeRuleRequest _
```

#### `DescribeRuleResponse`

``` purescript
newtype DescribeRuleResponse
  = DescribeRuleResponse { "Name" :: NullOrUndefined (RuleName), "Arn" :: NullOrUndefined (RuleArn), "EventPattern" :: NullOrUndefined (EventPattern), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "State" :: NullOrUndefined (RuleState), "Description" :: NullOrUndefined (RuleDescription), "RoleArn" :: NullOrUndefined (RoleArn) }
```

##### Instances
``` purescript
Newtype DescribeRuleResponse _
```

#### `DisableRuleRequest`

``` purescript
newtype DisableRuleRequest
  = DisableRuleRequest { "Name" :: RuleName }
```

##### Instances
``` purescript
Newtype DisableRuleRequest _
```

#### `EcsParameters`

``` purescript
newtype EcsParameters
  = EcsParameters { "TaskDefinitionArn" :: Arn, "TaskCount" :: NullOrUndefined (LimitMin1) }
```

<p>The custom parameters to be used when the target is an Amazon ECS cluster.</p>

##### Instances
``` purescript
Newtype EcsParameters _
```

#### `EnableRuleRequest`

``` purescript
newtype EnableRuleRequest
  = EnableRuleRequest { "Name" :: RuleName }
```

##### Instances
``` purescript
Newtype EnableRuleRequest _
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

##### Instances
``` purescript
Newtype ErrorCode _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
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

#### `EventPattern`

``` purescript
newtype EventPattern
  = EventPattern String
```

##### Instances
``` purescript
Newtype EventPattern _
```

#### `EventResource`

``` purescript
newtype EventResource
  = EventResource String
```

##### Instances
``` purescript
Newtype EventResource _
```

#### `EventResourceList`

``` purescript
newtype EventResourceList
  = EventResourceList (Array EventResource)
```

##### Instances
``` purescript
Newtype EventResourceList _
```

#### `EventTime`

``` purescript
newtype EventTime
  = EventTime Number
```

##### Instances
``` purescript
Newtype EventTime _
```

#### `InputTransformer`

``` purescript
newtype InputTransformer
  = InputTransformer { "InputPathsMap" :: NullOrUndefined (TransformerPaths), "InputTemplate" :: TransformerInput }
```

<p>Contains the parameters needed for you to provide custom input to a target based on one or more pieces of data extracted from the event.</p>

##### Instances
``` purescript
Newtype InputTransformer _
```

#### `InputTransformerPathKey`

``` purescript
newtype InputTransformerPathKey
  = InputTransformerPathKey String
```

##### Instances
``` purescript
Newtype InputTransformerPathKey _
```

#### `InternalException`

``` purescript
newtype InternalException
  = InternalException {  }
```

<p>This exception occurs due to unexpected causes.</p>

##### Instances
``` purescript
Newtype InternalException _
```

#### `InvalidEventPatternException`

``` purescript
newtype InvalidEventPatternException
  = InvalidEventPatternException {  }
```

<p>The event pattern is not valid.</p>

##### Instances
``` purescript
Newtype InvalidEventPatternException _
```

#### `KinesisParameters`

``` purescript
newtype KinesisParameters
  = KinesisParameters { "PartitionKeyPath" :: TargetPartitionKeyPath }
```

<p>This object enables you to specify a JSON path to extract from the event and use as the partition key for the Amazon Kinesis stream, so that you can control the shard to which the event goes. If you do not include this parameter, the default is to use the <code>eventId</code> as the partition key.</p>

##### Instances
``` purescript
Newtype KinesisParameters _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>You tried to create more rules or add more targets to a rule than is allowed.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `LimitMax100`

``` purescript
newtype LimitMax100
  = LimitMax100 Int
```

##### Instances
``` purescript
Newtype LimitMax100 _
```

#### `LimitMin1`

``` purescript
newtype LimitMin1
  = LimitMin1 Int
```

##### Instances
``` purescript
Newtype LimitMin1 _
```

#### `ListRuleNamesByTargetRequest`

``` purescript
newtype ListRuleNamesByTargetRequest
  = ListRuleNamesByTargetRequest { "TargetArn" :: TargetArn, "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (LimitMax100) }
```

##### Instances
``` purescript
Newtype ListRuleNamesByTargetRequest _
```

#### `ListRuleNamesByTargetResponse`

``` purescript
newtype ListRuleNamesByTargetResponse
  = ListRuleNamesByTargetResponse { "RuleNames" :: NullOrUndefined (RuleNameList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListRuleNamesByTargetResponse _
```

#### `ListRulesRequest`

``` purescript
newtype ListRulesRequest
  = ListRulesRequest { "NamePrefix" :: NullOrUndefined (RuleName), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (LimitMax100) }
```

##### Instances
``` purescript
Newtype ListRulesRequest _
```

#### `ListRulesResponse`

``` purescript
newtype ListRulesResponse
  = ListRulesResponse { "Rules" :: NullOrUndefined (RuleResponseList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListRulesResponse _
```

#### `ListTargetsByRuleRequest`

``` purescript
newtype ListTargetsByRuleRequest
  = ListTargetsByRuleRequest { "Rule" :: RuleName, "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (LimitMax100) }
```

##### Instances
``` purescript
Newtype ListTargetsByRuleRequest _
```

#### `ListTargetsByRuleResponse`

``` purescript
newtype ListTargetsByRuleResponse
  = ListTargetsByRuleResponse { "Targets" :: NullOrUndefined (TargetList), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTargetsByRuleResponse _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `PolicyLengthExceededException`

``` purescript
newtype PolicyLengthExceededException
  = PolicyLengthExceededException {  }
```

<p>The event bus policy is too long. For more information, see the limits.</p>

##### Instances
``` purescript
Newtype PolicyLengthExceededException _
```

#### `Principal`

``` purescript
newtype Principal
  = Principal String
```

##### Instances
``` purescript
Newtype Principal _
```

#### `PutEventsRequest`

``` purescript
newtype PutEventsRequest
  = PutEventsRequest { "Entries" :: PutEventsRequestEntryList }
```

##### Instances
``` purescript
Newtype PutEventsRequest _
```

#### `PutEventsRequestEntry`

``` purescript
newtype PutEventsRequestEntry
  = PutEventsRequestEntry { "Time" :: NullOrUndefined (EventTime), "Source" :: NullOrUndefined (String), "Resources" :: NullOrUndefined (EventResourceList), "DetailType" :: NullOrUndefined (String), "Detail" :: NullOrUndefined (String) }
```

<p>Represents an event to be submitted.</p>

##### Instances
``` purescript
Newtype PutEventsRequestEntry _
```

#### `PutEventsRequestEntryList`

``` purescript
newtype PutEventsRequestEntryList
  = PutEventsRequestEntryList (Array PutEventsRequestEntry)
```

##### Instances
``` purescript
Newtype PutEventsRequestEntryList _
```

#### `PutEventsResponse`

``` purescript
newtype PutEventsResponse
  = PutEventsResponse { "FailedEntryCount" :: NullOrUndefined (Int), "Entries" :: NullOrUndefined (PutEventsResultEntryList) }
```

##### Instances
``` purescript
Newtype PutEventsResponse _
```

#### `PutEventsResultEntry`

``` purescript
newtype PutEventsResultEntry
  = PutEventsResultEntry { "EventId" :: NullOrUndefined (EventId), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Represents an event that failed to be submitted.</p>

##### Instances
``` purescript
Newtype PutEventsResultEntry _
```

#### `PutEventsResultEntryList`

``` purescript
newtype PutEventsResultEntryList
  = PutEventsResultEntryList (Array PutEventsResultEntry)
```

##### Instances
``` purescript
Newtype PutEventsResultEntryList _
```

#### `PutPermissionRequest`

``` purescript
newtype PutPermissionRequest
  = PutPermissionRequest { "Action" :: Action, "Principal" :: Principal, "StatementId" :: StatementId }
```

##### Instances
``` purescript
Newtype PutPermissionRequest _
```

#### `PutRuleRequest`

``` purescript
newtype PutRuleRequest
  = PutRuleRequest { "Name" :: RuleName, "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "EventPattern" :: NullOrUndefined (EventPattern), "State" :: NullOrUndefined (RuleState), "Description" :: NullOrUndefined (RuleDescription), "RoleArn" :: NullOrUndefined (RoleArn) }
```

##### Instances
``` purescript
Newtype PutRuleRequest _
```

#### `PutRuleResponse`

``` purescript
newtype PutRuleResponse
  = PutRuleResponse { "RuleArn" :: NullOrUndefined (RuleArn) }
```

##### Instances
``` purescript
Newtype PutRuleResponse _
```

#### `PutTargetsRequest`

``` purescript
newtype PutTargetsRequest
  = PutTargetsRequest { "Rule" :: RuleName, "Targets" :: TargetList }
```

##### Instances
``` purescript
Newtype PutTargetsRequest _
```

#### `PutTargetsResponse`

``` purescript
newtype PutTargetsResponse
  = PutTargetsResponse { "FailedEntryCount" :: NullOrUndefined (Int), "FailedEntries" :: NullOrUndefined (PutTargetsResultEntryList) }
```

##### Instances
``` purescript
Newtype PutTargetsResponse _
```

#### `PutTargetsResultEntry`

``` purescript
newtype PutTargetsResultEntry
  = PutTargetsResultEntry { "TargetId" :: NullOrUndefined (TargetId), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Represents a target that failed to be added to a rule.</p>

##### Instances
``` purescript
Newtype PutTargetsResultEntry _
```

#### `PutTargetsResultEntryList`

``` purescript
newtype PutTargetsResultEntryList
  = PutTargetsResultEntryList (Array PutTargetsResultEntry)
```

##### Instances
``` purescript
Newtype PutTargetsResultEntryList _
```

#### `RemovePermissionRequest`

``` purescript
newtype RemovePermissionRequest
  = RemovePermissionRequest { "StatementId" :: StatementId }
```

##### Instances
``` purescript
Newtype RemovePermissionRequest _
```

#### `RemoveTargetsRequest`

``` purescript
newtype RemoveTargetsRequest
  = RemoveTargetsRequest { "Rule" :: RuleName, "Ids" :: TargetIdList }
```

##### Instances
``` purescript
Newtype RemoveTargetsRequest _
```

#### `RemoveTargetsResponse`

``` purescript
newtype RemoveTargetsResponse
  = RemoveTargetsResponse { "FailedEntryCount" :: NullOrUndefined (Int), "FailedEntries" :: NullOrUndefined (RemoveTargetsResultEntryList) }
```

##### Instances
``` purescript
Newtype RemoveTargetsResponse _
```

#### `RemoveTargetsResultEntry`

``` purescript
newtype RemoveTargetsResultEntry
  = RemoveTargetsResultEntry { "TargetId" :: NullOrUndefined (TargetId), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Represents a target that failed to be removed from a rule.</p>

##### Instances
``` purescript
Newtype RemoveTargetsResultEntry _
```

#### `RemoveTargetsResultEntryList`

``` purescript
newtype RemoveTargetsResultEntryList
  = RemoveTargetsResultEntryList (Array RemoveTargetsResultEntry)
```

##### Instances
``` purescript
Newtype RemoveTargetsResultEntryList _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>An entity that you specified does not exist.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
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

#### `Rule`

``` purescript
newtype Rule
  = Rule { "Name" :: NullOrUndefined (RuleName), "Arn" :: NullOrUndefined (RuleArn), "EventPattern" :: NullOrUndefined (EventPattern), "State" :: NullOrUndefined (RuleState), "Description" :: NullOrUndefined (RuleDescription), "ScheduleExpression" :: NullOrUndefined (ScheduleExpression), "RoleArn" :: NullOrUndefined (RoleArn) }
```

<p>Contains information about a rule in Amazon CloudWatch Events.</p>

##### Instances
``` purescript
Newtype Rule _
```

#### `RuleArn`

``` purescript
newtype RuleArn
  = RuleArn String
```

##### Instances
``` purescript
Newtype RuleArn _
```

#### `RuleDescription`

``` purescript
newtype RuleDescription
  = RuleDescription String
```

##### Instances
``` purescript
Newtype RuleDescription _
```

#### `RuleName`

``` purescript
newtype RuleName
  = RuleName String
```

##### Instances
``` purescript
Newtype RuleName _
```

#### `RuleNameList`

``` purescript
newtype RuleNameList
  = RuleNameList (Array RuleName)
```

##### Instances
``` purescript
Newtype RuleNameList _
```

#### `RuleResponseList`

``` purescript
newtype RuleResponseList
  = RuleResponseList (Array Rule)
```

##### Instances
``` purescript
Newtype RuleResponseList _
```

#### `RuleState`

``` purescript
newtype RuleState
  = RuleState String
```

##### Instances
``` purescript
Newtype RuleState _
```

#### `RunCommandParameters`

``` purescript
newtype RunCommandParameters
  = RunCommandParameters { "RunCommandTargets" :: RunCommandTargets }
```

<p>This parameter contains the criteria (either InstanceIds or a tag) used to specify which EC2 instances are to be sent the command. </p>

##### Instances
``` purescript
Newtype RunCommandParameters _
```

#### `RunCommandTarget`

``` purescript
newtype RunCommandTarget
  = RunCommandTarget { "Key" :: RunCommandTargetKey, "Values" :: RunCommandTargetValues }
```

<p>Information about the EC2 instances that are to be sent the command, specified as key-value pairs. Each <code>RunCommandTarget</code> block can include only one key, but this key may specify multiple values.</p>

##### Instances
``` purescript
Newtype RunCommandTarget _
```

#### `RunCommandTargetKey`

``` purescript
newtype RunCommandTargetKey
  = RunCommandTargetKey String
```

##### Instances
``` purescript
Newtype RunCommandTargetKey _
```

#### `RunCommandTargetValue`

``` purescript
newtype RunCommandTargetValue
  = RunCommandTargetValue String
```

##### Instances
``` purescript
Newtype RunCommandTargetValue _
```

#### `RunCommandTargetValues`

``` purescript
newtype RunCommandTargetValues
  = RunCommandTargetValues (Array RunCommandTargetValue)
```

##### Instances
``` purescript
Newtype RunCommandTargetValues _
```

#### `RunCommandTargets`

``` purescript
newtype RunCommandTargets
  = RunCommandTargets (Array RunCommandTarget)
```

##### Instances
``` purescript
Newtype RunCommandTargets _
```

#### `ScheduleExpression`

``` purescript
newtype ScheduleExpression
  = ScheduleExpression String
```

##### Instances
``` purescript
Newtype ScheduleExpression _
```

#### `StatementId`

``` purescript
newtype StatementId
  = StatementId String
```

##### Instances
``` purescript
Newtype StatementId _
```

#### `Target`

``` purescript
newtype Target
  = Target { "Id" :: TargetId, "Arn" :: TargetArn, "RoleArn" :: NullOrUndefined (RoleArn), "Input" :: NullOrUndefined (TargetInput), "InputPath" :: NullOrUndefined (TargetInputPath), "InputTransformer" :: NullOrUndefined (InputTransformer), "KinesisParameters" :: NullOrUndefined (KinesisParameters), "RunCommandParameters" :: NullOrUndefined (RunCommandParameters), "EcsParameters" :: NullOrUndefined (EcsParameters) }
```

<p>Targets are the resources to be invoked when a rule is triggered. Target types include EC2 instances, AWS Lambda functions, Amazon Kinesis streams, Amazon ECS tasks, AWS Step Functions state machines, Run Command, and built-in targets.</p>

##### Instances
``` purescript
Newtype Target _
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

#### `TargetId`

``` purescript
newtype TargetId
  = TargetId String
```

##### Instances
``` purescript
Newtype TargetId _
```

#### `TargetIdList`

``` purescript
newtype TargetIdList
  = TargetIdList (Array TargetId)
```

##### Instances
``` purescript
Newtype TargetIdList _
```

#### `TargetInput`

``` purescript
newtype TargetInput
  = TargetInput String
```

##### Instances
``` purescript
Newtype TargetInput _
```

#### `TargetInputPath`

``` purescript
newtype TargetInputPath
  = TargetInputPath String
```

##### Instances
``` purescript
Newtype TargetInputPath _
```

#### `TargetList`

``` purescript
newtype TargetList
  = TargetList (Array Target)
```

##### Instances
``` purescript
Newtype TargetList _
```

#### `TargetPartitionKeyPath`

``` purescript
newtype TargetPartitionKeyPath
  = TargetPartitionKeyPath String
```

##### Instances
``` purescript
Newtype TargetPartitionKeyPath _
```

#### `TestEventPatternRequest`

``` purescript
newtype TestEventPatternRequest
  = TestEventPatternRequest { "EventPattern" :: EventPattern, "Event" :: String }
```

##### Instances
``` purescript
Newtype TestEventPatternRequest _
```

#### `TestEventPatternResponse`

``` purescript
newtype TestEventPatternResponse
  = TestEventPatternResponse { "Result" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype TestEventPatternResponse _
```

#### `TransformerInput`

``` purescript
newtype TransformerInput
  = TransformerInput String
```

##### Instances
``` purescript
Newtype TransformerInput _
```

#### `TransformerPaths`

``` purescript
newtype TransformerPaths
  = TransformerPaths (Map InputTransformerPathKey TargetInputPath)
```

##### Instances
``` purescript
Newtype TransformerPaths _
```


