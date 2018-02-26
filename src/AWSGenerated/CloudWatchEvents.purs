

-- | <p>Amazon CloudWatch Events helps you to respond to state changes in your AWS resources. When your resources change state, they automatically send events into an event stream. You can create rules that match selected events in the stream and route them to targets to take action. You can also use rules to take action on a pre-determined schedule. For example, you can configure rules to:</p> <ul> <li> <p>Automatically invoke an AWS Lambda function to update DNS entries when an event notifies you that Amazon EC2 instance enters the running state.</p> </li> <li> <p>Direct specific API records from CloudTrail to an Amazon Kinesis stream for detailed analysis of potential security or availability risks.</p> </li> <li> <p>Periodically invoke a built-in target to create a snapshot of an Amazon EBS volume.</p> </li> </ul> <p>For more information about the features of Amazon CloudWatch Events, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events">Amazon CloudWatch Events User Guide</a>.</p>
module AWS.CloudWatchEvents where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CloudWatchEvents" :: String


-- | <p>Deletes the specified rule.</p> <p>You must remove all targets from a rule using <a>RemoveTargets</a> before you can delete the rule.</p> <p>When you delete a rule, incoming events might continue to match to the deleted rule. Please allow a short period of time for changes to take effect.</p>
deleteRule :: forall eff. DeleteRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteRule = AWS.request serviceName "DeleteRule" 


-- | <p>Displays the external AWS accounts that are permitted to write events to your account using your account's event bus, and the associated policy. To enable your account to receive events from other accounts, use <a>PutPermission</a>.</p>
describeEventBus :: forall eff. DescribeEventBusRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventBusResponse
describeEventBus = AWS.request serviceName "DescribeEventBus" 


-- | <p>Describes the specified rule.</p>
describeRule :: forall eff. DescribeRuleRequest -> Aff (err :: AWS.RequestError | eff) DescribeRuleResponse
describeRule = AWS.request serviceName "DescribeRule" 


-- | <p>Disables the specified rule. A disabled rule won't match any events, and won't self-trigger if it has a schedule expression.</p> <p>When you disable a rule, incoming events might continue to match to the disabled rule. Please allow a short period of time for changes to take effect.</p>
disableRule :: forall eff. DisableRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
disableRule = AWS.request serviceName "DisableRule" 


-- | <p>Enables the specified rule. If the rule does not exist, the operation fails.</p> <p>When you enable a rule, incoming events might not immediately start matching to a newly enabled rule. Please allow a short period of time for changes to take effect.</p>
enableRule :: forall eff. EnableRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
enableRule = AWS.request serviceName "EnableRule" 


-- | <p>Lists the rules for the specified target. You can see which of the rules in Amazon CloudWatch Events can invoke a specific target in your account.</p>
listRuleNamesByTarget :: forall eff. ListRuleNamesByTargetRequest -> Aff (err :: AWS.RequestError | eff) ListRuleNamesByTargetResponse
listRuleNamesByTarget = AWS.request serviceName "ListRuleNamesByTarget" 


-- | <p>Lists your Amazon CloudWatch Events rules. You can either list all the rules or you can provide a prefix to match to the rule names.</p>
listRules :: forall eff. ListRulesRequest -> Aff (err :: AWS.RequestError | eff) ListRulesResponse
listRules = AWS.request serviceName "ListRules" 


-- | <p>Lists the targets assigned to the specified rule.</p>
listTargetsByRule :: forall eff. ListTargetsByRuleRequest -> Aff (err :: AWS.RequestError | eff) ListTargetsByRuleResponse
listTargetsByRule = AWS.request serviceName "ListTargetsByRule" 


-- | <p>Sends custom events to Amazon CloudWatch Events so that they can be matched to rules.</p>
putEvents :: forall eff. PutEventsRequest -> Aff (err :: AWS.RequestError | eff) PutEventsResponse
putEvents = AWS.request serviceName "PutEvents" 


-- | <p>Running <code>PutPermission</code> permits the specified AWS account to put events to your account's default <i>event bus</i>. CloudWatch Events rules in your account are triggered by these events arriving to your default event bus. </p> <p>For another account to send events to your account, that external account must have a CloudWatch Events rule with your account's default event bus as a target.</p> <p>To enable multiple AWS accounts to put events to your default event bus, run <code>PutPermission</code> once for each of these accounts.</p> <p>The permission policy on the default event bus cannot exceed 10KB in size.</p>
putPermission :: forall eff. PutPermissionRequest -> Aff (err :: AWS.RequestError | eff) Unit
putPermission = AWS.request serviceName "PutPermission" 


-- | <p>Creates or updates the specified rule. Rules are enabled by default, or based on value of the state. You can disable a rule using <a>DisableRule</a>.</p> <p>When you create or update a rule, incoming events might not immediately start matching to new or updated rules. Please allow a short period of time for changes to take effect.</p> <p>A rule must contain at least an EventPattern or ScheduleExpression. Rules with EventPatterns are triggered when a matching event is observed. Rules with ScheduleExpressions self-trigger based on the given schedule. A rule can have both an EventPattern and a ScheduleExpression, in which case the rule triggers on matching events as well as on a schedule.</p> <p>Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, CloudWatch Events uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.</p>
putRule :: forall eff. PutRuleRequest -> Aff (err :: AWS.RequestError | eff) PutRuleResponse
putRule = AWS.request serviceName "PutRule" 


-- | <p>Adds the specified targets to the specified rule, or updates the targets if they are already associated with the rule.</p> <p>Targets are the resources that are invoked when a rule is triggered.</p> <p>You can configure the following as targets for CloudWatch Events:</p> <ul> <li> <p>EC2 instances</p> </li> <li> <p>AWS Lambda functions</p> </li> <li> <p>Streams in Amazon Kinesis Streams</p> </li> <li> <p>Delivery streams in Amazon Kinesis Firehose</p> </li> <li> <p>Amazon ECS tasks</p> </li> <li> <p>AWS Step Functions state machines</p> </li> <li> <p>Pipelines in Amazon Code Pipeline</p> </li> <li> <p>Amazon Inspector assessment templates</p> </li> <li> <p>Amazon SNS topics</p> </li> <li> <p>Amazon SQS queues</p> </li> <li> <p>The default event bus of another AWS account</p> </li> </ul> <p>Note that creating rules with built-in targets is supported only in the AWS Management Console.</p> <p>For some target types, <code>PutTargets</code> provides target-specific parameters. If the target is an Amazon Kinesis stream, you can optionally specify which shard the event goes to by using the <code>KinesisParameters</code> argument. To invoke a command on multiple EC2 instances with one rule, you can use the <code>RunCommandParameters</code> field.</p> <p>To be able to make API calls against the resources that you own, Amazon CloudWatch Events needs the appropriate permissions. For AWS Lambda and Amazon SNS resources, CloudWatch Events relies on resource-based policies. For EC2 instances, Amazon Kinesis streams, and AWS Step Functions state machines, CloudWatch Events relies on IAM roles that you specify in the <code>RoleARN</code> argument in <code>PutTargets</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/auth-and-access-control-cwe.html">Authentication and Access Control</a> in the <i>Amazon CloudWatch Events User Guide</i>.</p> <p>If another AWS account is in the same region and has granted you permission (using <code>PutPermission</code>), you can send events to that account by setting that account's event bus as a target of the rules in your account. To send the matched events to the other account, specify that account's event bus as the <code>Arn</code> when you run <code>PutTargets</code>. If your account sends events to another account, your account is charged for each sent event. Each event sent to antoher account is charged as a custom event. The account receiving the event is not charged. For more information on pricing, see <a href="https://aws.amazon.com/cloudwatch/pricing/">Amazon CloudWatch Pricing</a>.</p> <p>For more information about enabling cross-account events, see <a>PutPermission</a>.</p> <p> <b>Input</b>, <b>InputPath</b> and <b>InputTransformer</b> are mutually exclusive and optional parameters of a target. When a rule is triggered due to a matched event:</p> <ul> <li> <p>If none of the following arguments are specified for a target, then the entire event is passed to the target in JSON form (unless the target is Amazon EC2 Run Command or Amazon ECS task, in which case nothing from the event is passed to the target).</p> </li> <li> <p>If <b>Input</b> is specified in the form of valid JSON, then the matched event is overridden with this constant.</p> </li> <li> <p>If <b>InputPath</b> is specified in the form of JSONPath (for example, <code>$.detail</code>), then only the part of the event specified in the path is passed to the target (for example, only the detail part of the event is passed).</p> </li> <li> <p>If <b>InputTransformer</b> is specified, then one or more specified JSONPaths are extracted from the event and used as values in a template that you specify as the input to the target.</p> </li> </ul> <p>When you specify <code>Input</code>, <code>InputPath</code>, or <code>InputTransformer</code>, you must use JSON dot notation, not bracket notation.</p> <p>When you add targets to a rule and the associated rule triggers soon after, new or updated targets might not be immediately invoked. Please allow a short period of time for changes to take effect.</p> <p>This action can partially fail if too many requests are made at the same time. If that happens, <code>FailedEntryCount</code> is non-zero in the response and each entry in <code>FailedEntries</code> provides the ID of the failed target and the error code.</p>
putTargets :: forall eff. PutTargetsRequest -> Aff (err :: AWS.RequestError | eff) PutTargetsResponse
putTargets = AWS.request serviceName "PutTargets" 


-- | <p>Revokes the permission of another AWS account to be able to put events to your default event bus. Specify the account to revoke by the <code>StatementId</code> value that you associated with the account when you granted it permission with <code>PutPermission</code>. You can find the <code>StatementId</code> by using <a>DescribeEventBus</a>.</p>
removePermission :: forall eff. RemovePermissionRequest -> Aff (err :: AWS.RequestError | eff) Unit
removePermission = AWS.request serviceName "RemovePermission" 


-- | <p>Removes the specified targets from the specified rule. When the rule is triggered, those targets are no longer be invoked.</p> <p>When you remove a target, when the associated rule triggers, removed targets might continue to be invoked. Please allow a short period of time for changes to take effect.</p> <p>This action can partially fail if too many requests are made at the same time. If that happens, <code>FailedEntryCount</code> is non-zero in the response and each entry in <code>FailedEntries</code> provides the ID of the failed target and the error code.</p>
removeTargets :: forall eff. RemoveTargetsRequest -> Aff (err :: AWS.RequestError | eff) RemoveTargetsResponse
removeTargets = AWS.request serviceName "RemoveTargets" 


-- | <p>Tests whether the specified event pattern matches the provided event.</p> <p>Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, CloudWatch Events uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.</p>
testEventPattern :: forall eff. TestEventPatternRequest -> Aff (err :: AWS.RequestError | eff) TestEventPatternResponse
testEventPattern = AWS.request serviceName "TestEventPattern" 


newtype Action = Action String


newtype Arn = Arn String


-- | <p>There is concurrent modification on a rule or target.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { 
  }


newtype DeleteRuleRequest = DeleteRuleRequest 
  { "Name" :: (RuleName)
  }


newtype DescribeEventBusRequest = DescribeEventBusRequest 
  { 
  }


newtype DescribeEventBusResponse = DescribeEventBusResponse 
  { "Name" :: NullOrUndefined (String)
  , "Arn" :: NullOrUndefined (String)
  , "Policy" :: NullOrUndefined (String)
  }


newtype DescribeRuleRequest = DescribeRuleRequest 
  { "Name" :: (RuleName)
  }


newtype DescribeRuleResponse = DescribeRuleResponse 
  { "Name" :: NullOrUndefined (RuleName)
  , "Arn" :: NullOrUndefined (RuleArn)
  , "EventPattern" :: NullOrUndefined (EventPattern)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "State" :: NullOrUndefined (RuleState)
  , "Description" :: NullOrUndefined (RuleDescription)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  }


newtype DisableRuleRequest = DisableRuleRequest 
  { "Name" :: (RuleName)
  }


-- | <p>The custom parameters to be used when the target is an Amazon ECS cluster.</p>
newtype EcsParameters = EcsParameters 
  { "TaskDefinitionArn" :: (Arn)
  , "TaskCount" :: NullOrUndefined (LimitMin1)
  }


newtype EnableRuleRequest = EnableRuleRequest 
  { "Name" :: (RuleName)
  }


newtype ErrorCode = ErrorCode String


newtype ErrorMessage = ErrorMessage String


newtype EventId = EventId String


newtype EventPattern = EventPattern String


newtype EventResource = EventResource String


newtype EventResourceList = EventResourceList (Array EventResource)


newtype EventTime = EventTime Number


-- | <p>Contains the parameters needed for you to provide custom input to a target based on one or more pieces of data extracted from the event.</p>
newtype InputTransformer = InputTransformer 
  { "InputPathsMap" :: NullOrUndefined (TransformerPaths)
  , "InputTemplate" :: (TransformerInput)
  }


newtype InputTransformerPathKey = InputTransformerPathKey String


-- | <p>This exception occurs due to unexpected causes.</p>
newtype InternalException = InternalException 
  { 
  }


-- | <p>The event pattern is not valid.</p>
newtype InvalidEventPatternException = InvalidEventPatternException 
  { 
  }


-- | <p>This object enables you to specify a JSON path to extract from the event and use as the partition key for the Amazon Kinesis stream, so that you can control the shard to which the event goes. If you do not include this parameter, the default is to use the <code>eventId</code> as the partition key.</p>
newtype KinesisParameters = KinesisParameters 
  { "PartitionKeyPath" :: (TargetPartitionKeyPath)
  }


-- | <p>You tried to create more rules or add more targets to a rule than is allowed.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }


newtype LimitMax100 = LimitMax100 Int


newtype LimitMin1 = LimitMin1 Int


newtype ListRuleNamesByTargetRequest = ListRuleNamesByTargetRequest 
  { "TargetArn" :: (TargetArn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (LimitMax100)
  }


newtype ListRuleNamesByTargetResponse = ListRuleNamesByTargetResponse 
  { "RuleNames" :: NullOrUndefined (RuleNameList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListRulesRequest = ListRulesRequest 
  { "NamePrefix" :: NullOrUndefined (RuleName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (LimitMax100)
  }


newtype ListRulesResponse = ListRulesResponse 
  { "Rules" :: NullOrUndefined (RuleResponseList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListTargetsByRuleRequest = ListTargetsByRuleRequest 
  { "Rule" :: (RuleName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (LimitMax100)
  }


newtype ListTargetsByRuleResponse = ListTargetsByRuleResponse 
  { "Targets" :: NullOrUndefined (TargetList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype NextToken = NextToken String


-- | <p>The event bus policy is too long. For more information, see the limits.</p>
newtype PolicyLengthExceededException = PolicyLengthExceededException 
  { 
  }


newtype Principal = Principal String


newtype PutEventsRequest = PutEventsRequest 
  { "Entries" :: (PutEventsRequestEntryList)
  }


-- | <p>Represents an event to be submitted.</p>
newtype PutEventsRequestEntry = PutEventsRequestEntry 
  { "Time" :: NullOrUndefined (EventTime)
  , "Source" :: NullOrUndefined (String)
  , "Resources" :: NullOrUndefined (EventResourceList)
  , "DetailType" :: NullOrUndefined (String)
  , "Detail" :: NullOrUndefined (String)
  }


newtype PutEventsRequestEntryList = PutEventsRequestEntryList (Array PutEventsRequestEntry)


newtype PutEventsResponse = PutEventsResponse 
  { "FailedEntryCount" :: NullOrUndefined (Int)
  , "Entries" :: NullOrUndefined (PutEventsResultEntryList)
  }


-- | <p>Represents an event that failed to be submitted.</p>
newtype PutEventsResultEntry = PutEventsResultEntry 
  { "EventId" :: NullOrUndefined (EventId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }


newtype PutEventsResultEntryList = PutEventsResultEntryList (Array PutEventsResultEntry)


newtype PutPermissionRequest = PutPermissionRequest 
  { "Action" :: (Action)
  , "Principal" :: (Principal)
  , "StatementId" :: (StatementId)
  }


newtype PutRuleRequest = PutRuleRequest 
  { "Name" :: (RuleName)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "EventPattern" :: NullOrUndefined (EventPattern)
  , "State" :: NullOrUndefined (RuleState)
  , "Description" :: NullOrUndefined (RuleDescription)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  }


newtype PutRuleResponse = PutRuleResponse 
  { "RuleArn" :: NullOrUndefined (RuleArn)
  }


newtype PutTargetsRequest = PutTargetsRequest 
  { "Rule" :: (RuleName)
  , "Targets" :: (TargetList)
  }


newtype PutTargetsResponse = PutTargetsResponse 
  { "FailedEntryCount" :: NullOrUndefined (Int)
  , "FailedEntries" :: NullOrUndefined (PutTargetsResultEntryList)
  }


-- | <p>Represents a target that failed to be added to a rule.</p>
newtype PutTargetsResultEntry = PutTargetsResultEntry 
  { "TargetId" :: NullOrUndefined (TargetId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }


newtype PutTargetsResultEntryList = PutTargetsResultEntryList (Array PutTargetsResultEntry)


newtype RemovePermissionRequest = RemovePermissionRequest 
  { "StatementId" :: (StatementId)
  }


newtype RemoveTargetsRequest = RemoveTargetsRequest 
  { "Rule" :: (RuleName)
  , "Ids" :: (TargetIdList)
  }


newtype RemoveTargetsResponse = RemoveTargetsResponse 
  { "FailedEntryCount" :: NullOrUndefined (Int)
  , "FailedEntries" :: NullOrUndefined (RemoveTargetsResultEntryList)
  }


-- | <p>Represents a target that failed to be removed from a rule.</p>
newtype RemoveTargetsResultEntry = RemoveTargetsResultEntry 
  { "TargetId" :: NullOrUndefined (TargetId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }


newtype RemoveTargetsResultEntryList = RemoveTargetsResultEntryList (Array RemoveTargetsResultEntry)


-- | <p>An entity that you specified does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }


newtype RoleArn = RoleArn String


-- | <p>Contains information about a rule in Amazon CloudWatch Events.</p>
newtype Rule = Rule 
  { "Name" :: NullOrUndefined (RuleName)
  , "Arn" :: NullOrUndefined (RuleArn)
  , "EventPattern" :: NullOrUndefined (EventPattern)
  , "State" :: NullOrUndefined (RuleState)
  , "Description" :: NullOrUndefined (RuleDescription)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  }


newtype RuleArn = RuleArn String


newtype RuleDescription = RuleDescription String


newtype RuleName = RuleName String


newtype RuleNameList = RuleNameList (Array RuleName)


newtype RuleResponseList = RuleResponseList (Array Rule)


newtype RuleState = RuleState String


-- | <p>This parameter contains the criteria (either InstanceIds or a tag) used to specify which EC2 instances are to be sent the command. </p>
newtype RunCommandParameters = RunCommandParameters 
  { "RunCommandTargets" :: (RunCommandTargets)
  }


-- | <p>Information about the EC2 instances that are to be sent the command, specified as key-value pairs. Each <code>RunCommandTarget</code> block can include only one key, but this key may specify multiple values.</p>
newtype RunCommandTarget = RunCommandTarget 
  { "Key" :: (RunCommandTargetKey)
  , "Values" :: (RunCommandTargetValues)
  }


newtype RunCommandTargetKey = RunCommandTargetKey String


newtype RunCommandTargetValue = RunCommandTargetValue String


newtype RunCommandTargetValues = RunCommandTargetValues (Array RunCommandTargetValue)


newtype RunCommandTargets = RunCommandTargets (Array RunCommandTarget)


newtype ScheduleExpression = ScheduleExpression String


newtype StatementId = StatementId String


-- | <p>Targets are the resources to be invoked when a rule is triggered. Target types include EC2 instances, AWS Lambda functions, Amazon Kinesis streams, Amazon ECS tasks, AWS Step Functions state machines, Run Command, and built-in targets.</p>
newtype Target = Target 
  { "Id" :: (TargetId)
  , "Arn" :: (TargetArn)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  , "Input" :: NullOrUndefined (TargetInput)
  , "InputPath" :: NullOrUndefined (TargetInputPath)
  , "InputTransformer" :: NullOrUndefined (InputTransformer)
  , "KinesisParameters" :: NullOrUndefined (KinesisParameters)
  , "RunCommandParameters" :: NullOrUndefined (RunCommandParameters)
  , "EcsParameters" :: NullOrUndefined (EcsParameters)
  }


newtype TargetArn = TargetArn String


newtype TargetId = TargetId String


newtype TargetIdList = TargetIdList (Array TargetId)


newtype TargetInput = TargetInput String


newtype TargetInputPath = TargetInputPath String


newtype TargetList = TargetList (Array Target)


newtype TargetPartitionKeyPath = TargetPartitionKeyPath String


newtype TestEventPatternRequest = TestEventPatternRequest 
  { "EventPattern" :: (EventPattern)
  , "Event" :: (String)
  }


newtype TestEventPatternResponse = TestEventPatternResponse 
  { "Result" :: NullOrUndefined (Boolean)
  }


newtype TransformerInput = TransformerInput String


newtype TransformerPaths = TransformerPaths (Map InputTransformerPathKey TargetInputPath)
