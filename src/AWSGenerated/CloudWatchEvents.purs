

-- | <p>Amazon CloudWatch Events helps you to respond to state changes in your AWS resources. When your resources change state, they automatically send events into an event stream. You can create rules that match selected events in the stream and route them to targets to take action. You can also use rules to take action on a pre-determined schedule. For example, you can configure rules to:</p> <ul> <li> <p>Automatically invoke an AWS Lambda function to update DNS entries when an event notifies you that Amazon EC2 instance enters the running state.</p> </li> <li> <p>Direct specific API records from CloudTrail to an Amazon Kinesis stream for detailed analysis of potential security or availability risks.</p> </li> <li> <p>Periodically invoke a built-in target to create a snapshot of an Amazon EBS volume.</p> </li> </ul> <p>For more information about the features of Amazon CloudWatch Events, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events">Amazon CloudWatch Events User Guide</a>.</p>
module AWS.CloudWatchEvents where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeAction :: Newtype Action _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


-- | <p>There is concurrent modification on a rule or target.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { 
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


newtype DeleteRuleRequest = DeleteRuleRequest 
  { "Name" :: (RuleName)
  }
derive instance newtypeDeleteRuleRequest :: Newtype DeleteRuleRequest _


newtype DescribeEventBusRequest = DescribeEventBusRequest 
  { 
  }
derive instance newtypeDescribeEventBusRequest :: Newtype DescribeEventBusRequest _


newtype DescribeEventBusResponse = DescribeEventBusResponse 
  { "Name" :: NullOrUndefined (String)
  , "Arn" :: NullOrUndefined (String)
  , "Policy" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEventBusResponse :: Newtype DescribeEventBusResponse _


newtype DescribeRuleRequest = DescribeRuleRequest 
  { "Name" :: (RuleName)
  }
derive instance newtypeDescribeRuleRequest :: Newtype DescribeRuleRequest _


newtype DescribeRuleResponse = DescribeRuleResponse 
  { "Name" :: NullOrUndefined (RuleName)
  , "Arn" :: NullOrUndefined (RuleArn)
  , "EventPattern" :: NullOrUndefined (EventPattern)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "State" :: NullOrUndefined (RuleState)
  , "Description" :: NullOrUndefined (RuleDescription)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  }
derive instance newtypeDescribeRuleResponse :: Newtype DescribeRuleResponse _


newtype DisableRuleRequest = DisableRuleRequest 
  { "Name" :: (RuleName)
  }
derive instance newtypeDisableRuleRequest :: Newtype DisableRuleRequest _


-- | <p>The custom parameters to be used when the target is an Amazon ECS cluster.</p>
newtype EcsParameters = EcsParameters 
  { "TaskDefinitionArn" :: (Arn)
  , "TaskCount" :: NullOrUndefined (LimitMin1)
  }
derive instance newtypeEcsParameters :: Newtype EcsParameters _


newtype EnableRuleRequest = EnableRuleRequest 
  { "Name" :: (RuleName)
  }
derive instance newtypeEnableRuleRequest :: Newtype EnableRuleRequest _


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype EventId = EventId String
derive instance newtypeEventId :: Newtype EventId _


newtype EventPattern = EventPattern String
derive instance newtypeEventPattern :: Newtype EventPattern _


newtype EventResource = EventResource String
derive instance newtypeEventResource :: Newtype EventResource _


newtype EventResourceList = EventResourceList (Array EventResource)
derive instance newtypeEventResourceList :: Newtype EventResourceList _


newtype EventTime = EventTime Number
derive instance newtypeEventTime :: Newtype EventTime _


-- | <p>Contains the parameters needed for you to provide custom input to a target based on one or more pieces of data extracted from the event.</p>
newtype InputTransformer = InputTransformer 
  { "InputPathsMap" :: NullOrUndefined (TransformerPaths)
  , "InputTemplate" :: (TransformerInput)
  }
derive instance newtypeInputTransformer :: Newtype InputTransformer _


newtype InputTransformerPathKey = InputTransformerPathKey String
derive instance newtypeInputTransformerPathKey :: Newtype InputTransformerPathKey _


-- | <p>This exception occurs due to unexpected causes.</p>
newtype InternalException = InternalException 
  { 
  }
derive instance newtypeInternalException :: Newtype InternalException _


-- | <p>The event pattern is not valid.</p>
newtype InvalidEventPatternException = InvalidEventPatternException 
  { 
  }
derive instance newtypeInvalidEventPatternException :: Newtype InvalidEventPatternException _


-- | <p>This object enables you to specify a JSON path to extract from the event and use as the partition key for the Amazon Kinesis stream, so that you can control the shard to which the event goes. If you do not include this parameter, the default is to use the <code>eventId</code> as the partition key.</p>
newtype KinesisParameters = KinesisParameters 
  { "PartitionKeyPath" :: (TargetPartitionKeyPath)
  }
derive instance newtypeKinesisParameters :: Newtype KinesisParameters _


-- | <p>You tried to create more rules or add more targets to a rule than is allowed.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype LimitMax100 = LimitMax100 Int
derive instance newtypeLimitMax100 :: Newtype LimitMax100 _


newtype LimitMin1 = LimitMin1 Int
derive instance newtypeLimitMin1 :: Newtype LimitMin1 _


newtype ListRuleNamesByTargetRequest = ListRuleNamesByTargetRequest 
  { "TargetArn" :: (TargetArn)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (LimitMax100)
  }
derive instance newtypeListRuleNamesByTargetRequest :: Newtype ListRuleNamesByTargetRequest _


newtype ListRuleNamesByTargetResponse = ListRuleNamesByTargetResponse 
  { "RuleNames" :: NullOrUndefined (RuleNameList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListRuleNamesByTargetResponse :: Newtype ListRuleNamesByTargetResponse _


newtype ListRulesRequest = ListRulesRequest 
  { "NamePrefix" :: NullOrUndefined (RuleName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (LimitMax100)
  }
derive instance newtypeListRulesRequest :: Newtype ListRulesRequest _


newtype ListRulesResponse = ListRulesResponse 
  { "Rules" :: NullOrUndefined (RuleResponseList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListRulesResponse :: Newtype ListRulesResponse _


newtype ListTargetsByRuleRequest = ListTargetsByRuleRequest 
  { "Rule" :: (RuleName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (LimitMax100)
  }
derive instance newtypeListTargetsByRuleRequest :: Newtype ListTargetsByRuleRequest _


newtype ListTargetsByRuleResponse = ListTargetsByRuleResponse 
  { "Targets" :: NullOrUndefined (TargetList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTargetsByRuleResponse :: Newtype ListTargetsByRuleResponse _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>The event bus policy is too long. For more information, see the limits.</p>
newtype PolicyLengthExceededException = PolicyLengthExceededException 
  { 
  }
derive instance newtypePolicyLengthExceededException :: Newtype PolicyLengthExceededException _


newtype Principal = Principal String
derive instance newtypePrincipal :: Newtype Principal _


newtype PutEventsRequest = PutEventsRequest 
  { "Entries" :: (PutEventsRequestEntryList)
  }
derive instance newtypePutEventsRequest :: Newtype PutEventsRequest _


-- | <p>Represents an event to be submitted.</p>
newtype PutEventsRequestEntry = PutEventsRequestEntry 
  { "Time" :: NullOrUndefined (EventTime)
  , "Source" :: NullOrUndefined (String)
  , "Resources" :: NullOrUndefined (EventResourceList)
  , "DetailType" :: NullOrUndefined (String)
  , "Detail" :: NullOrUndefined (String)
  }
derive instance newtypePutEventsRequestEntry :: Newtype PutEventsRequestEntry _


newtype PutEventsRequestEntryList = PutEventsRequestEntryList (Array PutEventsRequestEntry)
derive instance newtypePutEventsRequestEntryList :: Newtype PutEventsRequestEntryList _


newtype PutEventsResponse = PutEventsResponse 
  { "FailedEntryCount" :: NullOrUndefined (Int)
  , "Entries" :: NullOrUndefined (PutEventsResultEntryList)
  }
derive instance newtypePutEventsResponse :: Newtype PutEventsResponse _


-- | <p>Represents an event that failed to be submitted.</p>
newtype PutEventsResultEntry = PutEventsResultEntry 
  { "EventId" :: NullOrUndefined (EventId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePutEventsResultEntry :: Newtype PutEventsResultEntry _


newtype PutEventsResultEntryList = PutEventsResultEntryList (Array PutEventsResultEntry)
derive instance newtypePutEventsResultEntryList :: Newtype PutEventsResultEntryList _


newtype PutPermissionRequest = PutPermissionRequest 
  { "Action" :: (Action)
  , "Principal" :: (Principal)
  , "StatementId" :: (StatementId)
  }
derive instance newtypePutPermissionRequest :: Newtype PutPermissionRequest _


newtype PutRuleRequest = PutRuleRequest 
  { "Name" :: (RuleName)
  , "ScheduleExpression" :: NullOrUndefined (ScheduleExpression)
  , "EventPattern" :: NullOrUndefined (EventPattern)
  , "State" :: NullOrUndefined (RuleState)
  , "Description" :: NullOrUndefined (RuleDescription)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  }
derive instance newtypePutRuleRequest :: Newtype PutRuleRequest _


newtype PutRuleResponse = PutRuleResponse 
  { "RuleArn" :: NullOrUndefined (RuleArn)
  }
derive instance newtypePutRuleResponse :: Newtype PutRuleResponse _


newtype PutTargetsRequest = PutTargetsRequest 
  { "Rule" :: (RuleName)
  , "Targets" :: (TargetList)
  }
derive instance newtypePutTargetsRequest :: Newtype PutTargetsRequest _


newtype PutTargetsResponse = PutTargetsResponse 
  { "FailedEntryCount" :: NullOrUndefined (Int)
  , "FailedEntries" :: NullOrUndefined (PutTargetsResultEntryList)
  }
derive instance newtypePutTargetsResponse :: Newtype PutTargetsResponse _


-- | <p>Represents a target that failed to be added to a rule.</p>
newtype PutTargetsResultEntry = PutTargetsResultEntry 
  { "TargetId" :: NullOrUndefined (TargetId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePutTargetsResultEntry :: Newtype PutTargetsResultEntry _


newtype PutTargetsResultEntryList = PutTargetsResultEntryList (Array PutTargetsResultEntry)
derive instance newtypePutTargetsResultEntryList :: Newtype PutTargetsResultEntryList _


newtype RemovePermissionRequest = RemovePermissionRequest 
  { "StatementId" :: (StatementId)
  }
derive instance newtypeRemovePermissionRequest :: Newtype RemovePermissionRequest _


newtype RemoveTargetsRequest = RemoveTargetsRequest 
  { "Rule" :: (RuleName)
  , "Ids" :: (TargetIdList)
  }
derive instance newtypeRemoveTargetsRequest :: Newtype RemoveTargetsRequest _


newtype RemoveTargetsResponse = RemoveTargetsResponse 
  { "FailedEntryCount" :: NullOrUndefined (Int)
  , "FailedEntries" :: NullOrUndefined (RemoveTargetsResultEntryList)
  }
derive instance newtypeRemoveTargetsResponse :: Newtype RemoveTargetsResponse _


-- | <p>Represents a target that failed to be removed from a rule.</p>
newtype RemoveTargetsResultEntry = RemoveTargetsResultEntry 
  { "TargetId" :: NullOrUndefined (TargetId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeRemoveTargetsResultEntry :: Newtype RemoveTargetsResultEntry _


newtype RemoveTargetsResultEntryList = RemoveTargetsResultEntryList (Array RemoveTargetsResultEntry)
derive instance newtypeRemoveTargetsResultEntryList :: Newtype RemoveTargetsResultEntryList _


-- | <p>An entity that you specified does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _


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
derive instance newtypeRule :: Newtype Rule _


newtype RuleArn = RuleArn String
derive instance newtypeRuleArn :: Newtype RuleArn _


newtype RuleDescription = RuleDescription String
derive instance newtypeRuleDescription :: Newtype RuleDescription _


newtype RuleName = RuleName String
derive instance newtypeRuleName :: Newtype RuleName _


newtype RuleNameList = RuleNameList (Array RuleName)
derive instance newtypeRuleNameList :: Newtype RuleNameList _


newtype RuleResponseList = RuleResponseList (Array Rule)
derive instance newtypeRuleResponseList :: Newtype RuleResponseList _


newtype RuleState = RuleState String
derive instance newtypeRuleState :: Newtype RuleState _


-- | <p>This parameter contains the criteria (either InstanceIds or a tag) used to specify which EC2 instances are to be sent the command. </p>
newtype RunCommandParameters = RunCommandParameters 
  { "RunCommandTargets" :: (RunCommandTargets)
  }
derive instance newtypeRunCommandParameters :: Newtype RunCommandParameters _


-- | <p>Information about the EC2 instances that are to be sent the command, specified as key-value pairs. Each <code>RunCommandTarget</code> block can include only one key, but this key may specify multiple values.</p>
newtype RunCommandTarget = RunCommandTarget 
  { "Key" :: (RunCommandTargetKey)
  , "Values" :: (RunCommandTargetValues)
  }
derive instance newtypeRunCommandTarget :: Newtype RunCommandTarget _


newtype RunCommandTargetKey = RunCommandTargetKey String
derive instance newtypeRunCommandTargetKey :: Newtype RunCommandTargetKey _


newtype RunCommandTargetValue = RunCommandTargetValue String
derive instance newtypeRunCommandTargetValue :: Newtype RunCommandTargetValue _


newtype RunCommandTargetValues = RunCommandTargetValues (Array RunCommandTargetValue)
derive instance newtypeRunCommandTargetValues :: Newtype RunCommandTargetValues _


newtype RunCommandTargets = RunCommandTargets (Array RunCommandTarget)
derive instance newtypeRunCommandTargets :: Newtype RunCommandTargets _


newtype ScheduleExpression = ScheduleExpression String
derive instance newtypeScheduleExpression :: Newtype ScheduleExpression _


newtype StatementId = StatementId String
derive instance newtypeStatementId :: Newtype StatementId _


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
derive instance newtypeTarget :: Newtype Target _


newtype TargetArn = TargetArn String
derive instance newtypeTargetArn :: Newtype TargetArn _


newtype TargetId = TargetId String
derive instance newtypeTargetId :: Newtype TargetId _


newtype TargetIdList = TargetIdList (Array TargetId)
derive instance newtypeTargetIdList :: Newtype TargetIdList _


newtype TargetInput = TargetInput String
derive instance newtypeTargetInput :: Newtype TargetInput _


newtype TargetInputPath = TargetInputPath String
derive instance newtypeTargetInputPath :: Newtype TargetInputPath _


newtype TargetList = TargetList (Array Target)
derive instance newtypeTargetList :: Newtype TargetList _


newtype TargetPartitionKeyPath = TargetPartitionKeyPath String
derive instance newtypeTargetPartitionKeyPath :: Newtype TargetPartitionKeyPath _


newtype TestEventPatternRequest = TestEventPatternRequest 
  { "EventPattern" :: (EventPattern)
  , "Event" :: (String)
  }
derive instance newtypeTestEventPatternRequest :: Newtype TestEventPatternRequest _


newtype TestEventPatternResponse = TestEventPatternResponse 
  { "Result" :: NullOrUndefined (Boolean)
  }
derive instance newtypeTestEventPatternResponse :: Newtype TestEventPatternResponse _


newtype TransformerInput = TransformerInput String
derive instance newtypeTransformerInput :: Newtype TransformerInput _


newtype TransformerPaths = TransformerPaths (Map InputTransformerPathKey TargetInputPath)
derive instance newtypeTransformerPaths :: Newtype TransformerPaths _
