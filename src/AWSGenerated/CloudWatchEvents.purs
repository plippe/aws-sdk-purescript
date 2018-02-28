

-- | <p>Amazon CloudWatch Events helps you to respond to state changes in your AWS resources. When your resources change state, they automatically send events into an event stream. You can create rules that match selected events in the stream and route them to targets to take action. You can also use rules to take action on a pre-determined schedule. For example, you can configure rules to:</p> <ul> <li> <p>Automatically invoke an AWS Lambda function to update DNS entries when an event notifies you that Amazon EC2 instance enters the running state.</p> </li> <li> <p>Direct specific API records from CloudTrail to an Amazon Kinesis stream for detailed analysis of potential security or availability risks.</p> </li> <li> <p>Periodically invoke a built-in target to create a snapshot of an Amazon EBS volume.</p> </li> </ul> <p>For more information about the features of Amazon CloudWatch Events, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events">Amazon CloudWatch Events User Guide</a>.</p>
module AWS.CloudWatchEvents where

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

serviceName = "CloudWatchEvents" :: String


-- | <p>Deletes the specified rule.</p> <p>You must remove all targets from a rule using <a>RemoveTargets</a> before you can delete the rule.</p> <p>When you delete a rule, incoming events might continue to match to the deleted rule. Please allow a short period of time for changes to take effect.</p>
deleteRule :: forall eff. DeleteRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteRule = Request.request serviceName "deleteRule" 


-- | <p>Displays the external AWS accounts that are permitted to write events to your account using your account's event bus, and the associated policy. To enable your account to receive events from other accounts, use <a>PutPermission</a>.</p>
describeEventBus :: forall eff. DescribeEventBusRequest -> Aff (exception :: EXCEPTION | eff) DescribeEventBusResponse
describeEventBus = Request.request serviceName "describeEventBus" 


-- | <p>Describes the specified rule.</p>
describeRule :: forall eff. DescribeRuleRequest -> Aff (exception :: EXCEPTION | eff) DescribeRuleResponse
describeRule = Request.request serviceName "describeRule" 


-- | <p>Disables the specified rule. A disabled rule won't match any events, and won't self-trigger if it has a schedule expression.</p> <p>When you disable a rule, incoming events might continue to match to the disabled rule. Please allow a short period of time for changes to take effect.</p>
disableRule :: forall eff. DisableRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
disableRule = Request.request serviceName "disableRule" 


-- | <p>Enables the specified rule. If the rule does not exist, the operation fails.</p> <p>When you enable a rule, incoming events might not immediately start matching to a newly enabled rule. Please allow a short period of time for changes to take effect.</p>
enableRule :: forall eff. EnableRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
enableRule = Request.request serviceName "enableRule" 


-- | <p>Lists the rules for the specified target. You can see which of the rules in Amazon CloudWatch Events can invoke a specific target in your account.</p>
listRuleNamesByTarget :: forall eff. ListRuleNamesByTargetRequest -> Aff (exception :: EXCEPTION | eff) ListRuleNamesByTargetResponse
listRuleNamesByTarget = Request.request serviceName "listRuleNamesByTarget" 


-- | <p>Lists your Amazon CloudWatch Events rules. You can either list all the rules or you can provide a prefix to match to the rule names.</p>
listRules :: forall eff. ListRulesRequest -> Aff (exception :: EXCEPTION | eff) ListRulesResponse
listRules = Request.request serviceName "listRules" 


-- | <p>Lists the targets assigned to the specified rule.</p>
listTargetsByRule :: forall eff. ListTargetsByRuleRequest -> Aff (exception :: EXCEPTION | eff) ListTargetsByRuleResponse
listTargetsByRule = Request.request serviceName "listTargetsByRule" 


-- | <p>Sends custom events to Amazon CloudWatch Events so that they can be matched to rules.</p>
putEvents :: forall eff. PutEventsRequest -> Aff (exception :: EXCEPTION | eff) PutEventsResponse
putEvents = Request.request serviceName "putEvents" 


-- | <p>Running <code>PutPermission</code> permits the specified AWS account to put events to your account's default <i>event bus</i>. CloudWatch Events rules in your account are triggered by these events arriving to your default event bus. </p> <p>For another account to send events to your account, that external account must have a CloudWatch Events rule with your account's default event bus as a target.</p> <p>To enable multiple AWS accounts to put events to your default event bus, run <code>PutPermission</code> once for each of these accounts.</p> <p>The permission policy on the default event bus cannot exceed 10KB in size.</p>
putPermission :: forall eff. PutPermissionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putPermission = Request.request serviceName "putPermission" 


-- | <p>Creates or updates the specified rule. Rules are enabled by default, or based on value of the state. You can disable a rule using <a>DisableRule</a>.</p> <p>When you create or update a rule, incoming events might not immediately start matching to new or updated rules. Please allow a short period of time for changes to take effect.</p> <p>A rule must contain at least an EventPattern or ScheduleExpression. Rules with EventPatterns are triggered when a matching event is observed. Rules with ScheduleExpressions self-trigger based on the given schedule. A rule can have both an EventPattern and a ScheduleExpression, in which case the rule triggers on matching events as well as on a schedule.</p> <p>Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, CloudWatch Events uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.</p>
putRule :: forall eff. PutRuleRequest -> Aff (exception :: EXCEPTION | eff) PutRuleResponse
putRule = Request.request serviceName "putRule" 


-- | <p>Adds the specified targets to the specified rule, or updates the targets if they are already associated with the rule.</p> <p>Targets are the resources that are invoked when a rule is triggered.</p> <p>You can configure the following as targets for CloudWatch Events:</p> <ul> <li> <p>EC2 instances</p> </li> <li> <p>AWS Lambda functions</p> </li> <li> <p>Streams in Amazon Kinesis Streams</p> </li> <li> <p>Delivery streams in Amazon Kinesis Firehose</p> </li> <li> <p>Amazon ECS tasks</p> </li> <li> <p>AWS Step Functions state machines</p> </li> <li> <p>Pipelines in Amazon Code Pipeline</p> </li> <li> <p>Amazon Inspector assessment templates</p> </li> <li> <p>Amazon SNS topics</p> </li> <li> <p>Amazon SQS queues</p> </li> <li> <p>The default event bus of another AWS account</p> </li> </ul> <p>Note that creating rules with built-in targets is supported only in the AWS Management Console.</p> <p>For some target types, <code>PutTargets</code> provides target-specific parameters. If the target is an Amazon Kinesis stream, you can optionally specify which shard the event goes to by using the <code>KinesisParameters</code> argument. To invoke a command on multiple EC2 instances with one rule, you can use the <code>RunCommandParameters</code> field.</p> <p>To be able to make API calls against the resources that you own, Amazon CloudWatch Events needs the appropriate permissions. For AWS Lambda and Amazon SNS resources, CloudWatch Events relies on resource-based policies. For EC2 instances, Amazon Kinesis streams, and AWS Step Functions state machines, CloudWatch Events relies on IAM roles that you specify in the <code>RoleARN</code> argument in <code>PutTargets</code>. For more information, see <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/auth-and-access-control-cwe.html">Authentication and Access Control</a> in the <i>Amazon CloudWatch Events User Guide</i>.</p> <p>If another AWS account is in the same region and has granted you permission (using <code>PutPermission</code>), you can send events to that account by setting that account's event bus as a target of the rules in your account. To send the matched events to the other account, specify that account's event bus as the <code>Arn</code> when you run <code>PutTargets</code>. If your account sends events to another account, your account is charged for each sent event. Each event sent to antoher account is charged as a custom event. The account receiving the event is not charged. For more information on pricing, see <a href="https://aws.amazon.com/cloudwatch/pricing/">Amazon CloudWatch Pricing</a>.</p> <p>For more information about enabling cross-account events, see <a>PutPermission</a>.</p> <p> <b>Input</b>, <b>InputPath</b> and <b>InputTransformer</b> are mutually exclusive and optional parameters of a target. When a rule is triggered due to a matched event:</p> <ul> <li> <p>If none of the following arguments are specified for a target, then the entire event is passed to the target in JSON form (unless the target is Amazon EC2 Run Command or Amazon ECS task, in which case nothing from the event is passed to the target).</p> </li> <li> <p>If <b>Input</b> is specified in the form of valid JSON, then the matched event is overridden with this constant.</p> </li> <li> <p>If <b>InputPath</b> is specified in the form of JSONPath (for example, <code>$.detail</code>), then only the part of the event specified in the path is passed to the target (for example, only the detail part of the event is passed).</p> </li> <li> <p>If <b>InputTransformer</b> is specified, then one or more specified JSONPaths are extracted from the event and used as values in a template that you specify as the input to the target.</p> </li> </ul> <p>When you specify <code>Input</code>, <code>InputPath</code>, or <code>InputTransformer</code>, you must use JSON dot notation, not bracket notation.</p> <p>When you add targets to a rule and the associated rule triggers soon after, new or updated targets might not be immediately invoked. Please allow a short period of time for changes to take effect.</p> <p>This action can partially fail if too many requests are made at the same time. If that happens, <code>FailedEntryCount</code> is non-zero in the response and each entry in <code>FailedEntries</code> provides the ID of the failed target and the error code.</p>
putTargets :: forall eff. PutTargetsRequest -> Aff (exception :: EXCEPTION | eff) PutTargetsResponse
putTargets = Request.request serviceName "putTargets" 


-- | <p>Revokes the permission of another AWS account to be able to put events to your default event bus. Specify the account to revoke by the <code>StatementId</code> value that you associated with the account when you granted it permission with <code>PutPermission</code>. You can find the <code>StatementId</code> by using <a>DescribeEventBus</a>.</p>
removePermission :: forall eff. RemovePermissionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removePermission = Request.request serviceName "removePermission" 


-- | <p>Removes the specified targets from the specified rule. When the rule is triggered, those targets are no longer be invoked.</p> <p>When you remove a target, when the associated rule triggers, removed targets might continue to be invoked. Please allow a short period of time for changes to take effect.</p> <p>This action can partially fail if too many requests are made at the same time. If that happens, <code>FailedEntryCount</code> is non-zero in the response and each entry in <code>FailedEntries</code> provides the ID of the failed target and the error code.</p>
removeTargets :: forall eff. RemoveTargetsRequest -> Aff (exception :: EXCEPTION | eff) RemoveTargetsResponse
removeTargets = Request.request serviceName "removeTargets" 


-- | <p>Tests whether the specified event pattern matches the provided event.</p> <p>Most services in AWS treat : or / as the same character in Amazon Resource Names (ARNs). However, CloudWatch Events uses an exact match in event patterns and rules. Be sure to use the correct ARN characters when creating event patterns so that they match the ARN syntax in the event you want to match.</p>
testEventPattern :: forall eff. TestEventPatternRequest -> Aff (exception :: EXCEPTION | eff) TestEventPatternResponse
testEventPattern = Request.request serviceName "testEventPattern" 


newtype Action = Action String
derive instance newtypeAction :: Newtype Action _
derive instance repGenericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
instance decodeAction :: Decode Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAction :: Encode Action where
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


-- | <p>There is concurrent modification on a rule or target.</p>
newtype ConcurrentModificationException = ConcurrentModificationException Types.NoArguments
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRuleRequest = DeleteRuleRequest 
  { "Name" :: (RuleName)
  }
derive instance newtypeDeleteRuleRequest :: Newtype DeleteRuleRequest _
derive instance repGenericDeleteRuleRequest :: Generic DeleteRuleRequest _
instance showDeleteRuleRequest :: Show DeleteRuleRequest where
  show = genericShow
instance decodeDeleteRuleRequest :: Decode DeleteRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRuleRequest :: Encode DeleteRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventBusRequest = DescribeEventBusRequest Types.NoArguments
derive instance newtypeDescribeEventBusRequest :: Newtype DescribeEventBusRequest _
derive instance repGenericDescribeEventBusRequest :: Generic DescribeEventBusRequest _
instance showDescribeEventBusRequest :: Show DescribeEventBusRequest where
  show = genericShow
instance decodeDescribeEventBusRequest :: Decode DescribeEventBusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventBusRequest :: Encode DescribeEventBusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeEventBusResponse = DescribeEventBusResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Arn" :: NullOrUndefined.NullOrUndefined (String)
  , "Policy" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEventBusResponse :: Newtype DescribeEventBusResponse _
derive instance repGenericDescribeEventBusResponse :: Generic DescribeEventBusResponse _
instance showDescribeEventBusResponse :: Show DescribeEventBusResponse where
  show = genericShow
instance decodeDescribeEventBusResponse :: Decode DescribeEventBusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventBusResponse :: Encode DescribeEventBusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRuleRequest = DescribeRuleRequest 
  { "Name" :: (RuleName)
  }
derive instance newtypeDescribeRuleRequest :: Newtype DescribeRuleRequest _
derive instance repGenericDescribeRuleRequest :: Generic DescribeRuleRequest _
instance showDescribeRuleRequest :: Show DescribeRuleRequest where
  show = genericShow
instance decodeDescribeRuleRequest :: Decode DescribeRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRuleRequest :: Encode DescribeRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRuleResponse = DescribeRuleResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (RuleName)
  , "Arn" :: NullOrUndefined.NullOrUndefined (RuleArn)
  , "EventPattern" :: NullOrUndefined.NullOrUndefined (EventPattern)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "State" :: NullOrUndefined.NullOrUndefined (RuleState)
  , "Description" :: NullOrUndefined.NullOrUndefined (RuleDescription)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (RoleArn)
  }
derive instance newtypeDescribeRuleResponse :: Newtype DescribeRuleResponse _
derive instance repGenericDescribeRuleResponse :: Generic DescribeRuleResponse _
instance showDescribeRuleResponse :: Show DescribeRuleResponse where
  show = genericShow
instance decodeDescribeRuleResponse :: Decode DescribeRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRuleResponse :: Encode DescribeRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisableRuleRequest = DisableRuleRequest 
  { "Name" :: (RuleName)
  }
derive instance newtypeDisableRuleRequest :: Newtype DisableRuleRequest _
derive instance repGenericDisableRuleRequest :: Generic DisableRuleRequest _
instance showDisableRuleRequest :: Show DisableRuleRequest where
  show = genericShow
instance decodeDisableRuleRequest :: Decode DisableRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableRuleRequest :: Encode DisableRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The custom parameters to be used when the target is an Amazon ECS cluster.</p>
newtype EcsParameters = EcsParameters 
  { "TaskDefinitionArn" :: (Arn)
  , "TaskCount" :: NullOrUndefined.NullOrUndefined (LimitMin1)
  }
derive instance newtypeEcsParameters :: Newtype EcsParameters _
derive instance repGenericEcsParameters :: Generic EcsParameters _
instance showEcsParameters :: Show EcsParameters where
  show = genericShow
instance decodeEcsParameters :: Decode EcsParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEcsParameters :: Encode EcsParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableRuleRequest = EnableRuleRequest 
  { "Name" :: (RuleName)
  }
derive instance newtypeEnableRuleRequest :: Newtype EnableRuleRequest _
derive instance repGenericEnableRuleRequest :: Generic EnableRuleRequest _
instance showEnableRuleRequest :: Show EnableRuleRequest where
  show = genericShow
instance decodeEnableRuleRequest :: Decode EnableRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableRuleRequest :: Encode EnableRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
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


newtype EventId = EventId String
derive instance newtypeEventId :: Newtype EventId _
derive instance repGenericEventId :: Generic EventId _
instance showEventId :: Show EventId where
  show = genericShow
instance decodeEventId :: Decode EventId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventId :: Encode EventId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventPattern = EventPattern String
derive instance newtypeEventPattern :: Newtype EventPattern _
derive instance repGenericEventPattern :: Generic EventPattern _
instance showEventPattern :: Show EventPattern where
  show = genericShow
instance decodeEventPattern :: Decode EventPattern where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventPattern :: Encode EventPattern where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventResource = EventResource String
derive instance newtypeEventResource :: Newtype EventResource _
derive instance repGenericEventResource :: Generic EventResource _
instance showEventResource :: Show EventResource where
  show = genericShow
instance decodeEventResource :: Decode EventResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventResource :: Encode EventResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventResourceList = EventResourceList (Array EventResource)
derive instance newtypeEventResourceList :: Newtype EventResourceList _
derive instance repGenericEventResourceList :: Generic EventResourceList _
instance showEventResourceList :: Show EventResourceList where
  show = genericShow
instance decodeEventResourceList :: Decode EventResourceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventResourceList :: Encode EventResourceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTime = EventTime Number
derive instance newtypeEventTime :: Newtype EventTime _
derive instance repGenericEventTime :: Generic EventTime _
instance showEventTime :: Show EventTime where
  show = genericShow
instance decodeEventTime :: Decode EventTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTime :: Encode EventTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters needed for you to provide custom input to a target based on one or more pieces of data extracted from the event.</p>
newtype InputTransformer = InputTransformer 
  { "InputPathsMap" :: NullOrUndefined.NullOrUndefined (TransformerPaths)
  , "InputTemplate" :: (TransformerInput)
  }
derive instance newtypeInputTransformer :: Newtype InputTransformer _
derive instance repGenericInputTransformer :: Generic InputTransformer _
instance showInputTransformer :: Show InputTransformer where
  show = genericShow
instance decodeInputTransformer :: Decode InputTransformer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputTransformer :: Encode InputTransformer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputTransformerPathKey = InputTransformerPathKey String
derive instance newtypeInputTransformerPathKey :: Newtype InputTransformerPathKey _
derive instance repGenericInputTransformerPathKey :: Generic InputTransformerPathKey _
instance showInputTransformerPathKey :: Show InputTransformerPathKey where
  show = genericShow
instance decodeInputTransformerPathKey :: Decode InputTransformerPathKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputTransformerPathKey :: Encode InputTransformerPathKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception occurs due to unexpected causes.</p>
newtype InternalException = InternalException Types.NoArguments
derive instance newtypeInternalException :: Newtype InternalException _
derive instance repGenericInternalException :: Generic InternalException _
instance showInternalException :: Show InternalException where
  show = genericShow
instance decodeInternalException :: Decode InternalException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalException :: Encode InternalException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The event pattern is not valid.</p>
newtype InvalidEventPatternException = InvalidEventPatternException Types.NoArguments
derive instance newtypeInvalidEventPatternException :: Newtype InvalidEventPatternException _
derive instance repGenericInvalidEventPatternException :: Generic InvalidEventPatternException _
instance showInvalidEventPatternException :: Show InvalidEventPatternException where
  show = genericShow
instance decodeInvalidEventPatternException :: Decode InvalidEventPatternException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidEventPatternException :: Encode InvalidEventPatternException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This object enables you to specify a JSON path to extract from the event and use as the partition key for the Amazon Kinesis stream, so that you can control the shard to which the event goes. If you do not include this parameter, the default is to use the <code>eventId</code> as the partition key.</p>
newtype KinesisParameters = KinesisParameters 
  { "PartitionKeyPath" :: (TargetPartitionKeyPath)
  }
derive instance newtypeKinesisParameters :: Newtype KinesisParameters _
derive instance repGenericKinesisParameters :: Generic KinesisParameters _
instance showKinesisParameters :: Show KinesisParameters where
  show = genericShow
instance decodeKinesisParameters :: Decode KinesisParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisParameters :: Encode KinesisParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You tried to create more rules or add more targets to a rule than is allowed.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitMax100 = LimitMax100 Int
derive instance newtypeLimitMax100 :: Newtype LimitMax100 _
derive instance repGenericLimitMax100 :: Generic LimitMax100 _
instance showLimitMax100 :: Show LimitMax100 where
  show = genericShow
instance decodeLimitMax100 :: Decode LimitMax100 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitMax100 :: Encode LimitMax100 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitMin1 = LimitMin1 Int
derive instance newtypeLimitMin1 :: Newtype LimitMin1 _
derive instance repGenericLimitMin1 :: Generic LimitMin1 _
instance showLimitMin1 :: Show LimitMin1 where
  show = genericShow
instance decodeLimitMin1 :: Decode LimitMin1 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitMin1 :: Encode LimitMin1 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRuleNamesByTargetRequest = ListRuleNamesByTargetRequest 
  { "TargetArn" :: (TargetArn)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitMax100)
  }
derive instance newtypeListRuleNamesByTargetRequest :: Newtype ListRuleNamesByTargetRequest _
derive instance repGenericListRuleNamesByTargetRequest :: Generic ListRuleNamesByTargetRequest _
instance showListRuleNamesByTargetRequest :: Show ListRuleNamesByTargetRequest where
  show = genericShow
instance decodeListRuleNamesByTargetRequest :: Decode ListRuleNamesByTargetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRuleNamesByTargetRequest :: Encode ListRuleNamesByTargetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRuleNamesByTargetResponse = ListRuleNamesByTargetResponse 
  { "RuleNames" :: NullOrUndefined.NullOrUndefined (RuleNameList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListRuleNamesByTargetResponse :: Newtype ListRuleNamesByTargetResponse _
derive instance repGenericListRuleNamesByTargetResponse :: Generic ListRuleNamesByTargetResponse _
instance showListRuleNamesByTargetResponse :: Show ListRuleNamesByTargetResponse where
  show = genericShow
instance decodeListRuleNamesByTargetResponse :: Decode ListRuleNamesByTargetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRuleNamesByTargetResponse :: Encode ListRuleNamesByTargetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRulesRequest = ListRulesRequest 
  { "NamePrefix" :: NullOrUndefined.NullOrUndefined (RuleName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitMax100)
  }
derive instance newtypeListRulesRequest :: Newtype ListRulesRequest _
derive instance repGenericListRulesRequest :: Generic ListRulesRequest _
instance showListRulesRequest :: Show ListRulesRequest where
  show = genericShow
instance decodeListRulesRequest :: Decode ListRulesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRulesRequest :: Encode ListRulesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListRulesResponse = ListRulesResponse 
  { "Rules" :: NullOrUndefined.NullOrUndefined (RuleResponseList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListRulesResponse :: Newtype ListRulesResponse _
derive instance repGenericListRulesResponse :: Generic ListRulesResponse _
instance showListRulesResponse :: Show ListRulesResponse where
  show = genericShow
instance decodeListRulesResponse :: Decode ListRulesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListRulesResponse :: Encode ListRulesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTargetsByRuleRequest = ListTargetsByRuleRequest 
  { "Rule" :: (RuleName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitMax100)
  }
derive instance newtypeListTargetsByRuleRequest :: Newtype ListTargetsByRuleRequest _
derive instance repGenericListTargetsByRuleRequest :: Generic ListTargetsByRuleRequest _
instance showListTargetsByRuleRequest :: Show ListTargetsByRuleRequest where
  show = genericShow
instance decodeListTargetsByRuleRequest :: Decode ListTargetsByRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTargetsByRuleRequest :: Encode ListTargetsByRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTargetsByRuleResponse = ListTargetsByRuleResponse 
  { "Targets" :: NullOrUndefined.NullOrUndefined (TargetList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListTargetsByRuleResponse :: Newtype ListTargetsByRuleResponse _
derive instance repGenericListTargetsByRuleResponse :: Generic ListTargetsByRuleResponse _
instance showListTargetsByRuleResponse :: Show ListTargetsByRuleResponse where
  show = genericShow
instance decodeListTargetsByRuleResponse :: Decode ListTargetsByRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTargetsByRuleResponse :: Encode ListTargetsByRuleResponse where
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


-- | <p>The event bus policy is too long. For more information, see the limits.</p>
newtype PolicyLengthExceededException = PolicyLengthExceededException Types.NoArguments
derive instance newtypePolicyLengthExceededException :: Newtype PolicyLengthExceededException _
derive instance repGenericPolicyLengthExceededException :: Generic PolicyLengthExceededException _
instance showPolicyLengthExceededException :: Show PolicyLengthExceededException where
  show = genericShow
instance decodePolicyLengthExceededException :: Decode PolicyLengthExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyLengthExceededException :: Encode PolicyLengthExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Principal = Principal String
derive instance newtypePrincipal :: Newtype Principal _
derive instance repGenericPrincipal :: Generic Principal _
instance showPrincipal :: Show Principal where
  show = genericShow
instance decodePrincipal :: Decode Principal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipal :: Encode Principal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutEventsRequest = PutEventsRequest 
  { "Entries" :: (PutEventsRequestEntryList)
  }
derive instance newtypePutEventsRequest :: Newtype PutEventsRequest _
derive instance repGenericPutEventsRequest :: Generic PutEventsRequest _
instance showPutEventsRequest :: Show PutEventsRequest where
  show = genericShow
instance decodePutEventsRequest :: Decode PutEventsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventsRequest :: Encode PutEventsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an event to be submitted.</p>
newtype PutEventsRequestEntry = PutEventsRequestEntry 
  { "Time" :: NullOrUndefined.NullOrUndefined (EventTime)
  , "Source" :: NullOrUndefined.NullOrUndefined (String)
  , "Resources" :: NullOrUndefined.NullOrUndefined (EventResourceList)
  , "DetailType" :: NullOrUndefined.NullOrUndefined (String)
  , "Detail" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePutEventsRequestEntry :: Newtype PutEventsRequestEntry _
derive instance repGenericPutEventsRequestEntry :: Generic PutEventsRequestEntry _
instance showPutEventsRequestEntry :: Show PutEventsRequestEntry where
  show = genericShow
instance decodePutEventsRequestEntry :: Decode PutEventsRequestEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventsRequestEntry :: Encode PutEventsRequestEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutEventsRequestEntryList = PutEventsRequestEntryList (Array PutEventsRequestEntry)
derive instance newtypePutEventsRequestEntryList :: Newtype PutEventsRequestEntryList _
derive instance repGenericPutEventsRequestEntryList :: Generic PutEventsRequestEntryList _
instance showPutEventsRequestEntryList :: Show PutEventsRequestEntryList where
  show = genericShow
instance decodePutEventsRequestEntryList :: Decode PutEventsRequestEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventsRequestEntryList :: Encode PutEventsRequestEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutEventsResponse = PutEventsResponse 
  { "FailedEntryCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "Entries" :: NullOrUndefined.NullOrUndefined (PutEventsResultEntryList)
  }
derive instance newtypePutEventsResponse :: Newtype PutEventsResponse _
derive instance repGenericPutEventsResponse :: Generic PutEventsResponse _
instance showPutEventsResponse :: Show PutEventsResponse where
  show = genericShow
instance decodePutEventsResponse :: Decode PutEventsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventsResponse :: Encode PutEventsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents an event that failed to be submitted.</p>
newtype PutEventsResultEntry = PutEventsResultEntry 
  { "EventId" :: NullOrUndefined.NullOrUndefined (EventId)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypePutEventsResultEntry :: Newtype PutEventsResultEntry _
derive instance repGenericPutEventsResultEntry :: Generic PutEventsResultEntry _
instance showPutEventsResultEntry :: Show PutEventsResultEntry where
  show = genericShow
instance decodePutEventsResultEntry :: Decode PutEventsResultEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventsResultEntry :: Encode PutEventsResultEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutEventsResultEntryList = PutEventsResultEntryList (Array PutEventsResultEntry)
derive instance newtypePutEventsResultEntryList :: Newtype PutEventsResultEntryList _
derive instance repGenericPutEventsResultEntryList :: Generic PutEventsResultEntryList _
instance showPutEventsResultEntryList :: Show PutEventsResultEntryList where
  show = genericShow
instance decodePutEventsResultEntryList :: Decode PutEventsResultEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEventsResultEntryList :: Encode PutEventsResultEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutPermissionRequest = PutPermissionRequest 
  { "Action" :: (Action)
  , "Principal" :: (Principal)
  , "StatementId" :: (StatementId)
  }
derive instance newtypePutPermissionRequest :: Newtype PutPermissionRequest _
derive instance repGenericPutPermissionRequest :: Generic PutPermissionRequest _
instance showPutPermissionRequest :: Show PutPermissionRequest where
  show = genericShow
instance decodePutPermissionRequest :: Decode PutPermissionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutPermissionRequest :: Encode PutPermissionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRuleRequest = PutRuleRequest 
  { "Name" :: (RuleName)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "EventPattern" :: NullOrUndefined.NullOrUndefined (EventPattern)
  , "State" :: NullOrUndefined.NullOrUndefined (RuleState)
  , "Description" :: NullOrUndefined.NullOrUndefined (RuleDescription)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (RoleArn)
  }
derive instance newtypePutRuleRequest :: Newtype PutRuleRequest _
derive instance repGenericPutRuleRequest :: Generic PutRuleRequest _
instance showPutRuleRequest :: Show PutRuleRequest where
  show = genericShow
instance decodePutRuleRequest :: Decode PutRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRuleRequest :: Encode PutRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRuleResponse = PutRuleResponse 
  { "RuleArn" :: NullOrUndefined.NullOrUndefined (RuleArn)
  }
derive instance newtypePutRuleResponse :: Newtype PutRuleResponse _
derive instance repGenericPutRuleResponse :: Generic PutRuleResponse _
instance showPutRuleResponse :: Show PutRuleResponse where
  show = genericShow
instance decodePutRuleResponse :: Decode PutRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRuleResponse :: Encode PutRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutTargetsRequest = PutTargetsRequest 
  { "Rule" :: (RuleName)
  , "Targets" :: (TargetList)
  }
derive instance newtypePutTargetsRequest :: Newtype PutTargetsRequest _
derive instance repGenericPutTargetsRequest :: Generic PutTargetsRequest _
instance showPutTargetsRequest :: Show PutTargetsRequest where
  show = genericShow
instance decodePutTargetsRequest :: Decode PutTargetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutTargetsRequest :: Encode PutTargetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutTargetsResponse = PutTargetsResponse 
  { "FailedEntryCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "FailedEntries" :: NullOrUndefined.NullOrUndefined (PutTargetsResultEntryList)
  }
derive instance newtypePutTargetsResponse :: Newtype PutTargetsResponse _
derive instance repGenericPutTargetsResponse :: Generic PutTargetsResponse _
instance showPutTargetsResponse :: Show PutTargetsResponse where
  show = genericShow
instance decodePutTargetsResponse :: Decode PutTargetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutTargetsResponse :: Encode PutTargetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a target that failed to be added to a rule.</p>
newtype PutTargetsResultEntry = PutTargetsResultEntry 
  { "TargetId" :: NullOrUndefined.NullOrUndefined (TargetId)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypePutTargetsResultEntry :: Newtype PutTargetsResultEntry _
derive instance repGenericPutTargetsResultEntry :: Generic PutTargetsResultEntry _
instance showPutTargetsResultEntry :: Show PutTargetsResultEntry where
  show = genericShow
instance decodePutTargetsResultEntry :: Decode PutTargetsResultEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutTargetsResultEntry :: Encode PutTargetsResultEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutTargetsResultEntryList = PutTargetsResultEntryList (Array PutTargetsResultEntry)
derive instance newtypePutTargetsResultEntryList :: Newtype PutTargetsResultEntryList _
derive instance repGenericPutTargetsResultEntryList :: Generic PutTargetsResultEntryList _
instance showPutTargetsResultEntryList :: Show PutTargetsResultEntryList where
  show = genericShow
instance decodePutTargetsResultEntryList :: Decode PutTargetsResultEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutTargetsResultEntryList :: Encode PutTargetsResultEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemovePermissionRequest = RemovePermissionRequest 
  { "StatementId" :: (StatementId)
  }
derive instance newtypeRemovePermissionRequest :: Newtype RemovePermissionRequest _
derive instance repGenericRemovePermissionRequest :: Generic RemovePermissionRequest _
instance showRemovePermissionRequest :: Show RemovePermissionRequest where
  show = genericShow
instance decodeRemovePermissionRequest :: Decode RemovePermissionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemovePermissionRequest :: Encode RemovePermissionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTargetsRequest = RemoveTargetsRequest 
  { "Rule" :: (RuleName)
  , "Ids" :: (TargetIdList)
  }
derive instance newtypeRemoveTargetsRequest :: Newtype RemoveTargetsRequest _
derive instance repGenericRemoveTargetsRequest :: Generic RemoveTargetsRequest _
instance showRemoveTargetsRequest :: Show RemoveTargetsRequest where
  show = genericShow
instance decodeRemoveTargetsRequest :: Decode RemoveTargetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTargetsRequest :: Encode RemoveTargetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTargetsResponse = RemoveTargetsResponse 
  { "FailedEntryCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "FailedEntries" :: NullOrUndefined.NullOrUndefined (RemoveTargetsResultEntryList)
  }
derive instance newtypeRemoveTargetsResponse :: Newtype RemoveTargetsResponse _
derive instance repGenericRemoveTargetsResponse :: Generic RemoveTargetsResponse _
instance showRemoveTargetsResponse :: Show RemoveTargetsResponse where
  show = genericShow
instance decodeRemoveTargetsResponse :: Decode RemoveTargetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTargetsResponse :: Encode RemoveTargetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a target that failed to be removed from a rule.</p>
newtype RemoveTargetsResultEntry = RemoveTargetsResultEntry 
  { "TargetId" :: NullOrUndefined.NullOrUndefined (TargetId)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeRemoveTargetsResultEntry :: Newtype RemoveTargetsResultEntry _
derive instance repGenericRemoveTargetsResultEntry :: Generic RemoveTargetsResultEntry _
instance showRemoveTargetsResultEntry :: Show RemoveTargetsResultEntry where
  show = genericShow
instance decodeRemoveTargetsResultEntry :: Decode RemoveTargetsResultEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTargetsResultEntry :: Encode RemoveTargetsResultEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTargetsResultEntryList = RemoveTargetsResultEntryList (Array RemoveTargetsResultEntry)
derive instance newtypeRemoveTargetsResultEntryList :: Newtype RemoveTargetsResultEntryList _
derive instance repGenericRemoveTargetsResultEntryList :: Generic RemoveTargetsResultEntryList _
instance showRemoveTargetsResultEntryList :: Show RemoveTargetsResultEntryList where
  show = genericShow
instance decodeRemoveTargetsResultEntryList :: Decode RemoveTargetsResultEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTargetsResultEntryList :: Encode RemoveTargetsResultEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An entity that you specified does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
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


-- | <p>Contains information about a rule in Amazon CloudWatch Events.</p>
newtype Rule = Rule 
  { "Name" :: NullOrUndefined.NullOrUndefined (RuleName)
  , "Arn" :: NullOrUndefined.NullOrUndefined (RuleArn)
  , "EventPattern" :: NullOrUndefined.NullOrUndefined (EventPattern)
  , "State" :: NullOrUndefined.NullOrUndefined (RuleState)
  , "Description" :: NullOrUndefined.NullOrUndefined (RuleDescription)
  , "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (ScheduleExpression)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (RoleArn)
  }
derive instance newtypeRule :: Newtype Rule _
derive instance repGenericRule :: Generic Rule _
instance showRule :: Show Rule where
  show = genericShow
instance decodeRule :: Decode Rule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRule :: Encode Rule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleArn = RuleArn String
derive instance newtypeRuleArn :: Newtype RuleArn _
derive instance repGenericRuleArn :: Generic RuleArn _
instance showRuleArn :: Show RuleArn where
  show = genericShow
instance decodeRuleArn :: Decode RuleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleArn :: Encode RuleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleDescription = RuleDescription String
derive instance newtypeRuleDescription :: Newtype RuleDescription _
derive instance repGenericRuleDescription :: Generic RuleDescription _
instance showRuleDescription :: Show RuleDescription where
  show = genericShow
instance decodeRuleDescription :: Decode RuleDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleDescription :: Encode RuleDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleName = RuleName String
derive instance newtypeRuleName :: Newtype RuleName _
derive instance repGenericRuleName :: Generic RuleName _
instance showRuleName :: Show RuleName where
  show = genericShow
instance decodeRuleName :: Decode RuleName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleName :: Encode RuleName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleNameList = RuleNameList (Array RuleName)
derive instance newtypeRuleNameList :: Newtype RuleNameList _
derive instance repGenericRuleNameList :: Generic RuleNameList _
instance showRuleNameList :: Show RuleNameList where
  show = genericShow
instance decodeRuleNameList :: Decode RuleNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleNameList :: Encode RuleNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleResponseList = RuleResponseList (Array Rule)
derive instance newtypeRuleResponseList :: Newtype RuleResponseList _
derive instance repGenericRuleResponseList :: Generic RuleResponseList _
instance showRuleResponseList :: Show RuleResponseList where
  show = genericShow
instance decodeRuleResponseList :: Decode RuleResponseList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleResponseList :: Encode RuleResponseList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleState = RuleState String
derive instance newtypeRuleState :: Newtype RuleState _
derive instance repGenericRuleState :: Generic RuleState _
instance showRuleState :: Show RuleState where
  show = genericShow
instance decodeRuleState :: Decode RuleState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleState :: Encode RuleState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This parameter contains the criteria (either InstanceIds or a tag) used to specify which EC2 instances are to be sent the command. </p>
newtype RunCommandParameters = RunCommandParameters 
  { "RunCommandTargets" :: (RunCommandTargets)
  }
derive instance newtypeRunCommandParameters :: Newtype RunCommandParameters _
derive instance repGenericRunCommandParameters :: Generic RunCommandParameters _
instance showRunCommandParameters :: Show RunCommandParameters where
  show = genericShow
instance decodeRunCommandParameters :: Decode RunCommandParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunCommandParameters :: Encode RunCommandParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the EC2 instances that are to be sent the command, specified as key-value pairs. Each <code>RunCommandTarget</code> block can include only one key, but this key may specify multiple values.</p>
newtype RunCommandTarget = RunCommandTarget 
  { "Key" :: (RunCommandTargetKey)
  , "Values" :: (RunCommandTargetValues)
  }
derive instance newtypeRunCommandTarget :: Newtype RunCommandTarget _
derive instance repGenericRunCommandTarget :: Generic RunCommandTarget _
instance showRunCommandTarget :: Show RunCommandTarget where
  show = genericShow
instance decodeRunCommandTarget :: Decode RunCommandTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunCommandTarget :: Encode RunCommandTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RunCommandTargetKey = RunCommandTargetKey String
derive instance newtypeRunCommandTargetKey :: Newtype RunCommandTargetKey _
derive instance repGenericRunCommandTargetKey :: Generic RunCommandTargetKey _
instance showRunCommandTargetKey :: Show RunCommandTargetKey where
  show = genericShow
instance decodeRunCommandTargetKey :: Decode RunCommandTargetKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunCommandTargetKey :: Encode RunCommandTargetKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RunCommandTargetValue = RunCommandTargetValue String
derive instance newtypeRunCommandTargetValue :: Newtype RunCommandTargetValue _
derive instance repGenericRunCommandTargetValue :: Generic RunCommandTargetValue _
instance showRunCommandTargetValue :: Show RunCommandTargetValue where
  show = genericShow
instance decodeRunCommandTargetValue :: Decode RunCommandTargetValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunCommandTargetValue :: Encode RunCommandTargetValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RunCommandTargetValues = RunCommandTargetValues (Array RunCommandTargetValue)
derive instance newtypeRunCommandTargetValues :: Newtype RunCommandTargetValues _
derive instance repGenericRunCommandTargetValues :: Generic RunCommandTargetValues _
instance showRunCommandTargetValues :: Show RunCommandTargetValues where
  show = genericShow
instance decodeRunCommandTargetValues :: Decode RunCommandTargetValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunCommandTargetValues :: Encode RunCommandTargetValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RunCommandTargets = RunCommandTargets (Array RunCommandTarget)
derive instance newtypeRunCommandTargets :: Newtype RunCommandTargets _
derive instance repGenericRunCommandTargets :: Generic RunCommandTargets _
instance showRunCommandTargets :: Show RunCommandTargets where
  show = genericShow
instance decodeRunCommandTargets :: Decode RunCommandTargets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunCommandTargets :: Encode RunCommandTargets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScheduleExpression = ScheduleExpression String
derive instance newtypeScheduleExpression :: Newtype ScheduleExpression _
derive instance repGenericScheduleExpression :: Generic ScheduleExpression _
instance showScheduleExpression :: Show ScheduleExpression where
  show = genericShow
instance decodeScheduleExpression :: Decode ScheduleExpression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleExpression :: Encode ScheduleExpression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatementId = StatementId String
derive instance newtypeStatementId :: Newtype StatementId _
derive instance repGenericStatementId :: Generic StatementId _
instance showStatementId :: Show StatementId where
  show = genericShow
instance decodeStatementId :: Decode StatementId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatementId :: Encode StatementId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Targets are the resources to be invoked when a rule is triggered. Target types include EC2 instances, AWS Lambda functions, Amazon Kinesis streams, Amazon ECS tasks, AWS Step Functions state machines, Run Command, and built-in targets.</p>
newtype Target = Target 
  { "Id" :: (TargetId)
  , "Arn" :: (TargetArn)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "Input" :: NullOrUndefined.NullOrUndefined (TargetInput)
  , "InputPath" :: NullOrUndefined.NullOrUndefined (TargetInputPath)
  , "InputTransformer" :: NullOrUndefined.NullOrUndefined (InputTransformer)
  , "KinesisParameters" :: NullOrUndefined.NullOrUndefined (KinesisParameters)
  , "RunCommandParameters" :: NullOrUndefined.NullOrUndefined (RunCommandParameters)
  , "EcsParameters" :: NullOrUndefined.NullOrUndefined (EcsParameters)
  }
derive instance newtypeTarget :: Newtype Target _
derive instance repGenericTarget :: Generic Target _
instance showTarget :: Show Target where
  show = genericShow
instance decodeTarget :: Decode Target where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTarget :: Encode Target where
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


newtype TargetId = TargetId String
derive instance newtypeTargetId :: Newtype TargetId _
derive instance repGenericTargetId :: Generic TargetId _
instance showTargetId :: Show TargetId where
  show = genericShow
instance decodeTargetId :: Decode TargetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetId :: Encode TargetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetIdList = TargetIdList (Array TargetId)
derive instance newtypeTargetIdList :: Newtype TargetIdList _
derive instance repGenericTargetIdList :: Generic TargetIdList _
instance showTargetIdList :: Show TargetIdList where
  show = genericShow
instance decodeTargetIdList :: Decode TargetIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetIdList :: Encode TargetIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetInput = TargetInput String
derive instance newtypeTargetInput :: Newtype TargetInput _
derive instance repGenericTargetInput :: Generic TargetInput _
instance showTargetInput :: Show TargetInput where
  show = genericShow
instance decodeTargetInput :: Decode TargetInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetInput :: Encode TargetInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetInputPath = TargetInputPath String
derive instance newtypeTargetInputPath :: Newtype TargetInputPath _
derive instance repGenericTargetInputPath :: Generic TargetInputPath _
instance showTargetInputPath :: Show TargetInputPath where
  show = genericShow
instance decodeTargetInputPath :: Decode TargetInputPath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetInputPath :: Encode TargetInputPath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetList = TargetList (Array Target)
derive instance newtypeTargetList :: Newtype TargetList _
derive instance repGenericTargetList :: Generic TargetList _
instance showTargetList :: Show TargetList where
  show = genericShow
instance decodeTargetList :: Decode TargetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetList :: Encode TargetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetPartitionKeyPath = TargetPartitionKeyPath String
derive instance newtypeTargetPartitionKeyPath :: Newtype TargetPartitionKeyPath _
derive instance repGenericTargetPartitionKeyPath :: Generic TargetPartitionKeyPath _
instance showTargetPartitionKeyPath :: Show TargetPartitionKeyPath where
  show = genericShow
instance decodeTargetPartitionKeyPath :: Decode TargetPartitionKeyPath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetPartitionKeyPath :: Encode TargetPartitionKeyPath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestEventPatternRequest = TestEventPatternRequest 
  { "EventPattern" :: (EventPattern)
  , "Event" :: (String)
  }
derive instance newtypeTestEventPatternRequest :: Newtype TestEventPatternRequest _
derive instance repGenericTestEventPatternRequest :: Generic TestEventPatternRequest _
instance showTestEventPatternRequest :: Show TestEventPatternRequest where
  show = genericShow
instance decodeTestEventPatternRequest :: Decode TestEventPatternRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestEventPatternRequest :: Encode TestEventPatternRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TestEventPatternResponse = TestEventPatternResponse 
  { "Result" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeTestEventPatternResponse :: Newtype TestEventPatternResponse _
derive instance repGenericTestEventPatternResponse :: Generic TestEventPatternResponse _
instance showTestEventPatternResponse :: Show TestEventPatternResponse where
  show = genericShow
instance decodeTestEventPatternResponse :: Decode TestEventPatternResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestEventPatternResponse :: Encode TestEventPatternResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransformerInput = TransformerInput String
derive instance newtypeTransformerInput :: Newtype TransformerInput _
derive instance repGenericTransformerInput :: Generic TransformerInput _
instance showTransformerInput :: Show TransformerInput where
  show = genericShow
instance decodeTransformerInput :: Decode TransformerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransformerInput :: Encode TransformerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransformerPaths = TransformerPaths (StrMap.StrMap TargetInputPath)
derive instance newtypeTransformerPaths :: Newtype TransformerPaths _
derive instance repGenericTransformerPaths :: Generic TransformerPaths _
instance showTransformerPaths :: Show TransformerPaths where
  show = genericShow
instance decodeTransformerPaths :: Decode TransformerPaths where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransformerPaths :: Encode TransformerPaths where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
