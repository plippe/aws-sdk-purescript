

-- | <fullname>AWS Config</fullname> <p>AWS Config provides a way to keep track of the configurations of all the AWS resources associated with your AWS account. You can use AWS Config to get the current and historical configurations of each AWS resource and also to get information about the relationship between the resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2) instance, an Elastic Block Store (EBS) volume, an Elastic network Interface (ENI), or a security group. For a complete list of resources currently supported by AWS Config, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources">Supported AWS Resources</a>.</p> <p>You can access and manage AWS Config through the AWS Management Console, the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS SDKs for AWS Config</p> <p>This reference guide contains documentation for the AWS Config API and the AWS CLI commands that you can use to manage AWS Config.</p> <p>The AWS Config API uses the Signature Version 4 protocol for signing requests. For more information about how to sign a request with this protocol, see <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>For detailed information about AWS Config features and their associated actions or commands, as well as how to work with AWS Management Console, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html">What Is AWS Config?</a> in the <i>AWS Config Developer Guide</i>.</p>
module AWS.ConfigService where

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

serviceName = "ConfigService" :: String


-- | <p>Deletes the specified AWS Config rule and all of its evaluation results.</p> <p>AWS Config sets the state of a rule to <code>DELETING</code> until the deletion is complete. You cannot update a rule while it is in this state. If you make a <code>PutConfigRule</code> or <code>DeleteConfigRule</code> request for the rule, you will receive a <code>ResourceInUseException</code>.</p> <p>You can check the state of a rule by using the <code>DescribeConfigRules</code> request.</p>
deleteConfigRule :: forall eff. DeleteConfigRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteConfigRule = Request.request serviceName "deleteConfigRule" 


-- | <p>Deletes the configuration recorder.</p> <p>After the configuration recorder is deleted, AWS Config will not record resource configuration changes until you create a new configuration recorder.</p> <p>This action does not delete the configuration information that was previously recorded. You will be able to access the previously recorded information by using the <code>GetResourceConfigHistory</code> action, but you will not be able to access this information in the AWS Config console until you create a new configuration recorder.</p>
deleteConfigurationRecorder :: forall eff. DeleteConfigurationRecorderRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteConfigurationRecorder = Request.request serviceName "deleteConfigurationRecorder" 


-- | <p>Deletes the delivery channel.</p> <p>Before you can delete the delivery channel, you must stop the configuration recorder by using the <a>StopConfigurationRecorder</a> action.</p>
deleteDeliveryChannel :: forall eff. DeleteDeliveryChannelRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDeliveryChannel = Request.request serviceName "deleteDeliveryChannel" 


-- | <p>Deletes the evaluation results for the specified Config rule. You can specify one Config rule per request. After you delete the evaluation results, you can call the <a>StartConfigRulesEvaluation</a> API to start evaluating your AWS resources against the rule.</p>
deleteEvaluationResults :: forall eff. DeleteEvaluationResultsRequest -> Aff (exception :: EXCEPTION | eff) DeleteEvaluationResultsResponse
deleteEvaluationResults = Request.request serviceName "deleteEvaluationResults" 


-- | <p>Schedules delivery of a configuration snapshot to the Amazon S3 bucket in the specified delivery channel. After the delivery has started, AWS Config sends following notifications using an Amazon SNS topic that you have specified.</p> <ul> <li> <p>Notification of starting the delivery.</p> </li> <li> <p>Notification of delivery completed, if the delivery was successfully completed.</p> </li> <li> <p>Notification of delivery failure, if the delivery failed to complete.</p> </li> </ul>
deliverConfigSnapshot :: forall eff. DeliverConfigSnapshotRequest -> Aff (exception :: EXCEPTION | eff) DeliverConfigSnapshotResponse
deliverConfigSnapshot = Request.request serviceName "deliverConfigSnapshot" 


-- | <p>Indicates whether the specified AWS Config rules are compliant. If a rule is noncompliant, this action returns the number of AWS resources that do not comply with the rule.</p> <p>A rule is compliant if all of the evaluated resources comply with it, and it is noncompliant if any of these resources do not comply.</p> <p>If AWS Config has no current evaluation results for the rule, it returns <code>INSUFFICIENT_DATA</code>. This result might indicate one of the following conditions:</p> <ul> <li> <p>AWS Config has never invoked an evaluation for the rule. To check whether it has, use the <code>DescribeConfigRuleEvaluationStatus</code> action to get the <code>LastSuccessfulInvocationTime</code> and <code>LastFailedInvocationTime</code>.</p> </li> <li> <p>The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role that you assigned to your configuration recorder includes the <code>config:PutEvaluations</code> permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the <code>config:PutEvaluations</code> permission.</p> </li> <li> <p>The rule's AWS Lambda function has returned <code>NOT_APPLICABLE</code> for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.</p> </li> </ul>
describeComplianceByConfigRule :: forall eff. DescribeComplianceByConfigRuleRequest -> Aff (exception :: EXCEPTION | eff) DescribeComplianceByConfigRuleResponse
describeComplianceByConfigRule = Request.request serviceName "describeComplianceByConfigRule" 


-- | <p>Indicates whether the specified AWS resources are compliant. If a resource is noncompliant, this action returns the number of AWS Config rules that the resource does not comply with.</p> <p>A resource is compliant if it complies with all the AWS Config rules that evaluate it. It is noncompliant if it does not comply with one or more of these rules.</p> <p>If AWS Config has no current evaluation results for the resource, it returns <code>INSUFFICIENT_DATA</code>. This result might indicate one of the following conditions about the rules that evaluate the resource:</p> <ul> <li> <p>AWS Config has never invoked an evaluation for the rule. To check whether it has, use the <code>DescribeConfigRuleEvaluationStatus</code> action to get the <code>LastSuccessfulInvocationTime</code> and <code>LastFailedInvocationTime</code>.</p> </li> <li> <p>The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role that you assigned to your configuration recorder includes the <code>config:PutEvaluations</code> permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the <code>config:PutEvaluations</code> permission.</p> </li> <li> <p>The rule's AWS Lambda function has returned <code>NOT_APPLICABLE</code> for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.</p> </li> </ul>
describeComplianceByResource :: forall eff. DescribeComplianceByResourceRequest -> Aff (exception :: EXCEPTION | eff) DescribeComplianceByResourceResponse
describeComplianceByResource = Request.request serviceName "describeComplianceByResource" 


-- | <p>Returns status information for each of your AWS managed Config rules. The status includes information such as the last time AWS Config invoked the rule, the last time AWS Config failed to invoke the rule, and the related error for the last failure.</p>
describeConfigRuleEvaluationStatus :: forall eff. DescribeConfigRuleEvaluationStatusRequest -> Aff (exception :: EXCEPTION | eff) DescribeConfigRuleEvaluationStatusResponse
describeConfigRuleEvaluationStatus = Request.request serviceName "describeConfigRuleEvaluationStatus" 


-- | <p>Returns details about your AWS Config rules.</p>
describeConfigRules :: forall eff. DescribeConfigRulesRequest -> Aff (exception :: EXCEPTION | eff) DescribeConfigRulesResponse
describeConfigRules = Request.request serviceName "describeConfigRules" 


-- | <p>Returns the current status of the specified configuration recorder. If a configuration recorder is not specified, this action returns the status of all configuration recorder associated with the account.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> </note>
describeConfigurationRecorderStatus :: forall eff. DescribeConfigurationRecorderStatusRequest -> Aff (exception :: EXCEPTION | eff) DescribeConfigurationRecorderStatusResponse
describeConfigurationRecorderStatus = Request.request serviceName "describeConfigurationRecorderStatus" 


-- | <p>Returns the details for the specified configuration recorders. If the configuration recorder is not specified, this action returns the details for all configuration recorders associated with the account.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> </note>
describeConfigurationRecorders :: forall eff. DescribeConfigurationRecordersRequest -> Aff (exception :: EXCEPTION | eff) DescribeConfigurationRecordersResponse
describeConfigurationRecorders = Request.request serviceName "describeConfigurationRecorders" 


-- | <p>Returns the current status of the specified delivery channel. If a delivery channel is not specified, this action returns the current status of all delivery channels associated with the account.</p> <note> <p>Currently, you can specify only one delivery channel per region in your account.</p> </note>
describeDeliveryChannelStatus :: forall eff. DescribeDeliveryChannelStatusRequest -> Aff (exception :: EXCEPTION | eff) DescribeDeliveryChannelStatusResponse
describeDeliveryChannelStatus = Request.request serviceName "describeDeliveryChannelStatus" 


-- | <p>Returns details about the specified delivery channel. If a delivery channel is not specified, this action returns the details of all delivery channels associated with the account.</p> <note> <p>Currently, you can specify only one delivery channel per region in your account.</p> </note>
describeDeliveryChannels :: forall eff. DescribeDeliveryChannelsRequest -> Aff (exception :: EXCEPTION | eff) DescribeDeliveryChannelsResponse
describeDeliveryChannels = Request.request serviceName "describeDeliveryChannels" 


-- | <p>Returns the evaluation results for the specified AWS Config rule. The results indicate which AWS resources were evaluated by the rule, when each resource was last evaluated, and whether each resource complies with the rule.</p>
getComplianceDetailsByConfigRule :: forall eff. GetComplianceDetailsByConfigRuleRequest -> Aff (exception :: EXCEPTION | eff) GetComplianceDetailsByConfigRuleResponse
getComplianceDetailsByConfigRule = Request.request serviceName "getComplianceDetailsByConfigRule" 


-- | <p>Returns the evaluation results for the specified AWS resource. The results indicate which AWS Config rules were used to evaluate the resource, when each rule was last used, and whether the resource complies with each rule.</p>
getComplianceDetailsByResource :: forall eff. GetComplianceDetailsByResourceRequest -> Aff (exception :: EXCEPTION | eff) GetComplianceDetailsByResourceResponse
getComplianceDetailsByResource = Request.request serviceName "getComplianceDetailsByResource" 


-- | <p>Returns the number of AWS Config rules that are compliant and noncompliant, up to a maximum of 25 for each.</p>
getComplianceSummaryByConfigRule :: forall eff.  Aff (exception :: EXCEPTION | eff) GetComplianceSummaryByConfigRuleResponse
getComplianceSummaryByConfigRule = Request.request serviceName "getComplianceSummaryByConfigRule" (Types.NoInput unit)


-- | <p>Returns the number of resources that are compliant and the number that are noncompliant. You can specify one or more resource types to get these numbers for each resource type. The maximum number returned is 100.</p>
getComplianceSummaryByResourceType :: forall eff. GetComplianceSummaryByResourceTypeRequest -> Aff (exception :: EXCEPTION | eff) GetComplianceSummaryByResourceTypeResponse
getComplianceSummaryByResourceType = Request.request serviceName "getComplianceSummaryByResourceType" 


-- | <p>Returns the resource types, the number of each resource type, and the total number of resources that AWS Config is recording in this region for your AWS account. </p> <p class="title"> <b>Example</b> </p> <ol> <li> <p>AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets.</p> </li> <li> <p>You make a call to the <code>GetDiscoveredResourceCounts</code> action and specify that you want all resource types. </p> </li> <li> <p>AWS Config returns the following:</p> <ul> <li> <p>The resource types (EC2 instances, IAM users, and S3 buckets)</p> </li> <li> <p>The number of each resource type (25, 20, and 15)</p> </li> <li> <p>The total number of all resources (60)</p> </li> </ul> </li> </ol> <p>The response is paginated. By default, AWS Config lists 100 <a>ResourceCount</a> objects on each page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p> <note> <p>If you make a call to the <a>GetDiscoveredResourceCounts</a> action, you may not immediately receive resource counts in the following situations:</p> <ul> <li> <p>You are a new AWS Config customer</p> </li> <li> <p>You just enabled resource recording</p> </li> </ul> <p>It may take a few minutes for AWS Config to record and count your resources. Wait a few minutes and then retry the <a>GetDiscoveredResourceCounts</a> action. </p> </note>
getDiscoveredResourceCounts :: forall eff. GetDiscoveredResourceCountsRequest -> Aff (exception :: EXCEPTION | eff) GetDiscoveredResourceCountsResponse
getDiscoveredResourceCounts = Request.request serviceName "getDiscoveredResourceCounts" 


-- | <p>Returns a list of configuration items for the specified resource. The list contains details about each state of the resource during the specified time interval.</p> <p>The response is paginated. By default, AWS Config returns a limit of 10 configuration items per page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p> <note> <p>Each call to the API is limited to span a duration of seven days. It is likely that the number of records returned is smaller than the specified <code>limit</code>. In such cases, you can make another call, using the <code>nextToken</code>.</p> </note>
getResourceConfigHistory :: forall eff. GetResourceConfigHistoryRequest -> Aff (exception :: EXCEPTION | eff) GetResourceConfigHistoryResponse
getResourceConfigHistory = Request.request serviceName "getResourceConfigHistory" 


-- | <p>Accepts a resource type and returns a list of resource identifiers for the resources of that type. A resource identifier includes the resource type, ID, and (if available) the custom resource name. The results consist of resources that AWS Config has discovered, including those that AWS Config is not currently recording. You can narrow the results to include only resources that have specific resource IDs or a resource name.</p> <note> <p>You can specify either resource IDs or a resource name but not both in the same request.</p> </note> <p>The response is paginated. By default, AWS Config lists 100 resource identifiers on each page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p>
listDiscoveredResources :: forall eff. ListDiscoveredResourcesRequest -> Aff (exception :: EXCEPTION | eff) ListDiscoveredResourcesResponse
listDiscoveredResources = Request.request serviceName "listDiscoveredResources" 


-- | <p>Adds or updates an AWS Config rule for evaluating whether your AWS resources comply with your desired configurations.</p> <p>You can use this action for custom Config rules and AWS managed Config rules. A custom Config rule is a rule that you develop and maintain. An AWS managed Config rule is a customizable, predefined rule that AWS Config provides.</p> <p>If you are adding a new custom Config rule, you must first create the AWS Lambda function that the rule invokes to evaluate your resources. When you use the <code>PutConfigRule</code> action to add the rule to AWS Config, you must specify the Amazon Resource Name (ARN) that AWS Lambda assigns to the function. Specify the ARN for the <code>SourceIdentifier</code> key. This key is part of the <code>Source</code> object, which is part of the <code>ConfigRule</code> object. </p> <p>If you are adding an AWS managed Config rule, specify the rule's identifier for the <code>SourceIdentifier</code> key. To reference AWS managed Config rule identifiers, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html">About AWS Managed Config Rules</a>.</p> <p>For any new rule that you add, specify the <code>ConfigRuleName</code> in the <code>ConfigRule</code> object. Do not specify the <code>ConfigRuleArn</code> or the <code>ConfigRuleId</code>. These values are generated by AWS Config for new rules.</p> <p>If you are updating a rule that you added previously, you can specify the rule by <code>ConfigRuleName</code>, <code>ConfigRuleId</code>, or <code>ConfigRuleArn</code> in the <code>ConfigRule</code> data type that you use in this request.</p> <p>The maximum number of rules that AWS Config supports is 50.</p> <p>For more information about requesting a rule limit increase, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config">AWS Config Limits</a> in the <i>AWS General Reference Guide</i>.</p> <p>For more information about developing and using AWS Config rules, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html">Evaluating AWS Resource Configurations with AWS Config</a> in the <i>AWS Config Developer Guide</i>.</p>
putConfigRule :: forall eff. PutConfigRuleRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putConfigRule = Request.request serviceName "putConfigRule" 


-- | <p>Creates a new configuration recorder to record the selected resource configurations.</p> <p>You can use this action to change the role <code>roleARN</code> and/or the <code>recordingGroup</code> of an existing recorder. To change the role, call the action on the existing configuration recorder and specify a role.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> <p>If <code>ConfigurationRecorder</code> does not have the <b>recordingGroup</b> parameter specified, the default is to record all supported resource types.</p> </note>
putConfigurationRecorder :: forall eff. PutConfigurationRecorderRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putConfigurationRecorder = Request.request serviceName "putConfigurationRecorder" 


-- | <p>Creates a delivery channel object to deliver configuration information to an Amazon S3 bucket and Amazon SNS topic.</p> <p>Before you can create a delivery channel, you must create a configuration recorder.</p> <p>You can use this action to change the Amazon S3 bucket or an Amazon SNS topic of the existing delivery channel. To change the Amazon S3 bucket or an Amazon SNS topic, call this action and specify the changed values for the S3 bucket and the SNS topic. If you specify a different value for either the S3 bucket or the SNS topic, this action will keep the existing value for the parameter that is not changed.</p> <note> <p>You can have only one delivery channel per region in your account.</p> </note>
putDeliveryChannel :: forall eff. PutDeliveryChannelRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putDeliveryChannel = Request.request serviceName "putDeliveryChannel" 


-- | <p>Used by an AWS Lambda function to deliver evaluation results to AWS Config. This action is required in every AWS Lambda function that is invoked by an AWS Config rule.</p>
putEvaluations :: forall eff. PutEvaluationsRequest -> Aff (exception :: EXCEPTION | eff) PutEvaluationsResponse
putEvaluations = Request.request serviceName "putEvaluations" 


-- | <p>Runs an on-demand evaluation for the specified Config rules against the last known configuration state of the resources. Use <code>StartConfigRulesEvaluation</code> when you want to test a rule that you updated is working as expected. <code>StartConfigRulesEvaluation</code> does not re-record the latest configuration state for your resources; it re-runs an evaluation against the last known state of your resources. </p> <p>You can specify up to 25 Config rules per request. </p> <p>An existing <code>StartConfigRulesEvaluation</code> call must complete for the specified rules before you can call the API again. If you chose to have AWS Config stream to an Amazon SNS topic, you will receive a <code>ConfigRuleEvaluationStarted</code> notification when the evaluation starts.</p> <note> <p>You don't need to call the <code>StartConfigRulesEvaluation</code> API to run an evaluation for a new rule. When you create a new rule, AWS Config automatically evaluates your resources against the rule. </p> </note> <p>The <code>StartConfigRulesEvaluation</code> API is useful if you want to run on-demand evaluations, such as the following example:</p> <ol> <li> <p>You have a custom rule that evaluates your IAM resources every 24 hours.</p> </li> <li> <p>You update your Lambda function to add additional conditions to your rule.</p> </li> <li> <p>Instead of waiting for the next periodic evaluation, you call the <code>StartConfigRulesEvaluation</code> API.</p> </li> <li> <p>AWS Config invokes your Lambda function and evaluates your IAM resources.</p> </li> <li> <p>Your custom rule will still run periodic evaluations every 24 hours.</p> </li> </ol>
startConfigRulesEvaluation :: forall eff. StartConfigRulesEvaluationRequest -> Aff (exception :: EXCEPTION | eff) StartConfigRulesEvaluationResponse
startConfigRulesEvaluation = Request.request serviceName "startConfigRulesEvaluation" 


-- | <p>Starts recording configurations of the AWS resources you have selected to record in your AWS account.</p> <p>You must have created at least one delivery channel to successfully start the configuration recorder.</p>
startConfigurationRecorder :: forall eff. StartConfigurationRecorderRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
startConfigurationRecorder = Request.request serviceName "startConfigurationRecorder" 


-- | <p>Stops recording configurations of the AWS resources you have selected to record in your AWS account.</p>
stopConfigurationRecorder :: forall eff. StopConfigurationRecorderRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
stopConfigurationRecorder = Request.request serviceName "stopConfigurationRecorder" 


newtype ARN = ARN String
derive instance newtypeARN :: Newtype ARN _
derive instance repGenericARN :: Generic ARN _
instance showARN :: Show ARN where
  show = genericShow
instance decodeARN :: Decode ARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeARN :: Encode ARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where
  show = genericShow
instance decodeAccountId :: Decode AccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId :: Encode AccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllSupported = AllSupported Boolean
derive instance newtypeAllSupported :: Newtype AllSupported _
derive instance repGenericAllSupported :: Generic AllSupported _
instance showAllSupported :: Show AllSupported where
  show = genericShow
instance decodeAllSupported :: Decode AllSupported where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllSupported :: Encode AllSupported where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZone = AvailabilityZone String
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _
derive instance repGenericAvailabilityZone :: Generic AvailabilityZone _
instance showAvailabilityZone :: Show AvailabilityZone where
  show = genericShow
instance decodeAvailabilityZone :: Decode AvailabilityZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZone :: Encode AvailabilityZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AwsRegion = AwsRegion String
derive instance newtypeAwsRegion :: Newtype AwsRegion _
derive instance repGenericAwsRegion :: Generic AwsRegion _
instance showAwsRegion :: Show AwsRegion where
  show = genericShow
instance decodeAwsRegion :: Decode AwsRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsRegion :: Encode AwsRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BaseResourceId = BaseResourceId String
derive instance newtypeBaseResourceId :: Newtype BaseResourceId _
derive instance repGenericBaseResourceId :: Generic BaseResourceId _
instance showBaseResourceId :: Show BaseResourceId where
  show = genericShow
instance decodeBaseResourceId :: Decode BaseResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBaseResourceId :: Encode BaseResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChannelName = ChannelName String
derive instance newtypeChannelName :: Newtype ChannelName _
derive instance repGenericChannelName :: Generic ChannelName _
instance showChannelName :: Show ChannelName where
  show = genericShow
instance decodeChannelName :: Decode ChannelName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChannelName :: Encode ChannelName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChronologicalOrder = ChronologicalOrder String
derive instance newtypeChronologicalOrder :: Newtype ChronologicalOrder _
derive instance repGenericChronologicalOrder :: Generic ChronologicalOrder _
instance showChronologicalOrder :: Show ChronologicalOrder where
  show = genericShow
instance decodeChronologicalOrder :: Decode ChronologicalOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChronologicalOrder :: Encode ChronologicalOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.</p>
newtype Compliance = Compliance 
  { "ComplianceType" :: NullOrUndefined.NullOrUndefined (ComplianceType)
  , "ComplianceContributorCount" :: NullOrUndefined.NullOrUndefined (ComplianceContributorCount)
  }
derive instance newtypeCompliance :: Newtype Compliance _
derive instance repGenericCompliance :: Generic Compliance _
instance showCompliance :: Show Compliance where
  show = genericShow
instance decodeCompliance :: Decode Compliance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompliance :: Encode Compliance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether an AWS Config rule is compliant. A rule is compliant if all of the resources that the rule evaluated comply with it, and it is noncompliant if any of these resources do not comply.</p>
newtype ComplianceByConfigRule = ComplianceByConfigRule 
  { "ConfigRuleName" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit64)
  , "Compliance" :: NullOrUndefined.NullOrUndefined (Compliance)
  }
derive instance newtypeComplianceByConfigRule :: Newtype ComplianceByConfigRule _
derive instance repGenericComplianceByConfigRule :: Generic ComplianceByConfigRule _
instance showComplianceByConfigRule :: Show ComplianceByConfigRule where
  show = genericShow
instance decodeComplianceByConfigRule :: Decode ComplianceByConfigRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceByConfigRule :: Encode ComplianceByConfigRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceByConfigRules = ComplianceByConfigRules (Array ComplianceByConfigRule)
derive instance newtypeComplianceByConfigRules :: Newtype ComplianceByConfigRules _
derive instance repGenericComplianceByConfigRules :: Generic ComplianceByConfigRules _
instance showComplianceByConfigRules :: Show ComplianceByConfigRules where
  show = genericShow
instance decodeComplianceByConfigRules :: Decode ComplianceByConfigRules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceByConfigRules :: Encode ComplianceByConfigRules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates whether an AWS resource that is evaluated according to one or more AWS Config rules is compliant. A resource is compliant if it complies with all of the rules that evaluate it, and it is noncompliant if it does not comply with one or more of these rules.</p>
newtype ComplianceByResource = ComplianceByResource 
  { "ResourceType" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit256)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (BaseResourceId)
  , "Compliance" :: NullOrUndefined.NullOrUndefined (Compliance)
  }
derive instance newtypeComplianceByResource :: Newtype ComplianceByResource _
derive instance repGenericComplianceByResource :: Generic ComplianceByResource _
instance showComplianceByResource :: Show ComplianceByResource where
  show = genericShow
instance decodeComplianceByResource :: Decode ComplianceByResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceByResource :: Encode ComplianceByResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceByResources = ComplianceByResources (Array ComplianceByResource)
derive instance newtypeComplianceByResources :: Newtype ComplianceByResources _
derive instance repGenericComplianceByResources :: Generic ComplianceByResources _
instance showComplianceByResources :: Show ComplianceByResources where
  show = genericShow
instance decodeComplianceByResources :: Decode ComplianceByResources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceByResources :: Encode ComplianceByResources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of AWS resources or AWS Config rules responsible for the current compliance of the item, up to a maximum number.</p>
newtype ComplianceContributorCount = ComplianceContributorCount 
  { "CappedCount" :: NullOrUndefined.NullOrUndefined (Int)
  , "CapExceeded" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeComplianceContributorCount :: Newtype ComplianceContributorCount _
derive instance repGenericComplianceContributorCount :: Generic ComplianceContributorCount _
instance showComplianceContributorCount :: Show ComplianceContributorCount where
  show = genericShow
instance decodeComplianceContributorCount :: Decode ComplianceContributorCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceContributorCount :: Encode ComplianceContributorCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceResourceTypes = ComplianceResourceTypes (Array StringWithCharLimit256)
derive instance newtypeComplianceResourceTypes :: Newtype ComplianceResourceTypes _
derive instance repGenericComplianceResourceTypes :: Generic ComplianceResourceTypes _
instance showComplianceResourceTypes :: Show ComplianceResourceTypes where
  show = genericShow
instance decodeComplianceResourceTypes :: Decode ComplianceResourceTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceResourceTypes :: Encode ComplianceResourceTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceSummariesByResourceType = ComplianceSummariesByResourceType (Array ComplianceSummaryByResourceType)
derive instance newtypeComplianceSummariesByResourceType :: Newtype ComplianceSummariesByResourceType _
derive instance repGenericComplianceSummariesByResourceType :: Generic ComplianceSummariesByResourceType _
instance showComplianceSummariesByResourceType :: Show ComplianceSummariesByResourceType where
  show = genericShow
instance decodeComplianceSummariesByResourceType :: Decode ComplianceSummariesByResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceSummariesByResourceType :: Encode ComplianceSummariesByResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of AWS Config rules or AWS resources that are compliant and noncompliant.</p>
newtype ComplianceSummary = ComplianceSummary 
  { "CompliantResourceCount" :: NullOrUndefined.NullOrUndefined (ComplianceContributorCount)
  , "NonCompliantResourceCount" :: NullOrUndefined.NullOrUndefined (ComplianceContributorCount)
  , "ComplianceSummaryTimestamp" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeComplianceSummary :: Newtype ComplianceSummary _
derive instance repGenericComplianceSummary :: Generic ComplianceSummary _
instance showComplianceSummary :: Show ComplianceSummary where
  show = genericShow
instance decodeComplianceSummary :: Decode ComplianceSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceSummary :: Encode ComplianceSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of AWS resources of a specific type that are compliant or noncompliant, up to a maximum of 100 for each compliance.</p>
newtype ComplianceSummaryByResourceType = ComplianceSummaryByResourceType 
  { "ResourceType" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit256)
  , "ComplianceSummary" :: NullOrUndefined.NullOrUndefined (ComplianceSummary)
  }
derive instance newtypeComplianceSummaryByResourceType :: Newtype ComplianceSummaryByResourceType _
derive instance repGenericComplianceSummaryByResourceType :: Generic ComplianceSummaryByResourceType _
instance showComplianceSummaryByResourceType :: Show ComplianceSummaryByResourceType where
  show = genericShow
instance decodeComplianceSummaryByResourceType :: Decode ComplianceSummaryByResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceSummaryByResourceType :: Encode ComplianceSummaryByResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceType = ComplianceType String
derive instance newtypeComplianceType :: Newtype ComplianceType _
derive instance repGenericComplianceType :: Generic ComplianceType _
instance showComplianceType :: Show ComplianceType where
  show = genericShow
instance decodeComplianceType :: Decode ComplianceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceType :: Encode ComplianceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComplianceTypes = ComplianceTypes (Array ComplianceType)
derive instance newtypeComplianceTypes :: Newtype ComplianceTypes _
derive instance repGenericComplianceTypes :: Generic ComplianceTypes _
instance showComplianceTypes :: Show ComplianceTypes where
  show = genericShow
instance decodeComplianceTypes :: Decode ComplianceTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComplianceTypes :: Encode ComplianceTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides status of the delivery of the snapshot or the configuration history to the specified Amazon S3 bucket. Also provides the status of notifications about the Amazon S3 delivery to the specified Amazon SNS topic.</p>
newtype ConfigExportDeliveryInfo = ConfigExportDeliveryInfo 
  { "LastStatus'" :: NullOrUndefined.NullOrUndefined (DeliveryStatus)
  , "LastErrorCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastErrorMessage'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastAttemptTime'" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastSuccessfulTime'" :: NullOrUndefined.NullOrUndefined (Date)
  , "NextDeliveryTime'" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeConfigExportDeliveryInfo :: Newtype ConfigExportDeliveryInfo _
derive instance repGenericConfigExportDeliveryInfo :: Generic ConfigExportDeliveryInfo _
instance showConfigExportDeliveryInfo :: Show ConfigExportDeliveryInfo where
  show = genericShow
instance decodeConfigExportDeliveryInfo :: Decode ConfigExportDeliveryInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigExportDeliveryInfo :: Encode ConfigExportDeliveryInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An AWS Config rule represents an AWS Lambda function that you create for a custom rule or a predefined function for an AWS managed rule. The function evaluates configuration items to assess whether your AWS resources comply with your desired configurations. This function can run when AWS Config detects a configuration change to an AWS resource and at a periodic frequency that you choose (for example, every 24 hours).</p> <note> <p>You can use the AWS CLI and AWS SDKs if you want to create a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot. For more information, see <a>ConfigSnapshotDeliveryProperties</a>.</p> </note> <p>For more information about developing and using AWS Config rules, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html">Evaluating AWS Resource Configurations with AWS Config</a> in the <i>AWS Config Developer Guide</i>.</p>
newtype ConfigRule = ConfigRule 
  { "ConfigRuleName" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit64)
  , "ConfigRuleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "ConfigRuleId" :: NullOrUndefined.NullOrUndefined (String)
  , "Description" :: NullOrUndefined.NullOrUndefined (EmptiableStringWithCharLimit256)
  , "Scope" :: NullOrUndefined.NullOrUndefined (Scope)
  , "Source" :: (Source)
  , "InputParameters" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit1024)
  , "MaximumExecutionFrequency" :: NullOrUndefined.NullOrUndefined (MaximumExecutionFrequency)
  , "ConfigRuleState" :: NullOrUndefined.NullOrUndefined (ConfigRuleState)
  }
derive instance newtypeConfigRule :: Newtype ConfigRule _
derive instance repGenericConfigRule :: Generic ConfigRule _
instance showConfigRule :: Show ConfigRule where
  show = genericShow
instance decodeConfigRule :: Decode ConfigRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigRule :: Encode ConfigRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status information for your AWS managed Config rules. The status includes information such as the last time the rule ran, the last time it failed, and the related error for the last failure.</p> <p>This action does not return status information about custom Config rules.</p>
newtype ConfigRuleEvaluationStatus = ConfigRuleEvaluationStatus 
  { "ConfigRuleName" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit64)
  , "ConfigRuleArn" :: NullOrUndefined.NullOrUndefined (String)
  , "ConfigRuleId" :: NullOrUndefined.NullOrUndefined (String)
  , "LastSuccessfulInvocationTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastFailedInvocationTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastSuccessfulEvaluationTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastFailedEvaluationTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "FirstActivatedTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastErrorCode" :: NullOrUndefined.NullOrUndefined (String)
  , "LastErrorMessage" :: NullOrUndefined.NullOrUndefined (String)
  , "FirstEvaluationStarted" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeConfigRuleEvaluationStatus :: Newtype ConfigRuleEvaluationStatus _
derive instance repGenericConfigRuleEvaluationStatus :: Generic ConfigRuleEvaluationStatus _
instance showConfigRuleEvaluationStatus :: Show ConfigRuleEvaluationStatus where
  show = genericShow
instance decodeConfigRuleEvaluationStatus :: Decode ConfigRuleEvaluationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigRuleEvaluationStatus :: Encode ConfigRuleEvaluationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigRuleEvaluationStatusList = ConfigRuleEvaluationStatusList (Array ConfigRuleEvaluationStatus)
derive instance newtypeConfigRuleEvaluationStatusList :: Newtype ConfigRuleEvaluationStatusList _
derive instance repGenericConfigRuleEvaluationStatusList :: Generic ConfigRuleEvaluationStatusList _
instance showConfigRuleEvaluationStatusList :: Show ConfigRuleEvaluationStatusList where
  show = genericShow
instance decodeConfigRuleEvaluationStatusList :: Decode ConfigRuleEvaluationStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigRuleEvaluationStatusList :: Encode ConfigRuleEvaluationStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigRuleNames = ConfigRuleNames (Array StringWithCharLimit64)
derive instance newtypeConfigRuleNames :: Newtype ConfigRuleNames _
derive instance repGenericConfigRuleNames :: Generic ConfigRuleNames _
instance showConfigRuleNames :: Show ConfigRuleNames where
  show = genericShow
instance decodeConfigRuleNames :: Decode ConfigRuleNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigRuleNames :: Encode ConfigRuleNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigRuleState = ConfigRuleState String
derive instance newtypeConfigRuleState :: Newtype ConfigRuleState _
derive instance repGenericConfigRuleState :: Generic ConfigRuleState _
instance showConfigRuleState :: Show ConfigRuleState where
  show = genericShow
instance decodeConfigRuleState :: Decode ConfigRuleState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigRuleState :: Encode ConfigRuleState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigRules = ConfigRules (Array ConfigRule)
derive instance newtypeConfigRules :: Newtype ConfigRules _
derive instance repGenericConfigRules :: Generic ConfigRules _
instance showConfigRules :: Show ConfigRules where
  show = genericShow
instance decodeConfigRules :: Decode ConfigRules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigRules :: Encode ConfigRules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket in your delivery channel.</p> <note> <p>If you want to create a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot, see the following:</p> </note> <p>The frequency for a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot is set by one of two values, depending on which is less frequent:</p> <ul> <li> <p>The value for the <code>deliveryFrequency</code> parameter within the delivery channel configuration, which sets how often AWS Config delivers configuration snapshots. This value also sets how often AWS Config invokes evaluations for Config rules.</p> </li> <li> <p>The value for the <code>MaximumExecutionFrequency</code> parameter, which sets the maximum frequency with which AWS Config invokes evaluations for the rule. For more information, see <a>ConfigRule</a>.</p> </li> </ul> <p>If the <code>deliveryFrequency</code> value is less frequent than the <code>MaximumExecutionFrequency</code> value for a rule, AWS Config invokes the rule only as often as the <code>deliveryFrequency</code> value.</p> <ol> <li> <p>For example, you want your rule to run evaluations when AWS Config delivers the configuration snapshot.</p> </li> <li> <p>You specify the <code>MaximumExecutionFrequency</code> value for <code>Six_Hours</code>. </p> </li> <li> <p>You then specify the delivery channel <code>deliveryFrequency</code> value for <code>TwentyFour_Hours</code>.</p> </li> <li> <p>Because the value for <code>deliveryFrequency</code> is less frequent than <code>MaximumExecutionFrequency</code>, AWS Config invokes evaluations for the rule every 24 hours. </p> </li> </ol> <p>You should set the <code>MaximumExecutionFrequency</code> value to be at least as frequent as the <code>deliveryFrequency</code> value. You can view the <code>deliveryFrequency</code> value by using the <code>DescribeDeliveryChannnels</code> action.</p> <p>To update the <code>deliveryFrequency</code> with which AWS Config delivers your configuration snapshots, use the <code>PutDeliveryChannel</code> action.</p>
newtype ConfigSnapshotDeliveryProperties = ConfigSnapshotDeliveryProperties 
  { "DeliveryFrequency'" :: NullOrUndefined.NullOrUndefined (MaximumExecutionFrequency)
  }
derive instance newtypeConfigSnapshotDeliveryProperties :: Newtype ConfigSnapshotDeliveryProperties _
derive instance repGenericConfigSnapshotDeliveryProperties :: Generic ConfigSnapshotDeliveryProperties _
instance showConfigSnapshotDeliveryProperties :: Show ConfigSnapshotDeliveryProperties where
  show = genericShow
instance decodeConfigSnapshotDeliveryProperties :: Decode ConfigSnapshotDeliveryProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigSnapshotDeliveryProperties :: Encode ConfigSnapshotDeliveryProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list that contains the status of the delivery of the configuration stream notification to the Amazon SNS topic.</p>
newtype ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo 
  { "LastStatus'" :: NullOrUndefined.NullOrUndefined (DeliveryStatus)
  , "LastErrorCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastErrorMessage'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastStatusChangeTime'" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeConfigStreamDeliveryInfo :: Newtype ConfigStreamDeliveryInfo _
derive instance repGenericConfigStreamDeliveryInfo :: Generic ConfigStreamDeliveryInfo _
instance showConfigStreamDeliveryInfo :: Show ConfigStreamDeliveryInfo where
  show = genericShow
instance decodeConfigStreamDeliveryInfo :: Decode ConfigStreamDeliveryInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigStreamDeliveryInfo :: Encode ConfigStreamDeliveryInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Configuration = Configuration String
derive instance newtypeConfiguration :: Newtype Configuration _
derive instance repGenericConfiguration :: Generic Configuration _
instance showConfiguration :: Show Configuration where
  show = genericShow
instance decodeConfiguration :: Decode Configuration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfiguration :: Encode Configuration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list that contains detailed configurations of a specified resource.</p>
newtype ConfigurationItem = ConfigurationItem 
  { "Version'" :: NullOrUndefined.NullOrUndefined (Version)
  , "AccountId'" :: NullOrUndefined.NullOrUndefined (AccountId)
  , "ConfigurationItemCaptureTime'" :: NullOrUndefined.NullOrUndefined (ConfigurationItemCaptureTime)
  , "ConfigurationItemStatus'" :: NullOrUndefined.NullOrUndefined (ConfigurationItemStatus)
  , "ConfigurationStateId'" :: NullOrUndefined.NullOrUndefined (ConfigurationStateId)
  , "ConfigurationItemMD5Hash'" :: NullOrUndefined.NullOrUndefined (ConfigurationItemMD5Hash)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (ARN)
  , "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "ResourceId'" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "ResourceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "AwsRegion'" :: NullOrUndefined.NullOrUndefined (AwsRegion)
  , "AvailabilityZone'" :: NullOrUndefined.NullOrUndefined (AvailabilityZone)
  , "ResourceCreationTime'" :: NullOrUndefined.NullOrUndefined (ResourceCreationTime)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (Tags)
  , "RelatedEvents'" :: NullOrUndefined.NullOrUndefined (RelatedEventList)
  , "Relationships'" :: NullOrUndefined.NullOrUndefined (RelationshipList)
  , "Configuration'" :: NullOrUndefined.NullOrUndefined (Configuration)
  , "SupplementaryConfiguration'" :: NullOrUndefined.NullOrUndefined (SupplementaryConfiguration)
  }
derive instance newtypeConfigurationItem :: Newtype ConfigurationItem _
derive instance repGenericConfigurationItem :: Generic ConfigurationItem _
instance showConfigurationItem :: Show ConfigurationItem where
  show = genericShow
instance decodeConfigurationItem :: Decode ConfigurationItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationItem :: Encode ConfigurationItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationItemCaptureTime = ConfigurationItemCaptureTime Number
derive instance newtypeConfigurationItemCaptureTime :: Newtype ConfigurationItemCaptureTime _
derive instance repGenericConfigurationItemCaptureTime :: Generic ConfigurationItemCaptureTime _
instance showConfigurationItemCaptureTime :: Show ConfigurationItemCaptureTime where
  show = genericShow
instance decodeConfigurationItemCaptureTime :: Decode ConfigurationItemCaptureTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationItemCaptureTime :: Encode ConfigurationItemCaptureTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationItemList = ConfigurationItemList (Array ConfigurationItem)
derive instance newtypeConfigurationItemList :: Newtype ConfigurationItemList _
derive instance repGenericConfigurationItemList :: Generic ConfigurationItemList _
instance showConfigurationItemList :: Show ConfigurationItemList where
  show = genericShow
instance decodeConfigurationItemList :: Decode ConfigurationItemList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationItemList :: Encode ConfigurationItemList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationItemMD5Hash = ConfigurationItemMD5Hash String
derive instance newtypeConfigurationItemMD5Hash :: Newtype ConfigurationItemMD5Hash _
derive instance repGenericConfigurationItemMD5Hash :: Generic ConfigurationItemMD5Hash _
instance showConfigurationItemMD5Hash :: Show ConfigurationItemMD5Hash where
  show = genericShow
instance decodeConfigurationItemMD5Hash :: Decode ConfigurationItemMD5Hash where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationItemMD5Hash :: Encode ConfigurationItemMD5Hash where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationItemStatus = ConfigurationItemStatus String
derive instance newtypeConfigurationItemStatus :: Newtype ConfigurationItemStatus _
derive instance repGenericConfigurationItemStatus :: Generic ConfigurationItemStatus _
instance showConfigurationItemStatus :: Show ConfigurationItemStatus where
  show = genericShow
instance decodeConfigurationItemStatus :: Decode ConfigurationItemStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationItemStatus :: Encode ConfigurationItemStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object that represents the recording of configuration changes of an AWS resource.</p>
newtype ConfigurationRecorder = ConfigurationRecorder 
  { "Name'" :: NullOrUndefined.NullOrUndefined (RecorderName)
  , "RoleARN'" :: NullOrUndefined.NullOrUndefined (String)
  , "RecordingGroup'" :: NullOrUndefined.NullOrUndefined (RecordingGroup)
  }
derive instance newtypeConfigurationRecorder :: Newtype ConfigurationRecorder _
derive instance repGenericConfigurationRecorder :: Generic ConfigurationRecorder _
instance showConfigurationRecorder :: Show ConfigurationRecorder where
  show = genericShow
instance decodeConfigurationRecorder :: Decode ConfigurationRecorder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationRecorder :: Encode ConfigurationRecorder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationRecorderList = ConfigurationRecorderList (Array ConfigurationRecorder)
derive instance newtypeConfigurationRecorderList :: Newtype ConfigurationRecorderList _
derive instance repGenericConfigurationRecorderList :: Generic ConfigurationRecorderList _
instance showConfigurationRecorderList :: Show ConfigurationRecorderList where
  show = genericShow
instance decodeConfigurationRecorderList :: Decode ConfigurationRecorderList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationRecorderList :: Encode ConfigurationRecorderList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationRecorderNameList = ConfigurationRecorderNameList (Array RecorderName)
derive instance newtypeConfigurationRecorderNameList :: Newtype ConfigurationRecorderNameList _
derive instance repGenericConfigurationRecorderNameList :: Generic ConfigurationRecorderNameList _
instance showConfigurationRecorderNameList :: Show ConfigurationRecorderNameList where
  show = genericShow
instance decodeConfigurationRecorderNameList :: Decode ConfigurationRecorderNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationRecorderNameList :: Encode ConfigurationRecorderNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The current status of the configuration recorder.</p>
newtype ConfigurationRecorderStatus = ConfigurationRecorderStatus 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastStartTime'" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastStopTime'" :: NullOrUndefined.NullOrUndefined (Date)
  , "Recording'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastStatus'" :: NullOrUndefined.NullOrUndefined (RecorderStatus)
  , "LastErrorCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastErrorMessage'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastStatusChangeTime'" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeConfigurationRecorderStatus :: Newtype ConfigurationRecorderStatus _
derive instance repGenericConfigurationRecorderStatus :: Generic ConfigurationRecorderStatus _
instance showConfigurationRecorderStatus :: Show ConfigurationRecorderStatus where
  show = genericShow
instance decodeConfigurationRecorderStatus :: Decode ConfigurationRecorderStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationRecorderStatus :: Encode ConfigurationRecorderStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationRecorderStatusList = ConfigurationRecorderStatusList (Array ConfigurationRecorderStatus)
derive instance newtypeConfigurationRecorderStatusList :: Newtype ConfigurationRecorderStatusList _
derive instance repGenericConfigurationRecorderStatusList :: Generic ConfigurationRecorderStatusList _
instance showConfigurationRecorderStatusList :: Show ConfigurationRecorderStatusList where
  show = genericShow
instance decodeConfigurationRecorderStatusList :: Decode ConfigurationRecorderStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationRecorderStatusList :: Encode ConfigurationRecorderStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationStateId = ConfigurationStateId String
derive instance newtypeConfigurationStateId :: Newtype ConfigurationStateId _
derive instance repGenericConfigurationStateId :: Generic ConfigurationStateId _
instance showConfigurationStateId :: Show ConfigurationStateId where
  show = genericShow
instance decodeConfigurationStateId :: Decode ConfigurationStateId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationStateId :: Encode ConfigurationStateId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteConfigRuleRequest = DeleteConfigRuleRequest 
  { "ConfigRuleName" :: (StringWithCharLimit64)
  }
derive instance newtypeDeleteConfigRuleRequest :: Newtype DeleteConfigRuleRequest _
derive instance repGenericDeleteConfigRuleRequest :: Generic DeleteConfigRuleRequest _
instance showDeleteConfigRuleRequest :: Show DeleteConfigRuleRequest where
  show = genericShow
instance decodeDeleteConfigRuleRequest :: Decode DeleteConfigRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigRuleRequest :: Encode DeleteConfigRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The request object for the <code>DeleteConfigurationRecorder</code> action.</p>
newtype DeleteConfigurationRecorderRequest = DeleteConfigurationRecorderRequest 
  { "ConfigurationRecorderName" :: (RecorderName)
  }
derive instance newtypeDeleteConfigurationRecorderRequest :: Newtype DeleteConfigurationRecorderRequest _
derive instance repGenericDeleteConfigurationRecorderRequest :: Generic DeleteConfigurationRecorderRequest _
instance showDeleteConfigurationRecorderRequest :: Show DeleteConfigurationRecorderRequest where
  show = genericShow
instance decodeDeleteConfigurationRecorderRequest :: Decode DeleteConfigurationRecorderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigurationRecorderRequest :: Encode DeleteConfigurationRecorderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DeleteDeliveryChannel</a> action. The action accepts the following data in JSON format. </p>
newtype DeleteDeliveryChannelRequest = DeleteDeliveryChannelRequest 
  { "DeliveryChannelName" :: (ChannelName)
  }
derive instance newtypeDeleteDeliveryChannelRequest :: Newtype DeleteDeliveryChannelRequest _
derive instance repGenericDeleteDeliveryChannelRequest :: Generic DeleteDeliveryChannelRequest _
instance showDeleteDeliveryChannelRequest :: Show DeleteDeliveryChannelRequest where
  show = genericShow
instance decodeDeleteDeliveryChannelRequest :: Decode DeleteDeliveryChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDeliveryChannelRequest :: Encode DeleteDeliveryChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DeleteEvaluationResultsRequest = DeleteEvaluationResultsRequest 
  { "ConfigRuleName" :: (StringWithCharLimit64)
  }
derive instance newtypeDeleteEvaluationResultsRequest :: Newtype DeleteEvaluationResultsRequest _
derive instance repGenericDeleteEvaluationResultsRequest :: Generic DeleteEvaluationResultsRequest _
instance showDeleteEvaluationResultsRequest :: Show DeleteEvaluationResultsRequest where
  show = genericShow
instance decodeDeleteEvaluationResultsRequest :: Decode DeleteEvaluationResultsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEvaluationResultsRequest :: Encode DeleteEvaluationResultsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output when you delete the evaluation results for the specified Config rule.</p>
newtype DeleteEvaluationResultsResponse = DeleteEvaluationResultsResponse Types.NoArguments
derive instance newtypeDeleteEvaluationResultsResponse :: Newtype DeleteEvaluationResultsResponse _
derive instance repGenericDeleteEvaluationResultsResponse :: Generic DeleteEvaluationResultsResponse _
instance showDeleteEvaluationResultsResponse :: Show DeleteEvaluationResultsResponse where
  show = genericShow
instance decodeDeleteEvaluationResultsResponse :: Decode DeleteEvaluationResultsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEvaluationResultsResponse :: Encode DeleteEvaluationResultsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DeliverConfigSnapshot</a> action.</p>
newtype DeliverConfigSnapshotRequest = DeliverConfigSnapshotRequest 
  { "DeliveryChannelName'" :: (ChannelName)
  }
derive instance newtypeDeliverConfigSnapshotRequest :: Newtype DeliverConfigSnapshotRequest _
derive instance repGenericDeliverConfigSnapshotRequest :: Generic DeliverConfigSnapshotRequest _
instance showDeliverConfigSnapshotRequest :: Show DeliverConfigSnapshotRequest where
  show = genericShow
instance decodeDeliverConfigSnapshotRequest :: Decode DeliverConfigSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliverConfigSnapshotRequest :: Encode DeliverConfigSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>DeliverConfigSnapshot</a> action in JSON format.</p>
newtype DeliverConfigSnapshotResponse = DeliverConfigSnapshotResponse 
  { "ConfigSnapshotId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDeliverConfigSnapshotResponse :: Newtype DeliverConfigSnapshotResponse _
derive instance repGenericDeliverConfigSnapshotResponse :: Generic DeliverConfigSnapshotResponse _
instance showDeliverConfigSnapshotResponse :: Show DeliverConfigSnapshotResponse where
  show = genericShow
instance decodeDeliverConfigSnapshotResponse :: Decode DeliverConfigSnapshotResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliverConfigSnapshotResponse :: Encode DeliverConfigSnapshotResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The channel through which AWS Config delivers notifications and updated configuration states.</p>
newtype DeliveryChannel = DeliveryChannel 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ChannelName)
  , "S3BucketName'" :: NullOrUndefined.NullOrUndefined (String)
  , "S3KeyPrefix'" :: NullOrUndefined.NullOrUndefined (String)
  , "SnsTopicARN'" :: NullOrUndefined.NullOrUndefined (String)
  , "ConfigSnapshotDeliveryProperties'" :: NullOrUndefined.NullOrUndefined (ConfigSnapshotDeliveryProperties)
  }
derive instance newtypeDeliveryChannel :: Newtype DeliveryChannel _
derive instance repGenericDeliveryChannel :: Generic DeliveryChannel _
instance showDeliveryChannel :: Show DeliveryChannel where
  show = genericShow
instance decodeDeliveryChannel :: Decode DeliveryChannel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryChannel :: Encode DeliveryChannel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryChannelList = DeliveryChannelList (Array DeliveryChannel)
derive instance newtypeDeliveryChannelList :: Newtype DeliveryChannelList _
derive instance repGenericDeliveryChannelList :: Generic DeliveryChannelList _
instance showDeliveryChannelList :: Show DeliveryChannelList where
  show = genericShow
instance decodeDeliveryChannelList :: Decode DeliveryChannelList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryChannelList :: Encode DeliveryChannelList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryChannelNameList = DeliveryChannelNameList (Array ChannelName)
derive instance newtypeDeliveryChannelNameList :: Newtype DeliveryChannelNameList _
derive instance repGenericDeliveryChannelNameList :: Generic DeliveryChannelNameList _
instance showDeliveryChannelNameList :: Show DeliveryChannelNameList where
  show = genericShow
instance decodeDeliveryChannelNameList :: Decode DeliveryChannelNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryChannelNameList :: Encode DeliveryChannelNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The status of a specified delivery channel.</p> <p>Valid values: <code>Success</code> | <code>Failure</code> </p>
newtype DeliveryChannelStatus = DeliveryChannelStatus 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "ConfigSnapshotDeliveryInfo'" :: NullOrUndefined.NullOrUndefined (ConfigExportDeliveryInfo)
  , "ConfigHistoryDeliveryInfo'" :: NullOrUndefined.NullOrUndefined (ConfigExportDeliveryInfo)
  , "ConfigStreamDeliveryInfo'" :: NullOrUndefined.NullOrUndefined (ConfigStreamDeliveryInfo)
  }
derive instance newtypeDeliveryChannelStatus :: Newtype DeliveryChannelStatus _
derive instance repGenericDeliveryChannelStatus :: Generic DeliveryChannelStatus _
instance showDeliveryChannelStatus :: Show DeliveryChannelStatus where
  show = genericShow
instance decodeDeliveryChannelStatus :: Decode DeliveryChannelStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryChannelStatus :: Encode DeliveryChannelStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryChannelStatusList = DeliveryChannelStatusList (Array DeliveryChannelStatus)
derive instance newtypeDeliveryChannelStatusList :: Newtype DeliveryChannelStatusList _
derive instance repGenericDeliveryChannelStatusList :: Generic DeliveryChannelStatusList _
instance showDeliveryChannelStatusList :: Show DeliveryChannelStatusList where
  show = genericShow
instance decodeDeliveryChannelStatusList :: Decode DeliveryChannelStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryChannelStatusList :: Encode DeliveryChannelStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStatus = DeliveryStatus String
derive instance newtypeDeliveryStatus :: Newtype DeliveryStatus _
derive instance repGenericDeliveryStatus :: Generic DeliveryStatus _
instance showDeliveryStatus :: Show DeliveryStatus where
  show = genericShow
instance decodeDeliveryStatus :: Decode DeliveryStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStatus :: Encode DeliveryStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeComplianceByConfigRuleRequest = DescribeComplianceByConfigRuleRequest 
  { "ConfigRuleNames" :: NullOrUndefined.NullOrUndefined (ConfigRuleNames)
  , "ComplianceTypes" :: NullOrUndefined.NullOrUndefined (ComplianceTypes)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeComplianceByConfigRuleRequest :: Newtype DescribeComplianceByConfigRuleRequest _
derive instance repGenericDescribeComplianceByConfigRuleRequest :: Generic DescribeComplianceByConfigRuleRequest _
instance showDescribeComplianceByConfigRuleRequest :: Show DescribeComplianceByConfigRuleRequest where
  show = genericShow
instance decodeDescribeComplianceByConfigRuleRequest :: Decode DescribeComplianceByConfigRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeComplianceByConfigRuleRequest :: Encode DescribeComplianceByConfigRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeComplianceByConfigRuleResponse = DescribeComplianceByConfigRuleResponse 
  { "ComplianceByConfigRules" :: NullOrUndefined.NullOrUndefined (ComplianceByConfigRules)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeComplianceByConfigRuleResponse :: Newtype DescribeComplianceByConfigRuleResponse _
derive instance repGenericDescribeComplianceByConfigRuleResponse :: Generic DescribeComplianceByConfigRuleResponse _
instance showDescribeComplianceByConfigRuleResponse :: Show DescribeComplianceByConfigRuleResponse where
  show = genericShow
instance decodeDescribeComplianceByConfigRuleResponse :: Decode DescribeComplianceByConfigRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeComplianceByConfigRuleResponse :: Encode DescribeComplianceByConfigRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeComplianceByResourceRequest = DescribeComplianceByResourceRequest 
  { "ResourceType" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit256)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (BaseResourceId)
  , "ComplianceTypes" :: NullOrUndefined.NullOrUndefined (ComplianceTypes)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeComplianceByResourceRequest :: Newtype DescribeComplianceByResourceRequest _
derive instance repGenericDescribeComplianceByResourceRequest :: Generic DescribeComplianceByResourceRequest _
instance showDescribeComplianceByResourceRequest :: Show DescribeComplianceByResourceRequest where
  show = genericShow
instance decodeDescribeComplianceByResourceRequest :: Decode DescribeComplianceByResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeComplianceByResourceRequest :: Encode DescribeComplianceByResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeComplianceByResourceResponse = DescribeComplianceByResourceResponse 
  { "ComplianceByResources" :: NullOrUndefined.NullOrUndefined (ComplianceByResources)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeComplianceByResourceResponse :: Newtype DescribeComplianceByResourceResponse _
derive instance repGenericDescribeComplianceByResourceResponse :: Generic DescribeComplianceByResourceResponse _
instance showDescribeComplianceByResourceResponse :: Show DescribeComplianceByResourceResponse where
  show = genericShow
instance decodeDescribeComplianceByResourceResponse :: Decode DescribeComplianceByResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeComplianceByResourceResponse :: Encode DescribeComplianceByResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeConfigRuleEvaluationStatusRequest = DescribeConfigRuleEvaluationStatusRequest 
  { "ConfigRuleNames" :: NullOrUndefined.NullOrUndefined (ConfigRuleNames)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "Limit" :: NullOrUndefined.NullOrUndefined (RuleLimit)
  }
derive instance newtypeDescribeConfigRuleEvaluationStatusRequest :: Newtype DescribeConfigRuleEvaluationStatusRequest _
derive instance repGenericDescribeConfigRuleEvaluationStatusRequest :: Generic DescribeConfigRuleEvaluationStatusRequest _
instance showDescribeConfigRuleEvaluationStatusRequest :: Show DescribeConfigRuleEvaluationStatusRequest where
  show = genericShow
instance decodeDescribeConfigRuleEvaluationStatusRequest :: Decode DescribeConfigRuleEvaluationStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigRuleEvaluationStatusRequest :: Encode DescribeConfigRuleEvaluationStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse 
  { "ConfigRulesEvaluationStatus" :: NullOrUndefined.NullOrUndefined (ConfigRuleEvaluationStatusList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigRuleEvaluationStatusResponse :: Newtype DescribeConfigRuleEvaluationStatusResponse _
derive instance repGenericDescribeConfigRuleEvaluationStatusResponse :: Generic DescribeConfigRuleEvaluationStatusResponse _
instance showDescribeConfigRuleEvaluationStatusResponse :: Show DescribeConfigRuleEvaluationStatusResponse where
  show = genericShow
instance decodeDescribeConfigRuleEvaluationStatusResponse :: Decode DescribeConfigRuleEvaluationStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigRuleEvaluationStatusResponse :: Encode DescribeConfigRuleEvaluationStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeConfigRulesRequest = DescribeConfigRulesRequest 
  { "ConfigRuleNames" :: NullOrUndefined.NullOrUndefined (ConfigRuleNames)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigRulesRequest :: Newtype DescribeConfigRulesRequest _
derive instance repGenericDescribeConfigRulesRequest :: Generic DescribeConfigRulesRequest _
instance showDescribeConfigRulesRequest :: Show DescribeConfigRulesRequest where
  show = genericShow
instance decodeDescribeConfigRulesRequest :: Decode DescribeConfigRulesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigRulesRequest :: Encode DescribeConfigRulesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype DescribeConfigRulesResponse = DescribeConfigRulesResponse 
  { "ConfigRules" :: NullOrUndefined.NullOrUndefined (ConfigRules)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigRulesResponse :: Newtype DescribeConfigRulesResponse _
derive instance repGenericDescribeConfigRulesResponse :: Generic DescribeConfigRulesResponse _
instance showDescribeConfigRulesResponse :: Show DescribeConfigRulesResponse where
  show = genericShow
instance decodeDescribeConfigRulesResponse :: Decode DescribeConfigRulesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigRulesResponse :: Encode DescribeConfigRulesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DescribeConfigurationRecorderStatus</a> action.</p>
newtype DescribeConfigurationRecorderStatusRequest = DescribeConfigurationRecorderStatusRequest 
  { "ConfigurationRecorderNames" :: NullOrUndefined.NullOrUndefined (ConfigurationRecorderNameList)
  }
derive instance newtypeDescribeConfigurationRecorderStatusRequest :: Newtype DescribeConfigurationRecorderStatusRequest _
derive instance repGenericDescribeConfigurationRecorderStatusRequest :: Generic DescribeConfigurationRecorderStatusRequest _
instance showDescribeConfigurationRecorderStatusRequest :: Show DescribeConfigurationRecorderStatusRequest where
  show = genericShow
instance decodeDescribeConfigurationRecorderStatusRequest :: Decode DescribeConfigurationRecorderStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationRecorderStatusRequest :: Encode DescribeConfigurationRecorderStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>DescribeConfigurationRecorderStatus</a> action in JSON format.</p>
newtype DescribeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse 
  { "ConfigurationRecordersStatus" :: NullOrUndefined.NullOrUndefined (ConfigurationRecorderStatusList)
  }
derive instance newtypeDescribeConfigurationRecorderStatusResponse :: Newtype DescribeConfigurationRecorderStatusResponse _
derive instance repGenericDescribeConfigurationRecorderStatusResponse :: Generic DescribeConfigurationRecorderStatusResponse _
instance showDescribeConfigurationRecorderStatusResponse :: Show DescribeConfigurationRecorderStatusResponse where
  show = genericShow
instance decodeDescribeConfigurationRecorderStatusResponse :: Decode DescribeConfigurationRecorderStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationRecorderStatusResponse :: Encode DescribeConfigurationRecorderStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DescribeConfigurationRecorders</a> action.</p>
newtype DescribeConfigurationRecordersRequest = DescribeConfigurationRecordersRequest 
  { "ConfigurationRecorderNames" :: NullOrUndefined.NullOrUndefined (ConfigurationRecorderNameList)
  }
derive instance newtypeDescribeConfigurationRecordersRequest :: Newtype DescribeConfigurationRecordersRequest _
derive instance repGenericDescribeConfigurationRecordersRequest :: Generic DescribeConfigurationRecordersRequest _
instance showDescribeConfigurationRecordersRequest :: Show DescribeConfigurationRecordersRequest where
  show = genericShow
instance decodeDescribeConfigurationRecordersRequest :: Decode DescribeConfigurationRecordersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationRecordersRequest :: Encode DescribeConfigurationRecordersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>DescribeConfigurationRecorders</a> action.</p>
newtype DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse 
  { "ConfigurationRecorders" :: NullOrUndefined.NullOrUndefined (ConfigurationRecorderList)
  }
derive instance newtypeDescribeConfigurationRecordersResponse :: Newtype DescribeConfigurationRecordersResponse _
derive instance repGenericDescribeConfigurationRecordersResponse :: Generic DescribeConfigurationRecordersResponse _
instance showDescribeConfigurationRecordersResponse :: Show DescribeConfigurationRecordersResponse where
  show = genericShow
instance decodeDescribeConfigurationRecordersResponse :: Decode DescribeConfigurationRecordersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationRecordersResponse :: Encode DescribeConfigurationRecordersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DeliveryChannelStatus</a> action.</p>
newtype DescribeDeliveryChannelStatusRequest = DescribeDeliveryChannelStatusRequest 
  { "DeliveryChannelNames" :: NullOrUndefined.NullOrUndefined (DeliveryChannelNameList)
  }
derive instance newtypeDescribeDeliveryChannelStatusRequest :: Newtype DescribeDeliveryChannelStatusRequest _
derive instance repGenericDescribeDeliveryChannelStatusRequest :: Generic DescribeDeliveryChannelStatusRequest _
instance showDescribeDeliveryChannelStatusRequest :: Show DescribeDeliveryChannelStatusRequest where
  show = genericShow
instance decodeDescribeDeliveryChannelStatusRequest :: Decode DescribeDeliveryChannelStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDeliveryChannelStatusRequest :: Encode DescribeDeliveryChannelStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>DescribeDeliveryChannelStatus</a> action.</p>
newtype DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse 
  { "DeliveryChannelsStatus" :: NullOrUndefined.NullOrUndefined (DeliveryChannelStatusList)
  }
derive instance newtypeDescribeDeliveryChannelStatusResponse :: Newtype DescribeDeliveryChannelStatusResponse _
derive instance repGenericDescribeDeliveryChannelStatusResponse :: Generic DescribeDeliveryChannelStatusResponse _
instance showDescribeDeliveryChannelStatusResponse :: Show DescribeDeliveryChannelStatusResponse where
  show = genericShow
instance decodeDescribeDeliveryChannelStatusResponse :: Decode DescribeDeliveryChannelStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDeliveryChannelStatusResponse :: Encode DescribeDeliveryChannelStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>DescribeDeliveryChannels</a> action.</p>
newtype DescribeDeliveryChannelsRequest = DescribeDeliveryChannelsRequest 
  { "DeliveryChannelNames" :: NullOrUndefined.NullOrUndefined (DeliveryChannelNameList)
  }
derive instance newtypeDescribeDeliveryChannelsRequest :: Newtype DescribeDeliveryChannelsRequest _
derive instance repGenericDescribeDeliveryChannelsRequest :: Generic DescribeDeliveryChannelsRequest _
instance showDescribeDeliveryChannelsRequest :: Show DescribeDeliveryChannelsRequest where
  show = genericShow
instance decodeDescribeDeliveryChannelsRequest :: Decode DescribeDeliveryChannelsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDeliveryChannelsRequest :: Encode DescribeDeliveryChannelsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>DescribeDeliveryChannels</a> action.</p>
newtype DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse 
  { "DeliveryChannels" :: NullOrUndefined.NullOrUndefined (DeliveryChannelList)
  }
derive instance newtypeDescribeDeliveryChannelsResponse :: Newtype DescribeDeliveryChannelsResponse _
derive instance repGenericDescribeDeliveryChannelsResponse :: Generic DescribeDeliveryChannelsResponse _
instance showDescribeDeliveryChannelsResponse :: Show DescribeDeliveryChannelsResponse where
  show = genericShow
instance decodeDescribeDeliveryChannelsResponse :: Decode DescribeDeliveryChannelsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDeliveryChannelsResponse :: Encode DescribeDeliveryChannelsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EarlierTime = EarlierTime Number
derive instance newtypeEarlierTime :: Newtype EarlierTime _
derive instance repGenericEarlierTime :: Generic EarlierTime _
instance showEarlierTime :: Show EarlierTime where
  show = genericShow
instance decodeEarlierTime :: Decode EarlierTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEarlierTime :: Encode EarlierTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmptiableStringWithCharLimit256 = EmptiableStringWithCharLimit256 String
derive instance newtypeEmptiableStringWithCharLimit256 :: Newtype EmptiableStringWithCharLimit256 _
derive instance repGenericEmptiableStringWithCharLimit256 :: Generic EmptiableStringWithCharLimit256 _
instance showEmptiableStringWithCharLimit256 :: Show EmptiableStringWithCharLimit256 where
  show = genericShow
instance decodeEmptiableStringWithCharLimit256 :: Decode EmptiableStringWithCharLimit256 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmptiableStringWithCharLimit256 :: Encode EmptiableStringWithCharLimit256 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies an AWS resource and indicates whether it complies with the AWS Config rule that it was evaluated against.</p>
newtype Evaluation = Evaluation 
  { "ComplianceResourceType" :: (StringWithCharLimit256)
  , "ComplianceResourceId" :: (BaseResourceId)
  , "ComplianceType" :: (ComplianceType)
  , "Annotation" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit256)
  , "OrderingTimestamp" :: (OrderingTimestamp)
  }
derive instance newtypeEvaluation :: Newtype Evaluation _
derive instance repGenericEvaluation :: Generic Evaluation _
instance showEvaluation :: Show Evaluation where
  show = genericShow
instance decodeEvaluation :: Decode Evaluation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluation :: Encode Evaluation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The details of an AWS Config evaluation. Provides the AWS resource that was evaluated, the compliance of the resource, related timestamps, and supplementary information.</p>
newtype EvaluationResult = EvaluationResult 
  { "EvaluationResultIdentifier" :: NullOrUndefined.NullOrUndefined (EvaluationResultIdentifier)
  , "ComplianceType" :: NullOrUndefined.NullOrUndefined (ComplianceType)
  , "ResultRecordedTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "ConfigRuleInvokedTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "Annotation" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit256)
  , "ResultToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEvaluationResult :: Newtype EvaluationResult _
derive instance repGenericEvaluationResult :: Generic EvaluationResult _
instance showEvaluationResult :: Show EvaluationResult where
  show = genericShow
instance decodeEvaluationResult :: Decode EvaluationResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluationResult :: Encode EvaluationResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Uniquely identifies an evaluation result.</p>
newtype EvaluationResultIdentifier = EvaluationResultIdentifier 
  { "EvaluationResultQualifier" :: NullOrUndefined.NullOrUndefined (EvaluationResultQualifier)
  , "OrderingTimestamp" :: NullOrUndefined.NullOrUndefined (Date)
  }
derive instance newtypeEvaluationResultIdentifier :: Newtype EvaluationResultIdentifier _
derive instance repGenericEvaluationResultIdentifier :: Generic EvaluationResultIdentifier _
instance showEvaluationResultIdentifier :: Show EvaluationResultIdentifier where
  show = genericShow
instance decodeEvaluationResultIdentifier :: Decode EvaluationResultIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluationResultIdentifier :: Encode EvaluationResultIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Identifies an AWS Config rule that evaluated an AWS resource, and provides the type and ID of the resource that the rule evaluated.</p>
newtype EvaluationResultQualifier = EvaluationResultQualifier 
  { "ConfigRuleName" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit64)
  , "ResourceType" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit256)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (BaseResourceId)
  }
derive instance newtypeEvaluationResultQualifier :: Newtype EvaluationResultQualifier _
derive instance repGenericEvaluationResultQualifier :: Generic EvaluationResultQualifier _
instance showEvaluationResultQualifier :: Show EvaluationResultQualifier where
  show = genericShow
instance decodeEvaluationResultQualifier :: Decode EvaluationResultQualifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluationResultQualifier :: Encode EvaluationResultQualifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EvaluationResults = EvaluationResults (Array EvaluationResult)
derive instance newtypeEvaluationResults :: Newtype EvaluationResults _
derive instance repGenericEvaluationResults :: Generic EvaluationResults _
instance showEvaluationResults :: Show EvaluationResults where
  show = genericShow
instance decodeEvaluationResults :: Decode EvaluationResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluationResults :: Encode EvaluationResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Evaluations = Evaluations (Array Evaluation)
derive instance newtypeEvaluations :: Newtype Evaluations _
derive instance repGenericEvaluations :: Generic Evaluations _
instance showEvaluations :: Show Evaluations where
  show = genericShow
instance decodeEvaluations :: Decode Evaluations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluations :: Encode Evaluations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventSource = EventSource String
derive instance newtypeEventSource :: Newtype EventSource _
derive instance repGenericEventSource :: Generic EventSource _
instance showEventSource :: Show EventSource where
  show = genericShow
instance decodeEventSource :: Decode EventSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSource :: Encode EventSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetComplianceDetailsByConfigRuleRequest = GetComplianceDetailsByConfigRuleRequest 
  { "ConfigRuleName" :: (StringWithCharLimit64)
  , "ComplianceTypes" :: NullOrUndefined.NullOrUndefined (ComplianceTypes)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetComplianceDetailsByConfigRuleRequest :: Newtype GetComplianceDetailsByConfigRuleRequest _
derive instance repGenericGetComplianceDetailsByConfigRuleRequest :: Generic GetComplianceDetailsByConfigRuleRequest _
instance showGetComplianceDetailsByConfigRuleRequest :: Show GetComplianceDetailsByConfigRuleRequest where
  show = genericShow
instance decodeGetComplianceDetailsByConfigRuleRequest :: Decode GetComplianceDetailsByConfigRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetComplianceDetailsByConfigRuleRequest :: Encode GetComplianceDetailsByConfigRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetComplianceDetailsByConfigRuleResponse = GetComplianceDetailsByConfigRuleResponse 
  { "EvaluationResults" :: NullOrUndefined.NullOrUndefined (EvaluationResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetComplianceDetailsByConfigRuleResponse :: Newtype GetComplianceDetailsByConfigRuleResponse _
derive instance repGenericGetComplianceDetailsByConfigRuleResponse :: Generic GetComplianceDetailsByConfigRuleResponse _
instance showGetComplianceDetailsByConfigRuleResponse :: Show GetComplianceDetailsByConfigRuleResponse where
  show = genericShow
instance decodeGetComplianceDetailsByConfigRuleResponse :: Decode GetComplianceDetailsByConfigRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetComplianceDetailsByConfigRuleResponse :: Encode GetComplianceDetailsByConfigRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetComplianceDetailsByResourceRequest = GetComplianceDetailsByResourceRequest 
  { "ResourceType" :: (StringWithCharLimit256)
  , "ResourceId" :: (BaseResourceId)
  , "ComplianceTypes" :: NullOrUndefined.NullOrUndefined (ComplianceTypes)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetComplianceDetailsByResourceRequest :: Newtype GetComplianceDetailsByResourceRequest _
derive instance repGenericGetComplianceDetailsByResourceRequest :: Generic GetComplianceDetailsByResourceRequest _
instance showGetComplianceDetailsByResourceRequest :: Show GetComplianceDetailsByResourceRequest where
  show = genericShow
instance decodeGetComplianceDetailsByResourceRequest :: Decode GetComplianceDetailsByResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetComplianceDetailsByResourceRequest :: Encode GetComplianceDetailsByResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetComplianceDetailsByResourceResponse = GetComplianceDetailsByResourceResponse 
  { "EvaluationResults" :: NullOrUndefined.NullOrUndefined (EvaluationResults)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeGetComplianceDetailsByResourceResponse :: Newtype GetComplianceDetailsByResourceResponse _
derive instance repGenericGetComplianceDetailsByResourceResponse :: Generic GetComplianceDetailsByResourceResponse _
instance showGetComplianceDetailsByResourceResponse :: Show GetComplianceDetailsByResourceResponse where
  show = genericShow
instance decodeGetComplianceDetailsByResourceResponse :: Decode GetComplianceDetailsByResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetComplianceDetailsByResourceResponse :: Encode GetComplianceDetailsByResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetComplianceSummaryByConfigRuleResponse = GetComplianceSummaryByConfigRuleResponse 
  { "ComplianceSummary" :: NullOrUndefined.NullOrUndefined (ComplianceSummary)
  }
derive instance newtypeGetComplianceSummaryByConfigRuleResponse :: Newtype GetComplianceSummaryByConfigRuleResponse _
derive instance repGenericGetComplianceSummaryByConfigRuleResponse :: Generic GetComplianceSummaryByConfigRuleResponse _
instance showGetComplianceSummaryByConfigRuleResponse :: Show GetComplianceSummaryByConfigRuleResponse where
  show = genericShow
instance decodeGetComplianceSummaryByConfigRuleResponse :: Decode GetComplianceSummaryByConfigRuleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetComplianceSummaryByConfigRuleResponse :: Encode GetComplianceSummaryByConfigRuleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetComplianceSummaryByResourceTypeRequest = GetComplianceSummaryByResourceTypeRequest 
  { "ResourceTypes" :: NullOrUndefined.NullOrUndefined (ResourceTypes)
  }
derive instance newtypeGetComplianceSummaryByResourceTypeRequest :: Newtype GetComplianceSummaryByResourceTypeRequest _
derive instance repGenericGetComplianceSummaryByResourceTypeRequest :: Generic GetComplianceSummaryByResourceTypeRequest _
instance showGetComplianceSummaryByResourceTypeRequest :: Show GetComplianceSummaryByResourceTypeRequest where
  show = genericShow
instance decodeGetComplianceSummaryByResourceTypeRequest :: Decode GetComplianceSummaryByResourceTypeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetComplianceSummaryByResourceTypeRequest :: Encode GetComplianceSummaryByResourceTypeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype GetComplianceSummaryByResourceTypeResponse = GetComplianceSummaryByResourceTypeResponse 
  { "ComplianceSummariesByResourceType" :: NullOrUndefined.NullOrUndefined (ComplianceSummariesByResourceType)
  }
derive instance newtypeGetComplianceSummaryByResourceTypeResponse :: Newtype GetComplianceSummaryByResourceTypeResponse _
derive instance repGenericGetComplianceSummaryByResourceTypeResponse :: Generic GetComplianceSummaryByResourceTypeResponse _
instance showGetComplianceSummaryByResourceTypeResponse :: Show GetComplianceSummaryByResourceTypeResponse where
  show = genericShow
instance decodeGetComplianceSummaryByResourceTypeResponse :: Decode GetComplianceSummaryByResourceTypeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetComplianceSummaryByResourceTypeResponse :: Encode GetComplianceSummaryByResourceTypeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiscoveredResourceCountsRequest = GetDiscoveredResourceCountsRequest 
  { "ResourceTypes'" :: NullOrUndefined.NullOrUndefined (ResourceTypes)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (Limit)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetDiscoveredResourceCountsRequest :: Newtype GetDiscoveredResourceCountsRequest _
derive instance repGenericGetDiscoveredResourceCountsRequest :: Generic GetDiscoveredResourceCountsRequest _
instance showGetDiscoveredResourceCountsRequest :: Show GetDiscoveredResourceCountsRequest where
  show = genericShow
instance decodeGetDiscoveredResourceCountsRequest :: Decode GetDiscoveredResourceCountsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiscoveredResourceCountsRequest :: Encode GetDiscoveredResourceCountsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiscoveredResourceCountsResponse = GetDiscoveredResourceCountsResponse 
  { "TotalDiscoveredResources'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ResourceCounts'" :: NullOrUndefined.NullOrUndefined (ResourceCounts)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetDiscoveredResourceCountsResponse :: Newtype GetDiscoveredResourceCountsResponse _
derive instance repGenericGetDiscoveredResourceCountsResponse :: Generic GetDiscoveredResourceCountsResponse _
instance showGetDiscoveredResourceCountsResponse :: Show GetDiscoveredResourceCountsResponse where
  show = genericShow
instance decodeGetDiscoveredResourceCountsResponse :: Decode GetDiscoveredResourceCountsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiscoveredResourceCountsResponse :: Encode GetDiscoveredResourceCountsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>GetResourceConfigHistory</a> action.</p>
newtype GetResourceConfigHistoryRequest = GetResourceConfigHistoryRequest 
  { "ResourceType'" :: (ResourceType)
  , "ResourceId'" :: (ResourceId)
  , "LaterTime'" :: NullOrUndefined.NullOrUndefined (LaterTime)
  , "EarlierTime'" :: NullOrUndefined.NullOrUndefined (EarlierTime)
  , "ChronologicalOrder'" :: NullOrUndefined.NullOrUndefined (ChronologicalOrder)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (Limit)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetResourceConfigHistoryRequest :: Newtype GetResourceConfigHistoryRequest _
derive instance repGenericGetResourceConfigHistoryRequest :: Generic GetResourceConfigHistoryRequest _
instance showGetResourceConfigHistoryRequest :: Show GetResourceConfigHistoryRequest where
  show = genericShow
instance decodeGetResourceConfigHistoryRequest :: Decode GetResourceConfigHistoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourceConfigHistoryRequest :: Encode GetResourceConfigHistoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output for the <a>GetResourceConfigHistory</a> action.</p>
newtype GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse 
  { "ConfigurationItems'" :: NullOrUndefined.NullOrUndefined (ConfigurationItemList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeGetResourceConfigHistoryResponse :: Newtype GetResourceConfigHistoryResponse _
derive instance repGenericGetResourceConfigHistoryResponse :: Generic GetResourceConfigHistoryResponse _
instance showGetResourceConfigHistoryResponse :: Show GetResourceConfigHistoryResponse where
  show = genericShow
instance decodeGetResourceConfigHistoryResponse :: Decode GetResourceConfigHistoryResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetResourceConfigHistoryResponse :: Encode GetResourceConfigHistoryResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IncludeGlobalResourceTypes = IncludeGlobalResourceTypes Boolean
derive instance newtypeIncludeGlobalResourceTypes :: Newtype IncludeGlobalResourceTypes _
derive instance repGenericIncludeGlobalResourceTypes :: Generic IncludeGlobalResourceTypes _
instance showIncludeGlobalResourceTypes :: Show IncludeGlobalResourceTypes where
  show = genericShow
instance decodeIncludeGlobalResourceTypes :: Decode IncludeGlobalResourceTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncludeGlobalResourceTypes :: Encode IncludeGlobalResourceTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your Amazon S3 bucket policy does not permit AWS Config to write to it.</p>
newtype InsufficientDeliveryPolicyException = InsufficientDeliveryPolicyException Types.NoArguments
derive instance newtypeInsufficientDeliveryPolicyException :: Newtype InsufficientDeliveryPolicyException _
derive instance repGenericInsufficientDeliveryPolicyException :: Generic InsufficientDeliveryPolicyException _
instance showInsufficientDeliveryPolicyException :: Show InsufficientDeliveryPolicyException where
  show = genericShow
instance decodeInsufficientDeliveryPolicyException :: Decode InsufficientDeliveryPolicyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientDeliveryPolicyException :: Encode InsufficientDeliveryPolicyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates one of the following errors:</p> <ul> <li> <p>The rule cannot be created because the IAM role assigned to AWS Config lacks permissions to perform the config:Put* action.</p> </li> <li> <p>The AWS Lambda function cannot be invoked. Check the function ARN, and check the function's permissions.</p> </li> </ul>
newtype InsufficientPermissionsException = InsufficientPermissionsException Types.NoArguments
derive instance newtypeInsufficientPermissionsException :: Newtype InsufficientPermissionsException _
derive instance repGenericInsufficientPermissionsException :: Generic InsufficientPermissionsException _
instance showInsufficientPermissionsException :: Show InsufficientPermissionsException where
  show = genericShow
instance decodeInsufficientPermissionsException :: Decode InsufficientPermissionsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientPermissionsException :: Encode InsufficientPermissionsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have provided a configuration recorder name that is not valid.</p>
newtype InvalidConfigurationRecorderNameException = InvalidConfigurationRecorderNameException Types.NoArguments
derive instance newtypeInvalidConfigurationRecorderNameException :: Newtype InvalidConfigurationRecorderNameException _
derive instance repGenericInvalidConfigurationRecorderNameException :: Generic InvalidConfigurationRecorderNameException _
instance showInvalidConfigurationRecorderNameException :: Show InvalidConfigurationRecorderNameException where
  show = genericShow
instance decodeInvalidConfigurationRecorderNameException :: Decode InvalidConfigurationRecorderNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidConfigurationRecorderNameException :: Encode InvalidConfigurationRecorderNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified delivery channel name is not valid.</p>
newtype InvalidDeliveryChannelNameException = InvalidDeliveryChannelNameException Types.NoArguments
derive instance newtypeInvalidDeliveryChannelNameException :: Newtype InvalidDeliveryChannelNameException _
derive instance repGenericInvalidDeliveryChannelNameException :: Generic InvalidDeliveryChannelNameException _
instance showInvalidDeliveryChannelNameException :: Show InvalidDeliveryChannelNameException where
  show = genericShow
instance decodeInvalidDeliveryChannelNameException :: Decode InvalidDeliveryChannelNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDeliveryChannelNameException :: Encode InvalidDeliveryChannelNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified limit is outside the allowable range.</p>
newtype InvalidLimitException = InvalidLimitException Types.NoArguments
derive instance newtypeInvalidLimitException :: Newtype InvalidLimitException _
derive instance repGenericInvalidLimitException :: Generic InvalidLimitException _
instance showInvalidLimitException :: Show InvalidLimitException where
  show = genericShow
instance decodeInvalidLimitException :: Decode InvalidLimitException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidLimitException :: Encode InvalidLimitException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified next token is invalid. Specify the <code>NextToken</code> string that was returned in the previous response to get the next page of results.</p>
newtype InvalidNextTokenException = InvalidNextTokenException Types.NoArguments
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more of the specified parameters are invalid. Verify that your parameters are valid and try again.</p>
newtype InvalidParameterValueException = InvalidParameterValueException Types.NoArguments
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _
derive instance repGenericInvalidParameterValueException :: Generic InvalidParameterValueException _
instance showInvalidParameterValueException :: Show InvalidParameterValueException where
  show = genericShow
instance decodeInvalidParameterValueException :: Decode InvalidParameterValueException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterValueException :: Encode InvalidParameterValueException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>AWS Config throws an exception if the recording group does not contain a valid list of resource types. Invalid values could also be incorrectly formatted.</p>
newtype InvalidRecordingGroupException = InvalidRecordingGroupException Types.NoArguments
derive instance newtypeInvalidRecordingGroupException :: Newtype InvalidRecordingGroupException _
derive instance repGenericInvalidRecordingGroupException :: Generic InvalidRecordingGroupException _
instance showInvalidRecordingGroupException :: Show InvalidRecordingGroupException where
  show = genericShow
instance decodeInvalidRecordingGroupException :: Decode InvalidRecordingGroupException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRecordingGroupException :: Encode InvalidRecordingGroupException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified <code>ResultToken</code> is invalid.</p>
newtype InvalidResultTokenException = InvalidResultTokenException Types.NoArguments
derive instance newtypeInvalidResultTokenException :: Newtype InvalidResultTokenException _
derive instance repGenericInvalidResultTokenException :: Generic InvalidResultTokenException _
instance showInvalidResultTokenException :: Show InvalidResultTokenException where
  show = genericShow
instance decodeInvalidResultTokenException :: Decode InvalidResultTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidResultTokenException :: Encode InvalidResultTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have provided a null or empty role ARN.</p>
newtype InvalidRoleException = InvalidRoleException Types.NoArguments
derive instance newtypeInvalidRoleException :: Newtype InvalidRoleException _
derive instance repGenericInvalidRoleException :: Generic InvalidRoleException _
instance showInvalidRoleException :: Show InvalidRoleException where
  show = genericShow
instance decodeInvalidRoleException :: Decode InvalidRoleException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRoleException :: Encode InvalidRoleException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Amazon S3 key prefix is not valid.</p>
newtype InvalidS3KeyPrefixException = InvalidS3KeyPrefixException Types.NoArguments
derive instance newtypeInvalidS3KeyPrefixException :: Newtype InvalidS3KeyPrefixException _
derive instance repGenericInvalidS3KeyPrefixException :: Generic InvalidS3KeyPrefixException _
instance showInvalidS3KeyPrefixException :: Show InvalidS3KeyPrefixException where
  show = genericShow
instance decodeInvalidS3KeyPrefixException :: Decode InvalidS3KeyPrefixException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidS3KeyPrefixException :: Encode InvalidS3KeyPrefixException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Amazon SNS topic does not exist.</p>
newtype InvalidSNSTopicARNException = InvalidSNSTopicARNException Types.NoArguments
derive instance newtypeInvalidSNSTopicARNException :: Newtype InvalidSNSTopicARNException _
derive instance repGenericInvalidSNSTopicARNException :: Generic InvalidSNSTopicARNException _
instance showInvalidSNSTopicARNException :: Show InvalidSNSTopicARNException where
  show = genericShow
instance decodeInvalidSNSTopicARNException :: Decode InvalidSNSTopicARNException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSNSTopicARNException :: Encode InvalidSNSTopicARNException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified time range is not valid. The earlier time is not chronologically before the later time.</p>
newtype InvalidTimeRangeException = InvalidTimeRangeException Types.NoArguments
derive instance newtypeInvalidTimeRangeException :: Newtype InvalidTimeRangeException _
derive instance repGenericInvalidTimeRangeException :: Generic InvalidTimeRangeException _
instance showInvalidTimeRangeException :: Show InvalidTimeRangeException where
  show = genericShow
instance decodeInvalidTimeRangeException :: Decode InvalidTimeRangeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTimeRangeException :: Encode InvalidTimeRangeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot delete the delivery channel you specified because the configuration recorder is running.</p>
newtype LastDeliveryChannelDeleteFailedException = LastDeliveryChannelDeleteFailedException Types.NoArguments
derive instance newtypeLastDeliveryChannelDeleteFailedException :: Newtype LastDeliveryChannelDeleteFailedException _
derive instance repGenericLastDeliveryChannelDeleteFailedException :: Generic LastDeliveryChannelDeleteFailedException _
instance showLastDeliveryChannelDeleteFailedException :: Show LastDeliveryChannelDeleteFailedException where
  show = genericShow
instance decodeLastDeliveryChannelDeleteFailedException :: Decode LastDeliveryChannelDeleteFailedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastDeliveryChannelDeleteFailedException :: Encode LastDeliveryChannelDeleteFailedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LaterTime = LaterTime Number
derive instance newtypeLaterTime :: Newtype LaterTime _
derive instance repGenericLaterTime :: Generic LaterTime _
instance showLaterTime :: Show LaterTime where
  show = genericShow
instance decodeLaterTime :: Decode LaterTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLaterTime :: Encode LaterTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Limit = Limit Int
derive instance newtypeLimit :: Newtype Limit _
derive instance repGenericLimit :: Generic Limit _
instance showLimit :: Show Limit where
  show = genericShow
instance decodeLimit :: Decode Limit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimit :: Encode Limit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown if an evaluation is in progress or if you call the <a>StartConfigRulesEvaluation</a> API more than once per minute.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ListDiscoveredResourcesRequest = ListDiscoveredResourcesRequest 
  { "ResourceType'" :: (ResourceType)
  , "ResourceIds'" :: NullOrUndefined.NullOrUndefined (ResourceIdList)
  , "ResourceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Limit'" :: NullOrUndefined.NullOrUndefined (Limit)
  , "IncludeDeletedResources'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDiscoveredResourcesRequest :: Newtype ListDiscoveredResourcesRequest _
derive instance repGenericListDiscoveredResourcesRequest :: Generic ListDiscoveredResourcesRequest _
instance showListDiscoveredResourcesRequest :: Show ListDiscoveredResourcesRequest where
  show = genericShow
instance decodeListDiscoveredResourcesRequest :: Decode ListDiscoveredResourcesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDiscoveredResourcesRequest :: Encode ListDiscoveredResourcesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse 
  { "ResourceIdentifiers'" :: NullOrUndefined.NullOrUndefined (ResourceIdentifierList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListDiscoveredResourcesResponse :: Newtype ListDiscoveredResourcesResponse _
derive instance repGenericListDiscoveredResourcesResponse :: Generic ListDiscoveredResourcesResponse _
instance showListDiscoveredResourcesResponse :: Show ListDiscoveredResourcesResponse where
  show = genericShow
instance decodeListDiscoveredResourcesResponse :: Decode ListDiscoveredResourcesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDiscoveredResourcesResponse :: Encode ListDiscoveredResourcesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Failed to add the AWS Config rule because the account already contains the maximum number of 50 rules. Consider deleting any deactivated rules before adding new rules.</p>
newtype MaxNumberOfConfigRulesExceededException = MaxNumberOfConfigRulesExceededException Types.NoArguments
derive instance newtypeMaxNumberOfConfigRulesExceededException :: Newtype MaxNumberOfConfigRulesExceededException _
derive instance repGenericMaxNumberOfConfigRulesExceededException :: Generic MaxNumberOfConfigRulesExceededException _
instance showMaxNumberOfConfigRulesExceededException :: Show MaxNumberOfConfigRulesExceededException where
  show = genericShow
instance decodeMaxNumberOfConfigRulesExceededException :: Decode MaxNumberOfConfigRulesExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxNumberOfConfigRulesExceededException :: Encode MaxNumberOfConfigRulesExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have reached the limit on the number of recorders you can create.</p>
newtype MaxNumberOfConfigurationRecordersExceededException = MaxNumberOfConfigurationRecordersExceededException Types.NoArguments
derive instance newtypeMaxNumberOfConfigurationRecordersExceededException :: Newtype MaxNumberOfConfigurationRecordersExceededException _
derive instance repGenericMaxNumberOfConfigurationRecordersExceededException :: Generic MaxNumberOfConfigurationRecordersExceededException _
instance showMaxNumberOfConfigurationRecordersExceededException :: Show MaxNumberOfConfigurationRecordersExceededException where
  show = genericShow
instance decodeMaxNumberOfConfigurationRecordersExceededException :: Decode MaxNumberOfConfigurationRecordersExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxNumberOfConfigurationRecordersExceededException :: Encode MaxNumberOfConfigurationRecordersExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have reached the limit on the number of delivery channels you can create.</p>
newtype MaxNumberOfDeliveryChannelsExceededException = MaxNumberOfDeliveryChannelsExceededException Types.NoArguments
derive instance newtypeMaxNumberOfDeliveryChannelsExceededException :: Newtype MaxNumberOfDeliveryChannelsExceededException _
derive instance repGenericMaxNumberOfDeliveryChannelsExceededException :: Generic MaxNumberOfDeliveryChannelsExceededException _
instance showMaxNumberOfDeliveryChannelsExceededException :: Show MaxNumberOfDeliveryChannelsExceededException where
  show = genericShow
instance decodeMaxNumberOfDeliveryChannelsExceededException :: Decode MaxNumberOfDeliveryChannelsExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxNumberOfDeliveryChannelsExceededException :: Encode MaxNumberOfDeliveryChannelsExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaximumExecutionFrequency = MaximumExecutionFrequency String
derive instance newtypeMaximumExecutionFrequency :: Newtype MaximumExecutionFrequency _
derive instance repGenericMaximumExecutionFrequency :: Generic MaximumExecutionFrequency _
instance showMaximumExecutionFrequency :: Show MaximumExecutionFrequency where
  show = genericShow
instance decodeMaximumExecutionFrequency :: Decode MaximumExecutionFrequency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaximumExecutionFrequency :: Encode MaximumExecutionFrequency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageType = MessageType String
derive instance newtypeMessageType :: Newtype MessageType _
derive instance repGenericMessageType :: Generic MessageType _
instance showMessageType :: Show MessageType where
  show = genericShow
instance decodeMessageType :: Decode MessageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageType :: Encode MessageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive instance repGenericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow
instance decodeName :: Decode Name where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName :: Encode Name where
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


-- | <p>There are no configuration recorders available to provide the role needed to describe your resources. Create a configuration recorder.</p>
newtype NoAvailableConfigurationRecorderException = NoAvailableConfigurationRecorderException Types.NoArguments
derive instance newtypeNoAvailableConfigurationRecorderException :: Newtype NoAvailableConfigurationRecorderException _
derive instance repGenericNoAvailableConfigurationRecorderException :: Generic NoAvailableConfigurationRecorderException _
instance showNoAvailableConfigurationRecorderException :: Show NoAvailableConfigurationRecorderException where
  show = genericShow
instance decodeNoAvailableConfigurationRecorderException :: Decode NoAvailableConfigurationRecorderException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoAvailableConfigurationRecorderException :: Encode NoAvailableConfigurationRecorderException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no delivery channel available to record configurations.</p>
newtype NoAvailableDeliveryChannelException = NoAvailableDeliveryChannelException Types.NoArguments
derive instance newtypeNoAvailableDeliveryChannelException :: Newtype NoAvailableDeliveryChannelException _
derive instance repGenericNoAvailableDeliveryChannelException :: Generic NoAvailableDeliveryChannelException _
instance showNoAvailableDeliveryChannelException :: Show NoAvailableDeliveryChannelException where
  show = genericShow
instance decodeNoAvailableDeliveryChannelException :: Decode NoAvailableDeliveryChannelException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoAvailableDeliveryChannelException :: Encode NoAvailableDeliveryChannelException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no configuration recorder running.</p>
newtype NoRunningConfigurationRecorderException = NoRunningConfigurationRecorderException Types.NoArguments
derive instance newtypeNoRunningConfigurationRecorderException :: Newtype NoRunningConfigurationRecorderException _
derive instance repGenericNoRunningConfigurationRecorderException :: Generic NoRunningConfigurationRecorderException _
instance showNoRunningConfigurationRecorderException :: Show NoRunningConfigurationRecorderException where
  show = genericShow
instance decodeNoRunningConfigurationRecorderException :: Decode NoRunningConfigurationRecorderException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoRunningConfigurationRecorderException :: Encode NoRunningConfigurationRecorderException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Amazon S3 bucket does not exist.</p>
newtype NoSuchBucketException = NoSuchBucketException Types.NoArguments
derive instance newtypeNoSuchBucketException :: Newtype NoSuchBucketException _
derive instance repGenericNoSuchBucketException :: Generic NoSuchBucketException _
instance showNoSuchBucketException :: Show NoSuchBucketException where
  show = genericShow
instance decodeNoSuchBucketException :: Decode NoSuchBucketException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchBucketException :: Encode NoSuchBucketException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more AWS Config rules in the request are invalid. Verify that the rule names are correct and try again.</p>
newtype NoSuchConfigRuleException = NoSuchConfigRuleException Types.NoArguments
derive instance newtypeNoSuchConfigRuleException :: Newtype NoSuchConfigRuleException _
derive instance repGenericNoSuchConfigRuleException :: Generic NoSuchConfigRuleException _
instance showNoSuchConfigRuleException :: Show NoSuchConfigRuleException where
  show = genericShow
instance decodeNoSuchConfigRuleException :: Decode NoSuchConfigRuleException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchConfigRuleException :: Encode NoSuchConfigRuleException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have specified a configuration recorder that does not exist.</p>
newtype NoSuchConfigurationRecorderException = NoSuchConfigurationRecorderException Types.NoArguments
derive instance newtypeNoSuchConfigurationRecorderException :: Newtype NoSuchConfigurationRecorderException _
derive instance repGenericNoSuchConfigurationRecorderException :: Generic NoSuchConfigurationRecorderException _
instance showNoSuchConfigurationRecorderException :: Show NoSuchConfigurationRecorderException where
  show = genericShow
instance decodeNoSuchConfigurationRecorderException :: Decode NoSuchConfigurationRecorderException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchConfigurationRecorderException :: Encode NoSuchConfigurationRecorderException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have specified a delivery channel that does not exist.</p>
newtype NoSuchDeliveryChannelException = NoSuchDeliveryChannelException Types.NoArguments
derive instance newtypeNoSuchDeliveryChannelException :: Newtype NoSuchDeliveryChannelException _
derive instance repGenericNoSuchDeliveryChannelException :: Generic NoSuchDeliveryChannelException _
instance showNoSuchDeliveryChannelException :: Show NoSuchDeliveryChannelException where
  show = genericShow
instance decodeNoSuchDeliveryChannelException :: Decode NoSuchDeliveryChannelException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchDeliveryChannelException :: Encode NoSuchDeliveryChannelException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderingTimestamp = OrderingTimestamp Number
derive instance newtypeOrderingTimestamp :: Newtype OrderingTimestamp _
derive instance repGenericOrderingTimestamp :: Generic OrderingTimestamp _
instance showOrderingTimestamp :: Show OrderingTimestamp where
  show = genericShow
instance decodeOrderingTimestamp :: Decode OrderingTimestamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderingTimestamp :: Encode OrderingTimestamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Owner = Owner String
derive instance newtypeOwner :: Newtype Owner _
derive instance repGenericOwner :: Generic Owner _
instance showOwner :: Show Owner where
  show = genericShow
instance decodeOwner :: Decode Owner where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOwner :: Encode Owner where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutConfigRuleRequest = PutConfigRuleRequest 
  { "ConfigRule" :: (ConfigRule)
  }
derive instance newtypePutConfigRuleRequest :: Newtype PutConfigRuleRequest _
derive instance repGenericPutConfigRuleRequest :: Generic PutConfigRuleRequest _
instance showPutConfigRuleRequest :: Show PutConfigRuleRequest where
  show = genericShow
instance decodePutConfigRuleRequest :: Decode PutConfigRuleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutConfigRuleRequest :: Encode PutConfigRuleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>PutConfigurationRecorder</a> action.</p>
newtype PutConfigurationRecorderRequest = PutConfigurationRecorderRequest 
  { "ConfigurationRecorder" :: (ConfigurationRecorder)
  }
derive instance newtypePutConfigurationRecorderRequest :: Newtype PutConfigurationRecorderRequest _
derive instance repGenericPutConfigurationRecorderRequest :: Generic PutConfigurationRecorderRequest _
instance showPutConfigurationRecorderRequest :: Show PutConfigurationRecorderRequest where
  show = genericShow
instance decodePutConfigurationRecorderRequest :: Decode PutConfigurationRecorderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutConfigurationRecorderRequest :: Encode PutConfigurationRecorderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>PutDeliveryChannel</a> action.</p>
newtype PutDeliveryChannelRequest = PutDeliveryChannelRequest 
  { "DeliveryChannel" :: (DeliveryChannel)
  }
derive instance newtypePutDeliveryChannelRequest :: Newtype PutDeliveryChannelRequest _
derive instance repGenericPutDeliveryChannelRequest :: Generic PutDeliveryChannelRequest _
instance showPutDeliveryChannelRequest :: Show PutDeliveryChannelRequest where
  show = genericShow
instance decodePutDeliveryChannelRequest :: Decode PutDeliveryChannelRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutDeliveryChannelRequest :: Encode PutDeliveryChannelRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype PutEvaluationsRequest = PutEvaluationsRequest 
  { "Evaluations" :: NullOrUndefined.NullOrUndefined (Evaluations)
  , "ResultToken" :: (String)
  , "TestMode" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypePutEvaluationsRequest :: Newtype PutEvaluationsRequest _
derive instance repGenericPutEvaluationsRequest :: Generic PutEvaluationsRequest _
instance showPutEvaluationsRequest :: Show PutEvaluationsRequest where
  show = genericShow
instance decodePutEvaluationsRequest :: Decode PutEvaluationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEvaluationsRequest :: Encode PutEvaluationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype PutEvaluationsResponse = PutEvaluationsResponse 
  { "FailedEvaluations" :: NullOrUndefined.NullOrUndefined (Evaluations)
  }
derive instance newtypePutEvaluationsResponse :: Newtype PutEvaluationsResponse _
derive instance repGenericPutEvaluationsResponse :: Generic PutEvaluationsResponse _
instance showPutEvaluationsResponse :: Show PutEvaluationsResponse where
  show = genericShow
instance decodePutEvaluationsResponse :: Decode PutEvaluationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutEvaluationsResponse :: Encode PutEvaluationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecorderName = RecorderName String
derive instance newtypeRecorderName :: Newtype RecorderName _
derive instance repGenericRecorderName :: Generic RecorderName _
instance showRecorderName :: Show RecorderName where
  show = genericShow
instance decodeRecorderName :: Decode RecorderName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecorderName :: Encode RecorderName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecorderStatus = RecorderStatus String
derive instance newtypeRecorderStatus :: Newtype RecorderStatus _
derive instance repGenericRecorderStatus :: Generic RecorderStatus _
instance showRecorderStatus :: Show RecorderStatus where
  show = genericShow
instance decodeRecorderStatus :: Decode RecorderStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecorderStatus :: Encode RecorderStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the types of AWS resource for which AWS Config records configuration changes.</p> <p>In the recording group, you specify whether all supported types or specific types of resources are recorded.</p> <p>By default, AWS Config records configuration changes for all supported types of regional resources that AWS Config discovers in the region in which it is running. Regional resources are tied to a region and can be used only in that region. Examples of regional resources are EC2 instances and EBS volumes.</p> <p>You can also have AWS Config record configuration changes for supported types of global resources (for example, IAM resources). Global resources are not tied to an individual region and can be used in all regions.</p> <important> <p>The configuration details for any global resource are the same in all regions. If you customize AWS Config in multiple regions to record global resources, it will create multiple configuration items each time a global resource changes: one configuration item for each region. These configuration items will contain identical data. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources, unless you want the configuration items to be available in multiple regions.</p> </important> <p>If you don't want AWS Config to record all resources, you can specify which types of resources it will record with the <code>resourceTypes</code> parameter.</p> <p>For a list of supported resource types, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources">Supported resource types</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/select-resources.html">Selecting Which Resources AWS Config Records</a>.</p>
newtype RecordingGroup = RecordingGroup 
  { "AllSupported'" :: NullOrUndefined.NullOrUndefined (AllSupported)
  , "IncludeGlobalResourceTypes'" :: NullOrUndefined.NullOrUndefined (IncludeGlobalResourceTypes)
  , "ResourceTypes'" :: NullOrUndefined.NullOrUndefined (ResourceTypeList)
  }
derive instance newtypeRecordingGroup :: Newtype RecordingGroup _
derive instance repGenericRecordingGroup :: Generic RecordingGroup _
instance showRecordingGroup :: Show RecordingGroup where
  show = genericShow
instance decodeRecordingGroup :: Decode RecordingGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordingGroup :: Encode RecordingGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReevaluateConfigRuleNames = ReevaluateConfigRuleNames (Array StringWithCharLimit64)
derive instance newtypeReevaluateConfigRuleNames :: Newtype ReevaluateConfigRuleNames _
derive instance repGenericReevaluateConfigRuleNames :: Generic ReevaluateConfigRuleNames _
instance showReevaluateConfigRuleNames :: Show ReevaluateConfigRuleNames where
  show = genericShow
instance decodeReevaluateConfigRuleNames :: Decode ReevaluateConfigRuleNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReevaluateConfigRuleNames :: Encode ReevaluateConfigRuleNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RelatedEvent = RelatedEvent String
derive instance newtypeRelatedEvent :: Newtype RelatedEvent _
derive instance repGenericRelatedEvent :: Generic RelatedEvent _
instance showRelatedEvent :: Show RelatedEvent where
  show = genericShow
instance decodeRelatedEvent :: Decode RelatedEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRelatedEvent :: Encode RelatedEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RelatedEventList = RelatedEventList (Array RelatedEvent)
derive instance newtypeRelatedEventList :: Newtype RelatedEventList _
derive instance repGenericRelatedEventList :: Generic RelatedEventList _
instance showRelatedEventList :: Show RelatedEventList where
  show = genericShow
instance decodeRelatedEventList :: Decode RelatedEventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRelatedEventList :: Encode RelatedEventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The relationship of the related resource to the main resource.</p>
newtype Relationship = Relationship 
  { "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "ResourceId'" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "ResourceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "RelationshipName'" :: NullOrUndefined.NullOrUndefined (RelationshipName)
  }
derive instance newtypeRelationship :: Newtype Relationship _
derive instance repGenericRelationship :: Generic Relationship _
instance showRelationship :: Show Relationship where
  show = genericShow
instance decodeRelationship :: Decode Relationship where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRelationship :: Encode Relationship where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RelationshipList = RelationshipList (Array Relationship)
derive instance newtypeRelationshipList :: Newtype RelationshipList _
derive instance repGenericRelationshipList :: Generic RelationshipList _
instance showRelationshipList :: Show RelationshipList where
  show = genericShow
instance decodeRelationshipList :: Decode RelationshipList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRelationshipList :: Encode RelationshipList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RelationshipName = RelationshipName String
derive instance newtypeRelationshipName :: Newtype RelationshipName _
derive instance repGenericRelationshipName :: Generic RelationshipName _
instance showRelationshipName :: Show RelationshipName where
  show = genericShow
instance decodeRelationshipName :: Decode RelationshipName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRelationshipName :: Encode RelationshipName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object that contains the resource type and the number of resources.</p>
newtype ResourceCount = ResourceCount 
  { "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "Count'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeResourceCount :: Newtype ResourceCount _
derive instance repGenericResourceCount :: Generic ResourceCount _
instance showResourceCount :: Show ResourceCount where
  show = genericShow
instance decodeResourceCount :: Decode ResourceCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceCount :: Encode ResourceCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceCounts = ResourceCounts (Array ResourceCount)
derive instance newtypeResourceCounts :: Newtype ResourceCounts _
derive instance repGenericResourceCounts :: Generic ResourceCounts _
instance showResourceCounts :: Show ResourceCounts where
  show = genericShow
instance decodeResourceCounts :: Decode ResourceCounts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceCounts :: Encode ResourceCounts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceCreationTime = ResourceCreationTime Number
derive instance newtypeResourceCreationTime :: Newtype ResourceCreationTime _
derive instance repGenericResourceCreationTime :: Generic ResourceCreationTime _
instance showResourceCreationTime :: Show ResourceCreationTime where
  show = genericShow
instance decodeResourceCreationTime :: Decode ResourceCreationTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceCreationTime :: Encode ResourceCreationTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDeletionTime = ResourceDeletionTime Number
derive instance newtypeResourceDeletionTime :: Newtype ResourceDeletionTime _
derive instance repGenericResourceDeletionTime :: Generic ResourceDeletionTime _
instance showResourceDeletionTime :: Show ResourceDeletionTime where
  show = genericShow
instance decodeResourceDeletionTime :: Decode ResourceDeletionTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDeletionTime :: Encode ResourceDeletionTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _
derive instance repGenericResourceId :: Generic ResourceId _
instance showResourceId :: Show ResourceId where
  show = genericShow
instance decodeResourceId :: Decode ResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceId :: Encode ResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceIdList = ResourceIdList (Array ResourceId)
derive instance newtypeResourceIdList :: Newtype ResourceIdList _
derive instance repGenericResourceIdList :: Generic ResourceIdList _
instance showResourceIdList :: Show ResourceIdList where
  show = genericShow
instance decodeResourceIdList :: Decode ResourceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceIdList :: Encode ResourceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.</p>
newtype ResourceIdentifier = ResourceIdentifier 
  { "ResourceType'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "ResourceId'" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "ResourceName'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "ResourceDeletionTime'" :: NullOrUndefined.NullOrUndefined (ResourceDeletionTime)
  }
derive instance newtypeResourceIdentifier :: Newtype ResourceIdentifier _
derive instance repGenericResourceIdentifier :: Generic ResourceIdentifier _
instance showResourceIdentifier :: Show ResourceIdentifier where
  show = genericShow
instance decodeResourceIdentifier :: Decode ResourceIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceIdentifier :: Encode ResourceIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceIdentifierList = ResourceIdentifierList (Array ResourceIdentifier)
derive instance newtypeResourceIdentifierList :: Newtype ResourceIdentifierList _
derive instance repGenericResourceIdentifierList :: Generic ResourceIdentifierList _
instance showResourceIdentifierList :: Show ResourceIdentifierList where
  show = genericShow
instance decodeResourceIdentifierList :: Decode ResourceIdentifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceIdentifierList :: Encode ResourceIdentifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The rule is currently being deleted or the rule is deleting your evaluation results. Try your request again later.</p>
newtype ResourceInUseException = ResourceInUseException Types.NoArguments
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
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


-- | <p>You have specified a resource that is either unknown or has not been discovered.</p>
newtype ResourceNotDiscoveredException = ResourceNotDiscoveredException Types.NoArguments
derive instance newtypeResourceNotDiscoveredException :: Newtype ResourceNotDiscoveredException _
derive instance repGenericResourceNotDiscoveredException :: Generic ResourceNotDiscoveredException _
instance showResourceNotDiscoveredException :: Show ResourceNotDiscoveredException where
  show = genericShow
instance decodeResourceNotDiscoveredException :: Decode ResourceNotDiscoveredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotDiscoveredException :: Encode ResourceNotDiscoveredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _
derive instance repGenericResourceType :: Generic ResourceType _
instance showResourceType :: Show ResourceType where
  show = genericShow
instance decodeResourceType :: Decode ResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceType :: Encode ResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTypeList = ResourceTypeList (Array ResourceType)
derive instance newtypeResourceTypeList :: Newtype ResourceTypeList _
derive instance repGenericResourceTypeList :: Generic ResourceTypeList _
instance showResourceTypeList :: Show ResourceTypeList where
  show = genericShow
instance decodeResourceTypeList :: Decode ResourceTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTypeList :: Encode ResourceTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTypes = ResourceTypes (Array StringWithCharLimit256)
derive instance newtypeResourceTypes :: Newtype ResourceTypes _
derive instance repGenericResourceTypes :: Generic ResourceTypes _
instance showResourceTypes :: Show ResourceTypes where
  show = genericShow
instance decodeResourceTypes :: Decode ResourceTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTypes :: Encode ResourceTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleLimit = RuleLimit Int
derive instance newtypeRuleLimit :: Newtype RuleLimit _
derive instance repGenericRuleLimit :: Generic RuleLimit _
instance showRuleLimit :: Show RuleLimit where
  show = genericShow
instance decodeRuleLimit :: Decode RuleLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleLimit :: Encode RuleLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines which resources trigger an evaluation for an AWS Config rule. The scope can include one or more resource types, a combination of a tag key and value, or a combination of one resource type and one resource ID. Specify a scope to constrain which resources trigger an evaluation for a rule. Otherwise, evaluations for the rule are triggered when any resource in your recording group changes in configuration.</p>
newtype Scope = Scope 
  { "ComplianceResourceTypes" :: NullOrUndefined.NullOrUndefined (ComplianceResourceTypes)
  , "TagKey" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit128)
  , "TagValue" :: NullOrUndefined.NullOrUndefined (StringWithCharLimit256)
  , "ComplianceResourceId" :: NullOrUndefined.NullOrUndefined (BaseResourceId)
  }
derive instance newtypeScope :: Newtype Scope _
derive instance repGenericScope :: Generic Scope _
instance showScope :: Show Scope where
  show = genericShow
instance decodeScope :: Decode Scope where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScope :: Encode Scope where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the AWS Config rule owner (AWS or customer), the rule identifier, and the events that trigger the evaluation of your AWS resources.</p>
newtype Source = Source 
  { "Owner" :: (Owner)
  , "SourceIdentifier" :: (StringWithCharLimit256)
  , "SourceDetails" :: NullOrUndefined.NullOrUndefined (SourceDetails)
  }
derive instance newtypeSource :: Newtype Source _
derive instance repGenericSource :: Generic Source _
instance showSource :: Show Source where
  show = genericShow
instance decodeSource :: Decode Source where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSource :: Encode Source where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides the source and the message types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic. You can specify the parameter values for <code>SourceDetail</code> only for custom rules. </p>
newtype SourceDetail = SourceDetail 
  { "EventSource" :: NullOrUndefined.NullOrUndefined (EventSource)
  , "MessageType" :: NullOrUndefined.NullOrUndefined (MessageType)
  , "MaximumExecutionFrequency" :: NullOrUndefined.NullOrUndefined (MaximumExecutionFrequency)
  }
derive instance newtypeSourceDetail :: Newtype SourceDetail _
derive instance repGenericSourceDetail :: Generic SourceDetail _
instance showSourceDetail :: Show SourceDetail where
  show = genericShow
instance decodeSourceDetail :: Decode SourceDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceDetail :: Encode SourceDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceDetails = SourceDetails (Array SourceDetail)
derive instance newtypeSourceDetails :: Newtype SourceDetails _
derive instance repGenericSourceDetails :: Generic SourceDetails _
instance showSourceDetails :: Show SourceDetails where
  show = genericShow
instance decodeSourceDetails :: Decode SourceDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceDetails :: Encode SourceDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype StartConfigRulesEvaluationRequest = StartConfigRulesEvaluationRequest 
  { "ConfigRuleNames" :: NullOrUndefined.NullOrUndefined (ReevaluateConfigRuleNames)
  }
derive instance newtypeStartConfigRulesEvaluationRequest :: Newtype StartConfigRulesEvaluationRequest _
derive instance repGenericStartConfigRulesEvaluationRequest :: Generic StartConfigRulesEvaluationRequest _
instance showStartConfigRulesEvaluationRequest :: Show StartConfigRulesEvaluationRequest where
  show = genericShow
instance decodeStartConfigRulesEvaluationRequest :: Decode StartConfigRulesEvaluationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartConfigRulesEvaluationRequest :: Encode StartConfigRulesEvaluationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The output when you start the evaluation for the specified Config rule.</p>
newtype StartConfigRulesEvaluationResponse = StartConfigRulesEvaluationResponse Types.NoArguments
derive instance newtypeStartConfigRulesEvaluationResponse :: Newtype StartConfigRulesEvaluationResponse _
derive instance repGenericStartConfigRulesEvaluationResponse :: Generic StartConfigRulesEvaluationResponse _
instance showStartConfigRulesEvaluationResponse :: Show StartConfigRulesEvaluationResponse where
  show = genericShow
instance decodeStartConfigRulesEvaluationResponse :: Decode StartConfigRulesEvaluationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartConfigRulesEvaluationResponse :: Encode StartConfigRulesEvaluationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>StartConfigurationRecorder</a> action.</p>
newtype StartConfigurationRecorderRequest = StartConfigurationRecorderRequest 
  { "ConfigurationRecorderName" :: (RecorderName)
  }
derive instance newtypeStartConfigurationRecorderRequest :: Newtype StartConfigurationRecorderRequest _
derive instance repGenericStartConfigurationRecorderRequest :: Generic StartConfigurationRecorderRequest _
instance showStartConfigurationRecorderRequest :: Show StartConfigurationRecorderRequest where
  show = genericShow
instance decodeStartConfigurationRecorderRequest :: Decode StartConfigurationRecorderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartConfigurationRecorderRequest :: Encode StartConfigurationRecorderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for the <a>StopConfigurationRecorder</a> action.</p>
newtype StopConfigurationRecorderRequest = StopConfigurationRecorderRequest 
  { "ConfigurationRecorderName" :: (RecorderName)
  }
derive instance newtypeStopConfigurationRecorderRequest :: Newtype StopConfigurationRecorderRequest _
derive instance repGenericStopConfigurationRecorderRequest :: Generic StopConfigurationRecorderRequest _
instance showStopConfigurationRecorderRequest :: Show StopConfigurationRecorderRequest where
  show = genericShow
instance decodeStopConfigurationRecorderRequest :: Decode StopConfigurationRecorderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopConfigurationRecorderRequest :: Encode StopConfigurationRecorderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringWithCharLimit1024 = StringWithCharLimit1024 String
derive instance newtypeStringWithCharLimit1024 :: Newtype StringWithCharLimit1024 _
derive instance repGenericStringWithCharLimit1024 :: Generic StringWithCharLimit1024 _
instance showStringWithCharLimit1024 :: Show StringWithCharLimit1024 where
  show = genericShow
instance decodeStringWithCharLimit1024 :: Decode StringWithCharLimit1024 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringWithCharLimit1024 :: Encode StringWithCharLimit1024 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringWithCharLimit128 = StringWithCharLimit128 String
derive instance newtypeStringWithCharLimit128 :: Newtype StringWithCharLimit128 _
derive instance repGenericStringWithCharLimit128 :: Generic StringWithCharLimit128 _
instance showStringWithCharLimit128 :: Show StringWithCharLimit128 where
  show = genericShow
instance decodeStringWithCharLimit128 :: Decode StringWithCharLimit128 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringWithCharLimit128 :: Encode StringWithCharLimit128 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringWithCharLimit256 = StringWithCharLimit256 String
derive instance newtypeStringWithCharLimit256 :: Newtype StringWithCharLimit256 _
derive instance repGenericStringWithCharLimit256 :: Generic StringWithCharLimit256 _
instance showStringWithCharLimit256 :: Show StringWithCharLimit256 where
  show = genericShow
instance decodeStringWithCharLimit256 :: Decode StringWithCharLimit256 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringWithCharLimit256 :: Encode StringWithCharLimit256 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringWithCharLimit64 = StringWithCharLimit64 String
derive instance newtypeStringWithCharLimit64 :: Newtype StringWithCharLimit64 _
derive instance repGenericStringWithCharLimit64 :: Generic StringWithCharLimit64 _
instance showStringWithCharLimit64 :: Show StringWithCharLimit64 where
  show = genericShow
instance decodeStringWithCharLimit64 :: Decode StringWithCharLimit64 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringWithCharLimit64 :: Encode StringWithCharLimit64 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupplementaryConfiguration = SupplementaryConfiguration (StrMap.StrMap SupplementaryConfigurationValue)
derive instance newtypeSupplementaryConfiguration :: Newtype SupplementaryConfiguration _
derive instance repGenericSupplementaryConfiguration :: Generic SupplementaryConfiguration _
instance showSupplementaryConfiguration :: Show SupplementaryConfiguration where
  show = genericShow
instance decodeSupplementaryConfiguration :: Decode SupplementaryConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupplementaryConfiguration :: Encode SupplementaryConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupplementaryConfigurationName = SupplementaryConfigurationName String
derive instance newtypeSupplementaryConfigurationName :: Newtype SupplementaryConfigurationName _
derive instance repGenericSupplementaryConfigurationName :: Generic SupplementaryConfigurationName _
instance showSupplementaryConfigurationName :: Show SupplementaryConfigurationName where
  show = genericShow
instance decodeSupplementaryConfigurationName :: Decode SupplementaryConfigurationName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupplementaryConfigurationName :: Encode SupplementaryConfigurationName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupplementaryConfigurationValue = SupplementaryConfigurationValue String
derive instance newtypeSupplementaryConfigurationValue :: Newtype SupplementaryConfigurationValue _
derive instance repGenericSupplementaryConfigurationValue :: Generic SupplementaryConfigurationValue _
instance showSupplementaryConfigurationValue :: Show SupplementaryConfigurationValue where
  show = genericShow
instance decodeSupplementaryConfigurationValue :: Decode SupplementaryConfigurationValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupplementaryConfigurationValue :: Encode SupplementaryConfigurationValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tags = Tags (StrMap.StrMap Value)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested action is not valid.</p>
newtype ValidationException = ValidationException Types.NoArguments
derive instance newtypeValidationException :: Newtype ValidationException _
derive instance repGenericValidationException :: Generic ValidationException _
instance showValidationException :: Show ValidationException where
  show = genericShow
instance decodeValidationException :: Decode ValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationException :: Encode ValidationException where
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


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
derive instance repGenericVersion :: Generic Version _
instance showVersion :: Show Version where
  show = genericShow
instance decodeVersion :: Decode Version where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersion :: Encode Version where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
