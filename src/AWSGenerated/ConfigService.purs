

-- | <fullname>AWS Config</fullname> <p>AWS Config provides a way to keep track of the configurations of all the AWS resources associated with your AWS account. You can use AWS Config to get the current and historical configurations of each AWS resource and also to get information about the relationship between the resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2) instance, an Elastic Block Store (EBS) volume, an Elastic network Interface (ENI), or a security group. For a complete list of resources currently supported by AWS Config, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources">Supported AWS Resources</a>.</p> <p>You can access and manage AWS Config through the AWS Management Console, the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS SDKs for AWS Config</p> <p>This reference guide contains documentation for the AWS Config API and the AWS CLI commands that you can use to manage AWS Config.</p> <p>The AWS Config API uses the Signature Version 4 protocol for signing requests. For more information about how to sign a request with this protocol, see <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>For detailed information about AWS Config features and their associated actions or commands, as well as how to work with AWS Management Console, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html">What Is AWS Config?</a> in the <i>AWS Config Developer Guide</i>.</p>
module AWS.ConfigService where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ConfigService" :: String


-- | <p>Deletes the specified AWS Config rule and all of its evaluation results.</p> <p>AWS Config sets the state of a rule to <code>DELETING</code> until the deletion is complete. You cannot update a rule while it is in this state. If you make a <code>PutConfigRule</code> or <code>DeleteConfigRule</code> request for the rule, you will receive a <code>ResourceInUseException</code>.</p> <p>You can check the state of a rule by using the <code>DescribeConfigRules</code> request.</p>
deleteConfigRule :: forall eff. DeleteConfigRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteConfigRule = AWS.request serviceName "deleteConfigRule" 


-- | <p>Deletes the configuration recorder.</p> <p>After the configuration recorder is deleted, AWS Config will not record resource configuration changes until you create a new configuration recorder.</p> <p>This action does not delete the configuration information that was previously recorded. You will be able to access the previously recorded information by using the <code>GetResourceConfigHistory</code> action, but you will not be able to access this information in the AWS Config console until you create a new configuration recorder.</p>
deleteConfigurationRecorder :: forall eff. DeleteConfigurationRecorderRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteConfigurationRecorder = AWS.request serviceName "deleteConfigurationRecorder" 


-- | <p>Deletes the delivery channel.</p> <p>Before you can delete the delivery channel, you must stop the configuration recorder by using the <a>StopConfigurationRecorder</a> action.</p>
deleteDeliveryChannel :: forall eff. DeleteDeliveryChannelRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteDeliveryChannel = AWS.request serviceName "deleteDeliveryChannel" 


-- | <p>Deletes the evaluation results for the specified Config rule. You can specify one Config rule per request. After you delete the evaluation results, you can call the <a>StartConfigRulesEvaluation</a> API to start evaluating your AWS resources against the rule.</p>
deleteEvaluationResults :: forall eff. DeleteEvaluationResultsRequest -> Aff (err :: AWS.RequestError | eff) DeleteEvaluationResultsResponse
deleteEvaluationResults = AWS.request serviceName "deleteEvaluationResults" 


-- | <p>Schedules delivery of a configuration snapshot to the Amazon S3 bucket in the specified delivery channel. After the delivery has started, AWS Config sends following notifications using an Amazon SNS topic that you have specified.</p> <ul> <li> <p>Notification of starting the delivery.</p> </li> <li> <p>Notification of delivery completed, if the delivery was successfully completed.</p> </li> <li> <p>Notification of delivery failure, if the delivery failed to complete.</p> </li> </ul>
deliverConfigSnapshot :: forall eff. DeliverConfigSnapshotRequest -> Aff (err :: AWS.RequestError | eff) DeliverConfigSnapshotResponse
deliverConfigSnapshot = AWS.request serviceName "deliverConfigSnapshot" 


-- | <p>Indicates whether the specified AWS Config rules are compliant. If a rule is noncompliant, this action returns the number of AWS resources that do not comply with the rule.</p> <p>A rule is compliant if all of the evaluated resources comply with it, and it is noncompliant if any of these resources do not comply.</p> <p>If AWS Config has no current evaluation results for the rule, it returns <code>INSUFFICIENT_DATA</code>. This result might indicate one of the following conditions:</p> <ul> <li> <p>AWS Config has never invoked an evaluation for the rule. To check whether it has, use the <code>DescribeConfigRuleEvaluationStatus</code> action to get the <code>LastSuccessfulInvocationTime</code> and <code>LastFailedInvocationTime</code>.</p> </li> <li> <p>The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role that you assigned to your configuration recorder includes the <code>config:PutEvaluations</code> permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the <code>config:PutEvaluations</code> permission.</p> </li> <li> <p>The rule's AWS Lambda function has returned <code>NOT_APPLICABLE</code> for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.</p> </li> </ul>
describeComplianceByConfigRule :: forall eff. DescribeComplianceByConfigRuleRequest -> Aff (err :: AWS.RequestError | eff) DescribeComplianceByConfigRuleResponse
describeComplianceByConfigRule = AWS.request serviceName "describeComplianceByConfigRule" 


-- | <p>Indicates whether the specified AWS resources are compliant. If a resource is noncompliant, this action returns the number of AWS Config rules that the resource does not comply with.</p> <p>A resource is compliant if it complies with all the AWS Config rules that evaluate it. It is noncompliant if it does not comply with one or more of these rules.</p> <p>If AWS Config has no current evaluation results for the resource, it returns <code>INSUFFICIENT_DATA</code>. This result might indicate one of the following conditions about the rules that evaluate the resource:</p> <ul> <li> <p>AWS Config has never invoked an evaluation for the rule. To check whether it has, use the <code>DescribeConfigRuleEvaluationStatus</code> action to get the <code>LastSuccessfulInvocationTime</code> and <code>LastFailedInvocationTime</code>.</p> </li> <li> <p>The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role that you assigned to your configuration recorder includes the <code>config:PutEvaluations</code> permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the <code>config:PutEvaluations</code> permission.</p> </li> <li> <p>The rule's AWS Lambda function has returned <code>NOT_APPLICABLE</code> for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.</p> </li> </ul>
describeComplianceByResource :: forall eff. DescribeComplianceByResourceRequest -> Aff (err :: AWS.RequestError | eff) DescribeComplianceByResourceResponse
describeComplianceByResource = AWS.request serviceName "describeComplianceByResource" 


-- | <p>Returns status information for each of your AWS managed Config rules. The status includes information such as the last time AWS Config invoked the rule, the last time AWS Config failed to invoke the rule, and the related error for the last failure.</p>
describeConfigRuleEvaluationStatus :: forall eff. DescribeConfigRuleEvaluationStatusRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigRuleEvaluationStatusResponse
describeConfigRuleEvaluationStatus = AWS.request serviceName "describeConfigRuleEvaluationStatus" 


-- | <p>Returns details about your AWS Config rules.</p>
describeConfigRules :: forall eff. DescribeConfigRulesRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigRulesResponse
describeConfigRules = AWS.request serviceName "describeConfigRules" 


-- | <p>Returns the current status of the specified configuration recorder. If a configuration recorder is not specified, this action returns the status of all configuration recorder associated with the account.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> </note>
describeConfigurationRecorderStatus :: forall eff. DescribeConfigurationRecorderStatusRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationRecorderStatusResponse
describeConfigurationRecorderStatus = AWS.request serviceName "describeConfigurationRecorderStatus" 


-- | <p>Returns the details for the specified configuration recorders. If the configuration recorder is not specified, this action returns the details for all configuration recorders associated with the account.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> </note>
describeConfigurationRecorders :: forall eff. DescribeConfigurationRecordersRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationRecordersResponse
describeConfigurationRecorders = AWS.request serviceName "describeConfigurationRecorders" 


-- | <p>Returns the current status of the specified delivery channel. If a delivery channel is not specified, this action returns the current status of all delivery channels associated with the account.</p> <note> <p>Currently, you can specify only one delivery channel per region in your account.</p> </note>
describeDeliveryChannelStatus :: forall eff. DescribeDeliveryChannelStatusRequest -> Aff (err :: AWS.RequestError | eff) DescribeDeliveryChannelStatusResponse
describeDeliveryChannelStatus = AWS.request serviceName "describeDeliveryChannelStatus" 


-- | <p>Returns details about the specified delivery channel. If a delivery channel is not specified, this action returns the details of all delivery channels associated with the account.</p> <note> <p>Currently, you can specify only one delivery channel per region in your account.</p> </note>
describeDeliveryChannels :: forall eff. DescribeDeliveryChannelsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDeliveryChannelsResponse
describeDeliveryChannels = AWS.request serviceName "describeDeliveryChannels" 


-- | <p>Returns the evaluation results for the specified AWS Config rule. The results indicate which AWS resources were evaluated by the rule, when each resource was last evaluated, and whether each resource complies with the rule.</p>
getComplianceDetailsByConfigRule :: forall eff. GetComplianceDetailsByConfigRuleRequest -> Aff (err :: AWS.RequestError | eff) GetComplianceDetailsByConfigRuleResponse
getComplianceDetailsByConfigRule = AWS.request serviceName "getComplianceDetailsByConfigRule" 


-- | <p>Returns the evaluation results for the specified AWS resource. The results indicate which AWS Config rules were used to evaluate the resource, when each rule was last used, and whether the resource complies with each rule.</p>
getComplianceDetailsByResource :: forall eff. GetComplianceDetailsByResourceRequest -> Aff (err :: AWS.RequestError | eff) GetComplianceDetailsByResourceResponse
getComplianceDetailsByResource = AWS.request serviceName "getComplianceDetailsByResource" 


-- | <p>Returns the number of AWS Config rules that are compliant and noncompliant, up to a maximum of 25 for each.</p>
getComplianceSummaryByConfigRule :: forall eff.  Aff (err :: AWS.RequestError | eff) GetComplianceSummaryByConfigRuleResponse
getComplianceSummaryByConfigRule = AWS.request serviceName "getComplianceSummaryByConfigRule" unit


-- | <p>Returns the number of resources that are compliant and the number that are noncompliant. You can specify one or more resource types to get these numbers for each resource type. The maximum number returned is 100.</p>
getComplianceSummaryByResourceType :: forall eff. GetComplianceSummaryByResourceTypeRequest -> Aff (err :: AWS.RequestError | eff) GetComplianceSummaryByResourceTypeResponse
getComplianceSummaryByResourceType = AWS.request serviceName "getComplianceSummaryByResourceType" 


-- | <p>Returns the resource types, the number of each resource type, and the total number of resources that AWS Config is recording in this region for your AWS account. </p> <p class="title"> <b>Example</b> </p> <ol> <li> <p>AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets.</p> </li> <li> <p>You make a call to the <code>GetDiscoveredResourceCounts</code> action and specify that you want all resource types. </p> </li> <li> <p>AWS Config returns the following:</p> <ul> <li> <p>The resource types (EC2 instances, IAM users, and S3 buckets)</p> </li> <li> <p>The number of each resource type (25, 20, and 15)</p> </li> <li> <p>The total number of all resources (60)</p> </li> </ul> </li> </ol> <p>The response is paginated. By default, AWS Config lists 100 <a>ResourceCount</a> objects on each page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p> <note> <p>If you make a call to the <a>GetDiscoveredResourceCounts</a> action, you may not immediately receive resource counts in the following situations:</p> <ul> <li> <p>You are a new AWS Config customer</p> </li> <li> <p>You just enabled resource recording</p> </li> </ul> <p>It may take a few minutes for AWS Config to record and count your resources. Wait a few minutes and then retry the <a>GetDiscoveredResourceCounts</a> action. </p> </note>
getDiscoveredResourceCounts :: forall eff. GetDiscoveredResourceCountsRequest -> Aff (err :: AWS.RequestError | eff) GetDiscoveredResourceCountsResponse
getDiscoveredResourceCounts = AWS.request serviceName "getDiscoveredResourceCounts" 


-- | <p>Returns a list of configuration items for the specified resource. The list contains details about each state of the resource during the specified time interval.</p> <p>The response is paginated. By default, AWS Config returns a limit of 10 configuration items per page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p> <note> <p>Each call to the API is limited to span a duration of seven days. It is likely that the number of records returned is smaller than the specified <code>limit</code>. In such cases, you can make another call, using the <code>nextToken</code>.</p> </note>
getResourceConfigHistory :: forall eff. GetResourceConfigHistoryRequest -> Aff (err :: AWS.RequestError | eff) GetResourceConfigHistoryResponse
getResourceConfigHistory = AWS.request serviceName "getResourceConfigHistory" 


-- | <p>Accepts a resource type and returns a list of resource identifiers for the resources of that type. A resource identifier includes the resource type, ID, and (if available) the custom resource name. The results consist of resources that AWS Config has discovered, including those that AWS Config is not currently recording. You can narrow the results to include only resources that have specific resource IDs or a resource name.</p> <note> <p>You can specify either resource IDs or a resource name but not both in the same request.</p> </note> <p>The response is paginated. By default, AWS Config lists 100 resource identifiers on each page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p>
listDiscoveredResources :: forall eff. ListDiscoveredResourcesRequest -> Aff (err :: AWS.RequestError | eff) ListDiscoveredResourcesResponse
listDiscoveredResources = AWS.request serviceName "listDiscoveredResources" 


-- | <p>Adds or updates an AWS Config rule for evaluating whether your AWS resources comply with your desired configurations.</p> <p>You can use this action for custom Config rules and AWS managed Config rules. A custom Config rule is a rule that you develop and maintain. An AWS managed Config rule is a customizable, predefined rule that AWS Config provides.</p> <p>If you are adding a new custom Config rule, you must first create the AWS Lambda function that the rule invokes to evaluate your resources. When you use the <code>PutConfigRule</code> action to add the rule to AWS Config, you must specify the Amazon Resource Name (ARN) that AWS Lambda assigns to the function. Specify the ARN for the <code>SourceIdentifier</code> key. This key is part of the <code>Source</code> object, which is part of the <code>ConfigRule</code> object. </p> <p>If you are adding an AWS managed Config rule, specify the rule's identifier for the <code>SourceIdentifier</code> key. To reference AWS managed Config rule identifiers, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html">About AWS Managed Config Rules</a>.</p> <p>For any new rule that you add, specify the <code>ConfigRuleName</code> in the <code>ConfigRule</code> object. Do not specify the <code>ConfigRuleArn</code> or the <code>ConfigRuleId</code>. These values are generated by AWS Config for new rules.</p> <p>If you are updating a rule that you added previously, you can specify the rule by <code>ConfigRuleName</code>, <code>ConfigRuleId</code>, or <code>ConfigRuleArn</code> in the <code>ConfigRule</code> data type that you use in this request.</p> <p>The maximum number of rules that AWS Config supports is 50.</p> <p>For more information about requesting a rule limit increase, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config">AWS Config Limits</a> in the <i>AWS General Reference Guide</i>.</p> <p>For more information about developing and using AWS Config rules, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html">Evaluating AWS Resource Configurations with AWS Config</a> in the <i>AWS Config Developer Guide</i>.</p>
putConfigRule :: forall eff. PutConfigRuleRequest -> Aff (err :: AWS.RequestError | eff) Unit
putConfigRule = AWS.request serviceName "putConfigRule" 


-- | <p>Creates a new configuration recorder to record the selected resource configurations.</p> <p>You can use this action to change the role <code>roleARN</code> and/or the <code>recordingGroup</code> of an existing recorder. To change the role, call the action on the existing configuration recorder and specify a role.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> <p>If <code>ConfigurationRecorder</code> does not have the <b>recordingGroup</b> parameter specified, the default is to record all supported resource types.</p> </note>
putConfigurationRecorder :: forall eff. PutConfigurationRecorderRequest -> Aff (err :: AWS.RequestError | eff) Unit
putConfigurationRecorder = AWS.request serviceName "putConfigurationRecorder" 


-- | <p>Creates a delivery channel object to deliver configuration information to an Amazon S3 bucket and Amazon SNS topic.</p> <p>Before you can create a delivery channel, you must create a configuration recorder.</p> <p>You can use this action to change the Amazon S3 bucket or an Amazon SNS topic of the existing delivery channel. To change the Amazon S3 bucket or an Amazon SNS topic, call this action and specify the changed values for the S3 bucket and the SNS topic. If you specify a different value for either the S3 bucket or the SNS topic, this action will keep the existing value for the parameter that is not changed.</p> <note> <p>You can have only one delivery channel per region in your account.</p> </note>
putDeliveryChannel :: forall eff. PutDeliveryChannelRequest -> Aff (err :: AWS.RequestError | eff) Unit
putDeliveryChannel = AWS.request serviceName "putDeliveryChannel" 


-- | <p>Used by an AWS Lambda function to deliver evaluation results to AWS Config. This action is required in every AWS Lambda function that is invoked by an AWS Config rule.</p>
putEvaluations :: forall eff. PutEvaluationsRequest -> Aff (err :: AWS.RequestError | eff) PutEvaluationsResponse
putEvaluations = AWS.request serviceName "putEvaluations" 


-- | <p>Runs an on-demand evaluation for the specified Config rules against the last known configuration state of the resources. Use <code>StartConfigRulesEvaluation</code> when you want to test a rule that you updated is working as expected. <code>StartConfigRulesEvaluation</code> does not re-record the latest configuration state for your resources; it re-runs an evaluation against the last known state of your resources. </p> <p>You can specify up to 25 Config rules per request. </p> <p>An existing <code>StartConfigRulesEvaluation</code> call must complete for the specified rules before you can call the API again. If you chose to have AWS Config stream to an Amazon SNS topic, you will receive a <code>ConfigRuleEvaluationStarted</code> notification when the evaluation starts.</p> <note> <p>You don't need to call the <code>StartConfigRulesEvaluation</code> API to run an evaluation for a new rule. When you create a new rule, AWS Config automatically evaluates your resources against the rule. </p> </note> <p>The <code>StartConfigRulesEvaluation</code> API is useful if you want to run on-demand evaluations, such as the following example:</p> <ol> <li> <p>You have a custom rule that evaluates your IAM resources every 24 hours.</p> </li> <li> <p>You update your Lambda function to add additional conditions to your rule.</p> </li> <li> <p>Instead of waiting for the next periodic evaluation, you call the <code>StartConfigRulesEvaluation</code> API.</p> </li> <li> <p>AWS Config invokes your Lambda function and evaluates your IAM resources.</p> </li> <li> <p>Your custom rule will still run periodic evaluations every 24 hours.</p> </li> </ol>
startConfigRulesEvaluation :: forall eff. StartConfigRulesEvaluationRequest -> Aff (err :: AWS.RequestError | eff) StartConfigRulesEvaluationResponse
startConfigRulesEvaluation = AWS.request serviceName "startConfigRulesEvaluation" 


-- | <p>Starts recording configurations of the AWS resources you have selected to record in your AWS account.</p> <p>You must have created at least one delivery channel to successfully start the configuration recorder.</p>
startConfigurationRecorder :: forall eff. StartConfigurationRecorderRequest -> Aff (err :: AWS.RequestError | eff) Unit
startConfigurationRecorder = AWS.request serviceName "startConfigurationRecorder" 


-- | <p>Stops recording configurations of the AWS resources you have selected to record in your AWS account.</p>
stopConfigurationRecorder :: forall eff. StopConfigurationRecorderRequest -> Aff (err :: AWS.RequestError | eff) Unit
stopConfigurationRecorder = AWS.request serviceName "stopConfigurationRecorder" 


newtype ARN = ARN String
derive instance newtypeARN :: Newtype ARN _


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _


newtype AllSupported = AllSupported Boolean
derive instance newtypeAllSupported :: Newtype AllSupported _


newtype AvailabilityZone = AvailabilityZone String
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _


newtype AwsRegion = AwsRegion String
derive instance newtypeAwsRegion :: Newtype AwsRegion _


newtype BaseResourceId = BaseResourceId String
derive instance newtypeBaseResourceId :: Newtype BaseResourceId _


newtype ChannelName = ChannelName String
derive instance newtypeChannelName :: Newtype ChannelName _


newtype ChronologicalOrder = ChronologicalOrder String
derive instance newtypeChronologicalOrder :: Newtype ChronologicalOrder _


-- | <p>Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.</p>
newtype Compliance = Compliance 
  { "ComplianceType" :: NullOrUndefined (ComplianceType)
  , "ComplianceContributorCount" :: NullOrUndefined (ComplianceContributorCount)
  }
derive instance newtypeCompliance :: Newtype Compliance _


-- | <p>Indicates whether an AWS Config rule is compliant. A rule is compliant if all of the resources that the rule evaluated comply with it, and it is noncompliant if any of these resources do not comply.</p>
newtype ComplianceByConfigRule = ComplianceByConfigRule 
  { "ConfigRuleName" :: NullOrUndefined (StringWithCharLimit64)
  , "Compliance" :: NullOrUndefined (Compliance)
  }
derive instance newtypeComplianceByConfigRule :: Newtype ComplianceByConfigRule _


newtype ComplianceByConfigRules = ComplianceByConfigRules (Array ComplianceByConfigRule)
derive instance newtypeComplianceByConfigRules :: Newtype ComplianceByConfigRules _


-- | <p>Indicates whether an AWS resource that is evaluated according to one or more AWS Config rules is compliant. A resource is compliant if it complies with all of the rules that evaluate it, and it is noncompliant if it does not comply with one or more of these rules.</p>
newtype ComplianceByResource = ComplianceByResource 
  { "ResourceType" :: NullOrUndefined (StringWithCharLimit256)
  , "ResourceId" :: NullOrUndefined (BaseResourceId)
  , "Compliance" :: NullOrUndefined (Compliance)
  }
derive instance newtypeComplianceByResource :: Newtype ComplianceByResource _


newtype ComplianceByResources = ComplianceByResources (Array ComplianceByResource)
derive instance newtypeComplianceByResources :: Newtype ComplianceByResources _


-- | <p>The number of AWS resources or AWS Config rules responsible for the current compliance of the item, up to a maximum number.</p>
newtype ComplianceContributorCount = ComplianceContributorCount 
  { "CappedCount" :: NullOrUndefined (Int)
  , "CapExceeded" :: NullOrUndefined (Boolean)
  }
derive instance newtypeComplianceContributorCount :: Newtype ComplianceContributorCount _


newtype ComplianceResourceTypes = ComplianceResourceTypes (Array StringWithCharLimit256)
derive instance newtypeComplianceResourceTypes :: Newtype ComplianceResourceTypes _


newtype ComplianceSummariesByResourceType = ComplianceSummariesByResourceType (Array ComplianceSummaryByResourceType)
derive instance newtypeComplianceSummariesByResourceType :: Newtype ComplianceSummariesByResourceType _


-- | <p>The number of AWS Config rules or AWS resources that are compliant and noncompliant.</p>
newtype ComplianceSummary = ComplianceSummary 
  { "CompliantResourceCount" :: NullOrUndefined (ComplianceContributorCount)
  , "NonCompliantResourceCount" :: NullOrUndefined (ComplianceContributorCount)
  , "ComplianceSummaryTimestamp" :: NullOrUndefined (Date)
  }
derive instance newtypeComplianceSummary :: Newtype ComplianceSummary _


-- | <p>The number of AWS resources of a specific type that are compliant or noncompliant, up to a maximum of 100 for each compliance.</p>
newtype ComplianceSummaryByResourceType = ComplianceSummaryByResourceType 
  { "ResourceType" :: NullOrUndefined (StringWithCharLimit256)
  , "ComplianceSummary" :: NullOrUndefined (ComplianceSummary)
  }
derive instance newtypeComplianceSummaryByResourceType :: Newtype ComplianceSummaryByResourceType _


newtype ComplianceType = ComplianceType String
derive instance newtypeComplianceType :: Newtype ComplianceType _


newtype ComplianceTypes = ComplianceTypes (Array ComplianceType)
derive instance newtypeComplianceTypes :: Newtype ComplianceTypes _


-- | <p>Provides status of the delivery of the snapshot or the configuration history to the specified Amazon S3 bucket. Also provides the status of notifications about the Amazon S3 delivery to the specified Amazon SNS topic.</p>
newtype ConfigExportDeliveryInfo = ConfigExportDeliveryInfo 
  { "LastStatus'" :: NullOrUndefined (DeliveryStatus)
  , "LastErrorCode'" :: NullOrUndefined (String)
  , "LastErrorMessage'" :: NullOrUndefined (String)
  , "LastAttemptTime'" :: NullOrUndefined (Date)
  , "LastSuccessfulTime'" :: NullOrUndefined (Date)
  , "NextDeliveryTime'" :: NullOrUndefined (Date)
  }
derive instance newtypeConfigExportDeliveryInfo :: Newtype ConfigExportDeliveryInfo _


-- | <p>An AWS Config rule represents an AWS Lambda function that you create for a custom rule or a predefined function for an AWS managed rule. The function evaluates configuration items to assess whether your AWS resources comply with your desired configurations. This function can run when AWS Config detects a configuration change to an AWS resource and at a periodic frequency that you choose (for example, every 24 hours).</p> <note> <p>You can use the AWS CLI and AWS SDKs if you want to create a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot. For more information, see <a>ConfigSnapshotDeliveryProperties</a>.</p> </note> <p>For more information about developing and using AWS Config rules, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html">Evaluating AWS Resource Configurations with AWS Config</a> in the <i>AWS Config Developer Guide</i>.</p>
newtype ConfigRule = ConfigRule 
  { "ConfigRuleName" :: NullOrUndefined (StringWithCharLimit64)
  , "ConfigRuleArn" :: NullOrUndefined (String)
  , "ConfigRuleId" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (EmptiableStringWithCharLimit256)
  , "Scope" :: NullOrUndefined (Scope)
  , "Source" :: (Source)
  , "InputParameters" :: NullOrUndefined (StringWithCharLimit1024)
  , "MaximumExecutionFrequency" :: NullOrUndefined (MaximumExecutionFrequency)
  , "ConfigRuleState" :: NullOrUndefined (ConfigRuleState)
  }
derive instance newtypeConfigRule :: Newtype ConfigRule _


-- | <p>Status information for your AWS managed Config rules. The status includes information such as the last time the rule ran, the last time it failed, and the related error for the last failure.</p> <p>This action does not return status information about custom Config rules.</p>
newtype ConfigRuleEvaluationStatus = ConfigRuleEvaluationStatus 
  { "ConfigRuleName" :: NullOrUndefined (StringWithCharLimit64)
  , "ConfigRuleArn" :: NullOrUndefined (String)
  , "ConfigRuleId" :: NullOrUndefined (String)
  , "LastSuccessfulInvocationTime" :: NullOrUndefined (Date)
  , "LastFailedInvocationTime" :: NullOrUndefined (Date)
  , "LastSuccessfulEvaluationTime" :: NullOrUndefined (Date)
  , "LastFailedEvaluationTime" :: NullOrUndefined (Date)
  , "FirstActivatedTime" :: NullOrUndefined (Date)
  , "LastErrorCode" :: NullOrUndefined (String)
  , "LastErrorMessage" :: NullOrUndefined (String)
  , "FirstEvaluationStarted" :: NullOrUndefined (Boolean)
  }
derive instance newtypeConfigRuleEvaluationStatus :: Newtype ConfigRuleEvaluationStatus _


newtype ConfigRuleEvaluationStatusList = ConfigRuleEvaluationStatusList (Array ConfigRuleEvaluationStatus)
derive instance newtypeConfigRuleEvaluationStatusList :: Newtype ConfigRuleEvaluationStatusList _


newtype ConfigRuleNames = ConfigRuleNames (Array StringWithCharLimit64)
derive instance newtypeConfigRuleNames :: Newtype ConfigRuleNames _


newtype ConfigRuleState = ConfigRuleState String
derive instance newtypeConfigRuleState :: Newtype ConfigRuleState _


newtype ConfigRules = ConfigRules (Array ConfigRule)
derive instance newtypeConfigRules :: Newtype ConfigRules _


-- | <p>Provides options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket in your delivery channel.</p> <note> <p>If you want to create a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot, see the following:</p> </note> <p>The frequency for a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot is set by one of two values, depending on which is less frequent:</p> <ul> <li> <p>The value for the <code>deliveryFrequency</code> parameter within the delivery channel configuration, which sets how often AWS Config delivers configuration snapshots. This value also sets how often AWS Config invokes evaluations for Config rules.</p> </li> <li> <p>The value for the <code>MaximumExecutionFrequency</code> parameter, which sets the maximum frequency with which AWS Config invokes evaluations for the rule. For more information, see <a>ConfigRule</a>.</p> </li> </ul> <p>If the <code>deliveryFrequency</code> value is less frequent than the <code>MaximumExecutionFrequency</code> value for a rule, AWS Config invokes the rule only as often as the <code>deliveryFrequency</code> value.</p> <ol> <li> <p>For example, you want your rule to run evaluations when AWS Config delivers the configuration snapshot.</p> </li> <li> <p>You specify the <code>MaximumExecutionFrequency</code> value for <code>Six_Hours</code>. </p> </li> <li> <p>You then specify the delivery channel <code>deliveryFrequency</code> value for <code>TwentyFour_Hours</code>.</p> </li> <li> <p>Because the value for <code>deliveryFrequency</code> is less frequent than <code>MaximumExecutionFrequency</code>, AWS Config invokes evaluations for the rule every 24 hours. </p> </li> </ol> <p>You should set the <code>MaximumExecutionFrequency</code> value to be at least as frequent as the <code>deliveryFrequency</code> value. You can view the <code>deliveryFrequency</code> value by using the <code>DescribeDeliveryChannnels</code> action.</p> <p>To update the <code>deliveryFrequency</code> with which AWS Config delivers your configuration snapshots, use the <code>PutDeliveryChannel</code> action.</p>
newtype ConfigSnapshotDeliveryProperties = ConfigSnapshotDeliveryProperties 
  { "DeliveryFrequency'" :: NullOrUndefined (MaximumExecutionFrequency)
  }
derive instance newtypeConfigSnapshotDeliveryProperties :: Newtype ConfigSnapshotDeliveryProperties _


-- | <p>A list that contains the status of the delivery of the configuration stream notification to the Amazon SNS topic.</p>
newtype ConfigStreamDeliveryInfo = ConfigStreamDeliveryInfo 
  { "LastStatus'" :: NullOrUndefined (DeliveryStatus)
  , "LastErrorCode'" :: NullOrUndefined (String)
  , "LastErrorMessage'" :: NullOrUndefined (String)
  , "LastStatusChangeTime'" :: NullOrUndefined (Date)
  }
derive instance newtypeConfigStreamDeliveryInfo :: Newtype ConfigStreamDeliveryInfo _


newtype Configuration = Configuration String
derive instance newtypeConfiguration :: Newtype Configuration _


-- | <p>A list that contains detailed configurations of a specified resource.</p>
newtype ConfigurationItem = ConfigurationItem 
  { "Version'" :: NullOrUndefined (Version)
  , "AccountId'" :: NullOrUndefined (AccountId)
  , "ConfigurationItemCaptureTime'" :: NullOrUndefined (ConfigurationItemCaptureTime)
  , "ConfigurationItemStatus'" :: NullOrUndefined (ConfigurationItemStatus)
  , "ConfigurationStateId'" :: NullOrUndefined (ConfigurationStateId)
  , "ConfigurationItemMD5Hash'" :: NullOrUndefined (ConfigurationItemMD5Hash)
  , "Arn'" :: NullOrUndefined (ARN)
  , "ResourceType'" :: NullOrUndefined (ResourceType)
  , "ResourceId'" :: NullOrUndefined (ResourceId)
  , "ResourceName'" :: NullOrUndefined (ResourceName)
  , "AwsRegion'" :: NullOrUndefined (AwsRegion)
  , "AvailabilityZone'" :: NullOrUndefined (AvailabilityZone)
  , "ResourceCreationTime'" :: NullOrUndefined (ResourceCreationTime)
  , "Tags'" :: NullOrUndefined (Tags)
  , "RelatedEvents'" :: NullOrUndefined (RelatedEventList)
  , "Relationships'" :: NullOrUndefined (RelationshipList)
  , "Configuration'" :: NullOrUndefined (Configuration)
  , "SupplementaryConfiguration'" :: NullOrUndefined (SupplementaryConfiguration)
  }
derive instance newtypeConfigurationItem :: Newtype ConfigurationItem _


newtype ConfigurationItemCaptureTime = ConfigurationItemCaptureTime Number
derive instance newtypeConfigurationItemCaptureTime :: Newtype ConfigurationItemCaptureTime _


newtype ConfigurationItemList = ConfigurationItemList (Array ConfigurationItem)
derive instance newtypeConfigurationItemList :: Newtype ConfigurationItemList _


newtype ConfigurationItemMD5Hash = ConfigurationItemMD5Hash String
derive instance newtypeConfigurationItemMD5Hash :: Newtype ConfigurationItemMD5Hash _


newtype ConfigurationItemStatus = ConfigurationItemStatus String
derive instance newtypeConfigurationItemStatus :: Newtype ConfigurationItemStatus _


-- | <p>An object that represents the recording of configuration changes of an AWS resource.</p>
newtype ConfigurationRecorder = ConfigurationRecorder 
  { "Name'" :: NullOrUndefined (RecorderName)
  , "RoleARN'" :: NullOrUndefined (String)
  , "RecordingGroup'" :: NullOrUndefined (RecordingGroup)
  }
derive instance newtypeConfigurationRecorder :: Newtype ConfigurationRecorder _


newtype ConfigurationRecorderList = ConfigurationRecorderList (Array ConfigurationRecorder)
derive instance newtypeConfigurationRecorderList :: Newtype ConfigurationRecorderList _


newtype ConfigurationRecorderNameList = ConfigurationRecorderNameList (Array RecorderName)
derive instance newtypeConfigurationRecorderNameList :: Newtype ConfigurationRecorderNameList _


-- | <p>The current status of the configuration recorder.</p>
newtype ConfigurationRecorderStatus = ConfigurationRecorderStatus 
  { "Name'" :: NullOrUndefined (String)
  , "LastStartTime'" :: NullOrUndefined (Date)
  , "LastStopTime'" :: NullOrUndefined (Date)
  , "Recording'" :: NullOrUndefined (Boolean)
  , "LastStatus'" :: NullOrUndefined (RecorderStatus)
  , "LastErrorCode'" :: NullOrUndefined (String)
  , "LastErrorMessage'" :: NullOrUndefined (String)
  , "LastStatusChangeTime'" :: NullOrUndefined (Date)
  }
derive instance newtypeConfigurationRecorderStatus :: Newtype ConfigurationRecorderStatus _


newtype ConfigurationRecorderStatusList = ConfigurationRecorderStatusList (Array ConfigurationRecorderStatus)
derive instance newtypeConfigurationRecorderStatusList :: Newtype ConfigurationRecorderStatusList _


newtype ConfigurationStateId = ConfigurationStateId String
derive instance newtypeConfigurationStateId :: Newtype ConfigurationStateId _


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _


-- | <p/>
newtype DeleteConfigRuleRequest = DeleteConfigRuleRequest 
  { "ConfigRuleName" :: (StringWithCharLimit64)
  }
derive instance newtypeDeleteConfigRuleRequest :: Newtype DeleteConfigRuleRequest _


-- | <p>The request object for the <code>DeleteConfigurationRecorder</code> action.</p>
newtype DeleteConfigurationRecorderRequest = DeleteConfigurationRecorderRequest 
  { "ConfigurationRecorderName" :: (RecorderName)
  }
derive instance newtypeDeleteConfigurationRecorderRequest :: Newtype DeleteConfigurationRecorderRequest _


-- | <p>The input for the <a>DeleteDeliveryChannel</a> action. The action accepts the following data in JSON format. </p>
newtype DeleteDeliveryChannelRequest = DeleteDeliveryChannelRequest 
  { "DeliveryChannelName" :: (ChannelName)
  }
derive instance newtypeDeleteDeliveryChannelRequest :: Newtype DeleteDeliveryChannelRequest _


-- | <p/>
newtype DeleteEvaluationResultsRequest = DeleteEvaluationResultsRequest 
  { "ConfigRuleName" :: (StringWithCharLimit64)
  }
derive instance newtypeDeleteEvaluationResultsRequest :: Newtype DeleteEvaluationResultsRequest _


-- | <p>The output when you delete the evaluation results for the specified Config rule.</p>
newtype DeleteEvaluationResultsResponse = DeleteEvaluationResultsResponse 
  { 
  }
derive instance newtypeDeleteEvaluationResultsResponse :: Newtype DeleteEvaluationResultsResponse _


-- | <p>The input for the <a>DeliverConfigSnapshot</a> action.</p>
newtype DeliverConfigSnapshotRequest = DeliverConfigSnapshotRequest 
  { "DeliveryChannelName'" :: (ChannelName)
  }
derive instance newtypeDeliverConfigSnapshotRequest :: Newtype DeliverConfigSnapshotRequest _


-- | <p>The output for the <a>DeliverConfigSnapshot</a> action in JSON format.</p>
newtype DeliverConfigSnapshotResponse = DeliverConfigSnapshotResponse 
  { "ConfigSnapshotId'" :: NullOrUndefined (String)
  }
derive instance newtypeDeliverConfigSnapshotResponse :: Newtype DeliverConfigSnapshotResponse _


-- | <p>The channel through which AWS Config delivers notifications and updated configuration states.</p>
newtype DeliveryChannel = DeliveryChannel 
  { "Name'" :: NullOrUndefined (ChannelName)
  , "S3BucketName'" :: NullOrUndefined (String)
  , "S3KeyPrefix'" :: NullOrUndefined (String)
  , "SnsTopicARN'" :: NullOrUndefined (String)
  , "ConfigSnapshotDeliveryProperties'" :: NullOrUndefined (ConfigSnapshotDeliveryProperties)
  }
derive instance newtypeDeliveryChannel :: Newtype DeliveryChannel _


newtype DeliveryChannelList = DeliveryChannelList (Array DeliveryChannel)
derive instance newtypeDeliveryChannelList :: Newtype DeliveryChannelList _


newtype DeliveryChannelNameList = DeliveryChannelNameList (Array ChannelName)
derive instance newtypeDeliveryChannelNameList :: Newtype DeliveryChannelNameList _


-- | <p>The status of a specified delivery channel.</p> <p>Valid values: <code>Success</code> | <code>Failure</code> </p>
newtype DeliveryChannelStatus = DeliveryChannelStatus 
  { "Name'" :: NullOrUndefined (String)
  , "ConfigSnapshotDeliveryInfo'" :: NullOrUndefined (ConfigExportDeliveryInfo)
  , "ConfigHistoryDeliveryInfo'" :: NullOrUndefined (ConfigExportDeliveryInfo)
  , "ConfigStreamDeliveryInfo'" :: NullOrUndefined (ConfigStreamDeliveryInfo)
  }
derive instance newtypeDeliveryChannelStatus :: Newtype DeliveryChannelStatus _


newtype DeliveryChannelStatusList = DeliveryChannelStatusList (Array DeliveryChannelStatus)
derive instance newtypeDeliveryChannelStatusList :: Newtype DeliveryChannelStatusList _


newtype DeliveryStatus = DeliveryStatus String
derive instance newtypeDeliveryStatus :: Newtype DeliveryStatus _


-- | <p/>
newtype DescribeComplianceByConfigRuleRequest = DescribeComplianceByConfigRuleRequest 
  { "ConfigRuleNames" :: NullOrUndefined (ConfigRuleNames)
  , "ComplianceTypes" :: NullOrUndefined (ComplianceTypes)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeComplianceByConfigRuleRequest :: Newtype DescribeComplianceByConfigRuleRequest _


-- | <p/>
newtype DescribeComplianceByConfigRuleResponse = DescribeComplianceByConfigRuleResponse 
  { "ComplianceByConfigRules" :: NullOrUndefined (ComplianceByConfigRules)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeComplianceByConfigRuleResponse :: Newtype DescribeComplianceByConfigRuleResponse _


-- | <p/>
newtype DescribeComplianceByResourceRequest = DescribeComplianceByResourceRequest 
  { "ResourceType" :: NullOrUndefined (StringWithCharLimit256)
  , "ResourceId" :: NullOrUndefined (BaseResourceId)
  , "ComplianceTypes" :: NullOrUndefined (ComplianceTypes)
  , "Limit" :: NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeComplianceByResourceRequest :: Newtype DescribeComplianceByResourceRequest _


-- | <p/>
newtype DescribeComplianceByResourceResponse = DescribeComplianceByResourceResponse 
  { "ComplianceByResources" :: NullOrUndefined (ComplianceByResources)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeComplianceByResourceResponse :: Newtype DescribeComplianceByResourceResponse _


-- | <p/>
newtype DescribeConfigRuleEvaluationStatusRequest = DescribeConfigRuleEvaluationStatusRequest 
  { "ConfigRuleNames" :: NullOrUndefined (ConfigRuleNames)
  , "NextToken" :: NullOrUndefined (String)
  , "Limit" :: NullOrUndefined (RuleLimit)
  }
derive instance newtypeDescribeConfigRuleEvaluationStatusRequest :: Newtype DescribeConfigRuleEvaluationStatusRequest _


-- | <p/>
newtype DescribeConfigRuleEvaluationStatusResponse = DescribeConfigRuleEvaluationStatusResponse 
  { "ConfigRulesEvaluationStatus" :: NullOrUndefined (ConfigRuleEvaluationStatusList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigRuleEvaluationStatusResponse :: Newtype DescribeConfigRuleEvaluationStatusResponse _


-- | <p/>
newtype DescribeConfigRulesRequest = DescribeConfigRulesRequest 
  { "ConfigRuleNames" :: NullOrUndefined (ConfigRuleNames)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigRulesRequest :: Newtype DescribeConfigRulesRequest _


-- | <p/>
newtype DescribeConfigRulesResponse = DescribeConfigRulesResponse 
  { "ConfigRules" :: NullOrUndefined (ConfigRules)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeConfigRulesResponse :: Newtype DescribeConfigRulesResponse _


-- | <p>The input for the <a>DescribeConfigurationRecorderStatus</a> action.</p>
newtype DescribeConfigurationRecorderStatusRequest = DescribeConfigurationRecorderStatusRequest 
  { "ConfigurationRecorderNames" :: NullOrUndefined (ConfigurationRecorderNameList)
  }
derive instance newtypeDescribeConfigurationRecorderStatusRequest :: Newtype DescribeConfigurationRecorderStatusRequest _


-- | <p>The output for the <a>DescribeConfigurationRecorderStatus</a> action in JSON format.</p>
newtype DescribeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse 
  { "ConfigurationRecordersStatus" :: NullOrUndefined (ConfigurationRecorderStatusList)
  }
derive instance newtypeDescribeConfigurationRecorderStatusResponse :: Newtype DescribeConfigurationRecorderStatusResponse _


-- | <p>The input for the <a>DescribeConfigurationRecorders</a> action.</p>
newtype DescribeConfigurationRecordersRequest = DescribeConfigurationRecordersRequest 
  { "ConfigurationRecorderNames" :: NullOrUndefined (ConfigurationRecorderNameList)
  }
derive instance newtypeDescribeConfigurationRecordersRequest :: Newtype DescribeConfigurationRecordersRequest _


-- | <p>The output for the <a>DescribeConfigurationRecorders</a> action.</p>
newtype DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse 
  { "ConfigurationRecorders" :: NullOrUndefined (ConfigurationRecorderList)
  }
derive instance newtypeDescribeConfigurationRecordersResponse :: Newtype DescribeConfigurationRecordersResponse _


-- | <p>The input for the <a>DeliveryChannelStatus</a> action.</p>
newtype DescribeDeliveryChannelStatusRequest = DescribeDeliveryChannelStatusRequest 
  { "DeliveryChannelNames" :: NullOrUndefined (DeliveryChannelNameList)
  }
derive instance newtypeDescribeDeliveryChannelStatusRequest :: Newtype DescribeDeliveryChannelStatusRequest _


-- | <p>The output for the <a>DescribeDeliveryChannelStatus</a> action.</p>
newtype DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse 
  { "DeliveryChannelsStatus" :: NullOrUndefined (DeliveryChannelStatusList)
  }
derive instance newtypeDescribeDeliveryChannelStatusResponse :: Newtype DescribeDeliveryChannelStatusResponse _


-- | <p>The input for the <a>DescribeDeliveryChannels</a> action.</p>
newtype DescribeDeliveryChannelsRequest = DescribeDeliveryChannelsRequest 
  { "DeliveryChannelNames" :: NullOrUndefined (DeliveryChannelNameList)
  }
derive instance newtypeDescribeDeliveryChannelsRequest :: Newtype DescribeDeliveryChannelsRequest _


-- | <p>The output for the <a>DescribeDeliveryChannels</a> action.</p>
newtype DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse 
  { "DeliveryChannels" :: NullOrUndefined (DeliveryChannelList)
  }
derive instance newtypeDescribeDeliveryChannelsResponse :: Newtype DescribeDeliveryChannelsResponse _


newtype EarlierTime = EarlierTime Number
derive instance newtypeEarlierTime :: Newtype EarlierTime _


newtype EmptiableStringWithCharLimit256 = EmptiableStringWithCharLimit256 String
derive instance newtypeEmptiableStringWithCharLimit256 :: Newtype EmptiableStringWithCharLimit256 _


-- | <p>Identifies an AWS resource and indicates whether it complies with the AWS Config rule that it was evaluated against.</p>
newtype Evaluation = Evaluation 
  { "ComplianceResourceType" :: (StringWithCharLimit256)
  , "ComplianceResourceId" :: (BaseResourceId)
  , "ComplianceType" :: (ComplianceType)
  , "Annotation" :: NullOrUndefined (StringWithCharLimit256)
  , "OrderingTimestamp" :: (OrderingTimestamp)
  }
derive instance newtypeEvaluation :: Newtype Evaluation _


-- | <p>The details of an AWS Config evaluation. Provides the AWS resource that was evaluated, the compliance of the resource, related timestamps, and supplementary information.</p>
newtype EvaluationResult = EvaluationResult 
  { "EvaluationResultIdentifier" :: NullOrUndefined (EvaluationResultIdentifier)
  , "ComplianceType" :: NullOrUndefined (ComplianceType)
  , "ResultRecordedTime" :: NullOrUndefined (Date)
  , "ConfigRuleInvokedTime" :: NullOrUndefined (Date)
  , "Annotation" :: NullOrUndefined (StringWithCharLimit256)
  , "ResultToken" :: NullOrUndefined (String)
  }
derive instance newtypeEvaluationResult :: Newtype EvaluationResult _


-- | <p>Uniquely identifies an evaluation result.</p>
newtype EvaluationResultIdentifier = EvaluationResultIdentifier 
  { "EvaluationResultQualifier" :: NullOrUndefined (EvaluationResultQualifier)
  , "OrderingTimestamp" :: NullOrUndefined (Date)
  }
derive instance newtypeEvaluationResultIdentifier :: Newtype EvaluationResultIdentifier _


-- | <p>Identifies an AWS Config rule that evaluated an AWS resource, and provides the type and ID of the resource that the rule evaluated.</p>
newtype EvaluationResultQualifier = EvaluationResultQualifier 
  { "ConfigRuleName" :: NullOrUndefined (StringWithCharLimit64)
  , "ResourceType" :: NullOrUndefined (StringWithCharLimit256)
  , "ResourceId" :: NullOrUndefined (BaseResourceId)
  }
derive instance newtypeEvaluationResultQualifier :: Newtype EvaluationResultQualifier _


newtype EvaluationResults = EvaluationResults (Array EvaluationResult)
derive instance newtypeEvaluationResults :: Newtype EvaluationResults _


newtype Evaluations = Evaluations (Array Evaluation)
derive instance newtypeEvaluations :: Newtype Evaluations _


newtype EventSource = EventSource String
derive instance newtypeEventSource :: Newtype EventSource _


-- | <p/>
newtype GetComplianceDetailsByConfigRuleRequest = GetComplianceDetailsByConfigRuleRequest 
  { "ConfigRuleName" :: (StringWithCharLimit64)
  , "ComplianceTypes" :: NullOrUndefined (ComplianceTypes)
  , "Limit" :: NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetComplianceDetailsByConfigRuleRequest :: Newtype GetComplianceDetailsByConfigRuleRequest _


-- | <p/>
newtype GetComplianceDetailsByConfigRuleResponse = GetComplianceDetailsByConfigRuleResponse 
  { "EvaluationResults" :: NullOrUndefined (EvaluationResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetComplianceDetailsByConfigRuleResponse :: Newtype GetComplianceDetailsByConfigRuleResponse _


-- | <p/>
newtype GetComplianceDetailsByResourceRequest = GetComplianceDetailsByResourceRequest 
  { "ResourceType" :: (StringWithCharLimit256)
  , "ResourceId" :: (BaseResourceId)
  , "ComplianceTypes" :: NullOrUndefined (ComplianceTypes)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetComplianceDetailsByResourceRequest :: Newtype GetComplianceDetailsByResourceRequest _


-- | <p/>
newtype GetComplianceDetailsByResourceResponse = GetComplianceDetailsByResourceResponse 
  { "EvaluationResults" :: NullOrUndefined (EvaluationResults)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeGetComplianceDetailsByResourceResponse :: Newtype GetComplianceDetailsByResourceResponse _


-- | <p/>
newtype GetComplianceSummaryByConfigRuleResponse = GetComplianceSummaryByConfigRuleResponse 
  { "ComplianceSummary" :: NullOrUndefined (ComplianceSummary)
  }
derive instance newtypeGetComplianceSummaryByConfigRuleResponse :: Newtype GetComplianceSummaryByConfigRuleResponse _


-- | <p/>
newtype GetComplianceSummaryByResourceTypeRequest = GetComplianceSummaryByResourceTypeRequest 
  { "ResourceTypes" :: NullOrUndefined (ResourceTypes)
  }
derive instance newtypeGetComplianceSummaryByResourceTypeRequest :: Newtype GetComplianceSummaryByResourceTypeRequest _


-- | <p/>
newtype GetComplianceSummaryByResourceTypeResponse = GetComplianceSummaryByResourceTypeResponse 
  { "ComplianceSummariesByResourceType" :: NullOrUndefined (ComplianceSummariesByResourceType)
  }
derive instance newtypeGetComplianceSummaryByResourceTypeResponse :: Newtype GetComplianceSummaryByResourceTypeResponse _


newtype GetDiscoveredResourceCountsRequest = GetDiscoveredResourceCountsRequest 
  { "ResourceTypes'" :: NullOrUndefined (ResourceTypes)
  , "Limit'" :: NullOrUndefined (Limit)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetDiscoveredResourceCountsRequest :: Newtype GetDiscoveredResourceCountsRequest _


newtype GetDiscoveredResourceCountsResponse = GetDiscoveredResourceCountsResponse 
  { "TotalDiscoveredResources'" :: NullOrUndefined (Number)
  , "ResourceCounts'" :: NullOrUndefined (ResourceCounts)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetDiscoveredResourceCountsResponse :: Newtype GetDiscoveredResourceCountsResponse _


-- | <p>The input for the <a>GetResourceConfigHistory</a> action.</p>
newtype GetResourceConfigHistoryRequest = GetResourceConfigHistoryRequest 
  { "ResourceType'" :: (ResourceType)
  , "ResourceId'" :: (ResourceId)
  , "LaterTime'" :: NullOrUndefined (LaterTime)
  , "EarlierTime'" :: NullOrUndefined (EarlierTime)
  , "ChronologicalOrder'" :: NullOrUndefined (ChronologicalOrder)
  , "Limit'" :: NullOrUndefined (Limit)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetResourceConfigHistoryRequest :: Newtype GetResourceConfigHistoryRequest _


-- | <p>The output for the <a>GetResourceConfigHistory</a> action.</p>
newtype GetResourceConfigHistoryResponse = GetResourceConfigHistoryResponse 
  { "ConfigurationItems'" :: NullOrUndefined (ConfigurationItemList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetResourceConfigHistoryResponse :: Newtype GetResourceConfigHistoryResponse _


newtype IncludeGlobalResourceTypes = IncludeGlobalResourceTypes Boolean
derive instance newtypeIncludeGlobalResourceTypes :: Newtype IncludeGlobalResourceTypes _


-- | <p>Your Amazon S3 bucket policy does not permit AWS Config to write to it.</p>
newtype InsufficientDeliveryPolicyException = InsufficientDeliveryPolicyException 
  { 
  }
derive instance newtypeInsufficientDeliveryPolicyException :: Newtype InsufficientDeliveryPolicyException _


-- | <p>Indicates one of the following errors:</p> <ul> <li> <p>The rule cannot be created because the IAM role assigned to AWS Config lacks permissions to perform the config:Put* action.</p> </li> <li> <p>The AWS Lambda function cannot be invoked. Check the function ARN, and check the function's permissions.</p> </li> </ul>
newtype InsufficientPermissionsException = InsufficientPermissionsException 
  { 
  }
derive instance newtypeInsufficientPermissionsException :: Newtype InsufficientPermissionsException _


-- | <p>You have provided a configuration recorder name that is not valid.</p>
newtype InvalidConfigurationRecorderNameException = InvalidConfigurationRecorderNameException 
  { 
  }
derive instance newtypeInvalidConfigurationRecorderNameException :: Newtype InvalidConfigurationRecorderNameException _


-- | <p>The specified delivery channel name is not valid.</p>
newtype InvalidDeliveryChannelNameException = InvalidDeliveryChannelNameException 
  { 
  }
derive instance newtypeInvalidDeliveryChannelNameException :: Newtype InvalidDeliveryChannelNameException _


-- | <p>The specified limit is outside the allowable range.</p>
newtype InvalidLimitException = InvalidLimitException 
  { 
  }
derive instance newtypeInvalidLimitException :: Newtype InvalidLimitException _


-- | <p>The specified next token is invalid. Specify the <code>NextToken</code> string that was returned in the previous response to get the next page of results.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { 
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>One or more of the specified parameters are invalid. Verify that your parameters are valid and try again.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { 
  }
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _


-- | <p>AWS Config throws an exception if the recording group does not contain a valid list of resource types. Invalid values could also be incorrectly formatted.</p>
newtype InvalidRecordingGroupException = InvalidRecordingGroupException 
  { 
  }
derive instance newtypeInvalidRecordingGroupException :: Newtype InvalidRecordingGroupException _


-- | <p>The specified <code>ResultToken</code> is invalid.</p>
newtype InvalidResultTokenException = InvalidResultTokenException 
  { 
  }
derive instance newtypeInvalidResultTokenException :: Newtype InvalidResultTokenException _


-- | <p>You have provided a null or empty role ARN.</p>
newtype InvalidRoleException = InvalidRoleException 
  { 
  }
derive instance newtypeInvalidRoleException :: Newtype InvalidRoleException _


-- | <p>The specified Amazon S3 key prefix is not valid.</p>
newtype InvalidS3KeyPrefixException = InvalidS3KeyPrefixException 
  { 
  }
derive instance newtypeInvalidS3KeyPrefixException :: Newtype InvalidS3KeyPrefixException _


-- | <p>The specified Amazon SNS topic does not exist.</p>
newtype InvalidSNSTopicARNException = InvalidSNSTopicARNException 
  { 
  }
derive instance newtypeInvalidSNSTopicARNException :: Newtype InvalidSNSTopicARNException _


-- | <p>The specified time range is not valid. The earlier time is not chronologically before the later time.</p>
newtype InvalidTimeRangeException = InvalidTimeRangeException 
  { 
  }
derive instance newtypeInvalidTimeRangeException :: Newtype InvalidTimeRangeException _


-- | <p>You cannot delete the delivery channel you specified because the configuration recorder is running.</p>
newtype LastDeliveryChannelDeleteFailedException = LastDeliveryChannelDeleteFailedException 
  { 
  }
derive instance newtypeLastDeliveryChannelDeleteFailedException :: Newtype LastDeliveryChannelDeleteFailedException _


newtype LaterTime = LaterTime Number
derive instance newtypeLaterTime :: Newtype LaterTime _


newtype Limit = Limit Int
derive instance newtypeLimit :: Newtype Limit _


-- | <p>This exception is thrown if an evaluation is in progress or if you call the <a>StartConfigRulesEvaluation</a> API more than once per minute.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p/>
newtype ListDiscoveredResourcesRequest = ListDiscoveredResourcesRequest 
  { "ResourceType'" :: (ResourceType)
  , "ResourceIds'" :: NullOrUndefined (ResourceIdList)
  , "ResourceName'" :: NullOrUndefined (ResourceName)
  , "Limit'" :: NullOrUndefined (Limit)
  , "IncludeDeletedResources'" :: NullOrUndefined (Boolean)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDiscoveredResourcesRequest :: Newtype ListDiscoveredResourcesRequest _


-- | <p/>
newtype ListDiscoveredResourcesResponse = ListDiscoveredResourcesResponse 
  { "ResourceIdentifiers'" :: NullOrUndefined (ResourceIdentifierList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDiscoveredResourcesResponse :: Newtype ListDiscoveredResourcesResponse _


-- | <p>Failed to add the AWS Config rule because the account already contains the maximum number of 50 rules. Consider deleting any deactivated rules before adding new rules.</p>
newtype MaxNumberOfConfigRulesExceededException = MaxNumberOfConfigRulesExceededException 
  { 
  }
derive instance newtypeMaxNumberOfConfigRulesExceededException :: Newtype MaxNumberOfConfigRulesExceededException _


-- | <p>You have reached the limit on the number of recorders you can create.</p>
newtype MaxNumberOfConfigurationRecordersExceededException = MaxNumberOfConfigurationRecordersExceededException 
  { 
  }
derive instance newtypeMaxNumberOfConfigurationRecordersExceededException :: Newtype MaxNumberOfConfigurationRecordersExceededException _


-- | <p>You have reached the limit on the number of delivery channels you can create.</p>
newtype MaxNumberOfDeliveryChannelsExceededException = MaxNumberOfDeliveryChannelsExceededException 
  { 
  }
derive instance newtypeMaxNumberOfDeliveryChannelsExceededException :: Newtype MaxNumberOfDeliveryChannelsExceededException _


newtype MaximumExecutionFrequency = MaximumExecutionFrequency String
derive instance newtypeMaximumExecutionFrequency :: Newtype MaximumExecutionFrequency _


newtype MessageType = MessageType String
derive instance newtypeMessageType :: Newtype MessageType _


newtype Name = Name String
derive instance newtypeName :: Newtype Name _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>There are no configuration recorders available to provide the role needed to describe your resources. Create a configuration recorder.</p>
newtype NoAvailableConfigurationRecorderException = NoAvailableConfigurationRecorderException 
  { 
  }
derive instance newtypeNoAvailableConfigurationRecorderException :: Newtype NoAvailableConfigurationRecorderException _


-- | <p>There is no delivery channel available to record configurations.</p>
newtype NoAvailableDeliveryChannelException = NoAvailableDeliveryChannelException 
  { 
  }
derive instance newtypeNoAvailableDeliveryChannelException :: Newtype NoAvailableDeliveryChannelException _


-- | <p>There is no configuration recorder running.</p>
newtype NoRunningConfigurationRecorderException = NoRunningConfigurationRecorderException 
  { 
  }
derive instance newtypeNoRunningConfigurationRecorderException :: Newtype NoRunningConfigurationRecorderException _


-- | <p>The specified Amazon S3 bucket does not exist.</p>
newtype NoSuchBucketException = NoSuchBucketException 
  { 
  }
derive instance newtypeNoSuchBucketException :: Newtype NoSuchBucketException _


-- | <p>One or more AWS Config rules in the request are invalid. Verify that the rule names are correct and try again.</p>
newtype NoSuchConfigRuleException = NoSuchConfigRuleException 
  { 
  }
derive instance newtypeNoSuchConfigRuleException :: Newtype NoSuchConfigRuleException _


-- | <p>You have specified a configuration recorder that does not exist.</p>
newtype NoSuchConfigurationRecorderException = NoSuchConfigurationRecorderException 
  { 
  }
derive instance newtypeNoSuchConfigurationRecorderException :: Newtype NoSuchConfigurationRecorderException _


-- | <p>You have specified a delivery channel that does not exist.</p>
newtype NoSuchDeliveryChannelException = NoSuchDeliveryChannelException 
  { 
  }
derive instance newtypeNoSuchDeliveryChannelException :: Newtype NoSuchDeliveryChannelException _


newtype OrderingTimestamp = OrderingTimestamp Number
derive instance newtypeOrderingTimestamp :: Newtype OrderingTimestamp _


newtype Owner = Owner String
derive instance newtypeOwner :: Newtype Owner _


newtype PutConfigRuleRequest = PutConfigRuleRequest 
  { "ConfigRule" :: (ConfigRule)
  }
derive instance newtypePutConfigRuleRequest :: Newtype PutConfigRuleRequest _


-- | <p>The input for the <a>PutConfigurationRecorder</a> action.</p>
newtype PutConfigurationRecorderRequest = PutConfigurationRecorderRequest 
  { "ConfigurationRecorder" :: (ConfigurationRecorder)
  }
derive instance newtypePutConfigurationRecorderRequest :: Newtype PutConfigurationRecorderRequest _


-- | <p>The input for the <a>PutDeliveryChannel</a> action.</p>
newtype PutDeliveryChannelRequest = PutDeliveryChannelRequest 
  { "DeliveryChannel" :: (DeliveryChannel)
  }
derive instance newtypePutDeliveryChannelRequest :: Newtype PutDeliveryChannelRequest _


-- | <p/>
newtype PutEvaluationsRequest = PutEvaluationsRequest 
  { "Evaluations" :: NullOrUndefined (Evaluations)
  , "ResultToken" :: (String)
  , "TestMode" :: NullOrUndefined (Boolean)
  }
derive instance newtypePutEvaluationsRequest :: Newtype PutEvaluationsRequest _


-- | <p/>
newtype PutEvaluationsResponse = PutEvaluationsResponse 
  { "FailedEvaluations" :: NullOrUndefined (Evaluations)
  }
derive instance newtypePutEvaluationsResponse :: Newtype PutEvaluationsResponse _


newtype RecorderName = RecorderName String
derive instance newtypeRecorderName :: Newtype RecorderName _


newtype RecorderStatus = RecorderStatus String
derive instance newtypeRecorderStatus :: Newtype RecorderStatus _


-- | <p>Specifies the types of AWS resource for which AWS Config records configuration changes.</p> <p>In the recording group, you specify whether all supported types or specific types of resources are recorded.</p> <p>By default, AWS Config records configuration changes for all supported types of regional resources that AWS Config discovers in the region in which it is running. Regional resources are tied to a region and can be used only in that region. Examples of regional resources are EC2 instances and EBS volumes.</p> <p>You can also have AWS Config record configuration changes for supported types of global resources (for example, IAM resources). Global resources are not tied to an individual region and can be used in all regions.</p> <important> <p>The configuration details for any global resource are the same in all regions. If you customize AWS Config in multiple regions to record global resources, it will create multiple configuration items each time a global resource changes: one configuration item for each region. These configuration items will contain identical data. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources, unless you want the configuration items to be available in multiple regions.</p> </important> <p>If you don't want AWS Config to record all resources, you can specify which types of resources it will record with the <code>resourceTypes</code> parameter.</p> <p>For a list of supported resource types, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources">Supported resource types</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/select-resources.html">Selecting Which Resources AWS Config Records</a>.</p>
newtype RecordingGroup = RecordingGroup 
  { "AllSupported'" :: NullOrUndefined (AllSupported)
  , "IncludeGlobalResourceTypes'" :: NullOrUndefined (IncludeGlobalResourceTypes)
  , "ResourceTypes'" :: NullOrUndefined (ResourceTypeList)
  }
derive instance newtypeRecordingGroup :: Newtype RecordingGroup _


newtype ReevaluateConfigRuleNames = ReevaluateConfigRuleNames (Array StringWithCharLimit64)
derive instance newtypeReevaluateConfigRuleNames :: Newtype ReevaluateConfigRuleNames _


newtype RelatedEvent = RelatedEvent String
derive instance newtypeRelatedEvent :: Newtype RelatedEvent _


newtype RelatedEventList = RelatedEventList (Array RelatedEvent)
derive instance newtypeRelatedEventList :: Newtype RelatedEventList _


-- | <p>The relationship of the related resource to the main resource.</p>
newtype Relationship = Relationship 
  { "ResourceType'" :: NullOrUndefined (ResourceType)
  , "ResourceId'" :: NullOrUndefined (ResourceId)
  , "ResourceName'" :: NullOrUndefined (ResourceName)
  , "RelationshipName'" :: NullOrUndefined (RelationshipName)
  }
derive instance newtypeRelationship :: Newtype Relationship _


newtype RelationshipList = RelationshipList (Array Relationship)
derive instance newtypeRelationshipList :: Newtype RelationshipList _


newtype RelationshipName = RelationshipName String
derive instance newtypeRelationshipName :: Newtype RelationshipName _


-- | <p>An object that contains the resource type and the number of resources.</p>
newtype ResourceCount = ResourceCount 
  { "ResourceType'" :: NullOrUndefined (ResourceType)
  , "Count'" :: NullOrUndefined (Number)
  }
derive instance newtypeResourceCount :: Newtype ResourceCount _


newtype ResourceCounts = ResourceCounts (Array ResourceCount)
derive instance newtypeResourceCounts :: Newtype ResourceCounts _


newtype ResourceCreationTime = ResourceCreationTime Number
derive instance newtypeResourceCreationTime :: Newtype ResourceCreationTime _


newtype ResourceDeletionTime = ResourceDeletionTime Number
derive instance newtypeResourceDeletionTime :: Newtype ResourceDeletionTime _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


newtype ResourceIdList = ResourceIdList (Array ResourceId)
derive instance newtypeResourceIdList :: Newtype ResourceIdList _


-- | <p>The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.</p>
newtype ResourceIdentifier = ResourceIdentifier 
  { "ResourceType'" :: NullOrUndefined (ResourceType)
  , "ResourceId'" :: NullOrUndefined (ResourceId)
  , "ResourceName'" :: NullOrUndefined (ResourceName)
  , "ResourceDeletionTime'" :: NullOrUndefined (ResourceDeletionTime)
  }
derive instance newtypeResourceIdentifier :: Newtype ResourceIdentifier _


newtype ResourceIdentifierList = ResourceIdentifierList (Array ResourceIdentifier)
derive instance newtypeResourceIdentifierList :: Newtype ResourceIdentifierList _


-- | <p>The rule is currently being deleted or the rule is deleting your evaluation results. Try your request again later.</p>
newtype ResourceInUseException = ResourceInUseException 
  { 
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _


-- | <p>You have specified a resource that is either unknown or has not been discovered.</p>
newtype ResourceNotDiscoveredException = ResourceNotDiscoveredException 
  { 
  }
derive instance newtypeResourceNotDiscoveredException :: Newtype ResourceNotDiscoveredException _


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


newtype ResourceTypeList = ResourceTypeList (Array ResourceType)
derive instance newtypeResourceTypeList :: Newtype ResourceTypeList _


newtype ResourceTypes = ResourceTypes (Array StringWithCharLimit256)
derive instance newtypeResourceTypes :: Newtype ResourceTypes _


newtype RuleLimit = RuleLimit Int
derive instance newtypeRuleLimit :: Newtype RuleLimit _


-- | <p>Defines which resources trigger an evaluation for an AWS Config rule. The scope can include one or more resource types, a combination of a tag key and value, or a combination of one resource type and one resource ID. Specify a scope to constrain which resources trigger an evaluation for a rule. Otherwise, evaluations for the rule are triggered when any resource in your recording group changes in configuration.</p>
newtype Scope = Scope 
  { "ComplianceResourceTypes" :: NullOrUndefined (ComplianceResourceTypes)
  , "TagKey" :: NullOrUndefined (StringWithCharLimit128)
  , "TagValue" :: NullOrUndefined (StringWithCharLimit256)
  , "ComplianceResourceId" :: NullOrUndefined (BaseResourceId)
  }
derive instance newtypeScope :: Newtype Scope _


-- | <p>Provides the AWS Config rule owner (AWS or customer), the rule identifier, and the events that trigger the evaluation of your AWS resources.</p>
newtype Source = Source 
  { "Owner" :: (Owner)
  , "SourceIdentifier" :: (StringWithCharLimit256)
  , "SourceDetails" :: NullOrUndefined (SourceDetails)
  }
derive instance newtypeSource :: Newtype Source _


-- | <p>Provides the source and the message types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic. You can specify the parameter values for <code>SourceDetail</code> only for custom rules. </p>
newtype SourceDetail = SourceDetail 
  { "EventSource" :: NullOrUndefined (EventSource)
  , "MessageType" :: NullOrUndefined (MessageType)
  , "MaximumExecutionFrequency" :: NullOrUndefined (MaximumExecutionFrequency)
  }
derive instance newtypeSourceDetail :: Newtype SourceDetail _


newtype SourceDetails = SourceDetails (Array SourceDetail)
derive instance newtypeSourceDetails :: Newtype SourceDetails _


-- | <p/>
newtype StartConfigRulesEvaluationRequest = StartConfigRulesEvaluationRequest 
  { "ConfigRuleNames" :: NullOrUndefined (ReevaluateConfigRuleNames)
  }
derive instance newtypeStartConfigRulesEvaluationRequest :: Newtype StartConfigRulesEvaluationRequest _


-- | <p>The output when you start the evaluation for the specified Config rule.</p>
newtype StartConfigRulesEvaluationResponse = StartConfigRulesEvaluationResponse 
  { 
  }
derive instance newtypeStartConfigRulesEvaluationResponse :: Newtype StartConfigRulesEvaluationResponse _


-- | <p>The input for the <a>StartConfigurationRecorder</a> action.</p>
newtype StartConfigurationRecorderRequest = StartConfigurationRecorderRequest 
  { "ConfigurationRecorderName" :: (RecorderName)
  }
derive instance newtypeStartConfigurationRecorderRequest :: Newtype StartConfigurationRecorderRequest _


-- | <p>The input for the <a>StopConfigurationRecorder</a> action.</p>
newtype StopConfigurationRecorderRequest = StopConfigurationRecorderRequest 
  { "ConfigurationRecorderName" :: (RecorderName)
  }
derive instance newtypeStopConfigurationRecorderRequest :: Newtype StopConfigurationRecorderRequest _


newtype StringWithCharLimit1024 = StringWithCharLimit1024 String
derive instance newtypeStringWithCharLimit1024 :: Newtype StringWithCharLimit1024 _


newtype StringWithCharLimit128 = StringWithCharLimit128 String
derive instance newtypeStringWithCharLimit128 :: Newtype StringWithCharLimit128 _


newtype StringWithCharLimit256 = StringWithCharLimit256 String
derive instance newtypeStringWithCharLimit256 :: Newtype StringWithCharLimit256 _


newtype StringWithCharLimit64 = StringWithCharLimit64 String
derive instance newtypeStringWithCharLimit64 :: Newtype StringWithCharLimit64 _


newtype SupplementaryConfiguration = SupplementaryConfiguration (Map SupplementaryConfigurationName SupplementaryConfigurationValue)
derive instance newtypeSupplementaryConfiguration :: Newtype SupplementaryConfiguration _


newtype SupplementaryConfigurationName = SupplementaryConfigurationName String
derive instance newtypeSupplementaryConfigurationName :: Newtype SupplementaryConfigurationName _


newtype SupplementaryConfigurationValue = SupplementaryConfigurationValue String
derive instance newtypeSupplementaryConfigurationValue :: Newtype SupplementaryConfigurationValue _


newtype Tags = Tags (Map Name Value)
derive instance newtypeTags :: Newtype Tags _


-- | <p>The requested action is not valid.</p>
newtype ValidationException = ValidationException 
  { 
  }
derive instance newtypeValidationException :: Newtype ValidationException _


newtype Value = Value String
derive instance newtypeValue :: Newtype Value _


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
