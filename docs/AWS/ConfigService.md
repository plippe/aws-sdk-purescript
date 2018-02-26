## Module AWS.ConfigService

<fullname>AWS Config</fullname> <p>AWS Config provides a way to keep track of the configurations of all the AWS resources associated with your AWS account. You can use AWS Config to get the current and historical configurations of each AWS resource and also to get information about the relationship between the resources. An AWS resource can be an Amazon Compute Cloud (Amazon EC2) instance, an Elastic Block Store (EBS) volume, an Elastic network Interface (ENI), or a security group. For a complete list of resources currently supported by AWS Config, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources">Supported AWS Resources</a>.</p> <p>You can access and manage AWS Config through the AWS Management Console, the AWS Command Line Interface (AWS CLI), the AWS Config API, or the AWS SDKs for AWS Config</p> <p>This reference guide contains documentation for the AWS Config API and the AWS CLI commands that you can use to manage AWS Config.</p> <p>The AWS Config API uses the Signature Version 4 protocol for signing requests. For more information about how to sign a request with this protocol, see <a href="http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html">Signature Version 4 Signing Process</a>.</p> <p>For detailed information about AWS Config features and their associated actions or commands, as well as how to work with AWS Management Console, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/WhatIsConfig.html">What Is AWS Config?</a> in the <i>AWS Config Developer Guide</i>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `deleteConfigRule`

``` purescript
deleteConfigRule :: forall eff. DeleteConfigRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified AWS Config rule and all of its evaluation results.</p> <p>AWS Config sets the state of a rule to <code>DELETING</code> until the deletion is complete. You cannot update a rule while it is in this state. If you make a <code>PutConfigRule</code> or <code>DeleteConfigRule</code> request for the rule, you will receive a <code>ResourceInUseException</code>.</p> <p>You can check the state of a rule by using the <code>DescribeConfigRules</code> request.</p>

#### `deleteConfigurationRecorder`

``` purescript
deleteConfigurationRecorder :: forall eff. DeleteConfigurationRecorderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the configuration recorder.</p> <p>After the configuration recorder is deleted, AWS Config will not record resource configuration changes until you create a new configuration recorder.</p> <p>This action does not delete the configuration information that was previously recorded. You will be able to access the previously recorded information by using the <code>GetResourceConfigHistory</code> action, but you will not be able to access this information in the AWS Config console until you create a new configuration recorder.</p>

#### `deleteDeliveryChannel`

``` purescript
deleteDeliveryChannel :: forall eff. DeleteDeliveryChannelRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the delivery channel.</p> <p>Before you can delete the delivery channel, you must stop the configuration recorder by using the <a>StopConfigurationRecorder</a> action.</p>

#### `deleteEvaluationResults`

``` purescript
deleteEvaluationResults :: forall eff. DeleteEvaluationResultsRequest -> Aff (err :: RequestError | eff) DeleteEvaluationResultsResponse
```

<p>Deletes the evaluation results for the specified Config rule. You can specify one Config rule per request. After you delete the evaluation results, you can call the <a>StartConfigRulesEvaluation</a> API to start evaluating your AWS resources against the rule.</p>

#### `deliverConfigSnapshot`

``` purescript
deliverConfigSnapshot :: forall eff. DeliverConfigSnapshotRequest -> Aff (err :: RequestError | eff) DeliverConfigSnapshotResponse
```

<p>Schedules delivery of a configuration snapshot to the Amazon S3 bucket in the specified delivery channel. After the delivery has started, AWS Config sends following notifications using an Amazon SNS topic that you have specified.</p> <ul> <li> <p>Notification of starting the delivery.</p> </li> <li> <p>Notification of delivery completed, if the delivery was successfully completed.</p> </li> <li> <p>Notification of delivery failure, if the delivery failed to complete.</p> </li> </ul>

#### `describeComplianceByConfigRule`

``` purescript
describeComplianceByConfigRule :: forall eff. DescribeComplianceByConfigRuleRequest -> Aff (err :: RequestError | eff) DescribeComplianceByConfigRuleResponse
```

<p>Indicates whether the specified AWS Config rules are compliant. If a rule is noncompliant, this action returns the number of AWS resources that do not comply with the rule.</p> <p>A rule is compliant if all of the evaluated resources comply with it, and it is noncompliant if any of these resources do not comply.</p> <p>If AWS Config has no current evaluation results for the rule, it returns <code>INSUFFICIENT_DATA</code>. This result might indicate one of the following conditions:</p> <ul> <li> <p>AWS Config has never invoked an evaluation for the rule. To check whether it has, use the <code>DescribeConfigRuleEvaluationStatus</code> action to get the <code>LastSuccessfulInvocationTime</code> and <code>LastFailedInvocationTime</code>.</p> </li> <li> <p>The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role that you assigned to your configuration recorder includes the <code>config:PutEvaluations</code> permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the <code>config:PutEvaluations</code> permission.</p> </li> <li> <p>The rule's AWS Lambda function has returned <code>NOT_APPLICABLE</code> for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.</p> </li> </ul>

#### `describeComplianceByResource`

``` purescript
describeComplianceByResource :: forall eff. DescribeComplianceByResourceRequest -> Aff (err :: RequestError | eff) DescribeComplianceByResourceResponse
```

<p>Indicates whether the specified AWS resources are compliant. If a resource is noncompliant, this action returns the number of AWS Config rules that the resource does not comply with.</p> <p>A resource is compliant if it complies with all the AWS Config rules that evaluate it. It is noncompliant if it does not comply with one or more of these rules.</p> <p>If AWS Config has no current evaluation results for the resource, it returns <code>INSUFFICIENT_DATA</code>. This result might indicate one of the following conditions about the rules that evaluate the resource:</p> <ul> <li> <p>AWS Config has never invoked an evaluation for the rule. To check whether it has, use the <code>DescribeConfigRuleEvaluationStatus</code> action to get the <code>LastSuccessfulInvocationTime</code> and <code>LastFailedInvocationTime</code>.</p> </li> <li> <p>The rule's AWS Lambda function is failing to send evaluation results to AWS Config. Verify that the role that you assigned to your configuration recorder includes the <code>config:PutEvaluations</code> permission. If the rule is a custom rule, verify that the AWS Lambda execution role includes the <code>config:PutEvaluations</code> permission.</p> </li> <li> <p>The rule's AWS Lambda function has returned <code>NOT_APPLICABLE</code> for all evaluation results. This can occur if the resources were deleted or removed from the rule's scope.</p> </li> </ul>

#### `describeConfigRuleEvaluationStatus`

``` purescript
describeConfigRuleEvaluationStatus :: forall eff. DescribeConfigRuleEvaluationStatusRequest -> Aff (err :: RequestError | eff) DescribeConfigRuleEvaluationStatusResponse
```

<p>Returns status information for each of your AWS managed Config rules. The status includes information such as the last time AWS Config invoked the rule, the last time AWS Config failed to invoke the rule, and the related error for the last failure.</p>

#### `describeConfigRules`

``` purescript
describeConfigRules :: forall eff. DescribeConfigRulesRequest -> Aff (err :: RequestError | eff) DescribeConfigRulesResponse
```

<p>Returns details about your AWS Config rules.</p>

#### `describeConfigurationRecorderStatus`

``` purescript
describeConfigurationRecorderStatus :: forall eff. DescribeConfigurationRecorderStatusRequest -> Aff (err :: RequestError | eff) DescribeConfigurationRecorderStatusResponse
```

<p>Returns the current status of the specified configuration recorder. If a configuration recorder is not specified, this action returns the status of all configuration recorder associated with the account.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> </note>

#### `describeConfigurationRecorders`

``` purescript
describeConfigurationRecorders :: forall eff. DescribeConfigurationRecordersRequest -> Aff (err :: RequestError | eff) DescribeConfigurationRecordersResponse
```

<p>Returns the details for the specified configuration recorders. If the configuration recorder is not specified, this action returns the details for all configuration recorders associated with the account.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> </note>

#### `describeDeliveryChannelStatus`

``` purescript
describeDeliveryChannelStatus :: forall eff. DescribeDeliveryChannelStatusRequest -> Aff (err :: RequestError | eff) DescribeDeliveryChannelStatusResponse
```

<p>Returns the current status of the specified delivery channel. If a delivery channel is not specified, this action returns the current status of all delivery channels associated with the account.</p> <note> <p>Currently, you can specify only one delivery channel per region in your account.</p> </note>

#### `describeDeliveryChannels`

``` purescript
describeDeliveryChannels :: forall eff. DescribeDeliveryChannelsRequest -> Aff (err :: RequestError | eff) DescribeDeliveryChannelsResponse
```

<p>Returns details about the specified delivery channel. If a delivery channel is not specified, this action returns the details of all delivery channels associated with the account.</p> <note> <p>Currently, you can specify only one delivery channel per region in your account.</p> </note>

#### `getComplianceDetailsByConfigRule`

``` purescript
getComplianceDetailsByConfigRule :: forall eff. GetComplianceDetailsByConfigRuleRequest -> Aff (err :: RequestError | eff) GetComplianceDetailsByConfigRuleResponse
```

<p>Returns the evaluation results for the specified AWS Config rule. The results indicate which AWS resources were evaluated by the rule, when each resource was last evaluated, and whether each resource complies with the rule.</p>

#### `getComplianceDetailsByResource`

``` purescript
getComplianceDetailsByResource :: forall eff. GetComplianceDetailsByResourceRequest -> Aff (err :: RequestError | eff) GetComplianceDetailsByResourceResponse
```

<p>Returns the evaluation results for the specified AWS resource. The results indicate which AWS Config rules were used to evaluate the resource, when each rule was last used, and whether the resource complies with each rule.</p>

#### `getComplianceSummaryByConfigRule`

``` purescript
getComplianceSummaryByConfigRule :: forall eff. Aff (err :: RequestError | eff) GetComplianceSummaryByConfigRuleResponse
```

<p>Returns the number of AWS Config rules that are compliant and noncompliant, up to a maximum of 25 for each.</p>

#### `getComplianceSummaryByResourceType`

``` purescript
getComplianceSummaryByResourceType :: forall eff. GetComplianceSummaryByResourceTypeRequest -> Aff (err :: RequestError | eff) GetComplianceSummaryByResourceTypeResponse
```

<p>Returns the number of resources that are compliant and the number that are noncompliant. You can specify one or more resource types to get these numbers for each resource type. The maximum number returned is 100.</p>

#### `getDiscoveredResourceCounts`

``` purescript
getDiscoveredResourceCounts :: forall eff. GetDiscoveredResourceCountsRequest -> Aff (err :: RequestError | eff) GetDiscoveredResourceCountsResponse
```

<p>Returns the resource types, the number of each resource type, and the total number of resources that AWS Config is recording in this region for your AWS account. </p> <p class="title"> <b>Example</b> </p> <ol> <li> <p>AWS Config is recording three resource types in the US East (Ohio) Region for your account: 25 EC2 instances, 20 IAM users, and 15 S3 buckets.</p> </li> <li> <p>You make a call to the <code>GetDiscoveredResourceCounts</code> action and specify that you want all resource types. </p> </li> <li> <p>AWS Config returns the following:</p> <ul> <li> <p>The resource types (EC2 instances, IAM users, and S3 buckets)</p> </li> <li> <p>The number of each resource type (25, 20, and 15)</p> </li> <li> <p>The total number of all resources (60)</p> </li> </ul> </li> </ol> <p>The response is paginated. By default, AWS Config lists 100 <a>ResourceCount</a> objects on each page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p> <note> <p>If you make a call to the <a>GetDiscoveredResourceCounts</a> action, you may not immediately receive resource counts in the following situations:</p> <ul> <li> <p>You are a new AWS Config customer</p> </li> <li> <p>You just enabled resource recording</p> </li> </ul> <p>It may take a few minutes for AWS Config to record and count your resources. Wait a few minutes and then retry the <a>GetDiscoveredResourceCounts</a> action. </p> </note>

#### `getResourceConfigHistory`

``` purescript
getResourceConfigHistory :: forall eff. GetResourceConfigHistoryRequest -> Aff (err :: RequestError | eff) GetResourceConfigHistoryResponse
```

<p>Returns a list of configuration items for the specified resource. The list contains details about each state of the resource during the specified time interval.</p> <p>The response is paginated. By default, AWS Config returns a limit of 10 configuration items per page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p> <note> <p>Each call to the API is limited to span a duration of seven days. It is likely that the number of records returned is smaller than the specified <code>limit</code>. In such cases, you can make another call, using the <code>nextToken</code>.</p> </note>

#### `listDiscoveredResources`

``` purescript
listDiscoveredResources :: forall eff. ListDiscoveredResourcesRequest -> Aff (err :: RequestError | eff) ListDiscoveredResourcesResponse
```

<p>Accepts a resource type and returns a list of resource identifiers for the resources of that type. A resource identifier includes the resource type, ID, and (if available) the custom resource name. The results consist of resources that AWS Config has discovered, including those that AWS Config is not currently recording. You can narrow the results to include only resources that have specific resource IDs or a resource name.</p> <note> <p>You can specify either resource IDs or a resource name but not both in the same request.</p> </note> <p>The response is paginated. By default, AWS Config lists 100 resource identifiers on each page. You can customize this number with the <code>limit</code> parameter. The response includes a <code>nextToken</code> string. To get the next page of results, run the request again and specify the string for the <code>nextToken</code> parameter.</p>

#### `putConfigRule`

``` purescript
putConfigRule :: forall eff. PutConfigRuleRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Adds or updates an AWS Config rule for evaluating whether your AWS resources comply with your desired configurations.</p> <p>You can use this action for custom Config rules and AWS managed Config rules. A custom Config rule is a rule that you develop and maintain. An AWS managed Config rule is a customizable, predefined rule that AWS Config provides.</p> <p>If you are adding a new custom Config rule, you must first create the AWS Lambda function that the rule invokes to evaluate your resources. When you use the <code>PutConfigRule</code> action to add the rule to AWS Config, you must specify the Amazon Resource Name (ARN) that AWS Lambda assigns to the function. Specify the ARN for the <code>SourceIdentifier</code> key. This key is part of the <code>Source</code> object, which is part of the <code>ConfigRule</code> object. </p> <p>If you are adding an AWS managed Config rule, specify the rule's identifier for the <code>SourceIdentifier</code> key. To reference AWS managed Config rule identifiers, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config_use-managed-rules.html">About AWS Managed Config Rules</a>.</p> <p>For any new rule that you add, specify the <code>ConfigRuleName</code> in the <code>ConfigRule</code> object. Do not specify the <code>ConfigRuleArn</code> or the <code>ConfigRuleId</code>. These values are generated by AWS Config for new rules.</p> <p>If you are updating a rule that you added previously, you can specify the rule by <code>ConfigRuleName</code>, <code>ConfigRuleId</code>, or <code>ConfigRuleArn</code> in the <code>ConfigRule</code> data type that you use in this request.</p> <p>The maximum number of rules that AWS Config supports is 50.</p> <p>For more information about requesting a rule limit increase, see <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html#limits_config">AWS Config Limits</a> in the <i>AWS General Reference Guide</i>.</p> <p>For more information about developing and using AWS Config rules, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html">Evaluating AWS Resource Configurations with AWS Config</a> in the <i>AWS Config Developer Guide</i>.</p>

#### `putConfigurationRecorder`

``` purescript
putConfigurationRecorder :: forall eff. PutConfigurationRecorderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a new configuration recorder to record the selected resource configurations.</p> <p>You can use this action to change the role <code>roleARN</code> and/or the <code>recordingGroup</code> of an existing recorder. To change the role, call the action on the existing configuration recorder and specify a role.</p> <note> <p>Currently, you can specify only one configuration recorder per region in your account.</p> <p>If <code>ConfigurationRecorder</code> does not have the <b>recordingGroup</b> parameter specified, the default is to record all supported resource types.</p> </note>

#### `putDeliveryChannel`

``` purescript
putDeliveryChannel :: forall eff. PutDeliveryChannelRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Creates a delivery channel object to deliver configuration information to an Amazon S3 bucket and Amazon SNS topic.</p> <p>Before you can create a delivery channel, you must create a configuration recorder.</p> <p>You can use this action to change the Amazon S3 bucket or an Amazon SNS topic of the existing delivery channel. To change the Amazon S3 bucket or an Amazon SNS topic, call this action and specify the changed values for the S3 bucket and the SNS topic. If you specify a different value for either the S3 bucket or the SNS topic, this action will keep the existing value for the parameter that is not changed.</p> <note> <p>You can have only one delivery channel per region in your account.</p> </note>

#### `putEvaluations`

``` purescript
putEvaluations :: forall eff. PutEvaluationsRequest -> Aff (err :: RequestError | eff) PutEvaluationsResponse
```

<p>Used by an AWS Lambda function to deliver evaluation results to AWS Config. This action is required in every AWS Lambda function that is invoked by an AWS Config rule.</p>

#### `startConfigRulesEvaluation`

``` purescript
startConfigRulesEvaluation :: forall eff. StartConfigRulesEvaluationRequest -> Aff (err :: RequestError | eff) StartConfigRulesEvaluationResponse
```

<p>Runs an on-demand evaluation for the specified Config rules against the last known configuration state of the resources. Use <code>StartConfigRulesEvaluation</code> when you want to test a rule that you updated is working as expected. <code>StartConfigRulesEvaluation</code> does not re-record the latest configuration state for your resources; it re-runs an evaluation against the last known state of your resources. </p> <p>You can specify up to 25 Config rules per request. </p> <p>An existing <code>StartConfigRulesEvaluation</code> call must complete for the specified rules before you can call the API again. If you chose to have AWS Config stream to an Amazon SNS topic, you will receive a <code>ConfigRuleEvaluationStarted</code> notification when the evaluation starts.</p> <note> <p>You don't need to call the <code>StartConfigRulesEvaluation</code> API to run an evaluation for a new rule. When you create a new rule, AWS Config automatically evaluates your resources against the rule. </p> </note> <p>The <code>StartConfigRulesEvaluation</code> API is useful if you want to run on-demand evaluations, such as the following example:</p> <ol> <li> <p>You have a custom rule that evaluates your IAM resources every 24 hours.</p> </li> <li> <p>You update your Lambda function to add additional conditions to your rule.</p> </li> <li> <p>Instead of waiting for the next periodic evaluation, you call the <code>StartConfigRulesEvaluation</code> API.</p> </li> <li> <p>AWS Config invokes your Lambda function and evaluates your IAM resources.</p> </li> <li> <p>Your custom rule will still run periodic evaluations every 24 hours.</p> </li> </ol>

#### `startConfigurationRecorder`

``` purescript
startConfigurationRecorder :: forall eff. StartConfigurationRecorderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Starts recording configurations of the AWS resources you have selected to record in your AWS account.</p> <p>You must have created at least one delivery channel to successfully start the configuration recorder.</p>

#### `stopConfigurationRecorder`

``` purescript
stopConfigurationRecorder :: forall eff. StopConfigurationRecorderRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Stops recording configurations of the AWS resources you have selected to record in your AWS account.</p>

#### `ARN`

``` purescript
newtype ARN
  = ARN String
```

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

#### `AllSupported`

``` purescript
newtype AllSupported
  = AllSupported Boolean
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone String
```

#### `AwsRegion`

``` purescript
newtype AwsRegion
  = AwsRegion String
```

#### `BaseResourceId`

``` purescript
newtype BaseResourceId
  = BaseResourceId String
```

#### `ChannelName`

``` purescript
newtype ChannelName
  = ChannelName String
```

#### `ChronologicalOrder`

``` purescript
newtype ChronologicalOrder
  = ChronologicalOrder String
```

#### `Compliance`

``` purescript
newtype Compliance
  = Compliance { "ComplianceType" :: NullOrUndefined (ComplianceType), "ComplianceContributorCount" :: NullOrUndefined (ComplianceContributorCount) }
```

<p>Indicates whether an AWS resource or AWS Config rule is compliant and provides the number of contributors that affect the compliance.</p>

#### `ComplianceByConfigRule`

``` purescript
newtype ComplianceByConfigRule
  = ComplianceByConfigRule { "ConfigRuleName" :: NullOrUndefined (StringWithCharLimit64), "Compliance" :: NullOrUndefined (Compliance) }
```

<p>Indicates whether an AWS Config rule is compliant. A rule is compliant if all of the resources that the rule evaluated comply with it, and it is noncompliant if any of these resources do not comply.</p>

#### `ComplianceByConfigRules`

``` purescript
newtype ComplianceByConfigRules
  = ComplianceByConfigRules (Array ComplianceByConfigRule)
```

#### `ComplianceByResource`

``` purescript
newtype ComplianceByResource
  = ComplianceByResource { "ResourceType" :: NullOrUndefined (StringWithCharLimit256), "ResourceId" :: NullOrUndefined (BaseResourceId), "Compliance" :: NullOrUndefined (Compliance) }
```

<p>Indicates whether an AWS resource that is evaluated according to one or more AWS Config rules is compliant. A resource is compliant if it complies with all of the rules that evaluate it, and it is noncompliant if it does not comply with one or more of these rules.</p>

#### `ComplianceByResources`

``` purescript
newtype ComplianceByResources
  = ComplianceByResources (Array ComplianceByResource)
```

#### `ComplianceContributorCount`

``` purescript
newtype ComplianceContributorCount
  = ComplianceContributorCount { "CappedCount" :: NullOrUndefined (Int), "CapExceeded" :: NullOrUndefined (Boolean) }
```

<p>The number of AWS resources or AWS Config rules responsible for the current compliance of the item, up to a maximum number.</p>

#### `ComplianceResourceTypes`

``` purescript
newtype ComplianceResourceTypes
  = ComplianceResourceTypes (Array StringWithCharLimit256)
```

#### `ComplianceSummariesByResourceType`

``` purescript
newtype ComplianceSummariesByResourceType
  = ComplianceSummariesByResourceType (Array ComplianceSummaryByResourceType)
```

#### `ComplianceSummary`

``` purescript
newtype ComplianceSummary
  = ComplianceSummary { "CompliantResourceCount" :: NullOrUndefined (ComplianceContributorCount), "NonCompliantResourceCount" :: NullOrUndefined (ComplianceContributorCount), "ComplianceSummaryTimestamp" :: NullOrUndefined (Date) }
```

<p>The number of AWS Config rules or AWS resources that are compliant and noncompliant.</p>

#### `ComplianceSummaryByResourceType`

``` purescript
newtype ComplianceSummaryByResourceType
  = ComplianceSummaryByResourceType { "ResourceType" :: NullOrUndefined (StringWithCharLimit256), "ComplianceSummary" :: NullOrUndefined (ComplianceSummary) }
```

<p>The number of AWS resources of a specific type that are compliant or noncompliant, up to a maximum of 100 for each compliance.</p>

#### `ComplianceType`

``` purescript
newtype ComplianceType
  = ComplianceType String
```

#### `ComplianceTypes`

``` purescript
newtype ComplianceTypes
  = ComplianceTypes (Array ComplianceType)
```

#### `ConfigExportDeliveryInfo`

``` purescript
newtype ConfigExportDeliveryInfo
  = ConfigExportDeliveryInfo { "LastStatus'" :: NullOrUndefined (DeliveryStatus), "LastErrorCode'" :: NullOrUndefined (String), "LastErrorMessage'" :: NullOrUndefined (String), "LastAttemptTime'" :: NullOrUndefined (Date), "LastSuccessfulTime'" :: NullOrUndefined (Date), "NextDeliveryTime'" :: NullOrUndefined (Date) }
```

<p>Provides status of the delivery of the snapshot or the configuration history to the specified Amazon S3 bucket. Also provides the status of notifications about the Amazon S3 delivery to the specified Amazon SNS topic.</p>

#### `ConfigRule`

``` purescript
newtype ConfigRule
  = ConfigRule { "ConfigRuleName" :: NullOrUndefined (StringWithCharLimit64), "ConfigRuleArn" :: NullOrUndefined (String), "ConfigRuleId" :: NullOrUndefined (String), "Description" :: NullOrUndefined (EmptiableStringWithCharLimit256), "Scope" :: NullOrUndefined (Scope), "Source" :: Source, "InputParameters" :: NullOrUndefined (StringWithCharLimit1024), "MaximumExecutionFrequency" :: NullOrUndefined (MaximumExecutionFrequency), "ConfigRuleState" :: NullOrUndefined (ConfigRuleState) }
```

<p>An AWS Config rule represents an AWS Lambda function that you create for a custom rule or a predefined function for an AWS managed rule. The function evaluates configuration items to assess whether your AWS resources comply with your desired configurations. This function can run when AWS Config detects a configuration change to an AWS resource and at a periodic frequency that you choose (for example, every 24 hours).</p> <note> <p>You can use the AWS CLI and AWS SDKs if you want to create a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot. For more information, see <a>ConfigSnapshotDeliveryProperties</a>.</p> </note> <p>For more information about developing and using AWS Config rules, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/evaluate-config.html">Evaluating AWS Resource Configurations with AWS Config</a> in the <i>AWS Config Developer Guide</i>.</p>

#### `ConfigRuleEvaluationStatus`

``` purescript
newtype ConfigRuleEvaluationStatus
  = ConfigRuleEvaluationStatus { "ConfigRuleName" :: NullOrUndefined (StringWithCharLimit64), "ConfigRuleArn" :: NullOrUndefined (String), "ConfigRuleId" :: NullOrUndefined (String), "LastSuccessfulInvocationTime" :: NullOrUndefined (Date), "LastFailedInvocationTime" :: NullOrUndefined (Date), "LastSuccessfulEvaluationTime" :: NullOrUndefined (Date), "LastFailedEvaluationTime" :: NullOrUndefined (Date), "FirstActivatedTime" :: NullOrUndefined (Date), "LastErrorCode" :: NullOrUndefined (String), "LastErrorMessage" :: NullOrUndefined (String), "FirstEvaluationStarted" :: NullOrUndefined (Boolean) }
```

<p>Status information for your AWS managed Config rules. The status includes information such as the last time the rule ran, the last time it failed, and the related error for the last failure.</p> <p>This action does not return status information about custom Config rules.</p>

#### `ConfigRuleEvaluationStatusList`

``` purescript
newtype ConfigRuleEvaluationStatusList
  = ConfigRuleEvaluationStatusList (Array ConfigRuleEvaluationStatus)
```

#### `ConfigRuleNames`

``` purescript
newtype ConfigRuleNames
  = ConfigRuleNames (Array StringWithCharLimit64)
```

#### `ConfigRuleState`

``` purescript
newtype ConfigRuleState
  = ConfigRuleState String
```

#### `ConfigRules`

``` purescript
newtype ConfigRules
  = ConfigRules (Array ConfigRule)
```

#### `ConfigSnapshotDeliveryProperties`

``` purescript
newtype ConfigSnapshotDeliveryProperties
  = ConfigSnapshotDeliveryProperties { "DeliveryFrequency'" :: NullOrUndefined (MaximumExecutionFrequency) }
```

<p>Provides options for how often AWS Config delivers configuration snapshots to the Amazon S3 bucket in your delivery channel.</p> <note> <p>If you want to create a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot, see the following:</p> </note> <p>The frequency for a rule that triggers evaluations for your resources when AWS Config delivers the configuration snapshot is set by one of two values, depending on which is less frequent:</p> <ul> <li> <p>The value for the <code>deliveryFrequency</code> parameter within the delivery channel configuration, which sets how often AWS Config delivers configuration snapshots. This value also sets how often AWS Config invokes evaluations for Config rules.</p> </li> <li> <p>The value for the <code>MaximumExecutionFrequency</code> parameter, which sets the maximum frequency with which AWS Config invokes evaluations for the rule. For more information, see <a>ConfigRule</a>.</p> </li> </ul> <p>If the <code>deliveryFrequency</code> value is less frequent than the <code>MaximumExecutionFrequency</code> value for a rule, AWS Config invokes the rule only as often as the <code>deliveryFrequency</code> value.</p> <ol> <li> <p>For example, you want your rule to run evaluations when AWS Config delivers the configuration snapshot.</p> </li> <li> <p>You specify the <code>MaximumExecutionFrequency</code> value for <code>Six_Hours</code>. </p> </li> <li> <p>You then specify the delivery channel <code>deliveryFrequency</code> value for <code>TwentyFour_Hours</code>.</p> </li> <li> <p>Because the value for <code>deliveryFrequency</code> is less frequent than <code>MaximumExecutionFrequency</code>, AWS Config invokes evaluations for the rule every 24 hours. </p> </li> </ol> <p>You should set the <code>MaximumExecutionFrequency</code> value to be at least as frequent as the <code>deliveryFrequency</code> value. You can view the <code>deliveryFrequency</code> value by using the <code>DescribeDeliveryChannnels</code> action.</p> <p>To update the <code>deliveryFrequency</code> with which AWS Config delivers your configuration snapshots, use the <code>PutDeliveryChannel</code> action.</p>

#### `ConfigStreamDeliveryInfo`

``` purescript
newtype ConfigStreamDeliveryInfo
  = ConfigStreamDeliveryInfo { "LastStatus'" :: NullOrUndefined (DeliveryStatus), "LastErrorCode'" :: NullOrUndefined (String), "LastErrorMessage'" :: NullOrUndefined (String), "LastStatusChangeTime'" :: NullOrUndefined (Date) }
```

<p>A list that contains the status of the delivery of the configuration stream notification to the Amazon SNS topic.</p>

#### `Configuration`

``` purescript
newtype Configuration
  = Configuration String
```

#### `ConfigurationItem`

``` purescript
newtype ConfigurationItem
  = ConfigurationItem { "Version'" :: NullOrUndefined (Version), "AccountId'" :: NullOrUndefined (AccountId), "ConfigurationItemCaptureTime'" :: NullOrUndefined (ConfigurationItemCaptureTime), "ConfigurationItemStatus'" :: NullOrUndefined (ConfigurationItemStatus), "ConfigurationStateId'" :: NullOrUndefined (ConfigurationStateId), "ConfigurationItemMD5Hash'" :: NullOrUndefined (ConfigurationItemMD5Hash), "Arn'" :: NullOrUndefined (ARN), "ResourceType'" :: NullOrUndefined (ResourceType), "ResourceId'" :: NullOrUndefined (ResourceId), "ResourceName'" :: NullOrUndefined (ResourceName), "AwsRegion'" :: NullOrUndefined (AwsRegion), "AvailabilityZone'" :: NullOrUndefined (AvailabilityZone), "ResourceCreationTime'" :: NullOrUndefined (ResourceCreationTime), "Tags'" :: NullOrUndefined (Tags), "RelatedEvents'" :: NullOrUndefined (RelatedEventList), "Relationships'" :: NullOrUndefined (RelationshipList), "Configuration'" :: NullOrUndefined (Configuration), "SupplementaryConfiguration'" :: NullOrUndefined (SupplementaryConfiguration) }
```

<p>A list that contains detailed configurations of a specified resource.</p>

#### `ConfigurationItemCaptureTime`

``` purescript
newtype ConfigurationItemCaptureTime
  = ConfigurationItemCaptureTime Number
```

#### `ConfigurationItemList`

``` purescript
newtype ConfigurationItemList
  = ConfigurationItemList (Array ConfigurationItem)
```

#### `ConfigurationItemMD5Hash`

``` purescript
newtype ConfigurationItemMD5Hash
  = ConfigurationItemMD5Hash String
```

#### `ConfigurationItemStatus`

``` purescript
newtype ConfigurationItemStatus
  = ConfigurationItemStatus String
```

#### `ConfigurationRecorder`

``` purescript
newtype ConfigurationRecorder
  = ConfigurationRecorder { "Name'" :: NullOrUndefined (RecorderName), "RoleARN'" :: NullOrUndefined (String), "RecordingGroup'" :: NullOrUndefined (RecordingGroup) }
```

<p>An object that represents the recording of configuration changes of an AWS resource.</p>

#### `ConfigurationRecorderList`

``` purescript
newtype ConfigurationRecorderList
  = ConfigurationRecorderList (Array ConfigurationRecorder)
```

#### `ConfigurationRecorderNameList`

``` purescript
newtype ConfigurationRecorderNameList
  = ConfigurationRecorderNameList (Array RecorderName)
```

#### `ConfigurationRecorderStatus`

``` purescript
newtype ConfigurationRecorderStatus
  = ConfigurationRecorderStatus { "Name'" :: NullOrUndefined (String), "LastStartTime'" :: NullOrUndefined (Date), "LastStopTime'" :: NullOrUndefined (Date), "Recording'" :: NullOrUndefined (Boolean), "LastStatus'" :: NullOrUndefined (RecorderStatus), "LastErrorCode'" :: NullOrUndefined (String), "LastErrorMessage'" :: NullOrUndefined (String), "LastStatusChangeTime'" :: NullOrUndefined (Date) }
```

<p>The current status of the configuration recorder.</p>

#### `ConfigurationRecorderStatusList`

``` purescript
newtype ConfigurationRecorderStatusList
  = ConfigurationRecorderStatusList (Array ConfigurationRecorderStatus)
```

#### `ConfigurationStateId`

``` purescript
newtype ConfigurationStateId
  = ConfigurationStateId String
```

#### `Date`

``` purescript
newtype Date
  = Date Number
```

#### `DeleteConfigRuleRequest`

``` purescript
newtype DeleteConfigRuleRequest
  = DeleteConfigRuleRequest { "ConfigRuleName" :: StringWithCharLimit64 }
```

<p/>

#### `DeleteConfigurationRecorderRequest`

``` purescript
newtype DeleteConfigurationRecorderRequest
  = DeleteConfigurationRecorderRequest { "ConfigurationRecorderName" :: RecorderName }
```

<p>The request object for the <code>DeleteConfigurationRecorder</code> action.</p>

#### `DeleteDeliveryChannelRequest`

``` purescript
newtype DeleteDeliveryChannelRequest
  = DeleteDeliveryChannelRequest { "DeliveryChannelName" :: ChannelName }
```

<p>The input for the <a>DeleteDeliveryChannel</a> action. The action accepts the following data in JSON format. </p>

#### `DeleteEvaluationResultsRequest`

``` purescript
newtype DeleteEvaluationResultsRequest
  = DeleteEvaluationResultsRequest { "ConfigRuleName" :: StringWithCharLimit64 }
```

<p/>

#### `DeleteEvaluationResultsResponse`

``` purescript
newtype DeleteEvaluationResultsResponse
  = DeleteEvaluationResultsResponse {  }
```

<p>The output when you delete the evaluation results for the specified Config rule.</p>

#### `DeliverConfigSnapshotRequest`

``` purescript
newtype DeliverConfigSnapshotRequest
  = DeliverConfigSnapshotRequest { "DeliveryChannelName'" :: ChannelName }
```

<p>The input for the <a>DeliverConfigSnapshot</a> action.</p>

#### `DeliverConfigSnapshotResponse`

``` purescript
newtype DeliverConfigSnapshotResponse
  = DeliverConfigSnapshotResponse { "ConfigSnapshotId'" :: NullOrUndefined (String) }
```

<p>The output for the <a>DeliverConfigSnapshot</a> action in JSON format.</p>

#### `DeliveryChannel`

``` purescript
newtype DeliveryChannel
  = DeliveryChannel { "Name'" :: NullOrUndefined (ChannelName), "S3BucketName'" :: NullOrUndefined (String), "S3KeyPrefix'" :: NullOrUndefined (String), "SnsTopicARN'" :: NullOrUndefined (String), "ConfigSnapshotDeliveryProperties'" :: NullOrUndefined (ConfigSnapshotDeliveryProperties) }
```

<p>The channel through which AWS Config delivers notifications and updated configuration states.</p>

#### `DeliveryChannelList`

``` purescript
newtype DeliveryChannelList
  = DeliveryChannelList (Array DeliveryChannel)
```

#### `DeliveryChannelNameList`

``` purescript
newtype DeliveryChannelNameList
  = DeliveryChannelNameList (Array ChannelName)
```

#### `DeliveryChannelStatus`

``` purescript
newtype DeliveryChannelStatus
  = DeliveryChannelStatus { "Name'" :: NullOrUndefined (String), "ConfigSnapshotDeliveryInfo'" :: NullOrUndefined (ConfigExportDeliveryInfo), "ConfigHistoryDeliveryInfo'" :: NullOrUndefined (ConfigExportDeliveryInfo), "ConfigStreamDeliveryInfo'" :: NullOrUndefined (ConfigStreamDeliveryInfo) }
```

<p>The status of a specified delivery channel.</p> <p>Valid values: <code>Success</code> | <code>Failure</code> </p>

#### `DeliveryChannelStatusList`

``` purescript
newtype DeliveryChannelStatusList
  = DeliveryChannelStatusList (Array DeliveryChannelStatus)
```

#### `DeliveryStatus`

``` purescript
newtype DeliveryStatus
  = DeliveryStatus String
```

#### `DescribeComplianceByConfigRuleRequest`

``` purescript
newtype DescribeComplianceByConfigRuleRequest
  = DescribeComplianceByConfigRuleRequest { "ConfigRuleNames" :: NullOrUndefined (ConfigRuleNames), "ComplianceTypes" :: NullOrUndefined (ComplianceTypes), "NextToken" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeComplianceByConfigRuleResponse`

``` purescript
newtype DescribeComplianceByConfigRuleResponse
  = DescribeComplianceByConfigRuleResponse { "ComplianceByConfigRules" :: NullOrUndefined (ComplianceByConfigRules), "NextToken" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeComplianceByResourceRequest`

``` purescript
newtype DescribeComplianceByResourceRequest
  = DescribeComplianceByResourceRequest { "ResourceType" :: NullOrUndefined (StringWithCharLimit256), "ResourceId" :: NullOrUndefined (BaseResourceId), "ComplianceTypes" :: NullOrUndefined (ComplianceTypes), "Limit" :: NullOrUndefined (Limit), "NextToken" :: NullOrUndefined (NextToken) }
```

<p/>

#### `DescribeComplianceByResourceResponse`

``` purescript
newtype DescribeComplianceByResourceResponse
  = DescribeComplianceByResourceResponse { "ComplianceByResources" :: NullOrUndefined (ComplianceByResources), "NextToken" :: NullOrUndefined (NextToken) }
```

<p/>

#### `DescribeConfigRuleEvaluationStatusRequest`

``` purescript
newtype DescribeConfigRuleEvaluationStatusRequest
  = DescribeConfigRuleEvaluationStatusRequest { "ConfigRuleNames" :: NullOrUndefined (ConfigRuleNames), "NextToken" :: NullOrUndefined (String), "Limit" :: NullOrUndefined (RuleLimit) }
```

<p/>

#### `DescribeConfigRuleEvaluationStatusResponse`

``` purescript
newtype DescribeConfigRuleEvaluationStatusResponse
  = DescribeConfigRuleEvaluationStatusResponse { "ConfigRulesEvaluationStatus" :: NullOrUndefined (ConfigRuleEvaluationStatusList), "NextToken" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeConfigRulesRequest`

``` purescript
newtype DescribeConfigRulesRequest
  = DescribeConfigRulesRequest { "ConfigRuleNames" :: NullOrUndefined (ConfigRuleNames), "NextToken" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeConfigRulesResponse`

``` purescript
newtype DescribeConfigRulesResponse
  = DescribeConfigRulesResponse { "ConfigRules" :: NullOrUndefined (ConfigRules), "NextToken" :: NullOrUndefined (String) }
```

<p/>

#### `DescribeConfigurationRecorderStatusRequest`

``` purescript
newtype DescribeConfigurationRecorderStatusRequest
  = DescribeConfigurationRecorderStatusRequest { "ConfigurationRecorderNames" :: NullOrUndefined (ConfigurationRecorderNameList) }
```

<p>The input for the <a>DescribeConfigurationRecorderStatus</a> action.</p>

#### `DescribeConfigurationRecorderStatusResponse`

``` purescript
newtype DescribeConfigurationRecorderStatusResponse
  = DescribeConfigurationRecorderStatusResponse { "ConfigurationRecordersStatus" :: NullOrUndefined (ConfigurationRecorderStatusList) }
```

<p>The output for the <a>DescribeConfigurationRecorderStatus</a> action in JSON format.</p>

#### `DescribeConfigurationRecordersRequest`

``` purescript
newtype DescribeConfigurationRecordersRequest
  = DescribeConfigurationRecordersRequest { "ConfigurationRecorderNames" :: NullOrUndefined (ConfigurationRecorderNameList) }
```

<p>The input for the <a>DescribeConfigurationRecorders</a> action.</p>

#### `DescribeConfigurationRecordersResponse`

``` purescript
newtype DescribeConfigurationRecordersResponse
  = DescribeConfigurationRecordersResponse { "ConfigurationRecorders" :: NullOrUndefined (ConfigurationRecorderList) }
```

<p>The output for the <a>DescribeConfigurationRecorders</a> action.</p>

#### `DescribeDeliveryChannelStatusRequest`

``` purescript
newtype DescribeDeliveryChannelStatusRequest
  = DescribeDeliveryChannelStatusRequest { "DeliveryChannelNames" :: NullOrUndefined (DeliveryChannelNameList) }
```

<p>The input for the <a>DeliveryChannelStatus</a> action.</p>

#### `DescribeDeliveryChannelStatusResponse`

``` purescript
newtype DescribeDeliveryChannelStatusResponse
  = DescribeDeliveryChannelStatusResponse { "DeliveryChannelsStatus" :: NullOrUndefined (DeliveryChannelStatusList) }
```

<p>The output for the <a>DescribeDeliveryChannelStatus</a> action.</p>

#### `DescribeDeliveryChannelsRequest`

``` purescript
newtype DescribeDeliveryChannelsRequest
  = DescribeDeliveryChannelsRequest { "DeliveryChannelNames" :: NullOrUndefined (DeliveryChannelNameList) }
```

<p>The input for the <a>DescribeDeliveryChannels</a> action.</p>

#### `DescribeDeliveryChannelsResponse`

``` purescript
newtype DescribeDeliveryChannelsResponse
  = DescribeDeliveryChannelsResponse { "DeliveryChannels" :: NullOrUndefined (DeliveryChannelList) }
```

<p>The output for the <a>DescribeDeliveryChannels</a> action.</p>

#### `EarlierTime`

``` purescript
newtype EarlierTime
  = EarlierTime Number
```

#### `EmptiableStringWithCharLimit256`

``` purescript
newtype EmptiableStringWithCharLimit256
  = EmptiableStringWithCharLimit256 String
```

#### `Evaluation`

``` purescript
newtype Evaluation
  = Evaluation { "ComplianceResourceType" :: StringWithCharLimit256, "ComplianceResourceId" :: BaseResourceId, "ComplianceType" :: ComplianceType, "Annotation" :: NullOrUndefined (StringWithCharLimit256), "OrderingTimestamp" :: OrderingTimestamp }
```

<p>Identifies an AWS resource and indicates whether it complies with the AWS Config rule that it was evaluated against.</p>

#### `EvaluationResult`

``` purescript
newtype EvaluationResult
  = EvaluationResult { "EvaluationResultIdentifier" :: NullOrUndefined (EvaluationResultIdentifier), "ComplianceType" :: NullOrUndefined (ComplianceType), "ResultRecordedTime" :: NullOrUndefined (Date), "ConfigRuleInvokedTime" :: NullOrUndefined (Date), "Annotation" :: NullOrUndefined (StringWithCharLimit256), "ResultToken" :: NullOrUndefined (String) }
```

<p>The details of an AWS Config evaluation. Provides the AWS resource that was evaluated, the compliance of the resource, related timestamps, and supplementary information.</p>

#### `EvaluationResultIdentifier`

``` purescript
newtype EvaluationResultIdentifier
  = EvaluationResultIdentifier { "EvaluationResultQualifier" :: NullOrUndefined (EvaluationResultQualifier), "OrderingTimestamp" :: NullOrUndefined (Date) }
```

<p>Uniquely identifies an evaluation result.</p>

#### `EvaluationResultQualifier`

``` purescript
newtype EvaluationResultQualifier
  = EvaluationResultQualifier { "ConfigRuleName" :: NullOrUndefined (StringWithCharLimit64), "ResourceType" :: NullOrUndefined (StringWithCharLimit256), "ResourceId" :: NullOrUndefined (BaseResourceId) }
```

<p>Identifies an AWS Config rule that evaluated an AWS resource, and provides the type and ID of the resource that the rule evaluated.</p>

#### `EvaluationResults`

``` purescript
newtype EvaluationResults
  = EvaluationResults (Array EvaluationResult)
```

#### `Evaluations`

``` purescript
newtype Evaluations
  = Evaluations (Array Evaluation)
```

#### `EventSource`

``` purescript
newtype EventSource
  = EventSource String
```

#### `GetComplianceDetailsByConfigRuleRequest`

``` purescript
newtype GetComplianceDetailsByConfigRuleRequest
  = GetComplianceDetailsByConfigRuleRequest { "ConfigRuleName" :: StringWithCharLimit64, "ComplianceTypes" :: NullOrUndefined (ComplianceTypes), "Limit" :: NullOrUndefined (Limit), "NextToken" :: NullOrUndefined (NextToken) }
```

<p/>

#### `GetComplianceDetailsByConfigRuleResponse`

``` purescript
newtype GetComplianceDetailsByConfigRuleResponse
  = GetComplianceDetailsByConfigRuleResponse { "EvaluationResults" :: NullOrUndefined (EvaluationResults), "NextToken" :: NullOrUndefined (NextToken) }
```

<p/>

#### `GetComplianceDetailsByResourceRequest`

``` purescript
newtype GetComplianceDetailsByResourceRequest
  = GetComplianceDetailsByResourceRequest { "ResourceType" :: StringWithCharLimit256, "ResourceId" :: BaseResourceId, "ComplianceTypes" :: NullOrUndefined (ComplianceTypes), "NextToken" :: NullOrUndefined (String) }
```

<p/>

#### `GetComplianceDetailsByResourceResponse`

``` purescript
newtype GetComplianceDetailsByResourceResponse
  = GetComplianceDetailsByResourceResponse { "EvaluationResults" :: NullOrUndefined (EvaluationResults), "NextToken" :: NullOrUndefined (String) }
```

<p/>

#### `GetComplianceSummaryByConfigRuleResponse`

``` purescript
newtype GetComplianceSummaryByConfigRuleResponse
  = GetComplianceSummaryByConfigRuleResponse { "ComplianceSummary" :: NullOrUndefined (ComplianceSummary) }
```

<p/>

#### `GetComplianceSummaryByResourceTypeRequest`

``` purescript
newtype GetComplianceSummaryByResourceTypeRequest
  = GetComplianceSummaryByResourceTypeRequest { "ResourceTypes" :: NullOrUndefined (ResourceTypes) }
```

<p/>

#### `GetComplianceSummaryByResourceTypeResponse`

``` purescript
newtype GetComplianceSummaryByResourceTypeResponse
  = GetComplianceSummaryByResourceTypeResponse { "ComplianceSummariesByResourceType" :: NullOrUndefined (ComplianceSummariesByResourceType) }
```

<p/>

#### `GetDiscoveredResourceCountsRequest`

``` purescript
newtype GetDiscoveredResourceCountsRequest
  = GetDiscoveredResourceCountsRequest { "ResourceTypes'" :: NullOrUndefined (ResourceTypes), "Limit'" :: NullOrUndefined (Limit), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `GetDiscoveredResourceCountsResponse`

``` purescript
newtype GetDiscoveredResourceCountsResponse
  = GetDiscoveredResourceCountsResponse { "TotalDiscoveredResources'" :: NullOrUndefined (Number), "ResourceCounts'" :: NullOrUndefined (ResourceCounts), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `GetResourceConfigHistoryRequest`

``` purescript
newtype GetResourceConfigHistoryRequest
  = GetResourceConfigHistoryRequest { "ResourceType'" :: ResourceType, "ResourceId'" :: ResourceId, "LaterTime'" :: NullOrUndefined (LaterTime), "EarlierTime'" :: NullOrUndefined (EarlierTime), "ChronologicalOrder'" :: NullOrUndefined (ChronologicalOrder), "Limit'" :: NullOrUndefined (Limit), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The input for the <a>GetResourceConfigHistory</a> action.</p>

#### `GetResourceConfigHistoryResponse`

``` purescript
newtype GetResourceConfigHistoryResponse
  = GetResourceConfigHistoryResponse { "ConfigurationItems'" :: NullOrUndefined (ConfigurationItemList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>The output for the <a>GetResourceConfigHistory</a> action.</p>

#### `IncludeGlobalResourceTypes`

``` purescript
newtype IncludeGlobalResourceTypes
  = IncludeGlobalResourceTypes Boolean
```

#### `InsufficientDeliveryPolicyException`

``` purescript
newtype InsufficientDeliveryPolicyException
  = InsufficientDeliveryPolicyException {  }
```

<p>Your Amazon S3 bucket policy does not permit AWS Config to write to it.</p>

#### `InsufficientPermissionsException`

``` purescript
newtype InsufficientPermissionsException
  = InsufficientPermissionsException {  }
```

<p>Indicates one of the following errors:</p> <ul> <li> <p>The rule cannot be created because the IAM role assigned to AWS Config lacks permissions to perform the config:Put* action.</p> </li> <li> <p>The AWS Lambda function cannot be invoked. Check the function ARN, and check the function's permissions.</p> </li> </ul>

#### `InvalidConfigurationRecorderNameException`

``` purescript
newtype InvalidConfigurationRecorderNameException
  = InvalidConfigurationRecorderNameException {  }
```

<p>You have provided a configuration recorder name that is not valid.</p>

#### `InvalidDeliveryChannelNameException`

``` purescript
newtype InvalidDeliveryChannelNameException
  = InvalidDeliveryChannelNameException {  }
```

<p>The specified delivery channel name is not valid.</p>

#### `InvalidLimitException`

``` purescript
newtype InvalidLimitException
  = InvalidLimitException {  }
```

<p>The specified limit is outside the allowable range.</p>

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException {  }
```

<p>The specified next token is invalid. Specify the <code>NextToken</code> string that was returned in the previous response to get the next page of results.</p>

#### `InvalidParameterValueException`

``` purescript
newtype InvalidParameterValueException
  = InvalidParameterValueException {  }
```

<p>One or more of the specified parameters are invalid. Verify that your parameters are valid and try again.</p>

#### `InvalidRecordingGroupException`

``` purescript
newtype InvalidRecordingGroupException
  = InvalidRecordingGroupException {  }
```

<p>AWS Config throws an exception if the recording group does not contain a valid list of resource types. Invalid values could also be incorrectly formatted.</p>

#### `InvalidResultTokenException`

``` purescript
newtype InvalidResultTokenException
  = InvalidResultTokenException {  }
```

<p>The specified <code>ResultToken</code> is invalid.</p>

#### `InvalidRoleException`

``` purescript
newtype InvalidRoleException
  = InvalidRoleException {  }
```

<p>You have provided a null or empty role ARN.</p>

#### `InvalidS3KeyPrefixException`

``` purescript
newtype InvalidS3KeyPrefixException
  = InvalidS3KeyPrefixException {  }
```

<p>The specified Amazon S3 key prefix is not valid.</p>

#### `InvalidSNSTopicARNException`

``` purescript
newtype InvalidSNSTopicARNException
  = InvalidSNSTopicARNException {  }
```

<p>The specified Amazon SNS topic does not exist.</p>

#### `InvalidTimeRangeException`

``` purescript
newtype InvalidTimeRangeException
  = InvalidTimeRangeException {  }
```

<p>The specified time range is not valid. The earlier time is not chronologically before the later time.</p>

#### `LastDeliveryChannelDeleteFailedException`

``` purescript
newtype LastDeliveryChannelDeleteFailedException
  = LastDeliveryChannelDeleteFailedException {  }
```

<p>You cannot delete the delivery channel you specified because the configuration recorder is running.</p>

#### `LaterTime`

``` purescript
newtype LaterTime
  = LaterTime Number
```

#### `Limit`

``` purescript
newtype Limit
  = Limit Int
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>This exception is thrown if an evaluation is in progress or if you call the <a>StartConfigRulesEvaluation</a> API more than once per minute.</p>

#### `ListDiscoveredResourcesRequest`

``` purescript
newtype ListDiscoveredResourcesRequest
  = ListDiscoveredResourcesRequest { "ResourceType'" :: ResourceType, "ResourceIds'" :: NullOrUndefined (ResourceIdList), "ResourceName'" :: NullOrUndefined (ResourceName), "Limit'" :: NullOrUndefined (Limit), "IncludeDeletedResources'" :: NullOrUndefined (Boolean), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p/>

#### `ListDiscoveredResourcesResponse`

``` purescript
newtype ListDiscoveredResourcesResponse
  = ListDiscoveredResourcesResponse { "ResourceIdentifiers'" :: NullOrUndefined (ResourceIdentifierList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p/>

#### `MaxNumberOfConfigRulesExceededException`

``` purescript
newtype MaxNumberOfConfigRulesExceededException
  = MaxNumberOfConfigRulesExceededException {  }
```

<p>Failed to add the AWS Config rule because the account already contains the maximum number of 50 rules. Consider deleting any deactivated rules before adding new rules.</p>

#### `MaxNumberOfConfigurationRecordersExceededException`

``` purescript
newtype MaxNumberOfConfigurationRecordersExceededException
  = MaxNumberOfConfigurationRecordersExceededException {  }
```

<p>You have reached the limit on the number of recorders you can create.</p>

#### `MaxNumberOfDeliveryChannelsExceededException`

``` purescript
newtype MaxNumberOfDeliveryChannelsExceededException
  = MaxNumberOfDeliveryChannelsExceededException {  }
```

<p>You have reached the limit on the number of delivery channels you can create.</p>

#### `MaximumExecutionFrequency`

``` purescript
newtype MaximumExecutionFrequency
  = MaximumExecutionFrequency String
```

#### `MessageType`

``` purescript
newtype MessageType
  = MessageType String
```

#### `Name`

``` purescript
newtype Name
  = Name String
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `NoAvailableConfigurationRecorderException`

``` purescript
newtype NoAvailableConfigurationRecorderException
  = NoAvailableConfigurationRecorderException {  }
```

<p>There are no configuration recorders available to provide the role needed to describe your resources. Create a configuration recorder.</p>

#### `NoAvailableDeliveryChannelException`

``` purescript
newtype NoAvailableDeliveryChannelException
  = NoAvailableDeliveryChannelException {  }
```

<p>There is no delivery channel available to record configurations.</p>

#### `NoRunningConfigurationRecorderException`

``` purescript
newtype NoRunningConfigurationRecorderException
  = NoRunningConfigurationRecorderException {  }
```

<p>There is no configuration recorder running.</p>

#### `NoSuchBucketException`

``` purescript
newtype NoSuchBucketException
  = NoSuchBucketException {  }
```

<p>The specified Amazon S3 bucket does not exist.</p>

#### `NoSuchConfigRuleException`

``` purescript
newtype NoSuchConfigRuleException
  = NoSuchConfigRuleException {  }
```

<p>One or more AWS Config rules in the request are invalid. Verify that the rule names are correct and try again.</p>

#### `NoSuchConfigurationRecorderException`

``` purescript
newtype NoSuchConfigurationRecorderException
  = NoSuchConfigurationRecorderException {  }
```

<p>You have specified a configuration recorder that does not exist.</p>

#### `NoSuchDeliveryChannelException`

``` purescript
newtype NoSuchDeliveryChannelException
  = NoSuchDeliveryChannelException {  }
```

<p>You have specified a delivery channel that does not exist.</p>

#### `OrderingTimestamp`

``` purescript
newtype OrderingTimestamp
  = OrderingTimestamp Number
```

#### `Owner`

``` purescript
newtype Owner
  = Owner String
```

#### `PutConfigRuleRequest`

``` purescript
newtype PutConfigRuleRequest
  = PutConfigRuleRequest { "ConfigRule" :: ConfigRule }
```

#### `PutConfigurationRecorderRequest`

``` purescript
newtype PutConfigurationRecorderRequest
  = PutConfigurationRecorderRequest { "ConfigurationRecorder" :: ConfigurationRecorder }
```

<p>The input for the <a>PutConfigurationRecorder</a> action.</p>

#### `PutDeliveryChannelRequest`

``` purescript
newtype PutDeliveryChannelRequest
  = PutDeliveryChannelRequest { "DeliveryChannel" :: DeliveryChannel }
```

<p>The input for the <a>PutDeliveryChannel</a> action.</p>

#### `PutEvaluationsRequest`

``` purescript
newtype PutEvaluationsRequest
  = PutEvaluationsRequest { "Evaluations" :: NullOrUndefined (Evaluations), "ResultToken" :: String, "TestMode" :: NullOrUndefined (Boolean) }
```

<p/>

#### `PutEvaluationsResponse`

``` purescript
newtype PutEvaluationsResponse
  = PutEvaluationsResponse { "FailedEvaluations" :: NullOrUndefined (Evaluations) }
```

<p/>

#### `RecorderName`

``` purescript
newtype RecorderName
  = RecorderName String
```

#### `RecorderStatus`

``` purescript
newtype RecorderStatus
  = RecorderStatus String
```

#### `RecordingGroup`

``` purescript
newtype RecordingGroup
  = RecordingGroup { "AllSupported'" :: NullOrUndefined (AllSupported), "IncludeGlobalResourceTypes'" :: NullOrUndefined (IncludeGlobalResourceTypes), "ResourceTypes'" :: NullOrUndefined (ResourceTypeList) }
```

<p>Specifies the types of AWS resource for which AWS Config records configuration changes.</p> <p>In the recording group, you specify whether all supported types or specific types of resources are recorded.</p> <p>By default, AWS Config records configuration changes for all supported types of regional resources that AWS Config discovers in the region in which it is running. Regional resources are tied to a region and can be used only in that region. Examples of regional resources are EC2 instances and EBS volumes.</p> <p>You can also have AWS Config record configuration changes for supported types of global resources (for example, IAM resources). Global resources are not tied to an individual region and can be used in all regions.</p> <important> <p>The configuration details for any global resource are the same in all regions. If you customize AWS Config in multiple regions to record global resources, it will create multiple configuration items each time a global resource changes: one configuration item for each region. These configuration items will contain identical data. To prevent duplicate configuration items, you should consider customizing AWS Config in only one region to record global resources, unless you want the configuration items to be available in multiple regions.</p> </important> <p>If you don't want AWS Config to record all resources, you can specify which types of resources it will record with the <code>resourceTypes</code> parameter.</p> <p>For a list of supported resource types, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/resource-config-reference.html#supported-resources">Supported resource types</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/config/latest/developerguide/select-resources.html">Selecting Which Resources AWS Config Records</a>.</p>

#### `ReevaluateConfigRuleNames`

``` purescript
newtype ReevaluateConfigRuleNames
  = ReevaluateConfigRuleNames (Array StringWithCharLimit64)
```

#### `RelatedEvent`

``` purescript
newtype RelatedEvent
  = RelatedEvent String
```

#### `RelatedEventList`

``` purescript
newtype RelatedEventList
  = RelatedEventList (Array RelatedEvent)
```

#### `Relationship`

``` purescript
newtype Relationship
  = Relationship { "ResourceType'" :: NullOrUndefined (ResourceType), "ResourceId'" :: NullOrUndefined (ResourceId), "ResourceName'" :: NullOrUndefined (ResourceName), "RelationshipName'" :: NullOrUndefined (RelationshipName) }
```

<p>The relationship of the related resource to the main resource.</p>

#### `RelationshipList`

``` purescript
newtype RelationshipList
  = RelationshipList (Array Relationship)
```

#### `RelationshipName`

``` purescript
newtype RelationshipName
  = RelationshipName String
```

#### `ResourceCount`

``` purescript
newtype ResourceCount
  = ResourceCount { "ResourceType'" :: NullOrUndefined (ResourceType), "Count'" :: NullOrUndefined (Number) }
```

<p>An object that contains the resource type and the number of resources.</p>

#### `ResourceCounts`

``` purescript
newtype ResourceCounts
  = ResourceCounts (Array ResourceCount)
```

#### `ResourceCreationTime`

``` purescript
newtype ResourceCreationTime
  = ResourceCreationTime Number
```

#### `ResourceDeletionTime`

``` purescript
newtype ResourceDeletionTime
  = ResourceDeletionTime Number
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

#### `ResourceIdList`

``` purescript
newtype ResourceIdList
  = ResourceIdList (Array ResourceId)
```

#### `ResourceIdentifier`

``` purescript
newtype ResourceIdentifier
  = ResourceIdentifier { "ResourceType'" :: NullOrUndefined (ResourceType), "ResourceId'" :: NullOrUndefined (ResourceId), "ResourceName'" :: NullOrUndefined (ResourceName), "ResourceDeletionTime'" :: NullOrUndefined (ResourceDeletionTime) }
```

<p>The details that identify a resource that is discovered by AWS Config, including the resource type, ID, and (if available) the custom resource name.</p>

#### `ResourceIdentifierList`

``` purescript
newtype ResourceIdentifierList
  = ResourceIdentifierList (Array ResourceIdentifier)
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException {  }
```

<p>The rule is currently being deleted or the rule is deleting your evaluation results. Try your request again later.</p>

#### `ResourceName`

``` purescript
newtype ResourceName
  = ResourceName String
```

#### `ResourceNotDiscoveredException`

``` purescript
newtype ResourceNotDiscoveredException
  = ResourceNotDiscoveredException {  }
```

<p>You have specified a resource that is either unknown or has not been discovered.</p>

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

#### `ResourceTypeList`

``` purescript
newtype ResourceTypeList
  = ResourceTypeList (Array ResourceType)
```

#### `ResourceTypes`

``` purescript
newtype ResourceTypes
  = ResourceTypes (Array StringWithCharLimit256)
```

#### `RuleLimit`

``` purescript
newtype RuleLimit
  = RuleLimit Int
```

#### `Scope`

``` purescript
newtype Scope
  = Scope { "ComplianceResourceTypes" :: NullOrUndefined (ComplianceResourceTypes), "TagKey" :: NullOrUndefined (StringWithCharLimit128), "TagValue" :: NullOrUndefined (StringWithCharLimit256), "ComplianceResourceId" :: NullOrUndefined (BaseResourceId) }
```

<p>Defines which resources trigger an evaluation for an AWS Config rule. The scope can include one or more resource types, a combination of a tag key and value, or a combination of one resource type and one resource ID. Specify a scope to constrain which resources trigger an evaluation for a rule. Otherwise, evaluations for the rule are triggered when any resource in your recording group changes in configuration.</p>

#### `Source`

``` purescript
newtype Source
  = Source { "Owner" :: Owner, "SourceIdentifier" :: StringWithCharLimit256, "SourceDetails" :: NullOrUndefined (SourceDetails) }
```

<p>Provides the AWS Config rule owner (AWS or customer), the rule identifier, and the events that trigger the evaluation of your AWS resources.</p>

#### `SourceDetail`

``` purescript
newtype SourceDetail
  = SourceDetail { "EventSource" :: NullOrUndefined (EventSource), "MessageType" :: NullOrUndefined (MessageType), "MaximumExecutionFrequency" :: NullOrUndefined (MaximumExecutionFrequency) }
```

<p>Provides the source and the message types that trigger AWS Config to evaluate your AWS resources against a rule. It also provides the frequency with which you want AWS Config to run evaluations for the rule if the trigger type is periodic. You can specify the parameter values for <code>SourceDetail</code> only for custom rules. </p>

#### `SourceDetails`

``` purescript
newtype SourceDetails
  = SourceDetails (Array SourceDetail)
```

#### `StartConfigRulesEvaluationRequest`

``` purescript
newtype StartConfigRulesEvaluationRequest
  = StartConfigRulesEvaluationRequest { "ConfigRuleNames" :: NullOrUndefined (ReevaluateConfigRuleNames) }
```

<p/>

#### `StartConfigRulesEvaluationResponse`

``` purescript
newtype StartConfigRulesEvaluationResponse
  = StartConfigRulesEvaluationResponse {  }
```

<p>The output when you start the evaluation for the specified Config rule.</p>

#### `StartConfigurationRecorderRequest`

``` purescript
newtype StartConfigurationRecorderRequest
  = StartConfigurationRecorderRequest { "ConfigurationRecorderName" :: RecorderName }
```

<p>The input for the <a>StartConfigurationRecorder</a> action.</p>

#### `StopConfigurationRecorderRequest`

``` purescript
newtype StopConfigurationRecorderRequest
  = StopConfigurationRecorderRequest { "ConfigurationRecorderName" :: RecorderName }
```

<p>The input for the <a>StopConfigurationRecorder</a> action.</p>

#### `StringWithCharLimit1024`

``` purescript
newtype StringWithCharLimit1024
  = StringWithCharLimit1024 String
```

#### `StringWithCharLimit128`

``` purescript
newtype StringWithCharLimit128
  = StringWithCharLimit128 String
```

#### `StringWithCharLimit256`

``` purescript
newtype StringWithCharLimit256
  = StringWithCharLimit256 String
```

#### `StringWithCharLimit64`

``` purescript
newtype StringWithCharLimit64
  = StringWithCharLimit64 String
```

#### `SupplementaryConfiguration`

``` purescript
newtype SupplementaryConfiguration
  = SupplementaryConfiguration (Map SupplementaryConfigurationName SupplementaryConfigurationValue)
```

#### `SupplementaryConfigurationName`

``` purescript
newtype SupplementaryConfigurationName
  = SupplementaryConfigurationName String
```

#### `SupplementaryConfigurationValue`

``` purescript
newtype SupplementaryConfigurationValue
  = SupplementaryConfigurationValue String
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Map Name Value)
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException {  }
```

<p>The requested action is not valid.</p>

#### `Value`

``` purescript
newtype Value
  = Value String
```

#### `Version`

``` purescript
newtype Version
  = Version String
```


