## Module AWS.Shield

<fullname>AWS Shield Advanced</fullname> <p>This is the <i>AWS Shield Advanced API Reference</i>. This guide is for developers who need detailed information about the AWS Shield Advanced API actions, data types, and errors. For detailed information about AWS WAF and AWS Shield Advanced features and an overview of how to use the AWS WAF and AWS Shield Advanced APIs, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF and AWS Shield Developer Guide</a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createProtection`

``` purescript
createProtection :: forall eff. CreateProtectionRequest -> Aff (err :: RequestError | eff) CreateProtectionResponse
```

<p>Enables AWS Shield Advanced for a specific AWS resource. The resource can be an Amazon CloudFront distribution, Elastic Load Balancing load balancer, Elastic IP Address, or an Amazon Route 53 hosted zone.</p>

#### `createSubscription`

``` purescript
createSubscription :: forall eff. CreateSubscriptionRequest -> Aff (err :: RequestError | eff) CreateSubscriptionResponse
```

<p>Activates AWS Shield Advanced for an account.</p>

#### `deleteProtection`

``` purescript
deleteProtection :: forall eff. DeleteProtectionRequest -> Aff (err :: RequestError | eff) DeleteProtectionResponse
```

<p>Deletes an AWS Shield Advanced <a>Protection</a>.</p>

#### `deleteSubscription`

``` purescript
deleteSubscription :: forall eff. DeleteSubscriptionRequest -> Aff (err :: RequestError | eff) DeleteSubscriptionResponse
```

<p>Removes AWS Shield Advanced from an account. AWS Shield Advanced requires a 1-year subscription commitment. You cannot delete a subscription prior to the completion of that commitment. </p>

#### `describeAttack`

``` purescript
describeAttack :: forall eff. DescribeAttackRequest -> Aff (err :: RequestError | eff) DescribeAttackResponse
```

<p>Describes the details of a DDoS attack. </p>

#### `describeProtection`

``` purescript
describeProtection :: forall eff. DescribeProtectionRequest -> Aff (err :: RequestError | eff) DescribeProtectionResponse
```

<p>Lists the details of a <a>Protection</a> object.</p>

#### `describeSubscription`

``` purescript
describeSubscription :: forall eff. DescribeSubscriptionRequest -> Aff (err :: RequestError | eff) DescribeSubscriptionResponse
```

<p>Provides details about the AWS Shield Advanced subscription for an account.</p>

#### `getSubscriptionState`

``` purescript
getSubscriptionState :: forall eff. GetSubscriptionStateRequest -> Aff (err :: RequestError | eff) GetSubscriptionStateResponse
```

<p>Returns the <code>SubscriptionState</code>, either <code>Active</code> or <code>Inactive</code>.</p>

#### `listAttacks`

``` purescript
listAttacks :: forall eff. ListAttacksRequest -> Aff (err :: RequestError | eff) ListAttacksResponse
```

<p>Returns all ongoing DDoS attacks or all DDoS attacks during a specified time period.</p>

#### `listProtections`

``` purescript
listProtections :: forall eff. ListProtectionsRequest -> Aff (err :: RequestError | eff) ListProtectionsResponse
```

<p>Lists all <a>Protection</a> objects for the account.</p>

#### `AttackDetail`

``` purescript
newtype AttackDetail
  = AttackDetail { "AttackId" :: NullOrUndefined (AttackId), "ResourceArn" :: NullOrUndefined (ResourceArn), "SubResources" :: NullOrUndefined (SubResourceSummaryList), "StartTime" :: NullOrUndefined (AttackTimestamp), "EndTime" :: NullOrUndefined (AttackTimestamp), "AttackCounters" :: NullOrUndefined (SummarizedCounterList), "AttackProperties" :: NullOrUndefined (AttackProperties), "Mitigations" :: NullOrUndefined (MitigationList) }
```

<p>The details of a DDoS attack.</p>

##### Instances
``` purescript
Newtype AttackDetail _
```

#### `AttackId`

``` purescript
newtype AttackId
  = AttackId String
```

##### Instances
``` purescript
Newtype AttackId _
```

#### `AttackLayer`

``` purescript
newtype AttackLayer
  = AttackLayer String
```

##### Instances
``` purescript
Newtype AttackLayer _
```

#### `AttackProperties`

``` purescript
newtype AttackProperties
  = AttackProperties (Array AttackProperty)
```

##### Instances
``` purescript
Newtype AttackProperties _
```

#### `AttackProperty`

``` purescript
newtype AttackProperty
  = AttackProperty { "AttackLayer" :: NullOrUndefined (AttackLayer), "AttackPropertyIdentifier" :: NullOrUndefined (AttackPropertyIdentifier), "TopContributors" :: NullOrUndefined (TopContributors), "Unit''" :: NullOrUndefined (Unit''), "Total" :: NullOrUndefined (Number) }
```

<p>Details of the described attack.</p>

##### Instances
``` purescript
Newtype AttackProperty _
```

#### `AttackPropertyIdentifier`

``` purescript
newtype AttackPropertyIdentifier
  = AttackPropertyIdentifier String
```

##### Instances
``` purescript
Newtype AttackPropertyIdentifier _
```

#### `AttackSummaries`

``` purescript
newtype AttackSummaries
  = AttackSummaries (Array AttackSummary)
```

##### Instances
``` purescript
Newtype AttackSummaries _
```

#### `AttackSummary`

``` purescript
newtype AttackSummary
  = AttackSummary { "AttackId" :: NullOrUndefined (String), "ResourceArn" :: NullOrUndefined (String), "StartTime" :: NullOrUndefined (AttackTimestamp), "EndTime" :: NullOrUndefined (AttackTimestamp), "AttackVectors" :: NullOrUndefined (AttackVectorDescriptionList) }
```

<p>Summarizes all DDoS attacks for a specified time period.</p>

##### Instances
``` purescript
Newtype AttackSummary _
```

#### `AttackTimestamp`

``` purescript
newtype AttackTimestamp
  = AttackTimestamp Number
```

##### Instances
``` purescript
Newtype AttackTimestamp _
```

#### `AttackVectorDescription`

``` purescript
newtype AttackVectorDescription
  = AttackVectorDescription { "VectorType" :: String }
```

<p>Describes the attack.</p>

##### Instances
``` purescript
Newtype AttackVectorDescription _
```

#### `AttackVectorDescriptionList`

``` purescript
newtype AttackVectorDescriptionList
  = AttackVectorDescriptionList (Array AttackVectorDescription)
```

##### Instances
``` purescript
Newtype AttackVectorDescriptionList _
```

#### `Contributor`

``` purescript
newtype Contributor
  = Contributor { "Name" :: NullOrUndefined (String), "Value" :: NullOrUndefined (Number) }
```

<p>A contributor to the attack and their contribution.</p>

##### Instances
``` purescript
Newtype Contributor _
```

#### `CreateProtectionRequest`

``` purescript
newtype CreateProtectionRequest
  = CreateProtectionRequest { "Name" :: ProtectionName, "ResourceArn" :: ResourceArn }
```

##### Instances
``` purescript
Newtype CreateProtectionRequest _
```

#### `CreateProtectionResponse`

``` purescript
newtype CreateProtectionResponse
  = CreateProtectionResponse { "ProtectionId" :: NullOrUndefined (ProtectionId) }
```

##### Instances
``` purescript
Newtype CreateProtectionResponse _
```

#### `CreateSubscriptionRequest`

``` purescript
newtype CreateSubscriptionRequest
  = CreateSubscriptionRequest {  }
```

##### Instances
``` purescript
Newtype CreateSubscriptionRequest _
```

#### `CreateSubscriptionResponse`

``` purescript
newtype CreateSubscriptionResponse
  = CreateSubscriptionResponse {  }
```

##### Instances
``` purescript
Newtype CreateSubscriptionResponse _
```

#### `DeleteProtectionRequest`

``` purescript
newtype DeleteProtectionRequest
  = DeleteProtectionRequest { "ProtectionId" :: ProtectionId }
```

##### Instances
``` purescript
Newtype DeleteProtectionRequest _
```

#### `DeleteProtectionResponse`

``` purescript
newtype DeleteProtectionResponse
  = DeleteProtectionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteProtectionResponse _
```

#### `DeleteSubscriptionRequest`

``` purescript
newtype DeleteSubscriptionRequest
  = DeleteSubscriptionRequest {  }
```

##### Instances
``` purescript
Newtype DeleteSubscriptionRequest _
```

#### `DeleteSubscriptionResponse`

``` purescript
newtype DeleteSubscriptionResponse
  = DeleteSubscriptionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteSubscriptionResponse _
```

#### `DescribeAttackRequest`

``` purescript
newtype DescribeAttackRequest
  = DescribeAttackRequest { "AttackId" :: AttackId }
```

##### Instances
``` purescript
Newtype DescribeAttackRequest _
```

#### `DescribeAttackResponse`

``` purescript
newtype DescribeAttackResponse
  = DescribeAttackResponse { "Attack" :: NullOrUndefined (AttackDetail) }
```

##### Instances
``` purescript
Newtype DescribeAttackResponse _
```

#### `DescribeProtectionRequest`

``` purescript
newtype DescribeProtectionRequest
  = DescribeProtectionRequest { "ProtectionId" :: ProtectionId }
```

##### Instances
``` purescript
Newtype DescribeProtectionRequest _
```

#### `DescribeProtectionResponse`

``` purescript
newtype DescribeProtectionResponse
  = DescribeProtectionResponse { "Protection" :: NullOrUndefined (Protection) }
```

##### Instances
``` purescript
Newtype DescribeProtectionResponse _
```

#### `DescribeSubscriptionRequest`

``` purescript
newtype DescribeSubscriptionRequest
  = DescribeSubscriptionRequest {  }
```

##### Instances
``` purescript
Newtype DescribeSubscriptionRequest _
```

#### `DescribeSubscriptionResponse`

``` purescript
newtype DescribeSubscriptionResponse
  = DescribeSubscriptionResponse { "Subscription" :: NullOrUndefined (Subscription) }
```

##### Instances
``` purescript
Newtype DescribeSubscriptionResponse _
```

#### `DurationInSeconds`

``` purescript
newtype DurationInSeconds
  = DurationInSeconds Number
```

##### Instances
``` purescript
Newtype DurationInSeconds _
```

#### `GetSubscriptionStateRequest`

``` purescript
newtype GetSubscriptionStateRequest
  = GetSubscriptionStateRequest {  }
```

##### Instances
``` purescript
Newtype GetSubscriptionStateRequest _
```

#### `GetSubscriptionStateResponse`

``` purescript
newtype GetSubscriptionStateResponse
  = GetSubscriptionStateResponse { "SubscriptionState" :: SubscriptionState }
```

##### Instances
``` purescript
Newtype GetSubscriptionStateResponse _
```

#### `InternalErrorException`

``` purescript
newtype InternalErrorException
  = InternalErrorException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Exception that indicates that a problem occurred with the service infrastructure. You can retry the request.</p>

##### Instances
``` purescript
Newtype InternalErrorException _
```

#### `InvalidOperationException`

``` purescript
newtype InvalidOperationException
  = InvalidOperationException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Exception that indicates that the operation would not cause any change to occur.</p>

##### Instances
``` purescript
Newtype InvalidOperationException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Exception that indicates that the parameters passed to the API are invalid. </p>

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `InvalidResourceException`

``` purescript
newtype InvalidResourceException
  = InvalidResourceException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Exception that indicates that the resource is invalid. You might not have access to the resource, or the resource might not exist.</p>

##### Instances
``` purescript
Newtype InvalidResourceException _
```

#### `LimitNumber`

``` purescript
newtype LimitNumber
  = LimitNumber Number
```

##### Instances
``` purescript
Newtype LimitNumber _
```

#### `LimitType`

``` purescript
newtype LimitType
  = LimitType String
```

##### Instances
``` purescript
Newtype LimitType _
```

#### `LimitsExceededException`

``` purescript
newtype LimitsExceededException
  = LimitsExceededException { "Message'" :: NullOrUndefined (ErrorMessage'), "Type" :: NullOrUndefined (LimitType), "Limit" :: NullOrUndefined (LimitNumber) }
```

<p>Exception that indicates that the operation would exceed a limit.</p> <p> <code>Type</code> is the type of limit that would be exceeded.</p> <p> <code>Limit</code> is the threshold that would be exceeded.</p>

##### Instances
``` purescript
Newtype LimitsExceededException _
```

#### `ListAttacksRequest`

``` purescript
newtype ListAttacksRequest
  = ListAttacksRequest { "ResourceArns" :: NullOrUndefined (ResourceArnFilterList), "StartTime" :: NullOrUndefined (TimeRange), "EndTime" :: NullOrUndefined (TimeRange), "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListAttacksRequest _
```

#### `ListAttacksResponse`

``` purescript
newtype ListAttacksResponse
  = ListAttacksResponse { "AttackSummaries" :: NullOrUndefined (AttackSummaries), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype ListAttacksResponse _
```

#### `ListProtectionsRequest`

``` purescript
newtype ListProtectionsRequest
  = ListProtectionsRequest { "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListProtectionsRequest _
```

#### `ListProtectionsResponse`

``` purescript
newtype ListProtectionsResponse
  = ListProtectionsResponse { "Protections" :: NullOrUndefined (Protections), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype ListProtectionsResponse _
```

#### `LockedSubscriptionException`

``` purescript
newtype LockedSubscriptionException
  = LockedSubscriptionException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Exception that indicates that the subscription you are trying to delete has not yet completed the 1-year commitment. You cannot delete this subscription.</p>

##### Instances
``` purescript
Newtype LockedSubscriptionException _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `Mitigation`

``` purescript
newtype Mitigation
  = Mitigation { "MitigationName" :: NullOrUndefined (String) }
```

<p>The mitigation applied to a DDoS attack.</p>

##### Instances
``` purescript
Newtype Mitigation _
```

#### `MitigationList`

``` purescript
newtype MitigationList
  = MitigationList (Array Mitigation)
```

##### Instances
``` purescript
Newtype MitigationList _
```

#### `OptimisticLockException`

``` purescript
newtype OptimisticLockException
  = OptimisticLockException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Exception that indicates that the protection state has been modified by another client. You can retry the request.</p>

##### Instances
``` purescript
Newtype OptimisticLockException _
```

#### `Protection`

``` purescript
newtype Protection
  = Protection { "Id" :: NullOrUndefined (ProtectionId), "Name" :: NullOrUndefined (ProtectionName), "ResourceArn" :: NullOrUndefined (ResourceArn) }
```

<p>An object that represents a resource that is under DDoS protection.</p>

##### Instances
``` purescript
Newtype Protection _
```

#### `ProtectionId`

``` purescript
newtype ProtectionId
  = ProtectionId String
```

##### Instances
``` purescript
Newtype ProtectionId _
```

#### `ProtectionName`

``` purescript
newtype ProtectionName
  = ProtectionName String
```

##### Instances
``` purescript
Newtype ProtectionName _
```

#### `Protections`

``` purescript
newtype Protections
  = Protections (Array Protection)
```

##### Instances
``` purescript
Newtype Protections _
```

#### `ResourceAlreadyExistsException`

``` purescript
newtype ResourceAlreadyExistsException
  = ResourceAlreadyExistsException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Exception indicating the specified resource already exists.</p>

##### Instances
``` purescript
Newtype ResourceAlreadyExistsException _
```

#### `ResourceArn`

``` purescript
newtype ResourceArn
  = ResourceArn String
```

##### Instances
``` purescript
Newtype ResourceArn _
```

#### `ResourceArnFilterList`

``` purescript
newtype ResourceArnFilterList
  = ResourceArnFilterList (Array ResourceArn)
```

##### Instances
``` purescript
Newtype ResourceArnFilterList _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>Exception indicating the specified resource does not exist.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `SubResourceSummary`

``` purescript
newtype SubResourceSummary
  = SubResourceSummary { "Type" :: NullOrUndefined (SubResourceType), "Id" :: NullOrUndefined (String), "AttackVectors" :: NullOrUndefined (SummarizedAttackVectorList), "Counters" :: NullOrUndefined (SummarizedCounterList) }
```

<p>The attack information for the specified SubResource.</p>

##### Instances
``` purescript
Newtype SubResourceSummary _
```

#### `SubResourceSummaryList`

``` purescript
newtype SubResourceSummaryList
  = SubResourceSummaryList (Array SubResourceSummary)
```

##### Instances
``` purescript
Newtype SubResourceSummaryList _
```

#### `SubResourceType`

``` purescript
newtype SubResourceType
  = SubResourceType String
```

##### Instances
``` purescript
Newtype SubResourceType _
```

#### `Subscription`

``` purescript
newtype Subscription
  = Subscription { "StartTime" :: NullOrUndefined (Number), "TimeCommitmentInSeconds" :: NullOrUndefined (DurationInSeconds) }
```

<p>Information about the AWS Shield Advanced subscription for an account.</p>

##### Instances
``` purescript
Newtype Subscription _
```

#### `SubscriptionState`

``` purescript
newtype SubscriptionState
  = SubscriptionState String
```

##### Instances
``` purescript
Newtype SubscriptionState _
```

#### `SummarizedAttackVector`

``` purescript
newtype SummarizedAttackVector
  = SummarizedAttackVector { "VectorType" :: String, "VectorCounters" :: NullOrUndefined (SummarizedCounterList) }
```

<p>A summary of information about the attack.</p>

##### Instances
``` purescript
Newtype SummarizedAttackVector _
```

#### `SummarizedAttackVectorList`

``` purescript
newtype SummarizedAttackVectorList
  = SummarizedAttackVectorList (Array SummarizedAttackVector)
```

##### Instances
``` purescript
Newtype SummarizedAttackVectorList _
```

#### `SummarizedCounter`

``` purescript
newtype SummarizedCounter
  = SummarizedCounter { "Name" :: NullOrUndefined (String), "Max" :: NullOrUndefined (Number), "Average" :: NullOrUndefined (Number), "Sum" :: NullOrUndefined (Number), "N" :: NullOrUndefined (Int), "Unit''" :: NullOrUndefined (String) }
```

<p>The counter that describes a DDoS attack.</p>

##### Instances
``` purescript
Newtype SummarizedCounter _
```

#### `SummarizedCounterList`

``` purescript
newtype SummarizedCounterList
  = SummarizedCounterList (Array SummarizedCounter)
```

##### Instances
``` purescript
Newtype SummarizedCounterList _
```

#### `TimeRange`

``` purescript
newtype TimeRange
  = TimeRange { "FromInclusive" :: NullOrUndefined (AttackTimestamp), "ToExclusive" :: NullOrUndefined (AttackTimestamp) }
```

<p>The time range.</p>

##### Instances
``` purescript
Newtype TimeRange _
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

#### `TopContributors`

``` purescript
newtype TopContributors
  = TopContributors (Array Contributor)
```

##### Instances
``` purescript
Newtype TopContributors _
```

#### `Unit''`

``` purescript
newtype Unit''
  = Unit'' String
```

##### Instances
``` purescript
Newtype Unit'' _
```

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```

##### Instances
``` purescript
Newtype ErrorMessage' _
```


