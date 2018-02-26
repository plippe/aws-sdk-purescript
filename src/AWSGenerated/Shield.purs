

-- | <fullname>AWS Shield Advanced</fullname> <p>This is the <i>AWS Shield Advanced API Reference</i>. This guide is for developers who need detailed information about the AWS Shield Advanced API actions, data types, and errors. For detailed information about AWS WAF and AWS Shield Advanced features and an overview of how to use the AWS WAF and AWS Shield Advanced APIs, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF and AWS Shield Developer Guide</a>.</p>
module AWS.Shield where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Shield" :: String


-- | <p>Enables AWS Shield Advanced for a specific AWS resource. The resource can be an Amazon CloudFront distribution, Elastic Load Balancing load balancer, Elastic IP Address, or an Amazon Route 53 hosted zone.</p>
createProtection :: forall eff. CreateProtectionRequest -> Aff (err :: AWS.RequestError | eff) CreateProtectionResponse
createProtection = AWS.request serviceName "CreateProtection" 


-- | <p>Activates AWS Shield Advanced for an account.</p>
createSubscription :: forall eff. CreateSubscriptionRequest -> Aff (err :: AWS.RequestError | eff) CreateSubscriptionResponse
createSubscription = AWS.request serviceName "CreateSubscription" 


-- | <p>Deletes an AWS Shield Advanced <a>Protection</a>.</p>
deleteProtection :: forall eff. DeleteProtectionRequest -> Aff (err :: AWS.RequestError | eff) DeleteProtectionResponse
deleteProtection = AWS.request serviceName "DeleteProtection" 


-- | <p>Removes AWS Shield Advanced from an account. AWS Shield Advanced requires a 1-year subscription commitment. You cannot delete a subscription prior to the completion of that commitment. </p>
deleteSubscription :: forall eff. DeleteSubscriptionRequest -> Aff (err :: AWS.RequestError | eff) DeleteSubscriptionResponse
deleteSubscription = AWS.request serviceName "DeleteSubscription" 


-- | <p>Describes the details of a DDoS attack. </p>
describeAttack :: forall eff. DescribeAttackRequest -> Aff (err :: AWS.RequestError | eff) DescribeAttackResponse
describeAttack = AWS.request serviceName "DescribeAttack" 


-- | <p>Lists the details of a <a>Protection</a> object.</p>
describeProtection :: forall eff. DescribeProtectionRequest -> Aff (err :: AWS.RequestError | eff) DescribeProtectionResponse
describeProtection = AWS.request serviceName "DescribeProtection" 


-- | <p>Provides details about the AWS Shield Advanced subscription for an account.</p>
describeSubscription :: forall eff. DescribeSubscriptionRequest -> Aff (err :: AWS.RequestError | eff) DescribeSubscriptionResponse
describeSubscription = AWS.request serviceName "DescribeSubscription" 


-- | <p>Returns the <code>SubscriptionState</code>, either <code>Active</code> or <code>Inactive</code>.</p>
getSubscriptionState :: forall eff. GetSubscriptionStateRequest -> Aff (err :: AWS.RequestError | eff) GetSubscriptionStateResponse
getSubscriptionState = AWS.request serviceName "GetSubscriptionState" 


-- | <p>Returns all ongoing DDoS attacks or all DDoS attacks during a specified time period.</p>
listAttacks :: forall eff. ListAttacksRequest -> Aff (err :: AWS.RequestError | eff) ListAttacksResponse
listAttacks = AWS.request serviceName "ListAttacks" 


-- | <p>Lists all <a>Protection</a> objects for the account.</p>
listProtections :: forall eff. ListProtectionsRequest -> Aff (err :: AWS.RequestError | eff) ListProtectionsResponse
listProtections = AWS.request serviceName "ListProtections" 


-- | <p>The details of a DDoS attack.</p>
newtype AttackDetail = AttackDetail 
  { "AttackId" :: NullOrUndefined (AttackId)
  , "ResourceArn" :: NullOrUndefined (ResourceArn)
  , "SubResources" :: NullOrUndefined (SubResourceSummaryList)
  , "StartTime" :: NullOrUndefined (AttackTimestamp)
  , "EndTime" :: NullOrUndefined (AttackTimestamp)
  , "AttackCounters" :: NullOrUndefined (SummarizedCounterList)
  , "AttackProperties" :: NullOrUndefined (AttackProperties)
  , "Mitigations" :: NullOrUndefined (MitigationList)
  }


newtype AttackId = AttackId String


newtype AttackLayer = AttackLayer String


newtype AttackProperties = AttackProperties (Array AttackProperty)


-- | <p>Details of the described attack.</p>
newtype AttackProperty = AttackProperty 
  { "AttackLayer" :: NullOrUndefined (AttackLayer)
  , "AttackPropertyIdentifier" :: NullOrUndefined (AttackPropertyIdentifier)
  , "TopContributors" :: NullOrUndefined (TopContributors)
  , "Unit''" :: NullOrUndefined (Unit'')
  , "Total" :: NullOrUndefined (Number)
  }


newtype AttackPropertyIdentifier = AttackPropertyIdentifier String


newtype AttackSummaries = AttackSummaries (Array AttackSummary)


-- | <p>Summarizes all DDoS attacks for a specified time period.</p>
newtype AttackSummary = AttackSummary 
  { "AttackId" :: NullOrUndefined (String)
  , "ResourceArn" :: NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined (AttackTimestamp)
  , "EndTime" :: NullOrUndefined (AttackTimestamp)
  , "AttackVectors" :: NullOrUndefined (AttackVectorDescriptionList)
  }


newtype AttackTimestamp = AttackTimestamp Number


-- | <p>Describes the attack.</p>
newtype AttackVectorDescription = AttackVectorDescription 
  { "VectorType" :: (String)
  }


newtype AttackVectorDescriptionList = AttackVectorDescriptionList (Array AttackVectorDescription)


-- | <p>A contributor to the attack and their contribution.</p>
newtype Contributor = Contributor 
  { "Name" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (Number)
  }


newtype CreateProtectionRequest = CreateProtectionRequest 
  { "Name" :: (ProtectionName)
  , "ResourceArn" :: (ResourceArn)
  }


newtype CreateProtectionResponse = CreateProtectionResponse 
  { "ProtectionId" :: NullOrUndefined (ProtectionId)
  }


newtype CreateSubscriptionRequest = CreateSubscriptionRequest 
  { 
  }


newtype CreateSubscriptionResponse = CreateSubscriptionResponse 
  { 
  }


newtype DeleteProtectionRequest = DeleteProtectionRequest 
  { "ProtectionId" :: (ProtectionId)
  }


newtype DeleteProtectionResponse = DeleteProtectionResponse 
  { 
  }


newtype DeleteSubscriptionRequest = DeleteSubscriptionRequest 
  { 
  }


newtype DeleteSubscriptionResponse = DeleteSubscriptionResponse 
  { 
  }


newtype DescribeAttackRequest = DescribeAttackRequest 
  { "AttackId" :: (AttackId)
  }


newtype DescribeAttackResponse = DescribeAttackResponse 
  { "Attack" :: NullOrUndefined (AttackDetail)
  }


newtype DescribeProtectionRequest = DescribeProtectionRequest 
  { "ProtectionId" :: (ProtectionId)
  }


newtype DescribeProtectionResponse = DescribeProtectionResponse 
  { "Protection" :: NullOrUndefined (Protection)
  }


newtype DescribeSubscriptionRequest = DescribeSubscriptionRequest 
  { 
  }


newtype DescribeSubscriptionResponse = DescribeSubscriptionResponse 
  { "Subscription" :: NullOrUndefined (Subscription)
  }


newtype DurationInSeconds = DurationInSeconds Number


newtype GetSubscriptionStateRequest = GetSubscriptionStateRequest 
  { 
  }


newtype GetSubscriptionStateResponse = GetSubscriptionStateResponse 
  { "SubscriptionState" :: (SubscriptionState)
  }


-- | <p>Exception that indicates that a problem occurred with the service infrastructure. You can retry the request.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>Exception that indicates that the operation would not cause any change to occur.</p>
newtype InvalidOperationException = InvalidOperationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>Exception that indicates that the parameters passed to the API are invalid. </p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>Exception that indicates that the resource is invalid. You might not have access to the resource, or the resource might not exist.</p>
newtype InvalidResourceException = InvalidResourceException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype LimitNumber = LimitNumber Number


newtype LimitType = LimitType String


-- | <p>Exception that indicates that the operation would exceed a limit.</p> <p> <code>Type</code> is the type of limit that would be exceeded.</p> <p> <code>Limit</code> is the threshold that would be exceeded.</p>
newtype LimitsExceededException = LimitsExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  , "Type" :: NullOrUndefined (LimitType)
  , "Limit" :: NullOrUndefined (LimitNumber)
  }


newtype ListAttacksRequest = ListAttacksRequest 
  { "ResourceArns" :: NullOrUndefined (ResourceArnFilterList)
  , "StartTime" :: NullOrUndefined (TimeRange)
  , "EndTime" :: NullOrUndefined (TimeRange)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListAttacksResponse = ListAttacksResponse 
  { "AttackSummaries" :: NullOrUndefined (AttackSummaries)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype ListProtectionsRequest = ListProtectionsRequest 
  { "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListProtectionsResponse = ListProtectionsResponse 
  { "Protections" :: NullOrUndefined (Protections)
  , "NextToken" :: NullOrUndefined (Token)
  }


-- | <p>Exception that indicates that the subscription you are trying to delete has not yet completed the 1-year commitment. You cannot delete this subscription.</p>
newtype LockedSubscriptionException = LockedSubscriptionException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype MaxResults = MaxResults Int


-- | <p>The mitigation applied to a DDoS attack.</p>
newtype Mitigation = Mitigation 
  { "MitigationName" :: NullOrUndefined (String)
  }


newtype MitigationList = MitigationList (Array Mitigation)


-- | <p>Exception that indicates that the protection state has been modified by another client. You can retry the request.</p>
newtype OptimisticLockException = OptimisticLockException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>An object that represents a resource that is under DDoS protection.</p>
newtype Protection = Protection 
  { "Id" :: NullOrUndefined (ProtectionId)
  , "Name" :: NullOrUndefined (ProtectionName)
  , "ResourceArn" :: NullOrUndefined (ResourceArn)
  }


newtype ProtectionId = ProtectionId String


newtype ProtectionName = ProtectionName String


newtype Protections = Protections (Array Protection)


-- | <p>Exception indicating the specified resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype ResourceArn = ResourceArn String


newtype ResourceArnFilterList = ResourceArnFilterList (Array ResourceArn)


-- | <p>Exception indicating the specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The attack information for the specified SubResource.</p>
newtype SubResourceSummary = SubResourceSummary 
  { "Type" :: NullOrUndefined (SubResourceType)
  , "Id" :: NullOrUndefined (String)
  , "AttackVectors" :: NullOrUndefined (SummarizedAttackVectorList)
  , "Counters" :: NullOrUndefined (SummarizedCounterList)
  }


newtype SubResourceSummaryList = SubResourceSummaryList (Array SubResourceSummary)


newtype SubResourceType = SubResourceType String


-- | <p>Information about the AWS Shield Advanced subscription for an account.</p>
newtype Subscription = Subscription 
  { "StartTime" :: NullOrUndefined (Number)
  , "TimeCommitmentInSeconds" :: NullOrUndefined (DurationInSeconds)
  }


newtype SubscriptionState = SubscriptionState String


-- | <p>A summary of information about the attack.</p>
newtype SummarizedAttackVector = SummarizedAttackVector 
  { "VectorType" :: (String)
  , "VectorCounters" :: NullOrUndefined (SummarizedCounterList)
  }


newtype SummarizedAttackVectorList = SummarizedAttackVectorList (Array SummarizedAttackVector)


-- | <p>The counter that describes a DDoS attack.</p>
newtype SummarizedCounter = SummarizedCounter 
  { "Name" :: NullOrUndefined (String)
  , "Max" :: NullOrUndefined (Number)
  , "Average" :: NullOrUndefined (Number)
  , "Sum" :: NullOrUndefined (Number)
  , "N" :: NullOrUndefined (Int)
  , "Unit''" :: NullOrUndefined (String)
  }


newtype SummarizedCounterList = SummarizedCounterList (Array SummarizedCounter)


-- | <p>The time range.</p>
newtype TimeRange = TimeRange 
  { "FromInclusive" :: NullOrUndefined (AttackTimestamp)
  , "ToExclusive" :: NullOrUndefined (AttackTimestamp)
  }


newtype Token = Token String


newtype TopContributors = TopContributors (Array Contributor)


newtype Unit'' = Unit'' String


newtype ErrorMessage' = ErrorMessage' String
