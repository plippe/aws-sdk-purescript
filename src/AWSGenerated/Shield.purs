

-- | <fullname>AWS Shield Advanced</fullname> <p>This is the <i>AWS Shield Advanced API Reference</i>. This guide is for developers who need detailed information about the AWS Shield Advanced API actions, data types, and errors. For detailed information about AWS WAF and AWS Shield Advanced features and an overview of how to use the AWS WAF and AWS Shield Advanced APIs, see the <a href="http://docs.aws.amazon.com/waf/latest/developerguide/">AWS WAF and AWS Shield Developer Guide</a>.</p>
module AWS.Shield where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeAttackDetail :: Newtype AttackDetail _


newtype AttackId = AttackId String
derive instance newtypeAttackId :: Newtype AttackId _


newtype AttackLayer = AttackLayer String
derive instance newtypeAttackLayer :: Newtype AttackLayer _


newtype AttackProperties = AttackProperties (Array AttackProperty)
derive instance newtypeAttackProperties :: Newtype AttackProperties _


-- | <p>Details of the described attack.</p>
newtype AttackProperty = AttackProperty 
  { "AttackLayer" :: NullOrUndefined (AttackLayer)
  , "AttackPropertyIdentifier" :: NullOrUndefined (AttackPropertyIdentifier)
  , "TopContributors" :: NullOrUndefined (TopContributors)
  , "Unit''" :: NullOrUndefined (Unit'')
  , "Total" :: NullOrUndefined (Number)
  }
derive instance newtypeAttackProperty :: Newtype AttackProperty _


newtype AttackPropertyIdentifier = AttackPropertyIdentifier String
derive instance newtypeAttackPropertyIdentifier :: Newtype AttackPropertyIdentifier _


newtype AttackSummaries = AttackSummaries (Array AttackSummary)
derive instance newtypeAttackSummaries :: Newtype AttackSummaries _


-- | <p>Summarizes all DDoS attacks for a specified time period.</p>
newtype AttackSummary = AttackSummary 
  { "AttackId" :: NullOrUndefined (String)
  , "ResourceArn" :: NullOrUndefined (String)
  , "StartTime" :: NullOrUndefined (AttackTimestamp)
  , "EndTime" :: NullOrUndefined (AttackTimestamp)
  , "AttackVectors" :: NullOrUndefined (AttackVectorDescriptionList)
  }
derive instance newtypeAttackSummary :: Newtype AttackSummary _


newtype AttackTimestamp = AttackTimestamp Number
derive instance newtypeAttackTimestamp :: Newtype AttackTimestamp _


-- | <p>Describes the attack.</p>
newtype AttackVectorDescription = AttackVectorDescription 
  { "VectorType" :: (String)
  }
derive instance newtypeAttackVectorDescription :: Newtype AttackVectorDescription _


newtype AttackVectorDescriptionList = AttackVectorDescriptionList (Array AttackVectorDescription)
derive instance newtypeAttackVectorDescriptionList :: Newtype AttackVectorDescriptionList _


-- | <p>A contributor to the attack and their contribution.</p>
newtype Contributor = Contributor 
  { "Name" :: NullOrUndefined (String)
  , "Value" :: NullOrUndefined (Number)
  }
derive instance newtypeContributor :: Newtype Contributor _


newtype CreateProtectionRequest = CreateProtectionRequest 
  { "Name" :: (ProtectionName)
  , "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeCreateProtectionRequest :: Newtype CreateProtectionRequest _


newtype CreateProtectionResponse = CreateProtectionResponse 
  { "ProtectionId" :: NullOrUndefined (ProtectionId)
  }
derive instance newtypeCreateProtectionResponse :: Newtype CreateProtectionResponse _


newtype CreateSubscriptionRequest = CreateSubscriptionRequest 
  { 
  }
derive instance newtypeCreateSubscriptionRequest :: Newtype CreateSubscriptionRequest _


newtype CreateSubscriptionResponse = CreateSubscriptionResponse 
  { 
  }
derive instance newtypeCreateSubscriptionResponse :: Newtype CreateSubscriptionResponse _


newtype DeleteProtectionRequest = DeleteProtectionRequest 
  { "ProtectionId" :: (ProtectionId)
  }
derive instance newtypeDeleteProtectionRequest :: Newtype DeleteProtectionRequest _


newtype DeleteProtectionResponse = DeleteProtectionResponse 
  { 
  }
derive instance newtypeDeleteProtectionResponse :: Newtype DeleteProtectionResponse _


newtype DeleteSubscriptionRequest = DeleteSubscriptionRequest 
  { 
  }
derive instance newtypeDeleteSubscriptionRequest :: Newtype DeleteSubscriptionRequest _


newtype DeleteSubscriptionResponse = DeleteSubscriptionResponse 
  { 
  }
derive instance newtypeDeleteSubscriptionResponse :: Newtype DeleteSubscriptionResponse _


newtype DescribeAttackRequest = DescribeAttackRequest 
  { "AttackId" :: (AttackId)
  }
derive instance newtypeDescribeAttackRequest :: Newtype DescribeAttackRequest _


newtype DescribeAttackResponse = DescribeAttackResponse 
  { "Attack" :: NullOrUndefined (AttackDetail)
  }
derive instance newtypeDescribeAttackResponse :: Newtype DescribeAttackResponse _


newtype DescribeProtectionRequest = DescribeProtectionRequest 
  { "ProtectionId" :: (ProtectionId)
  }
derive instance newtypeDescribeProtectionRequest :: Newtype DescribeProtectionRequest _


newtype DescribeProtectionResponse = DescribeProtectionResponse 
  { "Protection" :: NullOrUndefined (Protection)
  }
derive instance newtypeDescribeProtectionResponse :: Newtype DescribeProtectionResponse _


newtype DescribeSubscriptionRequest = DescribeSubscriptionRequest 
  { 
  }
derive instance newtypeDescribeSubscriptionRequest :: Newtype DescribeSubscriptionRequest _


newtype DescribeSubscriptionResponse = DescribeSubscriptionResponse 
  { "Subscription" :: NullOrUndefined (Subscription)
  }
derive instance newtypeDescribeSubscriptionResponse :: Newtype DescribeSubscriptionResponse _


newtype DurationInSeconds = DurationInSeconds Number
derive instance newtypeDurationInSeconds :: Newtype DurationInSeconds _


newtype GetSubscriptionStateRequest = GetSubscriptionStateRequest 
  { 
  }
derive instance newtypeGetSubscriptionStateRequest :: Newtype GetSubscriptionStateRequest _


newtype GetSubscriptionStateResponse = GetSubscriptionStateResponse 
  { "SubscriptionState" :: (SubscriptionState)
  }
derive instance newtypeGetSubscriptionStateResponse :: Newtype GetSubscriptionStateResponse _


-- | <p>Exception that indicates that a problem occurred with the service infrastructure. You can retry the request.</p>
newtype InternalErrorException = InternalErrorException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalErrorException :: Newtype InternalErrorException _


-- | <p>Exception that indicates that the operation would not cause any change to occur.</p>
newtype InvalidOperationException = InvalidOperationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidOperationException :: Newtype InvalidOperationException _


-- | <p>Exception that indicates that the parameters passed to the API are invalid. </p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>Exception that indicates that the resource is invalid. You might not have access to the resource, or the resource might not exist.</p>
newtype InvalidResourceException = InvalidResourceException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidResourceException :: Newtype InvalidResourceException _


newtype LimitNumber = LimitNumber Number
derive instance newtypeLimitNumber :: Newtype LimitNumber _


newtype LimitType = LimitType String
derive instance newtypeLimitType :: Newtype LimitType _


-- | <p>Exception that indicates that the operation would exceed a limit.</p> <p> <code>Type</code> is the type of limit that would be exceeded.</p> <p> <code>Limit</code> is the threshold that would be exceeded.</p>
newtype LimitsExceededException = LimitsExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  , "Type" :: NullOrUndefined (LimitType)
  , "Limit" :: NullOrUndefined (LimitNumber)
  }
derive instance newtypeLimitsExceededException :: Newtype LimitsExceededException _


newtype ListAttacksRequest = ListAttacksRequest 
  { "ResourceArns" :: NullOrUndefined (ResourceArnFilterList)
  , "StartTime" :: NullOrUndefined (TimeRange)
  , "EndTime" :: NullOrUndefined (TimeRange)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListAttacksRequest :: Newtype ListAttacksRequest _


newtype ListAttacksResponse = ListAttacksResponse 
  { "AttackSummaries" :: NullOrUndefined (AttackSummaries)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListAttacksResponse :: Newtype ListAttacksResponse _


newtype ListProtectionsRequest = ListProtectionsRequest 
  { "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListProtectionsRequest :: Newtype ListProtectionsRequest _


newtype ListProtectionsResponse = ListProtectionsResponse 
  { "Protections" :: NullOrUndefined (Protections)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListProtectionsResponse :: Newtype ListProtectionsResponse _


-- | <p>Exception that indicates that the subscription you are trying to delete has not yet completed the 1-year commitment. You cannot delete this subscription.</p>
newtype LockedSubscriptionException = LockedSubscriptionException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeLockedSubscriptionException :: Newtype LockedSubscriptionException _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p>The mitigation applied to a DDoS attack.</p>
newtype Mitigation = Mitigation 
  { "MitigationName" :: NullOrUndefined (String)
  }
derive instance newtypeMitigation :: Newtype Mitigation _


newtype MitigationList = MitigationList (Array Mitigation)
derive instance newtypeMitigationList :: Newtype MitigationList _


-- | <p>Exception that indicates that the protection state has been modified by another client. You can retry the request.</p>
newtype OptimisticLockException = OptimisticLockException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeOptimisticLockException :: Newtype OptimisticLockException _


-- | <p>An object that represents a resource that is under DDoS protection.</p>
newtype Protection = Protection 
  { "Id" :: NullOrUndefined (ProtectionId)
  , "Name" :: NullOrUndefined (ProtectionName)
  , "ResourceArn" :: NullOrUndefined (ResourceArn)
  }
derive instance newtypeProtection :: Newtype Protection _


newtype ProtectionId = ProtectionId String
derive instance newtypeProtectionId :: Newtype ProtectionId _


newtype ProtectionName = ProtectionName String
derive instance newtypeProtectionName :: Newtype ProtectionName _


newtype Protections = Protections (Array Protection)
derive instance newtypeProtections :: Newtype Protections _


-- | <p>Exception indicating the specified resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _


newtype ResourceArnFilterList = ResourceArnFilterList (Array ResourceArn)
derive instance newtypeResourceArnFilterList :: Newtype ResourceArnFilterList _


-- | <p>Exception indicating the specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The attack information for the specified SubResource.</p>
newtype SubResourceSummary = SubResourceSummary 
  { "Type" :: NullOrUndefined (SubResourceType)
  , "Id" :: NullOrUndefined (String)
  , "AttackVectors" :: NullOrUndefined (SummarizedAttackVectorList)
  , "Counters" :: NullOrUndefined (SummarizedCounterList)
  }
derive instance newtypeSubResourceSummary :: Newtype SubResourceSummary _


newtype SubResourceSummaryList = SubResourceSummaryList (Array SubResourceSummary)
derive instance newtypeSubResourceSummaryList :: Newtype SubResourceSummaryList _


newtype SubResourceType = SubResourceType String
derive instance newtypeSubResourceType :: Newtype SubResourceType _


-- | <p>Information about the AWS Shield Advanced subscription for an account.</p>
newtype Subscription = Subscription 
  { "StartTime" :: NullOrUndefined (Number)
  , "TimeCommitmentInSeconds" :: NullOrUndefined (DurationInSeconds)
  }
derive instance newtypeSubscription :: Newtype Subscription _


newtype SubscriptionState = SubscriptionState String
derive instance newtypeSubscriptionState :: Newtype SubscriptionState _


-- | <p>A summary of information about the attack.</p>
newtype SummarizedAttackVector = SummarizedAttackVector 
  { "VectorType" :: (String)
  , "VectorCounters" :: NullOrUndefined (SummarizedCounterList)
  }
derive instance newtypeSummarizedAttackVector :: Newtype SummarizedAttackVector _


newtype SummarizedAttackVectorList = SummarizedAttackVectorList (Array SummarizedAttackVector)
derive instance newtypeSummarizedAttackVectorList :: Newtype SummarizedAttackVectorList _


-- | <p>The counter that describes a DDoS attack.</p>
newtype SummarizedCounter = SummarizedCounter 
  { "Name" :: NullOrUndefined (String)
  , "Max" :: NullOrUndefined (Number)
  , "Average" :: NullOrUndefined (Number)
  , "Sum" :: NullOrUndefined (Number)
  , "N" :: NullOrUndefined (Int)
  , "Unit''" :: NullOrUndefined (String)
  }
derive instance newtypeSummarizedCounter :: Newtype SummarizedCounter _


newtype SummarizedCounterList = SummarizedCounterList (Array SummarizedCounter)
derive instance newtypeSummarizedCounterList :: Newtype SummarizedCounterList _


-- | <p>The time range.</p>
newtype TimeRange = TimeRange 
  { "FromInclusive" :: NullOrUndefined (AttackTimestamp)
  , "ToExclusive" :: NullOrUndefined (AttackTimestamp)
  }
derive instance newtypeTimeRange :: Newtype TimeRange _


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _


newtype TopContributors = TopContributors (Array Contributor)
derive instance newtypeTopContributors :: Newtype TopContributors _


newtype Unit'' = Unit'' String
derive instance newtypeUnit'' :: Newtype Unit'' _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
