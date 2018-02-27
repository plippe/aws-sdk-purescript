

-- | <p>Use AWS Auto Scaling to quickly discover all the scalable AWS resources for your application and configure dynamic scaling for your scalable resources.</p> <p>To get started, create a scaling plan with a set of instructions used to configure dynamic scaling for the scalable resources in your application. AWS Auto Scaling creates target tracking scaling policies for the scalable resources in your scaling plan. Target tracking scaling policies adjust the capacity of your scalable resource as required to maintain resource utilization at the target value that you specified.</p>
module AWS.AutoScalingPlans where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AutoScalingPlans" :: String


-- | <p>Creates a scaling plan.</p> <p>A scaling plan contains a set of instructions used to configure dynamic scaling for the scalable resources in your application. AWS Auto Scaling creates target tracking scaling policies based on the scaling instructions in your scaling plan.</p>
createScalingPlan :: forall eff. CreateScalingPlanRequest -> Aff (err :: AWS.RequestError | eff) CreateScalingPlanResponse
createScalingPlan = AWS.request serviceName "createScalingPlan" 


-- | <p>Deletes the specified scaling plan.</p>
deleteScalingPlan :: forall eff. DeleteScalingPlanRequest -> Aff (err :: AWS.RequestError | eff) DeleteScalingPlanResponse
deleteScalingPlan = AWS.request serviceName "deleteScalingPlan" 


-- | <p>Describes the scalable resources in the specified scaling plan.</p>
describeScalingPlanResources :: forall eff. DescribeScalingPlanResourcesRequest -> Aff (err :: AWS.RequestError | eff) DescribeScalingPlanResourcesResponse
describeScalingPlanResources = AWS.request serviceName "describeScalingPlanResources" 


-- | <p>Describes the specified scaling plans or all of your scaling plans.</p>
describeScalingPlans :: forall eff. DescribeScalingPlansRequest -> Aff (err :: AWS.RequestError | eff) DescribeScalingPlansResponse
describeScalingPlans = AWS.request serviceName "describeScalingPlans" 


-- | <p>Represents an application source.</p>
newtype ApplicationSource = ApplicationSource 
  { "CloudFormationStackARN" :: NullOrUndefined (XmlString)
  }
derive instance newtypeApplicationSource :: Newtype ApplicationSource _


newtype ApplicationSources = ApplicationSources (Array ApplicationSource)
derive instance newtypeApplicationSources :: Newtype ApplicationSources _


-- | <p>Concurrent updates caused an exception, for example, if you request an update to a scaling plan that already has a pending update.</p>
newtype ConcurrentUpdateException = ConcurrentUpdateException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentUpdateException :: Newtype ConcurrentUpdateException _


newtype Cooldown = Cooldown Int
derive instance newtypeCooldown :: Newtype Cooldown _


newtype CreateScalingPlanRequest = CreateScalingPlanRequest 
  { "ScalingPlanName" :: (ScalingPlanName)
  , "ApplicationSource" :: (ApplicationSource)
  , "ScalingInstructions" :: (ScalingInstructions)
  }
derive instance newtypeCreateScalingPlanRequest :: Newtype CreateScalingPlanRequest _


newtype CreateScalingPlanResponse = CreateScalingPlanResponse 
  { "ScalingPlanVersion" :: (ScalingPlanVersion)
  }
derive instance newtypeCreateScalingPlanResponse :: Newtype CreateScalingPlanResponse _


-- | <p>Represents a customized metric for a target tracking policy.</p>
newtype CustomizedScalingMetricSpecification = CustomizedScalingMetricSpecification 
  { "MetricName" :: (MetricName)
  , "Namespace" :: (MetricNamespace)
  , "Dimensions" :: NullOrUndefined (MetricDimensions)
  , "Statistic" :: (MetricStatistic)
  , "Unit''" :: NullOrUndefined (MetricUnit)
  }
derive instance newtypeCustomizedScalingMetricSpecification :: Newtype CustomizedScalingMetricSpecification _


newtype DeleteScalingPlanRequest = DeleteScalingPlanRequest 
  { "ScalingPlanName" :: (ScalingPlanName)
  , "ScalingPlanVersion" :: (ScalingPlanVersion)
  }
derive instance newtypeDeleteScalingPlanRequest :: Newtype DeleteScalingPlanRequest _


newtype DeleteScalingPlanResponse = DeleteScalingPlanResponse 
  { 
  }
derive instance newtypeDeleteScalingPlanResponse :: Newtype DeleteScalingPlanResponse _


newtype DescribeScalingPlanResourcesRequest = DescribeScalingPlanResourcesRequest 
  { "ScalingPlanName" :: (ScalingPlanName)
  , "ScalingPlanVersion" :: (ScalingPlanVersion)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeScalingPlanResourcesRequest :: Newtype DescribeScalingPlanResourcesRequest _


newtype DescribeScalingPlanResourcesResponse = DescribeScalingPlanResourcesResponse 
  { "ScalingPlanResources" :: NullOrUndefined (ScalingPlanResources)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeScalingPlanResourcesResponse :: Newtype DescribeScalingPlanResourcesResponse _


newtype DescribeScalingPlansRequest = DescribeScalingPlansRequest 
  { "ScalingPlanNames" :: NullOrUndefined (ScalingPlanNames)
  , "ScalingPlanVersion" :: NullOrUndefined (ScalingPlanVersion)
  , "ApplicationSources" :: NullOrUndefined (ApplicationSources)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeScalingPlansRequest :: Newtype DescribeScalingPlansRequest _


newtype DescribeScalingPlansResponse = DescribeScalingPlansResponse 
  { "ScalingPlans" :: NullOrUndefined (ScalingPlans)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeScalingPlansResponse :: Newtype DescribeScalingPlansResponse _


newtype DisableScaleIn = DisableScaleIn Boolean
derive instance newtypeDisableScaleIn :: Newtype DisableScaleIn _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>The service encountered an internal error.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServiceException :: Newtype InternalServiceException _


-- | <p>The token provided is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>Your account exceeded a limit. This exception is thrown when a per-account resource limit is exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p>Represents a dimension for a customized metric.</p>
newtype MetricDimension = MetricDimension 
  { "Name" :: (MetricDimensionName)
  , "Value" :: (MetricDimensionValue)
  }
derive instance newtypeMetricDimension :: Newtype MetricDimension _


newtype MetricDimensionName = MetricDimensionName String
derive instance newtypeMetricDimensionName :: Newtype MetricDimensionName _


newtype MetricDimensionValue = MetricDimensionValue String
derive instance newtypeMetricDimensionValue :: Newtype MetricDimensionValue _


newtype MetricDimensions = MetricDimensions (Array MetricDimension)
derive instance newtypeMetricDimensions :: Newtype MetricDimensions _


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _


newtype MetricNamespace = MetricNamespace String
derive instance newtypeMetricNamespace :: Newtype MetricNamespace _


newtype MetricScale = MetricScale Number
derive instance newtypeMetricScale :: Newtype MetricScale _


newtype MetricStatistic = MetricStatistic String
derive instance newtypeMetricStatistic :: Newtype MetricStatistic _


newtype MetricUnit = MetricUnit String
derive instance newtypeMetricUnit :: Newtype MetricUnit _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>The specified object could not be found.</p>
newtype ObjectNotFoundException = ObjectNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeObjectNotFoundException :: Newtype ObjectNotFoundException _


newtype PolicyName = PolicyName String
derive instance newtypePolicyName :: Newtype PolicyName _


newtype PolicyType = PolicyType String
derive instance newtypePolicyType :: Newtype PolicyType _


-- | <p>Represents a predefined metric for a target tracking policy.</p>
newtype PredefinedScalingMetricSpecification = PredefinedScalingMetricSpecification 
  { "PredefinedScalingMetricType" :: (ScalingMetricType)
  , "ResourceLabel" :: NullOrUndefined (ResourceLabel)
  }
derive instance newtypePredefinedScalingMetricSpecification :: Newtype PredefinedScalingMetricSpecification _


newtype ResourceCapacity = ResourceCapacity Int
derive instance newtypeResourceCapacity :: Newtype ResourceCapacity _


newtype ResourceIdMaxLen1600 = ResourceIdMaxLen1600 String
derive instance newtypeResourceIdMaxLen1600 :: Newtype ResourceIdMaxLen1600 _


newtype ResourceLabel = ResourceLabel String
derive instance newtypeResourceLabel :: Newtype ResourceLabel _


newtype ScalableDimension = ScalableDimension String
derive instance newtypeScalableDimension :: Newtype ScalableDimension _


-- | <p>Specifies the scaling configuration for a scalable resource.</p>
newtype ScalingInstruction = ScalingInstruction 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  , "MinCapacity" :: (ResourceCapacity)
  , "MaxCapacity" :: (ResourceCapacity)
  , "TargetTrackingConfigurations" :: (TargetTrackingConfigurations)
  }
derive instance newtypeScalingInstruction :: Newtype ScalingInstruction _


newtype ScalingInstructions = ScalingInstructions (Array ScalingInstruction)
derive instance newtypeScalingInstructions :: Newtype ScalingInstructions _


newtype ScalingMetricType = ScalingMetricType String
derive instance newtypeScalingMetricType :: Newtype ScalingMetricType _


-- | <p>Represents a scaling plan.</p>
newtype ScalingPlan = ScalingPlan 
  { "ScalingPlanName" :: (ScalingPlanName)
  , "ScalingPlanVersion" :: (ScalingPlanVersion)
  , "ApplicationSource" :: (ApplicationSource)
  , "ScalingInstructions" :: (ScalingInstructions)
  , "StatusCode" :: (ScalingPlanStatusCode)
  , "StatusMessage" :: NullOrUndefined (XmlString)
  , "CreationTime" :: NullOrUndefined (TimestampType)
  }
derive instance newtypeScalingPlan :: Newtype ScalingPlan _


newtype ScalingPlanName = ScalingPlanName String
derive instance newtypeScalingPlanName :: Newtype ScalingPlanName _


newtype ScalingPlanNames = ScalingPlanNames (Array ScalingPlanName)
derive instance newtypeScalingPlanNames :: Newtype ScalingPlanNames _


-- | <p>Represents a scalable resource.</p>
newtype ScalingPlanResource = ScalingPlanResource 
  { "ScalingPlanName" :: (ScalingPlanName)
  , "ScalingPlanVersion" :: (ScalingPlanVersion)
  , "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  , "ScalingPolicies" :: NullOrUndefined (ScalingPolicies)
  , "ScalingStatusCode" :: (ScalingStatusCode)
  , "ScalingStatusMessage" :: NullOrUndefined (XmlString)
  }
derive instance newtypeScalingPlanResource :: Newtype ScalingPlanResource _


newtype ScalingPlanResources = ScalingPlanResources (Array ScalingPlanResource)
derive instance newtypeScalingPlanResources :: Newtype ScalingPlanResources _


newtype ScalingPlanStatusCode = ScalingPlanStatusCode String
derive instance newtypeScalingPlanStatusCode :: Newtype ScalingPlanStatusCode _


newtype ScalingPlanVersion = ScalingPlanVersion Number
derive instance newtypeScalingPlanVersion :: Newtype ScalingPlanVersion _


newtype ScalingPlans = ScalingPlans (Array ScalingPlan)
derive instance newtypeScalingPlans :: Newtype ScalingPlans _


newtype ScalingPolicies = ScalingPolicies (Array ScalingPolicy)
derive instance newtypeScalingPolicies :: Newtype ScalingPolicies _


-- | <p>Represents a scaling policy.</p>
newtype ScalingPolicy = ScalingPolicy 
  { "PolicyName" :: (PolicyName)
  , "PolicyType" :: (PolicyType)
  , "TargetTrackingConfiguration" :: NullOrUndefined (TargetTrackingConfiguration)
  }
derive instance newtypeScalingPolicy :: Newtype ScalingPolicy _


newtype ScalingStatusCode = ScalingStatusCode String
derive instance newtypeScalingStatusCode :: Newtype ScalingStatusCode _


newtype ServiceNamespace = ServiceNamespace String
derive instance newtypeServiceNamespace :: Newtype ServiceNamespace _


-- | <p>Represents a target tracking scaling policy.</p>
newtype TargetTrackingConfiguration = TargetTrackingConfiguration 
  { "PredefinedScalingMetricSpecification" :: NullOrUndefined (PredefinedScalingMetricSpecification)
  , "CustomizedScalingMetricSpecification" :: NullOrUndefined (CustomizedScalingMetricSpecification)
  , "TargetValue" :: (MetricScale)
  , "DisableScaleIn" :: NullOrUndefined (DisableScaleIn)
  , "ScaleOutCooldown" :: NullOrUndefined (Cooldown)
  , "ScaleInCooldown" :: NullOrUndefined (Cooldown)
  , "EstimatedInstanceWarmup" :: NullOrUndefined (Cooldown)
  }
derive instance newtypeTargetTrackingConfiguration :: Newtype TargetTrackingConfiguration _


newtype TargetTrackingConfigurations = TargetTrackingConfigurations (Array TargetTrackingConfiguration)
derive instance newtypeTargetTrackingConfigurations :: Newtype TargetTrackingConfigurations _


newtype TimestampType = TimestampType Number
derive instance newtypeTimestampType :: Newtype TimestampType _


-- | <p>An exception was thrown for a validation issue. Review the parameters provided.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeValidationException :: Newtype ValidationException _


newtype XmlString = XmlString String
derive instance newtypeXmlString :: Newtype XmlString _
