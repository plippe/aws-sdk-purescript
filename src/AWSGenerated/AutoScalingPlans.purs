

-- | <p>Use AWS Auto Scaling to quickly discover all the scalable AWS resources for your application and configure dynamic scaling for your scalable resources.</p> <p>To get started, create a scaling plan with a set of instructions used to configure dynamic scaling for the scalable resources in your application. AWS Auto Scaling creates target tracking scaling policies for the scalable resources in your scaling plan. Target tracking scaling policies adjust the capacity of your scalable resource as required to maintain resource utilization at the target value that you specified.</p>
module AWS.AutoScalingPlans where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AutoScalingPlans" :: String


-- | <p>Creates a scaling plan.</p> <p>A scaling plan contains a set of instructions used to configure dynamic scaling for the scalable resources in your application. AWS Auto Scaling creates target tracking scaling policies based on the scaling instructions in your scaling plan.</p>
createScalingPlan :: forall eff. CreateScalingPlanRequest -> Aff (err :: AWS.RequestError | eff) CreateScalingPlanResponse
createScalingPlan = AWS.request serviceName "CreateScalingPlan" 


-- | <p>Deletes the specified scaling plan.</p>
deleteScalingPlan :: forall eff. DeleteScalingPlanRequest -> Aff (err :: AWS.RequestError | eff) DeleteScalingPlanResponse
deleteScalingPlan = AWS.request serviceName "DeleteScalingPlan" 


-- | <p>Describes the scalable resources in the specified scaling plan.</p>
describeScalingPlanResources :: forall eff. DescribeScalingPlanResourcesRequest -> Aff (err :: AWS.RequestError | eff) DescribeScalingPlanResourcesResponse
describeScalingPlanResources = AWS.request serviceName "DescribeScalingPlanResources" 


-- | <p>Describes the specified scaling plans or all of your scaling plans.</p>
describeScalingPlans :: forall eff. DescribeScalingPlansRequest -> Aff (err :: AWS.RequestError | eff) DescribeScalingPlansResponse
describeScalingPlans = AWS.request serviceName "DescribeScalingPlans" 


-- | <p>Represents an application source.</p>
newtype ApplicationSource = ApplicationSource 
  { "CloudFormationStackARN" :: NullOrUndefined (XmlString)
  }


newtype ApplicationSources = ApplicationSources (Array ApplicationSource)


-- | <p>Concurrent updates caused an exception, for example, if you request an update to a scaling plan that already has a pending update.</p>
newtype ConcurrentUpdateException = ConcurrentUpdateException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype Cooldown = Cooldown Int


newtype CreateScalingPlanRequest = CreateScalingPlanRequest 
  { "ScalingPlanName" :: (ScalingPlanName)
  , "ApplicationSource" :: (ApplicationSource)
  , "ScalingInstructions" :: (ScalingInstructions)
  }


newtype CreateScalingPlanResponse = CreateScalingPlanResponse 
  { "ScalingPlanVersion" :: (ScalingPlanVersion)
  }


-- | <p>Represents a customized metric for a target tracking policy.</p>
newtype CustomizedScalingMetricSpecification = CustomizedScalingMetricSpecification 
  { "MetricName" :: (MetricName)
  , "Namespace" :: (MetricNamespace)
  , "Dimensions" :: NullOrUndefined (MetricDimensions)
  , "Statistic" :: (MetricStatistic)
  , "Unit''" :: NullOrUndefined (MetricUnit)
  }


newtype DeleteScalingPlanRequest = DeleteScalingPlanRequest 
  { "ScalingPlanName" :: (ScalingPlanName)
  , "ScalingPlanVersion" :: (ScalingPlanVersion)
  }


newtype DeleteScalingPlanResponse = DeleteScalingPlanResponse 
  { 
  }


newtype DescribeScalingPlanResourcesRequest = DescribeScalingPlanResourcesRequest 
  { "ScalingPlanName" :: (ScalingPlanName)
  , "ScalingPlanVersion" :: (ScalingPlanVersion)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeScalingPlanResourcesResponse = DescribeScalingPlanResourcesResponse 
  { "ScalingPlanResources" :: NullOrUndefined (ScalingPlanResources)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeScalingPlansRequest = DescribeScalingPlansRequest 
  { "ScalingPlanNames" :: NullOrUndefined (ScalingPlanNames)
  , "ScalingPlanVersion" :: NullOrUndefined (ScalingPlanVersion)
  , "ApplicationSources" :: NullOrUndefined (ApplicationSources)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeScalingPlansResponse = DescribeScalingPlansResponse 
  { "ScalingPlans" :: NullOrUndefined (ScalingPlans)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DisableScaleIn = DisableScaleIn Boolean


newtype ErrorMessage = ErrorMessage String


-- | <p>The service encountered an internal error.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The token provided is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Your account exceeded a limit. This exception is thrown when a per-account resource limit is exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype MaxResults = MaxResults Int


-- | <p>Represents a dimension for a customized metric.</p>
newtype MetricDimension = MetricDimension 
  { "Name" :: (MetricDimensionName)
  , "Value" :: (MetricDimensionValue)
  }


newtype MetricDimensionName = MetricDimensionName String


newtype MetricDimensionValue = MetricDimensionValue String


newtype MetricDimensions = MetricDimensions (Array MetricDimension)


newtype MetricName = MetricName String


newtype MetricNamespace = MetricNamespace String


newtype MetricScale = MetricScale Number


newtype MetricStatistic = MetricStatistic String


newtype MetricUnit = MetricUnit String


newtype NextToken = NextToken String


-- | <p>The specified object could not be found.</p>
newtype ObjectNotFoundException = ObjectNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype PolicyName = PolicyName String


newtype PolicyType = PolicyType String


-- | <p>Represents a predefined metric for a target tracking policy.</p>
newtype PredefinedScalingMetricSpecification = PredefinedScalingMetricSpecification 
  { "PredefinedScalingMetricType" :: (ScalingMetricType)
  , "ResourceLabel" :: NullOrUndefined (ResourceLabel)
  }


newtype ResourceCapacity = ResourceCapacity Int


newtype ResourceIdMaxLen1600 = ResourceIdMaxLen1600 String


newtype ResourceLabel = ResourceLabel String


newtype ScalableDimension = ScalableDimension String


-- | <p>Specifies the scaling configuration for a scalable resource.</p>
newtype ScalingInstruction = ScalingInstruction 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  , "MinCapacity" :: (ResourceCapacity)
  , "MaxCapacity" :: (ResourceCapacity)
  , "TargetTrackingConfigurations" :: (TargetTrackingConfigurations)
  }


newtype ScalingInstructions = ScalingInstructions (Array ScalingInstruction)


newtype ScalingMetricType = ScalingMetricType String


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


newtype ScalingPlanName = ScalingPlanName String


newtype ScalingPlanNames = ScalingPlanNames (Array ScalingPlanName)


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


newtype ScalingPlanResources = ScalingPlanResources (Array ScalingPlanResource)


newtype ScalingPlanStatusCode = ScalingPlanStatusCode String


newtype ScalingPlanVersion = ScalingPlanVersion Number


newtype ScalingPlans = ScalingPlans (Array ScalingPlan)


newtype ScalingPolicies = ScalingPolicies (Array ScalingPolicy)


-- | <p>Represents a scaling policy.</p>
newtype ScalingPolicy = ScalingPolicy 
  { "PolicyName" :: (PolicyName)
  , "PolicyType" :: (PolicyType)
  , "TargetTrackingConfiguration" :: NullOrUndefined (TargetTrackingConfiguration)
  }


newtype ScalingStatusCode = ScalingStatusCode String


newtype ServiceNamespace = ServiceNamespace String


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


newtype TargetTrackingConfigurations = TargetTrackingConfigurations (Array TargetTrackingConfiguration)


newtype TimestampType = TimestampType Number


-- | <p>An exception was thrown for a validation issue. Review the parameters provided.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype XmlString = XmlString String
