

-- | <p>With Application Auto Scaling, you can automatically scale your AWS resources. The experience is similar to that of <a href="https://aws.amazon.com/autoscaling/">Auto Scaling</a>. You can use Application Auto Scaling to accomplish the following tasks:</p> <ul> <li> <p>Define scaling policies to automatically scale your AWS resources</p> </li> <li> <p>Scale your resources in response to CloudWatch alarms</p> </li> <li> <p>Schedule one-time or recurring scaling actions</p> </li> <li> <p>View the history of your scaling events</p> </li> </ul> <p>Application Auto Scaling can scale the following AWS resources:</p> <ul> <li> <p>Amazon ECS services. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-auto-scaling.html">Service Auto Scaling</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </li> <li> <p>Amazon EC2 Spot fleets. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/fleet-auto-scaling.html">Automatic Scaling for Spot Fleet</a> in the <i>Amazon EC2 User Guide</i>.</p> </li> <li> <p>Amazon EMR clusters. For more information, see <a href="http://docs.aws.amazon.com/ElasticMapReduce/latest/ManagementGuide/emr-automatic-scaling.html">Using Automatic Scaling in Amazon EMR</a> in the <i>Amazon EMR Management Guide</i>.</p> </li> <li> <p>AppStream 2.0 fleets. For more information, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/autoscaling.html">Fleet Auto Scaling for Amazon AppStream 2.0</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p> </li> <li> <p>Provisioned read and write capacity for Amazon DynamoDB tables and global secondary indexes. For more information, see <a href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/AutoScaling.html">Managing Throughput Capacity Automatically with DynamoDB Auto Scaling</a> in the <i>Amazon DynamoDB Developer Guide</i>.</p> </li> <li> <p>Amazon Aurora Replicas. For more information, see <a href="http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Integrating.AutoScaling.html">Using Amazon Aurora Auto Scaling with Aurora Replicas</a>.</p> </li> </ul> <p>For a list of supported regions, see <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#as-app_region">AWS Regions and Endpoints: Application Auto Scaling</a> in the <i>AWS General Reference</i>.</p>
module AWS.AutoScaling where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AutoScaling" :: String


-- | <p>Deletes the specified Application Auto Scaling scaling policy.</p> <p>Deleting a policy deletes the underlying alarm action, but does not delete the CloudWatch alarm associated with the scaling policy, even if it no longer has an associated action.</p> <p>To create a scaling policy or update an existing one, see <a>PutScalingPolicy</a>.</p>
deleteScalingPolicy :: forall eff. DeleteScalingPolicyRequest -> Aff (err :: AWS.RequestError | eff) DeleteScalingPolicyResponse
deleteScalingPolicy = AWS.request serviceName "DeleteScalingPolicy" 


-- | <p>Deletes the specified Application Auto Scaling scheduled action.</p>
deleteScheduledAction :: forall eff. DeleteScheduledActionRequest -> Aff (err :: AWS.RequestError | eff) DeleteScheduledActionResponse
deleteScheduledAction = AWS.request serviceName "DeleteScheduledAction" 


-- | <p>Deregisters a scalable target.</p> <p>Deregistering a scalable target deletes the scaling policies that are associated with it.</p> <p>To create a scalable target or update an existing one, see <a>RegisterScalableTarget</a>.</p>
deregisterScalableTarget :: forall eff. DeregisterScalableTargetRequest -> Aff (err :: AWS.RequestError | eff) DeregisterScalableTargetResponse
deregisterScalableTarget = AWS.request serviceName "DeregisterScalableTarget" 


-- | <p>Gets information about the scalable targets in the specified namespace.</p> <p>You can filter the results using the <code>ResourceIds</code> and <code>ScalableDimension</code> parameters.</p> <p>To create a scalable target or update an existing one, see <a>RegisterScalableTarget</a>. If you are no longer using a scalable target, you can deregister it using <a>DeregisterScalableTarget</a>.</p>
describeScalableTargets :: forall eff. DescribeScalableTargetsRequest -> Aff (err :: AWS.RequestError | eff) DescribeScalableTargetsResponse
describeScalableTargets = AWS.request serviceName "DescribeScalableTargets" 


-- | <p>Provides descriptive information about the scaling activities in the specified namespace from the previous six weeks.</p> <p>You can filter the results using the <code>ResourceId</code> and <code>ScalableDimension</code> parameters.</p> <p>Scaling activities are triggered by CloudWatch alarms that are associated with scaling policies. To view the scaling policies for a service namespace, see <a>DescribeScalingPolicies</a>. To create a scaling policy or update an existing one, see <a>PutScalingPolicy</a>.</p>
describeScalingActivities :: forall eff. DescribeScalingActivitiesRequest -> Aff (err :: AWS.RequestError | eff) DescribeScalingActivitiesResponse
describeScalingActivities = AWS.request serviceName "DescribeScalingActivities" 


-- | <p>Describes the scaling policies for the specified service namespace.</p> <p>You can filter the results using the <code>ResourceId</code>, <code>ScalableDimension</code>, and <code>PolicyNames</code> parameters.</p> <p>To create a scaling policy or update an existing one, see <a>PutScalingPolicy</a>. If you are no longer using a scaling policy, you can delete it using <a>DeleteScalingPolicy</a>.</p>
describeScalingPolicies :: forall eff. DescribeScalingPoliciesRequest -> Aff (err :: AWS.RequestError | eff) DescribeScalingPoliciesResponse
describeScalingPolicies = AWS.request serviceName "DescribeScalingPolicies" 


-- | <p>Describes the scheduled actions for the specified service namespace.</p> <p>You can filter the results using the <code>ResourceId</code>, <code>ScalableDimension</code>, and <code>ScheduledActionNames</code> parameters.</p> <p>To create a scheduled action or update an existing one, see <a>PutScheduledAction</a>. If you are no longer using a scheduled action, you can delete it using <a>DeleteScheduledAction</a>.</p>
describeScheduledActions :: forall eff. DescribeScheduledActionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeScheduledActionsResponse
describeScheduledActions = AWS.request serviceName "DescribeScheduledActions" 


-- | <p>Creates or updates a policy for an Application Auto Scaling scalable target.</p> <p>Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scaling policy applies to the scalable target identified by those three attributes. You cannot create a scaling policy until you register the scalable target using <a>RegisterScalableTarget</a>.</p> <p>To update a policy, specify its policy name and the parameters that you want to change. Any parameters that you don't specify are not changed by this update request.</p> <p>You can view the scaling policies for a service namespace using <a>DescribeScalingPolicies</a>. If you are no longer using a scaling policy, you can delete it using <a>DeleteScalingPolicy</a>.</p>
putScalingPolicy :: forall eff. PutScalingPolicyRequest -> Aff (err :: AWS.RequestError | eff) PutScalingPolicyResponse
putScalingPolicy = AWS.request serviceName "PutScalingPolicy" 


-- | <p>Creates or updates a scheduled action for an Application Auto Scaling scalable target.</p> <p>Each scalable target is identified by a service namespace, resource ID, and scalable dimension. A scheduled action applies to the scalable target identified by those three attributes. You cannot create a scheduled action until you register the scalable target using <a>RegisterScalableTarget</a>.</p> <p>To update an action, specify its name and the parameters that you want to change. If you don't specify start and end times, the old values are deleted. Any other parameters that you don't specify are not changed by this update request.</p> <p>You can view the scheduled actions using <a>DescribeScheduledActions</a>. If you are no longer using a scheduled action, you can delete it using <a>DeleteScheduledAction</a>.</p>
putScheduledAction :: forall eff. PutScheduledActionRequest -> Aff (err :: AWS.RequestError | eff) PutScheduledActionResponse
putScheduledAction = AWS.request serviceName "PutScheduledAction" 


-- | <p>Registers or updates a scalable target. A scalable target is a resource that Application Auto Scaling can scale out or scale in. After you have registered a scalable target, you can use this operation to update the minimum and maximum values for its scalable dimension.</p> <p>After you register a scalable target, you can create and apply scaling policies using <a>PutScalingPolicy</a>. You can view the scaling policies for a service namespace using <a>DescribeScalableTargets</a>. If you no longer need a scalable target, you can deregister it using <a>DeregisterScalableTarget</a>.</p>
registerScalableTarget :: forall eff. RegisterScalableTargetRequest -> Aff (err :: AWS.RequestError | eff) RegisterScalableTargetResponse
registerScalableTarget = AWS.request serviceName "RegisterScalableTarget" 


newtype AdjustmentType = AdjustmentType String


-- | <p>Represents a CloudWatch alarm associated with a scaling policy.</p>
newtype Alarm = Alarm 
  { "AlarmName" :: (ResourceId)
  , "AlarmARN" :: (ResourceId)
  }


newtype Alarms = Alarms (Array Alarm)


-- | <p>Concurrent updates caused an exception, for example, if you request an update to an Application Auto Scaling resource that already has a pending update.</p>
newtype ConcurrentUpdateException = ConcurrentUpdateException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype Cooldown = Cooldown Int


-- | <p>Configures a customized metric for a target tracking policy.</p>
newtype CustomizedMetricSpecification = CustomizedMetricSpecification 
  { "MetricName" :: (MetricName)
  , "Namespace" :: (MetricNamespace)
  , "Dimensions" :: NullOrUndefined (MetricDimensions)
  , "Statistic" :: (MetricStatistic)
  , "Unit''" :: NullOrUndefined (MetricUnit)
  }


newtype DeleteScalingPolicyRequest = DeleteScalingPolicyRequest 
  { "PolicyName" :: (ResourceIdMaxLen1600)
  , "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  }


newtype DeleteScalingPolicyResponse = DeleteScalingPolicyResponse 
  { 
  }


newtype DeleteScheduledActionRequest = DeleteScheduledActionRequest 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "ScheduledActionName" :: (ResourceIdMaxLen1600)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: NullOrUndefined (ScalableDimension)
  }


newtype DeleteScheduledActionResponse = DeleteScheduledActionResponse 
  { 
  }


newtype DeregisterScalableTargetRequest = DeregisterScalableTargetRequest 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  }


newtype DeregisterScalableTargetResponse = DeregisterScalableTargetResponse 
  { 
  }


newtype DescribeScalableTargetsRequest = DescribeScalableTargetsRequest 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceIds" :: NullOrUndefined (ResourceIdsMaxLen1600)
  , "ScalableDimension" :: NullOrUndefined (ScalableDimension)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (XmlString)
  }


newtype DescribeScalableTargetsResponse = DescribeScalableTargetsResponse 
  { "ScalableTargets" :: NullOrUndefined (ScalableTargets)
  , "NextToken" :: NullOrUndefined (XmlString)
  }


newtype DescribeScalingActivitiesRequest = DescribeScalingActivitiesRequest 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: NullOrUndefined (ResourceIdMaxLen1600)
  , "ScalableDimension" :: NullOrUndefined (ScalableDimension)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (XmlString)
  }


newtype DescribeScalingActivitiesResponse = DescribeScalingActivitiesResponse 
  { "ScalingActivities" :: NullOrUndefined (ScalingActivities)
  , "NextToken" :: NullOrUndefined (XmlString)
  }


newtype DescribeScalingPoliciesRequest = DescribeScalingPoliciesRequest 
  { "PolicyNames" :: NullOrUndefined (ResourceIdsMaxLen1600)
  , "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: NullOrUndefined (ResourceIdMaxLen1600)
  , "ScalableDimension" :: NullOrUndefined (ScalableDimension)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (XmlString)
  }


newtype DescribeScalingPoliciesResponse = DescribeScalingPoliciesResponse 
  { "ScalingPolicies" :: NullOrUndefined (ScalingPolicies)
  , "NextToken" :: NullOrUndefined (XmlString)
  }


newtype DescribeScheduledActionsRequest = DescribeScheduledActionsRequest 
  { "ScheduledActionNames" :: NullOrUndefined (ResourceIdsMaxLen1600)
  , "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: NullOrUndefined (ResourceIdMaxLen1600)
  , "ScalableDimension" :: NullOrUndefined (ScalableDimension)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (XmlString)
  }


newtype DescribeScheduledActionsResponse = DescribeScheduledActionsResponse 
  { "ScheduledActions" :: NullOrUndefined (ScheduledActions)
  , "NextToken" :: NullOrUndefined (XmlString)
  }


newtype DisableScaleIn = DisableScaleIn Boolean


newtype ErrorMessage = ErrorMessage String


-- | <p>Failed access to resources caused an exception. This exception is thrown when Application Auto Scaling is unable to retrieve the alarms associated with a scaling policy due to a client error, for example, if the role ARN specified for a scalable target does not have permission to call the CloudWatch <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DescribeAlarms.html">DescribeAlarms</a> on your behalf.</p>
newtype FailedResourceAccessException = FailedResourceAccessException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The service encountered an internal error.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The next token supplied was invalid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A per-account resource limit is exceeded. For more information, see <a href="http://docs.aws.amazon.com/ApplicationAutoScaling/latest/userguide/application-auto-scaling-limits.html">Application Auto Scaling Limits</a>.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype MaxResults = MaxResults Int


newtype MetricAggregationType = MetricAggregationType String


-- | <p>Describes the dimension of a metric.</p>
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


newtype MetricType = MetricType String


newtype MetricUnit = MetricUnit String


newtype MinAdjustmentMagnitude = MinAdjustmentMagnitude Int


-- | <p>The specified object could not be found. For any operation that depends on the existence of a scalable target, this exception is thrown if the scalable target with the specified service namespace, resource ID, and scalable dimension does not exist. For any operation that deletes or deregisters a resource, this exception is thrown if the resource cannot be found.</p>
newtype ObjectNotFoundException = ObjectNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype PolicyName = PolicyName String


newtype PolicyType = PolicyType String


-- | <p>Configures a predefined metric for a target tracking policy.</p>
newtype PredefinedMetricSpecification = PredefinedMetricSpecification 
  { "PredefinedMetricType" :: (MetricType)
  , "ResourceLabel" :: NullOrUndefined (ResourceLabel)
  }


newtype PutScalingPolicyRequest = PutScalingPolicyRequest 
  { "PolicyName" :: (PolicyName)
  , "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  , "PolicyType" :: NullOrUndefined (PolicyType)
  , "StepScalingPolicyConfiguration" :: NullOrUndefined (StepScalingPolicyConfiguration)
  , "TargetTrackingScalingPolicyConfiguration" :: NullOrUndefined (TargetTrackingScalingPolicyConfiguration)
  }


newtype PutScalingPolicyResponse = PutScalingPolicyResponse 
  { "PolicyARN" :: (ResourceIdMaxLen1600)
  , "Alarms" :: NullOrUndefined (Alarms)
  }


newtype PutScheduledActionRequest = PutScheduledActionRequest 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "Schedule" :: NullOrUndefined (ResourceIdMaxLen1600)
  , "ScheduledActionName" :: (ScheduledActionName)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: NullOrUndefined (ScalableDimension)
  , "StartTime" :: NullOrUndefined (TimestampType)
  , "EndTime" :: NullOrUndefined (TimestampType)
  , "ScalableTargetAction" :: NullOrUndefined (ScalableTargetAction)
  }


newtype PutScheduledActionResponse = PutScheduledActionResponse 
  { 
  }


newtype RegisterScalableTargetRequest = RegisterScalableTargetRequest 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  , "MinCapacity" :: NullOrUndefined (ResourceCapacity)
  , "MaxCapacity" :: NullOrUndefined (ResourceCapacity)
  , "RoleARN" :: NullOrUndefined (ResourceIdMaxLen1600)
  }


newtype RegisterScalableTargetResponse = RegisterScalableTargetResponse 
  { 
  }


newtype ResourceCapacity = ResourceCapacity Int


newtype ResourceId = ResourceId String


newtype ResourceIdMaxLen1600 = ResourceIdMaxLen1600 String


newtype ResourceIdsMaxLen1600 = ResourceIdsMaxLen1600 (Array ResourceIdMaxLen1600)


newtype ResourceLabel = ResourceLabel String


newtype ScalableDimension = ScalableDimension String


-- | <p>Represents a scalable target.</p>
newtype ScalableTarget = ScalableTarget 
  { "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  , "MinCapacity" :: (ResourceCapacity)
  , "MaxCapacity" :: (ResourceCapacity)
  , "RoleARN" :: (ResourceIdMaxLen1600)
  , "CreationTime" :: (TimestampType)
  }


-- | <p>Represents the minimum and maximum capacity for a scheduled action.</p>
newtype ScalableTargetAction = ScalableTargetAction 
  { "MinCapacity" :: NullOrUndefined (ResourceCapacity)
  , "MaxCapacity" :: NullOrUndefined (ResourceCapacity)
  }


newtype ScalableTargets = ScalableTargets (Array ScalableTarget)


newtype ScalingActivities = ScalingActivities (Array ScalingActivity)


-- | <p>Represents a scaling activity.</p>
newtype ScalingActivity = ScalingActivity 
  { "ActivityId" :: (ResourceId)
  , "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  , "Description" :: (XmlString)
  , "Cause" :: (XmlString)
  , "StartTime" :: (TimestampType)
  , "EndTime" :: NullOrUndefined (TimestampType)
  , "StatusCode" :: (ScalingActivityStatusCode)
  , "StatusMessage" :: NullOrUndefined (XmlString)
  , "Details" :: NullOrUndefined (XmlString)
  }


newtype ScalingActivityStatusCode = ScalingActivityStatusCode String


newtype ScalingAdjustment = ScalingAdjustment Int


newtype ScalingPolicies = ScalingPolicies (Array ScalingPolicy)


-- | <p>Represents a scaling policy.</p>
newtype ScalingPolicy = ScalingPolicy 
  { "PolicyARN" :: (ResourceIdMaxLen1600)
  , "PolicyName" :: (PolicyName)
  , "ServiceNamespace" :: (ServiceNamespace)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: (ScalableDimension)
  , "PolicyType" :: (PolicyType)
  , "StepScalingPolicyConfiguration" :: NullOrUndefined (StepScalingPolicyConfiguration)
  , "TargetTrackingScalingPolicyConfiguration" :: NullOrUndefined (TargetTrackingScalingPolicyConfiguration)
  , "Alarms" :: NullOrUndefined (Alarms)
  , "CreationTime" :: (TimestampType)
  }


-- | <p>Represents a scheduled action.</p>
newtype ScheduledAction = ScheduledAction 
  { "ScheduledActionName" :: (ScheduledActionName)
  , "ScheduledActionARN" :: (ResourceIdMaxLen1600)
  , "ServiceNamespace" :: (ServiceNamespace)
  , "Schedule" :: (ResourceIdMaxLen1600)
  , "ResourceId" :: (ResourceIdMaxLen1600)
  , "ScalableDimension" :: NullOrUndefined (ScalableDimension)
  , "StartTime" :: NullOrUndefined (TimestampType)
  , "EndTime" :: NullOrUndefined (TimestampType)
  , "ScalableTargetAction" :: NullOrUndefined (ScalableTargetAction)
  , "CreationTime" :: (TimestampType)
  }


newtype ScheduledActionName = ScheduledActionName String


newtype ScheduledActions = ScheduledActions (Array ScheduledAction)


newtype ServiceNamespace = ServiceNamespace String


-- | <p>Represents a step adjustment for a <a>StepScalingPolicyConfiguration</a>. Describes an adjustment based on the difference between the value of the aggregated CloudWatch metric and the breach threshold that you've defined for the alarm. </p> <p>For the following examples, suppose that you have an alarm with a breach threshold of 50:</p> <ul> <li> <p>To trigger the adjustment when the metric is greater than or equal to 50 and less than 60, specify a lower bound of 0 and an upper bound of 10.</p> </li> <li> <p>To trigger the adjustment when the metric is greater than 40 and less than or equal to 50, specify a lower bound of -10 and an upper bound of 0.</p> </li> </ul> <p>There are a few rules for the step adjustments for your step policy:</p> <ul> <li> <p>The ranges of your step adjustments can't overlap or have a gap.</p> </li> <li> <p>At most one step adjustment can have a null lower bound. If one step adjustment has a negative lower bound, then there must be a step adjustment with a null lower bound.</p> </li> <li> <p>At most one step adjustment can have a null upper bound. If one step adjustment has a positive upper bound, then there must be a step adjustment with a null upper bound.</p> </li> <li> <p>The upper and lower bound can't be null in the same step adjustment.</p> </li> </ul>
newtype StepAdjustment = StepAdjustment 
  { "MetricIntervalLowerBound" :: NullOrUndefined (MetricScale)
  , "MetricIntervalUpperBound" :: NullOrUndefined (MetricScale)
  , "ScalingAdjustment" :: (ScalingAdjustment)
  }


newtype StepAdjustments = StepAdjustments (Array StepAdjustment)


-- | <p>Represents a step scaling policy configuration.</p>
newtype StepScalingPolicyConfiguration = StepScalingPolicyConfiguration 
  { "AdjustmentType" :: NullOrUndefined (AdjustmentType)
  , "StepAdjustments" :: NullOrUndefined (StepAdjustments)
  , "MinAdjustmentMagnitude" :: NullOrUndefined (MinAdjustmentMagnitude)
  , "Cooldown" :: NullOrUndefined (Cooldown)
  , "MetricAggregationType" :: NullOrUndefined (MetricAggregationType)
  }


-- | <p>Represents a target tracking scaling policy configuration.</p>
newtype TargetTrackingScalingPolicyConfiguration = TargetTrackingScalingPolicyConfiguration 
  { "TargetValue" :: (MetricScale)
  , "PredefinedMetricSpecification" :: NullOrUndefined (PredefinedMetricSpecification)
  , "CustomizedMetricSpecification" :: NullOrUndefined (CustomizedMetricSpecification)
  , "ScaleOutCooldown" :: NullOrUndefined (Cooldown)
  , "ScaleInCooldown" :: NullOrUndefined (Cooldown)
  , "DisableScaleIn" :: NullOrUndefined (DisableScaleIn)
  }


newtype TimestampType = TimestampType Number


-- | <p>An exception was thrown for a validation issue. Review the available parameters for the API request.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype XmlString = XmlString String
