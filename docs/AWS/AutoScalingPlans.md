## Module AWS.AutoScalingPlans

<p>Use AWS Auto Scaling to quickly discover all the scalable AWS resources for your application and configure dynamic scaling for your scalable resources.</p> <p>To get started, create a scaling plan with a set of instructions used to configure dynamic scaling for the scalable resources in your application. AWS Auto Scaling creates target tracking scaling policies for the scalable resources in your scaling plan. Target tracking scaling policies adjust the capacity of your scalable resource as required to maintain resource utilization at the target value that you specified.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createScalingPlan`

``` purescript
createScalingPlan :: forall eff. CreateScalingPlanRequest -> Aff (err :: RequestError | eff) CreateScalingPlanResponse
```

<p>Creates a scaling plan.</p> <p>A scaling plan contains a set of instructions used to configure dynamic scaling for the scalable resources in your application. AWS Auto Scaling creates target tracking scaling policies based on the scaling instructions in your scaling plan.</p>

#### `deleteScalingPlan`

``` purescript
deleteScalingPlan :: forall eff. DeleteScalingPlanRequest -> Aff (err :: RequestError | eff) DeleteScalingPlanResponse
```

<p>Deletes the specified scaling plan.</p>

#### `describeScalingPlanResources`

``` purescript
describeScalingPlanResources :: forall eff. DescribeScalingPlanResourcesRequest -> Aff (err :: RequestError | eff) DescribeScalingPlanResourcesResponse
```

<p>Describes the scalable resources in the specified scaling plan.</p>

#### `describeScalingPlans`

``` purescript
describeScalingPlans :: forall eff. DescribeScalingPlansRequest -> Aff (err :: RequestError | eff) DescribeScalingPlansResponse
```

<p>Describes the specified scaling plans or all of your scaling plans.</p>

#### `ApplicationSource`

``` purescript
newtype ApplicationSource
  = ApplicationSource { "CloudFormationStackARN" :: NullOrUndefined (XmlString) }
```

<p>Represents an application source.</p>

##### Instances
``` purescript
Newtype ApplicationSource _
```

#### `ApplicationSources`

``` purescript
newtype ApplicationSources
  = ApplicationSources (Array ApplicationSource)
```

##### Instances
``` purescript
Newtype ApplicationSources _
```

#### `ConcurrentUpdateException`

``` purescript
newtype ConcurrentUpdateException
  = ConcurrentUpdateException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Concurrent updates caused an exception, for example, if you request an update to a scaling plan that already has a pending update.</p>

##### Instances
``` purescript
Newtype ConcurrentUpdateException _
```

#### `Cooldown`

``` purescript
newtype Cooldown
  = Cooldown Int
```

##### Instances
``` purescript
Newtype Cooldown _
```

#### `CreateScalingPlanRequest`

``` purescript
newtype CreateScalingPlanRequest
  = CreateScalingPlanRequest { "ScalingPlanName" :: ScalingPlanName, "ApplicationSource" :: ApplicationSource, "ScalingInstructions" :: ScalingInstructions }
```

##### Instances
``` purescript
Newtype CreateScalingPlanRequest _
```

#### `CreateScalingPlanResponse`

``` purescript
newtype CreateScalingPlanResponse
  = CreateScalingPlanResponse { "ScalingPlanVersion" :: ScalingPlanVersion }
```

##### Instances
``` purescript
Newtype CreateScalingPlanResponse _
```

#### `CustomizedScalingMetricSpecification`

``` purescript
newtype CustomizedScalingMetricSpecification
  = CustomizedScalingMetricSpecification { "MetricName" :: MetricName, "Namespace" :: MetricNamespace, "Dimensions" :: NullOrUndefined (MetricDimensions), "Statistic" :: MetricStatistic, "Unit''" :: NullOrUndefined (MetricUnit) }
```

<p>Represents a customized metric for a target tracking policy.</p>

##### Instances
``` purescript
Newtype CustomizedScalingMetricSpecification _
```

#### `DeleteScalingPlanRequest`

``` purescript
newtype DeleteScalingPlanRequest
  = DeleteScalingPlanRequest { "ScalingPlanName" :: ScalingPlanName, "ScalingPlanVersion" :: ScalingPlanVersion }
```

##### Instances
``` purescript
Newtype DeleteScalingPlanRequest _
```

#### `DeleteScalingPlanResponse`

``` purescript
newtype DeleteScalingPlanResponse
  = DeleteScalingPlanResponse {  }
```

##### Instances
``` purescript
Newtype DeleteScalingPlanResponse _
```

#### `DescribeScalingPlanResourcesRequest`

``` purescript
newtype DescribeScalingPlanResourcesRequest
  = DescribeScalingPlanResourcesRequest { "ScalingPlanName" :: ScalingPlanName, "ScalingPlanVersion" :: ScalingPlanVersion, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeScalingPlanResourcesRequest _
```

#### `DescribeScalingPlanResourcesResponse`

``` purescript
newtype DescribeScalingPlanResourcesResponse
  = DescribeScalingPlanResourcesResponse { "ScalingPlanResources" :: NullOrUndefined (ScalingPlanResources), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeScalingPlanResourcesResponse _
```

#### `DescribeScalingPlansRequest`

``` purescript
newtype DescribeScalingPlansRequest
  = DescribeScalingPlansRequest { "ScalingPlanNames" :: NullOrUndefined (ScalingPlanNames), "ScalingPlanVersion" :: NullOrUndefined (ScalingPlanVersion), "ApplicationSources" :: NullOrUndefined (ApplicationSources), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeScalingPlansRequest _
```

#### `DescribeScalingPlansResponse`

``` purescript
newtype DescribeScalingPlansResponse
  = DescribeScalingPlansResponse { "ScalingPlans" :: NullOrUndefined (ScalingPlans), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeScalingPlansResponse _
```

#### `DisableScaleIn`

``` purescript
newtype DisableScaleIn
  = DisableScaleIn Boolean
```

##### Instances
``` purescript
Newtype DisableScaleIn _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The service encountered an internal error.</p>

##### Instances
``` purescript
Newtype InternalServiceException _
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The token provided is not valid.</p>

##### Instances
``` purescript
Newtype InvalidNextTokenException _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Your account exceeded a limit. This exception is thrown when a per-account resource limit is exceeded.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
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

#### `MetricDimension`

``` purescript
newtype MetricDimension
  = MetricDimension { "Name" :: MetricDimensionName, "Value" :: MetricDimensionValue }
```

<p>Represents a dimension for a customized metric.</p>

##### Instances
``` purescript
Newtype MetricDimension _
```

#### `MetricDimensionName`

``` purescript
newtype MetricDimensionName
  = MetricDimensionName String
```

##### Instances
``` purescript
Newtype MetricDimensionName _
```

#### `MetricDimensionValue`

``` purescript
newtype MetricDimensionValue
  = MetricDimensionValue String
```

##### Instances
``` purescript
Newtype MetricDimensionValue _
```

#### `MetricDimensions`

``` purescript
newtype MetricDimensions
  = MetricDimensions (Array MetricDimension)
```

##### Instances
``` purescript
Newtype MetricDimensions _
```

#### `MetricName`

``` purescript
newtype MetricName
  = MetricName String
```

##### Instances
``` purescript
Newtype MetricName _
```

#### `MetricNamespace`

``` purescript
newtype MetricNamespace
  = MetricNamespace String
```

##### Instances
``` purescript
Newtype MetricNamespace _
```

#### `MetricScale`

``` purescript
newtype MetricScale
  = MetricScale Number
```

##### Instances
``` purescript
Newtype MetricScale _
```

#### `MetricStatistic`

``` purescript
newtype MetricStatistic
  = MetricStatistic String
```

##### Instances
``` purescript
Newtype MetricStatistic _
```

#### `MetricUnit`

``` purescript
newtype MetricUnit
  = MetricUnit String
```

##### Instances
``` purescript
Newtype MetricUnit _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `ObjectNotFoundException`

``` purescript
newtype ObjectNotFoundException
  = ObjectNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified object could not be found.</p>

##### Instances
``` purescript
Newtype ObjectNotFoundException _
```

#### `PolicyName`

``` purescript
newtype PolicyName
  = PolicyName String
```

##### Instances
``` purescript
Newtype PolicyName _
```

#### `PolicyType`

``` purescript
newtype PolicyType
  = PolicyType String
```

##### Instances
``` purescript
Newtype PolicyType _
```

#### `PredefinedScalingMetricSpecification`

``` purescript
newtype PredefinedScalingMetricSpecification
  = PredefinedScalingMetricSpecification { "PredefinedScalingMetricType" :: ScalingMetricType, "ResourceLabel" :: NullOrUndefined (ResourceLabel) }
```

<p>Represents a predefined metric for a target tracking policy.</p>

##### Instances
``` purescript
Newtype PredefinedScalingMetricSpecification _
```

#### `ResourceCapacity`

``` purescript
newtype ResourceCapacity
  = ResourceCapacity Int
```

##### Instances
``` purescript
Newtype ResourceCapacity _
```

#### `ResourceIdMaxLen1600`

``` purescript
newtype ResourceIdMaxLen1600
  = ResourceIdMaxLen1600 String
```

##### Instances
``` purescript
Newtype ResourceIdMaxLen1600 _
```

#### `ResourceLabel`

``` purescript
newtype ResourceLabel
  = ResourceLabel String
```

##### Instances
``` purescript
Newtype ResourceLabel _
```

#### `ScalableDimension`

``` purescript
newtype ScalableDimension
  = ScalableDimension String
```

##### Instances
``` purescript
Newtype ScalableDimension _
```

#### `ScalingInstruction`

``` purescript
newtype ScalingInstruction
  = ScalingInstruction { "ServiceNamespace" :: ServiceNamespace, "ResourceId" :: ResourceIdMaxLen1600, "ScalableDimension" :: ScalableDimension, "MinCapacity" :: ResourceCapacity, "MaxCapacity" :: ResourceCapacity, "TargetTrackingConfigurations" :: TargetTrackingConfigurations }
```

<p>Specifies the scaling configuration for a scalable resource.</p>

##### Instances
``` purescript
Newtype ScalingInstruction _
```

#### `ScalingInstructions`

``` purescript
newtype ScalingInstructions
  = ScalingInstructions (Array ScalingInstruction)
```

##### Instances
``` purescript
Newtype ScalingInstructions _
```

#### `ScalingMetricType`

``` purescript
newtype ScalingMetricType
  = ScalingMetricType String
```

##### Instances
``` purescript
Newtype ScalingMetricType _
```

#### `ScalingPlan`

``` purescript
newtype ScalingPlan
  = ScalingPlan { "ScalingPlanName" :: ScalingPlanName, "ScalingPlanVersion" :: ScalingPlanVersion, "ApplicationSource" :: ApplicationSource, "ScalingInstructions" :: ScalingInstructions, "StatusCode" :: ScalingPlanStatusCode, "StatusMessage" :: NullOrUndefined (XmlString), "CreationTime" :: NullOrUndefined (TimestampType) }
```

<p>Represents a scaling plan.</p>

##### Instances
``` purescript
Newtype ScalingPlan _
```

#### `ScalingPlanName`

``` purescript
newtype ScalingPlanName
  = ScalingPlanName String
```

##### Instances
``` purescript
Newtype ScalingPlanName _
```

#### `ScalingPlanNames`

``` purescript
newtype ScalingPlanNames
  = ScalingPlanNames (Array ScalingPlanName)
```

##### Instances
``` purescript
Newtype ScalingPlanNames _
```

#### `ScalingPlanResource`

``` purescript
newtype ScalingPlanResource
  = ScalingPlanResource { "ScalingPlanName" :: ScalingPlanName, "ScalingPlanVersion" :: ScalingPlanVersion, "ServiceNamespace" :: ServiceNamespace, "ResourceId" :: ResourceIdMaxLen1600, "ScalableDimension" :: ScalableDimension, "ScalingPolicies" :: NullOrUndefined (ScalingPolicies), "ScalingStatusCode" :: ScalingStatusCode, "ScalingStatusMessage" :: NullOrUndefined (XmlString) }
```

<p>Represents a scalable resource.</p>

##### Instances
``` purescript
Newtype ScalingPlanResource _
```

#### `ScalingPlanResources`

``` purescript
newtype ScalingPlanResources
  = ScalingPlanResources (Array ScalingPlanResource)
```

##### Instances
``` purescript
Newtype ScalingPlanResources _
```

#### `ScalingPlanStatusCode`

``` purescript
newtype ScalingPlanStatusCode
  = ScalingPlanStatusCode String
```

##### Instances
``` purescript
Newtype ScalingPlanStatusCode _
```

#### `ScalingPlanVersion`

``` purescript
newtype ScalingPlanVersion
  = ScalingPlanVersion Number
```

##### Instances
``` purescript
Newtype ScalingPlanVersion _
```

#### `ScalingPlans`

``` purescript
newtype ScalingPlans
  = ScalingPlans (Array ScalingPlan)
```

##### Instances
``` purescript
Newtype ScalingPlans _
```

#### `ScalingPolicies`

``` purescript
newtype ScalingPolicies
  = ScalingPolicies (Array ScalingPolicy)
```

##### Instances
``` purescript
Newtype ScalingPolicies _
```

#### `ScalingPolicy`

``` purescript
newtype ScalingPolicy
  = ScalingPolicy { "PolicyName" :: PolicyName, "PolicyType" :: PolicyType, "TargetTrackingConfiguration" :: NullOrUndefined (TargetTrackingConfiguration) }
```

<p>Represents a scaling policy.</p>

##### Instances
``` purescript
Newtype ScalingPolicy _
```

#### `ScalingStatusCode`

``` purescript
newtype ScalingStatusCode
  = ScalingStatusCode String
```

##### Instances
``` purescript
Newtype ScalingStatusCode _
```

#### `ServiceNamespace`

``` purescript
newtype ServiceNamespace
  = ServiceNamespace String
```

##### Instances
``` purescript
Newtype ServiceNamespace _
```

#### `TargetTrackingConfiguration`

``` purescript
newtype TargetTrackingConfiguration
  = TargetTrackingConfiguration { "PredefinedScalingMetricSpecification" :: NullOrUndefined (PredefinedScalingMetricSpecification), "CustomizedScalingMetricSpecification" :: NullOrUndefined (CustomizedScalingMetricSpecification), "TargetValue" :: MetricScale, "DisableScaleIn" :: NullOrUndefined (DisableScaleIn), "ScaleOutCooldown" :: NullOrUndefined (Cooldown), "ScaleInCooldown" :: NullOrUndefined (Cooldown), "EstimatedInstanceWarmup" :: NullOrUndefined (Cooldown) }
```

<p>Represents a target tracking scaling policy.</p>

##### Instances
``` purescript
Newtype TargetTrackingConfiguration _
```

#### `TargetTrackingConfigurations`

``` purescript
newtype TargetTrackingConfigurations
  = TargetTrackingConfigurations (Array TargetTrackingConfiguration)
```

##### Instances
``` purescript
Newtype TargetTrackingConfigurations _
```

#### `TimestampType`

``` purescript
newtype TimestampType
  = TimestampType Number
```

##### Instances
``` purescript
Newtype TimestampType _
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>An exception was thrown for a validation issue. Review the parameters provided.</p>

##### Instances
``` purescript
Newtype ValidationException _
```

#### `XmlString`

``` purescript
newtype XmlString
  = XmlString String
```

##### Instances
``` purescript
Newtype XmlString _
```


