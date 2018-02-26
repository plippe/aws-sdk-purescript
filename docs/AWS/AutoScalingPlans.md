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

#### `ApplicationSources`

``` purescript
newtype ApplicationSources
  = ApplicationSources (Array ApplicationSource)
```

#### `ConcurrentUpdateException`

``` purescript
newtype ConcurrentUpdateException
  = ConcurrentUpdateException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Concurrent updates caused an exception, for example, if you request an update to a scaling plan that already has a pending update.</p>

#### `Cooldown`

``` purescript
newtype Cooldown
  = Cooldown Int
```

#### `CreateScalingPlanRequest`

``` purescript
newtype CreateScalingPlanRequest
  = CreateScalingPlanRequest { "ScalingPlanName" :: ScalingPlanName, "ApplicationSource" :: ApplicationSource, "ScalingInstructions" :: ScalingInstructions }
```

#### `CreateScalingPlanResponse`

``` purescript
newtype CreateScalingPlanResponse
  = CreateScalingPlanResponse { "ScalingPlanVersion" :: ScalingPlanVersion }
```

#### `CustomizedScalingMetricSpecification`

``` purescript
newtype CustomizedScalingMetricSpecification
  = CustomizedScalingMetricSpecification { "MetricName" :: MetricName, "Namespace" :: MetricNamespace, "Dimensions" :: NullOrUndefined (MetricDimensions), "Statistic" :: MetricStatistic, "Unit''" :: NullOrUndefined (MetricUnit) }
```

<p>Represents a customized metric for a target tracking policy.</p>

#### `DeleteScalingPlanRequest`

``` purescript
newtype DeleteScalingPlanRequest
  = DeleteScalingPlanRequest { "ScalingPlanName" :: ScalingPlanName, "ScalingPlanVersion" :: ScalingPlanVersion }
```

#### `DeleteScalingPlanResponse`

``` purescript
newtype DeleteScalingPlanResponse
  = DeleteScalingPlanResponse {  }
```

#### `DescribeScalingPlanResourcesRequest`

``` purescript
newtype DescribeScalingPlanResourcesRequest
  = DescribeScalingPlanResourcesRequest { "ScalingPlanName" :: ScalingPlanName, "ScalingPlanVersion" :: ScalingPlanVersion, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeScalingPlanResourcesResponse`

``` purescript
newtype DescribeScalingPlanResourcesResponse
  = DescribeScalingPlanResourcesResponse { "ScalingPlanResources" :: NullOrUndefined (ScalingPlanResources), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeScalingPlansRequest`

``` purescript
newtype DescribeScalingPlansRequest
  = DescribeScalingPlansRequest { "ScalingPlanNames" :: NullOrUndefined (ScalingPlanNames), "ScalingPlanVersion" :: NullOrUndefined (ScalingPlanVersion), "ApplicationSources" :: NullOrUndefined (ApplicationSources), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeScalingPlansResponse`

``` purescript
newtype DescribeScalingPlansResponse
  = DescribeScalingPlansResponse { "ScalingPlans" :: NullOrUndefined (ScalingPlans), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DisableScaleIn`

``` purescript
newtype DisableScaleIn
  = DisableScaleIn Boolean
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The service encountered an internal error.</p>

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The token provided is not valid.</p>

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Your account exceeded a limit. This exception is thrown when a per-account resource limit is exceeded.</p>

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MetricDimension`

``` purescript
newtype MetricDimension
  = MetricDimension { "Name" :: MetricDimensionName, "Value" :: MetricDimensionValue }
```

<p>Represents a dimension for a customized metric.</p>

#### `MetricDimensionName`

``` purescript
newtype MetricDimensionName
  = MetricDimensionName String
```

#### `MetricDimensionValue`

``` purescript
newtype MetricDimensionValue
  = MetricDimensionValue String
```

#### `MetricDimensions`

``` purescript
newtype MetricDimensions
  = MetricDimensions (Array MetricDimension)
```

#### `MetricName`

``` purescript
newtype MetricName
  = MetricName String
```

#### `MetricNamespace`

``` purescript
newtype MetricNamespace
  = MetricNamespace String
```

#### `MetricScale`

``` purescript
newtype MetricScale
  = MetricScale Number
```

#### `MetricStatistic`

``` purescript
newtype MetricStatistic
  = MetricStatistic String
```

#### `MetricUnit`

``` purescript
newtype MetricUnit
  = MetricUnit String
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `ObjectNotFoundException`

``` purescript
newtype ObjectNotFoundException
  = ObjectNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified object could not be found.</p>

#### `PolicyName`

``` purescript
newtype PolicyName
  = PolicyName String
```

#### `PolicyType`

``` purescript
newtype PolicyType
  = PolicyType String
```

#### `PredefinedScalingMetricSpecification`

``` purescript
newtype PredefinedScalingMetricSpecification
  = PredefinedScalingMetricSpecification { "PredefinedScalingMetricType" :: ScalingMetricType, "ResourceLabel" :: NullOrUndefined (ResourceLabel) }
```

<p>Represents a predefined metric for a target tracking policy.</p>

#### `ResourceCapacity`

``` purescript
newtype ResourceCapacity
  = ResourceCapacity Int
```

#### `ResourceIdMaxLen1600`

``` purescript
newtype ResourceIdMaxLen1600
  = ResourceIdMaxLen1600 String
```

#### `ResourceLabel`

``` purescript
newtype ResourceLabel
  = ResourceLabel String
```

#### `ScalableDimension`

``` purescript
newtype ScalableDimension
  = ScalableDimension String
```

#### `ScalingInstruction`

``` purescript
newtype ScalingInstruction
  = ScalingInstruction { "ServiceNamespace" :: ServiceNamespace, "ResourceId" :: ResourceIdMaxLen1600, "ScalableDimension" :: ScalableDimension, "MinCapacity" :: ResourceCapacity, "MaxCapacity" :: ResourceCapacity, "TargetTrackingConfigurations" :: TargetTrackingConfigurations }
```

<p>Specifies the scaling configuration for a scalable resource.</p>

#### `ScalingInstructions`

``` purescript
newtype ScalingInstructions
  = ScalingInstructions (Array ScalingInstruction)
```

#### `ScalingMetricType`

``` purescript
newtype ScalingMetricType
  = ScalingMetricType String
```

#### `ScalingPlan`

``` purescript
newtype ScalingPlan
  = ScalingPlan { "ScalingPlanName" :: ScalingPlanName, "ScalingPlanVersion" :: ScalingPlanVersion, "ApplicationSource" :: ApplicationSource, "ScalingInstructions" :: ScalingInstructions, "StatusCode" :: ScalingPlanStatusCode, "StatusMessage" :: NullOrUndefined (XmlString), "CreationTime" :: NullOrUndefined (TimestampType) }
```

<p>Represents a scaling plan.</p>

#### `ScalingPlanName`

``` purescript
newtype ScalingPlanName
  = ScalingPlanName String
```

#### `ScalingPlanNames`

``` purescript
newtype ScalingPlanNames
  = ScalingPlanNames (Array ScalingPlanName)
```

#### `ScalingPlanResource`

``` purescript
newtype ScalingPlanResource
  = ScalingPlanResource { "ScalingPlanName" :: ScalingPlanName, "ScalingPlanVersion" :: ScalingPlanVersion, "ServiceNamespace" :: ServiceNamespace, "ResourceId" :: ResourceIdMaxLen1600, "ScalableDimension" :: ScalableDimension, "ScalingPolicies" :: NullOrUndefined (ScalingPolicies), "ScalingStatusCode" :: ScalingStatusCode, "ScalingStatusMessage" :: NullOrUndefined (XmlString) }
```

<p>Represents a scalable resource.</p>

#### `ScalingPlanResources`

``` purescript
newtype ScalingPlanResources
  = ScalingPlanResources (Array ScalingPlanResource)
```

#### `ScalingPlanStatusCode`

``` purescript
newtype ScalingPlanStatusCode
  = ScalingPlanStatusCode String
```

#### `ScalingPlanVersion`

``` purescript
newtype ScalingPlanVersion
  = ScalingPlanVersion Number
```

#### `ScalingPlans`

``` purescript
newtype ScalingPlans
  = ScalingPlans (Array ScalingPlan)
```

#### `ScalingPolicies`

``` purescript
newtype ScalingPolicies
  = ScalingPolicies (Array ScalingPolicy)
```

#### `ScalingPolicy`

``` purescript
newtype ScalingPolicy
  = ScalingPolicy { "PolicyName" :: PolicyName, "PolicyType" :: PolicyType, "TargetTrackingConfiguration" :: NullOrUndefined (TargetTrackingConfiguration) }
```

<p>Represents a scaling policy.</p>

#### `ScalingStatusCode`

``` purescript
newtype ScalingStatusCode
  = ScalingStatusCode String
```

#### `ServiceNamespace`

``` purescript
newtype ServiceNamespace
  = ServiceNamespace String
```

#### `TargetTrackingConfiguration`

``` purescript
newtype TargetTrackingConfiguration
  = TargetTrackingConfiguration { "PredefinedScalingMetricSpecification" :: NullOrUndefined (PredefinedScalingMetricSpecification), "CustomizedScalingMetricSpecification" :: NullOrUndefined (CustomizedScalingMetricSpecification), "TargetValue" :: MetricScale, "DisableScaleIn" :: NullOrUndefined (DisableScaleIn), "ScaleOutCooldown" :: NullOrUndefined (Cooldown), "ScaleInCooldown" :: NullOrUndefined (Cooldown), "EstimatedInstanceWarmup" :: NullOrUndefined (Cooldown) }
```

<p>Represents a target tracking scaling policy.</p>

#### `TargetTrackingConfigurations`

``` purescript
newtype TargetTrackingConfigurations
  = TargetTrackingConfigurations (Array TargetTrackingConfiguration)
```

#### `TimestampType`

``` purescript
newtype TimestampType
  = TimestampType Number
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>An exception was thrown for a validation issue. Review the parameters provided.</p>

#### `XmlString`

``` purescript
newtype XmlString
  = XmlString String
```


