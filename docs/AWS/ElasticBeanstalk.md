## Module AWS.ElasticBeanstalk

<fullname>AWS Elastic Beanstalk</fullname> <p>AWS Elastic Beanstalk makes it easy for you to create, deploy, and manage scalable, fault-tolerant applications running on the Amazon Web Services cloud.</p> <p>For more information about this product, go to the <a href="http://aws.amazon.com/elasticbeanstalk/">AWS Elastic Beanstalk</a> details page. The location of the latest AWS Elastic Beanstalk WSDL is <a href="http://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl">http://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl</a>. To install the Software Development Kits (SDKs), Integrated Development Environment (IDE) Toolkits, and command line tools that enable you to access the API, go to <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> <p> <b>Endpoints</b> </p> <p>For a list of region-specific endpoints that AWS Elastic Beanstalk supports, go to <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticbeanstalk_region">Regions and Endpoints</a> in the <i>Amazon Web Services Glossary</i>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `abortEnvironmentUpdate`

``` purescript
abortEnvironmentUpdate :: forall eff. AbortEnvironmentUpdateMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Cancels in-progress environment configuration update or application version deployment.</p>

#### `applyEnvironmentManagedAction`

``` purescript
applyEnvironmentManagedAction :: forall eff. ApplyEnvironmentManagedActionRequest -> Aff (err :: RequestError | eff) ApplyEnvironmentManagedActionResult
```

<p>Applies a scheduled managed action immediately. A managed action can be applied only if its status is <code>Scheduled</code>. Get the status and action ID of a managed action with <a>DescribeEnvironmentManagedActions</a>.</p>

#### `checkDNSAvailability`

``` purescript
checkDNSAvailability :: forall eff. CheckDNSAvailabilityMessage -> Aff (err :: RequestError | eff) CheckDNSAvailabilityResultMessage
```

<p>Checks if the specified CNAME is available.</p>

#### `composeEnvironments`

``` purescript
composeEnvironments :: forall eff. ComposeEnvironmentsMessage -> Aff (err :: RequestError | eff) EnvironmentDescriptionsMessage
```

<p>Create or update a group of environments that each run a separate component of a single application. Takes a list of version labels that specify application source bundles for each of the environments to create or update. The name of each environment and other required information must be included in the source bundles in an environment manifest named <code>env.yaml</code>. See <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-mgmt-compose.html">Compose Environments</a> for details.</p>

#### `createApplication`

``` purescript
createApplication :: forall eff. CreateApplicationMessage -> Aff (err :: RequestError | eff) ApplicationDescriptionMessage
```

<p> Creates an application that has one configuration template named <code>default</code> and no application versions. </p>

#### `createApplicationVersion`

``` purescript
createApplicationVersion :: forall eff. CreateApplicationVersionMessage -> Aff (err :: RequestError | eff) ApplicationVersionDescriptionMessage
```

<p>Creates an application version for the specified application. You can create an application version from a source bundle in Amazon S3, a commit in AWS CodeCommit, or the output of an AWS CodeBuild build as follows:</p> <p>Specify a commit in an AWS CodeCommit repository with <code>SourceBuildInformation</code>.</p> <p>Specify a build in an AWS CodeBuild with <code>SourceBuildInformation</code> and <code>BuildConfiguration</code>.</p> <p>Specify a source bundle in S3 with <code>SourceBundle</code> </p> <p>Omit both <code>SourceBuildInformation</code> and <code>SourceBundle</code> to use the default sample application.</p> <note> <p>Once you create an application version with a specified Amazon S3 bucket and key location, you cannot change that Amazon S3 location. If you change the Amazon S3 location, you receive an exception when you attempt to launch an environment from the application version.</p> </note>

#### `createConfigurationTemplate`

``` purescript
createConfigurationTemplate :: forall eff. CreateConfigurationTemplateMessage -> Aff (err :: RequestError | eff) ConfigurationSettingsDescription
```

<p>Creates a configuration template. Templates are associated with a specific application and are used to deploy different versions of the application with the same configuration settings.</p> <p>Related Topics</p> <ul> <li> <p> <a>DescribeConfigurationOptions</a> </p> </li> <li> <p> <a>DescribeConfigurationSettings</a> </p> </li> <li> <p> <a>ListAvailableSolutionStacks</a> </p> </li> </ul>

#### `createEnvironment`

``` purescript
createEnvironment :: forall eff. CreateEnvironmentMessage -> Aff (err :: RequestError | eff) EnvironmentDescription
```

<p>Launches an environment for the specified application using the specified configuration.</p>

#### `createPlatformVersion`

``` purescript
createPlatformVersion :: forall eff. CreatePlatformVersionRequest -> Aff (err :: RequestError | eff) CreatePlatformVersionResult
```

<p>Create a new version of your custom platform.</p>

#### `createStorageLocation`

``` purescript
createStorageLocation :: forall eff. Aff (err :: RequestError | eff) CreateStorageLocationResultMessage
```

<p>Creates a bucket in Amazon S3 to store application versions, logs, and other files used by Elastic Beanstalk environments. The Elastic Beanstalk console and EB CLI call this API the first time you create an environment in a region. If the storage location already exists, <code>CreateStorageLocation</code> still returns the bucket name but does not create a new bucket.</p>

#### `deleteApplication`

``` purescript
deleteApplication :: forall eff. DeleteApplicationMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified application along with all associated versions and configurations. The application versions will not be deleted from your Amazon S3 bucket.</p> <note> <p>You cannot delete an application that has a running environment.</p> </note>

#### `deleteApplicationVersion`

``` purescript
deleteApplicationVersion :: forall eff. DeleteApplicationVersionMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified version from the specified application.</p> <note> <p>You cannot delete an application version that is associated with a running environment.</p> </note>

#### `deleteConfigurationTemplate`

``` purescript
deleteConfigurationTemplate :: forall eff. DeleteConfigurationTemplateMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified configuration template.</p> <note> <p>When you launch an environment using a configuration template, the environment gets a copy of the template. You can delete or modify the environment's copy of the template without affecting the running environment.</p> </note>

#### `deleteEnvironmentConfiguration`

``` purescript
deleteEnvironmentConfiguration :: forall eff. DeleteEnvironmentConfigurationMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the draft configuration associated with the running environment.</p> <p>Updating a running environment with any configuration changes creates a draft configuration set. You can get the draft configuration using <a>DescribeConfigurationSettings</a> while the update is in progress or if the update fails. The <code>DeploymentStatus</code> for the draft configuration indicates whether the deployment is in process or has failed. The draft configuration remains in existence until it is deleted with this action.</p>

#### `deletePlatformVersion`

``` purescript
deletePlatformVersion :: forall eff. DeletePlatformVersionRequest -> Aff (err :: RequestError | eff) DeletePlatformVersionResult
```

<p>Deletes the specified version of a custom platform.</p>

#### `describeApplicationVersions`

``` purescript
describeApplicationVersions :: forall eff. DescribeApplicationVersionsMessage -> Aff (err :: RequestError | eff) ApplicationVersionDescriptionsMessage
```

<p>Retrieve a list of application versions.</p>

#### `describeApplications`

``` purescript
describeApplications :: forall eff. DescribeApplicationsMessage -> Aff (err :: RequestError | eff) ApplicationDescriptionsMessage
```

<p>Returns the descriptions of existing applications.</p>

#### `describeConfigurationOptions`

``` purescript
describeConfigurationOptions :: forall eff. DescribeConfigurationOptionsMessage -> Aff (err :: RequestError | eff) ConfigurationOptionsDescription
```

<p>Describes the configuration options that are used in a particular configuration template or environment, or that a specified solution stack defines. The description includes the values the options, their default values, and an indication of the required action on a running environment if an option value is changed.</p>

#### `describeConfigurationSettings`

``` purescript
describeConfigurationSettings :: forall eff. DescribeConfigurationSettingsMessage -> Aff (err :: RequestError | eff) ConfigurationSettingsDescriptions
```

<p>Returns a description of the settings for the specified configuration set, that is, either a configuration template or the configuration set associated with a running environment.</p> <p>When describing the settings for the configuration set associated with a running environment, it is possible to receive two sets of setting descriptions. One is the deployed configuration set, and the other is a draft configuration of an environment that is either in the process of deployment or that failed to deploy.</p> <p>Related Topics</p> <ul> <li> <p> <a>DeleteEnvironmentConfiguration</a> </p> </li> </ul>

#### `describeEnvironmentHealth`

``` purescript
describeEnvironmentHealth :: forall eff. DescribeEnvironmentHealthRequest -> Aff (err :: RequestError | eff) DescribeEnvironmentHealthResult
```

<p>Returns information about the overall health of the specified environment. The <b>DescribeEnvironmentHealth</b> operation is only available with AWS Elastic Beanstalk Enhanced Health.</p>

#### `describeEnvironmentManagedActionHistory`

``` purescript
describeEnvironmentManagedActionHistory :: forall eff. DescribeEnvironmentManagedActionHistoryRequest -> Aff (err :: RequestError | eff) DescribeEnvironmentManagedActionHistoryResult
```

<p>Lists an environment's completed and failed managed actions.</p>

#### `describeEnvironmentManagedActions`

``` purescript
describeEnvironmentManagedActions :: forall eff. DescribeEnvironmentManagedActionsRequest -> Aff (err :: RequestError | eff) DescribeEnvironmentManagedActionsResult
```

<p>Lists an environment's upcoming and in-progress managed actions.</p>

#### `describeEnvironmentResources`

``` purescript
describeEnvironmentResources :: forall eff. DescribeEnvironmentResourcesMessage -> Aff (err :: RequestError | eff) EnvironmentResourceDescriptionsMessage
```

<p>Returns AWS resources for this environment.</p>

#### `describeEnvironments`

``` purescript
describeEnvironments :: forall eff. DescribeEnvironmentsMessage -> Aff (err :: RequestError | eff) EnvironmentDescriptionsMessage
```

<p>Returns descriptions for existing environments.</p>

#### `describeEvents`

``` purescript
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: RequestError | eff) EventDescriptionsMessage
```

<p>Returns list of event descriptions matching criteria up to the last 6 weeks.</p> <note> <p>This action returns the most recent 1,000 events from the specified <code>NextToken</code>.</p> </note>

#### `describeInstancesHealth`

``` purescript
describeInstancesHealth :: forall eff. DescribeInstancesHealthRequest -> Aff (err :: RequestError | eff) DescribeInstancesHealthResult
```

<p>Retrives detailed information about the health of instances in your AWS Elastic Beanstalk. This operation requires <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced.html">enhanced health reporting</a>.</p>

#### `describePlatformVersion`

``` purescript
describePlatformVersion :: forall eff. DescribePlatformVersionRequest -> Aff (err :: RequestError | eff) DescribePlatformVersionResult
```

<p>Describes the version of the platform.</p>

#### `listAvailableSolutionStacks`

``` purescript
listAvailableSolutionStacks :: forall eff. Aff (err :: RequestError | eff) ListAvailableSolutionStacksResultMessage
```

<p>Returns a list of the available solution stack names, with the public version first and then in reverse chronological order.</p>

#### `listPlatformVersions`

``` purescript
listPlatformVersions :: forall eff. ListPlatformVersionsRequest -> Aff (err :: RequestError | eff) ListPlatformVersionsResult
```

<p>Lists the available platforms.</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: RequestError | eff) ResourceTagsDescriptionMessage
```

<p>Returns the tags applied to an AWS Elastic Beanstalk resource. The response contains a list of tag key-value pairs.</p> <p>Currently, Elastic Beanstalk only supports tagging of Elastic Beanstalk environments. For details about environment tagging, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.tagging.html">Tagging Resources in Your Elastic Beanstalk Environment</a>.</p>

#### `rebuildEnvironment`

``` purescript
rebuildEnvironment :: forall eff. RebuildEnvironmentMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes and recreates all of the AWS resources (for example: the Auto Scaling group, load balancer, etc.) for a specified environment and forces a restart.</p>

#### `requestEnvironmentInfo`

``` purescript
requestEnvironmentInfo :: forall eff. RequestEnvironmentInfoMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Initiates a request to compile the specified type of information of the deployed environment.</p> <p> Setting the <code>InfoType</code> to <code>tail</code> compiles the last lines from the application server log files of every Amazon EC2 instance in your environment. </p> <p> Setting the <code>InfoType</code> to <code>bundle</code> compresses the application server log files for every Amazon EC2 instance into a <code>.zip</code> file. Legacy and .NET containers do not support bundle logs. </p> <p> Use <a>RetrieveEnvironmentInfo</a> to obtain the set of logs. </p> <p>Related Topics</p> <ul> <li> <p> <a>RetrieveEnvironmentInfo</a> </p> </li> </ul>

#### `restartAppServer`

``` purescript
restartAppServer :: forall eff. RestartAppServerMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Causes the environment to restart the application container server running on each Amazon EC2 instance.</p>

#### `retrieveEnvironmentInfo`

``` purescript
retrieveEnvironmentInfo :: forall eff. RetrieveEnvironmentInfoMessage -> Aff (err :: RequestError | eff) RetrieveEnvironmentInfoResultMessage
```

<p>Retrieves the compiled information from a <a>RequestEnvironmentInfo</a> request.</p> <p>Related Topics</p> <ul> <li> <p> <a>RequestEnvironmentInfo</a> </p> </li> </ul>

#### `swapEnvironmentCNAMEs`

``` purescript
swapEnvironmentCNAMEs :: forall eff. SwapEnvironmentCNAMEsMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Swaps the CNAMEs of two environments.</p>

#### `terminateEnvironment`

``` purescript
terminateEnvironment :: forall eff. TerminateEnvironmentMessage -> Aff (err :: RequestError | eff) EnvironmentDescription
```

<p>Terminates the specified environment.</p>

#### `updateApplication`

``` purescript
updateApplication :: forall eff. UpdateApplicationMessage -> Aff (err :: RequestError | eff) ApplicationDescriptionMessage
```

<p>Updates the specified application to have the specified properties.</p> <note> <p>If a property (for example, <code>description</code>) is not provided, the value remains unchanged. To clear these properties, specify an empty string.</p> </note>

#### `updateApplicationResourceLifecycle`

``` purescript
updateApplicationResourceLifecycle :: forall eff. UpdateApplicationResourceLifecycleMessage -> Aff (err :: RequestError | eff) ApplicationResourceLifecycleDescriptionMessage
```

<p>Modifies lifecycle settings for an application.</p>

#### `updateApplicationVersion`

``` purescript
updateApplicationVersion :: forall eff. UpdateApplicationVersionMessage -> Aff (err :: RequestError | eff) ApplicationVersionDescriptionMessage
```

<p>Updates the specified application version to have the specified properties.</p> <note> <p>If a property (for example, <code>description</code>) is not provided, the value remains unchanged. To clear properties, specify an empty string.</p> </note>

#### `updateConfigurationTemplate`

``` purescript
updateConfigurationTemplate :: forall eff. UpdateConfigurationTemplateMessage -> Aff (err :: RequestError | eff) ConfigurationSettingsDescription
```

<p>Updates the specified configuration template to have the specified properties or configuration option values.</p> <note> <p>If a property (for example, <code>ApplicationName</code>) is not provided, its value remains unchanged. To clear such properties, specify an empty string.</p> </note> <p>Related Topics</p> <ul> <li> <p> <a>DescribeConfigurationOptions</a> </p> </li> </ul>

#### `updateEnvironment`

``` purescript
updateEnvironment :: forall eff. UpdateEnvironmentMessage -> Aff (err :: RequestError | eff) EnvironmentDescription
```

<p>Updates the environment description, deploys a new application version, updates the configuration settings to an entirely new configuration template, or updates select configuration option values in the running environment.</p> <p> Attempting to update both the release and configuration is not allowed and AWS Elastic Beanstalk returns an <code>InvalidParameterCombination</code> error. </p> <p> When updating the configuration settings to a new template or individual settings, a draft configuration is created and <a>DescribeConfigurationSettings</a> for this environment returns two setting descriptions with different <code>DeploymentStatus</code> values. </p>

#### `updateTagsForResource`

``` purescript
updateTagsForResource :: forall eff. UpdateTagsForResourceMessage -> Aff (err :: RequestError | eff) Unit
```

<p>Update the list of tags applied to an AWS Elastic Beanstalk resource. Two lists can be passed: <code>TagsToAdd</code> for tags to add or update, and <code>TagsToRemove</code>.</p> <p>Currently, Elastic Beanstalk only supports tagging of Elastic Beanstalk environments. For details about environment tagging, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.tagging.html">Tagging Resources in Your Elastic Beanstalk Environment</a>.</p> <p>If you create a custom IAM user policy to control permission to this operation, specify one of the following two virtual actions (or both) instead of the API operation name:</p> <dl> <dt>elasticbeanstalk:AddTags</dt> <dd> <p>Controls permission to call <code>UpdateTagsForResource</code> and pass a list of tags to add in the <code>TagsToAdd</code> parameter.</p> </dd> <dt>elasticbeanstalk:RemoveTags</dt> <dd> <p>Controls permission to call <code>UpdateTagsForResource</code> and pass a list of tag keys to remove in the <code>TagsToRemove</code> parameter.</p> </dd> </dl> <p>For details about creating a custom user policy, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/AWSHowTo.iam.managed-policies.html#AWSHowTo.iam.policies">Creating a Custom User Policy</a>.</p>

#### `validateConfigurationSettings`

``` purescript
validateConfigurationSettings :: forall eff. ValidateConfigurationSettingsMessage -> Aff (err :: RequestError | eff) ConfigurationSettingsValidationMessages
```

<p>Takes a set of configuration settings and either a configuration template or environment, and determines whether those values are valid.</p> <p>This action returns a list of messages indicating any errors or warnings associated with the selection of option values.</p>

#### `ARN`

``` purescript
newtype ARN
  = ARN String
```

#### `AbortEnvironmentUpdateMessage`

``` purescript
newtype AbortEnvironmentUpdateMessage
  = AbortEnvironmentUpdateMessage { "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName) }
```

<p/>

#### `AbortableOperationInProgress`

``` purescript
newtype AbortableOperationInProgress
  = AbortableOperationInProgress Boolean
```

#### `ActionHistoryStatus`

``` purescript
newtype ActionHistoryStatus
  = ActionHistoryStatus String
```

#### `ActionStatus`

``` purescript
newtype ActionStatus
  = ActionStatus String
```

#### `ActionType`

``` purescript
newtype ActionType
  = ActionType String
```

#### `ApplicationDescription`

``` purescript
newtype ApplicationDescription
  = ApplicationDescription { "ApplicationName" :: NullOrUndefined (ApplicationName), "Description" :: NullOrUndefined (Description), "DateCreated" :: NullOrUndefined (CreationDate), "DateUpdated" :: NullOrUndefined (UpdateDate), "Versions" :: NullOrUndefined (VersionLabelsList), "ConfigurationTemplates" :: NullOrUndefined (ConfigurationTemplateNamesList), "ResourceLifecycleConfig" :: NullOrUndefined (ApplicationResourceLifecycleConfig) }
```

<p>Describes the properties of an application.</p>

#### `ApplicationDescriptionList`

``` purescript
newtype ApplicationDescriptionList
  = ApplicationDescriptionList (Array ApplicationDescription)
```

#### `ApplicationDescriptionMessage`

``` purescript
newtype ApplicationDescriptionMessage
  = ApplicationDescriptionMessage { "Application" :: NullOrUndefined (ApplicationDescription) }
```

<p>Result message containing a single description of an application.</p>

#### `ApplicationDescriptionsMessage`

``` purescript
newtype ApplicationDescriptionsMessage
  = ApplicationDescriptionsMessage { "Applications" :: NullOrUndefined (ApplicationDescriptionList) }
```

<p>Result message containing a list of application descriptions.</p>

#### `ApplicationMetrics`

``` purescript
newtype ApplicationMetrics
  = ApplicationMetrics { "Duration" :: NullOrUndefined (NullableInteger), "RequestCount" :: NullOrUndefined (RequestCount), "StatusCodes" :: NullOrUndefined (StatusCodes), "Latency" :: NullOrUndefined (Latency) }
```

<p>Application request metrics for an AWS Elastic Beanstalk environment.</p>

#### `ApplicationName`

``` purescript
newtype ApplicationName
  = ApplicationName String
```

#### `ApplicationNamesList`

``` purescript
newtype ApplicationNamesList
  = ApplicationNamesList (Array ApplicationName)
```

#### `ApplicationResourceLifecycleConfig`

``` purescript
newtype ApplicationResourceLifecycleConfig
  = ApplicationResourceLifecycleConfig { "ServiceRole" :: NullOrUndefined (String), "VersionLifecycleConfig" :: NullOrUndefined (ApplicationVersionLifecycleConfig) }
```

<p>The resource lifecycle configuration for an application. Defines lifecycle settings for resources that belong to the application, and the service role that Elastic Beanstalk assumes in order to apply lifecycle settings. The version lifecycle configuration defines lifecycle settings for application versions.</p>

#### `ApplicationResourceLifecycleDescriptionMessage`

``` purescript
newtype ApplicationResourceLifecycleDescriptionMessage
  = ApplicationResourceLifecycleDescriptionMessage { "ApplicationName" :: NullOrUndefined (ApplicationName), "ResourceLifecycleConfig" :: NullOrUndefined (ApplicationResourceLifecycleConfig) }
```

#### `ApplicationVersionDescription`

``` purescript
newtype ApplicationVersionDescription
  = ApplicationVersionDescription { "ApplicationName" :: NullOrUndefined (ApplicationName), "Description" :: NullOrUndefined (Description), "VersionLabel" :: NullOrUndefined (VersionLabel), "SourceBuildInformation" :: NullOrUndefined (SourceBuildInformation), "BuildArn" :: NullOrUndefined (String), "SourceBundle" :: NullOrUndefined (S3Location), "DateCreated" :: NullOrUndefined (CreationDate), "DateUpdated" :: NullOrUndefined (UpdateDate), "Status" :: NullOrUndefined (ApplicationVersionStatus) }
```

<p>Describes the properties of an application version.</p>

#### `ApplicationVersionDescriptionList`

``` purescript
newtype ApplicationVersionDescriptionList
  = ApplicationVersionDescriptionList (Array ApplicationVersionDescription)
```

#### `ApplicationVersionDescriptionMessage`

``` purescript
newtype ApplicationVersionDescriptionMessage
  = ApplicationVersionDescriptionMessage { "ApplicationVersion" :: NullOrUndefined (ApplicationVersionDescription) }
```

<p>Result message wrapping a single description of an application version.</p>

#### `ApplicationVersionDescriptionsMessage`

``` purescript
newtype ApplicationVersionDescriptionsMessage
  = ApplicationVersionDescriptionsMessage { "ApplicationVersions" :: NullOrUndefined (ApplicationVersionDescriptionList), "NextToken" :: NullOrUndefined (Token) }
```

<p>Result message wrapping a list of application version descriptions.</p>

#### `ApplicationVersionLifecycleConfig`

``` purescript
newtype ApplicationVersionLifecycleConfig
  = ApplicationVersionLifecycleConfig { "MaxCountRule" :: NullOrUndefined (MaxCountRule), "MaxAgeRule" :: NullOrUndefined (MaxAgeRule) }
```

<p>The application version lifecycle settings for an application. Defines the rules that Elastic Beanstalk applies to an application's versions in order to avoid hitting the per-region limit for application versions.</p> <p>When Elastic Beanstalk deletes an application version from its database, you can no longer deploy that version to an environment. The source bundle remains in S3 unless you configure the rule to delete it.</p>

#### `ApplicationVersionProccess`

``` purescript
newtype ApplicationVersionProccess
  = ApplicationVersionProccess Boolean
```

#### `ApplicationVersionStatus`

``` purescript
newtype ApplicationVersionStatus
  = ApplicationVersionStatus String
```

#### `ApplyEnvironmentManagedActionRequest`

``` purescript
newtype ApplyEnvironmentManagedActionRequest
  = ApplyEnvironmentManagedActionRequest { "EnvironmentName" :: NullOrUndefined (String), "EnvironmentId" :: NullOrUndefined (String), "ActionId" :: String }
```

<p>Request to execute a scheduled managed action immediately.</p>

#### `ApplyEnvironmentManagedActionResult`

``` purescript
newtype ApplyEnvironmentManagedActionResult
  = ApplyEnvironmentManagedActionResult { "ActionId" :: NullOrUndefined (String), "ActionDescription" :: NullOrUndefined (String), "ActionType" :: NullOrUndefined (ActionType), "Status" :: NullOrUndefined (String) }
```

<p>The result message containing information about the managed action.</p>

#### `AutoCreateApplication`

``` purescript
newtype AutoCreateApplication
  = AutoCreateApplication Boolean
```

#### `AutoScalingGroup`

``` purescript
newtype AutoScalingGroup
  = AutoScalingGroup { "Name" :: NullOrUndefined (ResourceId) }
```

<p>Describes an Auto Scaling launch configuration.</p>

#### `AutoScalingGroupList`

``` purescript
newtype AutoScalingGroupList
  = AutoScalingGroupList (Array AutoScalingGroup)
```

#### `AvailableSolutionStackDetailsList`

``` purescript
newtype AvailableSolutionStackDetailsList
  = AvailableSolutionStackDetailsList (Array SolutionStackDescription)
```

#### `AvailableSolutionStackNamesList`

``` purescript
newtype AvailableSolutionStackNamesList
  = AvailableSolutionStackNamesList (Array SolutionStackName)
```

#### `BoxedBoolean`

``` purescript
newtype BoxedBoolean
  = BoxedBoolean Boolean
```

#### `BoxedInt`

``` purescript
newtype BoxedInt
  = BoxedInt Int
```

#### `BuildConfiguration`

``` purescript
newtype BuildConfiguration
  = BuildConfiguration { "ArtifactName" :: NullOrUndefined (String), "CodeBuildServiceRole" :: NonEmptyString, "ComputeType" :: NullOrUndefined (ComputeType), "Image" :: NonEmptyString, "TimeoutInMinutes" :: NullOrUndefined (BoxedInt) }
```

<p>Settings for an AWS CodeBuild build.</p>

#### `Builder`

``` purescript
newtype Builder
  = Builder { "ARN" :: NullOrUndefined (ARN) }
```

<p>The builder used to build the custom platform.</p>

#### `CPUUtilization`

``` purescript
newtype CPUUtilization
  = CPUUtilization { "User" :: NullOrUndefined (NullableDouble), "Nice" :: NullOrUndefined (NullableDouble), "System" :: NullOrUndefined (NullableDouble), "Idle" :: NullOrUndefined (NullableDouble), "IOWait" :: NullOrUndefined (NullableDouble), "IRQ" :: NullOrUndefined (NullableDouble), "SoftIRQ" :: NullOrUndefined (NullableDouble) }
```

<p>CPU utilization metrics for an instance.</p>

#### `Cause`

``` purescript
newtype Cause
  = Cause String
```

#### `Causes`

``` purescript
newtype Causes
  = Causes (Array Cause)
```

#### `CheckDNSAvailabilityMessage`

``` purescript
newtype CheckDNSAvailabilityMessage
  = CheckDNSAvailabilityMessage { "CNAMEPrefix" :: DNSCnamePrefix }
```

<p>Results message indicating whether a CNAME is available.</p>

#### `CheckDNSAvailabilityResultMessage`

``` purescript
newtype CheckDNSAvailabilityResultMessage
  = CheckDNSAvailabilityResultMessage { "Available" :: NullOrUndefined (CnameAvailability), "FullyQualifiedCNAME" :: NullOrUndefined (DNSCname) }
```

<p>Indicates if the specified CNAME is available.</p>

#### `CnameAvailability`

``` purescript
newtype CnameAvailability
  = CnameAvailability Boolean
```

#### `CodeBuildNotInServiceRegionException`

``` purescript
newtype CodeBuildNotInServiceRegionException
  = CodeBuildNotInServiceRegionException {  }
```

<p>AWS CodeBuild is not available in the specified region.</p>

#### `ComposeEnvironmentsMessage`

``` purescript
newtype ComposeEnvironmentsMessage
  = ComposeEnvironmentsMessage { "ApplicationName" :: NullOrUndefined (ApplicationName), "GroupName" :: NullOrUndefined (GroupName), "VersionLabels" :: NullOrUndefined (VersionLabels) }
```

<p>Request to create or update a group of environments.</p>

#### `ComputeType`

``` purescript
newtype ComputeType
  = ComputeType String
```

#### `ConfigurationDeploymentStatus`

``` purescript
newtype ConfigurationDeploymentStatus
  = ConfigurationDeploymentStatus String
```

#### `ConfigurationOptionDefaultValue`

``` purescript
newtype ConfigurationOptionDefaultValue
  = ConfigurationOptionDefaultValue String
```

#### `ConfigurationOptionDescription`

``` purescript
newtype ConfigurationOptionDescription
  = ConfigurationOptionDescription { "Namespace" :: NullOrUndefined (OptionNamespace), "Name" :: NullOrUndefined (ConfigurationOptionName), "DefaultValue" :: NullOrUndefined (ConfigurationOptionDefaultValue), "ChangeSeverity" :: NullOrUndefined (ConfigurationOptionSeverity), "UserDefined" :: NullOrUndefined (UserDefinedOption), "ValueType" :: NullOrUndefined (ConfigurationOptionValueType), "ValueOptions" :: NullOrUndefined (ConfigurationOptionPossibleValues), "MinValue" :: NullOrUndefined (OptionRestrictionMinValue), "MaxValue" :: NullOrUndefined (OptionRestrictionMaxValue), "MaxLength" :: NullOrUndefined (OptionRestrictionMaxLength), "Regex" :: NullOrUndefined (OptionRestrictionRegex) }
```

<p>Describes the possible values for a configuration option.</p>

#### `ConfigurationOptionDescriptionsList`

``` purescript
newtype ConfigurationOptionDescriptionsList
  = ConfigurationOptionDescriptionsList (Array ConfigurationOptionDescription)
```

#### `ConfigurationOptionName`

``` purescript
newtype ConfigurationOptionName
  = ConfigurationOptionName String
```

#### `ConfigurationOptionPossibleValue`

``` purescript
newtype ConfigurationOptionPossibleValue
  = ConfigurationOptionPossibleValue String
```

#### `ConfigurationOptionPossibleValues`

``` purescript
newtype ConfigurationOptionPossibleValues
  = ConfigurationOptionPossibleValues (Array ConfigurationOptionPossibleValue)
```

#### `ConfigurationOptionSetting`

``` purescript
newtype ConfigurationOptionSetting
  = ConfigurationOptionSetting { "ResourceName" :: NullOrUndefined (ResourceName), "Namespace" :: NullOrUndefined (OptionNamespace), "OptionName" :: NullOrUndefined (ConfigurationOptionName), "Value" :: NullOrUndefined (ConfigurationOptionValue) }
```

<p> A specification identifying an individual configuration option along with its current value. For a list of possible option values, go to <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html">Option Values</a> in the <i>AWS Elastic Beanstalk Developer Guide</i>. </p>

#### `ConfigurationOptionSettingsList`

``` purescript
newtype ConfigurationOptionSettingsList
  = ConfigurationOptionSettingsList (Array ConfigurationOptionSetting)
```

#### `ConfigurationOptionSeverity`

``` purescript
newtype ConfigurationOptionSeverity
  = ConfigurationOptionSeverity String
```

#### `ConfigurationOptionValue`

``` purescript
newtype ConfigurationOptionValue
  = ConfigurationOptionValue String
```

#### `ConfigurationOptionValueType`

``` purescript
newtype ConfigurationOptionValueType
  = ConfigurationOptionValueType String
```

#### `ConfigurationOptionsDescription`

``` purescript
newtype ConfigurationOptionsDescription
  = ConfigurationOptionsDescription { "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PlatformArn" :: NullOrUndefined (PlatformArn), "Options" :: NullOrUndefined (ConfigurationOptionDescriptionsList) }
```

<p>Describes the settings for a specified configuration set.</p>

#### `ConfigurationSettingsDescription`

``` purescript
newtype ConfigurationSettingsDescription
  = ConfigurationSettingsDescription { "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PlatformArn" :: NullOrUndefined (PlatformArn), "ApplicationName" :: NullOrUndefined (ApplicationName), "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "Description" :: NullOrUndefined (Description), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "DeploymentStatus" :: NullOrUndefined (ConfigurationDeploymentStatus), "DateCreated" :: NullOrUndefined (CreationDate), "DateUpdated" :: NullOrUndefined (UpdateDate), "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList) }
```

<p>Describes the settings for a configuration set.</p>

#### `ConfigurationSettingsDescriptionList`

``` purescript
newtype ConfigurationSettingsDescriptionList
  = ConfigurationSettingsDescriptionList (Array ConfigurationSettingsDescription)
```

#### `ConfigurationSettingsDescriptions`

``` purescript
newtype ConfigurationSettingsDescriptions
  = ConfigurationSettingsDescriptions { "ConfigurationSettings" :: NullOrUndefined (ConfigurationSettingsDescriptionList) }
```

<p>The results from a request to change the configuration settings of an environment.</p>

#### `ConfigurationSettingsValidationMessages`

``` purescript
newtype ConfigurationSettingsValidationMessages
  = ConfigurationSettingsValidationMessages { "Messages" :: NullOrUndefined (ValidationMessagesList) }
```

<p>Provides a list of validation messages.</p>

#### `ConfigurationTemplateName`

``` purescript
newtype ConfigurationTemplateName
  = ConfigurationTemplateName String
```

#### `ConfigurationTemplateNamesList`

``` purescript
newtype ConfigurationTemplateNamesList
  = ConfigurationTemplateNamesList (Array ConfigurationTemplateName)
```

#### `CreateApplicationMessage`

``` purescript
newtype CreateApplicationMessage
  = CreateApplicationMessage { "ApplicationName" :: ApplicationName, "Description" :: NullOrUndefined (Description), "ResourceLifecycleConfig" :: NullOrUndefined (ApplicationResourceLifecycleConfig) }
```

<p>Request to create an application.</p>

#### `CreateApplicationVersionMessage`

``` purescript
newtype CreateApplicationVersionMessage
  = CreateApplicationVersionMessage { "ApplicationName" :: ApplicationName, "VersionLabel" :: VersionLabel, "Description" :: NullOrUndefined (Description), "SourceBuildInformation" :: NullOrUndefined (SourceBuildInformation), "SourceBundle" :: NullOrUndefined (S3Location), "BuildConfiguration" :: NullOrUndefined (BuildConfiguration), "AutoCreateApplication" :: NullOrUndefined (AutoCreateApplication), "Process" :: NullOrUndefined (ApplicationVersionProccess) }
```

<p/>

#### `CreateConfigurationTemplateMessage`

``` purescript
newtype CreateConfigurationTemplateMessage
  = CreateConfigurationTemplateMessage { "ApplicationName" :: ApplicationName, "TemplateName" :: ConfigurationTemplateName, "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PlatformArn" :: NullOrUndefined (PlatformArn), "SourceConfiguration" :: NullOrUndefined (SourceConfiguration), "EnvironmentId" :: NullOrUndefined (EnvironmentId), "Description" :: NullOrUndefined (Description), "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList) }
```

<p>Request to create a configuration template.</p>

#### `CreateEnvironmentMessage`

``` purescript
newtype CreateEnvironmentMessage
  = CreateEnvironmentMessage { "ApplicationName" :: ApplicationName, "EnvironmentName" :: NullOrUndefined (EnvironmentName), "GroupName" :: NullOrUndefined (GroupName), "Description" :: NullOrUndefined (Description), "CNAMEPrefix" :: NullOrUndefined (DNSCnamePrefix), "Tier" :: NullOrUndefined (EnvironmentTier), "Tags" :: NullOrUndefined (Tags), "VersionLabel" :: NullOrUndefined (VersionLabel), "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PlatformArn" :: NullOrUndefined (PlatformArn), "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList), "OptionsToRemove" :: NullOrUndefined (OptionsSpecifierList) }
```

<p/>

#### `CreatePlatformVersionRequest`

``` purescript
newtype CreatePlatformVersionRequest
  = CreatePlatformVersionRequest { "PlatformName" :: PlatformName, "PlatformVersion" :: PlatformVersion, "PlatformDefinitionBundle" :: S3Location, "EnvironmentName" :: NullOrUndefined (EnvironmentName), "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList) }
```

<p>Request to create a new platform version.</p>

#### `CreatePlatformVersionResult`

``` purescript
newtype CreatePlatformVersionResult
  = CreatePlatformVersionResult { "PlatformSummary" :: NullOrUndefined (PlatformSummary), "Builder" :: NullOrUndefined (Builder) }
```

#### `CreateStorageLocationResultMessage`

``` purescript
newtype CreateStorageLocationResultMessage
  = CreateStorageLocationResultMessage { "S3Bucket" :: NullOrUndefined (S3Bucket) }
```

<p>Results of a <a>CreateStorageLocationResult</a> call.</p>

#### `CreationDate`

``` purescript
newtype CreationDate
  = CreationDate Number
```

#### `CustomAmi`

``` purescript
newtype CustomAmi
  = CustomAmi { "VirtualizationType" :: NullOrUndefined (VirtualizationType), "ImageId" :: NullOrUndefined (ImageId) }
```

<p>A custom AMI available to platforms.</p>

#### `CustomAmiList`

``` purescript
newtype CustomAmiList
  = CustomAmiList (Array CustomAmi)
```

#### `DNSCname`

``` purescript
newtype DNSCname
  = DNSCname String
```

#### `DNSCnamePrefix`

``` purescript
newtype DNSCnamePrefix
  = DNSCnamePrefix String
```

#### `DeleteApplicationMessage`

``` purescript
newtype DeleteApplicationMessage
  = DeleteApplicationMessage { "ApplicationName" :: ApplicationName, "TerminateEnvByForce" :: NullOrUndefined (TerminateEnvForce) }
```

<p>Request to delete an application.</p>

#### `DeleteApplicationVersionMessage`

``` purescript
newtype DeleteApplicationVersionMessage
  = DeleteApplicationVersionMessage { "ApplicationName" :: ApplicationName, "VersionLabel" :: VersionLabel, "DeleteSourceBundle" :: NullOrUndefined (DeleteSourceBundle) }
```

<p>Request to delete an application version.</p>

#### `DeleteConfigurationTemplateMessage`

``` purescript
newtype DeleteConfigurationTemplateMessage
  = DeleteConfigurationTemplateMessage { "ApplicationName" :: ApplicationName, "TemplateName" :: ConfigurationTemplateName }
```

<p>Request to delete a configuration template.</p>

#### `DeleteEnvironmentConfigurationMessage`

``` purescript
newtype DeleteEnvironmentConfigurationMessage
  = DeleteEnvironmentConfigurationMessage { "ApplicationName" :: ApplicationName, "EnvironmentName" :: EnvironmentName }
```

<p>Request to delete a draft environment configuration.</p>

#### `DeletePlatformVersionRequest`

``` purescript
newtype DeletePlatformVersionRequest
  = DeletePlatformVersionRequest { "PlatformArn" :: NullOrUndefined (PlatformArn) }
```

#### `DeletePlatformVersionResult`

``` purescript
newtype DeletePlatformVersionResult
  = DeletePlatformVersionResult { "PlatformSummary" :: NullOrUndefined (PlatformSummary) }
```

#### `DeleteSourceBundle`

``` purescript
newtype DeleteSourceBundle
  = DeleteSourceBundle Boolean
```

#### `Deployment`

``` purescript
newtype Deployment
  = Deployment { "VersionLabel" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (NullableLong), "Status" :: NullOrUndefined (String), "DeploymentTime" :: NullOrUndefined (DeploymentTimestamp) }
```

<p>Information about an application version deployment.</p>

#### `DeploymentTimestamp`

``` purescript
newtype DeploymentTimestamp
  = DeploymentTimestamp Number
```

#### `DescribeApplicationVersionsMessage`

``` purescript
newtype DescribeApplicationVersionsMessage
  = DescribeApplicationVersionsMessage { "ApplicationName" :: NullOrUndefined (ApplicationName), "VersionLabels" :: NullOrUndefined (VersionLabelsList), "MaxRecords" :: NullOrUndefined (MaxRecords), "NextToken" :: NullOrUndefined (Token) }
```

<p>Request to describe application versions.</p>

#### `DescribeApplicationsMessage`

``` purescript
newtype DescribeApplicationsMessage
  = DescribeApplicationsMessage { "ApplicationNames" :: NullOrUndefined (ApplicationNamesList) }
```

<p>Request to describe one or more applications.</p>

#### `DescribeConfigurationOptionsMessage`

``` purescript
newtype DescribeConfigurationOptionsMessage
  = DescribeConfigurationOptionsMessage { "ApplicationName" :: NullOrUndefined (ApplicationName), "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PlatformArn" :: NullOrUndefined (PlatformArn), "Options" :: NullOrUndefined (OptionsSpecifierList) }
```

<p>Result message containing a list of application version descriptions.</p>

#### `DescribeConfigurationSettingsMessage`

``` purescript
newtype DescribeConfigurationSettingsMessage
  = DescribeConfigurationSettingsMessage { "ApplicationName" :: ApplicationName, "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "EnvironmentName" :: NullOrUndefined (EnvironmentName) }
```

<p>Result message containing all of the configuration settings for a specified solution stack or configuration template.</p>

#### `DescribeEnvironmentHealthRequest`

``` purescript
newtype DescribeEnvironmentHealthRequest
  = DescribeEnvironmentHealthRequest { "EnvironmentName" :: NullOrUndefined (EnvironmentName), "EnvironmentId" :: NullOrUndefined (EnvironmentId), "AttributeNames" :: NullOrUndefined (EnvironmentHealthAttributes) }
```

<p>See the example below to learn how to create a request body.</p>

#### `DescribeEnvironmentHealthResult`

``` purescript
newtype DescribeEnvironmentHealthResult
  = DescribeEnvironmentHealthResult { "EnvironmentName" :: NullOrUndefined (EnvironmentName), "HealthStatus" :: NullOrUndefined (String), "Status" :: NullOrUndefined (EnvironmentHealth), "Color" :: NullOrUndefined (String), "Causes" :: NullOrUndefined (Causes), "ApplicationMetrics" :: NullOrUndefined (ApplicationMetrics), "InstancesHealth" :: NullOrUndefined (InstanceHealthSummary), "RefreshedAt" :: NullOrUndefined (RefreshedAt) }
```

<p>Health details for an AWS Elastic Beanstalk environment.</p>

#### `DescribeEnvironmentManagedActionHistoryRequest`

``` purescript
newtype DescribeEnvironmentManagedActionHistoryRequest
  = DescribeEnvironmentManagedActionHistoryRequest { "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "NextToken" :: NullOrUndefined (String), "MaxItems" :: NullOrUndefined (Int) }
```

<p>Request to list completed and failed managed actions.</p>

#### `DescribeEnvironmentManagedActionHistoryResult`

``` purescript
newtype DescribeEnvironmentManagedActionHistoryResult
  = DescribeEnvironmentManagedActionHistoryResult { "ManagedActionHistoryItems" :: NullOrUndefined (ManagedActionHistoryItems), "NextToken" :: NullOrUndefined (String) }
```

<p>A result message containing a list of completed and failed managed actions.</p>

#### `DescribeEnvironmentManagedActionsRequest`

``` purescript
newtype DescribeEnvironmentManagedActionsRequest
  = DescribeEnvironmentManagedActionsRequest { "EnvironmentName" :: NullOrUndefined (String), "EnvironmentId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (ActionStatus) }
```

<p>Request to list an environment's upcoming and in-progress managed actions.</p>

#### `DescribeEnvironmentManagedActionsResult`

``` purescript
newtype DescribeEnvironmentManagedActionsResult
  = DescribeEnvironmentManagedActionsResult { "ManagedActions" :: NullOrUndefined (ManagedActions) }
```

<p>The result message containing a list of managed actions.</p>

#### `DescribeEnvironmentResourcesMessage`

``` purescript
newtype DescribeEnvironmentResourcesMessage
  = DescribeEnvironmentResourcesMessage { "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName) }
```

<p>Request to describe the resources in an environment.</p>

#### `DescribeEnvironmentsMessage`

``` purescript
newtype DescribeEnvironmentsMessage
  = DescribeEnvironmentsMessage { "ApplicationName" :: NullOrUndefined (ApplicationName), "VersionLabel" :: NullOrUndefined (VersionLabel), "EnvironmentIds" :: NullOrUndefined (EnvironmentIdList), "EnvironmentNames" :: NullOrUndefined (EnvironmentNamesList), "IncludeDeleted" :: NullOrUndefined (IncludeDeleted), "IncludedDeletedBackTo" :: NullOrUndefined (IncludeDeletedBackTo), "MaxRecords" :: NullOrUndefined (MaxRecords), "NextToken" :: NullOrUndefined (Token) }
```

<p>Request to describe one or more environments.</p>

#### `DescribeEventsMessage`

``` purescript
newtype DescribeEventsMessage
  = DescribeEventsMessage { "ApplicationName" :: NullOrUndefined (ApplicationName), "VersionLabel" :: NullOrUndefined (VersionLabel), "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "PlatformArn" :: NullOrUndefined (PlatformArn), "RequestId" :: NullOrUndefined (RequestId), "Severity" :: NullOrUndefined (EventSeverity), "StartTime" :: NullOrUndefined (TimeFilterStart), "EndTime" :: NullOrUndefined (TimeFilterEnd), "MaxRecords" :: NullOrUndefined (MaxRecords), "NextToken" :: NullOrUndefined (Token) }
```

<p>Request to retrieve a list of events for an environment.</p>

#### `DescribeInstancesHealthRequest`

``` purescript
newtype DescribeInstancesHealthRequest
  = DescribeInstancesHealthRequest { "EnvironmentName" :: NullOrUndefined (EnvironmentName), "EnvironmentId" :: NullOrUndefined (EnvironmentId), "AttributeNames" :: NullOrUndefined (InstancesHealthAttributes), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Parameters for a call to <code>DescribeInstancesHealth</code>.</p>

#### `DescribeInstancesHealthResult`

``` purescript
newtype DescribeInstancesHealthResult
  = DescribeInstancesHealthResult { "InstanceHealthList" :: NullOrUndefined (InstanceHealthList), "RefreshedAt" :: NullOrUndefined (RefreshedAt), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Detailed health information about the Amazon EC2 instances in an AWS Elastic Beanstalk environment.</p>

#### `DescribePlatformVersionRequest`

``` purescript
newtype DescribePlatformVersionRequest
  = DescribePlatformVersionRequest { "PlatformArn" :: NullOrUndefined (PlatformArn) }
```

#### `DescribePlatformVersionResult`

``` purescript
newtype DescribePlatformVersionResult
  = DescribePlatformVersionResult { "PlatformDescription" :: NullOrUndefined (PlatformDescription) }
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `Ec2InstanceId`

``` purescript
newtype Ec2InstanceId
  = Ec2InstanceId String
```

#### `ElasticBeanstalkServiceException`

``` purescript
newtype ElasticBeanstalkServiceException
  = ElasticBeanstalkServiceException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>A generic service exception has occurred.</p>

#### `EndpointURL`

``` purescript
newtype EndpointURL
  = EndpointURL String
```

#### `EnvironmentArn`

``` purescript
newtype EnvironmentArn
  = EnvironmentArn String
```

#### `EnvironmentDescription`

``` purescript
newtype EnvironmentDescription
  = EnvironmentDescription { "EnvironmentName" :: NullOrUndefined (EnvironmentName), "EnvironmentId" :: NullOrUndefined (EnvironmentId), "ApplicationName" :: NullOrUndefined (ApplicationName), "VersionLabel" :: NullOrUndefined (VersionLabel), "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PlatformArn" :: NullOrUndefined (PlatformArn), "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "Description" :: NullOrUndefined (Description), "EndpointURL" :: NullOrUndefined (EndpointURL), "CNAME" :: NullOrUndefined (DNSCname), "DateCreated" :: NullOrUndefined (CreationDate), "DateUpdated" :: NullOrUndefined (UpdateDate), "Status" :: NullOrUndefined (EnvironmentStatus), "AbortableOperationInProgress" :: NullOrUndefined (AbortableOperationInProgress), "Health" :: NullOrUndefined (EnvironmentHealth), "HealthStatus" :: NullOrUndefined (EnvironmentHealthStatus), "Resources" :: NullOrUndefined (EnvironmentResourcesDescription), "Tier" :: NullOrUndefined (EnvironmentTier), "EnvironmentLinks" :: NullOrUndefined (EnvironmentLinks), "EnvironmentArn" :: NullOrUndefined (EnvironmentArn) }
```

<p>Describes the properties of an environment.</p>

#### `EnvironmentDescriptionsList`

``` purescript
newtype EnvironmentDescriptionsList
  = EnvironmentDescriptionsList (Array EnvironmentDescription)
```

#### `EnvironmentDescriptionsMessage`

``` purescript
newtype EnvironmentDescriptionsMessage
  = EnvironmentDescriptionsMessage { "Environments" :: NullOrUndefined (EnvironmentDescriptionsList), "NextToken" :: NullOrUndefined (Token) }
```

<p>Result message containing a list of environment descriptions.</p>

#### `EnvironmentHealth`

``` purescript
newtype EnvironmentHealth
  = EnvironmentHealth String
```

#### `EnvironmentHealthAttribute`

``` purescript
newtype EnvironmentHealthAttribute
  = EnvironmentHealthAttribute String
```

#### `EnvironmentHealthAttributes`

``` purescript
newtype EnvironmentHealthAttributes
  = EnvironmentHealthAttributes (Array EnvironmentHealthAttribute)
```

#### `EnvironmentHealthStatus`

``` purescript
newtype EnvironmentHealthStatus
  = EnvironmentHealthStatus String
```

#### `EnvironmentId`

``` purescript
newtype EnvironmentId
  = EnvironmentId String
```

#### `EnvironmentIdList`

``` purescript
newtype EnvironmentIdList
  = EnvironmentIdList (Array EnvironmentId)
```

#### `EnvironmentInfoDescription`

``` purescript
newtype EnvironmentInfoDescription
  = EnvironmentInfoDescription { "InfoType" :: NullOrUndefined (EnvironmentInfoType), "Ec2InstanceId" :: NullOrUndefined (Ec2InstanceId), "SampleTimestamp" :: NullOrUndefined (SampleTimestamp), "Message" :: NullOrUndefined (Message) }
```

<p>The information retrieved from the Amazon EC2 instances.</p>

#### `EnvironmentInfoDescriptionList`

``` purescript
newtype EnvironmentInfoDescriptionList
  = EnvironmentInfoDescriptionList (Array EnvironmentInfoDescription)
```

#### `EnvironmentInfoType`

``` purescript
newtype EnvironmentInfoType
  = EnvironmentInfoType String
```

#### `EnvironmentLink`

``` purescript
newtype EnvironmentLink
  = EnvironmentLink { "LinkName" :: NullOrUndefined (String), "EnvironmentName" :: NullOrUndefined (String) }
```

<p>A link to another environment, defined in the environment's manifest. Links provide connection information in system properties that can be used to connect to another environment in the same group. See <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html">Environment Manifest (env.yaml)</a> for details.</p>

#### `EnvironmentLinks`

``` purescript
newtype EnvironmentLinks
  = EnvironmentLinks (Array EnvironmentLink)
```

#### `EnvironmentName`

``` purescript
newtype EnvironmentName
  = EnvironmentName String
```

#### `EnvironmentNamesList`

``` purescript
newtype EnvironmentNamesList
  = EnvironmentNamesList (Array EnvironmentName)
```

#### `EnvironmentResourceDescription`

``` purescript
newtype EnvironmentResourceDescription
  = EnvironmentResourceDescription { "EnvironmentName" :: NullOrUndefined (EnvironmentName), "AutoScalingGroups" :: NullOrUndefined (AutoScalingGroupList), "Instances" :: NullOrUndefined (InstanceList), "LaunchConfigurations" :: NullOrUndefined (LaunchConfigurationList), "LoadBalancers" :: NullOrUndefined (LoadBalancerList), "Triggers" :: NullOrUndefined (TriggerList), "Queues" :: NullOrUndefined (QueueList) }
```

<p>Describes the AWS resources in use by this environment. This data is live.</p>

#### `EnvironmentResourceDescriptionsMessage`

``` purescript
newtype EnvironmentResourceDescriptionsMessage
  = EnvironmentResourceDescriptionsMessage { "EnvironmentResources" :: NullOrUndefined (EnvironmentResourceDescription) }
```

<p>Result message containing a list of environment resource descriptions.</p>

#### `EnvironmentResourcesDescription`

``` purescript
newtype EnvironmentResourcesDescription
  = EnvironmentResourcesDescription { "LoadBalancer" :: NullOrUndefined (LoadBalancerDescription) }
```

<p>Describes the AWS resources in use by this environment. This data is not live data.</p>

#### `EnvironmentStatus`

``` purescript
newtype EnvironmentStatus
  = EnvironmentStatus String
```

#### `EnvironmentTier`

``` purescript
newtype EnvironmentTier
  = EnvironmentTier { "Name" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

<p>Describes the properties of an environment tier</p>

#### `EventDate`

``` purescript
newtype EventDate
  = EventDate Number
```

#### `EventDescription`

``` purescript
newtype EventDescription
  = EventDescription { "EventDate" :: NullOrUndefined (EventDate), "Message" :: NullOrUndefined (EventMessage), "ApplicationName" :: NullOrUndefined (ApplicationName), "VersionLabel" :: NullOrUndefined (VersionLabel), "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "PlatformArn" :: NullOrUndefined (PlatformArn), "RequestId" :: NullOrUndefined (RequestId), "Severity" :: NullOrUndefined (EventSeverity) }
```

<p>Describes an event.</p>

#### `EventDescriptionList`

``` purescript
newtype EventDescriptionList
  = EventDescriptionList (Array EventDescription)
```

#### `EventDescriptionsMessage`

``` purescript
newtype EventDescriptionsMessage
  = EventDescriptionsMessage { "Events" :: NullOrUndefined (EventDescriptionList), "NextToken" :: NullOrUndefined (Token) }
```

<p>Result message wrapping a list of event descriptions.</p>

#### `EventMessage`

``` purescript
newtype EventMessage
  = EventMessage String
```

#### `EventSeverity`

``` purescript
newtype EventSeverity
  = EventSeverity String
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

#### `FailureType`

``` purescript
newtype FailureType
  = FailureType String
```

#### `FileTypeExtension`

``` purescript
newtype FileTypeExtension
  = FileTypeExtension String
```

#### `ForceTerminate`

``` purescript
newtype ForceTerminate
  = ForceTerminate Boolean
```

#### `GroupName`

``` purescript
newtype GroupName
  = GroupName String
```

#### `ImageId`

``` purescript
newtype ImageId
  = ImageId String
```

#### `IncludeDeleted`

``` purescript
newtype IncludeDeleted
  = IncludeDeleted Boolean
```

#### `IncludeDeletedBackTo`

``` purescript
newtype IncludeDeletedBackTo
  = IncludeDeletedBackTo Number
```

#### `Instance`

``` purescript
newtype Instance
  = Instance { "Id" :: NullOrUndefined (ResourceId) }
```

<p>The description of an Amazon EC2 instance.</p>

#### `InstanceHealthList`

``` purescript
newtype InstanceHealthList
  = InstanceHealthList (Array SingleInstanceHealth)
```

#### `InstanceHealthSummary`

``` purescript
newtype InstanceHealthSummary
  = InstanceHealthSummary { "NoData" :: NullOrUndefined (NullableInteger), "Unknown" :: NullOrUndefined (NullableInteger), "Pending" :: NullOrUndefined (NullableInteger), "Ok" :: NullOrUndefined (NullableInteger), "Info" :: NullOrUndefined (NullableInteger), "Warning" :: NullOrUndefined (NullableInteger), "Degraded" :: NullOrUndefined (NullableInteger), "Severe" :: NullOrUndefined (NullableInteger) }
```

<p>Represents summary information about the health of an instance. For more information, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html">Health Colors and Statuses</a>.</p>

#### `InstanceId`

``` purescript
newtype InstanceId
  = InstanceId String
```

#### `InstanceList`

``` purescript
newtype InstanceList
  = InstanceList (Array Instance)
```

#### `InstancesHealthAttribute`

``` purescript
newtype InstancesHealthAttribute
  = InstancesHealthAttribute String
```

#### `InstancesHealthAttributes`

``` purescript
newtype InstancesHealthAttributes
  = InstancesHealthAttributes (Array InstancesHealthAttribute)
```

#### `InsufficientPrivilegesException`

``` purescript
newtype InsufficientPrivilegesException
  = InsufficientPrivilegesException {  }
```

<p>The specified account does not have sufficient privileges for one of more AWS services.</p>

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException {  }
```

<p>One or more input parameters is not valid. Please correct the input parameters and try the operation again.</p>

#### `Latency`

``` purescript
newtype Latency
  = Latency { "P999" :: NullOrUndefined (NullableDouble), "P99" :: NullOrUndefined (NullableDouble), "P95" :: NullOrUndefined (NullableDouble), "P90" :: NullOrUndefined (NullableDouble), "P85" :: NullOrUndefined (NullableDouble), "P75" :: NullOrUndefined (NullableDouble), "P50" :: NullOrUndefined (NullableDouble), "P10" :: NullOrUndefined (NullableDouble) }
```

<p>Represents the average latency for the slowest X percent of requests over the last 10 seconds.</p>

#### `LaunchConfiguration`

``` purescript
newtype LaunchConfiguration
  = LaunchConfiguration { "Name" :: NullOrUndefined (ResourceId) }
```

<p>Describes an Auto Scaling launch configuration.</p>

#### `LaunchConfigurationList`

``` purescript
newtype LaunchConfigurationList
  = LaunchConfigurationList (Array LaunchConfiguration)
```

#### `LaunchedAt`

``` purescript
newtype LaunchedAt
  = LaunchedAt Number
```

#### `ListAvailableSolutionStacksResultMessage`

``` purescript
newtype ListAvailableSolutionStacksResultMessage
  = ListAvailableSolutionStacksResultMessage { "SolutionStacks" :: NullOrUndefined (AvailableSolutionStackNamesList), "SolutionStackDetails" :: NullOrUndefined (AvailableSolutionStackDetailsList) }
```

<p>A list of available AWS Elastic Beanstalk solution stacks.</p>

#### `ListPlatformVersionsRequest`

``` purescript
newtype ListPlatformVersionsRequest
  = ListPlatformVersionsRequest { "Filters" :: NullOrUndefined (PlatformFilters), "MaxRecords" :: NullOrUndefined (PlatformMaxRecords), "NextToken" :: NullOrUndefined (Token) }
```

#### `ListPlatformVersionsResult`

``` purescript
newtype ListPlatformVersionsResult
  = ListPlatformVersionsResult { "PlatformSummaryList" :: NullOrUndefined (PlatformSummaryList), "NextToken" :: NullOrUndefined (Token) }
```

#### `ListTagsForResourceMessage`

``` purescript
newtype ListTagsForResourceMessage
  = ListTagsForResourceMessage { "ResourceArn" :: ResourceArn }
```

#### `Listener`

``` purescript
newtype Listener
  = Listener { "Protocol" :: NullOrUndefined (String), "Port" :: NullOrUndefined (Int) }
```

<p>Describes the properties of a Listener for the LoadBalancer.</p>

#### `LoadAverage`

``` purescript
newtype LoadAverage
  = LoadAverage (Array LoadAverageValue)
```

#### `LoadAverageValue`

``` purescript
newtype LoadAverageValue
  = LoadAverageValue Number
```

#### `LoadBalancer`

``` purescript
newtype LoadBalancer
  = LoadBalancer { "Name" :: NullOrUndefined (ResourceId) }
```

<p>Describes a LoadBalancer.</p>

#### `LoadBalancerDescription`

``` purescript
newtype LoadBalancerDescription
  = LoadBalancerDescription { "LoadBalancerName" :: NullOrUndefined (String), "Domain" :: NullOrUndefined (String), "Listeners" :: NullOrUndefined (LoadBalancerListenersDescription) }
```

<p>Describes the details of a LoadBalancer.</p>

#### `LoadBalancerList`

``` purescript
newtype LoadBalancerList
  = LoadBalancerList (Array LoadBalancer)
```

#### `LoadBalancerListenersDescription`

``` purescript
newtype LoadBalancerListenersDescription
  = LoadBalancerListenersDescription (Array Listener)
```

#### `Maintainer`

``` purescript
newtype Maintainer
  = Maintainer String
```

#### `ManagedAction`

``` purescript
newtype ManagedAction
  = ManagedAction { "ActionId" :: NullOrUndefined (String), "ActionDescription" :: NullOrUndefined (String), "ActionType" :: NullOrUndefined (ActionType), "Status" :: NullOrUndefined (ActionStatus), "WindowStartTime" :: NullOrUndefined (Number) }
```

<p>The record of an upcoming or in-progress managed action.</p>

#### `ManagedActionHistoryItem`

``` purescript
newtype ManagedActionHistoryItem
  = ManagedActionHistoryItem { "ActionId" :: NullOrUndefined (String), "ActionType" :: NullOrUndefined (ActionType), "ActionDescription" :: NullOrUndefined (String), "FailureType" :: NullOrUndefined (FailureType), "Status" :: NullOrUndefined (ActionHistoryStatus), "FailureDescription" :: NullOrUndefined (String), "ExecutedTime" :: NullOrUndefined (Number), "FinishedTime" :: NullOrUndefined (Number) }
```

<p>The record of a completed or failed managed action.</p>

#### `ManagedActionHistoryItems`

``` purescript
newtype ManagedActionHistoryItems
  = ManagedActionHistoryItems (Array ManagedActionHistoryItem)
```

#### `ManagedActionInvalidStateException`

``` purescript
newtype ManagedActionInvalidStateException
  = ManagedActionInvalidStateException {  }
```

<p>Cannot modify the managed action in its current state.</p>

#### `ManagedActions`

``` purescript
newtype ManagedActions
  = ManagedActions (Array ManagedAction)
```

#### `MaxAgeRule`

``` purescript
newtype MaxAgeRule
  = MaxAgeRule { "Enabled" :: BoxedBoolean, "MaxAgeInDays" :: NullOrUndefined (BoxedInt), "DeleteSourceFromS3" :: NullOrUndefined (BoxedBoolean) }
```

<p>A lifecycle rule that deletes application versions after the specified number of days.</p>

#### `MaxCountRule`

``` purescript
newtype MaxCountRule
  = MaxCountRule { "Enabled" :: BoxedBoolean, "MaxCount" :: NullOrUndefined (BoxedInt), "DeleteSourceFromS3" :: NullOrUndefined (BoxedBoolean) }
```

<p>A lifecycle rule that deletes the oldest application version when the maximum count is exceeded.</p>

#### `MaxRecords`

``` purescript
newtype MaxRecords
  = MaxRecords Int
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

#### `NullableDouble`

``` purescript
newtype NullableDouble
  = NullableDouble Number
```

#### `NullableInteger`

``` purescript
newtype NullableInteger
  = NullableInteger Int
```

#### `NullableLong`

``` purescript
newtype NullableLong
  = NullableLong Number
```

#### `OperatingSystemName`

``` purescript
newtype OperatingSystemName
  = OperatingSystemName String
```

#### `OperatingSystemVersion`

``` purescript
newtype OperatingSystemVersion
  = OperatingSystemVersion String
```

#### `OperationInProgressException`

``` purescript
newtype OperationInProgressException
  = OperationInProgressException {  }
```

<p>Unable to perform the specified operation because another operation that effects an element in this activity is already in progress.</p>

#### `OptionNamespace`

``` purescript
newtype OptionNamespace
  = OptionNamespace String
```

#### `OptionRestrictionMaxLength`

``` purescript
newtype OptionRestrictionMaxLength
  = OptionRestrictionMaxLength Int
```

#### `OptionRestrictionMaxValue`

``` purescript
newtype OptionRestrictionMaxValue
  = OptionRestrictionMaxValue Int
```

#### `OptionRestrictionMinValue`

``` purescript
newtype OptionRestrictionMinValue
  = OptionRestrictionMinValue Int
```

#### `OptionRestrictionRegex`

``` purescript
newtype OptionRestrictionRegex
  = OptionRestrictionRegex { "Pattern" :: NullOrUndefined (RegexPattern), "Label" :: NullOrUndefined (RegexLabel) }
```

<p>A regular expression representing a restriction on a string configuration option value.</p>

#### `OptionSpecification`

``` purescript
newtype OptionSpecification
  = OptionSpecification { "ResourceName" :: NullOrUndefined (ResourceName), "Namespace" :: NullOrUndefined (OptionNamespace), "OptionName" :: NullOrUndefined (ConfigurationOptionName) }
```

<p>A specification identifying an individual configuration option.</p>

#### `OptionsSpecifierList`

``` purescript
newtype OptionsSpecifierList
  = OptionsSpecifierList (Array OptionSpecification)
```

#### `PlatformArn`

``` purescript
newtype PlatformArn
  = PlatformArn String
```

#### `PlatformCategory`

``` purescript
newtype PlatformCategory
  = PlatformCategory String
```

#### `PlatformDescription`

``` purescript
newtype PlatformDescription
  = PlatformDescription { "PlatformArn" :: NullOrUndefined (PlatformArn), "PlatformOwner" :: NullOrUndefined (PlatformOwner), "PlatformName" :: NullOrUndefined (PlatformName), "PlatformVersion" :: NullOrUndefined (PlatformVersion), "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PlatformStatus" :: NullOrUndefined (PlatformStatus), "DateCreated" :: NullOrUndefined (CreationDate), "DateUpdated" :: NullOrUndefined (UpdateDate), "PlatformCategory" :: NullOrUndefined (PlatformCategory), "Description" :: NullOrUndefined (Description), "Maintainer" :: NullOrUndefined (Maintainer), "OperatingSystemName" :: NullOrUndefined (OperatingSystemName), "OperatingSystemVersion" :: NullOrUndefined (OperatingSystemVersion), "ProgrammingLanguages" :: NullOrUndefined (PlatformProgrammingLanguages), "Frameworks" :: NullOrUndefined (PlatformFrameworks), "CustomAmiList" :: NullOrUndefined (CustomAmiList), "SupportedTierList" :: NullOrUndefined (SupportedTierList), "SupportedAddonList" :: NullOrUndefined (SupportedAddonList) }
```

<p>Detailed information about a platform.</p>

#### `PlatformFilter`

``` purescript
newtype PlatformFilter
  = PlatformFilter { "Type" :: NullOrUndefined (PlatformFilterType), "Operator" :: NullOrUndefined (PlatformFilterOperator), "Values" :: NullOrUndefined (PlatformFilterValueList) }
```

<p>Specify criteria to restrict the results when listing custom platforms.</p> <p>The filter is evaluated as the expression:</p> <p> <code>Type</code> <code>Operator</code> <code>Values[i]</code> </p>

#### `PlatformFilterOperator`

``` purescript
newtype PlatformFilterOperator
  = PlatformFilterOperator String
```

#### `PlatformFilterType`

``` purescript
newtype PlatformFilterType
  = PlatformFilterType String
```

#### `PlatformFilterValue`

``` purescript
newtype PlatformFilterValue
  = PlatformFilterValue String
```

#### `PlatformFilterValueList`

``` purescript
newtype PlatformFilterValueList
  = PlatformFilterValueList (Array PlatformFilterValue)
```

#### `PlatformFilters`

``` purescript
newtype PlatformFilters
  = PlatformFilters (Array PlatformFilter)
```

#### `PlatformFramework`

``` purescript
newtype PlatformFramework
  = PlatformFramework { "Name" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

<p>A framework supported by the custom platform.</p>

#### `PlatformFrameworks`

``` purescript
newtype PlatformFrameworks
  = PlatformFrameworks (Array PlatformFramework)
```

#### `PlatformMaxRecords`

``` purescript
newtype PlatformMaxRecords
  = PlatformMaxRecords Int
```

#### `PlatformName`

``` purescript
newtype PlatformName
  = PlatformName String
```

#### `PlatformOwner`

``` purescript
newtype PlatformOwner
  = PlatformOwner String
```

#### `PlatformProgrammingLanguage`

``` purescript
newtype PlatformProgrammingLanguage
  = PlatformProgrammingLanguage { "Name" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

<p>A programming language supported by the platform.</p>

#### `PlatformProgrammingLanguages`

``` purescript
newtype PlatformProgrammingLanguages
  = PlatformProgrammingLanguages (Array PlatformProgrammingLanguage)
```

#### `PlatformStatus`

``` purescript
newtype PlatformStatus
  = PlatformStatus String
```

#### `PlatformSummary`

``` purescript
newtype PlatformSummary
  = PlatformSummary { "PlatformArn" :: NullOrUndefined (PlatformArn), "PlatformOwner" :: NullOrUndefined (PlatformOwner), "PlatformStatus" :: NullOrUndefined (PlatformStatus), "PlatformCategory" :: NullOrUndefined (PlatformCategory), "OperatingSystemName" :: NullOrUndefined (OperatingSystemName), "OperatingSystemVersion" :: NullOrUndefined (OperatingSystemVersion), "SupportedTierList" :: NullOrUndefined (SupportedTierList), "SupportedAddonList" :: NullOrUndefined (SupportedAddonList) }
```

<p>Detailed information about a platform.</p>

#### `PlatformSummaryList`

``` purescript
newtype PlatformSummaryList
  = PlatformSummaryList (Array PlatformSummary)
```

#### `PlatformVersion`

``` purescript
newtype PlatformVersion
  = PlatformVersion String
```

#### `PlatformVersionStillReferencedException`

``` purescript
newtype PlatformVersionStillReferencedException
  = PlatformVersionStillReferencedException {  }
```

<p>You cannot delete the platform version because there are still environments running on it.</p>

#### `Queue`

``` purescript
newtype Queue
  = Queue { "Name" :: NullOrUndefined (String), "URL" :: NullOrUndefined (String) }
```

<p>Describes a queue.</p>

#### `QueueList`

``` purescript
newtype QueueList
  = QueueList (Array Queue)
```

#### `RebuildEnvironmentMessage`

``` purescript
newtype RebuildEnvironmentMessage
  = RebuildEnvironmentMessage { "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName) }
```

<p/>

#### `RefreshedAt`

``` purescript
newtype RefreshedAt
  = RefreshedAt Number
```

#### `RegexLabel`

``` purescript
newtype RegexLabel
  = RegexLabel String
```

#### `RegexPattern`

``` purescript
newtype RegexPattern
  = RegexPattern String
```

#### `RequestCount`

``` purescript
newtype RequestCount
  = RequestCount Int
```

#### `RequestEnvironmentInfoMessage`

``` purescript
newtype RequestEnvironmentInfoMessage
  = RequestEnvironmentInfoMessage { "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "InfoType" :: EnvironmentInfoType }
```

<p>Request to retrieve logs from an environment and store them in your Elastic Beanstalk storage bucket.</p>

#### `RequestId`

``` purescript
newtype RequestId
  = RequestId String
```

#### `ResourceArn`

``` purescript
newtype ResourceArn
  = ResourceArn String
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

#### `ResourceName`

``` purescript
newtype ResourceName
  = ResourceName String
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>A resource doesn't exist for the specified Amazon Resource Name (ARN).</p>

#### `ResourceTagsDescriptionMessage`

``` purescript
newtype ResourceTagsDescriptionMessage
  = ResourceTagsDescriptionMessage { "ResourceArn" :: NullOrUndefined (ResourceArn), "ResourceTags" :: NullOrUndefined (TagList) }
```

#### `ResourceTypeNotSupportedException`

``` purescript
newtype ResourceTypeNotSupportedException
  = ResourceTypeNotSupportedException {  }
```

<p>The type of the specified Amazon Resource Name (ARN) isn't supported for this operation.</p>

#### `RestartAppServerMessage`

``` purescript
newtype RestartAppServerMessage
  = RestartAppServerMessage { "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName) }
```

<p/>

#### `RetrieveEnvironmentInfoMessage`

``` purescript
newtype RetrieveEnvironmentInfoMessage
  = RetrieveEnvironmentInfoMessage { "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "InfoType" :: EnvironmentInfoType }
```

<p>Request to download logs retrieved with <a>RequestEnvironmentInfo</a>.</p>

#### `RetrieveEnvironmentInfoResultMessage`

``` purescript
newtype RetrieveEnvironmentInfoResultMessage
  = RetrieveEnvironmentInfoResultMessage { "EnvironmentInfo" :: NullOrUndefined (EnvironmentInfoDescriptionList) }
```

<p>Result message containing a description of the requested environment info.</p>

#### `S3Bucket`

``` purescript
newtype S3Bucket
  = S3Bucket String
```

#### `S3Key`

``` purescript
newtype S3Key
  = S3Key String
```

#### `S3Location`

``` purescript
newtype S3Location
  = S3Location { "S3Bucket" :: NullOrUndefined (S3Bucket), "S3Key" :: NullOrUndefined (S3Key) }
```

<p>The bucket and key of an item stored in Amazon S3.</p>

#### `S3LocationNotInServiceRegionException`

``` purescript
newtype S3LocationNotInServiceRegionException
  = S3LocationNotInServiceRegionException {  }
```

<p>The specified S3 bucket does not belong to the S3 region in which the service is running. The following regions are supported:</p> <ul> <li> <p>IAD/us-east-1</p> </li> <li> <p>PDX/us-west-2</p> </li> <li> <p>DUB/eu-west-1</p> </li> </ul>

#### `S3SubscriptionRequiredException`

``` purescript
newtype S3SubscriptionRequiredException
  = S3SubscriptionRequiredException {  }
```

<p>The specified account does not have a subscription to Amazon S3.</p>

#### `SampleTimestamp`

``` purescript
newtype SampleTimestamp
  = SampleTimestamp Number
```

#### `SingleInstanceHealth`

``` purescript
newtype SingleInstanceHealth
  = SingleInstanceHealth { "InstanceId" :: NullOrUndefined (InstanceId), "HealthStatus" :: NullOrUndefined (String), "Color" :: NullOrUndefined (String), "Causes" :: NullOrUndefined (Causes), "LaunchedAt" :: NullOrUndefined (LaunchedAt), "ApplicationMetrics" :: NullOrUndefined (ApplicationMetrics), "System" :: NullOrUndefined (SystemStatus), "Deployment" :: NullOrUndefined (Deployment), "AvailabilityZone" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (String) }
```

<p>Detailed health information about an Amazon EC2 instance in your Elastic Beanstalk environment.</p>

#### `SolutionStackDescription`

``` purescript
newtype SolutionStackDescription
  = SolutionStackDescription { "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PermittedFileTypes" :: NullOrUndefined (SolutionStackFileTypeList) }
```

<p>Describes the solution stack.</p>

#### `SolutionStackFileTypeList`

``` purescript
newtype SolutionStackFileTypeList
  = SolutionStackFileTypeList (Array FileTypeExtension)
```

#### `SolutionStackName`

``` purescript
newtype SolutionStackName
  = SolutionStackName String
```

#### `SourceBuildInformation`

``` purescript
newtype SourceBuildInformation
  = SourceBuildInformation { "SourceType" :: SourceType, "SourceRepository" :: SourceRepository, "SourceLocation" :: SourceLocation }
```

<p>Location of the source code for an application version.</p>

#### `SourceBundleDeletionException`

``` purescript
newtype SourceBundleDeletionException
  = SourceBundleDeletionException {  }
```

<p>Unable to delete the Amazon S3 source bundle associated with the application version. The application version was deleted successfully.</p>

#### `SourceConfiguration`

``` purescript
newtype SourceConfiguration
  = SourceConfiguration { "ApplicationName" :: NullOrUndefined (ApplicationName), "TemplateName" :: NullOrUndefined (ConfigurationTemplateName) }
```

<p>A specification for an environment configuration</p>

#### `SourceLocation`

``` purescript
newtype SourceLocation
  = SourceLocation String
```

#### `SourceRepository`

``` purescript
newtype SourceRepository
  = SourceRepository String
```

#### `SourceType`

``` purescript
newtype SourceType
  = SourceType String
```

#### `StatusCodes`

``` purescript
newtype StatusCodes
  = StatusCodes { "Status2xx" :: NullOrUndefined (NullableInteger), "Status3xx" :: NullOrUndefined (NullableInteger), "Status4xx" :: NullOrUndefined (NullableInteger), "Status5xx" :: NullOrUndefined (NullableInteger) }
```

<p>Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response. For more information, see <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html">Status Code Definitions</a>.</p>

#### `SupportedAddon`

``` purescript
newtype SupportedAddon
  = SupportedAddon String
```

#### `SupportedAddonList`

``` purescript
newtype SupportedAddonList
  = SupportedAddonList (Array SupportedAddon)
```

#### `SupportedTier`

``` purescript
newtype SupportedTier
  = SupportedTier String
```

#### `SupportedTierList`

``` purescript
newtype SupportedTierList
  = SupportedTierList (Array SupportedTier)
```

#### `SwapEnvironmentCNAMEsMessage`

``` purescript
newtype SwapEnvironmentCNAMEsMessage
  = SwapEnvironmentCNAMEsMessage { "SourceEnvironmentId" :: NullOrUndefined (EnvironmentId), "SourceEnvironmentName" :: NullOrUndefined (EnvironmentName), "DestinationEnvironmentId" :: NullOrUndefined (EnvironmentId), "DestinationEnvironmentName" :: NullOrUndefined (EnvironmentName) }
```

<p>Swaps the CNAMEs of two environments.</p>

#### `SystemStatus`

``` purescript
newtype SystemStatus
  = SystemStatus { "CPUUtilization" :: NullOrUndefined (CPUUtilization), "LoadAverage" :: NullOrUndefined (LoadAverage) }
```

<p>CPU utilization and load average metrics for an Amazon EC2 instance.</p>

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (TagKey), "Value" :: NullOrUndefined (TagValue) }
```

<p>Describes a tag applied to a resource in an environment.</p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Array Tag)
```

#### `TerminateEnvForce`

``` purescript
newtype TerminateEnvForce
  = TerminateEnvForce Boolean
```

#### `TerminateEnvironmentMessage`

``` purescript
newtype TerminateEnvironmentMessage
  = TerminateEnvironmentMessage { "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "TerminateResources" :: NullOrUndefined (TerminateEnvironmentResources), "ForceTerminate" :: NullOrUndefined (ForceTerminate) }
```

<p>Request to terminate an environment.</p>

#### `TerminateEnvironmentResources`

``` purescript
newtype TerminateEnvironmentResources
  = TerminateEnvironmentResources Boolean
```

#### `TimeFilterEnd`

``` purescript
newtype TimeFilterEnd
  = TimeFilterEnd Number
```

#### `TimeFilterStart`

``` purescript
newtype TimeFilterStart
  = TimeFilterStart Number
```

#### `Token`

``` purescript
newtype Token
  = Token String
```

#### `TooManyApplicationVersionsException`

``` purescript
newtype TooManyApplicationVersionsException
  = TooManyApplicationVersionsException {  }
```

<p>The specified account has reached its limit of application versions.</p>

#### `TooManyApplicationsException`

``` purescript
newtype TooManyApplicationsException
  = TooManyApplicationsException {  }
```

<p>The specified account has reached its limit of applications.</p>

#### `TooManyBucketsException`

``` purescript
newtype TooManyBucketsException
  = TooManyBucketsException {  }
```

<p>The specified account has reached its limit of Amazon S3 buckets.</p>

#### `TooManyConfigurationTemplatesException`

``` purescript
newtype TooManyConfigurationTemplatesException
  = TooManyConfigurationTemplatesException {  }
```

<p>The specified account has reached its limit of configuration templates.</p>

#### `TooManyEnvironmentsException`

``` purescript
newtype TooManyEnvironmentsException
  = TooManyEnvironmentsException {  }
```

<p>The specified account has reached its limit of environments.</p>

#### `TooManyPlatformsException`

``` purescript
newtype TooManyPlatformsException
  = TooManyPlatformsException {  }
```

<p>You have exceeded the maximum number of allowed platforms associated with the account.</p>

#### `TooManyTagsException`

``` purescript
newtype TooManyTagsException
  = TooManyTagsException {  }
```

<p>The number of tags in the resource would exceed the number of tags that each resource can have.</p> <p>To calculate this, the operation considers both the number of tags the resource already has and the tags this operation would add if it succeeded.</p>

#### `Trigger`

``` purescript
newtype Trigger
  = Trigger { "Name" :: NullOrUndefined (ResourceId) }
```

<p>Describes a trigger.</p>

#### `TriggerList`

``` purescript
newtype TriggerList
  = TriggerList (Array Trigger)
```

#### `UpdateApplicationMessage`

``` purescript
newtype UpdateApplicationMessage
  = UpdateApplicationMessage { "ApplicationName" :: ApplicationName, "Description" :: NullOrUndefined (Description) }
```

<p>Request to update an application.</p>

#### `UpdateApplicationResourceLifecycleMessage`

``` purescript
newtype UpdateApplicationResourceLifecycleMessage
  = UpdateApplicationResourceLifecycleMessage { "ApplicationName" :: ApplicationName, "ResourceLifecycleConfig" :: ApplicationResourceLifecycleConfig }
```

#### `UpdateApplicationVersionMessage`

``` purescript
newtype UpdateApplicationVersionMessage
  = UpdateApplicationVersionMessage { "ApplicationName" :: ApplicationName, "VersionLabel" :: VersionLabel, "Description" :: NullOrUndefined (Description) }
```

<p/>

#### `UpdateConfigurationTemplateMessage`

``` purescript
newtype UpdateConfigurationTemplateMessage
  = UpdateConfigurationTemplateMessage { "ApplicationName" :: ApplicationName, "TemplateName" :: ConfigurationTemplateName, "Description" :: NullOrUndefined (Description), "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList), "OptionsToRemove" :: NullOrUndefined (OptionsSpecifierList) }
```

<p>The result message containing the options for the specified solution stack.</p>

#### `UpdateDate`

``` purescript
newtype UpdateDate
  = UpdateDate Number
```

#### `UpdateEnvironmentMessage`

``` purescript
newtype UpdateEnvironmentMessage
  = UpdateEnvironmentMessage { "ApplicationName" :: NullOrUndefined (ApplicationName), "EnvironmentId" :: NullOrUndefined (EnvironmentId), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "GroupName" :: NullOrUndefined (GroupName), "Description" :: NullOrUndefined (Description), "Tier" :: NullOrUndefined (EnvironmentTier), "VersionLabel" :: NullOrUndefined (VersionLabel), "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "SolutionStackName" :: NullOrUndefined (SolutionStackName), "PlatformArn" :: NullOrUndefined (PlatformArn), "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList), "OptionsToRemove" :: NullOrUndefined (OptionsSpecifierList) }
```

<p>Request to update an environment.</p>

#### `UpdateTagsForResourceMessage`

``` purescript
newtype UpdateTagsForResourceMessage
  = UpdateTagsForResourceMessage { "ResourceArn" :: ResourceArn, "TagsToAdd" :: NullOrUndefined (TagList), "TagsToRemove" :: NullOrUndefined (TagKeyList) }
```

#### `UserDefinedOption`

``` purescript
newtype UserDefinedOption
  = UserDefinedOption Boolean
```

#### `ValidateConfigurationSettingsMessage`

``` purescript
newtype ValidateConfigurationSettingsMessage
  = ValidateConfigurationSettingsMessage { "ApplicationName" :: ApplicationName, "TemplateName" :: NullOrUndefined (ConfigurationTemplateName), "EnvironmentName" :: NullOrUndefined (EnvironmentName), "OptionSettings" :: ConfigurationOptionSettingsList }
```

<p>A list of validation messages for a specified configuration template.</p>

#### `ValidationMessage`

``` purescript
newtype ValidationMessage
  = ValidationMessage { "Message" :: NullOrUndefined (ValidationMessageString), "Severity" :: NullOrUndefined (ValidationSeverity), "Namespace" :: NullOrUndefined (OptionNamespace), "OptionName" :: NullOrUndefined (ConfigurationOptionName) }
```

<p>An error or warning for a desired configuration option value.</p>

#### `ValidationMessageString`

``` purescript
newtype ValidationMessageString
  = ValidationMessageString String
```

#### `ValidationMessagesList`

``` purescript
newtype ValidationMessagesList
  = ValidationMessagesList (Array ValidationMessage)
```

#### `ValidationSeverity`

``` purescript
newtype ValidationSeverity
  = ValidationSeverity String
```

#### `VersionLabel`

``` purescript
newtype VersionLabel
  = VersionLabel String
```

#### `VersionLabels`

``` purescript
newtype VersionLabels
  = VersionLabels (Array VersionLabel)
```

#### `VersionLabelsList`

``` purescript
newtype VersionLabelsList
  = VersionLabelsList (Array VersionLabel)
```

#### `VirtualizationType`

``` purescript
newtype VirtualizationType
  = VirtualizationType String
```


