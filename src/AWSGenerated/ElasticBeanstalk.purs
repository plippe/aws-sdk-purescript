

-- | <fullname>AWS Elastic Beanstalk</fullname> <p>AWS Elastic Beanstalk makes it easy for you to create, deploy, and manage scalable, fault-tolerant applications running on the Amazon Web Services cloud.</p> <p>For more information about this product, go to the <a href="http://aws.amazon.com/elasticbeanstalk/">AWS Elastic Beanstalk</a> details page. The location of the latest AWS Elastic Beanstalk WSDL is <a href="http://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl">http://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl</a>. To install the Software Development Kits (SDKs), Integrated Development Environment (IDE) Toolkits, and command line tools that enable you to access the API, go to <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> <p> <b>Endpoints</b> </p> <p>For a list of region-specific endpoints that AWS Elastic Beanstalk supports, go to <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticbeanstalk_region">Regions and Endpoints</a> in the <i>Amazon Web Services Glossary</i>.</p>
module AWS.ElasticBeanstalk where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ElasticBeanstalk" :: String


-- | <p>Cancels in-progress environment configuration update or application version deployment.</p>
abortEnvironmentUpdate :: forall eff. AbortEnvironmentUpdateMessage -> Aff (err :: AWS.RequestError | eff) Unit
abortEnvironmentUpdate = AWS.request serviceName "abortEnvironmentUpdate" 


-- | <p>Applies a scheduled managed action immediately. A managed action can be applied only if its status is <code>Scheduled</code>. Get the status and action ID of a managed action with <a>DescribeEnvironmentManagedActions</a>.</p>
applyEnvironmentManagedAction :: forall eff. ApplyEnvironmentManagedActionRequest -> Aff (err :: AWS.RequestError | eff) ApplyEnvironmentManagedActionResult
applyEnvironmentManagedAction = AWS.request serviceName "applyEnvironmentManagedAction" 


-- | <p>Checks if the specified CNAME is available.</p>
checkDNSAvailability :: forall eff. CheckDNSAvailabilityMessage -> Aff (err :: AWS.RequestError | eff) CheckDNSAvailabilityResultMessage
checkDNSAvailability = AWS.request serviceName "checkDNSAvailability" 


-- | <p>Create or update a group of environments that each run a separate component of a single application. Takes a list of version labels that specify application source bundles for each of the environments to create or update. The name of each environment and other required information must be included in the source bundles in an environment manifest named <code>env.yaml</code>. See <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-mgmt-compose.html">Compose Environments</a> for details.</p>
composeEnvironments :: forall eff. ComposeEnvironmentsMessage -> Aff (err :: AWS.RequestError | eff) EnvironmentDescriptionsMessage
composeEnvironments = AWS.request serviceName "composeEnvironments" 


-- | <p> Creates an application that has one configuration template named <code>default</code> and no application versions. </p>
createApplication :: forall eff. CreateApplicationMessage -> Aff (err :: AWS.RequestError | eff) ApplicationDescriptionMessage
createApplication = AWS.request serviceName "createApplication" 


-- | <p>Creates an application version for the specified application. You can create an application version from a source bundle in Amazon S3, a commit in AWS CodeCommit, or the output of an AWS CodeBuild build as follows:</p> <p>Specify a commit in an AWS CodeCommit repository with <code>SourceBuildInformation</code>.</p> <p>Specify a build in an AWS CodeBuild with <code>SourceBuildInformation</code> and <code>BuildConfiguration</code>.</p> <p>Specify a source bundle in S3 with <code>SourceBundle</code> </p> <p>Omit both <code>SourceBuildInformation</code> and <code>SourceBundle</code> to use the default sample application.</p> <note> <p>Once you create an application version with a specified Amazon S3 bucket and key location, you cannot change that Amazon S3 location. If you change the Amazon S3 location, you receive an exception when you attempt to launch an environment from the application version.</p> </note>
createApplicationVersion :: forall eff. CreateApplicationVersionMessage -> Aff (err :: AWS.RequestError | eff) ApplicationVersionDescriptionMessage
createApplicationVersion = AWS.request serviceName "createApplicationVersion" 


-- | <p>Creates a configuration template. Templates are associated with a specific application and are used to deploy different versions of the application with the same configuration settings.</p> <p>Related Topics</p> <ul> <li> <p> <a>DescribeConfigurationOptions</a> </p> </li> <li> <p> <a>DescribeConfigurationSettings</a> </p> </li> <li> <p> <a>ListAvailableSolutionStacks</a> </p> </li> </ul>
createConfigurationTemplate :: forall eff. CreateConfigurationTemplateMessage -> Aff (err :: AWS.RequestError | eff) ConfigurationSettingsDescription
createConfigurationTemplate = AWS.request serviceName "createConfigurationTemplate" 


-- | <p>Launches an environment for the specified application using the specified configuration.</p>
createEnvironment :: forall eff. CreateEnvironmentMessage -> Aff (err :: AWS.RequestError | eff) EnvironmentDescription
createEnvironment = AWS.request serviceName "createEnvironment" 


-- | <p>Create a new version of your custom platform.</p>
createPlatformVersion :: forall eff. CreatePlatformVersionRequest -> Aff (err :: AWS.RequestError | eff) CreatePlatformVersionResult
createPlatformVersion = AWS.request serviceName "createPlatformVersion" 


-- | <p>Creates a bucket in Amazon S3 to store application versions, logs, and other files used by Elastic Beanstalk environments. The Elastic Beanstalk console and EB CLI call this API the first time you create an environment in a region. If the storage location already exists, <code>CreateStorageLocation</code> still returns the bucket name but does not create a new bucket.</p>
createStorageLocation :: forall eff.  Aff (err :: AWS.RequestError | eff) CreateStorageLocationResultMessage
createStorageLocation = AWS.request serviceName "createStorageLocation" unit


-- | <p>Deletes the specified application along with all associated versions and configurations. The application versions will not be deleted from your Amazon S3 bucket.</p> <note> <p>You cannot delete an application that has a running environment.</p> </note>
deleteApplication :: forall eff. DeleteApplicationMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteApplication = AWS.request serviceName "deleteApplication" 


-- | <p>Deletes the specified version from the specified application.</p> <note> <p>You cannot delete an application version that is associated with a running environment.</p> </note>
deleteApplicationVersion :: forall eff. DeleteApplicationVersionMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteApplicationVersion = AWS.request serviceName "deleteApplicationVersion" 


-- | <p>Deletes the specified configuration template.</p> <note> <p>When you launch an environment using a configuration template, the environment gets a copy of the template. You can delete or modify the environment's copy of the template without affecting the running environment.</p> </note>
deleteConfigurationTemplate :: forall eff. DeleteConfigurationTemplateMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteConfigurationTemplate = AWS.request serviceName "deleteConfigurationTemplate" 


-- | <p>Deletes the draft configuration associated with the running environment.</p> <p>Updating a running environment with any configuration changes creates a draft configuration set. You can get the draft configuration using <a>DescribeConfigurationSettings</a> while the update is in progress or if the update fails. The <code>DeploymentStatus</code> for the draft configuration indicates whether the deployment is in process or has failed. The draft configuration remains in existence until it is deleted with this action.</p>
deleteEnvironmentConfiguration :: forall eff. DeleteEnvironmentConfigurationMessage -> Aff (err :: AWS.RequestError | eff) Unit
deleteEnvironmentConfiguration = AWS.request serviceName "deleteEnvironmentConfiguration" 


-- | <p>Deletes the specified version of a custom platform.</p>
deletePlatformVersion :: forall eff. DeletePlatformVersionRequest -> Aff (err :: AWS.RequestError | eff) DeletePlatformVersionResult
deletePlatformVersion = AWS.request serviceName "deletePlatformVersion" 


-- | <p>Retrieve a list of application versions.</p>
describeApplicationVersions :: forall eff. DescribeApplicationVersionsMessage -> Aff (err :: AWS.RequestError | eff) ApplicationVersionDescriptionsMessage
describeApplicationVersions = AWS.request serviceName "describeApplicationVersions" 


-- | <p>Returns the descriptions of existing applications.</p>
describeApplications :: forall eff. DescribeApplicationsMessage -> Aff (err :: AWS.RequestError | eff) ApplicationDescriptionsMessage
describeApplications = AWS.request serviceName "describeApplications" 


-- | <p>Describes the configuration options that are used in a particular configuration template or environment, or that a specified solution stack defines. The description includes the values the options, their default values, and an indication of the required action on a running environment if an option value is changed.</p>
describeConfigurationOptions :: forall eff. DescribeConfigurationOptionsMessage -> Aff (err :: AWS.RequestError | eff) ConfigurationOptionsDescription
describeConfigurationOptions = AWS.request serviceName "describeConfigurationOptions" 


-- | <p>Returns a description of the settings for the specified configuration set, that is, either a configuration template or the configuration set associated with a running environment.</p> <p>When describing the settings for the configuration set associated with a running environment, it is possible to receive two sets of setting descriptions. One is the deployed configuration set, and the other is a draft configuration of an environment that is either in the process of deployment or that failed to deploy.</p> <p>Related Topics</p> <ul> <li> <p> <a>DeleteEnvironmentConfiguration</a> </p> </li> </ul>
describeConfigurationSettings :: forall eff. DescribeConfigurationSettingsMessage -> Aff (err :: AWS.RequestError | eff) ConfigurationSettingsDescriptions
describeConfigurationSettings = AWS.request serviceName "describeConfigurationSettings" 


-- | <p>Returns information about the overall health of the specified environment. The <b>DescribeEnvironmentHealth</b> operation is only available with AWS Elastic Beanstalk Enhanced Health.</p>
describeEnvironmentHealth :: forall eff. DescribeEnvironmentHealthRequest -> Aff (err :: AWS.RequestError | eff) DescribeEnvironmentHealthResult
describeEnvironmentHealth = AWS.request serviceName "describeEnvironmentHealth" 


-- | <p>Lists an environment's completed and failed managed actions.</p>
describeEnvironmentManagedActionHistory :: forall eff. DescribeEnvironmentManagedActionHistoryRequest -> Aff (err :: AWS.RequestError | eff) DescribeEnvironmentManagedActionHistoryResult
describeEnvironmentManagedActionHistory = AWS.request serviceName "describeEnvironmentManagedActionHistory" 


-- | <p>Lists an environment's upcoming and in-progress managed actions.</p>
describeEnvironmentManagedActions :: forall eff. DescribeEnvironmentManagedActionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEnvironmentManagedActionsResult
describeEnvironmentManagedActions = AWS.request serviceName "describeEnvironmentManagedActions" 


-- | <p>Returns AWS resources for this environment.</p>
describeEnvironmentResources :: forall eff. DescribeEnvironmentResourcesMessage -> Aff (err :: AWS.RequestError | eff) EnvironmentResourceDescriptionsMessage
describeEnvironmentResources = AWS.request serviceName "describeEnvironmentResources" 


-- | <p>Returns descriptions for existing environments.</p>
describeEnvironments :: forall eff. DescribeEnvironmentsMessage -> Aff (err :: AWS.RequestError | eff) EnvironmentDescriptionsMessage
describeEnvironments = AWS.request serviceName "describeEnvironments" 


-- | <p>Returns list of event descriptions matching criteria up to the last 6 weeks.</p> <note> <p>This action returns the most recent 1,000 events from the specified <code>NextToken</code>.</p> </note>
describeEvents :: forall eff. DescribeEventsMessage -> Aff (err :: AWS.RequestError | eff) EventDescriptionsMessage
describeEvents = AWS.request serviceName "describeEvents" 


-- | <p>Retrives detailed information about the health of instances in your AWS Elastic Beanstalk. This operation requires <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced.html">enhanced health reporting</a>.</p>
describeInstancesHealth :: forall eff. DescribeInstancesHealthRequest -> Aff (err :: AWS.RequestError | eff) DescribeInstancesHealthResult
describeInstancesHealth = AWS.request serviceName "describeInstancesHealth" 


-- | <p>Describes the version of the platform.</p>
describePlatformVersion :: forall eff. DescribePlatformVersionRequest -> Aff (err :: AWS.RequestError | eff) DescribePlatformVersionResult
describePlatformVersion = AWS.request serviceName "describePlatformVersion" 


-- | <p>Returns a list of the available solution stack names, with the public version first and then in reverse chronological order.</p>
listAvailableSolutionStacks :: forall eff.  Aff (err :: AWS.RequestError | eff) ListAvailableSolutionStacksResultMessage
listAvailableSolutionStacks = AWS.request serviceName "listAvailableSolutionStacks" unit


-- | <p>Lists the available platforms.</p>
listPlatformVersions :: forall eff. ListPlatformVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListPlatformVersionsResult
listPlatformVersions = AWS.request serviceName "listPlatformVersions" 


-- | <p>Returns the tags applied to an AWS Elastic Beanstalk resource. The response contains a list of tag key-value pairs.</p> <p>Currently, Elastic Beanstalk only supports tagging of Elastic Beanstalk environments. For details about environment tagging, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.tagging.html">Tagging Resources in Your Elastic Beanstalk Environment</a>.</p>
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (err :: AWS.RequestError | eff) ResourceTagsDescriptionMessage
listTagsForResource = AWS.request serviceName "listTagsForResource" 


-- | <p>Deletes and recreates all of the AWS resources (for example: the Auto Scaling group, load balancer, etc.) for a specified environment and forces a restart.</p>
rebuildEnvironment :: forall eff. RebuildEnvironmentMessage -> Aff (err :: AWS.RequestError | eff) Unit
rebuildEnvironment = AWS.request serviceName "rebuildEnvironment" 


-- | <p>Initiates a request to compile the specified type of information of the deployed environment.</p> <p> Setting the <code>InfoType</code> to <code>tail</code> compiles the last lines from the application server log files of every Amazon EC2 instance in your environment. </p> <p> Setting the <code>InfoType</code> to <code>bundle</code> compresses the application server log files for every Amazon EC2 instance into a <code>.zip</code> file. Legacy and .NET containers do not support bundle logs. </p> <p> Use <a>RetrieveEnvironmentInfo</a> to obtain the set of logs. </p> <p>Related Topics</p> <ul> <li> <p> <a>RetrieveEnvironmentInfo</a> </p> </li> </ul>
requestEnvironmentInfo :: forall eff. RequestEnvironmentInfoMessage -> Aff (err :: AWS.RequestError | eff) Unit
requestEnvironmentInfo = AWS.request serviceName "requestEnvironmentInfo" 


-- | <p>Causes the environment to restart the application container server running on each Amazon EC2 instance.</p>
restartAppServer :: forall eff. RestartAppServerMessage -> Aff (err :: AWS.RequestError | eff) Unit
restartAppServer = AWS.request serviceName "restartAppServer" 


-- | <p>Retrieves the compiled information from a <a>RequestEnvironmentInfo</a> request.</p> <p>Related Topics</p> <ul> <li> <p> <a>RequestEnvironmentInfo</a> </p> </li> </ul>
retrieveEnvironmentInfo :: forall eff. RetrieveEnvironmentInfoMessage -> Aff (err :: AWS.RequestError | eff) RetrieveEnvironmentInfoResultMessage
retrieveEnvironmentInfo = AWS.request serviceName "retrieveEnvironmentInfo" 


-- | <p>Swaps the CNAMEs of two environments.</p>
swapEnvironmentCNAMEs :: forall eff. SwapEnvironmentCNAMEsMessage -> Aff (err :: AWS.RequestError | eff) Unit
swapEnvironmentCNAMEs = AWS.request serviceName "swapEnvironmentCNAMEs" 


-- | <p>Terminates the specified environment.</p>
terminateEnvironment :: forall eff. TerminateEnvironmentMessage -> Aff (err :: AWS.RequestError | eff) EnvironmentDescription
terminateEnvironment = AWS.request serviceName "terminateEnvironment" 


-- | <p>Updates the specified application to have the specified properties.</p> <note> <p>If a property (for example, <code>description</code>) is not provided, the value remains unchanged. To clear these properties, specify an empty string.</p> </note>
updateApplication :: forall eff. UpdateApplicationMessage -> Aff (err :: AWS.RequestError | eff) ApplicationDescriptionMessage
updateApplication = AWS.request serviceName "updateApplication" 


-- | <p>Modifies lifecycle settings for an application.</p>
updateApplicationResourceLifecycle :: forall eff. UpdateApplicationResourceLifecycleMessage -> Aff (err :: AWS.RequestError | eff) ApplicationResourceLifecycleDescriptionMessage
updateApplicationResourceLifecycle = AWS.request serviceName "updateApplicationResourceLifecycle" 


-- | <p>Updates the specified application version to have the specified properties.</p> <note> <p>If a property (for example, <code>description</code>) is not provided, the value remains unchanged. To clear properties, specify an empty string.</p> </note>
updateApplicationVersion :: forall eff. UpdateApplicationVersionMessage -> Aff (err :: AWS.RequestError | eff) ApplicationVersionDescriptionMessage
updateApplicationVersion = AWS.request serviceName "updateApplicationVersion" 


-- | <p>Updates the specified configuration template to have the specified properties or configuration option values.</p> <note> <p>If a property (for example, <code>ApplicationName</code>) is not provided, its value remains unchanged. To clear such properties, specify an empty string.</p> </note> <p>Related Topics</p> <ul> <li> <p> <a>DescribeConfigurationOptions</a> </p> </li> </ul>
updateConfigurationTemplate :: forall eff. UpdateConfigurationTemplateMessage -> Aff (err :: AWS.RequestError | eff) ConfigurationSettingsDescription
updateConfigurationTemplate = AWS.request serviceName "updateConfigurationTemplate" 


-- | <p>Updates the environment description, deploys a new application version, updates the configuration settings to an entirely new configuration template, or updates select configuration option values in the running environment.</p> <p> Attempting to update both the release and configuration is not allowed and AWS Elastic Beanstalk returns an <code>InvalidParameterCombination</code> error. </p> <p> When updating the configuration settings to a new template or individual settings, a draft configuration is created and <a>DescribeConfigurationSettings</a> for this environment returns two setting descriptions with different <code>DeploymentStatus</code> values. </p>
updateEnvironment :: forall eff. UpdateEnvironmentMessage -> Aff (err :: AWS.RequestError | eff) EnvironmentDescription
updateEnvironment = AWS.request serviceName "updateEnvironment" 


-- | <p>Update the list of tags applied to an AWS Elastic Beanstalk resource. Two lists can be passed: <code>TagsToAdd</code> for tags to add or update, and <code>TagsToRemove</code>.</p> <p>Currently, Elastic Beanstalk only supports tagging of Elastic Beanstalk environments. For details about environment tagging, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.tagging.html">Tagging Resources in Your Elastic Beanstalk Environment</a>.</p> <p>If you create a custom IAM user policy to control permission to this operation, specify one of the following two virtual actions (or both) instead of the API operation name:</p> <dl> <dt>elasticbeanstalk:AddTags</dt> <dd> <p>Controls permission to call <code>UpdateTagsForResource</code> and pass a list of tags to add in the <code>TagsToAdd</code> parameter.</p> </dd> <dt>elasticbeanstalk:RemoveTags</dt> <dd> <p>Controls permission to call <code>UpdateTagsForResource</code> and pass a list of tag keys to remove in the <code>TagsToRemove</code> parameter.</p> </dd> </dl> <p>For details about creating a custom user policy, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/AWSHowTo.iam.managed-policies.html#AWSHowTo.iam.policies">Creating a Custom User Policy</a>.</p>
updateTagsForResource :: forall eff. UpdateTagsForResourceMessage -> Aff (err :: AWS.RequestError | eff) Unit
updateTagsForResource = AWS.request serviceName "updateTagsForResource" 


-- | <p>Takes a set of configuration settings and either a configuration template or environment, and determines whether those values are valid.</p> <p>This action returns a list of messages indicating any errors or warnings associated with the selection of option values.</p>
validateConfigurationSettings :: forall eff. ValidateConfigurationSettingsMessage -> Aff (err :: AWS.RequestError | eff) ConfigurationSettingsValidationMessages
validateConfigurationSettings = AWS.request serviceName "validateConfigurationSettings" 


newtype ARN = ARN String
derive instance newtypeARN :: Newtype ARN _


-- | <p/>
newtype AbortEnvironmentUpdateMessage = AbortEnvironmentUpdateMessage 
  { "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  }
derive instance newtypeAbortEnvironmentUpdateMessage :: Newtype AbortEnvironmentUpdateMessage _


newtype AbortableOperationInProgress = AbortableOperationInProgress Boolean
derive instance newtypeAbortableOperationInProgress :: Newtype AbortableOperationInProgress _


newtype ActionHistoryStatus = ActionHistoryStatus String
derive instance newtypeActionHistoryStatus :: Newtype ActionHistoryStatus _


newtype ActionStatus = ActionStatus String
derive instance newtypeActionStatus :: Newtype ActionStatus _


newtype ActionType = ActionType String
derive instance newtypeActionType :: Newtype ActionType _


-- | <p>Describes the properties of an application.</p>
newtype ApplicationDescription = ApplicationDescription 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "Description" :: NullOrUndefined (Description)
  , "DateCreated" :: NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined (UpdateDate)
  , "Versions" :: NullOrUndefined (VersionLabelsList)
  , "ConfigurationTemplates" :: NullOrUndefined (ConfigurationTemplateNamesList)
  , "ResourceLifecycleConfig" :: NullOrUndefined (ApplicationResourceLifecycleConfig)
  }
derive instance newtypeApplicationDescription :: Newtype ApplicationDescription _


newtype ApplicationDescriptionList = ApplicationDescriptionList (Array ApplicationDescription)
derive instance newtypeApplicationDescriptionList :: Newtype ApplicationDescriptionList _


-- | <p>Result message containing a single description of an application.</p>
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage 
  { "Application" :: NullOrUndefined (ApplicationDescription)
  }
derive instance newtypeApplicationDescriptionMessage :: Newtype ApplicationDescriptionMessage _


-- | <p>Result message containing a list of application descriptions.</p>
newtype ApplicationDescriptionsMessage = ApplicationDescriptionsMessage 
  { "Applications" :: NullOrUndefined (ApplicationDescriptionList)
  }
derive instance newtypeApplicationDescriptionsMessage :: Newtype ApplicationDescriptionsMessage _


-- | <p>Application request metrics for an AWS Elastic Beanstalk environment.</p>
newtype ApplicationMetrics = ApplicationMetrics 
  { "Duration" :: NullOrUndefined (NullableInteger)
  , "RequestCount" :: NullOrUndefined (RequestCount)
  , "StatusCodes" :: NullOrUndefined (StatusCodes)
  , "Latency" :: NullOrUndefined (Latency)
  }
derive instance newtypeApplicationMetrics :: Newtype ApplicationMetrics _


newtype ApplicationName = ApplicationName String
derive instance newtypeApplicationName :: Newtype ApplicationName _


newtype ApplicationNamesList = ApplicationNamesList (Array ApplicationName)
derive instance newtypeApplicationNamesList :: Newtype ApplicationNamesList _


-- | <p>The resource lifecycle configuration for an application. Defines lifecycle settings for resources that belong to the application, and the service role that Elastic Beanstalk assumes in order to apply lifecycle settings. The version lifecycle configuration defines lifecycle settings for application versions.</p>
newtype ApplicationResourceLifecycleConfig = ApplicationResourceLifecycleConfig 
  { "ServiceRole" :: NullOrUndefined (String)
  , "VersionLifecycleConfig" :: NullOrUndefined (ApplicationVersionLifecycleConfig)
  }
derive instance newtypeApplicationResourceLifecycleConfig :: Newtype ApplicationResourceLifecycleConfig _


newtype ApplicationResourceLifecycleDescriptionMessage = ApplicationResourceLifecycleDescriptionMessage 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "ResourceLifecycleConfig" :: NullOrUndefined (ApplicationResourceLifecycleConfig)
  }
derive instance newtypeApplicationResourceLifecycleDescriptionMessage :: Newtype ApplicationResourceLifecycleDescriptionMessage _


-- | <p>Describes the properties of an application version.</p>
newtype ApplicationVersionDescription = ApplicationVersionDescription 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "Description" :: NullOrUndefined (Description)
  , "VersionLabel" :: NullOrUndefined (VersionLabel)
  , "SourceBuildInformation" :: NullOrUndefined (SourceBuildInformation)
  , "BuildArn" :: NullOrUndefined (String)
  , "SourceBundle" :: NullOrUndefined (S3Location)
  , "DateCreated" :: NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined (UpdateDate)
  , "Status" :: NullOrUndefined (ApplicationVersionStatus)
  }
derive instance newtypeApplicationVersionDescription :: Newtype ApplicationVersionDescription _


newtype ApplicationVersionDescriptionList = ApplicationVersionDescriptionList (Array ApplicationVersionDescription)
derive instance newtypeApplicationVersionDescriptionList :: Newtype ApplicationVersionDescriptionList _


-- | <p>Result message wrapping a single description of an application version.</p>
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage 
  { "ApplicationVersion" :: NullOrUndefined (ApplicationVersionDescription)
  }
derive instance newtypeApplicationVersionDescriptionMessage :: Newtype ApplicationVersionDescriptionMessage _


-- | <p>Result message wrapping a list of application version descriptions.</p>
newtype ApplicationVersionDescriptionsMessage = ApplicationVersionDescriptionsMessage 
  { "ApplicationVersions" :: NullOrUndefined (ApplicationVersionDescriptionList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeApplicationVersionDescriptionsMessage :: Newtype ApplicationVersionDescriptionsMessage _


-- | <p>The application version lifecycle settings for an application. Defines the rules that Elastic Beanstalk applies to an application's versions in order to avoid hitting the per-region limit for application versions.</p> <p>When Elastic Beanstalk deletes an application version from its database, you can no longer deploy that version to an environment. The source bundle remains in S3 unless you configure the rule to delete it.</p>
newtype ApplicationVersionLifecycleConfig = ApplicationVersionLifecycleConfig 
  { "MaxCountRule" :: NullOrUndefined (MaxCountRule)
  , "MaxAgeRule" :: NullOrUndefined (MaxAgeRule)
  }
derive instance newtypeApplicationVersionLifecycleConfig :: Newtype ApplicationVersionLifecycleConfig _


newtype ApplicationVersionProccess = ApplicationVersionProccess Boolean
derive instance newtypeApplicationVersionProccess :: Newtype ApplicationVersionProccess _


newtype ApplicationVersionStatus = ApplicationVersionStatus String
derive instance newtypeApplicationVersionStatus :: Newtype ApplicationVersionStatus _


-- | <p>Request to execute a scheduled managed action immediately.</p>
newtype ApplyEnvironmentManagedActionRequest = ApplyEnvironmentManagedActionRequest 
  { "EnvironmentName" :: NullOrUndefined (String)
  , "EnvironmentId" :: NullOrUndefined (String)
  , "ActionId" :: (String)
  }
derive instance newtypeApplyEnvironmentManagedActionRequest :: Newtype ApplyEnvironmentManagedActionRequest _


-- | <p>The result message containing information about the managed action.</p>
newtype ApplyEnvironmentManagedActionResult = ApplyEnvironmentManagedActionResult 
  { "ActionId" :: NullOrUndefined (String)
  , "ActionDescription" :: NullOrUndefined (String)
  , "ActionType" :: NullOrUndefined (ActionType)
  , "Status" :: NullOrUndefined (String)
  }
derive instance newtypeApplyEnvironmentManagedActionResult :: Newtype ApplyEnvironmentManagedActionResult _


newtype AutoCreateApplication = AutoCreateApplication Boolean
derive instance newtypeAutoCreateApplication :: Newtype AutoCreateApplication _


-- | <p>Describes an Auto Scaling launch configuration.</p>
newtype AutoScalingGroup = AutoScalingGroup 
  { "Name" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeAutoScalingGroup :: Newtype AutoScalingGroup _


newtype AutoScalingGroupList = AutoScalingGroupList (Array AutoScalingGroup)
derive instance newtypeAutoScalingGroupList :: Newtype AutoScalingGroupList _


newtype AvailableSolutionStackDetailsList = AvailableSolutionStackDetailsList (Array SolutionStackDescription)
derive instance newtypeAvailableSolutionStackDetailsList :: Newtype AvailableSolutionStackDetailsList _


newtype AvailableSolutionStackNamesList = AvailableSolutionStackNamesList (Array SolutionStackName)
derive instance newtypeAvailableSolutionStackNamesList :: Newtype AvailableSolutionStackNamesList _


newtype BoxedBoolean = BoxedBoolean Boolean
derive instance newtypeBoxedBoolean :: Newtype BoxedBoolean _


newtype BoxedInt = BoxedInt Int
derive instance newtypeBoxedInt :: Newtype BoxedInt _


-- | <p>Settings for an AWS CodeBuild build.</p>
newtype BuildConfiguration = BuildConfiguration 
  { "ArtifactName" :: NullOrUndefined (String)
  , "CodeBuildServiceRole" :: (NonEmptyString)
  , "ComputeType" :: NullOrUndefined (ComputeType)
  , "Image" :: (NonEmptyString)
  , "TimeoutInMinutes" :: NullOrUndefined (BoxedInt)
  }
derive instance newtypeBuildConfiguration :: Newtype BuildConfiguration _


-- | <p>The builder used to build the custom platform.</p>
newtype Builder = Builder 
  { "ARN" :: NullOrUndefined (ARN)
  }
derive instance newtypeBuilder :: Newtype Builder _


-- | <p>CPU utilization metrics for an instance.</p>
newtype CPUUtilization = CPUUtilization 
  { "User" :: NullOrUndefined (NullableDouble)
  , "Nice" :: NullOrUndefined (NullableDouble)
  , "System" :: NullOrUndefined (NullableDouble)
  , "Idle" :: NullOrUndefined (NullableDouble)
  , "IOWait" :: NullOrUndefined (NullableDouble)
  , "IRQ" :: NullOrUndefined (NullableDouble)
  , "SoftIRQ" :: NullOrUndefined (NullableDouble)
  }
derive instance newtypeCPUUtilization :: Newtype CPUUtilization _


newtype Cause = Cause String
derive instance newtypeCause :: Newtype Cause _


newtype Causes = Causes (Array Cause)
derive instance newtypeCauses :: Newtype Causes _


-- | <p>Results message indicating whether a CNAME is available.</p>
newtype CheckDNSAvailabilityMessage = CheckDNSAvailabilityMessage 
  { "CNAMEPrefix" :: (DNSCnamePrefix)
  }
derive instance newtypeCheckDNSAvailabilityMessage :: Newtype CheckDNSAvailabilityMessage _


-- | <p>Indicates if the specified CNAME is available.</p>
newtype CheckDNSAvailabilityResultMessage = CheckDNSAvailabilityResultMessage 
  { "Available" :: NullOrUndefined (CnameAvailability)
  , "FullyQualifiedCNAME" :: NullOrUndefined (DNSCname)
  }
derive instance newtypeCheckDNSAvailabilityResultMessage :: Newtype CheckDNSAvailabilityResultMessage _


newtype CnameAvailability = CnameAvailability Boolean
derive instance newtypeCnameAvailability :: Newtype CnameAvailability _


-- | <p>AWS CodeBuild is not available in the specified region.</p>
newtype CodeBuildNotInServiceRegionException = CodeBuildNotInServiceRegionException 
  { 
  }
derive instance newtypeCodeBuildNotInServiceRegionException :: Newtype CodeBuildNotInServiceRegionException _


-- | <p>Request to create or update a group of environments.</p>
newtype ComposeEnvironmentsMessage = ComposeEnvironmentsMessage 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "GroupName" :: NullOrUndefined (GroupName)
  , "VersionLabels" :: NullOrUndefined (VersionLabels)
  }
derive instance newtypeComposeEnvironmentsMessage :: Newtype ComposeEnvironmentsMessage _


newtype ComputeType = ComputeType String
derive instance newtypeComputeType :: Newtype ComputeType _


newtype ConfigurationDeploymentStatus = ConfigurationDeploymentStatus String
derive instance newtypeConfigurationDeploymentStatus :: Newtype ConfigurationDeploymentStatus _


newtype ConfigurationOptionDefaultValue = ConfigurationOptionDefaultValue String
derive instance newtypeConfigurationOptionDefaultValue :: Newtype ConfigurationOptionDefaultValue _


-- | <p>Describes the possible values for a configuration option.</p>
newtype ConfigurationOptionDescription = ConfigurationOptionDescription 
  { "Namespace" :: NullOrUndefined (OptionNamespace)
  , "Name" :: NullOrUndefined (ConfigurationOptionName)
  , "DefaultValue" :: NullOrUndefined (ConfigurationOptionDefaultValue)
  , "ChangeSeverity" :: NullOrUndefined (ConfigurationOptionSeverity)
  , "UserDefined" :: NullOrUndefined (UserDefinedOption)
  , "ValueType" :: NullOrUndefined (ConfigurationOptionValueType)
  , "ValueOptions" :: NullOrUndefined (ConfigurationOptionPossibleValues)
  , "MinValue" :: NullOrUndefined (OptionRestrictionMinValue)
  , "MaxValue" :: NullOrUndefined (OptionRestrictionMaxValue)
  , "MaxLength" :: NullOrUndefined (OptionRestrictionMaxLength)
  , "Regex" :: NullOrUndefined (OptionRestrictionRegex)
  }
derive instance newtypeConfigurationOptionDescription :: Newtype ConfigurationOptionDescription _


newtype ConfigurationOptionDescriptionsList = ConfigurationOptionDescriptionsList (Array ConfigurationOptionDescription)
derive instance newtypeConfigurationOptionDescriptionsList :: Newtype ConfigurationOptionDescriptionsList _


newtype ConfigurationOptionName = ConfigurationOptionName String
derive instance newtypeConfigurationOptionName :: Newtype ConfigurationOptionName _


newtype ConfigurationOptionPossibleValue = ConfigurationOptionPossibleValue String
derive instance newtypeConfigurationOptionPossibleValue :: Newtype ConfigurationOptionPossibleValue _


newtype ConfigurationOptionPossibleValues = ConfigurationOptionPossibleValues (Array ConfigurationOptionPossibleValue)
derive instance newtypeConfigurationOptionPossibleValues :: Newtype ConfigurationOptionPossibleValues _


-- | <p> A specification identifying an individual configuration option along with its current value. For a list of possible option values, go to <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html">Option Values</a> in the <i>AWS Elastic Beanstalk Developer Guide</i>. </p>
newtype ConfigurationOptionSetting = ConfigurationOptionSetting 
  { "ResourceName" :: NullOrUndefined (ResourceName)
  , "Namespace" :: NullOrUndefined (OptionNamespace)
  , "OptionName" :: NullOrUndefined (ConfigurationOptionName)
  , "Value" :: NullOrUndefined (ConfigurationOptionValue)
  }
derive instance newtypeConfigurationOptionSetting :: Newtype ConfigurationOptionSetting _


newtype ConfigurationOptionSettingsList = ConfigurationOptionSettingsList (Array ConfigurationOptionSetting)
derive instance newtypeConfigurationOptionSettingsList :: Newtype ConfigurationOptionSettingsList _


newtype ConfigurationOptionSeverity = ConfigurationOptionSeverity String
derive instance newtypeConfigurationOptionSeverity :: Newtype ConfigurationOptionSeverity _


newtype ConfigurationOptionValue = ConfigurationOptionValue String
derive instance newtypeConfigurationOptionValue :: Newtype ConfigurationOptionValue _


newtype ConfigurationOptionValueType = ConfigurationOptionValueType String
derive instance newtypeConfigurationOptionValueType :: Newtype ConfigurationOptionValueType _


-- | <p>Describes the settings for a specified configuration set.</p>
newtype ConfigurationOptionsDescription = ConfigurationOptionsDescription 
  { "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "Options" :: NullOrUndefined (ConfigurationOptionDescriptionsList)
  }
derive instance newtypeConfigurationOptionsDescription :: Newtype ConfigurationOptionsDescription _


-- | <p>Describes the settings for a configuration set.</p>
newtype ConfigurationSettingsDescription = ConfigurationSettingsDescription 
  { "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "Description" :: NullOrUndefined (Description)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "DeploymentStatus" :: NullOrUndefined (ConfigurationDeploymentStatus)
  , "DateCreated" :: NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined (UpdateDate)
  , "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList)
  }
derive instance newtypeConfigurationSettingsDescription :: Newtype ConfigurationSettingsDescription _


newtype ConfigurationSettingsDescriptionList = ConfigurationSettingsDescriptionList (Array ConfigurationSettingsDescription)
derive instance newtypeConfigurationSettingsDescriptionList :: Newtype ConfigurationSettingsDescriptionList _


-- | <p>The results from a request to change the configuration settings of an environment.</p>
newtype ConfigurationSettingsDescriptions = ConfigurationSettingsDescriptions 
  { "ConfigurationSettings" :: NullOrUndefined (ConfigurationSettingsDescriptionList)
  }
derive instance newtypeConfigurationSettingsDescriptions :: Newtype ConfigurationSettingsDescriptions _


-- | <p>Provides a list of validation messages.</p>
newtype ConfigurationSettingsValidationMessages = ConfigurationSettingsValidationMessages 
  { "Messages" :: NullOrUndefined (ValidationMessagesList)
  }
derive instance newtypeConfigurationSettingsValidationMessages :: Newtype ConfigurationSettingsValidationMessages _


newtype ConfigurationTemplateName = ConfigurationTemplateName String
derive instance newtypeConfigurationTemplateName :: Newtype ConfigurationTemplateName _


newtype ConfigurationTemplateNamesList = ConfigurationTemplateNamesList (Array ConfigurationTemplateName)
derive instance newtypeConfigurationTemplateNamesList :: Newtype ConfigurationTemplateNamesList _


-- | <p>Request to create an application.</p>
newtype CreateApplicationMessage = CreateApplicationMessage 
  { "ApplicationName" :: (ApplicationName)
  , "Description" :: NullOrUndefined (Description)
  , "ResourceLifecycleConfig" :: NullOrUndefined (ApplicationResourceLifecycleConfig)
  }
derive instance newtypeCreateApplicationMessage :: Newtype CreateApplicationMessage _


-- | <p/>
newtype CreateApplicationVersionMessage = CreateApplicationVersionMessage 
  { "ApplicationName" :: (ApplicationName)
  , "VersionLabel" :: (VersionLabel)
  , "Description" :: NullOrUndefined (Description)
  , "SourceBuildInformation" :: NullOrUndefined (SourceBuildInformation)
  , "SourceBundle" :: NullOrUndefined (S3Location)
  , "BuildConfiguration" :: NullOrUndefined (BuildConfiguration)
  , "AutoCreateApplication" :: NullOrUndefined (AutoCreateApplication)
  , "Process" :: NullOrUndefined (ApplicationVersionProccess)
  }
derive instance newtypeCreateApplicationVersionMessage :: Newtype CreateApplicationVersionMessage _


-- | <p>Request to create a configuration template.</p>
newtype CreateConfigurationTemplateMessage = CreateConfigurationTemplateMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: (ConfigurationTemplateName)
  , "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "SourceConfiguration" :: NullOrUndefined (SourceConfiguration)
  , "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "Description" :: NullOrUndefined (Description)
  , "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList)
  }
derive instance newtypeCreateConfigurationTemplateMessage :: Newtype CreateConfigurationTemplateMessage _


-- | <p/>
newtype CreateEnvironmentMessage = CreateEnvironmentMessage 
  { "ApplicationName" :: (ApplicationName)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "GroupName" :: NullOrUndefined (GroupName)
  , "Description" :: NullOrUndefined (Description)
  , "CNAMEPrefix" :: NullOrUndefined (DNSCnamePrefix)
  , "Tier" :: NullOrUndefined (EnvironmentTier)
  , "Tags" :: NullOrUndefined (Tags)
  , "VersionLabel" :: NullOrUndefined (VersionLabel)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList)
  , "OptionsToRemove" :: NullOrUndefined (OptionsSpecifierList)
  }
derive instance newtypeCreateEnvironmentMessage :: Newtype CreateEnvironmentMessage _


-- | <p>Request to create a new platform version.</p>
newtype CreatePlatformVersionRequest = CreatePlatformVersionRequest 
  { "PlatformName" :: (PlatformName)
  , "PlatformVersion" :: (PlatformVersion)
  , "PlatformDefinitionBundle" :: (S3Location)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList)
  }
derive instance newtypeCreatePlatformVersionRequest :: Newtype CreatePlatformVersionRequest _


newtype CreatePlatformVersionResult = CreatePlatformVersionResult 
  { "PlatformSummary" :: NullOrUndefined (PlatformSummary)
  , "Builder" :: NullOrUndefined (Builder)
  }
derive instance newtypeCreatePlatformVersionResult :: Newtype CreatePlatformVersionResult _


-- | <p>Results of a <a>CreateStorageLocationResult</a> call.</p>
newtype CreateStorageLocationResultMessage = CreateStorageLocationResultMessage 
  { "S3Bucket" :: NullOrUndefined (S3Bucket)
  }
derive instance newtypeCreateStorageLocationResultMessage :: Newtype CreateStorageLocationResultMessage _


newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _


-- | <p>A custom AMI available to platforms.</p>
newtype CustomAmi = CustomAmi 
  { "VirtualizationType" :: NullOrUndefined (VirtualizationType)
  , "ImageId" :: NullOrUndefined (ImageId)
  }
derive instance newtypeCustomAmi :: Newtype CustomAmi _


newtype CustomAmiList = CustomAmiList (Array CustomAmi)
derive instance newtypeCustomAmiList :: Newtype CustomAmiList _


newtype DNSCname = DNSCname String
derive instance newtypeDNSCname :: Newtype DNSCname _


newtype DNSCnamePrefix = DNSCnamePrefix String
derive instance newtypeDNSCnamePrefix :: Newtype DNSCnamePrefix _


-- | <p>Request to delete an application.</p>
newtype DeleteApplicationMessage = DeleteApplicationMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TerminateEnvByForce" :: NullOrUndefined (TerminateEnvForce)
  }
derive instance newtypeDeleteApplicationMessage :: Newtype DeleteApplicationMessage _


-- | <p>Request to delete an application version.</p>
newtype DeleteApplicationVersionMessage = DeleteApplicationVersionMessage 
  { "ApplicationName" :: (ApplicationName)
  , "VersionLabel" :: (VersionLabel)
  , "DeleteSourceBundle" :: NullOrUndefined (DeleteSourceBundle)
  }
derive instance newtypeDeleteApplicationVersionMessage :: Newtype DeleteApplicationVersionMessage _


-- | <p>Request to delete a configuration template.</p>
newtype DeleteConfigurationTemplateMessage = DeleteConfigurationTemplateMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: (ConfigurationTemplateName)
  }
derive instance newtypeDeleteConfigurationTemplateMessage :: Newtype DeleteConfigurationTemplateMessage _


-- | <p>Request to delete a draft environment configuration.</p>
newtype DeleteEnvironmentConfigurationMessage = DeleteEnvironmentConfigurationMessage 
  { "ApplicationName" :: (ApplicationName)
  , "EnvironmentName" :: (EnvironmentName)
  }
derive instance newtypeDeleteEnvironmentConfigurationMessage :: Newtype DeleteEnvironmentConfigurationMessage _


newtype DeletePlatformVersionRequest = DeletePlatformVersionRequest 
  { "PlatformArn" :: NullOrUndefined (PlatformArn)
  }
derive instance newtypeDeletePlatformVersionRequest :: Newtype DeletePlatformVersionRequest _


newtype DeletePlatformVersionResult = DeletePlatformVersionResult 
  { "PlatformSummary" :: NullOrUndefined (PlatformSummary)
  }
derive instance newtypeDeletePlatformVersionResult :: Newtype DeletePlatformVersionResult _


newtype DeleteSourceBundle = DeleteSourceBundle Boolean
derive instance newtypeDeleteSourceBundle :: Newtype DeleteSourceBundle _


-- | <p>Information about an application version deployment.</p>
newtype Deployment = Deployment 
  { "VersionLabel" :: NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined (NullableLong)
  , "Status" :: NullOrUndefined (String)
  , "DeploymentTime" :: NullOrUndefined (DeploymentTimestamp)
  }
derive instance newtypeDeployment :: Newtype Deployment _


newtype DeploymentTimestamp = DeploymentTimestamp Number
derive instance newtypeDeploymentTimestamp :: Newtype DeploymentTimestamp _


-- | <p>Request to describe application versions.</p>
newtype DescribeApplicationVersionsMessage = DescribeApplicationVersionsMessage 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "VersionLabels" :: NullOrUndefined (VersionLabelsList)
  , "MaxRecords" :: NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeDescribeApplicationVersionsMessage :: Newtype DescribeApplicationVersionsMessage _


-- | <p>Request to describe one or more applications.</p>
newtype DescribeApplicationsMessage = DescribeApplicationsMessage 
  { "ApplicationNames" :: NullOrUndefined (ApplicationNamesList)
  }
derive instance newtypeDescribeApplicationsMessage :: Newtype DescribeApplicationsMessage _


-- | <p>Result message containing a list of application version descriptions.</p>
newtype DescribeConfigurationOptionsMessage = DescribeConfigurationOptionsMessage 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "Options" :: NullOrUndefined (OptionsSpecifierList)
  }
derive instance newtypeDescribeConfigurationOptionsMessage :: Newtype DescribeConfigurationOptionsMessage _


-- | <p>Result message containing all of the configuration settings for a specified solution stack or configuration template.</p>
newtype DescribeConfigurationSettingsMessage = DescribeConfigurationSettingsMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  }
derive instance newtypeDescribeConfigurationSettingsMessage :: Newtype DescribeConfigurationSettingsMessage _


-- | <p>See the example below to learn how to create a request body.</p>
newtype DescribeEnvironmentHealthRequest = DescribeEnvironmentHealthRequest 
  { "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "AttributeNames" :: NullOrUndefined (EnvironmentHealthAttributes)
  }
derive instance newtypeDescribeEnvironmentHealthRequest :: Newtype DescribeEnvironmentHealthRequest _


-- | <p>Health details for an AWS Elastic Beanstalk environment.</p>
newtype DescribeEnvironmentHealthResult = DescribeEnvironmentHealthResult 
  { "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "HealthStatus" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (EnvironmentHealth)
  , "Color" :: NullOrUndefined (String)
  , "Causes" :: NullOrUndefined (Causes)
  , "ApplicationMetrics" :: NullOrUndefined (ApplicationMetrics)
  , "InstancesHealth" :: NullOrUndefined (InstanceHealthSummary)
  , "RefreshedAt" :: NullOrUndefined (RefreshedAt)
  }
derive instance newtypeDescribeEnvironmentHealthResult :: Newtype DescribeEnvironmentHealthResult _


-- | <p>Request to list completed and failed managed actions.</p>
newtype DescribeEnvironmentManagedActionHistoryRequest = DescribeEnvironmentManagedActionHistoryRequest 
  { "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined (Int)
  }
derive instance newtypeDescribeEnvironmentManagedActionHistoryRequest :: Newtype DescribeEnvironmentManagedActionHistoryRequest _


-- | <p>A result message containing a list of completed and failed managed actions.</p>
newtype DescribeEnvironmentManagedActionHistoryResult = DescribeEnvironmentManagedActionHistoryResult 
  { "ManagedActionHistoryItems" :: NullOrUndefined (ManagedActionHistoryItems)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEnvironmentManagedActionHistoryResult :: Newtype DescribeEnvironmentManagedActionHistoryResult _


-- | <p>Request to list an environment's upcoming and in-progress managed actions.</p>
newtype DescribeEnvironmentManagedActionsRequest = DescribeEnvironmentManagedActionsRequest 
  { "EnvironmentName" :: NullOrUndefined (String)
  , "EnvironmentId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (ActionStatus)
  }
derive instance newtypeDescribeEnvironmentManagedActionsRequest :: Newtype DescribeEnvironmentManagedActionsRequest _


-- | <p>The result message containing a list of managed actions.</p>
newtype DescribeEnvironmentManagedActionsResult = DescribeEnvironmentManagedActionsResult 
  { "ManagedActions" :: NullOrUndefined (ManagedActions)
  }
derive instance newtypeDescribeEnvironmentManagedActionsResult :: Newtype DescribeEnvironmentManagedActionsResult _


-- | <p>Request to describe the resources in an environment.</p>
newtype DescribeEnvironmentResourcesMessage = DescribeEnvironmentResourcesMessage 
  { "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  }
derive instance newtypeDescribeEnvironmentResourcesMessage :: Newtype DescribeEnvironmentResourcesMessage _


-- | <p>Request to describe one or more environments.</p>
newtype DescribeEnvironmentsMessage = DescribeEnvironmentsMessage 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "VersionLabel" :: NullOrUndefined (VersionLabel)
  , "EnvironmentIds" :: NullOrUndefined (EnvironmentIdList)
  , "EnvironmentNames" :: NullOrUndefined (EnvironmentNamesList)
  , "IncludeDeleted" :: NullOrUndefined (IncludeDeleted)
  , "IncludedDeletedBackTo" :: NullOrUndefined (IncludeDeletedBackTo)
  , "MaxRecords" :: NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeDescribeEnvironmentsMessage :: Newtype DescribeEnvironmentsMessage _


-- | <p>Request to retrieve a list of events for an environment.</p>
newtype DescribeEventsMessage = DescribeEventsMessage 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "VersionLabel" :: NullOrUndefined (VersionLabel)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "RequestId" :: NullOrUndefined (RequestId)
  , "Severity" :: NullOrUndefined (EventSeverity)
  , "StartTime" :: NullOrUndefined (TimeFilterStart)
  , "EndTime" :: NullOrUndefined (TimeFilterEnd)
  , "MaxRecords" :: NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeDescribeEventsMessage :: Newtype DescribeEventsMessage _


-- | <p>Parameters for a call to <code>DescribeInstancesHealth</code>.</p>
newtype DescribeInstancesHealthRequest = DescribeInstancesHealthRequest 
  { "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "AttributeNames" :: NullOrUndefined (InstancesHealthAttributes)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancesHealthRequest :: Newtype DescribeInstancesHealthRequest _


-- | <p>Detailed health information about the Amazon EC2 instances in an AWS Elastic Beanstalk environment.</p>
newtype DescribeInstancesHealthResult = DescribeInstancesHealthResult 
  { "InstanceHealthList" :: NullOrUndefined (InstanceHealthList)
  , "RefreshedAt" :: NullOrUndefined (RefreshedAt)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancesHealthResult :: Newtype DescribeInstancesHealthResult _


newtype DescribePlatformVersionRequest = DescribePlatformVersionRequest 
  { "PlatformArn" :: NullOrUndefined (PlatformArn)
  }
derive instance newtypeDescribePlatformVersionRequest :: Newtype DescribePlatformVersionRequest _


newtype DescribePlatformVersionResult = DescribePlatformVersionResult 
  { "PlatformDescription" :: NullOrUndefined (PlatformDescription)
  }
derive instance newtypeDescribePlatformVersionResult :: Newtype DescribePlatformVersionResult _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


newtype Ec2InstanceId = Ec2InstanceId String
derive instance newtypeEc2InstanceId :: Newtype Ec2InstanceId _


-- | <p>A generic service exception has occurred.</p>
newtype ElasticBeanstalkServiceException = ElasticBeanstalkServiceException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeElasticBeanstalkServiceException :: Newtype ElasticBeanstalkServiceException _


newtype EndpointURL = EndpointURL String
derive instance newtypeEndpointURL :: Newtype EndpointURL _


newtype EnvironmentArn = EnvironmentArn String
derive instance newtypeEnvironmentArn :: Newtype EnvironmentArn _


-- | <p>Describes the properties of an environment.</p>
newtype EnvironmentDescription = EnvironmentDescription 
  { "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "VersionLabel" :: NullOrUndefined (VersionLabel)
  , "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "Description" :: NullOrUndefined (Description)
  , "EndpointURL" :: NullOrUndefined (EndpointURL)
  , "CNAME" :: NullOrUndefined (DNSCname)
  , "DateCreated" :: NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined (UpdateDate)
  , "Status" :: NullOrUndefined (EnvironmentStatus)
  , "AbortableOperationInProgress" :: NullOrUndefined (AbortableOperationInProgress)
  , "Health" :: NullOrUndefined (EnvironmentHealth)
  , "HealthStatus" :: NullOrUndefined (EnvironmentHealthStatus)
  , "Resources" :: NullOrUndefined (EnvironmentResourcesDescription)
  , "Tier" :: NullOrUndefined (EnvironmentTier)
  , "EnvironmentLinks" :: NullOrUndefined (EnvironmentLinks)
  , "EnvironmentArn" :: NullOrUndefined (EnvironmentArn)
  }
derive instance newtypeEnvironmentDescription :: Newtype EnvironmentDescription _


newtype EnvironmentDescriptionsList = EnvironmentDescriptionsList (Array EnvironmentDescription)
derive instance newtypeEnvironmentDescriptionsList :: Newtype EnvironmentDescriptionsList _


-- | <p>Result message containing a list of environment descriptions.</p>
newtype EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage 
  { "Environments" :: NullOrUndefined (EnvironmentDescriptionsList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeEnvironmentDescriptionsMessage :: Newtype EnvironmentDescriptionsMessage _


newtype EnvironmentHealth = EnvironmentHealth String
derive instance newtypeEnvironmentHealth :: Newtype EnvironmentHealth _


newtype EnvironmentHealthAttribute = EnvironmentHealthAttribute String
derive instance newtypeEnvironmentHealthAttribute :: Newtype EnvironmentHealthAttribute _


newtype EnvironmentHealthAttributes = EnvironmentHealthAttributes (Array EnvironmentHealthAttribute)
derive instance newtypeEnvironmentHealthAttributes :: Newtype EnvironmentHealthAttributes _


newtype EnvironmentHealthStatus = EnvironmentHealthStatus String
derive instance newtypeEnvironmentHealthStatus :: Newtype EnvironmentHealthStatus _


newtype EnvironmentId = EnvironmentId String
derive instance newtypeEnvironmentId :: Newtype EnvironmentId _


newtype EnvironmentIdList = EnvironmentIdList (Array EnvironmentId)
derive instance newtypeEnvironmentIdList :: Newtype EnvironmentIdList _


-- | <p>The information retrieved from the Amazon EC2 instances.</p>
newtype EnvironmentInfoDescription = EnvironmentInfoDescription 
  { "InfoType" :: NullOrUndefined (EnvironmentInfoType)
  , "Ec2InstanceId" :: NullOrUndefined (Ec2InstanceId)
  , "SampleTimestamp" :: NullOrUndefined (SampleTimestamp)
  , "Message" :: NullOrUndefined (Message)
  }
derive instance newtypeEnvironmentInfoDescription :: Newtype EnvironmentInfoDescription _


newtype EnvironmentInfoDescriptionList = EnvironmentInfoDescriptionList (Array EnvironmentInfoDescription)
derive instance newtypeEnvironmentInfoDescriptionList :: Newtype EnvironmentInfoDescriptionList _


newtype EnvironmentInfoType = EnvironmentInfoType String
derive instance newtypeEnvironmentInfoType :: Newtype EnvironmentInfoType _


-- | <p>A link to another environment, defined in the environment's manifest. Links provide connection information in system properties that can be used to connect to another environment in the same group. See <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html">Environment Manifest (env.yaml)</a> for details.</p>
newtype EnvironmentLink = EnvironmentLink 
  { "LinkName" :: NullOrUndefined (String)
  , "EnvironmentName" :: NullOrUndefined (String)
  }
derive instance newtypeEnvironmentLink :: Newtype EnvironmentLink _


newtype EnvironmentLinks = EnvironmentLinks (Array EnvironmentLink)
derive instance newtypeEnvironmentLinks :: Newtype EnvironmentLinks _


newtype EnvironmentName = EnvironmentName String
derive instance newtypeEnvironmentName :: Newtype EnvironmentName _


newtype EnvironmentNamesList = EnvironmentNamesList (Array EnvironmentName)
derive instance newtypeEnvironmentNamesList :: Newtype EnvironmentNamesList _


-- | <p>Describes the AWS resources in use by this environment. This data is live.</p>
newtype EnvironmentResourceDescription = EnvironmentResourceDescription 
  { "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "AutoScalingGroups" :: NullOrUndefined (AutoScalingGroupList)
  , "Instances" :: NullOrUndefined (InstanceList)
  , "LaunchConfigurations" :: NullOrUndefined (LaunchConfigurationList)
  , "LoadBalancers" :: NullOrUndefined (LoadBalancerList)
  , "Triggers" :: NullOrUndefined (TriggerList)
  , "Queues" :: NullOrUndefined (QueueList)
  }
derive instance newtypeEnvironmentResourceDescription :: Newtype EnvironmentResourceDescription _


-- | <p>Result message containing a list of environment resource descriptions.</p>
newtype EnvironmentResourceDescriptionsMessage = EnvironmentResourceDescriptionsMessage 
  { "EnvironmentResources" :: NullOrUndefined (EnvironmentResourceDescription)
  }
derive instance newtypeEnvironmentResourceDescriptionsMessage :: Newtype EnvironmentResourceDescriptionsMessage _


-- | <p>Describes the AWS resources in use by this environment. This data is not live data.</p>
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription 
  { "LoadBalancer" :: NullOrUndefined (LoadBalancerDescription)
  }
derive instance newtypeEnvironmentResourcesDescription :: Newtype EnvironmentResourcesDescription _


newtype EnvironmentStatus = EnvironmentStatus String
derive instance newtypeEnvironmentStatus :: Newtype EnvironmentStatus _


-- | <p>Describes the properties of an environment tier</p>
newtype EnvironmentTier = EnvironmentTier 
  { "Name" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }
derive instance newtypeEnvironmentTier :: Newtype EnvironmentTier _


newtype EventDate = EventDate Number
derive instance newtypeEventDate :: Newtype EventDate _


-- | <p>Describes an event.</p>
newtype EventDescription = EventDescription 
  { "EventDate" :: NullOrUndefined (EventDate)
  , "Message" :: NullOrUndefined (EventMessage)
  , "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "VersionLabel" :: NullOrUndefined (VersionLabel)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "RequestId" :: NullOrUndefined (RequestId)
  , "Severity" :: NullOrUndefined (EventSeverity)
  }
derive instance newtypeEventDescription :: Newtype EventDescription _


newtype EventDescriptionList = EventDescriptionList (Array EventDescription)
derive instance newtypeEventDescriptionList :: Newtype EventDescriptionList _


-- | <p>Result message wrapping a list of event descriptions.</p>
newtype EventDescriptionsMessage = EventDescriptionsMessage 
  { "Events" :: NullOrUndefined (EventDescriptionList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeEventDescriptionsMessage :: Newtype EventDescriptionsMessage _


newtype EventMessage = EventMessage String
derive instance newtypeEventMessage :: Newtype EventMessage _


newtype EventSeverity = EventSeverity String
derive instance newtypeEventSeverity :: Newtype EventSeverity _


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _


newtype FailureType = FailureType String
derive instance newtypeFailureType :: Newtype FailureType _


newtype FileTypeExtension = FileTypeExtension String
derive instance newtypeFileTypeExtension :: Newtype FileTypeExtension _


newtype ForceTerminate = ForceTerminate Boolean
derive instance newtypeForceTerminate :: Newtype ForceTerminate _


newtype GroupName = GroupName String
derive instance newtypeGroupName :: Newtype GroupName _


newtype ImageId = ImageId String
derive instance newtypeImageId :: Newtype ImageId _


newtype IncludeDeleted = IncludeDeleted Boolean
derive instance newtypeIncludeDeleted :: Newtype IncludeDeleted _


newtype IncludeDeletedBackTo = IncludeDeletedBackTo Number
derive instance newtypeIncludeDeletedBackTo :: Newtype IncludeDeletedBackTo _


-- | <p>The description of an Amazon EC2 instance.</p>
newtype Instance = Instance 
  { "Id" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeInstance :: Newtype Instance _


newtype InstanceHealthList = InstanceHealthList (Array SingleInstanceHealth)
derive instance newtypeInstanceHealthList :: Newtype InstanceHealthList _


-- | <p>Represents summary information about the health of an instance. For more information, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html">Health Colors and Statuses</a>.</p>
newtype InstanceHealthSummary = InstanceHealthSummary 
  { "NoData" :: NullOrUndefined (NullableInteger)
  , "Unknown" :: NullOrUndefined (NullableInteger)
  , "Pending" :: NullOrUndefined (NullableInteger)
  , "Ok" :: NullOrUndefined (NullableInteger)
  , "Info" :: NullOrUndefined (NullableInteger)
  , "Warning" :: NullOrUndefined (NullableInteger)
  , "Degraded" :: NullOrUndefined (NullableInteger)
  , "Severe" :: NullOrUndefined (NullableInteger)
  }
derive instance newtypeInstanceHealthSummary :: Newtype InstanceHealthSummary _


newtype InstanceId = InstanceId String
derive instance newtypeInstanceId :: Newtype InstanceId _


newtype InstanceList = InstanceList (Array Instance)
derive instance newtypeInstanceList :: Newtype InstanceList _


newtype InstancesHealthAttribute = InstancesHealthAttribute String
derive instance newtypeInstancesHealthAttribute :: Newtype InstancesHealthAttribute _


newtype InstancesHealthAttributes = InstancesHealthAttributes (Array InstancesHealthAttribute)
derive instance newtypeInstancesHealthAttributes :: Newtype InstancesHealthAttributes _


-- | <p>The specified account does not have sufficient privileges for one of more AWS services.</p>
newtype InsufficientPrivilegesException = InsufficientPrivilegesException 
  { 
  }
derive instance newtypeInsufficientPrivilegesException :: Newtype InsufficientPrivilegesException _


-- | <p>One or more input parameters is not valid. Please correct the input parameters and try the operation again.</p>
newtype InvalidRequestException = InvalidRequestException 
  { 
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


-- | <p>Represents the average latency for the slowest X percent of requests over the last 10 seconds.</p>
newtype Latency = Latency 
  { "P999" :: NullOrUndefined (NullableDouble)
  , "P99" :: NullOrUndefined (NullableDouble)
  , "P95" :: NullOrUndefined (NullableDouble)
  , "P90" :: NullOrUndefined (NullableDouble)
  , "P85" :: NullOrUndefined (NullableDouble)
  , "P75" :: NullOrUndefined (NullableDouble)
  , "P50" :: NullOrUndefined (NullableDouble)
  , "P10" :: NullOrUndefined (NullableDouble)
  }
derive instance newtypeLatency :: Newtype Latency _


-- | <p>Describes an Auto Scaling launch configuration.</p>
newtype LaunchConfiguration = LaunchConfiguration 
  { "Name" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeLaunchConfiguration :: Newtype LaunchConfiguration _


newtype LaunchConfigurationList = LaunchConfigurationList (Array LaunchConfiguration)
derive instance newtypeLaunchConfigurationList :: Newtype LaunchConfigurationList _


newtype LaunchedAt = LaunchedAt Number
derive instance newtypeLaunchedAt :: Newtype LaunchedAt _


-- | <p>A list of available AWS Elastic Beanstalk solution stacks.</p>
newtype ListAvailableSolutionStacksResultMessage = ListAvailableSolutionStacksResultMessage 
  { "SolutionStacks" :: NullOrUndefined (AvailableSolutionStackNamesList)
  , "SolutionStackDetails" :: NullOrUndefined (AvailableSolutionStackDetailsList)
  }
derive instance newtypeListAvailableSolutionStacksResultMessage :: Newtype ListAvailableSolutionStacksResultMessage _


newtype ListPlatformVersionsRequest = ListPlatformVersionsRequest 
  { "Filters" :: NullOrUndefined (PlatformFilters)
  , "MaxRecords" :: NullOrUndefined (PlatformMaxRecords)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListPlatformVersionsRequest :: Newtype ListPlatformVersionsRequest _


newtype ListPlatformVersionsResult = ListPlatformVersionsResult 
  { "PlatformSummaryList" :: NullOrUndefined (PlatformSummaryList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeListPlatformVersionsResult :: Newtype ListPlatformVersionsResult _


newtype ListTagsForResourceMessage = ListTagsForResourceMessage 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeListTagsForResourceMessage :: Newtype ListTagsForResourceMessage _


-- | <p>Describes the properties of a Listener for the LoadBalancer.</p>
newtype Listener = Listener 
  { "Protocol" :: NullOrUndefined (String)
  , "Port" :: NullOrUndefined (Int)
  }
derive instance newtypeListener :: Newtype Listener _


newtype LoadAverage = LoadAverage (Array LoadAverageValue)
derive instance newtypeLoadAverage :: Newtype LoadAverage _


newtype LoadAverageValue = LoadAverageValue Number
derive instance newtypeLoadAverageValue :: Newtype LoadAverageValue _


-- | <p>Describes a LoadBalancer.</p>
newtype LoadBalancer = LoadBalancer 
  { "Name" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeLoadBalancer :: Newtype LoadBalancer _


-- | <p>Describes the details of a LoadBalancer.</p>
newtype LoadBalancerDescription = LoadBalancerDescription 
  { "LoadBalancerName" :: NullOrUndefined (String)
  , "Domain" :: NullOrUndefined (String)
  , "Listeners" :: NullOrUndefined (LoadBalancerListenersDescription)
  }
derive instance newtypeLoadBalancerDescription :: Newtype LoadBalancerDescription _


newtype LoadBalancerList = LoadBalancerList (Array LoadBalancer)
derive instance newtypeLoadBalancerList :: Newtype LoadBalancerList _


newtype LoadBalancerListenersDescription = LoadBalancerListenersDescription (Array Listener)
derive instance newtypeLoadBalancerListenersDescription :: Newtype LoadBalancerListenersDescription _


newtype Maintainer = Maintainer String
derive instance newtypeMaintainer :: Newtype Maintainer _


-- | <p>The record of an upcoming or in-progress managed action.</p>
newtype ManagedAction = ManagedAction 
  { "ActionId" :: NullOrUndefined (String)
  , "ActionDescription" :: NullOrUndefined (String)
  , "ActionType" :: NullOrUndefined (ActionType)
  , "Status" :: NullOrUndefined (ActionStatus)
  , "WindowStartTime" :: NullOrUndefined (Number)
  }
derive instance newtypeManagedAction :: Newtype ManagedAction _


-- | <p>The record of a completed or failed managed action.</p>
newtype ManagedActionHistoryItem = ManagedActionHistoryItem 
  { "ActionId" :: NullOrUndefined (String)
  , "ActionType" :: NullOrUndefined (ActionType)
  , "ActionDescription" :: NullOrUndefined (String)
  , "FailureType" :: NullOrUndefined (FailureType)
  , "Status" :: NullOrUndefined (ActionHistoryStatus)
  , "FailureDescription" :: NullOrUndefined (String)
  , "ExecutedTime" :: NullOrUndefined (Number)
  , "FinishedTime" :: NullOrUndefined (Number)
  }
derive instance newtypeManagedActionHistoryItem :: Newtype ManagedActionHistoryItem _


newtype ManagedActionHistoryItems = ManagedActionHistoryItems (Array ManagedActionHistoryItem)
derive instance newtypeManagedActionHistoryItems :: Newtype ManagedActionHistoryItems _


-- | <p>Cannot modify the managed action in its current state.</p>
newtype ManagedActionInvalidStateException = ManagedActionInvalidStateException 
  { 
  }
derive instance newtypeManagedActionInvalidStateException :: Newtype ManagedActionInvalidStateException _


newtype ManagedActions = ManagedActions (Array ManagedAction)
derive instance newtypeManagedActions :: Newtype ManagedActions _


-- | <p>A lifecycle rule that deletes application versions after the specified number of days.</p>
newtype MaxAgeRule = MaxAgeRule 
  { "Enabled" :: (BoxedBoolean)
  , "MaxAgeInDays" :: NullOrUndefined (BoxedInt)
  , "DeleteSourceFromS3" :: NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeMaxAgeRule :: Newtype MaxAgeRule _


-- | <p>A lifecycle rule that deletes the oldest application version when the maximum count is exceeded.</p>
newtype MaxCountRule = MaxCountRule 
  { "Enabled" :: (BoxedBoolean)
  , "MaxCount" :: NullOrUndefined (BoxedInt)
  , "DeleteSourceFromS3" :: NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeMaxCountRule :: Newtype MaxCountRule _


newtype MaxRecords = MaxRecords Int
derive instance newtypeMaxRecords :: Newtype MaxRecords _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _


newtype NullableDouble = NullableDouble Number
derive instance newtypeNullableDouble :: Newtype NullableDouble _


newtype NullableInteger = NullableInteger Int
derive instance newtypeNullableInteger :: Newtype NullableInteger _


newtype NullableLong = NullableLong Number
derive instance newtypeNullableLong :: Newtype NullableLong _


newtype OperatingSystemName = OperatingSystemName String
derive instance newtypeOperatingSystemName :: Newtype OperatingSystemName _


newtype OperatingSystemVersion = OperatingSystemVersion String
derive instance newtypeOperatingSystemVersion :: Newtype OperatingSystemVersion _


-- | <p>Unable to perform the specified operation because another operation that effects an element in this activity is already in progress.</p>
newtype OperationInProgressException = OperationInProgressException 
  { 
  }
derive instance newtypeOperationInProgressException :: Newtype OperationInProgressException _


newtype OptionNamespace = OptionNamespace String
derive instance newtypeOptionNamespace :: Newtype OptionNamespace _


newtype OptionRestrictionMaxLength = OptionRestrictionMaxLength Int
derive instance newtypeOptionRestrictionMaxLength :: Newtype OptionRestrictionMaxLength _


newtype OptionRestrictionMaxValue = OptionRestrictionMaxValue Int
derive instance newtypeOptionRestrictionMaxValue :: Newtype OptionRestrictionMaxValue _


newtype OptionRestrictionMinValue = OptionRestrictionMinValue Int
derive instance newtypeOptionRestrictionMinValue :: Newtype OptionRestrictionMinValue _


-- | <p>A regular expression representing a restriction on a string configuration option value.</p>
newtype OptionRestrictionRegex = OptionRestrictionRegex 
  { "Pattern" :: NullOrUndefined (RegexPattern)
  , "Label" :: NullOrUndefined (RegexLabel)
  }
derive instance newtypeOptionRestrictionRegex :: Newtype OptionRestrictionRegex _


-- | <p>A specification identifying an individual configuration option.</p>
newtype OptionSpecification = OptionSpecification 
  { "ResourceName" :: NullOrUndefined (ResourceName)
  , "Namespace" :: NullOrUndefined (OptionNamespace)
  , "OptionName" :: NullOrUndefined (ConfigurationOptionName)
  }
derive instance newtypeOptionSpecification :: Newtype OptionSpecification _


newtype OptionsSpecifierList = OptionsSpecifierList (Array OptionSpecification)
derive instance newtypeOptionsSpecifierList :: Newtype OptionsSpecifierList _


newtype PlatformArn = PlatformArn String
derive instance newtypePlatformArn :: Newtype PlatformArn _


newtype PlatformCategory = PlatformCategory String
derive instance newtypePlatformCategory :: Newtype PlatformCategory _


-- | <p>Detailed information about a platform.</p>
newtype PlatformDescription = PlatformDescription 
  { "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "PlatformOwner" :: NullOrUndefined (PlatformOwner)
  , "PlatformName" :: NullOrUndefined (PlatformName)
  , "PlatformVersion" :: NullOrUndefined (PlatformVersion)
  , "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PlatformStatus" :: NullOrUndefined (PlatformStatus)
  , "DateCreated" :: NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined (UpdateDate)
  , "PlatformCategory" :: NullOrUndefined (PlatformCategory)
  , "Description" :: NullOrUndefined (Description)
  , "Maintainer" :: NullOrUndefined (Maintainer)
  , "OperatingSystemName" :: NullOrUndefined (OperatingSystemName)
  , "OperatingSystemVersion" :: NullOrUndefined (OperatingSystemVersion)
  , "ProgrammingLanguages" :: NullOrUndefined (PlatformProgrammingLanguages)
  , "Frameworks" :: NullOrUndefined (PlatformFrameworks)
  , "CustomAmiList" :: NullOrUndefined (CustomAmiList)
  , "SupportedTierList" :: NullOrUndefined (SupportedTierList)
  , "SupportedAddonList" :: NullOrUndefined (SupportedAddonList)
  }
derive instance newtypePlatformDescription :: Newtype PlatformDescription _


-- | <p>Specify criteria to restrict the results when listing custom platforms.</p> <p>The filter is evaluated as the expression:</p> <p> <code>Type</code> <code>Operator</code> <code>Values[i]</code> </p>
newtype PlatformFilter = PlatformFilter 
  { "Type" :: NullOrUndefined (PlatformFilterType)
  , "Operator" :: NullOrUndefined (PlatformFilterOperator)
  , "Values" :: NullOrUndefined (PlatformFilterValueList)
  }
derive instance newtypePlatformFilter :: Newtype PlatformFilter _


newtype PlatformFilterOperator = PlatformFilterOperator String
derive instance newtypePlatformFilterOperator :: Newtype PlatformFilterOperator _


newtype PlatformFilterType = PlatformFilterType String
derive instance newtypePlatformFilterType :: Newtype PlatformFilterType _


newtype PlatformFilterValue = PlatformFilterValue String
derive instance newtypePlatformFilterValue :: Newtype PlatformFilterValue _


newtype PlatformFilterValueList = PlatformFilterValueList (Array PlatformFilterValue)
derive instance newtypePlatformFilterValueList :: Newtype PlatformFilterValueList _


newtype PlatformFilters = PlatformFilters (Array PlatformFilter)
derive instance newtypePlatformFilters :: Newtype PlatformFilters _


-- | <p>A framework supported by the custom platform.</p>
newtype PlatformFramework = PlatformFramework 
  { "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }
derive instance newtypePlatformFramework :: Newtype PlatformFramework _


newtype PlatformFrameworks = PlatformFrameworks (Array PlatformFramework)
derive instance newtypePlatformFrameworks :: Newtype PlatformFrameworks _


newtype PlatformMaxRecords = PlatformMaxRecords Int
derive instance newtypePlatformMaxRecords :: Newtype PlatformMaxRecords _


newtype PlatformName = PlatformName String
derive instance newtypePlatformName :: Newtype PlatformName _


newtype PlatformOwner = PlatformOwner String
derive instance newtypePlatformOwner :: Newtype PlatformOwner _


-- | <p>A programming language supported by the platform.</p>
newtype PlatformProgrammingLanguage = PlatformProgrammingLanguage 
  { "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }
derive instance newtypePlatformProgrammingLanguage :: Newtype PlatformProgrammingLanguage _


newtype PlatformProgrammingLanguages = PlatformProgrammingLanguages (Array PlatformProgrammingLanguage)
derive instance newtypePlatformProgrammingLanguages :: Newtype PlatformProgrammingLanguages _


newtype PlatformStatus = PlatformStatus String
derive instance newtypePlatformStatus :: Newtype PlatformStatus _


-- | <p>Detailed information about a platform.</p>
newtype PlatformSummary = PlatformSummary 
  { "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "PlatformOwner" :: NullOrUndefined (PlatformOwner)
  , "PlatformStatus" :: NullOrUndefined (PlatformStatus)
  , "PlatformCategory" :: NullOrUndefined (PlatformCategory)
  , "OperatingSystemName" :: NullOrUndefined (OperatingSystemName)
  , "OperatingSystemVersion" :: NullOrUndefined (OperatingSystemVersion)
  , "SupportedTierList" :: NullOrUndefined (SupportedTierList)
  , "SupportedAddonList" :: NullOrUndefined (SupportedAddonList)
  }
derive instance newtypePlatformSummary :: Newtype PlatformSummary _


newtype PlatformSummaryList = PlatformSummaryList (Array PlatformSummary)
derive instance newtypePlatformSummaryList :: Newtype PlatformSummaryList _


newtype PlatformVersion = PlatformVersion String
derive instance newtypePlatformVersion :: Newtype PlatformVersion _


-- | <p>You cannot delete the platform version because there are still environments running on it.</p>
newtype PlatformVersionStillReferencedException = PlatformVersionStillReferencedException 
  { 
  }
derive instance newtypePlatformVersionStillReferencedException :: Newtype PlatformVersionStillReferencedException _


-- | <p>Describes a queue.</p>
newtype Queue = Queue 
  { "Name" :: NullOrUndefined (String)
  , "URL" :: NullOrUndefined (String)
  }
derive instance newtypeQueue :: Newtype Queue _


newtype QueueList = QueueList (Array Queue)
derive instance newtypeQueueList :: Newtype QueueList _


-- | <p/>
newtype RebuildEnvironmentMessage = RebuildEnvironmentMessage 
  { "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  }
derive instance newtypeRebuildEnvironmentMessage :: Newtype RebuildEnvironmentMessage _


newtype RefreshedAt = RefreshedAt Number
derive instance newtypeRefreshedAt :: Newtype RefreshedAt _


newtype RegexLabel = RegexLabel String
derive instance newtypeRegexLabel :: Newtype RegexLabel _


newtype RegexPattern = RegexPattern String
derive instance newtypeRegexPattern :: Newtype RegexPattern _


newtype RequestCount = RequestCount Int
derive instance newtypeRequestCount :: Newtype RequestCount _


-- | <p>Request to retrieve logs from an environment and store them in your Elastic Beanstalk storage bucket.</p>
newtype RequestEnvironmentInfoMessage = RequestEnvironmentInfoMessage 
  { "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "InfoType" :: (EnvironmentInfoType)
  }
derive instance newtypeRequestEnvironmentInfoMessage :: Newtype RequestEnvironmentInfoMessage _


newtype RequestId = RequestId String
derive instance newtypeRequestId :: Newtype RequestId _


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _


-- | <p>A resource doesn't exist for the specified Amazon Resource Name (ARN).</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype ResourceTagsDescriptionMessage = ResourceTagsDescriptionMessage 
  { "ResourceArn" :: NullOrUndefined (ResourceArn)
  , "ResourceTags" :: NullOrUndefined (TagList)
  }
derive instance newtypeResourceTagsDescriptionMessage :: Newtype ResourceTagsDescriptionMessage _


-- | <p>The type of the specified Amazon Resource Name (ARN) isn't supported for this operation.</p>
newtype ResourceTypeNotSupportedException = ResourceTypeNotSupportedException 
  { 
  }
derive instance newtypeResourceTypeNotSupportedException :: Newtype ResourceTypeNotSupportedException _


-- | <p/>
newtype RestartAppServerMessage = RestartAppServerMessage 
  { "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  }
derive instance newtypeRestartAppServerMessage :: Newtype RestartAppServerMessage _


-- | <p>Request to download logs retrieved with <a>RequestEnvironmentInfo</a>.</p>
newtype RetrieveEnvironmentInfoMessage = RetrieveEnvironmentInfoMessage 
  { "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "InfoType" :: (EnvironmentInfoType)
  }
derive instance newtypeRetrieveEnvironmentInfoMessage :: Newtype RetrieveEnvironmentInfoMessage _


-- | <p>Result message containing a description of the requested environment info.</p>
newtype RetrieveEnvironmentInfoResultMessage = RetrieveEnvironmentInfoResultMessage 
  { "EnvironmentInfo" :: NullOrUndefined (EnvironmentInfoDescriptionList)
  }
derive instance newtypeRetrieveEnvironmentInfoResultMessage :: Newtype RetrieveEnvironmentInfoResultMessage _


newtype S3Bucket = S3Bucket String
derive instance newtypeS3Bucket :: Newtype S3Bucket _


newtype S3Key = S3Key String
derive instance newtypeS3Key :: Newtype S3Key _


-- | <p>The bucket and key of an item stored in Amazon S3.</p>
newtype S3Location = S3Location 
  { "S3Bucket" :: NullOrUndefined (S3Bucket)
  , "S3Key" :: NullOrUndefined (S3Key)
  }
derive instance newtypeS3Location :: Newtype S3Location _


-- | <p>The specified S3 bucket does not belong to the S3 region in which the service is running. The following regions are supported:</p> <ul> <li> <p>IAD/us-east-1</p> </li> <li> <p>PDX/us-west-2</p> </li> <li> <p>DUB/eu-west-1</p> </li> </ul>
newtype S3LocationNotInServiceRegionException = S3LocationNotInServiceRegionException 
  { 
  }
derive instance newtypeS3LocationNotInServiceRegionException :: Newtype S3LocationNotInServiceRegionException _


-- | <p>The specified account does not have a subscription to Amazon S3.</p>
newtype S3SubscriptionRequiredException = S3SubscriptionRequiredException 
  { 
  }
derive instance newtypeS3SubscriptionRequiredException :: Newtype S3SubscriptionRequiredException _


newtype SampleTimestamp = SampleTimestamp Number
derive instance newtypeSampleTimestamp :: Newtype SampleTimestamp _


-- | <p>Detailed health information about an Amazon EC2 instance in your Elastic Beanstalk environment.</p>
newtype SingleInstanceHealth = SingleInstanceHealth 
  { "InstanceId" :: NullOrUndefined (InstanceId)
  , "HealthStatus" :: NullOrUndefined (String)
  , "Color" :: NullOrUndefined (String)
  , "Causes" :: NullOrUndefined (Causes)
  , "LaunchedAt" :: NullOrUndefined (LaunchedAt)
  , "ApplicationMetrics" :: NullOrUndefined (ApplicationMetrics)
  , "System" :: NullOrUndefined (SystemStatus)
  , "Deployment" :: NullOrUndefined (Deployment)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined (String)
  }
derive instance newtypeSingleInstanceHealth :: Newtype SingleInstanceHealth _


-- | <p>Describes the solution stack.</p>
newtype SolutionStackDescription = SolutionStackDescription 
  { "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PermittedFileTypes" :: NullOrUndefined (SolutionStackFileTypeList)
  }
derive instance newtypeSolutionStackDescription :: Newtype SolutionStackDescription _


newtype SolutionStackFileTypeList = SolutionStackFileTypeList (Array FileTypeExtension)
derive instance newtypeSolutionStackFileTypeList :: Newtype SolutionStackFileTypeList _


newtype SolutionStackName = SolutionStackName String
derive instance newtypeSolutionStackName :: Newtype SolutionStackName _


-- | <p>Location of the source code for an application version.</p>
newtype SourceBuildInformation = SourceBuildInformation 
  { "SourceType" :: (SourceType)
  , "SourceRepository" :: (SourceRepository)
  , "SourceLocation" :: (SourceLocation)
  }
derive instance newtypeSourceBuildInformation :: Newtype SourceBuildInformation _


-- | <p>Unable to delete the Amazon S3 source bundle associated with the application version. The application version was deleted successfully.</p>
newtype SourceBundleDeletionException = SourceBundleDeletionException 
  { 
  }
derive instance newtypeSourceBundleDeletionException :: Newtype SourceBundleDeletionException _


-- | <p>A specification for an environment configuration</p>
newtype SourceConfiguration = SourceConfiguration 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  }
derive instance newtypeSourceConfiguration :: Newtype SourceConfiguration _


newtype SourceLocation = SourceLocation String
derive instance newtypeSourceLocation :: Newtype SourceLocation _


newtype SourceRepository = SourceRepository String
derive instance newtypeSourceRepository :: Newtype SourceRepository _


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _


-- | <p>Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response. For more information, see <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html">Status Code Definitions</a>.</p>
newtype StatusCodes = StatusCodes 
  { "Status2xx" :: NullOrUndefined (NullableInteger)
  , "Status3xx" :: NullOrUndefined (NullableInteger)
  , "Status4xx" :: NullOrUndefined (NullableInteger)
  , "Status5xx" :: NullOrUndefined (NullableInteger)
  }
derive instance newtypeStatusCodes :: Newtype StatusCodes _


newtype SupportedAddon = SupportedAddon String
derive instance newtypeSupportedAddon :: Newtype SupportedAddon _


newtype SupportedAddonList = SupportedAddonList (Array SupportedAddon)
derive instance newtypeSupportedAddonList :: Newtype SupportedAddonList _


newtype SupportedTier = SupportedTier String
derive instance newtypeSupportedTier :: Newtype SupportedTier _


newtype SupportedTierList = SupportedTierList (Array SupportedTier)
derive instance newtypeSupportedTierList :: Newtype SupportedTierList _


-- | <p>Swaps the CNAMEs of two environments.</p>
newtype SwapEnvironmentCNAMEsMessage = SwapEnvironmentCNAMEsMessage 
  { "SourceEnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "SourceEnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "DestinationEnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "DestinationEnvironmentName" :: NullOrUndefined (EnvironmentName)
  }
derive instance newtypeSwapEnvironmentCNAMEsMessage :: Newtype SwapEnvironmentCNAMEsMessage _


-- | <p>CPU utilization and load average metrics for an Amazon EC2 instance.</p>
newtype SystemStatus = SystemStatus 
  { "CPUUtilization" :: NullOrUndefined (CPUUtilization)
  , "LoadAverage" :: NullOrUndefined (LoadAverage)
  }
derive instance newtypeSystemStatus :: Newtype SystemStatus _


-- | <p>Describes a tag applied to a resource in an environment.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _


newtype TerminateEnvForce = TerminateEnvForce Boolean
derive instance newtypeTerminateEnvForce :: Newtype TerminateEnvForce _


-- | <p>Request to terminate an environment.</p>
newtype TerminateEnvironmentMessage = TerminateEnvironmentMessage 
  { "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "TerminateResources" :: NullOrUndefined (TerminateEnvironmentResources)
  , "ForceTerminate" :: NullOrUndefined (ForceTerminate)
  }
derive instance newtypeTerminateEnvironmentMessage :: Newtype TerminateEnvironmentMessage _


newtype TerminateEnvironmentResources = TerminateEnvironmentResources Boolean
derive instance newtypeTerminateEnvironmentResources :: Newtype TerminateEnvironmentResources _


newtype TimeFilterEnd = TimeFilterEnd Number
derive instance newtypeTimeFilterEnd :: Newtype TimeFilterEnd _


newtype TimeFilterStart = TimeFilterStart Number
derive instance newtypeTimeFilterStart :: Newtype TimeFilterStart _


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _


-- | <p>The specified account has reached its limit of application versions.</p>
newtype TooManyApplicationVersionsException = TooManyApplicationVersionsException 
  { 
  }
derive instance newtypeTooManyApplicationVersionsException :: Newtype TooManyApplicationVersionsException _


-- | <p>The specified account has reached its limit of applications.</p>
newtype TooManyApplicationsException = TooManyApplicationsException 
  { 
  }
derive instance newtypeTooManyApplicationsException :: Newtype TooManyApplicationsException _


-- | <p>The specified account has reached its limit of Amazon S3 buckets.</p>
newtype TooManyBucketsException = TooManyBucketsException 
  { 
  }
derive instance newtypeTooManyBucketsException :: Newtype TooManyBucketsException _


-- | <p>The specified account has reached its limit of configuration templates.</p>
newtype TooManyConfigurationTemplatesException = TooManyConfigurationTemplatesException 
  { 
  }
derive instance newtypeTooManyConfigurationTemplatesException :: Newtype TooManyConfigurationTemplatesException _


-- | <p>The specified account has reached its limit of environments.</p>
newtype TooManyEnvironmentsException = TooManyEnvironmentsException 
  { 
  }
derive instance newtypeTooManyEnvironmentsException :: Newtype TooManyEnvironmentsException _


-- | <p>You have exceeded the maximum number of allowed platforms associated with the account.</p>
newtype TooManyPlatformsException = TooManyPlatformsException 
  { 
  }
derive instance newtypeTooManyPlatformsException :: Newtype TooManyPlatformsException _


-- | <p>The number of tags in the resource would exceed the number of tags that each resource can have.</p> <p>To calculate this, the operation considers both the number of tags the resource already has and the tags this operation would add if it succeeded.</p>
newtype TooManyTagsException = TooManyTagsException 
  { 
  }
derive instance newtypeTooManyTagsException :: Newtype TooManyTagsException _


-- | <p>Describes a trigger.</p>
newtype Trigger = Trigger 
  { "Name" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeTrigger :: Newtype Trigger _


newtype TriggerList = TriggerList (Array Trigger)
derive instance newtypeTriggerList :: Newtype TriggerList _


-- | <p>Request to update an application.</p>
newtype UpdateApplicationMessage = UpdateApplicationMessage 
  { "ApplicationName" :: (ApplicationName)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeUpdateApplicationMessage :: Newtype UpdateApplicationMessage _


newtype UpdateApplicationResourceLifecycleMessage = UpdateApplicationResourceLifecycleMessage 
  { "ApplicationName" :: (ApplicationName)
  , "ResourceLifecycleConfig" :: (ApplicationResourceLifecycleConfig)
  }
derive instance newtypeUpdateApplicationResourceLifecycleMessage :: Newtype UpdateApplicationResourceLifecycleMessage _


-- | <p/>
newtype UpdateApplicationVersionMessage = UpdateApplicationVersionMessage 
  { "ApplicationName" :: (ApplicationName)
  , "VersionLabel" :: (VersionLabel)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeUpdateApplicationVersionMessage :: Newtype UpdateApplicationVersionMessage _


-- | <p>The result message containing the options for the specified solution stack.</p>
newtype UpdateConfigurationTemplateMessage = UpdateConfigurationTemplateMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: (ConfigurationTemplateName)
  , "Description" :: NullOrUndefined (Description)
  , "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList)
  , "OptionsToRemove" :: NullOrUndefined (OptionsSpecifierList)
  }
derive instance newtypeUpdateConfigurationTemplateMessage :: Newtype UpdateConfigurationTemplateMessage _


newtype UpdateDate = UpdateDate Number
derive instance newtypeUpdateDate :: Newtype UpdateDate _


-- | <p>Request to update an environment.</p>
newtype UpdateEnvironmentMessage = UpdateEnvironmentMessage 
  { "ApplicationName" :: NullOrUndefined (ApplicationName)
  , "EnvironmentId" :: NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "GroupName" :: NullOrUndefined (GroupName)
  , "Description" :: NullOrUndefined (Description)
  , "Tier" :: NullOrUndefined (EnvironmentTier)
  , "VersionLabel" :: NullOrUndefined (VersionLabel)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "SolutionStackName" :: NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined (PlatformArn)
  , "OptionSettings" :: NullOrUndefined (ConfigurationOptionSettingsList)
  , "OptionsToRemove" :: NullOrUndefined (OptionsSpecifierList)
  }
derive instance newtypeUpdateEnvironmentMessage :: Newtype UpdateEnvironmentMessage _


newtype UpdateTagsForResourceMessage = UpdateTagsForResourceMessage 
  { "ResourceArn" :: (ResourceArn)
  , "TagsToAdd" :: NullOrUndefined (TagList)
  , "TagsToRemove" :: NullOrUndefined (TagKeyList)
  }
derive instance newtypeUpdateTagsForResourceMessage :: Newtype UpdateTagsForResourceMessage _


newtype UserDefinedOption = UserDefinedOption Boolean
derive instance newtypeUserDefinedOption :: Newtype UserDefinedOption _


-- | <p>A list of validation messages for a specified configuration template.</p>
newtype ValidateConfigurationSettingsMessage = ValidateConfigurationSettingsMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentName" :: NullOrUndefined (EnvironmentName)
  , "OptionSettings" :: (ConfigurationOptionSettingsList)
  }
derive instance newtypeValidateConfigurationSettingsMessage :: Newtype ValidateConfigurationSettingsMessage _


-- | <p>An error or warning for a desired configuration option value.</p>
newtype ValidationMessage = ValidationMessage 
  { "Message" :: NullOrUndefined (ValidationMessageString)
  , "Severity" :: NullOrUndefined (ValidationSeverity)
  , "Namespace" :: NullOrUndefined (OptionNamespace)
  , "OptionName" :: NullOrUndefined (ConfigurationOptionName)
  }
derive instance newtypeValidationMessage :: Newtype ValidationMessage _


newtype ValidationMessageString = ValidationMessageString String
derive instance newtypeValidationMessageString :: Newtype ValidationMessageString _


newtype ValidationMessagesList = ValidationMessagesList (Array ValidationMessage)
derive instance newtypeValidationMessagesList :: Newtype ValidationMessagesList _


newtype ValidationSeverity = ValidationSeverity String
derive instance newtypeValidationSeverity :: Newtype ValidationSeverity _


newtype VersionLabel = VersionLabel String
derive instance newtypeVersionLabel :: Newtype VersionLabel _


newtype VersionLabels = VersionLabels (Array VersionLabel)
derive instance newtypeVersionLabels :: Newtype VersionLabels _


newtype VersionLabelsList = VersionLabelsList (Array VersionLabel)
derive instance newtypeVersionLabelsList :: Newtype VersionLabelsList _


newtype VirtualizationType = VirtualizationType String
derive instance newtypeVirtualizationType :: Newtype VirtualizationType _
