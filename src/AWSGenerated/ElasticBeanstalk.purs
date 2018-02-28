

-- | <fullname>AWS Elastic Beanstalk</fullname> <p>AWS Elastic Beanstalk makes it easy for you to create, deploy, and manage scalable, fault-tolerant applications running on the Amazon Web Services cloud.</p> <p>For more information about this product, go to the <a href="http://aws.amazon.com/elasticbeanstalk/">AWS Elastic Beanstalk</a> details page. The location of the latest AWS Elastic Beanstalk WSDL is <a href="http://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl">http://elasticbeanstalk.s3.amazonaws.com/doc/2010-12-01/AWSElasticBeanstalk.wsdl</a>. To install the Software Development Kits (SDKs), Integrated Development Environment (IDE) Toolkits, and command line tools that enable you to access the API, go to <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> <p> <b>Endpoints</b> </p> <p>For a list of region-specific endpoints that AWS Elastic Beanstalk supports, go to <a href="http://docs.aws.amazon.com/general/latest/gr/rande.html#elasticbeanstalk_region">Regions and Endpoints</a> in the <i>Amazon Web Services Glossary</i>.</p>
module AWS.ElasticBeanstalk where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "ElasticBeanstalk" :: String


-- | <p>Cancels in-progress environment configuration update or application version deployment.</p>
abortEnvironmentUpdate :: forall eff. AbortEnvironmentUpdateMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
abortEnvironmentUpdate = Request.request serviceName "abortEnvironmentUpdate" 


-- | <p>Applies a scheduled managed action immediately. A managed action can be applied only if its status is <code>Scheduled</code>. Get the status and action ID of a managed action with <a>DescribeEnvironmentManagedActions</a>.</p>
applyEnvironmentManagedAction :: forall eff. ApplyEnvironmentManagedActionRequest -> Aff (exception :: EXCEPTION | eff) ApplyEnvironmentManagedActionResult
applyEnvironmentManagedAction = Request.request serviceName "applyEnvironmentManagedAction" 


-- | <p>Checks if the specified CNAME is available.</p>
checkDNSAvailability :: forall eff. CheckDNSAvailabilityMessage -> Aff (exception :: EXCEPTION | eff) CheckDNSAvailabilityResultMessage
checkDNSAvailability = Request.request serviceName "checkDNSAvailability" 


-- | <p>Create or update a group of environments that each run a separate component of a single application. Takes a list of version labels that specify application source bundles for each of the environments to create or update. The name of each environment and other required information must be included in the source bundles in an environment manifest named <code>env.yaml</code>. See <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-mgmt-compose.html">Compose Environments</a> for details.</p>
composeEnvironments :: forall eff. ComposeEnvironmentsMessage -> Aff (exception :: EXCEPTION | eff) EnvironmentDescriptionsMessage
composeEnvironments = Request.request serviceName "composeEnvironments" 


-- | <p> Creates an application that has one configuration template named <code>default</code> and no application versions. </p>
createApplication :: forall eff. CreateApplicationMessage -> Aff (exception :: EXCEPTION | eff) ApplicationDescriptionMessage
createApplication = Request.request serviceName "createApplication" 


-- | <p>Creates an application version for the specified application. You can create an application version from a source bundle in Amazon S3, a commit in AWS CodeCommit, or the output of an AWS CodeBuild build as follows:</p> <p>Specify a commit in an AWS CodeCommit repository with <code>SourceBuildInformation</code>.</p> <p>Specify a build in an AWS CodeBuild with <code>SourceBuildInformation</code> and <code>BuildConfiguration</code>.</p> <p>Specify a source bundle in S3 with <code>SourceBundle</code> </p> <p>Omit both <code>SourceBuildInformation</code> and <code>SourceBundle</code> to use the default sample application.</p> <note> <p>Once you create an application version with a specified Amazon S3 bucket and key location, you cannot change that Amazon S3 location. If you change the Amazon S3 location, you receive an exception when you attempt to launch an environment from the application version.</p> </note>
createApplicationVersion :: forall eff. CreateApplicationVersionMessage -> Aff (exception :: EXCEPTION | eff) ApplicationVersionDescriptionMessage
createApplicationVersion = Request.request serviceName "createApplicationVersion" 


-- | <p>Creates a configuration template. Templates are associated with a specific application and are used to deploy different versions of the application with the same configuration settings.</p> <p>Related Topics</p> <ul> <li> <p> <a>DescribeConfigurationOptions</a> </p> </li> <li> <p> <a>DescribeConfigurationSettings</a> </p> </li> <li> <p> <a>ListAvailableSolutionStacks</a> </p> </li> </ul>
createConfigurationTemplate :: forall eff. CreateConfigurationTemplateMessage -> Aff (exception :: EXCEPTION | eff) ConfigurationSettingsDescription
createConfigurationTemplate = Request.request serviceName "createConfigurationTemplate" 


-- | <p>Launches an environment for the specified application using the specified configuration.</p>
createEnvironment :: forall eff. CreateEnvironmentMessage -> Aff (exception :: EXCEPTION | eff) EnvironmentDescription
createEnvironment = Request.request serviceName "createEnvironment" 


-- | <p>Create a new version of your custom platform.</p>
createPlatformVersion :: forall eff. CreatePlatformVersionRequest -> Aff (exception :: EXCEPTION | eff) CreatePlatformVersionResult
createPlatformVersion = Request.request serviceName "createPlatformVersion" 


-- | <p>Creates a bucket in Amazon S3 to store application versions, logs, and other files used by Elastic Beanstalk environments. The Elastic Beanstalk console and EB CLI call this API the first time you create an environment in a region. If the storage location already exists, <code>CreateStorageLocation</code> still returns the bucket name but does not create a new bucket.</p>
createStorageLocation :: forall eff.  Aff (exception :: EXCEPTION | eff) CreateStorageLocationResultMessage
createStorageLocation = Request.request serviceName "createStorageLocation" (Types.NoInput unit)


-- | <p>Deletes the specified application along with all associated versions and configurations. The application versions will not be deleted from your Amazon S3 bucket.</p> <note> <p>You cannot delete an application that has a running environment.</p> </note>
deleteApplication :: forall eff. DeleteApplicationMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteApplication = Request.request serviceName "deleteApplication" 


-- | <p>Deletes the specified version from the specified application.</p> <note> <p>You cannot delete an application version that is associated with a running environment.</p> </note>
deleteApplicationVersion :: forall eff. DeleteApplicationVersionMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteApplicationVersion = Request.request serviceName "deleteApplicationVersion" 


-- | <p>Deletes the specified configuration template.</p> <note> <p>When you launch an environment using a configuration template, the environment gets a copy of the template. You can delete or modify the environment's copy of the template without affecting the running environment.</p> </note>
deleteConfigurationTemplate :: forall eff. DeleteConfigurationTemplateMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteConfigurationTemplate = Request.request serviceName "deleteConfigurationTemplate" 


-- | <p>Deletes the draft configuration associated with the running environment.</p> <p>Updating a running environment with any configuration changes creates a draft configuration set. You can get the draft configuration using <a>DescribeConfigurationSettings</a> while the update is in progress or if the update fails. The <code>DeploymentStatus</code> for the draft configuration indicates whether the deployment is in process or has failed. The draft configuration remains in existence until it is deleted with this action.</p>
deleteEnvironmentConfiguration :: forall eff. DeleteEnvironmentConfigurationMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteEnvironmentConfiguration = Request.request serviceName "deleteEnvironmentConfiguration" 


-- | <p>Deletes the specified version of a custom platform.</p>
deletePlatformVersion :: forall eff. DeletePlatformVersionRequest -> Aff (exception :: EXCEPTION | eff) DeletePlatformVersionResult
deletePlatformVersion = Request.request serviceName "deletePlatformVersion" 


-- | <p>Retrieve a list of application versions.</p>
describeApplicationVersions :: forall eff. DescribeApplicationVersionsMessage -> Aff (exception :: EXCEPTION | eff) ApplicationVersionDescriptionsMessage
describeApplicationVersions = Request.request serviceName "describeApplicationVersions" 


-- | <p>Returns the descriptions of existing applications.</p>
describeApplications :: forall eff. DescribeApplicationsMessage -> Aff (exception :: EXCEPTION | eff) ApplicationDescriptionsMessage
describeApplications = Request.request serviceName "describeApplications" 


-- | <p>Describes the configuration options that are used in a particular configuration template or environment, or that a specified solution stack defines. The description includes the values the options, their default values, and an indication of the required action on a running environment if an option value is changed.</p>
describeConfigurationOptions :: forall eff. DescribeConfigurationOptionsMessage -> Aff (exception :: EXCEPTION | eff) ConfigurationOptionsDescription
describeConfigurationOptions = Request.request serviceName "describeConfigurationOptions" 


-- | <p>Returns a description of the settings for the specified configuration set, that is, either a configuration template or the configuration set associated with a running environment.</p> <p>When describing the settings for the configuration set associated with a running environment, it is possible to receive two sets of setting descriptions. One is the deployed configuration set, and the other is a draft configuration of an environment that is either in the process of deployment or that failed to deploy.</p> <p>Related Topics</p> <ul> <li> <p> <a>DeleteEnvironmentConfiguration</a> </p> </li> </ul>
describeConfigurationSettings :: forall eff. DescribeConfigurationSettingsMessage -> Aff (exception :: EXCEPTION | eff) ConfigurationSettingsDescriptions
describeConfigurationSettings = Request.request serviceName "describeConfigurationSettings" 


-- | <p>Returns information about the overall health of the specified environment. The <b>DescribeEnvironmentHealth</b> operation is only available with AWS Elastic Beanstalk Enhanced Health.</p>
describeEnvironmentHealth :: forall eff. DescribeEnvironmentHealthRequest -> Aff (exception :: EXCEPTION | eff) DescribeEnvironmentHealthResult
describeEnvironmentHealth = Request.request serviceName "describeEnvironmentHealth" 


-- | <p>Lists an environment's completed and failed managed actions.</p>
describeEnvironmentManagedActionHistory :: forall eff. DescribeEnvironmentManagedActionHistoryRequest -> Aff (exception :: EXCEPTION | eff) DescribeEnvironmentManagedActionHistoryResult
describeEnvironmentManagedActionHistory = Request.request serviceName "describeEnvironmentManagedActionHistory" 


-- | <p>Lists an environment's upcoming and in-progress managed actions.</p>
describeEnvironmentManagedActions :: forall eff. DescribeEnvironmentManagedActionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeEnvironmentManagedActionsResult
describeEnvironmentManagedActions = Request.request serviceName "describeEnvironmentManagedActions" 


-- | <p>Returns AWS resources for this environment.</p>
describeEnvironmentResources :: forall eff. DescribeEnvironmentResourcesMessage -> Aff (exception :: EXCEPTION | eff) EnvironmentResourceDescriptionsMessage
describeEnvironmentResources = Request.request serviceName "describeEnvironmentResources" 


-- | <p>Returns descriptions for existing environments.</p>
describeEnvironments :: forall eff. DescribeEnvironmentsMessage -> Aff (exception :: EXCEPTION | eff) EnvironmentDescriptionsMessage
describeEnvironments = Request.request serviceName "describeEnvironments" 


-- | <p>Returns list of event descriptions matching criteria up to the last 6 weeks.</p> <note> <p>This action returns the most recent 1,000 events from the specified <code>NextToken</code>.</p> </note>
describeEvents :: forall eff. DescribeEventsMessage -> Aff (exception :: EXCEPTION | eff) EventDescriptionsMessage
describeEvents = Request.request serviceName "describeEvents" 


-- | <p>Retrives detailed information about the health of instances in your AWS Elastic Beanstalk. This operation requires <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced.html">enhanced health reporting</a>.</p>
describeInstancesHealth :: forall eff. DescribeInstancesHealthRequest -> Aff (exception :: EXCEPTION | eff) DescribeInstancesHealthResult
describeInstancesHealth = Request.request serviceName "describeInstancesHealth" 


-- | <p>Describes the version of the platform.</p>
describePlatformVersion :: forall eff. DescribePlatformVersionRequest -> Aff (exception :: EXCEPTION | eff) DescribePlatformVersionResult
describePlatformVersion = Request.request serviceName "describePlatformVersion" 


-- | <p>Returns a list of the available solution stack names, with the public version first and then in reverse chronological order.</p>
listAvailableSolutionStacks :: forall eff.  Aff (exception :: EXCEPTION | eff) ListAvailableSolutionStacksResultMessage
listAvailableSolutionStacks = Request.request serviceName "listAvailableSolutionStacks" (Types.NoInput unit)


-- | <p>Lists the available platforms.</p>
listPlatformVersions :: forall eff. ListPlatformVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListPlatformVersionsResult
listPlatformVersions = Request.request serviceName "listPlatformVersions" 


-- | <p>Returns the tags applied to an AWS Elastic Beanstalk resource. The response contains a list of tag key-value pairs.</p> <p>Currently, Elastic Beanstalk only supports tagging of Elastic Beanstalk environments. For details about environment tagging, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.tagging.html">Tagging Resources in Your Elastic Beanstalk Environment</a>.</p>
listTagsForResource :: forall eff. ListTagsForResourceMessage -> Aff (exception :: EXCEPTION | eff) ResourceTagsDescriptionMessage
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>Deletes and recreates all of the AWS resources (for example: the Auto Scaling group, load balancer, etc.) for a specified environment and forces a restart.</p>
rebuildEnvironment :: forall eff. RebuildEnvironmentMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
rebuildEnvironment = Request.request serviceName "rebuildEnvironment" 


-- | <p>Initiates a request to compile the specified type of information of the deployed environment.</p> <p> Setting the <code>InfoType</code> to <code>tail</code> compiles the last lines from the application server log files of every Amazon EC2 instance in your environment. </p> <p> Setting the <code>InfoType</code> to <code>bundle</code> compresses the application server log files for every Amazon EC2 instance into a <code>.zip</code> file. Legacy and .NET containers do not support bundle logs. </p> <p> Use <a>RetrieveEnvironmentInfo</a> to obtain the set of logs. </p> <p>Related Topics</p> <ul> <li> <p> <a>RetrieveEnvironmentInfo</a> </p> </li> </ul>
requestEnvironmentInfo :: forall eff. RequestEnvironmentInfoMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
requestEnvironmentInfo = Request.request serviceName "requestEnvironmentInfo" 


-- | <p>Causes the environment to restart the application container server running on each Amazon EC2 instance.</p>
restartAppServer :: forall eff. RestartAppServerMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
restartAppServer = Request.request serviceName "restartAppServer" 


-- | <p>Retrieves the compiled information from a <a>RequestEnvironmentInfo</a> request.</p> <p>Related Topics</p> <ul> <li> <p> <a>RequestEnvironmentInfo</a> </p> </li> </ul>
retrieveEnvironmentInfo :: forall eff. RetrieveEnvironmentInfoMessage -> Aff (exception :: EXCEPTION | eff) RetrieveEnvironmentInfoResultMessage
retrieveEnvironmentInfo = Request.request serviceName "retrieveEnvironmentInfo" 


-- | <p>Swaps the CNAMEs of two environments.</p>
swapEnvironmentCNAMEs :: forall eff. SwapEnvironmentCNAMEsMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
swapEnvironmentCNAMEs = Request.request serviceName "swapEnvironmentCNAMEs" 


-- | <p>Terminates the specified environment.</p>
terminateEnvironment :: forall eff. TerminateEnvironmentMessage -> Aff (exception :: EXCEPTION | eff) EnvironmentDescription
terminateEnvironment = Request.request serviceName "terminateEnvironment" 


-- | <p>Updates the specified application to have the specified properties.</p> <note> <p>If a property (for example, <code>description</code>) is not provided, the value remains unchanged. To clear these properties, specify an empty string.</p> </note>
updateApplication :: forall eff. UpdateApplicationMessage -> Aff (exception :: EXCEPTION | eff) ApplicationDescriptionMessage
updateApplication = Request.request serviceName "updateApplication" 


-- | <p>Modifies lifecycle settings for an application.</p>
updateApplicationResourceLifecycle :: forall eff. UpdateApplicationResourceLifecycleMessage -> Aff (exception :: EXCEPTION | eff) ApplicationResourceLifecycleDescriptionMessage
updateApplicationResourceLifecycle = Request.request serviceName "updateApplicationResourceLifecycle" 


-- | <p>Updates the specified application version to have the specified properties.</p> <note> <p>If a property (for example, <code>description</code>) is not provided, the value remains unchanged. To clear properties, specify an empty string.</p> </note>
updateApplicationVersion :: forall eff. UpdateApplicationVersionMessage -> Aff (exception :: EXCEPTION | eff) ApplicationVersionDescriptionMessage
updateApplicationVersion = Request.request serviceName "updateApplicationVersion" 


-- | <p>Updates the specified configuration template to have the specified properties or configuration option values.</p> <note> <p>If a property (for example, <code>ApplicationName</code>) is not provided, its value remains unchanged. To clear such properties, specify an empty string.</p> </note> <p>Related Topics</p> <ul> <li> <p> <a>DescribeConfigurationOptions</a> </p> </li> </ul>
updateConfigurationTemplate :: forall eff. UpdateConfigurationTemplateMessage -> Aff (exception :: EXCEPTION | eff) ConfigurationSettingsDescription
updateConfigurationTemplate = Request.request serviceName "updateConfigurationTemplate" 


-- | <p>Updates the environment description, deploys a new application version, updates the configuration settings to an entirely new configuration template, or updates select configuration option values in the running environment.</p> <p> Attempting to update both the release and configuration is not allowed and AWS Elastic Beanstalk returns an <code>InvalidParameterCombination</code> error. </p> <p> When updating the configuration settings to a new template or individual settings, a draft configuration is created and <a>DescribeConfigurationSettings</a> for this environment returns two setting descriptions with different <code>DeploymentStatus</code> values. </p>
updateEnvironment :: forall eff. UpdateEnvironmentMessage -> Aff (exception :: EXCEPTION | eff) EnvironmentDescription
updateEnvironment = Request.request serviceName "updateEnvironment" 


-- | <p>Update the list of tags applied to an AWS Elastic Beanstalk resource. Two lists can be passed: <code>TagsToAdd</code> for tags to add or update, and <code>TagsToRemove</code>.</p> <p>Currently, Elastic Beanstalk only supports tagging of Elastic Beanstalk environments. For details about environment tagging, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/using-features.tagging.html">Tagging Resources in Your Elastic Beanstalk Environment</a>.</p> <p>If you create a custom IAM user policy to control permission to this operation, specify one of the following two virtual actions (or both) instead of the API operation name:</p> <dl> <dt>elasticbeanstalk:AddTags</dt> <dd> <p>Controls permission to call <code>UpdateTagsForResource</code> and pass a list of tags to add in the <code>TagsToAdd</code> parameter.</p> </dd> <dt>elasticbeanstalk:RemoveTags</dt> <dd> <p>Controls permission to call <code>UpdateTagsForResource</code> and pass a list of tag keys to remove in the <code>TagsToRemove</code> parameter.</p> </dd> </dl> <p>For details about creating a custom user policy, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/AWSHowTo.iam.managed-policies.html#AWSHowTo.iam.policies">Creating a Custom User Policy</a>.</p>
updateTagsForResource :: forall eff. UpdateTagsForResourceMessage -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateTagsForResource = Request.request serviceName "updateTagsForResource" 


-- | <p>Takes a set of configuration settings and either a configuration template or environment, and determines whether those values are valid.</p> <p>This action returns a list of messages indicating any errors or warnings associated with the selection of option values.</p>
validateConfigurationSettings :: forall eff. ValidateConfigurationSettingsMessage -> Aff (exception :: EXCEPTION | eff) ConfigurationSettingsValidationMessages
validateConfigurationSettings = Request.request serviceName "validateConfigurationSettings" 


newtype ARN = ARN String
derive instance newtypeARN :: Newtype ARN _
derive instance repGenericARN :: Generic ARN _
instance showARN :: Show ARN where
  show = genericShow
instance decodeARN :: Decode ARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeARN :: Encode ARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype AbortEnvironmentUpdateMessage = AbortEnvironmentUpdateMessage 
  { "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  }
derive instance newtypeAbortEnvironmentUpdateMessage :: Newtype AbortEnvironmentUpdateMessage _
derive instance repGenericAbortEnvironmentUpdateMessage :: Generic AbortEnvironmentUpdateMessage _
instance showAbortEnvironmentUpdateMessage :: Show AbortEnvironmentUpdateMessage where
  show = genericShow
instance decodeAbortEnvironmentUpdateMessage :: Decode AbortEnvironmentUpdateMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortEnvironmentUpdateMessage :: Encode AbortEnvironmentUpdateMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AbortableOperationInProgress = AbortableOperationInProgress Boolean
derive instance newtypeAbortableOperationInProgress :: Newtype AbortableOperationInProgress _
derive instance repGenericAbortableOperationInProgress :: Generic AbortableOperationInProgress _
instance showAbortableOperationInProgress :: Show AbortableOperationInProgress where
  show = genericShow
instance decodeAbortableOperationInProgress :: Decode AbortableOperationInProgress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortableOperationInProgress :: Encode AbortableOperationInProgress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionHistoryStatus = ActionHistoryStatus String
derive instance newtypeActionHistoryStatus :: Newtype ActionHistoryStatus _
derive instance repGenericActionHistoryStatus :: Generic ActionHistoryStatus _
instance showActionHistoryStatus :: Show ActionHistoryStatus where
  show = genericShow
instance decodeActionHistoryStatus :: Decode ActionHistoryStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionHistoryStatus :: Encode ActionHistoryStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionStatus = ActionStatus String
derive instance newtypeActionStatus :: Newtype ActionStatus _
derive instance repGenericActionStatus :: Generic ActionStatus _
instance showActionStatus :: Show ActionStatus where
  show = genericShow
instance decodeActionStatus :: Decode ActionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionStatus :: Encode ActionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionType = ActionType String
derive instance newtypeActionType :: Newtype ActionType _
derive instance repGenericActionType :: Generic ActionType _
instance showActionType :: Show ActionType where
  show = genericShow
instance decodeActionType :: Decode ActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionType :: Encode ActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the properties of an application.</p>
newtype ApplicationDescription = ApplicationDescription 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "DateCreated" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined.NullOrUndefined (UpdateDate)
  , "Versions" :: NullOrUndefined.NullOrUndefined (VersionLabelsList)
  , "ConfigurationTemplates" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateNamesList)
  , "ResourceLifecycleConfig" :: NullOrUndefined.NullOrUndefined (ApplicationResourceLifecycleConfig)
  }
derive instance newtypeApplicationDescription :: Newtype ApplicationDescription _
derive instance repGenericApplicationDescription :: Generic ApplicationDescription _
instance showApplicationDescription :: Show ApplicationDescription where
  show = genericShow
instance decodeApplicationDescription :: Decode ApplicationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationDescription :: Encode ApplicationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationDescriptionList = ApplicationDescriptionList (Array ApplicationDescription)
derive instance newtypeApplicationDescriptionList :: Newtype ApplicationDescriptionList _
derive instance repGenericApplicationDescriptionList :: Generic ApplicationDescriptionList _
instance showApplicationDescriptionList :: Show ApplicationDescriptionList where
  show = genericShow
instance decodeApplicationDescriptionList :: Decode ApplicationDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationDescriptionList :: Encode ApplicationDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message containing a single description of an application.</p>
newtype ApplicationDescriptionMessage = ApplicationDescriptionMessage 
  { "Application" :: NullOrUndefined.NullOrUndefined (ApplicationDescription)
  }
derive instance newtypeApplicationDescriptionMessage :: Newtype ApplicationDescriptionMessage _
derive instance repGenericApplicationDescriptionMessage :: Generic ApplicationDescriptionMessage _
instance showApplicationDescriptionMessage :: Show ApplicationDescriptionMessage where
  show = genericShow
instance decodeApplicationDescriptionMessage :: Decode ApplicationDescriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationDescriptionMessage :: Encode ApplicationDescriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message containing a list of application descriptions.</p>
newtype ApplicationDescriptionsMessage = ApplicationDescriptionsMessage 
  { "Applications" :: NullOrUndefined.NullOrUndefined (ApplicationDescriptionList)
  }
derive instance newtypeApplicationDescriptionsMessage :: Newtype ApplicationDescriptionsMessage _
derive instance repGenericApplicationDescriptionsMessage :: Generic ApplicationDescriptionsMessage _
instance showApplicationDescriptionsMessage :: Show ApplicationDescriptionsMessage where
  show = genericShow
instance decodeApplicationDescriptionsMessage :: Decode ApplicationDescriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationDescriptionsMessage :: Encode ApplicationDescriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Application request metrics for an AWS Elastic Beanstalk environment.</p>
newtype ApplicationMetrics = ApplicationMetrics 
  { "Duration" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "RequestCount" :: NullOrUndefined.NullOrUndefined (RequestCount)
  , "StatusCodes" :: NullOrUndefined.NullOrUndefined (StatusCodes)
  , "Latency" :: NullOrUndefined.NullOrUndefined (Latency)
  }
derive instance newtypeApplicationMetrics :: Newtype ApplicationMetrics _
derive instance repGenericApplicationMetrics :: Generic ApplicationMetrics _
instance showApplicationMetrics :: Show ApplicationMetrics where
  show = genericShow
instance decodeApplicationMetrics :: Decode ApplicationMetrics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationMetrics :: Encode ApplicationMetrics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationName = ApplicationName String
derive instance newtypeApplicationName :: Newtype ApplicationName _
derive instance repGenericApplicationName :: Generic ApplicationName _
instance showApplicationName :: Show ApplicationName where
  show = genericShow
instance decodeApplicationName :: Decode ApplicationName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationName :: Encode ApplicationName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationNamesList = ApplicationNamesList (Array ApplicationName)
derive instance newtypeApplicationNamesList :: Newtype ApplicationNamesList _
derive instance repGenericApplicationNamesList :: Generic ApplicationNamesList _
instance showApplicationNamesList :: Show ApplicationNamesList where
  show = genericShow
instance decodeApplicationNamesList :: Decode ApplicationNamesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationNamesList :: Encode ApplicationNamesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource lifecycle configuration for an application. Defines lifecycle settings for resources that belong to the application, and the service role that Elastic Beanstalk assumes in order to apply lifecycle settings. The version lifecycle configuration defines lifecycle settings for application versions.</p>
newtype ApplicationResourceLifecycleConfig = ApplicationResourceLifecycleConfig 
  { "ServiceRole" :: NullOrUndefined.NullOrUndefined (String)
  , "VersionLifecycleConfig" :: NullOrUndefined.NullOrUndefined (ApplicationVersionLifecycleConfig)
  }
derive instance newtypeApplicationResourceLifecycleConfig :: Newtype ApplicationResourceLifecycleConfig _
derive instance repGenericApplicationResourceLifecycleConfig :: Generic ApplicationResourceLifecycleConfig _
instance showApplicationResourceLifecycleConfig :: Show ApplicationResourceLifecycleConfig where
  show = genericShow
instance decodeApplicationResourceLifecycleConfig :: Decode ApplicationResourceLifecycleConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationResourceLifecycleConfig :: Encode ApplicationResourceLifecycleConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationResourceLifecycleDescriptionMessage = ApplicationResourceLifecycleDescriptionMessage 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "ResourceLifecycleConfig" :: NullOrUndefined.NullOrUndefined (ApplicationResourceLifecycleConfig)
  }
derive instance newtypeApplicationResourceLifecycleDescriptionMessage :: Newtype ApplicationResourceLifecycleDescriptionMessage _
derive instance repGenericApplicationResourceLifecycleDescriptionMessage :: Generic ApplicationResourceLifecycleDescriptionMessage _
instance showApplicationResourceLifecycleDescriptionMessage :: Show ApplicationResourceLifecycleDescriptionMessage where
  show = genericShow
instance decodeApplicationResourceLifecycleDescriptionMessage :: Decode ApplicationResourceLifecycleDescriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationResourceLifecycleDescriptionMessage :: Encode ApplicationResourceLifecycleDescriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the properties of an application version.</p>
newtype ApplicationVersionDescription = ApplicationVersionDescription 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "VersionLabel" :: NullOrUndefined.NullOrUndefined (VersionLabel)
  , "SourceBuildInformation" :: NullOrUndefined.NullOrUndefined (SourceBuildInformation)
  , "BuildArn" :: NullOrUndefined.NullOrUndefined (String)
  , "SourceBundle" :: NullOrUndefined.NullOrUndefined (S3Location)
  , "DateCreated" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined.NullOrUndefined (UpdateDate)
  , "Status" :: NullOrUndefined.NullOrUndefined (ApplicationVersionStatus)
  }
derive instance newtypeApplicationVersionDescription :: Newtype ApplicationVersionDescription _
derive instance repGenericApplicationVersionDescription :: Generic ApplicationVersionDescription _
instance showApplicationVersionDescription :: Show ApplicationVersionDescription where
  show = genericShow
instance decodeApplicationVersionDescription :: Decode ApplicationVersionDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionDescription :: Encode ApplicationVersionDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationVersionDescriptionList = ApplicationVersionDescriptionList (Array ApplicationVersionDescription)
derive instance newtypeApplicationVersionDescriptionList :: Newtype ApplicationVersionDescriptionList _
derive instance repGenericApplicationVersionDescriptionList :: Generic ApplicationVersionDescriptionList _
instance showApplicationVersionDescriptionList :: Show ApplicationVersionDescriptionList where
  show = genericShow
instance decodeApplicationVersionDescriptionList :: Decode ApplicationVersionDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionDescriptionList :: Encode ApplicationVersionDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message wrapping a single description of an application version.</p>
newtype ApplicationVersionDescriptionMessage = ApplicationVersionDescriptionMessage 
  { "ApplicationVersion" :: NullOrUndefined.NullOrUndefined (ApplicationVersionDescription)
  }
derive instance newtypeApplicationVersionDescriptionMessage :: Newtype ApplicationVersionDescriptionMessage _
derive instance repGenericApplicationVersionDescriptionMessage :: Generic ApplicationVersionDescriptionMessage _
instance showApplicationVersionDescriptionMessage :: Show ApplicationVersionDescriptionMessage where
  show = genericShow
instance decodeApplicationVersionDescriptionMessage :: Decode ApplicationVersionDescriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionDescriptionMessage :: Encode ApplicationVersionDescriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message wrapping a list of application version descriptions.</p>
newtype ApplicationVersionDescriptionsMessage = ApplicationVersionDescriptionsMessage 
  { "ApplicationVersions" :: NullOrUndefined.NullOrUndefined (ApplicationVersionDescriptionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeApplicationVersionDescriptionsMessage :: Newtype ApplicationVersionDescriptionsMessage _
derive instance repGenericApplicationVersionDescriptionsMessage :: Generic ApplicationVersionDescriptionsMessage _
instance showApplicationVersionDescriptionsMessage :: Show ApplicationVersionDescriptionsMessage where
  show = genericShow
instance decodeApplicationVersionDescriptionsMessage :: Decode ApplicationVersionDescriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionDescriptionsMessage :: Encode ApplicationVersionDescriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The application version lifecycle settings for an application. Defines the rules that Elastic Beanstalk applies to an application's versions in order to avoid hitting the per-region limit for application versions.</p> <p>When Elastic Beanstalk deletes an application version from its database, you can no longer deploy that version to an environment. The source bundle remains in S3 unless you configure the rule to delete it.</p>
newtype ApplicationVersionLifecycleConfig = ApplicationVersionLifecycleConfig 
  { "MaxCountRule" :: NullOrUndefined.NullOrUndefined (MaxCountRule)
  , "MaxAgeRule" :: NullOrUndefined.NullOrUndefined (MaxAgeRule)
  }
derive instance newtypeApplicationVersionLifecycleConfig :: Newtype ApplicationVersionLifecycleConfig _
derive instance repGenericApplicationVersionLifecycleConfig :: Generic ApplicationVersionLifecycleConfig _
instance showApplicationVersionLifecycleConfig :: Show ApplicationVersionLifecycleConfig where
  show = genericShow
instance decodeApplicationVersionLifecycleConfig :: Decode ApplicationVersionLifecycleConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionLifecycleConfig :: Encode ApplicationVersionLifecycleConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationVersionProccess = ApplicationVersionProccess Boolean
derive instance newtypeApplicationVersionProccess :: Newtype ApplicationVersionProccess _
derive instance repGenericApplicationVersionProccess :: Generic ApplicationVersionProccess _
instance showApplicationVersionProccess :: Show ApplicationVersionProccess where
  show = genericShow
instance decodeApplicationVersionProccess :: Decode ApplicationVersionProccess where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionProccess :: Encode ApplicationVersionProccess where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationVersionStatus = ApplicationVersionStatus String
derive instance newtypeApplicationVersionStatus :: Newtype ApplicationVersionStatus _
derive instance repGenericApplicationVersionStatus :: Generic ApplicationVersionStatus _
instance showApplicationVersionStatus :: Show ApplicationVersionStatus where
  show = genericShow
instance decodeApplicationVersionStatus :: Decode ApplicationVersionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationVersionStatus :: Encode ApplicationVersionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to execute a scheduled managed action immediately.</p>
newtype ApplyEnvironmentManagedActionRequest = ApplyEnvironmentManagedActionRequest 
  { "EnvironmentName" :: NullOrUndefined.NullOrUndefined (String)
  , "EnvironmentId" :: NullOrUndefined.NullOrUndefined (String)
  , "ActionId" :: (String)
  }
derive instance newtypeApplyEnvironmentManagedActionRequest :: Newtype ApplyEnvironmentManagedActionRequest _
derive instance repGenericApplyEnvironmentManagedActionRequest :: Generic ApplyEnvironmentManagedActionRequest _
instance showApplyEnvironmentManagedActionRequest :: Show ApplyEnvironmentManagedActionRequest where
  show = genericShow
instance decodeApplyEnvironmentManagedActionRequest :: Decode ApplyEnvironmentManagedActionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplyEnvironmentManagedActionRequest :: Encode ApplyEnvironmentManagedActionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result message containing information about the managed action.</p>
newtype ApplyEnvironmentManagedActionResult = ApplyEnvironmentManagedActionResult 
  { "ActionId" :: NullOrUndefined.NullOrUndefined (String)
  , "ActionDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "ActionType" :: NullOrUndefined.NullOrUndefined (ActionType)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeApplyEnvironmentManagedActionResult :: Newtype ApplyEnvironmentManagedActionResult _
derive instance repGenericApplyEnvironmentManagedActionResult :: Generic ApplyEnvironmentManagedActionResult _
instance showApplyEnvironmentManagedActionResult :: Show ApplyEnvironmentManagedActionResult where
  show = genericShow
instance decodeApplyEnvironmentManagedActionResult :: Decode ApplyEnvironmentManagedActionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplyEnvironmentManagedActionResult :: Encode ApplyEnvironmentManagedActionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutoCreateApplication = AutoCreateApplication Boolean
derive instance newtypeAutoCreateApplication :: Newtype AutoCreateApplication _
derive instance repGenericAutoCreateApplication :: Generic AutoCreateApplication _
instance showAutoCreateApplication :: Show AutoCreateApplication where
  show = genericShow
instance decodeAutoCreateApplication :: Decode AutoCreateApplication where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoCreateApplication :: Encode AutoCreateApplication where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an Auto Scaling launch configuration.</p>
newtype AutoScalingGroup = AutoScalingGroup 
  { "Name" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeAutoScalingGroup :: Newtype AutoScalingGroup _
derive instance repGenericAutoScalingGroup :: Generic AutoScalingGroup _
instance showAutoScalingGroup :: Show AutoScalingGroup where
  show = genericShow
instance decodeAutoScalingGroup :: Decode AutoScalingGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoScalingGroup :: Encode AutoScalingGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AutoScalingGroupList = AutoScalingGroupList (Array AutoScalingGroup)
derive instance newtypeAutoScalingGroupList :: Newtype AutoScalingGroupList _
derive instance repGenericAutoScalingGroupList :: Generic AutoScalingGroupList _
instance showAutoScalingGroupList :: Show AutoScalingGroupList where
  show = genericShow
instance decodeAutoScalingGroupList :: Decode AutoScalingGroupList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAutoScalingGroupList :: Encode AutoScalingGroupList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailableSolutionStackDetailsList = AvailableSolutionStackDetailsList (Array SolutionStackDescription)
derive instance newtypeAvailableSolutionStackDetailsList :: Newtype AvailableSolutionStackDetailsList _
derive instance repGenericAvailableSolutionStackDetailsList :: Generic AvailableSolutionStackDetailsList _
instance showAvailableSolutionStackDetailsList :: Show AvailableSolutionStackDetailsList where
  show = genericShow
instance decodeAvailableSolutionStackDetailsList :: Decode AvailableSolutionStackDetailsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailableSolutionStackDetailsList :: Encode AvailableSolutionStackDetailsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailableSolutionStackNamesList = AvailableSolutionStackNamesList (Array SolutionStackName)
derive instance newtypeAvailableSolutionStackNamesList :: Newtype AvailableSolutionStackNamesList _
derive instance repGenericAvailableSolutionStackNamesList :: Generic AvailableSolutionStackNamesList _
instance showAvailableSolutionStackNamesList :: Show AvailableSolutionStackNamesList where
  show = genericShow
instance decodeAvailableSolutionStackNamesList :: Decode AvailableSolutionStackNamesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailableSolutionStackNamesList :: Encode AvailableSolutionStackNamesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BoxedBoolean = BoxedBoolean Boolean
derive instance newtypeBoxedBoolean :: Newtype BoxedBoolean _
derive instance repGenericBoxedBoolean :: Generic BoxedBoolean _
instance showBoxedBoolean :: Show BoxedBoolean where
  show = genericShow
instance decodeBoxedBoolean :: Decode BoxedBoolean where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBoxedBoolean :: Encode BoxedBoolean where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BoxedInt = BoxedInt Int
derive instance newtypeBoxedInt :: Newtype BoxedInt _
derive instance repGenericBoxedInt :: Generic BoxedInt _
instance showBoxedInt :: Show BoxedInt where
  show = genericShow
instance decodeBoxedInt :: Decode BoxedInt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBoxedInt :: Encode BoxedInt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Settings for an AWS CodeBuild build.</p>
newtype BuildConfiguration = BuildConfiguration 
  { "ArtifactName" :: NullOrUndefined.NullOrUndefined (String)
  , "CodeBuildServiceRole" :: (NonEmptyString)
  , "ComputeType" :: NullOrUndefined.NullOrUndefined (ComputeType)
  , "Image" :: (NonEmptyString)
  , "TimeoutInMinutes" :: NullOrUndefined.NullOrUndefined (BoxedInt)
  }
derive instance newtypeBuildConfiguration :: Newtype BuildConfiguration _
derive instance repGenericBuildConfiguration :: Generic BuildConfiguration _
instance showBuildConfiguration :: Show BuildConfiguration where
  show = genericShow
instance decodeBuildConfiguration :: Decode BuildConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildConfiguration :: Encode BuildConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The builder used to build the custom platform.</p>
newtype Builder = Builder 
  { "ARN" :: NullOrUndefined.NullOrUndefined (ARN)
  }
derive instance newtypeBuilder :: Newtype Builder _
derive instance repGenericBuilder :: Generic Builder _
instance showBuilder :: Show Builder where
  show = genericShow
instance decodeBuilder :: Decode Builder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuilder :: Encode Builder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CPU utilization metrics for an instance.</p>
newtype CPUUtilization = CPUUtilization 
  { "User" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "Nice" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "System" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "Idle" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "IOWait" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "IRQ" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "SoftIRQ" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  }
derive instance newtypeCPUUtilization :: Newtype CPUUtilization _
derive instance repGenericCPUUtilization :: Generic CPUUtilization _
instance showCPUUtilization :: Show CPUUtilization where
  show = genericShow
instance decodeCPUUtilization :: Decode CPUUtilization where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCPUUtilization :: Encode CPUUtilization where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Cause = Cause String
derive instance newtypeCause :: Newtype Cause _
derive instance repGenericCause :: Generic Cause _
instance showCause :: Show Cause where
  show = genericShow
instance decodeCause :: Decode Cause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCause :: Encode Cause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Causes = Causes (Array Cause)
derive instance newtypeCauses :: Newtype Causes _
derive instance repGenericCauses :: Generic Causes _
instance showCauses :: Show Causes where
  show = genericShow
instance decodeCauses :: Decode Causes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCauses :: Encode Causes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Results message indicating whether a CNAME is available.</p>
newtype CheckDNSAvailabilityMessage = CheckDNSAvailabilityMessage 
  { "CNAMEPrefix" :: (DNSCnamePrefix)
  }
derive instance newtypeCheckDNSAvailabilityMessage :: Newtype CheckDNSAvailabilityMessage _
derive instance repGenericCheckDNSAvailabilityMessage :: Generic CheckDNSAvailabilityMessage _
instance showCheckDNSAvailabilityMessage :: Show CheckDNSAvailabilityMessage where
  show = genericShow
instance decodeCheckDNSAvailabilityMessage :: Decode CheckDNSAvailabilityMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCheckDNSAvailabilityMessage :: Encode CheckDNSAvailabilityMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Indicates if the specified CNAME is available.</p>
newtype CheckDNSAvailabilityResultMessage = CheckDNSAvailabilityResultMessage 
  { "Available" :: NullOrUndefined.NullOrUndefined (CnameAvailability)
  , "FullyQualifiedCNAME" :: NullOrUndefined.NullOrUndefined (DNSCname)
  }
derive instance newtypeCheckDNSAvailabilityResultMessage :: Newtype CheckDNSAvailabilityResultMessage _
derive instance repGenericCheckDNSAvailabilityResultMessage :: Generic CheckDNSAvailabilityResultMessage _
instance showCheckDNSAvailabilityResultMessage :: Show CheckDNSAvailabilityResultMessage where
  show = genericShow
instance decodeCheckDNSAvailabilityResultMessage :: Decode CheckDNSAvailabilityResultMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCheckDNSAvailabilityResultMessage :: Encode CheckDNSAvailabilityResultMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CnameAvailability = CnameAvailability Boolean
derive instance newtypeCnameAvailability :: Newtype CnameAvailability _
derive instance repGenericCnameAvailability :: Generic CnameAvailability _
instance showCnameAvailability :: Show CnameAvailability where
  show = genericShow
instance decodeCnameAvailability :: Decode CnameAvailability where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCnameAvailability :: Encode CnameAvailability where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>AWS CodeBuild is not available in the specified region.</p>
newtype CodeBuildNotInServiceRegionException = CodeBuildNotInServiceRegionException Types.NoArguments
derive instance newtypeCodeBuildNotInServiceRegionException :: Newtype CodeBuildNotInServiceRegionException _
derive instance repGenericCodeBuildNotInServiceRegionException :: Generic CodeBuildNotInServiceRegionException _
instance showCodeBuildNotInServiceRegionException :: Show CodeBuildNotInServiceRegionException where
  show = genericShow
instance decodeCodeBuildNotInServiceRegionException :: Decode CodeBuildNotInServiceRegionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeBuildNotInServiceRegionException :: Encode CodeBuildNotInServiceRegionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to create or update a group of environments.</p>
newtype ComposeEnvironmentsMessage = ComposeEnvironmentsMessage 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "GroupName" :: NullOrUndefined.NullOrUndefined (GroupName)
  , "VersionLabels" :: NullOrUndefined.NullOrUndefined (VersionLabels)
  }
derive instance newtypeComposeEnvironmentsMessage :: Newtype ComposeEnvironmentsMessage _
derive instance repGenericComposeEnvironmentsMessage :: Generic ComposeEnvironmentsMessage _
instance showComposeEnvironmentsMessage :: Show ComposeEnvironmentsMessage where
  show = genericShow
instance decodeComposeEnvironmentsMessage :: Decode ComposeEnvironmentsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComposeEnvironmentsMessage :: Encode ComposeEnvironmentsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComputeType = ComputeType String
derive instance newtypeComputeType :: Newtype ComputeType _
derive instance repGenericComputeType :: Generic ComputeType _
instance showComputeType :: Show ComputeType where
  show = genericShow
instance decodeComputeType :: Decode ComputeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeType :: Encode ComputeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationDeploymentStatus = ConfigurationDeploymentStatus String
derive instance newtypeConfigurationDeploymentStatus :: Newtype ConfigurationDeploymentStatus _
derive instance repGenericConfigurationDeploymentStatus :: Generic ConfigurationDeploymentStatus _
instance showConfigurationDeploymentStatus :: Show ConfigurationDeploymentStatus where
  show = genericShow
instance decodeConfigurationDeploymentStatus :: Decode ConfigurationDeploymentStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationDeploymentStatus :: Encode ConfigurationDeploymentStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionDefaultValue = ConfigurationOptionDefaultValue String
derive instance newtypeConfigurationOptionDefaultValue :: Newtype ConfigurationOptionDefaultValue _
derive instance repGenericConfigurationOptionDefaultValue :: Generic ConfigurationOptionDefaultValue _
instance showConfigurationOptionDefaultValue :: Show ConfigurationOptionDefaultValue where
  show = genericShow
instance decodeConfigurationOptionDefaultValue :: Decode ConfigurationOptionDefaultValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionDefaultValue :: Encode ConfigurationOptionDefaultValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the possible values for a configuration option.</p>
newtype ConfigurationOptionDescription = ConfigurationOptionDescription 
  { "Namespace" :: NullOrUndefined.NullOrUndefined (OptionNamespace)
  , "Name" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionName)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionDefaultValue)
  , "ChangeSeverity" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionSeverity)
  , "UserDefined" :: NullOrUndefined.NullOrUndefined (UserDefinedOption)
  , "ValueType" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionValueType)
  , "ValueOptions" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionPossibleValues)
  , "MinValue" :: NullOrUndefined.NullOrUndefined (OptionRestrictionMinValue)
  , "MaxValue" :: NullOrUndefined.NullOrUndefined (OptionRestrictionMaxValue)
  , "MaxLength" :: NullOrUndefined.NullOrUndefined (OptionRestrictionMaxLength)
  , "Regex" :: NullOrUndefined.NullOrUndefined (OptionRestrictionRegex)
  }
derive instance newtypeConfigurationOptionDescription :: Newtype ConfigurationOptionDescription _
derive instance repGenericConfigurationOptionDescription :: Generic ConfigurationOptionDescription _
instance showConfigurationOptionDescription :: Show ConfigurationOptionDescription where
  show = genericShow
instance decodeConfigurationOptionDescription :: Decode ConfigurationOptionDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionDescription :: Encode ConfigurationOptionDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionDescriptionsList = ConfigurationOptionDescriptionsList (Array ConfigurationOptionDescription)
derive instance newtypeConfigurationOptionDescriptionsList :: Newtype ConfigurationOptionDescriptionsList _
derive instance repGenericConfigurationOptionDescriptionsList :: Generic ConfigurationOptionDescriptionsList _
instance showConfigurationOptionDescriptionsList :: Show ConfigurationOptionDescriptionsList where
  show = genericShow
instance decodeConfigurationOptionDescriptionsList :: Decode ConfigurationOptionDescriptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionDescriptionsList :: Encode ConfigurationOptionDescriptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionName = ConfigurationOptionName String
derive instance newtypeConfigurationOptionName :: Newtype ConfigurationOptionName _
derive instance repGenericConfigurationOptionName :: Generic ConfigurationOptionName _
instance showConfigurationOptionName :: Show ConfigurationOptionName where
  show = genericShow
instance decodeConfigurationOptionName :: Decode ConfigurationOptionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionName :: Encode ConfigurationOptionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionPossibleValue = ConfigurationOptionPossibleValue String
derive instance newtypeConfigurationOptionPossibleValue :: Newtype ConfigurationOptionPossibleValue _
derive instance repGenericConfigurationOptionPossibleValue :: Generic ConfigurationOptionPossibleValue _
instance showConfigurationOptionPossibleValue :: Show ConfigurationOptionPossibleValue where
  show = genericShow
instance decodeConfigurationOptionPossibleValue :: Decode ConfigurationOptionPossibleValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionPossibleValue :: Encode ConfigurationOptionPossibleValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionPossibleValues = ConfigurationOptionPossibleValues (Array ConfigurationOptionPossibleValue)
derive instance newtypeConfigurationOptionPossibleValues :: Newtype ConfigurationOptionPossibleValues _
derive instance repGenericConfigurationOptionPossibleValues :: Generic ConfigurationOptionPossibleValues _
instance showConfigurationOptionPossibleValues :: Show ConfigurationOptionPossibleValues where
  show = genericShow
instance decodeConfigurationOptionPossibleValues :: Decode ConfigurationOptionPossibleValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionPossibleValues :: Encode ConfigurationOptionPossibleValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A specification identifying an individual configuration option along with its current value. For a list of possible option values, go to <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options.html">Option Values</a> in the <i>AWS Elastic Beanstalk Developer Guide</i>. </p>
newtype ConfigurationOptionSetting = ConfigurationOptionSetting 
  { "ResourceName" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Namespace" :: NullOrUndefined.NullOrUndefined (OptionNamespace)
  , "OptionName" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionName)
  , "Value" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionValue)
  }
derive instance newtypeConfigurationOptionSetting :: Newtype ConfigurationOptionSetting _
derive instance repGenericConfigurationOptionSetting :: Generic ConfigurationOptionSetting _
instance showConfigurationOptionSetting :: Show ConfigurationOptionSetting where
  show = genericShow
instance decodeConfigurationOptionSetting :: Decode ConfigurationOptionSetting where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionSetting :: Encode ConfigurationOptionSetting where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionSettingsList = ConfigurationOptionSettingsList (Array ConfigurationOptionSetting)
derive instance newtypeConfigurationOptionSettingsList :: Newtype ConfigurationOptionSettingsList _
derive instance repGenericConfigurationOptionSettingsList :: Generic ConfigurationOptionSettingsList _
instance showConfigurationOptionSettingsList :: Show ConfigurationOptionSettingsList where
  show = genericShow
instance decodeConfigurationOptionSettingsList :: Decode ConfigurationOptionSettingsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionSettingsList :: Encode ConfigurationOptionSettingsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionSeverity = ConfigurationOptionSeverity String
derive instance newtypeConfigurationOptionSeverity :: Newtype ConfigurationOptionSeverity _
derive instance repGenericConfigurationOptionSeverity :: Generic ConfigurationOptionSeverity _
instance showConfigurationOptionSeverity :: Show ConfigurationOptionSeverity where
  show = genericShow
instance decodeConfigurationOptionSeverity :: Decode ConfigurationOptionSeverity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionSeverity :: Encode ConfigurationOptionSeverity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionValue = ConfigurationOptionValue String
derive instance newtypeConfigurationOptionValue :: Newtype ConfigurationOptionValue _
derive instance repGenericConfigurationOptionValue :: Generic ConfigurationOptionValue _
instance showConfigurationOptionValue :: Show ConfigurationOptionValue where
  show = genericShow
instance decodeConfigurationOptionValue :: Decode ConfigurationOptionValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionValue :: Encode ConfigurationOptionValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationOptionValueType = ConfigurationOptionValueType String
derive instance newtypeConfigurationOptionValueType :: Newtype ConfigurationOptionValueType _
derive instance repGenericConfigurationOptionValueType :: Generic ConfigurationOptionValueType _
instance showConfigurationOptionValueType :: Show ConfigurationOptionValueType where
  show = genericShow
instance decodeConfigurationOptionValueType :: Decode ConfigurationOptionValueType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionValueType :: Encode ConfigurationOptionValueType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the settings for a specified configuration set.</p>
newtype ConfigurationOptionsDescription = ConfigurationOptionsDescription 
  { "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "Options" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionDescriptionsList)
  }
derive instance newtypeConfigurationOptionsDescription :: Newtype ConfigurationOptionsDescription _
derive instance repGenericConfigurationOptionsDescription :: Generic ConfigurationOptionsDescription _
instance showConfigurationOptionsDescription :: Show ConfigurationOptionsDescription where
  show = genericShow
instance decodeConfigurationOptionsDescription :: Decode ConfigurationOptionsDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationOptionsDescription :: Encode ConfigurationOptionsDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the settings for a configuration set.</p>
newtype ConfigurationSettingsDescription = ConfigurationSettingsDescription 
  { "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "DeploymentStatus" :: NullOrUndefined.NullOrUndefined (ConfigurationDeploymentStatus)
  , "DateCreated" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined.NullOrUndefined (UpdateDate)
  , "OptionSettings" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionSettingsList)
  }
derive instance newtypeConfigurationSettingsDescription :: Newtype ConfigurationSettingsDescription _
derive instance repGenericConfigurationSettingsDescription :: Generic ConfigurationSettingsDescription _
instance showConfigurationSettingsDescription :: Show ConfigurationSettingsDescription where
  show = genericShow
instance decodeConfigurationSettingsDescription :: Decode ConfigurationSettingsDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSettingsDescription :: Encode ConfigurationSettingsDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationSettingsDescriptionList = ConfigurationSettingsDescriptionList (Array ConfigurationSettingsDescription)
derive instance newtypeConfigurationSettingsDescriptionList :: Newtype ConfigurationSettingsDescriptionList _
derive instance repGenericConfigurationSettingsDescriptionList :: Generic ConfigurationSettingsDescriptionList _
instance showConfigurationSettingsDescriptionList :: Show ConfigurationSettingsDescriptionList where
  show = genericShow
instance decodeConfigurationSettingsDescriptionList :: Decode ConfigurationSettingsDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSettingsDescriptionList :: Encode ConfigurationSettingsDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The results from a request to change the configuration settings of an environment.</p>
newtype ConfigurationSettingsDescriptions = ConfigurationSettingsDescriptions 
  { "ConfigurationSettings" :: NullOrUndefined.NullOrUndefined (ConfigurationSettingsDescriptionList)
  }
derive instance newtypeConfigurationSettingsDescriptions :: Newtype ConfigurationSettingsDescriptions _
derive instance repGenericConfigurationSettingsDescriptions :: Generic ConfigurationSettingsDescriptions _
instance showConfigurationSettingsDescriptions :: Show ConfigurationSettingsDescriptions where
  show = genericShow
instance decodeConfigurationSettingsDescriptions :: Decode ConfigurationSettingsDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSettingsDescriptions :: Encode ConfigurationSettingsDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Provides a list of validation messages.</p>
newtype ConfigurationSettingsValidationMessages = ConfigurationSettingsValidationMessages 
  { "Messages" :: NullOrUndefined.NullOrUndefined (ValidationMessagesList)
  }
derive instance newtypeConfigurationSettingsValidationMessages :: Newtype ConfigurationSettingsValidationMessages _
derive instance repGenericConfigurationSettingsValidationMessages :: Generic ConfigurationSettingsValidationMessages _
instance showConfigurationSettingsValidationMessages :: Show ConfigurationSettingsValidationMessages where
  show = genericShow
instance decodeConfigurationSettingsValidationMessages :: Decode ConfigurationSettingsValidationMessages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationSettingsValidationMessages :: Encode ConfigurationSettingsValidationMessages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationTemplateName = ConfigurationTemplateName String
derive instance newtypeConfigurationTemplateName :: Newtype ConfigurationTemplateName _
derive instance repGenericConfigurationTemplateName :: Generic ConfigurationTemplateName _
instance showConfigurationTemplateName :: Show ConfigurationTemplateName where
  show = genericShow
instance decodeConfigurationTemplateName :: Decode ConfigurationTemplateName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationTemplateName :: Encode ConfigurationTemplateName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationTemplateNamesList = ConfigurationTemplateNamesList (Array ConfigurationTemplateName)
derive instance newtypeConfigurationTemplateNamesList :: Newtype ConfigurationTemplateNamesList _
derive instance repGenericConfigurationTemplateNamesList :: Generic ConfigurationTemplateNamesList _
instance showConfigurationTemplateNamesList :: Show ConfigurationTemplateNamesList where
  show = genericShow
instance decodeConfigurationTemplateNamesList :: Decode ConfigurationTemplateNamesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationTemplateNamesList :: Encode ConfigurationTemplateNamesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to create an application.</p>
newtype CreateApplicationMessage = CreateApplicationMessage 
  { "ApplicationName" :: (ApplicationName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "ResourceLifecycleConfig" :: NullOrUndefined.NullOrUndefined (ApplicationResourceLifecycleConfig)
  }
derive instance newtypeCreateApplicationMessage :: Newtype CreateApplicationMessage _
derive instance repGenericCreateApplicationMessage :: Generic CreateApplicationMessage _
instance showCreateApplicationMessage :: Show CreateApplicationMessage where
  show = genericShow
instance decodeCreateApplicationMessage :: Decode CreateApplicationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationMessage :: Encode CreateApplicationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateApplicationVersionMessage = CreateApplicationVersionMessage 
  { "ApplicationName" :: (ApplicationName)
  , "VersionLabel" :: (VersionLabel)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "SourceBuildInformation" :: NullOrUndefined.NullOrUndefined (SourceBuildInformation)
  , "SourceBundle" :: NullOrUndefined.NullOrUndefined (S3Location)
  , "BuildConfiguration" :: NullOrUndefined.NullOrUndefined (BuildConfiguration)
  , "AutoCreateApplication" :: NullOrUndefined.NullOrUndefined (AutoCreateApplication)
  , "Process" :: NullOrUndefined.NullOrUndefined (ApplicationVersionProccess)
  }
derive instance newtypeCreateApplicationVersionMessage :: Newtype CreateApplicationVersionMessage _
derive instance repGenericCreateApplicationVersionMessage :: Generic CreateApplicationVersionMessage _
instance showCreateApplicationVersionMessage :: Show CreateApplicationVersionMessage where
  show = genericShow
instance decodeCreateApplicationVersionMessage :: Decode CreateApplicationVersionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationVersionMessage :: Encode CreateApplicationVersionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to create a configuration template.</p>
newtype CreateConfigurationTemplateMessage = CreateConfigurationTemplateMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: (ConfigurationTemplateName)
  , "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "SourceConfiguration" :: NullOrUndefined.NullOrUndefined (SourceConfiguration)
  , "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "OptionSettings" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionSettingsList)
  }
derive instance newtypeCreateConfigurationTemplateMessage :: Newtype CreateConfigurationTemplateMessage _
derive instance repGenericCreateConfigurationTemplateMessage :: Generic CreateConfigurationTemplateMessage _
instance showCreateConfigurationTemplateMessage :: Show CreateConfigurationTemplateMessage where
  show = genericShow
instance decodeCreateConfigurationTemplateMessage :: Decode CreateConfigurationTemplateMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConfigurationTemplateMessage :: Encode CreateConfigurationTemplateMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype CreateEnvironmentMessage = CreateEnvironmentMessage 
  { "ApplicationName" :: (ApplicationName)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "GroupName" :: NullOrUndefined.NullOrUndefined (GroupName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "CNAMEPrefix" :: NullOrUndefined.NullOrUndefined (DNSCnamePrefix)
  , "Tier" :: NullOrUndefined.NullOrUndefined (EnvironmentTier)
  , "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "VersionLabel" :: NullOrUndefined.NullOrUndefined (VersionLabel)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "OptionSettings" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionSettingsList)
  , "OptionsToRemove" :: NullOrUndefined.NullOrUndefined (OptionsSpecifierList)
  }
derive instance newtypeCreateEnvironmentMessage :: Newtype CreateEnvironmentMessage _
derive instance repGenericCreateEnvironmentMessage :: Generic CreateEnvironmentMessage _
instance showCreateEnvironmentMessage :: Show CreateEnvironmentMessage where
  show = genericShow
instance decodeCreateEnvironmentMessage :: Decode CreateEnvironmentMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateEnvironmentMessage :: Encode CreateEnvironmentMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to create a new platform version.</p>
newtype CreatePlatformVersionRequest = CreatePlatformVersionRequest 
  { "PlatformName" :: (PlatformName)
  , "PlatformVersion" :: (PlatformVersion)
  , "PlatformDefinitionBundle" :: (S3Location)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "OptionSettings" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionSettingsList)
  }
derive instance newtypeCreatePlatformVersionRequest :: Newtype CreatePlatformVersionRequest _
derive instance repGenericCreatePlatformVersionRequest :: Generic CreatePlatformVersionRequest _
instance showCreatePlatformVersionRequest :: Show CreatePlatformVersionRequest where
  show = genericShow
instance decodeCreatePlatformVersionRequest :: Decode CreatePlatformVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlatformVersionRequest :: Encode CreatePlatformVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePlatformVersionResult = CreatePlatformVersionResult 
  { "PlatformSummary" :: NullOrUndefined.NullOrUndefined (PlatformSummary)
  , "Builder" :: NullOrUndefined.NullOrUndefined (Builder)
  }
derive instance newtypeCreatePlatformVersionResult :: Newtype CreatePlatformVersionResult _
derive instance repGenericCreatePlatformVersionResult :: Generic CreatePlatformVersionResult _
instance showCreatePlatformVersionResult :: Show CreatePlatformVersionResult where
  show = genericShow
instance decodeCreatePlatformVersionResult :: Decode CreatePlatformVersionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePlatformVersionResult :: Encode CreatePlatformVersionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Results of a <a>CreateStorageLocationResult</a> call.</p>
newtype CreateStorageLocationResultMessage = CreateStorageLocationResultMessage 
  { "S3Bucket" :: NullOrUndefined.NullOrUndefined (S3Bucket)
  }
derive instance newtypeCreateStorageLocationResultMessage :: Newtype CreateStorageLocationResultMessage _
derive instance repGenericCreateStorageLocationResultMessage :: Generic CreateStorageLocationResultMessage _
instance showCreateStorageLocationResultMessage :: Show CreateStorageLocationResultMessage where
  show = genericShow
instance decodeCreateStorageLocationResultMessage :: Decode CreateStorageLocationResultMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStorageLocationResultMessage :: Encode CreateStorageLocationResultMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreationDate = CreationDate Number
derive instance newtypeCreationDate :: Newtype CreationDate _
derive instance repGenericCreationDate :: Generic CreationDate _
instance showCreationDate :: Show CreationDate where
  show = genericShow
instance decodeCreationDate :: Decode CreationDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreationDate :: Encode CreationDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A custom AMI available to platforms.</p>
newtype CustomAmi = CustomAmi 
  { "VirtualizationType" :: NullOrUndefined.NullOrUndefined (VirtualizationType)
  , "ImageId" :: NullOrUndefined.NullOrUndefined (ImageId)
  }
derive instance newtypeCustomAmi :: Newtype CustomAmi _
derive instance repGenericCustomAmi :: Generic CustomAmi _
instance showCustomAmi :: Show CustomAmi where
  show = genericShow
instance decodeCustomAmi :: Decode CustomAmi where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomAmi :: Encode CustomAmi where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomAmiList = CustomAmiList (Array CustomAmi)
derive instance newtypeCustomAmiList :: Newtype CustomAmiList _
derive instance repGenericCustomAmiList :: Generic CustomAmiList _
instance showCustomAmiList :: Show CustomAmiList where
  show = genericShow
instance decodeCustomAmiList :: Decode CustomAmiList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomAmiList :: Encode CustomAmiList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DNSCname = DNSCname String
derive instance newtypeDNSCname :: Newtype DNSCname _
derive instance repGenericDNSCname :: Generic DNSCname _
instance showDNSCname :: Show DNSCname where
  show = genericShow
instance decodeDNSCname :: Decode DNSCname where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDNSCname :: Encode DNSCname where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DNSCnamePrefix = DNSCnamePrefix String
derive instance newtypeDNSCnamePrefix :: Newtype DNSCnamePrefix _
derive instance repGenericDNSCnamePrefix :: Generic DNSCnamePrefix _
instance showDNSCnamePrefix :: Show DNSCnamePrefix where
  show = genericShow
instance decodeDNSCnamePrefix :: Decode DNSCnamePrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDNSCnamePrefix :: Encode DNSCnamePrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete an application.</p>
newtype DeleteApplicationMessage = DeleteApplicationMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TerminateEnvByForce" :: NullOrUndefined.NullOrUndefined (TerminateEnvForce)
  }
derive instance newtypeDeleteApplicationMessage :: Newtype DeleteApplicationMessage _
derive instance repGenericDeleteApplicationMessage :: Generic DeleteApplicationMessage _
instance showDeleteApplicationMessage :: Show DeleteApplicationMessage where
  show = genericShow
instance decodeDeleteApplicationMessage :: Decode DeleteApplicationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationMessage :: Encode DeleteApplicationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete an application version.</p>
newtype DeleteApplicationVersionMessage = DeleteApplicationVersionMessage 
  { "ApplicationName" :: (ApplicationName)
  , "VersionLabel" :: (VersionLabel)
  , "DeleteSourceBundle" :: NullOrUndefined.NullOrUndefined (DeleteSourceBundle)
  }
derive instance newtypeDeleteApplicationVersionMessage :: Newtype DeleteApplicationVersionMessage _
derive instance repGenericDeleteApplicationVersionMessage :: Generic DeleteApplicationVersionMessage _
instance showDeleteApplicationVersionMessage :: Show DeleteApplicationVersionMessage where
  show = genericShow
instance decodeDeleteApplicationVersionMessage :: Decode DeleteApplicationVersionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationVersionMessage :: Encode DeleteApplicationVersionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete a configuration template.</p>
newtype DeleteConfigurationTemplateMessage = DeleteConfigurationTemplateMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: (ConfigurationTemplateName)
  }
derive instance newtypeDeleteConfigurationTemplateMessage :: Newtype DeleteConfigurationTemplateMessage _
derive instance repGenericDeleteConfigurationTemplateMessage :: Generic DeleteConfigurationTemplateMessage _
instance showDeleteConfigurationTemplateMessage :: Show DeleteConfigurationTemplateMessage where
  show = genericShow
instance decodeDeleteConfigurationTemplateMessage :: Decode DeleteConfigurationTemplateMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConfigurationTemplateMessage :: Encode DeleteConfigurationTemplateMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to delete a draft environment configuration.</p>
newtype DeleteEnvironmentConfigurationMessage = DeleteEnvironmentConfigurationMessage 
  { "ApplicationName" :: (ApplicationName)
  , "EnvironmentName" :: (EnvironmentName)
  }
derive instance newtypeDeleteEnvironmentConfigurationMessage :: Newtype DeleteEnvironmentConfigurationMessage _
derive instance repGenericDeleteEnvironmentConfigurationMessage :: Generic DeleteEnvironmentConfigurationMessage _
instance showDeleteEnvironmentConfigurationMessage :: Show DeleteEnvironmentConfigurationMessage where
  show = genericShow
instance decodeDeleteEnvironmentConfigurationMessage :: Decode DeleteEnvironmentConfigurationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteEnvironmentConfigurationMessage :: Encode DeleteEnvironmentConfigurationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePlatformVersionRequest = DeletePlatformVersionRequest 
  { "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  }
derive instance newtypeDeletePlatformVersionRequest :: Newtype DeletePlatformVersionRequest _
derive instance repGenericDeletePlatformVersionRequest :: Generic DeletePlatformVersionRequest _
instance showDeletePlatformVersionRequest :: Show DeletePlatformVersionRequest where
  show = genericShow
instance decodeDeletePlatformVersionRequest :: Decode DeletePlatformVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePlatformVersionRequest :: Encode DeletePlatformVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePlatformVersionResult = DeletePlatformVersionResult 
  { "PlatformSummary" :: NullOrUndefined.NullOrUndefined (PlatformSummary)
  }
derive instance newtypeDeletePlatformVersionResult :: Newtype DeletePlatformVersionResult _
derive instance repGenericDeletePlatformVersionResult :: Generic DeletePlatformVersionResult _
instance showDeletePlatformVersionResult :: Show DeletePlatformVersionResult where
  show = genericShow
instance decodeDeletePlatformVersionResult :: Decode DeletePlatformVersionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePlatformVersionResult :: Encode DeletePlatformVersionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteSourceBundle = DeleteSourceBundle Boolean
derive instance newtypeDeleteSourceBundle :: Newtype DeleteSourceBundle _
derive instance repGenericDeleteSourceBundle :: Generic DeleteSourceBundle _
instance showDeleteSourceBundle :: Show DeleteSourceBundle where
  show = genericShow
instance decodeDeleteSourceBundle :: Decode DeleteSourceBundle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSourceBundle :: Encode DeleteSourceBundle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an application version deployment.</p>
newtype Deployment = Deployment 
  { "VersionLabel" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined.NullOrUndefined (NullableLong)
  , "Status" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentTime" :: NullOrUndefined.NullOrUndefined (DeploymentTimestamp)
  }
derive instance newtypeDeployment :: Newtype Deployment _
derive instance repGenericDeployment :: Generic Deployment _
instance showDeployment :: Show Deployment where
  show = genericShow
instance decodeDeployment :: Decode Deployment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeployment :: Encode Deployment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeploymentTimestamp = DeploymentTimestamp Number
derive instance newtypeDeploymentTimestamp :: Newtype DeploymentTimestamp _
derive instance repGenericDeploymentTimestamp :: Generic DeploymentTimestamp _
instance showDeploymentTimestamp :: Show DeploymentTimestamp where
  show = genericShow
instance decodeDeploymentTimestamp :: Decode DeploymentTimestamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeploymentTimestamp :: Encode DeploymentTimestamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe application versions.</p>
newtype DescribeApplicationVersionsMessage = DescribeApplicationVersionsMessage 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "VersionLabels" :: NullOrUndefined.NullOrUndefined (VersionLabelsList)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeDescribeApplicationVersionsMessage :: Newtype DescribeApplicationVersionsMessage _
derive instance repGenericDescribeApplicationVersionsMessage :: Generic DescribeApplicationVersionsMessage _
instance showDescribeApplicationVersionsMessage :: Show DescribeApplicationVersionsMessage where
  show = genericShow
instance decodeDescribeApplicationVersionsMessage :: Decode DescribeApplicationVersionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeApplicationVersionsMessage :: Encode DescribeApplicationVersionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe one or more applications.</p>
newtype DescribeApplicationsMessage = DescribeApplicationsMessage 
  { "ApplicationNames" :: NullOrUndefined.NullOrUndefined (ApplicationNamesList)
  }
derive instance newtypeDescribeApplicationsMessage :: Newtype DescribeApplicationsMessage _
derive instance repGenericDescribeApplicationsMessage :: Generic DescribeApplicationsMessage _
instance showDescribeApplicationsMessage :: Show DescribeApplicationsMessage where
  show = genericShow
instance decodeDescribeApplicationsMessage :: Decode DescribeApplicationsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeApplicationsMessage :: Encode DescribeApplicationsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message containing a list of application version descriptions.</p>
newtype DescribeConfigurationOptionsMessage = DescribeConfigurationOptionsMessage 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "Options" :: NullOrUndefined.NullOrUndefined (OptionsSpecifierList)
  }
derive instance newtypeDescribeConfigurationOptionsMessage :: Newtype DescribeConfigurationOptionsMessage _
derive instance repGenericDescribeConfigurationOptionsMessage :: Generic DescribeConfigurationOptionsMessage _
instance showDescribeConfigurationOptionsMessage :: Show DescribeConfigurationOptionsMessage where
  show = genericShow
instance decodeDescribeConfigurationOptionsMessage :: Decode DescribeConfigurationOptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationOptionsMessage :: Encode DescribeConfigurationOptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message containing all of the configuration settings for a specified solution stack or configuration template.</p>
newtype DescribeConfigurationSettingsMessage = DescribeConfigurationSettingsMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  }
derive instance newtypeDescribeConfigurationSettingsMessage :: Newtype DescribeConfigurationSettingsMessage _
derive instance repGenericDescribeConfigurationSettingsMessage :: Generic DescribeConfigurationSettingsMessage _
instance showDescribeConfigurationSettingsMessage :: Show DescribeConfigurationSettingsMessage where
  show = genericShow
instance decodeDescribeConfigurationSettingsMessage :: Decode DescribeConfigurationSettingsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationSettingsMessage :: Encode DescribeConfigurationSettingsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>See the example below to learn how to create a request body.</p>
newtype DescribeEnvironmentHealthRequest = DescribeEnvironmentHealthRequest 
  { "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "AttributeNames" :: NullOrUndefined.NullOrUndefined (EnvironmentHealthAttributes)
  }
derive instance newtypeDescribeEnvironmentHealthRequest :: Newtype DescribeEnvironmentHealthRequest _
derive instance repGenericDescribeEnvironmentHealthRequest :: Generic DescribeEnvironmentHealthRequest _
instance showDescribeEnvironmentHealthRequest :: Show DescribeEnvironmentHealthRequest where
  show = genericShow
instance decodeDescribeEnvironmentHealthRequest :: Decode DescribeEnvironmentHealthRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEnvironmentHealthRequest :: Encode DescribeEnvironmentHealthRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Health details for an AWS Elastic Beanstalk environment.</p>
newtype DescribeEnvironmentHealthResult = DescribeEnvironmentHealthResult 
  { "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "HealthStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (EnvironmentHealth)
  , "Color" :: NullOrUndefined.NullOrUndefined (String)
  , "Causes" :: NullOrUndefined.NullOrUndefined (Causes)
  , "ApplicationMetrics" :: NullOrUndefined.NullOrUndefined (ApplicationMetrics)
  , "InstancesHealth" :: NullOrUndefined.NullOrUndefined (InstanceHealthSummary)
  , "RefreshedAt" :: NullOrUndefined.NullOrUndefined (RefreshedAt)
  }
derive instance newtypeDescribeEnvironmentHealthResult :: Newtype DescribeEnvironmentHealthResult _
derive instance repGenericDescribeEnvironmentHealthResult :: Generic DescribeEnvironmentHealthResult _
instance showDescribeEnvironmentHealthResult :: Show DescribeEnvironmentHealthResult where
  show = genericShow
instance decodeDescribeEnvironmentHealthResult :: Decode DescribeEnvironmentHealthResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEnvironmentHealthResult :: Encode DescribeEnvironmentHealthResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to list completed and failed managed actions.</p>
newtype DescribeEnvironmentManagedActionHistoryRequest = DescribeEnvironmentManagedActionHistoryRequest 
  { "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeDescribeEnvironmentManagedActionHistoryRequest :: Newtype DescribeEnvironmentManagedActionHistoryRequest _
derive instance repGenericDescribeEnvironmentManagedActionHistoryRequest :: Generic DescribeEnvironmentManagedActionHistoryRequest _
instance showDescribeEnvironmentManagedActionHistoryRequest :: Show DescribeEnvironmentManagedActionHistoryRequest where
  show = genericShow
instance decodeDescribeEnvironmentManagedActionHistoryRequest :: Decode DescribeEnvironmentManagedActionHistoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEnvironmentManagedActionHistoryRequest :: Encode DescribeEnvironmentManagedActionHistoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A result message containing a list of completed and failed managed actions.</p>
newtype DescribeEnvironmentManagedActionHistoryResult = DescribeEnvironmentManagedActionHistoryResult 
  { "ManagedActionHistoryItems" :: NullOrUndefined.NullOrUndefined (ManagedActionHistoryItems)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeEnvironmentManagedActionHistoryResult :: Newtype DescribeEnvironmentManagedActionHistoryResult _
derive instance repGenericDescribeEnvironmentManagedActionHistoryResult :: Generic DescribeEnvironmentManagedActionHistoryResult _
instance showDescribeEnvironmentManagedActionHistoryResult :: Show DescribeEnvironmentManagedActionHistoryResult where
  show = genericShow
instance decodeDescribeEnvironmentManagedActionHistoryResult :: Decode DescribeEnvironmentManagedActionHistoryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEnvironmentManagedActionHistoryResult :: Encode DescribeEnvironmentManagedActionHistoryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to list an environment's upcoming and in-progress managed actions.</p>
newtype DescribeEnvironmentManagedActionsRequest = DescribeEnvironmentManagedActionsRequest 
  { "EnvironmentName" :: NullOrUndefined.NullOrUndefined (String)
  , "EnvironmentId" :: NullOrUndefined.NullOrUndefined (String)
  , "Status" :: NullOrUndefined.NullOrUndefined (ActionStatus)
  }
derive instance newtypeDescribeEnvironmentManagedActionsRequest :: Newtype DescribeEnvironmentManagedActionsRequest _
derive instance repGenericDescribeEnvironmentManagedActionsRequest :: Generic DescribeEnvironmentManagedActionsRequest _
instance showDescribeEnvironmentManagedActionsRequest :: Show DescribeEnvironmentManagedActionsRequest where
  show = genericShow
instance decodeDescribeEnvironmentManagedActionsRequest :: Decode DescribeEnvironmentManagedActionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEnvironmentManagedActionsRequest :: Encode DescribeEnvironmentManagedActionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result message containing a list of managed actions.</p>
newtype DescribeEnvironmentManagedActionsResult = DescribeEnvironmentManagedActionsResult 
  { "ManagedActions" :: NullOrUndefined.NullOrUndefined (ManagedActions)
  }
derive instance newtypeDescribeEnvironmentManagedActionsResult :: Newtype DescribeEnvironmentManagedActionsResult _
derive instance repGenericDescribeEnvironmentManagedActionsResult :: Generic DescribeEnvironmentManagedActionsResult _
instance showDescribeEnvironmentManagedActionsResult :: Show DescribeEnvironmentManagedActionsResult where
  show = genericShow
instance decodeDescribeEnvironmentManagedActionsResult :: Decode DescribeEnvironmentManagedActionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEnvironmentManagedActionsResult :: Encode DescribeEnvironmentManagedActionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe the resources in an environment.</p>
newtype DescribeEnvironmentResourcesMessage = DescribeEnvironmentResourcesMessage 
  { "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  }
derive instance newtypeDescribeEnvironmentResourcesMessage :: Newtype DescribeEnvironmentResourcesMessage _
derive instance repGenericDescribeEnvironmentResourcesMessage :: Generic DescribeEnvironmentResourcesMessage _
instance showDescribeEnvironmentResourcesMessage :: Show DescribeEnvironmentResourcesMessage where
  show = genericShow
instance decodeDescribeEnvironmentResourcesMessage :: Decode DescribeEnvironmentResourcesMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEnvironmentResourcesMessage :: Encode DescribeEnvironmentResourcesMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to describe one or more environments.</p>
newtype DescribeEnvironmentsMessage = DescribeEnvironmentsMessage 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "VersionLabel" :: NullOrUndefined.NullOrUndefined (VersionLabel)
  , "EnvironmentIds" :: NullOrUndefined.NullOrUndefined (EnvironmentIdList)
  , "EnvironmentNames" :: NullOrUndefined.NullOrUndefined (EnvironmentNamesList)
  , "IncludeDeleted" :: NullOrUndefined.NullOrUndefined (IncludeDeleted)
  , "IncludedDeletedBackTo" :: NullOrUndefined.NullOrUndefined (IncludeDeletedBackTo)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeDescribeEnvironmentsMessage :: Newtype DescribeEnvironmentsMessage _
derive instance repGenericDescribeEnvironmentsMessage :: Generic DescribeEnvironmentsMessage _
instance showDescribeEnvironmentsMessage :: Show DescribeEnvironmentsMessage where
  show = genericShow
instance decodeDescribeEnvironmentsMessage :: Decode DescribeEnvironmentsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEnvironmentsMessage :: Encode DescribeEnvironmentsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to retrieve a list of events for an environment.</p>
newtype DescribeEventsMessage = DescribeEventsMessage 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "VersionLabel" :: NullOrUndefined.NullOrUndefined (VersionLabel)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  , "Severity" :: NullOrUndefined.NullOrUndefined (EventSeverity)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (TimeFilterStart)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (TimeFilterEnd)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (MaxRecords)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeDescribeEventsMessage :: Newtype DescribeEventsMessage _
derive instance repGenericDescribeEventsMessage :: Generic DescribeEventsMessage _
instance showDescribeEventsMessage :: Show DescribeEventsMessage where
  show = genericShow
instance decodeDescribeEventsMessage :: Decode DescribeEventsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventsMessage :: Encode DescribeEventsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Parameters for a call to <code>DescribeInstancesHealth</code>.</p>
newtype DescribeInstancesHealthRequest = DescribeInstancesHealthRequest 
  { "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "AttributeNames" :: NullOrUndefined.NullOrUndefined (InstancesHealthAttributes)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancesHealthRequest :: Newtype DescribeInstancesHealthRequest _
derive instance repGenericDescribeInstancesHealthRequest :: Generic DescribeInstancesHealthRequest _
instance showDescribeInstancesHealthRequest :: Show DescribeInstancesHealthRequest where
  show = genericShow
instance decodeDescribeInstancesHealthRequest :: Decode DescribeInstancesHealthRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancesHealthRequest :: Encode DescribeInstancesHealthRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed health information about the Amazon EC2 instances in an AWS Elastic Beanstalk environment.</p>
newtype DescribeInstancesHealthResult = DescribeInstancesHealthResult 
  { "InstanceHealthList" :: NullOrUndefined.NullOrUndefined (InstanceHealthList)
  , "RefreshedAt" :: NullOrUndefined.NullOrUndefined (RefreshedAt)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeInstancesHealthResult :: Newtype DescribeInstancesHealthResult _
derive instance repGenericDescribeInstancesHealthResult :: Generic DescribeInstancesHealthResult _
instance showDescribeInstancesHealthResult :: Show DescribeInstancesHealthResult where
  show = genericShow
instance decodeDescribeInstancesHealthResult :: Decode DescribeInstancesHealthResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeInstancesHealthResult :: Encode DescribeInstancesHealthResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePlatformVersionRequest = DescribePlatformVersionRequest 
  { "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  }
derive instance newtypeDescribePlatformVersionRequest :: Newtype DescribePlatformVersionRequest _
derive instance repGenericDescribePlatformVersionRequest :: Generic DescribePlatformVersionRequest _
instance showDescribePlatformVersionRequest :: Show DescribePlatformVersionRequest where
  show = genericShow
instance decodeDescribePlatformVersionRequest :: Decode DescribePlatformVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePlatformVersionRequest :: Encode DescribePlatformVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribePlatformVersionResult = DescribePlatformVersionResult 
  { "PlatformDescription" :: NullOrUndefined.NullOrUndefined (PlatformDescription)
  }
derive instance newtypeDescribePlatformVersionResult :: Newtype DescribePlatformVersionResult _
derive instance repGenericDescribePlatformVersionResult :: Generic DescribePlatformVersionResult _
instance showDescribePlatformVersionResult :: Show DescribePlatformVersionResult where
  show = genericShow
instance decodeDescribePlatformVersionResult :: Decode DescribePlatformVersionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribePlatformVersionResult :: Encode DescribePlatformVersionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Ec2InstanceId = Ec2InstanceId String
derive instance newtypeEc2InstanceId :: Newtype Ec2InstanceId _
derive instance repGenericEc2InstanceId :: Generic Ec2InstanceId _
instance showEc2InstanceId :: Show Ec2InstanceId where
  show = genericShow
instance decodeEc2InstanceId :: Decode Ec2InstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEc2InstanceId :: Encode Ec2InstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A generic service exception has occurred.</p>
newtype ElasticBeanstalkServiceException = ElasticBeanstalkServiceException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeElasticBeanstalkServiceException :: Newtype ElasticBeanstalkServiceException _
derive instance repGenericElasticBeanstalkServiceException :: Generic ElasticBeanstalkServiceException _
instance showElasticBeanstalkServiceException :: Show ElasticBeanstalkServiceException where
  show = genericShow
instance decodeElasticBeanstalkServiceException :: Decode ElasticBeanstalkServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticBeanstalkServiceException :: Encode ElasticBeanstalkServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EndpointURL = EndpointURL String
derive instance newtypeEndpointURL :: Newtype EndpointURL _
derive instance repGenericEndpointURL :: Generic EndpointURL _
instance showEndpointURL :: Show EndpointURL where
  show = genericShow
instance decodeEndpointURL :: Decode EndpointURL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndpointURL :: Encode EndpointURL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentArn = EnvironmentArn String
derive instance newtypeEnvironmentArn :: Newtype EnvironmentArn _
derive instance repGenericEnvironmentArn :: Generic EnvironmentArn _
instance showEnvironmentArn :: Show EnvironmentArn where
  show = genericShow
instance decodeEnvironmentArn :: Decode EnvironmentArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentArn :: Encode EnvironmentArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the properties of an environment.</p>
newtype EnvironmentDescription = EnvironmentDescription 
  { "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "VersionLabel" :: NullOrUndefined.NullOrUndefined (VersionLabel)
  , "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "EndpointURL" :: NullOrUndefined.NullOrUndefined (EndpointURL)
  , "CNAME" :: NullOrUndefined.NullOrUndefined (DNSCname)
  , "DateCreated" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined.NullOrUndefined (UpdateDate)
  , "Status" :: NullOrUndefined.NullOrUndefined (EnvironmentStatus)
  , "AbortableOperationInProgress" :: NullOrUndefined.NullOrUndefined (AbortableOperationInProgress)
  , "Health" :: NullOrUndefined.NullOrUndefined (EnvironmentHealth)
  , "HealthStatus" :: NullOrUndefined.NullOrUndefined (EnvironmentHealthStatus)
  , "Resources" :: NullOrUndefined.NullOrUndefined (EnvironmentResourcesDescription)
  , "Tier" :: NullOrUndefined.NullOrUndefined (EnvironmentTier)
  , "EnvironmentLinks" :: NullOrUndefined.NullOrUndefined (EnvironmentLinks)
  , "EnvironmentArn" :: NullOrUndefined.NullOrUndefined (EnvironmentArn)
  }
derive instance newtypeEnvironmentDescription :: Newtype EnvironmentDescription _
derive instance repGenericEnvironmentDescription :: Generic EnvironmentDescription _
instance showEnvironmentDescription :: Show EnvironmentDescription where
  show = genericShow
instance decodeEnvironmentDescription :: Decode EnvironmentDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentDescription :: Encode EnvironmentDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentDescriptionsList = EnvironmentDescriptionsList (Array EnvironmentDescription)
derive instance newtypeEnvironmentDescriptionsList :: Newtype EnvironmentDescriptionsList _
derive instance repGenericEnvironmentDescriptionsList :: Generic EnvironmentDescriptionsList _
instance showEnvironmentDescriptionsList :: Show EnvironmentDescriptionsList where
  show = genericShow
instance decodeEnvironmentDescriptionsList :: Decode EnvironmentDescriptionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentDescriptionsList :: Encode EnvironmentDescriptionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message containing a list of environment descriptions.</p>
newtype EnvironmentDescriptionsMessage = EnvironmentDescriptionsMessage 
  { "Environments" :: NullOrUndefined.NullOrUndefined (EnvironmentDescriptionsList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeEnvironmentDescriptionsMessage :: Newtype EnvironmentDescriptionsMessage _
derive instance repGenericEnvironmentDescriptionsMessage :: Generic EnvironmentDescriptionsMessage _
instance showEnvironmentDescriptionsMessage :: Show EnvironmentDescriptionsMessage where
  show = genericShow
instance decodeEnvironmentDescriptionsMessage :: Decode EnvironmentDescriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentDescriptionsMessage :: Encode EnvironmentDescriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentHealth = EnvironmentHealth String
derive instance newtypeEnvironmentHealth :: Newtype EnvironmentHealth _
derive instance repGenericEnvironmentHealth :: Generic EnvironmentHealth _
instance showEnvironmentHealth :: Show EnvironmentHealth where
  show = genericShow
instance decodeEnvironmentHealth :: Decode EnvironmentHealth where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentHealth :: Encode EnvironmentHealth where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentHealthAttribute = EnvironmentHealthAttribute String
derive instance newtypeEnvironmentHealthAttribute :: Newtype EnvironmentHealthAttribute _
derive instance repGenericEnvironmentHealthAttribute :: Generic EnvironmentHealthAttribute _
instance showEnvironmentHealthAttribute :: Show EnvironmentHealthAttribute where
  show = genericShow
instance decodeEnvironmentHealthAttribute :: Decode EnvironmentHealthAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentHealthAttribute :: Encode EnvironmentHealthAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentHealthAttributes = EnvironmentHealthAttributes (Array EnvironmentHealthAttribute)
derive instance newtypeEnvironmentHealthAttributes :: Newtype EnvironmentHealthAttributes _
derive instance repGenericEnvironmentHealthAttributes :: Generic EnvironmentHealthAttributes _
instance showEnvironmentHealthAttributes :: Show EnvironmentHealthAttributes where
  show = genericShow
instance decodeEnvironmentHealthAttributes :: Decode EnvironmentHealthAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentHealthAttributes :: Encode EnvironmentHealthAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentHealthStatus = EnvironmentHealthStatus String
derive instance newtypeEnvironmentHealthStatus :: Newtype EnvironmentHealthStatus _
derive instance repGenericEnvironmentHealthStatus :: Generic EnvironmentHealthStatus _
instance showEnvironmentHealthStatus :: Show EnvironmentHealthStatus where
  show = genericShow
instance decodeEnvironmentHealthStatus :: Decode EnvironmentHealthStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentHealthStatus :: Encode EnvironmentHealthStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentId = EnvironmentId String
derive instance newtypeEnvironmentId :: Newtype EnvironmentId _
derive instance repGenericEnvironmentId :: Generic EnvironmentId _
instance showEnvironmentId :: Show EnvironmentId where
  show = genericShow
instance decodeEnvironmentId :: Decode EnvironmentId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentId :: Encode EnvironmentId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentIdList = EnvironmentIdList (Array EnvironmentId)
derive instance newtypeEnvironmentIdList :: Newtype EnvironmentIdList _
derive instance repGenericEnvironmentIdList :: Generic EnvironmentIdList _
instance showEnvironmentIdList :: Show EnvironmentIdList where
  show = genericShow
instance decodeEnvironmentIdList :: Decode EnvironmentIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentIdList :: Encode EnvironmentIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The information retrieved from the Amazon EC2 instances.</p>
newtype EnvironmentInfoDescription = EnvironmentInfoDescription 
  { "InfoType" :: NullOrUndefined.NullOrUndefined (EnvironmentInfoType)
  , "Ec2InstanceId" :: NullOrUndefined.NullOrUndefined (Ec2InstanceId)
  , "SampleTimestamp" :: NullOrUndefined.NullOrUndefined (SampleTimestamp)
  , "Message" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeEnvironmentInfoDescription :: Newtype EnvironmentInfoDescription _
derive instance repGenericEnvironmentInfoDescription :: Generic EnvironmentInfoDescription _
instance showEnvironmentInfoDescription :: Show EnvironmentInfoDescription where
  show = genericShow
instance decodeEnvironmentInfoDescription :: Decode EnvironmentInfoDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentInfoDescription :: Encode EnvironmentInfoDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentInfoDescriptionList = EnvironmentInfoDescriptionList (Array EnvironmentInfoDescription)
derive instance newtypeEnvironmentInfoDescriptionList :: Newtype EnvironmentInfoDescriptionList _
derive instance repGenericEnvironmentInfoDescriptionList :: Generic EnvironmentInfoDescriptionList _
instance showEnvironmentInfoDescriptionList :: Show EnvironmentInfoDescriptionList where
  show = genericShow
instance decodeEnvironmentInfoDescriptionList :: Decode EnvironmentInfoDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentInfoDescriptionList :: Encode EnvironmentInfoDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentInfoType = EnvironmentInfoType String
derive instance newtypeEnvironmentInfoType :: Newtype EnvironmentInfoType _
derive instance repGenericEnvironmentInfoType :: Generic EnvironmentInfoType _
instance showEnvironmentInfoType :: Show EnvironmentInfoType where
  show = genericShow
instance decodeEnvironmentInfoType :: Decode EnvironmentInfoType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentInfoType :: Encode EnvironmentInfoType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A link to another environment, defined in the environment's manifest. Links provide connection information in system properties that can be used to connect to another environment in the same group. See <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html">Environment Manifest (env.yaml)</a> for details.</p>
newtype EnvironmentLink = EnvironmentLink 
  { "LinkName" :: NullOrUndefined.NullOrUndefined (String)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEnvironmentLink :: Newtype EnvironmentLink _
derive instance repGenericEnvironmentLink :: Generic EnvironmentLink _
instance showEnvironmentLink :: Show EnvironmentLink where
  show = genericShow
instance decodeEnvironmentLink :: Decode EnvironmentLink where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentLink :: Encode EnvironmentLink where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentLinks = EnvironmentLinks (Array EnvironmentLink)
derive instance newtypeEnvironmentLinks :: Newtype EnvironmentLinks _
derive instance repGenericEnvironmentLinks :: Generic EnvironmentLinks _
instance showEnvironmentLinks :: Show EnvironmentLinks where
  show = genericShow
instance decodeEnvironmentLinks :: Decode EnvironmentLinks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentLinks :: Encode EnvironmentLinks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentName = EnvironmentName String
derive instance newtypeEnvironmentName :: Newtype EnvironmentName _
derive instance repGenericEnvironmentName :: Generic EnvironmentName _
instance showEnvironmentName :: Show EnvironmentName where
  show = genericShow
instance decodeEnvironmentName :: Decode EnvironmentName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentName :: Encode EnvironmentName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentNamesList = EnvironmentNamesList (Array EnvironmentName)
derive instance newtypeEnvironmentNamesList :: Newtype EnvironmentNamesList _
derive instance repGenericEnvironmentNamesList :: Generic EnvironmentNamesList _
instance showEnvironmentNamesList :: Show EnvironmentNamesList where
  show = genericShow
instance decodeEnvironmentNamesList :: Decode EnvironmentNamesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentNamesList :: Encode EnvironmentNamesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the AWS resources in use by this environment. This data is live.</p>
newtype EnvironmentResourceDescription = EnvironmentResourceDescription 
  { "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "AutoScalingGroups" :: NullOrUndefined.NullOrUndefined (AutoScalingGroupList)
  , "Instances" :: NullOrUndefined.NullOrUndefined (InstanceList)
  , "LaunchConfigurations" :: NullOrUndefined.NullOrUndefined (LaunchConfigurationList)
  , "LoadBalancers" :: NullOrUndefined.NullOrUndefined (LoadBalancerList)
  , "Triggers" :: NullOrUndefined.NullOrUndefined (TriggerList)
  , "Queues" :: NullOrUndefined.NullOrUndefined (QueueList)
  }
derive instance newtypeEnvironmentResourceDescription :: Newtype EnvironmentResourceDescription _
derive instance repGenericEnvironmentResourceDescription :: Generic EnvironmentResourceDescription _
instance showEnvironmentResourceDescription :: Show EnvironmentResourceDescription where
  show = genericShow
instance decodeEnvironmentResourceDescription :: Decode EnvironmentResourceDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentResourceDescription :: Encode EnvironmentResourceDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message containing a list of environment resource descriptions.</p>
newtype EnvironmentResourceDescriptionsMessage = EnvironmentResourceDescriptionsMessage 
  { "EnvironmentResources" :: NullOrUndefined.NullOrUndefined (EnvironmentResourceDescription)
  }
derive instance newtypeEnvironmentResourceDescriptionsMessage :: Newtype EnvironmentResourceDescriptionsMessage _
derive instance repGenericEnvironmentResourceDescriptionsMessage :: Generic EnvironmentResourceDescriptionsMessage _
instance showEnvironmentResourceDescriptionsMessage :: Show EnvironmentResourceDescriptionsMessage where
  show = genericShow
instance decodeEnvironmentResourceDescriptionsMessage :: Decode EnvironmentResourceDescriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentResourceDescriptionsMessage :: Encode EnvironmentResourceDescriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the AWS resources in use by this environment. This data is not live data.</p>
newtype EnvironmentResourcesDescription = EnvironmentResourcesDescription 
  { "LoadBalancer" :: NullOrUndefined.NullOrUndefined (LoadBalancerDescription)
  }
derive instance newtypeEnvironmentResourcesDescription :: Newtype EnvironmentResourcesDescription _
derive instance repGenericEnvironmentResourcesDescription :: Generic EnvironmentResourcesDescription _
instance showEnvironmentResourcesDescription :: Show EnvironmentResourcesDescription where
  show = genericShow
instance decodeEnvironmentResourcesDescription :: Decode EnvironmentResourcesDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentResourcesDescription :: Encode EnvironmentResourcesDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentStatus = EnvironmentStatus String
derive instance newtypeEnvironmentStatus :: Newtype EnvironmentStatus _
derive instance repGenericEnvironmentStatus :: Generic EnvironmentStatus _
instance showEnvironmentStatus :: Show EnvironmentStatus where
  show = genericShow
instance decodeEnvironmentStatus :: Decode EnvironmentStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentStatus :: Encode EnvironmentStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the properties of an environment tier</p>
newtype EnvironmentTier = EnvironmentTier 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeEnvironmentTier :: Newtype EnvironmentTier _
derive instance repGenericEnvironmentTier :: Generic EnvironmentTier _
instance showEnvironmentTier :: Show EnvironmentTier where
  show = genericShow
instance decodeEnvironmentTier :: Decode EnvironmentTier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentTier :: Encode EnvironmentTier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventDate = EventDate Number
derive instance newtypeEventDate :: Newtype EventDate _
derive instance repGenericEventDate :: Generic EventDate _
instance showEventDate :: Show EventDate where
  show = genericShow
instance decodeEventDate :: Decode EventDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDate :: Encode EventDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an event.</p>
newtype EventDescription = EventDescription 
  { "EventDate" :: NullOrUndefined.NullOrUndefined (EventDate)
  , "Message" :: NullOrUndefined.NullOrUndefined (EventMessage)
  , "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "VersionLabel" :: NullOrUndefined.NullOrUndefined (VersionLabel)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  , "Severity" :: NullOrUndefined.NullOrUndefined (EventSeverity)
  }
derive instance newtypeEventDescription :: Newtype EventDescription _
derive instance repGenericEventDescription :: Generic EventDescription _
instance showEventDescription :: Show EventDescription where
  show = genericShow
instance decodeEventDescription :: Decode EventDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDescription :: Encode EventDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventDescriptionList = EventDescriptionList (Array EventDescription)
derive instance newtypeEventDescriptionList :: Newtype EventDescriptionList _
derive instance repGenericEventDescriptionList :: Generic EventDescriptionList _
instance showEventDescriptionList :: Show EventDescriptionList where
  show = genericShow
instance decodeEventDescriptionList :: Decode EventDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDescriptionList :: Encode EventDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message wrapping a list of event descriptions.</p>
newtype EventDescriptionsMessage = EventDescriptionsMessage 
  { "Events" :: NullOrUndefined.NullOrUndefined (EventDescriptionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeEventDescriptionsMessage :: Newtype EventDescriptionsMessage _
derive instance repGenericEventDescriptionsMessage :: Generic EventDescriptionsMessage _
instance showEventDescriptionsMessage :: Show EventDescriptionsMessage where
  show = genericShow
instance decodeEventDescriptionsMessage :: Decode EventDescriptionsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventDescriptionsMessage :: Encode EventDescriptionsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventMessage = EventMessage String
derive instance newtypeEventMessage :: Newtype EventMessage _
derive instance repGenericEventMessage :: Generic EventMessage _
instance showEventMessage :: Show EventMessage where
  show = genericShow
instance decodeEventMessage :: Decode EventMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventMessage :: Encode EventMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventSeverity = EventSeverity String
derive instance newtypeEventSeverity :: Newtype EventSeverity _
derive instance repGenericEventSeverity :: Generic EventSeverity _
instance showEventSeverity :: Show EventSeverity where
  show = genericShow
instance decodeEventSeverity :: Decode EventSeverity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventSeverity :: Encode EventSeverity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _
derive instance repGenericExceptionMessage :: Generic ExceptionMessage _
instance showExceptionMessage :: Show ExceptionMessage where
  show = genericShow
instance decodeExceptionMessage :: Decode ExceptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExceptionMessage :: Encode ExceptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailureType = FailureType String
derive instance newtypeFailureType :: Newtype FailureType _
derive instance repGenericFailureType :: Generic FailureType _
instance showFailureType :: Show FailureType where
  show = genericShow
instance decodeFailureType :: Decode FailureType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureType :: Encode FailureType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FileTypeExtension = FileTypeExtension String
derive instance newtypeFileTypeExtension :: Newtype FileTypeExtension _
derive instance repGenericFileTypeExtension :: Generic FileTypeExtension _
instance showFileTypeExtension :: Show FileTypeExtension where
  show = genericShow
instance decodeFileTypeExtension :: Decode FileTypeExtension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFileTypeExtension :: Encode FileTypeExtension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ForceTerminate = ForceTerminate Boolean
derive instance newtypeForceTerminate :: Newtype ForceTerminate _
derive instance repGenericForceTerminate :: Generic ForceTerminate _
instance showForceTerminate :: Show ForceTerminate where
  show = genericShow
instance decodeForceTerminate :: Decode ForceTerminate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeForceTerminate :: Encode ForceTerminate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupName = GroupName String
derive instance newtypeGroupName :: Newtype GroupName _
derive instance repGenericGroupName :: Generic GroupName _
instance showGroupName :: Show GroupName where
  show = genericShow
instance decodeGroupName :: Decode GroupName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupName :: Encode GroupName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImageId = ImageId String
derive instance newtypeImageId :: Newtype ImageId _
derive instance repGenericImageId :: Generic ImageId _
instance showImageId :: Show ImageId where
  show = genericShow
instance decodeImageId :: Decode ImageId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageId :: Encode ImageId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IncludeDeleted = IncludeDeleted Boolean
derive instance newtypeIncludeDeleted :: Newtype IncludeDeleted _
derive instance repGenericIncludeDeleted :: Generic IncludeDeleted _
instance showIncludeDeleted :: Show IncludeDeleted where
  show = genericShow
instance decodeIncludeDeleted :: Decode IncludeDeleted where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncludeDeleted :: Encode IncludeDeleted where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IncludeDeletedBackTo = IncludeDeletedBackTo Number
derive instance newtypeIncludeDeletedBackTo :: Newtype IncludeDeletedBackTo _
derive instance repGenericIncludeDeletedBackTo :: Generic IncludeDeletedBackTo _
instance showIncludeDeletedBackTo :: Show IncludeDeletedBackTo where
  show = genericShow
instance decodeIncludeDeletedBackTo :: Decode IncludeDeletedBackTo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncludeDeletedBackTo :: Encode IncludeDeletedBackTo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The description of an Amazon EC2 instance.</p>
newtype Instance = Instance 
  { "Id" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeInstance :: Newtype Instance _
derive instance repGenericInstance :: Generic Instance _
instance showInstance :: Show Instance where
  show = genericShow
instance decodeInstance :: Decode Instance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstance :: Encode Instance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceHealthList = InstanceHealthList (Array SingleInstanceHealth)
derive instance newtypeInstanceHealthList :: Newtype InstanceHealthList _
derive instance repGenericInstanceHealthList :: Generic InstanceHealthList _
instance showInstanceHealthList :: Show InstanceHealthList where
  show = genericShow
instance decodeInstanceHealthList :: Decode InstanceHealthList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceHealthList :: Encode InstanceHealthList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents summary information about the health of an instance. For more information, see <a href="http://docs.aws.amazon.com/elasticbeanstalk/latest/dg/health-enhanced-status.html">Health Colors and Statuses</a>.</p>
newtype InstanceHealthSummary = InstanceHealthSummary 
  { "NoData" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Unknown" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Pending" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Ok" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Info" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Warning" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Degraded" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Severe" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeInstanceHealthSummary :: Newtype InstanceHealthSummary _
derive instance repGenericInstanceHealthSummary :: Generic InstanceHealthSummary _
instance showInstanceHealthSummary :: Show InstanceHealthSummary where
  show = genericShow
instance decodeInstanceHealthSummary :: Decode InstanceHealthSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceHealthSummary :: Encode InstanceHealthSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceId = InstanceId String
derive instance newtypeInstanceId :: Newtype InstanceId _
derive instance repGenericInstanceId :: Generic InstanceId _
instance showInstanceId :: Show InstanceId where
  show = genericShow
instance decodeInstanceId :: Decode InstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceId :: Encode InstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceList = InstanceList (Array Instance)
derive instance newtypeInstanceList :: Newtype InstanceList _
derive instance repGenericInstanceList :: Generic InstanceList _
instance showInstanceList :: Show InstanceList where
  show = genericShow
instance decodeInstanceList :: Decode InstanceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceList :: Encode InstanceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancesHealthAttribute = InstancesHealthAttribute String
derive instance newtypeInstancesHealthAttribute :: Newtype InstancesHealthAttribute _
derive instance repGenericInstancesHealthAttribute :: Generic InstancesHealthAttribute _
instance showInstancesHealthAttribute :: Show InstancesHealthAttribute where
  show = genericShow
instance decodeInstancesHealthAttribute :: Decode InstancesHealthAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancesHealthAttribute :: Encode InstancesHealthAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancesHealthAttributes = InstancesHealthAttributes (Array InstancesHealthAttribute)
derive instance newtypeInstancesHealthAttributes :: Newtype InstancesHealthAttributes _
derive instance repGenericInstancesHealthAttributes :: Generic InstancesHealthAttributes _
instance showInstancesHealthAttributes :: Show InstancesHealthAttributes where
  show = genericShow
instance decodeInstancesHealthAttributes :: Decode InstancesHealthAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancesHealthAttributes :: Encode InstancesHealthAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified account does not have sufficient privileges for one of more AWS services.</p>
newtype InsufficientPrivilegesException = InsufficientPrivilegesException Types.NoArguments
derive instance newtypeInsufficientPrivilegesException :: Newtype InsufficientPrivilegesException _
derive instance repGenericInsufficientPrivilegesException :: Generic InsufficientPrivilegesException _
instance showInsufficientPrivilegesException :: Show InsufficientPrivilegesException where
  show = genericShow
instance decodeInsufficientPrivilegesException :: Decode InsufficientPrivilegesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientPrivilegesException :: Encode InsufficientPrivilegesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more input parameters is not valid. Please correct the input parameters and try the operation again.</p>
newtype InvalidRequestException = InvalidRequestException Types.NoArguments
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _
derive instance repGenericInvalidRequestException :: Generic InvalidRequestException _
instance showInvalidRequestException :: Show InvalidRequestException where
  show = genericShow
instance decodeInvalidRequestException :: Decode InvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequestException :: Encode InvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the average latency for the slowest X percent of requests over the last 10 seconds.</p>
newtype Latency = Latency 
  { "P999" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "P99" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "P95" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "P90" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "P85" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "P75" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "P50" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  , "P10" :: NullOrUndefined.NullOrUndefined (NullableDouble)
  }
derive instance newtypeLatency :: Newtype Latency _
derive instance repGenericLatency :: Generic Latency _
instance showLatency :: Show Latency where
  show = genericShow
instance decodeLatency :: Decode Latency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLatency :: Encode Latency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an Auto Scaling launch configuration.</p>
newtype LaunchConfiguration = LaunchConfiguration 
  { "Name" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeLaunchConfiguration :: Newtype LaunchConfiguration _
derive instance repGenericLaunchConfiguration :: Generic LaunchConfiguration _
instance showLaunchConfiguration :: Show LaunchConfiguration where
  show = genericShow
instance decodeLaunchConfiguration :: Decode LaunchConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLaunchConfiguration :: Encode LaunchConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LaunchConfigurationList = LaunchConfigurationList (Array LaunchConfiguration)
derive instance newtypeLaunchConfigurationList :: Newtype LaunchConfigurationList _
derive instance repGenericLaunchConfigurationList :: Generic LaunchConfigurationList _
instance showLaunchConfigurationList :: Show LaunchConfigurationList where
  show = genericShow
instance decodeLaunchConfigurationList :: Decode LaunchConfigurationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLaunchConfigurationList :: Encode LaunchConfigurationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LaunchedAt = LaunchedAt Number
derive instance newtypeLaunchedAt :: Newtype LaunchedAt _
derive instance repGenericLaunchedAt :: Generic LaunchedAt _
instance showLaunchedAt :: Show LaunchedAt where
  show = genericShow
instance decodeLaunchedAt :: Decode LaunchedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLaunchedAt :: Encode LaunchedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of available AWS Elastic Beanstalk solution stacks.</p>
newtype ListAvailableSolutionStacksResultMessage = ListAvailableSolutionStacksResultMessage 
  { "SolutionStacks" :: NullOrUndefined.NullOrUndefined (AvailableSolutionStackNamesList)
  , "SolutionStackDetails" :: NullOrUndefined.NullOrUndefined (AvailableSolutionStackDetailsList)
  }
derive instance newtypeListAvailableSolutionStacksResultMessage :: Newtype ListAvailableSolutionStacksResultMessage _
derive instance repGenericListAvailableSolutionStacksResultMessage :: Generic ListAvailableSolutionStacksResultMessage _
instance showListAvailableSolutionStacksResultMessage :: Show ListAvailableSolutionStacksResultMessage where
  show = genericShow
instance decodeListAvailableSolutionStacksResultMessage :: Decode ListAvailableSolutionStacksResultMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAvailableSolutionStacksResultMessage :: Encode ListAvailableSolutionStacksResultMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPlatformVersionsRequest = ListPlatformVersionsRequest 
  { "Filters" :: NullOrUndefined.NullOrUndefined (PlatformFilters)
  , "MaxRecords" :: NullOrUndefined.NullOrUndefined (PlatformMaxRecords)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeListPlatformVersionsRequest :: Newtype ListPlatformVersionsRequest _
derive instance repGenericListPlatformVersionsRequest :: Generic ListPlatformVersionsRequest _
instance showListPlatformVersionsRequest :: Show ListPlatformVersionsRequest where
  show = genericShow
instance decodeListPlatformVersionsRequest :: Decode ListPlatformVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPlatformVersionsRequest :: Encode ListPlatformVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListPlatformVersionsResult = ListPlatformVersionsResult 
  { "PlatformSummaryList" :: NullOrUndefined.NullOrUndefined (PlatformSummaryList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeListPlatformVersionsResult :: Newtype ListPlatformVersionsResult _
derive instance repGenericListPlatformVersionsResult :: Generic ListPlatformVersionsResult _
instance showListPlatformVersionsResult :: Show ListPlatformVersionsResult where
  show = genericShow
instance decodeListPlatformVersionsResult :: Decode ListPlatformVersionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPlatformVersionsResult :: Encode ListPlatformVersionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceMessage = ListTagsForResourceMessage 
  { "ResourceArn" :: (ResourceArn)
  }
derive instance newtypeListTagsForResourceMessage :: Newtype ListTagsForResourceMessage _
derive instance repGenericListTagsForResourceMessage :: Generic ListTagsForResourceMessage _
instance showListTagsForResourceMessage :: Show ListTagsForResourceMessage where
  show = genericShow
instance decodeListTagsForResourceMessage :: Decode ListTagsForResourceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceMessage :: Encode ListTagsForResourceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the properties of a Listener for the LoadBalancer.</p>
newtype Listener = Listener 
  { "Protocol" :: NullOrUndefined.NullOrUndefined (String)
  , "Port" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeListener :: Newtype Listener _
derive instance repGenericListener :: Generic Listener _
instance showListener :: Show Listener where
  show = genericShow
instance decodeListener :: Decode Listener where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListener :: Encode Listener where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadAverage = LoadAverage (Array LoadAverageValue)
derive instance newtypeLoadAverage :: Newtype LoadAverage _
derive instance repGenericLoadAverage :: Generic LoadAverage _
instance showLoadAverage :: Show LoadAverage where
  show = genericShow
instance decodeLoadAverage :: Decode LoadAverage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadAverage :: Encode LoadAverage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadAverageValue = LoadAverageValue Number
derive instance newtypeLoadAverageValue :: Newtype LoadAverageValue _
derive instance repGenericLoadAverageValue :: Generic LoadAverageValue _
instance showLoadAverageValue :: Show LoadAverageValue where
  show = genericShow
instance decodeLoadAverageValue :: Decode LoadAverageValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadAverageValue :: Encode LoadAverageValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a LoadBalancer.</p>
newtype LoadBalancer = LoadBalancer 
  { "Name" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeLoadBalancer :: Newtype LoadBalancer _
derive instance repGenericLoadBalancer :: Generic LoadBalancer _
instance showLoadBalancer :: Show LoadBalancer where
  show = genericShow
instance decodeLoadBalancer :: Decode LoadBalancer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancer :: Encode LoadBalancer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the details of a LoadBalancer.</p>
newtype LoadBalancerDescription = LoadBalancerDescription 
  { "LoadBalancerName" :: NullOrUndefined.NullOrUndefined (String)
  , "Domain" :: NullOrUndefined.NullOrUndefined (String)
  , "Listeners" :: NullOrUndefined.NullOrUndefined (LoadBalancerListenersDescription)
  }
derive instance newtypeLoadBalancerDescription :: Newtype LoadBalancerDescription _
derive instance repGenericLoadBalancerDescription :: Generic LoadBalancerDescription _
instance showLoadBalancerDescription :: Show LoadBalancerDescription where
  show = genericShow
instance decodeLoadBalancerDescription :: Decode LoadBalancerDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerDescription :: Encode LoadBalancerDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerList = LoadBalancerList (Array LoadBalancer)
derive instance newtypeLoadBalancerList :: Newtype LoadBalancerList _
derive instance repGenericLoadBalancerList :: Generic LoadBalancerList _
instance showLoadBalancerList :: Show LoadBalancerList where
  show = genericShow
instance decodeLoadBalancerList :: Decode LoadBalancerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerList :: Encode LoadBalancerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerListenersDescription = LoadBalancerListenersDescription (Array Listener)
derive instance newtypeLoadBalancerListenersDescription :: Newtype LoadBalancerListenersDescription _
derive instance repGenericLoadBalancerListenersDescription :: Generic LoadBalancerListenersDescription _
instance showLoadBalancerListenersDescription :: Show LoadBalancerListenersDescription where
  show = genericShow
instance decodeLoadBalancerListenersDescription :: Decode LoadBalancerListenersDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerListenersDescription :: Encode LoadBalancerListenersDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Maintainer = Maintainer String
derive instance newtypeMaintainer :: Newtype Maintainer _
derive instance repGenericMaintainer :: Generic Maintainer _
instance showMaintainer :: Show Maintainer where
  show = genericShow
instance decodeMaintainer :: Decode Maintainer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaintainer :: Encode Maintainer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The record of an upcoming or in-progress managed action.</p>
newtype ManagedAction = ManagedAction 
  { "ActionId" :: NullOrUndefined.NullOrUndefined (String)
  , "ActionDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "ActionType" :: NullOrUndefined.NullOrUndefined (ActionType)
  , "Status" :: NullOrUndefined.NullOrUndefined (ActionStatus)
  , "WindowStartTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeManagedAction :: Newtype ManagedAction _
derive instance repGenericManagedAction :: Generic ManagedAction _
instance showManagedAction :: Show ManagedAction where
  show = genericShow
instance decodeManagedAction :: Decode ManagedAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManagedAction :: Encode ManagedAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The record of a completed or failed managed action.</p>
newtype ManagedActionHistoryItem = ManagedActionHistoryItem 
  { "ActionId" :: NullOrUndefined.NullOrUndefined (String)
  , "ActionType" :: NullOrUndefined.NullOrUndefined (ActionType)
  , "ActionDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "FailureType" :: NullOrUndefined.NullOrUndefined (FailureType)
  , "Status" :: NullOrUndefined.NullOrUndefined (ActionHistoryStatus)
  , "FailureDescription" :: NullOrUndefined.NullOrUndefined (String)
  , "ExecutedTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "FinishedTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeManagedActionHistoryItem :: Newtype ManagedActionHistoryItem _
derive instance repGenericManagedActionHistoryItem :: Generic ManagedActionHistoryItem _
instance showManagedActionHistoryItem :: Show ManagedActionHistoryItem where
  show = genericShow
instance decodeManagedActionHistoryItem :: Decode ManagedActionHistoryItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManagedActionHistoryItem :: Encode ManagedActionHistoryItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ManagedActionHistoryItems = ManagedActionHistoryItems (Array ManagedActionHistoryItem)
derive instance newtypeManagedActionHistoryItems :: Newtype ManagedActionHistoryItems _
derive instance repGenericManagedActionHistoryItems :: Generic ManagedActionHistoryItems _
instance showManagedActionHistoryItems :: Show ManagedActionHistoryItems where
  show = genericShow
instance decodeManagedActionHistoryItems :: Decode ManagedActionHistoryItems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManagedActionHistoryItems :: Encode ManagedActionHistoryItems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Cannot modify the managed action in its current state.</p>
newtype ManagedActionInvalidStateException = ManagedActionInvalidStateException Types.NoArguments
derive instance newtypeManagedActionInvalidStateException :: Newtype ManagedActionInvalidStateException _
derive instance repGenericManagedActionInvalidStateException :: Generic ManagedActionInvalidStateException _
instance showManagedActionInvalidStateException :: Show ManagedActionInvalidStateException where
  show = genericShow
instance decodeManagedActionInvalidStateException :: Decode ManagedActionInvalidStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManagedActionInvalidStateException :: Encode ManagedActionInvalidStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ManagedActions = ManagedActions (Array ManagedAction)
derive instance newtypeManagedActions :: Newtype ManagedActions _
derive instance repGenericManagedActions :: Generic ManagedActions _
instance showManagedActions :: Show ManagedActions where
  show = genericShow
instance decodeManagedActions :: Decode ManagedActions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManagedActions :: Encode ManagedActions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A lifecycle rule that deletes application versions after the specified number of days.</p>
newtype MaxAgeRule = MaxAgeRule 
  { "Enabled" :: (BoxedBoolean)
  , "MaxAgeInDays" :: NullOrUndefined.NullOrUndefined (BoxedInt)
  , "DeleteSourceFromS3" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeMaxAgeRule :: Newtype MaxAgeRule _
derive instance repGenericMaxAgeRule :: Generic MaxAgeRule _
instance showMaxAgeRule :: Show MaxAgeRule where
  show = genericShow
instance decodeMaxAgeRule :: Decode MaxAgeRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxAgeRule :: Encode MaxAgeRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A lifecycle rule that deletes the oldest application version when the maximum count is exceeded.</p>
newtype MaxCountRule = MaxCountRule 
  { "Enabled" :: (BoxedBoolean)
  , "MaxCount" :: NullOrUndefined.NullOrUndefined (BoxedInt)
  , "DeleteSourceFromS3" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeMaxCountRule :: Newtype MaxCountRule _
derive instance repGenericMaxCountRule :: Generic MaxCountRule _
instance showMaxCountRule :: Show MaxCountRule where
  show = genericShow
instance decodeMaxCountRule :: Decode MaxCountRule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxCountRule :: Encode MaxCountRule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxRecords = MaxRecords Int
derive instance newtypeMaxRecords :: Newtype MaxRecords _
derive instance repGenericMaxRecords :: Generic MaxRecords _
instance showMaxRecords :: Show MaxRecords where
  show = genericShow
instance decodeMaxRecords :: Decode MaxRecords where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxRecords :: Encode MaxRecords where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _
derive instance repGenericNonEmptyString :: Generic NonEmptyString _
instance showNonEmptyString :: Show NonEmptyString where
  show = genericShow
instance decodeNonEmptyString :: Decode NonEmptyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonEmptyString :: Encode NonEmptyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableDouble = NullableDouble Number
derive instance newtypeNullableDouble :: Newtype NullableDouble _
derive instance repGenericNullableDouble :: Generic NullableDouble _
instance showNullableDouble :: Show NullableDouble where
  show = genericShow
instance decodeNullableDouble :: Decode NullableDouble where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableDouble :: Encode NullableDouble where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableInteger = NullableInteger Int
derive instance newtypeNullableInteger :: Newtype NullableInteger _
derive instance repGenericNullableInteger :: Generic NullableInteger _
instance showNullableInteger :: Show NullableInteger where
  show = genericShow
instance decodeNullableInteger :: Decode NullableInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableInteger :: Encode NullableInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullableLong = NullableLong Number
derive instance newtypeNullableLong :: Newtype NullableLong _
derive instance repGenericNullableLong :: Generic NullableLong _
instance showNullableLong :: Show NullableLong where
  show = genericShow
instance decodeNullableLong :: Decode NullableLong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullableLong :: Encode NullableLong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OperatingSystemName = OperatingSystemName String
derive instance newtypeOperatingSystemName :: Newtype OperatingSystemName _
derive instance repGenericOperatingSystemName :: Generic OperatingSystemName _
instance showOperatingSystemName :: Show OperatingSystemName where
  show = genericShow
instance decodeOperatingSystemName :: Decode OperatingSystemName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperatingSystemName :: Encode OperatingSystemName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OperatingSystemVersion = OperatingSystemVersion String
derive instance newtypeOperatingSystemVersion :: Newtype OperatingSystemVersion _
derive instance repGenericOperatingSystemVersion :: Generic OperatingSystemVersion _
instance showOperatingSystemVersion :: Show OperatingSystemVersion where
  show = genericShow
instance decodeOperatingSystemVersion :: Decode OperatingSystemVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperatingSystemVersion :: Encode OperatingSystemVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Unable to perform the specified operation because another operation that effects an element in this activity is already in progress.</p>
newtype OperationInProgressException = OperationInProgressException Types.NoArguments
derive instance newtypeOperationInProgressException :: Newtype OperationInProgressException _
derive instance repGenericOperationInProgressException :: Generic OperationInProgressException _
instance showOperationInProgressException :: Show OperationInProgressException where
  show = genericShow
instance decodeOperationInProgressException :: Decode OperationInProgressException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationInProgressException :: Encode OperationInProgressException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionNamespace = OptionNamespace String
derive instance newtypeOptionNamespace :: Newtype OptionNamespace _
derive instance repGenericOptionNamespace :: Generic OptionNamespace _
instance showOptionNamespace :: Show OptionNamespace where
  show = genericShow
instance decodeOptionNamespace :: Decode OptionNamespace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionNamespace :: Encode OptionNamespace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionRestrictionMaxLength = OptionRestrictionMaxLength Int
derive instance newtypeOptionRestrictionMaxLength :: Newtype OptionRestrictionMaxLength _
derive instance repGenericOptionRestrictionMaxLength :: Generic OptionRestrictionMaxLength _
instance showOptionRestrictionMaxLength :: Show OptionRestrictionMaxLength where
  show = genericShow
instance decodeOptionRestrictionMaxLength :: Decode OptionRestrictionMaxLength where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionRestrictionMaxLength :: Encode OptionRestrictionMaxLength where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionRestrictionMaxValue = OptionRestrictionMaxValue Int
derive instance newtypeOptionRestrictionMaxValue :: Newtype OptionRestrictionMaxValue _
derive instance repGenericOptionRestrictionMaxValue :: Generic OptionRestrictionMaxValue _
instance showOptionRestrictionMaxValue :: Show OptionRestrictionMaxValue where
  show = genericShow
instance decodeOptionRestrictionMaxValue :: Decode OptionRestrictionMaxValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionRestrictionMaxValue :: Encode OptionRestrictionMaxValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionRestrictionMinValue = OptionRestrictionMinValue Int
derive instance newtypeOptionRestrictionMinValue :: Newtype OptionRestrictionMinValue _
derive instance repGenericOptionRestrictionMinValue :: Generic OptionRestrictionMinValue _
instance showOptionRestrictionMinValue :: Show OptionRestrictionMinValue where
  show = genericShow
instance decodeOptionRestrictionMinValue :: Decode OptionRestrictionMinValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionRestrictionMinValue :: Encode OptionRestrictionMinValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A regular expression representing a restriction on a string configuration option value.</p>
newtype OptionRestrictionRegex = OptionRestrictionRegex 
  { "Pattern" :: NullOrUndefined.NullOrUndefined (RegexPattern)
  , "Label" :: NullOrUndefined.NullOrUndefined (RegexLabel)
  }
derive instance newtypeOptionRestrictionRegex :: Newtype OptionRestrictionRegex _
derive instance repGenericOptionRestrictionRegex :: Generic OptionRestrictionRegex _
instance showOptionRestrictionRegex :: Show OptionRestrictionRegex where
  show = genericShow
instance decodeOptionRestrictionRegex :: Decode OptionRestrictionRegex where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionRestrictionRegex :: Encode OptionRestrictionRegex where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A specification identifying an individual configuration option.</p>
newtype OptionSpecification = OptionSpecification 
  { "ResourceName" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Namespace" :: NullOrUndefined.NullOrUndefined (OptionNamespace)
  , "OptionName" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionName)
  }
derive instance newtypeOptionSpecification :: Newtype OptionSpecification _
derive instance repGenericOptionSpecification :: Generic OptionSpecification _
instance showOptionSpecification :: Show OptionSpecification where
  show = genericShow
instance decodeOptionSpecification :: Decode OptionSpecification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionSpecification :: Encode OptionSpecification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OptionsSpecifierList = OptionsSpecifierList (Array OptionSpecification)
derive instance newtypeOptionsSpecifierList :: Newtype OptionsSpecifierList _
derive instance repGenericOptionsSpecifierList :: Generic OptionsSpecifierList _
instance showOptionsSpecifierList :: Show OptionsSpecifierList where
  show = genericShow
instance decodeOptionsSpecifierList :: Decode OptionsSpecifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOptionsSpecifierList :: Encode OptionsSpecifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformArn = PlatformArn String
derive instance newtypePlatformArn :: Newtype PlatformArn _
derive instance repGenericPlatformArn :: Generic PlatformArn _
instance showPlatformArn :: Show PlatformArn where
  show = genericShow
instance decodePlatformArn :: Decode PlatformArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformArn :: Encode PlatformArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformCategory = PlatformCategory String
derive instance newtypePlatformCategory :: Newtype PlatformCategory _
derive instance repGenericPlatformCategory :: Generic PlatformCategory _
instance showPlatformCategory :: Show PlatformCategory where
  show = genericShow
instance decodePlatformCategory :: Decode PlatformCategory where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformCategory :: Encode PlatformCategory where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed information about a platform.</p>
newtype PlatformDescription = PlatformDescription 
  { "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "PlatformOwner" :: NullOrUndefined.NullOrUndefined (PlatformOwner)
  , "PlatformName" :: NullOrUndefined.NullOrUndefined (PlatformName)
  , "PlatformVersion" :: NullOrUndefined.NullOrUndefined (PlatformVersion)
  , "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PlatformStatus" :: NullOrUndefined.NullOrUndefined (PlatformStatus)
  , "DateCreated" :: NullOrUndefined.NullOrUndefined (CreationDate)
  , "DateUpdated" :: NullOrUndefined.NullOrUndefined (UpdateDate)
  , "PlatformCategory" :: NullOrUndefined.NullOrUndefined (PlatformCategory)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Maintainer" :: NullOrUndefined.NullOrUndefined (Maintainer)
  , "OperatingSystemName" :: NullOrUndefined.NullOrUndefined (OperatingSystemName)
  , "OperatingSystemVersion" :: NullOrUndefined.NullOrUndefined (OperatingSystemVersion)
  , "ProgrammingLanguages" :: NullOrUndefined.NullOrUndefined (PlatformProgrammingLanguages)
  , "Frameworks" :: NullOrUndefined.NullOrUndefined (PlatformFrameworks)
  , "CustomAmiList" :: NullOrUndefined.NullOrUndefined (CustomAmiList)
  , "SupportedTierList" :: NullOrUndefined.NullOrUndefined (SupportedTierList)
  , "SupportedAddonList" :: NullOrUndefined.NullOrUndefined (SupportedAddonList)
  }
derive instance newtypePlatformDescription :: Newtype PlatformDescription _
derive instance repGenericPlatformDescription :: Generic PlatformDescription _
instance showPlatformDescription :: Show PlatformDescription where
  show = genericShow
instance decodePlatformDescription :: Decode PlatformDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformDescription :: Encode PlatformDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specify criteria to restrict the results when listing custom platforms.</p> <p>The filter is evaluated as the expression:</p> <p> <code>Type</code> <code>Operator</code> <code>Values[i]</code> </p>
newtype PlatformFilter = PlatformFilter 
  { "Type" :: NullOrUndefined.NullOrUndefined (PlatformFilterType)
  , "Operator" :: NullOrUndefined.NullOrUndefined (PlatformFilterOperator)
  , "Values" :: NullOrUndefined.NullOrUndefined (PlatformFilterValueList)
  }
derive instance newtypePlatformFilter :: Newtype PlatformFilter _
derive instance repGenericPlatformFilter :: Generic PlatformFilter _
instance showPlatformFilter :: Show PlatformFilter where
  show = genericShow
instance decodePlatformFilter :: Decode PlatformFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformFilter :: Encode PlatformFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformFilterOperator = PlatformFilterOperator String
derive instance newtypePlatformFilterOperator :: Newtype PlatformFilterOperator _
derive instance repGenericPlatformFilterOperator :: Generic PlatformFilterOperator _
instance showPlatformFilterOperator :: Show PlatformFilterOperator where
  show = genericShow
instance decodePlatformFilterOperator :: Decode PlatformFilterOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformFilterOperator :: Encode PlatformFilterOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformFilterType = PlatformFilterType String
derive instance newtypePlatformFilterType :: Newtype PlatformFilterType _
derive instance repGenericPlatformFilterType :: Generic PlatformFilterType _
instance showPlatformFilterType :: Show PlatformFilterType where
  show = genericShow
instance decodePlatformFilterType :: Decode PlatformFilterType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformFilterType :: Encode PlatformFilterType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformFilterValue = PlatformFilterValue String
derive instance newtypePlatformFilterValue :: Newtype PlatformFilterValue _
derive instance repGenericPlatformFilterValue :: Generic PlatformFilterValue _
instance showPlatformFilterValue :: Show PlatformFilterValue where
  show = genericShow
instance decodePlatformFilterValue :: Decode PlatformFilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformFilterValue :: Encode PlatformFilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformFilterValueList = PlatformFilterValueList (Array PlatformFilterValue)
derive instance newtypePlatformFilterValueList :: Newtype PlatformFilterValueList _
derive instance repGenericPlatformFilterValueList :: Generic PlatformFilterValueList _
instance showPlatformFilterValueList :: Show PlatformFilterValueList where
  show = genericShow
instance decodePlatformFilterValueList :: Decode PlatformFilterValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformFilterValueList :: Encode PlatformFilterValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformFilters = PlatformFilters (Array PlatformFilter)
derive instance newtypePlatformFilters :: Newtype PlatformFilters _
derive instance repGenericPlatformFilters :: Generic PlatformFilters _
instance showPlatformFilters :: Show PlatformFilters where
  show = genericShow
instance decodePlatformFilters :: Decode PlatformFilters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformFilters :: Encode PlatformFilters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A framework supported by the custom platform.</p>
newtype PlatformFramework = PlatformFramework 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePlatformFramework :: Newtype PlatformFramework _
derive instance repGenericPlatformFramework :: Generic PlatformFramework _
instance showPlatformFramework :: Show PlatformFramework where
  show = genericShow
instance decodePlatformFramework :: Decode PlatformFramework where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformFramework :: Encode PlatformFramework where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformFrameworks = PlatformFrameworks (Array PlatformFramework)
derive instance newtypePlatformFrameworks :: Newtype PlatformFrameworks _
derive instance repGenericPlatformFrameworks :: Generic PlatformFrameworks _
instance showPlatformFrameworks :: Show PlatformFrameworks where
  show = genericShow
instance decodePlatformFrameworks :: Decode PlatformFrameworks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformFrameworks :: Encode PlatformFrameworks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformMaxRecords = PlatformMaxRecords Int
derive instance newtypePlatformMaxRecords :: Newtype PlatformMaxRecords _
derive instance repGenericPlatformMaxRecords :: Generic PlatformMaxRecords _
instance showPlatformMaxRecords :: Show PlatformMaxRecords where
  show = genericShow
instance decodePlatformMaxRecords :: Decode PlatformMaxRecords where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformMaxRecords :: Encode PlatformMaxRecords where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformName = PlatformName String
derive instance newtypePlatformName :: Newtype PlatformName _
derive instance repGenericPlatformName :: Generic PlatformName _
instance showPlatformName :: Show PlatformName where
  show = genericShow
instance decodePlatformName :: Decode PlatformName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformName :: Encode PlatformName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformOwner = PlatformOwner String
derive instance newtypePlatformOwner :: Newtype PlatformOwner _
derive instance repGenericPlatformOwner :: Generic PlatformOwner _
instance showPlatformOwner :: Show PlatformOwner where
  show = genericShow
instance decodePlatformOwner :: Decode PlatformOwner where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformOwner :: Encode PlatformOwner where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A programming language supported by the platform.</p>
newtype PlatformProgrammingLanguage = PlatformProgrammingLanguage 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "Version" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePlatformProgrammingLanguage :: Newtype PlatformProgrammingLanguage _
derive instance repGenericPlatformProgrammingLanguage :: Generic PlatformProgrammingLanguage _
instance showPlatformProgrammingLanguage :: Show PlatformProgrammingLanguage where
  show = genericShow
instance decodePlatformProgrammingLanguage :: Decode PlatformProgrammingLanguage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformProgrammingLanguage :: Encode PlatformProgrammingLanguage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformProgrammingLanguages = PlatformProgrammingLanguages (Array PlatformProgrammingLanguage)
derive instance newtypePlatformProgrammingLanguages :: Newtype PlatformProgrammingLanguages _
derive instance repGenericPlatformProgrammingLanguages :: Generic PlatformProgrammingLanguages _
instance showPlatformProgrammingLanguages :: Show PlatformProgrammingLanguages where
  show = genericShow
instance decodePlatformProgrammingLanguages :: Decode PlatformProgrammingLanguages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformProgrammingLanguages :: Encode PlatformProgrammingLanguages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformStatus = PlatformStatus String
derive instance newtypePlatformStatus :: Newtype PlatformStatus _
derive instance repGenericPlatformStatus :: Generic PlatformStatus _
instance showPlatformStatus :: Show PlatformStatus where
  show = genericShow
instance decodePlatformStatus :: Decode PlatformStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformStatus :: Encode PlatformStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed information about a platform.</p>
newtype PlatformSummary = PlatformSummary 
  { "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "PlatformOwner" :: NullOrUndefined.NullOrUndefined (PlatformOwner)
  , "PlatformStatus" :: NullOrUndefined.NullOrUndefined (PlatformStatus)
  , "PlatformCategory" :: NullOrUndefined.NullOrUndefined (PlatformCategory)
  , "OperatingSystemName" :: NullOrUndefined.NullOrUndefined (OperatingSystemName)
  , "OperatingSystemVersion" :: NullOrUndefined.NullOrUndefined (OperatingSystemVersion)
  , "SupportedTierList" :: NullOrUndefined.NullOrUndefined (SupportedTierList)
  , "SupportedAddonList" :: NullOrUndefined.NullOrUndefined (SupportedAddonList)
  }
derive instance newtypePlatformSummary :: Newtype PlatformSummary _
derive instance repGenericPlatformSummary :: Generic PlatformSummary _
instance showPlatformSummary :: Show PlatformSummary where
  show = genericShow
instance decodePlatformSummary :: Decode PlatformSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformSummary :: Encode PlatformSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformSummaryList = PlatformSummaryList (Array PlatformSummary)
derive instance newtypePlatformSummaryList :: Newtype PlatformSummaryList _
derive instance repGenericPlatformSummaryList :: Generic PlatformSummaryList _
instance showPlatformSummaryList :: Show PlatformSummaryList where
  show = genericShow
instance decodePlatformSummaryList :: Decode PlatformSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformSummaryList :: Encode PlatformSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformVersion = PlatformVersion String
derive instance newtypePlatformVersion :: Newtype PlatformVersion _
derive instance repGenericPlatformVersion :: Generic PlatformVersion _
instance showPlatformVersion :: Show PlatformVersion where
  show = genericShow
instance decodePlatformVersion :: Decode PlatformVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformVersion :: Encode PlatformVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot delete the platform version because there are still environments running on it.</p>
newtype PlatformVersionStillReferencedException = PlatformVersionStillReferencedException Types.NoArguments
derive instance newtypePlatformVersionStillReferencedException :: Newtype PlatformVersionStillReferencedException _
derive instance repGenericPlatformVersionStillReferencedException :: Generic PlatformVersionStillReferencedException _
instance showPlatformVersionStillReferencedException :: Show PlatformVersionStillReferencedException where
  show = genericShow
instance decodePlatformVersionStillReferencedException :: Decode PlatformVersionStillReferencedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformVersionStillReferencedException :: Encode PlatformVersionStillReferencedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a queue.</p>
newtype Queue = Queue 
  { "Name" :: NullOrUndefined.NullOrUndefined (String)
  , "URL" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeQueue :: Newtype Queue _
derive instance repGenericQueue :: Generic Queue _
instance showQueue :: Show Queue where
  show = genericShow
instance decodeQueue :: Decode Queue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueue :: Encode Queue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueueList = QueueList (Array Queue)
derive instance newtypeQueueList :: Newtype QueueList _
derive instance repGenericQueueList :: Generic QueueList _
instance showQueueList :: Show QueueList where
  show = genericShow
instance decodeQueueList :: Decode QueueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueueList :: Encode QueueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RebuildEnvironmentMessage = RebuildEnvironmentMessage 
  { "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  }
derive instance newtypeRebuildEnvironmentMessage :: Newtype RebuildEnvironmentMessage _
derive instance repGenericRebuildEnvironmentMessage :: Generic RebuildEnvironmentMessage _
instance showRebuildEnvironmentMessage :: Show RebuildEnvironmentMessage where
  show = genericShow
instance decodeRebuildEnvironmentMessage :: Decode RebuildEnvironmentMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebuildEnvironmentMessage :: Encode RebuildEnvironmentMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RefreshedAt = RefreshedAt Number
derive instance newtypeRefreshedAt :: Newtype RefreshedAt _
derive instance repGenericRefreshedAt :: Generic RefreshedAt _
instance showRefreshedAt :: Show RefreshedAt where
  show = genericShow
instance decodeRefreshedAt :: Decode RefreshedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRefreshedAt :: Encode RefreshedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexLabel = RegexLabel String
derive instance newtypeRegexLabel :: Newtype RegexLabel _
derive instance repGenericRegexLabel :: Generic RegexLabel _
instance showRegexLabel :: Show RegexLabel where
  show = genericShow
instance decodeRegexLabel :: Decode RegexLabel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexLabel :: Encode RegexLabel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegexPattern = RegexPattern String
derive instance newtypeRegexPattern :: Newtype RegexPattern _
derive instance repGenericRegexPattern :: Generic RegexPattern _
instance showRegexPattern :: Show RegexPattern where
  show = genericShow
instance decodeRegexPattern :: Decode RegexPattern where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegexPattern :: Encode RegexPattern where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestCount = RequestCount Int
derive instance newtypeRequestCount :: Newtype RequestCount _
derive instance repGenericRequestCount :: Generic RequestCount _
instance showRequestCount :: Show RequestCount where
  show = genericShow
instance decodeRequestCount :: Decode RequestCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestCount :: Encode RequestCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to retrieve logs from an environment and store them in your Elastic Beanstalk storage bucket.</p>
newtype RequestEnvironmentInfoMessage = RequestEnvironmentInfoMessage 
  { "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "InfoType" :: (EnvironmentInfoType)
  }
derive instance newtypeRequestEnvironmentInfoMessage :: Newtype RequestEnvironmentInfoMessage _
derive instance repGenericRequestEnvironmentInfoMessage :: Generic RequestEnvironmentInfoMessage _
instance showRequestEnvironmentInfoMessage :: Show RequestEnvironmentInfoMessage where
  show = genericShow
instance decodeRequestEnvironmentInfoMessage :: Decode RequestEnvironmentInfoMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestEnvironmentInfoMessage :: Encode RequestEnvironmentInfoMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestId = RequestId String
derive instance newtypeRequestId :: Newtype RequestId _
derive instance repGenericRequestId :: Generic RequestId _
instance showRequestId :: Show RequestId where
  show = genericShow
instance decodeRequestId :: Decode RequestId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestId :: Encode RequestId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _
derive instance repGenericResourceArn :: Generic ResourceArn _
instance showResourceArn :: Show ResourceArn where
  show = genericShow
instance decodeResourceArn :: Decode ResourceArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArn :: Encode ResourceArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _
derive instance repGenericResourceId :: Generic ResourceId _
instance showResourceId :: Show ResourceId where
  show = genericShow
instance decodeResourceId :: Decode ResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceId :: Encode ResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _
derive instance repGenericResourceName :: Generic ResourceName _
instance showResourceName :: Show ResourceName where
  show = genericShow
instance decodeResourceName :: Decode ResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceName :: Encode ResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A resource doesn't exist for the specified Amazon Resource Name (ARN).</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTagsDescriptionMessage = ResourceTagsDescriptionMessage 
  { "ResourceArn" :: NullOrUndefined.NullOrUndefined (ResourceArn)
  , "ResourceTags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeResourceTagsDescriptionMessage :: Newtype ResourceTagsDescriptionMessage _
derive instance repGenericResourceTagsDescriptionMessage :: Generic ResourceTagsDescriptionMessage _
instance showResourceTagsDescriptionMessage :: Show ResourceTagsDescriptionMessage where
  show = genericShow
instance decodeResourceTagsDescriptionMessage :: Decode ResourceTagsDescriptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTagsDescriptionMessage :: Encode ResourceTagsDescriptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The type of the specified Amazon Resource Name (ARN) isn't supported for this operation.</p>
newtype ResourceTypeNotSupportedException = ResourceTypeNotSupportedException Types.NoArguments
derive instance newtypeResourceTypeNotSupportedException :: Newtype ResourceTypeNotSupportedException _
derive instance repGenericResourceTypeNotSupportedException :: Generic ResourceTypeNotSupportedException _
instance showResourceTypeNotSupportedException :: Show ResourceTypeNotSupportedException where
  show = genericShow
instance decodeResourceTypeNotSupportedException :: Decode ResourceTypeNotSupportedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTypeNotSupportedException :: Encode ResourceTypeNotSupportedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype RestartAppServerMessage = RestartAppServerMessage 
  { "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  }
derive instance newtypeRestartAppServerMessage :: Newtype RestartAppServerMessage _
derive instance repGenericRestartAppServerMessage :: Generic RestartAppServerMessage _
instance showRestartAppServerMessage :: Show RestartAppServerMessage where
  show = genericShow
instance decodeRestartAppServerMessage :: Decode RestartAppServerMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestartAppServerMessage :: Encode RestartAppServerMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to download logs retrieved with <a>RequestEnvironmentInfo</a>.</p>
newtype RetrieveEnvironmentInfoMessage = RetrieveEnvironmentInfoMessage 
  { "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "InfoType" :: (EnvironmentInfoType)
  }
derive instance newtypeRetrieveEnvironmentInfoMessage :: Newtype RetrieveEnvironmentInfoMessage _
derive instance repGenericRetrieveEnvironmentInfoMessage :: Generic RetrieveEnvironmentInfoMessage _
instance showRetrieveEnvironmentInfoMessage :: Show RetrieveEnvironmentInfoMessage where
  show = genericShow
instance decodeRetrieveEnvironmentInfoMessage :: Decode RetrieveEnvironmentInfoMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetrieveEnvironmentInfoMessage :: Encode RetrieveEnvironmentInfoMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result message containing a description of the requested environment info.</p>
newtype RetrieveEnvironmentInfoResultMessage = RetrieveEnvironmentInfoResultMessage 
  { "EnvironmentInfo" :: NullOrUndefined.NullOrUndefined (EnvironmentInfoDescriptionList)
  }
derive instance newtypeRetrieveEnvironmentInfoResultMessage :: Newtype RetrieveEnvironmentInfoResultMessage _
derive instance repGenericRetrieveEnvironmentInfoResultMessage :: Generic RetrieveEnvironmentInfoResultMessage _
instance showRetrieveEnvironmentInfoResultMessage :: Show RetrieveEnvironmentInfoResultMessage where
  show = genericShow
instance decodeRetrieveEnvironmentInfoResultMessage :: Decode RetrieveEnvironmentInfoResultMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetrieveEnvironmentInfoResultMessage :: Encode RetrieveEnvironmentInfoResultMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3Bucket = S3Bucket String
derive instance newtypeS3Bucket :: Newtype S3Bucket _
derive instance repGenericS3Bucket :: Generic S3Bucket _
instance showS3Bucket :: Show S3Bucket where
  show = genericShow
instance decodeS3Bucket :: Decode S3Bucket where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Bucket :: Encode S3Bucket where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3Key = S3Key String
derive instance newtypeS3Key :: Newtype S3Key _
derive instance repGenericS3Key :: Generic S3Key _
instance showS3Key :: Show S3Key where
  show = genericShow
instance decodeS3Key :: Decode S3Key where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Key :: Encode S3Key where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The bucket and key of an item stored in Amazon S3.</p>
newtype S3Location = S3Location 
  { "S3Bucket" :: NullOrUndefined.NullOrUndefined (S3Bucket)
  , "S3Key" :: NullOrUndefined.NullOrUndefined (S3Key)
  }
derive instance newtypeS3Location :: Newtype S3Location _
derive instance repGenericS3Location :: Generic S3Location _
instance showS3Location :: Show S3Location where
  show = genericShow
instance decodeS3Location :: Decode S3Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Location :: Encode S3Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified S3 bucket does not belong to the S3 region in which the service is running. The following regions are supported:</p> <ul> <li> <p>IAD/us-east-1</p> </li> <li> <p>PDX/us-west-2</p> </li> <li> <p>DUB/eu-west-1</p> </li> </ul>
newtype S3LocationNotInServiceRegionException = S3LocationNotInServiceRegionException Types.NoArguments
derive instance newtypeS3LocationNotInServiceRegionException :: Newtype S3LocationNotInServiceRegionException _
derive instance repGenericS3LocationNotInServiceRegionException :: Generic S3LocationNotInServiceRegionException _
instance showS3LocationNotInServiceRegionException :: Show S3LocationNotInServiceRegionException where
  show = genericShow
instance decodeS3LocationNotInServiceRegionException :: Decode S3LocationNotInServiceRegionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3LocationNotInServiceRegionException :: Encode S3LocationNotInServiceRegionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified account does not have a subscription to Amazon S3.</p>
newtype S3SubscriptionRequiredException = S3SubscriptionRequiredException Types.NoArguments
derive instance newtypeS3SubscriptionRequiredException :: Newtype S3SubscriptionRequiredException _
derive instance repGenericS3SubscriptionRequiredException :: Generic S3SubscriptionRequiredException _
instance showS3SubscriptionRequiredException :: Show S3SubscriptionRequiredException where
  show = genericShow
instance decodeS3SubscriptionRequiredException :: Decode S3SubscriptionRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3SubscriptionRequiredException :: Encode S3SubscriptionRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SampleTimestamp = SampleTimestamp Number
derive instance newtypeSampleTimestamp :: Newtype SampleTimestamp _
derive instance repGenericSampleTimestamp :: Generic SampleTimestamp _
instance showSampleTimestamp :: Show SampleTimestamp where
  show = genericShow
instance decodeSampleTimestamp :: Decode SampleTimestamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSampleTimestamp :: Encode SampleTimestamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Detailed health information about an Amazon EC2 instance in your Elastic Beanstalk environment.</p>
newtype SingleInstanceHealth = SingleInstanceHealth 
  { "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "HealthStatus" :: NullOrUndefined.NullOrUndefined (String)
  , "Color" :: NullOrUndefined.NullOrUndefined (String)
  , "Causes" :: NullOrUndefined.NullOrUndefined (Causes)
  , "LaunchedAt" :: NullOrUndefined.NullOrUndefined (LaunchedAt)
  , "ApplicationMetrics" :: NullOrUndefined.NullOrUndefined (ApplicationMetrics)
  , "System" :: NullOrUndefined.NullOrUndefined (SystemStatus)
  , "Deployment" :: NullOrUndefined.NullOrUndefined (Deployment)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSingleInstanceHealth :: Newtype SingleInstanceHealth _
derive instance repGenericSingleInstanceHealth :: Generic SingleInstanceHealth _
instance showSingleInstanceHealth :: Show SingleInstanceHealth where
  show = genericShow
instance decodeSingleInstanceHealth :: Decode SingleInstanceHealth where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSingleInstanceHealth :: Encode SingleInstanceHealth where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the solution stack.</p>
newtype SolutionStackDescription = SolutionStackDescription 
  { "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PermittedFileTypes" :: NullOrUndefined.NullOrUndefined (SolutionStackFileTypeList)
  }
derive instance newtypeSolutionStackDescription :: Newtype SolutionStackDescription _
derive instance repGenericSolutionStackDescription :: Generic SolutionStackDescription _
instance showSolutionStackDescription :: Show SolutionStackDescription where
  show = genericShow
instance decodeSolutionStackDescription :: Decode SolutionStackDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSolutionStackDescription :: Encode SolutionStackDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SolutionStackFileTypeList = SolutionStackFileTypeList (Array FileTypeExtension)
derive instance newtypeSolutionStackFileTypeList :: Newtype SolutionStackFileTypeList _
derive instance repGenericSolutionStackFileTypeList :: Generic SolutionStackFileTypeList _
instance showSolutionStackFileTypeList :: Show SolutionStackFileTypeList where
  show = genericShow
instance decodeSolutionStackFileTypeList :: Decode SolutionStackFileTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSolutionStackFileTypeList :: Encode SolutionStackFileTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SolutionStackName = SolutionStackName String
derive instance newtypeSolutionStackName :: Newtype SolutionStackName _
derive instance repGenericSolutionStackName :: Generic SolutionStackName _
instance showSolutionStackName :: Show SolutionStackName where
  show = genericShow
instance decodeSolutionStackName :: Decode SolutionStackName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSolutionStackName :: Encode SolutionStackName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Location of the source code for an application version.</p>
newtype SourceBuildInformation = SourceBuildInformation 
  { "SourceType" :: (SourceType)
  , "SourceRepository" :: (SourceRepository)
  , "SourceLocation" :: (SourceLocation)
  }
derive instance newtypeSourceBuildInformation :: Newtype SourceBuildInformation _
derive instance repGenericSourceBuildInformation :: Generic SourceBuildInformation _
instance showSourceBuildInformation :: Show SourceBuildInformation where
  show = genericShow
instance decodeSourceBuildInformation :: Decode SourceBuildInformation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceBuildInformation :: Encode SourceBuildInformation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Unable to delete the Amazon S3 source bundle associated with the application version. The application version was deleted successfully.</p>
newtype SourceBundleDeletionException = SourceBundleDeletionException Types.NoArguments
derive instance newtypeSourceBundleDeletionException :: Newtype SourceBundleDeletionException _
derive instance repGenericSourceBundleDeletionException :: Generic SourceBundleDeletionException _
instance showSourceBundleDeletionException :: Show SourceBundleDeletionException where
  show = genericShow
instance decodeSourceBundleDeletionException :: Decode SourceBundleDeletionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceBundleDeletionException :: Encode SourceBundleDeletionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A specification for an environment configuration</p>
newtype SourceConfiguration = SourceConfiguration 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  }
derive instance newtypeSourceConfiguration :: Newtype SourceConfiguration _
derive instance repGenericSourceConfiguration :: Generic SourceConfiguration _
instance showSourceConfiguration :: Show SourceConfiguration where
  show = genericShow
instance decodeSourceConfiguration :: Decode SourceConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceConfiguration :: Encode SourceConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceLocation = SourceLocation String
derive instance newtypeSourceLocation :: Newtype SourceLocation _
derive instance repGenericSourceLocation :: Generic SourceLocation _
instance showSourceLocation :: Show SourceLocation where
  show = genericShow
instance decodeSourceLocation :: Decode SourceLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceLocation :: Encode SourceLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceRepository = SourceRepository String
derive instance newtypeSourceRepository :: Newtype SourceRepository _
derive instance repGenericSourceRepository :: Generic SourceRepository _
instance showSourceRepository :: Show SourceRepository where
  show = genericShow
instance decodeSourceRepository :: Decode SourceRepository where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceRepository :: Encode SourceRepository where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _
derive instance repGenericSourceType :: Generic SourceType _
instance showSourceType :: Show SourceType where
  show = genericShow
instance decodeSourceType :: Decode SourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceType :: Encode SourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the percentage of requests over the last 10 seconds that resulted in each type of status code response. For more information, see <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html">Status Code Definitions</a>.</p>
newtype StatusCodes = StatusCodes 
  { "Status2xx" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Status3xx" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Status4xx" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  , "Status5xx" :: NullOrUndefined.NullOrUndefined (NullableInteger)
  }
derive instance newtypeStatusCodes :: Newtype StatusCodes _
derive instance repGenericStatusCodes :: Generic StatusCodes _
instance showStatusCodes :: Show StatusCodes where
  show = genericShow
instance decodeStatusCodes :: Decode StatusCodes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusCodes :: Encode StatusCodes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupportedAddon = SupportedAddon String
derive instance newtypeSupportedAddon :: Newtype SupportedAddon _
derive instance repGenericSupportedAddon :: Generic SupportedAddon _
instance showSupportedAddon :: Show SupportedAddon where
  show = genericShow
instance decodeSupportedAddon :: Decode SupportedAddon where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportedAddon :: Encode SupportedAddon where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupportedAddonList = SupportedAddonList (Array SupportedAddon)
derive instance newtypeSupportedAddonList :: Newtype SupportedAddonList _
derive instance repGenericSupportedAddonList :: Generic SupportedAddonList _
instance showSupportedAddonList :: Show SupportedAddonList where
  show = genericShow
instance decodeSupportedAddonList :: Decode SupportedAddonList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportedAddonList :: Encode SupportedAddonList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupportedTier = SupportedTier String
derive instance newtypeSupportedTier :: Newtype SupportedTier _
derive instance repGenericSupportedTier :: Generic SupportedTier _
instance showSupportedTier :: Show SupportedTier where
  show = genericShow
instance decodeSupportedTier :: Decode SupportedTier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportedTier :: Encode SupportedTier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SupportedTierList = SupportedTierList (Array SupportedTier)
derive instance newtypeSupportedTierList :: Newtype SupportedTierList _
derive instance repGenericSupportedTierList :: Generic SupportedTierList _
instance showSupportedTierList :: Show SupportedTierList where
  show = genericShow
instance decodeSupportedTierList :: Decode SupportedTierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSupportedTierList :: Encode SupportedTierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Swaps the CNAMEs of two environments.</p>
newtype SwapEnvironmentCNAMEsMessage = SwapEnvironmentCNAMEsMessage 
  { "SourceEnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "SourceEnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "DestinationEnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "DestinationEnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  }
derive instance newtypeSwapEnvironmentCNAMEsMessage :: Newtype SwapEnvironmentCNAMEsMessage _
derive instance repGenericSwapEnvironmentCNAMEsMessage :: Generic SwapEnvironmentCNAMEsMessage _
instance showSwapEnvironmentCNAMEsMessage :: Show SwapEnvironmentCNAMEsMessage where
  show = genericShow
instance decodeSwapEnvironmentCNAMEsMessage :: Decode SwapEnvironmentCNAMEsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSwapEnvironmentCNAMEsMessage :: Encode SwapEnvironmentCNAMEsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>CPU utilization and load average metrics for an Amazon EC2 instance.</p>
newtype SystemStatus = SystemStatus 
  { "CPUUtilization" :: NullOrUndefined.NullOrUndefined (CPUUtilization)
  , "LoadAverage" :: NullOrUndefined.NullOrUndefined (LoadAverage)
  }
derive instance newtypeSystemStatus :: Newtype SystemStatus _
derive instance repGenericSystemStatus :: Generic SystemStatus _
instance showSystemStatus :: Show SystemStatus where
  show = genericShow
instance decodeSystemStatus :: Decode SystemStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSystemStatus :: Encode SystemStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a tag applied to a resource in an environment.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined.NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateEnvForce = TerminateEnvForce Boolean
derive instance newtypeTerminateEnvForce :: Newtype TerminateEnvForce _
derive instance repGenericTerminateEnvForce :: Generic TerminateEnvForce _
instance showTerminateEnvForce :: Show TerminateEnvForce where
  show = genericShow
instance decodeTerminateEnvForce :: Decode TerminateEnvForce where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateEnvForce :: Encode TerminateEnvForce where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to terminate an environment.</p>
newtype TerminateEnvironmentMessage = TerminateEnvironmentMessage 
  { "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "TerminateResources" :: NullOrUndefined.NullOrUndefined (TerminateEnvironmentResources)
  , "ForceTerminate" :: NullOrUndefined.NullOrUndefined (ForceTerminate)
  }
derive instance newtypeTerminateEnvironmentMessage :: Newtype TerminateEnvironmentMessage _
derive instance repGenericTerminateEnvironmentMessage :: Generic TerminateEnvironmentMessage _
instance showTerminateEnvironmentMessage :: Show TerminateEnvironmentMessage where
  show = genericShow
instance decodeTerminateEnvironmentMessage :: Decode TerminateEnvironmentMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateEnvironmentMessage :: Encode TerminateEnvironmentMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateEnvironmentResources = TerminateEnvironmentResources Boolean
derive instance newtypeTerminateEnvironmentResources :: Newtype TerminateEnvironmentResources _
derive instance repGenericTerminateEnvironmentResources :: Generic TerminateEnvironmentResources _
instance showTerminateEnvironmentResources :: Show TerminateEnvironmentResources where
  show = genericShow
instance decodeTerminateEnvironmentResources :: Decode TerminateEnvironmentResources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateEnvironmentResources :: Encode TerminateEnvironmentResources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeFilterEnd = TimeFilterEnd Number
derive instance newtypeTimeFilterEnd :: Newtype TimeFilterEnd _
derive instance repGenericTimeFilterEnd :: Generic TimeFilterEnd _
instance showTimeFilterEnd :: Show TimeFilterEnd where
  show = genericShow
instance decodeTimeFilterEnd :: Decode TimeFilterEnd where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeFilterEnd :: Encode TimeFilterEnd where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeFilterStart = TimeFilterStart Number
derive instance newtypeTimeFilterStart :: Newtype TimeFilterStart _
derive instance repGenericTimeFilterStart :: Generic TimeFilterStart _
instance showTimeFilterStart :: Show TimeFilterStart where
  show = genericShow
instance decodeTimeFilterStart :: Decode TimeFilterStart where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeFilterStart :: Encode TimeFilterStart where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _
derive instance repGenericToken :: Generic Token _
instance showToken :: Show Token where
  show = genericShow
instance decodeToken :: Decode Token where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeToken :: Encode Token where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified account has reached its limit of application versions.</p>
newtype TooManyApplicationVersionsException = TooManyApplicationVersionsException Types.NoArguments
derive instance newtypeTooManyApplicationVersionsException :: Newtype TooManyApplicationVersionsException _
derive instance repGenericTooManyApplicationVersionsException :: Generic TooManyApplicationVersionsException _
instance showTooManyApplicationVersionsException :: Show TooManyApplicationVersionsException where
  show = genericShow
instance decodeTooManyApplicationVersionsException :: Decode TooManyApplicationVersionsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyApplicationVersionsException :: Encode TooManyApplicationVersionsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified account has reached its limit of applications.</p>
newtype TooManyApplicationsException = TooManyApplicationsException Types.NoArguments
derive instance newtypeTooManyApplicationsException :: Newtype TooManyApplicationsException _
derive instance repGenericTooManyApplicationsException :: Generic TooManyApplicationsException _
instance showTooManyApplicationsException :: Show TooManyApplicationsException where
  show = genericShow
instance decodeTooManyApplicationsException :: Decode TooManyApplicationsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyApplicationsException :: Encode TooManyApplicationsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified account has reached its limit of Amazon S3 buckets.</p>
newtype TooManyBucketsException = TooManyBucketsException Types.NoArguments
derive instance newtypeTooManyBucketsException :: Newtype TooManyBucketsException _
derive instance repGenericTooManyBucketsException :: Generic TooManyBucketsException _
instance showTooManyBucketsException :: Show TooManyBucketsException where
  show = genericShow
instance decodeTooManyBucketsException :: Decode TooManyBucketsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyBucketsException :: Encode TooManyBucketsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified account has reached its limit of configuration templates.</p>
newtype TooManyConfigurationTemplatesException = TooManyConfigurationTemplatesException Types.NoArguments
derive instance newtypeTooManyConfigurationTemplatesException :: Newtype TooManyConfigurationTemplatesException _
derive instance repGenericTooManyConfigurationTemplatesException :: Generic TooManyConfigurationTemplatesException _
instance showTooManyConfigurationTemplatesException :: Show TooManyConfigurationTemplatesException where
  show = genericShow
instance decodeTooManyConfigurationTemplatesException :: Decode TooManyConfigurationTemplatesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyConfigurationTemplatesException :: Encode TooManyConfigurationTemplatesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified account has reached its limit of environments.</p>
newtype TooManyEnvironmentsException = TooManyEnvironmentsException Types.NoArguments
derive instance newtypeTooManyEnvironmentsException :: Newtype TooManyEnvironmentsException _
derive instance repGenericTooManyEnvironmentsException :: Generic TooManyEnvironmentsException _
instance showTooManyEnvironmentsException :: Show TooManyEnvironmentsException where
  show = genericShow
instance decodeTooManyEnvironmentsException :: Decode TooManyEnvironmentsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyEnvironmentsException :: Encode TooManyEnvironmentsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have exceeded the maximum number of allowed platforms associated with the account.</p>
newtype TooManyPlatformsException = TooManyPlatformsException Types.NoArguments
derive instance newtypeTooManyPlatformsException :: Newtype TooManyPlatformsException _
derive instance repGenericTooManyPlatformsException :: Generic TooManyPlatformsException _
instance showTooManyPlatformsException :: Show TooManyPlatformsException where
  show = genericShow
instance decodeTooManyPlatformsException :: Decode TooManyPlatformsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyPlatformsException :: Encode TooManyPlatformsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of tags in the resource would exceed the number of tags that each resource can have.</p> <p>To calculate this, the operation considers both the number of tags the resource already has and the tags this operation would add if it succeeded.</p>
newtype TooManyTagsException = TooManyTagsException Types.NoArguments
derive instance newtypeTooManyTagsException :: Newtype TooManyTagsException _
derive instance repGenericTooManyTagsException :: Generic TooManyTagsException _
instance showTooManyTagsException :: Show TooManyTagsException where
  show = genericShow
instance decodeTooManyTagsException :: Decode TooManyTagsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTagsException :: Encode TooManyTagsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a trigger.</p>
newtype Trigger = Trigger 
  { "Name" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeTrigger :: Newtype Trigger _
derive instance repGenericTrigger :: Generic Trigger _
instance showTrigger :: Show Trigger where
  show = genericShow
instance decodeTrigger :: Decode Trigger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrigger :: Encode Trigger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TriggerList = TriggerList (Array Trigger)
derive instance newtypeTriggerList :: Newtype TriggerList _
derive instance repGenericTriggerList :: Generic TriggerList _
instance showTriggerList :: Show TriggerList where
  show = genericShow
instance decodeTriggerList :: Decode TriggerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTriggerList :: Encode TriggerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to update an application.</p>
newtype UpdateApplicationMessage = UpdateApplicationMessage 
  { "ApplicationName" :: (ApplicationName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeUpdateApplicationMessage :: Newtype UpdateApplicationMessage _
derive instance repGenericUpdateApplicationMessage :: Generic UpdateApplicationMessage _
instance showUpdateApplicationMessage :: Show UpdateApplicationMessage where
  show = genericShow
instance decodeUpdateApplicationMessage :: Decode UpdateApplicationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationMessage :: Encode UpdateApplicationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationResourceLifecycleMessage = UpdateApplicationResourceLifecycleMessage 
  { "ApplicationName" :: (ApplicationName)
  , "ResourceLifecycleConfig" :: (ApplicationResourceLifecycleConfig)
  }
derive instance newtypeUpdateApplicationResourceLifecycleMessage :: Newtype UpdateApplicationResourceLifecycleMessage _
derive instance repGenericUpdateApplicationResourceLifecycleMessage :: Generic UpdateApplicationResourceLifecycleMessage _
instance showUpdateApplicationResourceLifecycleMessage :: Show UpdateApplicationResourceLifecycleMessage where
  show = genericShow
instance decodeUpdateApplicationResourceLifecycleMessage :: Decode UpdateApplicationResourceLifecycleMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationResourceLifecycleMessage :: Encode UpdateApplicationResourceLifecycleMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p/>
newtype UpdateApplicationVersionMessage = UpdateApplicationVersionMessage 
  { "ApplicationName" :: (ApplicationName)
  , "VersionLabel" :: (VersionLabel)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeUpdateApplicationVersionMessage :: Newtype UpdateApplicationVersionMessage _
derive instance repGenericUpdateApplicationVersionMessage :: Generic UpdateApplicationVersionMessage _
instance showUpdateApplicationVersionMessage :: Show UpdateApplicationVersionMessage where
  show = genericShow
instance decodeUpdateApplicationVersionMessage :: Decode UpdateApplicationVersionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationVersionMessage :: Encode UpdateApplicationVersionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result message containing the options for the specified solution stack.</p>
newtype UpdateConfigurationTemplateMessage = UpdateConfigurationTemplateMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: (ConfigurationTemplateName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "OptionSettings" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionSettingsList)
  , "OptionsToRemove" :: NullOrUndefined.NullOrUndefined (OptionsSpecifierList)
  }
derive instance newtypeUpdateConfigurationTemplateMessage :: Newtype UpdateConfigurationTemplateMessage _
derive instance repGenericUpdateConfigurationTemplateMessage :: Generic UpdateConfigurationTemplateMessage _
instance showUpdateConfigurationTemplateMessage :: Show UpdateConfigurationTemplateMessage where
  show = genericShow
instance decodeUpdateConfigurationTemplateMessage :: Decode UpdateConfigurationTemplateMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConfigurationTemplateMessage :: Encode UpdateConfigurationTemplateMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDate = UpdateDate Number
derive instance newtypeUpdateDate :: Newtype UpdateDate _
derive instance repGenericUpdateDate :: Generic UpdateDate _
instance showUpdateDate :: Show UpdateDate where
  show = genericShow
instance decodeUpdateDate :: Decode UpdateDate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDate :: Encode UpdateDate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to update an environment.</p>
newtype UpdateEnvironmentMessage = UpdateEnvironmentMessage 
  { "ApplicationName" :: NullOrUndefined.NullOrUndefined (ApplicationName)
  , "EnvironmentId" :: NullOrUndefined.NullOrUndefined (EnvironmentId)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "GroupName" :: NullOrUndefined.NullOrUndefined (GroupName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Tier" :: NullOrUndefined.NullOrUndefined (EnvironmentTier)
  , "VersionLabel" :: NullOrUndefined.NullOrUndefined (VersionLabel)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "SolutionStackName" :: NullOrUndefined.NullOrUndefined (SolutionStackName)
  , "PlatformArn" :: NullOrUndefined.NullOrUndefined (PlatformArn)
  , "OptionSettings" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionSettingsList)
  , "OptionsToRemove" :: NullOrUndefined.NullOrUndefined (OptionsSpecifierList)
  }
derive instance newtypeUpdateEnvironmentMessage :: Newtype UpdateEnvironmentMessage _
derive instance repGenericUpdateEnvironmentMessage :: Generic UpdateEnvironmentMessage _
instance showUpdateEnvironmentMessage :: Show UpdateEnvironmentMessage where
  show = genericShow
instance decodeUpdateEnvironmentMessage :: Decode UpdateEnvironmentMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateEnvironmentMessage :: Encode UpdateEnvironmentMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTagsForResourceMessage = UpdateTagsForResourceMessage 
  { "ResourceArn" :: (ResourceArn)
  , "TagsToAdd" :: NullOrUndefined.NullOrUndefined (TagList)
  , "TagsToRemove" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  }
derive instance newtypeUpdateTagsForResourceMessage :: Newtype UpdateTagsForResourceMessage _
derive instance repGenericUpdateTagsForResourceMessage :: Generic UpdateTagsForResourceMessage _
instance showUpdateTagsForResourceMessage :: Show UpdateTagsForResourceMessage where
  show = genericShow
instance decodeUpdateTagsForResourceMessage :: Decode UpdateTagsForResourceMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTagsForResourceMessage :: Encode UpdateTagsForResourceMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserDefinedOption = UserDefinedOption Boolean
derive instance newtypeUserDefinedOption :: Newtype UserDefinedOption _
derive instance repGenericUserDefinedOption :: Generic UserDefinedOption _
instance showUserDefinedOption :: Show UserDefinedOption where
  show = genericShow
instance decodeUserDefinedOption :: Decode UserDefinedOption where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserDefinedOption :: Encode UserDefinedOption where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of validation messages for a specified configuration template.</p>
newtype ValidateConfigurationSettingsMessage = ValidateConfigurationSettingsMessage 
  { "ApplicationName" :: (ApplicationName)
  , "TemplateName" :: NullOrUndefined.NullOrUndefined (ConfigurationTemplateName)
  , "EnvironmentName" :: NullOrUndefined.NullOrUndefined (EnvironmentName)
  , "OptionSettings" :: (ConfigurationOptionSettingsList)
  }
derive instance newtypeValidateConfigurationSettingsMessage :: Newtype ValidateConfigurationSettingsMessage _
derive instance repGenericValidateConfigurationSettingsMessage :: Generic ValidateConfigurationSettingsMessage _
instance showValidateConfigurationSettingsMessage :: Show ValidateConfigurationSettingsMessage where
  show = genericShow
instance decodeValidateConfigurationSettingsMessage :: Decode ValidateConfigurationSettingsMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidateConfigurationSettingsMessage :: Encode ValidateConfigurationSettingsMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error or warning for a desired configuration option value.</p>
newtype ValidationMessage = ValidationMessage 
  { "Message" :: NullOrUndefined.NullOrUndefined (ValidationMessageString)
  , "Severity" :: NullOrUndefined.NullOrUndefined (ValidationSeverity)
  , "Namespace" :: NullOrUndefined.NullOrUndefined (OptionNamespace)
  , "OptionName" :: NullOrUndefined.NullOrUndefined (ConfigurationOptionName)
  }
derive instance newtypeValidationMessage :: Newtype ValidationMessage _
derive instance repGenericValidationMessage :: Generic ValidationMessage _
instance showValidationMessage :: Show ValidationMessage where
  show = genericShow
instance decodeValidationMessage :: Decode ValidationMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationMessage :: Encode ValidationMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValidationMessageString = ValidationMessageString String
derive instance newtypeValidationMessageString :: Newtype ValidationMessageString _
derive instance repGenericValidationMessageString :: Generic ValidationMessageString _
instance showValidationMessageString :: Show ValidationMessageString where
  show = genericShow
instance decodeValidationMessageString :: Decode ValidationMessageString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationMessageString :: Encode ValidationMessageString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValidationMessagesList = ValidationMessagesList (Array ValidationMessage)
derive instance newtypeValidationMessagesList :: Newtype ValidationMessagesList _
derive instance repGenericValidationMessagesList :: Generic ValidationMessagesList _
instance showValidationMessagesList :: Show ValidationMessagesList where
  show = genericShow
instance decodeValidationMessagesList :: Decode ValidationMessagesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationMessagesList :: Encode ValidationMessagesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValidationSeverity = ValidationSeverity String
derive instance newtypeValidationSeverity :: Newtype ValidationSeverity _
derive instance repGenericValidationSeverity :: Generic ValidationSeverity _
instance showValidationSeverity :: Show ValidationSeverity where
  show = genericShow
instance decodeValidationSeverity :: Decode ValidationSeverity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationSeverity :: Encode ValidationSeverity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VersionLabel = VersionLabel String
derive instance newtypeVersionLabel :: Newtype VersionLabel _
derive instance repGenericVersionLabel :: Generic VersionLabel _
instance showVersionLabel :: Show VersionLabel where
  show = genericShow
instance decodeVersionLabel :: Decode VersionLabel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionLabel :: Encode VersionLabel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VersionLabels = VersionLabels (Array VersionLabel)
derive instance newtypeVersionLabels :: Newtype VersionLabels _
derive instance repGenericVersionLabels :: Generic VersionLabels _
instance showVersionLabels :: Show VersionLabels where
  show = genericShow
instance decodeVersionLabels :: Decode VersionLabels where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionLabels :: Encode VersionLabels where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VersionLabelsList = VersionLabelsList (Array VersionLabel)
derive instance newtypeVersionLabelsList :: Newtype VersionLabelsList _
derive instance repGenericVersionLabelsList :: Generic VersionLabelsList _
instance showVersionLabelsList :: Show VersionLabelsList where
  show = genericShow
instance decodeVersionLabelsList :: Decode VersionLabelsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionLabelsList :: Encode VersionLabelsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VirtualizationType = VirtualizationType String
derive instance newtypeVirtualizationType :: Newtype VirtualizationType _
derive instance repGenericVirtualizationType :: Generic VirtualizationType _
instance showVirtualizationType :: Show VirtualizationType where
  show = genericShow
instance decodeVirtualizationType :: Decode VirtualizationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVirtualizationType :: Encode VirtualizationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
