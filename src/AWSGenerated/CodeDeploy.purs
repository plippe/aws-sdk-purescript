

-- | <fullname>AWS CodeDeploy</fullname> <p>AWS CodeDeploy is a deployment service that automates application deployments to Amazon EC2 instances, on-premises instances running in your own facility, or serverless AWS Lambda functions.</p> <p>You can deploy a nearly unlimited variety of application content, such as an updated Lambda function, code, web and configuration files, executables, packages, scripts, multimedia files, and so on. AWS CodeDeploy can deploy application content stored in Amazon S3 buckets, GitHub repositories, or Bitbucket repositories. You do not need to make changes to your existing code before you can use AWS CodeDeploy.</p> <p>AWS CodeDeploy makes it easier for you to rapidly release new features, helps you avoid downtime during application deployment, and handles the complexity of updating your applications, without many of the risks associated with error-prone manual deployments.</p> <p> <b>AWS CodeDeploy Components</b> </p> <p>Use the information in this guide to help you work with the following AWS CodeDeploy components:</p> <ul> <li> <p> <b>Application</b>: A name that uniquely identifies the application you want to deploy. AWS CodeDeploy uses this name, which functions as a container, to ensure the correct combination of revision, deployment configuration, and deployment group are referenced during a deployment.</p> </li> <li> <p> <b>Deployment group</b>: A set of individual instances or CodeDeploy Lambda applications. A Lambda deployment group contains a group of applications. An EC2/On-premises deployment group contains individually tagged instances, Amazon EC2 instances in Auto Scaling groups, or both. </p> </li> <li> <p> <b>Deployment configuration</b>: A set of deployment rules and deployment success and failure conditions used by AWS CodeDeploy during a deployment.</p> </li> <li> <p> <b>Deployment</b>: The process and the components used in the process of updating a Lambda function or of installing content on one or more instances. </p> </li> <li> <p> <b>Application revisions</b>: For an AWS Lambda deployment, this is an AppSpec file that specifies the Lambda function to update and one or more functions to validate deployment lifecycle events. For an EC2/On-premises deployment, this is an archive file containing source content—source code, web pages, executable files, and deployment scripts—along with an AppSpec file. Revisions are stored in Amazon S3 buckets or GitHub repositories. For Amazon S3, a revision is uniquely identified by its Amazon S3 object key and its ETag, version, or both. For GitHub, a revision is uniquely identified by its commit ID.</p> </li> </ul> <p>This guide also contains information to help you get details about the instances in your deployments, to make on-premises instances available for AWS CodeDeploy deployments, and to get details about a Lambda function deployment.</p> <p> <b>AWS CodeDeploy Information Resources</b> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/codedeploy/latest/userguide">AWS CodeDeploy User Guide</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/codedeploy/latest/APIReference/">AWS CodeDeploy API Reference Guide</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/cli/latest/reference/deploy/index.html">AWS CLI Reference for AWS CodeDeploy</a> </p> </li> <li> <p> <a href="https://forums.aws.amazon.com/forum.jspa?forumID=179">AWS CodeDeploy Developer Forum</a> </p> </li> </ul>
module AWS.CodeDeploy where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CodeDeploy" :: String


-- | <p>Adds tags to on-premises instances.</p>
addTagsToOnPremisesInstances :: forall eff. AddTagsToOnPremisesInstancesInput -> Aff (err :: AWS.RequestError | eff) Unit
addTagsToOnPremisesInstances = AWS.request serviceName "AddTagsToOnPremisesInstances" 


-- | <p>Gets information about one or more application revisions.</p>
batchGetApplicationRevisions :: forall eff. BatchGetApplicationRevisionsInput -> Aff (err :: AWS.RequestError | eff) BatchGetApplicationRevisionsOutput
batchGetApplicationRevisions = AWS.request serviceName "BatchGetApplicationRevisions" 


-- | <p>Gets information about one or more applications.</p>
batchGetApplications :: forall eff. BatchGetApplicationsInput -> Aff (err :: AWS.RequestError | eff) BatchGetApplicationsOutput
batchGetApplications = AWS.request serviceName "BatchGetApplications" 


-- | <p>Gets information about one or more deployment groups.</p>
batchGetDeploymentGroups :: forall eff. BatchGetDeploymentGroupsInput -> Aff (err :: AWS.RequestError | eff) BatchGetDeploymentGroupsOutput
batchGetDeploymentGroups = AWS.request serviceName "BatchGetDeploymentGroups" 


-- | <p>Gets information about one or more instance that are part of a deployment group.</p>
batchGetDeploymentInstances :: forall eff. BatchGetDeploymentInstancesInput -> Aff (err :: AWS.RequestError | eff) BatchGetDeploymentInstancesOutput
batchGetDeploymentInstances = AWS.request serviceName "BatchGetDeploymentInstances" 


-- | <p>Gets information about one or more deployments.</p>
batchGetDeployments :: forall eff. BatchGetDeploymentsInput -> Aff (err :: AWS.RequestError | eff) BatchGetDeploymentsOutput
batchGetDeployments = AWS.request serviceName "BatchGetDeployments" 


-- | <p>Gets information about one or more on-premises instances.</p>
batchGetOnPremisesInstances :: forall eff. BatchGetOnPremisesInstancesInput -> Aff (err :: AWS.RequestError | eff) BatchGetOnPremisesInstancesOutput
batchGetOnPremisesInstances = AWS.request serviceName "BatchGetOnPremisesInstances" 


-- | <p>For a blue/green deployment, starts the process of rerouting traffic from instances in the original environment to instances in the replacement environment without waiting for a specified wait time to elapse. (Traffic rerouting, which is achieved by registering instances in the replacement environment with the load balancer, can start as soon as all instances have a status of Ready.) </p>
continueDeployment :: forall eff. ContinueDeploymentInput -> Aff (err :: AWS.RequestError | eff) Unit
continueDeployment = AWS.request serviceName "ContinueDeployment" 


-- | <p>Creates an application.</p>
createApplication :: forall eff. CreateApplicationInput -> Aff (err :: AWS.RequestError | eff) CreateApplicationOutput
createApplication = AWS.request serviceName "CreateApplication" 


-- | <p>Deploys an application revision through the specified deployment group.</p>
createDeployment :: forall eff. CreateDeploymentInput -> Aff (err :: AWS.RequestError | eff) CreateDeploymentOutput
createDeployment = AWS.request serviceName "CreateDeployment" 


-- | <p>Creates a deployment configuration.</p>
createDeploymentConfig :: forall eff. CreateDeploymentConfigInput -> Aff (err :: AWS.RequestError | eff) CreateDeploymentConfigOutput
createDeploymentConfig = AWS.request serviceName "CreateDeploymentConfig" 


-- | <p>Creates a deployment group to which application revisions will be deployed.</p>
createDeploymentGroup :: forall eff. CreateDeploymentGroupInput -> Aff (err :: AWS.RequestError | eff) CreateDeploymentGroupOutput
createDeploymentGroup = AWS.request serviceName "CreateDeploymentGroup" 


-- | <p>Deletes an application.</p>
deleteApplication :: forall eff. DeleteApplicationInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteApplication = AWS.request serviceName "DeleteApplication" 


-- | <p>Deletes a deployment configuration.</p> <note> <p>A deployment configuration cannot be deleted if it is currently in use. Predefined configurations cannot be deleted.</p> </note>
deleteDeploymentConfig :: forall eff. DeleteDeploymentConfigInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteDeploymentConfig = AWS.request serviceName "DeleteDeploymentConfig" 


-- | <p>Deletes a deployment group.</p>
deleteDeploymentGroup :: forall eff. DeleteDeploymentGroupInput -> Aff (err :: AWS.RequestError | eff) DeleteDeploymentGroupOutput
deleteDeploymentGroup = AWS.request serviceName "DeleteDeploymentGroup" 


-- | <p>Deletes a GitHub account connection.</p>
deleteGitHubAccountToken :: forall eff. DeleteGitHubAccountTokenInput -> Aff (err :: AWS.RequestError | eff) DeleteGitHubAccountTokenOutput
deleteGitHubAccountToken = AWS.request serviceName "DeleteGitHubAccountToken" 


-- | <p>Deregisters an on-premises instance.</p>
deregisterOnPremisesInstance :: forall eff. DeregisterOnPremisesInstanceInput -> Aff (err :: AWS.RequestError | eff) Unit
deregisterOnPremisesInstance = AWS.request serviceName "DeregisterOnPremisesInstance" 


-- | <p>Gets information about an application.</p>
getApplication :: forall eff. GetApplicationInput -> Aff (err :: AWS.RequestError | eff) GetApplicationOutput
getApplication = AWS.request serviceName "GetApplication" 


-- | <p>Gets information about an application revision.</p>
getApplicationRevision :: forall eff. GetApplicationRevisionInput -> Aff (err :: AWS.RequestError | eff) GetApplicationRevisionOutput
getApplicationRevision = AWS.request serviceName "GetApplicationRevision" 


-- | <p>Gets information about a deployment.</p>
getDeployment :: forall eff. GetDeploymentInput -> Aff (err :: AWS.RequestError | eff) GetDeploymentOutput
getDeployment = AWS.request serviceName "GetDeployment" 


-- | <p>Gets information about a deployment configuration.</p>
getDeploymentConfig :: forall eff. GetDeploymentConfigInput -> Aff (err :: AWS.RequestError | eff) GetDeploymentConfigOutput
getDeploymentConfig = AWS.request serviceName "GetDeploymentConfig" 


-- | <p>Gets information about a deployment group.</p>
getDeploymentGroup :: forall eff. GetDeploymentGroupInput -> Aff (err :: AWS.RequestError | eff) GetDeploymentGroupOutput
getDeploymentGroup = AWS.request serviceName "GetDeploymentGroup" 


-- | <p>Gets information about an instance as part of a deployment.</p>
getDeploymentInstance :: forall eff. GetDeploymentInstanceInput -> Aff (err :: AWS.RequestError | eff) GetDeploymentInstanceOutput
getDeploymentInstance = AWS.request serviceName "GetDeploymentInstance" 


-- | <p>Gets information about an on-premises instance.</p>
getOnPremisesInstance :: forall eff. GetOnPremisesInstanceInput -> Aff (err :: AWS.RequestError | eff) GetOnPremisesInstanceOutput
getOnPremisesInstance = AWS.request serviceName "GetOnPremisesInstance" 


-- | <p>Lists information about revisions for an application.</p>
listApplicationRevisions :: forall eff. ListApplicationRevisionsInput -> Aff (err :: AWS.RequestError | eff) ListApplicationRevisionsOutput
listApplicationRevisions = AWS.request serviceName "ListApplicationRevisions" 


-- | <p>Lists the applications registered with the applicable IAM user or AWS account.</p>
listApplications :: forall eff. ListApplicationsInput -> Aff (err :: AWS.RequestError | eff) ListApplicationsOutput
listApplications = AWS.request serviceName "ListApplications" 


-- | <p>Lists the deployment configurations with the applicable IAM user or AWS account.</p>
listDeploymentConfigs :: forall eff. ListDeploymentConfigsInput -> Aff (err :: AWS.RequestError | eff) ListDeploymentConfigsOutput
listDeploymentConfigs = AWS.request serviceName "ListDeploymentConfigs" 


-- | <p>Lists the deployment groups for an application registered with the applicable IAM user or AWS account.</p>
listDeploymentGroups :: forall eff. ListDeploymentGroupsInput -> Aff (err :: AWS.RequestError | eff) ListDeploymentGroupsOutput
listDeploymentGroups = AWS.request serviceName "ListDeploymentGroups" 


-- | <p>Lists the instance for a deployment associated with the applicable IAM user or AWS account.</p>
listDeploymentInstances :: forall eff. ListDeploymentInstancesInput -> Aff (err :: AWS.RequestError | eff) ListDeploymentInstancesOutput
listDeploymentInstances = AWS.request serviceName "ListDeploymentInstances" 


-- | <p>Lists the deployments in a deployment group for an application registered with the applicable IAM user or AWS account.</p>
listDeployments :: forall eff. ListDeploymentsInput -> Aff (err :: AWS.RequestError | eff) ListDeploymentsOutput
listDeployments = AWS.request serviceName "ListDeployments" 


-- | <p>Lists the names of stored connections to GitHub accounts.</p>
listGitHubAccountTokenNames :: forall eff. ListGitHubAccountTokenNamesInput -> Aff (err :: AWS.RequestError | eff) ListGitHubAccountTokenNamesOutput
listGitHubAccountTokenNames = AWS.request serviceName "ListGitHubAccountTokenNames" 


-- | <p>Gets a list of names for one or more on-premises instances.</p> <p>Unless otherwise specified, both registered and deregistered on-premises instance names will be listed. To list only registered or deregistered on-premises instance names, use the registration status parameter.</p>
listOnPremisesInstances :: forall eff. ListOnPremisesInstancesInput -> Aff (err :: AWS.RequestError | eff) ListOnPremisesInstancesOutput
listOnPremisesInstances = AWS.request serviceName "ListOnPremisesInstances" 


-- | <p>Sets the result of a Lambda validation function. The function validates one or both lifecycle events (<code>BeforeAllowTraffic</code> and <code>AfterAllowTraffic</code>) and returns <code>Succeeded</code> or <code>Failed</code>.</p>
putLifecycleEventHookExecutionStatus :: forall eff. PutLifecycleEventHookExecutionStatusInput -> Aff (err :: AWS.RequestError | eff) PutLifecycleEventHookExecutionStatusOutput
putLifecycleEventHookExecutionStatus = AWS.request serviceName "PutLifecycleEventHookExecutionStatus" 


-- | <p>Registers with AWS CodeDeploy a revision for the specified application.</p>
registerApplicationRevision :: forall eff. RegisterApplicationRevisionInput -> Aff (err :: AWS.RequestError | eff) Unit
registerApplicationRevision = AWS.request serviceName "RegisterApplicationRevision" 


-- | <p>Registers an on-premises instance.</p> <note> <p>Only one IAM ARN (an IAM session ARN or IAM user ARN) is supported in the request. You cannot use both.</p> </note>
registerOnPremisesInstance :: forall eff. RegisterOnPremisesInstanceInput -> Aff (err :: AWS.RequestError | eff) Unit
registerOnPremisesInstance = AWS.request serviceName "RegisterOnPremisesInstance" 


-- | <p>Removes one or more tags from one or more on-premises instances.</p>
removeTagsFromOnPremisesInstances :: forall eff. RemoveTagsFromOnPremisesInstancesInput -> Aff (err :: AWS.RequestError | eff) Unit
removeTagsFromOnPremisesInstances = AWS.request serviceName "RemoveTagsFromOnPremisesInstances" 


-- | <p>In a blue/green deployment, overrides any specified wait time and starts terminating instances immediately after the traffic routing is completed.</p>
skipWaitTimeForInstanceTermination :: forall eff. SkipWaitTimeForInstanceTerminationInput -> Aff (err :: AWS.RequestError | eff) Unit
skipWaitTimeForInstanceTermination = AWS.request serviceName "SkipWaitTimeForInstanceTermination" 


-- | <p>Attempts to stop an ongoing deployment.</p>
stopDeployment :: forall eff. StopDeploymentInput -> Aff (err :: AWS.RequestError | eff) StopDeploymentOutput
stopDeployment = AWS.request serviceName "StopDeployment" 


-- | <p>Changes the name of an application.</p>
updateApplication :: forall eff. UpdateApplicationInput -> Aff (err :: AWS.RequestError | eff) Unit
updateApplication = AWS.request serviceName "UpdateApplication" 


-- | <p>Changes information about a deployment group.</p>
updateDeploymentGroup :: forall eff. UpdateDeploymentGroupInput -> Aff (err :: AWS.RequestError | eff) UpdateDeploymentGroupOutput
updateDeploymentGroup = AWS.request serviceName "UpdateDeploymentGroup" 


-- | <p>Represents the input of, and adds tags to, an on-premises instance operation.</p>
newtype AddTagsToOnPremisesInstancesInput = AddTagsToOnPremisesInstancesInput 
  { "Tags'" :: (TagList)
  , "InstanceNames'" :: (InstanceNameList)
  }
derive instance newtypeAddTagsToOnPremisesInstancesInput :: Newtype AddTagsToOnPremisesInstancesInput _


newtype AdditionalDeploymentStatusInfo = AdditionalDeploymentStatusInfo String
derive instance newtypeAdditionalDeploymentStatusInfo :: Newtype AdditionalDeploymentStatusInfo _


-- | <p>Information about an alarm.</p>
newtype Alarm = Alarm 
  { "Name'" :: NullOrUndefined (AlarmName)
  }
derive instance newtypeAlarm :: Newtype Alarm _


-- | <p>Information about alarms associated with the deployment group.</p>
newtype AlarmConfiguration = AlarmConfiguration 
  { "Enabled'" :: NullOrUndefined (Boolean)
  , "IgnorePollAlarmFailure'" :: NullOrUndefined (Boolean)
  , "Alarms'" :: NullOrUndefined (AlarmList)
  }
derive instance newtypeAlarmConfiguration :: Newtype AlarmConfiguration _


newtype AlarmList = AlarmList (Array Alarm)
derive instance newtypeAlarmList :: Newtype AlarmList _


newtype AlarmName = AlarmName String
derive instance newtypeAlarmName :: Newtype AlarmName _


-- | <p>The maximum number of alarms for a deployment group (10) was exceeded.</p>
newtype AlarmsLimitExceededException = AlarmsLimitExceededException 
  { 
  }
derive instance newtypeAlarmsLimitExceededException :: Newtype AlarmsLimitExceededException _


-- | <p>An application with the specified name already exists with the applicable IAM user or AWS account.</p>
newtype ApplicationAlreadyExistsException = ApplicationAlreadyExistsException 
  { 
  }
derive instance newtypeApplicationAlreadyExistsException :: Newtype ApplicationAlreadyExistsException _


-- | <p>The application does not exist with the applicable IAM user or AWS account.</p>
newtype ApplicationDoesNotExistException = ApplicationDoesNotExistException 
  { 
  }
derive instance newtypeApplicationDoesNotExistException :: Newtype ApplicationDoesNotExistException _


newtype ApplicationId = ApplicationId String
derive instance newtypeApplicationId :: Newtype ApplicationId _


-- | <p>Information about an application.</p>
newtype ApplicationInfo = ApplicationInfo 
  { "ApplicationId'" :: NullOrUndefined (ApplicationId)
  , "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "CreateTime'" :: NullOrUndefined (Number)
  , "LinkedToGitHub'" :: NullOrUndefined (Boolean)
  , "GitHubAccountName'" :: NullOrUndefined (GitHubAccountTokenName)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  }
derive instance newtypeApplicationInfo :: Newtype ApplicationInfo _


-- | <p>More applications were attempted to be created than are allowed.</p>
newtype ApplicationLimitExceededException = ApplicationLimitExceededException 
  { 
  }
derive instance newtypeApplicationLimitExceededException :: Newtype ApplicationLimitExceededException _


newtype ApplicationName = ApplicationName String
derive instance newtypeApplicationName :: Newtype ApplicationName _


-- | <p>The minimum number of required application names was not specified.</p>
newtype ApplicationNameRequiredException = ApplicationNameRequiredException 
  { 
  }
derive instance newtypeApplicationNameRequiredException :: Newtype ApplicationNameRequiredException _


newtype ApplicationRevisionSortBy = ApplicationRevisionSortBy String
derive instance newtypeApplicationRevisionSortBy :: Newtype ApplicationRevisionSortBy _


newtype ApplicationsInfoList = ApplicationsInfoList (Array ApplicationInfo)
derive instance newtypeApplicationsInfoList :: Newtype ApplicationsInfoList _


newtype ApplicationsList = ApplicationsList (Array ApplicationName)
derive instance newtypeApplicationsList :: Newtype ApplicationsList _


-- | <p>Information about a configuration for automatically rolling back to a previous version of an application revision when a deployment doesn't complete successfully.</p>
newtype AutoRollbackConfiguration = AutoRollbackConfiguration 
  { "Enabled'" :: NullOrUndefined (Boolean)
  , "Events'" :: NullOrUndefined (AutoRollbackEventsList)
  }
derive instance newtypeAutoRollbackConfiguration :: Newtype AutoRollbackConfiguration _


newtype AutoRollbackEvent = AutoRollbackEvent String
derive instance newtypeAutoRollbackEvent :: Newtype AutoRollbackEvent _


newtype AutoRollbackEventsList = AutoRollbackEventsList (Array AutoRollbackEvent)
derive instance newtypeAutoRollbackEventsList :: Newtype AutoRollbackEventsList _


-- | <p>Information about an Auto Scaling group.</p>
newtype AutoScalingGroup = AutoScalingGroup 
  { "Name'" :: NullOrUndefined (AutoScalingGroupName)
  , "Hook'" :: NullOrUndefined (AutoScalingGroupHook)
  }
derive instance newtypeAutoScalingGroup :: Newtype AutoScalingGroup _


newtype AutoScalingGroupHook = AutoScalingGroupHook String
derive instance newtypeAutoScalingGroupHook :: Newtype AutoScalingGroupHook _


newtype AutoScalingGroupList = AutoScalingGroupList (Array AutoScalingGroup)
derive instance newtypeAutoScalingGroupList :: Newtype AutoScalingGroupList _


newtype AutoScalingGroupName = AutoScalingGroupName String
derive instance newtypeAutoScalingGroupName :: Newtype AutoScalingGroupName _


newtype AutoScalingGroupNameList = AutoScalingGroupNameList (Array AutoScalingGroupName)
derive instance newtypeAutoScalingGroupNameList :: Newtype AutoScalingGroupNameList _


-- | <p>Represents the input of a BatchGetApplicationRevisions operation.</p>
newtype BatchGetApplicationRevisionsInput = BatchGetApplicationRevisionsInput 
  { "ApplicationName'" :: (ApplicationName)
  , "Revisions'" :: (RevisionLocationList)
  }
derive instance newtypeBatchGetApplicationRevisionsInput :: Newtype BatchGetApplicationRevisionsInput _


-- | <p>Represents the output of a BatchGetApplicationRevisions operation.</p>
newtype BatchGetApplicationRevisionsOutput = BatchGetApplicationRevisionsOutput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "ErrorMessage'" :: NullOrUndefined (ErrorMessage)
  , "Revisions'" :: NullOrUndefined (RevisionInfoList)
  }
derive instance newtypeBatchGetApplicationRevisionsOutput :: Newtype BatchGetApplicationRevisionsOutput _


-- | <p>Represents the input of a BatchGetApplications operation.</p>
newtype BatchGetApplicationsInput = BatchGetApplicationsInput 
  { "ApplicationNames'" :: (ApplicationsList)
  }
derive instance newtypeBatchGetApplicationsInput :: Newtype BatchGetApplicationsInput _


-- | <p>Represents the output of a BatchGetApplications operation.</p>
newtype BatchGetApplicationsOutput = BatchGetApplicationsOutput 
  { "ApplicationsInfo'" :: NullOrUndefined (ApplicationsInfoList)
  }
derive instance newtypeBatchGetApplicationsOutput :: Newtype BatchGetApplicationsOutput _


-- | <p>Represents the input of a BatchGetDeploymentGroups operation.</p>
newtype BatchGetDeploymentGroupsInput = BatchGetDeploymentGroupsInput 
  { "ApplicationName'" :: (ApplicationName)
  , "DeploymentGroupNames'" :: (DeploymentGroupsList)
  }
derive instance newtypeBatchGetDeploymentGroupsInput :: Newtype BatchGetDeploymentGroupsInput _


-- | <p>Represents the output of a BatchGetDeploymentGroups operation.</p>
newtype BatchGetDeploymentGroupsOutput = BatchGetDeploymentGroupsOutput 
  { "DeploymentGroupsInfo'" :: NullOrUndefined (DeploymentGroupInfoList)
  , "ErrorMessage'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBatchGetDeploymentGroupsOutput :: Newtype BatchGetDeploymentGroupsOutput _


-- | <p>Represents the input of a BatchGetDeploymentInstances operation.</p>
newtype BatchGetDeploymentInstancesInput = BatchGetDeploymentInstancesInput 
  { "DeploymentId'" :: (DeploymentId)
  , "InstanceIds'" :: (InstancesList)
  }
derive instance newtypeBatchGetDeploymentInstancesInput :: Newtype BatchGetDeploymentInstancesInput _


-- | <p>Represents the output of a BatchGetDeploymentInstances operation.</p>
newtype BatchGetDeploymentInstancesOutput = BatchGetDeploymentInstancesOutput 
  { "InstancesSummary'" :: NullOrUndefined (InstanceSummaryList)
  , "ErrorMessage'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBatchGetDeploymentInstancesOutput :: Newtype BatchGetDeploymentInstancesOutput _


-- | <p>Represents the input of a BatchGetDeployments operation.</p>
newtype BatchGetDeploymentsInput = BatchGetDeploymentsInput 
  { "DeploymentIds'" :: (DeploymentsList)
  }
derive instance newtypeBatchGetDeploymentsInput :: Newtype BatchGetDeploymentsInput _


-- | <p>Represents the output of a BatchGetDeployments operation.</p>
newtype BatchGetDeploymentsOutput = BatchGetDeploymentsOutput 
  { "DeploymentsInfo'" :: NullOrUndefined (DeploymentsInfoList)
  }
derive instance newtypeBatchGetDeploymentsOutput :: Newtype BatchGetDeploymentsOutput _


-- | <p>Represents the input of a BatchGetOnPremisesInstances operation.</p>
newtype BatchGetOnPremisesInstancesInput = BatchGetOnPremisesInstancesInput 
  { "InstanceNames'" :: (InstanceNameList)
  }
derive instance newtypeBatchGetOnPremisesInstancesInput :: Newtype BatchGetOnPremisesInstancesInput _


-- | <p>Represents the output of a BatchGetOnPremisesInstances operation.</p>
newtype BatchGetOnPremisesInstancesOutput = BatchGetOnPremisesInstancesOutput 
  { "InstanceInfos'" :: NullOrUndefined (InstanceInfoList)
  }
derive instance newtypeBatchGetOnPremisesInstancesOutput :: Newtype BatchGetOnPremisesInstancesOutput _


-- | <p>The maximum number of names or IDs allowed for this request (100) was exceeded.</p>
newtype BatchLimitExceededException = BatchLimitExceededException 
  { 
  }
derive instance newtypeBatchLimitExceededException :: Newtype BatchLimitExceededException _


-- | <p>Information about blue/green deployment options for a deployment group.</p>
newtype BlueGreenDeploymentConfiguration = BlueGreenDeploymentConfiguration 
  { "TerminateBlueInstancesOnDeploymentSuccess'" :: NullOrUndefined (BlueInstanceTerminationOption)
  , "DeploymentReadyOption'" :: NullOrUndefined (DeploymentReadyOption)
  , "GreenFleetProvisioningOption'" :: NullOrUndefined (GreenFleetProvisioningOption)
  }
derive instance newtypeBlueGreenDeploymentConfiguration :: Newtype BlueGreenDeploymentConfiguration _


-- | <p>Information about whether instances in the original environment are terminated when a blue/green deployment is successful.</p>
newtype BlueInstanceTerminationOption = BlueInstanceTerminationOption 
  { "Action'" :: NullOrUndefined (InstanceAction)
  , "TerminationWaitTimeInMinutes'" :: NullOrUndefined (Duration)
  }
derive instance newtypeBlueInstanceTerminationOption :: Newtype BlueInstanceTerminationOption _


-- | <p>A bucket name is required, but was not provided.</p>
newtype BucketNameFilterRequiredException = BucketNameFilterRequiredException 
  { 
  }
derive instance newtypeBucketNameFilterRequiredException :: Newtype BucketNameFilterRequiredException _


newtype BundleType = BundleType String
derive instance newtypeBundleType :: Newtype BundleType _


newtype CommitId = CommitId String
derive instance newtypeCommitId :: Newtype CommitId _


newtype ComputePlatform = ComputePlatform String
derive instance newtypeComputePlatform :: Newtype ComputePlatform _


newtype ContinueDeploymentInput = ContinueDeploymentInput 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  }
derive instance newtypeContinueDeploymentInput :: Newtype ContinueDeploymentInput _


-- | <p>Represents the input of a CreateApplication operation.</p>
newtype CreateApplicationInput = CreateApplicationInput 
  { "ApplicationName'" :: (ApplicationName)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  }
derive instance newtypeCreateApplicationInput :: Newtype CreateApplicationInput _


-- | <p>Represents the output of a CreateApplication operation.</p>
newtype CreateApplicationOutput = CreateApplicationOutput 
  { "ApplicationId'" :: NullOrUndefined (ApplicationId)
  }
derive instance newtypeCreateApplicationOutput :: Newtype CreateApplicationOutput _


-- | <p>Represents the input of a CreateDeploymentConfig operation.</p>
newtype CreateDeploymentConfigInput = CreateDeploymentConfigInput 
  { "DeploymentConfigName'" :: (DeploymentConfigName)
  , "MinimumHealthyHosts'" :: NullOrUndefined (MinimumHealthyHosts)
  , "TrafficRoutingConfig'" :: NullOrUndefined (TrafficRoutingConfig)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  }
derive instance newtypeCreateDeploymentConfigInput :: Newtype CreateDeploymentConfigInput _


-- | <p>Represents the output of a CreateDeploymentConfig operation.</p>
newtype CreateDeploymentConfigOutput = CreateDeploymentConfigOutput 
  { "DeploymentConfigId'" :: NullOrUndefined (DeploymentConfigId)
  }
derive instance newtypeCreateDeploymentConfigOutput :: Newtype CreateDeploymentConfigOutput _


-- | <p>Represents the input of a CreateDeploymentGroup operation.</p>
newtype CreateDeploymentGroupInput = CreateDeploymentGroupInput 
  { "ApplicationName'" :: (ApplicationName)
  , "DeploymentGroupName'" :: (DeploymentGroupName)
  , "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName)
  , "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList)
  , "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList)
  , "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList)
  , "ServiceRoleArn'" :: (Role)
  , "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList)
  , "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration)
  , "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration)
  , "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle)
  , "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration)
  , "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo)
  , "Ec2TagSet'" :: NullOrUndefined (EC2TagSet)
  , "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet)
  }
derive instance newtypeCreateDeploymentGroupInput :: Newtype CreateDeploymentGroupInput _


-- | <p>Represents the output of a CreateDeploymentGroup operation.</p>
newtype CreateDeploymentGroupOutput = CreateDeploymentGroupOutput 
  { "DeploymentGroupId'" :: NullOrUndefined (DeploymentGroupId)
  }
derive instance newtypeCreateDeploymentGroupOutput :: Newtype CreateDeploymentGroupOutput _


-- | <p>Represents the input of a CreateDeployment operation.</p>
newtype CreateDeploymentInput = CreateDeploymentInput 
  { "ApplicationName'" :: (ApplicationName)
  , "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName)
  , "Revision'" :: NullOrUndefined (RevisionLocation)
  , "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName)
  , "Description'" :: NullOrUndefined (Description)
  , "IgnoreApplicationStopFailures'" :: NullOrUndefined (Boolean)
  , "TargetInstances'" :: NullOrUndefined (TargetInstances)
  , "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration)
  , "UpdateOutdatedInstancesOnly'" :: NullOrUndefined (Boolean)
  , "FileExistsBehavior'" :: NullOrUndefined (FileExistsBehavior)
  }
derive instance newtypeCreateDeploymentInput :: Newtype CreateDeploymentInput _


-- | <p>Represents the output of a CreateDeployment operation.</p>
newtype CreateDeploymentOutput = CreateDeploymentOutput 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  }
derive instance newtypeCreateDeploymentOutput :: Newtype CreateDeploymentOutput _


-- | <p>Represents the input of a DeleteApplication operation.</p>
newtype DeleteApplicationInput = DeleteApplicationInput 
  { "ApplicationName'" :: (ApplicationName)
  }
derive instance newtypeDeleteApplicationInput :: Newtype DeleteApplicationInput _


-- | <p>Represents the input of a DeleteDeploymentConfig operation.</p>
newtype DeleteDeploymentConfigInput = DeleteDeploymentConfigInput 
  { "DeploymentConfigName'" :: (DeploymentConfigName)
  }
derive instance newtypeDeleteDeploymentConfigInput :: Newtype DeleteDeploymentConfigInput _


-- | <p>Represents the input of a DeleteDeploymentGroup operation.</p>
newtype DeleteDeploymentGroupInput = DeleteDeploymentGroupInput 
  { "ApplicationName'" :: (ApplicationName)
  , "DeploymentGroupName'" :: (DeploymentGroupName)
  }
derive instance newtypeDeleteDeploymentGroupInput :: Newtype DeleteDeploymentGroupInput _


-- | <p>Represents the output of a DeleteDeploymentGroup operation.</p>
newtype DeleteDeploymentGroupOutput = DeleteDeploymentGroupOutput 
  { "HooksNotCleanedUp'" :: NullOrUndefined (AutoScalingGroupList)
  }
derive instance newtypeDeleteDeploymentGroupOutput :: Newtype DeleteDeploymentGroupOutput _


-- | <p>Represents the input of a DeleteGitHubAccount operation.</p>
newtype DeleteGitHubAccountTokenInput = DeleteGitHubAccountTokenInput 
  { "TokenName'" :: NullOrUndefined (GitHubAccountTokenName)
  }
derive instance newtypeDeleteGitHubAccountTokenInput :: Newtype DeleteGitHubAccountTokenInput _


-- | <p>Represents the output of a DeleteGitHubAccountToken operation.</p>
newtype DeleteGitHubAccountTokenOutput = DeleteGitHubAccountTokenOutput 
  { "TokenName'" :: NullOrUndefined (GitHubAccountTokenName)
  }
derive instance newtypeDeleteGitHubAccountTokenOutput :: Newtype DeleteGitHubAccountTokenOutput _


-- | <p>The deployment is already complete.</p>
newtype DeploymentAlreadyCompletedException = DeploymentAlreadyCompletedException 
  { 
  }
derive instance newtypeDeploymentAlreadyCompletedException :: Newtype DeploymentAlreadyCompletedException _


-- | <p>A deployment configuration with the specified name already exists with the applicable IAM user or AWS account.</p>
newtype DeploymentConfigAlreadyExistsException = DeploymentConfigAlreadyExistsException 
  { 
  }
derive instance newtypeDeploymentConfigAlreadyExistsException :: Newtype DeploymentConfigAlreadyExistsException _


-- | <p>The deployment configuration does not exist with the applicable IAM user or AWS account.</p>
newtype DeploymentConfigDoesNotExistException = DeploymentConfigDoesNotExistException 
  { 
  }
derive instance newtypeDeploymentConfigDoesNotExistException :: Newtype DeploymentConfigDoesNotExistException _


newtype DeploymentConfigId = DeploymentConfigId String
derive instance newtypeDeploymentConfigId :: Newtype DeploymentConfigId _


-- | <p>The deployment configuration is still in use.</p>
newtype DeploymentConfigInUseException = DeploymentConfigInUseException 
  { 
  }
derive instance newtypeDeploymentConfigInUseException :: Newtype DeploymentConfigInUseException _


-- | <p>Information about a deployment configuration.</p>
newtype DeploymentConfigInfo = DeploymentConfigInfo 
  { "DeploymentConfigId'" :: NullOrUndefined (DeploymentConfigId)
  , "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName)
  , "MinimumHealthyHosts'" :: NullOrUndefined (MinimumHealthyHosts)
  , "CreateTime'" :: NullOrUndefined (Number)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  , "TrafficRoutingConfig'" :: NullOrUndefined (TrafficRoutingConfig)
  }
derive instance newtypeDeploymentConfigInfo :: Newtype DeploymentConfigInfo _


-- | <p>The deployment configurations limit was exceeded.</p>
newtype DeploymentConfigLimitExceededException = DeploymentConfigLimitExceededException 
  { 
  }
derive instance newtypeDeploymentConfigLimitExceededException :: Newtype DeploymentConfigLimitExceededException _


newtype DeploymentConfigName = DeploymentConfigName String
derive instance newtypeDeploymentConfigName :: Newtype DeploymentConfigName _


-- | <p>The deployment configuration name was not specified.</p>
newtype DeploymentConfigNameRequiredException = DeploymentConfigNameRequiredException 
  { 
  }
derive instance newtypeDeploymentConfigNameRequiredException :: Newtype DeploymentConfigNameRequiredException _


newtype DeploymentConfigsList = DeploymentConfigsList (Array DeploymentConfigName)
derive instance newtypeDeploymentConfigsList :: Newtype DeploymentConfigsList _


newtype DeploymentCreator = DeploymentCreator String
derive instance newtypeDeploymentCreator :: Newtype DeploymentCreator _


-- | <p>The deployment does not exist with the applicable IAM user or AWS account.</p>
newtype DeploymentDoesNotExistException = DeploymentDoesNotExistException 
  { 
  }
derive instance newtypeDeploymentDoesNotExistException :: Newtype DeploymentDoesNotExistException _


-- | <p>A deployment group with the specified name already exists with the applicable IAM user or AWS account.</p>
newtype DeploymentGroupAlreadyExistsException = DeploymentGroupAlreadyExistsException 
  { 
  }
derive instance newtypeDeploymentGroupAlreadyExistsException :: Newtype DeploymentGroupAlreadyExistsException _


-- | <p>The named deployment group does not exist with the applicable IAM user or AWS account.</p>
newtype DeploymentGroupDoesNotExistException = DeploymentGroupDoesNotExistException 
  { 
  }
derive instance newtypeDeploymentGroupDoesNotExistException :: Newtype DeploymentGroupDoesNotExistException _


newtype DeploymentGroupId = DeploymentGroupId String
derive instance newtypeDeploymentGroupId :: Newtype DeploymentGroupId _


-- | <p>Information about a deployment group.</p>
newtype DeploymentGroupInfo = DeploymentGroupInfo 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "DeploymentGroupId'" :: NullOrUndefined (DeploymentGroupId)
  , "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName)
  , "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName)
  , "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList)
  , "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList)
  , "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupList)
  , "ServiceRoleArn'" :: NullOrUndefined (Role)
  , "TargetRevision'" :: NullOrUndefined (RevisionLocation)
  , "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList)
  , "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration)
  , "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration)
  , "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle)
  , "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration)
  , "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo)
  , "LastSuccessfulDeployment'" :: NullOrUndefined (LastDeploymentInfo)
  , "LastAttemptedDeployment'" :: NullOrUndefined (LastDeploymentInfo)
  , "Ec2TagSet'" :: NullOrUndefined (EC2TagSet)
  , "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  }
derive instance newtypeDeploymentGroupInfo :: Newtype DeploymentGroupInfo _


newtype DeploymentGroupInfoList = DeploymentGroupInfoList (Array DeploymentGroupInfo)
derive instance newtypeDeploymentGroupInfoList :: Newtype DeploymentGroupInfoList _


-- | <p> The deployment groups limit was exceeded.</p>
newtype DeploymentGroupLimitExceededException = DeploymentGroupLimitExceededException 
  { 
  }
derive instance newtypeDeploymentGroupLimitExceededException :: Newtype DeploymentGroupLimitExceededException _


newtype DeploymentGroupName = DeploymentGroupName String
derive instance newtypeDeploymentGroupName :: Newtype DeploymentGroupName _


-- | <p>The deployment group name was not specified.</p>
newtype DeploymentGroupNameRequiredException = DeploymentGroupNameRequiredException 
  { 
  }
derive instance newtypeDeploymentGroupNameRequiredException :: Newtype DeploymentGroupNameRequiredException _


newtype DeploymentGroupsList = DeploymentGroupsList (Array DeploymentGroupName)
derive instance newtypeDeploymentGroupsList :: Newtype DeploymentGroupsList _


newtype DeploymentId = DeploymentId String
derive instance newtypeDeploymentId :: Newtype DeploymentId _


-- | <p>At least one deployment ID must be specified.</p>
newtype DeploymentIdRequiredException = DeploymentIdRequiredException 
  { 
  }
derive instance newtypeDeploymentIdRequiredException :: Newtype DeploymentIdRequiredException _


-- | <p>Information about a deployment.</p>
newtype DeploymentInfo = DeploymentInfo 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName)
  , "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName)
  , "DeploymentId'" :: NullOrUndefined (DeploymentId)
  , "PreviousRevision'" :: NullOrUndefined (RevisionLocation)
  , "Revision'" :: NullOrUndefined (RevisionLocation)
  , "Status'" :: NullOrUndefined (DeploymentStatus)
  , "ErrorInformation'" :: NullOrUndefined (ErrorInformation)
  , "CreateTime'" :: NullOrUndefined (Number)
  , "StartTime'" :: NullOrUndefined (Number)
  , "CompleteTime'" :: NullOrUndefined (Number)
  , "DeploymentOverview'" :: NullOrUndefined (DeploymentOverview)
  , "Description'" :: NullOrUndefined (Description)
  , "Creator'" :: NullOrUndefined (DeploymentCreator)
  , "IgnoreApplicationStopFailures'" :: NullOrUndefined (Boolean)
  , "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration)
  , "UpdateOutdatedInstancesOnly'" :: NullOrUndefined (Boolean)
  , "RollbackInfo'" :: NullOrUndefined (RollbackInfo)
  , "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle)
  , "TargetInstances'" :: NullOrUndefined (TargetInstances)
  , "InstanceTerminationWaitTimeStarted'" :: NullOrUndefined (Boolean)
  , "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration)
  , "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo)
  , "AdditionalDeploymentStatusInfo'" :: NullOrUndefined (AdditionalDeploymentStatusInfo)
  , "FileExistsBehavior'" :: NullOrUndefined (FileExistsBehavior)
  , "DeploymentStatusMessages'" :: NullOrUndefined (DeploymentStatusMessageList)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  }
derive instance newtypeDeploymentInfo :: Newtype DeploymentInfo _


-- | <p>The deployment does not have a status of Ready and can't continue yet.</p>
newtype DeploymentIsNotInReadyStateException = DeploymentIsNotInReadyStateException 
  { 
  }
derive instance newtypeDeploymentIsNotInReadyStateException :: Newtype DeploymentIsNotInReadyStateException _


-- | <p>The number of allowed deployments was exceeded.</p>
newtype DeploymentLimitExceededException = DeploymentLimitExceededException 
  { 
  }
derive instance newtypeDeploymentLimitExceededException :: Newtype DeploymentLimitExceededException _


-- | <p>The specified deployment has not started.</p>
newtype DeploymentNotStartedException = DeploymentNotStartedException 
  { 
  }
derive instance newtypeDeploymentNotStartedException :: Newtype DeploymentNotStartedException _


newtype DeploymentOption = DeploymentOption String
derive instance newtypeDeploymentOption :: Newtype DeploymentOption _


-- | <p>Information about the deployment status of the instances in the deployment.</p>
newtype DeploymentOverview = DeploymentOverview 
  { "Pending" :: NullOrUndefined (InstanceCount)
  , "InProgress" :: NullOrUndefined (InstanceCount)
  , "Succeeded" :: NullOrUndefined (InstanceCount)
  , "Failed" :: NullOrUndefined (InstanceCount)
  , "Skipped" :: NullOrUndefined (InstanceCount)
  , "Ready" :: NullOrUndefined (InstanceCount)
  }
derive instance newtypeDeploymentOverview :: Newtype DeploymentOverview _


newtype DeploymentReadyAction = DeploymentReadyAction String
derive instance newtypeDeploymentReadyAction :: Newtype DeploymentReadyAction _


-- | <p>Information about how traffic is rerouted to instances in a replacement environment in a blue/green deployment.</p>
newtype DeploymentReadyOption = DeploymentReadyOption 
  { "ActionOnTimeout'" :: NullOrUndefined (DeploymentReadyAction)
  , "WaitTimeInMinutes'" :: NullOrUndefined (Duration)
  }
derive instance newtypeDeploymentReadyOption :: Newtype DeploymentReadyOption _


newtype DeploymentStatus = DeploymentStatus String
derive instance newtypeDeploymentStatus :: Newtype DeploymentStatus _


newtype DeploymentStatusList = DeploymentStatusList (Array DeploymentStatus)
derive instance newtypeDeploymentStatusList :: Newtype DeploymentStatusList _


newtype DeploymentStatusMessageList = DeploymentStatusMessageList (Array ErrorMessage)
derive instance newtypeDeploymentStatusMessageList :: Newtype DeploymentStatusMessageList _


-- | <p>Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.</p>
newtype DeploymentStyle = DeploymentStyle 
  { "DeploymentType'" :: NullOrUndefined (DeploymentType)
  , "DeploymentOption'" :: NullOrUndefined (DeploymentOption)
  }
derive instance newtypeDeploymentStyle :: Newtype DeploymentStyle _


newtype DeploymentType = DeploymentType String
derive instance newtypeDeploymentType :: Newtype DeploymentType _


newtype DeploymentsInfoList = DeploymentsInfoList (Array DeploymentInfo)
derive instance newtypeDeploymentsInfoList :: Newtype DeploymentsInfoList _


newtype DeploymentsList = DeploymentsList (Array DeploymentId)
derive instance newtypeDeploymentsList :: Newtype DeploymentsList _


-- | <p>Represents the input of a DeregisterOnPremisesInstance operation.</p>
newtype DeregisterOnPremisesInstanceInput = DeregisterOnPremisesInstanceInput 
  { "InstanceName'" :: (InstanceName)
  }
derive instance newtypeDeregisterOnPremisesInstanceInput :: Newtype DeregisterOnPremisesInstanceInput _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


-- | <p>The description is too long.</p>
newtype DescriptionTooLongException = DescriptionTooLongException 
  { 
  }
derive instance newtypeDescriptionTooLongException :: Newtype DescriptionTooLongException _


-- | <p>Diagnostic information about executable scripts that are part of a deployment.</p>
newtype Diagnostics = Diagnostics 
  { "ErrorCode'" :: NullOrUndefined (LifecycleErrorCode)
  , "ScriptName'" :: NullOrUndefined (ScriptName)
  , "Message'" :: NullOrUndefined (LifecycleMessage)
  , "LogTail'" :: NullOrUndefined (LogTail)
  }
derive instance newtypeDiagnostics :: Newtype Diagnostics _


newtype Duration = Duration Int
derive instance newtypeDuration :: Newtype Duration _


-- | <p>Information about an EC2 tag filter.</p>
newtype EC2TagFilter = EC2TagFilter 
  { "Key" :: NullOrUndefined (Key)
  , "Value" :: NullOrUndefined (Value)
  , "Type" :: NullOrUndefined (EC2TagFilterType)
  }
derive instance newtypeEC2TagFilter :: Newtype EC2TagFilter _


newtype EC2TagFilterList = EC2TagFilterList (Array EC2TagFilter)
derive instance newtypeEC2TagFilterList :: Newtype EC2TagFilterList _


newtype EC2TagFilterType = EC2TagFilterType String
derive instance newtypeEC2TagFilterType :: Newtype EC2TagFilterType _


-- | <p>Information about groups of EC2 instance tags.</p>
newtype EC2TagSet = EC2TagSet 
  { "Ec2TagSetList'" :: NullOrUndefined (EC2TagSetList)
  }
derive instance newtypeEC2TagSet :: Newtype EC2TagSet _


newtype EC2TagSetList = EC2TagSetList (Array EC2TagFilterList)
derive instance newtypeEC2TagSetList :: Newtype EC2TagSetList _


-- | <p>Information about a load balancer in Elastic Load Balancing to use in a deployment. Instances are registered directly with a load balancer, and traffic is routed to the load balancer.</p>
newtype ELBInfo = ELBInfo 
  { "Name'" :: NullOrUndefined (ELBName)
  }
derive instance newtypeELBInfo :: Newtype ELBInfo _


newtype ELBInfoList = ELBInfoList (Array ELBInfo)
derive instance newtypeELBInfoList :: Newtype ELBInfoList _


newtype ELBName = ELBName String
derive instance newtypeELBName :: Newtype ELBName _


newtype ETag = ETag String
derive instance newtypeETag :: Newtype ETag _


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _


-- | <p>Information about a deployment error.</p>
newtype ErrorInformation = ErrorInformation 
  { "Code'" :: NullOrUndefined (ErrorCode)
  , "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeErrorInformation :: Newtype ErrorInformation _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype FileExistsBehavior = FileExistsBehavior String
derive instance newtypeFileExistsBehavior :: Newtype FileExistsBehavior _


-- | <p>Information about an application revision.</p>
newtype GenericRevisionInfo = GenericRevisionInfo 
  { "Description'" :: NullOrUndefined (Description)
  , "DeploymentGroups'" :: NullOrUndefined (DeploymentGroupsList)
  , "FirstUsedTime'" :: NullOrUndefined (Number)
  , "LastUsedTime'" :: NullOrUndefined (Number)
  , "RegisterTime'" :: NullOrUndefined (Number)
  }
derive instance newtypeGenericRevisionInfo :: Newtype GenericRevisionInfo _


-- | <p>Represents the input of a GetApplication operation.</p>
newtype GetApplicationInput = GetApplicationInput 
  { "ApplicationName'" :: (ApplicationName)
  }
derive instance newtypeGetApplicationInput :: Newtype GetApplicationInput _


-- | <p>Represents the output of a GetApplication operation.</p>
newtype GetApplicationOutput = GetApplicationOutput 
  { "Application'" :: NullOrUndefined (ApplicationInfo)
  }
derive instance newtypeGetApplicationOutput :: Newtype GetApplicationOutput _


-- | <p>Represents the input of a GetApplicationRevision operation.</p>
newtype GetApplicationRevisionInput = GetApplicationRevisionInput 
  { "ApplicationName'" :: (ApplicationName)
  , "Revision'" :: (RevisionLocation)
  }
derive instance newtypeGetApplicationRevisionInput :: Newtype GetApplicationRevisionInput _


-- | <p>Represents the output of a GetApplicationRevision operation.</p>
newtype GetApplicationRevisionOutput = GetApplicationRevisionOutput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "Revision'" :: NullOrUndefined (RevisionLocation)
  , "RevisionInfo'" :: NullOrUndefined (GenericRevisionInfo)
  }
derive instance newtypeGetApplicationRevisionOutput :: Newtype GetApplicationRevisionOutput _


-- | <p>Represents the input of a GetDeploymentConfig operation.</p>
newtype GetDeploymentConfigInput = GetDeploymentConfigInput 
  { "DeploymentConfigName'" :: (DeploymentConfigName)
  }
derive instance newtypeGetDeploymentConfigInput :: Newtype GetDeploymentConfigInput _


-- | <p>Represents the output of a GetDeploymentConfig operation.</p>
newtype GetDeploymentConfigOutput = GetDeploymentConfigOutput 
  { "DeploymentConfigInfo'" :: NullOrUndefined (DeploymentConfigInfo)
  }
derive instance newtypeGetDeploymentConfigOutput :: Newtype GetDeploymentConfigOutput _


-- | <p>Represents the input of a GetDeploymentGroup operation.</p>
newtype GetDeploymentGroupInput = GetDeploymentGroupInput 
  { "ApplicationName'" :: (ApplicationName)
  , "DeploymentGroupName'" :: (DeploymentGroupName)
  }
derive instance newtypeGetDeploymentGroupInput :: Newtype GetDeploymentGroupInput _


-- | <p>Represents the output of a GetDeploymentGroup operation.</p>
newtype GetDeploymentGroupOutput = GetDeploymentGroupOutput 
  { "DeploymentGroupInfo'" :: NullOrUndefined (DeploymentGroupInfo)
  }
derive instance newtypeGetDeploymentGroupOutput :: Newtype GetDeploymentGroupOutput _


-- | <p>Represents the input of a GetDeployment operation.</p>
newtype GetDeploymentInput = GetDeploymentInput 
  { "DeploymentId'" :: (DeploymentId)
  }
derive instance newtypeGetDeploymentInput :: Newtype GetDeploymentInput _


-- | <p>Represents the input of a GetDeploymentInstance operation.</p>
newtype GetDeploymentInstanceInput = GetDeploymentInstanceInput 
  { "DeploymentId'" :: (DeploymentId)
  , "InstanceId'" :: (InstanceId)
  }
derive instance newtypeGetDeploymentInstanceInput :: Newtype GetDeploymentInstanceInput _


-- | <p>Represents the output of a GetDeploymentInstance operation.</p>
newtype GetDeploymentInstanceOutput = GetDeploymentInstanceOutput 
  { "InstanceSummary'" :: NullOrUndefined (InstanceSummary)
  }
derive instance newtypeGetDeploymentInstanceOutput :: Newtype GetDeploymentInstanceOutput _


-- | <p>Represents the output of a GetDeployment operation.</p>
newtype GetDeploymentOutput = GetDeploymentOutput 
  { "DeploymentInfo'" :: NullOrUndefined (DeploymentInfo)
  }
derive instance newtypeGetDeploymentOutput :: Newtype GetDeploymentOutput _


-- | <p>Represents the input of a GetOnPremisesInstance operation.</p>
newtype GetOnPremisesInstanceInput = GetOnPremisesInstanceInput 
  { "InstanceName'" :: (InstanceName)
  }
derive instance newtypeGetOnPremisesInstanceInput :: Newtype GetOnPremisesInstanceInput _


-- | <p>Represents the output of a GetOnPremisesInstance operation.</p>
newtype GetOnPremisesInstanceOutput = GetOnPremisesInstanceOutput 
  { "InstanceInfo'" :: NullOrUndefined (InstanceInfo)
  }
derive instance newtypeGetOnPremisesInstanceOutput :: Newtype GetOnPremisesInstanceOutput _


-- | <p>No GitHub account connection exists with the named specified in the call.</p>
newtype GitHubAccountTokenDoesNotExistException = GitHubAccountTokenDoesNotExistException 
  { 
  }
derive instance newtypeGitHubAccountTokenDoesNotExistException :: Newtype GitHubAccountTokenDoesNotExistException _


newtype GitHubAccountTokenName = GitHubAccountTokenName String
derive instance newtypeGitHubAccountTokenName :: Newtype GitHubAccountTokenName _


newtype GitHubAccountTokenNameList = GitHubAccountTokenNameList (Array GitHubAccountTokenName)
derive instance newtypeGitHubAccountTokenNameList :: Newtype GitHubAccountTokenNameList _


-- | <p>The call is missing a required GitHub account connection name.</p>
newtype GitHubAccountTokenNameRequiredException = GitHubAccountTokenNameRequiredException 
  { 
  }
derive instance newtypeGitHubAccountTokenNameRequiredException :: Newtype GitHubAccountTokenNameRequiredException _


-- | <p>Information about the location of application artifacts stored in GitHub.</p>
newtype GitHubLocation = GitHubLocation 
  { "Repository'" :: NullOrUndefined (Repository)
  , "CommitId'" :: NullOrUndefined (CommitId)
  }
derive instance newtypeGitHubLocation :: Newtype GitHubLocation _


newtype GreenFleetProvisioningAction = GreenFleetProvisioningAction String
derive instance newtypeGreenFleetProvisioningAction :: Newtype GreenFleetProvisioningAction _


-- | <p>Information about the instances that belong to the replacement environment in a blue/green deployment.</p>
newtype GreenFleetProvisioningOption = GreenFleetProvisioningOption 
  { "Action'" :: NullOrUndefined (GreenFleetProvisioningAction)
  }
derive instance newtypeGreenFleetProvisioningOption :: Newtype GreenFleetProvisioningOption _


-- | <p>No IAM ARN was included in the request. You must use an IAM session ARN or IAM user ARN in the request.</p>
newtype IamArnRequiredException = IamArnRequiredException 
  { 
  }
derive instance newtypeIamArnRequiredException :: Newtype IamArnRequiredException _


newtype IamSessionArn = IamSessionArn String
derive instance newtypeIamSessionArn :: Newtype IamSessionArn _


-- | <p>The request included an IAM session ARN that has already been used to register a different instance.</p>
newtype IamSessionArnAlreadyRegisteredException = IamSessionArnAlreadyRegisteredException 
  { 
  }
derive instance newtypeIamSessionArnAlreadyRegisteredException :: Newtype IamSessionArnAlreadyRegisteredException _


newtype IamUserArn = IamUserArn String
derive instance newtypeIamUserArn :: Newtype IamUserArn _


-- | <p>The specified IAM user ARN is already registered with an on-premises instance.</p>
newtype IamUserArnAlreadyRegisteredException = IamUserArnAlreadyRegisteredException 
  { 
  }
derive instance newtypeIamUserArnAlreadyRegisteredException :: Newtype IamUserArnAlreadyRegisteredException _


-- | <p>An IAM user ARN was not specified.</p>
newtype IamUserArnRequiredException = IamUserArnRequiredException 
  { 
  }
derive instance newtypeIamUserArnRequiredException :: Newtype IamUserArnRequiredException _


newtype InstanceAction = InstanceAction String
derive instance newtypeInstanceAction :: Newtype InstanceAction _


newtype InstanceArn = InstanceArn String
derive instance newtypeInstanceArn :: Newtype InstanceArn _


newtype InstanceCount = InstanceCount Number
derive instance newtypeInstanceCount :: Newtype InstanceCount _


-- | <p>The specified instance does not exist in the deployment group.</p>
newtype InstanceDoesNotExistException = InstanceDoesNotExistException 
  { 
  }
derive instance newtypeInstanceDoesNotExistException :: Newtype InstanceDoesNotExistException _


newtype InstanceId = InstanceId String
derive instance newtypeInstanceId :: Newtype InstanceId _


-- | <p>The instance ID was not specified.</p>
newtype InstanceIdRequiredException = InstanceIdRequiredException 
  { 
  }
derive instance newtypeInstanceIdRequiredException :: Newtype InstanceIdRequiredException _


-- | <p>Information about an on-premises instance.</p>
newtype InstanceInfo = InstanceInfo 
  { "InstanceName'" :: NullOrUndefined (InstanceName)
  , "IamSessionArn'" :: NullOrUndefined (IamSessionArn)
  , "IamUserArn'" :: NullOrUndefined (IamUserArn)
  , "InstanceArn'" :: NullOrUndefined (InstanceArn)
  , "RegisterTime'" :: NullOrUndefined (Number)
  , "DeregisterTime'" :: NullOrUndefined (Number)
  , "Tags'" :: NullOrUndefined (TagList)
  }
derive instance newtypeInstanceInfo :: Newtype InstanceInfo _


newtype InstanceInfoList = InstanceInfoList (Array InstanceInfo)
derive instance newtypeInstanceInfoList :: Newtype InstanceInfoList _


-- | <p>The maximum number of allowed on-premises instances in a single call was exceeded.</p>
newtype InstanceLimitExceededException = InstanceLimitExceededException 
  { 
  }
derive instance newtypeInstanceLimitExceededException :: Newtype InstanceLimitExceededException _


newtype InstanceName = InstanceName String
derive instance newtypeInstanceName :: Newtype InstanceName _


-- | <p>The specified on-premises instance name is already registered.</p>
newtype InstanceNameAlreadyRegisteredException = InstanceNameAlreadyRegisteredException 
  { 
  }
derive instance newtypeInstanceNameAlreadyRegisteredException :: Newtype InstanceNameAlreadyRegisteredException _


newtype InstanceNameList = InstanceNameList (Array InstanceName)
derive instance newtypeInstanceNameList :: Newtype InstanceNameList _


-- | <p>An on-premises instance name was not specified.</p>
newtype InstanceNameRequiredException = InstanceNameRequiredException 
  { 
  }
derive instance newtypeInstanceNameRequiredException :: Newtype InstanceNameRequiredException _


-- | <p>The specified on-premises instance is not registered.</p>
newtype InstanceNotRegisteredException = InstanceNotRegisteredException 
  { 
  }
derive instance newtypeInstanceNotRegisteredException :: Newtype InstanceNotRegisteredException _


newtype InstanceStatus = InstanceStatus String
derive instance newtypeInstanceStatus :: Newtype InstanceStatus _


newtype InstanceStatusList = InstanceStatusList (Array InstanceStatus)
derive instance newtypeInstanceStatusList :: Newtype InstanceStatusList _


-- | <p>Information about an instance in a deployment.</p>
newtype InstanceSummary = InstanceSummary 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  , "InstanceId'" :: NullOrUndefined (InstanceId)
  , "Status'" :: NullOrUndefined (InstanceStatus)
  , "LastUpdatedAt'" :: NullOrUndefined (Number)
  , "LifecycleEvents'" :: NullOrUndefined (LifecycleEventList)
  , "InstanceType'" :: NullOrUndefined (InstanceType)
  }
derive instance newtypeInstanceSummary :: Newtype InstanceSummary _


newtype InstanceSummaryList = InstanceSummaryList (Array InstanceSummary)
derive instance newtypeInstanceSummaryList :: Newtype InstanceSummaryList _


newtype InstanceType = InstanceType String
derive instance newtypeInstanceType :: Newtype InstanceType _


newtype InstanceTypeList = InstanceTypeList (Array InstanceType)
derive instance newtypeInstanceTypeList :: Newtype InstanceTypeList _


newtype InstancesList = InstancesList (Array InstanceId)
derive instance newtypeInstancesList :: Newtype InstancesList _


-- | <p>The format of the alarm configuration is invalid. Possible causes include:</p> <ul> <li> <p>The alarm list is null.</p> </li> <li> <p>The alarm object is null.</p> </li> <li> <p>The alarm name is empty or null or exceeds the 255 character limit.</p> </li> <li> <p>Two alarms with the same name have been specified.</p> </li> <li> <p>The alarm configuration is enabled but the alarm list is empty.</p> </li> </ul>
newtype InvalidAlarmConfigException = InvalidAlarmConfigException 
  { 
  }
derive instance newtypeInvalidAlarmConfigException :: Newtype InvalidAlarmConfigException _


-- | <p>The application name was specified in an invalid format.</p>
newtype InvalidApplicationNameException = InvalidApplicationNameException 
  { 
  }
derive instance newtypeInvalidApplicationNameException :: Newtype InvalidApplicationNameException _


-- | <p>The automatic rollback configuration was specified in an invalid format. For example, automatic rollback is enabled but an invalid triggering event type or no event types were listed.</p>
newtype InvalidAutoRollbackConfigException = InvalidAutoRollbackConfigException 
  { 
  }
derive instance newtypeInvalidAutoRollbackConfigException :: Newtype InvalidAutoRollbackConfigException _


-- | <p>The Auto Scaling group was specified in an invalid format or does not exist.</p>
newtype InvalidAutoScalingGroupException = InvalidAutoScalingGroupException 
  { 
  }
derive instance newtypeInvalidAutoScalingGroupException :: Newtype InvalidAutoScalingGroupException _


-- | <p>The configuration for the blue/green deployment group was provided in an invalid format. For information about deployment configuration format, see <a>CreateDeploymentConfig</a>.</p>
newtype InvalidBlueGreenDeploymentConfigurationException = InvalidBlueGreenDeploymentConfigurationException 
  { 
  }
derive instance newtypeInvalidBlueGreenDeploymentConfigurationException :: Newtype InvalidBlueGreenDeploymentConfigurationException _


-- | <p>The bucket name either doesn't exist or was specified in an invalid format.</p>
newtype InvalidBucketNameFilterException = InvalidBucketNameFilterException 
  { 
  }
derive instance newtypeInvalidBucketNameFilterException :: Newtype InvalidBucketNameFilterException _


-- | <p>The computePlatform is invalid. The computePlatform should be <code>Lambda</code> or <code>Server</code>.</p>
newtype InvalidComputePlatformException = InvalidComputePlatformException 
  { 
  }
derive instance newtypeInvalidComputePlatformException :: Newtype InvalidComputePlatformException _


-- | <p>The deployed state filter was specified in an invalid format.</p>
newtype InvalidDeployedStateFilterException = InvalidDeployedStateFilterException 
  { 
  }
derive instance newtypeInvalidDeployedStateFilterException :: Newtype InvalidDeployedStateFilterException _


-- | <p>The deployment configuration name was specified in an invalid format.</p>
newtype InvalidDeploymentConfigNameException = InvalidDeploymentConfigNameException 
  { 
  }
derive instance newtypeInvalidDeploymentConfigNameException :: Newtype InvalidDeploymentConfigNameException _


-- | <p>The deployment group name was specified in an invalid format.</p>
newtype InvalidDeploymentGroupNameException = InvalidDeploymentGroupNameException 
  { 
  }
derive instance newtypeInvalidDeploymentGroupNameException :: Newtype InvalidDeploymentGroupNameException _


-- | <p>At least one of the deployment IDs was specified in an invalid format.</p>
newtype InvalidDeploymentIdException = InvalidDeploymentIdException 
  { 
  }
derive instance newtypeInvalidDeploymentIdException :: Newtype InvalidDeploymentIdException _


-- | <p>An instance type was specified for an in-place deployment. Instance types are supported for blue/green deployments only.</p>
newtype InvalidDeploymentInstanceTypeException = InvalidDeploymentInstanceTypeException 
  { 
  }
derive instance newtypeInvalidDeploymentInstanceTypeException :: Newtype InvalidDeploymentInstanceTypeException _


-- | <p>The specified deployment status doesn't exist or cannot be determined.</p>
newtype InvalidDeploymentStatusException = InvalidDeploymentStatusException 
  { 
  }
derive instance newtypeInvalidDeploymentStatusException :: Newtype InvalidDeploymentStatusException _


-- | <p>An invalid deployment style was specified. Valid deployment types include "IN_PLACE" and "BLUE_GREEN". Valid deployment options include "WITH_TRAFFIC_CONTROL" and "WITHOUT_TRAFFIC_CONTROL".</p>
newtype InvalidDeploymentStyleException = InvalidDeploymentStyleException 
  { 
  }
derive instance newtypeInvalidDeploymentStyleException :: Newtype InvalidDeploymentStyleException _


-- | <p>A call was submitted that specified both Ec2TagFilters and Ec2TagSet, but only one of these data types can be used in a single call.</p>
newtype InvalidEC2TagCombinationException = InvalidEC2TagCombinationException 
  { 
  }
derive instance newtypeInvalidEC2TagCombinationException :: Newtype InvalidEC2TagCombinationException _


-- | <p>The tag was specified in an invalid format.</p>
newtype InvalidEC2TagException = InvalidEC2TagException 
  { 
  }
derive instance newtypeInvalidEC2TagException :: Newtype InvalidEC2TagException _


-- | <p>An invalid fileExistsBehavior option was specified to determine how AWS CodeDeploy handles files or directories that already exist in a deployment target location but weren't part of the previous successful deployment. Valid values include "DISALLOW", "OVERWRITE", and "RETAIN".</p>
newtype InvalidFileExistsBehaviorException = InvalidFileExistsBehaviorException 
  { 
  }
derive instance newtypeInvalidFileExistsBehaviorException :: Newtype InvalidFileExistsBehaviorException _


-- | <p>The format of the specified GitHub account connection name is invalid.</p>
newtype InvalidGitHubAccountTokenNameException = InvalidGitHubAccountTokenNameException 
  { 
  }
derive instance newtypeInvalidGitHubAccountTokenNameException :: Newtype InvalidGitHubAccountTokenNameException _


-- | <p>The IAM session ARN was specified in an invalid format.</p>
newtype InvalidIamSessionArnException = InvalidIamSessionArnException 
  { 
  }
derive instance newtypeInvalidIamSessionArnException :: Newtype InvalidIamSessionArnException _


-- | <p>The IAM user ARN was specified in an invalid format.</p>
newtype InvalidIamUserArnException = InvalidIamUserArnException 
  { 
  }
derive instance newtypeInvalidIamUserArnException :: Newtype InvalidIamUserArnException _


-- | <p>The IgnoreApplicationStopFailures value is invalid. For AWS Lambda deployments, <code>false</code> is expected. For EC2/On-premises deployments, <code>true</code> or <code>false</code> is expected.</p>
newtype InvalidIgnoreApplicationStopFailuresValueException = InvalidIgnoreApplicationStopFailuresValueException 
  { 
  }
derive instance newtypeInvalidIgnoreApplicationStopFailuresValueException :: Newtype InvalidIgnoreApplicationStopFailuresValueException _


-- | <p>The specified input was specified in an invalid format.</p>
newtype InvalidInputException = InvalidInputException 
  { 
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _


-- | <p> </p>
newtype InvalidInstanceIdException = InvalidInstanceIdException 
  { 
  }
derive instance newtypeInvalidInstanceIdException :: Newtype InvalidInstanceIdException _


-- | <p>The specified on-premises instance name was specified in an invalid format.</p>
newtype InvalidInstanceNameException = InvalidInstanceNameException 
  { 
  }
derive instance newtypeInvalidInstanceNameException :: Newtype InvalidInstanceNameException _


-- | <p>The specified instance status does not exist.</p>
newtype InvalidInstanceStatusException = InvalidInstanceStatusException 
  { 
  }
derive instance newtypeInvalidInstanceStatusException :: Newtype InvalidInstanceStatusException _


-- | <p>An invalid instance type was specified for instances in a blue/green deployment. Valid values include "Blue" for an original environment and "Green" for a replacement environment.</p>
newtype InvalidInstanceTypeException = InvalidInstanceTypeException 
  { 
  }
derive instance newtypeInvalidInstanceTypeException :: Newtype InvalidInstanceTypeException _


-- | <p>The specified key prefix filter was specified in an invalid format.</p>
newtype InvalidKeyPrefixFilterException = InvalidKeyPrefixFilterException 
  { 
  }
derive instance newtypeInvalidKeyPrefixFilterException :: Newtype InvalidKeyPrefixFilterException _


-- | <p>A lifecycle event hook is invalid. Review the <code>hooks</code> section in your AppSpec file to ensure the lifecycle events and <code>hooks</code> functions are valid.</p>
newtype InvalidLifecycleEventHookExecutionIdException = InvalidLifecycleEventHookExecutionIdException 
  { 
  }
derive instance newtypeInvalidLifecycleEventHookExecutionIdException :: Newtype InvalidLifecycleEventHookExecutionIdException _


-- | <p>The result of a Lambda validation function that verifies a lifecycle event is invalid. It should return <code>Succeeded</code> or <code>Failed</code>.</p>
newtype InvalidLifecycleEventHookExecutionStatusException = InvalidLifecycleEventHookExecutionStatusException 
  { 
  }
derive instance newtypeInvalidLifecycleEventHookExecutionStatusException :: Newtype InvalidLifecycleEventHookExecutionStatusException _


-- | <p>An invalid load balancer name, or no load balancer name, was specified.</p>
newtype InvalidLoadBalancerInfoException = InvalidLoadBalancerInfoException 
  { 
  }
derive instance newtypeInvalidLoadBalancerInfoException :: Newtype InvalidLoadBalancerInfoException _


-- | <p>The minimum healthy instance value was specified in an invalid format.</p>
newtype InvalidMinimumHealthyHostValueException = InvalidMinimumHealthyHostValueException 
  { 
  }
derive instance newtypeInvalidMinimumHealthyHostValueException :: Newtype InvalidMinimumHealthyHostValueException _


-- | <p>The next token was specified in an invalid format.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { 
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>A call was submitted that specified both OnPremisesTagFilters and OnPremisesTagSet, but only one of these data types can be used in a single call.</p>
newtype InvalidOnPremisesTagCombinationException = InvalidOnPremisesTagCombinationException 
  { 
  }
derive instance newtypeInvalidOnPremisesTagCombinationException :: Newtype InvalidOnPremisesTagCombinationException _


-- | <p>An invalid operation was detected.</p>
newtype InvalidOperationException = InvalidOperationException 
  { 
  }
derive instance newtypeInvalidOperationException :: Newtype InvalidOperationException _


-- | <p>The registration status was specified in an invalid format.</p>
newtype InvalidRegistrationStatusException = InvalidRegistrationStatusException 
  { 
  }
derive instance newtypeInvalidRegistrationStatusException :: Newtype InvalidRegistrationStatusException _


-- | <p>The revision was specified in an invalid format.</p>
newtype InvalidRevisionException = InvalidRevisionException 
  { 
  }
derive instance newtypeInvalidRevisionException :: Newtype InvalidRevisionException _


-- | <p>The service role ARN was specified in an invalid format. Or, if an Auto Scaling group was specified, the specified service role does not grant the appropriate permissions to Auto Scaling.</p>
newtype InvalidRoleException = InvalidRoleException 
  { 
  }
derive instance newtypeInvalidRoleException :: Newtype InvalidRoleException _


-- | <p>The column name to sort by is either not present or was specified in an invalid format.</p>
newtype InvalidSortByException = InvalidSortByException 
  { 
  }
derive instance newtypeInvalidSortByException :: Newtype InvalidSortByException _


-- | <p>The sort order was specified in an invalid format.</p>
newtype InvalidSortOrderException = InvalidSortOrderException 
  { 
  }
derive instance newtypeInvalidSortOrderException :: Newtype InvalidSortOrderException _


-- | <p>The specified tag was specified in an invalid format.</p>
newtype InvalidTagException = InvalidTagException 
  { 
  }
derive instance newtypeInvalidTagException :: Newtype InvalidTagException _


-- | <p>The specified tag filter was specified in an invalid format.</p>
newtype InvalidTagFilterException = InvalidTagFilterException 
  { 
  }
derive instance newtypeInvalidTagFilterException :: Newtype InvalidTagFilterException _


-- | <p>The target instance configuration is invalid. Possible causes include:</p> <ul> <li> <p>Configuration data for target instances was entered for an in-place deployment.</p> </li> <li> <p>The limit of 10 tags for a tag type was exceeded.</p> </li> <li> <p>The combined length of the tag names exceeded the limit. </p> </li> <li> <p>A specified tag is not currently applied to any instances.</p> </li> </ul>
newtype InvalidTargetInstancesException = InvalidTargetInstancesException 
  { 
  }
derive instance newtypeInvalidTargetInstancesException :: Newtype InvalidTargetInstancesException _


-- | <p>The specified time range was specified in an invalid format.</p>
newtype InvalidTimeRangeException = InvalidTimeRangeException 
  { 
  }
derive instance newtypeInvalidTimeRangeException :: Newtype InvalidTimeRangeException _


-- | <p> The configuration that specifies how traffic is routed during a deployment is invalid.</p>
newtype InvalidTrafficRoutingConfigurationException = InvalidTrafficRoutingConfigurationException 
  { 
  }
derive instance newtypeInvalidTrafficRoutingConfigurationException :: Newtype InvalidTrafficRoutingConfigurationException _


-- | <p>The trigger was specified in an invalid format.</p>
newtype InvalidTriggerConfigException = InvalidTriggerConfigException 
  { 
  }
derive instance newtypeInvalidTriggerConfigException :: Newtype InvalidTriggerConfigException _


-- | <p>The UpdateOutdatedInstancesOnly value is invalid. For AWS Lambda deployments, <code>false</code> is expected. For EC2/On-premises deployments, <code>true</code> or <code>false</code> is expected.</p>
newtype InvalidUpdateOutdatedInstancesOnlyValueException = InvalidUpdateOutdatedInstancesOnlyValueException 
  { 
  }
derive instance newtypeInvalidUpdateOutdatedInstancesOnlyValueException :: Newtype InvalidUpdateOutdatedInstancesOnlyValueException _


newtype Key = Key String
derive instance newtypeKey :: Newtype Key _


-- | <p>Information about the most recent attempted or successful deployment to a deployment group.</p>
newtype LastDeploymentInfo = LastDeploymentInfo 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  , "Status'" :: NullOrUndefined (DeploymentStatus)
  , "EndTime'" :: NullOrUndefined (Number)
  , "CreateTime'" :: NullOrUndefined (Number)
  }
derive instance newtypeLastDeploymentInfo :: Newtype LastDeploymentInfo _


newtype LifecycleErrorCode = LifecycleErrorCode String
derive instance newtypeLifecycleErrorCode :: Newtype LifecycleErrorCode _


-- | <p>Information about a deployment lifecycle event.</p>
newtype LifecycleEvent = LifecycleEvent 
  { "LifecycleEventName'" :: NullOrUndefined (LifecycleEventName)
  , "Diagnostics'" :: NullOrUndefined (Diagnostics)
  , "StartTime'" :: NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined (Number)
  , "Status'" :: NullOrUndefined (LifecycleEventStatus)
  }
derive instance newtypeLifecycleEvent :: Newtype LifecycleEvent _


-- | <p>An attempt to return the status of an already completed lifecycle event occurred.</p>
newtype LifecycleEventAlreadyCompletedException = LifecycleEventAlreadyCompletedException 
  { 
  }
derive instance newtypeLifecycleEventAlreadyCompletedException :: Newtype LifecycleEventAlreadyCompletedException _


newtype LifecycleEventHookExecutionId = LifecycleEventHookExecutionId String
derive instance newtypeLifecycleEventHookExecutionId :: Newtype LifecycleEventHookExecutionId _


newtype LifecycleEventList = LifecycleEventList (Array LifecycleEvent)
derive instance newtypeLifecycleEventList :: Newtype LifecycleEventList _


newtype LifecycleEventName = LifecycleEventName String
derive instance newtypeLifecycleEventName :: Newtype LifecycleEventName _


newtype LifecycleEventStatus = LifecycleEventStatus String
derive instance newtypeLifecycleEventStatus :: Newtype LifecycleEventStatus _


-- | <p>The limit for lifecycle hooks was exceeded.</p>
newtype LifecycleHookLimitExceededException = LifecycleHookLimitExceededException 
  { 
  }
derive instance newtypeLifecycleHookLimitExceededException :: Newtype LifecycleHookLimitExceededException _


newtype LifecycleMessage = LifecycleMessage String
derive instance newtypeLifecycleMessage :: Newtype LifecycleMessage _


-- | <p>Represents the input of a ListApplicationRevisions operation.</p>
newtype ListApplicationRevisionsInput = ListApplicationRevisionsInput 
  { "ApplicationName'" :: (ApplicationName)
  , "SortBy'" :: NullOrUndefined (ApplicationRevisionSortBy)
  , "SortOrder'" :: NullOrUndefined (SortOrder)
  , "S3Bucket'" :: NullOrUndefined (S3Bucket)
  , "S3KeyPrefix'" :: NullOrUndefined (S3Key)
  , "Deployed'" :: NullOrUndefined (ListStateFilterAction)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListApplicationRevisionsInput :: Newtype ListApplicationRevisionsInput _


-- | <p>Represents the output of a ListApplicationRevisions operation.</p>
newtype ListApplicationRevisionsOutput = ListApplicationRevisionsOutput 
  { "Revisions'" :: NullOrUndefined (RevisionLocationList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListApplicationRevisionsOutput :: Newtype ListApplicationRevisionsOutput _


-- | <p>Represents the input of a ListApplications operation.</p>
newtype ListApplicationsInput = ListApplicationsInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListApplicationsInput :: Newtype ListApplicationsInput _


-- | <p>Represents the output of a ListApplications operation.</p>
newtype ListApplicationsOutput = ListApplicationsOutput 
  { "Applications'" :: NullOrUndefined (ApplicationsList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListApplicationsOutput :: Newtype ListApplicationsOutput _


-- | <p>Represents the input of a ListDeploymentConfigs operation.</p>
newtype ListDeploymentConfigsInput = ListDeploymentConfigsInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDeploymentConfigsInput :: Newtype ListDeploymentConfigsInput _


-- | <p>Represents the output of a ListDeploymentConfigs operation.</p>
newtype ListDeploymentConfigsOutput = ListDeploymentConfigsOutput 
  { "DeploymentConfigsList'" :: NullOrUndefined (DeploymentConfigsList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDeploymentConfigsOutput :: Newtype ListDeploymentConfigsOutput _


-- | <p>Represents the input of a ListDeploymentGroups operation.</p>
newtype ListDeploymentGroupsInput = ListDeploymentGroupsInput 
  { "ApplicationName'" :: (ApplicationName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDeploymentGroupsInput :: Newtype ListDeploymentGroupsInput _


-- | <p>Represents the output of a ListDeploymentGroups operation.</p>
newtype ListDeploymentGroupsOutput = ListDeploymentGroupsOutput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "DeploymentGroups'" :: NullOrUndefined (DeploymentGroupsList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDeploymentGroupsOutput :: Newtype ListDeploymentGroupsOutput _


-- | <p>Represents the input of a ListDeploymentInstances operation.</p>
newtype ListDeploymentInstancesInput = ListDeploymentInstancesInput 
  { "DeploymentId'" :: (DeploymentId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "InstanceStatusFilter'" :: NullOrUndefined (InstanceStatusList)
  , "InstanceTypeFilter'" :: NullOrUndefined (InstanceTypeList)
  }
derive instance newtypeListDeploymentInstancesInput :: Newtype ListDeploymentInstancesInput _


-- | <p>Represents the output of a ListDeploymentInstances operation.</p>
newtype ListDeploymentInstancesOutput = ListDeploymentInstancesOutput 
  { "InstancesList'" :: NullOrUndefined (InstancesList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDeploymentInstancesOutput :: Newtype ListDeploymentInstancesOutput _


-- | <p>Represents the input of a ListDeployments operation.</p>
newtype ListDeploymentsInput = ListDeploymentsInput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName)
  , "IncludeOnlyStatuses'" :: NullOrUndefined (DeploymentStatusList)
  , "CreateTimeRange'" :: NullOrUndefined (TimeRange)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDeploymentsInput :: Newtype ListDeploymentsInput _


-- | <p>Represents the output of a ListDeployments operation.</p>
newtype ListDeploymentsOutput = ListDeploymentsOutput 
  { "Deployments'" :: NullOrUndefined (DeploymentsList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListDeploymentsOutput :: Newtype ListDeploymentsOutput _


-- | <p>Represents the input of a ListGitHubAccountTokenNames operation.</p>
newtype ListGitHubAccountTokenNamesInput = ListGitHubAccountTokenNamesInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListGitHubAccountTokenNamesInput :: Newtype ListGitHubAccountTokenNamesInput _


-- | <p>Represents the output of a ListGitHubAccountTokenNames operation.</p>
newtype ListGitHubAccountTokenNamesOutput = ListGitHubAccountTokenNamesOutput 
  { "TokenNameList'" :: NullOrUndefined (GitHubAccountTokenNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListGitHubAccountTokenNamesOutput :: Newtype ListGitHubAccountTokenNamesOutput _


-- | <p>Represents the input of a ListOnPremisesInstances operation.</p>
newtype ListOnPremisesInstancesInput = ListOnPremisesInstancesInput 
  { "RegistrationStatus'" :: NullOrUndefined (RegistrationStatus)
  , "TagFilters'" :: NullOrUndefined (TagFilterList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListOnPremisesInstancesInput :: Newtype ListOnPremisesInstancesInput _


-- | <p>Represents the output of list on-premises instances operation.</p>
newtype ListOnPremisesInstancesOutput = ListOnPremisesInstancesOutput 
  { "InstanceNames'" :: NullOrUndefined (InstanceNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListOnPremisesInstancesOutput :: Newtype ListOnPremisesInstancesOutput _


newtype ListStateFilterAction = ListStateFilterAction String
derive instance newtypeListStateFilterAction :: Newtype ListStateFilterAction _


-- | <p>Information about the Elastic Load Balancing load balancer or target group used in a deployment.</p>
newtype LoadBalancerInfo = LoadBalancerInfo 
  { "ElbInfoList'" :: NullOrUndefined (ELBInfoList)
  , "TargetGroupInfoList'" :: NullOrUndefined (TargetGroupInfoList)
  }
derive instance newtypeLoadBalancerInfo :: Newtype LoadBalancerInfo _


newtype LogTail = LogTail String
derive instance newtypeLogTail :: Newtype LogTail _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


-- | <p>Information about minimum healthy instance.</p>
newtype MinimumHealthyHosts = MinimumHealthyHosts 
  { "Value'" :: NullOrUndefined (MinimumHealthyHostsValue)
  , "Type'" :: NullOrUndefined (MinimumHealthyHostsType)
  }
derive instance newtypeMinimumHealthyHosts :: Newtype MinimumHealthyHosts _


newtype MinimumHealthyHostsType = MinimumHealthyHostsType String
derive instance newtypeMinimumHealthyHostsType :: Newtype MinimumHealthyHostsType _


newtype MinimumHealthyHostsValue = MinimumHealthyHostsValue Int
derive instance newtypeMinimumHealthyHostsValue :: Newtype MinimumHealthyHostsValue _


-- | <p>Both an IAM user ARN and an IAM session ARN were included in the request. Use only one ARN type.</p>
newtype MultipleIamArnsProvidedException = MultipleIamArnsProvidedException 
  { 
  }
derive instance newtypeMultipleIamArnsProvidedException :: Newtype MultipleIamArnsProvidedException _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype NullableBoolean = NullableBoolean Boolean
derive instance newtypeNullableBoolean :: Newtype NullableBoolean _


-- | <p>Information about groups of on-premises instance tags.</p>
newtype OnPremisesTagSet = OnPremisesTagSet 
  { "OnPremisesTagSetList'" :: NullOrUndefined (OnPremisesTagSetList)
  }
derive instance newtypeOnPremisesTagSet :: Newtype OnPremisesTagSet _


newtype OnPremisesTagSetList = OnPremisesTagSetList (Array TagFilterList)
derive instance newtypeOnPremisesTagSetList :: Newtype OnPremisesTagSetList _


-- | <p>The API used does not support the deployment.</p>
newtype OperationNotSupportedException = OperationNotSupportedException 
  { 
  }
derive instance newtypeOperationNotSupportedException :: Newtype OperationNotSupportedException _


newtype Percentage = Percentage Int
derive instance newtypePercentage :: Newtype Percentage _


newtype PutLifecycleEventHookExecutionStatusInput = PutLifecycleEventHookExecutionStatusInput 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  , "LifecycleEventHookExecutionId'" :: NullOrUndefined (LifecycleEventHookExecutionId)
  , "Status'" :: NullOrUndefined (LifecycleEventStatus)
  }
derive instance newtypePutLifecycleEventHookExecutionStatusInput :: Newtype PutLifecycleEventHookExecutionStatusInput _


newtype PutLifecycleEventHookExecutionStatusOutput = PutLifecycleEventHookExecutionStatusOutput 
  { "LifecycleEventHookExecutionId'" :: NullOrUndefined (LifecycleEventHookExecutionId)
  }
derive instance newtypePutLifecycleEventHookExecutionStatusOutput :: Newtype PutLifecycleEventHookExecutionStatusOutput _


-- | <p>A revision for an AWS Lambda deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda deployments, the revision is the same as the AppSpec file.</p>
newtype RawString = RawString 
  { "Content'" :: NullOrUndefined (RawStringContent)
  , "Sha256'" :: NullOrUndefined (RawStringSha256)
  }
derive instance newtypeRawString :: Newtype RawString _


newtype RawStringContent = RawStringContent String
derive instance newtypeRawStringContent :: Newtype RawStringContent _


newtype RawStringSha256 = RawStringSha256 String
derive instance newtypeRawStringSha256 :: Newtype RawStringSha256 _


-- | <p>Represents the input of a RegisterApplicationRevision operation.</p>
newtype RegisterApplicationRevisionInput = RegisterApplicationRevisionInput 
  { "ApplicationName'" :: (ApplicationName)
  , "Description'" :: NullOrUndefined (Description)
  , "Revision'" :: (RevisionLocation)
  }
derive instance newtypeRegisterApplicationRevisionInput :: Newtype RegisterApplicationRevisionInput _


-- | <p>Represents the input of the register on-premises instance operation.</p>
newtype RegisterOnPremisesInstanceInput = RegisterOnPremisesInstanceInput 
  { "InstanceName'" :: (InstanceName)
  , "IamSessionArn'" :: NullOrUndefined (IamSessionArn)
  , "IamUserArn'" :: NullOrUndefined (IamUserArn)
  }
derive instance newtypeRegisterOnPremisesInstanceInput :: Newtype RegisterOnPremisesInstanceInput _


newtype RegistrationStatus = RegistrationStatus String
derive instance newtypeRegistrationStatus :: Newtype RegistrationStatus _


-- | <p>Represents the input of a RemoveTagsFromOnPremisesInstances operation.</p>
newtype RemoveTagsFromOnPremisesInstancesInput = RemoveTagsFromOnPremisesInstancesInput 
  { "Tags'" :: (TagList)
  , "InstanceNames'" :: (InstanceNameList)
  }
derive instance newtypeRemoveTagsFromOnPremisesInstancesInput :: Newtype RemoveTagsFromOnPremisesInstancesInput _


newtype Repository = Repository String
derive instance newtypeRepository :: Newtype Repository _


-- | <p>The specified resource could not be validated.</p>
newtype ResourceValidationException = ResourceValidationException 
  { 
  }
derive instance newtypeResourceValidationException :: Newtype ResourceValidationException _


-- | <p>The named revision does not exist with the applicable IAM user or AWS account.</p>
newtype RevisionDoesNotExistException = RevisionDoesNotExistException 
  { 
  }
derive instance newtypeRevisionDoesNotExistException :: Newtype RevisionDoesNotExistException _


-- | <p>Information about an application revision.</p>
newtype RevisionInfo = RevisionInfo 
  { "RevisionLocation'" :: NullOrUndefined (RevisionLocation)
  , "GenericRevisionInfo'" :: NullOrUndefined (GenericRevisionInfo)
  }
derive instance newtypeRevisionInfo :: Newtype RevisionInfo _


newtype RevisionInfoList = RevisionInfoList (Array RevisionInfo)
derive instance newtypeRevisionInfoList :: Newtype RevisionInfoList _


-- | <p>Information about the location of an application revision.</p>
newtype RevisionLocation = RevisionLocation 
  { "RevisionType'" :: NullOrUndefined (RevisionLocationType)
  , "S3Location'" :: NullOrUndefined (S3Location)
  , "GitHubLocation'" :: NullOrUndefined (GitHubLocation)
  , "String" :: NullOrUndefined (RawString)
  }
derive instance newtypeRevisionLocation :: Newtype RevisionLocation _


newtype RevisionLocationList = RevisionLocationList (Array RevisionLocation)
derive instance newtypeRevisionLocationList :: Newtype RevisionLocationList _


newtype RevisionLocationType = RevisionLocationType String
derive instance newtypeRevisionLocationType :: Newtype RevisionLocationType _


-- | <p>The revision ID was not specified.</p>
newtype RevisionRequiredException = RevisionRequiredException 
  { 
  }
derive instance newtypeRevisionRequiredException :: Newtype RevisionRequiredException _


newtype Role = Role String
derive instance newtypeRole :: Newtype Role _


-- | <p>The role ID was not specified.</p>
newtype RoleRequiredException = RoleRequiredException 
  { 
  }
derive instance newtypeRoleRequiredException :: Newtype RoleRequiredException _


-- | <p>Information about a deployment rollback.</p>
newtype RollbackInfo = RollbackInfo 
  { "RollbackDeploymentId'" :: NullOrUndefined (DeploymentId)
  , "RollbackTriggeringDeploymentId'" :: NullOrUndefined (DeploymentId)
  , "RollbackMessage'" :: NullOrUndefined (Description)
  }
derive instance newtypeRollbackInfo :: Newtype RollbackInfo _


newtype S3Bucket = S3Bucket String
derive instance newtypeS3Bucket :: Newtype S3Bucket _


newtype S3Key = S3Key String
derive instance newtypeS3Key :: Newtype S3Key _


-- | <p>Information about the location of application artifacts stored in Amazon S3.</p>
newtype S3Location = S3Location 
  { "Bucket'" :: NullOrUndefined (S3Bucket)
  , "Key'" :: NullOrUndefined (S3Key)
  , "BundleType'" :: NullOrUndefined (BundleType)
  , "Version'" :: NullOrUndefined (VersionId)
  , "ETag'" :: NullOrUndefined (ETag)
  }
derive instance newtypeS3Location :: Newtype S3Location _


newtype ScriptName = ScriptName String
derive instance newtypeScriptName :: Newtype ScriptName _


newtype SkipWaitTimeForInstanceTerminationInput = SkipWaitTimeForInstanceTerminationInput 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  }
derive instance newtypeSkipWaitTimeForInstanceTerminationInput :: Newtype SkipWaitTimeForInstanceTerminationInput _


newtype SortOrder = SortOrder String
derive instance newtypeSortOrder :: Newtype SortOrder _


-- | <p>Represents the input of a StopDeployment operation.</p>
newtype StopDeploymentInput = StopDeploymentInput 
  { "DeploymentId'" :: (DeploymentId)
  , "AutoRollbackEnabled'" :: NullOrUndefined (NullableBoolean)
  }
derive instance newtypeStopDeploymentInput :: Newtype StopDeploymentInput _


-- | <p>Represents the output of a StopDeployment operation.</p>
newtype StopDeploymentOutput = StopDeploymentOutput 
  { "Status'" :: NullOrUndefined (StopStatus)
  , "StatusMessage'" :: NullOrUndefined (Message)
  }
derive instance newtypeStopDeploymentOutput :: Newtype StopDeploymentOutput _


newtype StopStatus = StopStatus String
derive instance newtypeStopStatus :: Newtype StopStatus _


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (Key)
  , "Value" :: NullOrUndefined (Value)
  }
derive instance newtypeTag :: Newtype Tag _


-- | <p>Information about an on-premises instance tag filter.</p>
newtype TagFilter = TagFilter 
  { "Key" :: NullOrUndefined (Key)
  , "Value" :: NullOrUndefined (Value)
  , "Type" :: NullOrUndefined (TagFilterType)
  }
derive instance newtypeTagFilter :: Newtype TagFilter _


newtype TagFilterList = TagFilterList (Array TagFilter)
derive instance newtypeTagFilterList :: Newtype TagFilterList _


newtype TagFilterType = TagFilterType String
derive instance newtypeTagFilterType :: Newtype TagFilterType _


-- | <p>The maximum allowed number of tags was exceeded.</p>
newtype TagLimitExceededException = TagLimitExceededException 
  { 
  }
derive instance newtypeTagLimitExceededException :: Newtype TagLimitExceededException _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


-- | <p>A tag was not specified.</p>
newtype TagRequiredException = TagRequiredException 
  { 
  }
derive instance newtypeTagRequiredException :: Newtype TagRequiredException _


-- | <p>The number of tag groups included in the tag set list exceeded the maximum allowed limit of 3.</p>
newtype TagSetListLimitExceededException = TagSetListLimitExceededException 
  { 
  }
derive instance newtypeTagSetListLimitExceededException :: Newtype TagSetListLimitExceededException _


-- | <p>Information about a target group in Elastic Load Balancing to use in a deployment. Instances are registered as targets in a target group, and traffic is routed to the target group.</p>
newtype TargetGroupInfo = TargetGroupInfo 
  { "Name'" :: NullOrUndefined (TargetGroupName)
  }
derive instance newtypeTargetGroupInfo :: Newtype TargetGroupInfo _


newtype TargetGroupInfoList = TargetGroupInfoList (Array TargetGroupInfo)
derive instance newtypeTargetGroupInfoList :: Newtype TargetGroupInfoList _


newtype TargetGroupName = TargetGroupName String
derive instance newtypeTargetGroupName :: Newtype TargetGroupName _


-- | <p>Information about the instances to be used in the replacement environment in a blue/green deployment.</p>
newtype TargetInstances = TargetInstances 
  { "TagFilters'" :: NullOrUndefined (EC2TagFilterList)
  , "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList)
  , "Ec2TagSet'" :: NullOrUndefined (EC2TagSet)
  }
derive instance newtypeTargetInstances :: Newtype TargetInstances _


-- | <p>An API function was called too frequently.</p>
newtype ThrottlingException = ThrottlingException 
  { 
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _


-- | <p>A configuration that shifts traffic from one version of a Lambda function to another in two increments. The original and target Lambda function versions are specified in the deployment's AppSpec file.</p>
newtype TimeBasedCanary = TimeBasedCanary 
  { "CanaryPercentage'" :: NullOrUndefined (Percentage)
  , "CanaryInterval'" :: NullOrUndefined (WaitTimeInMins)
  }
derive instance newtypeTimeBasedCanary :: Newtype TimeBasedCanary _


-- | <p>A configuration that shifts traffic from one version of a Lambda function to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions are specified in the deployment's AppSpec file.</p>
newtype TimeBasedLinear = TimeBasedLinear 
  { "LinearPercentage'" :: NullOrUndefined (Percentage)
  , "LinearInterval'" :: NullOrUndefined (WaitTimeInMins)
  }
derive instance newtypeTimeBasedLinear :: Newtype TimeBasedLinear _


-- | <p>Information about a time range.</p>
newtype TimeRange = TimeRange 
  { "Start'" :: NullOrUndefined (Number)
  , "End'" :: NullOrUndefined (Number)
  }
derive instance newtypeTimeRange :: Newtype TimeRange _


-- | <p>The configuration that specifies how traffic is shifted from one version of a Lambda function to another version during an AWS Lambda deployment.</p>
newtype TrafficRoutingConfig = TrafficRoutingConfig 
  { "Type'" :: NullOrUndefined (TrafficRoutingType)
  , "TimeBasedCanary'" :: NullOrUndefined (TimeBasedCanary)
  , "TimeBasedLinear'" :: NullOrUndefined (TimeBasedLinear)
  }
derive instance newtypeTrafficRoutingConfig :: Newtype TrafficRoutingConfig _


newtype TrafficRoutingType = TrafficRoutingType String
derive instance newtypeTrafficRoutingType :: Newtype TrafficRoutingType _


-- | <p>Information about notification triggers for the deployment group.</p>
newtype TriggerConfig = TriggerConfig 
  { "TriggerName'" :: NullOrUndefined (TriggerName)
  , "TriggerTargetArn'" :: NullOrUndefined (TriggerTargetArn)
  , "TriggerEvents'" :: NullOrUndefined (TriggerEventTypeList)
  }
derive instance newtypeTriggerConfig :: Newtype TriggerConfig _


newtype TriggerConfigList = TriggerConfigList (Array TriggerConfig)
derive instance newtypeTriggerConfigList :: Newtype TriggerConfigList _


newtype TriggerEventType = TriggerEventType String
derive instance newtypeTriggerEventType :: Newtype TriggerEventType _


newtype TriggerEventTypeList = TriggerEventTypeList (Array TriggerEventType)
derive instance newtypeTriggerEventTypeList :: Newtype TriggerEventTypeList _


newtype TriggerName = TriggerName String
derive instance newtypeTriggerName :: Newtype TriggerName _


newtype TriggerTargetArn = TriggerTargetArn String
derive instance newtypeTriggerTargetArn :: Newtype TriggerTargetArn _


-- | <p>The maximum allowed number of triggers was exceeded.</p>
newtype TriggerTargetsLimitExceededException = TriggerTargetsLimitExceededException 
  { 
  }
derive instance newtypeTriggerTargetsLimitExceededException :: Newtype TriggerTargetsLimitExceededException _


-- | <p>A call was submitted that is not supported for the specified deployment type.</p>
newtype UnsupportedActionForDeploymentTypeException = UnsupportedActionForDeploymentTypeException 
  { 
  }
derive instance newtypeUnsupportedActionForDeploymentTypeException :: Newtype UnsupportedActionForDeploymentTypeException _


-- | <p>Represents the input of an UpdateApplication operation.</p>
newtype UpdateApplicationInput = UpdateApplicationInput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "NewApplicationName'" :: NullOrUndefined (ApplicationName)
  }
derive instance newtypeUpdateApplicationInput :: Newtype UpdateApplicationInput _


-- | <p>Represents the input of an UpdateDeploymentGroup operation.</p>
newtype UpdateDeploymentGroupInput = UpdateDeploymentGroupInput 
  { "ApplicationName'" :: (ApplicationName)
  , "CurrentDeploymentGroupName'" :: (DeploymentGroupName)
  , "NewDeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName)
  , "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName)
  , "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList)
  , "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList)
  , "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList)
  , "ServiceRoleArn'" :: NullOrUndefined (Role)
  , "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList)
  , "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration)
  , "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration)
  , "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle)
  , "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration)
  , "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo)
  , "Ec2TagSet'" :: NullOrUndefined (EC2TagSet)
  , "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet)
  }
derive instance newtypeUpdateDeploymentGroupInput :: Newtype UpdateDeploymentGroupInput _


-- | <p>Represents the output of an UpdateDeploymentGroup operation.</p>
newtype UpdateDeploymentGroupOutput = UpdateDeploymentGroupOutput 
  { "HooksNotCleanedUp'" :: NullOrUndefined (AutoScalingGroupList)
  }
derive instance newtypeUpdateDeploymentGroupOutput :: Newtype UpdateDeploymentGroupOutput _


newtype Value = Value String
derive instance newtypeValue :: Newtype Value _


newtype VersionId = VersionId String
derive instance newtypeVersionId :: Newtype VersionId _


newtype WaitTimeInMins = WaitTimeInMins Int
derive instance newtypeWaitTimeInMins :: Newtype WaitTimeInMins _
