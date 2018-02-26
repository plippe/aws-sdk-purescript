

-- | <fullname>AWS CodeDeploy</fullname> <p>AWS CodeDeploy is a deployment service that automates application deployments to Amazon EC2 instances, on-premises instances running in your own facility, or serverless AWS Lambda functions.</p> <p>You can deploy a nearly unlimited variety of application content, such as an updated Lambda function, code, web and configuration files, executables, packages, scripts, multimedia files, and so on. AWS CodeDeploy can deploy application content stored in Amazon S3 buckets, GitHub repositories, or Bitbucket repositories. You do not need to make changes to your existing code before you can use AWS CodeDeploy.</p> <p>AWS CodeDeploy makes it easier for you to rapidly release new features, helps you avoid downtime during application deployment, and handles the complexity of updating your applications, without many of the risks associated with error-prone manual deployments.</p> <p> <b>AWS CodeDeploy Components</b> </p> <p>Use the information in this guide to help you work with the following AWS CodeDeploy components:</p> <ul> <li> <p> <b>Application</b>: A name that uniquely identifies the application you want to deploy. AWS CodeDeploy uses this name, which functions as a container, to ensure the correct combination of revision, deployment configuration, and deployment group are referenced during a deployment.</p> </li> <li> <p> <b>Deployment group</b>: A set of individual instances or CodeDeploy Lambda applications. A Lambda deployment group contains a group of applications. An EC2/On-premises deployment group contains individually tagged instances, Amazon EC2 instances in Auto Scaling groups, or both. </p> </li> <li> <p> <b>Deployment configuration</b>: A set of deployment rules and deployment success and failure conditions used by AWS CodeDeploy during a deployment.</p> </li> <li> <p> <b>Deployment</b>: The process and the components used in the process of updating a Lambda function or of installing content on one or more instances. </p> </li> <li> <p> <b>Application revisions</b>: For an AWS Lambda deployment, this is an AppSpec file that specifies the Lambda function to update and one or more functions to validate deployment lifecycle events. For an EC2/On-premises deployment, this is an archive file containing source content—source code, web pages, executable files, and deployment scripts—along with an AppSpec file. Revisions are stored in Amazon S3 buckets or GitHub repositories. For Amazon S3, a revision is uniquely identified by its Amazon S3 object key and its ETag, version, or both. For GitHub, a revision is uniquely identified by its commit ID.</p> </li> </ul> <p>This guide also contains information to help you get details about the instances in your deployments, to make on-premises instances available for AWS CodeDeploy deployments, and to get details about a Lambda function deployment.</p> <p> <b>AWS CodeDeploy Information Resources</b> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/codedeploy/latest/userguide">AWS CodeDeploy User Guide</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/codedeploy/latest/APIReference/">AWS CodeDeploy API Reference Guide</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/cli/latest/reference/deploy/index.html">AWS CLI Reference for AWS CodeDeploy</a> </p> </li> <li> <p> <a href="https://forums.aws.amazon.com/forum.jspa?forumID=179">AWS CodeDeploy Developer Forum</a> </p> </li> </ul>
module AWS.CodeDeploy where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
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


newtype AdditionalDeploymentStatusInfo = AdditionalDeploymentStatusInfo String


-- | <p>Information about an alarm.</p>
newtype Alarm = Alarm 
  { "Name'" :: NullOrUndefined (AlarmName)
  }


-- | <p>Information about alarms associated with the deployment group.</p>
newtype AlarmConfiguration = AlarmConfiguration 
  { "Enabled'" :: NullOrUndefined (Boolean)
  , "IgnorePollAlarmFailure'" :: NullOrUndefined (Boolean)
  , "Alarms'" :: NullOrUndefined (AlarmList)
  }


newtype AlarmList = AlarmList (Array Alarm)


newtype AlarmName = AlarmName String


-- | <p>The maximum number of alarms for a deployment group (10) was exceeded.</p>
newtype AlarmsLimitExceededException = AlarmsLimitExceededException 
  { 
  }


-- | <p>An application with the specified name already exists with the applicable IAM user or AWS account.</p>
newtype ApplicationAlreadyExistsException = ApplicationAlreadyExistsException 
  { 
  }


-- | <p>The application does not exist with the applicable IAM user or AWS account.</p>
newtype ApplicationDoesNotExistException = ApplicationDoesNotExistException 
  { 
  }


newtype ApplicationId = ApplicationId String


-- | <p>Information about an application.</p>
newtype ApplicationInfo = ApplicationInfo 
  { "ApplicationId'" :: NullOrUndefined (ApplicationId)
  , "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "CreateTime'" :: NullOrUndefined (Number)
  , "LinkedToGitHub'" :: NullOrUndefined (Boolean)
  , "GitHubAccountName'" :: NullOrUndefined (GitHubAccountTokenName)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  }


-- | <p>More applications were attempted to be created than are allowed.</p>
newtype ApplicationLimitExceededException = ApplicationLimitExceededException 
  { 
  }


newtype ApplicationName = ApplicationName String


-- | <p>The minimum number of required application names was not specified.</p>
newtype ApplicationNameRequiredException = ApplicationNameRequiredException 
  { 
  }


newtype ApplicationRevisionSortBy = ApplicationRevisionSortBy String


newtype ApplicationsInfoList = ApplicationsInfoList (Array ApplicationInfo)


newtype ApplicationsList = ApplicationsList (Array ApplicationName)


-- | <p>Information about a configuration for automatically rolling back to a previous version of an application revision when a deployment doesn't complete successfully.</p>
newtype AutoRollbackConfiguration = AutoRollbackConfiguration 
  { "Enabled'" :: NullOrUndefined (Boolean)
  , "Events'" :: NullOrUndefined (AutoRollbackEventsList)
  }


newtype AutoRollbackEvent = AutoRollbackEvent String


newtype AutoRollbackEventsList = AutoRollbackEventsList (Array AutoRollbackEvent)


-- | <p>Information about an Auto Scaling group.</p>
newtype AutoScalingGroup = AutoScalingGroup 
  { "Name'" :: NullOrUndefined (AutoScalingGroupName)
  , "Hook'" :: NullOrUndefined (AutoScalingGroupHook)
  }


newtype AutoScalingGroupHook = AutoScalingGroupHook String


newtype AutoScalingGroupList = AutoScalingGroupList (Array AutoScalingGroup)


newtype AutoScalingGroupName = AutoScalingGroupName String


newtype AutoScalingGroupNameList = AutoScalingGroupNameList (Array AutoScalingGroupName)


-- | <p>Represents the input of a BatchGetApplicationRevisions operation.</p>
newtype BatchGetApplicationRevisionsInput = BatchGetApplicationRevisionsInput 
  { "ApplicationName'" :: (ApplicationName)
  , "Revisions'" :: (RevisionLocationList)
  }


-- | <p>Represents the output of a BatchGetApplicationRevisions operation.</p>
newtype BatchGetApplicationRevisionsOutput = BatchGetApplicationRevisionsOutput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "ErrorMessage'" :: NullOrUndefined (ErrorMessage)
  , "Revisions'" :: NullOrUndefined (RevisionInfoList)
  }


-- | <p>Represents the input of a BatchGetApplications operation.</p>
newtype BatchGetApplicationsInput = BatchGetApplicationsInput 
  { "ApplicationNames'" :: (ApplicationsList)
  }


-- | <p>Represents the output of a BatchGetApplications operation.</p>
newtype BatchGetApplicationsOutput = BatchGetApplicationsOutput 
  { "ApplicationsInfo'" :: NullOrUndefined (ApplicationsInfoList)
  }


-- | <p>Represents the input of a BatchGetDeploymentGroups operation.</p>
newtype BatchGetDeploymentGroupsInput = BatchGetDeploymentGroupsInput 
  { "ApplicationName'" :: (ApplicationName)
  , "DeploymentGroupNames'" :: (DeploymentGroupsList)
  }


-- | <p>Represents the output of a BatchGetDeploymentGroups operation.</p>
newtype BatchGetDeploymentGroupsOutput = BatchGetDeploymentGroupsOutput 
  { "DeploymentGroupsInfo'" :: NullOrUndefined (DeploymentGroupInfoList)
  , "ErrorMessage'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Represents the input of a BatchGetDeploymentInstances operation.</p>
newtype BatchGetDeploymentInstancesInput = BatchGetDeploymentInstancesInput 
  { "DeploymentId'" :: (DeploymentId)
  , "InstanceIds'" :: (InstancesList)
  }


-- | <p>Represents the output of a BatchGetDeploymentInstances operation.</p>
newtype BatchGetDeploymentInstancesOutput = BatchGetDeploymentInstancesOutput 
  { "InstancesSummary'" :: NullOrUndefined (InstanceSummaryList)
  , "ErrorMessage'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Represents the input of a BatchGetDeployments operation.</p>
newtype BatchGetDeploymentsInput = BatchGetDeploymentsInput 
  { "DeploymentIds'" :: (DeploymentsList)
  }


-- | <p>Represents the output of a BatchGetDeployments operation.</p>
newtype BatchGetDeploymentsOutput = BatchGetDeploymentsOutput 
  { "DeploymentsInfo'" :: NullOrUndefined (DeploymentsInfoList)
  }


-- | <p>Represents the input of a BatchGetOnPremisesInstances operation.</p>
newtype BatchGetOnPremisesInstancesInput = BatchGetOnPremisesInstancesInput 
  { "InstanceNames'" :: (InstanceNameList)
  }


-- | <p>Represents the output of a BatchGetOnPremisesInstances operation.</p>
newtype BatchGetOnPremisesInstancesOutput = BatchGetOnPremisesInstancesOutput 
  { "InstanceInfos'" :: NullOrUndefined (InstanceInfoList)
  }


-- | <p>The maximum number of names or IDs allowed for this request (100) was exceeded.</p>
newtype BatchLimitExceededException = BatchLimitExceededException 
  { 
  }


-- | <p>Information about blue/green deployment options for a deployment group.</p>
newtype BlueGreenDeploymentConfiguration = BlueGreenDeploymentConfiguration 
  { "TerminateBlueInstancesOnDeploymentSuccess'" :: NullOrUndefined (BlueInstanceTerminationOption)
  , "DeploymentReadyOption'" :: NullOrUndefined (DeploymentReadyOption)
  , "GreenFleetProvisioningOption'" :: NullOrUndefined (GreenFleetProvisioningOption)
  }


-- | <p>Information about whether instances in the original environment are terminated when a blue/green deployment is successful.</p>
newtype BlueInstanceTerminationOption = BlueInstanceTerminationOption 
  { "Action'" :: NullOrUndefined (InstanceAction)
  , "TerminationWaitTimeInMinutes'" :: NullOrUndefined (Duration)
  }


-- | <p>A bucket name is required, but was not provided.</p>
newtype BucketNameFilterRequiredException = BucketNameFilterRequiredException 
  { 
  }


newtype BundleType = BundleType String


newtype CommitId = CommitId String


newtype ComputePlatform = ComputePlatform String


newtype ContinueDeploymentInput = ContinueDeploymentInput 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  }


-- | <p>Represents the input of a CreateApplication operation.</p>
newtype CreateApplicationInput = CreateApplicationInput 
  { "ApplicationName'" :: (ApplicationName)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  }


-- | <p>Represents the output of a CreateApplication operation.</p>
newtype CreateApplicationOutput = CreateApplicationOutput 
  { "ApplicationId'" :: NullOrUndefined (ApplicationId)
  }


-- | <p>Represents the input of a CreateDeploymentConfig operation.</p>
newtype CreateDeploymentConfigInput = CreateDeploymentConfigInput 
  { "DeploymentConfigName'" :: (DeploymentConfigName)
  , "MinimumHealthyHosts'" :: NullOrUndefined (MinimumHealthyHosts)
  , "TrafficRoutingConfig'" :: NullOrUndefined (TrafficRoutingConfig)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  }


-- | <p>Represents the output of a CreateDeploymentConfig operation.</p>
newtype CreateDeploymentConfigOutput = CreateDeploymentConfigOutput 
  { "DeploymentConfigId'" :: NullOrUndefined (DeploymentConfigId)
  }


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


-- | <p>Represents the output of a CreateDeploymentGroup operation.</p>
newtype CreateDeploymentGroupOutput = CreateDeploymentGroupOutput 
  { "DeploymentGroupId'" :: NullOrUndefined (DeploymentGroupId)
  }


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


-- | <p>Represents the output of a CreateDeployment operation.</p>
newtype CreateDeploymentOutput = CreateDeploymentOutput 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  }


-- | <p>Represents the input of a DeleteApplication operation.</p>
newtype DeleteApplicationInput = DeleteApplicationInput 
  { "ApplicationName'" :: (ApplicationName)
  }


-- | <p>Represents the input of a DeleteDeploymentConfig operation.</p>
newtype DeleteDeploymentConfigInput = DeleteDeploymentConfigInput 
  { "DeploymentConfigName'" :: (DeploymentConfigName)
  }


-- | <p>Represents the input of a DeleteDeploymentGroup operation.</p>
newtype DeleteDeploymentGroupInput = DeleteDeploymentGroupInput 
  { "ApplicationName'" :: (ApplicationName)
  , "DeploymentGroupName'" :: (DeploymentGroupName)
  }


-- | <p>Represents the output of a DeleteDeploymentGroup operation.</p>
newtype DeleteDeploymentGroupOutput = DeleteDeploymentGroupOutput 
  { "HooksNotCleanedUp'" :: NullOrUndefined (AutoScalingGroupList)
  }


-- | <p>Represents the input of a DeleteGitHubAccount operation.</p>
newtype DeleteGitHubAccountTokenInput = DeleteGitHubAccountTokenInput 
  { "TokenName'" :: NullOrUndefined (GitHubAccountTokenName)
  }


-- | <p>Represents the output of a DeleteGitHubAccountToken operation.</p>
newtype DeleteGitHubAccountTokenOutput = DeleteGitHubAccountTokenOutput 
  { "TokenName'" :: NullOrUndefined (GitHubAccountTokenName)
  }


-- | <p>The deployment is already complete.</p>
newtype DeploymentAlreadyCompletedException = DeploymentAlreadyCompletedException 
  { 
  }


-- | <p>A deployment configuration with the specified name already exists with the applicable IAM user or AWS account.</p>
newtype DeploymentConfigAlreadyExistsException = DeploymentConfigAlreadyExistsException 
  { 
  }


-- | <p>The deployment configuration does not exist with the applicable IAM user or AWS account.</p>
newtype DeploymentConfigDoesNotExistException = DeploymentConfigDoesNotExistException 
  { 
  }


newtype DeploymentConfigId = DeploymentConfigId String


-- | <p>The deployment configuration is still in use.</p>
newtype DeploymentConfigInUseException = DeploymentConfigInUseException 
  { 
  }


-- | <p>Information about a deployment configuration.</p>
newtype DeploymentConfigInfo = DeploymentConfigInfo 
  { "DeploymentConfigId'" :: NullOrUndefined (DeploymentConfigId)
  , "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName)
  , "MinimumHealthyHosts'" :: NullOrUndefined (MinimumHealthyHosts)
  , "CreateTime'" :: NullOrUndefined (Number)
  , "ComputePlatform'" :: NullOrUndefined (ComputePlatform)
  , "TrafficRoutingConfig'" :: NullOrUndefined (TrafficRoutingConfig)
  }


-- | <p>The deployment configurations limit was exceeded.</p>
newtype DeploymentConfigLimitExceededException = DeploymentConfigLimitExceededException 
  { 
  }


newtype DeploymentConfigName = DeploymentConfigName String


-- | <p>The deployment configuration name was not specified.</p>
newtype DeploymentConfigNameRequiredException = DeploymentConfigNameRequiredException 
  { 
  }


newtype DeploymentConfigsList = DeploymentConfigsList (Array DeploymentConfigName)


newtype DeploymentCreator = DeploymentCreator String


-- | <p>The deployment does not exist with the applicable IAM user or AWS account.</p>
newtype DeploymentDoesNotExistException = DeploymentDoesNotExistException 
  { 
  }


-- | <p>A deployment group with the specified name already exists with the applicable IAM user or AWS account.</p>
newtype DeploymentGroupAlreadyExistsException = DeploymentGroupAlreadyExistsException 
  { 
  }


-- | <p>The named deployment group does not exist with the applicable IAM user or AWS account.</p>
newtype DeploymentGroupDoesNotExistException = DeploymentGroupDoesNotExistException 
  { 
  }


newtype DeploymentGroupId = DeploymentGroupId String


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


newtype DeploymentGroupInfoList = DeploymentGroupInfoList (Array DeploymentGroupInfo)


-- | <p> The deployment groups limit was exceeded.</p>
newtype DeploymentGroupLimitExceededException = DeploymentGroupLimitExceededException 
  { 
  }


newtype DeploymentGroupName = DeploymentGroupName String


-- | <p>The deployment group name was not specified.</p>
newtype DeploymentGroupNameRequiredException = DeploymentGroupNameRequiredException 
  { 
  }


newtype DeploymentGroupsList = DeploymentGroupsList (Array DeploymentGroupName)


newtype DeploymentId = DeploymentId String


-- | <p>At least one deployment ID must be specified.</p>
newtype DeploymentIdRequiredException = DeploymentIdRequiredException 
  { 
  }


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


-- | <p>The deployment does not have a status of Ready and can't continue yet.</p>
newtype DeploymentIsNotInReadyStateException = DeploymentIsNotInReadyStateException 
  { 
  }


-- | <p>The number of allowed deployments was exceeded.</p>
newtype DeploymentLimitExceededException = DeploymentLimitExceededException 
  { 
  }


-- | <p>The specified deployment has not started.</p>
newtype DeploymentNotStartedException = DeploymentNotStartedException 
  { 
  }


newtype DeploymentOption = DeploymentOption String


-- | <p>Information about the deployment status of the instances in the deployment.</p>
newtype DeploymentOverview = DeploymentOverview 
  { "Pending" :: NullOrUndefined (InstanceCount)
  , "InProgress" :: NullOrUndefined (InstanceCount)
  , "Succeeded" :: NullOrUndefined (InstanceCount)
  , "Failed" :: NullOrUndefined (InstanceCount)
  , "Skipped" :: NullOrUndefined (InstanceCount)
  , "Ready" :: NullOrUndefined (InstanceCount)
  }


newtype DeploymentReadyAction = DeploymentReadyAction String


-- | <p>Information about how traffic is rerouted to instances in a replacement environment in a blue/green deployment.</p>
newtype DeploymentReadyOption = DeploymentReadyOption 
  { "ActionOnTimeout'" :: NullOrUndefined (DeploymentReadyAction)
  , "WaitTimeInMinutes'" :: NullOrUndefined (Duration)
  }


newtype DeploymentStatus = DeploymentStatus String


newtype DeploymentStatusList = DeploymentStatusList (Array DeploymentStatus)


newtype DeploymentStatusMessageList = DeploymentStatusMessageList (Array ErrorMessage)


-- | <p>Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.</p>
newtype DeploymentStyle = DeploymentStyle 
  { "DeploymentType'" :: NullOrUndefined (DeploymentType)
  , "DeploymentOption'" :: NullOrUndefined (DeploymentOption)
  }


newtype DeploymentType = DeploymentType String


newtype DeploymentsInfoList = DeploymentsInfoList (Array DeploymentInfo)


newtype DeploymentsList = DeploymentsList (Array DeploymentId)


-- | <p>Represents the input of a DeregisterOnPremisesInstance operation.</p>
newtype DeregisterOnPremisesInstanceInput = DeregisterOnPremisesInstanceInput 
  { "InstanceName'" :: (InstanceName)
  }


newtype Description = Description String


-- | <p>The description is too long.</p>
newtype DescriptionTooLongException = DescriptionTooLongException 
  { 
  }


-- | <p>Diagnostic information about executable scripts that are part of a deployment.</p>
newtype Diagnostics = Diagnostics 
  { "ErrorCode'" :: NullOrUndefined (LifecycleErrorCode)
  , "ScriptName'" :: NullOrUndefined (ScriptName)
  , "Message'" :: NullOrUndefined (LifecycleMessage)
  , "LogTail'" :: NullOrUndefined (LogTail)
  }


newtype Duration = Duration Int


-- | <p>Information about an EC2 tag filter.</p>
newtype EC2TagFilter = EC2TagFilter 
  { "Key" :: NullOrUndefined (Key)
  , "Value" :: NullOrUndefined (Value)
  , "Type" :: NullOrUndefined (EC2TagFilterType)
  }


newtype EC2TagFilterList = EC2TagFilterList (Array EC2TagFilter)


newtype EC2TagFilterType = EC2TagFilterType String


-- | <p>Information about groups of EC2 instance tags.</p>
newtype EC2TagSet = EC2TagSet 
  { "Ec2TagSetList'" :: NullOrUndefined (EC2TagSetList)
  }


newtype EC2TagSetList = EC2TagSetList (Array EC2TagFilterList)


-- | <p>Information about a load balancer in Elastic Load Balancing to use in a deployment. Instances are registered directly with a load balancer, and traffic is routed to the load balancer.</p>
newtype ELBInfo = ELBInfo 
  { "Name'" :: NullOrUndefined (ELBName)
  }


newtype ELBInfoList = ELBInfoList (Array ELBInfo)


newtype ELBName = ELBName String


newtype ETag = ETag String


newtype ErrorCode = ErrorCode String


-- | <p>Information about a deployment error.</p>
newtype ErrorInformation = ErrorInformation 
  { "Code'" :: NullOrUndefined (ErrorCode)
  , "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype ErrorMessage = ErrorMessage String


newtype FileExistsBehavior = FileExistsBehavior String


-- | <p>Information about an application revision.</p>
newtype GenericRevisionInfo = GenericRevisionInfo 
  { "Description'" :: NullOrUndefined (Description)
  , "DeploymentGroups'" :: NullOrUndefined (DeploymentGroupsList)
  , "FirstUsedTime'" :: NullOrUndefined (Number)
  , "LastUsedTime'" :: NullOrUndefined (Number)
  , "RegisterTime'" :: NullOrUndefined (Number)
  }


-- | <p>Represents the input of a GetApplication operation.</p>
newtype GetApplicationInput = GetApplicationInput 
  { "ApplicationName'" :: (ApplicationName)
  }


-- | <p>Represents the output of a GetApplication operation.</p>
newtype GetApplicationOutput = GetApplicationOutput 
  { "Application'" :: NullOrUndefined (ApplicationInfo)
  }


-- | <p>Represents the input of a GetApplicationRevision operation.</p>
newtype GetApplicationRevisionInput = GetApplicationRevisionInput 
  { "ApplicationName'" :: (ApplicationName)
  , "Revision'" :: (RevisionLocation)
  }


-- | <p>Represents the output of a GetApplicationRevision operation.</p>
newtype GetApplicationRevisionOutput = GetApplicationRevisionOutput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "Revision'" :: NullOrUndefined (RevisionLocation)
  , "RevisionInfo'" :: NullOrUndefined (GenericRevisionInfo)
  }


-- | <p>Represents the input of a GetDeploymentConfig operation.</p>
newtype GetDeploymentConfigInput = GetDeploymentConfigInput 
  { "DeploymentConfigName'" :: (DeploymentConfigName)
  }


-- | <p>Represents the output of a GetDeploymentConfig operation.</p>
newtype GetDeploymentConfigOutput = GetDeploymentConfigOutput 
  { "DeploymentConfigInfo'" :: NullOrUndefined (DeploymentConfigInfo)
  }


-- | <p>Represents the input of a GetDeploymentGroup operation.</p>
newtype GetDeploymentGroupInput = GetDeploymentGroupInput 
  { "ApplicationName'" :: (ApplicationName)
  , "DeploymentGroupName'" :: (DeploymentGroupName)
  }


-- | <p>Represents the output of a GetDeploymentGroup operation.</p>
newtype GetDeploymentGroupOutput = GetDeploymentGroupOutput 
  { "DeploymentGroupInfo'" :: NullOrUndefined (DeploymentGroupInfo)
  }


-- | <p>Represents the input of a GetDeployment operation.</p>
newtype GetDeploymentInput = GetDeploymentInput 
  { "DeploymentId'" :: (DeploymentId)
  }


-- | <p>Represents the input of a GetDeploymentInstance operation.</p>
newtype GetDeploymentInstanceInput = GetDeploymentInstanceInput 
  { "DeploymentId'" :: (DeploymentId)
  , "InstanceId'" :: (InstanceId)
  }


-- | <p>Represents the output of a GetDeploymentInstance operation.</p>
newtype GetDeploymentInstanceOutput = GetDeploymentInstanceOutput 
  { "InstanceSummary'" :: NullOrUndefined (InstanceSummary)
  }


-- | <p>Represents the output of a GetDeployment operation.</p>
newtype GetDeploymentOutput = GetDeploymentOutput 
  { "DeploymentInfo'" :: NullOrUndefined (DeploymentInfo)
  }


-- | <p>Represents the input of a GetOnPremisesInstance operation.</p>
newtype GetOnPremisesInstanceInput = GetOnPremisesInstanceInput 
  { "InstanceName'" :: (InstanceName)
  }


-- | <p>Represents the output of a GetOnPremisesInstance operation.</p>
newtype GetOnPremisesInstanceOutput = GetOnPremisesInstanceOutput 
  { "InstanceInfo'" :: NullOrUndefined (InstanceInfo)
  }


-- | <p>No GitHub account connection exists with the named specified in the call.</p>
newtype GitHubAccountTokenDoesNotExistException = GitHubAccountTokenDoesNotExistException 
  { 
  }


newtype GitHubAccountTokenName = GitHubAccountTokenName String


newtype GitHubAccountTokenNameList = GitHubAccountTokenNameList (Array GitHubAccountTokenName)


-- | <p>The call is missing a required GitHub account connection name.</p>
newtype GitHubAccountTokenNameRequiredException = GitHubAccountTokenNameRequiredException 
  { 
  }


-- | <p>Information about the location of application artifacts stored in GitHub.</p>
newtype GitHubLocation = GitHubLocation 
  { "Repository'" :: NullOrUndefined (Repository)
  , "CommitId'" :: NullOrUndefined (CommitId)
  }


newtype GreenFleetProvisioningAction = GreenFleetProvisioningAction String


-- | <p>Information about the instances that belong to the replacement environment in a blue/green deployment.</p>
newtype GreenFleetProvisioningOption = GreenFleetProvisioningOption 
  { "Action'" :: NullOrUndefined (GreenFleetProvisioningAction)
  }


-- | <p>No IAM ARN was included in the request. You must use an IAM session ARN or IAM user ARN in the request.</p>
newtype IamArnRequiredException = IamArnRequiredException 
  { 
  }


newtype IamSessionArn = IamSessionArn String


-- | <p>The request included an IAM session ARN that has already been used to register a different instance.</p>
newtype IamSessionArnAlreadyRegisteredException = IamSessionArnAlreadyRegisteredException 
  { 
  }


newtype IamUserArn = IamUserArn String


-- | <p>The specified IAM user ARN is already registered with an on-premises instance.</p>
newtype IamUserArnAlreadyRegisteredException = IamUserArnAlreadyRegisteredException 
  { 
  }


-- | <p>An IAM user ARN was not specified.</p>
newtype IamUserArnRequiredException = IamUserArnRequiredException 
  { 
  }


newtype InstanceAction = InstanceAction String


newtype InstanceArn = InstanceArn String


newtype InstanceCount = InstanceCount Number


-- | <p>The specified instance does not exist in the deployment group.</p>
newtype InstanceDoesNotExistException = InstanceDoesNotExistException 
  { 
  }


newtype InstanceId = InstanceId String


-- | <p>The instance ID was not specified.</p>
newtype InstanceIdRequiredException = InstanceIdRequiredException 
  { 
  }


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


newtype InstanceInfoList = InstanceInfoList (Array InstanceInfo)


-- | <p>The maximum number of allowed on-premises instances in a single call was exceeded.</p>
newtype InstanceLimitExceededException = InstanceLimitExceededException 
  { 
  }


newtype InstanceName = InstanceName String


-- | <p>The specified on-premises instance name is already registered.</p>
newtype InstanceNameAlreadyRegisteredException = InstanceNameAlreadyRegisteredException 
  { 
  }


newtype InstanceNameList = InstanceNameList (Array InstanceName)


-- | <p>An on-premises instance name was not specified.</p>
newtype InstanceNameRequiredException = InstanceNameRequiredException 
  { 
  }


-- | <p>The specified on-premises instance is not registered.</p>
newtype InstanceNotRegisteredException = InstanceNotRegisteredException 
  { 
  }


newtype InstanceStatus = InstanceStatus String


newtype InstanceStatusList = InstanceStatusList (Array InstanceStatus)


-- | <p>Information about an instance in a deployment.</p>
newtype InstanceSummary = InstanceSummary 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  , "InstanceId'" :: NullOrUndefined (InstanceId)
  , "Status'" :: NullOrUndefined (InstanceStatus)
  , "LastUpdatedAt'" :: NullOrUndefined (Number)
  , "LifecycleEvents'" :: NullOrUndefined (LifecycleEventList)
  , "InstanceType'" :: NullOrUndefined (InstanceType)
  }


newtype InstanceSummaryList = InstanceSummaryList (Array InstanceSummary)


newtype InstanceType = InstanceType String


newtype InstanceTypeList = InstanceTypeList (Array InstanceType)


newtype InstancesList = InstancesList (Array InstanceId)


-- | <p>The format of the alarm configuration is invalid. Possible causes include:</p> <ul> <li> <p>The alarm list is null.</p> </li> <li> <p>The alarm object is null.</p> </li> <li> <p>The alarm name is empty or null or exceeds the 255 character limit.</p> </li> <li> <p>Two alarms with the same name have been specified.</p> </li> <li> <p>The alarm configuration is enabled but the alarm list is empty.</p> </li> </ul>
newtype InvalidAlarmConfigException = InvalidAlarmConfigException 
  { 
  }


-- | <p>The application name was specified in an invalid format.</p>
newtype InvalidApplicationNameException = InvalidApplicationNameException 
  { 
  }


-- | <p>The automatic rollback configuration was specified in an invalid format. For example, automatic rollback is enabled but an invalid triggering event type or no event types were listed.</p>
newtype InvalidAutoRollbackConfigException = InvalidAutoRollbackConfigException 
  { 
  }


-- | <p>The Auto Scaling group was specified in an invalid format or does not exist.</p>
newtype InvalidAutoScalingGroupException = InvalidAutoScalingGroupException 
  { 
  }


-- | <p>The configuration for the blue/green deployment group was provided in an invalid format. For information about deployment configuration format, see <a>CreateDeploymentConfig</a>.</p>
newtype InvalidBlueGreenDeploymentConfigurationException = InvalidBlueGreenDeploymentConfigurationException 
  { 
  }


-- | <p>The bucket name either doesn't exist or was specified in an invalid format.</p>
newtype InvalidBucketNameFilterException = InvalidBucketNameFilterException 
  { 
  }


-- | <p>The computePlatform is invalid. The computePlatform should be <code>Lambda</code> or <code>Server</code>.</p>
newtype InvalidComputePlatformException = InvalidComputePlatformException 
  { 
  }


-- | <p>The deployed state filter was specified in an invalid format.</p>
newtype InvalidDeployedStateFilterException = InvalidDeployedStateFilterException 
  { 
  }


-- | <p>The deployment configuration name was specified in an invalid format.</p>
newtype InvalidDeploymentConfigNameException = InvalidDeploymentConfigNameException 
  { 
  }


-- | <p>The deployment group name was specified in an invalid format.</p>
newtype InvalidDeploymentGroupNameException = InvalidDeploymentGroupNameException 
  { 
  }


-- | <p>At least one of the deployment IDs was specified in an invalid format.</p>
newtype InvalidDeploymentIdException = InvalidDeploymentIdException 
  { 
  }


-- | <p>An instance type was specified for an in-place deployment. Instance types are supported for blue/green deployments only.</p>
newtype InvalidDeploymentInstanceTypeException = InvalidDeploymentInstanceTypeException 
  { 
  }


-- | <p>The specified deployment status doesn't exist or cannot be determined.</p>
newtype InvalidDeploymentStatusException = InvalidDeploymentStatusException 
  { 
  }


-- | <p>An invalid deployment style was specified. Valid deployment types include "IN_PLACE" and "BLUE_GREEN". Valid deployment options include "WITH_TRAFFIC_CONTROL" and "WITHOUT_TRAFFIC_CONTROL".</p>
newtype InvalidDeploymentStyleException = InvalidDeploymentStyleException 
  { 
  }


-- | <p>A call was submitted that specified both Ec2TagFilters and Ec2TagSet, but only one of these data types can be used in a single call.</p>
newtype InvalidEC2TagCombinationException = InvalidEC2TagCombinationException 
  { 
  }


-- | <p>The tag was specified in an invalid format.</p>
newtype InvalidEC2TagException = InvalidEC2TagException 
  { 
  }


-- | <p>An invalid fileExistsBehavior option was specified to determine how AWS CodeDeploy handles files or directories that already exist in a deployment target location but weren't part of the previous successful deployment. Valid values include "DISALLOW", "OVERWRITE", and "RETAIN".</p>
newtype InvalidFileExistsBehaviorException = InvalidFileExistsBehaviorException 
  { 
  }


-- | <p>The format of the specified GitHub account connection name is invalid.</p>
newtype InvalidGitHubAccountTokenNameException = InvalidGitHubAccountTokenNameException 
  { 
  }


-- | <p>The IAM session ARN was specified in an invalid format.</p>
newtype InvalidIamSessionArnException = InvalidIamSessionArnException 
  { 
  }


-- | <p>The IAM user ARN was specified in an invalid format.</p>
newtype InvalidIamUserArnException = InvalidIamUserArnException 
  { 
  }


-- | <p>The IgnoreApplicationStopFailures value is invalid. For AWS Lambda deployments, <code>false</code> is expected. For EC2/On-premises deployments, <code>true</code> or <code>false</code> is expected.</p>
newtype InvalidIgnoreApplicationStopFailuresValueException = InvalidIgnoreApplicationStopFailuresValueException 
  { 
  }


-- | <p>The specified input was specified in an invalid format.</p>
newtype InvalidInputException = InvalidInputException 
  { 
  }


-- | <p> </p>
newtype InvalidInstanceIdException = InvalidInstanceIdException 
  { 
  }


-- | <p>The specified on-premises instance name was specified in an invalid format.</p>
newtype InvalidInstanceNameException = InvalidInstanceNameException 
  { 
  }


-- | <p>The specified instance status does not exist.</p>
newtype InvalidInstanceStatusException = InvalidInstanceStatusException 
  { 
  }


-- | <p>An invalid instance type was specified for instances in a blue/green deployment. Valid values include "Blue" for an original environment and "Green" for a replacement environment.</p>
newtype InvalidInstanceTypeException = InvalidInstanceTypeException 
  { 
  }


-- | <p>The specified key prefix filter was specified in an invalid format.</p>
newtype InvalidKeyPrefixFilterException = InvalidKeyPrefixFilterException 
  { 
  }


-- | <p>A lifecycle event hook is invalid. Review the <code>hooks</code> section in your AppSpec file to ensure the lifecycle events and <code>hooks</code> functions are valid.</p>
newtype InvalidLifecycleEventHookExecutionIdException = InvalidLifecycleEventHookExecutionIdException 
  { 
  }


-- | <p>The result of a Lambda validation function that verifies a lifecycle event is invalid. It should return <code>Succeeded</code> or <code>Failed</code>.</p>
newtype InvalidLifecycleEventHookExecutionStatusException = InvalidLifecycleEventHookExecutionStatusException 
  { 
  }


-- | <p>An invalid load balancer name, or no load balancer name, was specified.</p>
newtype InvalidLoadBalancerInfoException = InvalidLoadBalancerInfoException 
  { 
  }


-- | <p>The minimum healthy instance value was specified in an invalid format.</p>
newtype InvalidMinimumHealthyHostValueException = InvalidMinimumHealthyHostValueException 
  { 
  }


-- | <p>The next token was specified in an invalid format.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { 
  }


-- | <p>A call was submitted that specified both OnPremisesTagFilters and OnPremisesTagSet, but only one of these data types can be used in a single call.</p>
newtype InvalidOnPremisesTagCombinationException = InvalidOnPremisesTagCombinationException 
  { 
  }


-- | <p>An invalid operation was detected.</p>
newtype InvalidOperationException = InvalidOperationException 
  { 
  }


-- | <p>The registration status was specified in an invalid format.</p>
newtype InvalidRegistrationStatusException = InvalidRegistrationStatusException 
  { 
  }


-- | <p>The revision was specified in an invalid format.</p>
newtype InvalidRevisionException = InvalidRevisionException 
  { 
  }


-- | <p>The service role ARN was specified in an invalid format. Or, if an Auto Scaling group was specified, the specified service role does not grant the appropriate permissions to Auto Scaling.</p>
newtype InvalidRoleException = InvalidRoleException 
  { 
  }


-- | <p>The column name to sort by is either not present or was specified in an invalid format.</p>
newtype InvalidSortByException = InvalidSortByException 
  { 
  }


-- | <p>The sort order was specified in an invalid format.</p>
newtype InvalidSortOrderException = InvalidSortOrderException 
  { 
  }


-- | <p>The specified tag was specified in an invalid format.</p>
newtype InvalidTagException = InvalidTagException 
  { 
  }


-- | <p>The specified tag filter was specified in an invalid format.</p>
newtype InvalidTagFilterException = InvalidTagFilterException 
  { 
  }


-- | <p>The target instance configuration is invalid. Possible causes include:</p> <ul> <li> <p>Configuration data for target instances was entered for an in-place deployment.</p> </li> <li> <p>The limit of 10 tags for a tag type was exceeded.</p> </li> <li> <p>The combined length of the tag names exceeded the limit. </p> </li> <li> <p>A specified tag is not currently applied to any instances.</p> </li> </ul>
newtype InvalidTargetInstancesException = InvalidTargetInstancesException 
  { 
  }


-- | <p>The specified time range was specified in an invalid format.</p>
newtype InvalidTimeRangeException = InvalidTimeRangeException 
  { 
  }


-- | <p> The configuration that specifies how traffic is routed during a deployment is invalid.</p>
newtype InvalidTrafficRoutingConfigurationException = InvalidTrafficRoutingConfigurationException 
  { 
  }


-- | <p>The trigger was specified in an invalid format.</p>
newtype InvalidTriggerConfigException = InvalidTriggerConfigException 
  { 
  }


-- | <p>The UpdateOutdatedInstancesOnly value is invalid. For AWS Lambda deployments, <code>false</code> is expected. For EC2/On-premises deployments, <code>true</code> or <code>false</code> is expected.</p>
newtype InvalidUpdateOutdatedInstancesOnlyValueException = InvalidUpdateOutdatedInstancesOnlyValueException 
  { 
  }


newtype Key = Key String


-- | <p>Information about the most recent attempted or successful deployment to a deployment group.</p>
newtype LastDeploymentInfo = LastDeploymentInfo 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  , "Status'" :: NullOrUndefined (DeploymentStatus)
  , "EndTime'" :: NullOrUndefined (Number)
  , "CreateTime'" :: NullOrUndefined (Number)
  }


newtype LifecycleErrorCode = LifecycleErrorCode String


-- | <p>Information about a deployment lifecycle event.</p>
newtype LifecycleEvent = LifecycleEvent 
  { "LifecycleEventName'" :: NullOrUndefined (LifecycleEventName)
  , "Diagnostics'" :: NullOrUndefined (Diagnostics)
  , "StartTime'" :: NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined (Number)
  , "Status'" :: NullOrUndefined (LifecycleEventStatus)
  }


-- | <p>An attempt to return the status of an already completed lifecycle event occurred.</p>
newtype LifecycleEventAlreadyCompletedException = LifecycleEventAlreadyCompletedException 
  { 
  }


newtype LifecycleEventHookExecutionId = LifecycleEventHookExecutionId String


newtype LifecycleEventList = LifecycleEventList (Array LifecycleEvent)


newtype LifecycleEventName = LifecycleEventName String


newtype LifecycleEventStatus = LifecycleEventStatus String


-- | <p>The limit for lifecycle hooks was exceeded.</p>
newtype LifecycleHookLimitExceededException = LifecycleHookLimitExceededException 
  { 
  }


newtype LifecycleMessage = LifecycleMessage String


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


-- | <p>Represents the output of a ListApplicationRevisions operation.</p>
newtype ListApplicationRevisionsOutput = ListApplicationRevisionsOutput 
  { "Revisions'" :: NullOrUndefined (RevisionLocationList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListApplications operation.</p>
newtype ListApplicationsInput = ListApplicationsInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a ListApplications operation.</p>
newtype ListApplicationsOutput = ListApplicationsOutput 
  { "Applications'" :: NullOrUndefined (ApplicationsList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListDeploymentConfigs operation.</p>
newtype ListDeploymentConfigsInput = ListDeploymentConfigsInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a ListDeploymentConfigs operation.</p>
newtype ListDeploymentConfigsOutput = ListDeploymentConfigsOutput 
  { "DeploymentConfigsList'" :: NullOrUndefined (DeploymentConfigsList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListDeploymentGroups operation.</p>
newtype ListDeploymentGroupsInput = ListDeploymentGroupsInput 
  { "ApplicationName'" :: (ApplicationName)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a ListDeploymentGroups operation.</p>
newtype ListDeploymentGroupsOutput = ListDeploymentGroupsOutput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "DeploymentGroups'" :: NullOrUndefined (DeploymentGroupsList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListDeploymentInstances operation.</p>
newtype ListDeploymentInstancesInput = ListDeploymentInstancesInput 
  { "DeploymentId'" :: (DeploymentId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "InstanceStatusFilter'" :: NullOrUndefined (InstanceStatusList)
  , "InstanceTypeFilter'" :: NullOrUndefined (InstanceTypeList)
  }


-- | <p>Represents the output of a ListDeploymentInstances operation.</p>
newtype ListDeploymentInstancesOutput = ListDeploymentInstancesOutput 
  { "InstancesList'" :: NullOrUndefined (InstancesList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListDeployments operation.</p>
newtype ListDeploymentsInput = ListDeploymentsInput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName)
  , "IncludeOnlyStatuses'" :: NullOrUndefined (DeploymentStatusList)
  , "CreateTimeRange'" :: NullOrUndefined (TimeRange)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a ListDeployments operation.</p>
newtype ListDeploymentsOutput = ListDeploymentsOutput 
  { "Deployments'" :: NullOrUndefined (DeploymentsList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListGitHubAccountTokenNames operation.</p>
newtype ListGitHubAccountTokenNamesInput = ListGitHubAccountTokenNamesInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a ListGitHubAccountTokenNames operation.</p>
newtype ListGitHubAccountTokenNamesOutput = ListGitHubAccountTokenNamesOutput 
  { "TokenNameList'" :: NullOrUndefined (GitHubAccountTokenNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListOnPremisesInstances operation.</p>
newtype ListOnPremisesInstancesInput = ListOnPremisesInstancesInput 
  { "RegistrationStatus'" :: NullOrUndefined (RegistrationStatus)
  , "TagFilters'" :: NullOrUndefined (TagFilterList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of list on-premises instances operation.</p>
newtype ListOnPremisesInstancesOutput = ListOnPremisesInstancesOutput 
  { "InstanceNames'" :: NullOrUndefined (InstanceNameList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListStateFilterAction = ListStateFilterAction String


-- | <p>Information about the Elastic Load Balancing load balancer or target group used in a deployment.</p>
newtype LoadBalancerInfo = LoadBalancerInfo 
  { "ElbInfoList'" :: NullOrUndefined (ELBInfoList)
  , "TargetGroupInfoList'" :: NullOrUndefined (TargetGroupInfoList)
  }


newtype LogTail = LogTail String


newtype Message = Message String


-- | <p>Information about minimum healthy instance.</p>
newtype MinimumHealthyHosts = MinimumHealthyHosts 
  { "Value'" :: NullOrUndefined (MinimumHealthyHostsValue)
  , "Type'" :: NullOrUndefined (MinimumHealthyHostsType)
  }


newtype MinimumHealthyHostsType = MinimumHealthyHostsType String


newtype MinimumHealthyHostsValue = MinimumHealthyHostsValue Int


-- | <p>Both an IAM user ARN and an IAM session ARN were included in the request. Use only one ARN type.</p>
newtype MultipleIamArnsProvidedException = MultipleIamArnsProvidedException 
  { 
  }


newtype NextToken = NextToken String


newtype NullableBoolean = NullableBoolean Boolean


-- | <p>Information about groups of on-premises instance tags.</p>
newtype OnPremisesTagSet = OnPremisesTagSet 
  { "OnPremisesTagSetList'" :: NullOrUndefined (OnPremisesTagSetList)
  }


newtype OnPremisesTagSetList = OnPremisesTagSetList (Array TagFilterList)


-- | <p>The API used does not support the deployment.</p>
newtype OperationNotSupportedException = OperationNotSupportedException 
  { 
  }


newtype Percentage = Percentage Int


newtype PutLifecycleEventHookExecutionStatusInput = PutLifecycleEventHookExecutionStatusInput 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  , "LifecycleEventHookExecutionId'" :: NullOrUndefined (LifecycleEventHookExecutionId)
  , "Status'" :: NullOrUndefined (LifecycleEventStatus)
  }


newtype PutLifecycleEventHookExecutionStatusOutput = PutLifecycleEventHookExecutionStatusOutput 
  { "LifecycleEventHookExecutionId'" :: NullOrUndefined (LifecycleEventHookExecutionId)
  }


-- | <p>A revision for an AWS Lambda deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda deployments, the revision is the same as the AppSpec file.</p>
newtype RawString = RawString 
  { "Content'" :: NullOrUndefined (RawStringContent)
  , "Sha256'" :: NullOrUndefined (RawStringSha256)
  }


newtype RawStringContent = RawStringContent String


newtype RawStringSha256 = RawStringSha256 String


-- | <p>Represents the input of a RegisterApplicationRevision operation.</p>
newtype RegisterApplicationRevisionInput = RegisterApplicationRevisionInput 
  { "ApplicationName'" :: (ApplicationName)
  , "Description'" :: NullOrUndefined (Description)
  , "Revision'" :: (RevisionLocation)
  }


-- | <p>Represents the input of the register on-premises instance operation.</p>
newtype RegisterOnPremisesInstanceInput = RegisterOnPremisesInstanceInput 
  { "InstanceName'" :: (InstanceName)
  , "IamSessionArn'" :: NullOrUndefined (IamSessionArn)
  , "IamUserArn'" :: NullOrUndefined (IamUserArn)
  }


newtype RegistrationStatus = RegistrationStatus String


-- | <p>Represents the input of a RemoveTagsFromOnPremisesInstances operation.</p>
newtype RemoveTagsFromOnPremisesInstancesInput = RemoveTagsFromOnPremisesInstancesInput 
  { "Tags'" :: (TagList)
  , "InstanceNames'" :: (InstanceNameList)
  }


newtype Repository = Repository String


-- | <p>The specified resource could not be validated.</p>
newtype ResourceValidationException = ResourceValidationException 
  { 
  }


-- | <p>The named revision does not exist with the applicable IAM user or AWS account.</p>
newtype RevisionDoesNotExistException = RevisionDoesNotExistException 
  { 
  }


-- | <p>Information about an application revision.</p>
newtype RevisionInfo = RevisionInfo 
  { "RevisionLocation'" :: NullOrUndefined (RevisionLocation)
  , "GenericRevisionInfo'" :: NullOrUndefined (GenericRevisionInfo)
  }


newtype RevisionInfoList = RevisionInfoList (Array RevisionInfo)


-- | <p>Information about the location of an application revision.</p>
newtype RevisionLocation = RevisionLocation 
  { "RevisionType'" :: NullOrUndefined (RevisionLocationType)
  , "S3Location'" :: NullOrUndefined (S3Location)
  , "GitHubLocation'" :: NullOrUndefined (GitHubLocation)
  , "String" :: NullOrUndefined (RawString)
  }


newtype RevisionLocationList = RevisionLocationList (Array RevisionLocation)


newtype RevisionLocationType = RevisionLocationType String


-- | <p>The revision ID was not specified.</p>
newtype RevisionRequiredException = RevisionRequiredException 
  { 
  }


newtype Role = Role String


-- | <p>The role ID was not specified.</p>
newtype RoleRequiredException = RoleRequiredException 
  { 
  }


-- | <p>Information about a deployment rollback.</p>
newtype RollbackInfo = RollbackInfo 
  { "RollbackDeploymentId'" :: NullOrUndefined (DeploymentId)
  , "RollbackTriggeringDeploymentId'" :: NullOrUndefined (DeploymentId)
  , "RollbackMessage'" :: NullOrUndefined (Description)
  }


newtype S3Bucket = S3Bucket String


newtype S3Key = S3Key String


-- | <p>Information about the location of application artifacts stored in Amazon S3.</p>
newtype S3Location = S3Location 
  { "Bucket'" :: NullOrUndefined (S3Bucket)
  , "Key'" :: NullOrUndefined (S3Key)
  , "BundleType'" :: NullOrUndefined (BundleType)
  , "Version'" :: NullOrUndefined (VersionId)
  , "ETag'" :: NullOrUndefined (ETag)
  }


newtype ScriptName = ScriptName String


newtype SkipWaitTimeForInstanceTerminationInput = SkipWaitTimeForInstanceTerminationInput 
  { "DeploymentId'" :: NullOrUndefined (DeploymentId)
  }


newtype SortOrder = SortOrder String


-- | <p>Represents the input of a StopDeployment operation.</p>
newtype StopDeploymentInput = StopDeploymentInput 
  { "DeploymentId'" :: (DeploymentId)
  , "AutoRollbackEnabled'" :: NullOrUndefined (NullableBoolean)
  }


-- | <p>Represents the output of a StopDeployment operation.</p>
newtype StopDeploymentOutput = StopDeploymentOutput 
  { "Status'" :: NullOrUndefined (StopStatus)
  , "StatusMessage'" :: NullOrUndefined (Message)
  }


newtype StopStatus = StopStatus String


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (Key)
  , "Value" :: NullOrUndefined (Value)
  }


-- | <p>Information about an on-premises instance tag filter.</p>
newtype TagFilter = TagFilter 
  { "Key" :: NullOrUndefined (Key)
  , "Value" :: NullOrUndefined (Value)
  , "Type" :: NullOrUndefined (TagFilterType)
  }


newtype TagFilterList = TagFilterList (Array TagFilter)


newtype TagFilterType = TagFilterType String


-- | <p>The maximum allowed number of tags was exceeded.</p>
newtype TagLimitExceededException = TagLimitExceededException 
  { 
  }


newtype TagList = TagList (Array Tag)


-- | <p>A tag was not specified.</p>
newtype TagRequiredException = TagRequiredException 
  { 
  }


-- | <p>The number of tag groups included in the tag set list exceeded the maximum allowed limit of 3.</p>
newtype TagSetListLimitExceededException = TagSetListLimitExceededException 
  { 
  }


-- | <p>Information about a target group in Elastic Load Balancing to use in a deployment. Instances are registered as targets in a target group, and traffic is routed to the target group.</p>
newtype TargetGroupInfo = TargetGroupInfo 
  { "Name'" :: NullOrUndefined (TargetGroupName)
  }


newtype TargetGroupInfoList = TargetGroupInfoList (Array TargetGroupInfo)


newtype TargetGroupName = TargetGroupName String


-- | <p>Information about the instances to be used in the replacement environment in a blue/green deployment.</p>
newtype TargetInstances = TargetInstances 
  { "TagFilters'" :: NullOrUndefined (EC2TagFilterList)
  , "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList)
  , "Ec2TagSet'" :: NullOrUndefined (EC2TagSet)
  }


-- | <p>An API function was called too frequently.</p>
newtype ThrottlingException = ThrottlingException 
  { 
  }


-- | <p>A configuration that shifts traffic from one version of a Lambda function to another in two increments. The original and target Lambda function versions are specified in the deployment's AppSpec file.</p>
newtype TimeBasedCanary = TimeBasedCanary 
  { "CanaryPercentage'" :: NullOrUndefined (Percentage)
  , "CanaryInterval'" :: NullOrUndefined (WaitTimeInMins)
  }


-- | <p>A configuration that shifts traffic from one version of a Lambda function to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions are specified in the deployment's AppSpec file.</p>
newtype TimeBasedLinear = TimeBasedLinear 
  { "LinearPercentage'" :: NullOrUndefined (Percentage)
  , "LinearInterval'" :: NullOrUndefined (WaitTimeInMins)
  }


-- | <p>Information about a time range.</p>
newtype TimeRange = TimeRange 
  { "Start'" :: NullOrUndefined (Number)
  , "End'" :: NullOrUndefined (Number)
  }


-- | <p>The configuration that specifies how traffic is shifted from one version of a Lambda function to another version during an AWS Lambda deployment.</p>
newtype TrafficRoutingConfig = TrafficRoutingConfig 
  { "Type'" :: NullOrUndefined (TrafficRoutingType)
  , "TimeBasedCanary'" :: NullOrUndefined (TimeBasedCanary)
  , "TimeBasedLinear'" :: NullOrUndefined (TimeBasedLinear)
  }


newtype TrafficRoutingType = TrafficRoutingType String


-- | <p>Information about notification triggers for the deployment group.</p>
newtype TriggerConfig = TriggerConfig 
  { "TriggerName'" :: NullOrUndefined (TriggerName)
  , "TriggerTargetArn'" :: NullOrUndefined (TriggerTargetArn)
  , "TriggerEvents'" :: NullOrUndefined (TriggerEventTypeList)
  }


newtype TriggerConfigList = TriggerConfigList (Array TriggerConfig)


newtype TriggerEventType = TriggerEventType String


newtype TriggerEventTypeList = TriggerEventTypeList (Array TriggerEventType)


newtype TriggerName = TriggerName String


newtype TriggerTargetArn = TriggerTargetArn String


-- | <p>The maximum allowed number of triggers was exceeded.</p>
newtype TriggerTargetsLimitExceededException = TriggerTargetsLimitExceededException 
  { 
  }


-- | <p>A call was submitted that is not supported for the specified deployment type.</p>
newtype UnsupportedActionForDeploymentTypeException = UnsupportedActionForDeploymentTypeException 
  { 
  }


-- | <p>Represents the input of an UpdateApplication operation.</p>
newtype UpdateApplicationInput = UpdateApplicationInput 
  { "ApplicationName'" :: NullOrUndefined (ApplicationName)
  , "NewApplicationName'" :: NullOrUndefined (ApplicationName)
  }


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


-- | <p>Represents the output of an UpdateDeploymentGroup operation.</p>
newtype UpdateDeploymentGroupOutput = UpdateDeploymentGroupOutput 
  { "HooksNotCleanedUp'" :: NullOrUndefined (AutoScalingGroupList)
  }


newtype Value = Value String


newtype VersionId = VersionId String


newtype WaitTimeInMins = WaitTimeInMins Int
