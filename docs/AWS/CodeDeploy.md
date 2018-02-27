## Module AWS.CodeDeploy

<fullname>AWS CodeDeploy</fullname> <p>AWS CodeDeploy is a deployment service that automates application deployments to Amazon EC2 instances, on-premises instances running in your own facility, or serverless AWS Lambda functions.</p> <p>You can deploy a nearly unlimited variety of application content, such as an updated Lambda function, code, web and configuration files, executables, packages, scripts, multimedia files, and so on. AWS CodeDeploy can deploy application content stored in Amazon S3 buckets, GitHub repositories, or Bitbucket repositories. You do not need to make changes to your existing code before you can use AWS CodeDeploy.</p> <p>AWS CodeDeploy makes it easier for you to rapidly release new features, helps you avoid downtime during application deployment, and handles the complexity of updating your applications, without many of the risks associated with error-prone manual deployments.</p> <p> <b>AWS CodeDeploy Components</b> </p> <p>Use the information in this guide to help you work with the following AWS CodeDeploy components:</p> <ul> <li> <p> <b>Application</b>: A name that uniquely identifies the application you want to deploy. AWS CodeDeploy uses this name, which functions as a container, to ensure the correct combination of revision, deployment configuration, and deployment group are referenced during a deployment.</p> </li> <li> <p> <b>Deployment group</b>: A set of individual instances or CodeDeploy Lambda applications. A Lambda deployment group contains a group of applications. An EC2/On-premises deployment group contains individually tagged instances, Amazon EC2 instances in Auto Scaling groups, or both. </p> </li> <li> <p> <b>Deployment configuration</b>: A set of deployment rules and deployment success and failure conditions used by AWS CodeDeploy during a deployment.</p> </li> <li> <p> <b>Deployment</b>: The process and the components used in the process of updating a Lambda function or of installing content on one or more instances. </p> </li> <li> <p> <b>Application revisions</b>: For an AWS Lambda deployment, this is an AppSpec file that specifies the Lambda function to update and one or more functions to validate deployment lifecycle events. For an EC2/On-premises deployment, this is an archive file containing source content—source code, web pages, executable files, and deployment scripts—along with an AppSpec file. Revisions are stored in Amazon S3 buckets or GitHub repositories. For Amazon S3, a revision is uniquely identified by its Amazon S3 object key and its ETag, version, or both. For GitHub, a revision is uniquely identified by its commit ID.</p> </li> </ul> <p>This guide also contains information to help you get details about the instances in your deployments, to make on-premises instances available for AWS CodeDeploy deployments, and to get details about a Lambda function deployment.</p> <p> <b>AWS CodeDeploy Information Resources</b> </p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/codedeploy/latest/userguide">AWS CodeDeploy User Guide</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/codedeploy/latest/APIReference/">AWS CodeDeploy API Reference Guide</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/cli/latest/reference/deploy/index.html">AWS CLI Reference for AWS CodeDeploy</a> </p> </li> <li> <p> <a href="https://forums.aws.amazon.com/forum.jspa?forumID=179">AWS CodeDeploy Developer Forum</a> </p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addTagsToOnPremisesInstances`

``` purescript
addTagsToOnPremisesInstances :: forall eff. AddTagsToOnPremisesInstancesInput -> Aff (err :: RequestError | eff) Unit
```

<p>Adds tags to on-premises instances.</p>

#### `batchGetApplicationRevisions`

``` purescript
batchGetApplicationRevisions :: forall eff. BatchGetApplicationRevisionsInput -> Aff (err :: RequestError | eff) BatchGetApplicationRevisionsOutput
```

<p>Gets information about one or more application revisions.</p>

#### `batchGetApplications`

``` purescript
batchGetApplications :: forall eff. BatchGetApplicationsInput -> Aff (err :: RequestError | eff) BatchGetApplicationsOutput
```

<p>Gets information about one or more applications.</p>

#### `batchGetDeploymentGroups`

``` purescript
batchGetDeploymentGroups :: forall eff. BatchGetDeploymentGroupsInput -> Aff (err :: RequestError | eff) BatchGetDeploymentGroupsOutput
```

<p>Gets information about one or more deployment groups.</p>

#### `batchGetDeploymentInstances`

``` purescript
batchGetDeploymentInstances :: forall eff. BatchGetDeploymentInstancesInput -> Aff (err :: RequestError | eff) BatchGetDeploymentInstancesOutput
```

<p>Gets information about one or more instance that are part of a deployment group.</p>

#### `batchGetDeployments`

``` purescript
batchGetDeployments :: forall eff. BatchGetDeploymentsInput -> Aff (err :: RequestError | eff) BatchGetDeploymentsOutput
```

<p>Gets information about one or more deployments.</p>

#### `batchGetOnPremisesInstances`

``` purescript
batchGetOnPremisesInstances :: forall eff. BatchGetOnPremisesInstancesInput -> Aff (err :: RequestError | eff) BatchGetOnPremisesInstancesOutput
```

<p>Gets information about one or more on-premises instances.</p>

#### `continueDeployment`

``` purescript
continueDeployment :: forall eff. ContinueDeploymentInput -> Aff (err :: RequestError | eff) Unit
```

<p>For a blue/green deployment, starts the process of rerouting traffic from instances in the original environment to instances in the replacement environment without waiting for a specified wait time to elapse. (Traffic rerouting, which is achieved by registering instances in the replacement environment with the load balancer, can start as soon as all instances have a status of Ready.) </p>

#### `createApplication`

``` purescript
createApplication :: forall eff. CreateApplicationInput -> Aff (err :: RequestError | eff) CreateApplicationOutput
```

<p>Creates an application.</p>

#### `createDeployment`

``` purescript
createDeployment :: forall eff. CreateDeploymentInput -> Aff (err :: RequestError | eff) CreateDeploymentOutput
```

<p>Deploys an application revision through the specified deployment group.</p>

#### `createDeploymentConfig`

``` purescript
createDeploymentConfig :: forall eff. CreateDeploymentConfigInput -> Aff (err :: RequestError | eff) CreateDeploymentConfigOutput
```

<p>Creates a deployment configuration.</p>

#### `createDeploymentGroup`

``` purescript
createDeploymentGroup :: forall eff. CreateDeploymentGroupInput -> Aff (err :: RequestError | eff) CreateDeploymentGroupOutput
```

<p>Creates a deployment group to which application revisions will be deployed.</p>

#### `deleteApplication`

``` purescript
deleteApplication :: forall eff. DeleteApplicationInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes an application.</p>

#### `deleteDeploymentConfig`

``` purescript
deleteDeploymentConfig :: forall eff. DeleteDeploymentConfigInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a deployment configuration.</p> <note> <p>A deployment configuration cannot be deleted if it is currently in use. Predefined configurations cannot be deleted.</p> </note>

#### `deleteDeploymentGroup`

``` purescript
deleteDeploymentGroup :: forall eff. DeleteDeploymentGroupInput -> Aff (err :: RequestError | eff) DeleteDeploymentGroupOutput
```

<p>Deletes a deployment group.</p>

#### `deleteGitHubAccountToken`

``` purescript
deleteGitHubAccountToken :: forall eff. DeleteGitHubAccountTokenInput -> Aff (err :: RequestError | eff) DeleteGitHubAccountTokenOutput
```

<p>Deletes a GitHub account connection.</p>

#### `deregisterOnPremisesInstance`

``` purescript
deregisterOnPremisesInstance :: forall eff. DeregisterOnPremisesInstanceInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deregisters an on-premises instance.</p>

#### `getApplication`

``` purescript
getApplication :: forall eff. GetApplicationInput -> Aff (err :: RequestError | eff) GetApplicationOutput
```

<p>Gets information about an application.</p>

#### `getApplicationRevision`

``` purescript
getApplicationRevision :: forall eff. GetApplicationRevisionInput -> Aff (err :: RequestError | eff) GetApplicationRevisionOutput
```

<p>Gets information about an application revision.</p>

#### `getDeployment`

``` purescript
getDeployment :: forall eff. GetDeploymentInput -> Aff (err :: RequestError | eff) GetDeploymentOutput
```

<p>Gets information about a deployment.</p>

#### `getDeploymentConfig`

``` purescript
getDeploymentConfig :: forall eff. GetDeploymentConfigInput -> Aff (err :: RequestError | eff) GetDeploymentConfigOutput
```

<p>Gets information about a deployment configuration.</p>

#### `getDeploymentGroup`

``` purescript
getDeploymentGroup :: forall eff. GetDeploymentGroupInput -> Aff (err :: RequestError | eff) GetDeploymentGroupOutput
```

<p>Gets information about a deployment group.</p>

#### `getDeploymentInstance`

``` purescript
getDeploymentInstance :: forall eff. GetDeploymentInstanceInput -> Aff (err :: RequestError | eff) GetDeploymentInstanceOutput
```

<p>Gets information about an instance as part of a deployment.</p>

#### `getOnPremisesInstance`

``` purescript
getOnPremisesInstance :: forall eff. GetOnPremisesInstanceInput -> Aff (err :: RequestError | eff) GetOnPremisesInstanceOutput
```

<p>Gets information about an on-premises instance.</p>

#### `listApplicationRevisions`

``` purescript
listApplicationRevisions :: forall eff. ListApplicationRevisionsInput -> Aff (err :: RequestError | eff) ListApplicationRevisionsOutput
```

<p>Lists information about revisions for an application.</p>

#### `listApplications`

``` purescript
listApplications :: forall eff. ListApplicationsInput -> Aff (err :: RequestError | eff) ListApplicationsOutput
```

<p>Lists the applications registered with the applicable IAM user or AWS account.</p>

#### `listDeploymentConfigs`

``` purescript
listDeploymentConfigs :: forall eff. ListDeploymentConfigsInput -> Aff (err :: RequestError | eff) ListDeploymentConfigsOutput
```

<p>Lists the deployment configurations with the applicable IAM user or AWS account.</p>

#### `listDeploymentGroups`

``` purescript
listDeploymentGroups :: forall eff. ListDeploymentGroupsInput -> Aff (err :: RequestError | eff) ListDeploymentGroupsOutput
```

<p>Lists the deployment groups for an application registered with the applicable IAM user or AWS account.</p>

#### `listDeploymentInstances`

``` purescript
listDeploymentInstances :: forall eff. ListDeploymentInstancesInput -> Aff (err :: RequestError | eff) ListDeploymentInstancesOutput
```

<p>Lists the instance for a deployment associated with the applicable IAM user or AWS account.</p>

#### `listDeployments`

``` purescript
listDeployments :: forall eff. ListDeploymentsInput -> Aff (err :: RequestError | eff) ListDeploymentsOutput
```

<p>Lists the deployments in a deployment group for an application registered with the applicable IAM user or AWS account.</p>

#### `listGitHubAccountTokenNames`

``` purescript
listGitHubAccountTokenNames :: forall eff. ListGitHubAccountTokenNamesInput -> Aff (err :: RequestError | eff) ListGitHubAccountTokenNamesOutput
```

<p>Lists the names of stored connections to GitHub accounts.</p>

#### `listOnPremisesInstances`

``` purescript
listOnPremisesInstances :: forall eff. ListOnPremisesInstancesInput -> Aff (err :: RequestError | eff) ListOnPremisesInstancesOutput
```

<p>Gets a list of names for one or more on-premises instances.</p> <p>Unless otherwise specified, both registered and deregistered on-premises instance names will be listed. To list only registered or deregistered on-premises instance names, use the registration status parameter.</p>

#### `putLifecycleEventHookExecutionStatus`

``` purescript
putLifecycleEventHookExecutionStatus :: forall eff. PutLifecycleEventHookExecutionStatusInput -> Aff (err :: RequestError | eff) PutLifecycleEventHookExecutionStatusOutput
```

<p>Sets the result of a Lambda validation function. The function validates one or both lifecycle events (<code>BeforeAllowTraffic</code> and <code>AfterAllowTraffic</code>) and returns <code>Succeeded</code> or <code>Failed</code>.</p>

#### `registerApplicationRevision`

``` purescript
registerApplicationRevision :: forall eff. RegisterApplicationRevisionInput -> Aff (err :: RequestError | eff) Unit
```

<p>Registers with AWS CodeDeploy a revision for the specified application.</p>

#### `registerOnPremisesInstance`

``` purescript
registerOnPremisesInstance :: forall eff. RegisterOnPremisesInstanceInput -> Aff (err :: RequestError | eff) Unit
```

<p>Registers an on-premises instance.</p> <note> <p>Only one IAM ARN (an IAM session ARN or IAM user ARN) is supported in the request. You cannot use both.</p> </note>

#### `removeTagsFromOnPremisesInstances`

``` purescript
removeTagsFromOnPremisesInstances :: forall eff. RemoveTagsFromOnPremisesInstancesInput -> Aff (err :: RequestError | eff) Unit
```

<p>Removes one or more tags from one or more on-premises instances.</p>

#### `skipWaitTimeForInstanceTermination`

``` purescript
skipWaitTimeForInstanceTermination :: forall eff. SkipWaitTimeForInstanceTerminationInput -> Aff (err :: RequestError | eff) Unit
```

<p>In a blue/green deployment, overrides any specified wait time and starts terminating instances immediately after the traffic routing is completed.</p>

#### `stopDeployment`

``` purescript
stopDeployment :: forall eff. StopDeploymentInput -> Aff (err :: RequestError | eff) StopDeploymentOutput
```

<p>Attempts to stop an ongoing deployment.</p>

#### `updateApplication`

``` purescript
updateApplication :: forall eff. UpdateApplicationInput -> Aff (err :: RequestError | eff) Unit
```

<p>Changes the name of an application.</p>

#### `updateDeploymentGroup`

``` purescript
updateDeploymentGroup :: forall eff. UpdateDeploymentGroupInput -> Aff (err :: RequestError | eff) UpdateDeploymentGroupOutput
```

<p>Changes information about a deployment group.</p>

#### `AddTagsToOnPremisesInstancesInput`

``` purescript
newtype AddTagsToOnPremisesInstancesInput
  = AddTagsToOnPremisesInstancesInput { "Tags'" :: TagList, "InstanceNames'" :: InstanceNameList }
```

<p>Represents the input of, and adds tags to, an on-premises instance operation.</p>

##### Instances
``` purescript
Newtype AddTagsToOnPremisesInstancesInput _
```

#### `AdditionalDeploymentStatusInfo`

``` purescript
newtype AdditionalDeploymentStatusInfo
  = AdditionalDeploymentStatusInfo String
```

##### Instances
``` purescript
Newtype AdditionalDeploymentStatusInfo _
```

#### `Alarm`

``` purescript
newtype Alarm
  = Alarm { "Name'" :: NullOrUndefined (AlarmName) }
```

<p>Information about an alarm.</p>

##### Instances
``` purescript
Newtype Alarm _
```

#### `AlarmConfiguration`

``` purescript
newtype AlarmConfiguration
  = AlarmConfiguration { "Enabled'" :: NullOrUndefined (Boolean), "IgnorePollAlarmFailure'" :: NullOrUndefined (Boolean), "Alarms'" :: NullOrUndefined (AlarmList) }
```

<p>Information about alarms associated with the deployment group.</p>

##### Instances
``` purescript
Newtype AlarmConfiguration _
```

#### `AlarmList`

``` purescript
newtype AlarmList
  = AlarmList (Array Alarm)
```

##### Instances
``` purescript
Newtype AlarmList _
```

#### `AlarmName`

``` purescript
newtype AlarmName
  = AlarmName String
```

##### Instances
``` purescript
Newtype AlarmName _
```

#### `AlarmsLimitExceededException`

``` purescript
newtype AlarmsLimitExceededException
  = AlarmsLimitExceededException {  }
```

<p>The maximum number of alarms for a deployment group (10) was exceeded.</p>

##### Instances
``` purescript
Newtype AlarmsLimitExceededException _
```

#### `ApplicationAlreadyExistsException`

``` purescript
newtype ApplicationAlreadyExistsException
  = ApplicationAlreadyExistsException {  }
```

<p>An application with the specified name already exists with the applicable IAM user or AWS account.</p>

##### Instances
``` purescript
Newtype ApplicationAlreadyExistsException _
```

#### `ApplicationDoesNotExistException`

``` purescript
newtype ApplicationDoesNotExistException
  = ApplicationDoesNotExistException {  }
```

<p>The application does not exist with the applicable IAM user or AWS account.</p>

##### Instances
``` purescript
Newtype ApplicationDoesNotExistException _
```

#### `ApplicationId`

``` purescript
newtype ApplicationId
  = ApplicationId String
```

##### Instances
``` purescript
Newtype ApplicationId _
```

#### `ApplicationInfo`

``` purescript
newtype ApplicationInfo
  = ApplicationInfo { "ApplicationId'" :: NullOrUndefined (ApplicationId), "ApplicationName'" :: NullOrUndefined (ApplicationName), "CreateTime'" :: NullOrUndefined (Number), "LinkedToGitHub'" :: NullOrUndefined (Boolean), "GitHubAccountName'" :: NullOrUndefined (GitHubAccountTokenName), "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Information about an application.</p>

##### Instances
``` purescript
Newtype ApplicationInfo _
```

#### `ApplicationLimitExceededException`

``` purescript
newtype ApplicationLimitExceededException
  = ApplicationLimitExceededException {  }
```

<p>More applications were attempted to be created than are allowed.</p>

##### Instances
``` purescript
Newtype ApplicationLimitExceededException _
```

#### `ApplicationName`

``` purescript
newtype ApplicationName
  = ApplicationName String
```

##### Instances
``` purescript
Newtype ApplicationName _
```

#### `ApplicationNameRequiredException`

``` purescript
newtype ApplicationNameRequiredException
  = ApplicationNameRequiredException {  }
```

<p>The minimum number of required application names was not specified.</p>

##### Instances
``` purescript
Newtype ApplicationNameRequiredException _
```

#### `ApplicationRevisionSortBy`

``` purescript
newtype ApplicationRevisionSortBy
  = ApplicationRevisionSortBy String
```

##### Instances
``` purescript
Newtype ApplicationRevisionSortBy _
```

#### `ApplicationsInfoList`

``` purescript
newtype ApplicationsInfoList
  = ApplicationsInfoList (Array ApplicationInfo)
```

##### Instances
``` purescript
Newtype ApplicationsInfoList _
```

#### `ApplicationsList`

``` purescript
newtype ApplicationsList
  = ApplicationsList (Array ApplicationName)
```

##### Instances
``` purescript
Newtype ApplicationsList _
```

#### `AutoRollbackConfiguration`

``` purescript
newtype AutoRollbackConfiguration
  = AutoRollbackConfiguration { "Enabled'" :: NullOrUndefined (Boolean), "Events'" :: NullOrUndefined (AutoRollbackEventsList) }
```

<p>Information about a configuration for automatically rolling back to a previous version of an application revision when a deployment doesn't complete successfully.</p>

##### Instances
``` purescript
Newtype AutoRollbackConfiguration _
```

#### `AutoRollbackEvent`

``` purescript
newtype AutoRollbackEvent
  = AutoRollbackEvent String
```

##### Instances
``` purescript
Newtype AutoRollbackEvent _
```

#### `AutoRollbackEventsList`

``` purescript
newtype AutoRollbackEventsList
  = AutoRollbackEventsList (Array AutoRollbackEvent)
```

##### Instances
``` purescript
Newtype AutoRollbackEventsList _
```

#### `AutoScalingGroup`

``` purescript
newtype AutoScalingGroup
  = AutoScalingGroup { "Name'" :: NullOrUndefined (AutoScalingGroupName), "Hook'" :: NullOrUndefined (AutoScalingGroupHook) }
```

<p>Information about an Auto Scaling group.</p>

##### Instances
``` purescript
Newtype AutoScalingGroup _
```

#### `AutoScalingGroupHook`

``` purescript
newtype AutoScalingGroupHook
  = AutoScalingGroupHook String
```

##### Instances
``` purescript
Newtype AutoScalingGroupHook _
```

#### `AutoScalingGroupList`

``` purescript
newtype AutoScalingGroupList
  = AutoScalingGroupList (Array AutoScalingGroup)
```

##### Instances
``` purescript
Newtype AutoScalingGroupList _
```

#### `AutoScalingGroupName`

``` purescript
newtype AutoScalingGroupName
  = AutoScalingGroupName String
```

##### Instances
``` purescript
Newtype AutoScalingGroupName _
```

#### `AutoScalingGroupNameList`

``` purescript
newtype AutoScalingGroupNameList
  = AutoScalingGroupNameList (Array AutoScalingGroupName)
```

##### Instances
``` purescript
Newtype AutoScalingGroupNameList _
```

#### `BatchGetApplicationRevisionsInput`

``` purescript
newtype BatchGetApplicationRevisionsInput
  = BatchGetApplicationRevisionsInput { "ApplicationName'" :: ApplicationName, "Revisions'" :: RevisionLocationList }
```

<p>Represents the input of a BatchGetApplicationRevisions operation.</p>

##### Instances
``` purescript
Newtype BatchGetApplicationRevisionsInput _
```

#### `BatchGetApplicationRevisionsOutput`

``` purescript
newtype BatchGetApplicationRevisionsOutput
  = BatchGetApplicationRevisionsOutput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "ErrorMessage'" :: NullOrUndefined (ErrorMessage), "Revisions'" :: NullOrUndefined (RevisionInfoList) }
```

<p>Represents the output of a BatchGetApplicationRevisions operation.</p>

##### Instances
``` purescript
Newtype BatchGetApplicationRevisionsOutput _
```

#### `BatchGetApplicationsInput`

``` purescript
newtype BatchGetApplicationsInput
  = BatchGetApplicationsInput { "ApplicationNames'" :: ApplicationsList }
```

<p>Represents the input of a BatchGetApplications operation.</p>

##### Instances
``` purescript
Newtype BatchGetApplicationsInput _
```

#### `BatchGetApplicationsOutput`

``` purescript
newtype BatchGetApplicationsOutput
  = BatchGetApplicationsOutput { "ApplicationsInfo'" :: NullOrUndefined (ApplicationsInfoList) }
```

<p>Represents the output of a BatchGetApplications operation.</p>

##### Instances
``` purescript
Newtype BatchGetApplicationsOutput _
```

#### `BatchGetDeploymentGroupsInput`

``` purescript
newtype BatchGetDeploymentGroupsInput
  = BatchGetDeploymentGroupsInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupNames'" :: DeploymentGroupsList }
```

<p>Represents the input of a BatchGetDeploymentGroups operation.</p>

##### Instances
``` purescript
Newtype BatchGetDeploymentGroupsInput _
```

#### `BatchGetDeploymentGroupsOutput`

``` purescript
newtype BatchGetDeploymentGroupsOutput
  = BatchGetDeploymentGroupsOutput { "DeploymentGroupsInfo'" :: NullOrUndefined (DeploymentGroupInfoList), "ErrorMessage'" :: NullOrUndefined (ErrorMessage) }
```

<p>Represents the output of a BatchGetDeploymentGroups operation.</p>

##### Instances
``` purescript
Newtype BatchGetDeploymentGroupsOutput _
```

#### `BatchGetDeploymentInstancesInput`

``` purescript
newtype BatchGetDeploymentInstancesInput
  = BatchGetDeploymentInstancesInput { "DeploymentId'" :: DeploymentId, "InstanceIds'" :: InstancesList }
```

<p>Represents the input of a BatchGetDeploymentInstances operation.</p>

##### Instances
``` purescript
Newtype BatchGetDeploymentInstancesInput _
```

#### `BatchGetDeploymentInstancesOutput`

``` purescript
newtype BatchGetDeploymentInstancesOutput
  = BatchGetDeploymentInstancesOutput { "InstancesSummary'" :: NullOrUndefined (InstanceSummaryList), "ErrorMessage'" :: NullOrUndefined (ErrorMessage) }
```

<p>Represents the output of a BatchGetDeploymentInstances operation.</p>

##### Instances
``` purescript
Newtype BatchGetDeploymentInstancesOutput _
```

#### `BatchGetDeploymentsInput`

``` purescript
newtype BatchGetDeploymentsInput
  = BatchGetDeploymentsInput { "DeploymentIds'" :: DeploymentsList }
```

<p>Represents the input of a BatchGetDeployments operation.</p>

##### Instances
``` purescript
Newtype BatchGetDeploymentsInput _
```

#### `BatchGetDeploymentsOutput`

``` purescript
newtype BatchGetDeploymentsOutput
  = BatchGetDeploymentsOutput { "DeploymentsInfo'" :: NullOrUndefined (DeploymentsInfoList) }
```

<p>Represents the output of a BatchGetDeployments operation.</p>

##### Instances
``` purescript
Newtype BatchGetDeploymentsOutput _
```

#### `BatchGetOnPremisesInstancesInput`

``` purescript
newtype BatchGetOnPremisesInstancesInput
  = BatchGetOnPremisesInstancesInput { "InstanceNames'" :: InstanceNameList }
```

<p>Represents the input of a BatchGetOnPremisesInstances operation.</p>

##### Instances
``` purescript
Newtype BatchGetOnPremisesInstancesInput _
```

#### `BatchGetOnPremisesInstancesOutput`

``` purescript
newtype BatchGetOnPremisesInstancesOutput
  = BatchGetOnPremisesInstancesOutput { "InstanceInfos'" :: NullOrUndefined (InstanceInfoList) }
```

<p>Represents the output of a BatchGetOnPremisesInstances operation.</p>

##### Instances
``` purescript
Newtype BatchGetOnPremisesInstancesOutput _
```

#### `BatchLimitExceededException`

``` purescript
newtype BatchLimitExceededException
  = BatchLimitExceededException {  }
```

<p>The maximum number of names or IDs allowed for this request (100) was exceeded.</p>

##### Instances
``` purescript
Newtype BatchLimitExceededException _
```

#### `BlueGreenDeploymentConfiguration`

``` purescript
newtype BlueGreenDeploymentConfiguration
  = BlueGreenDeploymentConfiguration { "TerminateBlueInstancesOnDeploymentSuccess'" :: NullOrUndefined (BlueInstanceTerminationOption), "DeploymentReadyOption'" :: NullOrUndefined (DeploymentReadyOption), "GreenFleetProvisioningOption'" :: NullOrUndefined (GreenFleetProvisioningOption) }
```

<p>Information about blue/green deployment options for a deployment group.</p>

##### Instances
``` purescript
Newtype BlueGreenDeploymentConfiguration _
```

#### `BlueInstanceTerminationOption`

``` purescript
newtype BlueInstanceTerminationOption
  = BlueInstanceTerminationOption { "Action'" :: NullOrUndefined (InstanceAction), "TerminationWaitTimeInMinutes'" :: NullOrUndefined (Duration) }
```

<p>Information about whether instances in the original environment are terminated when a blue/green deployment is successful.</p>

##### Instances
``` purescript
Newtype BlueInstanceTerminationOption _
```

#### `BucketNameFilterRequiredException`

``` purescript
newtype BucketNameFilterRequiredException
  = BucketNameFilterRequiredException {  }
```

<p>A bucket name is required, but was not provided.</p>

##### Instances
``` purescript
Newtype BucketNameFilterRequiredException _
```

#### `BundleType`

``` purescript
newtype BundleType
  = BundleType String
```

##### Instances
``` purescript
Newtype BundleType _
```

#### `CommitId`

``` purescript
newtype CommitId
  = CommitId String
```

##### Instances
``` purescript
Newtype CommitId _
```

#### `ComputePlatform`

``` purescript
newtype ComputePlatform
  = ComputePlatform String
```

##### Instances
``` purescript
Newtype ComputePlatform _
```

#### `ContinueDeploymentInput`

``` purescript
newtype ContinueDeploymentInput
  = ContinueDeploymentInput { "DeploymentId'" :: NullOrUndefined (DeploymentId) }
```

##### Instances
``` purescript
Newtype ContinueDeploymentInput _
```

#### `CreateApplicationInput`

``` purescript
newtype CreateApplicationInput
  = CreateApplicationInput { "ApplicationName'" :: ApplicationName, "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Represents the input of a CreateApplication operation.</p>

##### Instances
``` purescript
Newtype CreateApplicationInput _
```

#### `CreateApplicationOutput`

``` purescript
newtype CreateApplicationOutput
  = CreateApplicationOutput { "ApplicationId'" :: NullOrUndefined (ApplicationId) }
```

<p>Represents the output of a CreateApplication operation.</p>

##### Instances
``` purescript
Newtype CreateApplicationOutput _
```

#### `CreateDeploymentConfigInput`

``` purescript
newtype CreateDeploymentConfigInput
  = CreateDeploymentConfigInput { "DeploymentConfigName'" :: DeploymentConfigName, "MinimumHealthyHosts'" :: NullOrUndefined (MinimumHealthyHosts), "TrafficRoutingConfig'" :: NullOrUndefined (TrafficRoutingConfig), "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Represents the input of a CreateDeploymentConfig operation.</p>

##### Instances
``` purescript
Newtype CreateDeploymentConfigInput _
```

#### `CreateDeploymentConfigOutput`

``` purescript
newtype CreateDeploymentConfigOutput
  = CreateDeploymentConfigOutput { "DeploymentConfigId'" :: NullOrUndefined (DeploymentConfigId) }
```

<p>Represents the output of a CreateDeploymentConfig operation.</p>

##### Instances
``` purescript
Newtype CreateDeploymentConfigOutput _
```

#### `CreateDeploymentGroupInput`

``` purescript
newtype CreateDeploymentGroupInput
  = CreateDeploymentGroupInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupName'" :: DeploymentGroupName, "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList), "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList), "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList), "ServiceRoleArn'" :: Role, "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList), "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle), "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration), "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo), "Ec2TagSet'" :: NullOrUndefined (EC2TagSet), "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet) }
```

<p>Represents the input of a CreateDeploymentGroup operation.</p>

##### Instances
``` purescript
Newtype CreateDeploymentGroupInput _
```

#### `CreateDeploymentGroupOutput`

``` purescript
newtype CreateDeploymentGroupOutput
  = CreateDeploymentGroupOutput { "DeploymentGroupId'" :: NullOrUndefined (DeploymentGroupId) }
```

<p>Represents the output of a CreateDeploymentGroup operation.</p>

##### Instances
``` purescript
Newtype CreateDeploymentGroupOutput _
```

#### `CreateDeploymentInput`

``` purescript
newtype CreateDeploymentInput
  = CreateDeploymentInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "Revision'" :: NullOrUndefined (RevisionLocation), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "Description'" :: NullOrUndefined (Description), "IgnoreApplicationStopFailures'" :: NullOrUndefined (Boolean), "TargetInstances'" :: NullOrUndefined (TargetInstances), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "UpdateOutdatedInstancesOnly'" :: NullOrUndefined (Boolean), "FileExistsBehavior'" :: NullOrUndefined (FileExistsBehavior) }
```

<p>Represents the input of a CreateDeployment operation.</p>

##### Instances
``` purescript
Newtype CreateDeploymentInput _
```

#### `CreateDeploymentOutput`

``` purescript
newtype CreateDeploymentOutput
  = CreateDeploymentOutput { "DeploymentId'" :: NullOrUndefined (DeploymentId) }
```

<p>Represents the output of a CreateDeployment operation.</p>

##### Instances
``` purescript
Newtype CreateDeploymentOutput _
```

#### `DeleteApplicationInput`

``` purescript
newtype DeleteApplicationInput
  = DeleteApplicationInput { "ApplicationName'" :: ApplicationName }
```

<p>Represents the input of a DeleteApplication operation.</p>

##### Instances
``` purescript
Newtype DeleteApplicationInput _
```

#### `DeleteDeploymentConfigInput`

``` purescript
newtype DeleteDeploymentConfigInput
  = DeleteDeploymentConfigInput { "DeploymentConfigName'" :: DeploymentConfigName }
```

<p>Represents the input of a DeleteDeploymentConfig operation.</p>

##### Instances
``` purescript
Newtype DeleteDeploymentConfigInput _
```

#### `DeleteDeploymentGroupInput`

``` purescript
newtype DeleteDeploymentGroupInput
  = DeleteDeploymentGroupInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupName'" :: DeploymentGroupName }
```

<p>Represents the input of a DeleteDeploymentGroup operation.</p>

##### Instances
``` purescript
Newtype DeleteDeploymentGroupInput _
```

#### `DeleteDeploymentGroupOutput`

``` purescript
newtype DeleteDeploymentGroupOutput
  = DeleteDeploymentGroupOutput { "HooksNotCleanedUp'" :: NullOrUndefined (AutoScalingGroupList) }
```

<p>Represents the output of a DeleteDeploymentGroup operation.</p>

##### Instances
``` purescript
Newtype DeleteDeploymentGroupOutput _
```

#### `DeleteGitHubAccountTokenInput`

``` purescript
newtype DeleteGitHubAccountTokenInput
  = DeleteGitHubAccountTokenInput { "TokenName'" :: NullOrUndefined (GitHubAccountTokenName) }
```

<p>Represents the input of a DeleteGitHubAccount operation.</p>

##### Instances
``` purescript
Newtype DeleteGitHubAccountTokenInput _
```

#### `DeleteGitHubAccountTokenOutput`

``` purescript
newtype DeleteGitHubAccountTokenOutput
  = DeleteGitHubAccountTokenOutput { "TokenName'" :: NullOrUndefined (GitHubAccountTokenName) }
```

<p>Represents the output of a DeleteGitHubAccountToken operation.</p>

##### Instances
``` purescript
Newtype DeleteGitHubAccountTokenOutput _
```

#### `DeploymentAlreadyCompletedException`

``` purescript
newtype DeploymentAlreadyCompletedException
  = DeploymentAlreadyCompletedException {  }
```

<p>The deployment is already complete.</p>

##### Instances
``` purescript
Newtype DeploymentAlreadyCompletedException _
```

#### `DeploymentConfigAlreadyExistsException`

``` purescript
newtype DeploymentConfigAlreadyExistsException
  = DeploymentConfigAlreadyExistsException {  }
```

<p>A deployment configuration with the specified name already exists with the applicable IAM user or AWS account.</p>

##### Instances
``` purescript
Newtype DeploymentConfigAlreadyExistsException _
```

#### `DeploymentConfigDoesNotExistException`

``` purescript
newtype DeploymentConfigDoesNotExistException
  = DeploymentConfigDoesNotExistException {  }
```

<p>The deployment configuration does not exist with the applicable IAM user or AWS account.</p>

##### Instances
``` purescript
Newtype DeploymentConfigDoesNotExistException _
```

#### `DeploymentConfigId`

``` purescript
newtype DeploymentConfigId
  = DeploymentConfigId String
```

##### Instances
``` purescript
Newtype DeploymentConfigId _
```

#### `DeploymentConfigInUseException`

``` purescript
newtype DeploymentConfigInUseException
  = DeploymentConfigInUseException {  }
```

<p>The deployment configuration is still in use.</p>

##### Instances
``` purescript
Newtype DeploymentConfigInUseException _
```

#### `DeploymentConfigInfo`

``` purescript
newtype DeploymentConfigInfo
  = DeploymentConfigInfo { "DeploymentConfigId'" :: NullOrUndefined (DeploymentConfigId), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "MinimumHealthyHosts'" :: NullOrUndefined (MinimumHealthyHosts), "CreateTime'" :: NullOrUndefined (Number), "ComputePlatform'" :: NullOrUndefined (ComputePlatform), "TrafficRoutingConfig'" :: NullOrUndefined (TrafficRoutingConfig) }
```

<p>Information about a deployment configuration.</p>

##### Instances
``` purescript
Newtype DeploymentConfigInfo _
```

#### `DeploymentConfigLimitExceededException`

``` purescript
newtype DeploymentConfigLimitExceededException
  = DeploymentConfigLimitExceededException {  }
```

<p>The deployment configurations limit was exceeded.</p>

##### Instances
``` purescript
Newtype DeploymentConfigLimitExceededException _
```

#### `DeploymentConfigName`

``` purescript
newtype DeploymentConfigName
  = DeploymentConfigName String
```

##### Instances
``` purescript
Newtype DeploymentConfigName _
```

#### `DeploymentConfigNameRequiredException`

``` purescript
newtype DeploymentConfigNameRequiredException
  = DeploymentConfigNameRequiredException {  }
```

<p>The deployment configuration name was not specified.</p>

##### Instances
``` purescript
Newtype DeploymentConfigNameRequiredException _
```

#### `DeploymentConfigsList`

``` purescript
newtype DeploymentConfigsList
  = DeploymentConfigsList (Array DeploymentConfigName)
```

##### Instances
``` purescript
Newtype DeploymentConfigsList _
```

#### `DeploymentCreator`

``` purescript
newtype DeploymentCreator
  = DeploymentCreator String
```

##### Instances
``` purescript
Newtype DeploymentCreator _
```

#### `DeploymentDoesNotExistException`

``` purescript
newtype DeploymentDoesNotExistException
  = DeploymentDoesNotExistException {  }
```

<p>The deployment does not exist with the applicable IAM user or AWS account.</p>

##### Instances
``` purescript
Newtype DeploymentDoesNotExistException _
```

#### `DeploymentGroupAlreadyExistsException`

``` purescript
newtype DeploymentGroupAlreadyExistsException
  = DeploymentGroupAlreadyExistsException {  }
```

<p>A deployment group with the specified name already exists with the applicable IAM user or AWS account.</p>

##### Instances
``` purescript
Newtype DeploymentGroupAlreadyExistsException _
```

#### `DeploymentGroupDoesNotExistException`

``` purescript
newtype DeploymentGroupDoesNotExistException
  = DeploymentGroupDoesNotExistException {  }
```

<p>The named deployment group does not exist with the applicable IAM user or AWS account.</p>

##### Instances
``` purescript
Newtype DeploymentGroupDoesNotExistException _
```

#### `DeploymentGroupId`

``` purescript
newtype DeploymentGroupId
  = DeploymentGroupId String
```

##### Instances
``` purescript
Newtype DeploymentGroupId _
```

#### `DeploymentGroupInfo`

``` purescript
newtype DeploymentGroupInfo
  = DeploymentGroupInfo { "ApplicationName'" :: NullOrUndefined (ApplicationName), "DeploymentGroupId'" :: NullOrUndefined (DeploymentGroupId), "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList), "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList), "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupList), "ServiceRoleArn'" :: NullOrUndefined (Role), "TargetRevision'" :: NullOrUndefined (RevisionLocation), "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList), "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle), "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration), "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo), "LastSuccessfulDeployment'" :: NullOrUndefined (LastDeploymentInfo), "LastAttemptedDeployment'" :: NullOrUndefined (LastDeploymentInfo), "Ec2TagSet'" :: NullOrUndefined (EC2TagSet), "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet), "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Information about a deployment group.</p>

##### Instances
``` purescript
Newtype DeploymentGroupInfo _
```

#### `DeploymentGroupInfoList`

``` purescript
newtype DeploymentGroupInfoList
  = DeploymentGroupInfoList (Array DeploymentGroupInfo)
```

##### Instances
``` purescript
Newtype DeploymentGroupInfoList _
```

#### `DeploymentGroupLimitExceededException`

``` purescript
newtype DeploymentGroupLimitExceededException
  = DeploymentGroupLimitExceededException {  }
```

<p> The deployment groups limit was exceeded.</p>

##### Instances
``` purescript
Newtype DeploymentGroupLimitExceededException _
```

#### `DeploymentGroupName`

``` purescript
newtype DeploymentGroupName
  = DeploymentGroupName String
```

##### Instances
``` purescript
Newtype DeploymentGroupName _
```

#### `DeploymentGroupNameRequiredException`

``` purescript
newtype DeploymentGroupNameRequiredException
  = DeploymentGroupNameRequiredException {  }
```

<p>The deployment group name was not specified.</p>

##### Instances
``` purescript
Newtype DeploymentGroupNameRequiredException _
```

#### `DeploymentGroupsList`

``` purescript
newtype DeploymentGroupsList
  = DeploymentGroupsList (Array DeploymentGroupName)
```

##### Instances
``` purescript
Newtype DeploymentGroupsList _
```

#### `DeploymentId`

``` purescript
newtype DeploymentId
  = DeploymentId String
```

##### Instances
``` purescript
Newtype DeploymentId _
```

#### `DeploymentIdRequiredException`

``` purescript
newtype DeploymentIdRequiredException
  = DeploymentIdRequiredException {  }
```

<p>At least one deployment ID must be specified.</p>

##### Instances
``` purescript
Newtype DeploymentIdRequiredException _
```

#### `DeploymentInfo`

``` purescript
newtype DeploymentInfo
  = DeploymentInfo { "ApplicationName'" :: NullOrUndefined (ApplicationName), "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "DeploymentId'" :: NullOrUndefined (DeploymentId), "PreviousRevision'" :: NullOrUndefined (RevisionLocation), "Revision'" :: NullOrUndefined (RevisionLocation), "Status'" :: NullOrUndefined (DeploymentStatus), "ErrorInformation'" :: NullOrUndefined (ErrorInformation), "CreateTime'" :: NullOrUndefined (Number), "StartTime'" :: NullOrUndefined (Number), "CompleteTime'" :: NullOrUndefined (Number), "DeploymentOverview'" :: NullOrUndefined (DeploymentOverview), "Description'" :: NullOrUndefined (Description), "Creator'" :: NullOrUndefined (DeploymentCreator), "IgnoreApplicationStopFailures'" :: NullOrUndefined (Boolean), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "UpdateOutdatedInstancesOnly'" :: NullOrUndefined (Boolean), "RollbackInfo'" :: NullOrUndefined (RollbackInfo), "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle), "TargetInstances'" :: NullOrUndefined (TargetInstances), "InstanceTerminationWaitTimeStarted'" :: NullOrUndefined (Boolean), "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration), "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo), "AdditionalDeploymentStatusInfo'" :: NullOrUndefined (AdditionalDeploymentStatusInfo), "FileExistsBehavior'" :: NullOrUndefined (FileExistsBehavior), "DeploymentStatusMessages'" :: NullOrUndefined (DeploymentStatusMessageList), "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Information about a deployment.</p>

##### Instances
``` purescript
Newtype DeploymentInfo _
```

#### `DeploymentIsNotInReadyStateException`

``` purescript
newtype DeploymentIsNotInReadyStateException
  = DeploymentIsNotInReadyStateException {  }
```

<p>The deployment does not have a status of Ready and can't continue yet.</p>

##### Instances
``` purescript
Newtype DeploymentIsNotInReadyStateException _
```

#### `DeploymentLimitExceededException`

``` purescript
newtype DeploymentLimitExceededException
  = DeploymentLimitExceededException {  }
```

<p>The number of allowed deployments was exceeded.</p>

##### Instances
``` purescript
Newtype DeploymentLimitExceededException _
```

#### `DeploymentNotStartedException`

``` purescript
newtype DeploymentNotStartedException
  = DeploymentNotStartedException {  }
```

<p>The specified deployment has not started.</p>

##### Instances
``` purescript
Newtype DeploymentNotStartedException _
```

#### `DeploymentOption`

``` purescript
newtype DeploymentOption
  = DeploymentOption String
```

##### Instances
``` purescript
Newtype DeploymentOption _
```

#### `DeploymentOverview`

``` purescript
newtype DeploymentOverview
  = DeploymentOverview { "Pending" :: NullOrUndefined (InstanceCount), "InProgress" :: NullOrUndefined (InstanceCount), "Succeeded" :: NullOrUndefined (InstanceCount), "Failed" :: NullOrUndefined (InstanceCount), "Skipped" :: NullOrUndefined (InstanceCount), "Ready" :: NullOrUndefined (InstanceCount) }
```

<p>Information about the deployment status of the instances in the deployment.</p>

##### Instances
``` purescript
Newtype DeploymentOverview _
```

#### `DeploymentReadyAction`

``` purescript
newtype DeploymentReadyAction
  = DeploymentReadyAction String
```

##### Instances
``` purescript
Newtype DeploymentReadyAction _
```

#### `DeploymentReadyOption`

``` purescript
newtype DeploymentReadyOption
  = DeploymentReadyOption { "ActionOnTimeout'" :: NullOrUndefined (DeploymentReadyAction), "WaitTimeInMinutes'" :: NullOrUndefined (Duration) }
```

<p>Information about how traffic is rerouted to instances in a replacement environment in a blue/green deployment.</p>

##### Instances
``` purescript
Newtype DeploymentReadyOption _
```

#### `DeploymentStatus`

``` purescript
newtype DeploymentStatus
  = DeploymentStatus String
```

##### Instances
``` purescript
Newtype DeploymentStatus _
```

#### `DeploymentStatusList`

``` purescript
newtype DeploymentStatusList
  = DeploymentStatusList (Array DeploymentStatus)
```

##### Instances
``` purescript
Newtype DeploymentStatusList _
```

#### `DeploymentStatusMessageList`

``` purescript
newtype DeploymentStatusMessageList
  = DeploymentStatusMessageList (Array ErrorMessage)
```

##### Instances
``` purescript
Newtype DeploymentStatusMessageList _
```

#### `DeploymentStyle`

``` purescript
newtype DeploymentStyle
  = DeploymentStyle { "DeploymentType'" :: NullOrUndefined (DeploymentType), "DeploymentOption'" :: NullOrUndefined (DeploymentOption) }
```

<p>Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.</p>

##### Instances
``` purescript
Newtype DeploymentStyle _
```

#### `DeploymentType`

``` purescript
newtype DeploymentType
  = DeploymentType String
```

##### Instances
``` purescript
Newtype DeploymentType _
```

#### `DeploymentsInfoList`

``` purescript
newtype DeploymentsInfoList
  = DeploymentsInfoList (Array DeploymentInfo)
```

##### Instances
``` purescript
Newtype DeploymentsInfoList _
```

#### `DeploymentsList`

``` purescript
newtype DeploymentsList
  = DeploymentsList (Array DeploymentId)
```

##### Instances
``` purescript
Newtype DeploymentsList _
```

#### `DeregisterOnPremisesInstanceInput`

``` purescript
newtype DeregisterOnPremisesInstanceInput
  = DeregisterOnPremisesInstanceInput { "InstanceName'" :: InstanceName }
```

<p>Represents the input of a DeregisterOnPremisesInstance operation.</p>

##### Instances
``` purescript
Newtype DeregisterOnPremisesInstanceInput _
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

##### Instances
``` purescript
Newtype Description _
```

#### `DescriptionTooLongException`

``` purescript
newtype DescriptionTooLongException
  = DescriptionTooLongException {  }
```

<p>The description is too long.</p>

##### Instances
``` purescript
Newtype DescriptionTooLongException _
```

#### `Diagnostics`

``` purescript
newtype Diagnostics
  = Diagnostics { "ErrorCode'" :: NullOrUndefined (LifecycleErrorCode), "ScriptName'" :: NullOrUndefined (ScriptName), "Message'" :: NullOrUndefined (LifecycleMessage), "LogTail'" :: NullOrUndefined (LogTail) }
```

<p>Diagnostic information about executable scripts that are part of a deployment.</p>

##### Instances
``` purescript
Newtype Diagnostics _
```

#### `Duration`

``` purescript
newtype Duration
  = Duration Int
```

##### Instances
``` purescript
Newtype Duration _
```

#### `EC2TagFilter`

``` purescript
newtype EC2TagFilter
  = EC2TagFilter { "Key" :: NullOrUndefined (Key), "Value" :: NullOrUndefined (Value), "Type" :: NullOrUndefined (EC2TagFilterType) }
```

<p>Information about an EC2 tag filter.</p>

##### Instances
``` purescript
Newtype EC2TagFilter _
```

#### `EC2TagFilterList`

``` purescript
newtype EC2TagFilterList
  = EC2TagFilterList (Array EC2TagFilter)
```

##### Instances
``` purescript
Newtype EC2TagFilterList _
```

#### `EC2TagFilterType`

``` purescript
newtype EC2TagFilterType
  = EC2TagFilterType String
```

##### Instances
``` purescript
Newtype EC2TagFilterType _
```

#### `EC2TagSet`

``` purescript
newtype EC2TagSet
  = EC2TagSet { "Ec2TagSetList'" :: NullOrUndefined (EC2TagSetList) }
```

<p>Information about groups of EC2 instance tags.</p>

##### Instances
``` purescript
Newtype EC2TagSet _
```

#### `EC2TagSetList`

``` purescript
newtype EC2TagSetList
  = EC2TagSetList (Array EC2TagFilterList)
```

##### Instances
``` purescript
Newtype EC2TagSetList _
```

#### `ELBInfo`

``` purescript
newtype ELBInfo
  = ELBInfo { "Name'" :: NullOrUndefined (ELBName) }
```

<p>Information about a load balancer in Elastic Load Balancing to use in a deployment. Instances are registered directly with a load balancer, and traffic is routed to the load balancer.</p>

##### Instances
``` purescript
Newtype ELBInfo _
```

#### `ELBInfoList`

``` purescript
newtype ELBInfoList
  = ELBInfoList (Array ELBInfo)
```

##### Instances
``` purescript
Newtype ELBInfoList _
```

#### `ELBName`

``` purescript
newtype ELBName
  = ELBName String
```

##### Instances
``` purescript
Newtype ELBName _
```

#### `ETag`

``` purescript
newtype ETag
  = ETag String
```

##### Instances
``` purescript
Newtype ETag _
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

##### Instances
``` purescript
Newtype ErrorCode _
```

#### `ErrorInformation`

``` purescript
newtype ErrorInformation
  = ErrorInformation { "Code'" :: NullOrUndefined (ErrorCode), "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Information about a deployment error.</p>

##### Instances
``` purescript
Newtype ErrorInformation _
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

#### `FileExistsBehavior`

``` purescript
newtype FileExistsBehavior
  = FileExistsBehavior String
```

##### Instances
``` purescript
Newtype FileExistsBehavior _
```

#### `GenericRevisionInfo`

``` purescript
newtype GenericRevisionInfo
  = GenericRevisionInfo { "Description'" :: NullOrUndefined (Description), "DeploymentGroups'" :: NullOrUndefined (DeploymentGroupsList), "FirstUsedTime'" :: NullOrUndefined (Number), "LastUsedTime'" :: NullOrUndefined (Number), "RegisterTime'" :: NullOrUndefined (Number) }
```

<p>Information about an application revision.</p>

##### Instances
``` purescript
Newtype GenericRevisionInfo _
```

#### `GetApplicationInput`

``` purescript
newtype GetApplicationInput
  = GetApplicationInput { "ApplicationName'" :: ApplicationName }
```

<p>Represents the input of a GetApplication operation.</p>

##### Instances
``` purescript
Newtype GetApplicationInput _
```

#### `GetApplicationOutput`

``` purescript
newtype GetApplicationOutput
  = GetApplicationOutput { "Application'" :: NullOrUndefined (ApplicationInfo) }
```

<p>Represents the output of a GetApplication operation.</p>

##### Instances
``` purescript
Newtype GetApplicationOutput _
```

#### `GetApplicationRevisionInput`

``` purescript
newtype GetApplicationRevisionInput
  = GetApplicationRevisionInput { "ApplicationName'" :: ApplicationName, "Revision'" :: RevisionLocation }
```

<p>Represents the input of a GetApplicationRevision operation.</p>

##### Instances
``` purescript
Newtype GetApplicationRevisionInput _
```

#### `GetApplicationRevisionOutput`

``` purescript
newtype GetApplicationRevisionOutput
  = GetApplicationRevisionOutput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "Revision'" :: NullOrUndefined (RevisionLocation), "RevisionInfo'" :: NullOrUndefined (GenericRevisionInfo) }
```

<p>Represents the output of a GetApplicationRevision operation.</p>

##### Instances
``` purescript
Newtype GetApplicationRevisionOutput _
```

#### `GetDeploymentConfigInput`

``` purescript
newtype GetDeploymentConfigInput
  = GetDeploymentConfigInput { "DeploymentConfigName'" :: DeploymentConfigName }
```

<p>Represents the input of a GetDeploymentConfig operation.</p>

##### Instances
``` purescript
Newtype GetDeploymentConfigInput _
```

#### `GetDeploymentConfigOutput`

``` purescript
newtype GetDeploymentConfigOutput
  = GetDeploymentConfigOutput { "DeploymentConfigInfo'" :: NullOrUndefined (DeploymentConfigInfo) }
```

<p>Represents the output of a GetDeploymentConfig operation.</p>

##### Instances
``` purescript
Newtype GetDeploymentConfigOutput _
```

#### `GetDeploymentGroupInput`

``` purescript
newtype GetDeploymentGroupInput
  = GetDeploymentGroupInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupName'" :: DeploymentGroupName }
```

<p>Represents the input of a GetDeploymentGroup operation.</p>

##### Instances
``` purescript
Newtype GetDeploymentGroupInput _
```

#### `GetDeploymentGroupOutput`

``` purescript
newtype GetDeploymentGroupOutput
  = GetDeploymentGroupOutput { "DeploymentGroupInfo'" :: NullOrUndefined (DeploymentGroupInfo) }
```

<p>Represents the output of a GetDeploymentGroup operation.</p>

##### Instances
``` purescript
Newtype GetDeploymentGroupOutput _
```

#### `GetDeploymentInput`

``` purescript
newtype GetDeploymentInput
  = GetDeploymentInput { "DeploymentId'" :: DeploymentId }
```

<p>Represents the input of a GetDeployment operation.</p>

##### Instances
``` purescript
Newtype GetDeploymentInput _
```

#### `GetDeploymentInstanceInput`

``` purescript
newtype GetDeploymentInstanceInput
  = GetDeploymentInstanceInput { "DeploymentId'" :: DeploymentId, "InstanceId'" :: InstanceId }
```

<p>Represents the input of a GetDeploymentInstance operation.</p>

##### Instances
``` purescript
Newtype GetDeploymentInstanceInput _
```

#### `GetDeploymentInstanceOutput`

``` purescript
newtype GetDeploymentInstanceOutput
  = GetDeploymentInstanceOutput { "InstanceSummary'" :: NullOrUndefined (InstanceSummary) }
```

<p>Represents the output of a GetDeploymentInstance operation.</p>

##### Instances
``` purescript
Newtype GetDeploymentInstanceOutput _
```

#### `GetDeploymentOutput`

``` purescript
newtype GetDeploymentOutput
  = GetDeploymentOutput { "DeploymentInfo'" :: NullOrUndefined (DeploymentInfo) }
```

<p>Represents the output of a GetDeployment operation.</p>

##### Instances
``` purescript
Newtype GetDeploymentOutput _
```

#### `GetOnPremisesInstanceInput`

``` purescript
newtype GetOnPremisesInstanceInput
  = GetOnPremisesInstanceInput { "InstanceName'" :: InstanceName }
```

<p>Represents the input of a GetOnPremisesInstance operation.</p>

##### Instances
``` purescript
Newtype GetOnPremisesInstanceInput _
```

#### `GetOnPremisesInstanceOutput`

``` purescript
newtype GetOnPremisesInstanceOutput
  = GetOnPremisesInstanceOutput { "InstanceInfo'" :: NullOrUndefined (InstanceInfo) }
```

<p>Represents the output of a GetOnPremisesInstance operation.</p>

##### Instances
``` purescript
Newtype GetOnPremisesInstanceOutput _
```

#### `GitHubAccountTokenDoesNotExistException`

``` purescript
newtype GitHubAccountTokenDoesNotExistException
  = GitHubAccountTokenDoesNotExistException {  }
```

<p>No GitHub account connection exists with the named specified in the call.</p>

##### Instances
``` purescript
Newtype GitHubAccountTokenDoesNotExistException _
```

#### `GitHubAccountTokenName`

``` purescript
newtype GitHubAccountTokenName
  = GitHubAccountTokenName String
```

##### Instances
``` purescript
Newtype GitHubAccountTokenName _
```

#### `GitHubAccountTokenNameList`

``` purescript
newtype GitHubAccountTokenNameList
  = GitHubAccountTokenNameList (Array GitHubAccountTokenName)
```

##### Instances
``` purescript
Newtype GitHubAccountTokenNameList _
```

#### `GitHubAccountTokenNameRequiredException`

``` purescript
newtype GitHubAccountTokenNameRequiredException
  = GitHubAccountTokenNameRequiredException {  }
```

<p>The call is missing a required GitHub account connection name.</p>

##### Instances
``` purescript
Newtype GitHubAccountTokenNameRequiredException _
```

#### `GitHubLocation`

``` purescript
newtype GitHubLocation
  = GitHubLocation { "Repository'" :: NullOrUndefined (Repository), "CommitId'" :: NullOrUndefined (CommitId) }
```

<p>Information about the location of application artifacts stored in GitHub.</p>

##### Instances
``` purescript
Newtype GitHubLocation _
```

#### `GreenFleetProvisioningAction`

``` purescript
newtype GreenFleetProvisioningAction
  = GreenFleetProvisioningAction String
```

##### Instances
``` purescript
Newtype GreenFleetProvisioningAction _
```

#### `GreenFleetProvisioningOption`

``` purescript
newtype GreenFleetProvisioningOption
  = GreenFleetProvisioningOption { "Action'" :: NullOrUndefined (GreenFleetProvisioningAction) }
```

<p>Information about the instances that belong to the replacement environment in a blue/green deployment.</p>

##### Instances
``` purescript
Newtype GreenFleetProvisioningOption _
```

#### `IamArnRequiredException`

``` purescript
newtype IamArnRequiredException
  = IamArnRequiredException {  }
```

<p>No IAM ARN was included in the request. You must use an IAM session ARN or IAM user ARN in the request.</p>

##### Instances
``` purescript
Newtype IamArnRequiredException _
```

#### `IamSessionArn`

``` purescript
newtype IamSessionArn
  = IamSessionArn String
```

##### Instances
``` purescript
Newtype IamSessionArn _
```

#### `IamSessionArnAlreadyRegisteredException`

``` purescript
newtype IamSessionArnAlreadyRegisteredException
  = IamSessionArnAlreadyRegisteredException {  }
```

<p>The request included an IAM session ARN that has already been used to register a different instance.</p>

##### Instances
``` purescript
Newtype IamSessionArnAlreadyRegisteredException _
```

#### `IamUserArn`

``` purescript
newtype IamUserArn
  = IamUserArn String
```

##### Instances
``` purescript
Newtype IamUserArn _
```

#### `IamUserArnAlreadyRegisteredException`

``` purescript
newtype IamUserArnAlreadyRegisteredException
  = IamUserArnAlreadyRegisteredException {  }
```

<p>The specified IAM user ARN is already registered with an on-premises instance.</p>

##### Instances
``` purescript
Newtype IamUserArnAlreadyRegisteredException _
```

#### `IamUserArnRequiredException`

``` purescript
newtype IamUserArnRequiredException
  = IamUserArnRequiredException {  }
```

<p>An IAM user ARN was not specified.</p>

##### Instances
``` purescript
Newtype IamUserArnRequiredException _
```

#### `InstanceAction`

``` purescript
newtype InstanceAction
  = InstanceAction String
```

##### Instances
``` purescript
Newtype InstanceAction _
```

#### `InstanceArn`

``` purescript
newtype InstanceArn
  = InstanceArn String
```

##### Instances
``` purescript
Newtype InstanceArn _
```

#### `InstanceCount`

``` purescript
newtype InstanceCount
  = InstanceCount Number
```

##### Instances
``` purescript
Newtype InstanceCount _
```

#### `InstanceDoesNotExistException`

``` purescript
newtype InstanceDoesNotExistException
  = InstanceDoesNotExistException {  }
```

<p>The specified instance does not exist in the deployment group.</p>

##### Instances
``` purescript
Newtype InstanceDoesNotExistException _
```

#### `InstanceId`

``` purescript
newtype InstanceId
  = InstanceId String
```

##### Instances
``` purescript
Newtype InstanceId _
```

#### `InstanceIdRequiredException`

``` purescript
newtype InstanceIdRequiredException
  = InstanceIdRequiredException {  }
```

<p>The instance ID was not specified.</p>

##### Instances
``` purescript
Newtype InstanceIdRequiredException _
```

#### `InstanceInfo`

``` purescript
newtype InstanceInfo
  = InstanceInfo { "InstanceName'" :: NullOrUndefined (InstanceName), "IamSessionArn'" :: NullOrUndefined (IamSessionArn), "IamUserArn'" :: NullOrUndefined (IamUserArn), "InstanceArn'" :: NullOrUndefined (InstanceArn), "RegisterTime'" :: NullOrUndefined (Number), "DeregisterTime'" :: NullOrUndefined (Number), "Tags'" :: NullOrUndefined (TagList) }
```

<p>Information about an on-premises instance.</p>

##### Instances
``` purescript
Newtype InstanceInfo _
```

#### `InstanceInfoList`

``` purescript
newtype InstanceInfoList
  = InstanceInfoList (Array InstanceInfo)
```

##### Instances
``` purescript
Newtype InstanceInfoList _
```

#### `InstanceLimitExceededException`

``` purescript
newtype InstanceLimitExceededException
  = InstanceLimitExceededException {  }
```

<p>The maximum number of allowed on-premises instances in a single call was exceeded.</p>

##### Instances
``` purescript
Newtype InstanceLimitExceededException _
```

#### `InstanceName`

``` purescript
newtype InstanceName
  = InstanceName String
```

##### Instances
``` purescript
Newtype InstanceName _
```

#### `InstanceNameAlreadyRegisteredException`

``` purescript
newtype InstanceNameAlreadyRegisteredException
  = InstanceNameAlreadyRegisteredException {  }
```

<p>The specified on-premises instance name is already registered.</p>

##### Instances
``` purescript
Newtype InstanceNameAlreadyRegisteredException _
```

#### `InstanceNameList`

``` purescript
newtype InstanceNameList
  = InstanceNameList (Array InstanceName)
```

##### Instances
``` purescript
Newtype InstanceNameList _
```

#### `InstanceNameRequiredException`

``` purescript
newtype InstanceNameRequiredException
  = InstanceNameRequiredException {  }
```

<p>An on-premises instance name was not specified.</p>

##### Instances
``` purescript
Newtype InstanceNameRequiredException _
```

#### `InstanceNotRegisteredException`

``` purescript
newtype InstanceNotRegisteredException
  = InstanceNotRegisteredException {  }
```

<p>The specified on-premises instance is not registered.</p>

##### Instances
``` purescript
Newtype InstanceNotRegisteredException _
```

#### `InstanceStatus`

``` purescript
newtype InstanceStatus
  = InstanceStatus String
```

##### Instances
``` purescript
Newtype InstanceStatus _
```

#### `InstanceStatusList`

``` purescript
newtype InstanceStatusList
  = InstanceStatusList (Array InstanceStatus)
```

##### Instances
``` purescript
Newtype InstanceStatusList _
```

#### `InstanceSummary`

``` purescript
newtype InstanceSummary
  = InstanceSummary { "DeploymentId'" :: NullOrUndefined (DeploymentId), "InstanceId'" :: NullOrUndefined (InstanceId), "Status'" :: NullOrUndefined (InstanceStatus), "LastUpdatedAt'" :: NullOrUndefined (Number), "LifecycleEvents'" :: NullOrUndefined (LifecycleEventList), "InstanceType'" :: NullOrUndefined (InstanceType) }
```

<p>Information about an instance in a deployment.</p>

##### Instances
``` purescript
Newtype InstanceSummary _
```

#### `InstanceSummaryList`

``` purescript
newtype InstanceSummaryList
  = InstanceSummaryList (Array InstanceSummary)
```

##### Instances
``` purescript
Newtype InstanceSummaryList _
```

#### `InstanceType`

``` purescript
newtype InstanceType
  = InstanceType String
```

##### Instances
``` purescript
Newtype InstanceType _
```

#### `InstanceTypeList`

``` purescript
newtype InstanceTypeList
  = InstanceTypeList (Array InstanceType)
```

##### Instances
``` purescript
Newtype InstanceTypeList _
```

#### `InstancesList`

``` purescript
newtype InstancesList
  = InstancesList (Array InstanceId)
```

##### Instances
``` purescript
Newtype InstancesList _
```

#### `InvalidAlarmConfigException`

``` purescript
newtype InvalidAlarmConfigException
  = InvalidAlarmConfigException {  }
```

<p>The format of the alarm configuration is invalid. Possible causes include:</p> <ul> <li> <p>The alarm list is null.</p> </li> <li> <p>The alarm object is null.</p> </li> <li> <p>The alarm name is empty or null or exceeds the 255 character limit.</p> </li> <li> <p>Two alarms with the same name have been specified.</p> </li> <li> <p>The alarm configuration is enabled but the alarm list is empty.</p> </li> </ul>

##### Instances
``` purescript
Newtype InvalidAlarmConfigException _
```

#### `InvalidApplicationNameException`

``` purescript
newtype InvalidApplicationNameException
  = InvalidApplicationNameException {  }
```

<p>The application name was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidApplicationNameException _
```

#### `InvalidAutoRollbackConfigException`

``` purescript
newtype InvalidAutoRollbackConfigException
  = InvalidAutoRollbackConfigException {  }
```

<p>The automatic rollback configuration was specified in an invalid format. For example, automatic rollback is enabled but an invalid triggering event type or no event types were listed.</p>

##### Instances
``` purescript
Newtype InvalidAutoRollbackConfigException _
```

#### `InvalidAutoScalingGroupException`

``` purescript
newtype InvalidAutoScalingGroupException
  = InvalidAutoScalingGroupException {  }
```

<p>The Auto Scaling group was specified in an invalid format or does not exist.</p>

##### Instances
``` purescript
Newtype InvalidAutoScalingGroupException _
```

#### `InvalidBlueGreenDeploymentConfigurationException`

``` purescript
newtype InvalidBlueGreenDeploymentConfigurationException
  = InvalidBlueGreenDeploymentConfigurationException {  }
```

<p>The configuration for the blue/green deployment group was provided in an invalid format. For information about deployment configuration format, see <a>CreateDeploymentConfig</a>.</p>

##### Instances
``` purescript
Newtype InvalidBlueGreenDeploymentConfigurationException _
```

#### `InvalidBucketNameFilterException`

``` purescript
newtype InvalidBucketNameFilterException
  = InvalidBucketNameFilterException {  }
```

<p>The bucket name either doesn't exist or was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidBucketNameFilterException _
```

#### `InvalidComputePlatformException`

``` purescript
newtype InvalidComputePlatformException
  = InvalidComputePlatformException {  }
```

<p>The computePlatform is invalid. The computePlatform should be <code>Lambda</code> or <code>Server</code>.</p>

##### Instances
``` purescript
Newtype InvalidComputePlatformException _
```

#### `InvalidDeployedStateFilterException`

``` purescript
newtype InvalidDeployedStateFilterException
  = InvalidDeployedStateFilterException {  }
```

<p>The deployed state filter was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidDeployedStateFilterException _
```

#### `InvalidDeploymentConfigNameException`

``` purescript
newtype InvalidDeploymentConfigNameException
  = InvalidDeploymentConfigNameException {  }
```

<p>The deployment configuration name was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidDeploymentConfigNameException _
```

#### `InvalidDeploymentGroupNameException`

``` purescript
newtype InvalidDeploymentGroupNameException
  = InvalidDeploymentGroupNameException {  }
```

<p>The deployment group name was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidDeploymentGroupNameException _
```

#### `InvalidDeploymentIdException`

``` purescript
newtype InvalidDeploymentIdException
  = InvalidDeploymentIdException {  }
```

<p>At least one of the deployment IDs was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidDeploymentIdException _
```

#### `InvalidDeploymentInstanceTypeException`

``` purescript
newtype InvalidDeploymentInstanceTypeException
  = InvalidDeploymentInstanceTypeException {  }
```

<p>An instance type was specified for an in-place deployment. Instance types are supported for blue/green deployments only.</p>

##### Instances
``` purescript
Newtype InvalidDeploymentInstanceTypeException _
```

#### `InvalidDeploymentStatusException`

``` purescript
newtype InvalidDeploymentStatusException
  = InvalidDeploymentStatusException {  }
```

<p>The specified deployment status doesn't exist or cannot be determined.</p>

##### Instances
``` purescript
Newtype InvalidDeploymentStatusException _
```

#### `InvalidDeploymentStyleException`

``` purescript
newtype InvalidDeploymentStyleException
  = InvalidDeploymentStyleException {  }
```

<p>An invalid deployment style was specified. Valid deployment types include "IN_PLACE" and "BLUE_GREEN". Valid deployment options include "WITH_TRAFFIC_CONTROL" and "WITHOUT_TRAFFIC_CONTROL".</p>

##### Instances
``` purescript
Newtype InvalidDeploymentStyleException _
```

#### `InvalidEC2TagCombinationException`

``` purescript
newtype InvalidEC2TagCombinationException
  = InvalidEC2TagCombinationException {  }
```

<p>A call was submitted that specified both Ec2TagFilters and Ec2TagSet, but only one of these data types can be used in a single call.</p>

##### Instances
``` purescript
Newtype InvalidEC2TagCombinationException _
```

#### `InvalidEC2TagException`

``` purescript
newtype InvalidEC2TagException
  = InvalidEC2TagException {  }
```

<p>The tag was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidEC2TagException _
```

#### `InvalidFileExistsBehaviorException`

``` purescript
newtype InvalidFileExistsBehaviorException
  = InvalidFileExistsBehaviorException {  }
```

<p>An invalid fileExistsBehavior option was specified to determine how AWS CodeDeploy handles files or directories that already exist in a deployment target location but weren't part of the previous successful deployment. Valid values include "DISALLOW", "OVERWRITE", and "RETAIN".</p>

##### Instances
``` purescript
Newtype InvalidFileExistsBehaviorException _
```

#### `InvalidGitHubAccountTokenNameException`

``` purescript
newtype InvalidGitHubAccountTokenNameException
  = InvalidGitHubAccountTokenNameException {  }
```

<p>The format of the specified GitHub account connection name is invalid.</p>

##### Instances
``` purescript
Newtype InvalidGitHubAccountTokenNameException _
```

#### `InvalidIamSessionArnException`

``` purescript
newtype InvalidIamSessionArnException
  = InvalidIamSessionArnException {  }
```

<p>The IAM session ARN was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidIamSessionArnException _
```

#### `InvalidIamUserArnException`

``` purescript
newtype InvalidIamUserArnException
  = InvalidIamUserArnException {  }
```

<p>The IAM user ARN was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidIamUserArnException _
```

#### `InvalidIgnoreApplicationStopFailuresValueException`

``` purescript
newtype InvalidIgnoreApplicationStopFailuresValueException
  = InvalidIgnoreApplicationStopFailuresValueException {  }
```

<p>The IgnoreApplicationStopFailures value is invalid. For AWS Lambda deployments, <code>false</code> is expected. For EC2/On-premises deployments, <code>true</code> or <code>false</code> is expected.</p>

##### Instances
``` purescript
Newtype InvalidIgnoreApplicationStopFailuresValueException _
```

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException {  }
```

<p>The specified input was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidInputException _
```

#### `InvalidInstanceIdException`

``` purescript
newtype InvalidInstanceIdException
  = InvalidInstanceIdException {  }
```

<p> </p>

##### Instances
``` purescript
Newtype InvalidInstanceIdException _
```

#### `InvalidInstanceNameException`

``` purescript
newtype InvalidInstanceNameException
  = InvalidInstanceNameException {  }
```

<p>The specified on-premises instance name was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidInstanceNameException _
```

#### `InvalidInstanceStatusException`

``` purescript
newtype InvalidInstanceStatusException
  = InvalidInstanceStatusException {  }
```

<p>The specified instance status does not exist.</p>

##### Instances
``` purescript
Newtype InvalidInstanceStatusException _
```

#### `InvalidInstanceTypeException`

``` purescript
newtype InvalidInstanceTypeException
  = InvalidInstanceTypeException {  }
```

<p>An invalid instance type was specified for instances in a blue/green deployment. Valid values include "Blue" for an original environment and "Green" for a replacement environment.</p>

##### Instances
``` purescript
Newtype InvalidInstanceTypeException _
```

#### `InvalidKeyPrefixFilterException`

``` purescript
newtype InvalidKeyPrefixFilterException
  = InvalidKeyPrefixFilterException {  }
```

<p>The specified key prefix filter was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidKeyPrefixFilterException _
```

#### `InvalidLifecycleEventHookExecutionIdException`

``` purescript
newtype InvalidLifecycleEventHookExecutionIdException
  = InvalidLifecycleEventHookExecutionIdException {  }
```

<p>A lifecycle event hook is invalid. Review the <code>hooks</code> section in your AppSpec file to ensure the lifecycle events and <code>hooks</code> functions are valid.</p>

##### Instances
``` purescript
Newtype InvalidLifecycleEventHookExecutionIdException _
```

#### `InvalidLifecycleEventHookExecutionStatusException`

``` purescript
newtype InvalidLifecycleEventHookExecutionStatusException
  = InvalidLifecycleEventHookExecutionStatusException {  }
```

<p>The result of a Lambda validation function that verifies a lifecycle event is invalid. It should return <code>Succeeded</code> or <code>Failed</code>.</p>

##### Instances
``` purescript
Newtype InvalidLifecycleEventHookExecutionStatusException _
```

#### `InvalidLoadBalancerInfoException`

``` purescript
newtype InvalidLoadBalancerInfoException
  = InvalidLoadBalancerInfoException {  }
```

<p>An invalid load balancer name, or no load balancer name, was specified.</p>

##### Instances
``` purescript
Newtype InvalidLoadBalancerInfoException _
```

#### `InvalidMinimumHealthyHostValueException`

``` purescript
newtype InvalidMinimumHealthyHostValueException
  = InvalidMinimumHealthyHostValueException {  }
```

<p>The minimum healthy instance value was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidMinimumHealthyHostValueException _
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException {  }
```

<p>The next token was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidNextTokenException _
```

#### `InvalidOnPremisesTagCombinationException`

``` purescript
newtype InvalidOnPremisesTagCombinationException
  = InvalidOnPremisesTagCombinationException {  }
```

<p>A call was submitted that specified both OnPremisesTagFilters and OnPremisesTagSet, but only one of these data types can be used in a single call.</p>

##### Instances
``` purescript
Newtype InvalidOnPremisesTagCombinationException _
```

#### `InvalidOperationException`

``` purescript
newtype InvalidOperationException
  = InvalidOperationException {  }
```

<p>An invalid operation was detected.</p>

##### Instances
``` purescript
Newtype InvalidOperationException _
```

#### `InvalidRegistrationStatusException`

``` purescript
newtype InvalidRegistrationStatusException
  = InvalidRegistrationStatusException {  }
```

<p>The registration status was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidRegistrationStatusException _
```

#### `InvalidRevisionException`

``` purescript
newtype InvalidRevisionException
  = InvalidRevisionException {  }
```

<p>The revision was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidRevisionException _
```

#### `InvalidRoleException`

``` purescript
newtype InvalidRoleException
  = InvalidRoleException {  }
```

<p>The service role ARN was specified in an invalid format. Or, if an Auto Scaling group was specified, the specified service role does not grant the appropriate permissions to Auto Scaling.</p>

##### Instances
``` purescript
Newtype InvalidRoleException _
```

#### `InvalidSortByException`

``` purescript
newtype InvalidSortByException
  = InvalidSortByException {  }
```

<p>The column name to sort by is either not present or was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidSortByException _
```

#### `InvalidSortOrderException`

``` purescript
newtype InvalidSortOrderException
  = InvalidSortOrderException {  }
```

<p>The sort order was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidSortOrderException _
```

#### `InvalidTagException`

``` purescript
newtype InvalidTagException
  = InvalidTagException {  }
```

<p>The specified tag was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidTagException _
```

#### `InvalidTagFilterException`

``` purescript
newtype InvalidTagFilterException
  = InvalidTagFilterException {  }
```

<p>The specified tag filter was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidTagFilterException _
```

#### `InvalidTargetInstancesException`

``` purescript
newtype InvalidTargetInstancesException
  = InvalidTargetInstancesException {  }
```

<p>The target instance configuration is invalid. Possible causes include:</p> <ul> <li> <p>Configuration data for target instances was entered for an in-place deployment.</p> </li> <li> <p>The limit of 10 tags for a tag type was exceeded.</p> </li> <li> <p>The combined length of the tag names exceeded the limit. </p> </li> <li> <p>A specified tag is not currently applied to any instances.</p> </li> </ul>

##### Instances
``` purescript
Newtype InvalidTargetInstancesException _
```

#### `InvalidTimeRangeException`

``` purescript
newtype InvalidTimeRangeException
  = InvalidTimeRangeException {  }
```

<p>The specified time range was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidTimeRangeException _
```

#### `InvalidTrafficRoutingConfigurationException`

``` purescript
newtype InvalidTrafficRoutingConfigurationException
  = InvalidTrafficRoutingConfigurationException {  }
```

<p> The configuration that specifies how traffic is routed during a deployment is invalid.</p>

##### Instances
``` purescript
Newtype InvalidTrafficRoutingConfigurationException _
```

#### `InvalidTriggerConfigException`

``` purescript
newtype InvalidTriggerConfigException
  = InvalidTriggerConfigException {  }
```

<p>The trigger was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidTriggerConfigException _
```

#### `InvalidUpdateOutdatedInstancesOnlyValueException`

``` purescript
newtype InvalidUpdateOutdatedInstancesOnlyValueException
  = InvalidUpdateOutdatedInstancesOnlyValueException {  }
```

<p>The UpdateOutdatedInstancesOnly value is invalid. For AWS Lambda deployments, <code>false</code> is expected. For EC2/On-premises deployments, <code>true</code> or <code>false</code> is expected.</p>

##### Instances
``` purescript
Newtype InvalidUpdateOutdatedInstancesOnlyValueException _
```

#### `Key`

``` purescript
newtype Key
  = Key String
```

##### Instances
``` purescript
Newtype Key _
```

#### `LastDeploymentInfo`

``` purescript
newtype LastDeploymentInfo
  = LastDeploymentInfo { "DeploymentId'" :: NullOrUndefined (DeploymentId), "Status'" :: NullOrUndefined (DeploymentStatus), "EndTime'" :: NullOrUndefined (Number), "CreateTime'" :: NullOrUndefined (Number) }
```

<p>Information about the most recent attempted or successful deployment to a deployment group.</p>

##### Instances
``` purescript
Newtype LastDeploymentInfo _
```

#### `LifecycleErrorCode`

``` purescript
newtype LifecycleErrorCode
  = LifecycleErrorCode String
```

##### Instances
``` purescript
Newtype LifecycleErrorCode _
```

#### `LifecycleEvent`

``` purescript
newtype LifecycleEvent
  = LifecycleEvent { "LifecycleEventName'" :: NullOrUndefined (LifecycleEventName), "Diagnostics'" :: NullOrUndefined (Diagnostics), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "Status'" :: NullOrUndefined (LifecycleEventStatus) }
```

<p>Information about a deployment lifecycle event.</p>

##### Instances
``` purescript
Newtype LifecycleEvent _
```

#### `LifecycleEventAlreadyCompletedException`

``` purescript
newtype LifecycleEventAlreadyCompletedException
  = LifecycleEventAlreadyCompletedException {  }
```

<p>An attempt to return the status of an already completed lifecycle event occurred.</p>

##### Instances
``` purescript
Newtype LifecycleEventAlreadyCompletedException _
```

#### `LifecycleEventHookExecutionId`

``` purescript
newtype LifecycleEventHookExecutionId
  = LifecycleEventHookExecutionId String
```

##### Instances
``` purescript
Newtype LifecycleEventHookExecutionId _
```

#### `LifecycleEventList`

``` purescript
newtype LifecycleEventList
  = LifecycleEventList (Array LifecycleEvent)
```

##### Instances
``` purescript
Newtype LifecycleEventList _
```

#### `LifecycleEventName`

``` purescript
newtype LifecycleEventName
  = LifecycleEventName String
```

##### Instances
``` purescript
Newtype LifecycleEventName _
```

#### `LifecycleEventStatus`

``` purescript
newtype LifecycleEventStatus
  = LifecycleEventStatus String
```

##### Instances
``` purescript
Newtype LifecycleEventStatus _
```

#### `LifecycleHookLimitExceededException`

``` purescript
newtype LifecycleHookLimitExceededException
  = LifecycleHookLimitExceededException {  }
```

<p>The limit for lifecycle hooks was exceeded.</p>

##### Instances
``` purescript
Newtype LifecycleHookLimitExceededException _
```

#### `LifecycleMessage`

``` purescript
newtype LifecycleMessage
  = LifecycleMessage String
```

##### Instances
``` purescript
Newtype LifecycleMessage _
```

#### `ListApplicationRevisionsInput`

``` purescript
newtype ListApplicationRevisionsInput
  = ListApplicationRevisionsInput { "ApplicationName'" :: ApplicationName, "SortBy'" :: NullOrUndefined (ApplicationRevisionSortBy), "SortOrder'" :: NullOrUndefined (SortOrder), "S3Bucket'" :: NullOrUndefined (S3Bucket), "S3KeyPrefix'" :: NullOrUndefined (S3Key), "Deployed'" :: NullOrUndefined (ListStateFilterAction), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListApplicationRevisions operation.</p>

##### Instances
``` purescript
Newtype ListApplicationRevisionsInput _
```

#### `ListApplicationRevisionsOutput`

``` purescript
newtype ListApplicationRevisionsOutput
  = ListApplicationRevisionsOutput { "Revisions'" :: NullOrUndefined (RevisionLocationList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListApplicationRevisions operation.</p>

##### Instances
``` purescript
Newtype ListApplicationRevisionsOutput _
```

#### `ListApplicationsInput`

``` purescript
newtype ListApplicationsInput
  = ListApplicationsInput { "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListApplications operation.</p>

##### Instances
``` purescript
Newtype ListApplicationsInput _
```

#### `ListApplicationsOutput`

``` purescript
newtype ListApplicationsOutput
  = ListApplicationsOutput { "Applications'" :: NullOrUndefined (ApplicationsList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListApplications operation.</p>

##### Instances
``` purescript
Newtype ListApplicationsOutput _
```

#### `ListDeploymentConfigsInput`

``` purescript
newtype ListDeploymentConfigsInput
  = ListDeploymentConfigsInput { "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListDeploymentConfigs operation.</p>

##### Instances
``` purescript
Newtype ListDeploymentConfigsInput _
```

#### `ListDeploymentConfigsOutput`

``` purescript
newtype ListDeploymentConfigsOutput
  = ListDeploymentConfigsOutput { "DeploymentConfigsList'" :: NullOrUndefined (DeploymentConfigsList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListDeploymentConfigs operation.</p>

##### Instances
``` purescript
Newtype ListDeploymentConfigsOutput _
```

#### `ListDeploymentGroupsInput`

``` purescript
newtype ListDeploymentGroupsInput
  = ListDeploymentGroupsInput { "ApplicationName'" :: ApplicationName, "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListDeploymentGroups operation.</p>

##### Instances
``` purescript
Newtype ListDeploymentGroupsInput _
```

#### `ListDeploymentGroupsOutput`

``` purescript
newtype ListDeploymentGroupsOutput
  = ListDeploymentGroupsOutput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "DeploymentGroups'" :: NullOrUndefined (DeploymentGroupsList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListDeploymentGroups operation.</p>

##### Instances
``` purescript
Newtype ListDeploymentGroupsOutput _
```

#### `ListDeploymentInstancesInput`

``` purescript
newtype ListDeploymentInstancesInput
  = ListDeploymentInstancesInput { "DeploymentId'" :: DeploymentId, "NextToken'" :: NullOrUndefined (NextToken), "InstanceStatusFilter'" :: NullOrUndefined (InstanceStatusList), "InstanceTypeFilter'" :: NullOrUndefined (InstanceTypeList) }
```

<p>Represents the input of a ListDeploymentInstances operation.</p>

##### Instances
``` purescript
Newtype ListDeploymentInstancesInput _
```

#### `ListDeploymentInstancesOutput`

``` purescript
newtype ListDeploymentInstancesOutput
  = ListDeploymentInstancesOutput { "InstancesList'" :: NullOrUndefined (InstancesList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListDeploymentInstances operation.</p>

##### Instances
``` purescript
Newtype ListDeploymentInstancesOutput _
```

#### `ListDeploymentsInput`

``` purescript
newtype ListDeploymentsInput
  = ListDeploymentsInput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "IncludeOnlyStatuses'" :: NullOrUndefined (DeploymentStatusList), "CreateTimeRange'" :: NullOrUndefined (TimeRange), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListDeployments operation.</p>

##### Instances
``` purescript
Newtype ListDeploymentsInput _
```

#### `ListDeploymentsOutput`

``` purescript
newtype ListDeploymentsOutput
  = ListDeploymentsOutput { "Deployments'" :: NullOrUndefined (DeploymentsList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListDeployments operation.</p>

##### Instances
``` purescript
Newtype ListDeploymentsOutput _
```

#### `ListGitHubAccountTokenNamesInput`

``` purescript
newtype ListGitHubAccountTokenNamesInput
  = ListGitHubAccountTokenNamesInput { "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListGitHubAccountTokenNames operation.</p>

##### Instances
``` purescript
Newtype ListGitHubAccountTokenNamesInput _
```

#### `ListGitHubAccountTokenNamesOutput`

``` purescript
newtype ListGitHubAccountTokenNamesOutput
  = ListGitHubAccountTokenNamesOutput { "TokenNameList'" :: NullOrUndefined (GitHubAccountTokenNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListGitHubAccountTokenNames operation.</p>

##### Instances
``` purescript
Newtype ListGitHubAccountTokenNamesOutput _
```

#### `ListOnPremisesInstancesInput`

``` purescript
newtype ListOnPremisesInstancesInput
  = ListOnPremisesInstancesInput { "RegistrationStatus'" :: NullOrUndefined (RegistrationStatus), "TagFilters'" :: NullOrUndefined (TagFilterList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListOnPremisesInstances operation.</p>

##### Instances
``` purescript
Newtype ListOnPremisesInstancesInput _
```

#### `ListOnPremisesInstancesOutput`

``` purescript
newtype ListOnPremisesInstancesOutput
  = ListOnPremisesInstancesOutput { "InstanceNames'" :: NullOrUndefined (InstanceNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of list on-premises instances operation.</p>

##### Instances
``` purescript
Newtype ListOnPremisesInstancesOutput _
```

#### `ListStateFilterAction`

``` purescript
newtype ListStateFilterAction
  = ListStateFilterAction String
```

##### Instances
``` purescript
Newtype ListStateFilterAction _
```

#### `LoadBalancerInfo`

``` purescript
newtype LoadBalancerInfo
  = LoadBalancerInfo { "ElbInfoList'" :: NullOrUndefined (ELBInfoList), "TargetGroupInfoList'" :: NullOrUndefined (TargetGroupInfoList) }
```

<p>Information about the Elastic Load Balancing load balancer or target group used in a deployment.</p>

##### Instances
``` purescript
Newtype LoadBalancerInfo _
```

#### `LogTail`

``` purescript
newtype LogTail
  = LogTail String
```

##### Instances
``` purescript
Newtype LogTail _
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

##### Instances
``` purescript
Newtype Message _
```

#### `MinimumHealthyHosts`

``` purescript
newtype MinimumHealthyHosts
  = MinimumHealthyHosts { "Value'" :: NullOrUndefined (MinimumHealthyHostsValue), "Type'" :: NullOrUndefined (MinimumHealthyHostsType) }
```

<p>Information about minimum healthy instance.</p>

##### Instances
``` purescript
Newtype MinimumHealthyHosts _
```

#### `MinimumHealthyHostsType`

``` purescript
newtype MinimumHealthyHostsType
  = MinimumHealthyHostsType String
```

##### Instances
``` purescript
Newtype MinimumHealthyHostsType _
```

#### `MinimumHealthyHostsValue`

``` purescript
newtype MinimumHealthyHostsValue
  = MinimumHealthyHostsValue Int
```

##### Instances
``` purescript
Newtype MinimumHealthyHostsValue _
```

#### `MultipleIamArnsProvidedException`

``` purescript
newtype MultipleIamArnsProvidedException
  = MultipleIamArnsProvidedException {  }
```

<p>Both an IAM user ARN and an IAM session ARN were included in the request. Use only one ARN type.</p>

##### Instances
``` purescript
Newtype MultipleIamArnsProvidedException _
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

#### `NullableBoolean`

``` purescript
newtype NullableBoolean
  = NullableBoolean Boolean
```

##### Instances
``` purescript
Newtype NullableBoolean _
```

#### `OnPremisesTagSet`

``` purescript
newtype OnPremisesTagSet
  = OnPremisesTagSet { "OnPremisesTagSetList'" :: NullOrUndefined (OnPremisesTagSetList) }
```

<p>Information about groups of on-premises instance tags.</p>

##### Instances
``` purescript
Newtype OnPremisesTagSet _
```

#### `OnPremisesTagSetList`

``` purescript
newtype OnPremisesTagSetList
  = OnPremisesTagSetList (Array TagFilterList)
```

##### Instances
``` purescript
Newtype OnPremisesTagSetList _
```

#### `OperationNotSupportedException`

``` purescript
newtype OperationNotSupportedException
  = OperationNotSupportedException {  }
```

<p>The API used does not support the deployment.</p>

##### Instances
``` purescript
Newtype OperationNotSupportedException _
```

#### `Percentage`

``` purescript
newtype Percentage
  = Percentage Int
```

##### Instances
``` purescript
Newtype Percentage _
```

#### `PutLifecycleEventHookExecutionStatusInput`

``` purescript
newtype PutLifecycleEventHookExecutionStatusInput
  = PutLifecycleEventHookExecutionStatusInput { "DeploymentId'" :: NullOrUndefined (DeploymentId), "LifecycleEventHookExecutionId'" :: NullOrUndefined (LifecycleEventHookExecutionId), "Status'" :: NullOrUndefined (LifecycleEventStatus) }
```

##### Instances
``` purescript
Newtype PutLifecycleEventHookExecutionStatusInput _
```

#### `PutLifecycleEventHookExecutionStatusOutput`

``` purescript
newtype PutLifecycleEventHookExecutionStatusOutput
  = PutLifecycleEventHookExecutionStatusOutput { "LifecycleEventHookExecutionId'" :: NullOrUndefined (LifecycleEventHookExecutionId) }
```

##### Instances
``` purescript
Newtype PutLifecycleEventHookExecutionStatusOutput _
```

#### `RawString`

``` purescript
newtype RawString
  = RawString { "Content'" :: NullOrUndefined (RawStringContent), "Sha256'" :: NullOrUndefined (RawStringSha256) }
```

<p>A revision for an AWS Lambda deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda deployments, the revision is the same as the AppSpec file.</p>

##### Instances
``` purescript
Newtype RawString _
```

#### `RawStringContent`

``` purescript
newtype RawStringContent
  = RawStringContent String
```

##### Instances
``` purescript
Newtype RawStringContent _
```

#### `RawStringSha256`

``` purescript
newtype RawStringSha256
  = RawStringSha256 String
```

##### Instances
``` purescript
Newtype RawStringSha256 _
```

#### `RegisterApplicationRevisionInput`

``` purescript
newtype RegisterApplicationRevisionInput
  = RegisterApplicationRevisionInput { "ApplicationName'" :: ApplicationName, "Description'" :: NullOrUndefined (Description), "Revision'" :: RevisionLocation }
```

<p>Represents the input of a RegisterApplicationRevision operation.</p>

##### Instances
``` purescript
Newtype RegisterApplicationRevisionInput _
```

#### `RegisterOnPremisesInstanceInput`

``` purescript
newtype RegisterOnPremisesInstanceInput
  = RegisterOnPremisesInstanceInput { "InstanceName'" :: InstanceName, "IamSessionArn'" :: NullOrUndefined (IamSessionArn), "IamUserArn'" :: NullOrUndefined (IamUserArn) }
```

<p>Represents the input of the register on-premises instance operation.</p>

##### Instances
``` purescript
Newtype RegisterOnPremisesInstanceInput _
```

#### `RegistrationStatus`

``` purescript
newtype RegistrationStatus
  = RegistrationStatus String
```

##### Instances
``` purescript
Newtype RegistrationStatus _
```

#### `RemoveTagsFromOnPremisesInstancesInput`

``` purescript
newtype RemoveTagsFromOnPremisesInstancesInput
  = RemoveTagsFromOnPremisesInstancesInput { "Tags'" :: TagList, "InstanceNames'" :: InstanceNameList }
```

<p>Represents the input of a RemoveTagsFromOnPremisesInstances operation.</p>

##### Instances
``` purescript
Newtype RemoveTagsFromOnPremisesInstancesInput _
```

#### `Repository`

``` purescript
newtype Repository
  = Repository String
```

##### Instances
``` purescript
Newtype Repository _
```

#### `ResourceValidationException`

``` purescript
newtype ResourceValidationException
  = ResourceValidationException {  }
```

<p>The specified resource could not be validated.</p>

##### Instances
``` purescript
Newtype ResourceValidationException _
```

#### `RevisionDoesNotExistException`

``` purescript
newtype RevisionDoesNotExistException
  = RevisionDoesNotExistException {  }
```

<p>The named revision does not exist with the applicable IAM user or AWS account.</p>

##### Instances
``` purescript
Newtype RevisionDoesNotExistException _
```

#### `RevisionInfo`

``` purescript
newtype RevisionInfo
  = RevisionInfo { "RevisionLocation'" :: NullOrUndefined (RevisionLocation), "GenericRevisionInfo'" :: NullOrUndefined (GenericRevisionInfo) }
```

<p>Information about an application revision.</p>

##### Instances
``` purescript
Newtype RevisionInfo _
```

#### `RevisionInfoList`

``` purescript
newtype RevisionInfoList
  = RevisionInfoList (Array RevisionInfo)
```

##### Instances
``` purescript
Newtype RevisionInfoList _
```

#### `RevisionLocation`

``` purescript
newtype RevisionLocation
  = RevisionLocation { "RevisionType'" :: NullOrUndefined (RevisionLocationType), "S3Location'" :: NullOrUndefined (S3Location), "GitHubLocation'" :: NullOrUndefined (GitHubLocation), "String" :: NullOrUndefined (RawString) }
```

<p>Information about the location of an application revision.</p>

##### Instances
``` purescript
Newtype RevisionLocation _
```

#### `RevisionLocationList`

``` purescript
newtype RevisionLocationList
  = RevisionLocationList (Array RevisionLocation)
```

##### Instances
``` purescript
Newtype RevisionLocationList _
```

#### `RevisionLocationType`

``` purescript
newtype RevisionLocationType
  = RevisionLocationType String
```

##### Instances
``` purescript
Newtype RevisionLocationType _
```

#### `RevisionRequiredException`

``` purescript
newtype RevisionRequiredException
  = RevisionRequiredException {  }
```

<p>The revision ID was not specified.</p>

##### Instances
``` purescript
Newtype RevisionRequiredException _
```

#### `Role`

``` purescript
newtype Role
  = Role String
```

##### Instances
``` purescript
Newtype Role _
```

#### `RoleRequiredException`

``` purescript
newtype RoleRequiredException
  = RoleRequiredException {  }
```

<p>The role ID was not specified.</p>

##### Instances
``` purescript
Newtype RoleRequiredException _
```

#### `RollbackInfo`

``` purescript
newtype RollbackInfo
  = RollbackInfo { "RollbackDeploymentId'" :: NullOrUndefined (DeploymentId), "RollbackTriggeringDeploymentId'" :: NullOrUndefined (DeploymentId), "RollbackMessage'" :: NullOrUndefined (Description) }
```

<p>Information about a deployment rollback.</p>

##### Instances
``` purescript
Newtype RollbackInfo _
```

#### `S3Bucket`

``` purescript
newtype S3Bucket
  = S3Bucket String
```

##### Instances
``` purescript
Newtype S3Bucket _
```

#### `S3Key`

``` purescript
newtype S3Key
  = S3Key String
```

##### Instances
``` purescript
Newtype S3Key _
```

#### `S3Location`

``` purescript
newtype S3Location
  = S3Location { "Bucket'" :: NullOrUndefined (S3Bucket), "Key'" :: NullOrUndefined (S3Key), "BundleType'" :: NullOrUndefined (BundleType), "Version'" :: NullOrUndefined (VersionId), "ETag'" :: NullOrUndefined (ETag) }
```

<p>Information about the location of application artifacts stored in Amazon S3.</p>

##### Instances
``` purescript
Newtype S3Location _
```

#### `ScriptName`

``` purescript
newtype ScriptName
  = ScriptName String
```

##### Instances
``` purescript
Newtype ScriptName _
```

#### `SkipWaitTimeForInstanceTerminationInput`

``` purescript
newtype SkipWaitTimeForInstanceTerminationInput
  = SkipWaitTimeForInstanceTerminationInput { "DeploymentId'" :: NullOrUndefined (DeploymentId) }
```

##### Instances
``` purescript
Newtype SkipWaitTimeForInstanceTerminationInput _
```

#### `SortOrder`

``` purescript
newtype SortOrder
  = SortOrder String
```

##### Instances
``` purescript
Newtype SortOrder _
```

#### `StopDeploymentInput`

``` purescript
newtype StopDeploymentInput
  = StopDeploymentInput { "DeploymentId'" :: DeploymentId, "AutoRollbackEnabled'" :: NullOrUndefined (NullableBoolean) }
```

<p>Represents the input of a StopDeployment operation.</p>

##### Instances
``` purescript
Newtype StopDeploymentInput _
```

#### `StopDeploymentOutput`

``` purescript
newtype StopDeploymentOutput
  = StopDeploymentOutput { "Status'" :: NullOrUndefined (StopStatus), "StatusMessage'" :: NullOrUndefined (Message) }
```

<p>Represents the output of a StopDeployment operation.</p>

##### Instances
``` purescript
Newtype StopDeploymentOutput _
```

#### `StopStatus`

``` purescript
newtype StopStatus
  = StopStatus String
```

##### Instances
``` purescript
Newtype StopStatus _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (Key), "Value" :: NullOrUndefined (Value) }
```

<p>Information about a tag.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagFilter`

``` purescript
newtype TagFilter
  = TagFilter { "Key" :: NullOrUndefined (Key), "Value" :: NullOrUndefined (Value), "Type" :: NullOrUndefined (TagFilterType) }
```

<p>Information about an on-premises instance tag filter.</p>

##### Instances
``` purescript
Newtype TagFilter _
```

#### `TagFilterList`

``` purescript
newtype TagFilterList
  = TagFilterList (Array TagFilter)
```

##### Instances
``` purescript
Newtype TagFilterList _
```

#### `TagFilterType`

``` purescript
newtype TagFilterType
  = TagFilterType String
```

##### Instances
``` purescript
Newtype TagFilterType _
```

#### `TagLimitExceededException`

``` purescript
newtype TagLimitExceededException
  = TagLimitExceededException {  }
```

<p>The maximum allowed number of tags was exceeded.</p>

##### Instances
``` purescript
Newtype TagLimitExceededException _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TagRequiredException`

``` purescript
newtype TagRequiredException
  = TagRequiredException {  }
```

<p>A tag was not specified.</p>

##### Instances
``` purescript
Newtype TagRequiredException _
```

#### `TagSetListLimitExceededException`

``` purescript
newtype TagSetListLimitExceededException
  = TagSetListLimitExceededException {  }
```

<p>The number of tag groups included in the tag set list exceeded the maximum allowed limit of 3.</p>

##### Instances
``` purescript
Newtype TagSetListLimitExceededException _
```

#### `TargetGroupInfo`

``` purescript
newtype TargetGroupInfo
  = TargetGroupInfo { "Name'" :: NullOrUndefined (TargetGroupName) }
```

<p>Information about a target group in Elastic Load Balancing to use in a deployment. Instances are registered as targets in a target group, and traffic is routed to the target group.</p>

##### Instances
``` purescript
Newtype TargetGroupInfo _
```

#### `TargetGroupInfoList`

``` purescript
newtype TargetGroupInfoList
  = TargetGroupInfoList (Array TargetGroupInfo)
```

##### Instances
``` purescript
Newtype TargetGroupInfoList _
```

#### `TargetGroupName`

``` purescript
newtype TargetGroupName
  = TargetGroupName String
```

##### Instances
``` purescript
Newtype TargetGroupName _
```

#### `TargetInstances`

``` purescript
newtype TargetInstances
  = TargetInstances { "TagFilters'" :: NullOrUndefined (EC2TagFilterList), "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList), "Ec2TagSet'" :: NullOrUndefined (EC2TagSet) }
```

<p>Information about the instances to be used in the replacement environment in a blue/green deployment.</p>

##### Instances
``` purescript
Newtype TargetInstances _
```

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException {  }
```

<p>An API function was called too frequently.</p>

##### Instances
``` purescript
Newtype ThrottlingException _
```

#### `TimeBasedCanary`

``` purescript
newtype TimeBasedCanary
  = TimeBasedCanary { "CanaryPercentage'" :: NullOrUndefined (Percentage), "CanaryInterval'" :: NullOrUndefined (WaitTimeInMins) }
```

<p>A configuration that shifts traffic from one version of a Lambda function to another in two increments. The original and target Lambda function versions are specified in the deployment's AppSpec file.</p>

##### Instances
``` purescript
Newtype TimeBasedCanary _
```

#### `TimeBasedLinear`

``` purescript
newtype TimeBasedLinear
  = TimeBasedLinear { "LinearPercentage'" :: NullOrUndefined (Percentage), "LinearInterval'" :: NullOrUndefined (WaitTimeInMins) }
```

<p>A configuration that shifts traffic from one version of a Lambda function to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions are specified in the deployment's AppSpec file.</p>

##### Instances
``` purescript
Newtype TimeBasedLinear _
```

#### `TimeRange`

``` purescript
newtype TimeRange
  = TimeRange { "Start'" :: NullOrUndefined (Number), "End'" :: NullOrUndefined (Number) }
```

<p>Information about a time range.</p>

##### Instances
``` purescript
Newtype TimeRange _
```

#### `TrafficRoutingConfig`

``` purescript
newtype TrafficRoutingConfig
  = TrafficRoutingConfig { "Type'" :: NullOrUndefined (TrafficRoutingType), "TimeBasedCanary'" :: NullOrUndefined (TimeBasedCanary), "TimeBasedLinear'" :: NullOrUndefined (TimeBasedLinear) }
```

<p>The configuration that specifies how traffic is shifted from one version of a Lambda function to another version during an AWS Lambda deployment.</p>

##### Instances
``` purescript
Newtype TrafficRoutingConfig _
```

#### `TrafficRoutingType`

``` purescript
newtype TrafficRoutingType
  = TrafficRoutingType String
```

##### Instances
``` purescript
Newtype TrafficRoutingType _
```

#### `TriggerConfig`

``` purescript
newtype TriggerConfig
  = TriggerConfig { "TriggerName'" :: NullOrUndefined (TriggerName), "TriggerTargetArn'" :: NullOrUndefined (TriggerTargetArn), "TriggerEvents'" :: NullOrUndefined (TriggerEventTypeList) }
```

<p>Information about notification triggers for the deployment group.</p>

##### Instances
``` purescript
Newtype TriggerConfig _
```

#### `TriggerConfigList`

``` purescript
newtype TriggerConfigList
  = TriggerConfigList (Array TriggerConfig)
```

##### Instances
``` purescript
Newtype TriggerConfigList _
```

#### `TriggerEventType`

``` purescript
newtype TriggerEventType
  = TriggerEventType String
```

##### Instances
``` purescript
Newtype TriggerEventType _
```

#### `TriggerEventTypeList`

``` purescript
newtype TriggerEventTypeList
  = TriggerEventTypeList (Array TriggerEventType)
```

##### Instances
``` purescript
Newtype TriggerEventTypeList _
```

#### `TriggerName`

``` purescript
newtype TriggerName
  = TriggerName String
```

##### Instances
``` purescript
Newtype TriggerName _
```

#### `TriggerTargetArn`

``` purescript
newtype TriggerTargetArn
  = TriggerTargetArn String
```

##### Instances
``` purescript
Newtype TriggerTargetArn _
```

#### `TriggerTargetsLimitExceededException`

``` purescript
newtype TriggerTargetsLimitExceededException
  = TriggerTargetsLimitExceededException {  }
```

<p>The maximum allowed number of triggers was exceeded.</p>

##### Instances
``` purescript
Newtype TriggerTargetsLimitExceededException _
```

#### `UnsupportedActionForDeploymentTypeException`

``` purescript
newtype UnsupportedActionForDeploymentTypeException
  = UnsupportedActionForDeploymentTypeException {  }
```

<p>A call was submitted that is not supported for the specified deployment type.</p>

##### Instances
``` purescript
Newtype UnsupportedActionForDeploymentTypeException _
```

#### `UpdateApplicationInput`

``` purescript
newtype UpdateApplicationInput
  = UpdateApplicationInput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "NewApplicationName'" :: NullOrUndefined (ApplicationName) }
```

<p>Represents the input of an UpdateApplication operation.</p>

##### Instances
``` purescript
Newtype UpdateApplicationInput _
```

#### `UpdateDeploymentGroupInput`

``` purescript
newtype UpdateDeploymentGroupInput
  = UpdateDeploymentGroupInput { "ApplicationName'" :: ApplicationName, "CurrentDeploymentGroupName'" :: DeploymentGroupName, "NewDeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList), "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList), "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList), "ServiceRoleArn'" :: NullOrUndefined (Role), "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList), "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle), "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration), "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo), "Ec2TagSet'" :: NullOrUndefined (EC2TagSet), "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet) }
```

<p>Represents the input of an UpdateDeploymentGroup operation.</p>

##### Instances
``` purescript
Newtype UpdateDeploymentGroupInput _
```

#### `UpdateDeploymentGroupOutput`

``` purescript
newtype UpdateDeploymentGroupOutput
  = UpdateDeploymentGroupOutput { "HooksNotCleanedUp'" :: NullOrUndefined (AutoScalingGroupList) }
```

<p>Represents the output of an UpdateDeploymentGroup operation.</p>

##### Instances
``` purescript
Newtype UpdateDeploymentGroupOutput _
```

#### `Value`

``` purescript
newtype Value
  = Value String
```

##### Instances
``` purescript
Newtype Value _
```

#### `VersionId`

``` purescript
newtype VersionId
  = VersionId String
```

##### Instances
``` purescript
Newtype VersionId _
```

#### `WaitTimeInMins`

``` purescript
newtype WaitTimeInMins
  = WaitTimeInMins Int
```

##### Instances
``` purescript
Newtype WaitTimeInMins _
```


