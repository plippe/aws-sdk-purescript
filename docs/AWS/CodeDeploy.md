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

#### `AdditionalDeploymentStatusInfo`

``` purescript
newtype AdditionalDeploymentStatusInfo
  = AdditionalDeploymentStatusInfo String
```

#### `Alarm`

``` purescript
newtype Alarm
  = Alarm { "Name'" :: NullOrUndefined (AlarmName) }
```

<p>Information about an alarm.</p>

#### `AlarmConfiguration`

``` purescript
newtype AlarmConfiguration
  = AlarmConfiguration { "Enabled'" :: NullOrUndefined (Boolean), "IgnorePollAlarmFailure'" :: NullOrUndefined (Boolean), "Alarms'" :: NullOrUndefined (AlarmList) }
```

<p>Information about alarms associated with the deployment group.</p>

#### `AlarmList`

``` purescript
newtype AlarmList
  = AlarmList (Array Alarm)
```

#### `AlarmName`

``` purescript
newtype AlarmName
  = AlarmName String
```

#### `AlarmsLimitExceededException`

``` purescript
newtype AlarmsLimitExceededException
  = AlarmsLimitExceededException {  }
```

<p>The maximum number of alarms for a deployment group (10) was exceeded.</p>

#### `ApplicationAlreadyExistsException`

``` purescript
newtype ApplicationAlreadyExistsException
  = ApplicationAlreadyExistsException {  }
```

<p>An application with the specified name already exists with the applicable IAM user or AWS account.</p>

#### `ApplicationDoesNotExistException`

``` purescript
newtype ApplicationDoesNotExistException
  = ApplicationDoesNotExistException {  }
```

<p>The application does not exist with the applicable IAM user or AWS account.</p>

#### `ApplicationId`

``` purescript
newtype ApplicationId
  = ApplicationId String
```

#### `ApplicationInfo`

``` purescript
newtype ApplicationInfo
  = ApplicationInfo { "ApplicationId'" :: NullOrUndefined (ApplicationId), "ApplicationName'" :: NullOrUndefined (ApplicationName), "CreateTime'" :: NullOrUndefined (Number), "LinkedToGitHub'" :: NullOrUndefined (Boolean), "GitHubAccountName'" :: NullOrUndefined (GitHubAccountTokenName), "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Information about an application.</p>

#### `ApplicationLimitExceededException`

``` purescript
newtype ApplicationLimitExceededException
  = ApplicationLimitExceededException {  }
```

<p>More applications were attempted to be created than are allowed.</p>

#### `ApplicationName`

``` purescript
newtype ApplicationName
  = ApplicationName String
```

#### `ApplicationNameRequiredException`

``` purescript
newtype ApplicationNameRequiredException
  = ApplicationNameRequiredException {  }
```

<p>The minimum number of required application names was not specified.</p>

#### `ApplicationRevisionSortBy`

``` purescript
newtype ApplicationRevisionSortBy
  = ApplicationRevisionSortBy String
```

#### `ApplicationsInfoList`

``` purescript
newtype ApplicationsInfoList
  = ApplicationsInfoList (Array ApplicationInfo)
```

#### `ApplicationsList`

``` purescript
newtype ApplicationsList
  = ApplicationsList (Array ApplicationName)
```

#### `AutoRollbackConfiguration`

``` purescript
newtype AutoRollbackConfiguration
  = AutoRollbackConfiguration { "Enabled'" :: NullOrUndefined (Boolean), "Events'" :: NullOrUndefined (AutoRollbackEventsList) }
```

<p>Information about a configuration for automatically rolling back to a previous version of an application revision when a deployment doesn't complete successfully.</p>

#### `AutoRollbackEvent`

``` purescript
newtype AutoRollbackEvent
  = AutoRollbackEvent String
```

#### `AutoRollbackEventsList`

``` purescript
newtype AutoRollbackEventsList
  = AutoRollbackEventsList (Array AutoRollbackEvent)
```

#### `AutoScalingGroup`

``` purescript
newtype AutoScalingGroup
  = AutoScalingGroup { "Name'" :: NullOrUndefined (AutoScalingGroupName), "Hook'" :: NullOrUndefined (AutoScalingGroupHook) }
```

<p>Information about an Auto Scaling group.</p>

#### `AutoScalingGroupHook`

``` purescript
newtype AutoScalingGroupHook
  = AutoScalingGroupHook String
```

#### `AutoScalingGroupList`

``` purescript
newtype AutoScalingGroupList
  = AutoScalingGroupList (Array AutoScalingGroup)
```

#### `AutoScalingGroupName`

``` purescript
newtype AutoScalingGroupName
  = AutoScalingGroupName String
```

#### `AutoScalingGroupNameList`

``` purescript
newtype AutoScalingGroupNameList
  = AutoScalingGroupNameList (Array AutoScalingGroupName)
```

#### `BatchGetApplicationRevisionsInput`

``` purescript
newtype BatchGetApplicationRevisionsInput
  = BatchGetApplicationRevisionsInput { "ApplicationName'" :: ApplicationName, "Revisions'" :: RevisionLocationList }
```

<p>Represents the input of a BatchGetApplicationRevisions operation.</p>

#### `BatchGetApplicationRevisionsOutput`

``` purescript
newtype BatchGetApplicationRevisionsOutput
  = BatchGetApplicationRevisionsOutput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "ErrorMessage'" :: NullOrUndefined (ErrorMessage), "Revisions'" :: NullOrUndefined (RevisionInfoList) }
```

<p>Represents the output of a BatchGetApplicationRevisions operation.</p>

#### `BatchGetApplicationsInput`

``` purescript
newtype BatchGetApplicationsInput
  = BatchGetApplicationsInput { "ApplicationNames'" :: ApplicationsList }
```

<p>Represents the input of a BatchGetApplications operation.</p>

#### `BatchGetApplicationsOutput`

``` purescript
newtype BatchGetApplicationsOutput
  = BatchGetApplicationsOutput { "ApplicationsInfo'" :: NullOrUndefined (ApplicationsInfoList) }
```

<p>Represents the output of a BatchGetApplications operation.</p>

#### `BatchGetDeploymentGroupsInput`

``` purescript
newtype BatchGetDeploymentGroupsInput
  = BatchGetDeploymentGroupsInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupNames'" :: DeploymentGroupsList }
```

<p>Represents the input of a BatchGetDeploymentGroups operation.</p>

#### `BatchGetDeploymentGroupsOutput`

``` purescript
newtype BatchGetDeploymentGroupsOutput
  = BatchGetDeploymentGroupsOutput { "DeploymentGroupsInfo'" :: NullOrUndefined (DeploymentGroupInfoList), "ErrorMessage'" :: NullOrUndefined (ErrorMessage) }
```

<p>Represents the output of a BatchGetDeploymentGroups operation.</p>

#### `BatchGetDeploymentInstancesInput`

``` purescript
newtype BatchGetDeploymentInstancesInput
  = BatchGetDeploymentInstancesInput { "DeploymentId'" :: DeploymentId, "InstanceIds'" :: InstancesList }
```

<p>Represents the input of a BatchGetDeploymentInstances operation.</p>

#### `BatchGetDeploymentInstancesOutput`

``` purescript
newtype BatchGetDeploymentInstancesOutput
  = BatchGetDeploymentInstancesOutput { "InstancesSummary'" :: NullOrUndefined (InstanceSummaryList), "ErrorMessage'" :: NullOrUndefined (ErrorMessage) }
```

<p>Represents the output of a BatchGetDeploymentInstances operation.</p>

#### `BatchGetDeploymentsInput`

``` purescript
newtype BatchGetDeploymentsInput
  = BatchGetDeploymentsInput { "DeploymentIds'" :: DeploymentsList }
```

<p>Represents the input of a BatchGetDeployments operation.</p>

#### `BatchGetDeploymentsOutput`

``` purescript
newtype BatchGetDeploymentsOutput
  = BatchGetDeploymentsOutput { "DeploymentsInfo'" :: NullOrUndefined (DeploymentsInfoList) }
```

<p>Represents the output of a BatchGetDeployments operation.</p>

#### `BatchGetOnPremisesInstancesInput`

``` purescript
newtype BatchGetOnPremisesInstancesInput
  = BatchGetOnPremisesInstancesInput { "InstanceNames'" :: InstanceNameList }
```

<p>Represents the input of a BatchGetOnPremisesInstances operation.</p>

#### `BatchGetOnPremisesInstancesOutput`

``` purescript
newtype BatchGetOnPremisesInstancesOutput
  = BatchGetOnPremisesInstancesOutput { "InstanceInfos'" :: NullOrUndefined (InstanceInfoList) }
```

<p>Represents the output of a BatchGetOnPremisesInstances operation.</p>

#### `BatchLimitExceededException`

``` purescript
newtype BatchLimitExceededException
  = BatchLimitExceededException {  }
```

<p>The maximum number of names or IDs allowed for this request (100) was exceeded.</p>

#### `BlueGreenDeploymentConfiguration`

``` purescript
newtype BlueGreenDeploymentConfiguration
  = BlueGreenDeploymentConfiguration { "TerminateBlueInstancesOnDeploymentSuccess'" :: NullOrUndefined (BlueInstanceTerminationOption), "DeploymentReadyOption'" :: NullOrUndefined (DeploymentReadyOption), "GreenFleetProvisioningOption'" :: NullOrUndefined (GreenFleetProvisioningOption) }
```

<p>Information about blue/green deployment options for a deployment group.</p>

#### `BlueInstanceTerminationOption`

``` purescript
newtype BlueInstanceTerminationOption
  = BlueInstanceTerminationOption { "Action'" :: NullOrUndefined (InstanceAction), "TerminationWaitTimeInMinutes'" :: NullOrUndefined (Duration) }
```

<p>Information about whether instances in the original environment are terminated when a blue/green deployment is successful.</p>

#### `BucketNameFilterRequiredException`

``` purescript
newtype BucketNameFilterRequiredException
  = BucketNameFilterRequiredException {  }
```

<p>A bucket name is required, but was not provided.</p>

#### `BundleType`

``` purescript
newtype BundleType
  = BundleType String
```

#### `CommitId`

``` purescript
newtype CommitId
  = CommitId String
```

#### `ComputePlatform`

``` purescript
newtype ComputePlatform
  = ComputePlatform String
```

#### `ContinueDeploymentInput`

``` purescript
newtype ContinueDeploymentInput
  = ContinueDeploymentInput { "DeploymentId'" :: NullOrUndefined (DeploymentId) }
```

#### `CreateApplicationInput`

``` purescript
newtype CreateApplicationInput
  = CreateApplicationInput { "ApplicationName'" :: ApplicationName, "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Represents the input of a CreateApplication operation.</p>

#### `CreateApplicationOutput`

``` purescript
newtype CreateApplicationOutput
  = CreateApplicationOutput { "ApplicationId'" :: NullOrUndefined (ApplicationId) }
```

<p>Represents the output of a CreateApplication operation.</p>

#### `CreateDeploymentConfigInput`

``` purescript
newtype CreateDeploymentConfigInput
  = CreateDeploymentConfigInput { "DeploymentConfigName'" :: DeploymentConfigName, "MinimumHealthyHosts'" :: NullOrUndefined (MinimumHealthyHosts), "TrafficRoutingConfig'" :: NullOrUndefined (TrafficRoutingConfig), "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Represents the input of a CreateDeploymentConfig operation.</p>

#### `CreateDeploymentConfigOutput`

``` purescript
newtype CreateDeploymentConfigOutput
  = CreateDeploymentConfigOutput { "DeploymentConfigId'" :: NullOrUndefined (DeploymentConfigId) }
```

<p>Represents the output of a CreateDeploymentConfig operation.</p>

#### `CreateDeploymentGroupInput`

``` purescript
newtype CreateDeploymentGroupInput
  = CreateDeploymentGroupInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupName'" :: DeploymentGroupName, "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList), "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList), "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList), "ServiceRoleArn'" :: Role, "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList), "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle), "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration), "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo), "Ec2TagSet'" :: NullOrUndefined (EC2TagSet), "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet) }
```

<p>Represents the input of a CreateDeploymentGroup operation.</p>

#### `CreateDeploymentGroupOutput`

``` purescript
newtype CreateDeploymentGroupOutput
  = CreateDeploymentGroupOutput { "DeploymentGroupId'" :: NullOrUndefined (DeploymentGroupId) }
```

<p>Represents the output of a CreateDeploymentGroup operation.</p>

#### `CreateDeploymentInput`

``` purescript
newtype CreateDeploymentInput
  = CreateDeploymentInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "Revision'" :: NullOrUndefined (RevisionLocation), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "Description'" :: NullOrUndefined (Description), "IgnoreApplicationStopFailures'" :: NullOrUndefined (Boolean), "TargetInstances'" :: NullOrUndefined (TargetInstances), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "UpdateOutdatedInstancesOnly'" :: NullOrUndefined (Boolean), "FileExistsBehavior'" :: NullOrUndefined (FileExistsBehavior) }
```

<p>Represents the input of a CreateDeployment operation.</p>

#### `CreateDeploymentOutput`

``` purescript
newtype CreateDeploymentOutput
  = CreateDeploymentOutput { "DeploymentId'" :: NullOrUndefined (DeploymentId) }
```

<p>Represents the output of a CreateDeployment operation.</p>

#### `DeleteApplicationInput`

``` purescript
newtype DeleteApplicationInput
  = DeleteApplicationInput { "ApplicationName'" :: ApplicationName }
```

<p>Represents the input of a DeleteApplication operation.</p>

#### `DeleteDeploymentConfigInput`

``` purescript
newtype DeleteDeploymentConfigInput
  = DeleteDeploymentConfigInput { "DeploymentConfigName'" :: DeploymentConfigName }
```

<p>Represents the input of a DeleteDeploymentConfig operation.</p>

#### `DeleteDeploymentGroupInput`

``` purescript
newtype DeleteDeploymentGroupInput
  = DeleteDeploymentGroupInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupName'" :: DeploymentGroupName }
```

<p>Represents the input of a DeleteDeploymentGroup operation.</p>

#### `DeleteDeploymentGroupOutput`

``` purescript
newtype DeleteDeploymentGroupOutput
  = DeleteDeploymentGroupOutput { "HooksNotCleanedUp'" :: NullOrUndefined (AutoScalingGroupList) }
```

<p>Represents the output of a DeleteDeploymentGroup operation.</p>

#### `DeleteGitHubAccountTokenInput`

``` purescript
newtype DeleteGitHubAccountTokenInput
  = DeleteGitHubAccountTokenInput { "TokenName'" :: NullOrUndefined (GitHubAccountTokenName) }
```

<p>Represents the input of a DeleteGitHubAccount operation.</p>

#### `DeleteGitHubAccountTokenOutput`

``` purescript
newtype DeleteGitHubAccountTokenOutput
  = DeleteGitHubAccountTokenOutput { "TokenName'" :: NullOrUndefined (GitHubAccountTokenName) }
```

<p>Represents the output of a DeleteGitHubAccountToken operation.</p>

#### `DeploymentAlreadyCompletedException`

``` purescript
newtype DeploymentAlreadyCompletedException
  = DeploymentAlreadyCompletedException {  }
```

<p>The deployment is already complete.</p>

#### `DeploymentConfigAlreadyExistsException`

``` purescript
newtype DeploymentConfigAlreadyExistsException
  = DeploymentConfigAlreadyExistsException {  }
```

<p>A deployment configuration with the specified name already exists with the applicable IAM user or AWS account.</p>

#### `DeploymentConfigDoesNotExistException`

``` purescript
newtype DeploymentConfigDoesNotExistException
  = DeploymentConfigDoesNotExistException {  }
```

<p>The deployment configuration does not exist with the applicable IAM user or AWS account.</p>

#### `DeploymentConfigId`

``` purescript
newtype DeploymentConfigId
  = DeploymentConfigId String
```

#### `DeploymentConfigInUseException`

``` purescript
newtype DeploymentConfigInUseException
  = DeploymentConfigInUseException {  }
```

<p>The deployment configuration is still in use.</p>

#### `DeploymentConfigInfo`

``` purescript
newtype DeploymentConfigInfo
  = DeploymentConfigInfo { "DeploymentConfigId'" :: NullOrUndefined (DeploymentConfigId), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "MinimumHealthyHosts'" :: NullOrUndefined (MinimumHealthyHosts), "CreateTime'" :: NullOrUndefined (Number), "ComputePlatform'" :: NullOrUndefined (ComputePlatform), "TrafficRoutingConfig'" :: NullOrUndefined (TrafficRoutingConfig) }
```

<p>Information about a deployment configuration.</p>

#### `DeploymentConfigLimitExceededException`

``` purescript
newtype DeploymentConfigLimitExceededException
  = DeploymentConfigLimitExceededException {  }
```

<p>The deployment configurations limit was exceeded.</p>

#### `DeploymentConfigName`

``` purescript
newtype DeploymentConfigName
  = DeploymentConfigName String
```

#### `DeploymentConfigNameRequiredException`

``` purescript
newtype DeploymentConfigNameRequiredException
  = DeploymentConfigNameRequiredException {  }
```

<p>The deployment configuration name was not specified.</p>

#### `DeploymentConfigsList`

``` purescript
newtype DeploymentConfigsList
  = DeploymentConfigsList (Array DeploymentConfigName)
```

#### `DeploymentCreator`

``` purescript
newtype DeploymentCreator
  = DeploymentCreator String
```

#### `DeploymentDoesNotExistException`

``` purescript
newtype DeploymentDoesNotExistException
  = DeploymentDoesNotExistException {  }
```

<p>The deployment does not exist with the applicable IAM user or AWS account.</p>

#### `DeploymentGroupAlreadyExistsException`

``` purescript
newtype DeploymentGroupAlreadyExistsException
  = DeploymentGroupAlreadyExistsException {  }
```

<p>A deployment group with the specified name already exists with the applicable IAM user or AWS account.</p>

#### `DeploymentGroupDoesNotExistException`

``` purescript
newtype DeploymentGroupDoesNotExistException
  = DeploymentGroupDoesNotExistException {  }
```

<p>The named deployment group does not exist with the applicable IAM user or AWS account.</p>

#### `DeploymentGroupId`

``` purescript
newtype DeploymentGroupId
  = DeploymentGroupId String
```

#### `DeploymentGroupInfo`

``` purescript
newtype DeploymentGroupInfo
  = DeploymentGroupInfo { "ApplicationName'" :: NullOrUndefined (ApplicationName), "DeploymentGroupId'" :: NullOrUndefined (DeploymentGroupId), "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList), "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList), "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupList), "ServiceRoleArn'" :: NullOrUndefined (Role), "TargetRevision'" :: NullOrUndefined (RevisionLocation), "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList), "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle), "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration), "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo), "LastSuccessfulDeployment'" :: NullOrUndefined (LastDeploymentInfo), "LastAttemptedDeployment'" :: NullOrUndefined (LastDeploymentInfo), "Ec2TagSet'" :: NullOrUndefined (EC2TagSet), "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet), "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Information about a deployment group.</p>

#### `DeploymentGroupInfoList`

``` purescript
newtype DeploymentGroupInfoList
  = DeploymentGroupInfoList (Array DeploymentGroupInfo)
```

#### `DeploymentGroupLimitExceededException`

``` purescript
newtype DeploymentGroupLimitExceededException
  = DeploymentGroupLimitExceededException {  }
```

<p> The deployment groups limit was exceeded.</p>

#### `DeploymentGroupName`

``` purescript
newtype DeploymentGroupName
  = DeploymentGroupName String
```

#### `DeploymentGroupNameRequiredException`

``` purescript
newtype DeploymentGroupNameRequiredException
  = DeploymentGroupNameRequiredException {  }
```

<p>The deployment group name was not specified.</p>

#### `DeploymentGroupsList`

``` purescript
newtype DeploymentGroupsList
  = DeploymentGroupsList (Array DeploymentGroupName)
```

#### `DeploymentId`

``` purescript
newtype DeploymentId
  = DeploymentId String
```

#### `DeploymentIdRequiredException`

``` purescript
newtype DeploymentIdRequiredException
  = DeploymentIdRequiredException {  }
```

<p>At least one deployment ID must be specified.</p>

#### `DeploymentInfo`

``` purescript
newtype DeploymentInfo
  = DeploymentInfo { "ApplicationName'" :: NullOrUndefined (ApplicationName), "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "DeploymentId'" :: NullOrUndefined (DeploymentId), "PreviousRevision'" :: NullOrUndefined (RevisionLocation), "Revision'" :: NullOrUndefined (RevisionLocation), "Status'" :: NullOrUndefined (DeploymentStatus), "ErrorInformation'" :: NullOrUndefined (ErrorInformation), "CreateTime'" :: NullOrUndefined (Number), "StartTime'" :: NullOrUndefined (Number), "CompleteTime'" :: NullOrUndefined (Number), "DeploymentOverview'" :: NullOrUndefined (DeploymentOverview), "Description'" :: NullOrUndefined (Description), "Creator'" :: NullOrUndefined (DeploymentCreator), "IgnoreApplicationStopFailures'" :: NullOrUndefined (Boolean), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "UpdateOutdatedInstancesOnly'" :: NullOrUndefined (Boolean), "RollbackInfo'" :: NullOrUndefined (RollbackInfo), "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle), "TargetInstances'" :: NullOrUndefined (TargetInstances), "InstanceTerminationWaitTimeStarted'" :: NullOrUndefined (Boolean), "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration), "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo), "AdditionalDeploymentStatusInfo'" :: NullOrUndefined (AdditionalDeploymentStatusInfo), "FileExistsBehavior'" :: NullOrUndefined (FileExistsBehavior), "DeploymentStatusMessages'" :: NullOrUndefined (DeploymentStatusMessageList), "ComputePlatform'" :: NullOrUndefined (ComputePlatform) }
```

<p>Information about a deployment.</p>

#### `DeploymentIsNotInReadyStateException`

``` purescript
newtype DeploymentIsNotInReadyStateException
  = DeploymentIsNotInReadyStateException {  }
```

<p>The deployment does not have a status of Ready and can't continue yet.</p>

#### `DeploymentLimitExceededException`

``` purescript
newtype DeploymentLimitExceededException
  = DeploymentLimitExceededException {  }
```

<p>The number of allowed deployments was exceeded.</p>

#### `DeploymentNotStartedException`

``` purescript
newtype DeploymentNotStartedException
  = DeploymentNotStartedException {  }
```

<p>The specified deployment has not started.</p>

#### `DeploymentOption`

``` purescript
newtype DeploymentOption
  = DeploymentOption String
```

#### `DeploymentOverview`

``` purescript
newtype DeploymentOverview
  = DeploymentOverview { "Pending" :: NullOrUndefined (InstanceCount), "InProgress" :: NullOrUndefined (InstanceCount), "Succeeded" :: NullOrUndefined (InstanceCount), "Failed" :: NullOrUndefined (InstanceCount), "Skipped" :: NullOrUndefined (InstanceCount), "Ready" :: NullOrUndefined (InstanceCount) }
```

<p>Information about the deployment status of the instances in the deployment.</p>

#### `DeploymentReadyAction`

``` purescript
newtype DeploymentReadyAction
  = DeploymentReadyAction String
```

#### `DeploymentReadyOption`

``` purescript
newtype DeploymentReadyOption
  = DeploymentReadyOption { "ActionOnTimeout'" :: NullOrUndefined (DeploymentReadyAction), "WaitTimeInMinutes'" :: NullOrUndefined (Duration) }
```

<p>Information about how traffic is rerouted to instances in a replacement environment in a blue/green deployment.</p>

#### `DeploymentStatus`

``` purescript
newtype DeploymentStatus
  = DeploymentStatus String
```

#### `DeploymentStatusList`

``` purescript
newtype DeploymentStatusList
  = DeploymentStatusList (Array DeploymentStatus)
```

#### `DeploymentStatusMessageList`

``` purescript
newtype DeploymentStatusMessageList
  = DeploymentStatusMessageList (Array ErrorMessage)
```

#### `DeploymentStyle`

``` purescript
newtype DeploymentStyle
  = DeploymentStyle { "DeploymentType'" :: NullOrUndefined (DeploymentType), "DeploymentOption'" :: NullOrUndefined (DeploymentOption) }
```

<p>Information about the type of deployment, either in-place or blue/green, you want to run and whether to route deployment traffic behind a load balancer.</p>

#### `DeploymentType`

``` purescript
newtype DeploymentType
  = DeploymentType String
```

#### `DeploymentsInfoList`

``` purescript
newtype DeploymentsInfoList
  = DeploymentsInfoList (Array DeploymentInfo)
```

#### `DeploymentsList`

``` purescript
newtype DeploymentsList
  = DeploymentsList (Array DeploymentId)
```

#### `DeregisterOnPremisesInstanceInput`

``` purescript
newtype DeregisterOnPremisesInstanceInput
  = DeregisterOnPremisesInstanceInput { "InstanceName'" :: InstanceName }
```

<p>Represents the input of a DeregisterOnPremisesInstance operation.</p>

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `DescriptionTooLongException`

``` purescript
newtype DescriptionTooLongException
  = DescriptionTooLongException {  }
```

<p>The description is too long.</p>

#### `Diagnostics`

``` purescript
newtype Diagnostics
  = Diagnostics { "ErrorCode'" :: NullOrUndefined (LifecycleErrorCode), "ScriptName'" :: NullOrUndefined (ScriptName), "Message'" :: NullOrUndefined (LifecycleMessage), "LogTail'" :: NullOrUndefined (LogTail) }
```

<p>Diagnostic information about executable scripts that are part of a deployment.</p>

#### `Duration`

``` purescript
newtype Duration
  = Duration Int
```

#### `EC2TagFilter`

``` purescript
newtype EC2TagFilter
  = EC2TagFilter { "Key" :: NullOrUndefined (Key), "Value" :: NullOrUndefined (Value), "Type" :: NullOrUndefined (EC2TagFilterType) }
```

<p>Information about an EC2 tag filter.</p>

#### `EC2TagFilterList`

``` purescript
newtype EC2TagFilterList
  = EC2TagFilterList (Array EC2TagFilter)
```

#### `EC2TagFilterType`

``` purescript
newtype EC2TagFilterType
  = EC2TagFilterType String
```

#### `EC2TagSet`

``` purescript
newtype EC2TagSet
  = EC2TagSet { "Ec2TagSetList'" :: NullOrUndefined (EC2TagSetList) }
```

<p>Information about groups of EC2 instance tags.</p>

#### `EC2TagSetList`

``` purescript
newtype EC2TagSetList
  = EC2TagSetList (Array EC2TagFilterList)
```

#### `ELBInfo`

``` purescript
newtype ELBInfo
  = ELBInfo { "Name'" :: NullOrUndefined (ELBName) }
```

<p>Information about a load balancer in Elastic Load Balancing to use in a deployment. Instances are registered directly with a load balancer, and traffic is routed to the load balancer.</p>

#### `ELBInfoList`

``` purescript
newtype ELBInfoList
  = ELBInfoList (Array ELBInfo)
```

#### `ELBName`

``` purescript
newtype ELBName
  = ELBName String
```

#### `ETag`

``` purescript
newtype ETag
  = ETag String
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

#### `ErrorInformation`

``` purescript
newtype ErrorInformation
  = ErrorInformation { "Code'" :: NullOrUndefined (ErrorCode), "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Information about a deployment error.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `FileExistsBehavior`

``` purescript
newtype FileExistsBehavior
  = FileExistsBehavior String
```

#### `GenericRevisionInfo`

``` purescript
newtype GenericRevisionInfo
  = GenericRevisionInfo { "Description'" :: NullOrUndefined (Description), "DeploymentGroups'" :: NullOrUndefined (DeploymentGroupsList), "FirstUsedTime'" :: NullOrUndefined (Number), "LastUsedTime'" :: NullOrUndefined (Number), "RegisterTime'" :: NullOrUndefined (Number) }
```

<p>Information about an application revision.</p>

#### `GetApplicationInput`

``` purescript
newtype GetApplicationInput
  = GetApplicationInput { "ApplicationName'" :: ApplicationName }
```

<p>Represents the input of a GetApplication operation.</p>

#### `GetApplicationOutput`

``` purescript
newtype GetApplicationOutput
  = GetApplicationOutput { "Application'" :: NullOrUndefined (ApplicationInfo) }
```

<p>Represents the output of a GetApplication operation.</p>

#### `GetApplicationRevisionInput`

``` purescript
newtype GetApplicationRevisionInput
  = GetApplicationRevisionInput { "ApplicationName'" :: ApplicationName, "Revision'" :: RevisionLocation }
```

<p>Represents the input of a GetApplicationRevision operation.</p>

#### `GetApplicationRevisionOutput`

``` purescript
newtype GetApplicationRevisionOutput
  = GetApplicationRevisionOutput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "Revision'" :: NullOrUndefined (RevisionLocation), "RevisionInfo'" :: NullOrUndefined (GenericRevisionInfo) }
```

<p>Represents the output of a GetApplicationRevision operation.</p>

#### `GetDeploymentConfigInput`

``` purescript
newtype GetDeploymentConfigInput
  = GetDeploymentConfigInput { "DeploymentConfigName'" :: DeploymentConfigName }
```

<p>Represents the input of a GetDeploymentConfig operation.</p>

#### `GetDeploymentConfigOutput`

``` purescript
newtype GetDeploymentConfigOutput
  = GetDeploymentConfigOutput { "DeploymentConfigInfo'" :: NullOrUndefined (DeploymentConfigInfo) }
```

<p>Represents the output of a GetDeploymentConfig operation.</p>

#### `GetDeploymentGroupInput`

``` purescript
newtype GetDeploymentGroupInput
  = GetDeploymentGroupInput { "ApplicationName'" :: ApplicationName, "DeploymentGroupName'" :: DeploymentGroupName }
```

<p>Represents the input of a GetDeploymentGroup operation.</p>

#### `GetDeploymentGroupOutput`

``` purescript
newtype GetDeploymentGroupOutput
  = GetDeploymentGroupOutput { "DeploymentGroupInfo'" :: NullOrUndefined (DeploymentGroupInfo) }
```

<p>Represents the output of a GetDeploymentGroup operation.</p>

#### `GetDeploymentInput`

``` purescript
newtype GetDeploymentInput
  = GetDeploymentInput { "DeploymentId'" :: DeploymentId }
```

<p>Represents the input of a GetDeployment operation.</p>

#### `GetDeploymentInstanceInput`

``` purescript
newtype GetDeploymentInstanceInput
  = GetDeploymentInstanceInput { "DeploymentId'" :: DeploymentId, "InstanceId'" :: InstanceId }
```

<p>Represents the input of a GetDeploymentInstance operation.</p>

#### `GetDeploymentInstanceOutput`

``` purescript
newtype GetDeploymentInstanceOutput
  = GetDeploymentInstanceOutput { "InstanceSummary'" :: NullOrUndefined (InstanceSummary) }
```

<p>Represents the output of a GetDeploymentInstance operation.</p>

#### `GetDeploymentOutput`

``` purescript
newtype GetDeploymentOutput
  = GetDeploymentOutput { "DeploymentInfo'" :: NullOrUndefined (DeploymentInfo) }
```

<p>Represents the output of a GetDeployment operation.</p>

#### `GetOnPremisesInstanceInput`

``` purescript
newtype GetOnPremisesInstanceInput
  = GetOnPremisesInstanceInput { "InstanceName'" :: InstanceName }
```

<p>Represents the input of a GetOnPremisesInstance operation.</p>

#### `GetOnPremisesInstanceOutput`

``` purescript
newtype GetOnPremisesInstanceOutput
  = GetOnPremisesInstanceOutput { "InstanceInfo'" :: NullOrUndefined (InstanceInfo) }
```

<p>Represents the output of a GetOnPremisesInstance operation.</p>

#### `GitHubAccountTokenDoesNotExistException`

``` purescript
newtype GitHubAccountTokenDoesNotExistException
  = GitHubAccountTokenDoesNotExistException {  }
```

<p>No GitHub account connection exists with the named specified in the call.</p>

#### `GitHubAccountTokenName`

``` purescript
newtype GitHubAccountTokenName
  = GitHubAccountTokenName String
```

#### `GitHubAccountTokenNameList`

``` purescript
newtype GitHubAccountTokenNameList
  = GitHubAccountTokenNameList (Array GitHubAccountTokenName)
```

#### `GitHubAccountTokenNameRequiredException`

``` purescript
newtype GitHubAccountTokenNameRequiredException
  = GitHubAccountTokenNameRequiredException {  }
```

<p>The call is missing a required GitHub account connection name.</p>

#### `GitHubLocation`

``` purescript
newtype GitHubLocation
  = GitHubLocation { "Repository'" :: NullOrUndefined (Repository), "CommitId'" :: NullOrUndefined (CommitId) }
```

<p>Information about the location of application artifacts stored in GitHub.</p>

#### `GreenFleetProvisioningAction`

``` purescript
newtype GreenFleetProvisioningAction
  = GreenFleetProvisioningAction String
```

#### `GreenFleetProvisioningOption`

``` purescript
newtype GreenFleetProvisioningOption
  = GreenFleetProvisioningOption { "Action'" :: NullOrUndefined (GreenFleetProvisioningAction) }
```

<p>Information about the instances that belong to the replacement environment in a blue/green deployment.</p>

#### `IamArnRequiredException`

``` purescript
newtype IamArnRequiredException
  = IamArnRequiredException {  }
```

<p>No IAM ARN was included in the request. You must use an IAM session ARN or IAM user ARN in the request.</p>

#### `IamSessionArn`

``` purescript
newtype IamSessionArn
  = IamSessionArn String
```

#### `IamSessionArnAlreadyRegisteredException`

``` purescript
newtype IamSessionArnAlreadyRegisteredException
  = IamSessionArnAlreadyRegisteredException {  }
```

<p>The request included an IAM session ARN that has already been used to register a different instance.</p>

#### `IamUserArn`

``` purescript
newtype IamUserArn
  = IamUserArn String
```

#### `IamUserArnAlreadyRegisteredException`

``` purescript
newtype IamUserArnAlreadyRegisteredException
  = IamUserArnAlreadyRegisteredException {  }
```

<p>The specified IAM user ARN is already registered with an on-premises instance.</p>

#### `IamUserArnRequiredException`

``` purescript
newtype IamUserArnRequiredException
  = IamUserArnRequiredException {  }
```

<p>An IAM user ARN was not specified.</p>

#### `InstanceAction`

``` purescript
newtype InstanceAction
  = InstanceAction String
```

#### `InstanceArn`

``` purescript
newtype InstanceArn
  = InstanceArn String
```

#### `InstanceCount`

``` purescript
newtype InstanceCount
  = InstanceCount Number
```

#### `InstanceDoesNotExistException`

``` purescript
newtype InstanceDoesNotExistException
  = InstanceDoesNotExistException {  }
```

<p>The specified instance does not exist in the deployment group.</p>

#### `InstanceId`

``` purescript
newtype InstanceId
  = InstanceId String
```

#### `InstanceIdRequiredException`

``` purescript
newtype InstanceIdRequiredException
  = InstanceIdRequiredException {  }
```

<p>The instance ID was not specified.</p>

#### `InstanceInfo`

``` purescript
newtype InstanceInfo
  = InstanceInfo { "InstanceName'" :: NullOrUndefined (InstanceName), "IamSessionArn'" :: NullOrUndefined (IamSessionArn), "IamUserArn'" :: NullOrUndefined (IamUserArn), "InstanceArn'" :: NullOrUndefined (InstanceArn), "RegisterTime'" :: NullOrUndefined (Number), "DeregisterTime'" :: NullOrUndefined (Number), "Tags'" :: NullOrUndefined (TagList) }
```

<p>Information about an on-premises instance.</p>

#### `InstanceInfoList`

``` purescript
newtype InstanceInfoList
  = InstanceInfoList (Array InstanceInfo)
```

#### `InstanceLimitExceededException`

``` purescript
newtype InstanceLimitExceededException
  = InstanceLimitExceededException {  }
```

<p>The maximum number of allowed on-premises instances in a single call was exceeded.</p>

#### `InstanceName`

``` purescript
newtype InstanceName
  = InstanceName String
```

#### `InstanceNameAlreadyRegisteredException`

``` purescript
newtype InstanceNameAlreadyRegisteredException
  = InstanceNameAlreadyRegisteredException {  }
```

<p>The specified on-premises instance name is already registered.</p>

#### `InstanceNameList`

``` purescript
newtype InstanceNameList
  = InstanceNameList (Array InstanceName)
```

#### `InstanceNameRequiredException`

``` purescript
newtype InstanceNameRequiredException
  = InstanceNameRequiredException {  }
```

<p>An on-premises instance name was not specified.</p>

#### `InstanceNotRegisteredException`

``` purescript
newtype InstanceNotRegisteredException
  = InstanceNotRegisteredException {  }
```

<p>The specified on-premises instance is not registered.</p>

#### `InstanceStatus`

``` purescript
newtype InstanceStatus
  = InstanceStatus String
```

#### `InstanceStatusList`

``` purescript
newtype InstanceStatusList
  = InstanceStatusList (Array InstanceStatus)
```

#### `InstanceSummary`

``` purescript
newtype InstanceSummary
  = InstanceSummary { "DeploymentId'" :: NullOrUndefined (DeploymentId), "InstanceId'" :: NullOrUndefined (InstanceId), "Status'" :: NullOrUndefined (InstanceStatus), "LastUpdatedAt'" :: NullOrUndefined (Number), "LifecycleEvents'" :: NullOrUndefined (LifecycleEventList), "InstanceType'" :: NullOrUndefined (InstanceType) }
```

<p>Information about an instance in a deployment.</p>

#### `InstanceSummaryList`

``` purescript
newtype InstanceSummaryList
  = InstanceSummaryList (Array InstanceSummary)
```

#### `InstanceType`

``` purescript
newtype InstanceType
  = InstanceType String
```

#### `InstanceTypeList`

``` purescript
newtype InstanceTypeList
  = InstanceTypeList (Array InstanceType)
```

#### `InstancesList`

``` purescript
newtype InstancesList
  = InstancesList (Array InstanceId)
```

#### `InvalidAlarmConfigException`

``` purescript
newtype InvalidAlarmConfigException
  = InvalidAlarmConfigException {  }
```

<p>The format of the alarm configuration is invalid. Possible causes include:</p> <ul> <li> <p>The alarm list is null.</p> </li> <li> <p>The alarm object is null.</p> </li> <li> <p>The alarm name is empty or null or exceeds the 255 character limit.</p> </li> <li> <p>Two alarms with the same name have been specified.</p> </li> <li> <p>The alarm configuration is enabled but the alarm list is empty.</p> </li> </ul>

#### `InvalidApplicationNameException`

``` purescript
newtype InvalidApplicationNameException
  = InvalidApplicationNameException {  }
```

<p>The application name was specified in an invalid format.</p>

#### `InvalidAutoRollbackConfigException`

``` purescript
newtype InvalidAutoRollbackConfigException
  = InvalidAutoRollbackConfigException {  }
```

<p>The automatic rollback configuration was specified in an invalid format. For example, automatic rollback is enabled but an invalid triggering event type or no event types were listed.</p>

#### `InvalidAutoScalingGroupException`

``` purescript
newtype InvalidAutoScalingGroupException
  = InvalidAutoScalingGroupException {  }
```

<p>The Auto Scaling group was specified in an invalid format or does not exist.</p>

#### `InvalidBlueGreenDeploymentConfigurationException`

``` purescript
newtype InvalidBlueGreenDeploymentConfigurationException
  = InvalidBlueGreenDeploymentConfigurationException {  }
```

<p>The configuration for the blue/green deployment group was provided in an invalid format. For information about deployment configuration format, see <a>CreateDeploymentConfig</a>.</p>

#### `InvalidBucketNameFilterException`

``` purescript
newtype InvalidBucketNameFilterException
  = InvalidBucketNameFilterException {  }
```

<p>The bucket name either doesn't exist or was specified in an invalid format.</p>

#### `InvalidComputePlatformException`

``` purescript
newtype InvalidComputePlatformException
  = InvalidComputePlatformException {  }
```

<p>The computePlatform is invalid. The computePlatform should be <code>Lambda</code> or <code>Server</code>.</p>

#### `InvalidDeployedStateFilterException`

``` purescript
newtype InvalidDeployedStateFilterException
  = InvalidDeployedStateFilterException {  }
```

<p>The deployed state filter was specified in an invalid format.</p>

#### `InvalidDeploymentConfigNameException`

``` purescript
newtype InvalidDeploymentConfigNameException
  = InvalidDeploymentConfigNameException {  }
```

<p>The deployment configuration name was specified in an invalid format.</p>

#### `InvalidDeploymentGroupNameException`

``` purescript
newtype InvalidDeploymentGroupNameException
  = InvalidDeploymentGroupNameException {  }
```

<p>The deployment group name was specified in an invalid format.</p>

#### `InvalidDeploymentIdException`

``` purescript
newtype InvalidDeploymentIdException
  = InvalidDeploymentIdException {  }
```

<p>At least one of the deployment IDs was specified in an invalid format.</p>

#### `InvalidDeploymentInstanceTypeException`

``` purescript
newtype InvalidDeploymentInstanceTypeException
  = InvalidDeploymentInstanceTypeException {  }
```

<p>An instance type was specified for an in-place deployment. Instance types are supported for blue/green deployments only.</p>

#### `InvalidDeploymentStatusException`

``` purescript
newtype InvalidDeploymentStatusException
  = InvalidDeploymentStatusException {  }
```

<p>The specified deployment status doesn't exist or cannot be determined.</p>

#### `InvalidDeploymentStyleException`

``` purescript
newtype InvalidDeploymentStyleException
  = InvalidDeploymentStyleException {  }
```

<p>An invalid deployment style was specified. Valid deployment types include "IN_PLACE" and "BLUE_GREEN". Valid deployment options include "WITH_TRAFFIC_CONTROL" and "WITHOUT_TRAFFIC_CONTROL".</p>

#### `InvalidEC2TagCombinationException`

``` purescript
newtype InvalidEC2TagCombinationException
  = InvalidEC2TagCombinationException {  }
```

<p>A call was submitted that specified both Ec2TagFilters and Ec2TagSet, but only one of these data types can be used in a single call.</p>

#### `InvalidEC2TagException`

``` purescript
newtype InvalidEC2TagException
  = InvalidEC2TagException {  }
```

<p>The tag was specified in an invalid format.</p>

#### `InvalidFileExistsBehaviorException`

``` purescript
newtype InvalidFileExistsBehaviorException
  = InvalidFileExistsBehaviorException {  }
```

<p>An invalid fileExistsBehavior option was specified to determine how AWS CodeDeploy handles files or directories that already exist in a deployment target location but weren't part of the previous successful deployment. Valid values include "DISALLOW", "OVERWRITE", and "RETAIN".</p>

#### `InvalidGitHubAccountTokenNameException`

``` purescript
newtype InvalidGitHubAccountTokenNameException
  = InvalidGitHubAccountTokenNameException {  }
```

<p>The format of the specified GitHub account connection name is invalid.</p>

#### `InvalidIamSessionArnException`

``` purescript
newtype InvalidIamSessionArnException
  = InvalidIamSessionArnException {  }
```

<p>The IAM session ARN was specified in an invalid format.</p>

#### `InvalidIamUserArnException`

``` purescript
newtype InvalidIamUserArnException
  = InvalidIamUserArnException {  }
```

<p>The IAM user ARN was specified in an invalid format.</p>

#### `InvalidIgnoreApplicationStopFailuresValueException`

``` purescript
newtype InvalidIgnoreApplicationStopFailuresValueException
  = InvalidIgnoreApplicationStopFailuresValueException {  }
```

<p>The IgnoreApplicationStopFailures value is invalid. For AWS Lambda deployments, <code>false</code> is expected. For EC2/On-premises deployments, <code>true</code> or <code>false</code> is expected.</p>

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException {  }
```

<p>The specified input was specified in an invalid format.</p>

#### `InvalidInstanceIdException`

``` purescript
newtype InvalidInstanceIdException
  = InvalidInstanceIdException {  }
```

<p> </p>

#### `InvalidInstanceNameException`

``` purescript
newtype InvalidInstanceNameException
  = InvalidInstanceNameException {  }
```

<p>The specified on-premises instance name was specified in an invalid format.</p>

#### `InvalidInstanceStatusException`

``` purescript
newtype InvalidInstanceStatusException
  = InvalidInstanceStatusException {  }
```

<p>The specified instance status does not exist.</p>

#### `InvalidInstanceTypeException`

``` purescript
newtype InvalidInstanceTypeException
  = InvalidInstanceTypeException {  }
```

<p>An invalid instance type was specified for instances in a blue/green deployment. Valid values include "Blue" for an original environment and "Green" for a replacement environment.</p>

#### `InvalidKeyPrefixFilterException`

``` purescript
newtype InvalidKeyPrefixFilterException
  = InvalidKeyPrefixFilterException {  }
```

<p>The specified key prefix filter was specified in an invalid format.</p>

#### `InvalidLifecycleEventHookExecutionIdException`

``` purescript
newtype InvalidLifecycleEventHookExecutionIdException
  = InvalidLifecycleEventHookExecutionIdException {  }
```

<p>A lifecycle event hook is invalid. Review the <code>hooks</code> section in your AppSpec file to ensure the lifecycle events and <code>hooks</code> functions are valid.</p>

#### `InvalidLifecycleEventHookExecutionStatusException`

``` purescript
newtype InvalidLifecycleEventHookExecutionStatusException
  = InvalidLifecycleEventHookExecutionStatusException {  }
```

<p>The result of a Lambda validation function that verifies a lifecycle event is invalid. It should return <code>Succeeded</code> or <code>Failed</code>.</p>

#### `InvalidLoadBalancerInfoException`

``` purescript
newtype InvalidLoadBalancerInfoException
  = InvalidLoadBalancerInfoException {  }
```

<p>An invalid load balancer name, or no load balancer name, was specified.</p>

#### `InvalidMinimumHealthyHostValueException`

``` purescript
newtype InvalidMinimumHealthyHostValueException
  = InvalidMinimumHealthyHostValueException {  }
```

<p>The minimum healthy instance value was specified in an invalid format.</p>

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException {  }
```

<p>The next token was specified in an invalid format.</p>

#### `InvalidOnPremisesTagCombinationException`

``` purescript
newtype InvalidOnPremisesTagCombinationException
  = InvalidOnPremisesTagCombinationException {  }
```

<p>A call was submitted that specified both OnPremisesTagFilters and OnPremisesTagSet, but only one of these data types can be used in a single call.</p>

#### `InvalidOperationException`

``` purescript
newtype InvalidOperationException
  = InvalidOperationException {  }
```

<p>An invalid operation was detected.</p>

#### `InvalidRegistrationStatusException`

``` purescript
newtype InvalidRegistrationStatusException
  = InvalidRegistrationStatusException {  }
```

<p>The registration status was specified in an invalid format.</p>

#### `InvalidRevisionException`

``` purescript
newtype InvalidRevisionException
  = InvalidRevisionException {  }
```

<p>The revision was specified in an invalid format.</p>

#### `InvalidRoleException`

``` purescript
newtype InvalidRoleException
  = InvalidRoleException {  }
```

<p>The service role ARN was specified in an invalid format. Or, if an Auto Scaling group was specified, the specified service role does not grant the appropriate permissions to Auto Scaling.</p>

#### `InvalidSortByException`

``` purescript
newtype InvalidSortByException
  = InvalidSortByException {  }
```

<p>The column name to sort by is either not present or was specified in an invalid format.</p>

#### `InvalidSortOrderException`

``` purescript
newtype InvalidSortOrderException
  = InvalidSortOrderException {  }
```

<p>The sort order was specified in an invalid format.</p>

#### `InvalidTagException`

``` purescript
newtype InvalidTagException
  = InvalidTagException {  }
```

<p>The specified tag was specified in an invalid format.</p>

#### `InvalidTagFilterException`

``` purescript
newtype InvalidTagFilterException
  = InvalidTagFilterException {  }
```

<p>The specified tag filter was specified in an invalid format.</p>

#### `InvalidTargetInstancesException`

``` purescript
newtype InvalidTargetInstancesException
  = InvalidTargetInstancesException {  }
```

<p>The target instance configuration is invalid. Possible causes include:</p> <ul> <li> <p>Configuration data for target instances was entered for an in-place deployment.</p> </li> <li> <p>The limit of 10 tags for a tag type was exceeded.</p> </li> <li> <p>The combined length of the tag names exceeded the limit. </p> </li> <li> <p>A specified tag is not currently applied to any instances.</p> </li> </ul>

#### `InvalidTimeRangeException`

``` purescript
newtype InvalidTimeRangeException
  = InvalidTimeRangeException {  }
```

<p>The specified time range was specified in an invalid format.</p>

#### `InvalidTrafficRoutingConfigurationException`

``` purescript
newtype InvalidTrafficRoutingConfigurationException
  = InvalidTrafficRoutingConfigurationException {  }
```

<p> The configuration that specifies how traffic is routed during a deployment is invalid.</p>

#### `InvalidTriggerConfigException`

``` purescript
newtype InvalidTriggerConfigException
  = InvalidTriggerConfigException {  }
```

<p>The trigger was specified in an invalid format.</p>

#### `InvalidUpdateOutdatedInstancesOnlyValueException`

``` purescript
newtype InvalidUpdateOutdatedInstancesOnlyValueException
  = InvalidUpdateOutdatedInstancesOnlyValueException {  }
```

<p>The UpdateOutdatedInstancesOnly value is invalid. For AWS Lambda deployments, <code>false</code> is expected. For EC2/On-premises deployments, <code>true</code> or <code>false</code> is expected.</p>

#### `Key`

``` purescript
newtype Key
  = Key String
```

#### `LastDeploymentInfo`

``` purescript
newtype LastDeploymentInfo
  = LastDeploymentInfo { "DeploymentId'" :: NullOrUndefined (DeploymentId), "Status'" :: NullOrUndefined (DeploymentStatus), "EndTime'" :: NullOrUndefined (Number), "CreateTime'" :: NullOrUndefined (Number) }
```

<p>Information about the most recent attempted or successful deployment to a deployment group.</p>

#### `LifecycleErrorCode`

``` purescript
newtype LifecycleErrorCode
  = LifecycleErrorCode String
```

#### `LifecycleEvent`

``` purescript
newtype LifecycleEvent
  = LifecycleEvent { "LifecycleEventName'" :: NullOrUndefined (LifecycleEventName), "Diagnostics'" :: NullOrUndefined (Diagnostics), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "Status'" :: NullOrUndefined (LifecycleEventStatus) }
```

<p>Information about a deployment lifecycle event.</p>

#### `LifecycleEventAlreadyCompletedException`

``` purescript
newtype LifecycleEventAlreadyCompletedException
  = LifecycleEventAlreadyCompletedException {  }
```

<p>An attempt to return the status of an already completed lifecycle event occurred.</p>

#### `LifecycleEventHookExecutionId`

``` purescript
newtype LifecycleEventHookExecutionId
  = LifecycleEventHookExecutionId String
```

#### `LifecycleEventList`

``` purescript
newtype LifecycleEventList
  = LifecycleEventList (Array LifecycleEvent)
```

#### `LifecycleEventName`

``` purescript
newtype LifecycleEventName
  = LifecycleEventName String
```

#### `LifecycleEventStatus`

``` purescript
newtype LifecycleEventStatus
  = LifecycleEventStatus String
```

#### `LifecycleHookLimitExceededException`

``` purescript
newtype LifecycleHookLimitExceededException
  = LifecycleHookLimitExceededException {  }
```

<p>The limit for lifecycle hooks was exceeded.</p>

#### `LifecycleMessage`

``` purescript
newtype LifecycleMessage
  = LifecycleMessage String
```

#### `ListApplicationRevisionsInput`

``` purescript
newtype ListApplicationRevisionsInput
  = ListApplicationRevisionsInput { "ApplicationName'" :: ApplicationName, "SortBy'" :: NullOrUndefined (ApplicationRevisionSortBy), "SortOrder'" :: NullOrUndefined (SortOrder), "S3Bucket'" :: NullOrUndefined (S3Bucket), "S3KeyPrefix'" :: NullOrUndefined (S3Key), "Deployed'" :: NullOrUndefined (ListStateFilterAction), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListApplicationRevisions operation.</p>

#### `ListApplicationRevisionsOutput`

``` purescript
newtype ListApplicationRevisionsOutput
  = ListApplicationRevisionsOutput { "Revisions'" :: NullOrUndefined (RevisionLocationList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListApplicationRevisions operation.</p>

#### `ListApplicationsInput`

``` purescript
newtype ListApplicationsInput
  = ListApplicationsInput { "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListApplications operation.</p>

#### `ListApplicationsOutput`

``` purescript
newtype ListApplicationsOutput
  = ListApplicationsOutput { "Applications'" :: NullOrUndefined (ApplicationsList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListApplications operation.</p>

#### `ListDeploymentConfigsInput`

``` purescript
newtype ListDeploymentConfigsInput
  = ListDeploymentConfigsInput { "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListDeploymentConfigs operation.</p>

#### `ListDeploymentConfigsOutput`

``` purescript
newtype ListDeploymentConfigsOutput
  = ListDeploymentConfigsOutput { "DeploymentConfigsList'" :: NullOrUndefined (DeploymentConfigsList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListDeploymentConfigs operation.</p>

#### `ListDeploymentGroupsInput`

``` purescript
newtype ListDeploymentGroupsInput
  = ListDeploymentGroupsInput { "ApplicationName'" :: ApplicationName, "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListDeploymentGroups operation.</p>

#### `ListDeploymentGroupsOutput`

``` purescript
newtype ListDeploymentGroupsOutput
  = ListDeploymentGroupsOutput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "DeploymentGroups'" :: NullOrUndefined (DeploymentGroupsList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListDeploymentGroups operation.</p>

#### `ListDeploymentInstancesInput`

``` purescript
newtype ListDeploymentInstancesInput
  = ListDeploymentInstancesInput { "DeploymentId'" :: DeploymentId, "NextToken'" :: NullOrUndefined (NextToken), "InstanceStatusFilter'" :: NullOrUndefined (InstanceStatusList), "InstanceTypeFilter'" :: NullOrUndefined (InstanceTypeList) }
```

<p>Represents the input of a ListDeploymentInstances operation.</p>

#### `ListDeploymentInstancesOutput`

``` purescript
newtype ListDeploymentInstancesOutput
  = ListDeploymentInstancesOutput { "InstancesList'" :: NullOrUndefined (InstancesList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListDeploymentInstances operation.</p>

#### `ListDeploymentsInput`

``` purescript
newtype ListDeploymentsInput
  = ListDeploymentsInput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "DeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "IncludeOnlyStatuses'" :: NullOrUndefined (DeploymentStatusList), "CreateTimeRange'" :: NullOrUndefined (TimeRange), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListDeployments operation.</p>

#### `ListDeploymentsOutput`

``` purescript
newtype ListDeploymentsOutput
  = ListDeploymentsOutput { "Deployments'" :: NullOrUndefined (DeploymentsList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListDeployments operation.</p>

#### `ListGitHubAccountTokenNamesInput`

``` purescript
newtype ListGitHubAccountTokenNamesInput
  = ListGitHubAccountTokenNamesInput { "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListGitHubAccountTokenNames operation.</p>

#### `ListGitHubAccountTokenNamesOutput`

``` purescript
newtype ListGitHubAccountTokenNamesOutput
  = ListGitHubAccountTokenNamesOutput { "TokenNameList'" :: NullOrUndefined (GitHubAccountTokenNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListGitHubAccountTokenNames operation.</p>

#### `ListOnPremisesInstancesInput`

``` purescript
newtype ListOnPremisesInstancesInput
  = ListOnPremisesInstancesInput { "RegistrationStatus'" :: NullOrUndefined (RegistrationStatus), "TagFilters'" :: NullOrUndefined (TagFilterList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListOnPremisesInstances operation.</p>

#### `ListOnPremisesInstancesOutput`

``` purescript
newtype ListOnPremisesInstancesOutput
  = ListOnPremisesInstancesOutput { "InstanceNames'" :: NullOrUndefined (InstanceNameList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of list on-premises instances operation.</p>

#### `ListStateFilterAction`

``` purescript
newtype ListStateFilterAction
  = ListStateFilterAction String
```

#### `LoadBalancerInfo`

``` purescript
newtype LoadBalancerInfo
  = LoadBalancerInfo { "ElbInfoList'" :: NullOrUndefined (ELBInfoList), "TargetGroupInfoList'" :: NullOrUndefined (TargetGroupInfoList) }
```

<p>Information about the Elastic Load Balancing load balancer or target group used in a deployment.</p>

#### `LogTail`

``` purescript
newtype LogTail
  = LogTail String
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `MinimumHealthyHosts`

``` purescript
newtype MinimumHealthyHosts
  = MinimumHealthyHosts { "Value'" :: NullOrUndefined (MinimumHealthyHostsValue), "Type'" :: NullOrUndefined (MinimumHealthyHostsType) }
```

<p>Information about minimum healthy instance.</p>

#### `MinimumHealthyHostsType`

``` purescript
newtype MinimumHealthyHostsType
  = MinimumHealthyHostsType String
```

#### `MinimumHealthyHostsValue`

``` purescript
newtype MinimumHealthyHostsValue
  = MinimumHealthyHostsValue Int
```

#### `MultipleIamArnsProvidedException`

``` purescript
newtype MultipleIamArnsProvidedException
  = MultipleIamArnsProvidedException {  }
```

<p>Both an IAM user ARN and an IAM session ARN were included in the request. Use only one ARN type.</p>

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `NullableBoolean`

``` purescript
newtype NullableBoolean
  = NullableBoolean Boolean
```

#### `OnPremisesTagSet`

``` purescript
newtype OnPremisesTagSet
  = OnPremisesTagSet { "OnPremisesTagSetList'" :: NullOrUndefined (OnPremisesTagSetList) }
```

<p>Information about groups of on-premises instance tags.</p>

#### `OnPremisesTagSetList`

``` purescript
newtype OnPremisesTagSetList
  = OnPremisesTagSetList (Array TagFilterList)
```

#### `OperationNotSupportedException`

``` purescript
newtype OperationNotSupportedException
  = OperationNotSupportedException {  }
```

<p>The API used does not support the deployment.</p>

#### `Percentage`

``` purescript
newtype Percentage
  = Percentage Int
```

#### `PutLifecycleEventHookExecutionStatusInput`

``` purescript
newtype PutLifecycleEventHookExecutionStatusInput
  = PutLifecycleEventHookExecutionStatusInput { "DeploymentId'" :: NullOrUndefined (DeploymentId), "LifecycleEventHookExecutionId'" :: NullOrUndefined (LifecycleEventHookExecutionId), "Status'" :: NullOrUndefined (LifecycleEventStatus) }
```

#### `PutLifecycleEventHookExecutionStatusOutput`

``` purescript
newtype PutLifecycleEventHookExecutionStatusOutput
  = PutLifecycleEventHookExecutionStatusOutput { "LifecycleEventHookExecutionId'" :: NullOrUndefined (LifecycleEventHookExecutionId) }
```

#### `RawString`

``` purescript
newtype RawString
  = RawString { "Content'" :: NullOrUndefined (RawStringContent), "Sha256'" :: NullOrUndefined (RawStringSha256) }
```

<p>A revision for an AWS Lambda deployment that is a YAML-formatted or JSON-formatted string. For AWS Lambda deployments, the revision is the same as the AppSpec file.</p>

#### `RawStringContent`

``` purescript
newtype RawStringContent
  = RawStringContent String
```

#### `RawStringSha256`

``` purescript
newtype RawStringSha256
  = RawStringSha256 String
```

#### `RegisterApplicationRevisionInput`

``` purescript
newtype RegisterApplicationRevisionInput
  = RegisterApplicationRevisionInput { "ApplicationName'" :: ApplicationName, "Description'" :: NullOrUndefined (Description), "Revision'" :: RevisionLocation }
```

<p>Represents the input of a RegisterApplicationRevision operation.</p>

#### `RegisterOnPremisesInstanceInput`

``` purescript
newtype RegisterOnPremisesInstanceInput
  = RegisterOnPremisesInstanceInput { "InstanceName'" :: InstanceName, "IamSessionArn'" :: NullOrUndefined (IamSessionArn), "IamUserArn'" :: NullOrUndefined (IamUserArn) }
```

<p>Represents the input of the register on-premises instance operation.</p>

#### `RegistrationStatus`

``` purescript
newtype RegistrationStatus
  = RegistrationStatus String
```

#### `RemoveTagsFromOnPremisesInstancesInput`

``` purescript
newtype RemoveTagsFromOnPremisesInstancesInput
  = RemoveTagsFromOnPremisesInstancesInput { "Tags'" :: TagList, "InstanceNames'" :: InstanceNameList }
```

<p>Represents the input of a RemoveTagsFromOnPremisesInstances operation.</p>

#### `Repository`

``` purescript
newtype Repository
  = Repository String
```

#### `ResourceValidationException`

``` purescript
newtype ResourceValidationException
  = ResourceValidationException {  }
```

<p>The specified resource could not be validated.</p>

#### `RevisionDoesNotExistException`

``` purescript
newtype RevisionDoesNotExistException
  = RevisionDoesNotExistException {  }
```

<p>The named revision does not exist with the applicable IAM user or AWS account.</p>

#### `RevisionInfo`

``` purescript
newtype RevisionInfo
  = RevisionInfo { "RevisionLocation'" :: NullOrUndefined (RevisionLocation), "GenericRevisionInfo'" :: NullOrUndefined (GenericRevisionInfo) }
```

<p>Information about an application revision.</p>

#### `RevisionInfoList`

``` purescript
newtype RevisionInfoList
  = RevisionInfoList (Array RevisionInfo)
```

#### `RevisionLocation`

``` purescript
newtype RevisionLocation
  = RevisionLocation { "RevisionType'" :: NullOrUndefined (RevisionLocationType), "S3Location'" :: NullOrUndefined (S3Location), "GitHubLocation'" :: NullOrUndefined (GitHubLocation), "String" :: NullOrUndefined (RawString) }
```

<p>Information about the location of an application revision.</p>

#### `RevisionLocationList`

``` purescript
newtype RevisionLocationList
  = RevisionLocationList (Array RevisionLocation)
```

#### `RevisionLocationType`

``` purescript
newtype RevisionLocationType
  = RevisionLocationType String
```

#### `RevisionRequiredException`

``` purescript
newtype RevisionRequiredException
  = RevisionRequiredException {  }
```

<p>The revision ID was not specified.</p>

#### `Role`

``` purescript
newtype Role
  = Role String
```

#### `RoleRequiredException`

``` purescript
newtype RoleRequiredException
  = RoleRequiredException {  }
```

<p>The role ID was not specified.</p>

#### `RollbackInfo`

``` purescript
newtype RollbackInfo
  = RollbackInfo { "RollbackDeploymentId'" :: NullOrUndefined (DeploymentId), "RollbackTriggeringDeploymentId'" :: NullOrUndefined (DeploymentId), "RollbackMessage'" :: NullOrUndefined (Description) }
```

<p>Information about a deployment rollback.</p>

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
  = S3Location { "Bucket'" :: NullOrUndefined (S3Bucket), "Key'" :: NullOrUndefined (S3Key), "BundleType'" :: NullOrUndefined (BundleType), "Version'" :: NullOrUndefined (VersionId), "ETag'" :: NullOrUndefined (ETag) }
```

<p>Information about the location of application artifacts stored in Amazon S3.</p>

#### `ScriptName`

``` purescript
newtype ScriptName
  = ScriptName String
```

#### `SkipWaitTimeForInstanceTerminationInput`

``` purescript
newtype SkipWaitTimeForInstanceTerminationInput
  = SkipWaitTimeForInstanceTerminationInput { "DeploymentId'" :: NullOrUndefined (DeploymentId) }
```

#### `SortOrder`

``` purescript
newtype SortOrder
  = SortOrder String
```

#### `StopDeploymentInput`

``` purescript
newtype StopDeploymentInput
  = StopDeploymentInput { "DeploymentId'" :: DeploymentId, "AutoRollbackEnabled'" :: NullOrUndefined (NullableBoolean) }
```

<p>Represents the input of a StopDeployment operation.</p>

#### `StopDeploymentOutput`

``` purescript
newtype StopDeploymentOutput
  = StopDeploymentOutput { "Status'" :: NullOrUndefined (StopStatus), "StatusMessage'" :: NullOrUndefined (Message) }
```

<p>Represents the output of a StopDeployment operation.</p>

#### `StopStatus`

``` purescript
newtype StopStatus
  = StopStatus String
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (Key), "Value" :: NullOrUndefined (Value) }
```

<p>Information about a tag.</p>

#### `TagFilter`

``` purescript
newtype TagFilter
  = TagFilter { "Key" :: NullOrUndefined (Key), "Value" :: NullOrUndefined (Value), "Type" :: NullOrUndefined (TagFilterType) }
```

<p>Information about an on-premises instance tag filter.</p>

#### `TagFilterList`

``` purescript
newtype TagFilterList
  = TagFilterList (Array TagFilter)
```

#### `TagFilterType`

``` purescript
newtype TagFilterType
  = TagFilterType String
```

#### `TagLimitExceededException`

``` purescript
newtype TagLimitExceededException
  = TagLimitExceededException {  }
```

<p>The maximum allowed number of tags was exceeded.</p>

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagRequiredException`

``` purescript
newtype TagRequiredException
  = TagRequiredException {  }
```

<p>A tag was not specified.</p>

#### `TagSetListLimitExceededException`

``` purescript
newtype TagSetListLimitExceededException
  = TagSetListLimitExceededException {  }
```

<p>The number of tag groups included in the tag set list exceeded the maximum allowed limit of 3.</p>

#### `TargetGroupInfo`

``` purescript
newtype TargetGroupInfo
  = TargetGroupInfo { "Name'" :: NullOrUndefined (TargetGroupName) }
```

<p>Information about a target group in Elastic Load Balancing to use in a deployment. Instances are registered as targets in a target group, and traffic is routed to the target group.</p>

#### `TargetGroupInfoList`

``` purescript
newtype TargetGroupInfoList
  = TargetGroupInfoList (Array TargetGroupInfo)
```

#### `TargetGroupName`

``` purescript
newtype TargetGroupName
  = TargetGroupName String
```

#### `TargetInstances`

``` purescript
newtype TargetInstances
  = TargetInstances { "TagFilters'" :: NullOrUndefined (EC2TagFilterList), "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList), "Ec2TagSet'" :: NullOrUndefined (EC2TagSet) }
```

<p>Information about the instances to be used in the replacement environment in a blue/green deployment.</p>

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException {  }
```

<p>An API function was called too frequently.</p>

#### `TimeBasedCanary`

``` purescript
newtype TimeBasedCanary
  = TimeBasedCanary { "CanaryPercentage'" :: NullOrUndefined (Percentage), "CanaryInterval'" :: NullOrUndefined (WaitTimeInMins) }
```

<p>A configuration that shifts traffic from one version of a Lambda function to another in two increments. The original and target Lambda function versions are specified in the deployment's AppSpec file.</p>

#### `TimeBasedLinear`

``` purescript
newtype TimeBasedLinear
  = TimeBasedLinear { "LinearPercentage'" :: NullOrUndefined (Percentage), "LinearInterval'" :: NullOrUndefined (WaitTimeInMins) }
```

<p>A configuration that shifts traffic from one version of a Lambda function to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions are specified in the deployment's AppSpec file.</p>

#### `TimeRange`

``` purescript
newtype TimeRange
  = TimeRange { "Start'" :: NullOrUndefined (Number), "End'" :: NullOrUndefined (Number) }
```

<p>Information about a time range.</p>

#### `TrafficRoutingConfig`

``` purescript
newtype TrafficRoutingConfig
  = TrafficRoutingConfig { "Type'" :: NullOrUndefined (TrafficRoutingType), "TimeBasedCanary'" :: NullOrUndefined (TimeBasedCanary), "TimeBasedLinear'" :: NullOrUndefined (TimeBasedLinear) }
```

<p>The configuration that specifies how traffic is shifted from one version of a Lambda function to another version during an AWS Lambda deployment.</p>

#### `TrafficRoutingType`

``` purescript
newtype TrafficRoutingType
  = TrafficRoutingType String
```

#### `TriggerConfig`

``` purescript
newtype TriggerConfig
  = TriggerConfig { "TriggerName'" :: NullOrUndefined (TriggerName), "TriggerTargetArn'" :: NullOrUndefined (TriggerTargetArn), "TriggerEvents'" :: NullOrUndefined (TriggerEventTypeList) }
```

<p>Information about notification triggers for the deployment group.</p>

#### `TriggerConfigList`

``` purescript
newtype TriggerConfigList
  = TriggerConfigList (Array TriggerConfig)
```

#### `TriggerEventType`

``` purescript
newtype TriggerEventType
  = TriggerEventType String
```

#### `TriggerEventTypeList`

``` purescript
newtype TriggerEventTypeList
  = TriggerEventTypeList (Array TriggerEventType)
```

#### `TriggerName`

``` purescript
newtype TriggerName
  = TriggerName String
```

#### `TriggerTargetArn`

``` purescript
newtype TriggerTargetArn
  = TriggerTargetArn String
```

#### `TriggerTargetsLimitExceededException`

``` purescript
newtype TriggerTargetsLimitExceededException
  = TriggerTargetsLimitExceededException {  }
```

<p>The maximum allowed number of triggers was exceeded.</p>

#### `UnsupportedActionForDeploymentTypeException`

``` purescript
newtype UnsupportedActionForDeploymentTypeException
  = UnsupportedActionForDeploymentTypeException {  }
```

<p>A call was submitted that is not supported for the specified deployment type.</p>

#### `UpdateApplicationInput`

``` purescript
newtype UpdateApplicationInput
  = UpdateApplicationInput { "ApplicationName'" :: NullOrUndefined (ApplicationName), "NewApplicationName'" :: NullOrUndefined (ApplicationName) }
```

<p>Represents the input of an UpdateApplication operation.</p>

#### `UpdateDeploymentGroupInput`

``` purescript
newtype UpdateDeploymentGroupInput
  = UpdateDeploymentGroupInput { "ApplicationName'" :: ApplicationName, "CurrentDeploymentGroupName'" :: DeploymentGroupName, "NewDeploymentGroupName'" :: NullOrUndefined (DeploymentGroupName), "DeploymentConfigName'" :: NullOrUndefined (DeploymentConfigName), "Ec2TagFilters'" :: NullOrUndefined (EC2TagFilterList), "OnPremisesInstanceTagFilters'" :: NullOrUndefined (TagFilterList), "AutoScalingGroups'" :: NullOrUndefined (AutoScalingGroupNameList), "ServiceRoleArn'" :: NullOrUndefined (Role), "TriggerConfigurations'" :: NullOrUndefined (TriggerConfigList), "AlarmConfiguration'" :: NullOrUndefined (AlarmConfiguration), "AutoRollbackConfiguration'" :: NullOrUndefined (AutoRollbackConfiguration), "DeploymentStyle'" :: NullOrUndefined (DeploymentStyle), "BlueGreenDeploymentConfiguration'" :: NullOrUndefined (BlueGreenDeploymentConfiguration), "LoadBalancerInfo'" :: NullOrUndefined (LoadBalancerInfo), "Ec2TagSet'" :: NullOrUndefined (EC2TagSet), "OnPremisesTagSet'" :: NullOrUndefined (OnPremisesTagSet) }
```

<p>Represents the input of an UpdateDeploymentGroup operation.</p>

#### `UpdateDeploymentGroupOutput`

``` purescript
newtype UpdateDeploymentGroupOutput
  = UpdateDeploymentGroupOutput { "HooksNotCleanedUp'" :: NullOrUndefined (AutoScalingGroupList) }
```

<p>Represents the output of an UpdateDeploymentGroup operation.</p>

#### `Value`

``` purescript
newtype Value
  = Value String
```

#### `VersionId`

``` purescript
newtype VersionId
  = VersionId String
```

#### `WaitTimeInMins`

``` purescript
newtype WaitTimeInMins
  = WaitTimeInMins Int
```


