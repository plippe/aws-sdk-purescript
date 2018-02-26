## Module AWS.Batch

<p>AWS Batch enables you to run batch computing workloads on the AWS Cloud. Batch computing is a common way for developers, scientists, and engineers to access large amounts of compute resources, and AWS Batch removes the undifferentiated heavy lifting of configuring and managing the required infrastructure. AWS Batch will be familiar to users of traditional batch computing software. This service can efficiently provision resources in response to jobs submitted in order to eliminate capacity constraints, reduce compute costs, and deliver results quickly.</p> <p>As a fully managed service, AWS Batch enables developers, scientists, and engineers to run batch computing workloads of any scale. AWS Batch automatically provisions compute resources and optimizes the workload distribution based on the quantity and scale of the workloads. With AWS Batch, there is no need to install or manage batch computing software, which allows you to focus on analyzing results and solving problems. AWS Batch reduces operational complexities, saves time, and reduces costs, which makes it easy for developers, scientists, and engineers to run their batch jobs in the AWS Cloud.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `cancelJob`

``` purescript
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: RequestError | eff) CancelJobResponse
```

<p>Cancels a job in an AWS Batch job queue. Jobs that are in the <code>SUBMITTED</code>, <code>PENDING</code>, or <code>RUNNABLE</code> state are cancelled. Jobs that have progressed to <code>STARTING</code> or <code>RUNNING</code> are not cancelled (but the API operation still succeeds, even if no job is cancelled); these jobs must be terminated with the <a>TerminateJob</a> operation.</p>

#### `createComputeEnvironment`

``` purescript
createComputeEnvironment :: forall eff. CreateComputeEnvironmentRequest -> Aff (err :: RequestError | eff) CreateComputeEnvironmentResponse
```

<p>Creates an AWS Batch compute environment. You can create <code>MANAGED</code> or <code>UNMANAGED</code> compute environments.</p> <p>In a managed compute environment, AWS Batch manages the compute resources within the environment, based on the compute resources that you specify. Instances launched into a managed compute environment use a recent, approved version of the Amazon ECS-optimized AMI. You can choose to use Amazon EC2 On-Demand Instances in your managed compute environment, or you can use Amazon EC2 Spot Instances that only launch when the Spot bid price is below a specified percentage of the On-Demand price.</p> <p>In an unmanaged compute environment, you can manage your own compute resources. This provides more compute resource configuration options, such as using a custom AMI, but you must ensure that your AMI meets the Amazon ECS container instance AMI specification. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/container_instance_AMIs.html">Container Instance AMIs</a> in the <i>Amazon Elastic Container Service Developer Guide</i>. After you have created your unmanaged compute environment, you can use the <a>DescribeComputeEnvironments</a> operation to find the Amazon ECS cluster that is associated with it and then manually launch your container instances into that Amazon ECS cluster. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_container_instance.html">Launching an Amazon ECS Container Instance</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `createJobQueue`

``` purescript
createJobQueue :: forall eff. CreateJobQueueRequest -> Aff (err :: RequestError | eff) CreateJobQueueResponse
```

<p>Creates an AWS Batch job queue. When you create a job queue, you associate one or more compute environments to the queue and assign an order of preference for the compute environments.</p> <p>You also set a priority to the job queue that determines the order in which the AWS Batch scheduler places jobs onto its associated compute environments. For example, if a compute environment is associated with more than one job queue, the job queue with a higher priority is given preference for scheduling jobs to that compute environment.</p>

#### `deleteComputeEnvironment`

``` purescript
deleteComputeEnvironment :: forall eff. DeleteComputeEnvironmentRequest -> Aff (err :: RequestError | eff) DeleteComputeEnvironmentResponse
```

<p>Deletes an AWS Batch compute environment.</p> <p>Before you can delete a compute environment, you must set its state to <code>DISABLED</code> with the <a>UpdateComputeEnvironment</a> API operation and disassociate it from any job queues with the <a>UpdateJobQueue</a> API operation.</p>

#### `deleteJobQueue`

``` purescript
deleteJobQueue :: forall eff. DeleteJobQueueRequest -> Aff (err :: RequestError | eff) DeleteJobQueueResponse
```

<p>Deletes the specified job queue. You must first disable submissions for a queue with the <a>UpdateJobQueue</a> operation. All jobs in the queue are terminated when you delete a job queue.</p> <p>It is not necessary to disassociate compute environments from a queue before submitting a <code>DeleteJobQueue</code> request. </p>

#### `deregisterJobDefinition`

``` purescript
deregisterJobDefinition :: forall eff. DeregisterJobDefinitionRequest -> Aff (err :: RequestError | eff) DeregisterJobDefinitionResponse
```

<p>Deregisters an AWS Batch job definition.</p>

#### `describeComputeEnvironments`

``` purescript
describeComputeEnvironments :: forall eff. DescribeComputeEnvironmentsRequest -> Aff (err :: RequestError | eff) DescribeComputeEnvironmentsResponse
```

<p>Describes one or more of your compute environments.</p> <p>If you are using an unmanaged compute environment, you can use the <code>DescribeComputeEnvironment</code> operation to determine the <code>ecsClusterArn</code> that you should launch your Amazon ECS container instances into.</p>

#### `describeJobDefinitions`

``` purescript
describeJobDefinitions :: forall eff. DescribeJobDefinitionsRequest -> Aff (err :: RequestError | eff) DescribeJobDefinitionsResponse
```

<p>Describes a list of job definitions. You can specify a <code>status</code> (such as <code>ACTIVE</code>) to only return job definitions that match that status.</p>

#### `describeJobQueues`

``` purescript
describeJobQueues :: forall eff. DescribeJobQueuesRequest -> Aff (err :: RequestError | eff) DescribeJobQueuesResponse
```

<p>Describes one or more of your job queues.</p>

#### `describeJobs`

``` purescript
describeJobs :: forall eff. DescribeJobsRequest -> Aff (err :: RequestError | eff) DescribeJobsResponse
```

<p>Describes a list of AWS Batch jobs.</p>

#### `listJobs`

``` purescript
listJobs :: forall eff. ListJobsRequest -> Aff (err :: RequestError | eff) ListJobsResponse
```

<p>Returns a list of task jobs for a specified job queue. You can filter the results by job status with the <code>jobStatus</code> parameter. If you do not specify a status, only <code>RUNNING</code> jobs are returned.</p>

#### `registerJobDefinition`

``` purescript
registerJobDefinition :: forall eff. RegisterJobDefinitionRequest -> Aff (err :: RequestError | eff) RegisterJobDefinitionResponse
```

<p>Registers an AWS Batch job definition. </p>

#### `submitJob`

``` purescript
submitJob :: forall eff. SubmitJobRequest -> Aff (err :: RequestError | eff) SubmitJobResponse
```

<p>Submits an AWS Batch job from a job definition. Parameters specified during <a>SubmitJob</a> override parameters defined in the job definition. </p>

#### `terminateJob`

``` purescript
terminateJob :: forall eff. TerminateJobRequest -> Aff (err :: RequestError | eff) TerminateJobResponse
```

<p>Terminates a job in a job queue. Jobs that are in the <code>STARTING</code> or <code>RUNNING</code> state are terminated, which causes them to transition to <code>FAILED</code>. Jobs that have not progressed to the <code>STARTING</code> state are cancelled.</p>

#### `updateComputeEnvironment`

``` purescript
updateComputeEnvironment :: forall eff. UpdateComputeEnvironmentRequest -> Aff (err :: RequestError | eff) UpdateComputeEnvironmentResponse
```

<p>Updates an AWS Batch compute environment.</p>

#### `updateJobQueue`

``` purescript
updateJobQueue :: forall eff. UpdateJobQueueRequest -> Aff (err :: RequestError | eff) UpdateJobQueueResponse
```

<p>Updates a job queue.</p>

#### `ArrayJobDependency`

``` purescript
newtype ArrayJobDependency
  = ArrayJobDependency String
```

#### `ArrayJobStatusSummary`

``` purescript
newtype ArrayJobStatusSummary
  = ArrayJobStatusSummary (Map String Int)
```

#### `ArrayProperties`

``` purescript
newtype ArrayProperties
  = ArrayProperties { "Size'" :: NullOrUndefined (Int) }
```

<p>An object representing an AWS Batch array job.</p>

#### `ArrayPropertiesDetail`

``` purescript
newtype ArrayPropertiesDetail
  = ArrayPropertiesDetail { "StatusSummary'" :: NullOrUndefined (ArrayJobStatusSummary), "Size'" :: NullOrUndefined (Int), "Index'" :: NullOrUndefined (Int) }
```

<p>An object representing the array properties of a job.</p>

#### `ArrayPropertiesSummary`

``` purescript
newtype ArrayPropertiesSummary
  = ArrayPropertiesSummary { "Size'" :: NullOrUndefined (Int), "Index'" :: NullOrUndefined (Int) }
```

<p>An object representing the array properties of a job.</p>

#### `AttemptContainerDetail`

``` purescript
newtype AttemptContainerDetail
  = AttemptContainerDetail { "ContainerInstanceArn'" :: NullOrUndefined (String), "TaskArn'" :: NullOrUndefined (String), "ExitCode'" :: NullOrUndefined (Int), "Reason'" :: NullOrUndefined (String), "LogStreamName'" :: NullOrUndefined (String) }
```

<p>An object representing the details of a container that is part of a job attempt.</p>

#### `AttemptDetail`

``` purescript
newtype AttemptDetail
  = AttemptDetail { "Container'" :: NullOrUndefined (AttemptContainerDetail), "StartedAt'" :: NullOrUndefined (Number), "StoppedAt'" :: NullOrUndefined (Number), "StatusReason'" :: NullOrUndefined (String) }
```

<p>An object representing a job attempt.</p>

#### `AttemptDetails`

``` purescript
newtype AttemptDetails
  = AttemptDetails (Array AttemptDetail)
```

#### `CEState`

``` purescript
newtype CEState
  = CEState String
```

#### `CEStatus`

``` purescript
newtype CEStatus
  = CEStatus String
```

#### `CEType`

``` purescript
newtype CEType
  = CEType String
```

#### `CRType`

``` purescript
newtype CRType
  = CRType String
```

#### `CancelJobRequest`

``` purescript
newtype CancelJobRequest
  = CancelJobRequest { "JobId'" :: String, "Reason'" :: String }
```

#### `CancelJobResponse`

``` purescript
newtype CancelJobResponse
  = CancelJobResponse {  }
```

#### `ClientException`

``` purescript
newtype ClientException
  = ClientException { "Message'" :: NullOrUndefined (String) }
```

<p>These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid. </p>

#### `ComputeEnvironmentDetail`

``` purescript
newtype ComputeEnvironmentDetail
  = ComputeEnvironmentDetail { "ComputeEnvironmentName'" :: String, "ComputeEnvironmentArn'" :: String, "EcsClusterArn'" :: String, "Type'" :: NullOrUndefined (CEType), "State'" :: NullOrUndefined (CEState), "Status'" :: NullOrUndefined (CEStatus), "StatusReason'" :: NullOrUndefined (String), "ComputeResources'" :: NullOrUndefined (ComputeResource), "ServiceRole'" :: NullOrUndefined (String) }
```

<p>An object representing an AWS Batch compute environment.</p>

#### `ComputeEnvironmentDetailList`

``` purescript
newtype ComputeEnvironmentDetailList
  = ComputeEnvironmentDetailList (Array ComputeEnvironmentDetail)
```

#### `ComputeEnvironmentOrder`

``` purescript
newtype ComputeEnvironmentOrder
  = ComputeEnvironmentOrder { "Order'" :: Int, "ComputeEnvironment'" :: String }
```

<p>The order in which compute environments are tried for job placement within a queue. Compute environments are tried in ascending order. For example, if two compute environments are associated with a job queue, the compute environment with a lower order integer value is tried for job placement first.</p>

#### `ComputeEnvironmentOrders`

``` purescript
newtype ComputeEnvironmentOrders
  = ComputeEnvironmentOrders (Array ComputeEnvironmentOrder)
```

#### `ComputeResource`

``` purescript
newtype ComputeResource
  = ComputeResource { "Type'" :: CRType, "MinvCpus'" :: Int, "MaxvCpus'" :: Int, "DesiredvCpus'" :: NullOrUndefined (Int), "InstanceTypes'" :: StringList, "ImageId'" :: NullOrUndefined (String), "Subnets'" :: StringList, "SecurityGroupIds'" :: StringList, "Ec2KeyPair'" :: NullOrUndefined (String), "InstanceRole'" :: String, "Tags'" :: NullOrUndefined (TagsMap), "BidPercentage'" :: NullOrUndefined (Int), "SpotIamFleetRole'" :: NullOrUndefined (String) }
```

<p>An object representing an AWS Batch compute resource.</p>

#### `ComputeResourceUpdate`

``` purescript
newtype ComputeResourceUpdate
  = ComputeResourceUpdate { "MinvCpus'" :: NullOrUndefined (Int), "MaxvCpus'" :: NullOrUndefined (Int), "DesiredvCpus'" :: NullOrUndefined (Int) }
```

<p>An object representing the attributes of a compute environment that can be updated.</p>

#### `ContainerDetail`

``` purescript
newtype ContainerDetail
  = ContainerDetail { "Image'" :: NullOrUndefined (String), "Vcpus'" :: NullOrUndefined (Int), "Memory'" :: NullOrUndefined (Int), "Command'" :: NullOrUndefined (StringList), "JobRoleArn'" :: NullOrUndefined (String), "Volumes'" :: NullOrUndefined (Volumes), "Environment'" :: NullOrUndefined (EnvironmentVariables), "MountPoints'" :: NullOrUndefined (MountPoints), "ReadonlyRootFilesystem'" :: NullOrUndefined (Boolean), "Ulimits'" :: NullOrUndefined (Ulimits), "Privileged'" :: NullOrUndefined (Boolean), "User'" :: NullOrUndefined (String), "ExitCode'" :: NullOrUndefined (Int), "Reason'" :: NullOrUndefined (String), "ContainerInstanceArn'" :: NullOrUndefined (String), "TaskArn'" :: NullOrUndefined (String), "LogStreamName'" :: NullOrUndefined (String) }
```

<p>An object representing the details of a container that is part of a job.</p>

#### `ContainerOverrides`

``` purescript
newtype ContainerOverrides
  = ContainerOverrides { "Vcpus'" :: NullOrUndefined (Int), "Memory'" :: NullOrUndefined (Int), "Command'" :: NullOrUndefined (StringList), "Environment'" :: NullOrUndefined (EnvironmentVariables) }
```

<p>The overrides that should be sent to a container.</p>

#### `ContainerProperties`

``` purescript
newtype ContainerProperties
  = ContainerProperties { "Image'" :: String, "Vcpus'" :: Int, "Memory'" :: Int, "Command'" :: NullOrUndefined (StringList), "JobRoleArn'" :: NullOrUndefined (String), "Volumes'" :: NullOrUndefined (Volumes), "Environment'" :: NullOrUndefined (EnvironmentVariables), "MountPoints'" :: NullOrUndefined (MountPoints), "ReadonlyRootFilesystem'" :: NullOrUndefined (Boolean), "Privileged'" :: NullOrUndefined (Boolean), "Ulimits'" :: NullOrUndefined (Ulimits), "User'" :: NullOrUndefined (String) }
```

<p>Container properties are used in job definitions to describe the container that is launched as part of a job.</p>

#### `ContainerSummary`

``` purescript
newtype ContainerSummary
  = ContainerSummary { "ExitCode'" :: NullOrUndefined (Int), "Reason'" :: NullOrUndefined (String) }
```

<p>An object representing summary details of a container within a job.</p>

#### `CreateComputeEnvironmentRequest`

``` purescript
newtype CreateComputeEnvironmentRequest
  = CreateComputeEnvironmentRequest { "ComputeEnvironmentName'" :: String, "Type'" :: CEType, "State'" :: NullOrUndefined (CEState), "ComputeResources'" :: NullOrUndefined (ComputeResource), "ServiceRole'" :: String }
```

#### `CreateComputeEnvironmentResponse`

``` purescript
newtype CreateComputeEnvironmentResponse
  = CreateComputeEnvironmentResponse { "ComputeEnvironmentName'" :: NullOrUndefined (String), "ComputeEnvironmentArn'" :: NullOrUndefined (String) }
```

#### `CreateJobQueueRequest`

``` purescript
newtype CreateJobQueueRequest
  = CreateJobQueueRequest { "JobQueueName'" :: String, "State'" :: NullOrUndefined (JQState), "Priority'" :: Int, "ComputeEnvironmentOrder'" :: ComputeEnvironmentOrders }
```

#### `CreateJobQueueResponse`

``` purescript
newtype CreateJobQueueResponse
  = CreateJobQueueResponse { "JobQueueName'" :: String, "JobQueueArn'" :: String }
```

#### `DeleteComputeEnvironmentRequest`

``` purescript
newtype DeleteComputeEnvironmentRequest
  = DeleteComputeEnvironmentRequest { "ComputeEnvironment'" :: String }
```

#### `DeleteComputeEnvironmentResponse`

``` purescript
newtype DeleteComputeEnvironmentResponse
  = DeleteComputeEnvironmentResponse {  }
```

#### `DeleteJobQueueRequest`

``` purescript
newtype DeleteJobQueueRequest
  = DeleteJobQueueRequest { "JobQueue'" :: String }
```

#### `DeleteJobQueueResponse`

``` purescript
newtype DeleteJobQueueResponse
  = DeleteJobQueueResponse {  }
```

#### `DeregisterJobDefinitionRequest`

``` purescript
newtype DeregisterJobDefinitionRequest
  = DeregisterJobDefinitionRequest { "JobDefinition'" :: String }
```

#### `DeregisterJobDefinitionResponse`

``` purescript
newtype DeregisterJobDefinitionResponse
  = DeregisterJobDefinitionResponse {  }
```

#### `DescribeComputeEnvironmentsRequest`

``` purescript
newtype DescribeComputeEnvironmentsRequest
  = DescribeComputeEnvironmentsRequest { "ComputeEnvironments'" :: NullOrUndefined (StringList), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (String) }
```

#### `DescribeComputeEnvironmentsResponse`

``` purescript
newtype DescribeComputeEnvironmentsResponse
  = DescribeComputeEnvironmentsResponse { "ComputeEnvironments'" :: NullOrUndefined (ComputeEnvironmentDetailList), "NextToken'" :: NullOrUndefined (String) }
```

#### `DescribeJobDefinitionsRequest`

``` purescript
newtype DescribeJobDefinitionsRequest
  = DescribeJobDefinitionsRequest { "JobDefinitions'" :: NullOrUndefined (StringList), "MaxResults'" :: NullOrUndefined (Int), "JobDefinitionName'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (String), "NextToken'" :: NullOrUndefined (String) }
```

#### `DescribeJobDefinitionsResponse`

``` purescript
newtype DescribeJobDefinitionsResponse
  = DescribeJobDefinitionsResponse { "JobDefinitions'" :: NullOrUndefined (JobDefinitionList), "NextToken'" :: NullOrUndefined (String) }
```

#### `DescribeJobQueuesRequest`

``` purescript
newtype DescribeJobQueuesRequest
  = DescribeJobQueuesRequest { "JobQueues'" :: NullOrUndefined (StringList), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (String) }
```

#### `DescribeJobQueuesResponse`

``` purescript
newtype DescribeJobQueuesResponse
  = DescribeJobQueuesResponse { "JobQueues'" :: NullOrUndefined (JobQueueDetailList), "NextToken'" :: NullOrUndefined (String) }
```

#### `DescribeJobsRequest`

``` purescript
newtype DescribeJobsRequest
  = DescribeJobsRequest { "Jobs'" :: StringList }
```

#### `DescribeJobsResponse`

``` purescript
newtype DescribeJobsResponse
  = DescribeJobsResponse { "Jobs'" :: NullOrUndefined (JobDetailList) }
```

#### `EnvironmentVariables`

``` purescript
newtype EnvironmentVariables
  = EnvironmentVariables (Array KeyValuePair)
```

#### `Host`

``` purescript
newtype Host
  = Host { "SourcePath'" :: NullOrUndefined (String) }
```

<p>The contents of the <code>host</code> parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.</p>

#### `JQState`

``` purescript
newtype JQState
  = JQState String
```

#### `JQStatus`

``` purescript
newtype JQStatus
  = JQStatus String
```

#### `JobDefinition`

``` purescript
newtype JobDefinition
  = JobDefinition { "JobDefinitionName'" :: String, "JobDefinitionArn'" :: String, "Revision'" :: Int, "Status'" :: NullOrUndefined (String), "Type'" :: String, "Parameters'" :: NullOrUndefined (ParametersMap), "RetryStrategy'" :: NullOrUndefined (RetryStrategy), "ContainerProperties'" :: NullOrUndefined (ContainerProperties) }
```

<p>An object representing an AWS Batch job definition.</p>

#### `JobDefinitionList`

``` purescript
newtype JobDefinitionList
  = JobDefinitionList (Array JobDefinition)
```

#### `JobDefinitionType`

``` purescript
newtype JobDefinitionType
  = JobDefinitionType String
```

#### `JobDependency`

``` purescript
newtype JobDependency
  = JobDependency { "JobId'" :: NullOrUndefined (String), "Type'" :: NullOrUndefined (ArrayJobDependency) }
```

<p>An object representing an AWS Batch job dependency.</p>

#### `JobDependencyList`

``` purescript
newtype JobDependencyList
  = JobDependencyList (Array JobDependency)
```

#### `JobDetail`

``` purescript
newtype JobDetail
  = JobDetail { "JobName'" :: String, "JobId'" :: String, "JobQueue'" :: String, "Status'" :: JobStatus, "Attempts'" :: NullOrUndefined (AttemptDetails), "StatusReason'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (Number), "RetryStrategy'" :: NullOrUndefined (RetryStrategy), "StartedAt'" :: Number, "StoppedAt'" :: NullOrUndefined (Number), "DependsOn'" :: NullOrUndefined (JobDependencyList), "JobDefinition'" :: String, "Parameters'" :: NullOrUndefined (ParametersMap), "Container'" :: NullOrUndefined (ContainerDetail), "ArrayProperties'" :: NullOrUndefined (ArrayPropertiesDetail) }
```

<p>An object representing an AWS Batch job.</p>

#### `JobDetailList`

``` purescript
newtype JobDetailList
  = JobDetailList (Array JobDetail)
```

#### `JobQueueDetail`

``` purescript
newtype JobQueueDetail
  = JobQueueDetail { "JobQueueName'" :: String, "JobQueueArn'" :: String, "State'" :: JQState, "Status'" :: NullOrUndefined (JQStatus), "StatusReason'" :: NullOrUndefined (String), "Priority'" :: Int, "ComputeEnvironmentOrder'" :: ComputeEnvironmentOrders }
```

<p>An object representing the details of an AWS Batch job queue.</p>

#### `JobQueueDetailList`

``` purescript
newtype JobQueueDetailList
  = JobQueueDetailList (Array JobQueueDetail)
```

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

#### `JobSummary`

``` purescript
newtype JobSummary
  = JobSummary { "JobId'" :: String, "JobName'" :: String, "CreatedAt'" :: NullOrUndefined (Number), "Status'" :: NullOrUndefined (JobStatus), "StatusReason'" :: NullOrUndefined (String), "StartedAt'" :: NullOrUndefined (Number), "StoppedAt'" :: NullOrUndefined (Number), "Container'" :: NullOrUndefined (ContainerSummary), "ArrayProperties'" :: NullOrUndefined (ArrayPropertiesSummary) }
```

<p>An object representing summary details of a job.</p>

#### `JobSummaryList`

``` purescript
newtype JobSummaryList
  = JobSummaryList (Array JobSummary)
```

#### `KeyValuePair`

``` purescript
newtype KeyValuePair
  = KeyValuePair { "Name'" :: NullOrUndefined (String), "Value'" :: NullOrUndefined (String) }
```

<p>A key-value pair object.</p>

#### `ListJobsRequest`

``` purescript
newtype ListJobsRequest
  = ListJobsRequest { "JobQueue'" :: NullOrUndefined (String), "ArrayJobId'" :: NullOrUndefined (String), "JobStatus'" :: NullOrUndefined (JobStatus), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListJobsResponse`

``` purescript
newtype ListJobsResponse
  = ListJobsResponse { "JobSummaryList'" :: JobSummaryList, "NextToken'" :: NullOrUndefined (String) }
```

#### `MountPoint`

``` purescript
newtype MountPoint
  = MountPoint { "ContainerPath'" :: NullOrUndefined (String), "ReadOnly'" :: NullOrUndefined (Boolean), "SourceVolume'" :: NullOrUndefined (String) }
```

<p>Details on a Docker volume mount point that is used in a job's container properties.</p>

#### `MountPoints`

``` purescript
newtype MountPoints
  = MountPoints (Array MountPoint)
```

#### `ParametersMap`

``` purescript
newtype ParametersMap
  = ParametersMap (Map String String)
```

#### `RegisterJobDefinitionRequest`

``` purescript
newtype RegisterJobDefinitionRequest
  = RegisterJobDefinitionRequest { "JobDefinitionName'" :: String, "Type'" :: JobDefinitionType, "Parameters'" :: NullOrUndefined (ParametersMap), "ContainerProperties'" :: NullOrUndefined (ContainerProperties), "RetryStrategy'" :: NullOrUndefined (RetryStrategy) }
```

#### `RegisterJobDefinitionResponse`

``` purescript
newtype RegisterJobDefinitionResponse
  = RegisterJobDefinitionResponse { "JobDefinitionName'" :: String, "JobDefinitionArn'" :: String, "Revision'" :: Int }
```

#### `RetryStrategy`

``` purescript
newtype RetryStrategy
  = RetryStrategy { "Attempts'" :: NullOrUndefined (Int) }
```

<p>The retry strategy associated with a job.</p>

#### `ServerException`

``` purescript
newtype ServerException
  = ServerException { "Message'" :: NullOrUndefined (String) }
```

<p>These errors are usually caused by a server issue.</p>

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

#### `SubmitJobRequest`

``` purescript
newtype SubmitJobRequest
  = SubmitJobRequest { "JobName'" :: String, "JobQueue'" :: String, "ArrayProperties'" :: NullOrUndefined (ArrayProperties), "DependsOn'" :: NullOrUndefined (JobDependencyList), "JobDefinition'" :: String, "Parameters'" :: NullOrUndefined (ParametersMap), "ContainerOverrides'" :: NullOrUndefined (ContainerOverrides), "RetryStrategy'" :: NullOrUndefined (RetryStrategy) }
```

#### `SubmitJobResponse`

``` purescript
newtype SubmitJobResponse
  = SubmitJobResponse { "JobName'" :: String, "JobId'" :: String }
```

#### `TagsMap`

``` purescript
newtype TagsMap
  = TagsMap (Map String String)
```

#### `TerminateJobRequest`

``` purescript
newtype TerminateJobRequest
  = TerminateJobRequest { "JobId'" :: String, "Reason'" :: String }
```

#### `TerminateJobResponse`

``` purescript
newtype TerminateJobResponse
  = TerminateJobResponse {  }
```

#### `Ulimit`

``` purescript
newtype Ulimit
  = Ulimit { "HardLimit'" :: Int, "Name'" :: String, "SoftLimit'" :: Int }
```

<p>The <code>ulimit</code> settings to pass to the container.</p>

#### `Ulimits`

``` purescript
newtype Ulimits
  = Ulimits (Array Ulimit)
```

#### `UpdateComputeEnvironmentRequest`

``` purescript
newtype UpdateComputeEnvironmentRequest
  = UpdateComputeEnvironmentRequest { "ComputeEnvironment'" :: String, "State'" :: NullOrUndefined (CEState), "ComputeResources'" :: NullOrUndefined (ComputeResourceUpdate), "ServiceRole'" :: NullOrUndefined (String) }
```

#### `UpdateComputeEnvironmentResponse`

``` purescript
newtype UpdateComputeEnvironmentResponse
  = UpdateComputeEnvironmentResponse { "ComputeEnvironmentName'" :: NullOrUndefined (String), "ComputeEnvironmentArn'" :: NullOrUndefined (String) }
```

#### `UpdateJobQueueRequest`

``` purescript
newtype UpdateJobQueueRequest
  = UpdateJobQueueRequest { "JobQueue'" :: String, "State'" :: NullOrUndefined (JQState), "Priority'" :: NullOrUndefined (Int), "ComputeEnvironmentOrder'" :: NullOrUndefined (ComputeEnvironmentOrders) }
```

#### `UpdateJobQueueResponse`

``` purescript
newtype UpdateJobQueueResponse
  = UpdateJobQueueResponse { "JobQueueName'" :: NullOrUndefined (String), "JobQueueArn'" :: NullOrUndefined (String) }
```

#### `Volume`

``` purescript
newtype Volume
  = Volume { "Host'" :: NullOrUndefined (Host), "Name'" :: NullOrUndefined (String) }
```

<p>A data volume used in a job's container properties.</p>

#### `Volumes`

``` purescript
newtype Volumes
  = Volumes (Array Volume)
```


