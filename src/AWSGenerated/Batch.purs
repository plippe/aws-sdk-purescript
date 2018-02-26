

-- | <p>AWS Batch enables you to run batch computing workloads on the AWS Cloud. Batch computing is a common way for developers, scientists, and engineers to access large amounts of compute resources, and AWS Batch removes the undifferentiated heavy lifting of configuring and managing the required infrastructure. AWS Batch will be familiar to users of traditional batch computing software. This service can efficiently provision resources in response to jobs submitted in order to eliminate capacity constraints, reduce compute costs, and deliver results quickly.</p> <p>As a fully managed service, AWS Batch enables developers, scientists, and engineers to run batch computing workloads of any scale. AWS Batch automatically provisions compute resources and optimizes the workload distribution based on the quantity and scale of the workloads. With AWS Batch, there is no need to install or manage batch computing software, which allows you to focus on analyzing results and solving problems. AWS Batch reduces operational complexities, saves time, and reduces costs, which makes it easy for developers, scientists, and engineers to run their batch jobs in the AWS Cloud.</p>
module AWS.Batch where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Batch" :: String


-- | <p>Cancels a job in an AWS Batch job queue. Jobs that are in the <code>SUBMITTED</code>, <code>PENDING</code>, or <code>RUNNABLE</code> state are cancelled. Jobs that have progressed to <code>STARTING</code> or <code>RUNNING</code> are not cancelled (but the API operation still succeeds, even if no job is cancelled); these jobs must be terminated with the <a>TerminateJob</a> operation.</p>
cancelJob :: forall eff. CancelJobRequest -> Aff (err :: AWS.RequestError | eff) CancelJobResponse
cancelJob = AWS.request serviceName "CancelJob" 


-- | <p>Creates an AWS Batch compute environment. You can create <code>MANAGED</code> or <code>UNMANAGED</code> compute environments.</p> <p>In a managed compute environment, AWS Batch manages the compute resources within the environment, based on the compute resources that you specify. Instances launched into a managed compute environment use a recent, approved version of the Amazon ECS-optimized AMI. You can choose to use Amazon EC2 On-Demand Instances in your managed compute environment, or you can use Amazon EC2 Spot Instances that only launch when the Spot bid price is below a specified percentage of the On-Demand price.</p> <p>In an unmanaged compute environment, you can manage your own compute resources. This provides more compute resource configuration options, such as using a custom AMI, but you must ensure that your AMI meets the Amazon ECS container instance AMI specification. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/container_instance_AMIs.html">Container Instance AMIs</a> in the <i>Amazon Elastic Container Service Developer Guide</i>. After you have created your unmanaged compute environment, you can use the <a>DescribeComputeEnvironments</a> operation to find the Amazon ECS cluster that is associated with it and then manually launch your container instances into that Amazon ECS cluster. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_container_instance.html">Launching an Amazon ECS Container Instance</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
createComputeEnvironment :: forall eff. CreateComputeEnvironmentRequest -> Aff (err :: AWS.RequestError | eff) CreateComputeEnvironmentResponse
createComputeEnvironment = AWS.request serviceName "CreateComputeEnvironment" 


-- | <p>Creates an AWS Batch job queue. When you create a job queue, you associate one or more compute environments to the queue and assign an order of preference for the compute environments.</p> <p>You also set a priority to the job queue that determines the order in which the AWS Batch scheduler places jobs onto its associated compute environments. For example, if a compute environment is associated with more than one job queue, the job queue with a higher priority is given preference for scheduling jobs to that compute environment.</p>
createJobQueue :: forall eff. CreateJobQueueRequest -> Aff (err :: AWS.RequestError | eff) CreateJobQueueResponse
createJobQueue = AWS.request serviceName "CreateJobQueue" 


-- | <p>Deletes an AWS Batch compute environment.</p> <p>Before you can delete a compute environment, you must set its state to <code>DISABLED</code> with the <a>UpdateComputeEnvironment</a> API operation and disassociate it from any job queues with the <a>UpdateJobQueue</a> API operation.</p>
deleteComputeEnvironment :: forall eff. DeleteComputeEnvironmentRequest -> Aff (err :: AWS.RequestError | eff) DeleteComputeEnvironmentResponse
deleteComputeEnvironment = AWS.request serviceName "DeleteComputeEnvironment" 


-- | <p>Deletes the specified job queue. You must first disable submissions for a queue with the <a>UpdateJobQueue</a> operation. All jobs in the queue are terminated when you delete a job queue.</p> <p>It is not necessary to disassociate compute environments from a queue before submitting a <code>DeleteJobQueue</code> request. </p>
deleteJobQueue :: forall eff. DeleteJobQueueRequest -> Aff (err :: AWS.RequestError | eff) DeleteJobQueueResponse
deleteJobQueue = AWS.request serviceName "DeleteJobQueue" 


-- | <p>Deregisters an AWS Batch job definition.</p>
deregisterJobDefinition :: forall eff. DeregisterJobDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeregisterJobDefinitionResponse
deregisterJobDefinition = AWS.request serviceName "DeregisterJobDefinition" 


-- | <p>Describes one or more of your compute environments.</p> <p>If you are using an unmanaged compute environment, you can use the <code>DescribeComputeEnvironment</code> operation to determine the <code>ecsClusterArn</code> that you should launch your Amazon ECS container instances into.</p>
describeComputeEnvironments :: forall eff. DescribeComputeEnvironmentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeComputeEnvironmentsResponse
describeComputeEnvironments = AWS.request serviceName "DescribeComputeEnvironments" 


-- | <p>Describes a list of job definitions. You can specify a <code>status</code> (such as <code>ACTIVE</code>) to only return job definitions that match that status.</p>
describeJobDefinitions :: forall eff. DescribeJobDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobDefinitionsResponse
describeJobDefinitions = AWS.request serviceName "DescribeJobDefinitions" 


-- | <p>Describes one or more of your job queues.</p>
describeJobQueues :: forall eff. DescribeJobQueuesRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobQueuesResponse
describeJobQueues = AWS.request serviceName "DescribeJobQueues" 


-- | <p>Describes a list of AWS Batch jobs.</p>
describeJobs :: forall eff. DescribeJobsRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobsResponse
describeJobs = AWS.request serviceName "DescribeJobs" 


-- | <p>Returns a list of task jobs for a specified job queue. You can filter the results by job status with the <code>jobStatus</code> parameter. If you do not specify a status, only <code>RUNNING</code> jobs are returned.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (err :: AWS.RequestError | eff) ListJobsResponse
listJobs = AWS.request serviceName "ListJobs" 


-- | <p>Registers an AWS Batch job definition. </p>
registerJobDefinition :: forall eff. RegisterJobDefinitionRequest -> Aff (err :: AWS.RequestError | eff) RegisterJobDefinitionResponse
registerJobDefinition = AWS.request serviceName "RegisterJobDefinition" 


-- | <p>Submits an AWS Batch job from a job definition. Parameters specified during <a>SubmitJob</a> override parameters defined in the job definition. </p>
submitJob :: forall eff. SubmitJobRequest -> Aff (err :: AWS.RequestError | eff) SubmitJobResponse
submitJob = AWS.request serviceName "SubmitJob" 


-- | <p>Terminates a job in a job queue. Jobs that are in the <code>STARTING</code> or <code>RUNNING</code> state are terminated, which causes them to transition to <code>FAILED</code>. Jobs that have not progressed to the <code>STARTING</code> state are cancelled.</p>
terminateJob :: forall eff. TerminateJobRequest -> Aff (err :: AWS.RequestError | eff) TerminateJobResponse
terminateJob = AWS.request serviceName "TerminateJob" 


-- | <p>Updates an AWS Batch compute environment.</p>
updateComputeEnvironment :: forall eff. UpdateComputeEnvironmentRequest -> Aff (err :: AWS.RequestError | eff) UpdateComputeEnvironmentResponse
updateComputeEnvironment = AWS.request serviceName "UpdateComputeEnvironment" 


-- | <p>Updates a job queue.</p>
updateJobQueue :: forall eff. UpdateJobQueueRequest -> Aff (err :: AWS.RequestError | eff) UpdateJobQueueResponse
updateJobQueue = AWS.request serviceName "UpdateJobQueue" 


newtype ArrayJobDependency = ArrayJobDependency String


newtype ArrayJobStatusSummary = ArrayJobStatusSummary (Map String Int)


-- | <p>An object representing an AWS Batch array job.</p>
newtype ArrayProperties = ArrayProperties 
  { "Size'" :: NullOrUndefined (Int)
  }


-- | <p>An object representing the array properties of a job.</p>
newtype ArrayPropertiesDetail = ArrayPropertiesDetail 
  { "StatusSummary'" :: NullOrUndefined (ArrayJobStatusSummary)
  , "Size'" :: NullOrUndefined (Int)
  , "Index'" :: NullOrUndefined (Int)
  }


-- | <p>An object representing the array properties of a job.</p>
newtype ArrayPropertiesSummary = ArrayPropertiesSummary 
  { "Size'" :: NullOrUndefined (Int)
  , "Index'" :: NullOrUndefined (Int)
  }


-- | <p>An object representing the details of a container that is part of a job attempt.</p>
newtype AttemptContainerDetail = AttemptContainerDetail 
  { "ContainerInstanceArn'" :: NullOrUndefined (String)
  , "TaskArn'" :: NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined (Int)
  , "Reason'" :: NullOrUndefined (String)
  , "LogStreamName'" :: NullOrUndefined (String)
  }


-- | <p>An object representing a job attempt.</p>
newtype AttemptDetail = AttemptDetail 
  { "Container'" :: NullOrUndefined (AttemptContainerDetail)
  , "StartedAt'" :: NullOrUndefined (Number)
  , "StoppedAt'" :: NullOrUndefined (Number)
  , "StatusReason'" :: NullOrUndefined (String)
  }


newtype AttemptDetails = AttemptDetails (Array AttemptDetail)


newtype CEState = CEState String


newtype CEStatus = CEStatus String


newtype CEType = CEType String


newtype CRType = CRType String


newtype CancelJobRequest = CancelJobRequest 
  { "JobId'" :: (String)
  , "Reason'" :: (String)
  }


newtype CancelJobResponse = CancelJobResponse 
  { 
  }


-- | <p>These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid. </p>
newtype ClientException = ClientException 
  { "Message'" :: NullOrUndefined (String)
  }


-- | <p>An object representing an AWS Batch compute environment.</p>
newtype ComputeEnvironmentDetail = ComputeEnvironmentDetail 
  { "ComputeEnvironmentName'" :: (String)
  , "ComputeEnvironmentArn'" :: (String)
  , "EcsClusterArn'" :: (String)
  , "Type'" :: NullOrUndefined (CEType)
  , "State'" :: NullOrUndefined (CEState)
  , "Status'" :: NullOrUndefined (CEStatus)
  , "StatusReason'" :: NullOrUndefined (String)
  , "ComputeResources'" :: NullOrUndefined (ComputeResource)
  , "ServiceRole'" :: NullOrUndefined (String)
  }


newtype ComputeEnvironmentDetailList = ComputeEnvironmentDetailList (Array ComputeEnvironmentDetail)


-- | <p>The order in which compute environments are tried for job placement within a queue. Compute environments are tried in ascending order. For example, if two compute environments are associated with a job queue, the compute environment with a lower order integer value is tried for job placement first.</p>
newtype ComputeEnvironmentOrder = ComputeEnvironmentOrder 
  { "Order'" :: (Int)
  , "ComputeEnvironment'" :: (String)
  }


newtype ComputeEnvironmentOrders = ComputeEnvironmentOrders (Array ComputeEnvironmentOrder)


-- | <p>An object representing an AWS Batch compute resource.</p>
newtype ComputeResource = ComputeResource 
  { "Type'" :: (CRType)
  , "MinvCpus'" :: (Int)
  , "MaxvCpus'" :: (Int)
  , "DesiredvCpus'" :: NullOrUndefined (Int)
  , "InstanceTypes'" :: (StringList)
  , "ImageId'" :: NullOrUndefined (String)
  , "Subnets'" :: (StringList)
  , "SecurityGroupIds'" :: (StringList)
  , "Ec2KeyPair'" :: NullOrUndefined (String)
  , "InstanceRole'" :: (String)
  , "Tags'" :: NullOrUndefined (TagsMap)
  , "BidPercentage'" :: NullOrUndefined (Int)
  , "SpotIamFleetRole'" :: NullOrUndefined (String)
  }


-- | <p>An object representing the attributes of a compute environment that can be updated.</p>
newtype ComputeResourceUpdate = ComputeResourceUpdate 
  { "MinvCpus'" :: NullOrUndefined (Int)
  , "MaxvCpus'" :: NullOrUndefined (Int)
  , "DesiredvCpus'" :: NullOrUndefined (Int)
  }


-- | <p>An object representing the details of a container that is part of a job.</p>
newtype ContainerDetail = ContainerDetail 
  { "Image'" :: NullOrUndefined (String)
  , "Vcpus'" :: NullOrUndefined (Int)
  , "Memory'" :: NullOrUndefined (Int)
  , "Command'" :: NullOrUndefined (StringList)
  , "JobRoleArn'" :: NullOrUndefined (String)
  , "Volumes'" :: NullOrUndefined (Volumes)
  , "Environment'" :: NullOrUndefined (EnvironmentVariables)
  , "MountPoints'" :: NullOrUndefined (MountPoints)
  , "ReadonlyRootFilesystem'" :: NullOrUndefined (Boolean)
  , "Ulimits'" :: NullOrUndefined (Ulimits)
  , "Privileged'" :: NullOrUndefined (Boolean)
  , "User'" :: NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined (Int)
  , "Reason'" :: NullOrUndefined (String)
  , "ContainerInstanceArn'" :: NullOrUndefined (String)
  , "TaskArn'" :: NullOrUndefined (String)
  , "LogStreamName'" :: NullOrUndefined (String)
  }


-- | <p>The overrides that should be sent to a container.</p>
newtype ContainerOverrides = ContainerOverrides 
  { "Vcpus'" :: NullOrUndefined (Int)
  , "Memory'" :: NullOrUndefined (Int)
  , "Command'" :: NullOrUndefined (StringList)
  , "Environment'" :: NullOrUndefined (EnvironmentVariables)
  }


-- | <p>Container properties are used in job definitions to describe the container that is launched as part of a job.</p>
newtype ContainerProperties = ContainerProperties 
  { "Image'" :: (String)
  , "Vcpus'" :: (Int)
  , "Memory'" :: (Int)
  , "Command'" :: NullOrUndefined (StringList)
  , "JobRoleArn'" :: NullOrUndefined (String)
  , "Volumes'" :: NullOrUndefined (Volumes)
  , "Environment'" :: NullOrUndefined (EnvironmentVariables)
  , "MountPoints'" :: NullOrUndefined (MountPoints)
  , "ReadonlyRootFilesystem'" :: NullOrUndefined (Boolean)
  , "Privileged'" :: NullOrUndefined (Boolean)
  , "Ulimits'" :: NullOrUndefined (Ulimits)
  , "User'" :: NullOrUndefined (String)
  }


-- | <p>An object representing summary details of a container within a job.</p>
newtype ContainerSummary = ContainerSummary 
  { "ExitCode'" :: NullOrUndefined (Int)
  , "Reason'" :: NullOrUndefined (String)
  }


newtype CreateComputeEnvironmentRequest = CreateComputeEnvironmentRequest 
  { "ComputeEnvironmentName'" :: (String)
  , "Type'" :: (CEType)
  , "State'" :: NullOrUndefined (CEState)
  , "ComputeResources'" :: NullOrUndefined (ComputeResource)
  , "ServiceRole'" :: (String)
  }


newtype CreateComputeEnvironmentResponse = CreateComputeEnvironmentResponse 
  { "ComputeEnvironmentName'" :: NullOrUndefined (String)
  , "ComputeEnvironmentArn'" :: NullOrUndefined (String)
  }


newtype CreateJobQueueRequest = CreateJobQueueRequest 
  { "JobQueueName'" :: (String)
  , "State'" :: NullOrUndefined (JQState)
  , "Priority'" :: (Int)
  , "ComputeEnvironmentOrder'" :: (ComputeEnvironmentOrders)
  }


newtype CreateJobQueueResponse = CreateJobQueueResponse 
  { "JobQueueName'" :: (String)
  , "JobQueueArn'" :: (String)
  }


newtype DeleteComputeEnvironmentRequest = DeleteComputeEnvironmentRequest 
  { "ComputeEnvironment'" :: (String)
  }


newtype DeleteComputeEnvironmentResponse = DeleteComputeEnvironmentResponse 
  { 
  }


newtype DeleteJobQueueRequest = DeleteJobQueueRequest 
  { "JobQueue'" :: (String)
  }


newtype DeleteJobQueueResponse = DeleteJobQueueResponse 
  { 
  }


newtype DeregisterJobDefinitionRequest = DeregisterJobDefinitionRequest 
  { "JobDefinition'" :: (String)
  }


newtype DeregisterJobDefinitionResponse = DeregisterJobDefinitionResponse 
  { 
  }


newtype DescribeComputeEnvironmentsRequest = DescribeComputeEnvironmentsRequest 
  { "ComputeEnvironments'" :: NullOrUndefined (StringList)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype DescribeComputeEnvironmentsResponse = DescribeComputeEnvironmentsResponse 
  { "ComputeEnvironments'" :: NullOrUndefined (ComputeEnvironmentDetailList)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype DescribeJobDefinitionsRequest = DescribeJobDefinitionsRequest 
  { "JobDefinitions'" :: NullOrUndefined (StringList)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "JobDefinitionName'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype DescribeJobDefinitionsResponse = DescribeJobDefinitionsResponse 
  { "JobDefinitions'" :: NullOrUndefined (JobDefinitionList)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype DescribeJobQueuesRequest = DescribeJobQueuesRequest 
  { "JobQueues'" :: NullOrUndefined (StringList)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype DescribeJobQueuesResponse = DescribeJobQueuesResponse 
  { "JobQueues'" :: NullOrUndefined (JobQueueDetailList)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype DescribeJobsRequest = DescribeJobsRequest 
  { "Jobs'" :: (StringList)
  }


newtype DescribeJobsResponse = DescribeJobsResponse 
  { "Jobs'" :: NullOrUndefined (JobDetailList)
  }


newtype EnvironmentVariables = EnvironmentVariables (Array KeyValuePair)


-- | <p>The contents of the <code>host</code> parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.</p>
newtype Host = Host 
  { "SourcePath'" :: NullOrUndefined (String)
  }


newtype JQState = JQState String


newtype JQStatus = JQStatus String


-- | <p>An object representing an AWS Batch job definition.</p>
newtype JobDefinition = JobDefinition 
  { "JobDefinitionName'" :: (String)
  , "JobDefinitionArn'" :: (String)
  , "Revision'" :: (Int)
  , "Status'" :: NullOrUndefined (String)
  , "Type'" :: (String)
  , "Parameters'" :: NullOrUndefined (ParametersMap)
  , "RetryStrategy'" :: NullOrUndefined (RetryStrategy)
  , "ContainerProperties'" :: NullOrUndefined (ContainerProperties)
  }


newtype JobDefinitionList = JobDefinitionList (Array JobDefinition)


newtype JobDefinitionType = JobDefinitionType String


-- | <p>An object representing an AWS Batch job dependency.</p>
newtype JobDependency = JobDependency 
  { "JobId'" :: NullOrUndefined (String)
  , "Type'" :: NullOrUndefined (ArrayJobDependency)
  }


newtype JobDependencyList = JobDependencyList (Array JobDependency)


-- | <p>An object representing an AWS Batch job.</p>
newtype JobDetail = JobDetail 
  { "JobName'" :: (String)
  , "JobId'" :: (String)
  , "JobQueue'" :: (String)
  , "Status'" :: (JobStatus)
  , "Attempts'" :: NullOrUndefined (AttemptDetails)
  , "StatusReason'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (Number)
  , "RetryStrategy'" :: NullOrUndefined (RetryStrategy)
  , "StartedAt'" :: (Number)
  , "StoppedAt'" :: NullOrUndefined (Number)
  , "DependsOn'" :: NullOrUndefined (JobDependencyList)
  , "JobDefinition'" :: (String)
  , "Parameters'" :: NullOrUndefined (ParametersMap)
  , "Container'" :: NullOrUndefined (ContainerDetail)
  , "ArrayProperties'" :: NullOrUndefined (ArrayPropertiesDetail)
  }


newtype JobDetailList = JobDetailList (Array JobDetail)


-- | <p>An object representing the details of an AWS Batch job queue.</p>
newtype JobQueueDetail = JobQueueDetail 
  { "JobQueueName'" :: (String)
  , "JobQueueArn'" :: (String)
  , "State'" :: (JQState)
  , "Status'" :: NullOrUndefined (JQStatus)
  , "StatusReason'" :: NullOrUndefined (String)
  , "Priority'" :: (Int)
  , "ComputeEnvironmentOrder'" :: (ComputeEnvironmentOrders)
  }


newtype JobQueueDetailList = JobQueueDetailList (Array JobQueueDetail)


newtype JobStatus = JobStatus String


-- | <p>An object representing summary details of a job.</p>
newtype JobSummary = JobSummary 
  { "JobId'" :: (String)
  , "JobName'" :: (String)
  , "CreatedAt'" :: NullOrUndefined (Number)
  , "Status'" :: NullOrUndefined (JobStatus)
  , "StatusReason'" :: NullOrUndefined (String)
  , "StartedAt'" :: NullOrUndefined (Number)
  , "StoppedAt'" :: NullOrUndefined (Number)
  , "Container'" :: NullOrUndefined (ContainerSummary)
  , "ArrayProperties'" :: NullOrUndefined (ArrayPropertiesSummary)
  }


newtype JobSummaryList = JobSummaryList (Array JobSummary)


-- | <p>A key-value pair object.</p>
newtype KeyValuePair = KeyValuePair 
  { "Name'" :: NullOrUndefined (String)
  , "Value'" :: NullOrUndefined (String)
  }


newtype ListJobsRequest = ListJobsRequest 
  { "JobQueue'" :: NullOrUndefined (String)
  , "ArrayJobId'" :: NullOrUndefined (String)
  , "JobStatus'" :: NullOrUndefined (JobStatus)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype ListJobsResponse = ListJobsResponse 
  { "JobSummaryList'" :: (JobSummaryList)
  , "NextToken'" :: NullOrUndefined (String)
  }


-- | <p>Details on a Docker volume mount point that is used in a job's container properties.</p>
newtype MountPoint = MountPoint 
  { "ContainerPath'" :: NullOrUndefined (String)
  , "ReadOnly'" :: NullOrUndefined (Boolean)
  , "SourceVolume'" :: NullOrUndefined (String)
  }


newtype MountPoints = MountPoints (Array MountPoint)


newtype ParametersMap = ParametersMap (Map String String)


newtype RegisterJobDefinitionRequest = RegisterJobDefinitionRequest 
  { "JobDefinitionName'" :: (String)
  , "Type'" :: (JobDefinitionType)
  , "Parameters'" :: NullOrUndefined (ParametersMap)
  , "ContainerProperties'" :: NullOrUndefined (ContainerProperties)
  , "RetryStrategy'" :: NullOrUndefined (RetryStrategy)
  }


newtype RegisterJobDefinitionResponse = RegisterJobDefinitionResponse 
  { "JobDefinitionName'" :: (String)
  , "JobDefinitionArn'" :: (String)
  , "Revision'" :: (Int)
  }


-- | <p>The retry strategy associated with a job.</p>
newtype RetryStrategy = RetryStrategy 
  { "Attempts'" :: NullOrUndefined (Int)
  }


-- | <p>These errors are usually caused by a server issue.</p>
newtype ServerException = ServerException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype StringList = StringList (Array String)


newtype SubmitJobRequest = SubmitJobRequest 
  { "JobName'" :: (String)
  , "JobQueue'" :: (String)
  , "ArrayProperties'" :: NullOrUndefined (ArrayProperties)
  , "DependsOn'" :: NullOrUndefined (JobDependencyList)
  , "JobDefinition'" :: (String)
  , "Parameters'" :: NullOrUndefined (ParametersMap)
  , "ContainerOverrides'" :: NullOrUndefined (ContainerOverrides)
  , "RetryStrategy'" :: NullOrUndefined (RetryStrategy)
  }


newtype SubmitJobResponse = SubmitJobResponse 
  { "JobName'" :: (String)
  , "JobId'" :: (String)
  }


newtype TagsMap = TagsMap (Map String String)


newtype TerminateJobRequest = TerminateJobRequest 
  { "JobId'" :: (String)
  , "Reason'" :: (String)
  }


newtype TerminateJobResponse = TerminateJobResponse 
  { 
  }


-- | <p>The <code>ulimit</code> settings to pass to the container.</p>
newtype Ulimit = Ulimit 
  { "HardLimit'" :: (Int)
  , "Name'" :: (String)
  , "SoftLimit'" :: (Int)
  }


newtype Ulimits = Ulimits (Array Ulimit)


newtype UpdateComputeEnvironmentRequest = UpdateComputeEnvironmentRequest 
  { "ComputeEnvironment'" :: (String)
  , "State'" :: NullOrUndefined (CEState)
  , "ComputeResources'" :: NullOrUndefined (ComputeResourceUpdate)
  , "ServiceRole'" :: NullOrUndefined (String)
  }


newtype UpdateComputeEnvironmentResponse = UpdateComputeEnvironmentResponse 
  { "ComputeEnvironmentName'" :: NullOrUndefined (String)
  , "ComputeEnvironmentArn'" :: NullOrUndefined (String)
  }


newtype UpdateJobQueueRequest = UpdateJobQueueRequest 
  { "JobQueue'" :: (String)
  , "State'" :: NullOrUndefined (JQState)
  , "Priority'" :: NullOrUndefined (Int)
  , "ComputeEnvironmentOrder'" :: NullOrUndefined (ComputeEnvironmentOrders)
  }


newtype UpdateJobQueueResponse = UpdateJobQueueResponse 
  { "JobQueueName'" :: NullOrUndefined (String)
  , "JobQueueArn'" :: NullOrUndefined (String)
  }


-- | <p>A data volume used in a job's container properties.</p>
newtype Volume = Volume 
  { "Host'" :: NullOrUndefined (Host)
  , "Name'" :: NullOrUndefined (String)
  }


newtype Volumes = Volumes (Array Volume)
