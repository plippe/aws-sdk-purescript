

-- | <p>AWS Batch enables you to run batch computing workloads on the AWS Cloud. Batch computing is a common way for developers, scientists, and engineers to access large amounts of compute resources, and AWS Batch removes the undifferentiated heavy lifting of configuring and managing the required infrastructure. AWS Batch will be familiar to users of traditional batch computing software. This service can efficiently provision resources in response to jobs submitted in order to eliminate capacity constraints, reduce compute costs, and deliver results quickly.</p> <p>As a fully managed service, AWS Batch enables developers, scientists, and engineers to run batch computing workloads of any scale. AWS Batch automatically provisions compute resources and optimizes the workload distribution based on the quantity and scale of the workloads. With AWS Batch, there is no need to install or manage batch computing software, which allows you to focus on analyzing results and solving problems. AWS Batch reduces operational complexities, saves time, and reduces costs, which makes it easy for developers, scientists, and engineers to run their batch jobs in the AWS Cloud.</p>
module AWS.Batch where

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

serviceName = "Batch" :: String


-- | <p>Cancels a job in an AWS Batch job queue. Jobs that are in the <code>SUBMITTED</code>, <code>PENDING</code>, or <code>RUNNABLE</code> state are cancelled. Jobs that have progressed to <code>STARTING</code> or <code>RUNNING</code> are not cancelled (but the API operation still succeeds, even if no job is cancelled); these jobs must be terminated with the <a>TerminateJob</a> operation.</p>
cancelJob :: forall eff. CancelJobRequest -> Aff (exception :: EXCEPTION | eff) CancelJobResponse
cancelJob = Request.request serviceName "cancelJob" 


-- | <p>Creates an AWS Batch compute environment. You can create <code>MANAGED</code> or <code>UNMANAGED</code> compute environments.</p> <p>In a managed compute environment, AWS Batch manages the compute resources within the environment, based on the compute resources that you specify. Instances launched into a managed compute environment use a recent, approved version of the Amazon ECS-optimized AMI. You can choose to use Amazon EC2 On-Demand Instances in your managed compute environment, or you can use Amazon EC2 Spot Instances that only launch when the Spot bid price is below a specified percentage of the On-Demand price.</p> <p>In an unmanaged compute environment, you can manage your own compute resources. This provides more compute resource configuration options, such as using a custom AMI, but you must ensure that your AMI meets the Amazon ECS container instance AMI specification. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/container_instance_AMIs.html">Container Instance AMIs</a> in the <i>Amazon Elastic Container Service Developer Guide</i>. After you have created your unmanaged compute environment, you can use the <a>DescribeComputeEnvironments</a> operation to find the Amazon ECS cluster that is associated with it and then manually launch your container instances into that Amazon ECS cluster. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_container_instance.html">Launching an Amazon ECS Container Instance</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
createComputeEnvironment :: forall eff. CreateComputeEnvironmentRequest -> Aff (exception :: EXCEPTION | eff) CreateComputeEnvironmentResponse
createComputeEnvironment = Request.request serviceName "createComputeEnvironment" 


-- | <p>Creates an AWS Batch job queue. When you create a job queue, you associate one or more compute environments to the queue and assign an order of preference for the compute environments.</p> <p>You also set a priority to the job queue that determines the order in which the AWS Batch scheduler places jobs onto its associated compute environments. For example, if a compute environment is associated with more than one job queue, the job queue with a higher priority is given preference for scheduling jobs to that compute environment.</p>
createJobQueue :: forall eff. CreateJobQueueRequest -> Aff (exception :: EXCEPTION | eff) CreateJobQueueResponse
createJobQueue = Request.request serviceName "createJobQueue" 


-- | <p>Deletes an AWS Batch compute environment.</p> <p>Before you can delete a compute environment, you must set its state to <code>DISABLED</code> with the <a>UpdateComputeEnvironment</a> API operation and disassociate it from any job queues with the <a>UpdateJobQueue</a> API operation.</p>
deleteComputeEnvironment :: forall eff. DeleteComputeEnvironmentRequest -> Aff (exception :: EXCEPTION | eff) DeleteComputeEnvironmentResponse
deleteComputeEnvironment = Request.request serviceName "deleteComputeEnvironment" 


-- | <p>Deletes the specified job queue. You must first disable submissions for a queue with the <a>UpdateJobQueue</a> operation. All jobs in the queue are terminated when you delete a job queue.</p> <p>It is not necessary to disassociate compute environments from a queue before submitting a <code>DeleteJobQueue</code> request. </p>
deleteJobQueue :: forall eff. DeleteJobQueueRequest -> Aff (exception :: EXCEPTION | eff) DeleteJobQueueResponse
deleteJobQueue = Request.request serviceName "deleteJobQueue" 


-- | <p>Deregisters an AWS Batch job definition.</p>
deregisterJobDefinition :: forall eff. DeregisterJobDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeregisterJobDefinitionResponse
deregisterJobDefinition = Request.request serviceName "deregisterJobDefinition" 


-- | <p>Describes one or more of your compute environments.</p> <p>If you are using an unmanaged compute environment, you can use the <code>DescribeComputeEnvironment</code> operation to determine the <code>ecsClusterArn</code> that you should launch your Amazon ECS container instances into.</p>
describeComputeEnvironments :: forall eff. DescribeComputeEnvironmentsRequest -> Aff (exception :: EXCEPTION | eff) DescribeComputeEnvironmentsResponse
describeComputeEnvironments = Request.request serviceName "describeComputeEnvironments" 


-- | <p>Describes a list of job definitions. You can specify a <code>status</code> (such as <code>ACTIVE</code>) to only return job definitions that match that status.</p>
describeJobDefinitions :: forall eff. DescribeJobDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeJobDefinitionsResponse
describeJobDefinitions = Request.request serviceName "describeJobDefinitions" 


-- | <p>Describes one or more of your job queues.</p>
describeJobQueues :: forall eff. DescribeJobQueuesRequest -> Aff (exception :: EXCEPTION | eff) DescribeJobQueuesResponse
describeJobQueues = Request.request serviceName "describeJobQueues" 


-- | <p>Describes a list of AWS Batch jobs.</p>
describeJobs :: forall eff. DescribeJobsRequest -> Aff (exception :: EXCEPTION | eff) DescribeJobsResponse
describeJobs = Request.request serviceName "describeJobs" 


-- | <p>Returns a list of task jobs for a specified job queue. You can filter the results by job status with the <code>jobStatus</code> parameter. If you do not specify a status, only <code>RUNNING</code> jobs are returned.</p>
listJobs :: forall eff. ListJobsRequest -> Aff (exception :: EXCEPTION | eff) ListJobsResponse
listJobs = Request.request serviceName "listJobs" 


-- | <p>Registers an AWS Batch job definition. </p>
registerJobDefinition :: forall eff. RegisterJobDefinitionRequest -> Aff (exception :: EXCEPTION | eff) RegisterJobDefinitionResponse
registerJobDefinition = Request.request serviceName "registerJobDefinition" 


-- | <p>Submits an AWS Batch job from a job definition. Parameters specified during <a>SubmitJob</a> override parameters defined in the job definition. </p>
submitJob :: forall eff. SubmitJobRequest -> Aff (exception :: EXCEPTION | eff) SubmitJobResponse
submitJob = Request.request serviceName "submitJob" 


-- | <p>Terminates a job in a job queue. Jobs that are in the <code>STARTING</code> or <code>RUNNING</code> state are terminated, which causes them to transition to <code>FAILED</code>. Jobs that have not progressed to the <code>STARTING</code> state are cancelled.</p>
terminateJob :: forall eff. TerminateJobRequest -> Aff (exception :: EXCEPTION | eff) TerminateJobResponse
terminateJob = Request.request serviceName "terminateJob" 


-- | <p>Updates an AWS Batch compute environment.</p>
updateComputeEnvironment :: forall eff. UpdateComputeEnvironmentRequest -> Aff (exception :: EXCEPTION | eff) UpdateComputeEnvironmentResponse
updateComputeEnvironment = Request.request serviceName "updateComputeEnvironment" 


-- | <p>Updates a job queue.</p>
updateJobQueue :: forall eff. UpdateJobQueueRequest -> Aff (exception :: EXCEPTION | eff) UpdateJobQueueResponse
updateJobQueue = Request.request serviceName "updateJobQueue" 


newtype ArrayJobDependency = ArrayJobDependency String
derive instance newtypeArrayJobDependency :: Newtype ArrayJobDependency _
derive instance repGenericArrayJobDependency :: Generic ArrayJobDependency _
instance showArrayJobDependency :: Show ArrayJobDependency where
  show = genericShow
instance decodeArrayJobDependency :: Decode ArrayJobDependency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArrayJobDependency :: Encode ArrayJobDependency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArrayJobStatusSummary = ArrayJobStatusSummary (StrMap.StrMap Int)
derive instance newtypeArrayJobStatusSummary :: Newtype ArrayJobStatusSummary _
derive instance repGenericArrayJobStatusSummary :: Generic ArrayJobStatusSummary _
instance showArrayJobStatusSummary :: Show ArrayJobStatusSummary where
  show = genericShow
instance decodeArrayJobStatusSummary :: Decode ArrayJobStatusSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArrayJobStatusSummary :: Encode ArrayJobStatusSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing an AWS Batch array job.</p>
newtype ArrayProperties = ArrayProperties 
  { "Size'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeArrayProperties :: Newtype ArrayProperties _
derive instance repGenericArrayProperties :: Generic ArrayProperties _
instance showArrayProperties :: Show ArrayProperties where
  show = genericShow
instance decodeArrayProperties :: Decode ArrayProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArrayProperties :: Encode ArrayProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the array properties of a job.</p>
newtype ArrayPropertiesDetail = ArrayPropertiesDetail 
  { "StatusSummary'" :: NullOrUndefined.NullOrUndefined (ArrayJobStatusSummary)
  , "Size'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Index'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeArrayPropertiesDetail :: Newtype ArrayPropertiesDetail _
derive instance repGenericArrayPropertiesDetail :: Generic ArrayPropertiesDetail _
instance showArrayPropertiesDetail :: Show ArrayPropertiesDetail where
  show = genericShow
instance decodeArrayPropertiesDetail :: Decode ArrayPropertiesDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArrayPropertiesDetail :: Encode ArrayPropertiesDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the array properties of a job.</p>
newtype ArrayPropertiesSummary = ArrayPropertiesSummary 
  { "Size'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Index'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeArrayPropertiesSummary :: Newtype ArrayPropertiesSummary _
derive instance repGenericArrayPropertiesSummary :: Generic ArrayPropertiesSummary _
instance showArrayPropertiesSummary :: Show ArrayPropertiesSummary where
  show = genericShow
instance decodeArrayPropertiesSummary :: Decode ArrayPropertiesSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArrayPropertiesSummary :: Encode ArrayPropertiesSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the details of a container that is part of a job attempt.</p>
newtype AttemptContainerDetail = AttemptContainerDetail 
  { "ContainerInstanceArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  , "LogStreamName'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAttemptContainerDetail :: Newtype AttemptContainerDetail _
derive instance repGenericAttemptContainerDetail :: Generic AttemptContainerDetail _
instance showAttemptContainerDetail :: Show AttemptContainerDetail where
  show = genericShow
instance decodeAttemptContainerDetail :: Decode AttemptContainerDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttemptContainerDetail :: Encode AttemptContainerDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing a job attempt.</p>
newtype AttemptDetail = AttemptDetail 
  { "Container'" :: NullOrUndefined.NullOrUndefined (AttemptContainerDetail)
  , "StartedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StoppedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StatusReason'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAttemptDetail :: Newtype AttemptDetail _
derive instance repGenericAttemptDetail :: Generic AttemptDetail _
instance showAttemptDetail :: Show AttemptDetail where
  show = genericShow
instance decodeAttemptDetail :: Decode AttemptDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttemptDetail :: Encode AttemptDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttemptDetails = AttemptDetails (Array AttemptDetail)
derive instance newtypeAttemptDetails :: Newtype AttemptDetails _
derive instance repGenericAttemptDetails :: Generic AttemptDetails _
instance showAttemptDetails :: Show AttemptDetails where
  show = genericShow
instance decodeAttemptDetails :: Decode AttemptDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttemptDetails :: Encode AttemptDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CEState = CEState String
derive instance newtypeCEState :: Newtype CEState _
derive instance repGenericCEState :: Generic CEState _
instance showCEState :: Show CEState where
  show = genericShow
instance decodeCEState :: Decode CEState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCEState :: Encode CEState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CEStatus = CEStatus String
derive instance newtypeCEStatus :: Newtype CEStatus _
derive instance repGenericCEStatus :: Generic CEStatus _
instance showCEStatus :: Show CEStatus where
  show = genericShow
instance decodeCEStatus :: Decode CEStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCEStatus :: Encode CEStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CEType = CEType String
derive instance newtypeCEType :: Newtype CEType _
derive instance repGenericCEType :: Generic CEType _
instance showCEType :: Show CEType where
  show = genericShow
instance decodeCEType :: Decode CEType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCEType :: Encode CEType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CRType = CRType String
derive instance newtypeCRType :: Newtype CRType _
derive instance repGenericCRType :: Generic CRType _
instance showCRType :: Show CRType where
  show = genericShow
instance decodeCRType :: Decode CRType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCRType :: Encode CRType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelJobRequest = CancelJobRequest 
  { "JobId'" :: (String)
  , "Reason'" :: (String)
  }
derive instance newtypeCancelJobRequest :: Newtype CancelJobRequest _
derive instance repGenericCancelJobRequest :: Generic CancelJobRequest _
instance showCancelJobRequest :: Show CancelJobRequest where
  show = genericShow
instance decodeCancelJobRequest :: Decode CancelJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobRequest :: Encode CancelJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelJobResponse = CancelJobResponse Types.NoArguments
derive instance newtypeCancelJobResponse :: Newtype CancelJobResponse _
derive instance repGenericCancelJobResponse :: Generic CancelJobResponse _
instance showCancelJobResponse :: Show CancelJobResponse where
  show = genericShow
instance decodeCancelJobResponse :: Decode CancelJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelJobResponse :: Encode CancelJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid. </p>
newtype ClientException = ClientException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeClientException :: Newtype ClientException _
derive instance repGenericClientException :: Generic ClientException _
instance showClientException :: Show ClientException where
  show = genericShow
instance decodeClientException :: Decode ClientException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientException :: Encode ClientException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing an AWS Batch compute environment.</p>
newtype ComputeEnvironmentDetail = ComputeEnvironmentDetail 
  { "ComputeEnvironmentName'" :: (String)
  , "ComputeEnvironmentArn'" :: (String)
  , "EcsClusterArn'" :: (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (CEType)
  , "State'" :: NullOrUndefined.NullOrUndefined (CEState)
  , "Status'" :: NullOrUndefined.NullOrUndefined (CEStatus)
  , "StatusReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "ComputeResources'" :: NullOrUndefined.NullOrUndefined (ComputeResource)
  , "ServiceRole'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeComputeEnvironmentDetail :: Newtype ComputeEnvironmentDetail _
derive instance repGenericComputeEnvironmentDetail :: Generic ComputeEnvironmentDetail _
instance showComputeEnvironmentDetail :: Show ComputeEnvironmentDetail where
  show = genericShow
instance decodeComputeEnvironmentDetail :: Decode ComputeEnvironmentDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeEnvironmentDetail :: Encode ComputeEnvironmentDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComputeEnvironmentDetailList = ComputeEnvironmentDetailList (Array ComputeEnvironmentDetail)
derive instance newtypeComputeEnvironmentDetailList :: Newtype ComputeEnvironmentDetailList _
derive instance repGenericComputeEnvironmentDetailList :: Generic ComputeEnvironmentDetailList _
instance showComputeEnvironmentDetailList :: Show ComputeEnvironmentDetailList where
  show = genericShow
instance decodeComputeEnvironmentDetailList :: Decode ComputeEnvironmentDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeEnvironmentDetailList :: Encode ComputeEnvironmentDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The order in which compute environments are tried for job placement within a queue. Compute environments are tried in ascending order. For example, if two compute environments are associated with a job queue, the compute environment with a lower order integer value is tried for job placement first.</p>
newtype ComputeEnvironmentOrder = ComputeEnvironmentOrder 
  { "Order'" :: (Int)
  , "ComputeEnvironment'" :: (String)
  }
derive instance newtypeComputeEnvironmentOrder :: Newtype ComputeEnvironmentOrder _
derive instance repGenericComputeEnvironmentOrder :: Generic ComputeEnvironmentOrder _
instance showComputeEnvironmentOrder :: Show ComputeEnvironmentOrder where
  show = genericShow
instance decodeComputeEnvironmentOrder :: Decode ComputeEnvironmentOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeEnvironmentOrder :: Encode ComputeEnvironmentOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComputeEnvironmentOrders = ComputeEnvironmentOrders (Array ComputeEnvironmentOrder)
derive instance newtypeComputeEnvironmentOrders :: Newtype ComputeEnvironmentOrders _
derive instance repGenericComputeEnvironmentOrders :: Generic ComputeEnvironmentOrders _
instance showComputeEnvironmentOrders :: Show ComputeEnvironmentOrders where
  show = genericShow
instance decodeComputeEnvironmentOrders :: Decode ComputeEnvironmentOrders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeEnvironmentOrders :: Encode ComputeEnvironmentOrders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing an AWS Batch compute resource.</p>
newtype ComputeResource = ComputeResource 
  { "Type'" :: (CRType)
  , "MinvCpus'" :: (Int)
  , "MaxvCpus'" :: (Int)
  , "DesiredvCpus'" :: NullOrUndefined.NullOrUndefined (Int)
  , "InstanceTypes'" :: (StringList)
  , "ImageId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Subnets'" :: (StringList)
  , "SecurityGroupIds'" :: (StringList)
  , "Ec2KeyPair'" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceRole'" :: (String)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagsMap)
  , "BidPercentage'" :: NullOrUndefined.NullOrUndefined (Int)
  , "SpotIamFleetRole'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeComputeResource :: Newtype ComputeResource _
derive instance repGenericComputeResource :: Generic ComputeResource _
instance showComputeResource :: Show ComputeResource where
  show = genericShow
instance decodeComputeResource :: Decode ComputeResource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeResource :: Encode ComputeResource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the attributes of a compute environment that can be updated.</p>
newtype ComputeResourceUpdate = ComputeResourceUpdate 
  { "MinvCpus'" :: NullOrUndefined.NullOrUndefined (Int)
  , "MaxvCpus'" :: NullOrUndefined.NullOrUndefined (Int)
  , "DesiredvCpus'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeComputeResourceUpdate :: Newtype ComputeResourceUpdate _
derive instance repGenericComputeResourceUpdate :: Generic ComputeResourceUpdate _
instance showComputeResourceUpdate :: Show ComputeResourceUpdate where
  show = genericShow
instance decodeComputeResourceUpdate :: Decode ComputeResourceUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeResourceUpdate :: Encode ComputeResourceUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the details of a container that is part of a job.</p>
newtype ContainerDetail = ContainerDetail 
  { "Image'" :: NullOrUndefined.NullOrUndefined (String)
  , "Vcpus'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Memory'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Command'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "JobRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Volumes'" :: NullOrUndefined.NullOrUndefined (Volumes)
  , "Environment'" :: NullOrUndefined.NullOrUndefined (EnvironmentVariables)
  , "MountPoints'" :: NullOrUndefined.NullOrUndefined (MountPoints)
  , "ReadonlyRootFilesystem'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Ulimits'" :: NullOrUndefined.NullOrUndefined (Ulimits)
  , "Privileged'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "User'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerInstanceArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "LogStreamName'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeContainerDetail :: Newtype ContainerDetail _
derive instance repGenericContainerDetail :: Generic ContainerDetail _
instance showContainerDetail :: Show ContainerDetail where
  show = genericShow
instance decodeContainerDetail :: Decode ContainerDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerDetail :: Encode ContainerDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The overrides that should be sent to a container.</p>
newtype ContainerOverrides = ContainerOverrides 
  { "Vcpus'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Memory'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Command'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "Environment'" :: NullOrUndefined.NullOrUndefined (EnvironmentVariables)
  }
derive instance newtypeContainerOverrides :: Newtype ContainerOverrides _
derive instance repGenericContainerOverrides :: Generic ContainerOverrides _
instance showContainerOverrides :: Show ContainerOverrides where
  show = genericShow
instance decodeContainerOverrides :: Decode ContainerOverrides where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerOverrides :: Encode ContainerOverrides where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container properties are used in job definitions to describe the container that is launched as part of a job.</p>
newtype ContainerProperties = ContainerProperties 
  { "Image'" :: (String)
  , "Vcpus'" :: (Int)
  , "Memory'" :: (Int)
  , "Command'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "JobRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Volumes'" :: NullOrUndefined.NullOrUndefined (Volumes)
  , "Environment'" :: NullOrUndefined.NullOrUndefined (EnvironmentVariables)
  , "MountPoints'" :: NullOrUndefined.NullOrUndefined (MountPoints)
  , "ReadonlyRootFilesystem'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Privileged'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Ulimits'" :: NullOrUndefined.NullOrUndefined (Ulimits)
  , "User'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeContainerProperties :: Newtype ContainerProperties _
derive instance repGenericContainerProperties :: Generic ContainerProperties _
instance showContainerProperties :: Show ContainerProperties where
  show = genericShow
instance decodeContainerProperties :: Decode ContainerProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerProperties :: Encode ContainerProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing summary details of a container within a job.</p>
newtype ContainerSummary = ContainerSummary 
  { "ExitCode'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeContainerSummary :: Newtype ContainerSummary _
derive instance repGenericContainerSummary :: Generic ContainerSummary _
instance showContainerSummary :: Show ContainerSummary where
  show = genericShow
instance decodeContainerSummary :: Decode ContainerSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerSummary :: Encode ContainerSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateComputeEnvironmentRequest = CreateComputeEnvironmentRequest 
  { "ComputeEnvironmentName'" :: (String)
  , "Type'" :: (CEType)
  , "State'" :: NullOrUndefined.NullOrUndefined (CEState)
  , "ComputeResources'" :: NullOrUndefined.NullOrUndefined (ComputeResource)
  , "ServiceRole'" :: (String)
  }
derive instance newtypeCreateComputeEnvironmentRequest :: Newtype CreateComputeEnvironmentRequest _
derive instance repGenericCreateComputeEnvironmentRequest :: Generic CreateComputeEnvironmentRequest _
instance showCreateComputeEnvironmentRequest :: Show CreateComputeEnvironmentRequest where
  show = genericShow
instance decodeCreateComputeEnvironmentRequest :: Decode CreateComputeEnvironmentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateComputeEnvironmentRequest :: Encode CreateComputeEnvironmentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateComputeEnvironmentResponse = CreateComputeEnvironmentResponse 
  { "ComputeEnvironmentName'" :: NullOrUndefined.NullOrUndefined (String)
  , "ComputeEnvironmentArn'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateComputeEnvironmentResponse :: Newtype CreateComputeEnvironmentResponse _
derive instance repGenericCreateComputeEnvironmentResponse :: Generic CreateComputeEnvironmentResponse _
instance showCreateComputeEnvironmentResponse :: Show CreateComputeEnvironmentResponse where
  show = genericShow
instance decodeCreateComputeEnvironmentResponse :: Decode CreateComputeEnvironmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateComputeEnvironmentResponse :: Encode CreateComputeEnvironmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobQueueRequest = CreateJobQueueRequest 
  { "JobQueueName'" :: (String)
  , "State'" :: NullOrUndefined.NullOrUndefined (JQState)
  , "Priority'" :: (Int)
  , "ComputeEnvironmentOrder'" :: (ComputeEnvironmentOrders)
  }
derive instance newtypeCreateJobQueueRequest :: Newtype CreateJobQueueRequest _
derive instance repGenericCreateJobQueueRequest :: Generic CreateJobQueueRequest _
instance showCreateJobQueueRequest :: Show CreateJobQueueRequest where
  show = genericShow
instance decodeCreateJobQueueRequest :: Decode CreateJobQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobQueueRequest :: Encode CreateJobQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobQueueResponse = CreateJobQueueResponse 
  { "JobQueueName'" :: (String)
  , "JobQueueArn'" :: (String)
  }
derive instance newtypeCreateJobQueueResponse :: Newtype CreateJobQueueResponse _
derive instance repGenericCreateJobQueueResponse :: Generic CreateJobQueueResponse _
instance showCreateJobQueueResponse :: Show CreateJobQueueResponse where
  show = genericShow
instance decodeCreateJobQueueResponse :: Decode CreateJobQueueResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobQueueResponse :: Encode CreateJobQueueResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteComputeEnvironmentRequest = DeleteComputeEnvironmentRequest 
  { "ComputeEnvironment'" :: (String)
  }
derive instance newtypeDeleteComputeEnvironmentRequest :: Newtype DeleteComputeEnvironmentRequest _
derive instance repGenericDeleteComputeEnvironmentRequest :: Generic DeleteComputeEnvironmentRequest _
instance showDeleteComputeEnvironmentRequest :: Show DeleteComputeEnvironmentRequest where
  show = genericShow
instance decodeDeleteComputeEnvironmentRequest :: Decode DeleteComputeEnvironmentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteComputeEnvironmentRequest :: Encode DeleteComputeEnvironmentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteComputeEnvironmentResponse = DeleteComputeEnvironmentResponse Types.NoArguments
derive instance newtypeDeleteComputeEnvironmentResponse :: Newtype DeleteComputeEnvironmentResponse _
derive instance repGenericDeleteComputeEnvironmentResponse :: Generic DeleteComputeEnvironmentResponse _
instance showDeleteComputeEnvironmentResponse :: Show DeleteComputeEnvironmentResponse where
  show = genericShow
instance decodeDeleteComputeEnvironmentResponse :: Decode DeleteComputeEnvironmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteComputeEnvironmentResponse :: Encode DeleteComputeEnvironmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteJobQueueRequest = DeleteJobQueueRequest 
  { "JobQueue'" :: (String)
  }
derive instance newtypeDeleteJobQueueRequest :: Newtype DeleteJobQueueRequest _
derive instance repGenericDeleteJobQueueRequest :: Generic DeleteJobQueueRequest _
instance showDeleteJobQueueRequest :: Show DeleteJobQueueRequest where
  show = genericShow
instance decodeDeleteJobQueueRequest :: Decode DeleteJobQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteJobQueueRequest :: Encode DeleteJobQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteJobQueueResponse = DeleteJobQueueResponse Types.NoArguments
derive instance newtypeDeleteJobQueueResponse :: Newtype DeleteJobQueueResponse _
derive instance repGenericDeleteJobQueueResponse :: Generic DeleteJobQueueResponse _
instance showDeleteJobQueueResponse :: Show DeleteJobQueueResponse where
  show = genericShow
instance decodeDeleteJobQueueResponse :: Decode DeleteJobQueueResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteJobQueueResponse :: Encode DeleteJobQueueResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterJobDefinitionRequest = DeregisterJobDefinitionRequest 
  { "JobDefinition'" :: (String)
  }
derive instance newtypeDeregisterJobDefinitionRequest :: Newtype DeregisterJobDefinitionRequest _
derive instance repGenericDeregisterJobDefinitionRequest :: Generic DeregisterJobDefinitionRequest _
instance showDeregisterJobDefinitionRequest :: Show DeregisterJobDefinitionRequest where
  show = genericShow
instance decodeDeregisterJobDefinitionRequest :: Decode DeregisterJobDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterJobDefinitionRequest :: Encode DeregisterJobDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterJobDefinitionResponse = DeregisterJobDefinitionResponse Types.NoArguments
derive instance newtypeDeregisterJobDefinitionResponse :: Newtype DeregisterJobDefinitionResponse _
derive instance repGenericDeregisterJobDefinitionResponse :: Generic DeregisterJobDefinitionResponse _
instance showDeregisterJobDefinitionResponse :: Show DeregisterJobDefinitionResponse where
  show = genericShow
instance decodeDeregisterJobDefinitionResponse :: Decode DeregisterJobDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterJobDefinitionResponse :: Encode DeregisterJobDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeComputeEnvironmentsRequest = DescribeComputeEnvironmentsRequest 
  { "ComputeEnvironments'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeComputeEnvironmentsRequest :: Newtype DescribeComputeEnvironmentsRequest _
derive instance repGenericDescribeComputeEnvironmentsRequest :: Generic DescribeComputeEnvironmentsRequest _
instance showDescribeComputeEnvironmentsRequest :: Show DescribeComputeEnvironmentsRequest where
  show = genericShow
instance decodeDescribeComputeEnvironmentsRequest :: Decode DescribeComputeEnvironmentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeComputeEnvironmentsRequest :: Encode DescribeComputeEnvironmentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeComputeEnvironmentsResponse = DescribeComputeEnvironmentsResponse 
  { "ComputeEnvironments'" :: NullOrUndefined.NullOrUndefined (ComputeEnvironmentDetailList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeComputeEnvironmentsResponse :: Newtype DescribeComputeEnvironmentsResponse _
derive instance repGenericDescribeComputeEnvironmentsResponse :: Generic DescribeComputeEnvironmentsResponse _
instance showDescribeComputeEnvironmentsResponse :: Show DescribeComputeEnvironmentsResponse where
  show = genericShow
instance decodeDescribeComputeEnvironmentsResponse :: Decode DescribeComputeEnvironmentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeComputeEnvironmentsResponse :: Encode DescribeComputeEnvironmentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobDefinitionsRequest = DescribeJobDefinitionsRequest 
  { "JobDefinitions'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "JobDefinitionName'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeJobDefinitionsRequest :: Newtype DescribeJobDefinitionsRequest _
derive instance repGenericDescribeJobDefinitionsRequest :: Generic DescribeJobDefinitionsRequest _
instance showDescribeJobDefinitionsRequest :: Show DescribeJobDefinitionsRequest where
  show = genericShow
instance decodeDescribeJobDefinitionsRequest :: Decode DescribeJobDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobDefinitionsRequest :: Encode DescribeJobDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobDefinitionsResponse = DescribeJobDefinitionsResponse 
  { "JobDefinitions'" :: NullOrUndefined.NullOrUndefined (JobDefinitionList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeJobDefinitionsResponse :: Newtype DescribeJobDefinitionsResponse _
derive instance repGenericDescribeJobDefinitionsResponse :: Generic DescribeJobDefinitionsResponse _
instance showDescribeJobDefinitionsResponse :: Show DescribeJobDefinitionsResponse where
  show = genericShow
instance decodeDescribeJobDefinitionsResponse :: Decode DescribeJobDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobDefinitionsResponse :: Encode DescribeJobDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobQueuesRequest = DescribeJobQueuesRequest 
  { "JobQueues'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeJobQueuesRequest :: Newtype DescribeJobQueuesRequest _
derive instance repGenericDescribeJobQueuesRequest :: Generic DescribeJobQueuesRequest _
instance showDescribeJobQueuesRequest :: Show DescribeJobQueuesRequest where
  show = genericShow
instance decodeDescribeJobQueuesRequest :: Decode DescribeJobQueuesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobQueuesRequest :: Encode DescribeJobQueuesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobQueuesResponse = DescribeJobQueuesResponse 
  { "JobQueues'" :: NullOrUndefined.NullOrUndefined (JobQueueDetailList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDescribeJobQueuesResponse :: Newtype DescribeJobQueuesResponse _
derive instance repGenericDescribeJobQueuesResponse :: Generic DescribeJobQueuesResponse _
instance showDescribeJobQueuesResponse :: Show DescribeJobQueuesResponse where
  show = genericShow
instance decodeDescribeJobQueuesResponse :: Decode DescribeJobQueuesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobQueuesResponse :: Encode DescribeJobQueuesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobsRequest = DescribeJobsRequest 
  { "Jobs'" :: (StringList)
  }
derive instance newtypeDescribeJobsRequest :: Newtype DescribeJobsRequest _
derive instance repGenericDescribeJobsRequest :: Generic DescribeJobsRequest _
instance showDescribeJobsRequest :: Show DescribeJobsRequest where
  show = genericShow
instance decodeDescribeJobsRequest :: Decode DescribeJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobsRequest :: Encode DescribeJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobsResponse = DescribeJobsResponse 
  { "Jobs'" :: NullOrUndefined.NullOrUndefined (JobDetailList)
  }
derive instance newtypeDescribeJobsResponse :: Newtype DescribeJobsResponse _
derive instance repGenericDescribeJobsResponse :: Generic DescribeJobsResponse _
instance showDescribeJobsResponse :: Show DescribeJobsResponse where
  show = genericShow
instance decodeDescribeJobsResponse :: Decode DescribeJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobsResponse :: Encode DescribeJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentVariables = EnvironmentVariables (Array KeyValuePair)
derive instance newtypeEnvironmentVariables :: Newtype EnvironmentVariables _
derive instance repGenericEnvironmentVariables :: Generic EnvironmentVariables _
instance showEnvironmentVariables :: Show EnvironmentVariables where
  show = genericShow
instance decodeEnvironmentVariables :: Decode EnvironmentVariables where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentVariables :: Encode EnvironmentVariables where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The contents of the <code>host</code> parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume, but the data is not guaranteed to persist after the containers associated with it stop running.</p>
newtype Host = Host 
  { "SourcePath'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHost :: Newtype Host _
derive instance repGenericHost :: Generic Host _
instance showHost :: Show Host where
  show = genericShow
instance decodeHost :: Decode Host where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHost :: Encode Host where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JQState = JQState String
derive instance newtypeJQState :: Newtype JQState _
derive instance repGenericJQState :: Generic JQState _
instance showJQState :: Show JQState where
  show = genericShow
instance decodeJQState :: Decode JQState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJQState :: Encode JQState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JQStatus = JQStatus String
derive instance newtypeJQStatus :: Newtype JQStatus _
derive instance repGenericJQStatus :: Generic JQStatus _
instance showJQStatus :: Show JQStatus where
  show = genericShow
instance decodeJQStatus :: Decode JQStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJQStatus :: Encode JQStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing an AWS Batch job definition.</p>
newtype JobDefinition = JobDefinition 
  { "JobDefinitionName'" :: (String)
  , "JobDefinitionArn'" :: (String)
  , "Revision'" :: (Int)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: (String)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "RetryStrategy'" :: NullOrUndefined.NullOrUndefined (RetryStrategy)
  , "ContainerProperties'" :: NullOrUndefined.NullOrUndefined (ContainerProperties)
  }
derive instance newtypeJobDefinition :: Newtype JobDefinition _
derive instance repGenericJobDefinition :: Generic JobDefinition _
instance showJobDefinition :: Show JobDefinition where
  show = genericShow
instance decodeJobDefinition :: Decode JobDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDefinition :: Encode JobDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDefinitionList = JobDefinitionList (Array JobDefinition)
derive instance newtypeJobDefinitionList :: Newtype JobDefinitionList _
derive instance repGenericJobDefinitionList :: Generic JobDefinitionList _
instance showJobDefinitionList :: Show JobDefinitionList where
  show = genericShow
instance decodeJobDefinitionList :: Decode JobDefinitionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDefinitionList :: Encode JobDefinitionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDefinitionType = JobDefinitionType String
derive instance newtypeJobDefinitionType :: Newtype JobDefinitionType _
derive instance repGenericJobDefinitionType :: Generic JobDefinitionType _
instance showJobDefinitionType :: Show JobDefinitionType where
  show = genericShow
instance decodeJobDefinitionType :: Decode JobDefinitionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDefinitionType :: Encode JobDefinitionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing an AWS Batch job dependency.</p>
newtype JobDependency = JobDependency 
  { "JobId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (ArrayJobDependency)
  }
derive instance newtypeJobDependency :: Newtype JobDependency _
derive instance repGenericJobDependency :: Generic JobDependency _
instance showJobDependency :: Show JobDependency where
  show = genericShow
instance decodeJobDependency :: Decode JobDependency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDependency :: Encode JobDependency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDependencyList = JobDependencyList (Array JobDependency)
derive instance newtypeJobDependencyList :: Newtype JobDependencyList _
derive instance repGenericJobDependencyList :: Generic JobDependencyList _
instance showJobDependencyList :: Show JobDependencyList where
  show = genericShow
instance decodeJobDependencyList :: Decode JobDependencyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDependencyList :: Encode JobDependencyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing an AWS Batch job.</p>
newtype JobDetail = JobDetail 
  { "JobName'" :: (String)
  , "JobId'" :: (String)
  , "JobQueue'" :: (String)
  , "Status'" :: (JobStatus)
  , "Attempts'" :: NullOrUndefined.NullOrUndefined (AttemptDetails)
  , "StatusReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "RetryStrategy'" :: NullOrUndefined.NullOrUndefined (RetryStrategy)
  , "StartedAt'" :: (Number)
  , "StoppedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DependsOn'" :: NullOrUndefined.NullOrUndefined (JobDependencyList)
  , "JobDefinition'" :: (String)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "Container'" :: NullOrUndefined.NullOrUndefined (ContainerDetail)
  , "ArrayProperties'" :: NullOrUndefined.NullOrUndefined (ArrayPropertiesDetail)
  }
derive instance newtypeJobDetail :: Newtype JobDetail _
derive instance repGenericJobDetail :: Generic JobDetail _
instance showJobDetail :: Show JobDetail where
  show = genericShow
instance decodeJobDetail :: Decode JobDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDetail :: Encode JobDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDetailList = JobDetailList (Array JobDetail)
derive instance newtypeJobDetailList :: Newtype JobDetailList _
derive instance repGenericJobDetailList :: Generic JobDetailList _
instance showJobDetailList :: Show JobDetailList where
  show = genericShow
instance decodeJobDetailList :: Decode JobDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDetailList :: Encode JobDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the details of an AWS Batch job queue.</p>
newtype JobQueueDetail = JobQueueDetail 
  { "JobQueueName'" :: (String)
  , "JobQueueArn'" :: (String)
  , "State'" :: (JQState)
  , "Status'" :: NullOrUndefined.NullOrUndefined (JQStatus)
  , "StatusReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "Priority'" :: (Int)
  , "ComputeEnvironmentOrder'" :: (ComputeEnvironmentOrders)
  }
derive instance newtypeJobQueueDetail :: Newtype JobQueueDetail _
derive instance repGenericJobQueueDetail :: Generic JobQueueDetail _
instance showJobQueueDetail :: Show JobQueueDetail where
  show = genericShow
instance decodeJobQueueDetail :: Decode JobQueueDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobQueueDetail :: Encode JobQueueDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobQueueDetailList = JobQueueDetailList (Array JobQueueDetail)
derive instance newtypeJobQueueDetailList :: Newtype JobQueueDetailList _
derive instance repGenericJobQueueDetailList :: Generic JobQueueDetailList _
instance showJobQueueDetailList :: Show JobQueueDetailList where
  show = genericShow
instance decodeJobQueueDetailList :: Decode JobQueueDetailList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobQueueDetailList :: Encode JobQueueDetailList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _
derive instance repGenericJobStatus :: Generic JobStatus _
instance showJobStatus :: Show JobStatus where
  show = genericShow
instance decodeJobStatus :: Decode JobStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobStatus :: Encode JobStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing summary details of a job.</p>
newtype JobSummary = JobSummary 
  { "JobId'" :: (String)
  , "JobName'" :: (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Status'" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "StatusReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "StartedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StoppedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Container'" :: NullOrUndefined.NullOrUndefined (ContainerSummary)
  , "ArrayProperties'" :: NullOrUndefined.NullOrUndefined (ArrayPropertiesSummary)
  }
derive instance newtypeJobSummary :: Newtype JobSummary _
derive instance repGenericJobSummary :: Generic JobSummary _
instance showJobSummary :: Show JobSummary where
  show = genericShow
instance decodeJobSummary :: Decode JobSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobSummary :: Encode JobSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobSummaryList = JobSummaryList (Array JobSummary)
derive instance newtypeJobSummaryList :: Newtype JobSummaryList _
derive instance repGenericJobSummaryList :: Generic JobSummaryList _
instance showJobSummaryList :: Show JobSummaryList where
  show = genericShow
instance decodeJobSummaryList :: Decode JobSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobSummaryList :: Encode JobSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A key-value pair object.</p>
newtype KeyValuePair = KeyValuePair 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Value'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeKeyValuePair :: Newtype KeyValuePair _
derive instance repGenericKeyValuePair :: Generic KeyValuePair _
instance showKeyValuePair :: Show KeyValuePair where
  show = genericShow
instance decodeKeyValuePair :: Decode KeyValuePair where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyValuePair :: Encode KeyValuePair where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobsRequest = ListJobsRequest 
  { "JobQueue'" :: NullOrUndefined.NullOrUndefined (String)
  , "ArrayJobId'" :: NullOrUndefined.NullOrUndefined (String)
  , "JobStatus'" :: NullOrUndefined.NullOrUndefined (JobStatus)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListJobsRequest :: Newtype ListJobsRequest _
derive instance repGenericListJobsRequest :: Generic ListJobsRequest _
instance showListJobsRequest :: Show ListJobsRequest where
  show = genericShow
instance decodeListJobsRequest :: Decode ListJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsRequest :: Encode ListJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListJobsResponse = ListJobsResponse 
  { "JobSummaryList'" :: (JobSummaryList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListJobsResponse :: Newtype ListJobsResponse _
derive instance repGenericListJobsResponse :: Generic ListJobsResponse _
instance showListJobsResponse :: Show ListJobsResponse where
  show = genericShow
instance decodeListJobsResponse :: Decode ListJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListJobsResponse :: Encode ListJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details on a Docker volume mount point that is used in a job's container properties.</p>
newtype MountPoint = MountPoint 
  { "ContainerPath'" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadOnly'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "SourceVolume'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeMountPoint :: Newtype MountPoint _
derive instance repGenericMountPoint :: Generic MountPoint _
instance showMountPoint :: Show MountPoint where
  show = genericShow
instance decodeMountPoint :: Decode MountPoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountPoint :: Encode MountPoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MountPoints = MountPoints (Array MountPoint)
derive instance newtypeMountPoints :: Newtype MountPoints _
derive instance repGenericMountPoints :: Generic MountPoints _
instance showMountPoints :: Show MountPoints where
  show = genericShow
instance decodeMountPoints :: Decode MountPoints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountPoints :: Encode MountPoints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersMap = ParametersMap (StrMap.StrMap String)
derive instance newtypeParametersMap :: Newtype ParametersMap _
derive instance repGenericParametersMap :: Generic ParametersMap _
instance showParametersMap :: Show ParametersMap where
  show = genericShow
instance decodeParametersMap :: Decode ParametersMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersMap :: Encode ParametersMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterJobDefinitionRequest = RegisterJobDefinitionRequest 
  { "JobDefinitionName'" :: (String)
  , "Type'" :: (JobDefinitionType)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "ContainerProperties'" :: NullOrUndefined.NullOrUndefined (ContainerProperties)
  , "RetryStrategy'" :: NullOrUndefined.NullOrUndefined (RetryStrategy)
  }
derive instance newtypeRegisterJobDefinitionRequest :: Newtype RegisterJobDefinitionRequest _
derive instance repGenericRegisterJobDefinitionRequest :: Generic RegisterJobDefinitionRequest _
instance showRegisterJobDefinitionRequest :: Show RegisterJobDefinitionRequest where
  show = genericShow
instance decodeRegisterJobDefinitionRequest :: Decode RegisterJobDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterJobDefinitionRequest :: Encode RegisterJobDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterJobDefinitionResponse = RegisterJobDefinitionResponse 
  { "JobDefinitionName'" :: (String)
  , "JobDefinitionArn'" :: (String)
  , "Revision'" :: (Int)
  }
derive instance newtypeRegisterJobDefinitionResponse :: Newtype RegisterJobDefinitionResponse _
derive instance repGenericRegisterJobDefinitionResponse :: Generic RegisterJobDefinitionResponse _
instance showRegisterJobDefinitionResponse :: Show RegisterJobDefinitionResponse where
  show = genericShow
instance decodeRegisterJobDefinitionResponse :: Decode RegisterJobDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterJobDefinitionResponse :: Encode RegisterJobDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The retry strategy associated with a job.</p>
newtype RetryStrategy = RetryStrategy 
  { "Attempts'" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeRetryStrategy :: Newtype RetryStrategy _
derive instance repGenericRetryStrategy :: Generic RetryStrategy _
instance showRetryStrategy :: Show RetryStrategy where
  show = genericShow
instance decodeRetryStrategy :: Decode RetryStrategy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetryStrategy :: Encode RetryStrategy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>These errors are usually caused by a server issue.</p>
newtype ServerException = ServerException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServerException :: Newtype ServerException _
derive instance repGenericServerException :: Generic ServerException _
instance showServerException :: Show ServerException where
  show = genericShow
instance decodeServerException :: Decode ServerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerException :: Encode ServerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _
derive instance repGenericStringList :: Generic StringList _
instance showStringList :: Show StringList where
  show = genericShow
instance decodeStringList :: Decode StringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringList :: Encode StringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubmitJobRequest = SubmitJobRequest 
  { "JobName'" :: (String)
  , "JobQueue'" :: (String)
  , "ArrayProperties'" :: NullOrUndefined.NullOrUndefined (ArrayProperties)
  , "DependsOn'" :: NullOrUndefined.NullOrUndefined (JobDependencyList)
  , "JobDefinition'" :: (String)
  , "Parameters'" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "ContainerOverrides'" :: NullOrUndefined.NullOrUndefined (ContainerOverrides)
  , "RetryStrategy'" :: NullOrUndefined.NullOrUndefined (RetryStrategy)
  }
derive instance newtypeSubmitJobRequest :: Newtype SubmitJobRequest _
derive instance repGenericSubmitJobRequest :: Generic SubmitJobRequest _
instance showSubmitJobRequest :: Show SubmitJobRequest where
  show = genericShow
instance decodeSubmitJobRequest :: Decode SubmitJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubmitJobRequest :: Encode SubmitJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubmitJobResponse = SubmitJobResponse 
  { "JobName'" :: (String)
  , "JobId'" :: (String)
  }
derive instance newtypeSubmitJobResponse :: Newtype SubmitJobResponse _
derive instance repGenericSubmitJobResponse :: Generic SubmitJobResponse _
instance showSubmitJobResponse :: Show SubmitJobResponse where
  show = genericShow
instance decodeSubmitJobResponse :: Decode SubmitJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubmitJobResponse :: Encode SubmitJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagsMap = TagsMap (StrMap.StrMap String)
derive instance newtypeTagsMap :: Newtype TagsMap _
derive instance repGenericTagsMap :: Generic TagsMap _
instance showTagsMap :: Show TagsMap where
  show = genericShow
instance decodeTagsMap :: Decode TagsMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagsMap :: Encode TagsMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateJobRequest = TerminateJobRequest 
  { "JobId'" :: (String)
  , "Reason'" :: (String)
  }
derive instance newtypeTerminateJobRequest :: Newtype TerminateJobRequest _
derive instance repGenericTerminateJobRequest :: Generic TerminateJobRequest _
instance showTerminateJobRequest :: Show TerminateJobRequest where
  show = genericShow
instance decodeTerminateJobRequest :: Decode TerminateJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateJobRequest :: Encode TerminateJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateJobResponse = TerminateJobResponse Types.NoArguments
derive instance newtypeTerminateJobResponse :: Newtype TerminateJobResponse _
derive instance repGenericTerminateJobResponse :: Generic TerminateJobResponse _
instance showTerminateJobResponse :: Show TerminateJobResponse where
  show = genericShow
instance decodeTerminateJobResponse :: Decode TerminateJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateJobResponse :: Encode TerminateJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ulimit</code> settings to pass to the container.</p>
newtype Ulimit = Ulimit 
  { "HardLimit'" :: (Int)
  , "Name'" :: (String)
  , "SoftLimit'" :: (Int)
  }
derive instance newtypeUlimit :: Newtype Ulimit _
derive instance repGenericUlimit :: Generic Ulimit _
instance showUlimit :: Show Ulimit where
  show = genericShow
instance decodeUlimit :: Decode Ulimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUlimit :: Encode Ulimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Ulimits = Ulimits (Array Ulimit)
derive instance newtypeUlimits :: Newtype Ulimits _
derive instance repGenericUlimits :: Generic Ulimits _
instance showUlimits :: Show Ulimits where
  show = genericShow
instance decodeUlimits :: Decode Ulimits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUlimits :: Encode Ulimits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateComputeEnvironmentRequest = UpdateComputeEnvironmentRequest 
  { "ComputeEnvironment'" :: (String)
  , "State'" :: NullOrUndefined.NullOrUndefined (CEState)
  , "ComputeResources'" :: NullOrUndefined.NullOrUndefined (ComputeResourceUpdate)
  , "ServiceRole'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateComputeEnvironmentRequest :: Newtype UpdateComputeEnvironmentRequest _
derive instance repGenericUpdateComputeEnvironmentRequest :: Generic UpdateComputeEnvironmentRequest _
instance showUpdateComputeEnvironmentRequest :: Show UpdateComputeEnvironmentRequest where
  show = genericShow
instance decodeUpdateComputeEnvironmentRequest :: Decode UpdateComputeEnvironmentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateComputeEnvironmentRequest :: Encode UpdateComputeEnvironmentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateComputeEnvironmentResponse = UpdateComputeEnvironmentResponse 
  { "ComputeEnvironmentName'" :: NullOrUndefined.NullOrUndefined (String)
  , "ComputeEnvironmentArn'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateComputeEnvironmentResponse :: Newtype UpdateComputeEnvironmentResponse _
derive instance repGenericUpdateComputeEnvironmentResponse :: Generic UpdateComputeEnvironmentResponse _
instance showUpdateComputeEnvironmentResponse :: Show UpdateComputeEnvironmentResponse where
  show = genericShow
instance decodeUpdateComputeEnvironmentResponse :: Decode UpdateComputeEnvironmentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateComputeEnvironmentResponse :: Encode UpdateComputeEnvironmentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateJobQueueRequest = UpdateJobQueueRequest 
  { "JobQueue'" :: (String)
  , "State'" :: NullOrUndefined.NullOrUndefined (JQState)
  , "Priority'" :: NullOrUndefined.NullOrUndefined (Int)
  , "ComputeEnvironmentOrder'" :: NullOrUndefined.NullOrUndefined (ComputeEnvironmentOrders)
  }
derive instance newtypeUpdateJobQueueRequest :: Newtype UpdateJobQueueRequest _
derive instance repGenericUpdateJobQueueRequest :: Generic UpdateJobQueueRequest _
instance showUpdateJobQueueRequest :: Show UpdateJobQueueRequest where
  show = genericShow
instance decodeUpdateJobQueueRequest :: Decode UpdateJobQueueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobQueueRequest :: Encode UpdateJobQueueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateJobQueueResponse = UpdateJobQueueResponse 
  { "JobQueueName'" :: NullOrUndefined.NullOrUndefined (String)
  , "JobQueueArn'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateJobQueueResponse :: Newtype UpdateJobQueueResponse _
derive instance repGenericUpdateJobQueueResponse :: Generic UpdateJobQueueResponse _
instance showUpdateJobQueueResponse :: Show UpdateJobQueueResponse where
  show = genericShow
instance decodeUpdateJobQueueResponse :: Decode UpdateJobQueueResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobQueueResponse :: Encode UpdateJobQueueResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A data volume used in a job's container properties.</p>
newtype Volume = Volume 
  { "Host'" :: NullOrUndefined.NullOrUndefined (Host)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVolume :: Newtype Volume _
derive instance repGenericVolume :: Generic Volume _
instance showVolume :: Show Volume where
  show = genericShow
instance decodeVolume :: Decode Volume where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolume :: Encode Volume where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Volumes = Volumes (Array Volume)
derive instance newtypeVolumes :: Newtype Volumes _
derive instance repGenericVolumes :: Generic Volumes _
instance showVolumes :: Show Volumes where
  show = genericShow
instance decodeVolumes :: Decode Volumes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumes :: Encode Volumes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
