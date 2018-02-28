

-- | <p>Amazon Elastic Container Service (Amazon ECS) is a highly scalable, fast, container management service that makes it easy to run, stop, and manage Docker containers on a cluster. You can host your cluster on a serverless infrastructure that is managed by Amazon ECS by launching your services or tasks using the Fargate launch type. For more control, you can host your tasks on a cluster of Amazon Elastic Compute Cloud (Amazon EC2) instances that you manage by using the EC2 launch type. For more information about launch types, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html">Amazon ECS Launch Types</a>.</p> <p>Amazon ECS lets you launch and stop container-based applications with simple API calls, allows you to get the state of your cluster from a centralized service, and gives you access to many familiar Amazon EC2 features.</p> <p>You can use Amazon ECS to schedule the placement of containers across your cluster based on your resource needs, isolation policies, and availability requirements. Amazon ECS eliminates the need for you to operate your own cluster management and configuration management systems or worry about scaling your management infrastructure.</p>
module AWS.ECS where

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

serviceName = "ECS" :: String


-- | <p>Creates a new Amazon ECS cluster. By default, your account receives a <code>default</code> cluster when you launch your first container instance. However, you can create your own cluster with a unique name with the <code>CreateCluster</code> action.</p> <note> <p>When you call the <a>CreateCluster</a> API operation, Amazon ECS attempts to create the service-linked role for your account so that required resources in other AWS services can be managed on your behalf. However, if the IAM user that makes the call does not have permissions to create the service-linked role, it is not created. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html">Using Service-Linked Roles for Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </note>
createCluster :: forall eff. CreateClusterRequest -> Aff (exception :: EXCEPTION | eff) CreateClusterResponse
createCluster = Request.request serviceName "createCluster" 


-- | <p>Runs and maintains a desired number of tasks from a specified task definition. If the number of tasks running in a service drops below <code>desiredCount</code>, Amazon ECS spawns another copy of the task in the specified cluster. To update an existing service, see <a>UpdateService</a>.</p> <p>In addition to maintaining the desired count of tasks in your service, you can optionally run your service behind a load balancer. The load balancer distributes traffic across the tasks that are associated with the service. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html">Service Load Balancing</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can optionally specify a deployment configuration for your service. During a deployment, the service scheduler uses the <code>minimumHealthyPercent</code> and <code>maximumPercent</code> parameters to determine the deployment strategy. The deployment is triggered by changing the task definition or the desired count of a service with an <a>UpdateService</a> operation.</p> <p>The <code>minimumHealthyPercent</code> represents a lower limit on the number of your service's tasks that must remain in the <code>RUNNING</code> state during a deployment, as a percentage of the <code>desiredCount</code> (rounded up to the nearest integer). This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a <code>desiredCount</code> of four tasks and a <code>minimumHealthyPercent</code> of 50%, the scheduler can stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that <i>do not</i> use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that <i>do</i> use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer. The default value for <code>minimumHealthyPercent</code> is 50% in the console and 100% for the AWS CLI, the AWS SDKs, and the APIs.</p> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of your service's tasks that are allowed in the <code>RUNNING</code> or <code>PENDING</code> state during a deployment, as a percentage of the <code>desiredCount</code> (rounded down to the nearest integer). This parameter enables you to define the deployment batch size. For example, if your service has a <code>desiredCount</code> of four tasks and a <code>maximumPercent</code> value of 200%, the scheduler can start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for <code>maximumPercent</code> is 200%.</p> <p>When the service scheduler launches new tasks, it determines task placement in your cluster using the following logic:</p> <ul> <li> <p>Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).</p> </li> <li> <p>By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy) with the <code>placementStrategy</code> parameter):</p> <ul> <li> <p>Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.</p> </li> <li> <p>Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.</p> </li> </ul> </li> </ul>
createService :: forall eff. CreateServiceRequest -> Aff (exception :: EXCEPTION | eff) CreateServiceResponse
createService = Request.request serviceName "createService" 


-- | <p>Deletes one or more custom attributes from an Amazon ECS resource.</p>
deleteAttributes :: forall eff. DeleteAttributesRequest -> Aff (exception :: EXCEPTION | eff) DeleteAttributesResponse
deleteAttributes = Request.request serviceName "deleteAttributes" 


-- | <p>Deletes the specified cluster. You must deregister all container instances from this cluster before you may delete it. You can list the container instances in a cluster with <a>ListContainerInstances</a> and deregister them with <a>DeregisterContainerInstance</a>.</p>
deleteCluster :: forall eff. DeleteClusterRequest -> Aff (exception :: EXCEPTION | eff) DeleteClusterResponse
deleteCluster = Request.request serviceName "deleteCluster" 


-- | <p>Deletes a specified service within a cluster. You can delete a service if you have no running tasks in it and the desired task count is zero. If the service is actively maintaining tasks, you cannot delete it, and you must update the service to a desired task count of zero. For more information, see <a>UpdateService</a>.</p> <note> <p>When you delete a service, if there are still running tasks that require cleanup, the service status moves from <code>ACTIVE</code> to <code>DRAINING</code>, and the service is no longer visible in the console or in <a>ListServices</a> API operations. After the tasks have stopped, then the service status moves from <code>DRAINING</code> to <code>INACTIVE</code>. Services in the <code>DRAINING</code> or <code>INACTIVE</code> status can still be viewed with <a>DescribeServices</a> API operations. However, in the future, <code>INACTIVE</code> services may be cleaned up and purged from Amazon ECS record keeping, and <a>DescribeServices</a> API operations on those services return a <code>ServiceNotFoundException</code> error.</p> </note>
deleteService :: forall eff. DeleteServiceRequest -> Aff (exception :: EXCEPTION | eff) DeleteServiceResponse
deleteService = Request.request serviceName "deleteService" 


-- | <p>Deregisters an Amazon ECS container instance from the specified cluster. This instance is no longer available to run tasks.</p> <p>If you intend to use the container instance for some other purpose after deregistration, you should stop all of the tasks running on the container instance before deregistration. That prevents any orphaned tasks from consuming resources.</p> <p>Deregistering a container instance removes the instance from a cluster, but it does not terminate the EC2 instance; if you are finished using the instance, be sure to terminate it in the Amazon EC2 console to stop billing.</p> <note> <p>If you terminate a running container instance, Amazon ECS automatically deregisters the instance from your cluster (stopped container instances or instances with disconnected agents are not automatically deregistered when terminated).</p> </note>
deregisterContainerInstance :: forall eff. DeregisterContainerInstanceRequest -> Aff (exception :: EXCEPTION | eff) DeregisterContainerInstanceResponse
deregisterContainerInstance = Request.request serviceName "deregisterContainerInstance" 


-- | <p>Deregisters the specified task definition by family and revision. Upon deregistration, the task definition is marked as <code>INACTIVE</code>. Existing tasks and services that reference an <code>INACTIVE</code> task definition continue to run without disruption. Existing services that reference an <code>INACTIVE</code> task definition can still scale up or down by modifying the service's desired count.</p> <p>You cannot use an <code>INACTIVE</code> task definition to run new tasks or create new services, and you cannot update an existing service to reference an <code>INACTIVE</code> task definition (although there may be up to a 10-minute window following deregistration where these restrictions have not yet taken effect).</p> <note> <p>At this time, <code>INACTIVE</code> task definitions remain discoverable in your account indefinitely; however, this behavior is subject to change in the future, so you should not rely on <code>INACTIVE</code> task definitions persisting beyond the lifecycle of any associated tasks and services.</p> </note>
deregisterTaskDefinition :: forall eff. DeregisterTaskDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DeregisterTaskDefinitionResponse
deregisterTaskDefinition = Request.request serviceName "deregisterTaskDefinition" 


-- | <p>Describes one or more of your clusters.</p>
describeClusters :: forall eff. DescribeClustersRequest -> Aff (exception :: EXCEPTION | eff) DescribeClustersResponse
describeClusters = Request.request serviceName "describeClusters" 


-- | <p>Describes Amazon Elastic Container Service container instances. Returns metadata about registered and remaining resources on each container instance requested.</p>
describeContainerInstances :: forall eff. DescribeContainerInstancesRequest -> Aff (exception :: EXCEPTION | eff) DescribeContainerInstancesResponse
describeContainerInstances = Request.request serviceName "describeContainerInstances" 


-- | <p>Describes the specified services running in your cluster.</p>
describeServices :: forall eff. DescribeServicesRequest -> Aff (exception :: EXCEPTION | eff) DescribeServicesResponse
describeServices = Request.request serviceName "describeServices" 


-- | <p>Describes a task definition. You can specify a <code>family</code> and <code>revision</code> to find information about a specific task definition, or you can simply specify the family to find the latest <code>ACTIVE</code> revision in that family.</p> <note> <p>You can only describe <code>INACTIVE</code> task definitions while an active task or service references them.</p> </note>
describeTaskDefinition :: forall eff. DescribeTaskDefinitionRequest -> Aff (exception :: EXCEPTION | eff) DescribeTaskDefinitionResponse
describeTaskDefinition = Request.request serviceName "describeTaskDefinition" 


-- | <p>Describes a specified task or tasks.</p>
describeTasks :: forall eff. DescribeTasksRequest -> Aff (exception :: EXCEPTION | eff) DescribeTasksResponse
describeTasks = Request.request serviceName "describeTasks" 


-- | <note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Returns an endpoint for the Amazon ECS agent to poll for updates.</p>
discoverPollEndpoint :: forall eff. DiscoverPollEndpointRequest -> Aff (exception :: EXCEPTION | eff) DiscoverPollEndpointResponse
discoverPollEndpoint = Request.request serviceName "discoverPollEndpoint" 


-- | <p>Lists the attributes for Amazon ECS resources within a specified target type and cluster. When you specify a target type and cluster, <code>ListAttributes</code> returns a list of attribute objects, one for each attribute on each resource. You can filter the list of results to a single attribute name to only return results that have that name. You can also filter the results by attribute name and value, for example, to see which container instances in a cluster are running a Linux AMI (<code>ecs.os-type=linux</code>). </p>
listAttributes :: forall eff. ListAttributesRequest -> Aff (exception :: EXCEPTION | eff) ListAttributesResponse
listAttributes = Request.request serviceName "listAttributes" 


-- | <p>Returns a list of existing clusters.</p>
listClusters :: forall eff. ListClustersRequest -> Aff (exception :: EXCEPTION | eff) ListClustersResponse
listClusters = Request.request serviceName "listClusters" 


-- | <p>Returns a list of container instances in a specified cluster. You can filter the results of a <code>ListContainerInstances</code> operation with cluster query language statements inside the <code>filter</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html">Cluster Query Language</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
listContainerInstances :: forall eff. ListContainerInstancesRequest -> Aff (exception :: EXCEPTION | eff) ListContainerInstancesResponse
listContainerInstances = Request.request serviceName "listContainerInstances" 


-- | <p>Lists the services that are running in a specified cluster.</p>
listServices :: forall eff. ListServicesRequest -> Aff (exception :: EXCEPTION | eff) ListServicesResponse
listServices = Request.request serviceName "listServices" 


-- | <p>Returns a list of task definition families that are registered to your account (which may include task definition families that no longer have any <code>ACTIVE</code> task definition revisions).</p> <p>You can filter out task definition families that do not contain any <code>ACTIVE</code> task definition revisions by setting the <code>status</code> parameter to <code>ACTIVE</code>. You can also filter the results with the <code>familyPrefix</code> parameter.</p>
listTaskDefinitionFamilies :: forall eff. ListTaskDefinitionFamiliesRequest -> Aff (exception :: EXCEPTION | eff) ListTaskDefinitionFamiliesResponse
listTaskDefinitionFamilies = Request.request serviceName "listTaskDefinitionFamilies" 


-- | <p>Returns a list of task definitions that are registered to your account. You can filter the results by family name with the <code>familyPrefix</code> parameter or by status with the <code>status</code> parameter.</p>
listTaskDefinitions :: forall eff. ListTaskDefinitionsRequest -> Aff (exception :: EXCEPTION | eff) ListTaskDefinitionsResponse
listTaskDefinitions = Request.request serviceName "listTaskDefinitions" 


-- | <p>Returns a list of tasks for a specified cluster. You can filter the results by family name, by a particular container instance, or by the desired status of the task with the <code>family</code>, <code>containerInstance</code>, and <code>desiredStatus</code> parameters.</p> <p>Recently stopped tasks might appear in the returned results. Currently, stopped tasks appear in the returned results for at least one hour. </p>
listTasks :: forall eff. ListTasksRequest -> Aff (exception :: EXCEPTION | eff) ListTasksResponse
listTasks = Request.request serviceName "listTasks" 


-- | <p>Create or update an attribute on an Amazon ECS resource. If the attribute does not exist, it is created. If the attribute exists, its value is replaced with the specified value. To delete an attribute, use <a>DeleteAttributes</a>. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes">Attributes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
putAttributes :: forall eff. PutAttributesRequest -> Aff (exception :: EXCEPTION | eff) PutAttributesResponse
putAttributes = Request.request serviceName "putAttributes" 


-- | <note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Registers an EC2 instance into the specified cluster. This instance becomes available to place containers on.</p>
registerContainerInstance :: forall eff. RegisterContainerInstanceRequest -> Aff (exception :: EXCEPTION | eff) RegisterContainerInstanceResponse
registerContainerInstance = Request.request serviceName "registerContainerInstance" 


-- | <p>Registers a new task definition from the supplied <code>family</code> and <code>containerDefinitions</code>. Optionally, you can add data volumes to your containers with the <code>volumes</code> parameter. For more information about task definition parameters and defaults, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html">Amazon ECS Task Definitions</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can specify an IAM role for your task with the <code>taskRoleArn</code> parameter. When you specify an IAM role for a task, its containers can then use the latest versions of the AWS CLI or SDKs to make API requests to the AWS services that are specified in the IAM policy associated with the role. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html">IAM Roles for Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can specify a Docker networking mode for the containers in your task definition with the <code>networkMode</code> parameter. The available network modes correspond to those described in <a href="https://docs.docker.com/engine/reference/run/#/network-settings">Network settings</a> in the Docker run reference. If you specify the <code>awsvpc</code> network mode, the task is allocated an Elastic Network Interface, and you must specify a <a>NetworkConfiguration</a> when you create a service or run a task with the task definition. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html">Task Networking</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
registerTaskDefinition :: forall eff. RegisterTaskDefinitionRequest -> Aff (exception :: EXCEPTION | eff) RegisterTaskDefinitionResponse
registerTaskDefinition = Request.request serviceName "registerTaskDefinition" 


-- | <p>Starts a new task using the specified task definition.</p> <p>You can allow Amazon ECS to place tasks for you, or you can customize how Amazon ECS places tasks using placement constraints and placement strategies. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html">Scheduling Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>Alternatively, you can use <a>StartTask</a> to use your own scheduler or place tasks manually on specific container instances.</p> <p>The Amazon ECS API follows an eventual consistency model, due to the distributed nature of the system supporting the API. This means that the result of an API command you run that affects your Amazon ECS resources might not be immediately visible to all subsequent commands you run. You should keep this in mind when you carry out an API command that immediately follows a previous API command.</p> <p>To manage eventual consistency, you can do the following:</p> <ul> <li> <p>Confirm the state of the resource before you run a command to modify it. Run the DescribeTasks command using an exponential backoff algorithm to ensure that you allow enough time for the previous command to propagate through the system. To do this, run the DescribeTasks command repeatedly, starting with a couple of seconds of wait time, and increasing gradually up to five minutes of wait time.</p> </li> <li> <p>Add wait time between subsequent commands, even if the DescribeTasks command returns an accurate response. Apply an exponential backoff algorithm starting with a couple of seconds of wait time, and increase gradually up to about five minutes of wait time.</p> </li> </ul>
runTask :: forall eff. RunTaskRequest -> Aff (exception :: EXCEPTION | eff) RunTaskResponse
runTask = Request.request serviceName "runTask" 


-- | <p>Starts a new task from the specified task definition on the specified container instance or instances.</p> <p>Alternatively, you can use <a>RunTask</a> to place tasks for you. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html">Scheduling Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
startTask :: forall eff. StartTaskRequest -> Aff (exception :: EXCEPTION | eff) StartTaskResponse
startTask = Request.request serviceName "startTask" 


-- | <p>Stops a running task.</p> <p>When <a>StopTask</a> is called on a task, the equivalent of <code>docker stop</code> is issued to the containers running in the task. This results in a <code>SIGTERM</code> and a default 30-second timeout, after which <code>SIGKILL</code> is sent and the containers are forcibly stopped. If the container handles the <code>SIGTERM</code> gracefully and exits within 30 seconds from receiving it, no <code>SIGKILL</code> is sent.</p> <note> <p>The default 30-second timeout can be configured on the Amazon ECS container agent with the <code>ECS_CONTAINER_STOP_TIMEOUT</code> variable. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html">Amazon ECS Container Agent Configuration</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </note>
stopTask :: forall eff. StopTaskRequest -> Aff (exception :: EXCEPTION | eff) StopTaskResponse
stopTask = Request.request serviceName "stopTask" 


-- | <note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Sent to acknowledge that a container changed states.</p>
submitContainerStateChange :: forall eff. SubmitContainerStateChangeRequest -> Aff (exception :: EXCEPTION | eff) SubmitContainerStateChangeResponse
submitContainerStateChange = Request.request serviceName "submitContainerStateChange" 


-- | <note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Sent to acknowledge that a task changed states.</p>
submitTaskStateChange :: forall eff. SubmitTaskStateChangeRequest -> Aff (exception :: EXCEPTION | eff) SubmitTaskStateChangeResponse
submitTaskStateChange = Request.request serviceName "submitTaskStateChange" 


-- | <p>Updates the Amazon ECS container agent on a specified container instance. Updating the Amazon ECS container agent does not interrupt running tasks or services on the container instance. The process for updating the agent differs depending on whether your container instance was launched with the Amazon ECS-optimized AMI or another operating system.</p> <p> <code>UpdateContainerAgent</code> requires the Amazon ECS-optimized AMI or Amazon Linux with the <code>ecs-init</code> service installed and running. For help updating the Amazon ECS container agent on other operating systems, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent">Manually Updating the Amazon ECS Container Agent</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
updateContainerAgent :: forall eff. UpdateContainerAgentRequest -> Aff (exception :: EXCEPTION | eff) UpdateContainerAgentResponse
updateContainerAgent = Request.request serviceName "updateContainerAgent" 


-- | <p>Modifies the status of an Amazon ECS container instance.</p> <p>You can change the status of a container instance to <code>DRAINING</code> to manually remove an instance from a cluster, for example to perform system updates, update the Docker daemon, or scale down the cluster size. </p> <p>When you set a container instance to <code>DRAINING</code>, Amazon ECS prevents new tasks from being scheduled for placement on the container instance and replacement service tasks are started on other container instances in the cluster if the resources are available. Service tasks on the container instance that are in the <code>PENDING</code> state are stopped immediately.</p> <p>Service tasks on the container instance that are in the <code>RUNNING</code> state are stopped and replaced according to the service's deployment configuration parameters, <code>minimumHealthyPercent</code> and <code>maximumPercent</code>. You can change the deployment configuration of your service using <a>UpdateService</a>.</p> <ul> <li> <p>If <code>minimumHealthyPercent</code> is below 100%, the scheduler can ignore <code>desiredCount</code> temporarily during task replacement. For example, <code>desiredCount</code> is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. If the minimum is 100%, the service scheduler can't remove existing tasks until the replacement tasks are considered healthy. Tasks for services that do not use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer.</p> </li> <li> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of running tasks during task replacement, which enables you to define the replacement batch size. For example, if <code>desiredCount</code> of four tasks, a maximum of 200% starts four new tasks before stopping the four tasks to be drained (provided that the cluster resources required to do this are available). If the maximum is 100%, then replacement tasks can't start until the draining tasks have stopped.</p> </li> </ul> <p>Any <code>PENDING</code> or <code>RUNNING</code> tasks that do not belong to a service are not affected; you must wait for them to finish or stop them manually.</p> <p>A container instance has completed draining when it has no more <code>RUNNING</code> tasks. You can verify this using <a>ListTasks</a>.</p> <p>When you set a container instance to <code>ACTIVE</code>, the Amazon ECS scheduler can begin scheduling tasks on the instance again.</p>
updateContainerInstancesState :: forall eff. UpdateContainerInstancesStateRequest -> Aff (exception :: EXCEPTION | eff) UpdateContainerInstancesStateResponse
updateContainerInstancesState = Request.request serviceName "updateContainerInstancesState" 


-- | <p>Modifies the desired count, deployment configuration, network configuration, or task definition used in a service.</p> <p>You can add to or subtract from the number of instantiations of a task definition in a service by specifying the cluster that the service is running in and a new <code>desiredCount</code> parameter.</p> <p>You can use <a>UpdateService</a> to modify your task definition and deploy a new version of your service.</p> <p>You can also update the deployment configuration of a service. When a deployment is triggered by updating the task definition of a service, the service scheduler uses the deployment configuration parameters, <code>minimumHealthyPercent</code> and <code>maximumPercent</code>, to determine the deployment strategy.</p> <ul> <li> <p>If <code>minimumHealthyPercent</code> is below 100%, the scheduler can ignore <code>desiredCount</code> temporarily during a deployment. For example, if <code>desiredCount</code> is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. Tasks for services that do not use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer.</p> </li> <li> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of running tasks during a deployment, which enables you to define the deployment batch size. For example, if <code>desiredCount</code> is four tasks, a maximum of 200% starts four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available).</p> </li> </ul> <p>When <a>UpdateService</a> stops a task during a deployment, the equivalent of <code>docker stop</code> is issued to the containers running in the task. This results in a <code>SIGTERM</code> and a 30-second timeout, after which <code>SIGKILL</code> is sent and the containers are forcibly stopped. If the container handles the <code>SIGTERM</code> gracefully and exits within 30 seconds from receiving it, no <code>SIGKILL</code> is sent.</p> <p>When the service scheduler launches new tasks, it determines task placement in your cluster with the following logic:</p> <ul> <li> <p>Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).</p> </li> <li> <p>By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy):</p> <ul> <li> <p>Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.</p> </li> <li> <p>Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.</p> </li> </ul> </li> </ul> <p>When the service scheduler stops running tasks, it attempts to maintain balance across the Availability Zones in your cluster using the following logic: </p> <ul> <li> <p>Sort the container instances by the largest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have two, container instances in either zone B or C are considered optimal for termination.</p> </li> <li> <p>Stop the task on a container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the largest number of running tasks for this service.</p> </li> </ul>
updateService :: forall eff. UpdateServiceRequest -> Aff (exception :: EXCEPTION | eff) UpdateServiceResponse
updateService = Request.request serviceName "updateService" 


-- | <p>You do not have authorization to perform the requested action.</p>
newtype AccessDeniedException = AccessDeniedException Types.NoArguments
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _
derive instance repGenericAccessDeniedException :: Generic AccessDeniedException _
instance showAccessDeniedException :: Show AccessDeniedException where
  show = genericShow
instance decodeAccessDeniedException :: Decode AccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDeniedException :: Encode AccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AgentUpdateStatus = AgentUpdateStatus String
derive instance newtypeAgentUpdateStatus :: Newtype AgentUpdateStatus _
derive instance repGenericAgentUpdateStatus :: Generic AgentUpdateStatus _
instance showAgentUpdateStatus :: Show AgentUpdateStatus where
  show = genericShow
instance decodeAgentUpdateStatus :: Decode AgentUpdateStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentUpdateStatus :: Encode AgentUpdateStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssignPublicIp = AssignPublicIp String
derive instance newtypeAssignPublicIp :: Newtype AssignPublicIp _
derive instance repGenericAssignPublicIp :: Generic AssignPublicIp _
instance showAssignPublicIp :: Show AssignPublicIp where
  show = genericShow
instance decodeAssignPublicIp :: Decode AssignPublicIp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssignPublicIp :: Encode AssignPublicIp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing a container instance or task attachment.</p>
newtype Attachment = Attachment 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "Details'" :: NullOrUndefined.NullOrUndefined (AttachmentDetails)
  }
derive instance newtypeAttachment :: Newtype Attachment _
derive instance repGenericAttachment :: Generic Attachment _
instance showAttachment :: Show Attachment where
  show = genericShow
instance decodeAttachment :: Decode Attachment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachment :: Encode Attachment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachmentDetails = AttachmentDetails (Array KeyValuePair)
derive instance newtypeAttachmentDetails :: Newtype AttachmentDetails _
derive instance repGenericAttachmentDetails :: Generic AttachmentDetails _
instance showAttachmentDetails :: Show AttachmentDetails where
  show = genericShow
instance decodeAttachmentDetails :: Decode AttachmentDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachmentDetails :: Encode AttachmentDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing a change in state for a task attachment.</p>
newtype AttachmentStateChange = AttachmentStateChange 
  { "AttachmentArn'" :: (String)
  , "Status'" :: (String)
  }
derive instance newtypeAttachmentStateChange :: Newtype AttachmentStateChange _
derive instance repGenericAttachmentStateChange :: Generic AttachmentStateChange _
instance showAttachmentStateChange :: Show AttachmentStateChange where
  show = genericShow
instance decodeAttachmentStateChange :: Decode AttachmentStateChange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachmentStateChange :: Encode AttachmentStateChange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttachmentStateChanges = AttachmentStateChanges (Array AttachmentStateChange)
derive instance newtypeAttachmentStateChanges :: Newtype AttachmentStateChanges _
derive instance repGenericAttachmentStateChanges :: Generic AttachmentStateChanges _
instance showAttachmentStateChanges :: Show AttachmentStateChanges where
  show = genericShow
instance decodeAttachmentStateChanges :: Decode AttachmentStateChanges where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachmentStateChanges :: Encode AttachmentStateChanges where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Attachments = Attachments (Array Attachment)
derive instance newtypeAttachments :: Newtype Attachments _
derive instance repGenericAttachments :: Generic Attachments _
instance showAttachments :: Show Attachments where
  show = genericShow
instance decodeAttachments :: Decode Attachments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachments :: Encode Attachments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes">Attributes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
newtype Attribute = Attribute 
  { "Name'" :: (String)
  , "Value'" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetType'" :: NullOrUndefined.NullOrUndefined (TargetType)
  , "TargetId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAttribute :: Newtype Attribute _
derive instance repGenericAttribute :: Generic Attribute _
instance showAttribute :: Show Attribute where
  show = genericShow
instance decodeAttribute :: Decode Attribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttribute :: Encode Attribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can apply up to 10 custom attributes per resource. You can view the attributes of a resource with <a>ListAttributes</a>. You can remove existing attributes on a resource with <a>DeleteAttributes</a>.</p>
newtype AttributeLimitExceededException = AttributeLimitExceededException Types.NoArguments
derive instance newtypeAttributeLimitExceededException :: Newtype AttributeLimitExceededException _
derive instance repGenericAttributeLimitExceededException :: Generic AttributeLimitExceededException _
instance showAttributeLimitExceededException :: Show AttributeLimitExceededException where
  show = genericShow
instance decodeAttributeLimitExceededException :: Decode AttributeLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeLimitExceededException :: Encode AttributeLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Attributes = Attributes (Array Attribute)
derive instance newtypeAttributes :: Newtype Attributes _
derive instance repGenericAttributes :: Generic Attributes _
instance showAttributes :: Show Attributes where
  show = genericShow
instance decodeAttributes :: Decode Attributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributes :: Encode Attributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the networking details for a task or service.</p>
newtype AwsVpcConfiguration = AwsVpcConfiguration 
  { "Subnets'" :: (StringList)
  , "SecurityGroups'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "AssignPublicIp'" :: NullOrUndefined.NullOrUndefined (AssignPublicIp)
  }
derive instance newtypeAwsVpcConfiguration :: Newtype AwsVpcConfiguration _
derive instance repGenericAwsVpcConfiguration :: Generic AwsVpcConfiguration _
instance showAwsVpcConfiguration :: Show AwsVpcConfiguration where
  show = genericShow
instance decodeAwsVpcConfiguration :: Decode AwsVpcConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAwsVpcConfiguration :: Encode AwsVpcConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your AWS account has been blocked. <a href="http://aws.amazon.com/contact-us/">Contact AWS Customer Support</a> for more information.</p>
newtype BlockedException = BlockedException Types.NoArguments
derive instance newtypeBlockedException :: Newtype BlockedException _
derive instance repGenericBlockedException :: Generic BlockedException _
instance showBlockedException :: Show BlockedException where
  show = genericShow
instance decodeBlockedException :: Decode BlockedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlockedException :: Encode BlockedException where
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


newtype BoxedInteger = BoxedInteger Int
derive instance newtypeBoxedInteger :: Newtype BoxedInteger _
derive instance repGenericBoxedInteger :: Generic BoxedInteger _
instance showBoxedInteger :: Show BoxedInteger where
  show = genericShow
instance decodeBoxedInteger :: Decode BoxedInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBoxedInteger :: Encode BoxedInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid.</p>
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


-- | <p>A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.</p>
newtype Cluster = Cluster 
  { "ClusterArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterName'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "RegisteredContainerInstancesCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "RunningTasksCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "PendingTasksCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "ActiveServicesCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Statistics'" :: NullOrUndefined.NullOrUndefined (Statistics)
  }
derive instance newtypeCluster :: Newtype Cluster _
derive instance repGenericCluster :: Generic Cluster _
instance showCluster :: Show Cluster where
  show = genericShow
instance decodeCluster :: Decode Cluster where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCluster :: Encode Cluster where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot delete a cluster that has registered container instances. You must first deregister the container instances before you can delete the cluster. For more information, see <a>DeregisterContainerInstance</a>.</p>
newtype ClusterContainsContainerInstancesException = ClusterContainsContainerInstancesException Types.NoArguments
derive instance newtypeClusterContainsContainerInstancesException :: Newtype ClusterContainsContainerInstancesException _
derive instance repGenericClusterContainsContainerInstancesException :: Generic ClusterContainsContainerInstancesException _
instance showClusterContainsContainerInstancesException :: Show ClusterContainsContainerInstancesException where
  show = genericShow
instance decodeClusterContainsContainerInstancesException :: Decode ClusterContainsContainerInstancesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterContainsContainerInstancesException :: Encode ClusterContainsContainerInstancesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot delete a cluster that contains services. You must first update the service to reduce its desired task count to 0 and then delete the service. For more information, see <a>UpdateService</a> and <a>DeleteService</a>.</p>
newtype ClusterContainsServicesException = ClusterContainsServicesException Types.NoArguments
derive instance newtypeClusterContainsServicesException :: Newtype ClusterContainsServicesException _
derive instance repGenericClusterContainsServicesException :: Generic ClusterContainsServicesException _
instance showClusterContainsServicesException :: Show ClusterContainsServicesException where
  show = genericShow
instance decodeClusterContainsServicesException :: Decode ClusterContainsServicesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterContainsServicesException :: Encode ClusterContainsServicesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You cannot delete a cluster that has active tasks.</p>
newtype ClusterContainsTasksException = ClusterContainsTasksException Types.NoArguments
derive instance newtypeClusterContainsTasksException :: Newtype ClusterContainsTasksException _
derive instance repGenericClusterContainsTasksException :: Generic ClusterContainsTasksException _
instance showClusterContainsTasksException :: Show ClusterContainsTasksException where
  show = genericShow
instance decodeClusterContainsTasksException :: Decode ClusterContainsTasksException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterContainsTasksException :: Encode ClusterContainsTasksException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterField = ClusterField String
derive instance newtypeClusterField :: Newtype ClusterField _
derive instance repGenericClusterField :: Generic ClusterField _
instance showClusterField :: Show ClusterField where
  show = genericShow
instance decodeClusterField :: Decode ClusterField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterField :: Encode ClusterField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterFieldList = ClusterFieldList (Array ClusterField)
derive instance newtypeClusterFieldList :: Newtype ClusterFieldList _
derive instance repGenericClusterFieldList :: Generic ClusterFieldList _
instance showClusterFieldList :: Show ClusterFieldList where
  show = genericShow
instance decodeClusterFieldList :: Decode ClusterFieldList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterFieldList :: Encode ClusterFieldList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified cluster could not be found. You can view your available clusters with <a>ListClusters</a>. Amazon ECS clusters are region-specific.</p>
newtype ClusterNotFoundException = ClusterNotFoundException Types.NoArguments
derive instance newtypeClusterNotFoundException :: Newtype ClusterNotFoundException _
derive instance repGenericClusterNotFoundException :: Generic ClusterNotFoundException _
instance showClusterNotFoundException :: Show ClusterNotFoundException where
  show = genericShow
instance decodeClusterNotFoundException :: Decode ClusterNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterNotFoundException :: Encode ClusterNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Clusters = Clusters (Array Cluster)
derive instance newtypeClusters :: Newtype Clusters _
derive instance repGenericClusters :: Generic Clusters _
instance showClusters :: Show Clusters where
  show = genericShow
instance decodeClusters :: Decode Clusters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusters :: Encode Clusters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Compatibility = Compatibility String
derive instance newtypeCompatibility :: Newtype Compatibility _
derive instance repGenericCompatibility :: Generic Compatibility _
instance showCompatibility :: Show Compatibility where
  show = genericShow
instance decodeCompatibility :: Decode Compatibility where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompatibility :: Encode Compatibility where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompatibilityList = CompatibilityList (Array Compatibility)
derive instance newtypeCompatibilityList :: Newtype CompatibilityList _
derive instance repGenericCompatibilityList :: Generic CompatibilityList _
instance showCompatibilityList :: Show CompatibilityList where
  show = genericShow
instance decodeCompatibilityList :: Decode CompatibilityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompatibilityList :: Encode CompatibilityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Connectivity = Connectivity String
derive instance newtypeConnectivity :: Newtype Connectivity _
derive instance repGenericConnectivity :: Generic Connectivity _
instance showConnectivity :: Show Connectivity where
  show = genericShow
instance decodeConnectivity :: Decode Connectivity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectivity :: Encode Connectivity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A Docker container that is part of a task.</p>
newtype Container = Container 
  { "ContainerArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "LastStatus'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  , "NetworkBindings'" :: NullOrUndefined.NullOrUndefined (NetworkBindings)
  , "NetworkInterfaces'" :: NullOrUndefined.NullOrUndefined (NetworkInterfaces)
  }
derive instance newtypeContainer :: Newtype Container _
derive instance repGenericContainer :: Generic Container _
instance showContainer :: Show Container where
  show = genericShow
instance decodeContainer :: Decode Container where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainer :: Encode Container where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Container definitions are used in task definitions to describe the different containers that are launched as part of a task.</p>
newtype ContainerDefinition = ContainerDefinition 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Image'" :: NullOrUndefined.NullOrUndefined (String)
  , "Cpu'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Memory'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "MemoryReservation'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "Links'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "PortMappings'" :: NullOrUndefined.NullOrUndefined (PortMappingList)
  , "Essential'" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  , "EntryPoint'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "Command'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "Environment'" :: NullOrUndefined.NullOrUndefined (EnvironmentVariables)
  , "MountPoints'" :: NullOrUndefined.NullOrUndefined (MountPointList)
  , "VolumesFrom'" :: NullOrUndefined.NullOrUndefined (VolumeFromList)
  , "LinuxParameters'" :: NullOrUndefined.NullOrUndefined (LinuxParameters)
  , "Hostname'" :: NullOrUndefined.NullOrUndefined (String)
  , "User'" :: NullOrUndefined.NullOrUndefined (String)
  , "WorkingDirectory'" :: NullOrUndefined.NullOrUndefined (String)
  , "DisableNetworking'" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  , "Privileged'" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  , "ReadonlyRootFilesystem'" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  , "DnsServers'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "DnsSearchDomains'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "ExtraHosts'" :: NullOrUndefined.NullOrUndefined (HostEntryList)
  , "DockerSecurityOptions'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "DockerLabels'" :: NullOrUndefined.NullOrUndefined (DockerLabelsMap)
  , "Ulimits'" :: NullOrUndefined.NullOrUndefined (UlimitList)
  , "LogConfiguration'" :: NullOrUndefined.NullOrUndefined (LogConfiguration)
  }
derive instance newtypeContainerDefinition :: Newtype ContainerDefinition _
derive instance repGenericContainerDefinition :: Generic ContainerDefinition _
instance showContainerDefinition :: Show ContainerDefinition where
  show = genericShow
instance decodeContainerDefinition :: Decode ContainerDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerDefinition :: Encode ContainerDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerDefinitions = ContainerDefinitions (Array ContainerDefinition)
derive instance newtypeContainerDefinitions :: Newtype ContainerDefinitions _
derive instance repGenericContainerDefinitions :: Generic ContainerDefinitions _
instance showContainerDefinitions :: Show ContainerDefinitions where
  show = genericShow
instance decodeContainerDefinitions :: Decode ContainerDefinitions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerDefinitions :: Encode ContainerDefinitions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.</p>
newtype ContainerInstance = ContainerInstance 
  { "ContainerInstanceArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Ec2InstanceId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Number)
  , "VersionInfo'" :: NullOrUndefined.NullOrUndefined (VersionInfo)
  , "RemainingResources'" :: NullOrUndefined.NullOrUndefined (Resources)
  , "RegisteredResources'" :: NullOrUndefined.NullOrUndefined (Resources)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "AgentConnected'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RunningTasksCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "PendingTasksCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "AgentUpdateStatus'" :: NullOrUndefined.NullOrUndefined (AgentUpdateStatus)
  , "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  , "RegisteredAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Attachments'" :: NullOrUndefined.NullOrUndefined (Attachments)
  }
derive instance newtypeContainerInstance :: Newtype ContainerInstance _
derive instance repGenericContainerInstance :: Generic ContainerInstance _
instance showContainerInstance :: Show ContainerInstance where
  show = genericShow
instance decodeContainerInstance :: Decode ContainerInstance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerInstance :: Encode ContainerInstance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerInstanceStatus = ContainerInstanceStatus String
derive instance newtypeContainerInstanceStatus :: Newtype ContainerInstanceStatus _
derive instance repGenericContainerInstanceStatus :: Generic ContainerInstanceStatus _
instance showContainerInstanceStatus :: Show ContainerInstanceStatus where
  show = genericShow
instance decodeContainerInstanceStatus :: Decode ContainerInstanceStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerInstanceStatus :: Encode ContainerInstanceStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerInstances = ContainerInstances (Array ContainerInstance)
derive instance newtypeContainerInstances :: Newtype ContainerInstances _
derive instance repGenericContainerInstances :: Generic ContainerInstances _
instance showContainerInstances :: Show ContainerInstances where
  show = genericShow
instance decodeContainerInstances :: Decode ContainerInstances where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerInstances :: Encode ContainerInstances where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The overrides that should be sent to a container.</p>
newtype ContainerOverride = ContainerOverride 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Command'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "Environment'" :: NullOrUndefined.NullOrUndefined (EnvironmentVariables)
  , "Cpu'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "Memory'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "MemoryReservation'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeContainerOverride :: Newtype ContainerOverride _
derive instance repGenericContainerOverride :: Generic ContainerOverride _
instance showContainerOverride :: Show ContainerOverride where
  show = genericShow
instance decodeContainerOverride :: Decode ContainerOverride where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerOverride :: Encode ContainerOverride where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerOverrides = ContainerOverrides (Array ContainerOverride)
derive instance newtypeContainerOverrides :: Newtype ContainerOverrides _
derive instance repGenericContainerOverrides :: Generic ContainerOverrides _
instance showContainerOverrides :: Show ContainerOverrides where
  show = genericShow
instance decodeContainerOverrides :: Decode ContainerOverrides where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerOverrides :: Encode ContainerOverrides where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing a change in state for a container.</p>
newtype ContainerStateChange = ContainerStateChange 
  { "ContainerName'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "NetworkBindings'" :: NullOrUndefined.NullOrUndefined (NetworkBindings)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeContainerStateChange :: Newtype ContainerStateChange _
derive instance repGenericContainerStateChange :: Generic ContainerStateChange _
instance showContainerStateChange :: Show ContainerStateChange where
  show = genericShow
instance decodeContainerStateChange :: Decode ContainerStateChange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerStateChange :: Encode ContainerStateChange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContainerStateChanges = ContainerStateChanges (Array ContainerStateChange)
derive instance newtypeContainerStateChanges :: Newtype ContainerStateChanges _
derive instance repGenericContainerStateChanges :: Generic ContainerStateChanges _
instance showContainerStateChanges :: Show ContainerStateChanges where
  show = genericShow
instance decodeContainerStateChanges :: Decode ContainerStateChanges where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainerStateChanges :: Encode ContainerStateChanges where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Containers = Containers (Array Container)
derive instance newtypeContainers :: Newtype Containers _
derive instance repGenericContainers :: Generic Containers _
instance showContainers :: Show Containers where
  show = genericShow
instance decodeContainers :: Decode Containers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContainers :: Encode Containers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterRequest = CreateClusterRequest 
  { "ClusterName'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateClusterRequest :: Newtype CreateClusterRequest _
derive instance repGenericCreateClusterRequest :: Generic CreateClusterRequest _
instance showCreateClusterRequest :: Show CreateClusterRequest where
  show = genericShow
instance decodeCreateClusterRequest :: Decode CreateClusterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterRequest :: Encode CreateClusterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClusterResponse = CreateClusterResponse 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeCreateClusterResponse :: Newtype CreateClusterResponse _
derive instance repGenericCreateClusterResponse :: Generic CreateClusterResponse _
instance showCreateClusterResponse :: Show CreateClusterResponse where
  show = genericShow
instance decodeCreateClusterResponse :: Decode CreateClusterResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClusterResponse :: Encode CreateClusterResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateServiceRequest = CreateServiceRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "ServiceName'" :: (String)
  , "TaskDefinition'" :: (String)
  , "LoadBalancers'" :: NullOrUndefined.NullOrUndefined (LoadBalancers)
  , "DesiredCount'" :: (BoxedInteger)
  , "ClientToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "LaunchType'" :: NullOrUndefined.NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "Role'" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentConfiguration'" :: NullOrUndefined.NullOrUndefined (DeploymentConfiguration)
  , "PlacementConstraints'" :: NullOrUndefined.NullOrUndefined (PlacementConstraints)
  , "PlacementStrategy'" :: NullOrUndefined.NullOrUndefined (PlacementStrategies)
  , "NetworkConfiguration'" :: NullOrUndefined.NullOrUndefined (NetworkConfiguration)
  , "HealthCheckGracePeriodSeconds'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeCreateServiceRequest :: Newtype CreateServiceRequest _
derive instance repGenericCreateServiceRequest :: Generic CreateServiceRequest _
instance showCreateServiceRequest :: Show CreateServiceRequest where
  show = genericShow
instance decodeCreateServiceRequest :: Decode CreateServiceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateServiceRequest :: Encode CreateServiceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateServiceResponse = CreateServiceResponse 
  { "Service'" :: NullOrUndefined.NullOrUndefined (Service)
  }
derive instance newtypeCreateServiceResponse :: Newtype CreateServiceResponse _
derive instance repGenericCreateServiceResponse :: Generic CreateServiceResponse _
instance showCreateServiceResponse :: Show CreateServiceResponse where
  show = genericShow
instance decodeCreateServiceResponse :: Decode CreateServiceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateServiceResponse :: Encode CreateServiceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAttributesRequest = DeleteAttributesRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes'" :: (Attributes)
  }
derive instance newtypeDeleteAttributesRequest :: Newtype DeleteAttributesRequest _
derive instance repGenericDeleteAttributesRequest :: Generic DeleteAttributesRequest _
instance showDeleteAttributesRequest :: Show DeleteAttributesRequest where
  show = genericShow
instance decodeDeleteAttributesRequest :: Decode DeleteAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAttributesRequest :: Encode DeleteAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAttributesResponse = DeleteAttributesResponse 
  { "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypeDeleteAttributesResponse :: Newtype DeleteAttributesResponse _
derive instance repGenericDeleteAttributesResponse :: Generic DeleteAttributesResponse _
instance showDeleteAttributesResponse :: Show DeleteAttributesResponse where
  show = genericShow
instance decodeDeleteAttributesResponse :: Decode DeleteAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAttributesResponse :: Encode DeleteAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteClusterRequest = DeleteClusterRequest 
  { "Cluster'" :: (String)
  }
derive instance newtypeDeleteClusterRequest :: Newtype DeleteClusterRequest _
derive instance repGenericDeleteClusterRequest :: Generic DeleteClusterRequest _
instance showDeleteClusterRequest :: Show DeleteClusterRequest where
  show = genericShow
instance decodeDeleteClusterRequest :: Decode DeleteClusterRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterRequest :: Encode DeleteClusterRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteClusterResponse = DeleteClusterResponse 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (Cluster)
  }
derive instance newtypeDeleteClusterResponse :: Newtype DeleteClusterResponse _
derive instance repGenericDeleteClusterResponse :: Generic DeleteClusterResponse _
instance showDeleteClusterResponse :: Show DeleteClusterResponse where
  show = genericShow
instance decodeDeleteClusterResponse :: Decode DeleteClusterResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClusterResponse :: Encode DeleteClusterResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteServiceRequest = DeleteServiceRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Service'" :: (String)
  }
derive instance newtypeDeleteServiceRequest :: Newtype DeleteServiceRequest _
derive instance repGenericDeleteServiceRequest :: Generic DeleteServiceRequest _
instance showDeleteServiceRequest :: Show DeleteServiceRequest where
  show = genericShow
instance decodeDeleteServiceRequest :: Decode DeleteServiceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteServiceRequest :: Encode DeleteServiceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteServiceResponse = DeleteServiceResponse 
  { "Service'" :: NullOrUndefined.NullOrUndefined (Service)
  }
derive instance newtypeDeleteServiceResponse :: Newtype DeleteServiceResponse _
derive instance repGenericDeleteServiceResponse :: Generic DeleteServiceResponse _
instance showDeleteServiceResponse :: Show DeleteServiceResponse where
  show = genericShow
instance decodeDeleteServiceResponse :: Decode DeleteServiceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteServiceResponse :: Encode DeleteServiceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The details of an Amazon ECS service deployment.</p>
newtype Deployment = Deployment 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskDefinition'" :: NullOrUndefined.NullOrUndefined (String)
  , "DesiredCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "PendingCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "RunningCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "UpdatedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LaunchType'" :: NullOrUndefined.NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "NetworkConfiguration'" :: NullOrUndefined.NullOrUndefined (NetworkConfiguration)
  }
derive instance newtypeDeployment :: Newtype Deployment _
derive instance repGenericDeployment :: Generic Deployment _
instance showDeployment :: Show Deployment where
  show = genericShow
instance decodeDeployment :: Decode Deployment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeployment :: Encode Deployment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.</p>
newtype DeploymentConfiguration = DeploymentConfiguration 
  { "MaximumPercent'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "MinimumHealthyPercent'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeDeploymentConfiguration :: Newtype DeploymentConfiguration _
derive instance repGenericDeploymentConfiguration :: Generic DeploymentConfiguration _
instance showDeploymentConfiguration :: Show DeploymentConfiguration where
  show = genericShow
instance decodeDeploymentConfiguration :: Decode DeploymentConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeploymentConfiguration :: Encode DeploymentConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Deployments = Deployments (Array Deployment)
derive instance newtypeDeployments :: Newtype Deployments _
derive instance repGenericDeployments :: Generic Deployments _
instance showDeployments :: Show Deployments where
  show = genericShow
instance decodeDeployments :: Decode Deployments where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeployments :: Encode Deployments where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterContainerInstanceRequest = DeregisterContainerInstanceRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerInstance'" :: (String)
  , "Force'" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeDeregisterContainerInstanceRequest :: Newtype DeregisterContainerInstanceRequest _
derive instance repGenericDeregisterContainerInstanceRequest :: Generic DeregisterContainerInstanceRequest _
instance showDeregisterContainerInstanceRequest :: Show DeregisterContainerInstanceRequest where
  show = genericShow
instance decodeDeregisterContainerInstanceRequest :: Decode DeregisterContainerInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterContainerInstanceRequest :: Encode DeregisterContainerInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse 
  { "ContainerInstance'" :: NullOrUndefined.NullOrUndefined (ContainerInstance)
  }
derive instance newtypeDeregisterContainerInstanceResponse :: Newtype DeregisterContainerInstanceResponse _
derive instance repGenericDeregisterContainerInstanceResponse :: Generic DeregisterContainerInstanceResponse _
instance showDeregisterContainerInstanceResponse :: Show DeregisterContainerInstanceResponse where
  show = genericShow
instance decodeDeregisterContainerInstanceResponse :: Decode DeregisterContainerInstanceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterContainerInstanceResponse :: Encode DeregisterContainerInstanceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterTaskDefinitionRequest = DeregisterTaskDefinitionRequest 
  { "TaskDefinition'" :: (String)
  }
derive instance newtypeDeregisterTaskDefinitionRequest :: Newtype DeregisterTaskDefinitionRequest _
derive instance repGenericDeregisterTaskDefinitionRequest :: Generic DeregisterTaskDefinitionRequest _
instance showDeregisterTaskDefinitionRequest :: Show DeregisterTaskDefinitionRequest where
  show = genericShow
instance decodeDeregisterTaskDefinitionRequest :: Decode DeregisterTaskDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterTaskDefinitionRequest :: Encode DeregisterTaskDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse 
  { "TaskDefinition'" :: NullOrUndefined.NullOrUndefined (TaskDefinition)
  }
derive instance newtypeDeregisterTaskDefinitionResponse :: Newtype DeregisterTaskDefinitionResponse _
derive instance repGenericDeregisterTaskDefinitionResponse :: Generic DeregisterTaskDefinitionResponse _
instance showDeregisterTaskDefinitionResponse :: Show DeregisterTaskDefinitionResponse where
  show = genericShow
instance decodeDeregisterTaskDefinitionResponse :: Decode DeregisterTaskDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterTaskDefinitionResponse :: Encode DeregisterTaskDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeClustersRequest = DescribeClustersRequest 
  { "Clusters'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "Include'" :: NullOrUndefined.NullOrUndefined (ClusterFieldList)
  }
derive instance newtypeDescribeClustersRequest :: Newtype DescribeClustersRequest _
derive instance repGenericDescribeClustersRequest :: Generic DescribeClustersRequest _
instance showDescribeClustersRequest :: Show DescribeClustersRequest where
  show = genericShow
instance decodeDescribeClustersRequest :: Decode DescribeClustersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClustersRequest :: Encode DescribeClustersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeClustersResponse = DescribeClustersResponse 
  { "Clusters'" :: NullOrUndefined.NullOrUndefined (Clusters)
  , "Failures'" :: NullOrUndefined.NullOrUndefined (Failures)
  }
derive instance newtypeDescribeClustersResponse :: Newtype DescribeClustersResponse _
derive instance repGenericDescribeClustersResponse :: Generic DescribeClustersResponse _
instance showDescribeClustersResponse :: Show DescribeClustersResponse where
  show = genericShow
instance decodeDescribeClustersResponse :: Decode DescribeClustersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeClustersResponse :: Encode DescribeClustersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeContainerInstancesRequest = DescribeContainerInstancesRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerInstances'" :: (StringList)
  }
derive instance newtypeDescribeContainerInstancesRequest :: Newtype DescribeContainerInstancesRequest _
derive instance repGenericDescribeContainerInstancesRequest :: Generic DescribeContainerInstancesRequest _
instance showDescribeContainerInstancesRequest :: Show DescribeContainerInstancesRequest where
  show = genericShow
instance decodeDescribeContainerInstancesRequest :: Decode DescribeContainerInstancesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeContainerInstancesRequest :: Encode DescribeContainerInstancesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeContainerInstancesResponse = DescribeContainerInstancesResponse 
  { "ContainerInstances'" :: NullOrUndefined.NullOrUndefined (ContainerInstances)
  , "Failures'" :: NullOrUndefined.NullOrUndefined (Failures)
  }
derive instance newtypeDescribeContainerInstancesResponse :: Newtype DescribeContainerInstancesResponse _
derive instance repGenericDescribeContainerInstancesResponse :: Generic DescribeContainerInstancesResponse _
instance showDescribeContainerInstancesResponse :: Show DescribeContainerInstancesResponse where
  show = genericShow
instance decodeDescribeContainerInstancesResponse :: Decode DescribeContainerInstancesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeContainerInstancesResponse :: Encode DescribeContainerInstancesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeServicesRequest = DescribeServicesRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Services'" :: (StringList)
  }
derive instance newtypeDescribeServicesRequest :: Newtype DescribeServicesRequest _
derive instance repGenericDescribeServicesRequest :: Generic DescribeServicesRequest _
instance showDescribeServicesRequest :: Show DescribeServicesRequest where
  show = genericShow
instance decodeDescribeServicesRequest :: Decode DescribeServicesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeServicesRequest :: Encode DescribeServicesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeServicesResponse = DescribeServicesResponse 
  { "Services'" :: NullOrUndefined.NullOrUndefined (Services)
  , "Failures'" :: NullOrUndefined.NullOrUndefined (Failures)
  }
derive instance newtypeDescribeServicesResponse :: Newtype DescribeServicesResponse _
derive instance repGenericDescribeServicesResponse :: Generic DescribeServicesResponse _
instance showDescribeServicesResponse :: Show DescribeServicesResponse where
  show = genericShow
instance decodeDescribeServicesResponse :: Decode DescribeServicesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeServicesResponse :: Encode DescribeServicesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTaskDefinitionRequest = DescribeTaskDefinitionRequest 
  { "TaskDefinition'" :: (String)
  }
derive instance newtypeDescribeTaskDefinitionRequest :: Newtype DescribeTaskDefinitionRequest _
derive instance repGenericDescribeTaskDefinitionRequest :: Generic DescribeTaskDefinitionRequest _
instance showDescribeTaskDefinitionRequest :: Show DescribeTaskDefinitionRequest where
  show = genericShow
instance decodeDescribeTaskDefinitionRequest :: Decode DescribeTaskDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTaskDefinitionRequest :: Encode DescribeTaskDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse 
  { "TaskDefinition'" :: NullOrUndefined.NullOrUndefined (TaskDefinition)
  }
derive instance newtypeDescribeTaskDefinitionResponse :: Newtype DescribeTaskDefinitionResponse _
derive instance repGenericDescribeTaskDefinitionResponse :: Generic DescribeTaskDefinitionResponse _
instance showDescribeTaskDefinitionResponse :: Show DescribeTaskDefinitionResponse where
  show = genericShow
instance decodeDescribeTaskDefinitionResponse :: Decode DescribeTaskDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTaskDefinitionResponse :: Encode DescribeTaskDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTasksRequest = DescribeTasksRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Tasks'" :: (StringList)
  }
derive instance newtypeDescribeTasksRequest :: Newtype DescribeTasksRequest _
derive instance repGenericDescribeTasksRequest :: Generic DescribeTasksRequest _
instance showDescribeTasksRequest :: Show DescribeTasksRequest where
  show = genericShow
instance decodeDescribeTasksRequest :: Decode DescribeTasksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTasksRequest :: Encode DescribeTasksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTasksResponse = DescribeTasksResponse 
  { "Tasks'" :: NullOrUndefined.NullOrUndefined (Tasks)
  , "Failures'" :: NullOrUndefined.NullOrUndefined (Failures)
  }
derive instance newtypeDescribeTasksResponse :: Newtype DescribeTasksResponse _
derive instance repGenericDescribeTasksResponse :: Generic DescribeTasksResponse _
instance showDescribeTasksResponse :: Show DescribeTasksResponse where
  show = genericShow
instance decodeDescribeTasksResponse :: Decode DescribeTasksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTasksResponse :: Encode DescribeTasksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DesiredStatus = DesiredStatus String
derive instance newtypeDesiredStatus :: Newtype DesiredStatus _
derive instance repGenericDesiredStatus :: Generic DesiredStatus _
instance showDesiredStatus :: Show DesiredStatus where
  show = genericShow
instance decodeDesiredStatus :: Decode DesiredStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDesiredStatus :: Encode DesiredStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing a container instance host device.</p>
newtype Device = Device 
  { "HostPath'" :: (String)
  , "ContainerPath'" :: NullOrUndefined.NullOrUndefined (String)
  , "Permissions'" :: NullOrUndefined.NullOrUndefined (DeviceCgroupPermissions)
  }
derive instance newtypeDevice :: Newtype Device _
derive instance repGenericDevice :: Generic Device _
instance showDevice :: Show Device where
  show = genericShow
instance decodeDevice :: Decode Device where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevice :: Encode Device where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceCgroupPermission = DeviceCgroupPermission String
derive instance newtypeDeviceCgroupPermission :: Newtype DeviceCgroupPermission _
derive instance repGenericDeviceCgroupPermission :: Generic DeviceCgroupPermission _
instance showDeviceCgroupPermission :: Show DeviceCgroupPermission where
  show = genericShow
instance decodeDeviceCgroupPermission :: Decode DeviceCgroupPermission where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceCgroupPermission :: Encode DeviceCgroupPermission where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeviceCgroupPermissions = DeviceCgroupPermissions (Array DeviceCgroupPermission)
derive instance newtypeDeviceCgroupPermissions :: Newtype DeviceCgroupPermissions _
derive instance repGenericDeviceCgroupPermissions :: Generic DeviceCgroupPermissions _
instance showDeviceCgroupPermissions :: Show DeviceCgroupPermissions where
  show = genericShow
instance decodeDeviceCgroupPermissions :: Decode DeviceCgroupPermissions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeviceCgroupPermissions :: Encode DeviceCgroupPermissions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DevicesList = DevicesList (Array Device)
derive instance newtypeDevicesList :: Newtype DevicesList _
derive instance repGenericDevicesList :: Generic DevicesList _
instance showDevicesList :: Show DevicesList where
  show = genericShow
instance decodeDevicesList :: Decode DevicesList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevicesList :: Encode DevicesList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiscoverPollEndpointRequest = DiscoverPollEndpointRequest 
  { "ContainerInstance'" :: NullOrUndefined.NullOrUndefined (String)
  , "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDiscoverPollEndpointRequest :: Newtype DiscoverPollEndpointRequest _
derive instance repGenericDiscoverPollEndpointRequest :: Generic DiscoverPollEndpointRequest _
instance showDiscoverPollEndpointRequest :: Show DiscoverPollEndpointRequest where
  show = genericShow
instance decodeDiscoverPollEndpointRequest :: Decode DiscoverPollEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiscoverPollEndpointRequest :: Encode DiscoverPollEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DiscoverPollEndpointResponse = DiscoverPollEndpointResponse 
  { "Endpoint'" :: NullOrUndefined.NullOrUndefined (String)
  , "TelemetryEndpoint'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeDiscoverPollEndpointResponse :: Newtype DiscoverPollEndpointResponse _
derive instance repGenericDiscoverPollEndpointResponse :: Generic DiscoverPollEndpointResponse _
instance showDiscoverPollEndpointResponse :: Show DiscoverPollEndpointResponse where
  show = genericShow
instance decodeDiscoverPollEndpointResponse :: Decode DiscoverPollEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDiscoverPollEndpointResponse :: Encode DiscoverPollEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DockerLabelsMap = DockerLabelsMap (StrMap.StrMap String)
derive instance newtypeDockerLabelsMap :: Newtype DockerLabelsMap _
derive instance repGenericDockerLabelsMap :: Generic DockerLabelsMap _
instance showDockerLabelsMap :: Show DockerLabelsMap where
  show = genericShow
instance decodeDockerLabelsMap :: Decode DockerLabelsMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDockerLabelsMap :: Encode DockerLabelsMap where
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


-- | <p>A failed resource.</p>
newtype Failure = Failure 
  { "Arn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeFailure :: Newtype Failure _
derive instance repGenericFailure :: Generic Failure _
instance showFailure :: Show Failure where
  show = genericShow
instance decodeFailure :: Decode Failure where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailure :: Encode Failure where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Failures = Failures (Array Failure)
derive instance newtypeFailures :: Newtype Failures _
derive instance repGenericFailures :: Generic Failures _
instance showFailures :: Show Failures where
  show = genericShow
instance decodeFailures :: Decode Failures where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailures :: Encode Failures where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Hostnames and IP address entries that are added to the <code>/etc/hosts</code> file of a container via the <code>extraHosts</code> parameter of its <a>ContainerDefinition</a>. </p>
newtype HostEntry = HostEntry 
  { "Hostname'" :: (String)
  , "IpAddress'" :: (String)
  }
derive instance newtypeHostEntry :: Newtype HostEntry _
derive instance repGenericHostEntry :: Generic HostEntry _
instance showHostEntry :: Show HostEntry where
  show = genericShow
instance decodeHostEntry :: Decode HostEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostEntry :: Encode HostEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HostEntryList = HostEntryList (Array HostEntry)
derive instance newtypeHostEntryList :: Newtype HostEntryList _
derive instance repGenericHostEntryList :: Generic HostEntryList _
instance showHostEntryList :: Show HostEntryList where
  show = genericShow
instance decodeHostEntryList :: Decode HostEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostEntryList :: Encode HostEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details on a container instance host volume.</p>
newtype HostVolumeProperties = HostVolumeProperties 
  { "SourcePath'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeHostVolumeProperties :: Newtype HostVolumeProperties _
derive instance repGenericHostVolumeProperties :: Generic HostVolumeProperties _
instance showHostVolumeProperties :: Show HostVolumeProperties where
  show = genericShow
instance decodeHostVolumeProperties :: Decode HostVolumeProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostVolumeProperties :: Encode HostVolumeProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified parameter is invalid. Review the available parameters for the API request.</p>
newtype InvalidParameterException = InvalidParameterException Types.NoArguments
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <a href="https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities">Runtime privilege and Linux capabilities</a> in the <i>Docker run reference</i>. For more detailed information on these Linux capabilities, see the <a href="http://man7.org/linux/man-pages/man7/capabilities.7.html">capabilities(7)</a> Linux manual page.</p>
newtype KernelCapabilities = KernelCapabilities 
  { "Add'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "Drop'" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeKernelCapabilities :: Newtype KernelCapabilities _
derive instance repGenericKernelCapabilities :: Generic KernelCapabilities _
instance showKernelCapabilities :: Show KernelCapabilities where
  show = genericShow
instance decodeKernelCapabilities :: Decode KernelCapabilities where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKernelCapabilities :: Encode KernelCapabilities where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A key and value pair object.</p>
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


newtype LaunchType = LaunchType String
derive instance newtypeLaunchType :: Newtype LaunchType _
derive instance repGenericLaunchType :: Generic LaunchType _
instance showLaunchType :: Show LaunchType where
  show = genericShow
instance decodeLaunchType :: Decode LaunchType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLaunchType :: Encode LaunchType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Linux-specific options that are applied to the container, such as Linux <a>KernelCapabilities</a>.</p>
newtype LinuxParameters = LinuxParameters 
  { "Capabilities'" :: NullOrUndefined.NullOrUndefined (KernelCapabilities)
  , "Devices'" :: NullOrUndefined.NullOrUndefined (DevicesList)
  , "InitProcessEnabled'" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeLinuxParameters :: Newtype LinuxParameters _
derive instance repGenericLinuxParameters :: Generic LinuxParameters _
instance showLinuxParameters :: Show LinuxParameters where
  show = genericShow
instance decodeLinuxParameters :: Decode LinuxParameters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLinuxParameters :: Encode LinuxParameters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAttributesRequest = ListAttributesRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "TargetType'" :: (TargetType)
  , "AttributeName'" :: NullOrUndefined.NullOrUndefined (String)
  , "AttributeValue'" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeListAttributesRequest :: Newtype ListAttributesRequest _
derive instance repGenericListAttributesRequest :: Generic ListAttributesRequest _
instance showListAttributesRequest :: Show ListAttributesRequest where
  show = genericShow
instance decodeListAttributesRequest :: Decode ListAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAttributesRequest :: Encode ListAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAttributesResponse = ListAttributesResponse 
  { "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListAttributesResponse :: Newtype ListAttributesResponse _
derive instance repGenericListAttributesResponse :: Generic ListAttributesResponse _
instance showListAttributesResponse :: Show ListAttributesResponse where
  show = genericShow
instance decodeListAttributesResponse :: Decode ListAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAttributesResponse :: Encode ListAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListClustersRequest = ListClustersRequest 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeListClustersRequest :: Newtype ListClustersRequest _
derive instance repGenericListClustersRequest :: Generic ListClustersRequest _
instance showListClustersRequest :: Show ListClustersRequest where
  show = genericShow
instance decodeListClustersRequest :: Decode ListClustersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListClustersRequest :: Encode ListClustersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListClustersResponse = ListClustersResponse 
  { "ClusterArns'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListClustersResponse :: Newtype ListClustersResponse _
derive instance repGenericListClustersResponse :: Generic ListClustersResponse _
instance showListClustersResponse :: Show ListClustersResponse where
  show = genericShow
instance decodeListClustersResponse :: Decode ListClustersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListClustersResponse :: Encode ListClustersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListContainerInstancesRequest = ListContainerInstancesRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Filter'" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "Status'" :: NullOrUndefined.NullOrUndefined (ContainerInstanceStatus)
  }
derive instance newtypeListContainerInstancesRequest :: Newtype ListContainerInstancesRequest _
derive instance repGenericListContainerInstancesRequest :: Generic ListContainerInstancesRequest _
instance showListContainerInstancesRequest :: Show ListContainerInstancesRequest where
  show = genericShow
instance decodeListContainerInstancesRequest :: Decode ListContainerInstancesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListContainerInstancesRequest :: Encode ListContainerInstancesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListContainerInstancesResponse = ListContainerInstancesResponse 
  { "ContainerInstanceArns'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListContainerInstancesResponse :: Newtype ListContainerInstancesResponse _
derive instance repGenericListContainerInstancesResponse :: Generic ListContainerInstancesResponse _
instance showListContainerInstancesResponse :: Show ListContainerInstancesResponse where
  show = genericShow
instance decodeListContainerInstancesResponse :: Decode ListContainerInstancesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListContainerInstancesResponse :: Encode ListContainerInstancesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListServicesRequest = ListServicesRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "LaunchType'" :: NullOrUndefined.NullOrUndefined (LaunchType)
  }
derive instance newtypeListServicesRequest :: Newtype ListServicesRequest _
derive instance repGenericListServicesRequest :: Generic ListServicesRequest _
instance showListServicesRequest :: Show ListServicesRequest where
  show = genericShow
instance decodeListServicesRequest :: Decode ListServicesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListServicesRequest :: Encode ListServicesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListServicesResponse = ListServicesResponse 
  { "ServiceArns'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListServicesResponse :: Newtype ListServicesResponse _
derive instance repGenericListServicesResponse :: Generic ListServicesResponse _
instance showListServicesResponse :: Show ListServicesResponse where
  show = genericShow
instance decodeListServicesResponse :: Decode ListServicesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListServicesResponse :: Encode ListServicesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTaskDefinitionFamiliesRequest = ListTaskDefinitionFamiliesRequest 
  { "FamilyPrefix'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (TaskDefinitionFamilyStatus)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeListTaskDefinitionFamiliesRequest :: Newtype ListTaskDefinitionFamiliesRequest _
derive instance repGenericListTaskDefinitionFamiliesRequest :: Generic ListTaskDefinitionFamiliesRequest _
instance showListTaskDefinitionFamiliesRequest :: Show ListTaskDefinitionFamiliesRequest where
  show = genericShow
instance decodeListTaskDefinitionFamiliesRequest :: Decode ListTaskDefinitionFamiliesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTaskDefinitionFamiliesRequest :: Encode ListTaskDefinitionFamiliesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse 
  { "Families'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListTaskDefinitionFamiliesResponse :: Newtype ListTaskDefinitionFamiliesResponse _
derive instance repGenericListTaskDefinitionFamiliesResponse :: Generic ListTaskDefinitionFamiliesResponse _
instance showListTaskDefinitionFamiliesResponse :: Show ListTaskDefinitionFamiliesResponse where
  show = genericShow
instance decodeListTaskDefinitionFamiliesResponse :: Decode ListTaskDefinitionFamiliesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTaskDefinitionFamiliesResponse :: Encode ListTaskDefinitionFamiliesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTaskDefinitionsRequest = ListTaskDefinitionsRequest 
  { "FamilyPrefix'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (TaskDefinitionStatus)
  , "Sort'" :: NullOrUndefined.NullOrUndefined (SortOrder)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeListTaskDefinitionsRequest :: Newtype ListTaskDefinitionsRequest _
derive instance repGenericListTaskDefinitionsRequest :: Generic ListTaskDefinitionsRequest _
instance showListTaskDefinitionsRequest :: Show ListTaskDefinitionsRequest where
  show = genericShow
instance decodeListTaskDefinitionsRequest :: Decode ListTaskDefinitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTaskDefinitionsRequest :: Encode ListTaskDefinitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTaskDefinitionsResponse = ListTaskDefinitionsResponse 
  { "TaskDefinitionArns'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListTaskDefinitionsResponse :: Newtype ListTaskDefinitionsResponse _
derive instance repGenericListTaskDefinitionsResponse :: Generic ListTaskDefinitionsResponse _
instance showListTaskDefinitionsResponse :: Show ListTaskDefinitionsResponse where
  show = genericShow
instance decodeListTaskDefinitionsResponse :: Decode ListTaskDefinitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTaskDefinitionsResponse :: Encode ListTaskDefinitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTasksRequest = ListTasksRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerInstance'" :: NullOrUndefined.NullOrUndefined (String)
  , "Family'" :: NullOrUndefined.NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "StartedBy'" :: NullOrUndefined.NullOrUndefined (String)
  , "ServiceName'" :: NullOrUndefined.NullOrUndefined (String)
  , "DesiredStatus'" :: NullOrUndefined.NullOrUndefined (DesiredStatus)
  , "LaunchType'" :: NullOrUndefined.NullOrUndefined (LaunchType)
  }
derive instance newtypeListTasksRequest :: Newtype ListTasksRequest _
derive instance repGenericListTasksRequest :: Generic ListTasksRequest _
instance showListTasksRequest :: Show ListTasksRequest where
  show = genericShow
instance decodeListTasksRequest :: Decode ListTasksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTasksRequest :: Encode ListTasksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTasksResponse = ListTasksResponse 
  { "TaskArns'" :: NullOrUndefined.NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListTasksResponse :: Newtype ListTasksResponse _
derive instance repGenericListTasksResponse :: Generic ListTasksResponse _
instance showListTasksResponse :: Show ListTasksResponse where
  show = genericShow
instance decodeListTasksResponse :: Decode ListTasksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTasksResponse :: Encode ListTasksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details on a load balancer that is used with a service.</p>
newtype LoadBalancer = LoadBalancer 
  { "TargetGroupArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "LoadBalancerName'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerName'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerPort'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeLoadBalancer :: Newtype LoadBalancer _
derive instance repGenericLoadBalancer :: Generic LoadBalancer _
instance showLoadBalancer :: Show LoadBalancer where
  show = genericShow
instance decodeLoadBalancer :: Decode LoadBalancer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancer :: Encode LoadBalancer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancers = LoadBalancers (Array LoadBalancer)
derive instance newtypeLoadBalancers :: Newtype LoadBalancers _
derive instance repGenericLoadBalancers :: Generic LoadBalancers _
instance showLoadBalancers :: Show LoadBalancers where
  show = genericShow
instance decodeLoadBalancers :: Decode LoadBalancers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancers :: Encode LoadBalancers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Log configuration options to send to a custom log driver for the container.</p>
newtype LogConfiguration = LogConfiguration 
  { "LogDriver'" :: (LogDriver)
  , "Options'" :: NullOrUndefined.NullOrUndefined (LogConfigurationOptionsMap)
  }
derive instance newtypeLogConfiguration :: Newtype LogConfiguration _
derive instance repGenericLogConfiguration :: Generic LogConfiguration _
instance showLogConfiguration :: Show LogConfiguration where
  show = genericShow
instance decodeLogConfiguration :: Decode LogConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogConfiguration :: Encode LogConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogConfigurationOptionsMap = LogConfigurationOptionsMap (StrMap.StrMap String)
derive instance newtypeLogConfigurationOptionsMap :: Newtype LogConfigurationOptionsMap _
derive instance repGenericLogConfigurationOptionsMap :: Generic LogConfigurationOptionsMap _
instance showLogConfigurationOptionsMap :: Show LogConfigurationOptionsMap where
  show = genericShow
instance decodeLogConfigurationOptionsMap :: Decode LogConfigurationOptionsMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogConfigurationOptionsMap :: Encode LogConfigurationOptionsMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogDriver = LogDriver String
derive instance newtypeLogDriver :: Newtype LogDriver _
derive instance repGenericLogDriver :: Generic LogDriver _
instance showLogDriver :: Show LogDriver where
  show = genericShow
instance decodeLogDriver :: Decode LogDriver where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogDriver :: Encode LogDriver where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon ECS is unable to determine the current version of the Amazon ECS container agent on the container instance and does not have enough information to proceed with an update. This could be because the agent running on the container instance is an older or custom version that does not use our version information.</p>
newtype MissingVersionException = MissingVersionException Types.NoArguments
derive instance newtypeMissingVersionException :: Newtype MissingVersionException _
derive instance repGenericMissingVersionException :: Generic MissingVersionException _
instance showMissingVersionException :: Show MissingVersionException where
  show = genericShow
instance decodeMissingVersionException :: Decode MissingVersionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingVersionException :: Encode MissingVersionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details on a volume mount point that is used in a container definition.</p>
newtype MountPoint = MountPoint 
  { "SourceVolume'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerPath'" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadOnly'" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeMountPoint :: Newtype MountPoint _
derive instance repGenericMountPoint :: Generic MountPoint _
instance showMountPoint :: Show MountPoint where
  show = genericShow
instance decodeMountPoint :: Decode MountPoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountPoint :: Encode MountPoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MountPointList = MountPointList (Array MountPoint)
derive instance newtypeMountPointList :: Newtype MountPointList _
derive instance repGenericMountPointList :: Generic MountPointList _
instance showMountPointList :: Show MountPointList where
  show = genericShow
instance decodeMountPointList :: Decode MountPointList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMountPointList :: Encode MountPointList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details on the network bindings between a container and its host container instance. After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.</p>
newtype NetworkBinding = NetworkBinding 
  { "BindIP'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerPort'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "HostPort'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "Protocol'" :: NullOrUndefined.NullOrUndefined (TransportProtocol)
  }
derive instance newtypeNetworkBinding :: Newtype NetworkBinding _
derive instance repGenericNetworkBinding :: Generic NetworkBinding _
instance showNetworkBinding :: Show NetworkBinding where
  show = genericShow
instance decodeNetworkBinding :: Decode NetworkBinding where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkBinding :: Encode NetworkBinding where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NetworkBindings = NetworkBindings (Array NetworkBinding)
derive instance newtypeNetworkBindings :: Newtype NetworkBindings _
derive instance repGenericNetworkBindings :: Generic NetworkBindings _
instance showNetworkBindings :: Show NetworkBindings where
  show = genericShow
instance decodeNetworkBindings :: Decode NetworkBindings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkBindings :: Encode NetworkBindings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the network configuration for a task or service.</p>
newtype NetworkConfiguration = NetworkConfiguration 
  { "AwsvpcConfiguration'" :: NullOrUndefined.NullOrUndefined (AwsVpcConfiguration)
  }
derive instance newtypeNetworkConfiguration :: Newtype NetworkConfiguration _
derive instance repGenericNetworkConfiguration :: Generic NetworkConfiguration _
instance showNetworkConfiguration :: Show NetworkConfiguration where
  show = genericShow
instance decodeNetworkConfiguration :: Decode NetworkConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkConfiguration :: Encode NetworkConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the Elastic Network Interface for tasks that use the <code>awsvpc</code> network mode.</p>
newtype NetworkInterface = NetworkInterface 
  { "AttachmentId'" :: NullOrUndefined.NullOrUndefined (String)
  , "PrivateIpv4Address'" :: NullOrUndefined.NullOrUndefined (String)
  , "Ipv6Address'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeNetworkInterface :: Newtype NetworkInterface _
derive instance repGenericNetworkInterface :: Generic NetworkInterface _
instance showNetworkInterface :: Show NetworkInterface where
  show = genericShow
instance decodeNetworkInterface :: Decode NetworkInterface where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterface :: Encode NetworkInterface where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NetworkInterfaces = NetworkInterfaces (Array NetworkInterface)
derive instance newtypeNetworkInterfaces :: Newtype NetworkInterfaces _
derive instance repGenericNetworkInterfaces :: Generic NetworkInterfaces _
instance showNetworkInterfaces :: Show NetworkInterfaces where
  show = genericShow
instance decodeNetworkInterfaces :: Decode NetworkInterfaces where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterfaces :: Encode NetworkInterfaces where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NetworkMode = NetworkMode String
derive instance newtypeNetworkMode :: Newtype NetworkMode _
derive instance repGenericNetworkMode :: Generic NetworkMode _
instance showNetworkMode :: Show NetworkMode where
  show = genericShow
instance decodeNetworkMode :: Decode NetworkMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkMode :: Encode NetworkMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no update available for this Amazon ECS container agent. This could be because the agent is already running the latest version, or it is so old that there is no update path to the current version.</p>
newtype NoUpdateAvailableException = NoUpdateAvailableException Types.NoArguments
derive instance newtypeNoUpdateAvailableException :: Newtype NoUpdateAvailableException _
derive instance repGenericNoUpdateAvailableException :: Generic NoUpdateAvailableException _
instance showNoUpdateAvailableException :: Show NoUpdateAvailableException where
  show = genericShow
instance decodeNoUpdateAvailableException :: Decode NoUpdateAvailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoUpdateAvailableException :: Encode NoUpdateAvailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing a constraint on task placement. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
newtype PlacementConstraint = PlacementConstraint 
  { "Type'" :: NullOrUndefined.NullOrUndefined (PlacementConstraintType)
  , "Expression'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePlacementConstraint :: Newtype PlacementConstraint _
derive instance repGenericPlacementConstraint :: Generic PlacementConstraint _
instance showPlacementConstraint :: Show PlacementConstraint where
  show = genericShow
instance decodePlacementConstraint :: Decode PlacementConstraint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacementConstraint :: Encode PlacementConstraint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlacementConstraintType = PlacementConstraintType String
derive instance newtypePlacementConstraintType :: Newtype PlacementConstraintType _
derive instance repGenericPlacementConstraintType :: Generic PlacementConstraintType _
instance showPlacementConstraintType :: Show PlacementConstraintType where
  show = genericShow
instance decodePlacementConstraintType :: Decode PlacementConstraintType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacementConstraintType :: Encode PlacementConstraintType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlacementConstraints = PlacementConstraints (Array PlacementConstraint)
derive instance newtypePlacementConstraints :: Newtype PlacementConstraints _
derive instance repGenericPlacementConstraints :: Generic PlacementConstraints _
instance showPlacementConstraints :: Show PlacementConstraints where
  show = genericShow
instance decodePlacementConstraints :: Decode PlacementConstraints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacementConstraints :: Encode PlacementConstraints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlacementStrategies = PlacementStrategies (Array PlacementStrategy)
derive instance newtypePlacementStrategies :: Newtype PlacementStrategies _
derive instance repGenericPlacementStrategies :: Generic PlacementStrategies _
instance showPlacementStrategies :: Show PlacementStrategies where
  show = genericShow
instance decodePlacementStrategies :: Decode PlacementStrategies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacementStrategies :: Encode PlacementStrategies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The task placement strategy for a task or service. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html">Task Placement Strategies</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
newtype PlacementStrategy = PlacementStrategy 
  { "Type'" :: NullOrUndefined.NullOrUndefined (PlacementStrategyType)
  , "Field'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePlacementStrategy :: Newtype PlacementStrategy _
derive instance repGenericPlacementStrategy :: Generic PlacementStrategy _
instance showPlacementStrategy :: Show PlacementStrategy where
  show = genericShow
instance decodePlacementStrategy :: Decode PlacementStrategy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacementStrategy :: Encode PlacementStrategy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlacementStrategyType = PlacementStrategyType String
derive instance newtypePlacementStrategyType :: Newtype PlacementStrategyType _
derive instance repGenericPlacementStrategyType :: Generic PlacementStrategyType _
instance showPlacementStrategyType :: Show PlacementStrategyType where
  show = genericShow
instance decodePlacementStrategyType :: Decode PlacementStrategyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlacementStrategyType :: Encode PlacementStrategyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified platform version does not satisfy the task definition’s required capabilities.</p>
newtype PlatformTaskDefinitionIncompatibilityException = PlatformTaskDefinitionIncompatibilityException Types.NoArguments
derive instance newtypePlatformTaskDefinitionIncompatibilityException :: Newtype PlatformTaskDefinitionIncompatibilityException _
derive instance repGenericPlatformTaskDefinitionIncompatibilityException :: Generic PlatformTaskDefinitionIncompatibilityException _
instance showPlatformTaskDefinitionIncompatibilityException :: Show PlatformTaskDefinitionIncompatibilityException where
  show = genericShow
instance decodePlatformTaskDefinitionIncompatibilityException :: Decode PlatformTaskDefinitionIncompatibilityException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformTaskDefinitionIncompatibilityException :: Encode PlatformTaskDefinitionIncompatibilityException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified platform version does not exist.</p>
newtype PlatformUnknownException = PlatformUnknownException Types.NoArguments
derive instance newtypePlatformUnknownException :: Newtype PlatformUnknownException _
derive instance repGenericPlatformUnknownException :: Generic PlatformUnknownException _
instance showPlatformUnknownException :: Show PlatformUnknownException where
  show = genericShow
instance decodePlatformUnknownException :: Decode PlatformUnknownException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformUnknownException :: Encode PlatformUnknownException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition.</p> <p>If using containers in a task with the <code>awsvpc</code> or <code>host</code> network mode, exposed ports should be specified using <code>containerPort</code>. The <code>hostPort</code> can be left blank or it must be the same value as the <code>containerPort</code>.</p> <p>After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.</p>
newtype PortMapping = PortMapping 
  { "ContainerPort'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "HostPort'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "Protocol'" :: NullOrUndefined.NullOrUndefined (TransportProtocol)
  }
derive instance newtypePortMapping :: Newtype PortMapping _
derive instance repGenericPortMapping :: Generic PortMapping _
instance showPortMapping :: Show PortMapping where
  show = genericShow
instance decodePortMapping :: Decode PortMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortMapping :: Encode PortMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PortMappingList = PortMappingList (Array PortMapping)
derive instance newtypePortMappingList :: Newtype PortMappingList _
derive instance repGenericPortMappingList :: Generic PortMappingList _
instance showPortMappingList :: Show PortMappingList where
  show = genericShow
instance decodePortMappingList :: Decode PortMappingList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortMappingList :: Encode PortMappingList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutAttributesRequest = PutAttributesRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes'" :: (Attributes)
  }
derive instance newtypePutAttributesRequest :: Newtype PutAttributesRequest _
derive instance repGenericPutAttributesRequest :: Generic PutAttributesRequest _
instance showPutAttributesRequest :: Show PutAttributesRequest where
  show = genericShow
instance decodePutAttributesRequest :: Decode PutAttributesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutAttributesRequest :: Encode PutAttributesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutAttributesResponse = PutAttributesResponse 
  { "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypePutAttributesResponse :: Newtype PutAttributesResponse _
derive instance repGenericPutAttributesResponse :: Generic PutAttributesResponse _
instance showPutAttributesResponse :: Show PutAttributesResponse where
  show = genericShow
instance decodePutAttributesResponse :: Decode PutAttributesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutAttributesResponse :: Encode PutAttributesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterContainerInstanceRequest = RegisterContainerInstanceRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceIdentityDocument'" :: NullOrUndefined.NullOrUndefined (String)
  , "InstanceIdentityDocumentSignature'" :: NullOrUndefined.NullOrUndefined (String)
  , "TotalResources'" :: NullOrUndefined.NullOrUndefined (Resources)
  , "VersionInfo'" :: NullOrUndefined.NullOrUndefined (VersionInfo)
  , "ContainerInstanceArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypeRegisterContainerInstanceRequest :: Newtype RegisterContainerInstanceRequest _
derive instance repGenericRegisterContainerInstanceRequest :: Generic RegisterContainerInstanceRequest _
instance showRegisterContainerInstanceRequest :: Show RegisterContainerInstanceRequest where
  show = genericShow
instance decodeRegisterContainerInstanceRequest :: Decode RegisterContainerInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterContainerInstanceRequest :: Encode RegisterContainerInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterContainerInstanceResponse = RegisterContainerInstanceResponse 
  { "ContainerInstance'" :: NullOrUndefined.NullOrUndefined (ContainerInstance)
  }
derive instance newtypeRegisterContainerInstanceResponse :: Newtype RegisterContainerInstanceResponse _
derive instance repGenericRegisterContainerInstanceResponse :: Generic RegisterContainerInstanceResponse _
instance showRegisterContainerInstanceResponse :: Show RegisterContainerInstanceResponse where
  show = genericShow
instance decodeRegisterContainerInstanceResponse :: Decode RegisterContainerInstanceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterContainerInstanceResponse :: Encode RegisterContainerInstanceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterTaskDefinitionRequest = RegisterTaskDefinitionRequest 
  { "Family'" :: (String)
  , "TaskRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExecutionRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "NetworkMode'" :: NullOrUndefined.NullOrUndefined (NetworkMode)
  , "ContainerDefinitions'" :: (ContainerDefinitions)
  , "Volumes'" :: NullOrUndefined.NullOrUndefined (VolumeList)
  , "PlacementConstraints'" :: NullOrUndefined.NullOrUndefined (TaskDefinitionPlacementConstraints)
  , "RequiresCompatibilities'" :: NullOrUndefined.NullOrUndefined (CompatibilityList)
  , "Cpu'" :: NullOrUndefined.NullOrUndefined (String)
  , "Memory'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeRegisterTaskDefinitionRequest :: Newtype RegisterTaskDefinitionRequest _
derive instance repGenericRegisterTaskDefinitionRequest :: Generic RegisterTaskDefinitionRequest _
instance showRegisterTaskDefinitionRequest :: Show RegisterTaskDefinitionRequest where
  show = genericShow
instance decodeRegisterTaskDefinitionRequest :: Decode RegisterTaskDefinitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterTaskDefinitionRequest :: Encode RegisterTaskDefinitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse 
  { "TaskDefinition'" :: NullOrUndefined.NullOrUndefined (TaskDefinition)
  }
derive instance newtypeRegisterTaskDefinitionResponse :: Newtype RegisterTaskDefinitionResponse _
derive instance repGenericRegisterTaskDefinitionResponse :: Generic RegisterTaskDefinitionResponse _
instance showRegisterTaskDefinitionResponse :: Show RegisterTaskDefinitionResponse where
  show = genericShow
instance decodeRegisterTaskDefinitionResponse :: Decode RegisterTaskDefinitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterTaskDefinitionResponse :: Encode RegisterTaskDefinitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequiresAttributes = RequiresAttributes (Array Attribute)
derive instance newtypeRequiresAttributes :: Newtype RequiresAttributes _
derive instance repGenericRequiresAttributes :: Generic RequiresAttributes _
instance showRequiresAttributes :: Show RequiresAttributes where
  show = genericShow
instance decodeRequiresAttributes :: Decode RequiresAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequiresAttributes :: Encode RequiresAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the resources available for a container instance.</p>
newtype Resource = Resource 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (String)
  , "DoubleValue'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LongValue'" :: NullOrUndefined.NullOrUndefined (Number)
  , "IntegerValue'" :: NullOrUndefined.NullOrUndefined (Int)
  , "StringSetValue'" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeResource :: Newtype Resource _
derive instance repGenericResource :: Generic Resource _
instance showResource :: Show Resource where
  show = genericShow
instance decodeResource :: Decode Resource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResource :: Encode Resource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Resources = Resources (Array Resource)
derive instance newtypeResources :: Newtype Resources _
derive instance repGenericResources :: Generic Resources _
instance showResources :: Show Resources where
  show = genericShow
instance decodeResources :: Decode Resources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResources :: Encode Resources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RunTaskRequest = RunTaskRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskDefinition'" :: (String)
  , "Overrides'" :: NullOrUndefined.NullOrUndefined (TaskOverride)
  , "Count'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "StartedBy'" :: NullOrUndefined.NullOrUndefined (String)
  , "Group'" :: NullOrUndefined.NullOrUndefined (String)
  , "PlacementConstraints'" :: NullOrUndefined.NullOrUndefined (PlacementConstraints)
  , "PlacementStrategy'" :: NullOrUndefined.NullOrUndefined (PlacementStrategies)
  , "LaunchType'" :: NullOrUndefined.NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "NetworkConfiguration'" :: NullOrUndefined.NullOrUndefined (NetworkConfiguration)
  }
derive instance newtypeRunTaskRequest :: Newtype RunTaskRequest _
derive instance repGenericRunTaskRequest :: Generic RunTaskRequest _
instance showRunTaskRequest :: Show RunTaskRequest where
  show = genericShow
instance decodeRunTaskRequest :: Decode RunTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunTaskRequest :: Encode RunTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RunTaskResponse = RunTaskResponse 
  { "Tasks'" :: NullOrUndefined.NullOrUndefined (Tasks)
  , "Failures'" :: NullOrUndefined.NullOrUndefined (Failures)
  }
derive instance newtypeRunTaskResponse :: Newtype RunTaskResponse _
derive instance repGenericRunTaskResponse :: Generic RunTaskResponse _
instance showRunTaskResponse :: Show RunTaskResponse where
  show = genericShow
instance decodeRunTaskResponse :: Decode RunTaskResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunTaskResponse :: Encode RunTaskResponse where
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


-- | <p>Details on a service within a cluster</p>
newtype Service = Service 
  { "ServiceArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ServiceName'" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "LoadBalancers'" :: NullOrUndefined.NullOrUndefined (LoadBalancers)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "DesiredCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "RunningCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "PendingCount'" :: NullOrUndefined.NullOrUndefined (Int)
  , "LaunchType'" :: NullOrUndefined.NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskDefinition'" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentConfiguration'" :: NullOrUndefined.NullOrUndefined (DeploymentConfiguration)
  , "Deployments'" :: NullOrUndefined.NullOrUndefined (Deployments)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Events'" :: NullOrUndefined.NullOrUndefined (ServiceEvents)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "PlacementConstraints'" :: NullOrUndefined.NullOrUndefined (PlacementConstraints)
  , "PlacementStrategy'" :: NullOrUndefined.NullOrUndefined (PlacementStrategies)
  , "NetworkConfiguration'" :: NullOrUndefined.NullOrUndefined (NetworkConfiguration)
  , "HealthCheckGracePeriodSeconds'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeService :: Newtype Service _
derive instance repGenericService :: Generic Service _
instance showService :: Show Service where
  show = genericShow
instance decodeService :: Decode Service where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeService :: Encode Service where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details on an event associated with a service.</p>
newtype ServiceEvent = ServiceEvent 
  { "Id'" :: NullOrUndefined.NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeServiceEvent :: Newtype ServiceEvent _
derive instance repGenericServiceEvent :: Generic ServiceEvent _
instance showServiceEvent :: Show ServiceEvent where
  show = genericShow
instance decodeServiceEvent :: Decode ServiceEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceEvent :: Encode ServiceEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServiceEvents = ServiceEvents (Array ServiceEvent)
derive instance newtypeServiceEvents :: Newtype ServiceEvents _
derive instance repGenericServiceEvents :: Generic ServiceEvents _
instance showServiceEvents :: Show ServiceEvents where
  show = genericShow
instance decodeServiceEvents :: Decode ServiceEvents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceEvents :: Encode ServiceEvents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified service is not active. You can't update a service that is inactive. If you have previously deleted a service, you can re-create it with <a>CreateService</a>.</p>
newtype ServiceNotActiveException = ServiceNotActiveException Types.NoArguments
derive instance newtypeServiceNotActiveException :: Newtype ServiceNotActiveException _
derive instance repGenericServiceNotActiveException :: Generic ServiceNotActiveException _
instance showServiceNotActiveException :: Show ServiceNotActiveException where
  show = genericShow
instance decodeServiceNotActiveException :: Decode ServiceNotActiveException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceNotActiveException :: Encode ServiceNotActiveException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified service could not be found. You can view your available services with <a>ListServices</a>. Amazon ECS services are cluster-specific and region-specific.</p>
newtype ServiceNotFoundException = ServiceNotFoundException Types.NoArguments
derive instance newtypeServiceNotFoundException :: Newtype ServiceNotFoundException _
derive instance repGenericServiceNotFoundException :: Generic ServiceNotFoundException _
instance showServiceNotFoundException :: Show ServiceNotFoundException where
  show = genericShow
instance decodeServiceNotFoundException :: Decode ServiceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceNotFoundException :: Encode ServiceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Services = Services (Array Service)
derive instance newtypeServices :: Newtype Services _
derive instance repGenericServices :: Generic Services _
instance showServices :: Show Services where
  show = genericShow
instance decodeServices :: Decode Services where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServices :: Encode Services where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SortOrder = SortOrder String
derive instance newtypeSortOrder :: Newtype SortOrder _
derive instance repGenericSortOrder :: Generic SortOrder _
instance showSortOrder :: Show SortOrder where
  show = genericShow
instance decodeSortOrder :: Decode SortOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSortOrder :: Encode SortOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartTaskRequest = StartTaskRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskDefinition'" :: (String)
  , "Overrides'" :: NullOrUndefined.NullOrUndefined (TaskOverride)
  , "ContainerInstances'" :: (StringList)
  , "StartedBy'" :: NullOrUndefined.NullOrUndefined (String)
  , "Group'" :: NullOrUndefined.NullOrUndefined (String)
  , "NetworkConfiguration'" :: NullOrUndefined.NullOrUndefined (NetworkConfiguration)
  }
derive instance newtypeStartTaskRequest :: Newtype StartTaskRequest _
derive instance repGenericStartTaskRequest :: Generic StartTaskRequest _
instance showStartTaskRequest :: Show StartTaskRequest where
  show = genericShow
instance decodeStartTaskRequest :: Decode StartTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartTaskRequest :: Encode StartTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartTaskResponse = StartTaskResponse 
  { "Tasks'" :: NullOrUndefined.NullOrUndefined (Tasks)
  , "Failures'" :: NullOrUndefined.NullOrUndefined (Failures)
  }
derive instance newtypeStartTaskResponse :: Newtype StartTaskResponse _
derive instance repGenericStartTaskResponse :: Generic StartTaskResponse _
instance showStartTaskResponse :: Show StartTaskResponse where
  show = genericShow
instance decodeStartTaskResponse :: Decode StartTaskResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartTaskResponse :: Encode StartTaskResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Statistics = Statistics (Array KeyValuePair)
derive instance newtypeStatistics :: Newtype Statistics _
derive instance repGenericStatistics :: Generic Statistics _
instance showStatistics :: Show Statistics where
  show = genericShow
instance decodeStatistics :: Decode Statistics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatistics :: Encode Statistics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopTaskRequest = StopTaskRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Task'" :: (String)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeStopTaskRequest :: Newtype StopTaskRequest _
derive instance repGenericStopTaskRequest :: Generic StopTaskRequest _
instance showStopTaskRequest :: Show StopTaskRequest where
  show = genericShow
instance decodeStopTaskRequest :: Decode StopTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopTaskRequest :: Encode StopTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopTaskResponse = StopTaskResponse 
  { "Task'" :: NullOrUndefined.NullOrUndefined (Task)
  }
derive instance newtypeStopTaskResponse :: Newtype StopTaskResponse _
derive instance repGenericStopTaskResponse :: Generic StopTaskResponse _
instance showStopTaskResponse :: Show StopTaskResponse where
  show = genericShow
instance decodeStopTaskResponse :: Decode StopTaskResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopTaskResponse :: Encode StopTaskResponse where
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


newtype SubmitContainerStateChangeRequest = SubmitContainerStateChangeRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Task'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerName'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  , "NetworkBindings'" :: NullOrUndefined.NullOrUndefined (NetworkBindings)
  }
derive instance newtypeSubmitContainerStateChangeRequest :: Newtype SubmitContainerStateChangeRequest _
derive instance repGenericSubmitContainerStateChangeRequest :: Generic SubmitContainerStateChangeRequest _
instance showSubmitContainerStateChangeRequest :: Show SubmitContainerStateChangeRequest where
  show = genericShow
instance decodeSubmitContainerStateChangeRequest :: Decode SubmitContainerStateChangeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubmitContainerStateChangeRequest :: Encode SubmitContainerStateChangeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse 
  { "Acknowledgment'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSubmitContainerStateChangeResponse :: Newtype SubmitContainerStateChangeResponse _
derive instance repGenericSubmitContainerStateChangeResponse :: Generic SubmitContainerStateChangeResponse _
instance showSubmitContainerStateChangeResponse :: Show SubmitContainerStateChangeResponse where
  show = genericShow
instance decodeSubmitContainerStateChangeResponse :: Decode SubmitContainerStateChangeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubmitContainerStateChangeResponse :: Encode SubmitContainerStateChangeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubmitTaskStateChangeRequest = SubmitTaskStateChangeRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Task'" :: NullOrUndefined.NullOrUndefined (String)
  , "Status'" :: NullOrUndefined.NullOrUndefined (String)
  , "Reason'" :: NullOrUndefined.NullOrUndefined (String)
  , "Containers'" :: NullOrUndefined.NullOrUndefined (ContainerStateChanges)
  , "Attachments'" :: NullOrUndefined.NullOrUndefined (AttachmentStateChanges)
  , "PullStartedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "PullStoppedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ExecutionStoppedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeSubmitTaskStateChangeRequest :: Newtype SubmitTaskStateChangeRequest _
derive instance repGenericSubmitTaskStateChangeRequest :: Generic SubmitTaskStateChangeRequest _
instance showSubmitTaskStateChangeRequest :: Show SubmitTaskStateChangeRequest where
  show = genericShow
instance decodeSubmitTaskStateChangeRequest :: Decode SubmitTaskStateChangeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubmitTaskStateChangeRequest :: Encode SubmitTaskStateChangeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse 
  { "Acknowledgment'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSubmitTaskStateChangeResponse :: Newtype SubmitTaskStateChangeResponse _
derive instance repGenericSubmitTaskStateChangeResponse :: Generic SubmitTaskStateChangeResponse _
instance showSubmitTaskStateChangeResponse :: Show SubmitTaskStateChangeResponse where
  show = genericShow
instance decodeSubmitTaskStateChangeResponse :: Decode SubmitTaskStateChangeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubmitTaskStateChangeResponse :: Encode SubmitTaskStateChangeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified target could not be found. You can view your available container instances with <a>ListContainerInstances</a>. Amazon ECS container instances are cluster-specific and region-specific.</p>
newtype TargetNotFoundException = TargetNotFoundException Types.NoArguments
derive instance newtypeTargetNotFoundException :: Newtype TargetNotFoundException _
derive instance repGenericTargetNotFoundException :: Generic TargetNotFoundException _
instance showTargetNotFoundException :: Show TargetNotFoundException where
  show = genericShow
instance decodeTargetNotFoundException :: Decode TargetNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetNotFoundException :: Encode TargetNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetType = TargetType String
derive instance newtypeTargetType :: Newtype TargetType _
derive instance repGenericTargetType :: Generic TargetType _
instance showTargetType :: Show TargetType where
  show = genericShow
instance decodeTargetType :: Decode TargetType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetType :: Encode TargetType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details on a task in a cluster.</p>
newtype Task = Task 
  { "TaskArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ClusterArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskDefinitionArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerInstanceArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Overrides'" :: NullOrUndefined.NullOrUndefined (TaskOverride)
  , "LastStatus'" :: NullOrUndefined.NullOrUndefined (String)
  , "DesiredStatus'" :: NullOrUndefined.NullOrUndefined (String)
  , "Cpu'" :: NullOrUndefined.NullOrUndefined (String)
  , "Memory'" :: NullOrUndefined.NullOrUndefined (String)
  , "Containers'" :: NullOrUndefined.NullOrUndefined (Containers)
  , "StartedBy'" :: NullOrUndefined.NullOrUndefined (String)
  , "Version'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StoppedReason'" :: NullOrUndefined.NullOrUndefined (String)
  , "Connectivity'" :: NullOrUndefined.NullOrUndefined (Connectivity)
  , "ConnectivityAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "PullStartedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "PullStoppedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ExecutionStoppedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CreatedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StartedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StoppingAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "StoppedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Group'" :: NullOrUndefined.NullOrUndefined (String)
  , "LaunchType'" :: NullOrUndefined.NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "Attachments'" :: NullOrUndefined.NullOrUndefined (Attachments)
  }
derive instance newtypeTask :: Newtype Task _
derive instance repGenericTask :: Generic Task _
instance showTask :: Show Task where
  show = genericShow
instance decodeTask :: Decode Task where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTask :: Encode Task where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details of a task definition.</p>
newtype TaskDefinition = TaskDefinition 
  { "TaskDefinitionArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerDefinitions'" :: NullOrUndefined.NullOrUndefined (ContainerDefinitions)
  , "Family'" :: NullOrUndefined.NullOrUndefined (String)
  , "TaskRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExecutionRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "NetworkMode'" :: NullOrUndefined.NullOrUndefined (NetworkMode)
  , "Revision'" :: NullOrUndefined.NullOrUndefined (Int)
  , "Volumes'" :: NullOrUndefined.NullOrUndefined (VolumeList)
  , "Status'" :: NullOrUndefined.NullOrUndefined (TaskDefinitionStatus)
  , "RequiresAttributes'" :: NullOrUndefined.NullOrUndefined (RequiresAttributes)
  , "PlacementConstraints'" :: NullOrUndefined.NullOrUndefined (TaskDefinitionPlacementConstraints)
  , "Compatibilities'" :: NullOrUndefined.NullOrUndefined (CompatibilityList)
  , "RequiresCompatibilities'" :: NullOrUndefined.NullOrUndefined (CompatibilityList)
  , "Cpu'" :: NullOrUndefined.NullOrUndefined (String)
  , "Memory'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTaskDefinition :: Newtype TaskDefinition _
derive instance repGenericTaskDefinition :: Generic TaskDefinition _
instance showTaskDefinition :: Show TaskDefinition where
  show = genericShow
instance decodeTaskDefinition :: Decode TaskDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskDefinition :: Encode TaskDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskDefinitionFamilyStatus = TaskDefinitionFamilyStatus String
derive instance newtypeTaskDefinitionFamilyStatus :: Newtype TaskDefinitionFamilyStatus _
derive instance repGenericTaskDefinitionFamilyStatus :: Generic TaskDefinitionFamilyStatus _
instance showTaskDefinitionFamilyStatus :: Show TaskDefinitionFamilyStatus where
  show = genericShow
instance decodeTaskDefinitionFamilyStatus :: Decode TaskDefinitionFamilyStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskDefinitionFamilyStatus :: Encode TaskDefinitionFamilyStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing a constraint on task placement in the task definition.</p> <p>If you are using the Fargate launch type, task placement contraints are not supported.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
newtype TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint 
  { "Type'" :: NullOrUndefined.NullOrUndefined (TaskDefinitionPlacementConstraintType)
  , "Expression'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTaskDefinitionPlacementConstraint :: Newtype TaskDefinitionPlacementConstraint _
derive instance repGenericTaskDefinitionPlacementConstraint :: Generic TaskDefinitionPlacementConstraint _
instance showTaskDefinitionPlacementConstraint :: Show TaskDefinitionPlacementConstraint where
  show = genericShow
instance decodeTaskDefinitionPlacementConstraint :: Decode TaskDefinitionPlacementConstraint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskDefinitionPlacementConstraint :: Encode TaskDefinitionPlacementConstraint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskDefinitionPlacementConstraintType = TaskDefinitionPlacementConstraintType String
derive instance newtypeTaskDefinitionPlacementConstraintType :: Newtype TaskDefinitionPlacementConstraintType _
derive instance repGenericTaskDefinitionPlacementConstraintType :: Generic TaskDefinitionPlacementConstraintType _
instance showTaskDefinitionPlacementConstraintType :: Show TaskDefinitionPlacementConstraintType where
  show = genericShow
instance decodeTaskDefinitionPlacementConstraintType :: Decode TaskDefinitionPlacementConstraintType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskDefinitionPlacementConstraintType :: Encode TaskDefinitionPlacementConstraintType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskDefinitionPlacementConstraints = TaskDefinitionPlacementConstraints (Array TaskDefinitionPlacementConstraint)
derive instance newtypeTaskDefinitionPlacementConstraints :: Newtype TaskDefinitionPlacementConstraints _
derive instance repGenericTaskDefinitionPlacementConstraints :: Generic TaskDefinitionPlacementConstraints _
instance showTaskDefinitionPlacementConstraints :: Show TaskDefinitionPlacementConstraints where
  show = genericShow
instance decodeTaskDefinitionPlacementConstraints :: Decode TaskDefinitionPlacementConstraints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskDefinitionPlacementConstraints :: Encode TaskDefinitionPlacementConstraints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskDefinitionStatus = TaskDefinitionStatus String
derive instance newtypeTaskDefinitionStatus :: Newtype TaskDefinitionStatus _
derive instance repGenericTaskDefinitionStatus :: Generic TaskDefinitionStatus _
instance showTaskDefinitionStatus :: Show TaskDefinitionStatus where
  show = genericShow
instance decodeTaskDefinitionStatus :: Decode TaskDefinitionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskDefinitionStatus :: Encode TaskDefinitionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The overrides associated with a task.</p>
newtype TaskOverride = TaskOverride 
  { "ContainerOverrides'" :: NullOrUndefined.NullOrUndefined (ContainerOverrides)
  , "TaskRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  , "ExecutionRoleArn'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeTaskOverride :: Newtype TaskOverride _
derive instance repGenericTaskOverride :: Generic TaskOverride _
instance showTaskOverride :: Show TaskOverride where
  show = genericShow
instance decodeTaskOverride :: Decode TaskOverride where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskOverride :: Encode TaskOverride where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tasks = Tasks (Array Task)
derive instance newtypeTasks :: Newtype Tasks _
derive instance repGenericTasks :: Generic Tasks _
instance showTasks :: Show Tasks where
  show = genericShow
instance decodeTasks :: Decode Tasks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTasks :: Encode Tasks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransportProtocol = TransportProtocol String
derive instance newtypeTransportProtocol :: Newtype TransportProtocol _
derive instance repGenericTransportProtocol :: Generic TransportProtocol _
instance showTransportProtocol :: Show TransportProtocol where
  show = genericShow
instance decodeTransportProtocol :: Decode TransportProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransportProtocol :: Encode TransportProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>ulimit</code> settings to pass to the container.</p>
newtype Ulimit = Ulimit 
  { "Name'" :: (UlimitName)
  , "SoftLimit'" :: (Int)
  , "HardLimit'" :: (Int)
  }
derive instance newtypeUlimit :: Newtype Ulimit _
derive instance repGenericUlimit :: Generic Ulimit _
instance showUlimit :: Show Ulimit where
  show = genericShow
instance decodeUlimit :: Decode Ulimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUlimit :: Encode Ulimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UlimitList = UlimitList (Array Ulimit)
derive instance newtypeUlimitList :: Newtype UlimitList _
derive instance repGenericUlimitList :: Generic UlimitList _
instance showUlimitList :: Show UlimitList where
  show = genericShow
instance decodeUlimitList :: Decode UlimitList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUlimitList :: Encode UlimitList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UlimitName = UlimitName String
derive instance newtypeUlimitName :: Newtype UlimitName _
derive instance repGenericUlimitName :: Generic UlimitName _
instance showUlimitName :: Show UlimitName where
  show = genericShow
instance decodeUlimitName :: Decode UlimitName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUlimitName :: Encode UlimitName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified task is not supported in this region.</p>
newtype UnsupportedFeatureException = UnsupportedFeatureException Types.NoArguments
derive instance newtypeUnsupportedFeatureException :: Newtype UnsupportedFeatureException _
derive instance repGenericUnsupportedFeatureException :: Generic UnsupportedFeatureException _
instance showUnsupportedFeatureException :: Show UnsupportedFeatureException where
  show = genericShow
instance decodeUnsupportedFeatureException :: Decode UnsupportedFeatureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedFeatureException :: Encode UnsupportedFeatureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateContainerAgentRequest = UpdateContainerAgentRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerInstance'" :: (String)
  }
derive instance newtypeUpdateContainerAgentRequest :: Newtype UpdateContainerAgentRequest _
derive instance repGenericUpdateContainerAgentRequest :: Generic UpdateContainerAgentRequest _
instance showUpdateContainerAgentRequest :: Show UpdateContainerAgentRequest where
  show = genericShow
instance decodeUpdateContainerAgentRequest :: Decode UpdateContainerAgentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateContainerAgentRequest :: Encode UpdateContainerAgentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateContainerAgentResponse = UpdateContainerAgentResponse 
  { "ContainerInstance'" :: NullOrUndefined.NullOrUndefined (ContainerInstance)
  }
derive instance newtypeUpdateContainerAgentResponse :: Newtype UpdateContainerAgentResponse _
derive instance repGenericUpdateContainerAgentResponse :: Generic UpdateContainerAgentResponse _
instance showUpdateContainerAgentResponse :: Show UpdateContainerAgentResponse where
  show = genericShow
instance decodeUpdateContainerAgentResponse :: Decode UpdateContainerAgentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateContainerAgentResponse :: Encode UpdateContainerAgentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateContainerInstancesStateRequest = UpdateContainerInstancesStateRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "ContainerInstances'" :: (StringList)
  , "Status'" :: (ContainerInstanceStatus)
  }
derive instance newtypeUpdateContainerInstancesStateRequest :: Newtype UpdateContainerInstancesStateRequest _
derive instance repGenericUpdateContainerInstancesStateRequest :: Generic UpdateContainerInstancesStateRequest _
instance showUpdateContainerInstancesStateRequest :: Show UpdateContainerInstancesStateRequest where
  show = genericShow
instance decodeUpdateContainerInstancesStateRequest :: Decode UpdateContainerInstancesStateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateContainerInstancesStateRequest :: Encode UpdateContainerInstancesStateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateContainerInstancesStateResponse = UpdateContainerInstancesStateResponse 
  { "ContainerInstances'" :: NullOrUndefined.NullOrUndefined (ContainerInstances)
  , "Failures'" :: NullOrUndefined.NullOrUndefined (Failures)
  }
derive instance newtypeUpdateContainerInstancesStateResponse :: Newtype UpdateContainerInstancesStateResponse _
derive instance repGenericUpdateContainerInstancesStateResponse :: Generic UpdateContainerInstancesStateResponse _
instance showUpdateContainerInstancesStateResponse :: Show UpdateContainerInstancesStateResponse where
  show = genericShow
instance decodeUpdateContainerInstancesStateResponse :: Decode UpdateContainerInstancesStateResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateContainerInstancesStateResponse :: Encode UpdateContainerInstancesStateResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is already a current Amazon ECS container agent update in progress on the specified container instance. If the container agent becomes disconnected while it is in a transitional stage, such as <code>PENDING</code> or <code>STAGING</code>, the update process can get stuck in that state. However, when the agent reconnects, it resumes where it stopped previously.</p>
newtype UpdateInProgressException = UpdateInProgressException Types.NoArguments
derive instance newtypeUpdateInProgressException :: Newtype UpdateInProgressException _
derive instance repGenericUpdateInProgressException :: Generic UpdateInProgressException _
instance showUpdateInProgressException :: Show UpdateInProgressException where
  show = genericShow
instance decodeUpdateInProgressException :: Decode UpdateInProgressException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateInProgressException :: Encode UpdateInProgressException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateServiceRequest = UpdateServiceRequest 
  { "Cluster'" :: NullOrUndefined.NullOrUndefined (String)
  , "Service'" :: (String)
  , "DesiredCount'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "TaskDefinition'" :: NullOrUndefined.NullOrUndefined (String)
  , "DeploymentConfiguration'" :: NullOrUndefined.NullOrUndefined (DeploymentConfiguration)
  , "NetworkConfiguration'" :: NullOrUndefined.NullOrUndefined (NetworkConfiguration)
  , "PlatformVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "ForceNewDeployment'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "HealthCheckGracePeriodSeconds'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  }
derive instance newtypeUpdateServiceRequest :: Newtype UpdateServiceRequest _
derive instance repGenericUpdateServiceRequest :: Generic UpdateServiceRequest _
instance showUpdateServiceRequest :: Show UpdateServiceRequest where
  show = genericShow
instance decodeUpdateServiceRequest :: Decode UpdateServiceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateServiceRequest :: Encode UpdateServiceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateServiceResponse = UpdateServiceResponse 
  { "Service'" :: NullOrUndefined.NullOrUndefined (Service)
  }
derive instance newtypeUpdateServiceResponse :: Newtype UpdateServiceResponse _
derive instance repGenericUpdateServiceResponse :: Generic UpdateServiceResponse _
instance showUpdateServiceResponse :: Show UpdateServiceResponse where
  show = genericShow
instance decodeUpdateServiceResponse :: Decode UpdateServiceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateServiceResponse :: Encode UpdateServiceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Docker and Amazon ECS container agent version information about a container instance.</p>
newtype VersionInfo = VersionInfo 
  { "AgentVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "AgentHash'" :: NullOrUndefined.NullOrUndefined (String)
  , "DockerVersion'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeVersionInfo :: Newtype VersionInfo _
derive instance repGenericVersionInfo :: Generic VersionInfo _
instance showVersionInfo :: Show VersionInfo where
  show = genericShow
instance decodeVersionInfo :: Decode VersionInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionInfo :: Encode VersionInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A data volume used in a task definition.</p>
newtype Volume = Volume 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Host'" :: NullOrUndefined.NullOrUndefined (HostVolumeProperties)
  }
derive instance newtypeVolume :: Newtype Volume _
derive instance repGenericVolume :: Generic Volume _
instance showVolume :: Show Volume where
  show = genericShow
instance decodeVolume :: Decode Volume where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolume :: Encode Volume where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details on a data volume from another container in the same task definition.</p>
newtype VolumeFrom = VolumeFrom 
  { "SourceContainer'" :: NullOrUndefined.NullOrUndefined (String)
  , "ReadOnly'" :: NullOrUndefined.NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeVolumeFrom :: Newtype VolumeFrom _
derive instance repGenericVolumeFrom :: Generic VolumeFrom _
instance showVolumeFrom :: Show VolumeFrom where
  show = genericShow
instance decodeVolumeFrom :: Decode VolumeFrom where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeFrom :: Encode VolumeFrom where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeFromList = VolumeFromList (Array VolumeFrom)
derive instance newtypeVolumeFromList :: Newtype VolumeFromList _
derive instance repGenericVolumeFromList :: Generic VolumeFromList _
instance showVolumeFromList :: Show VolumeFromList where
  show = genericShow
instance decodeVolumeFromList :: Decode VolumeFromList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeFromList :: Encode VolumeFromList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeList = VolumeList (Array Volume)
derive instance newtypeVolumeList :: Newtype VolumeList _
derive instance repGenericVolumeList :: Generic VolumeList _
instance showVolumeList :: Show VolumeList where
  show = genericShow
instance decodeVolumeList :: Decode VolumeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeList :: Encode VolumeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
