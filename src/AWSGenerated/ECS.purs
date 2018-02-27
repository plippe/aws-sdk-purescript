

-- | <p>Amazon Elastic Container Service (Amazon ECS) is a highly scalable, fast, container management service that makes it easy to run, stop, and manage Docker containers on a cluster. You can host your cluster on a serverless infrastructure that is managed by Amazon ECS by launching your services or tasks using the Fargate launch type. For more control, you can host your tasks on a cluster of Amazon Elastic Compute Cloud (Amazon EC2) instances that you manage by using the EC2 launch type. For more information about launch types, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html">Amazon ECS Launch Types</a>.</p> <p>Amazon ECS lets you launch and stop container-based applications with simple API calls, allows you to get the state of your cluster from a centralized service, and gives you access to many familiar Amazon EC2 features.</p> <p>You can use Amazon ECS to schedule the placement of containers across your cluster based on your resource needs, isolation policies, and availability requirements. Amazon ECS eliminates the need for you to operate your own cluster management and configuration management systems or worry about scaling your management infrastructure.</p>
module AWS.ECS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ECS" :: String


-- | <p>Creates a new Amazon ECS cluster. By default, your account receives a <code>default</code> cluster when you launch your first container instance. However, you can create your own cluster with a unique name with the <code>CreateCluster</code> action.</p> <note> <p>When you call the <a>CreateCluster</a> API operation, Amazon ECS attempts to create the service-linked role for your account so that required resources in other AWS services can be managed on your behalf. However, if the IAM user that makes the call does not have permissions to create the service-linked role, it is not created. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html">Using Service-Linked Roles for Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </note>
createCluster :: forall eff. CreateClusterRequest -> Aff (err :: AWS.RequestError | eff) CreateClusterResponse
createCluster = AWS.request serviceName "createCluster" 


-- | <p>Runs and maintains a desired number of tasks from a specified task definition. If the number of tasks running in a service drops below <code>desiredCount</code>, Amazon ECS spawns another copy of the task in the specified cluster. To update an existing service, see <a>UpdateService</a>.</p> <p>In addition to maintaining the desired count of tasks in your service, you can optionally run your service behind a load balancer. The load balancer distributes traffic across the tasks that are associated with the service. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html">Service Load Balancing</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can optionally specify a deployment configuration for your service. During a deployment, the service scheduler uses the <code>minimumHealthyPercent</code> and <code>maximumPercent</code> parameters to determine the deployment strategy. The deployment is triggered by changing the task definition or the desired count of a service with an <a>UpdateService</a> operation.</p> <p>The <code>minimumHealthyPercent</code> represents a lower limit on the number of your service's tasks that must remain in the <code>RUNNING</code> state during a deployment, as a percentage of the <code>desiredCount</code> (rounded up to the nearest integer). This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a <code>desiredCount</code> of four tasks and a <code>minimumHealthyPercent</code> of 50%, the scheduler can stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that <i>do not</i> use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that <i>do</i> use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer. The default value for <code>minimumHealthyPercent</code> is 50% in the console and 100% for the AWS CLI, the AWS SDKs, and the APIs.</p> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of your service's tasks that are allowed in the <code>RUNNING</code> or <code>PENDING</code> state during a deployment, as a percentage of the <code>desiredCount</code> (rounded down to the nearest integer). This parameter enables you to define the deployment batch size. For example, if your service has a <code>desiredCount</code> of four tasks and a <code>maximumPercent</code> value of 200%, the scheduler can start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for <code>maximumPercent</code> is 200%.</p> <p>When the service scheduler launches new tasks, it determines task placement in your cluster using the following logic:</p> <ul> <li> <p>Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).</p> </li> <li> <p>By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy) with the <code>placementStrategy</code> parameter):</p> <ul> <li> <p>Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.</p> </li> <li> <p>Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.</p> </li> </ul> </li> </ul>
createService :: forall eff. CreateServiceRequest -> Aff (err :: AWS.RequestError | eff) CreateServiceResponse
createService = AWS.request serviceName "createService" 


-- | <p>Deletes one or more custom attributes from an Amazon ECS resource.</p>
deleteAttributes :: forall eff. DeleteAttributesRequest -> Aff (err :: AWS.RequestError | eff) DeleteAttributesResponse
deleteAttributes = AWS.request serviceName "deleteAttributes" 


-- | <p>Deletes the specified cluster. You must deregister all container instances from this cluster before you may delete it. You can list the container instances in a cluster with <a>ListContainerInstances</a> and deregister them with <a>DeregisterContainerInstance</a>.</p>
deleteCluster :: forall eff. DeleteClusterRequest -> Aff (err :: AWS.RequestError | eff) DeleteClusterResponse
deleteCluster = AWS.request serviceName "deleteCluster" 


-- | <p>Deletes a specified service within a cluster. You can delete a service if you have no running tasks in it and the desired task count is zero. If the service is actively maintaining tasks, you cannot delete it, and you must update the service to a desired task count of zero. For more information, see <a>UpdateService</a>.</p> <note> <p>When you delete a service, if there are still running tasks that require cleanup, the service status moves from <code>ACTIVE</code> to <code>DRAINING</code>, and the service is no longer visible in the console or in <a>ListServices</a> API operations. After the tasks have stopped, then the service status moves from <code>DRAINING</code> to <code>INACTIVE</code>. Services in the <code>DRAINING</code> or <code>INACTIVE</code> status can still be viewed with <a>DescribeServices</a> API operations. However, in the future, <code>INACTIVE</code> services may be cleaned up and purged from Amazon ECS record keeping, and <a>DescribeServices</a> API operations on those services return a <code>ServiceNotFoundException</code> error.</p> </note>
deleteService :: forall eff. DeleteServiceRequest -> Aff (err :: AWS.RequestError | eff) DeleteServiceResponse
deleteService = AWS.request serviceName "deleteService" 


-- | <p>Deregisters an Amazon ECS container instance from the specified cluster. This instance is no longer available to run tasks.</p> <p>If you intend to use the container instance for some other purpose after deregistration, you should stop all of the tasks running on the container instance before deregistration. That prevents any orphaned tasks from consuming resources.</p> <p>Deregistering a container instance removes the instance from a cluster, but it does not terminate the EC2 instance; if you are finished using the instance, be sure to terminate it in the Amazon EC2 console to stop billing.</p> <note> <p>If you terminate a running container instance, Amazon ECS automatically deregisters the instance from your cluster (stopped container instances or instances with disconnected agents are not automatically deregistered when terminated).</p> </note>
deregisterContainerInstance :: forall eff. DeregisterContainerInstanceRequest -> Aff (err :: AWS.RequestError | eff) DeregisterContainerInstanceResponse
deregisterContainerInstance = AWS.request serviceName "deregisterContainerInstance" 


-- | <p>Deregisters the specified task definition by family and revision. Upon deregistration, the task definition is marked as <code>INACTIVE</code>. Existing tasks and services that reference an <code>INACTIVE</code> task definition continue to run without disruption. Existing services that reference an <code>INACTIVE</code> task definition can still scale up or down by modifying the service's desired count.</p> <p>You cannot use an <code>INACTIVE</code> task definition to run new tasks or create new services, and you cannot update an existing service to reference an <code>INACTIVE</code> task definition (although there may be up to a 10-minute window following deregistration where these restrictions have not yet taken effect).</p> <note> <p>At this time, <code>INACTIVE</code> task definitions remain discoverable in your account indefinitely; however, this behavior is subject to change in the future, so you should not rely on <code>INACTIVE</code> task definitions persisting beyond the lifecycle of any associated tasks and services.</p> </note>
deregisterTaskDefinition :: forall eff. DeregisterTaskDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DeregisterTaskDefinitionResponse
deregisterTaskDefinition = AWS.request serviceName "deregisterTaskDefinition" 


-- | <p>Describes one or more of your clusters.</p>
describeClusters :: forall eff. DescribeClustersRequest -> Aff (err :: AWS.RequestError | eff) DescribeClustersResponse
describeClusters = AWS.request serviceName "describeClusters" 


-- | <p>Describes Amazon Elastic Container Service container instances. Returns metadata about registered and remaining resources on each container instance requested.</p>
describeContainerInstances :: forall eff. DescribeContainerInstancesRequest -> Aff (err :: AWS.RequestError | eff) DescribeContainerInstancesResponse
describeContainerInstances = AWS.request serviceName "describeContainerInstances" 


-- | <p>Describes the specified services running in your cluster.</p>
describeServices :: forall eff. DescribeServicesRequest -> Aff (err :: AWS.RequestError | eff) DescribeServicesResponse
describeServices = AWS.request serviceName "describeServices" 


-- | <p>Describes a task definition. You can specify a <code>family</code> and <code>revision</code> to find information about a specific task definition, or you can simply specify the family to find the latest <code>ACTIVE</code> revision in that family.</p> <note> <p>You can only describe <code>INACTIVE</code> task definitions while an active task or service references them.</p> </note>
describeTaskDefinition :: forall eff. DescribeTaskDefinitionRequest -> Aff (err :: AWS.RequestError | eff) DescribeTaskDefinitionResponse
describeTaskDefinition = AWS.request serviceName "describeTaskDefinition" 


-- | <p>Describes a specified task or tasks.</p>
describeTasks :: forall eff. DescribeTasksRequest -> Aff (err :: AWS.RequestError | eff) DescribeTasksResponse
describeTasks = AWS.request serviceName "describeTasks" 


-- | <note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Returns an endpoint for the Amazon ECS agent to poll for updates.</p>
discoverPollEndpoint :: forall eff. DiscoverPollEndpointRequest -> Aff (err :: AWS.RequestError | eff) DiscoverPollEndpointResponse
discoverPollEndpoint = AWS.request serviceName "discoverPollEndpoint" 


-- | <p>Lists the attributes for Amazon ECS resources within a specified target type and cluster. When you specify a target type and cluster, <code>ListAttributes</code> returns a list of attribute objects, one for each attribute on each resource. You can filter the list of results to a single attribute name to only return results that have that name. You can also filter the results by attribute name and value, for example, to see which container instances in a cluster are running a Linux AMI (<code>ecs.os-type=linux</code>). </p>
listAttributes :: forall eff. ListAttributesRequest -> Aff (err :: AWS.RequestError | eff) ListAttributesResponse
listAttributes = AWS.request serviceName "listAttributes" 


-- | <p>Returns a list of existing clusters.</p>
listClusters :: forall eff. ListClustersRequest -> Aff (err :: AWS.RequestError | eff) ListClustersResponse
listClusters = AWS.request serviceName "listClusters" 


-- | <p>Returns a list of container instances in a specified cluster. You can filter the results of a <code>ListContainerInstances</code> operation with cluster query language statements inside the <code>filter</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html">Cluster Query Language</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
listContainerInstances :: forall eff. ListContainerInstancesRequest -> Aff (err :: AWS.RequestError | eff) ListContainerInstancesResponse
listContainerInstances = AWS.request serviceName "listContainerInstances" 


-- | <p>Lists the services that are running in a specified cluster.</p>
listServices :: forall eff. ListServicesRequest -> Aff (err :: AWS.RequestError | eff) ListServicesResponse
listServices = AWS.request serviceName "listServices" 


-- | <p>Returns a list of task definition families that are registered to your account (which may include task definition families that no longer have any <code>ACTIVE</code> task definition revisions).</p> <p>You can filter out task definition families that do not contain any <code>ACTIVE</code> task definition revisions by setting the <code>status</code> parameter to <code>ACTIVE</code>. You can also filter the results with the <code>familyPrefix</code> parameter.</p>
listTaskDefinitionFamilies :: forall eff. ListTaskDefinitionFamiliesRequest -> Aff (err :: AWS.RequestError | eff) ListTaskDefinitionFamiliesResponse
listTaskDefinitionFamilies = AWS.request serviceName "listTaskDefinitionFamilies" 


-- | <p>Returns a list of task definitions that are registered to your account. You can filter the results by family name with the <code>familyPrefix</code> parameter or by status with the <code>status</code> parameter.</p>
listTaskDefinitions :: forall eff. ListTaskDefinitionsRequest -> Aff (err :: AWS.RequestError | eff) ListTaskDefinitionsResponse
listTaskDefinitions = AWS.request serviceName "listTaskDefinitions" 


-- | <p>Returns a list of tasks for a specified cluster. You can filter the results by family name, by a particular container instance, or by the desired status of the task with the <code>family</code>, <code>containerInstance</code>, and <code>desiredStatus</code> parameters.</p> <p>Recently stopped tasks might appear in the returned results. Currently, stopped tasks appear in the returned results for at least one hour. </p>
listTasks :: forall eff. ListTasksRequest -> Aff (err :: AWS.RequestError | eff) ListTasksResponse
listTasks = AWS.request serviceName "listTasks" 


-- | <p>Create or update an attribute on an Amazon ECS resource. If the attribute does not exist, it is created. If the attribute exists, its value is replaced with the specified value. To delete an attribute, use <a>DeleteAttributes</a>. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes">Attributes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
putAttributes :: forall eff. PutAttributesRequest -> Aff (err :: AWS.RequestError | eff) PutAttributesResponse
putAttributes = AWS.request serviceName "putAttributes" 


-- | <note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Registers an EC2 instance into the specified cluster. This instance becomes available to place containers on.</p>
registerContainerInstance :: forall eff. RegisterContainerInstanceRequest -> Aff (err :: AWS.RequestError | eff) RegisterContainerInstanceResponse
registerContainerInstance = AWS.request serviceName "registerContainerInstance" 


-- | <p>Registers a new task definition from the supplied <code>family</code> and <code>containerDefinitions</code>. Optionally, you can add data volumes to your containers with the <code>volumes</code> parameter. For more information about task definition parameters and defaults, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html">Amazon ECS Task Definitions</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can specify an IAM role for your task with the <code>taskRoleArn</code> parameter. When you specify an IAM role for a task, its containers can then use the latest versions of the AWS CLI or SDKs to make API requests to the AWS services that are specified in the IAM policy associated with the role. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html">IAM Roles for Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can specify a Docker networking mode for the containers in your task definition with the <code>networkMode</code> parameter. The available network modes correspond to those described in <a href="https://docs.docker.com/engine/reference/run/#/network-settings">Network settings</a> in the Docker run reference. If you specify the <code>awsvpc</code> network mode, the task is allocated an Elastic Network Interface, and you must specify a <a>NetworkConfiguration</a> when you create a service or run a task with the task definition. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html">Task Networking</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
registerTaskDefinition :: forall eff. RegisterTaskDefinitionRequest -> Aff (err :: AWS.RequestError | eff) RegisterTaskDefinitionResponse
registerTaskDefinition = AWS.request serviceName "registerTaskDefinition" 


-- | <p>Starts a new task using the specified task definition.</p> <p>You can allow Amazon ECS to place tasks for you, or you can customize how Amazon ECS places tasks using placement constraints and placement strategies. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html">Scheduling Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>Alternatively, you can use <a>StartTask</a> to use your own scheduler or place tasks manually on specific container instances.</p> <p>The Amazon ECS API follows an eventual consistency model, due to the distributed nature of the system supporting the API. This means that the result of an API command you run that affects your Amazon ECS resources might not be immediately visible to all subsequent commands you run. You should keep this in mind when you carry out an API command that immediately follows a previous API command.</p> <p>To manage eventual consistency, you can do the following:</p> <ul> <li> <p>Confirm the state of the resource before you run a command to modify it. Run the DescribeTasks command using an exponential backoff algorithm to ensure that you allow enough time for the previous command to propagate through the system. To do this, run the DescribeTasks command repeatedly, starting with a couple of seconds of wait time, and increasing gradually up to five minutes of wait time.</p> </li> <li> <p>Add wait time between subsequent commands, even if the DescribeTasks command returns an accurate response. Apply an exponential backoff algorithm starting with a couple of seconds of wait time, and increase gradually up to about five minutes of wait time.</p> </li> </ul>
runTask :: forall eff. RunTaskRequest -> Aff (err :: AWS.RequestError | eff) RunTaskResponse
runTask = AWS.request serviceName "runTask" 


-- | <p>Starts a new task from the specified task definition on the specified container instance or instances.</p> <p>Alternatively, you can use <a>RunTask</a> to place tasks for you. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html">Scheduling Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
startTask :: forall eff. StartTaskRequest -> Aff (err :: AWS.RequestError | eff) StartTaskResponse
startTask = AWS.request serviceName "startTask" 


-- | <p>Stops a running task.</p> <p>When <a>StopTask</a> is called on a task, the equivalent of <code>docker stop</code> is issued to the containers running in the task. This results in a <code>SIGTERM</code> and a default 30-second timeout, after which <code>SIGKILL</code> is sent and the containers are forcibly stopped. If the container handles the <code>SIGTERM</code> gracefully and exits within 30 seconds from receiving it, no <code>SIGKILL</code> is sent.</p> <note> <p>The default 30-second timeout can be configured on the Amazon ECS container agent with the <code>ECS_CONTAINER_STOP_TIMEOUT</code> variable. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html">Amazon ECS Container Agent Configuration</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </note>
stopTask :: forall eff. StopTaskRequest -> Aff (err :: AWS.RequestError | eff) StopTaskResponse
stopTask = AWS.request serviceName "stopTask" 


-- | <note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Sent to acknowledge that a container changed states.</p>
submitContainerStateChange :: forall eff. SubmitContainerStateChangeRequest -> Aff (err :: AWS.RequestError | eff) SubmitContainerStateChangeResponse
submitContainerStateChange = AWS.request serviceName "submitContainerStateChange" 


-- | <note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Sent to acknowledge that a task changed states.</p>
submitTaskStateChange :: forall eff. SubmitTaskStateChangeRequest -> Aff (err :: AWS.RequestError | eff) SubmitTaskStateChangeResponse
submitTaskStateChange = AWS.request serviceName "submitTaskStateChange" 


-- | <p>Updates the Amazon ECS container agent on a specified container instance. Updating the Amazon ECS container agent does not interrupt running tasks or services on the container instance. The process for updating the agent differs depending on whether your container instance was launched with the Amazon ECS-optimized AMI or another operating system.</p> <p> <code>UpdateContainerAgent</code> requires the Amazon ECS-optimized AMI or Amazon Linux with the <code>ecs-init</code> service installed and running. For help updating the Amazon ECS container agent on other operating systems, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent">Manually Updating the Amazon ECS Container Agent</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
updateContainerAgent :: forall eff. UpdateContainerAgentRequest -> Aff (err :: AWS.RequestError | eff) UpdateContainerAgentResponse
updateContainerAgent = AWS.request serviceName "updateContainerAgent" 


-- | <p>Modifies the status of an Amazon ECS container instance.</p> <p>You can change the status of a container instance to <code>DRAINING</code> to manually remove an instance from a cluster, for example to perform system updates, update the Docker daemon, or scale down the cluster size. </p> <p>When you set a container instance to <code>DRAINING</code>, Amazon ECS prevents new tasks from being scheduled for placement on the container instance and replacement service tasks are started on other container instances in the cluster if the resources are available. Service tasks on the container instance that are in the <code>PENDING</code> state are stopped immediately.</p> <p>Service tasks on the container instance that are in the <code>RUNNING</code> state are stopped and replaced according to the service's deployment configuration parameters, <code>minimumHealthyPercent</code> and <code>maximumPercent</code>. You can change the deployment configuration of your service using <a>UpdateService</a>.</p> <ul> <li> <p>If <code>minimumHealthyPercent</code> is below 100%, the scheduler can ignore <code>desiredCount</code> temporarily during task replacement. For example, <code>desiredCount</code> is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. If the minimum is 100%, the service scheduler can't remove existing tasks until the replacement tasks are considered healthy. Tasks for services that do not use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer.</p> </li> <li> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of running tasks during task replacement, which enables you to define the replacement batch size. For example, if <code>desiredCount</code> of four tasks, a maximum of 200% starts four new tasks before stopping the four tasks to be drained (provided that the cluster resources required to do this are available). If the maximum is 100%, then replacement tasks can't start until the draining tasks have stopped.</p> </li> </ul> <p>Any <code>PENDING</code> or <code>RUNNING</code> tasks that do not belong to a service are not affected; you must wait for them to finish or stop them manually.</p> <p>A container instance has completed draining when it has no more <code>RUNNING</code> tasks. You can verify this using <a>ListTasks</a>.</p> <p>When you set a container instance to <code>ACTIVE</code>, the Amazon ECS scheduler can begin scheduling tasks on the instance again.</p>
updateContainerInstancesState :: forall eff. UpdateContainerInstancesStateRequest -> Aff (err :: AWS.RequestError | eff) UpdateContainerInstancesStateResponse
updateContainerInstancesState = AWS.request serviceName "updateContainerInstancesState" 


-- | <p>Modifies the desired count, deployment configuration, network configuration, or task definition used in a service.</p> <p>You can add to or subtract from the number of instantiations of a task definition in a service by specifying the cluster that the service is running in and a new <code>desiredCount</code> parameter.</p> <p>You can use <a>UpdateService</a> to modify your task definition and deploy a new version of your service.</p> <p>You can also update the deployment configuration of a service. When a deployment is triggered by updating the task definition of a service, the service scheduler uses the deployment configuration parameters, <code>minimumHealthyPercent</code> and <code>maximumPercent</code>, to determine the deployment strategy.</p> <ul> <li> <p>If <code>minimumHealthyPercent</code> is below 100%, the scheduler can ignore <code>desiredCount</code> temporarily during a deployment. For example, if <code>desiredCount</code> is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. Tasks for services that do not use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer.</p> </li> <li> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of running tasks during a deployment, which enables you to define the deployment batch size. For example, if <code>desiredCount</code> is four tasks, a maximum of 200% starts four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available).</p> </li> </ul> <p>When <a>UpdateService</a> stops a task during a deployment, the equivalent of <code>docker stop</code> is issued to the containers running in the task. This results in a <code>SIGTERM</code> and a 30-second timeout, after which <code>SIGKILL</code> is sent and the containers are forcibly stopped. If the container handles the <code>SIGTERM</code> gracefully and exits within 30 seconds from receiving it, no <code>SIGKILL</code> is sent.</p> <p>When the service scheduler launches new tasks, it determines task placement in your cluster with the following logic:</p> <ul> <li> <p>Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).</p> </li> <li> <p>By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy):</p> <ul> <li> <p>Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.</p> </li> <li> <p>Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.</p> </li> </ul> </li> </ul> <p>When the service scheduler stops running tasks, it attempts to maintain balance across the Availability Zones in your cluster using the following logic: </p> <ul> <li> <p>Sort the container instances by the largest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have two, container instances in either zone B or C are considered optimal for termination.</p> </li> <li> <p>Stop the task on a container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the largest number of running tasks for this service.</p> </li> </ul>
updateService :: forall eff. UpdateServiceRequest -> Aff (err :: AWS.RequestError | eff) UpdateServiceResponse
updateService = AWS.request serviceName "updateService" 


-- | <p>You do not have authorization to perform the requested action.</p>
newtype AccessDeniedException = AccessDeniedException 
  { 
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _


newtype AgentUpdateStatus = AgentUpdateStatus String
derive instance newtypeAgentUpdateStatus :: Newtype AgentUpdateStatus _


newtype AssignPublicIp = AssignPublicIp String
derive instance newtypeAssignPublicIp :: Newtype AssignPublicIp _


-- | <p>An object representing a container instance or task attachment.</p>
newtype Attachment = Attachment 
  { "Id'" :: NullOrUndefined (String)
  , "Type'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (String)
  , "Details'" :: NullOrUndefined (AttachmentDetails)
  }
derive instance newtypeAttachment :: Newtype Attachment _


newtype AttachmentDetails = AttachmentDetails (Array KeyValuePair)
derive instance newtypeAttachmentDetails :: Newtype AttachmentDetails _


-- | <p>An object representing a change in state for a task attachment.</p>
newtype AttachmentStateChange = AttachmentStateChange 
  { "AttachmentArn'" :: (String)
  , "Status'" :: (String)
  }
derive instance newtypeAttachmentStateChange :: Newtype AttachmentStateChange _


newtype AttachmentStateChanges = AttachmentStateChanges (Array AttachmentStateChange)
derive instance newtypeAttachmentStateChanges :: Newtype AttachmentStateChanges _


newtype Attachments = Attachments (Array Attachment)
derive instance newtypeAttachments :: Newtype Attachments _


-- | <p>An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes">Attributes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
newtype Attribute = Attribute 
  { "Name'" :: (String)
  , "Value'" :: NullOrUndefined (String)
  , "TargetType'" :: NullOrUndefined (TargetType)
  , "TargetId'" :: NullOrUndefined (String)
  }
derive instance newtypeAttribute :: Newtype Attribute _


-- | <p>You can apply up to 10 custom attributes per resource. You can view the attributes of a resource with <a>ListAttributes</a>. You can remove existing attributes on a resource with <a>DeleteAttributes</a>.</p>
newtype AttributeLimitExceededException = AttributeLimitExceededException 
  { 
  }
derive instance newtypeAttributeLimitExceededException :: Newtype AttributeLimitExceededException _


newtype Attributes = Attributes (Array Attribute)
derive instance newtypeAttributes :: Newtype Attributes _


-- | <p>An object representing the networking details for a task or service.</p>
newtype AwsVpcConfiguration = AwsVpcConfiguration 
  { "Subnets'" :: (StringList)
  , "SecurityGroups'" :: NullOrUndefined (StringList)
  , "AssignPublicIp'" :: NullOrUndefined (AssignPublicIp)
  }
derive instance newtypeAwsVpcConfiguration :: Newtype AwsVpcConfiguration _


-- | <p>Your AWS account has been blocked. <a href="http://aws.amazon.com/contact-us/">Contact AWS Customer Support</a> for more information.</p>
newtype BlockedException = BlockedException 
  { 
  }
derive instance newtypeBlockedException :: Newtype BlockedException _


newtype BoxedBoolean = BoxedBoolean Boolean
derive instance newtypeBoxedBoolean :: Newtype BoxedBoolean _


newtype BoxedInteger = BoxedInteger Int
derive instance newtypeBoxedInteger :: Newtype BoxedInteger _


-- | <p>These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid.</p>
newtype ClientException = ClientException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeClientException :: Newtype ClientException _


-- | <p>A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.</p>
newtype Cluster = Cluster 
  { "ClusterArn'" :: NullOrUndefined (String)
  , "ClusterName'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (String)
  , "RegisteredContainerInstancesCount'" :: NullOrUndefined (Int)
  , "RunningTasksCount'" :: NullOrUndefined (Int)
  , "PendingTasksCount'" :: NullOrUndefined (Int)
  , "ActiveServicesCount'" :: NullOrUndefined (Int)
  , "Statistics'" :: NullOrUndefined (Statistics)
  }
derive instance newtypeCluster :: Newtype Cluster _


-- | <p>You cannot delete a cluster that has registered container instances. You must first deregister the container instances before you can delete the cluster. For more information, see <a>DeregisterContainerInstance</a>.</p>
newtype ClusterContainsContainerInstancesException = ClusterContainsContainerInstancesException 
  { 
  }
derive instance newtypeClusterContainsContainerInstancesException :: Newtype ClusterContainsContainerInstancesException _


-- | <p>You cannot delete a cluster that contains services. You must first update the service to reduce its desired task count to 0 and then delete the service. For more information, see <a>UpdateService</a> and <a>DeleteService</a>.</p>
newtype ClusterContainsServicesException = ClusterContainsServicesException 
  { 
  }
derive instance newtypeClusterContainsServicesException :: Newtype ClusterContainsServicesException _


-- | <p>You cannot delete a cluster that has active tasks.</p>
newtype ClusterContainsTasksException = ClusterContainsTasksException 
  { 
  }
derive instance newtypeClusterContainsTasksException :: Newtype ClusterContainsTasksException _


newtype ClusterField = ClusterField String
derive instance newtypeClusterField :: Newtype ClusterField _


newtype ClusterFieldList = ClusterFieldList (Array ClusterField)
derive instance newtypeClusterFieldList :: Newtype ClusterFieldList _


-- | <p>The specified cluster could not be found. You can view your available clusters with <a>ListClusters</a>. Amazon ECS clusters are region-specific.</p>
newtype ClusterNotFoundException = ClusterNotFoundException 
  { 
  }
derive instance newtypeClusterNotFoundException :: Newtype ClusterNotFoundException _


newtype Clusters = Clusters (Array Cluster)
derive instance newtypeClusters :: Newtype Clusters _


newtype Compatibility = Compatibility String
derive instance newtypeCompatibility :: Newtype Compatibility _


newtype CompatibilityList = CompatibilityList (Array Compatibility)
derive instance newtypeCompatibilityList :: Newtype CompatibilityList _


newtype Connectivity = Connectivity String
derive instance newtypeConnectivity :: Newtype Connectivity _


-- | <p>A Docker container that is part of a task.</p>
newtype Container = Container 
  { "ContainerArn'" :: NullOrUndefined (String)
  , "TaskArn'" :: NullOrUndefined (String)
  , "Name'" :: NullOrUndefined (String)
  , "LastStatus'" :: NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined (BoxedInteger)
  , "Reason'" :: NullOrUndefined (String)
  , "NetworkBindings'" :: NullOrUndefined (NetworkBindings)
  , "NetworkInterfaces'" :: NullOrUndefined (NetworkInterfaces)
  }
derive instance newtypeContainer :: Newtype Container _


-- | <p>Container definitions are used in task definitions to describe the different containers that are launched as part of a task.</p>
newtype ContainerDefinition = ContainerDefinition 
  { "Name'" :: NullOrUndefined (String)
  , "Image'" :: NullOrUndefined (String)
  , "Cpu'" :: NullOrUndefined (Int)
  , "Memory'" :: NullOrUndefined (BoxedInteger)
  , "MemoryReservation'" :: NullOrUndefined (BoxedInteger)
  , "Links'" :: NullOrUndefined (StringList)
  , "PortMappings'" :: NullOrUndefined (PortMappingList)
  , "Essential'" :: NullOrUndefined (BoxedBoolean)
  , "EntryPoint'" :: NullOrUndefined (StringList)
  , "Command'" :: NullOrUndefined (StringList)
  , "Environment'" :: NullOrUndefined (EnvironmentVariables)
  , "MountPoints'" :: NullOrUndefined (MountPointList)
  , "VolumesFrom'" :: NullOrUndefined (VolumeFromList)
  , "LinuxParameters'" :: NullOrUndefined (LinuxParameters)
  , "Hostname'" :: NullOrUndefined (String)
  , "User'" :: NullOrUndefined (String)
  , "WorkingDirectory'" :: NullOrUndefined (String)
  , "DisableNetworking'" :: NullOrUndefined (BoxedBoolean)
  , "Privileged'" :: NullOrUndefined (BoxedBoolean)
  , "ReadonlyRootFilesystem'" :: NullOrUndefined (BoxedBoolean)
  , "DnsServers'" :: NullOrUndefined (StringList)
  , "DnsSearchDomains'" :: NullOrUndefined (StringList)
  , "ExtraHosts'" :: NullOrUndefined (HostEntryList)
  , "DockerSecurityOptions'" :: NullOrUndefined (StringList)
  , "DockerLabels'" :: NullOrUndefined (DockerLabelsMap)
  , "Ulimits'" :: NullOrUndefined (UlimitList)
  , "LogConfiguration'" :: NullOrUndefined (LogConfiguration)
  }
derive instance newtypeContainerDefinition :: Newtype ContainerDefinition _


newtype ContainerDefinitions = ContainerDefinitions (Array ContainerDefinition)
derive instance newtypeContainerDefinitions :: Newtype ContainerDefinitions _


-- | <p>An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.</p>
newtype ContainerInstance = ContainerInstance 
  { "ContainerInstanceArn'" :: NullOrUndefined (String)
  , "Ec2InstanceId'" :: NullOrUndefined (String)
  , "Version'" :: NullOrUndefined (Number)
  , "VersionInfo'" :: NullOrUndefined (VersionInfo)
  , "RemainingResources'" :: NullOrUndefined (Resources)
  , "RegisteredResources'" :: NullOrUndefined (Resources)
  , "Status'" :: NullOrUndefined (String)
  , "AgentConnected'" :: NullOrUndefined (Boolean)
  , "RunningTasksCount'" :: NullOrUndefined (Int)
  , "PendingTasksCount'" :: NullOrUndefined (Int)
  , "AgentUpdateStatus'" :: NullOrUndefined (AgentUpdateStatus)
  , "Attributes'" :: NullOrUndefined (Attributes)
  , "RegisteredAt'" :: NullOrUndefined (Number)
  , "Attachments'" :: NullOrUndefined (Attachments)
  }
derive instance newtypeContainerInstance :: Newtype ContainerInstance _


newtype ContainerInstanceStatus = ContainerInstanceStatus String
derive instance newtypeContainerInstanceStatus :: Newtype ContainerInstanceStatus _


newtype ContainerInstances = ContainerInstances (Array ContainerInstance)
derive instance newtypeContainerInstances :: Newtype ContainerInstances _


-- | <p>The overrides that should be sent to a container.</p>
newtype ContainerOverride = ContainerOverride 
  { "Name'" :: NullOrUndefined (String)
  , "Command'" :: NullOrUndefined (StringList)
  , "Environment'" :: NullOrUndefined (EnvironmentVariables)
  , "Cpu'" :: NullOrUndefined (BoxedInteger)
  , "Memory'" :: NullOrUndefined (BoxedInteger)
  , "MemoryReservation'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeContainerOverride :: Newtype ContainerOverride _


newtype ContainerOverrides = ContainerOverrides (Array ContainerOverride)
derive instance newtypeContainerOverrides :: Newtype ContainerOverrides _


-- | <p>An object representing a change in state for a container.</p>
newtype ContainerStateChange = ContainerStateChange 
  { "ContainerName'" :: NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined (BoxedInteger)
  , "NetworkBindings'" :: NullOrUndefined (NetworkBindings)
  , "Reason'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (String)
  }
derive instance newtypeContainerStateChange :: Newtype ContainerStateChange _


newtype ContainerStateChanges = ContainerStateChanges (Array ContainerStateChange)
derive instance newtypeContainerStateChanges :: Newtype ContainerStateChanges _


newtype Containers = Containers (Array Container)
derive instance newtypeContainers :: Newtype Containers _


newtype CreateClusterRequest = CreateClusterRequest 
  { "ClusterName'" :: NullOrUndefined (String)
  }
derive instance newtypeCreateClusterRequest :: Newtype CreateClusterRequest _


newtype CreateClusterResponse = CreateClusterResponse 
  { "Cluster'" :: NullOrUndefined (Cluster)
  }
derive instance newtypeCreateClusterResponse :: Newtype CreateClusterResponse _


newtype CreateServiceRequest = CreateServiceRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "ServiceName'" :: (String)
  , "TaskDefinition'" :: (String)
  , "LoadBalancers'" :: NullOrUndefined (LoadBalancers)
  , "DesiredCount'" :: (BoxedInteger)
  , "ClientToken'" :: NullOrUndefined (String)
  , "LaunchType'" :: NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined (String)
  , "Role'" :: NullOrUndefined (String)
  , "DeploymentConfiguration'" :: NullOrUndefined (DeploymentConfiguration)
  , "PlacementConstraints'" :: NullOrUndefined (PlacementConstraints)
  , "PlacementStrategy'" :: NullOrUndefined (PlacementStrategies)
  , "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration)
  , "HealthCheckGracePeriodSeconds'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeCreateServiceRequest :: Newtype CreateServiceRequest _


newtype CreateServiceResponse = CreateServiceResponse 
  { "Service'" :: NullOrUndefined (Service)
  }
derive instance newtypeCreateServiceResponse :: Newtype CreateServiceResponse _


newtype DeleteAttributesRequest = DeleteAttributesRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Attributes'" :: (Attributes)
  }
derive instance newtypeDeleteAttributesRequest :: Newtype DeleteAttributesRequest _


newtype DeleteAttributesResponse = DeleteAttributesResponse 
  { "Attributes'" :: NullOrUndefined (Attributes)
  }
derive instance newtypeDeleteAttributesResponse :: Newtype DeleteAttributesResponse _


newtype DeleteClusterRequest = DeleteClusterRequest 
  { "Cluster'" :: (String)
  }
derive instance newtypeDeleteClusterRequest :: Newtype DeleteClusterRequest _


newtype DeleteClusterResponse = DeleteClusterResponse 
  { "Cluster'" :: NullOrUndefined (Cluster)
  }
derive instance newtypeDeleteClusterResponse :: Newtype DeleteClusterResponse _


newtype DeleteServiceRequest = DeleteServiceRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Service'" :: (String)
  }
derive instance newtypeDeleteServiceRequest :: Newtype DeleteServiceRequest _


newtype DeleteServiceResponse = DeleteServiceResponse 
  { "Service'" :: NullOrUndefined (Service)
  }
derive instance newtypeDeleteServiceResponse :: Newtype DeleteServiceResponse _


-- | <p>The details of an Amazon ECS service deployment.</p>
newtype Deployment = Deployment 
  { "Id'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (String)
  , "TaskDefinition'" :: NullOrUndefined (String)
  , "DesiredCount'" :: NullOrUndefined (Int)
  , "PendingCount'" :: NullOrUndefined (Int)
  , "RunningCount'" :: NullOrUndefined (Int)
  , "CreatedAt'" :: NullOrUndefined (Number)
  , "UpdatedAt'" :: NullOrUndefined (Number)
  , "LaunchType'" :: NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined (String)
  , "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration)
  }
derive instance newtypeDeployment :: Newtype Deployment _


-- | <p>Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.</p>
newtype DeploymentConfiguration = DeploymentConfiguration 
  { "MaximumPercent'" :: NullOrUndefined (BoxedInteger)
  , "MinimumHealthyPercent'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeDeploymentConfiguration :: Newtype DeploymentConfiguration _


newtype Deployments = Deployments (Array Deployment)
derive instance newtypeDeployments :: Newtype Deployments _


newtype DeregisterContainerInstanceRequest = DeregisterContainerInstanceRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "ContainerInstance'" :: (String)
  , "Force'" :: NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeDeregisterContainerInstanceRequest :: Newtype DeregisterContainerInstanceRequest _


newtype DeregisterContainerInstanceResponse = DeregisterContainerInstanceResponse 
  { "ContainerInstance'" :: NullOrUndefined (ContainerInstance)
  }
derive instance newtypeDeregisterContainerInstanceResponse :: Newtype DeregisterContainerInstanceResponse _


newtype DeregisterTaskDefinitionRequest = DeregisterTaskDefinitionRequest 
  { "TaskDefinition'" :: (String)
  }
derive instance newtypeDeregisterTaskDefinitionRequest :: Newtype DeregisterTaskDefinitionRequest _


newtype DeregisterTaskDefinitionResponse = DeregisterTaskDefinitionResponse 
  { "TaskDefinition'" :: NullOrUndefined (TaskDefinition)
  }
derive instance newtypeDeregisterTaskDefinitionResponse :: Newtype DeregisterTaskDefinitionResponse _


newtype DescribeClustersRequest = DescribeClustersRequest 
  { "Clusters'" :: NullOrUndefined (StringList)
  , "Include'" :: NullOrUndefined (ClusterFieldList)
  }
derive instance newtypeDescribeClustersRequest :: Newtype DescribeClustersRequest _


newtype DescribeClustersResponse = DescribeClustersResponse 
  { "Clusters'" :: NullOrUndefined (Clusters)
  , "Failures'" :: NullOrUndefined (Failures)
  }
derive instance newtypeDescribeClustersResponse :: Newtype DescribeClustersResponse _


newtype DescribeContainerInstancesRequest = DescribeContainerInstancesRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "ContainerInstances'" :: (StringList)
  }
derive instance newtypeDescribeContainerInstancesRequest :: Newtype DescribeContainerInstancesRequest _


newtype DescribeContainerInstancesResponse = DescribeContainerInstancesResponse 
  { "ContainerInstances'" :: NullOrUndefined (ContainerInstances)
  , "Failures'" :: NullOrUndefined (Failures)
  }
derive instance newtypeDescribeContainerInstancesResponse :: Newtype DescribeContainerInstancesResponse _


newtype DescribeServicesRequest = DescribeServicesRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Services'" :: (StringList)
  }
derive instance newtypeDescribeServicesRequest :: Newtype DescribeServicesRequest _


newtype DescribeServicesResponse = DescribeServicesResponse 
  { "Services'" :: NullOrUndefined (Services)
  , "Failures'" :: NullOrUndefined (Failures)
  }
derive instance newtypeDescribeServicesResponse :: Newtype DescribeServicesResponse _


newtype DescribeTaskDefinitionRequest = DescribeTaskDefinitionRequest 
  { "TaskDefinition'" :: (String)
  }
derive instance newtypeDescribeTaskDefinitionRequest :: Newtype DescribeTaskDefinitionRequest _


newtype DescribeTaskDefinitionResponse = DescribeTaskDefinitionResponse 
  { "TaskDefinition'" :: NullOrUndefined (TaskDefinition)
  }
derive instance newtypeDescribeTaskDefinitionResponse :: Newtype DescribeTaskDefinitionResponse _


newtype DescribeTasksRequest = DescribeTasksRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Tasks'" :: (StringList)
  }
derive instance newtypeDescribeTasksRequest :: Newtype DescribeTasksRequest _


newtype DescribeTasksResponse = DescribeTasksResponse 
  { "Tasks'" :: NullOrUndefined (Tasks)
  , "Failures'" :: NullOrUndefined (Failures)
  }
derive instance newtypeDescribeTasksResponse :: Newtype DescribeTasksResponse _


newtype DesiredStatus = DesiredStatus String
derive instance newtypeDesiredStatus :: Newtype DesiredStatus _


-- | <p>An object representing a container instance host device.</p>
newtype Device = Device 
  { "HostPath'" :: (String)
  , "ContainerPath'" :: NullOrUndefined (String)
  , "Permissions'" :: NullOrUndefined (DeviceCgroupPermissions)
  }
derive instance newtypeDevice :: Newtype Device _


newtype DeviceCgroupPermission = DeviceCgroupPermission String
derive instance newtypeDeviceCgroupPermission :: Newtype DeviceCgroupPermission _


newtype DeviceCgroupPermissions = DeviceCgroupPermissions (Array DeviceCgroupPermission)
derive instance newtypeDeviceCgroupPermissions :: Newtype DeviceCgroupPermissions _


newtype DevicesList = DevicesList (Array Device)
derive instance newtypeDevicesList :: Newtype DevicesList _


newtype DiscoverPollEndpointRequest = DiscoverPollEndpointRequest 
  { "ContainerInstance'" :: NullOrUndefined (String)
  , "Cluster'" :: NullOrUndefined (String)
  }
derive instance newtypeDiscoverPollEndpointRequest :: Newtype DiscoverPollEndpointRequest _


newtype DiscoverPollEndpointResponse = DiscoverPollEndpointResponse 
  { "Endpoint'" :: NullOrUndefined (String)
  , "TelemetryEndpoint'" :: NullOrUndefined (String)
  }
derive instance newtypeDiscoverPollEndpointResponse :: Newtype DiscoverPollEndpointResponse _


newtype DockerLabelsMap = DockerLabelsMap (Map String String)
derive instance newtypeDockerLabelsMap :: Newtype DockerLabelsMap _


newtype EnvironmentVariables = EnvironmentVariables (Array KeyValuePair)
derive instance newtypeEnvironmentVariables :: Newtype EnvironmentVariables _


-- | <p>A failed resource.</p>
newtype Failure = Failure 
  { "Arn'" :: NullOrUndefined (String)
  , "Reason'" :: NullOrUndefined (String)
  }
derive instance newtypeFailure :: Newtype Failure _


newtype Failures = Failures (Array Failure)
derive instance newtypeFailures :: Newtype Failures _


-- | <p>Hostnames and IP address entries that are added to the <code>/etc/hosts</code> file of a container via the <code>extraHosts</code> parameter of its <a>ContainerDefinition</a>. </p>
newtype HostEntry = HostEntry 
  { "Hostname'" :: (String)
  , "IpAddress'" :: (String)
  }
derive instance newtypeHostEntry :: Newtype HostEntry _


newtype HostEntryList = HostEntryList (Array HostEntry)
derive instance newtypeHostEntryList :: Newtype HostEntryList _


-- | <p>Details on a container instance host volume.</p>
newtype HostVolumeProperties = HostVolumeProperties 
  { "SourcePath'" :: NullOrUndefined (String)
  }
derive instance newtypeHostVolumeProperties :: Newtype HostVolumeProperties _


-- | <p>The specified parameter is invalid. Review the available parameters for the API request.</p>
newtype InvalidParameterException = InvalidParameterException 
  { 
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <a href="https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities">Runtime privilege and Linux capabilities</a> in the <i>Docker run reference</i>. For more detailed information on these Linux capabilities, see the <a href="http://man7.org/linux/man-pages/man7/capabilities.7.html">capabilities(7)</a> Linux manual page.</p>
newtype KernelCapabilities = KernelCapabilities 
  { "Add'" :: NullOrUndefined (StringList)
  , "Drop'" :: NullOrUndefined (StringList)
  }
derive instance newtypeKernelCapabilities :: Newtype KernelCapabilities _


-- | <p>A key and value pair object.</p>
newtype KeyValuePair = KeyValuePair 
  { "Name'" :: NullOrUndefined (String)
  , "Value'" :: NullOrUndefined (String)
  }
derive instance newtypeKeyValuePair :: Newtype KeyValuePair _


newtype LaunchType = LaunchType String
derive instance newtypeLaunchType :: Newtype LaunchType _


-- | <p>Linux-specific options that are applied to the container, such as Linux <a>KernelCapabilities</a>.</p>
newtype LinuxParameters = LinuxParameters 
  { "Capabilities'" :: NullOrUndefined (KernelCapabilities)
  , "Devices'" :: NullOrUndefined (DevicesList)
  , "InitProcessEnabled'" :: NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeLinuxParameters :: Newtype LinuxParameters _


newtype ListAttributesRequest = ListAttributesRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "TargetType'" :: (TargetType)
  , "AttributeName'" :: NullOrUndefined (String)
  , "AttributeValue'" :: NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeListAttributesRequest :: Newtype ListAttributesRequest _


newtype ListAttributesResponse = ListAttributesResponse 
  { "Attributes'" :: NullOrUndefined (Attributes)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListAttributesResponse :: Newtype ListAttributesResponse _


newtype ListClustersRequest = ListClustersRequest 
  { "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeListClustersRequest :: Newtype ListClustersRequest _


newtype ListClustersResponse = ListClustersResponse 
  { "ClusterArns'" :: NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListClustersResponse :: Newtype ListClustersResponse _


newtype ListContainerInstancesRequest = ListContainerInstancesRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Filter'" :: NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (BoxedInteger)
  , "Status'" :: NullOrUndefined (ContainerInstanceStatus)
  }
derive instance newtypeListContainerInstancesRequest :: Newtype ListContainerInstancesRequest _


newtype ListContainerInstancesResponse = ListContainerInstancesResponse 
  { "ContainerInstanceArns'" :: NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListContainerInstancesResponse :: Newtype ListContainerInstancesResponse _


newtype ListServicesRequest = ListServicesRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (BoxedInteger)
  , "LaunchType'" :: NullOrUndefined (LaunchType)
  }
derive instance newtypeListServicesRequest :: Newtype ListServicesRequest _


newtype ListServicesResponse = ListServicesResponse 
  { "ServiceArns'" :: NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListServicesResponse :: Newtype ListServicesResponse _


newtype ListTaskDefinitionFamiliesRequest = ListTaskDefinitionFamiliesRequest 
  { "FamilyPrefix'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (TaskDefinitionFamilyStatus)
  , "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeListTaskDefinitionFamiliesRequest :: Newtype ListTaskDefinitionFamiliesRequest _


newtype ListTaskDefinitionFamiliesResponse = ListTaskDefinitionFamiliesResponse 
  { "Families'" :: NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListTaskDefinitionFamiliesResponse :: Newtype ListTaskDefinitionFamiliesResponse _


newtype ListTaskDefinitionsRequest = ListTaskDefinitionsRequest 
  { "FamilyPrefix'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (TaskDefinitionStatus)
  , "Sort'" :: NullOrUndefined (SortOrder)
  , "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeListTaskDefinitionsRequest :: Newtype ListTaskDefinitionsRequest _


newtype ListTaskDefinitionsResponse = ListTaskDefinitionsResponse 
  { "TaskDefinitionArns'" :: NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListTaskDefinitionsResponse :: Newtype ListTaskDefinitionsResponse _


newtype ListTasksRequest = ListTasksRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "ContainerInstance'" :: NullOrUndefined (String)
  , "Family'" :: NullOrUndefined (String)
  , "NextToken'" :: NullOrUndefined (String)
  , "MaxResults'" :: NullOrUndefined (BoxedInteger)
  , "StartedBy'" :: NullOrUndefined (String)
  , "ServiceName'" :: NullOrUndefined (String)
  , "DesiredStatus'" :: NullOrUndefined (DesiredStatus)
  , "LaunchType'" :: NullOrUndefined (LaunchType)
  }
derive instance newtypeListTasksRequest :: Newtype ListTasksRequest _


newtype ListTasksResponse = ListTasksResponse 
  { "TaskArns'" :: NullOrUndefined (StringList)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListTasksResponse :: Newtype ListTasksResponse _


-- | <p>Details on a load balancer that is used with a service.</p>
newtype LoadBalancer = LoadBalancer 
  { "TargetGroupArn'" :: NullOrUndefined (String)
  , "LoadBalancerName'" :: NullOrUndefined (String)
  , "ContainerName'" :: NullOrUndefined (String)
  , "ContainerPort'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeLoadBalancer :: Newtype LoadBalancer _


newtype LoadBalancers = LoadBalancers (Array LoadBalancer)
derive instance newtypeLoadBalancers :: Newtype LoadBalancers _


-- | <p>Log configuration options to send to a custom log driver for the container.</p>
newtype LogConfiguration = LogConfiguration 
  { "LogDriver'" :: (LogDriver)
  , "Options'" :: NullOrUndefined (LogConfigurationOptionsMap)
  }
derive instance newtypeLogConfiguration :: Newtype LogConfiguration _


newtype LogConfigurationOptionsMap = LogConfigurationOptionsMap (Map String String)
derive instance newtypeLogConfigurationOptionsMap :: Newtype LogConfigurationOptionsMap _


newtype LogDriver = LogDriver String
derive instance newtypeLogDriver :: Newtype LogDriver _


-- | <p>Amazon ECS is unable to determine the current version of the Amazon ECS container agent on the container instance and does not have enough information to proceed with an update. This could be because the agent running on the container instance is an older or custom version that does not use our version information.</p>
newtype MissingVersionException = MissingVersionException 
  { 
  }
derive instance newtypeMissingVersionException :: Newtype MissingVersionException _


-- | <p>Details on a volume mount point that is used in a container definition.</p>
newtype MountPoint = MountPoint 
  { "SourceVolume'" :: NullOrUndefined (String)
  , "ContainerPath'" :: NullOrUndefined (String)
  , "ReadOnly'" :: NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeMountPoint :: Newtype MountPoint _


newtype MountPointList = MountPointList (Array MountPoint)
derive instance newtypeMountPointList :: Newtype MountPointList _


-- | <p>Details on the network bindings between a container and its host container instance. After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.</p>
newtype NetworkBinding = NetworkBinding 
  { "BindIP'" :: NullOrUndefined (String)
  , "ContainerPort'" :: NullOrUndefined (BoxedInteger)
  , "HostPort'" :: NullOrUndefined (BoxedInteger)
  , "Protocol'" :: NullOrUndefined (TransportProtocol)
  }
derive instance newtypeNetworkBinding :: Newtype NetworkBinding _


newtype NetworkBindings = NetworkBindings (Array NetworkBinding)
derive instance newtypeNetworkBindings :: Newtype NetworkBindings _


-- | <p>An object representing the network configuration for a task or service.</p>
newtype NetworkConfiguration = NetworkConfiguration 
  { "AwsvpcConfiguration'" :: NullOrUndefined (AwsVpcConfiguration)
  }
derive instance newtypeNetworkConfiguration :: Newtype NetworkConfiguration _


-- | <p>An object representing the Elastic Network Interface for tasks that use the <code>awsvpc</code> network mode.</p>
newtype NetworkInterface = NetworkInterface 
  { "AttachmentId'" :: NullOrUndefined (String)
  , "PrivateIpv4Address'" :: NullOrUndefined (String)
  , "Ipv6Address'" :: NullOrUndefined (String)
  }
derive instance newtypeNetworkInterface :: Newtype NetworkInterface _


newtype NetworkInterfaces = NetworkInterfaces (Array NetworkInterface)
derive instance newtypeNetworkInterfaces :: Newtype NetworkInterfaces _


newtype NetworkMode = NetworkMode String
derive instance newtypeNetworkMode :: Newtype NetworkMode _


-- | <p>There is no update available for this Amazon ECS container agent. This could be because the agent is already running the latest version, or it is so old that there is no update path to the current version.</p>
newtype NoUpdateAvailableException = NoUpdateAvailableException 
  { 
  }
derive instance newtypeNoUpdateAvailableException :: Newtype NoUpdateAvailableException _


-- | <p>An object representing a constraint on task placement. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
newtype PlacementConstraint = PlacementConstraint 
  { "Type'" :: NullOrUndefined (PlacementConstraintType)
  , "Expression'" :: NullOrUndefined (String)
  }
derive instance newtypePlacementConstraint :: Newtype PlacementConstraint _


newtype PlacementConstraintType = PlacementConstraintType String
derive instance newtypePlacementConstraintType :: Newtype PlacementConstraintType _


newtype PlacementConstraints = PlacementConstraints (Array PlacementConstraint)
derive instance newtypePlacementConstraints :: Newtype PlacementConstraints _


newtype PlacementStrategies = PlacementStrategies (Array PlacementStrategy)
derive instance newtypePlacementStrategies :: Newtype PlacementStrategies _


-- | <p>The task placement strategy for a task or service. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html">Task Placement Strategies</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
newtype PlacementStrategy = PlacementStrategy 
  { "Type'" :: NullOrUndefined (PlacementStrategyType)
  , "Field'" :: NullOrUndefined (String)
  }
derive instance newtypePlacementStrategy :: Newtype PlacementStrategy _


newtype PlacementStrategyType = PlacementStrategyType String
derive instance newtypePlacementStrategyType :: Newtype PlacementStrategyType _


-- | <p>The specified platform version does not satisfy the task definition’s required capabilities.</p>
newtype PlatformTaskDefinitionIncompatibilityException = PlatformTaskDefinitionIncompatibilityException 
  { 
  }
derive instance newtypePlatformTaskDefinitionIncompatibilityException :: Newtype PlatformTaskDefinitionIncompatibilityException _


-- | <p>The specified platform version does not exist.</p>
newtype PlatformUnknownException = PlatformUnknownException 
  { 
  }
derive instance newtypePlatformUnknownException :: Newtype PlatformUnknownException _


-- | <p>Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition.</p> <p>If using containers in a task with the <code>awsvpc</code> or <code>host</code> network mode, exposed ports should be specified using <code>containerPort</code>. The <code>hostPort</code> can be left blank or it must be the same value as the <code>containerPort</code>.</p> <p>After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.</p>
newtype PortMapping = PortMapping 
  { "ContainerPort'" :: NullOrUndefined (BoxedInteger)
  , "HostPort'" :: NullOrUndefined (BoxedInteger)
  , "Protocol'" :: NullOrUndefined (TransportProtocol)
  }
derive instance newtypePortMapping :: Newtype PortMapping _


newtype PortMappingList = PortMappingList (Array PortMapping)
derive instance newtypePortMappingList :: Newtype PortMappingList _


newtype PutAttributesRequest = PutAttributesRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Attributes'" :: (Attributes)
  }
derive instance newtypePutAttributesRequest :: Newtype PutAttributesRequest _


newtype PutAttributesResponse = PutAttributesResponse 
  { "Attributes'" :: NullOrUndefined (Attributes)
  }
derive instance newtypePutAttributesResponse :: Newtype PutAttributesResponse _


newtype RegisterContainerInstanceRequest = RegisterContainerInstanceRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "InstanceIdentityDocument'" :: NullOrUndefined (String)
  , "InstanceIdentityDocumentSignature'" :: NullOrUndefined (String)
  , "TotalResources'" :: NullOrUndefined (Resources)
  , "VersionInfo'" :: NullOrUndefined (VersionInfo)
  , "ContainerInstanceArn'" :: NullOrUndefined (String)
  , "Attributes'" :: NullOrUndefined (Attributes)
  }
derive instance newtypeRegisterContainerInstanceRequest :: Newtype RegisterContainerInstanceRequest _


newtype RegisterContainerInstanceResponse = RegisterContainerInstanceResponse 
  { "ContainerInstance'" :: NullOrUndefined (ContainerInstance)
  }
derive instance newtypeRegisterContainerInstanceResponse :: Newtype RegisterContainerInstanceResponse _


newtype RegisterTaskDefinitionRequest = RegisterTaskDefinitionRequest 
  { "Family'" :: (String)
  , "TaskRoleArn'" :: NullOrUndefined (String)
  , "ExecutionRoleArn'" :: NullOrUndefined (String)
  , "NetworkMode'" :: NullOrUndefined (NetworkMode)
  , "ContainerDefinitions'" :: (ContainerDefinitions)
  , "Volumes'" :: NullOrUndefined (VolumeList)
  , "PlacementConstraints'" :: NullOrUndefined (TaskDefinitionPlacementConstraints)
  , "RequiresCompatibilities'" :: NullOrUndefined (CompatibilityList)
  , "Cpu'" :: NullOrUndefined (String)
  , "Memory'" :: NullOrUndefined (String)
  }
derive instance newtypeRegisterTaskDefinitionRequest :: Newtype RegisterTaskDefinitionRequest _


newtype RegisterTaskDefinitionResponse = RegisterTaskDefinitionResponse 
  { "TaskDefinition'" :: NullOrUndefined (TaskDefinition)
  }
derive instance newtypeRegisterTaskDefinitionResponse :: Newtype RegisterTaskDefinitionResponse _


newtype RequiresAttributes = RequiresAttributes (Array Attribute)
derive instance newtypeRequiresAttributes :: Newtype RequiresAttributes _


-- | <p>Describes the resources available for a container instance.</p>
newtype Resource = Resource 
  { "Name'" :: NullOrUndefined (String)
  , "Type'" :: NullOrUndefined (String)
  , "DoubleValue'" :: NullOrUndefined (Number)
  , "LongValue'" :: NullOrUndefined (Number)
  , "IntegerValue'" :: NullOrUndefined (Int)
  , "StringSetValue'" :: NullOrUndefined (StringList)
  }
derive instance newtypeResource :: Newtype Resource _


newtype Resources = Resources (Array Resource)
derive instance newtypeResources :: Newtype Resources _


newtype RunTaskRequest = RunTaskRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "TaskDefinition'" :: (String)
  , "Overrides'" :: NullOrUndefined (TaskOverride)
  , "Count'" :: NullOrUndefined (BoxedInteger)
  , "StartedBy'" :: NullOrUndefined (String)
  , "Group'" :: NullOrUndefined (String)
  , "PlacementConstraints'" :: NullOrUndefined (PlacementConstraints)
  , "PlacementStrategy'" :: NullOrUndefined (PlacementStrategies)
  , "LaunchType'" :: NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined (String)
  , "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration)
  }
derive instance newtypeRunTaskRequest :: Newtype RunTaskRequest _


newtype RunTaskResponse = RunTaskResponse 
  { "Tasks'" :: NullOrUndefined (Tasks)
  , "Failures'" :: NullOrUndefined (Failures)
  }
derive instance newtypeRunTaskResponse :: Newtype RunTaskResponse _


-- | <p>These errors are usually caused by a server issue.</p>
newtype ServerException = ServerException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeServerException :: Newtype ServerException _


-- | <p>Details on a service within a cluster</p>
newtype Service = Service 
  { "ServiceArn'" :: NullOrUndefined (String)
  , "ServiceName'" :: NullOrUndefined (String)
  , "ClusterArn'" :: NullOrUndefined (String)
  , "LoadBalancers'" :: NullOrUndefined (LoadBalancers)
  , "Status'" :: NullOrUndefined (String)
  , "DesiredCount'" :: NullOrUndefined (Int)
  , "RunningCount'" :: NullOrUndefined (Int)
  , "PendingCount'" :: NullOrUndefined (Int)
  , "LaunchType'" :: NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined (String)
  , "TaskDefinition'" :: NullOrUndefined (String)
  , "DeploymentConfiguration'" :: NullOrUndefined (DeploymentConfiguration)
  , "Deployments'" :: NullOrUndefined (Deployments)
  , "RoleArn'" :: NullOrUndefined (String)
  , "Events'" :: NullOrUndefined (ServiceEvents)
  , "CreatedAt'" :: NullOrUndefined (Number)
  , "PlacementConstraints'" :: NullOrUndefined (PlacementConstraints)
  , "PlacementStrategy'" :: NullOrUndefined (PlacementStrategies)
  , "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration)
  , "HealthCheckGracePeriodSeconds'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeService :: Newtype Service _


-- | <p>Details on an event associated with a service.</p>
newtype ServiceEvent = ServiceEvent 
  { "Id'" :: NullOrUndefined (String)
  , "CreatedAt'" :: NullOrUndefined (Number)
  , "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeServiceEvent :: Newtype ServiceEvent _


newtype ServiceEvents = ServiceEvents (Array ServiceEvent)
derive instance newtypeServiceEvents :: Newtype ServiceEvents _


-- | <p>The specified service is not active. You can't update a service that is inactive. If you have previously deleted a service, you can re-create it with <a>CreateService</a>.</p>
newtype ServiceNotActiveException = ServiceNotActiveException 
  { 
  }
derive instance newtypeServiceNotActiveException :: Newtype ServiceNotActiveException _


-- | <p>The specified service could not be found. You can view your available services with <a>ListServices</a>. Amazon ECS services are cluster-specific and region-specific.</p>
newtype ServiceNotFoundException = ServiceNotFoundException 
  { 
  }
derive instance newtypeServiceNotFoundException :: Newtype ServiceNotFoundException _


newtype Services = Services (Array Service)
derive instance newtypeServices :: Newtype Services _


newtype SortOrder = SortOrder String
derive instance newtypeSortOrder :: Newtype SortOrder _


newtype StartTaskRequest = StartTaskRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "TaskDefinition'" :: (String)
  , "Overrides'" :: NullOrUndefined (TaskOverride)
  , "ContainerInstances'" :: (StringList)
  , "StartedBy'" :: NullOrUndefined (String)
  , "Group'" :: NullOrUndefined (String)
  , "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration)
  }
derive instance newtypeStartTaskRequest :: Newtype StartTaskRequest _


newtype StartTaskResponse = StartTaskResponse 
  { "Tasks'" :: NullOrUndefined (Tasks)
  , "Failures'" :: NullOrUndefined (Failures)
  }
derive instance newtypeStartTaskResponse :: Newtype StartTaskResponse _


newtype Statistics = Statistics (Array KeyValuePair)
derive instance newtypeStatistics :: Newtype Statistics _


newtype StopTaskRequest = StopTaskRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Task'" :: (String)
  , "Reason'" :: NullOrUndefined (String)
  }
derive instance newtypeStopTaskRequest :: Newtype StopTaskRequest _


newtype StopTaskResponse = StopTaskResponse 
  { "Task'" :: NullOrUndefined (Task)
  }
derive instance newtypeStopTaskResponse :: Newtype StopTaskResponse _


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _


newtype SubmitContainerStateChangeRequest = SubmitContainerStateChangeRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Task'" :: NullOrUndefined (String)
  , "ContainerName'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (String)
  , "ExitCode'" :: NullOrUndefined (BoxedInteger)
  , "Reason'" :: NullOrUndefined (String)
  , "NetworkBindings'" :: NullOrUndefined (NetworkBindings)
  }
derive instance newtypeSubmitContainerStateChangeRequest :: Newtype SubmitContainerStateChangeRequest _


newtype SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse 
  { "Acknowledgment'" :: NullOrUndefined (String)
  }
derive instance newtypeSubmitContainerStateChangeResponse :: Newtype SubmitContainerStateChangeResponse _


newtype SubmitTaskStateChangeRequest = SubmitTaskStateChangeRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Task'" :: NullOrUndefined (String)
  , "Status'" :: NullOrUndefined (String)
  , "Reason'" :: NullOrUndefined (String)
  , "Containers'" :: NullOrUndefined (ContainerStateChanges)
  , "Attachments'" :: NullOrUndefined (AttachmentStateChanges)
  , "PullStartedAt'" :: NullOrUndefined (Number)
  , "PullStoppedAt'" :: NullOrUndefined (Number)
  , "ExecutionStoppedAt'" :: NullOrUndefined (Number)
  }
derive instance newtypeSubmitTaskStateChangeRequest :: Newtype SubmitTaskStateChangeRequest _


newtype SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse 
  { "Acknowledgment'" :: NullOrUndefined (String)
  }
derive instance newtypeSubmitTaskStateChangeResponse :: Newtype SubmitTaskStateChangeResponse _


-- | <p>The specified target could not be found. You can view your available container instances with <a>ListContainerInstances</a>. Amazon ECS container instances are cluster-specific and region-specific.</p>
newtype TargetNotFoundException = TargetNotFoundException 
  { 
  }
derive instance newtypeTargetNotFoundException :: Newtype TargetNotFoundException _


newtype TargetType = TargetType String
derive instance newtypeTargetType :: Newtype TargetType _


-- | <p>Details on a task in a cluster.</p>
newtype Task = Task 
  { "TaskArn'" :: NullOrUndefined (String)
  , "ClusterArn'" :: NullOrUndefined (String)
  , "TaskDefinitionArn'" :: NullOrUndefined (String)
  , "ContainerInstanceArn'" :: NullOrUndefined (String)
  , "Overrides'" :: NullOrUndefined (TaskOverride)
  , "LastStatus'" :: NullOrUndefined (String)
  , "DesiredStatus'" :: NullOrUndefined (String)
  , "Cpu'" :: NullOrUndefined (String)
  , "Memory'" :: NullOrUndefined (String)
  , "Containers'" :: NullOrUndefined (Containers)
  , "StartedBy'" :: NullOrUndefined (String)
  , "Version'" :: NullOrUndefined (Number)
  , "StoppedReason'" :: NullOrUndefined (String)
  , "Connectivity'" :: NullOrUndefined (Connectivity)
  , "ConnectivityAt'" :: NullOrUndefined (Number)
  , "PullStartedAt'" :: NullOrUndefined (Number)
  , "PullStoppedAt'" :: NullOrUndefined (Number)
  , "ExecutionStoppedAt'" :: NullOrUndefined (Number)
  , "CreatedAt'" :: NullOrUndefined (Number)
  , "StartedAt'" :: NullOrUndefined (Number)
  , "StoppingAt'" :: NullOrUndefined (Number)
  , "StoppedAt'" :: NullOrUndefined (Number)
  , "Group'" :: NullOrUndefined (String)
  , "LaunchType'" :: NullOrUndefined (LaunchType)
  , "PlatformVersion'" :: NullOrUndefined (String)
  , "Attachments'" :: NullOrUndefined (Attachments)
  }
derive instance newtypeTask :: Newtype Task _


-- | <p>Details of a task definition.</p>
newtype TaskDefinition = TaskDefinition 
  { "TaskDefinitionArn'" :: NullOrUndefined (String)
  , "ContainerDefinitions'" :: NullOrUndefined (ContainerDefinitions)
  , "Family'" :: NullOrUndefined (String)
  , "TaskRoleArn'" :: NullOrUndefined (String)
  , "ExecutionRoleArn'" :: NullOrUndefined (String)
  , "NetworkMode'" :: NullOrUndefined (NetworkMode)
  , "Revision'" :: NullOrUndefined (Int)
  , "Volumes'" :: NullOrUndefined (VolumeList)
  , "Status'" :: NullOrUndefined (TaskDefinitionStatus)
  , "RequiresAttributes'" :: NullOrUndefined (RequiresAttributes)
  , "PlacementConstraints'" :: NullOrUndefined (TaskDefinitionPlacementConstraints)
  , "Compatibilities'" :: NullOrUndefined (CompatibilityList)
  , "RequiresCompatibilities'" :: NullOrUndefined (CompatibilityList)
  , "Cpu'" :: NullOrUndefined (String)
  , "Memory'" :: NullOrUndefined (String)
  }
derive instance newtypeTaskDefinition :: Newtype TaskDefinition _


newtype TaskDefinitionFamilyStatus = TaskDefinitionFamilyStatus String
derive instance newtypeTaskDefinitionFamilyStatus :: Newtype TaskDefinitionFamilyStatus _


-- | <p>An object representing a constraint on task placement in the task definition.</p> <p>If you are using the Fargate launch type, task placement contraints are not supported.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>
newtype TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint 
  { "Type'" :: NullOrUndefined (TaskDefinitionPlacementConstraintType)
  , "Expression'" :: NullOrUndefined (String)
  }
derive instance newtypeTaskDefinitionPlacementConstraint :: Newtype TaskDefinitionPlacementConstraint _


newtype TaskDefinitionPlacementConstraintType = TaskDefinitionPlacementConstraintType String
derive instance newtypeTaskDefinitionPlacementConstraintType :: Newtype TaskDefinitionPlacementConstraintType _


newtype TaskDefinitionPlacementConstraints = TaskDefinitionPlacementConstraints (Array TaskDefinitionPlacementConstraint)
derive instance newtypeTaskDefinitionPlacementConstraints :: Newtype TaskDefinitionPlacementConstraints _


newtype TaskDefinitionStatus = TaskDefinitionStatus String
derive instance newtypeTaskDefinitionStatus :: Newtype TaskDefinitionStatus _


-- | <p>The overrides associated with a task.</p>
newtype TaskOverride = TaskOverride 
  { "ContainerOverrides'" :: NullOrUndefined (ContainerOverrides)
  , "TaskRoleArn'" :: NullOrUndefined (String)
  , "ExecutionRoleArn'" :: NullOrUndefined (String)
  }
derive instance newtypeTaskOverride :: Newtype TaskOverride _


newtype Tasks = Tasks (Array Task)
derive instance newtypeTasks :: Newtype Tasks _


newtype TransportProtocol = TransportProtocol String
derive instance newtypeTransportProtocol :: Newtype TransportProtocol _


-- | <p>The <code>ulimit</code> settings to pass to the container.</p>
newtype Ulimit = Ulimit 
  { "Name'" :: (UlimitName)
  , "SoftLimit'" :: (Int)
  , "HardLimit'" :: (Int)
  }
derive instance newtypeUlimit :: Newtype Ulimit _


newtype UlimitList = UlimitList (Array Ulimit)
derive instance newtypeUlimitList :: Newtype UlimitList _


newtype UlimitName = UlimitName String
derive instance newtypeUlimitName :: Newtype UlimitName _


-- | <p>The specified task is not supported in this region.</p>
newtype UnsupportedFeatureException = UnsupportedFeatureException 
  { 
  }
derive instance newtypeUnsupportedFeatureException :: Newtype UnsupportedFeatureException _


newtype UpdateContainerAgentRequest = UpdateContainerAgentRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "ContainerInstance'" :: (String)
  }
derive instance newtypeUpdateContainerAgentRequest :: Newtype UpdateContainerAgentRequest _


newtype UpdateContainerAgentResponse = UpdateContainerAgentResponse 
  { "ContainerInstance'" :: NullOrUndefined (ContainerInstance)
  }
derive instance newtypeUpdateContainerAgentResponse :: Newtype UpdateContainerAgentResponse _


newtype UpdateContainerInstancesStateRequest = UpdateContainerInstancesStateRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "ContainerInstances'" :: (StringList)
  , "Status'" :: (ContainerInstanceStatus)
  }
derive instance newtypeUpdateContainerInstancesStateRequest :: Newtype UpdateContainerInstancesStateRequest _


newtype UpdateContainerInstancesStateResponse = UpdateContainerInstancesStateResponse 
  { "ContainerInstances'" :: NullOrUndefined (ContainerInstances)
  , "Failures'" :: NullOrUndefined (Failures)
  }
derive instance newtypeUpdateContainerInstancesStateResponse :: Newtype UpdateContainerInstancesStateResponse _


-- | <p>There is already a current Amazon ECS container agent update in progress on the specified container instance. If the container agent becomes disconnected while it is in a transitional stage, such as <code>PENDING</code> or <code>STAGING</code>, the update process can get stuck in that state. However, when the agent reconnects, it resumes where it stopped previously.</p>
newtype UpdateInProgressException = UpdateInProgressException 
  { 
  }
derive instance newtypeUpdateInProgressException :: Newtype UpdateInProgressException _


newtype UpdateServiceRequest = UpdateServiceRequest 
  { "Cluster'" :: NullOrUndefined (String)
  , "Service'" :: (String)
  , "DesiredCount'" :: NullOrUndefined (BoxedInteger)
  , "TaskDefinition'" :: NullOrUndefined (String)
  , "DeploymentConfiguration'" :: NullOrUndefined (DeploymentConfiguration)
  , "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration)
  , "PlatformVersion'" :: NullOrUndefined (String)
  , "ForceNewDeployment'" :: NullOrUndefined (Boolean)
  , "HealthCheckGracePeriodSeconds'" :: NullOrUndefined (BoxedInteger)
  }
derive instance newtypeUpdateServiceRequest :: Newtype UpdateServiceRequest _


newtype UpdateServiceResponse = UpdateServiceResponse 
  { "Service'" :: NullOrUndefined (Service)
  }
derive instance newtypeUpdateServiceResponse :: Newtype UpdateServiceResponse _


-- | <p>The Docker and Amazon ECS container agent version information about a container instance.</p>
newtype VersionInfo = VersionInfo 
  { "AgentVersion'" :: NullOrUndefined (String)
  , "AgentHash'" :: NullOrUndefined (String)
  , "DockerVersion'" :: NullOrUndefined (String)
  }
derive instance newtypeVersionInfo :: Newtype VersionInfo _


-- | <p>A data volume used in a task definition.</p>
newtype Volume = Volume 
  { "Name'" :: NullOrUndefined (String)
  , "Host'" :: NullOrUndefined (HostVolumeProperties)
  }
derive instance newtypeVolume :: Newtype Volume _


-- | <p>Details on a data volume from another container in the same task definition.</p>
newtype VolumeFrom = VolumeFrom 
  { "SourceContainer'" :: NullOrUndefined (String)
  , "ReadOnly'" :: NullOrUndefined (BoxedBoolean)
  }
derive instance newtypeVolumeFrom :: Newtype VolumeFrom _


newtype VolumeFromList = VolumeFromList (Array VolumeFrom)
derive instance newtypeVolumeFromList :: Newtype VolumeFromList _


newtype VolumeList = VolumeList (Array Volume)
derive instance newtypeVolumeList :: Newtype VolumeList _
