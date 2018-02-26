## Module AWS.ECS

<p>Amazon Elastic Container Service (Amazon ECS) is a highly scalable, fast, container management service that makes it easy to run, stop, and manage Docker containers on a cluster. You can host your cluster on a serverless infrastructure that is managed by Amazon ECS by launching your services or tasks using the Fargate launch type. For more control, you can host your tasks on a cluster of Amazon Elastic Compute Cloud (Amazon EC2) instances that you manage by using the EC2 launch type. For more information about launch types, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html">Amazon ECS Launch Types</a>.</p> <p>Amazon ECS lets you launch and stop container-based applications with simple API calls, allows you to get the state of your cluster from a centralized service, and gives you access to many familiar Amazon EC2 features.</p> <p>You can use Amazon ECS to schedule the placement of containers across your cluster based on your resource needs, isolation policies, and availability requirements. Amazon ECS eliminates the need for you to operate your own cluster management and configuration management systems or worry about scaling your management infrastructure.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createCluster`

``` purescript
createCluster :: forall eff. CreateClusterRequest -> Aff (err :: RequestError | eff) CreateClusterResponse
```

<p>Creates a new Amazon ECS cluster. By default, your account receives a <code>default</code> cluster when you launch your first container instance. However, you can create your own cluster with a unique name with the <code>CreateCluster</code> action.</p> <note> <p>When you call the <a>CreateCluster</a> API operation, Amazon ECS attempts to create the service-linked role for your account so that required resources in other AWS services can be managed on your behalf. However, if the IAM user that makes the call does not have permissions to create the service-linked role, it is not created. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/using-service-linked-roles.html">Using Service-Linked Roles for Amazon ECS</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </note>

#### `createService`

``` purescript
createService :: forall eff. CreateServiceRequest -> Aff (err :: RequestError | eff) CreateServiceResponse
```

<p>Runs and maintains a desired number of tasks from a specified task definition. If the number of tasks running in a service drops below <code>desiredCount</code>, Amazon ECS spawns another copy of the task in the specified cluster. To update an existing service, see <a>UpdateService</a>.</p> <p>In addition to maintaining the desired count of tasks in your service, you can optionally run your service behind a load balancer. The load balancer distributes traffic across the tasks that are associated with the service. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-load-balancing.html">Service Load Balancing</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can optionally specify a deployment configuration for your service. During a deployment, the service scheduler uses the <code>minimumHealthyPercent</code> and <code>maximumPercent</code> parameters to determine the deployment strategy. The deployment is triggered by changing the task definition or the desired count of a service with an <a>UpdateService</a> operation.</p> <p>The <code>minimumHealthyPercent</code> represents a lower limit on the number of your service's tasks that must remain in the <code>RUNNING</code> state during a deployment, as a percentage of the <code>desiredCount</code> (rounded up to the nearest integer). This parameter enables you to deploy without using additional cluster capacity. For example, if your service has a <code>desiredCount</code> of four tasks and a <code>minimumHealthyPercent</code> of 50%, the scheduler can stop two existing tasks to free up cluster capacity before starting two new tasks. Tasks for services that <i>do not</i> use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that <i>do</i> use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer. The default value for <code>minimumHealthyPercent</code> is 50% in the console and 100% for the AWS CLI, the AWS SDKs, and the APIs.</p> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of your service's tasks that are allowed in the <code>RUNNING</code> or <code>PENDING</code> state during a deployment, as a percentage of the <code>desiredCount</code> (rounded down to the nearest integer). This parameter enables you to define the deployment batch size. For example, if your service has a <code>desiredCount</code> of four tasks and a <code>maximumPercent</code> value of 200%, the scheduler can start four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available). The default value for <code>maximumPercent</code> is 200%.</p> <p>When the service scheduler launches new tasks, it determines task placement in your cluster using the following logic:</p> <ul> <li> <p>Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).</p> </li> <li> <p>By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy) with the <code>placementStrategy</code> parameter):</p> <ul> <li> <p>Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.</p> </li> <li> <p>Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.</p> </li> </ul> </li> </ul>

#### `deleteAttributes`

``` purescript
deleteAttributes :: forall eff. DeleteAttributesRequest -> Aff (err :: RequestError | eff) DeleteAttributesResponse
```

<p>Deletes one or more custom attributes from an Amazon ECS resource.</p>

#### `deleteCluster`

``` purescript
deleteCluster :: forall eff. DeleteClusterRequest -> Aff (err :: RequestError | eff) DeleteClusterResponse
```

<p>Deletes the specified cluster. You must deregister all container instances from this cluster before you may delete it. You can list the container instances in a cluster with <a>ListContainerInstances</a> and deregister them with <a>DeregisterContainerInstance</a>.</p>

#### `deleteService`

``` purescript
deleteService :: forall eff. DeleteServiceRequest -> Aff (err :: RequestError | eff) DeleteServiceResponse
```

<p>Deletes a specified service within a cluster. You can delete a service if you have no running tasks in it and the desired task count is zero. If the service is actively maintaining tasks, you cannot delete it, and you must update the service to a desired task count of zero. For more information, see <a>UpdateService</a>.</p> <note> <p>When you delete a service, if there are still running tasks that require cleanup, the service status moves from <code>ACTIVE</code> to <code>DRAINING</code>, and the service is no longer visible in the console or in <a>ListServices</a> API operations. After the tasks have stopped, then the service status moves from <code>DRAINING</code> to <code>INACTIVE</code>. Services in the <code>DRAINING</code> or <code>INACTIVE</code> status can still be viewed with <a>DescribeServices</a> API operations. However, in the future, <code>INACTIVE</code> services may be cleaned up and purged from Amazon ECS record keeping, and <a>DescribeServices</a> API operations on those services return a <code>ServiceNotFoundException</code> error.</p> </note>

#### `deregisterContainerInstance`

``` purescript
deregisterContainerInstance :: forall eff. DeregisterContainerInstanceRequest -> Aff (err :: RequestError | eff) DeregisterContainerInstanceResponse
```

<p>Deregisters an Amazon ECS container instance from the specified cluster. This instance is no longer available to run tasks.</p> <p>If you intend to use the container instance for some other purpose after deregistration, you should stop all of the tasks running on the container instance before deregistration. That prevents any orphaned tasks from consuming resources.</p> <p>Deregistering a container instance removes the instance from a cluster, but it does not terminate the EC2 instance; if you are finished using the instance, be sure to terminate it in the Amazon EC2 console to stop billing.</p> <note> <p>If you terminate a running container instance, Amazon ECS automatically deregisters the instance from your cluster (stopped container instances or instances with disconnected agents are not automatically deregistered when terminated).</p> </note>

#### `deregisterTaskDefinition`

``` purescript
deregisterTaskDefinition :: forall eff. DeregisterTaskDefinitionRequest -> Aff (err :: RequestError | eff) DeregisterTaskDefinitionResponse
```

<p>Deregisters the specified task definition by family and revision. Upon deregistration, the task definition is marked as <code>INACTIVE</code>. Existing tasks and services that reference an <code>INACTIVE</code> task definition continue to run without disruption. Existing services that reference an <code>INACTIVE</code> task definition can still scale up or down by modifying the service's desired count.</p> <p>You cannot use an <code>INACTIVE</code> task definition to run new tasks or create new services, and you cannot update an existing service to reference an <code>INACTIVE</code> task definition (although there may be up to a 10-minute window following deregistration where these restrictions have not yet taken effect).</p> <note> <p>At this time, <code>INACTIVE</code> task definitions remain discoverable in your account indefinitely; however, this behavior is subject to change in the future, so you should not rely on <code>INACTIVE</code> task definitions persisting beyond the lifecycle of any associated tasks and services.</p> </note>

#### `describeClusters`

``` purescript
describeClusters :: forall eff. DescribeClustersRequest -> Aff (err :: RequestError | eff) DescribeClustersResponse
```

<p>Describes one or more of your clusters.</p>

#### `describeContainerInstances`

``` purescript
describeContainerInstances :: forall eff. DescribeContainerInstancesRequest -> Aff (err :: RequestError | eff) DescribeContainerInstancesResponse
```

<p>Describes Amazon Elastic Container Service container instances. Returns metadata about registered and remaining resources on each container instance requested.</p>

#### `describeServices`

``` purescript
describeServices :: forall eff. DescribeServicesRequest -> Aff (err :: RequestError | eff) DescribeServicesResponse
```

<p>Describes the specified services running in your cluster.</p>

#### `describeTaskDefinition`

``` purescript
describeTaskDefinition :: forall eff. DescribeTaskDefinitionRequest -> Aff (err :: RequestError | eff) DescribeTaskDefinitionResponse
```

<p>Describes a task definition. You can specify a <code>family</code> and <code>revision</code> to find information about a specific task definition, or you can simply specify the family to find the latest <code>ACTIVE</code> revision in that family.</p> <note> <p>You can only describe <code>INACTIVE</code> task definitions while an active task or service references them.</p> </note>

#### `describeTasks`

``` purescript
describeTasks :: forall eff. DescribeTasksRequest -> Aff (err :: RequestError | eff) DescribeTasksResponse
```

<p>Describes a specified task or tasks.</p>

#### `discoverPollEndpoint`

``` purescript
discoverPollEndpoint :: forall eff. DiscoverPollEndpointRequest -> Aff (err :: RequestError | eff) DiscoverPollEndpointResponse
```

<note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Returns an endpoint for the Amazon ECS agent to poll for updates.</p>

#### `listAttributes`

``` purescript
listAttributes :: forall eff. ListAttributesRequest -> Aff (err :: RequestError | eff) ListAttributesResponse
```

<p>Lists the attributes for Amazon ECS resources within a specified target type and cluster. When you specify a target type and cluster, <code>ListAttributes</code> returns a list of attribute objects, one for each attribute on each resource. You can filter the list of results to a single attribute name to only return results that have that name. You can also filter the results by attribute name and value, for example, to see which container instances in a cluster are running a Linux AMI (<code>ecs.os-type=linux</code>). </p>

#### `listClusters`

``` purescript
listClusters :: forall eff. ListClustersRequest -> Aff (err :: RequestError | eff) ListClustersResponse
```

<p>Returns a list of existing clusters.</p>

#### `listContainerInstances`

``` purescript
listContainerInstances :: forall eff. ListContainerInstancesRequest -> Aff (err :: RequestError | eff) ListContainerInstancesResponse
```

<p>Returns a list of container instances in a specified cluster. You can filter the results of a <code>ListContainerInstances</code> operation with cluster query language statements inside the <code>filter</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html">Cluster Query Language</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `listServices`

``` purescript
listServices :: forall eff. ListServicesRequest -> Aff (err :: RequestError | eff) ListServicesResponse
```

<p>Lists the services that are running in a specified cluster.</p>

#### `listTaskDefinitionFamilies`

``` purescript
listTaskDefinitionFamilies :: forall eff. ListTaskDefinitionFamiliesRequest -> Aff (err :: RequestError | eff) ListTaskDefinitionFamiliesResponse
```

<p>Returns a list of task definition families that are registered to your account (which may include task definition families that no longer have any <code>ACTIVE</code> task definition revisions).</p> <p>You can filter out task definition families that do not contain any <code>ACTIVE</code> task definition revisions by setting the <code>status</code> parameter to <code>ACTIVE</code>. You can also filter the results with the <code>familyPrefix</code> parameter.</p>

#### `listTaskDefinitions`

``` purescript
listTaskDefinitions :: forall eff. ListTaskDefinitionsRequest -> Aff (err :: RequestError | eff) ListTaskDefinitionsResponse
```

<p>Returns a list of task definitions that are registered to your account. You can filter the results by family name with the <code>familyPrefix</code> parameter or by status with the <code>status</code> parameter.</p>

#### `listTasks`

``` purescript
listTasks :: forall eff. ListTasksRequest -> Aff (err :: RequestError | eff) ListTasksResponse
```

<p>Returns a list of tasks for a specified cluster. You can filter the results by family name, by a particular container instance, or by the desired status of the task with the <code>family</code>, <code>containerInstance</code>, and <code>desiredStatus</code> parameters.</p> <p>Recently stopped tasks might appear in the returned results. Currently, stopped tasks appear in the returned results for at least one hour. </p>

#### `putAttributes`

``` purescript
putAttributes :: forall eff. PutAttributesRequest -> Aff (err :: RequestError | eff) PutAttributesResponse
```

<p>Create or update an attribute on an Amazon ECS resource. If the attribute does not exist, it is created. If the attribute exists, its value is replaced with the specified value. To delete an attribute, use <a>DeleteAttributes</a>. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes">Attributes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `registerContainerInstance`

``` purescript
registerContainerInstance :: forall eff. RegisterContainerInstanceRequest -> Aff (err :: RequestError | eff) RegisterContainerInstanceResponse
```

<note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Registers an EC2 instance into the specified cluster. This instance becomes available to place containers on.</p>

#### `registerTaskDefinition`

``` purescript
registerTaskDefinition :: forall eff. RegisterTaskDefinitionRequest -> Aff (err :: RequestError | eff) RegisterTaskDefinitionResponse
```

<p>Registers a new task definition from the supplied <code>family</code> and <code>containerDefinitions</code>. Optionally, you can add data volumes to your containers with the <code>volumes</code> parameter. For more information about task definition parameters and defaults, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_defintions.html">Amazon ECS Task Definitions</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can specify an IAM role for your task with the <code>taskRoleArn</code> parameter. When you specify an IAM role for a task, its containers can then use the latest versions of the AWS CLI or SDKs to make API requests to the AWS services that are specified in the IAM policy associated with the role. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-iam-roles.html">IAM Roles for Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>You can specify a Docker networking mode for the containers in your task definition with the <code>networkMode</code> parameter. The available network modes correspond to those described in <a href="https://docs.docker.com/engine/reference/run/#/network-settings">Network settings</a> in the Docker run reference. If you specify the <code>awsvpc</code> network mode, the task is allocated an Elastic Network Interface, and you must specify a <a>NetworkConfiguration</a> when you create a service or run a task with the task definition. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-networking.html">Task Networking</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `runTask`

``` purescript
runTask :: forall eff. RunTaskRequest -> Aff (err :: RequestError | eff) RunTaskResponse
```

<p>Starts a new task using the specified task definition.</p> <p>You can allow Amazon ECS to place tasks for you, or you can customize how Amazon ECS places tasks using placement constraints and placement strategies. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html">Scheduling Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> <p>Alternatively, you can use <a>StartTask</a> to use your own scheduler or place tasks manually on specific container instances.</p> <p>The Amazon ECS API follows an eventual consistency model, due to the distributed nature of the system supporting the API. This means that the result of an API command you run that affects your Amazon ECS resources might not be immediately visible to all subsequent commands you run. You should keep this in mind when you carry out an API command that immediately follows a previous API command.</p> <p>To manage eventual consistency, you can do the following:</p> <ul> <li> <p>Confirm the state of the resource before you run a command to modify it. Run the DescribeTasks command using an exponential backoff algorithm to ensure that you allow enough time for the previous command to propagate through the system. To do this, run the DescribeTasks command repeatedly, starting with a couple of seconds of wait time, and increasing gradually up to five minutes of wait time.</p> </li> <li> <p>Add wait time between subsequent commands, even if the DescribeTasks command returns an accurate response. Apply an exponential backoff algorithm starting with a couple of seconds of wait time, and increase gradually up to about five minutes of wait time.</p> </li> </ul>

#### `startTask`

``` purescript
startTask :: forall eff. StartTaskRequest -> Aff (err :: RequestError | eff) StartTaskResponse
```

<p>Starts a new task from the specified task definition on the specified container instance or instances.</p> <p>Alternatively, you can use <a>RunTask</a> to place tasks for you. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/scheduling_tasks.html">Scheduling Tasks</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `stopTask`

``` purescript
stopTask :: forall eff. StopTaskRequest -> Aff (err :: RequestError | eff) StopTaskResponse
```

<p>Stops a running task.</p> <p>When <a>StopTask</a> is called on a task, the equivalent of <code>docker stop</code> is issued to the containers running in the task. This results in a <code>SIGTERM</code> and a default 30-second timeout, after which <code>SIGKILL</code> is sent and the containers are forcibly stopped. If the container handles the <code>SIGTERM</code> gracefully and exits within 30 seconds from receiving it, no <code>SIGKILL</code> is sent.</p> <note> <p>The default 30-second timeout can be configured on the Amazon ECS container agent with the <code>ECS_CONTAINER_STOP_TIMEOUT</code> variable. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-config.html">Amazon ECS Container Agent Configuration</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p> </note>

#### `submitContainerStateChange`

``` purescript
submitContainerStateChange :: forall eff. SubmitContainerStateChangeRequest -> Aff (err :: RequestError | eff) SubmitContainerStateChangeResponse
```

<note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Sent to acknowledge that a container changed states.</p>

#### `submitTaskStateChange`

``` purescript
submitTaskStateChange :: forall eff. SubmitTaskStateChangeRequest -> Aff (err :: RequestError | eff) SubmitTaskStateChangeResponse
```

<note> <p>This action is only used by the Amazon ECS agent, and it is not intended for use outside of the agent.</p> </note> <p>Sent to acknowledge that a task changed states.</p>

#### `updateContainerAgent`

``` purescript
updateContainerAgent :: forall eff. UpdateContainerAgentRequest -> Aff (err :: RequestError | eff) UpdateContainerAgentResponse
```

<p>Updates the Amazon ECS container agent on a specified container instance. Updating the Amazon ECS container agent does not interrupt running tasks or services on the container instance. The process for updating the agent differs depending on whether your container instance was launched with the Amazon ECS-optimized AMI or another operating system.</p> <p> <code>UpdateContainerAgent</code> requires the Amazon ECS-optimized AMI or Amazon Linux with the <code>ecs-init</code> service installed and running. For help updating the Amazon ECS container agent on other operating systems, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent">Manually Updating the Amazon ECS Container Agent</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `updateContainerInstancesState`

``` purescript
updateContainerInstancesState :: forall eff. UpdateContainerInstancesStateRequest -> Aff (err :: RequestError | eff) UpdateContainerInstancesStateResponse
```

<p>Modifies the status of an Amazon ECS container instance.</p> <p>You can change the status of a container instance to <code>DRAINING</code> to manually remove an instance from a cluster, for example to perform system updates, update the Docker daemon, or scale down the cluster size. </p> <p>When you set a container instance to <code>DRAINING</code>, Amazon ECS prevents new tasks from being scheduled for placement on the container instance and replacement service tasks are started on other container instances in the cluster if the resources are available. Service tasks on the container instance that are in the <code>PENDING</code> state are stopped immediately.</p> <p>Service tasks on the container instance that are in the <code>RUNNING</code> state are stopped and replaced according to the service's deployment configuration parameters, <code>minimumHealthyPercent</code> and <code>maximumPercent</code>. You can change the deployment configuration of your service using <a>UpdateService</a>.</p> <ul> <li> <p>If <code>minimumHealthyPercent</code> is below 100%, the scheduler can ignore <code>desiredCount</code> temporarily during task replacement. For example, <code>desiredCount</code> is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. If the minimum is 100%, the service scheduler can't remove existing tasks until the replacement tasks are considered healthy. Tasks for services that do not use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer.</p> </li> <li> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of running tasks during task replacement, which enables you to define the replacement batch size. For example, if <code>desiredCount</code> of four tasks, a maximum of 200% starts four new tasks before stopping the four tasks to be drained (provided that the cluster resources required to do this are available). If the maximum is 100%, then replacement tasks can't start until the draining tasks have stopped.</p> </li> </ul> <p>Any <code>PENDING</code> or <code>RUNNING</code> tasks that do not belong to a service are not affected; you must wait for them to finish or stop them manually.</p> <p>A container instance has completed draining when it has no more <code>RUNNING</code> tasks. You can verify this using <a>ListTasks</a>.</p> <p>When you set a container instance to <code>ACTIVE</code>, the Amazon ECS scheduler can begin scheduling tasks on the instance again.</p>

#### `updateService`

``` purescript
updateService :: forall eff. UpdateServiceRequest -> Aff (err :: RequestError | eff) UpdateServiceResponse
```

<p>Modifies the desired count, deployment configuration, network configuration, or task definition used in a service.</p> <p>You can add to or subtract from the number of instantiations of a task definition in a service by specifying the cluster that the service is running in and a new <code>desiredCount</code> parameter.</p> <p>You can use <a>UpdateService</a> to modify your task definition and deploy a new version of your service.</p> <p>You can also update the deployment configuration of a service. When a deployment is triggered by updating the task definition of a service, the service scheduler uses the deployment configuration parameters, <code>minimumHealthyPercent</code> and <code>maximumPercent</code>, to determine the deployment strategy.</p> <ul> <li> <p>If <code>minimumHealthyPercent</code> is below 100%, the scheduler can ignore <code>desiredCount</code> temporarily during a deployment. For example, if <code>desiredCount</code> is four tasks, a minimum of 50% allows the scheduler to stop two existing tasks before starting two new tasks. Tasks for services that do not use a load balancer are considered healthy if they are in the <code>RUNNING</code> state. Tasks for services that use a load balancer are considered healthy if they are in the <code>RUNNING</code> state and the container instance they are hosted on is reported as healthy by the load balancer.</p> </li> <li> <p>The <code>maximumPercent</code> parameter represents an upper limit on the number of running tasks during a deployment, which enables you to define the deployment batch size. For example, if <code>desiredCount</code> is four tasks, a maximum of 200% starts four new tasks before stopping the four older tasks (provided that the cluster resources required to do this are available).</p> </li> </ul> <p>When <a>UpdateService</a> stops a task during a deployment, the equivalent of <code>docker stop</code> is issued to the containers running in the task. This results in a <code>SIGTERM</code> and a 30-second timeout, after which <code>SIGKILL</code> is sent and the containers are forcibly stopped. If the container handles the <code>SIGTERM</code> gracefully and exits within 30 seconds from receiving it, no <code>SIGKILL</code> is sent.</p> <p>When the service scheduler launches new tasks, it determines task placement in your cluster with the following logic:</p> <ul> <li> <p>Determine which of the container instances in your cluster can support your service's task definition (for example, they have the required CPU, memory, ports, and container instance attributes).</p> </li> <li> <p>By default, the service scheduler attempts to balance tasks across Availability Zones in this manner (although you can choose a different placement strategy):</p> <ul> <li> <p>Sort the valid container instances by the fewest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have zero, valid container instances in either zone B or C are considered optimal for placement.</p> </li> <li> <p>Place the new service task on a valid container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the fewest number of running tasks for this service.</p> </li> </ul> </li> </ul> <p>When the service scheduler stops running tasks, it attempts to maintain balance across the Availability Zones in your cluster using the following logic: </p> <ul> <li> <p>Sort the container instances by the largest number of running tasks for this service in the same Availability Zone as the instance. For example, if zone A has one running service task and zones B and C each have two, container instances in either zone B or C are considered optimal for termination.</p> </li> <li> <p>Stop the task on a container instance in an optimal Availability Zone (based on the previous steps), favoring container instances with the largest number of running tasks for this service.</p> </li> </ul>

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException {  }
```

<p>You do not have authorization to perform the requested action.</p>

#### `AgentUpdateStatus`

``` purescript
newtype AgentUpdateStatus
  = AgentUpdateStatus String
```

#### `AssignPublicIp`

``` purescript
newtype AssignPublicIp
  = AssignPublicIp String
```

#### `Attachment`

``` purescript
newtype Attachment
  = Attachment { "Id'" :: NullOrUndefined (String), "Type'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (String), "Details'" :: NullOrUndefined (AttachmentDetails) }
```

<p>An object representing a container instance or task attachment.</p>

#### `AttachmentDetails`

``` purescript
newtype AttachmentDetails
  = AttachmentDetails (Array KeyValuePair)
```

#### `AttachmentStateChange`

``` purescript
newtype AttachmentStateChange
  = AttachmentStateChange { "AttachmentArn'" :: String, "Status'" :: String }
```

<p>An object representing a change in state for a task attachment.</p>

#### `AttachmentStateChanges`

``` purescript
newtype AttachmentStateChanges
  = AttachmentStateChanges (Array AttachmentStateChange)
```

#### `Attachments`

``` purescript
newtype Attachments
  = Attachments (Array Attachment)
```

#### `Attribute`

``` purescript
newtype Attribute
  = Attribute { "Name'" :: String, "Value'" :: NullOrUndefined (String), "TargetType'" :: NullOrUndefined (TargetType), "TargetId'" :: NullOrUndefined (String) }
```

<p>An attribute is a name-value pair associated with an Amazon ECS object. Attributes enable you to extend the Amazon ECS data model by adding custom metadata to your resources. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html#attributes">Attributes</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `AttributeLimitExceededException`

``` purescript
newtype AttributeLimitExceededException
  = AttributeLimitExceededException {  }
```

<p>You can apply up to 10 custom attributes per resource. You can view the attributes of a resource with <a>ListAttributes</a>. You can remove existing attributes on a resource with <a>DeleteAttributes</a>.</p>

#### `Attributes`

``` purescript
newtype Attributes
  = Attributes (Array Attribute)
```

#### `AwsVpcConfiguration`

``` purescript
newtype AwsVpcConfiguration
  = AwsVpcConfiguration { "Subnets'" :: StringList, "SecurityGroups'" :: NullOrUndefined (StringList), "AssignPublicIp'" :: NullOrUndefined (AssignPublicIp) }
```

<p>An object representing the networking details for a task or service.</p>

#### `BlockedException`

``` purescript
newtype BlockedException
  = BlockedException {  }
```

<p>Your AWS account has been blocked. <a href="http://aws.amazon.com/contact-us/">Contact AWS Customer Support</a> for more information.</p>

#### `BoxedBoolean`

``` purescript
newtype BoxedBoolean
  = BoxedBoolean Boolean
```

#### `BoxedInteger`

``` purescript
newtype BoxedInteger
  = BoxedInteger Int
```

#### `ClientException`

``` purescript
newtype ClientException
  = ClientException { "Message'" :: NullOrUndefined (String) }
```

<p>These errors are usually caused by a client action, such as using an action or resource on behalf of a user that doesn't have permissions to use the action or resource, or specifying an identifier that is not valid.</p>

#### `Cluster`

``` purescript
newtype Cluster
  = Cluster { "ClusterArn'" :: NullOrUndefined (String), "ClusterName'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (String), "RegisteredContainerInstancesCount'" :: NullOrUndefined (Int), "RunningTasksCount'" :: NullOrUndefined (Int), "PendingTasksCount'" :: NullOrUndefined (Int), "ActiveServicesCount'" :: NullOrUndefined (Int), "Statistics'" :: NullOrUndefined (Statistics) }
```

<p>A regional grouping of one or more container instances on which you can run task requests. Each account receives a default cluster the first time you use the Amazon ECS service, but you may also create other clusters. Clusters may contain more than one instance type simultaneously.</p>

#### `ClusterContainsContainerInstancesException`

``` purescript
newtype ClusterContainsContainerInstancesException
  = ClusterContainsContainerInstancesException {  }
```

<p>You cannot delete a cluster that has registered container instances. You must first deregister the container instances before you can delete the cluster. For more information, see <a>DeregisterContainerInstance</a>.</p>

#### `ClusterContainsServicesException`

``` purescript
newtype ClusterContainsServicesException
  = ClusterContainsServicesException {  }
```

<p>You cannot delete a cluster that contains services. You must first update the service to reduce its desired task count to 0 and then delete the service. For more information, see <a>UpdateService</a> and <a>DeleteService</a>.</p>

#### `ClusterContainsTasksException`

``` purescript
newtype ClusterContainsTasksException
  = ClusterContainsTasksException {  }
```

<p>You cannot delete a cluster that has active tasks.</p>

#### `ClusterField`

``` purescript
newtype ClusterField
  = ClusterField String
```

#### `ClusterFieldList`

``` purescript
newtype ClusterFieldList
  = ClusterFieldList (Array ClusterField)
```

#### `ClusterNotFoundException`

``` purescript
newtype ClusterNotFoundException
  = ClusterNotFoundException {  }
```

<p>The specified cluster could not be found. You can view your available clusters with <a>ListClusters</a>. Amazon ECS clusters are region-specific.</p>

#### `Clusters`

``` purescript
newtype Clusters
  = Clusters (Array Cluster)
```

#### `Compatibility`

``` purescript
newtype Compatibility
  = Compatibility String
```

#### `CompatibilityList`

``` purescript
newtype CompatibilityList
  = CompatibilityList (Array Compatibility)
```

#### `Connectivity`

``` purescript
newtype Connectivity
  = Connectivity String
```

#### `Container`

``` purescript
newtype Container
  = Container { "ContainerArn'" :: NullOrUndefined (String), "TaskArn'" :: NullOrUndefined (String), "Name'" :: NullOrUndefined (String), "LastStatus'" :: NullOrUndefined (String), "ExitCode'" :: NullOrUndefined (BoxedInteger), "Reason'" :: NullOrUndefined (String), "NetworkBindings'" :: NullOrUndefined (NetworkBindings), "NetworkInterfaces'" :: NullOrUndefined (NetworkInterfaces) }
```

<p>A Docker container that is part of a task.</p>

#### `ContainerDefinition`

``` purescript
newtype ContainerDefinition
  = ContainerDefinition { "Name'" :: NullOrUndefined (String), "Image'" :: NullOrUndefined (String), "Cpu'" :: NullOrUndefined (Int), "Memory'" :: NullOrUndefined (BoxedInteger), "MemoryReservation'" :: NullOrUndefined (BoxedInteger), "Links'" :: NullOrUndefined (StringList), "PortMappings'" :: NullOrUndefined (PortMappingList), "Essential'" :: NullOrUndefined (BoxedBoolean), "EntryPoint'" :: NullOrUndefined (StringList), "Command'" :: NullOrUndefined (StringList), "Environment'" :: NullOrUndefined (EnvironmentVariables), "MountPoints'" :: NullOrUndefined (MountPointList), "VolumesFrom'" :: NullOrUndefined (VolumeFromList), "LinuxParameters'" :: NullOrUndefined (LinuxParameters), "Hostname'" :: NullOrUndefined (String), "User'" :: NullOrUndefined (String), "WorkingDirectory'" :: NullOrUndefined (String), "DisableNetworking'" :: NullOrUndefined (BoxedBoolean), "Privileged'" :: NullOrUndefined (BoxedBoolean), "ReadonlyRootFilesystem'" :: NullOrUndefined (BoxedBoolean), "DnsServers'" :: NullOrUndefined (StringList), "DnsSearchDomains'" :: NullOrUndefined (StringList), "ExtraHosts'" :: NullOrUndefined (HostEntryList), "DockerSecurityOptions'" :: NullOrUndefined (StringList), "DockerLabels'" :: NullOrUndefined (DockerLabelsMap), "Ulimits'" :: NullOrUndefined (UlimitList), "LogConfiguration'" :: NullOrUndefined (LogConfiguration) }
```

<p>Container definitions are used in task definitions to describe the different containers that are launched as part of a task.</p>

#### `ContainerDefinitions`

``` purescript
newtype ContainerDefinitions
  = ContainerDefinitions (Array ContainerDefinition)
```

#### `ContainerInstance`

``` purescript
newtype ContainerInstance
  = ContainerInstance { "ContainerInstanceArn'" :: NullOrUndefined (String), "Ec2InstanceId'" :: NullOrUndefined (String), "Version'" :: NullOrUndefined (Number), "VersionInfo'" :: NullOrUndefined (VersionInfo), "RemainingResources'" :: NullOrUndefined (Resources), "RegisteredResources'" :: NullOrUndefined (Resources), "Status'" :: NullOrUndefined (String), "AgentConnected'" :: NullOrUndefined (Boolean), "RunningTasksCount'" :: NullOrUndefined (Int), "PendingTasksCount'" :: NullOrUndefined (Int), "AgentUpdateStatus'" :: NullOrUndefined (AgentUpdateStatus), "Attributes'" :: NullOrUndefined (Attributes), "RegisteredAt'" :: NullOrUndefined (Number), "Attachments'" :: NullOrUndefined (Attachments) }
```

<p>An EC2 instance that is running the Amazon ECS agent and has been registered with a cluster.</p>

#### `ContainerInstanceStatus`

``` purescript
newtype ContainerInstanceStatus
  = ContainerInstanceStatus String
```

#### `ContainerInstances`

``` purescript
newtype ContainerInstances
  = ContainerInstances (Array ContainerInstance)
```

#### `ContainerOverride`

``` purescript
newtype ContainerOverride
  = ContainerOverride { "Name'" :: NullOrUndefined (String), "Command'" :: NullOrUndefined (StringList), "Environment'" :: NullOrUndefined (EnvironmentVariables), "Cpu'" :: NullOrUndefined (BoxedInteger), "Memory'" :: NullOrUndefined (BoxedInteger), "MemoryReservation'" :: NullOrUndefined (BoxedInteger) }
```

<p>The overrides that should be sent to a container.</p>

#### `ContainerOverrides`

``` purescript
newtype ContainerOverrides
  = ContainerOverrides (Array ContainerOverride)
```

#### `ContainerStateChange`

``` purescript
newtype ContainerStateChange
  = ContainerStateChange { "ContainerName'" :: NullOrUndefined (String), "ExitCode'" :: NullOrUndefined (BoxedInteger), "NetworkBindings'" :: NullOrUndefined (NetworkBindings), "Reason'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (String) }
```

<p>An object representing a change in state for a container.</p>

#### `ContainerStateChanges`

``` purescript
newtype ContainerStateChanges
  = ContainerStateChanges (Array ContainerStateChange)
```

#### `Containers`

``` purescript
newtype Containers
  = Containers (Array Container)
```

#### `CreateClusterRequest`

``` purescript
newtype CreateClusterRequest
  = CreateClusterRequest { "ClusterName'" :: NullOrUndefined (String) }
```

#### `CreateClusterResponse`

``` purescript
newtype CreateClusterResponse
  = CreateClusterResponse { "Cluster'" :: NullOrUndefined (Cluster) }
```

#### `CreateServiceRequest`

``` purescript
newtype CreateServiceRequest
  = CreateServiceRequest { "Cluster'" :: NullOrUndefined (String), "ServiceName'" :: String, "TaskDefinition'" :: String, "LoadBalancers'" :: NullOrUndefined (LoadBalancers), "DesiredCount'" :: BoxedInteger, "ClientToken'" :: NullOrUndefined (String), "LaunchType'" :: NullOrUndefined (LaunchType), "PlatformVersion'" :: NullOrUndefined (String), "Role'" :: NullOrUndefined (String), "DeploymentConfiguration'" :: NullOrUndefined (DeploymentConfiguration), "PlacementConstraints'" :: NullOrUndefined (PlacementConstraints), "PlacementStrategy'" :: NullOrUndefined (PlacementStrategies), "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration), "HealthCheckGracePeriodSeconds'" :: NullOrUndefined (BoxedInteger) }
```

#### `CreateServiceResponse`

``` purescript
newtype CreateServiceResponse
  = CreateServiceResponse { "Service'" :: NullOrUndefined (Service) }
```

#### `DeleteAttributesRequest`

``` purescript
newtype DeleteAttributesRequest
  = DeleteAttributesRequest { "Cluster'" :: NullOrUndefined (String), "Attributes'" :: Attributes }
```

#### `DeleteAttributesResponse`

``` purescript
newtype DeleteAttributesResponse
  = DeleteAttributesResponse { "Attributes'" :: NullOrUndefined (Attributes) }
```

#### `DeleteClusterRequest`

``` purescript
newtype DeleteClusterRequest
  = DeleteClusterRequest { "Cluster'" :: String }
```

#### `DeleteClusterResponse`

``` purescript
newtype DeleteClusterResponse
  = DeleteClusterResponse { "Cluster'" :: NullOrUndefined (Cluster) }
```

#### `DeleteServiceRequest`

``` purescript
newtype DeleteServiceRequest
  = DeleteServiceRequest { "Cluster'" :: NullOrUndefined (String), "Service'" :: String }
```

#### `DeleteServiceResponse`

``` purescript
newtype DeleteServiceResponse
  = DeleteServiceResponse { "Service'" :: NullOrUndefined (Service) }
```

#### `Deployment`

``` purescript
newtype Deployment
  = Deployment { "Id'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (String), "TaskDefinition'" :: NullOrUndefined (String), "DesiredCount'" :: NullOrUndefined (Int), "PendingCount'" :: NullOrUndefined (Int), "RunningCount'" :: NullOrUndefined (Int), "CreatedAt'" :: NullOrUndefined (Number), "UpdatedAt'" :: NullOrUndefined (Number), "LaunchType'" :: NullOrUndefined (LaunchType), "PlatformVersion'" :: NullOrUndefined (String), "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration) }
```

<p>The details of an Amazon ECS service deployment.</p>

#### `DeploymentConfiguration`

``` purescript
newtype DeploymentConfiguration
  = DeploymentConfiguration { "MaximumPercent'" :: NullOrUndefined (BoxedInteger), "MinimumHealthyPercent'" :: NullOrUndefined (BoxedInteger) }
```

<p>Optional deployment parameters that control how many tasks run during the deployment and the ordering of stopping and starting tasks.</p>

#### `Deployments`

``` purescript
newtype Deployments
  = Deployments (Array Deployment)
```

#### `DeregisterContainerInstanceRequest`

``` purescript
newtype DeregisterContainerInstanceRequest
  = DeregisterContainerInstanceRequest { "Cluster'" :: NullOrUndefined (String), "ContainerInstance'" :: String, "Force'" :: NullOrUndefined (BoxedBoolean) }
```

#### `DeregisterContainerInstanceResponse`

``` purescript
newtype DeregisterContainerInstanceResponse
  = DeregisterContainerInstanceResponse { "ContainerInstance'" :: NullOrUndefined (ContainerInstance) }
```

#### `DeregisterTaskDefinitionRequest`

``` purescript
newtype DeregisterTaskDefinitionRequest
  = DeregisterTaskDefinitionRequest { "TaskDefinition'" :: String }
```

#### `DeregisterTaskDefinitionResponse`

``` purescript
newtype DeregisterTaskDefinitionResponse
  = DeregisterTaskDefinitionResponse { "TaskDefinition'" :: NullOrUndefined (TaskDefinition) }
```

#### `DescribeClustersRequest`

``` purescript
newtype DescribeClustersRequest
  = DescribeClustersRequest { "Clusters'" :: NullOrUndefined (StringList), "Include'" :: NullOrUndefined (ClusterFieldList) }
```

#### `DescribeClustersResponse`

``` purescript
newtype DescribeClustersResponse
  = DescribeClustersResponse { "Clusters'" :: NullOrUndefined (Clusters), "Failures'" :: NullOrUndefined (Failures) }
```

#### `DescribeContainerInstancesRequest`

``` purescript
newtype DescribeContainerInstancesRequest
  = DescribeContainerInstancesRequest { "Cluster'" :: NullOrUndefined (String), "ContainerInstances'" :: StringList }
```

#### `DescribeContainerInstancesResponse`

``` purescript
newtype DescribeContainerInstancesResponse
  = DescribeContainerInstancesResponse { "ContainerInstances'" :: NullOrUndefined (ContainerInstances), "Failures'" :: NullOrUndefined (Failures) }
```

#### `DescribeServicesRequest`

``` purescript
newtype DescribeServicesRequest
  = DescribeServicesRequest { "Cluster'" :: NullOrUndefined (String), "Services'" :: StringList }
```

#### `DescribeServicesResponse`

``` purescript
newtype DescribeServicesResponse
  = DescribeServicesResponse { "Services'" :: NullOrUndefined (Services), "Failures'" :: NullOrUndefined (Failures) }
```

#### `DescribeTaskDefinitionRequest`

``` purescript
newtype DescribeTaskDefinitionRequest
  = DescribeTaskDefinitionRequest { "TaskDefinition'" :: String }
```

#### `DescribeTaskDefinitionResponse`

``` purescript
newtype DescribeTaskDefinitionResponse
  = DescribeTaskDefinitionResponse { "TaskDefinition'" :: NullOrUndefined (TaskDefinition) }
```

#### `DescribeTasksRequest`

``` purescript
newtype DescribeTasksRequest
  = DescribeTasksRequest { "Cluster'" :: NullOrUndefined (String), "Tasks'" :: StringList }
```

#### `DescribeTasksResponse`

``` purescript
newtype DescribeTasksResponse
  = DescribeTasksResponse { "Tasks'" :: NullOrUndefined (Tasks), "Failures'" :: NullOrUndefined (Failures) }
```

#### `DesiredStatus`

``` purescript
newtype DesiredStatus
  = DesiredStatus String
```

#### `Device`

``` purescript
newtype Device
  = Device { "HostPath'" :: String, "ContainerPath'" :: NullOrUndefined (String), "Permissions'" :: NullOrUndefined (DeviceCgroupPermissions) }
```

<p>An object representing a container instance host device.</p>

#### `DeviceCgroupPermission`

``` purescript
newtype DeviceCgroupPermission
  = DeviceCgroupPermission String
```

#### `DeviceCgroupPermissions`

``` purescript
newtype DeviceCgroupPermissions
  = DeviceCgroupPermissions (Array DeviceCgroupPermission)
```

#### `DevicesList`

``` purescript
newtype DevicesList
  = DevicesList (Array Device)
```

#### `DiscoverPollEndpointRequest`

``` purescript
newtype DiscoverPollEndpointRequest
  = DiscoverPollEndpointRequest { "ContainerInstance'" :: NullOrUndefined (String), "Cluster'" :: NullOrUndefined (String) }
```

#### `DiscoverPollEndpointResponse`

``` purescript
newtype DiscoverPollEndpointResponse
  = DiscoverPollEndpointResponse { "Endpoint'" :: NullOrUndefined (String), "TelemetryEndpoint'" :: NullOrUndefined (String) }
```

#### `DockerLabelsMap`

``` purescript
newtype DockerLabelsMap
  = DockerLabelsMap (Map String String)
```

#### `EnvironmentVariables`

``` purescript
newtype EnvironmentVariables
  = EnvironmentVariables (Array KeyValuePair)
```

#### `Failure`

``` purescript
newtype Failure
  = Failure { "Arn'" :: NullOrUndefined (String), "Reason'" :: NullOrUndefined (String) }
```

<p>A failed resource.</p>

#### `Failures`

``` purescript
newtype Failures
  = Failures (Array Failure)
```

#### `HostEntry`

``` purescript
newtype HostEntry
  = HostEntry { "Hostname'" :: String, "IpAddress'" :: String }
```

<p>Hostnames and IP address entries that are added to the <code>/etc/hosts</code> file of a container via the <code>extraHosts</code> parameter of its <a>ContainerDefinition</a>. </p>

#### `HostEntryList`

``` purescript
newtype HostEntryList
  = HostEntryList (Array HostEntry)
```

#### `HostVolumeProperties`

``` purescript
newtype HostVolumeProperties
  = HostVolumeProperties { "SourcePath'" :: NullOrUndefined (String) }
```

<p>Details on a container instance host volume.</p>

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException {  }
```

<p>The specified parameter is invalid. Review the available parameters for the API request.</p>

#### `KernelCapabilities`

``` purescript
newtype KernelCapabilities
  = KernelCapabilities { "Add'" :: NullOrUndefined (StringList), "Drop'" :: NullOrUndefined (StringList) }
```

<p>The Linux capabilities for the container that are added to or dropped from the default configuration provided by Docker. For more information on the default capabilities and the non-default available capabilities, see <a href="https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities">Runtime privilege and Linux capabilities</a> in the <i>Docker run reference</i>. For more detailed information on these Linux capabilities, see the <a href="http://man7.org/linux/man-pages/man7/capabilities.7.html">capabilities(7)</a> Linux manual page.</p>

#### `KeyValuePair`

``` purescript
newtype KeyValuePair
  = KeyValuePair { "Name'" :: NullOrUndefined (String), "Value'" :: NullOrUndefined (String) }
```

<p>A key and value pair object.</p>

#### `LaunchType`

``` purescript
newtype LaunchType
  = LaunchType String
```

#### `LinuxParameters`

``` purescript
newtype LinuxParameters
  = LinuxParameters { "Capabilities'" :: NullOrUndefined (KernelCapabilities), "Devices'" :: NullOrUndefined (DevicesList), "InitProcessEnabled'" :: NullOrUndefined (BoxedBoolean) }
```

<p>Linux-specific options that are applied to the container, such as Linux <a>KernelCapabilities</a>.</p>

#### `ListAttributesRequest`

``` purescript
newtype ListAttributesRequest
  = ListAttributesRequest { "Cluster'" :: NullOrUndefined (String), "TargetType'" :: TargetType, "AttributeName'" :: NullOrUndefined (String), "AttributeValue'" :: NullOrUndefined (String), "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (BoxedInteger) }
```

#### `ListAttributesResponse`

``` purescript
newtype ListAttributesResponse
  = ListAttributesResponse { "Attributes'" :: NullOrUndefined (Attributes), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListClustersRequest`

``` purescript
newtype ListClustersRequest
  = ListClustersRequest { "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (BoxedInteger) }
```

#### `ListClustersResponse`

``` purescript
newtype ListClustersResponse
  = ListClustersResponse { "ClusterArns'" :: NullOrUndefined (StringList), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListContainerInstancesRequest`

``` purescript
newtype ListContainerInstancesRequest
  = ListContainerInstancesRequest { "Cluster'" :: NullOrUndefined (String), "Filter'" :: NullOrUndefined (String), "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (BoxedInteger), "Status'" :: NullOrUndefined (ContainerInstanceStatus) }
```

#### `ListContainerInstancesResponse`

``` purescript
newtype ListContainerInstancesResponse
  = ListContainerInstancesResponse { "ContainerInstanceArns'" :: NullOrUndefined (StringList), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListServicesRequest`

``` purescript
newtype ListServicesRequest
  = ListServicesRequest { "Cluster'" :: NullOrUndefined (String), "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (BoxedInteger), "LaunchType'" :: NullOrUndefined (LaunchType) }
```

#### `ListServicesResponse`

``` purescript
newtype ListServicesResponse
  = ListServicesResponse { "ServiceArns'" :: NullOrUndefined (StringList), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListTaskDefinitionFamiliesRequest`

``` purescript
newtype ListTaskDefinitionFamiliesRequest
  = ListTaskDefinitionFamiliesRequest { "FamilyPrefix'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (TaskDefinitionFamilyStatus), "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (BoxedInteger) }
```

#### `ListTaskDefinitionFamiliesResponse`

``` purescript
newtype ListTaskDefinitionFamiliesResponse
  = ListTaskDefinitionFamiliesResponse { "Families'" :: NullOrUndefined (StringList), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListTaskDefinitionsRequest`

``` purescript
newtype ListTaskDefinitionsRequest
  = ListTaskDefinitionsRequest { "FamilyPrefix'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (TaskDefinitionStatus), "Sort'" :: NullOrUndefined (SortOrder), "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (BoxedInteger) }
```

#### `ListTaskDefinitionsResponse`

``` purescript
newtype ListTaskDefinitionsResponse
  = ListTaskDefinitionsResponse { "TaskDefinitionArns'" :: NullOrUndefined (StringList), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListTasksRequest`

``` purescript
newtype ListTasksRequest
  = ListTasksRequest { "Cluster'" :: NullOrUndefined (String), "ContainerInstance'" :: NullOrUndefined (String), "Family'" :: NullOrUndefined (String), "NextToken'" :: NullOrUndefined (String), "MaxResults'" :: NullOrUndefined (BoxedInteger), "StartedBy'" :: NullOrUndefined (String), "ServiceName'" :: NullOrUndefined (String), "DesiredStatus'" :: NullOrUndefined (DesiredStatus), "LaunchType'" :: NullOrUndefined (LaunchType) }
```

#### `ListTasksResponse`

``` purescript
newtype ListTasksResponse
  = ListTasksResponse { "TaskArns'" :: NullOrUndefined (StringList), "NextToken'" :: NullOrUndefined (String) }
```

#### `LoadBalancer`

``` purescript
newtype LoadBalancer
  = LoadBalancer { "TargetGroupArn'" :: NullOrUndefined (String), "LoadBalancerName'" :: NullOrUndefined (String), "ContainerName'" :: NullOrUndefined (String), "ContainerPort'" :: NullOrUndefined (BoxedInteger) }
```

<p>Details on a load balancer that is used with a service.</p>

#### `LoadBalancers`

``` purescript
newtype LoadBalancers
  = LoadBalancers (Array LoadBalancer)
```

#### `LogConfiguration`

``` purescript
newtype LogConfiguration
  = LogConfiguration { "LogDriver'" :: LogDriver, "Options'" :: NullOrUndefined (LogConfigurationOptionsMap) }
```

<p>Log configuration options to send to a custom log driver for the container.</p>

#### `LogConfigurationOptionsMap`

``` purescript
newtype LogConfigurationOptionsMap
  = LogConfigurationOptionsMap (Map String String)
```

#### `LogDriver`

``` purescript
newtype LogDriver
  = LogDriver String
```

#### `MissingVersionException`

``` purescript
newtype MissingVersionException
  = MissingVersionException {  }
```

<p>Amazon ECS is unable to determine the current version of the Amazon ECS container agent on the container instance and does not have enough information to proceed with an update. This could be because the agent running on the container instance is an older or custom version that does not use our version information.</p>

#### `MountPoint`

``` purescript
newtype MountPoint
  = MountPoint { "SourceVolume'" :: NullOrUndefined (String), "ContainerPath'" :: NullOrUndefined (String), "ReadOnly'" :: NullOrUndefined (BoxedBoolean) }
```

<p>Details on a volume mount point that is used in a container definition.</p>

#### `MountPointList`

``` purescript
newtype MountPointList
  = MountPointList (Array MountPoint)
```

#### `NetworkBinding`

``` purescript
newtype NetworkBinding
  = NetworkBinding { "BindIP'" :: NullOrUndefined (String), "ContainerPort'" :: NullOrUndefined (BoxedInteger), "HostPort'" :: NullOrUndefined (BoxedInteger), "Protocol'" :: NullOrUndefined (TransportProtocol) }
```

<p>Details on the network bindings between a container and its host container instance. After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.</p>

#### `NetworkBindings`

``` purescript
newtype NetworkBindings
  = NetworkBindings (Array NetworkBinding)
```

#### `NetworkConfiguration`

``` purescript
newtype NetworkConfiguration
  = NetworkConfiguration { "AwsvpcConfiguration'" :: NullOrUndefined (AwsVpcConfiguration) }
```

<p>An object representing the network configuration for a task or service.</p>

#### `NetworkInterface`

``` purescript
newtype NetworkInterface
  = NetworkInterface { "AttachmentId'" :: NullOrUndefined (String), "PrivateIpv4Address'" :: NullOrUndefined (String), "Ipv6Address'" :: NullOrUndefined (String) }
```

<p>An object representing the Elastic Network Interface for tasks that use the <code>awsvpc</code> network mode.</p>

#### `NetworkInterfaces`

``` purescript
newtype NetworkInterfaces
  = NetworkInterfaces (Array NetworkInterface)
```

#### `NetworkMode`

``` purescript
newtype NetworkMode
  = NetworkMode String
```

#### `NoUpdateAvailableException`

``` purescript
newtype NoUpdateAvailableException
  = NoUpdateAvailableException {  }
```

<p>There is no update available for this Amazon ECS container agent. This could be because the agent is already running the latest version, or it is so old that there is no update path to the current version.</p>

#### `PlacementConstraint`

``` purescript
newtype PlacementConstraint
  = PlacementConstraint { "Type'" :: NullOrUndefined (PlacementConstraintType), "Expression'" :: NullOrUndefined (String) }
```

<p>An object representing a constraint on task placement. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `PlacementConstraintType`

``` purescript
newtype PlacementConstraintType
  = PlacementConstraintType String
```

#### `PlacementConstraints`

``` purescript
newtype PlacementConstraints
  = PlacementConstraints (Array PlacementConstraint)
```

#### `PlacementStrategies`

``` purescript
newtype PlacementStrategies
  = PlacementStrategies (Array PlacementStrategy)
```

#### `PlacementStrategy`

``` purescript
newtype PlacementStrategy
  = PlacementStrategy { "Type'" :: NullOrUndefined (PlacementStrategyType), "Field'" :: NullOrUndefined (String) }
```

<p>The task placement strategy for a task or service. For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html">Task Placement Strategies</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `PlacementStrategyType`

``` purescript
newtype PlacementStrategyType
  = PlacementStrategyType String
```

#### `PlatformTaskDefinitionIncompatibilityException`

``` purescript
newtype PlatformTaskDefinitionIncompatibilityException
  = PlatformTaskDefinitionIncompatibilityException {  }
```

<p>The specified platform version does not satisfy the task definition’s required capabilities.</p>

#### `PlatformUnknownException`

``` purescript
newtype PlatformUnknownException
  = PlatformUnknownException {  }
```

<p>The specified platform version does not exist.</p>

#### `PortMapping`

``` purescript
newtype PortMapping
  = PortMapping { "ContainerPort'" :: NullOrUndefined (BoxedInteger), "HostPort'" :: NullOrUndefined (BoxedInteger), "Protocol'" :: NullOrUndefined (TransportProtocol) }
```

<p>Port mappings allow containers to access ports on the host container instance to send or receive traffic. Port mappings are specified as part of the container definition.</p> <p>If using containers in a task with the <code>awsvpc</code> or <code>host</code> network mode, exposed ports should be specified using <code>containerPort</code>. The <code>hostPort</code> can be left blank or it must be the same value as the <code>containerPort</code>.</p> <p>After a task reaches the <code>RUNNING</code> status, manual and automatic host and container port assignments are visible in the <code>networkBindings</code> section of <a>DescribeTasks</a> API responses.</p>

#### `PortMappingList`

``` purescript
newtype PortMappingList
  = PortMappingList (Array PortMapping)
```

#### `PutAttributesRequest`

``` purescript
newtype PutAttributesRequest
  = PutAttributesRequest { "Cluster'" :: NullOrUndefined (String), "Attributes'" :: Attributes }
```

#### `PutAttributesResponse`

``` purescript
newtype PutAttributesResponse
  = PutAttributesResponse { "Attributes'" :: NullOrUndefined (Attributes) }
```

#### `RegisterContainerInstanceRequest`

``` purescript
newtype RegisterContainerInstanceRequest
  = RegisterContainerInstanceRequest { "Cluster'" :: NullOrUndefined (String), "InstanceIdentityDocument'" :: NullOrUndefined (String), "InstanceIdentityDocumentSignature'" :: NullOrUndefined (String), "TotalResources'" :: NullOrUndefined (Resources), "VersionInfo'" :: NullOrUndefined (VersionInfo), "ContainerInstanceArn'" :: NullOrUndefined (String), "Attributes'" :: NullOrUndefined (Attributes) }
```

#### `RegisterContainerInstanceResponse`

``` purescript
newtype RegisterContainerInstanceResponse
  = RegisterContainerInstanceResponse { "ContainerInstance'" :: NullOrUndefined (ContainerInstance) }
```

#### `RegisterTaskDefinitionRequest`

``` purescript
newtype RegisterTaskDefinitionRequest
  = RegisterTaskDefinitionRequest { "Family'" :: String, "TaskRoleArn'" :: NullOrUndefined (String), "ExecutionRoleArn'" :: NullOrUndefined (String), "NetworkMode'" :: NullOrUndefined (NetworkMode), "ContainerDefinitions'" :: ContainerDefinitions, "Volumes'" :: NullOrUndefined (VolumeList), "PlacementConstraints'" :: NullOrUndefined (TaskDefinitionPlacementConstraints), "RequiresCompatibilities'" :: NullOrUndefined (CompatibilityList), "Cpu'" :: NullOrUndefined (String), "Memory'" :: NullOrUndefined (String) }
```

#### `RegisterTaskDefinitionResponse`

``` purescript
newtype RegisterTaskDefinitionResponse
  = RegisterTaskDefinitionResponse { "TaskDefinition'" :: NullOrUndefined (TaskDefinition) }
```

#### `RequiresAttributes`

``` purescript
newtype RequiresAttributes
  = RequiresAttributes (Array Attribute)
```

#### `Resource`

``` purescript
newtype Resource
  = Resource { "Name'" :: NullOrUndefined (String), "Type'" :: NullOrUndefined (String), "DoubleValue'" :: NullOrUndefined (Number), "LongValue'" :: NullOrUndefined (Number), "IntegerValue'" :: NullOrUndefined (Int), "StringSetValue'" :: NullOrUndefined (StringList) }
```

<p>Describes the resources available for a container instance.</p>

#### `Resources`

``` purescript
newtype Resources
  = Resources (Array Resource)
```

#### `RunTaskRequest`

``` purescript
newtype RunTaskRequest
  = RunTaskRequest { "Cluster'" :: NullOrUndefined (String), "TaskDefinition'" :: String, "Overrides'" :: NullOrUndefined (TaskOverride), "Count'" :: NullOrUndefined (BoxedInteger), "StartedBy'" :: NullOrUndefined (String), "Group'" :: NullOrUndefined (String), "PlacementConstraints'" :: NullOrUndefined (PlacementConstraints), "PlacementStrategy'" :: NullOrUndefined (PlacementStrategies), "LaunchType'" :: NullOrUndefined (LaunchType), "PlatformVersion'" :: NullOrUndefined (String), "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration) }
```

#### `RunTaskResponse`

``` purescript
newtype RunTaskResponse
  = RunTaskResponse { "Tasks'" :: NullOrUndefined (Tasks), "Failures'" :: NullOrUndefined (Failures) }
```

#### `ServerException`

``` purescript
newtype ServerException
  = ServerException { "Message'" :: NullOrUndefined (String) }
```

<p>These errors are usually caused by a server issue.</p>

#### `Service`

``` purescript
newtype Service
  = Service { "ServiceArn'" :: NullOrUndefined (String), "ServiceName'" :: NullOrUndefined (String), "ClusterArn'" :: NullOrUndefined (String), "LoadBalancers'" :: NullOrUndefined (LoadBalancers), "Status'" :: NullOrUndefined (String), "DesiredCount'" :: NullOrUndefined (Int), "RunningCount'" :: NullOrUndefined (Int), "PendingCount'" :: NullOrUndefined (Int), "LaunchType'" :: NullOrUndefined (LaunchType), "PlatformVersion'" :: NullOrUndefined (String), "TaskDefinition'" :: NullOrUndefined (String), "DeploymentConfiguration'" :: NullOrUndefined (DeploymentConfiguration), "Deployments'" :: NullOrUndefined (Deployments), "RoleArn'" :: NullOrUndefined (String), "Events'" :: NullOrUndefined (ServiceEvents), "CreatedAt'" :: NullOrUndefined (Number), "PlacementConstraints'" :: NullOrUndefined (PlacementConstraints), "PlacementStrategy'" :: NullOrUndefined (PlacementStrategies), "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration), "HealthCheckGracePeriodSeconds'" :: NullOrUndefined (BoxedInteger) }
```

<p>Details on a service within a cluster</p>

#### `ServiceEvent`

``` purescript
newtype ServiceEvent
  = ServiceEvent { "Id'" :: NullOrUndefined (String), "CreatedAt'" :: NullOrUndefined (Number), "Message'" :: NullOrUndefined (String) }
```

<p>Details on an event associated with a service.</p>

#### `ServiceEvents`

``` purescript
newtype ServiceEvents
  = ServiceEvents (Array ServiceEvent)
```

#### `ServiceNotActiveException`

``` purescript
newtype ServiceNotActiveException
  = ServiceNotActiveException {  }
```

<p>The specified service is not active. You can't update a service that is inactive. If you have previously deleted a service, you can re-create it with <a>CreateService</a>.</p>

#### `ServiceNotFoundException`

``` purescript
newtype ServiceNotFoundException
  = ServiceNotFoundException {  }
```

<p>The specified service could not be found. You can view your available services with <a>ListServices</a>. Amazon ECS services are cluster-specific and region-specific.</p>

#### `Services`

``` purescript
newtype Services
  = Services (Array Service)
```

#### `SortOrder`

``` purescript
newtype SortOrder
  = SortOrder String
```

#### `StartTaskRequest`

``` purescript
newtype StartTaskRequest
  = StartTaskRequest { "Cluster'" :: NullOrUndefined (String), "TaskDefinition'" :: String, "Overrides'" :: NullOrUndefined (TaskOverride), "ContainerInstances'" :: StringList, "StartedBy'" :: NullOrUndefined (String), "Group'" :: NullOrUndefined (String), "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration) }
```

#### `StartTaskResponse`

``` purescript
newtype StartTaskResponse
  = StartTaskResponse { "Tasks'" :: NullOrUndefined (Tasks), "Failures'" :: NullOrUndefined (Failures) }
```

#### `Statistics`

``` purescript
newtype Statistics
  = Statistics (Array KeyValuePair)
```

#### `StopTaskRequest`

``` purescript
newtype StopTaskRequest
  = StopTaskRequest { "Cluster'" :: NullOrUndefined (String), "Task'" :: String, "Reason'" :: NullOrUndefined (String) }
```

#### `StopTaskResponse`

``` purescript
newtype StopTaskResponse
  = StopTaskResponse { "Task'" :: NullOrUndefined (Task) }
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

#### `SubmitContainerStateChangeRequest`

``` purescript
newtype SubmitContainerStateChangeRequest
  = SubmitContainerStateChangeRequest { "Cluster'" :: NullOrUndefined (String), "Task'" :: NullOrUndefined (String), "ContainerName'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (String), "ExitCode'" :: NullOrUndefined (BoxedInteger), "Reason'" :: NullOrUndefined (String), "NetworkBindings'" :: NullOrUndefined (NetworkBindings) }
```

#### `SubmitContainerStateChangeResponse`

``` purescript
newtype SubmitContainerStateChangeResponse
  = SubmitContainerStateChangeResponse { "Acknowledgment'" :: NullOrUndefined (String) }
```

#### `SubmitTaskStateChangeRequest`

``` purescript
newtype SubmitTaskStateChangeRequest
  = SubmitTaskStateChangeRequest { "Cluster'" :: NullOrUndefined (String), "Task'" :: NullOrUndefined (String), "Status'" :: NullOrUndefined (String), "Reason'" :: NullOrUndefined (String), "Containers'" :: NullOrUndefined (ContainerStateChanges), "Attachments'" :: NullOrUndefined (AttachmentStateChanges), "PullStartedAt'" :: NullOrUndefined (Number), "PullStoppedAt'" :: NullOrUndefined (Number), "ExecutionStoppedAt'" :: NullOrUndefined (Number) }
```

#### `SubmitTaskStateChangeResponse`

``` purescript
newtype SubmitTaskStateChangeResponse
  = SubmitTaskStateChangeResponse { "Acknowledgment'" :: NullOrUndefined (String) }
```

#### `TargetNotFoundException`

``` purescript
newtype TargetNotFoundException
  = TargetNotFoundException {  }
```

<p>The specified target could not be found. You can view your available container instances with <a>ListContainerInstances</a>. Amazon ECS container instances are cluster-specific and region-specific.</p>

#### `TargetType`

``` purescript
newtype TargetType
  = TargetType String
```

#### `Task`

``` purescript
newtype Task
  = Task { "TaskArn'" :: NullOrUndefined (String), "ClusterArn'" :: NullOrUndefined (String), "TaskDefinitionArn'" :: NullOrUndefined (String), "ContainerInstanceArn'" :: NullOrUndefined (String), "Overrides'" :: NullOrUndefined (TaskOverride), "LastStatus'" :: NullOrUndefined (String), "DesiredStatus'" :: NullOrUndefined (String), "Cpu'" :: NullOrUndefined (String), "Memory'" :: NullOrUndefined (String), "Containers'" :: NullOrUndefined (Containers), "StartedBy'" :: NullOrUndefined (String), "Version'" :: NullOrUndefined (Number), "StoppedReason'" :: NullOrUndefined (String), "Connectivity'" :: NullOrUndefined (Connectivity), "ConnectivityAt'" :: NullOrUndefined (Number), "PullStartedAt'" :: NullOrUndefined (Number), "PullStoppedAt'" :: NullOrUndefined (Number), "ExecutionStoppedAt'" :: NullOrUndefined (Number), "CreatedAt'" :: NullOrUndefined (Number), "StartedAt'" :: NullOrUndefined (Number), "StoppingAt'" :: NullOrUndefined (Number), "StoppedAt'" :: NullOrUndefined (Number), "Group'" :: NullOrUndefined (String), "LaunchType'" :: NullOrUndefined (LaunchType), "PlatformVersion'" :: NullOrUndefined (String), "Attachments'" :: NullOrUndefined (Attachments) }
```

<p>Details on a task in a cluster.</p>

#### `TaskDefinition`

``` purescript
newtype TaskDefinition
  = TaskDefinition { "TaskDefinitionArn'" :: NullOrUndefined (String), "ContainerDefinitions'" :: NullOrUndefined (ContainerDefinitions), "Family'" :: NullOrUndefined (String), "TaskRoleArn'" :: NullOrUndefined (String), "ExecutionRoleArn'" :: NullOrUndefined (String), "NetworkMode'" :: NullOrUndefined (NetworkMode), "Revision'" :: NullOrUndefined (Int), "Volumes'" :: NullOrUndefined (VolumeList), "Status'" :: NullOrUndefined (TaskDefinitionStatus), "RequiresAttributes'" :: NullOrUndefined (RequiresAttributes), "PlacementConstraints'" :: NullOrUndefined (TaskDefinitionPlacementConstraints), "Compatibilities'" :: NullOrUndefined (CompatibilityList), "RequiresCompatibilities'" :: NullOrUndefined (CompatibilityList), "Cpu'" :: NullOrUndefined (String), "Memory'" :: NullOrUndefined (String) }
```

<p>Details of a task definition.</p>

#### `TaskDefinitionFamilyStatus`

``` purescript
newtype TaskDefinitionFamilyStatus
  = TaskDefinitionFamilyStatus String
```

#### `TaskDefinitionPlacementConstraint`

``` purescript
newtype TaskDefinitionPlacementConstraint
  = TaskDefinitionPlacementConstraint { "Type'" :: NullOrUndefined (TaskDefinitionPlacementConstraintType), "Expression'" :: NullOrUndefined (String) }
```

<p>An object representing a constraint on task placement in the task definition.</p> <p>If you are using the Fargate launch type, task placement contraints are not supported.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html">Task Placement Constraints</a> in the <i>Amazon Elastic Container Service Developer Guide</i>.</p>

#### `TaskDefinitionPlacementConstraintType`

``` purescript
newtype TaskDefinitionPlacementConstraintType
  = TaskDefinitionPlacementConstraintType String
```

#### `TaskDefinitionPlacementConstraints`

``` purescript
newtype TaskDefinitionPlacementConstraints
  = TaskDefinitionPlacementConstraints (Array TaskDefinitionPlacementConstraint)
```

#### `TaskDefinitionStatus`

``` purescript
newtype TaskDefinitionStatus
  = TaskDefinitionStatus String
```

#### `TaskOverride`

``` purescript
newtype TaskOverride
  = TaskOverride { "ContainerOverrides'" :: NullOrUndefined (ContainerOverrides), "TaskRoleArn'" :: NullOrUndefined (String), "ExecutionRoleArn'" :: NullOrUndefined (String) }
```

<p>The overrides associated with a task.</p>

#### `Tasks`

``` purescript
newtype Tasks
  = Tasks (Array Task)
```

#### `TransportProtocol`

``` purescript
newtype TransportProtocol
  = TransportProtocol String
```

#### `Ulimit`

``` purescript
newtype Ulimit
  = Ulimit { "Name'" :: UlimitName, "SoftLimit'" :: Int, "HardLimit'" :: Int }
```

<p>The <code>ulimit</code> settings to pass to the container.</p>

#### `UlimitList`

``` purescript
newtype UlimitList
  = UlimitList (Array Ulimit)
```

#### `UlimitName`

``` purescript
newtype UlimitName
  = UlimitName String
```

#### `UnsupportedFeatureException`

``` purescript
newtype UnsupportedFeatureException
  = UnsupportedFeatureException {  }
```

<p>The specified task is not supported in this region.</p>

#### `UpdateContainerAgentRequest`

``` purescript
newtype UpdateContainerAgentRequest
  = UpdateContainerAgentRequest { "Cluster'" :: NullOrUndefined (String), "ContainerInstance'" :: String }
```

#### `UpdateContainerAgentResponse`

``` purescript
newtype UpdateContainerAgentResponse
  = UpdateContainerAgentResponse { "ContainerInstance'" :: NullOrUndefined (ContainerInstance) }
```

#### `UpdateContainerInstancesStateRequest`

``` purescript
newtype UpdateContainerInstancesStateRequest
  = UpdateContainerInstancesStateRequest { "Cluster'" :: NullOrUndefined (String), "ContainerInstances'" :: StringList, "Status'" :: ContainerInstanceStatus }
```

#### `UpdateContainerInstancesStateResponse`

``` purescript
newtype UpdateContainerInstancesStateResponse
  = UpdateContainerInstancesStateResponse { "ContainerInstances'" :: NullOrUndefined (ContainerInstances), "Failures'" :: NullOrUndefined (Failures) }
```

#### `UpdateInProgressException`

``` purescript
newtype UpdateInProgressException
  = UpdateInProgressException {  }
```

<p>There is already a current Amazon ECS container agent update in progress on the specified container instance. If the container agent becomes disconnected while it is in a transitional stage, such as <code>PENDING</code> or <code>STAGING</code>, the update process can get stuck in that state. However, when the agent reconnects, it resumes where it stopped previously.</p>

#### `UpdateServiceRequest`

``` purescript
newtype UpdateServiceRequest
  = UpdateServiceRequest { "Cluster'" :: NullOrUndefined (String), "Service'" :: String, "DesiredCount'" :: NullOrUndefined (BoxedInteger), "TaskDefinition'" :: NullOrUndefined (String), "DeploymentConfiguration'" :: NullOrUndefined (DeploymentConfiguration), "NetworkConfiguration'" :: NullOrUndefined (NetworkConfiguration), "PlatformVersion'" :: NullOrUndefined (String), "ForceNewDeployment'" :: NullOrUndefined (Boolean), "HealthCheckGracePeriodSeconds'" :: NullOrUndefined (BoxedInteger) }
```

#### `UpdateServiceResponse`

``` purescript
newtype UpdateServiceResponse
  = UpdateServiceResponse { "Service'" :: NullOrUndefined (Service) }
```

#### `VersionInfo`

``` purescript
newtype VersionInfo
  = VersionInfo { "AgentVersion'" :: NullOrUndefined (String), "AgentHash'" :: NullOrUndefined (String), "DockerVersion'" :: NullOrUndefined (String) }
```

<p>The Docker and Amazon ECS container agent version information about a container instance.</p>

#### `Volume`

``` purescript
newtype Volume
  = Volume { "Name'" :: NullOrUndefined (String), "Host'" :: NullOrUndefined (HostVolumeProperties) }
```

<p>A data volume used in a task definition.</p>

#### `VolumeFrom`

``` purescript
newtype VolumeFrom
  = VolumeFrom { "SourceContainer'" :: NullOrUndefined (String), "ReadOnly'" :: NullOrUndefined (BoxedBoolean) }
```

<p>Details on a data volume from another container in the same task definition.</p>

#### `VolumeFromList`

``` purescript
newtype VolumeFromList
  = VolumeFromList (Array VolumeFrom)
```

#### `VolumeList`

``` purescript
newtype VolumeList
  = VolumeList (Array Volume)
```


