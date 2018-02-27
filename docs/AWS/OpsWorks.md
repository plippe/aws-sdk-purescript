## Module AWS.OpsWorks

<fullname>AWS OpsWorks</fullname> <p>Welcome to the <i>AWS OpsWorks Stacks API Reference</i>. This guide provides descriptions, syntax, and usage examples for AWS OpsWorks Stacks actions and data types, including common parameters and error codes. </p> <p>AWS OpsWorks Stacks is an application management service that provides an integrated experience for overseeing the complete application lifecycle. For information about this product, go to the <a href="http://aws.amazon.com/opsworks/">AWS OpsWorks</a> details page. </p> <p> <b>SDKs and CLI</b> </p> <p>The most common way to use the AWS OpsWorks Stacks API is by using the AWS Command Line Interface (CLI) or by using one of the AWS SDKs to implement applications in your preferred language. For more information, see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html">AWS CLI</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html">AWS SDK for Java</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm">AWS SDK for .NET</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html">AWS SDK for PHP 2</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/sdkforruby/api/">AWS SDK for Ruby</a> </p> </li> <li> <p> <a href="http://aws.amazon.com/documentation/sdkforjavascript/">AWS SDK for Node.js</a> </p> </li> <li> <p> <a href="http://docs.pythonboto.org/en/latest/ref/opsworks.html">AWS SDK for Python(Boto)</a> </p> </li> </ul> <p> <b>Endpoints</b> </p> <p>AWS OpsWorks Stacks supports the following endpoints, all HTTPS. You must connect to one of the following endpoints. Stacks can only be accessed or managed within the endpoint in which they are created.</p> <ul> <li> <p>opsworks.us-east-1.amazonaws.com</p> </li> <li> <p>opsworks.us-east-2.amazonaws.com</p> </li> <li> <p>opsworks.us-west-1.amazonaws.com</p> </li> <li> <p>opsworks.us-west-2.amazonaws.com</p> </li> <li> <p>opsworks.ca-central-1.amazonaws.com (API only; not available in the AWS console)</p> </li> <li> <p>opsworks.eu-west-1.amazonaws.com</p> </li> <li> <p>opsworks.eu-west-2.amazonaws.com</p> </li> <li> <p>opsworks.eu-west-3.amazonaws.com</p> </li> <li> <p>opsworks.eu-central-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-northeast-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-northeast-2.amazonaws.com</p> </li> <li> <p>opsworks.ap-south-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-southeast-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-southeast-2.amazonaws.com</p> </li> <li> <p>opsworks.sa-east-1.amazonaws.com</p> </li> </ul> <p> <b>Chef Versions</b> </p> <p>When you call <a>CreateStack</a>, <a>CloneStack</a>, or <a>UpdateStack</a> we recommend you use the <code>ConfigurationManager</code> parameter to specify the Chef version. The recommended and default value for Linux stacks is currently 12. Windows stacks use Chef 12.2. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html">Chef Versions</a>.</p> <note> <p>You can specify Chef 12, 11.10, or 11.4 for your Linux stack. We recommend migrating your existing Linux stacks to Chef 12 as soon as possible.</p> </note>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `assignInstance`

``` purescript
assignInstance :: forall eff. AssignInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Assign a registered instance to a layer.</p> <ul> <li> <p>You can assign registered on-premises instances to any layer type.</p> </li> <li> <p>You can assign registered Amazon EC2 instances only to custom layers.</p> </li> <li> <p>You cannot use this action with instances that were created with AWS OpsWorks Stacks.</p> </li> </ul> <p> <b>Required Permissions</b>: To use this action, an AWS Identity and Access Management (IAM) user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `assignVolume`

``` purescript
assignVolume :: forall eff. AssignVolumeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Assigns one of the stack's registered Amazon EBS volumes to a specified instance. The volume must first be registered with the stack by calling <a>RegisterVolume</a>. After you register the volume, you must call <a>UpdateVolume</a> to specify a mount point before calling <code>AssignVolume</code>. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `associateElasticIp`

``` purescript
associateElasticIp :: forall eff. AssociateElasticIpRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Associates one of the stack's registered Elastic IP addresses with a specified instance. The address must first be registered with the stack by calling <a>RegisterElasticIp</a>. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `attachElasticLoadBalancer`

``` purescript
attachElasticLoadBalancer :: forall eff. AttachElasticLoadBalancerRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Attaches an Elastic Load Balancing load balancer to a specified layer. AWS OpsWorks Stacks does not support Application Load Balancer. You can only use Classic Load Balancer with AWS OpsWorks Stacks. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/layers-elb.html">Elastic Load Balancing</a>.</p> <note> <p>You must create the Elastic Load Balancing instance separately, by using the Elastic Load Balancing console, API, or CLI. For more information, see <a href="http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/Welcome.html"> Elastic Load Balancing Developer Guide</a>.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `cloneStack`

``` purescript
cloneStack :: forall eff. CloneStackRequest -> Aff (err :: RequestError | eff) CloneStackResult
```

<p>Creates a clone of a specified stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-cloning.html">Clone a Stack</a>. By default, all parameters are set to the values used by the parent stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `createApp`

``` purescript
createApp :: forall eff. CreateAppRequest -> Aff (err :: RequestError | eff) CreateAppResult
```

<p>Creates an app for a specified stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html">Creating Apps</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `createDeployment`

``` purescript
createDeployment :: forall eff. CreateDeploymentRequest -> Aff (err :: RequestError | eff) CreateDeploymentResult
```

<p>Runs deployment or stack commands. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-deploying.html">Deploying Apps</a> and <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-commands.html">Run Stack Commands</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `createInstance`

``` purescript
createInstance :: forall eff. CreateInstanceRequest -> Aff (err :: RequestError | eff) CreateInstanceResult
```

<p>Creates an instance in a specified stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html">Adding an Instance to a Layer</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `createLayer`

``` purescript
createLayer :: forall eff. CreateLayerRequest -> Aff (err :: RequestError | eff) CreateLayerResult
```

<p>Creates a layer. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-create.html">How to Create a Layer</a>.</p> <note> <p>You should use <b>CreateLayer</b> for noncustom layer types such as PHP App Server only if the stack does not have an existing layer of that type. A stack can have at most one instance of each noncustom layer; if you attempt to create a second instance, <b>CreateLayer</b> fails. A stack can have an arbitrary number of custom layers, so you can call <b>CreateLayer</b> as many times as you like for that layer type.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `createStack`

``` purescript
createStack :: forall eff. CreateStackRequest -> Aff (err :: RequestError | eff) CreateStackResult
```

<p>Creates a new stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-edit.html">Create a New Stack</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `createUserProfile`

``` purescript
createUserProfile :: forall eff. CreateUserProfileRequest -> Aff (err :: RequestError | eff) CreateUserProfileResult
```

<p>Creates a new user profile.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deleteApp`

``` purescript
deleteApp :: forall eff. DeleteAppRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a specified app.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deleteInstance`

``` purescript
deleteInstance :: forall eff. DeleteInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a specified instance, which terminates the associated Amazon EC2 instance. You must stop an instance before you can delete it.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-delete.html">Deleting Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deleteLayer`

``` purescript
deleteLayer :: forall eff. DeleteLayerRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a specified layer. You must first stop and then delete all associated instances or unassign registered instances. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-delete.html">How to Delete a Layer</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deleteStack`

``` purescript
deleteStack :: forall eff. DeleteStackRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a specified stack. You must first delete all instances, layers, and apps or deregister registered instances. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-shutting.html">Shut Down a Stack</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deleteUserProfile`

``` purescript
deleteUserProfile :: forall eff. DeleteUserProfileRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes a user profile.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deregisterEcsCluster`

``` purescript
deregisterEcsCluster :: forall eff. DeregisterEcsClusterRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deregisters a specified Amazon ECS cluster from a stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html#workinglayers-ecscluster-delete"> Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html</a>.</p>

#### `deregisterElasticIp`

``` purescript
deregisterElasticIp :: forall eff. DeregisterElasticIpRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deregisters a specified Elastic IP address. The address can then be registered by another stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deregisterInstance`

``` purescript
deregisterInstance :: forall eff. DeregisterInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deregister a registered Amazon EC2 or on-premises instance. This action removes the instance from the stack and returns it to your control. This action can not be used with instances that were created with AWS OpsWorks Stacks.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deregisterRdsDbInstance`

``` purescript
deregisterRdsDbInstance :: forall eff. DeregisterRdsDbInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deregisters an Amazon RDS instance.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `deregisterVolume`

``` purescript
deregisterVolume :: forall eff. DeregisterVolumeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Deregisters an Amazon EBS volume. The volume can then be registered by another stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeAgentVersions`

``` purescript
describeAgentVersions :: forall eff. DescribeAgentVersionsRequest -> Aff (err :: RequestError | eff) DescribeAgentVersionsResult
```

<p>Describes the available AWS OpsWorks Stacks agent versions. You must specify a stack ID or a configuration manager. <code>DescribeAgentVersions</code> returns a list of available agent versions for the specified stack or configuration manager.</p>

#### `describeApps`

``` purescript
describeApps :: forall eff. DescribeAppsRequest -> Aff (err :: RequestError | eff) DescribeAppsResult
```

<p>Requests a description of a specified set of apps.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeCommands`

``` purescript
describeCommands :: forall eff. DescribeCommandsRequest -> Aff (err :: RequestError | eff) DescribeCommandsResult
```

<p>Describes the results of specified commands.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeDeployments`

``` purescript
describeDeployments :: forall eff. DescribeDeploymentsRequest -> Aff (err :: RequestError | eff) DescribeDeploymentsResult
```

<p>Requests a description of a specified set of deployments.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeEcsClusters`

``` purescript
describeEcsClusters :: forall eff. DescribeEcsClustersRequest -> Aff (err :: RequestError | eff) DescribeEcsClustersResult
```

<p>Describes Amazon ECS clusters that are registered with a stack. If you specify only a stack ID, you can use the <code>MaxResults</code> and <code>NextToken</code> parameters to paginate the response. However, AWS OpsWorks Stacks currently supports only one cluster per layer, so the result set has a maximum of one element.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permission. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p> <p>This call accepts only one resource-identifying parameter.</p>

#### `describeElasticIps`

``` purescript
describeElasticIps :: forall eff. DescribeElasticIpsRequest -> Aff (err :: RequestError | eff) DescribeElasticIpsResult
```

<p>Describes <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html">Elastic IP addresses</a>.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeElasticLoadBalancers`

``` purescript
describeElasticLoadBalancers :: forall eff. DescribeElasticLoadBalancersRequest -> Aff (err :: RequestError | eff) DescribeElasticLoadBalancersResult
```

<p>Describes a stack's Elastic Load Balancing instances.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeInstances`

``` purescript
describeInstances :: forall eff. DescribeInstancesRequest -> Aff (err :: RequestError | eff) DescribeInstancesResult
```

<p>Requests a description of a set of instances.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeLayers`

``` purescript
describeLayers :: forall eff. DescribeLayersRequest -> Aff (err :: RequestError | eff) DescribeLayersResult
```

<p>Requests a description of one or more layers in a specified stack.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeLoadBasedAutoScaling`

``` purescript
describeLoadBasedAutoScaling :: forall eff. DescribeLoadBasedAutoScalingRequest -> Aff (err :: RequestError | eff) DescribeLoadBasedAutoScalingResult
```

<p>Describes load-based auto scaling configurations for specified layers.</p> <note> <p>You must specify at least one of the parameters.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeMyUserProfile`

``` purescript
describeMyUserProfile :: forall eff. Aff (err :: RequestError | eff) DescribeMyUserProfileResult
```

<p>Describes a user's SSH information.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have self-management enabled or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeOperatingSystems`

``` purescript
describeOperatingSystems :: forall eff. Aff (err :: RequestError | eff) DescribeOperatingSystemsResponse
```

<p>Describes the operating systems that are supported by AWS OpsWorks Stacks.</p>

#### `describePermissions`

``` purescript
describePermissions :: forall eff. DescribePermissionsRequest -> Aff (err :: RequestError | eff) DescribePermissionsResult
```

<p>Describes the permissions for a specified stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeRaidArrays`

``` purescript
describeRaidArrays :: forall eff. DescribeRaidArraysRequest -> Aff (err :: RequestError | eff) DescribeRaidArraysResult
```

<p>Describe an instance's RAID arrays.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeRdsDbInstances`

``` purescript
describeRdsDbInstances :: forall eff. DescribeRdsDbInstancesRequest -> Aff (err :: RequestError | eff) DescribeRdsDbInstancesResult
```

<p>Describes Amazon RDS instances.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p> <p>This call accepts only one resource-identifying parameter.</p>

#### `describeServiceErrors`

``` purescript
describeServiceErrors :: forall eff. DescribeServiceErrorsRequest -> Aff (err :: RequestError | eff) DescribeServiceErrorsResult
```

<p>Describes AWS OpsWorks Stacks service errors.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p> <p>This call accepts only one resource-identifying parameter.</p>

#### `describeStackProvisioningParameters`

``` purescript
describeStackProvisioningParameters :: forall eff. DescribeStackProvisioningParametersRequest -> Aff (err :: RequestError | eff) DescribeStackProvisioningParametersResult
```

<p>Requests a description of a stack's provisioning parameters.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeStackSummary`

``` purescript
describeStackSummary :: forall eff. DescribeStackSummaryRequest -> Aff (err :: RequestError | eff) DescribeStackSummaryResult
```

<p>Describes the number of layers and apps in a specified stack, and the number of instances in each state, such as <code>running_setup</code> or <code>online</code>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeStacks`

``` purescript
describeStacks :: forall eff. DescribeStacksRequest -> Aff (err :: RequestError | eff) DescribeStacksResult
```

<p>Requests a description of one or more stacks.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeTimeBasedAutoScaling`

``` purescript
describeTimeBasedAutoScaling :: forall eff. DescribeTimeBasedAutoScalingRequest -> Aff (err :: RequestError | eff) DescribeTimeBasedAutoScalingResult
```

<p>Describes time-based auto scaling configurations for specified instances.</p> <note> <p>You must specify at least one of the parameters.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeUserProfiles`

``` purescript
describeUserProfiles :: forall eff. DescribeUserProfilesRequest -> Aff (err :: RequestError | eff) DescribeUserProfilesResult
```

<p>Describe specified users.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `describeVolumes`

``` purescript
describeVolumes :: forall eff. DescribeVolumesRequest -> Aff (err :: RequestError | eff) DescribeVolumesResult
```

<p>Describes an instance's Amazon EBS volumes.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `detachElasticLoadBalancer`

``` purescript
detachElasticLoadBalancer :: forall eff. DetachElasticLoadBalancerRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Detaches a specified Elastic Load Balancing instance from its layer.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `disassociateElasticIp`

``` purescript
disassociateElasticIp :: forall eff. DisassociateElasticIpRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Disassociates an Elastic IP address from its instance. The address remains registered with the stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `getHostnameSuggestion`

``` purescript
getHostnameSuggestion :: forall eff. GetHostnameSuggestionRequest -> Aff (err :: RequestError | eff) GetHostnameSuggestionResult
```

<p>Gets a generated host name for the specified layer, based on the current host name theme.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `grantAccess`

``` purescript
grantAccess :: forall eff. GrantAccessRequest -> Aff (err :: RequestError | eff) GrantAccessResult
```

<note> <p>This action can be used only with Windows stacks.</p> </note> <p>Grants RDP access to a Windows instance for a specified time period.</p>

#### `listTags`

``` purescript
listTags :: forall eff. ListTagsRequest -> Aff (err :: RequestError | eff) ListTagsResult
```

<p>Returns a list of tags that are applied to the specified stack or layer.</p>

#### `rebootInstance`

``` purescript
rebootInstance :: forall eff. RebootInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Reboots a specified instance. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html">Starting, Stopping, and Rebooting Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `registerEcsCluster`

``` purescript
registerEcsCluster :: forall eff. RegisterEcsClusterRequest -> Aff (err :: RequestError | eff) RegisterEcsClusterResult
```

<p>Registers a specified Amazon ECS cluster with a stack. You can register only one cluster with a stack. A cluster can be registered with only one stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html"> Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html"> Managing User Permissions</a>.</p>

#### `registerElasticIp`

``` purescript
registerElasticIp :: forall eff. RegisterElasticIpRequest -> Aff (err :: RequestError | eff) RegisterElasticIpResult
```

<p>Registers an Elastic IP address with a specified stack. An address can be registered with only one stack at a time. If the address is already registered, you must first deregister it by calling <a>DeregisterElasticIp</a>. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `registerInstance`

``` purescript
registerInstance :: forall eff. RegisterInstanceRequest -> Aff (err :: RequestError | eff) RegisterInstanceResult
```

<p>Registers instances that were created outside of AWS OpsWorks Stacks with a specified stack.</p> <note> <p>We do not recommend using this action to register instances. The complete registration operation includes two tasks: installing the AWS OpsWorks Stacks agent on the instance, and registering the instance with the stack. <code>RegisterInstance</code> handles only the second step. You should instead use the AWS CLI <code>register</code> command, which performs the entire registration operation. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register.html"> Registering an Instance with an AWS OpsWorks Stacks Stack</a>.</p> </note> <p>Registered instances have the same requirements as instances that are created by using the <a>CreateInstance</a> API. For example, registered instances must be running a supported Linux-based operating system, and they must have a supported instance type. For more information about requirements for instances that you want to register, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register-registering-preparer.html"> Preparing the Instance</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `registerRdsDbInstance`

``` purescript
registerRdsDbInstance :: forall eff. RegisterRdsDbInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Registers an Amazon RDS instance with a stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `registerVolume`

``` purescript
registerVolume :: forall eff. RegisterVolumeRequest -> Aff (err :: RequestError | eff) RegisterVolumeResult
```

<p>Registers an Amazon EBS volume with a specified stack. A volume can be registered with only one stack at a time. If the volume is already registered, you must first deregister it by calling <a>DeregisterVolume</a>. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `setLoadBasedAutoScaling`

``` purescript
setLoadBasedAutoScaling :: forall eff. SetLoadBasedAutoScalingRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Specify the load-based auto scaling configuration for a specified layer. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html">Managing Load with Time-based and Load-based Instances</a>.</p> <note> <p>To use load-based auto scaling, you must create a set of load-based auto scaling instances. Load-based auto scaling operates only on the instances from that set, so you must ensure that you have created enough instances to handle the maximum anticipated load.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `setPermission`

``` purescript
setPermission :: forall eff. SetPermissionRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Specifies a user's permissions. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingsecurity.html">Security and Permissions</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `setTimeBasedAutoScaling`

``` purescript
setTimeBasedAutoScaling :: forall eff. SetTimeBasedAutoScalingRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Specify the time-based auto scaling configuration for a specified instance. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html">Managing Load with Time-based and Load-based Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `startInstance`

``` purescript
startInstance :: forall eff. StartInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Starts a specified instance. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html">Starting, Stopping, and Rebooting Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `startStack`

``` purescript
startStack :: forall eff. StartStackRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Starts a stack's instances.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `stopInstance`

``` purescript
stopInstance :: forall eff. StopInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Stops a specified instance. When you stop a standard instance, the data disappears and must be reinstalled when you restart the instance. You can stop an Amazon EBS-backed instance without losing data. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html">Starting, Stopping, and Rebooting Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `stopStack`

``` purescript
stopStack :: forall eff. StopStackRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Stops a specified stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Apply cost-allocation tags to a specified stack or layer in AWS OpsWorks Stacks. For more information about how tagging works, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/tagging.html">Tags</a> in the AWS OpsWorks User Guide.</p>

#### `unassignInstance`

``` purescript
unassignInstance :: forall eff. UnassignInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Unassigns a registered instance from all of it's layers. The instance remains in the stack as an unassigned instance and can be assigned to another layer, as needed. You cannot use this action with instances that were created with AWS OpsWorks Stacks.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `unassignVolume`

``` purescript
unassignVolume :: forall eff. UnassignVolumeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Unassigns an assigned Amazon EBS volume. The volume remains registered with the stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Removes tags from a specified stack or layer.</p>

#### `updateApp`

``` purescript
updateApp :: forall eff. UpdateAppRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates a specified app.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `updateElasticIp`

``` purescript
updateElasticIp :: forall eff. UpdateElasticIpRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates a registered Elastic IP address's name. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `updateInstance`

``` purescript
updateInstance :: forall eff. UpdateInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates a specified instance.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `updateLayer`

``` purescript
updateLayer :: forall eff. UpdateLayerRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates a specified layer.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `updateMyUserProfile`

``` purescript
updateMyUserProfile :: forall eff. UpdateMyUserProfileRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates a user's SSH public key.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have self-management enabled or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `updateRdsDbInstance`

``` purescript
updateRdsDbInstance :: forall eff. UpdateRdsDbInstanceRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates an Amazon RDS instance.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `updateStack`

``` purescript
updateStack :: forall eff. UpdateStackRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates a specified stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `updateUserProfile`

``` purescript
updateUserProfile :: forall eff. UpdateUserProfileRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates a specified user profile.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `updateVolume`

``` purescript
updateVolume :: forall eff. UpdateVolumeRequest -> Aff (err :: RequestError | eff) Unit
```

<p>Updates an Amazon EBS volume's name or mount point. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>

#### `AgentVersion`

``` purescript
newtype AgentVersion
  = AgentVersion { "Version" :: NullOrUndefined (String), "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager) }
```

<p>Describes an agent version.</p>

##### Instances
``` purescript
Newtype AgentVersion _
```

#### `AgentVersions`

``` purescript
newtype AgentVersions
  = AgentVersions (Array AgentVersion)
```

##### Instances
``` purescript
Newtype AgentVersions _
```

#### `App`

``` purescript
newtype App
  = App { "AppId" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "Shortname" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "DataSources" :: NullOrUndefined (DataSources), "Type" :: NullOrUndefined (AppType), "AppSource" :: NullOrUndefined (Source), "Domains" :: NullOrUndefined (Strings), "EnableSsl" :: NullOrUndefined (Boolean), "SslConfiguration" :: NullOrUndefined (SslConfiguration), "Attributes" :: NullOrUndefined (AppAttributes), "CreatedAt" :: NullOrUndefined (String), "Environment" :: NullOrUndefined (EnvironmentVariables) }
```

<p>A description of the app.</p>

##### Instances
``` purescript
Newtype App _
```

#### `AppAttributes`

``` purescript
newtype AppAttributes
  = AppAttributes (Map AppAttributesKeys String)
```

##### Instances
``` purescript
Newtype AppAttributes _
```

#### `AppAttributesKeys`

``` purescript
newtype AppAttributesKeys
  = AppAttributesKeys String
```

##### Instances
``` purescript
Newtype AppAttributesKeys _
```

#### `AppType`

``` purescript
newtype AppType
  = AppType String
```

##### Instances
``` purescript
Newtype AppType _
```

#### `Apps`

``` purescript
newtype Apps
  = Apps (Array App)
```

##### Instances
``` purescript
Newtype Apps _
```

#### `Architecture`

``` purescript
newtype Architecture
  = Architecture String
```

##### Instances
``` purescript
Newtype Architecture _
```

#### `AssignInstanceRequest`

``` purescript
newtype AssignInstanceRequest
  = AssignInstanceRequest { "InstanceId" :: String, "LayerIds" :: Strings }
```

##### Instances
``` purescript
Newtype AssignInstanceRequest _
```

#### `AssignVolumeRequest`

``` purescript
newtype AssignVolumeRequest
  = AssignVolumeRequest { "VolumeId" :: String, "InstanceId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype AssignVolumeRequest _
```

#### `AssociateElasticIpRequest`

``` purescript
newtype AssociateElasticIpRequest
  = AssociateElasticIpRequest { "ElasticIp" :: String, "InstanceId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype AssociateElasticIpRequest _
```

#### `AttachElasticLoadBalancerRequest`

``` purescript
newtype AttachElasticLoadBalancerRequest
  = AttachElasticLoadBalancerRequest { "ElasticLoadBalancerName" :: String, "LayerId" :: String }
```

##### Instances
``` purescript
Newtype AttachElasticLoadBalancerRequest _
```

#### `AutoScalingThresholds`

``` purescript
newtype AutoScalingThresholds
  = AutoScalingThresholds { "InstanceCount" :: NullOrUndefined (Int), "ThresholdsWaitTime" :: NullOrUndefined (Minute), "IgnoreMetricsTime" :: NullOrUndefined (Minute), "CpuThreshold" :: NullOrUndefined (Number), "MemoryThreshold" :: NullOrUndefined (Number), "LoadThreshold" :: NullOrUndefined (Number), "Alarms" :: NullOrUndefined (Strings) }
```

<p>Describes a load-based auto scaling upscaling or downscaling threshold configuration, which specifies when AWS OpsWorks Stacks starts or stops load-based instances.</p>

##### Instances
``` purescript
Newtype AutoScalingThresholds _
```

#### `AutoScalingType`

``` purescript
newtype AutoScalingType
  = AutoScalingType String
```

##### Instances
``` purescript
Newtype AutoScalingType _
```

#### `BlockDeviceMapping`

``` purescript
newtype BlockDeviceMapping
  = BlockDeviceMapping { "DeviceName" :: NullOrUndefined (String), "NoDevice" :: NullOrUndefined (String), "VirtualName" :: NullOrUndefined (String), "Ebs" :: NullOrUndefined (EbsBlockDevice) }
```

<p>Describes a block device mapping. This data type maps directly to the Amazon EC2 <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html">BlockDeviceMapping</a> data type. </p>

##### Instances
``` purescript
Newtype BlockDeviceMapping _
```

#### `BlockDeviceMappings`

``` purescript
newtype BlockDeviceMappings
  = BlockDeviceMappings (Array BlockDeviceMapping)
```

##### Instances
``` purescript
Newtype BlockDeviceMappings _
```

#### `ChefConfiguration`

``` purescript
newtype ChefConfiguration
  = ChefConfiguration { "ManageBerkshelf" :: NullOrUndefined (Boolean), "BerkshelfVersion" :: NullOrUndefined (String) }
```

<p>Describes the Chef configuration.</p>

##### Instances
``` purescript
Newtype ChefConfiguration _
```

#### `CloneStackRequest`

``` purescript
newtype CloneStackRequest
  = CloneStackRequest { "SourceStackId" :: String, "Name" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (StackAttributes), "ServiceRoleArn" :: String, "DefaultInstanceProfileArn" :: NullOrUndefined (String), "DefaultOs" :: NullOrUndefined (String), "HostnameTheme" :: NullOrUndefined (String), "DefaultAvailabilityZone" :: NullOrUndefined (String), "DefaultSubnetId" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String), "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager), "ChefConfiguration" :: NullOrUndefined (ChefConfiguration), "UseCustomCookbooks" :: NullOrUndefined (Boolean), "UseOpsworksSecurityGroups" :: NullOrUndefined (Boolean), "CustomCookbooksSource" :: NullOrUndefined (Source), "DefaultSshKeyName" :: NullOrUndefined (String), "ClonePermissions" :: NullOrUndefined (Boolean), "CloneAppIds" :: NullOrUndefined (Strings), "DefaultRootDeviceType" :: NullOrUndefined (RootDeviceType), "AgentVersion" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CloneStackRequest _
```

#### `CloneStackResult`

``` purescript
newtype CloneStackResult
  = CloneStackResult { "StackId" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>CloneStack</code> request.</p>

##### Instances
``` purescript
Newtype CloneStackResult _
```

#### `CloudWatchLogsConfiguration`

``` purescript
newtype CloudWatchLogsConfiguration
  = CloudWatchLogsConfiguration { "Enabled" :: NullOrUndefined (Boolean), "LogStreams" :: NullOrUndefined (CloudWatchLogsLogStreams) }
```

<p>Describes the Amazon CloudWatch logs configuration for a layer.</p>

##### Instances
``` purescript
Newtype CloudWatchLogsConfiguration _
```

#### `CloudWatchLogsEncoding`

``` purescript
newtype CloudWatchLogsEncoding
  = CloudWatchLogsEncoding String
```

<p>Specifies the encoding of the log file so that the file can be read correctly. The default is <code>utf_8</code>. Encodings supported by Python <code>codecs.decode()</code> can be used here.</p>

##### Instances
``` purescript
Newtype CloudWatchLogsEncoding _
```

#### `CloudWatchLogsInitialPosition`

``` purescript
newtype CloudWatchLogsInitialPosition
  = CloudWatchLogsInitialPosition String
```

<p>Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. It's only used if there is no state persisted for that log stream.</p>

##### Instances
``` purescript
Newtype CloudWatchLogsInitialPosition _
```

#### `CloudWatchLogsLogStream`

``` purescript
newtype CloudWatchLogsLogStream
  = CloudWatchLogsLogStream { "LogGroupName" :: NullOrUndefined (String), "DatetimeFormat" :: NullOrUndefined (String), "TimeZone" :: NullOrUndefined (CloudWatchLogsTimeZone), "File" :: NullOrUndefined (String), "FileFingerprintLines" :: NullOrUndefined (String), "MultiLineStartPattern" :: NullOrUndefined (String), "InitialPosition" :: NullOrUndefined (CloudWatchLogsInitialPosition), "Encoding" :: NullOrUndefined (CloudWatchLogsEncoding), "BufferDuration" :: NullOrUndefined (Int), "BatchCount" :: NullOrUndefined (Int), "BatchSize" :: NullOrUndefined (Int) }
```

<p>Describes the Amazon CloudWatch logs configuration for a layer. For detailed information about members of this data type, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html">CloudWatch Logs Agent Reference</a>.</p>

##### Instances
``` purescript
Newtype CloudWatchLogsLogStream _
```

#### `CloudWatchLogsLogStreams`

``` purescript
newtype CloudWatchLogsLogStreams
  = CloudWatchLogsLogStreams (Array CloudWatchLogsLogStream)
```

<p>Describes the Amazon CloudWatch logs configuration for a layer.</p>

##### Instances
``` purescript
Newtype CloudWatchLogsLogStreams _
```

#### `CloudWatchLogsTimeZone`

``` purescript
newtype CloudWatchLogsTimeZone
  = CloudWatchLogsTimeZone String
```

<p>The preferred time zone for logs streamed to CloudWatch Logs. Valid values are <code>LOCAL</code> and <code>UTC</code>, for Coordinated Universal Time.</p>

##### Instances
``` purescript
Newtype CloudWatchLogsTimeZone _
```

#### `Command`

``` purescript
newtype Command
  = Command { "CommandId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "DeploymentId" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (DateTime), "AcknowledgedAt" :: NullOrUndefined (DateTime), "CompletedAt" :: NullOrUndefined (DateTime), "Status" :: NullOrUndefined (String), "ExitCode" :: NullOrUndefined (Int), "LogUrl" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String) }
```

<p>Describes a command.</p>

##### Instances
``` purescript
Newtype Command _
```

#### `Commands`

``` purescript
newtype Commands
  = Commands (Array Command)
```

##### Instances
``` purescript
Newtype Commands _
```

#### `CreateAppRequest`

``` purescript
newtype CreateAppRequest
  = CreateAppRequest { "StackId" :: String, "Shortname" :: NullOrUndefined (String), "Name" :: String, "Description" :: NullOrUndefined (String), "DataSources" :: NullOrUndefined (DataSources), "Type" :: AppType, "AppSource" :: NullOrUndefined (Source), "Domains" :: NullOrUndefined (Strings), "EnableSsl" :: NullOrUndefined (Boolean), "SslConfiguration" :: NullOrUndefined (SslConfiguration), "Attributes" :: NullOrUndefined (AppAttributes), "Environment" :: NullOrUndefined (EnvironmentVariables) }
```

##### Instances
``` purescript
Newtype CreateAppRequest _
```

#### `CreateAppResult`

``` purescript
newtype CreateAppResult
  = CreateAppResult { "AppId" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>CreateApp</code> request.</p>

##### Instances
``` purescript
Newtype CreateAppResult _
```

#### `CreateDeploymentRequest`

``` purescript
newtype CreateDeploymentRequest
  = CreateDeploymentRequest { "StackId" :: String, "AppId" :: NullOrUndefined (String), "InstanceIds" :: NullOrUndefined (Strings), "LayerIds" :: NullOrUndefined (Strings), "Command" :: DeploymentCommand, "Comment" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateDeploymentRequest _
```

#### `CreateDeploymentResult`

``` purescript
newtype CreateDeploymentResult
  = CreateDeploymentResult { "DeploymentId" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>CreateDeployment</code> request.</p>

##### Instances
``` purescript
Newtype CreateDeploymentResult _
```

#### `CreateInstanceRequest`

``` purescript
newtype CreateInstanceRequest
  = CreateInstanceRequest { "StackId" :: String, "LayerIds" :: Strings, "InstanceType" :: String, "AutoScalingType" :: NullOrUndefined (AutoScalingType), "Hostname" :: NullOrUndefined (String), "Os" :: NullOrUndefined (String), "AmiId" :: NullOrUndefined (String), "SshKeyName" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (String), "VirtualizationType" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String), "Architecture" :: NullOrUndefined (Architecture), "RootDeviceType" :: NullOrUndefined (RootDeviceType), "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappings), "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean), "EbsOptimized" :: NullOrUndefined (Boolean), "AgentVersion" :: NullOrUndefined (String), "Tenancy" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateInstanceRequest _
```

#### `CreateInstanceResult`

``` purescript
newtype CreateInstanceResult
  = CreateInstanceResult { "InstanceId" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>CreateInstance</code> request.</p>

##### Instances
``` purescript
Newtype CreateInstanceResult _
```

#### `CreateLayerRequest`

``` purescript
newtype CreateLayerRequest
  = CreateLayerRequest { "StackId" :: String, "Type" :: LayerType, "Name" :: String, "Shortname" :: String, "Attributes" :: NullOrUndefined (LayerAttributes), "CloudWatchLogsConfiguration" :: NullOrUndefined (CloudWatchLogsConfiguration), "CustomInstanceProfileArn" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String), "CustomSecurityGroupIds" :: NullOrUndefined (Strings), "Packages" :: NullOrUndefined (Strings), "VolumeConfigurations" :: NullOrUndefined (VolumeConfigurations), "EnableAutoHealing" :: NullOrUndefined (Boolean), "AutoAssignElasticIps" :: NullOrUndefined (Boolean), "AutoAssignPublicIps" :: NullOrUndefined (Boolean), "CustomRecipes" :: NullOrUndefined (Recipes), "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean), "UseEbsOptimizedInstances" :: NullOrUndefined (Boolean), "LifecycleEventConfiguration" :: NullOrUndefined (LifecycleEventConfiguration) }
```

##### Instances
``` purescript
Newtype CreateLayerRequest _
```

#### `CreateLayerResult`

``` purescript
newtype CreateLayerResult
  = CreateLayerResult { "LayerId" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>CreateLayer</code> request.</p>

##### Instances
``` purescript
Newtype CreateLayerResult _
```

#### `CreateStackRequest`

``` purescript
newtype CreateStackRequest
  = CreateStackRequest { "Name" :: String, "Region" :: String, "VpcId" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (StackAttributes), "ServiceRoleArn" :: String, "DefaultInstanceProfileArn" :: String, "DefaultOs" :: NullOrUndefined (String), "HostnameTheme" :: NullOrUndefined (String), "DefaultAvailabilityZone" :: NullOrUndefined (String), "DefaultSubnetId" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String), "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager), "ChefConfiguration" :: NullOrUndefined (ChefConfiguration), "UseCustomCookbooks" :: NullOrUndefined (Boolean), "UseOpsworksSecurityGroups" :: NullOrUndefined (Boolean), "CustomCookbooksSource" :: NullOrUndefined (Source), "DefaultSshKeyName" :: NullOrUndefined (String), "DefaultRootDeviceType" :: NullOrUndefined (RootDeviceType), "AgentVersion" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateStackRequest _
```

#### `CreateStackResult`

``` purescript
newtype CreateStackResult
  = CreateStackResult { "StackId" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>CreateStack</code> request.</p>

##### Instances
``` purescript
Newtype CreateStackResult _
```

#### `CreateUserProfileRequest`

``` purescript
newtype CreateUserProfileRequest
  = CreateUserProfileRequest { "IamUserArn" :: String, "SshUsername" :: NullOrUndefined (String), "SshPublicKey" :: NullOrUndefined (String), "AllowSelfManagement" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype CreateUserProfileRequest _
```

#### `CreateUserProfileResult`

``` purescript
newtype CreateUserProfileResult
  = CreateUserProfileResult { "IamUserArn" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>CreateUserProfile</code> request.</p>

##### Instances
``` purescript
Newtype CreateUserProfileResult _
```

#### `DailyAutoScalingSchedule`

``` purescript
newtype DailyAutoScalingSchedule
  = DailyAutoScalingSchedule (Map Hour Switch)
```

##### Instances
``` purescript
Newtype DailyAutoScalingSchedule _
```

#### `DataSource`

``` purescript
newtype DataSource
  = DataSource { "Type" :: NullOrUndefined (String), "Arn" :: NullOrUndefined (String), "DatabaseName" :: NullOrUndefined (String) }
```

<p>Describes an app's data source.</p>

##### Instances
``` purescript
Newtype DataSource _
```

#### `DataSources`

``` purescript
newtype DataSources
  = DataSources (Array DataSource)
```

##### Instances
``` purescript
Newtype DataSources _
```

#### `DateTime`

``` purescript
newtype DateTime
  = DateTime String
```

##### Instances
``` purescript
Newtype DateTime _
```

#### `DeleteAppRequest`

``` purescript
newtype DeleteAppRequest
  = DeleteAppRequest { "AppId" :: String }
```

##### Instances
``` purescript
Newtype DeleteAppRequest _
```

#### `DeleteInstanceRequest`

``` purescript
newtype DeleteInstanceRequest
  = DeleteInstanceRequest { "InstanceId" :: String, "DeleteElasticIp" :: NullOrUndefined (Boolean), "DeleteVolumes" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype DeleteInstanceRequest _
```

#### `DeleteLayerRequest`

``` purescript
newtype DeleteLayerRequest
  = DeleteLayerRequest { "LayerId" :: String }
```

##### Instances
``` purescript
Newtype DeleteLayerRequest _
```

#### `DeleteStackRequest`

``` purescript
newtype DeleteStackRequest
  = DeleteStackRequest { "StackId" :: String }
```

##### Instances
``` purescript
Newtype DeleteStackRequest _
```

#### `DeleteUserProfileRequest`

``` purescript
newtype DeleteUserProfileRequest
  = DeleteUserProfileRequest { "IamUserArn" :: String }
```

##### Instances
``` purescript
Newtype DeleteUserProfileRequest _
```

#### `Deployment`

``` purescript
newtype Deployment
  = Deployment { "DeploymentId" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "AppId" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (DateTime), "CompletedAt" :: NullOrUndefined (DateTime), "Duration" :: NullOrUndefined (Int), "IamUserArn" :: NullOrUndefined (String), "Comment" :: NullOrUndefined (String), "Command" :: NullOrUndefined (DeploymentCommand), "Status" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String), "InstanceIds" :: NullOrUndefined (Strings) }
```

<p>Describes a deployment of a stack or app.</p>

##### Instances
``` purescript
Newtype Deployment _
```

#### `DeploymentCommand`

``` purescript
newtype DeploymentCommand
  = DeploymentCommand { "Name" :: DeploymentCommandName, "Args" :: NullOrUndefined (DeploymentCommandArgs) }
```

<p>Used to specify a stack or deployment command.</p>

##### Instances
``` purescript
Newtype DeploymentCommand _
```

#### `DeploymentCommandArgs`

``` purescript
newtype DeploymentCommandArgs
  = DeploymentCommandArgs (Map String Strings)
```

##### Instances
``` purescript
Newtype DeploymentCommandArgs _
```

#### `DeploymentCommandName`

``` purescript
newtype DeploymentCommandName
  = DeploymentCommandName String
```

##### Instances
``` purescript
Newtype DeploymentCommandName _
```

#### `Deployments`

``` purescript
newtype Deployments
  = Deployments (Array Deployment)
```

##### Instances
``` purescript
Newtype Deployments _
```

#### `DeregisterEcsClusterRequest`

``` purescript
newtype DeregisterEcsClusterRequest
  = DeregisterEcsClusterRequest { "EcsClusterArn" :: String }
```

##### Instances
``` purescript
Newtype DeregisterEcsClusterRequest _
```

#### `DeregisterElasticIpRequest`

``` purescript
newtype DeregisterElasticIpRequest
  = DeregisterElasticIpRequest { "ElasticIp" :: String }
```

##### Instances
``` purescript
Newtype DeregisterElasticIpRequest _
```

#### `DeregisterInstanceRequest`

``` purescript
newtype DeregisterInstanceRequest
  = DeregisterInstanceRequest { "InstanceId" :: String }
```

##### Instances
``` purescript
Newtype DeregisterInstanceRequest _
```

#### `DeregisterRdsDbInstanceRequest`

``` purescript
newtype DeregisterRdsDbInstanceRequest
  = DeregisterRdsDbInstanceRequest { "RdsDbInstanceArn" :: String }
```

##### Instances
``` purescript
Newtype DeregisterRdsDbInstanceRequest _
```

#### `DeregisterVolumeRequest`

``` purescript
newtype DeregisterVolumeRequest
  = DeregisterVolumeRequest { "VolumeId" :: String }
```

##### Instances
``` purescript
Newtype DeregisterVolumeRequest _
```

#### `DescribeAgentVersionsRequest`

``` purescript
newtype DescribeAgentVersionsRequest
  = DescribeAgentVersionsRequest { "StackId" :: NullOrUndefined (String), "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager) }
```

##### Instances
``` purescript
Newtype DescribeAgentVersionsRequest _
```

#### `DescribeAgentVersionsResult`

``` purescript
newtype DescribeAgentVersionsResult
  = DescribeAgentVersionsResult { "AgentVersions" :: NullOrUndefined (AgentVersions) }
```

<p>Contains the response to a <code>DescribeAgentVersions</code> request.</p>

##### Instances
``` purescript
Newtype DescribeAgentVersionsResult _
```

#### `DescribeAppsRequest`

``` purescript
newtype DescribeAppsRequest
  = DescribeAppsRequest { "StackId" :: NullOrUndefined (String), "AppIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeAppsRequest _
```

#### `DescribeAppsResult`

``` purescript
newtype DescribeAppsResult
  = DescribeAppsResult { "Apps" :: NullOrUndefined (Apps) }
```

<p>Contains the response to a <code>DescribeApps</code> request.</p>

##### Instances
``` purescript
Newtype DescribeAppsResult _
```

#### `DescribeCommandsRequest`

``` purescript
newtype DescribeCommandsRequest
  = DescribeCommandsRequest { "DeploymentId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "CommandIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeCommandsRequest _
```

#### `DescribeCommandsResult`

``` purescript
newtype DescribeCommandsResult
  = DescribeCommandsResult { "Commands" :: NullOrUndefined (Commands) }
```

<p>Contains the response to a <code>DescribeCommands</code> request.</p>

##### Instances
``` purescript
Newtype DescribeCommandsResult _
```

#### `DescribeDeploymentsRequest`

``` purescript
newtype DescribeDeploymentsRequest
  = DescribeDeploymentsRequest { "StackId" :: NullOrUndefined (String), "AppId" :: NullOrUndefined (String), "DeploymentIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeDeploymentsRequest _
```

#### `DescribeDeploymentsResult`

``` purescript
newtype DescribeDeploymentsResult
  = DescribeDeploymentsResult { "Deployments" :: NullOrUndefined (Deployments) }
```

<p>Contains the response to a <code>DescribeDeployments</code> request.</p>

##### Instances
``` purescript
Newtype DescribeDeploymentsResult _
```

#### `DescribeEcsClustersRequest`

``` purescript
newtype DescribeEcsClustersRequest
  = DescribeEcsClustersRequest { "EcsClusterArns" :: NullOrUndefined (Strings), "StackId" :: NullOrUndefined (String), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (Int) }
```

##### Instances
``` purescript
Newtype DescribeEcsClustersRequest _
```

#### `DescribeEcsClustersResult`

``` purescript
newtype DescribeEcsClustersResult
  = DescribeEcsClustersResult { "EcsClusters" :: NullOrUndefined (EcsClusters), "NextToken" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>DescribeEcsClusters</code> request.</p>

##### Instances
``` purescript
Newtype DescribeEcsClustersResult _
```

#### `DescribeElasticIpsRequest`

``` purescript
newtype DescribeElasticIpsRequest
  = DescribeElasticIpsRequest { "InstanceId" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "Ips" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeElasticIpsRequest _
```

#### `DescribeElasticIpsResult`

``` purescript
newtype DescribeElasticIpsResult
  = DescribeElasticIpsResult { "ElasticIps" :: NullOrUndefined (ElasticIps) }
```

<p>Contains the response to a <code>DescribeElasticIps</code> request.</p>

##### Instances
``` purescript
Newtype DescribeElasticIpsResult _
```

#### `DescribeElasticLoadBalancersRequest`

``` purescript
newtype DescribeElasticLoadBalancersRequest
  = DescribeElasticLoadBalancersRequest { "StackId" :: NullOrUndefined (String), "LayerIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeElasticLoadBalancersRequest _
```

#### `DescribeElasticLoadBalancersResult`

``` purescript
newtype DescribeElasticLoadBalancersResult
  = DescribeElasticLoadBalancersResult { "ElasticLoadBalancers" :: NullOrUndefined (ElasticLoadBalancers) }
```

<p>Contains the response to a <code>DescribeElasticLoadBalancers</code> request.</p>

##### Instances
``` purescript
Newtype DescribeElasticLoadBalancersResult _
```

#### `DescribeInstancesRequest`

``` purescript
newtype DescribeInstancesRequest
  = DescribeInstancesRequest { "StackId" :: NullOrUndefined (String), "LayerId" :: NullOrUndefined (String), "InstanceIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeInstancesRequest _
```

#### `DescribeInstancesResult`

``` purescript
newtype DescribeInstancesResult
  = DescribeInstancesResult { "Instances" :: NullOrUndefined (Instances) }
```

<p>Contains the response to a <code>DescribeInstances</code> request.</p>

##### Instances
``` purescript
Newtype DescribeInstancesResult _
```

#### `DescribeLayersRequest`

``` purescript
newtype DescribeLayersRequest
  = DescribeLayersRequest { "StackId" :: NullOrUndefined (String), "LayerIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeLayersRequest _
```

#### `DescribeLayersResult`

``` purescript
newtype DescribeLayersResult
  = DescribeLayersResult { "Layers" :: NullOrUndefined (Layers) }
```

<p>Contains the response to a <code>DescribeLayers</code> request.</p>

##### Instances
``` purescript
Newtype DescribeLayersResult _
```

#### `DescribeLoadBasedAutoScalingRequest`

``` purescript
newtype DescribeLoadBasedAutoScalingRequest
  = DescribeLoadBasedAutoScalingRequest { "LayerIds" :: Strings }
```

##### Instances
``` purescript
Newtype DescribeLoadBasedAutoScalingRequest _
```

#### `DescribeLoadBasedAutoScalingResult`

``` purescript
newtype DescribeLoadBasedAutoScalingResult
  = DescribeLoadBasedAutoScalingResult { "LoadBasedAutoScalingConfigurations" :: NullOrUndefined (LoadBasedAutoScalingConfigurations) }
```

<p>Contains the response to a <code>DescribeLoadBasedAutoScaling</code> request.</p>

##### Instances
``` purescript
Newtype DescribeLoadBasedAutoScalingResult _
```

#### `DescribeMyUserProfileResult`

``` purescript
newtype DescribeMyUserProfileResult
  = DescribeMyUserProfileResult { "UserProfile" :: NullOrUndefined (SelfUserProfile) }
```

<p>Contains the response to a <code>DescribeMyUserProfile</code> request.</p>

##### Instances
``` purescript
Newtype DescribeMyUserProfileResult _
```

#### `DescribeOperatingSystemsResponse`

``` purescript
newtype DescribeOperatingSystemsResponse
  = DescribeOperatingSystemsResponse { "OperatingSystems" :: NullOrUndefined (OperatingSystems) }
```

<p>The response to a <code>DescribeOperatingSystems</code> request.</p>

##### Instances
``` purescript
Newtype DescribeOperatingSystemsResponse _
```

#### `DescribePermissionsRequest`

``` purescript
newtype DescribePermissionsRequest
  = DescribePermissionsRequest { "IamUserArn" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribePermissionsRequest _
```

#### `DescribePermissionsResult`

``` purescript
newtype DescribePermissionsResult
  = DescribePermissionsResult { "Permissions" :: NullOrUndefined (Permissions) }
```

<p>Contains the response to a <code>DescribePermissions</code> request.</p>

##### Instances
``` purescript
Newtype DescribePermissionsResult _
```

#### `DescribeRaidArraysRequest`

``` purescript
newtype DescribeRaidArraysRequest
  = DescribeRaidArraysRequest { "InstanceId" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "RaidArrayIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeRaidArraysRequest _
```

#### `DescribeRaidArraysResult`

``` purescript
newtype DescribeRaidArraysResult
  = DescribeRaidArraysResult { "RaidArrays" :: NullOrUndefined (RaidArrays) }
```

<p>Contains the response to a <code>DescribeRaidArrays</code> request.</p>

##### Instances
``` purescript
Newtype DescribeRaidArraysResult _
```

#### `DescribeRdsDbInstancesRequest`

``` purescript
newtype DescribeRdsDbInstancesRequest
  = DescribeRdsDbInstancesRequest { "StackId" :: String, "RdsDbInstanceArns" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeRdsDbInstancesRequest _
```

#### `DescribeRdsDbInstancesResult`

``` purescript
newtype DescribeRdsDbInstancesResult
  = DescribeRdsDbInstancesResult { "RdsDbInstances" :: NullOrUndefined (RdsDbInstances) }
```

<p>Contains the response to a <code>DescribeRdsDbInstances</code> request.</p>

##### Instances
``` purescript
Newtype DescribeRdsDbInstancesResult _
```

#### `DescribeServiceErrorsRequest`

``` purescript
newtype DescribeServiceErrorsRequest
  = DescribeServiceErrorsRequest { "StackId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "ServiceErrorIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeServiceErrorsRequest _
```

#### `DescribeServiceErrorsResult`

``` purescript
newtype DescribeServiceErrorsResult
  = DescribeServiceErrorsResult { "ServiceErrors" :: NullOrUndefined (ServiceErrors) }
```

<p>Contains the response to a <code>DescribeServiceErrors</code> request.</p>

##### Instances
``` purescript
Newtype DescribeServiceErrorsResult _
```

#### `DescribeStackProvisioningParametersRequest`

``` purescript
newtype DescribeStackProvisioningParametersRequest
  = DescribeStackProvisioningParametersRequest { "StackId" :: String }
```

##### Instances
``` purescript
Newtype DescribeStackProvisioningParametersRequest _
```

#### `DescribeStackProvisioningParametersResult`

``` purescript
newtype DescribeStackProvisioningParametersResult
  = DescribeStackProvisioningParametersResult { "AgentInstallerUrl" :: NullOrUndefined (String), "Parameters" :: NullOrUndefined (Parameters) }
```

<p>Contains the response to a <code>DescribeStackProvisioningParameters</code> request.</p>

##### Instances
``` purescript
Newtype DescribeStackProvisioningParametersResult _
```

#### `DescribeStackSummaryRequest`

``` purescript
newtype DescribeStackSummaryRequest
  = DescribeStackSummaryRequest { "StackId" :: String }
```

##### Instances
``` purescript
Newtype DescribeStackSummaryRequest _
```

#### `DescribeStackSummaryResult`

``` purescript
newtype DescribeStackSummaryResult
  = DescribeStackSummaryResult { "StackSummary" :: NullOrUndefined (StackSummary) }
```

<p>Contains the response to a <code>DescribeStackSummary</code> request.</p>

##### Instances
``` purescript
Newtype DescribeStackSummaryResult _
```

#### `DescribeStacksRequest`

``` purescript
newtype DescribeStacksRequest
  = DescribeStacksRequest { "StackIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeStacksRequest _
```

#### `DescribeStacksResult`

``` purescript
newtype DescribeStacksResult
  = DescribeStacksResult { "Stacks" :: NullOrUndefined (Stacks) }
```

<p>Contains the response to a <code>DescribeStacks</code> request.</p>

##### Instances
``` purescript
Newtype DescribeStacksResult _
```

#### `DescribeTimeBasedAutoScalingRequest`

``` purescript
newtype DescribeTimeBasedAutoScalingRequest
  = DescribeTimeBasedAutoScalingRequest { "InstanceIds" :: Strings }
```

##### Instances
``` purescript
Newtype DescribeTimeBasedAutoScalingRequest _
```

#### `DescribeTimeBasedAutoScalingResult`

``` purescript
newtype DescribeTimeBasedAutoScalingResult
  = DescribeTimeBasedAutoScalingResult { "TimeBasedAutoScalingConfigurations" :: NullOrUndefined (TimeBasedAutoScalingConfigurations) }
```

<p>Contains the response to a <code>DescribeTimeBasedAutoScaling</code> request.</p>

##### Instances
``` purescript
Newtype DescribeTimeBasedAutoScalingResult _
```

#### `DescribeUserProfilesRequest`

``` purescript
newtype DescribeUserProfilesRequest
  = DescribeUserProfilesRequest { "IamUserArns" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeUserProfilesRequest _
```

#### `DescribeUserProfilesResult`

``` purescript
newtype DescribeUserProfilesResult
  = DescribeUserProfilesResult { "UserProfiles" :: NullOrUndefined (UserProfiles) }
```

<p>Contains the response to a <code>DescribeUserProfiles</code> request.</p>

##### Instances
``` purescript
Newtype DescribeUserProfilesResult _
```

#### `DescribeVolumesRequest`

``` purescript
newtype DescribeVolumesRequest
  = DescribeVolumesRequest { "InstanceId" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "RaidArrayId" :: NullOrUndefined (String), "VolumeIds" :: NullOrUndefined (Strings) }
```

##### Instances
``` purescript
Newtype DescribeVolumesRequest _
```

#### `DescribeVolumesResult`

``` purescript
newtype DescribeVolumesResult
  = DescribeVolumesResult { "Volumes" :: NullOrUndefined (Volumes) }
```

<p>Contains the response to a <code>DescribeVolumes</code> request.</p>

##### Instances
``` purescript
Newtype DescribeVolumesResult _
```

#### `DetachElasticLoadBalancerRequest`

``` purescript
newtype DetachElasticLoadBalancerRequest
  = DetachElasticLoadBalancerRequest { "ElasticLoadBalancerName" :: String, "LayerId" :: String }
```

##### Instances
``` purescript
Newtype DetachElasticLoadBalancerRequest _
```

#### `DisassociateElasticIpRequest`

``` purescript
newtype DisassociateElasticIpRequest
  = DisassociateElasticIpRequest { "ElasticIp" :: String }
```

##### Instances
``` purescript
Newtype DisassociateElasticIpRequest _
```

#### `EbsBlockDevice`

``` purescript
newtype EbsBlockDevice
  = EbsBlockDevice { "SnapshotId" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (Int), "VolumeSize" :: NullOrUndefined (Int), "VolumeType" :: NullOrUndefined (VolumeType), "DeleteOnTermination" :: NullOrUndefined (Boolean) }
```

<p>Describes an Amazon EBS volume. This data type maps directly to the Amazon EC2 <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html">EbsBlockDevice</a> data type.</p>

##### Instances
``` purescript
Newtype EbsBlockDevice _
```

#### `EcsCluster`

``` purescript
newtype EcsCluster
  = EcsCluster { "EcsClusterArn" :: NullOrUndefined (String), "EcsClusterName" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "RegisteredAt" :: NullOrUndefined (DateTime) }
```

<p>Describes a registered Amazon ECS cluster.</p>

##### Instances
``` purescript
Newtype EcsCluster _
```

#### `EcsClusters`

``` purescript
newtype EcsClusters
  = EcsClusters (Array EcsCluster)
```

##### Instances
``` purescript
Newtype EcsClusters _
```

#### `ElasticIp`

``` purescript
newtype ElasticIp
  = ElasticIp { "Ip" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Domain" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String) }
```

<p>Describes an Elastic IP address.</p>

##### Instances
``` purescript
Newtype ElasticIp _
```

#### `ElasticIps`

``` purescript
newtype ElasticIps
  = ElasticIps (Array ElasticIp)
```

##### Instances
``` purescript
Newtype ElasticIps _
```

#### `ElasticLoadBalancer`

``` purescript
newtype ElasticLoadBalancer
  = ElasticLoadBalancer { "ElasticLoadBalancerName" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String), "DnsName" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "LayerId" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "AvailabilityZones" :: NullOrUndefined (Strings), "SubnetIds" :: NullOrUndefined (Strings), "Ec2InstanceIds" :: NullOrUndefined (Strings) }
```

<p>Describes an Elastic Load Balancing instance.</p>

##### Instances
``` purescript
Newtype ElasticLoadBalancer _
```

#### `ElasticLoadBalancers`

``` purescript
newtype ElasticLoadBalancers
  = ElasticLoadBalancers (Array ElasticLoadBalancer)
```

##### Instances
``` purescript
Newtype ElasticLoadBalancers _
```

#### `EnvironmentVariable`

``` purescript
newtype EnvironmentVariable
  = EnvironmentVariable { "Key" :: String, "Value" :: String, "Secure" :: NullOrUndefined (Boolean) }
```

<p>Represents an app's environment variable.</p>

##### Instances
``` purescript
Newtype EnvironmentVariable _
```

#### `EnvironmentVariables`

``` purescript
newtype EnvironmentVariables
  = EnvironmentVariables (Array EnvironmentVariable)
```

##### Instances
``` purescript
Newtype EnvironmentVariables _
```

#### `GetHostnameSuggestionRequest`

``` purescript
newtype GetHostnameSuggestionRequest
  = GetHostnameSuggestionRequest { "LayerId" :: String }
```

##### Instances
``` purescript
Newtype GetHostnameSuggestionRequest _
```

#### `GetHostnameSuggestionResult`

``` purescript
newtype GetHostnameSuggestionResult
  = GetHostnameSuggestionResult { "LayerId" :: NullOrUndefined (String), "Hostname" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>GetHostnameSuggestion</code> request.</p>

##### Instances
``` purescript
Newtype GetHostnameSuggestionResult _
```

#### `GrantAccessRequest`

``` purescript
newtype GrantAccessRequest
  = GrantAccessRequest { "InstanceId" :: String, "ValidForInMinutes" :: NullOrUndefined (ValidForInMinutes) }
```

##### Instances
``` purescript
Newtype GrantAccessRequest _
```

#### `GrantAccessResult`

``` purescript
newtype GrantAccessResult
  = GrantAccessResult { "TemporaryCredential" :: NullOrUndefined (TemporaryCredential) }
```

<p>Contains the response to a <code>GrantAccess</code> request.</p>

##### Instances
``` purescript
Newtype GrantAccessResult _
```

#### `Hour`

``` purescript
newtype Hour
  = Hour String
```

##### Instances
``` purescript
Newtype Hour _
```

#### `Instance`

``` purescript
newtype Instance
  = Instance { "AgentVersion" :: NullOrUndefined (String), "AmiId" :: NullOrUndefined (String), "Architecture" :: NullOrUndefined (Architecture), "Arn" :: NullOrUndefined (String), "AutoScalingType" :: NullOrUndefined (AutoScalingType), "AvailabilityZone" :: NullOrUndefined (String), "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappings), "CreatedAt" :: NullOrUndefined (DateTime), "EbsOptimized" :: NullOrUndefined (Boolean), "Ec2InstanceId" :: NullOrUndefined (String), "EcsClusterArn" :: NullOrUndefined (String), "EcsContainerInstanceArn" :: NullOrUndefined (String), "ElasticIp" :: NullOrUndefined (String), "Hostname" :: NullOrUndefined (String), "InfrastructureClass" :: NullOrUndefined (String), "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean), "InstanceId" :: NullOrUndefined (String), "InstanceProfileArn" :: NullOrUndefined (String), "InstanceType" :: NullOrUndefined (String), "LastServiceErrorId" :: NullOrUndefined (String), "LayerIds" :: NullOrUndefined (Strings), "Os" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (String), "PrivateDns" :: NullOrUndefined (String), "PrivateIp" :: NullOrUndefined (String), "PublicDns" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String), "RegisteredBy" :: NullOrUndefined (String), "ReportedAgentVersion" :: NullOrUndefined (String), "ReportedOs" :: NullOrUndefined (ReportedOs), "RootDeviceType" :: NullOrUndefined (RootDeviceType), "RootDeviceVolumeId" :: NullOrUndefined (String), "SecurityGroupIds" :: NullOrUndefined (Strings), "SshHostDsaKeyFingerprint" :: NullOrUndefined (String), "SshHostRsaKeyFingerprint" :: NullOrUndefined (String), "SshKeyName" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "SubnetId" :: NullOrUndefined (String), "Tenancy" :: NullOrUndefined (String), "VirtualizationType" :: NullOrUndefined (VirtualizationType) }
```

<p>Describes an instance.</p>

##### Instances
``` purescript
Newtype Instance _
```

#### `InstanceIdentity`

``` purescript
newtype InstanceIdentity
  = InstanceIdentity { "Document" :: NullOrUndefined (String), "Signature" :: NullOrUndefined (String) }
```

<p>Contains a description of an Amazon EC2 instance from the Amazon EC2 metadata service. For more information, see <a href="http://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html">Instance Metadata and User Data</a>.</p>

##### Instances
``` purescript
Newtype InstanceIdentity _
```

#### `Instances`

``` purescript
newtype Instances
  = Instances (Array Instance)
```

##### Instances
``` purescript
Newtype Instances _
```

#### `InstancesCount`

``` purescript
newtype InstancesCount
  = InstancesCount { "Assigning" :: NullOrUndefined (Int), "Booting" :: NullOrUndefined (Int), "ConnectionLost" :: NullOrUndefined (Int), "Deregistering" :: NullOrUndefined (Int), "Online" :: NullOrUndefined (Int), "Pending" :: NullOrUndefined (Int), "Rebooting" :: NullOrUndefined (Int), "Registered" :: NullOrUndefined (Int), "Registering" :: NullOrUndefined (Int), "Requested" :: NullOrUndefined (Int), "RunningSetup" :: NullOrUndefined (Int), "SetupFailed" :: NullOrUndefined (Int), "ShuttingDown" :: NullOrUndefined (Int), "StartFailed" :: NullOrUndefined (Int), "StopFailed" :: NullOrUndefined (Int), "Stopped" :: NullOrUndefined (Int), "Stopping" :: NullOrUndefined (Int), "Terminated" :: NullOrUndefined (Int), "Terminating" :: NullOrUndefined (Int), "Unassigning" :: NullOrUndefined (Int) }
```

<p>Describes how many instances a stack has for each status.</p>

##### Instances
``` purescript
Newtype InstancesCount _
```

#### `Layer`

``` purescript
newtype Layer
  = Layer { "Arn" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "LayerId" :: NullOrUndefined (String), "Type" :: NullOrUndefined (LayerType), "Name" :: NullOrUndefined (String), "Shortname" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (LayerAttributes), "CloudWatchLogsConfiguration" :: NullOrUndefined (CloudWatchLogsConfiguration), "CustomInstanceProfileArn" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String), "CustomSecurityGroupIds" :: NullOrUndefined (Strings), "DefaultSecurityGroupNames" :: NullOrUndefined (Strings), "Packages" :: NullOrUndefined (Strings), "VolumeConfigurations" :: NullOrUndefined (VolumeConfigurations), "EnableAutoHealing" :: NullOrUndefined (Boolean), "AutoAssignElasticIps" :: NullOrUndefined (Boolean), "AutoAssignPublicIps" :: NullOrUndefined (Boolean), "DefaultRecipes" :: NullOrUndefined (Recipes), "CustomRecipes" :: NullOrUndefined (Recipes), "CreatedAt" :: NullOrUndefined (DateTime), "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean), "UseEbsOptimizedInstances" :: NullOrUndefined (Boolean), "LifecycleEventConfiguration" :: NullOrUndefined (LifecycleEventConfiguration) }
```

<p>Describes a layer.</p>

##### Instances
``` purescript
Newtype Layer _
```

#### `LayerAttributes`

``` purescript
newtype LayerAttributes
  = LayerAttributes (Map LayerAttributesKeys String)
```

##### Instances
``` purescript
Newtype LayerAttributes _
```

#### `LayerAttributesKeys`

``` purescript
newtype LayerAttributesKeys
  = LayerAttributesKeys String
```

##### Instances
``` purescript
Newtype LayerAttributesKeys _
```

#### `LayerType`

``` purescript
newtype LayerType
  = LayerType String
```

##### Instances
``` purescript
Newtype LayerType _
```

#### `Layers`

``` purescript
newtype Layers
  = Layers (Array Layer)
```

##### Instances
``` purescript
Newtype Layers _
```

#### `LifecycleEventConfiguration`

``` purescript
newtype LifecycleEventConfiguration
  = LifecycleEventConfiguration { "Shutdown" :: NullOrUndefined (ShutdownEventConfiguration) }
```

<p>Specifies the lifecycle event configuration</p>

##### Instances
``` purescript
Newtype LifecycleEventConfiguration _
```

#### `ListTagsRequest`

``` purescript
newtype ListTagsRequest
  = ListTagsRequest { "ResourceArn" :: ResourceArn, "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTagsRequest _
```

#### `ListTagsResult`

``` purescript
newtype ListTagsResult
  = ListTagsResult { "Tags" :: NullOrUndefined (Tags), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Contains the response to a <code>ListTags</code> request.</p>

##### Instances
``` purescript
Newtype ListTagsResult _
```

#### `LoadBasedAutoScalingConfiguration`

``` purescript
newtype LoadBasedAutoScalingConfiguration
  = LoadBasedAutoScalingConfiguration { "LayerId" :: NullOrUndefined (String), "Enable" :: NullOrUndefined (Boolean), "UpScaling" :: NullOrUndefined (AutoScalingThresholds), "DownScaling" :: NullOrUndefined (AutoScalingThresholds) }
```

<p>Describes a layer's load-based auto scaling configuration.</p>

##### Instances
``` purescript
Newtype LoadBasedAutoScalingConfiguration _
```

#### `LoadBasedAutoScalingConfigurations`

``` purescript
newtype LoadBasedAutoScalingConfigurations
  = LoadBasedAutoScalingConfigurations (Array LoadBasedAutoScalingConfiguration)
```

##### Instances
``` purescript
Newtype LoadBasedAutoScalingConfigurations _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `Minute`

``` purescript
newtype Minute
  = Minute Int
```

##### Instances
``` purescript
Newtype Minute _
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

#### `OperatingSystem`

``` purescript
newtype OperatingSystem
  = OperatingSystem { "Name" :: NullOrUndefined (String), "Id" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String), "ConfigurationManagers" :: NullOrUndefined (OperatingSystemConfigurationManagers), "ReportedName" :: NullOrUndefined (String), "ReportedVersion" :: NullOrUndefined (String), "Supported" :: NullOrUndefined (Boolean) }
```

<p>Describes supported operating systems in AWS OpsWorks Stacks.</p>

##### Instances
``` purescript
Newtype OperatingSystem _
```

#### `OperatingSystemConfigurationManager`

``` purescript
newtype OperatingSystemConfigurationManager
  = OperatingSystemConfigurationManager { "Name" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

<p>A block that contains information about the configuration manager (Chef) and the versions of the configuration manager that are supported for an operating system.</p>

##### Instances
``` purescript
Newtype OperatingSystemConfigurationManager _
```

#### `OperatingSystemConfigurationManagers`

``` purescript
newtype OperatingSystemConfigurationManagers
  = OperatingSystemConfigurationManagers (Array OperatingSystemConfigurationManager)
```

##### Instances
``` purescript
Newtype OperatingSystemConfigurationManagers _
```

#### `OperatingSystems`

``` purescript
newtype OperatingSystems
  = OperatingSystems (Array OperatingSystem)
```

##### Instances
``` purescript
Newtype OperatingSystems _
```

#### `Parameters`

``` purescript
newtype Parameters
  = Parameters (Map String String)
```

##### Instances
``` purescript
Newtype Parameters _
```

#### `Permission`

``` purescript
newtype Permission
  = Permission { "StackId" :: NullOrUndefined (String), "IamUserArn" :: NullOrUndefined (String), "AllowSsh" :: NullOrUndefined (Boolean), "AllowSudo" :: NullOrUndefined (Boolean), "Level" :: NullOrUndefined (String) }
```

<p>Describes stack or user permissions.</p>

##### Instances
``` purescript
Newtype Permission _
```

#### `Permissions`

``` purescript
newtype Permissions
  = Permissions (Array Permission)
```

##### Instances
``` purescript
Newtype Permissions _
```

#### `RaidArray`

``` purescript
newtype RaidArray
  = RaidArray { "RaidArrayId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "RaidLevel" :: NullOrUndefined (Int), "NumberOfDisks" :: NullOrUndefined (Int), "Size" :: NullOrUndefined (Int), "Device" :: NullOrUndefined (String), "MountPoint" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (DateTime), "StackId" :: NullOrUndefined (String), "VolumeType" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (Int) }
```

<p>Describes an instance's RAID array.</p>

##### Instances
``` purescript
Newtype RaidArray _
```

#### `RaidArrays`

``` purescript
newtype RaidArrays
  = RaidArrays (Array RaidArray)
```

##### Instances
``` purescript
Newtype RaidArrays _
```

#### `RdsDbInstance`

``` purescript
newtype RdsDbInstance
  = RdsDbInstance { "RdsDbInstanceArn" :: NullOrUndefined (String), "DbInstanceIdentifier" :: NullOrUndefined (String), "DbUser" :: NullOrUndefined (String), "DbPassword" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String), "Address" :: NullOrUndefined (String), "Engine" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "MissingOnRds" :: NullOrUndefined (Boolean) }
```

<p>Describes an Amazon RDS instance.</p>

##### Instances
``` purescript
Newtype RdsDbInstance _
```

#### `RdsDbInstances`

``` purescript
newtype RdsDbInstances
  = RdsDbInstances (Array RdsDbInstance)
```

##### Instances
``` purescript
Newtype RdsDbInstances _
```

#### `RebootInstanceRequest`

``` purescript
newtype RebootInstanceRequest
  = RebootInstanceRequest { "InstanceId" :: String }
```

##### Instances
``` purescript
Newtype RebootInstanceRequest _
```

#### `Recipes`

``` purescript
newtype Recipes
  = Recipes { "Setup" :: NullOrUndefined (Strings), "Configure" :: NullOrUndefined (Strings), "Deploy" :: NullOrUndefined (Strings), "Undeploy" :: NullOrUndefined (Strings), "Shutdown" :: NullOrUndefined (Strings) }
```

<p>AWS OpsWorks Stacks supports five lifecycle events: <b>setup</b>, <b>configuration</b>, <b>deploy</b>, <b>undeploy</b>, and <b>shutdown</b>. For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. In addition, you can provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. <code>LayerCustomRecipes</code> specifies the custom recipes for a particular layer to be run in response to each of the five events. </p> <p>To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the .rb extension. For example: phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository's phpapp2 folder.</p>

##### Instances
``` purescript
Newtype Recipes _
```

#### `RegisterEcsClusterRequest`

``` purescript
newtype RegisterEcsClusterRequest
  = RegisterEcsClusterRequest { "EcsClusterArn" :: String, "StackId" :: String }
```

##### Instances
``` purescript
Newtype RegisterEcsClusterRequest _
```

#### `RegisterEcsClusterResult`

``` purescript
newtype RegisterEcsClusterResult
  = RegisterEcsClusterResult { "EcsClusterArn" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>RegisterEcsCluster</code> request.</p>

##### Instances
``` purescript
Newtype RegisterEcsClusterResult _
```

#### `RegisterElasticIpRequest`

``` purescript
newtype RegisterElasticIpRequest
  = RegisterElasticIpRequest { "ElasticIp" :: String, "StackId" :: String }
```

##### Instances
``` purescript
Newtype RegisterElasticIpRequest _
```

#### `RegisterElasticIpResult`

``` purescript
newtype RegisterElasticIpResult
  = RegisterElasticIpResult { "ElasticIp" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>RegisterElasticIp</code> request.</p>

##### Instances
``` purescript
Newtype RegisterElasticIpResult _
```

#### `RegisterInstanceRequest`

``` purescript
newtype RegisterInstanceRequest
  = RegisterInstanceRequest { "StackId" :: String, "Hostname" :: NullOrUndefined (String), "PublicIp" :: NullOrUndefined (String), "PrivateIp" :: NullOrUndefined (String), "RsaPublicKey" :: NullOrUndefined (String), "RsaPublicKeyFingerprint" :: NullOrUndefined (String), "InstanceIdentity" :: NullOrUndefined (InstanceIdentity) }
```

##### Instances
``` purescript
Newtype RegisterInstanceRequest _
```

#### `RegisterInstanceResult`

``` purescript
newtype RegisterInstanceResult
  = RegisterInstanceResult { "InstanceId" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>RegisterInstanceResult</code> request.</p>

##### Instances
``` purescript
Newtype RegisterInstanceResult _
```

#### `RegisterRdsDbInstanceRequest`

``` purescript
newtype RegisterRdsDbInstanceRequest
  = RegisterRdsDbInstanceRequest { "StackId" :: String, "RdsDbInstanceArn" :: String, "DbUser" :: String, "DbPassword" :: String }
```

##### Instances
``` purescript
Newtype RegisterRdsDbInstanceRequest _
```

#### `RegisterVolumeRequest`

``` purescript
newtype RegisterVolumeRequest
  = RegisterVolumeRequest { "Ec2VolumeId" :: NullOrUndefined (String), "StackId" :: String }
```

##### Instances
``` purescript
Newtype RegisterVolumeRequest _
```

#### `RegisterVolumeResult`

``` purescript
newtype RegisterVolumeResult
  = RegisterVolumeResult { "VolumeId" :: NullOrUndefined (String) }
```

<p>Contains the response to a <code>RegisterVolume</code> request.</p>

##### Instances
``` purescript
Newtype RegisterVolumeResult _
```

#### `ReportedOs`

``` purescript
newtype ReportedOs
  = ReportedOs { "Family" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

<p>A registered instance's reported operating system.</p>

##### Instances
``` purescript
Newtype ReportedOs _
```

#### `ResourceArn`

``` purescript
newtype ResourceArn
  = ResourceArn String
```

##### Instances
``` purescript
Newtype ResourceArn _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (String) }
```

<p>Indicates that a resource was not found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `RootDeviceType`

``` purescript
newtype RootDeviceType
  = RootDeviceType String
```

##### Instances
``` purescript
Newtype RootDeviceType _
```

#### `SelfUserProfile`

``` purescript
newtype SelfUserProfile
  = SelfUserProfile { "IamUserArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SshUsername" :: NullOrUndefined (String), "SshPublicKey" :: NullOrUndefined (String) }
```

<p>Describes a user's SSH information.</p>

##### Instances
``` purescript
Newtype SelfUserProfile _
```

#### `ServiceError`

``` purescript
newtype ServiceError
  = ServiceError { "ServiceErrorId" :: NullOrUndefined (String), "StackId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String), "Message" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (DateTime) }
```

<p>Describes an AWS OpsWorks Stacks service error.</p>

##### Instances
``` purescript
Newtype ServiceError _
```

#### `ServiceErrors`

``` purescript
newtype ServiceErrors
  = ServiceErrors (Array ServiceError)
```

##### Instances
``` purescript
Newtype ServiceErrors _
```

#### `SetLoadBasedAutoScalingRequest`

``` purescript
newtype SetLoadBasedAutoScalingRequest
  = SetLoadBasedAutoScalingRequest { "LayerId" :: String, "Enable" :: NullOrUndefined (Boolean), "UpScaling" :: NullOrUndefined (AutoScalingThresholds), "DownScaling" :: NullOrUndefined (AutoScalingThresholds) }
```

##### Instances
``` purescript
Newtype SetLoadBasedAutoScalingRequest _
```

#### `SetPermissionRequest`

``` purescript
newtype SetPermissionRequest
  = SetPermissionRequest { "StackId" :: String, "IamUserArn" :: String, "AllowSsh" :: NullOrUndefined (Boolean), "AllowSudo" :: NullOrUndefined (Boolean), "Level" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype SetPermissionRequest _
```

#### `SetTimeBasedAutoScalingRequest`

``` purescript
newtype SetTimeBasedAutoScalingRequest
  = SetTimeBasedAutoScalingRequest { "InstanceId" :: String, "AutoScalingSchedule" :: NullOrUndefined (WeeklyAutoScalingSchedule) }
```

##### Instances
``` purescript
Newtype SetTimeBasedAutoScalingRequest _
```

#### `ShutdownEventConfiguration`

``` purescript
newtype ShutdownEventConfiguration
  = ShutdownEventConfiguration { "ExecutionTimeout" :: NullOrUndefined (Int), "DelayUntilElbConnectionsDrained" :: NullOrUndefined (Boolean) }
```

<p>The Shutdown event configuration.</p>

##### Instances
``` purescript
Newtype ShutdownEventConfiguration _
```

#### `Source`

``` purescript
newtype Source
  = Source { "Type" :: NullOrUndefined (SourceType), "Url" :: NullOrUndefined (String), "Username" :: NullOrUndefined (String), "Password" :: NullOrUndefined (String), "SshKey" :: NullOrUndefined (String), "Revision" :: NullOrUndefined (String) }
```

<p>Contains the information required to retrieve an app or cookbook from a repository. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html">Creating Apps</a> or <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html">Custom Recipes and Cookbooks</a>.</p>

##### Instances
``` purescript
Newtype Source _
```

#### `SourceType`

``` purescript
newtype SourceType
  = SourceType String
```

##### Instances
``` purescript
Newtype SourceType _
```

#### `SslConfiguration`

``` purescript
newtype SslConfiguration
  = SslConfiguration { "Certificate" :: String, "PrivateKey" :: String, "Chain" :: NullOrUndefined (String) }
```

<p>Describes an app's SSL configuration.</p>

##### Instances
``` purescript
Newtype SslConfiguration _
```

#### `Stack`

``` purescript
newtype Stack
  = Stack { "StackId" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Arn" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String), "VpcId" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (StackAttributes), "ServiceRoleArn" :: NullOrUndefined (String), "DefaultInstanceProfileArn" :: NullOrUndefined (String), "DefaultOs" :: NullOrUndefined (String), "HostnameTheme" :: NullOrUndefined (String), "DefaultAvailabilityZone" :: NullOrUndefined (String), "DefaultSubnetId" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String), "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager), "ChefConfiguration" :: NullOrUndefined (ChefConfiguration), "UseCustomCookbooks" :: NullOrUndefined (Boolean), "UseOpsworksSecurityGroups" :: NullOrUndefined (Boolean), "CustomCookbooksSource" :: NullOrUndefined (Source), "DefaultSshKeyName" :: NullOrUndefined (String), "CreatedAt" :: NullOrUndefined (DateTime), "DefaultRootDeviceType" :: NullOrUndefined (RootDeviceType), "AgentVersion" :: NullOrUndefined (String) }
```

<p>Describes a stack.</p>

##### Instances
``` purescript
Newtype Stack _
```

#### `StackAttributes`

``` purescript
newtype StackAttributes
  = StackAttributes (Map StackAttributesKeys String)
```

##### Instances
``` purescript
Newtype StackAttributes _
```

#### `StackAttributesKeys`

``` purescript
newtype StackAttributesKeys
  = StackAttributesKeys String
```

##### Instances
``` purescript
Newtype StackAttributesKeys _
```

#### `StackConfigurationManager`

``` purescript
newtype StackConfigurationManager
  = StackConfigurationManager { "Name" :: NullOrUndefined (String), "Version" :: NullOrUndefined (String) }
```

<p>Describes the configuration manager.</p>

##### Instances
``` purescript
Newtype StackConfigurationManager _
```

#### `StackSummary`

``` purescript
newtype StackSummary
  = StackSummary { "StackId" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "Arn" :: NullOrUndefined (String), "LayersCount" :: NullOrUndefined (Int), "AppsCount" :: NullOrUndefined (Int), "InstancesCount" :: NullOrUndefined (InstancesCount) }
```

<p>Summarizes the number of layers, instances, and apps in a stack.</p>

##### Instances
``` purescript
Newtype StackSummary _
```

#### `Stacks`

``` purescript
newtype Stacks
  = Stacks (Array Stack)
```

##### Instances
``` purescript
Newtype Stacks _
```

#### `StartInstanceRequest`

``` purescript
newtype StartInstanceRequest
  = StartInstanceRequest { "InstanceId" :: String }
```

##### Instances
``` purescript
Newtype StartInstanceRequest _
```

#### `StartStackRequest`

``` purescript
newtype StartStackRequest
  = StartStackRequest { "StackId" :: String }
```

##### Instances
``` purescript
Newtype StartStackRequest _
```

#### `StopInstanceRequest`

``` purescript
newtype StopInstanceRequest
  = StopInstanceRequest { "InstanceId" :: String, "Force" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype StopInstanceRequest _
```

#### `StopStackRequest`

``` purescript
newtype StopStackRequest
  = StopStackRequest { "StackId" :: String }
```

##### Instances
``` purescript
Newtype StopStackRequest _
```

#### `Strings`

``` purescript
newtype Strings
  = Strings (Array String)
```

##### Instances
``` purescript
Newtype Strings _
```

#### `Switch`

``` purescript
newtype Switch
  = Switch String
```

##### Instances
``` purescript
Newtype Switch _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeys _
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "ResourceArn" :: ResourceArn, "Tags" :: Tags }
```

##### Instances
``` purescript
Newtype TagResourceRequest _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Map TagKey TagValue)
```

##### Instances
``` purescript
Newtype Tags _
```

#### `TemporaryCredential`

``` purescript
newtype TemporaryCredential
  = TemporaryCredential { "Username" :: NullOrUndefined (String), "Password" :: NullOrUndefined (String), "ValidForInMinutes" :: NullOrUndefined (Int), "InstanceId" :: NullOrUndefined (String) }
```

<p>Contains the data needed by RDP clients such as the Microsoft Remote Desktop Connection to log in to the instance.</p>

##### Instances
``` purescript
Newtype TemporaryCredential _
```

#### `TimeBasedAutoScalingConfiguration`

``` purescript
newtype TimeBasedAutoScalingConfiguration
  = TimeBasedAutoScalingConfiguration { "InstanceId" :: NullOrUndefined (String), "AutoScalingSchedule" :: NullOrUndefined (WeeklyAutoScalingSchedule) }
```

<p>Describes an instance's time-based auto scaling configuration.</p>

##### Instances
``` purescript
Newtype TimeBasedAutoScalingConfiguration _
```

#### `TimeBasedAutoScalingConfigurations`

``` purescript
newtype TimeBasedAutoScalingConfigurations
  = TimeBasedAutoScalingConfigurations (Array TimeBasedAutoScalingConfiguration)
```

##### Instances
``` purescript
Newtype TimeBasedAutoScalingConfigurations _
```

#### `UnassignInstanceRequest`

``` purescript
newtype UnassignInstanceRequest
  = UnassignInstanceRequest { "InstanceId" :: String }
```

##### Instances
``` purescript
Newtype UnassignInstanceRequest _
```

#### `UnassignVolumeRequest`

``` purescript
newtype UnassignVolumeRequest
  = UnassignVolumeRequest { "VolumeId" :: String }
```

##### Instances
``` purescript
Newtype UnassignVolumeRequest _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "ResourceArn" :: ResourceArn, "TagKeys" :: TagKeys }
```

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UpdateAppRequest`

``` purescript
newtype UpdateAppRequest
  = UpdateAppRequest { "AppId" :: String, "Name" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "DataSources" :: NullOrUndefined (DataSources), "Type" :: NullOrUndefined (AppType), "AppSource" :: NullOrUndefined (Source), "Domains" :: NullOrUndefined (Strings), "EnableSsl" :: NullOrUndefined (Boolean), "SslConfiguration" :: NullOrUndefined (SslConfiguration), "Attributes" :: NullOrUndefined (AppAttributes), "Environment" :: NullOrUndefined (EnvironmentVariables) }
```

##### Instances
``` purescript
Newtype UpdateAppRequest _
```

#### `UpdateElasticIpRequest`

``` purescript
newtype UpdateElasticIpRequest
  = UpdateElasticIpRequest { "ElasticIp" :: String, "Name" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateElasticIpRequest _
```

#### `UpdateInstanceRequest`

``` purescript
newtype UpdateInstanceRequest
  = UpdateInstanceRequest { "InstanceId" :: String, "LayerIds" :: NullOrUndefined (Strings), "InstanceType" :: NullOrUndefined (String), "AutoScalingType" :: NullOrUndefined (AutoScalingType), "Hostname" :: NullOrUndefined (String), "Os" :: NullOrUndefined (String), "AmiId" :: NullOrUndefined (String), "SshKeyName" :: NullOrUndefined (String), "Architecture" :: NullOrUndefined (Architecture), "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean), "EbsOptimized" :: NullOrUndefined (Boolean), "AgentVersion" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateInstanceRequest _
```

#### `UpdateLayerRequest`

``` purescript
newtype UpdateLayerRequest
  = UpdateLayerRequest { "LayerId" :: String, "Name" :: NullOrUndefined (String), "Shortname" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (LayerAttributes), "CloudWatchLogsConfiguration" :: NullOrUndefined (CloudWatchLogsConfiguration), "CustomInstanceProfileArn" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String), "CustomSecurityGroupIds" :: NullOrUndefined (Strings), "Packages" :: NullOrUndefined (Strings), "VolumeConfigurations" :: NullOrUndefined (VolumeConfigurations), "EnableAutoHealing" :: NullOrUndefined (Boolean), "AutoAssignElasticIps" :: NullOrUndefined (Boolean), "AutoAssignPublicIps" :: NullOrUndefined (Boolean), "CustomRecipes" :: NullOrUndefined (Recipes), "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean), "UseEbsOptimizedInstances" :: NullOrUndefined (Boolean), "LifecycleEventConfiguration" :: NullOrUndefined (LifecycleEventConfiguration) }
```

##### Instances
``` purescript
Newtype UpdateLayerRequest _
```

#### `UpdateMyUserProfileRequest`

``` purescript
newtype UpdateMyUserProfileRequest
  = UpdateMyUserProfileRequest { "SshPublicKey" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateMyUserProfileRequest _
```

#### `UpdateRdsDbInstanceRequest`

``` purescript
newtype UpdateRdsDbInstanceRequest
  = UpdateRdsDbInstanceRequest { "RdsDbInstanceArn" :: String, "DbUser" :: NullOrUndefined (String), "DbPassword" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateRdsDbInstanceRequest _
```

#### `UpdateStackRequest`

``` purescript
newtype UpdateStackRequest
  = UpdateStackRequest { "StackId" :: String, "Name" :: NullOrUndefined (String), "Attributes" :: NullOrUndefined (StackAttributes), "ServiceRoleArn" :: NullOrUndefined (String), "DefaultInstanceProfileArn" :: NullOrUndefined (String), "DefaultOs" :: NullOrUndefined (String), "HostnameTheme" :: NullOrUndefined (String), "DefaultAvailabilityZone" :: NullOrUndefined (String), "DefaultSubnetId" :: NullOrUndefined (String), "CustomJson" :: NullOrUndefined (String), "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager), "ChefConfiguration" :: NullOrUndefined (ChefConfiguration), "UseCustomCookbooks" :: NullOrUndefined (Boolean), "CustomCookbooksSource" :: NullOrUndefined (Source), "DefaultSshKeyName" :: NullOrUndefined (String), "DefaultRootDeviceType" :: NullOrUndefined (RootDeviceType), "UseOpsworksSecurityGroups" :: NullOrUndefined (Boolean), "AgentVersion" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateStackRequest _
```

#### `UpdateUserProfileRequest`

``` purescript
newtype UpdateUserProfileRequest
  = UpdateUserProfileRequest { "IamUserArn" :: String, "SshUsername" :: NullOrUndefined (String), "SshPublicKey" :: NullOrUndefined (String), "AllowSelfManagement" :: NullOrUndefined (Boolean) }
```

##### Instances
``` purescript
Newtype UpdateUserProfileRequest _
```

#### `UpdateVolumeRequest`

``` purescript
newtype UpdateVolumeRequest
  = UpdateVolumeRequest { "VolumeId" :: String, "Name" :: NullOrUndefined (String), "MountPoint" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype UpdateVolumeRequest _
```

#### `UserProfile`

``` purescript
newtype UserProfile
  = UserProfile { "IamUserArn" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "SshUsername" :: NullOrUndefined (String), "SshPublicKey" :: NullOrUndefined (String), "AllowSelfManagement" :: NullOrUndefined (Boolean) }
```

<p>Describes a user's SSH information.</p>

##### Instances
``` purescript
Newtype UserProfile _
```

#### `UserProfiles`

``` purescript
newtype UserProfiles
  = UserProfiles (Array UserProfile)
```

##### Instances
``` purescript
Newtype UserProfiles _
```

#### `ValidForInMinutes`

``` purescript
newtype ValidForInMinutes
  = ValidForInMinutes Int
```

##### Instances
``` purescript
Newtype ValidForInMinutes _
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message'" :: NullOrUndefined (String) }
```

<p>Indicates that a request was not valid.</p>

##### Instances
``` purescript
Newtype ValidationException _
```

#### `VirtualizationType`

``` purescript
newtype VirtualizationType
  = VirtualizationType String
```

##### Instances
``` purescript
Newtype VirtualizationType _
```

#### `Volume`

``` purescript
newtype Volume
  = Volume { "VolumeId" :: NullOrUndefined (String), "Ec2VolumeId" :: NullOrUndefined (String), "Name" :: NullOrUndefined (String), "RaidArrayId" :: NullOrUndefined (String), "InstanceId" :: NullOrUndefined (String), "Status" :: NullOrUndefined (String), "Size" :: NullOrUndefined (Int), "Device" :: NullOrUndefined (String), "MountPoint" :: NullOrUndefined (String), "Region" :: NullOrUndefined (String), "AvailabilityZone" :: NullOrUndefined (String), "VolumeType" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (Int), "Encrypted" :: NullOrUndefined (Boolean) }
```

<p>Describes an instance's Amazon EBS volume.</p>

##### Instances
``` purescript
Newtype Volume _
```

#### `VolumeConfiguration`

``` purescript
newtype VolumeConfiguration
  = VolumeConfiguration { "MountPoint" :: String, "RaidLevel" :: NullOrUndefined (Int), "NumberOfDisks" :: Int, "Size" :: Int, "VolumeType" :: NullOrUndefined (String), "Iops" :: NullOrUndefined (Int), "Encrypted" :: NullOrUndefined (Boolean) }
```

<p>Describes an Amazon EBS volume configuration.</p>

##### Instances
``` purescript
Newtype VolumeConfiguration _
```

#### `VolumeConfigurations`

``` purescript
newtype VolumeConfigurations
  = VolumeConfigurations (Array VolumeConfiguration)
```

##### Instances
``` purescript
Newtype VolumeConfigurations _
```

#### `VolumeType`

``` purescript
newtype VolumeType
  = VolumeType String
```

##### Instances
``` purescript
Newtype VolumeType _
```

#### `Volumes`

``` purescript
newtype Volumes
  = Volumes (Array Volume)
```

##### Instances
``` purescript
Newtype Volumes _
```

#### `WeeklyAutoScalingSchedule`

``` purescript
newtype WeeklyAutoScalingSchedule
  = WeeklyAutoScalingSchedule { "Monday" :: NullOrUndefined (DailyAutoScalingSchedule), "Tuesday" :: NullOrUndefined (DailyAutoScalingSchedule), "Wednesday" :: NullOrUndefined (DailyAutoScalingSchedule), "Thursday" :: NullOrUndefined (DailyAutoScalingSchedule), "Friday" :: NullOrUndefined (DailyAutoScalingSchedule), "Saturday" :: NullOrUndefined (DailyAutoScalingSchedule), "Sunday" :: NullOrUndefined (DailyAutoScalingSchedule) }
```

<p>Describes a time-based instance's auto scaling schedule. The schedule consists of a set of key-value pairs.</p> <ul> <li> <p>The key is the time period (a UTC hour) and must be an integer from 0 - 23.</p> </li> <li> <p>The value indicates whether the instance should be online or offline for the specified period, and must be set to "on" or "off"</p> </li> </ul> <p>The default setting for all time periods is off, so you use the following parameters primarily to specify the online periods. You don't have to explicitly specify offline periods unless you want to change an online period to an offline period.</p> <p>The following example specifies that the instance should be online for four hours, from UTC 1200 - 1600. It will be off for the remainder of the day.</p> <p> <code> { "12":"on", "13":"on", "14":"on", "15":"on" } </code> </p>

##### Instances
``` purescript
Newtype WeeklyAutoScalingSchedule _
```


