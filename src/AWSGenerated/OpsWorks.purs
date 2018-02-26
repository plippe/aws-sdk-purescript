

-- | <fullname>AWS OpsWorks</fullname> <p>Welcome to the <i>AWS OpsWorks Stacks API Reference</i>. This guide provides descriptions, syntax, and usage examples for AWS OpsWorks Stacks actions and data types, including common parameters and error codes. </p> <p>AWS OpsWorks Stacks is an application management service that provides an integrated experience for overseeing the complete application lifecycle. For information about this product, go to the <a href="http://aws.amazon.com/opsworks/">AWS OpsWorks</a> details page. </p> <p> <b>SDKs and CLI</b> </p> <p>The most common way to use the AWS OpsWorks Stacks API is by using the AWS Command Line Interface (CLI) or by using one of the AWS SDKs to implement applications in your preferred language. For more information, see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html">AWS CLI</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html">AWS SDK for Java</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm">AWS SDK for .NET</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html">AWS SDK for PHP 2</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/sdkforruby/api/">AWS SDK for Ruby</a> </p> </li> <li> <p> <a href="http://aws.amazon.com/documentation/sdkforjavascript/">AWS SDK for Node.js</a> </p> </li> <li> <p> <a href="http://docs.pythonboto.org/en/latest/ref/opsworks.html">AWS SDK for Python(Boto)</a> </p> </li> </ul> <p> <b>Endpoints</b> </p> <p>AWS OpsWorks Stacks supports the following endpoints, all HTTPS. You must connect to one of the following endpoints. Stacks can only be accessed or managed within the endpoint in which they are created.</p> <ul> <li> <p>opsworks.us-east-1.amazonaws.com</p> </li> <li> <p>opsworks.us-east-2.amazonaws.com</p> </li> <li> <p>opsworks.us-west-1.amazonaws.com</p> </li> <li> <p>opsworks.us-west-2.amazonaws.com</p> </li> <li> <p>opsworks.ca-central-1.amazonaws.com (API only; not available in the AWS console)</p> </li> <li> <p>opsworks.eu-west-1.amazonaws.com</p> </li> <li> <p>opsworks.eu-west-2.amazonaws.com</p> </li> <li> <p>opsworks.eu-west-3.amazonaws.com</p> </li> <li> <p>opsworks.eu-central-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-northeast-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-northeast-2.amazonaws.com</p> </li> <li> <p>opsworks.ap-south-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-southeast-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-southeast-2.amazonaws.com</p> </li> <li> <p>opsworks.sa-east-1.amazonaws.com</p> </li> </ul> <p> <b>Chef Versions</b> </p> <p>When you call <a>CreateStack</a>, <a>CloneStack</a>, or <a>UpdateStack</a> we recommend you use the <code>ConfigurationManager</code> parameter to specify the Chef version. The recommended and default value for Linux stacks is currently 12. Windows stacks use Chef 12.2. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html">Chef Versions</a>.</p> <note> <p>You can specify Chef 12, 11.10, or 11.4 for your Linux stack. We recommend migrating your existing Linux stacks to Chef 12 as soon as possible.</p> </note>
module AWS.OpsWorks where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "OpsWorks" :: String


-- | <p>Assign a registered instance to a layer.</p> <ul> <li> <p>You can assign registered on-premises instances to any layer type.</p> </li> <li> <p>You can assign registered Amazon EC2 instances only to custom layers.</p> </li> <li> <p>You cannot use this action with instances that were created with AWS OpsWorks Stacks.</p> </li> </ul> <p> <b>Required Permissions</b>: To use this action, an AWS Identity and Access Management (IAM) user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
assignInstance :: forall eff. AssignInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
assignInstance = AWS.request serviceName "AssignInstance" 


-- | <p>Assigns one of the stack's registered Amazon EBS volumes to a specified instance. The volume must first be registered with the stack by calling <a>RegisterVolume</a>. After you register the volume, you must call <a>UpdateVolume</a> to specify a mount point before calling <code>AssignVolume</code>. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
assignVolume :: forall eff. AssignVolumeRequest -> Aff (err :: AWS.RequestError | eff) Unit
assignVolume = AWS.request serviceName "AssignVolume" 


-- | <p>Associates one of the stack's registered Elastic IP addresses with a specified instance. The address must first be registered with the stack by calling <a>RegisterElasticIp</a>. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
associateElasticIp :: forall eff. AssociateElasticIpRequest -> Aff (err :: AWS.RequestError | eff) Unit
associateElasticIp = AWS.request serviceName "AssociateElasticIp" 


-- | <p>Attaches an Elastic Load Balancing load balancer to a specified layer. AWS OpsWorks Stacks does not support Application Load Balancer. You can only use Classic Load Balancer with AWS OpsWorks Stacks. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/layers-elb.html">Elastic Load Balancing</a>.</p> <note> <p>You must create the Elastic Load Balancing instance separately, by using the Elastic Load Balancing console, API, or CLI. For more information, see <a href="http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/Welcome.html"> Elastic Load Balancing Developer Guide</a>.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
attachElasticLoadBalancer :: forall eff. AttachElasticLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) Unit
attachElasticLoadBalancer = AWS.request serviceName "AttachElasticLoadBalancer" 


-- | <p>Creates a clone of a specified stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-cloning.html">Clone a Stack</a>. By default, all parameters are set to the values used by the parent stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
cloneStack :: forall eff. CloneStackRequest -> Aff (err :: AWS.RequestError | eff) CloneStackResult
cloneStack = AWS.request serviceName "CloneStack" 


-- | <p>Creates an app for a specified stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html">Creating Apps</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
createApp :: forall eff. CreateAppRequest -> Aff (err :: AWS.RequestError | eff) CreateAppResult
createApp = AWS.request serviceName "CreateApp" 


-- | <p>Runs deployment or stack commands. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-deploying.html">Deploying Apps</a> and <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-commands.html">Run Stack Commands</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
createDeployment :: forall eff. CreateDeploymentRequest -> Aff (err :: AWS.RequestError | eff) CreateDeploymentResult
createDeployment = AWS.request serviceName "CreateDeployment" 


-- | <p>Creates an instance in a specified stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-add.html">Adding an Instance to a Layer</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
createInstance :: forall eff. CreateInstanceRequest -> Aff (err :: AWS.RequestError | eff) CreateInstanceResult
createInstance = AWS.request serviceName "CreateInstance" 


-- | <p>Creates a layer. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-create.html">How to Create a Layer</a>.</p> <note> <p>You should use <b>CreateLayer</b> for noncustom layer types such as PHP App Server only if the stack does not have an existing layer of that type. A stack can have at most one instance of each noncustom layer; if you attempt to create a second instance, <b>CreateLayer</b> fails. A stack can have an arbitrary number of custom layers, so you can call <b>CreateLayer</b> as many times as you like for that layer type.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
createLayer :: forall eff. CreateLayerRequest -> Aff (err :: AWS.RequestError | eff) CreateLayerResult
createLayer = AWS.request serviceName "CreateLayer" 


-- | <p>Creates a new stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-edit.html">Create a New Stack</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
createStack :: forall eff. CreateStackRequest -> Aff (err :: AWS.RequestError | eff) CreateStackResult
createStack = AWS.request serviceName "CreateStack" 


-- | <p>Creates a new user profile.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
createUserProfile :: forall eff. CreateUserProfileRequest -> Aff (err :: AWS.RequestError | eff) CreateUserProfileResult
createUserProfile = AWS.request serviceName "CreateUserProfile" 


-- | <p>Deletes a specified app.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deleteApp :: forall eff. DeleteAppRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteApp = AWS.request serviceName "DeleteApp" 


-- | <p>Deletes a specified instance, which terminates the associated Amazon EC2 instance. You must stop an instance before you can delete it.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-delete.html">Deleting Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deleteInstance :: forall eff. DeleteInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteInstance = AWS.request serviceName "DeleteInstance" 


-- | <p>Deletes a specified layer. You must first stop and then delete all associated instances or unassign registered instances. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-basics-delete.html">How to Delete a Layer</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deleteLayer :: forall eff. DeleteLayerRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteLayer = AWS.request serviceName "DeleteLayer" 


-- | <p>Deletes a specified stack. You must first delete all instances, layers, and apps or deregister registered instances. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingstacks-shutting.html">Shut Down a Stack</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deleteStack :: forall eff. DeleteStackRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteStack = AWS.request serviceName "DeleteStack" 


-- | <p>Deletes a user profile.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deleteUserProfile :: forall eff. DeleteUserProfileRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteUserProfile = AWS.request serviceName "DeleteUserProfile" 


-- | <p>Deregisters a specified Amazon ECS cluster from a stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html#workinglayers-ecscluster-delete"> Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html</a>.</p>
deregisterEcsCluster :: forall eff. DeregisterEcsClusterRequest -> Aff (err :: AWS.RequestError | eff) Unit
deregisterEcsCluster = AWS.request serviceName "DeregisterEcsCluster" 


-- | <p>Deregisters a specified Elastic IP address. The address can then be registered by another stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deregisterElasticIp :: forall eff. DeregisterElasticIpRequest -> Aff (err :: AWS.RequestError | eff) Unit
deregisterElasticIp = AWS.request serviceName "DeregisterElasticIp" 


-- | <p>Deregister a registered Amazon EC2 or on-premises instance. This action removes the instance from the stack and returns it to your control. This action can not be used with instances that were created with AWS OpsWorks Stacks.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deregisterInstance :: forall eff. DeregisterInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
deregisterInstance = AWS.request serviceName "DeregisterInstance" 


-- | <p>Deregisters an Amazon RDS instance.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deregisterRdsDbInstance :: forall eff. DeregisterRdsDbInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
deregisterRdsDbInstance = AWS.request serviceName "DeregisterRdsDbInstance" 


-- | <p>Deregisters an Amazon EBS volume. The volume can then be registered by another stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
deregisterVolume :: forall eff. DeregisterVolumeRequest -> Aff (err :: AWS.RequestError | eff) Unit
deregisterVolume = AWS.request serviceName "DeregisterVolume" 


-- | <p>Describes the available AWS OpsWorks Stacks agent versions. You must specify a stack ID or a configuration manager. <code>DescribeAgentVersions</code> returns a list of available agent versions for the specified stack or configuration manager.</p>
describeAgentVersions :: forall eff. DescribeAgentVersionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeAgentVersionsResult
describeAgentVersions = AWS.request serviceName "DescribeAgentVersions" 


-- | <p>Requests a description of a specified set of apps.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeApps :: forall eff. DescribeAppsRequest -> Aff (err :: AWS.RequestError | eff) DescribeAppsResult
describeApps = AWS.request serviceName "DescribeApps" 


-- | <p>Describes the results of specified commands.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeCommands :: forall eff. DescribeCommandsRequest -> Aff (err :: AWS.RequestError | eff) DescribeCommandsResult
describeCommands = AWS.request serviceName "DescribeCommands" 


-- | <p>Requests a description of a specified set of deployments.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeDeployments :: forall eff. DescribeDeploymentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDeploymentsResult
describeDeployments = AWS.request serviceName "DescribeDeployments" 


-- | <p>Describes Amazon ECS clusters that are registered with a stack. If you specify only a stack ID, you can use the <code>MaxResults</code> and <code>NextToken</code> parameters to paginate the response. However, AWS OpsWorks Stacks currently supports only one cluster per layer, so the result set has a maximum of one element.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permission. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p> <p>This call accepts only one resource-identifying parameter.</p>
describeEcsClusters :: forall eff. DescribeEcsClustersRequest -> Aff (err :: AWS.RequestError | eff) DescribeEcsClustersResult
describeEcsClusters = AWS.request serviceName "DescribeEcsClusters" 


-- | <p>Describes <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html">Elastic IP addresses</a>.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeElasticIps :: forall eff. DescribeElasticIpsRequest -> Aff (err :: AWS.RequestError | eff) DescribeElasticIpsResult
describeElasticIps = AWS.request serviceName "DescribeElasticIps" 


-- | <p>Describes a stack's Elastic Load Balancing instances.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeElasticLoadBalancers :: forall eff. DescribeElasticLoadBalancersRequest -> Aff (err :: AWS.RequestError | eff) DescribeElasticLoadBalancersResult
describeElasticLoadBalancers = AWS.request serviceName "DescribeElasticLoadBalancers" 


-- | <p>Requests a description of a set of instances.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeInstances :: forall eff. DescribeInstancesRequest -> Aff (err :: AWS.RequestError | eff) DescribeInstancesResult
describeInstances = AWS.request serviceName "DescribeInstances" 


-- | <p>Requests a description of one or more layers in a specified stack.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeLayers :: forall eff. DescribeLayersRequest -> Aff (err :: AWS.RequestError | eff) DescribeLayersResult
describeLayers = AWS.request serviceName "DescribeLayers" 


-- | <p>Describes load-based auto scaling configurations for specified layers.</p> <note> <p>You must specify at least one of the parameters.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeLoadBasedAutoScaling :: forall eff. DescribeLoadBasedAutoScalingRequest -> Aff (err :: AWS.RequestError | eff) DescribeLoadBasedAutoScalingResult
describeLoadBasedAutoScaling = AWS.request serviceName "DescribeLoadBasedAutoScaling" 


-- | <p>Describes a user's SSH information.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have self-management enabled or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeMyUserProfile :: forall eff.  Aff (err :: AWS.RequestError | eff) DescribeMyUserProfileResult
describeMyUserProfile = AWS.request serviceName "DescribeMyUserProfile" unit


-- | <p>Describes the operating systems that are supported by AWS OpsWorks Stacks.</p>
describeOperatingSystems :: forall eff.  Aff (err :: AWS.RequestError | eff) DescribeOperatingSystemsResponse
describeOperatingSystems = AWS.request serviceName "DescribeOperatingSystems" unit


-- | <p>Describes the permissions for a specified stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describePermissions :: forall eff. DescribePermissionsRequest -> Aff (err :: AWS.RequestError | eff) DescribePermissionsResult
describePermissions = AWS.request serviceName "DescribePermissions" 


-- | <p>Describe an instance's RAID arrays.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeRaidArrays :: forall eff. DescribeRaidArraysRequest -> Aff (err :: AWS.RequestError | eff) DescribeRaidArraysResult
describeRaidArrays = AWS.request serviceName "DescribeRaidArrays" 


-- | <p>Describes Amazon RDS instances.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p> <p>This call accepts only one resource-identifying parameter.</p>
describeRdsDbInstances :: forall eff. DescribeRdsDbInstancesRequest -> Aff (err :: AWS.RequestError | eff) DescribeRdsDbInstancesResult
describeRdsDbInstances = AWS.request serviceName "DescribeRdsDbInstances" 


-- | <p>Describes AWS OpsWorks Stacks service errors.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p> <p>This call accepts only one resource-identifying parameter.</p>
describeServiceErrors :: forall eff. DescribeServiceErrorsRequest -> Aff (err :: AWS.RequestError | eff) DescribeServiceErrorsResult
describeServiceErrors = AWS.request serviceName "DescribeServiceErrors" 


-- | <p>Requests a description of a stack's provisioning parameters.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeStackProvisioningParameters :: forall eff. DescribeStackProvisioningParametersRequest -> Aff (err :: AWS.RequestError | eff) DescribeStackProvisioningParametersResult
describeStackProvisioningParameters = AWS.request serviceName "DescribeStackProvisioningParameters" 


-- | <p>Describes the number of layers and apps in a specified stack, and the number of instances in each state, such as <code>running_setup</code> or <code>online</code>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeStackSummary :: forall eff. DescribeStackSummaryRequest -> Aff (err :: AWS.RequestError | eff) DescribeStackSummaryResult
describeStackSummary = AWS.request serviceName "DescribeStackSummary" 


-- | <p>Requests a description of one or more stacks.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeStacks :: forall eff. DescribeStacksRequest -> Aff (err :: AWS.RequestError | eff) DescribeStacksResult
describeStacks = AWS.request serviceName "DescribeStacks" 


-- | <p>Describes time-based auto scaling configurations for specified instances.</p> <note> <p>You must specify at least one of the parameters.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeTimeBasedAutoScaling :: forall eff. DescribeTimeBasedAutoScalingRequest -> Aff (err :: AWS.RequestError | eff) DescribeTimeBasedAutoScalingResult
describeTimeBasedAutoScaling = AWS.request serviceName "DescribeTimeBasedAutoScaling" 


-- | <p>Describe specified users.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeUserProfiles :: forall eff. DescribeUserProfilesRequest -> Aff (err :: AWS.RequestError | eff) DescribeUserProfilesResult
describeUserProfiles = AWS.request serviceName "DescribeUserProfiles" 


-- | <p>Describes an instance's Amazon EBS volumes.</p> <note> <p>This call accepts only one resource-identifying parameter.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
describeVolumes :: forall eff. DescribeVolumesRequest -> Aff (err :: AWS.RequestError | eff) DescribeVolumesResult
describeVolumes = AWS.request serviceName "DescribeVolumes" 


-- | <p>Detaches a specified Elastic Load Balancing instance from its layer.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
detachElasticLoadBalancer :: forall eff. DetachElasticLoadBalancerRequest -> Aff (err :: AWS.RequestError | eff) Unit
detachElasticLoadBalancer = AWS.request serviceName "DetachElasticLoadBalancer" 


-- | <p>Disassociates an Elastic IP address from its instance. The address remains registered with the stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
disassociateElasticIp :: forall eff. DisassociateElasticIpRequest -> Aff (err :: AWS.RequestError | eff) Unit
disassociateElasticIp = AWS.request serviceName "DisassociateElasticIp" 


-- | <p>Gets a generated host name for the specified layer, based on the current host name theme.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
getHostnameSuggestion :: forall eff. GetHostnameSuggestionRequest -> Aff (err :: AWS.RequestError | eff) GetHostnameSuggestionResult
getHostnameSuggestion = AWS.request serviceName "GetHostnameSuggestion" 


-- | <note> <p>This action can be used only with Windows stacks.</p> </note> <p>Grants RDP access to a Windows instance for a specified time period.</p>
grantAccess :: forall eff. GrantAccessRequest -> Aff (err :: AWS.RequestError | eff) GrantAccessResult
grantAccess = AWS.request serviceName "GrantAccess" 


-- | <p>Returns a list of tags that are applied to the specified stack or layer.</p>
listTags :: forall eff. ListTagsRequest -> Aff (err :: AWS.RequestError | eff) ListTagsResult
listTags = AWS.request serviceName "ListTags" 


-- | <p>Reboots a specified instance. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html">Starting, Stopping, and Rebooting Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
rebootInstance :: forall eff. RebootInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
rebootInstance = AWS.request serviceName "RebootInstance" 


-- | <p>Registers a specified Amazon ECS cluster with a stack. You can register only one cluster with a stack. A cluster can be registered with only one stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html"> Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html"> Managing User Permissions</a>.</p>
registerEcsCluster :: forall eff. RegisterEcsClusterRequest -> Aff (err :: AWS.RequestError | eff) RegisterEcsClusterResult
registerEcsCluster = AWS.request serviceName "RegisterEcsCluster" 


-- | <p>Registers an Elastic IP address with a specified stack. An address can be registered with only one stack at a time. If the address is already registered, you must first deregister it by calling <a>DeregisterElasticIp</a>. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
registerElasticIp :: forall eff. RegisterElasticIpRequest -> Aff (err :: AWS.RequestError | eff) RegisterElasticIpResult
registerElasticIp = AWS.request serviceName "RegisterElasticIp" 


-- | <p>Registers instances that were created outside of AWS OpsWorks Stacks with a specified stack.</p> <note> <p>We do not recommend using this action to register instances. The complete registration operation includes two tasks: installing the AWS OpsWorks Stacks agent on the instance, and registering the instance with the stack. <code>RegisterInstance</code> handles only the second step. You should instead use the AWS CLI <code>register</code> command, which performs the entire registration operation. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register.html"> Registering an Instance with an AWS OpsWorks Stacks Stack</a>.</p> </note> <p>Registered instances have the same requirements as instances that are created by using the <a>CreateInstance</a> API. For example, registered instances must be running a supported Linux-based operating system, and they must have a supported instance type. For more information about requirements for instances that you want to register, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register-registering-preparer.html"> Preparing the Instance</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
registerInstance :: forall eff. RegisterInstanceRequest -> Aff (err :: AWS.RequestError | eff) RegisterInstanceResult
registerInstance = AWS.request serviceName "RegisterInstance" 


-- | <p>Registers an Amazon RDS instance with a stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
registerRdsDbInstance :: forall eff. RegisterRdsDbInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
registerRdsDbInstance = AWS.request serviceName "RegisterRdsDbInstance" 


-- | <p>Registers an Amazon EBS volume with a specified stack. A volume can be registered with only one stack at a time. If the volume is already registered, you must first deregister it by calling <a>DeregisterVolume</a>. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
registerVolume :: forall eff. RegisterVolumeRequest -> Aff (err :: AWS.RequestError | eff) RegisterVolumeResult
registerVolume = AWS.request serviceName "RegisterVolume" 


-- | <p>Specify the load-based auto scaling configuration for a specified layer. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html">Managing Load with Time-based and Load-based Instances</a>.</p> <note> <p>To use load-based auto scaling, you must create a set of load-based auto scaling instances. Load-based auto scaling operates only on the instances from that set, so you must ensure that you have created enough instances to handle the maximum anticipated load.</p> </note> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
setLoadBasedAutoScaling :: forall eff. SetLoadBasedAutoScalingRequest -> Aff (err :: AWS.RequestError | eff) Unit
setLoadBasedAutoScaling = AWS.request serviceName "SetLoadBasedAutoScaling" 


-- | <p>Specifies a user's permissions. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingsecurity.html">Security and Permissions</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
setPermission :: forall eff. SetPermissionRequest -> Aff (err :: AWS.RequestError | eff) Unit
setPermission = AWS.request serviceName "SetPermission" 


-- | <p>Specify the time-based auto scaling configuration for a specified instance. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-autoscaling.html">Managing Load with Time-based and Load-based Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
setTimeBasedAutoScaling :: forall eff. SetTimeBasedAutoScalingRequest -> Aff (err :: AWS.RequestError | eff) Unit
setTimeBasedAutoScaling = AWS.request serviceName "SetTimeBasedAutoScaling" 


-- | <p>Starts a specified instance. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html">Starting, Stopping, and Rebooting Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
startInstance :: forall eff. StartInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
startInstance = AWS.request serviceName "StartInstance" 


-- | <p>Starts a stack's instances.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
startStack :: forall eff. StartStackRequest -> Aff (err :: AWS.RequestError | eff) Unit
startStack = AWS.request serviceName "StartStack" 


-- | <p>Stops a specified instance. When you stop a standard instance, the data disappears and must be reinstalled when you restart the instance. You can stop an Amazon EBS-backed instance without losing data. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workinginstances-starting.html">Starting, Stopping, and Rebooting Instances</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
stopInstance :: forall eff. StopInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
stopInstance = AWS.request serviceName "StopInstance" 


-- | <p>Stops a specified stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
stopStack :: forall eff. StopStackRequest -> Aff (err :: AWS.RequestError | eff) Unit
stopStack = AWS.request serviceName "StopStack" 


-- | <p>Apply cost-allocation tags to a specified stack or layer in AWS OpsWorks Stacks. For more information about how tagging works, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/tagging.html">Tags</a> in the AWS OpsWorks User Guide.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) Unit
tagResource = AWS.request serviceName "TagResource" 


-- | <p>Unassigns a registered instance from all of it's layers. The instance remains in the stack as an unassigned instance and can be assigned to another layer, as needed. You cannot use this action with instances that were created with AWS OpsWorks Stacks.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
unassignInstance :: forall eff. UnassignInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
unassignInstance = AWS.request serviceName "UnassignInstance" 


-- | <p>Unassigns an assigned Amazon EBS volume. The volume remains registered with the stack. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
unassignVolume :: forall eff. UnassignVolumeRequest -> Aff (err :: AWS.RequestError | eff) Unit
unassignVolume = AWS.request serviceName "UnassignVolume" 


-- | <p>Removes tags from a specified stack or layer.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) Unit
untagResource = AWS.request serviceName "UntagResource" 


-- | <p>Updates a specified app.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Deploy or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateApp :: forall eff. UpdateAppRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateApp = AWS.request serviceName "UpdateApp" 


-- | <p>Updates a registered Elastic IP address's name. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateElasticIp :: forall eff. UpdateElasticIpRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateElasticIp = AWS.request serviceName "UpdateElasticIp" 


-- | <p>Updates a specified instance.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateInstance :: forall eff. UpdateInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateInstance = AWS.request serviceName "UpdateInstance" 


-- | <p>Updates a specified layer.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateLayer :: forall eff. UpdateLayerRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateLayer = AWS.request serviceName "UpdateLayer" 


-- | <p>Updates a user's SSH public key.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have self-management enabled or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateMyUserProfile :: forall eff. UpdateMyUserProfileRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateMyUserProfile = AWS.request serviceName "UpdateMyUserProfile" 


-- | <p>Updates an Amazon RDS instance.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateRdsDbInstance :: forall eff. UpdateRdsDbInstanceRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateRdsDbInstance = AWS.request serviceName "UpdateRdsDbInstance" 


-- | <p>Updates a specified stack.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateStack :: forall eff. UpdateStackRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateStack = AWS.request serviceName "UpdateStack" 


-- | <p>Updates a specified user profile.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateUserProfile :: forall eff. UpdateUserProfileRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateUserProfile = AWS.request serviceName "UpdateUserProfile" 


-- | <p>Updates an Amazon EBS volume's name or mount point. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html">Resource Management</a>.</p> <p> <b>Required Permissions</b>: To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html">Managing User Permissions</a>.</p>
updateVolume :: forall eff. UpdateVolumeRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateVolume = AWS.request serviceName "UpdateVolume" 


-- | <p>Describes an agent version.</p>
newtype AgentVersion = AgentVersion 
  { "Version" :: NullOrUndefined (String)
  , "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager)
  }


newtype AgentVersions = AgentVersions (Array AgentVersion)


-- | <p>A description of the app.</p>
newtype App = App 
  { "AppId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "Shortname" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "DataSources" :: NullOrUndefined (DataSources)
  , "Type" :: NullOrUndefined (AppType)
  , "AppSource" :: NullOrUndefined (Source)
  , "Domains" :: NullOrUndefined (Strings)
  , "EnableSsl" :: NullOrUndefined (Boolean)
  , "SslConfiguration" :: NullOrUndefined (SslConfiguration)
  , "Attributes" :: NullOrUndefined (AppAttributes)
  , "CreatedAt" :: NullOrUndefined (String)
  , "Environment" :: NullOrUndefined (EnvironmentVariables)
  }


newtype AppAttributes = AppAttributes (Map AppAttributesKeys String)


newtype AppAttributesKeys = AppAttributesKeys String


newtype AppType = AppType String


newtype Apps = Apps (Array App)


newtype Architecture = Architecture String


newtype AssignInstanceRequest = AssignInstanceRequest 
  { "InstanceId" :: (String)
  , "LayerIds" :: (Strings)
  }


newtype AssignVolumeRequest = AssignVolumeRequest 
  { "VolumeId" :: (String)
  , "InstanceId" :: NullOrUndefined (String)
  }


newtype AssociateElasticIpRequest = AssociateElasticIpRequest 
  { "ElasticIp" :: (String)
  , "InstanceId" :: NullOrUndefined (String)
  }


newtype AttachElasticLoadBalancerRequest = AttachElasticLoadBalancerRequest 
  { "ElasticLoadBalancerName" :: (String)
  , "LayerId" :: (String)
  }


-- | <p>Describes a load-based auto scaling upscaling or downscaling threshold configuration, which specifies when AWS OpsWorks Stacks starts or stops load-based instances.</p>
newtype AutoScalingThresholds = AutoScalingThresholds 
  { "InstanceCount" :: NullOrUndefined (Int)
  , "ThresholdsWaitTime" :: NullOrUndefined (Minute)
  , "IgnoreMetricsTime" :: NullOrUndefined (Minute)
  , "CpuThreshold" :: NullOrUndefined (Number)
  , "MemoryThreshold" :: NullOrUndefined (Number)
  , "LoadThreshold" :: NullOrUndefined (Number)
  , "Alarms" :: NullOrUndefined (Strings)
  }


newtype AutoScalingType = AutoScalingType String


-- | <p>Describes a block device mapping. This data type maps directly to the Amazon EC2 <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html">BlockDeviceMapping</a> data type. </p>
newtype BlockDeviceMapping = BlockDeviceMapping 
  { "DeviceName" :: NullOrUndefined (String)
  , "NoDevice" :: NullOrUndefined (String)
  , "VirtualName" :: NullOrUndefined (String)
  , "Ebs" :: NullOrUndefined (EbsBlockDevice)
  }


newtype BlockDeviceMappings = BlockDeviceMappings (Array BlockDeviceMapping)


-- | <p>Describes the Chef configuration.</p>
newtype ChefConfiguration = ChefConfiguration 
  { "ManageBerkshelf" :: NullOrUndefined (Boolean)
  , "BerkshelfVersion" :: NullOrUndefined (String)
  }


newtype CloneStackRequest = CloneStackRequest 
  { "SourceStackId" :: (String)
  , "Name" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (StackAttributes)
  , "ServiceRoleArn" :: (String)
  , "DefaultInstanceProfileArn" :: NullOrUndefined (String)
  , "DefaultOs" :: NullOrUndefined (String)
  , "HostnameTheme" :: NullOrUndefined (String)
  , "DefaultAvailabilityZone" :: NullOrUndefined (String)
  , "DefaultSubnetId" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  , "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager)
  , "ChefConfiguration" :: NullOrUndefined (ChefConfiguration)
  , "UseCustomCookbooks" :: NullOrUndefined (Boolean)
  , "UseOpsworksSecurityGroups" :: NullOrUndefined (Boolean)
  , "CustomCookbooksSource" :: NullOrUndefined (Source)
  , "DefaultSshKeyName" :: NullOrUndefined (String)
  , "ClonePermissions" :: NullOrUndefined (Boolean)
  , "CloneAppIds" :: NullOrUndefined (Strings)
  , "DefaultRootDeviceType" :: NullOrUndefined (RootDeviceType)
  , "AgentVersion" :: NullOrUndefined (String)
  }


-- | <p>Contains the response to a <code>CloneStack</code> request.</p>
newtype CloneStackResult = CloneStackResult 
  { "StackId" :: NullOrUndefined (String)
  }


-- | <p>Describes the Amazon CloudWatch logs configuration for a layer.</p>
newtype CloudWatchLogsConfiguration = CloudWatchLogsConfiguration 
  { "Enabled" :: NullOrUndefined (Boolean)
  , "LogStreams" :: NullOrUndefined (CloudWatchLogsLogStreams)
  }


-- | <p>Specifies the encoding of the log file so that the file can be read correctly. The default is <code>utf_8</code>. Encodings supported by Python <code>codecs.decode()</code> can be used here.</p>
newtype CloudWatchLogsEncoding = CloudWatchLogsEncoding String


-- | <p>Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. It's only used if there is no state persisted for that log stream.</p>
newtype CloudWatchLogsInitialPosition = CloudWatchLogsInitialPosition String


-- | <p>Describes the Amazon CloudWatch logs configuration for a layer. For detailed information about members of this data type, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/AgentReference.html">CloudWatch Logs Agent Reference</a>.</p>
newtype CloudWatchLogsLogStream = CloudWatchLogsLogStream 
  { "LogGroupName" :: NullOrUndefined (String)
  , "DatetimeFormat" :: NullOrUndefined (String)
  , "TimeZone" :: NullOrUndefined (CloudWatchLogsTimeZone)
  , "File" :: NullOrUndefined (String)
  , "FileFingerprintLines" :: NullOrUndefined (String)
  , "MultiLineStartPattern" :: NullOrUndefined (String)
  , "InitialPosition" :: NullOrUndefined (CloudWatchLogsInitialPosition)
  , "Encoding" :: NullOrUndefined (CloudWatchLogsEncoding)
  , "BufferDuration" :: NullOrUndefined (Int)
  , "BatchCount" :: NullOrUndefined (Int)
  , "BatchSize" :: NullOrUndefined (Int)
  }


-- | <p>Describes the Amazon CloudWatch logs configuration for a layer.</p>
newtype CloudWatchLogsLogStreams = CloudWatchLogsLogStreams (Array CloudWatchLogsLogStream)


-- | <p>The preferred time zone for logs streamed to CloudWatch Logs. Valid values are <code>LOCAL</code> and <code>UTC</code>, for Coordinated Universal Time.</p>
newtype CloudWatchLogsTimeZone = CloudWatchLogsTimeZone String


-- | <p>Describes a command.</p>
newtype Command = Command 
  { "CommandId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "DeploymentId" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (DateTime)
  , "AcknowledgedAt" :: NullOrUndefined (DateTime)
  , "CompletedAt" :: NullOrUndefined (DateTime)
  , "Status" :: NullOrUndefined (String)
  , "ExitCode" :: NullOrUndefined (Int)
  , "LogUrl" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }


newtype Commands = Commands (Array Command)


newtype CreateAppRequest = CreateAppRequest 
  { "StackId" :: (String)
  , "Shortname" :: NullOrUndefined (String)
  , "Name" :: (String)
  , "Description" :: NullOrUndefined (String)
  , "DataSources" :: NullOrUndefined (DataSources)
  , "Type" :: (AppType)
  , "AppSource" :: NullOrUndefined (Source)
  , "Domains" :: NullOrUndefined (Strings)
  , "EnableSsl" :: NullOrUndefined (Boolean)
  , "SslConfiguration" :: NullOrUndefined (SslConfiguration)
  , "Attributes" :: NullOrUndefined (AppAttributes)
  , "Environment" :: NullOrUndefined (EnvironmentVariables)
  }


-- | <p>Contains the response to a <code>CreateApp</code> request.</p>
newtype CreateAppResult = CreateAppResult 
  { "AppId" :: NullOrUndefined (String)
  }


newtype CreateDeploymentRequest = CreateDeploymentRequest 
  { "StackId" :: (String)
  , "AppId" :: NullOrUndefined (String)
  , "InstanceIds" :: NullOrUndefined (Strings)
  , "LayerIds" :: NullOrUndefined (Strings)
  , "Command" :: (DeploymentCommand)
  , "Comment" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  }


-- | <p>Contains the response to a <code>CreateDeployment</code> request.</p>
newtype CreateDeploymentResult = CreateDeploymentResult 
  { "DeploymentId" :: NullOrUndefined (String)
  }


newtype CreateInstanceRequest = CreateInstanceRequest 
  { "StackId" :: (String)
  , "LayerIds" :: (Strings)
  , "InstanceType" :: (String)
  , "AutoScalingType" :: NullOrUndefined (AutoScalingType)
  , "Hostname" :: NullOrUndefined (String)
  , "Os" :: NullOrUndefined (String)
  , "AmiId" :: NullOrUndefined (String)
  , "SshKeyName" :: NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "VirtualizationType" :: NullOrUndefined (String)
  , "SubnetId" :: NullOrUndefined (String)
  , "Architecture" :: NullOrUndefined (Architecture)
  , "RootDeviceType" :: NullOrUndefined (RootDeviceType)
  , "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappings)
  , "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean)
  , "EbsOptimized" :: NullOrUndefined (Boolean)
  , "AgentVersion" :: NullOrUndefined (String)
  , "Tenancy" :: NullOrUndefined (String)
  }


-- | <p>Contains the response to a <code>CreateInstance</code> request.</p>
newtype CreateInstanceResult = CreateInstanceResult 
  { "InstanceId" :: NullOrUndefined (String)
  }


newtype CreateLayerRequest = CreateLayerRequest 
  { "StackId" :: (String)
  , "Type" :: (LayerType)
  , "Name" :: (String)
  , "Shortname" :: (String)
  , "Attributes" :: NullOrUndefined (LayerAttributes)
  , "CloudWatchLogsConfiguration" :: NullOrUndefined (CloudWatchLogsConfiguration)
  , "CustomInstanceProfileArn" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  , "CustomSecurityGroupIds" :: NullOrUndefined (Strings)
  , "Packages" :: NullOrUndefined (Strings)
  , "VolumeConfigurations" :: NullOrUndefined (VolumeConfigurations)
  , "EnableAutoHealing" :: NullOrUndefined (Boolean)
  , "AutoAssignElasticIps" :: NullOrUndefined (Boolean)
  , "AutoAssignPublicIps" :: NullOrUndefined (Boolean)
  , "CustomRecipes" :: NullOrUndefined (Recipes)
  , "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean)
  , "UseEbsOptimizedInstances" :: NullOrUndefined (Boolean)
  , "LifecycleEventConfiguration" :: NullOrUndefined (LifecycleEventConfiguration)
  }


-- | <p>Contains the response to a <code>CreateLayer</code> request.</p>
newtype CreateLayerResult = CreateLayerResult 
  { "LayerId" :: NullOrUndefined (String)
  }


newtype CreateStackRequest = CreateStackRequest 
  { "Name" :: (String)
  , "Region" :: (String)
  , "VpcId" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (StackAttributes)
  , "ServiceRoleArn" :: (String)
  , "DefaultInstanceProfileArn" :: (String)
  , "DefaultOs" :: NullOrUndefined (String)
  , "HostnameTheme" :: NullOrUndefined (String)
  , "DefaultAvailabilityZone" :: NullOrUndefined (String)
  , "DefaultSubnetId" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  , "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager)
  , "ChefConfiguration" :: NullOrUndefined (ChefConfiguration)
  , "UseCustomCookbooks" :: NullOrUndefined (Boolean)
  , "UseOpsworksSecurityGroups" :: NullOrUndefined (Boolean)
  , "CustomCookbooksSource" :: NullOrUndefined (Source)
  , "DefaultSshKeyName" :: NullOrUndefined (String)
  , "DefaultRootDeviceType" :: NullOrUndefined (RootDeviceType)
  , "AgentVersion" :: NullOrUndefined (String)
  }


-- | <p>Contains the response to a <code>CreateStack</code> request.</p>
newtype CreateStackResult = CreateStackResult 
  { "StackId" :: NullOrUndefined (String)
  }


newtype CreateUserProfileRequest = CreateUserProfileRequest 
  { "IamUserArn" :: (String)
  , "SshUsername" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (String)
  , "AllowSelfManagement" :: NullOrUndefined (Boolean)
  }


-- | <p>Contains the response to a <code>CreateUserProfile</code> request.</p>
newtype CreateUserProfileResult = CreateUserProfileResult 
  { "IamUserArn" :: NullOrUndefined (String)
  }


newtype DailyAutoScalingSchedule = DailyAutoScalingSchedule (Map Hour Switch)


-- | <p>Describes an app's data source.</p>
newtype DataSource = DataSource 
  { "Type" :: NullOrUndefined (String)
  , "Arn" :: NullOrUndefined (String)
  , "DatabaseName" :: NullOrUndefined (String)
  }


newtype DataSources = DataSources (Array DataSource)


newtype DateTime = DateTime String


newtype DeleteAppRequest = DeleteAppRequest 
  { "AppId" :: (String)
  }


newtype DeleteInstanceRequest = DeleteInstanceRequest 
  { "InstanceId" :: (String)
  , "DeleteElasticIp" :: NullOrUndefined (Boolean)
  , "DeleteVolumes" :: NullOrUndefined (Boolean)
  }


newtype DeleteLayerRequest = DeleteLayerRequest 
  { "LayerId" :: (String)
  }


newtype DeleteStackRequest = DeleteStackRequest 
  { "StackId" :: (String)
  }


newtype DeleteUserProfileRequest = DeleteUserProfileRequest 
  { "IamUserArn" :: (String)
  }


-- | <p>Describes a deployment of a stack or app.</p>
newtype Deployment = Deployment 
  { "DeploymentId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "AppId" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (DateTime)
  , "CompletedAt" :: NullOrUndefined (DateTime)
  , "Duration" :: NullOrUndefined (Int)
  , "IamUserArn" :: NullOrUndefined (String)
  , "Comment" :: NullOrUndefined (String)
  , "Command" :: NullOrUndefined (DeploymentCommand)
  , "Status" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  , "InstanceIds" :: NullOrUndefined (Strings)
  }


-- | <p>Used to specify a stack or deployment command.</p>
newtype DeploymentCommand = DeploymentCommand 
  { "Name" :: (DeploymentCommandName)
  , "Args" :: NullOrUndefined (DeploymentCommandArgs)
  }


newtype DeploymentCommandArgs = DeploymentCommandArgs (Map String Strings)


newtype DeploymentCommandName = DeploymentCommandName String


newtype Deployments = Deployments (Array Deployment)


newtype DeregisterEcsClusterRequest = DeregisterEcsClusterRequest 
  { "EcsClusterArn" :: (String)
  }


newtype DeregisterElasticIpRequest = DeregisterElasticIpRequest 
  { "ElasticIp" :: (String)
  }


newtype DeregisterInstanceRequest = DeregisterInstanceRequest 
  { "InstanceId" :: (String)
  }


newtype DeregisterRdsDbInstanceRequest = DeregisterRdsDbInstanceRequest 
  { "RdsDbInstanceArn" :: (String)
  }


newtype DeregisterVolumeRequest = DeregisterVolumeRequest 
  { "VolumeId" :: (String)
  }


newtype DescribeAgentVersionsRequest = DescribeAgentVersionsRequest 
  { "StackId" :: NullOrUndefined (String)
  , "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager)
  }


-- | <p>Contains the response to a <code>DescribeAgentVersions</code> request.</p>
newtype DescribeAgentVersionsResult = DescribeAgentVersionsResult 
  { "AgentVersions" :: NullOrUndefined (AgentVersions)
  }


newtype DescribeAppsRequest = DescribeAppsRequest 
  { "StackId" :: NullOrUndefined (String)
  , "AppIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeApps</code> request.</p>
newtype DescribeAppsResult = DescribeAppsResult 
  { "Apps" :: NullOrUndefined (Apps)
  }


newtype DescribeCommandsRequest = DescribeCommandsRequest 
  { "DeploymentId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "CommandIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeCommands</code> request.</p>
newtype DescribeCommandsResult = DescribeCommandsResult 
  { "Commands" :: NullOrUndefined (Commands)
  }


newtype DescribeDeploymentsRequest = DescribeDeploymentsRequest 
  { "StackId" :: NullOrUndefined (String)
  , "AppId" :: NullOrUndefined (String)
  , "DeploymentIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeDeployments</code> request.</p>
newtype DescribeDeploymentsResult = DescribeDeploymentsResult 
  { "Deployments" :: NullOrUndefined (Deployments)
  }


newtype DescribeEcsClustersRequest = DescribeEcsClustersRequest 
  { "EcsClusterArns" :: NullOrUndefined (Strings)
  , "StackId" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  }


-- | <p>Contains the response to a <code>DescribeEcsClusters</code> request.</p>
newtype DescribeEcsClustersResult = DescribeEcsClustersResult 
  { "EcsClusters" :: NullOrUndefined (EcsClusters)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeElasticIpsRequest = DescribeElasticIpsRequest 
  { "InstanceId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "Ips" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeElasticIps</code> request.</p>
newtype DescribeElasticIpsResult = DescribeElasticIpsResult 
  { "ElasticIps" :: NullOrUndefined (ElasticIps)
  }


newtype DescribeElasticLoadBalancersRequest = DescribeElasticLoadBalancersRequest 
  { "StackId" :: NullOrUndefined (String)
  , "LayerIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeElasticLoadBalancers</code> request.</p>
newtype DescribeElasticLoadBalancersResult = DescribeElasticLoadBalancersResult 
  { "ElasticLoadBalancers" :: NullOrUndefined (ElasticLoadBalancers)
  }


newtype DescribeInstancesRequest = DescribeInstancesRequest 
  { "StackId" :: NullOrUndefined (String)
  , "LayerId" :: NullOrUndefined (String)
  , "InstanceIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeInstances</code> request.</p>
newtype DescribeInstancesResult = DescribeInstancesResult 
  { "Instances" :: NullOrUndefined (Instances)
  }


newtype DescribeLayersRequest = DescribeLayersRequest 
  { "StackId" :: NullOrUndefined (String)
  , "LayerIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeLayers</code> request.</p>
newtype DescribeLayersResult = DescribeLayersResult 
  { "Layers" :: NullOrUndefined (Layers)
  }


newtype DescribeLoadBasedAutoScalingRequest = DescribeLoadBasedAutoScalingRequest 
  { "LayerIds" :: (Strings)
  }


-- | <p>Contains the response to a <code>DescribeLoadBasedAutoScaling</code> request.</p>
newtype DescribeLoadBasedAutoScalingResult = DescribeLoadBasedAutoScalingResult 
  { "LoadBasedAutoScalingConfigurations" :: NullOrUndefined (LoadBasedAutoScalingConfigurations)
  }


-- | <p>Contains the response to a <code>DescribeMyUserProfile</code> request.</p>
newtype DescribeMyUserProfileResult = DescribeMyUserProfileResult 
  { "UserProfile" :: NullOrUndefined (SelfUserProfile)
  }


-- | <p>The response to a <code>DescribeOperatingSystems</code> request.</p>
newtype DescribeOperatingSystemsResponse = DescribeOperatingSystemsResponse 
  { "OperatingSystems" :: NullOrUndefined (OperatingSystems)
  }


newtype DescribePermissionsRequest = DescribePermissionsRequest 
  { "IamUserArn" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  }


-- | <p>Contains the response to a <code>DescribePermissions</code> request.</p>
newtype DescribePermissionsResult = DescribePermissionsResult 
  { "Permissions" :: NullOrUndefined (Permissions)
  }


newtype DescribeRaidArraysRequest = DescribeRaidArraysRequest 
  { "InstanceId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "RaidArrayIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeRaidArrays</code> request.</p>
newtype DescribeRaidArraysResult = DescribeRaidArraysResult 
  { "RaidArrays" :: NullOrUndefined (RaidArrays)
  }


newtype DescribeRdsDbInstancesRequest = DescribeRdsDbInstancesRequest 
  { "StackId" :: (String)
  , "RdsDbInstanceArns" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeRdsDbInstances</code> request.</p>
newtype DescribeRdsDbInstancesResult = DescribeRdsDbInstancesResult 
  { "RdsDbInstances" :: NullOrUndefined (RdsDbInstances)
  }


newtype DescribeServiceErrorsRequest = DescribeServiceErrorsRequest 
  { "StackId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "ServiceErrorIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeServiceErrors</code> request.</p>
newtype DescribeServiceErrorsResult = DescribeServiceErrorsResult 
  { "ServiceErrors" :: NullOrUndefined (ServiceErrors)
  }


newtype DescribeStackProvisioningParametersRequest = DescribeStackProvisioningParametersRequest 
  { "StackId" :: (String)
  }


-- | <p>Contains the response to a <code>DescribeStackProvisioningParameters</code> request.</p>
newtype DescribeStackProvisioningParametersResult = DescribeStackProvisioningParametersResult 
  { "AgentInstallerUrl" :: NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined (Parameters)
  }


newtype DescribeStackSummaryRequest = DescribeStackSummaryRequest 
  { "StackId" :: (String)
  }


-- | <p>Contains the response to a <code>DescribeStackSummary</code> request.</p>
newtype DescribeStackSummaryResult = DescribeStackSummaryResult 
  { "StackSummary" :: NullOrUndefined (StackSummary)
  }


newtype DescribeStacksRequest = DescribeStacksRequest 
  { "StackIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeStacks</code> request.</p>
newtype DescribeStacksResult = DescribeStacksResult 
  { "Stacks" :: NullOrUndefined (Stacks)
  }


newtype DescribeTimeBasedAutoScalingRequest = DescribeTimeBasedAutoScalingRequest 
  { "InstanceIds" :: (Strings)
  }


-- | <p>Contains the response to a <code>DescribeTimeBasedAutoScaling</code> request.</p>
newtype DescribeTimeBasedAutoScalingResult = DescribeTimeBasedAutoScalingResult 
  { "TimeBasedAutoScalingConfigurations" :: NullOrUndefined (TimeBasedAutoScalingConfigurations)
  }


newtype DescribeUserProfilesRequest = DescribeUserProfilesRequest 
  { "IamUserArns" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeUserProfiles</code> request.</p>
newtype DescribeUserProfilesResult = DescribeUserProfilesResult 
  { "UserProfiles" :: NullOrUndefined (UserProfiles)
  }


newtype DescribeVolumesRequest = DescribeVolumesRequest 
  { "InstanceId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "RaidArrayId" :: NullOrUndefined (String)
  , "VolumeIds" :: NullOrUndefined (Strings)
  }


-- | <p>Contains the response to a <code>DescribeVolumes</code> request.</p>
newtype DescribeVolumesResult = DescribeVolumesResult 
  { "Volumes" :: NullOrUndefined (Volumes)
  }


newtype DetachElasticLoadBalancerRequest = DetachElasticLoadBalancerRequest 
  { "ElasticLoadBalancerName" :: (String)
  , "LayerId" :: (String)
  }


newtype DisassociateElasticIpRequest = DisassociateElasticIpRequest 
  { "ElasticIp" :: (String)
  }


-- | <p>Describes an Amazon EBS volume. This data type maps directly to the Amazon EC2 <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html">EbsBlockDevice</a> data type.</p>
newtype EbsBlockDevice = EbsBlockDevice 
  { "SnapshotId" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (Int)
  , "VolumeSize" :: NullOrUndefined (Int)
  , "VolumeType" :: NullOrUndefined (VolumeType)
  , "DeleteOnTermination" :: NullOrUndefined (Boolean)
  }


-- | <p>Describes a registered Amazon ECS cluster.</p>
newtype EcsCluster = EcsCluster 
  { "EcsClusterArn" :: NullOrUndefined (String)
  , "EcsClusterName" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "RegisteredAt" :: NullOrUndefined (DateTime)
  }


newtype EcsClusters = EcsClusters (Array EcsCluster)


-- | <p>Describes an Elastic IP address.</p>
newtype ElasticIp = ElasticIp 
  { "Ip" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Domain" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  }


newtype ElasticIps = ElasticIps (Array ElasticIp)


-- | <p>Describes an Elastic Load Balancing instance.</p>
newtype ElasticLoadBalancer = ElasticLoadBalancer 
  { "ElasticLoadBalancerName" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  , "DnsName" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "LayerId" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "AvailabilityZones" :: NullOrUndefined (Strings)
  , "SubnetIds" :: NullOrUndefined (Strings)
  , "Ec2InstanceIds" :: NullOrUndefined (Strings)
  }


newtype ElasticLoadBalancers = ElasticLoadBalancers (Array ElasticLoadBalancer)


-- | <p>Represents an app's environment variable.</p>
newtype EnvironmentVariable = EnvironmentVariable 
  { "Key" :: (String)
  , "Value" :: (String)
  , "Secure" :: NullOrUndefined (Boolean)
  }


newtype EnvironmentVariables = EnvironmentVariables (Array EnvironmentVariable)


newtype GetHostnameSuggestionRequest = GetHostnameSuggestionRequest 
  { "LayerId" :: (String)
  }


-- | <p>Contains the response to a <code>GetHostnameSuggestion</code> request.</p>
newtype GetHostnameSuggestionResult = GetHostnameSuggestionResult 
  { "LayerId" :: NullOrUndefined (String)
  , "Hostname" :: NullOrUndefined (String)
  }


newtype GrantAccessRequest = GrantAccessRequest 
  { "InstanceId" :: (String)
  , "ValidForInMinutes" :: NullOrUndefined (ValidForInMinutes)
  }


-- | <p>Contains the response to a <code>GrantAccess</code> request.</p>
newtype GrantAccessResult = GrantAccessResult 
  { "TemporaryCredential" :: NullOrUndefined (TemporaryCredential)
  }


newtype Hour = Hour String


-- | <p>Describes an instance.</p>
newtype Instance = Instance 
  { "AgentVersion" :: NullOrUndefined (String)
  , "AmiId" :: NullOrUndefined (String)
  , "Architecture" :: NullOrUndefined (Architecture)
  , "Arn" :: NullOrUndefined (String)
  , "AutoScalingType" :: NullOrUndefined (AutoScalingType)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "BlockDeviceMappings" :: NullOrUndefined (BlockDeviceMappings)
  , "CreatedAt" :: NullOrUndefined (DateTime)
  , "EbsOptimized" :: NullOrUndefined (Boolean)
  , "Ec2InstanceId" :: NullOrUndefined (String)
  , "EcsClusterArn" :: NullOrUndefined (String)
  , "EcsContainerInstanceArn" :: NullOrUndefined (String)
  , "ElasticIp" :: NullOrUndefined (String)
  , "Hostname" :: NullOrUndefined (String)
  , "InfrastructureClass" :: NullOrUndefined (String)
  , "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean)
  , "InstanceId" :: NullOrUndefined (String)
  , "InstanceProfileArn" :: NullOrUndefined (String)
  , "InstanceType" :: NullOrUndefined (String)
  , "LastServiceErrorId" :: NullOrUndefined (String)
  , "LayerIds" :: NullOrUndefined (Strings)
  , "Os" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (String)
  , "PrivateDns" :: NullOrUndefined (String)
  , "PrivateIp" :: NullOrUndefined (String)
  , "PublicDns" :: NullOrUndefined (String)
  , "PublicIp" :: NullOrUndefined (String)
  , "RegisteredBy" :: NullOrUndefined (String)
  , "ReportedAgentVersion" :: NullOrUndefined (String)
  , "ReportedOs" :: NullOrUndefined (ReportedOs)
  , "RootDeviceType" :: NullOrUndefined (RootDeviceType)
  , "RootDeviceVolumeId" :: NullOrUndefined (String)
  , "SecurityGroupIds" :: NullOrUndefined (Strings)
  , "SshHostDsaKeyFingerprint" :: NullOrUndefined (String)
  , "SshHostRsaKeyFingerprint" :: NullOrUndefined (String)
  , "SshKeyName" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "SubnetId" :: NullOrUndefined (String)
  , "Tenancy" :: NullOrUndefined (String)
  , "VirtualizationType" :: NullOrUndefined (VirtualizationType)
  }


-- | <p>Contains a description of an Amazon EC2 instance from the Amazon EC2 metadata service. For more information, see <a href="http://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html">Instance Metadata and User Data</a>.</p>
newtype InstanceIdentity = InstanceIdentity 
  { "Document" :: NullOrUndefined (String)
  , "Signature" :: NullOrUndefined (String)
  }


newtype Instances = Instances (Array Instance)


-- | <p>Describes how many instances a stack has for each status.</p>
newtype InstancesCount = InstancesCount 
  { "Assigning" :: NullOrUndefined (Int)
  , "Booting" :: NullOrUndefined (Int)
  , "ConnectionLost" :: NullOrUndefined (Int)
  , "Deregistering" :: NullOrUndefined (Int)
  , "Online" :: NullOrUndefined (Int)
  , "Pending" :: NullOrUndefined (Int)
  , "Rebooting" :: NullOrUndefined (Int)
  , "Registered" :: NullOrUndefined (Int)
  , "Registering" :: NullOrUndefined (Int)
  , "Requested" :: NullOrUndefined (Int)
  , "RunningSetup" :: NullOrUndefined (Int)
  , "SetupFailed" :: NullOrUndefined (Int)
  , "ShuttingDown" :: NullOrUndefined (Int)
  , "StartFailed" :: NullOrUndefined (Int)
  , "StopFailed" :: NullOrUndefined (Int)
  , "Stopped" :: NullOrUndefined (Int)
  , "Stopping" :: NullOrUndefined (Int)
  , "Terminated" :: NullOrUndefined (Int)
  , "Terminating" :: NullOrUndefined (Int)
  , "Unassigning" :: NullOrUndefined (Int)
  }


-- | <p>Describes a layer.</p>
newtype Layer = Layer 
  { "Arn" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "LayerId" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (LayerType)
  , "Name" :: NullOrUndefined (String)
  , "Shortname" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (LayerAttributes)
  , "CloudWatchLogsConfiguration" :: NullOrUndefined (CloudWatchLogsConfiguration)
  , "CustomInstanceProfileArn" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  , "CustomSecurityGroupIds" :: NullOrUndefined (Strings)
  , "DefaultSecurityGroupNames" :: NullOrUndefined (Strings)
  , "Packages" :: NullOrUndefined (Strings)
  , "VolumeConfigurations" :: NullOrUndefined (VolumeConfigurations)
  , "EnableAutoHealing" :: NullOrUndefined (Boolean)
  , "AutoAssignElasticIps" :: NullOrUndefined (Boolean)
  , "AutoAssignPublicIps" :: NullOrUndefined (Boolean)
  , "DefaultRecipes" :: NullOrUndefined (Recipes)
  , "CustomRecipes" :: NullOrUndefined (Recipes)
  , "CreatedAt" :: NullOrUndefined (DateTime)
  , "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean)
  , "UseEbsOptimizedInstances" :: NullOrUndefined (Boolean)
  , "LifecycleEventConfiguration" :: NullOrUndefined (LifecycleEventConfiguration)
  }


newtype LayerAttributes = LayerAttributes (Map LayerAttributesKeys String)


newtype LayerAttributesKeys = LayerAttributesKeys String


newtype LayerType = LayerType String


newtype Layers = Layers (Array Layer)


-- | <p>Specifies the lifecycle event configuration</p>
newtype LifecycleEventConfiguration = LifecycleEventConfiguration 
  { "Shutdown" :: NullOrUndefined (ShutdownEventConfiguration)
  }


newtype ListTagsRequest = ListTagsRequest 
  { "ResourceArn" :: (ResourceArn)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Contains the response to a <code>ListTags</code> request.</p>
newtype ListTagsResult = ListTagsResult 
  { "Tags" :: NullOrUndefined (Tags)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Describes a layer's load-based auto scaling configuration.</p>
newtype LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration 
  { "LayerId" :: NullOrUndefined (String)
  , "Enable" :: NullOrUndefined (Boolean)
  , "UpScaling" :: NullOrUndefined (AutoScalingThresholds)
  , "DownScaling" :: NullOrUndefined (AutoScalingThresholds)
  }


newtype LoadBasedAutoScalingConfigurations = LoadBasedAutoScalingConfigurations (Array LoadBasedAutoScalingConfiguration)


newtype MaxResults = MaxResults Int


newtype Minute = Minute Int


newtype NextToken = NextToken String


-- | <p>Describes supported operating systems in AWS OpsWorks Stacks.</p>
newtype OperatingSystem = OperatingSystem 
  { "Name" :: NullOrUndefined (String)
  , "Id" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  , "ConfigurationManagers" :: NullOrUndefined (OperatingSystemConfigurationManagers)
  , "ReportedName" :: NullOrUndefined (String)
  , "ReportedVersion" :: NullOrUndefined (String)
  , "Supported" :: NullOrUndefined (Boolean)
  }


-- | <p>A block that contains information about the configuration manager (Chef) and the versions of the configuration manager that are supported for an operating system.</p>
newtype OperatingSystemConfigurationManager = OperatingSystemConfigurationManager 
  { "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype OperatingSystemConfigurationManagers = OperatingSystemConfigurationManagers (Array OperatingSystemConfigurationManager)


newtype OperatingSystems = OperatingSystems (Array OperatingSystem)


newtype Parameters = Parameters (Map String String)


-- | <p>Describes stack or user permissions.</p>
newtype Permission = Permission 
  { "StackId" :: NullOrUndefined (String)
  , "IamUserArn" :: NullOrUndefined (String)
  , "AllowSsh" :: NullOrUndefined (Boolean)
  , "AllowSudo" :: NullOrUndefined (Boolean)
  , "Level" :: NullOrUndefined (String)
  }


newtype Permissions = Permissions (Array Permission)


-- | <p>Describes an instance's RAID array.</p>
newtype RaidArray = RaidArray 
  { "RaidArrayId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "RaidLevel" :: NullOrUndefined (Int)
  , "NumberOfDisks" :: NullOrUndefined (Int)
  , "Size" :: NullOrUndefined (Int)
  , "Device" :: NullOrUndefined (String)
  , "MountPoint" :: NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (DateTime)
  , "StackId" :: NullOrUndefined (String)
  , "VolumeType" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (Int)
  }


newtype RaidArrays = RaidArrays (Array RaidArray)


-- | <p>Describes an Amazon RDS instance.</p>
newtype RdsDbInstance = RdsDbInstance 
  { "RdsDbInstanceArn" :: NullOrUndefined (String)
  , "DbInstanceIdentifier" :: NullOrUndefined (String)
  , "DbUser" :: NullOrUndefined (String)
  , "DbPassword" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  , "Address" :: NullOrUndefined (String)
  , "Engine" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "MissingOnRds" :: NullOrUndefined (Boolean)
  }


newtype RdsDbInstances = RdsDbInstances (Array RdsDbInstance)


newtype RebootInstanceRequest = RebootInstanceRequest 
  { "InstanceId" :: (String)
  }


-- | <p>AWS OpsWorks Stacks supports five lifecycle events: <b>setup</b>, <b>configuration</b>, <b>deploy</b>, <b>undeploy</b>, and <b>shutdown</b>. For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. In addition, you can provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. <code>LayerCustomRecipes</code> specifies the custom recipes for a particular layer to be run in response to each of the five events. </p> <p>To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the .rb extension. For example: phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository's phpapp2 folder.</p>
newtype Recipes = Recipes 
  { "Setup" :: NullOrUndefined (Strings)
  , "Configure" :: NullOrUndefined (Strings)
  , "Deploy" :: NullOrUndefined (Strings)
  , "Undeploy" :: NullOrUndefined (Strings)
  , "Shutdown" :: NullOrUndefined (Strings)
  }


newtype RegisterEcsClusterRequest = RegisterEcsClusterRequest 
  { "EcsClusterArn" :: (String)
  , "StackId" :: (String)
  }


-- | <p>Contains the response to a <code>RegisterEcsCluster</code> request.</p>
newtype RegisterEcsClusterResult = RegisterEcsClusterResult 
  { "EcsClusterArn" :: NullOrUndefined (String)
  }


newtype RegisterElasticIpRequest = RegisterElasticIpRequest 
  { "ElasticIp" :: (String)
  , "StackId" :: (String)
  }


-- | <p>Contains the response to a <code>RegisterElasticIp</code> request.</p>
newtype RegisterElasticIpResult = RegisterElasticIpResult 
  { "ElasticIp" :: NullOrUndefined (String)
  }


newtype RegisterInstanceRequest = RegisterInstanceRequest 
  { "StackId" :: (String)
  , "Hostname" :: NullOrUndefined (String)
  , "PublicIp" :: NullOrUndefined (String)
  , "PrivateIp" :: NullOrUndefined (String)
  , "RsaPublicKey" :: NullOrUndefined (String)
  , "RsaPublicKeyFingerprint" :: NullOrUndefined (String)
  , "InstanceIdentity" :: NullOrUndefined (InstanceIdentity)
  }


-- | <p>Contains the response to a <code>RegisterInstanceResult</code> request.</p>
newtype RegisterInstanceResult = RegisterInstanceResult 
  { "InstanceId" :: NullOrUndefined (String)
  }


newtype RegisterRdsDbInstanceRequest = RegisterRdsDbInstanceRequest 
  { "StackId" :: (String)
  , "RdsDbInstanceArn" :: (String)
  , "DbUser" :: (String)
  , "DbPassword" :: (String)
  }


newtype RegisterVolumeRequest = RegisterVolumeRequest 
  { "Ec2VolumeId" :: NullOrUndefined (String)
  , "StackId" :: (String)
  }


-- | <p>Contains the response to a <code>RegisterVolume</code> request.</p>
newtype RegisterVolumeResult = RegisterVolumeResult 
  { "VolumeId" :: NullOrUndefined (String)
  }


-- | <p>A registered instance's reported operating system.</p>
newtype ReportedOs = ReportedOs 
  { "Family" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


newtype ResourceArn = ResourceArn String


-- | <p>Indicates that a resource was not found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype RootDeviceType = RootDeviceType String


-- | <p>Describes a user's SSH information.</p>
newtype SelfUserProfile = SelfUserProfile 
  { "IamUserArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SshUsername" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (String)
  }


-- | <p>Describes an AWS OpsWorks Stacks service error.</p>
newtype ServiceError = ServiceError 
  { "ServiceErrorId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (DateTime)
  }


newtype ServiceErrors = ServiceErrors (Array ServiceError)


newtype SetLoadBasedAutoScalingRequest = SetLoadBasedAutoScalingRequest 
  { "LayerId" :: (String)
  , "Enable" :: NullOrUndefined (Boolean)
  , "UpScaling" :: NullOrUndefined (AutoScalingThresholds)
  , "DownScaling" :: NullOrUndefined (AutoScalingThresholds)
  }


newtype SetPermissionRequest = SetPermissionRequest 
  { "StackId" :: (String)
  , "IamUserArn" :: (String)
  , "AllowSsh" :: NullOrUndefined (Boolean)
  , "AllowSudo" :: NullOrUndefined (Boolean)
  , "Level" :: NullOrUndefined (String)
  }


newtype SetTimeBasedAutoScalingRequest = SetTimeBasedAutoScalingRequest 
  { "InstanceId" :: (String)
  , "AutoScalingSchedule" :: NullOrUndefined (WeeklyAutoScalingSchedule)
  }


-- | <p>The Shutdown event configuration.</p>
newtype ShutdownEventConfiguration = ShutdownEventConfiguration 
  { "ExecutionTimeout" :: NullOrUndefined (Int)
  , "DelayUntilElbConnectionsDrained" :: NullOrUndefined (Boolean)
  }


-- | <p>Contains the information required to retrieve an app or cookbook from a repository. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html">Creating Apps</a> or <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html">Custom Recipes and Cookbooks</a>.</p>
newtype Source = Source 
  { "Type" :: NullOrUndefined (SourceType)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  , "Password" :: NullOrUndefined (String)
  , "SshKey" :: NullOrUndefined (String)
  , "Revision" :: NullOrUndefined (String)
  }


newtype SourceType = SourceType String


-- | <p>Describes an app's SSL configuration.</p>
newtype SslConfiguration = SslConfiguration 
  { "Certificate" :: (String)
  , "PrivateKey" :: (String)
  , "Chain" :: NullOrUndefined (String)
  }


-- | <p>Describes a stack.</p>
newtype Stack = Stack 
  { "StackId" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Arn" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  , "VpcId" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (StackAttributes)
  , "ServiceRoleArn" :: NullOrUndefined (String)
  , "DefaultInstanceProfileArn" :: NullOrUndefined (String)
  , "DefaultOs" :: NullOrUndefined (String)
  , "HostnameTheme" :: NullOrUndefined (String)
  , "DefaultAvailabilityZone" :: NullOrUndefined (String)
  , "DefaultSubnetId" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  , "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager)
  , "ChefConfiguration" :: NullOrUndefined (ChefConfiguration)
  , "UseCustomCookbooks" :: NullOrUndefined (Boolean)
  , "UseOpsworksSecurityGroups" :: NullOrUndefined (Boolean)
  , "CustomCookbooksSource" :: NullOrUndefined (Source)
  , "DefaultSshKeyName" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (DateTime)
  , "DefaultRootDeviceType" :: NullOrUndefined (RootDeviceType)
  , "AgentVersion" :: NullOrUndefined (String)
  }


newtype StackAttributes = StackAttributes (Map StackAttributesKeys String)


newtype StackAttributesKeys = StackAttributesKeys String


-- | <p>Describes the configuration manager.</p>
newtype StackConfigurationManager = StackConfigurationManager 
  { "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }


-- | <p>Summarizes the number of layers, instances, and apps in a stack.</p>
newtype StackSummary = StackSummary 
  { "StackId" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Arn" :: NullOrUndefined (String)
  , "LayersCount" :: NullOrUndefined (Int)
  , "AppsCount" :: NullOrUndefined (Int)
  , "InstancesCount" :: NullOrUndefined (InstancesCount)
  }


newtype Stacks = Stacks (Array Stack)


newtype StartInstanceRequest = StartInstanceRequest 
  { "InstanceId" :: (String)
  }


newtype StartStackRequest = StartStackRequest 
  { "StackId" :: (String)
  }


newtype StopInstanceRequest = StopInstanceRequest 
  { "InstanceId" :: (String)
  , "Force" :: NullOrUndefined (Boolean)
  }


newtype StopStackRequest = StopStackRequest 
  { "StackId" :: (String)
  }


newtype Strings = Strings (Array String)


newtype Switch = Switch String


newtype TagKey = TagKey String


newtype TagKeys = TagKeys (Array TagKey)


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn" :: (ResourceArn)
  , "Tags" :: (Tags)
  }


newtype TagValue = TagValue String


newtype Tags = Tags (Map TagKey TagValue)


-- | <p>Contains the data needed by RDP clients such as the Microsoft Remote Desktop Connection to log in to the instance.</p>
newtype TemporaryCredential = TemporaryCredential 
  { "Username" :: NullOrUndefined (String)
  , "Password" :: NullOrUndefined (String)
  , "ValidForInMinutes" :: NullOrUndefined (Int)
  , "InstanceId" :: NullOrUndefined (String)
  }


-- | <p>Describes an instance's time-based auto scaling configuration.</p>
newtype TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration 
  { "InstanceId" :: NullOrUndefined (String)
  , "AutoScalingSchedule" :: NullOrUndefined (WeeklyAutoScalingSchedule)
  }


newtype TimeBasedAutoScalingConfigurations = TimeBasedAutoScalingConfigurations (Array TimeBasedAutoScalingConfiguration)


newtype UnassignInstanceRequest = UnassignInstanceRequest 
  { "InstanceId" :: (String)
  }


newtype UnassignVolumeRequest = UnassignVolumeRequest 
  { "VolumeId" :: (String)
  }


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn" :: (ResourceArn)
  , "TagKeys" :: (TagKeys)
  }


newtype UpdateAppRequest = UpdateAppRequest 
  { "AppId" :: (String)
  , "Name" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "DataSources" :: NullOrUndefined (DataSources)
  , "Type" :: NullOrUndefined (AppType)
  , "AppSource" :: NullOrUndefined (Source)
  , "Domains" :: NullOrUndefined (Strings)
  , "EnableSsl" :: NullOrUndefined (Boolean)
  , "SslConfiguration" :: NullOrUndefined (SslConfiguration)
  , "Attributes" :: NullOrUndefined (AppAttributes)
  , "Environment" :: NullOrUndefined (EnvironmentVariables)
  }


newtype UpdateElasticIpRequest = UpdateElasticIpRequest 
  { "ElasticIp" :: (String)
  , "Name" :: NullOrUndefined (String)
  }


newtype UpdateInstanceRequest = UpdateInstanceRequest 
  { "InstanceId" :: (String)
  , "LayerIds" :: NullOrUndefined (Strings)
  , "InstanceType" :: NullOrUndefined (String)
  , "AutoScalingType" :: NullOrUndefined (AutoScalingType)
  , "Hostname" :: NullOrUndefined (String)
  , "Os" :: NullOrUndefined (String)
  , "AmiId" :: NullOrUndefined (String)
  , "SshKeyName" :: NullOrUndefined (String)
  , "Architecture" :: NullOrUndefined (Architecture)
  , "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean)
  , "EbsOptimized" :: NullOrUndefined (Boolean)
  , "AgentVersion" :: NullOrUndefined (String)
  }


newtype UpdateLayerRequest = UpdateLayerRequest 
  { "LayerId" :: (String)
  , "Name" :: NullOrUndefined (String)
  , "Shortname" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (LayerAttributes)
  , "CloudWatchLogsConfiguration" :: NullOrUndefined (CloudWatchLogsConfiguration)
  , "CustomInstanceProfileArn" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  , "CustomSecurityGroupIds" :: NullOrUndefined (Strings)
  , "Packages" :: NullOrUndefined (Strings)
  , "VolumeConfigurations" :: NullOrUndefined (VolumeConfigurations)
  , "EnableAutoHealing" :: NullOrUndefined (Boolean)
  , "AutoAssignElasticIps" :: NullOrUndefined (Boolean)
  , "AutoAssignPublicIps" :: NullOrUndefined (Boolean)
  , "CustomRecipes" :: NullOrUndefined (Recipes)
  , "InstallUpdatesOnBoot" :: NullOrUndefined (Boolean)
  , "UseEbsOptimizedInstances" :: NullOrUndefined (Boolean)
  , "LifecycleEventConfiguration" :: NullOrUndefined (LifecycleEventConfiguration)
  }


newtype UpdateMyUserProfileRequest = UpdateMyUserProfileRequest 
  { "SshPublicKey" :: NullOrUndefined (String)
  }


newtype UpdateRdsDbInstanceRequest = UpdateRdsDbInstanceRequest 
  { "RdsDbInstanceArn" :: (String)
  , "DbUser" :: NullOrUndefined (String)
  , "DbPassword" :: NullOrUndefined (String)
  }


newtype UpdateStackRequest = UpdateStackRequest 
  { "StackId" :: (String)
  , "Name" :: NullOrUndefined (String)
  , "Attributes" :: NullOrUndefined (StackAttributes)
  , "ServiceRoleArn" :: NullOrUndefined (String)
  , "DefaultInstanceProfileArn" :: NullOrUndefined (String)
  , "DefaultOs" :: NullOrUndefined (String)
  , "HostnameTheme" :: NullOrUndefined (String)
  , "DefaultAvailabilityZone" :: NullOrUndefined (String)
  , "DefaultSubnetId" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  , "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager)
  , "ChefConfiguration" :: NullOrUndefined (ChefConfiguration)
  , "UseCustomCookbooks" :: NullOrUndefined (Boolean)
  , "CustomCookbooksSource" :: NullOrUndefined (Source)
  , "DefaultSshKeyName" :: NullOrUndefined (String)
  , "DefaultRootDeviceType" :: NullOrUndefined (RootDeviceType)
  , "UseOpsworksSecurityGroups" :: NullOrUndefined (Boolean)
  , "AgentVersion" :: NullOrUndefined (String)
  }


newtype UpdateUserProfileRequest = UpdateUserProfileRequest 
  { "IamUserArn" :: (String)
  , "SshUsername" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (String)
  , "AllowSelfManagement" :: NullOrUndefined (Boolean)
  }


newtype UpdateVolumeRequest = UpdateVolumeRequest 
  { "VolumeId" :: (String)
  , "Name" :: NullOrUndefined (String)
  , "MountPoint" :: NullOrUndefined (String)
  }


-- | <p>Describes a user's SSH information.</p>
newtype UserProfile = UserProfile 
  { "IamUserArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SshUsername" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (String)
  , "AllowSelfManagement" :: NullOrUndefined (Boolean)
  }


newtype UserProfiles = UserProfiles (Array UserProfile)


newtype ValidForInMinutes = ValidForInMinutes Int


-- | <p>Indicates that a request was not valid.</p>
newtype ValidationException = ValidationException 
  { "Message'" :: NullOrUndefined (String)
  }


newtype VirtualizationType = VirtualizationType String


-- | <p>Describes an instance's Amazon EBS volume.</p>
newtype Volume = Volume 
  { "VolumeId" :: NullOrUndefined (String)
  , "Ec2VolumeId" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "RaidArrayId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "Status" :: NullOrUndefined (String)
  , "Size" :: NullOrUndefined (Int)
  , "Device" :: NullOrUndefined (String)
  , "MountPoint" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  , "AvailabilityZone" :: NullOrUndefined (String)
  , "VolumeType" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (Int)
  , "Encrypted" :: NullOrUndefined (Boolean)
  }


-- | <p>Describes an Amazon EBS volume configuration.</p>
newtype VolumeConfiguration = VolumeConfiguration 
  { "MountPoint" :: (String)
  , "RaidLevel" :: NullOrUndefined (Int)
  , "NumberOfDisks" :: (Int)
  , "Size" :: (Int)
  , "VolumeType" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (Int)
  , "Encrypted" :: NullOrUndefined (Boolean)
  }


newtype VolumeConfigurations = VolumeConfigurations (Array VolumeConfiguration)


newtype VolumeType = VolumeType String


newtype Volumes = Volumes (Array Volume)


-- | <p>Describes a time-based instance's auto scaling schedule. The schedule consists of a set of key-value pairs.</p> <ul> <li> <p>The key is the time period (a UTC hour) and must be an integer from 0 - 23.</p> </li> <li> <p>The value indicates whether the instance should be online or offline for the specified period, and must be set to "on" or "off"</p> </li> </ul> <p>The default setting for all time periods is off, so you use the following parameters primarily to specify the online periods. You don't have to explicitly specify offline periods unless you want to change an online period to an offline period.</p> <p>The following example specifies that the instance should be online for four hours, from UTC 1200 - 1600. It will be off for the remainder of the day.</p> <p> <code> { "12":"on", "13":"on", "14":"on", "15":"on" } </code> </p>
newtype WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule 
  { "Monday" :: NullOrUndefined (DailyAutoScalingSchedule)
  , "Tuesday" :: NullOrUndefined (DailyAutoScalingSchedule)
  , "Wednesday" :: NullOrUndefined (DailyAutoScalingSchedule)
  , "Thursday" :: NullOrUndefined (DailyAutoScalingSchedule)
  , "Friday" :: NullOrUndefined (DailyAutoScalingSchedule)
  , "Saturday" :: NullOrUndefined (DailyAutoScalingSchedule)
  , "Sunday" :: NullOrUndefined (DailyAutoScalingSchedule)
  }
