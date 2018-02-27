

-- | <fullname>AWS OpsWorks</fullname> <p>Welcome to the <i>AWS OpsWorks Stacks API Reference</i>. This guide provides descriptions, syntax, and usage examples for AWS OpsWorks Stacks actions and data types, including common parameters and error codes. </p> <p>AWS OpsWorks Stacks is an application management service that provides an integrated experience for overseeing the complete application lifecycle. For information about this product, go to the <a href="http://aws.amazon.com/opsworks/">AWS OpsWorks</a> details page. </p> <p> <b>SDKs and CLI</b> </p> <p>The most common way to use the AWS OpsWorks Stacks API is by using the AWS Command Line Interface (CLI) or by using one of the AWS SDKs to implement applications in your preferred language. For more information, see:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/cli/latest/userguide/cli-chap-welcome.html">AWS CLI</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/opsworks/AWSOpsWorksClient.html">AWS SDK for Java</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/sdkfornet/latest/apidocs/html/N_Amazon_OpsWorks.htm">AWS SDK for .NET</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/aws-sdk-php-2/latest/class-Aws.OpsWorks.OpsWorksClient.html">AWS SDK for PHP 2</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/sdkforruby/api/">AWS SDK for Ruby</a> </p> </li> <li> <p> <a href="http://aws.amazon.com/documentation/sdkforjavascript/">AWS SDK for Node.js</a> </p> </li> <li> <p> <a href="http://docs.pythonboto.org/en/latest/ref/opsworks.html">AWS SDK for Python(Boto)</a> </p> </li> </ul> <p> <b>Endpoints</b> </p> <p>AWS OpsWorks Stacks supports the following endpoints, all HTTPS. You must connect to one of the following endpoints. Stacks can only be accessed or managed within the endpoint in which they are created.</p> <ul> <li> <p>opsworks.us-east-1.amazonaws.com</p> </li> <li> <p>opsworks.us-east-2.amazonaws.com</p> </li> <li> <p>opsworks.us-west-1.amazonaws.com</p> </li> <li> <p>opsworks.us-west-2.amazonaws.com</p> </li> <li> <p>opsworks.ca-central-1.amazonaws.com (API only; not available in the AWS console)</p> </li> <li> <p>opsworks.eu-west-1.amazonaws.com</p> </li> <li> <p>opsworks.eu-west-2.amazonaws.com</p> </li> <li> <p>opsworks.eu-west-3.amazonaws.com</p> </li> <li> <p>opsworks.eu-central-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-northeast-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-northeast-2.amazonaws.com</p> </li> <li> <p>opsworks.ap-south-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-southeast-1.amazonaws.com</p> </li> <li> <p>opsworks.ap-southeast-2.amazonaws.com</p> </li> <li> <p>opsworks.sa-east-1.amazonaws.com</p> </li> </ul> <p> <b>Chef Versions</b> </p> <p>When you call <a>CreateStack</a>, <a>CloneStack</a>, or <a>UpdateStack</a> we recommend you use the <code>ConfigurationManager</code> parameter to specify the Chef version. The recommended and default value for Linux stacks is currently 12. Windows stacks use Chef 12.2. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook-chef11.html">Chef Versions</a>.</p> <note> <p>You can specify Chef 12, 11.10, or 11.4 for your Linux stack. We recommend migrating your existing Linux stacks to Chef 12 as soon as possible.</p> </note>
module AWS.OpsWorks where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeAgentVersion :: Newtype AgentVersion _


newtype AgentVersions = AgentVersions (Array AgentVersion)
derive instance newtypeAgentVersions :: Newtype AgentVersions _


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
derive instance newtypeApp :: Newtype App _


newtype AppAttributes = AppAttributes (Map AppAttributesKeys String)
derive instance newtypeAppAttributes :: Newtype AppAttributes _


newtype AppAttributesKeys = AppAttributesKeys String
derive instance newtypeAppAttributesKeys :: Newtype AppAttributesKeys _


newtype AppType = AppType String
derive instance newtypeAppType :: Newtype AppType _


newtype Apps = Apps (Array App)
derive instance newtypeApps :: Newtype Apps _


newtype Architecture = Architecture String
derive instance newtypeArchitecture :: Newtype Architecture _


newtype AssignInstanceRequest = AssignInstanceRequest 
  { "InstanceId" :: (String)
  , "LayerIds" :: (Strings)
  }
derive instance newtypeAssignInstanceRequest :: Newtype AssignInstanceRequest _


newtype AssignVolumeRequest = AssignVolumeRequest 
  { "VolumeId" :: (String)
  , "InstanceId" :: NullOrUndefined (String)
  }
derive instance newtypeAssignVolumeRequest :: Newtype AssignVolumeRequest _


newtype AssociateElasticIpRequest = AssociateElasticIpRequest 
  { "ElasticIp" :: (String)
  , "InstanceId" :: NullOrUndefined (String)
  }
derive instance newtypeAssociateElasticIpRequest :: Newtype AssociateElasticIpRequest _


newtype AttachElasticLoadBalancerRequest = AttachElasticLoadBalancerRequest 
  { "ElasticLoadBalancerName" :: (String)
  , "LayerId" :: (String)
  }
derive instance newtypeAttachElasticLoadBalancerRequest :: Newtype AttachElasticLoadBalancerRequest _


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
derive instance newtypeAutoScalingThresholds :: Newtype AutoScalingThresholds _


newtype AutoScalingType = AutoScalingType String
derive instance newtypeAutoScalingType :: Newtype AutoScalingType _


-- | <p>Describes a block device mapping. This data type maps directly to the Amazon EC2 <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html">BlockDeviceMapping</a> data type. </p>
newtype BlockDeviceMapping = BlockDeviceMapping 
  { "DeviceName" :: NullOrUndefined (String)
  , "NoDevice" :: NullOrUndefined (String)
  , "VirtualName" :: NullOrUndefined (String)
  , "Ebs" :: NullOrUndefined (EbsBlockDevice)
  }
derive instance newtypeBlockDeviceMapping :: Newtype BlockDeviceMapping _


newtype BlockDeviceMappings = BlockDeviceMappings (Array BlockDeviceMapping)
derive instance newtypeBlockDeviceMappings :: Newtype BlockDeviceMappings _


-- | <p>Describes the Chef configuration.</p>
newtype ChefConfiguration = ChefConfiguration 
  { "ManageBerkshelf" :: NullOrUndefined (Boolean)
  , "BerkshelfVersion" :: NullOrUndefined (String)
  }
derive instance newtypeChefConfiguration :: Newtype ChefConfiguration _


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
derive instance newtypeCloneStackRequest :: Newtype CloneStackRequest _


-- | <p>Contains the response to a <code>CloneStack</code> request.</p>
newtype CloneStackResult = CloneStackResult 
  { "StackId" :: NullOrUndefined (String)
  }
derive instance newtypeCloneStackResult :: Newtype CloneStackResult _


-- | <p>Describes the Amazon CloudWatch logs configuration for a layer.</p>
newtype CloudWatchLogsConfiguration = CloudWatchLogsConfiguration 
  { "Enabled" :: NullOrUndefined (Boolean)
  , "LogStreams" :: NullOrUndefined (CloudWatchLogsLogStreams)
  }
derive instance newtypeCloudWatchLogsConfiguration :: Newtype CloudWatchLogsConfiguration _


-- | <p>Specifies the encoding of the log file so that the file can be read correctly. The default is <code>utf_8</code>. Encodings supported by Python <code>codecs.decode()</code> can be used here.</p>
newtype CloudWatchLogsEncoding = CloudWatchLogsEncoding String
derive instance newtypeCloudWatchLogsEncoding :: Newtype CloudWatchLogsEncoding _


-- | <p>Specifies where to start to read data (start_of_file or end_of_file). The default is start_of_file. It's only used if there is no state persisted for that log stream.</p>
newtype CloudWatchLogsInitialPosition = CloudWatchLogsInitialPosition String
derive instance newtypeCloudWatchLogsInitialPosition :: Newtype CloudWatchLogsInitialPosition _


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
derive instance newtypeCloudWatchLogsLogStream :: Newtype CloudWatchLogsLogStream _


-- | <p>Describes the Amazon CloudWatch logs configuration for a layer.</p>
newtype CloudWatchLogsLogStreams = CloudWatchLogsLogStreams (Array CloudWatchLogsLogStream)
derive instance newtypeCloudWatchLogsLogStreams :: Newtype CloudWatchLogsLogStreams _


-- | <p>The preferred time zone for logs streamed to CloudWatch Logs. Valid values are <code>LOCAL</code> and <code>UTC</code>, for Coordinated Universal Time.</p>
newtype CloudWatchLogsTimeZone = CloudWatchLogsTimeZone String
derive instance newtypeCloudWatchLogsTimeZone :: Newtype CloudWatchLogsTimeZone _


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
derive instance newtypeCommand :: Newtype Command _


newtype Commands = Commands (Array Command)
derive instance newtypeCommands :: Newtype Commands _


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
derive instance newtypeCreateAppRequest :: Newtype CreateAppRequest _


-- | <p>Contains the response to a <code>CreateApp</code> request.</p>
newtype CreateAppResult = CreateAppResult 
  { "AppId" :: NullOrUndefined (String)
  }
derive instance newtypeCreateAppResult :: Newtype CreateAppResult _


newtype CreateDeploymentRequest = CreateDeploymentRequest 
  { "StackId" :: (String)
  , "AppId" :: NullOrUndefined (String)
  , "InstanceIds" :: NullOrUndefined (Strings)
  , "LayerIds" :: NullOrUndefined (Strings)
  , "Command" :: (DeploymentCommand)
  , "Comment" :: NullOrUndefined (String)
  , "CustomJson" :: NullOrUndefined (String)
  }
derive instance newtypeCreateDeploymentRequest :: Newtype CreateDeploymentRequest _


-- | <p>Contains the response to a <code>CreateDeployment</code> request.</p>
newtype CreateDeploymentResult = CreateDeploymentResult 
  { "DeploymentId" :: NullOrUndefined (String)
  }
derive instance newtypeCreateDeploymentResult :: Newtype CreateDeploymentResult _


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
derive instance newtypeCreateInstanceRequest :: Newtype CreateInstanceRequest _


-- | <p>Contains the response to a <code>CreateInstance</code> request.</p>
newtype CreateInstanceResult = CreateInstanceResult 
  { "InstanceId" :: NullOrUndefined (String)
  }
derive instance newtypeCreateInstanceResult :: Newtype CreateInstanceResult _


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
derive instance newtypeCreateLayerRequest :: Newtype CreateLayerRequest _


-- | <p>Contains the response to a <code>CreateLayer</code> request.</p>
newtype CreateLayerResult = CreateLayerResult 
  { "LayerId" :: NullOrUndefined (String)
  }
derive instance newtypeCreateLayerResult :: Newtype CreateLayerResult _


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
derive instance newtypeCreateStackRequest :: Newtype CreateStackRequest _


-- | <p>Contains the response to a <code>CreateStack</code> request.</p>
newtype CreateStackResult = CreateStackResult 
  { "StackId" :: NullOrUndefined (String)
  }
derive instance newtypeCreateStackResult :: Newtype CreateStackResult _


newtype CreateUserProfileRequest = CreateUserProfileRequest 
  { "IamUserArn" :: (String)
  , "SshUsername" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (String)
  , "AllowSelfManagement" :: NullOrUndefined (Boolean)
  }
derive instance newtypeCreateUserProfileRequest :: Newtype CreateUserProfileRequest _


-- | <p>Contains the response to a <code>CreateUserProfile</code> request.</p>
newtype CreateUserProfileResult = CreateUserProfileResult 
  { "IamUserArn" :: NullOrUndefined (String)
  }
derive instance newtypeCreateUserProfileResult :: Newtype CreateUserProfileResult _


newtype DailyAutoScalingSchedule = DailyAutoScalingSchedule (Map Hour Switch)
derive instance newtypeDailyAutoScalingSchedule :: Newtype DailyAutoScalingSchedule _


-- | <p>Describes an app's data source.</p>
newtype DataSource = DataSource 
  { "Type" :: NullOrUndefined (String)
  , "Arn" :: NullOrUndefined (String)
  , "DatabaseName" :: NullOrUndefined (String)
  }
derive instance newtypeDataSource :: Newtype DataSource _


newtype DataSources = DataSources (Array DataSource)
derive instance newtypeDataSources :: Newtype DataSources _


newtype DateTime = DateTime String
derive instance newtypeDateTime :: Newtype DateTime _


newtype DeleteAppRequest = DeleteAppRequest 
  { "AppId" :: (String)
  }
derive instance newtypeDeleteAppRequest :: Newtype DeleteAppRequest _


newtype DeleteInstanceRequest = DeleteInstanceRequest 
  { "InstanceId" :: (String)
  , "DeleteElasticIp" :: NullOrUndefined (Boolean)
  , "DeleteVolumes" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDeleteInstanceRequest :: Newtype DeleteInstanceRequest _


newtype DeleteLayerRequest = DeleteLayerRequest 
  { "LayerId" :: (String)
  }
derive instance newtypeDeleteLayerRequest :: Newtype DeleteLayerRequest _


newtype DeleteStackRequest = DeleteStackRequest 
  { "StackId" :: (String)
  }
derive instance newtypeDeleteStackRequest :: Newtype DeleteStackRequest _


newtype DeleteUserProfileRequest = DeleteUserProfileRequest 
  { "IamUserArn" :: (String)
  }
derive instance newtypeDeleteUserProfileRequest :: Newtype DeleteUserProfileRequest _


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
derive instance newtypeDeployment :: Newtype Deployment _


-- | <p>Used to specify a stack or deployment command.</p>
newtype DeploymentCommand = DeploymentCommand 
  { "Name" :: (DeploymentCommandName)
  , "Args" :: NullOrUndefined (DeploymentCommandArgs)
  }
derive instance newtypeDeploymentCommand :: Newtype DeploymentCommand _


newtype DeploymentCommandArgs = DeploymentCommandArgs (Map String Strings)
derive instance newtypeDeploymentCommandArgs :: Newtype DeploymentCommandArgs _


newtype DeploymentCommandName = DeploymentCommandName String
derive instance newtypeDeploymentCommandName :: Newtype DeploymentCommandName _


newtype Deployments = Deployments (Array Deployment)
derive instance newtypeDeployments :: Newtype Deployments _


newtype DeregisterEcsClusterRequest = DeregisterEcsClusterRequest 
  { "EcsClusterArn" :: (String)
  }
derive instance newtypeDeregisterEcsClusterRequest :: Newtype DeregisterEcsClusterRequest _


newtype DeregisterElasticIpRequest = DeregisterElasticIpRequest 
  { "ElasticIp" :: (String)
  }
derive instance newtypeDeregisterElasticIpRequest :: Newtype DeregisterElasticIpRequest _


newtype DeregisterInstanceRequest = DeregisterInstanceRequest 
  { "InstanceId" :: (String)
  }
derive instance newtypeDeregisterInstanceRequest :: Newtype DeregisterInstanceRequest _


newtype DeregisterRdsDbInstanceRequest = DeregisterRdsDbInstanceRequest 
  { "RdsDbInstanceArn" :: (String)
  }
derive instance newtypeDeregisterRdsDbInstanceRequest :: Newtype DeregisterRdsDbInstanceRequest _


newtype DeregisterVolumeRequest = DeregisterVolumeRequest 
  { "VolumeId" :: (String)
  }
derive instance newtypeDeregisterVolumeRequest :: Newtype DeregisterVolumeRequest _


newtype DescribeAgentVersionsRequest = DescribeAgentVersionsRequest 
  { "StackId" :: NullOrUndefined (String)
  , "ConfigurationManager" :: NullOrUndefined (StackConfigurationManager)
  }
derive instance newtypeDescribeAgentVersionsRequest :: Newtype DescribeAgentVersionsRequest _


-- | <p>Contains the response to a <code>DescribeAgentVersions</code> request.</p>
newtype DescribeAgentVersionsResult = DescribeAgentVersionsResult 
  { "AgentVersions" :: NullOrUndefined (AgentVersions)
  }
derive instance newtypeDescribeAgentVersionsResult :: Newtype DescribeAgentVersionsResult _


newtype DescribeAppsRequest = DescribeAppsRequest 
  { "StackId" :: NullOrUndefined (String)
  , "AppIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeAppsRequest :: Newtype DescribeAppsRequest _


-- | <p>Contains the response to a <code>DescribeApps</code> request.</p>
newtype DescribeAppsResult = DescribeAppsResult 
  { "Apps" :: NullOrUndefined (Apps)
  }
derive instance newtypeDescribeAppsResult :: Newtype DescribeAppsResult _


newtype DescribeCommandsRequest = DescribeCommandsRequest 
  { "DeploymentId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "CommandIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeCommandsRequest :: Newtype DescribeCommandsRequest _


-- | <p>Contains the response to a <code>DescribeCommands</code> request.</p>
newtype DescribeCommandsResult = DescribeCommandsResult 
  { "Commands" :: NullOrUndefined (Commands)
  }
derive instance newtypeDescribeCommandsResult :: Newtype DescribeCommandsResult _


newtype DescribeDeploymentsRequest = DescribeDeploymentsRequest 
  { "StackId" :: NullOrUndefined (String)
  , "AppId" :: NullOrUndefined (String)
  , "DeploymentIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeDeploymentsRequest :: Newtype DescribeDeploymentsRequest _


-- | <p>Contains the response to a <code>DescribeDeployments</code> request.</p>
newtype DescribeDeploymentsResult = DescribeDeploymentsResult 
  { "Deployments" :: NullOrUndefined (Deployments)
  }
derive instance newtypeDescribeDeploymentsResult :: Newtype DescribeDeploymentsResult _


newtype DescribeEcsClustersRequest = DescribeEcsClustersRequest 
  { "EcsClusterArns" :: NullOrUndefined (Strings)
  , "StackId" :: NullOrUndefined (String)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (Int)
  }
derive instance newtypeDescribeEcsClustersRequest :: Newtype DescribeEcsClustersRequest _


-- | <p>Contains the response to a <code>DescribeEcsClusters</code> request.</p>
newtype DescribeEcsClustersResult = DescribeEcsClustersResult 
  { "EcsClusters" :: NullOrUndefined (EcsClusters)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeEcsClustersResult :: Newtype DescribeEcsClustersResult _


newtype DescribeElasticIpsRequest = DescribeElasticIpsRequest 
  { "InstanceId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "Ips" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeElasticIpsRequest :: Newtype DescribeElasticIpsRequest _


-- | <p>Contains the response to a <code>DescribeElasticIps</code> request.</p>
newtype DescribeElasticIpsResult = DescribeElasticIpsResult 
  { "ElasticIps" :: NullOrUndefined (ElasticIps)
  }
derive instance newtypeDescribeElasticIpsResult :: Newtype DescribeElasticIpsResult _


newtype DescribeElasticLoadBalancersRequest = DescribeElasticLoadBalancersRequest 
  { "StackId" :: NullOrUndefined (String)
  , "LayerIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeElasticLoadBalancersRequest :: Newtype DescribeElasticLoadBalancersRequest _


-- | <p>Contains the response to a <code>DescribeElasticLoadBalancers</code> request.</p>
newtype DescribeElasticLoadBalancersResult = DescribeElasticLoadBalancersResult 
  { "ElasticLoadBalancers" :: NullOrUndefined (ElasticLoadBalancers)
  }
derive instance newtypeDescribeElasticLoadBalancersResult :: Newtype DescribeElasticLoadBalancersResult _


newtype DescribeInstancesRequest = DescribeInstancesRequest 
  { "StackId" :: NullOrUndefined (String)
  , "LayerId" :: NullOrUndefined (String)
  , "InstanceIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeInstancesRequest :: Newtype DescribeInstancesRequest _


-- | <p>Contains the response to a <code>DescribeInstances</code> request.</p>
newtype DescribeInstancesResult = DescribeInstancesResult 
  { "Instances" :: NullOrUndefined (Instances)
  }
derive instance newtypeDescribeInstancesResult :: Newtype DescribeInstancesResult _


newtype DescribeLayersRequest = DescribeLayersRequest 
  { "StackId" :: NullOrUndefined (String)
  , "LayerIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeLayersRequest :: Newtype DescribeLayersRequest _


-- | <p>Contains the response to a <code>DescribeLayers</code> request.</p>
newtype DescribeLayersResult = DescribeLayersResult 
  { "Layers" :: NullOrUndefined (Layers)
  }
derive instance newtypeDescribeLayersResult :: Newtype DescribeLayersResult _


newtype DescribeLoadBasedAutoScalingRequest = DescribeLoadBasedAutoScalingRequest 
  { "LayerIds" :: (Strings)
  }
derive instance newtypeDescribeLoadBasedAutoScalingRequest :: Newtype DescribeLoadBasedAutoScalingRequest _


-- | <p>Contains the response to a <code>DescribeLoadBasedAutoScaling</code> request.</p>
newtype DescribeLoadBasedAutoScalingResult = DescribeLoadBasedAutoScalingResult 
  { "LoadBasedAutoScalingConfigurations" :: NullOrUndefined (LoadBasedAutoScalingConfigurations)
  }
derive instance newtypeDescribeLoadBasedAutoScalingResult :: Newtype DescribeLoadBasedAutoScalingResult _


-- | <p>Contains the response to a <code>DescribeMyUserProfile</code> request.</p>
newtype DescribeMyUserProfileResult = DescribeMyUserProfileResult 
  { "UserProfile" :: NullOrUndefined (SelfUserProfile)
  }
derive instance newtypeDescribeMyUserProfileResult :: Newtype DescribeMyUserProfileResult _


-- | <p>The response to a <code>DescribeOperatingSystems</code> request.</p>
newtype DescribeOperatingSystemsResponse = DescribeOperatingSystemsResponse 
  { "OperatingSystems" :: NullOrUndefined (OperatingSystems)
  }
derive instance newtypeDescribeOperatingSystemsResponse :: Newtype DescribeOperatingSystemsResponse _


newtype DescribePermissionsRequest = DescribePermissionsRequest 
  { "IamUserArn" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  }
derive instance newtypeDescribePermissionsRequest :: Newtype DescribePermissionsRequest _


-- | <p>Contains the response to a <code>DescribePermissions</code> request.</p>
newtype DescribePermissionsResult = DescribePermissionsResult 
  { "Permissions" :: NullOrUndefined (Permissions)
  }
derive instance newtypeDescribePermissionsResult :: Newtype DescribePermissionsResult _


newtype DescribeRaidArraysRequest = DescribeRaidArraysRequest 
  { "InstanceId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "RaidArrayIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeRaidArraysRequest :: Newtype DescribeRaidArraysRequest _


-- | <p>Contains the response to a <code>DescribeRaidArrays</code> request.</p>
newtype DescribeRaidArraysResult = DescribeRaidArraysResult 
  { "RaidArrays" :: NullOrUndefined (RaidArrays)
  }
derive instance newtypeDescribeRaidArraysResult :: Newtype DescribeRaidArraysResult _


newtype DescribeRdsDbInstancesRequest = DescribeRdsDbInstancesRequest 
  { "StackId" :: (String)
  , "RdsDbInstanceArns" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeRdsDbInstancesRequest :: Newtype DescribeRdsDbInstancesRequest _


-- | <p>Contains the response to a <code>DescribeRdsDbInstances</code> request.</p>
newtype DescribeRdsDbInstancesResult = DescribeRdsDbInstancesResult 
  { "RdsDbInstances" :: NullOrUndefined (RdsDbInstances)
  }
derive instance newtypeDescribeRdsDbInstancesResult :: Newtype DescribeRdsDbInstancesResult _


newtype DescribeServiceErrorsRequest = DescribeServiceErrorsRequest 
  { "StackId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "ServiceErrorIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeServiceErrorsRequest :: Newtype DescribeServiceErrorsRequest _


-- | <p>Contains the response to a <code>DescribeServiceErrors</code> request.</p>
newtype DescribeServiceErrorsResult = DescribeServiceErrorsResult 
  { "ServiceErrors" :: NullOrUndefined (ServiceErrors)
  }
derive instance newtypeDescribeServiceErrorsResult :: Newtype DescribeServiceErrorsResult _


newtype DescribeStackProvisioningParametersRequest = DescribeStackProvisioningParametersRequest 
  { "StackId" :: (String)
  }
derive instance newtypeDescribeStackProvisioningParametersRequest :: Newtype DescribeStackProvisioningParametersRequest _


-- | <p>Contains the response to a <code>DescribeStackProvisioningParameters</code> request.</p>
newtype DescribeStackProvisioningParametersResult = DescribeStackProvisioningParametersResult 
  { "AgentInstallerUrl" :: NullOrUndefined (String)
  , "Parameters" :: NullOrUndefined (Parameters)
  }
derive instance newtypeDescribeStackProvisioningParametersResult :: Newtype DescribeStackProvisioningParametersResult _


newtype DescribeStackSummaryRequest = DescribeStackSummaryRequest 
  { "StackId" :: (String)
  }
derive instance newtypeDescribeStackSummaryRequest :: Newtype DescribeStackSummaryRequest _


-- | <p>Contains the response to a <code>DescribeStackSummary</code> request.</p>
newtype DescribeStackSummaryResult = DescribeStackSummaryResult 
  { "StackSummary" :: NullOrUndefined (StackSummary)
  }
derive instance newtypeDescribeStackSummaryResult :: Newtype DescribeStackSummaryResult _


newtype DescribeStacksRequest = DescribeStacksRequest 
  { "StackIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeStacksRequest :: Newtype DescribeStacksRequest _


-- | <p>Contains the response to a <code>DescribeStacks</code> request.</p>
newtype DescribeStacksResult = DescribeStacksResult 
  { "Stacks" :: NullOrUndefined (Stacks)
  }
derive instance newtypeDescribeStacksResult :: Newtype DescribeStacksResult _


newtype DescribeTimeBasedAutoScalingRequest = DescribeTimeBasedAutoScalingRequest 
  { "InstanceIds" :: (Strings)
  }
derive instance newtypeDescribeTimeBasedAutoScalingRequest :: Newtype DescribeTimeBasedAutoScalingRequest _


-- | <p>Contains the response to a <code>DescribeTimeBasedAutoScaling</code> request.</p>
newtype DescribeTimeBasedAutoScalingResult = DescribeTimeBasedAutoScalingResult 
  { "TimeBasedAutoScalingConfigurations" :: NullOrUndefined (TimeBasedAutoScalingConfigurations)
  }
derive instance newtypeDescribeTimeBasedAutoScalingResult :: Newtype DescribeTimeBasedAutoScalingResult _


newtype DescribeUserProfilesRequest = DescribeUserProfilesRequest 
  { "IamUserArns" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeUserProfilesRequest :: Newtype DescribeUserProfilesRequest _


-- | <p>Contains the response to a <code>DescribeUserProfiles</code> request.</p>
newtype DescribeUserProfilesResult = DescribeUserProfilesResult 
  { "UserProfiles" :: NullOrUndefined (UserProfiles)
  }
derive instance newtypeDescribeUserProfilesResult :: Newtype DescribeUserProfilesResult _


newtype DescribeVolumesRequest = DescribeVolumesRequest 
  { "InstanceId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "RaidArrayId" :: NullOrUndefined (String)
  , "VolumeIds" :: NullOrUndefined (Strings)
  }
derive instance newtypeDescribeVolumesRequest :: Newtype DescribeVolumesRequest _


-- | <p>Contains the response to a <code>DescribeVolumes</code> request.</p>
newtype DescribeVolumesResult = DescribeVolumesResult 
  { "Volumes" :: NullOrUndefined (Volumes)
  }
derive instance newtypeDescribeVolumesResult :: Newtype DescribeVolumesResult _


newtype DetachElasticLoadBalancerRequest = DetachElasticLoadBalancerRequest 
  { "ElasticLoadBalancerName" :: (String)
  , "LayerId" :: (String)
  }
derive instance newtypeDetachElasticLoadBalancerRequest :: Newtype DetachElasticLoadBalancerRequest _


newtype DisassociateElasticIpRequest = DisassociateElasticIpRequest 
  { "ElasticIp" :: (String)
  }
derive instance newtypeDisassociateElasticIpRequest :: Newtype DisassociateElasticIpRequest _


-- | <p>Describes an Amazon EBS volume. This data type maps directly to the Amazon EC2 <a href="http://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_EbsBlockDevice.html">EbsBlockDevice</a> data type.</p>
newtype EbsBlockDevice = EbsBlockDevice 
  { "SnapshotId" :: NullOrUndefined (String)
  , "Iops" :: NullOrUndefined (Int)
  , "VolumeSize" :: NullOrUndefined (Int)
  , "VolumeType" :: NullOrUndefined (VolumeType)
  , "DeleteOnTermination" :: NullOrUndefined (Boolean)
  }
derive instance newtypeEbsBlockDevice :: Newtype EbsBlockDevice _


-- | <p>Describes a registered Amazon ECS cluster.</p>
newtype EcsCluster = EcsCluster 
  { "EcsClusterArn" :: NullOrUndefined (String)
  , "EcsClusterName" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "RegisteredAt" :: NullOrUndefined (DateTime)
  }
derive instance newtypeEcsCluster :: Newtype EcsCluster _


newtype EcsClusters = EcsClusters (Array EcsCluster)
derive instance newtypeEcsClusters :: Newtype EcsClusters _


-- | <p>Describes an Elastic IP address.</p>
newtype ElasticIp = ElasticIp 
  { "Ip" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Domain" :: NullOrUndefined (String)
  , "Region" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  }
derive instance newtypeElasticIp :: Newtype ElasticIp _


newtype ElasticIps = ElasticIps (Array ElasticIp)
derive instance newtypeElasticIps :: Newtype ElasticIps _


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
derive instance newtypeElasticLoadBalancer :: Newtype ElasticLoadBalancer _


newtype ElasticLoadBalancers = ElasticLoadBalancers (Array ElasticLoadBalancer)
derive instance newtypeElasticLoadBalancers :: Newtype ElasticLoadBalancers _


-- | <p>Represents an app's environment variable.</p>
newtype EnvironmentVariable = EnvironmentVariable 
  { "Key" :: (String)
  , "Value" :: (String)
  , "Secure" :: NullOrUndefined (Boolean)
  }
derive instance newtypeEnvironmentVariable :: Newtype EnvironmentVariable _


newtype EnvironmentVariables = EnvironmentVariables (Array EnvironmentVariable)
derive instance newtypeEnvironmentVariables :: Newtype EnvironmentVariables _


newtype GetHostnameSuggestionRequest = GetHostnameSuggestionRequest 
  { "LayerId" :: (String)
  }
derive instance newtypeGetHostnameSuggestionRequest :: Newtype GetHostnameSuggestionRequest _


-- | <p>Contains the response to a <code>GetHostnameSuggestion</code> request.</p>
newtype GetHostnameSuggestionResult = GetHostnameSuggestionResult 
  { "LayerId" :: NullOrUndefined (String)
  , "Hostname" :: NullOrUndefined (String)
  }
derive instance newtypeGetHostnameSuggestionResult :: Newtype GetHostnameSuggestionResult _


newtype GrantAccessRequest = GrantAccessRequest 
  { "InstanceId" :: (String)
  , "ValidForInMinutes" :: NullOrUndefined (ValidForInMinutes)
  }
derive instance newtypeGrantAccessRequest :: Newtype GrantAccessRequest _


-- | <p>Contains the response to a <code>GrantAccess</code> request.</p>
newtype GrantAccessResult = GrantAccessResult 
  { "TemporaryCredential" :: NullOrUndefined (TemporaryCredential)
  }
derive instance newtypeGrantAccessResult :: Newtype GrantAccessResult _


newtype Hour = Hour String
derive instance newtypeHour :: Newtype Hour _


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
derive instance newtypeInstance :: Newtype Instance _


-- | <p>Contains a description of an Amazon EC2 instance from the Amazon EC2 metadata service. For more information, see <a href="http://docs.aws.amazon.com/sdkfornet/latest/apidocs/Index.html">Instance Metadata and User Data</a>.</p>
newtype InstanceIdentity = InstanceIdentity 
  { "Document" :: NullOrUndefined (String)
  , "Signature" :: NullOrUndefined (String)
  }
derive instance newtypeInstanceIdentity :: Newtype InstanceIdentity _


newtype Instances = Instances (Array Instance)
derive instance newtypeInstances :: Newtype Instances _


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
derive instance newtypeInstancesCount :: Newtype InstancesCount _


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
derive instance newtypeLayer :: Newtype Layer _


newtype LayerAttributes = LayerAttributes (Map LayerAttributesKeys String)
derive instance newtypeLayerAttributes :: Newtype LayerAttributes _


newtype LayerAttributesKeys = LayerAttributesKeys String
derive instance newtypeLayerAttributesKeys :: Newtype LayerAttributesKeys _


newtype LayerType = LayerType String
derive instance newtypeLayerType :: Newtype LayerType _


newtype Layers = Layers (Array Layer)
derive instance newtypeLayers :: Newtype Layers _


-- | <p>Specifies the lifecycle event configuration</p>
newtype LifecycleEventConfiguration = LifecycleEventConfiguration 
  { "Shutdown" :: NullOrUndefined (ShutdownEventConfiguration)
  }
derive instance newtypeLifecycleEventConfiguration :: Newtype LifecycleEventConfiguration _


newtype ListTagsRequest = ListTagsRequest 
  { "ResourceArn" :: (ResourceArn)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsRequest :: Newtype ListTagsRequest _


-- | <p>Contains the response to a <code>ListTags</code> request.</p>
newtype ListTagsResult = ListTagsResult 
  { "Tags" :: NullOrUndefined (Tags)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsResult :: Newtype ListTagsResult _


-- | <p>Describes a layer's load-based auto scaling configuration.</p>
newtype LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration 
  { "LayerId" :: NullOrUndefined (String)
  , "Enable" :: NullOrUndefined (Boolean)
  , "UpScaling" :: NullOrUndefined (AutoScalingThresholds)
  , "DownScaling" :: NullOrUndefined (AutoScalingThresholds)
  }
derive instance newtypeLoadBasedAutoScalingConfiguration :: Newtype LoadBasedAutoScalingConfiguration _


newtype LoadBasedAutoScalingConfigurations = LoadBasedAutoScalingConfigurations (Array LoadBasedAutoScalingConfiguration)
derive instance newtypeLoadBasedAutoScalingConfigurations :: Newtype LoadBasedAutoScalingConfigurations _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype Minute = Minute Int
derive instance newtypeMinute :: Newtype Minute _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


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
derive instance newtypeOperatingSystem :: Newtype OperatingSystem _


-- | <p>A block that contains information about the configuration manager (Chef) and the versions of the configuration manager that are supported for an operating system.</p>
newtype OperatingSystemConfigurationManager = OperatingSystemConfigurationManager 
  { "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }
derive instance newtypeOperatingSystemConfigurationManager :: Newtype OperatingSystemConfigurationManager _


newtype OperatingSystemConfigurationManagers = OperatingSystemConfigurationManagers (Array OperatingSystemConfigurationManager)
derive instance newtypeOperatingSystemConfigurationManagers :: Newtype OperatingSystemConfigurationManagers _


newtype OperatingSystems = OperatingSystems (Array OperatingSystem)
derive instance newtypeOperatingSystems :: Newtype OperatingSystems _


newtype Parameters = Parameters (Map String String)
derive instance newtypeParameters :: Newtype Parameters _


-- | <p>Describes stack or user permissions.</p>
newtype Permission = Permission 
  { "StackId" :: NullOrUndefined (String)
  , "IamUserArn" :: NullOrUndefined (String)
  , "AllowSsh" :: NullOrUndefined (Boolean)
  , "AllowSudo" :: NullOrUndefined (Boolean)
  , "Level" :: NullOrUndefined (String)
  }
derive instance newtypePermission :: Newtype Permission _


newtype Permissions = Permissions (Array Permission)
derive instance newtypePermissions :: Newtype Permissions _


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
derive instance newtypeRaidArray :: Newtype RaidArray _


newtype RaidArrays = RaidArrays (Array RaidArray)
derive instance newtypeRaidArrays :: Newtype RaidArrays _


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
derive instance newtypeRdsDbInstance :: Newtype RdsDbInstance _


newtype RdsDbInstances = RdsDbInstances (Array RdsDbInstance)
derive instance newtypeRdsDbInstances :: Newtype RdsDbInstances _


newtype RebootInstanceRequest = RebootInstanceRequest 
  { "InstanceId" :: (String)
  }
derive instance newtypeRebootInstanceRequest :: Newtype RebootInstanceRequest _


-- | <p>AWS OpsWorks Stacks supports five lifecycle events: <b>setup</b>, <b>configuration</b>, <b>deploy</b>, <b>undeploy</b>, and <b>shutdown</b>. For each layer, AWS OpsWorks Stacks runs a set of standard recipes for each event. In addition, you can provide custom recipes for any or all layers and events. AWS OpsWorks Stacks runs custom event recipes after the standard recipes. <code>LayerCustomRecipes</code> specifies the custom recipes for a particular layer to be run in response to each of the five events. </p> <p>To specify a recipe, use the cookbook's directory name in the repository followed by two colons and the recipe name, which is the recipe's file name without the .rb extension. For example: phpapp2::dbsetup specifies the dbsetup.rb recipe in the repository's phpapp2 folder.</p>
newtype Recipes = Recipes 
  { "Setup" :: NullOrUndefined (Strings)
  , "Configure" :: NullOrUndefined (Strings)
  , "Deploy" :: NullOrUndefined (Strings)
  , "Undeploy" :: NullOrUndefined (Strings)
  , "Shutdown" :: NullOrUndefined (Strings)
  }
derive instance newtypeRecipes :: Newtype Recipes _


newtype RegisterEcsClusterRequest = RegisterEcsClusterRequest 
  { "EcsClusterArn" :: (String)
  , "StackId" :: (String)
  }
derive instance newtypeRegisterEcsClusterRequest :: Newtype RegisterEcsClusterRequest _


-- | <p>Contains the response to a <code>RegisterEcsCluster</code> request.</p>
newtype RegisterEcsClusterResult = RegisterEcsClusterResult 
  { "EcsClusterArn" :: NullOrUndefined (String)
  }
derive instance newtypeRegisterEcsClusterResult :: Newtype RegisterEcsClusterResult _


newtype RegisterElasticIpRequest = RegisterElasticIpRequest 
  { "ElasticIp" :: (String)
  , "StackId" :: (String)
  }
derive instance newtypeRegisterElasticIpRequest :: Newtype RegisterElasticIpRequest _


-- | <p>Contains the response to a <code>RegisterElasticIp</code> request.</p>
newtype RegisterElasticIpResult = RegisterElasticIpResult 
  { "ElasticIp" :: NullOrUndefined (String)
  }
derive instance newtypeRegisterElasticIpResult :: Newtype RegisterElasticIpResult _


newtype RegisterInstanceRequest = RegisterInstanceRequest 
  { "StackId" :: (String)
  , "Hostname" :: NullOrUndefined (String)
  , "PublicIp" :: NullOrUndefined (String)
  , "PrivateIp" :: NullOrUndefined (String)
  , "RsaPublicKey" :: NullOrUndefined (String)
  , "RsaPublicKeyFingerprint" :: NullOrUndefined (String)
  , "InstanceIdentity" :: NullOrUndefined (InstanceIdentity)
  }
derive instance newtypeRegisterInstanceRequest :: Newtype RegisterInstanceRequest _


-- | <p>Contains the response to a <code>RegisterInstanceResult</code> request.</p>
newtype RegisterInstanceResult = RegisterInstanceResult 
  { "InstanceId" :: NullOrUndefined (String)
  }
derive instance newtypeRegisterInstanceResult :: Newtype RegisterInstanceResult _


newtype RegisterRdsDbInstanceRequest = RegisterRdsDbInstanceRequest 
  { "StackId" :: (String)
  , "RdsDbInstanceArn" :: (String)
  , "DbUser" :: (String)
  , "DbPassword" :: (String)
  }
derive instance newtypeRegisterRdsDbInstanceRequest :: Newtype RegisterRdsDbInstanceRequest _


newtype RegisterVolumeRequest = RegisterVolumeRequest 
  { "Ec2VolumeId" :: NullOrUndefined (String)
  , "StackId" :: (String)
  }
derive instance newtypeRegisterVolumeRequest :: Newtype RegisterVolumeRequest _


-- | <p>Contains the response to a <code>RegisterVolume</code> request.</p>
newtype RegisterVolumeResult = RegisterVolumeResult 
  { "VolumeId" :: NullOrUndefined (String)
  }
derive instance newtypeRegisterVolumeResult :: Newtype RegisterVolumeResult _


-- | <p>A registered instance's reported operating system.</p>
newtype ReportedOs = ReportedOs 
  { "Family" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }
derive instance newtypeReportedOs :: Newtype ReportedOs _


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _


-- | <p>Indicates that a resource was not found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype RootDeviceType = RootDeviceType String
derive instance newtypeRootDeviceType :: Newtype RootDeviceType _


-- | <p>Describes a user's SSH information.</p>
newtype SelfUserProfile = SelfUserProfile 
  { "IamUserArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SshUsername" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (String)
  }
derive instance newtypeSelfUserProfile :: Newtype SelfUserProfile _


-- | <p>Describes an AWS OpsWorks Stacks service error.</p>
newtype ServiceError = ServiceError 
  { "ServiceErrorId" :: NullOrUndefined (String)
  , "StackId" :: NullOrUndefined (String)
  , "InstanceId" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  , "Message" :: NullOrUndefined (String)
  , "CreatedAt" :: NullOrUndefined (DateTime)
  }
derive instance newtypeServiceError :: Newtype ServiceError _


newtype ServiceErrors = ServiceErrors (Array ServiceError)
derive instance newtypeServiceErrors :: Newtype ServiceErrors _


newtype SetLoadBasedAutoScalingRequest = SetLoadBasedAutoScalingRequest 
  { "LayerId" :: (String)
  , "Enable" :: NullOrUndefined (Boolean)
  , "UpScaling" :: NullOrUndefined (AutoScalingThresholds)
  , "DownScaling" :: NullOrUndefined (AutoScalingThresholds)
  }
derive instance newtypeSetLoadBasedAutoScalingRequest :: Newtype SetLoadBasedAutoScalingRequest _


newtype SetPermissionRequest = SetPermissionRequest 
  { "StackId" :: (String)
  , "IamUserArn" :: (String)
  , "AllowSsh" :: NullOrUndefined (Boolean)
  , "AllowSudo" :: NullOrUndefined (Boolean)
  , "Level" :: NullOrUndefined (String)
  }
derive instance newtypeSetPermissionRequest :: Newtype SetPermissionRequest _


newtype SetTimeBasedAutoScalingRequest = SetTimeBasedAutoScalingRequest 
  { "InstanceId" :: (String)
  , "AutoScalingSchedule" :: NullOrUndefined (WeeklyAutoScalingSchedule)
  }
derive instance newtypeSetTimeBasedAutoScalingRequest :: Newtype SetTimeBasedAutoScalingRequest _


-- | <p>The Shutdown event configuration.</p>
newtype ShutdownEventConfiguration = ShutdownEventConfiguration 
  { "ExecutionTimeout" :: NullOrUndefined (Int)
  , "DelayUntilElbConnectionsDrained" :: NullOrUndefined (Boolean)
  }
derive instance newtypeShutdownEventConfiguration :: Newtype ShutdownEventConfiguration _


-- | <p>Contains the information required to retrieve an app or cookbook from a repository. For more information, see <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingapps-creating.html">Creating Apps</a> or <a href="http://docs.aws.amazon.com/opsworks/latest/userguide/workingcookbook.html">Custom Recipes and Cookbooks</a>.</p>
newtype Source = Source 
  { "Type" :: NullOrUndefined (SourceType)
  , "Url" :: NullOrUndefined (String)
  , "Username" :: NullOrUndefined (String)
  , "Password" :: NullOrUndefined (String)
  , "SshKey" :: NullOrUndefined (String)
  , "Revision" :: NullOrUndefined (String)
  }
derive instance newtypeSource :: Newtype Source _


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _


-- | <p>Describes an app's SSL configuration.</p>
newtype SslConfiguration = SslConfiguration 
  { "Certificate" :: (String)
  , "PrivateKey" :: (String)
  , "Chain" :: NullOrUndefined (String)
  }
derive instance newtypeSslConfiguration :: Newtype SslConfiguration _


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
derive instance newtypeStack :: Newtype Stack _


newtype StackAttributes = StackAttributes (Map StackAttributesKeys String)
derive instance newtypeStackAttributes :: Newtype StackAttributes _


newtype StackAttributesKeys = StackAttributesKeys String
derive instance newtypeStackAttributesKeys :: Newtype StackAttributesKeys _


-- | <p>Describes the configuration manager.</p>
newtype StackConfigurationManager = StackConfigurationManager 
  { "Name" :: NullOrUndefined (String)
  , "Version" :: NullOrUndefined (String)
  }
derive instance newtypeStackConfigurationManager :: Newtype StackConfigurationManager _


-- | <p>Summarizes the number of layers, instances, and apps in a stack.</p>
newtype StackSummary = StackSummary 
  { "StackId" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "Arn" :: NullOrUndefined (String)
  , "LayersCount" :: NullOrUndefined (Int)
  , "AppsCount" :: NullOrUndefined (Int)
  , "InstancesCount" :: NullOrUndefined (InstancesCount)
  }
derive instance newtypeStackSummary :: Newtype StackSummary _


newtype Stacks = Stacks (Array Stack)
derive instance newtypeStacks :: Newtype Stacks _


newtype StartInstanceRequest = StartInstanceRequest 
  { "InstanceId" :: (String)
  }
derive instance newtypeStartInstanceRequest :: Newtype StartInstanceRequest _


newtype StartStackRequest = StartStackRequest 
  { "StackId" :: (String)
  }
derive instance newtypeStartStackRequest :: Newtype StartStackRequest _


newtype StopInstanceRequest = StopInstanceRequest 
  { "InstanceId" :: (String)
  , "Force" :: NullOrUndefined (Boolean)
  }
derive instance newtypeStopInstanceRequest :: Newtype StopInstanceRequest _


newtype StopStackRequest = StopStackRequest 
  { "StackId" :: (String)
  }
derive instance newtypeStopStackRequest :: Newtype StopStackRequest _


newtype Strings = Strings (Array String)
derive instance newtypeStrings :: Newtype Strings _


newtype Switch = Switch String
derive instance newtypeSwitch :: Newtype Switch _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn" :: (ResourceArn)
  , "Tags" :: (Tags)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Map TagKey TagValue)
derive instance newtypeTags :: Newtype Tags _


-- | <p>Contains the data needed by RDP clients such as the Microsoft Remote Desktop Connection to log in to the instance.</p>
newtype TemporaryCredential = TemporaryCredential 
  { "Username" :: NullOrUndefined (String)
  , "Password" :: NullOrUndefined (String)
  , "ValidForInMinutes" :: NullOrUndefined (Int)
  , "InstanceId" :: NullOrUndefined (String)
  }
derive instance newtypeTemporaryCredential :: Newtype TemporaryCredential _


-- | <p>Describes an instance's time-based auto scaling configuration.</p>
newtype TimeBasedAutoScalingConfiguration = TimeBasedAutoScalingConfiguration 
  { "InstanceId" :: NullOrUndefined (String)
  , "AutoScalingSchedule" :: NullOrUndefined (WeeklyAutoScalingSchedule)
  }
derive instance newtypeTimeBasedAutoScalingConfiguration :: Newtype TimeBasedAutoScalingConfiguration _


newtype TimeBasedAutoScalingConfigurations = TimeBasedAutoScalingConfigurations (Array TimeBasedAutoScalingConfiguration)
derive instance newtypeTimeBasedAutoScalingConfigurations :: Newtype TimeBasedAutoScalingConfigurations _


newtype UnassignInstanceRequest = UnassignInstanceRequest 
  { "InstanceId" :: (String)
  }
derive instance newtypeUnassignInstanceRequest :: Newtype UnassignInstanceRequest _


newtype UnassignVolumeRequest = UnassignVolumeRequest 
  { "VolumeId" :: (String)
  }
derive instance newtypeUnassignVolumeRequest :: Newtype UnassignVolumeRequest _


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn" :: (ResourceArn)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _


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
derive instance newtypeUpdateAppRequest :: Newtype UpdateAppRequest _


newtype UpdateElasticIpRequest = UpdateElasticIpRequest 
  { "ElasticIp" :: (String)
  , "Name" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateElasticIpRequest :: Newtype UpdateElasticIpRequest _


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
derive instance newtypeUpdateInstanceRequest :: Newtype UpdateInstanceRequest _


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
derive instance newtypeUpdateLayerRequest :: Newtype UpdateLayerRequest _


newtype UpdateMyUserProfileRequest = UpdateMyUserProfileRequest 
  { "SshPublicKey" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateMyUserProfileRequest :: Newtype UpdateMyUserProfileRequest _


newtype UpdateRdsDbInstanceRequest = UpdateRdsDbInstanceRequest 
  { "RdsDbInstanceArn" :: (String)
  , "DbUser" :: NullOrUndefined (String)
  , "DbPassword" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateRdsDbInstanceRequest :: Newtype UpdateRdsDbInstanceRequest _


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
derive instance newtypeUpdateStackRequest :: Newtype UpdateStackRequest _


newtype UpdateUserProfileRequest = UpdateUserProfileRequest 
  { "IamUserArn" :: (String)
  , "SshUsername" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (String)
  , "AllowSelfManagement" :: NullOrUndefined (Boolean)
  }
derive instance newtypeUpdateUserProfileRequest :: Newtype UpdateUserProfileRequest _


newtype UpdateVolumeRequest = UpdateVolumeRequest 
  { "VolumeId" :: (String)
  , "Name" :: NullOrUndefined (String)
  , "MountPoint" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateVolumeRequest :: Newtype UpdateVolumeRequest _


-- | <p>Describes a user's SSH information.</p>
newtype UserProfile = UserProfile 
  { "IamUserArn" :: NullOrUndefined (String)
  , "Name" :: NullOrUndefined (String)
  , "SshUsername" :: NullOrUndefined (String)
  , "SshPublicKey" :: NullOrUndefined (String)
  , "AllowSelfManagement" :: NullOrUndefined (Boolean)
  }
derive instance newtypeUserProfile :: Newtype UserProfile _


newtype UserProfiles = UserProfiles (Array UserProfile)
derive instance newtypeUserProfiles :: Newtype UserProfiles _


newtype ValidForInMinutes = ValidForInMinutes Int
derive instance newtypeValidForInMinutes :: Newtype ValidForInMinutes _


-- | <p>Indicates that a request was not valid.</p>
newtype ValidationException = ValidationException 
  { "Message'" :: NullOrUndefined (String)
  }
derive instance newtypeValidationException :: Newtype ValidationException _


newtype VirtualizationType = VirtualizationType String
derive instance newtypeVirtualizationType :: Newtype VirtualizationType _


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
derive instance newtypeVolume :: Newtype Volume _


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
derive instance newtypeVolumeConfiguration :: Newtype VolumeConfiguration _


newtype VolumeConfigurations = VolumeConfigurations (Array VolumeConfiguration)
derive instance newtypeVolumeConfigurations :: Newtype VolumeConfigurations _


newtype VolumeType = VolumeType String
derive instance newtypeVolumeType :: Newtype VolumeType _


newtype Volumes = Volumes (Array Volume)
derive instance newtypeVolumes :: Newtype Volumes _


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
derive instance newtypeWeeklyAutoScalingSchedule :: Newtype WeeklyAutoScalingSchedule _
