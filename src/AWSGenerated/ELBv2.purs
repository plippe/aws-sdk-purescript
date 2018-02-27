

-- | <fullname>Elastic Load Balancing</fullname> <p>A load balancer distributes incoming traffic across targets, such as your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered targets and ensures that it routes traffic only to healthy targets. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer. You configure a target group with a protocol and port number for connections from the load balancer to the targets, and with health check settings to be used when checking the health status of the targets.</p> <p>Elastic Load Balancing supports the following types of load balancers: Application Load Balancers, Network Load Balancers, and Classic Load Balancers.</p> <p>An Application Load Balancer makes routing and load balancing decisions at the application layer (HTTP/HTTPS). A Network Load Balancer makes routing and load balancing decisions at the transport layer (TCP). Both Application Load Balancers and Network Load Balancers can route requests to one or more ports on each EC2 instance or container instance in your virtual private cloud (VPC).</p> <p>A Classic Load Balancer makes routing and load balancing decisions either at the transport layer (TCP/SSL) or the application layer (HTTP/HTTPS), and supports either EC2-Classic or a VPC. For more information, see the <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/">Elastic Load Balancing User Guide</a>.</p> <p>This reference covers the 2015-12-01 API, which supports Application Load Balancers and Network Load Balancers. The 2012-06-01 API supports Classic Load Balancers.</p> <p>To get started, complete the following tasks:</p> <ol> <li> <p>Create a load balancer using <a>CreateLoadBalancer</a>.</p> </li> <li> <p>Create a target group using <a>CreateTargetGroup</a>.</p> </li> <li> <p>Register targets for the target group using <a>RegisterTargets</a>.</p> </li> <li> <p>Create one or more listeners for your load balancer using <a>CreateListener</a>.</p> </li> </ol> <p>To delete a load balancer and its related resources, complete the following tasks:</p> <ol> <li> <p>Delete the load balancer using <a>DeleteLoadBalancer</a>.</p> </li> <li> <p>Delete the target group using <a>DeleteTargetGroup</a>.</p> </li> </ol> <p>All Elastic Load Balancing operations are idempotent, which means that they complete at most one time. If you repeat an operation, it succeeds.</p>
module AWS.ELBv2 where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ELBv2" :: String


-- | <p>Adds the specified certificate to the specified secure listener.</p> <p>If the certificate was already added, the call is successful but the certificate is not added again.</p> <p>To list the certificates for your listener, use <a>DescribeListenerCertificates</a>. To remove certificates from your listener, use <a>RemoveListenerCertificates</a>.</p>
addListenerCertificates :: forall eff. AddListenerCertificatesInput -> Aff (err :: AWS.RequestError | eff) AddListenerCertificatesOutput
addListenerCertificates = AWS.request serviceName "AddListenerCertificates" 


-- | <p>Adds the specified tags to the specified Elastic Load Balancing resource. You can tag your Application Load Balancers, Network Load Balancers, and your target groups.</p> <p>Each tag consists of a key and an optional value. If a resource already has a tag with the same key, <code>AddTags</code> updates its value.</p> <p>To list the current tags for your resources, use <a>DescribeTags</a>. To remove tags from your resources, use <a>RemoveTags</a>.</p>
addTags :: forall eff. AddTagsInput -> Aff (err :: AWS.RequestError | eff) AddTagsOutput
addTags = AWS.request serviceName "AddTags" 


-- | <p>Creates a listener for the specified Application Load Balancer or Network Load Balancer.</p> <p>To update a listener, use <a>ModifyListener</a>. When you are finished with a listener, you can delete it using <a>DeleteListener</a>. If you are finished with both the listener and the load balancer, you can delete them both using <a>DeleteLoadBalancer</a>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple listeners with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html">Listeners for Your Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-listeners.html">Listeners for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>
createListener :: forall eff. CreateListenerInput -> Aff (err :: AWS.RequestError | eff) CreateListenerOutput
createListener = AWS.request serviceName "CreateListener" 


-- | <p>Creates an Application Load Balancer or a Network Load Balancer.</p> <p>When you create a load balancer, you can specify security groups, public subnets, IP address type, and tags. Otherwise, you could do so later using <a>SetSecurityGroups</a>, <a>SetSubnets</a>, <a>SetIpAddressType</a>, and <a>AddTags</a>.</p> <p>To create listeners for your load balancer, use <a>CreateListener</a>. To describe your current load balancers, see <a>DescribeLoadBalancers</a>. When you are finished with a load balancer, you can delete it using <a>DeleteLoadBalancer</a>.</p> <p>For limit information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-limits.html">Limits for Your Application Load Balancer</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-limits.html">Limits for Your Network Load Balancer</a> in the <i>Network Load Balancers Guide</i>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple load balancers with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/application-load-balancers.html">Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/network-load-balancers.html">Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>
createLoadBalancer :: forall eff. CreateLoadBalancerInput -> Aff (err :: AWS.RequestError | eff) CreateLoadBalancerOutput
createLoadBalancer = AWS.request serviceName "CreateLoadBalancer" 


-- | <p>Creates a rule for the specified listener. The listener must be associated with an Application Load Balancer.</p> <p>Rules are evaluated in priority order, from the lowest value to the highest value. When the condition for a rule is met, the specified action is taken. If no conditions are met, the action for the default rule is taken. For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html#listener-rules">Listener Rules</a> in the <i>Application Load Balancers Guide</i>.</p> <p>To view your current rules, use <a>DescribeRules</a>. To update a rule, use <a>ModifyRule</a>. To set the priorities of your rules, use <a>SetRulePriorities</a>. To delete a rule, use <a>DeleteRule</a>.</p>
createRule :: forall eff. CreateRuleInput -> Aff (err :: AWS.RequestError | eff) CreateRuleOutput
createRule = AWS.request serviceName "CreateRule" 


-- | <p>Creates a target group.</p> <p>To register targets with the target group, use <a>RegisterTargets</a>. To update the health check settings for the target group, use <a>ModifyTargetGroup</a>. To monitor the health of targets in the target group, use <a>DescribeTargetHealth</a>.</p> <p>To route traffic to the targets in a target group, specify the target group in an action using <a>CreateListener</a> or <a>CreateRule</a>.</p> <p>To delete a target group, use <a>DeleteTargetGroup</a>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple target groups with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html">Target Groups for Your Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> or <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html">Target Groups for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>
createTargetGroup :: forall eff. CreateTargetGroupInput -> Aff (err :: AWS.RequestError | eff) CreateTargetGroupOutput
createTargetGroup = AWS.request serviceName "CreateTargetGroup" 


-- | <p>Deletes the specified listener.</p> <p>Alternatively, your listener is deleted when you delete the load balancer it is attached to using <a>DeleteLoadBalancer</a>.</p>
deleteListener :: forall eff. DeleteListenerInput -> Aff (err :: AWS.RequestError | eff) DeleteListenerOutput
deleteListener = AWS.request serviceName "DeleteListener" 


-- | <p>Deletes the specified Application Load Balancer or Network Load Balancer and its attached listeners.</p> <p>You can't delete a load balancer if deletion protection is enabled. If the load balancer does not exist or has already been deleted, the call succeeds.</p> <p>Deleting a load balancer does not affect its registered targets. For example, your EC2 instances continue to run and are still registered to their target groups. If you no longer need these EC2 instances, you can stop or terminate them.</p>
deleteLoadBalancer :: forall eff. DeleteLoadBalancerInput -> Aff (err :: AWS.RequestError | eff) DeleteLoadBalancerOutput
deleteLoadBalancer = AWS.request serviceName "DeleteLoadBalancer" 


-- | <p>Deletes the specified rule.</p>
deleteRule :: forall eff. DeleteRuleInput -> Aff (err :: AWS.RequestError | eff) DeleteRuleOutput
deleteRule = AWS.request serviceName "DeleteRule" 


-- | <p>Deletes the specified target group.</p> <p>You can delete a target group if it is not referenced by any actions. Deleting a target group also deletes any associated health checks.</p>
deleteTargetGroup :: forall eff. DeleteTargetGroupInput -> Aff (err :: AWS.RequestError | eff) DeleteTargetGroupOutput
deleteTargetGroup = AWS.request serviceName "DeleteTargetGroup" 


-- | <p>Deregisters the specified targets from the specified target group. After the targets are deregistered, they no longer receive traffic from the load balancer.</p>
deregisterTargets :: forall eff. DeregisterTargetsInput -> Aff (err :: AWS.RequestError | eff) DeregisterTargetsOutput
deregisterTargets = AWS.request serviceName "DeregisterTargets" 


-- | <p>Describes the current Elastic Load Balancing resource limits for your AWS account.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-limits.html">Limits for Your Application Load Balancers</a> in the <i>Application Load Balancer Guide</i> or <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-limits.html">Limits for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>
describeAccountLimits :: forall eff. DescribeAccountLimitsInput -> Aff (err :: AWS.RequestError | eff) DescribeAccountLimitsOutput
describeAccountLimits = AWS.request serviceName "DescribeAccountLimits" 


-- | <p>Describes the certificates for the specified secure listener.</p>
describeListenerCertificates :: forall eff. DescribeListenerCertificatesInput -> Aff (err :: AWS.RequestError | eff) DescribeListenerCertificatesOutput
describeListenerCertificates = AWS.request serviceName "DescribeListenerCertificates" 


-- | <p>Describes the specified listeners or the listeners for the specified Application Load Balancer or Network Load Balancer. You must specify either a load balancer or one or more listeners.</p>
describeListeners :: forall eff. DescribeListenersInput -> Aff (err :: AWS.RequestError | eff) DescribeListenersOutput
describeListeners = AWS.request serviceName "DescribeListeners" 


-- | <p>Describes the attributes for the specified Application Load Balancer or Network Load Balancer.</p>
describeLoadBalancerAttributes :: forall eff. DescribeLoadBalancerAttributesInput -> Aff (err :: AWS.RequestError | eff) DescribeLoadBalancerAttributesOutput
describeLoadBalancerAttributes = AWS.request serviceName "DescribeLoadBalancerAttributes" 


-- | <p>Describes the specified load balancers or all of your load balancers.</p> <p>To describe the listeners for a load balancer, use <a>DescribeListeners</a>. To describe the attributes for a load balancer, use <a>DescribeLoadBalancerAttributes</a>.</p>
describeLoadBalancers :: forall eff. DescribeLoadBalancersInput -> Aff (err :: AWS.RequestError | eff) DescribeLoadBalancersOutput
describeLoadBalancers = AWS.request serviceName "DescribeLoadBalancers" 


-- | <p>Describes the specified rules or the rules for the specified listener. You must specify either a listener or one or more rules.</p>
describeRules :: forall eff. DescribeRulesInput -> Aff (err :: AWS.RequestError | eff) DescribeRulesOutput
describeRules = AWS.request serviceName "DescribeRules" 


-- | <p>Describes the specified policies or all policies used for SSL negotiation.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies">Security Policies</a> in the <i>Application Load Balancers Guide</i>.</p>
describeSSLPolicies :: forall eff. DescribeSSLPoliciesInput -> Aff (err :: AWS.RequestError | eff) DescribeSSLPoliciesOutput
describeSSLPolicies = AWS.request serviceName "DescribeSSLPolicies" 


-- | <p>Describes the tags for the specified resources. You can describe the tags for one or more Application Load Balancers, Network Load Balancers, and target groups.</p>
describeTags :: forall eff. DescribeTagsInput -> Aff (err :: AWS.RequestError | eff) DescribeTagsOutput
describeTags = AWS.request serviceName "DescribeTags" 


-- | <p>Describes the attributes for the specified target group.</p>
describeTargetGroupAttributes :: forall eff. DescribeTargetGroupAttributesInput -> Aff (err :: AWS.RequestError | eff) DescribeTargetGroupAttributesOutput
describeTargetGroupAttributes = AWS.request serviceName "DescribeTargetGroupAttributes" 


-- | <p>Describes the specified target groups or all of your target groups. By default, all target groups are described. Alternatively, you can specify one of the following to filter the results: the ARN of the load balancer, the names of one or more target groups, or the ARNs of one or more target groups.</p> <p>To describe the targets for a target group, use <a>DescribeTargetHealth</a>. To describe the attributes of a target group, use <a>DescribeTargetGroupAttributes</a>.</p>
describeTargetGroups :: forall eff. DescribeTargetGroupsInput -> Aff (err :: AWS.RequestError | eff) DescribeTargetGroupsOutput
describeTargetGroups = AWS.request serviceName "DescribeTargetGroups" 


-- | <p>Describes the health of the specified targets or all of your targets.</p>
describeTargetHealth :: forall eff. DescribeTargetHealthInput -> Aff (err :: AWS.RequestError | eff) DescribeTargetHealthOutput
describeTargetHealth = AWS.request serviceName "DescribeTargetHealth" 


-- | <p>Modifies the specified properties of the specified listener.</p> <p>Any properties that you do not specify retain their current values. However, changing the protocol from HTTPS to HTTP removes the security policy and SSL certificate properties. If you change the protocol from HTTP to HTTPS, you must add the security policy and server certificate.</p>
modifyListener :: forall eff. ModifyListenerInput -> Aff (err :: AWS.RequestError | eff) ModifyListenerOutput
modifyListener = AWS.request serviceName "ModifyListener" 


-- | <p>Modifies the specified attributes of the specified Application Load Balancer or Network Load Balancer.</p> <p>If any of the specified attributes can't be modified as requested, the call fails. Any existing attributes that you do not modify retain their current values.</p>
modifyLoadBalancerAttributes :: forall eff. ModifyLoadBalancerAttributesInput -> Aff (err :: AWS.RequestError | eff) ModifyLoadBalancerAttributesOutput
modifyLoadBalancerAttributes = AWS.request serviceName "ModifyLoadBalancerAttributes" 


-- | <p>Modifies the specified rule.</p> <p>Any existing properties that you do not modify retain their current values.</p> <p>To modify the default action, use <a>ModifyListener</a>.</p>
modifyRule :: forall eff. ModifyRuleInput -> Aff (err :: AWS.RequestError | eff) ModifyRuleOutput
modifyRule = AWS.request serviceName "ModifyRule" 


-- | <p>Modifies the health checks used when evaluating the health state of the targets in the specified target group.</p> <p>To monitor the health of the targets, use <a>DescribeTargetHealth</a>.</p>
modifyTargetGroup :: forall eff. ModifyTargetGroupInput -> Aff (err :: AWS.RequestError | eff) ModifyTargetGroupOutput
modifyTargetGroup = AWS.request serviceName "ModifyTargetGroup" 


-- | <p>Modifies the specified attributes of the specified target group.</p>
modifyTargetGroupAttributes :: forall eff. ModifyTargetGroupAttributesInput -> Aff (err :: AWS.RequestError | eff) ModifyTargetGroupAttributesOutput
modifyTargetGroupAttributes = AWS.request serviceName "ModifyTargetGroupAttributes" 


-- | <p>Registers the specified targets with the specified target group.</p> <p>You can register targets by instance ID or by IP address. If the target is an EC2 instance, it must be in the <code>running</code> state when you register it.</p> <p>By default, the load balancer routes requests to registered targets using the protocol and port for the target group. Alternatively, you can override the port for a target when you register it. You can register each EC2 instance or IP address with the same target group multiple times using different ports.</p> <p>With a Network Load Balancer, you cannot register instances by instance ID if they have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1. You can register instances of these types by IP address.</p> <p>To remove a target from a target group, use <a>DeregisterTargets</a>.</p>
registerTargets :: forall eff. RegisterTargetsInput -> Aff (err :: AWS.RequestError | eff) RegisterTargetsOutput
registerTargets = AWS.request serviceName "RegisterTargets" 


-- | <p>Removes the specified certificate from the specified secure listener.</p> <p>You can't remove the default certificate for a listener. To replace the default certificate, call <a>ModifyListener</a>.</p> <p>To list the certificates for your listener, use <a>DescribeListenerCertificates</a>.</p>
removeListenerCertificates :: forall eff. RemoveListenerCertificatesInput -> Aff (err :: AWS.RequestError | eff) RemoveListenerCertificatesOutput
removeListenerCertificates = AWS.request serviceName "RemoveListenerCertificates" 


-- | <p>Removes the specified tags from the specified Elastic Load Balancing resource.</p> <p>To list the current tags for your resources, use <a>DescribeTags</a>.</p>
removeTags :: forall eff. RemoveTagsInput -> Aff (err :: AWS.RequestError | eff) RemoveTagsOutput
removeTags = AWS.request serviceName "RemoveTags" 


-- | <p>Sets the type of IP addresses used by the subnets of the specified Application Load Balancer or Network Load Balancer.</p> <p>Note that Network Load Balancers must use <code>ipv4</code>.</p>
setIpAddressType :: forall eff. SetIpAddressTypeInput -> Aff (err :: AWS.RequestError | eff) SetIpAddressTypeOutput
setIpAddressType = AWS.request serviceName "SetIpAddressType" 


-- | <p>Sets the priorities of the specified rules.</p> <p>You can reorder the rules as long as there are no priority conflicts in the new order. Any existing rules that you do not specify retain their current priority.</p>
setRulePriorities :: forall eff. SetRulePrioritiesInput -> Aff (err :: AWS.RequestError | eff) SetRulePrioritiesOutput
setRulePriorities = AWS.request serviceName "SetRulePriorities" 


-- | <p>Associates the specified security groups with the specified Application Load Balancer. The specified security groups override the previously associated security groups.</p> <p>Note that you can't specify a security group for a Network Load Balancer.</p>
setSecurityGroups :: forall eff. SetSecurityGroupsInput -> Aff (err :: AWS.RequestError | eff) SetSecurityGroupsOutput
setSecurityGroups = AWS.request serviceName "SetSecurityGroups" 


-- | <p>Enables the Availability Zone for the specified public subnets for the specified Application Load Balancer. The specified subnets replace the previously enabled subnets.</p> <p>Note that you can't change the subnets for a Network Load Balancer.</p>
setSubnets :: forall eff. SetSubnetsInput -> Aff (err :: AWS.RequestError | eff) SetSubnetsOutput
setSubnets = AWS.request serviceName "SetSubnets" 


-- | <p>Information about an action.</p>
newtype Action = Action 
  { "Type" :: (ActionTypeEnum)
  , "TargetGroupArn" :: (TargetGroupArn)
  }
derive instance newtypeAction :: Newtype Action _


newtype ActionTypeEnum = ActionTypeEnum String
derive instance newtypeActionTypeEnum :: Newtype ActionTypeEnum _


newtype Actions = Actions (Array Action)
derive instance newtypeActions :: Newtype Actions _


newtype AddListenerCertificatesInput = AddListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Certificates" :: (CertificateList)
  }
derive instance newtypeAddListenerCertificatesInput :: Newtype AddListenerCertificatesInput _


newtype AddListenerCertificatesOutput = AddListenerCertificatesOutput 
  { "Certificates" :: NullOrUndefined (CertificateList)
  }
derive instance newtypeAddListenerCertificatesOutput :: Newtype AddListenerCertificatesOutput _


newtype AddTagsInput = AddTagsInput 
  { "ResourceArns" :: (ResourceArns)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsInput :: Newtype AddTagsInput _


newtype AddTagsOutput = AddTagsOutput 
  { 
  }
derive instance newtypeAddTagsOutput :: Newtype AddTagsOutput _


newtype AllocationId = AllocationId String
derive instance newtypeAllocationId :: Newtype AllocationId _


-- | <p>The specified allocation ID does not exist.</p>
newtype AllocationIdNotFoundException = AllocationIdNotFoundException 
  { 
  }
derive instance newtypeAllocationIdNotFoundException :: Newtype AllocationIdNotFoundException _


-- | <p>Information about an Availability Zone.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "ZoneName" :: NullOrUndefined (ZoneName)
  , "SubnetId" :: NullOrUndefined (SubnetId)
  , "LoadBalancerAddresses" :: NullOrUndefined (LoadBalancerAddresses)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _


-- | <p>The specified Availability Zone is not supported.</p>
newtype AvailabilityZoneNotSupportedException = AvailabilityZoneNotSupportedException 
  { 
  }
derive instance newtypeAvailabilityZoneNotSupportedException :: Newtype AvailabilityZoneNotSupportedException _


newtype AvailabilityZones = AvailabilityZones (Array AvailabilityZone)
derive instance newtypeAvailabilityZones :: Newtype AvailabilityZones _


newtype CanonicalHostedZoneId = CanonicalHostedZoneId String
derive instance newtypeCanonicalHostedZoneId :: Newtype CanonicalHostedZoneId _


-- | <p>Information about an SSL server certificate.</p>
newtype Certificate = Certificate 
  { "CertificateArn" :: NullOrUndefined (CertificateArn)
  , "IsDefault" :: NullOrUndefined (Default)
  }
derive instance newtypeCertificate :: Newtype Certificate _


newtype CertificateArn = CertificateArn String
derive instance newtypeCertificateArn :: Newtype CertificateArn _


newtype CertificateList = CertificateList (Array Certificate)
derive instance newtypeCertificateList :: Newtype CertificateList _


-- | <p>The specified certificate does not exist.</p>
newtype CertificateNotFoundException = CertificateNotFoundException 
  { 
  }
derive instance newtypeCertificateNotFoundException :: Newtype CertificateNotFoundException _


-- | <p>Information about a cipher used in a policy.</p>
newtype Cipher = Cipher 
  { "Name" :: NullOrUndefined (CipherName)
  , "Priority" :: NullOrUndefined (CipherPriority)
  }
derive instance newtypeCipher :: Newtype Cipher _


newtype CipherName = CipherName String
derive instance newtypeCipherName :: Newtype CipherName _


newtype CipherPriority = CipherPriority Int
derive instance newtypeCipherPriority :: Newtype CipherPriority _


newtype Ciphers = Ciphers (Array Cipher)
derive instance newtypeCiphers :: Newtype Ciphers _


newtype ConditionFieldName = ConditionFieldName String
derive instance newtypeConditionFieldName :: Newtype ConditionFieldName _


newtype CreateListenerInput = CreateListenerInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Protocol" :: (ProtocolEnum)
  , "Port" :: (Port)
  , "SslPolicy" :: NullOrUndefined (SslPolicyName)
  , "Certificates" :: NullOrUndefined (CertificateList)
  , "DefaultActions" :: (Actions)
  }
derive instance newtypeCreateListenerInput :: Newtype CreateListenerInput _


newtype CreateListenerOutput = CreateListenerOutput 
  { "Listeners" :: NullOrUndefined (Listeners)
  }
derive instance newtypeCreateListenerOutput :: Newtype CreateListenerOutput _


newtype CreateLoadBalancerInput = CreateLoadBalancerInput 
  { "Name" :: (LoadBalancerName)
  , "Subnets" :: NullOrUndefined (Subnets)
  , "SubnetMappings" :: NullOrUndefined (SubnetMappings)
  , "SecurityGroups" :: NullOrUndefined (SecurityGroups)
  , "Scheme" :: NullOrUndefined (LoadBalancerSchemeEnum)
  , "Tags" :: NullOrUndefined (TagList)
  , "Type" :: NullOrUndefined (LoadBalancerTypeEnum)
  , "IpAddressType" :: NullOrUndefined (IpAddressType)
  }
derive instance newtypeCreateLoadBalancerInput :: Newtype CreateLoadBalancerInput _


newtype CreateLoadBalancerOutput = CreateLoadBalancerOutput 
  { "LoadBalancers" :: NullOrUndefined (LoadBalancers)
  }
derive instance newtypeCreateLoadBalancerOutput :: Newtype CreateLoadBalancerOutput _


newtype CreateRuleInput = CreateRuleInput 
  { "ListenerArn" :: (ListenerArn)
  , "Conditions" :: (RuleConditionList)
  , "Priority" :: (RulePriority)
  , "Actions" :: (Actions)
  }
derive instance newtypeCreateRuleInput :: Newtype CreateRuleInput _


newtype CreateRuleOutput = CreateRuleOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }
derive instance newtypeCreateRuleOutput :: Newtype CreateRuleOutput _


newtype CreateTargetGroupInput = CreateTargetGroupInput 
  { "Name" :: (TargetGroupName)
  , "Protocol" :: (ProtocolEnum)
  , "Port" :: (Port)
  , "VpcId" :: (VpcId)
  , "HealthCheckProtocol" :: NullOrUndefined (ProtocolEnum)
  , "HealthCheckPort" :: NullOrUndefined (HealthCheckPort)
  , "HealthCheckPath" :: NullOrUndefined (Path)
  , "HealthCheckIntervalSeconds" :: NullOrUndefined (HealthCheckIntervalSeconds)
  , "HealthCheckTimeoutSeconds" :: NullOrUndefined (HealthCheckTimeoutSeconds)
  , "HealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount)
  , "UnhealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount)
  , "Matcher" :: NullOrUndefined (Matcher)
  , "TargetType" :: NullOrUndefined (TargetTypeEnum)
  }
derive instance newtypeCreateTargetGroupInput :: Newtype CreateTargetGroupInput _


newtype CreateTargetGroupOutput = CreateTargetGroupOutput 
  { "TargetGroups" :: NullOrUndefined (TargetGroups)
  }
derive instance newtypeCreateTargetGroupOutput :: Newtype CreateTargetGroupOutput _


newtype CreatedTime = CreatedTime Number
derive instance newtypeCreatedTime :: Newtype CreatedTime _


newtype DNSName = DNSName String
derive instance newtypeDNSName :: Newtype DNSName _


newtype Default = Default Boolean
derive instance newtypeDefault :: Newtype Default _


newtype DeleteListenerInput = DeleteListenerInput 
  { "ListenerArn" :: (ListenerArn)
  }
derive instance newtypeDeleteListenerInput :: Newtype DeleteListenerInput _


newtype DeleteListenerOutput = DeleteListenerOutput 
  { 
  }
derive instance newtypeDeleteListenerOutput :: Newtype DeleteListenerOutput _


newtype DeleteLoadBalancerInput = DeleteLoadBalancerInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  }
derive instance newtypeDeleteLoadBalancerInput :: Newtype DeleteLoadBalancerInput _


newtype DeleteLoadBalancerOutput = DeleteLoadBalancerOutput 
  { 
  }
derive instance newtypeDeleteLoadBalancerOutput :: Newtype DeleteLoadBalancerOutput _


newtype DeleteRuleInput = DeleteRuleInput 
  { "RuleArn" :: (RuleArn)
  }
derive instance newtypeDeleteRuleInput :: Newtype DeleteRuleInput _


newtype DeleteRuleOutput = DeleteRuleOutput 
  { 
  }
derive instance newtypeDeleteRuleOutput :: Newtype DeleteRuleOutput _


newtype DeleteTargetGroupInput = DeleteTargetGroupInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  }
derive instance newtypeDeleteTargetGroupInput :: Newtype DeleteTargetGroupInput _


newtype DeleteTargetGroupOutput = DeleteTargetGroupOutput 
  { 
  }
derive instance newtypeDeleteTargetGroupOutput :: Newtype DeleteTargetGroupOutput _


newtype DeregisterTargetsInput = DeregisterTargetsInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: (TargetDescriptions)
  }
derive instance newtypeDeregisterTargetsInput :: Newtype DeregisterTargetsInput _


newtype DeregisterTargetsOutput = DeregisterTargetsOutput 
  { 
  }
derive instance newtypeDeregisterTargetsOutput :: Newtype DeregisterTargetsOutput _


newtype DescribeAccountLimitsInput = DescribeAccountLimitsInput 
  { "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeAccountLimitsInput :: Newtype DescribeAccountLimitsInput _


newtype DescribeAccountLimitsOutput = DescribeAccountLimitsOutput 
  { "Limits" :: NullOrUndefined (Limits)
  , "NextMarker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeAccountLimitsOutput :: Newtype DescribeAccountLimitsOutput _


newtype DescribeListenerCertificatesInput = DescribeListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeListenerCertificatesInput :: Newtype DescribeListenerCertificatesInput _


newtype DescribeListenerCertificatesOutput = DescribeListenerCertificatesOutput 
  { "Certificates" :: NullOrUndefined (CertificateList)
  , "NextMarker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeListenerCertificatesOutput :: Newtype DescribeListenerCertificatesOutput _


newtype DescribeListenersInput = DescribeListenersInput 
  { "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn)
  , "ListenerArns" :: NullOrUndefined (ListenerArns)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeListenersInput :: Newtype DescribeListenersInput _


newtype DescribeListenersOutput = DescribeListenersOutput 
  { "Listeners" :: NullOrUndefined (Listeners)
  , "NextMarker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeListenersOutput :: Newtype DescribeListenersOutput _


newtype DescribeLoadBalancerAttributesInput = DescribeLoadBalancerAttributesInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  }
derive instance newtypeDescribeLoadBalancerAttributesInput :: Newtype DescribeLoadBalancerAttributesInput _


newtype DescribeLoadBalancerAttributesOutput = DescribeLoadBalancerAttributesOutput 
  { "Attributes" :: NullOrUndefined (LoadBalancerAttributes)
  }
derive instance newtypeDescribeLoadBalancerAttributesOutput :: Newtype DescribeLoadBalancerAttributesOutput _


newtype DescribeLoadBalancersInput = DescribeLoadBalancersInput 
  { "LoadBalancerArns" :: NullOrUndefined (LoadBalancerArns)
  , "Names" :: NullOrUndefined (LoadBalancerNames)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeLoadBalancersInput :: Newtype DescribeLoadBalancersInput _


newtype DescribeLoadBalancersOutput = DescribeLoadBalancersOutput 
  { "LoadBalancers" :: NullOrUndefined (LoadBalancers)
  , "NextMarker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeLoadBalancersOutput :: Newtype DescribeLoadBalancersOutput _


newtype DescribeRulesInput = DescribeRulesInput 
  { "ListenerArn" :: NullOrUndefined (ListenerArn)
  , "RuleArns" :: NullOrUndefined (RuleArns)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeRulesInput :: Newtype DescribeRulesInput _


newtype DescribeRulesOutput = DescribeRulesOutput 
  { "Rules" :: NullOrUndefined (Rules)
  , "NextMarker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeRulesOutput :: Newtype DescribeRulesOutput _


newtype DescribeSSLPoliciesInput = DescribeSSLPoliciesInput 
  { "Names" :: NullOrUndefined (SslPolicyNames)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeSSLPoliciesInput :: Newtype DescribeSSLPoliciesInput _


newtype DescribeSSLPoliciesOutput = DescribeSSLPoliciesOutput 
  { "SslPolicies" :: NullOrUndefined (SslPolicies)
  , "NextMarker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeSSLPoliciesOutput :: Newtype DescribeSSLPoliciesOutput _


newtype DescribeTagsInput = DescribeTagsInput 
  { "ResourceArns" :: (ResourceArns)
  }
derive instance newtypeDescribeTagsInput :: Newtype DescribeTagsInput _


newtype DescribeTagsOutput = DescribeTagsOutput 
  { "TagDescriptions" :: NullOrUndefined (TagDescriptions)
  }
derive instance newtypeDescribeTagsOutput :: Newtype DescribeTagsOutput _


newtype DescribeTargetGroupAttributesInput = DescribeTargetGroupAttributesInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  }
derive instance newtypeDescribeTargetGroupAttributesInput :: Newtype DescribeTargetGroupAttributesInput _


newtype DescribeTargetGroupAttributesOutput = DescribeTargetGroupAttributesOutput 
  { "Attributes" :: NullOrUndefined (TargetGroupAttributes)
  }
derive instance newtypeDescribeTargetGroupAttributesOutput :: Newtype DescribeTargetGroupAttributesOutput _


newtype DescribeTargetGroupsInput = DescribeTargetGroupsInput 
  { "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn)
  , "TargetGroupArns" :: NullOrUndefined (TargetGroupArns)
  , "Names" :: NullOrUndefined (TargetGroupNames)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeTargetGroupsInput :: Newtype DescribeTargetGroupsInput _


newtype DescribeTargetGroupsOutput = DescribeTargetGroupsOutput 
  { "TargetGroups" :: NullOrUndefined (TargetGroups)
  , "NextMarker" :: NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTargetGroupsOutput :: Newtype DescribeTargetGroupsOutput _


newtype DescribeTargetHealthInput = DescribeTargetHealthInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: NullOrUndefined (TargetDescriptions)
  }
derive instance newtypeDescribeTargetHealthInput :: Newtype DescribeTargetHealthInput _


newtype DescribeTargetHealthOutput = DescribeTargetHealthOutput 
  { "TargetHealthDescriptions" :: NullOrUndefined (TargetHealthDescriptions)
  }
derive instance newtypeDescribeTargetHealthOutput :: Newtype DescribeTargetHealthOutput _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


-- | <p>A listener with the specified port already exists.</p>
newtype DuplicateListenerException = DuplicateListenerException 
  { 
  }
derive instance newtypeDuplicateListenerException :: Newtype DuplicateListenerException _


-- | <p>A load balancer with the specified name already exists.</p>
newtype DuplicateLoadBalancerNameException = DuplicateLoadBalancerNameException 
  { 
  }
derive instance newtypeDuplicateLoadBalancerNameException :: Newtype DuplicateLoadBalancerNameException _


-- | <p>A tag key was specified more than once.</p>
newtype DuplicateTagKeysException = DuplicateTagKeysException 
  { 
  }
derive instance newtypeDuplicateTagKeysException :: Newtype DuplicateTagKeysException _


-- | <p>A target group with the specified name already exists.</p>
newtype DuplicateTargetGroupNameException = DuplicateTargetGroupNameException 
  { 
  }
derive instance newtypeDuplicateTargetGroupNameException :: Newtype DuplicateTargetGroupNameException _


newtype HealthCheckIntervalSeconds = HealthCheckIntervalSeconds Int
derive instance newtypeHealthCheckIntervalSeconds :: Newtype HealthCheckIntervalSeconds _


newtype HealthCheckPort = HealthCheckPort String
derive instance newtypeHealthCheckPort :: Newtype HealthCheckPort _


newtype HealthCheckThresholdCount = HealthCheckThresholdCount Int
derive instance newtypeHealthCheckThresholdCount :: Newtype HealthCheckThresholdCount _


newtype HealthCheckTimeoutSeconds = HealthCheckTimeoutSeconds Int
derive instance newtypeHealthCheckTimeoutSeconds :: Newtype HealthCheckTimeoutSeconds _


-- | <p>The health of the specified targets could not be retrieved due to an internal error.</p>
newtype HealthUnavailableException = HealthUnavailableException 
  { 
  }
derive instance newtypeHealthUnavailableException :: Newtype HealthUnavailableException _


newtype HttpCode = HttpCode String
derive instance newtypeHttpCode :: Newtype HttpCode _


-- | <p>The specified configuration is not valid with this protocol.</p>
newtype IncompatibleProtocolsException = IncompatibleProtocolsException 
  { 
  }
derive instance newtypeIncompatibleProtocolsException :: Newtype IncompatibleProtocolsException _


-- | <p>The requested configuration is not valid.</p>
newtype InvalidConfigurationRequestException = InvalidConfigurationRequestException 
  { 
  }
derive instance newtypeInvalidConfigurationRequestException :: Newtype InvalidConfigurationRequestException _


-- | <p>The requested scheme is not valid.</p>
newtype InvalidSchemeException = InvalidSchemeException 
  { 
  }
derive instance newtypeInvalidSchemeException :: Newtype InvalidSchemeException _


-- | <p>The specified security group does not exist.</p>
newtype InvalidSecurityGroupException = InvalidSecurityGroupException 
  { 
  }
derive instance newtypeInvalidSecurityGroupException :: Newtype InvalidSecurityGroupException _


-- | <p>The specified subnet is out of available addresses.</p>
newtype InvalidSubnetException = InvalidSubnetException 
  { 
  }
derive instance newtypeInvalidSubnetException :: Newtype InvalidSubnetException _


-- | <p>The specified target does not exist, is not in the same VPC as the target group, or has an unsupported instance type.</p>
newtype InvalidTargetException = InvalidTargetException 
  { 
  }
derive instance newtypeInvalidTargetException :: Newtype InvalidTargetException _


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _


newtype IpAddressType = IpAddressType String
derive instance newtypeIpAddressType :: Newtype IpAddressType _


newtype IsDefault = IsDefault Boolean
derive instance newtypeIsDefault :: Newtype IsDefault _


-- | <p>Information about an Elastic Load Balancing resource limit for your AWS account.</p>
newtype Limit = Limit 
  { "Name" :: NullOrUndefined (Name)
  , "Max" :: NullOrUndefined (Max)
  }
derive instance newtypeLimit :: Newtype Limit _


newtype Limits = Limits (Array Limit)
derive instance newtypeLimits :: Newtype Limits _


newtype ListOfString = ListOfString (Array StringValue)
derive instance newtypeListOfString :: Newtype ListOfString _


-- | <p>Information about a listener.</p>
newtype Listener = Listener 
  { "ListenerArn" :: NullOrUndefined (ListenerArn)
  , "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn)
  , "Port" :: NullOrUndefined (Port)
  , "Protocol" :: NullOrUndefined (ProtocolEnum)
  , "Certificates" :: NullOrUndefined (CertificateList)
  , "SslPolicy" :: NullOrUndefined (SslPolicyName)
  , "DefaultActions" :: NullOrUndefined (Actions)
  }
derive instance newtypeListener :: Newtype Listener _


newtype ListenerArn = ListenerArn String
derive instance newtypeListenerArn :: Newtype ListenerArn _


newtype ListenerArns = ListenerArns (Array ListenerArn)
derive instance newtypeListenerArns :: Newtype ListenerArns _


-- | <p>The specified listener does not exist.</p>
newtype ListenerNotFoundException = ListenerNotFoundException 
  { 
  }
derive instance newtypeListenerNotFoundException :: Newtype ListenerNotFoundException _


newtype Listeners = Listeners (Array Listener)
derive instance newtypeListeners :: Newtype Listeners _


-- | <p>Information about a load balancer.</p>
newtype LoadBalancer = LoadBalancer 
  { "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn)
  , "DNSName" :: NullOrUndefined (DNSName)
  , "CanonicalHostedZoneId" :: NullOrUndefined (CanonicalHostedZoneId)
  , "CreatedTime" :: NullOrUndefined (CreatedTime)
  , "LoadBalancerName" :: NullOrUndefined (LoadBalancerName)
  , "Scheme" :: NullOrUndefined (LoadBalancerSchemeEnum)
  , "VpcId" :: NullOrUndefined (VpcId)
  , "State" :: NullOrUndefined (LoadBalancerState)
  , "Type" :: NullOrUndefined (LoadBalancerTypeEnum)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  , "SecurityGroups" :: NullOrUndefined (SecurityGroups)
  , "IpAddressType" :: NullOrUndefined (IpAddressType)
  }
derive instance newtypeLoadBalancer :: Newtype LoadBalancer _


-- | <p>Information about a static IP address for a load balancer.</p>
newtype LoadBalancerAddress = LoadBalancerAddress 
  { "IpAddress" :: NullOrUndefined (IpAddress)
  , "AllocationId" :: NullOrUndefined (AllocationId)
  }
derive instance newtypeLoadBalancerAddress :: Newtype LoadBalancerAddress _


newtype LoadBalancerAddresses = LoadBalancerAddresses (Array LoadBalancerAddress)
derive instance newtypeLoadBalancerAddresses :: Newtype LoadBalancerAddresses _


newtype LoadBalancerArn = LoadBalancerArn String
derive instance newtypeLoadBalancerArn :: Newtype LoadBalancerArn _


newtype LoadBalancerArns = LoadBalancerArns (Array LoadBalancerArn)
derive instance newtypeLoadBalancerArns :: Newtype LoadBalancerArns _


-- | <p>Information about a load balancer attribute.</p>
newtype LoadBalancerAttribute = LoadBalancerAttribute 
  { "Key" :: NullOrUndefined (LoadBalancerAttributeKey)
  , "Value" :: NullOrUndefined (LoadBalancerAttributeValue)
  }
derive instance newtypeLoadBalancerAttribute :: Newtype LoadBalancerAttribute _


newtype LoadBalancerAttributeKey = LoadBalancerAttributeKey String
derive instance newtypeLoadBalancerAttributeKey :: Newtype LoadBalancerAttributeKey _


newtype LoadBalancerAttributeValue = LoadBalancerAttributeValue String
derive instance newtypeLoadBalancerAttributeValue :: Newtype LoadBalancerAttributeValue _


newtype LoadBalancerAttributes = LoadBalancerAttributes (Array LoadBalancerAttribute)
derive instance newtypeLoadBalancerAttributes :: Newtype LoadBalancerAttributes _


newtype LoadBalancerName = LoadBalancerName String
derive instance newtypeLoadBalancerName :: Newtype LoadBalancerName _


newtype LoadBalancerNames = LoadBalancerNames (Array LoadBalancerName)
derive instance newtypeLoadBalancerNames :: Newtype LoadBalancerNames _


-- | <p>The specified load balancer does not exist.</p>
newtype LoadBalancerNotFoundException = LoadBalancerNotFoundException 
  { 
  }
derive instance newtypeLoadBalancerNotFoundException :: Newtype LoadBalancerNotFoundException _


newtype LoadBalancerSchemeEnum = LoadBalancerSchemeEnum String
derive instance newtypeLoadBalancerSchemeEnum :: Newtype LoadBalancerSchemeEnum _


-- | <p>Information about the state of the load balancer.</p>
newtype LoadBalancerState = LoadBalancerState 
  { "Code" :: NullOrUndefined (LoadBalancerStateEnum)
  , "Reason" :: NullOrUndefined (StateReason)
  }
derive instance newtypeLoadBalancerState :: Newtype LoadBalancerState _


newtype LoadBalancerStateEnum = LoadBalancerStateEnum String
derive instance newtypeLoadBalancerStateEnum :: Newtype LoadBalancerStateEnum _


newtype LoadBalancerTypeEnum = LoadBalancerTypeEnum String
derive instance newtypeLoadBalancerTypeEnum :: Newtype LoadBalancerTypeEnum _


newtype LoadBalancers = LoadBalancers (Array LoadBalancer)
derive instance newtypeLoadBalancers :: Newtype LoadBalancers _


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _


-- | <p>Information to use when checking for a successful response from a target.</p>
newtype Matcher = Matcher 
  { "HttpCode" :: (HttpCode)
  }
derive instance newtypeMatcher :: Newtype Matcher _


newtype Max = Max String
derive instance newtypeMax :: Newtype Max _


newtype ModifyListenerInput = ModifyListenerInput 
  { "ListenerArn" :: (ListenerArn)
  , "Port" :: NullOrUndefined (Port)
  , "Protocol" :: NullOrUndefined (ProtocolEnum)
  , "SslPolicy" :: NullOrUndefined (SslPolicyName)
  , "Certificates" :: NullOrUndefined (CertificateList)
  , "DefaultActions" :: NullOrUndefined (Actions)
  }
derive instance newtypeModifyListenerInput :: Newtype ModifyListenerInput _


newtype ModifyListenerOutput = ModifyListenerOutput 
  { "Listeners" :: NullOrUndefined (Listeners)
  }
derive instance newtypeModifyListenerOutput :: Newtype ModifyListenerOutput _


newtype ModifyLoadBalancerAttributesInput = ModifyLoadBalancerAttributesInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Attributes" :: (LoadBalancerAttributes)
  }
derive instance newtypeModifyLoadBalancerAttributesInput :: Newtype ModifyLoadBalancerAttributesInput _


newtype ModifyLoadBalancerAttributesOutput = ModifyLoadBalancerAttributesOutput 
  { "Attributes" :: NullOrUndefined (LoadBalancerAttributes)
  }
derive instance newtypeModifyLoadBalancerAttributesOutput :: Newtype ModifyLoadBalancerAttributesOutput _


newtype ModifyRuleInput = ModifyRuleInput 
  { "RuleArn" :: (RuleArn)
  , "Conditions" :: NullOrUndefined (RuleConditionList)
  , "Actions" :: NullOrUndefined (Actions)
  }
derive instance newtypeModifyRuleInput :: Newtype ModifyRuleInput _


newtype ModifyRuleOutput = ModifyRuleOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }
derive instance newtypeModifyRuleOutput :: Newtype ModifyRuleOutput _


newtype ModifyTargetGroupAttributesInput = ModifyTargetGroupAttributesInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Attributes" :: (TargetGroupAttributes)
  }
derive instance newtypeModifyTargetGroupAttributesInput :: Newtype ModifyTargetGroupAttributesInput _


newtype ModifyTargetGroupAttributesOutput = ModifyTargetGroupAttributesOutput 
  { "Attributes" :: NullOrUndefined (TargetGroupAttributes)
  }
derive instance newtypeModifyTargetGroupAttributesOutput :: Newtype ModifyTargetGroupAttributesOutput _


newtype ModifyTargetGroupInput = ModifyTargetGroupInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "HealthCheckProtocol" :: NullOrUndefined (ProtocolEnum)
  , "HealthCheckPort" :: NullOrUndefined (HealthCheckPort)
  , "HealthCheckPath" :: NullOrUndefined (Path)
  , "HealthCheckIntervalSeconds" :: NullOrUndefined (HealthCheckIntervalSeconds)
  , "HealthCheckTimeoutSeconds" :: NullOrUndefined (HealthCheckTimeoutSeconds)
  , "HealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount)
  , "UnhealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount)
  , "Matcher" :: NullOrUndefined (Matcher)
  }
derive instance newtypeModifyTargetGroupInput :: Newtype ModifyTargetGroupInput _


newtype ModifyTargetGroupOutput = ModifyTargetGroupOutput 
  { "TargetGroups" :: NullOrUndefined (TargetGroups)
  }
derive instance newtypeModifyTargetGroupOutput :: Newtype ModifyTargetGroupOutput _


newtype Name = Name String
derive instance newtypeName :: Newtype Name _


-- | <p>This operation is not allowed.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { 
  }
derive instance newtypeOperationNotPermittedException :: Newtype OperationNotPermittedException _


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _


newtype Path = Path String
derive instance newtypePath :: Newtype Path _


newtype Port = Port Int
derive instance newtypePort :: Newtype Port _


-- | <p>The specified priority is in use.</p>
newtype PriorityInUseException = PriorityInUseException 
  { 
  }
derive instance newtypePriorityInUseException :: Newtype PriorityInUseException _


newtype ProtocolEnum = ProtocolEnum String
derive instance newtypeProtocolEnum :: Newtype ProtocolEnum _


newtype RegisterTargetsInput = RegisterTargetsInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: (TargetDescriptions)
  }
derive instance newtypeRegisterTargetsInput :: Newtype RegisterTargetsInput _


newtype RegisterTargetsOutput = RegisterTargetsOutput 
  { 
  }
derive instance newtypeRegisterTargetsOutput :: Newtype RegisterTargetsOutput _


newtype RemoveListenerCertificatesInput = RemoveListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Certificates" :: (CertificateList)
  }
derive instance newtypeRemoveListenerCertificatesInput :: Newtype RemoveListenerCertificatesInput _


newtype RemoveListenerCertificatesOutput = RemoveListenerCertificatesOutput 
  { 
  }
derive instance newtypeRemoveListenerCertificatesOutput :: Newtype RemoveListenerCertificatesOutput _


newtype RemoveTagsInput = RemoveTagsInput 
  { "ResourceArns" :: (ResourceArns)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeRemoveTagsInput :: Newtype RemoveTagsInput _


newtype RemoveTagsOutput = RemoveTagsOutput 
  { 
  }
derive instance newtypeRemoveTagsOutput :: Newtype RemoveTagsOutput _


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _


newtype ResourceArns = ResourceArns (Array ResourceArn)
derive instance newtypeResourceArns :: Newtype ResourceArns _


-- | <p>A specified resource is in use.</p>
newtype ResourceInUseException = ResourceInUseException 
  { 
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>Information about a rule.</p>
newtype Rule = Rule 
  { "RuleArn" :: NullOrUndefined (RuleArn)
  , "Priority" :: NullOrUndefined (String)
  , "Conditions" :: NullOrUndefined (RuleConditionList)
  , "Actions" :: NullOrUndefined (Actions)
  , "IsDefault" :: NullOrUndefined (IsDefault)
  }
derive instance newtypeRule :: Newtype Rule _


newtype RuleArn = RuleArn String
derive instance newtypeRuleArn :: Newtype RuleArn _


newtype RuleArns = RuleArns (Array RuleArn)
derive instance newtypeRuleArns :: Newtype RuleArns _


-- | <p>Information about a condition for a rule.</p>
newtype RuleCondition = RuleCondition 
  { "Field" :: NullOrUndefined (ConditionFieldName)
  , "Values" :: NullOrUndefined (ListOfString)
  }
derive instance newtypeRuleCondition :: Newtype RuleCondition _


newtype RuleConditionList = RuleConditionList (Array RuleCondition)
derive instance newtypeRuleConditionList :: Newtype RuleConditionList _


-- | <p>The specified rule does not exist.</p>
newtype RuleNotFoundException = RuleNotFoundException 
  { 
  }
derive instance newtypeRuleNotFoundException :: Newtype RuleNotFoundException _


newtype RulePriority = RulePriority Int
derive instance newtypeRulePriority :: Newtype RulePriority _


newtype RulePriorityList = RulePriorityList (Array RulePriorityPair)
derive instance newtypeRulePriorityList :: Newtype RulePriorityList _


-- | <p>Information about the priorities for the rules for a listener.</p>
newtype RulePriorityPair = RulePriorityPair 
  { "RuleArn" :: NullOrUndefined (RuleArn)
  , "Priority" :: NullOrUndefined (RulePriority)
  }
derive instance newtypeRulePriorityPair :: Newtype RulePriorityPair _


newtype Rules = Rules (Array Rule)
derive instance newtypeRules :: Newtype Rules _


-- | <p>The specified SSL policy does not exist.</p>
newtype SSLPolicyNotFoundException = SSLPolicyNotFoundException 
  { 
  }
derive instance newtypeSSLPolicyNotFoundException :: Newtype SSLPolicyNotFoundException _


newtype SecurityGroupId = SecurityGroupId String
derive instance newtypeSecurityGroupId :: Newtype SecurityGroupId _


newtype SecurityGroups = SecurityGroups (Array SecurityGroupId)
derive instance newtypeSecurityGroups :: Newtype SecurityGroups _


newtype SetIpAddressTypeInput = SetIpAddressTypeInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "IpAddressType" :: (IpAddressType)
  }
derive instance newtypeSetIpAddressTypeInput :: Newtype SetIpAddressTypeInput _


newtype SetIpAddressTypeOutput = SetIpAddressTypeOutput 
  { "IpAddressType" :: NullOrUndefined (IpAddressType)
  }
derive instance newtypeSetIpAddressTypeOutput :: Newtype SetIpAddressTypeOutput _


newtype SetRulePrioritiesInput = SetRulePrioritiesInput 
  { "RulePriorities" :: (RulePriorityList)
  }
derive instance newtypeSetRulePrioritiesInput :: Newtype SetRulePrioritiesInput _


newtype SetRulePrioritiesOutput = SetRulePrioritiesOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }
derive instance newtypeSetRulePrioritiesOutput :: Newtype SetRulePrioritiesOutput _


newtype SetSecurityGroupsInput = SetSecurityGroupsInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "SecurityGroups" :: (SecurityGroups)
  }
derive instance newtypeSetSecurityGroupsInput :: Newtype SetSecurityGroupsInput _


newtype SetSecurityGroupsOutput = SetSecurityGroupsOutput 
  { "SecurityGroupIds" :: NullOrUndefined (SecurityGroups)
  }
derive instance newtypeSetSecurityGroupsOutput :: Newtype SetSecurityGroupsOutput _


newtype SetSubnetsInput = SetSubnetsInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Subnets" :: (Subnets)
  , "SubnetMappings" :: NullOrUndefined (SubnetMappings)
  }
derive instance newtypeSetSubnetsInput :: Newtype SetSubnetsInput _


newtype SetSubnetsOutput = SetSubnetsOutput 
  { "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  }
derive instance newtypeSetSubnetsOutput :: Newtype SetSubnetsOutput _


newtype SslPolicies = SslPolicies (Array SslPolicy)
derive instance newtypeSslPolicies :: Newtype SslPolicies _


-- | <p>Information about a policy used for SSL negotiation.</p>
newtype SslPolicy = SslPolicy 
  { "SslProtocols" :: NullOrUndefined (SslProtocols)
  , "Ciphers" :: NullOrUndefined (Ciphers)
  , "Name" :: NullOrUndefined (SslPolicyName)
  }
derive instance newtypeSslPolicy :: Newtype SslPolicy _


newtype SslPolicyName = SslPolicyName String
derive instance newtypeSslPolicyName :: Newtype SslPolicyName _


newtype SslPolicyNames = SslPolicyNames (Array SslPolicyName)
derive instance newtypeSslPolicyNames :: Newtype SslPolicyNames _


newtype SslProtocol = SslProtocol String
derive instance newtypeSslProtocol :: Newtype SslProtocol _


newtype SslProtocols = SslProtocols (Array SslProtocol)
derive instance newtypeSslProtocols :: Newtype SslProtocols _


newtype StateReason = StateReason String
derive instance newtypeStateReason :: Newtype StateReason _


newtype StringValue = StringValue String
derive instance newtypeStringValue :: Newtype StringValue _


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _


-- | <p>Information about a subnet mapping.</p>
newtype SubnetMapping = SubnetMapping 
  { "SubnetId" :: NullOrUndefined (SubnetId)
  , "AllocationId" :: NullOrUndefined (AllocationId)
  }
derive instance newtypeSubnetMapping :: Newtype SubnetMapping _


newtype SubnetMappings = SubnetMappings (Array SubnetMapping)
derive instance newtypeSubnetMappings :: Newtype SubnetMappings _


-- | <p>The specified subnet does not exist.</p>
newtype SubnetNotFoundException = SubnetNotFoundException 
  { 
  }
derive instance newtypeSubnetNotFoundException :: Newtype SubnetNotFoundException _


newtype Subnets = Subnets (Array SubnetId)
derive instance newtypeSubnets :: Newtype Subnets _


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


-- | <p>The tags associated with a resource.</p>
newtype TagDescription = TagDescription 
  { "ResourceArn" :: NullOrUndefined (ResourceArn)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeTagDescription :: Newtype TagDescription _


newtype TagDescriptions = TagDescriptions (Array TagDescription)
derive instance newtypeTagDescriptions :: Newtype TagDescriptions _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


-- | <p>Information about a target.</p>
newtype TargetDescription = TargetDescription 
  { "Id" :: (TargetId)
  , "Port" :: NullOrUndefined (Port)
  , "AvailabilityZone" :: NullOrUndefined (ZoneName)
  }
derive instance newtypeTargetDescription :: Newtype TargetDescription _


newtype TargetDescriptions = TargetDescriptions (Array TargetDescription)
derive instance newtypeTargetDescriptions :: Newtype TargetDescriptions _


-- | <p>Information about a target group.</p>
newtype TargetGroup = TargetGroup 
  { "TargetGroupArn" :: NullOrUndefined (TargetGroupArn)
  , "TargetGroupName" :: NullOrUndefined (TargetGroupName)
  , "Protocol" :: NullOrUndefined (ProtocolEnum)
  , "Port" :: NullOrUndefined (Port)
  , "VpcId" :: NullOrUndefined (VpcId)
  , "HealthCheckProtocol" :: NullOrUndefined (ProtocolEnum)
  , "HealthCheckPort" :: NullOrUndefined (HealthCheckPort)
  , "HealthCheckIntervalSeconds" :: NullOrUndefined (HealthCheckIntervalSeconds)
  , "HealthCheckTimeoutSeconds" :: NullOrUndefined (HealthCheckTimeoutSeconds)
  , "HealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount)
  , "UnhealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount)
  , "HealthCheckPath" :: NullOrUndefined (Path)
  , "Matcher" :: NullOrUndefined (Matcher)
  , "LoadBalancerArns" :: NullOrUndefined (LoadBalancerArns)
  , "TargetType" :: NullOrUndefined (TargetTypeEnum)
  }
derive instance newtypeTargetGroup :: Newtype TargetGroup _


newtype TargetGroupArn = TargetGroupArn String
derive instance newtypeTargetGroupArn :: Newtype TargetGroupArn _


newtype TargetGroupArns = TargetGroupArns (Array TargetGroupArn)
derive instance newtypeTargetGroupArns :: Newtype TargetGroupArns _


-- | <p>You've reached the limit on the number of load balancers per target group.</p>
newtype TargetGroupAssociationLimitException = TargetGroupAssociationLimitException 
  { 
  }
derive instance newtypeTargetGroupAssociationLimitException :: Newtype TargetGroupAssociationLimitException _


-- | <p>Information about a target group attribute.</p>
newtype TargetGroupAttribute = TargetGroupAttribute 
  { "Key" :: NullOrUndefined (TargetGroupAttributeKey)
  , "Value" :: NullOrUndefined (TargetGroupAttributeValue)
  }
derive instance newtypeTargetGroupAttribute :: Newtype TargetGroupAttribute _


newtype TargetGroupAttributeKey = TargetGroupAttributeKey String
derive instance newtypeTargetGroupAttributeKey :: Newtype TargetGroupAttributeKey _


newtype TargetGroupAttributeValue = TargetGroupAttributeValue String
derive instance newtypeTargetGroupAttributeValue :: Newtype TargetGroupAttributeValue _


newtype TargetGroupAttributes = TargetGroupAttributes (Array TargetGroupAttribute)
derive instance newtypeTargetGroupAttributes :: Newtype TargetGroupAttributes _


newtype TargetGroupName = TargetGroupName String
derive instance newtypeTargetGroupName :: Newtype TargetGroupName _


newtype TargetGroupNames = TargetGroupNames (Array TargetGroupName)
derive instance newtypeTargetGroupNames :: Newtype TargetGroupNames _


-- | <p>The specified target group does not exist.</p>
newtype TargetGroupNotFoundException = TargetGroupNotFoundException 
  { 
  }
derive instance newtypeTargetGroupNotFoundException :: Newtype TargetGroupNotFoundException _


newtype TargetGroups = TargetGroups (Array TargetGroup)
derive instance newtypeTargetGroups :: Newtype TargetGroups _


-- | <p>Information about the current health of a target.</p>
newtype TargetHealth = TargetHealth 
  { "State" :: NullOrUndefined (TargetHealthStateEnum)
  , "Reason" :: NullOrUndefined (TargetHealthReasonEnum)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeTargetHealth :: Newtype TargetHealth _


-- | <p>Information about the health of a target.</p>
newtype TargetHealthDescription = TargetHealthDescription 
  { "Target" :: NullOrUndefined (TargetDescription)
  , "HealthCheckPort" :: NullOrUndefined (HealthCheckPort)
  , "TargetHealth" :: NullOrUndefined (TargetHealth)
  }
derive instance newtypeTargetHealthDescription :: Newtype TargetHealthDescription _


newtype TargetHealthDescriptions = TargetHealthDescriptions (Array TargetHealthDescription)
derive instance newtypeTargetHealthDescriptions :: Newtype TargetHealthDescriptions _


newtype TargetHealthReasonEnum = TargetHealthReasonEnum String
derive instance newtypeTargetHealthReasonEnum :: Newtype TargetHealthReasonEnum _


newtype TargetHealthStateEnum = TargetHealthStateEnum String
derive instance newtypeTargetHealthStateEnum :: Newtype TargetHealthStateEnum _


newtype TargetId = TargetId String
derive instance newtypeTargetId :: Newtype TargetId _


newtype TargetTypeEnum = TargetTypeEnum String
derive instance newtypeTargetTypeEnum :: Newtype TargetTypeEnum _


-- | <p>You've reached the limit on the number of certificates per load balancer.</p>
newtype TooManyCertificatesException = TooManyCertificatesException 
  { 
  }
derive instance newtypeTooManyCertificatesException :: Newtype TooManyCertificatesException _


-- | <p>You've reached the limit on the number of listeners per load balancer.</p>
newtype TooManyListenersException = TooManyListenersException 
  { 
  }
derive instance newtypeTooManyListenersException :: Newtype TooManyListenersException _


-- | <p>You've reached the limit on the number of load balancers for your AWS account.</p>
newtype TooManyLoadBalancersException = TooManyLoadBalancersException 
  { 
  }
derive instance newtypeTooManyLoadBalancersException :: Newtype TooManyLoadBalancersException _


-- | <p>You've reached the limit on the number of times a target can be registered with a load balancer.</p>
newtype TooManyRegistrationsForTargetIdException = TooManyRegistrationsForTargetIdException 
  { 
  }
derive instance newtypeTooManyRegistrationsForTargetIdException :: Newtype TooManyRegistrationsForTargetIdException _


-- | <p>You've reached the limit on the number of rules per load balancer.</p>
newtype TooManyRulesException = TooManyRulesException 
  { 
  }
derive instance newtypeTooManyRulesException :: Newtype TooManyRulesException _


-- | <p>You've reached the limit on the number of tags per load balancer.</p>
newtype TooManyTagsException = TooManyTagsException 
  { 
  }
derive instance newtypeTooManyTagsException :: Newtype TooManyTagsException _


-- | <p>You've reached the limit on the number of target groups for your AWS account.</p>
newtype TooManyTargetGroupsException = TooManyTargetGroupsException 
  { 
  }
derive instance newtypeTooManyTargetGroupsException :: Newtype TooManyTargetGroupsException _


-- | <p>You've reached the limit on the number of targets.</p>
newtype TooManyTargetsException = TooManyTargetsException 
  { 
  }
derive instance newtypeTooManyTargetsException :: Newtype TooManyTargetsException _


-- | <p>The specified protocol is not supported.</p>
newtype UnsupportedProtocolException = UnsupportedProtocolException 
  { 
  }
derive instance newtypeUnsupportedProtocolException :: Newtype UnsupportedProtocolException _


newtype VpcId = VpcId String
derive instance newtypeVpcId :: Newtype VpcId _


newtype ZoneName = ZoneName String
derive instance newtypeZoneName :: Newtype ZoneName _
