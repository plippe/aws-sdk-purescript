

-- | <fullname>Elastic Load Balancing</fullname> <p>A load balancer distributes incoming traffic across targets, such as your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered targets and ensures that it routes traffic only to healthy targets. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer. You configure a target group with a protocol and port number for connections from the load balancer to the targets, and with health check settings to be used when checking the health status of the targets.</p> <p>Elastic Load Balancing supports the following types of load balancers: Application Load Balancers, Network Load Balancers, and Classic Load Balancers.</p> <p>An Application Load Balancer makes routing and load balancing decisions at the application layer (HTTP/HTTPS). A Network Load Balancer makes routing and load balancing decisions at the transport layer (TCP). Both Application Load Balancers and Network Load Balancers can route requests to one or more ports on each EC2 instance or container instance in your virtual private cloud (VPC).</p> <p>A Classic Load Balancer makes routing and load balancing decisions either at the transport layer (TCP/SSL) or the application layer (HTTP/HTTPS), and supports either EC2-Classic or a VPC. For more information, see the <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/">Elastic Load Balancing User Guide</a>.</p> <p>This reference covers the 2015-12-01 API, which supports Application Load Balancers and Network Load Balancers. The 2012-06-01 API supports Classic Load Balancers.</p> <p>To get started, complete the following tasks:</p> <ol> <li> <p>Create a load balancer using <a>CreateLoadBalancer</a>.</p> </li> <li> <p>Create a target group using <a>CreateTargetGroup</a>.</p> </li> <li> <p>Register targets for the target group using <a>RegisterTargets</a>.</p> </li> <li> <p>Create one or more listeners for your load balancer using <a>CreateListener</a>.</p> </li> </ol> <p>To delete a load balancer and its related resources, complete the following tasks:</p> <ol> <li> <p>Delete the load balancer using <a>DeleteLoadBalancer</a>.</p> </li> <li> <p>Delete the target group using <a>DeleteTargetGroup</a>.</p> </li> </ol> <p>All Elastic Load Balancing operations are idempotent, which means that they complete at most one time. If you repeat an operation, it succeeds.</p>
module AWS.ELBv2 where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
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


newtype ActionTypeEnum = ActionTypeEnum String


newtype Actions = Actions (Array Action)


newtype AddListenerCertificatesInput = AddListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Certificates" :: (CertificateList)
  }


newtype AddListenerCertificatesOutput = AddListenerCertificatesOutput 
  { "Certificates" :: NullOrUndefined (CertificateList)
  }


newtype AddTagsInput = AddTagsInput 
  { "ResourceArns" :: (ResourceArns)
  , "Tags" :: (TagList)
  }


newtype AddTagsOutput = AddTagsOutput 
  { 
  }


newtype AllocationId = AllocationId String


-- | <p>The specified allocation ID does not exist.</p>
newtype AllocationIdNotFoundException = AllocationIdNotFoundException 
  { 
  }


-- | <p>Information about an Availability Zone.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "ZoneName" :: NullOrUndefined (ZoneName)
  , "SubnetId" :: NullOrUndefined (SubnetId)
  , "LoadBalancerAddresses" :: NullOrUndefined (LoadBalancerAddresses)
  }


-- | <p>The specified Availability Zone is not supported.</p>
newtype AvailabilityZoneNotSupportedException = AvailabilityZoneNotSupportedException 
  { 
  }


newtype AvailabilityZones = AvailabilityZones (Array AvailabilityZone)


newtype CanonicalHostedZoneId = CanonicalHostedZoneId String


-- | <p>Information about an SSL server certificate.</p>
newtype Certificate = Certificate 
  { "CertificateArn" :: NullOrUndefined (CertificateArn)
  , "IsDefault" :: NullOrUndefined (Default)
  }


newtype CertificateArn = CertificateArn String


newtype CertificateList = CertificateList (Array Certificate)


-- | <p>The specified certificate does not exist.</p>
newtype CertificateNotFoundException = CertificateNotFoundException 
  { 
  }


-- | <p>Information about a cipher used in a policy.</p>
newtype Cipher = Cipher 
  { "Name" :: NullOrUndefined (CipherName)
  , "Priority" :: NullOrUndefined (CipherPriority)
  }


newtype CipherName = CipherName String


newtype CipherPriority = CipherPriority Int


newtype Ciphers = Ciphers (Array Cipher)


newtype ConditionFieldName = ConditionFieldName String


newtype CreateListenerInput = CreateListenerInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Protocol" :: (ProtocolEnum)
  , "Port" :: (Port)
  , "SslPolicy" :: NullOrUndefined (SslPolicyName)
  , "Certificates" :: NullOrUndefined (CertificateList)
  , "DefaultActions" :: (Actions)
  }


newtype CreateListenerOutput = CreateListenerOutput 
  { "Listeners" :: NullOrUndefined (Listeners)
  }


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


newtype CreateLoadBalancerOutput = CreateLoadBalancerOutput 
  { "LoadBalancers" :: NullOrUndefined (LoadBalancers)
  }


newtype CreateRuleInput = CreateRuleInput 
  { "ListenerArn" :: (ListenerArn)
  , "Conditions" :: (RuleConditionList)
  , "Priority" :: (RulePriority)
  , "Actions" :: (Actions)
  }


newtype CreateRuleOutput = CreateRuleOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }


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


newtype CreateTargetGroupOutput = CreateTargetGroupOutput 
  { "TargetGroups" :: NullOrUndefined (TargetGroups)
  }


newtype CreatedTime = CreatedTime Number


newtype DNSName = DNSName String


newtype Default = Default Boolean


newtype DeleteListenerInput = DeleteListenerInput 
  { "ListenerArn" :: (ListenerArn)
  }


newtype DeleteListenerOutput = DeleteListenerOutput 
  { 
  }


newtype DeleteLoadBalancerInput = DeleteLoadBalancerInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  }


newtype DeleteLoadBalancerOutput = DeleteLoadBalancerOutput 
  { 
  }


newtype DeleteRuleInput = DeleteRuleInput 
  { "RuleArn" :: (RuleArn)
  }


newtype DeleteRuleOutput = DeleteRuleOutput 
  { 
  }


newtype DeleteTargetGroupInput = DeleteTargetGroupInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  }


newtype DeleteTargetGroupOutput = DeleteTargetGroupOutput 
  { 
  }


newtype DeregisterTargetsInput = DeregisterTargetsInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: (TargetDescriptions)
  }


newtype DeregisterTargetsOutput = DeregisterTargetsOutput 
  { 
  }


newtype DescribeAccountLimitsInput = DescribeAccountLimitsInput 
  { "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeAccountLimitsOutput = DescribeAccountLimitsOutput 
  { "Limits" :: NullOrUndefined (Limits)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype DescribeListenerCertificatesInput = DescribeListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeListenerCertificatesOutput = DescribeListenerCertificatesOutput 
  { "Certificates" :: NullOrUndefined (CertificateList)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype DescribeListenersInput = DescribeListenersInput 
  { "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn)
  , "ListenerArns" :: NullOrUndefined (ListenerArns)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeListenersOutput = DescribeListenersOutput 
  { "Listeners" :: NullOrUndefined (Listeners)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype DescribeLoadBalancerAttributesInput = DescribeLoadBalancerAttributesInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  }


newtype DescribeLoadBalancerAttributesOutput = DescribeLoadBalancerAttributesOutput 
  { "Attributes" :: NullOrUndefined (LoadBalancerAttributes)
  }


newtype DescribeLoadBalancersInput = DescribeLoadBalancersInput 
  { "LoadBalancerArns" :: NullOrUndefined (LoadBalancerArns)
  , "Names" :: NullOrUndefined (LoadBalancerNames)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeLoadBalancersOutput = DescribeLoadBalancersOutput 
  { "LoadBalancers" :: NullOrUndefined (LoadBalancers)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype DescribeRulesInput = DescribeRulesInput 
  { "ListenerArn" :: NullOrUndefined (ListenerArn)
  , "RuleArns" :: NullOrUndefined (RuleArns)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeRulesOutput = DescribeRulesOutput 
  { "Rules" :: NullOrUndefined (Rules)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype DescribeSSLPoliciesInput = DescribeSSLPoliciesInput 
  { "Names" :: NullOrUndefined (SslPolicyNames)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeSSLPoliciesOutput = DescribeSSLPoliciesOutput 
  { "SslPolicies" :: NullOrUndefined (SslPolicies)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype DescribeTagsInput = DescribeTagsInput 
  { "ResourceArns" :: (ResourceArns)
  }


newtype DescribeTagsOutput = DescribeTagsOutput 
  { "TagDescriptions" :: NullOrUndefined (TagDescriptions)
  }


newtype DescribeTargetGroupAttributesInput = DescribeTargetGroupAttributesInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  }


newtype DescribeTargetGroupAttributesOutput = DescribeTargetGroupAttributesOutput 
  { "Attributes" :: NullOrUndefined (TargetGroupAttributes)
  }


newtype DescribeTargetGroupsInput = DescribeTargetGroupsInput 
  { "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn)
  , "TargetGroupArns" :: NullOrUndefined (TargetGroupArns)
  , "Names" :: NullOrUndefined (TargetGroupNames)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeTargetGroupsOutput = DescribeTargetGroupsOutput 
  { "TargetGroups" :: NullOrUndefined (TargetGroups)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype DescribeTargetHealthInput = DescribeTargetHealthInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: NullOrUndefined (TargetDescriptions)
  }


newtype DescribeTargetHealthOutput = DescribeTargetHealthOutput 
  { "TargetHealthDescriptions" :: NullOrUndefined (TargetHealthDescriptions)
  }


newtype Description = Description String


-- | <p>A listener with the specified port already exists.</p>
newtype DuplicateListenerException = DuplicateListenerException 
  { 
  }


-- | <p>A load balancer with the specified name already exists.</p>
newtype DuplicateLoadBalancerNameException = DuplicateLoadBalancerNameException 
  { 
  }


-- | <p>A tag key was specified more than once.</p>
newtype DuplicateTagKeysException = DuplicateTagKeysException 
  { 
  }


-- | <p>A target group with the specified name already exists.</p>
newtype DuplicateTargetGroupNameException = DuplicateTargetGroupNameException 
  { 
  }


newtype HealthCheckIntervalSeconds = HealthCheckIntervalSeconds Int


newtype HealthCheckPort = HealthCheckPort String


newtype HealthCheckThresholdCount = HealthCheckThresholdCount Int


newtype HealthCheckTimeoutSeconds = HealthCheckTimeoutSeconds Int


-- | <p>The health of the specified targets could not be retrieved due to an internal error.</p>
newtype HealthUnavailableException = HealthUnavailableException 
  { 
  }


newtype HttpCode = HttpCode String


-- | <p>The specified configuration is not valid with this protocol.</p>
newtype IncompatibleProtocolsException = IncompatibleProtocolsException 
  { 
  }


-- | <p>The requested configuration is not valid.</p>
newtype InvalidConfigurationRequestException = InvalidConfigurationRequestException 
  { 
  }


-- | <p>The requested scheme is not valid.</p>
newtype InvalidSchemeException = InvalidSchemeException 
  { 
  }


-- | <p>The specified security group does not exist.</p>
newtype InvalidSecurityGroupException = InvalidSecurityGroupException 
  { 
  }


-- | <p>The specified subnet is out of available addresses.</p>
newtype InvalidSubnetException = InvalidSubnetException 
  { 
  }


-- | <p>The specified target does not exist, is not in the same VPC as the target group, or has an unsupported instance type.</p>
newtype InvalidTargetException = InvalidTargetException 
  { 
  }


newtype IpAddress = IpAddress String


newtype IpAddressType = IpAddressType String


newtype IsDefault = IsDefault Boolean


-- | <p>Information about an Elastic Load Balancing resource limit for your AWS account.</p>
newtype Limit = Limit 
  { "Name" :: NullOrUndefined (Name)
  , "Max" :: NullOrUndefined (Max)
  }


newtype Limits = Limits (Array Limit)


newtype ListOfString = ListOfString (Array StringValue)


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


newtype ListenerArn = ListenerArn String


newtype ListenerArns = ListenerArns (Array ListenerArn)


-- | <p>The specified listener does not exist.</p>
newtype ListenerNotFoundException = ListenerNotFoundException 
  { 
  }


newtype Listeners = Listeners (Array Listener)


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


-- | <p>Information about a static IP address for a load balancer.</p>
newtype LoadBalancerAddress = LoadBalancerAddress 
  { "IpAddress" :: NullOrUndefined (IpAddress)
  , "AllocationId" :: NullOrUndefined (AllocationId)
  }


newtype LoadBalancerAddresses = LoadBalancerAddresses (Array LoadBalancerAddress)


newtype LoadBalancerArn = LoadBalancerArn String


newtype LoadBalancerArns = LoadBalancerArns (Array LoadBalancerArn)


-- | <p>Information about a load balancer attribute.</p>
newtype LoadBalancerAttribute = LoadBalancerAttribute 
  { "Key" :: NullOrUndefined (LoadBalancerAttributeKey)
  , "Value" :: NullOrUndefined (LoadBalancerAttributeValue)
  }


newtype LoadBalancerAttributeKey = LoadBalancerAttributeKey String


newtype LoadBalancerAttributeValue = LoadBalancerAttributeValue String


newtype LoadBalancerAttributes = LoadBalancerAttributes (Array LoadBalancerAttribute)


newtype LoadBalancerName = LoadBalancerName String


newtype LoadBalancerNames = LoadBalancerNames (Array LoadBalancerName)


-- | <p>The specified load balancer does not exist.</p>
newtype LoadBalancerNotFoundException = LoadBalancerNotFoundException 
  { 
  }


newtype LoadBalancerSchemeEnum = LoadBalancerSchemeEnum String


-- | <p>Information about the state of the load balancer.</p>
newtype LoadBalancerState = LoadBalancerState 
  { "Code" :: NullOrUndefined (LoadBalancerStateEnum)
  , "Reason" :: NullOrUndefined (StateReason)
  }


newtype LoadBalancerStateEnum = LoadBalancerStateEnum String


newtype LoadBalancerTypeEnum = LoadBalancerTypeEnum String


newtype LoadBalancers = LoadBalancers (Array LoadBalancer)


newtype Marker = Marker String


-- | <p>Information to use when checking for a successful response from a target.</p>
newtype Matcher = Matcher 
  { "HttpCode" :: (HttpCode)
  }


newtype Max = Max String


newtype ModifyListenerInput = ModifyListenerInput 
  { "ListenerArn" :: (ListenerArn)
  , "Port" :: NullOrUndefined (Port)
  , "Protocol" :: NullOrUndefined (ProtocolEnum)
  , "SslPolicy" :: NullOrUndefined (SslPolicyName)
  , "Certificates" :: NullOrUndefined (CertificateList)
  , "DefaultActions" :: NullOrUndefined (Actions)
  }


newtype ModifyListenerOutput = ModifyListenerOutput 
  { "Listeners" :: NullOrUndefined (Listeners)
  }


newtype ModifyLoadBalancerAttributesInput = ModifyLoadBalancerAttributesInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Attributes" :: (LoadBalancerAttributes)
  }


newtype ModifyLoadBalancerAttributesOutput = ModifyLoadBalancerAttributesOutput 
  { "Attributes" :: NullOrUndefined (LoadBalancerAttributes)
  }


newtype ModifyRuleInput = ModifyRuleInput 
  { "RuleArn" :: (RuleArn)
  , "Conditions" :: NullOrUndefined (RuleConditionList)
  , "Actions" :: NullOrUndefined (Actions)
  }


newtype ModifyRuleOutput = ModifyRuleOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }


newtype ModifyTargetGroupAttributesInput = ModifyTargetGroupAttributesInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Attributes" :: (TargetGroupAttributes)
  }


newtype ModifyTargetGroupAttributesOutput = ModifyTargetGroupAttributesOutput 
  { "Attributes" :: NullOrUndefined (TargetGroupAttributes)
  }


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


newtype ModifyTargetGroupOutput = ModifyTargetGroupOutput 
  { "TargetGroups" :: NullOrUndefined (TargetGroups)
  }


newtype Name = Name String


-- | <p>This operation is not allowed.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { 
  }


newtype PageSize = PageSize Int


newtype Path = Path String


newtype Port = Port Int


-- | <p>The specified priority is in use.</p>
newtype PriorityInUseException = PriorityInUseException 
  { 
  }


newtype ProtocolEnum = ProtocolEnum String


newtype RegisterTargetsInput = RegisterTargetsInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: (TargetDescriptions)
  }


newtype RegisterTargetsOutput = RegisterTargetsOutput 
  { 
  }


newtype RemoveListenerCertificatesInput = RemoveListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Certificates" :: (CertificateList)
  }


newtype RemoveListenerCertificatesOutput = RemoveListenerCertificatesOutput 
  { 
  }


newtype RemoveTagsInput = RemoveTagsInput 
  { "ResourceArns" :: (ResourceArns)
  , "TagKeys" :: (TagKeys)
  }


newtype RemoveTagsOutput = RemoveTagsOutput 
  { 
  }


newtype ResourceArn = ResourceArn String


newtype ResourceArns = ResourceArns (Array ResourceArn)


-- | <p>A specified resource is in use.</p>
newtype ResourceInUseException = ResourceInUseException 
  { 
  }


-- | <p>Information about a rule.</p>
newtype Rule = Rule 
  { "RuleArn" :: NullOrUndefined (RuleArn)
  , "Priority" :: NullOrUndefined (String)
  , "Conditions" :: NullOrUndefined (RuleConditionList)
  , "Actions" :: NullOrUndefined (Actions)
  , "IsDefault" :: NullOrUndefined (IsDefault)
  }


newtype RuleArn = RuleArn String


newtype RuleArns = RuleArns (Array RuleArn)


-- | <p>Information about a condition for a rule.</p>
newtype RuleCondition = RuleCondition 
  { "Field" :: NullOrUndefined (ConditionFieldName)
  , "Values" :: NullOrUndefined (ListOfString)
  }


newtype RuleConditionList = RuleConditionList (Array RuleCondition)


-- | <p>The specified rule does not exist.</p>
newtype RuleNotFoundException = RuleNotFoundException 
  { 
  }


newtype RulePriority = RulePriority Int


newtype RulePriorityList = RulePriorityList (Array RulePriorityPair)


-- | <p>Information about the priorities for the rules for a listener.</p>
newtype RulePriorityPair = RulePriorityPair 
  { "RuleArn" :: NullOrUndefined (RuleArn)
  , "Priority" :: NullOrUndefined (RulePriority)
  }


newtype Rules = Rules (Array Rule)


-- | <p>The specified SSL policy does not exist.</p>
newtype SSLPolicyNotFoundException = SSLPolicyNotFoundException 
  { 
  }


newtype SecurityGroupId = SecurityGroupId String


newtype SecurityGroups = SecurityGroups (Array SecurityGroupId)


newtype SetIpAddressTypeInput = SetIpAddressTypeInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "IpAddressType" :: (IpAddressType)
  }


newtype SetIpAddressTypeOutput = SetIpAddressTypeOutput 
  { "IpAddressType" :: NullOrUndefined (IpAddressType)
  }


newtype SetRulePrioritiesInput = SetRulePrioritiesInput 
  { "RulePriorities" :: (RulePriorityList)
  }


newtype SetRulePrioritiesOutput = SetRulePrioritiesOutput 
  { "Rules" :: NullOrUndefined (Rules)
  }


newtype SetSecurityGroupsInput = SetSecurityGroupsInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "SecurityGroups" :: (SecurityGroups)
  }


newtype SetSecurityGroupsOutput = SetSecurityGroupsOutput 
  { "SecurityGroupIds" :: NullOrUndefined (SecurityGroups)
  }


newtype SetSubnetsInput = SetSubnetsInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Subnets" :: (Subnets)
  , "SubnetMappings" :: NullOrUndefined (SubnetMappings)
  }


newtype SetSubnetsOutput = SetSubnetsOutput 
  { "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  }


newtype SslPolicies = SslPolicies (Array SslPolicy)


-- | <p>Information about a policy used for SSL negotiation.</p>
newtype SslPolicy = SslPolicy 
  { "SslProtocols" :: NullOrUndefined (SslProtocols)
  , "Ciphers" :: NullOrUndefined (Ciphers)
  , "Name" :: NullOrUndefined (SslPolicyName)
  }


newtype SslPolicyName = SslPolicyName String


newtype SslPolicyNames = SslPolicyNames (Array SslPolicyName)


newtype SslProtocol = SslProtocol String


newtype SslProtocols = SslProtocols (Array SslProtocol)


newtype StateReason = StateReason String


newtype StringValue = StringValue String


newtype SubnetId = SubnetId String


-- | <p>Information about a subnet mapping.</p>
newtype SubnetMapping = SubnetMapping 
  { "SubnetId" :: NullOrUndefined (SubnetId)
  , "AllocationId" :: NullOrUndefined (AllocationId)
  }


newtype SubnetMappings = SubnetMappings (Array SubnetMapping)


-- | <p>The specified subnet does not exist.</p>
newtype SubnetNotFoundException = SubnetNotFoundException 
  { 
  }


newtype Subnets = Subnets (Array SubnetId)


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }


-- | <p>The tags associated with a resource.</p>
newtype TagDescription = TagDescription 
  { "ResourceArn" :: NullOrUndefined (ResourceArn)
  , "Tags" :: NullOrUndefined (TagList)
  }


newtype TagDescriptions = TagDescriptions (Array TagDescription)


newtype TagKey = TagKey String


newtype TagKeys = TagKeys (Array TagKey)


newtype TagList = TagList (Array Tag)


newtype TagValue = TagValue String


-- | <p>Information about a target.</p>
newtype TargetDescription = TargetDescription 
  { "Id" :: (TargetId)
  , "Port" :: NullOrUndefined (Port)
  , "AvailabilityZone" :: NullOrUndefined (ZoneName)
  }


newtype TargetDescriptions = TargetDescriptions (Array TargetDescription)


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


newtype TargetGroupArn = TargetGroupArn String


newtype TargetGroupArns = TargetGroupArns (Array TargetGroupArn)


-- | <p>You've reached the limit on the number of load balancers per target group.</p>
newtype TargetGroupAssociationLimitException = TargetGroupAssociationLimitException 
  { 
  }


-- | <p>Information about a target group attribute.</p>
newtype TargetGroupAttribute = TargetGroupAttribute 
  { "Key" :: NullOrUndefined (TargetGroupAttributeKey)
  , "Value" :: NullOrUndefined (TargetGroupAttributeValue)
  }


newtype TargetGroupAttributeKey = TargetGroupAttributeKey String


newtype TargetGroupAttributeValue = TargetGroupAttributeValue String


newtype TargetGroupAttributes = TargetGroupAttributes (Array TargetGroupAttribute)


newtype TargetGroupName = TargetGroupName String


newtype TargetGroupNames = TargetGroupNames (Array TargetGroupName)


-- | <p>The specified target group does not exist.</p>
newtype TargetGroupNotFoundException = TargetGroupNotFoundException 
  { 
  }


newtype TargetGroups = TargetGroups (Array TargetGroup)


-- | <p>Information about the current health of a target.</p>
newtype TargetHealth = TargetHealth 
  { "State" :: NullOrUndefined (TargetHealthStateEnum)
  , "Reason" :: NullOrUndefined (TargetHealthReasonEnum)
  , "Description" :: NullOrUndefined (Description)
  }


-- | <p>Information about the health of a target.</p>
newtype TargetHealthDescription = TargetHealthDescription 
  { "Target" :: NullOrUndefined (TargetDescription)
  , "HealthCheckPort" :: NullOrUndefined (HealthCheckPort)
  , "TargetHealth" :: NullOrUndefined (TargetHealth)
  }


newtype TargetHealthDescriptions = TargetHealthDescriptions (Array TargetHealthDescription)


newtype TargetHealthReasonEnum = TargetHealthReasonEnum String


newtype TargetHealthStateEnum = TargetHealthStateEnum String


newtype TargetId = TargetId String


newtype TargetTypeEnum = TargetTypeEnum String


-- | <p>You've reached the limit on the number of certificates per load balancer.</p>
newtype TooManyCertificatesException = TooManyCertificatesException 
  { 
  }


-- | <p>You've reached the limit on the number of listeners per load balancer.</p>
newtype TooManyListenersException = TooManyListenersException 
  { 
  }


-- | <p>You've reached the limit on the number of load balancers for your AWS account.</p>
newtype TooManyLoadBalancersException = TooManyLoadBalancersException 
  { 
  }


-- | <p>You've reached the limit on the number of times a target can be registered with a load balancer.</p>
newtype TooManyRegistrationsForTargetIdException = TooManyRegistrationsForTargetIdException 
  { 
  }


-- | <p>You've reached the limit on the number of rules per load balancer.</p>
newtype TooManyRulesException = TooManyRulesException 
  { 
  }


-- | <p>You've reached the limit on the number of tags per load balancer.</p>
newtype TooManyTagsException = TooManyTagsException 
  { 
  }


-- | <p>You've reached the limit on the number of target groups for your AWS account.</p>
newtype TooManyTargetGroupsException = TooManyTargetGroupsException 
  { 
  }


-- | <p>You've reached the limit on the number of targets.</p>
newtype TooManyTargetsException = TooManyTargetsException 
  { 
  }


-- | <p>The specified protocol is not supported.</p>
newtype UnsupportedProtocolException = UnsupportedProtocolException 
  { 
  }


newtype VpcId = VpcId String


newtype ZoneName = ZoneName String
