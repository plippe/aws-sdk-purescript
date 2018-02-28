

-- | <fullname>Elastic Load Balancing</fullname> <p>A load balancer distributes incoming traffic across targets, such as your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered targets and ensures that it routes traffic only to healthy targets. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer. You configure a target group with a protocol and port number for connections from the load balancer to the targets, and with health check settings to be used when checking the health status of the targets.</p> <p>Elastic Load Balancing supports the following types of load balancers: Application Load Balancers, Network Load Balancers, and Classic Load Balancers.</p> <p>An Application Load Balancer makes routing and load balancing decisions at the application layer (HTTP/HTTPS). A Network Load Balancer makes routing and load balancing decisions at the transport layer (TCP). Both Application Load Balancers and Network Load Balancers can route requests to one or more ports on each EC2 instance or container instance in your virtual private cloud (VPC).</p> <p>A Classic Load Balancer makes routing and load balancing decisions either at the transport layer (TCP/SSL) or the application layer (HTTP/HTTPS), and supports either EC2-Classic or a VPC. For more information, see the <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/">Elastic Load Balancing User Guide</a>.</p> <p>This reference covers the 2015-12-01 API, which supports Application Load Balancers and Network Load Balancers. The 2012-06-01 API supports Classic Load Balancers.</p> <p>To get started, complete the following tasks:</p> <ol> <li> <p>Create a load balancer using <a>CreateLoadBalancer</a>.</p> </li> <li> <p>Create a target group using <a>CreateTargetGroup</a>.</p> </li> <li> <p>Register targets for the target group using <a>RegisterTargets</a>.</p> </li> <li> <p>Create one or more listeners for your load balancer using <a>CreateListener</a>.</p> </li> </ol> <p>To delete a load balancer and its related resources, complete the following tasks:</p> <ol> <li> <p>Delete the load balancer using <a>DeleteLoadBalancer</a>.</p> </li> <li> <p>Delete the target group using <a>DeleteTargetGroup</a>.</p> </li> </ol> <p>All Elastic Load Balancing operations are idempotent, which means that they complete at most one time. If you repeat an operation, it succeeds.</p>
module AWS.ELBv2 where

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

serviceName = "ELBv2" :: String


-- | <p>Adds the specified certificate to the specified secure listener.</p> <p>If the certificate was already added, the call is successful but the certificate is not added again.</p> <p>To list the certificates for your listener, use <a>DescribeListenerCertificates</a>. To remove certificates from your listener, use <a>RemoveListenerCertificates</a>.</p>
addListenerCertificates :: forall eff. AddListenerCertificatesInput -> Aff (exception :: EXCEPTION | eff) AddListenerCertificatesOutput
addListenerCertificates = Request.request serviceName "addListenerCertificates" 


-- | <p>Adds the specified tags to the specified Elastic Load Balancing resource. You can tag your Application Load Balancers, Network Load Balancers, and your target groups.</p> <p>Each tag consists of a key and an optional value. If a resource already has a tag with the same key, <code>AddTags</code> updates its value.</p> <p>To list the current tags for your resources, use <a>DescribeTags</a>. To remove tags from your resources, use <a>RemoveTags</a>.</p>
addTags :: forall eff. AddTagsInput -> Aff (exception :: EXCEPTION | eff) AddTagsOutput
addTags = Request.request serviceName "addTags" 


-- | <p>Creates a listener for the specified Application Load Balancer or Network Load Balancer.</p> <p>To update a listener, use <a>ModifyListener</a>. When you are finished with a listener, you can delete it using <a>DeleteListener</a>. If you are finished with both the listener and the load balancer, you can delete them both using <a>DeleteLoadBalancer</a>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple listeners with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html">Listeners for Your Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-listeners.html">Listeners for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>
createListener :: forall eff. CreateListenerInput -> Aff (exception :: EXCEPTION | eff) CreateListenerOutput
createListener = Request.request serviceName "createListener" 


-- | <p>Creates an Application Load Balancer or a Network Load Balancer.</p> <p>When you create a load balancer, you can specify security groups, public subnets, IP address type, and tags. Otherwise, you could do so later using <a>SetSecurityGroups</a>, <a>SetSubnets</a>, <a>SetIpAddressType</a>, and <a>AddTags</a>.</p> <p>To create listeners for your load balancer, use <a>CreateListener</a>. To describe your current load balancers, see <a>DescribeLoadBalancers</a>. When you are finished with a load balancer, you can delete it using <a>DeleteLoadBalancer</a>.</p> <p>For limit information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-limits.html">Limits for Your Application Load Balancer</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-limits.html">Limits for Your Network Load Balancer</a> in the <i>Network Load Balancers Guide</i>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple load balancers with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/application-load-balancers.html">Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/network-load-balancers.html">Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>
createLoadBalancer :: forall eff. CreateLoadBalancerInput -> Aff (exception :: EXCEPTION | eff) CreateLoadBalancerOutput
createLoadBalancer = Request.request serviceName "createLoadBalancer" 


-- | <p>Creates a rule for the specified listener. The listener must be associated with an Application Load Balancer.</p> <p>Rules are evaluated in priority order, from the lowest value to the highest value. When the condition for a rule is met, the specified action is taken. If no conditions are met, the action for the default rule is taken. For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html#listener-rules">Listener Rules</a> in the <i>Application Load Balancers Guide</i>.</p> <p>To view your current rules, use <a>DescribeRules</a>. To update a rule, use <a>ModifyRule</a>. To set the priorities of your rules, use <a>SetRulePriorities</a>. To delete a rule, use <a>DeleteRule</a>.</p>
createRule :: forall eff. CreateRuleInput -> Aff (exception :: EXCEPTION | eff) CreateRuleOutput
createRule = Request.request serviceName "createRule" 


-- | <p>Creates a target group.</p> <p>To register targets with the target group, use <a>RegisterTargets</a>. To update the health check settings for the target group, use <a>ModifyTargetGroup</a>. To monitor the health of targets in the target group, use <a>DescribeTargetHealth</a>.</p> <p>To route traffic to the targets in a target group, specify the target group in an action using <a>CreateListener</a> or <a>CreateRule</a>.</p> <p>To delete a target group, use <a>DeleteTargetGroup</a>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple target groups with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html">Target Groups for Your Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> or <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html">Target Groups for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>
createTargetGroup :: forall eff. CreateTargetGroupInput -> Aff (exception :: EXCEPTION | eff) CreateTargetGroupOutput
createTargetGroup = Request.request serviceName "createTargetGroup" 


-- | <p>Deletes the specified listener.</p> <p>Alternatively, your listener is deleted when you delete the load balancer it is attached to using <a>DeleteLoadBalancer</a>.</p>
deleteListener :: forall eff. DeleteListenerInput -> Aff (exception :: EXCEPTION | eff) DeleteListenerOutput
deleteListener = Request.request serviceName "deleteListener" 


-- | <p>Deletes the specified Application Load Balancer or Network Load Balancer and its attached listeners.</p> <p>You can't delete a load balancer if deletion protection is enabled. If the load balancer does not exist or has already been deleted, the call succeeds.</p> <p>Deleting a load balancer does not affect its registered targets. For example, your EC2 instances continue to run and are still registered to their target groups. If you no longer need these EC2 instances, you can stop or terminate them.</p>
deleteLoadBalancer :: forall eff. DeleteLoadBalancerInput -> Aff (exception :: EXCEPTION | eff) DeleteLoadBalancerOutput
deleteLoadBalancer = Request.request serviceName "deleteLoadBalancer" 


-- | <p>Deletes the specified rule.</p>
deleteRule :: forall eff. DeleteRuleInput -> Aff (exception :: EXCEPTION | eff) DeleteRuleOutput
deleteRule = Request.request serviceName "deleteRule" 


-- | <p>Deletes the specified target group.</p> <p>You can delete a target group if it is not referenced by any actions. Deleting a target group also deletes any associated health checks.</p>
deleteTargetGroup :: forall eff. DeleteTargetGroupInput -> Aff (exception :: EXCEPTION | eff) DeleteTargetGroupOutput
deleteTargetGroup = Request.request serviceName "deleteTargetGroup" 


-- | <p>Deregisters the specified targets from the specified target group. After the targets are deregistered, they no longer receive traffic from the load balancer.</p>
deregisterTargets :: forall eff. DeregisterTargetsInput -> Aff (exception :: EXCEPTION | eff) DeregisterTargetsOutput
deregisterTargets = Request.request serviceName "deregisterTargets" 


-- | <p>Describes the current Elastic Load Balancing resource limits for your AWS account.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-limits.html">Limits for Your Application Load Balancers</a> in the <i>Application Load Balancer Guide</i> or <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-limits.html">Limits for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>
describeAccountLimits :: forall eff. DescribeAccountLimitsInput -> Aff (exception :: EXCEPTION | eff) DescribeAccountLimitsOutput
describeAccountLimits = Request.request serviceName "describeAccountLimits" 


-- | <p>Describes the certificates for the specified secure listener.</p>
describeListenerCertificates :: forall eff. DescribeListenerCertificatesInput -> Aff (exception :: EXCEPTION | eff) DescribeListenerCertificatesOutput
describeListenerCertificates = Request.request serviceName "describeListenerCertificates" 


-- | <p>Describes the specified listeners or the listeners for the specified Application Load Balancer or Network Load Balancer. You must specify either a load balancer or one or more listeners.</p>
describeListeners :: forall eff. DescribeListenersInput -> Aff (exception :: EXCEPTION | eff) DescribeListenersOutput
describeListeners = Request.request serviceName "describeListeners" 


-- | <p>Describes the attributes for the specified Application Load Balancer or Network Load Balancer.</p>
describeLoadBalancerAttributes :: forall eff. DescribeLoadBalancerAttributesInput -> Aff (exception :: EXCEPTION | eff) DescribeLoadBalancerAttributesOutput
describeLoadBalancerAttributes = Request.request serviceName "describeLoadBalancerAttributes" 


-- | <p>Describes the specified load balancers or all of your load balancers.</p> <p>To describe the listeners for a load balancer, use <a>DescribeListeners</a>. To describe the attributes for a load balancer, use <a>DescribeLoadBalancerAttributes</a>.</p>
describeLoadBalancers :: forall eff. DescribeLoadBalancersInput -> Aff (exception :: EXCEPTION | eff) DescribeLoadBalancersOutput
describeLoadBalancers = Request.request serviceName "describeLoadBalancers" 


-- | <p>Describes the specified rules or the rules for the specified listener. You must specify either a listener or one or more rules.</p>
describeRules :: forall eff. DescribeRulesInput -> Aff (exception :: EXCEPTION | eff) DescribeRulesOutput
describeRules = Request.request serviceName "describeRules" 


-- | <p>Describes the specified policies or all policies used for SSL negotiation.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies">Security Policies</a> in the <i>Application Load Balancers Guide</i>.</p>
describeSSLPolicies :: forall eff. DescribeSSLPoliciesInput -> Aff (exception :: EXCEPTION | eff) DescribeSSLPoliciesOutput
describeSSLPolicies = Request.request serviceName "describeSSLPolicies" 


-- | <p>Describes the tags for the specified resources. You can describe the tags for one or more Application Load Balancers, Network Load Balancers, and target groups.</p>
describeTags :: forall eff. DescribeTagsInput -> Aff (exception :: EXCEPTION | eff) DescribeTagsOutput
describeTags = Request.request serviceName "describeTags" 


-- | <p>Describes the attributes for the specified target group.</p>
describeTargetGroupAttributes :: forall eff. DescribeTargetGroupAttributesInput -> Aff (exception :: EXCEPTION | eff) DescribeTargetGroupAttributesOutput
describeTargetGroupAttributes = Request.request serviceName "describeTargetGroupAttributes" 


-- | <p>Describes the specified target groups or all of your target groups. By default, all target groups are described. Alternatively, you can specify one of the following to filter the results: the ARN of the load balancer, the names of one or more target groups, or the ARNs of one or more target groups.</p> <p>To describe the targets for a target group, use <a>DescribeTargetHealth</a>. To describe the attributes of a target group, use <a>DescribeTargetGroupAttributes</a>.</p>
describeTargetGroups :: forall eff. DescribeTargetGroupsInput -> Aff (exception :: EXCEPTION | eff) DescribeTargetGroupsOutput
describeTargetGroups = Request.request serviceName "describeTargetGroups" 


-- | <p>Describes the health of the specified targets or all of your targets.</p>
describeTargetHealth :: forall eff. DescribeTargetHealthInput -> Aff (exception :: EXCEPTION | eff) DescribeTargetHealthOutput
describeTargetHealth = Request.request serviceName "describeTargetHealth" 


-- | <p>Modifies the specified properties of the specified listener.</p> <p>Any properties that you do not specify retain their current values. However, changing the protocol from HTTPS to HTTP removes the security policy and SSL certificate properties. If you change the protocol from HTTP to HTTPS, you must add the security policy and server certificate.</p>
modifyListener :: forall eff. ModifyListenerInput -> Aff (exception :: EXCEPTION | eff) ModifyListenerOutput
modifyListener = Request.request serviceName "modifyListener" 


-- | <p>Modifies the specified attributes of the specified Application Load Balancer or Network Load Balancer.</p> <p>If any of the specified attributes can't be modified as requested, the call fails. Any existing attributes that you do not modify retain their current values.</p>
modifyLoadBalancerAttributes :: forall eff. ModifyLoadBalancerAttributesInput -> Aff (exception :: EXCEPTION | eff) ModifyLoadBalancerAttributesOutput
modifyLoadBalancerAttributes = Request.request serviceName "modifyLoadBalancerAttributes" 


-- | <p>Modifies the specified rule.</p> <p>Any existing properties that you do not modify retain their current values.</p> <p>To modify the default action, use <a>ModifyListener</a>.</p>
modifyRule :: forall eff. ModifyRuleInput -> Aff (exception :: EXCEPTION | eff) ModifyRuleOutput
modifyRule = Request.request serviceName "modifyRule" 


-- | <p>Modifies the health checks used when evaluating the health state of the targets in the specified target group.</p> <p>To monitor the health of the targets, use <a>DescribeTargetHealth</a>.</p>
modifyTargetGroup :: forall eff. ModifyTargetGroupInput -> Aff (exception :: EXCEPTION | eff) ModifyTargetGroupOutput
modifyTargetGroup = Request.request serviceName "modifyTargetGroup" 


-- | <p>Modifies the specified attributes of the specified target group.</p>
modifyTargetGroupAttributes :: forall eff. ModifyTargetGroupAttributesInput -> Aff (exception :: EXCEPTION | eff) ModifyTargetGroupAttributesOutput
modifyTargetGroupAttributes = Request.request serviceName "modifyTargetGroupAttributes" 


-- | <p>Registers the specified targets with the specified target group.</p> <p>You can register targets by instance ID or by IP address. If the target is an EC2 instance, it must be in the <code>running</code> state when you register it.</p> <p>By default, the load balancer routes requests to registered targets using the protocol and port for the target group. Alternatively, you can override the port for a target when you register it. You can register each EC2 instance or IP address with the same target group multiple times using different ports.</p> <p>With a Network Load Balancer, you cannot register instances by instance ID if they have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1. You can register instances of these types by IP address.</p> <p>To remove a target from a target group, use <a>DeregisterTargets</a>.</p>
registerTargets :: forall eff. RegisterTargetsInput -> Aff (exception :: EXCEPTION | eff) RegisterTargetsOutput
registerTargets = Request.request serviceName "registerTargets" 


-- | <p>Removes the specified certificate from the specified secure listener.</p> <p>You can't remove the default certificate for a listener. To replace the default certificate, call <a>ModifyListener</a>.</p> <p>To list the certificates for your listener, use <a>DescribeListenerCertificates</a>.</p>
removeListenerCertificates :: forall eff. RemoveListenerCertificatesInput -> Aff (exception :: EXCEPTION | eff) RemoveListenerCertificatesOutput
removeListenerCertificates = Request.request serviceName "removeListenerCertificates" 


-- | <p>Removes the specified tags from the specified Elastic Load Balancing resource.</p> <p>To list the current tags for your resources, use <a>DescribeTags</a>.</p>
removeTags :: forall eff. RemoveTagsInput -> Aff (exception :: EXCEPTION | eff) RemoveTagsOutput
removeTags = Request.request serviceName "removeTags" 


-- | <p>Sets the type of IP addresses used by the subnets of the specified Application Load Balancer or Network Load Balancer.</p> <p>Note that Network Load Balancers must use <code>ipv4</code>.</p>
setIpAddressType :: forall eff. SetIpAddressTypeInput -> Aff (exception :: EXCEPTION | eff) SetIpAddressTypeOutput
setIpAddressType = Request.request serviceName "setIpAddressType" 


-- | <p>Sets the priorities of the specified rules.</p> <p>You can reorder the rules as long as there are no priority conflicts in the new order. Any existing rules that you do not specify retain their current priority.</p>
setRulePriorities :: forall eff. SetRulePrioritiesInput -> Aff (exception :: EXCEPTION | eff) SetRulePrioritiesOutput
setRulePriorities = Request.request serviceName "setRulePriorities" 


-- | <p>Associates the specified security groups with the specified Application Load Balancer. The specified security groups override the previously associated security groups.</p> <p>Note that you can't specify a security group for a Network Load Balancer.</p>
setSecurityGroups :: forall eff. SetSecurityGroupsInput -> Aff (exception :: EXCEPTION | eff) SetSecurityGroupsOutput
setSecurityGroups = Request.request serviceName "setSecurityGroups" 


-- | <p>Enables the Availability Zone for the specified public subnets for the specified Application Load Balancer. The specified subnets replace the previously enabled subnets.</p> <p>Note that you can't change the subnets for a Network Load Balancer.</p>
setSubnets :: forall eff. SetSubnetsInput -> Aff (exception :: EXCEPTION | eff) SetSubnetsOutput
setSubnets = Request.request serviceName "setSubnets" 


-- | <p>Information about an action.</p>
newtype Action = Action 
  { "Type" :: (ActionTypeEnum)
  , "TargetGroupArn" :: (TargetGroupArn)
  }
derive instance newtypeAction :: Newtype Action _
derive instance repGenericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
instance decodeAction :: Decode Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAction :: Encode Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionTypeEnum = ActionTypeEnum String
derive instance newtypeActionTypeEnum :: Newtype ActionTypeEnum _
derive instance repGenericActionTypeEnum :: Generic ActionTypeEnum _
instance showActionTypeEnum :: Show ActionTypeEnum where
  show = genericShow
instance decodeActionTypeEnum :: Decode ActionTypeEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionTypeEnum :: Encode ActionTypeEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Actions = Actions (Array Action)
derive instance newtypeActions :: Newtype Actions _
derive instance repGenericActions :: Generic Actions _
instance showActions :: Show Actions where
  show = genericShow
instance decodeActions :: Decode Actions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActions :: Encode Actions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddListenerCertificatesInput = AddListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Certificates" :: (CertificateList)
  }
derive instance newtypeAddListenerCertificatesInput :: Newtype AddListenerCertificatesInput _
derive instance repGenericAddListenerCertificatesInput :: Generic AddListenerCertificatesInput _
instance showAddListenerCertificatesInput :: Show AddListenerCertificatesInput where
  show = genericShow
instance decodeAddListenerCertificatesInput :: Decode AddListenerCertificatesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddListenerCertificatesInput :: Encode AddListenerCertificatesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddListenerCertificatesOutput = AddListenerCertificatesOutput 
  { "Certificates" :: NullOrUndefined.NullOrUndefined (CertificateList)
  }
derive instance newtypeAddListenerCertificatesOutput :: Newtype AddListenerCertificatesOutput _
derive instance repGenericAddListenerCertificatesOutput :: Generic AddListenerCertificatesOutput _
instance showAddListenerCertificatesOutput :: Show AddListenerCertificatesOutput where
  show = genericShow
instance decodeAddListenerCertificatesOutput :: Decode AddListenerCertificatesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddListenerCertificatesOutput :: Encode AddListenerCertificatesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsInput = AddTagsInput 
  { "ResourceArns" :: (ResourceArns)
  , "Tags" :: (TagList)
  }
derive instance newtypeAddTagsInput :: Newtype AddTagsInput _
derive instance repGenericAddTagsInput :: Generic AddTagsInput _
instance showAddTagsInput :: Show AddTagsInput where
  show = genericShow
instance decodeAddTagsInput :: Decode AddTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsInput :: Encode AddTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsOutput = AddTagsOutput Types.NoArguments
derive instance newtypeAddTagsOutput :: Newtype AddTagsOutput _
derive instance repGenericAddTagsOutput :: Generic AddTagsOutput _
instance showAddTagsOutput :: Show AddTagsOutput where
  show = genericShow
instance decodeAddTagsOutput :: Decode AddTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsOutput :: Encode AddTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AllocationId = AllocationId String
derive instance newtypeAllocationId :: Newtype AllocationId _
derive instance repGenericAllocationId :: Generic AllocationId _
instance showAllocationId :: Show AllocationId where
  show = genericShow
instance decodeAllocationId :: Decode AllocationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllocationId :: Encode AllocationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified allocation ID does not exist.</p>
newtype AllocationIdNotFoundException = AllocationIdNotFoundException Types.NoArguments
derive instance newtypeAllocationIdNotFoundException :: Newtype AllocationIdNotFoundException _
derive instance repGenericAllocationIdNotFoundException :: Generic AllocationIdNotFoundException _
instance showAllocationIdNotFoundException :: Show AllocationIdNotFoundException where
  show = genericShow
instance decodeAllocationIdNotFoundException :: Decode AllocationIdNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAllocationIdNotFoundException :: Encode AllocationIdNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an Availability Zone.</p>
newtype AvailabilityZone = AvailabilityZone 
  { "ZoneName" :: NullOrUndefined.NullOrUndefined (ZoneName)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (SubnetId)
  , "LoadBalancerAddresses" :: NullOrUndefined.NullOrUndefined (LoadBalancerAddresses)
  }
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _
derive instance repGenericAvailabilityZone :: Generic AvailabilityZone _
instance showAvailabilityZone :: Show AvailabilityZone where
  show = genericShow
instance decodeAvailabilityZone :: Decode AvailabilityZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZone :: Encode AvailabilityZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified Availability Zone is not supported.</p>
newtype AvailabilityZoneNotSupportedException = AvailabilityZoneNotSupportedException Types.NoArguments
derive instance newtypeAvailabilityZoneNotSupportedException :: Newtype AvailabilityZoneNotSupportedException _
derive instance repGenericAvailabilityZoneNotSupportedException :: Generic AvailabilityZoneNotSupportedException _
instance showAvailabilityZoneNotSupportedException :: Show AvailabilityZoneNotSupportedException where
  show = genericShow
instance decodeAvailabilityZoneNotSupportedException :: Decode AvailabilityZoneNotSupportedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZoneNotSupportedException :: Encode AvailabilityZoneNotSupportedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZones = AvailabilityZones (Array AvailabilityZone)
derive instance newtypeAvailabilityZones :: Newtype AvailabilityZones _
derive instance repGenericAvailabilityZones :: Generic AvailabilityZones _
instance showAvailabilityZones :: Show AvailabilityZones where
  show = genericShow
instance decodeAvailabilityZones :: Decode AvailabilityZones where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZones :: Encode AvailabilityZones where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CanonicalHostedZoneId = CanonicalHostedZoneId String
derive instance newtypeCanonicalHostedZoneId :: Newtype CanonicalHostedZoneId _
derive instance repGenericCanonicalHostedZoneId :: Generic CanonicalHostedZoneId _
instance showCanonicalHostedZoneId :: Show CanonicalHostedZoneId where
  show = genericShow
instance decodeCanonicalHostedZoneId :: Decode CanonicalHostedZoneId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCanonicalHostedZoneId :: Encode CanonicalHostedZoneId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an SSL server certificate.</p>
newtype Certificate = Certificate 
  { "CertificateArn" :: NullOrUndefined.NullOrUndefined (CertificateArn)
  , "IsDefault" :: NullOrUndefined.NullOrUndefined (Default)
  }
derive instance newtypeCertificate :: Newtype Certificate _
derive instance repGenericCertificate :: Generic Certificate _
instance showCertificate :: Show Certificate where
  show = genericShow
instance decodeCertificate :: Decode Certificate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificate :: Encode Certificate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateArn = CertificateArn String
derive instance newtypeCertificateArn :: Newtype CertificateArn _
derive instance repGenericCertificateArn :: Generic CertificateArn _
instance showCertificateArn :: Show CertificateArn where
  show = genericShow
instance decodeCertificateArn :: Decode CertificateArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateArn :: Encode CertificateArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CertificateList = CertificateList (Array Certificate)
derive instance newtypeCertificateList :: Newtype CertificateList _
derive instance repGenericCertificateList :: Generic CertificateList _
instance showCertificateList :: Show CertificateList where
  show = genericShow
instance decodeCertificateList :: Decode CertificateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateList :: Encode CertificateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified certificate does not exist.</p>
newtype CertificateNotFoundException = CertificateNotFoundException Types.NoArguments
derive instance newtypeCertificateNotFoundException :: Newtype CertificateNotFoundException _
derive instance repGenericCertificateNotFoundException :: Generic CertificateNotFoundException _
instance showCertificateNotFoundException :: Show CertificateNotFoundException where
  show = genericShow
instance decodeCertificateNotFoundException :: Decode CertificateNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateNotFoundException :: Encode CertificateNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a cipher used in a policy.</p>
newtype Cipher = Cipher 
  { "Name" :: NullOrUndefined.NullOrUndefined (CipherName)
  , "Priority" :: NullOrUndefined.NullOrUndefined (CipherPriority)
  }
derive instance newtypeCipher :: Newtype Cipher _
derive instance repGenericCipher :: Generic Cipher _
instance showCipher :: Show Cipher where
  show = genericShow
instance decodeCipher :: Decode Cipher where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCipher :: Encode Cipher where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CipherName = CipherName String
derive instance newtypeCipherName :: Newtype CipherName _
derive instance repGenericCipherName :: Generic CipherName _
instance showCipherName :: Show CipherName where
  show = genericShow
instance decodeCipherName :: Decode CipherName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCipherName :: Encode CipherName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CipherPriority = CipherPriority Int
derive instance newtypeCipherPriority :: Newtype CipherPriority _
derive instance repGenericCipherPriority :: Generic CipherPriority _
instance showCipherPriority :: Show CipherPriority where
  show = genericShow
instance decodeCipherPriority :: Decode CipherPriority where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCipherPriority :: Encode CipherPriority where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Ciphers = Ciphers (Array Cipher)
derive instance newtypeCiphers :: Newtype Ciphers _
derive instance repGenericCiphers :: Generic Ciphers _
instance showCiphers :: Show Ciphers where
  show = genericShow
instance decodeCiphers :: Decode Ciphers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCiphers :: Encode Ciphers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConditionFieldName = ConditionFieldName String
derive instance newtypeConditionFieldName :: Newtype ConditionFieldName _
derive instance repGenericConditionFieldName :: Generic ConditionFieldName _
instance showConditionFieldName :: Show ConditionFieldName where
  show = genericShow
instance decodeConditionFieldName :: Decode ConditionFieldName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConditionFieldName :: Encode ConditionFieldName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateListenerInput = CreateListenerInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Protocol" :: (ProtocolEnum)
  , "Port" :: (Port)
  , "SslPolicy" :: NullOrUndefined.NullOrUndefined (SslPolicyName)
  , "Certificates" :: NullOrUndefined.NullOrUndefined (CertificateList)
  , "DefaultActions" :: (Actions)
  }
derive instance newtypeCreateListenerInput :: Newtype CreateListenerInput _
derive instance repGenericCreateListenerInput :: Generic CreateListenerInput _
instance showCreateListenerInput :: Show CreateListenerInput where
  show = genericShow
instance decodeCreateListenerInput :: Decode CreateListenerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateListenerInput :: Encode CreateListenerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateListenerOutput = CreateListenerOutput 
  { "Listeners" :: NullOrUndefined.NullOrUndefined (Listeners)
  }
derive instance newtypeCreateListenerOutput :: Newtype CreateListenerOutput _
derive instance repGenericCreateListenerOutput :: Generic CreateListenerOutput _
instance showCreateListenerOutput :: Show CreateListenerOutput where
  show = genericShow
instance decodeCreateListenerOutput :: Decode CreateListenerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateListenerOutput :: Encode CreateListenerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoadBalancerInput = CreateLoadBalancerInput 
  { "Name" :: (LoadBalancerName)
  , "Subnets" :: NullOrUndefined.NullOrUndefined (Subnets)
  , "SubnetMappings" :: NullOrUndefined.NullOrUndefined (SubnetMappings)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  , "Scheme" :: NullOrUndefined.NullOrUndefined (LoadBalancerSchemeEnum)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "Type" :: NullOrUndefined.NullOrUndefined (LoadBalancerTypeEnum)
  , "IpAddressType" :: NullOrUndefined.NullOrUndefined (IpAddressType)
  }
derive instance newtypeCreateLoadBalancerInput :: Newtype CreateLoadBalancerInput _
derive instance repGenericCreateLoadBalancerInput :: Generic CreateLoadBalancerInput _
instance showCreateLoadBalancerInput :: Show CreateLoadBalancerInput where
  show = genericShow
instance decodeCreateLoadBalancerInput :: Decode CreateLoadBalancerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerInput :: Encode CreateLoadBalancerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLoadBalancerOutput = CreateLoadBalancerOutput 
  { "LoadBalancers" :: NullOrUndefined.NullOrUndefined (LoadBalancers)
  }
derive instance newtypeCreateLoadBalancerOutput :: Newtype CreateLoadBalancerOutput _
derive instance repGenericCreateLoadBalancerOutput :: Generic CreateLoadBalancerOutput _
instance showCreateLoadBalancerOutput :: Show CreateLoadBalancerOutput where
  show = genericShow
instance decodeCreateLoadBalancerOutput :: Decode CreateLoadBalancerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerOutput :: Encode CreateLoadBalancerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRuleInput = CreateRuleInput 
  { "ListenerArn" :: (ListenerArn)
  , "Conditions" :: (RuleConditionList)
  , "Priority" :: (RulePriority)
  , "Actions" :: (Actions)
  }
derive instance newtypeCreateRuleInput :: Newtype CreateRuleInput _
derive instance repGenericCreateRuleInput :: Generic CreateRuleInput _
instance showCreateRuleInput :: Show CreateRuleInput where
  show = genericShow
instance decodeCreateRuleInput :: Decode CreateRuleInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRuleInput :: Encode CreateRuleInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateRuleOutput = CreateRuleOutput 
  { "Rules" :: NullOrUndefined.NullOrUndefined (Rules)
  }
derive instance newtypeCreateRuleOutput :: Newtype CreateRuleOutput _
derive instance repGenericCreateRuleOutput :: Generic CreateRuleOutput _
instance showCreateRuleOutput :: Show CreateRuleOutput where
  show = genericShow
instance decodeCreateRuleOutput :: Decode CreateRuleOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateRuleOutput :: Encode CreateRuleOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTargetGroupInput = CreateTargetGroupInput 
  { "Name" :: (TargetGroupName)
  , "Protocol" :: (ProtocolEnum)
  , "Port" :: (Port)
  , "VpcId" :: (VpcId)
  , "HealthCheckProtocol" :: NullOrUndefined.NullOrUndefined (ProtocolEnum)
  , "HealthCheckPort" :: NullOrUndefined.NullOrUndefined (HealthCheckPort)
  , "HealthCheckPath" :: NullOrUndefined.NullOrUndefined (Path)
  , "HealthCheckIntervalSeconds" :: NullOrUndefined.NullOrUndefined (HealthCheckIntervalSeconds)
  , "HealthCheckTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (HealthCheckTimeoutSeconds)
  , "HealthyThresholdCount" :: NullOrUndefined.NullOrUndefined (HealthCheckThresholdCount)
  , "UnhealthyThresholdCount" :: NullOrUndefined.NullOrUndefined (HealthCheckThresholdCount)
  , "Matcher" :: NullOrUndefined.NullOrUndefined (Matcher)
  , "TargetType" :: NullOrUndefined.NullOrUndefined (TargetTypeEnum)
  }
derive instance newtypeCreateTargetGroupInput :: Newtype CreateTargetGroupInput _
derive instance repGenericCreateTargetGroupInput :: Generic CreateTargetGroupInput _
instance showCreateTargetGroupInput :: Show CreateTargetGroupInput where
  show = genericShow
instance decodeCreateTargetGroupInput :: Decode CreateTargetGroupInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTargetGroupInput :: Encode CreateTargetGroupInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTargetGroupOutput = CreateTargetGroupOutput 
  { "TargetGroups" :: NullOrUndefined.NullOrUndefined (TargetGroups)
  }
derive instance newtypeCreateTargetGroupOutput :: Newtype CreateTargetGroupOutput _
derive instance repGenericCreateTargetGroupOutput :: Generic CreateTargetGroupOutput _
instance showCreateTargetGroupOutput :: Show CreateTargetGroupOutput where
  show = genericShow
instance decodeCreateTargetGroupOutput :: Decode CreateTargetGroupOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTargetGroupOutput :: Encode CreateTargetGroupOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatedTime = CreatedTime Number
derive instance newtypeCreatedTime :: Newtype CreatedTime _
derive instance repGenericCreatedTime :: Generic CreatedTime _
instance showCreatedTime :: Show CreatedTime where
  show = genericShow
instance decodeCreatedTime :: Decode CreatedTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatedTime :: Encode CreatedTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DNSName = DNSName String
derive instance newtypeDNSName :: Newtype DNSName _
derive instance repGenericDNSName :: Generic DNSName _
instance showDNSName :: Show DNSName where
  show = genericShow
instance decodeDNSName :: Decode DNSName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDNSName :: Encode DNSName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Default = Default Boolean
derive instance newtypeDefault :: Newtype Default _
derive instance repGenericDefault :: Generic Default _
instance showDefault :: Show Default where
  show = genericShow
instance decodeDefault :: Decode Default where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefault :: Encode Default where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteListenerInput = DeleteListenerInput 
  { "ListenerArn" :: (ListenerArn)
  }
derive instance newtypeDeleteListenerInput :: Newtype DeleteListenerInput _
derive instance repGenericDeleteListenerInput :: Generic DeleteListenerInput _
instance showDeleteListenerInput :: Show DeleteListenerInput where
  show = genericShow
instance decodeDeleteListenerInput :: Decode DeleteListenerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteListenerInput :: Encode DeleteListenerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteListenerOutput = DeleteListenerOutput Types.NoArguments
derive instance newtypeDeleteListenerOutput :: Newtype DeleteListenerOutput _
derive instance repGenericDeleteListenerOutput :: Generic DeleteListenerOutput _
instance showDeleteListenerOutput :: Show DeleteListenerOutput where
  show = genericShow
instance decodeDeleteListenerOutput :: Decode DeleteListenerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteListenerOutput :: Encode DeleteListenerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLoadBalancerInput = DeleteLoadBalancerInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  }
derive instance newtypeDeleteLoadBalancerInput :: Newtype DeleteLoadBalancerInput _
derive instance repGenericDeleteLoadBalancerInput :: Generic DeleteLoadBalancerInput _
instance showDeleteLoadBalancerInput :: Show DeleteLoadBalancerInput where
  show = genericShow
instance decodeDeleteLoadBalancerInput :: Decode DeleteLoadBalancerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerInput :: Encode DeleteLoadBalancerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLoadBalancerOutput = DeleteLoadBalancerOutput Types.NoArguments
derive instance newtypeDeleteLoadBalancerOutput :: Newtype DeleteLoadBalancerOutput _
derive instance repGenericDeleteLoadBalancerOutput :: Generic DeleteLoadBalancerOutput _
instance showDeleteLoadBalancerOutput :: Show DeleteLoadBalancerOutput where
  show = genericShow
instance decodeDeleteLoadBalancerOutput :: Decode DeleteLoadBalancerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerOutput :: Encode DeleteLoadBalancerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRuleInput = DeleteRuleInput 
  { "RuleArn" :: (RuleArn)
  }
derive instance newtypeDeleteRuleInput :: Newtype DeleteRuleInput _
derive instance repGenericDeleteRuleInput :: Generic DeleteRuleInput _
instance showDeleteRuleInput :: Show DeleteRuleInput where
  show = genericShow
instance decodeDeleteRuleInput :: Decode DeleteRuleInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRuleInput :: Encode DeleteRuleInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteRuleOutput = DeleteRuleOutput Types.NoArguments
derive instance newtypeDeleteRuleOutput :: Newtype DeleteRuleOutput _
derive instance repGenericDeleteRuleOutput :: Generic DeleteRuleOutput _
instance showDeleteRuleOutput :: Show DeleteRuleOutput where
  show = genericShow
instance decodeDeleteRuleOutput :: Decode DeleteRuleOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteRuleOutput :: Encode DeleteRuleOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTargetGroupInput = DeleteTargetGroupInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  }
derive instance newtypeDeleteTargetGroupInput :: Newtype DeleteTargetGroupInput _
derive instance repGenericDeleteTargetGroupInput :: Generic DeleteTargetGroupInput _
instance showDeleteTargetGroupInput :: Show DeleteTargetGroupInput where
  show = genericShow
instance decodeDeleteTargetGroupInput :: Decode DeleteTargetGroupInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTargetGroupInput :: Encode DeleteTargetGroupInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTargetGroupOutput = DeleteTargetGroupOutput Types.NoArguments
derive instance newtypeDeleteTargetGroupOutput :: Newtype DeleteTargetGroupOutput _
derive instance repGenericDeleteTargetGroupOutput :: Generic DeleteTargetGroupOutput _
instance showDeleteTargetGroupOutput :: Show DeleteTargetGroupOutput where
  show = genericShow
instance decodeDeleteTargetGroupOutput :: Decode DeleteTargetGroupOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTargetGroupOutput :: Encode DeleteTargetGroupOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterTargetsInput = DeregisterTargetsInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: (TargetDescriptions)
  }
derive instance newtypeDeregisterTargetsInput :: Newtype DeregisterTargetsInput _
derive instance repGenericDeregisterTargetsInput :: Generic DeregisterTargetsInput _
instance showDeregisterTargetsInput :: Show DeregisterTargetsInput where
  show = genericShow
instance decodeDeregisterTargetsInput :: Decode DeregisterTargetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterTargetsInput :: Encode DeregisterTargetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeregisterTargetsOutput = DeregisterTargetsOutput Types.NoArguments
derive instance newtypeDeregisterTargetsOutput :: Newtype DeregisterTargetsOutput _
derive instance repGenericDeregisterTargetsOutput :: Generic DeregisterTargetsOutput _
instance showDeregisterTargetsOutput :: Show DeregisterTargetsOutput where
  show = genericShow
instance decodeDeregisterTargetsOutput :: Decode DeregisterTargetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterTargetsOutput :: Encode DeregisterTargetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAccountLimitsInput = DescribeAccountLimitsInput 
  { "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeAccountLimitsInput :: Newtype DescribeAccountLimitsInput _
derive instance repGenericDescribeAccountLimitsInput :: Generic DescribeAccountLimitsInput _
instance showDescribeAccountLimitsInput :: Show DescribeAccountLimitsInput where
  show = genericShow
instance decodeDescribeAccountLimitsInput :: Decode DescribeAccountLimitsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAccountLimitsInput :: Encode DescribeAccountLimitsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAccountLimitsOutput = DescribeAccountLimitsOutput 
  { "Limits" :: NullOrUndefined.NullOrUndefined (Limits)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeAccountLimitsOutput :: Newtype DescribeAccountLimitsOutput _
derive instance repGenericDescribeAccountLimitsOutput :: Generic DescribeAccountLimitsOutput _
instance showDescribeAccountLimitsOutput :: Show DescribeAccountLimitsOutput where
  show = genericShow
instance decodeDescribeAccountLimitsOutput :: Decode DescribeAccountLimitsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAccountLimitsOutput :: Encode DescribeAccountLimitsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeListenerCertificatesInput = DescribeListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeListenerCertificatesInput :: Newtype DescribeListenerCertificatesInput _
derive instance repGenericDescribeListenerCertificatesInput :: Generic DescribeListenerCertificatesInput _
instance showDescribeListenerCertificatesInput :: Show DescribeListenerCertificatesInput where
  show = genericShow
instance decodeDescribeListenerCertificatesInput :: Decode DescribeListenerCertificatesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeListenerCertificatesInput :: Encode DescribeListenerCertificatesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeListenerCertificatesOutput = DescribeListenerCertificatesOutput 
  { "Certificates" :: NullOrUndefined.NullOrUndefined (CertificateList)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeListenerCertificatesOutput :: Newtype DescribeListenerCertificatesOutput _
derive instance repGenericDescribeListenerCertificatesOutput :: Generic DescribeListenerCertificatesOutput _
instance showDescribeListenerCertificatesOutput :: Show DescribeListenerCertificatesOutput where
  show = genericShow
instance decodeDescribeListenerCertificatesOutput :: Decode DescribeListenerCertificatesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeListenerCertificatesOutput :: Encode DescribeListenerCertificatesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeListenersInput = DescribeListenersInput 
  { "LoadBalancerArn" :: NullOrUndefined.NullOrUndefined (LoadBalancerArn)
  , "ListenerArns" :: NullOrUndefined.NullOrUndefined (ListenerArns)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeListenersInput :: Newtype DescribeListenersInput _
derive instance repGenericDescribeListenersInput :: Generic DescribeListenersInput _
instance showDescribeListenersInput :: Show DescribeListenersInput where
  show = genericShow
instance decodeDescribeListenersInput :: Decode DescribeListenersInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeListenersInput :: Encode DescribeListenersInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeListenersOutput = DescribeListenersOutput 
  { "Listeners" :: NullOrUndefined.NullOrUndefined (Listeners)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeListenersOutput :: Newtype DescribeListenersOutput _
derive instance repGenericDescribeListenersOutput :: Generic DescribeListenersOutput _
instance showDescribeListenersOutput :: Show DescribeListenersOutput where
  show = genericShow
instance decodeDescribeListenersOutput :: Decode DescribeListenersOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeListenersOutput :: Encode DescribeListenersOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLoadBalancerAttributesInput = DescribeLoadBalancerAttributesInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  }
derive instance newtypeDescribeLoadBalancerAttributesInput :: Newtype DescribeLoadBalancerAttributesInput _
derive instance repGenericDescribeLoadBalancerAttributesInput :: Generic DescribeLoadBalancerAttributesInput _
instance showDescribeLoadBalancerAttributesInput :: Show DescribeLoadBalancerAttributesInput where
  show = genericShow
instance decodeDescribeLoadBalancerAttributesInput :: Decode DescribeLoadBalancerAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancerAttributesInput :: Encode DescribeLoadBalancerAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLoadBalancerAttributesOutput = DescribeLoadBalancerAttributesOutput 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (LoadBalancerAttributes)
  }
derive instance newtypeDescribeLoadBalancerAttributesOutput :: Newtype DescribeLoadBalancerAttributesOutput _
derive instance repGenericDescribeLoadBalancerAttributesOutput :: Generic DescribeLoadBalancerAttributesOutput _
instance showDescribeLoadBalancerAttributesOutput :: Show DescribeLoadBalancerAttributesOutput where
  show = genericShow
instance decodeDescribeLoadBalancerAttributesOutput :: Decode DescribeLoadBalancerAttributesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancerAttributesOutput :: Encode DescribeLoadBalancerAttributesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLoadBalancersInput = DescribeLoadBalancersInput 
  { "LoadBalancerArns" :: NullOrUndefined.NullOrUndefined (LoadBalancerArns)
  , "Names" :: NullOrUndefined.NullOrUndefined (LoadBalancerNames)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeLoadBalancersInput :: Newtype DescribeLoadBalancersInput _
derive instance repGenericDescribeLoadBalancersInput :: Generic DescribeLoadBalancersInput _
instance showDescribeLoadBalancersInput :: Show DescribeLoadBalancersInput where
  show = genericShow
instance decodeDescribeLoadBalancersInput :: Decode DescribeLoadBalancersInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancersInput :: Encode DescribeLoadBalancersInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeLoadBalancersOutput = DescribeLoadBalancersOutput 
  { "LoadBalancers" :: NullOrUndefined.NullOrUndefined (LoadBalancers)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeLoadBalancersOutput :: Newtype DescribeLoadBalancersOutput _
derive instance repGenericDescribeLoadBalancersOutput :: Generic DescribeLoadBalancersOutput _
instance showDescribeLoadBalancersOutput :: Show DescribeLoadBalancersOutput where
  show = genericShow
instance decodeDescribeLoadBalancersOutput :: Decode DescribeLoadBalancersOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancersOutput :: Encode DescribeLoadBalancersOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRulesInput = DescribeRulesInput 
  { "ListenerArn" :: NullOrUndefined.NullOrUndefined (ListenerArn)
  , "RuleArns" :: NullOrUndefined.NullOrUndefined (RuleArns)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeRulesInput :: Newtype DescribeRulesInput _
derive instance repGenericDescribeRulesInput :: Generic DescribeRulesInput _
instance showDescribeRulesInput :: Show DescribeRulesInput where
  show = genericShow
instance decodeDescribeRulesInput :: Decode DescribeRulesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRulesInput :: Encode DescribeRulesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRulesOutput = DescribeRulesOutput 
  { "Rules" :: NullOrUndefined.NullOrUndefined (Rules)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeRulesOutput :: Newtype DescribeRulesOutput _
derive instance repGenericDescribeRulesOutput :: Generic DescribeRulesOutput _
instance showDescribeRulesOutput :: Show DescribeRulesOutput where
  show = genericShow
instance decodeDescribeRulesOutput :: Decode DescribeRulesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRulesOutput :: Encode DescribeRulesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeSSLPoliciesInput = DescribeSSLPoliciesInput 
  { "Names" :: NullOrUndefined.NullOrUndefined (SslPolicyNames)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeSSLPoliciesInput :: Newtype DescribeSSLPoliciesInput _
derive instance repGenericDescribeSSLPoliciesInput :: Generic DescribeSSLPoliciesInput _
instance showDescribeSSLPoliciesInput :: Show DescribeSSLPoliciesInput where
  show = genericShow
instance decodeDescribeSSLPoliciesInput :: Decode DescribeSSLPoliciesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSSLPoliciesInput :: Encode DescribeSSLPoliciesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeSSLPoliciesOutput = DescribeSSLPoliciesOutput 
  { "SslPolicies" :: NullOrUndefined.NullOrUndefined (SslPolicies)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeSSLPoliciesOutput :: Newtype DescribeSSLPoliciesOutput _
derive instance repGenericDescribeSSLPoliciesOutput :: Generic DescribeSSLPoliciesOutput _
instance showDescribeSSLPoliciesOutput :: Show DescribeSSLPoliciesOutput where
  show = genericShow
instance decodeDescribeSSLPoliciesOutput :: Decode DescribeSSLPoliciesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSSLPoliciesOutput :: Encode DescribeSSLPoliciesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTagsInput = DescribeTagsInput 
  { "ResourceArns" :: (ResourceArns)
  }
derive instance newtypeDescribeTagsInput :: Newtype DescribeTagsInput _
derive instance repGenericDescribeTagsInput :: Generic DescribeTagsInput _
instance showDescribeTagsInput :: Show DescribeTagsInput where
  show = genericShow
instance decodeDescribeTagsInput :: Decode DescribeTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsInput :: Encode DescribeTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTagsOutput = DescribeTagsOutput 
  { "TagDescriptions" :: NullOrUndefined.NullOrUndefined (TagDescriptions)
  }
derive instance newtypeDescribeTagsOutput :: Newtype DescribeTagsOutput _
derive instance repGenericDescribeTagsOutput :: Generic DescribeTagsOutput _
instance showDescribeTagsOutput :: Show DescribeTagsOutput where
  show = genericShow
instance decodeDescribeTagsOutput :: Decode DescribeTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsOutput :: Encode DescribeTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTargetGroupAttributesInput = DescribeTargetGroupAttributesInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  }
derive instance newtypeDescribeTargetGroupAttributesInput :: Newtype DescribeTargetGroupAttributesInput _
derive instance repGenericDescribeTargetGroupAttributesInput :: Generic DescribeTargetGroupAttributesInput _
instance showDescribeTargetGroupAttributesInput :: Show DescribeTargetGroupAttributesInput where
  show = genericShow
instance decodeDescribeTargetGroupAttributesInput :: Decode DescribeTargetGroupAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTargetGroupAttributesInput :: Encode DescribeTargetGroupAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTargetGroupAttributesOutput = DescribeTargetGroupAttributesOutput 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (TargetGroupAttributes)
  }
derive instance newtypeDescribeTargetGroupAttributesOutput :: Newtype DescribeTargetGroupAttributesOutput _
derive instance repGenericDescribeTargetGroupAttributesOutput :: Generic DescribeTargetGroupAttributesOutput _
instance showDescribeTargetGroupAttributesOutput :: Show DescribeTargetGroupAttributesOutput where
  show = genericShow
instance decodeDescribeTargetGroupAttributesOutput :: Decode DescribeTargetGroupAttributesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTargetGroupAttributesOutput :: Encode DescribeTargetGroupAttributesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTargetGroupsInput = DescribeTargetGroupsInput 
  { "LoadBalancerArn" :: NullOrUndefined.NullOrUndefined (LoadBalancerArn)
  , "TargetGroupArns" :: NullOrUndefined.NullOrUndefined (TargetGroupArns)
  , "Names" :: NullOrUndefined.NullOrUndefined (TargetGroupNames)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeTargetGroupsInput :: Newtype DescribeTargetGroupsInput _
derive instance repGenericDescribeTargetGroupsInput :: Generic DescribeTargetGroupsInput _
instance showDescribeTargetGroupsInput :: Show DescribeTargetGroupsInput where
  show = genericShow
instance decodeDescribeTargetGroupsInput :: Decode DescribeTargetGroupsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTargetGroupsInput :: Encode DescribeTargetGroupsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTargetGroupsOutput = DescribeTargetGroupsOutput 
  { "TargetGroups" :: NullOrUndefined.NullOrUndefined (TargetGroups)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeTargetGroupsOutput :: Newtype DescribeTargetGroupsOutput _
derive instance repGenericDescribeTargetGroupsOutput :: Generic DescribeTargetGroupsOutput _
instance showDescribeTargetGroupsOutput :: Show DescribeTargetGroupsOutput where
  show = genericShow
instance decodeDescribeTargetGroupsOutput :: Decode DescribeTargetGroupsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTargetGroupsOutput :: Encode DescribeTargetGroupsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTargetHealthInput = DescribeTargetHealthInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: NullOrUndefined.NullOrUndefined (TargetDescriptions)
  }
derive instance newtypeDescribeTargetHealthInput :: Newtype DescribeTargetHealthInput _
derive instance repGenericDescribeTargetHealthInput :: Generic DescribeTargetHealthInput _
instance showDescribeTargetHealthInput :: Show DescribeTargetHealthInput where
  show = genericShow
instance decodeDescribeTargetHealthInput :: Decode DescribeTargetHealthInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTargetHealthInput :: Encode DescribeTargetHealthInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTargetHealthOutput = DescribeTargetHealthOutput 
  { "TargetHealthDescriptions" :: NullOrUndefined.NullOrUndefined (TargetHealthDescriptions)
  }
derive instance newtypeDescribeTargetHealthOutput :: Newtype DescribeTargetHealthOutput _
derive instance repGenericDescribeTargetHealthOutput :: Generic DescribeTargetHealthOutput _
instance showDescribeTargetHealthOutput :: Show DescribeTargetHealthOutput where
  show = genericShow
instance decodeDescribeTargetHealthOutput :: Decode DescribeTargetHealthOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTargetHealthOutput :: Encode DescribeTargetHealthOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A listener with the specified port already exists.</p>
newtype DuplicateListenerException = DuplicateListenerException Types.NoArguments
derive instance newtypeDuplicateListenerException :: Newtype DuplicateListenerException _
derive instance repGenericDuplicateListenerException :: Generic DuplicateListenerException _
instance showDuplicateListenerException :: Show DuplicateListenerException where
  show = genericShow
instance decodeDuplicateListenerException :: Decode DuplicateListenerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateListenerException :: Encode DuplicateListenerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A load balancer with the specified name already exists.</p>
newtype DuplicateLoadBalancerNameException = DuplicateLoadBalancerNameException Types.NoArguments
derive instance newtypeDuplicateLoadBalancerNameException :: Newtype DuplicateLoadBalancerNameException _
derive instance repGenericDuplicateLoadBalancerNameException :: Generic DuplicateLoadBalancerNameException _
instance showDuplicateLoadBalancerNameException :: Show DuplicateLoadBalancerNameException where
  show = genericShow
instance decodeDuplicateLoadBalancerNameException :: Decode DuplicateLoadBalancerNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateLoadBalancerNameException :: Encode DuplicateLoadBalancerNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A tag key was specified more than once.</p>
newtype DuplicateTagKeysException = DuplicateTagKeysException Types.NoArguments
derive instance newtypeDuplicateTagKeysException :: Newtype DuplicateTagKeysException _
derive instance repGenericDuplicateTagKeysException :: Generic DuplicateTagKeysException _
instance showDuplicateTagKeysException :: Show DuplicateTagKeysException where
  show = genericShow
instance decodeDuplicateTagKeysException :: Decode DuplicateTagKeysException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateTagKeysException :: Encode DuplicateTagKeysException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A target group with the specified name already exists.</p>
newtype DuplicateTargetGroupNameException = DuplicateTargetGroupNameException Types.NoArguments
derive instance newtypeDuplicateTargetGroupNameException :: Newtype DuplicateTargetGroupNameException _
derive instance repGenericDuplicateTargetGroupNameException :: Generic DuplicateTargetGroupNameException _
instance showDuplicateTargetGroupNameException :: Show DuplicateTargetGroupNameException where
  show = genericShow
instance decodeDuplicateTargetGroupNameException :: Decode DuplicateTargetGroupNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateTargetGroupNameException :: Encode DuplicateTargetGroupNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckIntervalSeconds = HealthCheckIntervalSeconds Int
derive instance newtypeHealthCheckIntervalSeconds :: Newtype HealthCheckIntervalSeconds _
derive instance repGenericHealthCheckIntervalSeconds :: Generic HealthCheckIntervalSeconds _
instance showHealthCheckIntervalSeconds :: Show HealthCheckIntervalSeconds where
  show = genericShow
instance decodeHealthCheckIntervalSeconds :: Decode HealthCheckIntervalSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckIntervalSeconds :: Encode HealthCheckIntervalSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckPort = HealthCheckPort String
derive instance newtypeHealthCheckPort :: Newtype HealthCheckPort _
derive instance repGenericHealthCheckPort :: Generic HealthCheckPort _
instance showHealthCheckPort :: Show HealthCheckPort where
  show = genericShow
instance decodeHealthCheckPort :: Decode HealthCheckPort where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckPort :: Encode HealthCheckPort where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckThresholdCount = HealthCheckThresholdCount Int
derive instance newtypeHealthCheckThresholdCount :: Newtype HealthCheckThresholdCount _
derive instance repGenericHealthCheckThresholdCount :: Generic HealthCheckThresholdCount _
instance showHealthCheckThresholdCount :: Show HealthCheckThresholdCount where
  show = genericShow
instance decodeHealthCheckThresholdCount :: Decode HealthCheckThresholdCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckThresholdCount :: Encode HealthCheckThresholdCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckTimeoutSeconds = HealthCheckTimeoutSeconds Int
derive instance newtypeHealthCheckTimeoutSeconds :: Newtype HealthCheckTimeoutSeconds _
derive instance repGenericHealthCheckTimeoutSeconds :: Generic HealthCheckTimeoutSeconds _
instance showHealthCheckTimeoutSeconds :: Show HealthCheckTimeoutSeconds where
  show = genericShow
instance decodeHealthCheckTimeoutSeconds :: Decode HealthCheckTimeoutSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckTimeoutSeconds :: Encode HealthCheckTimeoutSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The health of the specified targets could not be retrieved due to an internal error.</p>
newtype HealthUnavailableException = HealthUnavailableException Types.NoArguments
derive instance newtypeHealthUnavailableException :: Newtype HealthUnavailableException _
derive instance repGenericHealthUnavailableException :: Generic HealthUnavailableException _
instance showHealthUnavailableException :: Show HealthUnavailableException where
  show = genericShow
instance decodeHealthUnavailableException :: Decode HealthUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthUnavailableException :: Encode HealthUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HttpCode = HttpCode String
derive instance newtypeHttpCode :: Newtype HttpCode _
derive instance repGenericHttpCode :: Generic HttpCode _
instance showHttpCode :: Show HttpCode where
  show = genericShow
instance decodeHttpCode :: Decode HttpCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHttpCode :: Encode HttpCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified configuration is not valid with this protocol.</p>
newtype IncompatibleProtocolsException = IncompatibleProtocolsException Types.NoArguments
derive instance newtypeIncompatibleProtocolsException :: Newtype IncompatibleProtocolsException _
derive instance repGenericIncompatibleProtocolsException :: Generic IncompatibleProtocolsException _
instance showIncompatibleProtocolsException :: Show IncompatibleProtocolsException where
  show = genericShow
instance decodeIncompatibleProtocolsException :: Decode IncompatibleProtocolsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncompatibleProtocolsException :: Encode IncompatibleProtocolsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested configuration is not valid.</p>
newtype InvalidConfigurationRequestException = InvalidConfigurationRequestException Types.NoArguments
derive instance newtypeInvalidConfigurationRequestException :: Newtype InvalidConfigurationRequestException _
derive instance repGenericInvalidConfigurationRequestException :: Generic InvalidConfigurationRequestException _
instance showInvalidConfigurationRequestException :: Show InvalidConfigurationRequestException where
  show = genericShow
instance decodeInvalidConfigurationRequestException :: Decode InvalidConfigurationRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidConfigurationRequestException :: Encode InvalidConfigurationRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested scheme is not valid.</p>
newtype InvalidSchemeException = InvalidSchemeException Types.NoArguments
derive instance newtypeInvalidSchemeException :: Newtype InvalidSchemeException _
derive instance repGenericInvalidSchemeException :: Generic InvalidSchemeException _
instance showInvalidSchemeException :: Show InvalidSchemeException where
  show = genericShow
instance decodeInvalidSchemeException :: Decode InvalidSchemeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSchemeException :: Encode InvalidSchemeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified security group does not exist.</p>
newtype InvalidSecurityGroupException = InvalidSecurityGroupException Types.NoArguments
derive instance newtypeInvalidSecurityGroupException :: Newtype InvalidSecurityGroupException _
derive instance repGenericInvalidSecurityGroupException :: Generic InvalidSecurityGroupException _
instance showInvalidSecurityGroupException :: Show InvalidSecurityGroupException where
  show = genericShow
instance decodeInvalidSecurityGroupException :: Decode InvalidSecurityGroupException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSecurityGroupException :: Encode InvalidSecurityGroupException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified subnet is out of available addresses.</p>
newtype InvalidSubnetException = InvalidSubnetException Types.NoArguments
derive instance newtypeInvalidSubnetException :: Newtype InvalidSubnetException _
derive instance repGenericInvalidSubnetException :: Generic InvalidSubnetException _
instance showInvalidSubnetException :: Show InvalidSubnetException where
  show = genericShow
instance decodeInvalidSubnetException :: Decode InvalidSubnetException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSubnetException :: Encode InvalidSubnetException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified target does not exist, is not in the same VPC as the target group, or has an unsupported instance type.</p>
newtype InvalidTargetException = InvalidTargetException Types.NoArguments
derive instance newtypeInvalidTargetException :: Newtype InvalidTargetException _
derive instance repGenericInvalidTargetException :: Generic InvalidTargetException _
instance showInvalidTargetException :: Show InvalidTargetException where
  show = genericShow
instance decodeInvalidTargetException :: Decode InvalidTargetException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTargetException :: Encode InvalidTargetException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _
derive instance repGenericIpAddress :: Generic IpAddress _
instance showIpAddress :: Show IpAddress where
  show = genericShow
instance decodeIpAddress :: Decode IpAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddress :: Encode IpAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddressType = IpAddressType String
derive instance newtypeIpAddressType :: Newtype IpAddressType _
derive instance repGenericIpAddressType :: Generic IpAddressType _
instance showIpAddressType :: Show IpAddressType where
  show = genericShow
instance decodeIpAddressType :: Decode IpAddressType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddressType :: Encode IpAddressType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsDefault = IsDefault Boolean
derive instance newtypeIsDefault :: Newtype IsDefault _
derive instance repGenericIsDefault :: Generic IsDefault _
instance showIsDefault :: Show IsDefault where
  show = genericShow
instance decodeIsDefault :: Decode IsDefault where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsDefault :: Encode IsDefault where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an Elastic Load Balancing resource limit for your AWS account.</p>
newtype Limit = Limit 
  { "Name" :: NullOrUndefined.NullOrUndefined (Name)
  , "Max" :: NullOrUndefined.NullOrUndefined (Max)
  }
derive instance newtypeLimit :: Newtype Limit _
derive instance repGenericLimit :: Generic Limit _
instance showLimit :: Show Limit where
  show = genericShow
instance decodeLimit :: Decode Limit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimit :: Encode Limit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Limits = Limits (Array Limit)
derive instance newtypeLimits :: Newtype Limits _
derive instance repGenericLimits :: Generic Limits _
instance showLimits :: Show Limits where
  show = genericShow
instance decodeLimits :: Decode Limits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimits :: Encode Limits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListOfString = ListOfString (Array StringValue)
derive instance newtypeListOfString :: Newtype ListOfString _
derive instance repGenericListOfString :: Generic ListOfString _
instance showListOfString :: Show ListOfString where
  show = genericShow
instance decodeListOfString :: Decode ListOfString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListOfString :: Encode ListOfString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a listener.</p>
newtype Listener = Listener 
  { "ListenerArn" :: NullOrUndefined.NullOrUndefined (ListenerArn)
  , "LoadBalancerArn" :: NullOrUndefined.NullOrUndefined (LoadBalancerArn)
  , "Port" :: NullOrUndefined.NullOrUndefined (Port)
  , "Protocol" :: NullOrUndefined.NullOrUndefined (ProtocolEnum)
  , "Certificates" :: NullOrUndefined.NullOrUndefined (CertificateList)
  , "SslPolicy" :: NullOrUndefined.NullOrUndefined (SslPolicyName)
  , "DefaultActions" :: NullOrUndefined.NullOrUndefined (Actions)
  }
derive instance newtypeListener :: Newtype Listener _
derive instance repGenericListener :: Generic Listener _
instance showListener :: Show Listener where
  show = genericShow
instance decodeListener :: Decode Listener where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListener :: Encode Listener where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListenerArn = ListenerArn String
derive instance newtypeListenerArn :: Newtype ListenerArn _
derive instance repGenericListenerArn :: Generic ListenerArn _
instance showListenerArn :: Show ListenerArn where
  show = genericShow
instance decodeListenerArn :: Decode ListenerArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListenerArn :: Encode ListenerArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListenerArns = ListenerArns (Array ListenerArn)
derive instance newtypeListenerArns :: Newtype ListenerArns _
derive instance repGenericListenerArns :: Generic ListenerArns _
instance showListenerArns :: Show ListenerArns where
  show = genericShow
instance decodeListenerArns :: Decode ListenerArns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListenerArns :: Encode ListenerArns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified listener does not exist.</p>
newtype ListenerNotFoundException = ListenerNotFoundException Types.NoArguments
derive instance newtypeListenerNotFoundException :: Newtype ListenerNotFoundException _
derive instance repGenericListenerNotFoundException :: Generic ListenerNotFoundException _
instance showListenerNotFoundException :: Show ListenerNotFoundException where
  show = genericShow
instance decodeListenerNotFoundException :: Decode ListenerNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListenerNotFoundException :: Encode ListenerNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Listeners = Listeners (Array Listener)
derive instance newtypeListeners :: Newtype Listeners _
derive instance repGenericListeners :: Generic Listeners _
instance showListeners :: Show Listeners where
  show = genericShow
instance decodeListeners :: Decode Listeners where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListeners :: Encode Listeners where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a load balancer.</p>
newtype LoadBalancer = LoadBalancer 
  { "LoadBalancerArn" :: NullOrUndefined.NullOrUndefined (LoadBalancerArn)
  , "DNSName" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "CanonicalHostedZoneId" :: NullOrUndefined.NullOrUndefined (CanonicalHostedZoneId)
  , "CreatedTime" :: NullOrUndefined.NullOrUndefined (CreatedTime)
  , "LoadBalancerName" :: NullOrUndefined.NullOrUndefined (LoadBalancerName)
  , "Scheme" :: NullOrUndefined.NullOrUndefined (LoadBalancerSchemeEnum)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (VpcId)
  , "State" :: NullOrUndefined.NullOrUndefined (LoadBalancerState)
  , "Type" :: NullOrUndefined.NullOrUndefined (LoadBalancerTypeEnum)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZones)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  , "IpAddressType" :: NullOrUndefined.NullOrUndefined (IpAddressType)
  }
derive instance newtypeLoadBalancer :: Newtype LoadBalancer _
derive instance repGenericLoadBalancer :: Generic LoadBalancer _
instance showLoadBalancer :: Show LoadBalancer where
  show = genericShow
instance decodeLoadBalancer :: Decode LoadBalancer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancer :: Encode LoadBalancer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a static IP address for a load balancer.</p>
newtype LoadBalancerAddress = LoadBalancerAddress 
  { "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "AllocationId" :: NullOrUndefined.NullOrUndefined (AllocationId)
  }
derive instance newtypeLoadBalancerAddress :: Newtype LoadBalancerAddress _
derive instance repGenericLoadBalancerAddress :: Generic LoadBalancerAddress _
instance showLoadBalancerAddress :: Show LoadBalancerAddress where
  show = genericShow
instance decodeLoadBalancerAddress :: Decode LoadBalancerAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAddress :: Encode LoadBalancerAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerAddresses = LoadBalancerAddresses (Array LoadBalancerAddress)
derive instance newtypeLoadBalancerAddresses :: Newtype LoadBalancerAddresses _
derive instance repGenericLoadBalancerAddresses :: Generic LoadBalancerAddresses _
instance showLoadBalancerAddresses :: Show LoadBalancerAddresses where
  show = genericShow
instance decodeLoadBalancerAddresses :: Decode LoadBalancerAddresses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAddresses :: Encode LoadBalancerAddresses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerArn = LoadBalancerArn String
derive instance newtypeLoadBalancerArn :: Newtype LoadBalancerArn _
derive instance repGenericLoadBalancerArn :: Generic LoadBalancerArn _
instance showLoadBalancerArn :: Show LoadBalancerArn where
  show = genericShow
instance decodeLoadBalancerArn :: Decode LoadBalancerArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerArn :: Encode LoadBalancerArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerArns = LoadBalancerArns (Array LoadBalancerArn)
derive instance newtypeLoadBalancerArns :: Newtype LoadBalancerArns _
derive instance repGenericLoadBalancerArns :: Generic LoadBalancerArns _
instance showLoadBalancerArns :: Show LoadBalancerArns where
  show = genericShow
instance decodeLoadBalancerArns :: Decode LoadBalancerArns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerArns :: Encode LoadBalancerArns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a load balancer attribute.</p>
newtype LoadBalancerAttribute = LoadBalancerAttribute 
  { "Key" :: NullOrUndefined.NullOrUndefined (LoadBalancerAttributeKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (LoadBalancerAttributeValue)
  }
derive instance newtypeLoadBalancerAttribute :: Newtype LoadBalancerAttribute _
derive instance repGenericLoadBalancerAttribute :: Generic LoadBalancerAttribute _
instance showLoadBalancerAttribute :: Show LoadBalancerAttribute where
  show = genericShow
instance decodeLoadBalancerAttribute :: Decode LoadBalancerAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAttribute :: Encode LoadBalancerAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerAttributeKey = LoadBalancerAttributeKey String
derive instance newtypeLoadBalancerAttributeKey :: Newtype LoadBalancerAttributeKey _
derive instance repGenericLoadBalancerAttributeKey :: Generic LoadBalancerAttributeKey _
instance showLoadBalancerAttributeKey :: Show LoadBalancerAttributeKey where
  show = genericShow
instance decodeLoadBalancerAttributeKey :: Decode LoadBalancerAttributeKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAttributeKey :: Encode LoadBalancerAttributeKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerAttributeValue = LoadBalancerAttributeValue String
derive instance newtypeLoadBalancerAttributeValue :: Newtype LoadBalancerAttributeValue _
derive instance repGenericLoadBalancerAttributeValue :: Generic LoadBalancerAttributeValue _
instance showLoadBalancerAttributeValue :: Show LoadBalancerAttributeValue where
  show = genericShow
instance decodeLoadBalancerAttributeValue :: Decode LoadBalancerAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAttributeValue :: Encode LoadBalancerAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerAttributes = LoadBalancerAttributes (Array LoadBalancerAttribute)
derive instance newtypeLoadBalancerAttributes :: Newtype LoadBalancerAttributes _
derive instance repGenericLoadBalancerAttributes :: Generic LoadBalancerAttributes _
instance showLoadBalancerAttributes :: Show LoadBalancerAttributes where
  show = genericShow
instance decodeLoadBalancerAttributes :: Decode LoadBalancerAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAttributes :: Encode LoadBalancerAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerName = LoadBalancerName String
derive instance newtypeLoadBalancerName :: Newtype LoadBalancerName _
derive instance repGenericLoadBalancerName :: Generic LoadBalancerName _
instance showLoadBalancerName :: Show LoadBalancerName where
  show = genericShow
instance decodeLoadBalancerName :: Decode LoadBalancerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerName :: Encode LoadBalancerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerNames = LoadBalancerNames (Array LoadBalancerName)
derive instance newtypeLoadBalancerNames :: Newtype LoadBalancerNames _
derive instance repGenericLoadBalancerNames :: Generic LoadBalancerNames _
instance showLoadBalancerNames :: Show LoadBalancerNames where
  show = genericShow
instance decodeLoadBalancerNames :: Decode LoadBalancerNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerNames :: Encode LoadBalancerNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified load balancer does not exist.</p>
newtype LoadBalancerNotFoundException = LoadBalancerNotFoundException Types.NoArguments
derive instance newtypeLoadBalancerNotFoundException :: Newtype LoadBalancerNotFoundException _
derive instance repGenericLoadBalancerNotFoundException :: Generic LoadBalancerNotFoundException _
instance showLoadBalancerNotFoundException :: Show LoadBalancerNotFoundException where
  show = genericShow
instance decodeLoadBalancerNotFoundException :: Decode LoadBalancerNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerNotFoundException :: Encode LoadBalancerNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerSchemeEnum = LoadBalancerSchemeEnum String
derive instance newtypeLoadBalancerSchemeEnum :: Newtype LoadBalancerSchemeEnum _
derive instance repGenericLoadBalancerSchemeEnum :: Generic LoadBalancerSchemeEnum _
instance showLoadBalancerSchemeEnum :: Show LoadBalancerSchemeEnum where
  show = genericShow
instance decodeLoadBalancerSchemeEnum :: Decode LoadBalancerSchemeEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerSchemeEnum :: Encode LoadBalancerSchemeEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the state of the load balancer.</p>
newtype LoadBalancerState = LoadBalancerState 
  { "Code" :: NullOrUndefined.NullOrUndefined (LoadBalancerStateEnum)
  , "Reason" :: NullOrUndefined.NullOrUndefined (StateReason)
  }
derive instance newtypeLoadBalancerState :: Newtype LoadBalancerState _
derive instance repGenericLoadBalancerState :: Generic LoadBalancerState _
instance showLoadBalancerState :: Show LoadBalancerState where
  show = genericShow
instance decodeLoadBalancerState :: Decode LoadBalancerState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerState :: Encode LoadBalancerState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerStateEnum = LoadBalancerStateEnum String
derive instance newtypeLoadBalancerStateEnum :: Newtype LoadBalancerStateEnum _
derive instance repGenericLoadBalancerStateEnum :: Generic LoadBalancerStateEnum _
instance showLoadBalancerStateEnum :: Show LoadBalancerStateEnum where
  show = genericShow
instance decodeLoadBalancerStateEnum :: Decode LoadBalancerStateEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerStateEnum :: Encode LoadBalancerStateEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerTypeEnum = LoadBalancerTypeEnum String
derive instance newtypeLoadBalancerTypeEnum :: Newtype LoadBalancerTypeEnum _
derive instance repGenericLoadBalancerTypeEnum :: Generic LoadBalancerTypeEnum _
instance showLoadBalancerTypeEnum :: Show LoadBalancerTypeEnum where
  show = genericShow
instance decodeLoadBalancerTypeEnum :: Decode LoadBalancerTypeEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerTypeEnum :: Encode LoadBalancerTypeEnum where
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


newtype Marker = Marker String
derive instance newtypeMarker :: Newtype Marker _
derive instance repGenericMarker :: Generic Marker _
instance showMarker :: Show Marker where
  show = genericShow
instance decodeMarker :: Decode Marker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarker :: Encode Marker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information to use when checking for a successful response from a target.</p>
newtype Matcher = Matcher 
  { "HttpCode" :: (HttpCode)
  }
derive instance newtypeMatcher :: Newtype Matcher _
derive instance repGenericMatcher :: Generic Matcher _
instance showMatcher :: Show Matcher where
  show = genericShow
instance decodeMatcher :: Decode Matcher where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatcher :: Encode Matcher where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Max = Max String
derive instance newtypeMax :: Newtype Max _
derive instance repGenericMax :: Generic Max _
instance showMax :: Show Max where
  show = genericShow
instance decodeMax :: Decode Max where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMax :: Encode Max where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyListenerInput = ModifyListenerInput 
  { "ListenerArn" :: (ListenerArn)
  , "Port" :: NullOrUndefined.NullOrUndefined (Port)
  , "Protocol" :: NullOrUndefined.NullOrUndefined (ProtocolEnum)
  , "SslPolicy" :: NullOrUndefined.NullOrUndefined (SslPolicyName)
  , "Certificates" :: NullOrUndefined.NullOrUndefined (CertificateList)
  , "DefaultActions" :: NullOrUndefined.NullOrUndefined (Actions)
  }
derive instance newtypeModifyListenerInput :: Newtype ModifyListenerInput _
derive instance repGenericModifyListenerInput :: Generic ModifyListenerInput _
instance showModifyListenerInput :: Show ModifyListenerInput where
  show = genericShow
instance decodeModifyListenerInput :: Decode ModifyListenerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyListenerInput :: Encode ModifyListenerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyListenerOutput = ModifyListenerOutput 
  { "Listeners" :: NullOrUndefined.NullOrUndefined (Listeners)
  }
derive instance newtypeModifyListenerOutput :: Newtype ModifyListenerOutput _
derive instance repGenericModifyListenerOutput :: Generic ModifyListenerOutput _
instance showModifyListenerOutput :: Show ModifyListenerOutput where
  show = genericShow
instance decodeModifyListenerOutput :: Decode ModifyListenerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyListenerOutput :: Encode ModifyListenerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyLoadBalancerAttributesInput = ModifyLoadBalancerAttributesInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Attributes" :: (LoadBalancerAttributes)
  }
derive instance newtypeModifyLoadBalancerAttributesInput :: Newtype ModifyLoadBalancerAttributesInput _
derive instance repGenericModifyLoadBalancerAttributesInput :: Generic ModifyLoadBalancerAttributesInput _
instance showModifyLoadBalancerAttributesInput :: Show ModifyLoadBalancerAttributesInput where
  show = genericShow
instance decodeModifyLoadBalancerAttributesInput :: Decode ModifyLoadBalancerAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyLoadBalancerAttributesInput :: Encode ModifyLoadBalancerAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyLoadBalancerAttributesOutput = ModifyLoadBalancerAttributesOutput 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (LoadBalancerAttributes)
  }
derive instance newtypeModifyLoadBalancerAttributesOutput :: Newtype ModifyLoadBalancerAttributesOutput _
derive instance repGenericModifyLoadBalancerAttributesOutput :: Generic ModifyLoadBalancerAttributesOutput _
instance showModifyLoadBalancerAttributesOutput :: Show ModifyLoadBalancerAttributesOutput where
  show = genericShow
instance decodeModifyLoadBalancerAttributesOutput :: Decode ModifyLoadBalancerAttributesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyLoadBalancerAttributesOutput :: Encode ModifyLoadBalancerAttributesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyRuleInput = ModifyRuleInput 
  { "RuleArn" :: (RuleArn)
  , "Conditions" :: NullOrUndefined.NullOrUndefined (RuleConditionList)
  , "Actions" :: NullOrUndefined.NullOrUndefined (Actions)
  }
derive instance newtypeModifyRuleInput :: Newtype ModifyRuleInput _
derive instance repGenericModifyRuleInput :: Generic ModifyRuleInput _
instance showModifyRuleInput :: Show ModifyRuleInput where
  show = genericShow
instance decodeModifyRuleInput :: Decode ModifyRuleInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyRuleInput :: Encode ModifyRuleInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyRuleOutput = ModifyRuleOutput 
  { "Rules" :: NullOrUndefined.NullOrUndefined (Rules)
  }
derive instance newtypeModifyRuleOutput :: Newtype ModifyRuleOutput _
derive instance repGenericModifyRuleOutput :: Generic ModifyRuleOutput _
instance showModifyRuleOutput :: Show ModifyRuleOutput where
  show = genericShow
instance decodeModifyRuleOutput :: Decode ModifyRuleOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyRuleOutput :: Encode ModifyRuleOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyTargetGroupAttributesInput = ModifyTargetGroupAttributesInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Attributes" :: (TargetGroupAttributes)
  }
derive instance newtypeModifyTargetGroupAttributesInput :: Newtype ModifyTargetGroupAttributesInput _
derive instance repGenericModifyTargetGroupAttributesInput :: Generic ModifyTargetGroupAttributesInput _
instance showModifyTargetGroupAttributesInput :: Show ModifyTargetGroupAttributesInput where
  show = genericShow
instance decodeModifyTargetGroupAttributesInput :: Decode ModifyTargetGroupAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyTargetGroupAttributesInput :: Encode ModifyTargetGroupAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyTargetGroupAttributesOutput = ModifyTargetGroupAttributesOutput 
  { "Attributes" :: NullOrUndefined.NullOrUndefined (TargetGroupAttributes)
  }
derive instance newtypeModifyTargetGroupAttributesOutput :: Newtype ModifyTargetGroupAttributesOutput _
derive instance repGenericModifyTargetGroupAttributesOutput :: Generic ModifyTargetGroupAttributesOutput _
instance showModifyTargetGroupAttributesOutput :: Show ModifyTargetGroupAttributesOutput where
  show = genericShow
instance decodeModifyTargetGroupAttributesOutput :: Decode ModifyTargetGroupAttributesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyTargetGroupAttributesOutput :: Encode ModifyTargetGroupAttributesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyTargetGroupInput = ModifyTargetGroupInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "HealthCheckProtocol" :: NullOrUndefined.NullOrUndefined (ProtocolEnum)
  , "HealthCheckPort" :: NullOrUndefined.NullOrUndefined (HealthCheckPort)
  , "HealthCheckPath" :: NullOrUndefined.NullOrUndefined (Path)
  , "HealthCheckIntervalSeconds" :: NullOrUndefined.NullOrUndefined (HealthCheckIntervalSeconds)
  , "HealthCheckTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (HealthCheckTimeoutSeconds)
  , "HealthyThresholdCount" :: NullOrUndefined.NullOrUndefined (HealthCheckThresholdCount)
  , "UnhealthyThresholdCount" :: NullOrUndefined.NullOrUndefined (HealthCheckThresholdCount)
  , "Matcher" :: NullOrUndefined.NullOrUndefined (Matcher)
  }
derive instance newtypeModifyTargetGroupInput :: Newtype ModifyTargetGroupInput _
derive instance repGenericModifyTargetGroupInput :: Generic ModifyTargetGroupInput _
instance showModifyTargetGroupInput :: Show ModifyTargetGroupInput where
  show = genericShow
instance decodeModifyTargetGroupInput :: Decode ModifyTargetGroupInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyTargetGroupInput :: Encode ModifyTargetGroupInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyTargetGroupOutput = ModifyTargetGroupOutput 
  { "TargetGroups" :: NullOrUndefined.NullOrUndefined (TargetGroups)
  }
derive instance newtypeModifyTargetGroupOutput :: Newtype ModifyTargetGroupOutput _
derive instance repGenericModifyTargetGroupOutput :: Generic ModifyTargetGroupOutput _
instance showModifyTargetGroupOutput :: Show ModifyTargetGroupOutput where
  show = genericShow
instance decodeModifyTargetGroupOutput :: Decode ModifyTargetGroupOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyTargetGroupOutput :: Encode ModifyTargetGroupOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive instance repGenericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow
instance decodeName :: Decode Name where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName :: Encode Name where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This operation is not allowed.</p>
newtype OperationNotPermittedException = OperationNotPermittedException Types.NoArguments
derive instance newtypeOperationNotPermittedException :: Newtype OperationNotPermittedException _
derive instance repGenericOperationNotPermittedException :: Generic OperationNotPermittedException _
instance showOperationNotPermittedException :: Show OperationNotPermittedException where
  show = genericShow
instance decodeOperationNotPermittedException :: Decode OperationNotPermittedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationNotPermittedException :: Encode OperationNotPermittedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _
derive instance repGenericPageSize :: Generic PageSize _
instance showPageSize :: Show PageSize where
  show = genericShow
instance decodePageSize :: Decode PageSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageSize :: Encode PageSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Path = Path String
derive instance newtypePath :: Newtype Path _
derive instance repGenericPath :: Generic Path _
instance showPath :: Show Path where
  show = genericShow
instance decodePath :: Decode Path where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePath :: Encode Path where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Port = Port Int
derive instance newtypePort :: Newtype Port _
derive instance repGenericPort :: Generic Port _
instance showPort :: Show Port where
  show = genericShow
instance decodePort :: Decode Port where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePort :: Encode Port where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified priority is in use.</p>
newtype PriorityInUseException = PriorityInUseException Types.NoArguments
derive instance newtypePriorityInUseException :: Newtype PriorityInUseException _
derive instance repGenericPriorityInUseException :: Generic PriorityInUseException _
instance showPriorityInUseException :: Show PriorityInUseException where
  show = genericShow
instance decodePriorityInUseException :: Decode PriorityInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePriorityInUseException :: Encode PriorityInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProtocolEnum = ProtocolEnum String
derive instance newtypeProtocolEnum :: Newtype ProtocolEnum _
derive instance repGenericProtocolEnum :: Generic ProtocolEnum _
instance showProtocolEnum :: Show ProtocolEnum where
  show = genericShow
instance decodeProtocolEnum :: Decode ProtocolEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProtocolEnum :: Encode ProtocolEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterTargetsInput = RegisterTargetsInput 
  { "TargetGroupArn" :: (TargetGroupArn)
  , "Targets" :: (TargetDescriptions)
  }
derive instance newtypeRegisterTargetsInput :: Newtype RegisterTargetsInput _
derive instance repGenericRegisterTargetsInput :: Generic RegisterTargetsInput _
instance showRegisterTargetsInput :: Show RegisterTargetsInput where
  show = genericShow
instance decodeRegisterTargetsInput :: Decode RegisterTargetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterTargetsInput :: Encode RegisterTargetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegisterTargetsOutput = RegisterTargetsOutput Types.NoArguments
derive instance newtypeRegisterTargetsOutput :: Newtype RegisterTargetsOutput _
derive instance repGenericRegisterTargetsOutput :: Generic RegisterTargetsOutput _
instance showRegisterTargetsOutput :: Show RegisterTargetsOutput where
  show = genericShow
instance decodeRegisterTargetsOutput :: Decode RegisterTargetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterTargetsOutput :: Encode RegisterTargetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveListenerCertificatesInput = RemoveListenerCertificatesInput 
  { "ListenerArn" :: (ListenerArn)
  , "Certificates" :: (CertificateList)
  }
derive instance newtypeRemoveListenerCertificatesInput :: Newtype RemoveListenerCertificatesInput _
derive instance repGenericRemoveListenerCertificatesInput :: Generic RemoveListenerCertificatesInput _
instance showRemoveListenerCertificatesInput :: Show RemoveListenerCertificatesInput where
  show = genericShow
instance decodeRemoveListenerCertificatesInput :: Decode RemoveListenerCertificatesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveListenerCertificatesInput :: Encode RemoveListenerCertificatesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveListenerCertificatesOutput = RemoveListenerCertificatesOutput Types.NoArguments
derive instance newtypeRemoveListenerCertificatesOutput :: Newtype RemoveListenerCertificatesOutput _
derive instance repGenericRemoveListenerCertificatesOutput :: Generic RemoveListenerCertificatesOutput _
instance showRemoveListenerCertificatesOutput :: Show RemoveListenerCertificatesOutput where
  show = genericShow
instance decodeRemoveListenerCertificatesOutput :: Decode RemoveListenerCertificatesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveListenerCertificatesOutput :: Encode RemoveListenerCertificatesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsInput = RemoveTagsInput 
  { "ResourceArns" :: (ResourceArns)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeRemoveTagsInput :: Newtype RemoveTagsInput _
derive instance repGenericRemoveTagsInput :: Generic RemoveTagsInput _
instance showRemoveTagsInput :: Show RemoveTagsInput where
  show = genericShow
instance decodeRemoveTagsInput :: Decode RemoveTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsInput :: Encode RemoveTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsOutput = RemoveTagsOutput Types.NoArguments
derive instance newtypeRemoveTagsOutput :: Newtype RemoveTagsOutput _
derive instance repGenericRemoveTagsOutput :: Generic RemoveTagsOutput _
instance showRemoveTagsOutput :: Show RemoveTagsOutput where
  show = genericShow
instance decodeRemoveTagsOutput :: Decode RemoveTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsOutput :: Encode RemoveTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _
derive instance repGenericResourceArn :: Generic ResourceArn _
instance showResourceArn :: Show ResourceArn where
  show = genericShow
instance decodeResourceArn :: Decode ResourceArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArn :: Encode ResourceArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceArns = ResourceArns (Array ResourceArn)
derive instance newtypeResourceArns :: Newtype ResourceArns _
derive instance repGenericResourceArns :: Generic ResourceArns _
instance showResourceArns :: Show ResourceArns where
  show = genericShow
instance decodeResourceArns :: Decode ResourceArns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArns :: Encode ResourceArns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A specified resource is in use.</p>
newtype ResourceInUseException = ResourceInUseException Types.NoArguments
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a rule.</p>
newtype Rule = Rule 
  { "RuleArn" :: NullOrUndefined.NullOrUndefined (RuleArn)
  , "Priority" :: NullOrUndefined.NullOrUndefined (String)
  , "Conditions" :: NullOrUndefined.NullOrUndefined (RuleConditionList)
  , "Actions" :: NullOrUndefined.NullOrUndefined (Actions)
  , "IsDefault" :: NullOrUndefined.NullOrUndefined (IsDefault)
  }
derive instance newtypeRule :: Newtype Rule _
derive instance repGenericRule :: Generic Rule _
instance showRule :: Show Rule where
  show = genericShow
instance decodeRule :: Decode Rule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRule :: Encode Rule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleArn = RuleArn String
derive instance newtypeRuleArn :: Newtype RuleArn _
derive instance repGenericRuleArn :: Generic RuleArn _
instance showRuleArn :: Show RuleArn where
  show = genericShow
instance decodeRuleArn :: Decode RuleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleArn :: Encode RuleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleArns = RuleArns (Array RuleArn)
derive instance newtypeRuleArns :: Newtype RuleArns _
derive instance repGenericRuleArns :: Generic RuleArns _
instance showRuleArns :: Show RuleArns where
  show = genericShow
instance decodeRuleArns :: Decode RuleArns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleArns :: Encode RuleArns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a condition for a rule.</p>
newtype RuleCondition = RuleCondition 
  { "Field" :: NullOrUndefined.NullOrUndefined (ConditionFieldName)
  , "Values" :: NullOrUndefined.NullOrUndefined (ListOfString)
  }
derive instance newtypeRuleCondition :: Newtype RuleCondition _
derive instance repGenericRuleCondition :: Generic RuleCondition _
instance showRuleCondition :: Show RuleCondition where
  show = genericShow
instance decodeRuleCondition :: Decode RuleCondition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleCondition :: Encode RuleCondition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RuleConditionList = RuleConditionList (Array RuleCondition)
derive instance newtypeRuleConditionList :: Newtype RuleConditionList _
derive instance repGenericRuleConditionList :: Generic RuleConditionList _
instance showRuleConditionList :: Show RuleConditionList where
  show = genericShow
instance decodeRuleConditionList :: Decode RuleConditionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleConditionList :: Encode RuleConditionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified rule does not exist.</p>
newtype RuleNotFoundException = RuleNotFoundException Types.NoArguments
derive instance newtypeRuleNotFoundException :: Newtype RuleNotFoundException _
derive instance repGenericRuleNotFoundException :: Generic RuleNotFoundException _
instance showRuleNotFoundException :: Show RuleNotFoundException where
  show = genericShow
instance decodeRuleNotFoundException :: Decode RuleNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRuleNotFoundException :: Encode RuleNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RulePriority = RulePriority Int
derive instance newtypeRulePriority :: Newtype RulePriority _
derive instance repGenericRulePriority :: Generic RulePriority _
instance showRulePriority :: Show RulePriority where
  show = genericShow
instance decodeRulePriority :: Decode RulePriority where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRulePriority :: Encode RulePriority where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RulePriorityList = RulePriorityList (Array RulePriorityPair)
derive instance newtypeRulePriorityList :: Newtype RulePriorityList _
derive instance repGenericRulePriorityList :: Generic RulePriorityList _
instance showRulePriorityList :: Show RulePriorityList where
  show = genericShow
instance decodeRulePriorityList :: Decode RulePriorityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRulePriorityList :: Encode RulePriorityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the priorities for the rules for a listener.</p>
newtype RulePriorityPair = RulePriorityPair 
  { "RuleArn" :: NullOrUndefined.NullOrUndefined (RuleArn)
  , "Priority" :: NullOrUndefined.NullOrUndefined (RulePriority)
  }
derive instance newtypeRulePriorityPair :: Newtype RulePriorityPair _
derive instance repGenericRulePriorityPair :: Generic RulePriorityPair _
instance showRulePriorityPair :: Show RulePriorityPair where
  show = genericShow
instance decodeRulePriorityPair :: Decode RulePriorityPair where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRulePriorityPair :: Encode RulePriorityPair where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Rules = Rules (Array Rule)
derive instance newtypeRules :: Newtype Rules _
derive instance repGenericRules :: Generic Rules _
instance showRules :: Show Rules where
  show = genericShow
instance decodeRules :: Decode Rules where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRules :: Encode Rules where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified SSL policy does not exist.</p>
newtype SSLPolicyNotFoundException = SSLPolicyNotFoundException Types.NoArguments
derive instance newtypeSSLPolicyNotFoundException :: Newtype SSLPolicyNotFoundException _
derive instance repGenericSSLPolicyNotFoundException :: Generic SSLPolicyNotFoundException _
instance showSSLPolicyNotFoundException :: Show SSLPolicyNotFoundException where
  show = genericShow
instance decodeSSLPolicyNotFoundException :: Decode SSLPolicyNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSLPolicyNotFoundException :: Encode SSLPolicyNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroupId = SecurityGroupId String
derive instance newtypeSecurityGroupId :: Newtype SecurityGroupId _
derive instance repGenericSecurityGroupId :: Generic SecurityGroupId _
instance showSecurityGroupId :: Show SecurityGroupId where
  show = genericShow
instance decodeSecurityGroupId :: Decode SecurityGroupId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupId :: Encode SecurityGroupId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroups = SecurityGroups (Array SecurityGroupId)
derive instance newtypeSecurityGroups :: Newtype SecurityGroups _
derive instance repGenericSecurityGroups :: Generic SecurityGroups _
instance showSecurityGroups :: Show SecurityGroups where
  show = genericShow
instance decodeSecurityGroups :: Decode SecurityGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroups :: Encode SecurityGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetIpAddressTypeInput = SetIpAddressTypeInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "IpAddressType" :: (IpAddressType)
  }
derive instance newtypeSetIpAddressTypeInput :: Newtype SetIpAddressTypeInput _
derive instance repGenericSetIpAddressTypeInput :: Generic SetIpAddressTypeInput _
instance showSetIpAddressTypeInput :: Show SetIpAddressTypeInput where
  show = genericShow
instance decodeSetIpAddressTypeInput :: Decode SetIpAddressTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIpAddressTypeInput :: Encode SetIpAddressTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetIpAddressTypeOutput = SetIpAddressTypeOutput 
  { "IpAddressType" :: NullOrUndefined.NullOrUndefined (IpAddressType)
  }
derive instance newtypeSetIpAddressTypeOutput :: Newtype SetIpAddressTypeOutput _
derive instance repGenericSetIpAddressTypeOutput :: Generic SetIpAddressTypeOutput _
instance showSetIpAddressTypeOutput :: Show SetIpAddressTypeOutput where
  show = genericShow
instance decodeSetIpAddressTypeOutput :: Decode SetIpAddressTypeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetIpAddressTypeOutput :: Encode SetIpAddressTypeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetRulePrioritiesInput = SetRulePrioritiesInput 
  { "RulePriorities" :: (RulePriorityList)
  }
derive instance newtypeSetRulePrioritiesInput :: Newtype SetRulePrioritiesInput _
derive instance repGenericSetRulePrioritiesInput :: Generic SetRulePrioritiesInput _
instance showSetRulePrioritiesInput :: Show SetRulePrioritiesInput where
  show = genericShow
instance decodeSetRulePrioritiesInput :: Decode SetRulePrioritiesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetRulePrioritiesInput :: Encode SetRulePrioritiesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetRulePrioritiesOutput = SetRulePrioritiesOutput 
  { "Rules" :: NullOrUndefined.NullOrUndefined (Rules)
  }
derive instance newtypeSetRulePrioritiesOutput :: Newtype SetRulePrioritiesOutput _
derive instance repGenericSetRulePrioritiesOutput :: Generic SetRulePrioritiesOutput _
instance showSetRulePrioritiesOutput :: Show SetRulePrioritiesOutput where
  show = genericShow
instance decodeSetRulePrioritiesOutput :: Decode SetRulePrioritiesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetRulePrioritiesOutput :: Encode SetRulePrioritiesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetSecurityGroupsInput = SetSecurityGroupsInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "SecurityGroups" :: (SecurityGroups)
  }
derive instance newtypeSetSecurityGroupsInput :: Newtype SetSecurityGroupsInput _
derive instance repGenericSetSecurityGroupsInput :: Generic SetSecurityGroupsInput _
instance showSetSecurityGroupsInput :: Show SetSecurityGroupsInput where
  show = genericShow
instance decodeSetSecurityGroupsInput :: Decode SetSecurityGroupsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetSecurityGroupsInput :: Encode SetSecurityGroupsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetSecurityGroupsOutput = SetSecurityGroupsOutput 
  { "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  }
derive instance newtypeSetSecurityGroupsOutput :: Newtype SetSecurityGroupsOutput _
derive instance repGenericSetSecurityGroupsOutput :: Generic SetSecurityGroupsOutput _
instance showSetSecurityGroupsOutput :: Show SetSecurityGroupsOutput where
  show = genericShow
instance decodeSetSecurityGroupsOutput :: Decode SetSecurityGroupsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetSecurityGroupsOutput :: Encode SetSecurityGroupsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetSubnetsInput = SetSubnetsInput 
  { "LoadBalancerArn" :: (LoadBalancerArn)
  , "Subnets" :: (Subnets)
  , "SubnetMappings" :: NullOrUndefined.NullOrUndefined (SubnetMappings)
  }
derive instance newtypeSetSubnetsInput :: Newtype SetSubnetsInput _
derive instance repGenericSetSubnetsInput :: Generic SetSubnetsInput _
instance showSetSubnetsInput :: Show SetSubnetsInput where
  show = genericShow
instance decodeSetSubnetsInput :: Decode SetSubnetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetSubnetsInput :: Encode SetSubnetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SetSubnetsOutput = SetSubnetsOutput 
  { "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZones)
  }
derive instance newtypeSetSubnetsOutput :: Newtype SetSubnetsOutput _
derive instance repGenericSetSubnetsOutput :: Generic SetSubnetsOutput _
instance showSetSubnetsOutput :: Show SetSubnetsOutput where
  show = genericShow
instance decodeSetSubnetsOutput :: Decode SetSubnetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetSubnetsOutput :: Encode SetSubnetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SslPolicies = SslPolicies (Array SslPolicy)
derive instance newtypeSslPolicies :: Newtype SslPolicies _
derive instance repGenericSslPolicies :: Generic SslPolicies _
instance showSslPolicies :: Show SslPolicies where
  show = genericShow
instance decodeSslPolicies :: Decode SslPolicies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSslPolicies :: Encode SslPolicies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a policy used for SSL negotiation.</p>
newtype SslPolicy = SslPolicy 
  { "SslProtocols" :: NullOrUndefined.NullOrUndefined (SslProtocols)
  , "Ciphers" :: NullOrUndefined.NullOrUndefined (Ciphers)
  , "Name" :: NullOrUndefined.NullOrUndefined (SslPolicyName)
  }
derive instance newtypeSslPolicy :: Newtype SslPolicy _
derive instance repGenericSslPolicy :: Generic SslPolicy _
instance showSslPolicy :: Show SslPolicy where
  show = genericShow
instance decodeSslPolicy :: Decode SslPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSslPolicy :: Encode SslPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SslPolicyName = SslPolicyName String
derive instance newtypeSslPolicyName :: Newtype SslPolicyName _
derive instance repGenericSslPolicyName :: Generic SslPolicyName _
instance showSslPolicyName :: Show SslPolicyName where
  show = genericShow
instance decodeSslPolicyName :: Decode SslPolicyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSslPolicyName :: Encode SslPolicyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SslPolicyNames = SslPolicyNames (Array SslPolicyName)
derive instance newtypeSslPolicyNames :: Newtype SslPolicyNames _
derive instance repGenericSslPolicyNames :: Generic SslPolicyNames _
instance showSslPolicyNames :: Show SslPolicyNames where
  show = genericShow
instance decodeSslPolicyNames :: Decode SslPolicyNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSslPolicyNames :: Encode SslPolicyNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SslProtocol = SslProtocol String
derive instance newtypeSslProtocol :: Newtype SslProtocol _
derive instance repGenericSslProtocol :: Generic SslProtocol _
instance showSslProtocol :: Show SslProtocol where
  show = genericShow
instance decodeSslProtocol :: Decode SslProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSslProtocol :: Encode SslProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SslProtocols = SslProtocols (Array SslProtocol)
derive instance newtypeSslProtocols :: Newtype SslProtocols _
derive instance repGenericSslProtocols :: Generic SslProtocols _
instance showSslProtocols :: Show SslProtocols where
  show = genericShow
instance decodeSslProtocols :: Decode SslProtocols where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSslProtocols :: Encode SslProtocols where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateReason = StateReason String
derive instance newtypeStateReason :: Newtype StateReason _
derive instance repGenericStateReason :: Generic StateReason _
instance showStateReason :: Show StateReason where
  show = genericShow
instance decodeStateReason :: Decode StateReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateReason :: Encode StateReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringValue = StringValue String
derive instance newtypeStringValue :: Newtype StringValue _
derive instance repGenericStringValue :: Generic StringValue _
instance showStringValue :: Show StringValue where
  show = genericShow
instance decodeStringValue :: Decode StringValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringValue :: Encode StringValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _
derive instance repGenericSubnetId :: Generic SubnetId _
instance showSubnetId :: Show SubnetId where
  show = genericShow
instance decodeSubnetId :: Decode SubnetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetId :: Encode SubnetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a subnet mapping.</p>
newtype SubnetMapping = SubnetMapping 
  { "SubnetId" :: NullOrUndefined.NullOrUndefined (SubnetId)
  , "AllocationId" :: NullOrUndefined.NullOrUndefined (AllocationId)
  }
derive instance newtypeSubnetMapping :: Newtype SubnetMapping _
derive instance repGenericSubnetMapping :: Generic SubnetMapping _
instance showSubnetMapping :: Show SubnetMapping where
  show = genericShow
instance decodeSubnetMapping :: Decode SubnetMapping where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetMapping :: Encode SubnetMapping where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetMappings = SubnetMappings (Array SubnetMapping)
derive instance newtypeSubnetMappings :: Newtype SubnetMappings _
derive instance repGenericSubnetMappings :: Generic SubnetMappings _
instance showSubnetMappings :: Show SubnetMappings where
  show = genericShow
instance decodeSubnetMappings :: Decode SubnetMappings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetMappings :: Encode SubnetMappings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified subnet does not exist.</p>
newtype SubnetNotFoundException = SubnetNotFoundException Types.NoArguments
derive instance newtypeSubnetNotFoundException :: Newtype SubnetNotFoundException _
derive instance repGenericSubnetNotFoundException :: Generic SubnetNotFoundException _
instance showSubnetNotFoundException :: Show SubnetNotFoundException where
  show = genericShow
instance decodeSubnetNotFoundException :: Decode SubnetNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetNotFoundException :: Encode SubnetNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Subnets = Subnets (Array SubnetId)
derive instance newtypeSubnets :: Newtype Subnets _
derive instance repGenericSubnets :: Generic Subnets _
instance showSubnets :: Show Subnets where
  show = genericShow
instance decodeSubnets :: Decode Subnets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnets :: Encode Subnets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The tags associated with a resource.</p>
newtype TagDescription = TagDescription 
  { "ResourceArn" :: NullOrUndefined.NullOrUndefined (ResourceArn)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeTagDescription :: Newtype TagDescription _
derive instance repGenericTagDescription :: Generic TagDescription _
instance showTagDescription :: Show TagDescription where
  show = genericShow
instance decodeTagDescription :: Decode TagDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagDescription :: Encode TagDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagDescriptions = TagDescriptions (Array TagDescription)
derive instance newtypeTagDescriptions :: Newtype TagDescriptions _
derive instance repGenericTagDescriptions :: Generic TagDescriptions _
instance showTagDescriptions :: Show TagDescriptions where
  show = genericShow
instance decodeTagDescriptions :: Decode TagDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagDescriptions :: Encode TagDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _
derive instance repGenericTagKeys :: Generic TagKeys _
instance showTagKeys :: Show TagKeys where
  show = genericShow
instance decodeTagKeys :: Decode TagKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeys :: Encode TagKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a target.</p>
newtype TargetDescription = TargetDescription 
  { "Id" :: (TargetId)
  , "Port" :: NullOrUndefined.NullOrUndefined (Port)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (ZoneName)
  }
derive instance newtypeTargetDescription :: Newtype TargetDescription _
derive instance repGenericTargetDescription :: Generic TargetDescription _
instance showTargetDescription :: Show TargetDescription where
  show = genericShow
instance decodeTargetDescription :: Decode TargetDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetDescription :: Encode TargetDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetDescriptions = TargetDescriptions (Array TargetDescription)
derive instance newtypeTargetDescriptions :: Newtype TargetDescriptions _
derive instance repGenericTargetDescriptions :: Generic TargetDescriptions _
instance showTargetDescriptions :: Show TargetDescriptions where
  show = genericShow
instance decodeTargetDescriptions :: Decode TargetDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetDescriptions :: Encode TargetDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a target group.</p>
newtype TargetGroup = TargetGroup 
  { "TargetGroupArn" :: NullOrUndefined.NullOrUndefined (TargetGroupArn)
  , "TargetGroupName" :: NullOrUndefined.NullOrUndefined (TargetGroupName)
  , "Protocol" :: NullOrUndefined.NullOrUndefined (ProtocolEnum)
  , "Port" :: NullOrUndefined.NullOrUndefined (Port)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (VpcId)
  , "HealthCheckProtocol" :: NullOrUndefined.NullOrUndefined (ProtocolEnum)
  , "HealthCheckPort" :: NullOrUndefined.NullOrUndefined (HealthCheckPort)
  , "HealthCheckIntervalSeconds" :: NullOrUndefined.NullOrUndefined (HealthCheckIntervalSeconds)
  , "HealthCheckTimeoutSeconds" :: NullOrUndefined.NullOrUndefined (HealthCheckTimeoutSeconds)
  , "HealthyThresholdCount" :: NullOrUndefined.NullOrUndefined (HealthCheckThresholdCount)
  , "UnhealthyThresholdCount" :: NullOrUndefined.NullOrUndefined (HealthCheckThresholdCount)
  , "HealthCheckPath" :: NullOrUndefined.NullOrUndefined (Path)
  , "Matcher" :: NullOrUndefined.NullOrUndefined (Matcher)
  , "LoadBalancerArns" :: NullOrUndefined.NullOrUndefined (LoadBalancerArns)
  , "TargetType" :: NullOrUndefined.NullOrUndefined (TargetTypeEnum)
  }
derive instance newtypeTargetGroup :: Newtype TargetGroup _
derive instance repGenericTargetGroup :: Generic TargetGroup _
instance showTargetGroup :: Show TargetGroup where
  show = genericShow
instance decodeTargetGroup :: Decode TargetGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroup :: Encode TargetGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGroupArn = TargetGroupArn String
derive instance newtypeTargetGroupArn :: Newtype TargetGroupArn _
derive instance repGenericTargetGroupArn :: Generic TargetGroupArn _
instance showTargetGroupArn :: Show TargetGroupArn where
  show = genericShow
instance decodeTargetGroupArn :: Decode TargetGroupArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupArn :: Encode TargetGroupArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGroupArns = TargetGroupArns (Array TargetGroupArn)
derive instance newtypeTargetGroupArns :: Newtype TargetGroupArns _
derive instance repGenericTargetGroupArns :: Generic TargetGroupArns _
instance showTargetGroupArns :: Show TargetGroupArns where
  show = genericShow
instance decodeTargetGroupArns :: Decode TargetGroupArns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupArns :: Encode TargetGroupArns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of load balancers per target group.</p>
newtype TargetGroupAssociationLimitException = TargetGroupAssociationLimitException Types.NoArguments
derive instance newtypeTargetGroupAssociationLimitException :: Newtype TargetGroupAssociationLimitException _
derive instance repGenericTargetGroupAssociationLimitException :: Generic TargetGroupAssociationLimitException _
instance showTargetGroupAssociationLimitException :: Show TargetGroupAssociationLimitException where
  show = genericShow
instance decodeTargetGroupAssociationLimitException :: Decode TargetGroupAssociationLimitException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupAssociationLimitException :: Encode TargetGroupAssociationLimitException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a target group attribute.</p>
newtype TargetGroupAttribute = TargetGroupAttribute 
  { "Key" :: NullOrUndefined.NullOrUndefined (TargetGroupAttributeKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (TargetGroupAttributeValue)
  }
derive instance newtypeTargetGroupAttribute :: Newtype TargetGroupAttribute _
derive instance repGenericTargetGroupAttribute :: Generic TargetGroupAttribute _
instance showTargetGroupAttribute :: Show TargetGroupAttribute where
  show = genericShow
instance decodeTargetGroupAttribute :: Decode TargetGroupAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupAttribute :: Encode TargetGroupAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGroupAttributeKey = TargetGroupAttributeKey String
derive instance newtypeTargetGroupAttributeKey :: Newtype TargetGroupAttributeKey _
derive instance repGenericTargetGroupAttributeKey :: Generic TargetGroupAttributeKey _
instance showTargetGroupAttributeKey :: Show TargetGroupAttributeKey where
  show = genericShow
instance decodeTargetGroupAttributeKey :: Decode TargetGroupAttributeKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupAttributeKey :: Encode TargetGroupAttributeKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGroupAttributeValue = TargetGroupAttributeValue String
derive instance newtypeTargetGroupAttributeValue :: Newtype TargetGroupAttributeValue _
derive instance repGenericTargetGroupAttributeValue :: Generic TargetGroupAttributeValue _
instance showTargetGroupAttributeValue :: Show TargetGroupAttributeValue where
  show = genericShow
instance decodeTargetGroupAttributeValue :: Decode TargetGroupAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupAttributeValue :: Encode TargetGroupAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGroupAttributes = TargetGroupAttributes (Array TargetGroupAttribute)
derive instance newtypeTargetGroupAttributes :: Newtype TargetGroupAttributes _
derive instance repGenericTargetGroupAttributes :: Generic TargetGroupAttributes _
instance showTargetGroupAttributes :: Show TargetGroupAttributes where
  show = genericShow
instance decodeTargetGroupAttributes :: Decode TargetGroupAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupAttributes :: Encode TargetGroupAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGroupName = TargetGroupName String
derive instance newtypeTargetGroupName :: Newtype TargetGroupName _
derive instance repGenericTargetGroupName :: Generic TargetGroupName _
instance showTargetGroupName :: Show TargetGroupName where
  show = genericShow
instance decodeTargetGroupName :: Decode TargetGroupName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupName :: Encode TargetGroupName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGroupNames = TargetGroupNames (Array TargetGroupName)
derive instance newtypeTargetGroupNames :: Newtype TargetGroupNames _
derive instance repGenericTargetGroupNames :: Generic TargetGroupNames _
instance showTargetGroupNames :: Show TargetGroupNames where
  show = genericShow
instance decodeTargetGroupNames :: Decode TargetGroupNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupNames :: Encode TargetGroupNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified target group does not exist.</p>
newtype TargetGroupNotFoundException = TargetGroupNotFoundException Types.NoArguments
derive instance newtypeTargetGroupNotFoundException :: Newtype TargetGroupNotFoundException _
derive instance repGenericTargetGroupNotFoundException :: Generic TargetGroupNotFoundException _
instance showTargetGroupNotFoundException :: Show TargetGroupNotFoundException where
  show = genericShow
instance decodeTargetGroupNotFoundException :: Decode TargetGroupNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroupNotFoundException :: Encode TargetGroupNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetGroups = TargetGroups (Array TargetGroup)
derive instance newtypeTargetGroups :: Newtype TargetGroups _
derive instance repGenericTargetGroups :: Generic TargetGroups _
instance showTargetGroups :: Show TargetGroups where
  show = genericShow
instance decodeTargetGroups :: Decode TargetGroups where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetGroups :: Encode TargetGroups where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the current health of a target.</p>
newtype TargetHealth = TargetHealth 
  { "State" :: NullOrUndefined.NullOrUndefined (TargetHealthStateEnum)
  , "Reason" :: NullOrUndefined.NullOrUndefined (TargetHealthReasonEnum)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeTargetHealth :: Newtype TargetHealth _
derive instance repGenericTargetHealth :: Generic TargetHealth _
instance showTargetHealth :: Show TargetHealth where
  show = genericShow
instance decodeTargetHealth :: Decode TargetHealth where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetHealth :: Encode TargetHealth where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the health of a target.</p>
newtype TargetHealthDescription = TargetHealthDescription 
  { "Target" :: NullOrUndefined.NullOrUndefined (TargetDescription)
  , "HealthCheckPort" :: NullOrUndefined.NullOrUndefined (HealthCheckPort)
  , "TargetHealth" :: NullOrUndefined.NullOrUndefined (TargetHealth)
  }
derive instance newtypeTargetHealthDescription :: Newtype TargetHealthDescription _
derive instance repGenericTargetHealthDescription :: Generic TargetHealthDescription _
instance showTargetHealthDescription :: Show TargetHealthDescription where
  show = genericShow
instance decodeTargetHealthDescription :: Decode TargetHealthDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetHealthDescription :: Encode TargetHealthDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetHealthDescriptions = TargetHealthDescriptions (Array TargetHealthDescription)
derive instance newtypeTargetHealthDescriptions :: Newtype TargetHealthDescriptions _
derive instance repGenericTargetHealthDescriptions :: Generic TargetHealthDescriptions _
instance showTargetHealthDescriptions :: Show TargetHealthDescriptions where
  show = genericShow
instance decodeTargetHealthDescriptions :: Decode TargetHealthDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetHealthDescriptions :: Encode TargetHealthDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetHealthReasonEnum = TargetHealthReasonEnum String
derive instance newtypeTargetHealthReasonEnum :: Newtype TargetHealthReasonEnum _
derive instance repGenericTargetHealthReasonEnum :: Generic TargetHealthReasonEnum _
instance showTargetHealthReasonEnum :: Show TargetHealthReasonEnum where
  show = genericShow
instance decodeTargetHealthReasonEnum :: Decode TargetHealthReasonEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetHealthReasonEnum :: Encode TargetHealthReasonEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetHealthStateEnum = TargetHealthStateEnum String
derive instance newtypeTargetHealthStateEnum :: Newtype TargetHealthStateEnum _
derive instance repGenericTargetHealthStateEnum :: Generic TargetHealthStateEnum _
instance showTargetHealthStateEnum :: Show TargetHealthStateEnum where
  show = genericShow
instance decodeTargetHealthStateEnum :: Decode TargetHealthStateEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetHealthStateEnum :: Encode TargetHealthStateEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetId = TargetId String
derive instance newtypeTargetId :: Newtype TargetId _
derive instance repGenericTargetId :: Generic TargetId _
instance showTargetId :: Show TargetId where
  show = genericShow
instance decodeTargetId :: Decode TargetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetId :: Encode TargetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TargetTypeEnum = TargetTypeEnum String
derive instance newtypeTargetTypeEnum :: Newtype TargetTypeEnum _
derive instance repGenericTargetTypeEnum :: Generic TargetTypeEnum _
instance showTargetTypeEnum :: Show TargetTypeEnum where
  show = genericShow
instance decodeTargetTypeEnum :: Decode TargetTypeEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTargetTypeEnum :: Encode TargetTypeEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of certificates per load balancer.</p>
newtype TooManyCertificatesException = TooManyCertificatesException Types.NoArguments
derive instance newtypeTooManyCertificatesException :: Newtype TooManyCertificatesException _
derive instance repGenericTooManyCertificatesException :: Generic TooManyCertificatesException _
instance showTooManyCertificatesException :: Show TooManyCertificatesException where
  show = genericShow
instance decodeTooManyCertificatesException :: Decode TooManyCertificatesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyCertificatesException :: Encode TooManyCertificatesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of listeners per load balancer.</p>
newtype TooManyListenersException = TooManyListenersException Types.NoArguments
derive instance newtypeTooManyListenersException :: Newtype TooManyListenersException _
derive instance repGenericTooManyListenersException :: Generic TooManyListenersException _
instance showTooManyListenersException :: Show TooManyListenersException where
  show = genericShow
instance decodeTooManyListenersException :: Decode TooManyListenersException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyListenersException :: Encode TooManyListenersException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of load balancers for your AWS account.</p>
newtype TooManyLoadBalancersException = TooManyLoadBalancersException Types.NoArguments
derive instance newtypeTooManyLoadBalancersException :: Newtype TooManyLoadBalancersException _
derive instance repGenericTooManyLoadBalancersException :: Generic TooManyLoadBalancersException _
instance showTooManyLoadBalancersException :: Show TooManyLoadBalancersException where
  show = genericShow
instance decodeTooManyLoadBalancersException :: Decode TooManyLoadBalancersException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyLoadBalancersException :: Encode TooManyLoadBalancersException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of times a target can be registered with a load balancer.</p>
newtype TooManyRegistrationsForTargetIdException = TooManyRegistrationsForTargetIdException Types.NoArguments
derive instance newtypeTooManyRegistrationsForTargetIdException :: Newtype TooManyRegistrationsForTargetIdException _
derive instance repGenericTooManyRegistrationsForTargetIdException :: Generic TooManyRegistrationsForTargetIdException _
instance showTooManyRegistrationsForTargetIdException :: Show TooManyRegistrationsForTargetIdException where
  show = genericShow
instance decodeTooManyRegistrationsForTargetIdException :: Decode TooManyRegistrationsForTargetIdException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRegistrationsForTargetIdException :: Encode TooManyRegistrationsForTargetIdException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of rules per load balancer.</p>
newtype TooManyRulesException = TooManyRulesException Types.NoArguments
derive instance newtypeTooManyRulesException :: Newtype TooManyRulesException _
derive instance repGenericTooManyRulesException :: Generic TooManyRulesException _
instance showTooManyRulesException :: Show TooManyRulesException where
  show = genericShow
instance decodeTooManyRulesException :: Decode TooManyRulesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRulesException :: Encode TooManyRulesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of tags per load balancer.</p>
newtype TooManyTagsException = TooManyTagsException Types.NoArguments
derive instance newtypeTooManyTagsException :: Newtype TooManyTagsException _
derive instance repGenericTooManyTagsException :: Generic TooManyTagsException _
instance showTooManyTagsException :: Show TooManyTagsException where
  show = genericShow
instance decodeTooManyTagsException :: Decode TooManyTagsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTagsException :: Encode TooManyTagsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of target groups for your AWS account.</p>
newtype TooManyTargetGroupsException = TooManyTargetGroupsException Types.NoArguments
derive instance newtypeTooManyTargetGroupsException :: Newtype TooManyTargetGroupsException _
derive instance repGenericTooManyTargetGroupsException :: Generic TooManyTargetGroupsException _
instance showTooManyTargetGroupsException :: Show TooManyTargetGroupsException where
  show = genericShow
instance decodeTooManyTargetGroupsException :: Decode TooManyTargetGroupsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTargetGroupsException :: Encode TooManyTargetGroupsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of targets.</p>
newtype TooManyTargetsException = TooManyTargetsException Types.NoArguments
derive instance newtypeTooManyTargetsException :: Newtype TooManyTargetsException _
derive instance repGenericTooManyTargetsException :: Generic TooManyTargetsException _
instance showTooManyTargetsException :: Show TooManyTargetsException where
  show = genericShow
instance decodeTooManyTargetsException :: Decode TooManyTargetsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTargetsException :: Encode TooManyTargetsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified protocol is not supported.</p>
newtype UnsupportedProtocolException = UnsupportedProtocolException Types.NoArguments
derive instance newtypeUnsupportedProtocolException :: Newtype UnsupportedProtocolException _
derive instance repGenericUnsupportedProtocolException :: Generic UnsupportedProtocolException _
instance showUnsupportedProtocolException :: Show UnsupportedProtocolException where
  show = genericShow
instance decodeUnsupportedProtocolException :: Decode UnsupportedProtocolException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedProtocolException :: Encode UnsupportedProtocolException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcId = VpcId String
derive instance newtypeVpcId :: Newtype VpcId _
derive instance repGenericVpcId :: Generic VpcId _
instance showVpcId :: Show VpcId where
  show = genericShow
instance decodeVpcId :: Decode VpcId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcId :: Encode VpcId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ZoneName = ZoneName String
derive instance newtypeZoneName :: Newtype ZoneName _
derive instance repGenericZoneName :: Generic ZoneName _
instance showZoneName :: Show ZoneName where
  show = genericShow
instance decodeZoneName :: Decode ZoneName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeZoneName :: Encode ZoneName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
