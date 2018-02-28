

-- | <fullname>Elastic Load Balancing</fullname> <p>A load balancer can distribute incoming traffic across your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered instances and ensures that it routes traffic only to healthy instances. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer and a protocol and port number for connections from the load balancer to the instances.</p> <p>Elastic Load Balancing supports three types of load balancers: Application Load Balancers, Network Load Balancers, and Classic Load Balancers. You can select a load balancer based on your application needs. For more information, see the <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/">Elastic Load Balancing User Guide</a>.</p> <p>This reference covers the 2012-06-01 API, which supports Classic Load Balancers. The 2015-12-01 API supports Application Load Balancers and Network Load Balancers.</p> <p>To get started, create a load balancer with one or more listeners using <a>CreateLoadBalancer</a>. Register your instances with the load balancer using <a>RegisterInstancesWithLoadBalancer</a>.</p> <p>All Elastic Load Balancing operations are <i>idempotent</i>, which means that they complete at most one time. If you repeat an operation, it succeeds with a 200 OK response code.</p>
module AWS.ELB where

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

serviceName = "ELB" :: String


-- | <p>Adds the specified tags to the specified load balancer. Each load balancer can have a maximum of 10 tags.</p> <p>Each tag consists of a key and an optional value. If a tag with the same key is already associated with the load balancer, <code>AddTags</code> updates its value.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html">Tag Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
addTags :: forall eff. AddTagsInput -> Aff (exception :: EXCEPTION | eff) AddTagsOutput
addTags = Request.request serviceName "addTags" 


-- | <p>Associates one or more security groups with your load balancer in a virtual private cloud (VPC). The specified security groups override the previously associated security groups.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-security-groups.html#elb-vpc-security-groups">Security Groups for Load Balancers in a VPC</a> in the <i>Classic Load Balancer Guide</i>.</p>
applySecurityGroupsToLoadBalancer :: forall eff. ApplySecurityGroupsToLoadBalancerInput -> Aff (exception :: EXCEPTION | eff) ApplySecurityGroupsToLoadBalancerOutput
applySecurityGroupsToLoadBalancer = Request.request serviceName "applySecurityGroupsToLoadBalancer" 


-- | <p>Adds one or more subnets to the set of configured subnets for the specified load balancer.</p> <p>The load balancer evenly distributes requests across all registered subnets. For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-manage-subnets.html">Add or Remove Subnets for Your Load Balancer in a VPC</a> in the <i>Classic Load Balancer Guide</i>.</p>
attachLoadBalancerToSubnets :: forall eff. AttachLoadBalancerToSubnetsInput -> Aff (exception :: EXCEPTION | eff) AttachLoadBalancerToSubnetsOutput
attachLoadBalancerToSubnets = Request.request serviceName "attachLoadBalancerToSubnets" 


-- | <p>Specifies the health check settings to use when evaluating the health state of your EC2 instances.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-healthchecks.html">Configure Health Checks for Your Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
configureHealthCheck :: forall eff. ConfigureHealthCheckInput -> Aff (exception :: EXCEPTION | eff) ConfigureHealthCheckOutput
configureHealthCheck = Request.request serviceName "configureHealthCheck" 


-- | <p>Generates a stickiness policy with sticky session lifetimes that follow that of an application-generated cookie. This policy can be associated only with HTTP/HTTPS listeners.</p> <p>This policy is similar to the policy created by <a>CreateLBCookieStickinessPolicy</a>, except that the lifetime of the special Elastic Load Balancing cookie, <code>AWSELB</code>, follows the lifetime of the application-generated cookie specified in the policy configuration. The load balancer only inserts a new stickiness cookie when the application response includes a new application cookie.</p> <p>If the application cookie is explicitly removed or expires, the session stops being sticky until a new application cookie is issued.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application">Application-Controlled Session Stickiness</a> in the <i>Classic Load Balancer Guide</i>.</p>
createAppCookieStickinessPolicy :: forall eff. CreateAppCookieStickinessPolicyInput -> Aff (exception :: EXCEPTION | eff) CreateAppCookieStickinessPolicyOutput
createAppCookieStickinessPolicy = Request.request serviceName "createAppCookieStickinessPolicy" 


-- | <p>Generates a stickiness policy with sticky session lifetimes controlled by the lifetime of the browser (user-agent) or a specified expiration period. This policy can be associated only with HTTP/HTTPS listeners.</p> <p>When a load balancer implements this policy, the load balancer uses a special cookie to track the instance for each request. When the load balancer receives a request, it first checks to see if this cookie is present in the request. If so, the load balancer sends the request to the application server specified in the cookie. If not, the load balancer sends the request to a server that is chosen based on the existing load-balancing algorithm.</p> <p>A cookie is inserted into the response for binding subsequent requests from the same user to that server. The validity of the cookie is based on the cookie expiration time, which is specified in the policy configuration.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration">Duration-Based Session Stickiness</a> in the <i>Classic Load Balancer Guide</i>.</p>
createLBCookieStickinessPolicy :: forall eff. CreateLBCookieStickinessPolicyInput -> Aff (exception :: EXCEPTION | eff) CreateLBCookieStickinessPolicyOutput
createLBCookieStickinessPolicy = Request.request serviceName "createLBCookieStickinessPolicy" 


-- | <p>Creates a Classic Load Balancer.</p> <p>You can add listeners, security groups, subnets, and tags when you create your load balancer, or you can add them later using <a>CreateLoadBalancerListeners</a>, <a>ApplySecurityGroupsToLoadBalancer</a>, <a>AttachLoadBalancerToSubnets</a>, and <a>AddTags</a>.</p> <p>To describe your current load balancers, see <a>DescribeLoadBalancers</a>. When you are finished with a load balancer, you can delete it using <a>DeleteLoadBalancer</a>.</p> <p>You can create up to 20 load balancers per region per account. You can request an increase for the number of load balancers for your account. For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-limits.html">Limits for Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
createLoadBalancer :: forall eff. CreateAccessPointInput -> Aff (exception :: EXCEPTION | eff) CreateAccessPointOutput
createLoadBalancer = Request.request serviceName "createLoadBalancer" 


-- | <p>Creates one or more listeners for the specified load balancer. If a listener with the specified port does not already exist, it is created; otherwise, the properties of the new listener must match the properties of the existing listener.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html">Listeners for Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
createLoadBalancerListeners :: forall eff. CreateLoadBalancerListenerInput -> Aff (exception :: EXCEPTION | eff) CreateLoadBalancerListenerOutput
createLoadBalancerListeners = Request.request serviceName "createLoadBalancerListeners" 


-- | <p>Creates a policy with the specified attributes for the specified load balancer.</p> <p>Policies are settings that are saved for your load balancer and that can be applied to the listener or the application server, depending on the policy type.</p>
createLoadBalancerPolicy :: forall eff. CreateLoadBalancerPolicyInput -> Aff (exception :: EXCEPTION | eff) CreateLoadBalancerPolicyOutput
createLoadBalancerPolicy = Request.request serviceName "createLoadBalancerPolicy" 


-- | <p>Deletes the specified load balancer.</p> <p>If you are attempting to recreate a load balancer, you must reconfigure all settings. The DNS name associated with a deleted load balancer are no longer usable. The name and associated DNS record of the deleted load balancer no longer exist and traffic sent to any of its IP addresses is no longer delivered to your instances.</p> <p>If the load balancer does not exist or has already been deleted, the call to <code>DeleteLoadBalancer</code> still succeeds.</p>
deleteLoadBalancer :: forall eff. DeleteAccessPointInput -> Aff (exception :: EXCEPTION | eff) DeleteAccessPointOutput
deleteLoadBalancer = Request.request serviceName "deleteLoadBalancer" 


-- | <p>Deletes the specified listeners from the specified load balancer.</p>
deleteLoadBalancerListeners :: forall eff. DeleteLoadBalancerListenerInput -> Aff (exception :: EXCEPTION | eff) DeleteLoadBalancerListenerOutput
deleteLoadBalancerListeners = Request.request serviceName "deleteLoadBalancerListeners" 


-- | <p>Deletes the specified policy from the specified load balancer. This policy must not be enabled for any listeners.</p>
deleteLoadBalancerPolicy :: forall eff. DeleteLoadBalancerPolicyInput -> Aff (exception :: EXCEPTION | eff) DeleteLoadBalancerPolicyOutput
deleteLoadBalancerPolicy = Request.request serviceName "deleteLoadBalancerPolicy" 


-- | <p>Deregisters the specified instances from the specified load balancer. After the instance is deregistered, it no longer receives traffic from the load balancer.</p> <p>You can use <a>DescribeLoadBalancers</a> to verify that the instance is deregistered from the load balancer.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html">Register or De-Register EC2 Instances</a> in the <i>Classic Load Balancer Guide</i>.</p>
deregisterInstancesFromLoadBalancer :: forall eff. DeregisterEndPointsInput -> Aff (exception :: EXCEPTION | eff) DeregisterEndPointsOutput
deregisterInstancesFromLoadBalancer = Request.request serviceName "deregisterInstancesFromLoadBalancer" 


-- | <p>Describes the current Elastic Load Balancing resource limits for your AWS account.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-limits.html">Limits for Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
describeAccountLimits :: forall eff. DescribeAccountLimitsInput -> Aff (exception :: EXCEPTION | eff) DescribeAccountLimitsOutput
describeAccountLimits = Request.request serviceName "describeAccountLimits" 


-- | <p>Describes the state of the specified instances with respect to the specified load balancer. If no instances are specified, the call describes the state of all instances that are currently registered with the load balancer. If instances are specified, their state is returned even if they are no longer registered with the load balancer. The state of terminated instances is not returned.</p>
describeInstanceHealth :: forall eff. DescribeEndPointStateInput -> Aff (exception :: EXCEPTION | eff) DescribeEndPointStateOutput
describeInstanceHealth = Request.request serviceName "describeInstanceHealth" 


-- | <p>Describes the attributes for the specified load balancer.</p>
describeLoadBalancerAttributes :: forall eff. DescribeLoadBalancerAttributesInput -> Aff (exception :: EXCEPTION | eff) DescribeLoadBalancerAttributesOutput
describeLoadBalancerAttributes = Request.request serviceName "describeLoadBalancerAttributes" 


-- | <p>Describes the specified policies.</p> <p>If you specify a load balancer name, the action returns the descriptions of all policies created for the load balancer. If you specify a policy name associated with your load balancer, the action returns the description of that policy. If you don't specify a load balancer name, the action returns descriptions of the specified sample policies, or descriptions of all sample policies. The names of the sample policies have the <code>ELBSample-</code> prefix.</p>
describeLoadBalancerPolicies :: forall eff. DescribeLoadBalancerPoliciesInput -> Aff (exception :: EXCEPTION | eff) DescribeLoadBalancerPoliciesOutput
describeLoadBalancerPolicies = Request.request serviceName "describeLoadBalancerPolicies" 


-- | <p>Describes the specified load balancer policy types or all load balancer policy types.</p> <p>The description of each type indicates how it can be used. For example, some policies can be used only with layer 7 listeners, some policies can be used only with layer 4 listeners, and some policies can be used only with your EC2 instances.</p> <p>You can use <a>CreateLoadBalancerPolicy</a> to create a policy configuration for any of these policy types. Then, depending on the policy type, use either <a>SetLoadBalancerPoliciesOfListener</a> or <a>SetLoadBalancerPoliciesForBackendServer</a> to set the policy.</p>
describeLoadBalancerPolicyTypes :: forall eff. DescribeLoadBalancerPolicyTypesInput -> Aff (exception :: EXCEPTION | eff) DescribeLoadBalancerPolicyTypesOutput
describeLoadBalancerPolicyTypes = Request.request serviceName "describeLoadBalancerPolicyTypes" 


-- | <p>Describes the specified the load balancers. If no load balancers are specified, the call describes all of your load balancers.</p>
describeLoadBalancers :: forall eff. DescribeAccessPointsInput -> Aff (exception :: EXCEPTION | eff) DescribeAccessPointsOutput
describeLoadBalancers = Request.request serviceName "describeLoadBalancers" 


-- | <p>Describes the tags associated with the specified load balancers.</p>
describeTags :: forall eff. DescribeTagsInput -> Aff (exception :: EXCEPTION | eff) DescribeTagsOutput
describeTags = Request.request serviceName "describeTags" 


-- | <p>Removes the specified subnets from the set of configured subnets for the load balancer.</p> <p>After a subnet is removed, all EC2 instances registered with the load balancer in the removed subnet go into the <code>OutOfService</code> state. Then, the load balancer balances the traffic among the remaining routable subnets.</p>
detachLoadBalancerFromSubnets :: forall eff. DetachLoadBalancerFromSubnetsInput -> Aff (exception :: EXCEPTION | eff) DetachLoadBalancerFromSubnetsOutput
detachLoadBalancerFromSubnets = Request.request serviceName "detachLoadBalancerFromSubnets" 


-- | <p>Removes the specified Availability Zones from the set of Availability Zones for the specified load balancer.</p> <p>There must be at least one Availability Zone registered with a load balancer at all times. After an Availability Zone is removed, all instances registered with the load balancer that are in the removed Availability Zone go into the <code>OutOfService</code> state. Then, the load balancer attempts to equally balance the traffic among its remaining Availability Zones.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html">Add or Remove Availability Zones</a> in the <i>Classic Load Balancer Guide</i>.</p>
disableAvailabilityZonesForLoadBalancer :: forall eff. RemoveAvailabilityZonesInput -> Aff (exception :: EXCEPTION | eff) RemoveAvailabilityZonesOutput
disableAvailabilityZonesForLoadBalancer = Request.request serviceName "disableAvailabilityZonesForLoadBalancer" 


-- | <p>Adds the specified Availability Zones to the set of Availability Zones for the specified load balancer.</p> <p>The load balancer evenly distributes requests across all its registered Availability Zones that contain instances.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html">Add or Remove Availability Zones</a> in the <i>Classic Load Balancer Guide</i>.</p>
enableAvailabilityZonesForLoadBalancer :: forall eff. AddAvailabilityZonesInput -> Aff (exception :: EXCEPTION | eff) AddAvailabilityZonesOutput
enableAvailabilityZonesForLoadBalancer = Request.request serviceName "enableAvailabilityZonesForLoadBalancer" 


-- | <p>Modifies the attributes of the specified load balancer.</p> <p>You can modify the load balancer attributes, such as <code>AccessLogs</code>, <code>ConnectionDraining</code>, and <code>CrossZoneLoadBalancing</code> by either enabling or disabling them. Or, you can modify the load balancer attribute <code>ConnectionSettings</code> by specifying an idle connection timeout value for your load balancer.</p> <p>For more information, see the following in the <i>Classic Load Balancer Guide</i>:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html">Cross-Zone Load Balancing</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html">Connection Draining</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/access-log-collection.html">Access Logs</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html">Idle Connection Timeout</a> </p> </li> </ul>
modifyLoadBalancerAttributes :: forall eff. ModifyLoadBalancerAttributesInput -> Aff (exception :: EXCEPTION | eff) ModifyLoadBalancerAttributesOutput
modifyLoadBalancerAttributes = Request.request serviceName "modifyLoadBalancerAttributes" 


-- | <p>Adds the specified instances to the specified load balancer.</p> <p>The instance must be a running instance in the same network as the load balancer (EC2-Classic or the same VPC). If you have EC2-Classic instances and a load balancer in a VPC with ClassicLink enabled, you can link the EC2-Classic instances to that VPC and then register the linked EC2-Classic instances with the load balancer in the VPC.</p> <p>Note that <code>RegisterInstanceWithLoadBalancer</code> completes when the request has been registered. Instance registration takes a little time to complete. To check the state of the registered instances, use <a>DescribeLoadBalancers</a> or <a>DescribeInstanceHealth</a>.</p> <p>After the instance is registered, it starts receiving traffic and requests from the load balancer. Any instance that is not in one of the Availability Zones registered for the load balancer is moved to the <code>OutOfService</code> state. If an Availability Zone is added to the load balancer later, any instances registered with the load balancer move to the <code>InService</code> state.</p> <p>To deregister instances from a load balancer, use <a>DeregisterInstancesFromLoadBalancer</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html">Register or De-Register EC2 Instances</a> in the <i>Classic Load Balancer Guide</i>.</p>
registerInstancesWithLoadBalancer :: forall eff. RegisterEndPointsInput -> Aff (exception :: EXCEPTION | eff) RegisterEndPointsOutput
registerInstancesWithLoadBalancer = Request.request serviceName "registerInstancesWithLoadBalancer" 


-- | <p>Removes one or more tags from the specified load balancer.</p>
removeTags :: forall eff. RemoveTagsInput -> Aff (exception :: EXCEPTION | eff) RemoveTagsOutput
removeTags = Request.request serviceName "removeTags" 


-- | <p>Sets the certificate that terminates the specified listener's SSL connections. The specified certificate replaces any prior certificate that was used on the same load balancer and port.</p> <p>For more information about updating your SSL certificate, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-update-ssl-cert.html">Replace the SSL Certificate for Your Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
setLoadBalancerListenerSSLCertificate :: forall eff. SetLoadBalancerListenerSSLCertificateInput -> Aff (exception :: EXCEPTION | eff) SetLoadBalancerListenerSSLCertificateOutput
setLoadBalancerListenerSSLCertificate = Request.request serviceName "setLoadBalancerListenerSSLCertificate" 


-- | <p>Replaces the set of policies associated with the specified port on which the EC2 instance is listening with a new set of policies. At this time, only the back-end server authentication policy type can be applied to the instance ports; this policy type is composed of multiple public key policies.</p> <p>Each time you use <code>SetLoadBalancerPoliciesForBackendServer</code> to enable the policies, use the <code>PolicyNames</code> parameter to list the policies that you want to enable.</p> <p>You can use <a>DescribeLoadBalancers</a> or <a>DescribeLoadBalancerPolicies</a> to verify that the policy is associated with the EC2 instance.</p> <p>For more information about enabling back-end instance authentication, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-create-https-ssl-load-balancer.html#configure_backendauth_clt">Configure Back-end Instance Authentication</a> in the <i>Classic Load Balancer Guide</i>. For more information about Proxy Protocol, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-proxy-protocol.html">Configure Proxy Protocol Support</a> in the <i>Classic Load Balancer Guide</i>.</p>
setLoadBalancerPoliciesForBackendServer :: forall eff. SetLoadBalancerPoliciesForBackendServerInput -> Aff (exception :: EXCEPTION | eff) SetLoadBalancerPoliciesForBackendServerOutput
setLoadBalancerPoliciesForBackendServer = Request.request serviceName "setLoadBalancerPoliciesForBackendServer" 


-- | <p>Replaces the current set of policies for the specified load balancer port with the specified set of policies.</p> <p>To enable back-end server authentication, use <a>SetLoadBalancerPoliciesForBackendServer</a>.</p> <p>For more information about setting policies, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/ssl-config-update.html">Update the SSL Negotiation Configuration</a>, <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration">Duration-Based Session Stickiness</a>, and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application">Application-Controlled Session Stickiness</a> in the <i>Classic Load Balancer Guide</i>.</p>
setLoadBalancerPoliciesOfListener :: forall eff. SetLoadBalancerPoliciesOfListenerInput -> Aff (exception :: EXCEPTION | eff) SetLoadBalancerPoliciesOfListenerOutput
setLoadBalancerPoliciesOfListener = Request.request serviceName "setLoadBalancerPoliciesOfListener" 


-- | <p>Information about the <code>AccessLog</code> attribute.</p>
newtype AccessLog = AccessLog 
  { "Enabled" :: (AccessLogEnabled)
  , "S3BucketName" :: NullOrUndefined.NullOrUndefined (S3BucketName)
  , "EmitInterval" :: NullOrUndefined.NullOrUndefined (AccessLogInterval)
  , "S3BucketPrefix" :: NullOrUndefined.NullOrUndefined (AccessLogPrefix)
  }
derive instance newtypeAccessLog :: Newtype AccessLog _
derive instance repGenericAccessLog :: Generic AccessLog _
instance showAccessLog :: Show AccessLog where
  show = genericShow
instance decodeAccessLog :: Decode AccessLog where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessLog :: Encode AccessLog where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessLogEnabled = AccessLogEnabled Boolean
derive instance newtypeAccessLogEnabled :: Newtype AccessLogEnabled _
derive instance repGenericAccessLogEnabled :: Generic AccessLogEnabled _
instance showAccessLogEnabled :: Show AccessLogEnabled where
  show = genericShow
instance decodeAccessLogEnabled :: Decode AccessLogEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessLogEnabled :: Encode AccessLogEnabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessLogInterval = AccessLogInterval Int
derive instance newtypeAccessLogInterval :: Newtype AccessLogInterval _
derive instance repGenericAccessLogInterval :: Generic AccessLogInterval _
instance showAccessLogInterval :: Show AccessLogInterval where
  show = genericShow
instance decodeAccessLogInterval :: Decode AccessLogInterval where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessLogInterval :: Encode AccessLogInterval where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessLogPrefix = AccessLogPrefix String
derive instance newtypeAccessLogPrefix :: Newtype AccessLogPrefix _
derive instance repGenericAccessLogPrefix :: Generic AccessLogPrefix _
instance showAccessLogPrefix :: Show AccessLogPrefix where
  show = genericShow
instance decodeAccessLogPrefix :: Decode AccessLogPrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessLogPrefix :: Encode AccessLogPrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessPointName = AccessPointName String
derive instance newtypeAccessPointName :: Newtype AccessPointName _
derive instance repGenericAccessPointName :: Generic AccessPointName _
instance showAccessPointName :: Show AccessPointName where
  show = genericShow
instance decodeAccessPointName :: Decode AccessPointName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessPointName :: Encode AccessPointName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified load balancer does not exist.</p>
newtype AccessPointNotFoundException = AccessPointNotFoundException Types.NoArguments
derive instance newtypeAccessPointNotFoundException :: Newtype AccessPointNotFoundException _
derive instance repGenericAccessPointNotFoundException :: Generic AccessPointNotFoundException _
instance showAccessPointNotFoundException :: Show AccessPointNotFoundException where
  show = genericShow
instance decodeAccessPointNotFoundException :: Decode AccessPointNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessPointNotFoundException :: Encode AccessPointNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessPointPort = AccessPointPort Int
derive instance newtypeAccessPointPort :: Newtype AccessPointPort _
derive instance repGenericAccessPointPort :: Generic AccessPointPort _
instance showAccessPointPort :: Show AccessPointPort where
  show = genericShow
instance decodeAccessPointPort :: Decode AccessPointPort where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessPointPort :: Encode AccessPointPort where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for EnableAvailabilityZonesForLoadBalancer.</p>
newtype AddAvailabilityZonesInput = AddAvailabilityZonesInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "AvailabilityZones" :: (AvailabilityZones)
  }
derive instance newtypeAddAvailabilityZonesInput :: Newtype AddAvailabilityZonesInput _
derive instance repGenericAddAvailabilityZonesInput :: Generic AddAvailabilityZonesInput _
instance showAddAvailabilityZonesInput :: Show AddAvailabilityZonesInput where
  show = genericShow
instance decodeAddAvailabilityZonesInput :: Decode AddAvailabilityZonesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddAvailabilityZonesInput :: Encode AddAvailabilityZonesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of EnableAvailabilityZonesForLoadBalancer.</p>
newtype AddAvailabilityZonesOutput = AddAvailabilityZonesOutput 
  { "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZones)
  }
derive instance newtypeAddAvailabilityZonesOutput :: Newtype AddAvailabilityZonesOutput _
derive instance repGenericAddAvailabilityZonesOutput :: Generic AddAvailabilityZonesOutput _
instance showAddAvailabilityZonesOutput :: Show AddAvailabilityZonesOutput where
  show = genericShow
instance decodeAddAvailabilityZonesOutput :: Decode AddAvailabilityZonesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddAvailabilityZonesOutput :: Encode AddAvailabilityZonesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for AddTags.</p>
newtype AddTagsInput = AddTagsInput 
  { "LoadBalancerNames" :: (LoadBalancerNames)
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


-- | <p>Contains the output of AddTags.</p>
newtype AddTagsOutput = AddTagsOutput Types.NoArguments
derive instance newtypeAddTagsOutput :: Newtype AddTagsOutput _
derive instance repGenericAddTagsOutput :: Generic AddTagsOutput _
instance showAddTagsOutput :: Show AddTagsOutput where
  show = genericShow
instance decodeAddTagsOutput :: Decode AddTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsOutput :: Encode AddTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This data type is reserved.</p>
newtype AdditionalAttribute = AdditionalAttribute 
  { "Key" :: NullOrUndefined.NullOrUndefined (AdditionalAttributeKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (AdditionalAttributeValue)
  }
derive instance newtypeAdditionalAttribute :: Newtype AdditionalAttribute _
derive instance repGenericAdditionalAttribute :: Generic AdditionalAttribute _
instance showAdditionalAttribute :: Show AdditionalAttribute where
  show = genericShow
instance decodeAdditionalAttribute :: Decode AdditionalAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalAttribute :: Encode AdditionalAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdditionalAttributeKey = AdditionalAttributeKey String
derive instance newtypeAdditionalAttributeKey :: Newtype AdditionalAttributeKey _
derive instance repGenericAdditionalAttributeKey :: Generic AdditionalAttributeKey _
instance showAdditionalAttributeKey :: Show AdditionalAttributeKey where
  show = genericShow
instance decodeAdditionalAttributeKey :: Decode AdditionalAttributeKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalAttributeKey :: Encode AdditionalAttributeKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdditionalAttributeValue = AdditionalAttributeValue String
derive instance newtypeAdditionalAttributeValue :: Newtype AdditionalAttributeValue _
derive instance repGenericAdditionalAttributeValue :: Generic AdditionalAttributeValue _
instance showAdditionalAttributeValue :: Show AdditionalAttributeValue where
  show = genericShow
instance decodeAdditionalAttributeValue :: Decode AdditionalAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalAttributeValue :: Encode AdditionalAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AdditionalAttributes = AdditionalAttributes (Array AdditionalAttribute)
derive instance newtypeAdditionalAttributes :: Newtype AdditionalAttributes _
derive instance repGenericAdditionalAttributes :: Generic AdditionalAttributes _
instance showAdditionalAttributes :: Show AdditionalAttributes where
  show = genericShow
instance decodeAdditionalAttributes :: Decode AdditionalAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAdditionalAttributes :: Encode AdditionalAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AppCookieStickinessPolicies = AppCookieStickinessPolicies (Array AppCookieStickinessPolicy)
derive instance newtypeAppCookieStickinessPolicies :: Newtype AppCookieStickinessPolicies _
derive instance repGenericAppCookieStickinessPolicies :: Generic AppCookieStickinessPolicies _
instance showAppCookieStickinessPolicies :: Show AppCookieStickinessPolicies where
  show = genericShow
instance decodeAppCookieStickinessPolicies :: Decode AppCookieStickinessPolicies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAppCookieStickinessPolicies :: Encode AppCookieStickinessPolicies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a policy for application-controlled session stickiness.</p>
newtype AppCookieStickinessPolicy = AppCookieStickinessPolicy 
  { "PolicyName" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "CookieName" :: NullOrUndefined.NullOrUndefined (CookieName)
  }
derive instance newtypeAppCookieStickinessPolicy :: Newtype AppCookieStickinessPolicy _
derive instance repGenericAppCookieStickinessPolicy :: Generic AppCookieStickinessPolicy _
instance showAppCookieStickinessPolicy :: Show AppCookieStickinessPolicy where
  show = genericShow
instance decodeAppCookieStickinessPolicy :: Decode AppCookieStickinessPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAppCookieStickinessPolicy :: Encode AppCookieStickinessPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for ApplySecurityGroupsToLoadBalancer.</p>
newtype ApplySecurityGroupsToLoadBalancerInput = ApplySecurityGroupsToLoadBalancerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "SecurityGroups" :: (SecurityGroups)
  }
derive instance newtypeApplySecurityGroupsToLoadBalancerInput :: Newtype ApplySecurityGroupsToLoadBalancerInput _
derive instance repGenericApplySecurityGroupsToLoadBalancerInput :: Generic ApplySecurityGroupsToLoadBalancerInput _
instance showApplySecurityGroupsToLoadBalancerInput :: Show ApplySecurityGroupsToLoadBalancerInput where
  show = genericShow
instance decodeApplySecurityGroupsToLoadBalancerInput :: Decode ApplySecurityGroupsToLoadBalancerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplySecurityGroupsToLoadBalancerInput :: Encode ApplySecurityGroupsToLoadBalancerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of ApplySecurityGroupsToLoadBalancer.</p>
newtype ApplySecurityGroupsToLoadBalancerOutput = ApplySecurityGroupsToLoadBalancerOutput 
  { "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  }
derive instance newtypeApplySecurityGroupsToLoadBalancerOutput :: Newtype ApplySecurityGroupsToLoadBalancerOutput _
derive instance repGenericApplySecurityGroupsToLoadBalancerOutput :: Generic ApplySecurityGroupsToLoadBalancerOutput _
instance showApplySecurityGroupsToLoadBalancerOutput :: Show ApplySecurityGroupsToLoadBalancerOutput where
  show = genericShow
instance decodeApplySecurityGroupsToLoadBalancerOutput :: Decode ApplySecurityGroupsToLoadBalancerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplySecurityGroupsToLoadBalancerOutput :: Encode ApplySecurityGroupsToLoadBalancerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for AttachLoaBalancerToSubnets.</p>
newtype AttachLoadBalancerToSubnetsInput = AttachLoadBalancerToSubnetsInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Subnets" :: (Subnets)
  }
derive instance newtypeAttachLoadBalancerToSubnetsInput :: Newtype AttachLoadBalancerToSubnetsInput _
derive instance repGenericAttachLoadBalancerToSubnetsInput :: Generic AttachLoadBalancerToSubnetsInput _
instance showAttachLoadBalancerToSubnetsInput :: Show AttachLoadBalancerToSubnetsInput where
  show = genericShow
instance decodeAttachLoadBalancerToSubnetsInput :: Decode AttachLoadBalancerToSubnetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachLoadBalancerToSubnetsInput :: Encode AttachLoadBalancerToSubnetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of AttachLoadBalancerToSubnets.</p>
newtype AttachLoadBalancerToSubnetsOutput = AttachLoadBalancerToSubnetsOutput 
  { "Subnets" :: NullOrUndefined.NullOrUndefined (Subnets)
  }
derive instance newtypeAttachLoadBalancerToSubnetsOutput :: Newtype AttachLoadBalancerToSubnetsOutput _
derive instance repGenericAttachLoadBalancerToSubnetsOutput :: Generic AttachLoadBalancerToSubnetsOutput _
instance showAttachLoadBalancerToSubnetsOutput :: Show AttachLoadBalancerToSubnetsOutput where
  show = genericShow
instance decodeAttachLoadBalancerToSubnetsOutput :: Decode AttachLoadBalancerToSubnetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttachLoadBalancerToSubnetsOutput :: Encode AttachLoadBalancerToSubnetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _
derive instance repGenericAttributeName :: Generic AttributeName _
instance showAttributeName :: Show AttributeName where
  show = genericShow
instance decodeAttributeName :: Decode AttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeName :: Encode AttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeType = AttributeType String
derive instance newtypeAttributeType :: Newtype AttributeType _
derive instance repGenericAttributeType :: Generic AttributeType _
instance showAttributeType :: Show AttributeType where
  show = genericShow
instance decodeAttributeType :: Decode AttributeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeType :: Encode AttributeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZone = AvailabilityZone String
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _
derive instance repGenericAvailabilityZone :: Generic AvailabilityZone _
instance showAvailabilityZone :: Show AvailabilityZone where
  show = genericShow
instance decodeAvailabilityZone :: Decode AvailabilityZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZone :: Encode AvailabilityZone where
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


-- | <p>Information about the configuration of an EC2 instance.</p>
newtype BackendServerDescription = BackendServerDescription 
  { "InstancePort" :: NullOrUndefined.NullOrUndefined (InstancePort)
  , "PolicyNames" :: NullOrUndefined.NullOrUndefined (PolicyNames)
  }
derive instance newtypeBackendServerDescription :: Newtype BackendServerDescription _
derive instance repGenericBackendServerDescription :: Generic BackendServerDescription _
instance showBackendServerDescription :: Show BackendServerDescription where
  show = genericShow
instance decodeBackendServerDescription :: Decode BackendServerDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackendServerDescription :: Encode BackendServerDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BackendServerDescriptions = BackendServerDescriptions (Array BackendServerDescription)
derive instance newtypeBackendServerDescriptions :: Newtype BackendServerDescriptions _
derive instance repGenericBackendServerDescriptions :: Generic BackendServerDescriptions _
instance showBackendServerDescriptions :: Show BackendServerDescriptions where
  show = genericShow
instance decodeBackendServerDescriptions :: Decode BackendServerDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBackendServerDescriptions :: Encode BackendServerDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Cardinality = Cardinality String
derive instance newtypeCardinality :: Newtype Cardinality _
derive instance repGenericCardinality :: Generic Cardinality _
instance showCardinality :: Show Cardinality where
  show = genericShow
instance decodeCardinality :: Decode Cardinality where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCardinality :: Encode Cardinality where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified ARN does not refer to a valid SSL certificate in AWS Identity and Access Management (IAM) or AWS Certificate Manager (ACM). Note that if you recently uploaded the certificate to IAM, this error might indicate that the certificate is not fully available yet.</p>
newtype CertificateNotFoundException = CertificateNotFoundException Types.NoArguments
derive instance newtypeCertificateNotFoundException :: Newtype CertificateNotFoundException _
derive instance repGenericCertificateNotFoundException :: Generic CertificateNotFoundException _
instance showCertificateNotFoundException :: Show CertificateNotFoundException where
  show = genericShow
instance decodeCertificateNotFoundException :: Decode CertificateNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateNotFoundException :: Encode CertificateNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for ConfigureHealthCheck.</p>
newtype ConfigureHealthCheckInput = ConfigureHealthCheckInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "HealthCheck" :: (HealthCheck)
  }
derive instance newtypeConfigureHealthCheckInput :: Newtype ConfigureHealthCheckInput _
derive instance repGenericConfigureHealthCheckInput :: Generic ConfigureHealthCheckInput _
instance showConfigureHealthCheckInput :: Show ConfigureHealthCheckInput where
  show = genericShow
instance decodeConfigureHealthCheckInput :: Decode ConfigureHealthCheckInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigureHealthCheckInput :: Encode ConfigureHealthCheckInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of ConfigureHealthCheck.</p>
newtype ConfigureHealthCheckOutput = ConfigureHealthCheckOutput 
  { "HealthCheck" :: NullOrUndefined.NullOrUndefined (HealthCheck)
  }
derive instance newtypeConfigureHealthCheckOutput :: Newtype ConfigureHealthCheckOutput _
derive instance repGenericConfigureHealthCheckOutput :: Generic ConfigureHealthCheckOutput _
instance showConfigureHealthCheckOutput :: Show ConfigureHealthCheckOutput where
  show = genericShow
instance decodeConfigureHealthCheckOutput :: Decode ConfigureHealthCheckOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigureHealthCheckOutput :: Encode ConfigureHealthCheckOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the <code>ConnectionDraining</code> attribute.</p>
newtype ConnectionDraining = ConnectionDraining 
  { "Enabled" :: (ConnectionDrainingEnabled)
  , "Timeout" :: NullOrUndefined.NullOrUndefined (ConnectionDrainingTimeout)
  }
derive instance newtypeConnectionDraining :: Newtype ConnectionDraining _
derive instance repGenericConnectionDraining :: Generic ConnectionDraining _
instance showConnectionDraining :: Show ConnectionDraining where
  show = genericShow
instance decodeConnectionDraining :: Decode ConnectionDraining where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionDraining :: Encode ConnectionDraining where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectionDrainingEnabled = ConnectionDrainingEnabled Boolean
derive instance newtypeConnectionDrainingEnabled :: Newtype ConnectionDrainingEnabled _
derive instance repGenericConnectionDrainingEnabled :: Generic ConnectionDrainingEnabled _
instance showConnectionDrainingEnabled :: Show ConnectionDrainingEnabled where
  show = genericShow
instance decodeConnectionDrainingEnabled :: Decode ConnectionDrainingEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionDrainingEnabled :: Encode ConnectionDrainingEnabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectionDrainingTimeout = ConnectionDrainingTimeout Int
derive instance newtypeConnectionDrainingTimeout :: Newtype ConnectionDrainingTimeout _
derive instance repGenericConnectionDrainingTimeout :: Generic ConnectionDrainingTimeout _
instance showConnectionDrainingTimeout :: Show ConnectionDrainingTimeout where
  show = genericShow
instance decodeConnectionDrainingTimeout :: Decode ConnectionDrainingTimeout where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionDrainingTimeout :: Encode ConnectionDrainingTimeout where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the <code>ConnectionSettings</code> attribute.</p>
newtype ConnectionSettings = ConnectionSettings 
  { "IdleTimeout" :: (IdleTimeout)
  }
derive instance newtypeConnectionSettings :: Newtype ConnectionSettings _
derive instance repGenericConnectionSettings :: Generic ConnectionSettings _
instance showConnectionSettings :: Show ConnectionSettings where
  show = genericShow
instance decodeConnectionSettings :: Decode ConnectionSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionSettings :: Encode ConnectionSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CookieExpirationPeriod = CookieExpirationPeriod Number
derive instance newtypeCookieExpirationPeriod :: Newtype CookieExpirationPeriod _
derive instance repGenericCookieExpirationPeriod :: Generic CookieExpirationPeriod _
instance showCookieExpirationPeriod :: Show CookieExpirationPeriod where
  show = genericShow
instance decodeCookieExpirationPeriod :: Decode CookieExpirationPeriod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCookieExpirationPeriod :: Encode CookieExpirationPeriod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CookieName = CookieName String
derive instance newtypeCookieName :: Newtype CookieName _
derive instance repGenericCookieName :: Generic CookieName _
instance showCookieName :: Show CookieName where
  show = genericShow
instance decodeCookieName :: Decode CookieName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCookieName :: Encode CookieName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for CreateLoadBalancer.</p>
newtype CreateAccessPointInput = CreateAccessPointInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Listeners" :: (Listeners)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZones)
  , "Subnets" :: NullOrUndefined.NullOrUndefined (Subnets)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  , "Scheme" :: NullOrUndefined.NullOrUndefined (LoadBalancerScheme)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeCreateAccessPointInput :: Newtype CreateAccessPointInput _
derive instance repGenericCreateAccessPointInput :: Generic CreateAccessPointInput _
instance showCreateAccessPointInput :: Show CreateAccessPointInput where
  show = genericShow
instance decodeCreateAccessPointInput :: Decode CreateAccessPointInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAccessPointInput :: Encode CreateAccessPointInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output for CreateLoadBalancer.</p>
newtype CreateAccessPointOutput = CreateAccessPointOutput 
  { "DNSName" :: NullOrUndefined.NullOrUndefined (DNSName)
  }
derive instance newtypeCreateAccessPointOutput :: Newtype CreateAccessPointOutput _
derive instance repGenericCreateAccessPointOutput :: Generic CreateAccessPointOutput _
instance showCreateAccessPointOutput :: Show CreateAccessPointOutput where
  show = genericShow
instance decodeCreateAccessPointOutput :: Decode CreateAccessPointOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAccessPointOutput :: Encode CreateAccessPointOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for CreateAppCookieStickinessPolicy.</p>
newtype CreateAppCookieStickinessPolicyInput = CreateAppCookieStickinessPolicyInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "PolicyName" :: (PolicyName)
  , "CookieName" :: (CookieName)
  }
derive instance newtypeCreateAppCookieStickinessPolicyInput :: Newtype CreateAppCookieStickinessPolicyInput _
derive instance repGenericCreateAppCookieStickinessPolicyInput :: Generic CreateAppCookieStickinessPolicyInput _
instance showCreateAppCookieStickinessPolicyInput :: Show CreateAppCookieStickinessPolicyInput where
  show = genericShow
instance decodeCreateAppCookieStickinessPolicyInput :: Decode CreateAppCookieStickinessPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAppCookieStickinessPolicyInput :: Encode CreateAppCookieStickinessPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output for CreateAppCookieStickinessPolicy.</p>
newtype CreateAppCookieStickinessPolicyOutput = CreateAppCookieStickinessPolicyOutput Types.NoArguments
derive instance newtypeCreateAppCookieStickinessPolicyOutput :: Newtype CreateAppCookieStickinessPolicyOutput _
derive instance repGenericCreateAppCookieStickinessPolicyOutput :: Generic CreateAppCookieStickinessPolicyOutput _
instance showCreateAppCookieStickinessPolicyOutput :: Show CreateAppCookieStickinessPolicyOutput where
  show = genericShow
instance decodeCreateAppCookieStickinessPolicyOutput :: Decode CreateAppCookieStickinessPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAppCookieStickinessPolicyOutput :: Encode CreateAppCookieStickinessPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for CreateLBCookieStickinessPolicy.</p>
newtype CreateLBCookieStickinessPolicyInput = CreateLBCookieStickinessPolicyInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "PolicyName" :: (PolicyName)
  , "CookieExpirationPeriod" :: NullOrUndefined.NullOrUndefined (CookieExpirationPeriod)
  }
derive instance newtypeCreateLBCookieStickinessPolicyInput :: Newtype CreateLBCookieStickinessPolicyInput _
derive instance repGenericCreateLBCookieStickinessPolicyInput :: Generic CreateLBCookieStickinessPolicyInput _
instance showCreateLBCookieStickinessPolicyInput :: Show CreateLBCookieStickinessPolicyInput where
  show = genericShow
instance decodeCreateLBCookieStickinessPolicyInput :: Decode CreateLBCookieStickinessPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLBCookieStickinessPolicyInput :: Encode CreateLBCookieStickinessPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output for CreateLBCookieStickinessPolicy.</p>
newtype CreateLBCookieStickinessPolicyOutput = CreateLBCookieStickinessPolicyOutput Types.NoArguments
derive instance newtypeCreateLBCookieStickinessPolicyOutput :: Newtype CreateLBCookieStickinessPolicyOutput _
derive instance repGenericCreateLBCookieStickinessPolicyOutput :: Generic CreateLBCookieStickinessPolicyOutput _
instance showCreateLBCookieStickinessPolicyOutput :: Show CreateLBCookieStickinessPolicyOutput where
  show = genericShow
instance decodeCreateLBCookieStickinessPolicyOutput :: Decode CreateLBCookieStickinessPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLBCookieStickinessPolicyOutput :: Encode CreateLBCookieStickinessPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for CreateLoadBalancerListeners.</p>
newtype CreateLoadBalancerListenerInput = CreateLoadBalancerListenerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Listeners" :: (Listeners)
  }
derive instance newtypeCreateLoadBalancerListenerInput :: Newtype CreateLoadBalancerListenerInput _
derive instance repGenericCreateLoadBalancerListenerInput :: Generic CreateLoadBalancerListenerInput _
instance showCreateLoadBalancerListenerInput :: Show CreateLoadBalancerListenerInput where
  show = genericShow
instance decodeCreateLoadBalancerListenerInput :: Decode CreateLoadBalancerListenerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerListenerInput :: Encode CreateLoadBalancerListenerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for CreateLoadBalancerListener.</p>
newtype CreateLoadBalancerListenerOutput = CreateLoadBalancerListenerOutput Types.NoArguments
derive instance newtypeCreateLoadBalancerListenerOutput :: Newtype CreateLoadBalancerListenerOutput _
derive instance repGenericCreateLoadBalancerListenerOutput :: Generic CreateLoadBalancerListenerOutput _
instance showCreateLoadBalancerListenerOutput :: Show CreateLoadBalancerListenerOutput where
  show = genericShow
instance decodeCreateLoadBalancerListenerOutput :: Decode CreateLoadBalancerListenerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerListenerOutput :: Encode CreateLoadBalancerListenerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for CreateLoadBalancerPolicy.</p>
newtype CreateLoadBalancerPolicyInput = CreateLoadBalancerPolicyInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "PolicyName" :: (PolicyName)
  , "PolicyTypeName" :: (PolicyTypeName)
  , "PolicyAttributes" :: NullOrUndefined.NullOrUndefined (PolicyAttributes)
  }
derive instance newtypeCreateLoadBalancerPolicyInput :: Newtype CreateLoadBalancerPolicyInput _
derive instance repGenericCreateLoadBalancerPolicyInput :: Generic CreateLoadBalancerPolicyInput _
instance showCreateLoadBalancerPolicyInput :: Show CreateLoadBalancerPolicyInput where
  show = genericShow
instance decodeCreateLoadBalancerPolicyInput :: Decode CreateLoadBalancerPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerPolicyInput :: Encode CreateLoadBalancerPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of CreateLoadBalancerPolicy.</p>
newtype CreateLoadBalancerPolicyOutput = CreateLoadBalancerPolicyOutput Types.NoArguments
derive instance newtypeCreateLoadBalancerPolicyOutput :: Newtype CreateLoadBalancerPolicyOutput _
derive instance repGenericCreateLoadBalancerPolicyOutput :: Generic CreateLoadBalancerPolicyOutput _
instance showCreateLoadBalancerPolicyOutput :: Show CreateLoadBalancerPolicyOutput where
  show = genericShow
instance decodeCreateLoadBalancerPolicyOutput :: Decode CreateLoadBalancerPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLoadBalancerPolicyOutput :: Encode CreateLoadBalancerPolicyOutput where
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


-- | <p>Information about the <code>CrossZoneLoadBalancing</code> attribute.</p>
newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing 
  { "Enabled" :: (CrossZoneLoadBalancingEnabled)
  }
derive instance newtypeCrossZoneLoadBalancing :: Newtype CrossZoneLoadBalancing _
derive instance repGenericCrossZoneLoadBalancing :: Generic CrossZoneLoadBalancing _
instance showCrossZoneLoadBalancing :: Show CrossZoneLoadBalancing where
  show = genericShow
instance decodeCrossZoneLoadBalancing :: Decode CrossZoneLoadBalancing where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrossZoneLoadBalancing :: Encode CrossZoneLoadBalancing where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CrossZoneLoadBalancingEnabled = CrossZoneLoadBalancingEnabled Boolean
derive instance newtypeCrossZoneLoadBalancingEnabled :: Newtype CrossZoneLoadBalancingEnabled _
derive instance repGenericCrossZoneLoadBalancingEnabled :: Generic CrossZoneLoadBalancingEnabled _
instance showCrossZoneLoadBalancingEnabled :: Show CrossZoneLoadBalancingEnabled where
  show = genericShow
instance decodeCrossZoneLoadBalancingEnabled :: Decode CrossZoneLoadBalancingEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrossZoneLoadBalancingEnabled :: Encode CrossZoneLoadBalancingEnabled where
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


newtype DefaultValue = DefaultValue String
derive instance newtypeDefaultValue :: Newtype DefaultValue _
derive instance repGenericDefaultValue :: Generic DefaultValue _
instance showDefaultValue :: Show DefaultValue where
  show = genericShow
instance decodeDefaultValue :: Decode DefaultValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultValue :: Encode DefaultValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DeleteLoadBalancer.</p>
newtype DeleteAccessPointInput = DeleteAccessPointInput 
  { "LoadBalancerName" :: (AccessPointName)
  }
derive instance newtypeDeleteAccessPointInput :: Newtype DeleteAccessPointInput _
derive instance repGenericDeleteAccessPointInput :: Generic DeleteAccessPointInput _
instance showDeleteAccessPointInput :: Show DeleteAccessPointInput where
  show = genericShow
instance decodeDeleteAccessPointInput :: Decode DeleteAccessPointInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAccessPointInput :: Encode DeleteAccessPointInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of DeleteLoadBalancer.</p>
newtype DeleteAccessPointOutput = DeleteAccessPointOutput Types.NoArguments
derive instance newtypeDeleteAccessPointOutput :: Newtype DeleteAccessPointOutput _
derive instance repGenericDeleteAccessPointOutput :: Generic DeleteAccessPointOutput _
instance showDeleteAccessPointOutput :: Show DeleteAccessPointOutput where
  show = genericShow
instance decodeDeleteAccessPointOutput :: Decode DeleteAccessPointOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAccessPointOutput :: Encode DeleteAccessPointOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DeleteLoadBalancerListeners.</p>
newtype DeleteLoadBalancerListenerInput = DeleteLoadBalancerListenerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "LoadBalancerPorts" :: (Ports)
  }
derive instance newtypeDeleteLoadBalancerListenerInput :: Newtype DeleteLoadBalancerListenerInput _
derive instance repGenericDeleteLoadBalancerListenerInput :: Generic DeleteLoadBalancerListenerInput _
instance showDeleteLoadBalancerListenerInput :: Show DeleteLoadBalancerListenerInput where
  show = genericShow
instance decodeDeleteLoadBalancerListenerInput :: Decode DeleteLoadBalancerListenerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerListenerInput :: Encode DeleteLoadBalancerListenerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of DeleteLoadBalancerListeners.</p>
newtype DeleteLoadBalancerListenerOutput = DeleteLoadBalancerListenerOutput Types.NoArguments
derive instance newtypeDeleteLoadBalancerListenerOutput :: Newtype DeleteLoadBalancerListenerOutput _
derive instance repGenericDeleteLoadBalancerListenerOutput :: Generic DeleteLoadBalancerListenerOutput _
instance showDeleteLoadBalancerListenerOutput :: Show DeleteLoadBalancerListenerOutput where
  show = genericShow
instance decodeDeleteLoadBalancerListenerOutput :: Decode DeleteLoadBalancerListenerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerListenerOutput :: Encode DeleteLoadBalancerListenerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DeleteLoadBalancerPolicy.</p>
newtype DeleteLoadBalancerPolicyInput = DeleteLoadBalancerPolicyInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "PolicyName" :: (PolicyName)
  }
derive instance newtypeDeleteLoadBalancerPolicyInput :: Newtype DeleteLoadBalancerPolicyInput _
derive instance repGenericDeleteLoadBalancerPolicyInput :: Generic DeleteLoadBalancerPolicyInput _
instance showDeleteLoadBalancerPolicyInput :: Show DeleteLoadBalancerPolicyInput where
  show = genericShow
instance decodeDeleteLoadBalancerPolicyInput :: Decode DeleteLoadBalancerPolicyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerPolicyInput :: Encode DeleteLoadBalancerPolicyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of DeleteLoadBalancerPolicy.</p>
newtype DeleteLoadBalancerPolicyOutput = DeleteLoadBalancerPolicyOutput Types.NoArguments
derive instance newtypeDeleteLoadBalancerPolicyOutput :: Newtype DeleteLoadBalancerPolicyOutput _
derive instance repGenericDeleteLoadBalancerPolicyOutput :: Generic DeleteLoadBalancerPolicyOutput _
instance showDeleteLoadBalancerPolicyOutput :: Show DeleteLoadBalancerPolicyOutput where
  show = genericShow
instance decodeDeleteLoadBalancerPolicyOutput :: Decode DeleteLoadBalancerPolicyOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLoadBalancerPolicyOutput :: Encode DeleteLoadBalancerPolicyOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DependencyThrottleException = DependencyThrottleException Types.NoArguments
derive instance newtypeDependencyThrottleException :: Newtype DependencyThrottleException _
derive instance repGenericDependencyThrottleException :: Generic DependencyThrottleException _
instance showDependencyThrottleException :: Show DependencyThrottleException where
  show = genericShow
instance decodeDependencyThrottleException :: Decode DependencyThrottleException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDependencyThrottleException :: Encode DependencyThrottleException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DeregisterInstancesFromLoadBalancer.</p>
newtype DeregisterEndPointsInput = DeregisterEndPointsInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Instances" :: (Instances)
  }
derive instance newtypeDeregisterEndPointsInput :: Newtype DeregisterEndPointsInput _
derive instance repGenericDeregisterEndPointsInput :: Generic DeregisterEndPointsInput _
instance showDeregisterEndPointsInput :: Show DeregisterEndPointsInput where
  show = genericShow
instance decodeDeregisterEndPointsInput :: Decode DeregisterEndPointsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterEndPointsInput :: Encode DeregisterEndPointsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of DeregisterInstancesFromLoadBalancer.</p>
newtype DeregisterEndPointsOutput = DeregisterEndPointsOutput 
  { "Instances" :: NullOrUndefined.NullOrUndefined (Instances)
  }
derive instance newtypeDeregisterEndPointsOutput :: Newtype DeregisterEndPointsOutput _
derive instance repGenericDeregisterEndPointsOutput :: Generic DeregisterEndPointsOutput _
instance showDeregisterEndPointsOutput :: Show DeregisterEndPointsOutput where
  show = genericShow
instance decodeDeregisterEndPointsOutput :: Decode DeregisterEndPointsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterEndPointsOutput :: Encode DeregisterEndPointsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DescribeLoadBalancers.</p>
newtype DescribeAccessPointsInput = DescribeAccessPointsInput 
  { "LoadBalancerNames" :: NullOrUndefined.NullOrUndefined (LoadBalancerNames)
  , "Marker" :: NullOrUndefined.NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeDescribeAccessPointsInput :: Newtype DescribeAccessPointsInput _
derive instance repGenericDescribeAccessPointsInput :: Generic DescribeAccessPointsInput _
instance showDescribeAccessPointsInput :: Show DescribeAccessPointsInput where
  show = genericShow
instance decodeDescribeAccessPointsInput :: Decode DescribeAccessPointsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAccessPointsInput :: Encode DescribeAccessPointsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DescribeLoadBalancers.</p>
newtype DescribeAccessPointsOutput = DescribeAccessPointsOutput 
  { "LoadBalancerDescriptions" :: NullOrUndefined.NullOrUndefined (LoadBalancerDescriptions)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (Marker)
  }
derive instance newtypeDescribeAccessPointsOutput :: Newtype DescribeAccessPointsOutput _
derive instance repGenericDescribeAccessPointsOutput :: Generic DescribeAccessPointsOutput _
instance showDescribeAccessPointsOutput :: Show DescribeAccessPointsOutput where
  show = genericShow
instance decodeDescribeAccessPointsOutput :: Decode DescribeAccessPointsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAccessPointsOutput :: Encode DescribeAccessPointsOutput where
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


-- | <p>Contains the parameters for DescribeInstanceHealth.</p>
newtype DescribeEndPointStateInput = DescribeEndPointStateInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Instances" :: NullOrUndefined.NullOrUndefined (Instances)
  }
derive instance newtypeDescribeEndPointStateInput :: Newtype DescribeEndPointStateInput _
derive instance repGenericDescribeEndPointStateInput :: Generic DescribeEndPointStateInput _
instance showDescribeEndPointStateInput :: Show DescribeEndPointStateInput where
  show = genericShow
instance decodeDescribeEndPointStateInput :: Decode DescribeEndPointStateInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEndPointStateInput :: Encode DescribeEndPointStateInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output for DescribeInstanceHealth.</p>
newtype DescribeEndPointStateOutput = DescribeEndPointStateOutput 
  { "InstanceStates" :: NullOrUndefined.NullOrUndefined (InstanceStates)
  }
derive instance newtypeDescribeEndPointStateOutput :: Newtype DescribeEndPointStateOutput _
derive instance repGenericDescribeEndPointStateOutput :: Generic DescribeEndPointStateOutput _
instance showDescribeEndPointStateOutput :: Show DescribeEndPointStateOutput where
  show = genericShow
instance decodeDescribeEndPointStateOutput :: Decode DescribeEndPointStateOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEndPointStateOutput :: Encode DescribeEndPointStateOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DescribeLoadBalancerAttributes.</p>
newtype DescribeLoadBalancerAttributesInput = DescribeLoadBalancerAttributesInput 
  { "LoadBalancerName" :: (AccessPointName)
  }
derive instance newtypeDescribeLoadBalancerAttributesInput :: Newtype DescribeLoadBalancerAttributesInput _
derive instance repGenericDescribeLoadBalancerAttributesInput :: Generic DescribeLoadBalancerAttributesInput _
instance showDescribeLoadBalancerAttributesInput :: Show DescribeLoadBalancerAttributesInput where
  show = genericShow
instance decodeDescribeLoadBalancerAttributesInput :: Decode DescribeLoadBalancerAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancerAttributesInput :: Encode DescribeLoadBalancerAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of DescribeLoadBalancerAttributes.</p>
newtype DescribeLoadBalancerAttributesOutput = DescribeLoadBalancerAttributesOutput 
  { "LoadBalancerAttributes" :: NullOrUndefined.NullOrUndefined (LoadBalancerAttributes)
  }
derive instance newtypeDescribeLoadBalancerAttributesOutput :: Newtype DescribeLoadBalancerAttributesOutput _
derive instance repGenericDescribeLoadBalancerAttributesOutput :: Generic DescribeLoadBalancerAttributesOutput _
instance showDescribeLoadBalancerAttributesOutput :: Show DescribeLoadBalancerAttributesOutput where
  show = genericShow
instance decodeDescribeLoadBalancerAttributesOutput :: Decode DescribeLoadBalancerAttributesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancerAttributesOutput :: Encode DescribeLoadBalancerAttributesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DescribeLoadBalancerPolicies.</p>
newtype DescribeLoadBalancerPoliciesInput = DescribeLoadBalancerPoliciesInput 
  { "LoadBalancerName" :: NullOrUndefined.NullOrUndefined (AccessPointName)
  , "PolicyNames" :: NullOrUndefined.NullOrUndefined (PolicyNames)
  }
derive instance newtypeDescribeLoadBalancerPoliciesInput :: Newtype DescribeLoadBalancerPoliciesInput _
derive instance repGenericDescribeLoadBalancerPoliciesInput :: Generic DescribeLoadBalancerPoliciesInput _
instance showDescribeLoadBalancerPoliciesInput :: Show DescribeLoadBalancerPoliciesInput where
  show = genericShow
instance decodeDescribeLoadBalancerPoliciesInput :: Decode DescribeLoadBalancerPoliciesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancerPoliciesInput :: Encode DescribeLoadBalancerPoliciesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of DescribeLoadBalancerPolicies.</p>
newtype DescribeLoadBalancerPoliciesOutput = DescribeLoadBalancerPoliciesOutput 
  { "PolicyDescriptions" :: NullOrUndefined.NullOrUndefined (PolicyDescriptions)
  }
derive instance newtypeDescribeLoadBalancerPoliciesOutput :: Newtype DescribeLoadBalancerPoliciesOutput _
derive instance repGenericDescribeLoadBalancerPoliciesOutput :: Generic DescribeLoadBalancerPoliciesOutput _
instance showDescribeLoadBalancerPoliciesOutput :: Show DescribeLoadBalancerPoliciesOutput where
  show = genericShow
instance decodeDescribeLoadBalancerPoliciesOutput :: Decode DescribeLoadBalancerPoliciesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancerPoliciesOutput :: Encode DescribeLoadBalancerPoliciesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DescribeLoadBalancerPolicyTypes.</p>
newtype DescribeLoadBalancerPolicyTypesInput = DescribeLoadBalancerPolicyTypesInput 
  { "PolicyTypeNames" :: NullOrUndefined.NullOrUndefined (PolicyTypeNames)
  }
derive instance newtypeDescribeLoadBalancerPolicyTypesInput :: Newtype DescribeLoadBalancerPolicyTypesInput _
derive instance repGenericDescribeLoadBalancerPolicyTypesInput :: Generic DescribeLoadBalancerPolicyTypesInput _
instance showDescribeLoadBalancerPolicyTypesInput :: Show DescribeLoadBalancerPolicyTypesInput where
  show = genericShow
instance decodeDescribeLoadBalancerPolicyTypesInput :: Decode DescribeLoadBalancerPolicyTypesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancerPolicyTypesInput :: Encode DescribeLoadBalancerPolicyTypesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of DescribeLoadBalancerPolicyTypes.</p>
newtype DescribeLoadBalancerPolicyTypesOutput = DescribeLoadBalancerPolicyTypesOutput 
  { "PolicyTypeDescriptions" :: NullOrUndefined.NullOrUndefined (PolicyTypeDescriptions)
  }
derive instance newtypeDescribeLoadBalancerPolicyTypesOutput :: Newtype DescribeLoadBalancerPolicyTypesOutput _
derive instance repGenericDescribeLoadBalancerPolicyTypesOutput :: Generic DescribeLoadBalancerPolicyTypesOutput _
instance showDescribeLoadBalancerPolicyTypesOutput :: Show DescribeLoadBalancerPolicyTypesOutput where
  show = genericShow
instance decodeDescribeLoadBalancerPolicyTypesOutput :: Decode DescribeLoadBalancerPolicyTypesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeLoadBalancerPolicyTypesOutput :: Encode DescribeLoadBalancerPolicyTypesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DescribeTags.</p>
newtype DescribeTagsInput = DescribeTagsInput 
  { "LoadBalancerNames" :: (LoadBalancerNamesMax20)
  }
derive instance newtypeDescribeTagsInput :: Newtype DescribeTagsInput _
derive instance repGenericDescribeTagsInput :: Generic DescribeTagsInput _
instance showDescribeTagsInput :: Show DescribeTagsInput where
  show = genericShow
instance decodeDescribeTagsInput :: Decode DescribeTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsInput :: Encode DescribeTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output for DescribeTags.</p>
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


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DetachLoadBalancerFromSubnets.</p>
newtype DetachLoadBalancerFromSubnetsInput = DetachLoadBalancerFromSubnetsInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Subnets" :: (Subnets)
  }
derive instance newtypeDetachLoadBalancerFromSubnetsInput :: Newtype DetachLoadBalancerFromSubnetsInput _
derive instance repGenericDetachLoadBalancerFromSubnetsInput :: Generic DetachLoadBalancerFromSubnetsInput _
instance showDetachLoadBalancerFromSubnetsInput :: Show DetachLoadBalancerFromSubnetsInput where
  show = genericShow
instance decodeDetachLoadBalancerFromSubnetsInput :: Decode DetachLoadBalancerFromSubnetsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachLoadBalancerFromSubnetsInput :: Encode DetachLoadBalancerFromSubnetsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of DetachLoadBalancerFromSubnets.</p>
newtype DetachLoadBalancerFromSubnetsOutput = DetachLoadBalancerFromSubnetsOutput 
  { "Subnets" :: NullOrUndefined.NullOrUndefined (Subnets)
  }
derive instance newtypeDetachLoadBalancerFromSubnetsOutput :: Newtype DetachLoadBalancerFromSubnetsOutput _
derive instance repGenericDetachLoadBalancerFromSubnetsOutput :: Generic DetachLoadBalancerFromSubnetsOutput _
instance showDetachLoadBalancerFromSubnetsOutput :: Show DetachLoadBalancerFromSubnetsOutput where
  show = genericShow
instance decodeDetachLoadBalancerFromSubnetsOutput :: Decode DetachLoadBalancerFromSubnetsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetachLoadBalancerFromSubnetsOutput :: Encode DetachLoadBalancerFromSubnetsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified load balancer name already exists for this account.</p>
newtype DuplicateAccessPointNameException = DuplicateAccessPointNameException Types.NoArguments
derive instance newtypeDuplicateAccessPointNameException :: Newtype DuplicateAccessPointNameException _
derive instance repGenericDuplicateAccessPointNameException :: Generic DuplicateAccessPointNameException _
instance showDuplicateAccessPointNameException :: Show DuplicateAccessPointNameException where
  show = genericShow
instance decodeDuplicateAccessPointNameException :: Decode DuplicateAccessPointNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateAccessPointNameException :: Encode DuplicateAccessPointNameException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A listener already exists for the specified load balancer name and port, but with a different instance port, protocol, or SSL certificate.</p>
newtype DuplicateListenerException = DuplicateListenerException Types.NoArguments
derive instance newtypeDuplicateListenerException :: Newtype DuplicateListenerException _
derive instance repGenericDuplicateListenerException :: Generic DuplicateListenerException _
instance showDuplicateListenerException :: Show DuplicateListenerException where
  show = genericShow
instance decodeDuplicateListenerException :: Decode DuplicateListenerException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicateListenerException :: Encode DuplicateListenerException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A policy with the specified name already exists for this load balancer.</p>
newtype DuplicatePolicyNameException = DuplicatePolicyNameException Types.NoArguments
derive instance newtypeDuplicatePolicyNameException :: Newtype DuplicatePolicyNameException _
derive instance repGenericDuplicatePolicyNameException :: Generic DuplicatePolicyNameException _
instance showDuplicatePolicyNameException :: Show DuplicatePolicyNameException where
  show = genericShow
instance decodeDuplicatePolicyNameException :: Decode DuplicatePolicyNameException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDuplicatePolicyNameException :: Encode DuplicatePolicyNameException where
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


newtype EndPointPort = EndPointPort Int
derive instance newtypeEndPointPort :: Newtype EndPointPort _
derive instance repGenericEndPointPort :: Generic EndPointPort _
instance showEndPointPort :: Show EndPointPort where
  show = genericShow
instance decodeEndPointPort :: Decode EndPointPort where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndPointPort :: Encode EndPointPort where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a health check.</p>
newtype HealthCheck = HealthCheck 
  { "Target" :: (HealthCheckTarget)
  , "Interval" :: (HealthCheckInterval)
  , "Timeout" :: (HealthCheckTimeout)
  , "UnhealthyThreshold" :: (UnhealthyThreshold)
  , "HealthyThreshold" :: (HealthyThreshold)
  }
derive instance newtypeHealthCheck :: Newtype HealthCheck _
derive instance repGenericHealthCheck :: Generic HealthCheck _
instance showHealthCheck :: Show HealthCheck where
  show = genericShow
instance decodeHealthCheck :: Decode HealthCheck where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheck :: Encode HealthCheck where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckInterval = HealthCheckInterval Int
derive instance newtypeHealthCheckInterval :: Newtype HealthCheckInterval _
derive instance repGenericHealthCheckInterval :: Generic HealthCheckInterval _
instance showHealthCheckInterval :: Show HealthCheckInterval where
  show = genericShow
instance decodeHealthCheckInterval :: Decode HealthCheckInterval where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckInterval :: Encode HealthCheckInterval where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckTarget = HealthCheckTarget String
derive instance newtypeHealthCheckTarget :: Newtype HealthCheckTarget _
derive instance repGenericHealthCheckTarget :: Generic HealthCheckTarget _
instance showHealthCheckTarget :: Show HealthCheckTarget where
  show = genericShow
instance decodeHealthCheckTarget :: Decode HealthCheckTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckTarget :: Encode HealthCheckTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckTimeout = HealthCheckTimeout Int
derive instance newtypeHealthCheckTimeout :: Newtype HealthCheckTimeout _
derive instance repGenericHealthCheckTimeout :: Generic HealthCheckTimeout _
instance showHealthCheckTimeout :: Show HealthCheckTimeout where
  show = genericShow
instance decodeHealthCheckTimeout :: Decode HealthCheckTimeout where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckTimeout :: Encode HealthCheckTimeout where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthyThreshold = HealthyThreshold Int
derive instance newtypeHealthyThreshold :: Newtype HealthyThreshold _
derive instance repGenericHealthyThreshold :: Generic HealthyThreshold _
instance showHealthyThreshold :: Show HealthyThreshold where
  show = genericShow
instance decodeHealthyThreshold :: Decode HealthyThreshold where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthyThreshold :: Encode HealthyThreshold where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdleTimeout = IdleTimeout Int
derive instance newtypeIdleTimeout :: Newtype IdleTimeout _
derive instance repGenericIdleTimeout :: Generic IdleTimeout _
instance showIdleTimeout :: Show IdleTimeout where
  show = genericShow
instance decodeIdleTimeout :: Decode IdleTimeout where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdleTimeout :: Encode IdleTimeout where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The ID of an EC2 instance.</p>
newtype Instance = Instance 
  { "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  }
derive instance newtypeInstance :: Newtype Instance _
derive instance repGenericInstance :: Generic Instance _
instance showInstance :: Show Instance where
  show = genericShow
instance decodeInstance :: Decode Instance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstance :: Encode Instance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceId = InstanceId String
derive instance newtypeInstanceId :: Newtype InstanceId _
derive instance repGenericInstanceId :: Generic InstanceId _
instance showInstanceId :: Show InstanceId where
  show = genericShow
instance decodeInstanceId :: Decode InstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceId :: Encode InstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstancePort = InstancePort Int
derive instance newtypeInstancePort :: Newtype InstancePort _
derive instance repGenericInstancePort :: Generic InstancePort _
instance showInstancePort :: Show InstancePort where
  show = genericShow
instance decodeInstancePort :: Decode InstancePort where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstancePort :: Encode InstancePort where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the state of an EC2 instance.</p>
newtype InstanceState = InstanceState 
  { "InstanceId" :: NullOrUndefined.NullOrUndefined (InstanceId)
  , "State" :: NullOrUndefined.NullOrUndefined (State)
  , "ReasonCode" :: NullOrUndefined.NullOrUndefined (ReasonCode)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeInstanceState :: Newtype InstanceState _
derive instance repGenericInstanceState :: Generic InstanceState _
instance showInstanceState :: Show InstanceState where
  show = genericShow
instance decodeInstanceState :: Decode InstanceState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceState :: Encode InstanceState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InstanceStates = InstanceStates (Array InstanceState)
derive instance newtypeInstanceStates :: Newtype InstanceStates _
derive instance repGenericInstanceStates :: Generic InstanceStates _
instance showInstanceStates :: Show InstanceStates where
  show = genericShow
instance decodeInstanceStates :: Decode InstanceStates where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstanceStates :: Encode InstanceStates where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Instances = Instances (Array Instance)
derive instance newtypeInstances :: Newtype Instances _
derive instance repGenericInstances :: Generic Instances _
instance showInstances :: Show Instances where
  show = genericShow
instance decodeInstances :: Decode Instances where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInstances :: Encode Instances where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The requested configuration change is not valid.</p>
newtype InvalidConfigurationRequestException = InvalidConfigurationRequestException Types.NoArguments
derive instance newtypeInvalidConfigurationRequestException :: Newtype InvalidConfigurationRequestException _
derive instance repGenericInvalidConfigurationRequestException :: Generic InvalidConfigurationRequestException _
instance showInvalidConfigurationRequestException :: Show InvalidConfigurationRequestException where
  show = genericShow
instance decodeInvalidConfigurationRequestException :: Decode InvalidConfigurationRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidConfigurationRequestException :: Encode InvalidConfigurationRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified endpoint is not valid.</p>
newtype InvalidEndPointException = InvalidEndPointException Types.NoArguments
derive instance newtypeInvalidEndPointException :: Newtype InvalidEndPointException _
derive instance repGenericInvalidEndPointException :: Generic InvalidEndPointException _
instance showInvalidEndPointException :: Show InvalidEndPointException where
  show = genericShow
instance decodeInvalidEndPointException :: Decode InvalidEndPointException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidEndPointException :: Encode InvalidEndPointException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified value for the schema is not valid. You can only specify a scheme for load balancers in a VPC.</p>
newtype InvalidSchemeException = InvalidSchemeException Types.NoArguments
derive instance newtypeInvalidSchemeException :: Newtype InvalidSchemeException _
derive instance repGenericInvalidSchemeException :: Generic InvalidSchemeException _
instance showInvalidSchemeException :: Show InvalidSchemeException where
  show = genericShow
instance decodeInvalidSchemeException :: Decode InvalidSchemeException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSchemeException :: Encode InvalidSchemeException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more of the specified security groups do not exist.</p>
newtype InvalidSecurityGroupException = InvalidSecurityGroupException Types.NoArguments
derive instance newtypeInvalidSecurityGroupException :: Newtype InvalidSecurityGroupException _
derive instance repGenericInvalidSecurityGroupException :: Generic InvalidSecurityGroupException _
instance showInvalidSecurityGroupException :: Show InvalidSecurityGroupException where
  show = genericShow
instance decodeInvalidSecurityGroupException :: Decode InvalidSecurityGroupException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSecurityGroupException :: Encode InvalidSecurityGroupException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified VPC has no associated Internet gateway.</p>
newtype InvalidSubnetException = InvalidSubnetException Types.NoArguments
derive instance newtypeInvalidSubnetException :: Newtype InvalidSubnetException _
derive instance repGenericInvalidSubnetException :: Generic InvalidSubnetException _
instance showInvalidSubnetException :: Show InvalidSubnetException where
  show = genericShow
instance decodeInvalidSubnetException :: Decode InvalidSubnetException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidSubnetException :: Encode InvalidSubnetException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LBCookieStickinessPolicies = LBCookieStickinessPolicies (Array LBCookieStickinessPolicy)
derive instance newtypeLBCookieStickinessPolicies :: Newtype LBCookieStickinessPolicies _
derive instance repGenericLBCookieStickinessPolicies :: Generic LBCookieStickinessPolicies _
instance showLBCookieStickinessPolicies :: Show LBCookieStickinessPolicies where
  show = genericShow
instance decodeLBCookieStickinessPolicies :: Decode LBCookieStickinessPolicies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLBCookieStickinessPolicies :: Encode LBCookieStickinessPolicies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a policy for duration-based session stickiness.</p>
newtype LBCookieStickinessPolicy = LBCookieStickinessPolicy 
  { "PolicyName" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "CookieExpirationPeriod" :: NullOrUndefined.NullOrUndefined (CookieExpirationPeriod)
  }
derive instance newtypeLBCookieStickinessPolicy :: Newtype LBCookieStickinessPolicy _
derive instance repGenericLBCookieStickinessPolicy :: Generic LBCookieStickinessPolicy _
instance showLBCookieStickinessPolicy :: Show LBCookieStickinessPolicy where
  show = genericShow
instance decodeLBCookieStickinessPolicy :: Decode LBCookieStickinessPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLBCookieStickinessPolicy :: Encode LBCookieStickinessPolicy where
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


-- | <p>Information about a listener.</p> <p>For information about the protocols and the ports supported by Elastic Load Balancing, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html">Listeners for Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
newtype Listener = Listener 
  { "Protocol" :: (Protocol)
  , "LoadBalancerPort" :: (AccessPointPort)
  , "InstanceProtocol" :: NullOrUndefined.NullOrUndefined (Protocol)
  , "InstancePort" :: (InstancePort)
  , "SSLCertificateId" :: NullOrUndefined.NullOrUndefined (SSLCertificateId)
  }
derive instance newtypeListener :: Newtype Listener _
derive instance repGenericListener :: Generic Listener _
instance showListener :: Show Listener where
  show = genericShow
instance decodeListener :: Decode Listener where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListener :: Encode Listener where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The policies enabled for a listener.</p>
newtype ListenerDescription = ListenerDescription 
  { "Listener" :: NullOrUndefined.NullOrUndefined (Listener)
  , "PolicyNames" :: NullOrUndefined.NullOrUndefined (PolicyNames)
  }
derive instance newtypeListenerDescription :: Newtype ListenerDescription _
derive instance repGenericListenerDescription :: Generic ListenerDescription _
instance showListenerDescription :: Show ListenerDescription where
  show = genericShow
instance decodeListenerDescription :: Decode ListenerDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListenerDescription :: Encode ListenerDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListenerDescriptions = ListenerDescriptions (Array ListenerDescription)
derive instance newtypeListenerDescriptions :: Newtype ListenerDescriptions _
derive instance repGenericListenerDescriptions :: Generic ListenerDescriptions _
instance showListenerDescriptions :: Show ListenerDescriptions where
  show = genericShow
instance decodeListenerDescriptions :: Decode ListenerDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListenerDescriptions :: Encode ListenerDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The load balancer does not have a listener configured at the specified port.</p>
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


-- | <p>The specified load balancer attribute does not exist.</p>
newtype LoadBalancerAttributeNotFoundException = LoadBalancerAttributeNotFoundException Types.NoArguments
derive instance newtypeLoadBalancerAttributeNotFoundException :: Newtype LoadBalancerAttributeNotFoundException _
derive instance repGenericLoadBalancerAttributeNotFoundException :: Generic LoadBalancerAttributeNotFoundException _
instance showLoadBalancerAttributeNotFoundException :: Show LoadBalancerAttributeNotFoundException where
  show = genericShow
instance decodeLoadBalancerAttributeNotFoundException :: Decode LoadBalancerAttributeNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAttributeNotFoundException :: Encode LoadBalancerAttributeNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The attributes for a load balancer.</p>
newtype LoadBalancerAttributes = LoadBalancerAttributes 
  { "CrossZoneLoadBalancing" :: NullOrUndefined.NullOrUndefined (CrossZoneLoadBalancing)
  , "AccessLog" :: NullOrUndefined.NullOrUndefined (AccessLog)
  , "ConnectionDraining" :: NullOrUndefined.NullOrUndefined (ConnectionDraining)
  , "ConnectionSettings" :: NullOrUndefined.NullOrUndefined (ConnectionSettings)
  , "AdditionalAttributes" :: NullOrUndefined.NullOrUndefined (AdditionalAttributes)
  }
derive instance newtypeLoadBalancerAttributes :: Newtype LoadBalancerAttributes _
derive instance repGenericLoadBalancerAttributes :: Generic LoadBalancerAttributes _
instance showLoadBalancerAttributes :: Show LoadBalancerAttributes where
  show = genericShow
instance decodeLoadBalancerAttributes :: Decode LoadBalancerAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerAttributes :: Encode LoadBalancerAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a load balancer.</p>
newtype LoadBalancerDescription = LoadBalancerDescription 
  { "LoadBalancerName" :: NullOrUndefined.NullOrUndefined (AccessPointName)
  , "DNSName" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "CanonicalHostedZoneName" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "CanonicalHostedZoneNameID" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "ListenerDescriptions" :: NullOrUndefined.NullOrUndefined (ListenerDescriptions)
  , "Policies" :: NullOrUndefined.NullOrUndefined (Policies)
  , "BackendServerDescriptions" :: NullOrUndefined.NullOrUndefined (BackendServerDescriptions)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZones)
  , "Subnets" :: NullOrUndefined.NullOrUndefined (Subnets)
  , "VPCId" :: NullOrUndefined.NullOrUndefined (VPCId)
  , "Instances" :: NullOrUndefined.NullOrUndefined (Instances)
  , "HealthCheck" :: NullOrUndefined.NullOrUndefined (HealthCheck)
  , "SourceSecurityGroup" :: NullOrUndefined.NullOrUndefined (SourceSecurityGroup)
  , "SecurityGroups" :: NullOrUndefined.NullOrUndefined (SecurityGroups)
  , "CreatedTime" :: NullOrUndefined.NullOrUndefined (CreatedTime)
  , "Scheme" :: NullOrUndefined.NullOrUndefined (LoadBalancerScheme)
  }
derive instance newtypeLoadBalancerDescription :: Newtype LoadBalancerDescription _
derive instance repGenericLoadBalancerDescription :: Generic LoadBalancerDescription _
instance showLoadBalancerDescription :: Show LoadBalancerDescription where
  show = genericShow
instance decodeLoadBalancerDescription :: Decode LoadBalancerDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerDescription :: Encode LoadBalancerDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerDescriptions = LoadBalancerDescriptions (Array LoadBalancerDescription)
derive instance newtypeLoadBalancerDescriptions :: Newtype LoadBalancerDescriptions _
derive instance repGenericLoadBalancerDescriptions :: Generic LoadBalancerDescriptions _
instance showLoadBalancerDescriptions :: Show LoadBalancerDescriptions where
  show = genericShow
instance decodeLoadBalancerDescriptions :: Decode LoadBalancerDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerDescriptions :: Encode LoadBalancerDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerNames = LoadBalancerNames (Array AccessPointName)
derive instance newtypeLoadBalancerNames :: Newtype LoadBalancerNames _
derive instance repGenericLoadBalancerNames :: Generic LoadBalancerNames _
instance showLoadBalancerNames :: Show LoadBalancerNames where
  show = genericShow
instance decodeLoadBalancerNames :: Decode LoadBalancerNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerNames :: Encode LoadBalancerNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerNamesMax20 = LoadBalancerNamesMax20 (Array AccessPointName)
derive instance newtypeLoadBalancerNamesMax20 :: Newtype LoadBalancerNamesMax20 _
derive instance repGenericLoadBalancerNamesMax20 :: Generic LoadBalancerNamesMax20 _
instance showLoadBalancerNamesMax20 :: Show LoadBalancerNamesMax20 where
  show = genericShow
instance decodeLoadBalancerNamesMax20 :: Decode LoadBalancerNamesMax20 where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerNamesMax20 :: Encode LoadBalancerNamesMax20 where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LoadBalancerScheme = LoadBalancerScheme String
derive instance newtypeLoadBalancerScheme :: Newtype LoadBalancerScheme _
derive instance repGenericLoadBalancerScheme :: Generic LoadBalancerScheme _
instance showLoadBalancerScheme :: Show LoadBalancerScheme where
  show = genericShow
instance decodeLoadBalancerScheme :: Decode LoadBalancerScheme where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLoadBalancerScheme :: Encode LoadBalancerScheme where
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


newtype Max = Max String
derive instance newtypeMax :: Newtype Max _
derive instance repGenericMax :: Generic Max _
instance showMax :: Show Max where
  show = genericShow
instance decodeMax :: Decode Max where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMax :: Encode Max where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for ModifyLoadBalancerAttributes.</p>
newtype ModifyLoadBalancerAttributesInput = ModifyLoadBalancerAttributesInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "LoadBalancerAttributes" :: (LoadBalancerAttributes)
  }
derive instance newtypeModifyLoadBalancerAttributesInput :: Newtype ModifyLoadBalancerAttributesInput _
derive instance repGenericModifyLoadBalancerAttributesInput :: Generic ModifyLoadBalancerAttributesInput _
instance showModifyLoadBalancerAttributesInput :: Show ModifyLoadBalancerAttributesInput where
  show = genericShow
instance decodeModifyLoadBalancerAttributesInput :: Decode ModifyLoadBalancerAttributesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyLoadBalancerAttributesInput :: Encode ModifyLoadBalancerAttributesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of ModifyLoadBalancerAttributes.</p>
newtype ModifyLoadBalancerAttributesOutput = ModifyLoadBalancerAttributesOutput 
  { "LoadBalancerName" :: NullOrUndefined.NullOrUndefined (AccessPointName)
  , "LoadBalancerAttributes" :: NullOrUndefined.NullOrUndefined (LoadBalancerAttributes)
  }
derive instance newtypeModifyLoadBalancerAttributesOutput :: Newtype ModifyLoadBalancerAttributesOutput _
derive instance repGenericModifyLoadBalancerAttributesOutput :: Generic ModifyLoadBalancerAttributesOutput _
instance showModifyLoadBalancerAttributesOutput :: Show ModifyLoadBalancerAttributesOutput where
  show = genericShow
instance decodeModifyLoadBalancerAttributesOutput :: Decode ModifyLoadBalancerAttributesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyLoadBalancerAttributesOutput :: Encode ModifyLoadBalancerAttributesOutput where
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


-- | <p>The policies for a load balancer.</p>
newtype Policies = Policies 
  { "AppCookieStickinessPolicies" :: NullOrUndefined.NullOrUndefined (AppCookieStickinessPolicies)
  , "LBCookieStickinessPolicies" :: NullOrUndefined.NullOrUndefined (LBCookieStickinessPolicies)
  , "OtherPolicies" :: NullOrUndefined.NullOrUndefined (PolicyNames)
  }
derive instance newtypePolicies :: Newtype Policies _
derive instance repGenericPolicies :: Generic Policies _
instance showPolicies :: Show Policies where
  show = genericShow
instance decodePolicies :: Decode Policies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicies :: Encode Policies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a policy attribute.</p>
newtype PolicyAttribute = PolicyAttribute 
  { "AttributeName" :: NullOrUndefined.NullOrUndefined (AttributeName)
  , "AttributeValue" :: NullOrUndefined.NullOrUndefined (AttributeValue)
  }
derive instance newtypePolicyAttribute :: Newtype PolicyAttribute _
derive instance repGenericPolicyAttribute :: Generic PolicyAttribute _
instance showPolicyAttribute :: Show PolicyAttribute where
  show = genericShow
instance decodePolicyAttribute :: Decode PolicyAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyAttribute :: Encode PolicyAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a policy attribute.</p>
newtype PolicyAttributeDescription = PolicyAttributeDescription 
  { "AttributeName" :: NullOrUndefined.NullOrUndefined (AttributeName)
  , "AttributeValue" :: NullOrUndefined.NullOrUndefined (AttributeValue)
  }
derive instance newtypePolicyAttributeDescription :: Newtype PolicyAttributeDescription _
derive instance repGenericPolicyAttributeDescription :: Generic PolicyAttributeDescription _
instance showPolicyAttributeDescription :: Show PolicyAttributeDescription where
  show = genericShow
instance decodePolicyAttributeDescription :: Decode PolicyAttributeDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyAttributeDescription :: Encode PolicyAttributeDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyAttributeDescriptions = PolicyAttributeDescriptions (Array PolicyAttributeDescription)
derive instance newtypePolicyAttributeDescriptions :: Newtype PolicyAttributeDescriptions _
derive instance repGenericPolicyAttributeDescriptions :: Generic PolicyAttributeDescriptions _
instance showPolicyAttributeDescriptions :: Show PolicyAttributeDescriptions where
  show = genericShow
instance decodePolicyAttributeDescriptions :: Decode PolicyAttributeDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyAttributeDescriptions :: Encode PolicyAttributeDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a policy attribute type.</p>
newtype PolicyAttributeTypeDescription = PolicyAttributeTypeDescription 
  { "AttributeName" :: NullOrUndefined.NullOrUndefined (AttributeName)
  , "AttributeType" :: NullOrUndefined.NullOrUndefined (AttributeType)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "DefaultValue" :: NullOrUndefined.NullOrUndefined (DefaultValue)
  , "Cardinality" :: NullOrUndefined.NullOrUndefined (Cardinality)
  }
derive instance newtypePolicyAttributeTypeDescription :: Newtype PolicyAttributeTypeDescription _
derive instance repGenericPolicyAttributeTypeDescription :: Generic PolicyAttributeTypeDescription _
instance showPolicyAttributeTypeDescription :: Show PolicyAttributeTypeDescription where
  show = genericShow
instance decodePolicyAttributeTypeDescription :: Decode PolicyAttributeTypeDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyAttributeTypeDescription :: Encode PolicyAttributeTypeDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyAttributeTypeDescriptions = PolicyAttributeTypeDescriptions (Array PolicyAttributeTypeDescription)
derive instance newtypePolicyAttributeTypeDescriptions :: Newtype PolicyAttributeTypeDescriptions _
derive instance repGenericPolicyAttributeTypeDescriptions :: Generic PolicyAttributeTypeDescriptions _
instance showPolicyAttributeTypeDescriptions :: Show PolicyAttributeTypeDescriptions where
  show = genericShow
instance decodePolicyAttributeTypeDescriptions :: Decode PolicyAttributeTypeDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyAttributeTypeDescriptions :: Encode PolicyAttributeTypeDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyAttributes = PolicyAttributes (Array PolicyAttribute)
derive instance newtypePolicyAttributes :: Newtype PolicyAttributes _
derive instance repGenericPolicyAttributes :: Generic PolicyAttributes _
instance showPolicyAttributes :: Show PolicyAttributes where
  show = genericShow
instance decodePolicyAttributes :: Decode PolicyAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyAttributes :: Encode PolicyAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a policy.</p>
newtype PolicyDescription = PolicyDescription 
  { "PolicyName" :: NullOrUndefined.NullOrUndefined (PolicyName)
  , "PolicyTypeName" :: NullOrUndefined.NullOrUndefined (PolicyTypeName)
  , "PolicyAttributeDescriptions" :: NullOrUndefined.NullOrUndefined (PolicyAttributeDescriptions)
  }
derive instance newtypePolicyDescription :: Newtype PolicyDescription _
derive instance repGenericPolicyDescription :: Generic PolicyDescription _
instance showPolicyDescription :: Show PolicyDescription where
  show = genericShow
instance decodePolicyDescription :: Decode PolicyDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyDescription :: Encode PolicyDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyDescriptions = PolicyDescriptions (Array PolicyDescription)
derive instance newtypePolicyDescriptions :: Newtype PolicyDescriptions _
derive instance repGenericPolicyDescriptions :: Generic PolicyDescriptions _
instance showPolicyDescriptions :: Show PolicyDescriptions where
  show = genericShow
instance decodePolicyDescriptions :: Decode PolicyDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyDescriptions :: Encode PolicyDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyName = PolicyName String
derive instance newtypePolicyName :: Newtype PolicyName _
derive instance repGenericPolicyName :: Generic PolicyName _
instance showPolicyName :: Show PolicyName where
  show = genericShow
instance decodePolicyName :: Decode PolicyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyName :: Encode PolicyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyNames = PolicyNames (Array PolicyName)
derive instance newtypePolicyNames :: Newtype PolicyNames _
derive instance repGenericPolicyNames :: Generic PolicyNames _
instance showPolicyNames :: Show PolicyNames where
  show = genericShow
instance decodePolicyNames :: Decode PolicyNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyNames :: Encode PolicyNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more of the specified policies do not exist.</p>
newtype PolicyNotFoundException = PolicyNotFoundException Types.NoArguments
derive instance newtypePolicyNotFoundException :: Newtype PolicyNotFoundException _
derive instance repGenericPolicyNotFoundException :: Generic PolicyNotFoundException _
instance showPolicyNotFoundException :: Show PolicyNotFoundException where
  show = genericShow
instance decodePolicyNotFoundException :: Decode PolicyNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyNotFoundException :: Encode PolicyNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a policy type.</p>
newtype PolicyTypeDescription = PolicyTypeDescription 
  { "PolicyTypeName" :: NullOrUndefined.NullOrUndefined (PolicyTypeName)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "PolicyAttributeTypeDescriptions" :: NullOrUndefined.NullOrUndefined (PolicyAttributeTypeDescriptions)
  }
derive instance newtypePolicyTypeDescription :: Newtype PolicyTypeDescription _
derive instance repGenericPolicyTypeDescription :: Generic PolicyTypeDescription _
instance showPolicyTypeDescription :: Show PolicyTypeDescription where
  show = genericShow
instance decodePolicyTypeDescription :: Decode PolicyTypeDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyTypeDescription :: Encode PolicyTypeDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyTypeDescriptions = PolicyTypeDescriptions (Array PolicyTypeDescription)
derive instance newtypePolicyTypeDescriptions :: Newtype PolicyTypeDescriptions _
derive instance repGenericPolicyTypeDescriptions :: Generic PolicyTypeDescriptions _
instance showPolicyTypeDescriptions :: Show PolicyTypeDescriptions where
  show = genericShow
instance decodePolicyTypeDescriptions :: Decode PolicyTypeDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyTypeDescriptions :: Encode PolicyTypeDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyTypeName = PolicyTypeName String
derive instance newtypePolicyTypeName :: Newtype PolicyTypeName _
derive instance repGenericPolicyTypeName :: Generic PolicyTypeName _
instance showPolicyTypeName :: Show PolicyTypeName where
  show = genericShow
instance decodePolicyTypeName :: Decode PolicyTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyTypeName :: Encode PolicyTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PolicyTypeNames = PolicyTypeNames (Array PolicyTypeName)
derive instance newtypePolicyTypeNames :: Newtype PolicyTypeNames _
derive instance repGenericPolicyTypeNames :: Generic PolicyTypeNames _
instance showPolicyTypeNames :: Show PolicyTypeNames where
  show = genericShow
instance decodePolicyTypeNames :: Decode PolicyTypeNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyTypeNames :: Encode PolicyTypeNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more of the specified policy types do not exist.</p>
newtype PolicyTypeNotFoundException = PolicyTypeNotFoundException Types.NoArguments
derive instance newtypePolicyTypeNotFoundException :: Newtype PolicyTypeNotFoundException _
derive instance repGenericPolicyTypeNotFoundException :: Generic PolicyTypeNotFoundException _
instance showPolicyTypeNotFoundException :: Show PolicyTypeNotFoundException where
  show = genericShow
instance decodePolicyTypeNotFoundException :: Decode PolicyTypeNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePolicyTypeNotFoundException :: Encode PolicyTypeNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Ports = Ports (Array AccessPointPort)
derive instance newtypePorts :: Newtype Ports _
derive instance repGenericPorts :: Generic Ports _
instance showPorts :: Show Ports where
  show = genericShow
instance decodePorts :: Decode Ports where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePorts :: Encode Ports where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Protocol = Protocol String
derive instance newtypeProtocol :: Newtype Protocol _
derive instance repGenericProtocol :: Generic Protocol _
instance showProtocol :: Show Protocol where
  show = genericShow
instance decodeProtocol :: Decode Protocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProtocol :: Encode Protocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReasonCode = ReasonCode String
derive instance newtypeReasonCode :: Newtype ReasonCode _
derive instance repGenericReasonCode :: Generic ReasonCode _
instance showReasonCode :: Show ReasonCode where
  show = genericShow
instance decodeReasonCode :: Decode ReasonCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReasonCode :: Encode ReasonCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for RegisterInstancesWithLoadBalancer.</p>
newtype RegisterEndPointsInput = RegisterEndPointsInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Instances" :: (Instances)
  }
derive instance newtypeRegisterEndPointsInput :: Newtype RegisterEndPointsInput _
derive instance repGenericRegisterEndPointsInput :: Generic RegisterEndPointsInput _
instance showRegisterEndPointsInput :: Show RegisterEndPointsInput where
  show = genericShow
instance decodeRegisterEndPointsInput :: Decode RegisterEndPointsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterEndPointsInput :: Encode RegisterEndPointsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of RegisterInstancesWithLoadBalancer.</p>
newtype RegisterEndPointsOutput = RegisterEndPointsOutput 
  { "Instances" :: NullOrUndefined.NullOrUndefined (Instances)
  }
derive instance newtypeRegisterEndPointsOutput :: Newtype RegisterEndPointsOutput _
derive instance repGenericRegisterEndPointsOutput :: Generic RegisterEndPointsOutput _
instance showRegisterEndPointsOutput :: Show RegisterEndPointsOutput where
  show = genericShow
instance decodeRegisterEndPointsOutput :: Decode RegisterEndPointsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterEndPointsOutput :: Encode RegisterEndPointsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for DisableAvailabilityZonesForLoadBalancer.</p>
newtype RemoveAvailabilityZonesInput = RemoveAvailabilityZonesInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "AvailabilityZones" :: (AvailabilityZones)
  }
derive instance newtypeRemoveAvailabilityZonesInput :: Newtype RemoveAvailabilityZonesInput _
derive instance repGenericRemoveAvailabilityZonesInput :: Generic RemoveAvailabilityZonesInput _
instance showRemoveAvailabilityZonesInput :: Show RemoveAvailabilityZonesInput where
  show = genericShow
instance decodeRemoveAvailabilityZonesInput :: Decode RemoveAvailabilityZonesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveAvailabilityZonesInput :: Encode RemoveAvailabilityZonesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output for DisableAvailabilityZonesForLoadBalancer.</p>
newtype RemoveAvailabilityZonesOutput = RemoveAvailabilityZonesOutput 
  { "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZones)
  }
derive instance newtypeRemoveAvailabilityZonesOutput :: Newtype RemoveAvailabilityZonesOutput _
derive instance repGenericRemoveAvailabilityZonesOutput :: Generic RemoveAvailabilityZonesOutput _
instance showRemoveAvailabilityZonesOutput :: Show RemoveAvailabilityZonesOutput where
  show = genericShow
instance decodeRemoveAvailabilityZonesOutput :: Decode RemoveAvailabilityZonesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveAvailabilityZonesOutput :: Encode RemoveAvailabilityZonesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for RemoveTags.</p>
newtype RemoveTagsInput = RemoveTagsInput 
  { "LoadBalancerNames" :: (LoadBalancerNames)
  , "Tags" :: (TagKeyList)
  }
derive instance newtypeRemoveTagsInput :: Newtype RemoveTagsInput _
derive instance repGenericRemoveTagsInput :: Generic RemoveTagsInput _
instance showRemoveTagsInput :: Show RemoveTagsInput where
  show = genericShow
instance decodeRemoveTagsInput :: Decode RemoveTagsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsInput :: Encode RemoveTagsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of RemoveTags.</p>
newtype RemoveTagsOutput = RemoveTagsOutput Types.NoArguments
derive instance newtypeRemoveTagsOutput :: Newtype RemoveTagsOutput _
derive instance repGenericRemoveTagsOutput :: Generic RemoveTagsOutput _
instance showRemoveTagsOutput :: Show RemoveTagsOutput where
  show = genericShow
instance decodeRemoveTagsOutput :: Decode RemoveTagsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsOutput :: Encode RemoveTagsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3BucketName = S3BucketName String
derive instance newtypeS3BucketName :: Newtype S3BucketName _
derive instance repGenericS3BucketName :: Generic S3BucketName _
instance showS3BucketName :: Show S3BucketName where
  show = genericShow
instance decodeS3BucketName :: Decode S3BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3BucketName :: Encode S3BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SSLCertificateId = SSLCertificateId String
derive instance newtypeSSLCertificateId :: Newtype SSLCertificateId _
derive instance repGenericSSLCertificateId :: Generic SSLCertificateId _
instance showSSLCertificateId :: Show SSLCertificateId where
  show = genericShow
instance decodeSSLCertificateId :: Decode SSLCertificateId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSSLCertificateId :: Encode SSLCertificateId where
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


newtype SecurityGroupName = SecurityGroupName String
derive instance newtypeSecurityGroupName :: Newtype SecurityGroupName _
derive instance repGenericSecurityGroupName :: Generic SecurityGroupName _
instance showSecurityGroupName :: Show SecurityGroupName where
  show = genericShow
instance decodeSecurityGroupName :: Decode SecurityGroupName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupName :: Encode SecurityGroupName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroupOwnerAlias = SecurityGroupOwnerAlias String
derive instance newtypeSecurityGroupOwnerAlias :: Newtype SecurityGroupOwnerAlias _
derive instance repGenericSecurityGroupOwnerAlias :: Generic SecurityGroupOwnerAlias _
instance showSecurityGroupOwnerAlias :: Show SecurityGroupOwnerAlias where
  show = genericShow
instance decodeSecurityGroupOwnerAlias :: Decode SecurityGroupOwnerAlias where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupOwnerAlias :: Encode SecurityGroupOwnerAlias where
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


-- | <p>Contains the parameters for SetLoadBalancerListenerSSLCertificate.</p>
newtype SetLoadBalancerListenerSSLCertificateInput = SetLoadBalancerListenerSSLCertificateInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "LoadBalancerPort" :: (AccessPointPort)
  , "SSLCertificateId" :: (SSLCertificateId)
  }
derive instance newtypeSetLoadBalancerListenerSSLCertificateInput :: Newtype SetLoadBalancerListenerSSLCertificateInput _
derive instance repGenericSetLoadBalancerListenerSSLCertificateInput :: Generic SetLoadBalancerListenerSSLCertificateInput _
instance showSetLoadBalancerListenerSSLCertificateInput :: Show SetLoadBalancerListenerSSLCertificateInput where
  show = genericShow
instance decodeSetLoadBalancerListenerSSLCertificateInput :: Decode SetLoadBalancerListenerSSLCertificateInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLoadBalancerListenerSSLCertificateInput :: Encode SetLoadBalancerListenerSSLCertificateInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of SetLoadBalancerListenerSSLCertificate.</p>
newtype SetLoadBalancerListenerSSLCertificateOutput = SetLoadBalancerListenerSSLCertificateOutput Types.NoArguments
derive instance newtypeSetLoadBalancerListenerSSLCertificateOutput :: Newtype SetLoadBalancerListenerSSLCertificateOutput _
derive instance repGenericSetLoadBalancerListenerSSLCertificateOutput :: Generic SetLoadBalancerListenerSSLCertificateOutput _
instance showSetLoadBalancerListenerSSLCertificateOutput :: Show SetLoadBalancerListenerSSLCertificateOutput where
  show = genericShow
instance decodeSetLoadBalancerListenerSSLCertificateOutput :: Decode SetLoadBalancerListenerSSLCertificateOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLoadBalancerListenerSSLCertificateOutput :: Encode SetLoadBalancerListenerSSLCertificateOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for SetLoadBalancerPoliciesForBackendServer.</p>
newtype SetLoadBalancerPoliciesForBackendServerInput = SetLoadBalancerPoliciesForBackendServerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "InstancePort" :: (EndPointPort)
  , "PolicyNames" :: (PolicyNames)
  }
derive instance newtypeSetLoadBalancerPoliciesForBackendServerInput :: Newtype SetLoadBalancerPoliciesForBackendServerInput _
derive instance repGenericSetLoadBalancerPoliciesForBackendServerInput :: Generic SetLoadBalancerPoliciesForBackendServerInput _
instance showSetLoadBalancerPoliciesForBackendServerInput :: Show SetLoadBalancerPoliciesForBackendServerInput where
  show = genericShow
instance decodeSetLoadBalancerPoliciesForBackendServerInput :: Decode SetLoadBalancerPoliciesForBackendServerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLoadBalancerPoliciesForBackendServerInput :: Encode SetLoadBalancerPoliciesForBackendServerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of SetLoadBalancerPoliciesForBackendServer.</p>
newtype SetLoadBalancerPoliciesForBackendServerOutput = SetLoadBalancerPoliciesForBackendServerOutput Types.NoArguments
derive instance newtypeSetLoadBalancerPoliciesForBackendServerOutput :: Newtype SetLoadBalancerPoliciesForBackendServerOutput _
derive instance repGenericSetLoadBalancerPoliciesForBackendServerOutput :: Generic SetLoadBalancerPoliciesForBackendServerOutput _
instance showSetLoadBalancerPoliciesForBackendServerOutput :: Show SetLoadBalancerPoliciesForBackendServerOutput where
  show = genericShow
instance decodeSetLoadBalancerPoliciesForBackendServerOutput :: Decode SetLoadBalancerPoliciesForBackendServerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLoadBalancerPoliciesForBackendServerOutput :: Encode SetLoadBalancerPoliciesForBackendServerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the parameters for SetLoadBalancePoliciesOfListener.</p>
newtype SetLoadBalancerPoliciesOfListenerInput = SetLoadBalancerPoliciesOfListenerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "LoadBalancerPort" :: (AccessPointPort)
  , "PolicyNames" :: (PolicyNames)
  }
derive instance newtypeSetLoadBalancerPoliciesOfListenerInput :: Newtype SetLoadBalancerPoliciesOfListenerInput _
derive instance repGenericSetLoadBalancerPoliciesOfListenerInput :: Generic SetLoadBalancerPoliciesOfListenerInput _
instance showSetLoadBalancerPoliciesOfListenerInput :: Show SetLoadBalancerPoliciesOfListenerInput where
  show = genericShow
instance decodeSetLoadBalancerPoliciesOfListenerInput :: Decode SetLoadBalancerPoliciesOfListenerInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLoadBalancerPoliciesOfListenerInput :: Encode SetLoadBalancerPoliciesOfListenerInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the output of SetLoadBalancePoliciesOfListener.</p>
newtype SetLoadBalancerPoliciesOfListenerOutput = SetLoadBalancerPoliciesOfListenerOutput Types.NoArguments
derive instance newtypeSetLoadBalancerPoliciesOfListenerOutput :: Newtype SetLoadBalancerPoliciesOfListenerOutput _
derive instance repGenericSetLoadBalancerPoliciesOfListenerOutput :: Generic SetLoadBalancerPoliciesOfListenerOutput _
instance showSetLoadBalancerPoliciesOfListenerOutput :: Show SetLoadBalancerPoliciesOfListenerOutput where
  show = genericShow
instance decodeSetLoadBalancerPoliciesOfListenerOutput :: Decode SetLoadBalancerPoliciesOfListenerOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSetLoadBalancerPoliciesOfListenerOutput :: Encode SetLoadBalancerPoliciesOfListenerOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a source security group.</p>
newtype SourceSecurityGroup = SourceSecurityGroup 
  { "OwnerAlias" :: NullOrUndefined.NullOrUndefined (SecurityGroupOwnerAlias)
  , "GroupName" :: NullOrUndefined.NullOrUndefined (SecurityGroupName)
  }
derive instance newtypeSourceSecurityGroup :: Newtype SourceSecurityGroup _
derive instance repGenericSourceSecurityGroup :: Generic SourceSecurityGroup _
instance showSourceSecurityGroup :: Show SourceSecurityGroup where
  show = genericShow
instance decodeSourceSecurityGroup :: Decode SourceSecurityGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceSecurityGroup :: Encode SourceSecurityGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype State = State String
derive instance newtypeState :: Newtype State _
derive instance repGenericState :: Generic State _
instance showState :: Show State where
  show = genericShow
instance decodeState :: Decode State where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeState :: Encode State where
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


-- | <p>One or more of the specified subnets do not exist.</p>
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


-- | <p>The tags associated with a load balancer.</p>
newtype TagDescription = TagDescription 
  { "LoadBalancerName" :: NullOrUndefined.NullOrUndefined (AccessPointName)
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


newtype TagKeyList = TagKeyList (Array TagKeyOnly)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The key of a tag.</p>
newtype TagKeyOnly = TagKeyOnly 
  { "Key" :: NullOrUndefined.NullOrUndefined (TagKey)
  }
derive instance newtypeTagKeyOnly :: Newtype TagKeyOnly _
derive instance repGenericTagKeyOnly :: Generic TagKeyOnly _
instance showTagKeyOnly :: Show TagKeyOnly where
  show = genericShow
instance decodeTagKeyOnly :: Decode TagKeyOnly where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyOnly :: Encode TagKeyOnly where
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


-- | <p>The quota for the number of load balancers has been reached.</p>
newtype TooManyAccessPointsException = TooManyAccessPointsException Types.NoArguments
derive instance newtypeTooManyAccessPointsException :: Newtype TooManyAccessPointsException _
derive instance repGenericTooManyAccessPointsException :: Generic TooManyAccessPointsException _
instance showTooManyAccessPointsException :: Show TooManyAccessPointsException where
  show = genericShow
instance decodeTooManyAccessPointsException :: Decode TooManyAccessPointsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyAccessPointsException :: Encode TooManyAccessPointsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The quota for the number of policies for this load balancer has been reached.</p>
newtype TooManyPoliciesException = TooManyPoliciesException Types.NoArguments
derive instance newtypeTooManyPoliciesException :: Newtype TooManyPoliciesException _
derive instance repGenericTooManyPoliciesException :: Generic TooManyPoliciesException _
instance showTooManyPoliciesException :: Show TooManyPoliciesException where
  show = genericShow
instance decodeTooManyPoliciesException :: Decode TooManyPoliciesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyPoliciesException :: Encode TooManyPoliciesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The quota for the number of tags that can be assigned to a load balancer has been reached.</p>
newtype TooManyTagsException = TooManyTagsException Types.NoArguments
derive instance newtypeTooManyTagsException :: Newtype TooManyTagsException _
derive instance repGenericTooManyTagsException :: Generic TooManyTagsException _
instance showTooManyTagsException :: Show TooManyTagsException where
  show = genericShow
instance decodeTooManyTagsException :: Decode TooManyTagsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTagsException :: Encode TooManyTagsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UnhealthyThreshold = UnhealthyThreshold Int
derive instance newtypeUnhealthyThreshold :: Newtype UnhealthyThreshold _
derive instance repGenericUnhealthyThreshold :: Generic UnhealthyThreshold _
instance showUnhealthyThreshold :: Show UnhealthyThreshold where
  show = genericShow
instance decodeUnhealthyThreshold :: Decode UnhealthyThreshold where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnhealthyThreshold :: Encode UnhealthyThreshold where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified protocol or signature version is not supported.</p>
newtype UnsupportedProtocolException = UnsupportedProtocolException Types.NoArguments
derive instance newtypeUnsupportedProtocolException :: Newtype UnsupportedProtocolException _
derive instance repGenericUnsupportedProtocolException :: Generic UnsupportedProtocolException _
instance showUnsupportedProtocolException :: Show UnsupportedProtocolException where
  show = genericShow
instance decodeUnsupportedProtocolException :: Decode UnsupportedProtocolException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedProtocolException :: Encode UnsupportedProtocolException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VPCId = VPCId String
derive instance newtypeVPCId :: Newtype VPCId _
derive instance repGenericVPCId :: Generic VPCId _
instance showVPCId :: Show VPCId where
  show = genericShow
instance decodeVPCId :: Decode VPCId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCId :: Encode VPCId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
