

-- | <fullname>Elastic Load Balancing</fullname> <p>A load balancer can distribute incoming traffic across your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered instances and ensures that it routes traffic only to healthy instances. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer and a protocol and port number for connections from the load balancer to the instances.</p> <p>Elastic Load Balancing supports three types of load balancers: Application Load Balancers, Network Load Balancers, and Classic Load Balancers. You can select a load balancer based on your application needs. For more information, see the <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/">Elastic Load Balancing User Guide</a>.</p> <p>This reference covers the 2012-06-01 API, which supports Classic Load Balancers. The 2015-12-01 API supports Application Load Balancers and Network Load Balancers.</p> <p>To get started, create a load balancer with one or more listeners using <a>CreateLoadBalancer</a>. Register your instances with the load balancer using <a>RegisterInstancesWithLoadBalancer</a>.</p> <p>All Elastic Load Balancing operations are <i>idempotent</i>, which means that they complete at most one time. If you repeat an operation, it succeeds with a 200 OK response code.</p>
module AWS.ELB where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ELB" :: String


-- | <p>Adds the specified tags to the specified load balancer. Each load balancer can have a maximum of 10 tags.</p> <p>Each tag consists of a key and an optional value. If a tag with the same key is already associated with the load balancer, <code>AddTags</code> updates its value.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/add-remove-tags.html">Tag Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
addTags :: forall eff. AddTagsInput -> Aff (err :: AWS.RequestError | eff) AddTagsOutput
addTags = AWS.request serviceName "AddTags" 


-- | <p>Associates one or more security groups with your load balancer in a virtual private cloud (VPC). The specified security groups override the previously associated security groups.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-security-groups.html#elb-vpc-security-groups">Security Groups for Load Balancers in a VPC</a> in the <i>Classic Load Balancer Guide</i>.</p>
applySecurityGroupsToLoadBalancer :: forall eff. ApplySecurityGroupsToLoadBalancerInput -> Aff (err :: AWS.RequestError | eff) ApplySecurityGroupsToLoadBalancerOutput
applySecurityGroupsToLoadBalancer = AWS.request serviceName "ApplySecurityGroupsToLoadBalancer" 


-- | <p>Adds one or more subnets to the set of configured subnets for the specified load balancer.</p> <p>The load balancer evenly distributes requests across all registered subnets. For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-manage-subnets.html">Add or Remove Subnets for Your Load Balancer in a VPC</a> in the <i>Classic Load Balancer Guide</i>.</p>
attachLoadBalancerToSubnets :: forall eff. AttachLoadBalancerToSubnetsInput -> Aff (err :: AWS.RequestError | eff) AttachLoadBalancerToSubnetsOutput
attachLoadBalancerToSubnets = AWS.request serviceName "AttachLoadBalancerToSubnets" 


-- | <p>Specifies the health check settings to use when evaluating the health state of your EC2 instances.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-healthchecks.html">Configure Health Checks for Your Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
configureHealthCheck :: forall eff. ConfigureHealthCheckInput -> Aff (err :: AWS.RequestError | eff) ConfigureHealthCheckOutput
configureHealthCheck = AWS.request serviceName "ConfigureHealthCheck" 


-- | <p>Generates a stickiness policy with sticky session lifetimes that follow that of an application-generated cookie. This policy can be associated only with HTTP/HTTPS listeners.</p> <p>This policy is similar to the policy created by <a>CreateLBCookieStickinessPolicy</a>, except that the lifetime of the special Elastic Load Balancing cookie, <code>AWSELB</code>, follows the lifetime of the application-generated cookie specified in the policy configuration. The load balancer only inserts a new stickiness cookie when the application response includes a new application cookie.</p> <p>If the application cookie is explicitly removed or expires, the session stops being sticky until a new application cookie is issued.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application">Application-Controlled Session Stickiness</a> in the <i>Classic Load Balancer Guide</i>.</p>
createAppCookieStickinessPolicy :: forall eff. CreateAppCookieStickinessPolicyInput -> Aff (err :: AWS.RequestError | eff) CreateAppCookieStickinessPolicyOutput
createAppCookieStickinessPolicy = AWS.request serviceName "CreateAppCookieStickinessPolicy" 


-- | <p>Generates a stickiness policy with sticky session lifetimes controlled by the lifetime of the browser (user-agent) or a specified expiration period. This policy can be associated only with HTTP/HTTPS listeners.</p> <p>When a load balancer implements this policy, the load balancer uses a special cookie to track the instance for each request. When the load balancer receives a request, it first checks to see if this cookie is present in the request. If so, the load balancer sends the request to the application server specified in the cookie. If not, the load balancer sends the request to a server that is chosen based on the existing load-balancing algorithm.</p> <p>A cookie is inserted into the response for binding subsequent requests from the same user to that server. The validity of the cookie is based on the cookie expiration time, which is specified in the policy configuration.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration">Duration-Based Session Stickiness</a> in the <i>Classic Load Balancer Guide</i>.</p>
createLBCookieStickinessPolicy :: forall eff. CreateLBCookieStickinessPolicyInput -> Aff (err :: AWS.RequestError | eff) CreateLBCookieStickinessPolicyOutput
createLBCookieStickinessPolicy = AWS.request serviceName "CreateLBCookieStickinessPolicy" 


-- | <p>Creates a Classic Load Balancer.</p> <p>You can add listeners, security groups, subnets, and tags when you create your load balancer, or you can add them later using <a>CreateLoadBalancerListeners</a>, <a>ApplySecurityGroupsToLoadBalancer</a>, <a>AttachLoadBalancerToSubnets</a>, and <a>AddTags</a>.</p> <p>To describe your current load balancers, see <a>DescribeLoadBalancers</a>. When you are finished with a load balancer, you can delete it using <a>DeleteLoadBalancer</a>.</p> <p>You can create up to 20 load balancers per region per account. You can request an increase for the number of load balancers for your account. For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-limits.html">Limits for Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
createLoadBalancer :: forall eff. CreateAccessPointInput -> Aff (err :: AWS.RequestError | eff) CreateAccessPointOutput
createLoadBalancer = AWS.request serviceName "CreateLoadBalancer" 


-- | <p>Creates one or more listeners for the specified load balancer. If a listener with the specified port does not already exist, it is created; otherwise, the properties of the new listener must match the properties of the existing listener.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html">Listeners for Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
createLoadBalancerListeners :: forall eff. CreateLoadBalancerListenerInput -> Aff (err :: AWS.RequestError | eff) CreateLoadBalancerListenerOutput
createLoadBalancerListeners = AWS.request serviceName "CreateLoadBalancerListeners" 


-- | <p>Creates a policy with the specified attributes for the specified load balancer.</p> <p>Policies are settings that are saved for your load balancer and that can be applied to the listener or the application server, depending on the policy type.</p>
createLoadBalancerPolicy :: forall eff. CreateLoadBalancerPolicyInput -> Aff (err :: AWS.RequestError | eff) CreateLoadBalancerPolicyOutput
createLoadBalancerPolicy = AWS.request serviceName "CreateLoadBalancerPolicy" 


-- | <p>Deletes the specified load balancer.</p> <p>If you are attempting to recreate a load balancer, you must reconfigure all settings. The DNS name associated with a deleted load balancer are no longer usable. The name and associated DNS record of the deleted load balancer no longer exist and traffic sent to any of its IP addresses is no longer delivered to your instances.</p> <p>If the load balancer does not exist or has already been deleted, the call to <code>DeleteLoadBalancer</code> still succeeds.</p>
deleteLoadBalancer :: forall eff. DeleteAccessPointInput -> Aff (err :: AWS.RequestError | eff) DeleteAccessPointOutput
deleteLoadBalancer = AWS.request serviceName "DeleteLoadBalancer" 


-- | <p>Deletes the specified listeners from the specified load balancer.</p>
deleteLoadBalancerListeners :: forall eff. DeleteLoadBalancerListenerInput -> Aff (err :: AWS.RequestError | eff) DeleteLoadBalancerListenerOutput
deleteLoadBalancerListeners = AWS.request serviceName "DeleteLoadBalancerListeners" 


-- | <p>Deletes the specified policy from the specified load balancer. This policy must not be enabled for any listeners.</p>
deleteLoadBalancerPolicy :: forall eff. DeleteLoadBalancerPolicyInput -> Aff (err :: AWS.RequestError | eff) DeleteLoadBalancerPolicyOutput
deleteLoadBalancerPolicy = AWS.request serviceName "DeleteLoadBalancerPolicy" 


-- | <p>Deregisters the specified instances from the specified load balancer. After the instance is deregistered, it no longer receives traffic from the load balancer.</p> <p>You can use <a>DescribeLoadBalancers</a> to verify that the instance is deregistered from the load balancer.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html">Register or De-Register EC2 Instances</a> in the <i>Classic Load Balancer Guide</i>.</p>
deregisterInstancesFromLoadBalancer :: forall eff. DeregisterEndPointsInput -> Aff (err :: AWS.RequestError | eff) DeregisterEndPointsOutput
deregisterInstancesFromLoadBalancer = AWS.request serviceName "DeregisterInstancesFromLoadBalancer" 


-- | <p>Describes the current Elastic Load Balancing resource limits for your AWS account.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-limits.html">Limits for Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
describeAccountLimits :: forall eff. DescribeAccountLimitsInput -> Aff (err :: AWS.RequestError | eff) DescribeAccountLimitsOutput
describeAccountLimits = AWS.request serviceName "DescribeAccountLimits" 


-- | <p>Describes the state of the specified instances with respect to the specified load balancer. If no instances are specified, the call describes the state of all instances that are currently registered with the load balancer. If instances are specified, their state is returned even if they are no longer registered with the load balancer. The state of terminated instances is not returned.</p>
describeInstanceHealth :: forall eff. DescribeEndPointStateInput -> Aff (err :: AWS.RequestError | eff) DescribeEndPointStateOutput
describeInstanceHealth = AWS.request serviceName "DescribeInstanceHealth" 


-- | <p>Describes the attributes for the specified load balancer.</p>
describeLoadBalancerAttributes :: forall eff. DescribeLoadBalancerAttributesInput -> Aff (err :: AWS.RequestError | eff) DescribeLoadBalancerAttributesOutput
describeLoadBalancerAttributes = AWS.request serviceName "DescribeLoadBalancerAttributes" 


-- | <p>Describes the specified policies.</p> <p>If you specify a load balancer name, the action returns the descriptions of all policies created for the load balancer. If you specify a policy name associated with your load balancer, the action returns the description of that policy. If you don't specify a load balancer name, the action returns descriptions of the specified sample policies, or descriptions of all sample policies. The names of the sample policies have the <code>ELBSample-</code> prefix.</p>
describeLoadBalancerPolicies :: forall eff. DescribeLoadBalancerPoliciesInput -> Aff (err :: AWS.RequestError | eff) DescribeLoadBalancerPoliciesOutput
describeLoadBalancerPolicies = AWS.request serviceName "DescribeLoadBalancerPolicies" 


-- | <p>Describes the specified load balancer policy types or all load balancer policy types.</p> <p>The description of each type indicates how it can be used. For example, some policies can be used only with layer 7 listeners, some policies can be used only with layer 4 listeners, and some policies can be used only with your EC2 instances.</p> <p>You can use <a>CreateLoadBalancerPolicy</a> to create a policy configuration for any of these policy types. Then, depending on the policy type, use either <a>SetLoadBalancerPoliciesOfListener</a> or <a>SetLoadBalancerPoliciesForBackendServer</a> to set the policy.</p>
describeLoadBalancerPolicyTypes :: forall eff. DescribeLoadBalancerPolicyTypesInput -> Aff (err :: AWS.RequestError | eff) DescribeLoadBalancerPolicyTypesOutput
describeLoadBalancerPolicyTypes = AWS.request serviceName "DescribeLoadBalancerPolicyTypes" 


-- | <p>Describes the specified the load balancers. If no load balancers are specified, the call describes all of your load balancers.</p>
describeLoadBalancers :: forall eff. DescribeAccessPointsInput -> Aff (err :: AWS.RequestError | eff) DescribeAccessPointsOutput
describeLoadBalancers = AWS.request serviceName "DescribeLoadBalancers" 


-- | <p>Describes the tags associated with the specified load balancers.</p>
describeTags :: forall eff. DescribeTagsInput -> Aff (err :: AWS.RequestError | eff) DescribeTagsOutput
describeTags = AWS.request serviceName "DescribeTags" 


-- | <p>Removes the specified subnets from the set of configured subnets for the load balancer.</p> <p>After a subnet is removed, all EC2 instances registered with the load balancer in the removed subnet go into the <code>OutOfService</code> state. Then, the load balancer balances the traffic among the remaining routable subnets.</p>
detachLoadBalancerFromSubnets :: forall eff. DetachLoadBalancerFromSubnetsInput -> Aff (err :: AWS.RequestError | eff) DetachLoadBalancerFromSubnetsOutput
detachLoadBalancerFromSubnets = AWS.request serviceName "DetachLoadBalancerFromSubnets" 


-- | <p>Removes the specified Availability Zones from the set of Availability Zones for the specified load balancer.</p> <p>There must be at least one Availability Zone registered with a load balancer at all times. After an Availability Zone is removed, all instances registered with the load balancer that are in the removed Availability Zone go into the <code>OutOfService</code> state. Then, the load balancer attempts to equally balance the traffic among its remaining Availability Zones.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html">Add or Remove Availability Zones</a> in the <i>Classic Load Balancer Guide</i>.</p>
disableAvailabilityZonesForLoadBalancer :: forall eff. RemoveAvailabilityZonesInput -> Aff (err :: AWS.RequestError | eff) RemoveAvailabilityZonesOutput
disableAvailabilityZonesForLoadBalancer = AWS.request serviceName "DisableAvailabilityZonesForLoadBalancer" 


-- | <p>Adds the specified Availability Zones to the set of Availability Zones for the specified load balancer.</p> <p>The load balancer evenly distributes requests across all its registered Availability Zones that contain instances.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html">Add or Remove Availability Zones</a> in the <i>Classic Load Balancer Guide</i>.</p>
enableAvailabilityZonesForLoadBalancer :: forall eff. AddAvailabilityZonesInput -> Aff (err :: AWS.RequestError | eff) AddAvailabilityZonesOutput
enableAvailabilityZonesForLoadBalancer = AWS.request serviceName "EnableAvailabilityZonesForLoadBalancer" 


-- | <p>Modifies the attributes of the specified load balancer.</p> <p>You can modify the load balancer attributes, such as <code>AccessLogs</code>, <code>ConnectionDraining</code>, and <code>CrossZoneLoadBalancing</code> by either enabling or disabling them. Or, you can modify the load balancer attribute <code>ConnectionSettings</code> by specifying an idle connection timeout value for your load balancer.</p> <p>For more information, see the following in the <i>Classic Load Balancer Guide</i>:</p> <ul> <li> <p> <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html">Cross-Zone Load Balancing</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html">Connection Draining</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/access-log-collection.html">Access Logs</a> </p> </li> <li> <p> <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html">Idle Connection Timeout</a> </p> </li> </ul>
modifyLoadBalancerAttributes :: forall eff. ModifyLoadBalancerAttributesInput -> Aff (err :: AWS.RequestError | eff) ModifyLoadBalancerAttributesOutput
modifyLoadBalancerAttributes = AWS.request serviceName "ModifyLoadBalancerAttributes" 


-- | <p>Adds the specified instances to the specified load balancer.</p> <p>The instance must be a running instance in the same network as the load balancer (EC2-Classic or the same VPC). If you have EC2-Classic instances and a load balancer in a VPC with ClassicLink enabled, you can link the EC2-Classic instances to that VPC and then register the linked EC2-Classic instances with the load balancer in the VPC.</p> <p>Note that <code>RegisterInstanceWithLoadBalancer</code> completes when the request has been registered. Instance registration takes a little time to complete. To check the state of the registered instances, use <a>DescribeLoadBalancers</a> or <a>DescribeInstanceHealth</a>.</p> <p>After the instance is registered, it starts receiving traffic and requests from the load balancer. Any instance that is not in one of the Availability Zones registered for the load balancer is moved to the <code>OutOfService</code> state. If an Availability Zone is added to the load balancer later, any instances registered with the load balancer move to the <code>InService</code> state.</p> <p>To deregister instances from a load balancer, use <a>DeregisterInstancesFromLoadBalancer</a>.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html">Register or De-Register EC2 Instances</a> in the <i>Classic Load Balancer Guide</i>.</p>
registerInstancesWithLoadBalancer :: forall eff. RegisterEndPointsInput -> Aff (err :: AWS.RequestError | eff) RegisterEndPointsOutput
registerInstancesWithLoadBalancer = AWS.request serviceName "RegisterInstancesWithLoadBalancer" 


-- | <p>Removes one or more tags from the specified load balancer.</p>
removeTags :: forall eff. RemoveTagsInput -> Aff (err :: AWS.RequestError | eff) RemoveTagsOutput
removeTags = AWS.request serviceName "RemoveTags" 


-- | <p>Sets the certificate that terminates the specified listener's SSL connections. The specified certificate replaces any prior certificate that was used on the same load balancer and port.</p> <p>For more information about updating your SSL certificate, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-update-ssl-cert.html">Replace the SSL Certificate for Your Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
setLoadBalancerListenerSSLCertificate :: forall eff. SetLoadBalancerListenerSSLCertificateInput -> Aff (err :: AWS.RequestError | eff) SetLoadBalancerListenerSSLCertificateOutput
setLoadBalancerListenerSSLCertificate = AWS.request serviceName "SetLoadBalancerListenerSSLCertificate" 


-- | <p>Replaces the set of policies associated with the specified port on which the EC2 instance is listening with a new set of policies. At this time, only the back-end server authentication policy type can be applied to the instance ports; this policy type is composed of multiple public key policies.</p> <p>Each time you use <code>SetLoadBalancerPoliciesForBackendServer</code> to enable the policies, use the <code>PolicyNames</code> parameter to list the policies that you want to enable.</p> <p>You can use <a>DescribeLoadBalancers</a> or <a>DescribeLoadBalancerPolicies</a> to verify that the policy is associated with the EC2 instance.</p> <p>For more information about enabling back-end instance authentication, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-create-https-ssl-load-balancer.html#configure_backendauth_clt">Configure Back-end Instance Authentication</a> in the <i>Classic Load Balancer Guide</i>. For more information about Proxy Protocol, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-proxy-protocol.html">Configure Proxy Protocol Support</a> in the <i>Classic Load Balancer Guide</i>.</p>
setLoadBalancerPoliciesForBackendServer :: forall eff. SetLoadBalancerPoliciesForBackendServerInput -> Aff (err :: AWS.RequestError | eff) SetLoadBalancerPoliciesForBackendServerOutput
setLoadBalancerPoliciesForBackendServer = AWS.request serviceName "SetLoadBalancerPoliciesForBackendServer" 


-- | <p>Replaces the current set of policies for the specified load balancer port with the specified set of policies.</p> <p>To enable back-end server authentication, use <a>SetLoadBalancerPoliciesForBackendServer</a>.</p> <p>For more information about setting policies, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/ssl-config-update.html">Update the SSL Negotiation Configuration</a>, <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration">Duration-Based Session Stickiness</a>, and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application">Application-Controlled Session Stickiness</a> in the <i>Classic Load Balancer Guide</i>.</p>
setLoadBalancerPoliciesOfListener :: forall eff. SetLoadBalancerPoliciesOfListenerInput -> Aff (err :: AWS.RequestError | eff) SetLoadBalancerPoliciesOfListenerOutput
setLoadBalancerPoliciesOfListener = AWS.request serviceName "SetLoadBalancerPoliciesOfListener" 


-- | <p>Information about the <code>AccessLog</code> attribute.</p>
newtype AccessLog = AccessLog 
  { "Enabled" :: (AccessLogEnabled)
  , "S3BucketName" :: NullOrUndefined (S3BucketName)
  , "EmitInterval" :: NullOrUndefined (AccessLogInterval)
  , "S3BucketPrefix" :: NullOrUndefined (AccessLogPrefix)
  }


newtype AccessLogEnabled = AccessLogEnabled Boolean


newtype AccessLogInterval = AccessLogInterval Int


newtype AccessLogPrefix = AccessLogPrefix String


newtype AccessPointName = AccessPointName String


-- | <p>The specified load balancer does not exist.</p>
newtype AccessPointNotFoundException = AccessPointNotFoundException 
  { 
  }


newtype AccessPointPort = AccessPointPort Int


-- | <p>Contains the parameters for EnableAvailabilityZonesForLoadBalancer.</p>
newtype AddAvailabilityZonesInput = AddAvailabilityZonesInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "AvailabilityZones" :: (AvailabilityZones)
  }


-- | <p>Contains the output of EnableAvailabilityZonesForLoadBalancer.</p>
newtype AddAvailabilityZonesOutput = AddAvailabilityZonesOutput 
  { "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  }


-- | <p>Contains the parameters for AddTags.</p>
newtype AddTagsInput = AddTagsInput 
  { "LoadBalancerNames" :: (LoadBalancerNames)
  , "Tags" :: (TagList)
  }


-- | <p>Contains the output of AddTags.</p>
newtype AddTagsOutput = AddTagsOutput 
  { 
  }


-- | <p>This data type is reserved.</p>
newtype AdditionalAttribute = AdditionalAttribute 
  { "Key" :: NullOrUndefined (AdditionalAttributeKey)
  , "Value" :: NullOrUndefined (AdditionalAttributeValue)
  }


newtype AdditionalAttributeKey = AdditionalAttributeKey String


newtype AdditionalAttributeValue = AdditionalAttributeValue String


newtype AdditionalAttributes = AdditionalAttributes (Array AdditionalAttribute)


newtype AppCookieStickinessPolicies = AppCookieStickinessPolicies (Array AppCookieStickinessPolicy)


-- | <p>Information about a policy for application-controlled session stickiness.</p>
newtype AppCookieStickinessPolicy = AppCookieStickinessPolicy 
  { "PolicyName" :: NullOrUndefined (PolicyName)
  , "CookieName" :: NullOrUndefined (CookieName)
  }


-- | <p>Contains the parameters for ApplySecurityGroupsToLoadBalancer.</p>
newtype ApplySecurityGroupsToLoadBalancerInput = ApplySecurityGroupsToLoadBalancerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "SecurityGroups" :: (SecurityGroups)
  }


-- | <p>Contains the output of ApplySecurityGroupsToLoadBalancer.</p>
newtype ApplySecurityGroupsToLoadBalancerOutput = ApplySecurityGroupsToLoadBalancerOutput 
  { "SecurityGroups" :: NullOrUndefined (SecurityGroups)
  }


-- | <p>Contains the parameters for AttachLoaBalancerToSubnets.</p>
newtype AttachLoadBalancerToSubnetsInput = AttachLoadBalancerToSubnetsInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Subnets" :: (Subnets)
  }


-- | <p>Contains the output of AttachLoadBalancerToSubnets.</p>
newtype AttachLoadBalancerToSubnetsOutput = AttachLoadBalancerToSubnetsOutput 
  { "Subnets" :: NullOrUndefined (Subnets)
  }


newtype AttributeName = AttributeName String


newtype AttributeType = AttributeType String


newtype AttributeValue = AttributeValue String


newtype AvailabilityZone = AvailabilityZone String


newtype AvailabilityZones = AvailabilityZones (Array AvailabilityZone)


-- | <p>Information about the configuration of an EC2 instance.</p>
newtype BackendServerDescription = BackendServerDescription 
  { "InstancePort" :: NullOrUndefined (InstancePort)
  , "PolicyNames" :: NullOrUndefined (PolicyNames)
  }


newtype BackendServerDescriptions = BackendServerDescriptions (Array BackendServerDescription)


newtype Cardinality = Cardinality String


-- | <p>The specified ARN does not refer to a valid SSL certificate in AWS Identity and Access Management (IAM) or AWS Certificate Manager (ACM). Note that if you recently uploaded the certificate to IAM, this error might indicate that the certificate is not fully available yet.</p>
newtype CertificateNotFoundException = CertificateNotFoundException 
  { 
  }


-- | <p>Contains the parameters for ConfigureHealthCheck.</p>
newtype ConfigureHealthCheckInput = ConfigureHealthCheckInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "HealthCheck" :: (HealthCheck)
  }


-- | <p>Contains the output of ConfigureHealthCheck.</p>
newtype ConfigureHealthCheckOutput = ConfigureHealthCheckOutput 
  { "HealthCheck" :: NullOrUndefined (HealthCheck)
  }


-- | <p>Information about the <code>ConnectionDraining</code> attribute.</p>
newtype ConnectionDraining = ConnectionDraining 
  { "Enabled" :: (ConnectionDrainingEnabled)
  , "Timeout" :: NullOrUndefined (ConnectionDrainingTimeout)
  }


newtype ConnectionDrainingEnabled = ConnectionDrainingEnabled Boolean


newtype ConnectionDrainingTimeout = ConnectionDrainingTimeout Int


-- | <p>Information about the <code>ConnectionSettings</code> attribute.</p>
newtype ConnectionSettings = ConnectionSettings 
  { "IdleTimeout" :: (IdleTimeout)
  }


newtype CookieExpirationPeriod = CookieExpirationPeriod Number


newtype CookieName = CookieName String


-- | <p>Contains the parameters for CreateLoadBalancer.</p>
newtype CreateAccessPointInput = CreateAccessPointInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Listeners" :: (Listeners)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  , "Subnets" :: NullOrUndefined (Subnets)
  , "SecurityGroups" :: NullOrUndefined (SecurityGroups)
  , "Scheme" :: NullOrUndefined (LoadBalancerScheme)
  , "Tags" :: NullOrUndefined (TagList)
  }


-- | <p>Contains the output for CreateLoadBalancer.</p>
newtype CreateAccessPointOutput = CreateAccessPointOutput 
  { "DNSName" :: NullOrUndefined (DNSName)
  }


-- | <p>Contains the parameters for CreateAppCookieStickinessPolicy.</p>
newtype CreateAppCookieStickinessPolicyInput = CreateAppCookieStickinessPolicyInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "PolicyName" :: (PolicyName)
  , "CookieName" :: (CookieName)
  }


-- | <p>Contains the output for CreateAppCookieStickinessPolicy.</p>
newtype CreateAppCookieStickinessPolicyOutput = CreateAppCookieStickinessPolicyOutput 
  { 
  }


-- | <p>Contains the parameters for CreateLBCookieStickinessPolicy.</p>
newtype CreateLBCookieStickinessPolicyInput = CreateLBCookieStickinessPolicyInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "PolicyName" :: (PolicyName)
  , "CookieExpirationPeriod" :: NullOrUndefined (CookieExpirationPeriod)
  }


-- | <p>Contains the output for CreateLBCookieStickinessPolicy.</p>
newtype CreateLBCookieStickinessPolicyOutput = CreateLBCookieStickinessPolicyOutput 
  { 
  }


-- | <p>Contains the parameters for CreateLoadBalancerListeners.</p>
newtype CreateLoadBalancerListenerInput = CreateLoadBalancerListenerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Listeners" :: (Listeners)
  }


-- | <p>Contains the parameters for CreateLoadBalancerListener.</p>
newtype CreateLoadBalancerListenerOutput = CreateLoadBalancerListenerOutput 
  { 
  }


-- | <p>Contains the parameters for CreateLoadBalancerPolicy.</p>
newtype CreateLoadBalancerPolicyInput = CreateLoadBalancerPolicyInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "PolicyName" :: (PolicyName)
  , "PolicyTypeName" :: (PolicyTypeName)
  , "PolicyAttributes" :: NullOrUndefined (PolicyAttributes)
  }


-- | <p>Contains the output of CreateLoadBalancerPolicy.</p>
newtype CreateLoadBalancerPolicyOutput = CreateLoadBalancerPolicyOutput 
  { 
  }


newtype CreatedTime = CreatedTime Number


-- | <p>Information about the <code>CrossZoneLoadBalancing</code> attribute.</p>
newtype CrossZoneLoadBalancing = CrossZoneLoadBalancing 
  { "Enabled" :: (CrossZoneLoadBalancingEnabled)
  }


newtype CrossZoneLoadBalancingEnabled = CrossZoneLoadBalancingEnabled Boolean


newtype DNSName = DNSName String


newtype DefaultValue = DefaultValue String


-- | <p>Contains the parameters for DeleteLoadBalancer.</p>
newtype DeleteAccessPointInput = DeleteAccessPointInput 
  { "LoadBalancerName" :: (AccessPointName)
  }


-- | <p>Contains the output of DeleteLoadBalancer.</p>
newtype DeleteAccessPointOutput = DeleteAccessPointOutput 
  { 
  }


-- | <p>Contains the parameters for DeleteLoadBalancerListeners.</p>
newtype DeleteLoadBalancerListenerInput = DeleteLoadBalancerListenerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "LoadBalancerPorts" :: (Ports)
  }


-- | <p>Contains the output of DeleteLoadBalancerListeners.</p>
newtype DeleteLoadBalancerListenerOutput = DeleteLoadBalancerListenerOutput 
  { 
  }


-- | <p>Contains the parameters for DeleteLoadBalancerPolicy.</p>
newtype DeleteLoadBalancerPolicyInput = DeleteLoadBalancerPolicyInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "PolicyName" :: (PolicyName)
  }


-- | <p>Contains the output of DeleteLoadBalancerPolicy.</p>
newtype DeleteLoadBalancerPolicyOutput = DeleteLoadBalancerPolicyOutput 
  { 
  }


newtype DependencyThrottleException = DependencyThrottleException 
  { 
  }


-- | <p>Contains the parameters for DeregisterInstancesFromLoadBalancer.</p>
newtype DeregisterEndPointsInput = DeregisterEndPointsInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Instances" :: (Instances)
  }


-- | <p>Contains the output of DeregisterInstancesFromLoadBalancer.</p>
newtype DeregisterEndPointsOutput = DeregisterEndPointsOutput 
  { "Instances" :: NullOrUndefined (Instances)
  }


-- | <p>Contains the parameters for DescribeLoadBalancers.</p>
newtype DescribeAccessPointsInput = DescribeAccessPointsInput 
  { "LoadBalancerNames" :: NullOrUndefined (LoadBalancerNames)
  , "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


-- | <p>Contains the parameters for DescribeLoadBalancers.</p>
newtype DescribeAccessPointsOutput = DescribeAccessPointsOutput 
  { "LoadBalancerDescriptions" :: NullOrUndefined (LoadBalancerDescriptions)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


newtype DescribeAccountLimitsInput = DescribeAccountLimitsInput 
  { "Marker" :: NullOrUndefined (Marker)
  , "PageSize" :: NullOrUndefined (PageSize)
  }


newtype DescribeAccountLimitsOutput = DescribeAccountLimitsOutput 
  { "Limits" :: NullOrUndefined (Limits)
  , "NextMarker" :: NullOrUndefined (Marker)
  }


-- | <p>Contains the parameters for DescribeInstanceHealth.</p>
newtype DescribeEndPointStateInput = DescribeEndPointStateInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Instances" :: NullOrUndefined (Instances)
  }


-- | <p>Contains the output for DescribeInstanceHealth.</p>
newtype DescribeEndPointStateOutput = DescribeEndPointStateOutput 
  { "InstanceStates" :: NullOrUndefined (InstanceStates)
  }


-- | <p>Contains the parameters for DescribeLoadBalancerAttributes.</p>
newtype DescribeLoadBalancerAttributesInput = DescribeLoadBalancerAttributesInput 
  { "LoadBalancerName" :: (AccessPointName)
  }


-- | <p>Contains the output of DescribeLoadBalancerAttributes.</p>
newtype DescribeLoadBalancerAttributesOutput = DescribeLoadBalancerAttributesOutput 
  { "LoadBalancerAttributes" :: NullOrUndefined (LoadBalancerAttributes)
  }


-- | <p>Contains the parameters for DescribeLoadBalancerPolicies.</p>
newtype DescribeLoadBalancerPoliciesInput = DescribeLoadBalancerPoliciesInput 
  { "LoadBalancerName" :: NullOrUndefined (AccessPointName)
  , "PolicyNames" :: NullOrUndefined (PolicyNames)
  }


-- | <p>Contains the output of DescribeLoadBalancerPolicies.</p>
newtype DescribeLoadBalancerPoliciesOutput = DescribeLoadBalancerPoliciesOutput 
  { "PolicyDescriptions" :: NullOrUndefined (PolicyDescriptions)
  }


-- | <p>Contains the parameters for DescribeLoadBalancerPolicyTypes.</p>
newtype DescribeLoadBalancerPolicyTypesInput = DescribeLoadBalancerPolicyTypesInput 
  { "PolicyTypeNames" :: NullOrUndefined (PolicyTypeNames)
  }


-- | <p>Contains the output of DescribeLoadBalancerPolicyTypes.</p>
newtype DescribeLoadBalancerPolicyTypesOutput = DescribeLoadBalancerPolicyTypesOutput 
  { "PolicyTypeDescriptions" :: NullOrUndefined (PolicyTypeDescriptions)
  }


-- | <p>Contains the parameters for DescribeTags.</p>
newtype DescribeTagsInput = DescribeTagsInput 
  { "LoadBalancerNames" :: (LoadBalancerNamesMax20)
  }


-- | <p>Contains the output for DescribeTags.</p>
newtype DescribeTagsOutput = DescribeTagsOutput 
  { "TagDescriptions" :: NullOrUndefined (TagDescriptions)
  }


newtype Description = Description String


-- | <p>Contains the parameters for DetachLoadBalancerFromSubnets.</p>
newtype DetachLoadBalancerFromSubnetsInput = DetachLoadBalancerFromSubnetsInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Subnets" :: (Subnets)
  }


-- | <p>Contains the output of DetachLoadBalancerFromSubnets.</p>
newtype DetachLoadBalancerFromSubnetsOutput = DetachLoadBalancerFromSubnetsOutput 
  { "Subnets" :: NullOrUndefined (Subnets)
  }


-- | <p>The specified load balancer name already exists for this account.</p>
newtype DuplicateAccessPointNameException = DuplicateAccessPointNameException 
  { 
  }


-- | <p>A listener already exists for the specified load balancer name and port, but with a different instance port, protocol, or SSL certificate.</p>
newtype DuplicateListenerException = DuplicateListenerException 
  { 
  }


-- | <p>A policy with the specified name already exists for this load balancer.</p>
newtype DuplicatePolicyNameException = DuplicatePolicyNameException 
  { 
  }


-- | <p>A tag key was specified more than once.</p>
newtype DuplicateTagKeysException = DuplicateTagKeysException 
  { 
  }


newtype EndPointPort = EndPointPort Int


-- | <p>Information about a health check.</p>
newtype HealthCheck = HealthCheck 
  { "Target" :: (HealthCheckTarget)
  , "Interval" :: (HealthCheckInterval)
  , "Timeout" :: (HealthCheckTimeout)
  , "UnhealthyThreshold" :: (UnhealthyThreshold)
  , "HealthyThreshold" :: (HealthyThreshold)
  }


newtype HealthCheckInterval = HealthCheckInterval Int


newtype HealthCheckTarget = HealthCheckTarget String


newtype HealthCheckTimeout = HealthCheckTimeout Int


newtype HealthyThreshold = HealthyThreshold Int


newtype IdleTimeout = IdleTimeout Int


-- | <p>The ID of an EC2 instance.</p>
newtype Instance = Instance 
  { "InstanceId" :: NullOrUndefined (InstanceId)
  }


newtype InstanceId = InstanceId String


newtype InstancePort = InstancePort Int


-- | <p>Information about the state of an EC2 instance.</p>
newtype InstanceState = InstanceState 
  { "InstanceId" :: NullOrUndefined (InstanceId)
  , "State" :: NullOrUndefined (State)
  , "ReasonCode" :: NullOrUndefined (ReasonCode)
  , "Description" :: NullOrUndefined (Description)
  }


newtype InstanceStates = InstanceStates (Array InstanceState)


newtype Instances = Instances (Array Instance)


-- | <p>The requested configuration change is not valid.</p>
newtype InvalidConfigurationRequestException = InvalidConfigurationRequestException 
  { 
  }


-- | <p>The specified endpoint is not valid.</p>
newtype InvalidEndPointException = InvalidEndPointException 
  { 
  }


-- | <p>The specified value for the schema is not valid. You can only specify a scheme for load balancers in a VPC.</p>
newtype InvalidSchemeException = InvalidSchemeException 
  { 
  }


-- | <p>One or more of the specified security groups do not exist.</p>
newtype InvalidSecurityGroupException = InvalidSecurityGroupException 
  { 
  }


-- | <p>The specified VPC has no associated Internet gateway.</p>
newtype InvalidSubnetException = InvalidSubnetException 
  { 
  }


newtype LBCookieStickinessPolicies = LBCookieStickinessPolicies (Array LBCookieStickinessPolicy)


-- | <p>Information about a policy for duration-based session stickiness.</p>
newtype LBCookieStickinessPolicy = LBCookieStickinessPolicy 
  { "PolicyName" :: NullOrUndefined (PolicyName)
  , "CookieExpirationPeriod" :: NullOrUndefined (CookieExpirationPeriod)
  }


-- | <p>Information about an Elastic Load Balancing resource limit for your AWS account.</p>
newtype Limit = Limit 
  { "Name" :: NullOrUndefined (Name)
  , "Max" :: NullOrUndefined (Max)
  }


newtype Limits = Limits (Array Limit)


-- | <p>Information about a listener.</p> <p>For information about the protocols and the ports supported by Elastic Load Balancing, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html">Listeners for Your Classic Load Balancer</a> in the <i>Classic Load Balancer Guide</i>.</p>
newtype Listener = Listener 
  { "Protocol" :: (Protocol)
  , "LoadBalancerPort" :: (AccessPointPort)
  , "InstanceProtocol" :: NullOrUndefined (Protocol)
  , "InstancePort" :: (InstancePort)
  , "SSLCertificateId" :: NullOrUndefined (SSLCertificateId)
  }


-- | <p>The policies enabled for a listener.</p>
newtype ListenerDescription = ListenerDescription 
  { "Listener" :: NullOrUndefined (Listener)
  , "PolicyNames" :: NullOrUndefined (PolicyNames)
  }


newtype ListenerDescriptions = ListenerDescriptions (Array ListenerDescription)


-- | <p>The load balancer does not have a listener configured at the specified port.</p>
newtype ListenerNotFoundException = ListenerNotFoundException 
  { 
  }


newtype Listeners = Listeners (Array Listener)


-- | <p>The specified load balancer attribute does not exist.</p>
newtype LoadBalancerAttributeNotFoundException = LoadBalancerAttributeNotFoundException 
  { 
  }


-- | <p>The attributes for a load balancer.</p>
newtype LoadBalancerAttributes = LoadBalancerAttributes 
  { "CrossZoneLoadBalancing" :: NullOrUndefined (CrossZoneLoadBalancing)
  , "AccessLog" :: NullOrUndefined (AccessLog)
  , "ConnectionDraining" :: NullOrUndefined (ConnectionDraining)
  , "ConnectionSettings" :: NullOrUndefined (ConnectionSettings)
  , "AdditionalAttributes" :: NullOrUndefined (AdditionalAttributes)
  }


-- | <p>Information about a load balancer.</p>
newtype LoadBalancerDescription = LoadBalancerDescription 
  { "LoadBalancerName" :: NullOrUndefined (AccessPointName)
  , "DNSName" :: NullOrUndefined (DNSName)
  , "CanonicalHostedZoneName" :: NullOrUndefined (DNSName)
  , "CanonicalHostedZoneNameID" :: NullOrUndefined (DNSName)
  , "ListenerDescriptions" :: NullOrUndefined (ListenerDescriptions)
  , "Policies" :: NullOrUndefined (Policies)
  , "BackendServerDescriptions" :: NullOrUndefined (BackendServerDescriptions)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  , "Subnets" :: NullOrUndefined (Subnets)
  , "VPCId" :: NullOrUndefined (VPCId)
  , "Instances" :: NullOrUndefined (Instances)
  , "HealthCheck" :: NullOrUndefined (HealthCheck)
  , "SourceSecurityGroup" :: NullOrUndefined (SourceSecurityGroup)
  , "SecurityGroups" :: NullOrUndefined (SecurityGroups)
  , "CreatedTime" :: NullOrUndefined (CreatedTime)
  , "Scheme" :: NullOrUndefined (LoadBalancerScheme)
  }


newtype LoadBalancerDescriptions = LoadBalancerDescriptions (Array LoadBalancerDescription)


newtype LoadBalancerNames = LoadBalancerNames (Array AccessPointName)


newtype LoadBalancerNamesMax20 = LoadBalancerNamesMax20 (Array AccessPointName)


newtype LoadBalancerScheme = LoadBalancerScheme String


newtype Marker = Marker String


newtype Max = Max String


-- | <p>Contains the parameters for ModifyLoadBalancerAttributes.</p>
newtype ModifyLoadBalancerAttributesInput = ModifyLoadBalancerAttributesInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "LoadBalancerAttributes" :: (LoadBalancerAttributes)
  }


-- | <p>Contains the output of ModifyLoadBalancerAttributes.</p>
newtype ModifyLoadBalancerAttributesOutput = ModifyLoadBalancerAttributesOutput 
  { "LoadBalancerName" :: NullOrUndefined (AccessPointName)
  , "LoadBalancerAttributes" :: NullOrUndefined (LoadBalancerAttributes)
  }


newtype Name = Name String


-- | <p>This operation is not allowed.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { 
  }


newtype PageSize = PageSize Int


-- | <p>The policies for a load balancer.</p>
newtype Policies = Policies 
  { "AppCookieStickinessPolicies" :: NullOrUndefined (AppCookieStickinessPolicies)
  , "LBCookieStickinessPolicies" :: NullOrUndefined (LBCookieStickinessPolicies)
  , "OtherPolicies" :: NullOrUndefined (PolicyNames)
  }


-- | <p>Information about a policy attribute.</p>
newtype PolicyAttribute = PolicyAttribute 
  { "AttributeName" :: NullOrUndefined (AttributeName)
  , "AttributeValue" :: NullOrUndefined (AttributeValue)
  }


-- | <p>Information about a policy attribute.</p>
newtype PolicyAttributeDescription = PolicyAttributeDescription 
  { "AttributeName" :: NullOrUndefined (AttributeName)
  , "AttributeValue" :: NullOrUndefined (AttributeValue)
  }


newtype PolicyAttributeDescriptions = PolicyAttributeDescriptions (Array PolicyAttributeDescription)


-- | <p>Information about a policy attribute type.</p>
newtype PolicyAttributeTypeDescription = PolicyAttributeTypeDescription 
  { "AttributeName" :: NullOrUndefined (AttributeName)
  , "AttributeType" :: NullOrUndefined (AttributeType)
  , "Description" :: NullOrUndefined (Description)
  , "DefaultValue" :: NullOrUndefined (DefaultValue)
  , "Cardinality" :: NullOrUndefined (Cardinality)
  }


newtype PolicyAttributeTypeDescriptions = PolicyAttributeTypeDescriptions (Array PolicyAttributeTypeDescription)


newtype PolicyAttributes = PolicyAttributes (Array PolicyAttribute)


-- | <p>Information about a policy.</p>
newtype PolicyDescription = PolicyDescription 
  { "PolicyName" :: NullOrUndefined (PolicyName)
  , "PolicyTypeName" :: NullOrUndefined (PolicyTypeName)
  , "PolicyAttributeDescriptions" :: NullOrUndefined (PolicyAttributeDescriptions)
  }


newtype PolicyDescriptions = PolicyDescriptions (Array PolicyDescription)


newtype PolicyName = PolicyName String


newtype PolicyNames = PolicyNames (Array PolicyName)


-- | <p>One or more of the specified policies do not exist.</p>
newtype PolicyNotFoundException = PolicyNotFoundException 
  { 
  }


-- | <p>Information about a policy type.</p>
newtype PolicyTypeDescription = PolicyTypeDescription 
  { "PolicyTypeName" :: NullOrUndefined (PolicyTypeName)
  , "Description" :: NullOrUndefined (Description)
  , "PolicyAttributeTypeDescriptions" :: NullOrUndefined (PolicyAttributeTypeDescriptions)
  }


newtype PolicyTypeDescriptions = PolicyTypeDescriptions (Array PolicyTypeDescription)


newtype PolicyTypeName = PolicyTypeName String


newtype PolicyTypeNames = PolicyTypeNames (Array PolicyTypeName)


-- | <p>One or more of the specified policy types do not exist.</p>
newtype PolicyTypeNotFoundException = PolicyTypeNotFoundException 
  { 
  }


newtype Ports = Ports (Array AccessPointPort)


newtype Protocol = Protocol String


newtype ReasonCode = ReasonCode String


-- | <p>Contains the parameters for RegisterInstancesWithLoadBalancer.</p>
newtype RegisterEndPointsInput = RegisterEndPointsInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "Instances" :: (Instances)
  }


-- | <p>Contains the output of RegisterInstancesWithLoadBalancer.</p>
newtype RegisterEndPointsOutput = RegisterEndPointsOutput 
  { "Instances" :: NullOrUndefined (Instances)
  }


-- | <p>Contains the parameters for DisableAvailabilityZonesForLoadBalancer.</p>
newtype RemoveAvailabilityZonesInput = RemoveAvailabilityZonesInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "AvailabilityZones" :: (AvailabilityZones)
  }


-- | <p>Contains the output for DisableAvailabilityZonesForLoadBalancer.</p>
newtype RemoveAvailabilityZonesOutput = RemoveAvailabilityZonesOutput 
  { "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  }


-- | <p>Contains the parameters for RemoveTags.</p>
newtype RemoveTagsInput = RemoveTagsInput 
  { "LoadBalancerNames" :: (LoadBalancerNames)
  , "Tags" :: (TagKeyList)
  }


-- | <p>Contains the output of RemoveTags.</p>
newtype RemoveTagsOutput = RemoveTagsOutput 
  { 
  }


newtype S3BucketName = S3BucketName String


newtype SSLCertificateId = SSLCertificateId String


newtype SecurityGroupId = SecurityGroupId String


newtype SecurityGroupName = SecurityGroupName String


newtype SecurityGroupOwnerAlias = SecurityGroupOwnerAlias String


newtype SecurityGroups = SecurityGroups (Array SecurityGroupId)


-- | <p>Contains the parameters for SetLoadBalancerListenerSSLCertificate.</p>
newtype SetLoadBalancerListenerSSLCertificateInput = SetLoadBalancerListenerSSLCertificateInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "LoadBalancerPort" :: (AccessPointPort)
  , "SSLCertificateId" :: (SSLCertificateId)
  }


-- | <p>Contains the output of SetLoadBalancerListenerSSLCertificate.</p>
newtype SetLoadBalancerListenerSSLCertificateOutput = SetLoadBalancerListenerSSLCertificateOutput 
  { 
  }


-- | <p>Contains the parameters for SetLoadBalancerPoliciesForBackendServer.</p>
newtype SetLoadBalancerPoliciesForBackendServerInput = SetLoadBalancerPoliciesForBackendServerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "InstancePort" :: (EndPointPort)
  , "PolicyNames" :: (PolicyNames)
  }


-- | <p>Contains the output of SetLoadBalancerPoliciesForBackendServer.</p>
newtype SetLoadBalancerPoliciesForBackendServerOutput = SetLoadBalancerPoliciesForBackendServerOutput 
  { 
  }


-- | <p>Contains the parameters for SetLoadBalancePoliciesOfListener.</p>
newtype SetLoadBalancerPoliciesOfListenerInput = SetLoadBalancerPoliciesOfListenerInput 
  { "LoadBalancerName" :: (AccessPointName)
  , "LoadBalancerPort" :: (AccessPointPort)
  , "PolicyNames" :: (PolicyNames)
  }


-- | <p>Contains the output of SetLoadBalancePoliciesOfListener.</p>
newtype SetLoadBalancerPoliciesOfListenerOutput = SetLoadBalancerPoliciesOfListenerOutput 
  { 
  }


-- | <p>Information about a source security group.</p>
newtype SourceSecurityGroup = SourceSecurityGroup 
  { "OwnerAlias" :: NullOrUndefined (SecurityGroupOwnerAlias)
  , "GroupName" :: NullOrUndefined (SecurityGroupName)
  }


newtype State = State String


newtype SubnetId = SubnetId String


-- | <p>One or more of the specified subnets do not exist.</p>
newtype SubnetNotFoundException = SubnetNotFoundException 
  { 
  }


newtype Subnets = Subnets (Array SubnetId)


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }


-- | <p>The tags associated with a load balancer.</p>
newtype TagDescription = TagDescription 
  { "LoadBalancerName" :: NullOrUndefined (AccessPointName)
  , "Tags" :: NullOrUndefined (TagList)
  }


newtype TagDescriptions = TagDescriptions (Array TagDescription)


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKeyOnly)


-- | <p>The key of a tag.</p>
newtype TagKeyOnly = TagKeyOnly 
  { "Key" :: NullOrUndefined (TagKey)
  }


newtype TagList = TagList (Array Tag)


newtype TagValue = TagValue String


-- | <p>The quota for the number of load balancers has been reached.</p>
newtype TooManyAccessPointsException = TooManyAccessPointsException 
  { 
  }


-- | <p>The quota for the number of policies for this load balancer has been reached.</p>
newtype TooManyPoliciesException = TooManyPoliciesException 
  { 
  }


-- | <p>The quota for the number of tags that can be assigned to a load balancer has been reached.</p>
newtype TooManyTagsException = TooManyTagsException 
  { 
  }


newtype UnhealthyThreshold = UnhealthyThreshold Int


-- | <p>The specified protocol or signature version is not supported.</p>
newtype UnsupportedProtocolException = UnsupportedProtocolException 
  { 
  }


newtype VPCId = VPCId String
