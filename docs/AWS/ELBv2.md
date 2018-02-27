## Module AWS.ELBv2

<fullname>Elastic Load Balancing</fullname> <p>A load balancer distributes incoming traffic across targets, such as your EC2 instances. This enables you to increase the availability of your application. The load balancer also monitors the health of its registered targets and ensures that it routes traffic only to healthy targets. You configure your load balancer to accept incoming traffic by specifying one or more listeners, which are configured with a protocol and port number for connections from clients to the load balancer. You configure a target group with a protocol and port number for connections from the load balancer to the targets, and with health check settings to be used when checking the health status of the targets.</p> <p>Elastic Load Balancing supports the following types of load balancers: Application Load Balancers, Network Load Balancers, and Classic Load Balancers.</p> <p>An Application Load Balancer makes routing and load balancing decisions at the application layer (HTTP/HTTPS). A Network Load Balancer makes routing and load balancing decisions at the transport layer (TCP). Both Application Load Balancers and Network Load Balancers can route requests to one or more ports on each EC2 instance or container instance in your virtual private cloud (VPC).</p> <p>A Classic Load Balancer makes routing and load balancing decisions either at the transport layer (TCP/SSL) or the application layer (HTTP/HTTPS), and supports either EC2-Classic or a VPC. For more information, see the <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/userguide/">Elastic Load Balancing User Guide</a>.</p> <p>This reference covers the 2015-12-01 API, which supports Application Load Balancers and Network Load Balancers. The 2012-06-01 API supports Classic Load Balancers.</p> <p>To get started, complete the following tasks:</p> <ol> <li> <p>Create a load balancer using <a>CreateLoadBalancer</a>.</p> </li> <li> <p>Create a target group using <a>CreateTargetGroup</a>.</p> </li> <li> <p>Register targets for the target group using <a>RegisterTargets</a>.</p> </li> <li> <p>Create one or more listeners for your load balancer using <a>CreateListener</a>.</p> </li> </ol> <p>To delete a load balancer and its related resources, complete the following tasks:</p> <ol> <li> <p>Delete the load balancer using <a>DeleteLoadBalancer</a>.</p> </li> <li> <p>Delete the target group using <a>DeleteTargetGroup</a>.</p> </li> </ol> <p>All Elastic Load Balancing operations are idempotent, which means that they complete at most one time. If you repeat an operation, it succeeds.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addListenerCertificates`

``` purescript
addListenerCertificates :: forall eff. AddListenerCertificatesInput -> Aff (err :: RequestError | eff) AddListenerCertificatesOutput
```

<p>Adds the specified certificate to the specified secure listener.</p> <p>If the certificate was already added, the call is successful but the certificate is not added again.</p> <p>To list the certificates for your listener, use <a>DescribeListenerCertificates</a>. To remove certificates from your listener, use <a>RemoveListenerCertificates</a>.</p>

#### `addTags`

``` purescript
addTags :: forall eff. AddTagsInput -> Aff (err :: RequestError | eff) AddTagsOutput
```

<p>Adds the specified tags to the specified Elastic Load Balancing resource. You can tag your Application Load Balancers, Network Load Balancers, and your target groups.</p> <p>Each tag consists of a key and an optional value. If a resource already has a tag with the same key, <code>AddTags</code> updates its value.</p> <p>To list the current tags for your resources, use <a>DescribeTags</a>. To remove tags from your resources, use <a>RemoveTags</a>.</p>

#### `createListener`

``` purescript
createListener :: forall eff. CreateListenerInput -> Aff (err :: RequestError | eff) CreateListenerOutput
```

<p>Creates a listener for the specified Application Load Balancer or Network Load Balancer.</p> <p>To update a listener, use <a>ModifyListener</a>. When you are finished with a listener, you can delete it using <a>DeleteListener</a>. If you are finished with both the listener and the load balancer, you can delete them both using <a>DeleteLoadBalancer</a>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple listeners with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html">Listeners for Your Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-listeners.html">Listeners for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>

#### `createLoadBalancer`

``` purescript
createLoadBalancer :: forall eff. CreateLoadBalancerInput -> Aff (err :: RequestError | eff) CreateLoadBalancerOutput
```

<p>Creates an Application Load Balancer or a Network Load Balancer.</p> <p>When you create a load balancer, you can specify security groups, public subnets, IP address type, and tags. Otherwise, you could do so later using <a>SetSecurityGroups</a>, <a>SetSubnets</a>, <a>SetIpAddressType</a>, and <a>AddTags</a>.</p> <p>To create listeners for your load balancer, use <a>CreateListener</a>. To describe your current load balancers, see <a>DescribeLoadBalancers</a>. When you are finished with a load balancer, you can delete it using <a>DeleteLoadBalancer</a>.</p> <p>For limit information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-limits.html">Limits for Your Application Load Balancer</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-limits.html">Limits for Your Network Load Balancer</a> in the <i>Network Load Balancers Guide</i>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple load balancers with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/application-load-balancers.html">Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> and <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/network-load-balancers.html">Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>

#### `createRule`

``` purescript
createRule :: forall eff. CreateRuleInput -> Aff (err :: RequestError | eff) CreateRuleOutput
```

<p>Creates a rule for the specified listener. The listener must be associated with an Application Load Balancer.</p> <p>Rules are evaluated in priority order, from the lowest value to the highest value. When the condition for a rule is met, the specified action is taken. If no conditions are met, the action for the default rule is taken. For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-listeners.html#listener-rules">Listener Rules</a> in the <i>Application Load Balancers Guide</i>.</p> <p>To view your current rules, use <a>DescribeRules</a>. To update a rule, use <a>ModifyRule</a>. To set the priorities of your rules, use <a>SetRulePriorities</a>. To delete a rule, use <a>DeleteRule</a>.</p>

#### `createTargetGroup`

``` purescript
createTargetGroup :: forall eff. CreateTargetGroupInput -> Aff (err :: RequestError | eff) CreateTargetGroupOutput
```

<p>Creates a target group.</p> <p>To register targets with the target group, use <a>RegisterTargets</a>. To update the health check settings for the target group, use <a>ModifyTargetGroup</a>. To monitor the health of targets in the target group, use <a>DescribeTargetHealth</a>.</p> <p>To route traffic to the targets in a target group, specify the target group in an action using <a>CreateListener</a> or <a>CreateRule</a>.</p> <p>To delete a target group, use <a>DeleteTargetGroup</a>.</p> <p>This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple target groups with the same settings, each call succeeds.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html">Target Groups for Your Application Load Balancers</a> in the <i>Application Load Balancers Guide</i> or <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html">Target Groups for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>

#### `deleteListener`

``` purescript
deleteListener :: forall eff. DeleteListenerInput -> Aff (err :: RequestError | eff) DeleteListenerOutput
```

<p>Deletes the specified listener.</p> <p>Alternatively, your listener is deleted when you delete the load balancer it is attached to using <a>DeleteLoadBalancer</a>.</p>

#### `deleteLoadBalancer`

``` purescript
deleteLoadBalancer :: forall eff. DeleteLoadBalancerInput -> Aff (err :: RequestError | eff) DeleteLoadBalancerOutput
```

<p>Deletes the specified Application Load Balancer or Network Load Balancer and its attached listeners.</p> <p>You can't delete a load balancer if deletion protection is enabled. If the load balancer does not exist or has already been deleted, the call succeeds.</p> <p>Deleting a load balancer does not affect its registered targets. For example, your EC2 instances continue to run and are still registered to their target groups. If you no longer need these EC2 instances, you can stop or terminate them.</p>

#### `deleteRule`

``` purescript
deleteRule :: forall eff. DeleteRuleInput -> Aff (err :: RequestError | eff) DeleteRuleOutput
```

<p>Deletes the specified rule.</p>

#### `deleteTargetGroup`

``` purescript
deleteTargetGroup :: forall eff. DeleteTargetGroupInput -> Aff (err :: RequestError | eff) DeleteTargetGroupOutput
```

<p>Deletes the specified target group.</p> <p>You can delete a target group if it is not referenced by any actions. Deleting a target group also deletes any associated health checks.</p>

#### `deregisterTargets`

``` purescript
deregisterTargets :: forall eff. DeregisterTargetsInput -> Aff (err :: RequestError | eff) DeregisterTargetsOutput
```

<p>Deregisters the specified targets from the specified target group. After the targets are deregistered, they no longer receive traffic from the load balancer.</p>

#### `describeAccountLimits`

``` purescript
describeAccountLimits :: forall eff. DescribeAccountLimitsInput -> Aff (err :: RequestError | eff) DescribeAccountLimitsOutput
```

<p>Describes the current Elastic Load Balancing resource limits for your AWS account.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-limits.html">Limits for Your Application Load Balancers</a> in the <i>Application Load Balancer Guide</i> or <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-limits.html">Limits for Your Network Load Balancers</a> in the <i>Network Load Balancers Guide</i>.</p>

#### `describeListenerCertificates`

``` purescript
describeListenerCertificates :: forall eff. DescribeListenerCertificatesInput -> Aff (err :: RequestError | eff) DescribeListenerCertificatesOutput
```

<p>Describes the certificates for the specified secure listener.</p>

#### `describeListeners`

``` purescript
describeListeners :: forall eff. DescribeListenersInput -> Aff (err :: RequestError | eff) DescribeListenersOutput
```

<p>Describes the specified listeners or the listeners for the specified Application Load Balancer or Network Load Balancer. You must specify either a load balancer or one or more listeners.</p>

#### `describeLoadBalancerAttributes`

``` purescript
describeLoadBalancerAttributes :: forall eff. DescribeLoadBalancerAttributesInput -> Aff (err :: RequestError | eff) DescribeLoadBalancerAttributesOutput
```

<p>Describes the attributes for the specified Application Load Balancer or Network Load Balancer.</p>

#### `describeLoadBalancers`

``` purescript
describeLoadBalancers :: forall eff. DescribeLoadBalancersInput -> Aff (err :: RequestError | eff) DescribeLoadBalancersOutput
```

<p>Describes the specified load balancers or all of your load balancers.</p> <p>To describe the listeners for a load balancer, use <a>DescribeListeners</a>. To describe the attributes for a load balancer, use <a>DescribeLoadBalancerAttributes</a>.</p>

#### `describeRules`

``` purescript
describeRules :: forall eff. DescribeRulesInput -> Aff (err :: RequestError | eff) DescribeRulesOutput
```

<p>Describes the specified rules or the rules for the specified listener. You must specify either a listener or one or more rules.</p>

#### `describeSSLPolicies`

``` purescript
describeSSLPolicies :: forall eff. DescribeSSLPoliciesInput -> Aff (err :: RequestError | eff) DescribeSSLPoliciesOutput
```

<p>Describes the specified policies or all policies used for SSL negotiation.</p> <p>For more information, see <a href="http://docs.aws.amazon.com/elasticloadbalancing/latest/application/create-https-listener.html#describe-ssl-policies">Security Policies</a> in the <i>Application Load Balancers Guide</i>.</p>

#### `describeTags`

``` purescript
describeTags :: forall eff. DescribeTagsInput -> Aff (err :: RequestError | eff) DescribeTagsOutput
```

<p>Describes the tags for the specified resources. You can describe the tags for one or more Application Load Balancers, Network Load Balancers, and target groups.</p>

#### `describeTargetGroupAttributes`

``` purescript
describeTargetGroupAttributes :: forall eff. DescribeTargetGroupAttributesInput -> Aff (err :: RequestError | eff) DescribeTargetGroupAttributesOutput
```

<p>Describes the attributes for the specified target group.</p>

#### `describeTargetGroups`

``` purescript
describeTargetGroups :: forall eff. DescribeTargetGroupsInput -> Aff (err :: RequestError | eff) DescribeTargetGroupsOutput
```

<p>Describes the specified target groups or all of your target groups. By default, all target groups are described. Alternatively, you can specify one of the following to filter the results: the ARN of the load balancer, the names of one or more target groups, or the ARNs of one or more target groups.</p> <p>To describe the targets for a target group, use <a>DescribeTargetHealth</a>. To describe the attributes of a target group, use <a>DescribeTargetGroupAttributes</a>.</p>

#### `describeTargetHealth`

``` purescript
describeTargetHealth :: forall eff. DescribeTargetHealthInput -> Aff (err :: RequestError | eff) DescribeTargetHealthOutput
```

<p>Describes the health of the specified targets or all of your targets.</p>

#### `modifyListener`

``` purescript
modifyListener :: forall eff. ModifyListenerInput -> Aff (err :: RequestError | eff) ModifyListenerOutput
```

<p>Modifies the specified properties of the specified listener.</p> <p>Any properties that you do not specify retain their current values. However, changing the protocol from HTTPS to HTTP removes the security policy and SSL certificate properties. If you change the protocol from HTTP to HTTPS, you must add the security policy and server certificate.</p>

#### `modifyLoadBalancerAttributes`

``` purescript
modifyLoadBalancerAttributes :: forall eff. ModifyLoadBalancerAttributesInput -> Aff (err :: RequestError | eff) ModifyLoadBalancerAttributesOutput
```

<p>Modifies the specified attributes of the specified Application Load Balancer or Network Load Balancer.</p> <p>If any of the specified attributes can't be modified as requested, the call fails. Any existing attributes that you do not modify retain their current values.</p>

#### `modifyRule`

``` purescript
modifyRule :: forall eff. ModifyRuleInput -> Aff (err :: RequestError | eff) ModifyRuleOutput
```

<p>Modifies the specified rule.</p> <p>Any existing properties that you do not modify retain their current values.</p> <p>To modify the default action, use <a>ModifyListener</a>.</p>

#### `modifyTargetGroup`

``` purescript
modifyTargetGroup :: forall eff. ModifyTargetGroupInput -> Aff (err :: RequestError | eff) ModifyTargetGroupOutput
```

<p>Modifies the health checks used when evaluating the health state of the targets in the specified target group.</p> <p>To monitor the health of the targets, use <a>DescribeTargetHealth</a>.</p>

#### `modifyTargetGroupAttributes`

``` purescript
modifyTargetGroupAttributes :: forall eff. ModifyTargetGroupAttributesInput -> Aff (err :: RequestError | eff) ModifyTargetGroupAttributesOutput
```

<p>Modifies the specified attributes of the specified target group.</p>

#### `registerTargets`

``` purescript
registerTargets :: forall eff. RegisterTargetsInput -> Aff (err :: RequestError | eff) RegisterTargetsOutput
```

<p>Registers the specified targets with the specified target group.</p> <p>You can register targets by instance ID or by IP address. If the target is an EC2 instance, it must be in the <code>running</code> state when you register it.</p> <p>By default, the load balancer routes requests to registered targets using the protocol and port for the target group. Alternatively, you can override the port for a target when you register it. You can register each EC2 instance or IP address with the same target group multiple times using different ports.</p> <p>With a Network Load Balancer, you cannot register instances by instance ID if they have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1. You can register instances of these types by IP address.</p> <p>To remove a target from a target group, use <a>DeregisterTargets</a>.</p>

#### `removeListenerCertificates`

``` purescript
removeListenerCertificates :: forall eff. RemoveListenerCertificatesInput -> Aff (err :: RequestError | eff) RemoveListenerCertificatesOutput
```

<p>Removes the specified certificate from the specified secure listener.</p> <p>You can't remove the default certificate for a listener. To replace the default certificate, call <a>ModifyListener</a>.</p> <p>To list the certificates for your listener, use <a>DescribeListenerCertificates</a>.</p>

#### `removeTags`

``` purescript
removeTags :: forall eff. RemoveTagsInput -> Aff (err :: RequestError | eff) RemoveTagsOutput
```

<p>Removes the specified tags from the specified Elastic Load Balancing resource.</p> <p>To list the current tags for your resources, use <a>DescribeTags</a>.</p>

#### `setIpAddressType`

``` purescript
setIpAddressType :: forall eff. SetIpAddressTypeInput -> Aff (err :: RequestError | eff) SetIpAddressTypeOutput
```

<p>Sets the type of IP addresses used by the subnets of the specified Application Load Balancer or Network Load Balancer.</p> <p>Note that Network Load Balancers must use <code>ipv4</code>.</p>

#### `setRulePriorities`

``` purescript
setRulePriorities :: forall eff. SetRulePrioritiesInput -> Aff (err :: RequestError | eff) SetRulePrioritiesOutput
```

<p>Sets the priorities of the specified rules.</p> <p>You can reorder the rules as long as there are no priority conflicts in the new order. Any existing rules that you do not specify retain their current priority.</p>

#### `setSecurityGroups`

``` purescript
setSecurityGroups :: forall eff. SetSecurityGroupsInput -> Aff (err :: RequestError | eff) SetSecurityGroupsOutput
```

<p>Associates the specified security groups with the specified Application Load Balancer. The specified security groups override the previously associated security groups.</p> <p>Note that you can't specify a security group for a Network Load Balancer.</p>

#### `setSubnets`

``` purescript
setSubnets :: forall eff. SetSubnetsInput -> Aff (err :: RequestError | eff) SetSubnetsOutput
```

<p>Enables the Availability Zone for the specified public subnets for the specified Application Load Balancer. The specified subnets replace the previously enabled subnets.</p> <p>Note that you can't change the subnets for a Network Load Balancer.</p>

#### `Action`

``` purescript
newtype Action
  = Action { "Type" :: ActionTypeEnum, "TargetGroupArn" :: TargetGroupArn }
```

<p>Information about an action.</p>

##### Instances
``` purescript
Newtype Action _
```

#### `ActionTypeEnum`

``` purescript
newtype ActionTypeEnum
  = ActionTypeEnum String
```

##### Instances
``` purescript
Newtype ActionTypeEnum _
```

#### `Actions`

``` purescript
newtype Actions
  = Actions (Array Action)
```

##### Instances
``` purescript
Newtype Actions _
```

#### `AddListenerCertificatesInput`

``` purescript
newtype AddListenerCertificatesInput
  = AddListenerCertificatesInput { "ListenerArn" :: ListenerArn, "Certificates" :: CertificateList }
```

##### Instances
``` purescript
Newtype AddListenerCertificatesInput _
```

#### `AddListenerCertificatesOutput`

``` purescript
newtype AddListenerCertificatesOutput
  = AddListenerCertificatesOutput { "Certificates" :: NullOrUndefined (CertificateList) }
```

##### Instances
``` purescript
Newtype AddListenerCertificatesOutput _
```

#### `AddTagsInput`

``` purescript
newtype AddTagsInput
  = AddTagsInput { "ResourceArns" :: ResourceArns, "Tags" :: TagList }
```

##### Instances
``` purescript
Newtype AddTagsInput _
```

#### `AddTagsOutput`

``` purescript
newtype AddTagsOutput
  = AddTagsOutput {  }
```

##### Instances
``` purescript
Newtype AddTagsOutput _
```

#### `AllocationId`

``` purescript
newtype AllocationId
  = AllocationId String
```

##### Instances
``` purescript
Newtype AllocationId _
```

#### `AllocationIdNotFoundException`

``` purescript
newtype AllocationIdNotFoundException
  = AllocationIdNotFoundException {  }
```

<p>The specified allocation ID does not exist.</p>

##### Instances
``` purescript
Newtype AllocationIdNotFoundException _
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone { "ZoneName" :: NullOrUndefined (ZoneName), "SubnetId" :: NullOrUndefined (SubnetId), "LoadBalancerAddresses" :: NullOrUndefined (LoadBalancerAddresses) }
```

<p>Information about an Availability Zone.</p>

##### Instances
``` purescript
Newtype AvailabilityZone _
```

#### `AvailabilityZoneNotSupportedException`

``` purescript
newtype AvailabilityZoneNotSupportedException
  = AvailabilityZoneNotSupportedException {  }
```

<p>The specified Availability Zone is not supported.</p>

##### Instances
``` purescript
Newtype AvailabilityZoneNotSupportedException _
```

#### `AvailabilityZones`

``` purescript
newtype AvailabilityZones
  = AvailabilityZones (Array AvailabilityZone)
```

##### Instances
``` purescript
Newtype AvailabilityZones _
```

#### `CanonicalHostedZoneId`

``` purescript
newtype CanonicalHostedZoneId
  = CanonicalHostedZoneId String
```

##### Instances
``` purescript
Newtype CanonicalHostedZoneId _
```

#### `Certificate`

``` purescript
newtype Certificate
  = Certificate { "CertificateArn" :: NullOrUndefined (CertificateArn), "IsDefault" :: NullOrUndefined (Default) }
```

<p>Information about an SSL server certificate.</p>

##### Instances
``` purescript
Newtype Certificate _
```

#### `CertificateArn`

``` purescript
newtype CertificateArn
  = CertificateArn String
```

##### Instances
``` purescript
Newtype CertificateArn _
```

#### `CertificateList`

``` purescript
newtype CertificateList
  = CertificateList (Array Certificate)
```

##### Instances
``` purescript
Newtype CertificateList _
```

#### `CertificateNotFoundException`

``` purescript
newtype CertificateNotFoundException
  = CertificateNotFoundException {  }
```

<p>The specified certificate does not exist.</p>

##### Instances
``` purescript
Newtype CertificateNotFoundException _
```

#### `Cipher`

``` purescript
newtype Cipher
  = Cipher { "Name" :: NullOrUndefined (CipherName), "Priority" :: NullOrUndefined (CipherPriority) }
```

<p>Information about a cipher used in a policy.</p>

##### Instances
``` purescript
Newtype Cipher _
```

#### `CipherName`

``` purescript
newtype CipherName
  = CipherName String
```

##### Instances
``` purescript
Newtype CipherName _
```

#### `CipherPriority`

``` purescript
newtype CipherPriority
  = CipherPriority Int
```

##### Instances
``` purescript
Newtype CipherPriority _
```

#### `Ciphers`

``` purescript
newtype Ciphers
  = Ciphers (Array Cipher)
```

##### Instances
``` purescript
Newtype Ciphers _
```

#### `ConditionFieldName`

``` purescript
newtype ConditionFieldName
  = ConditionFieldName String
```

##### Instances
``` purescript
Newtype ConditionFieldName _
```

#### `CreateListenerInput`

``` purescript
newtype CreateListenerInput
  = CreateListenerInput { "LoadBalancerArn" :: LoadBalancerArn, "Protocol" :: ProtocolEnum, "Port" :: Port, "SslPolicy" :: NullOrUndefined (SslPolicyName), "Certificates" :: NullOrUndefined (CertificateList), "DefaultActions" :: Actions }
```

##### Instances
``` purescript
Newtype CreateListenerInput _
```

#### `CreateListenerOutput`

``` purescript
newtype CreateListenerOutput
  = CreateListenerOutput { "Listeners" :: NullOrUndefined (Listeners) }
```

##### Instances
``` purescript
Newtype CreateListenerOutput _
```

#### `CreateLoadBalancerInput`

``` purescript
newtype CreateLoadBalancerInput
  = CreateLoadBalancerInput { "Name" :: LoadBalancerName, "Subnets" :: NullOrUndefined (Subnets), "SubnetMappings" :: NullOrUndefined (SubnetMappings), "SecurityGroups" :: NullOrUndefined (SecurityGroups), "Scheme" :: NullOrUndefined (LoadBalancerSchemeEnum), "Tags" :: NullOrUndefined (TagList), "Type" :: NullOrUndefined (LoadBalancerTypeEnum), "IpAddressType" :: NullOrUndefined (IpAddressType) }
```

##### Instances
``` purescript
Newtype CreateLoadBalancerInput _
```

#### `CreateLoadBalancerOutput`

``` purescript
newtype CreateLoadBalancerOutput
  = CreateLoadBalancerOutput { "LoadBalancers" :: NullOrUndefined (LoadBalancers) }
```

##### Instances
``` purescript
Newtype CreateLoadBalancerOutput _
```

#### `CreateRuleInput`

``` purescript
newtype CreateRuleInput
  = CreateRuleInput { "ListenerArn" :: ListenerArn, "Conditions" :: RuleConditionList, "Priority" :: RulePriority, "Actions" :: Actions }
```

##### Instances
``` purescript
Newtype CreateRuleInput _
```

#### `CreateRuleOutput`

``` purescript
newtype CreateRuleOutput
  = CreateRuleOutput { "Rules" :: NullOrUndefined (Rules) }
```

##### Instances
``` purescript
Newtype CreateRuleOutput _
```

#### `CreateTargetGroupInput`

``` purescript
newtype CreateTargetGroupInput
  = CreateTargetGroupInput { "Name" :: TargetGroupName, "Protocol" :: ProtocolEnum, "Port" :: Port, "VpcId" :: VpcId, "HealthCheckProtocol" :: NullOrUndefined (ProtocolEnum), "HealthCheckPort" :: NullOrUndefined (HealthCheckPort), "HealthCheckPath" :: NullOrUndefined (Path), "HealthCheckIntervalSeconds" :: NullOrUndefined (HealthCheckIntervalSeconds), "HealthCheckTimeoutSeconds" :: NullOrUndefined (HealthCheckTimeoutSeconds), "HealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount), "UnhealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount), "Matcher" :: NullOrUndefined (Matcher), "TargetType" :: NullOrUndefined (TargetTypeEnum) }
```

##### Instances
``` purescript
Newtype CreateTargetGroupInput _
```

#### `CreateTargetGroupOutput`

``` purescript
newtype CreateTargetGroupOutput
  = CreateTargetGroupOutput { "TargetGroups" :: NullOrUndefined (TargetGroups) }
```

##### Instances
``` purescript
Newtype CreateTargetGroupOutput _
```

#### `CreatedTime`

``` purescript
newtype CreatedTime
  = CreatedTime Number
```

##### Instances
``` purescript
Newtype CreatedTime _
```

#### `DNSName`

``` purescript
newtype DNSName
  = DNSName String
```

##### Instances
``` purescript
Newtype DNSName _
```

#### `Default`

``` purescript
newtype Default
  = Default Boolean
```

##### Instances
``` purescript
Newtype Default _
```

#### `DeleteListenerInput`

``` purescript
newtype DeleteListenerInput
  = DeleteListenerInput { "ListenerArn" :: ListenerArn }
```

##### Instances
``` purescript
Newtype DeleteListenerInput _
```

#### `DeleteListenerOutput`

``` purescript
newtype DeleteListenerOutput
  = DeleteListenerOutput {  }
```

##### Instances
``` purescript
Newtype DeleteListenerOutput _
```

#### `DeleteLoadBalancerInput`

``` purescript
newtype DeleteLoadBalancerInput
  = DeleteLoadBalancerInput { "LoadBalancerArn" :: LoadBalancerArn }
```

##### Instances
``` purescript
Newtype DeleteLoadBalancerInput _
```

#### `DeleteLoadBalancerOutput`

``` purescript
newtype DeleteLoadBalancerOutput
  = DeleteLoadBalancerOutput {  }
```

##### Instances
``` purescript
Newtype DeleteLoadBalancerOutput _
```

#### `DeleteRuleInput`

``` purescript
newtype DeleteRuleInput
  = DeleteRuleInput { "RuleArn" :: RuleArn }
```

##### Instances
``` purescript
Newtype DeleteRuleInput _
```

#### `DeleteRuleOutput`

``` purescript
newtype DeleteRuleOutput
  = DeleteRuleOutput {  }
```

##### Instances
``` purescript
Newtype DeleteRuleOutput _
```

#### `DeleteTargetGroupInput`

``` purescript
newtype DeleteTargetGroupInput
  = DeleteTargetGroupInput { "TargetGroupArn" :: TargetGroupArn }
```

##### Instances
``` purescript
Newtype DeleteTargetGroupInput _
```

#### `DeleteTargetGroupOutput`

``` purescript
newtype DeleteTargetGroupOutput
  = DeleteTargetGroupOutput {  }
```

##### Instances
``` purescript
Newtype DeleteTargetGroupOutput _
```

#### `DeregisterTargetsInput`

``` purescript
newtype DeregisterTargetsInput
  = DeregisterTargetsInput { "TargetGroupArn" :: TargetGroupArn, "Targets" :: TargetDescriptions }
```

##### Instances
``` purescript
Newtype DeregisterTargetsInput _
```

#### `DeregisterTargetsOutput`

``` purescript
newtype DeregisterTargetsOutput
  = DeregisterTargetsOutput {  }
```

##### Instances
``` purescript
Newtype DeregisterTargetsOutput _
```

#### `DescribeAccountLimitsInput`

``` purescript
newtype DescribeAccountLimitsInput
  = DescribeAccountLimitsInput { "Marker" :: NullOrUndefined (Marker), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype DescribeAccountLimitsInput _
```

#### `DescribeAccountLimitsOutput`

``` purescript
newtype DescribeAccountLimitsOutput
  = DescribeAccountLimitsOutput { "Limits" :: NullOrUndefined (Limits), "NextMarker" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype DescribeAccountLimitsOutput _
```

#### `DescribeListenerCertificatesInput`

``` purescript
newtype DescribeListenerCertificatesInput
  = DescribeListenerCertificatesInput { "ListenerArn" :: ListenerArn, "Marker" :: NullOrUndefined (Marker), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype DescribeListenerCertificatesInput _
```

#### `DescribeListenerCertificatesOutput`

``` purescript
newtype DescribeListenerCertificatesOutput
  = DescribeListenerCertificatesOutput { "Certificates" :: NullOrUndefined (CertificateList), "NextMarker" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype DescribeListenerCertificatesOutput _
```

#### `DescribeListenersInput`

``` purescript
newtype DescribeListenersInput
  = DescribeListenersInput { "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn), "ListenerArns" :: NullOrUndefined (ListenerArns), "Marker" :: NullOrUndefined (Marker), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype DescribeListenersInput _
```

#### `DescribeListenersOutput`

``` purescript
newtype DescribeListenersOutput
  = DescribeListenersOutput { "Listeners" :: NullOrUndefined (Listeners), "NextMarker" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype DescribeListenersOutput _
```

#### `DescribeLoadBalancerAttributesInput`

``` purescript
newtype DescribeLoadBalancerAttributesInput
  = DescribeLoadBalancerAttributesInput { "LoadBalancerArn" :: LoadBalancerArn }
```

##### Instances
``` purescript
Newtype DescribeLoadBalancerAttributesInput _
```

#### `DescribeLoadBalancerAttributesOutput`

``` purescript
newtype DescribeLoadBalancerAttributesOutput
  = DescribeLoadBalancerAttributesOutput { "Attributes" :: NullOrUndefined (LoadBalancerAttributes) }
```

##### Instances
``` purescript
Newtype DescribeLoadBalancerAttributesOutput _
```

#### `DescribeLoadBalancersInput`

``` purescript
newtype DescribeLoadBalancersInput
  = DescribeLoadBalancersInput { "LoadBalancerArns" :: NullOrUndefined (LoadBalancerArns), "Names" :: NullOrUndefined (LoadBalancerNames), "Marker" :: NullOrUndefined (Marker), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype DescribeLoadBalancersInput _
```

#### `DescribeLoadBalancersOutput`

``` purescript
newtype DescribeLoadBalancersOutput
  = DescribeLoadBalancersOutput { "LoadBalancers" :: NullOrUndefined (LoadBalancers), "NextMarker" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype DescribeLoadBalancersOutput _
```

#### `DescribeRulesInput`

``` purescript
newtype DescribeRulesInput
  = DescribeRulesInput { "ListenerArn" :: NullOrUndefined (ListenerArn), "RuleArns" :: NullOrUndefined (RuleArns), "Marker" :: NullOrUndefined (Marker), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype DescribeRulesInput _
```

#### `DescribeRulesOutput`

``` purescript
newtype DescribeRulesOutput
  = DescribeRulesOutput { "Rules" :: NullOrUndefined (Rules), "NextMarker" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype DescribeRulesOutput _
```

#### `DescribeSSLPoliciesInput`

``` purescript
newtype DescribeSSLPoliciesInput
  = DescribeSSLPoliciesInput { "Names" :: NullOrUndefined (SslPolicyNames), "Marker" :: NullOrUndefined (Marker), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype DescribeSSLPoliciesInput _
```

#### `DescribeSSLPoliciesOutput`

``` purescript
newtype DescribeSSLPoliciesOutput
  = DescribeSSLPoliciesOutput { "SslPolicies" :: NullOrUndefined (SslPolicies), "NextMarker" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype DescribeSSLPoliciesOutput _
```

#### `DescribeTagsInput`

``` purescript
newtype DescribeTagsInput
  = DescribeTagsInput { "ResourceArns" :: ResourceArns }
```

##### Instances
``` purescript
Newtype DescribeTagsInput _
```

#### `DescribeTagsOutput`

``` purescript
newtype DescribeTagsOutput
  = DescribeTagsOutput { "TagDescriptions" :: NullOrUndefined (TagDescriptions) }
```

##### Instances
``` purescript
Newtype DescribeTagsOutput _
```

#### `DescribeTargetGroupAttributesInput`

``` purescript
newtype DescribeTargetGroupAttributesInput
  = DescribeTargetGroupAttributesInput { "TargetGroupArn" :: TargetGroupArn }
```

##### Instances
``` purescript
Newtype DescribeTargetGroupAttributesInput _
```

#### `DescribeTargetGroupAttributesOutput`

``` purescript
newtype DescribeTargetGroupAttributesOutput
  = DescribeTargetGroupAttributesOutput { "Attributes" :: NullOrUndefined (TargetGroupAttributes) }
```

##### Instances
``` purescript
Newtype DescribeTargetGroupAttributesOutput _
```

#### `DescribeTargetGroupsInput`

``` purescript
newtype DescribeTargetGroupsInput
  = DescribeTargetGroupsInput { "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn), "TargetGroupArns" :: NullOrUndefined (TargetGroupArns), "Names" :: NullOrUndefined (TargetGroupNames), "Marker" :: NullOrUndefined (Marker), "PageSize" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype DescribeTargetGroupsInput _
```

#### `DescribeTargetGroupsOutput`

``` purescript
newtype DescribeTargetGroupsOutput
  = DescribeTargetGroupsOutput { "TargetGroups" :: NullOrUndefined (TargetGroups), "NextMarker" :: NullOrUndefined (Marker) }
```

##### Instances
``` purescript
Newtype DescribeTargetGroupsOutput _
```

#### `DescribeTargetHealthInput`

``` purescript
newtype DescribeTargetHealthInput
  = DescribeTargetHealthInput { "TargetGroupArn" :: TargetGroupArn, "Targets" :: NullOrUndefined (TargetDescriptions) }
```

##### Instances
``` purescript
Newtype DescribeTargetHealthInput _
```

#### `DescribeTargetHealthOutput`

``` purescript
newtype DescribeTargetHealthOutput
  = DescribeTargetHealthOutput { "TargetHealthDescriptions" :: NullOrUndefined (TargetHealthDescriptions) }
```

##### Instances
``` purescript
Newtype DescribeTargetHealthOutput _
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

##### Instances
``` purescript
Newtype Description _
```

#### `DuplicateListenerException`

``` purescript
newtype DuplicateListenerException
  = DuplicateListenerException {  }
```

<p>A listener with the specified port already exists.</p>

##### Instances
``` purescript
Newtype DuplicateListenerException _
```

#### `DuplicateLoadBalancerNameException`

``` purescript
newtype DuplicateLoadBalancerNameException
  = DuplicateLoadBalancerNameException {  }
```

<p>A load balancer with the specified name already exists.</p>

##### Instances
``` purescript
Newtype DuplicateLoadBalancerNameException _
```

#### `DuplicateTagKeysException`

``` purescript
newtype DuplicateTagKeysException
  = DuplicateTagKeysException {  }
```

<p>A tag key was specified more than once.</p>

##### Instances
``` purescript
Newtype DuplicateTagKeysException _
```

#### `DuplicateTargetGroupNameException`

``` purescript
newtype DuplicateTargetGroupNameException
  = DuplicateTargetGroupNameException {  }
```

<p>A target group with the specified name already exists.</p>

##### Instances
``` purescript
Newtype DuplicateTargetGroupNameException _
```

#### `HealthCheckIntervalSeconds`

``` purescript
newtype HealthCheckIntervalSeconds
  = HealthCheckIntervalSeconds Int
```

##### Instances
``` purescript
Newtype HealthCheckIntervalSeconds _
```

#### `HealthCheckPort`

``` purescript
newtype HealthCheckPort
  = HealthCheckPort String
```

##### Instances
``` purescript
Newtype HealthCheckPort _
```

#### `HealthCheckThresholdCount`

``` purescript
newtype HealthCheckThresholdCount
  = HealthCheckThresholdCount Int
```

##### Instances
``` purescript
Newtype HealthCheckThresholdCount _
```

#### `HealthCheckTimeoutSeconds`

``` purescript
newtype HealthCheckTimeoutSeconds
  = HealthCheckTimeoutSeconds Int
```

##### Instances
``` purescript
Newtype HealthCheckTimeoutSeconds _
```

#### `HealthUnavailableException`

``` purescript
newtype HealthUnavailableException
  = HealthUnavailableException {  }
```

<p>The health of the specified targets could not be retrieved due to an internal error.</p>

##### Instances
``` purescript
Newtype HealthUnavailableException _
```

#### `HttpCode`

``` purescript
newtype HttpCode
  = HttpCode String
```

##### Instances
``` purescript
Newtype HttpCode _
```

#### `IncompatibleProtocolsException`

``` purescript
newtype IncompatibleProtocolsException
  = IncompatibleProtocolsException {  }
```

<p>The specified configuration is not valid with this protocol.</p>

##### Instances
``` purescript
Newtype IncompatibleProtocolsException _
```

#### `InvalidConfigurationRequestException`

``` purescript
newtype InvalidConfigurationRequestException
  = InvalidConfigurationRequestException {  }
```

<p>The requested configuration is not valid.</p>

##### Instances
``` purescript
Newtype InvalidConfigurationRequestException _
```

#### `InvalidSchemeException`

``` purescript
newtype InvalidSchemeException
  = InvalidSchemeException {  }
```

<p>The requested scheme is not valid.</p>

##### Instances
``` purescript
Newtype InvalidSchemeException _
```

#### `InvalidSecurityGroupException`

``` purescript
newtype InvalidSecurityGroupException
  = InvalidSecurityGroupException {  }
```

<p>The specified security group does not exist.</p>

##### Instances
``` purescript
Newtype InvalidSecurityGroupException _
```

#### `InvalidSubnetException`

``` purescript
newtype InvalidSubnetException
  = InvalidSubnetException {  }
```

<p>The specified subnet is out of available addresses.</p>

##### Instances
``` purescript
Newtype InvalidSubnetException _
```

#### `InvalidTargetException`

``` purescript
newtype InvalidTargetException
  = InvalidTargetException {  }
```

<p>The specified target does not exist, is not in the same VPC as the target group, or has an unsupported instance type.</p>

##### Instances
``` purescript
Newtype InvalidTargetException _
```

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

##### Instances
``` purescript
Newtype IpAddress _
```

#### `IpAddressType`

``` purescript
newtype IpAddressType
  = IpAddressType String
```

##### Instances
``` purescript
Newtype IpAddressType _
```

#### `IsDefault`

``` purescript
newtype IsDefault
  = IsDefault Boolean
```

##### Instances
``` purescript
Newtype IsDefault _
```

#### `Limit`

``` purescript
newtype Limit
  = Limit { "Name" :: NullOrUndefined (Name), "Max" :: NullOrUndefined (Max) }
```

<p>Information about an Elastic Load Balancing resource limit for your AWS account.</p>

##### Instances
``` purescript
Newtype Limit _
```

#### `Limits`

``` purescript
newtype Limits
  = Limits (Array Limit)
```

##### Instances
``` purescript
Newtype Limits _
```

#### `ListOfString`

``` purescript
newtype ListOfString
  = ListOfString (Array StringValue)
```

##### Instances
``` purescript
Newtype ListOfString _
```

#### `Listener`

``` purescript
newtype Listener
  = Listener { "ListenerArn" :: NullOrUndefined (ListenerArn), "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn), "Port" :: NullOrUndefined (Port), "Protocol" :: NullOrUndefined (ProtocolEnum), "Certificates" :: NullOrUndefined (CertificateList), "SslPolicy" :: NullOrUndefined (SslPolicyName), "DefaultActions" :: NullOrUndefined (Actions) }
```

<p>Information about a listener.</p>

##### Instances
``` purescript
Newtype Listener _
```

#### `ListenerArn`

``` purescript
newtype ListenerArn
  = ListenerArn String
```

##### Instances
``` purescript
Newtype ListenerArn _
```

#### `ListenerArns`

``` purescript
newtype ListenerArns
  = ListenerArns (Array ListenerArn)
```

##### Instances
``` purescript
Newtype ListenerArns _
```

#### `ListenerNotFoundException`

``` purescript
newtype ListenerNotFoundException
  = ListenerNotFoundException {  }
```

<p>The specified listener does not exist.</p>

##### Instances
``` purescript
Newtype ListenerNotFoundException _
```

#### `Listeners`

``` purescript
newtype Listeners
  = Listeners (Array Listener)
```

##### Instances
``` purescript
Newtype Listeners _
```

#### `LoadBalancer`

``` purescript
newtype LoadBalancer
  = LoadBalancer { "LoadBalancerArn" :: NullOrUndefined (LoadBalancerArn), "DNSName" :: NullOrUndefined (DNSName), "CanonicalHostedZoneId" :: NullOrUndefined (CanonicalHostedZoneId), "CreatedTime" :: NullOrUndefined (CreatedTime), "LoadBalancerName" :: NullOrUndefined (LoadBalancerName), "Scheme" :: NullOrUndefined (LoadBalancerSchemeEnum), "VpcId" :: NullOrUndefined (VpcId), "State" :: NullOrUndefined (LoadBalancerState), "Type" :: NullOrUndefined (LoadBalancerTypeEnum), "AvailabilityZones" :: NullOrUndefined (AvailabilityZones), "SecurityGroups" :: NullOrUndefined (SecurityGroups), "IpAddressType" :: NullOrUndefined (IpAddressType) }
```

<p>Information about a load balancer.</p>

##### Instances
``` purescript
Newtype LoadBalancer _
```

#### `LoadBalancerAddress`

``` purescript
newtype LoadBalancerAddress
  = LoadBalancerAddress { "IpAddress" :: NullOrUndefined (IpAddress), "AllocationId" :: NullOrUndefined (AllocationId) }
```

<p>Information about a static IP address for a load balancer.</p>

##### Instances
``` purescript
Newtype LoadBalancerAddress _
```

#### `LoadBalancerAddresses`

``` purescript
newtype LoadBalancerAddresses
  = LoadBalancerAddresses (Array LoadBalancerAddress)
```

##### Instances
``` purescript
Newtype LoadBalancerAddresses _
```

#### `LoadBalancerArn`

``` purescript
newtype LoadBalancerArn
  = LoadBalancerArn String
```

##### Instances
``` purescript
Newtype LoadBalancerArn _
```

#### `LoadBalancerArns`

``` purescript
newtype LoadBalancerArns
  = LoadBalancerArns (Array LoadBalancerArn)
```

##### Instances
``` purescript
Newtype LoadBalancerArns _
```

#### `LoadBalancerAttribute`

``` purescript
newtype LoadBalancerAttribute
  = LoadBalancerAttribute { "Key" :: NullOrUndefined (LoadBalancerAttributeKey), "Value" :: NullOrUndefined (LoadBalancerAttributeValue) }
```

<p>Information about a load balancer attribute.</p>

##### Instances
``` purescript
Newtype LoadBalancerAttribute _
```

#### `LoadBalancerAttributeKey`

``` purescript
newtype LoadBalancerAttributeKey
  = LoadBalancerAttributeKey String
```

##### Instances
``` purescript
Newtype LoadBalancerAttributeKey _
```

#### `LoadBalancerAttributeValue`

``` purescript
newtype LoadBalancerAttributeValue
  = LoadBalancerAttributeValue String
```

##### Instances
``` purescript
Newtype LoadBalancerAttributeValue _
```

#### `LoadBalancerAttributes`

``` purescript
newtype LoadBalancerAttributes
  = LoadBalancerAttributes (Array LoadBalancerAttribute)
```

##### Instances
``` purescript
Newtype LoadBalancerAttributes _
```

#### `LoadBalancerName`

``` purescript
newtype LoadBalancerName
  = LoadBalancerName String
```

##### Instances
``` purescript
Newtype LoadBalancerName _
```

#### `LoadBalancerNames`

``` purescript
newtype LoadBalancerNames
  = LoadBalancerNames (Array LoadBalancerName)
```

##### Instances
``` purescript
Newtype LoadBalancerNames _
```

#### `LoadBalancerNotFoundException`

``` purescript
newtype LoadBalancerNotFoundException
  = LoadBalancerNotFoundException {  }
```

<p>The specified load balancer does not exist.</p>

##### Instances
``` purescript
Newtype LoadBalancerNotFoundException _
```

#### `LoadBalancerSchemeEnum`

``` purescript
newtype LoadBalancerSchemeEnum
  = LoadBalancerSchemeEnum String
```

##### Instances
``` purescript
Newtype LoadBalancerSchemeEnum _
```

#### `LoadBalancerState`

``` purescript
newtype LoadBalancerState
  = LoadBalancerState { "Code" :: NullOrUndefined (LoadBalancerStateEnum), "Reason" :: NullOrUndefined (StateReason) }
```

<p>Information about the state of the load balancer.</p>

##### Instances
``` purescript
Newtype LoadBalancerState _
```

#### `LoadBalancerStateEnum`

``` purescript
newtype LoadBalancerStateEnum
  = LoadBalancerStateEnum String
```

##### Instances
``` purescript
Newtype LoadBalancerStateEnum _
```

#### `LoadBalancerTypeEnum`

``` purescript
newtype LoadBalancerTypeEnum
  = LoadBalancerTypeEnum String
```

##### Instances
``` purescript
Newtype LoadBalancerTypeEnum _
```

#### `LoadBalancers`

``` purescript
newtype LoadBalancers
  = LoadBalancers (Array LoadBalancer)
```

##### Instances
``` purescript
Newtype LoadBalancers _
```

#### `Marker`

``` purescript
newtype Marker
  = Marker String
```

##### Instances
``` purescript
Newtype Marker _
```

#### `Matcher`

``` purescript
newtype Matcher
  = Matcher { "HttpCode" :: HttpCode }
```

<p>Information to use when checking for a successful response from a target.</p>

##### Instances
``` purescript
Newtype Matcher _
```

#### `Max`

``` purescript
newtype Max
  = Max String
```

##### Instances
``` purescript
Newtype Max _
```

#### `ModifyListenerInput`

``` purescript
newtype ModifyListenerInput
  = ModifyListenerInput { "ListenerArn" :: ListenerArn, "Port" :: NullOrUndefined (Port), "Protocol" :: NullOrUndefined (ProtocolEnum), "SslPolicy" :: NullOrUndefined (SslPolicyName), "Certificates" :: NullOrUndefined (CertificateList), "DefaultActions" :: NullOrUndefined (Actions) }
```

##### Instances
``` purescript
Newtype ModifyListenerInput _
```

#### `ModifyListenerOutput`

``` purescript
newtype ModifyListenerOutput
  = ModifyListenerOutput { "Listeners" :: NullOrUndefined (Listeners) }
```

##### Instances
``` purescript
Newtype ModifyListenerOutput _
```

#### `ModifyLoadBalancerAttributesInput`

``` purescript
newtype ModifyLoadBalancerAttributesInput
  = ModifyLoadBalancerAttributesInput { "LoadBalancerArn" :: LoadBalancerArn, "Attributes" :: LoadBalancerAttributes }
```

##### Instances
``` purescript
Newtype ModifyLoadBalancerAttributesInput _
```

#### `ModifyLoadBalancerAttributesOutput`

``` purescript
newtype ModifyLoadBalancerAttributesOutput
  = ModifyLoadBalancerAttributesOutput { "Attributes" :: NullOrUndefined (LoadBalancerAttributes) }
```

##### Instances
``` purescript
Newtype ModifyLoadBalancerAttributesOutput _
```

#### `ModifyRuleInput`

``` purescript
newtype ModifyRuleInput
  = ModifyRuleInput { "RuleArn" :: RuleArn, "Conditions" :: NullOrUndefined (RuleConditionList), "Actions" :: NullOrUndefined (Actions) }
```

##### Instances
``` purescript
Newtype ModifyRuleInput _
```

#### `ModifyRuleOutput`

``` purescript
newtype ModifyRuleOutput
  = ModifyRuleOutput { "Rules" :: NullOrUndefined (Rules) }
```

##### Instances
``` purescript
Newtype ModifyRuleOutput _
```

#### `ModifyTargetGroupAttributesInput`

``` purescript
newtype ModifyTargetGroupAttributesInput
  = ModifyTargetGroupAttributesInput { "TargetGroupArn" :: TargetGroupArn, "Attributes" :: TargetGroupAttributes }
```

##### Instances
``` purescript
Newtype ModifyTargetGroupAttributesInput _
```

#### `ModifyTargetGroupAttributesOutput`

``` purescript
newtype ModifyTargetGroupAttributesOutput
  = ModifyTargetGroupAttributesOutput { "Attributes" :: NullOrUndefined (TargetGroupAttributes) }
```

##### Instances
``` purescript
Newtype ModifyTargetGroupAttributesOutput _
```

#### `ModifyTargetGroupInput`

``` purescript
newtype ModifyTargetGroupInput
  = ModifyTargetGroupInput { "TargetGroupArn" :: TargetGroupArn, "HealthCheckProtocol" :: NullOrUndefined (ProtocolEnum), "HealthCheckPort" :: NullOrUndefined (HealthCheckPort), "HealthCheckPath" :: NullOrUndefined (Path), "HealthCheckIntervalSeconds" :: NullOrUndefined (HealthCheckIntervalSeconds), "HealthCheckTimeoutSeconds" :: NullOrUndefined (HealthCheckTimeoutSeconds), "HealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount), "UnhealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount), "Matcher" :: NullOrUndefined (Matcher) }
```

##### Instances
``` purescript
Newtype ModifyTargetGroupInput _
```

#### `ModifyTargetGroupOutput`

``` purescript
newtype ModifyTargetGroupOutput
  = ModifyTargetGroupOutput { "TargetGroups" :: NullOrUndefined (TargetGroups) }
```

##### Instances
``` purescript
Newtype ModifyTargetGroupOutput _
```

#### `Name`

``` purescript
newtype Name
  = Name String
```

##### Instances
``` purescript
Newtype Name _
```

#### `OperationNotPermittedException`

``` purescript
newtype OperationNotPermittedException
  = OperationNotPermittedException {  }
```

<p>This operation is not allowed.</p>

##### Instances
``` purescript
Newtype OperationNotPermittedException _
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

##### Instances
``` purescript
Newtype PageSize _
```

#### `Path`

``` purescript
newtype Path
  = Path String
```

##### Instances
``` purescript
Newtype Path _
```

#### `Port`

``` purescript
newtype Port
  = Port Int
```

##### Instances
``` purescript
Newtype Port _
```

#### `PriorityInUseException`

``` purescript
newtype PriorityInUseException
  = PriorityInUseException {  }
```

<p>The specified priority is in use.</p>

##### Instances
``` purescript
Newtype PriorityInUseException _
```

#### `ProtocolEnum`

``` purescript
newtype ProtocolEnum
  = ProtocolEnum String
```

##### Instances
``` purescript
Newtype ProtocolEnum _
```

#### `RegisterTargetsInput`

``` purescript
newtype RegisterTargetsInput
  = RegisterTargetsInput { "TargetGroupArn" :: TargetGroupArn, "Targets" :: TargetDescriptions }
```

##### Instances
``` purescript
Newtype RegisterTargetsInput _
```

#### `RegisterTargetsOutput`

``` purescript
newtype RegisterTargetsOutput
  = RegisterTargetsOutput {  }
```

##### Instances
``` purescript
Newtype RegisterTargetsOutput _
```

#### `RemoveListenerCertificatesInput`

``` purescript
newtype RemoveListenerCertificatesInput
  = RemoveListenerCertificatesInput { "ListenerArn" :: ListenerArn, "Certificates" :: CertificateList }
```

##### Instances
``` purescript
Newtype RemoveListenerCertificatesInput _
```

#### `RemoveListenerCertificatesOutput`

``` purescript
newtype RemoveListenerCertificatesOutput
  = RemoveListenerCertificatesOutput {  }
```

##### Instances
``` purescript
Newtype RemoveListenerCertificatesOutput _
```

#### `RemoveTagsInput`

``` purescript
newtype RemoveTagsInput
  = RemoveTagsInput { "ResourceArns" :: ResourceArns, "TagKeys" :: TagKeys }
```

##### Instances
``` purescript
Newtype RemoveTagsInput _
```

#### `RemoveTagsOutput`

``` purescript
newtype RemoveTagsOutput
  = RemoveTagsOutput {  }
```

##### Instances
``` purescript
Newtype RemoveTagsOutput _
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

#### `ResourceArns`

``` purescript
newtype ResourceArns
  = ResourceArns (Array ResourceArn)
```

##### Instances
``` purescript
Newtype ResourceArns _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException {  }
```

<p>A specified resource is in use.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `Rule`

``` purescript
newtype Rule
  = Rule { "RuleArn" :: NullOrUndefined (RuleArn), "Priority" :: NullOrUndefined (String), "Conditions" :: NullOrUndefined (RuleConditionList), "Actions" :: NullOrUndefined (Actions), "IsDefault" :: NullOrUndefined (IsDefault) }
```

<p>Information about a rule.</p>

##### Instances
``` purescript
Newtype Rule _
```

#### `RuleArn`

``` purescript
newtype RuleArn
  = RuleArn String
```

##### Instances
``` purescript
Newtype RuleArn _
```

#### `RuleArns`

``` purescript
newtype RuleArns
  = RuleArns (Array RuleArn)
```

##### Instances
``` purescript
Newtype RuleArns _
```

#### `RuleCondition`

``` purescript
newtype RuleCondition
  = RuleCondition { "Field" :: NullOrUndefined (ConditionFieldName), "Values" :: NullOrUndefined (ListOfString) }
```

<p>Information about a condition for a rule.</p>

##### Instances
``` purescript
Newtype RuleCondition _
```

#### `RuleConditionList`

``` purescript
newtype RuleConditionList
  = RuleConditionList (Array RuleCondition)
```

##### Instances
``` purescript
Newtype RuleConditionList _
```

#### `RuleNotFoundException`

``` purescript
newtype RuleNotFoundException
  = RuleNotFoundException {  }
```

<p>The specified rule does not exist.</p>

##### Instances
``` purescript
Newtype RuleNotFoundException _
```

#### `RulePriority`

``` purescript
newtype RulePriority
  = RulePriority Int
```

##### Instances
``` purescript
Newtype RulePriority _
```

#### `RulePriorityList`

``` purescript
newtype RulePriorityList
  = RulePriorityList (Array RulePriorityPair)
```

##### Instances
``` purescript
Newtype RulePriorityList _
```

#### `RulePriorityPair`

``` purescript
newtype RulePriorityPair
  = RulePriorityPair { "RuleArn" :: NullOrUndefined (RuleArn), "Priority" :: NullOrUndefined (RulePriority) }
```

<p>Information about the priorities for the rules for a listener.</p>

##### Instances
``` purescript
Newtype RulePriorityPair _
```

#### `Rules`

``` purescript
newtype Rules
  = Rules (Array Rule)
```

##### Instances
``` purescript
Newtype Rules _
```

#### `SSLPolicyNotFoundException`

``` purescript
newtype SSLPolicyNotFoundException
  = SSLPolicyNotFoundException {  }
```

<p>The specified SSL policy does not exist.</p>

##### Instances
``` purescript
Newtype SSLPolicyNotFoundException _
```

#### `SecurityGroupId`

``` purescript
newtype SecurityGroupId
  = SecurityGroupId String
```

##### Instances
``` purescript
Newtype SecurityGroupId _
```

#### `SecurityGroups`

``` purescript
newtype SecurityGroups
  = SecurityGroups (Array SecurityGroupId)
```

##### Instances
``` purescript
Newtype SecurityGroups _
```

#### `SetIpAddressTypeInput`

``` purescript
newtype SetIpAddressTypeInput
  = SetIpAddressTypeInput { "LoadBalancerArn" :: LoadBalancerArn, "IpAddressType" :: IpAddressType }
```

##### Instances
``` purescript
Newtype SetIpAddressTypeInput _
```

#### `SetIpAddressTypeOutput`

``` purescript
newtype SetIpAddressTypeOutput
  = SetIpAddressTypeOutput { "IpAddressType" :: NullOrUndefined (IpAddressType) }
```

##### Instances
``` purescript
Newtype SetIpAddressTypeOutput _
```

#### `SetRulePrioritiesInput`

``` purescript
newtype SetRulePrioritiesInput
  = SetRulePrioritiesInput { "RulePriorities" :: RulePriorityList }
```

##### Instances
``` purescript
Newtype SetRulePrioritiesInput _
```

#### `SetRulePrioritiesOutput`

``` purescript
newtype SetRulePrioritiesOutput
  = SetRulePrioritiesOutput { "Rules" :: NullOrUndefined (Rules) }
```

##### Instances
``` purescript
Newtype SetRulePrioritiesOutput _
```

#### `SetSecurityGroupsInput`

``` purescript
newtype SetSecurityGroupsInput
  = SetSecurityGroupsInput { "LoadBalancerArn" :: LoadBalancerArn, "SecurityGroups" :: SecurityGroups }
```

##### Instances
``` purescript
Newtype SetSecurityGroupsInput _
```

#### `SetSecurityGroupsOutput`

``` purescript
newtype SetSecurityGroupsOutput
  = SetSecurityGroupsOutput { "SecurityGroupIds" :: NullOrUndefined (SecurityGroups) }
```

##### Instances
``` purescript
Newtype SetSecurityGroupsOutput _
```

#### `SetSubnetsInput`

``` purescript
newtype SetSubnetsInput
  = SetSubnetsInput { "LoadBalancerArn" :: LoadBalancerArn, "Subnets" :: Subnets, "SubnetMappings" :: NullOrUndefined (SubnetMappings) }
```

##### Instances
``` purescript
Newtype SetSubnetsInput _
```

#### `SetSubnetsOutput`

``` purescript
newtype SetSubnetsOutput
  = SetSubnetsOutput { "AvailabilityZones" :: NullOrUndefined (AvailabilityZones) }
```

##### Instances
``` purescript
Newtype SetSubnetsOutput _
```

#### `SslPolicies`

``` purescript
newtype SslPolicies
  = SslPolicies (Array SslPolicy)
```

##### Instances
``` purescript
Newtype SslPolicies _
```

#### `SslPolicy`

``` purescript
newtype SslPolicy
  = SslPolicy { "SslProtocols" :: NullOrUndefined (SslProtocols), "Ciphers" :: NullOrUndefined (Ciphers), "Name" :: NullOrUndefined (SslPolicyName) }
```

<p>Information about a policy used for SSL negotiation.</p>

##### Instances
``` purescript
Newtype SslPolicy _
```

#### `SslPolicyName`

``` purescript
newtype SslPolicyName
  = SslPolicyName String
```

##### Instances
``` purescript
Newtype SslPolicyName _
```

#### `SslPolicyNames`

``` purescript
newtype SslPolicyNames
  = SslPolicyNames (Array SslPolicyName)
```

##### Instances
``` purescript
Newtype SslPolicyNames _
```

#### `SslProtocol`

``` purescript
newtype SslProtocol
  = SslProtocol String
```

##### Instances
``` purescript
Newtype SslProtocol _
```

#### `SslProtocols`

``` purescript
newtype SslProtocols
  = SslProtocols (Array SslProtocol)
```

##### Instances
``` purescript
Newtype SslProtocols _
```

#### `StateReason`

``` purescript
newtype StateReason
  = StateReason String
```

##### Instances
``` purescript
Newtype StateReason _
```

#### `StringValue`

``` purescript
newtype StringValue
  = StringValue String
```

##### Instances
``` purescript
Newtype StringValue _
```

#### `SubnetId`

``` purescript
newtype SubnetId
  = SubnetId String
```

##### Instances
``` purescript
Newtype SubnetId _
```

#### `SubnetMapping`

``` purescript
newtype SubnetMapping
  = SubnetMapping { "SubnetId" :: NullOrUndefined (SubnetId), "AllocationId" :: NullOrUndefined (AllocationId) }
```

<p>Information about a subnet mapping.</p>

##### Instances
``` purescript
Newtype SubnetMapping _
```

#### `SubnetMappings`

``` purescript
newtype SubnetMappings
  = SubnetMappings (Array SubnetMapping)
```

##### Instances
``` purescript
Newtype SubnetMappings _
```

#### `SubnetNotFoundException`

``` purescript
newtype SubnetNotFoundException
  = SubnetNotFoundException {  }
```

<p>The specified subnet does not exist.</p>

##### Instances
``` purescript
Newtype SubnetNotFoundException _
```

#### `Subnets`

``` purescript
newtype Subnets
  = Subnets (Array SubnetId)
```

##### Instances
``` purescript
Newtype Subnets _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: NullOrUndefined (TagValue) }
```

<p>Information about a tag.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagDescription`

``` purescript
newtype TagDescription
  = TagDescription { "ResourceArn" :: NullOrUndefined (ResourceArn), "Tags" :: NullOrUndefined (TagList) }
```

<p>The tags associated with a resource.</p>

##### Instances
``` purescript
Newtype TagDescription _
```

#### `TagDescriptions`

``` purescript
newtype TagDescriptions
  = TagDescriptions (Array TagDescription)
```

##### Instances
``` purescript
Newtype TagDescriptions _
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

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
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

#### `TargetDescription`

``` purescript
newtype TargetDescription
  = TargetDescription { "Id" :: TargetId, "Port" :: NullOrUndefined (Port), "AvailabilityZone" :: NullOrUndefined (ZoneName) }
```

<p>Information about a target.</p>

##### Instances
``` purescript
Newtype TargetDescription _
```

#### `TargetDescriptions`

``` purescript
newtype TargetDescriptions
  = TargetDescriptions (Array TargetDescription)
```

##### Instances
``` purescript
Newtype TargetDescriptions _
```

#### `TargetGroup`

``` purescript
newtype TargetGroup
  = TargetGroup { "TargetGroupArn" :: NullOrUndefined (TargetGroupArn), "TargetGroupName" :: NullOrUndefined (TargetGroupName), "Protocol" :: NullOrUndefined (ProtocolEnum), "Port" :: NullOrUndefined (Port), "VpcId" :: NullOrUndefined (VpcId), "HealthCheckProtocol" :: NullOrUndefined (ProtocolEnum), "HealthCheckPort" :: NullOrUndefined (HealthCheckPort), "HealthCheckIntervalSeconds" :: NullOrUndefined (HealthCheckIntervalSeconds), "HealthCheckTimeoutSeconds" :: NullOrUndefined (HealthCheckTimeoutSeconds), "HealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount), "UnhealthyThresholdCount" :: NullOrUndefined (HealthCheckThresholdCount), "HealthCheckPath" :: NullOrUndefined (Path), "Matcher" :: NullOrUndefined (Matcher), "LoadBalancerArns" :: NullOrUndefined (LoadBalancerArns), "TargetType" :: NullOrUndefined (TargetTypeEnum) }
```

<p>Information about a target group.</p>

##### Instances
``` purescript
Newtype TargetGroup _
```

#### `TargetGroupArn`

``` purescript
newtype TargetGroupArn
  = TargetGroupArn String
```

##### Instances
``` purescript
Newtype TargetGroupArn _
```

#### `TargetGroupArns`

``` purescript
newtype TargetGroupArns
  = TargetGroupArns (Array TargetGroupArn)
```

##### Instances
``` purescript
Newtype TargetGroupArns _
```

#### `TargetGroupAssociationLimitException`

``` purescript
newtype TargetGroupAssociationLimitException
  = TargetGroupAssociationLimitException {  }
```

<p>You've reached the limit on the number of load balancers per target group.</p>

##### Instances
``` purescript
Newtype TargetGroupAssociationLimitException _
```

#### `TargetGroupAttribute`

``` purescript
newtype TargetGroupAttribute
  = TargetGroupAttribute { "Key" :: NullOrUndefined (TargetGroupAttributeKey), "Value" :: NullOrUndefined (TargetGroupAttributeValue) }
```

<p>Information about a target group attribute.</p>

##### Instances
``` purescript
Newtype TargetGroupAttribute _
```

#### `TargetGroupAttributeKey`

``` purescript
newtype TargetGroupAttributeKey
  = TargetGroupAttributeKey String
```

##### Instances
``` purescript
Newtype TargetGroupAttributeKey _
```

#### `TargetGroupAttributeValue`

``` purescript
newtype TargetGroupAttributeValue
  = TargetGroupAttributeValue String
```

##### Instances
``` purescript
Newtype TargetGroupAttributeValue _
```

#### `TargetGroupAttributes`

``` purescript
newtype TargetGroupAttributes
  = TargetGroupAttributes (Array TargetGroupAttribute)
```

##### Instances
``` purescript
Newtype TargetGroupAttributes _
```

#### `TargetGroupName`

``` purescript
newtype TargetGroupName
  = TargetGroupName String
```

##### Instances
``` purescript
Newtype TargetGroupName _
```

#### `TargetGroupNames`

``` purescript
newtype TargetGroupNames
  = TargetGroupNames (Array TargetGroupName)
```

##### Instances
``` purescript
Newtype TargetGroupNames _
```

#### `TargetGroupNotFoundException`

``` purescript
newtype TargetGroupNotFoundException
  = TargetGroupNotFoundException {  }
```

<p>The specified target group does not exist.</p>

##### Instances
``` purescript
Newtype TargetGroupNotFoundException _
```

#### `TargetGroups`

``` purescript
newtype TargetGroups
  = TargetGroups (Array TargetGroup)
```

##### Instances
``` purescript
Newtype TargetGroups _
```

#### `TargetHealth`

``` purescript
newtype TargetHealth
  = TargetHealth { "State" :: NullOrUndefined (TargetHealthStateEnum), "Reason" :: NullOrUndefined (TargetHealthReasonEnum), "Description" :: NullOrUndefined (Description) }
```

<p>Information about the current health of a target.</p>

##### Instances
``` purescript
Newtype TargetHealth _
```

#### `TargetHealthDescription`

``` purescript
newtype TargetHealthDescription
  = TargetHealthDescription { "Target" :: NullOrUndefined (TargetDescription), "HealthCheckPort" :: NullOrUndefined (HealthCheckPort), "TargetHealth" :: NullOrUndefined (TargetHealth) }
```

<p>Information about the health of a target.</p>

##### Instances
``` purescript
Newtype TargetHealthDescription _
```

#### `TargetHealthDescriptions`

``` purescript
newtype TargetHealthDescriptions
  = TargetHealthDescriptions (Array TargetHealthDescription)
```

##### Instances
``` purescript
Newtype TargetHealthDescriptions _
```

#### `TargetHealthReasonEnum`

``` purescript
newtype TargetHealthReasonEnum
  = TargetHealthReasonEnum String
```

##### Instances
``` purescript
Newtype TargetHealthReasonEnum _
```

#### `TargetHealthStateEnum`

``` purescript
newtype TargetHealthStateEnum
  = TargetHealthStateEnum String
```

##### Instances
``` purescript
Newtype TargetHealthStateEnum _
```

#### `TargetId`

``` purescript
newtype TargetId
  = TargetId String
```

##### Instances
``` purescript
Newtype TargetId _
```

#### `TargetTypeEnum`

``` purescript
newtype TargetTypeEnum
  = TargetTypeEnum String
```

##### Instances
``` purescript
Newtype TargetTypeEnum _
```

#### `TooManyCertificatesException`

``` purescript
newtype TooManyCertificatesException
  = TooManyCertificatesException {  }
```

<p>You've reached the limit on the number of certificates per load balancer.</p>

##### Instances
``` purescript
Newtype TooManyCertificatesException _
```

#### `TooManyListenersException`

``` purescript
newtype TooManyListenersException
  = TooManyListenersException {  }
```

<p>You've reached the limit on the number of listeners per load balancer.</p>

##### Instances
``` purescript
Newtype TooManyListenersException _
```

#### `TooManyLoadBalancersException`

``` purescript
newtype TooManyLoadBalancersException
  = TooManyLoadBalancersException {  }
```

<p>You've reached the limit on the number of load balancers for your AWS account.</p>

##### Instances
``` purescript
Newtype TooManyLoadBalancersException _
```

#### `TooManyRegistrationsForTargetIdException`

``` purescript
newtype TooManyRegistrationsForTargetIdException
  = TooManyRegistrationsForTargetIdException {  }
```

<p>You've reached the limit on the number of times a target can be registered with a load balancer.</p>

##### Instances
``` purescript
Newtype TooManyRegistrationsForTargetIdException _
```

#### `TooManyRulesException`

``` purescript
newtype TooManyRulesException
  = TooManyRulesException {  }
```

<p>You've reached the limit on the number of rules per load balancer.</p>

##### Instances
``` purescript
Newtype TooManyRulesException _
```

#### `TooManyTagsException`

``` purescript
newtype TooManyTagsException
  = TooManyTagsException {  }
```

<p>You've reached the limit on the number of tags per load balancer.</p>

##### Instances
``` purescript
Newtype TooManyTagsException _
```

#### `TooManyTargetGroupsException`

``` purescript
newtype TooManyTargetGroupsException
  = TooManyTargetGroupsException {  }
```

<p>You've reached the limit on the number of target groups for your AWS account.</p>

##### Instances
``` purescript
Newtype TooManyTargetGroupsException _
```

#### `TooManyTargetsException`

``` purescript
newtype TooManyTargetsException
  = TooManyTargetsException {  }
```

<p>You've reached the limit on the number of targets.</p>

##### Instances
``` purescript
Newtype TooManyTargetsException _
```

#### `UnsupportedProtocolException`

``` purescript
newtype UnsupportedProtocolException
  = UnsupportedProtocolException {  }
```

<p>The specified protocol is not supported.</p>

##### Instances
``` purescript
Newtype UnsupportedProtocolException _
```

#### `VpcId`

``` purescript
newtype VpcId
  = VpcId String
```

##### Instances
``` purescript
Newtype VpcId _
```

#### `ZoneName`

``` purescript
newtype ZoneName
  = ZoneName String
```

##### Instances
``` purescript
Newtype ZoneName _
```


