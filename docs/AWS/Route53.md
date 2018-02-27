## Module AWS.Route53

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateVPCWithHostedZone`

``` purescript
associateVPCWithHostedZone :: forall eff. AssociateVPCWithHostedZoneRequest -> Aff (err :: RequestError | eff) AssociateVPCWithHostedZoneResponse
```

<p>Associates an Amazon VPC with a private hosted zone. </p> <important> <p>To perform the association, the VPC and the private hosted zone must already exist. You can't convert a public hosted zone into a private hosted zone.</p> </important> <note> <p>If you want to associate a VPC that was created by using one AWS account with a private hosted zone that was created by using a different account, the AWS account that created the private hosted zone must first submit a <code>CreateVPCAssociationAuthorization</code> request. Then the account that created the VPC must submit an <code>AssociateVPCWithHostedZone</code> request.</p> </note>

#### `changeResourceRecordSets`

``` purescript
changeResourceRecordSets :: forall eff. ChangeResourceRecordSetsRequest -> Aff (err :: RequestError | eff) ChangeResourceRecordSetsResponse
```

<p>Creates, changes, or deletes a resource record set, which contains authoritative DNS information for a specified domain name or subdomain name. For example, you can use <code>ChangeResourceRecordSets</code> to create a resource record set that routes traffic for test.example.com to a web server that has an IP address of 192.0.2.44.</p> <p> <b>Change Batches and Transactional Changes</b> </p> <p>The request body must include a document with a <code>ChangeResourceRecordSetsRequest</code> element. The request body contains a list of change items, known as a change batch. Change batches are considered transactional changes. When using the Amazon Route 53 API to change resource record sets, Amazon Route 53 either makes all or none of the changes in a change batch request. This ensures that Amazon Route 53 never partially implements the intended changes to the resource record sets in a hosted zone. </p> <p>For example, a change batch request that deletes the <code>CNAME</code> record for www.example.com and creates an alias resource record set for www.example.com. Amazon Route 53 deletes the first resource record set and creates the second resource record set in a single operation. If either the <code>DELETE</code> or the <code>CREATE</code> action fails, then both changes (plus any other changes in the batch) fail, and the original <code>CNAME</code> record continues to exist.</p> <important> <p>Due to the nature of transactional changes, you can't delete the same resource record set more than once in a single change batch. If you attempt to delete the same change batch more than once, Amazon Route 53 returns an <code>InvalidChangeBatch</code> error.</p> </important> <p> <b>Traffic Flow</b> </p> <p>To create resource record sets for complex routing configurations, use either the traffic flow visual editor in the Amazon Route 53 console or the API actions for traffic policies and traffic policy instances. Save the configuration as a traffic policy, then associate the traffic policy with one or more domain names (such as example.com) or subdomain names (such as www.example.com), in the same hosted zone or in multiple hosted zones. You can roll back the updates if the new configuration isn't performing as expected. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/traffic-flow.html">Using Traffic Flow to Route DNS Traffic</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p> <b>Create, Delete, and Upsert</b> </p> <p>Use <code>ChangeResourceRecordsSetsRequest</code> to perform the following actions:</p> <ul> <li> <p> <code>CREATE</code>: Creates a resource record set that has the specified values.</p> </li> <li> <p> <code>DELETE</code>: Deletes an existing resource record set that has the specified values.</p> </li> <li> <p> <code>UPSERT</code>: If a resource record set does not already exist, AWS creates it. If a resource set does exist, Amazon Route 53 updates it with the values in the request. </p> </li> </ul> <p> <b>Syntaxes for Creating, Updating, and Deleting Resource Record Sets</b> </p> <p>The syntax for a request depends on the type of resource record set that you want to create, delete, or update, such as weighted, alias, or failover. The XML elements in your request must appear in the order listed in the syntax. </p> <p>For an example for each type of resource record set, see "Examples."</p> <p>Don't refer to the syntax in the "Parameter Syntax" section, which includes all of the elements for every kind of resource record set that you can create, delete, or update by using <code>ChangeResourceRecordSets</code>. </p> <p> <b>Change Propagation to Amazon Route 53 DNS Servers</b> </p> <p>When you submit a <code>ChangeResourceRecordSets</code> request, Amazon Route 53 propagates your changes to all of the Amazon Route 53 authoritative DNS servers. While your changes are propagating, <code>GetChange</code> returns a status of <code>PENDING</code>. When propagation is complete, <code>GetChange</code> returns a status of <code>INSYNC</code>. Changes generally propagate to all Amazon Route 53 name servers within 60 seconds. For more information, see <a>GetChange</a>.</p> <p> <b>Limits on ChangeResourceRecordSets Requests</b> </p> <p>For information about the limits on a <code>ChangeResourceRecordSets</code> request, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>

#### `changeTagsForResource`

``` purescript
changeTagsForResource :: forall eff. ChangeTagsForResourceRequest -> Aff (err :: RequestError | eff) ChangeTagsForResourceResponse
```

<p>Adds, edits, or deletes tags for a health check or a hosted zone.</p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>

#### `createHealthCheck`

``` purescript
createHealthCheck :: forall eff. CreateHealthCheckRequest -> Aff (err :: RequestError | eff) CreateHealthCheckResponse
```

<p>Creates a new health check.</p> <p>For information about adding health checks to resource record sets, see <a>ResourceRecordSet$HealthCheckId</a> in <a>ChangeResourceRecordSets</a>. </p> <p> <b>ELB Load Balancers</b> </p> <p>If you're registering EC2 instances with an Elastic Load Balancing (ELB) load balancer, do not create Amazon Route 53 health checks for the EC2 instances. When you register an EC2 instance with a load balancer, you configure settings for an ELB health check, which performs a similar function to an Amazon Route 53 health check.</p> <p> <b>Private Hosted Zones</b> </p> <p>You can associate health checks with failover resource record sets in a private hosted zone. Note the following:</p> <ul> <li> <p>Amazon Route 53 health checkers are outside the VPC. To check the health of an endpoint within a VPC by IP address, you must assign a public IP address to the instance in the VPC.</p> </li> <li> <p>You can configure a health checker to check the health of an external resource that the instance relies on, such as a database server.</p> </li> <li> <p>You can create a CloudWatch metric, associate an alarm with the metric, and then create a health check that is based on the state of the alarm. For example, you might create a CloudWatch metric that checks the status of the Amazon EC2 <code>StatusCheckFailed</code> metric, add an alarm to the metric, and then create a health check that is based on the state of the alarm. For information about creating CloudWatch metrics and alarms by using the CloudWatch console, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/WhatIsCloudWatch.html">Amazon CloudWatch User Guide</a>.</p> </li> </ul>

#### `createHostedZone`

``` purescript
createHostedZone :: forall eff. CreateHostedZoneRequest -> Aff (err :: RequestError | eff) CreateHostedZoneResponse
```

<p>Creates a new public hosted zone, which you use to specify how the Domain Name System (DNS) routes traffic on the Internet for a domain, such as example.com, and its subdomains. </p> <important> <p>You can't convert a public hosted zones to a private hosted zone or vice versa. Instead, you must create a new hosted zone with the same name and create new resource record sets.</p> </important> <p>For more information about charges for hosted zones, see <a href="http://aws.amazon.com/route53/pricing/">Amazon Route 53 Pricing</a>.</p> <p>Note the following:</p> <ul> <li> <p>You can't create a hosted zone for a top-level domain (TLD).</p> </li> <li> <p>Amazon Route 53 automatically creates a default SOA record and four NS records for the zone. For more information about SOA and NS records, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/SOA-NSrecords.html">NS and SOA Records that Amazon Route 53 Creates for a Hosted Zone</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>If you want to use the same name servers for multiple hosted zones, you can optionally associate a reusable delegation set with the hosted zone. See the <code>DelegationSetId</code> element.</p> </li> <li> <p>If your domain is registered with a registrar other than Amazon Route 53, you must update the name servers with your registrar to make Amazon Route 53 your DNS service. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/creating-migrating.html">Configuring Amazon Route 53 as your DNS Service</a> in the <i>Amazon Route 53 Developer Guide</i>. </p> </li> </ul> <p>When you submit a <code>CreateHostedZone</code> request, the initial status of the hosted zone is <code>PENDING</code>. This means that the NS and SOA records are not yet available on all Amazon Route 53 DNS servers. When the NS and SOA records are available, the status of the zone changes to <code>INSYNC</code>.</p>

#### `createQueryLoggingConfig`

``` purescript
createQueryLoggingConfig :: forall eff. CreateQueryLoggingConfigRequest -> Aff (err :: RequestError | eff) CreateQueryLoggingConfigResponse
```

<p>Creates a configuration for DNS query logging. After you create a query logging configuration, Amazon Route 53 begins to publish log data to an Amazon CloudWatch Logs log group.</p> <p>DNS query logs contain information about the queries that Amazon Route 53 receives for a specified public hosted zone, such as the following:</p> <ul> <li> <p>Amazon Route 53 edge location that responded to the DNS query</p> </li> <li> <p>Domain or subdomain that was requested</p> </li> <li> <p>DNS record type, such as A or AAAA</p> </li> <li> <p>DNS response code, such as <code>NoError</code> or <code>ServFail</code> </p> </li> </ul> <dl> <dt>Log Group and Resource Policy</dt> <dd> <p>Before you create a query logging configuration, perform the following operations.</p> <note> <p>If you create a query logging configuration using the Amazon Route 53 console, Amazon Route 53 performs these operations automatically.</p> </note> <ol> <li> <p>Create a CloudWatch Logs log group, and make note of the ARN, which you specify when you create a query logging configuration. Note the following:</p> <ul> <li> <p>You must create the log group in the us-east-1 region.</p> </li> <li> <p>You must use the same AWS account to create the log group and the hosted zone that you want to configure query logging for.</p> </li> <li> <p>When you create log groups for query logging, we recommend that you use a consistent prefix, for example:</p> <p> <code>/aws/route53/<i>hosted zone name</i> </code> </p> <p>In the next step, you'll create a resource policy, which controls access to one or more log groups and the associated AWS resources, such as Amazon Route 53 hosted zones. There's a limit on the number of resource policies that you can create, so we recommend that you use a consistent prefix so you can use the same resource policy for all the log groups that you create for query logging.</p> </li> </ul> </li> <li> <p>Create a CloudWatch Logs resource policy, and give it the permissions that Amazon Route 53 needs to create log streams and to send query logs to log streams. For the value of <code>Resource</code>, specify the ARN for the log group that you created in the previous step. To use the same resource policy for all the CloudWatch Logs log groups that you created for query logging configurations, replace the hosted zone name with <code>*</code>, for example:</p> <p> <code>arn:aws:logs:us-east-1:123412341234:log-group:/aws/route53/*</code> </p> <note> <p>You can't use the CloudWatch console to create or edit a resource policy. You must use the CloudWatch API, one of the AWS SDKs, or the AWS CLI.</p> </note> </li> </ol> </dd> <dt>Log Streams and Edge Locations</dt> <dd> <p>When Amazon Route 53 finishes creating the configuration for DNS query logging, it does the following:</p> <ul> <li> <p>Creates a log stream for an edge location the first time that the edge location responds to DNS queries for the specified hosted zone. That log stream is used to log all queries that Amazon Route 53 responds to for that edge location.</p> </li> <li> <p>Begins to send query logs to the applicable log stream.</p> </li> </ul> <p>The name of each log stream is in the following format:</p> <p> <code> <i>hosted zone ID</i>/<i>edge location code</i> </code> </p> <p>The edge location code is a three-letter code and an arbitrarily assigned number, for example, DFW3. The three-letter code typically corresponds with the International Air Transport Association airport code for an airport near the edge location. (These abbreviations might change in the future.) For a list of edge locations, see "The Amazon Route 53 Global Network" on the <a href="http://aws.amazon.com/route53/details/">Amazon Route 53 Product Details</a> page.</p> </dd> <dt>Queries That Are Logged</dt> <dd> <p>Query logs contain only the queries that DNS resolvers forward to Amazon Route 53. If a DNS resolver has already cached the response to a query (such as the IP address for a load balancer for example.com), the resolver will continue to return the cached response. It doesn't forward another query to Amazon Route 53 until the TTL for the corresponding resource record set expires. Depending on how many DNS queries are submitted for a resource record set, and depending on the TTL for that resource record set, query logs might contain information about only one query out of every several thousand queries that are submitted to DNS. For more information about how DNS works, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/welcome-dns-service.html">Routing Internet Traffic to Your Website or Web Application</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </dd> <dt>Log File Format</dt> <dd> <p>For a list of the values in each query log and the format of each value, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </dd> <dt>Pricing</dt> <dd> <p>For information about charges for query logs, see <a href="http://aws.amazon.com/cloudwatch/pricing/">Amazon CloudWatch Pricing</a>.</p> </dd> <dt>How to Stop Logging</dt> <dd> <p>If you want Amazon Route 53 to stop sending query logs to CloudWatch Logs, delete the query logging configuration. For more information, see <a>DeleteQueryLoggingConfig</a>.</p> </dd> </dl>

#### `createReusableDelegationSet`

``` purescript
createReusableDelegationSet :: forall eff. CreateReusableDelegationSetRequest -> Aff (err :: RequestError | eff) CreateReusableDelegationSetResponse
```

<p>Creates a delegation set (a group of four name servers) that can be reused by multiple hosted zones. If a hosted zoned ID is specified, <code>CreateReusableDelegationSet</code> marks the delegation set associated with that zone as reusable.</p> <note> <p>You can't associate a reusable delegation set with a private hosted zone.</p> </note> <p>For information about using a reusable delegation set to configure white label name servers, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/white-label-name-servers.html">Configuring White Label Name Servers</a>.</p> <p>The process for migrating existing hosted zones to use a reusable delegation set is comparable to the process for configuring white label name servers. You need to perform the following steps:</p> <ol> <li> <p>Create a reusable delegation set.</p> </li> <li> <p>Recreate hosted zones, and reduce the TTL to 60 seconds or less.</p> </li> <li> <p>Recreate resource record sets in the new hosted zones.</p> </li> <li> <p>Change the registrar's name servers to use the name servers for the new hosted zones.</p> </li> <li> <p>Monitor traffic for the website or application.</p> </li> <li> <p>Change TTLs back to their original values.</p> </li> </ol> <p>If you want to migrate existing hosted zones to use a reusable delegation set, the existing hosted zones can't use any of the name servers that are assigned to the reusable delegation set. If one or more hosted zones do use one or more name servers that are assigned to the reusable delegation set, you can do one of the following:</p> <ul> <li> <p>For small numbers of hosted zones—up to a few hundred—it's relatively easy to create reusable delegation sets until you get one that has four name servers that don't overlap with any of the name servers in your hosted zones.</p> </li> <li> <p>For larger numbers of hosted zones, the easiest solution is to use more than one reusable delegation set.</p> </li> <li> <p>For larger numbers of hosted zones, you can also migrate hosted zones that have overlapping name servers to hosted zones that don't have overlapping name servers, then migrate the hosted zones again to use the reusable delegation set.</p> </li> </ul>

#### `createTrafficPolicy`

``` purescript
createTrafficPolicy :: forall eff. CreateTrafficPolicyRequest -> Aff (err :: RequestError | eff) CreateTrafficPolicyResponse
```

<p>Creates a traffic policy, which you use to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com).</p>

#### `createTrafficPolicyInstance`

``` purescript
createTrafficPolicyInstance :: forall eff. CreateTrafficPolicyInstanceRequest -> Aff (err :: RequestError | eff) CreateTrafficPolicyInstanceResponse
```

<p>Creates resource record sets in a specified hosted zone based on the settings in a specified traffic policy version. In addition, <code>CreateTrafficPolicyInstance</code> associates the resource record sets with a specified domain name (such as example.com) or subdomain name (such as www.example.com). Amazon Route 53 responds to DNS queries for the domain or subdomain name by using the resource record sets that <code>CreateTrafficPolicyInstance</code> created.</p>

#### `createTrafficPolicyVersion`

``` purescript
createTrafficPolicyVersion :: forall eff. CreateTrafficPolicyVersionRequest -> Aff (err :: RequestError | eff) CreateTrafficPolicyVersionResponse
```

<p>Creates a new version of an existing traffic policy. When you create a new version of a traffic policy, you specify the ID of the traffic policy that you want to update and a JSON-formatted document that describes the new version. You use traffic policies to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com). You can create a maximum of 1000 versions of a traffic policy. If you reach the limit and need to create another version, you'll need to start a new traffic policy.</p>

#### `createVPCAssociationAuthorization`

``` purescript
createVPCAssociationAuthorization :: forall eff. CreateVPCAssociationAuthorizationRequest -> Aff (err :: RequestError | eff) CreateVPCAssociationAuthorizationResponse
```

<p>Authorizes the AWS account that created a specified VPC to submit an <code>AssociateVPCWithHostedZone</code> request to associate the VPC with a specified hosted zone that was created by a different account. To submit a <code>CreateVPCAssociationAuthorization</code> request, you must use the account that created the hosted zone. After you authorize the association, use the account that created the VPC to submit an <code>AssociateVPCWithHostedZone</code> request.</p> <note> <p>If you want to associate multiple VPCs that you created by using one account with a hosted zone that you created by using a different account, you must submit one authorization request for each VPC.</p> </note>

#### `deleteHealthCheck`

``` purescript
deleteHealthCheck :: forall eff. DeleteHealthCheckRequest -> Aff (err :: RequestError | eff) DeleteHealthCheckResponse
```

<p>Deletes a health check.</p> <important> <p>Amazon Route 53 does not prevent you from deleting a health check even if the health check is associated with one or more resource record sets. If you delete a health check and you don't update the associated resource record sets, the future status of the health check can't be predicted and may change. This will affect the routing of DNS queries for your DNS failover configuration. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html#health-checks-deleting.html">Replacing and Deleting Health Checks</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </important>

#### `deleteHostedZone`

``` purescript
deleteHostedZone :: forall eff. DeleteHostedZoneRequest -> Aff (err :: RequestError | eff) DeleteHostedZoneResponse
```

<p>Deletes a hosted zone.</p> <important> <p>If the name servers for the hosted zone are associated with a domain and if you want to make the domain unavailable on the Internet, we recommend that you delete the name servers from the domain to prevent future DNS queries from possibly being misrouted. If the domain is registered with Amazon Route 53, see <code>UpdateDomainNameservers</code>. If the domain is registered with another registrar, use the method provided by the registrar to delete name servers for the domain.</p> <p>Some domain registries don't allow you to remove all of the name servers for a domain. If the registry for your domain requires one or more name servers, we recommend that you delete the hosted zone only if you transfer DNS service to another service provider, and you replace the name servers for the domain with name servers from the new provider.</p> </important> <p>You can delete a hosted zone only if it contains only the default SOA record and NS resource record sets. If the hosted zone contains other resource record sets, you must delete them before you can delete the hosted zone. If you try to delete a hosted zone that contains other resource record sets, the request fails, and Amazon Route 53 returns a <code>HostedZoneNotEmpty</code> error. For information about deleting records from your hosted zone, see <a>ChangeResourceRecordSets</a>.</p> <p>To verify that the hosted zone has been deleted, do one of the following:</p> <ul> <li> <p>Use the <code>GetHostedZone</code> action to request information about the hosted zone.</p> </li> <li> <p>Use the <code>ListHostedZones</code> action to get a list of the hosted zones associated with the current AWS account.</p> </li> </ul>

#### `deleteQueryLoggingConfig`

``` purescript
deleteQueryLoggingConfig :: forall eff. DeleteQueryLoggingConfigRequest -> Aff (err :: RequestError | eff) DeleteQueryLoggingConfigResponse
```

<p>Deletes a configuration for DNS query logging. If you delete a configuration, Amazon Route 53 stops sending query logs to CloudWatch Logs. Amazon Route 53 doesn't delete any logs that are already in CloudWatch Logs.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a>.</p>

#### `deleteReusableDelegationSet`

``` purescript
deleteReusableDelegationSet :: forall eff. DeleteReusableDelegationSetRequest -> Aff (err :: RequestError | eff) DeleteReusableDelegationSetResponse
```

<p>Deletes a reusable delegation set.</p> <important> <p>You can delete a reusable delegation set only if it isn't associated with any hosted zones.</p> </important> <p>To verify that the reusable delegation set is not associated with any hosted zones, submit a <a>GetReusableDelegationSet</a> request and specify the ID of the reusable delegation set that you want to delete.</p>

#### `deleteTrafficPolicy`

``` purescript
deleteTrafficPolicy :: forall eff. DeleteTrafficPolicyRequest -> Aff (err :: RequestError | eff) DeleteTrafficPolicyResponse
```

<p>Deletes a traffic policy.</p>

#### `deleteTrafficPolicyInstance`

``` purescript
deleteTrafficPolicyInstance :: forall eff. DeleteTrafficPolicyInstanceRequest -> Aff (err :: RequestError | eff) DeleteTrafficPolicyInstanceResponse
```

<p>Deletes a traffic policy instance and all of the resource record sets that Amazon Route 53 created when you created the instance.</p> <note> <p>In the Amazon Route 53 console, traffic policy instances are known as policy records.</p> </note>

#### `deleteVPCAssociationAuthorization`

``` purescript
deleteVPCAssociationAuthorization :: forall eff. DeleteVPCAssociationAuthorizationRequest -> Aff (err :: RequestError | eff) DeleteVPCAssociationAuthorizationResponse
```

<p>Removes authorization to submit an <code>AssociateVPCWithHostedZone</code> request to associate a specified VPC with a hosted zone that was created by a different account. You must use the account that created the hosted zone to submit a <code>DeleteVPCAssociationAuthorization</code> request.</p> <important> <p>Sending this request only prevents the AWS account that created the VPC from associating the VPC with the Amazon Route 53 hosted zone in the future. If the VPC is already associated with the hosted zone, <code>DeleteVPCAssociationAuthorization</code> won't disassociate the VPC from the hosted zone. If you want to delete an existing association, use <code>DisassociateVPCFromHostedZone</code>.</p> </important>

#### `disassociateVPCFromHostedZone`

``` purescript
disassociateVPCFromHostedZone :: forall eff. DisassociateVPCFromHostedZoneRequest -> Aff (err :: RequestError | eff) DisassociateVPCFromHostedZoneResponse
```

<p>Disassociates a VPC from a Amazon Route 53 private hosted zone. </p> <note> <p>You can't disassociate the last VPC from a private hosted zone.</p> </note> <important> <p>You can't disassociate a VPC from a private hosted zone when only one VPC is associated with the hosted zone. You also can't convert a private hosted zone into a public hosted zone.</p> </important>

#### `getAccountLimit`

``` purescript
getAccountLimit :: forall eff. GetAccountLimitRequest -> Aff (err :: RequestError | eff) GetAccountLimitResponse
```

<p>Gets the specified limit for the current account, for example, the maximum number of health checks that you can create using the account.</p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>

#### `getChange`

``` purescript
getChange :: forall eff. GetChangeRequest -> Aff (err :: RequestError | eff) GetChangeResponse
```

<p>Returns the current status of a change batch request. The status is one of the following values:</p> <ul> <li> <p> <code>PENDING</code> indicates that the changes in this request have not propagated to all Amazon Route 53 DNS servers. This is the initial status of all change batch requests.</p> </li> <li> <p> <code>INSYNC</code> indicates that the changes have propagated to all Amazon Route 53 DNS servers. </p> </li> </ul>

#### `getCheckerIpRanges`

``` purescript
getCheckerIpRanges :: forall eff. GetCheckerIpRangesRequest -> Aff (err :: RequestError | eff) GetCheckerIpRangesResponse
```

<p> <code>GetCheckerIpRanges</code> still works, but we recommend that you download ip-ranges.json, which includes IP address ranges for all AWS services. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/route-53-ip-addresses.html">IP Address Ranges of Amazon Route 53 Servers</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>

#### `getGeoLocation`

``` purescript
getGeoLocation :: forall eff. GetGeoLocationRequest -> Aff (err :: RequestError | eff) GetGeoLocationResponse
```

<p>Gets information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.</p> <p>Use the following syntax to determine whether a continent is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?ContinentCode=<i>two-letter abbreviation for a continent</i> </code> </p> <p>Use the following syntax to determine whether a country is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?CountryCode=<i>two-character country code</i> </code> </p> <p>Use the following syntax to determine whether a subdivision of a country is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?CountryCode=<i>two-character country code</i>&amp;SubdivisionCode=<i>subdivision code</i> </code> </p>

#### `getHealthCheck`

``` purescript
getHealthCheck :: forall eff. GetHealthCheckRequest -> Aff (err :: RequestError | eff) GetHealthCheckResponse
```

<p>Gets information about a specified health check.</p>

#### `getHealthCheckCount`

``` purescript
getHealthCheckCount :: forall eff. GetHealthCheckCountRequest -> Aff (err :: RequestError | eff) GetHealthCheckCountResponse
```

<p>Retrieves the number of health checks that are associated with the current AWS account.</p>

#### `getHealthCheckLastFailureReason`

``` purescript
getHealthCheckLastFailureReason :: forall eff. GetHealthCheckLastFailureReasonRequest -> Aff (err :: RequestError | eff) GetHealthCheckLastFailureReasonResponse
```

<p>Gets the reason that a specified health check failed most recently.</p>

#### `getHealthCheckStatus`

``` purescript
getHealthCheckStatus :: forall eff. GetHealthCheckStatusRequest -> Aff (err :: RequestError | eff) GetHealthCheckStatusResponse
```

<p>Gets status of a specified health check. </p>

#### `getHostedZone`

``` purescript
getHostedZone :: forall eff. GetHostedZoneRequest -> Aff (err :: RequestError | eff) GetHostedZoneResponse
```

<p>Gets information about a specified hosted zone including the four name servers assigned to the hosted zone.</p>

#### `getHostedZoneCount`

``` purescript
getHostedZoneCount :: forall eff. GetHostedZoneCountRequest -> Aff (err :: RequestError | eff) GetHostedZoneCountResponse
```

<p>Retrieves the number of hosted zones that are associated with the current AWS account.</p>

#### `getHostedZoneLimit`

``` purescript
getHostedZoneLimit :: forall eff. GetHostedZoneLimitRequest -> Aff (err :: RequestError | eff) GetHostedZoneLimitResponse
```

<p>Gets the specified limit for a specified hosted zone, for example, the maximum number of records that you can create in the hosted zone. </p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>

#### `getQueryLoggingConfig`

``` purescript
getQueryLoggingConfig :: forall eff. GetQueryLoggingConfigRequest -> Aff (err :: RequestError | eff) GetQueryLoggingConfigResponse
```

<p>Gets information about a specified configuration for DNS query logging.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a> and <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a>.</p>

#### `getReusableDelegationSet`

``` purescript
getReusableDelegationSet :: forall eff. GetReusableDelegationSetRequest -> Aff (err :: RequestError | eff) GetReusableDelegationSetResponse
```

<p>Retrieves information about a specified reusable delegation set, including the four name servers that are assigned to the delegation set.</p>

#### `getReusableDelegationSetLimit`

``` purescript
getReusableDelegationSetLimit :: forall eff. GetReusableDelegationSetLimitRequest -> Aff (err :: RequestError | eff) GetReusableDelegationSetLimitResponse
```

<p>Gets the maximum number of hosted zones that you can associate with the specified reusable delegation set.</p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>

#### `getTrafficPolicy`

``` purescript
getTrafficPolicy :: forall eff. GetTrafficPolicyRequest -> Aff (err :: RequestError | eff) GetTrafficPolicyResponse
```

<p>Gets information about a specific traffic policy version.</p>

#### `getTrafficPolicyInstance`

``` purescript
getTrafficPolicyInstance :: forall eff. GetTrafficPolicyInstanceRequest -> Aff (err :: RequestError | eff) GetTrafficPolicyInstanceResponse
```

<p>Gets information about a specified traffic policy instance.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <note> <p>In the Amazon Route 53 console, traffic policy instances are known as policy records.</p> </note>

#### `getTrafficPolicyInstanceCount`

``` purescript
getTrafficPolicyInstanceCount :: forall eff. GetTrafficPolicyInstanceCountRequest -> Aff (err :: RequestError | eff) GetTrafficPolicyInstanceCountResponse
```

<p>Gets the number of traffic policy instances that are associated with the current AWS account.</p>

#### `listGeoLocations`

``` purescript
listGeoLocations :: forall eff. ListGeoLocationsRequest -> Aff (err :: RequestError | eff) ListGeoLocationsResponse
```

<p>Retrieves a list of supported geo locations.</p> <p>Countries are listed first, and continents are listed last. If Amazon Route 53 supports subdivisions for a country (for example, states or provinces), the subdivisions for that country are listed in alphabetical order immediately after the corresponding country.</p>

#### `listHealthChecks`

``` purescript
listHealthChecks :: forall eff. ListHealthChecksRequest -> Aff (err :: RequestError | eff) ListHealthChecksResponse
```

<p>Retrieve a list of the health checks that are associated with the current AWS account. </p>

#### `listHostedZones`

``` purescript
listHostedZones :: forall eff. ListHostedZonesRequest -> Aff (err :: RequestError | eff) ListHostedZonesResponse
```

<p>Retrieves a list of the public and private hosted zones that are associated with the current AWS account. The response includes a <code>HostedZones</code> child element for each hosted zone.</p> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of hosted zones, you can use the <code>maxitems</code> parameter to list them in groups of up to 100.</p>

#### `listHostedZonesByName`

``` purescript
listHostedZonesByName :: forall eff. ListHostedZonesByNameRequest -> Aff (err :: RequestError | eff) ListHostedZonesByNameResponse
```

<p>Retrieves a list of your hosted zones in lexicographic order. The response includes a <code>HostedZones</code> child element for each hosted zone created by the current AWS account. </p> <p> <code>ListHostedZonesByName</code> sorts hosted zones by name with the labels reversed. For example:</p> <p> <code>com.example.www.</code> </p> <p>Note the trailing dot, which can change the sort order in some circumstances.</p> <p>If the domain name includes escape characters or Punycode, <code>ListHostedZonesByName</code> alphabetizes the domain name using the escaped or Punycoded value, which is the format that Amazon Route 53 saves in its database. For example, to create a hosted zone for exämple.com, you specify ex\344mple.com for the domain name. <code>ListHostedZonesByName</code> alphabetizes it as:</p> <p> <code>com.ex\344mple.</code> </p> <p>The labels are reversed and alphabetized using the escaped value. For more information about valid domain name formats, including internationalized domain names, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html">DNS Domain Name Format</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>Amazon Route 53 returns up to 100 items in each response. If you have a lot of hosted zones, use the <code>MaxItems</code> parameter to list them in groups of up to 100. The response includes values that help navigate from one group of <code>MaxItems</code> hosted zones to the next:</p> <ul> <li> <p>The <code>DNSName</code> and <code>HostedZoneId</code> elements in the response contain the values, if any, specified for the <code>dnsname</code> and <code>hostedzoneid</code> parameters in the request that produced the current response.</p> </li> <li> <p>The <code>MaxItems</code> element in the response contains the value, if any, that you specified for the <code>maxitems</code> parameter in the request that produced the current response.</p> </li> <li> <p>If the value of <code>IsTruncated</code> in the response is true, there are more hosted zones associated with the current AWS account. </p> <p>If <code>IsTruncated</code> is false, this response includes the last hosted zone that is associated with the current account. The <code>NextDNSName</code> element and <code>NextHostedZoneId</code> elements are omitted from the response.</p> </li> <li> <p>The <code>NextDNSName</code> and <code>NextHostedZoneId</code> elements in the response contain the domain name and the hosted zone ID of the next hosted zone that is associated with the current AWS account. If you want to list more hosted zones, make another call to <code>ListHostedZonesByName</code>, and specify the value of <code>NextDNSName</code> and <code>NextHostedZoneId</code> in the <code>dnsname</code> and <code>hostedzoneid</code> parameters, respectively.</p> </li> </ul>

#### `listQueryLoggingConfigs`

``` purescript
listQueryLoggingConfigs :: forall eff. ListQueryLoggingConfigsRequest -> Aff (err :: RequestError | eff) ListQueryLoggingConfigsResponse
```

<p>Lists the configurations for DNS query logging that are associated with the current AWS account or the configuration that is associated with a specified hosted zone.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a>. Additional information, including the format of DNS query logs, appears in <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>

#### `listResourceRecordSets`

``` purescript
listResourceRecordSets :: forall eff. ListResourceRecordSetsRequest -> Aff (err :: RequestError | eff) ListResourceRecordSetsResponse
```

<p>Lists the resource record sets in a specified hosted zone.</p> <p> <code>ListResourceRecordSets</code> returns up to 100 resource record sets at a time in ASCII order, beginning at a position specified by the <code>name</code> and <code>type</code> elements. The action sorts results first by DNS name with the labels reversed, for example:</p> <p> <code>com.example.www.</code> </p> <p>Note the trailing dot, which can change the sort order in some circumstances.</p> <p>When multiple records have the same DNS name, the action sorts results by the record type.</p> <p>You can use the name and type elements to adjust the beginning position of the list of resource record sets returned:</p> <dl> <dt>If you do not specify Name or Type</dt> <dd> <p>The results begin with the first resource record set that the hosted zone contains.</p> </dd> <dt>If you specify Name but not Type</dt> <dd> <p>The results begin with the first resource record set in the list whose name is greater than or equal to <code>Name</code>.</p> </dd> <dt>If you specify Type but not Name</dt> <dd> <p>Amazon Route 53 returns the <code>InvalidInput</code> error.</p> </dd> <dt>If you specify both Name and Type</dt> <dd> <p>The results begin with the first resource record set in the list whose name is greater than or equal to <code>Name</code>, and whose type is greater than or equal to <code>Type</code>.</p> </dd> </dl> <p>This action returns the most current version of the records. This includes records that are <code>PENDING</code>, and that are not yet available on all Amazon Route 53 DNS servers.</p> <p>To ensure that you get an accurate listing of the resource record sets for a hosted zone at a point in time, do not submit a <code>ChangeResourceRecordSets</code> request while you're paging through the results of a <code>ListResourceRecordSets</code> request. If you do, some pages may display results without the latest changes while other pages display results with the latest changes.</p>

#### `listReusableDelegationSets`

``` purescript
listReusableDelegationSets :: forall eff. ListReusableDelegationSetsRequest -> Aff (err :: RequestError | eff) ListReusableDelegationSetsResponse
```

<p>Retrieves a list of the reusable delegation sets that are associated with the current AWS account.</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: RequestError | eff) ListTagsForResourceResponse
```

<p>Lists tags for one health check or hosted zone. </p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>

#### `listTagsForResources`

``` purescript
listTagsForResources :: forall eff. ListTagsForResourcesRequest -> Aff (err :: RequestError | eff) ListTagsForResourcesResponse
```

<p>Lists tags for up to 10 health checks or hosted zones.</p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>

#### `listTrafficPolicies`

``` purescript
listTrafficPolicies :: forall eff. ListTrafficPoliciesRequest -> Aff (err :: RequestError | eff) ListTrafficPoliciesResponse
```

<p>Gets information about the latest version for every traffic policy that is associated with the current AWS account. Policies are listed in the order in which they were created. </p>

#### `listTrafficPolicyInstances`

``` purescript
listTrafficPolicyInstances :: forall eff. ListTrafficPolicyInstancesRequest -> Aff (err :: RequestError | eff) ListTrafficPolicyInstancesResponse
```

<p>Gets information about the traffic policy instances that you created by using the current AWS account.</p> <note> <p>After you submit an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>

#### `listTrafficPolicyInstancesByHostedZone`

``` purescript
listTrafficPolicyInstancesByHostedZone :: forall eff. ListTrafficPolicyInstancesByHostedZoneRequest -> Aff (err :: RequestError | eff) ListTrafficPolicyInstancesByHostedZoneResponse
```

<p>Gets information about the traffic policy instances that you created in a specified hosted zone.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>

#### `listTrafficPolicyInstancesByPolicy`

``` purescript
listTrafficPolicyInstancesByPolicy :: forall eff. ListTrafficPolicyInstancesByPolicyRequest -> Aff (err :: RequestError | eff) ListTrafficPolicyInstancesByPolicyResponse
```

<p>Gets information about the traffic policy instances that you created by using a specify traffic policy version.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>

#### `listTrafficPolicyVersions`

``` purescript
listTrafficPolicyVersions :: forall eff. ListTrafficPolicyVersionsRequest -> Aff (err :: RequestError | eff) ListTrafficPolicyVersionsResponse
```

<p>Gets information about all of the versions for a specified traffic policy.</p> <p>Traffic policy versions are listed in numerical order by <code>VersionNumber</code>.</p>

#### `listVPCAssociationAuthorizations`

``` purescript
listVPCAssociationAuthorizations :: forall eff. ListVPCAssociationAuthorizationsRequest -> Aff (err :: RequestError | eff) ListVPCAssociationAuthorizationsResponse
```

<p>Gets a list of the VPCs that were created by other accounts and that can be associated with a specified hosted zone because you've submitted one or more <code>CreateVPCAssociationAuthorization</code> requests. </p> <p>The response includes a <code>VPCs</code> element with a <code>VPC</code> child element for each VPC that can be associated with the hosted zone.</p>

#### `testDNSAnswer`

``` purescript
testDNSAnswer :: forall eff. TestDNSAnswerRequest -> Aff (err :: RequestError | eff) TestDNSAnswerResponse
```

<p>Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask. </p>

#### `updateHealthCheck`

``` purescript
updateHealthCheck :: forall eff. UpdateHealthCheckRequest -> Aff (err :: RequestError | eff) UpdateHealthCheckResponse
```

<p>Updates an existing health check. Note that some values can't be updated. </p> <p>For more information about updating health checks, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html">Creating, Updating, and Deleting Health Checks</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>

#### `updateHostedZoneComment`

``` purescript
updateHostedZoneComment :: forall eff. UpdateHostedZoneCommentRequest -> Aff (err :: RequestError | eff) UpdateHostedZoneCommentResponse
```

<p>Updates the comment for a specified hosted zone.</p>

#### `updateTrafficPolicyComment`

``` purescript
updateTrafficPolicyComment :: forall eff. UpdateTrafficPolicyCommentRequest -> Aff (err :: RequestError | eff) UpdateTrafficPolicyCommentResponse
```

<p>Updates the comment for a specified traffic policy version.</p>

#### `updateTrafficPolicyInstance`

``` purescript
updateTrafficPolicyInstance :: forall eff. UpdateTrafficPolicyInstanceRequest -> Aff (err :: RequestError | eff) UpdateTrafficPolicyInstanceResponse
```

<p>Updates the resource record sets in a specified hosted zone that were created based on the settings in a specified traffic policy version.</p> <p>When you update a traffic policy instance, Amazon Route 53 continues to respond to DNS queries for the root resource record set name (such as example.com) while it replaces one group of resource record sets with another. Amazon Route 53 performs the following operations:</p> <ol> <li> <p>Amazon Route 53 creates a new group of resource record sets based on the specified traffic policy. This is true regardless of how significant the differences are between the existing resource record sets and the new resource record sets. </p> </li> <li> <p>When all of the new resource record sets have been created, Amazon Route 53 starts to respond to DNS queries for the root resource record set name (such as example.com) by using the new resource record sets.</p> </li> <li> <p>Amazon Route 53 deletes the old group of resource record sets that are associated with the root resource record set name.</p> </li> </ol>

#### `AccountLimit`

``` purescript
newtype AccountLimit
  = AccountLimit { "Type" :: AccountLimitType, "Value" :: LimitValue }
```

<p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>

##### Instances
``` purescript
Newtype AccountLimit _
```

#### `AccountLimitType`

``` purescript
newtype AccountLimitType
  = AccountLimitType String
```

##### Instances
``` purescript
Newtype AccountLimitType _
```

#### `AlarmIdentifier`

``` purescript
newtype AlarmIdentifier
  = AlarmIdentifier { "Region" :: CloudWatchRegion, "Name" :: AlarmName }
```

<p>A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.</p>

##### Instances
``` purescript
Newtype AlarmIdentifier _
```

#### `AlarmName`

``` purescript
newtype AlarmName
  = AlarmName String
```

##### Instances
``` purescript
Newtype AlarmName _
```

#### `AliasHealthEnabled`

``` purescript
newtype AliasHealthEnabled
  = AliasHealthEnabled Boolean
```

##### Instances
``` purescript
Newtype AliasHealthEnabled _
```

#### `AliasTarget`

``` purescript
newtype AliasTarget
  = AliasTarget { "HostedZoneId" :: ResourceId, "DNSName" :: DNSName, "EvaluateTargetHealth" :: AliasHealthEnabled }
```

<p> <i>Alias resource record sets only:</i> Information about the CloudFront distribution, Elastic Beanstalk environment, ELB load balancer, Amazon S3 bucket, or Amazon Route 53 resource record set that you're redirecting queries to. An Elastic Beanstalk environment must have a regionalized subdomain.</p> <p>When creating resource record sets for a private hosted zone, note the following:</p> <ul> <li> <p>Resource record sets can't be created for CloudFront distributions in a private hosted zone.</p> </li> <li> <p>Creating geolocation alias resource record sets or latency alias resource record sets in a private hosted zone is unsupported.</p> </li> <li> <p>For information about creating failover resource record sets in a private hosted zone, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html">Configuring Failover in a Private Hosted Zone</a>.</p> </li> </ul>

##### Instances
``` purescript
Newtype AliasTarget _
```

#### `AssociateVPCComment`

``` purescript
newtype AssociateVPCComment
  = AssociateVPCComment String
```

##### Instances
``` purescript
Newtype AssociateVPCComment _
```

#### `AssociateVPCWithHostedZoneRequest`

``` purescript
newtype AssociateVPCWithHostedZoneRequest
  = AssociateVPCWithHostedZoneRequest { "HostedZoneId" :: ResourceId, "VPC" :: VPC, "Comment" :: NullOrUndefined (AssociateVPCComment) }
```

<p>A complex type that contains information about the request to associate a VPC with a private hosted zone.</p>

##### Instances
``` purescript
Newtype AssociateVPCWithHostedZoneRequest _
```

#### `AssociateVPCWithHostedZoneResponse`

``` purescript
newtype AssociateVPCWithHostedZoneResponse
  = AssociateVPCWithHostedZoneResponse { "ChangeInfo" :: ChangeInfo }
```

<p>A complex type that contains the response information for the <code>AssociateVPCWithHostedZone</code> request.</p>

##### Instances
``` purescript
Newtype AssociateVPCWithHostedZoneResponse _
```

#### `Change`

``` purescript
newtype Change
  = Change { "Action" :: ChangeAction, "ResourceRecordSet" :: ResourceRecordSet }
```

<p>The information for each resource record set that you want to change.</p>

##### Instances
``` purescript
Newtype Change _
```

#### `ChangeAction`

``` purescript
newtype ChangeAction
  = ChangeAction String
```

##### Instances
``` purescript
Newtype ChangeAction _
```

#### `ChangeBatch`

``` purescript
newtype ChangeBatch
  = ChangeBatch { "Comment" :: NullOrUndefined (ResourceDescription), "Changes" :: Changes }
```

<p>The information for a change request.</p>

##### Instances
``` purescript
Newtype ChangeBatch _
```

#### `ChangeInfo`

``` purescript
newtype ChangeInfo
  = ChangeInfo { "Id" :: ResourceId, "Status" :: ChangeStatus, "SubmittedAt" :: TimeStamp, "Comment" :: NullOrUndefined (ResourceDescription) }
```

<p>A complex type that describes change information about changes made to your hosted zone.</p>

##### Instances
``` purescript
Newtype ChangeInfo _
```

#### `ChangeResourceRecordSetsRequest`

``` purescript
newtype ChangeResourceRecordSetsRequest
  = ChangeResourceRecordSetsRequest { "HostedZoneId" :: ResourceId, "ChangeBatch" :: ChangeBatch }
```

<p>A complex type that contains change information for the resource record set.</p>

##### Instances
``` purescript
Newtype ChangeResourceRecordSetsRequest _
```

#### `ChangeResourceRecordSetsResponse`

``` purescript
newtype ChangeResourceRecordSetsResponse
  = ChangeResourceRecordSetsResponse { "ChangeInfo" :: ChangeInfo }
```

<p>A complex type containing the response for the request.</p>

##### Instances
``` purescript
Newtype ChangeResourceRecordSetsResponse _
```

#### `ChangeStatus`

``` purescript
newtype ChangeStatus
  = ChangeStatus String
```

##### Instances
``` purescript
Newtype ChangeStatus _
```

#### `ChangeTagsForResourceRequest`

``` purescript
newtype ChangeTagsForResourceRequest
  = ChangeTagsForResourceRequest { "ResourceType" :: TagResourceType, "ResourceId" :: TagResourceId, "AddTags" :: NullOrUndefined (TagList), "RemoveTagKeys" :: NullOrUndefined (TagKeyList) }
```

<p>A complex type that contains information about the tags that you want to add, edit, or delete.</p>

##### Instances
``` purescript
Newtype ChangeTagsForResourceRequest _
```

#### `ChangeTagsForResourceResponse`

``` purescript
newtype ChangeTagsForResourceResponse
  = ChangeTagsForResourceResponse {  }
```

<p>Empty response for the request.</p>

##### Instances
``` purescript
Newtype ChangeTagsForResourceResponse _
```

#### `Changes`

``` purescript
newtype Changes
  = Changes (Array Change)
```

##### Instances
``` purescript
Newtype Changes _
```

#### `CheckerIpRanges`

``` purescript
newtype CheckerIpRanges
  = CheckerIpRanges (Array IPAddressCidr)
```

##### Instances
``` purescript
Newtype CheckerIpRanges _
```

#### `ChildHealthCheckList`

``` purescript
newtype ChildHealthCheckList
  = ChildHealthCheckList (Array HealthCheckId)
```

##### Instances
``` purescript
Newtype ChildHealthCheckList _
```

#### `CloudWatchAlarmConfiguration`

``` purescript
newtype CloudWatchAlarmConfiguration
  = CloudWatchAlarmConfiguration { "EvaluationPeriods" :: EvaluationPeriods, "Threshold" :: Threshold, "ComparisonOperator" :: ComparisonOperator, "Period" :: Period, "MetricName" :: MetricName, "Namespace" :: Namespace, "Statistic" :: Statistic, "Dimensions" :: NullOrUndefined (DimensionList) }
```

<p>A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.</p>

##### Instances
``` purescript
Newtype CloudWatchAlarmConfiguration _
```

#### `CloudWatchLogsLogGroupArn`

``` purescript
newtype CloudWatchLogsLogGroupArn
  = CloudWatchLogsLogGroupArn String
```

##### Instances
``` purescript
Newtype CloudWatchLogsLogGroupArn _
```

#### `CloudWatchRegion`

``` purescript
newtype CloudWatchRegion
  = CloudWatchRegion String
```

##### Instances
``` purescript
Newtype CloudWatchRegion _
```

#### `ComparisonOperator`

``` purescript
newtype ComparisonOperator
  = ComparisonOperator String
```

##### Instances
``` purescript
Newtype ComparisonOperator _
```

#### `ConcurrentModification`

``` purescript
newtype ConcurrentModification
  = ConcurrentModification { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Another user submitted a request to create, update, or delete the object at the same time that you did. Retry the request. </p>

##### Instances
``` purescript
Newtype ConcurrentModification _
```

#### `ConflictingDomainExists`

``` purescript
newtype ConflictingDomainExists
  = ConflictingDomainExists { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The cause of this error depends on whether you're trying to create a public or a private hosted zone:</p> <ul> <li> <p> <b>Public hosted zone:</b> Two hosted zones that have the same name or that have a parent/child relationship (example.com and test.example.com) can't have any common name servers. You tried to create a hosted zone that has the same name as an existing hosted zone or that's the parent or child of an existing hosted zone, and you specified a delegation set that shares one or more name servers with the existing hosted zone. For more information, see <a>CreateReusableDelegationSet</a>.</p> </li> <li> <p> <b>Private hosted zone:</b> You specified an Amazon VPC that you're already using for another hosted zone, and the domain that you specified for one of the hosted zones is a subdomain of the domain that you specified for the other hosted zone. For example, you can't use the same Amazon VPC for the hosted zones for example.com and test.example.com.</p> </li> </ul>

##### Instances
``` purescript
Newtype ConflictingDomainExists _
```

#### `ConflictingTypes`

``` purescript
newtype ConflictingTypes
  = ConflictingTypes { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>You tried to update a traffic policy instance by using a traffic policy version that has a different DNS type than the current type for the instance. You specified the type in the JSON document in the <code>CreateTrafficPolicy</code> or <code>CreateTrafficPolicyVersion</code>request. </p>

##### Instances
``` purescript
Newtype ConflictingTypes _
```

#### `CreateHealthCheckRequest`

``` purescript
newtype CreateHealthCheckRequest
  = CreateHealthCheckRequest { "CallerReference" :: HealthCheckNonce, "HealthCheckConfig" :: HealthCheckConfig }
```

<p>A complex type that contains the health check request information.</p>

##### Instances
``` purescript
Newtype CreateHealthCheckRequest _
```

#### `CreateHealthCheckResponse`

``` purescript
newtype CreateHealthCheckResponse
  = CreateHealthCheckResponse { "HealthCheck" :: HealthCheck, "Location" :: ResourceURI }
```

<p>A complex type containing the response information for the new health check.</p>

##### Instances
``` purescript
Newtype CreateHealthCheckResponse _
```

#### `CreateHostedZoneRequest`

``` purescript
newtype CreateHostedZoneRequest
  = CreateHostedZoneRequest { "Name" :: DNSName, "VPC" :: NullOrUndefined (VPC), "CallerReference" :: Nonce, "HostedZoneConfig" :: NullOrUndefined (HostedZoneConfig), "DelegationSetId" :: NullOrUndefined (ResourceId) }
```

<p>A complex type that contains information about the request to create a hosted zone.</p>

##### Instances
``` purescript
Newtype CreateHostedZoneRequest _
```

#### `CreateHostedZoneResponse`

``` purescript
newtype CreateHostedZoneResponse
  = CreateHostedZoneResponse { "HostedZone" :: HostedZone, "ChangeInfo" :: ChangeInfo, "DelegationSet" :: DelegationSet, "VPC" :: NullOrUndefined (VPC), "Location" :: ResourceURI }
```

<p>A complex type containing the response information for the hosted zone.</p>

##### Instances
``` purescript
Newtype CreateHostedZoneResponse _
```

#### `CreateQueryLoggingConfigRequest`

``` purescript
newtype CreateQueryLoggingConfigRequest
  = CreateQueryLoggingConfigRequest { "HostedZoneId" :: ResourceId, "CloudWatchLogsLogGroupArn" :: CloudWatchLogsLogGroupArn }
```

##### Instances
``` purescript
Newtype CreateQueryLoggingConfigRequest _
```

#### `CreateQueryLoggingConfigResponse`

``` purescript
newtype CreateQueryLoggingConfigResponse
  = CreateQueryLoggingConfigResponse { "QueryLoggingConfig" :: QueryLoggingConfig, "Location" :: ResourceURI }
```

##### Instances
``` purescript
Newtype CreateQueryLoggingConfigResponse _
```

#### `CreateReusableDelegationSetRequest`

``` purescript
newtype CreateReusableDelegationSetRequest
  = CreateReusableDelegationSetRequest { "CallerReference" :: Nonce, "HostedZoneId" :: NullOrUndefined (ResourceId) }
```

##### Instances
``` purescript
Newtype CreateReusableDelegationSetRequest _
```

#### `CreateReusableDelegationSetResponse`

``` purescript
newtype CreateReusableDelegationSetResponse
  = CreateReusableDelegationSetResponse { "DelegationSet" :: DelegationSet, "Location" :: ResourceURI }
```

##### Instances
``` purescript
Newtype CreateReusableDelegationSetResponse _
```

#### `CreateTrafficPolicyInstanceRequest`

``` purescript
newtype CreateTrafficPolicyInstanceRequest
  = CreateTrafficPolicyInstanceRequest { "HostedZoneId" :: ResourceId, "Name" :: DNSName, "TTL" :: TTL, "TrafficPolicyId" :: TrafficPolicyId, "TrafficPolicyVersion" :: TrafficPolicyVersion }
```

<p>A complex type that contains information about the resource record sets that you want to create based on a specified traffic policy.</p>

##### Instances
``` purescript
Newtype CreateTrafficPolicyInstanceRequest _
```

#### `CreateTrafficPolicyInstanceResponse`

``` purescript
newtype CreateTrafficPolicyInstanceResponse
  = CreateTrafficPolicyInstanceResponse { "TrafficPolicyInstance" :: TrafficPolicyInstance, "Location" :: ResourceURI }
```

<p>A complex type that contains the response information for the <code>CreateTrafficPolicyInstance</code> request.</p>

##### Instances
``` purescript
Newtype CreateTrafficPolicyInstanceResponse _
```

#### `CreateTrafficPolicyRequest`

``` purescript
newtype CreateTrafficPolicyRequest
  = CreateTrafficPolicyRequest { "Name" :: TrafficPolicyName, "Document" :: TrafficPolicyDocument, "Comment" :: NullOrUndefined (TrafficPolicyComment) }
```

<p>A complex type that contains information about the traffic policy that you want to create.</p>

##### Instances
``` purescript
Newtype CreateTrafficPolicyRequest _
```

#### `CreateTrafficPolicyResponse`

``` purescript
newtype CreateTrafficPolicyResponse
  = CreateTrafficPolicyResponse { "TrafficPolicy" :: TrafficPolicy, "Location" :: ResourceURI }
```

<p>A complex type that contains the response information for the <code>CreateTrafficPolicy</code> request.</p>

##### Instances
``` purescript
Newtype CreateTrafficPolicyResponse _
```

#### `CreateTrafficPolicyVersionRequest`

``` purescript
newtype CreateTrafficPolicyVersionRequest
  = CreateTrafficPolicyVersionRequest { "Id" :: TrafficPolicyId, "Document" :: TrafficPolicyDocument, "Comment" :: NullOrUndefined (TrafficPolicyComment) }
```

<p>A complex type that contains information about the traffic policy that you want to create a new version for.</p>

##### Instances
``` purescript
Newtype CreateTrafficPolicyVersionRequest _
```

#### `CreateTrafficPolicyVersionResponse`

``` purescript
newtype CreateTrafficPolicyVersionResponse
  = CreateTrafficPolicyVersionResponse { "TrafficPolicy" :: TrafficPolicy, "Location" :: ResourceURI }
```

<p>A complex type that contains the response information for the <code>CreateTrafficPolicyVersion</code> request.</p>

##### Instances
``` purescript
Newtype CreateTrafficPolicyVersionResponse _
```

#### `CreateVPCAssociationAuthorizationRequest`

``` purescript
newtype CreateVPCAssociationAuthorizationRequest
  = CreateVPCAssociationAuthorizationRequest { "HostedZoneId" :: ResourceId, "VPC" :: VPC }
```

<p>A complex type that contains information about the request to authorize associating a VPC with your private hosted zone. Authorization is only required when a private hosted zone and a VPC were created by using different accounts.</p>

##### Instances
``` purescript
Newtype CreateVPCAssociationAuthorizationRequest _
```

#### `CreateVPCAssociationAuthorizationResponse`

``` purescript
newtype CreateVPCAssociationAuthorizationResponse
  = CreateVPCAssociationAuthorizationResponse { "HostedZoneId" :: ResourceId, "VPC" :: VPC }
```

<p>A complex type that contains the response information from a <code>CreateVPCAssociationAuthorization</code> request.</p>

##### Instances
``` purescript
Newtype CreateVPCAssociationAuthorizationResponse _
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

#### `DNSRCode`

``` purescript
newtype DNSRCode
  = DNSRCode String
```

##### Instances
``` purescript
Newtype DNSRCode _
```

#### `DelegationSet`

``` purescript
newtype DelegationSet
  = DelegationSet { "Id" :: NullOrUndefined (ResourceId), "CallerReference" :: NullOrUndefined (Nonce), "NameServers" :: DelegationSetNameServers }
```

<p>A complex type that lists the name servers in a delegation set, as well as the <code>CallerReference</code> and the <code>ID</code> for the delegation set.</p>

##### Instances
``` purescript
Newtype DelegationSet _
```

#### `DelegationSetAlreadyCreated`

``` purescript
newtype DelegationSetAlreadyCreated
  = DelegationSetAlreadyCreated { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>A delegation set with the same owner and caller reference combination has already been created.</p>

##### Instances
``` purescript
Newtype DelegationSetAlreadyCreated _
```

#### `DelegationSetAlreadyReusable`

``` purescript
newtype DelegationSetAlreadyReusable
  = DelegationSetAlreadyReusable { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified delegation set has already been marked as reusable.</p>

##### Instances
``` purescript
Newtype DelegationSetAlreadyReusable _
```

#### `DelegationSetInUse`

``` purescript
newtype DelegationSetInUse
  = DelegationSetInUse { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified delegation contains associated hosted zones which must be deleted before the reusable delegation set can be deleted.</p>

##### Instances
``` purescript
Newtype DelegationSetInUse _
```

#### `DelegationSetNameServers`

``` purescript
newtype DelegationSetNameServers
  = DelegationSetNameServers (Array DNSName)
```

##### Instances
``` purescript
Newtype DelegationSetNameServers _
```

#### `DelegationSetNotAvailable`

``` purescript
newtype DelegationSetNotAvailable
  = DelegationSetNotAvailable { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>You can create a hosted zone that has the same name as an existing hosted zone (example.com is common), but there is a limit to the number of hosted zones that have the same name. If you get this error, Amazon Route 53 has reached that limit. If you own the domain name and Amazon Route 53 generates this error, contact Customer Support.</p>

##### Instances
``` purescript
Newtype DelegationSetNotAvailable _
```

#### `DelegationSetNotReusable`

``` purescript
newtype DelegationSetNotReusable
  = DelegationSetNotReusable { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>A reusable delegation set with the specified ID does not exist.</p>

##### Instances
``` purescript
Newtype DelegationSetNotReusable _
```

#### `DelegationSets`

``` purescript
newtype DelegationSets
  = DelegationSets (Array DelegationSet)
```

##### Instances
``` purescript
Newtype DelegationSets _
```

#### `DeleteHealthCheckRequest`

``` purescript
newtype DeleteHealthCheckRequest
  = DeleteHealthCheckRequest { "HealthCheckId" :: HealthCheckId }
```

<p>This action deletes a health check.</p>

##### Instances
``` purescript
Newtype DeleteHealthCheckRequest _
```

#### `DeleteHealthCheckResponse`

``` purescript
newtype DeleteHealthCheckResponse
  = DeleteHealthCheckResponse {  }
```

<p>An empty element.</p>

##### Instances
``` purescript
Newtype DeleteHealthCheckResponse _
```

#### `DeleteHostedZoneRequest`

``` purescript
newtype DeleteHostedZoneRequest
  = DeleteHostedZoneRequest { "Id" :: ResourceId }
```

<p>A request to delete a hosted zone.</p>

##### Instances
``` purescript
Newtype DeleteHostedZoneRequest _
```

#### `DeleteHostedZoneResponse`

``` purescript
newtype DeleteHostedZoneResponse
  = DeleteHostedZoneResponse { "ChangeInfo" :: ChangeInfo }
```

<p>A complex type that contains the response to a <code>DeleteHostedZone</code> request.</p>

##### Instances
``` purescript
Newtype DeleteHostedZoneResponse _
```

#### `DeleteQueryLoggingConfigRequest`

``` purescript
newtype DeleteQueryLoggingConfigRequest
  = DeleteQueryLoggingConfigRequest { "Id" :: QueryLoggingConfigId }
```

##### Instances
``` purescript
Newtype DeleteQueryLoggingConfigRequest _
```

#### `DeleteQueryLoggingConfigResponse`

``` purescript
newtype DeleteQueryLoggingConfigResponse
  = DeleteQueryLoggingConfigResponse {  }
```

##### Instances
``` purescript
Newtype DeleteQueryLoggingConfigResponse _
```

#### `DeleteReusableDelegationSetRequest`

``` purescript
newtype DeleteReusableDelegationSetRequest
  = DeleteReusableDelegationSetRequest { "Id" :: ResourceId }
```

<p>A request to delete a reusable delegation set.</p>

##### Instances
``` purescript
Newtype DeleteReusableDelegationSetRequest _
```

#### `DeleteReusableDelegationSetResponse`

``` purescript
newtype DeleteReusableDelegationSetResponse
  = DeleteReusableDelegationSetResponse {  }
```

<p>An empty element.</p>

##### Instances
``` purescript
Newtype DeleteReusableDelegationSetResponse _
```

#### `DeleteTrafficPolicyInstanceRequest`

``` purescript
newtype DeleteTrafficPolicyInstanceRequest
  = DeleteTrafficPolicyInstanceRequest { "Id" :: TrafficPolicyInstanceId }
```

<p>A request to delete a specified traffic policy instance.</p>

##### Instances
``` purescript
Newtype DeleteTrafficPolicyInstanceRequest _
```

#### `DeleteTrafficPolicyInstanceResponse`

``` purescript
newtype DeleteTrafficPolicyInstanceResponse
  = DeleteTrafficPolicyInstanceResponse {  }
```

<p>An empty element.</p>

##### Instances
``` purescript
Newtype DeleteTrafficPolicyInstanceResponse _
```

#### `DeleteTrafficPolicyRequest`

``` purescript
newtype DeleteTrafficPolicyRequest
  = DeleteTrafficPolicyRequest { "Id" :: TrafficPolicyId, "Version" :: TrafficPolicyVersion }
```

<p>A request to delete a specified traffic policy version.</p>

##### Instances
``` purescript
Newtype DeleteTrafficPolicyRequest _
```

#### `DeleteTrafficPolicyResponse`

``` purescript
newtype DeleteTrafficPolicyResponse
  = DeleteTrafficPolicyResponse {  }
```

<p>An empty element.</p>

##### Instances
``` purescript
Newtype DeleteTrafficPolicyResponse _
```

#### `DeleteVPCAssociationAuthorizationRequest`

``` purescript
newtype DeleteVPCAssociationAuthorizationRequest
  = DeleteVPCAssociationAuthorizationRequest { "HostedZoneId" :: ResourceId, "VPC" :: VPC }
```

<p>A complex type that contains information about the request to remove authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account. </p>

##### Instances
``` purescript
Newtype DeleteVPCAssociationAuthorizationRequest _
```

#### `DeleteVPCAssociationAuthorizationResponse`

``` purescript
newtype DeleteVPCAssociationAuthorizationResponse
  = DeleteVPCAssociationAuthorizationResponse {  }
```

<p>Empty response for the request.</p>

##### Instances
``` purescript
Newtype DeleteVPCAssociationAuthorizationResponse _
```

#### `Dimension`

``` purescript
newtype Dimension
  = Dimension { "Name" :: DimensionField, "Value" :: DimensionField }
```

<p>For the metric that the CloudWatch alarm is associated with, a complex type that contains information about one dimension.</p>

##### Instances
``` purescript
Newtype Dimension _
```

#### `DimensionField`

``` purescript
newtype DimensionField
  = DimensionField String
```

##### Instances
``` purescript
Newtype DimensionField _
```

#### `DimensionList`

``` purescript
newtype DimensionList
  = DimensionList (Array Dimension)
```

##### Instances
``` purescript
Newtype DimensionList _
```

#### `DisassociateVPCComment`

``` purescript
newtype DisassociateVPCComment
  = DisassociateVPCComment String
```

##### Instances
``` purescript
Newtype DisassociateVPCComment _
```

#### `DisassociateVPCFromHostedZoneRequest`

``` purescript
newtype DisassociateVPCFromHostedZoneRequest
  = DisassociateVPCFromHostedZoneRequest { "HostedZoneId" :: ResourceId, "VPC" :: VPC, "Comment" :: NullOrUndefined (DisassociateVPCComment) }
```

<p>A complex type that contains information about the VPC that you want to disassociate from a specified private hosted zone.</p>

##### Instances
``` purescript
Newtype DisassociateVPCFromHostedZoneRequest _
```

#### `DisassociateVPCFromHostedZoneResponse`

``` purescript
newtype DisassociateVPCFromHostedZoneResponse
  = DisassociateVPCFromHostedZoneResponse { "ChangeInfo" :: ChangeInfo }
```

<p>A complex type that contains the response information for the disassociate request.</p>

##### Instances
``` purescript
Newtype DisassociateVPCFromHostedZoneResponse _
```

#### `EnableSNI`

``` purescript
newtype EnableSNI
  = EnableSNI Boolean
```

##### Instances
``` purescript
Newtype EnableSNI _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `ErrorMessages`

``` purescript
newtype ErrorMessages
  = ErrorMessages (Array ErrorMessage)
```

##### Instances
``` purescript
Newtype ErrorMessages _
```

#### `EvaluationPeriods`

``` purescript
newtype EvaluationPeriods
  = EvaluationPeriods Int
```

##### Instances
``` purescript
Newtype EvaluationPeriods _
```

#### `FailureThreshold`

``` purescript
newtype FailureThreshold
  = FailureThreshold Int
```

##### Instances
``` purescript
Newtype FailureThreshold _
```

#### `FullyQualifiedDomainName`

``` purescript
newtype FullyQualifiedDomainName
  = FullyQualifiedDomainName String
```

##### Instances
``` purescript
Newtype FullyQualifiedDomainName _
```

#### `GeoLocation`

``` purescript
newtype GeoLocation
  = GeoLocation { "ContinentCode" :: NullOrUndefined (GeoLocationContinentCode), "CountryCode" :: NullOrUndefined (GeoLocationCountryCode), "SubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode) }
```

<p>A complex type that contains information about a geo location.</p>

##### Instances
``` purescript
Newtype GeoLocation _
```

#### `GeoLocationContinentCode`

``` purescript
newtype GeoLocationContinentCode
  = GeoLocationContinentCode String
```

##### Instances
``` purescript
Newtype GeoLocationContinentCode _
```

#### `GeoLocationContinentName`

``` purescript
newtype GeoLocationContinentName
  = GeoLocationContinentName String
```

##### Instances
``` purescript
Newtype GeoLocationContinentName _
```

#### `GeoLocationCountryCode`

``` purescript
newtype GeoLocationCountryCode
  = GeoLocationCountryCode String
```

##### Instances
``` purescript
Newtype GeoLocationCountryCode _
```

#### `GeoLocationCountryName`

``` purescript
newtype GeoLocationCountryName
  = GeoLocationCountryName String
```

##### Instances
``` purescript
Newtype GeoLocationCountryName _
```

#### `GeoLocationDetails`

``` purescript
newtype GeoLocationDetails
  = GeoLocationDetails { "ContinentCode" :: NullOrUndefined (GeoLocationContinentCode), "ContinentName" :: NullOrUndefined (GeoLocationContinentName), "CountryCode" :: NullOrUndefined (GeoLocationCountryCode), "CountryName" :: NullOrUndefined (GeoLocationCountryName), "SubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode), "SubdivisionName" :: NullOrUndefined (GeoLocationSubdivisionName) }
```

<p>A complex type that contains the codes and full continent, country, and subdivision names for the specified <code>geolocation</code> code.</p>

##### Instances
``` purescript
Newtype GeoLocationDetails _
```

#### `GeoLocationDetailsList`

``` purescript
newtype GeoLocationDetailsList
  = GeoLocationDetailsList (Array GeoLocationDetails)
```

##### Instances
``` purescript
Newtype GeoLocationDetailsList _
```

#### `GeoLocationSubdivisionCode`

``` purescript
newtype GeoLocationSubdivisionCode
  = GeoLocationSubdivisionCode String
```

##### Instances
``` purescript
Newtype GeoLocationSubdivisionCode _
```

#### `GeoLocationSubdivisionName`

``` purescript
newtype GeoLocationSubdivisionName
  = GeoLocationSubdivisionName String
```

##### Instances
``` purescript
Newtype GeoLocationSubdivisionName _
```

#### `GetAccountLimitRequest`

``` purescript
newtype GetAccountLimitRequest
  = GetAccountLimitRequest { "Type" :: AccountLimitType }
```

<p>A complex type that contains information about the request to create a hosted zone.</p>

##### Instances
``` purescript
Newtype GetAccountLimitRequest _
```

#### `GetAccountLimitResponse`

``` purescript
newtype GetAccountLimitResponse
  = GetAccountLimitResponse { "Limit" :: AccountLimit, "Count" :: UsageCount }
```

<p>A complex type that contains the requested limit. </p>

##### Instances
``` purescript
Newtype GetAccountLimitResponse _
```

#### `GetChangeRequest`

``` purescript
newtype GetChangeRequest
  = GetChangeRequest { "Id" :: ResourceId }
```

<p>The input for a GetChange request.</p>

##### Instances
``` purescript
Newtype GetChangeRequest _
```

#### `GetChangeResponse`

``` purescript
newtype GetChangeResponse
  = GetChangeResponse { "ChangeInfo" :: ChangeInfo }
```

<p>A complex type that contains the <code>ChangeInfo</code> element.</p>

##### Instances
``` purescript
Newtype GetChangeResponse _
```

#### `GetCheckerIpRangesRequest`

``` purescript
newtype GetCheckerIpRangesRequest
  = GetCheckerIpRangesRequest {  }
```

##### Instances
``` purescript
Newtype GetCheckerIpRangesRequest _
```

#### `GetCheckerIpRangesResponse`

``` purescript
newtype GetCheckerIpRangesResponse
  = GetCheckerIpRangesResponse { "CheckerIpRanges" :: CheckerIpRanges }
```

##### Instances
``` purescript
Newtype GetCheckerIpRangesResponse _
```

#### `GetGeoLocationRequest`

``` purescript
newtype GetGeoLocationRequest
  = GetGeoLocationRequest { "ContinentCode" :: NullOrUndefined (GeoLocationContinentCode), "CountryCode" :: NullOrUndefined (GeoLocationCountryCode), "SubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode) }
```

<p>A request for information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.</p>

##### Instances
``` purescript
Newtype GetGeoLocationRequest _
```

#### `GetGeoLocationResponse`

``` purescript
newtype GetGeoLocationResponse
  = GetGeoLocationResponse { "GeoLocationDetails" :: GeoLocationDetails }
```

<p>A complex type that contains the response information for the specified geolocation code.</p>

##### Instances
``` purescript
Newtype GetGeoLocationResponse _
```

#### `GetHealthCheckCountRequest`

``` purescript
newtype GetHealthCheckCountRequest
  = GetHealthCheckCountRequest {  }
```

<p>A request for the number of health checks that are associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype GetHealthCheckCountRequest _
```

#### `GetHealthCheckCountResponse`

``` purescript
newtype GetHealthCheckCountResponse
  = GetHealthCheckCountResponse { "HealthCheckCount" :: HealthCheckCount }
```

<p>A complex type that contains the response to a <code>GetHealthCheckCount</code> request.</p>

##### Instances
``` purescript
Newtype GetHealthCheckCountResponse _
```

#### `GetHealthCheckLastFailureReasonRequest`

``` purescript
newtype GetHealthCheckLastFailureReasonRequest
  = GetHealthCheckLastFailureReasonRequest { "HealthCheckId" :: HealthCheckId }
```

<p>A request for the reason that a health check failed most recently.</p>

##### Instances
``` purescript
Newtype GetHealthCheckLastFailureReasonRequest _
```

#### `GetHealthCheckLastFailureReasonResponse`

``` purescript
newtype GetHealthCheckLastFailureReasonResponse
  = GetHealthCheckLastFailureReasonResponse { "HealthCheckObservations" :: HealthCheckObservations }
```

<p>A complex type that contains the response to a <code>GetHealthCheckLastFailureReason</code> request.</p>

##### Instances
``` purescript
Newtype GetHealthCheckLastFailureReasonResponse _
```

#### `GetHealthCheckRequest`

``` purescript
newtype GetHealthCheckRequest
  = GetHealthCheckRequest { "HealthCheckId" :: HealthCheckId }
```

<p>A request to get information about a specified health check. </p>

##### Instances
``` purescript
Newtype GetHealthCheckRequest _
```

#### `GetHealthCheckResponse`

``` purescript
newtype GetHealthCheckResponse
  = GetHealthCheckResponse { "HealthCheck" :: HealthCheck }
```

<p>A complex type that contains the response to a <code>GetHealthCheck</code> request.</p>

##### Instances
``` purescript
Newtype GetHealthCheckResponse _
```

#### `GetHealthCheckStatusRequest`

``` purescript
newtype GetHealthCheckStatusRequest
  = GetHealthCheckStatusRequest { "HealthCheckId" :: HealthCheckId }
```

<p>A request to get the status for a health check.</p>

##### Instances
``` purescript
Newtype GetHealthCheckStatusRequest _
```

#### `GetHealthCheckStatusResponse`

``` purescript
newtype GetHealthCheckStatusResponse
  = GetHealthCheckStatusResponse { "HealthCheckObservations" :: HealthCheckObservations }
```

<p>A complex type that contains the response to a <code>GetHealthCheck</code> request.</p>

##### Instances
``` purescript
Newtype GetHealthCheckStatusResponse _
```

#### `GetHostedZoneCountRequest`

``` purescript
newtype GetHostedZoneCountRequest
  = GetHostedZoneCountRequest {  }
```

<p>A request to retrieve a count of all the hosted zones that are associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype GetHostedZoneCountRequest _
```

#### `GetHostedZoneCountResponse`

``` purescript
newtype GetHostedZoneCountResponse
  = GetHostedZoneCountResponse { "HostedZoneCount" :: HostedZoneCount }
```

<p>A complex type that contains the response to a <code>GetHostedZoneCount</code> request.</p>

##### Instances
``` purescript
Newtype GetHostedZoneCountResponse _
```

#### `GetHostedZoneLimitRequest`

``` purescript
newtype GetHostedZoneLimitRequest
  = GetHostedZoneLimitRequest { "Type" :: HostedZoneLimitType, "HostedZoneId" :: ResourceId }
```

<p>A complex type that contains information about the request to create a hosted zone.</p>

##### Instances
``` purescript
Newtype GetHostedZoneLimitRequest _
```

#### `GetHostedZoneLimitResponse`

``` purescript
newtype GetHostedZoneLimitResponse
  = GetHostedZoneLimitResponse { "Limit" :: HostedZoneLimit, "Count" :: UsageCount }
```

<p>A complex type that contains the requested limit. </p>

##### Instances
``` purescript
Newtype GetHostedZoneLimitResponse _
```

#### `GetHostedZoneRequest`

``` purescript
newtype GetHostedZoneRequest
  = GetHostedZoneRequest { "Id" :: ResourceId }
```

<p>A request to get information about a specified hosted zone. </p>

##### Instances
``` purescript
Newtype GetHostedZoneRequest _
```

#### `GetHostedZoneResponse`

``` purescript
newtype GetHostedZoneResponse
  = GetHostedZoneResponse { "HostedZone" :: HostedZone, "DelegationSet" :: NullOrUndefined (DelegationSet), "VPCs" :: NullOrUndefined (VPCs) }
```

<p>A complex type that contain the response to a <code>GetHostedZone</code> request.</p>

##### Instances
``` purescript
Newtype GetHostedZoneResponse _
```

#### `GetQueryLoggingConfigRequest`

``` purescript
newtype GetQueryLoggingConfigRequest
  = GetQueryLoggingConfigRequest { "Id" :: QueryLoggingConfigId }
```

##### Instances
``` purescript
Newtype GetQueryLoggingConfigRequest _
```

#### `GetQueryLoggingConfigResponse`

``` purescript
newtype GetQueryLoggingConfigResponse
  = GetQueryLoggingConfigResponse { "QueryLoggingConfig" :: QueryLoggingConfig }
```

##### Instances
``` purescript
Newtype GetQueryLoggingConfigResponse _
```

#### `GetReusableDelegationSetLimitRequest`

``` purescript
newtype GetReusableDelegationSetLimitRequest
  = GetReusableDelegationSetLimitRequest { "Type" :: ReusableDelegationSetLimitType, "DelegationSetId" :: ResourceId }
```

<p>A complex type that contains information about the request to create a hosted zone.</p>

##### Instances
``` purescript
Newtype GetReusableDelegationSetLimitRequest _
```

#### `GetReusableDelegationSetLimitResponse`

``` purescript
newtype GetReusableDelegationSetLimitResponse
  = GetReusableDelegationSetLimitResponse { "Limit" :: ReusableDelegationSetLimit, "Count" :: UsageCount }
```

<p>A complex type that contains the requested limit. </p>

##### Instances
``` purescript
Newtype GetReusableDelegationSetLimitResponse _
```

#### `GetReusableDelegationSetRequest`

``` purescript
newtype GetReusableDelegationSetRequest
  = GetReusableDelegationSetRequest { "Id" :: ResourceId }
```

<p>A request to get information about a specified reusable delegation set.</p>

##### Instances
``` purescript
Newtype GetReusableDelegationSetRequest _
```

#### `GetReusableDelegationSetResponse`

``` purescript
newtype GetReusableDelegationSetResponse
  = GetReusableDelegationSetResponse { "DelegationSet" :: DelegationSet }
```

<p>A complex type that contains the response to the <code>GetReusableDelegationSet</code> request.</p>

##### Instances
``` purescript
Newtype GetReusableDelegationSetResponse _
```

#### `GetTrafficPolicyInstanceCountRequest`

``` purescript
newtype GetTrafficPolicyInstanceCountRequest
  = GetTrafficPolicyInstanceCountRequest {  }
```

<p>Request to get the number of traffic policy instances that are associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype GetTrafficPolicyInstanceCountRequest _
```

#### `GetTrafficPolicyInstanceCountResponse`

``` purescript
newtype GetTrafficPolicyInstanceCountResponse
  = GetTrafficPolicyInstanceCountResponse { "TrafficPolicyInstanceCount" :: TrafficPolicyInstanceCount }
```

<p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>

##### Instances
``` purescript
Newtype GetTrafficPolicyInstanceCountResponse _
```

#### `GetTrafficPolicyInstanceRequest`

``` purescript
newtype GetTrafficPolicyInstanceRequest
  = GetTrafficPolicyInstanceRequest { "Id" :: TrafficPolicyInstanceId }
```

<p>Gets information about a specified traffic policy instance.</p>

##### Instances
``` purescript
Newtype GetTrafficPolicyInstanceRequest _
```

#### `GetTrafficPolicyInstanceResponse`

``` purescript
newtype GetTrafficPolicyInstanceResponse
  = GetTrafficPolicyInstanceResponse { "TrafficPolicyInstance" :: TrafficPolicyInstance }
```

<p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>

##### Instances
``` purescript
Newtype GetTrafficPolicyInstanceResponse _
```

#### `GetTrafficPolicyRequest`

``` purescript
newtype GetTrafficPolicyRequest
  = GetTrafficPolicyRequest { "Id" :: TrafficPolicyId, "Version" :: TrafficPolicyVersion }
```

<p>Gets information about a specific traffic policy version.</p>

##### Instances
``` purescript
Newtype GetTrafficPolicyRequest _
```

#### `GetTrafficPolicyResponse`

``` purescript
newtype GetTrafficPolicyResponse
  = GetTrafficPolicyResponse { "TrafficPolicy" :: TrafficPolicy }
```

<p>A complex type that contains the response information for the request.</p>

##### Instances
``` purescript
Newtype GetTrafficPolicyResponse _
```

#### `HealthCheck`

``` purescript
newtype HealthCheck
  = HealthCheck { "Id" :: HealthCheckId, "CallerReference" :: HealthCheckNonce, "LinkedService" :: NullOrUndefined (LinkedService), "HealthCheckConfig" :: HealthCheckConfig, "HealthCheckVersion" :: HealthCheckVersion, "CloudWatchAlarmConfiguration" :: NullOrUndefined (CloudWatchAlarmConfiguration) }
```

<p>A complex type that contains information about one health check that is associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype HealthCheck _
```

#### `HealthCheckAlreadyExists`

``` purescript
newtype HealthCheckAlreadyExists
  = HealthCheckAlreadyExists { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> The health check you're attempting to create already exists. Amazon Route 53 returns this error when you submit a request that has the following values:</p> <ul> <li> <p>The same value for <code>CallerReference</code> as an existing health check, and one or more values that differ from the existing health check that has the same caller reference.</p> </li> <li> <p>The same value for <code>CallerReference</code> as a health check that you created and later deleted, regardless of the other settings in the request.</p> </li> </ul>

##### Instances
``` purescript
Newtype HealthCheckAlreadyExists _
```

#### `HealthCheckConfig`

``` purescript
newtype HealthCheckConfig
  = HealthCheckConfig { "IPAddress" :: NullOrUndefined (IPAddress), "Port" :: NullOrUndefined (Port), "Type" :: HealthCheckType, "ResourcePath" :: NullOrUndefined (ResourcePath), "FullyQualifiedDomainName" :: NullOrUndefined (FullyQualifiedDomainName), "SearchString" :: NullOrUndefined (SearchString), "RequestInterval" :: NullOrUndefined (RequestInterval), "FailureThreshold" :: NullOrUndefined (FailureThreshold), "MeasureLatency" :: NullOrUndefined (MeasureLatency), "Inverted" :: NullOrUndefined (Inverted), "HealthThreshold" :: NullOrUndefined (HealthThreshold), "ChildHealthChecks" :: NullOrUndefined (ChildHealthCheckList), "EnableSNI" :: NullOrUndefined (EnableSNI), "Regions" :: NullOrUndefined (HealthCheckRegionList), "AlarmIdentifier" :: NullOrUndefined (AlarmIdentifier), "InsufficientDataHealthStatus" :: NullOrUndefined (InsufficientDataHealthStatus) }
```

<p>A complex type that contains information about the health check.</p>

##### Instances
``` purescript
Newtype HealthCheckConfig _
```

#### `HealthCheckCount`

``` purescript
newtype HealthCheckCount
  = HealthCheckCount Number
```

##### Instances
``` purescript
Newtype HealthCheckCount _
```

#### `HealthCheckId`

``` purescript
newtype HealthCheckId
  = HealthCheckId String
```

##### Instances
``` purescript
Newtype HealthCheckId _
```

#### `HealthCheckInUse`

``` purescript
newtype HealthCheckInUse
  = HealthCheckInUse { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This error code is not in use.</p>

##### Instances
``` purescript
Newtype HealthCheckInUse _
```

#### `HealthCheckNonce`

``` purescript
newtype HealthCheckNonce
  = HealthCheckNonce String
```

##### Instances
``` purescript
Newtype HealthCheckNonce _
```

#### `HealthCheckObservation`

``` purescript
newtype HealthCheckObservation
  = HealthCheckObservation { "Region" :: NullOrUndefined (HealthCheckRegion), "IPAddress" :: NullOrUndefined (IPAddress), "StatusReport" :: NullOrUndefined (StatusReport) }
```

<p>A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker.</p>

##### Instances
``` purescript
Newtype HealthCheckObservation _
```

#### `HealthCheckObservations`

``` purescript
newtype HealthCheckObservations
  = HealthCheckObservations (Array HealthCheckObservation)
```

##### Instances
``` purescript
Newtype HealthCheckObservations _
```

#### `HealthCheckRegion`

``` purescript
newtype HealthCheckRegion
  = HealthCheckRegion String
```

##### Instances
``` purescript
Newtype HealthCheckRegion _
```

#### `HealthCheckRegionList`

``` purescript
newtype HealthCheckRegionList
  = HealthCheckRegionList (Array HealthCheckRegion)
```

##### Instances
``` purescript
Newtype HealthCheckRegionList _
```

#### `HealthCheckType`

``` purescript
newtype HealthCheckType
  = HealthCheckType String
```

##### Instances
``` purescript
Newtype HealthCheckType _
```

#### `HealthCheckVersion`

``` purescript
newtype HealthCheckVersion
  = HealthCheckVersion Number
```

##### Instances
``` purescript
Newtype HealthCheckVersion _
```

#### `HealthCheckVersionMismatch`

``` purescript
newtype HealthCheckVersionMismatch
  = HealthCheckVersionMismatch { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The value of <code>HealthCheckVersion</code> in the request doesn't match the value of <code>HealthCheckVersion</code> in the health check.</p>

##### Instances
``` purescript
Newtype HealthCheckVersionMismatch _
```

#### `HealthChecks`

``` purescript
newtype HealthChecks
  = HealthChecks (Array HealthCheck)
```

##### Instances
``` purescript
Newtype HealthChecks _
```

#### `HealthThreshold`

``` purescript
newtype HealthThreshold
  = HealthThreshold Int
```

##### Instances
``` purescript
Newtype HealthThreshold _
```

#### `HostedZone`

``` purescript
newtype HostedZone
  = HostedZone { "Id" :: ResourceId, "Name" :: DNSName, "CallerReference" :: Nonce, "Config" :: NullOrUndefined (HostedZoneConfig), "ResourceRecordSetCount" :: NullOrUndefined (HostedZoneRRSetCount), "LinkedService" :: NullOrUndefined (LinkedService) }
```

<p>A complex type that contains general information about the hosted zone.</p>

##### Instances
``` purescript
Newtype HostedZone _
```

#### `HostedZoneAlreadyExists`

``` purescript
newtype HostedZoneAlreadyExists
  = HostedZoneAlreadyExists { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The hosted zone you're trying to create already exists. Amazon Route 53 returns this error when a hosted zone has already been created with the specified <code>CallerReference</code>.</p>

##### Instances
``` purescript
Newtype HostedZoneAlreadyExists _
```

#### `HostedZoneConfig`

``` purescript
newtype HostedZoneConfig
  = HostedZoneConfig { "Comment" :: NullOrUndefined (ResourceDescription), "PrivateZone" :: NullOrUndefined (IsPrivateZone) }
```

<p>A complex type that contains an optional comment about your hosted zone. If you don't want to specify a comment, omit both the <code>HostedZoneConfig</code> and <code>Comment</code> elements.</p>

##### Instances
``` purescript
Newtype HostedZoneConfig _
```

#### `HostedZoneCount`

``` purescript
newtype HostedZoneCount
  = HostedZoneCount Number
```

##### Instances
``` purescript
Newtype HostedZoneCount _
```

#### `HostedZoneLimit`

``` purescript
newtype HostedZoneLimit
  = HostedZoneLimit { "Type" :: HostedZoneLimitType, "Value" :: LimitValue }
```

<p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>

##### Instances
``` purescript
Newtype HostedZoneLimit _
```

#### `HostedZoneLimitType`

``` purescript
newtype HostedZoneLimitType
  = HostedZoneLimitType String
```

##### Instances
``` purescript
Newtype HostedZoneLimitType _
```

#### `HostedZoneNotEmpty`

``` purescript
newtype HostedZoneNotEmpty
  = HostedZoneNotEmpty { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The hosted zone contains resource records that are not SOA or NS records.</p>

##### Instances
``` purescript
Newtype HostedZoneNotEmpty _
```

#### `HostedZoneNotFound`

``` purescript
newtype HostedZoneNotFound
  = HostedZoneNotFound { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified HostedZone can't be found.</p>

##### Instances
``` purescript
Newtype HostedZoneNotFound _
```

#### `HostedZoneNotPrivate`

``` purescript
newtype HostedZoneNotPrivate
  = HostedZoneNotPrivate { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified hosted zone is a public hosted zone, not a private hosted zone.</p>

##### Instances
``` purescript
Newtype HostedZoneNotPrivate _
```

#### `HostedZoneRRSetCount`

``` purescript
newtype HostedZoneRRSetCount
  = HostedZoneRRSetCount Number
```

##### Instances
``` purescript
Newtype HostedZoneRRSetCount _
```

#### `HostedZones`

``` purescript
newtype HostedZones
  = HostedZones (Array HostedZone)
```

##### Instances
``` purescript
Newtype HostedZones _
```

#### `IPAddress`

``` purescript
newtype IPAddress
  = IPAddress String
```

##### Instances
``` purescript
Newtype IPAddress _
```

#### `IPAddressCidr`

``` purescript
newtype IPAddressCidr
  = IPAddressCidr String
```

##### Instances
``` purescript
Newtype IPAddressCidr _
```

#### `IncompatibleVersion`

``` purescript
newtype IncompatibleVersion
  = IncompatibleVersion { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The resource you're trying to access is unsupported on this Amazon Route 53 endpoint.</p>

##### Instances
``` purescript
Newtype IncompatibleVersion _
```

#### `InsufficientCloudWatchLogsResourcePolicy`

``` purescript
newtype InsufficientCloudWatchLogsResourcePolicy
  = InsufficientCloudWatchLogsResourcePolicy { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Amazon Route 53 doesn't have the permissions required to create log streams and send query logs to log streams. Possible causes include the following:</p> <ul> <li> <p>There is no resource policy that specifies the log group ARN in the value for <code>Resource</code>.</p> </li> <li> <p>The resource policy that includes the log group ARN in the value for <code>Resource</code> doesn't have the necessary permissions.</p> </li> <li> <p>The resource policy hasn't finished propagating yet.</p> </li> </ul>

##### Instances
``` purescript
Newtype InsufficientCloudWatchLogsResourcePolicy _
```

#### `InsufficientDataHealthStatus`

``` purescript
newtype InsufficientDataHealthStatus
  = InsufficientDataHealthStatus String
```

##### Instances
``` purescript
Newtype InsufficientDataHealthStatus _
```

#### `InvalidArgument`

``` purescript
newtype InvalidArgument
  = InvalidArgument { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Parameter name is invalid.</p>

##### Instances
``` purescript
Newtype InvalidArgument _
```

#### `InvalidChangeBatch`

``` purescript
newtype InvalidChangeBatch
  = InvalidChangeBatch { "Messages'" :: NullOrUndefined (ErrorMessages), "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This exception contains a list of messages that might contain one or more error messages. Each error message indicates one error in the change batch.</p>

##### Instances
``` purescript
Newtype InvalidChangeBatch _
```

#### `InvalidDomainName`

``` purescript
newtype InvalidDomainName
  = InvalidDomainName { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified domain name is not valid.</p>

##### Instances
``` purescript
Newtype InvalidDomainName _
```

#### `InvalidInput`

``` purescript
newtype InvalidInput
  = InvalidInput { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The input is not valid.</p>

##### Instances
``` purescript
Newtype InvalidInput _
```

#### `InvalidPaginationToken`

``` purescript
newtype InvalidPaginationToken
  = InvalidPaginationToken { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The value that you specified to get the second or subsequent page of results is invalid.</p>

##### Instances
``` purescript
Newtype InvalidPaginationToken _
```

#### `InvalidTrafficPolicyDocument`

``` purescript
newtype InvalidTrafficPolicyDocument
  = InvalidTrafficPolicyDocument { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The format of the traffic policy document that you specified in the <code>Document</code> element is invalid.</p>

##### Instances
``` purescript
Newtype InvalidTrafficPolicyDocument _
```

#### `InvalidVPCId`

``` purescript
newtype InvalidVPCId
  = InvalidVPCId { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The VPC ID that you specified either isn't a valid ID or the current account is not authorized to access this VPC.</p>

##### Instances
``` purescript
Newtype InvalidVPCId _
```

#### `Inverted`

``` purescript
newtype Inverted
  = Inverted Boolean
```

##### Instances
``` purescript
Newtype Inverted _
```

#### `IsPrivateZone`

``` purescript
newtype IsPrivateZone
  = IsPrivateZone Boolean
```

##### Instances
``` purescript
Newtype IsPrivateZone _
```

#### `LastVPCAssociation`

``` purescript
newtype LastVPCAssociation
  = LastVPCAssociation { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The VPC that you're trying to disassociate from the private hosted zone is the last VPC that is associated with the hosted zone. Amazon Route 53 doesn't support disassociating the last VPC from a hosted zone.</p>

##### Instances
``` purescript
Newtype LastVPCAssociation _
```

#### `LimitValue`

``` purescript
newtype LimitValue
  = LimitValue Number
```

##### Instances
``` purescript
Newtype LimitValue _
```

#### `LimitsExceeded`

``` purescript
newtype LimitsExceeded
  = LimitsExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This operation can't be completed either because the current account has reached the limit on reusable delegation sets that it can create or because you've reached the limit on the number of Amazon VPCs that you can associate with a private hosted zone. To get the current limit on the number of reusable delegation sets, see <a>GetAccountLimit</a>. To get the current limit on the number of Amazon VPCs that you can associate with a private hosted zone, see <a>GetHostedZoneLimit</a>. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>

##### Instances
``` purescript
Newtype LimitsExceeded _
```

#### `LinkedService`

``` purescript
newtype LinkedService
  = LinkedService { "ServicePrincipal" :: NullOrUndefined (ServicePrincipal), "Description" :: NullOrUndefined (ResourceDescription) }
```

<p>If a health check or hosted zone was created by another service, <code>LinkedService</code> is a complex type that describes the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53. </p>

##### Instances
``` purescript
Newtype LinkedService _
```

#### `ListGeoLocationsRequest`

``` purescript
newtype ListGeoLocationsRequest
  = ListGeoLocationsRequest { "StartContinentCode" :: NullOrUndefined (GeoLocationContinentCode), "StartCountryCode" :: NullOrUndefined (GeoLocationCountryCode), "StartSubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A request to get a list of geographic locations that Amazon Route 53 supports for geolocation resource record sets. </p>

##### Instances
``` purescript
Newtype ListGeoLocationsRequest _
```

#### `ListGeoLocationsResponse`

``` purescript
newtype ListGeoLocationsResponse
  = ListGeoLocationsResponse { "GeoLocationDetailsList" :: GeoLocationDetailsList, "IsTruncated" :: PageTruncated, "NextContinentCode" :: NullOrUndefined (GeoLocationContinentCode), "NextCountryCode" :: NullOrUndefined (GeoLocationCountryCode), "NextSubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode), "MaxItems" :: PageMaxItems }
```

<p>A complex type containing the response information for the request.</p>

##### Instances
``` purescript
Newtype ListGeoLocationsResponse _
```

#### `ListHealthChecksRequest`

``` purescript
newtype ListHealthChecksRequest
  = ListHealthChecksRequest { "Marker" :: NullOrUndefined (PageMarker), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A request to retrieve a list of the health checks that are associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype ListHealthChecksRequest _
```

#### `ListHealthChecksResponse`

``` purescript
newtype ListHealthChecksResponse
  = ListHealthChecksResponse { "HealthChecks" :: HealthChecks, "Marker" :: PageMarker, "IsTruncated" :: PageTruncated, "NextMarker" :: NullOrUndefined (PageMarker), "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains the response to a <code>ListHealthChecks</code> request.</p>

##### Instances
``` purescript
Newtype ListHealthChecksResponse _
```

#### `ListHostedZonesByNameRequest`

``` purescript
newtype ListHostedZonesByNameRequest
  = ListHostedZonesByNameRequest { "DNSName" :: NullOrUndefined (DNSName), "HostedZoneId" :: NullOrUndefined (ResourceId), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>Retrieves a list of the public and private hosted zones that are associated with the current AWS account in ASCII order by domain name. </p>

##### Instances
``` purescript
Newtype ListHostedZonesByNameRequest _
```

#### `ListHostedZonesByNameResponse`

``` purescript
newtype ListHostedZonesByNameResponse
  = ListHostedZonesByNameResponse { "HostedZones" :: HostedZones, "DNSName" :: NullOrUndefined (DNSName), "HostedZoneId" :: NullOrUndefined (ResourceId), "IsTruncated" :: PageTruncated, "NextDNSName" :: NullOrUndefined (DNSName), "NextHostedZoneId" :: NullOrUndefined (ResourceId), "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains the response information for the request.</p>

##### Instances
``` purescript
Newtype ListHostedZonesByNameResponse _
```

#### `ListHostedZonesRequest`

``` purescript
newtype ListHostedZonesRequest
  = ListHostedZonesRequest { "Marker" :: NullOrUndefined (PageMarker), "MaxItems" :: NullOrUndefined (PageMaxItems), "DelegationSetId" :: NullOrUndefined (ResourceId) }
```

<p>A request to retrieve a list of the public and private hosted zones that are associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype ListHostedZonesRequest _
```

#### `ListHostedZonesResponse`

``` purescript
newtype ListHostedZonesResponse
  = ListHostedZonesResponse { "HostedZones" :: HostedZones, "Marker" :: PageMarker, "IsTruncated" :: PageTruncated, "NextMarker" :: NullOrUndefined (PageMarker), "MaxItems" :: PageMaxItems }
```

##### Instances
``` purescript
Newtype ListHostedZonesResponse _
```

#### `ListQueryLoggingConfigsRequest`

``` purescript
newtype ListQueryLoggingConfigsRequest
  = ListQueryLoggingConfigsRequest { "HostedZoneId" :: NullOrUndefined (ResourceId), "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListQueryLoggingConfigsRequest _
```

#### `ListQueryLoggingConfigsResponse`

``` purescript
newtype ListQueryLoggingConfigsResponse
  = ListQueryLoggingConfigsResponse { "QueryLoggingConfigs" :: QueryLoggingConfigs, "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype ListQueryLoggingConfigsResponse _
```

#### `ListResourceRecordSetsRequest`

``` purescript
newtype ListResourceRecordSetsRequest
  = ListResourceRecordSetsRequest { "HostedZoneId" :: ResourceId, "StartRecordName" :: NullOrUndefined (DNSName), "StartRecordType" :: NullOrUndefined (RRType), "StartRecordIdentifier" :: NullOrUndefined (ResourceRecordSetIdentifier), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A request for the resource record sets that are associated with a specified hosted zone.</p>

##### Instances
``` purescript
Newtype ListResourceRecordSetsRequest _
```

#### `ListResourceRecordSetsResponse`

``` purescript
newtype ListResourceRecordSetsResponse
  = ListResourceRecordSetsResponse { "ResourceRecordSets" :: ResourceRecordSets, "IsTruncated" :: PageTruncated, "NextRecordName" :: NullOrUndefined (DNSName), "NextRecordType" :: NullOrUndefined (RRType), "NextRecordIdentifier" :: NullOrUndefined (ResourceRecordSetIdentifier), "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains list information for the resource record set.</p>

##### Instances
``` purescript
Newtype ListResourceRecordSetsResponse _
```

#### `ListReusableDelegationSetsRequest`

``` purescript
newtype ListReusableDelegationSetsRequest
  = ListReusableDelegationSetsRequest { "Marker" :: NullOrUndefined (PageMarker), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A request to get a list of the reusable delegation sets that are associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype ListReusableDelegationSetsRequest _
```

#### `ListReusableDelegationSetsResponse`

``` purescript
newtype ListReusableDelegationSetsResponse
  = ListReusableDelegationSetsResponse { "DelegationSets" :: DelegationSets, "Marker" :: PageMarker, "IsTruncated" :: PageTruncated, "NextMarker" :: NullOrUndefined (PageMarker), "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains information about the reusable delegation sets that are associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype ListReusableDelegationSetsResponse _
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceType" :: TagResourceType, "ResourceId" :: TagResourceId }
```

<p>A complex type containing information about a request for a list of the tags that are associated with an individual resource.</p>

##### Instances
``` purescript
Newtype ListTagsForResourceRequest _
```

#### `ListTagsForResourceResponse`

``` purescript
newtype ListTagsForResourceResponse
  = ListTagsForResourceResponse { "ResourceTagSet" :: ResourceTagSet }
```

<p>A complex type that contains information about the health checks or hosted zones for which you want to list tags.</p>

##### Instances
``` purescript
Newtype ListTagsForResourceResponse _
```

#### `ListTagsForResourcesRequest`

``` purescript
newtype ListTagsForResourcesRequest
  = ListTagsForResourcesRequest { "ResourceType" :: TagResourceType, "ResourceIds" :: TagResourceIdList }
```

<p>A complex type that contains information about the health checks or hosted zones for which you want to list tags.</p>

##### Instances
``` purescript
Newtype ListTagsForResourcesRequest _
```

#### `ListTagsForResourcesResponse`

``` purescript
newtype ListTagsForResourcesResponse
  = ListTagsForResourcesResponse { "ResourceTagSets" :: ResourceTagSetList }
```

<p>A complex type containing tags for the specified resources.</p>

##### Instances
``` purescript
Newtype ListTagsForResourcesResponse _
```

#### `ListTrafficPoliciesRequest`

``` purescript
newtype ListTrafficPoliciesRequest
  = ListTrafficPoliciesRequest { "TrafficPolicyIdMarker" :: NullOrUndefined (TrafficPolicyId), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A complex type that contains the information about the request to list the traffic policies that are associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype ListTrafficPoliciesRequest _
```

#### `ListTrafficPoliciesResponse`

``` purescript
newtype ListTrafficPoliciesResponse
  = ListTrafficPoliciesResponse { "TrafficPolicySummaries" :: TrafficPolicySummaries, "IsTruncated" :: PageTruncated, "TrafficPolicyIdMarker" :: TrafficPolicyId, "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains the response information for the request.</p>

##### Instances
``` purescript
Newtype ListTrafficPoliciesResponse _
```

#### `ListTrafficPolicyInstancesByHostedZoneRequest`

``` purescript
newtype ListTrafficPolicyInstancesByHostedZoneRequest
  = ListTrafficPolicyInstancesByHostedZoneRequest { "HostedZoneId" :: ResourceId, "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName), "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A request for the traffic policy instances that you created in a specified hosted zone.</p>

##### Instances
``` purescript
Newtype ListTrafficPolicyInstancesByHostedZoneRequest _
```

#### `ListTrafficPolicyInstancesByHostedZoneResponse`

``` purescript
newtype ListTrafficPolicyInstancesByHostedZoneResponse
  = ListTrafficPolicyInstancesByHostedZoneResponse { "TrafficPolicyInstances" :: TrafficPolicyInstances, "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName), "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType), "IsTruncated" :: PageTruncated, "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains the response information for the request.</p>

##### Instances
``` purescript
Newtype ListTrafficPolicyInstancesByHostedZoneResponse _
```

#### `ListTrafficPolicyInstancesByPolicyRequest`

``` purescript
newtype ListTrafficPolicyInstancesByPolicyRequest
  = ListTrafficPolicyInstancesByPolicyRequest { "TrafficPolicyId" :: TrafficPolicyId, "TrafficPolicyVersion" :: TrafficPolicyVersion, "HostedZoneIdMarker" :: NullOrUndefined (ResourceId), "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName), "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A complex type that contains the information about the request to list your traffic policy instances.</p>

##### Instances
``` purescript
Newtype ListTrafficPolicyInstancesByPolicyRequest _
```

#### `ListTrafficPolicyInstancesByPolicyResponse`

``` purescript
newtype ListTrafficPolicyInstancesByPolicyResponse
  = ListTrafficPolicyInstancesByPolicyResponse { "TrafficPolicyInstances" :: TrafficPolicyInstances, "HostedZoneIdMarker" :: NullOrUndefined (ResourceId), "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName), "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType), "IsTruncated" :: PageTruncated, "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains the response information for the request.</p>

##### Instances
``` purescript
Newtype ListTrafficPolicyInstancesByPolicyResponse _
```

#### `ListTrafficPolicyInstancesRequest`

``` purescript
newtype ListTrafficPolicyInstancesRequest
  = ListTrafficPolicyInstancesRequest { "HostedZoneIdMarker" :: NullOrUndefined (ResourceId), "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName), "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A request to get information about the traffic policy instances that you created by using the current AWS account.</p>

##### Instances
``` purescript
Newtype ListTrafficPolicyInstancesRequest _
```

#### `ListTrafficPolicyInstancesResponse`

``` purescript
newtype ListTrafficPolicyInstancesResponse
  = ListTrafficPolicyInstancesResponse { "TrafficPolicyInstances" :: TrafficPolicyInstances, "HostedZoneIdMarker" :: NullOrUndefined (ResourceId), "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName), "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType), "IsTruncated" :: PageTruncated, "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains the response information for the request.</p>

##### Instances
``` purescript
Newtype ListTrafficPolicyInstancesResponse _
```

#### `ListTrafficPolicyVersionsRequest`

``` purescript
newtype ListTrafficPolicyVersionsRequest
  = ListTrafficPolicyVersionsRequest { "Id" :: TrafficPolicyId, "TrafficPolicyVersionMarker" :: NullOrUndefined (TrafficPolicyVersionMarker), "MaxItems" :: NullOrUndefined (PageMaxItems) }
```

<p>A complex type that contains the information about the request to list your traffic policies.</p>

##### Instances
``` purescript
Newtype ListTrafficPolicyVersionsRequest _
```

#### `ListTrafficPolicyVersionsResponse`

``` purescript
newtype ListTrafficPolicyVersionsResponse
  = ListTrafficPolicyVersionsResponse { "TrafficPolicies" :: TrafficPolicies, "IsTruncated" :: PageTruncated, "TrafficPolicyVersionMarker" :: TrafficPolicyVersionMarker, "MaxItems" :: PageMaxItems }
```

<p>A complex type that contains the response information for the request.</p>

##### Instances
``` purescript
Newtype ListTrafficPolicyVersionsResponse _
```

#### `ListVPCAssociationAuthorizationsRequest`

``` purescript
newtype ListVPCAssociationAuthorizationsRequest
  = ListVPCAssociationAuthorizationsRequest { "HostedZoneId" :: ResourceId, "NextToken" :: NullOrUndefined (PaginationToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

<p>A complex type that contains information about that can be associated with your hosted zone.</p>

##### Instances
``` purescript
Newtype ListVPCAssociationAuthorizationsRequest _
```

#### `ListVPCAssociationAuthorizationsResponse`

``` purescript
newtype ListVPCAssociationAuthorizationsResponse
  = ListVPCAssociationAuthorizationsResponse { "HostedZoneId" :: ResourceId, "NextToken" :: NullOrUndefined (PaginationToken), "VPCs" :: VPCs }
```

<p>A complex type that contains the response information for the request.</p>

##### Instances
``` purescript
Newtype ListVPCAssociationAuthorizationsResponse _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults String
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `MeasureLatency`

``` purescript
newtype MeasureLatency
  = MeasureLatency Boolean
```

##### Instances
``` purescript
Newtype MeasureLatency _
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

##### Instances
``` purescript
Newtype Message _
```

#### `MetricName`

``` purescript
newtype MetricName
  = MetricName String
```

##### Instances
``` purescript
Newtype MetricName _
```

#### `Nameserver`

``` purescript
newtype Nameserver
  = Nameserver String
```

##### Instances
``` purescript
Newtype Nameserver _
```

#### `Namespace`

``` purescript
newtype Namespace
  = Namespace String
```

##### Instances
``` purescript
Newtype Namespace _
```

#### `NoSuchChange`

``` purescript
newtype NoSuchChange
  = NoSuchChange { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>A change with the specified change ID does not exist.</p>

##### Instances
``` purescript
Newtype NoSuchChange _
```

#### `NoSuchCloudWatchLogsLogGroup`

``` purescript
newtype NoSuchCloudWatchLogsLogGroup
  = NoSuchCloudWatchLogsLogGroup { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>There is no CloudWatch Logs log group with the specified ARN.</p>

##### Instances
``` purescript
Newtype NoSuchCloudWatchLogsLogGroup _
```

#### `NoSuchDelegationSet`

``` purescript
newtype NoSuchDelegationSet
  = NoSuchDelegationSet { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>A reusable delegation set with the specified ID does not exist.</p>

##### Instances
``` purescript
Newtype NoSuchDelegationSet _
```

#### `NoSuchGeoLocation`

``` purescript
newtype NoSuchGeoLocation
  = NoSuchGeoLocation { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Amazon Route 53 doesn't support the specified geolocation.</p>

##### Instances
``` purescript
Newtype NoSuchGeoLocation _
```

#### `NoSuchHealthCheck`

``` purescript
newtype NoSuchHealthCheck
  = NoSuchHealthCheck { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>No health check exists with the ID that you specified in the <code>DeleteHealthCheck</code> request.</p>

##### Instances
``` purescript
Newtype NoSuchHealthCheck _
```

#### `NoSuchHostedZone`

``` purescript
newtype NoSuchHostedZone
  = NoSuchHostedZone { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>No hosted zone exists with the ID that you specified.</p>

##### Instances
``` purescript
Newtype NoSuchHostedZone _
```

#### `NoSuchQueryLoggingConfig`

``` purescript
newtype NoSuchQueryLoggingConfig
  = NoSuchQueryLoggingConfig { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>There is no DNS query logging configuration with the specified ID.</p>

##### Instances
``` purescript
Newtype NoSuchQueryLoggingConfig _
```

#### `NoSuchTrafficPolicy`

``` purescript
newtype NoSuchTrafficPolicy
  = NoSuchTrafficPolicy { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>No traffic policy exists with the specified ID.</p>

##### Instances
``` purescript
Newtype NoSuchTrafficPolicy _
```

#### `NoSuchTrafficPolicyInstance`

``` purescript
newtype NoSuchTrafficPolicyInstance
  = NoSuchTrafficPolicyInstance { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>No traffic policy instance exists with the specified ID.</p>

##### Instances
``` purescript
Newtype NoSuchTrafficPolicyInstance _
```

#### `Nonce`

``` purescript
newtype Nonce
  = Nonce String
```

##### Instances
``` purescript
Newtype Nonce _
```

#### `NotAuthorizedException`

``` purescript
newtype NotAuthorizedException
  = NotAuthorizedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Associating the specified VPC with the specified hosted zone has not been authorized.</p>

##### Instances
``` purescript
Newtype NotAuthorizedException _
```

#### `PageMarker`

``` purescript
newtype PageMarker
  = PageMarker String
```

##### Instances
``` purescript
Newtype PageMarker _
```

#### `PageMaxItems`

``` purescript
newtype PageMaxItems
  = PageMaxItems String
```

##### Instances
``` purescript
Newtype PageMaxItems _
```

#### `PageTruncated`

``` purescript
newtype PageTruncated
  = PageTruncated Boolean
```

##### Instances
``` purescript
Newtype PageTruncated _
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

##### Instances
``` purescript
Newtype PaginationToken _
```

#### `Period`

``` purescript
newtype Period
  = Period Int
```

##### Instances
``` purescript
Newtype Period _
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

#### `PriorRequestNotComplete`

``` purescript
newtype PriorRequestNotComplete
  = PriorRequestNotComplete { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>If Amazon Route 53 can't process a request before the next request arrives, it will reject subsequent requests for the same hosted zone and return an <code>HTTP 400 error</code> (<code>Bad request</code>). If Amazon Route 53 returns this error repeatedly for the same request, we recommend that you wait, in intervals of increasing duration, before you try the request again.</p>

##### Instances
``` purescript
Newtype PriorRequestNotComplete _
```

#### `PublicZoneVPCAssociation`

``` purescript
newtype PublicZoneVPCAssociation
  = PublicZoneVPCAssociation { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>You're trying to associate a VPC with a public hosted zone. Amazon Route 53 doesn't support associating a VPC with a public hosted zone.</p>

##### Instances
``` purescript
Newtype PublicZoneVPCAssociation _
```

#### `QueryLoggingConfig`

``` purescript
newtype QueryLoggingConfig
  = QueryLoggingConfig { "Id" :: QueryLoggingConfigId, "HostedZoneId" :: ResourceId, "CloudWatchLogsLogGroupArn" :: CloudWatchLogsLogGroupArn }
```

<p>A complex type that contains information about a configuration for DNS query logging.</p>

##### Instances
``` purescript
Newtype QueryLoggingConfig _
```

#### `QueryLoggingConfigAlreadyExists`

``` purescript
newtype QueryLoggingConfigAlreadyExists
  = QueryLoggingConfigAlreadyExists { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>You can create only one query logging configuration for a hosted zone, and a query logging configuration already exists for this hosted zone.</p>

##### Instances
``` purescript
Newtype QueryLoggingConfigAlreadyExists _
```

#### `QueryLoggingConfigId`

``` purescript
newtype QueryLoggingConfigId
  = QueryLoggingConfigId String
```

##### Instances
``` purescript
Newtype QueryLoggingConfigId _
```

#### `QueryLoggingConfigs`

``` purescript
newtype QueryLoggingConfigs
  = QueryLoggingConfigs (Array QueryLoggingConfig)
```

##### Instances
``` purescript
Newtype QueryLoggingConfigs _
```

#### `RData`

``` purescript
newtype RData
  = RData String
```

##### Instances
``` purescript
Newtype RData _
```

#### `RRType`

``` purescript
newtype RRType
  = RRType String
```

##### Instances
``` purescript
Newtype RRType _
```

#### `RecordData`

``` purescript
newtype RecordData
  = RecordData (Array RecordDataEntry)
```

##### Instances
``` purescript
Newtype RecordData _
```

#### `RecordDataEntry`

``` purescript
newtype RecordDataEntry
  = RecordDataEntry String
```

<p>A value that Amazon Route 53 returned for this resource record set. A <code>RecordDataEntry</code> element is one of the following:</p> <ul> <li> <p>For non-alias resource record sets, a <code>RecordDataEntry</code> element contains one value in the resource record set. If the resource record set contains multiple values, the response includes one <code>RecordDataEntry</code> element for each value.</p> </li> <li> <p>For multiple resource record sets that have the same name and type, which includes weighted, latency, geolocation, and failover, a <code>RecordDataEntry</code> element contains the value from the appropriate resource record set based on the request.</p> </li> <li> <p>For alias resource record sets that refer to AWS resources other than another resource record set, the <code>RecordDataEntry</code> element contains an IP address or a domain name for the AWS resource, depending on the type of resource.</p> </li> <li> <p>For alias resource record sets that refer to other resource record sets, a <code>RecordDataEntry</code> element contains one value from the referenced resource record set. If the referenced resource record set contains multiple values, the response includes one <code>RecordDataEntry</code> element for each value.</p> </li> </ul>

##### Instances
``` purescript
Newtype RecordDataEntry _
```

#### `RequestInterval`

``` purescript
newtype RequestInterval
  = RequestInterval Int
```

##### Instances
``` purescript
Newtype RequestInterval _
```

#### `ResettableElementName`

``` purescript
newtype ResettableElementName
  = ResettableElementName String
```

##### Instances
``` purescript
Newtype ResettableElementName _
```

#### `ResettableElementNameList`

``` purescript
newtype ResettableElementNameList
  = ResettableElementNameList (Array ResettableElementName)
```

##### Instances
``` purescript
Newtype ResettableElementNameList _
```

#### `ResourceDescription`

``` purescript
newtype ResourceDescription
  = ResourceDescription String
```

##### Instances
``` purescript
Newtype ResourceDescription _
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

##### Instances
``` purescript
Newtype ResourceId _
```

#### `ResourcePath`

``` purescript
newtype ResourcePath
  = ResourcePath String
```

##### Instances
``` purescript
Newtype ResourcePath _
```

#### `ResourceRecord`

``` purescript
newtype ResourceRecord
  = ResourceRecord { "Value" :: RData }
```

<p>Information specific to the resource record.</p> <note> <p>If you're creating an alias resource record set, omit <code>ResourceRecord</code>.</p> </note>

##### Instances
``` purescript
Newtype ResourceRecord _
```

#### `ResourceRecordSet`

``` purescript
newtype ResourceRecordSet
  = ResourceRecordSet { "Name" :: DNSName, "Type" :: RRType, "SetIdentifier" :: NullOrUndefined (ResourceRecordSetIdentifier), "Weight" :: NullOrUndefined (ResourceRecordSetWeight), "Region" :: NullOrUndefined (ResourceRecordSetRegion), "GeoLocation" :: NullOrUndefined (GeoLocation), "Failover" :: NullOrUndefined (ResourceRecordSetFailover), "MultiValueAnswer" :: NullOrUndefined (ResourceRecordSetMultiValueAnswer), "TTL" :: NullOrUndefined (TTL), "ResourceRecords" :: NullOrUndefined (ResourceRecords), "AliasTarget" :: NullOrUndefined (AliasTarget), "HealthCheckId" :: NullOrUndefined (HealthCheckId), "TrafficPolicyInstanceId" :: NullOrUndefined (TrafficPolicyInstanceId) }
```

<p>Information about the resource record set to create or delete.</p>

##### Instances
``` purescript
Newtype ResourceRecordSet _
```

#### `ResourceRecordSetFailover`

``` purescript
newtype ResourceRecordSetFailover
  = ResourceRecordSetFailover String
```

##### Instances
``` purescript
Newtype ResourceRecordSetFailover _
```

#### `ResourceRecordSetIdentifier`

``` purescript
newtype ResourceRecordSetIdentifier
  = ResourceRecordSetIdentifier String
```

##### Instances
``` purescript
Newtype ResourceRecordSetIdentifier _
```

#### `ResourceRecordSetMultiValueAnswer`

``` purescript
newtype ResourceRecordSetMultiValueAnswer
  = ResourceRecordSetMultiValueAnswer Boolean
```

##### Instances
``` purescript
Newtype ResourceRecordSetMultiValueAnswer _
```

#### `ResourceRecordSetRegion`

``` purescript
newtype ResourceRecordSetRegion
  = ResourceRecordSetRegion String
```

##### Instances
``` purescript
Newtype ResourceRecordSetRegion _
```

#### `ResourceRecordSetWeight`

``` purescript
newtype ResourceRecordSetWeight
  = ResourceRecordSetWeight Number
```

##### Instances
``` purescript
Newtype ResourceRecordSetWeight _
```

#### `ResourceRecordSets`

``` purescript
newtype ResourceRecordSets
  = ResourceRecordSets (Array ResourceRecordSet)
```

##### Instances
``` purescript
Newtype ResourceRecordSets _
```

#### `ResourceRecords`

``` purescript
newtype ResourceRecords
  = ResourceRecords (Array ResourceRecord)
```

##### Instances
``` purescript
Newtype ResourceRecords _
```

#### `ResourceTagSet`

``` purescript
newtype ResourceTagSet
  = ResourceTagSet { "ResourceType" :: NullOrUndefined (TagResourceType), "ResourceId" :: NullOrUndefined (TagResourceId), "Tags" :: NullOrUndefined (TagList) }
```

<p>A complex type containing a resource and its associated tags.</p>

##### Instances
``` purescript
Newtype ResourceTagSet _
```

#### `ResourceTagSetList`

``` purescript
newtype ResourceTagSetList
  = ResourceTagSetList (Array ResourceTagSet)
```

##### Instances
``` purescript
Newtype ResourceTagSetList _
```

#### `ResourceURI`

``` purescript
newtype ResourceURI
  = ResourceURI String
```

##### Instances
``` purescript
Newtype ResourceURI _
```

#### `ReusableDelegationSetLimit`

``` purescript
newtype ReusableDelegationSetLimit
  = ReusableDelegationSetLimit { "Type" :: ReusableDelegationSetLimitType, "Value" :: LimitValue }
```

<p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>

##### Instances
``` purescript
Newtype ReusableDelegationSetLimit _
```

#### `ReusableDelegationSetLimitType`

``` purescript
newtype ReusableDelegationSetLimitType
  = ReusableDelegationSetLimitType String
```

##### Instances
``` purescript
Newtype ReusableDelegationSetLimitType _
```

#### `SearchString`

``` purescript
newtype SearchString
  = SearchString String
```

##### Instances
``` purescript
Newtype SearchString _
```

#### `ServicePrincipal`

``` purescript
newtype ServicePrincipal
  = ServicePrincipal String
```

##### Instances
``` purescript
Newtype ServicePrincipal _
```

#### `Statistic`

``` purescript
newtype Statistic
  = Statistic String
```

##### Instances
``` purescript
Newtype Statistic _
```

#### `Status`

``` purescript
newtype Status
  = Status String
```

##### Instances
``` purescript
Newtype Status _
```

#### `StatusReport`

``` purescript
newtype StatusReport
  = StatusReport { "Status" :: NullOrUndefined (Status), "CheckedTime" :: NullOrUndefined (TimeStamp) }
```

<p>A complex type that contains the status that one Amazon Route 53 health checker reports and the time of the health check.</p>

##### Instances
``` purescript
Newtype StatusReport _
```

#### `SubnetMask`

``` purescript
newtype SubnetMask
  = SubnetMask String
```

##### Instances
``` purescript
Newtype SubnetMask _
```

#### `TTL`

``` purescript
newtype TTL
  = TTL Number
```

##### Instances
``` purescript
Newtype TTL _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: NullOrUndefined (TagKey), "Value" :: NullOrUndefined (TagValue) }
```

<p>A complex type that contains information about a tag that you want to add or edit for the specified health check or hosted zone.</p>

##### Instances
``` purescript
Newtype Tag _
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

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeyList _
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

#### `TagResourceId`

``` purescript
newtype TagResourceId
  = TagResourceId String
```

##### Instances
``` purescript
Newtype TagResourceId _
```

#### `TagResourceIdList`

``` purescript
newtype TagResourceIdList
  = TagResourceIdList (Array TagResourceId)
```

##### Instances
``` purescript
Newtype TagResourceIdList _
```

#### `TagResourceType`

``` purescript
newtype TagResourceType
  = TagResourceType String
```

##### Instances
``` purescript
Newtype TagResourceType _
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

#### `TestDNSAnswerRequest`

``` purescript
newtype TestDNSAnswerRequest
  = TestDNSAnswerRequest { "HostedZoneId" :: ResourceId, "RecordName" :: DNSName, "RecordType" :: RRType, "ResolverIP" :: NullOrUndefined (IPAddress), "EDNS0ClientSubnetIP" :: NullOrUndefined (IPAddress), "EDNS0ClientSubnetMask" :: NullOrUndefined (SubnetMask) }
```

<p>Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask. </p>

##### Instances
``` purescript
Newtype TestDNSAnswerRequest _
```

#### `TestDNSAnswerResponse`

``` purescript
newtype TestDNSAnswerResponse
  = TestDNSAnswerResponse { "Nameserver" :: Nameserver, "RecordName" :: DNSName, "RecordType" :: RRType, "RecordData" :: RecordData, "ResponseCode" :: DNSRCode, "Protocol" :: TransportProtocol }
```

<p>A complex type that contains the response to a <code>TestDNSAnswer</code> request. </p>

##### Instances
``` purescript
Newtype TestDNSAnswerResponse _
```

#### `Threshold`

``` purescript
newtype Threshold
  = Threshold Number
```

##### Instances
``` purescript
Newtype Threshold _
```

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The limit on the number of requests per second was exceeded.</p>

##### Instances
``` purescript
Newtype ThrottlingException _
```

#### `TimeStamp`

``` purescript
newtype TimeStamp
  = TimeStamp Number
```

##### Instances
``` purescript
Newtype TimeStamp _
```

#### `TooManyHealthChecks`

``` purescript
newtype TooManyHealthChecks
  = TooManyHealthChecks { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This health check can't be created because the current account has reached the limit on the number of active health checks.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>For information about how to get the current limit for an account, see <a>GetAccountLimit</a>. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p> <p>You have reached the maximum number of active health checks for an AWS account. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>

##### Instances
``` purescript
Newtype TooManyHealthChecks _
```

#### `TooManyHostedZones`

``` purescript
newtype TooManyHostedZones
  = TooManyHostedZones { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This operation can't be completed either because the current account has reached the limit on the number of hosted zones or because you've reached the limit on the number of hosted zones that can be associated with a reusable delegation set.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>To get the current limit on hosted zones that can be created by an account, see <a>GetAccountLimit</a>.</p> <p>To get the current limit on hosted zones that can be associated with a reusable delegation set, see <a>GetReusableDelegationSetLimit</a>.</p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>

##### Instances
``` purescript
Newtype TooManyHostedZones _
```

#### `TooManyTrafficPolicies`

``` purescript
newtype TooManyTrafficPolicies
  = TooManyTrafficPolicies { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This traffic policy can't be created because the current account has reached the limit on the number of traffic policies.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>To get the current limit for an account, see <a>GetAccountLimit</a>. </p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>

##### Instances
``` purescript
Newtype TooManyTrafficPolicies _
```

#### `TooManyTrafficPolicyInstances`

``` purescript
newtype TooManyTrafficPolicyInstances
  = TooManyTrafficPolicyInstances { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This traffic policy instance can't be created because the current account has reached the limit on the number of traffic policy instances.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>For information about how to get the current limit for an account, see <a>GetAccountLimit</a>.</p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>

##### Instances
``` purescript
Newtype TooManyTrafficPolicyInstances _
```

#### `TooManyTrafficPolicyVersionsForCurrentPolicy`

``` purescript
newtype TooManyTrafficPolicyVersionsForCurrentPolicy
  = TooManyTrafficPolicyVersionsForCurrentPolicy { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This traffic policy version can't be created because you've reached the limit of 1000 on the number of versions that you can create for the current traffic policy.</p> <p>To create more traffic policy versions, you can use <a>GetTrafficPolicy</a> to get the traffic policy document for a specified traffic policy version, and then use <a>CreateTrafficPolicy</a> to create a new traffic policy using the traffic policy document.</p>

##### Instances
``` purescript
Newtype TooManyTrafficPolicyVersionsForCurrentPolicy _
```

#### `TooManyVPCAssociationAuthorizations`

``` purescript
newtype TooManyVPCAssociationAuthorizations
  = TooManyVPCAssociationAuthorizations { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>You've created the maximum number of authorizations that can be created for the specified hosted zone. To authorize another VPC to be associated with the hosted zone, submit a <code>DeleteVPCAssociationAuthorization</code> request to remove an existing authorization. To get a list of existing authorizations, submit a <code>ListVPCAssociationAuthorizations</code> request.</p>

##### Instances
``` purescript
Newtype TooManyVPCAssociationAuthorizations _
```

#### `TrafficPolicies`

``` purescript
newtype TrafficPolicies
  = TrafficPolicies (Array TrafficPolicy)
```

##### Instances
``` purescript
Newtype TrafficPolicies _
```

#### `TrafficPolicy`

``` purescript
newtype TrafficPolicy
  = TrafficPolicy { "Id" :: TrafficPolicyId, "Version" :: TrafficPolicyVersion, "Name" :: TrafficPolicyName, "Type" :: RRType, "Document" :: TrafficPolicyDocument, "Comment" :: NullOrUndefined (TrafficPolicyComment) }
```

<p>A complex type that contains settings for a traffic policy.</p>

##### Instances
``` purescript
Newtype TrafficPolicy _
```

#### `TrafficPolicyAlreadyExists`

``` purescript
newtype TrafficPolicyAlreadyExists
  = TrafficPolicyAlreadyExists { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>A traffic policy that has the same value for <code>Name</code> already exists.</p>

##### Instances
``` purescript
Newtype TrafficPolicyAlreadyExists _
```

#### `TrafficPolicyComment`

``` purescript
newtype TrafficPolicyComment
  = TrafficPolicyComment String
```

##### Instances
``` purescript
Newtype TrafficPolicyComment _
```

#### `TrafficPolicyDocument`

``` purescript
newtype TrafficPolicyDocument
  = TrafficPolicyDocument String
```

##### Instances
``` purescript
Newtype TrafficPolicyDocument _
```

#### `TrafficPolicyId`

``` purescript
newtype TrafficPolicyId
  = TrafficPolicyId String
```

##### Instances
``` purescript
Newtype TrafficPolicyId _
```

#### `TrafficPolicyInUse`

``` purescript
newtype TrafficPolicyInUse
  = TrafficPolicyInUse { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>One or more traffic policy instances were created by using the specified traffic policy.</p>

##### Instances
``` purescript
Newtype TrafficPolicyInUse _
```

#### `TrafficPolicyInstance`

``` purescript
newtype TrafficPolicyInstance
  = TrafficPolicyInstance { "Id" :: TrafficPolicyInstanceId, "HostedZoneId" :: ResourceId, "Name" :: DNSName, "TTL" :: TTL, "State" :: TrafficPolicyInstanceState, "Message" :: Message, "TrafficPolicyId" :: TrafficPolicyId, "TrafficPolicyVersion" :: TrafficPolicyVersion, "TrafficPolicyType" :: RRType }
```

<p>A complex type that contains settings for the new traffic policy instance.</p>

##### Instances
``` purescript
Newtype TrafficPolicyInstance _
```

#### `TrafficPolicyInstanceAlreadyExists`

``` purescript
newtype TrafficPolicyInstanceAlreadyExists
  = TrafficPolicyInstanceAlreadyExists { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>There is already a traffic policy instance with the specified ID.</p>

##### Instances
``` purescript
Newtype TrafficPolicyInstanceAlreadyExists _
```

#### `TrafficPolicyInstanceCount`

``` purescript
newtype TrafficPolicyInstanceCount
  = TrafficPolicyInstanceCount Int
```

##### Instances
``` purescript
Newtype TrafficPolicyInstanceCount _
```

#### `TrafficPolicyInstanceId`

``` purescript
newtype TrafficPolicyInstanceId
  = TrafficPolicyInstanceId String
```

##### Instances
``` purescript
Newtype TrafficPolicyInstanceId _
```

#### `TrafficPolicyInstanceState`

``` purescript
newtype TrafficPolicyInstanceState
  = TrafficPolicyInstanceState String
```

##### Instances
``` purescript
Newtype TrafficPolicyInstanceState _
```

#### `TrafficPolicyInstances`

``` purescript
newtype TrafficPolicyInstances
  = TrafficPolicyInstances (Array TrafficPolicyInstance)
```

##### Instances
``` purescript
Newtype TrafficPolicyInstances _
```

#### `TrafficPolicyName`

``` purescript
newtype TrafficPolicyName
  = TrafficPolicyName String
```

##### Instances
``` purescript
Newtype TrafficPolicyName _
```

#### `TrafficPolicySummaries`

``` purescript
newtype TrafficPolicySummaries
  = TrafficPolicySummaries (Array TrafficPolicySummary)
```

##### Instances
``` purescript
Newtype TrafficPolicySummaries _
```

#### `TrafficPolicySummary`

``` purescript
newtype TrafficPolicySummary
  = TrafficPolicySummary { "Id" :: TrafficPolicyId, "Name" :: TrafficPolicyName, "Type" :: RRType, "LatestVersion" :: TrafficPolicyVersion, "TrafficPolicyCount" :: TrafficPolicyVersion }
```

<p>A complex type that contains information about the latest version of one traffic policy that is associated with the current AWS account.</p>

##### Instances
``` purescript
Newtype TrafficPolicySummary _
```

#### `TrafficPolicyVersion`

``` purescript
newtype TrafficPolicyVersion
  = TrafficPolicyVersion Int
```

##### Instances
``` purescript
Newtype TrafficPolicyVersion _
```

#### `TrafficPolicyVersionMarker`

``` purescript
newtype TrafficPolicyVersionMarker
  = TrafficPolicyVersionMarker String
```

##### Instances
``` purescript
Newtype TrafficPolicyVersionMarker _
```

#### `TransportProtocol`

``` purescript
newtype TransportProtocol
  = TransportProtocol String
```

##### Instances
``` purescript
Newtype TransportProtocol _
```

#### `UpdateHealthCheckRequest`

``` purescript
newtype UpdateHealthCheckRequest
  = UpdateHealthCheckRequest { "HealthCheckId" :: HealthCheckId, "HealthCheckVersion" :: NullOrUndefined (HealthCheckVersion), "IPAddress" :: NullOrUndefined (IPAddress), "Port" :: NullOrUndefined (Port), "ResourcePath" :: NullOrUndefined (ResourcePath), "FullyQualifiedDomainName" :: NullOrUndefined (FullyQualifiedDomainName), "SearchString" :: NullOrUndefined (SearchString), "FailureThreshold" :: NullOrUndefined (FailureThreshold), "Inverted" :: NullOrUndefined (Inverted), "HealthThreshold" :: NullOrUndefined (HealthThreshold), "ChildHealthChecks" :: NullOrUndefined (ChildHealthCheckList), "EnableSNI" :: NullOrUndefined (EnableSNI), "Regions" :: NullOrUndefined (HealthCheckRegionList), "AlarmIdentifier" :: NullOrUndefined (AlarmIdentifier), "InsufficientDataHealthStatus" :: NullOrUndefined (InsufficientDataHealthStatus), "ResetElements" :: NullOrUndefined (ResettableElementNameList) }
```

<p>A complex type that contains information about a request to update a health check.</p>

##### Instances
``` purescript
Newtype UpdateHealthCheckRequest _
```

#### `UpdateHealthCheckResponse`

``` purescript
newtype UpdateHealthCheckResponse
  = UpdateHealthCheckResponse { "HealthCheck" :: HealthCheck }
```

##### Instances
``` purescript
Newtype UpdateHealthCheckResponse _
```

#### `UpdateHostedZoneCommentRequest`

``` purescript
newtype UpdateHostedZoneCommentRequest
  = UpdateHostedZoneCommentRequest { "Id" :: ResourceId, "Comment" :: NullOrUndefined (ResourceDescription) }
```

<p>A request to update the comment for a hosted zone.</p>

##### Instances
``` purescript
Newtype UpdateHostedZoneCommentRequest _
```

#### `UpdateHostedZoneCommentResponse`

``` purescript
newtype UpdateHostedZoneCommentResponse
  = UpdateHostedZoneCommentResponse { "HostedZone" :: HostedZone }
```

<p>A complex type that contains the response to the <code>UpdateHostedZoneComment</code> request.</p>

##### Instances
``` purescript
Newtype UpdateHostedZoneCommentResponse _
```

#### `UpdateTrafficPolicyCommentRequest`

``` purescript
newtype UpdateTrafficPolicyCommentRequest
  = UpdateTrafficPolicyCommentRequest { "Id" :: TrafficPolicyId, "Version" :: TrafficPolicyVersion, "Comment" :: TrafficPolicyComment }
```

<p>A complex type that contains information about the traffic policy that you want to update the comment for.</p>

##### Instances
``` purescript
Newtype UpdateTrafficPolicyCommentRequest _
```

#### `UpdateTrafficPolicyCommentResponse`

``` purescript
newtype UpdateTrafficPolicyCommentResponse
  = UpdateTrafficPolicyCommentResponse { "TrafficPolicy" :: TrafficPolicy }
```

<p>A complex type that contains the response information for the traffic policy.</p>

##### Instances
``` purescript
Newtype UpdateTrafficPolicyCommentResponse _
```

#### `UpdateTrafficPolicyInstanceRequest`

``` purescript
newtype UpdateTrafficPolicyInstanceRequest
  = UpdateTrafficPolicyInstanceRequest { "Id" :: TrafficPolicyInstanceId, "TTL" :: TTL, "TrafficPolicyId" :: TrafficPolicyId, "TrafficPolicyVersion" :: TrafficPolicyVersion }
```

<p>A complex type that contains information about the resource record sets that you want to update based on a specified traffic policy instance.</p>

##### Instances
``` purescript
Newtype UpdateTrafficPolicyInstanceRequest _
```

#### `UpdateTrafficPolicyInstanceResponse`

``` purescript
newtype UpdateTrafficPolicyInstanceResponse
  = UpdateTrafficPolicyInstanceResponse { "TrafficPolicyInstance" :: TrafficPolicyInstance }
```

<p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>

##### Instances
``` purescript
Newtype UpdateTrafficPolicyInstanceResponse _
```

#### `UsageCount`

``` purescript
newtype UsageCount
  = UsageCount Number
```

##### Instances
``` purescript
Newtype UsageCount _
```

#### `VPC`

``` purescript
newtype VPC
  = VPC { "VPCRegion" :: NullOrUndefined (VPCRegion), "VPCId" :: NullOrUndefined (VPCId) }
```

<p>(Private hosted zones only) A complex type that contains information about an Amazon VPC.</p>

##### Instances
``` purescript
Newtype VPC _
```

#### `VPCAssociationAuthorizationNotFound`

``` purescript
newtype VPCAssociationAuthorizationNotFound
  = VPCAssociationAuthorizationNotFound { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The VPC that you specified is not authorized to be associated with the hosted zone.</p>

##### Instances
``` purescript
Newtype VPCAssociationAuthorizationNotFound _
```

#### `VPCAssociationNotFound`

``` purescript
newtype VPCAssociationNotFound
  = VPCAssociationNotFound { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified VPC and hosted zone are not currently associated.</p>

##### Instances
``` purescript
Newtype VPCAssociationNotFound _
```

#### `VPCId`

``` purescript
newtype VPCId
  = VPCId String
```

<p>(Private hosted zones only) The ID of an Amazon VPC. </p>

##### Instances
``` purescript
Newtype VPCId _
```

#### `VPCRegion`

``` purescript
newtype VPCRegion
  = VPCRegion String
```

##### Instances
``` purescript
Newtype VPCRegion _
```

#### `VPCs`

``` purescript
newtype VPCs
  = VPCs (Array VPC)
```

<p>(Private hosted zones only) A list of <code>VPC</code> elements.</p>

##### Instances
``` purescript
Newtype VPCs _
```


