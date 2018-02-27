

module AWS.Route53 where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Route53" :: String


-- | <p>Associates an Amazon VPC with a private hosted zone. </p> <important> <p>To perform the association, the VPC and the private hosted zone must already exist. You can't convert a public hosted zone into a private hosted zone.</p> </important> <note> <p>If you want to associate a VPC that was created by using one AWS account with a private hosted zone that was created by using a different account, the AWS account that created the private hosted zone must first submit a <code>CreateVPCAssociationAuthorization</code> request. Then the account that created the VPC must submit an <code>AssociateVPCWithHostedZone</code> request.</p> </note>
associateVPCWithHostedZone :: forall eff. AssociateVPCWithHostedZoneRequest -> Aff (err :: AWS.RequestError | eff) AssociateVPCWithHostedZoneResponse
associateVPCWithHostedZone = AWS.request serviceName "associateVPCWithHostedZone" 


-- | <p>Creates, changes, or deletes a resource record set, which contains authoritative DNS information for a specified domain name or subdomain name. For example, you can use <code>ChangeResourceRecordSets</code> to create a resource record set that routes traffic for test.example.com to a web server that has an IP address of 192.0.2.44.</p> <p> <b>Change Batches and Transactional Changes</b> </p> <p>The request body must include a document with a <code>ChangeResourceRecordSetsRequest</code> element. The request body contains a list of change items, known as a change batch. Change batches are considered transactional changes. When using the Amazon Route 53 API to change resource record sets, Amazon Route 53 either makes all or none of the changes in a change batch request. This ensures that Amazon Route 53 never partially implements the intended changes to the resource record sets in a hosted zone. </p> <p>For example, a change batch request that deletes the <code>CNAME</code> record for www.example.com and creates an alias resource record set for www.example.com. Amazon Route 53 deletes the first resource record set and creates the second resource record set in a single operation. If either the <code>DELETE</code> or the <code>CREATE</code> action fails, then both changes (plus any other changes in the batch) fail, and the original <code>CNAME</code> record continues to exist.</p> <important> <p>Due to the nature of transactional changes, you can't delete the same resource record set more than once in a single change batch. If you attempt to delete the same change batch more than once, Amazon Route 53 returns an <code>InvalidChangeBatch</code> error.</p> </important> <p> <b>Traffic Flow</b> </p> <p>To create resource record sets for complex routing configurations, use either the traffic flow visual editor in the Amazon Route 53 console or the API actions for traffic policies and traffic policy instances. Save the configuration as a traffic policy, then associate the traffic policy with one or more domain names (such as example.com) or subdomain names (such as www.example.com), in the same hosted zone or in multiple hosted zones. You can roll back the updates if the new configuration isn't performing as expected. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/traffic-flow.html">Using Traffic Flow to Route DNS Traffic</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p> <b>Create, Delete, and Upsert</b> </p> <p>Use <code>ChangeResourceRecordsSetsRequest</code> to perform the following actions:</p> <ul> <li> <p> <code>CREATE</code>: Creates a resource record set that has the specified values.</p> </li> <li> <p> <code>DELETE</code>: Deletes an existing resource record set that has the specified values.</p> </li> <li> <p> <code>UPSERT</code>: If a resource record set does not already exist, AWS creates it. If a resource set does exist, Amazon Route 53 updates it with the values in the request. </p> </li> </ul> <p> <b>Syntaxes for Creating, Updating, and Deleting Resource Record Sets</b> </p> <p>The syntax for a request depends on the type of resource record set that you want to create, delete, or update, such as weighted, alias, or failover. The XML elements in your request must appear in the order listed in the syntax. </p> <p>For an example for each type of resource record set, see "Examples."</p> <p>Don't refer to the syntax in the "Parameter Syntax" section, which includes all of the elements for every kind of resource record set that you can create, delete, or update by using <code>ChangeResourceRecordSets</code>. </p> <p> <b>Change Propagation to Amazon Route 53 DNS Servers</b> </p> <p>When you submit a <code>ChangeResourceRecordSets</code> request, Amazon Route 53 propagates your changes to all of the Amazon Route 53 authoritative DNS servers. While your changes are propagating, <code>GetChange</code> returns a status of <code>PENDING</code>. When propagation is complete, <code>GetChange</code> returns a status of <code>INSYNC</code>. Changes generally propagate to all Amazon Route 53 name servers within 60 seconds. For more information, see <a>GetChange</a>.</p> <p> <b>Limits on ChangeResourceRecordSets Requests</b> </p> <p>For information about the limits on a <code>ChangeResourceRecordSets</code> request, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>
changeResourceRecordSets :: forall eff. ChangeResourceRecordSetsRequest -> Aff (err :: AWS.RequestError | eff) ChangeResourceRecordSetsResponse
changeResourceRecordSets = AWS.request serviceName "changeResourceRecordSets" 


-- | <p>Adds, edits, or deletes tags for a health check or a hosted zone.</p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>
changeTagsForResource :: forall eff. ChangeTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ChangeTagsForResourceResponse
changeTagsForResource = AWS.request serviceName "changeTagsForResource" 


-- | <p>Creates a new health check.</p> <p>For information about adding health checks to resource record sets, see <a>ResourceRecordSet$HealthCheckId</a> in <a>ChangeResourceRecordSets</a>. </p> <p> <b>ELB Load Balancers</b> </p> <p>If you're registering EC2 instances with an Elastic Load Balancing (ELB) load balancer, do not create Amazon Route 53 health checks for the EC2 instances. When you register an EC2 instance with a load balancer, you configure settings for an ELB health check, which performs a similar function to an Amazon Route 53 health check.</p> <p> <b>Private Hosted Zones</b> </p> <p>You can associate health checks with failover resource record sets in a private hosted zone. Note the following:</p> <ul> <li> <p>Amazon Route 53 health checkers are outside the VPC. To check the health of an endpoint within a VPC by IP address, you must assign a public IP address to the instance in the VPC.</p> </li> <li> <p>You can configure a health checker to check the health of an external resource that the instance relies on, such as a database server.</p> </li> <li> <p>You can create a CloudWatch metric, associate an alarm with the metric, and then create a health check that is based on the state of the alarm. For example, you might create a CloudWatch metric that checks the status of the Amazon EC2 <code>StatusCheckFailed</code> metric, add an alarm to the metric, and then create a health check that is based on the state of the alarm. For information about creating CloudWatch metrics and alarms by using the CloudWatch console, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/WhatIsCloudWatch.html">Amazon CloudWatch User Guide</a>.</p> </li> </ul>
createHealthCheck :: forall eff. CreateHealthCheckRequest -> Aff (err :: AWS.RequestError | eff) CreateHealthCheckResponse
createHealthCheck = AWS.request serviceName "createHealthCheck" 


-- | <p>Creates a new public hosted zone, which you use to specify how the Domain Name System (DNS) routes traffic on the Internet for a domain, such as example.com, and its subdomains. </p> <important> <p>You can't convert a public hosted zones to a private hosted zone or vice versa. Instead, you must create a new hosted zone with the same name and create new resource record sets.</p> </important> <p>For more information about charges for hosted zones, see <a href="http://aws.amazon.com/route53/pricing/">Amazon Route 53 Pricing</a>.</p> <p>Note the following:</p> <ul> <li> <p>You can't create a hosted zone for a top-level domain (TLD).</p> </li> <li> <p>Amazon Route 53 automatically creates a default SOA record and four NS records for the zone. For more information about SOA and NS records, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/SOA-NSrecords.html">NS and SOA Records that Amazon Route 53 Creates for a Hosted Zone</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>If you want to use the same name servers for multiple hosted zones, you can optionally associate a reusable delegation set with the hosted zone. See the <code>DelegationSetId</code> element.</p> </li> <li> <p>If your domain is registered with a registrar other than Amazon Route 53, you must update the name servers with your registrar to make Amazon Route 53 your DNS service. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/creating-migrating.html">Configuring Amazon Route 53 as your DNS Service</a> in the <i>Amazon Route 53 Developer Guide</i>. </p> </li> </ul> <p>When you submit a <code>CreateHostedZone</code> request, the initial status of the hosted zone is <code>PENDING</code>. This means that the NS and SOA records are not yet available on all Amazon Route 53 DNS servers. When the NS and SOA records are available, the status of the zone changes to <code>INSYNC</code>.</p>
createHostedZone :: forall eff. CreateHostedZoneRequest -> Aff (err :: AWS.RequestError | eff) CreateHostedZoneResponse
createHostedZone = AWS.request serviceName "createHostedZone" 


-- | <p>Creates a configuration for DNS query logging. After you create a query logging configuration, Amazon Route 53 begins to publish log data to an Amazon CloudWatch Logs log group.</p> <p>DNS query logs contain information about the queries that Amazon Route 53 receives for a specified public hosted zone, such as the following:</p> <ul> <li> <p>Amazon Route 53 edge location that responded to the DNS query</p> </li> <li> <p>Domain or subdomain that was requested</p> </li> <li> <p>DNS record type, such as A or AAAA</p> </li> <li> <p>DNS response code, such as <code>NoError</code> or <code>ServFail</code> </p> </li> </ul> <dl> <dt>Log Group and Resource Policy</dt> <dd> <p>Before you create a query logging configuration, perform the following operations.</p> <note> <p>If you create a query logging configuration using the Amazon Route 53 console, Amazon Route 53 performs these operations automatically.</p> </note> <ol> <li> <p>Create a CloudWatch Logs log group, and make note of the ARN, which you specify when you create a query logging configuration. Note the following:</p> <ul> <li> <p>You must create the log group in the us-east-1 region.</p> </li> <li> <p>You must use the same AWS account to create the log group and the hosted zone that you want to configure query logging for.</p> </li> <li> <p>When you create log groups for query logging, we recommend that you use a consistent prefix, for example:</p> <p> <code>/aws/route53/<i>hosted zone name</i> </code> </p> <p>In the next step, you'll create a resource policy, which controls access to one or more log groups and the associated AWS resources, such as Amazon Route 53 hosted zones. There's a limit on the number of resource policies that you can create, so we recommend that you use a consistent prefix so you can use the same resource policy for all the log groups that you create for query logging.</p> </li> </ul> </li> <li> <p>Create a CloudWatch Logs resource policy, and give it the permissions that Amazon Route 53 needs to create log streams and to send query logs to log streams. For the value of <code>Resource</code>, specify the ARN for the log group that you created in the previous step. To use the same resource policy for all the CloudWatch Logs log groups that you created for query logging configurations, replace the hosted zone name with <code>*</code>, for example:</p> <p> <code>arn:aws:logs:us-east-1:123412341234:log-group:/aws/route53/*</code> </p> <note> <p>You can't use the CloudWatch console to create or edit a resource policy. You must use the CloudWatch API, one of the AWS SDKs, or the AWS CLI.</p> </note> </li> </ol> </dd> <dt>Log Streams and Edge Locations</dt> <dd> <p>When Amazon Route 53 finishes creating the configuration for DNS query logging, it does the following:</p> <ul> <li> <p>Creates a log stream for an edge location the first time that the edge location responds to DNS queries for the specified hosted zone. That log stream is used to log all queries that Amazon Route 53 responds to for that edge location.</p> </li> <li> <p>Begins to send query logs to the applicable log stream.</p> </li> </ul> <p>The name of each log stream is in the following format:</p> <p> <code> <i>hosted zone ID</i>/<i>edge location code</i> </code> </p> <p>The edge location code is a three-letter code and an arbitrarily assigned number, for example, DFW3. The three-letter code typically corresponds with the International Air Transport Association airport code for an airport near the edge location. (These abbreviations might change in the future.) For a list of edge locations, see "The Amazon Route 53 Global Network" on the <a href="http://aws.amazon.com/route53/details/">Amazon Route 53 Product Details</a> page.</p> </dd> <dt>Queries That Are Logged</dt> <dd> <p>Query logs contain only the queries that DNS resolvers forward to Amazon Route 53. If a DNS resolver has already cached the response to a query (such as the IP address for a load balancer for example.com), the resolver will continue to return the cached response. It doesn't forward another query to Amazon Route 53 until the TTL for the corresponding resource record set expires. Depending on how many DNS queries are submitted for a resource record set, and depending on the TTL for that resource record set, query logs might contain information about only one query out of every several thousand queries that are submitted to DNS. For more information about how DNS works, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/welcome-dns-service.html">Routing Internet Traffic to Your Website or Web Application</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </dd> <dt>Log File Format</dt> <dd> <p>For a list of the values in each query log and the format of each value, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </dd> <dt>Pricing</dt> <dd> <p>For information about charges for query logs, see <a href="http://aws.amazon.com/cloudwatch/pricing/">Amazon CloudWatch Pricing</a>.</p> </dd> <dt>How to Stop Logging</dt> <dd> <p>If you want Amazon Route 53 to stop sending query logs to CloudWatch Logs, delete the query logging configuration. For more information, see <a>DeleteQueryLoggingConfig</a>.</p> </dd> </dl>
createQueryLoggingConfig :: forall eff. CreateQueryLoggingConfigRequest -> Aff (err :: AWS.RequestError | eff) CreateQueryLoggingConfigResponse
createQueryLoggingConfig = AWS.request serviceName "createQueryLoggingConfig" 


-- | <p>Creates a delegation set (a group of four name servers) that can be reused by multiple hosted zones. If a hosted zoned ID is specified, <code>CreateReusableDelegationSet</code> marks the delegation set associated with that zone as reusable.</p> <note> <p>You can't associate a reusable delegation set with a private hosted zone.</p> </note> <p>For information about using a reusable delegation set to configure white label name servers, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/white-label-name-servers.html">Configuring White Label Name Servers</a>.</p> <p>The process for migrating existing hosted zones to use a reusable delegation set is comparable to the process for configuring white label name servers. You need to perform the following steps:</p> <ol> <li> <p>Create a reusable delegation set.</p> </li> <li> <p>Recreate hosted zones, and reduce the TTL to 60 seconds or less.</p> </li> <li> <p>Recreate resource record sets in the new hosted zones.</p> </li> <li> <p>Change the registrar's name servers to use the name servers for the new hosted zones.</p> </li> <li> <p>Monitor traffic for the website or application.</p> </li> <li> <p>Change TTLs back to their original values.</p> </li> </ol> <p>If you want to migrate existing hosted zones to use a reusable delegation set, the existing hosted zones can't use any of the name servers that are assigned to the reusable delegation set. If one or more hosted zones do use one or more name servers that are assigned to the reusable delegation set, you can do one of the following:</p> <ul> <li> <p>For small numbers of hosted zones—up to a few hundred—it's relatively easy to create reusable delegation sets until you get one that has four name servers that don't overlap with any of the name servers in your hosted zones.</p> </li> <li> <p>For larger numbers of hosted zones, the easiest solution is to use more than one reusable delegation set.</p> </li> <li> <p>For larger numbers of hosted zones, you can also migrate hosted zones that have overlapping name servers to hosted zones that don't have overlapping name servers, then migrate the hosted zones again to use the reusable delegation set.</p> </li> </ul>
createReusableDelegationSet :: forall eff. CreateReusableDelegationSetRequest -> Aff (err :: AWS.RequestError | eff) CreateReusableDelegationSetResponse
createReusableDelegationSet = AWS.request serviceName "createReusableDelegationSet" 


-- | <p>Creates a traffic policy, which you use to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com).</p>
createTrafficPolicy :: forall eff. CreateTrafficPolicyRequest -> Aff (err :: AWS.RequestError | eff) CreateTrafficPolicyResponse
createTrafficPolicy = AWS.request serviceName "createTrafficPolicy" 


-- | <p>Creates resource record sets in a specified hosted zone based on the settings in a specified traffic policy version. In addition, <code>CreateTrafficPolicyInstance</code> associates the resource record sets with a specified domain name (such as example.com) or subdomain name (such as www.example.com). Amazon Route 53 responds to DNS queries for the domain or subdomain name by using the resource record sets that <code>CreateTrafficPolicyInstance</code> created.</p>
createTrafficPolicyInstance :: forall eff. CreateTrafficPolicyInstanceRequest -> Aff (err :: AWS.RequestError | eff) CreateTrafficPolicyInstanceResponse
createTrafficPolicyInstance = AWS.request serviceName "createTrafficPolicyInstance" 


-- | <p>Creates a new version of an existing traffic policy. When you create a new version of a traffic policy, you specify the ID of the traffic policy that you want to update and a JSON-formatted document that describes the new version. You use traffic policies to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com). You can create a maximum of 1000 versions of a traffic policy. If you reach the limit and need to create another version, you'll need to start a new traffic policy.</p>
createTrafficPolicyVersion :: forall eff. CreateTrafficPolicyVersionRequest -> Aff (err :: AWS.RequestError | eff) CreateTrafficPolicyVersionResponse
createTrafficPolicyVersion = AWS.request serviceName "createTrafficPolicyVersion" 


-- | <p>Authorizes the AWS account that created a specified VPC to submit an <code>AssociateVPCWithHostedZone</code> request to associate the VPC with a specified hosted zone that was created by a different account. To submit a <code>CreateVPCAssociationAuthorization</code> request, you must use the account that created the hosted zone. After you authorize the association, use the account that created the VPC to submit an <code>AssociateVPCWithHostedZone</code> request.</p> <note> <p>If you want to associate multiple VPCs that you created by using one account with a hosted zone that you created by using a different account, you must submit one authorization request for each VPC.</p> </note>
createVPCAssociationAuthorization :: forall eff. CreateVPCAssociationAuthorizationRequest -> Aff (err :: AWS.RequestError | eff) CreateVPCAssociationAuthorizationResponse
createVPCAssociationAuthorization = AWS.request serviceName "createVPCAssociationAuthorization" 


-- | <p>Deletes a health check.</p> <important> <p>Amazon Route 53 does not prevent you from deleting a health check even if the health check is associated with one or more resource record sets. If you delete a health check and you don't update the associated resource record sets, the future status of the health check can't be predicted and may change. This will affect the routing of DNS queries for your DNS failover configuration. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html#health-checks-deleting.html">Replacing and Deleting Health Checks</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </important>
deleteHealthCheck :: forall eff. DeleteHealthCheckRequest -> Aff (err :: AWS.RequestError | eff) DeleteHealthCheckResponse
deleteHealthCheck = AWS.request serviceName "deleteHealthCheck" 


-- | <p>Deletes a hosted zone.</p> <important> <p>If the name servers for the hosted zone are associated with a domain and if you want to make the domain unavailable on the Internet, we recommend that you delete the name servers from the domain to prevent future DNS queries from possibly being misrouted. If the domain is registered with Amazon Route 53, see <code>UpdateDomainNameservers</code>. If the domain is registered with another registrar, use the method provided by the registrar to delete name servers for the domain.</p> <p>Some domain registries don't allow you to remove all of the name servers for a domain. If the registry for your domain requires one or more name servers, we recommend that you delete the hosted zone only if you transfer DNS service to another service provider, and you replace the name servers for the domain with name servers from the new provider.</p> </important> <p>You can delete a hosted zone only if it contains only the default SOA record and NS resource record sets. If the hosted zone contains other resource record sets, you must delete them before you can delete the hosted zone. If you try to delete a hosted zone that contains other resource record sets, the request fails, and Amazon Route 53 returns a <code>HostedZoneNotEmpty</code> error. For information about deleting records from your hosted zone, see <a>ChangeResourceRecordSets</a>.</p> <p>To verify that the hosted zone has been deleted, do one of the following:</p> <ul> <li> <p>Use the <code>GetHostedZone</code> action to request information about the hosted zone.</p> </li> <li> <p>Use the <code>ListHostedZones</code> action to get a list of the hosted zones associated with the current AWS account.</p> </li> </ul>
deleteHostedZone :: forall eff. DeleteHostedZoneRequest -> Aff (err :: AWS.RequestError | eff) DeleteHostedZoneResponse
deleteHostedZone = AWS.request serviceName "deleteHostedZone" 


-- | <p>Deletes a configuration for DNS query logging. If you delete a configuration, Amazon Route 53 stops sending query logs to CloudWatch Logs. Amazon Route 53 doesn't delete any logs that are already in CloudWatch Logs.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a>.</p>
deleteQueryLoggingConfig :: forall eff. DeleteQueryLoggingConfigRequest -> Aff (err :: AWS.RequestError | eff) DeleteQueryLoggingConfigResponse
deleteQueryLoggingConfig = AWS.request serviceName "deleteQueryLoggingConfig" 


-- | <p>Deletes a reusable delegation set.</p> <important> <p>You can delete a reusable delegation set only if it isn't associated with any hosted zones.</p> </important> <p>To verify that the reusable delegation set is not associated with any hosted zones, submit a <a>GetReusableDelegationSet</a> request and specify the ID of the reusable delegation set that you want to delete.</p>
deleteReusableDelegationSet :: forall eff. DeleteReusableDelegationSetRequest -> Aff (err :: AWS.RequestError | eff) DeleteReusableDelegationSetResponse
deleteReusableDelegationSet = AWS.request serviceName "deleteReusableDelegationSet" 


-- | <p>Deletes a traffic policy.</p>
deleteTrafficPolicy :: forall eff. DeleteTrafficPolicyRequest -> Aff (err :: AWS.RequestError | eff) DeleteTrafficPolicyResponse
deleteTrafficPolicy = AWS.request serviceName "deleteTrafficPolicy" 


-- | <p>Deletes a traffic policy instance and all of the resource record sets that Amazon Route 53 created when you created the instance.</p> <note> <p>In the Amazon Route 53 console, traffic policy instances are known as policy records.</p> </note>
deleteTrafficPolicyInstance :: forall eff. DeleteTrafficPolicyInstanceRequest -> Aff (err :: AWS.RequestError | eff) DeleteTrafficPolicyInstanceResponse
deleteTrafficPolicyInstance = AWS.request serviceName "deleteTrafficPolicyInstance" 


-- | <p>Removes authorization to submit an <code>AssociateVPCWithHostedZone</code> request to associate a specified VPC with a hosted zone that was created by a different account. You must use the account that created the hosted zone to submit a <code>DeleteVPCAssociationAuthorization</code> request.</p> <important> <p>Sending this request only prevents the AWS account that created the VPC from associating the VPC with the Amazon Route 53 hosted zone in the future. If the VPC is already associated with the hosted zone, <code>DeleteVPCAssociationAuthorization</code> won't disassociate the VPC from the hosted zone. If you want to delete an existing association, use <code>DisassociateVPCFromHostedZone</code>.</p> </important>
deleteVPCAssociationAuthorization :: forall eff. DeleteVPCAssociationAuthorizationRequest -> Aff (err :: AWS.RequestError | eff) DeleteVPCAssociationAuthorizationResponse
deleteVPCAssociationAuthorization = AWS.request serviceName "deleteVPCAssociationAuthorization" 


-- | <p>Disassociates a VPC from a Amazon Route 53 private hosted zone. </p> <note> <p>You can't disassociate the last VPC from a private hosted zone.</p> </note> <important> <p>You can't disassociate a VPC from a private hosted zone when only one VPC is associated with the hosted zone. You also can't convert a private hosted zone into a public hosted zone.</p> </important>
disassociateVPCFromHostedZone :: forall eff. DisassociateVPCFromHostedZoneRequest -> Aff (err :: AWS.RequestError | eff) DisassociateVPCFromHostedZoneResponse
disassociateVPCFromHostedZone = AWS.request serviceName "disassociateVPCFromHostedZone" 


-- | <p>Gets the specified limit for the current account, for example, the maximum number of health checks that you can create using the account.</p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>
getAccountLimit :: forall eff. GetAccountLimitRequest -> Aff (err :: AWS.RequestError | eff) GetAccountLimitResponse
getAccountLimit = AWS.request serviceName "getAccountLimit" 


-- | <p>Returns the current status of a change batch request. The status is one of the following values:</p> <ul> <li> <p> <code>PENDING</code> indicates that the changes in this request have not propagated to all Amazon Route 53 DNS servers. This is the initial status of all change batch requests.</p> </li> <li> <p> <code>INSYNC</code> indicates that the changes have propagated to all Amazon Route 53 DNS servers. </p> </li> </ul>
getChange :: forall eff. GetChangeRequest -> Aff (err :: AWS.RequestError | eff) GetChangeResponse
getChange = AWS.request serviceName "getChange" 


-- | <p> <code>GetCheckerIpRanges</code> still works, but we recommend that you download ip-ranges.json, which includes IP address ranges for all AWS services. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/route-53-ip-addresses.html">IP Address Ranges of Amazon Route 53 Servers</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>
getCheckerIpRanges :: forall eff. GetCheckerIpRangesRequest -> Aff (err :: AWS.RequestError | eff) GetCheckerIpRangesResponse
getCheckerIpRanges = AWS.request serviceName "getCheckerIpRanges" 


-- | <p>Gets information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.</p> <p>Use the following syntax to determine whether a continent is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?ContinentCode=<i>two-letter abbreviation for a continent</i> </code> </p> <p>Use the following syntax to determine whether a country is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?CountryCode=<i>two-character country code</i> </code> </p> <p>Use the following syntax to determine whether a subdivision of a country is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?CountryCode=<i>two-character country code</i>&amp;SubdivisionCode=<i>subdivision code</i> </code> </p>
getGeoLocation :: forall eff. GetGeoLocationRequest -> Aff (err :: AWS.RequestError | eff) GetGeoLocationResponse
getGeoLocation = AWS.request serviceName "getGeoLocation" 


-- | <p>Gets information about a specified health check.</p>
getHealthCheck :: forall eff. GetHealthCheckRequest -> Aff (err :: AWS.RequestError | eff) GetHealthCheckResponse
getHealthCheck = AWS.request serviceName "getHealthCheck" 


-- | <p>Retrieves the number of health checks that are associated with the current AWS account.</p>
getHealthCheckCount :: forall eff. GetHealthCheckCountRequest -> Aff (err :: AWS.RequestError | eff) GetHealthCheckCountResponse
getHealthCheckCount = AWS.request serviceName "getHealthCheckCount" 


-- | <p>Gets the reason that a specified health check failed most recently.</p>
getHealthCheckLastFailureReason :: forall eff. GetHealthCheckLastFailureReasonRequest -> Aff (err :: AWS.RequestError | eff) GetHealthCheckLastFailureReasonResponse
getHealthCheckLastFailureReason = AWS.request serviceName "getHealthCheckLastFailureReason" 


-- | <p>Gets status of a specified health check. </p>
getHealthCheckStatus :: forall eff. GetHealthCheckStatusRequest -> Aff (err :: AWS.RequestError | eff) GetHealthCheckStatusResponse
getHealthCheckStatus = AWS.request serviceName "getHealthCheckStatus" 


-- | <p>Gets information about a specified hosted zone including the four name servers assigned to the hosted zone.</p>
getHostedZone :: forall eff. GetHostedZoneRequest -> Aff (err :: AWS.RequestError | eff) GetHostedZoneResponse
getHostedZone = AWS.request serviceName "getHostedZone" 


-- | <p>Retrieves the number of hosted zones that are associated with the current AWS account.</p>
getHostedZoneCount :: forall eff. GetHostedZoneCountRequest -> Aff (err :: AWS.RequestError | eff) GetHostedZoneCountResponse
getHostedZoneCount = AWS.request serviceName "getHostedZoneCount" 


-- | <p>Gets the specified limit for a specified hosted zone, for example, the maximum number of records that you can create in the hosted zone. </p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>
getHostedZoneLimit :: forall eff. GetHostedZoneLimitRequest -> Aff (err :: AWS.RequestError | eff) GetHostedZoneLimitResponse
getHostedZoneLimit = AWS.request serviceName "getHostedZoneLimit" 


-- | <p>Gets information about a specified configuration for DNS query logging.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a> and <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a>.</p>
getQueryLoggingConfig :: forall eff. GetQueryLoggingConfigRequest -> Aff (err :: AWS.RequestError | eff) GetQueryLoggingConfigResponse
getQueryLoggingConfig = AWS.request serviceName "getQueryLoggingConfig" 


-- | <p>Retrieves information about a specified reusable delegation set, including the four name servers that are assigned to the delegation set.</p>
getReusableDelegationSet :: forall eff. GetReusableDelegationSetRequest -> Aff (err :: AWS.RequestError | eff) GetReusableDelegationSetResponse
getReusableDelegationSet = AWS.request serviceName "getReusableDelegationSet" 


-- | <p>Gets the maximum number of hosted zones that you can associate with the specified reusable delegation set.</p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>
getReusableDelegationSetLimit :: forall eff. GetReusableDelegationSetLimitRequest -> Aff (err :: AWS.RequestError | eff) GetReusableDelegationSetLimitResponse
getReusableDelegationSetLimit = AWS.request serviceName "getReusableDelegationSetLimit" 


-- | <p>Gets information about a specific traffic policy version.</p>
getTrafficPolicy :: forall eff. GetTrafficPolicyRequest -> Aff (err :: AWS.RequestError | eff) GetTrafficPolicyResponse
getTrafficPolicy = AWS.request serviceName "getTrafficPolicy" 


-- | <p>Gets information about a specified traffic policy instance.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <note> <p>In the Amazon Route 53 console, traffic policy instances are known as policy records.</p> </note>
getTrafficPolicyInstance :: forall eff. GetTrafficPolicyInstanceRequest -> Aff (err :: AWS.RequestError | eff) GetTrafficPolicyInstanceResponse
getTrafficPolicyInstance = AWS.request serviceName "getTrafficPolicyInstance" 


-- | <p>Gets the number of traffic policy instances that are associated with the current AWS account.</p>
getTrafficPolicyInstanceCount :: forall eff. GetTrafficPolicyInstanceCountRequest -> Aff (err :: AWS.RequestError | eff) GetTrafficPolicyInstanceCountResponse
getTrafficPolicyInstanceCount = AWS.request serviceName "getTrafficPolicyInstanceCount" 


-- | <p>Retrieves a list of supported geo locations.</p> <p>Countries are listed first, and continents are listed last. If Amazon Route 53 supports subdivisions for a country (for example, states or provinces), the subdivisions for that country are listed in alphabetical order immediately after the corresponding country.</p>
listGeoLocations :: forall eff. ListGeoLocationsRequest -> Aff (err :: AWS.RequestError | eff) ListGeoLocationsResponse
listGeoLocations = AWS.request serviceName "listGeoLocations" 


-- | <p>Retrieve a list of the health checks that are associated with the current AWS account. </p>
listHealthChecks :: forall eff. ListHealthChecksRequest -> Aff (err :: AWS.RequestError | eff) ListHealthChecksResponse
listHealthChecks = AWS.request serviceName "listHealthChecks" 


-- | <p>Retrieves a list of the public and private hosted zones that are associated with the current AWS account. The response includes a <code>HostedZones</code> child element for each hosted zone.</p> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of hosted zones, you can use the <code>maxitems</code> parameter to list them in groups of up to 100.</p>
listHostedZones :: forall eff. ListHostedZonesRequest -> Aff (err :: AWS.RequestError | eff) ListHostedZonesResponse
listHostedZones = AWS.request serviceName "listHostedZones" 


-- | <p>Retrieves a list of your hosted zones in lexicographic order. The response includes a <code>HostedZones</code> child element for each hosted zone created by the current AWS account. </p> <p> <code>ListHostedZonesByName</code> sorts hosted zones by name with the labels reversed. For example:</p> <p> <code>com.example.www.</code> </p> <p>Note the trailing dot, which can change the sort order in some circumstances.</p> <p>If the domain name includes escape characters or Punycode, <code>ListHostedZonesByName</code> alphabetizes the domain name using the escaped or Punycoded value, which is the format that Amazon Route 53 saves in its database. For example, to create a hosted zone for exämple.com, you specify ex\344mple.com for the domain name. <code>ListHostedZonesByName</code> alphabetizes it as:</p> <p> <code>com.ex\344mple.</code> </p> <p>The labels are reversed and alphabetized using the escaped value. For more information about valid domain name formats, including internationalized domain names, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html">DNS Domain Name Format</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>Amazon Route 53 returns up to 100 items in each response. If you have a lot of hosted zones, use the <code>MaxItems</code> parameter to list them in groups of up to 100. The response includes values that help navigate from one group of <code>MaxItems</code> hosted zones to the next:</p> <ul> <li> <p>The <code>DNSName</code> and <code>HostedZoneId</code> elements in the response contain the values, if any, specified for the <code>dnsname</code> and <code>hostedzoneid</code> parameters in the request that produced the current response.</p> </li> <li> <p>The <code>MaxItems</code> element in the response contains the value, if any, that you specified for the <code>maxitems</code> parameter in the request that produced the current response.</p> </li> <li> <p>If the value of <code>IsTruncated</code> in the response is true, there are more hosted zones associated with the current AWS account. </p> <p>If <code>IsTruncated</code> is false, this response includes the last hosted zone that is associated with the current account. The <code>NextDNSName</code> element and <code>NextHostedZoneId</code> elements are omitted from the response.</p> </li> <li> <p>The <code>NextDNSName</code> and <code>NextHostedZoneId</code> elements in the response contain the domain name and the hosted zone ID of the next hosted zone that is associated with the current AWS account. If you want to list more hosted zones, make another call to <code>ListHostedZonesByName</code>, and specify the value of <code>NextDNSName</code> and <code>NextHostedZoneId</code> in the <code>dnsname</code> and <code>hostedzoneid</code> parameters, respectively.</p> </li> </ul>
listHostedZonesByName :: forall eff. ListHostedZonesByNameRequest -> Aff (err :: AWS.RequestError | eff) ListHostedZonesByNameResponse
listHostedZonesByName = AWS.request serviceName "listHostedZonesByName" 


-- | <p>Lists the configurations for DNS query logging that are associated with the current AWS account or the configuration that is associated with a specified hosted zone.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a>. Additional information, including the format of DNS query logs, appears in <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>
listQueryLoggingConfigs :: forall eff. ListQueryLoggingConfigsRequest -> Aff (err :: AWS.RequestError | eff) ListQueryLoggingConfigsResponse
listQueryLoggingConfigs = AWS.request serviceName "listQueryLoggingConfigs" 


-- | <p>Lists the resource record sets in a specified hosted zone.</p> <p> <code>ListResourceRecordSets</code> returns up to 100 resource record sets at a time in ASCII order, beginning at a position specified by the <code>name</code> and <code>type</code> elements. The action sorts results first by DNS name with the labels reversed, for example:</p> <p> <code>com.example.www.</code> </p> <p>Note the trailing dot, which can change the sort order in some circumstances.</p> <p>When multiple records have the same DNS name, the action sorts results by the record type.</p> <p>You can use the name and type elements to adjust the beginning position of the list of resource record sets returned:</p> <dl> <dt>If you do not specify Name or Type</dt> <dd> <p>The results begin with the first resource record set that the hosted zone contains.</p> </dd> <dt>If you specify Name but not Type</dt> <dd> <p>The results begin with the first resource record set in the list whose name is greater than or equal to <code>Name</code>.</p> </dd> <dt>If you specify Type but not Name</dt> <dd> <p>Amazon Route 53 returns the <code>InvalidInput</code> error.</p> </dd> <dt>If you specify both Name and Type</dt> <dd> <p>The results begin with the first resource record set in the list whose name is greater than or equal to <code>Name</code>, and whose type is greater than or equal to <code>Type</code>.</p> </dd> </dl> <p>This action returns the most current version of the records. This includes records that are <code>PENDING</code>, and that are not yet available on all Amazon Route 53 DNS servers.</p> <p>To ensure that you get an accurate listing of the resource record sets for a hosted zone at a point in time, do not submit a <code>ChangeResourceRecordSets</code> request while you're paging through the results of a <code>ListResourceRecordSets</code> request. If you do, some pages may display results without the latest changes while other pages display results with the latest changes.</p>
listResourceRecordSets :: forall eff. ListResourceRecordSetsRequest -> Aff (err :: AWS.RequestError | eff) ListResourceRecordSetsResponse
listResourceRecordSets = AWS.request serviceName "listResourceRecordSets" 


-- | <p>Retrieves a list of the reusable delegation sets that are associated with the current AWS account.</p>
listReusableDelegationSets :: forall eff. ListReusableDelegationSetsRequest -> Aff (err :: AWS.RequestError | eff) ListReusableDelegationSetsResponse
listReusableDelegationSets = AWS.request serviceName "listReusableDelegationSets" 


-- | <p>Lists tags for one health check or hosted zone. </p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResponse
listTagsForResource = AWS.request serviceName "listTagsForResource" 


-- | <p>Lists tags for up to 10 health checks or hosted zones.</p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>
listTagsForResources :: forall eff. ListTagsForResourcesRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourcesResponse
listTagsForResources = AWS.request serviceName "listTagsForResources" 


-- | <p>Gets information about the latest version for every traffic policy that is associated with the current AWS account. Policies are listed in the order in which they were created. </p>
listTrafficPolicies :: forall eff. ListTrafficPoliciesRequest -> Aff (err :: AWS.RequestError | eff) ListTrafficPoliciesResponse
listTrafficPolicies = AWS.request serviceName "listTrafficPolicies" 


-- | <p>Gets information about the traffic policy instances that you created by using the current AWS account.</p> <note> <p>After you submit an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>
listTrafficPolicyInstances :: forall eff. ListTrafficPolicyInstancesRequest -> Aff (err :: AWS.RequestError | eff) ListTrafficPolicyInstancesResponse
listTrafficPolicyInstances = AWS.request serviceName "listTrafficPolicyInstances" 


-- | <p>Gets information about the traffic policy instances that you created in a specified hosted zone.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>
listTrafficPolicyInstancesByHostedZone :: forall eff. ListTrafficPolicyInstancesByHostedZoneRequest -> Aff (err :: AWS.RequestError | eff) ListTrafficPolicyInstancesByHostedZoneResponse
listTrafficPolicyInstancesByHostedZone = AWS.request serviceName "listTrafficPolicyInstancesByHostedZone" 


-- | <p>Gets information about the traffic policy instances that you created by using a specify traffic policy version.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>
listTrafficPolicyInstancesByPolicy :: forall eff. ListTrafficPolicyInstancesByPolicyRequest -> Aff (err :: AWS.RequestError | eff) ListTrafficPolicyInstancesByPolicyResponse
listTrafficPolicyInstancesByPolicy = AWS.request serviceName "listTrafficPolicyInstancesByPolicy" 


-- | <p>Gets information about all of the versions for a specified traffic policy.</p> <p>Traffic policy versions are listed in numerical order by <code>VersionNumber</code>.</p>
listTrafficPolicyVersions :: forall eff. ListTrafficPolicyVersionsRequest -> Aff (err :: AWS.RequestError | eff) ListTrafficPolicyVersionsResponse
listTrafficPolicyVersions = AWS.request serviceName "listTrafficPolicyVersions" 


-- | <p>Gets a list of the VPCs that were created by other accounts and that can be associated with a specified hosted zone because you've submitted one or more <code>CreateVPCAssociationAuthorization</code> requests. </p> <p>The response includes a <code>VPCs</code> element with a <code>VPC</code> child element for each VPC that can be associated with the hosted zone.</p>
listVPCAssociationAuthorizations :: forall eff. ListVPCAssociationAuthorizationsRequest -> Aff (err :: AWS.RequestError | eff) ListVPCAssociationAuthorizationsResponse
listVPCAssociationAuthorizations = AWS.request serviceName "listVPCAssociationAuthorizations" 


-- | <p>Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask. </p>
testDNSAnswer :: forall eff. TestDNSAnswerRequest -> Aff (err :: AWS.RequestError | eff) TestDNSAnswerResponse
testDNSAnswer = AWS.request serviceName "testDNSAnswer" 


-- | <p>Updates an existing health check. Note that some values can't be updated. </p> <p>For more information about updating health checks, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html">Creating, Updating, and Deleting Health Checks</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>
updateHealthCheck :: forall eff. UpdateHealthCheckRequest -> Aff (err :: AWS.RequestError | eff) UpdateHealthCheckResponse
updateHealthCheck = AWS.request serviceName "updateHealthCheck" 


-- | <p>Updates the comment for a specified hosted zone.</p>
updateHostedZoneComment :: forall eff. UpdateHostedZoneCommentRequest -> Aff (err :: AWS.RequestError | eff) UpdateHostedZoneCommentResponse
updateHostedZoneComment = AWS.request serviceName "updateHostedZoneComment" 


-- | <p>Updates the comment for a specified traffic policy version.</p>
updateTrafficPolicyComment :: forall eff. UpdateTrafficPolicyCommentRequest -> Aff (err :: AWS.RequestError | eff) UpdateTrafficPolicyCommentResponse
updateTrafficPolicyComment = AWS.request serviceName "updateTrafficPolicyComment" 


-- | <p>Updates the resource record sets in a specified hosted zone that were created based on the settings in a specified traffic policy version.</p> <p>When you update a traffic policy instance, Amazon Route 53 continues to respond to DNS queries for the root resource record set name (such as example.com) while it replaces one group of resource record sets with another. Amazon Route 53 performs the following operations:</p> <ol> <li> <p>Amazon Route 53 creates a new group of resource record sets based on the specified traffic policy. This is true regardless of how significant the differences are between the existing resource record sets and the new resource record sets. </p> </li> <li> <p>When all of the new resource record sets have been created, Amazon Route 53 starts to respond to DNS queries for the root resource record set name (such as example.com) by using the new resource record sets.</p> </li> <li> <p>Amazon Route 53 deletes the old group of resource record sets that are associated with the root resource record set name.</p> </li> </ol>
updateTrafficPolicyInstance :: forall eff. UpdateTrafficPolicyInstanceRequest -> Aff (err :: AWS.RequestError | eff) UpdateTrafficPolicyInstanceResponse
updateTrafficPolicyInstance = AWS.request serviceName "updateTrafficPolicyInstance" 


-- | <p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>
newtype AccountLimit = AccountLimit 
  { "Type" :: (AccountLimitType)
  , "Value" :: (LimitValue)
  }
derive instance newtypeAccountLimit :: Newtype AccountLimit _


newtype AccountLimitType = AccountLimitType String
derive instance newtypeAccountLimitType :: Newtype AccountLimitType _


-- | <p>A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.</p>
newtype AlarmIdentifier = AlarmIdentifier 
  { "Region" :: (CloudWatchRegion)
  , "Name" :: (AlarmName)
  }
derive instance newtypeAlarmIdentifier :: Newtype AlarmIdentifier _


newtype AlarmName = AlarmName String
derive instance newtypeAlarmName :: Newtype AlarmName _


newtype AliasHealthEnabled = AliasHealthEnabled Boolean
derive instance newtypeAliasHealthEnabled :: Newtype AliasHealthEnabled _


-- | <p> <i>Alias resource record sets only:</i> Information about the CloudFront distribution, Elastic Beanstalk environment, ELB load balancer, Amazon S3 bucket, or Amazon Route 53 resource record set that you're redirecting queries to. An Elastic Beanstalk environment must have a regionalized subdomain.</p> <p>When creating resource record sets for a private hosted zone, note the following:</p> <ul> <li> <p>Resource record sets can't be created for CloudFront distributions in a private hosted zone.</p> </li> <li> <p>Creating geolocation alias resource record sets or latency alias resource record sets in a private hosted zone is unsupported.</p> </li> <li> <p>For information about creating failover resource record sets in a private hosted zone, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html">Configuring Failover in a Private Hosted Zone</a>.</p> </li> </ul>
newtype AliasTarget = AliasTarget 
  { "HostedZoneId" :: (ResourceId)
  , "DNSName" :: (DNSName)
  , "EvaluateTargetHealth" :: (AliasHealthEnabled)
  }
derive instance newtypeAliasTarget :: Newtype AliasTarget _


newtype AssociateVPCComment = AssociateVPCComment String
derive instance newtypeAssociateVPCComment :: Newtype AssociateVPCComment _


-- | <p>A complex type that contains information about the request to associate a VPC with a private hosted zone.</p>
newtype AssociateVPCWithHostedZoneRequest = AssociateVPCWithHostedZoneRequest 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  , "Comment" :: NullOrUndefined (AssociateVPCComment)
  }
derive instance newtypeAssociateVPCWithHostedZoneRequest :: Newtype AssociateVPCWithHostedZoneRequest _


-- | <p>A complex type that contains the response information for the <code>AssociateVPCWithHostedZone</code> request.</p>
newtype AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeAssociateVPCWithHostedZoneResponse :: Newtype AssociateVPCWithHostedZoneResponse _


-- | <p>The information for each resource record set that you want to change.</p>
newtype Change = Change 
  { "Action" :: (ChangeAction)
  , "ResourceRecordSet" :: (ResourceRecordSet)
  }
derive instance newtypeChange :: Newtype Change _


newtype ChangeAction = ChangeAction String
derive instance newtypeChangeAction :: Newtype ChangeAction _


-- | <p>The information for a change request.</p>
newtype ChangeBatch = ChangeBatch 
  { "Comment" :: NullOrUndefined (ResourceDescription)
  , "Changes" :: (Changes)
  }
derive instance newtypeChangeBatch :: Newtype ChangeBatch _


-- | <p>A complex type that describes change information about changes made to your hosted zone.</p>
newtype ChangeInfo = ChangeInfo 
  { "Id" :: (ResourceId)
  , "Status" :: (ChangeStatus)
  , "SubmittedAt" :: (TimeStamp)
  , "Comment" :: NullOrUndefined (ResourceDescription)
  }
derive instance newtypeChangeInfo :: Newtype ChangeInfo _


-- | <p>A complex type that contains change information for the resource record set.</p>
newtype ChangeResourceRecordSetsRequest = ChangeResourceRecordSetsRequest 
  { "HostedZoneId" :: (ResourceId)
  , "ChangeBatch" :: (ChangeBatch)
  }
derive instance newtypeChangeResourceRecordSetsRequest :: Newtype ChangeResourceRecordSetsRequest _


-- | <p>A complex type containing the response for the request.</p>
newtype ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeChangeResourceRecordSetsResponse :: Newtype ChangeResourceRecordSetsResponse _


newtype ChangeStatus = ChangeStatus String
derive instance newtypeChangeStatus :: Newtype ChangeStatus _


-- | <p>A complex type that contains information about the tags that you want to add, edit, or delete.</p>
newtype ChangeTagsForResourceRequest = ChangeTagsForResourceRequest 
  { "ResourceType" :: (TagResourceType)
  , "ResourceId" :: (TagResourceId)
  , "AddTags" :: NullOrUndefined (TagList)
  , "RemoveTagKeys" :: NullOrUndefined (TagKeyList)
  }
derive instance newtypeChangeTagsForResourceRequest :: Newtype ChangeTagsForResourceRequest _


-- | <p>Empty response for the request.</p>
newtype ChangeTagsForResourceResponse = ChangeTagsForResourceResponse 
  { 
  }
derive instance newtypeChangeTagsForResourceResponse :: Newtype ChangeTagsForResourceResponse _


newtype Changes = Changes (Array Change)
derive instance newtypeChanges :: Newtype Changes _


newtype CheckerIpRanges = CheckerIpRanges (Array IPAddressCidr)
derive instance newtypeCheckerIpRanges :: Newtype CheckerIpRanges _


newtype ChildHealthCheckList = ChildHealthCheckList (Array HealthCheckId)
derive instance newtypeChildHealthCheckList :: Newtype ChildHealthCheckList _


-- | <p>A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.</p>
newtype CloudWatchAlarmConfiguration = CloudWatchAlarmConfiguration 
  { "EvaluationPeriods" :: (EvaluationPeriods)
  , "Threshold" :: (Threshold)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "Period" :: (Period)
  , "MetricName" :: (MetricName)
  , "Namespace" :: (Namespace)
  , "Statistic" :: (Statistic)
  , "Dimensions" :: NullOrUndefined (DimensionList)
  }
derive instance newtypeCloudWatchAlarmConfiguration :: Newtype CloudWatchAlarmConfiguration _


newtype CloudWatchLogsLogGroupArn = CloudWatchLogsLogGroupArn String
derive instance newtypeCloudWatchLogsLogGroupArn :: Newtype CloudWatchLogsLogGroupArn _


newtype CloudWatchRegion = CloudWatchRegion String
derive instance newtypeCloudWatchRegion :: Newtype CloudWatchRegion _


newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _


-- | <p>Another user submitted a request to create, update, or delete the object at the same time that you did. Retry the request. </p>
newtype ConcurrentModification = ConcurrentModification 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModification :: Newtype ConcurrentModification _


-- | <p>The cause of this error depends on whether you're trying to create a public or a private hosted zone:</p> <ul> <li> <p> <b>Public hosted zone:</b> Two hosted zones that have the same name or that have a parent/child relationship (example.com and test.example.com) can't have any common name servers. You tried to create a hosted zone that has the same name as an existing hosted zone or that's the parent or child of an existing hosted zone, and you specified a delegation set that shares one or more name servers with the existing hosted zone. For more information, see <a>CreateReusableDelegationSet</a>.</p> </li> <li> <p> <b>Private hosted zone:</b> You specified an Amazon VPC that you're already using for another hosted zone, and the domain that you specified for one of the hosted zones is a subdomain of the domain that you specified for the other hosted zone. For example, you can't use the same Amazon VPC for the hosted zones for example.com and test.example.com.</p> </li> </ul>
newtype ConflictingDomainExists = ConflictingDomainExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConflictingDomainExists :: Newtype ConflictingDomainExists _


-- | <p>You tried to update a traffic policy instance by using a traffic policy version that has a different DNS type than the current type for the instance. You specified the type in the JSON document in the <code>CreateTrafficPolicy</code> or <code>CreateTrafficPolicyVersion</code>request. </p>
newtype ConflictingTypes = ConflictingTypes 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConflictingTypes :: Newtype ConflictingTypes _


-- | <p>A complex type that contains the health check request information.</p>
newtype CreateHealthCheckRequest = CreateHealthCheckRequest 
  { "CallerReference" :: (HealthCheckNonce)
  , "HealthCheckConfig" :: (HealthCheckConfig)
  }
derive instance newtypeCreateHealthCheckRequest :: Newtype CreateHealthCheckRequest _


-- | <p>A complex type containing the response information for the new health check.</p>
newtype CreateHealthCheckResponse = CreateHealthCheckResponse 
  { "HealthCheck" :: (HealthCheck)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateHealthCheckResponse :: Newtype CreateHealthCheckResponse _


-- | <p>A complex type that contains information about the request to create a hosted zone.</p>
newtype CreateHostedZoneRequest = CreateHostedZoneRequest 
  { "Name" :: (DNSName)
  , "VPC" :: NullOrUndefined (VPC)
  , "CallerReference" :: (Nonce)
  , "HostedZoneConfig" :: NullOrUndefined (HostedZoneConfig)
  , "DelegationSetId" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeCreateHostedZoneRequest :: Newtype CreateHostedZoneRequest _


-- | <p>A complex type containing the response information for the hosted zone.</p>
newtype CreateHostedZoneResponse = CreateHostedZoneResponse 
  { "HostedZone" :: (HostedZone)
  , "ChangeInfo" :: (ChangeInfo)
  , "DelegationSet" :: (DelegationSet)
  , "VPC" :: NullOrUndefined (VPC)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateHostedZoneResponse :: Newtype CreateHostedZoneResponse _


newtype CreateQueryLoggingConfigRequest = CreateQueryLoggingConfigRequest 
  { "HostedZoneId" :: (ResourceId)
  , "CloudWatchLogsLogGroupArn" :: (CloudWatchLogsLogGroupArn)
  }
derive instance newtypeCreateQueryLoggingConfigRequest :: Newtype CreateQueryLoggingConfigRequest _


newtype CreateQueryLoggingConfigResponse = CreateQueryLoggingConfigResponse 
  { "QueryLoggingConfig" :: (QueryLoggingConfig)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateQueryLoggingConfigResponse :: Newtype CreateQueryLoggingConfigResponse _


newtype CreateReusableDelegationSetRequest = CreateReusableDelegationSetRequest 
  { "CallerReference" :: (Nonce)
  , "HostedZoneId" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeCreateReusableDelegationSetRequest :: Newtype CreateReusableDelegationSetRequest _


newtype CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse 
  { "DelegationSet" :: (DelegationSet)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateReusableDelegationSetResponse :: Newtype CreateReusableDelegationSetResponse _


-- | <p>A complex type that contains information about the resource record sets that you want to create based on a specified traffic policy.</p>
newtype CreateTrafficPolicyInstanceRequest = CreateTrafficPolicyInstanceRequest 
  { "HostedZoneId" :: (ResourceId)
  , "Name" :: (DNSName)
  , "TTL" :: (TTL)
  , "TrafficPolicyId" :: (TrafficPolicyId)
  , "TrafficPolicyVersion" :: (TrafficPolicyVersion)
  }
derive instance newtypeCreateTrafficPolicyInstanceRequest :: Newtype CreateTrafficPolicyInstanceRequest _


-- | <p>A complex type that contains the response information for the <code>CreateTrafficPolicyInstance</code> request.</p>
newtype CreateTrafficPolicyInstanceResponse = CreateTrafficPolicyInstanceResponse 
  { "TrafficPolicyInstance" :: (TrafficPolicyInstance)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateTrafficPolicyInstanceResponse :: Newtype CreateTrafficPolicyInstanceResponse _


-- | <p>A complex type that contains information about the traffic policy that you want to create.</p>
newtype CreateTrafficPolicyRequest = CreateTrafficPolicyRequest 
  { "Name" :: (TrafficPolicyName)
  , "Document" :: (TrafficPolicyDocument)
  , "Comment" :: NullOrUndefined (TrafficPolicyComment)
  }
derive instance newtypeCreateTrafficPolicyRequest :: Newtype CreateTrafficPolicyRequest _


-- | <p>A complex type that contains the response information for the <code>CreateTrafficPolicy</code> request.</p>
newtype CreateTrafficPolicyResponse = CreateTrafficPolicyResponse 
  { "TrafficPolicy" :: (TrafficPolicy)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateTrafficPolicyResponse :: Newtype CreateTrafficPolicyResponse _


-- | <p>A complex type that contains information about the traffic policy that you want to create a new version for.</p>
newtype CreateTrafficPolicyVersionRequest = CreateTrafficPolicyVersionRequest 
  { "Id" :: (TrafficPolicyId)
  , "Document" :: (TrafficPolicyDocument)
  , "Comment" :: NullOrUndefined (TrafficPolicyComment)
  }
derive instance newtypeCreateTrafficPolicyVersionRequest :: Newtype CreateTrafficPolicyVersionRequest _


-- | <p>A complex type that contains the response information for the <code>CreateTrafficPolicyVersion</code> request.</p>
newtype CreateTrafficPolicyVersionResponse = CreateTrafficPolicyVersionResponse 
  { "TrafficPolicy" :: (TrafficPolicy)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateTrafficPolicyVersionResponse :: Newtype CreateTrafficPolicyVersionResponse _


-- | <p>A complex type that contains information about the request to authorize associating a VPC with your private hosted zone. Authorization is only required when a private hosted zone and a VPC were created by using different accounts.</p>
newtype CreateVPCAssociationAuthorizationRequest = CreateVPCAssociationAuthorizationRequest 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  }
derive instance newtypeCreateVPCAssociationAuthorizationRequest :: Newtype CreateVPCAssociationAuthorizationRequest _


-- | <p>A complex type that contains the response information from a <code>CreateVPCAssociationAuthorization</code> request.</p>
newtype CreateVPCAssociationAuthorizationResponse = CreateVPCAssociationAuthorizationResponse 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  }
derive instance newtypeCreateVPCAssociationAuthorizationResponse :: Newtype CreateVPCAssociationAuthorizationResponse _


newtype DNSName = DNSName String
derive instance newtypeDNSName :: Newtype DNSName _


newtype DNSRCode = DNSRCode String
derive instance newtypeDNSRCode :: Newtype DNSRCode _


-- | <p>A complex type that lists the name servers in a delegation set, as well as the <code>CallerReference</code> and the <code>ID</code> for the delegation set.</p>
newtype DelegationSet = DelegationSet 
  { "Id" :: NullOrUndefined (ResourceId)
  , "CallerReference" :: NullOrUndefined (Nonce)
  , "NameServers" :: (DelegationSetNameServers)
  }
derive instance newtypeDelegationSet :: Newtype DelegationSet _


-- | <p>A delegation set with the same owner and caller reference combination has already been created.</p>
newtype DelegationSetAlreadyCreated = DelegationSetAlreadyCreated 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetAlreadyCreated :: Newtype DelegationSetAlreadyCreated _


-- | <p>The specified delegation set has already been marked as reusable.</p>
newtype DelegationSetAlreadyReusable = DelegationSetAlreadyReusable 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetAlreadyReusable :: Newtype DelegationSetAlreadyReusable _


-- | <p>The specified delegation contains associated hosted zones which must be deleted before the reusable delegation set can be deleted.</p>
newtype DelegationSetInUse = DelegationSetInUse 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetInUse :: Newtype DelegationSetInUse _


newtype DelegationSetNameServers = DelegationSetNameServers (Array DNSName)
derive instance newtypeDelegationSetNameServers :: Newtype DelegationSetNameServers _


-- | <p>You can create a hosted zone that has the same name as an existing hosted zone (example.com is common), but there is a limit to the number of hosted zones that have the same name. If you get this error, Amazon Route 53 has reached that limit. If you own the domain name and Amazon Route 53 generates this error, contact Customer Support.</p>
newtype DelegationSetNotAvailable = DelegationSetNotAvailable 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetNotAvailable :: Newtype DelegationSetNotAvailable _


-- | <p>A reusable delegation set with the specified ID does not exist.</p>
newtype DelegationSetNotReusable = DelegationSetNotReusable 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetNotReusable :: Newtype DelegationSetNotReusable _


newtype DelegationSets = DelegationSets (Array DelegationSet)
derive instance newtypeDelegationSets :: Newtype DelegationSets _


-- | <p>This action deletes a health check.</p>
newtype DeleteHealthCheckRequest = DeleteHealthCheckRequest 
  { "HealthCheckId" :: (HealthCheckId)
  }
derive instance newtypeDeleteHealthCheckRequest :: Newtype DeleteHealthCheckRequest _


-- | <p>An empty element.</p>
newtype DeleteHealthCheckResponse = DeleteHealthCheckResponse 
  { 
  }
derive instance newtypeDeleteHealthCheckResponse :: Newtype DeleteHealthCheckResponse _


-- | <p>A request to delete a hosted zone.</p>
newtype DeleteHostedZoneRequest = DeleteHostedZoneRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeDeleteHostedZoneRequest :: Newtype DeleteHostedZoneRequest _


-- | <p>A complex type that contains the response to a <code>DeleteHostedZone</code> request.</p>
newtype DeleteHostedZoneResponse = DeleteHostedZoneResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeDeleteHostedZoneResponse :: Newtype DeleteHostedZoneResponse _


newtype DeleteQueryLoggingConfigRequest = DeleteQueryLoggingConfigRequest 
  { "Id" :: (QueryLoggingConfigId)
  }
derive instance newtypeDeleteQueryLoggingConfigRequest :: Newtype DeleteQueryLoggingConfigRequest _


newtype DeleteQueryLoggingConfigResponse = DeleteQueryLoggingConfigResponse 
  { 
  }
derive instance newtypeDeleteQueryLoggingConfigResponse :: Newtype DeleteQueryLoggingConfigResponse _


-- | <p>A request to delete a reusable delegation set.</p>
newtype DeleteReusableDelegationSetRequest = DeleteReusableDelegationSetRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeDeleteReusableDelegationSetRequest :: Newtype DeleteReusableDelegationSetRequest _


-- | <p>An empty element.</p>
newtype DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse 
  { 
  }
derive instance newtypeDeleteReusableDelegationSetResponse :: Newtype DeleteReusableDelegationSetResponse _


-- | <p>A request to delete a specified traffic policy instance.</p>
newtype DeleteTrafficPolicyInstanceRequest = DeleteTrafficPolicyInstanceRequest 
  { "Id" :: (TrafficPolicyInstanceId)
  }
derive instance newtypeDeleteTrafficPolicyInstanceRequest :: Newtype DeleteTrafficPolicyInstanceRequest _


-- | <p>An empty element.</p>
newtype DeleteTrafficPolicyInstanceResponse = DeleteTrafficPolicyInstanceResponse 
  { 
  }
derive instance newtypeDeleteTrafficPolicyInstanceResponse :: Newtype DeleteTrafficPolicyInstanceResponse _


-- | <p>A request to delete a specified traffic policy version.</p>
newtype DeleteTrafficPolicyRequest = DeleteTrafficPolicyRequest 
  { "Id" :: (TrafficPolicyId)
  , "Version" :: (TrafficPolicyVersion)
  }
derive instance newtypeDeleteTrafficPolicyRequest :: Newtype DeleteTrafficPolicyRequest _


-- | <p>An empty element.</p>
newtype DeleteTrafficPolicyResponse = DeleteTrafficPolicyResponse 
  { 
  }
derive instance newtypeDeleteTrafficPolicyResponse :: Newtype DeleteTrafficPolicyResponse _


-- | <p>A complex type that contains information about the request to remove authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account. </p>
newtype DeleteVPCAssociationAuthorizationRequest = DeleteVPCAssociationAuthorizationRequest 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  }
derive instance newtypeDeleteVPCAssociationAuthorizationRequest :: Newtype DeleteVPCAssociationAuthorizationRequest _


-- | <p>Empty response for the request.</p>
newtype DeleteVPCAssociationAuthorizationResponse = DeleteVPCAssociationAuthorizationResponse 
  { 
  }
derive instance newtypeDeleteVPCAssociationAuthorizationResponse :: Newtype DeleteVPCAssociationAuthorizationResponse _


-- | <p>For the metric that the CloudWatch alarm is associated with, a complex type that contains information about one dimension.</p>
newtype Dimension = Dimension 
  { "Name" :: (DimensionField)
  , "Value" :: (DimensionField)
  }
derive instance newtypeDimension :: Newtype Dimension _


newtype DimensionField = DimensionField String
derive instance newtypeDimensionField :: Newtype DimensionField _


newtype DimensionList = DimensionList (Array Dimension)
derive instance newtypeDimensionList :: Newtype DimensionList _


newtype DisassociateVPCComment = DisassociateVPCComment String
derive instance newtypeDisassociateVPCComment :: Newtype DisassociateVPCComment _


-- | <p>A complex type that contains information about the VPC that you want to disassociate from a specified private hosted zone.</p>
newtype DisassociateVPCFromHostedZoneRequest = DisassociateVPCFromHostedZoneRequest 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  , "Comment" :: NullOrUndefined (DisassociateVPCComment)
  }
derive instance newtypeDisassociateVPCFromHostedZoneRequest :: Newtype DisassociateVPCFromHostedZoneRequest _


-- | <p>A complex type that contains the response information for the disassociate request.</p>
newtype DisassociateVPCFromHostedZoneResponse = DisassociateVPCFromHostedZoneResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeDisassociateVPCFromHostedZoneResponse :: Newtype DisassociateVPCFromHostedZoneResponse _


newtype EnableSNI = EnableSNI Boolean
derive instance newtypeEnableSNI :: Newtype EnableSNI _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype ErrorMessages = ErrorMessages (Array ErrorMessage)
derive instance newtypeErrorMessages :: Newtype ErrorMessages _


newtype EvaluationPeriods = EvaluationPeriods Int
derive instance newtypeEvaluationPeriods :: Newtype EvaluationPeriods _


newtype FailureThreshold = FailureThreshold Int
derive instance newtypeFailureThreshold :: Newtype FailureThreshold _


newtype FullyQualifiedDomainName = FullyQualifiedDomainName String
derive instance newtypeFullyQualifiedDomainName :: Newtype FullyQualifiedDomainName _


-- | <p>A complex type that contains information about a geo location.</p>
newtype GeoLocation = GeoLocation 
  { "ContinentCode" :: NullOrUndefined (GeoLocationContinentCode)
  , "CountryCode" :: NullOrUndefined (GeoLocationCountryCode)
  , "SubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode)
  }
derive instance newtypeGeoLocation :: Newtype GeoLocation _


newtype GeoLocationContinentCode = GeoLocationContinentCode String
derive instance newtypeGeoLocationContinentCode :: Newtype GeoLocationContinentCode _


newtype GeoLocationContinentName = GeoLocationContinentName String
derive instance newtypeGeoLocationContinentName :: Newtype GeoLocationContinentName _


newtype GeoLocationCountryCode = GeoLocationCountryCode String
derive instance newtypeGeoLocationCountryCode :: Newtype GeoLocationCountryCode _


newtype GeoLocationCountryName = GeoLocationCountryName String
derive instance newtypeGeoLocationCountryName :: Newtype GeoLocationCountryName _


-- | <p>A complex type that contains the codes and full continent, country, and subdivision names for the specified <code>geolocation</code> code.</p>
newtype GeoLocationDetails = GeoLocationDetails 
  { "ContinentCode" :: NullOrUndefined (GeoLocationContinentCode)
  , "ContinentName" :: NullOrUndefined (GeoLocationContinentName)
  , "CountryCode" :: NullOrUndefined (GeoLocationCountryCode)
  , "CountryName" :: NullOrUndefined (GeoLocationCountryName)
  , "SubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode)
  , "SubdivisionName" :: NullOrUndefined (GeoLocationSubdivisionName)
  }
derive instance newtypeGeoLocationDetails :: Newtype GeoLocationDetails _


newtype GeoLocationDetailsList = GeoLocationDetailsList (Array GeoLocationDetails)
derive instance newtypeGeoLocationDetailsList :: Newtype GeoLocationDetailsList _


newtype GeoLocationSubdivisionCode = GeoLocationSubdivisionCode String
derive instance newtypeGeoLocationSubdivisionCode :: Newtype GeoLocationSubdivisionCode _


newtype GeoLocationSubdivisionName = GeoLocationSubdivisionName String
derive instance newtypeGeoLocationSubdivisionName :: Newtype GeoLocationSubdivisionName _


-- | <p>A complex type that contains information about the request to create a hosted zone.</p>
newtype GetAccountLimitRequest = GetAccountLimitRequest 
  { "Type" :: (AccountLimitType)
  }
derive instance newtypeGetAccountLimitRequest :: Newtype GetAccountLimitRequest _


-- | <p>A complex type that contains the requested limit. </p>
newtype GetAccountLimitResponse = GetAccountLimitResponse 
  { "Limit" :: (AccountLimit)
  , "Count" :: (UsageCount)
  }
derive instance newtypeGetAccountLimitResponse :: Newtype GetAccountLimitResponse _


-- | <p>The input for a GetChange request.</p>
newtype GetChangeRequest = GetChangeRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeGetChangeRequest :: Newtype GetChangeRequest _


-- | <p>A complex type that contains the <code>ChangeInfo</code> element.</p>
newtype GetChangeResponse = GetChangeResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeGetChangeResponse :: Newtype GetChangeResponse _


newtype GetCheckerIpRangesRequest = GetCheckerIpRangesRequest 
  { 
  }
derive instance newtypeGetCheckerIpRangesRequest :: Newtype GetCheckerIpRangesRequest _


newtype GetCheckerIpRangesResponse = GetCheckerIpRangesResponse 
  { "CheckerIpRanges" :: (CheckerIpRanges)
  }
derive instance newtypeGetCheckerIpRangesResponse :: Newtype GetCheckerIpRangesResponse _


-- | <p>A request for information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.</p>
newtype GetGeoLocationRequest = GetGeoLocationRequest 
  { "ContinentCode" :: NullOrUndefined (GeoLocationContinentCode)
  , "CountryCode" :: NullOrUndefined (GeoLocationCountryCode)
  , "SubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode)
  }
derive instance newtypeGetGeoLocationRequest :: Newtype GetGeoLocationRequest _


-- | <p>A complex type that contains the response information for the specified geolocation code.</p>
newtype GetGeoLocationResponse = GetGeoLocationResponse 
  { "GeoLocationDetails" :: (GeoLocationDetails)
  }
derive instance newtypeGetGeoLocationResponse :: Newtype GetGeoLocationResponse _


-- | <p>A request for the number of health checks that are associated with the current AWS account.</p>
newtype GetHealthCheckCountRequest = GetHealthCheckCountRequest 
  { 
  }
derive instance newtypeGetHealthCheckCountRequest :: Newtype GetHealthCheckCountRequest _


-- | <p>A complex type that contains the response to a <code>GetHealthCheckCount</code> request.</p>
newtype GetHealthCheckCountResponse = GetHealthCheckCountResponse 
  { "HealthCheckCount" :: (HealthCheckCount)
  }
derive instance newtypeGetHealthCheckCountResponse :: Newtype GetHealthCheckCountResponse _


-- | <p>A request for the reason that a health check failed most recently.</p>
newtype GetHealthCheckLastFailureReasonRequest = GetHealthCheckLastFailureReasonRequest 
  { "HealthCheckId" :: (HealthCheckId)
  }
derive instance newtypeGetHealthCheckLastFailureReasonRequest :: Newtype GetHealthCheckLastFailureReasonRequest _


-- | <p>A complex type that contains the response to a <code>GetHealthCheckLastFailureReason</code> request.</p>
newtype GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse 
  { "HealthCheckObservations" :: (HealthCheckObservations)
  }
derive instance newtypeGetHealthCheckLastFailureReasonResponse :: Newtype GetHealthCheckLastFailureReasonResponse _


-- | <p>A request to get information about a specified health check. </p>
newtype GetHealthCheckRequest = GetHealthCheckRequest 
  { "HealthCheckId" :: (HealthCheckId)
  }
derive instance newtypeGetHealthCheckRequest :: Newtype GetHealthCheckRequest _


-- | <p>A complex type that contains the response to a <code>GetHealthCheck</code> request.</p>
newtype GetHealthCheckResponse = GetHealthCheckResponse 
  { "HealthCheck" :: (HealthCheck)
  }
derive instance newtypeGetHealthCheckResponse :: Newtype GetHealthCheckResponse _


-- | <p>A request to get the status for a health check.</p>
newtype GetHealthCheckStatusRequest = GetHealthCheckStatusRequest 
  { "HealthCheckId" :: (HealthCheckId)
  }
derive instance newtypeGetHealthCheckStatusRequest :: Newtype GetHealthCheckStatusRequest _


-- | <p>A complex type that contains the response to a <code>GetHealthCheck</code> request.</p>
newtype GetHealthCheckStatusResponse = GetHealthCheckStatusResponse 
  { "HealthCheckObservations" :: (HealthCheckObservations)
  }
derive instance newtypeGetHealthCheckStatusResponse :: Newtype GetHealthCheckStatusResponse _


-- | <p>A request to retrieve a count of all the hosted zones that are associated with the current AWS account.</p>
newtype GetHostedZoneCountRequest = GetHostedZoneCountRequest 
  { 
  }
derive instance newtypeGetHostedZoneCountRequest :: Newtype GetHostedZoneCountRequest _


-- | <p>A complex type that contains the response to a <code>GetHostedZoneCount</code> request.</p>
newtype GetHostedZoneCountResponse = GetHostedZoneCountResponse 
  { "HostedZoneCount" :: (HostedZoneCount)
  }
derive instance newtypeGetHostedZoneCountResponse :: Newtype GetHostedZoneCountResponse _


-- | <p>A complex type that contains information about the request to create a hosted zone.</p>
newtype GetHostedZoneLimitRequest = GetHostedZoneLimitRequest 
  { "Type" :: (HostedZoneLimitType)
  , "HostedZoneId" :: (ResourceId)
  }
derive instance newtypeGetHostedZoneLimitRequest :: Newtype GetHostedZoneLimitRequest _


-- | <p>A complex type that contains the requested limit. </p>
newtype GetHostedZoneLimitResponse = GetHostedZoneLimitResponse 
  { "Limit" :: (HostedZoneLimit)
  , "Count" :: (UsageCount)
  }
derive instance newtypeGetHostedZoneLimitResponse :: Newtype GetHostedZoneLimitResponse _


-- | <p>A request to get information about a specified hosted zone. </p>
newtype GetHostedZoneRequest = GetHostedZoneRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeGetHostedZoneRequest :: Newtype GetHostedZoneRequest _


-- | <p>A complex type that contain the response to a <code>GetHostedZone</code> request.</p>
newtype GetHostedZoneResponse = GetHostedZoneResponse 
  { "HostedZone" :: (HostedZone)
  , "DelegationSet" :: NullOrUndefined (DelegationSet)
  , "VPCs" :: NullOrUndefined (VPCs)
  }
derive instance newtypeGetHostedZoneResponse :: Newtype GetHostedZoneResponse _


newtype GetQueryLoggingConfigRequest = GetQueryLoggingConfigRequest 
  { "Id" :: (QueryLoggingConfigId)
  }
derive instance newtypeGetQueryLoggingConfigRequest :: Newtype GetQueryLoggingConfigRequest _


newtype GetQueryLoggingConfigResponse = GetQueryLoggingConfigResponse 
  { "QueryLoggingConfig" :: (QueryLoggingConfig)
  }
derive instance newtypeGetQueryLoggingConfigResponse :: Newtype GetQueryLoggingConfigResponse _


-- | <p>A complex type that contains information about the request to create a hosted zone.</p>
newtype GetReusableDelegationSetLimitRequest = GetReusableDelegationSetLimitRequest 
  { "Type" :: (ReusableDelegationSetLimitType)
  , "DelegationSetId" :: (ResourceId)
  }
derive instance newtypeGetReusableDelegationSetLimitRequest :: Newtype GetReusableDelegationSetLimitRequest _


-- | <p>A complex type that contains the requested limit. </p>
newtype GetReusableDelegationSetLimitResponse = GetReusableDelegationSetLimitResponse 
  { "Limit" :: (ReusableDelegationSetLimit)
  , "Count" :: (UsageCount)
  }
derive instance newtypeGetReusableDelegationSetLimitResponse :: Newtype GetReusableDelegationSetLimitResponse _


-- | <p>A request to get information about a specified reusable delegation set.</p>
newtype GetReusableDelegationSetRequest = GetReusableDelegationSetRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeGetReusableDelegationSetRequest :: Newtype GetReusableDelegationSetRequest _


-- | <p>A complex type that contains the response to the <code>GetReusableDelegationSet</code> request.</p>
newtype GetReusableDelegationSetResponse = GetReusableDelegationSetResponse 
  { "DelegationSet" :: (DelegationSet)
  }
derive instance newtypeGetReusableDelegationSetResponse :: Newtype GetReusableDelegationSetResponse _


-- | <p>Request to get the number of traffic policy instances that are associated with the current AWS account.</p>
newtype GetTrafficPolicyInstanceCountRequest = GetTrafficPolicyInstanceCountRequest 
  { 
  }
derive instance newtypeGetTrafficPolicyInstanceCountRequest :: Newtype GetTrafficPolicyInstanceCountRequest _


-- | <p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>
newtype GetTrafficPolicyInstanceCountResponse = GetTrafficPolicyInstanceCountResponse 
  { "TrafficPolicyInstanceCount" :: (TrafficPolicyInstanceCount)
  }
derive instance newtypeGetTrafficPolicyInstanceCountResponse :: Newtype GetTrafficPolicyInstanceCountResponse _


-- | <p>Gets information about a specified traffic policy instance.</p>
newtype GetTrafficPolicyInstanceRequest = GetTrafficPolicyInstanceRequest 
  { "Id" :: (TrafficPolicyInstanceId)
  }
derive instance newtypeGetTrafficPolicyInstanceRequest :: Newtype GetTrafficPolicyInstanceRequest _


-- | <p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>
newtype GetTrafficPolicyInstanceResponse = GetTrafficPolicyInstanceResponse 
  { "TrafficPolicyInstance" :: (TrafficPolicyInstance)
  }
derive instance newtypeGetTrafficPolicyInstanceResponse :: Newtype GetTrafficPolicyInstanceResponse _


-- | <p>Gets information about a specific traffic policy version.</p>
newtype GetTrafficPolicyRequest = GetTrafficPolicyRequest 
  { "Id" :: (TrafficPolicyId)
  , "Version" :: (TrafficPolicyVersion)
  }
derive instance newtypeGetTrafficPolicyRequest :: Newtype GetTrafficPolicyRequest _


-- | <p>A complex type that contains the response information for the request.</p>
newtype GetTrafficPolicyResponse = GetTrafficPolicyResponse 
  { "TrafficPolicy" :: (TrafficPolicy)
  }
derive instance newtypeGetTrafficPolicyResponse :: Newtype GetTrafficPolicyResponse _


-- | <p>A complex type that contains information about one health check that is associated with the current AWS account.</p>
newtype HealthCheck = HealthCheck 
  { "Id" :: (HealthCheckId)
  , "CallerReference" :: (HealthCheckNonce)
  , "LinkedService" :: NullOrUndefined (LinkedService)
  , "HealthCheckConfig" :: (HealthCheckConfig)
  , "HealthCheckVersion" :: (HealthCheckVersion)
  , "CloudWatchAlarmConfiguration" :: NullOrUndefined (CloudWatchAlarmConfiguration)
  }
derive instance newtypeHealthCheck :: Newtype HealthCheck _


-- | <p> The health check you're attempting to create already exists. Amazon Route 53 returns this error when you submit a request that has the following values:</p> <ul> <li> <p>The same value for <code>CallerReference</code> as an existing health check, and one or more values that differ from the existing health check that has the same caller reference.</p> </li> <li> <p>The same value for <code>CallerReference</code> as a health check that you created and later deleted, regardless of the other settings in the request.</p> </li> </ul>
newtype HealthCheckAlreadyExists = HealthCheckAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHealthCheckAlreadyExists :: Newtype HealthCheckAlreadyExists _


-- | <p>A complex type that contains information about the health check.</p>
newtype HealthCheckConfig = HealthCheckConfig 
  { "IPAddress" :: NullOrUndefined (IPAddress)
  , "Port" :: NullOrUndefined (Port)
  , "Type" :: (HealthCheckType)
  , "ResourcePath" :: NullOrUndefined (ResourcePath)
  , "FullyQualifiedDomainName" :: NullOrUndefined (FullyQualifiedDomainName)
  , "SearchString" :: NullOrUndefined (SearchString)
  , "RequestInterval" :: NullOrUndefined (RequestInterval)
  , "FailureThreshold" :: NullOrUndefined (FailureThreshold)
  , "MeasureLatency" :: NullOrUndefined (MeasureLatency)
  , "Inverted" :: NullOrUndefined (Inverted)
  , "HealthThreshold" :: NullOrUndefined (HealthThreshold)
  , "ChildHealthChecks" :: NullOrUndefined (ChildHealthCheckList)
  , "EnableSNI" :: NullOrUndefined (EnableSNI)
  , "Regions" :: NullOrUndefined (HealthCheckRegionList)
  , "AlarmIdentifier" :: NullOrUndefined (AlarmIdentifier)
  , "InsufficientDataHealthStatus" :: NullOrUndefined (InsufficientDataHealthStatus)
  }
derive instance newtypeHealthCheckConfig :: Newtype HealthCheckConfig _


newtype HealthCheckCount = HealthCheckCount Number
derive instance newtypeHealthCheckCount :: Newtype HealthCheckCount _


newtype HealthCheckId = HealthCheckId String
derive instance newtypeHealthCheckId :: Newtype HealthCheckId _


-- | <p>This error code is not in use.</p>
newtype HealthCheckInUse = HealthCheckInUse 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHealthCheckInUse :: Newtype HealthCheckInUse _


newtype HealthCheckNonce = HealthCheckNonce String
derive instance newtypeHealthCheckNonce :: Newtype HealthCheckNonce _


-- | <p>A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker.</p>
newtype HealthCheckObservation = HealthCheckObservation 
  { "Region" :: NullOrUndefined (HealthCheckRegion)
  , "IPAddress" :: NullOrUndefined (IPAddress)
  , "StatusReport" :: NullOrUndefined (StatusReport)
  }
derive instance newtypeHealthCheckObservation :: Newtype HealthCheckObservation _


newtype HealthCheckObservations = HealthCheckObservations (Array HealthCheckObservation)
derive instance newtypeHealthCheckObservations :: Newtype HealthCheckObservations _


newtype HealthCheckRegion = HealthCheckRegion String
derive instance newtypeHealthCheckRegion :: Newtype HealthCheckRegion _


newtype HealthCheckRegionList = HealthCheckRegionList (Array HealthCheckRegion)
derive instance newtypeHealthCheckRegionList :: Newtype HealthCheckRegionList _


newtype HealthCheckType = HealthCheckType String
derive instance newtypeHealthCheckType :: Newtype HealthCheckType _


newtype HealthCheckVersion = HealthCheckVersion Number
derive instance newtypeHealthCheckVersion :: Newtype HealthCheckVersion _


-- | <p>The value of <code>HealthCheckVersion</code> in the request doesn't match the value of <code>HealthCheckVersion</code> in the health check.</p>
newtype HealthCheckVersionMismatch = HealthCheckVersionMismatch 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHealthCheckVersionMismatch :: Newtype HealthCheckVersionMismatch _


newtype HealthChecks = HealthChecks (Array HealthCheck)
derive instance newtypeHealthChecks :: Newtype HealthChecks _


newtype HealthThreshold = HealthThreshold Int
derive instance newtypeHealthThreshold :: Newtype HealthThreshold _


-- | <p>A complex type that contains general information about the hosted zone.</p>
newtype HostedZone = HostedZone 
  { "Id" :: (ResourceId)
  , "Name" :: (DNSName)
  , "CallerReference" :: (Nonce)
  , "Config" :: NullOrUndefined (HostedZoneConfig)
  , "ResourceRecordSetCount" :: NullOrUndefined (HostedZoneRRSetCount)
  , "LinkedService" :: NullOrUndefined (LinkedService)
  }
derive instance newtypeHostedZone :: Newtype HostedZone _


-- | <p>The hosted zone you're trying to create already exists. Amazon Route 53 returns this error when a hosted zone has already been created with the specified <code>CallerReference</code>.</p>
newtype HostedZoneAlreadyExists = HostedZoneAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHostedZoneAlreadyExists :: Newtype HostedZoneAlreadyExists _


-- | <p>A complex type that contains an optional comment about your hosted zone. If you don't want to specify a comment, omit both the <code>HostedZoneConfig</code> and <code>Comment</code> elements.</p>
newtype HostedZoneConfig = HostedZoneConfig 
  { "Comment" :: NullOrUndefined (ResourceDescription)
  , "PrivateZone" :: NullOrUndefined (IsPrivateZone)
  }
derive instance newtypeHostedZoneConfig :: Newtype HostedZoneConfig _


newtype HostedZoneCount = HostedZoneCount Number
derive instance newtypeHostedZoneCount :: Newtype HostedZoneCount _


-- | <p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>
newtype HostedZoneLimit = HostedZoneLimit 
  { "Type" :: (HostedZoneLimitType)
  , "Value" :: (LimitValue)
  }
derive instance newtypeHostedZoneLimit :: Newtype HostedZoneLimit _


newtype HostedZoneLimitType = HostedZoneLimitType String
derive instance newtypeHostedZoneLimitType :: Newtype HostedZoneLimitType _


-- | <p>The hosted zone contains resource records that are not SOA or NS records.</p>
newtype HostedZoneNotEmpty = HostedZoneNotEmpty 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHostedZoneNotEmpty :: Newtype HostedZoneNotEmpty _


-- | <p>The specified HostedZone can't be found.</p>
newtype HostedZoneNotFound = HostedZoneNotFound 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHostedZoneNotFound :: Newtype HostedZoneNotFound _


-- | <p>The specified hosted zone is a public hosted zone, not a private hosted zone.</p>
newtype HostedZoneNotPrivate = HostedZoneNotPrivate 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHostedZoneNotPrivate :: Newtype HostedZoneNotPrivate _


newtype HostedZoneRRSetCount = HostedZoneRRSetCount Number
derive instance newtypeHostedZoneRRSetCount :: Newtype HostedZoneRRSetCount _


newtype HostedZones = HostedZones (Array HostedZone)
derive instance newtypeHostedZones :: Newtype HostedZones _


newtype IPAddress = IPAddress String
derive instance newtypeIPAddress :: Newtype IPAddress _


newtype IPAddressCidr = IPAddressCidr String
derive instance newtypeIPAddressCidr :: Newtype IPAddressCidr _


-- | <p>The resource you're trying to access is unsupported on this Amazon Route 53 endpoint.</p>
newtype IncompatibleVersion = IncompatibleVersion 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeIncompatibleVersion :: Newtype IncompatibleVersion _


-- | <p>Amazon Route 53 doesn't have the permissions required to create log streams and send query logs to log streams. Possible causes include the following:</p> <ul> <li> <p>There is no resource policy that specifies the log group ARN in the value for <code>Resource</code>.</p> </li> <li> <p>The resource policy that includes the log group ARN in the value for <code>Resource</code> doesn't have the necessary permissions.</p> </li> <li> <p>The resource policy hasn't finished propagating yet.</p> </li> </ul>
newtype InsufficientCloudWatchLogsResourcePolicy = InsufficientCloudWatchLogsResourcePolicy 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInsufficientCloudWatchLogsResourcePolicy :: Newtype InsufficientCloudWatchLogsResourcePolicy _


newtype InsufficientDataHealthStatus = InsufficientDataHealthStatus String
derive instance newtypeInsufficientDataHealthStatus :: Newtype InsufficientDataHealthStatus _


-- | <p>Parameter name is invalid.</p>
newtype InvalidArgument = InvalidArgument 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgument :: Newtype InvalidArgument _


-- | <p>This exception contains a list of messages that might contain one or more error messages. Each error message indicates one error in the change batch.</p>
newtype InvalidChangeBatch = InvalidChangeBatch 
  { "Messages'" :: NullOrUndefined (ErrorMessages)
  , "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidChangeBatch :: Newtype InvalidChangeBatch _


-- | <p>The specified domain name is not valid.</p>
newtype InvalidDomainName = InvalidDomainName 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidDomainName :: Newtype InvalidDomainName _


-- | <p>The input is not valid.</p>
newtype InvalidInput = InvalidInput 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidInput :: Newtype InvalidInput _


-- | <p>The value that you specified to get the second or subsequent page of results is invalid.</p>
newtype InvalidPaginationToken = InvalidPaginationToken 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidPaginationToken :: Newtype InvalidPaginationToken _


-- | <p>The format of the traffic policy document that you specified in the <code>Document</code> element is invalid.</p>
newtype InvalidTrafficPolicyDocument = InvalidTrafficPolicyDocument 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidTrafficPolicyDocument :: Newtype InvalidTrafficPolicyDocument _


-- | <p>The VPC ID that you specified either isn't a valid ID or the current account is not authorized to access this VPC.</p>
newtype InvalidVPCId = InvalidVPCId 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidVPCId :: Newtype InvalidVPCId _


newtype Inverted = Inverted Boolean
derive instance newtypeInverted :: Newtype Inverted _


newtype IsPrivateZone = IsPrivateZone Boolean
derive instance newtypeIsPrivateZone :: Newtype IsPrivateZone _


-- | <p>The VPC that you're trying to disassociate from the private hosted zone is the last VPC that is associated with the hosted zone. Amazon Route 53 doesn't support disassociating the last VPC from a hosted zone.</p>
newtype LastVPCAssociation = LastVPCAssociation 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLastVPCAssociation :: Newtype LastVPCAssociation _


newtype LimitValue = LimitValue Number
derive instance newtypeLimitValue :: Newtype LimitValue _


-- | <p>This operation can't be completed either because the current account has reached the limit on reusable delegation sets that it can create or because you've reached the limit on the number of Amazon VPCs that you can associate with a private hosted zone. To get the current limit on the number of reusable delegation sets, see <a>GetAccountLimit</a>. To get the current limit on the number of Amazon VPCs that you can associate with a private hosted zone, see <a>GetHostedZoneLimit</a>. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype LimitsExceeded = LimitsExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitsExceeded :: Newtype LimitsExceeded _


-- | <p>If a health check or hosted zone was created by another service, <code>LinkedService</code> is a complex type that describes the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53. </p>
newtype LinkedService = LinkedService 
  { "ServicePrincipal" :: NullOrUndefined (ServicePrincipal)
  , "Description" :: NullOrUndefined (ResourceDescription)
  }
derive instance newtypeLinkedService :: Newtype LinkedService _


-- | <p>A request to get a list of geographic locations that Amazon Route 53 supports for geolocation resource record sets. </p>
newtype ListGeoLocationsRequest = ListGeoLocationsRequest 
  { "StartContinentCode" :: NullOrUndefined (GeoLocationContinentCode)
  , "StartCountryCode" :: NullOrUndefined (GeoLocationCountryCode)
  , "StartSubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListGeoLocationsRequest :: Newtype ListGeoLocationsRequest _


-- | <p>A complex type containing the response information for the request.</p>
newtype ListGeoLocationsResponse = ListGeoLocationsResponse 
  { "GeoLocationDetailsList" :: (GeoLocationDetailsList)
  , "IsTruncated" :: (PageTruncated)
  , "NextContinentCode" :: NullOrUndefined (GeoLocationContinentCode)
  , "NextCountryCode" :: NullOrUndefined (GeoLocationCountryCode)
  , "NextSubdivisionCode" :: NullOrUndefined (GeoLocationSubdivisionCode)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListGeoLocationsResponse :: Newtype ListGeoLocationsResponse _


-- | <p>A request to retrieve a list of the health checks that are associated with the current AWS account.</p>
newtype ListHealthChecksRequest = ListHealthChecksRequest 
  { "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListHealthChecksRequest :: Newtype ListHealthChecksRequest _


-- | <p>A complex type that contains the response to a <code>ListHealthChecks</code> request.</p>
newtype ListHealthChecksResponse = ListHealthChecksResponse 
  { "HealthChecks" :: (HealthChecks)
  , "Marker" :: (PageMarker)
  , "IsTruncated" :: (PageTruncated)
  , "NextMarker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListHealthChecksResponse :: Newtype ListHealthChecksResponse _


-- | <p>Retrieves a list of the public and private hosted zones that are associated with the current AWS account in ASCII order by domain name. </p>
newtype ListHostedZonesByNameRequest = ListHostedZonesByNameRequest 
  { "DNSName" :: NullOrUndefined (DNSName)
  , "HostedZoneId" :: NullOrUndefined (ResourceId)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListHostedZonesByNameRequest :: Newtype ListHostedZonesByNameRequest _


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListHostedZonesByNameResponse = ListHostedZonesByNameResponse 
  { "HostedZones" :: (HostedZones)
  , "DNSName" :: NullOrUndefined (DNSName)
  , "HostedZoneId" :: NullOrUndefined (ResourceId)
  , "IsTruncated" :: (PageTruncated)
  , "NextDNSName" :: NullOrUndefined (DNSName)
  , "NextHostedZoneId" :: NullOrUndefined (ResourceId)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListHostedZonesByNameResponse :: Newtype ListHostedZonesByNameResponse _


-- | <p>A request to retrieve a list of the public and private hosted zones that are associated with the current AWS account.</p>
newtype ListHostedZonesRequest = ListHostedZonesRequest 
  { "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  , "DelegationSetId" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeListHostedZonesRequest :: Newtype ListHostedZonesRequest _


newtype ListHostedZonesResponse = ListHostedZonesResponse 
  { "HostedZones" :: (HostedZones)
  , "Marker" :: (PageMarker)
  , "IsTruncated" :: (PageTruncated)
  , "NextMarker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListHostedZonesResponse :: Newtype ListHostedZonesResponse _


newtype ListQueryLoggingConfigsRequest = ListQueryLoggingConfigsRequest 
  { "HostedZoneId" :: NullOrUndefined (ResourceId)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListQueryLoggingConfigsRequest :: Newtype ListQueryLoggingConfigsRequest _


newtype ListQueryLoggingConfigsResponse = ListQueryLoggingConfigsResponse 
  { "QueryLoggingConfigs" :: (QueryLoggingConfigs)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeListQueryLoggingConfigsResponse :: Newtype ListQueryLoggingConfigsResponse _


-- | <p>A request for the resource record sets that are associated with a specified hosted zone.</p>
newtype ListResourceRecordSetsRequest = ListResourceRecordSetsRequest 
  { "HostedZoneId" :: (ResourceId)
  , "StartRecordName" :: NullOrUndefined (DNSName)
  , "StartRecordType" :: NullOrUndefined (RRType)
  , "StartRecordIdentifier" :: NullOrUndefined (ResourceRecordSetIdentifier)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListResourceRecordSetsRequest :: Newtype ListResourceRecordSetsRequest _


-- | <p>A complex type that contains list information for the resource record set.</p>
newtype ListResourceRecordSetsResponse = ListResourceRecordSetsResponse 
  { "ResourceRecordSets" :: (ResourceRecordSets)
  , "IsTruncated" :: (PageTruncated)
  , "NextRecordName" :: NullOrUndefined (DNSName)
  , "NextRecordType" :: NullOrUndefined (RRType)
  , "NextRecordIdentifier" :: NullOrUndefined (ResourceRecordSetIdentifier)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListResourceRecordSetsResponse :: Newtype ListResourceRecordSetsResponse _


-- | <p>A request to get a list of the reusable delegation sets that are associated with the current AWS account.</p>
newtype ListReusableDelegationSetsRequest = ListReusableDelegationSetsRequest 
  { "Marker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListReusableDelegationSetsRequest :: Newtype ListReusableDelegationSetsRequest _


-- | <p>A complex type that contains information about the reusable delegation sets that are associated with the current AWS account.</p>
newtype ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse 
  { "DelegationSets" :: (DelegationSets)
  , "Marker" :: (PageMarker)
  , "IsTruncated" :: (PageTruncated)
  , "NextMarker" :: NullOrUndefined (PageMarker)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListReusableDelegationSetsResponse :: Newtype ListReusableDelegationSetsResponse _


-- | <p>A complex type containing information about a request for a list of the tags that are associated with an individual resource.</p>
newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceType" :: (TagResourceType)
  , "ResourceId" :: (TagResourceId)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _


-- | <p>A complex type that contains information about the health checks or hosted zones for which you want to list tags.</p>
newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "ResourceTagSet" :: (ResourceTagSet)
  }
derive instance newtypeListTagsForResourceResponse :: Newtype ListTagsForResourceResponse _


-- | <p>A complex type that contains information about the health checks or hosted zones for which you want to list tags.</p>
newtype ListTagsForResourcesRequest = ListTagsForResourcesRequest 
  { "ResourceType" :: (TagResourceType)
  , "ResourceIds" :: (TagResourceIdList)
  }
derive instance newtypeListTagsForResourcesRequest :: Newtype ListTagsForResourcesRequest _


-- | <p>A complex type containing tags for the specified resources.</p>
newtype ListTagsForResourcesResponse = ListTagsForResourcesResponse 
  { "ResourceTagSets" :: (ResourceTagSetList)
  }
derive instance newtypeListTagsForResourcesResponse :: Newtype ListTagsForResourcesResponse _


-- | <p>A complex type that contains the information about the request to list the traffic policies that are associated with the current AWS account.</p>
newtype ListTrafficPoliciesRequest = ListTrafficPoliciesRequest 
  { "TrafficPolicyIdMarker" :: NullOrUndefined (TrafficPolicyId)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPoliciesRequest :: Newtype ListTrafficPoliciesRequest _


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPoliciesResponse = ListTrafficPoliciesResponse 
  { "TrafficPolicySummaries" :: (TrafficPolicySummaries)
  , "IsTruncated" :: (PageTruncated)
  , "TrafficPolicyIdMarker" :: (TrafficPolicyId)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPoliciesResponse :: Newtype ListTrafficPoliciesResponse _


-- | <p>A request for the traffic policy instances that you created in a specified hosted zone.</p>
newtype ListTrafficPolicyInstancesByHostedZoneRequest = ListTrafficPolicyInstancesByHostedZoneRequest 
  { "HostedZoneId" :: (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesByHostedZoneRequest :: Newtype ListTrafficPolicyInstancesByHostedZoneRequest _


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPolicyInstancesByHostedZoneResponse = ListTrafficPolicyInstancesByHostedZoneResponse 
  { "TrafficPolicyInstances" :: (TrafficPolicyInstances)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType)
  , "IsTruncated" :: (PageTruncated)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesByHostedZoneResponse :: Newtype ListTrafficPolicyInstancesByHostedZoneResponse _


-- | <p>A complex type that contains the information about the request to list your traffic policy instances.</p>
newtype ListTrafficPolicyInstancesByPolicyRequest = ListTrafficPolicyInstancesByPolicyRequest 
  { "TrafficPolicyId" :: (TrafficPolicyId)
  , "TrafficPolicyVersion" :: (TrafficPolicyVersion)
  , "HostedZoneIdMarker" :: NullOrUndefined (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesByPolicyRequest :: Newtype ListTrafficPolicyInstancesByPolicyRequest _


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPolicyInstancesByPolicyResponse = ListTrafficPolicyInstancesByPolicyResponse 
  { "TrafficPolicyInstances" :: (TrafficPolicyInstances)
  , "HostedZoneIdMarker" :: NullOrUndefined (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType)
  , "IsTruncated" :: (PageTruncated)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesByPolicyResponse :: Newtype ListTrafficPolicyInstancesByPolicyResponse _


-- | <p>A request to get information about the traffic policy instances that you created by using the current AWS account.</p>
newtype ListTrafficPolicyInstancesRequest = ListTrafficPolicyInstancesRequest 
  { "HostedZoneIdMarker" :: NullOrUndefined (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesRequest :: Newtype ListTrafficPolicyInstancesRequest _


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPolicyInstancesResponse = ListTrafficPolicyInstancesResponse 
  { "TrafficPolicyInstances" :: (TrafficPolicyInstances)
  , "HostedZoneIdMarker" :: NullOrUndefined (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined (RRType)
  , "IsTruncated" :: (PageTruncated)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesResponse :: Newtype ListTrafficPolicyInstancesResponse _


-- | <p>A complex type that contains the information about the request to list your traffic policies.</p>
newtype ListTrafficPolicyVersionsRequest = ListTrafficPolicyVersionsRequest 
  { "Id" :: (TrafficPolicyId)
  , "TrafficPolicyVersionMarker" :: NullOrUndefined (TrafficPolicyVersionMarker)
  , "MaxItems" :: NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyVersionsRequest :: Newtype ListTrafficPolicyVersionsRequest _


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPolicyVersionsResponse = ListTrafficPolicyVersionsResponse 
  { "TrafficPolicies" :: (TrafficPolicies)
  , "IsTruncated" :: (PageTruncated)
  , "TrafficPolicyVersionMarker" :: (TrafficPolicyVersionMarker)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyVersionsResponse :: Newtype ListTrafficPolicyVersionsResponse _


-- | <p>A complex type that contains information about that can be associated with your hosted zone.</p>
newtype ListVPCAssociationAuthorizationsRequest = ListVPCAssociationAuthorizationsRequest 
  { "HostedZoneId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListVPCAssociationAuthorizationsRequest :: Newtype ListVPCAssociationAuthorizationsRequest _


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListVPCAssociationAuthorizationsResponse = ListVPCAssociationAuthorizationsResponse 
  { "HostedZoneId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  , "VPCs" :: (VPCs)
  }
derive instance newtypeListVPCAssociationAuthorizationsResponse :: Newtype ListVPCAssociationAuthorizationsResponse _


newtype MaxResults = MaxResults String
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype MeasureLatency = MeasureLatency Boolean
derive instance newtypeMeasureLatency :: Newtype MeasureLatency _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _


newtype Nameserver = Nameserver String
derive instance newtypeNameserver :: Newtype Nameserver _


newtype Namespace = Namespace String
derive instance newtypeNamespace :: Newtype Namespace _


-- | <p>A change with the specified change ID does not exist.</p>
newtype NoSuchChange = NoSuchChange 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchChange :: Newtype NoSuchChange _


-- | <p>There is no CloudWatch Logs log group with the specified ARN.</p>
newtype NoSuchCloudWatchLogsLogGroup = NoSuchCloudWatchLogsLogGroup 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchCloudWatchLogsLogGroup :: Newtype NoSuchCloudWatchLogsLogGroup _


-- | <p>A reusable delegation set with the specified ID does not exist.</p>
newtype NoSuchDelegationSet = NoSuchDelegationSet 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchDelegationSet :: Newtype NoSuchDelegationSet _


-- | <p>Amazon Route 53 doesn't support the specified geolocation.</p>
newtype NoSuchGeoLocation = NoSuchGeoLocation 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchGeoLocation :: Newtype NoSuchGeoLocation _


-- | <p>No health check exists with the ID that you specified in the <code>DeleteHealthCheck</code> request.</p>
newtype NoSuchHealthCheck = NoSuchHealthCheck 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchHealthCheck :: Newtype NoSuchHealthCheck _


-- | <p>No hosted zone exists with the ID that you specified.</p>
newtype NoSuchHostedZone = NoSuchHostedZone 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchHostedZone :: Newtype NoSuchHostedZone _


-- | <p>There is no DNS query logging configuration with the specified ID.</p>
newtype NoSuchQueryLoggingConfig = NoSuchQueryLoggingConfig 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchQueryLoggingConfig :: Newtype NoSuchQueryLoggingConfig _


-- | <p>No traffic policy exists with the specified ID.</p>
newtype NoSuchTrafficPolicy = NoSuchTrafficPolicy 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchTrafficPolicy :: Newtype NoSuchTrafficPolicy _


-- | <p>No traffic policy instance exists with the specified ID.</p>
newtype NoSuchTrafficPolicyInstance = NoSuchTrafficPolicyInstance 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchTrafficPolicyInstance :: Newtype NoSuchTrafficPolicyInstance _


newtype Nonce = Nonce String
derive instance newtypeNonce :: Newtype Nonce _


-- | <p>Associating the specified VPC with the specified hosted zone has not been authorized.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _


newtype PageMarker = PageMarker String
derive instance newtypePageMarker :: Newtype PageMarker _


newtype PageMaxItems = PageMaxItems String
derive instance newtypePageMaxItems :: Newtype PageMaxItems _


newtype PageTruncated = PageTruncated Boolean
derive instance newtypePageTruncated :: Newtype PageTruncated _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


newtype Period = Period Int
derive instance newtypePeriod :: Newtype Period _


newtype Port = Port Int
derive instance newtypePort :: Newtype Port _


-- | <p>If Amazon Route 53 can't process a request before the next request arrives, it will reject subsequent requests for the same hosted zone and return an <code>HTTP 400 error</code> (<code>Bad request</code>). If Amazon Route 53 returns this error repeatedly for the same request, we recommend that you wait, in intervals of increasing duration, before you try the request again.</p>
newtype PriorRequestNotComplete = PriorRequestNotComplete 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePriorRequestNotComplete :: Newtype PriorRequestNotComplete _


-- | <p>You're trying to associate a VPC with a public hosted zone. Amazon Route 53 doesn't support associating a VPC with a public hosted zone.</p>
newtype PublicZoneVPCAssociation = PublicZoneVPCAssociation 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePublicZoneVPCAssociation :: Newtype PublicZoneVPCAssociation _


-- | <p>A complex type that contains information about a configuration for DNS query logging.</p>
newtype QueryLoggingConfig = QueryLoggingConfig 
  { "Id" :: (QueryLoggingConfigId)
  , "HostedZoneId" :: (ResourceId)
  , "CloudWatchLogsLogGroupArn" :: (CloudWatchLogsLogGroupArn)
  }
derive instance newtypeQueryLoggingConfig :: Newtype QueryLoggingConfig _


-- | <p>You can create only one query logging configuration for a hosted zone, and a query logging configuration already exists for this hosted zone.</p>
newtype QueryLoggingConfigAlreadyExists = QueryLoggingConfigAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeQueryLoggingConfigAlreadyExists :: Newtype QueryLoggingConfigAlreadyExists _


newtype QueryLoggingConfigId = QueryLoggingConfigId String
derive instance newtypeQueryLoggingConfigId :: Newtype QueryLoggingConfigId _


newtype QueryLoggingConfigs = QueryLoggingConfigs (Array QueryLoggingConfig)
derive instance newtypeQueryLoggingConfigs :: Newtype QueryLoggingConfigs _


newtype RData = RData String
derive instance newtypeRData :: Newtype RData _


newtype RRType = RRType String
derive instance newtypeRRType :: Newtype RRType _


newtype RecordData = RecordData (Array RecordDataEntry)
derive instance newtypeRecordData :: Newtype RecordData _


-- | <p>A value that Amazon Route 53 returned for this resource record set. A <code>RecordDataEntry</code> element is one of the following:</p> <ul> <li> <p>For non-alias resource record sets, a <code>RecordDataEntry</code> element contains one value in the resource record set. If the resource record set contains multiple values, the response includes one <code>RecordDataEntry</code> element for each value.</p> </li> <li> <p>For multiple resource record sets that have the same name and type, which includes weighted, latency, geolocation, and failover, a <code>RecordDataEntry</code> element contains the value from the appropriate resource record set based on the request.</p> </li> <li> <p>For alias resource record sets that refer to AWS resources other than another resource record set, the <code>RecordDataEntry</code> element contains an IP address or a domain name for the AWS resource, depending on the type of resource.</p> </li> <li> <p>For alias resource record sets that refer to other resource record sets, a <code>RecordDataEntry</code> element contains one value from the referenced resource record set. If the referenced resource record set contains multiple values, the response includes one <code>RecordDataEntry</code> element for each value.</p> </li> </ul>
newtype RecordDataEntry = RecordDataEntry String
derive instance newtypeRecordDataEntry :: Newtype RecordDataEntry _


newtype RequestInterval = RequestInterval Int
derive instance newtypeRequestInterval :: Newtype RequestInterval _


newtype ResettableElementName = ResettableElementName String
derive instance newtypeResettableElementName :: Newtype ResettableElementName _


newtype ResettableElementNameList = ResettableElementNameList (Array ResettableElementName)
derive instance newtypeResettableElementNameList :: Newtype ResettableElementNameList _


newtype ResourceDescription = ResourceDescription String
derive instance newtypeResourceDescription :: Newtype ResourceDescription _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


newtype ResourcePath = ResourcePath String
derive instance newtypeResourcePath :: Newtype ResourcePath _


-- | <p>Information specific to the resource record.</p> <note> <p>If you're creating an alias resource record set, omit <code>ResourceRecord</code>.</p> </note>
newtype ResourceRecord = ResourceRecord 
  { "Value" :: (RData)
  }
derive instance newtypeResourceRecord :: Newtype ResourceRecord _


-- | <p>Information about the resource record set to create or delete.</p>
newtype ResourceRecordSet = ResourceRecordSet 
  { "Name" :: (DNSName)
  , "Type" :: (RRType)
  , "SetIdentifier" :: NullOrUndefined (ResourceRecordSetIdentifier)
  , "Weight" :: NullOrUndefined (ResourceRecordSetWeight)
  , "Region" :: NullOrUndefined (ResourceRecordSetRegion)
  , "GeoLocation" :: NullOrUndefined (GeoLocation)
  , "Failover" :: NullOrUndefined (ResourceRecordSetFailover)
  , "MultiValueAnswer" :: NullOrUndefined (ResourceRecordSetMultiValueAnswer)
  , "TTL" :: NullOrUndefined (TTL)
  , "ResourceRecords" :: NullOrUndefined (ResourceRecords)
  , "AliasTarget" :: NullOrUndefined (AliasTarget)
  , "HealthCheckId" :: NullOrUndefined (HealthCheckId)
  , "TrafficPolicyInstanceId" :: NullOrUndefined (TrafficPolicyInstanceId)
  }
derive instance newtypeResourceRecordSet :: Newtype ResourceRecordSet _


newtype ResourceRecordSetFailover = ResourceRecordSetFailover String
derive instance newtypeResourceRecordSetFailover :: Newtype ResourceRecordSetFailover _


newtype ResourceRecordSetIdentifier = ResourceRecordSetIdentifier String
derive instance newtypeResourceRecordSetIdentifier :: Newtype ResourceRecordSetIdentifier _


newtype ResourceRecordSetMultiValueAnswer = ResourceRecordSetMultiValueAnswer Boolean
derive instance newtypeResourceRecordSetMultiValueAnswer :: Newtype ResourceRecordSetMultiValueAnswer _


newtype ResourceRecordSetRegion = ResourceRecordSetRegion String
derive instance newtypeResourceRecordSetRegion :: Newtype ResourceRecordSetRegion _


newtype ResourceRecordSetWeight = ResourceRecordSetWeight Number
derive instance newtypeResourceRecordSetWeight :: Newtype ResourceRecordSetWeight _


newtype ResourceRecordSets = ResourceRecordSets (Array ResourceRecordSet)
derive instance newtypeResourceRecordSets :: Newtype ResourceRecordSets _


newtype ResourceRecords = ResourceRecords (Array ResourceRecord)
derive instance newtypeResourceRecords :: Newtype ResourceRecords _


-- | <p>A complex type containing a resource and its associated tags.</p>
newtype ResourceTagSet = ResourceTagSet 
  { "ResourceType" :: NullOrUndefined (TagResourceType)
  , "ResourceId" :: NullOrUndefined (TagResourceId)
  , "Tags" :: NullOrUndefined (TagList)
  }
derive instance newtypeResourceTagSet :: Newtype ResourceTagSet _


newtype ResourceTagSetList = ResourceTagSetList (Array ResourceTagSet)
derive instance newtypeResourceTagSetList :: Newtype ResourceTagSetList _


newtype ResourceURI = ResourceURI String
derive instance newtypeResourceURI :: Newtype ResourceURI _


-- | <p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>
newtype ReusableDelegationSetLimit = ReusableDelegationSetLimit 
  { "Type" :: (ReusableDelegationSetLimitType)
  , "Value" :: (LimitValue)
  }
derive instance newtypeReusableDelegationSetLimit :: Newtype ReusableDelegationSetLimit _


newtype ReusableDelegationSetLimitType = ReusableDelegationSetLimitType String
derive instance newtypeReusableDelegationSetLimitType :: Newtype ReusableDelegationSetLimitType _


newtype SearchString = SearchString String
derive instance newtypeSearchString :: Newtype SearchString _


newtype ServicePrincipal = ServicePrincipal String
derive instance newtypeServicePrincipal :: Newtype ServicePrincipal _


newtype Statistic = Statistic String
derive instance newtypeStatistic :: Newtype Statistic _


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _


-- | <p>A complex type that contains the status that one Amazon Route 53 health checker reports and the time of the health check.</p>
newtype StatusReport = StatusReport 
  { "Status" :: NullOrUndefined (Status)
  , "CheckedTime" :: NullOrUndefined (TimeStamp)
  }
derive instance newtypeStatusReport :: Newtype StatusReport _


newtype SubnetMask = SubnetMask String
derive instance newtypeSubnetMask :: Newtype SubnetMask _


newtype TTL = TTL Number
derive instance newtypeTTL :: Newtype TTL _


-- | <p>A complex type that contains information about a tag that you want to add or edit for the specified health check or hosted zone.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagResourceId = TagResourceId String
derive instance newtypeTagResourceId :: Newtype TagResourceId _


newtype TagResourceIdList = TagResourceIdList (Array TagResourceId)
derive instance newtypeTagResourceIdList :: Newtype TagResourceIdList _


newtype TagResourceType = TagResourceType String
derive instance newtypeTagResourceType :: Newtype TagResourceType _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


-- | <p>Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask. </p>
newtype TestDNSAnswerRequest = TestDNSAnswerRequest 
  { "HostedZoneId" :: (ResourceId)
  , "RecordName" :: (DNSName)
  , "RecordType" :: (RRType)
  , "ResolverIP" :: NullOrUndefined (IPAddress)
  , "EDNS0ClientSubnetIP" :: NullOrUndefined (IPAddress)
  , "EDNS0ClientSubnetMask" :: NullOrUndefined (SubnetMask)
  }
derive instance newtypeTestDNSAnswerRequest :: Newtype TestDNSAnswerRequest _


-- | <p>A complex type that contains the response to a <code>TestDNSAnswer</code> request. </p>
newtype TestDNSAnswerResponse = TestDNSAnswerResponse 
  { "Nameserver" :: (Nameserver)
  , "RecordName" :: (DNSName)
  , "RecordType" :: (RRType)
  , "RecordData" :: (RecordData)
  , "ResponseCode" :: (DNSRCode)
  , "Protocol" :: (TransportProtocol)
  }
derive instance newtypeTestDNSAnswerResponse :: Newtype TestDNSAnswerResponse _


newtype Threshold = Threshold Number
derive instance newtypeThreshold :: Newtype Threshold _


-- | <p>The limit on the number of requests per second was exceeded.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _


newtype TimeStamp = TimeStamp Number
derive instance newtypeTimeStamp :: Newtype TimeStamp _


-- | <p>This health check can't be created because the current account has reached the limit on the number of active health checks.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>For information about how to get the current limit for an account, see <a>GetAccountLimit</a>. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p> <p>You have reached the maximum number of active health checks for an AWS account. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype TooManyHealthChecks = TooManyHealthChecks 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyHealthChecks :: Newtype TooManyHealthChecks _


-- | <p>This operation can't be completed either because the current account has reached the limit on the number of hosted zones or because you've reached the limit on the number of hosted zones that can be associated with a reusable delegation set.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>To get the current limit on hosted zones that can be created by an account, see <a>GetAccountLimit</a>.</p> <p>To get the current limit on hosted zones that can be associated with a reusable delegation set, see <a>GetReusableDelegationSetLimit</a>.</p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype TooManyHostedZones = TooManyHostedZones 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyHostedZones :: Newtype TooManyHostedZones _


-- | <p>This traffic policy can't be created because the current account has reached the limit on the number of traffic policies.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>To get the current limit for an account, see <a>GetAccountLimit</a>. </p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype TooManyTrafficPolicies = TooManyTrafficPolicies 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyTrafficPolicies :: Newtype TooManyTrafficPolicies _


-- | <p>This traffic policy instance can't be created because the current account has reached the limit on the number of traffic policy instances.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>For information about how to get the current limit for an account, see <a>GetAccountLimit</a>.</p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype TooManyTrafficPolicyInstances = TooManyTrafficPolicyInstances 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyTrafficPolicyInstances :: Newtype TooManyTrafficPolicyInstances _


-- | <p>This traffic policy version can't be created because you've reached the limit of 1000 on the number of versions that you can create for the current traffic policy.</p> <p>To create more traffic policy versions, you can use <a>GetTrafficPolicy</a> to get the traffic policy document for a specified traffic policy version, and then use <a>CreateTrafficPolicy</a> to create a new traffic policy using the traffic policy document.</p>
newtype TooManyTrafficPolicyVersionsForCurrentPolicy = TooManyTrafficPolicyVersionsForCurrentPolicy 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyTrafficPolicyVersionsForCurrentPolicy :: Newtype TooManyTrafficPolicyVersionsForCurrentPolicy _


-- | <p>You've created the maximum number of authorizations that can be created for the specified hosted zone. To authorize another VPC to be associated with the hosted zone, submit a <code>DeleteVPCAssociationAuthorization</code> request to remove an existing authorization. To get a list of existing authorizations, submit a <code>ListVPCAssociationAuthorizations</code> request.</p>
newtype TooManyVPCAssociationAuthorizations = TooManyVPCAssociationAuthorizations 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyVPCAssociationAuthorizations :: Newtype TooManyVPCAssociationAuthorizations _


newtype TrafficPolicies = TrafficPolicies (Array TrafficPolicy)
derive instance newtypeTrafficPolicies :: Newtype TrafficPolicies _


-- | <p>A complex type that contains settings for a traffic policy.</p>
newtype TrafficPolicy = TrafficPolicy 
  { "Id" :: (TrafficPolicyId)
  , "Version" :: (TrafficPolicyVersion)
  , "Name" :: (TrafficPolicyName)
  , "Type" :: (RRType)
  , "Document" :: (TrafficPolicyDocument)
  , "Comment" :: NullOrUndefined (TrafficPolicyComment)
  }
derive instance newtypeTrafficPolicy :: Newtype TrafficPolicy _


-- | <p>A traffic policy that has the same value for <code>Name</code> already exists.</p>
newtype TrafficPolicyAlreadyExists = TrafficPolicyAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTrafficPolicyAlreadyExists :: Newtype TrafficPolicyAlreadyExists _


newtype TrafficPolicyComment = TrafficPolicyComment String
derive instance newtypeTrafficPolicyComment :: Newtype TrafficPolicyComment _


newtype TrafficPolicyDocument = TrafficPolicyDocument String
derive instance newtypeTrafficPolicyDocument :: Newtype TrafficPolicyDocument _


newtype TrafficPolicyId = TrafficPolicyId String
derive instance newtypeTrafficPolicyId :: Newtype TrafficPolicyId _


-- | <p>One or more traffic policy instances were created by using the specified traffic policy.</p>
newtype TrafficPolicyInUse = TrafficPolicyInUse 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTrafficPolicyInUse :: Newtype TrafficPolicyInUse _


-- | <p>A complex type that contains settings for the new traffic policy instance.</p>
newtype TrafficPolicyInstance = TrafficPolicyInstance 
  { "Id" :: (TrafficPolicyInstanceId)
  , "HostedZoneId" :: (ResourceId)
  , "Name" :: (DNSName)
  , "TTL" :: (TTL)
  , "State" :: (TrafficPolicyInstanceState)
  , "Message" :: (Message)
  , "TrafficPolicyId" :: (TrafficPolicyId)
  , "TrafficPolicyVersion" :: (TrafficPolicyVersion)
  , "TrafficPolicyType" :: (RRType)
  }
derive instance newtypeTrafficPolicyInstance :: Newtype TrafficPolicyInstance _


-- | <p>There is already a traffic policy instance with the specified ID.</p>
newtype TrafficPolicyInstanceAlreadyExists = TrafficPolicyInstanceAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTrafficPolicyInstanceAlreadyExists :: Newtype TrafficPolicyInstanceAlreadyExists _


newtype TrafficPolicyInstanceCount = TrafficPolicyInstanceCount Int
derive instance newtypeTrafficPolicyInstanceCount :: Newtype TrafficPolicyInstanceCount _


newtype TrafficPolicyInstanceId = TrafficPolicyInstanceId String
derive instance newtypeTrafficPolicyInstanceId :: Newtype TrafficPolicyInstanceId _


newtype TrafficPolicyInstanceState = TrafficPolicyInstanceState String
derive instance newtypeTrafficPolicyInstanceState :: Newtype TrafficPolicyInstanceState _


newtype TrafficPolicyInstances = TrafficPolicyInstances (Array TrafficPolicyInstance)
derive instance newtypeTrafficPolicyInstances :: Newtype TrafficPolicyInstances _


newtype TrafficPolicyName = TrafficPolicyName String
derive instance newtypeTrafficPolicyName :: Newtype TrafficPolicyName _


newtype TrafficPolicySummaries = TrafficPolicySummaries (Array TrafficPolicySummary)
derive instance newtypeTrafficPolicySummaries :: Newtype TrafficPolicySummaries _


-- | <p>A complex type that contains information about the latest version of one traffic policy that is associated with the current AWS account.</p>
newtype TrafficPolicySummary = TrafficPolicySummary 
  { "Id" :: (TrafficPolicyId)
  , "Name" :: (TrafficPolicyName)
  , "Type" :: (RRType)
  , "LatestVersion" :: (TrafficPolicyVersion)
  , "TrafficPolicyCount" :: (TrafficPolicyVersion)
  }
derive instance newtypeTrafficPolicySummary :: Newtype TrafficPolicySummary _


newtype TrafficPolicyVersion = TrafficPolicyVersion Int
derive instance newtypeTrafficPolicyVersion :: Newtype TrafficPolicyVersion _


newtype TrafficPolicyVersionMarker = TrafficPolicyVersionMarker String
derive instance newtypeTrafficPolicyVersionMarker :: Newtype TrafficPolicyVersionMarker _


newtype TransportProtocol = TransportProtocol String
derive instance newtypeTransportProtocol :: Newtype TransportProtocol _


-- | <p>A complex type that contains information about a request to update a health check.</p>
newtype UpdateHealthCheckRequest = UpdateHealthCheckRequest 
  { "HealthCheckId" :: (HealthCheckId)
  , "HealthCheckVersion" :: NullOrUndefined (HealthCheckVersion)
  , "IPAddress" :: NullOrUndefined (IPAddress)
  , "Port" :: NullOrUndefined (Port)
  , "ResourcePath" :: NullOrUndefined (ResourcePath)
  , "FullyQualifiedDomainName" :: NullOrUndefined (FullyQualifiedDomainName)
  , "SearchString" :: NullOrUndefined (SearchString)
  , "FailureThreshold" :: NullOrUndefined (FailureThreshold)
  , "Inverted" :: NullOrUndefined (Inverted)
  , "HealthThreshold" :: NullOrUndefined (HealthThreshold)
  , "ChildHealthChecks" :: NullOrUndefined (ChildHealthCheckList)
  , "EnableSNI" :: NullOrUndefined (EnableSNI)
  , "Regions" :: NullOrUndefined (HealthCheckRegionList)
  , "AlarmIdentifier" :: NullOrUndefined (AlarmIdentifier)
  , "InsufficientDataHealthStatus" :: NullOrUndefined (InsufficientDataHealthStatus)
  , "ResetElements" :: NullOrUndefined (ResettableElementNameList)
  }
derive instance newtypeUpdateHealthCheckRequest :: Newtype UpdateHealthCheckRequest _


newtype UpdateHealthCheckResponse = UpdateHealthCheckResponse 
  { "HealthCheck" :: (HealthCheck)
  }
derive instance newtypeUpdateHealthCheckResponse :: Newtype UpdateHealthCheckResponse _


-- | <p>A request to update the comment for a hosted zone.</p>
newtype UpdateHostedZoneCommentRequest = UpdateHostedZoneCommentRequest 
  { "Id" :: (ResourceId)
  , "Comment" :: NullOrUndefined (ResourceDescription)
  }
derive instance newtypeUpdateHostedZoneCommentRequest :: Newtype UpdateHostedZoneCommentRequest _


-- | <p>A complex type that contains the response to the <code>UpdateHostedZoneComment</code> request.</p>
newtype UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse 
  { "HostedZone" :: (HostedZone)
  }
derive instance newtypeUpdateHostedZoneCommentResponse :: Newtype UpdateHostedZoneCommentResponse _


-- | <p>A complex type that contains information about the traffic policy that you want to update the comment for.</p>
newtype UpdateTrafficPolicyCommentRequest = UpdateTrafficPolicyCommentRequest 
  { "Id" :: (TrafficPolicyId)
  , "Version" :: (TrafficPolicyVersion)
  , "Comment" :: (TrafficPolicyComment)
  }
derive instance newtypeUpdateTrafficPolicyCommentRequest :: Newtype UpdateTrafficPolicyCommentRequest _


-- | <p>A complex type that contains the response information for the traffic policy.</p>
newtype UpdateTrafficPolicyCommentResponse = UpdateTrafficPolicyCommentResponse 
  { "TrafficPolicy" :: (TrafficPolicy)
  }
derive instance newtypeUpdateTrafficPolicyCommentResponse :: Newtype UpdateTrafficPolicyCommentResponse _


-- | <p>A complex type that contains information about the resource record sets that you want to update based on a specified traffic policy instance.</p>
newtype UpdateTrafficPolicyInstanceRequest = UpdateTrafficPolicyInstanceRequest 
  { "Id" :: (TrafficPolicyInstanceId)
  , "TTL" :: (TTL)
  , "TrafficPolicyId" :: (TrafficPolicyId)
  , "TrafficPolicyVersion" :: (TrafficPolicyVersion)
  }
derive instance newtypeUpdateTrafficPolicyInstanceRequest :: Newtype UpdateTrafficPolicyInstanceRequest _


-- | <p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>
newtype UpdateTrafficPolicyInstanceResponse = UpdateTrafficPolicyInstanceResponse 
  { "TrafficPolicyInstance" :: (TrafficPolicyInstance)
  }
derive instance newtypeUpdateTrafficPolicyInstanceResponse :: Newtype UpdateTrafficPolicyInstanceResponse _


newtype UsageCount = UsageCount Number
derive instance newtypeUsageCount :: Newtype UsageCount _


-- | <p>(Private hosted zones only) A complex type that contains information about an Amazon VPC.</p>
newtype VPC = VPC 
  { "VPCRegion" :: NullOrUndefined (VPCRegion)
  , "VPCId" :: NullOrUndefined (VPCId)
  }
derive instance newtypeVPC :: Newtype VPC _


-- | <p>The VPC that you specified is not authorized to be associated with the hosted zone.</p>
newtype VPCAssociationAuthorizationNotFound = VPCAssociationAuthorizationNotFound 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeVPCAssociationAuthorizationNotFound :: Newtype VPCAssociationAuthorizationNotFound _


-- | <p>The specified VPC and hosted zone are not currently associated.</p>
newtype VPCAssociationNotFound = VPCAssociationNotFound 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeVPCAssociationNotFound :: Newtype VPCAssociationNotFound _


-- | <p>(Private hosted zones only) The ID of an Amazon VPC. </p>
newtype VPCId = VPCId String
derive instance newtypeVPCId :: Newtype VPCId _


newtype VPCRegion = VPCRegion String
derive instance newtypeVPCRegion :: Newtype VPCRegion _


-- | <p>(Private hosted zones only) A list of <code>VPC</code> elements.</p>
newtype VPCs = VPCs (Array VPC)
derive instance newtypeVPCs :: Newtype VPCs _
