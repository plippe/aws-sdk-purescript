

module AWS.Route53 where

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

serviceName = "Route53" :: String


-- | <p>Associates an Amazon VPC with a private hosted zone. </p> <important> <p>To perform the association, the VPC and the private hosted zone must already exist. You can't convert a public hosted zone into a private hosted zone.</p> </important> <note> <p>If you want to associate a VPC that was created by using one AWS account with a private hosted zone that was created by using a different account, the AWS account that created the private hosted zone must first submit a <code>CreateVPCAssociationAuthorization</code> request. Then the account that created the VPC must submit an <code>AssociateVPCWithHostedZone</code> request.</p> </note>
associateVPCWithHostedZone :: forall eff. AssociateVPCWithHostedZoneRequest -> Aff (exception :: EXCEPTION | eff) AssociateVPCWithHostedZoneResponse
associateVPCWithHostedZone = Request.request serviceName "associateVPCWithHostedZone" 


-- | <p>Creates, changes, or deletes a resource record set, which contains authoritative DNS information for a specified domain name or subdomain name. For example, you can use <code>ChangeResourceRecordSets</code> to create a resource record set that routes traffic for test.example.com to a web server that has an IP address of 192.0.2.44.</p> <p> <b>Change Batches and Transactional Changes</b> </p> <p>The request body must include a document with a <code>ChangeResourceRecordSetsRequest</code> element. The request body contains a list of change items, known as a change batch. Change batches are considered transactional changes. When using the Amazon Route 53 API to change resource record sets, Amazon Route 53 either makes all or none of the changes in a change batch request. This ensures that Amazon Route 53 never partially implements the intended changes to the resource record sets in a hosted zone. </p> <p>For example, a change batch request that deletes the <code>CNAME</code> record for www.example.com and creates an alias resource record set for www.example.com. Amazon Route 53 deletes the first resource record set and creates the second resource record set in a single operation. If either the <code>DELETE</code> or the <code>CREATE</code> action fails, then both changes (plus any other changes in the batch) fail, and the original <code>CNAME</code> record continues to exist.</p> <important> <p>Due to the nature of transactional changes, you can't delete the same resource record set more than once in a single change batch. If you attempt to delete the same change batch more than once, Amazon Route 53 returns an <code>InvalidChangeBatch</code> error.</p> </important> <p> <b>Traffic Flow</b> </p> <p>To create resource record sets for complex routing configurations, use either the traffic flow visual editor in the Amazon Route 53 console or the API actions for traffic policies and traffic policy instances. Save the configuration as a traffic policy, then associate the traffic policy with one or more domain names (such as example.com) or subdomain names (such as www.example.com), in the same hosted zone or in multiple hosted zones. You can roll back the updates if the new configuration isn't performing as expected. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/traffic-flow.html">Using Traffic Flow to Route DNS Traffic</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p> <b>Create, Delete, and Upsert</b> </p> <p>Use <code>ChangeResourceRecordsSetsRequest</code> to perform the following actions:</p> <ul> <li> <p> <code>CREATE</code>: Creates a resource record set that has the specified values.</p> </li> <li> <p> <code>DELETE</code>: Deletes an existing resource record set that has the specified values.</p> </li> <li> <p> <code>UPSERT</code>: If a resource record set does not already exist, AWS creates it. If a resource set does exist, Amazon Route 53 updates it with the values in the request. </p> </li> </ul> <p> <b>Syntaxes for Creating, Updating, and Deleting Resource Record Sets</b> </p> <p>The syntax for a request depends on the type of resource record set that you want to create, delete, or update, such as weighted, alias, or failover. The XML elements in your request must appear in the order listed in the syntax. </p> <p>For an example for each type of resource record set, see "Examples."</p> <p>Don't refer to the syntax in the "Parameter Syntax" section, which includes all of the elements for every kind of resource record set that you can create, delete, or update by using <code>ChangeResourceRecordSets</code>. </p> <p> <b>Change Propagation to Amazon Route 53 DNS Servers</b> </p> <p>When you submit a <code>ChangeResourceRecordSets</code> request, Amazon Route 53 propagates your changes to all of the Amazon Route 53 authoritative DNS servers. While your changes are propagating, <code>GetChange</code> returns a status of <code>PENDING</code>. When propagation is complete, <code>GetChange</code> returns a status of <code>INSYNC</code>. Changes generally propagate to all Amazon Route 53 name servers within 60 seconds. For more information, see <a>GetChange</a>.</p> <p> <b>Limits on ChangeResourceRecordSets Requests</b> </p> <p>For information about the limits on a <code>ChangeResourceRecordSets</code> request, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>
changeResourceRecordSets :: forall eff. ChangeResourceRecordSetsRequest -> Aff (exception :: EXCEPTION | eff) ChangeResourceRecordSetsResponse
changeResourceRecordSets = Request.request serviceName "changeResourceRecordSets" 


-- | <p>Adds, edits, or deletes tags for a health check or a hosted zone.</p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>
changeTagsForResource :: forall eff. ChangeTagsForResourceRequest -> Aff (exception :: EXCEPTION | eff) ChangeTagsForResourceResponse
changeTagsForResource = Request.request serviceName "changeTagsForResource" 


-- | <p>Creates a new health check.</p> <p>For information about adding health checks to resource record sets, see <a>ResourceRecordSet$HealthCheckId</a> in <a>ChangeResourceRecordSets</a>. </p> <p> <b>ELB Load Balancers</b> </p> <p>If you're registering EC2 instances with an Elastic Load Balancing (ELB) load balancer, do not create Amazon Route 53 health checks for the EC2 instances. When you register an EC2 instance with a load balancer, you configure settings for an ELB health check, which performs a similar function to an Amazon Route 53 health check.</p> <p> <b>Private Hosted Zones</b> </p> <p>You can associate health checks with failover resource record sets in a private hosted zone. Note the following:</p> <ul> <li> <p>Amazon Route 53 health checkers are outside the VPC. To check the health of an endpoint within a VPC by IP address, you must assign a public IP address to the instance in the VPC.</p> </li> <li> <p>You can configure a health checker to check the health of an external resource that the instance relies on, such as a database server.</p> </li> <li> <p>You can create a CloudWatch metric, associate an alarm with the metric, and then create a health check that is based on the state of the alarm. For example, you might create a CloudWatch metric that checks the status of the Amazon EC2 <code>StatusCheckFailed</code> metric, add an alarm to the metric, and then create a health check that is based on the state of the alarm. For information about creating CloudWatch metrics and alarms by using the CloudWatch console, see the <a href="http://docs.aws.amazon.com/AmazonCloudWatch/latest/DeveloperGuide/WhatIsCloudWatch.html">Amazon CloudWatch User Guide</a>.</p> </li> </ul>
createHealthCheck :: forall eff. CreateHealthCheckRequest -> Aff (exception :: EXCEPTION | eff) CreateHealthCheckResponse
createHealthCheck = Request.request serviceName "createHealthCheck" 


-- | <p>Creates a new public hosted zone, which you use to specify how the Domain Name System (DNS) routes traffic on the Internet for a domain, such as example.com, and its subdomains. </p> <important> <p>You can't convert a public hosted zones to a private hosted zone or vice versa. Instead, you must create a new hosted zone with the same name and create new resource record sets.</p> </important> <p>For more information about charges for hosted zones, see <a href="http://aws.amazon.com/route53/pricing/">Amazon Route 53 Pricing</a>.</p> <p>Note the following:</p> <ul> <li> <p>You can't create a hosted zone for a top-level domain (TLD).</p> </li> <li> <p>Amazon Route 53 automatically creates a default SOA record and four NS records for the zone. For more information about SOA and NS records, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/SOA-NSrecords.html">NS and SOA Records that Amazon Route 53 Creates for a Hosted Zone</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>If you want to use the same name servers for multiple hosted zones, you can optionally associate a reusable delegation set with the hosted zone. See the <code>DelegationSetId</code> element.</p> </li> <li> <p>If your domain is registered with a registrar other than Amazon Route 53, you must update the name servers with your registrar to make Amazon Route 53 your DNS service. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/creating-migrating.html">Configuring Amazon Route 53 as your DNS Service</a> in the <i>Amazon Route 53 Developer Guide</i>. </p> </li> </ul> <p>When you submit a <code>CreateHostedZone</code> request, the initial status of the hosted zone is <code>PENDING</code>. This means that the NS and SOA records are not yet available on all Amazon Route 53 DNS servers. When the NS and SOA records are available, the status of the zone changes to <code>INSYNC</code>.</p>
createHostedZone :: forall eff. CreateHostedZoneRequest -> Aff (exception :: EXCEPTION | eff) CreateHostedZoneResponse
createHostedZone = Request.request serviceName "createHostedZone" 


-- | <p>Creates a configuration for DNS query logging. After you create a query logging configuration, Amazon Route 53 begins to publish log data to an Amazon CloudWatch Logs log group.</p> <p>DNS query logs contain information about the queries that Amazon Route 53 receives for a specified public hosted zone, such as the following:</p> <ul> <li> <p>Amazon Route 53 edge location that responded to the DNS query</p> </li> <li> <p>Domain or subdomain that was requested</p> </li> <li> <p>DNS record type, such as A or AAAA</p> </li> <li> <p>DNS response code, such as <code>NoError</code> or <code>ServFail</code> </p> </li> </ul> <dl> <dt>Log Group and Resource Policy</dt> <dd> <p>Before you create a query logging configuration, perform the following operations.</p> <note> <p>If you create a query logging configuration using the Amazon Route 53 console, Amazon Route 53 performs these operations automatically.</p> </note> <ol> <li> <p>Create a CloudWatch Logs log group, and make note of the ARN, which you specify when you create a query logging configuration. Note the following:</p> <ul> <li> <p>You must create the log group in the us-east-1 region.</p> </li> <li> <p>You must use the same AWS account to create the log group and the hosted zone that you want to configure query logging for.</p> </li> <li> <p>When you create log groups for query logging, we recommend that you use a consistent prefix, for example:</p> <p> <code>/aws/route53/<i>hosted zone name</i> </code> </p> <p>In the next step, you'll create a resource policy, which controls access to one or more log groups and the associated AWS resources, such as Amazon Route 53 hosted zones. There's a limit on the number of resource policies that you can create, so we recommend that you use a consistent prefix so you can use the same resource policy for all the log groups that you create for query logging.</p> </li> </ul> </li> <li> <p>Create a CloudWatch Logs resource policy, and give it the permissions that Amazon Route 53 needs to create log streams and to send query logs to log streams. For the value of <code>Resource</code>, specify the ARN for the log group that you created in the previous step. To use the same resource policy for all the CloudWatch Logs log groups that you created for query logging configurations, replace the hosted zone name with <code>*</code>, for example:</p> <p> <code>arn:aws:logs:us-east-1:123412341234:log-group:/aws/route53/*</code> </p> <note> <p>You can't use the CloudWatch console to create or edit a resource policy. You must use the CloudWatch API, one of the AWS SDKs, or the AWS CLI.</p> </note> </li> </ol> </dd> <dt>Log Streams and Edge Locations</dt> <dd> <p>When Amazon Route 53 finishes creating the configuration for DNS query logging, it does the following:</p> <ul> <li> <p>Creates a log stream for an edge location the first time that the edge location responds to DNS queries for the specified hosted zone. That log stream is used to log all queries that Amazon Route 53 responds to for that edge location.</p> </li> <li> <p>Begins to send query logs to the applicable log stream.</p> </li> </ul> <p>The name of each log stream is in the following format:</p> <p> <code> <i>hosted zone ID</i>/<i>edge location code</i> </code> </p> <p>The edge location code is a three-letter code and an arbitrarily assigned number, for example, DFW3. The three-letter code typically corresponds with the International Air Transport Association airport code for an airport near the edge location. (These abbreviations might change in the future.) For a list of edge locations, see "The Amazon Route 53 Global Network" on the <a href="http://aws.amazon.com/route53/details/">Amazon Route 53 Product Details</a> page.</p> </dd> <dt>Queries That Are Logged</dt> <dd> <p>Query logs contain only the queries that DNS resolvers forward to Amazon Route 53. If a DNS resolver has already cached the response to a query (such as the IP address for a load balancer for example.com), the resolver will continue to return the cached response. It doesn't forward another query to Amazon Route 53 until the TTL for the corresponding resource record set expires. Depending on how many DNS queries are submitted for a resource record set, and depending on the TTL for that resource record set, query logs might contain information about only one query out of every several thousand queries that are submitted to DNS. For more information about how DNS works, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/welcome-dns-service.html">Routing Internet Traffic to Your Website or Web Application</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </dd> <dt>Log File Format</dt> <dd> <p>For a list of the values in each query log and the format of each value, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </dd> <dt>Pricing</dt> <dd> <p>For information about charges for query logs, see <a href="http://aws.amazon.com/cloudwatch/pricing/">Amazon CloudWatch Pricing</a>.</p> </dd> <dt>How to Stop Logging</dt> <dd> <p>If you want Amazon Route 53 to stop sending query logs to CloudWatch Logs, delete the query logging configuration. For more information, see <a>DeleteQueryLoggingConfig</a>.</p> </dd> </dl>
createQueryLoggingConfig :: forall eff. CreateQueryLoggingConfigRequest -> Aff (exception :: EXCEPTION | eff) CreateQueryLoggingConfigResponse
createQueryLoggingConfig = Request.request serviceName "createQueryLoggingConfig" 


-- | <p>Creates a delegation set (a group of four name servers) that can be reused by multiple hosted zones. If a hosted zoned ID is specified, <code>CreateReusableDelegationSet</code> marks the delegation set associated with that zone as reusable.</p> <note> <p>You can't associate a reusable delegation set with a private hosted zone.</p> </note> <p>For information about using a reusable delegation set to configure white label name servers, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/white-label-name-servers.html">Configuring White Label Name Servers</a>.</p> <p>The process for migrating existing hosted zones to use a reusable delegation set is comparable to the process for configuring white label name servers. You need to perform the following steps:</p> <ol> <li> <p>Create a reusable delegation set.</p> </li> <li> <p>Recreate hosted zones, and reduce the TTL to 60 seconds or less.</p> </li> <li> <p>Recreate resource record sets in the new hosted zones.</p> </li> <li> <p>Change the registrar's name servers to use the name servers for the new hosted zones.</p> </li> <li> <p>Monitor traffic for the website or application.</p> </li> <li> <p>Change TTLs back to their original values.</p> </li> </ol> <p>If you want to migrate existing hosted zones to use a reusable delegation set, the existing hosted zones can't use any of the name servers that are assigned to the reusable delegation set. If one or more hosted zones do use one or more name servers that are assigned to the reusable delegation set, you can do one of the following:</p> <ul> <li> <p>For small numbers of hosted zones—up to a few hundred—it's relatively easy to create reusable delegation sets until you get one that has four name servers that don't overlap with any of the name servers in your hosted zones.</p> </li> <li> <p>For larger numbers of hosted zones, the easiest solution is to use more than one reusable delegation set.</p> </li> <li> <p>For larger numbers of hosted zones, you can also migrate hosted zones that have overlapping name servers to hosted zones that don't have overlapping name servers, then migrate the hosted zones again to use the reusable delegation set.</p> </li> </ul>
createReusableDelegationSet :: forall eff. CreateReusableDelegationSetRequest -> Aff (exception :: EXCEPTION | eff) CreateReusableDelegationSetResponse
createReusableDelegationSet = Request.request serviceName "createReusableDelegationSet" 


-- | <p>Creates a traffic policy, which you use to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com).</p>
createTrafficPolicy :: forall eff. CreateTrafficPolicyRequest -> Aff (exception :: EXCEPTION | eff) CreateTrafficPolicyResponse
createTrafficPolicy = Request.request serviceName "createTrafficPolicy" 


-- | <p>Creates resource record sets in a specified hosted zone based on the settings in a specified traffic policy version. In addition, <code>CreateTrafficPolicyInstance</code> associates the resource record sets with a specified domain name (such as example.com) or subdomain name (such as www.example.com). Amazon Route 53 responds to DNS queries for the domain or subdomain name by using the resource record sets that <code>CreateTrafficPolicyInstance</code> created.</p>
createTrafficPolicyInstance :: forall eff. CreateTrafficPolicyInstanceRequest -> Aff (exception :: EXCEPTION | eff) CreateTrafficPolicyInstanceResponse
createTrafficPolicyInstance = Request.request serviceName "createTrafficPolicyInstance" 


-- | <p>Creates a new version of an existing traffic policy. When you create a new version of a traffic policy, you specify the ID of the traffic policy that you want to update and a JSON-formatted document that describes the new version. You use traffic policies to create multiple DNS resource record sets for one domain name (such as example.com) or one subdomain name (such as www.example.com). You can create a maximum of 1000 versions of a traffic policy. If you reach the limit and need to create another version, you'll need to start a new traffic policy.</p>
createTrafficPolicyVersion :: forall eff. CreateTrafficPolicyVersionRequest -> Aff (exception :: EXCEPTION | eff) CreateTrafficPolicyVersionResponse
createTrafficPolicyVersion = Request.request serviceName "createTrafficPolicyVersion" 


-- | <p>Authorizes the AWS account that created a specified VPC to submit an <code>AssociateVPCWithHostedZone</code> request to associate the VPC with a specified hosted zone that was created by a different account. To submit a <code>CreateVPCAssociationAuthorization</code> request, you must use the account that created the hosted zone. After you authorize the association, use the account that created the VPC to submit an <code>AssociateVPCWithHostedZone</code> request.</p> <note> <p>If you want to associate multiple VPCs that you created by using one account with a hosted zone that you created by using a different account, you must submit one authorization request for each VPC.</p> </note>
createVPCAssociationAuthorization :: forall eff. CreateVPCAssociationAuthorizationRequest -> Aff (exception :: EXCEPTION | eff) CreateVPCAssociationAuthorizationResponse
createVPCAssociationAuthorization = Request.request serviceName "createVPCAssociationAuthorization" 


-- | <p>Deletes a health check.</p> <important> <p>Amazon Route 53 does not prevent you from deleting a health check even if the health check is associated with one or more resource record sets. If you delete a health check and you don't update the associated resource record sets, the future status of the health check can't be predicted and may change. This will affect the routing of DNS queries for your DNS failover configuration. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html#health-checks-deleting.html">Replacing and Deleting Health Checks</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> </important>
deleteHealthCheck :: forall eff. DeleteHealthCheckRequest -> Aff (exception :: EXCEPTION | eff) DeleteHealthCheckResponse
deleteHealthCheck = Request.request serviceName "deleteHealthCheck" 


-- | <p>Deletes a hosted zone.</p> <important> <p>If the name servers for the hosted zone are associated with a domain and if you want to make the domain unavailable on the Internet, we recommend that you delete the name servers from the domain to prevent future DNS queries from possibly being misrouted. If the domain is registered with Amazon Route 53, see <code>UpdateDomainNameservers</code>. If the domain is registered with another registrar, use the method provided by the registrar to delete name servers for the domain.</p> <p>Some domain registries don't allow you to remove all of the name servers for a domain. If the registry for your domain requires one or more name servers, we recommend that you delete the hosted zone only if you transfer DNS service to another service provider, and you replace the name servers for the domain with name servers from the new provider.</p> </important> <p>You can delete a hosted zone only if it contains only the default SOA record and NS resource record sets. If the hosted zone contains other resource record sets, you must delete them before you can delete the hosted zone. If you try to delete a hosted zone that contains other resource record sets, the request fails, and Amazon Route 53 returns a <code>HostedZoneNotEmpty</code> error. For information about deleting records from your hosted zone, see <a>ChangeResourceRecordSets</a>.</p> <p>To verify that the hosted zone has been deleted, do one of the following:</p> <ul> <li> <p>Use the <code>GetHostedZone</code> action to request information about the hosted zone.</p> </li> <li> <p>Use the <code>ListHostedZones</code> action to get a list of the hosted zones associated with the current AWS account.</p> </li> </ul>
deleteHostedZone :: forall eff. DeleteHostedZoneRequest -> Aff (exception :: EXCEPTION | eff) DeleteHostedZoneResponse
deleteHostedZone = Request.request serviceName "deleteHostedZone" 


-- | <p>Deletes a configuration for DNS query logging. If you delete a configuration, Amazon Route 53 stops sending query logs to CloudWatch Logs. Amazon Route 53 doesn't delete any logs that are already in CloudWatch Logs.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a>.</p>
deleteQueryLoggingConfig :: forall eff. DeleteQueryLoggingConfigRequest -> Aff (exception :: EXCEPTION | eff) DeleteQueryLoggingConfigResponse
deleteQueryLoggingConfig = Request.request serviceName "deleteQueryLoggingConfig" 


-- | <p>Deletes a reusable delegation set.</p> <important> <p>You can delete a reusable delegation set only if it isn't associated with any hosted zones.</p> </important> <p>To verify that the reusable delegation set is not associated with any hosted zones, submit a <a>GetReusableDelegationSet</a> request and specify the ID of the reusable delegation set that you want to delete.</p>
deleteReusableDelegationSet :: forall eff. DeleteReusableDelegationSetRequest -> Aff (exception :: EXCEPTION | eff) DeleteReusableDelegationSetResponse
deleteReusableDelegationSet = Request.request serviceName "deleteReusableDelegationSet" 


-- | <p>Deletes a traffic policy.</p>
deleteTrafficPolicy :: forall eff. DeleteTrafficPolicyRequest -> Aff (exception :: EXCEPTION | eff) DeleteTrafficPolicyResponse
deleteTrafficPolicy = Request.request serviceName "deleteTrafficPolicy" 


-- | <p>Deletes a traffic policy instance and all of the resource record sets that Amazon Route 53 created when you created the instance.</p> <note> <p>In the Amazon Route 53 console, traffic policy instances are known as policy records.</p> </note>
deleteTrafficPolicyInstance :: forall eff. DeleteTrafficPolicyInstanceRequest -> Aff (exception :: EXCEPTION | eff) DeleteTrafficPolicyInstanceResponse
deleteTrafficPolicyInstance = Request.request serviceName "deleteTrafficPolicyInstance" 


-- | <p>Removes authorization to submit an <code>AssociateVPCWithHostedZone</code> request to associate a specified VPC with a hosted zone that was created by a different account. You must use the account that created the hosted zone to submit a <code>DeleteVPCAssociationAuthorization</code> request.</p> <important> <p>Sending this request only prevents the AWS account that created the VPC from associating the VPC with the Amazon Route 53 hosted zone in the future. If the VPC is already associated with the hosted zone, <code>DeleteVPCAssociationAuthorization</code> won't disassociate the VPC from the hosted zone. If you want to delete an existing association, use <code>DisassociateVPCFromHostedZone</code>.</p> </important>
deleteVPCAssociationAuthorization :: forall eff. DeleteVPCAssociationAuthorizationRequest -> Aff (exception :: EXCEPTION | eff) DeleteVPCAssociationAuthorizationResponse
deleteVPCAssociationAuthorization = Request.request serviceName "deleteVPCAssociationAuthorization" 


-- | <p>Disassociates a VPC from a Amazon Route 53 private hosted zone. </p> <note> <p>You can't disassociate the last VPC from a private hosted zone.</p> </note> <important> <p>You can't disassociate a VPC from a private hosted zone when only one VPC is associated with the hosted zone. You also can't convert a private hosted zone into a public hosted zone.</p> </important>
disassociateVPCFromHostedZone :: forall eff. DisassociateVPCFromHostedZoneRequest -> Aff (exception :: EXCEPTION | eff) DisassociateVPCFromHostedZoneResponse
disassociateVPCFromHostedZone = Request.request serviceName "disassociateVPCFromHostedZone" 


-- | <p>Gets the specified limit for the current account, for example, the maximum number of health checks that you can create using the account.</p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>
getAccountLimit :: forall eff. GetAccountLimitRequest -> Aff (exception :: EXCEPTION | eff) GetAccountLimitResponse
getAccountLimit = Request.request serviceName "getAccountLimit" 


-- | <p>Returns the current status of a change batch request. The status is one of the following values:</p> <ul> <li> <p> <code>PENDING</code> indicates that the changes in this request have not propagated to all Amazon Route 53 DNS servers. This is the initial status of all change batch requests.</p> </li> <li> <p> <code>INSYNC</code> indicates that the changes have propagated to all Amazon Route 53 DNS servers. </p> </li> </ul>
getChange :: forall eff. GetChangeRequest -> Aff (exception :: EXCEPTION | eff) GetChangeResponse
getChange = Request.request serviceName "getChange" 


-- | <p> <code>GetCheckerIpRanges</code> still works, but we recommend that you download ip-ranges.json, which includes IP address ranges for all AWS services. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/route-53-ip-addresses.html">IP Address Ranges of Amazon Route 53 Servers</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>
getCheckerIpRanges :: forall eff. GetCheckerIpRangesRequest -> Aff (exception :: EXCEPTION | eff) GetCheckerIpRangesResponse
getCheckerIpRanges = Request.request serviceName "getCheckerIpRanges" 


-- | <p>Gets information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.</p> <p>Use the following syntax to determine whether a continent is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?ContinentCode=<i>two-letter abbreviation for a continent</i> </code> </p> <p>Use the following syntax to determine whether a country is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?CountryCode=<i>two-character country code</i> </code> </p> <p>Use the following syntax to determine whether a subdivision of a country is supported for geolocation:</p> <p> <code>GET /2013-04-01/geolocation?CountryCode=<i>two-character country code</i>&amp;SubdivisionCode=<i>subdivision code</i> </code> </p>
getGeoLocation :: forall eff. GetGeoLocationRequest -> Aff (exception :: EXCEPTION | eff) GetGeoLocationResponse
getGeoLocation = Request.request serviceName "getGeoLocation" 


-- | <p>Gets information about a specified health check.</p>
getHealthCheck :: forall eff. GetHealthCheckRequest -> Aff (exception :: EXCEPTION | eff) GetHealthCheckResponse
getHealthCheck = Request.request serviceName "getHealthCheck" 


-- | <p>Retrieves the number of health checks that are associated with the current AWS account.</p>
getHealthCheckCount :: forall eff. GetHealthCheckCountRequest -> Aff (exception :: EXCEPTION | eff) GetHealthCheckCountResponse
getHealthCheckCount = Request.request serviceName "getHealthCheckCount" 


-- | <p>Gets the reason that a specified health check failed most recently.</p>
getHealthCheckLastFailureReason :: forall eff. GetHealthCheckLastFailureReasonRequest -> Aff (exception :: EXCEPTION | eff) GetHealthCheckLastFailureReasonResponse
getHealthCheckLastFailureReason = Request.request serviceName "getHealthCheckLastFailureReason" 


-- | <p>Gets status of a specified health check. </p>
getHealthCheckStatus :: forall eff. GetHealthCheckStatusRequest -> Aff (exception :: EXCEPTION | eff) GetHealthCheckStatusResponse
getHealthCheckStatus = Request.request serviceName "getHealthCheckStatus" 


-- | <p>Gets information about a specified hosted zone including the four name servers assigned to the hosted zone.</p>
getHostedZone :: forall eff. GetHostedZoneRequest -> Aff (exception :: EXCEPTION | eff) GetHostedZoneResponse
getHostedZone = Request.request serviceName "getHostedZone" 


-- | <p>Retrieves the number of hosted zones that are associated with the current AWS account.</p>
getHostedZoneCount :: forall eff. GetHostedZoneCountRequest -> Aff (exception :: EXCEPTION | eff) GetHostedZoneCountResponse
getHostedZoneCount = Request.request serviceName "getHostedZoneCount" 


-- | <p>Gets the specified limit for a specified hosted zone, for example, the maximum number of records that you can create in the hosted zone. </p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>
getHostedZoneLimit :: forall eff. GetHostedZoneLimitRequest -> Aff (exception :: EXCEPTION | eff) GetHostedZoneLimitResponse
getHostedZoneLimit = Request.request serviceName "getHostedZoneLimit" 


-- | <p>Gets information about a specified configuration for DNS query logging.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a> and <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a>.</p>
getQueryLoggingConfig :: forall eff. GetQueryLoggingConfigRequest -> Aff (exception :: EXCEPTION | eff) GetQueryLoggingConfigResponse
getQueryLoggingConfig = Request.request serviceName "getQueryLoggingConfig" 


-- | <p>Retrieves information about a specified reusable delegation set, including the four name servers that are assigned to the delegation set.</p>
getReusableDelegationSet :: forall eff. GetReusableDelegationSetRequest -> Aff (exception :: EXCEPTION | eff) GetReusableDelegationSetResponse
getReusableDelegationSet = Request.request serviceName "getReusableDelegationSet" 


-- | <p>Gets the maximum number of hosted zones that you can associate with the specified reusable delegation set.</p> <p>For the default limit, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>. To request a higher limit, <a href="https://console.aws.amazon.com/support/home#/case/create?issueType=service-limit-increase&amp;limitType=service-code-route53">open a case</a>.</p>
getReusableDelegationSetLimit :: forall eff. GetReusableDelegationSetLimitRequest -> Aff (exception :: EXCEPTION | eff) GetReusableDelegationSetLimitResponse
getReusableDelegationSetLimit = Request.request serviceName "getReusableDelegationSetLimit" 


-- | <p>Gets information about a specific traffic policy version.</p>
getTrafficPolicy :: forall eff. GetTrafficPolicyRequest -> Aff (exception :: EXCEPTION | eff) GetTrafficPolicyResponse
getTrafficPolicy = Request.request serviceName "getTrafficPolicy" 


-- | <p>Gets information about a specified traffic policy instance.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <note> <p>In the Amazon Route 53 console, traffic policy instances are known as policy records.</p> </note>
getTrafficPolicyInstance :: forall eff. GetTrafficPolicyInstanceRequest -> Aff (exception :: EXCEPTION | eff) GetTrafficPolicyInstanceResponse
getTrafficPolicyInstance = Request.request serviceName "getTrafficPolicyInstance" 


-- | <p>Gets the number of traffic policy instances that are associated with the current AWS account.</p>
getTrafficPolicyInstanceCount :: forall eff. GetTrafficPolicyInstanceCountRequest -> Aff (exception :: EXCEPTION | eff) GetTrafficPolicyInstanceCountResponse
getTrafficPolicyInstanceCount = Request.request serviceName "getTrafficPolicyInstanceCount" 


-- | <p>Retrieves a list of supported geo locations.</p> <p>Countries are listed first, and continents are listed last. If Amazon Route 53 supports subdivisions for a country (for example, states or provinces), the subdivisions for that country are listed in alphabetical order immediately after the corresponding country.</p>
listGeoLocations :: forall eff. ListGeoLocationsRequest -> Aff (exception :: EXCEPTION | eff) ListGeoLocationsResponse
listGeoLocations = Request.request serviceName "listGeoLocations" 


-- | <p>Retrieve a list of the health checks that are associated with the current AWS account. </p>
listHealthChecks :: forall eff. ListHealthChecksRequest -> Aff (exception :: EXCEPTION | eff) ListHealthChecksResponse
listHealthChecks = Request.request serviceName "listHealthChecks" 


-- | <p>Retrieves a list of the public and private hosted zones that are associated with the current AWS account. The response includes a <code>HostedZones</code> child element for each hosted zone.</p> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of hosted zones, you can use the <code>maxitems</code> parameter to list them in groups of up to 100.</p>
listHostedZones :: forall eff. ListHostedZonesRequest -> Aff (exception :: EXCEPTION | eff) ListHostedZonesResponse
listHostedZones = Request.request serviceName "listHostedZones" 


-- | <p>Retrieves a list of your hosted zones in lexicographic order. The response includes a <code>HostedZones</code> child element for each hosted zone created by the current AWS account. </p> <p> <code>ListHostedZonesByName</code> sorts hosted zones by name with the labels reversed. For example:</p> <p> <code>com.example.www.</code> </p> <p>Note the trailing dot, which can change the sort order in some circumstances.</p> <p>If the domain name includes escape characters or Punycode, <code>ListHostedZonesByName</code> alphabetizes the domain name using the escaped or Punycoded value, which is the format that Amazon Route 53 saves in its database. For example, to create a hosted zone for exämple.com, you specify ex\344mple.com for the domain name. <code>ListHostedZonesByName</code> alphabetizes it as:</p> <p> <code>com.ex\344mple.</code> </p> <p>The labels are reversed and alphabetized using the escaped value. For more information about valid domain name formats, including internationalized domain names, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html">DNS Domain Name Format</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>Amazon Route 53 returns up to 100 items in each response. If you have a lot of hosted zones, use the <code>MaxItems</code> parameter to list them in groups of up to 100. The response includes values that help navigate from one group of <code>MaxItems</code> hosted zones to the next:</p> <ul> <li> <p>The <code>DNSName</code> and <code>HostedZoneId</code> elements in the response contain the values, if any, specified for the <code>dnsname</code> and <code>hostedzoneid</code> parameters in the request that produced the current response.</p> </li> <li> <p>The <code>MaxItems</code> element in the response contains the value, if any, that you specified for the <code>maxitems</code> parameter in the request that produced the current response.</p> </li> <li> <p>If the value of <code>IsTruncated</code> in the response is true, there are more hosted zones associated with the current AWS account. </p> <p>If <code>IsTruncated</code> is false, this response includes the last hosted zone that is associated with the current account. The <code>NextDNSName</code> element and <code>NextHostedZoneId</code> elements are omitted from the response.</p> </li> <li> <p>The <code>NextDNSName</code> and <code>NextHostedZoneId</code> elements in the response contain the domain name and the hosted zone ID of the next hosted zone that is associated with the current AWS account. If you want to list more hosted zones, make another call to <code>ListHostedZonesByName</code>, and specify the value of <code>NextDNSName</code> and <code>NextHostedZoneId</code> in the <code>dnsname</code> and <code>hostedzoneid</code> parameters, respectively.</p> </li> </ul>
listHostedZonesByName :: forall eff. ListHostedZonesByNameRequest -> Aff (exception :: EXCEPTION | eff) ListHostedZonesByNameResponse
listHostedZonesByName = Request.request serviceName "listHostedZonesByName" 


-- | <p>Lists the configurations for DNS query logging that are associated with the current AWS account or the configuration that is associated with a specified hosted zone.</p> <p>For more information about DNS query logs, see <a>CreateQueryLoggingConfig</a>. Additional information, including the format of DNS query logs, appears in <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/query-logs.html">Logging DNS Queries</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>
listQueryLoggingConfigs :: forall eff. ListQueryLoggingConfigsRequest -> Aff (exception :: EXCEPTION | eff) ListQueryLoggingConfigsResponse
listQueryLoggingConfigs = Request.request serviceName "listQueryLoggingConfigs" 


-- | <p>Lists the resource record sets in a specified hosted zone.</p> <p> <code>ListResourceRecordSets</code> returns up to 100 resource record sets at a time in ASCII order, beginning at a position specified by the <code>name</code> and <code>type</code> elements. The action sorts results first by DNS name with the labels reversed, for example:</p> <p> <code>com.example.www.</code> </p> <p>Note the trailing dot, which can change the sort order in some circumstances.</p> <p>When multiple records have the same DNS name, the action sorts results by the record type.</p> <p>You can use the name and type elements to adjust the beginning position of the list of resource record sets returned:</p> <dl> <dt>If you do not specify Name or Type</dt> <dd> <p>The results begin with the first resource record set that the hosted zone contains.</p> </dd> <dt>If you specify Name but not Type</dt> <dd> <p>The results begin with the first resource record set in the list whose name is greater than or equal to <code>Name</code>.</p> </dd> <dt>If you specify Type but not Name</dt> <dd> <p>Amazon Route 53 returns the <code>InvalidInput</code> error.</p> </dd> <dt>If you specify both Name and Type</dt> <dd> <p>The results begin with the first resource record set in the list whose name is greater than or equal to <code>Name</code>, and whose type is greater than or equal to <code>Type</code>.</p> </dd> </dl> <p>This action returns the most current version of the records. This includes records that are <code>PENDING</code>, and that are not yet available on all Amazon Route 53 DNS servers.</p> <p>To ensure that you get an accurate listing of the resource record sets for a hosted zone at a point in time, do not submit a <code>ChangeResourceRecordSets</code> request while you're paging through the results of a <code>ListResourceRecordSets</code> request. If you do, some pages may display results without the latest changes while other pages display results with the latest changes.</p>
listResourceRecordSets :: forall eff. ListResourceRecordSetsRequest -> Aff (exception :: EXCEPTION | eff) ListResourceRecordSetsResponse
listResourceRecordSets = Request.request serviceName "listResourceRecordSets" 


-- | <p>Retrieves a list of the reusable delegation sets that are associated with the current AWS account.</p>
listReusableDelegationSets :: forall eff. ListReusableDelegationSetsRequest -> Aff (exception :: EXCEPTION | eff) ListReusableDelegationSetsResponse
listReusableDelegationSets = Request.request serviceName "listReusableDelegationSets" 


-- | <p>Lists tags for one health check or hosted zone. </p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (exception :: EXCEPTION | eff) ListTagsForResourceResponse
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>Lists tags for up to 10 health checks or hosted zones.</p> <p>For information about using tags for cost allocation, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>.</p>
listTagsForResources :: forall eff. ListTagsForResourcesRequest -> Aff (exception :: EXCEPTION | eff) ListTagsForResourcesResponse
listTagsForResources = Request.request serviceName "listTagsForResources" 


-- | <p>Gets information about the latest version for every traffic policy that is associated with the current AWS account. Policies are listed in the order in which they were created. </p>
listTrafficPolicies :: forall eff. ListTrafficPoliciesRequest -> Aff (exception :: EXCEPTION | eff) ListTrafficPoliciesResponse
listTrafficPolicies = Request.request serviceName "listTrafficPolicies" 


-- | <p>Gets information about the traffic policy instances that you created by using the current AWS account.</p> <note> <p>After you submit an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>
listTrafficPolicyInstances :: forall eff. ListTrafficPolicyInstancesRequest -> Aff (exception :: EXCEPTION | eff) ListTrafficPolicyInstancesResponse
listTrafficPolicyInstances = Request.request serviceName "listTrafficPolicyInstances" 


-- | <p>Gets information about the traffic policy instances that you created in a specified hosted zone.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>
listTrafficPolicyInstancesByHostedZone :: forall eff. ListTrafficPolicyInstancesByHostedZoneRequest -> Aff (exception :: EXCEPTION | eff) ListTrafficPolicyInstancesByHostedZoneResponse
listTrafficPolicyInstancesByHostedZone = Request.request serviceName "listTrafficPolicyInstancesByHostedZone" 


-- | <p>Gets information about the traffic policy instances that you created by using a specify traffic policy version.</p> <note> <p>After you submit a <code>CreateTrafficPolicyInstance</code> or an <code>UpdateTrafficPolicyInstance</code> request, there's a brief delay while Amazon Route 53 creates the resource record sets that are specified in the traffic policy definition. For more information, see the <code>State</code> response element.</p> </note> <p>Amazon Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the <code>MaxItems</code> parameter to list them in groups of up to 100.</p>
listTrafficPolicyInstancesByPolicy :: forall eff. ListTrafficPolicyInstancesByPolicyRequest -> Aff (exception :: EXCEPTION | eff) ListTrafficPolicyInstancesByPolicyResponse
listTrafficPolicyInstancesByPolicy = Request.request serviceName "listTrafficPolicyInstancesByPolicy" 


-- | <p>Gets information about all of the versions for a specified traffic policy.</p> <p>Traffic policy versions are listed in numerical order by <code>VersionNumber</code>.</p>
listTrafficPolicyVersions :: forall eff. ListTrafficPolicyVersionsRequest -> Aff (exception :: EXCEPTION | eff) ListTrafficPolicyVersionsResponse
listTrafficPolicyVersions = Request.request serviceName "listTrafficPolicyVersions" 


-- | <p>Gets a list of the VPCs that were created by other accounts and that can be associated with a specified hosted zone because you've submitted one or more <code>CreateVPCAssociationAuthorization</code> requests. </p> <p>The response includes a <code>VPCs</code> element with a <code>VPC</code> child element for each VPC that can be associated with the hosted zone.</p>
listVPCAssociationAuthorizations :: forall eff. ListVPCAssociationAuthorizationsRequest -> Aff (exception :: EXCEPTION | eff) ListVPCAssociationAuthorizationsResponse
listVPCAssociationAuthorizations = Request.request serviceName "listVPCAssociationAuthorizations" 


-- | <p>Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask. </p>
testDNSAnswer :: forall eff. TestDNSAnswerRequest -> Aff (exception :: EXCEPTION | eff) TestDNSAnswerResponse
testDNSAnswer = Request.request serviceName "testDNSAnswer" 


-- | <p>Updates an existing health check. Note that some values can't be updated. </p> <p>For more information about updating health checks, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/health-checks-creating-deleting.html">Creating, Updating, and Deleting Health Checks</a> in the <i>Amazon Route 53 Developer Guide</i>.</p>
updateHealthCheck :: forall eff. UpdateHealthCheckRequest -> Aff (exception :: EXCEPTION | eff) UpdateHealthCheckResponse
updateHealthCheck = Request.request serviceName "updateHealthCheck" 


-- | <p>Updates the comment for a specified hosted zone.</p>
updateHostedZoneComment :: forall eff. UpdateHostedZoneCommentRequest -> Aff (exception :: EXCEPTION | eff) UpdateHostedZoneCommentResponse
updateHostedZoneComment = Request.request serviceName "updateHostedZoneComment" 


-- | <p>Updates the comment for a specified traffic policy version.</p>
updateTrafficPolicyComment :: forall eff. UpdateTrafficPolicyCommentRequest -> Aff (exception :: EXCEPTION | eff) UpdateTrafficPolicyCommentResponse
updateTrafficPolicyComment = Request.request serviceName "updateTrafficPolicyComment" 


-- | <p>Updates the resource record sets in a specified hosted zone that were created based on the settings in a specified traffic policy version.</p> <p>When you update a traffic policy instance, Amazon Route 53 continues to respond to DNS queries for the root resource record set name (such as example.com) while it replaces one group of resource record sets with another. Amazon Route 53 performs the following operations:</p> <ol> <li> <p>Amazon Route 53 creates a new group of resource record sets based on the specified traffic policy. This is true regardless of how significant the differences are between the existing resource record sets and the new resource record sets. </p> </li> <li> <p>When all of the new resource record sets have been created, Amazon Route 53 starts to respond to DNS queries for the root resource record set name (such as example.com) by using the new resource record sets.</p> </li> <li> <p>Amazon Route 53 deletes the old group of resource record sets that are associated with the root resource record set name.</p> </li> </ol>
updateTrafficPolicyInstance :: forall eff. UpdateTrafficPolicyInstanceRequest -> Aff (exception :: EXCEPTION | eff) UpdateTrafficPolicyInstanceResponse
updateTrafficPolicyInstance = Request.request serviceName "updateTrafficPolicyInstance" 


-- | <p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>
newtype AccountLimit = AccountLimit 
  { "Type" :: (AccountLimitType)
  , "Value" :: (LimitValue)
  }
derive instance newtypeAccountLimit :: Newtype AccountLimit _
derive instance repGenericAccountLimit :: Generic AccountLimit _
instance showAccountLimit :: Show AccountLimit where
  show = genericShow
instance decodeAccountLimit :: Decode AccountLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountLimit :: Encode AccountLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountLimitType = AccountLimitType String
derive instance newtypeAccountLimitType :: Newtype AccountLimitType _
derive instance repGenericAccountLimitType :: Generic AccountLimitType _
instance showAccountLimitType :: Show AccountLimitType where
  show = genericShow
instance decodeAccountLimitType :: Decode AccountLimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountLimitType :: Encode AccountLimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that identifies the CloudWatch alarm that you want Amazon Route 53 health checkers to use to determine whether this health check is healthy.</p>
newtype AlarmIdentifier = AlarmIdentifier 
  { "Region" :: (CloudWatchRegion)
  , "Name" :: (AlarmName)
  }
derive instance newtypeAlarmIdentifier :: Newtype AlarmIdentifier _
derive instance repGenericAlarmIdentifier :: Generic AlarmIdentifier _
instance showAlarmIdentifier :: Show AlarmIdentifier where
  show = genericShow
instance decodeAlarmIdentifier :: Decode AlarmIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmIdentifier :: Encode AlarmIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AlarmName = AlarmName String
derive instance newtypeAlarmName :: Newtype AlarmName _
derive instance repGenericAlarmName :: Generic AlarmName _
instance showAlarmName :: Show AlarmName where
  show = genericShow
instance decodeAlarmName :: Decode AlarmName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlarmName :: Encode AlarmName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasHealthEnabled = AliasHealthEnabled Boolean
derive instance newtypeAliasHealthEnabled :: Newtype AliasHealthEnabled _
derive instance repGenericAliasHealthEnabled :: Generic AliasHealthEnabled _
instance showAliasHealthEnabled :: Show AliasHealthEnabled where
  show = genericShow
instance decodeAliasHealthEnabled :: Decode AliasHealthEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasHealthEnabled :: Encode AliasHealthEnabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> <i>Alias resource record sets only:</i> Information about the CloudFront distribution, Elastic Beanstalk environment, ELB load balancer, Amazon S3 bucket, or Amazon Route 53 resource record set that you're redirecting queries to. An Elastic Beanstalk environment must have a regionalized subdomain.</p> <p>When creating resource record sets for a private hosted zone, note the following:</p> <ul> <li> <p>Resource record sets can't be created for CloudFront distributions in a private hosted zone.</p> </li> <li> <p>Creating geolocation alias resource record sets or latency alias resource record sets in a private hosted zone is unsupported.</p> </li> <li> <p>For information about creating failover resource record sets in a private hosted zone, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/dns-failover-private-hosted-zones.html">Configuring Failover in a Private Hosted Zone</a>.</p> </li> </ul>
newtype AliasTarget = AliasTarget 
  { "HostedZoneId" :: (ResourceId)
  , "DNSName" :: (DNSName)
  , "EvaluateTargetHealth" :: (AliasHealthEnabled)
  }
derive instance newtypeAliasTarget :: Newtype AliasTarget _
derive instance repGenericAliasTarget :: Generic AliasTarget _
instance showAliasTarget :: Show AliasTarget where
  show = genericShow
instance decodeAliasTarget :: Decode AliasTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasTarget :: Encode AliasTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateVPCComment = AssociateVPCComment String
derive instance newtypeAssociateVPCComment :: Newtype AssociateVPCComment _
derive instance repGenericAssociateVPCComment :: Generic AssociateVPCComment _
instance showAssociateVPCComment :: Show AssociateVPCComment where
  show = genericShow
instance decodeAssociateVPCComment :: Decode AssociateVPCComment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateVPCComment :: Encode AssociateVPCComment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the request to associate a VPC with a private hosted zone.</p>
newtype AssociateVPCWithHostedZoneRequest = AssociateVPCWithHostedZoneRequest 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  , "Comment" :: NullOrUndefined.NullOrUndefined (AssociateVPCComment)
  }
derive instance newtypeAssociateVPCWithHostedZoneRequest :: Newtype AssociateVPCWithHostedZoneRequest _
derive instance repGenericAssociateVPCWithHostedZoneRequest :: Generic AssociateVPCWithHostedZoneRequest _
instance showAssociateVPCWithHostedZoneRequest :: Show AssociateVPCWithHostedZoneRequest where
  show = genericShow
instance decodeAssociateVPCWithHostedZoneRequest :: Decode AssociateVPCWithHostedZoneRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateVPCWithHostedZoneRequest :: Encode AssociateVPCWithHostedZoneRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the <code>AssociateVPCWithHostedZone</code> request.</p>
newtype AssociateVPCWithHostedZoneResponse = AssociateVPCWithHostedZoneResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeAssociateVPCWithHostedZoneResponse :: Newtype AssociateVPCWithHostedZoneResponse _
derive instance repGenericAssociateVPCWithHostedZoneResponse :: Generic AssociateVPCWithHostedZoneResponse _
instance showAssociateVPCWithHostedZoneResponse :: Show AssociateVPCWithHostedZoneResponse where
  show = genericShow
instance decodeAssociateVPCWithHostedZoneResponse :: Decode AssociateVPCWithHostedZoneResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateVPCWithHostedZoneResponse :: Encode AssociateVPCWithHostedZoneResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The information for each resource record set that you want to change.</p>
newtype Change = Change 
  { "Action" :: (ChangeAction)
  , "ResourceRecordSet" :: (ResourceRecordSet)
  }
derive instance newtypeChange :: Newtype Change _
derive instance repGenericChange :: Generic Change _
instance showChange :: Show Change where
  show = genericShow
instance decodeChange :: Decode Change where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChange :: Encode Change where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeAction = ChangeAction String
derive instance newtypeChangeAction :: Newtype ChangeAction _
derive instance repGenericChangeAction :: Generic ChangeAction _
instance showChangeAction :: Show ChangeAction where
  show = genericShow
instance decodeChangeAction :: Decode ChangeAction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeAction :: Encode ChangeAction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The information for a change request.</p>
newtype ChangeBatch = ChangeBatch 
  { "Comment" :: NullOrUndefined.NullOrUndefined (ResourceDescription)
  , "Changes" :: (Changes)
  }
derive instance newtypeChangeBatch :: Newtype ChangeBatch _
derive instance repGenericChangeBatch :: Generic ChangeBatch _
instance showChangeBatch :: Show ChangeBatch where
  show = genericShow
instance decodeChangeBatch :: Decode ChangeBatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeBatch :: Encode ChangeBatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that describes change information about changes made to your hosted zone.</p>
newtype ChangeInfo = ChangeInfo 
  { "Id" :: (ResourceId)
  , "Status" :: (ChangeStatus)
  , "SubmittedAt" :: (TimeStamp)
  , "Comment" :: NullOrUndefined.NullOrUndefined (ResourceDescription)
  }
derive instance newtypeChangeInfo :: Newtype ChangeInfo _
derive instance repGenericChangeInfo :: Generic ChangeInfo _
instance showChangeInfo :: Show ChangeInfo where
  show = genericShow
instance decodeChangeInfo :: Decode ChangeInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeInfo :: Encode ChangeInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains change information for the resource record set.</p>
newtype ChangeResourceRecordSetsRequest = ChangeResourceRecordSetsRequest 
  { "HostedZoneId" :: (ResourceId)
  , "ChangeBatch" :: (ChangeBatch)
  }
derive instance newtypeChangeResourceRecordSetsRequest :: Newtype ChangeResourceRecordSetsRequest _
derive instance repGenericChangeResourceRecordSetsRequest :: Generic ChangeResourceRecordSetsRequest _
instance showChangeResourceRecordSetsRequest :: Show ChangeResourceRecordSetsRequest where
  show = genericShow
instance decodeChangeResourceRecordSetsRequest :: Decode ChangeResourceRecordSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeResourceRecordSetsRequest :: Encode ChangeResourceRecordSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type containing the response for the request.</p>
newtype ChangeResourceRecordSetsResponse = ChangeResourceRecordSetsResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeChangeResourceRecordSetsResponse :: Newtype ChangeResourceRecordSetsResponse _
derive instance repGenericChangeResourceRecordSetsResponse :: Generic ChangeResourceRecordSetsResponse _
instance showChangeResourceRecordSetsResponse :: Show ChangeResourceRecordSetsResponse where
  show = genericShow
instance decodeChangeResourceRecordSetsResponse :: Decode ChangeResourceRecordSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeResourceRecordSetsResponse :: Encode ChangeResourceRecordSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChangeStatus = ChangeStatus String
derive instance newtypeChangeStatus :: Newtype ChangeStatus _
derive instance repGenericChangeStatus :: Generic ChangeStatus _
instance showChangeStatus :: Show ChangeStatus where
  show = genericShow
instance decodeChangeStatus :: Decode ChangeStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeStatus :: Encode ChangeStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the tags that you want to add, edit, or delete.</p>
newtype ChangeTagsForResourceRequest = ChangeTagsForResourceRequest 
  { "ResourceType" :: (TagResourceType)
  , "ResourceId" :: (TagResourceId)
  , "AddTags" :: NullOrUndefined.NullOrUndefined (TagList)
  , "RemoveTagKeys" :: NullOrUndefined.NullOrUndefined (TagKeyList)
  }
derive instance newtypeChangeTagsForResourceRequest :: Newtype ChangeTagsForResourceRequest _
derive instance repGenericChangeTagsForResourceRequest :: Generic ChangeTagsForResourceRequest _
instance showChangeTagsForResourceRequest :: Show ChangeTagsForResourceRequest where
  show = genericShow
instance decodeChangeTagsForResourceRequest :: Decode ChangeTagsForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeTagsForResourceRequest :: Encode ChangeTagsForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Empty response for the request.</p>
newtype ChangeTagsForResourceResponse = ChangeTagsForResourceResponse Types.NoArguments
derive instance newtypeChangeTagsForResourceResponse :: Newtype ChangeTagsForResourceResponse _
derive instance repGenericChangeTagsForResourceResponse :: Generic ChangeTagsForResourceResponse _
instance showChangeTagsForResourceResponse :: Show ChangeTagsForResourceResponse where
  show = genericShow
instance decodeChangeTagsForResourceResponse :: Decode ChangeTagsForResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChangeTagsForResourceResponse :: Encode ChangeTagsForResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Changes = Changes (Array Change)
derive instance newtypeChanges :: Newtype Changes _
derive instance repGenericChanges :: Generic Changes _
instance showChanges :: Show Changes where
  show = genericShow
instance decodeChanges :: Decode Changes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChanges :: Encode Changes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CheckerIpRanges = CheckerIpRanges (Array IPAddressCidr)
derive instance newtypeCheckerIpRanges :: Newtype CheckerIpRanges _
derive instance repGenericCheckerIpRanges :: Generic CheckerIpRanges _
instance showCheckerIpRanges :: Show CheckerIpRanges where
  show = genericShow
instance decodeCheckerIpRanges :: Decode CheckerIpRanges where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCheckerIpRanges :: Encode CheckerIpRanges where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ChildHealthCheckList = ChildHealthCheckList (Array HealthCheckId)
derive instance newtypeChildHealthCheckList :: Newtype ChildHealthCheckList _
derive instance repGenericChildHealthCheckList :: Generic ChildHealthCheckList _
instance showChildHealthCheckList :: Show ChildHealthCheckList where
  show = genericShow
instance decodeChildHealthCheckList :: Decode ChildHealthCheckList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeChildHealthCheckList :: Encode ChildHealthCheckList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the CloudWatch alarm that Amazon Route 53 is monitoring for this health check.</p>
newtype CloudWatchAlarmConfiguration = CloudWatchAlarmConfiguration 
  { "EvaluationPeriods" :: (EvaluationPeriods)
  , "Threshold" :: (Threshold)
  , "ComparisonOperator" :: (ComparisonOperator)
  , "Period" :: (Period)
  , "MetricName" :: (MetricName)
  , "Namespace" :: (Namespace)
  , "Statistic" :: (Statistic)
  , "Dimensions" :: NullOrUndefined.NullOrUndefined (DimensionList)
  }
derive instance newtypeCloudWatchAlarmConfiguration :: Newtype CloudWatchAlarmConfiguration _
derive instance repGenericCloudWatchAlarmConfiguration :: Generic CloudWatchAlarmConfiguration _
instance showCloudWatchAlarmConfiguration :: Show CloudWatchAlarmConfiguration where
  show = genericShow
instance decodeCloudWatchAlarmConfiguration :: Decode CloudWatchAlarmConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchAlarmConfiguration :: Encode CloudWatchAlarmConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudWatchLogsLogGroupArn = CloudWatchLogsLogGroupArn String
derive instance newtypeCloudWatchLogsLogGroupArn :: Newtype CloudWatchLogsLogGroupArn _
derive instance repGenericCloudWatchLogsLogGroupArn :: Generic CloudWatchLogsLogGroupArn _
instance showCloudWatchLogsLogGroupArn :: Show CloudWatchLogsLogGroupArn where
  show = genericShow
instance decodeCloudWatchLogsLogGroupArn :: Decode CloudWatchLogsLogGroupArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLogsLogGroupArn :: Encode CloudWatchLogsLogGroupArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudWatchRegion = CloudWatchRegion String
derive instance newtypeCloudWatchRegion :: Newtype CloudWatchRegion _
derive instance repGenericCloudWatchRegion :: Generic CloudWatchRegion _
instance showCloudWatchRegion :: Show CloudWatchRegion where
  show = genericShow
instance decodeCloudWatchRegion :: Decode CloudWatchRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchRegion :: Encode CloudWatchRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _
derive instance repGenericComparisonOperator :: Generic ComparisonOperator _
instance showComparisonOperator :: Show ComparisonOperator where
  show = genericShow
instance decodeComparisonOperator :: Decode ComparisonOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComparisonOperator :: Encode ComparisonOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Another user submitted a request to create, update, or delete the object at the same time that you did. Retry the request. </p>
newtype ConcurrentModification = ConcurrentModification 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModification :: Newtype ConcurrentModification _
derive instance repGenericConcurrentModification :: Generic ConcurrentModification _
instance showConcurrentModification :: Show ConcurrentModification where
  show = genericShow
instance decodeConcurrentModification :: Decode ConcurrentModification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModification :: Encode ConcurrentModification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The cause of this error depends on whether you're trying to create a public or a private hosted zone:</p> <ul> <li> <p> <b>Public hosted zone:</b> Two hosted zones that have the same name or that have a parent/child relationship (example.com and test.example.com) can't have any common name servers. You tried to create a hosted zone that has the same name as an existing hosted zone or that's the parent or child of an existing hosted zone, and you specified a delegation set that shares one or more name servers with the existing hosted zone. For more information, see <a>CreateReusableDelegationSet</a>.</p> </li> <li> <p> <b>Private hosted zone:</b> You specified an Amazon VPC that you're already using for another hosted zone, and the domain that you specified for one of the hosted zones is a subdomain of the domain that you specified for the other hosted zone. For example, you can't use the same Amazon VPC for the hosted zones for example.com and test.example.com.</p> </li> </ul>
newtype ConflictingDomainExists = ConflictingDomainExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConflictingDomainExists :: Newtype ConflictingDomainExists _
derive instance repGenericConflictingDomainExists :: Generic ConflictingDomainExists _
instance showConflictingDomainExists :: Show ConflictingDomainExists where
  show = genericShow
instance decodeConflictingDomainExists :: Decode ConflictingDomainExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictingDomainExists :: Encode ConflictingDomainExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You tried to update a traffic policy instance by using a traffic policy version that has a different DNS type than the current type for the instance. You specified the type in the JSON document in the <code>CreateTrafficPolicy</code> or <code>CreateTrafficPolicyVersion</code>request. </p>
newtype ConflictingTypes = ConflictingTypes 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConflictingTypes :: Newtype ConflictingTypes _
derive instance repGenericConflictingTypes :: Generic ConflictingTypes _
instance showConflictingTypes :: Show ConflictingTypes where
  show = genericShow
instance decodeConflictingTypes :: Decode ConflictingTypes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConflictingTypes :: Encode ConflictingTypes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the health check request information.</p>
newtype CreateHealthCheckRequest = CreateHealthCheckRequest 
  { "CallerReference" :: (HealthCheckNonce)
  , "HealthCheckConfig" :: (HealthCheckConfig)
  }
derive instance newtypeCreateHealthCheckRequest :: Newtype CreateHealthCheckRequest _
derive instance repGenericCreateHealthCheckRequest :: Generic CreateHealthCheckRequest _
instance showCreateHealthCheckRequest :: Show CreateHealthCheckRequest where
  show = genericShow
instance decodeCreateHealthCheckRequest :: Decode CreateHealthCheckRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHealthCheckRequest :: Encode CreateHealthCheckRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type containing the response information for the new health check.</p>
newtype CreateHealthCheckResponse = CreateHealthCheckResponse 
  { "HealthCheck" :: (HealthCheck)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateHealthCheckResponse :: Newtype CreateHealthCheckResponse _
derive instance repGenericCreateHealthCheckResponse :: Generic CreateHealthCheckResponse _
instance showCreateHealthCheckResponse :: Show CreateHealthCheckResponse where
  show = genericShow
instance decodeCreateHealthCheckResponse :: Decode CreateHealthCheckResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHealthCheckResponse :: Encode CreateHealthCheckResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the request to create a hosted zone.</p>
newtype CreateHostedZoneRequest = CreateHostedZoneRequest 
  { "Name" :: (DNSName)
  , "VPC" :: NullOrUndefined.NullOrUndefined (VPC)
  , "CallerReference" :: (Nonce)
  , "HostedZoneConfig" :: NullOrUndefined.NullOrUndefined (HostedZoneConfig)
  , "DelegationSetId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeCreateHostedZoneRequest :: Newtype CreateHostedZoneRequest _
derive instance repGenericCreateHostedZoneRequest :: Generic CreateHostedZoneRequest _
instance showCreateHostedZoneRequest :: Show CreateHostedZoneRequest where
  show = genericShow
instance decodeCreateHostedZoneRequest :: Decode CreateHostedZoneRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHostedZoneRequest :: Encode CreateHostedZoneRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type containing the response information for the hosted zone.</p>
newtype CreateHostedZoneResponse = CreateHostedZoneResponse 
  { "HostedZone" :: (HostedZone)
  , "ChangeInfo" :: (ChangeInfo)
  , "DelegationSet" :: (DelegationSet)
  , "VPC" :: NullOrUndefined.NullOrUndefined (VPC)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateHostedZoneResponse :: Newtype CreateHostedZoneResponse _
derive instance repGenericCreateHostedZoneResponse :: Generic CreateHostedZoneResponse _
instance showCreateHostedZoneResponse :: Show CreateHostedZoneResponse where
  show = genericShow
instance decodeCreateHostedZoneResponse :: Decode CreateHostedZoneResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateHostedZoneResponse :: Encode CreateHostedZoneResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateQueryLoggingConfigRequest = CreateQueryLoggingConfigRequest 
  { "HostedZoneId" :: (ResourceId)
  , "CloudWatchLogsLogGroupArn" :: (CloudWatchLogsLogGroupArn)
  }
derive instance newtypeCreateQueryLoggingConfigRequest :: Newtype CreateQueryLoggingConfigRequest _
derive instance repGenericCreateQueryLoggingConfigRequest :: Generic CreateQueryLoggingConfigRequest _
instance showCreateQueryLoggingConfigRequest :: Show CreateQueryLoggingConfigRequest where
  show = genericShow
instance decodeCreateQueryLoggingConfigRequest :: Decode CreateQueryLoggingConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateQueryLoggingConfigRequest :: Encode CreateQueryLoggingConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateQueryLoggingConfigResponse = CreateQueryLoggingConfigResponse 
  { "QueryLoggingConfig" :: (QueryLoggingConfig)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateQueryLoggingConfigResponse :: Newtype CreateQueryLoggingConfigResponse _
derive instance repGenericCreateQueryLoggingConfigResponse :: Generic CreateQueryLoggingConfigResponse _
instance showCreateQueryLoggingConfigResponse :: Show CreateQueryLoggingConfigResponse where
  show = genericShow
instance decodeCreateQueryLoggingConfigResponse :: Decode CreateQueryLoggingConfigResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateQueryLoggingConfigResponse :: Encode CreateQueryLoggingConfigResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateReusableDelegationSetRequest = CreateReusableDelegationSetRequest 
  { "CallerReference" :: (Nonce)
  , "HostedZoneId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeCreateReusableDelegationSetRequest :: Newtype CreateReusableDelegationSetRequest _
derive instance repGenericCreateReusableDelegationSetRequest :: Generic CreateReusableDelegationSetRequest _
instance showCreateReusableDelegationSetRequest :: Show CreateReusableDelegationSetRequest where
  show = genericShow
instance decodeCreateReusableDelegationSetRequest :: Decode CreateReusableDelegationSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReusableDelegationSetRequest :: Encode CreateReusableDelegationSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateReusableDelegationSetResponse = CreateReusableDelegationSetResponse 
  { "DelegationSet" :: (DelegationSet)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateReusableDelegationSetResponse :: Newtype CreateReusableDelegationSetResponse _
derive instance repGenericCreateReusableDelegationSetResponse :: Generic CreateReusableDelegationSetResponse _
instance showCreateReusableDelegationSetResponse :: Show CreateReusableDelegationSetResponse where
  show = genericShow
instance decodeCreateReusableDelegationSetResponse :: Decode CreateReusableDelegationSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateReusableDelegationSetResponse :: Encode CreateReusableDelegationSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the resource record sets that you want to create based on a specified traffic policy.</p>
newtype CreateTrafficPolicyInstanceRequest = CreateTrafficPolicyInstanceRequest 
  { "HostedZoneId" :: (ResourceId)
  , "Name" :: (DNSName)
  , "TTL" :: (TTL)
  , "TrafficPolicyId" :: (TrafficPolicyId)
  , "TrafficPolicyVersion" :: (TrafficPolicyVersion)
  }
derive instance newtypeCreateTrafficPolicyInstanceRequest :: Newtype CreateTrafficPolicyInstanceRequest _
derive instance repGenericCreateTrafficPolicyInstanceRequest :: Generic CreateTrafficPolicyInstanceRequest _
instance showCreateTrafficPolicyInstanceRequest :: Show CreateTrafficPolicyInstanceRequest where
  show = genericShow
instance decodeCreateTrafficPolicyInstanceRequest :: Decode CreateTrafficPolicyInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrafficPolicyInstanceRequest :: Encode CreateTrafficPolicyInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the <code>CreateTrafficPolicyInstance</code> request.</p>
newtype CreateTrafficPolicyInstanceResponse = CreateTrafficPolicyInstanceResponse 
  { "TrafficPolicyInstance" :: (TrafficPolicyInstance)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateTrafficPolicyInstanceResponse :: Newtype CreateTrafficPolicyInstanceResponse _
derive instance repGenericCreateTrafficPolicyInstanceResponse :: Generic CreateTrafficPolicyInstanceResponse _
instance showCreateTrafficPolicyInstanceResponse :: Show CreateTrafficPolicyInstanceResponse where
  show = genericShow
instance decodeCreateTrafficPolicyInstanceResponse :: Decode CreateTrafficPolicyInstanceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrafficPolicyInstanceResponse :: Encode CreateTrafficPolicyInstanceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the traffic policy that you want to create.</p>
newtype CreateTrafficPolicyRequest = CreateTrafficPolicyRequest 
  { "Name" :: (TrafficPolicyName)
  , "Document" :: (TrafficPolicyDocument)
  , "Comment" :: NullOrUndefined.NullOrUndefined (TrafficPolicyComment)
  }
derive instance newtypeCreateTrafficPolicyRequest :: Newtype CreateTrafficPolicyRequest _
derive instance repGenericCreateTrafficPolicyRequest :: Generic CreateTrafficPolicyRequest _
instance showCreateTrafficPolicyRequest :: Show CreateTrafficPolicyRequest where
  show = genericShow
instance decodeCreateTrafficPolicyRequest :: Decode CreateTrafficPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrafficPolicyRequest :: Encode CreateTrafficPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the <code>CreateTrafficPolicy</code> request.</p>
newtype CreateTrafficPolicyResponse = CreateTrafficPolicyResponse 
  { "TrafficPolicy" :: (TrafficPolicy)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateTrafficPolicyResponse :: Newtype CreateTrafficPolicyResponse _
derive instance repGenericCreateTrafficPolicyResponse :: Generic CreateTrafficPolicyResponse _
instance showCreateTrafficPolicyResponse :: Show CreateTrafficPolicyResponse where
  show = genericShow
instance decodeCreateTrafficPolicyResponse :: Decode CreateTrafficPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrafficPolicyResponse :: Encode CreateTrafficPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the traffic policy that you want to create a new version for.</p>
newtype CreateTrafficPolicyVersionRequest = CreateTrafficPolicyVersionRequest 
  { "Id" :: (TrafficPolicyId)
  , "Document" :: (TrafficPolicyDocument)
  , "Comment" :: NullOrUndefined.NullOrUndefined (TrafficPolicyComment)
  }
derive instance newtypeCreateTrafficPolicyVersionRequest :: Newtype CreateTrafficPolicyVersionRequest _
derive instance repGenericCreateTrafficPolicyVersionRequest :: Generic CreateTrafficPolicyVersionRequest _
instance showCreateTrafficPolicyVersionRequest :: Show CreateTrafficPolicyVersionRequest where
  show = genericShow
instance decodeCreateTrafficPolicyVersionRequest :: Decode CreateTrafficPolicyVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrafficPolicyVersionRequest :: Encode CreateTrafficPolicyVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the <code>CreateTrafficPolicyVersion</code> request.</p>
newtype CreateTrafficPolicyVersionResponse = CreateTrafficPolicyVersionResponse 
  { "TrafficPolicy" :: (TrafficPolicy)
  , "Location" :: (ResourceURI)
  }
derive instance newtypeCreateTrafficPolicyVersionResponse :: Newtype CreateTrafficPolicyVersionResponse _
derive instance repGenericCreateTrafficPolicyVersionResponse :: Generic CreateTrafficPolicyVersionResponse _
instance showCreateTrafficPolicyVersionResponse :: Show CreateTrafficPolicyVersionResponse where
  show = genericShow
instance decodeCreateTrafficPolicyVersionResponse :: Decode CreateTrafficPolicyVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrafficPolicyVersionResponse :: Encode CreateTrafficPolicyVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the request to authorize associating a VPC with your private hosted zone. Authorization is only required when a private hosted zone and a VPC were created by using different accounts.</p>
newtype CreateVPCAssociationAuthorizationRequest = CreateVPCAssociationAuthorizationRequest 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  }
derive instance newtypeCreateVPCAssociationAuthorizationRequest :: Newtype CreateVPCAssociationAuthorizationRequest _
derive instance repGenericCreateVPCAssociationAuthorizationRequest :: Generic CreateVPCAssociationAuthorizationRequest _
instance showCreateVPCAssociationAuthorizationRequest :: Show CreateVPCAssociationAuthorizationRequest where
  show = genericShow
instance decodeCreateVPCAssociationAuthorizationRequest :: Decode CreateVPCAssociationAuthorizationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVPCAssociationAuthorizationRequest :: Encode CreateVPCAssociationAuthorizationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information from a <code>CreateVPCAssociationAuthorization</code> request.</p>
newtype CreateVPCAssociationAuthorizationResponse = CreateVPCAssociationAuthorizationResponse 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  }
derive instance newtypeCreateVPCAssociationAuthorizationResponse :: Newtype CreateVPCAssociationAuthorizationResponse _
derive instance repGenericCreateVPCAssociationAuthorizationResponse :: Generic CreateVPCAssociationAuthorizationResponse _
instance showCreateVPCAssociationAuthorizationResponse :: Show CreateVPCAssociationAuthorizationResponse where
  show = genericShow
instance decodeCreateVPCAssociationAuthorizationResponse :: Decode CreateVPCAssociationAuthorizationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateVPCAssociationAuthorizationResponse :: Encode CreateVPCAssociationAuthorizationResponse where
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


newtype DNSRCode = DNSRCode String
derive instance newtypeDNSRCode :: Newtype DNSRCode _
derive instance repGenericDNSRCode :: Generic DNSRCode _
instance showDNSRCode :: Show DNSRCode where
  show = genericShow
instance decodeDNSRCode :: Decode DNSRCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDNSRCode :: Encode DNSRCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that lists the name servers in a delegation set, as well as the <code>CallerReference</code> and the <code>ID</code> for the delegation set.</p>
newtype DelegationSet = DelegationSet 
  { "Id" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "CallerReference" :: NullOrUndefined.NullOrUndefined (Nonce)
  , "NameServers" :: (DelegationSetNameServers)
  }
derive instance newtypeDelegationSet :: Newtype DelegationSet _
derive instance repGenericDelegationSet :: Generic DelegationSet _
instance showDelegationSet :: Show DelegationSet where
  show = genericShow
instance decodeDelegationSet :: Decode DelegationSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegationSet :: Encode DelegationSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A delegation set with the same owner and caller reference combination has already been created.</p>
newtype DelegationSetAlreadyCreated = DelegationSetAlreadyCreated 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetAlreadyCreated :: Newtype DelegationSetAlreadyCreated _
derive instance repGenericDelegationSetAlreadyCreated :: Generic DelegationSetAlreadyCreated _
instance showDelegationSetAlreadyCreated :: Show DelegationSetAlreadyCreated where
  show = genericShow
instance decodeDelegationSetAlreadyCreated :: Decode DelegationSetAlreadyCreated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegationSetAlreadyCreated :: Encode DelegationSetAlreadyCreated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified delegation set has already been marked as reusable.</p>
newtype DelegationSetAlreadyReusable = DelegationSetAlreadyReusable 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetAlreadyReusable :: Newtype DelegationSetAlreadyReusable _
derive instance repGenericDelegationSetAlreadyReusable :: Generic DelegationSetAlreadyReusable _
instance showDelegationSetAlreadyReusable :: Show DelegationSetAlreadyReusable where
  show = genericShow
instance decodeDelegationSetAlreadyReusable :: Decode DelegationSetAlreadyReusable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegationSetAlreadyReusable :: Encode DelegationSetAlreadyReusable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified delegation contains associated hosted zones which must be deleted before the reusable delegation set can be deleted.</p>
newtype DelegationSetInUse = DelegationSetInUse 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetInUse :: Newtype DelegationSetInUse _
derive instance repGenericDelegationSetInUse :: Generic DelegationSetInUse _
instance showDelegationSetInUse :: Show DelegationSetInUse where
  show = genericShow
instance decodeDelegationSetInUse :: Decode DelegationSetInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegationSetInUse :: Encode DelegationSetInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DelegationSetNameServers = DelegationSetNameServers (Array DNSName)
derive instance newtypeDelegationSetNameServers :: Newtype DelegationSetNameServers _
derive instance repGenericDelegationSetNameServers :: Generic DelegationSetNameServers _
instance showDelegationSetNameServers :: Show DelegationSetNameServers where
  show = genericShow
instance decodeDelegationSetNameServers :: Decode DelegationSetNameServers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegationSetNameServers :: Encode DelegationSetNameServers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can create a hosted zone that has the same name as an existing hosted zone (example.com is common), but there is a limit to the number of hosted zones that have the same name. If you get this error, Amazon Route 53 has reached that limit. If you own the domain name and Amazon Route 53 generates this error, contact Customer Support.</p>
newtype DelegationSetNotAvailable = DelegationSetNotAvailable 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetNotAvailable :: Newtype DelegationSetNotAvailable _
derive instance repGenericDelegationSetNotAvailable :: Generic DelegationSetNotAvailable _
instance showDelegationSetNotAvailable :: Show DelegationSetNotAvailable where
  show = genericShow
instance decodeDelegationSetNotAvailable :: Decode DelegationSetNotAvailable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegationSetNotAvailable :: Encode DelegationSetNotAvailable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A reusable delegation set with the specified ID does not exist.</p>
newtype DelegationSetNotReusable = DelegationSetNotReusable 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDelegationSetNotReusable :: Newtype DelegationSetNotReusable _
derive instance repGenericDelegationSetNotReusable :: Generic DelegationSetNotReusable _
instance showDelegationSetNotReusable :: Show DelegationSetNotReusable where
  show = genericShow
instance decodeDelegationSetNotReusable :: Decode DelegationSetNotReusable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegationSetNotReusable :: Encode DelegationSetNotReusable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DelegationSets = DelegationSets (Array DelegationSet)
derive instance newtypeDelegationSets :: Newtype DelegationSets _
derive instance repGenericDelegationSets :: Generic DelegationSets _
instance showDelegationSets :: Show DelegationSets where
  show = genericShow
instance decodeDelegationSets :: Decode DelegationSets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDelegationSets :: Encode DelegationSets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This action deletes a health check.</p>
newtype DeleteHealthCheckRequest = DeleteHealthCheckRequest 
  { "HealthCheckId" :: (HealthCheckId)
  }
derive instance newtypeDeleteHealthCheckRequest :: Newtype DeleteHealthCheckRequest _
derive instance repGenericDeleteHealthCheckRequest :: Generic DeleteHealthCheckRequest _
instance showDeleteHealthCheckRequest :: Show DeleteHealthCheckRequest where
  show = genericShow
instance decodeDeleteHealthCheckRequest :: Decode DeleteHealthCheckRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHealthCheckRequest :: Encode DeleteHealthCheckRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element.</p>
newtype DeleteHealthCheckResponse = DeleteHealthCheckResponse Types.NoArguments
derive instance newtypeDeleteHealthCheckResponse :: Newtype DeleteHealthCheckResponse _
derive instance repGenericDeleteHealthCheckResponse :: Generic DeleteHealthCheckResponse _
instance showDeleteHealthCheckResponse :: Show DeleteHealthCheckResponse where
  show = genericShow
instance decodeDeleteHealthCheckResponse :: Decode DeleteHealthCheckResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHealthCheckResponse :: Encode DeleteHealthCheckResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete a hosted zone.</p>
newtype DeleteHostedZoneRequest = DeleteHostedZoneRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeDeleteHostedZoneRequest :: Newtype DeleteHostedZoneRequest _
derive instance repGenericDeleteHostedZoneRequest :: Generic DeleteHostedZoneRequest _
instance showDeleteHostedZoneRequest :: Show DeleteHostedZoneRequest where
  show = genericShow
instance decodeDeleteHostedZoneRequest :: Decode DeleteHostedZoneRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHostedZoneRequest :: Encode DeleteHostedZoneRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to a <code>DeleteHostedZone</code> request.</p>
newtype DeleteHostedZoneResponse = DeleteHostedZoneResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeDeleteHostedZoneResponse :: Newtype DeleteHostedZoneResponse _
derive instance repGenericDeleteHostedZoneResponse :: Generic DeleteHostedZoneResponse _
instance showDeleteHostedZoneResponse :: Show DeleteHostedZoneResponse where
  show = genericShow
instance decodeDeleteHostedZoneResponse :: Decode DeleteHostedZoneResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteHostedZoneResponse :: Encode DeleteHostedZoneResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteQueryLoggingConfigRequest = DeleteQueryLoggingConfigRequest 
  { "Id" :: (QueryLoggingConfigId)
  }
derive instance newtypeDeleteQueryLoggingConfigRequest :: Newtype DeleteQueryLoggingConfigRequest _
derive instance repGenericDeleteQueryLoggingConfigRequest :: Generic DeleteQueryLoggingConfigRequest _
instance showDeleteQueryLoggingConfigRequest :: Show DeleteQueryLoggingConfigRequest where
  show = genericShow
instance decodeDeleteQueryLoggingConfigRequest :: Decode DeleteQueryLoggingConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteQueryLoggingConfigRequest :: Encode DeleteQueryLoggingConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteQueryLoggingConfigResponse = DeleteQueryLoggingConfigResponse Types.NoArguments
derive instance newtypeDeleteQueryLoggingConfigResponse :: Newtype DeleteQueryLoggingConfigResponse _
derive instance repGenericDeleteQueryLoggingConfigResponse :: Generic DeleteQueryLoggingConfigResponse _
instance showDeleteQueryLoggingConfigResponse :: Show DeleteQueryLoggingConfigResponse where
  show = genericShow
instance decodeDeleteQueryLoggingConfigResponse :: Decode DeleteQueryLoggingConfigResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteQueryLoggingConfigResponse :: Encode DeleteQueryLoggingConfigResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete a reusable delegation set.</p>
newtype DeleteReusableDelegationSetRequest = DeleteReusableDelegationSetRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeDeleteReusableDelegationSetRequest :: Newtype DeleteReusableDelegationSetRequest _
derive instance repGenericDeleteReusableDelegationSetRequest :: Generic DeleteReusableDelegationSetRequest _
instance showDeleteReusableDelegationSetRequest :: Show DeleteReusableDelegationSetRequest where
  show = genericShow
instance decodeDeleteReusableDelegationSetRequest :: Decode DeleteReusableDelegationSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReusableDelegationSetRequest :: Encode DeleteReusableDelegationSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element.</p>
newtype DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse Types.NoArguments
derive instance newtypeDeleteReusableDelegationSetResponse :: Newtype DeleteReusableDelegationSetResponse _
derive instance repGenericDeleteReusableDelegationSetResponse :: Generic DeleteReusableDelegationSetResponse _
instance showDeleteReusableDelegationSetResponse :: Show DeleteReusableDelegationSetResponse where
  show = genericShow
instance decodeDeleteReusableDelegationSetResponse :: Decode DeleteReusableDelegationSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteReusableDelegationSetResponse :: Encode DeleteReusableDelegationSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete a specified traffic policy instance.</p>
newtype DeleteTrafficPolicyInstanceRequest = DeleteTrafficPolicyInstanceRequest 
  { "Id" :: (TrafficPolicyInstanceId)
  }
derive instance newtypeDeleteTrafficPolicyInstanceRequest :: Newtype DeleteTrafficPolicyInstanceRequest _
derive instance repGenericDeleteTrafficPolicyInstanceRequest :: Generic DeleteTrafficPolicyInstanceRequest _
instance showDeleteTrafficPolicyInstanceRequest :: Show DeleteTrafficPolicyInstanceRequest where
  show = genericShow
instance decodeDeleteTrafficPolicyInstanceRequest :: Decode DeleteTrafficPolicyInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTrafficPolicyInstanceRequest :: Encode DeleteTrafficPolicyInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element.</p>
newtype DeleteTrafficPolicyInstanceResponse = DeleteTrafficPolicyInstanceResponse Types.NoArguments
derive instance newtypeDeleteTrafficPolicyInstanceResponse :: Newtype DeleteTrafficPolicyInstanceResponse _
derive instance repGenericDeleteTrafficPolicyInstanceResponse :: Generic DeleteTrafficPolicyInstanceResponse _
instance showDeleteTrafficPolicyInstanceResponse :: Show DeleteTrafficPolicyInstanceResponse where
  show = genericShow
instance decodeDeleteTrafficPolicyInstanceResponse :: Decode DeleteTrafficPolicyInstanceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTrafficPolicyInstanceResponse :: Encode DeleteTrafficPolicyInstanceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to delete a specified traffic policy version.</p>
newtype DeleteTrafficPolicyRequest = DeleteTrafficPolicyRequest 
  { "Id" :: (TrafficPolicyId)
  , "Version" :: (TrafficPolicyVersion)
  }
derive instance newtypeDeleteTrafficPolicyRequest :: Newtype DeleteTrafficPolicyRequest _
derive instance repGenericDeleteTrafficPolicyRequest :: Generic DeleteTrafficPolicyRequest _
instance showDeleteTrafficPolicyRequest :: Show DeleteTrafficPolicyRequest where
  show = genericShow
instance decodeDeleteTrafficPolicyRequest :: Decode DeleteTrafficPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTrafficPolicyRequest :: Encode DeleteTrafficPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An empty element.</p>
newtype DeleteTrafficPolicyResponse = DeleteTrafficPolicyResponse Types.NoArguments
derive instance newtypeDeleteTrafficPolicyResponse :: Newtype DeleteTrafficPolicyResponse _
derive instance repGenericDeleteTrafficPolicyResponse :: Generic DeleteTrafficPolicyResponse _
instance showDeleteTrafficPolicyResponse :: Show DeleteTrafficPolicyResponse where
  show = genericShow
instance decodeDeleteTrafficPolicyResponse :: Decode DeleteTrafficPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTrafficPolicyResponse :: Encode DeleteTrafficPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the request to remove authorization to associate a VPC that was created by one AWS account with a hosted zone that was created with a different AWS account. </p>
newtype DeleteVPCAssociationAuthorizationRequest = DeleteVPCAssociationAuthorizationRequest 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  }
derive instance newtypeDeleteVPCAssociationAuthorizationRequest :: Newtype DeleteVPCAssociationAuthorizationRequest _
derive instance repGenericDeleteVPCAssociationAuthorizationRequest :: Generic DeleteVPCAssociationAuthorizationRequest _
instance showDeleteVPCAssociationAuthorizationRequest :: Show DeleteVPCAssociationAuthorizationRequest where
  show = genericShow
instance decodeDeleteVPCAssociationAuthorizationRequest :: Decode DeleteVPCAssociationAuthorizationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVPCAssociationAuthorizationRequest :: Encode DeleteVPCAssociationAuthorizationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Empty response for the request.</p>
newtype DeleteVPCAssociationAuthorizationResponse = DeleteVPCAssociationAuthorizationResponse Types.NoArguments
derive instance newtypeDeleteVPCAssociationAuthorizationResponse :: Newtype DeleteVPCAssociationAuthorizationResponse _
derive instance repGenericDeleteVPCAssociationAuthorizationResponse :: Generic DeleteVPCAssociationAuthorizationResponse _
instance showDeleteVPCAssociationAuthorizationResponse :: Show DeleteVPCAssociationAuthorizationResponse where
  show = genericShow
instance decodeDeleteVPCAssociationAuthorizationResponse :: Decode DeleteVPCAssociationAuthorizationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteVPCAssociationAuthorizationResponse :: Encode DeleteVPCAssociationAuthorizationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>For the metric that the CloudWatch alarm is associated with, a complex type that contains information about one dimension.</p>
newtype Dimension = Dimension 
  { "Name" :: (DimensionField)
  , "Value" :: (DimensionField)
  }
derive instance newtypeDimension :: Newtype Dimension _
derive instance repGenericDimension :: Generic Dimension _
instance showDimension :: Show Dimension where
  show = genericShow
instance decodeDimension :: Decode Dimension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimension :: Encode Dimension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionField = DimensionField String
derive instance newtypeDimensionField :: Newtype DimensionField _
derive instance repGenericDimensionField :: Generic DimensionField _
instance showDimensionField :: Show DimensionField where
  show = genericShow
instance decodeDimensionField :: Decode DimensionField where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionField :: Encode DimensionField where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DimensionList = DimensionList (Array Dimension)
derive instance newtypeDimensionList :: Newtype DimensionList _
derive instance repGenericDimensionList :: Generic DimensionList _
instance showDimensionList :: Show DimensionList where
  show = genericShow
instance decodeDimensionList :: Decode DimensionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDimensionList :: Encode DimensionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateVPCComment = DisassociateVPCComment String
derive instance newtypeDisassociateVPCComment :: Newtype DisassociateVPCComment _
derive instance repGenericDisassociateVPCComment :: Generic DisassociateVPCComment _
instance showDisassociateVPCComment :: Show DisassociateVPCComment where
  show = genericShow
instance decodeDisassociateVPCComment :: Decode DisassociateVPCComment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateVPCComment :: Encode DisassociateVPCComment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the VPC that you want to disassociate from a specified private hosted zone.</p>
newtype DisassociateVPCFromHostedZoneRequest = DisassociateVPCFromHostedZoneRequest 
  { "HostedZoneId" :: (ResourceId)
  , "VPC" :: (VPC)
  , "Comment" :: NullOrUndefined.NullOrUndefined (DisassociateVPCComment)
  }
derive instance newtypeDisassociateVPCFromHostedZoneRequest :: Newtype DisassociateVPCFromHostedZoneRequest _
derive instance repGenericDisassociateVPCFromHostedZoneRequest :: Generic DisassociateVPCFromHostedZoneRequest _
instance showDisassociateVPCFromHostedZoneRequest :: Show DisassociateVPCFromHostedZoneRequest where
  show = genericShow
instance decodeDisassociateVPCFromHostedZoneRequest :: Decode DisassociateVPCFromHostedZoneRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateVPCFromHostedZoneRequest :: Encode DisassociateVPCFromHostedZoneRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the disassociate request.</p>
newtype DisassociateVPCFromHostedZoneResponse = DisassociateVPCFromHostedZoneResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeDisassociateVPCFromHostedZoneResponse :: Newtype DisassociateVPCFromHostedZoneResponse _
derive instance repGenericDisassociateVPCFromHostedZoneResponse :: Generic DisassociateVPCFromHostedZoneResponse _
instance showDisassociateVPCFromHostedZoneResponse :: Show DisassociateVPCFromHostedZoneResponse where
  show = genericShow
instance decodeDisassociateVPCFromHostedZoneResponse :: Decode DisassociateVPCFromHostedZoneResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateVPCFromHostedZoneResponse :: Encode DisassociateVPCFromHostedZoneResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnableSNI = EnableSNI Boolean
derive instance newtypeEnableSNI :: Newtype EnableSNI _
derive instance repGenericEnableSNI :: Generic EnableSNI _
instance showEnableSNI :: Show EnableSNI where
  show = genericShow
instance decodeEnableSNI :: Decode EnableSNI where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableSNI :: Encode EnableSNI where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessages = ErrorMessages (Array ErrorMessage)
derive instance newtypeErrorMessages :: Newtype ErrorMessages _
derive instance repGenericErrorMessages :: Generic ErrorMessages _
instance showErrorMessages :: Show ErrorMessages where
  show = genericShow
instance decodeErrorMessages :: Decode ErrorMessages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessages :: Encode ErrorMessages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EvaluationPeriods = EvaluationPeriods Int
derive instance newtypeEvaluationPeriods :: Newtype EvaluationPeriods _
derive instance repGenericEvaluationPeriods :: Generic EvaluationPeriods _
instance showEvaluationPeriods :: Show EvaluationPeriods where
  show = genericShow
instance decodeEvaluationPeriods :: Decode EvaluationPeriods where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEvaluationPeriods :: Encode EvaluationPeriods where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailureThreshold = FailureThreshold Int
derive instance newtypeFailureThreshold :: Newtype FailureThreshold _
derive instance repGenericFailureThreshold :: Generic FailureThreshold _
instance showFailureThreshold :: Show FailureThreshold where
  show = genericShow
instance decodeFailureThreshold :: Decode FailureThreshold where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureThreshold :: Encode FailureThreshold where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FullyQualifiedDomainName = FullyQualifiedDomainName String
derive instance newtypeFullyQualifiedDomainName :: Newtype FullyQualifiedDomainName _
derive instance repGenericFullyQualifiedDomainName :: Generic FullyQualifiedDomainName _
instance showFullyQualifiedDomainName :: Show FullyQualifiedDomainName where
  show = genericShow
instance decodeFullyQualifiedDomainName :: Decode FullyQualifiedDomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFullyQualifiedDomainName :: Encode FullyQualifiedDomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about a geo location.</p>
newtype GeoLocation = GeoLocation 
  { "ContinentCode" :: NullOrUndefined.NullOrUndefined (GeoLocationContinentCode)
  , "CountryCode" :: NullOrUndefined.NullOrUndefined (GeoLocationCountryCode)
  , "SubdivisionCode" :: NullOrUndefined.NullOrUndefined (GeoLocationSubdivisionCode)
  }
derive instance newtypeGeoLocation :: Newtype GeoLocation _
derive instance repGenericGeoLocation :: Generic GeoLocation _
instance showGeoLocation :: Show GeoLocation where
  show = genericShow
instance decodeGeoLocation :: Decode GeoLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocation :: Encode GeoLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoLocationContinentCode = GeoLocationContinentCode String
derive instance newtypeGeoLocationContinentCode :: Newtype GeoLocationContinentCode _
derive instance repGenericGeoLocationContinentCode :: Generic GeoLocationContinentCode _
instance showGeoLocationContinentCode :: Show GeoLocationContinentCode where
  show = genericShow
instance decodeGeoLocationContinentCode :: Decode GeoLocationContinentCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocationContinentCode :: Encode GeoLocationContinentCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoLocationContinentName = GeoLocationContinentName String
derive instance newtypeGeoLocationContinentName :: Newtype GeoLocationContinentName _
derive instance repGenericGeoLocationContinentName :: Generic GeoLocationContinentName _
instance showGeoLocationContinentName :: Show GeoLocationContinentName where
  show = genericShow
instance decodeGeoLocationContinentName :: Decode GeoLocationContinentName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocationContinentName :: Encode GeoLocationContinentName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoLocationCountryCode = GeoLocationCountryCode String
derive instance newtypeGeoLocationCountryCode :: Newtype GeoLocationCountryCode _
derive instance repGenericGeoLocationCountryCode :: Generic GeoLocationCountryCode _
instance showGeoLocationCountryCode :: Show GeoLocationCountryCode where
  show = genericShow
instance decodeGeoLocationCountryCode :: Decode GeoLocationCountryCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocationCountryCode :: Encode GeoLocationCountryCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoLocationCountryName = GeoLocationCountryName String
derive instance newtypeGeoLocationCountryName :: Newtype GeoLocationCountryName _
derive instance repGenericGeoLocationCountryName :: Generic GeoLocationCountryName _
instance showGeoLocationCountryName :: Show GeoLocationCountryName where
  show = genericShow
instance decodeGeoLocationCountryName :: Decode GeoLocationCountryName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocationCountryName :: Encode GeoLocationCountryName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the codes and full continent, country, and subdivision names for the specified <code>geolocation</code> code.</p>
newtype GeoLocationDetails = GeoLocationDetails 
  { "ContinentCode" :: NullOrUndefined.NullOrUndefined (GeoLocationContinentCode)
  , "ContinentName" :: NullOrUndefined.NullOrUndefined (GeoLocationContinentName)
  , "CountryCode" :: NullOrUndefined.NullOrUndefined (GeoLocationCountryCode)
  , "CountryName" :: NullOrUndefined.NullOrUndefined (GeoLocationCountryName)
  , "SubdivisionCode" :: NullOrUndefined.NullOrUndefined (GeoLocationSubdivisionCode)
  , "SubdivisionName" :: NullOrUndefined.NullOrUndefined (GeoLocationSubdivisionName)
  }
derive instance newtypeGeoLocationDetails :: Newtype GeoLocationDetails _
derive instance repGenericGeoLocationDetails :: Generic GeoLocationDetails _
instance showGeoLocationDetails :: Show GeoLocationDetails where
  show = genericShow
instance decodeGeoLocationDetails :: Decode GeoLocationDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocationDetails :: Encode GeoLocationDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoLocationDetailsList = GeoLocationDetailsList (Array GeoLocationDetails)
derive instance newtypeGeoLocationDetailsList :: Newtype GeoLocationDetailsList _
derive instance repGenericGeoLocationDetailsList :: Generic GeoLocationDetailsList _
instance showGeoLocationDetailsList :: Show GeoLocationDetailsList where
  show = genericShow
instance decodeGeoLocationDetailsList :: Decode GeoLocationDetailsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocationDetailsList :: Encode GeoLocationDetailsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoLocationSubdivisionCode = GeoLocationSubdivisionCode String
derive instance newtypeGeoLocationSubdivisionCode :: Newtype GeoLocationSubdivisionCode _
derive instance repGenericGeoLocationSubdivisionCode :: Generic GeoLocationSubdivisionCode _
instance showGeoLocationSubdivisionCode :: Show GeoLocationSubdivisionCode where
  show = genericShow
instance decodeGeoLocationSubdivisionCode :: Decode GeoLocationSubdivisionCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocationSubdivisionCode :: Encode GeoLocationSubdivisionCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GeoLocationSubdivisionName = GeoLocationSubdivisionName String
derive instance newtypeGeoLocationSubdivisionName :: Newtype GeoLocationSubdivisionName _
derive instance repGenericGeoLocationSubdivisionName :: Generic GeoLocationSubdivisionName _
instance showGeoLocationSubdivisionName :: Show GeoLocationSubdivisionName where
  show = genericShow
instance decodeGeoLocationSubdivisionName :: Decode GeoLocationSubdivisionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGeoLocationSubdivisionName :: Encode GeoLocationSubdivisionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the request to create a hosted zone.</p>
newtype GetAccountLimitRequest = GetAccountLimitRequest 
  { "Type" :: (AccountLimitType)
  }
derive instance newtypeGetAccountLimitRequest :: Newtype GetAccountLimitRequest _
derive instance repGenericGetAccountLimitRequest :: Generic GetAccountLimitRequest _
instance showGetAccountLimitRequest :: Show GetAccountLimitRequest where
  show = genericShow
instance decodeGetAccountLimitRequest :: Decode GetAccountLimitRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAccountLimitRequest :: Encode GetAccountLimitRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the requested limit. </p>
newtype GetAccountLimitResponse = GetAccountLimitResponse 
  { "Limit" :: (AccountLimit)
  , "Count" :: (UsageCount)
  }
derive instance newtypeGetAccountLimitResponse :: Newtype GetAccountLimitResponse _
derive instance repGenericGetAccountLimitResponse :: Generic GetAccountLimitResponse _
instance showGetAccountLimitResponse :: Show GetAccountLimitResponse where
  show = genericShow
instance decodeGetAccountLimitResponse :: Decode GetAccountLimitResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetAccountLimitResponse :: Encode GetAccountLimitResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input for a GetChange request.</p>
newtype GetChangeRequest = GetChangeRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeGetChangeRequest :: Newtype GetChangeRequest _
derive instance repGenericGetChangeRequest :: Generic GetChangeRequest _
instance showGetChangeRequest :: Show GetChangeRequest where
  show = genericShow
instance decodeGetChangeRequest :: Decode GetChangeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetChangeRequest :: Encode GetChangeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the <code>ChangeInfo</code> element.</p>
newtype GetChangeResponse = GetChangeResponse 
  { "ChangeInfo" :: (ChangeInfo)
  }
derive instance newtypeGetChangeResponse :: Newtype GetChangeResponse _
derive instance repGenericGetChangeResponse :: Generic GetChangeResponse _
instance showGetChangeResponse :: Show GetChangeResponse where
  show = genericShow
instance decodeGetChangeResponse :: Decode GetChangeResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetChangeResponse :: Encode GetChangeResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCheckerIpRangesRequest = GetCheckerIpRangesRequest Types.NoArguments
derive instance newtypeGetCheckerIpRangesRequest :: Newtype GetCheckerIpRangesRequest _
derive instance repGenericGetCheckerIpRangesRequest :: Generic GetCheckerIpRangesRequest _
instance showGetCheckerIpRangesRequest :: Show GetCheckerIpRangesRequest where
  show = genericShow
instance decodeGetCheckerIpRangesRequest :: Decode GetCheckerIpRangesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCheckerIpRangesRequest :: Encode GetCheckerIpRangesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCheckerIpRangesResponse = GetCheckerIpRangesResponse 
  { "CheckerIpRanges" :: (CheckerIpRanges)
  }
derive instance newtypeGetCheckerIpRangesResponse :: Newtype GetCheckerIpRangesResponse _
derive instance repGenericGetCheckerIpRangesResponse :: Generic GetCheckerIpRangesResponse _
instance showGetCheckerIpRangesResponse :: Show GetCheckerIpRangesResponse where
  show = genericShow
instance decodeGetCheckerIpRangesResponse :: Decode GetCheckerIpRangesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCheckerIpRangesResponse :: Encode GetCheckerIpRangesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request for information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.</p>
newtype GetGeoLocationRequest = GetGeoLocationRequest 
  { "ContinentCode" :: NullOrUndefined.NullOrUndefined (GeoLocationContinentCode)
  , "CountryCode" :: NullOrUndefined.NullOrUndefined (GeoLocationCountryCode)
  , "SubdivisionCode" :: NullOrUndefined.NullOrUndefined (GeoLocationSubdivisionCode)
  }
derive instance newtypeGetGeoLocationRequest :: Newtype GetGeoLocationRequest _
derive instance repGenericGetGeoLocationRequest :: Generic GetGeoLocationRequest _
instance showGetGeoLocationRequest :: Show GetGeoLocationRequest where
  show = genericShow
instance decodeGetGeoLocationRequest :: Decode GetGeoLocationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGeoLocationRequest :: Encode GetGeoLocationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the specified geolocation code.</p>
newtype GetGeoLocationResponse = GetGeoLocationResponse 
  { "GeoLocationDetails" :: (GeoLocationDetails)
  }
derive instance newtypeGetGeoLocationResponse :: Newtype GetGeoLocationResponse _
derive instance repGenericGetGeoLocationResponse :: Generic GetGeoLocationResponse _
instance showGetGeoLocationResponse :: Show GetGeoLocationResponse where
  show = genericShow
instance decodeGetGeoLocationResponse :: Decode GetGeoLocationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetGeoLocationResponse :: Encode GetGeoLocationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request for the number of health checks that are associated with the current AWS account.</p>
newtype GetHealthCheckCountRequest = GetHealthCheckCountRequest Types.NoArguments
derive instance newtypeGetHealthCheckCountRequest :: Newtype GetHealthCheckCountRequest _
derive instance repGenericGetHealthCheckCountRequest :: Generic GetHealthCheckCountRequest _
instance showGetHealthCheckCountRequest :: Show GetHealthCheckCountRequest where
  show = genericShow
instance decodeGetHealthCheckCountRequest :: Decode GetHealthCheckCountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHealthCheckCountRequest :: Encode GetHealthCheckCountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to a <code>GetHealthCheckCount</code> request.</p>
newtype GetHealthCheckCountResponse = GetHealthCheckCountResponse 
  { "HealthCheckCount" :: (HealthCheckCount)
  }
derive instance newtypeGetHealthCheckCountResponse :: Newtype GetHealthCheckCountResponse _
derive instance repGenericGetHealthCheckCountResponse :: Generic GetHealthCheckCountResponse _
instance showGetHealthCheckCountResponse :: Show GetHealthCheckCountResponse where
  show = genericShow
instance decodeGetHealthCheckCountResponse :: Decode GetHealthCheckCountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHealthCheckCountResponse :: Encode GetHealthCheckCountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request for the reason that a health check failed most recently.</p>
newtype GetHealthCheckLastFailureReasonRequest = GetHealthCheckLastFailureReasonRequest 
  { "HealthCheckId" :: (HealthCheckId)
  }
derive instance newtypeGetHealthCheckLastFailureReasonRequest :: Newtype GetHealthCheckLastFailureReasonRequest _
derive instance repGenericGetHealthCheckLastFailureReasonRequest :: Generic GetHealthCheckLastFailureReasonRequest _
instance showGetHealthCheckLastFailureReasonRequest :: Show GetHealthCheckLastFailureReasonRequest where
  show = genericShow
instance decodeGetHealthCheckLastFailureReasonRequest :: Decode GetHealthCheckLastFailureReasonRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHealthCheckLastFailureReasonRequest :: Encode GetHealthCheckLastFailureReasonRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to a <code>GetHealthCheckLastFailureReason</code> request.</p>
newtype GetHealthCheckLastFailureReasonResponse = GetHealthCheckLastFailureReasonResponse 
  { "HealthCheckObservations" :: (HealthCheckObservations)
  }
derive instance newtypeGetHealthCheckLastFailureReasonResponse :: Newtype GetHealthCheckLastFailureReasonResponse _
derive instance repGenericGetHealthCheckLastFailureReasonResponse :: Generic GetHealthCheckLastFailureReasonResponse _
instance showGetHealthCheckLastFailureReasonResponse :: Show GetHealthCheckLastFailureReasonResponse where
  show = genericShow
instance decodeGetHealthCheckLastFailureReasonResponse :: Decode GetHealthCheckLastFailureReasonResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHealthCheckLastFailureReasonResponse :: Encode GetHealthCheckLastFailureReasonResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about a specified health check. </p>
newtype GetHealthCheckRequest = GetHealthCheckRequest 
  { "HealthCheckId" :: (HealthCheckId)
  }
derive instance newtypeGetHealthCheckRequest :: Newtype GetHealthCheckRequest _
derive instance repGenericGetHealthCheckRequest :: Generic GetHealthCheckRequest _
instance showGetHealthCheckRequest :: Show GetHealthCheckRequest where
  show = genericShow
instance decodeGetHealthCheckRequest :: Decode GetHealthCheckRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHealthCheckRequest :: Encode GetHealthCheckRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to a <code>GetHealthCheck</code> request.</p>
newtype GetHealthCheckResponse = GetHealthCheckResponse 
  { "HealthCheck" :: (HealthCheck)
  }
derive instance newtypeGetHealthCheckResponse :: Newtype GetHealthCheckResponse _
derive instance repGenericGetHealthCheckResponse :: Generic GetHealthCheckResponse _
instance showGetHealthCheckResponse :: Show GetHealthCheckResponse where
  show = genericShow
instance decodeGetHealthCheckResponse :: Decode GetHealthCheckResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHealthCheckResponse :: Encode GetHealthCheckResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get the status for a health check.</p>
newtype GetHealthCheckStatusRequest = GetHealthCheckStatusRequest 
  { "HealthCheckId" :: (HealthCheckId)
  }
derive instance newtypeGetHealthCheckStatusRequest :: Newtype GetHealthCheckStatusRequest _
derive instance repGenericGetHealthCheckStatusRequest :: Generic GetHealthCheckStatusRequest _
instance showGetHealthCheckStatusRequest :: Show GetHealthCheckStatusRequest where
  show = genericShow
instance decodeGetHealthCheckStatusRequest :: Decode GetHealthCheckStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHealthCheckStatusRequest :: Encode GetHealthCheckStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to a <code>GetHealthCheck</code> request.</p>
newtype GetHealthCheckStatusResponse = GetHealthCheckStatusResponse 
  { "HealthCheckObservations" :: (HealthCheckObservations)
  }
derive instance newtypeGetHealthCheckStatusResponse :: Newtype GetHealthCheckStatusResponse _
derive instance repGenericGetHealthCheckStatusResponse :: Generic GetHealthCheckStatusResponse _
instance showGetHealthCheckStatusResponse :: Show GetHealthCheckStatusResponse where
  show = genericShow
instance decodeGetHealthCheckStatusResponse :: Decode GetHealthCheckStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHealthCheckStatusResponse :: Encode GetHealthCheckStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to retrieve a count of all the hosted zones that are associated with the current AWS account.</p>
newtype GetHostedZoneCountRequest = GetHostedZoneCountRequest Types.NoArguments
derive instance newtypeGetHostedZoneCountRequest :: Newtype GetHostedZoneCountRequest _
derive instance repGenericGetHostedZoneCountRequest :: Generic GetHostedZoneCountRequest _
instance showGetHostedZoneCountRequest :: Show GetHostedZoneCountRequest where
  show = genericShow
instance decodeGetHostedZoneCountRequest :: Decode GetHostedZoneCountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHostedZoneCountRequest :: Encode GetHostedZoneCountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to a <code>GetHostedZoneCount</code> request.</p>
newtype GetHostedZoneCountResponse = GetHostedZoneCountResponse 
  { "HostedZoneCount" :: (HostedZoneCount)
  }
derive instance newtypeGetHostedZoneCountResponse :: Newtype GetHostedZoneCountResponse _
derive instance repGenericGetHostedZoneCountResponse :: Generic GetHostedZoneCountResponse _
instance showGetHostedZoneCountResponse :: Show GetHostedZoneCountResponse where
  show = genericShow
instance decodeGetHostedZoneCountResponse :: Decode GetHostedZoneCountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHostedZoneCountResponse :: Encode GetHostedZoneCountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the request to create a hosted zone.</p>
newtype GetHostedZoneLimitRequest = GetHostedZoneLimitRequest 
  { "Type" :: (HostedZoneLimitType)
  , "HostedZoneId" :: (ResourceId)
  }
derive instance newtypeGetHostedZoneLimitRequest :: Newtype GetHostedZoneLimitRequest _
derive instance repGenericGetHostedZoneLimitRequest :: Generic GetHostedZoneLimitRequest _
instance showGetHostedZoneLimitRequest :: Show GetHostedZoneLimitRequest where
  show = genericShow
instance decodeGetHostedZoneLimitRequest :: Decode GetHostedZoneLimitRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHostedZoneLimitRequest :: Encode GetHostedZoneLimitRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the requested limit. </p>
newtype GetHostedZoneLimitResponse = GetHostedZoneLimitResponse 
  { "Limit" :: (HostedZoneLimit)
  , "Count" :: (UsageCount)
  }
derive instance newtypeGetHostedZoneLimitResponse :: Newtype GetHostedZoneLimitResponse _
derive instance repGenericGetHostedZoneLimitResponse :: Generic GetHostedZoneLimitResponse _
instance showGetHostedZoneLimitResponse :: Show GetHostedZoneLimitResponse where
  show = genericShow
instance decodeGetHostedZoneLimitResponse :: Decode GetHostedZoneLimitResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHostedZoneLimitResponse :: Encode GetHostedZoneLimitResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about a specified hosted zone. </p>
newtype GetHostedZoneRequest = GetHostedZoneRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeGetHostedZoneRequest :: Newtype GetHostedZoneRequest _
derive instance repGenericGetHostedZoneRequest :: Generic GetHostedZoneRequest _
instance showGetHostedZoneRequest :: Show GetHostedZoneRequest where
  show = genericShow
instance decodeGetHostedZoneRequest :: Decode GetHostedZoneRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHostedZoneRequest :: Encode GetHostedZoneRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contain the response to a <code>GetHostedZone</code> request.</p>
newtype GetHostedZoneResponse = GetHostedZoneResponse 
  { "HostedZone" :: (HostedZone)
  , "DelegationSet" :: NullOrUndefined.NullOrUndefined (DelegationSet)
  , "VPCs" :: NullOrUndefined.NullOrUndefined (VPCs)
  }
derive instance newtypeGetHostedZoneResponse :: Newtype GetHostedZoneResponse _
derive instance repGenericGetHostedZoneResponse :: Generic GetHostedZoneResponse _
instance showGetHostedZoneResponse :: Show GetHostedZoneResponse where
  show = genericShow
instance decodeGetHostedZoneResponse :: Decode GetHostedZoneResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetHostedZoneResponse :: Encode GetHostedZoneResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetQueryLoggingConfigRequest = GetQueryLoggingConfigRequest 
  { "Id" :: (QueryLoggingConfigId)
  }
derive instance newtypeGetQueryLoggingConfigRequest :: Newtype GetQueryLoggingConfigRequest _
derive instance repGenericGetQueryLoggingConfigRequest :: Generic GetQueryLoggingConfigRequest _
instance showGetQueryLoggingConfigRequest :: Show GetQueryLoggingConfigRequest where
  show = genericShow
instance decodeGetQueryLoggingConfigRequest :: Decode GetQueryLoggingConfigRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetQueryLoggingConfigRequest :: Encode GetQueryLoggingConfigRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetQueryLoggingConfigResponse = GetQueryLoggingConfigResponse 
  { "QueryLoggingConfig" :: (QueryLoggingConfig)
  }
derive instance newtypeGetQueryLoggingConfigResponse :: Newtype GetQueryLoggingConfigResponse _
derive instance repGenericGetQueryLoggingConfigResponse :: Generic GetQueryLoggingConfigResponse _
instance showGetQueryLoggingConfigResponse :: Show GetQueryLoggingConfigResponse where
  show = genericShow
instance decodeGetQueryLoggingConfigResponse :: Decode GetQueryLoggingConfigResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetQueryLoggingConfigResponse :: Encode GetQueryLoggingConfigResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the request to create a hosted zone.</p>
newtype GetReusableDelegationSetLimitRequest = GetReusableDelegationSetLimitRequest 
  { "Type" :: (ReusableDelegationSetLimitType)
  , "DelegationSetId" :: (ResourceId)
  }
derive instance newtypeGetReusableDelegationSetLimitRequest :: Newtype GetReusableDelegationSetLimitRequest _
derive instance repGenericGetReusableDelegationSetLimitRequest :: Generic GetReusableDelegationSetLimitRequest _
instance showGetReusableDelegationSetLimitRequest :: Show GetReusableDelegationSetLimitRequest where
  show = genericShow
instance decodeGetReusableDelegationSetLimitRequest :: Decode GetReusableDelegationSetLimitRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReusableDelegationSetLimitRequest :: Encode GetReusableDelegationSetLimitRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the requested limit. </p>
newtype GetReusableDelegationSetLimitResponse = GetReusableDelegationSetLimitResponse 
  { "Limit" :: (ReusableDelegationSetLimit)
  , "Count" :: (UsageCount)
  }
derive instance newtypeGetReusableDelegationSetLimitResponse :: Newtype GetReusableDelegationSetLimitResponse _
derive instance repGenericGetReusableDelegationSetLimitResponse :: Generic GetReusableDelegationSetLimitResponse _
instance showGetReusableDelegationSetLimitResponse :: Show GetReusableDelegationSetLimitResponse where
  show = genericShow
instance decodeGetReusableDelegationSetLimitResponse :: Decode GetReusableDelegationSetLimitResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReusableDelegationSetLimitResponse :: Encode GetReusableDelegationSetLimitResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about a specified reusable delegation set.</p>
newtype GetReusableDelegationSetRequest = GetReusableDelegationSetRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeGetReusableDelegationSetRequest :: Newtype GetReusableDelegationSetRequest _
derive instance repGenericGetReusableDelegationSetRequest :: Generic GetReusableDelegationSetRequest _
instance showGetReusableDelegationSetRequest :: Show GetReusableDelegationSetRequest where
  show = genericShow
instance decodeGetReusableDelegationSetRequest :: Decode GetReusableDelegationSetRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReusableDelegationSetRequest :: Encode GetReusableDelegationSetRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to the <code>GetReusableDelegationSet</code> request.</p>
newtype GetReusableDelegationSetResponse = GetReusableDelegationSetResponse 
  { "DelegationSet" :: (DelegationSet)
  }
derive instance newtypeGetReusableDelegationSetResponse :: Newtype GetReusableDelegationSetResponse _
derive instance repGenericGetReusableDelegationSetResponse :: Generic GetReusableDelegationSetResponse _
instance showGetReusableDelegationSetResponse :: Show GetReusableDelegationSetResponse where
  show = genericShow
instance decodeGetReusableDelegationSetResponse :: Decode GetReusableDelegationSetResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetReusableDelegationSetResponse :: Encode GetReusableDelegationSetResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request to get the number of traffic policy instances that are associated with the current AWS account.</p>
newtype GetTrafficPolicyInstanceCountRequest = GetTrafficPolicyInstanceCountRequest Types.NoArguments
derive instance newtypeGetTrafficPolicyInstanceCountRequest :: Newtype GetTrafficPolicyInstanceCountRequest _
derive instance repGenericGetTrafficPolicyInstanceCountRequest :: Generic GetTrafficPolicyInstanceCountRequest _
instance showGetTrafficPolicyInstanceCountRequest :: Show GetTrafficPolicyInstanceCountRequest where
  show = genericShow
instance decodeGetTrafficPolicyInstanceCountRequest :: Decode GetTrafficPolicyInstanceCountRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTrafficPolicyInstanceCountRequest :: Encode GetTrafficPolicyInstanceCountRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>
newtype GetTrafficPolicyInstanceCountResponse = GetTrafficPolicyInstanceCountResponse 
  { "TrafficPolicyInstanceCount" :: (TrafficPolicyInstanceCount)
  }
derive instance newtypeGetTrafficPolicyInstanceCountResponse :: Newtype GetTrafficPolicyInstanceCountResponse _
derive instance repGenericGetTrafficPolicyInstanceCountResponse :: Generic GetTrafficPolicyInstanceCountResponse _
instance showGetTrafficPolicyInstanceCountResponse :: Show GetTrafficPolicyInstanceCountResponse where
  show = genericShow
instance decodeGetTrafficPolicyInstanceCountResponse :: Decode GetTrafficPolicyInstanceCountResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTrafficPolicyInstanceCountResponse :: Encode GetTrafficPolicyInstanceCountResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets information about a specified traffic policy instance.</p>
newtype GetTrafficPolicyInstanceRequest = GetTrafficPolicyInstanceRequest 
  { "Id" :: (TrafficPolicyInstanceId)
  }
derive instance newtypeGetTrafficPolicyInstanceRequest :: Newtype GetTrafficPolicyInstanceRequest _
derive instance repGenericGetTrafficPolicyInstanceRequest :: Generic GetTrafficPolicyInstanceRequest _
instance showGetTrafficPolicyInstanceRequest :: Show GetTrafficPolicyInstanceRequest where
  show = genericShow
instance decodeGetTrafficPolicyInstanceRequest :: Decode GetTrafficPolicyInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTrafficPolicyInstanceRequest :: Encode GetTrafficPolicyInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>
newtype GetTrafficPolicyInstanceResponse = GetTrafficPolicyInstanceResponse 
  { "TrafficPolicyInstance" :: (TrafficPolicyInstance)
  }
derive instance newtypeGetTrafficPolicyInstanceResponse :: Newtype GetTrafficPolicyInstanceResponse _
derive instance repGenericGetTrafficPolicyInstanceResponse :: Generic GetTrafficPolicyInstanceResponse _
instance showGetTrafficPolicyInstanceResponse :: Show GetTrafficPolicyInstanceResponse where
  show = genericShow
instance decodeGetTrafficPolicyInstanceResponse :: Decode GetTrafficPolicyInstanceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTrafficPolicyInstanceResponse :: Encode GetTrafficPolicyInstanceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Gets information about a specific traffic policy version.</p>
newtype GetTrafficPolicyRequest = GetTrafficPolicyRequest 
  { "Id" :: (TrafficPolicyId)
  , "Version" :: (TrafficPolicyVersion)
  }
derive instance newtypeGetTrafficPolicyRequest :: Newtype GetTrafficPolicyRequest _
derive instance repGenericGetTrafficPolicyRequest :: Generic GetTrafficPolicyRequest _
instance showGetTrafficPolicyRequest :: Show GetTrafficPolicyRequest where
  show = genericShow
instance decodeGetTrafficPolicyRequest :: Decode GetTrafficPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTrafficPolicyRequest :: Encode GetTrafficPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the request.</p>
newtype GetTrafficPolicyResponse = GetTrafficPolicyResponse 
  { "TrafficPolicy" :: (TrafficPolicy)
  }
derive instance newtypeGetTrafficPolicyResponse :: Newtype GetTrafficPolicyResponse _
derive instance repGenericGetTrafficPolicyResponse :: Generic GetTrafficPolicyResponse _
instance showGetTrafficPolicyResponse :: Show GetTrafficPolicyResponse where
  show = genericShow
instance decodeGetTrafficPolicyResponse :: Decode GetTrafficPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTrafficPolicyResponse :: Encode GetTrafficPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about one health check that is associated with the current AWS account.</p>
newtype HealthCheck = HealthCheck 
  { "Id" :: (HealthCheckId)
  , "CallerReference" :: (HealthCheckNonce)
  , "LinkedService" :: NullOrUndefined.NullOrUndefined (LinkedService)
  , "HealthCheckConfig" :: (HealthCheckConfig)
  , "HealthCheckVersion" :: (HealthCheckVersion)
  , "CloudWatchAlarmConfiguration" :: NullOrUndefined.NullOrUndefined (CloudWatchAlarmConfiguration)
  }
derive instance newtypeHealthCheck :: Newtype HealthCheck _
derive instance repGenericHealthCheck :: Generic HealthCheck _
instance showHealthCheck :: Show HealthCheck where
  show = genericShow
instance decodeHealthCheck :: Decode HealthCheck where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheck :: Encode HealthCheck where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The health check you're attempting to create already exists. Amazon Route 53 returns this error when you submit a request that has the following values:</p> <ul> <li> <p>The same value for <code>CallerReference</code> as an existing health check, and one or more values that differ from the existing health check that has the same caller reference.</p> </li> <li> <p>The same value for <code>CallerReference</code> as a health check that you created and later deleted, regardless of the other settings in the request.</p> </li> </ul>
newtype HealthCheckAlreadyExists = HealthCheckAlreadyExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHealthCheckAlreadyExists :: Newtype HealthCheckAlreadyExists _
derive instance repGenericHealthCheckAlreadyExists :: Generic HealthCheckAlreadyExists _
instance showHealthCheckAlreadyExists :: Show HealthCheckAlreadyExists where
  show = genericShow
instance decodeHealthCheckAlreadyExists :: Decode HealthCheckAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckAlreadyExists :: Encode HealthCheckAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the health check.</p>
newtype HealthCheckConfig = HealthCheckConfig 
  { "IPAddress" :: NullOrUndefined.NullOrUndefined (IPAddress)
  , "Port" :: NullOrUndefined.NullOrUndefined (Port)
  , "Type" :: (HealthCheckType)
  , "ResourcePath" :: NullOrUndefined.NullOrUndefined (ResourcePath)
  , "FullyQualifiedDomainName" :: NullOrUndefined.NullOrUndefined (FullyQualifiedDomainName)
  , "SearchString" :: NullOrUndefined.NullOrUndefined (SearchString)
  , "RequestInterval" :: NullOrUndefined.NullOrUndefined (RequestInterval)
  , "FailureThreshold" :: NullOrUndefined.NullOrUndefined (FailureThreshold)
  , "MeasureLatency" :: NullOrUndefined.NullOrUndefined (MeasureLatency)
  , "Inverted" :: NullOrUndefined.NullOrUndefined (Inverted)
  , "HealthThreshold" :: NullOrUndefined.NullOrUndefined (HealthThreshold)
  , "ChildHealthChecks" :: NullOrUndefined.NullOrUndefined (ChildHealthCheckList)
  , "EnableSNI" :: NullOrUndefined.NullOrUndefined (EnableSNI)
  , "Regions" :: NullOrUndefined.NullOrUndefined (HealthCheckRegionList)
  , "AlarmIdentifier" :: NullOrUndefined.NullOrUndefined (AlarmIdentifier)
  , "InsufficientDataHealthStatus" :: NullOrUndefined.NullOrUndefined (InsufficientDataHealthStatus)
  }
derive instance newtypeHealthCheckConfig :: Newtype HealthCheckConfig _
derive instance repGenericHealthCheckConfig :: Generic HealthCheckConfig _
instance showHealthCheckConfig :: Show HealthCheckConfig where
  show = genericShow
instance decodeHealthCheckConfig :: Decode HealthCheckConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckConfig :: Encode HealthCheckConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckCount = HealthCheckCount Number
derive instance newtypeHealthCheckCount :: Newtype HealthCheckCount _
derive instance repGenericHealthCheckCount :: Generic HealthCheckCount _
instance showHealthCheckCount :: Show HealthCheckCount where
  show = genericShow
instance decodeHealthCheckCount :: Decode HealthCheckCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckCount :: Encode HealthCheckCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckId = HealthCheckId String
derive instance newtypeHealthCheckId :: Newtype HealthCheckId _
derive instance repGenericHealthCheckId :: Generic HealthCheckId _
instance showHealthCheckId :: Show HealthCheckId where
  show = genericShow
instance decodeHealthCheckId :: Decode HealthCheckId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckId :: Encode HealthCheckId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This error code is not in use.</p>
newtype HealthCheckInUse = HealthCheckInUse 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHealthCheckInUse :: Newtype HealthCheckInUse _
derive instance repGenericHealthCheckInUse :: Generic HealthCheckInUse _
instance showHealthCheckInUse :: Show HealthCheckInUse where
  show = genericShow
instance decodeHealthCheckInUse :: Decode HealthCheckInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckInUse :: Encode HealthCheckInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckNonce = HealthCheckNonce String
derive instance newtypeHealthCheckNonce :: Newtype HealthCheckNonce _
derive instance repGenericHealthCheckNonce :: Generic HealthCheckNonce _
instance showHealthCheckNonce :: Show HealthCheckNonce where
  show = genericShow
instance decodeHealthCheckNonce :: Decode HealthCheckNonce where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckNonce :: Encode HealthCheckNonce where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the last failure reason as reported by one Amazon Route 53 health checker.</p>
newtype HealthCheckObservation = HealthCheckObservation 
  { "Region" :: NullOrUndefined.NullOrUndefined (HealthCheckRegion)
  , "IPAddress" :: NullOrUndefined.NullOrUndefined (IPAddress)
  , "StatusReport" :: NullOrUndefined.NullOrUndefined (StatusReport)
  }
derive instance newtypeHealthCheckObservation :: Newtype HealthCheckObservation _
derive instance repGenericHealthCheckObservation :: Generic HealthCheckObservation _
instance showHealthCheckObservation :: Show HealthCheckObservation where
  show = genericShow
instance decodeHealthCheckObservation :: Decode HealthCheckObservation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckObservation :: Encode HealthCheckObservation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckObservations = HealthCheckObservations (Array HealthCheckObservation)
derive instance newtypeHealthCheckObservations :: Newtype HealthCheckObservations _
derive instance repGenericHealthCheckObservations :: Generic HealthCheckObservations _
instance showHealthCheckObservations :: Show HealthCheckObservations where
  show = genericShow
instance decodeHealthCheckObservations :: Decode HealthCheckObservations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckObservations :: Encode HealthCheckObservations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckRegion = HealthCheckRegion String
derive instance newtypeHealthCheckRegion :: Newtype HealthCheckRegion _
derive instance repGenericHealthCheckRegion :: Generic HealthCheckRegion _
instance showHealthCheckRegion :: Show HealthCheckRegion where
  show = genericShow
instance decodeHealthCheckRegion :: Decode HealthCheckRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckRegion :: Encode HealthCheckRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckRegionList = HealthCheckRegionList (Array HealthCheckRegion)
derive instance newtypeHealthCheckRegionList :: Newtype HealthCheckRegionList _
derive instance repGenericHealthCheckRegionList :: Generic HealthCheckRegionList _
instance showHealthCheckRegionList :: Show HealthCheckRegionList where
  show = genericShow
instance decodeHealthCheckRegionList :: Decode HealthCheckRegionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckRegionList :: Encode HealthCheckRegionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckType = HealthCheckType String
derive instance newtypeHealthCheckType :: Newtype HealthCheckType _
derive instance repGenericHealthCheckType :: Generic HealthCheckType _
instance showHealthCheckType :: Show HealthCheckType where
  show = genericShow
instance decodeHealthCheckType :: Decode HealthCheckType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckType :: Encode HealthCheckType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthCheckVersion = HealthCheckVersion Number
derive instance newtypeHealthCheckVersion :: Newtype HealthCheckVersion _
derive instance repGenericHealthCheckVersion :: Generic HealthCheckVersion _
instance showHealthCheckVersion :: Show HealthCheckVersion where
  show = genericShow
instance decodeHealthCheckVersion :: Decode HealthCheckVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckVersion :: Encode HealthCheckVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of <code>HealthCheckVersion</code> in the request doesn't match the value of <code>HealthCheckVersion</code> in the health check.</p>
newtype HealthCheckVersionMismatch = HealthCheckVersionMismatch 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHealthCheckVersionMismatch :: Newtype HealthCheckVersionMismatch _
derive instance repGenericHealthCheckVersionMismatch :: Generic HealthCheckVersionMismatch _
instance showHealthCheckVersionMismatch :: Show HealthCheckVersionMismatch where
  show = genericShow
instance decodeHealthCheckVersionMismatch :: Decode HealthCheckVersionMismatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthCheckVersionMismatch :: Encode HealthCheckVersionMismatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthChecks = HealthChecks (Array HealthCheck)
derive instance newtypeHealthChecks :: Newtype HealthChecks _
derive instance repGenericHealthChecks :: Generic HealthChecks _
instance showHealthChecks :: Show HealthChecks where
  show = genericShow
instance decodeHealthChecks :: Decode HealthChecks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthChecks :: Encode HealthChecks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HealthThreshold = HealthThreshold Int
derive instance newtypeHealthThreshold :: Newtype HealthThreshold _
derive instance repGenericHealthThreshold :: Generic HealthThreshold _
instance showHealthThreshold :: Show HealthThreshold where
  show = genericShow
instance decodeHealthThreshold :: Decode HealthThreshold where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHealthThreshold :: Encode HealthThreshold where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains general information about the hosted zone.</p>
newtype HostedZone = HostedZone 
  { "Id" :: (ResourceId)
  , "Name" :: (DNSName)
  , "CallerReference" :: (Nonce)
  , "Config" :: NullOrUndefined.NullOrUndefined (HostedZoneConfig)
  , "ResourceRecordSetCount" :: NullOrUndefined.NullOrUndefined (HostedZoneRRSetCount)
  , "LinkedService" :: NullOrUndefined.NullOrUndefined (LinkedService)
  }
derive instance newtypeHostedZone :: Newtype HostedZone _
derive instance repGenericHostedZone :: Generic HostedZone _
instance showHostedZone :: Show HostedZone where
  show = genericShow
instance decodeHostedZone :: Decode HostedZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZone :: Encode HostedZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The hosted zone you're trying to create already exists. Amazon Route 53 returns this error when a hosted zone has already been created with the specified <code>CallerReference</code>.</p>
newtype HostedZoneAlreadyExists = HostedZoneAlreadyExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHostedZoneAlreadyExists :: Newtype HostedZoneAlreadyExists _
derive instance repGenericHostedZoneAlreadyExists :: Generic HostedZoneAlreadyExists _
instance showHostedZoneAlreadyExists :: Show HostedZoneAlreadyExists where
  show = genericShow
instance decodeHostedZoneAlreadyExists :: Decode HostedZoneAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneAlreadyExists :: Encode HostedZoneAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains an optional comment about your hosted zone. If you don't want to specify a comment, omit both the <code>HostedZoneConfig</code> and <code>Comment</code> elements.</p>
newtype HostedZoneConfig = HostedZoneConfig 
  { "Comment" :: NullOrUndefined.NullOrUndefined (ResourceDescription)
  , "PrivateZone" :: NullOrUndefined.NullOrUndefined (IsPrivateZone)
  }
derive instance newtypeHostedZoneConfig :: Newtype HostedZoneConfig _
derive instance repGenericHostedZoneConfig :: Generic HostedZoneConfig _
instance showHostedZoneConfig :: Show HostedZoneConfig where
  show = genericShow
instance decodeHostedZoneConfig :: Decode HostedZoneConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneConfig :: Encode HostedZoneConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HostedZoneCount = HostedZoneCount Number
derive instance newtypeHostedZoneCount :: Newtype HostedZoneCount _
derive instance repGenericHostedZoneCount :: Generic HostedZoneCount _
instance showHostedZoneCount :: Show HostedZoneCount where
  show = genericShow
instance decodeHostedZoneCount :: Decode HostedZoneCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneCount :: Encode HostedZoneCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>
newtype HostedZoneLimit = HostedZoneLimit 
  { "Type" :: (HostedZoneLimitType)
  , "Value" :: (LimitValue)
  }
derive instance newtypeHostedZoneLimit :: Newtype HostedZoneLimit _
derive instance repGenericHostedZoneLimit :: Generic HostedZoneLimit _
instance showHostedZoneLimit :: Show HostedZoneLimit where
  show = genericShow
instance decodeHostedZoneLimit :: Decode HostedZoneLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneLimit :: Encode HostedZoneLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HostedZoneLimitType = HostedZoneLimitType String
derive instance newtypeHostedZoneLimitType :: Newtype HostedZoneLimitType _
derive instance repGenericHostedZoneLimitType :: Generic HostedZoneLimitType _
instance showHostedZoneLimitType :: Show HostedZoneLimitType where
  show = genericShow
instance decodeHostedZoneLimitType :: Decode HostedZoneLimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneLimitType :: Encode HostedZoneLimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The hosted zone contains resource records that are not SOA or NS records.</p>
newtype HostedZoneNotEmpty = HostedZoneNotEmpty 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHostedZoneNotEmpty :: Newtype HostedZoneNotEmpty _
derive instance repGenericHostedZoneNotEmpty :: Generic HostedZoneNotEmpty _
instance showHostedZoneNotEmpty :: Show HostedZoneNotEmpty where
  show = genericShow
instance decodeHostedZoneNotEmpty :: Decode HostedZoneNotEmpty where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneNotEmpty :: Encode HostedZoneNotEmpty where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified HostedZone can't be found.</p>
newtype HostedZoneNotFound = HostedZoneNotFound 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHostedZoneNotFound :: Newtype HostedZoneNotFound _
derive instance repGenericHostedZoneNotFound :: Generic HostedZoneNotFound _
instance showHostedZoneNotFound :: Show HostedZoneNotFound where
  show = genericShow
instance decodeHostedZoneNotFound :: Decode HostedZoneNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneNotFound :: Encode HostedZoneNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified hosted zone is a public hosted zone, not a private hosted zone.</p>
newtype HostedZoneNotPrivate = HostedZoneNotPrivate 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeHostedZoneNotPrivate :: Newtype HostedZoneNotPrivate _
derive instance repGenericHostedZoneNotPrivate :: Generic HostedZoneNotPrivate _
instance showHostedZoneNotPrivate :: Show HostedZoneNotPrivate where
  show = genericShow
instance decodeHostedZoneNotPrivate :: Decode HostedZoneNotPrivate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneNotPrivate :: Encode HostedZoneNotPrivate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HostedZoneRRSetCount = HostedZoneRRSetCount Number
derive instance newtypeHostedZoneRRSetCount :: Newtype HostedZoneRRSetCount _
derive instance repGenericHostedZoneRRSetCount :: Generic HostedZoneRRSetCount _
instance showHostedZoneRRSetCount :: Show HostedZoneRRSetCount where
  show = genericShow
instance decodeHostedZoneRRSetCount :: Decode HostedZoneRRSetCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZoneRRSetCount :: Encode HostedZoneRRSetCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HostedZones = HostedZones (Array HostedZone)
derive instance newtypeHostedZones :: Newtype HostedZones _
derive instance repGenericHostedZones :: Generic HostedZones _
instance showHostedZones :: Show HostedZones where
  show = genericShow
instance decodeHostedZones :: Decode HostedZones where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHostedZones :: Encode HostedZones where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPAddress = IPAddress String
derive instance newtypeIPAddress :: Newtype IPAddress _
derive instance repGenericIPAddress :: Generic IPAddress _
instance showIPAddress :: Show IPAddress where
  show = genericShow
instance decodeIPAddress :: Decode IPAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPAddress :: Encode IPAddress where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IPAddressCidr = IPAddressCidr String
derive instance newtypeIPAddressCidr :: Newtype IPAddressCidr _
derive instance repGenericIPAddressCidr :: Generic IPAddressCidr _
instance showIPAddressCidr :: Show IPAddressCidr where
  show = genericShow
instance decodeIPAddressCidr :: Decode IPAddressCidr where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIPAddressCidr :: Encode IPAddressCidr where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource you're trying to access is unsupported on this Amazon Route 53 endpoint.</p>
newtype IncompatibleVersion = IncompatibleVersion 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeIncompatibleVersion :: Newtype IncompatibleVersion _
derive instance repGenericIncompatibleVersion :: Generic IncompatibleVersion _
instance showIncompatibleVersion :: Show IncompatibleVersion where
  show = genericShow
instance decodeIncompatibleVersion :: Decode IncompatibleVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncompatibleVersion :: Encode IncompatibleVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Route 53 doesn't have the permissions required to create log streams and send query logs to log streams. Possible causes include the following:</p> <ul> <li> <p>There is no resource policy that specifies the log group ARN in the value for <code>Resource</code>.</p> </li> <li> <p>The resource policy that includes the log group ARN in the value for <code>Resource</code> doesn't have the necessary permissions.</p> </li> <li> <p>The resource policy hasn't finished propagating yet.</p> </li> </ul>
newtype InsufficientCloudWatchLogsResourcePolicy = InsufficientCloudWatchLogsResourcePolicy 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInsufficientCloudWatchLogsResourcePolicy :: Newtype InsufficientCloudWatchLogsResourcePolicy _
derive instance repGenericInsufficientCloudWatchLogsResourcePolicy :: Generic InsufficientCloudWatchLogsResourcePolicy _
instance showInsufficientCloudWatchLogsResourcePolicy :: Show InsufficientCloudWatchLogsResourcePolicy where
  show = genericShow
instance decodeInsufficientCloudWatchLogsResourcePolicy :: Decode InsufficientCloudWatchLogsResourcePolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientCloudWatchLogsResourcePolicy :: Encode InsufficientCloudWatchLogsResourcePolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InsufficientDataHealthStatus = InsufficientDataHealthStatus String
derive instance newtypeInsufficientDataHealthStatus :: Newtype InsufficientDataHealthStatus _
derive instance repGenericInsufficientDataHealthStatus :: Generic InsufficientDataHealthStatus _
instance showInsufficientDataHealthStatus :: Show InsufficientDataHealthStatus where
  show = genericShow
instance decodeInsufficientDataHealthStatus :: Decode InsufficientDataHealthStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientDataHealthStatus :: Encode InsufficientDataHealthStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Parameter name is invalid.</p>
newtype InvalidArgument = InvalidArgument 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgument :: Newtype InvalidArgument _
derive instance repGenericInvalidArgument :: Generic InvalidArgument _
instance showInvalidArgument :: Show InvalidArgument where
  show = genericShow
instance decodeInvalidArgument :: Decode InvalidArgument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArgument :: Encode InvalidArgument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception contains a list of messages that might contain one or more error messages. Each error message indicates one error in the change batch.</p>
newtype InvalidChangeBatch = InvalidChangeBatch 
  { "Messages'" :: NullOrUndefined.NullOrUndefined (ErrorMessages)
  , "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidChangeBatch :: Newtype InvalidChangeBatch _
derive instance repGenericInvalidChangeBatch :: Generic InvalidChangeBatch _
instance showInvalidChangeBatch :: Show InvalidChangeBatch where
  show = genericShow
instance decodeInvalidChangeBatch :: Decode InvalidChangeBatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidChangeBatch :: Encode InvalidChangeBatch where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified domain name is not valid.</p>
newtype InvalidDomainName = InvalidDomainName 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidDomainName :: Newtype InvalidDomainName _
derive instance repGenericInvalidDomainName :: Generic InvalidDomainName _
instance showInvalidDomainName :: Show InvalidDomainName where
  show = genericShow
instance decodeInvalidDomainName :: Decode InvalidDomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDomainName :: Encode InvalidDomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input is not valid.</p>
newtype InvalidInput = InvalidInput 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidInput :: Newtype InvalidInput _
derive instance repGenericInvalidInput :: Generic InvalidInput _
instance showInvalidInput :: Show InvalidInput where
  show = genericShow
instance decodeInvalidInput :: Decode InvalidInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidInput :: Encode InvalidInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value that you specified to get the second or subsequent page of results is invalid.</p>
newtype InvalidPaginationToken = InvalidPaginationToken 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidPaginationToken :: Newtype InvalidPaginationToken _
derive instance repGenericInvalidPaginationToken :: Generic InvalidPaginationToken _
instance showInvalidPaginationToken :: Show InvalidPaginationToken where
  show = genericShow
instance decodeInvalidPaginationToken :: Decode InvalidPaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPaginationToken :: Encode InvalidPaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The format of the traffic policy document that you specified in the <code>Document</code> element is invalid.</p>
newtype InvalidTrafficPolicyDocument = InvalidTrafficPolicyDocument 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidTrafficPolicyDocument :: Newtype InvalidTrafficPolicyDocument _
derive instance repGenericInvalidTrafficPolicyDocument :: Generic InvalidTrafficPolicyDocument _
instance showInvalidTrafficPolicyDocument :: Show InvalidTrafficPolicyDocument where
  show = genericShow
instance decodeInvalidTrafficPolicyDocument :: Decode InvalidTrafficPolicyDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidTrafficPolicyDocument :: Encode InvalidTrafficPolicyDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The VPC ID that you specified either isn't a valid ID or the current account is not authorized to access this VPC.</p>
newtype InvalidVPCId = InvalidVPCId 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidVPCId :: Newtype InvalidVPCId _
derive instance repGenericInvalidVPCId :: Generic InvalidVPCId _
instance showInvalidVPCId :: Show InvalidVPCId where
  show = genericShow
instance decodeInvalidVPCId :: Decode InvalidVPCId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidVPCId :: Encode InvalidVPCId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Inverted = Inverted Boolean
derive instance newtypeInverted :: Newtype Inverted _
derive instance repGenericInverted :: Generic Inverted _
instance showInverted :: Show Inverted where
  show = genericShow
instance decodeInverted :: Decode Inverted where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInverted :: Encode Inverted where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IsPrivateZone = IsPrivateZone Boolean
derive instance newtypeIsPrivateZone :: Newtype IsPrivateZone _
derive instance repGenericIsPrivateZone :: Generic IsPrivateZone _
instance showIsPrivateZone :: Show IsPrivateZone where
  show = genericShow
instance decodeIsPrivateZone :: Decode IsPrivateZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIsPrivateZone :: Encode IsPrivateZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The VPC that you're trying to disassociate from the private hosted zone is the last VPC that is associated with the hosted zone. Amazon Route 53 doesn't support disassociating the last VPC from a hosted zone.</p>
newtype LastVPCAssociation = LastVPCAssociation 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLastVPCAssociation :: Newtype LastVPCAssociation _
derive instance repGenericLastVPCAssociation :: Generic LastVPCAssociation _
instance showLastVPCAssociation :: Show LastVPCAssociation where
  show = genericShow
instance decodeLastVPCAssociation :: Decode LastVPCAssociation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastVPCAssociation :: Encode LastVPCAssociation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitValue = LimitValue Number
derive instance newtypeLimitValue :: Newtype LimitValue _
derive instance repGenericLimitValue :: Generic LimitValue _
instance showLimitValue :: Show LimitValue where
  show = genericShow
instance decodeLimitValue :: Decode LimitValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitValue :: Encode LimitValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This operation can't be completed either because the current account has reached the limit on reusable delegation sets that it can create or because you've reached the limit on the number of Amazon VPCs that you can associate with a private hosted zone. To get the current limit on the number of reusable delegation sets, see <a>GetAccountLimit</a>. To get the current limit on the number of Amazon VPCs that you can associate with a private hosted zone, see <a>GetHostedZoneLimit</a>. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype LimitsExceeded = LimitsExceeded 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitsExceeded :: Newtype LimitsExceeded _
derive instance repGenericLimitsExceeded :: Generic LimitsExceeded _
instance showLimitsExceeded :: Show LimitsExceeded where
  show = genericShow
instance decodeLimitsExceeded :: Decode LimitsExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitsExceeded :: Encode LimitsExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>If a health check or hosted zone was created by another service, <code>LinkedService</code> is a complex type that describes the service that created the resource. When a resource is created by another service, you can't edit or delete it using Amazon Route 53. </p>
newtype LinkedService = LinkedService 
  { "ServicePrincipal" :: NullOrUndefined.NullOrUndefined (ServicePrincipal)
  , "Description" :: NullOrUndefined.NullOrUndefined (ResourceDescription)
  }
derive instance newtypeLinkedService :: Newtype LinkedService _
derive instance repGenericLinkedService :: Generic LinkedService _
instance showLinkedService :: Show LinkedService where
  show = genericShow
instance decodeLinkedService :: Decode LinkedService where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLinkedService :: Encode LinkedService where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get a list of geographic locations that Amazon Route 53 supports for geolocation resource record sets. </p>
newtype ListGeoLocationsRequest = ListGeoLocationsRequest 
  { "StartContinentCode" :: NullOrUndefined.NullOrUndefined (GeoLocationContinentCode)
  , "StartCountryCode" :: NullOrUndefined.NullOrUndefined (GeoLocationCountryCode)
  , "StartSubdivisionCode" :: NullOrUndefined.NullOrUndefined (GeoLocationSubdivisionCode)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListGeoLocationsRequest :: Newtype ListGeoLocationsRequest _
derive instance repGenericListGeoLocationsRequest :: Generic ListGeoLocationsRequest _
instance showListGeoLocationsRequest :: Show ListGeoLocationsRequest where
  show = genericShow
instance decodeListGeoLocationsRequest :: Decode ListGeoLocationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGeoLocationsRequest :: Encode ListGeoLocationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type containing the response information for the request.</p>
newtype ListGeoLocationsResponse = ListGeoLocationsResponse 
  { "GeoLocationDetailsList" :: (GeoLocationDetailsList)
  , "IsTruncated" :: (PageTruncated)
  , "NextContinentCode" :: NullOrUndefined.NullOrUndefined (GeoLocationContinentCode)
  , "NextCountryCode" :: NullOrUndefined.NullOrUndefined (GeoLocationCountryCode)
  , "NextSubdivisionCode" :: NullOrUndefined.NullOrUndefined (GeoLocationSubdivisionCode)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListGeoLocationsResponse :: Newtype ListGeoLocationsResponse _
derive instance repGenericListGeoLocationsResponse :: Generic ListGeoLocationsResponse _
instance showListGeoLocationsResponse :: Show ListGeoLocationsResponse where
  show = genericShow
instance decodeListGeoLocationsResponse :: Decode ListGeoLocationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListGeoLocationsResponse :: Encode ListGeoLocationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to retrieve a list of the health checks that are associated with the current AWS account.</p>
newtype ListHealthChecksRequest = ListHealthChecksRequest 
  { "Marker" :: NullOrUndefined.NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListHealthChecksRequest :: Newtype ListHealthChecksRequest _
derive instance repGenericListHealthChecksRequest :: Generic ListHealthChecksRequest _
instance showListHealthChecksRequest :: Show ListHealthChecksRequest where
  show = genericShow
instance decodeListHealthChecksRequest :: Decode ListHealthChecksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHealthChecksRequest :: Encode ListHealthChecksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to a <code>ListHealthChecks</code> request.</p>
newtype ListHealthChecksResponse = ListHealthChecksResponse 
  { "HealthChecks" :: (HealthChecks)
  , "Marker" :: (PageMarker)
  , "IsTruncated" :: (PageTruncated)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (PageMarker)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListHealthChecksResponse :: Newtype ListHealthChecksResponse _
derive instance repGenericListHealthChecksResponse :: Generic ListHealthChecksResponse _
instance showListHealthChecksResponse :: Show ListHealthChecksResponse where
  show = genericShow
instance decodeListHealthChecksResponse :: Decode ListHealthChecksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHealthChecksResponse :: Encode ListHealthChecksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Retrieves a list of the public and private hosted zones that are associated with the current AWS account in ASCII order by domain name. </p>
newtype ListHostedZonesByNameRequest = ListHostedZonesByNameRequest 
  { "DNSName" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "HostedZoneId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListHostedZonesByNameRequest :: Newtype ListHostedZonesByNameRequest _
derive instance repGenericListHostedZonesByNameRequest :: Generic ListHostedZonesByNameRequest _
instance showListHostedZonesByNameRequest :: Show ListHostedZonesByNameRequest where
  show = genericShow
instance decodeListHostedZonesByNameRequest :: Decode ListHostedZonesByNameRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHostedZonesByNameRequest :: Encode ListHostedZonesByNameRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListHostedZonesByNameResponse = ListHostedZonesByNameResponse 
  { "HostedZones" :: (HostedZones)
  , "DNSName" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "HostedZoneId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "IsTruncated" :: (PageTruncated)
  , "NextDNSName" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "NextHostedZoneId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListHostedZonesByNameResponse :: Newtype ListHostedZonesByNameResponse _
derive instance repGenericListHostedZonesByNameResponse :: Generic ListHostedZonesByNameResponse _
instance showListHostedZonesByNameResponse :: Show ListHostedZonesByNameResponse where
  show = genericShow
instance decodeListHostedZonesByNameResponse :: Decode ListHostedZonesByNameResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHostedZonesByNameResponse :: Encode ListHostedZonesByNameResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to retrieve a list of the public and private hosted zones that are associated with the current AWS account.</p>
newtype ListHostedZonesRequest = ListHostedZonesRequest 
  { "Marker" :: NullOrUndefined.NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  , "DelegationSetId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  }
derive instance newtypeListHostedZonesRequest :: Newtype ListHostedZonesRequest _
derive instance repGenericListHostedZonesRequest :: Generic ListHostedZonesRequest _
instance showListHostedZonesRequest :: Show ListHostedZonesRequest where
  show = genericShow
instance decodeListHostedZonesRequest :: Decode ListHostedZonesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHostedZonesRequest :: Encode ListHostedZonesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListHostedZonesResponse = ListHostedZonesResponse 
  { "HostedZones" :: (HostedZones)
  , "Marker" :: (PageMarker)
  , "IsTruncated" :: (PageTruncated)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (PageMarker)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListHostedZonesResponse :: Newtype ListHostedZonesResponse _
derive instance repGenericListHostedZonesResponse :: Generic ListHostedZonesResponse _
instance showListHostedZonesResponse :: Show ListHostedZonesResponse where
  show = genericShow
instance decodeListHostedZonesResponse :: Decode ListHostedZonesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListHostedZonesResponse :: Encode ListHostedZonesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListQueryLoggingConfigsRequest = ListQueryLoggingConfigsRequest 
  { "HostedZoneId" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListQueryLoggingConfigsRequest :: Newtype ListQueryLoggingConfigsRequest _
derive instance repGenericListQueryLoggingConfigsRequest :: Generic ListQueryLoggingConfigsRequest _
instance showListQueryLoggingConfigsRequest :: Show ListQueryLoggingConfigsRequest where
  show = genericShow
instance decodeListQueryLoggingConfigsRequest :: Decode ListQueryLoggingConfigsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListQueryLoggingConfigsRequest :: Encode ListQueryLoggingConfigsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListQueryLoggingConfigsResponse = ListQueryLoggingConfigsResponse 
  { "QueryLoggingConfigs" :: (QueryLoggingConfigs)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeListQueryLoggingConfigsResponse :: Newtype ListQueryLoggingConfigsResponse _
derive instance repGenericListQueryLoggingConfigsResponse :: Generic ListQueryLoggingConfigsResponse _
instance showListQueryLoggingConfigsResponse :: Show ListQueryLoggingConfigsResponse where
  show = genericShow
instance decodeListQueryLoggingConfigsResponse :: Decode ListQueryLoggingConfigsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListQueryLoggingConfigsResponse :: Encode ListQueryLoggingConfigsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request for the resource record sets that are associated with a specified hosted zone.</p>
newtype ListResourceRecordSetsRequest = ListResourceRecordSetsRequest 
  { "HostedZoneId" :: (ResourceId)
  , "StartRecordName" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "StartRecordType" :: NullOrUndefined.NullOrUndefined (RRType)
  , "StartRecordIdentifier" :: NullOrUndefined.NullOrUndefined (ResourceRecordSetIdentifier)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListResourceRecordSetsRequest :: Newtype ListResourceRecordSetsRequest _
derive instance repGenericListResourceRecordSetsRequest :: Generic ListResourceRecordSetsRequest _
instance showListResourceRecordSetsRequest :: Show ListResourceRecordSetsRequest where
  show = genericShow
instance decodeListResourceRecordSetsRequest :: Decode ListResourceRecordSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceRecordSetsRequest :: Encode ListResourceRecordSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains list information for the resource record set.</p>
newtype ListResourceRecordSetsResponse = ListResourceRecordSetsResponse 
  { "ResourceRecordSets" :: (ResourceRecordSets)
  , "IsTruncated" :: (PageTruncated)
  , "NextRecordName" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "NextRecordType" :: NullOrUndefined.NullOrUndefined (RRType)
  , "NextRecordIdentifier" :: NullOrUndefined.NullOrUndefined (ResourceRecordSetIdentifier)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListResourceRecordSetsResponse :: Newtype ListResourceRecordSetsResponse _
derive instance repGenericListResourceRecordSetsResponse :: Generic ListResourceRecordSetsResponse _
instance showListResourceRecordSetsResponse :: Show ListResourceRecordSetsResponse where
  show = genericShow
instance decodeListResourceRecordSetsResponse :: Decode ListResourceRecordSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListResourceRecordSetsResponse :: Encode ListResourceRecordSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get a list of the reusable delegation sets that are associated with the current AWS account.</p>
newtype ListReusableDelegationSetsRequest = ListReusableDelegationSetsRequest 
  { "Marker" :: NullOrUndefined.NullOrUndefined (PageMarker)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListReusableDelegationSetsRequest :: Newtype ListReusableDelegationSetsRequest _
derive instance repGenericListReusableDelegationSetsRequest :: Generic ListReusableDelegationSetsRequest _
instance showListReusableDelegationSetsRequest :: Show ListReusableDelegationSetsRequest where
  show = genericShow
instance decodeListReusableDelegationSetsRequest :: Decode ListReusableDelegationSetsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListReusableDelegationSetsRequest :: Encode ListReusableDelegationSetsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the reusable delegation sets that are associated with the current AWS account.</p>
newtype ListReusableDelegationSetsResponse = ListReusableDelegationSetsResponse 
  { "DelegationSets" :: (DelegationSets)
  , "Marker" :: (PageMarker)
  , "IsTruncated" :: (PageTruncated)
  , "NextMarker" :: NullOrUndefined.NullOrUndefined (PageMarker)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListReusableDelegationSetsResponse :: Newtype ListReusableDelegationSetsResponse _
derive instance repGenericListReusableDelegationSetsResponse :: Generic ListReusableDelegationSetsResponse _
instance showListReusableDelegationSetsResponse :: Show ListReusableDelegationSetsResponse where
  show = genericShow
instance decodeListReusableDelegationSetsResponse :: Decode ListReusableDelegationSetsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListReusableDelegationSetsResponse :: Encode ListReusableDelegationSetsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type containing information about a request for a list of the tags that are associated with an individual resource.</p>
newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceType" :: (TagResourceType)
  , "ResourceId" :: (TagResourceId)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _
derive instance repGenericListTagsForResourceRequest :: Generic ListTagsForResourceRequest _
instance showListTagsForResourceRequest :: Show ListTagsForResourceRequest where
  show = genericShow
instance decodeListTagsForResourceRequest :: Decode ListTagsForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceRequest :: Encode ListTagsForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the health checks or hosted zones for which you want to list tags.</p>
newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "ResourceTagSet" :: (ResourceTagSet)
  }
derive instance newtypeListTagsForResourceResponse :: Newtype ListTagsForResourceResponse _
derive instance repGenericListTagsForResourceResponse :: Generic ListTagsForResourceResponse _
instance showListTagsForResourceResponse :: Show ListTagsForResourceResponse where
  show = genericShow
instance decodeListTagsForResourceResponse :: Decode ListTagsForResourceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceResponse :: Encode ListTagsForResourceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the health checks or hosted zones for which you want to list tags.</p>
newtype ListTagsForResourcesRequest = ListTagsForResourcesRequest 
  { "ResourceType" :: (TagResourceType)
  , "ResourceIds" :: (TagResourceIdList)
  }
derive instance newtypeListTagsForResourcesRequest :: Newtype ListTagsForResourcesRequest _
derive instance repGenericListTagsForResourcesRequest :: Generic ListTagsForResourcesRequest _
instance showListTagsForResourcesRequest :: Show ListTagsForResourcesRequest where
  show = genericShow
instance decodeListTagsForResourcesRequest :: Decode ListTagsForResourcesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourcesRequest :: Encode ListTagsForResourcesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type containing tags for the specified resources.</p>
newtype ListTagsForResourcesResponse = ListTagsForResourcesResponse 
  { "ResourceTagSets" :: (ResourceTagSetList)
  }
derive instance newtypeListTagsForResourcesResponse :: Newtype ListTagsForResourcesResponse _
derive instance repGenericListTagsForResourcesResponse :: Generic ListTagsForResourcesResponse _
instance showListTagsForResourcesResponse :: Show ListTagsForResourcesResponse where
  show = genericShow
instance decodeListTagsForResourcesResponse :: Decode ListTagsForResourcesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourcesResponse :: Encode ListTagsForResourcesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the information about the request to list the traffic policies that are associated with the current AWS account.</p>
newtype ListTrafficPoliciesRequest = ListTrafficPoliciesRequest 
  { "TrafficPolicyIdMarker" :: NullOrUndefined.NullOrUndefined (TrafficPolicyId)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPoliciesRequest :: Newtype ListTrafficPoliciesRequest _
derive instance repGenericListTrafficPoliciesRequest :: Generic ListTrafficPoliciesRequest _
instance showListTrafficPoliciesRequest :: Show ListTrafficPoliciesRequest where
  show = genericShow
instance decodeListTrafficPoliciesRequest :: Decode ListTrafficPoliciesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPoliciesRequest :: Encode ListTrafficPoliciesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPoliciesResponse = ListTrafficPoliciesResponse 
  { "TrafficPolicySummaries" :: (TrafficPolicySummaries)
  , "IsTruncated" :: (PageTruncated)
  , "TrafficPolicyIdMarker" :: (TrafficPolicyId)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPoliciesResponse :: Newtype ListTrafficPoliciesResponse _
derive instance repGenericListTrafficPoliciesResponse :: Generic ListTrafficPoliciesResponse _
instance showListTrafficPoliciesResponse :: Show ListTrafficPoliciesResponse where
  show = genericShow
instance decodeListTrafficPoliciesResponse :: Decode ListTrafficPoliciesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPoliciesResponse :: Encode ListTrafficPoliciesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request for the traffic policy instances that you created in a specified hosted zone.</p>
newtype ListTrafficPolicyInstancesByHostedZoneRequest = ListTrafficPolicyInstancesByHostedZoneRequest 
  { "HostedZoneId" :: (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined.NullOrUndefined (RRType)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesByHostedZoneRequest :: Newtype ListTrafficPolicyInstancesByHostedZoneRequest _
derive instance repGenericListTrafficPolicyInstancesByHostedZoneRequest :: Generic ListTrafficPolicyInstancesByHostedZoneRequest _
instance showListTrafficPolicyInstancesByHostedZoneRequest :: Show ListTrafficPolicyInstancesByHostedZoneRequest where
  show = genericShow
instance decodeListTrafficPolicyInstancesByHostedZoneRequest :: Decode ListTrafficPolicyInstancesByHostedZoneRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPolicyInstancesByHostedZoneRequest :: Encode ListTrafficPolicyInstancesByHostedZoneRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPolicyInstancesByHostedZoneResponse = ListTrafficPolicyInstancesByHostedZoneResponse 
  { "TrafficPolicyInstances" :: (TrafficPolicyInstances)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined.NullOrUndefined (RRType)
  , "IsTruncated" :: (PageTruncated)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesByHostedZoneResponse :: Newtype ListTrafficPolicyInstancesByHostedZoneResponse _
derive instance repGenericListTrafficPolicyInstancesByHostedZoneResponse :: Generic ListTrafficPolicyInstancesByHostedZoneResponse _
instance showListTrafficPolicyInstancesByHostedZoneResponse :: Show ListTrafficPolicyInstancesByHostedZoneResponse where
  show = genericShow
instance decodeListTrafficPolicyInstancesByHostedZoneResponse :: Decode ListTrafficPolicyInstancesByHostedZoneResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPolicyInstancesByHostedZoneResponse :: Encode ListTrafficPolicyInstancesByHostedZoneResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the information about the request to list your traffic policy instances.</p>
newtype ListTrafficPolicyInstancesByPolicyRequest = ListTrafficPolicyInstancesByPolicyRequest 
  { "TrafficPolicyId" :: (TrafficPolicyId)
  , "TrafficPolicyVersion" :: (TrafficPolicyVersion)
  , "HostedZoneIdMarker" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined.NullOrUndefined (RRType)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesByPolicyRequest :: Newtype ListTrafficPolicyInstancesByPolicyRequest _
derive instance repGenericListTrafficPolicyInstancesByPolicyRequest :: Generic ListTrafficPolicyInstancesByPolicyRequest _
instance showListTrafficPolicyInstancesByPolicyRequest :: Show ListTrafficPolicyInstancesByPolicyRequest where
  show = genericShow
instance decodeListTrafficPolicyInstancesByPolicyRequest :: Decode ListTrafficPolicyInstancesByPolicyRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPolicyInstancesByPolicyRequest :: Encode ListTrafficPolicyInstancesByPolicyRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPolicyInstancesByPolicyResponse = ListTrafficPolicyInstancesByPolicyResponse 
  { "TrafficPolicyInstances" :: (TrafficPolicyInstances)
  , "HostedZoneIdMarker" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined.NullOrUndefined (RRType)
  , "IsTruncated" :: (PageTruncated)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesByPolicyResponse :: Newtype ListTrafficPolicyInstancesByPolicyResponse _
derive instance repGenericListTrafficPolicyInstancesByPolicyResponse :: Generic ListTrafficPolicyInstancesByPolicyResponse _
instance showListTrafficPolicyInstancesByPolicyResponse :: Show ListTrafficPolicyInstancesByPolicyResponse where
  show = genericShow
instance decodeListTrafficPolicyInstancesByPolicyResponse :: Decode ListTrafficPolicyInstancesByPolicyResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPolicyInstancesByPolicyResponse :: Encode ListTrafficPolicyInstancesByPolicyResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to get information about the traffic policy instances that you created by using the current AWS account.</p>
newtype ListTrafficPolicyInstancesRequest = ListTrafficPolicyInstancesRequest 
  { "HostedZoneIdMarker" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined.NullOrUndefined (RRType)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesRequest :: Newtype ListTrafficPolicyInstancesRequest _
derive instance repGenericListTrafficPolicyInstancesRequest :: Generic ListTrafficPolicyInstancesRequest _
instance showListTrafficPolicyInstancesRequest :: Show ListTrafficPolicyInstancesRequest where
  show = genericShow
instance decodeListTrafficPolicyInstancesRequest :: Decode ListTrafficPolicyInstancesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPolicyInstancesRequest :: Encode ListTrafficPolicyInstancesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPolicyInstancesResponse = ListTrafficPolicyInstancesResponse 
  { "TrafficPolicyInstances" :: (TrafficPolicyInstances)
  , "HostedZoneIdMarker" :: NullOrUndefined.NullOrUndefined (ResourceId)
  , "TrafficPolicyInstanceNameMarker" :: NullOrUndefined.NullOrUndefined (DNSName)
  , "TrafficPolicyInstanceTypeMarker" :: NullOrUndefined.NullOrUndefined (RRType)
  , "IsTruncated" :: (PageTruncated)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyInstancesResponse :: Newtype ListTrafficPolicyInstancesResponse _
derive instance repGenericListTrafficPolicyInstancesResponse :: Generic ListTrafficPolicyInstancesResponse _
instance showListTrafficPolicyInstancesResponse :: Show ListTrafficPolicyInstancesResponse where
  show = genericShow
instance decodeListTrafficPolicyInstancesResponse :: Decode ListTrafficPolicyInstancesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPolicyInstancesResponse :: Encode ListTrafficPolicyInstancesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the information about the request to list your traffic policies.</p>
newtype ListTrafficPolicyVersionsRequest = ListTrafficPolicyVersionsRequest 
  { "Id" :: (TrafficPolicyId)
  , "TrafficPolicyVersionMarker" :: NullOrUndefined.NullOrUndefined (TrafficPolicyVersionMarker)
  , "MaxItems" :: NullOrUndefined.NullOrUndefined (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyVersionsRequest :: Newtype ListTrafficPolicyVersionsRequest _
derive instance repGenericListTrafficPolicyVersionsRequest :: Generic ListTrafficPolicyVersionsRequest _
instance showListTrafficPolicyVersionsRequest :: Show ListTrafficPolicyVersionsRequest where
  show = genericShow
instance decodeListTrafficPolicyVersionsRequest :: Decode ListTrafficPolicyVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPolicyVersionsRequest :: Encode ListTrafficPolicyVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListTrafficPolicyVersionsResponse = ListTrafficPolicyVersionsResponse 
  { "TrafficPolicies" :: (TrafficPolicies)
  , "IsTruncated" :: (PageTruncated)
  , "TrafficPolicyVersionMarker" :: (TrafficPolicyVersionMarker)
  , "MaxItems" :: (PageMaxItems)
  }
derive instance newtypeListTrafficPolicyVersionsResponse :: Newtype ListTrafficPolicyVersionsResponse _
derive instance repGenericListTrafficPolicyVersionsResponse :: Generic ListTrafficPolicyVersionsResponse _
instance showListTrafficPolicyVersionsResponse :: Show ListTrafficPolicyVersionsResponse where
  show = genericShow
instance decodeListTrafficPolicyVersionsResponse :: Decode ListTrafficPolicyVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTrafficPolicyVersionsResponse :: Encode ListTrafficPolicyVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about that can be associated with your hosted zone.</p>
newtype ListVPCAssociationAuthorizationsRequest = ListVPCAssociationAuthorizationsRequest 
  { "HostedZoneId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (MaxResults)
  }
derive instance newtypeListVPCAssociationAuthorizationsRequest :: Newtype ListVPCAssociationAuthorizationsRequest _
derive instance repGenericListVPCAssociationAuthorizationsRequest :: Generic ListVPCAssociationAuthorizationsRequest _
instance showListVPCAssociationAuthorizationsRequest :: Show ListVPCAssociationAuthorizationsRequest where
  show = genericShow
instance decodeListVPCAssociationAuthorizationsRequest :: Decode ListVPCAssociationAuthorizationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVPCAssociationAuthorizationsRequest :: Encode ListVPCAssociationAuthorizationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the request.</p>
newtype ListVPCAssociationAuthorizationsResponse = ListVPCAssociationAuthorizationsResponse 
  { "HostedZoneId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  , "VPCs" :: (VPCs)
  }
derive instance newtypeListVPCAssociationAuthorizationsResponse :: Newtype ListVPCAssociationAuthorizationsResponse _
derive instance repGenericListVPCAssociationAuthorizationsResponse :: Generic ListVPCAssociationAuthorizationsResponse _
instance showListVPCAssociationAuthorizationsResponse :: Show ListVPCAssociationAuthorizationsResponse where
  show = genericShow
instance decodeListVPCAssociationAuthorizationsResponse :: Decode ListVPCAssociationAuthorizationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListVPCAssociationAuthorizationsResponse :: Encode ListVPCAssociationAuthorizationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults String
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MeasureLatency = MeasureLatency Boolean
derive instance newtypeMeasureLatency :: Newtype MeasureLatency _
derive instance repGenericMeasureLatency :: Generic MeasureLatency _
instance showMeasureLatency :: Show MeasureLatency where
  show = genericShow
instance decodeMeasureLatency :: Decode MeasureLatency where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMeasureLatency :: Encode MeasureLatency where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MetricName = MetricName String
derive instance newtypeMetricName :: Newtype MetricName _
derive instance repGenericMetricName :: Generic MetricName _
instance showMetricName :: Show MetricName where
  show = genericShow
instance decodeMetricName :: Decode MetricName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMetricName :: Encode MetricName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Nameserver = Nameserver String
derive instance newtypeNameserver :: Newtype Nameserver _
derive instance repGenericNameserver :: Generic Nameserver _
instance showNameserver :: Show Nameserver where
  show = genericShow
instance decodeNameserver :: Decode Nameserver where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNameserver :: Encode Nameserver where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Namespace = Namespace String
derive instance newtypeNamespace :: Newtype Namespace _
derive instance repGenericNamespace :: Generic Namespace _
instance showNamespace :: Show Namespace where
  show = genericShow
instance decodeNamespace :: Decode Namespace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNamespace :: Encode Namespace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A change with the specified change ID does not exist.</p>
newtype NoSuchChange = NoSuchChange 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchChange :: Newtype NoSuchChange _
derive instance repGenericNoSuchChange :: Generic NoSuchChange _
instance showNoSuchChange :: Show NoSuchChange where
  show = genericShow
instance decodeNoSuchChange :: Decode NoSuchChange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchChange :: Encode NoSuchChange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no CloudWatch Logs log group with the specified ARN.</p>
newtype NoSuchCloudWatchLogsLogGroup = NoSuchCloudWatchLogsLogGroup 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchCloudWatchLogsLogGroup :: Newtype NoSuchCloudWatchLogsLogGroup _
derive instance repGenericNoSuchCloudWatchLogsLogGroup :: Generic NoSuchCloudWatchLogsLogGroup _
instance showNoSuchCloudWatchLogsLogGroup :: Show NoSuchCloudWatchLogsLogGroup where
  show = genericShow
instance decodeNoSuchCloudWatchLogsLogGroup :: Decode NoSuchCloudWatchLogsLogGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchCloudWatchLogsLogGroup :: Encode NoSuchCloudWatchLogsLogGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A reusable delegation set with the specified ID does not exist.</p>
newtype NoSuchDelegationSet = NoSuchDelegationSet 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchDelegationSet :: Newtype NoSuchDelegationSet _
derive instance repGenericNoSuchDelegationSet :: Generic NoSuchDelegationSet _
instance showNoSuchDelegationSet :: Show NoSuchDelegationSet where
  show = genericShow
instance decodeNoSuchDelegationSet :: Decode NoSuchDelegationSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchDelegationSet :: Encode NoSuchDelegationSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Amazon Route 53 doesn't support the specified geolocation.</p>
newtype NoSuchGeoLocation = NoSuchGeoLocation 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchGeoLocation :: Newtype NoSuchGeoLocation _
derive instance repGenericNoSuchGeoLocation :: Generic NoSuchGeoLocation _
instance showNoSuchGeoLocation :: Show NoSuchGeoLocation where
  show = genericShow
instance decodeNoSuchGeoLocation :: Decode NoSuchGeoLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchGeoLocation :: Encode NoSuchGeoLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>No health check exists with the ID that you specified in the <code>DeleteHealthCheck</code> request.</p>
newtype NoSuchHealthCheck = NoSuchHealthCheck 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchHealthCheck :: Newtype NoSuchHealthCheck _
derive instance repGenericNoSuchHealthCheck :: Generic NoSuchHealthCheck _
instance showNoSuchHealthCheck :: Show NoSuchHealthCheck where
  show = genericShow
instance decodeNoSuchHealthCheck :: Decode NoSuchHealthCheck where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchHealthCheck :: Encode NoSuchHealthCheck where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>No hosted zone exists with the ID that you specified.</p>
newtype NoSuchHostedZone = NoSuchHostedZone 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchHostedZone :: Newtype NoSuchHostedZone _
derive instance repGenericNoSuchHostedZone :: Generic NoSuchHostedZone _
instance showNoSuchHostedZone :: Show NoSuchHostedZone where
  show = genericShow
instance decodeNoSuchHostedZone :: Decode NoSuchHostedZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchHostedZone :: Encode NoSuchHostedZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no DNS query logging configuration with the specified ID.</p>
newtype NoSuchQueryLoggingConfig = NoSuchQueryLoggingConfig 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchQueryLoggingConfig :: Newtype NoSuchQueryLoggingConfig _
derive instance repGenericNoSuchQueryLoggingConfig :: Generic NoSuchQueryLoggingConfig _
instance showNoSuchQueryLoggingConfig :: Show NoSuchQueryLoggingConfig where
  show = genericShow
instance decodeNoSuchQueryLoggingConfig :: Decode NoSuchQueryLoggingConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchQueryLoggingConfig :: Encode NoSuchQueryLoggingConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>No traffic policy exists with the specified ID.</p>
newtype NoSuchTrafficPolicy = NoSuchTrafficPolicy 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchTrafficPolicy :: Newtype NoSuchTrafficPolicy _
derive instance repGenericNoSuchTrafficPolicy :: Generic NoSuchTrafficPolicy _
instance showNoSuchTrafficPolicy :: Show NoSuchTrafficPolicy where
  show = genericShow
instance decodeNoSuchTrafficPolicy :: Decode NoSuchTrafficPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchTrafficPolicy :: Encode NoSuchTrafficPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>No traffic policy instance exists with the specified ID.</p>
newtype NoSuchTrafficPolicyInstance = NoSuchTrafficPolicyInstance 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoSuchTrafficPolicyInstance :: Newtype NoSuchTrafficPolicyInstance _
derive instance repGenericNoSuchTrafficPolicyInstance :: Generic NoSuchTrafficPolicyInstance _
instance showNoSuchTrafficPolicyInstance :: Show NoSuchTrafficPolicyInstance where
  show = genericShow
instance decodeNoSuchTrafficPolicyInstance :: Decode NoSuchTrafficPolicyInstance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoSuchTrafficPolicyInstance :: Encode NoSuchTrafficPolicyInstance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Nonce = Nonce String
derive instance newtypeNonce :: Newtype Nonce _
derive instance repGenericNonce :: Generic Nonce _
instance showNonce :: Show Nonce where
  show = genericShow
instance decodeNonce :: Decode Nonce where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonce :: Encode Nonce where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Associating the specified VPC with the specified hosted zone has not been authorized.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _
derive instance repGenericNotAuthorizedException :: Generic NotAuthorizedException _
instance showNotAuthorizedException :: Show NotAuthorizedException where
  show = genericShow
instance decodeNotAuthorizedException :: Decode NotAuthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotAuthorizedException :: Encode NotAuthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageMarker = PageMarker String
derive instance newtypePageMarker :: Newtype PageMarker _
derive instance repGenericPageMarker :: Generic PageMarker _
instance showPageMarker :: Show PageMarker where
  show = genericShow
instance decodePageMarker :: Decode PageMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageMarker :: Encode PageMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageMaxItems = PageMaxItems String
derive instance newtypePageMaxItems :: Newtype PageMaxItems _
derive instance repGenericPageMaxItems :: Generic PageMaxItems _
instance showPageMaxItems :: Show PageMaxItems where
  show = genericShow
instance decodePageMaxItems :: Decode PageMaxItems where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageMaxItems :: Encode PageMaxItems where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageTruncated = PageTruncated Boolean
derive instance newtypePageTruncated :: Newtype PageTruncated _
derive instance repGenericPageTruncated :: Generic PageTruncated _
instance showPageTruncated :: Show PageTruncated where
  show = genericShow
instance decodePageTruncated :: Decode PageTruncated where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageTruncated :: Encode PageTruncated where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _
derive instance repGenericPaginationToken :: Generic PaginationToken _
instance showPaginationToken :: Show PaginationToken where
  show = genericShow
instance decodePaginationToken :: Decode PaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationToken :: Encode PaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Period = Period Int
derive instance newtypePeriod :: Newtype Period _
derive instance repGenericPeriod :: Generic Period _
instance showPeriod :: Show Period where
  show = genericShow
instance decodePeriod :: Decode Period where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePeriod :: Encode Period where
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


-- | <p>If Amazon Route 53 can't process a request before the next request arrives, it will reject subsequent requests for the same hosted zone and return an <code>HTTP 400 error</code> (<code>Bad request</code>). If Amazon Route 53 returns this error repeatedly for the same request, we recommend that you wait, in intervals of increasing duration, before you try the request again.</p>
newtype PriorRequestNotComplete = PriorRequestNotComplete 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypePriorRequestNotComplete :: Newtype PriorRequestNotComplete _
derive instance repGenericPriorRequestNotComplete :: Generic PriorRequestNotComplete _
instance showPriorRequestNotComplete :: Show PriorRequestNotComplete where
  show = genericShow
instance decodePriorRequestNotComplete :: Decode PriorRequestNotComplete where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePriorRequestNotComplete :: Encode PriorRequestNotComplete where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You're trying to associate a VPC with a public hosted zone. Amazon Route 53 doesn't support associating a VPC with a public hosted zone.</p>
newtype PublicZoneVPCAssociation = PublicZoneVPCAssociation 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypePublicZoneVPCAssociation :: Newtype PublicZoneVPCAssociation _
derive instance repGenericPublicZoneVPCAssociation :: Generic PublicZoneVPCAssociation _
instance showPublicZoneVPCAssociation :: Show PublicZoneVPCAssociation where
  show = genericShow
instance decodePublicZoneVPCAssociation :: Decode PublicZoneVPCAssociation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePublicZoneVPCAssociation :: Encode PublicZoneVPCAssociation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about a configuration for DNS query logging.</p>
newtype QueryLoggingConfig = QueryLoggingConfig 
  { "Id" :: (QueryLoggingConfigId)
  , "HostedZoneId" :: (ResourceId)
  , "CloudWatchLogsLogGroupArn" :: (CloudWatchLogsLogGroupArn)
  }
derive instance newtypeQueryLoggingConfig :: Newtype QueryLoggingConfig _
derive instance repGenericQueryLoggingConfig :: Generic QueryLoggingConfig _
instance showQueryLoggingConfig :: Show QueryLoggingConfig where
  show = genericShow
instance decodeQueryLoggingConfig :: Decode QueryLoggingConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryLoggingConfig :: Encode QueryLoggingConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You can create only one query logging configuration for a hosted zone, and a query logging configuration already exists for this hosted zone.</p>
newtype QueryLoggingConfigAlreadyExists = QueryLoggingConfigAlreadyExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeQueryLoggingConfigAlreadyExists :: Newtype QueryLoggingConfigAlreadyExists _
derive instance repGenericQueryLoggingConfigAlreadyExists :: Generic QueryLoggingConfigAlreadyExists _
instance showQueryLoggingConfigAlreadyExists :: Show QueryLoggingConfigAlreadyExists where
  show = genericShow
instance decodeQueryLoggingConfigAlreadyExists :: Decode QueryLoggingConfigAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryLoggingConfigAlreadyExists :: Encode QueryLoggingConfigAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryLoggingConfigId = QueryLoggingConfigId String
derive instance newtypeQueryLoggingConfigId :: Newtype QueryLoggingConfigId _
derive instance repGenericQueryLoggingConfigId :: Generic QueryLoggingConfigId _
instance showQueryLoggingConfigId :: Show QueryLoggingConfigId where
  show = genericShow
instance decodeQueryLoggingConfigId :: Decode QueryLoggingConfigId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryLoggingConfigId :: Encode QueryLoggingConfigId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryLoggingConfigs = QueryLoggingConfigs (Array QueryLoggingConfig)
derive instance newtypeQueryLoggingConfigs :: Newtype QueryLoggingConfigs _
derive instance repGenericQueryLoggingConfigs :: Generic QueryLoggingConfigs _
instance showQueryLoggingConfigs :: Show QueryLoggingConfigs where
  show = genericShow
instance decodeQueryLoggingConfigs :: Decode QueryLoggingConfigs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryLoggingConfigs :: Encode QueryLoggingConfigs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RData = RData String
derive instance newtypeRData :: Newtype RData _
derive instance repGenericRData :: Generic RData _
instance showRData :: Show RData where
  show = genericShow
instance decodeRData :: Decode RData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRData :: Encode RData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RRType = RRType String
derive instance newtypeRRType :: Newtype RRType _
derive instance repGenericRRType :: Generic RRType _
instance showRRType :: Show RRType where
  show = genericShow
instance decodeRRType :: Decode RRType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRRType :: Encode RRType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordData = RecordData (Array RecordDataEntry)
derive instance newtypeRecordData :: Newtype RecordData _
derive instance repGenericRecordData :: Generic RecordData _
instance showRecordData :: Show RecordData where
  show = genericShow
instance decodeRecordData :: Decode RecordData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordData :: Encode RecordData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A value that Amazon Route 53 returned for this resource record set. A <code>RecordDataEntry</code> element is one of the following:</p> <ul> <li> <p>For non-alias resource record sets, a <code>RecordDataEntry</code> element contains one value in the resource record set. If the resource record set contains multiple values, the response includes one <code>RecordDataEntry</code> element for each value.</p> </li> <li> <p>For multiple resource record sets that have the same name and type, which includes weighted, latency, geolocation, and failover, a <code>RecordDataEntry</code> element contains the value from the appropriate resource record set based on the request.</p> </li> <li> <p>For alias resource record sets that refer to AWS resources other than another resource record set, the <code>RecordDataEntry</code> element contains an IP address or a domain name for the AWS resource, depending on the type of resource.</p> </li> <li> <p>For alias resource record sets that refer to other resource record sets, a <code>RecordDataEntry</code> element contains one value from the referenced resource record set. If the referenced resource record set contains multiple values, the response includes one <code>RecordDataEntry</code> element for each value.</p> </li> </ul>
newtype RecordDataEntry = RecordDataEntry String
derive instance newtypeRecordDataEntry :: Newtype RecordDataEntry _
derive instance repGenericRecordDataEntry :: Generic RecordDataEntry _
instance showRecordDataEntry :: Show RecordDataEntry where
  show = genericShow
instance decodeRecordDataEntry :: Decode RecordDataEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordDataEntry :: Encode RecordDataEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RequestInterval = RequestInterval Int
derive instance newtypeRequestInterval :: Newtype RequestInterval _
derive instance repGenericRequestInterval :: Generic RequestInterval _
instance showRequestInterval :: Show RequestInterval where
  show = genericShow
instance decodeRequestInterval :: Decode RequestInterval where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestInterval :: Encode RequestInterval where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResettableElementName = ResettableElementName String
derive instance newtypeResettableElementName :: Newtype ResettableElementName _
derive instance repGenericResettableElementName :: Generic ResettableElementName _
instance showResettableElementName :: Show ResettableElementName where
  show = genericShow
instance decodeResettableElementName :: Decode ResettableElementName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResettableElementName :: Encode ResettableElementName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResettableElementNameList = ResettableElementNameList (Array ResettableElementName)
derive instance newtypeResettableElementNameList :: Newtype ResettableElementNameList _
derive instance repGenericResettableElementNameList :: Generic ResettableElementNameList _
instance showResettableElementNameList :: Show ResettableElementNameList where
  show = genericShow
instance decodeResettableElementNameList :: Decode ResettableElementNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResettableElementNameList :: Encode ResettableElementNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceDescription = ResourceDescription String
derive instance newtypeResourceDescription :: Newtype ResourceDescription _
derive instance repGenericResourceDescription :: Generic ResourceDescription _
instance showResourceDescription :: Show ResourceDescription where
  show = genericShow
instance decodeResourceDescription :: Decode ResourceDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceDescription :: Encode ResourceDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _
derive instance repGenericResourceId :: Generic ResourceId _
instance showResourceId :: Show ResourceId where
  show = genericShow
instance decodeResourceId :: Decode ResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceId :: Encode ResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourcePath = ResourcePath String
derive instance newtypeResourcePath :: Newtype ResourcePath _
derive instance repGenericResourcePath :: Generic ResourcePath _
instance showResourcePath :: Show ResourcePath where
  show = genericShow
instance decodeResourcePath :: Decode ResourcePath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourcePath :: Encode ResourcePath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information specific to the resource record.</p> <note> <p>If you're creating an alias resource record set, omit <code>ResourceRecord</code>.</p> </note>
newtype ResourceRecord = ResourceRecord 
  { "Value" :: (RData)
  }
derive instance newtypeResourceRecord :: Newtype ResourceRecord _
derive instance repGenericResourceRecord :: Generic ResourceRecord _
instance showResourceRecord :: Show ResourceRecord where
  show = genericShow
instance decodeResourceRecord :: Decode ResourceRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecord :: Encode ResourceRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the resource record set to create or delete.</p>
newtype ResourceRecordSet = ResourceRecordSet 
  { "Name" :: (DNSName)
  , "Type" :: (RRType)
  , "SetIdentifier" :: NullOrUndefined.NullOrUndefined (ResourceRecordSetIdentifier)
  , "Weight" :: NullOrUndefined.NullOrUndefined (ResourceRecordSetWeight)
  , "Region" :: NullOrUndefined.NullOrUndefined (ResourceRecordSetRegion)
  , "GeoLocation" :: NullOrUndefined.NullOrUndefined (GeoLocation)
  , "Failover" :: NullOrUndefined.NullOrUndefined (ResourceRecordSetFailover)
  , "MultiValueAnswer" :: NullOrUndefined.NullOrUndefined (ResourceRecordSetMultiValueAnswer)
  , "TTL" :: NullOrUndefined.NullOrUndefined (TTL)
  , "ResourceRecords" :: NullOrUndefined.NullOrUndefined (ResourceRecords)
  , "AliasTarget" :: NullOrUndefined.NullOrUndefined (AliasTarget)
  , "HealthCheckId" :: NullOrUndefined.NullOrUndefined (HealthCheckId)
  , "TrafficPolicyInstanceId" :: NullOrUndefined.NullOrUndefined (TrafficPolicyInstanceId)
  }
derive instance newtypeResourceRecordSet :: Newtype ResourceRecordSet _
derive instance repGenericResourceRecordSet :: Generic ResourceRecordSet _
instance showResourceRecordSet :: Show ResourceRecordSet where
  show = genericShow
instance decodeResourceRecordSet :: Decode ResourceRecordSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecordSet :: Encode ResourceRecordSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceRecordSetFailover = ResourceRecordSetFailover String
derive instance newtypeResourceRecordSetFailover :: Newtype ResourceRecordSetFailover _
derive instance repGenericResourceRecordSetFailover :: Generic ResourceRecordSetFailover _
instance showResourceRecordSetFailover :: Show ResourceRecordSetFailover where
  show = genericShow
instance decodeResourceRecordSetFailover :: Decode ResourceRecordSetFailover where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecordSetFailover :: Encode ResourceRecordSetFailover where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceRecordSetIdentifier = ResourceRecordSetIdentifier String
derive instance newtypeResourceRecordSetIdentifier :: Newtype ResourceRecordSetIdentifier _
derive instance repGenericResourceRecordSetIdentifier :: Generic ResourceRecordSetIdentifier _
instance showResourceRecordSetIdentifier :: Show ResourceRecordSetIdentifier where
  show = genericShow
instance decodeResourceRecordSetIdentifier :: Decode ResourceRecordSetIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecordSetIdentifier :: Encode ResourceRecordSetIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceRecordSetMultiValueAnswer = ResourceRecordSetMultiValueAnswer Boolean
derive instance newtypeResourceRecordSetMultiValueAnswer :: Newtype ResourceRecordSetMultiValueAnswer _
derive instance repGenericResourceRecordSetMultiValueAnswer :: Generic ResourceRecordSetMultiValueAnswer _
instance showResourceRecordSetMultiValueAnswer :: Show ResourceRecordSetMultiValueAnswer where
  show = genericShow
instance decodeResourceRecordSetMultiValueAnswer :: Decode ResourceRecordSetMultiValueAnswer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecordSetMultiValueAnswer :: Encode ResourceRecordSetMultiValueAnswer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceRecordSetRegion = ResourceRecordSetRegion String
derive instance newtypeResourceRecordSetRegion :: Newtype ResourceRecordSetRegion _
derive instance repGenericResourceRecordSetRegion :: Generic ResourceRecordSetRegion _
instance showResourceRecordSetRegion :: Show ResourceRecordSetRegion where
  show = genericShow
instance decodeResourceRecordSetRegion :: Decode ResourceRecordSetRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecordSetRegion :: Encode ResourceRecordSetRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceRecordSetWeight = ResourceRecordSetWeight Number
derive instance newtypeResourceRecordSetWeight :: Newtype ResourceRecordSetWeight _
derive instance repGenericResourceRecordSetWeight :: Generic ResourceRecordSetWeight _
instance showResourceRecordSetWeight :: Show ResourceRecordSetWeight where
  show = genericShow
instance decodeResourceRecordSetWeight :: Decode ResourceRecordSetWeight where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecordSetWeight :: Encode ResourceRecordSetWeight where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceRecordSets = ResourceRecordSets (Array ResourceRecordSet)
derive instance newtypeResourceRecordSets :: Newtype ResourceRecordSets _
derive instance repGenericResourceRecordSets :: Generic ResourceRecordSets _
instance showResourceRecordSets :: Show ResourceRecordSets where
  show = genericShow
instance decodeResourceRecordSets :: Decode ResourceRecordSets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecordSets :: Encode ResourceRecordSets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceRecords = ResourceRecords (Array ResourceRecord)
derive instance newtypeResourceRecords :: Newtype ResourceRecords _
derive instance repGenericResourceRecords :: Generic ResourceRecords _
instance showResourceRecords :: Show ResourceRecords where
  show = genericShow
instance decodeResourceRecords :: Decode ResourceRecords where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceRecords :: Encode ResourceRecords where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type containing a resource and its associated tags.</p>
newtype ResourceTagSet = ResourceTagSet 
  { "ResourceType" :: NullOrUndefined.NullOrUndefined (TagResourceType)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (TagResourceId)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeResourceTagSet :: Newtype ResourceTagSet _
derive instance repGenericResourceTagSet :: Generic ResourceTagSet _
instance showResourceTagSet :: Show ResourceTagSet where
  show = genericShow
instance decodeResourceTagSet :: Decode ResourceTagSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTagSet :: Encode ResourceTagSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceTagSetList = ResourceTagSetList (Array ResourceTagSet)
derive instance newtypeResourceTagSetList :: Newtype ResourceTagSetList _
derive instance repGenericResourceTagSetList :: Generic ResourceTagSetList _
instance showResourceTagSetList :: Show ResourceTagSetList where
  show = genericShow
instance decodeResourceTagSetList :: Decode ResourceTagSetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceTagSetList :: Encode ResourceTagSetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceURI = ResourceURI String
derive instance newtypeResourceURI :: Newtype ResourceURI _
derive instance repGenericResourceURI :: Generic ResourceURI _
instance showResourceURI :: Show ResourceURI where
  show = genericShow
instance decodeResourceURI :: Decode ResourceURI where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceURI :: Encode ResourceURI where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the type of limit that you specified in the request and the current value for that limit.</p>
newtype ReusableDelegationSetLimit = ReusableDelegationSetLimit 
  { "Type" :: (ReusableDelegationSetLimitType)
  , "Value" :: (LimitValue)
  }
derive instance newtypeReusableDelegationSetLimit :: Newtype ReusableDelegationSetLimit _
derive instance repGenericReusableDelegationSetLimit :: Generic ReusableDelegationSetLimit _
instance showReusableDelegationSetLimit :: Show ReusableDelegationSetLimit where
  show = genericShow
instance decodeReusableDelegationSetLimit :: Decode ReusableDelegationSetLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReusableDelegationSetLimit :: Encode ReusableDelegationSetLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReusableDelegationSetLimitType = ReusableDelegationSetLimitType String
derive instance newtypeReusableDelegationSetLimitType :: Newtype ReusableDelegationSetLimitType _
derive instance repGenericReusableDelegationSetLimitType :: Generic ReusableDelegationSetLimitType _
instance showReusableDelegationSetLimitType :: Show ReusableDelegationSetLimitType where
  show = genericShow
instance decodeReusableDelegationSetLimitType :: Decode ReusableDelegationSetLimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReusableDelegationSetLimitType :: Encode ReusableDelegationSetLimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchString = SearchString String
derive instance newtypeSearchString :: Newtype SearchString _
derive instance repGenericSearchString :: Generic SearchString _
instance showSearchString :: Show SearchString where
  show = genericShow
instance decodeSearchString :: Decode SearchString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchString :: Encode SearchString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ServicePrincipal = ServicePrincipal String
derive instance newtypeServicePrincipal :: Newtype ServicePrincipal _
derive instance repGenericServicePrincipal :: Generic ServicePrincipal _
instance showServicePrincipal :: Show ServicePrincipal where
  show = genericShow
instance decodeServicePrincipal :: Decode ServicePrincipal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServicePrincipal :: Encode ServicePrincipal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Statistic = Statistic String
derive instance newtypeStatistic :: Newtype Statistic _
derive instance repGenericStatistic :: Generic Statistic _
instance showStatistic :: Show Statistic where
  show = genericShow
instance decodeStatistic :: Decode Statistic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatistic :: Encode Statistic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _
derive instance repGenericStatus :: Generic Status _
instance showStatus :: Show Status where
  show = genericShow
instance decodeStatus :: Decode Status where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatus :: Encode Status where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the status that one Amazon Route 53 health checker reports and the time of the health check.</p>
newtype StatusReport = StatusReport 
  { "Status" :: NullOrUndefined.NullOrUndefined (Status)
  , "CheckedTime" :: NullOrUndefined.NullOrUndefined (TimeStamp)
  }
derive instance newtypeStatusReport :: Newtype StatusReport _
derive instance repGenericStatusReport :: Generic StatusReport _
instance showStatusReport :: Show StatusReport where
  show = genericShow
instance decodeStatusReport :: Decode StatusReport where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusReport :: Encode StatusReport where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetMask = SubnetMask String
derive instance newtypeSubnetMask :: Newtype SubnetMask _
derive instance repGenericSubnetMask :: Generic SubnetMask _
instance showSubnetMask :: Show SubnetMask where
  show = genericShow
instance decodeSubnetMask :: Decode SubnetMask where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetMask :: Encode SubnetMask where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TTL = TTL Number
derive instance newtypeTTL :: Newtype TTL _
derive instance repGenericTTL :: Generic TTL _
instance showTTL :: Show TTL where
  show = genericShow
instance decodeTTL :: Decode TTL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTTL :: Encode TTL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about a tag that you want to add or edit for the specified health check or hosted zone.</p>
newtype Tag = Tag 
  { "Key" :: NullOrUndefined.NullOrUndefined (TagKey)
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


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
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


newtype TagResourceId = TagResourceId String
derive instance newtypeTagResourceId :: Newtype TagResourceId _
derive instance repGenericTagResourceId :: Generic TagResourceId _
instance showTagResourceId :: Show TagResourceId where
  show = genericShow
instance decodeTagResourceId :: Decode TagResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceId :: Encode TagResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagResourceIdList = TagResourceIdList (Array TagResourceId)
derive instance newtypeTagResourceIdList :: Newtype TagResourceIdList _
derive instance repGenericTagResourceIdList :: Generic TagResourceIdList _
instance showTagResourceIdList :: Show TagResourceIdList where
  show = genericShow
instance decodeTagResourceIdList :: Decode TagResourceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceIdList :: Encode TagResourceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagResourceType = TagResourceType String
derive instance newtypeTagResourceType :: Newtype TagResourceType _
derive instance repGenericTagResourceType :: Generic TagResourceType _
instance showTagResourceType :: Show TagResourceType where
  show = genericShow
instance decodeTagResourceType :: Decode TagResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagResourceType :: Encode TagResourceType where
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


-- | <p>Gets the value that Amazon Route 53 returns in response to a DNS request for a specified record name and type. You can optionally specify the IP address of a DNS resolver, an EDNS0 client subnet IP address, and a subnet mask. </p>
newtype TestDNSAnswerRequest = TestDNSAnswerRequest 
  { "HostedZoneId" :: (ResourceId)
  , "RecordName" :: (DNSName)
  , "RecordType" :: (RRType)
  , "ResolverIP" :: NullOrUndefined.NullOrUndefined (IPAddress)
  , "EDNS0ClientSubnetIP" :: NullOrUndefined.NullOrUndefined (IPAddress)
  , "EDNS0ClientSubnetMask" :: NullOrUndefined.NullOrUndefined (SubnetMask)
  }
derive instance newtypeTestDNSAnswerRequest :: Newtype TestDNSAnswerRequest _
derive instance repGenericTestDNSAnswerRequest :: Generic TestDNSAnswerRequest _
instance showTestDNSAnswerRequest :: Show TestDNSAnswerRequest where
  show = genericShow
instance decodeTestDNSAnswerRequest :: Decode TestDNSAnswerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestDNSAnswerRequest :: Encode TestDNSAnswerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


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
derive instance repGenericTestDNSAnswerResponse :: Generic TestDNSAnswerResponse _
instance showTestDNSAnswerResponse :: Show TestDNSAnswerResponse where
  show = genericShow
instance decodeTestDNSAnswerResponse :: Decode TestDNSAnswerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTestDNSAnswerResponse :: Encode TestDNSAnswerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Threshold = Threshold Number
derive instance newtypeThreshold :: Newtype Threshold _
derive instance repGenericThreshold :: Generic Threshold _
instance showThreshold :: Show Threshold where
  show = genericShow
instance decodeThreshold :: Decode Threshold where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThreshold :: Encode Threshold where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The limit on the number of requests per second was exceeded.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _
derive instance repGenericThrottlingException :: Generic ThrottlingException _
instance showThrottlingException :: Show ThrottlingException where
  show = genericShow
instance decodeThrottlingException :: Decode ThrottlingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottlingException :: Encode ThrottlingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeStamp = TimeStamp Number
derive instance newtypeTimeStamp :: Newtype TimeStamp _
derive instance repGenericTimeStamp :: Generic TimeStamp _
instance showTimeStamp :: Show TimeStamp where
  show = genericShow
instance decodeTimeStamp :: Decode TimeStamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeStamp :: Encode TimeStamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This health check can't be created because the current account has reached the limit on the number of active health checks.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>For information about how to get the current limit for an account, see <a>GetAccountLimit</a>. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p> <p>You have reached the maximum number of active health checks for an AWS account. To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype TooManyHealthChecks = TooManyHealthChecks 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyHealthChecks :: Newtype TooManyHealthChecks _
derive instance repGenericTooManyHealthChecks :: Generic TooManyHealthChecks _
instance showTooManyHealthChecks :: Show TooManyHealthChecks where
  show = genericShow
instance decodeTooManyHealthChecks :: Decode TooManyHealthChecks where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyHealthChecks :: Encode TooManyHealthChecks where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This operation can't be completed either because the current account has reached the limit on the number of hosted zones or because you've reached the limit on the number of hosted zones that can be associated with a reusable delegation set.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>To get the current limit on hosted zones that can be created by an account, see <a>GetAccountLimit</a>.</p> <p>To get the current limit on hosted zones that can be associated with a reusable delegation set, see <a>GetReusableDelegationSetLimit</a>.</p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype TooManyHostedZones = TooManyHostedZones 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyHostedZones :: Newtype TooManyHostedZones _
derive instance repGenericTooManyHostedZones :: Generic TooManyHostedZones _
instance showTooManyHostedZones :: Show TooManyHostedZones where
  show = genericShow
instance decodeTooManyHostedZones :: Decode TooManyHostedZones where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyHostedZones :: Encode TooManyHostedZones where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This traffic policy can't be created because the current account has reached the limit on the number of traffic policies.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>To get the current limit for an account, see <a>GetAccountLimit</a>. </p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype TooManyTrafficPolicies = TooManyTrafficPolicies 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyTrafficPolicies :: Newtype TooManyTrafficPolicies _
derive instance repGenericTooManyTrafficPolicies :: Generic TooManyTrafficPolicies _
instance showTooManyTrafficPolicies :: Show TooManyTrafficPolicies where
  show = genericShow
instance decodeTooManyTrafficPolicies :: Decode TooManyTrafficPolicies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTrafficPolicies :: Encode TooManyTrafficPolicies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This traffic policy instance can't be created because the current account has reached the limit on the number of traffic policy instances.</p> <p>For information about default limits, see <a href="http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html">Limits</a> in the <i>Amazon Route 53 Developer Guide</i>.</p> <p>For information about how to get the current limit for an account, see <a>GetAccountLimit</a>.</p> <p>To request a higher limit, <a href="http://aws.amazon.com/route53-request">create a case</a> with the AWS Support Center.</p>
newtype TooManyTrafficPolicyInstances = TooManyTrafficPolicyInstances 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyTrafficPolicyInstances :: Newtype TooManyTrafficPolicyInstances _
derive instance repGenericTooManyTrafficPolicyInstances :: Generic TooManyTrafficPolicyInstances _
instance showTooManyTrafficPolicyInstances :: Show TooManyTrafficPolicyInstances where
  show = genericShow
instance decodeTooManyTrafficPolicyInstances :: Decode TooManyTrafficPolicyInstances where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTrafficPolicyInstances :: Encode TooManyTrafficPolicyInstances where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This traffic policy version can't be created because you've reached the limit of 1000 on the number of versions that you can create for the current traffic policy.</p> <p>To create more traffic policy versions, you can use <a>GetTrafficPolicy</a> to get the traffic policy document for a specified traffic policy version, and then use <a>CreateTrafficPolicy</a> to create a new traffic policy using the traffic policy document.</p>
newtype TooManyTrafficPolicyVersionsForCurrentPolicy = TooManyTrafficPolicyVersionsForCurrentPolicy 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyTrafficPolicyVersionsForCurrentPolicy :: Newtype TooManyTrafficPolicyVersionsForCurrentPolicy _
derive instance repGenericTooManyTrafficPolicyVersionsForCurrentPolicy :: Generic TooManyTrafficPolicyVersionsForCurrentPolicy _
instance showTooManyTrafficPolicyVersionsForCurrentPolicy :: Show TooManyTrafficPolicyVersionsForCurrentPolicy where
  show = genericShow
instance decodeTooManyTrafficPolicyVersionsForCurrentPolicy :: Decode TooManyTrafficPolicyVersionsForCurrentPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyTrafficPolicyVersionsForCurrentPolicy :: Encode TooManyTrafficPolicyVersionsForCurrentPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've created the maximum number of authorizations that can be created for the specified hosted zone. To authorize another VPC to be associated with the hosted zone, submit a <code>DeleteVPCAssociationAuthorization</code> request to remove an existing authorization. To get a list of existing authorizations, submit a <code>ListVPCAssociationAuthorizations</code> request.</p>
newtype TooManyVPCAssociationAuthorizations = TooManyVPCAssociationAuthorizations 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyVPCAssociationAuthorizations :: Newtype TooManyVPCAssociationAuthorizations _
derive instance repGenericTooManyVPCAssociationAuthorizations :: Generic TooManyVPCAssociationAuthorizations _
instance showTooManyVPCAssociationAuthorizations :: Show TooManyVPCAssociationAuthorizations where
  show = genericShow
instance decodeTooManyVPCAssociationAuthorizations :: Decode TooManyVPCAssociationAuthorizations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyVPCAssociationAuthorizations :: Encode TooManyVPCAssociationAuthorizations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicies = TrafficPolicies (Array TrafficPolicy)
derive instance newtypeTrafficPolicies :: Newtype TrafficPolicies _
derive instance repGenericTrafficPolicies :: Generic TrafficPolicies _
instance showTrafficPolicies :: Show TrafficPolicies where
  show = genericShow
instance decodeTrafficPolicies :: Decode TrafficPolicies where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicies :: Encode TrafficPolicies where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains settings for a traffic policy.</p>
newtype TrafficPolicy = TrafficPolicy 
  { "Id" :: (TrafficPolicyId)
  , "Version" :: (TrafficPolicyVersion)
  , "Name" :: (TrafficPolicyName)
  , "Type" :: (RRType)
  , "Document" :: (TrafficPolicyDocument)
  , "Comment" :: NullOrUndefined.NullOrUndefined (TrafficPolicyComment)
  }
derive instance newtypeTrafficPolicy :: Newtype TrafficPolicy _
derive instance repGenericTrafficPolicy :: Generic TrafficPolicy _
instance showTrafficPolicy :: Show TrafficPolicy where
  show = genericShow
instance decodeTrafficPolicy :: Decode TrafficPolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicy :: Encode TrafficPolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A traffic policy that has the same value for <code>Name</code> already exists.</p>
newtype TrafficPolicyAlreadyExists = TrafficPolicyAlreadyExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTrafficPolicyAlreadyExists :: Newtype TrafficPolicyAlreadyExists _
derive instance repGenericTrafficPolicyAlreadyExists :: Generic TrafficPolicyAlreadyExists _
instance showTrafficPolicyAlreadyExists :: Show TrafficPolicyAlreadyExists where
  show = genericShow
instance decodeTrafficPolicyAlreadyExists :: Decode TrafficPolicyAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyAlreadyExists :: Encode TrafficPolicyAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyComment = TrafficPolicyComment String
derive instance newtypeTrafficPolicyComment :: Newtype TrafficPolicyComment _
derive instance repGenericTrafficPolicyComment :: Generic TrafficPolicyComment _
instance showTrafficPolicyComment :: Show TrafficPolicyComment where
  show = genericShow
instance decodeTrafficPolicyComment :: Decode TrafficPolicyComment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyComment :: Encode TrafficPolicyComment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyDocument = TrafficPolicyDocument String
derive instance newtypeTrafficPolicyDocument :: Newtype TrafficPolicyDocument _
derive instance repGenericTrafficPolicyDocument :: Generic TrafficPolicyDocument _
instance showTrafficPolicyDocument :: Show TrafficPolicyDocument where
  show = genericShow
instance decodeTrafficPolicyDocument :: Decode TrafficPolicyDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyDocument :: Encode TrafficPolicyDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyId = TrafficPolicyId String
derive instance newtypeTrafficPolicyId :: Newtype TrafficPolicyId _
derive instance repGenericTrafficPolicyId :: Generic TrafficPolicyId _
instance showTrafficPolicyId :: Show TrafficPolicyId where
  show = genericShow
instance decodeTrafficPolicyId :: Decode TrafficPolicyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyId :: Encode TrafficPolicyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more traffic policy instances were created by using the specified traffic policy.</p>
newtype TrafficPolicyInUse = TrafficPolicyInUse 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTrafficPolicyInUse :: Newtype TrafficPolicyInUse _
derive instance repGenericTrafficPolicyInUse :: Generic TrafficPolicyInUse _
instance showTrafficPolicyInUse :: Show TrafficPolicyInUse where
  show = genericShow
instance decodeTrafficPolicyInUse :: Decode TrafficPolicyInUse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyInUse :: Encode TrafficPolicyInUse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


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
derive instance repGenericTrafficPolicyInstance :: Generic TrafficPolicyInstance _
instance showTrafficPolicyInstance :: Show TrafficPolicyInstance where
  show = genericShow
instance decodeTrafficPolicyInstance :: Decode TrafficPolicyInstance where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyInstance :: Encode TrafficPolicyInstance where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is already a traffic policy instance with the specified ID.</p>
newtype TrafficPolicyInstanceAlreadyExists = TrafficPolicyInstanceAlreadyExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTrafficPolicyInstanceAlreadyExists :: Newtype TrafficPolicyInstanceAlreadyExists _
derive instance repGenericTrafficPolicyInstanceAlreadyExists :: Generic TrafficPolicyInstanceAlreadyExists _
instance showTrafficPolicyInstanceAlreadyExists :: Show TrafficPolicyInstanceAlreadyExists where
  show = genericShow
instance decodeTrafficPolicyInstanceAlreadyExists :: Decode TrafficPolicyInstanceAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyInstanceAlreadyExists :: Encode TrafficPolicyInstanceAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyInstanceCount = TrafficPolicyInstanceCount Int
derive instance newtypeTrafficPolicyInstanceCount :: Newtype TrafficPolicyInstanceCount _
derive instance repGenericTrafficPolicyInstanceCount :: Generic TrafficPolicyInstanceCount _
instance showTrafficPolicyInstanceCount :: Show TrafficPolicyInstanceCount where
  show = genericShow
instance decodeTrafficPolicyInstanceCount :: Decode TrafficPolicyInstanceCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyInstanceCount :: Encode TrafficPolicyInstanceCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyInstanceId = TrafficPolicyInstanceId String
derive instance newtypeTrafficPolicyInstanceId :: Newtype TrafficPolicyInstanceId _
derive instance repGenericTrafficPolicyInstanceId :: Generic TrafficPolicyInstanceId _
instance showTrafficPolicyInstanceId :: Show TrafficPolicyInstanceId where
  show = genericShow
instance decodeTrafficPolicyInstanceId :: Decode TrafficPolicyInstanceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyInstanceId :: Encode TrafficPolicyInstanceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyInstanceState = TrafficPolicyInstanceState String
derive instance newtypeTrafficPolicyInstanceState :: Newtype TrafficPolicyInstanceState _
derive instance repGenericTrafficPolicyInstanceState :: Generic TrafficPolicyInstanceState _
instance showTrafficPolicyInstanceState :: Show TrafficPolicyInstanceState where
  show = genericShow
instance decodeTrafficPolicyInstanceState :: Decode TrafficPolicyInstanceState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyInstanceState :: Encode TrafficPolicyInstanceState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyInstances = TrafficPolicyInstances (Array TrafficPolicyInstance)
derive instance newtypeTrafficPolicyInstances :: Newtype TrafficPolicyInstances _
derive instance repGenericTrafficPolicyInstances :: Generic TrafficPolicyInstances _
instance showTrafficPolicyInstances :: Show TrafficPolicyInstances where
  show = genericShow
instance decodeTrafficPolicyInstances :: Decode TrafficPolicyInstances where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyInstances :: Encode TrafficPolicyInstances where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyName = TrafficPolicyName String
derive instance newtypeTrafficPolicyName :: Newtype TrafficPolicyName _
derive instance repGenericTrafficPolicyName :: Generic TrafficPolicyName _
instance showTrafficPolicyName :: Show TrafficPolicyName where
  show = genericShow
instance decodeTrafficPolicyName :: Decode TrafficPolicyName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyName :: Encode TrafficPolicyName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicySummaries = TrafficPolicySummaries (Array TrafficPolicySummary)
derive instance newtypeTrafficPolicySummaries :: Newtype TrafficPolicySummaries _
derive instance repGenericTrafficPolicySummaries :: Generic TrafficPolicySummaries _
instance showTrafficPolicySummaries :: Show TrafficPolicySummaries where
  show = genericShow
instance decodeTrafficPolicySummaries :: Decode TrafficPolicySummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicySummaries :: Encode TrafficPolicySummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the latest version of one traffic policy that is associated with the current AWS account.</p>
newtype TrafficPolicySummary = TrafficPolicySummary 
  { "Id" :: (TrafficPolicyId)
  , "Name" :: (TrafficPolicyName)
  , "Type" :: (RRType)
  , "LatestVersion" :: (TrafficPolicyVersion)
  , "TrafficPolicyCount" :: (TrafficPolicyVersion)
  }
derive instance newtypeTrafficPolicySummary :: Newtype TrafficPolicySummary _
derive instance repGenericTrafficPolicySummary :: Generic TrafficPolicySummary _
instance showTrafficPolicySummary :: Show TrafficPolicySummary where
  show = genericShow
instance decodeTrafficPolicySummary :: Decode TrafficPolicySummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicySummary :: Encode TrafficPolicySummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyVersion = TrafficPolicyVersion Int
derive instance newtypeTrafficPolicyVersion :: Newtype TrafficPolicyVersion _
derive instance repGenericTrafficPolicyVersion :: Generic TrafficPolicyVersion _
instance showTrafficPolicyVersion :: Show TrafficPolicyVersion where
  show = genericShow
instance decodeTrafficPolicyVersion :: Decode TrafficPolicyVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyVersion :: Encode TrafficPolicyVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrafficPolicyVersionMarker = TrafficPolicyVersionMarker String
derive instance newtypeTrafficPolicyVersionMarker :: Newtype TrafficPolicyVersionMarker _
derive instance repGenericTrafficPolicyVersionMarker :: Generic TrafficPolicyVersionMarker _
instance showTrafficPolicyVersionMarker :: Show TrafficPolicyVersionMarker where
  show = genericShow
instance decodeTrafficPolicyVersionMarker :: Decode TrafficPolicyVersionMarker where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrafficPolicyVersionMarker :: Encode TrafficPolicyVersionMarker where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TransportProtocol = TransportProtocol String
derive instance newtypeTransportProtocol :: Newtype TransportProtocol _
derive instance repGenericTransportProtocol :: Generic TransportProtocol _
instance showTransportProtocol :: Show TransportProtocol where
  show = genericShow
instance decodeTransportProtocol :: Decode TransportProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransportProtocol :: Encode TransportProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about a request to update a health check.</p>
newtype UpdateHealthCheckRequest = UpdateHealthCheckRequest 
  { "HealthCheckId" :: (HealthCheckId)
  , "HealthCheckVersion" :: NullOrUndefined.NullOrUndefined (HealthCheckVersion)
  , "IPAddress" :: NullOrUndefined.NullOrUndefined (IPAddress)
  , "Port" :: NullOrUndefined.NullOrUndefined (Port)
  , "ResourcePath" :: NullOrUndefined.NullOrUndefined (ResourcePath)
  , "FullyQualifiedDomainName" :: NullOrUndefined.NullOrUndefined (FullyQualifiedDomainName)
  , "SearchString" :: NullOrUndefined.NullOrUndefined (SearchString)
  , "FailureThreshold" :: NullOrUndefined.NullOrUndefined (FailureThreshold)
  , "Inverted" :: NullOrUndefined.NullOrUndefined (Inverted)
  , "HealthThreshold" :: NullOrUndefined.NullOrUndefined (HealthThreshold)
  , "ChildHealthChecks" :: NullOrUndefined.NullOrUndefined (ChildHealthCheckList)
  , "EnableSNI" :: NullOrUndefined.NullOrUndefined (EnableSNI)
  , "Regions" :: NullOrUndefined.NullOrUndefined (HealthCheckRegionList)
  , "AlarmIdentifier" :: NullOrUndefined.NullOrUndefined (AlarmIdentifier)
  , "InsufficientDataHealthStatus" :: NullOrUndefined.NullOrUndefined (InsufficientDataHealthStatus)
  , "ResetElements" :: NullOrUndefined.NullOrUndefined (ResettableElementNameList)
  }
derive instance newtypeUpdateHealthCheckRequest :: Newtype UpdateHealthCheckRequest _
derive instance repGenericUpdateHealthCheckRequest :: Generic UpdateHealthCheckRequest _
instance showUpdateHealthCheckRequest :: Show UpdateHealthCheckRequest where
  show = genericShow
instance decodeUpdateHealthCheckRequest :: Decode UpdateHealthCheckRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateHealthCheckRequest :: Encode UpdateHealthCheckRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateHealthCheckResponse = UpdateHealthCheckResponse 
  { "HealthCheck" :: (HealthCheck)
  }
derive instance newtypeUpdateHealthCheckResponse :: Newtype UpdateHealthCheckResponse _
derive instance repGenericUpdateHealthCheckResponse :: Generic UpdateHealthCheckResponse _
instance showUpdateHealthCheckResponse :: Show UpdateHealthCheckResponse where
  show = genericShow
instance decodeUpdateHealthCheckResponse :: Decode UpdateHealthCheckResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateHealthCheckResponse :: Encode UpdateHealthCheckResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A request to update the comment for a hosted zone.</p>
newtype UpdateHostedZoneCommentRequest = UpdateHostedZoneCommentRequest 
  { "Id" :: (ResourceId)
  , "Comment" :: NullOrUndefined.NullOrUndefined (ResourceDescription)
  }
derive instance newtypeUpdateHostedZoneCommentRequest :: Newtype UpdateHostedZoneCommentRequest _
derive instance repGenericUpdateHostedZoneCommentRequest :: Generic UpdateHostedZoneCommentRequest _
instance showUpdateHostedZoneCommentRequest :: Show UpdateHostedZoneCommentRequest where
  show = genericShow
instance decodeUpdateHostedZoneCommentRequest :: Decode UpdateHostedZoneCommentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateHostedZoneCommentRequest :: Encode UpdateHostedZoneCommentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response to the <code>UpdateHostedZoneComment</code> request.</p>
newtype UpdateHostedZoneCommentResponse = UpdateHostedZoneCommentResponse 
  { "HostedZone" :: (HostedZone)
  }
derive instance newtypeUpdateHostedZoneCommentResponse :: Newtype UpdateHostedZoneCommentResponse _
derive instance repGenericUpdateHostedZoneCommentResponse :: Generic UpdateHostedZoneCommentResponse _
instance showUpdateHostedZoneCommentResponse :: Show UpdateHostedZoneCommentResponse where
  show = genericShow
instance decodeUpdateHostedZoneCommentResponse :: Decode UpdateHostedZoneCommentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateHostedZoneCommentResponse :: Encode UpdateHostedZoneCommentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the traffic policy that you want to update the comment for.</p>
newtype UpdateTrafficPolicyCommentRequest = UpdateTrafficPolicyCommentRequest 
  { "Id" :: (TrafficPolicyId)
  , "Version" :: (TrafficPolicyVersion)
  , "Comment" :: (TrafficPolicyComment)
  }
derive instance newtypeUpdateTrafficPolicyCommentRequest :: Newtype UpdateTrafficPolicyCommentRequest _
derive instance repGenericUpdateTrafficPolicyCommentRequest :: Generic UpdateTrafficPolicyCommentRequest _
instance showUpdateTrafficPolicyCommentRequest :: Show UpdateTrafficPolicyCommentRequest where
  show = genericShow
instance decodeUpdateTrafficPolicyCommentRequest :: Decode UpdateTrafficPolicyCommentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTrafficPolicyCommentRequest :: Encode UpdateTrafficPolicyCommentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains the response information for the traffic policy.</p>
newtype UpdateTrafficPolicyCommentResponse = UpdateTrafficPolicyCommentResponse 
  { "TrafficPolicy" :: (TrafficPolicy)
  }
derive instance newtypeUpdateTrafficPolicyCommentResponse :: Newtype UpdateTrafficPolicyCommentResponse _
derive instance repGenericUpdateTrafficPolicyCommentResponse :: Generic UpdateTrafficPolicyCommentResponse _
instance showUpdateTrafficPolicyCommentResponse :: Show UpdateTrafficPolicyCommentResponse where
  show = genericShow
instance decodeUpdateTrafficPolicyCommentResponse :: Decode UpdateTrafficPolicyCommentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTrafficPolicyCommentResponse :: Encode UpdateTrafficPolicyCommentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the resource record sets that you want to update based on a specified traffic policy instance.</p>
newtype UpdateTrafficPolicyInstanceRequest = UpdateTrafficPolicyInstanceRequest 
  { "Id" :: (TrafficPolicyInstanceId)
  , "TTL" :: (TTL)
  , "TrafficPolicyId" :: (TrafficPolicyId)
  , "TrafficPolicyVersion" :: (TrafficPolicyVersion)
  }
derive instance newtypeUpdateTrafficPolicyInstanceRequest :: Newtype UpdateTrafficPolicyInstanceRequest _
derive instance repGenericUpdateTrafficPolicyInstanceRequest :: Generic UpdateTrafficPolicyInstanceRequest _
instance showUpdateTrafficPolicyInstanceRequest :: Show UpdateTrafficPolicyInstanceRequest where
  show = genericShow
instance decodeUpdateTrafficPolicyInstanceRequest :: Decode UpdateTrafficPolicyInstanceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTrafficPolicyInstanceRequest :: Encode UpdateTrafficPolicyInstanceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A complex type that contains information about the resource record sets that Amazon Route 53 created based on a specified traffic policy.</p>
newtype UpdateTrafficPolicyInstanceResponse = UpdateTrafficPolicyInstanceResponse 
  { "TrafficPolicyInstance" :: (TrafficPolicyInstance)
  }
derive instance newtypeUpdateTrafficPolicyInstanceResponse :: Newtype UpdateTrafficPolicyInstanceResponse _
derive instance repGenericUpdateTrafficPolicyInstanceResponse :: Generic UpdateTrafficPolicyInstanceResponse _
instance showUpdateTrafficPolicyInstanceResponse :: Show UpdateTrafficPolicyInstanceResponse where
  show = genericShow
instance decodeUpdateTrafficPolicyInstanceResponse :: Decode UpdateTrafficPolicyInstanceResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTrafficPolicyInstanceResponse :: Encode UpdateTrafficPolicyInstanceResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsageCount = UsageCount Number
derive instance newtypeUsageCount :: Newtype UsageCount _
derive instance repGenericUsageCount :: Generic UsageCount _
instance showUsageCount :: Show UsageCount where
  show = genericShow
instance decodeUsageCount :: Decode UsageCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsageCount :: Encode UsageCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>(Private hosted zones only) A complex type that contains information about an Amazon VPC.</p>
newtype VPC = VPC 
  { "VPCRegion" :: NullOrUndefined.NullOrUndefined (VPCRegion)
  , "VPCId" :: NullOrUndefined.NullOrUndefined (VPCId)
  }
derive instance newtypeVPC :: Newtype VPC _
derive instance repGenericVPC :: Generic VPC _
instance showVPC :: Show VPC where
  show = genericShow
instance decodeVPC :: Decode VPC where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPC :: Encode VPC where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The VPC that you specified is not authorized to be associated with the hosted zone.</p>
newtype VPCAssociationAuthorizationNotFound = VPCAssociationAuthorizationNotFound 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeVPCAssociationAuthorizationNotFound :: Newtype VPCAssociationAuthorizationNotFound _
derive instance repGenericVPCAssociationAuthorizationNotFound :: Generic VPCAssociationAuthorizationNotFound _
instance showVPCAssociationAuthorizationNotFound :: Show VPCAssociationAuthorizationNotFound where
  show = genericShow
instance decodeVPCAssociationAuthorizationNotFound :: Decode VPCAssociationAuthorizationNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCAssociationAuthorizationNotFound :: Encode VPCAssociationAuthorizationNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified VPC and hosted zone are not currently associated.</p>
newtype VPCAssociationNotFound = VPCAssociationNotFound 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeVPCAssociationNotFound :: Newtype VPCAssociationNotFound _
derive instance repGenericVPCAssociationNotFound :: Generic VPCAssociationNotFound _
instance showVPCAssociationNotFound :: Show VPCAssociationNotFound where
  show = genericShow
instance decodeVPCAssociationNotFound :: Decode VPCAssociationNotFound where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCAssociationNotFound :: Encode VPCAssociationNotFound where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>(Private hosted zones only) The ID of an Amazon VPC. </p>
newtype VPCId = VPCId String
derive instance newtypeVPCId :: Newtype VPCId _
derive instance repGenericVPCId :: Generic VPCId _
instance showVPCId :: Show VPCId where
  show = genericShow
instance decodeVPCId :: Decode VPCId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCId :: Encode VPCId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VPCRegion = VPCRegion String
derive instance newtypeVPCRegion :: Newtype VPCRegion _
derive instance repGenericVPCRegion :: Generic VPCRegion _
instance showVPCRegion :: Show VPCRegion where
  show = genericShow
instance decodeVPCRegion :: Decode VPCRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCRegion :: Encode VPCRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>(Private hosted zones only) A list of <code>VPC</code> elements.</p>
newtype VPCs = VPCs (Array VPC)
derive instance newtypeVPCs :: Newtype VPCs _
derive instance repGenericVPCs :: Generic VPCs _
instance showVPCs :: Show VPCs where
  show = genericShow
instance decodeVPCs :: Decode VPCs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVPCs :: Encode VPCs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
