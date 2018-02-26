

-- | <p>Amazon Route 53 auto naming lets you configure public or private namespaces that your microservice applications run in. When instances of the service become available, you can call the auto naming API to register the instance, and Route 53 automatically creates up to five DNS records and an optional health check. Clients that submit DNS queries for the service receive an answer that contains up to eight healthy records.</p>
module AWS.ServiceDiscovery where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "ServiceDiscovery" :: String


-- | <p>Creates a private namespace based on DNS, which will be visible only inside a specified Amazon VPC. The namespace defines your service naming scheme. For example, if you name your namespace <code>example.com</code> and name your service <code>backend</code>, the resulting DNS name for the service will be <code>backend.example.com</code>. You can associate more than one service with the same namespace.</p>
createPrivateDnsNamespace :: forall eff. CreatePrivateDnsNamespaceRequest -> Aff (err :: AWS.RequestError | eff) CreatePrivateDnsNamespaceResponse
createPrivateDnsNamespace = AWS.request serviceName "CreatePrivateDnsNamespace" 


-- | <p>Creates a public namespace based on DNS, which will be visible on the internet. The namespace defines your service naming scheme. For example, if you name your namespace <code>example.com</code> and name your service <code>backend</code>, the resulting DNS name for the service will be <code>backend.example.com</code>. You can associate more than one service with the same namespace.</p>
createPublicDnsNamespace :: forall eff. CreatePublicDnsNamespaceRequest -> Aff (err :: AWS.RequestError | eff) CreatePublicDnsNamespaceResponse
createPublicDnsNamespace = AWS.request serviceName "CreatePublicDnsNamespace" 


-- | <p>Creates a service, which defines the configuration for the following entities:</p> <ul> <li> <p>Up to three records (A, AAAA, and SRV) or one CNAME record</p> </li> <li> <p>Optionally, a health check</p> </li> </ul> <p>After you create the service, you can submit a <a>RegisterInstance</a> request, and Amazon Route 53 uses the values in the configuration to create the specified entities. </p>
createService :: forall eff. CreateServiceRequest -> Aff (err :: AWS.RequestError | eff) CreateServiceResponse
createService = AWS.request serviceName "CreateService" 


-- | <p>Deletes a namespace from the current account. If the namespace still contains one or more services, the request fails.</p>
deleteNamespace :: forall eff. DeleteNamespaceRequest -> Aff (err :: AWS.RequestError | eff) DeleteNamespaceResponse
deleteNamespace = AWS.request serviceName "DeleteNamespace" 


-- | <p>Deletes a specified service. If the service still contains one or more registered instances, the request fails.</p>
deleteService :: forall eff. DeleteServiceRequest -> Aff (err :: AWS.RequestError | eff) DeleteServiceResponse
deleteService = AWS.request serviceName "DeleteService" 


-- | <p>Deletes the records and the health check, if any, that Amazon Route 53 created for the specified instance.</p>
deregisterInstance :: forall eff. DeregisterInstanceRequest -> Aff (err :: AWS.RequestError | eff) DeregisterInstanceResponse
deregisterInstance = AWS.request serviceName "DeregisterInstance" 


-- | <p>Gets information about a specified instance.</p>
getInstance :: forall eff. GetInstanceRequest -> Aff (err :: AWS.RequestError | eff) GetInstanceResponse
getInstance = AWS.request serviceName "GetInstance" 


-- | <p>Gets the current health status (<code>Healthy</code>, <code>Unhealthy</code>, or <code>Unknown</code>) of one or more instances that are associated with a specified service.</p> <note> <p>There is a brief delay between when you register an instance and when the health status for the instance is available. </p> </note>
getInstancesHealthStatus :: forall eff. GetInstancesHealthStatusRequest -> Aff (err :: AWS.RequestError | eff) GetInstancesHealthStatusResponse
getInstancesHealthStatus = AWS.request serviceName "GetInstancesHealthStatus" 


-- | <p>Gets information about a namespace.</p>
getNamespace :: forall eff. GetNamespaceRequest -> Aff (err :: AWS.RequestError | eff) GetNamespaceResponse
getNamespace = AWS.request serviceName "GetNamespace" 


-- | <p>Gets information about any operation that returns an operation ID in the response, such as a <code>CreateService</code> request.</p> <note> <p>To get a list of operations that match specified criteria, see <a>ListOperations</a>.</p> </note>
getOperation :: forall eff. GetOperationRequest -> Aff (err :: AWS.RequestError | eff) GetOperationResponse
getOperation = AWS.request serviceName "GetOperation" 


-- | <p>Gets the settings for a specified service.</p>
getService :: forall eff. GetServiceRequest -> Aff (err :: AWS.RequestError | eff) GetServiceResponse
getService = AWS.request serviceName "GetService" 


-- | <p>Lists summary information about the instances that you registered by using a specified service.</p>
listInstances :: forall eff. ListInstancesRequest -> Aff (err :: AWS.RequestError | eff) ListInstancesResponse
listInstances = AWS.request serviceName "ListInstances" 


-- | <p>Lists summary information about the namespaces that were created by the current AWS account.</p>
listNamespaces :: forall eff. ListNamespacesRequest -> Aff (err :: AWS.RequestError | eff) ListNamespacesResponse
listNamespaces = AWS.request serviceName "ListNamespaces" 


-- | <p>Lists operations that match the criteria that you specify.</p>
listOperations :: forall eff. ListOperationsRequest -> Aff (err :: AWS.RequestError | eff) ListOperationsResponse
listOperations = AWS.request serviceName "ListOperations" 


-- | <p>Lists summary information for all the services that are associated with one or more specified namespaces.</p>
listServices :: forall eff. ListServicesRequest -> Aff (err :: AWS.RequestError | eff) ListServicesResponse
listServices = AWS.request serviceName "ListServices" 


-- | <p>Creates or updates one or more records and optionally a health check based on the settings in a specified service. When you submit a <code>RegisterInstance</code> request, Amazon Route 53 does the following:</p> <ul> <li> <p>For each DNS record that you define in the service specified by <code>ServiceId</code>, creates or updates a record in the hosted zone that is associated with the corresponding namespace</p> </li> <li> <p>Creates or updates a health check based on the settings in the health check configuration, if any, for the service</p> </li> <li> <p>Associates the health check, if any, with each of the records</p> </li> </ul> <important> <p>One <code>RegisterInstance</code> request must complete before you can submit another request and specify the same service ID and instance ID.</p> </important> <p>For more information, see <a>CreateService</a>.</p> <p>When Route 53 receives a DNS query for the specified DNS name, it returns the applicable value:</p> <ul> <li> <p> <b>If the health check is healthy</b>: returns all the records</p> </li> <li> <p> <b>If the health check is unhealthy</b>: returns the IP address of the last healthy instance</p> </li> <li> <p> <b>If you didn't specify a health check configuration</b>: returns all the records</p> </li> </ul>
registerInstance :: forall eff. RegisterInstanceRequest -> Aff (err :: AWS.RequestError | eff) RegisterInstanceResponse
registerInstance = AWS.request serviceName "RegisterInstance" 


-- | <p>Submits a request to perform the following operations:</p> <ul> <li> <p>Add or delete <code>DnsRecords</code> configurations</p> </li> <li> <p>Update the TTL setting for existing <code>DnsRecords</code> configurations</p> </li> <li> <p>Add, update, or delete <code>HealthCheckConfig</code> for a specified service</p> </li> <li> <p/> </li> </ul> <p>You must specify all <code>DnsRecords</code> configurations (and, optionally, <code>HealthCheckConfig</code>) that you want to appear in the updated service. Any current configurations that don't appear in an <code>UpdateService</code> request are deleted.</p> <p>When you update the TTL setting for a service, Amazon Route 53 also updates the corresponding settings in all the records and health checks that were created by using the specified service.</p>
updateService :: forall eff. UpdateServiceRequest -> Aff (err :: AWS.RequestError | eff) UpdateServiceResponse
updateService = AWS.request serviceName "UpdateService" 


newtype Arn = Arn String


newtype AttrKey = AttrKey String


newtype AttrValue = AttrValue String


newtype Attributes = Attributes (Map AttrKey AttrValue)


newtype Code = Code String


newtype CreatePrivateDnsNamespaceRequest = CreatePrivateDnsNamespaceRequest 
  { "Name" :: (NamespaceName)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Description" :: NullOrUndefined (ResourceDescription)
  , "Vpc" :: (ResourceId)
  }


newtype CreatePrivateDnsNamespaceResponse = CreatePrivateDnsNamespaceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }


newtype CreatePublicDnsNamespaceRequest = CreatePublicDnsNamespaceRequest 
  { "Name" :: (NamespaceName)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Description" :: NullOrUndefined (ResourceDescription)
  }


newtype CreatePublicDnsNamespaceResponse = CreatePublicDnsNamespaceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }


newtype CreateServiceRequest = CreateServiceRequest 
  { "Name" :: (ServiceName)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Description" :: NullOrUndefined (ResourceDescription)
  , "DnsConfig" :: (DnsConfig)
  , "HealthCheckConfig" :: NullOrUndefined (HealthCheckConfig)
  }


newtype CreateServiceResponse = CreateServiceResponse 
  { "Service" :: NullOrUndefined (Service)
  }


newtype DeleteNamespaceRequest = DeleteNamespaceRequest 
  { "Id" :: (ResourceId)
  }


newtype DeleteNamespaceResponse = DeleteNamespaceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }


newtype DeleteServiceRequest = DeleteServiceRequest 
  { "Id" :: (ResourceId)
  }


newtype DeleteServiceResponse = DeleteServiceResponse 
  { 
  }


newtype DeregisterInstanceRequest = DeregisterInstanceRequest 
  { "ServiceId" :: (ResourceId)
  , "InstanceId" :: (ResourceId)
  }


newtype DeregisterInstanceResponse = DeregisterInstanceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }


-- | <p>A complex type that contains information about the records that you want Amazon Route 53 to create when you register an instance.</p>
newtype DnsConfig = DnsConfig 
  { "NamespaceId" :: (ResourceId)
  , "RoutingPolicy" :: NullOrUndefined (RoutingPolicy)
  , "DnsRecords" :: (DnsRecordList)
  }


-- | <p>A complex type that contains information about changes to the records that Route 53 creates when you register an instance.</p>
newtype DnsConfigChange = DnsConfigChange 
  { "DnsRecords" :: (DnsRecordList)
  }


-- | <p>A complex type that contains the ID for the hosted zone that Route 53 creates when you create a namespace.</p>
newtype DnsProperties = DnsProperties 
  { "HostedZoneId" :: NullOrUndefined (ResourceId)
  }


-- | <p>A complex type that contains information about the records that you want Route 53 to create when you register an instance.</p>
newtype DnsRecord = DnsRecord 
  { "Type" :: (RecordType)
  , "TTL" :: (RecordTTL)
  }


newtype DnsRecordList = DnsRecordList (Array DnsRecord)


-- | <p>The operation is already in progress.</p>
newtype DuplicateRequest = DuplicateRequest 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ErrorMessage = ErrorMessage String


newtype FailureThreshold = FailureThreshold Int


newtype FilterCondition = FilterCondition String


newtype FilterValue = FilterValue String


newtype FilterValues = FilterValues (Array FilterValue)


newtype GetInstanceRequest = GetInstanceRequest 
  { "ServiceId" :: (ResourceId)
  , "InstanceId" :: (ResourceId)
  }


newtype GetInstanceResponse = GetInstanceResponse 
  { "Instance" :: NullOrUndefined (Instance)
  }


newtype GetInstancesHealthStatusRequest = GetInstancesHealthStatusRequest 
  { "ServiceId" :: (ResourceId)
  , "Instances" :: NullOrUndefined (InstanceIdList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetInstancesHealthStatusResponse = GetInstancesHealthStatusResponse 
  { "Status" :: NullOrUndefined (InstanceHealthStatusMap)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype GetNamespaceRequest = GetNamespaceRequest 
  { "Id" :: (ResourceId)
  }


newtype GetNamespaceResponse = GetNamespaceResponse 
  { "Namespace" :: NullOrUndefined (Namespace)
  }


newtype GetOperationRequest = GetOperationRequest 
  { "OperationId" :: (ResourceId)
  }


newtype GetOperationResponse = GetOperationResponse 
  { "Operation" :: NullOrUndefined (Operation)
  }


newtype GetServiceRequest = GetServiceRequest 
  { "Id" :: (ResourceId)
  }


newtype GetServiceResponse = GetServiceResponse 
  { "Service" :: NullOrUndefined (Service)
  }


-- | <p> <i>Public DNS namespaces only.</i> A complex type that contains settings for an optional health check. If you specify settings for a health check, Amazon Route 53 associates the health check with all the records that you specify in <code>DnsConfig</code>.</p> <p> <b>A and AAAA records</b> </p> <p>If <code>DnsConfig</code> includes configurations for both A and AAAA records, Route 53 creates a health check that uses the IPv4 address to check the health of the resource. If the endpoint that is specified by the IPv4 address is unhealthy, Route 53 considers both the A and AAAA records to be unhealthy. </p> <p> <b>CNAME records</b> </p> <p>You can't specify settings for <code>HealthCheckConfig</code> when the <code>DNSConfig</code> includes <code>CNAME</code> for the value of <code>Type</code>. If you do, the <code>CreateService</code> request will fail with an <code>InvalidInput</code> error.</p> <p> <b>Request interval</b> </p> <p>The health check uses 30 seconds as the request interval. This is the number of seconds between the time that each Route 53 health checker gets a response from your endpoint and the time that it sends the next health check request. A health checker in each data center around the world sends your endpoint a health check request every 30 seconds. On average, your endpoint receives a health check request about every two seconds. Health checkers in different data centers don't coordinate with one another, so you'll sometimes see several requests per second followed by a few seconds with no health checks at all.</p> <p> <b>Health checking regions</b> </p> <p>Health checkers perform checks from all Route 53 health-checking regions. For a list of the current regions, see <a href="http://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions">Regions</a>.</p> <p> <b>Alias records</b> </p> <p>When you register an instance, if you include the <code>AWS_ALIAS_DNS_NAME</code> attribute, Route 53 creates an alias record. Note the following:</p> <ul> <li> <p>Route 53 automatically sets <code>EvaluateTargetHealth</code> to true for alias records. When <code>EvaluateTargetHealth</code> is true, the alias record inherits the health of the referenced AWS resource. such as an ELB load balancer. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-EvaluateTargetHealth">EvaluateTargetHealth</a>.</p> </li> <li> <p>If you include <code>HealthCheckConfig</code> and then use the service to register an instance that creates an alias record, Route 53 doesn't create the health check.</p> </li> </ul> <p>For information about the charges for health checks, see <a href="http://aws.amazon.com/route53/pricing">Route 53 Pricing</a>.</p>
newtype HealthCheckConfig = HealthCheckConfig 
  { "Type" :: (HealthCheckType)
  , "ResourcePath" :: NullOrUndefined (ResourcePath)
  , "FailureThreshold" :: NullOrUndefined (FailureThreshold)
  }


newtype HealthCheckType = HealthCheckType String


newtype HealthStatus = HealthStatus String


-- | <p>A complex type that contains information about an instance that Amazon Route 53 creates when you submit a <code>RegisterInstance</code> request.</p>
newtype Instance = Instance 
  { "Id" :: (ResourceId)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Attributes" :: NullOrUndefined (Attributes)
  }


newtype InstanceHealthStatusMap = InstanceHealthStatusMap (Map ResourceId HealthStatus)


newtype InstanceIdList = InstanceIdList (Array ResourceId)


-- | <p>No instance exists with the specified ID, or the instance was recently registered, and information about the instance hasn't propagated yet.</p>
newtype InstanceNotFound = InstanceNotFound 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A complex type that contains information about the instances that you registered by using a specified service.</p>
newtype InstanceSummary = InstanceSummary 
  { "Id" :: NullOrUndefined (ResourceId)
  , "Attributes" :: NullOrUndefined (Attributes)
  }


newtype InstanceSummaryList = InstanceSummaryList (Array InstanceSummary)


-- | <p>One or more specified values aren't valid. For example, when you're creating a namespace, the value of <code>Name</code> might not be a valid DNS name.</p>
newtype InvalidInput = InvalidInput 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ListInstancesRequest = ListInstancesRequest 
  { "ServiceId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListInstancesResponse = ListInstancesResponse 
  { "Instances" :: NullOrUndefined (InstanceSummaryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListNamespacesRequest = ListNamespacesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (NamespaceFilters)
  }


newtype ListNamespacesResponse = ListNamespacesResponse 
  { "Namespaces" :: NullOrUndefined (NamespaceSummariesList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListOperationsRequest = ListOperationsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (OperationFilters)
  }


newtype ListOperationsResponse = ListOperationsResponse 
  { "Operations" :: NullOrUndefined (OperationSummaryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListServicesRequest = ListServicesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (ServiceFilters)
  }


newtype ListServicesResponse = ListServicesResponse 
  { "Services" :: NullOrUndefined (ServiceSummariesList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype MaxResults = MaxResults Int


newtype Message = Message String


-- | <p>A complex type that contains information about a specified namespace.</p>
newtype Namespace = Namespace 
  { "Id" :: NullOrUndefined (ResourceId)
  , "Arn" :: NullOrUndefined (Arn)
  , "Name" :: NullOrUndefined (NamespaceName)
  , "Type" :: NullOrUndefined (NamespaceType)
  , "Description" :: NullOrUndefined (ResourceDescription)
  , "ServiceCount" :: NullOrUndefined (ResourceCount)
  , "Properties" :: NullOrUndefined (NamespaceProperties)
  , "CreateDate" :: NullOrUndefined (Number)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  }


-- | <p>The namespace that you're trying to create already exists.</p>
newtype NamespaceAlreadyExists = NamespaceAlreadyExists 
  { "Message" :: NullOrUndefined (ErrorMessage)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "NamespaceId" :: NullOrUndefined (ResourceId)
  }


-- | <p>A complex type that identifies the namespaces that you want to list. You can choose to list public or private namespaces.</p>
newtype NamespaceFilter = NamespaceFilter 
  { "Name" :: (NamespaceFilterName)
  , "Values" :: (FilterValues)
  , "Condition" :: NullOrUndefined (FilterCondition)
  }


newtype NamespaceFilterName = NamespaceFilterName String


newtype NamespaceFilters = NamespaceFilters (Array NamespaceFilter)


newtype NamespaceName = NamespaceName String


-- | <p>No namespace exists with the specified ID.</p>
newtype NamespaceNotFound = NamespaceNotFound 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>A complex type that contains information that is specific to the namespace type.</p>
newtype NamespaceProperties = NamespaceProperties 
  { "DnsProperties" :: NullOrUndefined (DnsProperties)
  }


newtype NamespaceSummariesList = NamespaceSummariesList (Array NamespaceSummary)


-- | <p>A complex type that contains information about a namespace.</p>
newtype NamespaceSummary = NamespaceSummary 
  { "Id" :: NullOrUndefined (ResourceId)
  , "Arn" :: NullOrUndefined (Arn)
  , "Name" :: NullOrUndefined (NamespaceName)
  , "Type" :: NullOrUndefined (NamespaceType)
  }


newtype NamespaceType = NamespaceType String


newtype NextToken = NextToken String


-- | <p>A complex type that contains information about a specified operation.</p>
newtype Operation = Operation 
  { "Id" :: NullOrUndefined (OperationId)
  , "Type" :: NullOrUndefined (OperationType)
  , "Status" :: NullOrUndefined (OperationStatus)
  , "ErrorMessage" :: NullOrUndefined (Message)
  , "ErrorCode" :: NullOrUndefined (Code)
  , "CreateDate" :: NullOrUndefined (Number)
  , "UpdateDate" :: NullOrUndefined (Number)
  , "Targets" :: NullOrUndefined (OperationTargetsMap)
  }


-- | <p>A complex type that lets you select the operations that you want to list.</p>
newtype OperationFilter = OperationFilter 
  { "Name" :: (OperationFilterName)
  , "Values" :: (FilterValues)
  , "Condition" :: NullOrUndefined (FilterCondition)
  }


newtype OperationFilterName = OperationFilterName String


newtype OperationFilters = OperationFilters (Array OperationFilter)


newtype OperationId = OperationId String


-- | <p>No operation exists with the specified ID.</p>
newtype OperationNotFound = OperationNotFound 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype OperationStatus = OperationStatus String


-- | <p>A complex type that contains information about an operation that matches the criteria that you specified in a <a>ListOperations</a> request.</p>
newtype OperationSummary = OperationSummary 
  { "Id" :: NullOrUndefined (OperationId)
  , "Status" :: NullOrUndefined (OperationStatus)
  }


newtype OperationSummaryList = OperationSummaryList (Array OperationSummary)


newtype OperationTargetType = OperationTargetType String


newtype OperationTargetsMap = OperationTargetsMap (Map OperationTargetType ResourceId)


newtype OperationType = OperationType String


newtype RecordTTL = RecordTTL Number


newtype RecordType = RecordType String


newtype RegisterInstanceRequest = RegisterInstanceRequest 
  { "ServiceId" :: (ResourceId)
  , "InstanceId" :: (ResourceId)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Attributes" :: (Attributes)
  }


newtype RegisterInstanceResponse = RegisterInstanceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }


newtype ResourceCount = ResourceCount Int


newtype ResourceDescription = ResourceDescription String


newtype ResourceId = ResourceId String


-- | <p>The specified resource can't be deleted because it contains other resources. For example, you can't delete a service that contains any instances.</p>
newtype ResourceInUse = ResourceInUse 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The resource can't be created because you've reached the limit on the number of resources.</p>
newtype ResourceLimitExceeded = ResourceLimitExceeded 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ResourcePath = ResourcePath String


newtype RoutingPolicy = RoutingPolicy String


-- | <p>A complex type that contains information about the specified service.</p>
newtype Service = Service 
  { "Id" :: NullOrUndefined (ResourceId)
  , "Arn" :: NullOrUndefined (Arn)
  , "Name" :: NullOrUndefined (ServiceName)
  , "Description" :: NullOrUndefined (ResourceDescription)
  , "InstanceCount" :: NullOrUndefined (ResourceCount)
  , "DnsConfig" :: NullOrUndefined (DnsConfig)
  , "HealthCheckConfig" :: NullOrUndefined (HealthCheckConfig)
  , "CreateDate" :: NullOrUndefined (Number)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  }


-- | <p>The service can't be created because a service with the same name already exists.</p>
newtype ServiceAlreadyExists = ServiceAlreadyExists 
  { "Message" :: NullOrUndefined (ErrorMessage)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "ServiceId" :: NullOrUndefined (ResourceId)
  }


-- | <p>A complex type that contains changes to an existing service.</p>
newtype ServiceChange = ServiceChange 
  { "Description" :: NullOrUndefined (ResourceDescription)
  , "DnsConfig" :: (DnsConfigChange)
  , "HealthCheckConfig" :: NullOrUndefined (HealthCheckConfig)
  }


-- | <p>A complex type that lets you specify the namespaces that you want to list services for.</p>
newtype ServiceFilter = ServiceFilter 
  { "Name" :: (ServiceFilterName)
  , "Values" :: (FilterValues)
  , "Condition" :: NullOrUndefined (FilterCondition)
  }


newtype ServiceFilterName = ServiceFilterName String


newtype ServiceFilters = ServiceFilters (Array ServiceFilter)


newtype ServiceName = ServiceName String


-- | <p>No service exists with the specified ID.</p>
newtype ServiceNotFound = ServiceNotFound 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ServiceSummariesList = ServiceSummariesList (Array ServiceSummary)


-- | <p>A complex type that contains information about a specified service.</p>
newtype ServiceSummary = ServiceSummary 
  { "Id" :: NullOrUndefined (ResourceId)
  , "Arn" :: NullOrUndefined (Arn)
  , "Name" :: NullOrUndefined (ServiceName)
  , "Description" :: NullOrUndefined (ResourceDescription)
  , "InstanceCount" :: NullOrUndefined (ResourceCount)
  }


newtype UpdateServiceRequest = UpdateServiceRequest 
  { "Id" :: (ResourceId)
  , "Service" :: (ServiceChange)
  }


newtype UpdateServiceResponse = UpdateServiceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }
