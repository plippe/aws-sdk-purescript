## Module AWS.ServiceDiscovery

<p>Amazon Route 53 auto naming lets you configure public or private namespaces that your microservice applications run in. When instances of the service become available, you can call the auto naming API to register the instance, and Route 53 automatically creates up to five DNS records and an optional health check. Clients that submit DNS queries for the service receive an answer that contains up to eight healthy records.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createPrivateDnsNamespace`

``` purescript
createPrivateDnsNamespace :: forall eff. CreatePrivateDnsNamespaceRequest -> Aff (err :: RequestError | eff) CreatePrivateDnsNamespaceResponse
```

<p>Creates a private namespace based on DNS, which will be visible only inside a specified Amazon VPC. The namespace defines your service naming scheme. For example, if you name your namespace <code>example.com</code> and name your service <code>backend</code>, the resulting DNS name for the service will be <code>backend.example.com</code>. You can associate more than one service with the same namespace.</p>

#### `createPublicDnsNamespace`

``` purescript
createPublicDnsNamespace :: forall eff. CreatePublicDnsNamespaceRequest -> Aff (err :: RequestError | eff) CreatePublicDnsNamespaceResponse
```

<p>Creates a public namespace based on DNS, which will be visible on the internet. The namespace defines your service naming scheme. For example, if you name your namespace <code>example.com</code> and name your service <code>backend</code>, the resulting DNS name for the service will be <code>backend.example.com</code>. You can associate more than one service with the same namespace.</p>

#### `createService`

``` purescript
createService :: forall eff. CreateServiceRequest -> Aff (err :: RequestError | eff) CreateServiceResponse
```

<p>Creates a service, which defines the configuration for the following entities:</p> <ul> <li> <p>Up to three records (A, AAAA, and SRV) or one CNAME record</p> </li> <li> <p>Optionally, a health check</p> </li> </ul> <p>After you create the service, you can submit a <a>RegisterInstance</a> request, and Amazon Route 53 uses the values in the configuration to create the specified entities. </p>

#### `deleteNamespace`

``` purescript
deleteNamespace :: forall eff. DeleteNamespaceRequest -> Aff (err :: RequestError | eff) DeleteNamespaceResponse
```

<p>Deletes a namespace from the current account. If the namespace still contains one or more services, the request fails.</p>

#### `deleteService`

``` purescript
deleteService :: forall eff. DeleteServiceRequest -> Aff (err :: RequestError | eff) DeleteServiceResponse
```

<p>Deletes a specified service. If the service still contains one or more registered instances, the request fails.</p>

#### `deregisterInstance`

``` purescript
deregisterInstance :: forall eff. DeregisterInstanceRequest -> Aff (err :: RequestError | eff) DeregisterInstanceResponse
```

<p>Deletes the records and the health check, if any, that Amazon Route 53 created for the specified instance.</p>

#### `getInstance`

``` purescript
getInstance :: forall eff. GetInstanceRequest -> Aff (err :: RequestError | eff) GetInstanceResponse
```

<p>Gets information about a specified instance.</p>

#### `getInstancesHealthStatus`

``` purescript
getInstancesHealthStatus :: forall eff. GetInstancesHealthStatusRequest -> Aff (err :: RequestError | eff) GetInstancesHealthStatusResponse
```

<p>Gets the current health status (<code>Healthy</code>, <code>Unhealthy</code>, or <code>Unknown</code>) of one or more instances that are associated with a specified service.</p> <note> <p>There is a brief delay between when you register an instance and when the health status for the instance is available. </p> </note>

#### `getNamespace`

``` purescript
getNamespace :: forall eff. GetNamespaceRequest -> Aff (err :: RequestError | eff) GetNamespaceResponse
```

<p>Gets information about a namespace.</p>

#### `getOperation`

``` purescript
getOperation :: forall eff. GetOperationRequest -> Aff (err :: RequestError | eff) GetOperationResponse
```

<p>Gets information about any operation that returns an operation ID in the response, such as a <code>CreateService</code> request.</p> <note> <p>To get a list of operations that match specified criteria, see <a>ListOperations</a>.</p> </note>

#### `getService`

``` purescript
getService :: forall eff. GetServiceRequest -> Aff (err :: RequestError | eff) GetServiceResponse
```

<p>Gets the settings for a specified service.</p>

#### `listInstances`

``` purescript
listInstances :: forall eff. ListInstancesRequest -> Aff (err :: RequestError | eff) ListInstancesResponse
```

<p>Lists summary information about the instances that you registered by using a specified service.</p>

#### `listNamespaces`

``` purescript
listNamespaces :: forall eff. ListNamespacesRequest -> Aff (err :: RequestError | eff) ListNamespacesResponse
```

<p>Lists summary information about the namespaces that were created by the current AWS account.</p>

#### `listOperations`

``` purescript
listOperations :: forall eff. ListOperationsRequest -> Aff (err :: RequestError | eff) ListOperationsResponse
```

<p>Lists operations that match the criteria that you specify.</p>

#### `listServices`

``` purescript
listServices :: forall eff. ListServicesRequest -> Aff (err :: RequestError | eff) ListServicesResponse
```

<p>Lists summary information for all the services that are associated with one or more specified namespaces.</p>

#### `registerInstance`

``` purescript
registerInstance :: forall eff. RegisterInstanceRequest -> Aff (err :: RequestError | eff) RegisterInstanceResponse
```

<p>Creates or updates one or more records and optionally a health check based on the settings in a specified service. When you submit a <code>RegisterInstance</code> request, Amazon Route 53 does the following:</p> <ul> <li> <p>For each DNS record that you define in the service specified by <code>ServiceId</code>, creates or updates a record in the hosted zone that is associated with the corresponding namespace</p> </li> <li> <p>Creates or updates a health check based on the settings in the health check configuration, if any, for the service</p> </li> <li> <p>Associates the health check, if any, with each of the records</p> </li> </ul> <important> <p>One <code>RegisterInstance</code> request must complete before you can submit another request and specify the same service ID and instance ID.</p> </important> <p>For more information, see <a>CreateService</a>.</p> <p>When Route 53 receives a DNS query for the specified DNS name, it returns the applicable value:</p> <ul> <li> <p> <b>If the health check is healthy</b>: returns all the records</p> </li> <li> <p> <b>If the health check is unhealthy</b>: returns the IP address of the last healthy instance</p> </li> <li> <p> <b>If you didn't specify a health check configuration</b>: returns all the records</p> </li> </ul>

#### `updateService`

``` purescript
updateService :: forall eff. UpdateServiceRequest -> Aff (err :: RequestError | eff) UpdateServiceResponse
```

<p>Submits a request to perform the following operations:</p> <ul> <li> <p>Add or delete <code>DnsRecords</code> configurations</p> </li> <li> <p>Update the TTL setting for existing <code>DnsRecords</code> configurations</p> </li> <li> <p>Add, update, or delete <code>HealthCheckConfig</code> for a specified service</p> </li> <li> <p/> </li> </ul> <p>You must specify all <code>DnsRecords</code> configurations (and, optionally, <code>HealthCheckConfig</code>) that you want to appear in the updated service. Any current configurations that don't appear in an <code>UpdateService</code> request are deleted.</p> <p>When you update the TTL setting for a service, Amazon Route 53 also updates the corresponding settings in all the records and health checks that were created by using the specified service.</p>

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

#### `AttrKey`

``` purescript
newtype AttrKey
  = AttrKey String
```

#### `AttrValue`

``` purescript
newtype AttrValue
  = AttrValue String
```

#### `Attributes`

``` purescript
newtype Attributes
  = Attributes (Map AttrKey AttrValue)
```

#### `Code`

``` purescript
newtype Code
  = Code String
```

#### `CreatePrivateDnsNamespaceRequest`

``` purescript
newtype CreatePrivateDnsNamespaceRequest
  = CreatePrivateDnsNamespaceRequest { "Name" :: NamespaceName, "CreatorRequestId" :: NullOrUndefined (ResourceId), "Description" :: NullOrUndefined (ResourceDescription), "Vpc" :: ResourceId }
```

#### `CreatePrivateDnsNamespaceResponse`

``` purescript
newtype CreatePrivateDnsNamespaceResponse
  = CreatePrivateDnsNamespaceResponse { "OperationId" :: NullOrUndefined (OperationId) }
```

#### `CreatePublicDnsNamespaceRequest`

``` purescript
newtype CreatePublicDnsNamespaceRequest
  = CreatePublicDnsNamespaceRequest { "Name" :: NamespaceName, "CreatorRequestId" :: NullOrUndefined (ResourceId), "Description" :: NullOrUndefined (ResourceDescription) }
```

#### `CreatePublicDnsNamespaceResponse`

``` purescript
newtype CreatePublicDnsNamespaceResponse
  = CreatePublicDnsNamespaceResponse { "OperationId" :: NullOrUndefined (OperationId) }
```

#### `CreateServiceRequest`

``` purescript
newtype CreateServiceRequest
  = CreateServiceRequest { "Name" :: ServiceName, "CreatorRequestId" :: NullOrUndefined (ResourceId), "Description" :: NullOrUndefined (ResourceDescription), "DnsConfig" :: DnsConfig, "HealthCheckConfig" :: NullOrUndefined (HealthCheckConfig) }
```

#### `CreateServiceResponse`

``` purescript
newtype CreateServiceResponse
  = CreateServiceResponse { "Service" :: NullOrUndefined (Service) }
```

#### `DeleteNamespaceRequest`

``` purescript
newtype DeleteNamespaceRequest
  = DeleteNamespaceRequest { "Id" :: ResourceId }
```

#### `DeleteNamespaceResponse`

``` purescript
newtype DeleteNamespaceResponse
  = DeleteNamespaceResponse { "OperationId" :: NullOrUndefined (OperationId) }
```

#### `DeleteServiceRequest`

``` purescript
newtype DeleteServiceRequest
  = DeleteServiceRequest { "Id" :: ResourceId }
```

#### `DeleteServiceResponse`

``` purescript
newtype DeleteServiceResponse
  = DeleteServiceResponse {  }
```

#### `DeregisterInstanceRequest`

``` purescript
newtype DeregisterInstanceRequest
  = DeregisterInstanceRequest { "ServiceId" :: ResourceId, "InstanceId" :: ResourceId }
```

#### `DeregisterInstanceResponse`

``` purescript
newtype DeregisterInstanceResponse
  = DeregisterInstanceResponse { "OperationId" :: NullOrUndefined (OperationId) }
```

#### `DnsConfig`

``` purescript
newtype DnsConfig
  = DnsConfig { "NamespaceId" :: ResourceId, "RoutingPolicy" :: NullOrUndefined (RoutingPolicy), "DnsRecords" :: DnsRecordList }
```

<p>A complex type that contains information about the records that you want Amazon Route 53 to create when you register an instance.</p>

#### `DnsConfigChange`

``` purescript
newtype DnsConfigChange
  = DnsConfigChange { "DnsRecords" :: DnsRecordList }
```

<p>A complex type that contains information about changes to the records that Route 53 creates when you register an instance.</p>

#### `DnsProperties`

``` purescript
newtype DnsProperties
  = DnsProperties { "HostedZoneId" :: NullOrUndefined (ResourceId) }
```

<p>A complex type that contains the ID for the hosted zone that Route 53 creates when you create a namespace.</p>

#### `DnsRecord`

``` purescript
newtype DnsRecord
  = DnsRecord { "Type" :: RecordType, "TTL" :: RecordTTL }
```

<p>A complex type that contains information about the records that you want Route 53 to create when you register an instance.</p>

#### `DnsRecordList`

``` purescript
newtype DnsRecordList
  = DnsRecordList (Array DnsRecord)
```

#### `DuplicateRequest`

``` purescript
newtype DuplicateRequest
  = DuplicateRequest { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The operation is already in progress.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `FailureThreshold`

``` purescript
newtype FailureThreshold
  = FailureThreshold Int
```

#### `FilterCondition`

``` purescript
newtype FilterCondition
  = FilterCondition String
```

#### `FilterValue`

``` purescript
newtype FilterValue
  = FilterValue String
```

#### `FilterValues`

``` purescript
newtype FilterValues
  = FilterValues (Array FilterValue)
```

#### `GetInstanceRequest`

``` purescript
newtype GetInstanceRequest
  = GetInstanceRequest { "ServiceId" :: ResourceId, "InstanceId" :: ResourceId }
```

#### `GetInstanceResponse`

``` purescript
newtype GetInstanceResponse
  = GetInstanceResponse { "Instance" :: NullOrUndefined (Instance) }
```

#### `GetInstancesHealthStatusRequest`

``` purescript
newtype GetInstancesHealthStatusRequest
  = GetInstancesHealthStatusRequest { "ServiceId" :: ResourceId, "Instances" :: NullOrUndefined (InstanceIdList), "MaxResults" :: NullOrUndefined (MaxResults), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetInstancesHealthStatusResponse`

``` purescript
newtype GetInstancesHealthStatusResponse
  = GetInstancesHealthStatusResponse { "Status" :: NullOrUndefined (InstanceHealthStatusMap), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `GetNamespaceRequest`

``` purescript
newtype GetNamespaceRequest
  = GetNamespaceRequest { "Id" :: ResourceId }
```

#### `GetNamespaceResponse`

``` purescript
newtype GetNamespaceResponse
  = GetNamespaceResponse { "Namespace" :: NullOrUndefined (Namespace) }
```

#### `GetOperationRequest`

``` purescript
newtype GetOperationRequest
  = GetOperationRequest { "OperationId" :: ResourceId }
```

#### `GetOperationResponse`

``` purescript
newtype GetOperationResponse
  = GetOperationResponse { "Operation" :: NullOrUndefined (Operation) }
```

#### `GetServiceRequest`

``` purescript
newtype GetServiceRequest
  = GetServiceRequest { "Id" :: ResourceId }
```

#### `GetServiceResponse`

``` purescript
newtype GetServiceResponse
  = GetServiceResponse { "Service" :: NullOrUndefined (Service) }
```

#### `HealthCheckConfig`

``` purescript
newtype HealthCheckConfig
  = HealthCheckConfig { "Type" :: HealthCheckType, "ResourcePath" :: NullOrUndefined (ResourcePath), "FailureThreshold" :: NullOrUndefined (FailureThreshold) }
```

<p> <i>Public DNS namespaces only.</i> A complex type that contains settings for an optional health check. If you specify settings for a health check, Amazon Route 53 associates the health check with all the records that you specify in <code>DnsConfig</code>.</p> <p> <b>A and AAAA records</b> </p> <p>If <code>DnsConfig</code> includes configurations for both A and AAAA records, Route 53 creates a health check that uses the IPv4 address to check the health of the resource. If the endpoint that is specified by the IPv4 address is unhealthy, Route 53 considers both the A and AAAA records to be unhealthy. </p> <p> <b>CNAME records</b> </p> <p>You can't specify settings for <code>HealthCheckConfig</code> when the <code>DNSConfig</code> includes <code>CNAME</code> for the value of <code>Type</code>. If you do, the <code>CreateService</code> request will fail with an <code>InvalidInput</code> error.</p> <p> <b>Request interval</b> </p> <p>The health check uses 30 seconds as the request interval. This is the number of seconds between the time that each Route 53 health checker gets a response from your endpoint and the time that it sends the next health check request. A health checker in each data center around the world sends your endpoint a health check request every 30 seconds. On average, your endpoint receives a health check request about every two seconds. Health checkers in different data centers don't coordinate with one another, so you'll sometimes see several requests per second followed by a few seconds with no health checks at all.</p> <p> <b>Health checking regions</b> </p> <p>Health checkers perform checks from all Route 53 health-checking regions. For a list of the current regions, see <a href="http://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions">Regions</a>.</p> <p> <b>Alias records</b> </p> <p>When you register an instance, if you include the <code>AWS_ALIAS_DNS_NAME</code> attribute, Route 53 creates an alias record. Note the following:</p> <ul> <li> <p>Route 53 automatically sets <code>EvaluateTargetHealth</code> to true for alias records. When <code>EvaluateTargetHealth</code> is true, the alias record inherits the health of the referenced AWS resource. such as an ELB load balancer. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-EvaluateTargetHealth">EvaluateTargetHealth</a>.</p> </li> <li> <p>If you include <code>HealthCheckConfig</code> and then use the service to register an instance that creates an alias record, Route 53 doesn't create the health check.</p> </li> </ul> <p>For information about the charges for health checks, see <a href="http://aws.amazon.com/route53/pricing">Route 53 Pricing</a>.</p>

#### `HealthCheckType`

``` purescript
newtype HealthCheckType
  = HealthCheckType String
```

#### `HealthStatus`

``` purescript
newtype HealthStatus
  = HealthStatus String
```

#### `Instance`

``` purescript
newtype Instance
  = Instance { "Id" :: ResourceId, "CreatorRequestId" :: NullOrUndefined (ResourceId), "Attributes" :: NullOrUndefined (Attributes) }
```

<p>A complex type that contains information about an instance that Amazon Route 53 creates when you submit a <code>RegisterInstance</code> request.</p>

#### `InstanceHealthStatusMap`

``` purescript
newtype InstanceHealthStatusMap
  = InstanceHealthStatusMap (Map ResourceId HealthStatus)
```

#### `InstanceIdList`

``` purescript
newtype InstanceIdList
  = InstanceIdList (Array ResourceId)
```

#### `InstanceNotFound`

``` purescript
newtype InstanceNotFound
  = InstanceNotFound { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>No instance exists with the specified ID, or the instance was recently registered, and information about the instance hasn't propagated yet.</p>

#### `InstanceSummary`

``` purescript
newtype InstanceSummary
  = InstanceSummary { "Id" :: NullOrUndefined (ResourceId), "Attributes" :: NullOrUndefined (Attributes) }
```

<p>A complex type that contains information about the instances that you registered by using a specified service.</p>

#### `InstanceSummaryList`

``` purescript
newtype InstanceSummaryList
  = InstanceSummaryList (Array InstanceSummary)
```

#### `InvalidInput`

``` purescript
newtype InvalidInput
  = InvalidInput { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>One or more specified values aren't valid. For example, when you're creating a namespace, the value of <code>Name</code> might not be a valid DNS name.</p>

#### `ListInstancesRequest`

``` purescript
newtype ListInstancesRequest
  = ListInstancesRequest { "ServiceId" :: ResourceId, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `ListInstancesResponse`

``` purescript
newtype ListInstancesResponse
  = ListInstancesResponse { "Instances" :: NullOrUndefined (InstanceSummaryList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListNamespacesRequest`

``` purescript
newtype ListNamespacesRequest
  = ListNamespacesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (NamespaceFilters) }
```

#### `ListNamespacesResponse`

``` purescript
newtype ListNamespacesResponse
  = ListNamespacesResponse { "Namespaces" :: NullOrUndefined (NamespaceSummariesList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListOperationsRequest`

``` purescript
newtype ListOperationsRequest
  = ListOperationsRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (OperationFilters) }
```

#### `ListOperationsResponse`

``` purescript
newtype ListOperationsResponse
  = ListOperationsResponse { "Operations" :: NullOrUndefined (OperationSummaryList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListServicesRequest`

``` purescript
newtype ListServicesRequest
  = ListServicesRequest { "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults), "Filters" :: NullOrUndefined (ServiceFilters) }
```

#### `ListServicesResponse`

``` purescript
newtype ListServicesResponse
  = ListServicesResponse { "Services" :: NullOrUndefined (ServiceSummariesList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `Namespace`

``` purescript
newtype Namespace
  = Namespace { "Id" :: NullOrUndefined (ResourceId), "Arn" :: NullOrUndefined (Arn), "Name" :: NullOrUndefined (NamespaceName), "Type" :: NullOrUndefined (NamespaceType), "Description" :: NullOrUndefined (ResourceDescription), "ServiceCount" :: NullOrUndefined (ResourceCount), "Properties" :: NullOrUndefined (NamespaceProperties), "CreateDate" :: NullOrUndefined (Number), "CreatorRequestId" :: NullOrUndefined (ResourceId) }
```

<p>A complex type that contains information about a specified namespace.</p>

#### `NamespaceAlreadyExists`

``` purescript
newtype NamespaceAlreadyExists
  = NamespaceAlreadyExists { "Message" :: NullOrUndefined (ErrorMessage), "CreatorRequestId" :: NullOrUndefined (ResourceId), "NamespaceId" :: NullOrUndefined (ResourceId) }
```

<p>The namespace that you're trying to create already exists.</p>

#### `NamespaceFilter`

``` purescript
newtype NamespaceFilter
  = NamespaceFilter { "Name" :: NamespaceFilterName, "Values" :: FilterValues, "Condition" :: NullOrUndefined (FilterCondition) }
```

<p>A complex type that identifies the namespaces that you want to list. You can choose to list public or private namespaces.</p>

#### `NamespaceFilterName`

``` purescript
newtype NamespaceFilterName
  = NamespaceFilterName String
```

#### `NamespaceFilters`

``` purescript
newtype NamespaceFilters
  = NamespaceFilters (Array NamespaceFilter)
```

#### `NamespaceName`

``` purescript
newtype NamespaceName
  = NamespaceName String
```

#### `NamespaceNotFound`

``` purescript
newtype NamespaceNotFound
  = NamespaceNotFound { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>No namespace exists with the specified ID.</p>

#### `NamespaceProperties`

``` purescript
newtype NamespaceProperties
  = NamespaceProperties { "DnsProperties" :: NullOrUndefined (DnsProperties) }
```

<p>A complex type that contains information that is specific to the namespace type.</p>

#### `NamespaceSummariesList`

``` purescript
newtype NamespaceSummariesList
  = NamespaceSummariesList (Array NamespaceSummary)
```

#### `NamespaceSummary`

``` purescript
newtype NamespaceSummary
  = NamespaceSummary { "Id" :: NullOrUndefined (ResourceId), "Arn" :: NullOrUndefined (Arn), "Name" :: NullOrUndefined (NamespaceName), "Type" :: NullOrUndefined (NamespaceType) }
```

<p>A complex type that contains information about a namespace.</p>

#### `NamespaceType`

``` purescript
newtype NamespaceType
  = NamespaceType String
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `Operation`

``` purescript
newtype Operation
  = Operation { "Id" :: NullOrUndefined (OperationId), "Type" :: NullOrUndefined (OperationType), "Status" :: NullOrUndefined (OperationStatus), "ErrorMessage" :: NullOrUndefined (Message), "ErrorCode" :: NullOrUndefined (Code), "CreateDate" :: NullOrUndefined (Number), "UpdateDate" :: NullOrUndefined (Number), "Targets" :: NullOrUndefined (OperationTargetsMap) }
```

<p>A complex type that contains information about a specified operation.</p>

#### `OperationFilter`

``` purescript
newtype OperationFilter
  = OperationFilter { "Name" :: OperationFilterName, "Values" :: FilterValues, "Condition" :: NullOrUndefined (FilterCondition) }
```

<p>A complex type that lets you select the operations that you want to list.</p>

#### `OperationFilterName`

``` purescript
newtype OperationFilterName
  = OperationFilterName String
```

#### `OperationFilters`

``` purescript
newtype OperationFilters
  = OperationFilters (Array OperationFilter)
```

#### `OperationId`

``` purescript
newtype OperationId
  = OperationId String
```

#### `OperationNotFound`

``` purescript
newtype OperationNotFound
  = OperationNotFound { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>No operation exists with the specified ID.</p>

#### `OperationStatus`

``` purescript
newtype OperationStatus
  = OperationStatus String
```

#### `OperationSummary`

``` purescript
newtype OperationSummary
  = OperationSummary { "Id" :: NullOrUndefined (OperationId), "Status" :: NullOrUndefined (OperationStatus) }
```

<p>A complex type that contains information about an operation that matches the criteria that you specified in a <a>ListOperations</a> request.</p>

#### `OperationSummaryList`

``` purescript
newtype OperationSummaryList
  = OperationSummaryList (Array OperationSummary)
```

#### `OperationTargetType`

``` purescript
newtype OperationTargetType
  = OperationTargetType String
```

#### `OperationTargetsMap`

``` purescript
newtype OperationTargetsMap
  = OperationTargetsMap (Map OperationTargetType ResourceId)
```

#### `OperationType`

``` purescript
newtype OperationType
  = OperationType String
```

#### `RecordTTL`

``` purescript
newtype RecordTTL
  = RecordTTL Number
```

#### `RecordType`

``` purescript
newtype RecordType
  = RecordType String
```

#### `RegisterInstanceRequest`

``` purescript
newtype RegisterInstanceRequest
  = RegisterInstanceRequest { "ServiceId" :: ResourceId, "InstanceId" :: ResourceId, "CreatorRequestId" :: NullOrUndefined (ResourceId), "Attributes" :: Attributes }
```

#### `RegisterInstanceResponse`

``` purescript
newtype RegisterInstanceResponse
  = RegisterInstanceResponse { "OperationId" :: NullOrUndefined (OperationId) }
```

#### `ResourceCount`

``` purescript
newtype ResourceCount
  = ResourceCount Int
```

#### `ResourceDescription`

``` purescript
newtype ResourceDescription
  = ResourceDescription String
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

#### `ResourceInUse`

``` purescript
newtype ResourceInUse
  = ResourceInUse { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified resource can't be deleted because it contains other resources. For example, you can't delete a service that contains any instances.</p>

#### `ResourceLimitExceeded`

``` purescript
newtype ResourceLimitExceeded
  = ResourceLimitExceeded { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The resource can't be created because you've reached the limit on the number of resources.</p>

#### `ResourcePath`

``` purescript
newtype ResourcePath
  = ResourcePath String
```

#### `RoutingPolicy`

``` purescript
newtype RoutingPolicy
  = RoutingPolicy String
```

#### `Service`

``` purescript
newtype Service
  = Service { "Id" :: NullOrUndefined (ResourceId), "Arn" :: NullOrUndefined (Arn), "Name" :: NullOrUndefined (ServiceName), "Description" :: NullOrUndefined (ResourceDescription), "InstanceCount" :: NullOrUndefined (ResourceCount), "DnsConfig" :: NullOrUndefined (DnsConfig), "HealthCheckConfig" :: NullOrUndefined (HealthCheckConfig), "CreateDate" :: NullOrUndefined (Number), "CreatorRequestId" :: NullOrUndefined (ResourceId) }
```

<p>A complex type that contains information about the specified service.</p>

#### `ServiceAlreadyExists`

``` purescript
newtype ServiceAlreadyExists
  = ServiceAlreadyExists { "Message" :: NullOrUndefined (ErrorMessage), "CreatorRequestId" :: NullOrUndefined (ResourceId), "ServiceId" :: NullOrUndefined (ResourceId) }
```

<p>The service can't be created because a service with the same name already exists.</p>

#### `ServiceChange`

``` purescript
newtype ServiceChange
  = ServiceChange { "Description" :: NullOrUndefined (ResourceDescription), "DnsConfig" :: DnsConfigChange, "HealthCheckConfig" :: NullOrUndefined (HealthCheckConfig) }
```

<p>A complex type that contains changes to an existing service.</p>

#### `ServiceFilter`

``` purescript
newtype ServiceFilter
  = ServiceFilter { "Name" :: ServiceFilterName, "Values" :: FilterValues, "Condition" :: NullOrUndefined (FilterCondition) }
```

<p>A complex type that lets you specify the namespaces that you want to list services for.</p>

#### `ServiceFilterName`

``` purescript
newtype ServiceFilterName
  = ServiceFilterName String
```

#### `ServiceFilters`

``` purescript
newtype ServiceFilters
  = ServiceFilters (Array ServiceFilter)
```

#### `ServiceName`

``` purescript
newtype ServiceName
  = ServiceName String
```

#### `ServiceNotFound`

``` purescript
newtype ServiceNotFound
  = ServiceNotFound { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>No service exists with the specified ID.</p>

#### `ServiceSummariesList`

``` purescript
newtype ServiceSummariesList
  = ServiceSummariesList (Array ServiceSummary)
```

#### `ServiceSummary`

``` purescript
newtype ServiceSummary
  = ServiceSummary { "Id" :: NullOrUndefined (ResourceId), "Arn" :: NullOrUndefined (Arn), "Name" :: NullOrUndefined (ServiceName), "Description" :: NullOrUndefined (ResourceDescription), "InstanceCount" :: NullOrUndefined (ResourceCount) }
```

<p>A complex type that contains information about a specified service.</p>

#### `UpdateServiceRequest`

``` purescript
newtype UpdateServiceRequest
  = UpdateServiceRequest { "Id" :: ResourceId, "Service" :: ServiceChange }
```

#### `UpdateServiceResponse`

``` purescript
newtype UpdateServiceResponse
  = UpdateServiceResponse { "OperationId" :: NullOrUndefined (OperationId) }
```


