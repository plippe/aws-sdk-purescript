

-- | <p>Amazon Route 53 auto naming lets you configure public or private namespaces that your microservice applications run in. When instances of the service become available, you can call the auto naming API to register the instance, and Route 53 automatically creates up to five DNS records and an optional health check. Clients that submit DNS queries for the service receive an answer that contains up to eight healthy records.</p>
module AWS.ServiceDiscovery where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeArn :: Newtype Arn _


newtype AttrKey = AttrKey String
derive instance newtypeAttrKey :: Newtype AttrKey _


newtype AttrValue = AttrValue String
derive instance newtypeAttrValue :: Newtype AttrValue _


newtype Attributes = Attributes (Map AttrKey AttrValue)
derive instance newtypeAttributes :: Newtype Attributes _


newtype Code = Code String
derive instance newtypeCode :: Newtype Code _


newtype CreatePrivateDnsNamespaceRequest = CreatePrivateDnsNamespaceRequest 
  { "Name" :: (NamespaceName)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Description" :: NullOrUndefined (ResourceDescription)
  , "Vpc" :: (ResourceId)
  }
derive instance newtypeCreatePrivateDnsNamespaceRequest :: Newtype CreatePrivateDnsNamespaceRequest _


newtype CreatePrivateDnsNamespaceResponse = CreatePrivateDnsNamespaceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }
derive instance newtypeCreatePrivateDnsNamespaceResponse :: Newtype CreatePrivateDnsNamespaceResponse _


newtype CreatePublicDnsNamespaceRequest = CreatePublicDnsNamespaceRequest 
  { "Name" :: (NamespaceName)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Description" :: NullOrUndefined (ResourceDescription)
  }
derive instance newtypeCreatePublicDnsNamespaceRequest :: Newtype CreatePublicDnsNamespaceRequest _


newtype CreatePublicDnsNamespaceResponse = CreatePublicDnsNamespaceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }
derive instance newtypeCreatePublicDnsNamespaceResponse :: Newtype CreatePublicDnsNamespaceResponse _


newtype CreateServiceRequest = CreateServiceRequest 
  { "Name" :: (ServiceName)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Description" :: NullOrUndefined (ResourceDescription)
  , "DnsConfig" :: (DnsConfig)
  , "HealthCheckConfig" :: NullOrUndefined (HealthCheckConfig)
  }
derive instance newtypeCreateServiceRequest :: Newtype CreateServiceRequest _


newtype CreateServiceResponse = CreateServiceResponse 
  { "Service" :: NullOrUndefined (Service)
  }
derive instance newtypeCreateServiceResponse :: Newtype CreateServiceResponse _


newtype DeleteNamespaceRequest = DeleteNamespaceRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeDeleteNamespaceRequest :: Newtype DeleteNamespaceRequest _


newtype DeleteNamespaceResponse = DeleteNamespaceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }
derive instance newtypeDeleteNamespaceResponse :: Newtype DeleteNamespaceResponse _


newtype DeleteServiceRequest = DeleteServiceRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeDeleteServiceRequest :: Newtype DeleteServiceRequest _


newtype DeleteServiceResponse = DeleteServiceResponse 
  { 
  }
derive instance newtypeDeleteServiceResponse :: Newtype DeleteServiceResponse _


newtype DeregisterInstanceRequest = DeregisterInstanceRequest 
  { "ServiceId" :: (ResourceId)
  , "InstanceId" :: (ResourceId)
  }
derive instance newtypeDeregisterInstanceRequest :: Newtype DeregisterInstanceRequest _


newtype DeregisterInstanceResponse = DeregisterInstanceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }
derive instance newtypeDeregisterInstanceResponse :: Newtype DeregisterInstanceResponse _


-- | <p>A complex type that contains information about the records that you want Amazon Route 53 to create when you register an instance.</p>
newtype DnsConfig = DnsConfig 
  { "NamespaceId" :: (ResourceId)
  , "RoutingPolicy" :: NullOrUndefined (RoutingPolicy)
  , "DnsRecords" :: (DnsRecordList)
  }
derive instance newtypeDnsConfig :: Newtype DnsConfig _


-- | <p>A complex type that contains information about changes to the records that Route 53 creates when you register an instance.</p>
newtype DnsConfigChange = DnsConfigChange 
  { "DnsRecords" :: (DnsRecordList)
  }
derive instance newtypeDnsConfigChange :: Newtype DnsConfigChange _


-- | <p>A complex type that contains the ID for the hosted zone that Route 53 creates when you create a namespace.</p>
newtype DnsProperties = DnsProperties 
  { "HostedZoneId" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeDnsProperties :: Newtype DnsProperties _


-- | <p>A complex type that contains information about the records that you want Route 53 to create when you register an instance.</p>
newtype DnsRecord = DnsRecord 
  { "Type" :: (RecordType)
  , "TTL" :: (RecordTTL)
  }
derive instance newtypeDnsRecord :: Newtype DnsRecord _


newtype DnsRecordList = DnsRecordList (Array DnsRecord)
derive instance newtypeDnsRecordList :: Newtype DnsRecordList _


-- | <p>The operation is already in progress.</p>
newtype DuplicateRequest = DuplicateRequest 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDuplicateRequest :: Newtype DuplicateRequest _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype FailureThreshold = FailureThreshold Int
derive instance newtypeFailureThreshold :: Newtype FailureThreshold _


newtype FilterCondition = FilterCondition String
derive instance newtypeFilterCondition :: Newtype FilterCondition _


newtype FilterValue = FilterValue String
derive instance newtypeFilterValue :: Newtype FilterValue _


newtype FilterValues = FilterValues (Array FilterValue)
derive instance newtypeFilterValues :: Newtype FilterValues _


newtype GetInstanceRequest = GetInstanceRequest 
  { "ServiceId" :: (ResourceId)
  , "InstanceId" :: (ResourceId)
  }
derive instance newtypeGetInstanceRequest :: Newtype GetInstanceRequest _


newtype GetInstanceResponse = GetInstanceResponse 
  { "Instance" :: NullOrUndefined (Instance)
  }
derive instance newtypeGetInstanceResponse :: Newtype GetInstanceResponse _


newtype GetInstancesHealthStatusRequest = GetInstancesHealthStatusRequest 
  { "ServiceId" :: (ResourceId)
  , "Instances" :: NullOrUndefined (InstanceIdList)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetInstancesHealthStatusRequest :: Newtype GetInstancesHealthStatusRequest _


newtype GetInstancesHealthStatusResponse = GetInstancesHealthStatusResponse 
  { "Status" :: NullOrUndefined (InstanceHealthStatusMap)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetInstancesHealthStatusResponse :: Newtype GetInstancesHealthStatusResponse _


newtype GetNamespaceRequest = GetNamespaceRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeGetNamespaceRequest :: Newtype GetNamespaceRequest _


newtype GetNamespaceResponse = GetNamespaceResponse 
  { "Namespace" :: NullOrUndefined (Namespace)
  }
derive instance newtypeGetNamespaceResponse :: Newtype GetNamespaceResponse _


newtype GetOperationRequest = GetOperationRequest 
  { "OperationId" :: (ResourceId)
  }
derive instance newtypeGetOperationRequest :: Newtype GetOperationRequest _


newtype GetOperationResponse = GetOperationResponse 
  { "Operation" :: NullOrUndefined (Operation)
  }
derive instance newtypeGetOperationResponse :: Newtype GetOperationResponse _


newtype GetServiceRequest = GetServiceRequest 
  { "Id" :: (ResourceId)
  }
derive instance newtypeGetServiceRequest :: Newtype GetServiceRequest _


newtype GetServiceResponse = GetServiceResponse 
  { "Service" :: NullOrUndefined (Service)
  }
derive instance newtypeGetServiceResponse :: Newtype GetServiceResponse _


-- | <p> <i>Public DNS namespaces only.</i> A complex type that contains settings for an optional health check. If you specify settings for a health check, Amazon Route 53 associates the health check with all the records that you specify in <code>DnsConfig</code>.</p> <p> <b>A and AAAA records</b> </p> <p>If <code>DnsConfig</code> includes configurations for both A and AAAA records, Route 53 creates a health check that uses the IPv4 address to check the health of the resource. If the endpoint that is specified by the IPv4 address is unhealthy, Route 53 considers both the A and AAAA records to be unhealthy. </p> <p> <b>CNAME records</b> </p> <p>You can't specify settings for <code>HealthCheckConfig</code> when the <code>DNSConfig</code> includes <code>CNAME</code> for the value of <code>Type</code>. If you do, the <code>CreateService</code> request will fail with an <code>InvalidInput</code> error.</p> <p> <b>Request interval</b> </p> <p>The health check uses 30 seconds as the request interval. This is the number of seconds between the time that each Route 53 health checker gets a response from your endpoint and the time that it sends the next health check request. A health checker in each data center around the world sends your endpoint a health check request every 30 seconds. On average, your endpoint receives a health check request about every two seconds. Health checkers in different data centers don't coordinate with one another, so you'll sometimes see several requests per second followed by a few seconds with no health checks at all.</p> <p> <b>Health checking regions</b> </p> <p>Health checkers perform checks from all Route 53 health-checking regions. For a list of the current regions, see <a href="http://docs.aws.amazon.com/Route53/latest/APIReference/API_HealthCheckConfig.html#Route53-Type-HealthCheckConfig-Regions">Regions</a>.</p> <p> <b>Alias records</b> </p> <p>When you register an instance, if you include the <code>AWS_ALIAS_DNS_NAME</code> attribute, Route 53 creates an alias record. Note the following:</p> <ul> <li> <p>Route 53 automatically sets <code>EvaluateTargetHealth</code> to true for alias records. When <code>EvaluateTargetHealth</code> is true, the alias record inherits the health of the referenced AWS resource. such as an ELB load balancer. For more information, see <a href="http://docs.aws.amazon.com/Route53/latest/APIReference/API_AliasTarget.html#Route53-Type-AliasTarget-EvaluateTargetHealth">EvaluateTargetHealth</a>.</p> </li> <li> <p>If you include <code>HealthCheckConfig</code> and then use the service to register an instance that creates an alias record, Route 53 doesn't create the health check.</p> </li> </ul> <p>For information about the charges for health checks, see <a href="http://aws.amazon.com/route53/pricing">Route 53 Pricing</a>.</p>
newtype HealthCheckConfig = HealthCheckConfig 
  { "Type" :: (HealthCheckType)
  , "ResourcePath" :: NullOrUndefined (ResourcePath)
  , "FailureThreshold" :: NullOrUndefined (FailureThreshold)
  }
derive instance newtypeHealthCheckConfig :: Newtype HealthCheckConfig _


newtype HealthCheckType = HealthCheckType String
derive instance newtypeHealthCheckType :: Newtype HealthCheckType _


newtype HealthStatus = HealthStatus String
derive instance newtypeHealthStatus :: Newtype HealthStatus _


-- | <p>A complex type that contains information about an instance that Amazon Route 53 creates when you submit a <code>RegisterInstance</code> request.</p>
newtype Instance = Instance 
  { "Id" :: (ResourceId)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Attributes" :: NullOrUndefined (Attributes)
  }
derive instance newtypeInstance :: Newtype Instance _


newtype InstanceHealthStatusMap = InstanceHealthStatusMap (Map ResourceId HealthStatus)
derive instance newtypeInstanceHealthStatusMap :: Newtype InstanceHealthStatusMap _


newtype InstanceIdList = InstanceIdList (Array ResourceId)
derive instance newtypeInstanceIdList :: Newtype InstanceIdList _


-- | <p>No instance exists with the specified ID, or the instance was recently registered, and information about the instance hasn't propagated yet.</p>
newtype InstanceNotFound = InstanceNotFound 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInstanceNotFound :: Newtype InstanceNotFound _


-- | <p>A complex type that contains information about the instances that you registered by using a specified service.</p>
newtype InstanceSummary = InstanceSummary 
  { "Id" :: NullOrUndefined (ResourceId)
  , "Attributes" :: NullOrUndefined (Attributes)
  }
derive instance newtypeInstanceSummary :: Newtype InstanceSummary _


newtype InstanceSummaryList = InstanceSummaryList (Array InstanceSummary)
derive instance newtypeInstanceSummaryList :: Newtype InstanceSummaryList _


-- | <p>One or more specified values aren't valid. For example, when you're creating a namespace, the value of <code>Name</code> might not be a valid DNS name.</p>
newtype InvalidInput = InvalidInput 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidInput :: Newtype InvalidInput _


newtype ListInstancesRequest = ListInstancesRequest 
  { "ServiceId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListInstancesRequest :: Newtype ListInstancesRequest _


newtype ListInstancesResponse = ListInstancesResponse 
  { "Instances" :: NullOrUndefined (InstanceSummaryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListInstancesResponse :: Newtype ListInstancesResponse _


newtype ListNamespacesRequest = ListNamespacesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (NamespaceFilters)
  }
derive instance newtypeListNamespacesRequest :: Newtype ListNamespacesRequest _


newtype ListNamespacesResponse = ListNamespacesResponse 
  { "Namespaces" :: NullOrUndefined (NamespaceSummariesList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListNamespacesResponse :: Newtype ListNamespacesResponse _


newtype ListOperationsRequest = ListOperationsRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (OperationFilters)
  }
derive instance newtypeListOperationsRequest :: Newtype ListOperationsRequest _


newtype ListOperationsResponse = ListOperationsResponse 
  { "Operations" :: NullOrUndefined (OperationSummaryList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListOperationsResponse :: Newtype ListOperationsResponse _


newtype ListServicesRequest = ListServicesRequest 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  , "Filters" :: NullOrUndefined (ServiceFilters)
  }
derive instance newtypeListServicesRequest :: Newtype ListServicesRequest _


newtype ListServicesResponse = ListServicesResponse 
  { "Services" :: NullOrUndefined (ServiceSummariesList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListServicesResponse :: Newtype ListServicesResponse _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


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
derive instance newtypeNamespace :: Newtype Namespace _


-- | <p>The namespace that you're trying to create already exists.</p>
newtype NamespaceAlreadyExists = NamespaceAlreadyExists 
  { "Message" :: NullOrUndefined (ErrorMessage)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "NamespaceId" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeNamespaceAlreadyExists :: Newtype NamespaceAlreadyExists _


-- | <p>A complex type that identifies the namespaces that you want to list. You can choose to list public or private namespaces.</p>
newtype NamespaceFilter = NamespaceFilter 
  { "Name" :: (NamespaceFilterName)
  , "Values" :: (FilterValues)
  , "Condition" :: NullOrUndefined (FilterCondition)
  }
derive instance newtypeNamespaceFilter :: Newtype NamespaceFilter _


newtype NamespaceFilterName = NamespaceFilterName String
derive instance newtypeNamespaceFilterName :: Newtype NamespaceFilterName _


newtype NamespaceFilters = NamespaceFilters (Array NamespaceFilter)
derive instance newtypeNamespaceFilters :: Newtype NamespaceFilters _


newtype NamespaceName = NamespaceName String
derive instance newtypeNamespaceName :: Newtype NamespaceName _


-- | <p>No namespace exists with the specified ID.</p>
newtype NamespaceNotFound = NamespaceNotFound 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNamespaceNotFound :: Newtype NamespaceNotFound _


-- | <p>A complex type that contains information that is specific to the namespace type.</p>
newtype NamespaceProperties = NamespaceProperties 
  { "DnsProperties" :: NullOrUndefined (DnsProperties)
  }
derive instance newtypeNamespaceProperties :: Newtype NamespaceProperties _


newtype NamespaceSummariesList = NamespaceSummariesList (Array NamespaceSummary)
derive instance newtypeNamespaceSummariesList :: Newtype NamespaceSummariesList _


-- | <p>A complex type that contains information about a namespace.</p>
newtype NamespaceSummary = NamespaceSummary 
  { "Id" :: NullOrUndefined (ResourceId)
  , "Arn" :: NullOrUndefined (Arn)
  , "Name" :: NullOrUndefined (NamespaceName)
  , "Type" :: NullOrUndefined (NamespaceType)
  }
derive instance newtypeNamespaceSummary :: Newtype NamespaceSummary _


newtype NamespaceType = NamespaceType String
derive instance newtypeNamespaceType :: Newtype NamespaceType _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


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
derive instance newtypeOperation :: Newtype Operation _


-- | <p>A complex type that lets you select the operations that you want to list.</p>
newtype OperationFilter = OperationFilter 
  { "Name" :: (OperationFilterName)
  , "Values" :: (FilterValues)
  , "Condition" :: NullOrUndefined (FilterCondition)
  }
derive instance newtypeOperationFilter :: Newtype OperationFilter _


newtype OperationFilterName = OperationFilterName String
derive instance newtypeOperationFilterName :: Newtype OperationFilterName _


newtype OperationFilters = OperationFilters (Array OperationFilter)
derive instance newtypeOperationFilters :: Newtype OperationFilters _


newtype OperationId = OperationId String
derive instance newtypeOperationId :: Newtype OperationId _


-- | <p>No operation exists with the specified ID.</p>
newtype OperationNotFound = OperationNotFound 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeOperationNotFound :: Newtype OperationNotFound _


newtype OperationStatus = OperationStatus String
derive instance newtypeOperationStatus :: Newtype OperationStatus _


-- | <p>A complex type that contains information about an operation that matches the criteria that you specified in a <a>ListOperations</a> request.</p>
newtype OperationSummary = OperationSummary 
  { "Id" :: NullOrUndefined (OperationId)
  , "Status" :: NullOrUndefined (OperationStatus)
  }
derive instance newtypeOperationSummary :: Newtype OperationSummary _


newtype OperationSummaryList = OperationSummaryList (Array OperationSummary)
derive instance newtypeOperationSummaryList :: Newtype OperationSummaryList _


newtype OperationTargetType = OperationTargetType String
derive instance newtypeOperationTargetType :: Newtype OperationTargetType _


newtype OperationTargetsMap = OperationTargetsMap (Map OperationTargetType ResourceId)
derive instance newtypeOperationTargetsMap :: Newtype OperationTargetsMap _


newtype OperationType = OperationType String
derive instance newtypeOperationType :: Newtype OperationType _


newtype RecordTTL = RecordTTL Number
derive instance newtypeRecordTTL :: Newtype RecordTTL _


newtype RecordType = RecordType String
derive instance newtypeRecordType :: Newtype RecordType _


newtype RegisterInstanceRequest = RegisterInstanceRequest 
  { "ServiceId" :: (ResourceId)
  , "InstanceId" :: (ResourceId)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "Attributes" :: (Attributes)
  }
derive instance newtypeRegisterInstanceRequest :: Newtype RegisterInstanceRequest _


newtype RegisterInstanceResponse = RegisterInstanceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }
derive instance newtypeRegisterInstanceResponse :: Newtype RegisterInstanceResponse _


newtype ResourceCount = ResourceCount Int
derive instance newtypeResourceCount :: Newtype ResourceCount _


newtype ResourceDescription = ResourceDescription String
derive instance newtypeResourceDescription :: Newtype ResourceDescription _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


-- | <p>The specified resource can't be deleted because it contains other resources. For example, you can't delete a service that contains any instances.</p>
newtype ResourceInUse = ResourceInUse 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUse :: Newtype ResourceInUse _


-- | <p>The resource can't be created because you've reached the limit on the number of resources.</p>
newtype ResourceLimitExceeded = ResourceLimitExceeded 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceLimitExceeded :: Newtype ResourceLimitExceeded _


newtype ResourcePath = ResourcePath String
derive instance newtypeResourcePath :: Newtype ResourcePath _


newtype RoutingPolicy = RoutingPolicy String
derive instance newtypeRoutingPolicy :: Newtype RoutingPolicy _


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
derive instance newtypeService :: Newtype Service _


-- | <p>The service can't be created because a service with the same name already exists.</p>
newtype ServiceAlreadyExists = ServiceAlreadyExists 
  { "Message" :: NullOrUndefined (ErrorMessage)
  , "CreatorRequestId" :: NullOrUndefined (ResourceId)
  , "ServiceId" :: NullOrUndefined (ResourceId)
  }
derive instance newtypeServiceAlreadyExists :: Newtype ServiceAlreadyExists _


-- | <p>A complex type that contains changes to an existing service.</p>
newtype ServiceChange = ServiceChange 
  { "Description" :: NullOrUndefined (ResourceDescription)
  , "DnsConfig" :: (DnsConfigChange)
  , "HealthCheckConfig" :: NullOrUndefined (HealthCheckConfig)
  }
derive instance newtypeServiceChange :: Newtype ServiceChange _


-- | <p>A complex type that lets you specify the namespaces that you want to list services for.</p>
newtype ServiceFilter = ServiceFilter 
  { "Name" :: (ServiceFilterName)
  , "Values" :: (FilterValues)
  , "Condition" :: NullOrUndefined (FilterCondition)
  }
derive instance newtypeServiceFilter :: Newtype ServiceFilter _


newtype ServiceFilterName = ServiceFilterName String
derive instance newtypeServiceFilterName :: Newtype ServiceFilterName _


newtype ServiceFilters = ServiceFilters (Array ServiceFilter)
derive instance newtypeServiceFilters :: Newtype ServiceFilters _


newtype ServiceName = ServiceName String
derive instance newtypeServiceName :: Newtype ServiceName _


-- | <p>No service exists with the specified ID.</p>
newtype ServiceNotFound = ServiceNotFound 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceNotFound :: Newtype ServiceNotFound _


newtype ServiceSummariesList = ServiceSummariesList (Array ServiceSummary)
derive instance newtypeServiceSummariesList :: Newtype ServiceSummariesList _


-- | <p>A complex type that contains information about a specified service.</p>
newtype ServiceSummary = ServiceSummary 
  { "Id" :: NullOrUndefined (ResourceId)
  , "Arn" :: NullOrUndefined (Arn)
  , "Name" :: NullOrUndefined (ServiceName)
  , "Description" :: NullOrUndefined (ResourceDescription)
  , "InstanceCount" :: NullOrUndefined (ResourceCount)
  }
derive instance newtypeServiceSummary :: Newtype ServiceSummary _


newtype UpdateServiceRequest = UpdateServiceRequest 
  { "Id" :: (ResourceId)
  , "Service" :: (ServiceChange)
  }
derive instance newtypeUpdateServiceRequest :: Newtype UpdateServiceRequest _


newtype UpdateServiceResponse = UpdateServiceResponse 
  { "OperationId" :: NullOrUndefined (OperationId)
  }
derive instance newtypeUpdateServiceResponse :: Newtype UpdateServiceResponse _
