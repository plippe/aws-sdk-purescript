## Module AWS.Discovery

<fullname>AWS Application Discovery Service</fullname> <p>AWS Application Discovery Service helps you plan application migration projects by automatically identifying servers, virtual machines (VMs), software, and software dependencies running in your on-premises data centers. Application Discovery Service also collects application performance data, which can help you assess the outcome of your migration. The data collected by Application Discovery Service is securely retained in an Amazon-hosted and managed database in the cloud. You can export the data as a CSV or XML file into your preferred visualization tool or cloud-migration solution to plan your migration. For more information, see the Application Discovery Service <a href="http://aws.amazon.com/application-discovery/faqs/">FAQ</a>.</p> <p>Application Discovery Service offers two modes of operation.</p> <ul> <li> <p> <b>Agentless discovery</b> mode is recommended for environments that use VMware vCenter Server. This mode doesn't require you to install an agent on each host. Agentless discovery gathers server information regardless of the operating systems, which minimizes the time required for initial on-premises infrastructure assessment. Agentless discovery doesn't collect information about software and software dependencies. It also doesn't work in non-VMware environments. We recommend that you use agent-based discovery for non-VMware environments and if you want to collect information about software and software dependencies. You can also run agent-based and agentless discovery simultaneously. Use agentless discovery to quickly complete the initial infrastructure assessment and then install agents on select hosts to gather information about software and software dependencies.</p> </li> <li> <p> <b>Agent-based discovery</b> mode collects a richer set of data than agentless discovery by using Amazon software, the AWS Application Discovery Agent, which you install on one or more hosts in your data center. The agent captures infrastructure and application information, including an inventory of installed software applications, system and process performance, resource utilization, and network dependencies between workloads. The information collected by agents is secured at rest and in transit to the Application Discovery Service database in the cloud. </p> </li> </ul> <p>Application Discovery Service integrates with application discovery solutions from AWS Partner Network (APN) partners. Third-party application discovery tools can query Application Discovery Service and write to the Application Discovery Service database using a public API. You can then import the data into either a visualization tool or cloud-migration solution.</p> <important> <p>Application Discovery Service doesn't gather sensitive information. All data is handled according to the <a href="http://aws.amazon.com/privacy/">AWS Privacy Policy</a>. You can operate Application Discovery Service using offline mode to inspect collected data before it is shared with the service.</p> </important> <p>Your AWS account must be granted access to Application Discovery Service, a process called <i>whitelisting</i>. This is true for AWS partners and customers alike. To request access, <a href="http://aws.amazon.com/application-discovery/how-to-start/"> sign up for AWS Application Discovery Service</a>.</p> <p>This API reference provides descriptions, syntax, and usage examples for each of the actions and data types for Application Discovery Service. The topic for each action shows the API request parameters and the response. Alternatively, you can use one of the AWS SDKs to access an API that is tailored to the programming language or platform that you're using. For more information, see <a href="http://aws.amazon.com/tools/#SDKs">AWS SDKs</a>.</p> <p>This guide is intended for use with the <a href="http://docs.aws.amazon.com/application-discovery/latest/userguide/"> <i>AWS Application Discovery Service User Guide</i> </a>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateConfigurationItemsToApplication`

``` purescript
associateConfigurationItemsToApplication :: forall eff. AssociateConfigurationItemsToApplicationRequest -> Aff (err :: RequestError | eff) AssociateConfigurationItemsToApplicationResponse
```

<p>Associates one or more configuration items with an application.</p>

#### `createApplication`

``` purescript
createApplication :: forall eff. CreateApplicationRequest -> Aff (err :: RequestError | eff) CreateApplicationResponse
```

<p>Creates an application with the given name and description.</p>

#### `createTags`

``` purescript
createTags :: forall eff. CreateTagsRequest -> Aff (err :: RequestError | eff) CreateTagsResponse
```

<p>Creates one or more tags for configuration items. Tags are metadata that help you categorize IT assets. This API accepts a list of multiple configuration items.</p>

#### `deleteApplications`

``` purescript
deleteApplications :: forall eff. DeleteApplicationsRequest -> Aff (err :: RequestError | eff) DeleteApplicationsResponse
```

<p>Deletes a list of applications and their associations with configuration items.</p>

#### `deleteTags`

``` purescript
deleteTags :: forall eff. DeleteTagsRequest -> Aff (err :: RequestError | eff) DeleteTagsResponse
```

<p>Deletes the association between configuration items and one or more tags. This API accepts a list of multiple configuration items.</p>

#### `describeAgents`

``` purescript
describeAgents :: forall eff. DescribeAgentsRequest -> Aff (err :: RequestError | eff) DescribeAgentsResponse
```

<p>Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an ID.</p>

#### `describeConfigurations`

``` purescript
describeConfigurations :: forall eff. DescribeConfigurationsRequest -> Aff (err :: RequestError | eff) DescribeConfigurationsResponse
```

<p>Retrieves attributes for a list of configuration item IDs. All of the supplied IDs must be for the same asset type (server, application, process, or connection). Output fields are specific to the asset type selected. For example, the output for a <i>server</i> configuration item includes a list of attributes about the server, such as host name, operating system, and number of network cards.</p> <p>For a complete list of outputs for each asset type, see <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html#DescribeConfigurations">Using the DescribeConfigurations Action</a>.</p>

#### `describeExportConfigurations`

``` purescript
describeExportConfigurations :: forall eff. DescribeExportConfigurationsRequest -> Aff (err :: RequestError | eff) DescribeExportConfigurationsResponse
```

<p>Deprecated. Use <code>DescribeExportTasks</code> instead.</p> <p>Retrieves the status of a given export process. You can retrieve status from a maximum of 100 processes.</p>

#### `describeExportTasks`

``` purescript
describeExportTasks :: forall eff. DescribeExportTasksRequest -> Aff (err :: RequestError | eff) DescribeExportTasksResponse
```

<p>Retrieve status of one or more export tasks. You can retrieve the status of up to 100 export tasks.</p>

#### `describeTags`

``` purescript
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: RequestError | eff) DescribeTagsResponse
```

<p>Retrieves a list of configuration items that are tagged with a specific tag. Or retrieves a list of all tags assigned to a specific configuration item.</p>

#### `disassociateConfigurationItemsFromApplication`

``` purescript
disassociateConfigurationItemsFromApplication :: forall eff. DisassociateConfigurationItemsFromApplicationRequest -> Aff (err :: RequestError | eff) DisassociateConfigurationItemsFromApplicationResponse
```

<p>Disassociates one or more configuration items from an application.</p>

#### `exportConfigurations`

``` purescript
exportConfigurations :: forall eff. Aff (err :: RequestError | eff) ExportConfigurationsResponse
```

<p>Deprecated. Use <code>StartExportTask</code> instead.</p> <p>Exports all discovered configuration data to an Amazon S3 bucket or an application that enables you to view and evaluate the data. Data includes tags and tag associations, processes, connections, servers, and system performance. This API returns an export ID that you can query using the <i>DescribeExportConfigurations</i> API. The system imposes a limit of two configuration exports in six hours.</p>

#### `getDiscoverySummary`

``` purescript
getDiscoverySummary :: forall eff. GetDiscoverySummaryRequest -> Aff (err :: RequestError | eff) GetDiscoverySummaryResponse
```

<p>Retrieves a short summary of discovered assets.</p>

#### `listConfigurations`

``` purescript
listConfigurations :: forall eff. ListConfigurationsRequest -> Aff (err :: RequestError | eff) ListConfigurationsResponse
```

<p>Retrieves a list of configuration items according to criteria that you specify in a filter. The filter criteria identifies the relationship requirements.</p>

#### `listServerNeighbors`

``` purescript
listServerNeighbors :: forall eff. ListServerNeighborsRequest -> Aff (err :: RequestError | eff) ListServerNeighborsResponse
```

<p>Retrieves a list of servers that are one network hop away from a specified server.</p>

#### `startDataCollectionByAgentIds`

``` purescript
startDataCollectionByAgentIds :: forall eff. StartDataCollectionByAgentIdsRequest -> Aff (err :: RequestError | eff) StartDataCollectionByAgentIdsResponse
```

<p>Instructs the specified agents or connectors to start collecting data.</p>

#### `startExportTask`

``` purescript
startExportTask :: forall eff. StartExportTaskRequest -> Aff (err :: RequestError | eff) StartExportTaskResponse
```

<p> Begins the export of discovered data to an S3 bucket.</p> <p> If you specify <code>agentIds</code> in a filter, the task exports up to 72 hours of detailed data collected by the identified Application Discovery Agent, including network, process, and performance details. A time range for exported agent data may be set by using <code>startTime</code> and <code>endTime</code>. Export of detailed agent data is limited to five concurrently running exports. </p> <p> If you do not include an <code>agentIds</code> filter, summary data is exported that includes both AWS Agentless Discovery Connector data and summary data from AWS Discovery Agents. Export of summary data is limited to two exports per day. </p>

#### `stopDataCollectionByAgentIds`

``` purescript
stopDataCollectionByAgentIds :: forall eff. StopDataCollectionByAgentIdsRequest -> Aff (err :: RequestError | eff) StopDataCollectionByAgentIdsResponse
```

<p>Instructs the specified agents or connectors to stop collecting data.</p>

#### `updateApplication`

``` purescript
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (err :: RequestError | eff) UpdateApplicationResponse
```

<p>Updates metadata about an application.</p>

#### `AgentConfigurationStatus`

``` purescript
newtype AgentConfigurationStatus
  = AgentConfigurationStatus { "AgentId'" :: NullOrUndefined (String), "OperationSucceeded'" :: NullOrUndefined (Boolean), "Description'" :: NullOrUndefined (String) }
```

<p>Information about agents or connectors that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation, and whether the agent/connector configuration was updated.</p>

#### `AgentConfigurationStatusList`

``` purescript
newtype AgentConfigurationStatusList
  = AgentConfigurationStatusList (Array AgentConfigurationStatus)
```

#### `AgentId`

``` purescript
newtype AgentId
  = AgentId String
```

#### `AgentIds`

``` purescript
newtype AgentIds
  = AgentIds (Array AgentId)
```

#### `AgentInfo`

``` purescript
newtype AgentInfo
  = AgentInfo { "AgentId'" :: NullOrUndefined (AgentId), "HostName'" :: NullOrUndefined (String), "AgentNetworkInfoList'" :: NullOrUndefined (AgentNetworkInfoList), "ConnectorId'" :: NullOrUndefined (String), "Version'" :: NullOrUndefined (String), "Health'" :: NullOrUndefined (AgentStatus), "LastHealthPingTime'" :: NullOrUndefined (String), "CollectionStatus'" :: NullOrUndefined (String), "AgentType'" :: NullOrUndefined (String), "RegisteredTime'" :: NullOrUndefined (String) }
```

<p>Information about agents or connectors associated with the user’s AWS account. Information includes agent or connector IDs, IP addresses, media access control (MAC) addresses, agent or connector health, hostname where the agent or connector resides, and agent version for each agent.</p>

#### `AgentNetworkInfo`

``` purescript
newtype AgentNetworkInfo
  = AgentNetworkInfo { "IpAddress'" :: NullOrUndefined (String), "MacAddress'" :: NullOrUndefined (String) }
```

<p>Network details about the host where the agent/connector resides.</p>

#### `AgentNetworkInfoList`

``` purescript
newtype AgentNetworkInfoList
  = AgentNetworkInfoList (Array AgentNetworkInfo)
```

#### `AgentStatus`

``` purescript
newtype AgentStatus
  = AgentStatus String
```

#### `AgentsInfo`

``` purescript
newtype AgentsInfo
  = AgentsInfo (Array AgentInfo)
```

#### `ApplicationId`

``` purescript
newtype ApplicationId
  = ApplicationId String
```

#### `ApplicationIdsList`

``` purescript
newtype ApplicationIdsList
  = ApplicationIdsList (Array ApplicationId)
```

#### `AssociateConfigurationItemsToApplicationRequest`

``` purescript
newtype AssociateConfigurationItemsToApplicationRequest
  = AssociateConfigurationItemsToApplicationRequest { "ApplicationConfigurationId'" :: ApplicationId, "ConfigurationIds'" :: ConfigurationIdList }
```

#### `AssociateConfigurationItemsToApplicationResponse`

``` purescript
newtype AssociateConfigurationItemsToApplicationResponse
  = AssociateConfigurationItemsToApplicationResponse {  }
```

#### `AuthorizationErrorException`

``` purescript
newtype AuthorizationErrorException
  = AuthorizationErrorException { "Message'" :: NullOrUndefined (Message) }
```

<p>The AWS user account does not have permission to perform the action. Check the IAM policy associated with this account.</p>

#### `BoxedInteger`

``` purescript
newtype BoxedInteger
  = BoxedInteger Int
```

#### `Condition`

``` purescript
newtype Condition
  = Condition String
```

#### `Configuration`

``` purescript
newtype Configuration
  = Configuration (Map String String)
```

#### `ConfigurationId`

``` purescript
newtype ConfigurationId
  = ConfigurationId String
```

#### `ConfigurationIdList`

``` purescript
newtype ConfigurationIdList
  = ConfigurationIdList (Array ConfigurationId)
```

#### `ConfigurationItemType`

``` purescript
newtype ConfigurationItemType
  = ConfigurationItemType String
```

#### `ConfigurationTag`

``` purescript
newtype ConfigurationTag
  = ConfigurationTag { "ConfigurationType'" :: NullOrUndefined (ConfigurationItemType), "ConfigurationId'" :: NullOrUndefined (ConfigurationId), "Key'" :: NullOrUndefined (TagKey), "Value'" :: NullOrUndefined (TagValue), "TimeOfCreation'" :: NullOrUndefined (TimeStamp) }
```

<p>Tags for a configuration item. Tags are metadata that help you categorize IT assets.</p>

#### `ConfigurationTagSet`

``` purescript
newtype ConfigurationTagSet
  = ConfigurationTagSet (Array ConfigurationTag)
```

#### `Configurations`

``` purescript
newtype Configurations
  = Configurations (Array Configuration)
```

#### `ConfigurationsDownloadUrl`

``` purescript
newtype ConfigurationsDownloadUrl
  = ConfigurationsDownloadUrl String
```

#### `ConfigurationsExportId`

``` purescript
newtype ConfigurationsExportId
  = ConfigurationsExportId String
```

#### `CreateApplicationRequest`

``` purescript
newtype CreateApplicationRequest
  = CreateApplicationRequest { "Name'" :: String, "Description'" :: NullOrUndefined (String) }
```

#### `CreateApplicationResponse`

``` purescript
newtype CreateApplicationResponse
  = CreateApplicationResponse { "ConfigurationId'" :: NullOrUndefined (String) }
```

#### `CreateTagsRequest`

``` purescript
newtype CreateTagsRequest
  = CreateTagsRequest { "ConfigurationIds'" :: ConfigurationIdList, "Tags'" :: TagSet }
```

#### `CreateTagsResponse`

``` purescript
newtype CreateTagsResponse
  = CreateTagsResponse {  }
```

#### `CustomerAgentInfo`

``` purescript
newtype CustomerAgentInfo
  = CustomerAgentInfo { "ActiveAgents'" :: Int, "HealthyAgents'" :: Int, "BlackListedAgents'" :: Int, "ShutdownAgents'" :: Int, "UnhealthyAgents'" :: Int, "TotalAgents'" :: Int, "UnknownAgents'" :: Int }
```

<p>Inventory data for installed discovery agents.</p>

#### `CustomerConnectorInfo`

``` purescript
newtype CustomerConnectorInfo
  = CustomerConnectorInfo { "ActiveConnectors'" :: Int, "HealthyConnectors'" :: Int, "BlackListedConnectors'" :: Int, "ShutdownConnectors'" :: Int, "UnhealthyConnectors'" :: Int, "TotalConnectors'" :: Int, "UnknownConnectors'" :: Int }
```

<p>Inventory data for installed discovery connectors.</p>

#### `DeleteApplicationsRequest`

``` purescript
newtype DeleteApplicationsRequest
  = DeleteApplicationsRequest { "ConfigurationIds'" :: ApplicationIdsList }
```

#### `DeleteApplicationsResponse`

``` purescript
newtype DeleteApplicationsResponse
  = DeleteApplicationsResponse {  }
```

#### `DeleteTagsRequest`

``` purescript
newtype DeleteTagsRequest
  = DeleteTagsRequest { "ConfigurationIds'" :: ConfigurationIdList, "Tags'" :: NullOrUndefined (TagSet) }
```

#### `DeleteTagsResponse`

``` purescript
newtype DeleteTagsResponse
  = DeleteTagsResponse {  }
```

#### `DescribeAgentsRequest`

``` purescript
newtype DescribeAgentsRequest
  = DescribeAgentsRequest { "AgentIds'" :: NullOrUndefined (AgentIds), "Filters'" :: NullOrUndefined (Filters), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DescribeAgentsResponse`

``` purescript
newtype DescribeAgentsResponse
  = DescribeAgentsResponse { "AgentsInfo'" :: NullOrUndefined (AgentsInfo), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DescribeConfigurationsAttribute`

``` purescript
newtype DescribeConfigurationsAttribute
  = DescribeConfigurationsAttribute (Map String String)
```

#### `DescribeConfigurationsAttributes`

``` purescript
newtype DescribeConfigurationsAttributes
  = DescribeConfigurationsAttributes (Array DescribeConfigurationsAttribute)
```

#### `DescribeConfigurationsRequest`

``` purescript
newtype DescribeConfigurationsRequest
  = DescribeConfigurationsRequest { "ConfigurationIds'" :: ConfigurationIdList }
```

#### `DescribeConfigurationsResponse`

``` purescript
newtype DescribeConfigurationsResponse
  = DescribeConfigurationsResponse { "Configurations'" :: NullOrUndefined (DescribeConfigurationsAttributes) }
```

#### `DescribeExportConfigurationsRequest`

``` purescript
newtype DescribeExportConfigurationsRequest
  = DescribeExportConfigurationsRequest { "ExportIds'" :: NullOrUndefined (ExportIds), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DescribeExportConfigurationsResponse`

``` purescript
newtype DescribeExportConfigurationsResponse
  = DescribeExportConfigurationsResponse { "ExportsInfo'" :: NullOrUndefined (ExportsInfo), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DescribeExportTasksRequest`

``` purescript
newtype DescribeExportTasksRequest
  = DescribeExportTasksRequest { "ExportIds'" :: NullOrUndefined (ExportIds), "Filters'" :: NullOrUndefined (ExportFilters), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DescribeExportTasksResponse`

``` purescript
newtype DescribeExportTasksResponse
  = DescribeExportTasksResponse { "ExportsInfo'" :: NullOrUndefined (ExportsInfo), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DescribeTagsRequest`

``` purescript
newtype DescribeTagsRequest
  = DescribeTagsRequest { "Filters'" :: NullOrUndefined (TagFilters), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DescribeTagsResponse`

``` purescript
newtype DescribeTagsResponse
  = DescribeTagsResponse { "Tags'" :: NullOrUndefined (ConfigurationTagSet), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `DisassociateConfigurationItemsFromApplicationRequest`

``` purescript
newtype DisassociateConfigurationItemsFromApplicationRequest
  = DisassociateConfigurationItemsFromApplicationRequest { "ApplicationConfigurationId'" :: ApplicationId, "ConfigurationIds'" :: ConfigurationIdList }
```

#### `DisassociateConfigurationItemsFromApplicationResponse`

``` purescript
newtype DisassociateConfigurationItemsFromApplicationResponse
  = DisassociateConfigurationItemsFromApplicationResponse {  }
```

#### `ExportConfigurationsResponse`

``` purescript
newtype ExportConfigurationsResponse
  = ExportConfigurationsResponse { "ExportId'" :: NullOrUndefined (ConfigurationsExportId) }
```

#### `ExportDataFormat`

``` purescript
newtype ExportDataFormat
  = ExportDataFormat String
```

#### `ExportDataFormats`

``` purescript
newtype ExportDataFormats
  = ExportDataFormats (Array ExportDataFormat)
```

#### `ExportFilter`

``` purescript
newtype ExportFilter
  = ExportFilter { "Name'" :: FilterName, "Values'" :: FilterValues, "Condition'" :: Condition }
```

<p>Used to select which agent's data is to be exported. A single agent ID may be selected for export using the <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html">StartExportTask</a> action.</p>

#### `ExportFilters`

``` purescript
newtype ExportFilters
  = ExportFilters (Array ExportFilter)
```

#### `ExportIds`

``` purescript
newtype ExportIds
  = ExportIds (Array ConfigurationsExportId)
```

#### `ExportInfo`

``` purescript
newtype ExportInfo
  = ExportInfo { "ExportId'" :: ConfigurationsExportId, "ExportStatus'" :: ExportStatus, "StatusMessage'" :: ExportStatusMessage, "ConfigurationsDownloadUrl'" :: NullOrUndefined (ConfigurationsDownloadUrl), "ExportRequestTime'" :: ExportRequestTime, "IsTruncated'" :: NullOrUndefined (Boolean), "RequestedStartTime'" :: NullOrUndefined (TimeStamp), "RequestedEndTime'" :: NullOrUndefined (TimeStamp) }
```

<p>Information regarding the export status of discovered data. The value is an array of objects.</p>

#### `ExportRequestTime`

``` purescript
newtype ExportRequestTime
  = ExportRequestTime Number
```

#### `ExportStatus`

``` purescript
newtype ExportStatus
  = ExportStatus String
```

#### `ExportStatusMessage`

``` purescript
newtype ExportStatusMessage
  = ExportStatusMessage String
```

#### `ExportsInfo`

``` purescript
newtype ExportsInfo
  = ExportsInfo (Array ExportInfo)
```

#### `Filter`

``` purescript
newtype Filter
  = Filter { "Name'" :: String, "Values'" :: FilterValues, "Condition'" :: Condition }
```

<p>A filter that can use conditional operators.</p> <p>For more information about filters, see <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html">Querying Discovered Configuration Items</a>. </p>

#### `FilterName`

``` purescript
newtype FilterName
  = FilterName String
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

#### `Filters`

``` purescript
newtype Filters
  = Filters (Array Filter)
```

#### `GetDiscoverySummaryRequest`

``` purescript
newtype GetDiscoverySummaryRequest
  = GetDiscoverySummaryRequest {  }
```

#### `GetDiscoverySummaryResponse`

``` purescript
newtype GetDiscoverySummaryResponse
  = GetDiscoverySummaryResponse { "Servers'" :: NullOrUndefined (Number), "Applications'" :: NullOrUndefined (Number), "ServersMappedToApplications'" :: NullOrUndefined (Number), "ServersMappedtoTags'" :: NullOrUndefined (Number), "AgentSummary'" :: NullOrUndefined (CustomerAgentInfo), "ConnectorSummary'" :: NullOrUndefined (CustomerConnectorInfo) }
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (Message) }
```

<p>One or more parameters are not valid. Verify the parameters and try again.</p>

#### `InvalidParameterValueException`

``` purescript
newtype InvalidParameterValueException
  = InvalidParameterValueException { "Message'" :: NullOrUndefined (Message) }
```

<p>The value of one or more parameters are either invalid or out of range. Verify the parameter values and try again.</p>

#### `ListConfigurationsRequest`

``` purescript
newtype ListConfigurationsRequest
  = ListConfigurationsRequest { "ConfigurationType'" :: ConfigurationItemType, "Filters'" :: NullOrUndefined (Filters), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (NextToken), "OrderBy'" :: NullOrUndefined (OrderByList) }
```

#### `ListConfigurationsResponse`

``` purescript
newtype ListConfigurationsResponse
  = ListConfigurationsResponse { "Configurations'" :: NullOrUndefined (Configurations), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ListServerNeighborsRequest`

``` purescript
newtype ListServerNeighborsRequest
  = ListServerNeighborsRequest { "ConfigurationId'" :: ConfigurationId, "PortInformationNeeded'" :: NullOrUndefined (Boolean), "NeighborConfigurationIds'" :: NullOrUndefined (ConfigurationIdList), "MaxResults'" :: NullOrUndefined (Int), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListServerNeighborsResponse`

``` purescript
newtype ListServerNeighborsResponse
  = ListServerNeighborsResponse { "Neighbors'" :: NeighborDetailsList, "NextToken'" :: NullOrUndefined (String), "KnownDependencyCount'" :: NullOrUndefined (Number) }
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `NeighborConnectionDetail`

``` purescript
newtype NeighborConnectionDetail
  = NeighborConnectionDetail { "SourceServerId'" :: ConfigurationId, "DestinationServerId'" :: ConfigurationId, "DestinationPort'" :: NullOrUndefined (BoxedInteger), "TransportProtocol'" :: NullOrUndefined (String), "ConnectionsCount'" :: Number }
```

<p>Details about neighboring servers.</p>

#### `NeighborDetailsList`

``` purescript
newtype NeighborDetailsList
  = NeighborDetailsList (Array NeighborConnectionDetail)
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `OperationNotPermittedException`

``` purescript
newtype OperationNotPermittedException
  = OperationNotPermittedException { "Message'" :: NullOrUndefined (Message) }
```

<p>This operation is not permitted.</p>

#### `OrderByElement`

``` purescript
newtype OrderByElement
  = OrderByElement { "FieldName'" :: String, "SortOrder'" :: NullOrUndefined (OrderString') }
```

<p>A field and direction for ordered output.</p>

#### `OrderByList`

``` purescript
newtype OrderByList
  = OrderByList (Array OrderByElement)
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (Message) }
```

<p>The specified configuration ID was not located. Verify the configuration ID and try again.</p>

#### `ServerInternalErrorException`

``` purescript
newtype ServerInternalErrorException
  = ServerInternalErrorException { "Message'" :: NullOrUndefined (Message) }
```

<p>The server experienced an internal error. Try again.</p>

#### `StartDataCollectionByAgentIdsRequest`

``` purescript
newtype StartDataCollectionByAgentIdsRequest
  = StartDataCollectionByAgentIdsRequest { "AgentIds'" :: AgentIds }
```

#### `StartDataCollectionByAgentIdsResponse`

``` purescript
newtype StartDataCollectionByAgentIdsResponse
  = StartDataCollectionByAgentIdsResponse { "AgentsConfigurationStatus'" :: NullOrUndefined (AgentConfigurationStatusList) }
```

#### `StartExportTaskRequest`

``` purescript
newtype StartExportTaskRequest
  = StartExportTaskRequest { "ExportDataFormat'" :: NullOrUndefined (ExportDataFormats), "Filters'" :: NullOrUndefined (ExportFilters), "StartTime'" :: NullOrUndefined (TimeStamp), "EndTime'" :: NullOrUndefined (TimeStamp) }
```

#### `StartExportTaskResponse`

``` purescript
newtype StartExportTaskResponse
  = StartExportTaskResponse { "ExportId'" :: NullOrUndefined (ConfigurationsExportId) }
```

#### `StopDataCollectionByAgentIdsRequest`

``` purescript
newtype StopDataCollectionByAgentIdsRequest
  = StopDataCollectionByAgentIdsRequest { "AgentIds'" :: AgentIds }
```

#### `StopDataCollectionByAgentIdsResponse`

``` purescript
newtype StopDataCollectionByAgentIdsResponse
  = StopDataCollectionByAgentIdsResponse { "AgentsConfigurationStatus'" :: NullOrUndefined (AgentConfigurationStatusList) }
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key'" :: TagKey, "Value'" :: TagValue }
```

<p>Metadata that help you categorize IT assets.</p>

#### `TagFilter`

``` purescript
newtype TagFilter
  = TagFilter { "Name'" :: FilterName, "Values'" :: FilterValues }
```

<p>The tag filter. Valid names are: <code>tagKey</code>, <code>tagValue</code>, <code>configurationId</code>.</p>

#### `TagFilters`

``` purescript
newtype TagFilters
  = TagFilters (Array TagFilter)
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagSet`

``` purescript
newtype TagSet
  = TagSet (Array Tag)
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `TimeStamp`

``` purescript
newtype TimeStamp
  = TimeStamp Number
```

#### `UpdateApplicationRequest`

``` purescript
newtype UpdateApplicationRequest
  = UpdateApplicationRequest { "ConfigurationId'" :: ApplicationId, "Name'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String) }
```

#### `UpdateApplicationResponse`

``` purescript
newtype UpdateApplicationResponse
  = UpdateApplicationResponse {  }
```

#### `OrderString'`

``` purescript
newtype OrderString'
  = OrderString' String
```


