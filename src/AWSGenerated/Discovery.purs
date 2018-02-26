

-- | <fullname>AWS Application Discovery Service</fullname> <p>AWS Application Discovery Service helps you plan application migration projects by automatically identifying servers, virtual machines (VMs), software, and software dependencies running in your on-premises data centers. Application Discovery Service also collects application performance data, which can help you assess the outcome of your migration. The data collected by Application Discovery Service is securely retained in an Amazon-hosted and managed database in the cloud. You can export the data as a CSV or XML file into your preferred visualization tool or cloud-migration solution to plan your migration. For more information, see the Application Discovery Service <a href="http://aws.amazon.com/application-discovery/faqs/">FAQ</a>.</p> <p>Application Discovery Service offers two modes of operation.</p> <ul> <li> <p> <b>Agentless discovery</b> mode is recommended for environments that use VMware vCenter Server. This mode doesn't require you to install an agent on each host. Agentless discovery gathers server information regardless of the operating systems, which minimizes the time required for initial on-premises infrastructure assessment. Agentless discovery doesn't collect information about software and software dependencies. It also doesn't work in non-VMware environments. We recommend that you use agent-based discovery for non-VMware environments and if you want to collect information about software and software dependencies. You can also run agent-based and agentless discovery simultaneously. Use agentless discovery to quickly complete the initial infrastructure assessment and then install agents on select hosts to gather information about software and software dependencies.</p> </li> <li> <p> <b>Agent-based discovery</b> mode collects a richer set of data than agentless discovery by using Amazon software, the AWS Application Discovery Agent, which you install on one or more hosts in your data center. The agent captures infrastructure and application information, including an inventory of installed software applications, system and process performance, resource utilization, and network dependencies between workloads. The information collected by agents is secured at rest and in transit to the Application Discovery Service database in the cloud. </p> </li> </ul> <p>Application Discovery Service integrates with application discovery solutions from AWS Partner Network (APN) partners. Third-party application discovery tools can query Application Discovery Service and write to the Application Discovery Service database using a public API. You can then import the data into either a visualization tool or cloud-migration solution.</p> <important> <p>Application Discovery Service doesn't gather sensitive information. All data is handled according to the <a href="http://aws.amazon.com/privacy/">AWS Privacy Policy</a>. You can operate Application Discovery Service using offline mode to inspect collected data before it is shared with the service.</p> </important> <p>Your AWS account must be granted access to Application Discovery Service, a process called <i>whitelisting</i>. This is true for AWS partners and customers alike. To request access, <a href="http://aws.amazon.com/application-discovery/how-to-start/"> sign up for AWS Application Discovery Service</a>.</p> <p>This API reference provides descriptions, syntax, and usage examples for each of the actions and data types for Application Discovery Service. The topic for each action shows the API request parameters and the response. Alternatively, you can use one of the AWS SDKs to access an API that is tailored to the programming language or platform that you're using. For more information, see <a href="http://aws.amazon.com/tools/#SDKs">AWS SDKs</a>.</p> <p>This guide is intended for use with the <a href="http://docs.aws.amazon.com/application-discovery/latest/userguide/"> <i>AWS Application Discovery Service User Guide</i> </a>.</p>
module AWS.Discovery where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Discovery" :: String


-- | <p>Associates one or more configuration items with an application.</p>
associateConfigurationItemsToApplication :: forall eff. AssociateConfigurationItemsToApplicationRequest -> Aff (err :: AWS.RequestError | eff) AssociateConfigurationItemsToApplicationResponse
associateConfigurationItemsToApplication = AWS.request serviceName "AssociateConfigurationItemsToApplication" 


-- | <p>Creates an application with the given name and description.</p>
createApplication :: forall eff. CreateApplicationRequest -> Aff (err :: AWS.RequestError | eff) CreateApplicationResponse
createApplication = AWS.request serviceName "CreateApplication" 


-- | <p>Creates one or more tags for configuration items. Tags are metadata that help you categorize IT assets. This API accepts a list of multiple configuration items.</p>
createTags :: forall eff. CreateTagsRequest -> Aff (err :: AWS.RequestError | eff) CreateTagsResponse
createTags = AWS.request serviceName "CreateTags" 


-- | <p>Deletes a list of applications and their associations with configuration items.</p>
deleteApplications :: forall eff. DeleteApplicationsRequest -> Aff (err :: AWS.RequestError | eff) DeleteApplicationsResponse
deleteApplications = AWS.request serviceName "DeleteApplications" 


-- | <p>Deletes the association between configuration items and one or more tags. This API accepts a list of multiple configuration items.</p>
deleteTags :: forall eff. DeleteTagsRequest -> Aff (err :: AWS.RequestError | eff) DeleteTagsResponse
deleteTags = AWS.request serviceName "DeleteTags" 


-- | <p>Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an ID.</p>
describeAgents :: forall eff. DescribeAgentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeAgentsResponse
describeAgents = AWS.request serviceName "DescribeAgents" 


-- | <p>Retrieves attributes for a list of configuration item IDs. All of the supplied IDs must be for the same asset type (server, application, process, or connection). Output fields are specific to the asset type selected. For example, the output for a <i>server</i> configuration item includes a list of attributes about the server, such as host name, operating system, and number of network cards.</p> <p>For a complete list of outputs for each asset type, see <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html#DescribeConfigurations">Using the DescribeConfigurations Action</a>.</p>
describeConfigurations :: forall eff. DescribeConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationsResponse
describeConfigurations = AWS.request serviceName "DescribeConfigurations" 


-- | <p>Deprecated. Use <code>DescribeExportTasks</code> instead.</p> <p>Retrieves the status of a given export process. You can retrieve status from a maximum of 100 processes.</p>
describeExportConfigurations :: forall eff. DescribeExportConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeExportConfigurationsResponse
describeExportConfigurations = AWS.request serviceName "DescribeExportConfigurations" 


-- | <p>Retrieve status of one or more export tasks. You can retrieve the status of up to 100 export tasks.</p>
describeExportTasks :: forall eff. DescribeExportTasksRequest -> Aff (err :: AWS.RequestError | eff) DescribeExportTasksResponse
describeExportTasks = AWS.request serviceName "DescribeExportTasks" 


-- | <p>Retrieves a list of configuration items that are tagged with a specific tag. Or retrieves a list of all tags assigned to a specific configuration item.</p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: AWS.RequestError | eff) DescribeTagsResponse
describeTags = AWS.request serviceName "DescribeTags" 


-- | <p>Disassociates one or more configuration items from an application.</p>
disassociateConfigurationItemsFromApplication :: forall eff. DisassociateConfigurationItemsFromApplicationRequest -> Aff (err :: AWS.RequestError | eff) DisassociateConfigurationItemsFromApplicationResponse
disassociateConfigurationItemsFromApplication = AWS.request serviceName "DisassociateConfigurationItemsFromApplication" 


-- | <p>Deprecated. Use <code>StartExportTask</code> instead.</p> <p>Exports all discovered configuration data to an Amazon S3 bucket or an application that enables you to view and evaluate the data. Data includes tags and tag associations, processes, connections, servers, and system performance. This API returns an export ID that you can query using the <i>DescribeExportConfigurations</i> API. The system imposes a limit of two configuration exports in six hours.</p>
exportConfigurations :: forall eff.  Aff (err :: AWS.RequestError | eff) ExportConfigurationsResponse
exportConfigurations = AWS.request serviceName "ExportConfigurations" unit


-- | <p>Retrieves a short summary of discovered assets.</p>
getDiscoverySummary :: forall eff. GetDiscoverySummaryRequest -> Aff (err :: AWS.RequestError | eff) GetDiscoverySummaryResponse
getDiscoverySummary = AWS.request serviceName "GetDiscoverySummary" 


-- | <p>Retrieves a list of configuration items according to criteria that you specify in a filter. The filter criteria identifies the relationship requirements.</p>
listConfigurations :: forall eff. ListConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) ListConfigurationsResponse
listConfigurations = AWS.request serviceName "ListConfigurations" 


-- | <p>Retrieves a list of servers that are one network hop away from a specified server.</p>
listServerNeighbors :: forall eff. ListServerNeighborsRequest -> Aff (err :: AWS.RequestError | eff) ListServerNeighborsResponse
listServerNeighbors = AWS.request serviceName "ListServerNeighbors" 


-- | <p>Instructs the specified agents or connectors to start collecting data.</p>
startDataCollectionByAgentIds :: forall eff. StartDataCollectionByAgentIdsRequest -> Aff (err :: AWS.RequestError | eff) StartDataCollectionByAgentIdsResponse
startDataCollectionByAgentIds = AWS.request serviceName "StartDataCollectionByAgentIds" 


-- | <p> Begins the export of discovered data to an S3 bucket.</p> <p> If you specify <code>agentIds</code> in a filter, the task exports up to 72 hours of detailed data collected by the identified Application Discovery Agent, including network, process, and performance details. A time range for exported agent data may be set by using <code>startTime</code> and <code>endTime</code>. Export of detailed agent data is limited to five concurrently running exports. </p> <p> If you do not include an <code>agentIds</code> filter, summary data is exported that includes both AWS Agentless Discovery Connector data and summary data from AWS Discovery Agents. Export of summary data is limited to two exports per day. </p>
startExportTask :: forall eff. StartExportTaskRequest -> Aff (err :: AWS.RequestError | eff) StartExportTaskResponse
startExportTask = AWS.request serviceName "StartExportTask" 


-- | <p>Instructs the specified agents or connectors to stop collecting data.</p>
stopDataCollectionByAgentIds :: forall eff. StopDataCollectionByAgentIdsRequest -> Aff (err :: AWS.RequestError | eff) StopDataCollectionByAgentIdsResponse
stopDataCollectionByAgentIds = AWS.request serviceName "StopDataCollectionByAgentIds" 


-- | <p>Updates metadata about an application.</p>
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (err :: AWS.RequestError | eff) UpdateApplicationResponse
updateApplication = AWS.request serviceName "UpdateApplication" 


-- | <p>Information about agents or connectors that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation, and whether the agent/connector configuration was updated.</p>
newtype AgentConfigurationStatus = AgentConfigurationStatus 
  { "AgentId'" :: NullOrUndefined (String)
  , "OperationSucceeded'" :: NullOrUndefined (Boolean)
  , "Description'" :: NullOrUndefined (String)
  }


newtype AgentConfigurationStatusList = AgentConfigurationStatusList (Array AgentConfigurationStatus)


newtype AgentId = AgentId String


newtype AgentIds = AgentIds (Array AgentId)


-- | <p>Information about agents or connectors associated with the user’s AWS account. Information includes agent or connector IDs, IP addresses, media access control (MAC) addresses, agent or connector health, hostname where the agent or connector resides, and agent version for each agent.</p>
newtype AgentInfo = AgentInfo 
  { "AgentId'" :: NullOrUndefined (AgentId)
  , "HostName'" :: NullOrUndefined (String)
  , "AgentNetworkInfoList'" :: NullOrUndefined (AgentNetworkInfoList)
  , "ConnectorId'" :: NullOrUndefined (String)
  , "Version'" :: NullOrUndefined (String)
  , "Health'" :: NullOrUndefined (AgentStatus)
  , "LastHealthPingTime'" :: NullOrUndefined (String)
  , "CollectionStatus'" :: NullOrUndefined (String)
  , "AgentType'" :: NullOrUndefined (String)
  , "RegisteredTime'" :: NullOrUndefined (String)
  }


-- | <p>Network details about the host where the agent/connector resides.</p>
newtype AgentNetworkInfo = AgentNetworkInfo 
  { "IpAddress'" :: NullOrUndefined (String)
  , "MacAddress'" :: NullOrUndefined (String)
  }


newtype AgentNetworkInfoList = AgentNetworkInfoList (Array AgentNetworkInfo)


newtype AgentStatus = AgentStatus String


newtype AgentsInfo = AgentsInfo (Array AgentInfo)


newtype ApplicationId = ApplicationId String


newtype ApplicationIdsList = ApplicationIdsList (Array ApplicationId)


newtype AssociateConfigurationItemsToApplicationRequest = AssociateConfigurationItemsToApplicationRequest 
  { "ApplicationConfigurationId'" :: (ApplicationId)
  , "ConfigurationIds'" :: (ConfigurationIdList)
  }


newtype AssociateConfigurationItemsToApplicationResponse = AssociateConfigurationItemsToApplicationResponse 
  { 
  }


-- | <p>The AWS user account does not have permission to perform the action. Check the IAM policy associated with this account.</p>
newtype AuthorizationErrorException = AuthorizationErrorException 
  { "Message'" :: NullOrUndefined (Message)
  }


newtype BoxedInteger = BoxedInteger Int


newtype Condition = Condition String


newtype Configuration = Configuration (Map String String)


newtype ConfigurationId = ConfigurationId String


newtype ConfigurationIdList = ConfigurationIdList (Array ConfigurationId)


newtype ConfigurationItemType = ConfigurationItemType String


-- | <p>Tags for a configuration item. Tags are metadata that help you categorize IT assets.</p>
newtype ConfigurationTag = ConfigurationTag 
  { "ConfigurationType'" :: NullOrUndefined (ConfigurationItemType)
  , "ConfigurationId'" :: NullOrUndefined (ConfigurationId)
  , "Key'" :: NullOrUndefined (TagKey)
  , "Value'" :: NullOrUndefined (TagValue)
  , "TimeOfCreation'" :: NullOrUndefined (TimeStamp)
  }


newtype ConfigurationTagSet = ConfigurationTagSet (Array ConfigurationTag)


newtype Configurations = Configurations (Array Configuration)


newtype ConfigurationsDownloadUrl = ConfigurationsDownloadUrl String


newtype ConfigurationsExportId = ConfigurationsExportId String


newtype CreateApplicationRequest = CreateApplicationRequest 
  { "Name'" :: (String)
  , "Description'" :: NullOrUndefined (String)
  }


newtype CreateApplicationResponse = CreateApplicationResponse 
  { "ConfigurationId'" :: NullOrUndefined (String)
  }


newtype CreateTagsRequest = CreateTagsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  , "Tags'" :: (TagSet)
  }


newtype CreateTagsResponse = CreateTagsResponse 
  { 
  }


-- | <p>Inventory data for installed discovery agents.</p>
newtype CustomerAgentInfo = CustomerAgentInfo 
  { "ActiveAgents'" :: (Int)
  , "HealthyAgents'" :: (Int)
  , "BlackListedAgents'" :: (Int)
  , "ShutdownAgents'" :: (Int)
  , "UnhealthyAgents'" :: (Int)
  , "TotalAgents'" :: (Int)
  , "UnknownAgents'" :: (Int)
  }


-- | <p>Inventory data for installed discovery connectors.</p>
newtype CustomerConnectorInfo = CustomerConnectorInfo 
  { "ActiveConnectors'" :: (Int)
  , "HealthyConnectors'" :: (Int)
  , "BlackListedConnectors'" :: (Int)
  , "ShutdownConnectors'" :: (Int)
  , "UnhealthyConnectors'" :: (Int)
  , "TotalConnectors'" :: (Int)
  , "UnknownConnectors'" :: (Int)
  }


newtype DeleteApplicationsRequest = DeleteApplicationsRequest 
  { "ConfigurationIds'" :: (ApplicationIdsList)
  }


newtype DeleteApplicationsResponse = DeleteApplicationsResponse 
  { 
  }


newtype DeleteTagsRequest = DeleteTagsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  , "Tags'" :: NullOrUndefined (TagSet)
  }


newtype DeleteTagsResponse = DeleteTagsResponse 
  { 
  }


newtype DescribeAgentsRequest = DescribeAgentsRequest 
  { "AgentIds'" :: NullOrUndefined (AgentIds)
  , "Filters'" :: NullOrUndefined (Filters)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeAgentsResponse = DescribeAgentsResponse 
  { "AgentsInfo'" :: NullOrUndefined (AgentsInfo)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeConfigurationsAttribute = DescribeConfigurationsAttribute (Map String String)


newtype DescribeConfigurationsAttributes = DescribeConfigurationsAttributes (Array DescribeConfigurationsAttribute)


newtype DescribeConfigurationsRequest = DescribeConfigurationsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  }


newtype DescribeConfigurationsResponse = DescribeConfigurationsResponse 
  { "Configurations'" :: NullOrUndefined (DescribeConfigurationsAttributes)
  }


newtype DescribeExportConfigurationsRequest = DescribeExportConfigurationsRequest 
  { "ExportIds'" :: NullOrUndefined (ExportIds)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeExportConfigurationsResponse = DescribeExportConfigurationsResponse 
  { "ExportsInfo'" :: NullOrUndefined (ExportsInfo)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeExportTasksRequest = DescribeExportTasksRequest 
  { "ExportIds'" :: NullOrUndefined (ExportIds)
  , "Filters'" :: NullOrUndefined (ExportFilters)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeExportTasksResponse = DescribeExportTasksResponse 
  { "ExportsInfo'" :: NullOrUndefined (ExportsInfo)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeTagsRequest = DescribeTagsRequest 
  { "Filters'" :: NullOrUndefined (TagFilters)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DescribeTagsResponse = DescribeTagsResponse 
  { "Tags'" :: NullOrUndefined (ConfigurationTagSet)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype DisassociateConfigurationItemsFromApplicationRequest = DisassociateConfigurationItemsFromApplicationRequest 
  { "ApplicationConfigurationId'" :: (ApplicationId)
  , "ConfigurationIds'" :: (ConfigurationIdList)
  }


newtype DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse 
  { 
  }


newtype ExportConfigurationsResponse = ExportConfigurationsResponse 
  { "ExportId'" :: NullOrUndefined (ConfigurationsExportId)
  }


newtype ExportDataFormat = ExportDataFormat String


newtype ExportDataFormats = ExportDataFormats (Array ExportDataFormat)


-- | <p>Used to select which agent's data is to be exported. A single agent ID may be selected for export using the <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html">StartExportTask</a> action.</p>
newtype ExportFilter = ExportFilter 
  { "Name'" :: (FilterName)
  , "Values'" :: (FilterValues)
  , "Condition'" :: (Condition)
  }


newtype ExportFilters = ExportFilters (Array ExportFilter)


newtype ExportIds = ExportIds (Array ConfigurationsExportId)


-- | <p>Information regarding the export status of discovered data. The value is an array of objects.</p>
newtype ExportInfo = ExportInfo 
  { "ExportId'" :: (ConfigurationsExportId)
  , "ExportStatus'" :: (ExportStatus)
  , "StatusMessage'" :: (ExportStatusMessage)
  , "ConfigurationsDownloadUrl'" :: NullOrUndefined (ConfigurationsDownloadUrl)
  , "ExportRequestTime'" :: (ExportRequestTime)
  , "IsTruncated'" :: NullOrUndefined (Boolean)
  , "RequestedStartTime'" :: NullOrUndefined (TimeStamp)
  , "RequestedEndTime'" :: NullOrUndefined (TimeStamp)
  }


newtype ExportRequestTime = ExportRequestTime Number


newtype ExportStatus = ExportStatus String


newtype ExportStatusMessage = ExportStatusMessage String


newtype ExportsInfo = ExportsInfo (Array ExportInfo)


-- | <p>A filter that can use conditional operators.</p> <p>For more information about filters, see <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html">Querying Discovered Configuration Items</a>. </p>
newtype Filter = Filter 
  { "Name'" :: (String)
  , "Values'" :: (FilterValues)
  , "Condition'" :: (Condition)
  }


newtype FilterName = FilterName String


newtype FilterValue = FilterValue String


newtype FilterValues = FilterValues (Array FilterValue)


newtype Filters = Filters (Array Filter)


newtype GetDiscoverySummaryRequest = GetDiscoverySummaryRequest 
  { 
  }


newtype GetDiscoverySummaryResponse = GetDiscoverySummaryResponse 
  { "Servers'" :: NullOrUndefined (Number)
  , "Applications'" :: NullOrUndefined (Number)
  , "ServersMappedToApplications'" :: NullOrUndefined (Number)
  , "ServersMappedtoTags'" :: NullOrUndefined (Number)
  , "AgentSummary'" :: NullOrUndefined (CustomerAgentInfo)
  , "ConnectorSummary'" :: NullOrUndefined (CustomerConnectorInfo)
  }


-- | <p>One or more parameters are not valid. Verify the parameters and try again.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (Message)
  }


-- | <p>The value of one or more parameters are either invalid or out of range. Verify the parameter values and try again.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined (Message)
  }


newtype ListConfigurationsRequest = ListConfigurationsRequest 
  { "ConfigurationType'" :: (ConfigurationItemType)
  , "Filters'" :: NullOrUndefined (Filters)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "OrderBy'" :: NullOrUndefined (OrderByList)
  }


newtype ListConfigurationsResponse = ListConfigurationsResponse 
  { "Configurations'" :: NullOrUndefined (Configurations)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ListServerNeighborsRequest = ListServerNeighborsRequest 
  { "ConfigurationId'" :: (ConfigurationId)
  , "PortInformationNeeded'" :: NullOrUndefined (Boolean)
  , "NeighborConfigurationIds'" :: NullOrUndefined (ConfigurationIdList)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype ListServerNeighborsResponse = ListServerNeighborsResponse 
  { "Neighbors'" :: (NeighborDetailsList)
  , "NextToken'" :: NullOrUndefined (String)
  , "KnownDependencyCount'" :: NullOrUndefined (Number)
  }


newtype Message = Message String


-- | <p>Details about neighboring servers.</p>
newtype NeighborConnectionDetail = NeighborConnectionDetail 
  { "SourceServerId'" :: (ConfigurationId)
  , "DestinationServerId'" :: (ConfigurationId)
  , "DestinationPort'" :: NullOrUndefined (BoxedInteger)
  , "TransportProtocol'" :: NullOrUndefined (String)
  , "ConnectionsCount'" :: (Number)
  }


newtype NeighborDetailsList = NeighborDetailsList (Array NeighborConnectionDetail)


newtype NextToken = NextToken String


-- | <p>This operation is not permitted.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { "Message'" :: NullOrUndefined (Message)
  }


-- | <p>A field and direction for ordered output.</p>
newtype OrderByElement = OrderByElement 
  { "FieldName'" :: (String)
  , "SortOrder'" :: NullOrUndefined (OrderString')
  }


newtype OrderByList = OrderByList (Array OrderByElement)


-- | <p>The specified configuration ID was not located. Verify the configuration ID and try again.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (Message)
  }


-- | <p>The server experienced an internal error. Try again.</p>
newtype ServerInternalErrorException = ServerInternalErrorException 
  { "Message'" :: NullOrUndefined (Message)
  }


newtype StartDataCollectionByAgentIdsRequest = StartDataCollectionByAgentIdsRequest 
  { "AgentIds'" :: (AgentIds)
  }


newtype StartDataCollectionByAgentIdsResponse = StartDataCollectionByAgentIdsResponse 
  { "AgentsConfigurationStatus'" :: NullOrUndefined (AgentConfigurationStatusList)
  }


newtype StartExportTaskRequest = StartExportTaskRequest 
  { "ExportDataFormat'" :: NullOrUndefined (ExportDataFormats)
  , "Filters'" :: NullOrUndefined (ExportFilters)
  , "StartTime'" :: NullOrUndefined (TimeStamp)
  , "EndTime'" :: NullOrUndefined (TimeStamp)
  }


newtype StartExportTaskResponse = StartExportTaskResponse 
  { "ExportId'" :: NullOrUndefined (ConfigurationsExportId)
  }


newtype StopDataCollectionByAgentIdsRequest = StopDataCollectionByAgentIdsRequest 
  { "AgentIds'" :: (AgentIds)
  }


newtype StopDataCollectionByAgentIdsResponse = StopDataCollectionByAgentIdsResponse 
  { "AgentsConfigurationStatus'" :: NullOrUndefined (AgentConfigurationStatusList)
  }


-- | <p>Metadata that help you categorize IT assets.</p>
newtype Tag = Tag 
  { "Key'" :: (TagKey)
  , "Value'" :: (TagValue)
  }


-- | <p>The tag filter. Valid names are: <code>tagKey</code>, <code>tagValue</code>, <code>configurationId</code>.</p>
newtype TagFilter = TagFilter 
  { "Name'" :: (FilterName)
  , "Values'" :: (FilterValues)
  }


newtype TagFilters = TagFilters (Array TagFilter)


newtype TagKey = TagKey String


newtype TagSet = TagSet (Array Tag)


newtype TagValue = TagValue String


newtype TimeStamp = TimeStamp Number


newtype UpdateApplicationRequest = UpdateApplicationRequest 
  { "ConfigurationId'" :: (ApplicationId)
  , "Name'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (String)
  }


newtype UpdateApplicationResponse = UpdateApplicationResponse 
  { 
  }


newtype OrderString' = OrderString' String
