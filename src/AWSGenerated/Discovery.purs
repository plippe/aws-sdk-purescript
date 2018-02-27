

-- | <fullname>AWS Application Discovery Service</fullname> <p>AWS Application Discovery Service helps you plan application migration projects by automatically identifying servers, virtual machines (VMs), software, and software dependencies running in your on-premises data centers. Application Discovery Service also collects application performance data, which can help you assess the outcome of your migration. The data collected by Application Discovery Service is securely retained in an Amazon-hosted and managed database in the cloud. You can export the data as a CSV or XML file into your preferred visualization tool or cloud-migration solution to plan your migration. For more information, see the Application Discovery Service <a href="http://aws.amazon.com/application-discovery/faqs/">FAQ</a>.</p> <p>Application Discovery Service offers two modes of operation.</p> <ul> <li> <p> <b>Agentless discovery</b> mode is recommended for environments that use VMware vCenter Server. This mode doesn't require you to install an agent on each host. Agentless discovery gathers server information regardless of the operating systems, which minimizes the time required for initial on-premises infrastructure assessment. Agentless discovery doesn't collect information about software and software dependencies. It also doesn't work in non-VMware environments. We recommend that you use agent-based discovery for non-VMware environments and if you want to collect information about software and software dependencies. You can also run agent-based and agentless discovery simultaneously. Use agentless discovery to quickly complete the initial infrastructure assessment and then install agents on select hosts to gather information about software and software dependencies.</p> </li> <li> <p> <b>Agent-based discovery</b> mode collects a richer set of data than agentless discovery by using Amazon software, the AWS Application Discovery Agent, which you install on one or more hosts in your data center. The agent captures infrastructure and application information, including an inventory of installed software applications, system and process performance, resource utilization, and network dependencies between workloads. The information collected by agents is secured at rest and in transit to the Application Discovery Service database in the cloud. </p> </li> </ul> <p>Application Discovery Service integrates with application discovery solutions from AWS Partner Network (APN) partners. Third-party application discovery tools can query Application Discovery Service and write to the Application Discovery Service database using a public API. You can then import the data into either a visualization tool or cloud-migration solution.</p> <important> <p>Application Discovery Service doesn't gather sensitive information. All data is handled according to the <a href="http://aws.amazon.com/privacy/">AWS Privacy Policy</a>. You can operate Application Discovery Service using offline mode to inspect collected data before it is shared with the service.</p> </important> <p>Your AWS account must be granted access to Application Discovery Service, a process called <i>whitelisting</i>. This is true for AWS partners and customers alike. To request access, <a href="http://aws.amazon.com/application-discovery/how-to-start/"> sign up for AWS Application Discovery Service</a>.</p> <p>This API reference provides descriptions, syntax, and usage examples for each of the actions and data types for Application Discovery Service. The topic for each action shows the API request parameters and the response. Alternatively, you can use one of the AWS SDKs to access an API that is tailored to the programming language or platform that you're using. For more information, see <a href="http://aws.amazon.com/tools/#SDKs">AWS SDKs</a>.</p> <p>This guide is intended for use with the <a href="http://docs.aws.amazon.com/application-discovery/latest/userguide/"> <i>AWS Application Discovery Service User Guide</i> </a>.</p>
module AWS.Discovery where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Discovery" :: String


-- | <p>Associates one or more configuration items with an application.</p>
associateConfigurationItemsToApplication :: forall eff. AssociateConfigurationItemsToApplicationRequest -> Aff (err :: AWS.RequestError | eff) AssociateConfigurationItemsToApplicationResponse
associateConfigurationItemsToApplication = AWS.request serviceName "associateConfigurationItemsToApplication" 


-- | <p>Creates an application with the given name and description.</p>
createApplication :: forall eff. CreateApplicationRequest -> Aff (err :: AWS.RequestError | eff) CreateApplicationResponse
createApplication = AWS.request serviceName "createApplication" 


-- | <p>Creates one or more tags for configuration items. Tags are metadata that help you categorize IT assets. This API accepts a list of multiple configuration items.</p>
createTags :: forall eff. CreateTagsRequest -> Aff (err :: AWS.RequestError | eff) CreateTagsResponse
createTags = AWS.request serviceName "createTags" 


-- | <p>Deletes a list of applications and their associations with configuration items.</p>
deleteApplications :: forall eff. DeleteApplicationsRequest -> Aff (err :: AWS.RequestError | eff) DeleteApplicationsResponse
deleteApplications = AWS.request serviceName "deleteApplications" 


-- | <p>Deletes the association between configuration items and one or more tags. This API accepts a list of multiple configuration items.</p>
deleteTags :: forall eff. DeleteTagsRequest -> Aff (err :: AWS.RequestError | eff) DeleteTagsResponse
deleteTags = AWS.request serviceName "deleteTags" 


-- | <p>Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an ID.</p>
describeAgents :: forall eff. DescribeAgentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeAgentsResponse
describeAgents = AWS.request serviceName "describeAgents" 


-- | <p>Retrieves attributes for a list of configuration item IDs. All of the supplied IDs must be for the same asset type (server, application, process, or connection). Output fields are specific to the asset type selected. For example, the output for a <i>server</i> configuration item includes a list of attributes about the server, such as host name, operating system, and number of network cards.</p> <p>For a complete list of outputs for each asset type, see <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html#DescribeConfigurations">Using the DescribeConfigurations Action</a>.</p>
describeConfigurations :: forall eff. DescribeConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeConfigurationsResponse
describeConfigurations = AWS.request serviceName "describeConfigurations" 


-- | <p>Deprecated. Use <code>DescribeExportTasks</code> instead.</p> <p>Retrieves the status of a given export process. You can retrieve status from a maximum of 100 processes.</p>
describeExportConfigurations :: forall eff. DescribeExportConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) DescribeExportConfigurationsResponse
describeExportConfigurations = AWS.request serviceName "describeExportConfigurations" 


-- | <p>Retrieve status of one or more export tasks. You can retrieve the status of up to 100 export tasks.</p>
describeExportTasks :: forall eff. DescribeExportTasksRequest -> Aff (err :: AWS.RequestError | eff) DescribeExportTasksResponse
describeExportTasks = AWS.request serviceName "describeExportTasks" 


-- | <p>Retrieves a list of configuration items that are tagged with a specific tag. Or retrieves a list of all tags assigned to a specific configuration item.</p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: AWS.RequestError | eff) DescribeTagsResponse
describeTags = AWS.request serviceName "describeTags" 


-- | <p>Disassociates one or more configuration items from an application.</p>
disassociateConfigurationItemsFromApplication :: forall eff. DisassociateConfigurationItemsFromApplicationRequest -> Aff (err :: AWS.RequestError | eff) DisassociateConfigurationItemsFromApplicationResponse
disassociateConfigurationItemsFromApplication = AWS.request serviceName "disassociateConfigurationItemsFromApplication" 


-- | <p>Deprecated. Use <code>StartExportTask</code> instead.</p> <p>Exports all discovered configuration data to an Amazon S3 bucket or an application that enables you to view and evaluate the data. Data includes tags and tag associations, processes, connections, servers, and system performance. This API returns an export ID that you can query using the <i>DescribeExportConfigurations</i> API. The system imposes a limit of two configuration exports in six hours.</p>
exportConfigurations :: forall eff.  Aff (err :: AWS.RequestError | eff) ExportConfigurationsResponse
exportConfigurations = AWS.request serviceName "exportConfigurations" unit


-- | <p>Retrieves a short summary of discovered assets.</p>
getDiscoverySummary :: forall eff. GetDiscoverySummaryRequest -> Aff (err :: AWS.RequestError | eff) GetDiscoverySummaryResponse
getDiscoverySummary = AWS.request serviceName "getDiscoverySummary" 


-- | <p>Retrieves a list of configuration items according to criteria that you specify in a filter. The filter criteria identifies the relationship requirements.</p>
listConfigurations :: forall eff. ListConfigurationsRequest -> Aff (err :: AWS.RequestError | eff) ListConfigurationsResponse
listConfigurations = AWS.request serviceName "listConfigurations" 


-- | <p>Retrieves a list of servers that are one network hop away from a specified server.</p>
listServerNeighbors :: forall eff. ListServerNeighborsRequest -> Aff (err :: AWS.RequestError | eff) ListServerNeighborsResponse
listServerNeighbors = AWS.request serviceName "listServerNeighbors" 


-- | <p>Instructs the specified agents or connectors to start collecting data.</p>
startDataCollectionByAgentIds :: forall eff. StartDataCollectionByAgentIdsRequest -> Aff (err :: AWS.RequestError | eff) StartDataCollectionByAgentIdsResponse
startDataCollectionByAgentIds = AWS.request serviceName "startDataCollectionByAgentIds" 


-- | <p> Begins the export of discovered data to an S3 bucket.</p> <p> If you specify <code>agentIds</code> in a filter, the task exports up to 72 hours of detailed data collected by the identified Application Discovery Agent, including network, process, and performance details. A time range for exported agent data may be set by using <code>startTime</code> and <code>endTime</code>. Export of detailed agent data is limited to five concurrently running exports. </p> <p> If you do not include an <code>agentIds</code> filter, summary data is exported that includes both AWS Agentless Discovery Connector data and summary data from AWS Discovery Agents. Export of summary data is limited to two exports per day. </p>
startExportTask :: forall eff. StartExportTaskRequest -> Aff (err :: AWS.RequestError | eff) StartExportTaskResponse
startExportTask = AWS.request serviceName "startExportTask" 


-- | <p>Instructs the specified agents or connectors to stop collecting data.</p>
stopDataCollectionByAgentIds :: forall eff. StopDataCollectionByAgentIdsRequest -> Aff (err :: AWS.RequestError | eff) StopDataCollectionByAgentIdsResponse
stopDataCollectionByAgentIds = AWS.request serviceName "stopDataCollectionByAgentIds" 


-- | <p>Updates metadata about an application.</p>
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (err :: AWS.RequestError | eff) UpdateApplicationResponse
updateApplication = AWS.request serviceName "updateApplication" 


-- | <p>Information about agents or connectors that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation, and whether the agent/connector configuration was updated.</p>
newtype AgentConfigurationStatus = AgentConfigurationStatus 
  { "AgentId'" :: NullOrUndefined (String)
  , "OperationSucceeded'" :: NullOrUndefined (Boolean)
  , "Description'" :: NullOrUndefined (String)
  }
derive instance newtypeAgentConfigurationStatus :: Newtype AgentConfigurationStatus _


newtype AgentConfigurationStatusList = AgentConfigurationStatusList (Array AgentConfigurationStatus)
derive instance newtypeAgentConfigurationStatusList :: Newtype AgentConfigurationStatusList _


newtype AgentId = AgentId String
derive instance newtypeAgentId :: Newtype AgentId _


newtype AgentIds = AgentIds (Array AgentId)
derive instance newtypeAgentIds :: Newtype AgentIds _


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
derive instance newtypeAgentInfo :: Newtype AgentInfo _


-- | <p>Network details about the host where the agent/connector resides.</p>
newtype AgentNetworkInfo = AgentNetworkInfo 
  { "IpAddress'" :: NullOrUndefined (String)
  , "MacAddress'" :: NullOrUndefined (String)
  }
derive instance newtypeAgentNetworkInfo :: Newtype AgentNetworkInfo _


newtype AgentNetworkInfoList = AgentNetworkInfoList (Array AgentNetworkInfo)
derive instance newtypeAgentNetworkInfoList :: Newtype AgentNetworkInfoList _


newtype AgentStatus = AgentStatus String
derive instance newtypeAgentStatus :: Newtype AgentStatus _


newtype AgentsInfo = AgentsInfo (Array AgentInfo)
derive instance newtypeAgentsInfo :: Newtype AgentsInfo _


newtype ApplicationId = ApplicationId String
derive instance newtypeApplicationId :: Newtype ApplicationId _


newtype ApplicationIdsList = ApplicationIdsList (Array ApplicationId)
derive instance newtypeApplicationIdsList :: Newtype ApplicationIdsList _


newtype AssociateConfigurationItemsToApplicationRequest = AssociateConfigurationItemsToApplicationRequest 
  { "ApplicationConfigurationId'" :: (ApplicationId)
  , "ConfigurationIds'" :: (ConfigurationIdList)
  }
derive instance newtypeAssociateConfigurationItemsToApplicationRequest :: Newtype AssociateConfigurationItemsToApplicationRequest _


newtype AssociateConfigurationItemsToApplicationResponse = AssociateConfigurationItemsToApplicationResponse 
  { 
  }
derive instance newtypeAssociateConfigurationItemsToApplicationResponse :: Newtype AssociateConfigurationItemsToApplicationResponse _


-- | <p>The AWS user account does not have permission to perform the action. Check the IAM policy associated with this account.</p>
newtype AuthorizationErrorException = AuthorizationErrorException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeAuthorizationErrorException :: Newtype AuthorizationErrorException _


newtype BoxedInteger = BoxedInteger Int
derive instance newtypeBoxedInteger :: Newtype BoxedInteger _


newtype Condition = Condition String
derive instance newtypeCondition :: Newtype Condition _


newtype Configuration = Configuration (Map String String)
derive instance newtypeConfiguration :: Newtype Configuration _


newtype ConfigurationId = ConfigurationId String
derive instance newtypeConfigurationId :: Newtype ConfigurationId _


newtype ConfigurationIdList = ConfigurationIdList (Array ConfigurationId)
derive instance newtypeConfigurationIdList :: Newtype ConfigurationIdList _


newtype ConfigurationItemType = ConfigurationItemType String
derive instance newtypeConfigurationItemType :: Newtype ConfigurationItemType _


-- | <p>Tags for a configuration item. Tags are metadata that help you categorize IT assets.</p>
newtype ConfigurationTag = ConfigurationTag 
  { "ConfigurationType'" :: NullOrUndefined (ConfigurationItemType)
  , "ConfigurationId'" :: NullOrUndefined (ConfigurationId)
  , "Key'" :: NullOrUndefined (TagKey)
  , "Value'" :: NullOrUndefined (TagValue)
  , "TimeOfCreation'" :: NullOrUndefined (TimeStamp)
  }
derive instance newtypeConfigurationTag :: Newtype ConfigurationTag _


newtype ConfigurationTagSet = ConfigurationTagSet (Array ConfigurationTag)
derive instance newtypeConfigurationTagSet :: Newtype ConfigurationTagSet _


newtype Configurations = Configurations (Array Configuration)
derive instance newtypeConfigurations :: Newtype Configurations _


newtype ConfigurationsDownloadUrl = ConfigurationsDownloadUrl String
derive instance newtypeConfigurationsDownloadUrl :: Newtype ConfigurationsDownloadUrl _


newtype ConfigurationsExportId = ConfigurationsExportId String
derive instance newtypeConfigurationsExportId :: Newtype ConfigurationsExportId _


newtype CreateApplicationRequest = CreateApplicationRequest 
  { "Name'" :: (String)
  , "Description'" :: NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationRequest :: Newtype CreateApplicationRequest _


newtype CreateApplicationResponse = CreateApplicationResponse 
  { "ConfigurationId'" :: NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationResponse :: Newtype CreateApplicationResponse _


newtype CreateTagsRequest = CreateTagsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  , "Tags'" :: (TagSet)
  }
derive instance newtypeCreateTagsRequest :: Newtype CreateTagsRequest _


newtype CreateTagsResponse = CreateTagsResponse 
  { 
  }
derive instance newtypeCreateTagsResponse :: Newtype CreateTagsResponse _


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
derive instance newtypeCustomerAgentInfo :: Newtype CustomerAgentInfo _


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
derive instance newtypeCustomerConnectorInfo :: Newtype CustomerConnectorInfo _


newtype DeleteApplicationsRequest = DeleteApplicationsRequest 
  { "ConfigurationIds'" :: (ApplicationIdsList)
  }
derive instance newtypeDeleteApplicationsRequest :: Newtype DeleteApplicationsRequest _


newtype DeleteApplicationsResponse = DeleteApplicationsResponse 
  { 
  }
derive instance newtypeDeleteApplicationsResponse :: Newtype DeleteApplicationsResponse _


newtype DeleteTagsRequest = DeleteTagsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  , "Tags'" :: NullOrUndefined (TagSet)
  }
derive instance newtypeDeleteTagsRequest :: Newtype DeleteTagsRequest _


newtype DeleteTagsResponse = DeleteTagsResponse 
  { 
  }
derive instance newtypeDeleteTagsResponse :: Newtype DeleteTagsResponse _


newtype DescribeAgentsRequest = DescribeAgentsRequest 
  { "AgentIds'" :: NullOrUndefined (AgentIds)
  , "Filters'" :: NullOrUndefined (Filters)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAgentsRequest :: Newtype DescribeAgentsRequest _


newtype DescribeAgentsResponse = DescribeAgentsResponse 
  { "AgentsInfo'" :: NullOrUndefined (AgentsInfo)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAgentsResponse :: Newtype DescribeAgentsResponse _


newtype DescribeConfigurationsAttribute = DescribeConfigurationsAttribute (Map String String)
derive instance newtypeDescribeConfigurationsAttribute :: Newtype DescribeConfigurationsAttribute _


newtype DescribeConfigurationsAttributes = DescribeConfigurationsAttributes (Array DescribeConfigurationsAttribute)
derive instance newtypeDescribeConfigurationsAttributes :: Newtype DescribeConfigurationsAttributes _


newtype DescribeConfigurationsRequest = DescribeConfigurationsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  }
derive instance newtypeDescribeConfigurationsRequest :: Newtype DescribeConfigurationsRequest _


newtype DescribeConfigurationsResponse = DescribeConfigurationsResponse 
  { "Configurations'" :: NullOrUndefined (DescribeConfigurationsAttributes)
  }
derive instance newtypeDescribeConfigurationsResponse :: Newtype DescribeConfigurationsResponse _


newtype DescribeExportConfigurationsRequest = DescribeExportConfigurationsRequest 
  { "ExportIds'" :: NullOrUndefined (ExportIds)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportConfigurationsRequest :: Newtype DescribeExportConfigurationsRequest _


newtype DescribeExportConfigurationsResponse = DescribeExportConfigurationsResponse 
  { "ExportsInfo'" :: NullOrUndefined (ExportsInfo)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportConfigurationsResponse :: Newtype DescribeExportConfigurationsResponse _


newtype DescribeExportTasksRequest = DescribeExportTasksRequest 
  { "ExportIds'" :: NullOrUndefined (ExportIds)
  , "Filters'" :: NullOrUndefined (ExportFilters)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportTasksRequest :: Newtype DescribeExportTasksRequest _


newtype DescribeExportTasksResponse = DescribeExportTasksResponse 
  { "ExportsInfo'" :: NullOrUndefined (ExportsInfo)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportTasksResponse :: Newtype DescribeExportTasksResponse _


newtype DescribeTagsRequest = DescribeTagsRequest 
  { "Filters'" :: NullOrUndefined (TagFilters)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeTagsRequest :: Newtype DescribeTagsRequest _


newtype DescribeTagsResponse = DescribeTagsResponse 
  { "Tags'" :: NullOrUndefined (ConfigurationTagSet)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeTagsResponse :: Newtype DescribeTagsResponse _


newtype DisassociateConfigurationItemsFromApplicationRequest = DisassociateConfigurationItemsFromApplicationRequest 
  { "ApplicationConfigurationId'" :: (ApplicationId)
  , "ConfigurationIds'" :: (ConfigurationIdList)
  }
derive instance newtypeDisassociateConfigurationItemsFromApplicationRequest :: Newtype DisassociateConfigurationItemsFromApplicationRequest _


newtype DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse 
  { 
  }
derive instance newtypeDisassociateConfigurationItemsFromApplicationResponse :: Newtype DisassociateConfigurationItemsFromApplicationResponse _


newtype ExportConfigurationsResponse = ExportConfigurationsResponse 
  { "ExportId'" :: NullOrUndefined (ConfigurationsExportId)
  }
derive instance newtypeExportConfigurationsResponse :: Newtype ExportConfigurationsResponse _


newtype ExportDataFormat = ExportDataFormat String
derive instance newtypeExportDataFormat :: Newtype ExportDataFormat _


newtype ExportDataFormats = ExportDataFormats (Array ExportDataFormat)
derive instance newtypeExportDataFormats :: Newtype ExportDataFormats _


-- | <p>Used to select which agent's data is to be exported. A single agent ID may be selected for export using the <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html">StartExportTask</a> action.</p>
newtype ExportFilter = ExportFilter 
  { "Name'" :: (FilterName)
  , "Values'" :: (FilterValues)
  , "Condition'" :: (Condition)
  }
derive instance newtypeExportFilter :: Newtype ExportFilter _


newtype ExportFilters = ExportFilters (Array ExportFilter)
derive instance newtypeExportFilters :: Newtype ExportFilters _


newtype ExportIds = ExportIds (Array ConfigurationsExportId)
derive instance newtypeExportIds :: Newtype ExportIds _


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
derive instance newtypeExportInfo :: Newtype ExportInfo _


newtype ExportRequestTime = ExportRequestTime Number
derive instance newtypeExportRequestTime :: Newtype ExportRequestTime _


newtype ExportStatus = ExportStatus String
derive instance newtypeExportStatus :: Newtype ExportStatus _


newtype ExportStatusMessage = ExportStatusMessage String
derive instance newtypeExportStatusMessage :: Newtype ExportStatusMessage _


newtype ExportsInfo = ExportsInfo (Array ExportInfo)
derive instance newtypeExportsInfo :: Newtype ExportsInfo _


-- | <p>A filter that can use conditional operators.</p> <p>For more information about filters, see <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html">Querying Discovered Configuration Items</a>. </p>
newtype Filter = Filter 
  { "Name'" :: (String)
  , "Values'" :: (FilterValues)
  , "Condition'" :: (Condition)
  }
derive instance newtypeFilter :: Newtype Filter _


newtype FilterName = FilterName String
derive instance newtypeFilterName :: Newtype FilterName _


newtype FilterValue = FilterValue String
derive instance newtypeFilterValue :: Newtype FilterValue _


newtype FilterValues = FilterValues (Array FilterValue)
derive instance newtypeFilterValues :: Newtype FilterValues _


newtype Filters = Filters (Array Filter)
derive instance newtypeFilters :: Newtype Filters _


newtype GetDiscoverySummaryRequest = GetDiscoverySummaryRequest 
  { 
  }
derive instance newtypeGetDiscoverySummaryRequest :: Newtype GetDiscoverySummaryRequest _


newtype GetDiscoverySummaryResponse = GetDiscoverySummaryResponse 
  { "Servers'" :: NullOrUndefined (Number)
  , "Applications'" :: NullOrUndefined (Number)
  , "ServersMappedToApplications'" :: NullOrUndefined (Number)
  , "ServersMappedtoTags'" :: NullOrUndefined (Number)
  , "AgentSummary'" :: NullOrUndefined (CustomerAgentInfo)
  , "ConnectorSummary'" :: NullOrUndefined (CustomerConnectorInfo)
  }
derive instance newtypeGetDiscoverySummaryResponse :: Newtype GetDiscoverySummaryResponse _


-- | <p>One or more parameters are not valid. Verify the parameters and try again.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | <p>The value of one or more parameters are either invalid or out of range. Verify the parameter values and try again.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _


newtype ListConfigurationsRequest = ListConfigurationsRequest 
  { "ConfigurationType'" :: (ConfigurationItemType)
  , "Filters'" :: NullOrUndefined (Filters)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "OrderBy'" :: NullOrUndefined (OrderByList)
  }
derive instance newtypeListConfigurationsRequest :: Newtype ListConfigurationsRequest _


newtype ListConfigurationsResponse = ListConfigurationsResponse 
  { "Configurations'" :: NullOrUndefined (Configurations)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListConfigurationsResponse :: Newtype ListConfigurationsResponse _


newtype ListServerNeighborsRequest = ListServerNeighborsRequest 
  { "ConfigurationId'" :: (ConfigurationId)
  , "PortInformationNeeded'" :: NullOrUndefined (Boolean)
  , "NeighborConfigurationIds'" :: NullOrUndefined (ConfigurationIdList)
  , "MaxResults'" :: NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListServerNeighborsRequest :: Newtype ListServerNeighborsRequest _


newtype ListServerNeighborsResponse = ListServerNeighborsResponse 
  { "Neighbors'" :: (NeighborDetailsList)
  , "NextToken'" :: NullOrUndefined (String)
  , "KnownDependencyCount'" :: NullOrUndefined (Number)
  }
derive instance newtypeListServerNeighborsResponse :: Newtype ListServerNeighborsResponse _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


-- | <p>Details about neighboring servers.</p>
newtype NeighborConnectionDetail = NeighborConnectionDetail 
  { "SourceServerId'" :: (ConfigurationId)
  , "DestinationServerId'" :: (ConfigurationId)
  , "DestinationPort'" :: NullOrUndefined (BoxedInteger)
  , "TransportProtocol'" :: NullOrUndefined (String)
  , "ConnectionsCount'" :: (Number)
  }
derive instance newtypeNeighborConnectionDetail :: Newtype NeighborConnectionDetail _


newtype NeighborDetailsList = NeighborDetailsList (Array NeighborConnectionDetail)
derive instance newtypeNeighborDetailsList :: Newtype NeighborDetailsList _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>This operation is not permitted.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeOperationNotPermittedException :: Newtype OperationNotPermittedException _


-- | <p>A field and direction for ordered output.</p>
newtype OrderByElement = OrderByElement 
  { "FieldName'" :: (String)
  , "SortOrder'" :: NullOrUndefined (OrderString')
  }
derive instance newtypeOrderByElement :: Newtype OrderByElement _


newtype OrderByList = OrderByList (Array OrderByElement)
derive instance newtypeOrderByList :: Newtype OrderByList _


-- | <p>The specified configuration ID was not located. Verify the configuration ID and try again.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The server experienced an internal error. Try again.</p>
newtype ServerInternalErrorException = ServerInternalErrorException 
  { "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeServerInternalErrorException :: Newtype ServerInternalErrorException _


newtype StartDataCollectionByAgentIdsRequest = StartDataCollectionByAgentIdsRequest 
  { "AgentIds'" :: (AgentIds)
  }
derive instance newtypeStartDataCollectionByAgentIdsRequest :: Newtype StartDataCollectionByAgentIdsRequest _


newtype StartDataCollectionByAgentIdsResponse = StartDataCollectionByAgentIdsResponse 
  { "AgentsConfigurationStatus'" :: NullOrUndefined (AgentConfigurationStatusList)
  }
derive instance newtypeStartDataCollectionByAgentIdsResponse :: Newtype StartDataCollectionByAgentIdsResponse _


newtype StartExportTaskRequest = StartExportTaskRequest 
  { "ExportDataFormat'" :: NullOrUndefined (ExportDataFormats)
  , "Filters'" :: NullOrUndefined (ExportFilters)
  , "StartTime'" :: NullOrUndefined (TimeStamp)
  , "EndTime'" :: NullOrUndefined (TimeStamp)
  }
derive instance newtypeStartExportTaskRequest :: Newtype StartExportTaskRequest _


newtype StartExportTaskResponse = StartExportTaskResponse 
  { "ExportId'" :: NullOrUndefined (ConfigurationsExportId)
  }
derive instance newtypeStartExportTaskResponse :: Newtype StartExportTaskResponse _


newtype StopDataCollectionByAgentIdsRequest = StopDataCollectionByAgentIdsRequest 
  { "AgentIds'" :: (AgentIds)
  }
derive instance newtypeStopDataCollectionByAgentIdsRequest :: Newtype StopDataCollectionByAgentIdsRequest _


newtype StopDataCollectionByAgentIdsResponse = StopDataCollectionByAgentIdsResponse 
  { "AgentsConfigurationStatus'" :: NullOrUndefined (AgentConfigurationStatusList)
  }
derive instance newtypeStopDataCollectionByAgentIdsResponse :: Newtype StopDataCollectionByAgentIdsResponse _


-- | <p>Metadata that help you categorize IT assets.</p>
newtype Tag = Tag 
  { "Key'" :: (TagKey)
  , "Value'" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


-- | <p>The tag filter. Valid names are: <code>tagKey</code>, <code>tagValue</code>, <code>configurationId</code>.</p>
newtype TagFilter = TagFilter 
  { "Name'" :: (FilterName)
  , "Values'" :: (FilterValues)
  }
derive instance newtypeTagFilter :: Newtype TagFilter _


newtype TagFilters = TagFilters (Array TagFilter)
derive instance newtypeTagFilters :: Newtype TagFilters _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagSet = TagSet (Array Tag)
derive instance newtypeTagSet :: Newtype TagSet _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype TimeStamp = TimeStamp Number
derive instance newtypeTimeStamp :: Newtype TimeStamp _


newtype UpdateApplicationRequest = UpdateApplicationRequest 
  { "ConfigurationId'" :: (ApplicationId)
  , "Name'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (String)
  }
derive instance newtypeUpdateApplicationRequest :: Newtype UpdateApplicationRequest _


newtype UpdateApplicationResponse = UpdateApplicationResponse 
  { 
  }
derive instance newtypeUpdateApplicationResponse :: Newtype UpdateApplicationResponse _


newtype OrderString' = OrderString' String
derive instance newtypeOrderString' :: Newtype OrderString' _
