

-- | <fullname>AWS Application Discovery Service</fullname> <p>AWS Application Discovery Service helps you plan application migration projects by automatically identifying servers, virtual machines (VMs), software, and software dependencies running in your on-premises data centers. Application Discovery Service also collects application performance data, which can help you assess the outcome of your migration. The data collected by Application Discovery Service is securely retained in an Amazon-hosted and managed database in the cloud. You can export the data as a CSV or XML file into your preferred visualization tool or cloud-migration solution to plan your migration. For more information, see the Application Discovery Service <a href="http://aws.amazon.com/application-discovery/faqs/">FAQ</a>.</p> <p>Application Discovery Service offers two modes of operation.</p> <ul> <li> <p> <b>Agentless discovery</b> mode is recommended for environments that use VMware vCenter Server. This mode doesn't require you to install an agent on each host. Agentless discovery gathers server information regardless of the operating systems, which minimizes the time required for initial on-premises infrastructure assessment. Agentless discovery doesn't collect information about software and software dependencies. It also doesn't work in non-VMware environments. We recommend that you use agent-based discovery for non-VMware environments and if you want to collect information about software and software dependencies. You can also run agent-based and agentless discovery simultaneously. Use agentless discovery to quickly complete the initial infrastructure assessment and then install agents on select hosts to gather information about software and software dependencies.</p> </li> <li> <p> <b>Agent-based discovery</b> mode collects a richer set of data than agentless discovery by using Amazon software, the AWS Application Discovery Agent, which you install on one or more hosts in your data center. The agent captures infrastructure and application information, including an inventory of installed software applications, system and process performance, resource utilization, and network dependencies between workloads. The information collected by agents is secured at rest and in transit to the Application Discovery Service database in the cloud. </p> </li> </ul> <p>Application Discovery Service integrates with application discovery solutions from AWS Partner Network (APN) partners. Third-party application discovery tools can query Application Discovery Service and write to the Application Discovery Service database using a public API. You can then import the data into either a visualization tool or cloud-migration solution.</p> <important> <p>Application Discovery Service doesn't gather sensitive information. All data is handled according to the <a href="http://aws.amazon.com/privacy/">AWS Privacy Policy</a>. You can operate Application Discovery Service using offline mode to inspect collected data before it is shared with the service.</p> </important> <p>Your AWS account must be granted access to Application Discovery Service, a process called <i>whitelisting</i>. This is true for AWS partners and customers alike. To request access, <a href="http://aws.amazon.com/application-discovery/how-to-start/"> sign up for AWS Application Discovery Service</a>.</p> <p>This API reference provides descriptions, syntax, and usage examples for each of the actions and data types for Application Discovery Service. The topic for each action shows the API request parameters and the response. Alternatively, you can use one of the AWS SDKs to access an API that is tailored to the programming language or platform that you're using. For more information, see <a href="http://aws.amazon.com/tools/#SDKs">AWS SDKs</a>.</p> <p>This guide is intended for use with the <a href="http://docs.aws.amazon.com/application-discovery/latest/userguide/"> <i>AWS Application Discovery Service User Guide</i> </a>.</p>
module AWS.Discovery where

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

serviceName = "Discovery" :: String


-- | <p>Associates one or more configuration items with an application.</p>
associateConfigurationItemsToApplication :: forall eff. AssociateConfigurationItemsToApplicationRequest -> Aff (exception :: EXCEPTION | eff) AssociateConfigurationItemsToApplicationResponse
associateConfigurationItemsToApplication = Request.request serviceName "associateConfigurationItemsToApplication" 


-- | <p>Creates an application with the given name and description.</p>
createApplication :: forall eff. CreateApplicationRequest -> Aff (exception :: EXCEPTION | eff) CreateApplicationResponse
createApplication = Request.request serviceName "createApplication" 


-- | <p>Creates one or more tags for configuration items. Tags are metadata that help you categorize IT assets. This API accepts a list of multiple configuration items.</p>
createTags :: forall eff. CreateTagsRequest -> Aff (exception :: EXCEPTION | eff) CreateTagsResponse
createTags = Request.request serviceName "createTags" 


-- | <p>Deletes a list of applications and their associations with configuration items.</p>
deleteApplications :: forall eff. DeleteApplicationsRequest -> Aff (exception :: EXCEPTION | eff) DeleteApplicationsResponse
deleteApplications = Request.request serviceName "deleteApplications" 


-- | <p>Deletes the association between configuration items and one or more tags. This API accepts a list of multiple configuration items.</p>
deleteTags :: forall eff. DeleteTagsRequest -> Aff (exception :: EXCEPTION | eff) DeleteTagsResponse
deleteTags = Request.request serviceName "deleteTags" 


-- | <p>Lists agents or the Connector by ID or lists all agents/Connectors associated with your user account if you did not specify an ID.</p>
describeAgents :: forall eff. DescribeAgentsRequest -> Aff (exception :: EXCEPTION | eff) DescribeAgentsResponse
describeAgents = Request.request serviceName "describeAgents" 


-- | <p>Retrieves attributes for a list of configuration item IDs. All of the supplied IDs must be for the same asset type (server, application, process, or connection). Output fields are specific to the asset type selected. For example, the output for a <i>server</i> configuration item includes a list of attributes about the server, such as host name, operating system, and number of network cards.</p> <p>For a complete list of outputs for each asset type, see <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html#DescribeConfigurations">Using the DescribeConfigurations Action</a>.</p>
describeConfigurations :: forall eff. DescribeConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) DescribeConfigurationsResponse
describeConfigurations = Request.request serviceName "describeConfigurations" 


-- | <p>Deprecated. Use <code>DescribeExportTasks</code> instead.</p> <p>Retrieves the status of a given export process. You can retrieve status from a maximum of 100 processes.</p>
describeExportConfigurations :: forall eff. DescribeExportConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) DescribeExportConfigurationsResponse
describeExportConfigurations = Request.request serviceName "describeExportConfigurations" 


-- | <p>Retrieve status of one or more export tasks. You can retrieve the status of up to 100 export tasks.</p>
describeExportTasks :: forall eff. DescribeExportTasksRequest -> Aff (exception :: EXCEPTION | eff) DescribeExportTasksResponse
describeExportTasks = Request.request serviceName "describeExportTasks" 


-- | <p>Retrieves a list of configuration items that are tagged with a specific tag. Or retrieves a list of all tags assigned to a specific configuration item.</p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (exception :: EXCEPTION | eff) DescribeTagsResponse
describeTags = Request.request serviceName "describeTags" 


-- | <p>Disassociates one or more configuration items from an application.</p>
disassociateConfigurationItemsFromApplication :: forall eff. DisassociateConfigurationItemsFromApplicationRequest -> Aff (exception :: EXCEPTION | eff) DisassociateConfigurationItemsFromApplicationResponse
disassociateConfigurationItemsFromApplication = Request.request serviceName "disassociateConfigurationItemsFromApplication" 


-- | <p>Deprecated. Use <code>StartExportTask</code> instead.</p> <p>Exports all discovered configuration data to an Amazon S3 bucket or an application that enables you to view and evaluate the data. Data includes tags and tag associations, processes, connections, servers, and system performance. This API returns an export ID that you can query using the <i>DescribeExportConfigurations</i> API. The system imposes a limit of two configuration exports in six hours.</p>
exportConfigurations :: forall eff.  Aff (exception :: EXCEPTION | eff) ExportConfigurationsResponse
exportConfigurations = Request.request serviceName "exportConfigurations" (Types.NoInput unit)


-- | <p>Retrieves a short summary of discovered assets.</p>
getDiscoverySummary :: forall eff. GetDiscoverySummaryRequest -> Aff (exception :: EXCEPTION | eff) GetDiscoverySummaryResponse
getDiscoverySummary = Request.request serviceName "getDiscoverySummary" 


-- | <p>Retrieves a list of configuration items according to criteria that you specify in a filter. The filter criteria identifies the relationship requirements.</p>
listConfigurations :: forall eff. ListConfigurationsRequest -> Aff (exception :: EXCEPTION | eff) ListConfigurationsResponse
listConfigurations = Request.request serviceName "listConfigurations" 


-- | <p>Retrieves a list of servers that are one network hop away from a specified server.</p>
listServerNeighbors :: forall eff. ListServerNeighborsRequest -> Aff (exception :: EXCEPTION | eff) ListServerNeighborsResponse
listServerNeighbors = Request.request serviceName "listServerNeighbors" 


-- | <p>Instructs the specified agents or connectors to start collecting data.</p>
startDataCollectionByAgentIds :: forall eff. StartDataCollectionByAgentIdsRequest -> Aff (exception :: EXCEPTION | eff) StartDataCollectionByAgentIdsResponse
startDataCollectionByAgentIds = Request.request serviceName "startDataCollectionByAgentIds" 


-- | <p> Begins the export of discovered data to an S3 bucket.</p> <p> If you specify <code>agentIds</code> in a filter, the task exports up to 72 hours of detailed data collected by the identified Application Discovery Agent, including network, process, and performance details. A time range for exported agent data may be set by using <code>startTime</code> and <code>endTime</code>. Export of detailed agent data is limited to five concurrently running exports. </p> <p> If you do not include an <code>agentIds</code> filter, summary data is exported that includes both AWS Agentless Discovery Connector data and summary data from AWS Discovery Agents. Export of summary data is limited to two exports per day. </p>
startExportTask :: forall eff. StartExportTaskRequest -> Aff (exception :: EXCEPTION | eff) StartExportTaskResponse
startExportTask = Request.request serviceName "startExportTask" 


-- | <p>Instructs the specified agents or connectors to stop collecting data.</p>
stopDataCollectionByAgentIds :: forall eff. StopDataCollectionByAgentIdsRequest -> Aff (exception :: EXCEPTION | eff) StopDataCollectionByAgentIdsResponse
stopDataCollectionByAgentIds = Request.request serviceName "stopDataCollectionByAgentIds" 


-- | <p>Updates metadata about an application.</p>
updateApplication :: forall eff. UpdateApplicationRequest -> Aff (exception :: EXCEPTION | eff) UpdateApplicationResponse
updateApplication = Request.request serviceName "updateApplication" 


-- | <p>Information about agents or connectors that were instructed to start collecting data. Information includes the agent/connector ID, a description of the operation, and whether the agent/connector configuration was updated.</p>
newtype AgentConfigurationStatus = AgentConfigurationStatus 
  { "AgentId'" :: NullOrUndefined.NullOrUndefined (String)
  , "OperationSucceeded'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAgentConfigurationStatus :: Newtype AgentConfigurationStatus _
derive instance repGenericAgentConfigurationStatus :: Generic AgentConfigurationStatus _
instance showAgentConfigurationStatus :: Show AgentConfigurationStatus where
  show = genericShow
instance decodeAgentConfigurationStatus :: Decode AgentConfigurationStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentConfigurationStatus :: Encode AgentConfigurationStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AgentConfigurationStatusList = AgentConfigurationStatusList (Array AgentConfigurationStatus)
derive instance newtypeAgentConfigurationStatusList :: Newtype AgentConfigurationStatusList _
derive instance repGenericAgentConfigurationStatusList :: Generic AgentConfigurationStatusList _
instance showAgentConfigurationStatusList :: Show AgentConfigurationStatusList where
  show = genericShow
instance decodeAgentConfigurationStatusList :: Decode AgentConfigurationStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentConfigurationStatusList :: Encode AgentConfigurationStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AgentId = AgentId String
derive instance newtypeAgentId :: Newtype AgentId _
derive instance repGenericAgentId :: Generic AgentId _
instance showAgentId :: Show AgentId where
  show = genericShow
instance decodeAgentId :: Decode AgentId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentId :: Encode AgentId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AgentIds = AgentIds (Array AgentId)
derive instance newtypeAgentIds :: Newtype AgentIds _
derive instance repGenericAgentIds :: Generic AgentIds _
instance showAgentIds :: Show AgentIds where
  show = genericShow
instance decodeAgentIds :: Decode AgentIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentIds :: Encode AgentIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about agents or connectors associated with the user’s AWS account. Information includes agent or connector IDs, IP addresses, media access control (MAC) addresses, agent or connector health, hostname where the agent or connector resides, and agent version for each agent.</p>
newtype AgentInfo = AgentInfo 
  { "AgentId'" :: NullOrUndefined.NullOrUndefined (AgentId)
  , "HostName'" :: NullOrUndefined.NullOrUndefined (String)
  , "AgentNetworkInfoList'" :: NullOrUndefined.NullOrUndefined (AgentNetworkInfoList)
  , "ConnectorId'" :: NullOrUndefined.NullOrUndefined (String)
  , "Version'" :: NullOrUndefined.NullOrUndefined (String)
  , "Health'" :: NullOrUndefined.NullOrUndefined (AgentStatus)
  , "LastHealthPingTime'" :: NullOrUndefined.NullOrUndefined (String)
  , "CollectionStatus'" :: NullOrUndefined.NullOrUndefined (String)
  , "AgentType'" :: NullOrUndefined.NullOrUndefined (String)
  , "RegisteredTime'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAgentInfo :: Newtype AgentInfo _
derive instance repGenericAgentInfo :: Generic AgentInfo _
instance showAgentInfo :: Show AgentInfo where
  show = genericShow
instance decodeAgentInfo :: Decode AgentInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentInfo :: Encode AgentInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Network details about the host where the agent/connector resides.</p>
newtype AgentNetworkInfo = AgentNetworkInfo 
  { "IpAddress'" :: NullOrUndefined.NullOrUndefined (String)
  , "MacAddress'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeAgentNetworkInfo :: Newtype AgentNetworkInfo _
derive instance repGenericAgentNetworkInfo :: Generic AgentNetworkInfo _
instance showAgentNetworkInfo :: Show AgentNetworkInfo where
  show = genericShow
instance decodeAgentNetworkInfo :: Decode AgentNetworkInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentNetworkInfo :: Encode AgentNetworkInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AgentNetworkInfoList = AgentNetworkInfoList (Array AgentNetworkInfo)
derive instance newtypeAgentNetworkInfoList :: Newtype AgentNetworkInfoList _
derive instance repGenericAgentNetworkInfoList :: Generic AgentNetworkInfoList _
instance showAgentNetworkInfoList :: Show AgentNetworkInfoList where
  show = genericShow
instance decodeAgentNetworkInfoList :: Decode AgentNetworkInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentNetworkInfoList :: Encode AgentNetworkInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AgentStatus = AgentStatus String
derive instance newtypeAgentStatus :: Newtype AgentStatus _
derive instance repGenericAgentStatus :: Generic AgentStatus _
instance showAgentStatus :: Show AgentStatus where
  show = genericShow
instance decodeAgentStatus :: Decode AgentStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentStatus :: Encode AgentStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AgentsInfo = AgentsInfo (Array AgentInfo)
derive instance newtypeAgentsInfo :: Newtype AgentsInfo _
derive instance repGenericAgentsInfo :: Generic AgentsInfo _
instance showAgentsInfo :: Show AgentsInfo where
  show = genericShow
instance decodeAgentsInfo :: Decode AgentsInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAgentsInfo :: Encode AgentsInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationId = ApplicationId String
derive instance newtypeApplicationId :: Newtype ApplicationId _
derive instance repGenericApplicationId :: Generic ApplicationId _
instance showApplicationId :: Show ApplicationId where
  show = genericShow
instance decodeApplicationId :: Decode ApplicationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationId :: Encode ApplicationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApplicationIdsList = ApplicationIdsList (Array ApplicationId)
derive instance newtypeApplicationIdsList :: Newtype ApplicationIdsList _
derive instance repGenericApplicationIdsList :: Generic ApplicationIdsList _
instance showApplicationIdsList :: Show ApplicationIdsList where
  show = genericShow
instance decodeApplicationIdsList :: Decode ApplicationIdsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApplicationIdsList :: Encode ApplicationIdsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateConfigurationItemsToApplicationRequest = AssociateConfigurationItemsToApplicationRequest 
  { "ApplicationConfigurationId'" :: (ApplicationId)
  , "ConfigurationIds'" :: (ConfigurationIdList)
  }
derive instance newtypeAssociateConfigurationItemsToApplicationRequest :: Newtype AssociateConfigurationItemsToApplicationRequest _
derive instance repGenericAssociateConfigurationItemsToApplicationRequest :: Generic AssociateConfigurationItemsToApplicationRequest _
instance showAssociateConfigurationItemsToApplicationRequest :: Show AssociateConfigurationItemsToApplicationRequest where
  show = genericShow
instance decodeAssociateConfigurationItemsToApplicationRequest :: Decode AssociateConfigurationItemsToApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateConfigurationItemsToApplicationRequest :: Encode AssociateConfigurationItemsToApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AssociateConfigurationItemsToApplicationResponse = AssociateConfigurationItemsToApplicationResponse Types.NoArguments
derive instance newtypeAssociateConfigurationItemsToApplicationResponse :: Newtype AssociateConfigurationItemsToApplicationResponse _
derive instance repGenericAssociateConfigurationItemsToApplicationResponse :: Generic AssociateConfigurationItemsToApplicationResponse _
instance showAssociateConfigurationItemsToApplicationResponse :: Show AssociateConfigurationItemsToApplicationResponse where
  show = genericShow
instance decodeAssociateConfigurationItemsToApplicationResponse :: Decode AssociateConfigurationItemsToApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAssociateConfigurationItemsToApplicationResponse :: Encode AssociateConfigurationItemsToApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The AWS user account does not have permission to perform the action. Check the IAM policy associated with this account.</p>
newtype AuthorizationErrorException = AuthorizationErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeAuthorizationErrorException :: Newtype AuthorizationErrorException _
derive instance repGenericAuthorizationErrorException :: Generic AuthorizationErrorException _
instance showAuthorizationErrorException :: Show AuthorizationErrorException where
  show = genericShow
instance decodeAuthorizationErrorException :: Decode AuthorizationErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthorizationErrorException :: Encode AuthorizationErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BoxedInteger = BoxedInteger Int
derive instance newtypeBoxedInteger :: Newtype BoxedInteger _
derive instance repGenericBoxedInteger :: Generic BoxedInteger _
instance showBoxedInteger :: Show BoxedInteger where
  show = genericShow
instance decodeBoxedInteger :: Decode BoxedInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBoxedInteger :: Encode BoxedInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Condition = Condition String
derive instance newtypeCondition :: Newtype Condition _
derive instance repGenericCondition :: Generic Condition _
instance showCondition :: Show Condition where
  show = genericShow
instance decodeCondition :: Decode Condition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCondition :: Encode Condition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Configuration = Configuration (StrMap.StrMap String)
derive instance newtypeConfiguration :: Newtype Configuration _
derive instance repGenericConfiguration :: Generic Configuration _
instance showConfiguration :: Show Configuration where
  show = genericShow
instance decodeConfiguration :: Decode Configuration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfiguration :: Encode Configuration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationId = ConfigurationId String
derive instance newtypeConfigurationId :: Newtype ConfigurationId _
derive instance repGenericConfigurationId :: Generic ConfigurationId _
instance showConfigurationId :: Show ConfigurationId where
  show = genericShow
instance decodeConfigurationId :: Decode ConfigurationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationId :: Encode ConfigurationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationIdList = ConfigurationIdList (Array ConfigurationId)
derive instance newtypeConfigurationIdList :: Newtype ConfigurationIdList _
derive instance repGenericConfigurationIdList :: Generic ConfigurationIdList _
instance showConfigurationIdList :: Show ConfigurationIdList where
  show = genericShow
instance decodeConfigurationIdList :: Decode ConfigurationIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationIdList :: Encode ConfigurationIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationItemType = ConfigurationItemType String
derive instance newtypeConfigurationItemType :: Newtype ConfigurationItemType _
derive instance repGenericConfigurationItemType :: Generic ConfigurationItemType _
instance showConfigurationItemType :: Show ConfigurationItemType where
  show = genericShow
instance decodeConfigurationItemType :: Decode ConfigurationItemType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationItemType :: Encode ConfigurationItemType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Tags for a configuration item. Tags are metadata that help you categorize IT assets.</p>
newtype ConfigurationTag = ConfigurationTag 
  { "ConfigurationType'" :: NullOrUndefined.NullOrUndefined (ConfigurationItemType)
  , "ConfigurationId'" :: NullOrUndefined.NullOrUndefined (ConfigurationId)
  , "Key'" :: NullOrUndefined.NullOrUndefined (TagKey)
  , "Value'" :: NullOrUndefined.NullOrUndefined (TagValue)
  , "TimeOfCreation'" :: NullOrUndefined.NullOrUndefined (TimeStamp)
  }
derive instance newtypeConfigurationTag :: Newtype ConfigurationTag _
derive instance repGenericConfigurationTag :: Generic ConfigurationTag _
instance showConfigurationTag :: Show ConfigurationTag where
  show = genericShow
instance decodeConfigurationTag :: Decode ConfigurationTag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationTag :: Encode ConfigurationTag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationTagSet = ConfigurationTagSet (Array ConfigurationTag)
derive instance newtypeConfigurationTagSet :: Newtype ConfigurationTagSet _
derive instance repGenericConfigurationTagSet :: Generic ConfigurationTagSet _
instance showConfigurationTagSet :: Show ConfigurationTagSet where
  show = genericShow
instance decodeConfigurationTagSet :: Decode ConfigurationTagSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationTagSet :: Encode ConfigurationTagSet where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Configurations = Configurations (Array Configuration)
derive instance newtypeConfigurations :: Newtype Configurations _
derive instance repGenericConfigurations :: Generic Configurations _
instance showConfigurations :: Show Configurations where
  show = genericShow
instance decodeConfigurations :: Decode Configurations where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurations :: Encode Configurations where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationsDownloadUrl = ConfigurationsDownloadUrl String
derive instance newtypeConfigurationsDownloadUrl :: Newtype ConfigurationsDownloadUrl _
derive instance repGenericConfigurationsDownloadUrl :: Generic ConfigurationsDownloadUrl _
instance showConfigurationsDownloadUrl :: Show ConfigurationsDownloadUrl where
  show = genericShow
instance decodeConfigurationsDownloadUrl :: Decode ConfigurationsDownloadUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationsDownloadUrl :: Encode ConfigurationsDownloadUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConfigurationsExportId = ConfigurationsExportId String
derive instance newtypeConfigurationsExportId :: Newtype ConfigurationsExportId _
derive instance repGenericConfigurationsExportId :: Generic ConfigurationsExportId _
instance showConfigurationsExportId :: Show ConfigurationsExportId where
  show = genericShow
instance decodeConfigurationsExportId :: Decode ConfigurationsExportId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConfigurationsExportId :: Encode ConfigurationsExportId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateApplicationRequest = CreateApplicationRequest 
  { "Name'" :: (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationRequest :: Newtype CreateApplicationRequest _
derive instance repGenericCreateApplicationRequest :: Generic CreateApplicationRequest _
instance showCreateApplicationRequest :: Show CreateApplicationRequest where
  show = genericShow
instance decodeCreateApplicationRequest :: Decode CreateApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationRequest :: Encode CreateApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateApplicationResponse = CreateApplicationResponse 
  { "ConfigurationId'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeCreateApplicationResponse :: Newtype CreateApplicationResponse _
derive instance repGenericCreateApplicationResponse :: Generic CreateApplicationResponse _
instance showCreateApplicationResponse :: Show CreateApplicationResponse where
  show = genericShow
instance decodeCreateApplicationResponse :: Decode CreateApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateApplicationResponse :: Encode CreateApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTagsRequest = CreateTagsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  , "Tags'" :: (TagSet)
  }
derive instance newtypeCreateTagsRequest :: Newtype CreateTagsRequest _
derive instance repGenericCreateTagsRequest :: Generic CreateTagsRequest _
instance showCreateTagsRequest :: Show CreateTagsRequest where
  show = genericShow
instance decodeCreateTagsRequest :: Decode CreateTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTagsRequest :: Encode CreateTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTagsResponse = CreateTagsResponse Types.NoArguments
derive instance newtypeCreateTagsResponse :: Newtype CreateTagsResponse _
derive instance repGenericCreateTagsResponse :: Generic CreateTagsResponse _
instance showCreateTagsResponse :: Show CreateTagsResponse where
  show = genericShow
instance decodeCreateTagsResponse :: Decode CreateTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTagsResponse :: Encode CreateTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


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
derive instance repGenericCustomerAgentInfo :: Generic CustomerAgentInfo _
instance showCustomerAgentInfo :: Show CustomerAgentInfo where
  show = genericShow
instance decodeCustomerAgentInfo :: Decode CustomerAgentInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomerAgentInfo :: Encode CustomerAgentInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


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
derive instance repGenericCustomerConnectorInfo :: Generic CustomerConnectorInfo _
instance showCustomerConnectorInfo :: Show CustomerConnectorInfo where
  show = genericShow
instance decodeCustomerConnectorInfo :: Decode CustomerConnectorInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomerConnectorInfo :: Encode CustomerConnectorInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationsRequest = DeleteApplicationsRequest 
  { "ConfigurationIds'" :: (ApplicationIdsList)
  }
derive instance newtypeDeleteApplicationsRequest :: Newtype DeleteApplicationsRequest _
derive instance repGenericDeleteApplicationsRequest :: Generic DeleteApplicationsRequest _
instance showDeleteApplicationsRequest :: Show DeleteApplicationsRequest where
  show = genericShow
instance decodeDeleteApplicationsRequest :: Decode DeleteApplicationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationsRequest :: Encode DeleteApplicationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteApplicationsResponse = DeleteApplicationsResponse Types.NoArguments
derive instance newtypeDeleteApplicationsResponse :: Newtype DeleteApplicationsResponse _
derive instance repGenericDeleteApplicationsResponse :: Generic DeleteApplicationsResponse _
instance showDeleteApplicationsResponse :: Show DeleteApplicationsResponse where
  show = genericShow
instance decodeDeleteApplicationsResponse :: Decode DeleteApplicationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteApplicationsResponse :: Encode DeleteApplicationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTagsRequest = DeleteTagsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagSet)
  }
derive instance newtypeDeleteTagsRequest :: Newtype DeleteTagsRequest _
derive instance repGenericDeleteTagsRequest :: Generic DeleteTagsRequest _
instance showDeleteTagsRequest :: Show DeleteTagsRequest where
  show = genericShow
instance decodeDeleteTagsRequest :: Decode DeleteTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTagsRequest :: Encode DeleteTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTagsResponse = DeleteTagsResponse Types.NoArguments
derive instance newtypeDeleteTagsResponse :: Newtype DeleteTagsResponse _
derive instance repGenericDeleteTagsResponse :: Generic DeleteTagsResponse _
instance showDeleteTagsResponse :: Show DeleteTagsResponse where
  show = genericShow
instance decodeDeleteTagsResponse :: Decode DeleteTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTagsResponse :: Encode DeleteTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAgentsRequest = DescribeAgentsRequest 
  { "AgentIds'" :: NullOrUndefined.NullOrUndefined (AgentIds)
  , "Filters'" :: NullOrUndefined.NullOrUndefined (Filters)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAgentsRequest :: Newtype DescribeAgentsRequest _
derive instance repGenericDescribeAgentsRequest :: Generic DescribeAgentsRequest _
instance showDescribeAgentsRequest :: Show DescribeAgentsRequest where
  show = genericShow
instance decodeDescribeAgentsRequest :: Decode DescribeAgentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAgentsRequest :: Encode DescribeAgentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeAgentsResponse = DescribeAgentsResponse 
  { "AgentsInfo'" :: NullOrUndefined.NullOrUndefined (AgentsInfo)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeAgentsResponse :: Newtype DescribeAgentsResponse _
derive instance repGenericDescribeAgentsResponse :: Generic DescribeAgentsResponse _
instance showDescribeAgentsResponse :: Show DescribeAgentsResponse where
  show = genericShow
instance decodeDescribeAgentsResponse :: Decode DescribeAgentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeAgentsResponse :: Encode DescribeAgentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeConfigurationsAttribute = DescribeConfigurationsAttribute (StrMap.StrMap String)
derive instance newtypeDescribeConfigurationsAttribute :: Newtype DescribeConfigurationsAttribute _
derive instance repGenericDescribeConfigurationsAttribute :: Generic DescribeConfigurationsAttribute _
instance showDescribeConfigurationsAttribute :: Show DescribeConfigurationsAttribute where
  show = genericShow
instance decodeDescribeConfigurationsAttribute :: Decode DescribeConfigurationsAttribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationsAttribute :: Encode DescribeConfigurationsAttribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeConfigurationsAttributes = DescribeConfigurationsAttributes (Array DescribeConfigurationsAttribute)
derive instance newtypeDescribeConfigurationsAttributes :: Newtype DescribeConfigurationsAttributes _
derive instance repGenericDescribeConfigurationsAttributes :: Generic DescribeConfigurationsAttributes _
instance showDescribeConfigurationsAttributes :: Show DescribeConfigurationsAttributes where
  show = genericShow
instance decodeDescribeConfigurationsAttributes :: Decode DescribeConfigurationsAttributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationsAttributes :: Encode DescribeConfigurationsAttributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeConfigurationsRequest = DescribeConfigurationsRequest 
  { "ConfigurationIds'" :: (ConfigurationIdList)
  }
derive instance newtypeDescribeConfigurationsRequest :: Newtype DescribeConfigurationsRequest _
derive instance repGenericDescribeConfigurationsRequest :: Generic DescribeConfigurationsRequest _
instance showDescribeConfigurationsRequest :: Show DescribeConfigurationsRequest where
  show = genericShow
instance decodeDescribeConfigurationsRequest :: Decode DescribeConfigurationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationsRequest :: Encode DescribeConfigurationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeConfigurationsResponse = DescribeConfigurationsResponse 
  { "Configurations'" :: NullOrUndefined.NullOrUndefined (DescribeConfigurationsAttributes)
  }
derive instance newtypeDescribeConfigurationsResponse :: Newtype DescribeConfigurationsResponse _
derive instance repGenericDescribeConfigurationsResponse :: Generic DescribeConfigurationsResponse _
instance showDescribeConfigurationsResponse :: Show DescribeConfigurationsResponse where
  show = genericShow
instance decodeDescribeConfigurationsResponse :: Decode DescribeConfigurationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConfigurationsResponse :: Encode DescribeConfigurationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeExportConfigurationsRequest = DescribeExportConfigurationsRequest 
  { "ExportIds'" :: NullOrUndefined.NullOrUndefined (ExportIds)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportConfigurationsRequest :: Newtype DescribeExportConfigurationsRequest _
derive instance repGenericDescribeExportConfigurationsRequest :: Generic DescribeExportConfigurationsRequest _
instance showDescribeExportConfigurationsRequest :: Show DescribeExportConfigurationsRequest where
  show = genericShow
instance decodeDescribeExportConfigurationsRequest :: Decode DescribeExportConfigurationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeExportConfigurationsRequest :: Encode DescribeExportConfigurationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeExportConfigurationsResponse = DescribeExportConfigurationsResponse 
  { "ExportsInfo'" :: NullOrUndefined.NullOrUndefined (ExportsInfo)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportConfigurationsResponse :: Newtype DescribeExportConfigurationsResponse _
derive instance repGenericDescribeExportConfigurationsResponse :: Generic DescribeExportConfigurationsResponse _
instance showDescribeExportConfigurationsResponse :: Show DescribeExportConfigurationsResponse where
  show = genericShow
instance decodeDescribeExportConfigurationsResponse :: Decode DescribeExportConfigurationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeExportConfigurationsResponse :: Encode DescribeExportConfigurationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeExportTasksRequest = DescribeExportTasksRequest 
  { "ExportIds'" :: NullOrUndefined.NullOrUndefined (ExportIds)
  , "Filters'" :: NullOrUndefined.NullOrUndefined (ExportFilters)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportTasksRequest :: Newtype DescribeExportTasksRequest _
derive instance repGenericDescribeExportTasksRequest :: Generic DescribeExportTasksRequest _
instance showDescribeExportTasksRequest :: Show DescribeExportTasksRequest where
  show = genericShow
instance decodeDescribeExportTasksRequest :: Decode DescribeExportTasksRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeExportTasksRequest :: Encode DescribeExportTasksRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeExportTasksResponse = DescribeExportTasksResponse 
  { "ExportsInfo'" :: NullOrUndefined.NullOrUndefined (ExportsInfo)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeExportTasksResponse :: Newtype DescribeExportTasksResponse _
derive instance repGenericDescribeExportTasksResponse :: Generic DescribeExportTasksResponse _
instance showDescribeExportTasksResponse :: Show DescribeExportTasksResponse where
  show = genericShow
instance decodeDescribeExportTasksResponse :: Decode DescribeExportTasksResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeExportTasksResponse :: Encode DescribeExportTasksResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTagsRequest = DescribeTagsRequest 
  { "Filters'" :: NullOrUndefined.NullOrUndefined (TagFilters)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeTagsRequest :: Newtype DescribeTagsRequest _
derive instance repGenericDescribeTagsRequest :: Generic DescribeTagsRequest _
instance showDescribeTagsRequest :: Show DescribeTagsRequest where
  show = genericShow
instance decodeDescribeTagsRequest :: Decode DescribeTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsRequest :: Encode DescribeTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTagsResponse = DescribeTagsResponse 
  { "Tags'" :: NullOrUndefined.NullOrUndefined (ConfigurationTagSet)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeTagsResponse :: Newtype DescribeTagsResponse _
derive instance repGenericDescribeTagsResponse :: Generic DescribeTagsResponse _
instance showDescribeTagsResponse :: Show DescribeTagsResponse where
  show = genericShow
instance decodeDescribeTagsResponse :: Decode DescribeTagsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsResponse :: Encode DescribeTagsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateConfigurationItemsFromApplicationRequest = DisassociateConfigurationItemsFromApplicationRequest 
  { "ApplicationConfigurationId'" :: (ApplicationId)
  , "ConfigurationIds'" :: (ConfigurationIdList)
  }
derive instance newtypeDisassociateConfigurationItemsFromApplicationRequest :: Newtype DisassociateConfigurationItemsFromApplicationRequest _
derive instance repGenericDisassociateConfigurationItemsFromApplicationRequest :: Generic DisassociateConfigurationItemsFromApplicationRequest _
instance showDisassociateConfigurationItemsFromApplicationRequest :: Show DisassociateConfigurationItemsFromApplicationRequest where
  show = genericShow
instance decodeDisassociateConfigurationItemsFromApplicationRequest :: Decode DisassociateConfigurationItemsFromApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateConfigurationItemsFromApplicationRequest :: Encode DisassociateConfigurationItemsFromApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisassociateConfigurationItemsFromApplicationResponse = DisassociateConfigurationItemsFromApplicationResponse Types.NoArguments
derive instance newtypeDisassociateConfigurationItemsFromApplicationResponse :: Newtype DisassociateConfigurationItemsFromApplicationResponse _
derive instance repGenericDisassociateConfigurationItemsFromApplicationResponse :: Generic DisassociateConfigurationItemsFromApplicationResponse _
instance showDisassociateConfigurationItemsFromApplicationResponse :: Show DisassociateConfigurationItemsFromApplicationResponse where
  show = genericShow
instance decodeDisassociateConfigurationItemsFromApplicationResponse :: Decode DisassociateConfigurationItemsFromApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisassociateConfigurationItemsFromApplicationResponse :: Encode DisassociateConfigurationItemsFromApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportConfigurationsResponse = ExportConfigurationsResponse 
  { "ExportId'" :: NullOrUndefined.NullOrUndefined (ConfigurationsExportId)
  }
derive instance newtypeExportConfigurationsResponse :: Newtype ExportConfigurationsResponse _
derive instance repGenericExportConfigurationsResponse :: Generic ExportConfigurationsResponse _
instance showExportConfigurationsResponse :: Show ExportConfigurationsResponse where
  show = genericShow
instance decodeExportConfigurationsResponse :: Decode ExportConfigurationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportConfigurationsResponse :: Encode ExportConfigurationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportDataFormat = ExportDataFormat String
derive instance newtypeExportDataFormat :: Newtype ExportDataFormat _
derive instance repGenericExportDataFormat :: Generic ExportDataFormat _
instance showExportDataFormat :: Show ExportDataFormat where
  show = genericShow
instance decodeExportDataFormat :: Decode ExportDataFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportDataFormat :: Encode ExportDataFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportDataFormats = ExportDataFormats (Array ExportDataFormat)
derive instance newtypeExportDataFormats :: Newtype ExportDataFormats _
derive instance repGenericExportDataFormats :: Generic ExportDataFormats _
instance showExportDataFormats :: Show ExportDataFormats where
  show = genericShow
instance decodeExportDataFormats :: Decode ExportDataFormats where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportDataFormats :: Encode ExportDataFormats where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Used to select which agent's data is to be exported. A single agent ID may be selected for export using the <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/API_StartExportTask.html">StartExportTask</a> action.</p>
newtype ExportFilter = ExportFilter 
  { "Name'" :: (FilterName)
  , "Values'" :: (FilterValues)
  , "Condition'" :: (Condition)
  }
derive instance newtypeExportFilter :: Newtype ExportFilter _
derive instance repGenericExportFilter :: Generic ExportFilter _
instance showExportFilter :: Show ExportFilter where
  show = genericShow
instance decodeExportFilter :: Decode ExportFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportFilter :: Encode ExportFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportFilters = ExportFilters (Array ExportFilter)
derive instance newtypeExportFilters :: Newtype ExportFilters _
derive instance repGenericExportFilters :: Generic ExportFilters _
instance showExportFilters :: Show ExportFilters where
  show = genericShow
instance decodeExportFilters :: Decode ExportFilters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportFilters :: Encode ExportFilters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportIds = ExportIds (Array ConfigurationsExportId)
derive instance newtypeExportIds :: Newtype ExportIds _
derive instance repGenericExportIds :: Generic ExportIds _
instance showExportIds :: Show ExportIds where
  show = genericShow
instance decodeExportIds :: Decode ExportIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportIds :: Encode ExportIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information regarding the export status of discovered data. The value is an array of objects.</p>
newtype ExportInfo = ExportInfo 
  { "ExportId'" :: (ConfigurationsExportId)
  , "ExportStatus'" :: (ExportStatus)
  , "StatusMessage'" :: (ExportStatusMessage)
  , "ConfigurationsDownloadUrl'" :: NullOrUndefined.NullOrUndefined (ConfigurationsDownloadUrl)
  , "ExportRequestTime'" :: (ExportRequestTime)
  , "IsTruncated'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "RequestedStartTime'" :: NullOrUndefined.NullOrUndefined (TimeStamp)
  , "RequestedEndTime'" :: NullOrUndefined.NullOrUndefined (TimeStamp)
  }
derive instance newtypeExportInfo :: Newtype ExportInfo _
derive instance repGenericExportInfo :: Generic ExportInfo _
instance showExportInfo :: Show ExportInfo where
  show = genericShow
instance decodeExportInfo :: Decode ExportInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportInfo :: Encode ExportInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportRequestTime = ExportRequestTime Number
derive instance newtypeExportRequestTime :: Newtype ExportRequestTime _
derive instance repGenericExportRequestTime :: Generic ExportRequestTime _
instance showExportRequestTime :: Show ExportRequestTime where
  show = genericShow
instance decodeExportRequestTime :: Decode ExportRequestTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportRequestTime :: Encode ExportRequestTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportStatus = ExportStatus String
derive instance newtypeExportStatus :: Newtype ExportStatus _
derive instance repGenericExportStatus :: Generic ExportStatus _
instance showExportStatus :: Show ExportStatus where
  show = genericShow
instance decodeExportStatus :: Decode ExportStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportStatus :: Encode ExportStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportStatusMessage = ExportStatusMessage String
derive instance newtypeExportStatusMessage :: Newtype ExportStatusMessage _
derive instance repGenericExportStatusMessage :: Generic ExportStatusMessage _
instance showExportStatusMessage :: Show ExportStatusMessage where
  show = genericShow
instance decodeExportStatusMessage :: Decode ExportStatusMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportStatusMessage :: Encode ExportStatusMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExportsInfo = ExportsInfo (Array ExportInfo)
derive instance newtypeExportsInfo :: Newtype ExportsInfo _
derive instance repGenericExportsInfo :: Generic ExportsInfo _
instance showExportsInfo :: Show ExportsInfo where
  show = genericShow
instance decodeExportsInfo :: Decode ExportsInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportsInfo :: Encode ExportsInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A filter that can use conditional operators.</p> <p>For more information about filters, see <a href="http://docs.aws.amazon.com/application-discovery/latest/APIReference/discovery-api-queries.html">Querying Discovered Configuration Items</a>. </p>
newtype Filter = Filter 
  { "Name'" :: (String)
  , "Values'" :: (FilterValues)
  , "Condition'" :: (Condition)
  }
derive instance newtypeFilter :: Newtype Filter _
derive instance repGenericFilter :: Generic Filter _
instance showFilter :: Show Filter where
  show = genericShow
instance decodeFilter :: Decode Filter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilter :: Encode Filter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterName = FilterName String
derive instance newtypeFilterName :: Newtype FilterName _
derive instance repGenericFilterName :: Generic FilterName _
instance showFilterName :: Show FilterName where
  show = genericShow
instance decodeFilterName :: Decode FilterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterName :: Encode FilterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterValue = FilterValue String
derive instance newtypeFilterValue :: Newtype FilterValue _
derive instance repGenericFilterValue :: Generic FilterValue _
instance showFilterValue :: Show FilterValue where
  show = genericShow
instance decodeFilterValue :: Decode FilterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterValue :: Encode FilterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterValues = FilterValues (Array FilterValue)
derive instance newtypeFilterValues :: Newtype FilterValues _
derive instance repGenericFilterValues :: Generic FilterValues _
instance showFilterValues :: Show FilterValues where
  show = genericShow
instance decodeFilterValues :: Decode FilterValues where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterValues :: Encode FilterValues where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Filters = Filters (Array Filter)
derive instance newtypeFilters :: Newtype Filters _
derive instance repGenericFilters :: Generic Filters _
instance showFilters :: Show Filters where
  show = genericShow
instance decodeFilters :: Decode Filters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilters :: Encode Filters where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiscoverySummaryRequest = GetDiscoverySummaryRequest Types.NoArguments
derive instance newtypeGetDiscoverySummaryRequest :: Newtype GetDiscoverySummaryRequest _
derive instance repGenericGetDiscoverySummaryRequest :: Generic GetDiscoverySummaryRequest _
instance showGetDiscoverySummaryRequest :: Show GetDiscoverySummaryRequest where
  show = genericShow
instance decodeGetDiscoverySummaryRequest :: Decode GetDiscoverySummaryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiscoverySummaryRequest :: Encode GetDiscoverySummaryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDiscoverySummaryResponse = GetDiscoverySummaryResponse 
  { "Servers'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Applications'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ServersMappedToApplications'" :: NullOrUndefined.NullOrUndefined (Number)
  , "ServersMappedtoTags'" :: NullOrUndefined.NullOrUndefined (Number)
  , "AgentSummary'" :: NullOrUndefined.NullOrUndefined (CustomerAgentInfo)
  , "ConnectorSummary'" :: NullOrUndefined.NullOrUndefined (CustomerConnectorInfo)
  }
derive instance newtypeGetDiscoverySummaryResponse :: Newtype GetDiscoverySummaryResponse _
derive instance repGenericGetDiscoverySummaryResponse :: Generic GetDiscoverySummaryResponse _
instance showGetDiscoverySummaryResponse :: Show GetDiscoverySummaryResponse where
  show = genericShow
instance decodeGetDiscoverySummaryResponse :: Decode GetDiscoverySummaryResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDiscoverySummaryResponse :: Encode GetDiscoverySummaryResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more parameters are not valid. Verify the parameters and try again.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The value of one or more parameters are either invalid or out of range. Verify the parameter values and try again.</p>
newtype InvalidParameterValueException = InvalidParameterValueException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeInvalidParameterValueException :: Newtype InvalidParameterValueException _
derive instance repGenericInvalidParameterValueException :: Generic InvalidParameterValueException _
instance showInvalidParameterValueException :: Show InvalidParameterValueException where
  show = genericShow
instance decodeInvalidParameterValueException :: Decode InvalidParameterValueException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterValueException :: Encode InvalidParameterValueException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListConfigurationsRequest = ListConfigurationsRequest 
  { "ConfigurationType'" :: (ConfigurationItemType)
  , "Filters'" :: NullOrUndefined.NullOrUndefined (Filters)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "OrderBy'" :: NullOrUndefined.NullOrUndefined (OrderByList)
  }
derive instance newtypeListConfigurationsRequest :: Newtype ListConfigurationsRequest _
derive instance repGenericListConfigurationsRequest :: Generic ListConfigurationsRequest _
instance showListConfigurationsRequest :: Show ListConfigurationsRequest where
  show = genericShow
instance decodeListConfigurationsRequest :: Decode ListConfigurationsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListConfigurationsRequest :: Encode ListConfigurationsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListConfigurationsResponse = ListConfigurationsResponse 
  { "Configurations'" :: NullOrUndefined.NullOrUndefined (Configurations)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListConfigurationsResponse :: Newtype ListConfigurationsResponse _
derive instance repGenericListConfigurationsResponse :: Generic ListConfigurationsResponse _
instance showListConfigurationsResponse :: Show ListConfigurationsResponse where
  show = genericShow
instance decodeListConfigurationsResponse :: Decode ListConfigurationsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListConfigurationsResponse :: Encode ListConfigurationsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListServerNeighborsRequest = ListServerNeighborsRequest 
  { "ConfigurationId'" :: (ConfigurationId)
  , "PortInformationNeeded'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "NeighborConfigurationIds'" :: NullOrUndefined.NullOrUndefined (ConfigurationIdList)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (Int)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListServerNeighborsRequest :: Newtype ListServerNeighborsRequest _
derive instance repGenericListServerNeighborsRequest :: Generic ListServerNeighborsRequest _
instance showListServerNeighborsRequest :: Show ListServerNeighborsRequest where
  show = genericShow
instance decodeListServerNeighborsRequest :: Decode ListServerNeighborsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListServerNeighborsRequest :: Encode ListServerNeighborsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListServerNeighborsResponse = ListServerNeighborsResponse 
  { "Neighbors'" :: (NeighborDetailsList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "KnownDependencyCount'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeListServerNeighborsResponse :: Newtype ListServerNeighborsResponse _
derive instance repGenericListServerNeighborsResponse :: Generic ListServerNeighborsResponse _
instance showListServerNeighborsResponse :: Show ListServerNeighborsResponse where
  show = genericShow
instance decodeListServerNeighborsResponse :: Decode ListServerNeighborsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListServerNeighborsResponse :: Encode ListServerNeighborsResponse where
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


-- | <p>Details about neighboring servers.</p>
newtype NeighborConnectionDetail = NeighborConnectionDetail 
  { "SourceServerId'" :: (ConfigurationId)
  , "DestinationServerId'" :: (ConfigurationId)
  , "DestinationPort'" :: NullOrUndefined.NullOrUndefined (BoxedInteger)
  , "TransportProtocol'" :: NullOrUndefined.NullOrUndefined (String)
  , "ConnectionsCount'" :: (Number)
  }
derive instance newtypeNeighborConnectionDetail :: Newtype NeighborConnectionDetail _
derive instance repGenericNeighborConnectionDetail :: Generic NeighborConnectionDetail _
instance showNeighborConnectionDetail :: Show NeighborConnectionDetail where
  show = genericShow
instance decodeNeighborConnectionDetail :: Decode NeighborConnectionDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNeighborConnectionDetail :: Encode NeighborConnectionDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NeighborDetailsList = NeighborDetailsList (Array NeighborConnectionDetail)
derive instance newtypeNeighborDetailsList :: Newtype NeighborDetailsList _
derive instance repGenericNeighborDetailsList :: Generic NeighborDetailsList _
instance showNeighborDetailsList :: Show NeighborDetailsList where
  show = genericShow
instance decodeNeighborDetailsList :: Decode NeighborDetailsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNeighborDetailsList :: Encode NeighborDetailsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This operation is not permitted.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeOperationNotPermittedException :: Newtype OperationNotPermittedException _
derive instance repGenericOperationNotPermittedException :: Generic OperationNotPermittedException _
instance showOperationNotPermittedException :: Show OperationNotPermittedException where
  show = genericShow
instance decodeOperationNotPermittedException :: Decode OperationNotPermittedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationNotPermittedException :: Encode OperationNotPermittedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A field and direction for ordered output.</p>
newtype OrderByElement = OrderByElement 
  { "FieldName'" :: (String)
  , "SortOrder'" :: NullOrUndefined.NullOrUndefined (OrderString')
  }
derive instance newtypeOrderByElement :: Newtype OrderByElement _
derive instance repGenericOrderByElement :: Generic OrderByElement _
instance showOrderByElement :: Show OrderByElement where
  show = genericShow
instance decodeOrderByElement :: Decode OrderByElement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderByElement :: Encode OrderByElement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderByList = OrderByList (Array OrderByElement)
derive instance newtypeOrderByList :: Newtype OrderByList _
derive instance repGenericOrderByList :: Generic OrderByList _
instance showOrderByList :: Show OrderByList where
  show = genericShow
instance decodeOrderByList :: Decode OrderByList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderByList :: Encode OrderByList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified configuration ID was not located. Verify the configuration ID and try again.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The server experienced an internal error. Try again.</p>
newtype ServerInternalErrorException = ServerInternalErrorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeServerInternalErrorException :: Newtype ServerInternalErrorException _
derive instance repGenericServerInternalErrorException :: Generic ServerInternalErrorException _
instance showServerInternalErrorException :: Show ServerInternalErrorException where
  show = genericShow
instance decodeServerInternalErrorException :: Decode ServerInternalErrorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServerInternalErrorException :: Encode ServerInternalErrorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartDataCollectionByAgentIdsRequest = StartDataCollectionByAgentIdsRequest 
  { "AgentIds'" :: (AgentIds)
  }
derive instance newtypeStartDataCollectionByAgentIdsRequest :: Newtype StartDataCollectionByAgentIdsRequest _
derive instance repGenericStartDataCollectionByAgentIdsRequest :: Generic StartDataCollectionByAgentIdsRequest _
instance showStartDataCollectionByAgentIdsRequest :: Show StartDataCollectionByAgentIdsRequest where
  show = genericShow
instance decodeStartDataCollectionByAgentIdsRequest :: Decode StartDataCollectionByAgentIdsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartDataCollectionByAgentIdsRequest :: Encode StartDataCollectionByAgentIdsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartDataCollectionByAgentIdsResponse = StartDataCollectionByAgentIdsResponse 
  { "AgentsConfigurationStatus'" :: NullOrUndefined.NullOrUndefined (AgentConfigurationStatusList)
  }
derive instance newtypeStartDataCollectionByAgentIdsResponse :: Newtype StartDataCollectionByAgentIdsResponse _
derive instance repGenericStartDataCollectionByAgentIdsResponse :: Generic StartDataCollectionByAgentIdsResponse _
instance showStartDataCollectionByAgentIdsResponse :: Show StartDataCollectionByAgentIdsResponse where
  show = genericShow
instance decodeStartDataCollectionByAgentIdsResponse :: Decode StartDataCollectionByAgentIdsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartDataCollectionByAgentIdsResponse :: Encode StartDataCollectionByAgentIdsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartExportTaskRequest = StartExportTaskRequest 
  { "ExportDataFormat'" :: NullOrUndefined.NullOrUndefined (ExportDataFormats)
  , "Filters'" :: NullOrUndefined.NullOrUndefined (ExportFilters)
  , "StartTime'" :: NullOrUndefined.NullOrUndefined (TimeStamp)
  , "EndTime'" :: NullOrUndefined.NullOrUndefined (TimeStamp)
  }
derive instance newtypeStartExportTaskRequest :: Newtype StartExportTaskRequest _
derive instance repGenericStartExportTaskRequest :: Generic StartExportTaskRequest _
instance showStartExportTaskRequest :: Show StartExportTaskRequest where
  show = genericShow
instance decodeStartExportTaskRequest :: Decode StartExportTaskRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartExportTaskRequest :: Encode StartExportTaskRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartExportTaskResponse = StartExportTaskResponse 
  { "ExportId'" :: NullOrUndefined.NullOrUndefined (ConfigurationsExportId)
  }
derive instance newtypeStartExportTaskResponse :: Newtype StartExportTaskResponse _
derive instance repGenericStartExportTaskResponse :: Generic StartExportTaskResponse _
instance showStartExportTaskResponse :: Show StartExportTaskResponse where
  show = genericShow
instance decodeStartExportTaskResponse :: Decode StartExportTaskResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartExportTaskResponse :: Encode StartExportTaskResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopDataCollectionByAgentIdsRequest = StopDataCollectionByAgentIdsRequest 
  { "AgentIds'" :: (AgentIds)
  }
derive instance newtypeStopDataCollectionByAgentIdsRequest :: Newtype StopDataCollectionByAgentIdsRequest _
derive instance repGenericStopDataCollectionByAgentIdsRequest :: Generic StopDataCollectionByAgentIdsRequest _
instance showStopDataCollectionByAgentIdsRequest :: Show StopDataCollectionByAgentIdsRequest where
  show = genericShow
instance decodeStopDataCollectionByAgentIdsRequest :: Decode StopDataCollectionByAgentIdsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopDataCollectionByAgentIdsRequest :: Encode StopDataCollectionByAgentIdsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopDataCollectionByAgentIdsResponse = StopDataCollectionByAgentIdsResponse 
  { "AgentsConfigurationStatus'" :: NullOrUndefined.NullOrUndefined (AgentConfigurationStatusList)
  }
derive instance newtypeStopDataCollectionByAgentIdsResponse :: Newtype StopDataCollectionByAgentIdsResponse _
derive instance repGenericStopDataCollectionByAgentIdsResponse :: Generic StopDataCollectionByAgentIdsResponse _
instance showStopDataCollectionByAgentIdsResponse :: Show StopDataCollectionByAgentIdsResponse where
  show = genericShow
instance decodeStopDataCollectionByAgentIdsResponse :: Decode StopDataCollectionByAgentIdsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopDataCollectionByAgentIdsResponse :: Encode StopDataCollectionByAgentIdsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Metadata that help you categorize IT assets.</p>
newtype Tag = Tag 
  { "Key'" :: (TagKey)
  , "Value'" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The tag filter. Valid names are: <code>tagKey</code>, <code>tagValue</code>, <code>configurationId</code>.</p>
newtype TagFilter = TagFilter 
  { "Name'" :: (FilterName)
  , "Values'" :: (FilterValues)
  }
derive instance newtypeTagFilter :: Newtype TagFilter _
derive instance repGenericTagFilter :: Generic TagFilter _
instance showTagFilter :: Show TagFilter where
  show = genericShow
instance decodeTagFilter :: Decode TagFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagFilter :: Encode TagFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagFilters = TagFilters (Array TagFilter)
derive instance newtypeTagFilters :: Newtype TagFilters _
derive instance repGenericTagFilters :: Generic TagFilters _
instance showTagFilters :: Show TagFilters where
  show = genericShow
instance decodeTagFilters :: Decode TagFilters where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagFilters :: Encode TagFilters where
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


newtype TagSet = TagSet (Array Tag)
derive instance newtypeTagSet :: Newtype TagSet _
derive instance repGenericTagSet :: Generic TagSet _
instance showTagSet :: Show TagSet where
  show = genericShow
instance decodeTagSet :: Decode TagSet where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagSet :: Encode TagSet where
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


newtype TimeStamp = TimeStamp Number
derive instance newtypeTimeStamp :: Newtype TimeStamp _
derive instance repGenericTimeStamp :: Generic TimeStamp _
instance showTimeStamp :: Show TimeStamp where
  show = genericShow
instance decodeTimeStamp :: Decode TimeStamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeStamp :: Encode TimeStamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationRequest = UpdateApplicationRequest 
  { "ConfigurationId'" :: (ApplicationId)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeUpdateApplicationRequest :: Newtype UpdateApplicationRequest _
derive instance repGenericUpdateApplicationRequest :: Generic UpdateApplicationRequest _
instance showUpdateApplicationRequest :: Show UpdateApplicationRequest where
  show = genericShow
instance decodeUpdateApplicationRequest :: Decode UpdateApplicationRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationRequest :: Encode UpdateApplicationRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateApplicationResponse = UpdateApplicationResponse Types.NoArguments
derive instance newtypeUpdateApplicationResponse :: Newtype UpdateApplicationResponse _
derive instance repGenericUpdateApplicationResponse :: Generic UpdateApplicationResponse _
instance showUpdateApplicationResponse :: Show UpdateApplicationResponse where
  show = genericShow
instance decodeUpdateApplicationResponse :: Decode UpdateApplicationResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateApplicationResponse :: Encode UpdateApplicationResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderString' = OrderString' String
derive instance newtypeOrderString' :: Newtype OrderString' _
derive instance repGenericOrderString' :: Generic OrderString' _
instance showOrderString' :: Show OrderString' where
  show = genericShow
instance decodeOrderString' :: Decode OrderString' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderString' :: Encode OrderString' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
