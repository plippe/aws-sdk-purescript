

-- | Amazon Server Migration Service automates the process of migrating servers to EC2.
module AWS.SMS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SMS" :: String


-- | The CreateReplicationJob API is used to create a ReplicationJob to replicate a server on AWS. Call this API to first create a ReplicationJob, which will then schedule periodic ReplicationRuns to replicate your server to AWS. Each ReplicationRun will result in the creation of an AWS AMI.
createReplicationJob :: forall eff. CreateReplicationJobRequest -> Aff (err :: AWS.RequestError | eff) CreateReplicationJobResponse
createReplicationJob = AWS.request serviceName "createReplicationJob" 


-- | The DeleteReplicationJob API is used to delete a ReplicationJob, resulting in no further ReplicationRuns. This will delete the contents of the S3 bucket used to store SMS artifacts, but will not delete any AMIs created by the SMS service.
deleteReplicationJob :: forall eff. DeleteReplicationJobRequest -> Aff (err :: AWS.RequestError | eff) DeleteReplicationJobResponse
deleteReplicationJob = AWS.request serviceName "deleteReplicationJob" 


-- | The DeleteServerCatalog API clears all servers from your server catalog. This means that these servers will no longer be accessible to the Server Migration Service.
deleteServerCatalog :: forall eff. DeleteServerCatalogRequest -> Aff (err :: AWS.RequestError | eff) DeleteServerCatalogResponse
deleteServerCatalog = AWS.request serviceName "deleteServerCatalog" 


-- | The DisassociateConnector API will disassociate a connector from the Server Migration Service, rendering it unavailable to support replication jobs.
disassociateConnector :: forall eff. DisassociateConnectorRequest -> Aff (err :: AWS.RequestError | eff) DisassociateConnectorResponse
disassociateConnector = AWS.request serviceName "disassociateConnector" 


-- | The GetConnectors API returns a list of connectors that are registered with the Server Migration Service.
getConnectors :: forall eff. GetConnectorsRequest -> Aff (err :: AWS.RequestError | eff) GetConnectorsResponse
getConnectors = AWS.request serviceName "getConnectors" 


-- | The GetReplicationJobs API will return all of your ReplicationJobs and their details. This API returns a paginated list, that may be consecutively called with nextToken to retrieve all ReplicationJobs.
getReplicationJobs :: forall eff. GetReplicationJobsRequest -> Aff (err :: AWS.RequestError | eff) GetReplicationJobsResponse
getReplicationJobs = AWS.request serviceName "getReplicationJobs" 


-- | The GetReplicationRuns API will return all ReplicationRuns for a given ReplicationJob. This API returns a paginated list, that may be consecutively called with nextToken to retrieve all ReplicationRuns for a ReplicationJob.
getReplicationRuns :: forall eff. GetReplicationRunsRequest -> Aff (err :: AWS.RequestError | eff) GetReplicationRunsResponse
getReplicationRuns = AWS.request serviceName "getReplicationRuns" 


-- | The GetServers API returns a list of all servers in your server catalog. For this call to succeed, you must previously have called ImportServerCatalog.
getServers :: forall eff. GetServersRequest -> Aff (err :: AWS.RequestError | eff) GetServersResponse
getServers = AWS.request serviceName "getServers" 


-- | The ImportServerCatalog API is used to gather the complete list of on-premises servers on your premises. This API call requires connectors to be installed and monitoring all servers you would like imported. This API call returns immediately, but may take some time to retrieve all of the servers.
importServerCatalog :: forall eff. ImportServerCatalogRequest -> Aff (err :: AWS.RequestError | eff) ImportServerCatalogResponse
importServerCatalog = AWS.request serviceName "importServerCatalog" 


-- | The StartOnDemandReplicationRun API is used to start a ReplicationRun on demand (in addition to those that are scheduled based on your frequency). This ReplicationRun will start immediately. StartOnDemandReplicationRun is subject to limits on how many on demand ReplicationRuns you may call per 24-hour period.
startOnDemandReplicationRun :: forall eff. StartOnDemandReplicationRunRequest -> Aff (err :: AWS.RequestError | eff) StartOnDemandReplicationRunResponse
startOnDemandReplicationRun = AWS.request serviceName "startOnDemandReplicationRun" 


-- | The UpdateReplicationJob API is used to change the settings of your existing ReplicationJob created using CreateReplicationJob. Calling this API will affect the next scheduled ReplicationRun.
updateReplicationJob :: forall eff. UpdateReplicationJobRequest -> Aff (err :: AWS.RequestError | eff) UpdateReplicationJobResponse
updateReplicationJob = AWS.request serviceName "updateReplicationJob" 


-- | The AMI id for the image resulting from a Replication Run.
newtype AmiId = AmiId String
derive instance newtypeAmiId :: Newtype AmiId _


-- | Object representing a Connector
newtype Connector = Connector 
  { "ConnectorId'" :: NullOrUndefined (ConnectorId)
  , "Version'" :: NullOrUndefined (ConnectorVersion)
  , "Status'" :: NullOrUndefined (ConnectorStatus)
  , "CapabilityList'" :: NullOrUndefined (ConnectorCapabilityList)
  , "VmManagerName'" :: NullOrUndefined (VmManagerName)
  , "VmManagerType'" :: NullOrUndefined (VmManagerType)
  , "VmManagerId'" :: NullOrUndefined (VmManagerId)
  , "IpAddress'" :: NullOrUndefined (IpAddress)
  , "MacAddress'" :: NullOrUndefined (MacAddress)
  , "AssociatedOn'" :: NullOrUndefined (Number)
  }
derive instance newtypeConnector :: Newtype Connector _


-- | Capabilities for a Connector
newtype ConnectorCapability = ConnectorCapability String
derive instance newtypeConnectorCapability :: Newtype ConnectorCapability _


-- | List of Connector Capabilities
newtype ConnectorCapabilityList = ConnectorCapabilityList (Array ConnectorCapability)
derive instance newtypeConnectorCapabilityList :: Newtype ConnectorCapabilityList _


-- | Unique Identifier for Connector
newtype ConnectorId = ConnectorId String
derive instance newtypeConnectorId :: Newtype ConnectorId _


-- | List of connectors
newtype ConnectorList = ConnectorList (Array Connector)
derive instance newtypeConnectorList :: Newtype ConnectorList _


-- | Status of on-premise Connector
newtype ConnectorStatus = ConnectorStatus String
derive instance newtypeConnectorStatus :: Newtype ConnectorStatus _


-- | Connector version string
newtype ConnectorVersion = ConnectorVersion String
derive instance newtypeConnectorVersion :: Newtype ConnectorVersion _


newtype CreateReplicationJobRequest = CreateReplicationJobRequest 
  { "ServerId'" :: (ServerId)
  , "SeedReplicationTime'" :: (Number)
  , "Frequency'" :: (Frequency)
  , "LicenseType'" :: NullOrUndefined (LicenseType)
  , "RoleName'" :: NullOrUndefined (RoleName)
  , "Description'" :: NullOrUndefined (Description)
  }
derive instance newtypeCreateReplicationJobRequest :: Newtype CreateReplicationJobRequest _


newtype CreateReplicationJobResponse = CreateReplicationJobResponse 
  { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId)
  }
derive instance newtypeCreateReplicationJobResponse :: Newtype CreateReplicationJobResponse _


newtype DeleteReplicationJobRequest = DeleteReplicationJobRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  }
derive instance newtypeDeleteReplicationJobRequest :: Newtype DeleteReplicationJobRequest _


newtype DeleteReplicationJobResponse = DeleteReplicationJobResponse 
  { 
  }
derive instance newtypeDeleteReplicationJobResponse :: Newtype DeleteReplicationJobResponse _


newtype DeleteServerCatalogRequest = DeleteServerCatalogRequest 
  { 
  }
derive instance newtypeDeleteServerCatalogRequest :: Newtype DeleteServerCatalogRequest _


newtype DeleteServerCatalogResponse = DeleteServerCatalogResponse 
  { 
  }
derive instance newtypeDeleteServerCatalogResponse :: Newtype DeleteServerCatalogResponse _


-- | The description for a Replication Job/Run.
newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


newtype DisassociateConnectorRequest = DisassociateConnectorRequest 
  { "ConnectorId'" :: (ConnectorId)
  }
derive instance newtypeDisassociateConnectorRequest :: Newtype DisassociateConnectorRequest _


newtype DisassociateConnectorResponse = DisassociateConnectorResponse 
  { 
  }
derive instance newtypeDisassociateConnectorResponse :: Newtype DisassociateConnectorResponse _


-- | Error Message string
newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | Interval between Replication Runs. This value is specified in hours, and represents the time between consecutive Replication Runs.
newtype Frequency = Frequency Int
derive instance newtypeFrequency :: Newtype Frequency _


newtype GetConnectorsRequest = GetConnectorsRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeGetConnectorsRequest :: Newtype GetConnectorsRequest _


newtype GetConnectorsResponse = GetConnectorsResponse 
  { "ConnectorList'" :: NullOrUndefined (ConnectorList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetConnectorsResponse :: Newtype GetConnectorsResponse _


newtype GetReplicationJobsRequest = GetReplicationJobsRequest 
  { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeGetReplicationJobsRequest :: Newtype GetReplicationJobsRequest _


newtype GetReplicationJobsResponse = GetReplicationJobsResponse 
  { "ReplicationJobList'" :: NullOrUndefined (ReplicationJobList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetReplicationJobsResponse :: Newtype GetReplicationJobsResponse _


newtype GetReplicationRunsRequest = GetReplicationRunsRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeGetReplicationRunsRequest :: Newtype GetReplicationRunsRequest _


newtype GetReplicationRunsResponse = GetReplicationRunsResponse 
  { "ReplicationJob'" :: NullOrUndefined (ReplicationJob)
  , "ReplicationRunList'" :: NullOrUndefined (ReplicationRunList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetReplicationRunsResponse :: Newtype GetReplicationRunsResponse _


newtype GetServersRequest = GetServersRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeGetServersRequest :: Newtype GetServersRequest _


newtype GetServersResponse = GetServersResponse 
  { "LastModifiedOn'" :: NullOrUndefined (Number)
  , "ServerCatalogStatus'" :: NullOrUndefined (ServerCatalogStatus)
  , "ServerList'" :: NullOrUndefined (ServerList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeGetServersResponse :: Newtype GetServersResponse _


newtype ImportServerCatalogRequest = ImportServerCatalogRequest 
  { 
  }
derive instance newtypeImportServerCatalogRequest :: Newtype ImportServerCatalogRequest _


newtype ImportServerCatalogResponse = ImportServerCatalogResponse 
  { 
  }
derive instance newtypeImportServerCatalogResponse :: Newtype ImportServerCatalogResponse _


-- | An internal error has occured.
newtype InternalError = InternalError 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalError :: Newtype InternalError _


-- | A parameter specified in the request is not valid, is unsupported, or cannot be used.
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


-- | Internet Protocol (IP) Address
newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _


-- | The license type to be used for the Amazon Machine Image (AMI) created after a successful ReplicationRun.
newtype LicenseType = LicenseType String
derive instance newtypeLicenseType :: Newtype LicenseType _


-- | Hardware (MAC) address
newtype MacAddress = MacAddress String
derive instance newtypeMacAddress :: Newtype MacAddress _


-- | The maximum number of results to return in one API call. If left empty, this will default to 50.
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | The request is missing a required parameter. Ensure that you have supplied all the required parameters for the request.
newtype MissingRequiredParameterException = MissingRequiredParameterException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMissingRequiredParameterException :: Newtype MissingRequiredParameterException _


-- | Pagination token to pass as input to API call
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | No connectors are available to handle this request. Please associate connector(s) and verify any existing connectors are healthy and can respond to requests.
newtype NoConnectorsAvailableException = NoConnectorsAvailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNoConnectorsAvailableException :: Newtype NoConnectorsAvailableException _


-- | The specified operation is not allowed. This error can occur for a number of reasons; for example, you might be trying to start a Replication Run before seed Replication Run.
newtype OperationNotPermittedException = OperationNotPermittedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeOperationNotPermittedException :: Newtype OperationNotPermittedException _


-- | Object representing a Replication Job
newtype ReplicationJob = ReplicationJob 
  { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId)
  , "ServerId'" :: NullOrUndefined (ServerId)
  , "ServerType'" :: NullOrUndefined (ServerType)
  , "VmServer'" :: NullOrUndefined (VmServer)
  , "SeedReplicationTime'" :: NullOrUndefined (Number)
  , "Frequency'" :: NullOrUndefined (Frequency)
  , "NextReplicationRunStartTime'" :: NullOrUndefined (Number)
  , "LicenseType'" :: NullOrUndefined (LicenseType)
  , "RoleName'" :: NullOrUndefined (RoleName)
  , "LatestAmiId'" :: NullOrUndefined (AmiId)
  , "State'" :: NullOrUndefined (ReplicationJobState)
  , "StatusMessage'" :: NullOrUndefined (ReplicationJobStatusMessage)
  , "Description'" :: NullOrUndefined (Description)
  , "ReplicationRunList'" :: NullOrUndefined (ReplicationRunList)
  }
derive instance newtypeReplicationJob :: Newtype ReplicationJob _


-- | An active Replication Job already exists for the specified server.
newtype ReplicationJobAlreadyExistsException = ReplicationJobAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeReplicationJobAlreadyExistsException :: Newtype ReplicationJobAlreadyExistsException _


-- | The unique identifier for a Replication Job.
newtype ReplicationJobId = ReplicationJobId String
derive instance newtypeReplicationJobId :: Newtype ReplicationJobId _


-- | List of Replication Jobs
newtype ReplicationJobList = ReplicationJobList (Array ReplicationJob)
derive instance newtypeReplicationJobList :: Newtype ReplicationJobList _


-- | The specified Replication Job cannot be found.
newtype ReplicationJobNotFoundException = ReplicationJobNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeReplicationJobNotFoundException :: Newtype ReplicationJobNotFoundException _


-- | Current state of Replication Job
newtype ReplicationJobState = ReplicationJobState String
derive instance newtypeReplicationJobState :: Newtype ReplicationJobState _


-- | String describing current status of Replication Job
newtype ReplicationJobStatusMessage = ReplicationJobStatusMessage String
derive instance newtypeReplicationJobStatusMessage :: Newtype ReplicationJobStatusMessage _


-- | An indicator of the Replication Job being deleted or failed.
newtype ReplicationJobTerminated = ReplicationJobTerminated Boolean
derive instance newtypeReplicationJobTerminated :: Newtype ReplicationJobTerminated _


-- | Object representing a Replication Run
newtype ReplicationRun = ReplicationRun 
  { "ReplicationRunId'" :: NullOrUndefined (ReplicationRunId)
  , "State'" :: NullOrUndefined (ReplicationRunState)
  , "Type'" :: NullOrUndefined (ReplicationRunType)
  , "StatusMessage'" :: NullOrUndefined (ReplicationRunStatusMessage)
  , "AmiId'" :: NullOrUndefined (AmiId)
  , "ScheduledStartTime'" :: NullOrUndefined (Number)
  , "CompletedTime'" :: NullOrUndefined (Number)
  , "Description'" :: NullOrUndefined (Description)
  }
derive instance newtypeReplicationRun :: Newtype ReplicationRun _


-- | The unique identifier for a Replication Run.
newtype ReplicationRunId = ReplicationRunId String
derive instance newtypeReplicationRunId :: Newtype ReplicationRunId _


-- | This user has exceeded the maximum allowed Replication Run limit.
newtype ReplicationRunLimitExceededException = ReplicationRunLimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeReplicationRunLimitExceededException :: Newtype ReplicationRunLimitExceededException _


-- | List of Replication Runs
newtype ReplicationRunList = ReplicationRunList (Array ReplicationRun)
derive instance newtypeReplicationRunList :: Newtype ReplicationRunList _


-- | Current state of Replication Run
newtype ReplicationRunState = ReplicationRunState String
derive instance newtypeReplicationRunState :: Newtype ReplicationRunState _


-- | String describing current status of Replication Run
newtype ReplicationRunStatusMessage = ReplicationRunStatusMessage String
derive instance newtypeReplicationRunStatusMessage :: Newtype ReplicationRunStatusMessage _


-- | Type of Replication Run
newtype ReplicationRunType = ReplicationRunType String
derive instance newtypeReplicationRunType :: Newtype ReplicationRunType _


-- | Name of service role in customer's account to be used by SMS service.
newtype RoleName = RoleName String
derive instance newtypeRoleName :: Newtype RoleName _


-- | Object representing a server
newtype Server = Server 
  { "ServerId'" :: NullOrUndefined (ServerId)
  , "ServerType'" :: NullOrUndefined (ServerType)
  , "VmServer'" :: NullOrUndefined (VmServer)
  , "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId)
  , "ReplicationJobTerminated'" :: NullOrUndefined (ReplicationJobTerminated)
  }
derive instance newtypeServer :: Newtype Server _


-- | The provided server cannot be replicated.
newtype ServerCannotBeReplicatedException = ServerCannotBeReplicatedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServerCannotBeReplicatedException :: Newtype ServerCannotBeReplicatedException _


-- | Status of Server catalog
newtype ServerCatalogStatus = ServerCatalogStatus String
derive instance newtypeServerCatalogStatus :: Newtype ServerCatalogStatus _


-- | Unique Identifier for a server
newtype ServerId = ServerId String
derive instance newtypeServerId :: Newtype ServerId _


-- | List of servers from catalog
newtype ServerList = ServerList (Array Server)
derive instance newtypeServerList :: Newtype ServerList _


-- | Type of server.
newtype ServerType = ServerType String
derive instance newtypeServerType :: Newtype ServerType _


newtype StartOnDemandReplicationRunRequest = StartOnDemandReplicationRunRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "Description'" :: NullOrUndefined (Description)
  }
derive instance newtypeStartOnDemandReplicationRunRequest :: Newtype StartOnDemandReplicationRunRequest _


newtype StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse 
  { "ReplicationRunId'" :: NullOrUndefined (ReplicationRunId)
  }
derive instance newtypeStartOnDemandReplicationRunResponse :: Newtype StartOnDemandReplicationRunResponse _


-- | This user does not have permissions to perform this operation.
newtype UnauthorizedOperationException = UnauthorizedOperationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnauthorizedOperationException :: Newtype UnauthorizedOperationException _


newtype UpdateReplicationJobRequest = UpdateReplicationJobRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "Frequency'" :: NullOrUndefined (Frequency)
  , "NextReplicationRunStartTime'" :: NullOrUndefined (Number)
  , "LicenseType'" :: NullOrUndefined (LicenseType)
  , "RoleName'" :: NullOrUndefined (RoleName)
  , "Description'" :: NullOrUndefined (Description)
  }
derive instance newtypeUpdateReplicationJobRequest :: Newtype UpdateReplicationJobRequest _


newtype UpdateReplicationJobResponse = UpdateReplicationJobResponse 
  { 
  }
derive instance newtypeUpdateReplicationJobResponse :: Newtype UpdateReplicationJobResponse _


-- | Unique Identifier for a VM
newtype VmId = VmId String
derive instance newtypeVmId :: Newtype VmId _


-- | Unique Identifier for VM Manager
newtype VmManagerId = VmManagerId String
derive instance newtypeVmManagerId :: Newtype VmManagerId _


-- | VM Manager Name
newtype VmManagerName = VmManagerName String
derive instance newtypeVmManagerName :: Newtype VmManagerName _


-- | VM Management Product
newtype VmManagerType = VmManagerType String
derive instance newtypeVmManagerType :: Newtype VmManagerType _


-- | Name of Virtual Machine
newtype VmName = VmName String
derive instance newtypeVmName :: Newtype VmName _


-- | Path to VM
newtype VmPath = VmPath String
derive instance newtypeVmPath :: Newtype VmPath _


-- | Object representing a VM server
newtype VmServer = VmServer 
  { "VmServerAddress'" :: NullOrUndefined (VmServerAddress)
  , "VmName'" :: NullOrUndefined (VmName)
  , "VmManagerName'" :: NullOrUndefined (VmManagerName)
  , "VmManagerType'" :: NullOrUndefined (VmManagerType)
  , "VmPath'" :: NullOrUndefined (VmPath)
  }
derive instance newtypeVmServer :: Newtype VmServer _


-- | Object representing a server's location
newtype VmServerAddress = VmServerAddress 
  { "VmManagerId'" :: NullOrUndefined (VmManagerId)
  , "VmId'" :: NullOrUndefined (VmId)
  }
derive instance newtypeVmServerAddress :: Newtype VmServerAddress _
