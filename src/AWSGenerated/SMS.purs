

-- | Amazon Server Migration Service automates the process of migrating servers to EC2.
module AWS.SMS where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "SMS" :: String


-- | The CreateReplicationJob API is used to create a ReplicationJob to replicate a server on AWS. Call this API to first create a ReplicationJob, which will then schedule periodic ReplicationRuns to replicate your server to AWS. Each ReplicationRun will result in the creation of an AWS AMI.
createReplicationJob :: forall eff. CreateReplicationJobRequest -> Aff (err :: AWS.RequestError | eff) CreateReplicationJobResponse
createReplicationJob = AWS.request serviceName "CreateReplicationJob" 


-- | The DeleteReplicationJob API is used to delete a ReplicationJob, resulting in no further ReplicationRuns. This will delete the contents of the S3 bucket used to store SMS artifacts, but will not delete any AMIs created by the SMS service.
deleteReplicationJob :: forall eff. DeleteReplicationJobRequest -> Aff (err :: AWS.RequestError | eff) DeleteReplicationJobResponse
deleteReplicationJob = AWS.request serviceName "DeleteReplicationJob" 


-- | The DeleteServerCatalog API clears all servers from your server catalog. This means that these servers will no longer be accessible to the Server Migration Service.
deleteServerCatalog :: forall eff. DeleteServerCatalogRequest -> Aff (err :: AWS.RequestError | eff) DeleteServerCatalogResponse
deleteServerCatalog = AWS.request serviceName "DeleteServerCatalog" 


-- | The DisassociateConnector API will disassociate a connector from the Server Migration Service, rendering it unavailable to support replication jobs.
disassociateConnector :: forall eff. DisassociateConnectorRequest -> Aff (err :: AWS.RequestError | eff) DisassociateConnectorResponse
disassociateConnector = AWS.request serviceName "DisassociateConnector" 


-- | The GetConnectors API returns a list of connectors that are registered with the Server Migration Service.
getConnectors :: forall eff. GetConnectorsRequest -> Aff (err :: AWS.RequestError | eff) GetConnectorsResponse
getConnectors = AWS.request serviceName "GetConnectors" 


-- | The GetReplicationJobs API will return all of your ReplicationJobs and their details. This API returns a paginated list, that may be consecutively called with nextToken to retrieve all ReplicationJobs.
getReplicationJobs :: forall eff. GetReplicationJobsRequest -> Aff (err :: AWS.RequestError | eff) GetReplicationJobsResponse
getReplicationJobs = AWS.request serviceName "GetReplicationJobs" 


-- | The GetReplicationRuns API will return all ReplicationRuns for a given ReplicationJob. This API returns a paginated list, that may be consecutively called with nextToken to retrieve all ReplicationRuns for a ReplicationJob.
getReplicationRuns :: forall eff. GetReplicationRunsRequest -> Aff (err :: AWS.RequestError | eff) GetReplicationRunsResponse
getReplicationRuns = AWS.request serviceName "GetReplicationRuns" 


-- | The GetServers API returns a list of all servers in your server catalog. For this call to succeed, you must previously have called ImportServerCatalog.
getServers :: forall eff. GetServersRequest -> Aff (err :: AWS.RequestError | eff) GetServersResponse
getServers = AWS.request serviceName "GetServers" 


-- | The ImportServerCatalog API is used to gather the complete list of on-premises servers on your premises. This API call requires connectors to be installed and monitoring all servers you would like imported. This API call returns immediately, but may take some time to retrieve all of the servers.
importServerCatalog :: forall eff. ImportServerCatalogRequest -> Aff (err :: AWS.RequestError | eff) ImportServerCatalogResponse
importServerCatalog = AWS.request serviceName "ImportServerCatalog" 


-- | The StartOnDemandReplicationRun API is used to start a ReplicationRun on demand (in addition to those that are scheduled based on your frequency). This ReplicationRun will start immediately. StartOnDemandReplicationRun is subject to limits on how many on demand ReplicationRuns you may call per 24-hour period.
startOnDemandReplicationRun :: forall eff. StartOnDemandReplicationRunRequest -> Aff (err :: AWS.RequestError | eff) StartOnDemandReplicationRunResponse
startOnDemandReplicationRun = AWS.request serviceName "StartOnDemandReplicationRun" 


-- | The UpdateReplicationJob API is used to change the settings of your existing ReplicationJob created using CreateReplicationJob. Calling this API will affect the next scheduled ReplicationRun.
updateReplicationJob :: forall eff. UpdateReplicationJobRequest -> Aff (err :: AWS.RequestError | eff) UpdateReplicationJobResponse
updateReplicationJob = AWS.request serviceName "UpdateReplicationJob" 


-- | The AMI id for the image resulting from a Replication Run.
newtype AmiId = AmiId String


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


-- | Capabilities for a Connector
newtype ConnectorCapability = ConnectorCapability String


-- | List of Connector Capabilities
newtype ConnectorCapabilityList = ConnectorCapabilityList (Array ConnectorCapability)


-- | Unique Identifier for Connector
newtype ConnectorId = ConnectorId String


-- | List of connectors
newtype ConnectorList = ConnectorList (Array Connector)


-- | Status of on-premise Connector
newtype ConnectorStatus = ConnectorStatus String


-- | Connector version string
newtype ConnectorVersion = ConnectorVersion String


newtype CreateReplicationJobRequest = CreateReplicationJobRequest 
  { "ServerId'" :: (ServerId)
  , "SeedReplicationTime'" :: (Number)
  , "Frequency'" :: (Frequency)
  , "LicenseType'" :: NullOrUndefined (LicenseType)
  , "RoleName'" :: NullOrUndefined (RoleName)
  , "Description'" :: NullOrUndefined (Description)
  }


newtype CreateReplicationJobResponse = CreateReplicationJobResponse 
  { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId)
  }


newtype DeleteReplicationJobRequest = DeleteReplicationJobRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  }


newtype DeleteReplicationJobResponse = DeleteReplicationJobResponse 
  { 
  }


newtype DeleteServerCatalogRequest = DeleteServerCatalogRequest 
  { 
  }


newtype DeleteServerCatalogResponse = DeleteServerCatalogResponse 
  { 
  }


-- | The description for a Replication Job/Run.
newtype Description = Description String


newtype DisassociateConnectorRequest = DisassociateConnectorRequest 
  { "ConnectorId'" :: (ConnectorId)
  }


newtype DisassociateConnectorResponse = DisassociateConnectorResponse 
  { 
  }


-- | Error Message string
newtype ErrorMessage = ErrorMessage String


-- | Interval between Replication Runs. This value is specified in hours, and represents the time between consecutive Replication Runs.
newtype Frequency = Frequency Int


newtype GetConnectorsRequest = GetConnectorsRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype GetConnectorsResponse = GetConnectorsResponse 
  { "ConnectorList'" :: NullOrUndefined (ConnectorList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype GetReplicationJobsRequest = GetReplicationJobsRequest 
  { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype GetReplicationJobsResponse = GetReplicationJobsResponse 
  { "ReplicationJobList'" :: NullOrUndefined (ReplicationJobList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype GetReplicationRunsRequest = GetReplicationRunsRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype GetReplicationRunsResponse = GetReplicationRunsResponse 
  { "ReplicationJob'" :: NullOrUndefined (ReplicationJob)
  , "ReplicationRunList'" :: NullOrUndefined (ReplicationRunList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype GetServersRequest = GetServersRequest 
  { "NextToken'" :: NullOrUndefined (NextToken)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  }


newtype GetServersResponse = GetServersResponse 
  { "LastModifiedOn'" :: NullOrUndefined (Number)
  , "ServerCatalogStatus'" :: NullOrUndefined (ServerCatalogStatus)
  , "ServerList'" :: NullOrUndefined (ServerList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype ImportServerCatalogRequest = ImportServerCatalogRequest 
  { 
  }


newtype ImportServerCatalogResponse = ImportServerCatalogResponse 
  { 
  }


-- | An internal error has occured.
newtype InternalError = InternalError 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | A parameter specified in the request is not valid, is unsupported, or cannot be used.
newtype InvalidParameterException = InvalidParameterException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | Internet Protocol (IP) Address
newtype IpAddress = IpAddress String


-- | The license type to be used for the Amazon Machine Image (AMI) created after a successful ReplicationRun.
newtype LicenseType = LicenseType String


-- | Hardware (MAC) address
newtype MacAddress = MacAddress String


-- | The maximum number of results to return in one API call. If left empty, this will default to 50.
newtype MaxResults = MaxResults Int


-- | The request is missing a required parameter. Ensure that you have supplied all the required parameters for the request.
newtype MissingRequiredParameterException = MissingRequiredParameterException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | Pagination token to pass as input to API call
newtype NextToken = NextToken String


-- | No connectors are available to handle this request. Please associate connector(s) and verify any existing connectors are healthy and can respond to requests.
newtype NoConnectorsAvailableException = NoConnectorsAvailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | The specified operation is not allowed. This error can occur for a number of reasons; for example, you might be trying to start a Replication Run before seed Replication Run.
newtype OperationNotPermittedException = OperationNotPermittedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


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


-- | An active Replication Job already exists for the specified server.
newtype ReplicationJobAlreadyExistsException = ReplicationJobAlreadyExistsException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | The unique identifier for a Replication Job.
newtype ReplicationJobId = ReplicationJobId String


-- | List of Replication Jobs
newtype ReplicationJobList = ReplicationJobList (Array ReplicationJob)


-- | The specified Replication Job cannot be found.
newtype ReplicationJobNotFoundException = ReplicationJobNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | Current state of Replication Job
newtype ReplicationJobState = ReplicationJobState String


-- | String describing current status of Replication Job
newtype ReplicationJobStatusMessage = ReplicationJobStatusMessage String


-- | An indicator of the Replication Job being deleted or failed.
newtype ReplicationJobTerminated = ReplicationJobTerminated Boolean


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


-- | The unique identifier for a Replication Run.
newtype ReplicationRunId = ReplicationRunId String


-- | This user has exceeded the maximum allowed Replication Run limit.
newtype ReplicationRunLimitExceededException = ReplicationRunLimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | List of Replication Runs
newtype ReplicationRunList = ReplicationRunList (Array ReplicationRun)


-- | Current state of Replication Run
newtype ReplicationRunState = ReplicationRunState String


-- | String describing current status of Replication Run
newtype ReplicationRunStatusMessage = ReplicationRunStatusMessage String


-- | Type of Replication Run
newtype ReplicationRunType = ReplicationRunType String


-- | Name of service role in customer's account to be used by SMS service.
newtype RoleName = RoleName String


-- | Object representing a server
newtype Server = Server 
  { "ServerId'" :: NullOrUndefined (ServerId)
  , "ServerType'" :: NullOrUndefined (ServerType)
  , "VmServer'" :: NullOrUndefined (VmServer)
  , "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId)
  , "ReplicationJobTerminated'" :: NullOrUndefined (ReplicationJobTerminated)
  }


-- | The provided server cannot be replicated.
newtype ServerCannotBeReplicatedException = ServerCannotBeReplicatedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | Status of Server catalog
newtype ServerCatalogStatus = ServerCatalogStatus String


-- | Unique Identifier for a server
newtype ServerId = ServerId String


-- | List of servers from catalog
newtype ServerList = ServerList (Array Server)


-- | Type of server.
newtype ServerType = ServerType String


newtype StartOnDemandReplicationRunRequest = StartOnDemandReplicationRunRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "Description'" :: NullOrUndefined (Description)
  }


newtype StartOnDemandReplicationRunResponse = StartOnDemandReplicationRunResponse 
  { "ReplicationRunId'" :: NullOrUndefined (ReplicationRunId)
  }


-- | This user does not have permissions to perform this operation.
newtype UnauthorizedOperationException = UnauthorizedOperationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype UpdateReplicationJobRequest = UpdateReplicationJobRequest 
  { "ReplicationJobId'" :: (ReplicationJobId)
  , "Frequency'" :: NullOrUndefined (Frequency)
  , "NextReplicationRunStartTime'" :: NullOrUndefined (Number)
  , "LicenseType'" :: NullOrUndefined (LicenseType)
  , "RoleName'" :: NullOrUndefined (RoleName)
  , "Description'" :: NullOrUndefined (Description)
  }


newtype UpdateReplicationJobResponse = UpdateReplicationJobResponse 
  { 
  }


-- | Unique Identifier for a VM
newtype VmId = VmId String


-- | Unique Identifier for VM Manager
newtype VmManagerId = VmManagerId String


-- | VM Manager Name
newtype VmManagerName = VmManagerName String


-- | VM Management Product
newtype VmManagerType = VmManagerType String


-- | Name of Virtual Machine
newtype VmName = VmName String


-- | Path to VM
newtype VmPath = VmPath String


-- | Object representing a VM server
newtype VmServer = VmServer 
  { "VmServerAddress'" :: NullOrUndefined (VmServerAddress)
  , "VmName'" :: NullOrUndefined (VmName)
  , "VmManagerName'" :: NullOrUndefined (VmManagerName)
  , "VmManagerType'" :: NullOrUndefined (VmManagerType)
  , "VmPath'" :: NullOrUndefined (VmPath)
  }


-- | Object representing a server's location
newtype VmServerAddress = VmServerAddress 
  { "VmManagerId'" :: NullOrUndefined (VmManagerId)
  , "VmId'" :: NullOrUndefined (VmId)
  }
