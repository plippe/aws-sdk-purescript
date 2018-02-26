## Module AWS.SMS

Amazon Server Migration Service automates the process of migrating servers to EC2.

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createReplicationJob`

``` purescript
createReplicationJob :: forall eff. CreateReplicationJobRequest -> Aff (err :: RequestError | eff) CreateReplicationJobResponse
```

The CreateReplicationJob API is used to create a ReplicationJob to replicate a server on AWS. Call this API to first create a ReplicationJob, which will then schedule periodic ReplicationRuns to replicate your server to AWS. Each ReplicationRun will result in the creation of an AWS AMI.

#### `deleteReplicationJob`

``` purescript
deleteReplicationJob :: forall eff. DeleteReplicationJobRequest -> Aff (err :: RequestError | eff) DeleteReplicationJobResponse
```

The DeleteReplicationJob API is used to delete a ReplicationJob, resulting in no further ReplicationRuns. This will delete the contents of the S3 bucket used to store SMS artifacts, but will not delete any AMIs created by the SMS service.

#### `deleteServerCatalog`

``` purescript
deleteServerCatalog :: forall eff. DeleteServerCatalogRequest -> Aff (err :: RequestError | eff) DeleteServerCatalogResponse
```

The DeleteServerCatalog API clears all servers from your server catalog. This means that these servers will no longer be accessible to the Server Migration Service.

#### `disassociateConnector`

``` purescript
disassociateConnector :: forall eff. DisassociateConnectorRequest -> Aff (err :: RequestError | eff) DisassociateConnectorResponse
```

The DisassociateConnector API will disassociate a connector from the Server Migration Service, rendering it unavailable to support replication jobs.

#### `getConnectors`

``` purescript
getConnectors :: forall eff. GetConnectorsRequest -> Aff (err :: RequestError | eff) GetConnectorsResponse
```

The GetConnectors API returns a list of connectors that are registered with the Server Migration Service.

#### `getReplicationJobs`

``` purescript
getReplicationJobs :: forall eff. GetReplicationJobsRequest -> Aff (err :: RequestError | eff) GetReplicationJobsResponse
```

The GetReplicationJobs API will return all of your ReplicationJobs and their details. This API returns a paginated list, that may be consecutively called with nextToken to retrieve all ReplicationJobs.

#### `getReplicationRuns`

``` purescript
getReplicationRuns :: forall eff. GetReplicationRunsRequest -> Aff (err :: RequestError | eff) GetReplicationRunsResponse
```

The GetReplicationRuns API will return all ReplicationRuns for a given ReplicationJob. This API returns a paginated list, that may be consecutively called with nextToken to retrieve all ReplicationRuns for a ReplicationJob.

#### `getServers`

``` purescript
getServers :: forall eff. GetServersRequest -> Aff (err :: RequestError | eff) GetServersResponse
```

The GetServers API returns a list of all servers in your server catalog. For this call to succeed, you must previously have called ImportServerCatalog.

#### `importServerCatalog`

``` purescript
importServerCatalog :: forall eff. ImportServerCatalogRequest -> Aff (err :: RequestError | eff) ImportServerCatalogResponse
```

The ImportServerCatalog API is used to gather the complete list of on-premises servers on your premises. This API call requires connectors to be installed and monitoring all servers you would like imported. This API call returns immediately, but may take some time to retrieve all of the servers.

#### `startOnDemandReplicationRun`

``` purescript
startOnDemandReplicationRun :: forall eff. StartOnDemandReplicationRunRequest -> Aff (err :: RequestError | eff) StartOnDemandReplicationRunResponse
```

The StartOnDemandReplicationRun API is used to start a ReplicationRun on demand (in addition to those that are scheduled based on your frequency). This ReplicationRun will start immediately. StartOnDemandReplicationRun is subject to limits on how many on demand ReplicationRuns you may call per 24-hour period.

#### `updateReplicationJob`

``` purescript
updateReplicationJob :: forall eff. UpdateReplicationJobRequest -> Aff (err :: RequestError | eff) UpdateReplicationJobResponse
```

The UpdateReplicationJob API is used to change the settings of your existing ReplicationJob created using CreateReplicationJob. Calling this API will affect the next scheduled ReplicationRun.

#### `AmiId`

``` purescript
newtype AmiId
  = AmiId String
```

The AMI id for the image resulting from a Replication Run.

#### `Connector`

``` purescript
newtype Connector
  = Connector { "ConnectorId'" :: NullOrUndefined (ConnectorId), "Version'" :: NullOrUndefined (ConnectorVersion), "Status'" :: NullOrUndefined (ConnectorStatus), "CapabilityList'" :: NullOrUndefined (ConnectorCapabilityList), "VmManagerName'" :: NullOrUndefined (VmManagerName), "VmManagerType'" :: NullOrUndefined (VmManagerType), "VmManagerId'" :: NullOrUndefined (VmManagerId), "IpAddress'" :: NullOrUndefined (IpAddress), "MacAddress'" :: NullOrUndefined (MacAddress), "AssociatedOn'" :: NullOrUndefined (Number) }
```

Object representing a Connector

#### `ConnectorCapability`

``` purescript
newtype ConnectorCapability
  = ConnectorCapability String
```

Capabilities for a Connector

#### `ConnectorCapabilityList`

``` purescript
newtype ConnectorCapabilityList
  = ConnectorCapabilityList (Array ConnectorCapability)
```

List of Connector Capabilities

#### `ConnectorId`

``` purescript
newtype ConnectorId
  = ConnectorId String
```

Unique Identifier for Connector

#### `ConnectorList`

``` purescript
newtype ConnectorList
  = ConnectorList (Array Connector)
```

List of connectors

#### `ConnectorStatus`

``` purescript
newtype ConnectorStatus
  = ConnectorStatus String
```

Status of on-premise Connector

#### `ConnectorVersion`

``` purescript
newtype ConnectorVersion
  = ConnectorVersion String
```

Connector version string

#### `CreateReplicationJobRequest`

``` purescript
newtype CreateReplicationJobRequest
  = CreateReplicationJobRequest { "ServerId'" :: ServerId, "SeedReplicationTime'" :: Number, "Frequency'" :: Frequency, "LicenseType'" :: NullOrUndefined (LicenseType), "RoleName'" :: NullOrUndefined (RoleName), "Description'" :: NullOrUndefined (Description) }
```

#### `CreateReplicationJobResponse`

``` purescript
newtype CreateReplicationJobResponse
  = CreateReplicationJobResponse { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId) }
```

#### `DeleteReplicationJobRequest`

``` purescript
newtype DeleteReplicationJobRequest
  = DeleteReplicationJobRequest { "ReplicationJobId'" :: ReplicationJobId }
```

#### `DeleteReplicationJobResponse`

``` purescript
newtype DeleteReplicationJobResponse
  = DeleteReplicationJobResponse {  }
```

#### `DeleteServerCatalogRequest`

``` purescript
newtype DeleteServerCatalogRequest
  = DeleteServerCatalogRequest {  }
```

#### `DeleteServerCatalogResponse`

``` purescript
newtype DeleteServerCatalogResponse
  = DeleteServerCatalogResponse {  }
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

The description for a Replication Job/Run.

#### `DisassociateConnectorRequest`

``` purescript
newtype DisassociateConnectorRequest
  = DisassociateConnectorRequest { "ConnectorId'" :: ConnectorId }
```

#### `DisassociateConnectorResponse`

``` purescript
newtype DisassociateConnectorResponse
  = DisassociateConnectorResponse {  }
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

Error Message string

#### `Frequency`

``` purescript
newtype Frequency
  = Frequency Int
```

Interval between Replication Runs. This value is specified in hours, and represents the time between consecutive Replication Runs.

#### `GetConnectorsRequest`

``` purescript
newtype GetConnectorsRequest
  = GetConnectorsRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `GetConnectorsResponse`

``` purescript
newtype GetConnectorsResponse
  = GetConnectorsResponse { "ConnectorList'" :: NullOrUndefined (ConnectorList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `GetReplicationJobsRequest`

``` purescript
newtype GetReplicationJobsRequest
  = GetReplicationJobsRequest { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `GetReplicationJobsResponse`

``` purescript
newtype GetReplicationJobsResponse
  = GetReplicationJobsResponse { "ReplicationJobList'" :: NullOrUndefined (ReplicationJobList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `GetReplicationRunsRequest`

``` purescript
newtype GetReplicationRunsRequest
  = GetReplicationRunsRequest { "ReplicationJobId'" :: ReplicationJobId, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `GetReplicationRunsResponse`

``` purescript
newtype GetReplicationRunsResponse
  = GetReplicationRunsResponse { "ReplicationJob'" :: NullOrUndefined (ReplicationJob), "ReplicationRunList'" :: NullOrUndefined (ReplicationRunList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `GetServersRequest`

``` purescript
newtype GetServersRequest
  = GetServersRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

#### `GetServersResponse`

``` purescript
newtype GetServersResponse
  = GetServersResponse { "LastModifiedOn'" :: NullOrUndefined (Number), "ServerCatalogStatus'" :: NullOrUndefined (ServerCatalogStatus), "ServerList'" :: NullOrUndefined (ServerList), "NextToken'" :: NullOrUndefined (NextToken) }
```

#### `ImportServerCatalogRequest`

``` purescript
newtype ImportServerCatalogRequest
  = ImportServerCatalogRequest {  }
```

#### `ImportServerCatalogResponse`

``` purescript
newtype ImportServerCatalogResponse
  = ImportServerCatalogResponse {  }
```

#### `InternalError`

``` purescript
newtype InternalError
  = InternalError { "Message'" :: NullOrUndefined (ErrorMessage) }
```

An internal error has occured.

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

A parameter specified in the request is not valid, is unsupported, or cannot be used.

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

Internet Protocol (IP) Address

#### `LicenseType`

``` purescript
newtype LicenseType
  = LicenseType String
```

The license type to be used for the Amazon Machine Image (AMI) created after a successful ReplicationRun.

#### `MacAddress`

``` purescript
newtype MacAddress
  = MacAddress String
```

Hardware (MAC) address

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

The maximum number of results to return in one API call. If left empty, this will default to 50.

#### `MissingRequiredParameterException`

``` purescript
newtype MissingRequiredParameterException
  = MissingRequiredParameterException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

The request is missing a required parameter. Ensure that you have supplied all the required parameters for the request.

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

Pagination token to pass as input to API call

#### `NoConnectorsAvailableException`

``` purescript
newtype NoConnectorsAvailableException
  = NoConnectorsAvailableException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

No connectors are available to handle this request. Please associate connector(s) and verify any existing connectors are healthy and can respond to requests.

#### `OperationNotPermittedException`

``` purescript
newtype OperationNotPermittedException
  = OperationNotPermittedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

The specified operation is not allowed. This error can occur for a number of reasons; for example, you might be trying to start a Replication Run before seed Replication Run.

#### `ReplicationJob`

``` purescript
newtype ReplicationJob
  = ReplicationJob { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId), "ServerId'" :: NullOrUndefined (ServerId), "ServerType'" :: NullOrUndefined (ServerType), "VmServer'" :: NullOrUndefined (VmServer), "SeedReplicationTime'" :: NullOrUndefined (Number), "Frequency'" :: NullOrUndefined (Frequency), "NextReplicationRunStartTime'" :: NullOrUndefined (Number), "LicenseType'" :: NullOrUndefined (LicenseType), "RoleName'" :: NullOrUndefined (RoleName), "LatestAmiId'" :: NullOrUndefined (AmiId), "State'" :: NullOrUndefined (ReplicationJobState), "StatusMessage'" :: NullOrUndefined (ReplicationJobStatusMessage), "Description'" :: NullOrUndefined (Description), "ReplicationRunList'" :: NullOrUndefined (ReplicationRunList) }
```

Object representing a Replication Job

#### `ReplicationJobAlreadyExistsException`

``` purescript
newtype ReplicationJobAlreadyExistsException
  = ReplicationJobAlreadyExistsException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

An active Replication Job already exists for the specified server.

#### `ReplicationJobId`

``` purescript
newtype ReplicationJobId
  = ReplicationJobId String
```

The unique identifier for a Replication Job.

#### `ReplicationJobList`

``` purescript
newtype ReplicationJobList
  = ReplicationJobList (Array ReplicationJob)
```

List of Replication Jobs

#### `ReplicationJobNotFoundException`

``` purescript
newtype ReplicationJobNotFoundException
  = ReplicationJobNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

The specified Replication Job cannot be found.

#### `ReplicationJobState`

``` purescript
newtype ReplicationJobState
  = ReplicationJobState String
```

Current state of Replication Job

#### `ReplicationJobStatusMessage`

``` purescript
newtype ReplicationJobStatusMessage
  = ReplicationJobStatusMessage String
```

String describing current status of Replication Job

#### `ReplicationJobTerminated`

``` purescript
newtype ReplicationJobTerminated
  = ReplicationJobTerminated Boolean
```

An indicator of the Replication Job being deleted or failed.

#### `ReplicationRun`

``` purescript
newtype ReplicationRun
  = ReplicationRun { "ReplicationRunId'" :: NullOrUndefined (ReplicationRunId), "State'" :: NullOrUndefined (ReplicationRunState), "Type'" :: NullOrUndefined (ReplicationRunType), "StatusMessage'" :: NullOrUndefined (ReplicationRunStatusMessage), "AmiId'" :: NullOrUndefined (AmiId), "ScheduledStartTime'" :: NullOrUndefined (Number), "CompletedTime'" :: NullOrUndefined (Number), "Description'" :: NullOrUndefined (Description) }
```

Object representing a Replication Run

#### `ReplicationRunId`

``` purescript
newtype ReplicationRunId
  = ReplicationRunId String
```

The unique identifier for a Replication Run.

#### `ReplicationRunLimitExceededException`

``` purescript
newtype ReplicationRunLimitExceededException
  = ReplicationRunLimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

This user has exceeded the maximum allowed Replication Run limit.

#### `ReplicationRunList`

``` purescript
newtype ReplicationRunList
  = ReplicationRunList (Array ReplicationRun)
```

List of Replication Runs

#### `ReplicationRunState`

``` purescript
newtype ReplicationRunState
  = ReplicationRunState String
```

Current state of Replication Run

#### `ReplicationRunStatusMessage`

``` purescript
newtype ReplicationRunStatusMessage
  = ReplicationRunStatusMessage String
```

String describing current status of Replication Run

#### `ReplicationRunType`

``` purescript
newtype ReplicationRunType
  = ReplicationRunType String
```

Type of Replication Run

#### `RoleName`

``` purescript
newtype RoleName
  = RoleName String
```

Name of service role in customer's account to be used by SMS service.

#### `Server`

``` purescript
newtype Server
  = Server { "ServerId'" :: NullOrUndefined (ServerId), "ServerType'" :: NullOrUndefined (ServerType), "VmServer'" :: NullOrUndefined (VmServer), "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId), "ReplicationJobTerminated'" :: NullOrUndefined (ReplicationJobTerminated) }
```

Object representing a server

#### `ServerCannotBeReplicatedException`

``` purescript
newtype ServerCannotBeReplicatedException
  = ServerCannotBeReplicatedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

The provided server cannot be replicated.

#### `ServerCatalogStatus`

``` purescript
newtype ServerCatalogStatus
  = ServerCatalogStatus String
```

Status of Server catalog

#### `ServerId`

``` purescript
newtype ServerId
  = ServerId String
```

Unique Identifier for a server

#### `ServerList`

``` purescript
newtype ServerList
  = ServerList (Array Server)
```

List of servers from catalog

#### `ServerType`

``` purescript
newtype ServerType
  = ServerType String
```

Type of server.

#### `StartOnDemandReplicationRunRequest`

``` purescript
newtype StartOnDemandReplicationRunRequest
  = StartOnDemandReplicationRunRequest { "ReplicationJobId'" :: ReplicationJobId, "Description'" :: NullOrUndefined (Description) }
```

#### `StartOnDemandReplicationRunResponse`

``` purescript
newtype StartOnDemandReplicationRunResponse
  = StartOnDemandReplicationRunResponse { "ReplicationRunId'" :: NullOrUndefined (ReplicationRunId) }
```

#### `UnauthorizedOperationException`

``` purescript
newtype UnauthorizedOperationException
  = UnauthorizedOperationException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

This user does not have permissions to perform this operation.

#### `UpdateReplicationJobRequest`

``` purescript
newtype UpdateReplicationJobRequest
  = UpdateReplicationJobRequest { "ReplicationJobId'" :: ReplicationJobId, "Frequency'" :: NullOrUndefined (Frequency), "NextReplicationRunStartTime'" :: NullOrUndefined (Number), "LicenseType'" :: NullOrUndefined (LicenseType), "RoleName'" :: NullOrUndefined (RoleName), "Description'" :: NullOrUndefined (Description) }
```

#### `UpdateReplicationJobResponse`

``` purescript
newtype UpdateReplicationJobResponse
  = UpdateReplicationJobResponse {  }
```

#### `VmId`

``` purescript
newtype VmId
  = VmId String
```

Unique Identifier for a VM

#### `VmManagerId`

``` purescript
newtype VmManagerId
  = VmManagerId String
```

Unique Identifier for VM Manager

#### `VmManagerName`

``` purescript
newtype VmManagerName
  = VmManagerName String
```

VM Manager Name

#### `VmManagerType`

``` purescript
newtype VmManagerType
  = VmManagerType String
```

VM Management Product

#### `VmName`

``` purescript
newtype VmName
  = VmName String
```

Name of Virtual Machine

#### `VmPath`

``` purescript
newtype VmPath
  = VmPath String
```

Path to VM

#### `VmServer`

``` purescript
newtype VmServer
  = VmServer { "VmServerAddress'" :: NullOrUndefined (VmServerAddress), "VmName'" :: NullOrUndefined (VmName), "VmManagerName'" :: NullOrUndefined (VmManagerName), "VmManagerType'" :: NullOrUndefined (VmManagerType), "VmPath'" :: NullOrUndefined (VmPath) }
```

Object representing a VM server

#### `VmServerAddress`

``` purescript
newtype VmServerAddress
  = VmServerAddress { "VmManagerId'" :: NullOrUndefined (VmManagerId), "VmId'" :: NullOrUndefined (VmId) }
```

Object representing a server's location


