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

##### Instances
``` purescript
Newtype AmiId _
```

#### `Connector`

``` purescript
newtype Connector
  = Connector { "ConnectorId'" :: NullOrUndefined (ConnectorId), "Version'" :: NullOrUndefined (ConnectorVersion), "Status'" :: NullOrUndefined (ConnectorStatus), "CapabilityList'" :: NullOrUndefined (ConnectorCapabilityList), "VmManagerName'" :: NullOrUndefined (VmManagerName), "VmManagerType'" :: NullOrUndefined (VmManagerType), "VmManagerId'" :: NullOrUndefined (VmManagerId), "IpAddress'" :: NullOrUndefined (IpAddress), "MacAddress'" :: NullOrUndefined (MacAddress), "AssociatedOn'" :: NullOrUndefined (Number) }
```

Object representing a Connector

##### Instances
``` purescript
Newtype Connector _
```

#### `ConnectorCapability`

``` purescript
newtype ConnectorCapability
  = ConnectorCapability String
```

Capabilities for a Connector

##### Instances
``` purescript
Newtype ConnectorCapability _
```

#### `ConnectorCapabilityList`

``` purescript
newtype ConnectorCapabilityList
  = ConnectorCapabilityList (Array ConnectorCapability)
```

List of Connector Capabilities

##### Instances
``` purescript
Newtype ConnectorCapabilityList _
```

#### `ConnectorId`

``` purescript
newtype ConnectorId
  = ConnectorId String
```

Unique Identifier for Connector

##### Instances
``` purescript
Newtype ConnectorId _
```

#### `ConnectorList`

``` purescript
newtype ConnectorList
  = ConnectorList (Array Connector)
```

List of connectors

##### Instances
``` purescript
Newtype ConnectorList _
```

#### `ConnectorStatus`

``` purescript
newtype ConnectorStatus
  = ConnectorStatus String
```

Status of on-premise Connector

##### Instances
``` purescript
Newtype ConnectorStatus _
```

#### `ConnectorVersion`

``` purescript
newtype ConnectorVersion
  = ConnectorVersion String
```

Connector version string

##### Instances
``` purescript
Newtype ConnectorVersion _
```

#### `CreateReplicationJobRequest`

``` purescript
newtype CreateReplicationJobRequest
  = CreateReplicationJobRequest { "ServerId'" :: ServerId, "SeedReplicationTime'" :: Number, "Frequency'" :: Frequency, "LicenseType'" :: NullOrUndefined (LicenseType), "RoleName'" :: NullOrUndefined (RoleName), "Description'" :: NullOrUndefined (Description) }
```

##### Instances
``` purescript
Newtype CreateReplicationJobRequest _
```

#### `CreateReplicationJobResponse`

``` purescript
newtype CreateReplicationJobResponse
  = CreateReplicationJobResponse { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId) }
```

##### Instances
``` purescript
Newtype CreateReplicationJobResponse _
```

#### `DeleteReplicationJobRequest`

``` purescript
newtype DeleteReplicationJobRequest
  = DeleteReplicationJobRequest { "ReplicationJobId'" :: ReplicationJobId }
```

##### Instances
``` purescript
Newtype DeleteReplicationJobRequest _
```

#### `DeleteReplicationJobResponse`

``` purescript
newtype DeleteReplicationJobResponse
  = DeleteReplicationJobResponse {  }
```

##### Instances
``` purescript
Newtype DeleteReplicationJobResponse _
```

#### `DeleteServerCatalogRequest`

``` purescript
newtype DeleteServerCatalogRequest
  = DeleteServerCatalogRequest {  }
```

##### Instances
``` purescript
Newtype DeleteServerCatalogRequest _
```

#### `DeleteServerCatalogResponse`

``` purescript
newtype DeleteServerCatalogResponse
  = DeleteServerCatalogResponse {  }
```

##### Instances
``` purescript
Newtype DeleteServerCatalogResponse _
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

The description for a Replication Job/Run.

##### Instances
``` purescript
Newtype Description _
```

#### `DisassociateConnectorRequest`

``` purescript
newtype DisassociateConnectorRequest
  = DisassociateConnectorRequest { "ConnectorId'" :: ConnectorId }
```

##### Instances
``` purescript
Newtype DisassociateConnectorRequest _
```

#### `DisassociateConnectorResponse`

``` purescript
newtype DisassociateConnectorResponse
  = DisassociateConnectorResponse {  }
```

##### Instances
``` purescript
Newtype DisassociateConnectorResponse _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

Error Message string

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `Frequency`

``` purescript
newtype Frequency
  = Frequency Int
```

Interval between Replication Runs. This value is specified in hours, and represents the time between consecutive Replication Runs.

##### Instances
``` purescript
Newtype Frequency _
```

#### `GetConnectorsRequest`

``` purescript
newtype GetConnectorsRequest
  = GetConnectorsRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype GetConnectorsRequest _
```

#### `GetConnectorsResponse`

``` purescript
newtype GetConnectorsResponse
  = GetConnectorsResponse { "ConnectorList'" :: NullOrUndefined (ConnectorList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetConnectorsResponse _
```

#### `GetReplicationJobsRequest`

``` purescript
newtype GetReplicationJobsRequest
  = GetReplicationJobsRequest { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId), "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype GetReplicationJobsRequest _
```

#### `GetReplicationJobsResponse`

``` purescript
newtype GetReplicationJobsResponse
  = GetReplicationJobsResponse { "ReplicationJobList'" :: NullOrUndefined (ReplicationJobList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetReplicationJobsResponse _
```

#### `GetReplicationRunsRequest`

``` purescript
newtype GetReplicationRunsRequest
  = GetReplicationRunsRequest { "ReplicationJobId'" :: ReplicationJobId, "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype GetReplicationRunsRequest _
```

#### `GetReplicationRunsResponse`

``` purescript
newtype GetReplicationRunsResponse
  = GetReplicationRunsResponse { "ReplicationJob'" :: NullOrUndefined (ReplicationJob), "ReplicationRunList'" :: NullOrUndefined (ReplicationRunList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetReplicationRunsResponse _
```

#### `GetServersRequest`

``` purescript
newtype GetServersRequest
  = GetServersRequest { "NextToken'" :: NullOrUndefined (NextToken), "MaxResults'" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype GetServersRequest _
```

#### `GetServersResponse`

``` purescript
newtype GetServersResponse
  = GetServersResponse { "LastModifiedOn'" :: NullOrUndefined (Number), "ServerCatalogStatus'" :: NullOrUndefined (ServerCatalogStatus), "ServerList'" :: NullOrUndefined (ServerList), "NextToken'" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype GetServersResponse _
```

#### `ImportServerCatalogRequest`

``` purescript
newtype ImportServerCatalogRequest
  = ImportServerCatalogRequest {  }
```

##### Instances
``` purescript
Newtype ImportServerCatalogRequest _
```

#### `ImportServerCatalogResponse`

``` purescript
newtype ImportServerCatalogResponse
  = ImportServerCatalogResponse {  }
```

##### Instances
``` purescript
Newtype ImportServerCatalogResponse _
```

#### `InternalError`

``` purescript
newtype InternalError
  = InternalError { "Message'" :: NullOrUndefined (ErrorMessage) }
```

An internal error has occured.

##### Instances
``` purescript
Newtype InternalError _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

A parameter specified in the request is not valid, is unsupported, or cannot be used.

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

Internet Protocol (IP) Address

##### Instances
``` purescript
Newtype IpAddress _
```

#### `LicenseType`

``` purescript
newtype LicenseType
  = LicenseType String
```

The license type to be used for the Amazon Machine Image (AMI) created after a successful ReplicationRun.

##### Instances
``` purescript
Newtype LicenseType _
```

#### `MacAddress`

``` purescript
newtype MacAddress
  = MacAddress String
```

Hardware (MAC) address

##### Instances
``` purescript
Newtype MacAddress _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

The maximum number of results to return in one API call. If left empty, this will default to 50.

##### Instances
``` purescript
Newtype MaxResults _
```

#### `MissingRequiredParameterException`

``` purescript
newtype MissingRequiredParameterException
  = MissingRequiredParameterException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

The request is missing a required parameter. Ensure that you have supplied all the required parameters for the request.

##### Instances
``` purescript
Newtype MissingRequiredParameterException _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

Pagination token to pass as input to API call

##### Instances
``` purescript
Newtype NextToken _
```

#### `NoConnectorsAvailableException`

``` purescript
newtype NoConnectorsAvailableException
  = NoConnectorsAvailableException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

No connectors are available to handle this request. Please associate connector(s) and verify any existing connectors are healthy and can respond to requests.

##### Instances
``` purescript
Newtype NoConnectorsAvailableException _
```

#### `OperationNotPermittedException`

``` purescript
newtype OperationNotPermittedException
  = OperationNotPermittedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

The specified operation is not allowed. This error can occur for a number of reasons; for example, you might be trying to start a Replication Run before seed Replication Run.

##### Instances
``` purescript
Newtype OperationNotPermittedException _
```

#### `ReplicationJob`

``` purescript
newtype ReplicationJob
  = ReplicationJob { "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId), "ServerId'" :: NullOrUndefined (ServerId), "ServerType'" :: NullOrUndefined (ServerType), "VmServer'" :: NullOrUndefined (VmServer), "SeedReplicationTime'" :: NullOrUndefined (Number), "Frequency'" :: NullOrUndefined (Frequency), "NextReplicationRunStartTime'" :: NullOrUndefined (Number), "LicenseType'" :: NullOrUndefined (LicenseType), "RoleName'" :: NullOrUndefined (RoleName), "LatestAmiId'" :: NullOrUndefined (AmiId), "State'" :: NullOrUndefined (ReplicationJobState), "StatusMessage'" :: NullOrUndefined (ReplicationJobStatusMessage), "Description'" :: NullOrUndefined (Description), "ReplicationRunList'" :: NullOrUndefined (ReplicationRunList) }
```

Object representing a Replication Job

##### Instances
``` purescript
Newtype ReplicationJob _
```

#### `ReplicationJobAlreadyExistsException`

``` purescript
newtype ReplicationJobAlreadyExistsException
  = ReplicationJobAlreadyExistsException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

An active Replication Job already exists for the specified server.

##### Instances
``` purescript
Newtype ReplicationJobAlreadyExistsException _
```

#### `ReplicationJobId`

``` purescript
newtype ReplicationJobId
  = ReplicationJobId String
```

The unique identifier for a Replication Job.

##### Instances
``` purescript
Newtype ReplicationJobId _
```

#### `ReplicationJobList`

``` purescript
newtype ReplicationJobList
  = ReplicationJobList (Array ReplicationJob)
```

List of Replication Jobs

##### Instances
``` purescript
Newtype ReplicationJobList _
```

#### `ReplicationJobNotFoundException`

``` purescript
newtype ReplicationJobNotFoundException
  = ReplicationJobNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

The specified Replication Job cannot be found.

##### Instances
``` purescript
Newtype ReplicationJobNotFoundException _
```

#### `ReplicationJobState`

``` purescript
newtype ReplicationJobState
  = ReplicationJobState String
```

Current state of Replication Job

##### Instances
``` purescript
Newtype ReplicationJobState _
```

#### `ReplicationJobStatusMessage`

``` purescript
newtype ReplicationJobStatusMessage
  = ReplicationJobStatusMessage String
```

String describing current status of Replication Job

##### Instances
``` purescript
Newtype ReplicationJobStatusMessage _
```

#### `ReplicationJobTerminated`

``` purescript
newtype ReplicationJobTerminated
  = ReplicationJobTerminated Boolean
```

An indicator of the Replication Job being deleted or failed.

##### Instances
``` purescript
Newtype ReplicationJobTerminated _
```

#### `ReplicationRun`

``` purescript
newtype ReplicationRun
  = ReplicationRun { "ReplicationRunId'" :: NullOrUndefined (ReplicationRunId), "State'" :: NullOrUndefined (ReplicationRunState), "Type'" :: NullOrUndefined (ReplicationRunType), "StatusMessage'" :: NullOrUndefined (ReplicationRunStatusMessage), "AmiId'" :: NullOrUndefined (AmiId), "ScheduledStartTime'" :: NullOrUndefined (Number), "CompletedTime'" :: NullOrUndefined (Number), "Description'" :: NullOrUndefined (Description) }
```

Object representing a Replication Run

##### Instances
``` purescript
Newtype ReplicationRun _
```

#### `ReplicationRunId`

``` purescript
newtype ReplicationRunId
  = ReplicationRunId String
```

The unique identifier for a Replication Run.

##### Instances
``` purescript
Newtype ReplicationRunId _
```

#### `ReplicationRunLimitExceededException`

``` purescript
newtype ReplicationRunLimitExceededException
  = ReplicationRunLimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

This user has exceeded the maximum allowed Replication Run limit.

##### Instances
``` purescript
Newtype ReplicationRunLimitExceededException _
```

#### `ReplicationRunList`

``` purescript
newtype ReplicationRunList
  = ReplicationRunList (Array ReplicationRun)
```

List of Replication Runs

##### Instances
``` purescript
Newtype ReplicationRunList _
```

#### `ReplicationRunState`

``` purescript
newtype ReplicationRunState
  = ReplicationRunState String
```

Current state of Replication Run

##### Instances
``` purescript
Newtype ReplicationRunState _
```

#### `ReplicationRunStatusMessage`

``` purescript
newtype ReplicationRunStatusMessage
  = ReplicationRunStatusMessage String
```

String describing current status of Replication Run

##### Instances
``` purescript
Newtype ReplicationRunStatusMessage _
```

#### `ReplicationRunType`

``` purescript
newtype ReplicationRunType
  = ReplicationRunType String
```

Type of Replication Run

##### Instances
``` purescript
Newtype ReplicationRunType _
```

#### `RoleName`

``` purescript
newtype RoleName
  = RoleName String
```

Name of service role in customer's account to be used by SMS service.

##### Instances
``` purescript
Newtype RoleName _
```

#### `Server`

``` purescript
newtype Server
  = Server { "ServerId'" :: NullOrUndefined (ServerId), "ServerType'" :: NullOrUndefined (ServerType), "VmServer'" :: NullOrUndefined (VmServer), "ReplicationJobId'" :: NullOrUndefined (ReplicationJobId), "ReplicationJobTerminated'" :: NullOrUndefined (ReplicationJobTerminated) }
```

Object representing a server

##### Instances
``` purescript
Newtype Server _
```

#### `ServerCannotBeReplicatedException`

``` purescript
newtype ServerCannotBeReplicatedException
  = ServerCannotBeReplicatedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

The provided server cannot be replicated.

##### Instances
``` purescript
Newtype ServerCannotBeReplicatedException _
```

#### `ServerCatalogStatus`

``` purescript
newtype ServerCatalogStatus
  = ServerCatalogStatus String
```

Status of Server catalog

##### Instances
``` purescript
Newtype ServerCatalogStatus _
```

#### `ServerId`

``` purescript
newtype ServerId
  = ServerId String
```

Unique Identifier for a server

##### Instances
``` purescript
Newtype ServerId _
```

#### `ServerList`

``` purescript
newtype ServerList
  = ServerList (Array Server)
```

List of servers from catalog

##### Instances
``` purescript
Newtype ServerList _
```

#### `ServerType`

``` purescript
newtype ServerType
  = ServerType String
```

Type of server.

##### Instances
``` purescript
Newtype ServerType _
```

#### `StartOnDemandReplicationRunRequest`

``` purescript
newtype StartOnDemandReplicationRunRequest
  = StartOnDemandReplicationRunRequest { "ReplicationJobId'" :: ReplicationJobId, "Description'" :: NullOrUndefined (Description) }
```

##### Instances
``` purescript
Newtype StartOnDemandReplicationRunRequest _
```

#### `StartOnDemandReplicationRunResponse`

``` purescript
newtype StartOnDemandReplicationRunResponse
  = StartOnDemandReplicationRunResponse { "ReplicationRunId'" :: NullOrUndefined (ReplicationRunId) }
```

##### Instances
``` purescript
Newtype StartOnDemandReplicationRunResponse _
```

#### `UnauthorizedOperationException`

``` purescript
newtype UnauthorizedOperationException
  = UnauthorizedOperationException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

This user does not have permissions to perform this operation.

##### Instances
``` purescript
Newtype UnauthorizedOperationException _
```

#### `UpdateReplicationJobRequest`

``` purescript
newtype UpdateReplicationJobRequest
  = UpdateReplicationJobRequest { "ReplicationJobId'" :: ReplicationJobId, "Frequency'" :: NullOrUndefined (Frequency), "NextReplicationRunStartTime'" :: NullOrUndefined (Number), "LicenseType'" :: NullOrUndefined (LicenseType), "RoleName'" :: NullOrUndefined (RoleName), "Description'" :: NullOrUndefined (Description) }
```

##### Instances
``` purescript
Newtype UpdateReplicationJobRequest _
```

#### `UpdateReplicationJobResponse`

``` purescript
newtype UpdateReplicationJobResponse
  = UpdateReplicationJobResponse {  }
```

##### Instances
``` purescript
Newtype UpdateReplicationJobResponse _
```

#### `VmId`

``` purescript
newtype VmId
  = VmId String
```

Unique Identifier for a VM

##### Instances
``` purescript
Newtype VmId _
```

#### `VmManagerId`

``` purescript
newtype VmManagerId
  = VmManagerId String
```

Unique Identifier for VM Manager

##### Instances
``` purescript
Newtype VmManagerId _
```

#### `VmManagerName`

``` purescript
newtype VmManagerName
  = VmManagerName String
```

VM Manager Name

##### Instances
``` purescript
Newtype VmManagerName _
```

#### `VmManagerType`

``` purescript
newtype VmManagerType
  = VmManagerType String
```

VM Management Product

##### Instances
``` purescript
Newtype VmManagerType _
```

#### `VmName`

``` purescript
newtype VmName
  = VmName String
```

Name of Virtual Machine

##### Instances
``` purescript
Newtype VmName _
```

#### `VmPath`

``` purescript
newtype VmPath
  = VmPath String
```

Path to VM

##### Instances
``` purescript
Newtype VmPath _
```

#### `VmServer`

``` purescript
newtype VmServer
  = VmServer { "VmServerAddress'" :: NullOrUndefined (VmServerAddress), "VmName'" :: NullOrUndefined (VmName), "VmManagerName'" :: NullOrUndefined (VmManagerName), "VmManagerType'" :: NullOrUndefined (VmManagerType), "VmPath'" :: NullOrUndefined (VmPath) }
```

Object representing a VM server

##### Instances
``` purescript
Newtype VmServer _
```

#### `VmServerAddress`

``` purescript
newtype VmServerAddress
  = VmServerAddress { "VmManagerId'" :: NullOrUndefined (VmManagerId), "VmId'" :: NullOrUndefined (VmId) }
```

Object representing a server's location

##### Instances
``` purescript
Newtype VmServerAddress _
```


