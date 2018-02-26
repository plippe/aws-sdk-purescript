## Module AWS.MigrationHub

<p/>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateCreatedArtifact`

``` purescript
associateCreatedArtifact :: forall eff. AssociateCreatedArtifactRequest -> Aff (err :: RequestError | eff) AssociateCreatedArtifactResult
```

<p>Associates a created artifact of an AWS cloud resource, the target receiving the migration, with the migration task performed by a migration tool. This API has the following traits:</p> <ul> <li> <p>Migration tools can call the <code>AssociateCreatedArtifact</code> operation to indicate which AWS artifact is associated with a migration task.</p> </li> <li> <p>The created artifact name must be provided in ARN (Amazon Resource Name) format which will contain information about type and region; for example: <code>arn:aws:ec2:us-east-1:488216288981:image/ami-6d0ba87b</code>.</p> </li> <li> <p>Examples of the AWS resource behind the created artifact are, AMI's, EC2 instance, or DMS endpoint, etc.</p> </li> </ul>

#### `associateDiscoveredResource`

``` purescript
associateDiscoveredResource :: forall eff. AssociateDiscoveredResourceRequest -> Aff (err :: RequestError | eff) AssociateDiscoveredResourceResult
```

<p>Associates a discovered resource ID from Application Discovery Service (ADS) with a migration task.</p>

#### `createProgressUpdateStream`

``` purescript
createProgressUpdateStream :: forall eff. CreateProgressUpdateStreamRequest -> Aff (err :: RequestError | eff) CreateProgressUpdateStreamResult
```

<p>Creates a progress update stream which is an AWS resource used for access control as well as a namespace for migration task names that is implicitly linked to your AWS account. It must uniquely identify the migration tool as it is used for all updates made by the tool; however, it does not need to be unique for each AWS account because it is scoped to the AWS account.</p>

#### `deleteProgressUpdateStream`

``` purescript
deleteProgressUpdateStream :: forall eff. DeleteProgressUpdateStreamRequest -> Aff (err :: RequestError | eff) DeleteProgressUpdateStreamResult
```

<p>Deletes a progress update stream, including all of its tasks, which was previously created as an AWS resource used for access control. This API has the following traits:</p> <ul> <li> <p>The only parameter needed for <code>DeleteProgressUpdateStream</code> is the stream name (same as a <code>CreateProgressUpdateStream</code> call).</p> </li> <li> <p>The call will return, and a background process will asynchronously be doing the actual delete of the stream and all of its resources (tasks, associated resources, resource attributes, created artifacts).</p> </li> <li> <p>If the stream takes time to be deleted, it might still show up on a <code>ListProgressUpdateStreams</code> call.</p> </li> <li> <p> <code>CreateProgressUpdateStream</code>, <code>ImportMigrationTask</code>, <code>NotifyMigrationTaskState</code>, and all Associate[*] APIs realted to the tasks belonging to the stream will throw "InvalidInputException" if the stream of the same name is in the process of being deleted.</p> </li> <li> <p>Once the stream and all of its resources are deleted, <code>CreateProgressUpdateStream</code> for a stream of the same name will succeed, and that stream will be an entirely new logical resource (without any resources associated with the old stream).</p> </li> </ul>

#### `describeApplicationState`

``` purescript
describeApplicationState :: forall eff. DescribeApplicationStateRequest -> Aff (err :: RequestError | eff) DescribeApplicationStateResult
```

<p>Gets the migration status of an application.</p>

#### `describeMigrationTask`

``` purescript
describeMigrationTask :: forall eff. DescribeMigrationTaskRequest -> Aff (err :: RequestError | eff) DescribeMigrationTaskResult
```

<p>Retrieves a list of all attributes associated with a specific migration task.</p>

#### `disassociateCreatedArtifact`

``` purescript
disassociateCreatedArtifact :: forall eff. DisassociateCreatedArtifactRequest -> Aff (err :: RequestError | eff) DisassociateCreatedArtifactResult
```

<p>Disassociates a created artifact of an AWS resource with a migration task performed by a migration tool that was previously associated. This API has the following traits:</p> <ul> <li> <p>A migration user can call the <code>DisassociateCreatedArtifacts</code> operation to disassociate a created AWS Artifact from a migration task.</p> </li> <li> <p>The created artifact name must be provided in ARN (Amazon Resource Name) format which will contain information about type and region; for example: <code>arn:aws:ec2:us-east-1:488216288981:image/ami-6d0ba87b</code>.</p> </li> <li> <p>Examples of the AWS resource behind the created artifact are, AMI's, EC2 instance, or RDS instance, etc.</p> </li> </ul>

#### `disassociateDiscoveredResource`

``` purescript
disassociateDiscoveredResource :: forall eff. DisassociateDiscoveredResourceRequest -> Aff (err :: RequestError | eff) DisassociateDiscoveredResourceResult
```

<p>Disassociate an Application Discovery Service (ADS) discovered resource from a migration task.</p>

#### `importMigrationTask`

``` purescript
importMigrationTask :: forall eff. ImportMigrationTaskRequest -> Aff (err :: RequestError | eff) ImportMigrationTaskResult
```

<p>Registers a new migration task which represents a server, database, etc., being migrated to AWS by a migration tool.</p> <p>This API is a prerequisite to calling the <code>NotifyMigrationTaskState</code> API as the migration tool must first register the migration task with Migration Hub.</p>

#### `listCreatedArtifacts`

``` purescript
listCreatedArtifacts :: forall eff. ListCreatedArtifactsRequest -> Aff (err :: RequestError | eff) ListCreatedArtifactsResult
```

<p>Lists the created artifacts attached to a given migration task in an update stream. This API has the following traits:</p> <ul> <li> <p>Gets the list of the created artifacts while migration is taking place.</p> </li> <li> <p>Shows the artifacts created by the migration tool that was associated by the <code>AssociateCreatedArtifact</code> API. </p> </li> <li> <p>Lists created artifacts in a paginated interface. </p> </li> </ul>

#### `listDiscoveredResources`

``` purescript
listDiscoveredResources :: forall eff. ListDiscoveredResourcesRequest -> Aff (err :: RequestError | eff) ListDiscoveredResourcesResult
```

<p>Lists discovered resources associated with the given <code>MigrationTask</code>.</p>

#### `listMigrationTasks`

``` purescript
listMigrationTasks :: forall eff. ListMigrationTasksRequest -> Aff (err :: RequestError | eff) ListMigrationTasksResult
```

<p>Lists all, or filtered by resource name, migration tasks associated with the user account making this call. This API has the following traits:</p> <ul> <li> <p>Can show a summary list of the most recent migration tasks.</p> </li> <li> <p>Can show a summary list of migration tasks associated with a given discovered resource.</p> </li> <li> <p>Lists migration tasks in a paginated interface.</p> </li> </ul>

#### `listProgressUpdateStreams`

``` purescript
listProgressUpdateStreams :: forall eff. ListProgressUpdateStreamsRequest -> Aff (err :: RequestError | eff) ListProgressUpdateStreamsResult
```

<p>Lists progress update streams associated with the user account making this call.</p>

#### `notifyApplicationState`

``` purescript
notifyApplicationState :: forall eff. NotifyApplicationStateRequest -> Aff (err :: RequestError | eff) NotifyApplicationStateResult
```

<p>Sets the migration state of an application. For a given application identified by the value passed to <code>ApplicationId</code>, its status is set or updated by passing one of three values to <code>Status</code>: <code>NOT_STARTED | IN_PROGRESS | COMPLETED</code>.</p>

#### `notifyMigrationTaskState`

``` purescript
notifyMigrationTaskState :: forall eff. NotifyMigrationTaskStateRequest -> Aff (err :: RequestError | eff) NotifyMigrationTaskStateResult
```

<p>Notifies Migration Hub of the current status, progress, or other detail regarding a migration task. This API has the following traits:</p> <ul> <li> <p>Migration tools will call the <code>NotifyMigrationTaskState</code> API to share the latest progress and status.</p> </li> <li> <p> <code>MigrationTaskName</code> is used for addressing updates to the correct target.</p> </li> <li> <p> <code>ProgressUpdateStream</code> is used for access control and to provide a namespace for each migration tool.</p> </li> </ul>

#### `putResourceAttributes`

``` purescript
putResourceAttributes :: forall eff. PutResourceAttributesRequest -> Aff (err :: RequestError | eff) PutResourceAttributesResult
```

<p>Provides identifying details of the resource being migrated so that it can be associated in the Application Discovery Service (ADS)'s repository. This association occurs asynchronously after <code>PutResourceAttributes</code> returns.</p> <important> <p>Keep in mind that subsequent calls to PutResourceAttributes will override previously stored attributes. For example, if it is first called with a MAC address, but later, it is desired to <i>add</i> an IP address, it will then be required to call it with <i>both</i> the IP and MAC addresses to prevent overiding the MAC address.</p> </important> <note> <p>Because this is an asynchronous call, it will always return 200, whether an association occurs or not. To confirm if an association was found based on the provided details, call <code>ListAssociatedResource</code>.</p> </note>

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception raised when the account making the call is not whitelisted or there are other authentication errors.</p>

#### `ApplicationId`

``` purescript
newtype ApplicationId
  = ApplicationId String
```

#### `ApplicationStatus`

``` purescript
newtype ApplicationStatus
  = ApplicationStatus String
```

#### `AssociateCreatedArtifactRequest`

``` purescript
newtype AssociateCreatedArtifactRequest
  = AssociateCreatedArtifactRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "CreatedArtifact" :: CreatedArtifact, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `AssociateCreatedArtifactResult`

``` purescript
newtype AssociateCreatedArtifactResult
  = AssociateCreatedArtifactResult {  }
```

#### `AssociateDiscoveredResourceRequest`

``` purescript
newtype AssociateDiscoveredResourceRequest
  = AssociateDiscoveredResourceRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "DiscoveredResource" :: DiscoveredResource, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `AssociateDiscoveredResourceResult`

``` purescript
newtype AssociateDiscoveredResourceResult
  = AssociateDiscoveredResourceResult {  }
```

#### `ConfigurationId`

``` purescript
newtype ConfigurationId
  = ConfigurationId String
```

#### `CreateProgressUpdateStreamRequest`

``` purescript
newtype CreateProgressUpdateStreamRequest
  = CreateProgressUpdateStreamRequest { "ProgressUpdateStreamName" :: ProgressUpdateStream, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `CreateProgressUpdateStreamResult`

``` purescript
newtype CreateProgressUpdateStreamResult
  = CreateProgressUpdateStreamResult {  }
```

#### `CreatedArtifact`

``` purescript
newtype CreatedArtifact
  = CreatedArtifact { "Name" :: CreatedArtifactName, "Description" :: NullOrUndefined (CreatedArtifactDescription) }
```

<p>An ARN of the AWS cloud resource target receiving the migration (e.g., AMI, EC2 instance, RDS instance, etc.).</p>

#### `CreatedArtifactDescription`

``` purescript
newtype CreatedArtifactDescription
  = CreatedArtifactDescription String
```

#### `CreatedArtifactList`

``` purescript
newtype CreatedArtifactList
  = CreatedArtifactList (Array CreatedArtifact)
```

#### `CreatedArtifactName`

``` purescript
newtype CreatedArtifactName
  = CreatedArtifactName String
```

#### `DeleteProgressUpdateStreamRequest`

``` purescript
newtype DeleteProgressUpdateStreamRequest
  = DeleteProgressUpdateStreamRequest { "ProgressUpdateStreamName" :: ProgressUpdateStream, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `DeleteProgressUpdateStreamResult`

``` purescript
newtype DeleteProgressUpdateStreamResult
  = DeleteProgressUpdateStreamResult {  }
```

#### `DescribeApplicationStateRequest`

``` purescript
newtype DescribeApplicationStateRequest
  = DescribeApplicationStateRequest { "ApplicationId" :: ApplicationId }
```

#### `DescribeApplicationStateResult`

``` purescript
newtype DescribeApplicationStateResult
  = DescribeApplicationStateResult { "ApplicationStatus" :: NullOrUndefined (ApplicationStatus), "LastUpdatedTime" :: NullOrUndefined (UpdateDateTime) }
```

#### `DescribeMigrationTaskRequest`

``` purescript
newtype DescribeMigrationTaskRequest
  = DescribeMigrationTaskRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName }
```

#### `DescribeMigrationTaskResult`

``` purescript
newtype DescribeMigrationTaskResult
  = DescribeMigrationTaskResult { "MigrationTask" :: NullOrUndefined (MigrationTask) }
```

#### `DisassociateCreatedArtifactRequest`

``` purescript
newtype DisassociateCreatedArtifactRequest
  = DisassociateCreatedArtifactRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "CreatedArtifactName" :: CreatedArtifactName, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `DisassociateCreatedArtifactResult`

``` purescript
newtype DisassociateCreatedArtifactResult
  = DisassociateCreatedArtifactResult {  }
```

#### `DisassociateDiscoveredResourceRequest`

``` purescript
newtype DisassociateDiscoveredResourceRequest
  = DisassociateDiscoveredResourceRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "ConfigurationId" :: ConfigurationId, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `DisassociateDiscoveredResourceResult`

``` purescript
newtype DisassociateDiscoveredResourceResult
  = DisassociateDiscoveredResourceResult {  }
```

#### `DiscoveredResource`

``` purescript
newtype DiscoveredResource
  = DiscoveredResource { "ConfigurationId" :: ConfigurationId, "Description" :: NullOrUndefined (DiscoveredResourceDescription) }
```

<p>Object representing the on-premises resource being migrated.</p>

#### `DiscoveredResourceDescription`

``` purescript
newtype DiscoveredResourceDescription
  = DiscoveredResourceDescription String
```

#### `DiscoveredResourceList`

``` purescript
newtype DiscoveredResourceList
  = DiscoveredResourceList (Array DiscoveredResource)
```

#### `DryRun`

``` purescript
newtype DryRun
  = DryRun Boolean
```

#### `DryRunOperation`

``` purescript
newtype DryRunOperation
  = DryRunOperation { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception raised to indicate a successfully authorized action when the <code>DryRun</code> flag is set to "true".</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `ImportMigrationTaskRequest`

``` purescript
newtype ImportMigrationTaskRequest
  = ImportMigrationTaskRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `ImportMigrationTaskResult`

``` purescript
newtype ImportMigrationTaskResult
  = ImportMigrationTaskResult {  }
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception raised when there is an internal, configuration, or dependency error encountered.</p>

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception raised when the provided input violates a policy constraint or is entered in the wrong format or data type.</p>

#### `LatestResourceAttributeList`

``` purescript
newtype LatestResourceAttributeList
  = LatestResourceAttributeList (Array ResourceAttribute)
```

#### `ListCreatedArtifactsRequest`

``` purescript
newtype ListCreatedArtifactsRequest
  = ListCreatedArtifactsRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxResultsCreatedArtifacts) }
```

#### `ListCreatedArtifactsResult`

``` purescript
newtype ListCreatedArtifactsResult
  = ListCreatedArtifactsResult { "NextToken" :: NullOrUndefined (Token), "CreatedArtifactList" :: NullOrUndefined (CreatedArtifactList) }
```

#### `ListDiscoveredResourcesRequest`

``` purescript
newtype ListDiscoveredResourcesRequest
  = ListDiscoveredResourcesRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxResultsResources) }
```

#### `ListDiscoveredResourcesResult`

``` purescript
newtype ListDiscoveredResourcesResult
  = ListDiscoveredResourcesResult { "NextToken" :: NullOrUndefined (Token), "DiscoveredResourceList" :: NullOrUndefined (DiscoveredResourceList) }
```

#### `ListMigrationTasksRequest`

``` purescript
newtype ListMigrationTasksRequest
  = ListMigrationTasksRequest { "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxResults), "ResourceName" :: NullOrUndefined (ResourceName) }
```

#### `ListMigrationTasksResult`

``` purescript
newtype ListMigrationTasksResult
  = ListMigrationTasksResult { "NextToken" :: NullOrUndefined (Token), "MigrationTaskSummaryList" :: NullOrUndefined (MigrationTaskSummaryList) }
```

#### `ListProgressUpdateStreamsRequest`

``` purescript
newtype ListProgressUpdateStreamsRequest
  = ListProgressUpdateStreamsRequest { "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxResults) }
```

#### `ListProgressUpdateStreamsResult`

``` purescript
newtype ListProgressUpdateStreamsResult
  = ListProgressUpdateStreamsResult { "ProgressUpdateStreamSummaryList" :: NullOrUndefined (ProgressUpdateStreamSummaryList), "NextToken" :: NullOrUndefined (Token) }
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MaxResultsCreatedArtifacts`

``` purescript
newtype MaxResultsCreatedArtifacts
  = MaxResultsCreatedArtifacts Int
```

#### `MaxResultsResources`

``` purescript
newtype MaxResultsResources
  = MaxResultsResources Int
```

#### `MigrationTask`

``` purescript
newtype MigrationTask
  = MigrationTask { "ProgressUpdateStream" :: NullOrUndefined (ProgressUpdateStream), "MigrationTaskName" :: NullOrUndefined (MigrationTaskName), "Task" :: NullOrUndefined (Task), "UpdateDateTime" :: NullOrUndefined (UpdateDateTime), "ResourceAttributeList" :: NullOrUndefined (LatestResourceAttributeList) }
```

<p>Represents a migration task in a migration tool.</p>

#### `MigrationTaskName`

``` purescript
newtype MigrationTaskName
  = MigrationTaskName String
```

#### `MigrationTaskSummary`

``` purescript
newtype MigrationTaskSummary
  = MigrationTaskSummary { "ProgressUpdateStream" :: NullOrUndefined (ProgressUpdateStream), "MigrationTaskName" :: NullOrUndefined (MigrationTaskName), "Status" :: NullOrUndefined (Status), "ProgressPercent" :: NullOrUndefined (ProgressPercent), "StatusDetail" :: NullOrUndefined (StatusDetail), "UpdateDateTime" :: NullOrUndefined (UpdateDateTime) }
```

<p>MigrationTaskSummary includes <code>MigrationTaskName</code>, <code>ProgressPercent</code>, <code>ProgressUpdateStream</code>, <code>Status</code>, and <code>UpdateDateTime</code> for each task.</p>

#### `MigrationTaskSummaryList`

``` purescript
newtype MigrationTaskSummaryList
  = MigrationTaskSummaryList (Array MigrationTaskSummary)
```

#### `NextUpdateSeconds`

``` purescript
newtype NextUpdateSeconds
  = NextUpdateSeconds Int
```

#### `NotifyApplicationStateRequest`

``` purescript
newtype NotifyApplicationStateRequest
  = NotifyApplicationStateRequest { "ApplicationId" :: ApplicationId, "Status" :: ApplicationStatus, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `NotifyApplicationStateResult`

``` purescript
newtype NotifyApplicationStateResult
  = NotifyApplicationStateResult {  }
```

#### `NotifyMigrationTaskStateRequest`

``` purescript
newtype NotifyMigrationTaskStateRequest
  = NotifyMigrationTaskStateRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "Task" :: Task, "UpdateDateTime" :: UpdateDateTime, "NextUpdateSeconds" :: NextUpdateSeconds, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `NotifyMigrationTaskStateResult`

``` purescript
newtype NotifyMigrationTaskStateResult
  = NotifyMigrationTaskStateResult {  }
```

#### `PolicyErrorException`

``` purescript
newtype PolicyErrorException
  = PolicyErrorException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception raised when there are problems accessing ADS (Application Discovery Service); most likely due to a misconfigured policy or the <code>ADSCaller</code> role is missing or not configured correctly.</p>

#### `ProgressPercent`

``` purescript
newtype ProgressPercent
  = ProgressPercent Int
```

#### `ProgressUpdateStream`

``` purescript
newtype ProgressUpdateStream
  = ProgressUpdateStream String
```

#### `ProgressUpdateStreamSummary`

``` purescript
newtype ProgressUpdateStreamSummary
  = ProgressUpdateStreamSummary { "ProgressUpdateStreamName" :: NullOrUndefined (ProgressUpdateStream) }
```

<p>Summary of the AWS resource used for access control that is implicitly linked to your AWS account.</p>

#### `ProgressUpdateStreamSummaryList`

``` purescript
newtype ProgressUpdateStreamSummaryList
  = ProgressUpdateStreamSummaryList (Array ProgressUpdateStreamSummary)
```

#### `PutResourceAttributesRequest`

``` purescript
newtype PutResourceAttributesRequest
  = PutResourceAttributesRequest { "ProgressUpdateStream" :: ProgressUpdateStream, "MigrationTaskName" :: MigrationTaskName, "ResourceAttributeList" :: ResourceAttributeList, "DryRun" :: NullOrUndefined (DryRun) }
```

#### `PutResourceAttributesResult`

``` purescript
newtype PutResourceAttributesResult
  = PutResourceAttributesResult {  }
```

#### `ResourceAttribute`

``` purescript
newtype ResourceAttribute
  = ResourceAttribute { "Type" :: ResourceAttributeType, "Value" :: ResourceAttributeValue }
```

<p>Attribute associated with a resource.</p>

#### `ResourceAttributeList`

``` purescript
newtype ResourceAttributeList
  = ResourceAttributeList (Array ResourceAttribute)
```

#### `ResourceAttributeType`

``` purescript
newtype ResourceAttributeType
  = ResourceAttributeType String
```

#### `ResourceAttributeValue`

``` purescript
newtype ResourceAttributeValue
  = ResourceAttributeValue String
```

#### `ResourceName`

``` purescript
newtype ResourceName
  = ResourceName String
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception raised when the request references a resource (ADS configuration, update stream, migration task, etc.) that does not exist in ADS (Application Discovery Service) or in Migration Hub's repository.</p>

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception raised when the service encounters throttled communication with upstream dependencies or is overloaded with requests.</p>

#### `Status`

``` purescript
newtype Status
  = Status String
```

#### `StatusDetail`

``` purescript
newtype StatusDetail
  = StatusDetail String
```

#### `Task`

``` purescript
newtype Task
  = Task { "Status" :: Status, "StatusDetail" :: NullOrUndefined (StatusDetail), "ProgressPercent" :: NullOrUndefined (ProgressPercent) }
```

<p>Task object encapsulating task information.</p>

#### `Token`

``` purescript
newtype Token
  = Token String
```

#### `UnauthorizedOperation`

``` purescript
newtype UnauthorizedOperation
  = UnauthorizedOperation { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Exception raised to indicate a request was not authorized when the <code>DryRun</code> flag is set to "true".</p>

#### `UpdateDateTime`

``` purescript
newtype UpdateDateTime
  = UpdateDateTime Number
```


