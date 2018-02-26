## Module AWS.Glue

<fullname>AWS Glue</fullname> <p>Defines the public endpoint for the AWS Glue service.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchCreatePartition`

``` purescript
batchCreatePartition :: forall eff. BatchCreatePartitionRequest -> Aff (err :: RequestError | eff) BatchCreatePartitionResponse
```

<p>Creates one or more partitions in a batch operation.</p>

#### `batchDeleteConnection`

``` purescript
batchDeleteConnection :: forall eff. BatchDeleteConnectionRequest -> Aff (err :: RequestError | eff) BatchDeleteConnectionResponse
```

<p>Deletes a list of connection definitions from the Data Catalog.</p>

#### `batchDeletePartition`

``` purescript
batchDeletePartition :: forall eff. BatchDeletePartitionRequest -> Aff (err :: RequestError | eff) BatchDeletePartitionResponse
```

<p>Deletes one or more partitions in a batch operation.</p>

#### `batchDeleteTable`

``` purescript
batchDeleteTable :: forall eff. BatchDeleteTableRequest -> Aff (err :: RequestError | eff) BatchDeleteTableResponse
```

<p>Deletes multiple tables at once.</p>

#### `batchDeleteTableVersion`

``` purescript
batchDeleteTableVersion :: forall eff. BatchDeleteTableVersionRequest -> Aff (err :: RequestError | eff) BatchDeleteTableVersionResponse
```

<p>Deletes a specified batch of versions of a table.</p>

#### `batchGetPartition`

``` purescript
batchGetPartition :: forall eff. BatchGetPartitionRequest -> Aff (err :: RequestError | eff) BatchGetPartitionResponse
```

<p>Retrieves partitions in a batch request.</p>

#### `batchStopJobRun`

``` purescript
batchStopJobRun :: forall eff. BatchStopJobRunRequest -> Aff (err :: RequestError | eff) BatchStopJobRunResponse
```

<p>Stops one or more job runs for a specified Job.</p>

#### `createClassifier`

``` purescript
createClassifier :: forall eff. CreateClassifierRequest -> Aff (err :: RequestError | eff) CreateClassifierResponse
```

<p>Creates a classifier in the user's account. This may be a <code>GrokClassifier</code>, an <code>XMLClassifier</code>, or abbrev <code>JsonClassifier</code>, depending on which field of the request is present.</p>

#### `createConnection`

``` purescript
createConnection :: forall eff. CreateConnectionRequest -> Aff (err :: RequestError | eff) CreateConnectionResponse
```

<p>Creates a connection definition in the Data Catalog.</p>

#### `createCrawler`

``` purescript
createCrawler :: forall eff. CreateCrawlerRequest -> Aff (err :: RequestError | eff) CreateCrawlerResponse
```

<p>Creates a new crawler with specified targets, role, configuration, and optional schedule. At least one crawl target must be specified, in either the <i>s3Targets</i> or the <i>jdbcTargets</i> field.</p>

#### `createDatabase`

``` purescript
createDatabase :: forall eff. CreateDatabaseRequest -> Aff (err :: RequestError | eff) CreateDatabaseResponse
```

<p>Creates a new database in a Data Catalog.</p>

#### `createDevEndpoint`

``` purescript
createDevEndpoint :: forall eff. CreateDevEndpointRequest -> Aff (err :: RequestError | eff) CreateDevEndpointResponse
```

<p>Creates a new DevEndpoint.</p>

#### `createJob`

``` purescript
createJob :: forall eff. CreateJobRequest -> Aff (err :: RequestError | eff) CreateJobResponse
```

<p>Creates a new job.</p>

#### `createPartition`

``` purescript
createPartition :: forall eff. CreatePartitionRequest -> Aff (err :: RequestError | eff) CreatePartitionResponse
```

<p>Creates a new partition.</p>

#### `createScript`

``` purescript
createScript :: forall eff. CreateScriptRequest -> Aff (err :: RequestError | eff) CreateScriptResponse
```

<p>Transforms a directed acyclic graph (DAG) into code.</p>

#### `createTable`

``` purescript
createTable :: forall eff. CreateTableRequest -> Aff (err :: RequestError | eff) CreateTableResponse
```

<p>Creates a new table definition in the Data Catalog.</p>

#### `createTrigger`

``` purescript
createTrigger :: forall eff. CreateTriggerRequest -> Aff (err :: RequestError | eff) CreateTriggerResponse
```

<p>Creates a new trigger.</p>

#### `createUserDefinedFunction`

``` purescript
createUserDefinedFunction :: forall eff. CreateUserDefinedFunctionRequest -> Aff (err :: RequestError | eff) CreateUserDefinedFunctionResponse
```

<p>Creates a new function definition in the Data Catalog.</p>

#### `deleteClassifier`

``` purescript
deleteClassifier :: forall eff. DeleteClassifierRequest -> Aff (err :: RequestError | eff) DeleteClassifierResponse
```

<p>Removes a classifier from the Data Catalog.</p>

#### `deleteConnection`

``` purescript
deleteConnection :: forall eff. DeleteConnectionRequest -> Aff (err :: RequestError | eff) DeleteConnectionResponse
```

<p>Deletes a connection from the Data Catalog.</p>

#### `deleteCrawler`

``` purescript
deleteCrawler :: forall eff. DeleteCrawlerRequest -> Aff (err :: RequestError | eff) DeleteCrawlerResponse
```

<p>Removes a specified crawler from the Data Catalog, unless the crawler state is <code>RUNNING</code>.</p>

#### `deleteDatabase`

``` purescript
deleteDatabase :: forall eff. DeleteDatabaseRequest -> Aff (err :: RequestError | eff) DeleteDatabaseResponse
```

<p>Removes a specified Database from a Data Catalog.</p>

#### `deleteDevEndpoint`

``` purescript
deleteDevEndpoint :: forall eff. DeleteDevEndpointRequest -> Aff (err :: RequestError | eff) DeleteDevEndpointResponse
```

<p>Deletes a specified DevEndpoint.</p>

#### `deleteJob`

``` purescript
deleteJob :: forall eff. DeleteJobRequest -> Aff (err :: RequestError | eff) DeleteJobResponse
```

<p>Deletes a specified job. If the job is not found, no exception is thrown.</p>

#### `deletePartition`

``` purescript
deletePartition :: forall eff. DeletePartitionRequest -> Aff (err :: RequestError | eff) DeletePartitionResponse
```

<p>Deletes a specified partition.</p>

#### `deleteTable`

``` purescript
deleteTable :: forall eff. DeleteTableRequest -> Aff (err :: RequestError | eff) DeleteTableResponse
```

<p>Removes a table definition from the Data Catalog.</p>

#### `deleteTableVersion`

``` purescript
deleteTableVersion :: forall eff. DeleteTableVersionRequest -> Aff (err :: RequestError | eff) DeleteTableVersionResponse
```

<p>Deletes a specified version of a table.</p>

#### `deleteTrigger`

``` purescript
deleteTrigger :: forall eff. DeleteTriggerRequest -> Aff (err :: RequestError | eff) DeleteTriggerResponse
```

<p>Deletes a specified trigger. If the trigger is not found, no exception is thrown.</p>

#### `deleteUserDefinedFunction`

``` purescript
deleteUserDefinedFunction :: forall eff. DeleteUserDefinedFunctionRequest -> Aff (err :: RequestError | eff) DeleteUserDefinedFunctionResponse
```

<p>Deletes an existing function definition from the Data Catalog.</p>

#### `getCatalogImportStatus`

``` purescript
getCatalogImportStatus :: forall eff. GetCatalogImportStatusRequest -> Aff (err :: RequestError | eff) GetCatalogImportStatusResponse
```

<p>Retrieves the status of a migration operation.</p>

#### `getClassifier`

``` purescript
getClassifier :: forall eff. GetClassifierRequest -> Aff (err :: RequestError | eff) GetClassifierResponse
```

<p>Retrieve a classifier by name.</p>

#### `getClassifiers`

``` purescript
getClassifiers :: forall eff. GetClassifiersRequest -> Aff (err :: RequestError | eff) GetClassifiersResponse
```

<p>Lists all classifier objects in the Data Catalog.</p>

#### `getConnection`

``` purescript
getConnection :: forall eff. GetConnectionRequest -> Aff (err :: RequestError | eff) GetConnectionResponse
```

<p>Retrieves a connection definition from the Data Catalog.</p>

#### `getConnections`

``` purescript
getConnections :: forall eff. GetConnectionsRequest -> Aff (err :: RequestError | eff) GetConnectionsResponse
```

<p>Retrieves a list of connection definitions from the Data Catalog.</p>

#### `getCrawler`

``` purescript
getCrawler :: forall eff. GetCrawlerRequest -> Aff (err :: RequestError | eff) GetCrawlerResponse
```

<p>Retrieves metadata for a specified crawler.</p>

#### `getCrawlerMetrics`

``` purescript
getCrawlerMetrics :: forall eff. GetCrawlerMetricsRequest -> Aff (err :: RequestError | eff) GetCrawlerMetricsResponse
```

<p>Retrieves metrics about specified crawlers.</p>

#### `getCrawlers`

``` purescript
getCrawlers :: forall eff. GetCrawlersRequest -> Aff (err :: RequestError | eff) GetCrawlersResponse
```

<p>Retrieves metadata for all crawlers defined in the customer account.</p>

#### `getDatabase`

``` purescript
getDatabase :: forall eff. GetDatabaseRequest -> Aff (err :: RequestError | eff) GetDatabaseResponse
```

<p>Retrieves the definition of a specified database.</p>

#### `getDatabases`

``` purescript
getDatabases :: forall eff. GetDatabasesRequest -> Aff (err :: RequestError | eff) GetDatabasesResponse
```

<p>Retrieves all Databases defined in a given Data Catalog.</p>

#### `getDataflowGraph`

``` purescript
getDataflowGraph :: forall eff. GetDataflowGraphRequest -> Aff (err :: RequestError | eff) GetDataflowGraphResponse
```

<p>Transforms a Python script into a directed acyclic graph (DAG). </p>

#### `getDevEndpoint`

``` purescript
getDevEndpoint :: forall eff. GetDevEndpointRequest -> Aff (err :: RequestError | eff) GetDevEndpointResponse
```

<p>Retrieves information about a specified DevEndpoint.</p>

#### `getDevEndpoints`

``` purescript
getDevEndpoints :: forall eff. GetDevEndpointsRequest -> Aff (err :: RequestError | eff) GetDevEndpointsResponse
```

<p>Retrieves all the DevEndpoints in this AWS account.</p>

#### `getJob`

``` purescript
getJob :: forall eff. GetJobRequest -> Aff (err :: RequestError | eff) GetJobResponse
```

<p>Retrieves an existing job definition.</p>

#### `getJobRun`

``` purescript
getJobRun :: forall eff. GetJobRunRequest -> Aff (err :: RequestError | eff) GetJobRunResponse
```

<p>Retrieves the metadata for a given job run.</p>

#### `getJobRuns`

``` purescript
getJobRuns :: forall eff. GetJobRunsRequest -> Aff (err :: RequestError | eff) GetJobRunsResponse
```

<p>Retrieves metadata for all runs of a given job.</p>

#### `getJobs`

``` purescript
getJobs :: forall eff. GetJobsRequest -> Aff (err :: RequestError | eff) GetJobsResponse
```

<p>Retrieves all current jobs.</p>

#### `getMapping`

``` purescript
getMapping :: forall eff. GetMappingRequest -> Aff (err :: RequestError | eff) GetMappingResponse
```

<p>Creates mappings.</p>

#### `getPartition`

``` purescript
getPartition :: forall eff. GetPartitionRequest -> Aff (err :: RequestError | eff) GetPartitionResponse
```

<p>Retrieves information about a specified partition.</p>

#### `getPartitions`

``` purescript
getPartitions :: forall eff. GetPartitionsRequest -> Aff (err :: RequestError | eff) GetPartitionsResponse
```

<p>Retrieves information about the partitions in a table.</p>

#### `getPlan`

``` purescript
getPlan :: forall eff. GetPlanRequest -> Aff (err :: RequestError | eff) GetPlanResponse
```

<p>Gets code to perform a specified mapping.</p>

#### `getTable`

``` purescript
getTable :: forall eff. GetTableRequest -> Aff (err :: RequestError | eff) GetTableResponse
```

<p>Retrieves the <code>Table</code> definition in a Data Catalog for a specified table.</p>

#### `getTableVersion`

``` purescript
getTableVersion :: forall eff. GetTableVersionRequest -> Aff (err :: RequestError | eff) GetTableVersionResponse
```

<p>Retrieves a specified version of a table.</p>

#### `getTableVersions`

``` purescript
getTableVersions :: forall eff. GetTableVersionsRequest -> Aff (err :: RequestError | eff) GetTableVersionsResponse
```

<p>Retrieves a list of strings that identify available versions of a specified table.</p>

#### `getTables`

``` purescript
getTables :: forall eff. GetTablesRequest -> Aff (err :: RequestError | eff) GetTablesResponse
```

<p>Retrieves the definitions of some or all of the tables in a given <code>Database</code>.</p>

#### `getTrigger`

``` purescript
getTrigger :: forall eff. GetTriggerRequest -> Aff (err :: RequestError | eff) GetTriggerResponse
```

<p>Retrieves the definition of a trigger.</p>

#### `getTriggers`

``` purescript
getTriggers :: forall eff. GetTriggersRequest -> Aff (err :: RequestError | eff) GetTriggersResponse
```

<p>Gets all the triggers associated with a job.</p>

#### `getUserDefinedFunction`

``` purescript
getUserDefinedFunction :: forall eff. GetUserDefinedFunctionRequest -> Aff (err :: RequestError | eff) GetUserDefinedFunctionResponse
```

<p>Retrieves a specified function definition from the Data Catalog.</p>

#### `getUserDefinedFunctions`

``` purescript
getUserDefinedFunctions :: forall eff. GetUserDefinedFunctionsRequest -> Aff (err :: RequestError | eff) GetUserDefinedFunctionsResponse
```

<p>Retrieves a multiple function definitions from the Data Catalog.</p>

#### `importCatalogToGlue`

``` purescript
importCatalogToGlue :: forall eff. ImportCatalogToGlueRequest -> Aff (err :: RequestError | eff) ImportCatalogToGlueResponse
```

<p>Imports an existing Athena Data Catalog to AWS Glue</p>

#### `resetJobBookmark`

``` purescript
resetJobBookmark :: forall eff. ResetJobBookmarkRequest -> Aff (err :: RequestError | eff) ResetJobBookmarkResponse
```

<p>Resets a bookmark entry.</p>

#### `startCrawler`

``` purescript
startCrawler :: forall eff. StartCrawlerRequest -> Aff (err :: RequestError | eff) StartCrawlerResponse
```

<p>Starts a crawl using the specified crawler, regardless of what is scheduled. If the crawler is already running, does nothing.</p>

#### `startCrawlerSchedule`

``` purescript
startCrawlerSchedule :: forall eff. StartCrawlerScheduleRequest -> Aff (err :: RequestError | eff) StartCrawlerScheduleResponse
```

<p>Changes the schedule state of the specified crawler to <code>SCHEDULED</code>, unless the crawler is already running or the schedule state is already <code>SCHEDULED</code>.</p>

#### `startJobRun`

``` purescript
startJobRun :: forall eff. StartJobRunRequest -> Aff (err :: RequestError | eff) StartJobRunResponse
```

<p>Runs a job.</p>

#### `startTrigger`

``` purescript
startTrigger :: forall eff. StartTriggerRequest -> Aff (err :: RequestError | eff) StartTriggerResponse
```

<p>Starts an existing trigger. See <a href="http://docs.aws.amazon.com/glue/latest/dg/trigger-job.html">Triggering Jobs</a> for information about how different types of trigger are started.</p>

#### `stopCrawler`

``` purescript
stopCrawler :: forall eff. StopCrawlerRequest -> Aff (err :: RequestError | eff) StopCrawlerResponse
```

<p>If the specified crawler is running, stops the crawl.</p>

#### `stopCrawlerSchedule`

``` purescript
stopCrawlerSchedule :: forall eff. StopCrawlerScheduleRequest -> Aff (err :: RequestError | eff) StopCrawlerScheduleResponse
```

<p>Sets the schedule state of the specified crawler to <code>NOT_SCHEDULED</code>, but does not stop the crawler if it is already running.</p>

#### `stopTrigger`

``` purescript
stopTrigger :: forall eff. StopTriggerRequest -> Aff (err :: RequestError | eff) StopTriggerResponse
```

<p>Stops a specified trigger.</p>

#### `updateClassifier`

``` purescript
updateClassifier :: forall eff. UpdateClassifierRequest -> Aff (err :: RequestError | eff) UpdateClassifierResponse
```

<p>Modifies an existing classifier (a <code>GrokClassifier</code>, <code>XMLClassifier</code>, or <code>JsonClassifier</code>, depending on which field is present).</p>

#### `updateConnection`

``` purescript
updateConnection :: forall eff. UpdateConnectionRequest -> Aff (err :: RequestError | eff) UpdateConnectionResponse
```

<p>Updates a connection definition in the Data Catalog.</p>

#### `updateCrawler`

``` purescript
updateCrawler :: forall eff. UpdateCrawlerRequest -> Aff (err :: RequestError | eff) UpdateCrawlerResponse
```

<p>Updates a crawler. If a crawler is running, you must stop it using <code>StopCrawler</code> before updating it.</p>

#### `updateCrawlerSchedule`

``` purescript
updateCrawlerSchedule :: forall eff. UpdateCrawlerScheduleRequest -> Aff (err :: RequestError | eff) UpdateCrawlerScheduleResponse
```

<p>Updates the schedule of a crawler using a <code>cron</code> expression. </p>

#### `updateDatabase`

``` purescript
updateDatabase :: forall eff. UpdateDatabaseRequest -> Aff (err :: RequestError | eff) UpdateDatabaseResponse
```

<p>Updates an existing database definition in a Data Catalog.</p>

#### `updateDevEndpoint`

``` purescript
updateDevEndpoint :: forall eff. UpdateDevEndpointRequest -> Aff (err :: RequestError | eff) UpdateDevEndpointResponse
```

<p>Updates a specified DevEndpoint.</p>

#### `updateJob`

``` purescript
updateJob :: forall eff. UpdateJobRequest -> Aff (err :: RequestError | eff) UpdateJobResponse
```

<p>Updates an existing job definition.</p>

#### `updatePartition`

``` purescript
updatePartition :: forall eff. UpdatePartitionRequest -> Aff (err :: RequestError | eff) UpdatePartitionResponse
```

<p>Updates a partition.</p>

#### `updateTable`

``` purescript
updateTable :: forall eff. UpdateTableRequest -> Aff (err :: RequestError | eff) UpdateTableResponse
```

<p>Updates a metadata table in the Data Catalog.</p>

#### `updateTrigger`

``` purescript
updateTrigger :: forall eff. UpdateTriggerRequest -> Aff (err :: RequestError | eff) UpdateTriggerResponse
```

<p>Updates a trigger definition.</p>

#### `updateUserDefinedFunction`

``` purescript
updateUserDefinedFunction :: forall eff. UpdateUserDefinedFunctionRequest -> Aff (err :: RequestError | eff) UpdateUserDefinedFunctionResponse
```

<p>Updates an existing function definition in the Data Catalog.</p>

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException { "Message" :: NullOrUndefined (MessageString) }
```

<p>Access to a resource was denied.</p>

#### `Action`

``` purescript
newtype Action
  = Action { "JobName" :: NullOrUndefined (NameString), "Arguments" :: NullOrUndefined (GenericMap) }
```

<p>Defines an action to be initiated by a trigger.</p>

#### `ActionList`

``` purescript
newtype ActionList
  = ActionList (Array Action)
```

#### `AlreadyExistsException`

``` purescript
newtype AlreadyExistsException
  = AlreadyExistsException { "Message" :: NullOrUndefined (MessageString) }
```

<p>A resource to be created or added already exists.</p>

#### `AttemptCount`

``` purescript
newtype AttemptCount
  = AttemptCount Int
```

#### `BatchCreatePartitionRequest`

``` purescript
newtype BatchCreatePartitionRequest
  = BatchCreatePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionInputList" :: PartitionInputList }
```

#### `BatchCreatePartitionResponse`

``` purescript
newtype BatchCreatePartitionResponse
  = BatchCreatePartitionResponse { "Errors" :: NullOrUndefined (PartitionErrors) }
```

#### `BatchDeleteConnectionRequest`

``` purescript
newtype BatchDeleteConnectionRequest
  = BatchDeleteConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "ConnectionNameList" :: DeleteConnectionNameList }
```

#### `BatchDeleteConnectionResponse`

``` purescript
newtype BatchDeleteConnectionResponse
  = BatchDeleteConnectionResponse { "Succeeded" :: NullOrUndefined (NameStringList), "Errors" :: NullOrUndefined (ErrorByName) }
```

#### `BatchDeletePartitionRequest`

``` purescript
newtype BatchDeletePartitionRequest
  = BatchDeletePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionsToDelete" :: BatchDeletePartitionValueList }
```

#### `BatchDeletePartitionResponse`

``` purescript
newtype BatchDeletePartitionResponse
  = BatchDeletePartitionResponse { "Errors" :: NullOrUndefined (PartitionErrors) }
```

#### `BatchDeletePartitionValueList`

``` purescript
newtype BatchDeletePartitionValueList
  = BatchDeletePartitionValueList (Array PartitionValueList)
```

#### `BatchDeleteTableNameList`

``` purescript
newtype BatchDeleteTableNameList
  = BatchDeleteTableNameList (Array NameString)
```

#### `BatchDeleteTableRequest`

``` purescript
newtype BatchDeleteTableRequest
  = BatchDeleteTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TablesToDelete" :: BatchDeleteTableNameList }
```

#### `BatchDeleteTableResponse`

``` purescript
newtype BatchDeleteTableResponse
  = BatchDeleteTableResponse { "Errors" :: NullOrUndefined (TableErrors) }
```

#### `BatchDeleteTableVersionList`

``` purescript
newtype BatchDeleteTableVersionList
  = BatchDeleteTableVersionList (Array VersionString)
```

#### `BatchDeleteTableVersionRequest`

``` purescript
newtype BatchDeleteTableVersionRequest
  = BatchDeleteTableVersionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "VersionIds" :: BatchDeleteTableVersionList }
```

#### `BatchDeleteTableVersionResponse`

``` purescript
newtype BatchDeleteTableVersionResponse
  = BatchDeleteTableVersionResponse { "Errors" :: NullOrUndefined (TableVersionErrors) }
```

#### `BatchGetPartitionRequest`

``` purescript
newtype BatchGetPartitionRequest
  = BatchGetPartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionsToGet" :: BatchGetPartitionValueList }
```

#### `BatchGetPartitionResponse`

``` purescript
newtype BatchGetPartitionResponse
  = BatchGetPartitionResponse { "Partitions" :: NullOrUndefined (PartitionList), "UnprocessedKeys" :: NullOrUndefined (BatchGetPartitionValueList) }
```

#### `BatchGetPartitionValueList`

``` purescript
newtype BatchGetPartitionValueList
  = BatchGetPartitionValueList (Array PartitionValueList)
```

#### `BatchStopJobRunError`

``` purescript
newtype BatchStopJobRunError
  = BatchStopJobRunError { "JobName" :: NullOrUndefined (NameString), "JobRunId" :: NullOrUndefined (IdString), "ErrorDetail" :: NullOrUndefined (ErrorDetail) }
```

<p>Records an error that occurred when attempting to stop a specified JobRun.</p>

#### `BatchStopJobRunErrorList`

``` purescript
newtype BatchStopJobRunErrorList
  = BatchStopJobRunErrorList (Array BatchStopJobRunError)
```

#### `BatchStopJobRunJobRunIdList`

``` purescript
newtype BatchStopJobRunJobRunIdList
  = BatchStopJobRunJobRunIdList (Array IdString)
```

#### `BatchStopJobRunRequest`

``` purescript
newtype BatchStopJobRunRequest
  = BatchStopJobRunRequest { "JobName" :: NameString, "JobRunIds" :: BatchStopJobRunJobRunIdList }
```

#### `BatchStopJobRunResponse`

``` purescript
newtype BatchStopJobRunResponse
  = BatchStopJobRunResponse { "SuccessfulSubmissions" :: NullOrUndefined (BatchStopJobRunSuccessfulSubmissionList), "Errors" :: NullOrUndefined (BatchStopJobRunErrorList) }
```

#### `BatchStopJobRunSuccessfulSubmission`

``` purescript
newtype BatchStopJobRunSuccessfulSubmission
  = BatchStopJobRunSuccessfulSubmission { "JobName" :: NullOrUndefined (NameString), "JobRunId" :: NullOrUndefined (IdString) }
```

<p>Records a successful request to stop a specified JobRun.</p>

#### `BatchStopJobRunSuccessfulSubmissionList`

``` purescript
newtype BatchStopJobRunSuccessfulSubmissionList
  = BatchStopJobRunSuccessfulSubmissionList (Array BatchStopJobRunSuccessfulSubmission)
```

#### `BooleanNullable`

``` purescript
newtype BooleanNullable
  = BooleanNullable Boolean
```

#### `BooleanValue`

``` purescript
newtype BooleanValue
  = BooleanValue Boolean
```

#### `BoundedPartitionValueList`

``` purescript
newtype BoundedPartitionValueList
  = BoundedPartitionValueList (Array ValueString)
```

#### `CatalogEntries`

``` purescript
newtype CatalogEntries
  = CatalogEntries (Array CatalogEntry)
```

#### `CatalogEntry`

``` purescript
newtype CatalogEntry
  = CatalogEntry { "DatabaseName" :: NameString, "TableName" :: NameString }
```

<p>Specifies a table definition in the Data Catalog.</p>

#### `CatalogIdString`

``` purescript
newtype CatalogIdString
  = CatalogIdString String
```

#### `CatalogImportStatus`

``` purescript
newtype CatalogImportStatus
  = CatalogImportStatus { "ImportCompleted" :: NullOrUndefined (Boolean), "ImportTime" :: NullOrUndefined (Number), "ImportedBy" :: NullOrUndefined (NameString) }
```

<p>A structure containing migration status information.</p>

#### `Classification`

``` purescript
newtype Classification
  = Classification String
```

#### `Classifier`

``` purescript
newtype Classifier
  = Classifier { "GrokClassifier" :: NullOrUndefined (GrokClassifier), "XMLClassifier" :: NullOrUndefined (XMLClassifier), "JsonClassifier" :: NullOrUndefined (JsonClassifier) }
```

<p>Classifiers are written in Python and triggered during a crawl task. You can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier checks whether a given file is in a format it can handle, and if it is, the classifier creates a schema in the form of a <code>StructType</code> object that matches that data format.</p> <p>A classifier can be a <code>grok</code> classifier, an XML classifier, or a JSON classifier, asspecified in one of the fields in the <code>Classifier</code> object.</p>

#### `ClassifierList`

``` purescript
newtype ClassifierList
  = ClassifierList (Array Classifier)
```

#### `ClassifierNameList`

``` purescript
newtype ClassifierNameList
  = ClassifierNameList (Array NameString)
```

#### `CodeGenArgName`

``` purescript
newtype CodeGenArgName
  = CodeGenArgName String
```

#### `CodeGenArgValue`

``` purescript
newtype CodeGenArgValue
  = CodeGenArgValue String
```

#### `CodeGenEdge`

``` purescript
newtype CodeGenEdge
  = CodeGenEdge { "Source" :: CodeGenIdentifier, "Target" :: CodeGenIdentifier, "TargetParameter" :: NullOrUndefined (CodeGenArgName) }
```

<p>Represents a directional edge in a directed acyclic graph (DAG).</p>

#### `CodeGenIdentifier`

``` purescript
newtype CodeGenIdentifier
  = CodeGenIdentifier String
```

#### `CodeGenNode`

``` purescript
newtype CodeGenNode
  = CodeGenNode { "Id" :: CodeGenIdentifier, "NodeType" :: CodeGenNodeType, "Args" :: CodeGenNodeArgs, "LineNumber" :: NullOrUndefined (Int) }
```

<p>Represents a node in a directed acyclic graph (DAG)</p>

#### `CodeGenNodeArg`

``` purescript
newtype CodeGenNodeArg
  = CodeGenNodeArg { "Name" :: CodeGenArgName, "Value" :: CodeGenArgValue, "Param" :: NullOrUndefined (Boolean) }
```

<p>An argument or property of a node.</p>

#### `CodeGenNodeArgs`

``` purescript
newtype CodeGenNodeArgs
  = CodeGenNodeArgs (Array CodeGenNodeArg)
```

#### `CodeGenNodeType`

``` purescript
newtype CodeGenNodeType
  = CodeGenNodeType String
```

#### `Column`

``` purescript
newtype Column
  = Column { "Name" :: NameString, "Type" :: NullOrUndefined (ColumnTypeString), "Comment" :: NullOrUndefined (CommentString) }
```

<p>A column in a <code>Table</code>.</p>

#### `ColumnList`

``` purescript
newtype ColumnList
  = ColumnList (Array Column)
```

#### `ColumnTypeString`

``` purescript
newtype ColumnTypeString
  = ColumnTypeString String
```

#### `ColumnValueStringList`

``` purescript
newtype ColumnValueStringList
  = ColumnValueStringList (Array ColumnValuesString)
```

#### `ColumnValuesString`

``` purescript
newtype ColumnValuesString
  = ColumnValuesString String
```

#### `CommentString`

``` purescript
newtype CommentString
  = CommentString String
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message" :: NullOrUndefined (MessageString) }
```

<p>Two processes are trying to modify a resource simultaneously.</p>

#### `ConcurrentRunsExceededException`

``` purescript
newtype ConcurrentRunsExceededException
  = ConcurrentRunsExceededException { "Message" :: NullOrUndefined (MessageString) }
```

<p>Too many jobs are being run concurrently.</p>

#### `Condition`

``` purescript
newtype Condition
  = Condition { "LogicalOperator" :: NullOrUndefined (LogicalOperator), "JobName" :: NullOrUndefined (NameString), "State" :: NullOrUndefined (JobRunState) }
```

<p>Defines a condition under which a trigger fires.</p>

#### `ConditionList`

``` purescript
newtype ConditionList
  = ConditionList (Array Condition)
```

#### `Connection`

``` purescript
newtype Connection
  = Connection { "Name" :: NullOrUndefined (NameString), "Description" :: NullOrUndefined (DescriptionString), "ConnectionType" :: NullOrUndefined (ConnectionType), "MatchCriteria" :: NullOrUndefined (MatchCriteria), "ConnectionProperties" :: NullOrUndefined (ConnectionProperties), "PhysicalConnectionRequirements" :: NullOrUndefined (PhysicalConnectionRequirements), "CreationTime" :: NullOrUndefined (Number), "LastUpdatedTime" :: NullOrUndefined (Number), "LastUpdatedBy" :: NullOrUndefined (NameString) }
```

<p>Defines a connection to a data source.</p>

#### `ConnectionInput`

``` purescript
newtype ConnectionInput
  = ConnectionInput { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "ConnectionType" :: ConnectionType, "MatchCriteria" :: NullOrUndefined (MatchCriteria), "ConnectionProperties" :: ConnectionProperties, "PhysicalConnectionRequirements" :: NullOrUndefined (PhysicalConnectionRequirements) }
```

<p>A structure used to specify a connection to create or update.</p>

#### `ConnectionList`

``` purescript
newtype ConnectionList
  = ConnectionList (Array Connection)
```

#### `ConnectionName`

``` purescript
newtype ConnectionName
  = ConnectionName String
```

#### `ConnectionProperties`

``` purescript
newtype ConnectionProperties
  = ConnectionProperties (Map ConnectionPropertyKey ValueString)
```

#### `ConnectionPropertyKey`

``` purescript
newtype ConnectionPropertyKey
  = ConnectionPropertyKey String
```

#### `ConnectionType`

``` purescript
newtype ConnectionType
  = ConnectionType String
```

#### `ConnectionsList`

``` purescript
newtype ConnectionsList
  = ConnectionsList { "Connections" :: NullOrUndefined (StringList) }
```

<p>Specifies the connections used by a job.</p>

#### `Crawler`

``` purescript
newtype Crawler
  = Crawler { "Name" :: NullOrUndefined (NameString), "Role" :: NullOrUndefined (Role), "Targets" :: NullOrUndefined (CrawlerTargets), "DatabaseName" :: NullOrUndefined (DatabaseName), "Description" :: NullOrUndefined (DescriptionString), "Classifiers" :: NullOrUndefined (ClassifierNameList), "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy), "State" :: NullOrUndefined (CrawlerState), "TablePrefix" :: NullOrUndefined (TablePrefix), "Schedule" :: NullOrUndefined (Schedule), "CrawlElapsedTime" :: NullOrUndefined (MillisecondsCount), "CreationTime" :: NullOrUndefined (Number), "LastUpdated" :: NullOrUndefined (Number), "LastCrawl" :: NullOrUndefined (LastCrawlInfo), "Version" :: NullOrUndefined (VersionId), "Configuration" :: NullOrUndefined (CrawlerConfiguration) }
```

<p>Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.</p>

#### `CrawlerConfiguration`

``` purescript
newtype CrawlerConfiguration
  = CrawlerConfiguration String
```

#### `CrawlerList`

``` purescript
newtype CrawlerList
  = CrawlerList (Array Crawler)
```

#### `CrawlerMetrics`

``` purescript
newtype CrawlerMetrics
  = CrawlerMetrics { "CrawlerName" :: NullOrUndefined (NameString), "TimeLeftSeconds" :: NullOrUndefined (NonNegativeDouble), "StillEstimating" :: NullOrUndefined (Boolean), "LastRuntimeSeconds" :: NullOrUndefined (NonNegativeDouble), "MedianRuntimeSeconds" :: NullOrUndefined (NonNegativeDouble), "TablesCreated" :: NullOrUndefined (NonNegativeInteger), "TablesUpdated" :: NullOrUndefined (NonNegativeInteger), "TablesDeleted" :: NullOrUndefined (NonNegativeInteger) }
```

<p>Metrics for a specified crawler.</p>

#### `CrawlerMetricsList`

``` purescript
newtype CrawlerMetricsList
  = CrawlerMetricsList (Array CrawlerMetrics)
```

#### `CrawlerNameList`

``` purescript
newtype CrawlerNameList
  = CrawlerNameList (Array NameString)
```

#### `CrawlerNotRunningException`

``` purescript
newtype CrawlerNotRunningException
  = CrawlerNotRunningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified crawler is not running.</p>

#### `CrawlerRunningException`

``` purescript
newtype CrawlerRunningException
  = CrawlerRunningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The operation cannot be performed because the crawler is already running.</p>

#### `CrawlerState`

``` purescript
newtype CrawlerState
  = CrawlerState String
```

#### `CrawlerStoppingException`

``` purescript
newtype CrawlerStoppingException
  = CrawlerStoppingException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified crawler is stopping.</p>

#### `CrawlerTargets`

``` purescript
newtype CrawlerTargets
  = CrawlerTargets { "S3Targets" :: NullOrUndefined (S3TargetList), "JdbcTargets" :: NullOrUndefined (JdbcTargetList) }
```

<p>Specifies data stores to crawl.</p>

#### `CreateClassifierRequest`

``` purescript
newtype CreateClassifierRequest
  = CreateClassifierRequest { "GrokClassifier" :: NullOrUndefined (CreateGrokClassifierRequest), "XMLClassifier" :: NullOrUndefined (CreateXMLClassifierRequest), "JsonClassifier" :: NullOrUndefined (CreateJsonClassifierRequest) }
```

#### `CreateClassifierResponse`

``` purescript
newtype CreateClassifierResponse
  = CreateClassifierResponse {  }
```

#### `CreateConnectionRequest`

``` purescript
newtype CreateConnectionRequest
  = CreateConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "ConnectionInput" :: ConnectionInput }
```

#### `CreateConnectionResponse`

``` purescript
newtype CreateConnectionResponse
  = CreateConnectionResponse {  }
```

#### `CreateCrawlerRequest`

``` purescript
newtype CreateCrawlerRequest
  = CreateCrawlerRequest { "Name" :: NameString, "Role" :: Role, "DatabaseName" :: DatabaseName, "Description" :: NullOrUndefined (DescriptionString), "Targets" :: CrawlerTargets, "Schedule" :: NullOrUndefined (CronExpression), "Classifiers" :: NullOrUndefined (ClassifierNameList), "TablePrefix" :: NullOrUndefined (TablePrefix), "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy), "Configuration" :: NullOrUndefined (CrawlerConfiguration) }
```

#### `CreateCrawlerResponse`

``` purescript
newtype CreateCrawlerResponse
  = CreateCrawlerResponse {  }
```

#### `CreateDatabaseRequest`

``` purescript
newtype CreateDatabaseRequest
  = CreateDatabaseRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseInput" :: DatabaseInput }
```

#### `CreateDatabaseResponse`

``` purescript
newtype CreateDatabaseResponse
  = CreateDatabaseResponse {  }
```

#### `CreateDevEndpointRequest`

``` purescript
newtype CreateDevEndpointRequest
  = CreateDevEndpointRequest { "EndpointName" :: GenericString, "RoleArn" :: RoleArn, "SecurityGroupIds" :: NullOrUndefined (StringList), "SubnetId" :: NullOrUndefined (GenericString), "PublicKey" :: GenericString, "NumberOfNodes" :: NullOrUndefined (IntegerValue), "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString), "ExtraJarsS3Path" :: NullOrUndefined (GenericString) }
```

#### `CreateDevEndpointResponse`

``` purescript
newtype CreateDevEndpointResponse
  = CreateDevEndpointResponse { "EndpointName" :: NullOrUndefined (GenericString), "Status" :: NullOrUndefined (GenericString), "SecurityGroupIds" :: NullOrUndefined (StringList), "SubnetId" :: NullOrUndefined (GenericString), "RoleArn" :: NullOrUndefined (RoleArn), "YarnEndpointAddress" :: NullOrUndefined (GenericString), "ZeppelinRemoteSparkInterpreterPort" :: NullOrUndefined (IntegerValue), "NumberOfNodes" :: NullOrUndefined (IntegerValue), "AvailabilityZone" :: NullOrUndefined (GenericString), "VpcId" :: NullOrUndefined (GenericString), "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString), "ExtraJarsS3Path" :: NullOrUndefined (GenericString), "FailureReason" :: NullOrUndefined (GenericString), "CreatedTimestamp" :: NullOrUndefined (TimestampValue) }
```

#### `CreateGrokClassifierRequest`

``` purescript
newtype CreateGrokClassifierRequest
  = CreateGrokClassifierRequest { "Classification" :: Classification, "Name" :: NameString, "GrokPattern" :: GrokPattern, "CustomPatterns" :: NullOrUndefined (CustomPatterns) }
```

<p>Specifies a <code>grok</code> classifier for <code>CreateClassifier</code> to create.</p>

#### `CreateJobRequest`

``` purescript
newtype CreateJobRequest
  = CreateJobRequest { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "LogUri" :: NullOrUndefined (UriString), "Role" :: RoleString, "ExecutionProperty" :: NullOrUndefined (ExecutionProperty), "Command" :: JobCommand, "DefaultArguments" :: NullOrUndefined (GenericMap), "Connections" :: NullOrUndefined (ConnectionsList), "MaxRetries" :: NullOrUndefined (MaxRetries), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

#### `CreateJobResponse`

``` purescript
newtype CreateJobResponse
  = CreateJobResponse { "Name" :: NullOrUndefined (NameString) }
```

#### `CreateJsonClassifierRequest`

``` purescript
newtype CreateJsonClassifierRequest
  = CreateJsonClassifierRequest { "Name" :: NameString, "JsonPath" :: JsonPath }
```

<p>Specifies a JSON classifier for <code>CreateClassifier</code> to create.</p>

#### `CreatePartitionRequest`

``` purescript
newtype CreatePartitionRequest
  = CreatePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionInput" :: PartitionInput }
```

#### `CreatePartitionResponse`

``` purescript
newtype CreatePartitionResponse
  = CreatePartitionResponse {  }
```

#### `CreateScriptRequest`

``` purescript
newtype CreateScriptRequest
  = CreateScriptRequest { "DagNodes" :: NullOrUndefined (DagNodes), "DagEdges" :: NullOrUndefined (DagEdges), "Language" :: NullOrUndefined (Language) }
```

#### `CreateScriptResponse`

``` purescript
newtype CreateScriptResponse
  = CreateScriptResponse { "PythonScript" :: NullOrUndefined (PythonScript), "ScalaCode" :: NullOrUndefined (ScalaCode) }
```

#### `CreateTableRequest`

``` purescript
newtype CreateTableRequest
  = CreateTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableInput" :: TableInput }
```

#### `CreateTableResponse`

``` purescript
newtype CreateTableResponse
  = CreateTableResponse {  }
```

#### `CreateTriggerRequest`

``` purescript
newtype CreateTriggerRequest
  = CreateTriggerRequest { "Name" :: NameString, "Type" :: TriggerType, "Schedule" :: NullOrUndefined (GenericString), "Predicate" :: NullOrUndefined (Predicate), "Actions" :: ActionList, "Description" :: NullOrUndefined (DescriptionString) }
```

#### `CreateTriggerResponse`

``` purescript
newtype CreateTriggerResponse
  = CreateTriggerResponse { "Name" :: NullOrUndefined (NameString) }
```

#### `CreateUserDefinedFunctionRequest`

``` purescript
newtype CreateUserDefinedFunctionRequest
  = CreateUserDefinedFunctionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "FunctionInput" :: UserDefinedFunctionInput }
```

#### `CreateUserDefinedFunctionResponse`

``` purescript
newtype CreateUserDefinedFunctionResponse
  = CreateUserDefinedFunctionResponse {  }
```

#### `CreateXMLClassifierRequest`

``` purescript
newtype CreateXMLClassifierRequest
  = CreateXMLClassifierRequest { "Classification" :: Classification, "Name" :: NameString, "RowTag" :: NullOrUndefined (RowTag) }
```

<p>Specifies an XML classifier for <code>CreateClassifier</code> to create.</p>

#### `CronExpression`

``` purescript
newtype CronExpression
  = CronExpression String
```

#### `CustomPatterns`

``` purescript
newtype CustomPatterns
  = CustomPatterns String
```

#### `DagEdges`

``` purescript
newtype DagEdges
  = DagEdges (Array CodeGenEdge)
```

#### `DagNodes`

``` purescript
newtype DagNodes
  = DagNodes (Array CodeGenNode)
```

#### `Database`

``` purescript
newtype Database
  = Database { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "LocationUri" :: NullOrUndefined (URI), "Parameters" :: NullOrUndefined (ParametersMap), "CreateTime" :: NullOrUndefined (Number) }
```

<p>The <code>Database</code> object represents a logical grouping of tables that may reside in a Hive metastore or an RDBMS.</p>

#### `DatabaseInput`

``` purescript
newtype DatabaseInput
  = DatabaseInput { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "LocationUri" :: NullOrUndefined (URI), "Parameters" :: NullOrUndefined (ParametersMap) }
```

<p>The structure used to create or update a database.</p>

#### `DatabaseList`

``` purescript
newtype DatabaseList
  = DatabaseList (Array Database)
```

#### `DatabaseName`

``` purescript
newtype DatabaseName
  = DatabaseName String
```

#### `DeleteBehavior`

``` purescript
newtype DeleteBehavior
  = DeleteBehavior String
```

#### `DeleteClassifierRequest`

``` purescript
newtype DeleteClassifierRequest
  = DeleteClassifierRequest { "Name" :: NameString }
```

#### `DeleteClassifierResponse`

``` purescript
newtype DeleteClassifierResponse
  = DeleteClassifierResponse {  }
```

#### `DeleteConnectionNameList`

``` purescript
newtype DeleteConnectionNameList
  = DeleteConnectionNameList (Array NameString)
```

#### `DeleteConnectionRequest`

``` purescript
newtype DeleteConnectionRequest
  = DeleteConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "ConnectionName" :: NameString }
```

#### `DeleteConnectionResponse`

``` purescript
newtype DeleteConnectionResponse
  = DeleteConnectionResponse {  }
```

#### `DeleteCrawlerRequest`

``` purescript
newtype DeleteCrawlerRequest
  = DeleteCrawlerRequest { "Name" :: NameString }
```

#### `DeleteCrawlerResponse`

``` purescript
newtype DeleteCrawlerResponse
  = DeleteCrawlerResponse {  }
```

#### `DeleteDatabaseRequest`

``` purescript
newtype DeleteDatabaseRequest
  = DeleteDatabaseRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString }
```

#### `DeleteDatabaseResponse`

``` purescript
newtype DeleteDatabaseResponse
  = DeleteDatabaseResponse {  }
```

#### `DeleteDevEndpointRequest`

``` purescript
newtype DeleteDevEndpointRequest
  = DeleteDevEndpointRequest { "EndpointName" :: GenericString }
```

#### `DeleteDevEndpointResponse`

``` purescript
newtype DeleteDevEndpointResponse
  = DeleteDevEndpointResponse {  }
```

#### `DeleteJobRequest`

``` purescript
newtype DeleteJobRequest
  = DeleteJobRequest { "JobName" :: NameString }
```

#### `DeleteJobResponse`

``` purescript
newtype DeleteJobResponse
  = DeleteJobResponse { "JobName" :: NullOrUndefined (NameString) }
```

#### `DeletePartitionRequest`

``` purescript
newtype DeletePartitionRequest
  = DeletePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionValues" :: ValueStringList }
```

#### `DeletePartitionResponse`

``` purescript
newtype DeletePartitionResponse
  = DeletePartitionResponse {  }
```

#### `DeleteTableRequest`

``` purescript
newtype DeleteTableRequest
  = DeleteTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "Name" :: NameString }
```

#### `DeleteTableResponse`

``` purescript
newtype DeleteTableResponse
  = DeleteTableResponse {  }
```

#### `DeleteTableVersionRequest`

``` purescript
newtype DeleteTableVersionRequest
  = DeleteTableVersionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "VersionId" :: VersionString }
```

#### `DeleteTableVersionResponse`

``` purescript
newtype DeleteTableVersionResponse
  = DeleteTableVersionResponse {  }
```

#### `DeleteTriggerRequest`

``` purescript
newtype DeleteTriggerRequest
  = DeleteTriggerRequest { "Name" :: NameString }
```

#### `DeleteTriggerResponse`

``` purescript
newtype DeleteTriggerResponse
  = DeleteTriggerResponse { "Name" :: NullOrUndefined (NameString) }
```

#### `DeleteUserDefinedFunctionRequest`

``` purescript
newtype DeleteUserDefinedFunctionRequest
  = DeleteUserDefinedFunctionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "FunctionName" :: NameString }
```

#### `DeleteUserDefinedFunctionResponse`

``` purescript
newtype DeleteUserDefinedFunctionResponse
  = DeleteUserDefinedFunctionResponse {  }
```

#### `DescriptionString`

``` purescript
newtype DescriptionString
  = DescriptionString String
```

#### `DescriptionStringRemovable`

``` purescript
newtype DescriptionStringRemovable
  = DescriptionStringRemovable String
```

#### `DevEndpoint`

``` purescript
newtype DevEndpoint
  = DevEndpoint { "EndpointName" :: NullOrUndefined (GenericString), "RoleArn" :: NullOrUndefined (RoleArn), "SecurityGroupIds" :: NullOrUndefined (StringList), "SubnetId" :: NullOrUndefined (GenericString), "YarnEndpointAddress" :: NullOrUndefined (GenericString), "ZeppelinRemoteSparkInterpreterPort" :: NullOrUndefined (IntegerValue), "PublicAddress" :: NullOrUndefined (GenericString), "Status" :: NullOrUndefined (GenericString), "NumberOfNodes" :: NullOrUndefined (IntegerValue), "AvailabilityZone" :: NullOrUndefined (GenericString), "VpcId" :: NullOrUndefined (GenericString), "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString), "ExtraJarsS3Path" :: NullOrUndefined (GenericString), "FailureReason" :: NullOrUndefined (GenericString), "LastUpdateStatus" :: NullOrUndefined (GenericString), "CreatedTimestamp" :: NullOrUndefined (TimestampValue), "LastModifiedTimestamp" :: NullOrUndefined (TimestampValue), "PublicKey" :: NullOrUndefined (GenericString) }
```

<p>A development endpoint where a developer can remotely debug ETL scripts.</p>

#### `DevEndpointCustomLibraries`

``` purescript
newtype DevEndpointCustomLibraries
  = DevEndpointCustomLibraries { "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString), "ExtraJarsS3Path" :: NullOrUndefined (GenericString) }
```

<p>Custom libraries to be loaded into a DevEndpoint.</p>

#### `DevEndpointList`

``` purescript
newtype DevEndpointList
  = DevEndpointList (Array DevEndpoint)
```

#### `EntityNotFoundException`

``` purescript
newtype EntityNotFoundException
  = EntityNotFoundException { "Message" :: NullOrUndefined (MessageString) }
```

<p>A specified entity does not exist</p>

#### `ErrorByName`

``` purescript
newtype ErrorByName
  = ErrorByName (Map NameString ErrorDetail)
```

#### `ErrorDetail`

``` purescript
newtype ErrorDetail
  = ErrorDetail { "ErrorCode" :: NullOrUndefined (NameString), "ErrorMessage" :: NullOrUndefined (DescriptionString) }
```

<p>Contains details about an error.</p>

#### `ErrorString`

``` purescript
newtype ErrorString
  = ErrorString String
```

#### `ExecutionProperty`

``` purescript
newtype ExecutionProperty
  = ExecutionProperty { "MaxConcurrentRuns" :: NullOrUndefined (MaxConcurrentRuns) }
```

<p>An execution property of a job.</p>

#### `FieldType`

``` purescript
newtype FieldType
  = FieldType String
```

#### `FilterString`

``` purescript
newtype FilterString
  = FilterString String
```

#### `FormatString`

``` purescript
newtype FormatString
  = FormatString String
```

#### `GenericMap`

``` purescript
newtype GenericMap
  = GenericMap (Map GenericString GenericString)
```

#### `GenericString`

``` purescript
newtype GenericString
  = GenericString String
```

#### `GetCatalogImportStatusRequest`

``` purescript
newtype GetCatalogImportStatusRequest
  = GetCatalogImportStatusRequest { "CatalogId" :: NullOrUndefined (CatalogIdString) }
```

#### `GetCatalogImportStatusResponse`

``` purescript
newtype GetCatalogImportStatusResponse
  = GetCatalogImportStatusResponse { "ImportStatus" :: NullOrUndefined (CatalogImportStatus) }
```

#### `GetClassifierRequest`

``` purescript
newtype GetClassifierRequest
  = GetClassifierRequest { "Name" :: NameString }
```

#### `GetClassifierResponse`

``` purescript
newtype GetClassifierResponse
  = GetClassifierResponse { "Classifier" :: NullOrUndefined (Classifier) }
```

#### `GetClassifiersRequest`

``` purescript
newtype GetClassifiersRequest
  = GetClassifiersRequest { "MaxResults" :: NullOrUndefined (PageSize), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetClassifiersResponse`

``` purescript
newtype GetClassifiersResponse
  = GetClassifiersResponse { "Classifiers" :: NullOrUndefined (ClassifierList), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetConnectionRequest`

``` purescript
newtype GetConnectionRequest
  = GetConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString }
```

#### `GetConnectionResponse`

``` purescript
newtype GetConnectionResponse
  = GetConnectionResponse { "Connection" :: NullOrUndefined (Connection) }
```

#### `GetConnectionsFilter`

``` purescript
newtype GetConnectionsFilter
  = GetConnectionsFilter { "MatchCriteria" :: NullOrUndefined (MatchCriteria), "ConnectionType" :: NullOrUndefined (ConnectionType) }
```

<p>Filters the connection definitions returned by the <code>GetConnections</code> API.</p>

#### `GetConnectionsRequest`

``` purescript
newtype GetConnectionsRequest
  = GetConnectionsRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Filter" :: NullOrUndefined (GetConnectionsFilter), "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetConnectionsResponse`

``` purescript
newtype GetConnectionsResponse
  = GetConnectionsResponse { "ConnectionList" :: NullOrUndefined (ConnectionList), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetCrawlerMetricsRequest`

``` purescript
newtype GetCrawlerMetricsRequest
  = GetCrawlerMetricsRequest { "CrawlerNameList" :: NullOrUndefined (CrawlerNameList), "MaxResults" :: NullOrUndefined (PageSize), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetCrawlerMetricsResponse`

``` purescript
newtype GetCrawlerMetricsResponse
  = GetCrawlerMetricsResponse { "CrawlerMetricsList" :: NullOrUndefined (CrawlerMetricsList), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetCrawlerRequest`

``` purescript
newtype GetCrawlerRequest
  = GetCrawlerRequest { "Name" :: NameString }
```

#### `GetCrawlerResponse`

``` purescript
newtype GetCrawlerResponse
  = GetCrawlerResponse { "Crawler" :: NullOrUndefined (Crawler) }
```

#### `GetCrawlersRequest`

``` purescript
newtype GetCrawlersRequest
  = GetCrawlersRequest { "MaxResults" :: NullOrUndefined (PageSize), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetCrawlersResponse`

``` purescript
newtype GetCrawlersResponse
  = GetCrawlersResponse { "Crawlers" :: NullOrUndefined (CrawlerList), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetDatabaseRequest`

``` purescript
newtype GetDatabaseRequest
  = GetDatabaseRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString }
```

#### `GetDatabaseResponse`

``` purescript
newtype GetDatabaseResponse
  = GetDatabaseResponse { "Database" :: NullOrUndefined (Database) }
```

#### `GetDatabasesRequest`

``` purescript
newtype GetDatabasesRequest
  = GetDatabasesRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetDatabasesResponse`

``` purescript
newtype GetDatabasesResponse
  = GetDatabasesResponse { "DatabaseList" :: DatabaseList, "NextToken" :: NullOrUndefined (Token) }
```

#### `GetDataflowGraphRequest`

``` purescript
newtype GetDataflowGraphRequest
  = GetDataflowGraphRequest { "PythonScript" :: NullOrUndefined (PythonScript) }
```

#### `GetDataflowGraphResponse`

``` purescript
newtype GetDataflowGraphResponse
  = GetDataflowGraphResponse { "DagNodes" :: NullOrUndefined (DagNodes), "DagEdges" :: NullOrUndefined (DagEdges) }
```

#### `GetDevEndpointRequest`

``` purescript
newtype GetDevEndpointRequest
  = GetDevEndpointRequest { "EndpointName" :: GenericString }
```

#### `GetDevEndpointResponse`

``` purescript
newtype GetDevEndpointResponse
  = GetDevEndpointResponse { "DevEndpoint" :: NullOrUndefined (DevEndpoint) }
```

#### `GetDevEndpointsRequest`

``` purescript
newtype GetDevEndpointsRequest
  = GetDevEndpointsRequest { "MaxResults" :: NullOrUndefined (PageSize), "NextToken" :: NullOrUndefined (GenericString) }
```

#### `GetDevEndpointsResponse`

``` purescript
newtype GetDevEndpointsResponse
  = GetDevEndpointsResponse { "DevEndpoints" :: NullOrUndefined (DevEndpointList), "NextToken" :: NullOrUndefined (GenericString) }
```

#### `GetJobRequest`

``` purescript
newtype GetJobRequest
  = GetJobRequest { "JobName" :: NameString }
```

#### `GetJobResponse`

``` purescript
newtype GetJobResponse
  = GetJobResponse { "Job" :: NullOrUndefined (Job) }
```

#### `GetJobRunRequest`

``` purescript
newtype GetJobRunRequest
  = GetJobRunRequest { "JobName" :: NameString, "RunId" :: IdString, "PredecessorsIncluded" :: NullOrUndefined (BooleanValue) }
```

#### `GetJobRunResponse`

``` purescript
newtype GetJobRunResponse
  = GetJobRunResponse { "JobRun" :: NullOrUndefined (JobRun) }
```

#### `GetJobRunsRequest`

``` purescript
newtype GetJobRunsRequest
  = GetJobRunsRequest { "JobName" :: NameString, "NextToken" :: NullOrUndefined (GenericString), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetJobRunsResponse`

``` purescript
newtype GetJobRunsResponse
  = GetJobRunsResponse { "JobRuns" :: NullOrUndefined (JobRunList), "NextToken" :: NullOrUndefined (GenericString) }
```

#### `GetJobsRequest`

``` purescript
newtype GetJobsRequest
  = GetJobsRequest { "NextToken" :: NullOrUndefined (GenericString), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetJobsResponse`

``` purescript
newtype GetJobsResponse
  = GetJobsResponse { "Jobs" :: NullOrUndefined (JobList), "NextToken" :: NullOrUndefined (GenericString) }
```

#### `GetMappingRequest`

``` purescript
newtype GetMappingRequest
  = GetMappingRequest { "Source" :: CatalogEntry, "Sinks" :: NullOrUndefined (CatalogEntries), "Location" :: NullOrUndefined (Location) }
```

#### `GetMappingResponse`

``` purescript
newtype GetMappingResponse
  = GetMappingResponse { "Mapping" :: MappingList }
```

#### `GetPartitionRequest`

``` purescript
newtype GetPartitionRequest
  = GetPartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionValues" :: ValueStringList }
```

#### `GetPartitionResponse`

``` purescript
newtype GetPartitionResponse
  = GetPartitionResponse { "Partition" :: NullOrUndefined (Partition) }
```

#### `GetPartitionsRequest`

``` purescript
newtype GetPartitionsRequest
  = GetPartitionsRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "Expression" :: NullOrUndefined (PredicateString), "NextToken" :: NullOrUndefined (Token), "Segment" :: NullOrUndefined (Segment), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetPartitionsResponse`

``` purescript
newtype GetPartitionsResponse
  = GetPartitionsResponse { "Partitions" :: NullOrUndefined (PartitionList), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetPlanRequest`

``` purescript
newtype GetPlanRequest
  = GetPlanRequest { "Mapping" :: MappingList, "Source" :: CatalogEntry, "Sinks" :: NullOrUndefined (CatalogEntries), "Location" :: NullOrUndefined (Location), "Language" :: NullOrUndefined (Language) }
```

#### `GetPlanResponse`

``` purescript
newtype GetPlanResponse
  = GetPlanResponse { "PythonScript" :: NullOrUndefined (PythonScript), "ScalaCode" :: NullOrUndefined (ScalaCode) }
```

#### `GetTableRequest`

``` purescript
newtype GetTableRequest
  = GetTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "Name" :: NameString }
```

#### `GetTableResponse`

``` purescript
newtype GetTableResponse
  = GetTableResponse { "Table" :: NullOrUndefined (Table) }
```

#### `GetTableVersionRequest`

``` purescript
newtype GetTableVersionRequest
  = GetTableVersionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "VersionId" :: NullOrUndefined (VersionString) }
```

#### `GetTableVersionResponse`

``` purescript
newtype GetTableVersionResponse
  = GetTableVersionResponse { "TableVersion" :: NullOrUndefined (TableVersion) }
```

#### `GetTableVersionsList`

``` purescript
newtype GetTableVersionsList
  = GetTableVersionsList (Array TableVersion)
```

#### `GetTableVersionsRequest`

``` purescript
newtype GetTableVersionsRequest
  = GetTableVersionsRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetTableVersionsResponse`

``` purescript
newtype GetTableVersionsResponse
  = GetTableVersionsResponse { "TableVersions" :: NullOrUndefined (GetTableVersionsList), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetTablesRequest`

``` purescript
newtype GetTablesRequest
  = GetTablesRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "Expression" :: NullOrUndefined (FilterString), "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetTablesResponse`

``` purescript
newtype GetTablesResponse
  = GetTablesResponse { "TableList" :: NullOrUndefined (TableList), "NextToken" :: NullOrUndefined (Token) }
```

#### `GetTriggerRequest`

``` purescript
newtype GetTriggerRequest
  = GetTriggerRequest { "Name" :: NameString }
```

#### `GetTriggerResponse`

``` purescript
newtype GetTriggerResponse
  = GetTriggerResponse { "Trigger" :: NullOrUndefined (Trigger) }
```

#### `GetTriggersRequest`

``` purescript
newtype GetTriggersRequest
  = GetTriggersRequest { "NextToken" :: NullOrUndefined (GenericString), "DependentJobName" :: NullOrUndefined (NameString), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetTriggersResponse`

``` purescript
newtype GetTriggersResponse
  = GetTriggersResponse { "Triggers" :: NullOrUndefined (TriggerList), "NextToken" :: NullOrUndefined (GenericString) }
```

#### `GetUserDefinedFunctionRequest`

``` purescript
newtype GetUserDefinedFunctionRequest
  = GetUserDefinedFunctionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "FunctionName" :: NameString }
```

#### `GetUserDefinedFunctionResponse`

``` purescript
newtype GetUserDefinedFunctionResponse
  = GetUserDefinedFunctionResponse { "UserDefinedFunction" :: NullOrUndefined (UserDefinedFunction) }
```

#### `GetUserDefinedFunctionsRequest`

``` purescript
newtype GetUserDefinedFunctionsRequest
  = GetUserDefinedFunctionsRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "Pattern" :: NameString, "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

#### `GetUserDefinedFunctionsResponse`

``` purescript
newtype GetUserDefinedFunctionsResponse
  = GetUserDefinedFunctionsResponse { "UserDefinedFunctions" :: NullOrUndefined (UserDefinedFunctionList), "NextToken" :: NullOrUndefined (Token) }
```

#### `GrokClassifier`

``` purescript
newtype GrokClassifier
  = GrokClassifier { "Name" :: NameString, "Classification" :: Classification, "CreationTime" :: NullOrUndefined (Number), "LastUpdated" :: NullOrUndefined (Number), "Version" :: NullOrUndefined (VersionId), "GrokPattern" :: GrokPattern, "CustomPatterns" :: NullOrUndefined (CustomPatterns) }
```

<p>A classifier that uses <code>grok</code> patterns.</p>

#### `GrokPattern`

``` purescript
newtype GrokPattern
  = GrokPattern String
```

#### `IdString`

``` purescript
newtype IdString
  = IdString String
```

#### `IdempotentParameterMismatchException`

``` purescript
newtype IdempotentParameterMismatchException
  = IdempotentParameterMismatchException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The same unique identifier was associated with two different records.</p>

#### `ImportCatalogToGlueRequest`

``` purescript
newtype ImportCatalogToGlueRequest
  = ImportCatalogToGlueRequest { "CatalogId" :: NullOrUndefined (CatalogIdString) }
```

#### `ImportCatalogToGlueResponse`

``` purescript
newtype ImportCatalogToGlueResponse
  = ImportCatalogToGlueResponse {  }
```

#### `IntegerFlag`

``` purescript
newtype IntegerFlag
  = IntegerFlag Int
```

#### `IntegerValue`

``` purescript
newtype IntegerValue
  = IntegerValue Int
```

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException { "Message" :: NullOrUndefined (MessageString) }
```

<p>An internal service error occurred.</p>

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The input provided was not valid.</p>

#### `JdbcTarget`

``` purescript
newtype JdbcTarget
  = JdbcTarget { "ConnectionName" :: NullOrUndefined (ConnectionName), "Path" :: NullOrUndefined (Path), "Exclusions" :: NullOrUndefined (PathList) }
```

<p>Specifies a JDBC data store to crawl.</p>

#### `JdbcTargetList`

``` purescript
newtype JdbcTargetList
  = JdbcTargetList (Array JdbcTarget)
```

#### `Job`

``` purescript
newtype Job
  = Job { "Name" :: NullOrUndefined (NameString), "Description" :: NullOrUndefined (DescriptionString), "LogUri" :: NullOrUndefined (UriString), "Role" :: NullOrUndefined (RoleString), "CreatedOn" :: NullOrUndefined (TimestampValue), "LastModifiedOn" :: NullOrUndefined (TimestampValue), "ExecutionProperty" :: NullOrUndefined (ExecutionProperty), "Command" :: NullOrUndefined (JobCommand), "DefaultArguments" :: NullOrUndefined (GenericMap), "Connections" :: NullOrUndefined (ConnectionsList), "MaxRetries" :: NullOrUndefined (MaxRetries), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

<p>Specifies a job.</p>

#### `JobBookmarkEntry`

``` purescript
newtype JobBookmarkEntry
  = JobBookmarkEntry { "JobName" :: NullOrUndefined (JobName), "Version" :: NullOrUndefined (IntegerValue), "Run" :: NullOrUndefined (IntegerValue), "Attempt" :: NullOrUndefined (IntegerValue), "JobBookmark" :: NullOrUndefined (JsonValue) }
```

<p>Defines a point which a job can resume processing.</p>

#### `JobCommand`

``` purescript
newtype JobCommand
  = JobCommand { "Name" :: NullOrUndefined (GenericString), "ScriptLocation" :: NullOrUndefined (ScriptLocationString) }
```

<p>Specifies code that executes a job.</p>

#### `JobList`

``` purescript
newtype JobList
  = JobList (Array Job)
```

#### `JobName`

``` purescript
newtype JobName
  = JobName String
```

#### `JobRun`

``` purescript
newtype JobRun
  = JobRun { "Id" :: NullOrUndefined (IdString), "Attempt" :: NullOrUndefined (AttemptCount), "PreviousRunId" :: NullOrUndefined (IdString), "TriggerName" :: NullOrUndefined (NameString), "JobName" :: NullOrUndefined (NameString), "StartedOn" :: NullOrUndefined (TimestampValue), "LastModifiedOn" :: NullOrUndefined (TimestampValue), "CompletedOn" :: NullOrUndefined (TimestampValue), "JobRunState" :: NullOrUndefined (JobRunState), "Arguments" :: NullOrUndefined (GenericMap), "ErrorMessage" :: NullOrUndefined (ErrorString), "PredecessorRuns" :: NullOrUndefined (PredecessorList), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

<p>Contains information about a job run.</p>

#### `JobRunList`

``` purescript
newtype JobRunList
  = JobRunList (Array JobRun)
```

#### `JobRunState`

``` purescript
newtype JobRunState
  = JobRunState String
```

#### `JobUpdate`

``` purescript
newtype JobUpdate
  = JobUpdate { "Description" :: NullOrUndefined (DescriptionString), "LogUri" :: NullOrUndefined (UriString), "Role" :: NullOrUndefined (RoleString), "ExecutionProperty" :: NullOrUndefined (ExecutionProperty), "Command" :: NullOrUndefined (JobCommand), "DefaultArguments" :: NullOrUndefined (GenericMap), "Connections" :: NullOrUndefined (ConnectionsList), "MaxRetries" :: NullOrUndefined (MaxRetries), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

<p>Specifies information used to update an existing job. Note that the previous job definition will be completely overwritten by this information.</p>

#### `JsonClassifier`

``` purescript
newtype JsonClassifier
  = JsonClassifier { "Name" :: NameString, "CreationTime" :: NullOrUndefined (Number), "LastUpdated" :: NullOrUndefined (Number), "Version" :: NullOrUndefined (VersionId), "JsonPath" :: JsonPath }
```

<p>A classifier for <code>JSON</code> content.</p>

#### `JsonPath`

``` purescript
newtype JsonPath
  = JsonPath String
```

#### `JsonValue`

``` purescript
newtype JsonValue
  = JsonValue String
```

#### `KeyString`

``` purescript
newtype KeyString
  = KeyString String
```

#### `Language`

``` purescript
newtype Language
  = Language String
```

#### `LastCrawlInfo`

``` purescript
newtype LastCrawlInfo
  = LastCrawlInfo { "Status" :: NullOrUndefined (LastCrawlStatus), "ErrorMessage" :: NullOrUndefined (DescriptionString), "LogGroup" :: NullOrUndefined (LogGroup), "LogStream" :: NullOrUndefined (LogStream), "MessagePrefix" :: NullOrUndefined (MessagePrefix), "StartTime" :: NullOrUndefined (Number) }
```

<p>Status and error information about the most recent crawl.</p>

#### `LastCrawlStatus`

``` purescript
newtype LastCrawlStatus
  = LastCrawlStatus String
```

#### `Location`

``` purescript
newtype Location
  = Location { "Jdbc" :: NullOrUndefined (CodeGenNodeArgs), "S3" :: NullOrUndefined (CodeGenNodeArgs) }
```

<p>The location of resources.</p>

#### `LocationMap`

``` purescript
newtype LocationMap
  = LocationMap (Map ColumnValuesString ColumnValuesString)
```

#### `LocationString`

``` purescript
newtype LocationString
  = LocationString String
```

#### `LogGroup`

``` purescript
newtype LogGroup
  = LogGroup String
```

#### `LogStream`

``` purescript
newtype LogStream
  = LogStream String
```

#### `Logical`

``` purescript
newtype Logical
  = Logical String
```

#### `LogicalOperator`

``` purescript
newtype LogicalOperator
  = LogicalOperator String
```

#### `MappingEntry`

``` purescript
newtype MappingEntry
  = MappingEntry { "SourceTable" :: NullOrUndefined (TableName), "SourcePath" :: NullOrUndefined (SchemaPathString), "SourceType" :: NullOrUndefined (FieldType), "TargetTable" :: NullOrUndefined (TableName), "TargetPath" :: NullOrUndefined (SchemaPathString), "TargetType" :: NullOrUndefined (FieldType) }
```

<p>Defines a mapping.</p>

#### `MappingList`

``` purescript
newtype MappingList
  = MappingList (Array MappingEntry)
```

#### `MatchCriteria`

``` purescript
newtype MatchCriteria
  = MatchCriteria (Array NameString)
```

#### `MaxConcurrentRuns`

``` purescript
newtype MaxConcurrentRuns
  = MaxConcurrentRuns Int
```

#### `MaxRetries`

``` purescript
newtype MaxRetries
  = MaxRetries Int
```

#### `MessagePrefix`

``` purescript
newtype MessagePrefix
  = MessagePrefix String
```

#### `MessageString`

``` purescript
newtype MessageString
  = MessageString String
```

#### `MillisecondsCount`

``` purescript
newtype MillisecondsCount
  = MillisecondsCount Number
```

#### `NameString`

``` purescript
newtype NameString
  = NameString String
```

#### `NameStringList`

``` purescript
newtype NameStringList
  = NameStringList (Array NameString)
```

#### `NoScheduleException`

``` purescript
newtype NoScheduleException
  = NoScheduleException { "Message" :: NullOrUndefined (MessageString) }
```

<p>There is no applicable schedule.</p>

#### `NonNegativeDouble`

``` purescript
newtype NonNegativeDouble
  = NonNegativeDouble Number
```

#### `NonNegativeInteger`

``` purescript
newtype NonNegativeInteger
  = NonNegativeInteger Int
```

#### `OperationTimeoutException`

``` purescript
newtype OperationTimeoutException
  = OperationTimeoutException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The operation timed out.</p>

#### `Order`

``` purescript
newtype Order
  = Order { "Column" :: NameString, "SortOrder" :: IntegerFlag }
```

<p>Specifies the sort order of a sorted column.</p>

#### `OrderList`

``` purescript
newtype OrderList
  = OrderList (Array Order)
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

#### `ParametersMap`

``` purescript
newtype ParametersMap
  = ParametersMap (Map KeyString ParametersMapValue)
```

#### `ParametersMapValue`

``` purescript
newtype ParametersMapValue
  = ParametersMapValue String
```

#### `Partition`

``` purescript
newtype Partition
  = Partition { "Values" :: NullOrUndefined (ValueStringList), "DatabaseName" :: NullOrUndefined (NameString), "TableName" :: NullOrUndefined (NameString), "CreationTime" :: NullOrUndefined (Number), "LastAccessTime" :: NullOrUndefined (Number), "StorageDescriptor" :: NullOrUndefined (StorageDescriptor), "Parameters" :: NullOrUndefined (ParametersMap), "LastAnalyzedTime" :: NullOrUndefined (Number) }
```

<p>Represents a slice of table data.</p>

#### `PartitionError`

``` purescript
newtype PartitionError
  = PartitionError { "PartitionValues" :: NullOrUndefined (ValueStringList), "ErrorDetail" :: NullOrUndefined (ErrorDetail) }
```

<p>Contains information about a partition error.</p>

#### `PartitionErrors`

``` purescript
newtype PartitionErrors
  = PartitionErrors (Array PartitionError)
```

#### `PartitionInput`

``` purescript
newtype PartitionInput
  = PartitionInput { "Values" :: NullOrUndefined (ValueStringList), "LastAccessTime" :: NullOrUndefined (Number), "StorageDescriptor" :: NullOrUndefined (StorageDescriptor), "Parameters" :: NullOrUndefined (ParametersMap), "LastAnalyzedTime" :: NullOrUndefined (Number) }
```

<p>The structure used to create and update a partion.</p>

#### `PartitionInputList`

``` purescript
newtype PartitionInputList
  = PartitionInputList (Array PartitionInput)
```

#### `PartitionList`

``` purescript
newtype PartitionList
  = PartitionList (Array Partition)
```

#### `PartitionValueList`

``` purescript
newtype PartitionValueList
  = PartitionValueList { "Values" :: ValueStringList }
```

<p>Contains a list of values defining partitions.</p>

#### `Path`

``` purescript
newtype Path
  = Path String
```

#### `PathList`

``` purescript
newtype PathList
  = PathList (Array Path)
```

#### `PhysicalConnectionRequirements`

``` purescript
newtype PhysicalConnectionRequirements
  = PhysicalConnectionRequirements { "SubnetId" :: NullOrUndefined (NameString), "SecurityGroupIdList" :: NullOrUndefined (SecurityGroupIdList), "AvailabilityZone" :: NullOrUndefined (NameString) }
```

<p>Specifies the physical requirements for a connection.</p>

#### `Predecessor`

``` purescript
newtype Predecessor
  = Predecessor { "JobName" :: NullOrUndefined (NameString), "RunId" :: NullOrUndefined (IdString) }
```

<p>A job run that was used in the predicate of a conditional trigger that triggered this job run.</p>

#### `PredecessorList`

``` purescript
newtype PredecessorList
  = PredecessorList (Array Predecessor)
```

#### `Predicate`

``` purescript
newtype Predicate
  = Predicate { "Logical" :: NullOrUndefined (Logical), "Conditions" :: NullOrUndefined (ConditionList) }
```

<p>Defines the predicate of the trigger, which determines when it fires.</p>

#### `PredicateString`

``` purescript
newtype PredicateString
  = PredicateString String
```

#### `PrincipalType`

``` purescript
newtype PrincipalType
  = PrincipalType String
```

#### `PythonScript`

``` purescript
newtype PythonScript
  = PythonScript String
```

#### `ResetJobBookmarkRequest`

``` purescript
newtype ResetJobBookmarkRequest
  = ResetJobBookmarkRequest { "JobName" :: JobName }
```

#### `ResetJobBookmarkResponse`

``` purescript
newtype ResetJobBookmarkResponse
  = ResetJobBookmarkResponse { "JobBookmarkEntry" :: NullOrUndefined (JobBookmarkEntry) }
```

#### `ResourceNumberLimitExceededException`

``` purescript
newtype ResourceNumberLimitExceededException
  = ResourceNumberLimitExceededException { "Message" :: NullOrUndefined (MessageString) }
```

<p>A resource numerical limit was exceeded.</p>

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

#### `ResourceUri`

``` purescript
newtype ResourceUri
  = ResourceUri { "ResourceType" :: NullOrUndefined (ResourceType), "Uri" :: NullOrUndefined (URI) }
```

<p>URIs for function resources.</p>

#### `ResourceUriList`

``` purescript
newtype ResourceUriList
  = ResourceUriList (Array ResourceUri)
```

#### `Role`

``` purescript
newtype Role
  = Role String
```

#### `RoleArn`

``` purescript
newtype RoleArn
  = RoleArn String
```

#### `RoleString`

``` purescript
newtype RoleString
  = RoleString String
```

#### `RowTag`

``` purescript
newtype RowTag
  = RowTag String
```

#### `S3Target`

``` purescript
newtype S3Target
  = S3Target { "Path" :: NullOrUndefined (Path), "Exclusions" :: NullOrUndefined (PathList) }
```

<p>Specifies a data store in Amazon S3.</p>

#### `S3TargetList`

``` purescript
newtype S3TargetList
  = S3TargetList (Array S3Target)
```

#### `ScalaCode`

``` purescript
newtype ScalaCode
  = ScalaCode String
```

#### `Schedule`

``` purescript
newtype Schedule
  = Schedule { "ScheduleExpression" :: NullOrUndefined (CronExpression), "State" :: NullOrUndefined (ScheduleState) }
```

<p>A scheduling object using a <code>cron</code> statement to schedule an event.</p>

#### `ScheduleState`

``` purescript
newtype ScheduleState
  = ScheduleState String
```

#### `SchedulerNotRunningException`

``` purescript
newtype SchedulerNotRunningException
  = SchedulerNotRunningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified scheduler is not running.</p>

#### `SchedulerRunningException`

``` purescript
newtype SchedulerRunningException
  = SchedulerRunningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified scheduler is already running.</p>

#### `SchedulerTransitioningException`

``` purescript
newtype SchedulerTransitioningException
  = SchedulerTransitioningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified scheduler is transitioning.</p>

#### `SchemaChangePolicy`

``` purescript
newtype SchemaChangePolicy
  = SchemaChangePolicy { "UpdateBehavior" :: NullOrUndefined (UpdateBehavior), "DeleteBehavior" :: NullOrUndefined (DeleteBehavior) }
```

<p>Crawler policy for update and deletion behavior.</p>

#### `SchemaPathString`

``` purescript
newtype SchemaPathString
  = SchemaPathString String
```

#### `ScriptLocationString`

``` purescript
newtype ScriptLocationString
  = ScriptLocationString String
```

#### `SecurityGroupIdList`

``` purescript
newtype SecurityGroupIdList
  = SecurityGroupIdList (Array NameString)
```

#### `Segment`

``` purescript
newtype Segment
  = Segment { "SegmentNumber" :: NonNegativeInteger, "TotalSegments" :: TotalSegmentsInteger }
```

<p>Defines a non-overlapping region of a table's partitions, allowing multiple requests to be executed in parallel.</p>

#### `SerDeInfo`

``` purescript
newtype SerDeInfo
  = SerDeInfo { "Name" :: NullOrUndefined (NameString), "SerializationLibrary" :: NullOrUndefined (NameString), "Parameters" :: NullOrUndefined (ParametersMap) }
```

<p>Information about a serialization/deserialization program (SerDe) which serves as an extractor and loader.</p>

#### `SkewedInfo`

``` purescript
newtype SkewedInfo
  = SkewedInfo { "SkewedColumnNames" :: NullOrUndefined (NameStringList), "SkewedColumnValues" :: NullOrUndefined (ColumnValueStringList), "SkewedColumnValueLocationMaps" :: NullOrUndefined (LocationMap) }
```

<p>Specifies skewed values in a table. Skewed are ones that occur with very high frequency.</p>

#### `StartCrawlerRequest`

``` purescript
newtype StartCrawlerRequest
  = StartCrawlerRequest { "Name" :: NameString }
```

#### `StartCrawlerResponse`

``` purescript
newtype StartCrawlerResponse
  = StartCrawlerResponse {  }
```

#### `StartCrawlerScheduleRequest`

``` purescript
newtype StartCrawlerScheduleRequest
  = StartCrawlerScheduleRequest { "CrawlerName" :: NameString }
```

#### `StartCrawlerScheduleResponse`

``` purescript
newtype StartCrawlerScheduleResponse
  = StartCrawlerScheduleResponse {  }
```

#### `StartJobRunRequest`

``` purescript
newtype StartJobRunRequest
  = StartJobRunRequest { "JobName" :: NameString, "JobRunId" :: NullOrUndefined (IdString), "Arguments" :: NullOrUndefined (GenericMap), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

#### `StartJobRunResponse`

``` purescript
newtype StartJobRunResponse
  = StartJobRunResponse { "JobRunId" :: NullOrUndefined (IdString) }
```

#### `StartTriggerRequest`

``` purescript
newtype StartTriggerRequest
  = StartTriggerRequest { "Name" :: NameString }
```

#### `StartTriggerResponse`

``` purescript
newtype StartTriggerResponse
  = StartTriggerResponse { "Name" :: NullOrUndefined (NameString) }
```

#### `StopCrawlerRequest`

``` purescript
newtype StopCrawlerRequest
  = StopCrawlerRequest { "Name" :: NameString }
```

#### `StopCrawlerResponse`

``` purescript
newtype StopCrawlerResponse
  = StopCrawlerResponse {  }
```

#### `StopCrawlerScheduleRequest`

``` purescript
newtype StopCrawlerScheduleRequest
  = StopCrawlerScheduleRequest { "CrawlerName" :: NameString }
```

#### `StopCrawlerScheduleResponse`

``` purescript
newtype StopCrawlerScheduleResponse
  = StopCrawlerScheduleResponse {  }
```

#### `StopTriggerRequest`

``` purescript
newtype StopTriggerRequest
  = StopTriggerRequest { "Name" :: NameString }
```

#### `StopTriggerResponse`

``` purescript
newtype StopTriggerResponse
  = StopTriggerResponse { "Name" :: NullOrUndefined (NameString) }
```

#### `StorageDescriptor`

``` purescript
newtype StorageDescriptor
  = StorageDescriptor { "Columns" :: NullOrUndefined (ColumnList), "Location" :: NullOrUndefined (LocationString), "InputFormat" :: NullOrUndefined (FormatString), "OutputFormat" :: NullOrUndefined (FormatString), "Compressed" :: NullOrUndefined (Boolean), "NumberOfBuckets" :: NullOrUndefined (Int), "SerdeInfo" :: NullOrUndefined (SerDeInfo), "BucketColumns" :: NullOrUndefined (NameStringList), "SortColumns" :: NullOrUndefined (OrderList), "Parameters" :: NullOrUndefined (ParametersMap), "SkewedInfo" :: NullOrUndefined (SkewedInfo), "StoredAsSubDirectories" :: NullOrUndefined (Boolean) }
```

<p>Describes the physical storage of table data.</p>

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array GenericString)
```

#### `Table`

``` purescript
newtype Table
  = Table { "Name" :: NameString, "DatabaseName" :: NullOrUndefined (NameString), "Description" :: NullOrUndefined (DescriptionString), "Owner" :: NullOrUndefined (NameString), "CreateTime" :: NullOrUndefined (Number), "UpdateTime" :: NullOrUndefined (Number), "LastAccessTime" :: NullOrUndefined (Number), "LastAnalyzedTime" :: NullOrUndefined (Number), "Retention" :: NullOrUndefined (NonNegativeInteger), "StorageDescriptor" :: NullOrUndefined (StorageDescriptor), "PartitionKeys" :: NullOrUndefined (ColumnList), "ViewOriginalText" :: NullOrUndefined (ViewTextString), "ViewExpandedText" :: NullOrUndefined (ViewTextString), "TableType" :: NullOrUndefined (TableTypeString), "Parameters" :: NullOrUndefined (ParametersMap), "CreatedBy" :: NullOrUndefined (NameString) }
```

<p>Represents a collection of related data organized in columns and rows.</p>

#### `TableError`

``` purescript
newtype TableError
  = TableError { "TableName" :: NullOrUndefined (NameString), "ErrorDetail" :: NullOrUndefined (ErrorDetail) }
```

<p>An error record for table operations.</p>

#### `TableErrors`

``` purescript
newtype TableErrors
  = TableErrors (Array TableError)
```

#### `TableInput`

``` purescript
newtype TableInput
  = TableInput { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "Owner" :: NullOrUndefined (NameString), "LastAccessTime" :: NullOrUndefined (Number), "LastAnalyzedTime" :: NullOrUndefined (Number), "Retention" :: NullOrUndefined (NonNegativeInteger), "StorageDescriptor" :: NullOrUndefined (StorageDescriptor), "PartitionKeys" :: NullOrUndefined (ColumnList), "ViewOriginalText" :: NullOrUndefined (ViewTextString), "ViewExpandedText" :: NullOrUndefined (ViewTextString), "TableType" :: NullOrUndefined (TableTypeString), "Parameters" :: NullOrUndefined (ParametersMap) }
```

<p>Structure used to create or update the table.</p>

#### `TableList`

``` purescript
newtype TableList
  = TableList (Array Table)
```

#### `TableName`

``` purescript
newtype TableName
  = TableName String
```

#### `TablePrefix`

``` purescript
newtype TablePrefix
  = TablePrefix String
```

#### `TableTypeString`

``` purescript
newtype TableTypeString
  = TableTypeString String
```

#### `TableVersion`

``` purescript
newtype TableVersion
  = TableVersion { "Table" :: NullOrUndefined (Table), "VersionId" :: NullOrUndefined (VersionString) }
```

<p>Specifies a version of a table.</p>

#### `TableVersionError`

``` purescript
newtype TableVersionError
  = TableVersionError { "TableName" :: NullOrUndefined (NameString), "VersionId" :: NullOrUndefined (VersionString), "ErrorDetail" :: NullOrUndefined (ErrorDetail) }
```

<p>An error record for table-version operations.</p>

#### `TableVersionErrors`

``` purescript
newtype TableVersionErrors
  = TableVersionErrors (Array TableVersionError)
```

#### `TimestampValue`

``` purescript
newtype TimestampValue
  = TimestampValue Number
```

#### `Token`

``` purescript
newtype Token
  = Token String
```

#### `TotalSegmentsInteger`

``` purescript
newtype TotalSegmentsInteger
  = TotalSegmentsInteger Int
```

#### `Trigger`

``` purescript
newtype Trigger
  = Trigger { "Name" :: NullOrUndefined (NameString), "Id" :: NullOrUndefined (IdString), "Type" :: NullOrUndefined (TriggerType), "State" :: NullOrUndefined (TriggerState), "Description" :: NullOrUndefined (DescriptionString), "Schedule" :: NullOrUndefined (GenericString), "Actions" :: NullOrUndefined (ActionList), "Predicate" :: NullOrUndefined (Predicate) }
```

<p>Information about a specific trigger.</p>

#### `TriggerList`

``` purescript
newtype TriggerList
  = TriggerList (Array Trigger)
```

#### `TriggerState`

``` purescript
newtype TriggerState
  = TriggerState String
```

#### `TriggerType`

``` purescript
newtype TriggerType
  = TriggerType String
```

#### `TriggerUpdate`

``` purescript
newtype TriggerUpdate
  = TriggerUpdate { "Name" :: NullOrUndefined (NameString), "Description" :: NullOrUndefined (DescriptionString), "Schedule" :: NullOrUndefined (GenericString), "Actions" :: NullOrUndefined (ActionList), "Predicate" :: NullOrUndefined (Predicate) }
```

<p>A structure used to provide information used to update a trigger. This object will update the the previous trigger definition by overwriting it completely.</p>

#### `URI`

``` purescript
newtype URI
  = URI String
```

#### `UpdateBehavior`

``` purescript
newtype UpdateBehavior
  = UpdateBehavior String
```

#### `UpdateClassifierRequest`

``` purescript
newtype UpdateClassifierRequest
  = UpdateClassifierRequest { "GrokClassifier" :: NullOrUndefined (UpdateGrokClassifierRequest), "XMLClassifier" :: NullOrUndefined (UpdateXMLClassifierRequest), "JsonClassifier" :: NullOrUndefined (UpdateJsonClassifierRequest) }
```

#### `UpdateClassifierResponse`

``` purescript
newtype UpdateClassifierResponse
  = UpdateClassifierResponse {  }
```

#### `UpdateConnectionRequest`

``` purescript
newtype UpdateConnectionRequest
  = UpdateConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString, "ConnectionInput" :: ConnectionInput }
```

#### `UpdateConnectionResponse`

``` purescript
newtype UpdateConnectionResponse
  = UpdateConnectionResponse {  }
```

#### `UpdateCrawlerRequest`

``` purescript
newtype UpdateCrawlerRequest
  = UpdateCrawlerRequest { "Name" :: NameString, "Role" :: NullOrUndefined (Role), "DatabaseName" :: NullOrUndefined (DatabaseName), "Description" :: NullOrUndefined (DescriptionStringRemovable), "Targets" :: NullOrUndefined (CrawlerTargets), "Schedule" :: NullOrUndefined (CronExpression), "Classifiers" :: NullOrUndefined (ClassifierNameList), "TablePrefix" :: NullOrUndefined (TablePrefix), "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy), "Configuration" :: NullOrUndefined (CrawlerConfiguration) }
```

#### `UpdateCrawlerResponse`

``` purescript
newtype UpdateCrawlerResponse
  = UpdateCrawlerResponse {  }
```

#### `UpdateCrawlerScheduleRequest`

``` purescript
newtype UpdateCrawlerScheduleRequest
  = UpdateCrawlerScheduleRequest { "CrawlerName" :: NameString, "Schedule" :: NullOrUndefined (CronExpression) }
```

#### `UpdateCrawlerScheduleResponse`

``` purescript
newtype UpdateCrawlerScheduleResponse
  = UpdateCrawlerScheduleResponse {  }
```

#### `UpdateDatabaseRequest`

``` purescript
newtype UpdateDatabaseRequest
  = UpdateDatabaseRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString, "DatabaseInput" :: DatabaseInput }
```

#### `UpdateDatabaseResponse`

``` purescript
newtype UpdateDatabaseResponse
  = UpdateDatabaseResponse {  }
```

#### `UpdateDevEndpointRequest`

``` purescript
newtype UpdateDevEndpointRequest
  = UpdateDevEndpointRequest { "EndpointName" :: GenericString, "PublicKey" :: NullOrUndefined (GenericString), "CustomLibraries" :: NullOrUndefined (DevEndpointCustomLibraries), "UpdateEtlLibraries" :: NullOrUndefined (BooleanValue) }
```

#### `UpdateDevEndpointResponse`

``` purescript
newtype UpdateDevEndpointResponse
  = UpdateDevEndpointResponse {  }
```

#### `UpdateGrokClassifierRequest`

``` purescript
newtype UpdateGrokClassifierRequest
  = UpdateGrokClassifierRequest { "Name" :: NameString, "Classification" :: NullOrUndefined (Classification), "GrokPattern" :: NullOrUndefined (GrokPattern), "CustomPatterns" :: NullOrUndefined (CustomPatterns) }
```

<p>Specifies a grok classifier to update when passed to <code>UpdateClassifier</code>.</p>

#### `UpdateJobRequest`

``` purescript
newtype UpdateJobRequest
  = UpdateJobRequest { "JobName" :: NameString, "JobUpdate" :: JobUpdate }
```

#### `UpdateJobResponse`

``` purescript
newtype UpdateJobResponse
  = UpdateJobResponse { "JobName" :: NullOrUndefined (NameString) }
```

#### `UpdateJsonClassifierRequest`

``` purescript
newtype UpdateJsonClassifierRequest
  = UpdateJsonClassifierRequest { "Name" :: NameString, "JsonPath" :: NullOrUndefined (JsonPath) }
```

<p>Specifies a JSON classifier to be updated.</p>

#### `UpdatePartitionRequest`

``` purescript
newtype UpdatePartitionRequest
  = UpdatePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionValueList" :: BoundedPartitionValueList, "PartitionInput" :: PartitionInput }
```

#### `UpdatePartitionResponse`

``` purescript
newtype UpdatePartitionResponse
  = UpdatePartitionResponse {  }
```

#### `UpdateTableRequest`

``` purescript
newtype UpdateTableRequest
  = UpdateTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableInput" :: TableInput, "SkipArchive" :: NullOrUndefined (BooleanNullable) }
```

#### `UpdateTableResponse`

``` purescript
newtype UpdateTableResponse
  = UpdateTableResponse {  }
```

#### `UpdateTriggerRequest`

``` purescript
newtype UpdateTriggerRequest
  = UpdateTriggerRequest { "Name" :: NameString, "TriggerUpdate" :: TriggerUpdate }
```

#### `UpdateTriggerResponse`

``` purescript
newtype UpdateTriggerResponse
  = UpdateTriggerResponse { "Trigger" :: NullOrUndefined (Trigger) }
```

#### `UpdateUserDefinedFunctionRequest`

``` purescript
newtype UpdateUserDefinedFunctionRequest
  = UpdateUserDefinedFunctionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "FunctionName" :: NameString, "FunctionInput" :: UserDefinedFunctionInput }
```

#### `UpdateUserDefinedFunctionResponse`

``` purescript
newtype UpdateUserDefinedFunctionResponse
  = UpdateUserDefinedFunctionResponse {  }
```

#### `UpdateXMLClassifierRequest`

``` purescript
newtype UpdateXMLClassifierRequest
  = UpdateXMLClassifierRequest { "Name" :: NameString, "Classification" :: NullOrUndefined (Classification), "RowTag" :: NullOrUndefined (RowTag) }
```

<p>Specifies an XML classifier to be updated.</p>

#### `UriString`

``` purescript
newtype UriString
  = UriString String
```

#### `UserDefinedFunction`

``` purescript
newtype UserDefinedFunction
  = UserDefinedFunction { "FunctionName" :: NullOrUndefined (NameString), "ClassName" :: NullOrUndefined (NameString), "OwnerName" :: NullOrUndefined (NameString), "OwnerType" :: NullOrUndefined (PrincipalType), "CreateTime" :: NullOrUndefined (Number), "ResourceUris" :: NullOrUndefined (ResourceUriList) }
```

<p>Represents the equivalent of a Hive user-defined function (<code>UDF</code>) definition.</p>

#### `UserDefinedFunctionInput`

``` purescript
newtype UserDefinedFunctionInput
  = UserDefinedFunctionInput { "FunctionName" :: NullOrUndefined (NameString), "ClassName" :: NullOrUndefined (NameString), "OwnerName" :: NullOrUndefined (NameString), "OwnerType" :: NullOrUndefined (PrincipalType), "ResourceUris" :: NullOrUndefined (ResourceUriList) }
```

<p>A structure used to create or updata a user-defined function.</p>

#### `UserDefinedFunctionList`

``` purescript
newtype UserDefinedFunctionList
  = UserDefinedFunctionList (Array UserDefinedFunction)
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message" :: NullOrUndefined (MessageString) }
```

<p>A value could not be validated.</p>

#### `ValueString`

``` purescript
newtype ValueString
  = ValueString String
```

#### `ValueStringList`

``` purescript
newtype ValueStringList
  = ValueStringList (Array ValueString)
```

#### `VersionId`

``` purescript
newtype VersionId
  = VersionId Number
```

#### `VersionMismatchException`

``` purescript
newtype VersionMismatchException
  = VersionMismatchException { "Message" :: NullOrUndefined (MessageString) }
```

<p>There was a version conflict.</p>

#### `VersionString`

``` purescript
newtype VersionString
  = VersionString String
```

#### `ViewTextString`

``` purescript
newtype ViewTextString
  = ViewTextString String
```

#### `XMLClassifier`

``` purescript
newtype XMLClassifier
  = XMLClassifier { "Name" :: NameString, "Classification" :: Classification, "CreationTime" :: NullOrUndefined (Number), "LastUpdated" :: NullOrUndefined (Number), "Version" :: NullOrUndefined (VersionId), "RowTag" :: NullOrUndefined (RowTag) }
```

<p>A classifier for <code>XML</code> content.</p>


