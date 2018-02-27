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

##### Instances
``` purescript
Newtype AccessDeniedException _
```

#### `Action`

``` purescript
newtype Action
  = Action { "JobName" :: NullOrUndefined (NameString), "Arguments" :: NullOrUndefined (GenericMap) }
```

<p>Defines an action to be initiated by a trigger.</p>

##### Instances
``` purescript
Newtype Action _
```

#### `ActionList`

``` purescript
newtype ActionList
  = ActionList (Array Action)
```

##### Instances
``` purescript
Newtype ActionList _
```

#### `AlreadyExistsException`

``` purescript
newtype AlreadyExistsException
  = AlreadyExistsException { "Message" :: NullOrUndefined (MessageString) }
```

<p>A resource to be created or added already exists.</p>

##### Instances
``` purescript
Newtype AlreadyExistsException _
```

#### `AttemptCount`

``` purescript
newtype AttemptCount
  = AttemptCount Int
```

##### Instances
``` purescript
Newtype AttemptCount _
```

#### `BatchCreatePartitionRequest`

``` purescript
newtype BatchCreatePartitionRequest
  = BatchCreatePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionInputList" :: PartitionInputList }
```

##### Instances
``` purescript
Newtype BatchCreatePartitionRequest _
```

#### `BatchCreatePartitionResponse`

``` purescript
newtype BatchCreatePartitionResponse
  = BatchCreatePartitionResponse { "Errors" :: NullOrUndefined (PartitionErrors) }
```

##### Instances
``` purescript
Newtype BatchCreatePartitionResponse _
```

#### `BatchDeleteConnectionRequest`

``` purescript
newtype BatchDeleteConnectionRequest
  = BatchDeleteConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "ConnectionNameList" :: DeleteConnectionNameList }
```

##### Instances
``` purescript
Newtype BatchDeleteConnectionRequest _
```

#### `BatchDeleteConnectionResponse`

``` purescript
newtype BatchDeleteConnectionResponse
  = BatchDeleteConnectionResponse { "Succeeded" :: NullOrUndefined (NameStringList), "Errors" :: NullOrUndefined (ErrorByName) }
```

##### Instances
``` purescript
Newtype BatchDeleteConnectionResponse _
```

#### `BatchDeletePartitionRequest`

``` purescript
newtype BatchDeletePartitionRequest
  = BatchDeletePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionsToDelete" :: BatchDeletePartitionValueList }
```

##### Instances
``` purescript
Newtype BatchDeletePartitionRequest _
```

#### `BatchDeletePartitionResponse`

``` purescript
newtype BatchDeletePartitionResponse
  = BatchDeletePartitionResponse { "Errors" :: NullOrUndefined (PartitionErrors) }
```

##### Instances
``` purescript
Newtype BatchDeletePartitionResponse _
```

#### `BatchDeletePartitionValueList`

``` purescript
newtype BatchDeletePartitionValueList
  = BatchDeletePartitionValueList (Array PartitionValueList)
```

##### Instances
``` purescript
Newtype BatchDeletePartitionValueList _
```

#### `BatchDeleteTableNameList`

``` purescript
newtype BatchDeleteTableNameList
  = BatchDeleteTableNameList (Array NameString)
```

##### Instances
``` purescript
Newtype BatchDeleteTableNameList _
```

#### `BatchDeleteTableRequest`

``` purescript
newtype BatchDeleteTableRequest
  = BatchDeleteTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TablesToDelete" :: BatchDeleteTableNameList }
```

##### Instances
``` purescript
Newtype BatchDeleteTableRequest _
```

#### `BatchDeleteTableResponse`

``` purescript
newtype BatchDeleteTableResponse
  = BatchDeleteTableResponse { "Errors" :: NullOrUndefined (TableErrors) }
```

##### Instances
``` purescript
Newtype BatchDeleteTableResponse _
```

#### `BatchDeleteTableVersionList`

``` purescript
newtype BatchDeleteTableVersionList
  = BatchDeleteTableVersionList (Array VersionString)
```

##### Instances
``` purescript
Newtype BatchDeleteTableVersionList _
```

#### `BatchDeleteTableVersionRequest`

``` purescript
newtype BatchDeleteTableVersionRequest
  = BatchDeleteTableVersionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "VersionIds" :: BatchDeleteTableVersionList }
```

##### Instances
``` purescript
Newtype BatchDeleteTableVersionRequest _
```

#### `BatchDeleteTableVersionResponse`

``` purescript
newtype BatchDeleteTableVersionResponse
  = BatchDeleteTableVersionResponse { "Errors" :: NullOrUndefined (TableVersionErrors) }
```

##### Instances
``` purescript
Newtype BatchDeleteTableVersionResponse _
```

#### `BatchGetPartitionRequest`

``` purescript
newtype BatchGetPartitionRequest
  = BatchGetPartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionsToGet" :: BatchGetPartitionValueList }
```

##### Instances
``` purescript
Newtype BatchGetPartitionRequest _
```

#### `BatchGetPartitionResponse`

``` purescript
newtype BatchGetPartitionResponse
  = BatchGetPartitionResponse { "Partitions" :: NullOrUndefined (PartitionList), "UnprocessedKeys" :: NullOrUndefined (BatchGetPartitionValueList) }
```

##### Instances
``` purescript
Newtype BatchGetPartitionResponse _
```

#### `BatchGetPartitionValueList`

``` purescript
newtype BatchGetPartitionValueList
  = BatchGetPartitionValueList (Array PartitionValueList)
```

##### Instances
``` purescript
Newtype BatchGetPartitionValueList _
```

#### `BatchStopJobRunError`

``` purescript
newtype BatchStopJobRunError
  = BatchStopJobRunError { "JobName" :: NullOrUndefined (NameString), "JobRunId" :: NullOrUndefined (IdString), "ErrorDetail" :: NullOrUndefined (ErrorDetail) }
```

<p>Records an error that occurred when attempting to stop a specified JobRun.</p>

##### Instances
``` purescript
Newtype BatchStopJobRunError _
```

#### `BatchStopJobRunErrorList`

``` purescript
newtype BatchStopJobRunErrorList
  = BatchStopJobRunErrorList (Array BatchStopJobRunError)
```

##### Instances
``` purescript
Newtype BatchStopJobRunErrorList _
```

#### `BatchStopJobRunJobRunIdList`

``` purescript
newtype BatchStopJobRunJobRunIdList
  = BatchStopJobRunJobRunIdList (Array IdString)
```

##### Instances
``` purescript
Newtype BatchStopJobRunJobRunIdList _
```

#### `BatchStopJobRunRequest`

``` purescript
newtype BatchStopJobRunRequest
  = BatchStopJobRunRequest { "JobName" :: NameString, "JobRunIds" :: BatchStopJobRunJobRunIdList }
```

##### Instances
``` purescript
Newtype BatchStopJobRunRequest _
```

#### `BatchStopJobRunResponse`

``` purescript
newtype BatchStopJobRunResponse
  = BatchStopJobRunResponse { "SuccessfulSubmissions" :: NullOrUndefined (BatchStopJobRunSuccessfulSubmissionList), "Errors" :: NullOrUndefined (BatchStopJobRunErrorList) }
```

##### Instances
``` purescript
Newtype BatchStopJobRunResponse _
```

#### `BatchStopJobRunSuccessfulSubmission`

``` purescript
newtype BatchStopJobRunSuccessfulSubmission
  = BatchStopJobRunSuccessfulSubmission { "JobName" :: NullOrUndefined (NameString), "JobRunId" :: NullOrUndefined (IdString) }
```

<p>Records a successful request to stop a specified JobRun.</p>

##### Instances
``` purescript
Newtype BatchStopJobRunSuccessfulSubmission _
```

#### `BatchStopJobRunSuccessfulSubmissionList`

``` purescript
newtype BatchStopJobRunSuccessfulSubmissionList
  = BatchStopJobRunSuccessfulSubmissionList (Array BatchStopJobRunSuccessfulSubmission)
```

##### Instances
``` purescript
Newtype BatchStopJobRunSuccessfulSubmissionList _
```

#### `BooleanNullable`

``` purescript
newtype BooleanNullable
  = BooleanNullable Boolean
```

##### Instances
``` purescript
Newtype BooleanNullable _
```

#### `BooleanValue`

``` purescript
newtype BooleanValue
  = BooleanValue Boolean
```

##### Instances
``` purescript
Newtype BooleanValue _
```

#### `BoundedPartitionValueList`

``` purescript
newtype BoundedPartitionValueList
  = BoundedPartitionValueList (Array ValueString)
```

##### Instances
``` purescript
Newtype BoundedPartitionValueList _
```

#### `CatalogEntries`

``` purescript
newtype CatalogEntries
  = CatalogEntries (Array CatalogEntry)
```

##### Instances
``` purescript
Newtype CatalogEntries _
```

#### `CatalogEntry`

``` purescript
newtype CatalogEntry
  = CatalogEntry { "DatabaseName" :: NameString, "TableName" :: NameString }
```

<p>Specifies a table definition in the Data Catalog.</p>

##### Instances
``` purescript
Newtype CatalogEntry _
```

#### `CatalogIdString`

``` purescript
newtype CatalogIdString
  = CatalogIdString String
```

##### Instances
``` purescript
Newtype CatalogIdString _
```

#### `CatalogImportStatus`

``` purescript
newtype CatalogImportStatus
  = CatalogImportStatus { "ImportCompleted" :: NullOrUndefined (Boolean), "ImportTime" :: NullOrUndefined (Number), "ImportedBy" :: NullOrUndefined (NameString) }
```

<p>A structure containing migration status information.</p>

##### Instances
``` purescript
Newtype CatalogImportStatus _
```

#### `Classification`

``` purescript
newtype Classification
  = Classification String
```

##### Instances
``` purescript
Newtype Classification _
```

#### `Classifier`

``` purescript
newtype Classifier
  = Classifier { "GrokClassifier" :: NullOrUndefined (GrokClassifier), "XMLClassifier" :: NullOrUndefined (XMLClassifier), "JsonClassifier" :: NullOrUndefined (JsonClassifier) }
```

<p>Classifiers are written in Python and triggered during a crawl task. You can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier checks whether a given file is in a format it can handle, and if it is, the classifier creates a schema in the form of a <code>StructType</code> object that matches that data format.</p> <p>A classifier can be a <code>grok</code> classifier, an XML classifier, or a JSON classifier, asspecified in one of the fields in the <code>Classifier</code> object.</p>

##### Instances
``` purescript
Newtype Classifier _
```

#### `ClassifierList`

``` purescript
newtype ClassifierList
  = ClassifierList (Array Classifier)
```

##### Instances
``` purescript
Newtype ClassifierList _
```

#### `ClassifierNameList`

``` purescript
newtype ClassifierNameList
  = ClassifierNameList (Array NameString)
```

##### Instances
``` purescript
Newtype ClassifierNameList _
```

#### `CodeGenArgName`

``` purescript
newtype CodeGenArgName
  = CodeGenArgName String
```

##### Instances
``` purescript
Newtype CodeGenArgName _
```

#### `CodeGenArgValue`

``` purescript
newtype CodeGenArgValue
  = CodeGenArgValue String
```

##### Instances
``` purescript
Newtype CodeGenArgValue _
```

#### `CodeGenEdge`

``` purescript
newtype CodeGenEdge
  = CodeGenEdge { "Source" :: CodeGenIdentifier, "Target" :: CodeGenIdentifier, "TargetParameter" :: NullOrUndefined (CodeGenArgName) }
```

<p>Represents a directional edge in a directed acyclic graph (DAG).</p>

##### Instances
``` purescript
Newtype CodeGenEdge _
```

#### `CodeGenIdentifier`

``` purescript
newtype CodeGenIdentifier
  = CodeGenIdentifier String
```

##### Instances
``` purescript
Newtype CodeGenIdentifier _
```

#### `CodeGenNode`

``` purescript
newtype CodeGenNode
  = CodeGenNode { "Id" :: CodeGenIdentifier, "NodeType" :: CodeGenNodeType, "Args" :: CodeGenNodeArgs, "LineNumber" :: NullOrUndefined (Int) }
```

<p>Represents a node in a directed acyclic graph (DAG)</p>

##### Instances
``` purescript
Newtype CodeGenNode _
```

#### `CodeGenNodeArg`

``` purescript
newtype CodeGenNodeArg
  = CodeGenNodeArg { "Name" :: CodeGenArgName, "Value" :: CodeGenArgValue, "Param" :: NullOrUndefined (Boolean) }
```

<p>An argument or property of a node.</p>

##### Instances
``` purescript
Newtype CodeGenNodeArg _
```

#### `CodeGenNodeArgs`

``` purescript
newtype CodeGenNodeArgs
  = CodeGenNodeArgs (Array CodeGenNodeArg)
```

##### Instances
``` purescript
Newtype CodeGenNodeArgs _
```

#### `CodeGenNodeType`

``` purescript
newtype CodeGenNodeType
  = CodeGenNodeType String
```

##### Instances
``` purescript
Newtype CodeGenNodeType _
```

#### `Column`

``` purescript
newtype Column
  = Column { "Name" :: NameString, "Type" :: NullOrUndefined (ColumnTypeString), "Comment" :: NullOrUndefined (CommentString) }
```

<p>A column in a <code>Table</code>.</p>

##### Instances
``` purescript
Newtype Column _
```

#### `ColumnList`

``` purescript
newtype ColumnList
  = ColumnList (Array Column)
```

##### Instances
``` purescript
Newtype ColumnList _
```

#### `ColumnTypeString`

``` purescript
newtype ColumnTypeString
  = ColumnTypeString String
```

##### Instances
``` purescript
Newtype ColumnTypeString _
```

#### `ColumnValueStringList`

``` purescript
newtype ColumnValueStringList
  = ColumnValueStringList (Array ColumnValuesString)
```

##### Instances
``` purescript
Newtype ColumnValueStringList _
```

#### `ColumnValuesString`

``` purescript
newtype ColumnValuesString
  = ColumnValuesString String
```

##### Instances
``` purescript
Newtype ColumnValuesString _
```

#### `CommentString`

``` purescript
newtype CommentString
  = CommentString String
```

##### Instances
``` purescript
Newtype CommentString _
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message" :: NullOrUndefined (MessageString) }
```

<p>Two processes are trying to modify a resource simultaneously.</p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `ConcurrentRunsExceededException`

``` purescript
newtype ConcurrentRunsExceededException
  = ConcurrentRunsExceededException { "Message" :: NullOrUndefined (MessageString) }
```

<p>Too many jobs are being run concurrently.</p>

##### Instances
``` purescript
Newtype ConcurrentRunsExceededException _
```

#### `Condition`

``` purescript
newtype Condition
  = Condition { "LogicalOperator" :: NullOrUndefined (LogicalOperator), "JobName" :: NullOrUndefined (NameString), "State" :: NullOrUndefined (JobRunState) }
```

<p>Defines a condition under which a trigger fires.</p>

##### Instances
``` purescript
Newtype Condition _
```

#### `ConditionList`

``` purescript
newtype ConditionList
  = ConditionList (Array Condition)
```

##### Instances
``` purescript
Newtype ConditionList _
```

#### `Connection`

``` purescript
newtype Connection
  = Connection { "Name" :: NullOrUndefined (NameString), "Description" :: NullOrUndefined (DescriptionString), "ConnectionType" :: NullOrUndefined (ConnectionType), "MatchCriteria" :: NullOrUndefined (MatchCriteria), "ConnectionProperties" :: NullOrUndefined (ConnectionProperties), "PhysicalConnectionRequirements" :: NullOrUndefined (PhysicalConnectionRequirements), "CreationTime" :: NullOrUndefined (Number), "LastUpdatedTime" :: NullOrUndefined (Number), "LastUpdatedBy" :: NullOrUndefined (NameString) }
```

<p>Defines a connection to a data source.</p>

##### Instances
``` purescript
Newtype Connection _
```

#### `ConnectionInput`

``` purescript
newtype ConnectionInput
  = ConnectionInput { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "ConnectionType" :: ConnectionType, "MatchCriteria" :: NullOrUndefined (MatchCriteria), "ConnectionProperties" :: ConnectionProperties, "PhysicalConnectionRequirements" :: NullOrUndefined (PhysicalConnectionRequirements) }
```

<p>A structure used to specify a connection to create or update.</p>

##### Instances
``` purescript
Newtype ConnectionInput _
```

#### `ConnectionList`

``` purescript
newtype ConnectionList
  = ConnectionList (Array Connection)
```

##### Instances
``` purescript
Newtype ConnectionList _
```

#### `ConnectionName`

``` purescript
newtype ConnectionName
  = ConnectionName String
```

##### Instances
``` purescript
Newtype ConnectionName _
```

#### `ConnectionProperties`

``` purescript
newtype ConnectionProperties
  = ConnectionProperties (Map ConnectionPropertyKey ValueString)
```

##### Instances
``` purescript
Newtype ConnectionProperties _
```

#### `ConnectionPropertyKey`

``` purescript
newtype ConnectionPropertyKey
  = ConnectionPropertyKey String
```

##### Instances
``` purescript
Newtype ConnectionPropertyKey _
```

#### `ConnectionType`

``` purescript
newtype ConnectionType
  = ConnectionType String
```

##### Instances
``` purescript
Newtype ConnectionType _
```

#### `ConnectionsList`

``` purescript
newtype ConnectionsList
  = ConnectionsList { "Connections" :: NullOrUndefined (StringList) }
```

<p>Specifies the connections used by a job.</p>

##### Instances
``` purescript
Newtype ConnectionsList _
```

#### `Crawler`

``` purescript
newtype Crawler
  = Crawler { "Name" :: NullOrUndefined (NameString), "Role" :: NullOrUndefined (Role), "Targets" :: NullOrUndefined (CrawlerTargets), "DatabaseName" :: NullOrUndefined (DatabaseName), "Description" :: NullOrUndefined (DescriptionString), "Classifiers" :: NullOrUndefined (ClassifierNameList), "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy), "State" :: NullOrUndefined (CrawlerState), "TablePrefix" :: NullOrUndefined (TablePrefix), "Schedule" :: NullOrUndefined (Schedule), "CrawlElapsedTime" :: NullOrUndefined (MillisecondsCount), "CreationTime" :: NullOrUndefined (Number), "LastUpdated" :: NullOrUndefined (Number), "LastCrawl" :: NullOrUndefined (LastCrawlInfo), "Version" :: NullOrUndefined (VersionId), "Configuration" :: NullOrUndefined (CrawlerConfiguration) }
```

<p>Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.</p>

##### Instances
``` purescript
Newtype Crawler _
```

#### `CrawlerConfiguration`

``` purescript
newtype CrawlerConfiguration
  = CrawlerConfiguration String
```

##### Instances
``` purescript
Newtype CrawlerConfiguration _
```

#### `CrawlerList`

``` purescript
newtype CrawlerList
  = CrawlerList (Array Crawler)
```

##### Instances
``` purescript
Newtype CrawlerList _
```

#### `CrawlerMetrics`

``` purescript
newtype CrawlerMetrics
  = CrawlerMetrics { "CrawlerName" :: NullOrUndefined (NameString), "TimeLeftSeconds" :: NullOrUndefined (NonNegativeDouble), "StillEstimating" :: NullOrUndefined (Boolean), "LastRuntimeSeconds" :: NullOrUndefined (NonNegativeDouble), "MedianRuntimeSeconds" :: NullOrUndefined (NonNegativeDouble), "TablesCreated" :: NullOrUndefined (NonNegativeInteger), "TablesUpdated" :: NullOrUndefined (NonNegativeInteger), "TablesDeleted" :: NullOrUndefined (NonNegativeInteger) }
```

<p>Metrics for a specified crawler.</p>

##### Instances
``` purescript
Newtype CrawlerMetrics _
```

#### `CrawlerMetricsList`

``` purescript
newtype CrawlerMetricsList
  = CrawlerMetricsList (Array CrawlerMetrics)
```

##### Instances
``` purescript
Newtype CrawlerMetricsList _
```

#### `CrawlerNameList`

``` purescript
newtype CrawlerNameList
  = CrawlerNameList (Array NameString)
```

##### Instances
``` purescript
Newtype CrawlerNameList _
```

#### `CrawlerNotRunningException`

``` purescript
newtype CrawlerNotRunningException
  = CrawlerNotRunningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified crawler is not running.</p>

##### Instances
``` purescript
Newtype CrawlerNotRunningException _
```

#### `CrawlerRunningException`

``` purescript
newtype CrawlerRunningException
  = CrawlerRunningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The operation cannot be performed because the crawler is already running.</p>

##### Instances
``` purescript
Newtype CrawlerRunningException _
```

#### `CrawlerState`

``` purescript
newtype CrawlerState
  = CrawlerState String
```

##### Instances
``` purescript
Newtype CrawlerState _
```

#### `CrawlerStoppingException`

``` purescript
newtype CrawlerStoppingException
  = CrawlerStoppingException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified crawler is stopping.</p>

##### Instances
``` purescript
Newtype CrawlerStoppingException _
```

#### `CrawlerTargets`

``` purescript
newtype CrawlerTargets
  = CrawlerTargets { "S3Targets" :: NullOrUndefined (S3TargetList), "JdbcTargets" :: NullOrUndefined (JdbcTargetList) }
```

<p>Specifies data stores to crawl.</p>

##### Instances
``` purescript
Newtype CrawlerTargets _
```

#### `CreateClassifierRequest`

``` purescript
newtype CreateClassifierRequest
  = CreateClassifierRequest { "GrokClassifier" :: NullOrUndefined (CreateGrokClassifierRequest), "XMLClassifier" :: NullOrUndefined (CreateXMLClassifierRequest), "JsonClassifier" :: NullOrUndefined (CreateJsonClassifierRequest) }
```

##### Instances
``` purescript
Newtype CreateClassifierRequest _
```

#### `CreateClassifierResponse`

``` purescript
newtype CreateClassifierResponse
  = CreateClassifierResponse {  }
```

##### Instances
``` purescript
Newtype CreateClassifierResponse _
```

#### `CreateConnectionRequest`

``` purescript
newtype CreateConnectionRequest
  = CreateConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "ConnectionInput" :: ConnectionInput }
```

##### Instances
``` purescript
Newtype CreateConnectionRequest _
```

#### `CreateConnectionResponse`

``` purescript
newtype CreateConnectionResponse
  = CreateConnectionResponse {  }
```

##### Instances
``` purescript
Newtype CreateConnectionResponse _
```

#### `CreateCrawlerRequest`

``` purescript
newtype CreateCrawlerRequest
  = CreateCrawlerRequest { "Name" :: NameString, "Role" :: Role, "DatabaseName" :: DatabaseName, "Description" :: NullOrUndefined (DescriptionString), "Targets" :: CrawlerTargets, "Schedule" :: NullOrUndefined (CronExpression), "Classifiers" :: NullOrUndefined (ClassifierNameList), "TablePrefix" :: NullOrUndefined (TablePrefix), "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy), "Configuration" :: NullOrUndefined (CrawlerConfiguration) }
```

##### Instances
``` purescript
Newtype CreateCrawlerRequest _
```

#### `CreateCrawlerResponse`

``` purescript
newtype CreateCrawlerResponse
  = CreateCrawlerResponse {  }
```

##### Instances
``` purescript
Newtype CreateCrawlerResponse _
```

#### `CreateDatabaseRequest`

``` purescript
newtype CreateDatabaseRequest
  = CreateDatabaseRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseInput" :: DatabaseInput }
```

##### Instances
``` purescript
Newtype CreateDatabaseRequest _
```

#### `CreateDatabaseResponse`

``` purescript
newtype CreateDatabaseResponse
  = CreateDatabaseResponse {  }
```

##### Instances
``` purescript
Newtype CreateDatabaseResponse _
```

#### `CreateDevEndpointRequest`

``` purescript
newtype CreateDevEndpointRequest
  = CreateDevEndpointRequest { "EndpointName" :: GenericString, "RoleArn" :: RoleArn, "SecurityGroupIds" :: NullOrUndefined (StringList), "SubnetId" :: NullOrUndefined (GenericString), "PublicKey" :: GenericString, "NumberOfNodes" :: NullOrUndefined (IntegerValue), "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString), "ExtraJarsS3Path" :: NullOrUndefined (GenericString) }
```

##### Instances
``` purescript
Newtype CreateDevEndpointRequest _
```

#### `CreateDevEndpointResponse`

``` purescript
newtype CreateDevEndpointResponse
  = CreateDevEndpointResponse { "EndpointName" :: NullOrUndefined (GenericString), "Status" :: NullOrUndefined (GenericString), "SecurityGroupIds" :: NullOrUndefined (StringList), "SubnetId" :: NullOrUndefined (GenericString), "RoleArn" :: NullOrUndefined (RoleArn), "YarnEndpointAddress" :: NullOrUndefined (GenericString), "ZeppelinRemoteSparkInterpreterPort" :: NullOrUndefined (IntegerValue), "NumberOfNodes" :: NullOrUndefined (IntegerValue), "AvailabilityZone" :: NullOrUndefined (GenericString), "VpcId" :: NullOrUndefined (GenericString), "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString), "ExtraJarsS3Path" :: NullOrUndefined (GenericString), "FailureReason" :: NullOrUndefined (GenericString), "CreatedTimestamp" :: NullOrUndefined (TimestampValue) }
```

##### Instances
``` purescript
Newtype CreateDevEndpointResponse _
```

#### `CreateGrokClassifierRequest`

``` purescript
newtype CreateGrokClassifierRequest
  = CreateGrokClassifierRequest { "Classification" :: Classification, "Name" :: NameString, "GrokPattern" :: GrokPattern, "CustomPatterns" :: NullOrUndefined (CustomPatterns) }
```

<p>Specifies a <code>grok</code> classifier for <code>CreateClassifier</code> to create.</p>

##### Instances
``` purescript
Newtype CreateGrokClassifierRequest _
```

#### `CreateJobRequest`

``` purescript
newtype CreateJobRequest
  = CreateJobRequest { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "LogUri" :: NullOrUndefined (UriString), "Role" :: RoleString, "ExecutionProperty" :: NullOrUndefined (ExecutionProperty), "Command" :: JobCommand, "DefaultArguments" :: NullOrUndefined (GenericMap), "Connections" :: NullOrUndefined (ConnectionsList), "MaxRetries" :: NullOrUndefined (MaxRetries), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

##### Instances
``` purescript
Newtype CreateJobRequest _
```

#### `CreateJobResponse`

``` purescript
newtype CreateJobResponse
  = CreateJobResponse { "Name" :: NullOrUndefined (NameString) }
```

##### Instances
``` purescript
Newtype CreateJobResponse _
```

#### `CreateJsonClassifierRequest`

``` purescript
newtype CreateJsonClassifierRequest
  = CreateJsonClassifierRequest { "Name" :: NameString, "JsonPath" :: JsonPath }
```

<p>Specifies a JSON classifier for <code>CreateClassifier</code> to create.</p>

##### Instances
``` purescript
Newtype CreateJsonClassifierRequest _
```

#### `CreatePartitionRequest`

``` purescript
newtype CreatePartitionRequest
  = CreatePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionInput" :: PartitionInput }
```

##### Instances
``` purescript
Newtype CreatePartitionRequest _
```

#### `CreatePartitionResponse`

``` purescript
newtype CreatePartitionResponse
  = CreatePartitionResponse {  }
```

##### Instances
``` purescript
Newtype CreatePartitionResponse _
```

#### `CreateScriptRequest`

``` purescript
newtype CreateScriptRequest
  = CreateScriptRequest { "DagNodes" :: NullOrUndefined (DagNodes), "DagEdges" :: NullOrUndefined (DagEdges), "Language" :: NullOrUndefined (Language) }
```

##### Instances
``` purescript
Newtype CreateScriptRequest _
```

#### `CreateScriptResponse`

``` purescript
newtype CreateScriptResponse
  = CreateScriptResponse { "PythonScript" :: NullOrUndefined (PythonScript), "ScalaCode" :: NullOrUndefined (ScalaCode) }
```

##### Instances
``` purescript
Newtype CreateScriptResponse _
```

#### `CreateTableRequest`

``` purescript
newtype CreateTableRequest
  = CreateTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableInput" :: TableInput }
```

##### Instances
``` purescript
Newtype CreateTableRequest _
```

#### `CreateTableResponse`

``` purescript
newtype CreateTableResponse
  = CreateTableResponse {  }
```

##### Instances
``` purescript
Newtype CreateTableResponse _
```

#### `CreateTriggerRequest`

``` purescript
newtype CreateTriggerRequest
  = CreateTriggerRequest { "Name" :: NameString, "Type" :: TriggerType, "Schedule" :: NullOrUndefined (GenericString), "Predicate" :: NullOrUndefined (Predicate), "Actions" :: ActionList, "Description" :: NullOrUndefined (DescriptionString) }
```

##### Instances
``` purescript
Newtype CreateTriggerRequest _
```

#### `CreateTriggerResponse`

``` purescript
newtype CreateTriggerResponse
  = CreateTriggerResponse { "Name" :: NullOrUndefined (NameString) }
```

##### Instances
``` purescript
Newtype CreateTriggerResponse _
```

#### `CreateUserDefinedFunctionRequest`

``` purescript
newtype CreateUserDefinedFunctionRequest
  = CreateUserDefinedFunctionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "FunctionInput" :: UserDefinedFunctionInput }
```

##### Instances
``` purescript
Newtype CreateUserDefinedFunctionRequest _
```

#### `CreateUserDefinedFunctionResponse`

``` purescript
newtype CreateUserDefinedFunctionResponse
  = CreateUserDefinedFunctionResponse {  }
```

##### Instances
``` purescript
Newtype CreateUserDefinedFunctionResponse _
```

#### `CreateXMLClassifierRequest`

``` purescript
newtype CreateXMLClassifierRequest
  = CreateXMLClassifierRequest { "Classification" :: Classification, "Name" :: NameString, "RowTag" :: NullOrUndefined (RowTag) }
```

<p>Specifies an XML classifier for <code>CreateClassifier</code> to create.</p>

##### Instances
``` purescript
Newtype CreateXMLClassifierRequest _
```

#### `CronExpression`

``` purescript
newtype CronExpression
  = CronExpression String
```

##### Instances
``` purescript
Newtype CronExpression _
```

#### `CustomPatterns`

``` purescript
newtype CustomPatterns
  = CustomPatterns String
```

##### Instances
``` purescript
Newtype CustomPatterns _
```

#### `DagEdges`

``` purescript
newtype DagEdges
  = DagEdges (Array CodeGenEdge)
```

##### Instances
``` purescript
Newtype DagEdges _
```

#### `DagNodes`

``` purescript
newtype DagNodes
  = DagNodes (Array CodeGenNode)
```

##### Instances
``` purescript
Newtype DagNodes _
```

#### `Database`

``` purescript
newtype Database
  = Database { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "LocationUri" :: NullOrUndefined (URI), "Parameters" :: NullOrUndefined (ParametersMap), "CreateTime" :: NullOrUndefined (Number) }
```

<p>The <code>Database</code> object represents a logical grouping of tables that may reside in a Hive metastore or an RDBMS.</p>

##### Instances
``` purescript
Newtype Database _
```

#### `DatabaseInput`

``` purescript
newtype DatabaseInput
  = DatabaseInput { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "LocationUri" :: NullOrUndefined (URI), "Parameters" :: NullOrUndefined (ParametersMap) }
```

<p>The structure used to create or update a database.</p>

##### Instances
``` purescript
Newtype DatabaseInput _
```

#### `DatabaseList`

``` purescript
newtype DatabaseList
  = DatabaseList (Array Database)
```

##### Instances
``` purescript
Newtype DatabaseList _
```

#### `DatabaseName`

``` purescript
newtype DatabaseName
  = DatabaseName String
```

##### Instances
``` purescript
Newtype DatabaseName _
```

#### `DeleteBehavior`

``` purescript
newtype DeleteBehavior
  = DeleteBehavior String
```

##### Instances
``` purescript
Newtype DeleteBehavior _
```

#### `DeleteClassifierRequest`

``` purescript
newtype DeleteClassifierRequest
  = DeleteClassifierRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype DeleteClassifierRequest _
```

#### `DeleteClassifierResponse`

``` purescript
newtype DeleteClassifierResponse
  = DeleteClassifierResponse {  }
```

##### Instances
``` purescript
Newtype DeleteClassifierResponse _
```

#### `DeleteConnectionNameList`

``` purescript
newtype DeleteConnectionNameList
  = DeleteConnectionNameList (Array NameString)
```

##### Instances
``` purescript
Newtype DeleteConnectionNameList _
```

#### `DeleteConnectionRequest`

``` purescript
newtype DeleteConnectionRequest
  = DeleteConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "ConnectionName" :: NameString }
```

##### Instances
``` purescript
Newtype DeleteConnectionRequest _
```

#### `DeleteConnectionResponse`

``` purescript
newtype DeleteConnectionResponse
  = DeleteConnectionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteConnectionResponse _
```

#### `DeleteCrawlerRequest`

``` purescript
newtype DeleteCrawlerRequest
  = DeleteCrawlerRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype DeleteCrawlerRequest _
```

#### `DeleteCrawlerResponse`

``` purescript
newtype DeleteCrawlerResponse
  = DeleteCrawlerResponse {  }
```

##### Instances
``` purescript
Newtype DeleteCrawlerResponse _
```

#### `DeleteDatabaseRequest`

``` purescript
newtype DeleteDatabaseRequest
  = DeleteDatabaseRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString }
```

##### Instances
``` purescript
Newtype DeleteDatabaseRequest _
```

#### `DeleteDatabaseResponse`

``` purescript
newtype DeleteDatabaseResponse
  = DeleteDatabaseResponse {  }
```

##### Instances
``` purescript
Newtype DeleteDatabaseResponse _
```

#### `DeleteDevEndpointRequest`

``` purescript
newtype DeleteDevEndpointRequest
  = DeleteDevEndpointRequest { "EndpointName" :: GenericString }
```

##### Instances
``` purescript
Newtype DeleteDevEndpointRequest _
```

#### `DeleteDevEndpointResponse`

``` purescript
newtype DeleteDevEndpointResponse
  = DeleteDevEndpointResponse {  }
```

##### Instances
``` purescript
Newtype DeleteDevEndpointResponse _
```

#### `DeleteJobRequest`

``` purescript
newtype DeleteJobRequest
  = DeleteJobRequest { "JobName" :: NameString }
```

##### Instances
``` purescript
Newtype DeleteJobRequest _
```

#### `DeleteJobResponse`

``` purescript
newtype DeleteJobResponse
  = DeleteJobResponse { "JobName" :: NullOrUndefined (NameString) }
```

##### Instances
``` purescript
Newtype DeleteJobResponse _
```

#### `DeletePartitionRequest`

``` purescript
newtype DeletePartitionRequest
  = DeletePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionValues" :: ValueStringList }
```

##### Instances
``` purescript
Newtype DeletePartitionRequest _
```

#### `DeletePartitionResponse`

``` purescript
newtype DeletePartitionResponse
  = DeletePartitionResponse {  }
```

##### Instances
``` purescript
Newtype DeletePartitionResponse _
```

#### `DeleteTableRequest`

``` purescript
newtype DeleteTableRequest
  = DeleteTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "Name" :: NameString }
```

##### Instances
``` purescript
Newtype DeleteTableRequest _
```

#### `DeleteTableResponse`

``` purescript
newtype DeleteTableResponse
  = DeleteTableResponse {  }
```

##### Instances
``` purescript
Newtype DeleteTableResponse _
```

#### `DeleteTableVersionRequest`

``` purescript
newtype DeleteTableVersionRequest
  = DeleteTableVersionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "VersionId" :: VersionString }
```

##### Instances
``` purescript
Newtype DeleteTableVersionRequest _
```

#### `DeleteTableVersionResponse`

``` purescript
newtype DeleteTableVersionResponse
  = DeleteTableVersionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteTableVersionResponse _
```

#### `DeleteTriggerRequest`

``` purescript
newtype DeleteTriggerRequest
  = DeleteTriggerRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype DeleteTriggerRequest _
```

#### `DeleteTriggerResponse`

``` purescript
newtype DeleteTriggerResponse
  = DeleteTriggerResponse { "Name" :: NullOrUndefined (NameString) }
```

##### Instances
``` purescript
Newtype DeleteTriggerResponse _
```

#### `DeleteUserDefinedFunctionRequest`

``` purescript
newtype DeleteUserDefinedFunctionRequest
  = DeleteUserDefinedFunctionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "FunctionName" :: NameString }
```

##### Instances
``` purescript
Newtype DeleteUserDefinedFunctionRequest _
```

#### `DeleteUserDefinedFunctionResponse`

``` purescript
newtype DeleteUserDefinedFunctionResponse
  = DeleteUserDefinedFunctionResponse {  }
```

##### Instances
``` purescript
Newtype DeleteUserDefinedFunctionResponse _
```

#### `DescriptionString`

``` purescript
newtype DescriptionString
  = DescriptionString String
```

##### Instances
``` purescript
Newtype DescriptionString _
```

#### `DescriptionStringRemovable`

``` purescript
newtype DescriptionStringRemovable
  = DescriptionStringRemovable String
```

##### Instances
``` purescript
Newtype DescriptionStringRemovable _
```

#### `DevEndpoint`

``` purescript
newtype DevEndpoint
  = DevEndpoint { "EndpointName" :: NullOrUndefined (GenericString), "RoleArn" :: NullOrUndefined (RoleArn), "SecurityGroupIds" :: NullOrUndefined (StringList), "SubnetId" :: NullOrUndefined (GenericString), "YarnEndpointAddress" :: NullOrUndefined (GenericString), "ZeppelinRemoteSparkInterpreterPort" :: NullOrUndefined (IntegerValue), "PublicAddress" :: NullOrUndefined (GenericString), "Status" :: NullOrUndefined (GenericString), "NumberOfNodes" :: NullOrUndefined (IntegerValue), "AvailabilityZone" :: NullOrUndefined (GenericString), "VpcId" :: NullOrUndefined (GenericString), "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString), "ExtraJarsS3Path" :: NullOrUndefined (GenericString), "FailureReason" :: NullOrUndefined (GenericString), "LastUpdateStatus" :: NullOrUndefined (GenericString), "CreatedTimestamp" :: NullOrUndefined (TimestampValue), "LastModifiedTimestamp" :: NullOrUndefined (TimestampValue), "PublicKey" :: NullOrUndefined (GenericString) }
```

<p>A development endpoint where a developer can remotely debug ETL scripts.</p>

##### Instances
``` purescript
Newtype DevEndpoint _
```

#### `DevEndpointCustomLibraries`

``` purescript
newtype DevEndpointCustomLibraries
  = DevEndpointCustomLibraries { "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString), "ExtraJarsS3Path" :: NullOrUndefined (GenericString) }
```

<p>Custom libraries to be loaded into a DevEndpoint.</p>

##### Instances
``` purescript
Newtype DevEndpointCustomLibraries _
```

#### `DevEndpointList`

``` purescript
newtype DevEndpointList
  = DevEndpointList (Array DevEndpoint)
```

##### Instances
``` purescript
Newtype DevEndpointList _
```

#### `EntityNotFoundException`

``` purescript
newtype EntityNotFoundException
  = EntityNotFoundException { "Message" :: NullOrUndefined (MessageString) }
```

<p>A specified entity does not exist</p>

##### Instances
``` purescript
Newtype EntityNotFoundException _
```

#### `ErrorByName`

``` purescript
newtype ErrorByName
  = ErrorByName (Map NameString ErrorDetail)
```

##### Instances
``` purescript
Newtype ErrorByName _
```

#### `ErrorDetail`

``` purescript
newtype ErrorDetail
  = ErrorDetail { "ErrorCode" :: NullOrUndefined (NameString), "ErrorMessage" :: NullOrUndefined (DescriptionString) }
```

<p>Contains details about an error.</p>

##### Instances
``` purescript
Newtype ErrorDetail _
```

#### `ErrorString`

``` purescript
newtype ErrorString
  = ErrorString String
```

##### Instances
``` purescript
Newtype ErrorString _
```

#### `ExecutionProperty`

``` purescript
newtype ExecutionProperty
  = ExecutionProperty { "MaxConcurrentRuns" :: NullOrUndefined (MaxConcurrentRuns) }
```

<p>An execution property of a job.</p>

##### Instances
``` purescript
Newtype ExecutionProperty _
```

#### `FieldType`

``` purescript
newtype FieldType
  = FieldType String
```

##### Instances
``` purescript
Newtype FieldType _
```

#### `FilterString`

``` purescript
newtype FilterString
  = FilterString String
```

##### Instances
``` purescript
Newtype FilterString _
```

#### `FormatString`

``` purescript
newtype FormatString
  = FormatString String
```

##### Instances
``` purescript
Newtype FormatString _
```

#### `GenericMap`

``` purescript
newtype GenericMap
  = GenericMap (Map GenericString GenericString)
```

##### Instances
``` purescript
Newtype GenericMap _
```

#### `GenericString`

``` purescript
newtype GenericString
  = GenericString String
```

##### Instances
``` purescript
Newtype GenericString _
```

#### `GetCatalogImportStatusRequest`

``` purescript
newtype GetCatalogImportStatusRequest
  = GetCatalogImportStatusRequest { "CatalogId" :: NullOrUndefined (CatalogIdString) }
```

##### Instances
``` purescript
Newtype GetCatalogImportStatusRequest _
```

#### `GetCatalogImportStatusResponse`

``` purescript
newtype GetCatalogImportStatusResponse
  = GetCatalogImportStatusResponse { "ImportStatus" :: NullOrUndefined (CatalogImportStatus) }
```

##### Instances
``` purescript
Newtype GetCatalogImportStatusResponse _
```

#### `GetClassifierRequest`

``` purescript
newtype GetClassifierRequest
  = GetClassifierRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype GetClassifierRequest _
```

#### `GetClassifierResponse`

``` purescript
newtype GetClassifierResponse
  = GetClassifierResponse { "Classifier" :: NullOrUndefined (Classifier) }
```

##### Instances
``` purescript
Newtype GetClassifierResponse _
```

#### `GetClassifiersRequest`

``` purescript
newtype GetClassifiersRequest
  = GetClassifiersRequest { "MaxResults" :: NullOrUndefined (PageSize), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetClassifiersRequest _
```

#### `GetClassifiersResponse`

``` purescript
newtype GetClassifiersResponse
  = GetClassifiersResponse { "Classifiers" :: NullOrUndefined (ClassifierList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetClassifiersResponse _
```

#### `GetConnectionRequest`

``` purescript
newtype GetConnectionRequest
  = GetConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString }
```

##### Instances
``` purescript
Newtype GetConnectionRequest _
```

#### `GetConnectionResponse`

``` purescript
newtype GetConnectionResponse
  = GetConnectionResponse { "Connection" :: NullOrUndefined (Connection) }
```

##### Instances
``` purescript
Newtype GetConnectionResponse _
```

#### `GetConnectionsFilter`

``` purescript
newtype GetConnectionsFilter
  = GetConnectionsFilter { "MatchCriteria" :: NullOrUndefined (MatchCriteria), "ConnectionType" :: NullOrUndefined (ConnectionType) }
```

<p>Filters the connection definitions returned by the <code>GetConnections</code> API.</p>

##### Instances
``` purescript
Newtype GetConnectionsFilter _
```

#### `GetConnectionsRequest`

``` purescript
newtype GetConnectionsRequest
  = GetConnectionsRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Filter" :: NullOrUndefined (GetConnectionsFilter), "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetConnectionsRequest _
```

#### `GetConnectionsResponse`

``` purescript
newtype GetConnectionsResponse
  = GetConnectionsResponse { "ConnectionList" :: NullOrUndefined (ConnectionList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetConnectionsResponse _
```

#### `GetCrawlerMetricsRequest`

``` purescript
newtype GetCrawlerMetricsRequest
  = GetCrawlerMetricsRequest { "CrawlerNameList" :: NullOrUndefined (CrawlerNameList), "MaxResults" :: NullOrUndefined (PageSize), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetCrawlerMetricsRequest _
```

#### `GetCrawlerMetricsResponse`

``` purescript
newtype GetCrawlerMetricsResponse
  = GetCrawlerMetricsResponse { "CrawlerMetricsList" :: NullOrUndefined (CrawlerMetricsList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetCrawlerMetricsResponse _
```

#### `GetCrawlerRequest`

``` purescript
newtype GetCrawlerRequest
  = GetCrawlerRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype GetCrawlerRequest _
```

#### `GetCrawlerResponse`

``` purescript
newtype GetCrawlerResponse
  = GetCrawlerResponse { "Crawler" :: NullOrUndefined (Crawler) }
```

##### Instances
``` purescript
Newtype GetCrawlerResponse _
```

#### `GetCrawlersRequest`

``` purescript
newtype GetCrawlersRequest
  = GetCrawlersRequest { "MaxResults" :: NullOrUndefined (PageSize), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetCrawlersRequest _
```

#### `GetCrawlersResponse`

``` purescript
newtype GetCrawlersResponse
  = GetCrawlersResponse { "Crawlers" :: NullOrUndefined (CrawlerList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetCrawlersResponse _
```

#### `GetDatabaseRequest`

``` purescript
newtype GetDatabaseRequest
  = GetDatabaseRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString }
```

##### Instances
``` purescript
Newtype GetDatabaseRequest _
```

#### `GetDatabaseResponse`

``` purescript
newtype GetDatabaseResponse
  = GetDatabaseResponse { "Database" :: NullOrUndefined (Database) }
```

##### Instances
``` purescript
Newtype GetDatabaseResponse _
```

#### `GetDatabasesRequest`

``` purescript
newtype GetDatabasesRequest
  = GetDatabasesRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetDatabasesRequest _
```

#### `GetDatabasesResponse`

``` purescript
newtype GetDatabasesResponse
  = GetDatabasesResponse { "DatabaseList" :: DatabaseList, "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetDatabasesResponse _
```

#### `GetDataflowGraphRequest`

``` purescript
newtype GetDataflowGraphRequest
  = GetDataflowGraphRequest { "PythonScript" :: NullOrUndefined (PythonScript) }
```

##### Instances
``` purescript
Newtype GetDataflowGraphRequest _
```

#### `GetDataflowGraphResponse`

``` purescript
newtype GetDataflowGraphResponse
  = GetDataflowGraphResponse { "DagNodes" :: NullOrUndefined (DagNodes), "DagEdges" :: NullOrUndefined (DagEdges) }
```

##### Instances
``` purescript
Newtype GetDataflowGraphResponse _
```

#### `GetDevEndpointRequest`

``` purescript
newtype GetDevEndpointRequest
  = GetDevEndpointRequest { "EndpointName" :: GenericString }
```

##### Instances
``` purescript
Newtype GetDevEndpointRequest _
```

#### `GetDevEndpointResponse`

``` purescript
newtype GetDevEndpointResponse
  = GetDevEndpointResponse { "DevEndpoint" :: NullOrUndefined (DevEndpoint) }
```

##### Instances
``` purescript
Newtype GetDevEndpointResponse _
```

#### `GetDevEndpointsRequest`

``` purescript
newtype GetDevEndpointsRequest
  = GetDevEndpointsRequest { "MaxResults" :: NullOrUndefined (PageSize), "NextToken" :: NullOrUndefined (GenericString) }
```

##### Instances
``` purescript
Newtype GetDevEndpointsRequest _
```

#### `GetDevEndpointsResponse`

``` purescript
newtype GetDevEndpointsResponse
  = GetDevEndpointsResponse { "DevEndpoints" :: NullOrUndefined (DevEndpointList), "NextToken" :: NullOrUndefined (GenericString) }
```

##### Instances
``` purescript
Newtype GetDevEndpointsResponse _
```

#### `GetJobRequest`

``` purescript
newtype GetJobRequest
  = GetJobRequest { "JobName" :: NameString }
```

##### Instances
``` purescript
Newtype GetJobRequest _
```

#### `GetJobResponse`

``` purescript
newtype GetJobResponse
  = GetJobResponse { "Job" :: NullOrUndefined (Job) }
```

##### Instances
``` purescript
Newtype GetJobResponse _
```

#### `GetJobRunRequest`

``` purescript
newtype GetJobRunRequest
  = GetJobRunRequest { "JobName" :: NameString, "RunId" :: IdString, "PredecessorsIncluded" :: NullOrUndefined (BooleanValue) }
```

##### Instances
``` purescript
Newtype GetJobRunRequest _
```

#### `GetJobRunResponse`

``` purescript
newtype GetJobRunResponse
  = GetJobRunResponse { "JobRun" :: NullOrUndefined (JobRun) }
```

##### Instances
``` purescript
Newtype GetJobRunResponse _
```

#### `GetJobRunsRequest`

``` purescript
newtype GetJobRunsRequest
  = GetJobRunsRequest { "JobName" :: NameString, "NextToken" :: NullOrUndefined (GenericString), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetJobRunsRequest _
```

#### `GetJobRunsResponse`

``` purescript
newtype GetJobRunsResponse
  = GetJobRunsResponse { "JobRuns" :: NullOrUndefined (JobRunList), "NextToken" :: NullOrUndefined (GenericString) }
```

##### Instances
``` purescript
Newtype GetJobRunsResponse _
```

#### `GetJobsRequest`

``` purescript
newtype GetJobsRequest
  = GetJobsRequest { "NextToken" :: NullOrUndefined (GenericString), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetJobsRequest _
```

#### `GetJobsResponse`

``` purescript
newtype GetJobsResponse
  = GetJobsResponse { "Jobs" :: NullOrUndefined (JobList), "NextToken" :: NullOrUndefined (GenericString) }
```

##### Instances
``` purescript
Newtype GetJobsResponse _
```

#### `GetMappingRequest`

``` purescript
newtype GetMappingRequest
  = GetMappingRequest { "Source" :: CatalogEntry, "Sinks" :: NullOrUndefined (CatalogEntries), "Location" :: NullOrUndefined (Location) }
```

##### Instances
``` purescript
Newtype GetMappingRequest _
```

#### `GetMappingResponse`

``` purescript
newtype GetMappingResponse
  = GetMappingResponse { "Mapping" :: MappingList }
```

##### Instances
``` purescript
Newtype GetMappingResponse _
```

#### `GetPartitionRequest`

``` purescript
newtype GetPartitionRequest
  = GetPartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionValues" :: ValueStringList }
```

##### Instances
``` purescript
Newtype GetPartitionRequest _
```

#### `GetPartitionResponse`

``` purescript
newtype GetPartitionResponse
  = GetPartitionResponse { "Partition" :: NullOrUndefined (Partition) }
```

##### Instances
``` purescript
Newtype GetPartitionResponse _
```

#### `GetPartitionsRequest`

``` purescript
newtype GetPartitionsRequest
  = GetPartitionsRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "Expression" :: NullOrUndefined (PredicateString), "NextToken" :: NullOrUndefined (Token), "Segment" :: NullOrUndefined (Segment), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetPartitionsRequest _
```

#### `GetPartitionsResponse`

``` purescript
newtype GetPartitionsResponse
  = GetPartitionsResponse { "Partitions" :: NullOrUndefined (PartitionList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetPartitionsResponse _
```

#### `GetPlanRequest`

``` purescript
newtype GetPlanRequest
  = GetPlanRequest { "Mapping" :: MappingList, "Source" :: CatalogEntry, "Sinks" :: NullOrUndefined (CatalogEntries), "Location" :: NullOrUndefined (Location), "Language" :: NullOrUndefined (Language) }
```

##### Instances
``` purescript
Newtype GetPlanRequest _
```

#### `GetPlanResponse`

``` purescript
newtype GetPlanResponse
  = GetPlanResponse { "PythonScript" :: NullOrUndefined (PythonScript), "ScalaCode" :: NullOrUndefined (ScalaCode) }
```

##### Instances
``` purescript
Newtype GetPlanResponse _
```

#### `GetTableRequest`

``` purescript
newtype GetTableRequest
  = GetTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "Name" :: NameString }
```

##### Instances
``` purescript
Newtype GetTableRequest _
```

#### `GetTableResponse`

``` purescript
newtype GetTableResponse
  = GetTableResponse { "Table" :: NullOrUndefined (Table) }
```

##### Instances
``` purescript
Newtype GetTableResponse _
```

#### `GetTableVersionRequest`

``` purescript
newtype GetTableVersionRequest
  = GetTableVersionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "VersionId" :: NullOrUndefined (VersionString) }
```

##### Instances
``` purescript
Newtype GetTableVersionRequest _
```

#### `GetTableVersionResponse`

``` purescript
newtype GetTableVersionResponse
  = GetTableVersionResponse { "TableVersion" :: NullOrUndefined (TableVersion) }
```

##### Instances
``` purescript
Newtype GetTableVersionResponse _
```

#### `GetTableVersionsList`

``` purescript
newtype GetTableVersionsList
  = GetTableVersionsList (Array TableVersion)
```

##### Instances
``` purescript
Newtype GetTableVersionsList _
```

#### `GetTableVersionsRequest`

``` purescript
newtype GetTableVersionsRequest
  = GetTableVersionsRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetTableVersionsRequest _
```

#### `GetTableVersionsResponse`

``` purescript
newtype GetTableVersionsResponse
  = GetTableVersionsResponse { "TableVersions" :: NullOrUndefined (GetTableVersionsList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetTableVersionsResponse _
```

#### `GetTablesRequest`

``` purescript
newtype GetTablesRequest
  = GetTablesRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "Expression" :: NullOrUndefined (FilterString), "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetTablesRequest _
```

#### `GetTablesResponse`

``` purescript
newtype GetTablesResponse
  = GetTablesResponse { "TableList" :: NullOrUndefined (TableList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetTablesResponse _
```

#### `GetTriggerRequest`

``` purescript
newtype GetTriggerRequest
  = GetTriggerRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype GetTriggerRequest _
```

#### `GetTriggerResponse`

``` purescript
newtype GetTriggerResponse
  = GetTriggerResponse { "Trigger" :: NullOrUndefined (Trigger) }
```

##### Instances
``` purescript
Newtype GetTriggerResponse _
```

#### `GetTriggersRequest`

``` purescript
newtype GetTriggersRequest
  = GetTriggersRequest { "NextToken" :: NullOrUndefined (GenericString), "DependentJobName" :: NullOrUndefined (NameString), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetTriggersRequest _
```

#### `GetTriggersResponse`

``` purescript
newtype GetTriggersResponse
  = GetTriggersResponse { "Triggers" :: NullOrUndefined (TriggerList), "NextToken" :: NullOrUndefined (GenericString) }
```

##### Instances
``` purescript
Newtype GetTriggersResponse _
```

#### `GetUserDefinedFunctionRequest`

``` purescript
newtype GetUserDefinedFunctionRequest
  = GetUserDefinedFunctionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "FunctionName" :: NameString }
```

##### Instances
``` purescript
Newtype GetUserDefinedFunctionRequest _
```

#### `GetUserDefinedFunctionResponse`

``` purescript
newtype GetUserDefinedFunctionResponse
  = GetUserDefinedFunctionResponse { "UserDefinedFunction" :: NullOrUndefined (UserDefinedFunction) }
```

##### Instances
``` purescript
Newtype GetUserDefinedFunctionResponse _
```

#### `GetUserDefinedFunctionsRequest`

``` purescript
newtype GetUserDefinedFunctionsRequest
  = GetUserDefinedFunctionsRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "Pattern" :: NameString, "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (PageSize) }
```

##### Instances
``` purescript
Newtype GetUserDefinedFunctionsRequest _
```

#### `GetUserDefinedFunctionsResponse`

``` purescript
newtype GetUserDefinedFunctionsResponse
  = GetUserDefinedFunctionsResponse { "UserDefinedFunctions" :: NullOrUndefined (UserDefinedFunctionList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetUserDefinedFunctionsResponse _
```

#### `GrokClassifier`

``` purescript
newtype GrokClassifier
  = GrokClassifier { "Name" :: NameString, "Classification" :: Classification, "CreationTime" :: NullOrUndefined (Number), "LastUpdated" :: NullOrUndefined (Number), "Version" :: NullOrUndefined (VersionId), "GrokPattern" :: GrokPattern, "CustomPatterns" :: NullOrUndefined (CustomPatterns) }
```

<p>A classifier that uses <code>grok</code> patterns.</p>

##### Instances
``` purescript
Newtype GrokClassifier _
```

#### `GrokPattern`

``` purescript
newtype GrokPattern
  = GrokPattern String
```

##### Instances
``` purescript
Newtype GrokPattern _
```

#### `IdString`

``` purescript
newtype IdString
  = IdString String
```

##### Instances
``` purescript
Newtype IdString _
```

#### `IdempotentParameterMismatchException`

``` purescript
newtype IdempotentParameterMismatchException
  = IdempotentParameterMismatchException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The same unique identifier was associated with two different records.</p>

##### Instances
``` purescript
Newtype IdempotentParameterMismatchException _
```

#### `ImportCatalogToGlueRequest`

``` purescript
newtype ImportCatalogToGlueRequest
  = ImportCatalogToGlueRequest { "CatalogId" :: NullOrUndefined (CatalogIdString) }
```

##### Instances
``` purescript
Newtype ImportCatalogToGlueRequest _
```

#### `ImportCatalogToGlueResponse`

``` purescript
newtype ImportCatalogToGlueResponse
  = ImportCatalogToGlueResponse {  }
```

##### Instances
``` purescript
Newtype ImportCatalogToGlueResponse _
```

#### `IntegerFlag`

``` purescript
newtype IntegerFlag
  = IntegerFlag Int
```

##### Instances
``` purescript
Newtype IntegerFlag _
```

#### `IntegerValue`

``` purescript
newtype IntegerValue
  = IntegerValue Int
```

##### Instances
``` purescript
Newtype IntegerValue _
```

#### `InternalServiceException`

``` purescript
newtype InternalServiceException
  = InternalServiceException { "Message" :: NullOrUndefined (MessageString) }
```

<p>An internal service error occurred.</p>

##### Instances
``` purescript
Newtype InternalServiceException _
```

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The input provided was not valid.</p>

##### Instances
``` purescript
Newtype InvalidInputException _
```

#### `JdbcTarget`

``` purescript
newtype JdbcTarget
  = JdbcTarget { "ConnectionName" :: NullOrUndefined (ConnectionName), "Path" :: NullOrUndefined (Path), "Exclusions" :: NullOrUndefined (PathList) }
```

<p>Specifies a JDBC data store to crawl.</p>

##### Instances
``` purescript
Newtype JdbcTarget _
```

#### `JdbcTargetList`

``` purescript
newtype JdbcTargetList
  = JdbcTargetList (Array JdbcTarget)
```

##### Instances
``` purescript
Newtype JdbcTargetList _
```

#### `Job`

``` purescript
newtype Job
  = Job { "Name" :: NullOrUndefined (NameString), "Description" :: NullOrUndefined (DescriptionString), "LogUri" :: NullOrUndefined (UriString), "Role" :: NullOrUndefined (RoleString), "CreatedOn" :: NullOrUndefined (TimestampValue), "LastModifiedOn" :: NullOrUndefined (TimestampValue), "ExecutionProperty" :: NullOrUndefined (ExecutionProperty), "Command" :: NullOrUndefined (JobCommand), "DefaultArguments" :: NullOrUndefined (GenericMap), "Connections" :: NullOrUndefined (ConnectionsList), "MaxRetries" :: NullOrUndefined (MaxRetries), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

<p>Specifies a job.</p>

##### Instances
``` purescript
Newtype Job _
```

#### `JobBookmarkEntry`

``` purescript
newtype JobBookmarkEntry
  = JobBookmarkEntry { "JobName" :: NullOrUndefined (JobName), "Version" :: NullOrUndefined (IntegerValue), "Run" :: NullOrUndefined (IntegerValue), "Attempt" :: NullOrUndefined (IntegerValue), "JobBookmark" :: NullOrUndefined (JsonValue) }
```

<p>Defines a point which a job can resume processing.</p>

##### Instances
``` purescript
Newtype JobBookmarkEntry _
```

#### `JobCommand`

``` purescript
newtype JobCommand
  = JobCommand { "Name" :: NullOrUndefined (GenericString), "ScriptLocation" :: NullOrUndefined (ScriptLocationString) }
```

<p>Specifies code that executes a job.</p>

##### Instances
``` purescript
Newtype JobCommand _
```

#### `JobList`

``` purescript
newtype JobList
  = JobList (Array Job)
```

##### Instances
``` purescript
Newtype JobList _
```

#### `JobName`

``` purescript
newtype JobName
  = JobName String
```

##### Instances
``` purescript
Newtype JobName _
```

#### `JobRun`

``` purescript
newtype JobRun
  = JobRun { "Id" :: NullOrUndefined (IdString), "Attempt" :: NullOrUndefined (AttemptCount), "PreviousRunId" :: NullOrUndefined (IdString), "TriggerName" :: NullOrUndefined (NameString), "JobName" :: NullOrUndefined (NameString), "StartedOn" :: NullOrUndefined (TimestampValue), "LastModifiedOn" :: NullOrUndefined (TimestampValue), "CompletedOn" :: NullOrUndefined (TimestampValue), "JobRunState" :: NullOrUndefined (JobRunState), "Arguments" :: NullOrUndefined (GenericMap), "ErrorMessage" :: NullOrUndefined (ErrorString), "PredecessorRuns" :: NullOrUndefined (PredecessorList), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

<p>Contains information about a job run.</p>

##### Instances
``` purescript
Newtype JobRun _
```

#### `JobRunList`

``` purescript
newtype JobRunList
  = JobRunList (Array JobRun)
```

##### Instances
``` purescript
Newtype JobRunList _
```

#### `JobRunState`

``` purescript
newtype JobRunState
  = JobRunState String
```

##### Instances
``` purescript
Newtype JobRunState _
```

#### `JobUpdate`

``` purescript
newtype JobUpdate
  = JobUpdate { "Description" :: NullOrUndefined (DescriptionString), "LogUri" :: NullOrUndefined (UriString), "Role" :: NullOrUndefined (RoleString), "ExecutionProperty" :: NullOrUndefined (ExecutionProperty), "Command" :: NullOrUndefined (JobCommand), "DefaultArguments" :: NullOrUndefined (GenericMap), "Connections" :: NullOrUndefined (ConnectionsList), "MaxRetries" :: NullOrUndefined (MaxRetries), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

<p>Specifies information used to update an existing job. Note that the previous job definition will be completely overwritten by this information.</p>

##### Instances
``` purescript
Newtype JobUpdate _
```

#### `JsonClassifier`

``` purescript
newtype JsonClassifier
  = JsonClassifier { "Name" :: NameString, "CreationTime" :: NullOrUndefined (Number), "LastUpdated" :: NullOrUndefined (Number), "Version" :: NullOrUndefined (VersionId), "JsonPath" :: JsonPath }
```

<p>A classifier for <code>JSON</code> content.</p>

##### Instances
``` purescript
Newtype JsonClassifier _
```

#### `JsonPath`

``` purescript
newtype JsonPath
  = JsonPath String
```

##### Instances
``` purescript
Newtype JsonPath _
```

#### `JsonValue`

``` purescript
newtype JsonValue
  = JsonValue String
```

##### Instances
``` purescript
Newtype JsonValue _
```

#### `KeyString`

``` purescript
newtype KeyString
  = KeyString String
```

##### Instances
``` purescript
Newtype KeyString _
```

#### `Language`

``` purescript
newtype Language
  = Language String
```

##### Instances
``` purescript
Newtype Language _
```

#### `LastCrawlInfo`

``` purescript
newtype LastCrawlInfo
  = LastCrawlInfo { "Status" :: NullOrUndefined (LastCrawlStatus), "ErrorMessage" :: NullOrUndefined (DescriptionString), "LogGroup" :: NullOrUndefined (LogGroup), "LogStream" :: NullOrUndefined (LogStream), "MessagePrefix" :: NullOrUndefined (MessagePrefix), "StartTime" :: NullOrUndefined (Number) }
```

<p>Status and error information about the most recent crawl.</p>

##### Instances
``` purescript
Newtype LastCrawlInfo _
```

#### `LastCrawlStatus`

``` purescript
newtype LastCrawlStatus
  = LastCrawlStatus String
```

##### Instances
``` purescript
Newtype LastCrawlStatus _
```

#### `Location`

``` purescript
newtype Location
  = Location { "Jdbc" :: NullOrUndefined (CodeGenNodeArgs), "S3" :: NullOrUndefined (CodeGenNodeArgs) }
```

<p>The location of resources.</p>

##### Instances
``` purescript
Newtype Location _
```

#### `LocationMap`

``` purescript
newtype LocationMap
  = LocationMap (Map ColumnValuesString ColumnValuesString)
```

##### Instances
``` purescript
Newtype LocationMap _
```

#### `LocationString`

``` purescript
newtype LocationString
  = LocationString String
```

##### Instances
``` purescript
Newtype LocationString _
```

#### `LogGroup`

``` purescript
newtype LogGroup
  = LogGroup String
```

##### Instances
``` purescript
Newtype LogGroup _
```

#### `LogStream`

``` purescript
newtype LogStream
  = LogStream String
```

##### Instances
``` purescript
Newtype LogStream _
```

#### `Logical`

``` purescript
newtype Logical
  = Logical String
```

##### Instances
``` purescript
Newtype Logical _
```

#### `LogicalOperator`

``` purescript
newtype LogicalOperator
  = LogicalOperator String
```

##### Instances
``` purescript
Newtype LogicalOperator _
```

#### `MappingEntry`

``` purescript
newtype MappingEntry
  = MappingEntry { "SourceTable" :: NullOrUndefined (TableName), "SourcePath" :: NullOrUndefined (SchemaPathString), "SourceType" :: NullOrUndefined (FieldType), "TargetTable" :: NullOrUndefined (TableName), "TargetPath" :: NullOrUndefined (SchemaPathString), "TargetType" :: NullOrUndefined (FieldType) }
```

<p>Defines a mapping.</p>

##### Instances
``` purescript
Newtype MappingEntry _
```

#### `MappingList`

``` purescript
newtype MappingList
  = MappingList (Array MappingEntry)
```

##### Instances
``` purescript
Newtype MappingList _
```

#### `MatchCriteria`

``` purescript
newtype MatchCriteria
  = MatchCriteria (Array NameString)
```

##### Instances
``` purescript
Newtype MatchCriteria _
```

#### `MaxConcurrentRuns`

``` purescript
newtype MaxConcurrentRuns
  = MaxConcurrentRuns Int
```

##### Instances
``` purescript
Newtype MaxConcurrentRuns _
```

#### `MaxRetries`

``` purescript
newtype MaxRetries
  = MaxRetries Int
```

##### Instances
``` purescript
Newtype MaxRetries _
```

#### `MessagePrefix`

``` purescript
newtype MessagePrefix
  = MessagePrefix String
```

##### Instances
``` purescript
Newtype MessagePrefix _
```

#### `MessageString`

``` purescript
newtype MessageString
  = MessageString String
```

##### Instances
``` purescript
Newtype MessageString _
```

#### `MillisecondsCount`

``` purescript
newtype MillisecondsCount
  = MillisecondsCount Number
```

##### Instances
``` purescript
Newtype MillisecondsCount _
```

#### `NameString`

``` purescript
newtype NameString
  = NameString String
```

##### Instances
``` purescript
Newtype NameString _
```

#### `NameStringList`

``` purescript
newtype NameStringList
  = NameStringList (Array NameString)
```

##### Instances
``` purescript
Newtype NameStringList _
```

#### `NoScheduleException`

``` purescript
newtype NoScheduleException
  = NoScheduleException { "Message" :: NullOrUndefined (MessageString) }
```

<p>There is no applicable schedule.</p>

##### Instances
``` purescript
Newtype NoScheduleException _
```

#### `NonNegativeDouble`

``` purescript
newtype NonNegativeDouble
  = NonNegativeDouble Number
```

##### Instances
``` purescript
Newtype NonNegativeDouble _
```

#### `NonNegativeInteger`

``` purescript
newtype NonNegativeInteger
  = NonNegativeInteger Int
```

##### Instances
``` purescript
Newtype NonNegativeInteger _
```

#### `OperationTimeoutException`

``` purescript
newtype OperationTimeoutException
  = OperationTimeoutException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The operation timed out.</p>

##### Instances
``` purescript
Newtype OperationTimeoutException _
```

#### `Order`

``` purescript
newtype Order
  = Order { "Column" :: NameString, "SortOrder" :: IntegerFlag }
```

<p>Specifies the sort order of a sorted column.</p>

##### Instances
``` purescript
Newtype Order _
```

#### `OrderList`

``` purescript
newtype OrderList
  = OrderList (Array Order)
```

##### Instances
``` purescript
Newtype OrderList _
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

##### Instances
``` purescript
Newtype PageSize _
```

#### `ParametersMap`

``` purescript
newtype ParametersMap
  = ParametersMap (Map KeyString ParametersMapValue)
```

##### Instances
``` purescript
Newtype ParametersMap _
```

#### `ParametersMapValue`

``` purescript
newtype ParametersMapValue
  = ParametersMapValue String
```

##### Instances
``` purescript
Newtype ParametersMapValue _
```

#### `Partition`

``` purescript
newtype Partition
  = Partition { "Values" :: NullOrUndefined (ValueStringList), "DatabaseName" :: NullOrUndefined (NameString), "TableName" :: NullOrUndefined (NameString), "CreationTime" :: NullOrUndefined (Number), "LastAccessTime" :: NullOrUndefined (Number), "StorageDescriptor" :: NullOrUndefined (StorageDescriptor), "Parameters" :: NullOrUndefined (ParametersMap), "LastAnalyzedTime" :: NullOrUndefined (Number) }
```

<p>Represents a slice of table data.</p>

##### Instances
``` purescript
Newtype Partition _
```

#### `PartitionError`

``` purescript
newtype PartitionError
  = PartitionError { "PartitionValues" :: NullOrUndefined (ValueStringList), "ErrorDetail" :: NullOrUndefined (ErrorDetail) }
```

<p>Contains information about a partition error.</p>

##### Instances
``` purescript
Newtype PartitionError _
```

#### `PartitionErrors`

``` purescript
newtype PartitionErrors
  = PartitionErrors (Array PartitionError)
```

##### Instances
``` purescript
Newtype PartitionErrors _
```

#### `PartitionInput`

``` purescript
newtype PartitionInput
  = PartitionInput { "Values" :: NullOrUndefined (ValueStringList), "LastAccessTime" :: NullOrUndefined (Number), "StorageDescriptor" :: NullOrUndefined (StorageDescriptor), "Parameters" :: NullOrUndefined (ParametersMap), "LastAnalyzedTime" :: NullOrUndefined (Number) }
```

<p>The structure used to create and update a partion.</p>

##### Instances
``` purescript
Newtype PartitionInput _
```

#### `PartitionInputList`

``` purescript
newtype PartitionInputList
  = PartitionInputList (Array PartitionInput)
```

##### Instances
``` purescript
Newtype PartitionInputList _
```

#### `PartitionList`

``` purescript
newtype PartitionList
  = PartitionList (Array Partition)
```

##### Instances
``` purescript
Newtype PartitionList _
```

#### `PartitionValueList`

``` purescript
newtype PartitionValueList
  = PartitionValueList { "Values" :: ValueStringList }
```

<p>Contains a list of values defining partitions.</p>

##### Instances
``` purescript
Newtype PartitionValueList _
```

#### `Path`

``` purescript
newtype Path
  = Path String
```

##### Instances
``` purescript
Newtype Path _
```

#### `PathList`

``` purescript
newtype PathList
  = PathList (Array Path)
```

##### Instances
``` purescript
Newtype PathList _
```

#### `PhysicalConnectionRequirements`

``` purescript
newtype PhysicalConnectionRequirements
  = PhysicalConnectionRequirements { "SubnetId" :: NullOrUndefined (NameString), "SecurityGroupIdList" :: NullOrUndefined (SecurityGroupIdList), "AvailabilityZone" :: NullOrUndefined (NameString) }
```

<p>Specifies the physical requirements for a connection.</p>

##### Instances
``` purescript
Newtype PhysicalConnectionRequirements _
```

#### `Predecessor`

``` purescript
newtype Predecessor
  = Predecessor { "JobName" :: NullOrUndefined (NameString), "RunId" :: NullOrUndefined (IdString) }
```

<p>A job run that was used in the predicate of a conditional trigger that triggered this job run.</p>

##### Instances
``` purescript
Newtype Predecessor _
```

#### `PredecessorList`

``` purescript
newtype PredecessorList
  = PredecessorList (Array Predecessor)
```

##### Instances
``` purescript
Newtype PredecessorList _
```

#### `Predicate`

``` purescript
newtype Predicate
  = Predicate { "Logical" :: NullOrUndefined (Logical), "Conditions" :: NullOrUndefined (ConditionList) }
```

<p>Defines the predicate of the trigger, which determines when it fires.</p>

##### Instances
``` purescript
Newtype Predicate _
```

#### `PredicateString`

``` purescript
newtype PredicateString
  = PredicateString String
```

##### Instances
``` purescript
Newtype PredicateString _
```

#### `PrincipalType`

``` purescript
newtype PrincipalType
  = PrincipalType String
```

##### Instances
``` purescript
Newtype PrincipalType _
```

#### `PythonScript`

``` purescript
newtype PythonScript
  = PythonScript String
```

##### Instances
``` purescript
Newtype PythonScript _
```

#### `ResetJobBookmarkRequest`

``` purescript
newtype ResetJobBookmarkRequest
  = ResetJobBookmarkRequest { "JobName" :: JobName }
```

##### Instances
``` purescript
Newtype ResetJobBookmarkRequest _
```

#### `ResetJobBookmarkResponse`

``` purescript
newtype ResetJobBookmarkResponse
  = ResetJobBookmarkResponse { "JobBookmarkEntry" :: NullOrUndefined (JobBookmarkEntry) }
```

##### Instances
``` purescript
Newtype ResetJobBookmarkResponse _
```

#### `ResourceNumberLimitExceededException`

``` purescript
newtype ResourceNumberLimitExceededException
  = ResourceNumberLimitExceededException { "Message" :: NullOrUndefined (MessageString) }
```

<p>A resource numerical limit was exceeded.</p>

##### Instances
``` purescript
Newtype ResourceNumberLimitExceededException _
```

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

##### Instances
``` purescript
Newtype ResourceType _
```

#### `ResourceUri`

``` purescript
newtype ResourceUri
  = ResourceUri { "ResourceType" :: NullOrUndefined (ResourceType), "Uri" :: NullOrUndefined (URI) }
```

<p>URIs for function resources.</p>

##### Instances
``` purescript
Newtype ResourceUri _
```

#### `ResourceUriList`

``` purescript
newtype ResourceUriList
  = ResourceUriList (Array ResourceUri)
```

##### Instances
``` purescript
Newtype ResourceUriList _
```

#### `Role`

``` purescript
newtype Role
  = Role String
```

##### Instances
``` purescript
Newtype Role _
```

#### `RoleArn`

``` purescript
newtype RoleArn
  = RoleArn String
```

##### Instances
``` purescript
Newtype RoleArn _
```

#### `RoleString`

``` purescript
newtype RoleString
  = RoleString String
```

##### Instances
``` purescript
Newtype RoleString _
```

#### `RowTag`

``` purescript
newtype RowTag
  = RowTag String
```

##### Instances
``` purescript
Newtype RowTag _
```

#### `S3Target`

``` purescript
newtype S3Target
  = S3Target { "Path" :: NullOrUndefined (Path), "Exclusions" :: NullOrUndefined (PathList) }
```

<p>Specifies a data store in Amazon S3.</p>

##### Instances
``` purescript
Newtype S3Target _
```

#### `S3TargetList`

``` purescript
newtype S3TargetList
  = S3TargetList (Array S3Target)
```

##### Instances
``` purescript
Newtype S3TargetList _
```

#### `ScalaCode`

``` purescript
newtype ScalaCode
  = ScalaCode String
```

##### Instances
``` purescript
Newtype ScalaCode _
```

#### `Schedule`

``` purescript
newtype Schedule
  = Schedule { "ScheduleExpression" :: NullOrUndefined (CronExpression), "State" :: NullOrUndefined (ScheduleState) }
```

<p>A scheduling object using a <code>cron</code> statement to schedule an event.</p>

##### Instances
``` purescript
Newtype Schedule _
```

#### `ScheduleState`

``` purescript
newtype ScheduleState
  = ScheduleState String
```

##### Instances
``` purescript
Newtype ScheduleState _
```

#### `SchedulerNotRunningException`

``` purescript
newtype SchedulerNotRunningException
  = SchedulerNotRunningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified scheduler is not running.</p>

##### Instances
``` purescript
Newtype SchedulerNotRunningException _
```

#### `SchedulerRunningException`

``` purescript
newtype SchedulerRunningException
  = SchedulerRunningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified scheduler is already running.</p>

##### Instances
``` purescript
Newtype SchedulerRunningException _
```

#### `SchedulerTransitioningException`

``` purescript
newtype SchedulerTransitioningException
  = SchedulerTransitioningException { "Message" :: NullOrUndefined (MessageString) }
```

<p>The specified scheduler is transitioning.</p>

##### Instances
``` purescript
Newtype SchedulerTransitioningException _
```

#### `SchemaChangePolicy`

``` purescript
newtype SchemaChangePolicy
  = SchemaChangePolicy { "UpdateBehavior" :: NullOrUndefined (UpdateBehavior), "DeleteBehavior" :: NullOrUndefined (DeleteBehavior) }
```

<p>Crawler policy for update and deletion behavior.</p>

##### Instances
``` purescript
Newtype SchemaChangePolicy _
```

#### `SchemaPathString`

``` purescript
newtype SchemaPathString
  = SchemaPathString String
```

##### Instances
``` purescript
Newtype SchemaPathString _
```

#### `ScriptLocationString`

``` purescript
newtype ScriptLocationString
  = ScriptLocationString String
```

##### Instances
``` purescript
Newtype ScriptLocationString _
```

#### `SecurityGroupIdList`

``` purescript
newtype SecurityGroupIdList
  = SecurityGroupIdList (Array NameString)
```

##### Instances
``` purescript
Newtype SecurityGroupIdList _
```

#### `Segment`

``` purescript
newtype Segment
  = Segment { "SegmentNumber" :: NonNegativeInteger, "TotalSegments" :: TotalSegmentsInteger }
```

<p>Defines a non-overlapping region of a table's partitions, allowing multiple requests to be executed in parallel.</p>

##### Instances
``` purescript
Newtype Segment _
```

#### `SerDeInfo`

``` purescript
newtype SerDeInfo
  = SerDeInfo { "Name" :: NullOrUndefined (NameString), "SerializationLibrary" :: NullOrUndefined (NameString), "Parameters" :: NullOrUndefined (ParametersMap) }
```

<p>Information about a serialization/deserialization program (SerDe) which serves as an extractor and loader.</p>

##### Instances
``` purescript
Newtype SerDeInfo _
```

#### `SkewedInfo`

``` purescript
newtype SkewedInfo
  = SkewedInfo { "SkewedColumnNames" :: NullOrUndefined (NameStringList), "SkewedColumnValues" :: NullOrUndefined (ColumnValueStringList), "SkewedColumnValueLocationMaps" :: NullOrUndefined (LocationMap) }
```

<p>Specifies skewed values in a table. Skewed are ones that occur with very high frequency.</p>

##### Instances
``` purescript
Newtype SkewedInfo _
```

#### `StartCrawlerRequest`

``` purescript
newtype StartCrawlerRequest
  = StartCrawlerRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype StartCrawlerRequest _
```

#### `StartCrawlerResponse`

``` purescript
newtype StartCrawlerResponse
  = StartCrawlerResponse {  }
```

##### Instances
``` purescript
Newtype StartCrawlerResponse _
```

#### `StartCrawlerScheduleRequest`

``` purescript
newtype StartCrawlerScheduleRequest
  = StartCrawlerScheduleRequest { "CrawlerName" :: NameString }
```

##### Instances
``` purescript
Newtype StartCrawlerScheduleRequest _
```

#### `StartCrawlerScheduleResponse`

``` purescript
newtype StartCrawlerScheduleResponse
  = StartCrawlerScheduleResponse {  }
```

##### Instances
``` purescript
Newtype StartCrawlerScheduleResponse _
```

#### `StartJobRunRequest`

``` purescript
newtype StartJobRunRequest
  = StartJobRunRequest { "JobName" :: NameString, "JobRunId" :: NullOrUndefined (IdString), "Arguments" :: NullOrUndefined (GenericMap), "AllocatedCapacity" :: NullOrUndefined (IntegerValue) }
```

##### Instances
``` purescript
Newtype StartJobRunRequest _
```

#### `StartJobRunResponse`

``` purescript
newtype StartJobRunResponse
  = StartJobRunResponse { "JobRunId" :: NullOrUndefined (IdString) }
```

##### Instances
``` purescript
Newtype StartJobRunResponse _
```

#### `StartTriggerRequest`

``` purescript
newtype StartTriggerRequest
  = StartTriggerRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype StartTriggerRequest _
```

#### `StartTriggerResponse`

``` purescript
newtype StartTriggerResponse
  = StartTriggerResponse { "Name" :: NullOrUndefined (NameString) }
```

##### Instances
``` purescript
Newtype StartTriggerResponse _
```

#### `StopCrawlerRequest`

``` purescript
newtype StopCrawlerRequest
  = StopCrawlerRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype StopCrawlerRequest _
```

#### `StopCrawlerResponse`

``` purescript
newtype StopCrawlerResponse
  = StopCrawlerResponse {  }
```

##### Instances
``` purescript
Newtype StopCrawlerResponse _
```

#### `StopCrawlerScheduleRequest`

``` purescript
newtype StopCrawlerScheduleRequest
  = StopCrawlerScheduleRequest { "CrawlerName" :: NameString }
```

##### Instances
``` purescript
Newtype StopCrawlerScheduleRequest _
```

#### `StopCrawlerScheduleResponse`

``` purescript
newtype StopCrawlerScheduleResponse
  = StopCrawlerScheduleResponse {  }
```

##### Instances
``` purescript
Newtype StopCrawlerScheduleResponse _
```

#### `StopTriggerRequest`

``` purescript
newtype StopTriggerRequest
  = StopTriggerRequest { "Name" :: NameString }
```

##### Instances
``` purescript
Newtype StopTriggerRequest _
```

#### `StopTriggerResponse`

``` purescript
newtype StopTriggerResponse
  = StopTriggerResponse { "Name" :: NullOrUndefined (NameString) }
```

##### Instances
``` purescript
Newtype StopTriggerResponse _
```

#### `StorageDescriptor`

``` purescript
newtype StorageDescriptor
  = StorageDescriptor { "Columns" :: NullOrUndefined (ColumnList), "Location" :: NullOrUndefined (LocationString), "InputFormat" :: NullOrUndefined (FormatString), "OutputFormat" :: NullOrUndefined (FormatString), "Compressed" :: NullOrUndefined (Boolean), "NumberOfBuckets" :: NullOrUndefined (Int), "SerdeInfo" :: NullOrUndefined (SerDeInfo), "BucketColumns" :: NullOrUndefined (NameStringList), "SortColumns" :: NullOrUndefined (OrderList), "Parameters" :: NullOrUndefined (ParametersMap), "SkewedInfo" :: NullOrUndefined (SkewedInfo), "StoredAsSubDirectories" :: NullOrUndefined (Boolean) }
```

<p>Describes the physical storage of table data.</p>

##### Instances
``` purescript
Newtype StorageDescriptor _
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array GenericString)
```

##### Instances
``` purescript
Newtype StringList _
```

#### `Table`

``` purescript
newtype Table
  = Table { "Name" :: NameString, "DatabaseName" :: NullOrUndefined (NameString), "Description" :: NullOrUndefined (DescriptionString), "Owner" :: NullOrUndefined (NameString), "CreateTime" :: NullOrUndefined (Number), "UpdateTime" :: NullOrUndefined (Number), "LastAccessTime" :: NullOrUndefined (Number), "LastAnalyzedTime" :: NullOrUndefined (Number), "Retention" :: NullOrUndefined (NonNegativeInteger), "StorageDescriptor" :: NullOrUndefined (StorageDescriptor), "PartitionKeys" :: NullOrUndefined (ColumnList), "ViewOriginalText" :: NullOrUndefined (ViewTextString), "ViewExpandedText" :: NullOrUndefined (ViewTextString), "TableType" :: NullOrUndefined (TableTypeString), "Parameters" :: NullOrUndefined (ParametersMap), "CreatedBy" :: NullOrUndefined (NameString) }
```

<p>Represents a collection of related data organized in columns and rows.</p>

##### Instances
``` purescript
Newtype Table _
```

#### `TableError`

``` purescript
newtype TableError
  = TableError { "TableName" :: NullOrUndefined (NameString), "ErrorDetail" :: NullOrUndefined (ErrorDetail) }
```

<p>An error record for table operations.</p>

##### Instances
``` purescript
Newtype TableError _
```

#### `TableErrors`

``` purescript
newtype TableErrors
  = TableErrors (Array TableError)
```

##### Instances
``` purescript
Newtype TableErrors _
```

#### `TableInput`

``` purescript
newtype TableInput
  = TableInput { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "Owner" :: NullOrUndefined (NameString), "LastAccessTime" :: NullOrUndefined (Number), "LastAnalyzedTime" :: NullOrUndefined (Number), "Retention" :: NullOrUndefined (NonNegativeInteger), "StorageDescriptor" :: NullOrUndefined (StorageDescriptor), "PartitionKeys" :: NullOrUndefined (ColumnList), "ViewOriginalText" :: NullOrUndefined (ViewTextString), "ViewExpandedText" :: NullOrUndefined (ViewTextString), "TableType" :: NullOrUndefined (TableTypeString), "Parameters" :: NullOrUndefined (ParametersMap) }
```

<p>Structure used to create or update the table.</p>

##### Instances
``` purescript
Newtype TableInput _
```

#### `TableList`

``` purescript
newtype TableList
  = TableList (Array Table)
```

##### Instances
``` purescript
Newtype TableList _
```

#### `TableName`

``` purescript
newtype TableName
  = TableName String
```

##### Instances
``` purescript
Newtype TableName _
```

#### `TablePrefix`

``` purescript
newtype TablePrefix
  = TablePrefix String
```

##### Instances
``` purescript
Newtype TablePrefix _
```

#### `TableTypeString`

``` purescript
newtype TableTypeString
  = TableTypeString String
```

##### Instances
``` purescript
Newtype TableTypeString _
```

#### `TableVersion`

``` purescript
newtype TableVersion
  = TableVersion { "Table" :: NullOrUndefined (Table), "VersionId" :: NullOrUndefined (VersionString) }
```

<p>Specifies a version of a table.</p>

##### Instances
``` purescript
Newtype TableVersion _
```

#### `TableVersionError`

``` purescript
newtype TableVersionError
  = TableVersionError { "TableName" :: NullOrUndefined (NameString), "VersionId" :: NullOrUndefined (VersionString), "ErrorDetail" :: NullOrUndefined (ErrorDetail) }
```

<p>An error record for table-version operations.</p>

##### Instances
``` purescript
Newtype TableVersionError _
```

#### `TableVersionErrors`

``` purescript
newtype TableVersionErrors
  = TableVersionErrors (Array TableVersionError)
```

##### Instances
``` purescript
Newtype TableVersionErrors _
```

#### `TimestampValue`

``` purescript
newtype TimestampValue
  = TimestampValue Number
```

##### Instances
``` purescript
Newtype TimestampValue _
```

#### `Token`

``` purescript
newtype Token
  = Token String
```

##### Instances
``` purescript
Newtype Token _
```

#### `TotalSegmentsInteger`

``` purescript
newtype TotalSegmentsInteger
  = TotalSegmentsInteger Int
```

##### Instances
``` purescript
Newtype TotalSegmentsInteger _
```

#### `Trigger`

``` purescript
newtype Trigger
  = Trigger { "Name" :: NullOrUndefined (NameString), "Id" :: NullOrUndefined (IdString), "Type" :: NullOrUndefined (TriggerType), "State" :: NullOrUndefined (TriggerState), "Description" :: NullOrUndefined (DescriptionString), "Schedule" :: NullOrUndefined (GenericString), "Actions" :: NullOrUndefined (ActionList), "Predicate" :: NullOrUndefined (Predicate) }
```

<p>Information about a specific trigger.</p>

##### Instances
``` purescript
Newtype Trigger _
```

#### `TriggerList`

``` purescript
newtype TriggerList
  = TriggerList (Array Trigger)
```

##### Instances
``` purescript
Newtype TriggerList _
```

#### `TriggerState`

``` purescript
newtype TriggerState
  = TriggerState String
```

##### Instances
``` purescript
Newtype TriggerState _
```

#### `TriggerType`

``` purescript
newtype TriggerType
  = TriggerType String
```

##### Instances
``` purescript
Newtype TriggerType _
```

#### `TriggerUpdate`

``` purescript
newtype TriggerUpdate
  = TriggerUpdate { "Name" :: NullOrUndefined (NameString), "Description" :: NullOrUndefined (DescriptionString), "Schedule" :: NullOrUndefined (GenericString), "Actions" :: NullOrUndefined (ActionList), "Predicate" :: NullOrUndefined (Predicate) }
```

<p>A structure used to provide information used to update a trigger. This object will update the the previous trigger definition by overwriting it completely.</p>

##### Instances
``` purescript
Newtype TriggerUpdate _
```

#### `URI`

``` purescript
newtype URI
  = URI String
```

##### Instances
``` purescript
Newtype URI _
```

#### `UpdateBehavior`

``` purescript
newtype UpdateBehavior
  = UpdateBehavior String
```

##### Instances
``` purescript
Newtype UpdateBehavior _
```

#### `UpdateClassifierRequest`

``` purescript
newtype UpdateClassifierRequest
  = UpdateClassifierRequest { "GrokClassifier" :: NullOrUndefined (UpdateGrokClassifierRequest), "XMLClassifier" :: NullOrUndefined (UpdateXMLClassifierRequest), "JsonClassifier" :: NullOrUndefined (UpdateJsonClassifierRequest) }
```

##### Instances
``` purescript
Newtype UpdateClassifierRequest _
```

#### `UpdateClassifierResponse`

``` purescript
newtype UpdateClassifierResponse
  = UpdateClassifierResponse {  }
```

##### Instances
``` purescript
Newtype UpdateClassifierResponse _
```

#### `UpdateConnectionRequest`

``` purescript
newtype UpdateConnectionRequest
  = UpdateConnectionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString, "ConnectionInput" :: ConnectionInput }
```

##### Instances
``` purescript
Newtype UpdateConnectionRequest _
```

#### `UpdateConnectionResponse`

``` purescript
newtype UpdateConnectionResponse
  = UpdateConnectionResponse {  }
```

##### Instances
``` purescript
Newtype UpdateConnectionResponse _
```

#### `UpdateCrawlerRequest`

``` purescript
newtype UpdateCrawlerRequest
  = UpdateCrawlerRequest { "Name" :: NameString, "Role" :: NullOrUndefined (Role), "DatabaseName" :: NullOrUndefined (DatabaseName), "Description" :: NullOrUndefined (DescriptionStringRemovable), "Targets" :: NullOrUndefined (CrawlerTargets), "Schedule" :: NullOrUndefined (CronExpression), "Classifiers" :: NullOrUndefined (ClassifierNameList), "TablePrefix" :: NullOrUndefined (TablePrefix), "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy), "Configuration" :: NullOrUndefined (CrawlerConfiguration) }
```

##### Instances
``` purescript
Newtype UpdateCrawlerRequest _
```

#### `UpdateCrawlerResponse`

``` purescript
newtype UpdateCrawlerResponse
  = UpdateCrawlerResponse {  }
```

##### Instances
``` purescript
Newtype UpdateCrawlerResponse _
```

#### `UpdateCrawlerScheduleRequest`

``` purescript
newtype UpdateCrawlerScheduleRequest
  = UpdateCrawlerScheduleRequest { "CrawlerName" :: NameString, "Schedule" :: NullOrUndefined (CronExpression) }
```

##### Instances
``` purescript
Newtype UpdateCrawlerScheduleRequest _
```

#### `UpdateCrawlerScheduleResponse`

``` purescript
newtype UpdateCrawlerScheduleResponse
  = UpdateCrawlerScheduleResponse {  }
```

##### Instances
``` purescript
Newtype UpdateCrawlerScheduleResponse _
```

#### `UpdateDatabaseRequest`

``` purescript
newtype UpdateDatabaseRequest
  = UpdateDatabaseRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "Name" :: NameString, "DatabaseInput" :: DatabaseInput }
```

##### Instances
``` purescript
Newtype UpdateDatabaseRequest _
```

#### `UpdateDatabaseResponse`

``` purescript
newtype UpdateDatabaseResponse
  = UpdateDatabaseResponse {  }
```

##### Instances
``` purescript
Newtype UpdateDatabaseResponse _
```

#### `UpdateDevEndpointRequest`

``` purescript
newtype UpdateDevEndpointRequest
  = UpdateDevEndpointRequest { "EndpointName" :: GenericString, "PublicKey" :: NullOrUndefined (GenericString), "CustomLibraries" :: NullOrUndefined (DevEndpointCustomLibraries), "UpdateEtlLibraries" :: NullOrUndefined (BooleanValue) }
```

##### Instances
``` purescript
Newtype UpdateDevEndpointRequest _
```

#### `UpdateDevEndpointResponse`

``` purescript
newtype UpdateDevEndpointResponse
  = UpdateDevEndpointResponse {  }
```

##### Instances
``` purescript
Newtype UpdateDevEndpointResponse _
```

#### `UpdateGrokClassifierRequest`

``` purescript
newtype UpdateGrokClassifierRequest
  = UpdateGrokClassifierRequest { "Name" :: NameString, "Classification" :: NullOrUndefined (Classification), "GrokPattern" :: NullOrUndefined (GrokPattern), "CustomPatterns" :: NullOrUndefined (CustomPatterns) }
```

<p>Specifies a grok classifier to update when passed to <code>UpdateClassifier</code>.</p>

##### Instances
``` purescript
Newtype UpdateGrokClassifierRequest _
```

#### `UpdateJobRequest`

``` purescript
newtype UpdateJobRequest
  = UpdateJobRequest { "JobName" :: NameString, "JobUpdate" :: JobUpdate }
```

##### Instances
``` purescript
Newtype UpdateJobRequest _
```

#### `UpdateJobResponse`

``` purescript
newtype UpdateJobResponse
  = UpdateJobResponse { "JobName" :: NullOrUndefined (NameString) }
```

##### Instances
``` purescript
Newtype UpdateJobResponse _
```

#### `UpdateJsonClassifierRequest`

``` purescript
newtype UpdateJsonClassifierRequest
  = UpdateJsonClassifierRequest { "Name" :: NameString, "JsonPath" :: NullOrUndefined (JsonPath) }
```

<p>Specifies a JSON classifier to be updated.</p>

##### Instances
``` purescript
Newtype UpdateJsonClassifierRequest _
```

#### `UpdatePartitionRequest`

``` purescript
newtype UpdatePartitionRequest
  = UpdatePartitionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableName" :: NameString, "PartitionValueList" :: BoundedPartitionValueList, "PartitionInput" :: PartitionInput }
```

##### Instances
``` purescript
Newtype UpdatePartitionRequest _
```

#### `UpdatePartitionResponse`

``` purescript
newtype UpdatePartitionResponse
  = UpdatePartitionResponse {  }
```

##### Instances
``` purescript
Newtype UpdatePartitionResponse _
```

#### `UpdateTableRequest`

``` purescript
newtype UpdateTableRequest
  = UpdateTableRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "TableInput" :: TableInput, "SkipArchive" :: NullOrUndefined (BooleanNullable) }
```

##### Instances
``` purescript
Newtype UpdateTableRequest _
```

#### `UpdateTableResponse`

``` purescript
newtype UpdateTableResponse
  = UpdateTableResponse {  }
```

##### Instances
``` purescript
Newtype UpdateTableResponse _
```

#### `UpdateTriggerRequest`

``` purescript
newtype UpdateTriggerRequest
  = UpdateTriggerRequest { "Name" :: NameString, "TriggerUpdate" :: TriggerUpdate }
```

##### Instances
``` purescript
Newtype UpdateTriggerRequest _
```

#### `UpdateTriggerResponse`

``` purescript
newtype UpdateTriggerResponse
  = UpdateTriggerResponse { "Trigger" :: NullOrUndefined (Trigger) }
```

##### Instances
``` purescript
Newtype UpdateTriggerResponse _
```

#### `UpdateUserDefinedFunctionRequest`

``` purescript
newtype UpdateUserDefinedFunctionRequest
  = UpdateUserDefinedFunctionRequest { "CatalogId" :: NullOrUndefined (CatalogIdString), "DatabaseName" :: NameString, "FunctionName" :: NameString, "FunctionInput" :: UserDefinedFunctionInput }
```

##### Instances
``` purescript
Newtype UpdateUserDefinedFunctionRequest _
```

#### `UpdateUserDefinedFunctionResponse`

``` purescript
newtype UpdateUserDefinedFunctionResponse
  = UpdateUserDefinedFunctionResponse {  }
```

##### Instances
``` purescript
Newtype UpdateUserDefinedFunctionResponse _
```

#### `UpdateXMLClassifierRequest`

``` purescript
newtype UpdateXMLClassifierRequest
  = UpdateXMLClassifierRequest { "Name" :: NameString, "Classification" :: NullOrUndefined (Classification), "RowTag" :: NullOrUndefined (RowTag) }
```

<p>Specifies an XML classifier to be updated.</p>

##### Instances
``` purescript
Newtype UpdateXMLClassifierRequest _
```

#### `UriString`

``` purescript
newtype UriString
  = UriString String
```

##### Instances
``` purescript
Newtype UriString _
```

#### `UserDefinedFunction`

``` purescript
newtype UserDefinedFunction
  = UserDefinedFunction { "FunctionName" :: NullOrUndefined (NameString), "ClassName" :: NullOrUndefined (NameString), "OwnerName" :: NullOrUndefined (NameString), "OwnerType" :: NullOrUndefined (PrincipalType), "CreateTime" :: NullOrUndefined (Number), "ResourceUris" :: NullOrUndefined (ResourceUriList) }
```

<p>Represents the equivalent of a Hive user-defined function (<code>UDF</code>) definition.</p>

##### Instances
``` purescript
Newtype UserDefinedFunction _
```

#### `UserDefinedFunctionInput`

``` purescript
newtype UserDefinedFunctionInput
  = UserDefinedFunctionInput { "FunctionName" :: NullOrUndefined (NameString), "ClassName" :: NullOrUndefined (NameString), "OwnerName" :: NullOrUndefined (NameString), "OwnerType" :: NullOrUndefined (PrincipalType), "ResourceUris" :: NullOrUndefined (ResourceUriList) }
```

<p>A structure used to create or updata a user-defined function.</p>

##### Instances
``` purescript
Newtype UserDefinedFunctionInput _
```

#### `UserDefinedFunctionList`

``` purescript
newtype UserDefinedFunctionList
  = UserDefinedFunctionList (Array UserDefinedFunction)
```

##### Instances
``` purescript
Newtype UserDefinedFunctionList _
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException { "Message" :: NullOrUndefined (MessageString) }
```

<p>A value could not be validated.</p>

##### Instances
``` purescript
Newtype ValidationException _
```

#### `ValueString`

``` purescript
newtype ValueString
  = ValueString String
```

##### Instances
``` purescript
Newtype ValueString _
```

#### `ValueStringList`

``` purescript
newtype ValueStringList
  = ValueStringList (Array ValueString)
```

##### Instances
``` purescript
Newtype ValueStringList _
```

#### `VersionId`

``` purescript
newtype VersionId
  = VersionId Number
```

##### Instances
``` purescript
Newtype VersionId _
```

#### `VersionMismatchException`

``` purescript
newtype VersionMismatchException
  = VersionMismatchException { "Message" :: NullOrUndefined (MessageString) }
```

<p>There was a version conflict.</p>

##### Instances
``` purescript
Newtype VersionMismatchException _
```

#### `VersionString`

``` purescript
newtype VersionString
  = VersionString String
```

##### Instances
``` purescript
Newtype VersionString _
```

#### `ViewTextString`

``` purescript
newtype ViewTextString
  = ViewTextString String
```

##### Instances
``` purescript
Newtype ViewTextString _
```

#### `XMLClassifier`

``` purescript
newtype XMLClassifier
  = XMLClassifier { "Name" :: NameString, "Classification" :: Classification, "CreationTime" :: NullOrUndefined (Number), "LastUpdated" :: NullOrUndefined (Number), "Version" :: NullOrUndefined (VersionId), "RowTag" :: NullOrUndefined (RowTag) }
```

<p>A classifier for <code>XML</code> content.</p>

##### Instances
``` purescript
Newtype XMLClassifier _
```


