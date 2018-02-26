

-- | <fullname>AWS Glue</fullname> <p>Defines the public endpoint for the AWS Glue service.</p>
module AWS.Glue where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Glue" :: String


-- | <p>Creates one or more partitions in a batch operation.</p>
batchCreatePartition :: forall eff. BatchCreatePartitionRequest -> Aff (err :: AWS.RequestError | eff) BatchCreatePartitionResponse
batchCreatePartition = AWS.request serviceName "BatchCreatePartition" 


-- | <p>Deletes a list of connection definitions from the Data Catalog.</p>
batchDeleteConnection :: forall eff. BatchDeleteConnectionRequest -> Aff (err :: AWS.RequestError | eff) BatchDeleteConnectionResponse
batchDeleteConnection = AWS.request serviceName "BatchDeleteConnection" 


-- | <p>Deletes one or more partitions in a batch operation.</p>
batchDeletePartition :: forall eff. BatchDeletePartitionRequest -> Aff (err :: AWS.RequestError | eff) BatchDeletePartitionResponse
batchDeletePartition = AWS.request serviceName "BatchDeletePartition" 


-- | <p>Deletes multiple tables at once.</p>
batchDeleteTable :: forall eff. BatchDeleteTableRequest -> Aff (err :: AWS.RequestError | eff) BatchDeleteTableResponse
batchDeleteTable = AWS.request serviceName "BatchDeleteTable" 


-- | <p>Deletes a specified batch of versions of a table.</p>
batchDeleteTableVersion :: forall eff. BatchDeleteTableVersionRequest -> Aff (err :: AWS.RequestError | eff) BatchDeleteTableVersionResponse
batchDeleteTableVersion = AWS.request serviceName "BatchDeleteTableVersion" 


-- | <p>Retrieves partitions in a batch request.</p>
batchGetPartition :: forall eff. BatchGetPartitionRequest -> Aff (err :: AWS.RequestError | eff) BatchGetPartitionResponse
batchGetPartition = AWS.request serviceName "BatchGetPartition" 


-- | <p>Stops one or more job runs for a specified Job.</p>
batchStopJobRun :: forall eff. BatchStopJobRunRequest -> Aff (err :: AWS.RequestError | eff) BatchStopJobRunResponse
batchStopJobRun = AWS.request serviceName "BatchStopJobRun" 


-- | <p>Creates a classifier in the user's account. This may be a <code>GrokClassifier</code>, an <code>XMLClassifier</code>, or abbrev <code>JsonClassifier</code>, depending on which field of the request is present.</p>
createClassifier :: forall eff. CreateClassifierRequest -> Aff (err :: AWS.RequestError | eff) CreateClassifierResponse
createClassifier = AWS.request serviceName "CreateClassifier" 


-- | <p>Creates a connection definition in the Data Catalog.</p>
createConnection :: forall eff. CreateConnectionRequest -> Aff (err :: AWS.RequestError | eff) CreateConnectionResponse
createConnection = AWS.request serviceName "CreateConnection" 


-- | <p>Creates a new crawler with specified targets, role, configuration, and optional schedule. At least one crawl target must be specified, in either the <i>s3Targets</i> or the <i>jdbcTargets</i> field.</p>
createCrawler :: forall eff. CreateCrawlerRequest -> Aff (err :: AWS.RequestError | eff) CreateCrawlerResponse
createCrawler = AWS.request serviceName "CreateCrawler" 


-- | <p>Creates a new database in a Data Catalog.</p>
createDatabase :: forall eff. CreateDatabaseRequest -> Aff (err :: AWS.RequestError | eff) CreateDatabaseResponse
createDatabase = AWS.request serviceName "CreateDatabase" 


-- | <p>Creates a new DevEndpoint.</p>
createDevEndpoint :: forall eff. CreateDevEndpointRequest -> Aff (err :: AWS.RequestError | eff) CreateDevEndpointResponse
createDevEndpoint = AWS.request serviceName "CreateDevEndpoint" 


-- | <p>Creates a new job.</p>
createJob :: forall eff. CreateJobRequest -> Aff (err :: AWS.RequestError | eff) CreateJobResponse
createJob = AWS.request serviceName "CreateJob" 


-- | <p>Creates a new partition.</p>
createPartition :: forall eff. CreatePartitionRequest -> Aff (err :: AWS.RequestError | eff) CreatePartitionResponse
createPartition = AWS.request serviceName "CreatePartition" 


-- | <p>Transforms a directed acyclic graph (DAG) into code.</p>
createScript :: forall eff. CreateScriptRequest -> Aff (err :: AWS.RequestError | eff) CreateScriptResponse
createScript = AWS.request serviceName "CreateScript" 


-- | <p>Creates a new table definition in the Data Catalog.</p>
createTable :: forall eff. CreateTableRequest -> Aff (err :: AWS.RequestError | eff) CreateTableResponse
createTable = AWS.request serviceName "CreateTable" 


-- | <p>Creates a new trigger.</p>
createTrigger :: forall eff. CreateTriggerRequest -> Aff (err :: AWS.RequestError | eff) CreateTriggerResponse
createTrigger = AWS.request serviceName "CreateTrigger" 


-- | <p>Creates a new function definition in the Data Catalog.</p>
createUserDefinedFunction :: forall eff. CreateUserDefinedFunctionRequest -> Aff (err :: AWS.RequestError | eff) CreateUserDefinedFunctionResponse
createUserDefinedFunction = AWS.request serviceName "CreateUserDefinedFunction" 


-- | <p>Removes a classifier from the Data Catalog.</p>
deleteClassifier :: forall eff. DeleteClassifierRequest -> Aff (err :: AWS.RequestError | eff) DeleteClassifierResponse
deleteClassifier = AWS.request serviceName "DeleteClassifier" 


-- | <p>Deletes a connection from the Data Catalog.</p>
deleteConnection :: forall eff. DeleteConnectionRequest -> Aff (err :: AWS.RequestError | eff) DeleteConnectionResponse
deleteConnection = AWS.request serviceName "DeleteConnection" 


-- | <p>Removes a specified crawler from the Data Catalog, unless the crawler state is <code>RUNNING</code>.</p>
deleteCrawler :: forall eff. DeleteCrawlerRequest -> Aff (err :: AWS.RequestError | eff) DeleteCrawlerResponse
deleteCrawler = AWS.request serviceName "DeleteCrawler" 


-- | <p>Removes a specified Database from a Data Catalog.</p>
deleteDatabase :: forall eff. DeleteDatabaseRequest -> Aff (err :: AWS.RequestError | eff) DeleteDatabaseResponse
deleteDatabase = AWS.request serviceName "DeleteDatabase" 


-- | <p>Deletes a specified DevEndpoint.</p>
deleteDevEndpoint :: forall eff. DeleteDevEndpointRequest -> Aff (err :: AWS.RequestError | eff) DeleteDevEndpointResponse
deleteDevEndpoint = AWS.request serviceName "DeleteDevEndpoint" 


-- | <p>Deletes a specified job. If the job is not found, no exception is thrown.</p>
deleteJob :: forall eff. DeleteJobRequest -> Aff (err :: AWS.RequestError | eff) DeleteJobResponse
deleteJob = AWS.request serviceName "DeleteJob" 


-- | <p>Deletes a specified partition.</p>
deletePartition :: forall eff. DeletePartitionRequest -> Aff (err :: AWS.RequestError | eff) DeletePartitionResponse
deletePartition = AWS.request serviceName "DeletePartition" 


-- | <p>Removes a table definition from the Data Catalog.</p>
deleteTable :: forall eff. DeleteTableRequest -> Aff (err :: AWS.RequestError | eff) DeleteTableResponse
deleteTable = AWS.request serviceName "DeleteTable" 


-- | <p>Deletes a specified version of a table.</p>
deleteTableVersion :: forall eff. DeleteTableVersionRequest -> Aff (err :: AWS.RequestError | eff) DeleteTableVersionResponse
deleteTableVersion = AWS.request serviceName "DeleteTableVersion" 


-- | <p>Deletes a specified trigger. If the trigger is not found, no exception is thrown.</p>
deleteTrigger :: forall eff. DeleteTriggerRequest -> Aff (err :: AWS.RequestError | eff) DeleteTriggerResponse
deleteTrigger = AWS.request serviceName "DeleteTrigger" 


-- | <p>Deletes an existing function definition from the Data Catalog.</p>
deleteUserDefinedFunction :: forall eff. DeleteUserDefinedFunctionRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserDefinedFunctionResponse
deleteUserDefinedFunction = AWS.request serviceName "DeleteUserDefinedFunction" 


-- | <p>Retrieves the status of a migration operation.</p>
getCatalogImportStatus :: forall eff. GetCatalogImportStatusRequest -> Aff (err :: AWS.RequestError | eff) GetCatalogImportStatusResponse
getCatalogImportStatus = AWS.request serviceName "GetCatalogImportStatus" 


-- | <p>Retrieve a classifier by name.</p>
getClassifier :: forall eff. GetClassifierRequest -> Aff (err :: AWS.RequestError | eff) GetClassifierResponse
getClassifier = AWS.request serviceName "GetClassifier" 


-- | <p>Lists all classifier objects in the Data Catalog.</p>
getClassifiers :: forall eff. GetClassifiersRequest -> Aff (err :: AWS.RequestError | eff) GetClassifiersResponse
getClassifiers = AWS.request serviceName "GetClassifiers" 


-- | <p>Retrieves a connection definition from the Data Catalog.</p>
getConnection :: forall eff. GetConnectionRequest -> Aff (err :: AWS.RequestError | eff) GetConnectionResponse
getConnection = AWS.request serviceName "GetConnection" 


-- | <p>Retrieves a list of connection definitions from the Data Catalog.</p>
getConnections :: forall eff. GetConnectionsRequest -> Aff (err :: AWS.RequestError | eff) GetConnectionsResponse
getConnections = AWS.request serviceName "GetConnections" 


-- | <p>Retrieves metadata for a specified crawler.</p>
getCrawler :: forall eff. GetCrawlerRequest -> Aff (err :: AWS.RequestError | eff) GetCrawlerResponse
getCrawler = AWS.request serviceName "GetCrawler" 


-- | <p>Retrieves metrics about specified crawlers.</p>
getCrawlerMetrics :: forall eff. GetCrawlerMetricsRequest -> Aff (err :: AWS.RequestError | eff) GetCrawlerMetricsResponse
getCrawlerMetrics = AWS.request serviceName "GetCrawlerMetrics" 


-- | <p>Retrieves metadata for all crawlers defined in the customer account.</p>
getCrawlers :: forall eff. GetCrawlersRequest -> Aff (err :: AWS.RequestError | eff) GetCrawlersResponse
getCrawlers = AWS.request serviceName "GetCrawlers" 


-- | <p>Retrieves the definition of a specified database.</p>
getDatabase :: forall eff. GetDatabaseRequest -> Aff (err :: AWS.RequestError | eff) GetDatabaseResponse
getDatabase = AWS.request serviceName "GetDatabase" 


-- | <p>Retrieves all Databases defined in a given Data Catalog.</p>
getDatabases :: forall eff. GetDatabasesRequest -> Aff (err :: AWS.RequestError | eff) GetDatabasesResponse
getDatabases = AWS.request serviceName "GetDatabases" 


-- | <p>Transforms a Python script into a directed acyclic graph (DAG). </p>
getDataflowGraph :: forall eff. GetDataflowGraphRequest -> Aff (err :: AWS.RequestError | eff) GetDataflowGraphResponse
getDataflowGraph = AWS.request serviceName "GetDataflowGraph" 


-- | <p>Retrieves information about a specified DevEndpoint.</p>
getDevEndpoint :: forall eff. GetDevEndpointRequest -> Aff (err :: AWS.RequestError | eff) GetDevEndpointResponse
getDevEndpoint = AWS.request serviceName "GetDevEndpoint" 


-- | <p>Retrieves all the DevEndpoints in this AWS account.</p>
getDevEndpoints :: forall eff. GetDevEndpointsRequest -> Aff (err :: AWS.RequestError | eff) GetDevEndpointsResponse
getDevEndpoints = AWS.request serviceName "GetDevEndpoints" 


-- | <p>Retrieves an existing job definition.</p>
getJob :: forall eff. GetJobRequest -> Aff (err :: AWS.RequestError | eff) GetJobResponse
getJob = AWS.request serviceName "GetJob" 


-- | <p>Retrieves the metadata for a given job run.</p>
getJobRun :: forall eff. GetJobRunRequest -> Aff (err :: AWS.RequestError | eff) GetJobRunResponse
getJobRun = AWS.request serviceName "GetJobRun" 


-- | <p>Retrieves metadata for all runs of a given job.</p>
getJobRuns :: forall eff. GetJobRunsRequest -> Aff (err :: AWS.RequestError | eff) GetJobRunsResponse
getJobRuns = AWS.request serviceName "GetJobRuns" 


-- | <p>Retrieves all current jobs.</p>
getJobs :: forall eff. GetJobsRequest -> Aff (err :: AWS.RequestError | eff) GetJobsResponse
getJobs = AWS.request serviceName "GetJobs" 


-- | <p>Creates mappings.</p>
getMapping :: forall eff. GetMappingRequest -> Aff (err :: AWS.RequestError | eff) GetMappingResponse
getMapping = AWS.request serviceName "GetMapping" 


-- | <p>Retrieves information about a specified partition.</p>
getPartition :: forall eff. GetPartitionRequest -> Aff (err :: AWS.RequestError | eff) GetPartitionResponse
getPartition = AWS.request serviceName "GetPartition" 


-- | <p>Retrieves information about the partitions in a table.</p>
getPartitions :: forall eff. GetPartitionsRequest -> Aff (err :: AWS.RequestError | eff) GetPartitionsResponse
getPartitions = AWS.request serviceName "GetPartitions" 


-- | <p>Gets code to perform a specified mapping.</p>
getPlan :: forall eff. GetPlanRequest -> Aff (err :: AWS.RequestError | eff) GetPlanResponse
getPlan = AWS.request serviceName "GetPlan" 


-- | <p>Retrieves the <code>Table</code> definition in a Data Catalog for a specified table.</p>
getTable :: forall eff. GetTableRequest -> Aff (err :: AWS.RequestError | eff) GetTableResponse
getTable = AWS.request serviceName "GetTable" 


-- | <p>Retrieves a specified version of a table.</p>
getTableVersion :: forall eff. GetTableVersionRequest -> Aff (err :: AWS.RequestError | eff) GetTableVersionResponse
getTableVersion = AWS.request serviceName "GetTableVersion" 


-- | <p>Retrieves a list of strings that identify available versions of a specified table.</p>
getTableVersions :: forall eff. GetTableVersionsRequest -> Aff (err :: AWS.RequestError | eff) GetTableVersionsResponse
getTableVersions = AWS.request serviceName "GetTableVersions" 


-- | <p>Retrieves the definitions of some or all of the tables in a given <code>Database</code>.</p>
getTables :: forall eff. GetTablesRequest -> Aff (err :: AWS.RequestError | eff) GetTablesResponse
getTables = AWS.request serviceName "GetTables" 


-- | <p>Retrieves the definition of a trigger.</p>
getTrigger :: forall eff. GetTriggerRequest -> Aff (err :: AWS.RequestError | eff) GetTriggerResponse
getTrigger = AWS.request serviceName "GetTrigger" 


-- | <p>Gets all the triggers associated with a job.</p>
getTriggers :: forall eff. GetTriggersRequest -> Aff (err :: AWS.RequestError | eff) GetTriggersResponse
getTriggers = AWS.request serviceName "GetTriggers" 


-- | <p>Retrieves a specified function definition from the Data Catalog.</p>
getUserDefinedFunction :: forall eff. GetUserDefinedFunctionRequest -> Aff (err :: AWS.RequestError | eff) GetUserDefinedFunctionResponse
getUserDefinedFunction = AWS.request serviceName "GetUserDefinedFunction" 


-- | <p>Retrieves a multiple function definitions from the Data Catalog.</p>
getUserDefinedFunctions :: forall eff. GetUserDefinedFunctionsRequest -> Aff (err :: AWS.RequestError | eff) GetUserDefinedFunctionsResponse
getUserDefinedFunctions = AWS.request serviceName "GetUserDefinedFunctions" 


-- | <p>Imports an existing Athena Data Catalog to AWS Glue</p>
importCatalogToGlue :: forall eff. ImportCatalogToGlueRequest -> Aff (err :: AWS.RequestError | eff) ImportCatalogToGlueResponse
importCatalogToGlue = AWS.request serviceName "ImportCatalogToGlue" 


-- | <p>Resets a bookmark entry.</p>
resetJobBookmark :: forall eff. ResetJobBookmarkRequest -> Aff (err :: AWS.RequestError | eff) ResetJobBookmarkResponse
resetJobBookmark = AWS.request serviceName "ResetJobBookmark" 


-- | <p>Starts a crawl using the specified crawler, regardless of what is scheduled. If the crawler is already running, does nothing.</p>
startCrawler :: forall eff. StartCrawlerRequest -> Aff (err :: AWS.RequestError | eff) StartCrawlerResponse
startCrawler = AWS.request serviceName "StartCrawler" 


-- | <p>Changes the schedule state of the specified crawler to <code>SCHEDULED</code>, unless the crawler is already running or the schedule state is already <code>SCHEDULED</code>.</p>
startCrawlerSchedule :: forall eff. StartCrawlerScheduleRequest -> Aff (err :: AWS.RequestError | eff) StartCrawlerScheduleResponse
startCrawlerSchedule = AWS.request serviceName "StartCrawlerSchedule" 


-- | <p>Runs a job.</p>
startJobRun :: forall eff. StartJobRunRequest -> Aff (err :: AWS.RequestError | eff) StartJobRunResponse
startJobRun = AWS.request serviceName "StartJobRun" 


-- | <p>Starts an existing trigger. See <a href="http://docs.aws.amazon.com/glue/latest/dg/trigger-job.html">Triggering Jobs</a> for information about how different types of trigger are started.</p>
startTrigger :: forall eff. StartTriggerRequest -> Aff (err :: AWS.RequestError | eff) StartTriggerResponse
startTrigger = AWS.request serviceName "StartTrigger" 


-- | <p>If the specified crawler is running, stops the crawl.</p>
stopCrawler :: forall eff. StopCrawlerRequest -> Aff (err :: AWS.RequestError | eff) StopCrawlerResponse
stopCrawler = AWS.request serviceName "StopCrawler" 


-- | <p>Sets the schedule state of the specified crawler to <code>NOT_SCHEDULED</code>, but does not stop the crawler if it is already running.</p>
stopCrawlerSchedule :: forall eff. StopCrawlerScheduleRequest -> Aff (err :: AWS.RequestError | eff) StopCrawlerScheduleResponse
stopCrawlerSchedule = AWS.request serviceName "StopCrawlerSchedule" 


-- | <p>Stops a specified trigger.</p>
stopTrigger :: forall eff. StopTriggerRequest -> Aff (err :: AWS.RequestError | eff) StopTriggerResponse
stopTrigger = AWS.request serviceName "StopTrigger" 


-- | <p>Modifies an existing classifier (a <code>GrokClassifier</code>, <code>XMLClassifier</code>, or <code>JsonClassifier</code>, depending on which field is present).</p>
updateClassifier :: forall eff. UpdateClassifierRequest -> Aff (err :: AWS.RequestError | eff) UpdateClassifierResponse
updateClassifier = AWS.request serviceName "UpdateClassifier" 


-- | <p>Updates a connection definition in the Data Catalog.</p>
updateConnection :: forall eff. UpdateConnectionRequest -> Aff (err :: AWS.RequestError | eff) UpdateConnectionResponse
updateConnection = AWS.request serviceName "UpdateConnection" 


-- | <p>Updates a crawler. If a crawler is running, you must stop it using <code>StopCrawler</code> before updating it.</p>
updateCrawler :: forall eff. UpdateCrawlerRequest -> Aff (err :: AWS.RequestError | eff) UpdateCrawlerResponse
updateCrawler = AWS.request serviceName "UpdateCrawler" 


-- | <p>Updates the schedule of a crawler using a <code>cron</code> expression. </p>
updateCrawlerSchedule :: forall eff. UpdateCrawlerScheduleRequest -> Aff (err :: AWS.RequestError | eff) UpdateCrawlerScheduleResponse
updateCrawlerSchedule = AWS.request serviceName "UpdateCrawlerSchedule" 


-- | <p>Updates an existing database definition in a Data Catalog.</p>
updateDatabase :: forall eff. UpdateDatabaseRequest -> Aff (err :: AWS.RequestError | eff) UpdateDatabaseResponse
updateDatabase = AWS.request serviceName "UpdateDatabase" 


-- | <p>Updates a specified DevEndpoint.</p>
updateDevEndpoint :: forall eff. UpdateDevEndpointRequest -> Aff (err :: AWS.RequestError | eff) UpdateDevEndpointResponse
updateDevEndpoint = AWS.request serviceName "UpdateDevEndpoint" 


-- | <p>Updates an existing job definition.</p>
updateJob :: forall eff. UpdateJobRequest -> Aff (err :: AWS.RequestError | eff) UpdateJobResponse
updateJob = AWS.request serviceName "UpdateJob" 


-- | <p>Updates a partition.</p>
updatePartition :: forall eff. UpdatePartitionRequest -> Aff (err :: AWS.RequestError | eff) UpdatePartitionResponse
updatePartition = AWS.request serviceName "UpdatePartition" 


-- | <p>Updates a metadata table in the Data Catalog.</p>
updateTable :: forall eff. UpdateTableRequest -> Aff (err :: AWS.RequestError | eff) UpdateTableResponse
updateTable = AWS.request serviceName "UpdateTable" 


-- | <p>Updates a trigger definition.</p>
updateTrigger :: forall eff. UpdateTriggerRequest -> Aff (err :: AWS.RequestError | eff) UpdateTriggerResponse
updateTrigger = AWS.request serviceName "UpdateTrigger" 


-- | <p>Updates an existing function definition in the Data Catalog.</p>
updateUserDefinedFunction :: forall eff. UpdateUserDefinedFunctionRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserDefinedFunctionResponse
updateUserDefinedFunction = AWS.request serviceName "UpdateUserDefinedFunction" 


-- | <p>Access to a resource was denied.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>Defines an action to be initiated by a trigger.</p>
newtype Action = Action 
  { "JobName" :: NullOrUndefined (NameString)
  , "Arguments" :: NullOrUndefined (GenericMap)
  }


newtype ActionList = ActionList (Array Action)


-- | <p>A resource to be created or added already exists.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message" :: NullOrUndefined (MessageString)
  }


newtype AttemptCount = AttemptCount Int


newtype BatchCreatePartitionRequest = BatchCreatePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionInputList" :: (PartitionInputList)
  }


newtype BatchCreatePartitionResponse = BatchCreatePartitionResponse 
  { "Errors" :: NullOrUndefined (PartitionErrors)
  }


newtype BatchDeleteConnectionRequest = BatchDeleteConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "ConnectionNameList" :: (DeleteConnectionNameList)
  }


newtype BatchDeleteConnectionResponse = BatchDeleteConnectionResponse 
  { "Succeeded" :: NullOrUndefined (NameStringList)
  , "Errors" :: NullOrUndefined (ErrorByName)
  }


newtype BatchDeletePartitionRequest = BatchDeletePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionsToDelete" :: (BatchDeletePartitionValueList)
  }


newtype BatchDeletePartitionResponse = BatchDeletePartitionResponse 
  { "Errors" :: NullOrUndefined (PartitionErrors)
  }


newtype BatchDeletePartitionValueList = BatchDeletePartitionValueList (Array PartitionValueList)


newtype BatchDeleteTableNameList = BatchDeleteTableNameList (Array NameString)


newtype BatchDeleteTableRequest = BatchDeleteTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TablesToDelete" :: (BatchDeleteTableNameList)
  }


newtype BatchDeleteTableResponse = BatchDeleteTableResponse 
  { "Errors" :: NullOrUndefined (TableErrors)
  }


newtype BatchDeleteTableVersionList = BatchDeleteTableVersionList (Array VersionString)


newtype BatchDeleteTableVersionRequest = BatchDeleteTableVersionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionIds" :: (BatchDeleteTableVersionList)
  }


newtype BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse 
  { "Errors" :: NullOrUndefined (TableVersionErrors)
  }


newtype BatchGetPartitionRequest = BatchGetPartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionsToGet" :: (BatchGetPartitionValueList)
  }


newtype BatchGetPartitionResponse = BatchGetPartitionResponse 
  { "Partitions" :: NullOrUndefined (PartitionList)
  , "UnprocessedKeys" :: NullOrUndefined (BatchGetPartitionValueList)
  }


newtype BatchGetPartitionValueList = BatchGetPartitionValueList (Array PartitionValueList)


-- | <p>Records an error that occurred when attempting to stop a specified JobRun.</p>
newtype BatchStopJobRunError = BatchStopJobRunError 
  { "JobName" :: NullOrUndefined (NameString)
  , "JobRunId" :: NullOrUndefined (IdString)
  , "ErrorDetail" :: NullOrUndefined (ErrorDetail)
  }


newtype BatchStopJobRunErrorList = BatchStopJobRunErrorList (Array BatchStopJobRunError)


newtype BatchStopJobRunJobRunIdList = BatchStopJobRunJobRunIdList (Array IdString)


newtype BatchStopJobRunRequest = BatchStopJobRunRequest 
  { "JobName" :: (NameString)
  , "JobRunIds" :: (BatchStopJobRunJobRunIdList)
  }


newtype BatchStopJobRunResponse = BatchStopJobRunResponse 
  { "SuccessfulSubmissions" :: NullOrUndefined (BatchStopJobRunSuccessfulSubmissionList)
  , "Errors" :: NullOrUndefined (BatchStopJobRunErrorList)
  }


-- | <p>Records a successful request to stop a specified JobRun.</p>
newtype BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission 
  { "JobName" :: NullOrUndefined (NameString)
  , "JobRunId" :: NullOrUndefined (IdString)
  }


newtype BatchStopJobRunSuccessfulSubmissionList = BatchStopJobRunSuccessfulSubmissionList (Array BatchStopJobRunSuccessfulSubmission)


newtype BooleanNullable = BooleanNullable Boolean


newtype BooleanValue = BooleanValue Boolean


newtype BoundedPartitionValueList = BoundedPartitionValueList (Array ValueString)


newtype CatalogEntries = CatalogEntries (Array CatalogEntry)


-- | <p>Specifies a table definition in the Data Catalog.</p>
newtype CatalogEntry = CatalogEntry 
  { "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  }


newtype CatalogIdString = CatalogIdString String


-- | <p>A structure containing migration status information.</p>
newtype CatalogImportStatus = CatalogImportStatus 
  { "ImportCompleted" :: NullOrUndefined (Boolean)
  , "ImportTime" :: NullOrUndefined (Number)
  , "ImportedBy" :: NullOrUndefined (NameString)
  }


newtype Classification = Classification String


-- | <p>Classifiers are written in Python and triggered during a crawl task. You can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier checks whether a given file is in a format it can handle, and if it is, the classifier creates a schema in the form of a <code>StructType</code> object that matches that data format.</p> <p>A classifier can be a <code>grok</code> classifier, an XML classifier, or a JSON classifier, asspecified in one of the fields in the <code>Classifier</code> object.</p>
newtype Classifier = Classifier 
  { "GrokClassifier" :: NullOrUndefined (GrokClassifier)
  , "XMLClassifier" :: NullOrUndefined (XMLClassifier)
  , "JsonClassifier" :: NullOrUndefined (JsonClassifier)
  }


newtype ClassifierList = ClassifierList (Array Classifier)


newtype ClassifierNameList = ClassifierNameList (Array NameString)


newtype CodeGenArgName = CodeGenArgName String


newtype CodeGenArgValue = CodeGenArgValue String


-- | <p>Represents a directional edge in a directed acyclic graph (DAG).</p>
newtype CodeGenEdge = CodeGenEdge 
  { "Source" :: (CodeGenIdentifier)
  , "Target" :: (CodeGenIdentifier)
  , "TargetParameter" :: NullOrUndefined (CodeGenArgName)
  }


newtype CodeGenIdentifier = CodeGenIdentifier String


-- | <p>Represents a node in a directed acyclic graph (DAG)</p>
newtype CodeGenNode = CodeGenNode 
  { "Id" :: (CodeGenIdentifier)
  , "NodeType" :: (CodeGenNodeType)
  , "Args" :: (CodeGenNodeArgs)
  , "LineNumber" :: NullOrUndefined (Int)
  }


-- | <p>An argument or property of a node.</p>
newtype CodeGenNodeArg = CodeGenNodeArg 
  { "Name" :: (CodeGenArgName)
  , "Value" :: (CodeGenArgValue)
  , "Param" :: NullOrUndefined (Boolean)
  }


newtype CodeGenNodeArgs = CodeGenNodeArgs (Array CodeGenNodeArg)


newtype CodeGenNodeType = CodeGenNodeType String


-- | <p>A column in a <code>Table</code>.</p>
newtype Column = Column 
  { "Name" :: (NameString)
  , "Type" :: NullOrUndefined (ColumnTypeString)
  , "Comment" :: NullOrUndefined (CommentString)
  }


newtype ColumnList = ColumnList (Array Column)


newtype ColumnTypeString = ColumnTypeString String


newtype ColumnValueStringList = ColumnValueStringList (Array ColumnValuesString)


newtype ColumnValuesString = ColumnValuesString String


newtype CommentString = CommentString String


-- | <p>Two processes are trying to modify a resource simultaneously.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>Too many jobs are being run concurrently.</p>
newtype ConcurrentRunsExceededException = ConcurrentRunsExceededException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>Defines a condition under which a trigger fires.</p>
newtype Condition = Condition 
  { "LogicalOperator" :: NullOrUndefined (LogicalOperator)
  , "JobName" :: NullOrUndefined (NameString)
  , "State" :: NullOrUndefined (JobRunState)
  }


newtype ConditionList = ConditionList (Array Condition)


-- | <p>Defines a connection to a data source.</p>
newtype Connection = Connection 
  { "Name" :: NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "ConnectionType" :: NullOrUndefined (ConnectionType)
  , "MatchCriteria" :: NullOrUndefined (MatchCriteria)
  , "ConnectionProperties" :: NullOrUndefined (ConnectionProperties)
  , "PhysicalConnectionRequirements" :: NullOrUndefined (PhysicalConnectionRequirements)
  , "CreationTime" :: NullOrUndefined (Number)
  , "LastUpdatedTime" :: NullOrUndefined (Number)
  , "LastUpdatedBy" :: NullOrUndefined (NameString)
  }


-- | <p>A structure used to specify a connection to create or update.</p>
newtype ConnectionInput = ConnectionInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "ConnectionType" :: (ConnectionType)
  , "MatchCriteria" :: NullOrUndefined (MatchCriteria)
  , "ConnectionProperties" :: (ConnectionProperties)
  , "PhysicalConnectionRequirements" :: NullOrUndefined (PhysicalConnectionRequirements)
  }


newtype ConnectionList = ConnectionList (Array Connection)


newtype ConnectionName = ConnectionName String


newtype ConnectionProperties = ConnectionProperties (Map ConnectionPropertyKey ValueString)


newtype ConnectionPropertyKey = ConnectionPropertyKey String


newtype ConnectionType = ConnectionType String


-- | <p>Specifies the connections used by a job.</p>
newtype ConnectionsList = ConnectionsList 
  { "Connections" :: NullOrUndefined (StringList)
  }


-- | <p>Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.</p>
newtype Crawler = Crawler 
  { "Name" :: NullOrUndefined (NameString)
  , "Role" :: NullOrUndefined (Role)
  , "Targets" :: NullOrUndefined (CrawlerTargets)
  , "DatabaseName" :: NullOrUndefined (DatabaseName)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Classifiers" :: NullOrUndefined (ClassifierNameList)
  , "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy)
  , "State" :: NullOrUndefined (CrawlerState)
  , "TablePrefix" :: NullOrUndefined (TablePrefix)
  , "Schedule" :: NullOrUndefined (Schedule)
  , "CrawlElapsedTime" :: NullOrUndefined (MillisecondsCount)
  , "CreationTime" :: NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "LastCrawl" :: NullOrUndefined (LastCrawlInfo)
  , "Version" :: NullOrUndefined (VersionId)
  , "Configuration" :: NullOrUndefined (CrawlerConfiguration)
  }


newtype CrawlerConfiguration = CrawlerConfiguration String


newtype CrawlerList = CrawlerList (Array Crawler)


-- | <p>Metrics for a specified crawler.</p>
newtype CrawlerMetrics = CrawlerMetrics 
  { "CrawlerName" :: NullOrUndefined (NameString)
  , "TimeLeftSeconds" :: NullOrUndefined (NonNegativeDouble)
  , "StillEstimating" :: NullOrUndefined (Boolean)
  , "LastRuntimeSeconds" :: NullOrUndefined (NonNegativeDouble)
  , "MedianRuntimeSeconds" :: NullOrUndefined (NonNegativeDouble)
  , "TablesCreated" :: NullOrUndefined (NonNegativeInteger)
  , "TablesUpdated" :: NullOrUndefined (NonNegativeInteger)
  , "TablesDeleted" :: NullOrUndefined (NonNegativeInteger)
  }


newtype CrawlerMetricsList = CrawlerMetricsList (Array CrawlerMetrics)


newtype CrawlerNameList = CrawlerNameList (Array NameString)


-- | <p>The specified crawler is not running.</p>
newtype CrawlerNotRunningException = CrawlerNotRunningException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>The operation cannot be performed because the crawler is already running.</p>
newtype CrawlerRunningException = CrawlerRunningException 
  { "Message" :: NullOrUndefined (MessageString)
  }


newtype CrawlerState = CrawlerState String


-- | <p>The specified crawler is stopping.</p>
newtype CrawlerStoppingException = CrawlerStoppingException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>Specifies data stores to crawl.</p>
newtype CrawlerTargets = CrawlerTargets 
  { "S3Targets" :: NullOrUndefined (S3TargetList)
  , "JdbcTargets" :: NullOrUndefined (JdbcTargetList)
  }


newtype CreateClassifierRequest = CreateClassifierRequest 
  { "GrokClassifier" :: NullOrUndefined (CreateGrokClassifierRequest)
  , "XMLClassifier" :: NullOrUndefined (CreateXMLClassifierRequest)
  , "JsonClassifier" :: NullOrUndefined (CreateJsonClassifierRequest)
  }


newtype CreateClassifierResponse = CreateClassifierResponse 
  { 
  }


newtype CreateConnectionRequest = CreateConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "ConnectionInput" :: (ConnectionInput)
  }


newtype CreateConnectionResponse = CreateConnectionResponse 
  { 
  }


newtype CreateCrawlerRequest = CreateCrawlerRequest 
  { "Name" :: (NameString)
  , "Role" :: (Role)
  , "DatabaseName" :: (DatabaseName)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Targets" :: (CrawlerTargets)
  , "Schedule" :: NullOrUndefined (CronExpression)
  , "Classifiers" :: NullOrUndefined (ClassifierNameList)
  , "TablePrefix" :: NullOrUndefined (TablePrefix)
  , "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy)
  , "Configuration" :: NullOrUndefined (CrawlerConfiguration)
  }


newtype CreateCrawlerResponse = CreateCrawlerResponse 
  { 
  }


newtype CreateDatabaseRequest = CreateDatabaseRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseInput" :: (DatabaseInput)
  }


newtype CreateDatabaseResponse = CreateDatabaseResponse 
  { 
  }


newtype CreateDevEndpointRequest = CreateDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  , "RoleArn" :: (RoleArn)
  , "SecurityGroupIds" :: NullOrUndefined (StringList)
  , "SubnetId" :: NullOrUndefined (GenericString)
  , "PublicKey" :: (GenericString)
  , "NumberOfNodes" :: NullOrUndefined (IntegerValue)
  , "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined (GenericString)
  }


newtype CreateDevEndpointResponse = CreateDevEndpointResponse 
  { "EndpointName" :: NullOrUndefined (GenericString)
  , "Status" :: NullOrUndefined (GenericString)
  , "SecurityGroupIds" :: NullOrUndefined (StringList)
  , "SubnetId" :: NullOrUndefined (GenericString)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  , "YarnEndpointAddress" :: NullOrUndefined (GenericString)
  , "ZeppelinRemoteSparkInterpreterPort" :: NullOrUndefined (IntegerValue)
  , "NumberOfNodes" :: NullOrUndefined (IntegerValue)
  , "AvailabilityZone" :: NullOrUndefined (GenericString)
  , "VpcId" :: NullOrUndefined (GenericString)
  , "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined (GenericString)
  , "FailureReason" :: NullOrUndefined (GenericString)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampValue)
  }


-- | <p>Specifies a <code>grok</code> classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateGrokClassifierRequest = CreateGrokClassifierRequest 
  { "Classification" :: (Classification)
  , "Name" :: (NameString)
  , "GrokPattern" :: (GrokPattern)
  , "CustomPatterns" :: NullOrUndefined (CustomPatterns)
  }


newtype CreateJobRequest = CreateJobRequest 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "LogUri" :: NullOrUndefined (UriString)
  , "Role" :: (RoleString)
  , "ExecutionProperty" :: NullOrUndefined (ExecutionProperty)
  , "Command" :: (JobCommand)
  , "DefaultArguments" :: NullOrUndefined (GenericMap)
  , "Connections" :: NullOrUndefined (ConnectionsList)
  , "MaxRetries" :: NullOrUndefined (MaxRetries)
  , "AllocatedCapacity" :: NullOrUndefined (IntegerValue)
  }


newtype CreateJobResponse = CreateJobResponse 
  { "Name" :: NullOrUndefined (NameString)
  }


-- | <p>Specifies a JSON classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateJsonClassifierRequest = CreateJsonClassifierRequest 
  { "Name" :: (NameString)
  , "JsonPath" :: (JsonPath)
  }


newtype CreatePartitionRequest = CreatePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionInput" :: (PartitionInput)
  }


newtype CreatePartitionResponse = CreatePartitionResponse 
  { 
  }


newtype CreateScriptRequest = CreateScriptRequest 
  { "DagNodes" :: NullOrUndefined (DagNodes)
  , "DagEdges" :: NullOrUndefined (DagEdges)
  , "Language" :: NullOrUndefined (Language)
  }


newtype CreateScriptResponse = CreateScriptResponse 
  { "PythonScript" :: NullOrUndefined (PythonScript)
  , "ScalaCode" :: NullOrUndefined (ScalaCode)
  }


newtype CreateTableRequest = CreateTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableInput" :: (TableInput)
  }


newtype CreateTableResponse = CreateTableResponse 
  { 
  }


newtype CreateTriggerRequest = CreateTriggerRequest 
  { "Name" :: (NameString)
  , "Type" :: (TriggerType)
  , "Schedule" :: NullOrUndefined (GenericString)
  , "Predicate" :: NullOrUndefined (Predicate)
  , "Actions" :: (ActionList)
  , "Description" :: NullOrUndefined (DescriptionString)
  }


newtype CreateTriggerResponse = CreateTriggerResponse 
  { "Name" :: NullOrUndefined (NameString)
  }


newtype CreateUserDefinedFunctionRequest = CreateUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionInput" :: (UserDefinedFunctionInput)
  }


newtype CreateUserDefinedFunctionResponse = CreateUserDefinedFunctionResponse 
  { 
  }


-- | <p>Specifies an XML classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateXMLClassifierRequest = CreateXMLClassifierRequest 
  { "Classification" :: (Classification)
  , "Name" :: (NameString)
  , "RowTag" :: NullOrUndefined (RowTag)
  }


newtype CronExpression = CronExpression String


newtype CustomPatterns = CustomPatterns String


newtype DagEdges = DagEdges (Array CodeGenEdge)


newtype DagNodes = DagNodes (Array CodeGenNode)


-- | <p>The <code>Database</code> object represents a logical grouping of tables that may reside in a Hive metastore or an RDBMS.</p>
newtype Database = Database 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "LocationUri" :: NullOrUndefined (URI)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  , "CreateTime" :: NullOrUndefined (Number)
  }


-- | <p>The structure used to create or update a database.</p>
newtype DatabaseInput = DatabaseInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "LocationUri" :: NullOrUndefined (URI)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  }


newtype DatabaseList = DatabaseList (Array Database)


newtype DatabaseName = DatabaseName String


newtype DeleteBehavior = DeleteBehavior String


newtype DeleteClassifierRequest = DeleteClassifierRequest 
  { "Name" :: (NameString)
  }


newtype DeleteClassifierResponse = DeleteClassifierResponse 
  { 
  }


newtype DeleteConnectionNameList = DeleteConnectionNameList (Array NameString)


newtype DeleteConnectionRequest = DeleteConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "ConnectionName" :: (NameString)
  }


newtype DeleteConnectionResponse = DeleteConnectionResponse 
  { 
  }


newtype DeleteCrawlerRequest = DeleteCrawlerRequest 
  { "Name" :: (NameString)
  }


newtype DeleteCrawlerResponse = DeleteCrawlerResponse 
  { 
  }


newtype DeleteDatabaseRequest = DeleteDatabaseRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }


newtype DeleteDatabaseResponse = DeleteDatabaseResponse 
  { 
  }


newtype DeleteDevEndpointRequest = DeleteDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  }


newtype DeleteDevEndpointResponse = DeleteDevEndpointResponse 
  { 
  }


newtype DeleteJobRequest = DeleteJobRequest 
  { "JobName" :: (NameString)
  }


newtype DeleteJobResponse = DeleteJobResponse 
  { "JobName" :: NullOrUndefined (NameString)
  }


newtype DeletePartitionRequest = DeletePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValues" :: (ValueStringList)
  }


newtype DeletePartitionResponse = DeletePartitionResponse 
  { 
  }


newtype DeleteTableRequest = DeleteTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Name" :: (NameString)
  }


newtype DeleteTableResponse = DeleteTableResponse 
  { 
  }


newtype DeleteTableVersionRequest = DeleteTableVersionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionId" :: (VersionString)
  }


newtype DeleteTableVersionResponse = DeleteTableVersionResponse 
  { 
  }


newtype DeleteTriggerRequest = DeleteTriggerRequest 
  { "Name" :: (NameString)
  }


newtype DeleteTriggerResponse = DeleteTriggerResponse 
  { "Name" :: NullOrUndefined (NameString)
  }


newtype DeleteUserDefinedFunctionRequest = DeleteUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  }


newtype DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse 
  { 
  }


newtype DescriptionString = DescriptionString String


newtype DescriptionStringRemovable = DescriptionStringRemovable String


-- | <p>A development endpoint where a developer can remotely debug ETL scripts.</p>
newtype DevEndpoint = DevEndpoint 
  { "EndpointName" :: NullOrUndefined (GenericString)
  , "RoleArn" :: NullOrUndefined (RoleArn)
  , "SecurityGroupIds" :: NullOrUndefined (StringList)
  , "SubnetId" :: NullOrUndefined (GenericString)
  , "YarnEndpointAddress" :: NullOrUndefined (GenericString)
  , "ZeppelinRemoteSparkInterpreterPort" :: NullOrUndefined (IntegerValue)
  , "PublicAddress" :: NullOrUndefined (GenericString)
  , "Status" :: NullOrUndefined (GenericString)
  , "NumberOfNodes" :: NullOrUndefined (IntegerValue)
  , "AvailabilityZone" :: NullOrUndefined (GenericString)
  , "VpcId" :: NullOrUndefined (GenericString)
  , "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined (GenericString)
  , "FailureReason" :: NullOrUndefined (GenericString)
  , "LastUpdateStatus" :: NullOrUndefined (GenericString)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampValue)
  , "LastModifiedTimestamp" :: NullOrUndefined (TimestampValue)
  , "PublicKey" :: NullOrUndefined (GenericString)
  }


-- | <p>Custom libraries to be loaded into a DevEndpoint.</p>
newtype DevEndpointCustomLibraries = DevEndpointCustomLibraries 
  { "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined (GenericString)
  }


newtype DevEndpointList = DevEndpointList (Array DevEndpoint)


-- | <p>A specified entity does not exist</p>
newtype EntityNotFoundException = EntityNotFoundException 
  { "Message" :: NullOrUndefined (MessageString)
  }


newtype ErrorByName = ErrorByName (Map NameString ErrorDetail)


-- | <p>Contains details about an error.</p>
newtype ErrorDetail = ErrorDetail 
  { "ErrorCode" :: NullOrUndefined (NameString)
  , "ErrorMessage" :: NullOrUndefined (DescriptionString)
  }


newtype ErrorString = ErrorString String


-- | <p>An execution property of a job.</p>
newtype ExecutionProperty = ExecutionProperty 
  { "MaxConcurrentRuns" :: NullOrUndefined (MaxConcurrentRuns)
  }


newtype FieldType = FieldType String


newtype FilterString = FilterString String


newtype FormatString = FormatString String


newtype GenericMap = GenericMap (Map GenericString GenericString)


newtype GenericString = GenericString String


newtype GetCatalogImportStatusRequest = GetCatalogImportStatusRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  }


newtype GetCatalogImportStatusResponse = GetCatalogImportStatusResponse 
  { "ImportStatus" :: NullOrUndefined (CatalogImportStatus)
  }


newtype GetClassifierRequest = GetClassifierRequest 
  { "Name" :: (NameString)
  }


newtype GetClassifierResponse = GetClassifierResponse 
  { "Classifier" :: NullOrUndefined (Classifier)
  }


newtype GetClassifiersRequest = GetClassifiersRequest 
  { "MaxResults" :: NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetClassifiersResponse = GetClassifiersResponse 
  { "Classifiers" :: NullOrUndefined (ClassifierList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetConnectionRequest = GetConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }


newtype GetConnectionResponse = GetConnectionResponse 
  { "Connection" :: NullOrUndefined (Connection)
  }


-- | <p>Filters the connection definitions returned by the <code>GetConnections</code> API.</p>
newtype GetConnectionsFilter = GetConnectionsFilter 
  { "MatchCriteria" :: NullOrUndefined (MatchCriteria)
  , "ConnectionType" :: NullOrUndefined (ConnectionType)
  }


newtype GetConnectionsRequest = GetConnectionsRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Filter" :: NullOrUndefined (GetConnectionsFilter)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetConnectionsResponse = GetConnectionsResponse 
  { "ConnectionList" :: NullOrUndefined (ConnectionList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetCrawlerMetricsRequest = GetCrawlerMetricsRequest 
  { "CrawlerNameList" :: NullOrUndefined (CrawlerNameList)
  , "MaxResults" :: NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetCrawlerMetricsResponse = GetCrawlerMetricsResponse 
  { "CrawlerMetricsList" :: NullOrUndefined (CrawlerMetricsList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetCrawlerRequest = GetCrawlerRequest 
  { "Name" :: (NameString)
  }


newtype GetCrawlerResponse = GetCrawlerResponse 
  { "Crawler" :: NullOrUndefined (Crawler)
  }


newtype GetCrawlersRequest = GetCrawlersRequest 
  { "MaxResults" :: NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetCrawlersResponse = GetCrawlersResponse 
  { "Crawlers" :: NullOrUndefined (CrawlerList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetDatabaseRequest = GetDatabaseRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }


newtype GetDatabaseResponse = GetDatabaseResponse 
  { "Database" :: NullOrUndefined (Database)
  }


newtype GetDatabasesRequest = GetDatabasesRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetDatabasesResponse = GetDatabasesResponse 
  { "DatabaseList" :: (DatabaseList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetDataflowGraphRequest = GetDataflowGraphRequest 
  { "PythonScript" :: NullOrUndefined (PythonScript)
  }


newtype GetDataflowGraphResponse = GetDataflowGraphResponse 
  { "DagNodes" :: NullOrUndefined (DagNodes)
  , "DagEdges" :: NullOrUndefined (DagEdges)
  }


newtype GetDevEndpointRequest = GetDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  }


newtype GetDevEndpointResponse = GetDevEndpointResponse 
  { "DevEndpoint" :: NullOrUndefined (DevEndpoint)
  }


newtype GetDevEndpointsRequest = GetDevEndpointsRequest 
  { "MaxResults" :: NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


newtype GetDevEndpointsResponse = GetDevEndpointsResponse 
  { "DevEndpoints" :: NullOrUndefined (DevEndpointList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


newtype GetJobRequest = GetJobRequest 
  { "JobName" :: (NameString)
  }


newtype GetJobResponse = GetJobResponse 
  { "Job" :: NullOrUndefined (Job)
  }


newtype GetJobRunRequest = GetJobRunRequest 
  { "JobName" :: (NameString)
  , "RunId" :: (IdString)
  , "PredecessorsIncluded" :: NullOrUndefined (BooleanValue)
  }


newtype GetJobRunResponse = GetJobRunResponse 
  { "JobRun" :: NullOrUndefined (JobRun)
  }


newtype GetJobRunsRequest = GetJobRunsRequest 
  { "JobName" :: (NameString)
  , "NextToken" :: NullOrUndefined (GenericString)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetJobRunsResponse = GetJobRunsResponse 
  { "JobRuns" :: NullOrUndefined (JobRunList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


newtype GetJobsRequest = GetJobsRequest 
  { "NextToken" :: NullOrUndefined (GenericString)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetJobsResponse = GetJobsResponse 
  { "Jobs" :: NullOrUndefined (JobList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


newtype GetMappingRequest = GetMappingRequest 
  { "Source" :: (CatalogEntry)
  , "Sinks" :: NullOrUndefined (CatalogEntries)
  , "Location" :: NullOrUndefined (Location)
  }


newtype GetMappingResponse = GetMappingResponse 
  { "Mapping" :: (MappingList)
  }


newtype GetPartitionRequest = GetPartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValues" :: (ValueStringList)
  }


newtype GetPartitionResponse = GetPartitionResponse 
  { "Partition" :: NullOrUndefined (Partition)
  }


newtype GetPartitionsRequest = GetPartitionsRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "Expression" :: NullOrUndefined (PredicateString)
  , "NextToken" :: NullOrUndefined (Token)
  , "Segment" :: NullOrUndefined (Segment)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetPartitionsResponse = GetPartitionsResponse 
  { "Partitions" :: NullOrUndefined (PartitionList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetPlanRequest = GetPlanRequest 
  { "Mapping" :: (MappingList)
  , "Source" :: (CatalogEntry)
  , "Sinks" :: NullOrUndefined (CatalogEntries)
  , "Location" :: NullOrUndefined (Location)
  , "Language" :: NullOrUndefined (Language)
  }


newtype GetPlanResponse = GetPlanResponse 
  { "PythonScript" :: NullOrUndefined (PythonScript)
  , "ScalaCode" :: NullOrUndefined (ScalaCode)
  }


newtype GetTableRequest = GetTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Name" :: (NameString)
  }


newtype GetTableResponse = GetTableResponse 
  { "Table" :: NullOrUndefined (Table)
  }


newtype GetTableVersionRequest = GetTableVersionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionId" :: NullOrUndefined (VersionString)
  }


newtype GetTableVersionResponse = GetTableVersionResponse 
  { "TableVersion" :: NullOrUndefined (TableVersion)
  }


newtype GetTableVersionsList = GetTableVersionsList (Array TableVersion)


newtype GetTableVersionsRequest = GetTableVersionsRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetTableVersionsResponse = GetTableVersionsResponse 
  { "TableVersions" :: NullOrUndefined (GetTableVersionsList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetTablesRequest = GetTablesRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Expression" :: NullOrUndefined (FilterString)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetTablesResponse = GetTablesResponse 
  { "TableList" :: NullOrUndefined (TableList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype GetTriggerRequest = GetTriggerRequest 
  { "Name" :: (NameString)
  }


newtype GetTriggerResponse = GetTriggerResponse 
  { "Trigger" :: NullOrUndefined (Trigger)
  }


newtype GetTriggersRequest = GetTriggersRequest 
  { "NextToken" :: NullOrUndefined (GenericString)
  , "DependentJobName" :: NullOrUndefined (NameString)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetTriggersResponse = GetTriggersResponse 
  { "Triggers" :: NullOrUndefined (TriggerList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }


newtype GetUserDefinedFunctionRequest = GetUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  }


newtype GetUserDefinedFunctionResponse = GetUserDefinedFunctionResponse 
  { "UserDefinedFunction" :: NullOrUndefined (UserDefinedFunction)
  }


newtype GetUserDefinedFunctionsRequest = GetUserDefinedFunctionsRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Pattern" :: (NameString)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }


newtype GetUserDefinedFunctionsResponse = GetUserDefinedFunctionsResponse 
  { "UserDefinedFunctions" :: NullOrUndefined (UserDefinedFunctionList)
  , "NextToken" :: NullOrUndefined (Token)
  }


-- | <p>A classifier that uses <code>grok</code> patterns.</p>
newtype GrokClassifier = GrokClassifier 
  { "Name" :: (NameString)
  , "Classification" :: (Classification)
  , "CreationTime" :: NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "Version" :: NullOrUndefined (VersionId)
  , "GrokPattern" :: (GrokPattern)
  , "CustomPatterns" :: NullOrUndefined (CustomPatterns)
  }


newtype GrokPattern = GrokPattern String


newtype IdString = IdString String


-- | <p>The same unique identifier was associated with two different records.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException 
  { "Message" :: NullOrUndefined (MessageString)
  }


newtype ImportCatalogToGlueRequest = ImportCatalogToGlueRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  }


newtype ImportCatalogToGlueResponse = ImportCatalogToGlueResponse 
  { 
  }


newtype IntegerFlag = IntegerFlag Int


newtype IntegerValue = IntegerValue Int


-- | <p>An internal service error occurred.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>The input provided was not valid.</p>
newtype InvalidInputException = InvalidInputException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>Specifies a JDBC data store to crawl.</p>
newtype JdbcTarget = JdbcTarget 
  { "ConnectionName" :: NullOrUndefined (ConnectionName)
  , "Path" :: NullOrUndefined (Path)
  , "Exclusions" :: NullOrUndefined (PathList)
  }


newtype JdbcTargetList = JdbcTargetList (Array JdbcTarget)


-- | <p>Specifies a job.</p>
newtype Job = Job 
  { "Name" :: NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "LogUri" :: NullOrUndefined (UriString)
  , "Role" :: NullOrUndefined (RoleString)
  , "CreatedOn" :: NullOrUndefined (TimestampValue)
  , "LastModifiedOn" :: NullOrUndefined (TimestampValue)
  , "ExecutionProperty" :: NullOrUndefined (ExecutionProperty)
  , "Command" :: NullOrUndefined (JobCommand)
  , "DefaultArguments" :: NullOrUndefined (GenericMap)
  , "Connections" :: NullOrUndefined (ConnectionsList)
  , "MaxRetries" :: NullOrUndefined (MaxRetries)
  , "AllocatedCapacity" :: NullOrUndefined (IntegerValue)
  }


-- | <p>Defines a point which a job can resume processing.</p>
newtype JobBookmarkEntry = JobBookmarkEntry 
  { "JobName" :: NullOrUndefined (JobName)
  , "Version" :: NullOrUndefined (IntegerValue)
  , "Run" :: NullOrUndefined (IntegerValue)
  , "Attempt" :: NullOrUndefined (IntegerValue)
  , "JobBookmark" :: NullOrUndefined (JsonValue)
  }


-- | <p>Specifies code that executes a job.</p>
newtype JobCommand = JobCommand 
  { "Name" :: NullOrUndefined (GenericString)
  , "ScriptLocation" :: NullOrUndefined (ScriptLocationString)
  }


newtype JobList = JobList (Array Job)


newtype JobName = JobName String


-- | <p>Contains information about a job run.</p>
newtype JobRun = JobRun 
  { "Id" :: NullOrUndefined (IdString)
  , "Attempt" :: NullOrUndefined (AttemptCount)
  , "PreviousRunId" :: NullOrUndefined (IdString)
  , "TriggerName" :: NullOrUndefined (NameString)
  , "JobName" :: NullOrUndefined (NameString)
  , "StartedOn" :: NullOrUndefined (TimestampValue)
  , "LastModifiedOn" :: NullOrUndefined (TimestampValue)
  , "CompletedOn" :: NullOrUndefined (TimestampValue)
  , "JobRunState" :: NullOrUndefined (JobRunState)
  , "Arguments" :: NullOrUndefined (GenericMap)
  , "ErrorMessage" :: NullOrUndefined (ErrorString)
  , "PredecessorRuns" :: NullOrUndefined (PredecessorList)
  , "AllocatedCapacity" :: NullOrUndefined (IntegerValue)
  }


newtype JobRunList = JobRunList (Array JobRun)


newtype JobRunState = JobRunState String


-- | <p>Specifies information used to update an existing job. Note that the previous job definition will be completely overwritten by this information.</p>
newtype JobUpdate = JobUpdate 
  { "Description" :: NullOrUndefined (DescriptionString)
  , "LogUri" :: NullOrUndefined (UriString)
  , "Role" :: NullOrUndefined (RoleString)
  , "ExecutionProperty" :: NullOrUndefined (ExecutionProperty)
  , "Command" :: NullOrUndefined (JobCommand)
  , "DefaultArguments" :: NullOrUndefined (GenericMap)
  , "Connections" :: NullOrUndefined (ConnectionsList)
  , "MaxRetries" :: NullOrUndefined (MaxRetries)
  , "AllocatedCapacity" :: NullOrUndefined (IntegerValue)
  }


-- | <p>A classifier for <code>JSON</code> content.</p>
newtype JsonClassifier = JsonClassifier 
  { "Name" :: (NameString)
  , "CreationTime" :: NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "Version" :: NullOrUndefined (VersionId)
  , "JsonPath" :: (JsonPath)
  }


newtype JsonPath = JsonPath String


newtype JsonValue = JsonValue String


newtype KeyString = KeyString String


newtype Language = Language String


-- | <p>Status and error information about the most recent crawl.</p>
newtype LastCrawlInfo = LastCrawlInfo 
  { "Status" :: NullOrUndefined (LastCrawlStatus)
  , "ErrorMessage" :: NullOrUndefined (DescriptionString)
  , "LogGroup" :: NullOrUndefined (LogGroup)
  , "LogStream" :: NullOrUndefined (LogStream)
  , "MessagePrefix" :: NullOrUndefined (MessagePrefix)
  , "StartTime" :: NullOrUndefined (Number)
  }


newtype LastCrawlStatus = LastCrawlStatus String


-- | <p>The location of resources.</p>
newtype Location = Location 
  { "Jdbc" :: NullOrUndefined (CodeGenNodeArgs)
  , "S3" :: NullOrUndefined (CodeGenNodeArgs)
  }


newtype LocationMap = LocationMap (Map ColumnValuesString ColumnValuesString)


newtype LocationString = LocationString String


newtype LogGroup = LogGroup String


newtype LogStream = LogStream String


newtype Logical = Logical String


newtype LogicalOperator = LogicalOperator String


-- | <p>Defines a mapping.</p>
newtype MappingEntry = MappingEntry 
  { "SourceTable" :: NullOrUndefined (TableName)
  , "SourcePath" :: NullOrUndefined (SchemaPathString)
  , "SourceType" :: NullOrUndefined (FieldType)
  , "TargetTable" :: NullOrUndefined (TableName)
  , "TargetPath" :: NullOrUndefined (SchemaPathString)
  , "TargetType" :: NullOrUndefined (FieldType)
  }


newtype MappingList = MappingList (Array MappingEntry)


newtype MatchCriteria = MatchCriteria (Array NameString)


newtype MaxConcurrentRuns = MaxConcurrentRuns Int


newtype MaxRetries = MaxRetries Int


newtype MessagePrefix = MessagePrefix String


newtype MessageString = MessageString String


newtype MillisecondsCount = MillisecondsCount Number


newtype NameString = NameString String


newtype NameStringList = NameStringList (Array NameString)


-- | <p>There is no applicable schedule.</p>
newtype NoScheduleException = NoScheduleException 
  { "Message" :: NullOrUndefined (MessageString)
  }


newtype NonNegativeDouble = NonNegativeDouble Number


newtype NonNegativeInteger = NonNegativeInteger Int


-- | <p>The operation timed out.</p>
newtype OperationTimeoutException = OperationTimeoutException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>Specifies the sort order of a sorted column.</p>
newtype Order = Order 
  { "Column" :: (NameString)
  , "SortOrder" :: (IntegerFlag)
  }


newtype OrderList = OrderList (Array Order)


newtype PageSize = PageSize Int


newtype ParametersMap = ParametersMap (Map KeyString ParametersMapValue)


newtype ParametersMapValue = ParametersMapValue String


-- | <p>Represents a slice of table data.</p>
newtype Partition = Partition 
  { "Values" :: NullOrUndefined (ValueStringList)
  , "DatabaseName" :: NullOrUndefined (NameString)
  , "TableName" :: NullOrUndefined (NameString)
  , "CreationTime" :: NullOrUndefined (Number)
  , "LastAccessTime" :: NullOrUndefined (Number)
  , "StorageDescriptor" :: NullOrUndefined (StorageDescriptor)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  , "LastAnalyzedTime" :: NullOrUndefined (Number)
  }


-- | <p>Contains information about a partition error.</p>
newtype PartitionError = PartitionError 
  { "PartitionValues" :: NullOrUndefined (ValueStringList)
  , "ErrorDetail" :: NullOrUndefined (ErrorDetail)
  }


newtype PartitionErrors = PartitionErrors (Array PartitionError)


-- | <p>The structure used to create and update a partion.</p>
newtype PartitionInput = PartitionInput 
  { "Values" :: NullOrUndefined (ValueStringList)
  , "LastAccessTime" :: NullOrUndefined (Number)
  , "StorageDescriptor" :: NullOrUndefined (StorageDescriptor)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  , "LastAnalyzedTime" :: NullOrUndefined (Number)
  }


newtype PartitionInputList = PartitionInputList (Array PartitionInput)


newtype PartitionList = PartitionList (Array Partition)


-- | <p>Contains a list of values defining partitions.</p>
newtype PartitionValueList = PartitionValueList 
  { "Values" :: (ValueStringList)
  }


newtype Path = Path String


newtype PathList = PathList (Array Path)


-- | <p>Specifies the physical requirements for a connection.</p>
newtype PhysicalConnectionRequirements = PhysicalConnectionRequirements 
  { "SubnetId" :: NullOrUndefined (NameString)
  , "SecurityGroupIdList" :: NullOrUndefined (SecurityGroupIdList)
  , "AvailabilityZone" :: NullOrUndefined (NameString)
  }


-- | <p>A job run that was used in the predicate of a conditional trigger that triggered this job run.</p>
newtype Predecessor = Predecessor 
  { "JobName" :: NullOrUndefined (NameString)
  , "RunId" :: NullOrUndefined (IdString)
  }


newtype PredecessorList = PredecessorList (Array Predecessor)


-- | <p>Defines the predicate of the trigger, which determines when it fires.</p>
newtype Predicate = Predicate 
  { "Logical" :: NullOrUndefined (Logical)
  , "Conditions" :: NullOrUndefined (ConditionList)
  }


newtype PredicateString = PredicateString String


newtype PrincipalType = PrincipalType String


newtype PythonScript = PythonScript String


newtype ResetJobBookmarkRequest = ResetJobBookmarkRequest 
  { "JobName" :: (JobName)
  }


newtype ResetJobBookmarkResponse = ResetJobBookmarkResponse 
  { "JobBookmarkEntry" :: NullOrUndefined (JobBookmarkEntry)
  }


-- | <p>A resource numerical limit was exceeded.</p>
newtype ResourceNumberLimitExceededException = ResourceNumberLimitExceededException 
  { "Message" :: NullOrUndefined (MessageString)
  }


newtype ResourceType = ResourceType String


-- | <p>URIs for function resources.</p>
newtype ResourceUri = ResourceUri 
  { "ResourceType" :: NullOrUndefined (ResourceType)
  , "Uri" :: NullOrUndefined (URI)
  }


newtype ResourceUriList = ResourceUriList (Array ResourceUri)


newtype Role = Role String


newtype RoleArn = RoleArn String


newtype RoleString = RoleString String


newtype RowTag = RowTag String


-- | <p>Specifies a data store in Amazon S3.</p>
newtype S3Target = S3Target 
  { "Path" :: NullOrUndefined (Path)
  , "Exclusions" :: NullOrUndefined (PathList)
  }


newtype S3TargetList = S3TargetList (Array S3Target)


newtype ScalaCode = ScalaCode String


-- | <p>A scheduling object using a <code>cron</code> statement to schedule an event.</p>
newtype Schedule = Schedule 
  { "ScheduleExpression" :: NullOrUndefined (CronExpression)
  , "State" :: NullOrUndefined (ScheduleState)
  }


newtype ScheduleState = ScheduleState String


-- | <p>The specified scheduler is not running.</p>
newtype SchedulerNotRunningException = SchedulerNotRunningException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>The specified scheduler is already running.</p>
newtype SchedulerRunningException = SchedulerRunningException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>The specified scheduler is transitioning.</p>
newtype SchedulerTransitioningException = SchedulerTransitioningException 
  { "Message" :: NullOrUndefined (MessageString)
  }


-- | <p>Crawler policy for update and deletion behavior.</p>
newtype SchemaChangePolicy = SchemaChangePolicy 
  { "UpdateBehavior" :: NullOrUndefined (UpdateBehavior)
  , "DeleteBehavior" :: NullOrUndefined (DeleteBehavior)
  }


newtype SchemaPathString = SchemaPathString String


newtype ScriptLocationString = ScriptLocationString String


newtype SecurityGroupIdList = SecurityGroupIdList (Array NameString)


-- | <p>Defines a non-overlapping region of a table's partitions, allowing multiple requests to be executed in parallel.</p>
newtype Segment = Segment 
  { "SegmentNumber" :: (NonNegativeInteger)
  , "TotalSegments" :: (TotalSegmentsInteger)
  }


-- | <p>Information about a serialization/deserialization program (SerDe) which serves as an extractor and loader.</p>
newtype SerDeInfo = SerDeInfo 
  { "Name" :: NullOrUndefined (NameString)
  , "SerializationLibrary" :: NullOrUndefined (NameString)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  }


-- | <p>Specifies skewed values in a table. Skewed are ones that occur with very high frequency.</p>
newtype SkewedInfo = SkewedInfo 
  { "SkewedColumnNames" :: NullOrUndefined (NameStringList)
  , "SkewedColumnValues" :: NullOrUndefined (ColumnValueStringList)
  , "SkewedColumnValueLocationMaps" :: NullOrUndefined (LocationMap)
  }


newtype StartCrawlerRequest = StartCrawlerRequest 
  { "Name" :: (NameString)
  }


newtype StartCrawlerResponse = StartCrawlerResponse 
  { 
  }


newtype StartCrawlerScheduleRequest = StartCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  }


newtype StartCrawlerScheduleResponse = StartCrawlerScheduleResponse 
  { 
  }


newtype StartJobRunRequest = StartJobRunRequest 
  { "JobName" :: (NameString)
  , "JobRunId" :: NullOrUndefined (IdString)
  , "Arguments" :: NullOrUndefined (GenericMap)
  , "AllocatedCapacity" :: NullOrUndefined (IntegerValue)
  }


newtype StartJobRunResponse = StartJobRunResponse 
  { "JobRunId" :: NullOrUndefined (IdString)
  }


newtype StartTriggerRequest = StartTriggerRequest 
  { "Name" :: (NameString)
  }


newtype StartTriggerResponse = StartTriggerResponse 
  { "Name" :: NullOrUndefined (NameString)
  }


newtype StopCrawlerRequest = StopCrawlerRequest 
  { "Name" :: (NameString)
  }


newtype StopCrawlerResponse = StopCrawlerResponse 
  { 
  }


newtype StopCrawlerScheduleRequest = StopCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  }


newtype StopCrawlerScheduleResponse = StopCrawlerScheduleResponse 
  { 
  }


newtype StopTriggerRequest = StopTriggerRequest 
  { "Name" :: (NameString)
  }


newtype StopTriggerResponse = StopTriggerResponse 
  { "Name" :: NullOrUndefined (NameString)
  }


-- | <p>Describes the physical storage of table data.</p>
newtype StorageDescriptor = StorageDescriptor 
  { "Columns" :: NullOrUndefined (ColumnList)
  , "Location" :: NullOrUndefined (LocationString)
  , "InputFormat" :: NullOrUndefined (FormatString)
  , "OutputFormat" :: NullOrUndefined (FormatString)
  , "Compressed" :: NullOrUndefined (Boolean)
  , "NumberOfBuckets" :: NullOrUndefined (Int)
  , "SerdeInfo" :: NullOrUndefined (SerDeInfo)
  , "BucketColumns" :: NullOrUndefined (NameStringList)
  , "SortColumns" :: NullOrUndefined (OrderList)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  , "SkewedInfo" :: NullOrUndefined (SkewedInfo)
  , "StoredAsSubDirectories" :: NullOrUndefined (Boolean)
  }


newtype StringList = StringList (Array GenericString)


-- | <p>Represents a collection of related data organized in columns and rows.</p>
newtype Table = Table 
  { "Name" :: (NameString)
  , "DatabaseName" :: NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Owner" :: NullOrUndefined (NameString)
  , "CreateTime" :: NullOrUndefined (Number)
  , "UpdateTime" :: NullOrUndefined (Number)
  , "LastAccessTime" :: NullOrUndefined (Number)
  , "LastAnalyzedTime" :: NullOrUndefined (Number)
  , "Retention" :: NullOrUndefined (NonNegativeInteger)
  , "StorageDescriptor" :: NullOrUndefined (StorageDescriptor)
  , "PartitionKeys" :: NullOrUndefined (ColumnList)
  , "ViewOriginalText" :: NullOrUndefined (ViewTextString)
  , "ViewExpandedText" :: NullOrUndefined (ViewTextString)
  , "TableType" :: NullOrUndefined (TableTypeString)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  , "CreatedBy" :: NullOrUndefined (NameString)
  }


-- | <p>An error record for table operations.</p>
newtype TableError = TableError 
  { "TableName" :: NullOrUndefined (NameString)
  , "ErrorDetail" :: NullOrUndefined (ErrorDetail)
  }


newtype TableErrors = TableErrors (Array TableError)


-- | <p>Structure used to create or update the table.</p>
newtype TableInput = TableInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Owner" :: NullOrUndefined (NameString)
  , "LastAccessTime" :: NullOrUndefined (Number)
  , "LastAnalyzedTime" :: NullOrUndefined (Number)
  , "Retention" :: NullOrUndefined (NonNegativeInteger)
  , "StorageDescriptor" :: NullOrUndefined (StorageDescriptor)
  , "PartitionKeys" :: NullOrUndefined (ColumnList)
  , "ViewOriginalText" :: NullOrUndefined (ViewTextString)
  , "ViewExpandedText" :: NullOrUndefined (ViewTextString)
  , "TableType" :: NullOrUndefined (TableTypeString)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  }


newtype TableList = TableList (Array Table)


newtype TableName = TableName String


newtype TablePrefix = TablePrefix String


newtype TableTypeString = TableTypeString String


-- | <p>Specifies a version of a table.</p>
newtype TableVersion = TableVersion 
  { "Table" :: NullOrUndefined (Table)
  , "VersionId" :: NullOrUndefined (VersionString)
  }


-- | <p>An error record for table-version operations.</p>
newtype TableVersionError = TableVersionError 
  { "TableName" :: NullOrUndefined (NameString)
  , "VersionId" :: NullOrUndefined (VersionString)
  , "ErrorDetail" :: NullOrUndefined (ErrorDetail)
  }


newtype TableVersionErrors = TableVersionErrors (Array TableVersionError)


newtype TimestampValue = TimestampValue Number


newtype Token = Token String


newtype TotalSegmentsInteger = TotalSegmentsInteger Int


-- | <p>Information about a specific trigger.</p>
newtype Trigger = Trigger 
  { "Name" :: NullOrUndefined (NameString)
  , "Id" :: NullOrUndefined (IdString)
  , "Type" :: NullOrUndefined (TriggerType)
  , "State" :: NullOrUndefined (TriggerState)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Schedule" :: NullOrUndefined (GenericString)
  , "Actions" :: NullOrUndefined (ActionList)
  , "Predicate" :: NullOrUndefined (Predicate)
  }


newtype TriggerList = TriggerList (Array Trigger)


newtype TriggerState = TriggerState String


newtype TriggerType = TriggerType String


-- | <p>A structure used to provide information used to update a trigger. This object will update the the previous trigger definition by overwriting it completely.</p>
newtype TriggerUpdate = TriggerUpdate 
  { "Name" :: NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Schedule" :: NullOrUndefined (GenericString)
  , "Actions" :: NullOrUndefined (ActionList)
  , "Predicate" :: NullOrUndefined (Predicate)
  }


newtype URI = URI String


newtype UpdateBehavior = UpdateBehavior String


newtype UpdateClassifierRequest = UpdateClassifierRequest 
  { "GrokClassifier" :: NullOrUndefined (UpdateGrokClassifierRequest)
  , "XMLClassifier" :: NullOrUndefined (UpdateXMLClassifierRequest)
  , "JsonClassifier" :: NullOrUndefined (UpdateJsonClassifierRequest)
  }


newtype UpdateClassifierResponse = UpdateClassifierResponse 
  { 
  }


newtype UpdateConnectionRequest = UpdateConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  , "ConnectionInput" :: (ConnectionInput)
  }


newtype UpdateConnectionResponse = UpdateConnectionResponse 
  { 
  }


newtype UpdateCrawlerRequest = UpdateCrawlerRequest 
  { "Name" :: (NameString)
  , "Role" :: NullOrUndefined (Role)
  , "DatabaseName" :: NullOrUndefined (DatabaseName)
  , "Description" :: NullOrUndefined (DescriptionStringRemovable)
  , "Targets" :: NullOrUndefined (CrawlerTargets)
  , "Schedule" :: NullOrUndefined (CronExpression)
  , "Classifiers" :: NullOrUndefined (ClassifierNameList)
  , "TablePrefix" :: NullOrUndefined (TablePrefix)
  , "SchemaChangePolicy" :: NullOrUndefined (SchemaChangePolicy)
  , "Configuration" :: NullOrUndefined (CrawlerConfiguration)
  }


newtype UpdateCrawlerResponse = UpdateCrawlerResponse 
  { 
  }


newtype UpdateCrawlerScheduleRequest = UpdateCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  , "Schedule" :: NullOrUndefined (CronExpression)
  }


newtype UpdateCrawlerScheduleResponse = UpdateCrawlerScheduleResponse 
  { 
  }


newtype UpdateDatabaseRequest = UpdateDatabaseRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  , "DatabaseInput" :: (DatabaseInput)
  }


newtype UpdateDatabaseResponse = UpdateDatabaseResponse 
  { 
  }


newtype UpdateDevEndpointRequest = UpdateDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  , "PublicKey" :: NullOrUndefined (GenericString)
  , "CustomLibraries" :: NullOrUndefined (DevEndpointCustomLibraries)
  , "UpdateEtlLibraries" :: NullOrUndefined (BooleanValue)
  }


newtype UpdateDevEndpointResponse = UpdateDevEndpointResponse 
  { 
  }


-- | <p>Specifies a grok classifier to update when passed to <code>UpdateClassifier</code>.</p>
newtype UpdateGrokClassifierRequest = UpdateGrokClassifierRequest 
  { "Name" :: (NameString)
  , "Classification" :: NullOrUndefined (Classification)
  , "GrokPattern" :: NullOrUndefined (GrokPattern)
  , "CustomPatterns" :: NullOrUndefined (CustomPatterns)
  }


newtype UpdateJobRequest = UpdateJobRequest 
  { "JobName" :: (NameString)
  , "JobUpdate" :: (JobUpdate)
  }


newtype UpdateJobResponse = UpdateJobResponse 
  { "JobName" :: NullOrUndefined (NameString)
  }


-- | <p>Specifies a JSON classifier to be updated.</p>
newtype UpdateJsonClassifierRequest = UpdateJsonClassifierRequest 
  { "Name" :: (NameString)
  , "JsonPath" :: NullOrUndefined (JsonPath)
  }


newtype UpdatePartitionRequest = UpdatePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValueList" :: (BoundedPartitionValueList)
  , "PartitionInput" :: (PartitionInput)
  }


newtype UpdatePartitionResponse = UpdatePartitionResponse 
  { 
  }


newtype UpdateTableRequest = UpdateTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableInput" :: (TableInput)
  , "SkipArchive" :: NullOrUndefined (BooleanNullable)
  }


newtype UpdateTableResponse = UpdateTableResponse 
  { 
  }


newtype UpdateTriggerRequest = UpdateTriggerRequest 
  { "Name" :: (NameString)
  , "TriggerUpdate" :: (TriggerUpdate)
  }


newtype UpdateTriggerResponse = UpdateTriggerResponse 
  { "Trigger" :: NullOrUndefined (Trigger)
  }


newtype UpdateUserDefinedFunctionRequest = UpdateUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  , "FunctionInput" :: (UserDefinedFunctionInput)
  }


newtype UpdateUserDefinedFunctionResponse = UpdateUserDefinedFunctionResponse 
  { 
  }


-- | <p>Specifies an XML classifier to be updated.</p>
newtype UpdateXMLClassifierRequest = UpdateXMLClassifierRequest 
  { "Name" :: (NameString)
  , "Classification" :: NullOrUndefined (Classification)
  , "RowTag" :: NullOrUndefined (RowTag)
  }


newtype UriString = UriString String


-- | <p>Represents the equivalent of a Hive user-defined function (<code>UDF</code>) definition.</p>
newtype UserDefinedFunction = UserDefinedFunction 
  { "FunctionName" :: NullOrUndefined (NameString)
  , "ClassName" :: NullOrUndefined (NameString)
  , "OwnerName" :: NullOrUndefined (NameString)
  , "OwnerType" :: NullOrUndefined (PrincipalType)
  , "CreateTime" :: NullOrUndefined (Number)
  , "ResourceUris" :: NullOrUndefined (ResourceUriList)
  }


-- | <p>A structure used to create or updata a user-defined function.</p>
newtype UserDefinedFunctionInput = UserDefinedFunctionInput 
  { "FunctionName" :: NullOrUndefined (NameString)
  , "ClassName" :: NullOrUndefined (NameString)
  , "OwnerName" :: NullOrUndefined (NameString)
  , "OwnerType" :: NullOrUndefined (PrincipalType)
  , "ResourceUris" :: NullOrUndefined (ResourceUriList)
  }


newtype UserDefinedFunctionList = UserDefinedFunctionList (Array UserDefinedFunction)


-- | <p>A value could not be validated.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (MessageString)
  }


newtype ValueString = ValueString String


newtype ValueStringList = ValueStringList (Array ValueString)


newtype VersionId = VersionId Number


-- | <p>There was a version conflict.</p>
newtype VersionMismatchException = VersionMismatchException 
  { "Message" :: NullOrUndefined (MessageString)
  }


newtype VersionString = VersionString String


newtype ViewTextString = ViewTextString String


-- | <p>A classifier for <code>XML</code> content.</p>
newtype XMLClassifier = XMLClassifier 
  { "Name" :: (NameString)
  , "Classification" :: (Classification)
  , "CreationTime" :: NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "Version" :: NullOrUndefined (VersionId)
  , "RowTag" :: NullOrUndefined (RowTag)
  }
