

-- | <fullname>AWS Glue</fullname> <p>Defines the public endpoint for the AWS Glue service.</p>
module AWS.Glue where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Glue" :: String


-- | <p>Creates one or more partitions in a batch operation.</p>
batchCreatePartition :: forall eff. BatchCreatePartitionRequest -> Aff (err :: AWS.RequestError | eff) BatchCreatePartitionResponse
batchCreatePartition = AWS.request serviceName "batchCreatePartition" 


-- | <p>Deletes a list of connection definitions from the Data Catalog.</p>
batchDeleteConnection :: forall eff. BatchDeleteConnectionRequest -> Aff (err :: AWS.RequestError | eff) BatchDeleteConnectionResponse
batchDeleteConnection = AWS.request serviceName "batchDeleteConnection" 


-- | <p>Deletes one or more partitions in a batch operation.</p>
batchDeletePartition :: forall eff. BatchDeletePartitionRequest -> Aff (err :: AWS.RequestError | eff) BatchDeletePartitionResponse
batchDeletePartition = AWS.request serviceName "batchDeletePartition" 


-- | <p>Deletes multiple tables at once.</p>
batchDeleteTable :: forall eff. BatchDeleteTableRequest -> Aff (err :: AWS.RequestError | eff) BatchDeleteTableResponse
batchDeleteTable = AWS.request serviceName "batchDeleteTable" 


-- | <p>Deletes a specified batch of versions of a table.</p>
batchDeleteTableVersion :: forall eff. BatchDeleteTableVersionRequest -> Aff (err :: AWS.RequestError | eff) BatchDeleteTableVersionResponse
batchDeleteTableVersion = AWS.request serviceName "batchDeleteTableVersion" 


-- | <p>Retrieves partitions in a batch request.</p>
batchGetPartition :: forall eff. BatchGetPartitionRequest -> Aff (err :: AWS.RequestError | eff) BatchGetPartitionResponse
batchGetPartition = AWS.request serviceName "batchGetPartition" 


-- | <p>Stops one or more job runs for a specified Job.</p>
batchStopJobRun :: forall eff. BatchStopJobRunRequest -> Aff (err :: AWS.RequestError | eff) BatchStopJobRunResponse
batchStopJobRun = AWS.request serviceName "batchStopJobRun" 


-- | <p>Creates a classifier in the user's account. This may be a <code>GrokClassifier</code>, an <code>XMLClassifier</code>, or abbrev <code>JsonClassifier</code>, depending on which field of the request is present.</p>
createClassifier :: forall eff. CreateClassifierRequest -> Aff (err :: AWS.RequestError | eff) CreateClassifierResponse
createClassifier = AWS.request serviceName "createClassifier" 


-- | <p>Creates a connection definition in the Data Catalog.</p>
createConnection :: forall eff. CreateConnectionRequest -> Aff (err :: AWS.RequestError | eff) CreateConnectionResponse
createConnection = AWS.request serviceName "createConnection" 


-- | <p>Creates a new crawler with specified targets, role, configuration, and optional schedule. At least one crawl target must be specified, in either the <i>s3Targets</i> or the <i>jdbcTargets</i> field.</p>
createCrawler :: forall eff. CreateCrawlerRequest -> Aff (err :: AWS.RequestError | eff) CreateCrawlerResponse
createCrawler = AWS.request serviceName "createCrawler" 


-- | <p>Creates a new database in a Data Catalog.</p>
createDatabase :: forall eff. CreateDatabaseRequest -> Aff (err :: AWS.RequestError | eff) CreateDatabaseResponse
createDatabase = AWS.request serviceName "createDatabase" 


-- | <p>Creates a new DevEndpoint.</p>
createDevEndpoint :: forall eff. CreateDevEndpointRequest -> Aff (err :: AWS.RequestError | eff) CreateDevEndpointResponse
createDevEndpoint = AWS.request serviceName "createDevEndpoint" 


-- | <p>Creates a new job.</p>
createJob :: forall eff. CreateJobRequest -> Aff (err :: AWS.RequestError | eff) CreateJobResponse
createJob = AWS.request serviceName "createJob" 


-- | <p>Creates a new partition.</p>
createPartition :: forall eff. CreatePartitionRequest -> Aff (err :: AWS.RequestError | eff) CreatePartitionResponse
createPartition = AWS.request serviceName "createPartition" 


-- | <p>Transforms a directed acyclic graph (DAG) into code.</p>
createScript :: forall eff. CreateScriptRequest -> Aff (err :: AWS.RequestError | eff) CreateScriptResponse
createScript = AWS.request serviceName "createScript" 


-- | <p>Creates a new table definition in the Data Catalog.</p>
createTable :: forall eff. CreateTableRequest -> Aff (err :: AWS.RequestError | eff) CreateTableResponse
createTable = AWS.request serviceName "createTable" 


-- | <p>Creates a new trigger.</p>
createTrigger :: forall eff. CreateTriggerRequest -> Aff (err :: AWS.RequestError | eff) CreateTriggerResponse
createTrigger = AWS.request serviceName "createTrigger" 


-- | <p>Creates a new function definition in the Data Catalog.</p>
createUserDefinedFunction :: forall eff. CreateUserDefinedFunctionRequest -> Aff (err :: AWS.RequestError | eff) CreateUserDefinedFunctionResponse
createUserDefinedFunction = AWS.request serviceName "createUserDefinedFunction" 


-- | <p>Removes a classifier from the Data Catalog.</p>
deleteClassifier :: forall eff. DeleteClassifierRequest -> Aff (err :: AWS.RequestError | eff) DeleteClassifierResponse
deleteClassifier = AWS.request serviceName "deleteClassifier" 


-- | <p>Deletes a connection from the Data Catalog.</p>
deleteConnection :: forall eff. DeleteConnectionRequest -> Aff (err :: AWS.RequestError | eff) DeleteConnectionResponse
deleteConnection = AWS.request serviceName "deleteConnection" 


-- | <p>Removes a specified crawler from the Data Catalog, unless the crawler state is <code>RUNNING</code>.</p>
deleteCrawler :: forall eff. DeleteCrawlerRequest -> Aff (err :: AWS.RequestError | eff) DeleteCrawlerResponse
deleteCrawler = AWS.request serviceName "deleteCrawler" 


-- | <p>Removes a specified Database from a Data Catalog.</p>
deleteDatabase :: forall eff. DeleteDatabaseRequest -> Aff (err :: AWS.RequestError | eff) DeleteDatabaseResponse
deleteDatabase = AWS.request serviceName "deleteDatabase" 


-- | <p>Deletes a specified DevEndpoint.</p>
deleteDevEndpoint :: forall eff. DeleteDevEndpointRequest -> Aff (err :: AWS.RequestError | eff) DeleteDevEndpointResponse
deleteDevEndpoint = AWS.request serviceName "deleteDevEndpoint" 


-- | <p>Deletes a specified job. If the job is not found, no exception is thrown.</p>
deleteJob :: forall eff. DeleteJobRequest -> Aff (err :: AWS.RequestError | eff) DeleteJobResponse
deleteJob = AWS.request serviceName "deleteJob" 


-- | <p>Deletes a specified partition.</p>
deletePartition :: forall eff. DeletePartitionRequest -> Aff (err :: AWS.RequestError | eff) DeletePartitionResponse
deletePartition = AWS.request serviceName "deletePartition" 


-- | <p>Removes a table definition from the Data Catalog.</p>
deleteTable :: forall eff. DeleteTableRequest -> Aff (err :: AWS.RequestError | eff) DeleteTableResponse
deleteTable = AWS.request serviceName "deleteTable" 


-- | <p>Deletes a specified version of a table.</p>
deleteTableVersion :: forall eff. DeleteTableVersionRequest -> Aff (err :: AWS.RequestError | eff) DeleteTableVersionResponse
deleteTableVersion = AWS.request serviceName "deleteTableVersion" 


-- | <p>Deletes a specified trigger. If the trigger is not found, no exception is thrown.</p>
deleteTrigger :: forall eff. DeleteTriggerRequest -> Aff (err :: AWS.RequestError | eff) DeleteTriggerResponse
deleteTrigger = AWS.request serviceName "deleteTrigger" 


-- | <p>Deletes an existing function definition from the Data Catalog.</p>
deleteUserDefinedFunction :: forall eff. DeleteUserDefinedFunctionRequest -> Aff (err :: AWS.RequestError | eff) DeleteUserDefinedFunctionResponse
deleteUserDefinedFunction = AWS.request serviceName "deleteUserDefinedFunction" 


-- | <p>Retrieves the status of a migration operation.</p>
getCatalogImportStatus :: forall eff. GetCatalogImportStatusRequest -> Aff (err :: AWS.RequestError | eff) GetCatalogImportStatusResponse
getCatalogImportStatus = AWS.request serviceName "getCatalogImportStatus" 


-- | <p>Retrieve a classifier by name.</p>
getClassifier :: forall eff. GetClassifierRequest -> Aff (err :: AWS.RequestError | eff) GetClassifierResponse
getClassifier = AWS.request serviceName "getClassifier" 


-- | <p>Lists all classifier objects in the Data Catalog.</p>
getClassifiers :: forall eff. GetClassifiersRequest -> Aff (err :: AWS.RequestError | eff) GetClassifiersResponse
getClassifiers = AWS.request serviceName "getClassifiers" 


-- | <p>Retrieves a connection definition from the Data Catalog.</p>
getConnection :: forall eff. GetConnectionRequest -> Aff (err :: AWS.RequestError | eff) GetConnectionResponse
getConnection = AWS.request serviceName "getConnection" 


-- | <p>Retrieves a list of connection definitions from the Data Catalog.</p>
getConnections :: forall eff. GetConnectionsRequest -> Aff (err :: AWS.RequestError | eff) GetConnectionsResponse
getConnections = AWS.request serviceName "getConnections" 


-- | <p>Retrieves metadata for a specified crawler.</p>
getCrawler :: forall eff. GetCrawlerRequest -> Aff (err :: AWS.RequestError | eff) GetCrawlerResponse
getCrawler = AWS.request serviceName "getCrawler" 


-- | <p>Retrieves metrics about specified crawlers.</p>
getCrawlerMetrics :: forall eff. GetCrawlerMetricsRequest -> Aff (err :: AWS.RequestError | eff) GetCrawlerMetricsResponse
getCrawlerMetrics = AWS.request serviceName "getCrawlerMetrics" 


-- | <p>Retrieves metadata for all crawlers defined in the customer account.</p>
getCrawlers :: forall eff. GetCrawlersRequest -> Aff (err :: AWS.RequestError | eff) GetCrawlersResponse
getCrawlers = AWS.request serviceName "getCrawlers" 


-- | <p>Retrieves the definition of a specified database.</p>
getDatabase :: forall eff. GetDatabaseRequest -> Aff (err :: AWS.RequestError | eff) GetDatabaseResponse
getDatabase = AWS.request serviceName "getDatabase" 


-- | <p>Retrieves all Databases defined in a given Data Catalog.</p>
getDatabases :: forall eff. GetDatabasesRequest -> Aff (err :: AWS.RequestError | eff) GetDatabasesResponse
getDatabases = AWS.request serviceName "getDatabases" 


-- | <p>Transforms a Python script into a directed acyclic graph (DAG). </p>
getDataflowGraph :: forall eff. GetDataflowGraphRequest -> Aff (err :: AWS.RequestError | eff) GetDataflowGraphResponse
getDataflowGraph = AWS.request serviceName "getDataflowGraph" 


-- | <p>Retrieves information about a specified DevEndpoint.</p>
getDevEndpoint :: forall eff. GetDevEndpointRequest -> Aff (err :: AWS.RequestError | eff) GetDevEndpointResponse
getDevEndpoint = AWS.request serviceName "getDevEndpoint" 


-- | <p>Retrieves all the DevEndpoints in this AWS account.</p>
getDevEndpoints :: forall eff. GetDevEndpointsRequest -> Aff (err :: AWS.RequestError | eff) GetDevEndpointsResponse
getDevEndpoints = AWS.request serviceName "getDevEndpoints" 


-- | <p>Retrieves an existing job definition.</p>
getJob :: forall eff. GetJobRequest -> Aff (err :: AWS.RequestError | eff) GetJobResponse
getJob = AWS.request serviceName "getJob" 


-- | <p>Retrieves the metadata for a given job run.</p>
getJobRun :: forall eff. GetJobRunRequest -> Aff (err :: AWS.RequestError | eff) GetJobRunResponse
getJobRun = AWS.request serviceName "getJobRun" 


-- | <p>Retrieves metadata for all runs of a given job.</p>
getJobRuns :: forall eff. GetJobRunsRequest -> Aff (err :: AWS.RequestError | eff) GetJobRunsResponse
getJobRuns = AWS.request serviceName "getJobRuns" 


-- | <p>Retrieves all current jobs.</p>
getJobs :: forall eff. GetJobsRequest -> Aff (err :: AWS.RequestError | eff) GetJobsResponse
getJobs = AWS.request serviceName "getJobs" 


-- | <p>Creates mappings.</p>
getMapping :: forall eff. GetMappingRequest -> Aff (err :: AWS.RequestError | eff) GetMappingResponse
getMapping = AWS.request serviceName "getMapping" 


-- | <p>Retrieves information about a specified partition.</p>
getPartition :: forall eff. GetPartitionRequest -> Aff (err :: AWS.RequestError | eff) GetPartitionResponse
getPartition = AWS.request serviceName "getPartition" 


-- | <p>Retrieves information about the partitions in a table.</p>
getPartitions :: forall eff. GetPartitionsRequest -> Aff (err :: AWS.RequestError | eff) GetPartitionsResponse
getPartitions = AWS.request serviceName "getPartitions" 


-- | <p>Gets code to perform a specified mapping.</p>
getPlan :: forall eff. GetPlanRequest -> Aff (err :: AWS.RequestError | eff) GetPlanResponse
getPlan = AWS.request serviceName "getPlan" 


-- | <p>Retrieves the <code>Table</code> definition in a Data Catalog for a specified table.</p>
getTable :: forall eff. GetTableRequest -> Aff (err :: AWS.RequestError | eff) GetTableResponse
getTable = AWS.request serviceName "getTable" 


-- | <p>Retrieves a specified version of a table.</p>
getTableVersion :: forall eff. GetTableVersionRequest -> Aff (err :: AWS.RequestError | eff) GetTableVersionResponse
getTableVersion = AWS.request serviceName "getTableVersion" 


-- | <p>Retrieves a list of strings that identify available versions of a specified table.</p>
getTableVersions :: forall eff. GetTableVersionsRequest -> Aff (err :: AWS.RequestError | eff) GetTableVersionsResponse
getTableVersions = AWS.request serviceName "getTableVersions" 


-- | <p>Retrieves the definitions of some or all of the tables in a given <code>Database</code>.</p>
getTables :: forall eff. GetTablesRequest -> Aff (err :: AWS.RequestError | eff) GetTablesResponse
getTables = AWS.request serviceName "getTables" 


-- | <p>Retrieves the definition of a trigger.</p>
getTrigger :: forall eff. GetTriggerRequest -> Aff (err :: AWS.RequestError | eff) GetTriggerResponse
getTrigger = AWS.request serviceName "getTrigger" 


-- | <p>Gets all the triggers associated with a job.</p>
getTriggers :: forall eff. GetTriggersRequest -> Aff (err :: AWS.RequestError | eff) GetTriggersResponse
getTriggers = AWS.request serviceName "getTriggers" 


-- | <p>Retrieves a specified function definition from the Data Catalog.</p>
getUserDefinedFunction :: forall eff. GetUserDefinedFunctionRequest -> Aff (err :: AWS.RequestError | eff) GetUserDefinedFunctionResponse
getUserDefinedFunction = AWS.request serviceName "getUserDefinedFunction" 


-- | <p>Retrieves a multiple function definitions from the Data Catalog.</p>
getUserDefinedFunctions :: forall eff. GetUserDefinedFunctionsRequest -> Aff (err :: AWS.RequestError | eff) GetUserDefinedFunctionsResponse
getUserDefinedFunctions = AWS.request serviceName "getUserDefinedFunctions" 


-- | <p>Imports an existing Athena Data Catalog to AWS Glue</p>
importCatalogToGlue :: forall eff. ImportCatalogToGlueRequest -> Aff (err :: AWS.RequestError | eff) ImportCatalogToGlueResponse
importCatalogToGlue = AWS.request serviceName "importCatalogToGlue" 


-- | <p>Resets a bookmark entry.</p>
resetJobBookmark :: forall eff. ResetJobBookmarkRequest -> Aff (err :: AWS.RequestError | eff) ResetJobBookmarkResponse
resetJobBookmark = AWS.request serviceName "resetJobBookmark" 


-- | <p>Starts a crawl using the specified crawler, regardless of what is scheduled. If the crawler is already running, does nothing.</p>
startCrawler :: forall eff. StartCrawlerRequest -> Aff (err :: AWS.RequestError | eff) StartCrawlerResponse
startCrawler = AWS.request serviceName "startCrawler" 


-- | <p>Changes the schedule state of the specified crawler to <code>SCHEDULED</code>, unless the crawler is already running or the schedule state is already <code>SCHEDULED</code>.</p>
startCrawlerSchedule :: forall eff. StartCrawlerScheduleRequest -> Aff (err :: AWS.RequestError | eff) StartCrawlerScheduleResponse
startCrawlerSchedule = AWS.request serviceName "startCrawlerSchedule" 


-- | <p>Runs a job.</p>
startJobRun :: forall eff. StartJobRunRequest -> Aff (err :: AWS.RequestError | eff) StartJobRunResponse
startJobRun = AWS.request serviceName "startJobRun" 


-- | <p>Starts an existing trigger. See <a href="http://docs.aws.amazon.com/glue/latest/dg/trigger-job.html">Triggering Jobs</a> for information about how different types of trigger are started.</p>
startTrigger :: forall eff. StartTriggerRequest -> Aff (err :: AWS.RequestError | eff) StartTriggerResponse
startTrigger = AWS.request serviceName "startTrigger" 


-- | <p>If the specified crawler is running, stops the crawl.</p>
stopCrawler :: forall eff. StopCrawlerRequest -> Aff (err :: AWS.RequestError | eff) StopCrawlerResponse
stopCrawler = AWS.request serviceName "stopCrawler" 


-- | <p>Sets the schedule state of the specified crawler to <code>NOT_SCHEDULED</code>, but does not stop the crawler if it is already running.</p>
stopCrawlerSchedule :: forall eff. StopCrawlerScheduleRequest -> Aff (err :: AWS.RequestError | eff) StopCrawlerScheduleResponse
stopCrawlerSchedule = AWS.request serviceName "stopCrawlerSchedule" 


-- | <p>Stops a specified trigger.</p>
stopTrigger :: forall eff. StopTriggerRequest -> Aff (err :: AWS.RequestError | eff) StopTriggerResponse
stopTrigger = AWS.request serviceName "stopTrigger" 


-- | <p>Modifies an existing classifier (a <code>GrokClassifier</code>, <code>XMLClassifier</code>, or <code>JsonClassifier</code>, depending on which field is present).</p>
updateClassifier :: forall eff. UpdateClassifierRequest -> Aff (err :: AWS.RequestError | eff) UpdateClassifierResponse
updateClassifier = AWS.request serviceName "updateClassifier" 


-- | <p>Updates a connection definition in the Data Catalog.</p>
updateConnection :: forall eff. UpdateConnectionRequest -> Aff (err :: AWS.RequestError | eff) UpdateConnectionResponse
updateConnection = AWS.request serviceName "updateConnection" 


-- | <p>Updates a crawler. If a crawler is running, you must stop it using <code>StopCrawler</code> before updating it.</p>
updateCrawler :: forall eff. UpdateCrawlerRequest -> Aff (err :: AWS.RequestError | eff) UpdateCrawlerResponse
updateCrawler = AWS.request serviceName "updateCrawler" 


-- | <p>Updates the schedule of a crawler using a <code>cron</code> expression. </p>
updateCrawlerSchedule :: forall eff. UpdateCrawlerScheduleRequest -> Aff (err :: AWS.RequestError | eff) UpdateCrawlerScheduleResponse
updateCrawlerSchedule = AWS.request serviceName "updateCrawlerSchedule" 


-- | <p>Updates an existing database definition in a Data Catalog.</p>
updateDatabase :: forall eff. UpdateDatabaseRequest -> Aff (err :: AWS.RequestError | eff) UpdateDatabaseResponse
updateDatabase = AWS.request serviceName "updateDatabase" 


-- | <p>Updates a specified DevEndpoint.</p>
updateDevEndpoint :: forall eff. UpdateDevEndpointRequest -> Aff (err :: AWS.RequestError | eff) UpdateDevEndpointResponse
updateDevEndpoint = AWS.request serviceName "updateDevEndpoint" 


-- | <p>Updates an existing job definition.</p>
updateJob :: forall eff. UpdateJobRequest -> Aff (err :: AWS.RequestError | eff) UpdateJobResponse
updateJob = AWS.request serviceName "updateJob" 


-- | <p>Updates a partition.</p>
updatePartition :: forall eff. UpdatePartitionRequest -> Aff (err :: AWS.RequestError | eff) UpdatePartitionResponse
updatePartition = AWS.request serviceName "updatePartition" 


-- | <p>Updates a metadata table in the Data Catalog.</p>
updateTable :: forall eff. UpdateTableRequest -> Aff (err :: AWS.RequestError | eff) UpdateTableResponse
updateTable = AWS.request serviceName "updateTable" 


-- | <p>Updates a trigger definition.</p>
updateTrigger :: forall eff. UpdateTriggerRequest -> Aff (err :: AWS.RequestError | eff) UpdateTriggerResponse
updateTrigger = AWS.request serviceName "updateTrigger" 


-- | <p>Updates an existing function definition in the Data Catalog.</p>
updateUserDefinedFunction :: forall eff. UpdateUserDefinedFunctionRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserDefinedFunctionResponse
updateUserDefinedFunction = AWS.request serviceName "updateUserDefinedFunction" 


-- | <p>Access to a resource was denied.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _


-- | <p>Defines an action to be initiated by a trigger.</p>
newtype Action = Action 
  { "JobName" :: NullOrUndefined (NameString)
  , "Arguments" :: NullOrUndefined (GenericMap)
  }
derive instance newtypeAction :: Newtype Action _


newtype ActionList = ActionList (Array Action)
derive instance newtypeActionList :: Newtype ActionList _


-- | <p>A resource to be created or added already exists.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _


newtype AttemptCount = AttemptCount Int
derive instance newtypeAttemptCount :: Newtype AttemptCount _


newtype BatchCreatePartitionRequest = BatchCreatePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionInputList" :: (PartitionInputList)
  }
derive instance newtypeBatchCreatePartitionRequest :: Newtype BatchCreatePartitionRequest _


newtype BatchCreatePartitionResponse = BatchCreatePartitionResponse 
  { "Errors" :: NullOrUndefined (PartitionErrors)
  }
derive instance newtypeBatchCreatePartitionResponse :: Newtype BatchCreatePartitionResponse _


newtype BatchDeleteConnectionRequest = BatchDeleteConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "ConnectionNameList" :: (DeleteConnectionNameList)
  }
derive instance newtypeBatchDeleteConnectionRequest :: Newtype BatchDeleteConnectionRequest _


newtype BatchDeleteConnectionResponse = BatchDeleteConnectionResponse 
  { "Succeeded" :: NullOrUndefined (NameStringList)
  , "Errors" :: NullOrUndefined (ErrorByName)
  }
derive instance newtypeBatchDeleteConnectionResponse :: Newtype BatchDeleteConnectionResponse _


newtype BatchDeletePartitionRequest = BatchDeletePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionsToDelete" :: (BatchDeletePartitionValueList)
  }
derive instance newtypeBatchDeletePartitionRequest :: Newtype BatchDeletePartitionRequest _


newtype BatchDeletePartitionResponse = BatchDeletePartitionResponse 
  { "Errors" :: NullOrUndefined (PartitionErrors)
  }
derive instance newtypeBatchDeletePartitionResponse :: Newtype BatchDeletePartitionResponse _


newtype BatchDeletePartitionValueList = BatchDeletePartitionValueList (Array PartitionValueList)
derive instance newtypeBatchDeletePartitionValueList :: Newtype BatchDeletePartitionValueList _


newtype BatchDeleteTableNameList = BatchDeleteTableNameList (Array NameString)
derive instance newtypeBatchDeleteTableNameList :: Newtype BatchDeleteTableNameList _


newtype BatchDeleteTableRequest = BatchDeleteTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TablesToDelete" :: (BatchDeleteTableNameList)
  }
derive instance newtypeBatchDeleteTableRequest :: Newtype BatchDeleteTableRequest _


newtype BatchDeleteTableResponse = BatchDeleteTableResponse 
  { "Errors" :: NullOrUndefined (TableErrors)
  }
derive instance newtypeBatchDeleteTableResponse :: Newtype BatchDeleteTableResponse _


newtype BatchDeleteTableVersionList = BatchDeleteTableVersionList (Array VersionString)
derive instance newtypeBatchDeleteTableVersionList :: Newtype BatchDeleteTableVersionList _


newtype BatchDeleteTableVersionRequest = BatchDeleteTableVersionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionIds" :: (BatchDeleteTableVersionList)
  }
derive instance newtypeBatchDeleteTableVersionRequest :: Newtype BatchDeleteTableVersionRequest _


newtype BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse 
  { "Errors" :: NullOrUndefined (TableVersionErrors)
  }
derive instance newtypeBatchDeleteTableVersionResponse :: Newtype BatchDeleteTableVersionResponse _


newtype BatchGetPartitionRequest = BatchGetPartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionsToGet" :: (BatchGetPartitionValueList)
  }
derive instance newtypeBatchGetPartitionRequest :: Newtype BatchGetPartitionRequest _


newtype BatchGetPartitionResponse = BatchGetPartitionResponse 
  { "Partitions" :: NullOrUndefined (PartitionList)
  , "UnprocessedKeys" :: NullOrUndefined (BatchGetPartitionValueList)
  }
derive instance newtypeBatchGetPartitionResponse :: Newtype BatchGetPartitionResponse _


newtype BatchGetPartitionValueList = BatchGetPartitionValueList (Array PartitionValueList)
derive instance newtypeBatchGetPartitionValueList :: Newtype BatchGetPartitionValueList _


-- | <p>Records an error that occurred when attempting to stop a specified JobRun.</p>
newtype BatchStopJobRunError = BatchStopJobRunError 
  { "JobName" :: NullOrUndefined (NameString)
  , "JobRunId" :: NullOrUndefined (IdString)
  , "ErrorDetail" :: NullOrUndefined (ErrorDetail)
  }
derive instance newtypeBatchStopJobRunError :: Newtype BatchStopJobRunError _


newtype BatchStopJobRunErrorList = BatchStopJobRunErrorList (Array BatchStopJobRunError)
derive instance newtypeBatchStopJobRunErrorList :: Newtype BatchStopJobRunErrorList _


newtype BatchStopJobRunJobRunIdList = BatchStopJobRunJobRunIdList (Array IdString)
derive instance newtypeBatchStopJobRunJobRunIdList :: Newtype BatchStopJobRunJobRunIdList _


newtype BatchStopJobRunRequest = BatchStopJobRunRequest 
  { "JobName" :: (NameString)
  , "JobRunIds" :: (BatchStopJobRunJobRunIdList)
  }
derive instance newtypeBatchStopJobRunRequest :: Newtype BatchStopJobRunRequest _


newtype BatchStopJobRunResponse = BatchStopJobRunResponse 
  { "SuccessfulSubmissions" :: NullOrUndefined (BatchStopJobRunSuccessfulSubmissionList)
  , "Errors" :: NullOrUndefined (BatchStopJobRunErrorList)
  }
derive instance newtypeBatchStopJobRunResponse :: Newtype BatchStopJobRunResponse _


-- | <p>Records a successful request to stop a specified JobRun.</p>
newtype BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission 
  { "JobName" :: NullOrUndefined (NameString)
  , "JobRunId" :: NullOrUndefined (IdString)
  }
derive instance newtypeBatchStopJobRunSuccessfulSubmission :: Newtype BatchStopJobRunSuccessfulSubmission _


newtype BatchStopJobRunSuccessfulSubmissionList = BatchStopJobRunSuccessfulSubmissionList (Array BatchStopJobRunSuccessfulSubmission)
derive instance newtypeBatchStopJobRunSuccessfulSubmissionList :: Newtype BatchStopJobRunSuccessfulSubmissionList _


newtype BooleanNullable = BooleanNullable Boolean
derive instance newtypeBooleanNullable :: Newtype BooleanNullable _


newtype BooleanValue = BooleanValue Boolean
derive instance newtypeBooleanValue :: Newtype BooleanValue _


newtype BoundedPartitionValueList = BoundedPartitionValueList (Array ValueString)
derive instance newtypeBoundedPartitionValueList :: Newtype BoundedPartitionValueList _


newtype CatalogEntries = CatalogEntries (Array CatalogEntry)
derive instance newtypeCatalogEntries :: Newtype CatalogEntries _


-- | <p>Specifies a table definition in the Data Catalog.</p>
newtype CatalogEntry = CatalogEntry 
  { "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  }
derive instance newtypeCatalogEntry :: Newtype CatalogEntry _


newtype CatalogIdString = CatalogIdString String
derive instance newtypeCatalogIdString :: Newtype CatalogIdString _


-- | <p>A structure containing migration status information.</p>
newtype CatalogImportStatus = CatalogImportStatus 
  { "ImportCompleted" :: NullOrUndefined (Boolean)
  , "ImportTime" :: NullOrUndefined (Number)
  , "ImportedBy" :: NullOrUndefined (NameString)
  }
derive instance newtypeCatalogImportStatus :: Newtype CatalogImportStatus _


newtype Classification = Classification String
derive instance newtypeClassification :: Newtype Classification _


-- | <p>Classifiers are written in Python and triggered during a crawl task. You can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier checks whether a given file is in a format it can handle, and if it is, the classifier creates a schema in the form of a <code>StructType</code> object that matches that data format.</p> <p>A classifier can be a <code>grok</code> classifier, an XML classifier, or a JSON classifier, asspecified in one of the fields in the <code>Classifier</code> object.</p>
newtype Classifier = Classifier 
  { "GrokClassifier" :: NullOrUndefined (GrokClassifier)
  , "XMLClassifier" :: NullOrUndefined (XMLClassifier)
  , "JsonClassifier" :: NullOrUndefined (JsonClassifier)
  }
derive instance newtypeClassifier :: Newtype Classifier _


newtype ClassifierList = ClassifierList (Array Classifier)
derive instance newtypeClassifierList :: Newtype ClassifierList _


newtype ClassifierNameList = ClassifierNameList (Array NameString)
derive instance newtypeClassifierNameList :: Newtype ClassifierNameList _


newtype CodeGenArgName = CodeGenArgName String
derive instance newtypeCodeGenArgName :: Newtype CodeGenArgName _


newtype CodeGenArgValue = CodeGenArgValue String
derive instance newtypeCodeGenArgValue :: Newtype CodeGenArgValue _


-- | <p>Represents a directional edge in a directed acyclic graph (DAG).</p>
newtype CodeGenEdge = CodeGenEdge 
  { "Source" :: (CodeGenIdentifier)
  , "Target" :: (CodeGenIdentifier)
  , "TargetParameter" :: NullOrUndefined (CodeGenArgName)
  }
derive instance newtypeCodeGenEdge :: Newtype CodeGenEdge _


newtype CodeGenIdentifier = CodeGenIdentifier String
derive instance newtypeCodeGenIdentifier :: Newtype CodeGenIdentifier _


-- | <p>Represents a node in a directed acyclic graph (DAG)</p>
newtype CodeGenNode = CodeGenNode 
  { "Id" :: (CodeGenIdentifier)
  , "NodeType" :: (CodeGenNodeType)
  , "Args" :: (CodeGenNodeArgs)
  , "LineNumber" :: NullOrUndefined (Int)
  }
derive instance newtypeCodeGenNode :: Newtype CodeGenNode _


-- | <p>An argument or property of a node.</p>
newtype CodeGenNodeArg = CodeGenNodeArg 
  { "Name" :: (CodeGenArgName)
  , "Value" :: (CodeGenArgValue)
  , "Param" :: NullOrUndefined (Boolean)
  }
derive instance newtypeCodeGenNodeArg :: Newtype CodeGenNodeArg _


newtype CodeGenNodeArgs = CodeGenNodeArgs (Array CodeGenNodeArg)
derive instance newtypeCodeGenNodeArgs :: Newtype CodeGenNodeArgs _


newtype CodeGenNodeType = CodeGenNodeType String
derive instance newtypeCodeGenNodeType :: Newtype CodeGenNodeType _


-- | <p>A column in a <code>Table</code>.</p>
newtype Column = Column 
  { "Name" :: (NameString)
  , "Type" :: NullOrUndefined (ColumnTypeString)
  , "Comment" :: NullOrUndefined (CommentString)
  }
derive instance newtypeColumn :: Newtype Column _


newtype ColumnList = ColumnList (Array Column)
derive instance newtypeColumnList :: Newtype ColumnList _


newtype ColumnTypeString = ColumnTypeString String
derive instance newtypeColumnTypeString :: Newtype ColumnTypeString _


newtype ColumnValueStringList = ColumnValueStringList (Array ColumnValuesString)
derive instance newtypeColumnValueStringList :: Newtype ColumnValueStringList _


newtype ColumnValuesString = ColumnValuesString String
derive instance newtypeColumnValuesString :: Newtype ColumnValuesString _


newtype CommentString = CommentString String
derive instance newtypeCommentString :: Newtype CommentString _


-- | <p>Two processes are trying to modify a resource simultaneously.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


-- | <p>Too many jobs are being run concurrently.</p>
newtype ConcurrentRunsExceededException = ConcurrentRunsExceededException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeConcurrentRunsExceededException :: Newtype ConcurrentRunsExceededException _


-- | <p>Defines a condition under which a trigger fires.</p>
newtype Condition = Condition 
  { "LogicalOperator" :: NullOrUndefined (LogicalOperator)
  , "JobName" :: NullOrUndefined (NameString)
  , "State" :: NullOrUndefined (JobRunState)
  }
derive instance newtypeCondition :: Newtype Condition _


newtype ConditionList = ConditionList (Array Condition)
derive instance newtypeConditionList :: Newtype ConditionList _


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
derive instance newtypeConnection :: Newtype Connection _


-- | <p>A structure used to specify a connection to create or update.</p>
newtype ConnectionInput = ConnectionInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "ConnectionType" :: (ConnectionType)
  , "MatchCriteria" :: NullOrUndefined (MatchCriteria)
  , "ConnectionProperties" :: (ConnectionProperties)
  , "PhysicalConnectionRequirements" :: NullOrUndefined (PhysicalConnectionRequirements)
  }
derive instance newtypeConnectionInput :: Newtype ConnectionInput _


newtype ConnectionList = ConnectionList (Array Connection)
derive instance newtypeConnectionList :: Newtype ConnectionList _


newtype ConnectionName = ConnectionName String
derive instance newtypeConnectionName :: Newtype ConnectionName _


newtype ConnectionProperties = ConnectionProperties (Map ConnectionPropertyKey ValueString)
derive instance newtypeConnectionProperties :: Newtype ConnectionProperties _


newtype ConnectionPropertyKey = ConnectionPropertyKey String
derive instance newtypeConnectionPropertyKey :: Newtype ConnectionPropertyKey _


newtype ConnectionType = ConnectionType String
derive instance newtypeConnectionType :: Newtype ConnectionType _


-- | <p>Specifies the connections used by a job.</p>
newtype ConnectionsList = ConnectionsList 
  { "Connections" :: NullOrUndefined (StringList)
  }
derive instance newtypeConnectionsList :: Newtype ConnectionsList _


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
derive instance newtypeCrawler :: Newtype Crawler _


newtype CrawlerConfiguration = CrawlerConfiguration String
derive instance newtypeCrawlerConfiguration :: Newtype CrawlerConfiguration _


newtype CrawlerList = CrawlerList (Array Crawler)
derive instance newtypeCrawlerList :: Newtype CrawlerList _


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
derive instance newtypeCrawlerMetrics :: Newtype CrawlerMetrics _


newtype CrawlerMetricsList = CrawlerMetricsList (Array CrawlerMetrics)
derive instance newtypeCrawlerMetricsList :: Newtype CrawlerMetricsList _


newtype CrawlerNameList = CrawlerNameList (Array NameString)
derive instance newtypeCrawlerNameList :: Newtype CrawlerNameList _


-- | <p>The specified crawler is not running.</p>
newtype CrawlerNotRunningException = CrawlerNotRunningException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeCrawlerNotRunningException :: Newtype CrawlerNotRunningException _


-- | <p>The operation cannot be performed because the crawler is already running.</p>
newtype CrawlerRunningException = CrawlerRunningException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeCrawlerRunningException :: Newtype CrawlerRunningException _


newtype CrawlerState = CrawlerState String
derive instance newtypeCrawlerState :: Newtype CrawlerState _


-- | <p>The specified crawler is stopping.</p>
newtype CrawlerStoppingException = CrawlerStoppingException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeCrawlerStoppingException :: Newtype CrawlerStoppingException _


-- | <p>Specifies data stores to crawl.</p>
newtype CrawlerTargets = CrawlerTargets 
  { "S3Targets" :: NullOrUndefined (S3TargetList)
  , "JdbcTargets" :: NullOrUndefined (JdbcTargetList)
  }
derive instance newtypeCrawlerTargets :: Newtype CrawlerTargets _


newtype CreateClassifierRequest = CreateClassifierRequest 
  { "GrokClassifier" :: NullOrUndefined (CreateGrokClassifierRequest)
  , "XMLClassifier" :: NullOrUndefined (CreateXMLClassifierRequest)
  , "JsonClassifier" :: NullOrUndefined (CreateJsonClassifierRequest)
  }
derive instance newtypeCreateClassifierRequest :: Newtype CreateClassifierRequest _


newtype CreateClassifierResponse = CreateClassifierResponse 
  { 
  }
derive instance newtypeCreateClassifierResponse :: Newtype CreateClassifierResponse _


newtype CreateConnectionRequest = CreateConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "ConnectionInput" :: (ConnectionInput)
  }
derive instance newtypeCreateConnectionRequest :: Newtype CreateConnectionRequest _


newtype CreateConnectionResponse = CreateConnectionResponse 
  { 
  }
derive instance newtypeCreateConnectionResponse :: Newtype CreateConnectionResponse _


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
derive instance newtypeCreateCrawlerRequest :: Newtype CreateCrawlerRequest _


newtype CreateCrawlerResponse = CreateCrawlerResponse 
  { 
  }
derive instance newtypeCreateCrawlerResponse :: Newtype CreateCrawlerResponse _


newtype CreateDatabaseRequest = CreateDatabaseRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseInput" :: (DatabaseInput)
  }
derive instance newtypeCreateDatabaseRequest :: Newtype CreateDatabaseRequest _


newtype CreateDatabaseResponse = CreateDatabaseResponse 
  { 
  }
derive instance newtypeCreateDatabaseResponse :: Newtype CreateDatabaseResponse _


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
derive instance newtypeCreateDevEndpointRequest :: Newtype CreateDevEndpointRequest _


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
derive instance newtypeCreateDevEndpointResponse :: Newtype CreateDevEndpointResponse _


-- | <p>Specifies a <code>grok</code> classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateGrokClassifierRequest = CreateGrokClassifierRequest 
  { "Classification" :: (Classification)
  , "Name" :: (NameString)
  , "GrokPattern" :: (GrokPattern)
  , "CustomPatterns" :: NullOrUndefined (CustomPatterns)
  }
derive instance newtypeCreateGrokClassifierRequest :: Newtype CreateGrokClassifierRequest _


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
derive instance newtypeCreateJobRequest :: Newtype CreateJobRequest _


newtype CreateJobResponse = CreateJobResponse 
  { "Name" :: NullOrUndefined (NameString)
  }
derive instance newtypeCreateJobResponse :: Newtype CreateJobResponse _


-- | <p>Specifies a JSON classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateJsonClassifierRequest = CreateJsonClassifierRequest 
  { "Name" :: (NameString)
  , "JsonPath" :: (JsonPath)
  }
derive instance newtypeCreateJsonClassifierRequest :: Newtype CreateJsonClassifierRequest _


newtype CreatePartitionRequest = CreatePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionInput" :: (PartitionInput)
  }
derive instance newtypeCreatePartitionRequest :: Newtype CreatePartitionRequest _


newtype CreatePartitionResponse = CreatePartitionResponse 
  { 
  }
derive instance newtypeCreatePartitionResponse :: Newtype CreatePartitionResponse _


newtype CreateScriptRequest = CreateScriptRequest 
  { "DagNodes" :: NullOrUndefined (DagNodes)
  , "DagEdges" :: NullOrUndefined (DagEdges)
  , "Language" :: NullOrUndefined (Language)
  }
derive instance newtypeCreateScriptRequest :: Newtype CreateScriptRequest _


newtype CreateScriptResponse = CreateScriptResponse 
  { "PythonScript" :: NullOrUndefined (PythonScript)
  , "ScalaCode" :: NullOrUndefined (ScalaCode)
  }
derive instance newtypeCreateScriptResponse :: Newtype CreateScriptResponse _


newtype CreateTableRequest = CreateTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableInput" :: (TableInput)
  }
derive instance newtypeCreateTableRequest :: Newtype CreateTableRequest _


newtype CreateTableResponse = CreateTableResponse 
  { 
  }
derive instance newtypeCreateTableResponse :: Newtype CreateTableResponse _


newtype CreateTriggerRequest = CreateTriggerRequest 
  { "Name" :: (NameString)
  , "Type" :: (TriggerType)
  , "Schedule" :: NullOrUndefined (GenericString)
  , "Predicate" :: NullOrUndefined (Predicate)
  , "Actions" :: (ActionList)
  , "Description" :: NullOrUndefined (DescriptionString)
  }
derive instance newtypeCreateTriggerRequest :: Newtype CreateTriggerRequest _


newtype CreateTriggerResponse = CreateTriggerResponse 
  { "Name" :: NullOrUndefined (NameString)
  }
derive instance newtypeCreateTriggerResponse :: Newtype CreateTriggerResponse _


newtype CreateUserDefinedFunctionRequest = CreateUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionInput" :: (UserDefinedFunctionInput)
  }
derive instance newtypeCreateUserDefinedFunctionRequest :: Newtype CreateUserDefinedFunctionRequest _


newtype CreateUserDefinedFunctionResponse = CreateUserDefinedFunctionResponse 
  { 
  }
derive instance newtypeCreateUserDefinedFunctionResponse :: Newtype CreateUserDefinedFunctionResponse _


-- | <p>Specifies an XML classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateXMLClassifierRequest = CreateXMLClassifierRequest 
  { "Classification" :: (Classification)
  , "Name" :: (NameString)
  , "RowTag" :: NullOrUndefined (RowTag)
  }
derive instance newtypeCreateXMLClassifierRequest :: Newtype CreateXMLClassifierRequest _


newtype CronExpression = CronExpression String
derive instance newtypeCronExpression :: Newtype CronExpression _


newtype CustomPatterns = CustomPatterns String
derive instance newtypeCustomPatterns :: Newtype CustomPatterns _


newtype DagEdges = DagEdges (Array CodeGenEdge)
derive instance newtypeDagEdges :: Newtype DagEdges _


newtype DagNodes = DagNodes (Array CodeGenNode)
derive instance newtypeDagNodes :: Newtype DagNodes _


-- | <p>The <code>Database</code> object represents a logical grouping of tables that may reside in a Hive metastore or an RDBMS.</p>
newtype Database = Database 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "LocationUri" :: NullOrUndefined (URI)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  , "CreateTime" :: NullOrUndefined (Number)
  }
derive instance newtypeDatabase :: Newtype Database _


-- | <p>The structure used to create or update a database.</p>
newtype DatabaseInput = DatabaseInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "LocationUri" :: NullOrUndefined (URI)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  }
derive instance newtypeDatabaseInput :: Newtype DatabaseInput _


newtype DatabaseList = DatabaseList (Array Database)
derive instance newtypeDatabaseList :: Newtype DatabaseList _


newtype DatabaseName = DatabaseName String
derive instance newtypeDatabaseName :: Newtype DatabaseName _


newtype DeleteBehavior = DeleteBehavior String
derive instance newtypeDeleteBehavior :: Newtype DeleteBehavior _


newtype DeleteClassifierRequest = DeleteClassifierRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeDeleteClassifierRequest :: Newtype DeleteClassifierRequest _


newtype DeleteClassifierResponse = DeleteClassifierResponse 
  { 
  }
derive instance newtypeDeleteClassifierResponse :: Newtype DeleteClassifierResponse _


newtype DeleteConnectionNameList = DeleteConnectionNameList (Array NameString)
derive instance newtypeDeleteConnectionNameList :: Newtype DeleteConnectionNameList _


newtype DeleteConnectionRequest = DeleteConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "ConnectionName" :: (NameString)
  }
derive instance newtypeDeleteConnectionRequest :: Newtype DeleteConnectionRequest _


newtype DeleteConnectionResponse = DeleteConnectionResponse 
  { 
  }
derive instance newtypeDeleteConnectionResponse :: Newtype DeleteConnectionResponse _


newtype DeleteCrawlerRequest = DeleteCrawlerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeDeleteCrawlerRequest :: Newtype DeleteCrawlerRequest _


newtype DeleteCrawlerResponse = DeleteCrawlerResponse 
  { 
  }
derive instance newtypeDeleteCrawlerResponse :: Newtype DeleteCrawlerResponse _


newtype DeleteDatabaseRequest = DeleteDatabaseRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }
derive instance newtypeDeleteDatabaseRequest :: Newtype DeleteDatabaseRequest _


newtype DeleteDatabaseResponse = DeleteDatabaseResponse 
  { 
  }
derive instance newtypeDeleteDatabaseResponse :: Newtype DeleteDatabaseResponse _


newtype DeleteDevEndpointRequest = DeleteDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  }
derive instance newtypeDeleteDevEndpointRequest :: Newtype DeleteDevEndpointRequest _


newtype DeleteDevEndpointResponse = DeleteDevEndpointResponse 
  { 
  }
derive instance newtypeDeleteDevEndpointResponse :: Newtype DeleteDevEndpointResponse _


newtype DeleteJobRequest = DeleteJobRequest 
  { "JobName" :: (NameString)
  }
derive instance newtypeDeleteJobRequest :: Newtype DeleteJobRequest _


newtype DeleteJobResponse = DeleteJobResponse 
  { "JobName" :: NullOrUndefined (NameString)
  }
derive instance newtypeDeleteJobResponse :: Newtype DeleteJobResponse _


newtype DeletePartitionRequest = DeletePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValues" :: (ValueStringList)
  }
derive instance newtypeDeletePartitionRequest :: Newtype DeletePartitionRequest _


newtype DeletePartitionResponse = DeletePartitionResponse 
  { 
  }
derive instance newtypeDeletePartitionResponse :: Newtype DeletePartitionResponse _


newtype DeleteTableRequest = DeleteTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Name" :: (NameString)
  }
derive instance newtypeDeleteTableRequest :: Newtype DeleteTableRequest _


newtype DeleteTableResponse = DeleteTableResponse 
  { 
  }
derive instance newtypeDeleteTableResponse :: Newtype DeleteTableResponse _


newtype DeleteTableVersionRequest = DeleteTableVersionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionId" :: (VersionString)
  }
derive instance newtypeDeleteTableVersionRequest :: Newtype DeleteTableVersionRequest _


newtype DeleteTableVersionResponse = DeleteTableVersionResponse 
  { 
  }
derive instance newtypeDeleteTableVersionResponse :: Newtype DeleteTableVersionResponse _


newtype DeleteTriggerRequest = DeleteTriggerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeDeleteTriggerRequest :: Newtype DeleteTriggerRequest _


newtype DeleteTriggerResponse = DeleteTriggerResponse 
  { "Name" :: NullOrUndefined (NameString)
  }
derive instance newtypeDeleteTriggerResponse :: Newtype DeleteTriggerResponse _


newtype DeleteUserDefinedFunctionRequest = DeleteUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  }
derive instance newtypeDeleteUserDefinedFunctionRequest :: Newtype DeleteUserDefinedFunctionRequest _


newtype DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse 
  { 
  }
derive instance newtypeDeleteUserDefinedFunctionResponse :: Newtype DeleteUserDefinedFunctionResponse _


newtype DescriptionString = DescriptionString String
derive instance newtypeDescriptionString :: Newtype DescriptionString _


newtype DescriptionStringRemovable = DescriptionStringRemovable String
derive instance newtypeDescriptionStringRemovable :: Newtype DescriptionStringRemovable _


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
derive instance newtypeDevEndpoint :: Newtype DevEndpoint _


-- | <p>Custom libraries to be loaded into a DevEndpoint.</p>
newtype DevEndpointCustomLibraries = DevEndpointCustomLibraries 
  { "ExtraPythonLibsS3Path" :: NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined (GenericString)
  }
derive instance newtypeDevEndpointCustomLibraries :: Newtype DevEndpointCustomLibraries _


newtype DevEndpointList = DevEndpointList (Array DevEndpoint)
derive instance newtypeDevEndpointList :: Newtype DevEndpointList _


-- | <p>A specified entity does not exist</p>
newtype EntityNotFoundException = EntityNotFoundException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeEntityNotFoundException :: Newtype EntityNotFoundException _


newtype ErrorByName = ErrorByName (Map NameString ErrorDetail)
derive instance newtypeErrorByName :: Newtype ErrorByName _


-- | <p>Contains details about an error.</p>
newtype ErrorDetail = ErrorDetail 
  { "ErrorCode" :: NullOrUndefined (NameString)
  , "ErrorMessage" :: NullOrUndefined (DescriptionString)
  }
derive instance newtypeErrorDetail :: Newtype ErrorDetail _


newtype ErrorString = ErrorString String
derive instance newtypeErrorString :: Newtype ErrorString _


-- | <p>An execution property of a job.</p>
newtype ExecutionProperty = ExecutionProperty 
  { "MaxConcurrentRuns" :: NullOrUndefined (MaxConcurrentRuns)
  }
derive instance newtypeExecutionProperty :: Newtype ExecutionProperty _


newtype FieldType = FieldType String
derive instance newtypeFieldType :: Newtype FieldType _


newtype FilterString = FilterString String
derive instance newtypeFilterString :: Newtype FilterString _


newtype FormatString = FormatString String
derive instance newtypeFormatString :: Newtype FormatString _


newtype GenericMap = GenericMap (Map GenericString GenericString)
derive instance newtypeGenericMap :: Newtype GenericMap _


newtype GenericString = GenericString String
derive instance newtypeGenericString :: Newtype GenericString _


newtype GetCatalogImportStatusRequest = GetCatalogImportStatusRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  }
derive instance newtypeGetCatalogImportStatusRequest :: Newtype GetCatalogImportStatusRequest _


newtype GetCatalogImportStatusResponse = GetCatalogImportStatusResponse 
  { "ImportStatus" :: NullOrUndefined (CatalogImportStatus)
  }
derive instance newtypeGetCatalogImportStatusResponse :: Newtype GetCatalogImportStatusResponse _


newtype GetClassifierRequest = GetClassifierRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeGetClassifierRequest :: Newtype GetClassifierRequest _


newtype GetClassifierResponse = GetClassifierResponse 
  { "Classifier" :: NullOrUndefined (Classifier)
  }
derive instance newtypeGetClassifierResponse :: Newtype GetClassifierResponse _


newtype GetClassifiersRequest = GetClassifiersRequest 
  { "MaxResults" :: NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetClassifiersRequest :: Newtype GetClassifiersRequest _


newtype GetClassifiersResponse = GetClassifiersResponse 
  { "Classifiers" :: NullOrUndefined (ClassifierList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetClassifiersResponse :: Newtype GetClassifiersResponse _


newtype GetConnectionRequest = GetConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }
derive instance newtypeGetConnectionRequest :: Newtype GetConnectionRequest _


newtype GetConnectionResponse = GetConnectionResponse 
  { "Connection" :: NullOrUndefined (Connection)
  }
derive instance newtypeGetConnectionResponse :: Newtype GetConnectionResponse _


-- | <p>Filters the connection definitions returned by the <code>GetConnections</code> API.</p>
newtype GetConnectionsFilter = GetConnectionsFilter 
  { "MatchCriteria" :: NullOrUndefined (MatchCriteria)
  , "ConnectionType" :: NullOrUndefined (ConnectionType)
  }
derive instance newtypeGetConnectionsFilter :: Newtype GetConnectionsFilter _


newtype GetConnectionsRequest = GetConnectionsRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Filter" :: NullOrUndefined (GetConnectionsFilter)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetConnectionsRequest :: Newtype GetConnectionsRequest _


newtype GetConnectionsResponse = GetConnectionsResponse 
  { "ConnectionList" :: NullOrUndefined (ConnectionList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetConnectionsResponse :: Newtype GetConnectionsResponse _


newtype GetCrawlerMetricsRequest = GetCrawlerMetricsRequest 
  { "CrawlerNameList" :: NullOrUndefined (CrawlerNameList)
  , "MaxResults" :: NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetCrawlerMetricsRequest :: Newtype GetCrawlerMetricsRequest _


newtype GetCrawlerMetricsResponse = GetCrawlerMetricsResponse 
  { "CrawlerMetricsList" :: NullOrUndefined (CrawlerMetricsList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetCrawlerMetricsResponse :: Newtype GetCrawlerMetricsResponse _


newtype GetCrawlerRequest = GetCrawlerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeGetCrawlerRequest :: Newtype GetCrawlerRequest _


newtype GetCrawlerResponse = GetCrawlerResponse 
  { "Crawler" :: NullOrUndefined (Crawler)
  }
derive instance newtypeGetCrawlerResponse :: Newtype GetCrawlerResponse _


newtype GetCrawlersRequest = GetCrawlersRequest 
  { "MaxResults" :: NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetCrawlersRequest :: Newtype GetCrawlersRequest _


newtype GetCrawlersResponse = GetCrawlersResponse 
  { "Crawlers" :: NullOrUndefined (CrawlerList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetCrawlersResponse :: Newtype GetCrawlersResponse _


newtype GetDatabaseRequest = GetDatabaseRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }
derive instance newtypeGetDatabaseRequest :: Newtype GetDatabaseRequest _


newtype GetDatabaseResponse = GetDatabaseResponse 
  { "Database" :: NullOrUndefined (Database)
  }
derive instance newtypeGetDatabaseResponse :: Newtype GetDatabaseResponse _


newtype GetDatabasesRequest = GetDatabasesRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetDatabasesRequest :: Newtype GetDatabasesRequest _


newtype GetDatabasesResponse = GetDatabasesResponse 
  { "DatabaseList" :: (DatabaseList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetDatabasesResponse :: Newtype GetDatabasesResponse _


newtype GetDataflowGraphRequest = GetDataflowGraphRequest 
  { "PythonScript" :: NullOrUndefined (PythonScript)
  }
derive instance newtypeGetDataflowGraphRequest :: Newtype GetDataflowGraphRequest _


newtype GetDataflowGraphResponse = GetDataflowGraphResponse 
  { "DagNodes" :: NullOrUndefined (DagNodes)
  , "DagEdges" :: NullOrUndefined (DagEdges)
  }
derive instance newtypeGetDataflowGraphResponse :: Newtype GetDataflowGraphResponse _


newtype GetDevEndpointRequest = GetDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  }
derive instance newtypeGetDevEndpointRequest :: Newtype GetDevEndpointRequest _


newtype GetDevEndpointResponse = GetDevEndpointResponse 
  { "DevEndpoint" :: NullOrUndefined (DevEndpoint)
  }
derive instance newtypeGetDevEndpointResponse :: Newtype GetDevEndpointResponse _


newtype GetDevEndpointsRequest = GetDevEndpointsRequest 
  { "MaxResults" :: NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined (GenericString)
  }
derive instance newtypeGetDevEndpointsRequest :: Newtype GetDevEndpointsRequest _


newtype GetDevEndpointsResponse = GetDevEndpointsResponse 
  { "DevEndpoints" :: NullOrUndefined (DevEndpointList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }
derive instance newtypeGetDevEndpointsResponse :: Newtype GetDevEndpointsResponse _


newtype GetJobRequest = GetJobRequest 
  { "JobName" :: (NameString)
  }
derive instance newtypeGetJobRequest :: Newtype GetJobRequest _


newtype GetJobResponse = GetJobResponse 
  { "Job" :: NullOrUndefined (Job)
  }
derive instance newtypeGetJobResponse :: Newtype GetJobResponse _


newtype GetJobRunRequest = GetJobRunRequest 
  { "JobName" :: (NameString)
  , "RunId" :: (IdString)
  , "PredecessorsIncluded" :: NullOrUndefined (BooleanValue)
  }
derive instance newtypeGetJobRunRequest :: Newtype GetJobRunRequest _


newtype GetJobRunResponse = GetJobRunResponse 
  { "JobRun" :: NullOrUndefined (JobRun)
  }
derive instance newtypeGetJobRunResponse :: Newtype GetJobRunResponse _


newtype GetJobRunsRequest = GetJobRunsRequest 
  { "JobName" :: (NameString)
  , "NextToken" :: NullOrUndefined (GenericString)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetJobRunsRequest :: Newtype GetJobRunsRequest _


newtype GetJobRunsResponse = GetJobRunsResponse 
  { "JobRuns" :: NullOrUndefined (JobRunList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }
derive instance newtypeGetJobRunsResponse :: Newtype GetJobRunsResponse _


newtype GetJobsRequest = GetJobsRequest 
  { "NextToken" :: NullOrUndefined (GenericString)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetJobsRequest :: Newtype GetJobsRequest _


newtype GetJobsResponse = GetJobsResponse 
  { "Jobs" :: NullOrUndefined (JobList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }
derive instance newtypeGetJobsResponse :: Newtype GetJobsResponse _


newtype GetMappingRequest = GetMappingRequest 
  { "Source" :: (CatalogEntry)
  , "Sinks" :: NullOrUndefined (CatalogEntries)
  , "Location" :: NullOrUndefined (Location)
  }
derive instance newtypeGetMappingRequest :: Newtype GetMappingRequest _


newtype GetMappingResponse = GetMappingResponse 
  { "Mapping" :: (MappingList)
  }
derive instance newtypeGetMappingResponse :: Newtype GetMappingResponse _


newtype GetPartitionRequest = GetPartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValues" :: (ValueStringList)
  }
derive instance newtypeGetPartitionRequest :: Newtype GetPartitionRequest _


newtype GetPartitionResponse = GetPartitionResponse 
  { "Partition" :: NullOrUndefined (Partition)
  }
derive instance newtypeGetPartitionResponse :: Newtype GetPartitionResponse _


newtype GetPartitionsRequest = GetPartitionsRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "Expression" :: NullOrUndefined (PredicateString)
  , "NextToken" :: NullOrUndefined (Token)
  , "Segment" :: NullOrUndefined (Segment)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetPartitionsRequest :: Newtype GetPartitionsRequest _


newtype GetPartitionsResponse = GetPartitionsResponse 
  { "Partitions" :: NullOrUndefined (PartitionList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetPartitionsResponse :: Newtype GetPartitionsResponse _


newtype GetPlanRequest = GetPlanRequest 
  { "Mapping" :: (MappingList)
  , "Source" :: (CatalogEntry)
  , "Sinks" :: NullOrUndefined (CatalogEntries)
  , "Location" :: NullOrUndefined (Location)
  , "Language" :: NullOrUndefined (Language)
  }
derive instance newtypeGetPlanRequest :: Newtype GetPlanRequest _


newtype GetPlanResponse = GetPlanResponse 
  { "PythonScript" :: NullOrUndefined (PythonScript)
  , "ScalaCode" :: NullOrUndefined (ScalaCode)
  }
derive instance newtypeGetPlanResponse :: Newtype GetPlanResponse _


newtype GetTableRequest = GetTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Name" :: (NameString)
  }
derive instance newtypeGetTableRequest :: Newtype GetTableRequest _


newtype GetTableResponse = GetTableResponse 
  { "Table" :: NullOrUndefined (Table)
  }
derive instance newtypeGetTableResponse :: Newtype GetTableResponse _


newtype GetTableVersionRequest = GetTableVersionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionId" :: NullOrUndefined (VersionString)
  }
derive instance newtypeGetTableVersionRequest :: Newtype GetTableVersionRequest _


newtype GetTableVersionResponse = GetTableVersionResponse 
  { "TableVersion" :: NullOrUndefined (TableVersion)
  }
derive instance newtypeGetTableVersionResponse :: Newtype GetTableVersionResponse _


newtype GetTableVersionsList = GetTableVersionsList (Array TableVersion)
derive instance newtypeGetTableVersionsList :: Newtype GetTableVersionsList _


newtype GetTableVersionsRequest = GetTableVersionsRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetTableVersionsRequest :: Newtype GetTableVersionsRequest _


newtype GetTableVersionsResponse = GetTableVersionsResponse 
  { "TableVersions" :: NullOrUndefined (GetTableVersionsList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetTableVersionsResponse :: Newtype GetTableVersionsResponse _


newtype GetTablesRequest = GetTablesRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Expression" :: NullOrUndefined (FilterString)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetTablesRequest :: Newtype GetTablesRequest _


newtype GetTablesResponse = GetTablesResponse 
  { "TableList" :: NullOrUndefined (TableList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetTablesResponse :: Newtype GetTablesResponse _


newtype GetTriggerRequest = GetTriggerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeGetTriggerRequest :: Newtype GetTriggerRequest _


newtype GetTriggerResponse = GetTriggerResponse 
  { "Trigger" :: NullOrUndefined (Trigger)
  }
derive instance newtypeGetTriggerResponse :: Newtype GetTriggerResponse _


newtype GetTriggersRequest = GetTriggersRequest 
  { "NextToken" :: NullOrUndefined (GenericString)
  , "DependentJobName" :: NullOrUndefined (NameString)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetTriggersRequest :: Newtype GetTriggersRequest _


newtype GetTriggersResponse = GetTriggersResponse 
  { "Triggers" :: NullOrUndefined (TriggerList)
  , "NextToken" :: NullOrUndefined (GenericString)
  }
derive instance newtypeGetTriggersResponse :: Newtype GetTriggersResponse _


newtype GetUserDefinedFunctionRequest = GetUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  }
derive instance newtypeGetUserDefinedFunctionRequest :: Newtype GetUserDefinedFunctionRequest _


newtype GetUserDefinedFunctionResponse = GetUserDefinedFunctionResponse 
  { "UserDefinedFunction" :: NullOrUndefined (UserDefinedFunction)
  }
derive instance newtypeGetUserDefinedFunctionResponse :: Newtype GetUserDefinedFunctionResponse _


newtype GetUserDefinedFunctionsRequest = GetUserDefinedFunctionsRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Pattern" :: (NameString)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (PageSize)
  }
derive instance newtypeGetUserDefinedFunctionsRequest :: Newtype GetUserDefinedFunctionsRequest _


newtype GetUserDefinedFunctionsResponse = GetUserDefinedFunctionsResponse 
  { "UserDefinedFunctions" :: NullOrUndefined (UserDefinedFunctionList)
  , "NextToken" :: NullOrUndefined (Token)
  }
derive instance newtypeGetUserDefinedFunctionsResponse :: Newtype GetUserDefinedFunctionsResponse _


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
derive instance newtypeGrokClassifier :: Newtype GrokClassifier _


newtype GrokPattern = GrokPattern String
derive instance newtypeGrokPattern :: Newtype GrokPattern _


newtype IdString = IdString String
derive instance newtypeIdString :: Newtype IdString _


-- | <p>The same unique identifier was associated with two different records.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeIdempotentParameterMismatchException :: Newtype IdempotentParameterMismatchException _


newtype ImportCatalogToGlueRequest = ImportCatalogToGlueRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  }
derive instance newtypeImportCatalogToGlueRequest :: Newtype ImportCatalogToGlueRequest _


newtype ImportCatalogToGlueResponse = ImportCatalogToGlueResponse 
  { 
  }
derive instance newtypeImportCatalogToGlueResponse :: Newtype ImportCatalogToGlueResponse _


newtype IntegerFlag = IntegerFlag Int
derive instance newtypeIntegerFlag :: Newtype IntegerFlag _


newtype IntegerValue = IntegerValue Int
derive instance newtypeIntegerValue :: Newtype IntegerValue _


-- | <p>An internal service error occurred.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeInternalServiceException :: Newtype InternalServiceException _


-- | <p>The input provided was not valid.</p>
newtype InvalidInputException = InvalidInputException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _


-- | <p>Specifies a JDBC data store to crawl.</p>
newtype JdbcTarget = JdbcTarget 
  { "ConnectionName" :: NullOrUndefined (ConnectionName)
  , "Path" :: NullOrUndefined (Path)
  , "Exclusions" :: NullOrUndefined (PathList)
  }
derive instance newtypeJdbcTarget :: Newtype JdbcTarget _


newtype JdbcTargetList = JdbcTargetList (Array JdbcTarget)
derive instance newtypeJdbcTargetList :: Newtype JdbcTargetList _


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
derive instance newtypeJob :: Newtype Job _


-- | <p>Defines a point which a job can resume processing.</p>
newtype JobBookmarkEntry = JobBookmarkEntry 
  { "JobName" :: NullOrUndefined (JobName)
  , "Version" :: NullOrUndefined (IntegerValue)
  , "Run" :: NullOrUndefined (IntegerValue)
  , "Attempt" :: NullOrUndefined (IntegerValue)
  , "JobBookmark" :: NullOrUndefined (JsonValue)
  }
derive instance newtypeJobBookmarkEntry :: Newtype JobBookmarkEntry _


-- | <p>Specifies code that executes a job.</p>
newtype JobCommand = JobCommand 
  { "Name" :: NullOrUndefined (GenericString)
  , "ScriptLocation" :: NullOrUndefined (ScriptLocationString)
  }
derive instance newtypeJobCommand :: Newtype JobCommand _


newtype JobList = JobList (Array Job)
derive instance newtypeJobList :: Newtype JobList _


newtype JobName = JobName String
derive instance newtypeJobName :: Newtype JobName _


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
derive instance newtypeJobRun :: Newtype JobRun _


newtype JobRunList = JobRunList (Array JobRun)
derive instance newtypeJobRunList :: Newtype JobRunList _


newtype JobRunState = JobRunState String
derive instance newtypeJobRunState :: Newtype JobRunState _


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
derive instance newtypeJobUpdate :: Newtype JobUpdate _


-- | <p>A classifier for <code>JSON</code> content.</p>
newtype JsonClassifier = JsonClassifier 
  { "Name" :: (NameString)
  , "CreationTime" :: NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "Version" :: NullOrUndefined (VersionId)
  , "JsonPath" :: (JsonPath)
  }
derive instance newtypeJsonClassifier :: Newtype JsonClassifier _


newtype JsonPath = JsonPath String
derive instance newtypeJsonPath :: Newtype JsonPath _


newtype JsonValue = JsonValue String
derive instance newtypeJsonValue :: Newtype JsonValue _


newtype KeyString = KeyString String
derive instance newtypeKeyString :: Newtype KeyString _


newtype Language = Language String
derive instance newtypeLanguage :: Newtype Language _


-- | <p>Status and error information about the most recent crawl.</p>
newtype LastCrawlInfo = LastCrawlInfo 
  { "Status" :: NullOrUndefined (LastCrawlStatus)
  , "ErrorMessage" :: NullOrUndefined (DescriptionString)
  , "LogGroup" :: NullOrUndefined (LogGroup)
  , "LogStream" :: NullOrUndefined (LogStream)
  , "MessagePrefix" :: NullOrUndefined (MessagePrefix)
  , "StartTime" :: NullOrUndefined (Number)
  }
derive instance newtypeLastCrawlInfo :: Newtype LastCrawlInfo _


newtype LastCrawlStatus = LastCrawlStatus String
derive instance newtypeLastCrawlStatus :: Newtype LastCrawlStatus _


-- | <p>The location of resources.</p>
newtype Location = Location 
  { "Jdbc" :: NullOrUndefined (CodeGenNodeArgs)
  , "S3" :: NullOrUndefined (CodeGenNodeArgs)
  }
derive instance newtypeLocation :: Newtype Location _


newtype LocationMap = LocationMap (Map ColumnValuesString ColumnValuesString)
derive instance newtypeLocationMap :: Newtype LocationMap _


newtype LocationString = LocationString String
derive instance newtypeLocationString :: Newtype LocationString _


newtype LogGroup = LogGroup String
derive instance newtypeLogGroup :: Newtype LogGroup _


newtype LogStream = LogStream String
derive instance newtypeLogStream :: Newtype LogStream _


newtype Logical = Logical String
derive instance newtypeLogical :: Newtype Logical _


newtype LogicalOperator = LogicalOperator String
derive instance newtypeLogicalOperator :: Newtype LogicalOperator _


-- | <p>Defines a mapping.</p>
newtype MappingEntry = MappingEntry 
  { "SourceTable" :: NullOrUndefined (TableName)
  , "SourcePath" :: NullOrUndefined (SchemaPathString)
  , "SourceType" :: NullOrUndefined (FieldType)
  , "TargetTable" :: NullOrUndefined (TableName)
  , "TargetPath" :: NullOrUndefined (SchemaPathString)
  , "TargetType" :: NullOrUndefined (FieldType)
  }
derive instance newtypeMappingEntry :: Newtype MappingEntry _


newtype MappingList = MappingList (Array MappingEntry)
derive instance newtypeMappingList :: Newtype MappingList _


newtype MatchCriteria = MatchCriteria (Array NameString)
derive instance newtypeMatchCriteria :: Newtype MatchCriteria _


newtype MaxConcurrentRuns = MaxConcurrentRuns Int
derive instance newtypeMaxConcurrentRuns :: Newtype MaxConcurrentRuns _


newtype MaxRetries = MaxRetries Int
derive instance newtypeMaxRetries :: Newtype MaxRetries _


newtype MessagePrefix = MessagePrefix String
derive instance newtypeMessagePrefix :: Newtype MessagePrefix _


newtype MessageString = MessageString String
derive instance newtypeMessageString :: Newtype MessageString _


newtype MillisecondsCount = MillisecondsCount Number
derive instance newtypeMillisecondsCount :: Newtype MillisecondsCount _


newtype NameString = NameString String
derive instance newtypeNameString :: Newtype NameString _


newtype NameStringList = NameStringList (Array NameString)
derive instance newtypeNameStringList :: Newtype NameStringList _


-- | <p>There is no applicable schedule.</p>
newtype NoScheduleException = NoScheduleException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeNoScheduleException :: Newtype NoScheduleException _


newtype NonNegativeDouble = NonNegativeDouble Number
derive instance newtypeNonNegativeDouble :: Newtype NonNegativeDouble _


newtype NonNegativeInteger = NonNegativeInteger Int
derive instance newtypeNonNegativeInteger :: Newtype NonNegativeInteger _


-- | <p>The operation timed out.</p>
newtype OperationTimeoutException = OperationTimeoutException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeOperationTimeoutException :: Newtype OperationTimeoutException _


-- | <p>Specifies the sort order of a sorted column.</p>
newtype Order = Order 
  { "Column" :: (NameString)
  , "SortOrder" :: (IntegerFlag)
  }
derive instance newtypeOrder :: Newtype Order _


newtype OrderList = OrderList (Array Order)
derive instance newtypeOrderList :: Newtype OrderList _


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _


newtype ParametersMap = ParametersMap (Map KeyString ParametersMapValue)
derive instance newtypeParametersMap :: Newtype ParametersMap _


newtype ParametersMapValue = ParametersMapValue String
derive instance newtypeParametersMapValue :: Newtype ParametersMapValue _


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
derive instance newtypePartition :: Newtype Partition _


-- | <p>Contains information about a partition error.</p>
newtype PartitionError = PartitionError 
  { "PartitionValues" :: NullOrUndefined (ValueStringList)
  , "ErrorDetail" :: NullOrUndefined (ErrorDetail)
  }
derive instance newtypePartitionError :: Newtype PartitionError _


newtype PartitionErrors = PartitionErrors (Array PartitionError)
derive instance newtypePartitionErrors :: Newtype PartitionErrors _


-- | <p>The structure used to create and update a partion.</p>
newtype PartitionInput = PartitionInput 
  { "Values" :: NullOrUndefined (ValueStringList)
  , "LastAccessTime" :: NullOrUndefined (Number)
  , "StorageDescriptor" :: NullOrUndefined (StorageDescriptor)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  , "LastAnalyzedTime" :: NullOrUndefined (Number)
  }
derive instance newtypePartitionInput :: Newtype PartitionInput _


newtype PartitionInputList = PartitionInputList (Array PartitionInput)
derive instance newtypePartitionInputList :: Newtype PartitionInputList _


newtype PartitionList = PartitionList (Array Partition)
derive instance newtypePartitionList :: Newtype PartitionList _


-- | <p>Contains a list of values defining partitions.</p>
newtype PartitionValueList = PartitionValueList 
  { "Values" :: (ValueStringList)
  }
derive instance newtypePartitionValueList :: Newtype PartitionValueList _


newtype Path = Path String
derive instance newtypePath :: Newtype Path _


newtype PathList = PathList (Array Path)
derive instance newtypePathList :: Newtype PathList _


-- | <p>Specifies the physical requirements for a connection.</p>
newtype PhysicalConnectionRequirements = PhysicalConnectionRequirements 
  { "SubnetId" :: NullOrUndefined (NameString)
  , "SecurityGroupIdList" :: NullOrUndefined (SecurityGroupIdList)
  , "AvailabilityZone" :: NullOrUndefined (NameString)
  }
derive instance newtypePhysicalConnectionRequirements :: Newtype PhysicalConnectionRequirements _


-- | <p>A job run that was used in the predicate of a conditional trigger that triggered this job run.</p>
newtype Predecessor = Predecessor 
  { "JobName" :: NullOrUndefined (NameString)
  , "RunId" :: NullOrUndefined (IdString)
  }
derive instance newtypePredecessor :: Newtype Predecessor _


newtype PredecessorList = PredecessorList (Array Predecessor)
derive instance newtypePredecessorList :: Newtype PredecessorList _


-- | <p>Defines the predicate of the trigger, which determines when it fires.</p>
newtype Predicate = Predicate 
  { "Logical" :: NullOrUndefined (Logical)
  , "Conditions" :: NullOrUndefined (ConditionList)
  }
derive instance newtypePredicate :: Newtype Predicate _


newtype PredicateString = PredicateString String
derive instance newtypePredicateString :: Newtype PredicateString _


newtype PrincipalType = PrincipalType String
derive instance newtypePrincipalType :: Newtype PrincipalType _


newtype PythonScript = PythonScript String
derive instance newtypePythonScript :: Newtype PythonScript _


newtype ResetJobBookmarkRequest = ResetJobBookmarkRequest 
  { "JobName" :: (JobName)
  }
derive instance newtypeResetJobBookmarkRequest :: Newtype ResetJobBookmarkRequest _


newtype ResetJobBookmarkResponse = ResetJobBookmarkResponse 
  { "JobBookmarkEntry" :: NullOrUndefined (JobBookmarkEntry)
  }
derive instance newtypeResetJobBookmarkResponse :: Newtype ResetJobBookmarkResponse _


-- | <p>A resource numerical limit was exceeded.</p>
newtype ResourceNumberLimitExceededException = ResourceNumberLimitExceededException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeResourceNumberLimitExceededException :: Newtype ResourceNumberLimitExceededException _


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


-- | <p>URIs for function resources.</p>
newtype ResourceUri = ResourceUri 
  { "ResourceType" :: NullOrUndefined (ResourceType)
  , "Uri" :: NullOrUndefined (URI)
  }
derive instance newtypeResourceUri :: Newtype ResourceUri _


newtype ResourceUriList = ResourceUriList (Array ResourceUri)
derive instance newtypeResourceUriList :: Newtype ResourceUriList _


newtype Role = Role String
derive instance newtypeRole :: Newtype Role _


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _


newtype RoleString = RoleString String
derive instance newtypeRoleString :: Newtype RoleString _


newtype RowTag = RowTag String
derive instance newtypeRowTag :: Newtype RowTag _


-- | <p>Specifies a data store in Amazon S3.</p>
newtype S3Target = S3Target 
  { "Path" :: NullOrUndefined (Path)
  , "Exclusions" :: NullOrUndefined (PathList)
  }
derive instance newtypeS3Target :: Newtype S3Target _


newtype S3TargetList = S3TargetList (Array S3Target)
derive instance newtypeS3TargetList :: Newtype S3TargetList _


newtype ScalaCode = ScalaCode String
derive instance newtypeScalaCode :: Newtype ScalaCode _


-- | <p>A scheduling object using a <code>cron</code> statement to schedule an event.</p>
newtype Schedule = Schedule 
  { "ScheduleExpression" :: NullOrUndefined (CronExpression)
  , "State" :: NullOrUndefined (ScheduleState)
  }
derive instance newtypeSchedule :: Newtype Schedule _


newtype ScheduleState = ScheduleState String
derive instance newtypeScheduleState :: Newtype ScheduleState _


-- | <p>The specified scheduler is not running.</p>
newtype SchedulerNotRunningException = SchedulerNotRunningException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeSchedulerNotRunningException :: Newtype SchedulerNotRunningException _


-- | <p>The specified scheduler is already running.</p>
newtype SchedulerRunningException = SchedulerRunningException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeSchedulerRunningException :: Newtype SchedulerRunningException _


-- | <p>The specified scheduler is transitioning.</p>
newtype SchedulerTransitioningException = SchedulerTransitioningException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeSchedulerTransitioningException :: Newtype SchedulerTransitioningException _


-- | <p>Crawler policy for update and deletion behavior.</p>
newtype SchemaChangePolicy = SchemaChangePolicy 
  { "UpdateBehavior" :: NullOrUndefined (UpdateBehavior)
  , "DeleteBehavior" :: NullOrUndefined (DeleteBehavior)
  }
derive instance newtypeSchemaChangePolicy :: Newtype SchemaChangePolicy _


newtype SchemaPathString = SchemaPathString String
derive instance newtypeSchemaPathString :: Newtype SchemaPathString _


newtype ScriptLocationString = ScriptLocationString String
derive instance newtypeScriptLocationString :: Newtype ScriptLocationString _


newtype SecurityGroupIdList = SecurityGroupIdList (Array NameString)
derive instance newtypeSecurityGroupIdList :: Newtype SecurityGroupIdList _


-- | <p>Defines a non-overlapping region of a table's partitions, allowing multiple requests to be executed in parallel.</p>
newtype Segment = Segment 
  { "SegmentNumber" :: (NonNegativeInteger)
  , "TotalSegments" :: (TotalSegmentsInteger)
  }
derive instance newtypeSegment :: Newtype Segment _


-- | <p>Information about a serialization/deserialization program (SerDe) which serves as an extractor and loader.</p>
newtype SerDeInfo = SerDeInfo 
  { "Name" :: NullOrUndefined (NameString)
  , "SerializationLibrary" :: NullOrUndefined (NameString)
  , "Parameters" :: NullOrUndefined (ParametersMap)
  }
derive instance newtypeSerDeInfo :: Newtype SerDeInfo _


-- | <p>Specifies skewed values in a table. Skewed are ones that occur with very high frequency.</p>
newtype SkewedInfo = SkewedInfo 
  { "SkewedColumnNames" :: NullOrUndefined (NameStringList)
  , "SkewedColumnValues" :: NullOrUndefined (ColumnValueStringList)
  , "SkewedColumnValueLocationMaps" :: NullOrUndefined (LocationMap)
  }
derive instance newtypeSkewedInfo :: Newtype SkewedInfo _


newtype StartCrawlerRequest = StartCrawlerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeStartCrawlerRequest :: Newtype StartCrawlerRequest _


newtype StartCrawlerResponse = StartCrawlerResponse 
  { 
  }
derive instance newtypeStartCrawlerResponse :: Newtype StartCrawlerResponse _


newtype StartCrawlerScheduleRequest = StartCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  }
derive instance newtypeStartCrawlerScheduleRequest :: Newtype StartCrawlerScheduleRequest _


newtype StartCrawlerScheduleResponse = StartCrawlerScheduleResponse 
  { 
  }
derive instance newtypeStartCrawlerScheduleResponse :: Newtype StartCrawlerScheduleResponse _


newtype StartJobRunRequest = StartJobRunRequest 
  { "JobName" :: (NameString)
  , "JobRunId" :: NullOrUndefined (IdString)
  , "Arguments" :: NullOrUndefined (GenericMap)
  , "AllocatedCapacity" :: NullOrUndefined (IntegerValue)
  }
derive instance newtypeStartJobRunRequest :: Newtype StartJobRunRequest _


newtype StartJobRunResponse = StartJobRunResponse 
  { "JobRunId" :: NullOrUndefined (IdString)
  }
derive instance newtypeStartJobRunResponse :: Newtype StartJobRunResponse _


newtype StartTriggerRequest = StartTriggerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeStartTriggerRequest :: Newtype StartTriggerRequest _


newtype StartTriggerResponse = StartTriggerResponse 
  { "Name" :: NullOrUndefined (NameString)
  }
derive instance newtypeStartTriggerResponse :: Newtype StartTriggerResponse _


newtype StopCrawlerRequest = StopCrawlerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeStopCrawlerRequest :: Newtype StopCrawlerRequest _


newtype StopCrawlerResponse = StopCrawlerResponse 
  { 
  }
derive instance newtypeStopCrawlerResponse :: Newtype StopCrawlerResponse _


newtype StopCrawlerScheduleRequest = StopCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  }
derive instance newtypeStopCrawlerScheduleRequest :: Newtype StopCrawlerScheduleRequest _


newtype StopCrawlerScheduleResponse = StopCrawlerScheduleResponse 
  { 
  }
derive instance newtypeStopCrawlerScheduleResponse :: Newtype StopCrawlerScheduleResponse _


newtype StopTriggerRequest = StopTriggerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeStopTriggerRequest :: Newtype StopTriggerRequest _


newtype StopTriggerResponse = StopTriggerResponse 
  { "Name" :: NullOrUndefined (NameString)
  }
derive instance newtypeStopTriggerResponse :: Newtype StopTriggerResponse _


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
derive instance newtypeStorageDescriptor :: Newtype StorageDescriptor _


newtype StringList = StringList (Array GenericString)
derive instance newtypeStringList :: Newtype StringList _


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
derive instance newtypeTable :: Newtype Table _


-- | <p>An error record for table operations.</p>
newtype TableError = TableError 
  { "TableName" :: NullOrUndefined (NameString)
  , "ErrorDetail" :: NullOrUndefined (ErrorDetail)
  }
derive instance newtypeTableError :: Newtype TableError _


newtype TableErrors = TableErrors (Array TableError)
derive instance newtypeTableErrors :: Newtype TableErrors _


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
derive instance newtypeTableInput :: Newtype TableInput _


newtype TableList = TableList (Array Table)
derive instance newtypeTableList :: Newtype TableList _


newtype TableName = TableName String
derive instance newtypeTableName :: Newtype TableName _


newtype TablePrefix = TablePrefix String
derive instance newtypeTablePrefix :: Newtype TablePrefix _


newtype TableTypeString = TableTypeString String
derive instance newtypeTableTypeString :: Newtype TableTypeString _


-- | <p>Specifies a version of a table.</p>
newtype TableVersion = TableVersion 
  { "Table" :: NullOrUndefined (Table)
  , "VersionId" :: NullOrUndefined (VersionString)
  }
derive instance newtypeTableVersion :: Newtype TableVersion _


-- | <p>An error record for table-version operations.</p>
newtype TableVersionError = TableVersionError 
  { "TableName" :: NullOrUndefined (NameString)
  , "VersionId" :: NullOrUndefined (VersionString)
  , "ErrorDetail" :: NullOrUndefined (ErrorDetail)
  }
derive instance newtypeTableVersionError :: Newtype TableVersionError _


newtype TableVersionErrors = TableVersionErrors (Array TableVersionError)
derive instance newtypeTableVersionErrors :: Newtype TableVersionErrors _


newtype TimestampValue = TimestampValue Number
derive instance newtypeTimestampValue :: Newtype TimestampValue _


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _


newtype TotalSegmentsInteger = TotalSegmentsInteger Int
derive instance newtypeTotalSegmentsInteger :: Newtype TotalSegmentsInteger _


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
derive instance newtypeTrigger :: Newtype Trigger _


newtype TriggerList = TriggerList (Array Trigger)
derive instance newtypeTriggerList :: Newtype TriggerList _


newtype TriggerState = TriggerState String
derive instance newtypeTriggerState :: Newtype TriggerState _


newtype TriggerType = TriggerType String
derive instance newtypeTriggerType :: Newtype TriggerType _


-- | <p>A structure used to provide information used to update a trigger. This object will update the the previous trigger definition by overwriting it completely.</p>
newtype TriggerUpdate = TriggerUpdate 
  { "Name" :: NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Schedule" :: NullOrUndefined (GenericString)
  , "Actions" :: NullOrUndefined (ActionList)
  , "Predicate" :: NullOrUndefined (Predicate)
  }
derive instance newtypeTriggerUpdate :: Newtype TriggerUpdate _


newtype URI = URI String
derive instance newtypeURI :: Newtype URI _


newtype UpdateBehavior = UpdateBehavior String
derive instance newtypeUpdateBehavior :: Newtype UpdateBehavior _


newtype UpdateClassifierRequest = UpdateClassifierRequest 
  { "GrokClassifier" :: NullOrUndefined (UpdateGrokClassifierRequest)
  , "XMLClassifier" :: NullOrUndefined (UpdateXMLClassifierRequest)
  , "JsonClassifier" :: NullOrUndefined (UpdateJsonClassifierRequest)
  }
derive instance newtypeUpdateClassifierRequest :: Newtype UpdateClassifierRequest _


newtype UpdateClassifierResponse = UpdateClassifierResponse 
  { 
  }
derive instance newtypeUpdateClassifierResponse :: Newtype UpdateClassifierResponse _


newtype UpdateConnectionRequest = UpdateConnectionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  , "ConnectionInput" :: (ConnectionInput)
  }
derive instance newtypeUpdateConnectionRequest :: Newtype UpdateConnectionRequest _


newtype UpdateConnectionResponse = UpdateConnectionResponse 
  { 
  }
derive instance newtypeUpdateConnectionResponse :: Newtype UpdateConnectionResponse _


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
derive instance newtypeUpdateCrawlerRequest :: Newtype UpdateCrawlerRequest _


newtype UpdateCrawlerResponse = UpdateCrawlerResponse 
  { 
  }
derive instance newtypeUpdateCrawlerResponse :: Newtype UpdateCrawlerResponse _


newtype UpdateCrawlerScheduleRequest = UpdateCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  , "Schedule" :: NullOrUndefined (CronExpression)
  }
derive instance newtypeUpdateCrawlerScheduleRequest :: Newtype UpdateCrawlerScheduleRequest _


newtype UpdateCrawlerScheduleResponse = UpdateCrawlerScheduleResponse 
  { 
  }
derive instance newtypeUpdateCrawlerScheduleResponse :: Newtype UpdateCrawlerScheduleResponse _


newtype UpdateDatabaseRequest = UpdateDatabaseRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  , "DatabaseInput" :: (DatabaseInput)
  }
derive instance newtypeUpdateDatabaseRequest :: Newtype UpdateDatabaseRequest _


newtype UpdateDatabaseResponse = UpdateDatabaseResponse 
  { 
  }
derive instance newtypeUpdateDatabaseResponse :: Newtype UpdateDatabaseResponse _


newtype UpdateDevEndpointRequest = UpdateDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  , "PublicKey" :: NullOrUndefined (GenericString)
  , "CustomLibraries" :: NullOrUndefined (DevEndpointCustomLibraries)
  , "UpdateEtlLibraries" :: NullOrUndefined (BooleanValue)
  }
derive instance newtypeUpdateDevEndpointRequest :: Newtype UpdateDevEndpointRequest _


newtype UpdateDevEndpointResponse = UpdateDevEndpointResponse 
  { 
  }
derive instance newtypeUpdateDevEndpointResponse :: Newtype UpdateDevEndpointResponse _


-- | <p>Specifies a grok classifier to update when passed to <code>UpdateClassifier</code>.</p>
newtype UpdateGrokClassifierRequest = UpdateGrokClassifierRequest 
  { "Name" :: (NameString)
  , "Classification" :: NullOrUndefined (Classification)
  , "GrokPattern" :: NullOrUndefined (GrokPattern)
  , "CustomPatterns" :: NullOrUndefined (CustomPatterns)
  }
derive instance newtypeUpdateGrokClassifierRequest :: Newtype UpdateGrokClassifierRequest _


newtype UpdateJobRequest = UpdateJobRequest 
  { "JobName" :: (NameString)
  , "JobUpdate" :: (JobUpdate)
  }
derive instance newtypeUpdateJobRequest :: Newtype UpdateJobRequest _


newtype UpdateJobResponse = UpdateJobResponse 
  { "JobName" :: NullOrUndefined (NameString)
  }
derive instance newtypeUpdateJobResponse :: Newtype UpdateJobResponse _


-- | <p>Specifies a JSON classifier to be updated.</p>
newtype UpdateJsonClassifierRequest = UpdateJsonClassifierRequest 
  { "Name" :: (NameString)
  , "JsonPath" :: NullOrUndefined (JsonPath)
  }
derive instance newtypeUpdateJsonClassifierRequest :: Newtype UpdateJsonClassifierRequest _


newtype UpdatePartitionRequest = UpdatePartitionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValueList" :: (BoundedPartitionValueList)
  , "PartitionInput" :: (PartitionInput)
  }
derive instance newtypeUpdatePartitionRequest :: Newtype UpdatePartitionRequest _


newtype UpdatePartitionResponse = UpdatePartitionResponse 
  { 
  }
derive instance newtypeUpdatePartitionResponse :: Newtype UpdatePartitionResponse _


newtype UpdateTableRequest = UpdateTableRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableInput" :: (TableInput)
  , "SkipArchive" :: NullOrUndefined (BooleanNullable)
  }
derive instance newtypeUpdateTableRequest :: Newtype UpdateTableRequest _


newtype UpdateTableResponse = UpdateTableResponse 
  { 
  }
derive instance newtypeUpdateTableResponse :: Newtype UpdateTableResponse _


newtype UpdateTriggerRequest = UpdateTriggerRequest 
  { "Name" :: (NameString)
  , "TriggerUpdate" :: (TriggerUpdate)
  }
derive instance newtypeUpdateTriggerRequest :: Newtype UpdateTriggerRequest _


newtype UpdateTriggerResponse = UpdateTriggerResponse 
  { "Trigger" :: NullOrUndefined (Trigger)
  }
derive instance newtypeUpdateTriggerResponse :: Newtype UpdateTriggerResponse _


newtype UpdateUserDefinedFunctionRequest = UpdateUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  , "FunctionInput" :: (UserDefinedFunctionInput)
  }
derive instance newtypeUpdateUserDefinedFunctionRequest :: Newtype UpdateUserDefinedFunctionRequest _


newtype UpdateUserDefinedFunctionResponse = UpdateUserDefinedFunctionResponse 
  { 
  }
derive instance newtypeUpdateUserDefinedFunctionResponse :: Newtype UpdateUserDefinedFunctionResponse _


-- | <p>Specifies an XML classifier to be updated.</p>
newtype UpdateXMLClassifierRequest = UpdateXMLClassifierRequest 
  { "Name" :: (NameString)
  , "Classification" :: NullOrUndefined (Classification)
  , "RowTag" :: NullOrUndefined (RowTag)
  }
derive instance newtypeUpdateXMLClassifierRequest :: Newtype UpdateXMLClassifierRequest _


newtype UriString = UriString String
derive instance newtypeUriString :: Newtype UriString _


-- | <p>Represents the equivalent of a Hive user-defined function (<code>UDF</code>) definition.</p>
newtype UserDefinedFunction = UserDefinedFunction 
  { "FunctionName" :: NullOrUndefined (NameString)
  , "ClassName" :: NullOrUndefined (NameString)
  , "OwnerName" :: NullOrUndefined (NameString)
  , "OwnerType" :: NullOrUndefined (PrincipalType)
  , "CreateTime" :: NullOrUndefined (Number)
  , "ResourceUris" :: NullOrUndefined (ResourceUriList)
  }
derive instance newtypeUserDefinedFunction :: Newtype UserDefinedFunction _


-- | <p>A structure used to create or updata a user-defined function.</p>
newtype UserDefinedFunctionInput = UserDefinedFunctionInput 
  { "FunctionName" :: NullOrUndefined (NameString)
  , "ClassName" :: NullOrUndefined (NameString)
  , "OwnerName" :: NullOrUndefined (NameString)
  , "OwnerType" :: NullOrUndefined (PrincipalType)
  , "ResourceUris" :: NullOrUndefined (ResourceUriList)
  }
derive instance newtypeUserDefinedFunctionInput :: Newtype UserDefinedFunctionInput _


newtype UserDefinedFunctionList = UserDefinedFunctionList (Array UserDefinedFunction)
derive instance newtypeUserDefinedFunctionList :: Newtype UserDefinedFunctionList _


-- | <p>A value could not be validated.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeValidationException :: Newtype ValidationException _


newtype ValueString = ValueString String
derive instance newtypeValueString :: Newtype ValueString _


newtype ValueStringList = ValueStringList (Array ValueString)
derive instance newtypeValueStringList :: Newtype ValueStringList _


newtype VersionId = VersionId Number
derive instance newtypeVersionId :: Newtype VersionId _


-- | <p>There was a version conflict.</p>
newtype VersionMismatchException = VersionMismatchException 
  { "Message" :: NullOrUndefined (MessageString)
  }
derive instance newtypeVersionMismatchException :: Newtype VersionMismatchException _


newtype VersionString = VersionString String
derive instance newtypeVersionString :: Newtype VersionString _


newtype ViewTextString = ViewTextString String
derive instance newtypeViewTextString :: Newtype ViewTextString _


-- | <p>A classifier for <code>XML</code> content.</p>
newtype XMLClassifier = XMLClassifier 
  { "Name" :: (NameString)
  , "Classification" :: (Classification)
  , "CreationTime" :: NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined (Number)
  , "Version" :: NullOrUndefined (VersionId)
  , "RowTag" :: NullOrUndefined (RowTag)
  }
derive instance newtypeXMLClassifier :: Newtype XMLClassifier _
