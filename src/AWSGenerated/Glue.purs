

-- | <fullname>AWS Glue</fullname> <p>Defines the public endpoint for the AWS Glue service.</p>
module AWS.Glue where

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

serviceName = "Glue" :: String


-- | <p>Creates one or more partitions in a batch operation.</p>
batchCreatePartition :: forall eff. BatchCreatePartitionRequest -> Aff (exception :: EXCEPTION | eff) BatchCreatePartitionResponse
batchCreatePartition = Request.request serviceName "batchCreatePartition" 


-- | <p>Deletes a list of connection definitions from the Data Catalog.</p>
batchDeleteConnection :: forall eff. BatchDeleteConnectionRequest -> Aff (exception :: EXCEPTION | eff) BatchDeleteConnectionResponse
batchDeleteConnection = Request.request serviceName "batchDeleteConnection" 


-- | <p>Deletes one or more partitions in a batch operation.</p>
batchDeletePartition :: forall eff. BatchDeletePartitionRequest -> Aff (exception :: EXCEPTION | eff) BatchDeletePartitionResponse
batchDeletePartition = Request.request serviceName "batchDeletePartition" 


-- | <p>Deletes multiple tables at once.</p>
batchDeleteTable :: forall eff. BatchDeleteTableRequest -> Aff (exception :: EXCEPTION | eff) BatchDeleteTableResponse
batchDeleteTable = Request.request serviceName "batchDeleteTable" 


-- | <p>Deletes a specified batch of versions of a table.</p>
batchDeleteTableVersion :: forall eff. BatchDeleteTableVersionRequest -> Aff (exception :: EXCEPTION | eff) BatchDeleteTableVersionResponse
batchDeleteTableVersion = Request.request serviceName "batchDeleteTableVersion" 


-- | <p>Retrieves partitions in a batch request.</p>
batchGetPartition :: forall eff. BatchGetPartitionRequest -> Aff (exception :: EXCEPTION | eff) BatchGetPartitionResponse
batchGetPartition = Request.request serviceName "batchGetPartition" 


-- | <p>Stops one or more job runs for a specified Job.</p>
batchStopJobRun :: forall eff. BatchStopJobRunRequest -> Aff (exception :: EXCEPTION | eff) BatchStopJobRunResponse
batchStopJobRun = Request.request serviceName "batchStopJobRun" 


-- | <p>Creates a classifier in the user's account. This may be a <code>GrokClassifier</code>, an <code>XMLClassifier</code>, or abbrev <code>JsonClassifier</code>, depending on which field of the request is present.</p>
createClassifier :: forall eff. CreateClassifierRequest -> Aff (exception :: EXCEPTION | eff) CreateClassifierResponse
createClassifier = Request.request serviceName "createClassifier" 


-- | <p>Creates a connection definition in the Data Catalog.</p>
createConnection :: forall eff. CreateConnectionRequest -> Aff (exception :: EXCEPTION | eff) CreateConnectionResponse
createConnection = Request.request serviceName "createConnection" 


-- | <p>Creates a new crawler with specified targets, role, configuration, and optional schedule. At least one crawl target must be specified, in either the <i>s3Targets</i> or the <i>jdbcTargets</i> field.</p>
createCrawler :: forall eff. CreateCrawlerRequest -> Aff (exception :: EXCEPTION | eff) CreateCrawlerResponse
createCrawler = Request.request serviceName "createCrawler" 


-- | <p>Creates a new database in a Data Catalog.</p>
createDatabase :: forall eff. CreateDatabaseRequest -> Aff (exception :: EXCEPTION | eff) CreateDatabaseResponse
createDatabase = Request.request serviceName "createDatabase" 


-- | <p>Creates a new DevEndpoint.</p>
createDevEndpoint :: forall eff. CreateDevEndpointRequest -> Aff (exception :: EXCEPTION | eff) CreateDevEndpointResponse
createDevEndpoint = Request.request serviceName "createDevEndpoint" 


-- | <p>Creates a new job.</p>
createJob :: forall eff. CreateJobRequest -> Aff (exception :: EXCEPTION | eff) CreateJobResponse
createJob = Request.request serviceName "createJob" 


-- | <p>Creates a new partition.</p>
createPartition :: forall eff. CreatePartitionRequest -> Aff (exception :: EXCEPTION | eff) CreatePartitionResponse
createPartition = Request.request serviceName "createPartition" 


-- | <p>Transforms a directed acyclic graph (DAG) into code.</p>
createScript :: forall eff. CreateScriptRequest -> Aff (exception :: EXCEPTION | eff) CreateScriptResponse
createScript = Request.request serviceName "createScript" 


-- | <p>Creates a new table definition in the Data Catalog.</p>
createTable :: forall eff. CreateTableRequest -> Aff (exception :: EXCEPTION | eff) CreateTableResponse
createTable = Request.request serviceName "createTable" 


-- | <p>Creates a new trigger.</p>
createTrigger :: forall eff. CreateTriggerRequest -> Aff (exception :: EXCEPTION | eff) CreateTriggerResponse
createTrigger = Request.request serviceName "createTrigger" 


-- | <p>Creates a new function definition in the Data Catalog.</p>
createUserDefinedFunction :: forall eff. CreateUserDefinedFunctionRequest -> Aff (exception :: EXCEPTION | eff) CreateUserDefinedFunctionResponse
createUserDefinedFunction = Request.request serviceName "createUserDefinedFunction" 


-- | <p>Removes a classifier from the Data Catalog.</p>
deleteClassifier :: forall eff. DeleteClassifierRequest -> Aff (exception :: EXCEPTION | eff) DeleteClassifierResponse
deleteClassifier = Request.request serviceName "deleteClassifier" 


-- | <p>Deletes a connection from the Data Catalog.</p>
deleteConnection :: forall eff. DeleteConnectionRequest -> Aff (exception :: EXCEPTION | eff) DeleteConnectionResponse
deleteConnection = Request.request serviceName "deleteConnection" 


-- | <p>Removes a specified crawler from the Data Catalog, unless the crawler state is <code>RUNNING</code>.</p>
deleteCrawler :: forall eff. DeleteCrawlerRequest -> Aff (exception :: EXCEPTION | eff) DeleteCrawlerResponse
deleteCrawler = Request.request serviceName "deleteCrawler" 


-- | <p>Removes a specified Database from a Data Catalog.</p>
deleteDatabase :: forall eff. DeleteDatabaseRequest -> Aff (exception :: EXCEPTION | eff) DeleteDatabaseResponse
deleteDatabase = Request.request serviceName "deleteDatabase" 


-- | <p>Deletes a specified DevEndpoint.</p>
deleteDevEndpoint :: forall eff. DeleteDevEndpointRequest -> Aff (exception :: EXCEPTION | eff) DeleteDevEndpointResponse
deleteDevEndpoint = Request.request serviceName "deleteDevEndpoint" 


-- | <p>Deletes a specified job. If the job is not found, no exception is thrown.</p>
deleteJob :: forall eff. DeleteJobRequest -> Aff (exception :: EXCEPTION | eff) DeleteJobResponse
deleteJob = Request.request serviceName "deleteJob" 


-- | <p>Deletes a specified partition.</p>
deletePartition :: forall eff. DeletePartitionRequest -> Aff (exception :: EXCEPTION | eff) DeletePartitionResponse
deletePartition = Request.request serviceName "deletePartition" 


-- | <p>Removes a table definition from the Data Catalog.</p>
deleteTable :: forall eff. DeleteTableRequest -> Aff (exception :: EXCEPTION | eff) DeleteTableResponse
deleteTable = Request.request serviceName "deleteTable" 


-- | <p>Deletes a specified version of a table.</p>
deleteTableVersion :: forall eff. DeleteTableVersionRequest -> Aff (exception :: EXCEPTION | eff) DeleteTableVersionResponse
deleteTableVersion = Request.request serviceName "deleteTableVersion" 


-- | <p>Deletes a specified trigger. If the trigger is not found, no exception is thrown.</p>
deleteTrigger :: forall eff. DeleteTriggerRequest -> Aff (exception :: EXCEPTION | eff) DeleteTriggerResponse
deleteTrigger = Request.request serviceName "deleteTrigger" 


-- | <p>Deletes an existing function definition from the Data Catalog.</p>
deleteUserDefinedFunction :: forall eff. DeleteUserDefinedFunctionRequest -> Aff (exception :: EXCEPTION | eff) DeleteUserDefinedFunctionResponse
deleteUserDefinedFunction = Request.request serviceName "deleteUserDefinedFunction" 


-- | <p>Retrieves the status of a migration operation.</p>
getCatalogImportStatus :: forall eff. GetCatalogImportStatusRequest -> Aff (exception :: EXCEPTION | eff) GetCatalogImportStatusResponse
getCatalogImportStatus = Request.request serviceName "getCatalogImportStatus" 


-- | <p>Retrieve a classifier by name.</p>
getClassifier :: forall eff. GetClassifierRequest -> Aff (exception :: EXCEPTION | eff) GetClassifierResponse
getClassifier = Request.request serviceName "getClassifier" 


-- | <p>Lists all classifier objects in the Data Catalog.</p>
getClassifiers :: forall eff. GetClassifiersRequest -> Aff (exception :: EXCEPTION | eff) GetClassifiersResponse
getClassifiers = Request.request serviceName "getClassifiers" 


-- | <p>Retrieves a connection definition from the Data Catalog.</p>
getConnection :: forall eff. GetConnectionRequest -> Aff (exception :: EXCEPTION | eff) GetConnectionResponse
getConnection = Request.request serviceName "getConnection" 


-- | <p>Retrieves a list of connection definitions from the Data Catalog.</p>
getConnections :: forall eff. GetConnectionsRequest -> Aff (exception :: EXCEPTION | eff) GetConnectionsResponse
getConnections = Request.request serviceName "getConnections" 


-- | <p>Retrieves metadata for a specified crawler.</p>
getCrawler :: forall eff. GetCrawlerRequest -> Aff (exception :: EXCEPTION | eff) GetCrawlerResponse
getCrawler = Request.request serviceName "getCrawler" 


-- | <p>Retrieves metrics about specified crawlers.</p>
getCrawlerMetrics :: forall eff. GetCrawlerMetricsRequest -> Aff (exception :: EXCEPTION | eff) GetCrawlerMetricsResponse
getCrawlerMetrics = Request.request serviceName "getCrawlerMetrics" 


-- | <p>Retrieves metadata for all crawlers defined in the customer account.</p>
getCrawlers :: forall eff. GetCrawlersRequest -> Aff (exception :: EXCEPTION | eff) GetCrawlersResponse
getCrawlers = Request.request serviceName "getCrawlers" 


-- | <p>Retrieves the definition of a specified database.</p>
getDatabase :: forall eff. GetDatabaseRequest -> Aff (exception :: EXCEPTION | eff) GetDatabaseResponse
getDatabase = Request.request serviceName "getDatabase" 


-- | <p>Retrieves all Databases defined in a given Data Catalog.</p>
getDatabases :: forall eff. GetDatabasesRequest -> Aff (exception :: EXCEPTION | eff) GetDatabasesResponse
getDatabases = Request.request serviceName "getDatabases" 


-- | <p>Transforms a Python script into a directed acyclic graph (DAG). </p>
getDataflowGraph :: forall eff. GetDataflowGraphRequest -> Aff (exception :: EXCEPTION | eff) GetDataflowGraphResponse
getDataflowGraph = Request.request serviceName "getDataflowGraph" 


-- | <p>Retrieves information about a specified DevEndpoint.</p>
getDevEndpoint :: forall eff. GetDevEndpointRequest -> Aff (exception :: EXCEPTION | eff) GetDevEndpointResponse
getDevEndpoint = Request.request serviceName "getDevEndpoint" 


-- | <p>Retrieves all the DevEndpoints in this AWS account.</p>
getDevEndpoints :: forall eff. GetDevEndpointsRequest -> Aff (exception :: EXCEPTION | eff) GetDevEndpointsResponse
getDevEndpoints = Request.request serviceName "getDevEndpoints" 


-- | <p>Retrieves an existing job definition.</p>
getJob :: forall eff. GetJobRequest -> Aff (exception :: EXCEPTION | eff) GetJobResponse
getJob = Request.request serviceName "getJob" 


-- | <p>Retrieves the metadata for a given job run.</p>
getJobRun :: forall eff. GetJobRunRequest -> Aff (exception :: EXCEPTION | eff) GetJobRunResponse
getJobRun = Request.request serviceName "getJobRun" 


-- | <p>Retrieves metadata for all runs of a given job.</p>
getJobRuns :: forall eff. GetJobRunsRequest -> Aff (exception :: EXCEPTION | eff) GetJobRunsResponse
getJobRuns = Request.request serviceName "getJobRuns" 


-- | <p>Retrieves all current jobs.</p>
getJobs :: forall eff. GetJobsRequest -> Aff (exception :: EXCEPTION | eff) GetJobsResponse
getJobs = Request.request serviceName "getJobs" 


-- | <p>Creates mappings.</p>
getMapping :: forall eff. GetMappingRequest -> Aff (exception :: EXCEPTION | eff) GetMappingResponse
getMapping = Request.request serviceName "getMapping" 


-- | <p>Retrieves information about a specified partition.</p>
getPartition :: forall eff. GetPartitionRequest -> Aff (exception :: EXCEPTION | eff) GetPartitionResponse
getPartition = Request.request serviceName "getPartition" 


-- | <p>Retrieves information about the partitions in a table.</p>
getPartitions :: forall eff. GetPartitionsRequest -> Aff (exception :: EXCEPTION | eff) GetPartitionsResponse
getPartitions = Request.request serviceName "getPartitions" 


-- | <p>Gets code to perform a specified mapping.</p>
getPlan :: forall eff. GetPlanRequest -> Aff (exception :: EXCEPTION | eff) GetPlanResponse
getPlan = Request.request serviceName "getPlan" 


-- | <p>Retrieves the <code>Table</code> definition in a Data Catalog for a specified table.</p>
getTable :: forall eff. GetTableRequest -> Aff (exception :: EXCEPTION | eff) GetTableResponse
getTable = Request.request serviceName "getTable" 


-- | <p>Retrieves a specified version of a table.</p>
getTableVersion :: forall eff. GetTableVersionRequest -> Aff (exception :: EXCEPTION | eff) GetTableVersionResponse
getTableVersion = Request.request serviceName "getTableVersion" 


-- | <p>Retrieves a list of strings that identify available versions of a specified table.</p>
getTableVersions :: forall eff. GetTableVersionsRequest -> Aff (exception :: EXCEPTION | eff) GetTableVersionsResponse
getTableVersions = Request.request serviceName "getTableVersions" 


-- | <p>Retrieves the definitions of some or all of the tables in a given <code>Database</code>.</p>
getTables :: forall eff. GetTablesRequest -> Aff (exception :: EXCEPTION | eff) GetTablesResponse
getTables = Request.request serviceName "getTables" 


-- | <p>Retrieves the definition of a trigger.</p>
getTrigger :: forall eff. GetTriggerRequest -> Aff (exception :: EXCEPTION | eff) GetTriggerResponse
getTrigger = Request.request serviceName "getTrigger" 


-- | <p>Gets all the triggers associated with a job.</p>
getTriggers :: forall eff. GetTriggersRequest -> Aff (exception :: EXCEPTION | eff) GetTriggersResponse
getTriggers = Request.request serviceName "getTriggers" 


-- | <p>Retrieves a specified function definition from the Data Catalog.</p>
getUserDefinedFunction :: forall eff. GetUserDefinedFunctionRequest -> Aff (exception :: EXCEPTION | eff) GetUserDefinedFunctionResponse
getUserDefinedFunction = Request.request serviceName "getUserDefinedFunction" 


-- | <p>Retrieves a multiple function definitions from the Data Catalog.</p>
getUserDefinedFunctions :: forall eff. GetUserDefinedFunctionsRequest -> Aff (exception :: EXCEPTION | eff) GetUserDefinedFunctionsResponse
getUserDefinedFunctions = Request.request serviceName "getUserDefinedFunctions" 


-- | <p>Imports an existing Athena Data Catalog to AWS Glue</p>
importCatalogToGlue :: forall eff. ImportCatalogToGlueRequest -> Aff (exception :: EXCEPTION | eff) ImportCatalogToGlueResponse
importCatalogToGlue = Request.request serviceName "importCatalogToGlue" 


-- | <p>Resets a bookmark entry.</p>
resetJobBookmark :: forall eff. ResetJobBookmarkRequest -> Aff (exception :: EXCEPTION | eff) ResetJobBookmarkResponse
resetJobBookmark = Request.request serviceName "resetJobBookmark" 


-- | <p>Starts a crawl using the specified crawler, regardless of what is scheduled. If the crawler is already running, does nothing.</p>
startCrawler :: forall eff. StartCrawlerRequest -> Aff (exception :: EXCEPTION | eff) StartCrawlerResponse
startCrawler = Request.request serviceName "startCrawler" 


-- | <p>Changes the schedule state of the specified crawler to <code>SCHEDULED</code>, unless the crawler is already running or the schedule state is already <code>SCHEDULED</code>.</p>
startCrawlerSchedule :: forall eff. StartCrawlerScheduleRequest -> Aff (exception :: EXCEPTION | eff) StartCrawlerScheduleResponse
startCrawlerSchedule = Request.request serviceName "startCrawlerSchedule" 


-- | <p>Runs a job.</p>
startJobRun :: forall eff. StartJobRunRequest -> Aff (exception :: EXCEPTION | eff) StartJobRunResponse
startJobRun = Request.request serviceName "startJobRun" 


-- | <p>Starts an existing trigger. See <a href="http://docs.aws.amazon.com/glue/latest/dg/trigger-job.html">Triggering Jobs</a> for information about how different types of trigger are started.</p>
startTrigger :: forall eff. StartTriggerRequest -> Aff (exception :: EXCEPTION | eff) StartTriggerResponse
startTrigger = Request.request serviceName "startTrigger" 


-- | <p>If the specified crawler is running, stops the crawl.</p>
stopCrawler :: forall eff. StopCrawlerRequest -> Aff (exception :: EXCEPTION | eff) StopCrawlerResponse
stopCrawler = Request.request serviceName "stopCrawler" 


-- | <p>Sets the schedule state of the specified crawler to <code>NOT_SCHEDULED</code>, but does not stop the crawler if it is already running.</p>
stopCrawlerSchedule :: forall eff. StopCrawlerScheduleRequest -> Aff (exception :: EXCEPTION | eff) StopCrawlerScheduleResponse
stopCrawlerSchedule = Request.request serviceName "stopCrawlerSchedule" 


-- | <p>Stops a specified trigger.</p>
stopTrigger :: forall eff. StopTriggerRequest -> Aff (exception :: EXCEPTION | eff) StopTriggerResponse
stopTrigger = Request.request serviceName "stopTrigger" 


-- | <p>Modifies an existing classifier (a <code>GrokClassifier</code>, <code>XMLClassifier</code>, or <code>JsonClassifier</code>, depending on which field is present).</p>
updateClassifier :: forall eff. UpdateClassifierRequest -> Aff (exception :: EXCEPTION | eff) UpdateClassifierResponse
updateClassifier = Request.request serviceName "updateClassifier" 


-- | <p>Updates a connection definition in the Data Catalog.</p>
updateConnection :: forall eff. UpdateConnectionRequest -> Aff (exception :: EXCEPTION | eff) UpdateConnectionResponse
updateConnection = Request.request serviceName "updateConnection" 


-- | <p>Updates a crawler. If a crawler is running, you must stop it using <code>StopCrawler</code> before updating it.</p>
updateCrawler :: forall eff. UpdateCrawlerRequest -> Aff (exception :: EXCEPTION | eff) UpdateCrawlerResponse
updateCrawler = Request.request serviceName "updateCrawler" 


-- | <p>Updates the schedule of a crawler using a <code>cron</code> expression. </p>
updateCrawlerSchedule :: forall eff. UpdateCrawlerScheduleRequest -> Aff (exception :: EXCEPTION | eff) UpdateCrawlerScheduleResponse
updateCrawlerSchedule = Request.request serviceName "updateCrawlerSchedule" 


-- | <p>Updates an existing database definition in a Data Catalog.</p>
updateDatabase :: forall eff. UpdateDatabaseRequest -> Aff (exception :: EXCEPTION | eff) UpdateDatabaseResponse
updateDatabase = Request.request serviceName "updateDatabase" 


-- | <p>Updates a specified DevEndpoint.</p>
updateDevEndpoint :: forall eff. UpdateDevEndpointRequest -> Aff (exception :: EXCEPTION | eff) UpdateDevEndpointResponse
updateDevEndpoint = Request.request serviceName "updateDevEndpoint" 


-- | <p>Updates an existing job definition.</p>
updateJob :: forall eff. UpdateJobRequest -> Aff (exception :: EXCEPTION | eff) UpdateJobResponse
updateJob = Request.request serviceName "updateJob" 


-- | <p>Updates a partition.</p>
updatePartition :: forall eff. UpdatePartitionRequest -> Aff (exception :: EXCEPTION | eff) UpdatePartitionResponse
updatePartition = Request.request serviceName "updatePartition" 


-- | <p>Updates a metadata table in the Data Catalog.</p>
updateTable :: forall eff. UpdateTableRequest -> Aff (exception :: EXCEPTION | eff) UpdateTableResponse
updateTable = Request.request serviceName "updateTable" 


-- | <p>Updates a trigger definition.</p>
updateTrigger :: forall eff. UpdateTriggerRequest -> Aff (exception :: EXCEPTION | eff) UpdateTriggerResponse
updateTrigger = Request.request serviceName "updateTrigger" 


-- | <p>Updates an existing function definition in the Data Catalog.</p>
updateUserDefinedFunction :: forall eff. UpdateUserDefinedFunctionRequest -> Aff (exception :: EXCEPTION | eff) UpdateUserDefinedFunctionResponse
updateUserDefinedFunction = Request.request serviceName "updateUserDefinedFunction" 


-- | <p>Access to a resource was denied.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _
derive instance repGenericAccessDeniedException :: Generic AccessDeniedException _
instance showAccessDeniedException :: Show AccessDeniedException where
  show = genericShow
instance decodeAccessDeniedException :: Decode AccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDeniedException :: Encode AccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines an action to be initiated by a trigger.</p>
newtype Action = Action 
  { "JobName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "Arguments" :: NullOrUndefined.NullOrUndefined (GenericMap)
  }
derive instance newtypeAction :: Newtype Action _
derive instance repGenericAction :: Generic Action _
instance showAction :: Show Action where
  show = genericShow
instance decodeAction :: Decode Action where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAction :: Encode Action where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionList = ActionList (Array Action)
derive instance newtypeActionList :: Newtype ActionList _
derive instance repGenericActionList :: Generic ActionList _
instance showActionList :: Show ActionList where
  show = genericShow
instance decodeActionList :: Decode ActionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionList :: Encode ActionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A resource to be created or added already exists.</p>
newtype AlreadyExistsException = AlreadyExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeAlreadyExistsException :: Newtype AlreadyExistsException _
derive instance repGenericAlreadyExistsException :: Generic AlreadyExistsException _
instance showAlreadyExistsException :: Show AlreadyExistsException where
  show = genericShow
instance decodeAlreadyExistsException :: Decode AlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlreadyExistsException :: Encode AlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttemptCount = AttemptCount Int
derive instance newtypeAttemptCount :: Newtype AttemptCount _
derive instance repGenericAttemptCount :: Generic AttemptCount _
instance showAttemptCount :: Show AttemptCount where
  show = genericShow
instance decodeAttemptCount :: Decode AttemptCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttemptCount :: Encode AttemptCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchCreatePartitionRequest = BatchCreatePartitionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionInputList" :: (PartitionInputList)
  }
derive instance newtypeBatchCreatePartitionRequest :: Newtype BatchCreatePartitionRequest _
derive instance repGenericBatchCreatePartitionRequest :: Generic BatchCreatePartitionRequest _
instance showBatchCreatePartitionRequest :: Show BatchCreatePartitionRequest where
  show = genericShow
instance decodeBatchCreatePartitionRequest :: Decode BatchCreatePartitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchCreatePartitionRequest :: Encode BatchCreatePartitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchCreatePartitionResponse = BatchCreatePartitionResponse 
  { "Errors" :: NullOrUndefined.NullOrUndefined (PartitionErrors)
  }
derive instance newtypeBatchCreatePartitionResponse :: Newtype BatchCreatePartitionResponse _
derive instance repGenericBatchCreatePartitionResponse :: Generic BatchCreatePartitionResponse _
instance showBatchCreatePartitionResponse :: Show BatchCreatePartitionResponse where
  show = genericShow
instance decodeBatchCreatePartitionResponse :: Decode BatchCreatePartitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchCreatePartitionResponse :: Encode BatchCreatePartitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteConnectionRequest = BatchDeleteConnectionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "ConnectionNameList" :: (DeleteConnectionNameList)
  }
derive instance newtypeBatchDeleteConnectionRequest :: Newtype BatchDeleteConnectionRequest _
derive instance repGenericBatchDeleteConnectionRequest :: Generic BatchDeleteConnectionRequest _
instance showBatchDeleteConnectionRequest :: Show BatchDeleteConnectionRequest where
  show = genericShow
instance decodeBatchDeleteConnectionRequest :: Decode BatchDeleteConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteConnectionRequest :: Encode BatchDeleteConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteConnectionResponse = BatchDeleteConnectionResponse 
  { "Succeeded" :: NullOrUndefined.NullOrUndefined (NameStringList)
  , "Errors" :: NullOrUndefined.NullOrUndefined (ErrorByName)
  }
derive instance newtypeBatchDeleteConnectionResponse :: Newtype BatchDeleteConnectionResponse _
derive instance repGenericBatchDeleteConnectionResponse :: Generic BatchDeleteConnectionResponse _
instance showBatchDeleteConnectionResponse :: Show BatchDeleteConnectionResponse where
  show = genericShow
instance decodeBatchDeleteConnectionResponse :: Decode BatchDeleteConnectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteConnectionResponse :: Encode BatchDeleteConnectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeletePartitionRequest = BatchDeletePartitionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionsToDelete" :: (BatchDeletePartitionValueList)
  }
derive instance newtypeBatchDeletePartitionRequest :: Newtype BatchDeletePartitionRequest _
derive instance repGenericBatchDeletePartitionRequest :: Generic BatchDeletePartitionRequest _
instance showBatchDeletePartitionRequest :: Show BatchDeletePartitionRequest where
  show = genericShow
instance decodeBatchDeletePartitionRequest :: Decode BatchDeletePartitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeletePartitionRequest :: Encode BatchDeletePartitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeletePartitionResponse = BatchDeletePartitionResponse 
  { "Errors" :: NullOrUndefined.NullOrUndefined (PartitionErrors)
  }
derive instance newtypeBatchDeletePartitionResponse :: Newtype BatchDeletePartitionResponse _
derive instance repGenericBatchDeletePartitionResponse :: Generic BatchDeletePartitionResponse _
instance showBatchDeletePartitionResponse :: Show BatchDeletePartitionResponse where
  show = genericShow
instance decodeBatchDeletePartitionResponse :: Decode BatchDeletePartitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeletePartitionResponse :: Encode BatchDeletePartitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeletePartitionValueList = BatchDeletePartitionValueList (Array PartitionValueList)
derive instance newtypeBatchDeletePartitionValueList :: Newtype BatchDeletePartitionValueList _
derive instance repGenericBatchDeletePartitionValueList :: Generic BatchDeletePartitionValueList _
instance showBatchDeletePartitionValueList :: Show BatchDeletePartitionValueList where
  show = genericShow
instance decodeBatchDeletePartitionValueList :: Decode BatchDeletePartitionValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeletePartitionValueList :: Encode BatchDeletePartitionValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteTableNameList = BatchDeleteTableNameList (Array NameString)
derive instance newtypeBatchDeleteTableNameList :: Newtype BatchDeleteTableNameList _
derive instance repGenericBatchDeleteTableNameList :: Generic BatchDeleteTableNameList _
instance showBatchDeleteTableNameList :: Show BatchDeleteTableNameList where
  show = genericShow
instance decodeBatchDeleteTableNameList :: Decode BatchDeleteTableNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteTableNameList :: Encode BatchDeleteTableNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteTableRequest = BatchDeleteTableRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TablesToDelete" :: (BatchDeleteTableNameList)
  }
derive instance newtypeBatchDeleteTableRequest :: Newtype BatchDeleteTableRequest _
derive instance repGenericBatchDeleteTableRequest :: Generic BatchDeleteTableRequest _
instance showBatchDeleteTableRequest :: Show BatchDeleteTableRequest where
  show = genericShow
instance decodeBatchDeleteTableRequest :: Decode BatchDeleteTableRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteTableRequest :: Encode BatchDeleteTableRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteTableResponse = BatchDeleteTableResponse 
  { "Errors" :: NullOrUndefined.NullOrUndefined (TableErrors)
  }
derive instance newtypeBatchDeleteTableResponse :: Newtype BatchDeleteTableResponse _
derive instance repGenericBatchDeleteTableResponse :: Generic BatchDeleteTableResponse _
instance showBatchDeleteTableResponse :: Show BatchDeleteTableResponse where
  show = genericShow
instance decodeBatchDeleteTableResponse :: Decode BatchDeleteTableResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteTableResponse :: Encode BatchDeleteTableResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteTableVersionList = BatchDeleteTableVersionList (Array VersionString)
derive instance newtypeBatchDeleteTableVersionList :: Newtype BatchDeleteTableVersionList _
derive instance repGenericBatchDeleteTableVersionList :: Generic BatchDeleteTableVersionList _
instance showBatchDeleteTableVersionList :: Show BatchDeleteTableVersionList where
  show = genericShow
instance decodeBatchDeleteTableVersionList :: Decode BatchDeleteTableVersionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteTableVersionList :: Encode BatchDeleteTableVersionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteTableVersionRequest = BatchDeleteTableVersionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionIds" :: (BatchDeleteTableVersionList)
  }
derive instance newtypeBatchDeleteTableVersionRequest :: Newtype BatchDeleteTableVersionRequest _
derive instance repGenericBatchDeleteTableVersionRequest :: Generic BatchDeleteTableVersionRequest _
instance showBatchDeleteTableVersionRequest :: Show BatchDeleteTableVersionRequest where
  show = genericShow
instance decodeBatchDeleteTableVersionRequest :: Decode BatchDeleteTableVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteTableVersionRequest :: Encode BatchDeleteTableVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse 
  { "Errors" :: NullOrUndefined.NullOrUndefined (TableVersionErrors)
  }
derive instance newtypeBatchDeleteTableVersionResponse :: Newtype BatchDeleteTableVersionResponse _
derive instance repGenericBatchDeleteTableVersionResponse :: Generic BatchDeleteTableVersionResponse _
instance showBatchDeleteTableVersionResponse :: Show BatchDeleteTableVersionResponse where
  show = genericShow
instance decodeBatchDeleteTableVersionResponse :: Decode BatchDeleteTableVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteTableVersionResponse :: Encode BatchDeleteTableVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetPartitionRequest = BatchGetPartitionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionsToGet" :: (BatchGetPartitionValueList)
  }
derive instance newtypeBatchGetPartitionRequest :: Newtype BatchGetPartitionRequest _
derive instance repGenericBatchGetPartitionRequest :: Generic BatchGetPartitionRequest _
instance showBatchGetPartitionRequest :: Show BatchGetPartitionRequest where
  show = genericShow
instance decodeBatchGetPartitionRequest :: Decode BatchGetPartitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetPartitionRequest :: Encode BatchGetPartitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetPartitionResponse = BatchGetPartitionResponse 
  { "Partitions" :: NullOrUndefined.NullOrUndefined (PartitionList)
  , "UnprocessedKeys" :: NullOrUndefined.NullOrUndefined (BatchGetPartitionValueList)
  }
derive instance newtypeBatchGetPartitionResponse :: Newtype BatchGetPartitionResponse _
derive instance repGenericBatchGetPartitionResponse :: Generic BatchGetPartitionResponse _
instance showBatchGetPartitionResponse :: Show BatchGetPartitionResponse where
  show = genericShow
instance decodeBatchGetPartitionResponse :: Decode BatchGetPartitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetPartitionResponse :: Encode BatchGetPartitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetPartitionValueList = BatchGetPartitionValueList (Array PartitionValueList)
derive instance newtypeBatchGetPartitionValueList :: Newtype BatchGetPartitionValueList _
derive instance repGenericBatchGetPartitionValueList :: Generic BatchGetPartitionValueList _
instance showBatchGetPartitionValueList :: Show BatchGetPartitionValueList where
  show = genericShow
instance decodeBatchGetPartitionValueList :: Decode BatchGetPartitionValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetPartitionValueList :: Encode BatchGetPartitionValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Records an error that occurred when attempting to stop a specified JobRun.</p>
newtype BatchStopJobRunError = BatchStopJobRunError 
  { "JobName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "JobRunId" :: NullOrUndefined.NullOrUndefined (IdString)
  , "ErrorDetail" :: NullOrUndefined.NullOrUndefined (ErrorDetail)
  }
derive instance newtypeBatchStopJobRunError :: Newtype BatchStopJobRunError _
derive instance repGenericBatchStopJobRunError :: Generic BatchStopJobRunError _
instance showBatchStopJobRunError :: Show BatchStopJobRunError where
  show = genericShow
instance decodeBatchStopJobRunError :: Decode BatchStopJobRunError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchStopJobRunError :: Encode BatchStopJobRunError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchStopJobRunErrorList = BatchStopJobRunErrorList (Array BatchStopJobRunError)
derive instance newtypeBatchStopJobRunErrorList :: Newtype BatchStopJobRunErrorList _
derive instance repGenericBatchStopJobRunErrorList :: Generic BatchStopJobRunErrorList _
instance showBatchStopJobRunErrorList :: Show BatchStopJobRunErrorList where
  show = genericShow
instance decodeBatchStopJobRunErrorList :: Decode BatchStopJobRunErrorList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchStopJobRunErrorList :: Encode BatchStopJobRunErrorList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchStopJobRunJobRunIdList = BatchStopJobRunJobRunIdList (Array IdString)
derive instance newtypeBatchStopJobRunJobRunIdList :: Newtype BatchStopJobRunJobRunIdList _
derive instance repGenericBatchStopJobRunJobRunIdList :: Generic BatchStopJobRunJobRunIdList _
instance showBatchStopJobRunJobRunIdList :: Show BatchStopJobRunJobRunIdList where
  show = genericShow
instance decodeBatchStopJobRunJobRunIdList :: Decode BatchStopJobRunJobRunIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchStopJobRunJobRunIdList :: Encode BatchStopJobRunJobRunIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchStopJobRunRequest = BatchStopJobRunRequest 
  { "JobName" :: (NameString)
  , "JobRunIds" :: (BatchStopJobRunJobRunIdList)
  }
derive instance newtypeBatchStopJobRunRequest :: Newtype BatchStopJobRunRequest _
derive instance repGenericBatchStopJobRunRequest :: Generic BatchStopJobRunRequest _
instance showBatchStopJobRunRequest :: Show BatchStopJobRunRequest where
  show = genericShow
instance decodeBatchStopJobRunRequest :: Decode BatchStopJobRunRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchStopJobRunRequest :: Encode BatchStopJobRunRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchStopJobRunResponse = BatchStopJobRunResponse 
  { "SuccessfulSubmissions" :: NullOrUndefined.NullOrUndefined (BatchStopJobRunSuccessfulSubmissionList)
  , "Errors" :: NullOrUndefined.NullOrUndefined (BatchStopJobRunErrorList)
  }
derive instance newtypeBatchStopJobRunResponse :: Newtype BatchStopJobRunResponse _
derive instance repGenericBatchStopJobRunResponse :: Generic BatchStopJobRunResponse _
instance showBatchStopJobRunResponse :: Show BatchStopJobRunResponse where
  show = genericShow
instance decodeBatchStopJobRunResponse :: Decode BatchStopJobRunResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchStopJobRunResponse :: Encode BatchStopJobRunResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Records a successful request to stop a specified JobRun.</p>
newtype BatchStopJobRunSuccessfulSubmission = BatchStopJobRunSuccessfulSubmission 
  { "JobName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "JobRunId" :: NullOrUndefined.NullOrUndefined (IdString)
  }
derive instance newtypeBatchStopJobRunSuccessfulSubmission :: Newtype BatchStopJobRunSuccessfulSubmission _
derive instance repGenericBatchStopJobRunSuccessfulSubmission :: Generic BatchStopJobRunSuccessfulSubmission _
instance showBatchStopJobRunSuccessfulSubmission :: Show BatchStopJobRunSuccessfulSubmission where
  show = genericShow
instance decodeBatchStopJobRunSuccessfulSubmission :: Decode BatchStopJobRunSuccessfulSubmission where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchStopJobRunSuccessfulSubmission :: Encode BatchStopJobRunSuccessfulSubmission where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchStopJobRunSuccessfulSubmissionList = BatchStopJobRunSuccessfulSubmissionList (Array BatchStopJobRunSuccessfulSubmission)
derive instance newtypeBatchStopJobRunSuccessfulSubmissionList :: Newtype BatchStopJobRunSuccessfulSubmissionList _
derive instance repGenericBatchStopJobRunSuccessfulSubmissionList :: Generic BatchStopJobRunSuccessfulSubmissionList _
instance showBatchStopJobRunSuccessfulSubmissionList :: Show BatchStopJobRunSuccessfulSubmissionList where
  show = genericShow
instance decodeBatchStopJobRunSuccessfulSubmissionList :: Decode BatchStopJobRunSuccessfulSubmissionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchStopJobRunSuccessfulSubmissionList :: Encode BatchStopJobRunSuccessfulSubmissionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanNullable = BooleanNullable Boolean
derive instance newtypeBooleanNullable :: Newtype BooleanNullable _
derive instance repGenericBooleanNullable :: Generic BooleanNullable _
instance showBooleanNullable :: Show BooleanNullable where
  show = genericShow
instance decodeBooleanNullable :: Decode BooleanNullable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanNullable :: Encode BooleanNullable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanValue = BooleanValue Boolean
derive instance newtypeBooleanValue :: Newtype BooleanValue _
derive instance repGenericBooleanValue :: Generic BooleanValue _
instance showBooleanValue :: Show BooleanValue where
  show = genericShow
instance decodeBooleanValue :: Decode BooleanValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanValue :: Encode BooleanValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BoundedPartitionValueList = BoundedPartitionValueList (Array ValueString)
derive instance newtypeBoundedPartitionValueList :: Newtype BoundedPartitionValueList _
derive instance repGenericBoundedPartitionValueList :: Generic BoundedPartitionValueList _
instance showBoundedPartitionValueList :: Show BoundedPartitionValueList where
  show = genericShow
instance decodeBoundedPartitionValueList :: Decode BoundedPartitionValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBoundedPartitionValueList :: Encode BoundedPartitionValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CatalogEntries = CatalogEntries (Array CatalogEntry)
derive instance newtypeCatalogEntries :: Newtype CatalogEntries _
derive instance repGenericCatalogEntries :: Generic CatalogEntries _
instance showCatalogEntries :: Show CatalogEntries where
  show = genericShow
instance decodeCatalogEntries :: Decode CatalogEntries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCatalogEntries :: Encode CatalogEntries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a table definition in the Data Catalog.</p>
newtype CatalogEntry = CatalogEntry 
  { "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  }
derive instance newtypeCatalogEntry :: Newtype CatalogEntry _
derive instance repGenericCatalogEntry :: Generic CatalogEntry _
instance showCatalogEntry :: Show CatalogEntry where
  show = genericShow
instance decodeCatalogEntry :: Decode CatalogEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCatalogEntry :: Encode CatalogEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CatalogIdString = CatalogIdString String
derive instance newtypeCatalogIdString :: Newtype CatalogIdString _
derive instance repGenericCatalogIdString :: Generic CatalogIdString _
instance showCatalogIdString :: Show CatalogIdString where
  show = genericShow
instance decodeCatalogIdString :: Decode CatalogIdString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCatalogIdString :: Encode CatalogIdString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure containing migration status information.</p>
newtype CatalogImportStatus = CatalogImportStatus 
  { "ImportCompleted" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "ImportTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "ImportedBy" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeCatalogImportStatus :: Newtype CatalogImportStatus _
derive instance repGenericCatalogImportStatus :: Generic CatalogImportStatus _
instance showCatalogImportStatus :: Show CatalogImportStatus where
  show = genericShow
instance decodeCatalogImportStatus :: Decode CatalogImportStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCatalogImportStatus :: Encode CatalogImportStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Classification = Classification String
derive instance newtypeClassification :: Newtype Classification _
derive instance repGenericClassification :: Generic Classification _
instance showClassification :: Show Classification where
  show = genericShow
instance decodeClassification :: Decode Classification where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClassification :: Encode Classification where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Classifiers are written in Python and triggered during a crawl task. You can write your own classifiers to best categorize your data sources and specify the appropriate schemas to use for them. A classifier checks whether a given file is in a format it can handle, and if it is, the classifier creates a schema in the form of a <code>StructType</code> object that matches that data format.</p> <p>A classifier can be a <code>grok</code> classifier, an XML classifier, or a JSON classifier, asspecified in one of the fields in the <code>Classifier</code> object.</p>
newtype Classifier = Classifier 
  { "GrokClassifier" :: NullOrUndefined.NullOrUndefined (GrokClassifier)
  , "XMLClassifier" :: NullOrUndefined.NullOrUndefined (XMLClassifier)
  , "JsonClassifier" :: NullOrUndefined.NullOrUndefined (JsonClassifier)
  }
derive instance newtypeClassifier :: Newtype Classifier _
derive instance repGenericClassifier :: Generic Classifier _
instance showClassifier :: Show Classifier where
  show = genericShow
instance decodeClassifier :: Decode Classifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClassifier :: Encode Classifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClassifierList = ClassifierList (Array Classifier)
derive instance newtypeClassifierList :: Newtype ClassifierList _
derive instance repGenericClassifierList :: Generic ClassifierList _
instance showClassifierList :: Show ClassifierList where
  show = genericShow
instance decodeClassifierList :: Decode ClassifierList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClassifierList :: Encode ClassifierList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClassifierNameList = ClassifierNameList (Array NameString)
derive instance newtypeClassifierNameList :: Newtype ClassifierNameList _
derive instance repGenericClassifierNameList :: Generic ClassifierNameList _
instance showClassifierNameList :: Show ClassifierNameList where
  show = genericShow
instance decodeClassifierNameList :: Decode ClassifierNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClassifierNameList :: Encode ClassifierNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CodeGenArgName = CodeGenArgName String
derive instance newtypeCodeGenArgName :: Newtype CodeGenArgName _
derive instance repGenericCodeGenArgName :: Generic CodeGenArgName _
instance showCodeGenArgName :: Show CodeGenArgName where
  show = genericShow
instance decodeCodeGenArgName :: Decode CodeGenArgName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeGenArgName :: Encode CodeGenArgName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CodeGenArgValue = CodeGenArgValue String
derive instance newtypeCodeGenArgValue :: Newtype CodeGenArgValue _
derive instance repGenericCodeGenArgValue :: Generic CodeGenArgValue _
instance showCodeGenArgValue :: Show CodeGenArgValue where
  show = genericShow
instance decodeCodeGenArgValue :: Decode CodeGenArgValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeGenArgValue :: Encode CodeGenArgValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a directional edge in a directed acyclic graph (DAG).</p>
newtype CodeGenEdge = CodeGenEdge 
  { "Source" :: (CodeGenIdentifier)
  , "Target" :: (CodeGenIdentifier)
  , "TargetParameter" :: NullOrUndefined.NullOrUndefined (CodeGenArgName)
  }
derive instance newtypeCodeGenEdge :: Newtype CodeGenEdge _
derive instance repGenericCodeGenEdge :: Generic CodeGenEdge _
instance showCodeGenEdge :: Show CodeGenEdge where
  show = genericShow
instance decodeCodeGenEdge :: Decode CodeGenEdge where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeGenEdge :: Encode CodeGenEdge where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CodeGenIdentifier = CodeGenIdentifier String
derive instance newtypeCodeGenIdentifier :: Newtype CodeGenIdentifier _
derive instance repGenericCodeGenIdentifier :: Generic CodeGenIdentifier _
instance showCodeGenIdentifier :: Show CodeGenIdentifier where
  show = genericShow
instance decodeCodeGenIdentifier :: Decode CodeGenIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeGenIdentifier :: Encode CodeGenIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a node in a directed acyclic graph (DAG)</p>
newtype CodeGenNode = CodeGenNode 
  { "Id" :: (CodeGenIdentifier)
  , "NodeType" :: (CodeGenNodeType)
  , "Args" :: (CodeGenNodeArgs)
  , "LineNumber" :: NullOrUndefined.NullOrUndefined (Int)
  }
derive instance newtypeCodeGenNode :: Newtype CodeGenNode _
derive instance repGenericCodeGenNode :: Generic CodeGenNode _
instance showCodeGenNode :: Show CodeGenNode where
  show = genericShow
instance decodeCodeGenNode :: Decode CodeGenNode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeGenNode :: Encode CodeGenNode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An argument or property of a node.</p>
newtype CodeGenNodeArg = CodeGenNodeArg 
  { "Name" :: (CodeGenArgName)
  , "Value" :: (CodeGenArgValue)
  , "Param" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeCodeGenNodeArg :: Newtype CodeGenNodeArg _
derive instance repGenericCodeGenNodeArg :: Generic CodeGenNodeArg _
instance showCodeGenNodeArg :: Show CodeGenNodeArg where
  show = genericShow
instance decodeCodeGenNodeArg :: Decode CodeGenNodeArg where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeGenNodeArg :: Encode CodeGenNodeArg where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CodeGenNodeArgs = CodeGenNodeArgs (Array CodeGenNodeArg)
derive instance newtypeCodeGenNodeArgs :: Newtype CodeGenNodeArgs _
derive instance repGenericCodeGenNodeArgs :: Generic CodeGenNodeArgs _
instance showCodeGenNodeArgs :: Show CodeGenNodeArgs where
  show = genericShow
instance decodeCodeGenNodeArgs :: Decode CodeGenNodeArgs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeGenNodeArgs :: Encode CodeGenNodeArgs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CodeGenNodeType = CodeGenNodeType String
derive instance newtypeCodeGenNodeType :: Newtype CodeGenNodeType _
derive instance repGenericCodeGenNodeType :: Generic CodeGenNodeType _
instance showCodeGenNodeType :: Show CodeGenNodeType where
  show = genericShow
instance decodeCodeGenNodeType :: Decode CodeGenNodeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCodeGenNodeType :: Encode CodeGenNodeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A column in a <code>Table</code>.</p>
newtype Column = Column 
  { "Name" :: (NameString)
  , "Type" :: NullOrUndefined.NullOrUndefined (ColumnTypeString)
  , "Comment" :: NullOrUndefined.NullOrUndefined (CommentString)
  }
derive instance newtypeColumn :: Newtype Column _
derive instance repGenericColumn :: Generic Column _
instance showColumn :: Show Column where
  show = genericShow
instance decodeColumn :: Decode Column where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColumn :: Encode Column where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ColumnList = ColumnList (Array Column)
derive instance newtypeColumnList :: Newtype ColumnList _
derive instance repGenericColumnList :: Generic ColumnList _
instance showColumnList :: Show ColumnList where
  show = genericShow
instance decodeColumnList :: Decode ColumnList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColumnList :: Encode ColumnList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ColumnTypeString = ColumnTypeString String
derive instance newtypeColumnTypeString :: Newtype ColumnTypeString _
derive instance repGenericColumnTypeString :: Generic ColumnTypeString _
instance showColumnTypeString :: Show ColumnTypeString where
  show = genericShow
instance decodeColumnTypeString :: Decode ColumnTypeString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColumnTypeString :: Encode ColumnTypeString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ColumnValueStringList = ColumnValueStringList (Array ColumnValuesString)
derive instance newtypeColumnValueStringList :: Newtype ColumnValueStringList _
derive instance repGenericColumnValueStringList :: Generic ColumnValueStringList _
instance showColumnValueStringList :: Show ColumnValueStringList where
  show = genericShow
instance decodeColumnValueStringList :: Decode ColumnValueStringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColumnValueStringList :: Encode ColumnValueStringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ColumnValuesString = ColumnValuesString String
derive instance newtypeColumnValuesString :: Newtype ColumnValuesString _
derive instance repGenericColumnValuesString :: Generic ColumnValuesString _
instance showColumnValuesString :: Show ColumnValuesString where
  show = genericShow
instance decodeColumnValuesString :: Decode ColumnValuesString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeColumnValuesString :: Encode ColumnValuesString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentString = CommentString String
derive instance newtypeCommentString :: Newtype CommentString _
derive instance repGenericCommentString :: Generic CommentString _
instance showCommentString :: Show CommentString where
  show = genericShow
instance decodeCommentString :: Decode CommentString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentString :: Encode CommentString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Two processes are trying to modify a resource simultaneously.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Too many jobs are being run concurrently.</p>
newtype ConcurrentRunsExceededException = ConcurrentRunsExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeConcurrentRunsExceededException :: Newtype ConcurrentRunsExceededException _
derive instance repGenericConcurrentRunsExceededException :: Generic ConcurrentRunsExceededException _
instance showConcurrentRunsExceededException :: Show ConcurrentRunsExceededException where
  show = genericShow
instance decodeConcurrentRunsExceededException :: Decode ConcurrentRunsExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentRunsExceededException :: Encode ConcurrentRunsExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a condition under which a trigger fires.</p>
newtype Condition = Condition 
  { "LogicalOperator" :: NullOrUndefined.NullOrUndefined (LogicalOperator)
  , "JobName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "State" :: NullOrUndefined.NullOrUndefined (JobRunState)
  }
derive instance newtypeCondition :: Newtype Condition _
derive instance repGenericCondition :: Generic Condition _
instance showCondition :: Show Condition where
  show = genericShow
instance decodeCondition :: Decode Condition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCondition :: Encode Condition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConditionList = ConditionList (Array Condition)
derive instance newtypeConditionList :: Newtype ConditionList _
derive instance repGenericConditionList :: Generic ConditionList _
instance showConditionList :: Show ConditionList where
  show = genericShow
instance decodeConditionList :: Decode ConditionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConditionList :: Encode ConditionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a connection to a data source.</p>
newtype Connection = Connection 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "ConnectionType" :: NullOrUndefined.NullOrUndefined (ConnectionType)
  , "MatchCriteria" :: NullOrUndefined.NullOrUndefined (MatchCriteria)
  , "ConnectionProperties" :: NullOrUndefined.NullOrUndefined (ConnectionProperties)
  , "PhysicalConnectionRequirements" :: NullOrUndefined.NullOrUndefined (PhysicalConnectionRequirements)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdatedTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdatedBy" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeConnection :: Newtype Connection _
derive instance repGenericConnection :: Generic Connection _
instance showConnection :: Show Connection where
  show = genericShow
instance decodeConnection :: Decode Connection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnection :: Encode Connection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure used to specify a connection to create or update.</p>
newtype ConnectionInput = ConnectionInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "ConnectionType" :: (ConnectionType)
  , "MatchCriteria" :: NullOrUndefined.NullOrUndefined (MatchCriteria)
  , "ConnectionProperties" :: (ConnectionProperties)
  , "PhysicalConnectionRequirements" :: NullOrUndefined.NullOrUndefined (PhysicalConnectionRequirements)
  }
derive instance newtypeConnectionInput :: Newtype ConnectionInput _
derive instance repGenericConnectionInput :: Generic ConnectionInput _
instance showConnectionInput :: Show ConnectionInput where
  show = genericShow
instance decodeConnectionInput :: Decode ConnectionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionInput :: Encode ConnectionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectionList = ConnectionList (Array Connection)
derive instance newtypeConnectionList :: Newtype ConnectionList _
derive instance repGenericConnectionList :: Generic ConnectionList _
instance showConnectionList :: Show ConnectionList where
  show = genericShow
instance decodeConnectionList :: Decode ConnectionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionList :: Encode ConnectionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectionName = ConnectionName String
derive instance newtypeConnectionName :: Newtype ConnectionName _
derive instance repGenericConnectionName :: Generic ConnectionName _
instance showConnectionName :: Show ConnectionName where
  show = genericShow
instance decodeConnectionName :: Decode ConnectionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionName :: Encode ConnectionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectionProperties = ConnectionProperties (StrMap.StrMap ValueString)
derive instance newtypeConnectionProperties :: Newtype ConnectionProperties _
derive instance repGenericConnectionProperties :: Generic ConnectionProperties _
instance showConnectionProperties :: Show ConnectionProperties where
  show = genericShow
instance decodeConnectionProperties :: Decode ConnectionProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionProperties :: Encode ConnectionProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectionPropertyKey = ConnectionPropertyKey String
derive instance newtypeConnectionPropertyKey :: Newtype ConnectionPropertyKey _
derive instance repGenericConnectionPropertyKey :: Generic ConnectionPropertyKey _
instance showConnectionPropertyKey :: Show ConnectionPropertyKey where
  show = genericShow
instance decodeConnectionPropertyKey :: Decode ConnectionPropertyKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionPropertyKey :: Encode ConnectionPropertyKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectionType = ConnectionType String
derive instance newtypeConnectionType :: Newtype ConnectionType _
derive instance repGenericConnectionType :: Generic ConnectionType _
instance showConnectionType :: Show ConnectionType where
  show = genericShow
instance decodeConnectionType :: Decode ConnectionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionType :: Encode ConnectionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the connections used by a job.</p>
newtype ConnectionsList = ConnectionsList 
  { "Connections" :: NullOrUndefined.NullOrUndefined (StringList)
  }
derive instance newtypeConnectionsList :: Newtype ConnectionsList _
derive instance repGenericConnectionsList :: Generic ConnectionsList _
instance showConnectionsList :: Show ConnectionsList where
  show = genericShow
instance decodeConnectionsList :: Decode ConnectionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionsList :: Encode ConnectionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a crawler program that examines a data source and uses classifiers to try to determine its schema. If successful, the crawler records metadata concerning the data source in the AWS Glue Data Catalog.</p>
newtype Crawler = Crawler 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  , "Role" :: NullOrUndefined.NullOrUndefined (Role)
  , "Targets" :: NullOrUndefined.NullOrUndefined (CrawlerTargets)
  , "DatabaseName" :: NullOrUndefined.NullOrUndefined (DatabaseName)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "Classifiers" :: NullOrUndefined.NullOrUndefined (ClassifierNameList)
  , "SchemaChangePolicy" :: NullOrUndefined.NullOrUndefined (SchemaChangePolicy)
  , "State" :: NullOrUndefined.NullOrUndefined (CrawlerState)
  , "TablePrefix" :: NullOrUndefined.NullOrUndefined (TablePrefix)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (Schedule)
  , "CrawlElapsedTime" :: NullOrUndefined.NullOrUndefined (MillisecondsCount)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastCrawl" :: NullOrUndefined.NullOrUndefined (LastCrawlInfo)
  , "Version" :: NullOrUndefined.NullOrUndefined (VersionId)
  , "Configuration" :: NullOrUndefined.NullOrUndefined (CrawlerConfiguration)
  }
derive instance newtypeCrawler :: Newtype Crawler _
derive instance repGenericCrawler :: Generic Crawler _
instance showCrawler :: Show Crawler where
  show = genericShow
instance decodeCrawler :: Decode Crawler where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawler :: Encode Crawler where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CrawlerConfiguration = CrawlerConfiguration String
derive instance newtypeCrawlerConfiguration :: Newtype CrawlerConfiguration _
derive instance repGenericCrawlerConfiguration :: Generic CrawlerConfiguration _
instance showCrawlerConfiguration :: Show CrawlerConfiguration where
  show = genericShow
instance decodeCrawlerConfiguration :: Decode CrawlerConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerConfiguration :: Encode CrawlerConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CrawlerList = CrawlerList (Array Crawler)
derive instance newtypeCrawlerList :: Newtype CrawlerList _
derive instance repGenericCrawlerList :: Generic CrawlerList _
instance showCrawlerList :: Show CrawlerList where
  show = genericShow
instance decodeCrawlerList :: Decode CrawlerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerList :: Encode CrawlerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Metrics for a specified crawler.</p>
newtype CrawlerMetrics = CrawlerMetrics 
  { "CrawlerName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "TimeLeftSeconds" :: NullOrUndefined.NullOrUndefined (NonNegativeDouble)
  , "StillEstimating" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "LastRuntimeSeconds" :: NullOrUndefined.NullOrUndefined (NonNegativeDouble)
  , "MedianRuntimeSeconds" :: NullOrUndefined.NullOrUndefined (NonNegativeDouble)
  , "TablesCreated" :: NullOrUndefined.NullOrUndefined (NonNegativeInteger)
  , "TablesUpdated" :: NullOrUndefined.NullOrUndefined (NonNegativeInteger)
  , "TablesDeleted" :: NullOrUndefined.NullOrUndefined (NonNegativeInteger)
  }
derive instance newtypeCrawlerMetrics :: Newtype CrawlerMetrics _
derive instance repGenericCrawlerMetrics :: Generic CrawlerMetrics _
instance showCrawlerMetrics :: Show CrawlerMetrics where
  show = genericShow
instance decodeCrawlerMetrics :: Decode CrawlerMetrics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerMetrics :: Encode CrawlerMetrics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CrawlerMetricsList = CrawlerMetricsList (Array CrawlerMetrics)
derive instance newtypeCrawlerMetricsList :: Newtype CrawlerMetricsList _
derive instance repGenericCrawlerMetricsList :: Generic CrawlerMetricsList _
instance showCrawlerMetricsList :: Show CrawlerMetricsList where
  show = genericShow
instance decodeCrawlerMetricsList :: Decode CrawlerMetricsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerMetricsList :: Encode CrawlerMetricsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CrawlerNameList = CrawlerNameList (Array NameString)
derive instance newtypeCrawlerNameList :: Newtype CrawlerNameList _
derive instance repGenericCrawlerNameList :: Generic CrawlerNameList _
instance showCrawlerNameList :: Show CrawlerNameList where
  show = genericShow
instance decodeCrawlerNameList :: Decode CrawlerNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerNameList :: Encode CrawlerNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified crawler is not running.</p>
newtype CrawlerNotRunningException = CrawlerNotRunningException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeCrawlerNotRunningException :: Newtype CrawlerNotRunningException _
derive instance repGenericCrawlerNotRunningException :: Generic CrawlerNotRunningException _
instance showCrawlerNotRunningException :: Show CrawlerNotRunningException where
  show = genericShow
instance decodeCrawlerNotRunningException :: Decode CrawlerNotRunningException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerNotRunningException :: Encode CrawlerNotRunningException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation cannot be performed because the crawler is already running.</p>
newtype CrawlerRunningException = CrawlerRunningException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeCrawlerRunningException :: Newtype CrawlerRunningException _
derive instance repGenericCrawlerRunningException :: Generic CrawlerRunningException _
instance showCrawlerRunningException :: Show CrawlerRunningException where
  show = genericShow
instance decodeCrawlerRunningException :: Decode CrawlerRunningException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerRunningException :: Encode CrawlerRunningException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CrawlerState = CrawlerState String
derive instance newtypeCrawlerState :: Newtype CrawlerState _
derive instance repGenericCrawlerState :: Generic CrawlerState _
instance showCrawlerState :: Show CrawlerState where
  show = genericShow
instance decodeCrawlerState :: Decode CrawlerState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerState :: Encode CrawlerState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified crawler is stopping.</p>
newtype CrawlerStoppingException = CrawlerStoppingException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeCrawlerStoppingException :: Newtype CrawlerStoppingException _
derive instance repGenericCrawlerStoppingException :: Generic CrawlerStoppingException _
instance showCrawlerStoppingException :: Show CrawlerStoppingException where
  show = genericShow
instance decodeCrawlerStoppingException :: Decode CrawlerStoppingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerStoppingException :: Encode CrawlerStoppingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies data stores to crawl.</p>
newtype CrawlerTargets = CrawlerTargets 
  { "S3Targets" :: NullOrUndefined.NullOrUndefined (S3TargetList)
  , "JdbcTargets" :: NullOrUndefined.NullOrUndefined (JdbcTargetList)
  }
derive instance newtypeCrawlerTargets :: Newtype CrawlerTargets _
derive instance repGenericCrawlerTargets :: Generic CrawlerTargets _
instance showCrawlerTargets :: Show CrawlerTargets where
  show = genericShow
instance decodeCrawlerTargets :: Decode CrawlerTargets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCrawlerTargets :: Encode CrawlerTargets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClassifierRequest = CreateClassifierRequest 
  { "GrokClassifier" :: NullOrUndefined.NullOrUndefined (CreateGrokClassifierRequest)
  , "XMLClassifier" :: NullOrUndefined.NullOrUndefined (CreateXMLClassifierRequest)
  , "JsonClassifier" :: NullOrUndefined.NullOrUndefined (CreateJsonClassifierRequest)
  }
derive instance newtypeCreateClassifierRequest :: Newtype CreateClassifierRequest _
derive instance repGenericCreateClassifierRequest :: Generic CreateClassifierRequest _
instance showCreateClassifierRequest :: Show CreateClassifierRequest where
  show = genericShow
instance decodeCreateClassifierRequest :: Decode CreateClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClassifierRequest :: Encode CreateClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateClassifierResponse = CreateClassifierResponse Types.NoArguments
derive instance newtypeCreateClassifierResponse :: Newtype CreateClassifierResponse _
derive instance repGenericCreateClassifierResponse :: Generic CreateClassifierResponse _
instance showCreateClassifierResponse :: Show CreateClassifierResponse where
  show = genericShow
instance decodeCreateClassifierResponse :: Decode CreateClassifierResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateClassifierResponse :: Encode CreateClassifierResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateConnectionRequest = CreateConnectionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "ConnectionInput" :: (ConnectionInput)
  }
derive instance newtypeCreateConnectionRequest :: Newtype CreateConnectionRequest _
derive instance repGenericCreateConnectionRequest :: Generic CreateConnectionRequest _
instance showCreateConnectionRequest :: Show CreateConnectionRequest where
  show = genericShow
instance decodeCreateConnectionRequest :: Decode CreateConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConnectionRequest :: Encode CreateConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateConnectionResponse = CreateConnectionResponse Types.NoArguments
derive instance newtypeCreateConnectionResponse :: Newtype CreateConnectionResponse _
derive instance repGenericCreateConnectionResponse :: Generic CreateConnectionResponse _
instance showCreateConnectionResponse :: Show CreateConnectionResponse where
  show = genericShow
instance decodeCreateConnectionResponse :: Decode CreateConnectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConnectionResponse :: Encode CreateConnectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCrawlerRequest = CreateCrawlerRequest 
  { "Name" :: (NameString)
  , "Role" :: (Role)
  , "DatabaseName" :: (DatabaseName)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "Targets" :: (CrawlerTargets)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (CronExpression)
  , "Classifiers" :: NullOrUndefined.NullOrUndefined (ClassifierNameList)
  , "TablePrefix" :: NullOrUndefined.NullOrUndefined (TablePrefix)
  , "SchemaChangePolicy" :: NullOrUndefined.NullOrUndefined (SchemaChangePolicy)
  , "Configuration" :: NullOrUndefined.NullOrUndefined (CrawlerConfiguration)
  }
derive instance newtypeCreateCrawlerRequest :: Newtype CreateCrawlerRequest _
derive instance repGenericCreateCrawlerRequest :: Generic CreateCrawlerRequest _
instance showCreateCrawlerRequest :: Show CreateCrawlerRequest where
  show = genericShow
instance decodeCreateCrawlerRequest :: Decode CreateCrawlerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCrawlerRequest :: Encode CreateCrawlerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCrawlerResponse = CreateCrawlerResponse Types.NoArguments
derive instance newtypeCreateCrawlerResponse :: Newtype CreateCrawlerResponse _
derive instance repGenericCreateCrawlerResponse :: Generic CreateCrawlerResponse _
instance showCreateCrawlerResponse :: Show CreateCrawlerResponse where
  show = genericShow
instance decodeCreateCrawlerResponse :: Decode CreateCrawlerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCrawlerResponse :: Encode CreateCrawlerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDatabaseRequest = CreateDatabaseRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseInput" :: (DatabaseInput)
  }
derive instance newtypeCreateDatabaseRequest :: Newtype CreateDatabaseRequest _
derive instance repGenericCreateDatabaseRequest :: Generic CreateDatabaseRequest _
instance showCreateDatabaseRequest :: Show CreateDatabaseRequest where
  show = genericShow
instance decodeCreateDatabaseRequest :: Decode CreateDatabaseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDatabaseRequest :: Encode CreateDatabaseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDatabaseResponse = CreateDatabaseResponse Types.NoArguments
derive instance newtypeCreateDatabaseResponse :: Newtype CreateDatabaseResponse _
derive instance repGenericCreateDatabaseResponse :: Generic CreateDatabaseResponse _
instance showCreateDatabaseResponse :: Show CreateDatabaseResponse where
  show = genericShow
instance decodeCreateDatabaseResponse :: Decode CreateDatabaseResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDatabaseResponse :: Encode CreateDatabaseResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDevEndpointRequest = CreateDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  , "RoleArn" :: (RoleArn)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (StringList)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "PublicKey" :: (GenericString)
  , "NumberOfNodes" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  , "ExtraPythonLibsS3Path" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeCreateDevEndpointRequest :: Newtype CreateDevEndpointRequest _
derive instance repGenericCreateDevEndpointRequest :: Generic CreateDevEndpointRequest _
instance showCreateDevEndpointRequest :: Show CreateDevEndpointRequest where
  show = genericShow
instance decodeCreateDevEndpointRequest :: Decode CreateDevEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDevEndpointRequest :: Encode CreateDevEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDevEndpointResponse = CreateDevEndpointResponse 
  { "EndpointName" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "Status" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (StringList)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "YarnEndpointAddress" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ZeppelinRemoteSparkInterpreterPort" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  , "NumberOfNodes" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ExtraPythonLibsS3Path" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "FailureReason" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampValue)
  }
derive instance newtypeCreateDevEndpointResponse :: Newtype CreateDevEndpointResponse _
derive instance repGenericCreateDevEndpointResponse :: Generic CreateDevEndpointResponse _
instance showCreateDevEndpointResponse :: Show CreateDevEndpointResponse where
  show = genericShow
instance decodeCreateDevEndpointResponse :: Decode CreateDevEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDevEndpointResponse :: Encode CreateDevEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a <code>grok</code> classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateGrokClassifierRequest = CreateGrokClassifierRequest 
  { "Classification" :: (Classification)
  , "Name" :: (NameString)
  , "GrokPattern" :: (GrokPattern)
  , "CustomPatterns" :: NullOrUndefined.NullOrUndefined (CustomPatterns)
  }
derive instance newtypeCreateGrokClassifierRequest :: Newtype CreateGrokClassifierRequest _
derive instance repGenericCreateGrokClassifierRequest :: Generic CreateGrokClassifierRequest _
instance showCreateGrokClassifierRequest :: Show CreateGrokClassifierRequest where
  show = genericShow
instance decodeCreateGrokClassifierRequest :: Decode CreateGrokClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateGrokClassifierRequest :: Encode CreateGrokClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobRequest = CreateJobRequest 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (UriString)
  , "Role" :: (RoleString)
  , "ExecutionProperty" :: NullOrUndefined.NullOrUndefined (ExecutionProperty)
  , "Command" :: (JobCommand)
  , "DefaultArguments" :: NullOrUndefined.NullOrUndefined (GenericMap)
  , "Connections" :: NullOrUndefined.NullOrUndefined (ConnectionsList)
  , "MaxRetries" :: NullOrUndefined.NullOrUndefined (MaxRetries)
  , "AllocatedCapacity" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  }
derive instance newtypeCreateJobRequest :: Newtype CreateJobRequest _
derive instance repGenericCreateJobRequest :: Generic CreateJobRequest _
instance showCreateJobRequest :: Show CreateJobRequest where
  show = genericShow
instance decodeCreateJobRequest :: Decode CreateJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobRequest :: Encode CreateJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateJobResponse = CreateJobResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeCreateJobResponse :: Newtype CreateJobResponse _
derive instance repGenericCreateJobResponse :: Generic CreateJobResponse _
instance showCreateJobResponse :: Show CreateJobResponse where
  show = genericShow
instance decodeCreateJobResponse :: Decode CreateJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJobResponse :: Encode CreateJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a JSON classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateJsonClassifierRequest = CreateJsonClassifierRequest 
  { "Name" :: (NameString)
  , "JsonPath" :: (JsonPath)
  }
derive instance newtypeCreateJsonClassifierRequest :: Newtype CreateJsonClassifierRequest _
derive instance repGenericCreateJsonClassifierRequest :: Generic CreateJsonClassifierRequest _
instance showCreateJsonClassifierRequest :: Show CreateJsonClassifierRequest where
  show = genericShow
instance decodeCreateJsonClassifierRequest :: Decode CreateJsonClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateJsonClassifierRequest :: Encode CreateJsonClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePartitionRequest = CreatePartitionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionInput" :: (PartitionInput)
  }
derive instance newtypeCreatePartitionRequest :: Newtype CreatePartitionRequest _
derive instance repGenericCreatePartitionRequest :: Generic CreatePartitionRequest _
instance showCreatePartitionRequest :: Show CreatePartitionRequest where
  show = genericShow
instance decodeCreatePartitionRequest :: Decode CreatePartitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePartitionRequest :: Encode CreatePartitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatePartitionResponse = CreatePartitionResponse Types.NoArguments
derive instance newtypeCreatePartitionResponse :: Newtype CreatePartitionResponse _
derive instance repGenericCreatePartitionResponse :: Generic CreatePartitionResponse _
instance showCreatePartitionResponse :: Show CreatePartitionResponse where
  show = genericShow
instance decodeCreatePartitionResponse :: Decode CreatePartitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePartitionResponse :: Encode CreatePartitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateScriptRequest = CreateScriptRequest 
  { "DagNodes" :: NullOrUndefined.NullOrUndefined (DagNodes)
  , "DagEdges" :: NullOrUndefined.NullOrUndefined (DagEdges)
  , "Language" :: NullOrUndefined.NullOrUndefined (Language)
  }
derive instance newtypeCreateScriptRequest :: Newtype CreateScriptRequest _
derive instance repGenericCreateScriptRequest :: Generic CreateScriptRequest _
instance showCreateScriptRequest :: Show CreateScriptRequest where
  show = genericShow
instance decodeCreateScriptRequest :: Decode CreateScriptRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateScriptRequest :: Encode CreateScriptRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateScriptResponse = CreateScriptResponse 
  { "PythonScript" :: NullOrUndefined.NullOrUndefined (PythonScript)
  , "ScalaCode" :: NullOrUndefined.NullOrUndefined (ScalaCode)
  }
derive instance newtypeCreateScriptResponse :: Newtype CreateScriptResponse _
derive instance repGenericCreateScriptResponse :: Generic CreateScriptResponse _
instance showCreateScriptResponse :: Show CreateScriptResponse where
  show = genericShow
instance decodeCreateScriptResponse :: Decode CreateScriptResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateScriptResponse :: Encode CreateScriptResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTableRequest = CreateTableRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableInput" :: (TableInput)
  }
derive instance newtypeCreateTableRequest :: Newtype CreateTableRequest _
derive instance repGenericCreateTableRequest :: Generic CreateTableRequest _
instance showCreateTableRequest :: Show CreateTableRequest where
  show = genericShow
instance decodeCreateTableRequest :: Decode CreateTableRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTableRequest :: Encode CreateTableRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTableResponse = CreateTableResponse Types.NoArguments
derive instance newtypeCreateTableResponse :: Newtype CreateTableResponse _
derive instance repGenericCreateTableResponse :: Generic CreateTableResponse _
instance showCreateTableResponse :: Show CreateTableResponse where
  show = genericShow
instance decodeCreateTableResponse :: Decode CreateTableResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTableResponse :: Encode CreateTableResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTriggerRequest = CreateTriggerRequest 
  { "Name" :: (NameString)
  , "Type" :: (TriggerType)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "Predicate" :: NullOrUndefined.NullOrUndefined (Predicate)
  , "Actions" :: (ActionList)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  }
derive instance newtypeCreateTriggerRequest :: Newtype CreateTriggerRequest _
derive instance repGenericCreateTriggerRequest :: Generic CreateTriggerRequest _
instance showCreateTriggerRequest :: Show CreateTriggerRequest where
  show = genericShow
instance decodeCreateTriggerRequest :: Decode CreateTriggerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTriggerRequest :: Encode CreateTriggerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTriggerResponse = CreateTriggerResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeCreateTriggerResponse :: Newtype CreateTriggerResponse _
derive instance repGenericCreateTriggerResponse :: Generic CreateTriggerResponse _
instance showCreateTriggerResponse :: Show CreateTriggerResponse where
  show = genericShow
instance decodeCreateTriggerResponse :: Decode CreateTriggerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTriggerResponse :: Encode CreateTriggerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateUserDefinedFunctionRequest = CreateUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionInput" :: (UserDefinedFunctionInput)
  }
derive instance newtypeCreateUserDefinedFunctionRequest :: Newtype CreateUserDefinedFunctionRequest _
derive instance repGenericCreateUserDefinedFunctionRequest :: Generic CreateUserDefinedFunctionRequest _
instance showCreateUserDefinedFunctionRequest :: Show CreateUserDefinedFunctionRequest where
  show = genericShow
instance decodeCreateUserDefinedFunctionRequest :: Decode CreateUserDefinedFunctionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserDefinedFunctionRequest :: Encode CreateUserDefinedFunctionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateUserDefinedFunctionResponse = CreateUserDefinedFunctionResponse Types.NoArguments
derive instance newtypeCreateUserDefinedFunctionResponse :: Newtype CreateUserDefinedFunctionResponse _
derive instance repGenericCreateUserDefinedFunctionResponse :: Generic CreateUserDefinedFunctionResponse _
instance showCreateUserDefinedFunctionResponse :: Show CreateUserDefinedFunctionResponse where
  show = genericShow
instance decodeCreateUserDefinedFunctionResponse :: Decode CreateUserDefinedFunctionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserDefinedFunctionResponse :: Encode CreateUserDefinedFunctionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies an XML classifier for <code>CreateClassifier</code> to create.</p>
newtype CreateXMLClassifierRequest = CreateXMLClassifierRequest 
  { "Classification" :: (Classification)
  , "Name" :: (NameString)
  , "RowTag" :: NullOrUndefined.NullOrUndefined (RowTag)
  }
derive instance newtypeCreateXMLClassifierRequest :: Newtype CreateXMLClassifierRequest _
derive instance repGenericCreateXMLClassifierRequest :: Generic CreateXMLClassifierRequest _
instance showCreateXMLClassifierRequest :: Show CreateXMLClassifierRequest where
  show = genericShow
instance decodeCreateXMLClassifierRequest :: Decode CreateXMLClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateXMLClassifierRequest :: Encode CreateXMLClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CronExpression = CronExpression String
derive instance newtypeCronExpression :: Newtype CronExpression _
derive instance repGenericCronExpression :: Generic CronExpression _
instance showCronExpression :: Show CronExpression where
  show = genericShow
instance decodeCronExpression :: Decode CronExpression where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCronExpression :: Encode CronExpression where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomPatterns = CustomPatterns String
derive instance newtypeCustomPatterns :: Newtype CustomPatterns _
derive instance repGenericCustomPatterns :: Generic CustomPatterns _
instance showCustomPatterns :: Show CustomPatterns where
  show = genericShow
instance decodeCustomPatterns :: Decode CustomPatterns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomPatterns :: Encode CustomPatterns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DagEdges = DagEdges (Array CodeGenEdge)
derive instance newtypeDagEdges :: Newtype DagEdges _
derive instance repGenericDagEdges :: Generic DagEdges _
instance showDagEdges :: Show DagEdges where
  show = genericShow
instance decodeDagEdges :: Decode DagEdges where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDagEdges :: Encode DagEdges where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DagNodes = DagNodes (Array CodeGenNode)
derive instance newtypeDagNodes :: Newtype DagNodes _
derive instance repGenericDagNodes :: Generic DagNodes _
instance showDagNodes :: Show DagNodes where
  show = genericShow
instance decodeDagNodes :: Decode DagNodes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDagNodes :: Encode DagNodes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <code>Database</code> object represents a logical grouping of tables that may reside in a Hive metastore or an RDBMS.</p>
newtype Database = Database 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "LocationUri" :: NullOrUndefined.NullOrUndefined (URI)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "CreateTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeDatabase :: Newtype Database _
derive instance repGenericDatabase :: Generic Database _
instance showDatabase :: Show Database where
  show = genericShow
instance decodeDatabase :: Decode Database where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatabase :: Encode Database where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The structure used to create or update a database.</p>
newtype DatabaseInput = DatabaseInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "LocationUri" :: NullOrUndefined.NullOrUndefined (URI)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  }
derive instance newtypeDatabaseInput :: Newtype DatabaseInput _
derive instance repGenericDatabaseInput :: Generic DatabaseInput _
instance showDatabaseInput :: Show DatabaseInput where
  show = genericShow
instance decodeDatabaseInput :: Decode DatabaseInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatabaseInput :: Encode DatabaseInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DatabaseList = DatabaseList (Array Database)
derive instance newtypeDatabaseList :: Newtype DatabaseList _
derive instance repGenericDatabaseList :: Generic DatabaseList _
instance showDatabaseList :: Show DatabaseList where
  show = genericShow
instance decodeDatabaseList :: Decode DatabaseList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatabaseList :: Encode DatabaseList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DatabaseName = DatabaseName String
derive instance newtypeDatabaseName :: Newtype DatabaseName _
derive instance repGenericDatabaseName :: Generic DatabaseName _
instance showDatabaseName :: Show DatabaseName where
  show = genericShow
instance decodeDatabaseName :: Decode DatabaseName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDatabaseName :: Encode DatabaseName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteBehavior = DeleteBehavior String
derive instance newtypeDeleteBehavior :: Newtype DeleteBehavior _
derive instance repGenericDeleteBehavior :: Generic DeleteBehavior _
instance showDeleteBehavior :: Show DeleteBehavior where
  show = genericShow
instance decodeDeleteBehavior :: Decode DeleteBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteBehavior :: Encode DeleteBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteClassifierRequest = DeleteClassifierRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeDeleteClassifierRequest :: Newtype DeleteClassifierRequest _
derive instance repGenericDeleteClassifierRequest :: Generic DeleteClassifierRequest _
instance showDeleteClassifierRequest :: Show DeleteClassifierRequest where
  show = genericShow
instance decodeDeleteClassifierRequest :: Decode DeleteClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClassifierRequest :: Encode DeleteClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteClassifierResponse = DeleteClassifierResponse Types.NoArguments
derive instance newtypeDeleteClassifierResponse :: Newtype DeleteClassifierResponse _
derive instance repGenericDeleteClassifierResponse :: Generic DeleteClassifierResponse _
instance showDeleteClassifierResponse :: Show DeleteClassifierResponse where
  show = genericShow
instance decodeDeleteClassifierResponse :: Decode DeleteClassifierResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteClassifierResponse :: Encode DeleteClassifierResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteConnectionNameList = DeleteConnectionNameList (Array NameString)
derive instance newtypeDeleteConnectionNameList :: Newtype DeleteConnectionNameList _
derive instance repGenericDeleteConnectionNameList :: Generic DeleteConnectionNameList _
instance showDeleteConnectionNameList :: Show DeleteConnectionNameList where
  show = genericShow
instance decodeDeleteConnectionNameList :: Decode DeleteConnectionNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConnectionNameList :: Encode DeleteConnectionNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteConnectionRequest = DeleteConnectionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "ConnectionName" :: (NameString)
  }
derive instance newtypeDeleteConnectionRequest :: Newtype DeleteConnectionRequest _
derive instance repGenericDeleteConnectionRequest :: Generic DeleteConnectionRequest _
instance showDeleteConnectionRequest :: Show DeleteConnectionRequest where
  show = genericShow
instance decodeDeleteConnectionRequest :: Decode DeleteConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConnectionRequest :: Encode DeleteConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteConnectionResponse = DeleteConnectionResponse Types.NoArguments
derive instance newtypeDeleteConnectionResponse :: Newtype DeleteConnectionResponse _
derive instance repGenericDeleteConnectionResponse :: Generic DeleteConnectionResponse _
instance showDeleteConnectionResponse :: Show DeleteConnectionResponse where
  show = genericShow
instance decodeDeleteConnectionResponse :: Decode DeleteConnectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConnectionResponse :: Encode DeleteConnectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCrawlerRequest = DeleteCrawlerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeDeleteCrawlerRequest :: Newtype DeleteCrawlerRequest _
derive instance repGenericDeleteCrawlerRequest :: Generic DeleteCrawlerRequest _
instance showDeleteCrawlerRequest :: Show DeleteCrawlerRequest where
  show = genericShow
instance decodeDeleteCrawlerRequest :: Decode DeleteCrawlerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCrawlerRequest :: Encode DeleteCrawlerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCrawlerResponse = DeleteCrawlerResponse Types.NoArguments
derive instance newtypeDeleteCrawlerResponse :: Newtype DeleteCrawlerResponse _
derive instance repGenericDeleteCrawlerResponse :: Generic DeleteCrawlerResponse _
instance showDeleteCrawlerResponse :: Show DeleteCrawlerResponse where
  show = genericShow
instance decodeDeleteCrawlerResponse :: Decode DeleteCrawlerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCrawlerResponse :: Encode DeleteCrawlerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDatabaseRequest = DeleteDatabaseRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }
derive instance newtypeDeleteDatabaseRequest :: Newtype DeleteDatabaseRequest _
derive instance repGenericDeleteDatabaseRequest :: Generic DeleteDatabaseRequest _
instance showDeleteDatabaseRequest :: Show DeleteDatabaseRequest where
  show = genericShow
instance decodeDeleteDatabaseRequest :: Decode DeleteDatabaseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDatabaseRequest :: Encode DeleteDatabaseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDatabaseResponse = DeleteDatabaseResponse Types.NoArguments
derive instance newtypeDeleteDatabaseResponse :: Newtype DeleteDatabaseResponse _
derive instance repGenericDeleteDatabaseResponse :: Generic DeleteDatabaseResponse _
instance showDeleteDatabaseResponse :: Show DeleteDatabaseResponse where
  show = genericShow
instance decodeDeleteDatabaseResponse :: Decode DeleteDatabaseResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDatabaseResponse :: Encode DeleteDatabaseResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDevEndpointRequest = DeleteDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  }
derive instance newtypeDeleteDevEndpointRequest :: Newtype DeleteDevEndpointRequest _
derive instance repGenericDeleteDevEndpointRequest :: Generic DeleteDevEndpointRequest _
instance showDeleteDevEndpointRequest :: Show DeleteDevEndpointRequest where
  show = genericShow
instance decodeDeleteDevEndpointRequest :: Decode DeleteDevEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDevEndpointRequest :: Encode DeleteDevEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDevEndpointResponse = DeleteDevEndpointResponse Types.NoArguments
derive instance newtypeDeleteDevEndpointResponse :: Newtype DeleteDevEndpointResponse _
derive instance repGenericDeleteDevEndpointResponse :: Generic DeleteDevEndpointResponse _
instance showDeleteDevEndpointResponse :: Show DeleteDevEndpointResponse where
  show = genericShow
instance decodeDeleteDevEndpointResponse :: Decode DeleteDevEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDevEndpointResponse :: Encode DeleteDevEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteJobRequest = DeleteJobRequest 
  { "JobName" :: (NameString)
  }
derive instance newtypeDeleteJobRequest :: Newtype DeleteJobRequest _
derive instance repGenericDeleteJobRequest :: Generic DeleteJobRequest _
instance showDeleteJobRequest :: Show DeleteJobRequest where
  show = genericShow
instance decodeDeleteJobRequest :: Decode DeleteJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteJobRequest :: Encode DeleteJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteJobResponse = DeleteJobResponse 
  { "JobName" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeDeleteJobResponse :: Newtype DeleteJobResponse _
derive instance repGenericDeleteJobResponse :: Generic DeleteJobResponse _
instance showDeleteJobResponse :: Show DeleteJobResponse where
  show = genericShow
instance decodeDeleteJobResponse :: Decode DeleteJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteJobResponse :: Encode DeleteJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePartitionRequest = DeletePartitionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValues" :: (ValueStringList)
  }
derive instance newtypeDeletePartitionRequest :: Newtype DeletePartitionRequest _
derive instance repGenericDeletePartitionRequest :: Generic DeletePartitionRequest _
instance showDeletePartitionRequest :: Show DeletePartitionRequest where
  show = genericShow
instance decodeDeletePartitionRequest :: Decode DeletePartitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePartitionRequest :: Encode DeletePartitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeletePartitionResponse = DeletePartitionResponse Types.NoArguments
derive instance newtypeDeletePartitionResponse :: Newtype DeletePartitionResponse _
derive instance repGenericDeletePartitionResponse :: Generic DeletePartitionResponse _
instance showDeletePartitionResponse :: Show DeletePartitionResponse where
  show = genericShow
instance decodeDeletePartitionResponse :: Decode DeletePartitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePartitionResponse :: Encode DeletePartitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTableRequest = DeleteTableRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Name" :: (NameString)
  }
derive instance newtypeDeleteTableRequest :: Newtype DeleteTableRequest _
derive instance repGenericDeleteTableRequest :: Generic DeleteTableRequest _
instance showDeleteTableRequest :: Show DeleteTableRequest where
  show = genericShow
instance decodeDeleteTableRequest :: Decode DeleteTableRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTableRequest :: Encode DeleteTableRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTableResponse = DeleteTableResponse Types.NoArguments
derive instance newtypeDeleteTableResponse :: Newtype DeleteTableResponse _
derive instance repGenericDeleteTableResponse :: Generic DeleteTableResponse _
instance showDeleteTableResponse :: Show DeleteTableResponse where
  show = genericShow
instance decodeDeleteTableResponse :: Decode DeleteTableResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTableResponse :: Encode DeleteTableResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTableVersionRequest = DeleteTableVersionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionId" :: (VersionString)
  }
derive instance newtypeDeleteTableVersionRequest :: Newtype DeleteTableVersionRequest _
derive instance repGenericDeleteTableVersionRequest :: Generic DeleteTableVersionRequest _
instance showDeleteTableVersionRequest :: Show DeleteTableVersionRequest where
  show = genericShow
instance decodeDeleteTableVersionRequest :: Decode DeleteTableVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTableVersionRequest :: Encode DeleteTableVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTableVersionResponse = DeleteTableVersionResponse Types.NoArguments
derive instance newtypeDeleteTableVersionResponse :: Newtype DeleteTableVersionResponse _
derive instance repGenericDeleteTableVersionResponse :: Generic DeleteTableVersionResponse _
instance showDeleteTableVersionResponse :: Show DeleteTableVersionResponse where
  show = genericShow
instance decodeDeleteTableVersionResponse :: Decode DeleteTableVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTableVersionResponse :: Encode DeleteTableVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTriggerRequest = DeleteTriggerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeDeleteTriggerRequest :: Newtype DeleteTriggerRequest _
derive instance repGenericDeleteTriggerRequest :: Generic DeleteTriggerRequest _
instance showDeleteTriggerRequest :: Show DeleteTriggerRequest where
  show = genericShow
instance decodeDeleteTriggerRequest :: Decode DeleteTriggerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTriggerRequest :: Encode DeleteTriggerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTriggerResponse = DeleteTriggerResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeDeleteTriggerResponse :: Newtype DeleteTriggerResponse _
derive instance repGenericDeleteTriggerResponse :: Generic DeleteTriggerResponse _
instance showDeleteTriggerResponse :: Show DeleteTriggerResponse where
  show = genericShow
instance decodeDeleteTriggerResponse :: Decode DeleteTriggerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTriggerResponse :: Encode DeleteTriggerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteUserDefinedFunctionRequest = DeleteUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  }
derive instance newtypeDeleteUserDefinedFunctionRequest :: Newtype DeleteUserDefinedFunctionRequest _
derive instance repGenericDeleteUserDefinedFunctionRequest :: Generic DeleteUserDefinedFunctionRequest _
instance showDeleteUserDefinedFunctionRequest :: Show DeleteUserDefinedFunctionRequest where
  show = genericShow
instance decodeDeleteUserDefinedFunctionRequest :: Decode DeleteUserDefinedFunctionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserDefinedFunctionRequest :: Encode DeleteUserDefinedFunctionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse Types.NoArguments
derive instance newtypeDeleteUserDefinedFunctionResponse :: Newtype DeleteUserDefinedFunctionResponse _
derive instance repGenericDeleteUserDefinedFunctionResponse :: Generic DeleteUserDefinedFunctionResponse _
instance showDeleteUserDefinedFunctionResponse :: Show DeleteUserDefinedFunctionResponse where
  show = genericShow
instance decodeDeleteUserDefinedFunctionResponse :: Decode DeleteUserDefinedFunctionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserDefinedFunctionResponse :: Encode DeleteUserDefinedFunctionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescriptionString = DescriptionString String
derive instance newtypeDescriptionString :: Newtype DescriptionString _
derive instance repGenericDescriptionString :: Generic DescriptionString _
instance showDescriptionString :: Show DescriptionString where
  show = genericShow
instance decodeDescriptionString :: Decode DescriptionString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescriptionString :: Encode DescriptionString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescriptionStringRemovable = DescriptionStringRemovable String
derive instance newtypeDescriptionStringRemovable :: Newtype DescriptionStringRemovable _
derive instance repGenericDescriptionStringRemovable :: Generic DescriptionStringRemovable _
instance showDescriptionStringRemovable :: Show DescriptionStringRemovable where
  show = genericShow
instance decodeDescriptionStringRemovable :: Decode DescriptionStringRemovable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescriptionStringRemovable :: Encode DescriptionStringRemovable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A development endpoint where a developer can remotely debug ETL scripts.</p>
newtype DevEndpoint = DevEndpoint 
  { "EndpointName" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "RoleArn" :: NullOrUndefined.NullOrUndefined (RoleArn)
  , "SecurityGroupIds" :: NullOrUndefined.NullOrUndefined (StringList)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "YarnEndpointAddress" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ZeppelinRemoteSparkInterpreterPort" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  , "PublicAddress" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "Status" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "NumberOfNodes" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ExtraPythonLibsS3Path" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "FailureReason" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "LastUpdateStatus" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampValue)
  , "LastModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampValue)
  , "PublicKey" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDevEndpoint :: Newtype DevEndpoint _
derive instance repGenericDevEndpoint :: Generic DevEndpoint _
instance showDevEndpoint :: Show DevEndpoint where
  show = genericShow
instance decodeDevEndpoint :: Decode DevEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevEndpoint :: Encode DevEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Custom libraries to be loaded into a DevEndpoint.</p>
newtype DevEndpointCustomLibraries = DevEndpointCustomLibraries 
  { "ExtraPythonLibsS3Path" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ExtraJarsS3Path" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeDevEndpointCustomLibraries :: Newtype DevEndpointCustomLibraries _
derive instance repGenericDevEndpointCustomLibraries :: Generic DevEndpointCustomLibraries _
instance showDevEndpointCustomLibraries :: Show DevEndpointCustomLibraries where
  show = genericShow
instance decodeDevEndpointCustomLibraries :: Decode DevEndpointCustomLibraries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevEndpointCustomLibraries :: Encode DevEndpointCustomLibraries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DevEndpointList = DevEndpointList (Array DevEndpoint)
derive instance newtypeDevEndpointList :: Newtype DevEndpointList _
derive instance repGenericDevEndpointList :: Generic DevEndpointList _
instance showDevEndpointList :: Show DevEndpointList where
  show = genericShow
instance decodeDevEndpointList :: Decode DevEndpointList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDevEndpointList :: Encode DevEndpointList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A specified entity does not exist</p>
newtype EntityNotFoundException = EntityNotFoundException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeEntityNotFoundException :: Newtype EntityNotFoundException _
derive instance repGenericEntityNotFoundException :: Generic EntityNotFoundException _
instance showEntityNotFoundException :: Show EntityNotFoundException where
  show = genericShow
instance decodeEntityNotFoundException :: Decode EntityNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityNotFoundException :: Encode EntityNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorByName = ErrorByName (StrMap.StrMap ErrorDetail)
derive instance newtypeErrorByName :: Newtype ErrorByName _
derive instance repGenericErrorByName :: Generic ErrorByName _
instance showErrorByName :: Show ErrorByName where
  show = genericShow
instance decodeErrorByName :: Decode ErrorByName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorByName :: Encode ErrorByName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an error.</p>
newtype ErrorDetail = ErrorDetail 
  { "ErrorCode" :: NullOrUndefined.NullOrUndefined (NameString)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  }
derive instance newtypeErrorDetail :: Newtype ErrorDetail _
derive instance repGenericErrorDetail :: Generic ErrorDetail _
instance showErrorDetail :: Show ErrorDetail where
  show = genericShow
instance decodeErrorDetail :: Decode ErrorDetail where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorDetail :: Encode ErrorDetail where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorString = ErrorString String
derive instance newtypeErrorString :: Newtype ErrorString _
derive instance repGenericErrorString :: Generic ErrorString _
instance showErrorString :: Show ErrorString where
  show = genericShow
instance decodeErrorString :: Decode ErrorString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorString :: Encode ErrorString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An execution property of a job.</p>
newtype ExecutionProperty = ExecutionProperty 
  { "MaxConcurrentRuns" :: NullOrUndefined.NullOrUndefined (MaxConcurrentRuns)
  }
derive instance newtypeExecutionProperty :: Newtype ExecutionProperty _
derive instance repGenericExecutionProperty :: Generic ExecutionProperty _
instance showExecutionProperty :: Show ExecutionProperty where
  show = genericShow
instance decodeExecutionProperty :: Decode ExecutionProperty where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionProperty :: Encode ExecutionProperty where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FieldType = FieldType String
derive instance newtypeFieldType :: Newtype FieldType _
derive instance repGenericFieldType :: Generic FieldType _
instance showFieldType :: Show FieldType where
  show = genericShow
instance decodeFieldType :: Decode FieldType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFieldType :: Encode FieldType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FilterString = FilterString String
derive instance newtypeFilterString :: Newtype FilterString _
derive instance repGenericFilterString :: Generic FilterString _
instance showFilterString :: Show FilterString where
  show = genericShow
instance decodeFilterString :: Decode FilterString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFilterString :: Encode FilterString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FormatString = FormatString String
derive instance newtypeFormatString :: Newtype FormatString _
derive instance repGenericFormatString :: Generic FormatString _
instance showFormatString :: Show FormatString where
  show = genericShow
instance decodeFormatString :: Decode FormatString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFormatString :: Encode FormatString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenericMap = GenericMap (StrMap.StrMap GenericString)
derive instance newtypeGenericMap :: Newtype GenericMap _
derive instance repGenericGenericMap :: Generic GenericMap _
instance showGenericMap :: Show GenericMap where
  show = genericShow
instance decodeGenericMap :: Decode GenericMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenericMap :: Encode GenericMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GenericString = GenericString String
derive instance newtypeGenericString :: Newtype GenericString _
derive instance repGenericGenericString :: Generic GenericString _
instance showGenericString :: Show GenericString where
  show = genericShow
instance decodeGenericString :: Decode GenericString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGenericString :: Encode GenericString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCatalogImportStatusRequest = GetCatalogImportStatusRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  }
derive instance newtypeGetCatalogImportStatusRequest :: Newtype GetCatalogImportStatusRequest _
derive instance repGenericGetCatalogImportStatusRequest :: Generic GetCatalogImportStatusRequest _
instance showGetCatalogImportStatusRequest :: Show GetCatalogImportStatusRequest where
  show = genericShow
instance decodeGetCatalogImportStatusRequest :: Decode GetCatalogImportStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCatalogImportStatusRequest :: Encode GetCatalogImportStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCatalogImportStatusResponse = GetCatalogImportStatusResponse 
  { "ImportStatus" :: NullOrUndefined.NullOrUndefined (CatalogImportStatus)
  }
derive instance newtypeGetCatalogImportStatusResponse :: Newtype GetCatalogImportStatusResponse _
derive instance repGenericGetCatalogImportStatusResponse :: Generic GetCatalogImportStatusResponse _
instance showGetCatalogImportStatusResponse :: Show GetCatalogImportStatusResponse where
  show = genericShow
instance decodeGetCatalogImportStatusResponse :: Decode GetCatalogImportStatusResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCatalogImportStatusResponse :: Encode GetCatalogImportStatusResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetClassifierRequest = GetClassifierRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeGetClassifierRequest :: Newtype GetClassifierRequest _
derive instance repGenericGetClassifierRequest :: Generic GetClassifierRequest _
instance showGetClassifierRequest :: Show GetClassifierRequest where
  show = genericShow
instance decodeGetClassifierRequest :: Decode GetClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetClassifierRequest :: Encode GetClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetClassifierResponse = GetClassifierResponse 
  { "Classifier" :: NullOrUndefined.NullOrUndefined (Classifier)
  }
derive instance newtypeGetClassifierResponse :: Newtype GetClassifierResponse _
derive instance repGenericGetClassifierResponse :: Generic GetClassifierResponse _
instance showGetClassifierResponse :: Show GetClassifierResponse where
  show = genericShow
instance decodeGetClassifierResponse :: Decode GetClassifierResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetClassifierResponse :: Encode GetClassifierResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetClassifiersRequest = GetClassifiersRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetClassifiersRequest :: Newtype GetClassifiersRequest _
derive instance repGenericGetClassifiersRequest :: Generic GetClassifiersRequest _
instance showGetClassifiersRequest :: Show GetClassifiersRequest where
  show = genericShow
instance decodeGetClassifiersRequest :: Decode GetClassifiersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetClassifiersRequest :: Encode GetClassifiersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetClassifiersResponse = GetClassifiersResponse 
  { "Classifiers" :: NullOrUndefined.NullOrUndefined (ClassifierList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetClassifiersResponse :: Newtype GetClassifiersResponse _
derive instance repGenericGetClassifiersResponse :: Generic GetClassifiersResponse _
instance showGetClassifiersResponse :: Show GetClassifiersResponse where
  show = genericShow
instance decodeGetClassifiersResponse :: Decode GetClassifiersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetClassifiersResponse :: Encode GetClassifiersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConnectionRequest = GetConnectionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }
derive instance newtypeGetConnectionRequest :: Newtype GetConnectionRequest _
derive instance repGenericGetConnectionRequest :: Generic GetConnectionRequest _
instance showGetConnectionRequest :: Show GetConnectionRequest where
  show = genericShow
instance decodeGetConnectionRequest :: Decode GetConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectionRequest :: Encode GetConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConnectionResponse = GetConnectionResponse 
  { "Connection" :: NullOrUndefined.NullOrUndefined (Connection)
  }
derive instance newtypeGetConnectionResponse :: Newtype GetConnectionResponse _
derive instance repGenericGetConnectionResponse :: Generic GetConnectionResponse _
instance showGetConnectionResponse :: Show GetConnectionResponse where
  show = genericShow
instance decodeGetConnectionResponse :: Decode GetConnectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectionResponse :: Encode GetConnectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Filters the connection definitions returned by the <code>GetConnections</code> API.</p>
newtype GetConnectionsFilter = GetConnectionsFilter 
  { "MatchCriteria" :: NullOrUndefined.NullOrUndefined (MatchCriteria)
  , "ConnectionType" :: NullOrUndefined.NullOrUndefined (ConnectionType)
  }
derive instance newtypeGetConnectionsFilter :: Newtype GetConnectionsFilter _
derive instance repGenericGetConnectionsFilter :: Generic GetConnectionsFilter _
instance showGetConnectionsFilter :: Show GetConnectionsFilter where
  show = genericShow
instance decodeGetConnectionsFilter :: Decode GetConnectionsFilter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectionsFilter :: Encode GetConnectionsFilter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConnectionsRequest = GetConnectionsRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "Filter" :: NullOrUndefined.NullOrUndefined (GetConnectionsFilter)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetConnectionsRequest :: Newtype GetConnectionsRequest _
derive instance repGenericGetConnectionsRequest :: Generic GetConnectionsRequest _
instance showGetConnectionsRequest :: Show GetConnectionsRequest where
  show = genericShow
instance decodeGetConnectionsRequest :: Decode GetConnectionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectionsRequest :: Encode GetConnectionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetConnectionsResponse = GetConnectionsResponse 
  { "ConnectionList" :: NullOrUndefined.NullOrUndefined (ConnectionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetConnectionsResponse :: Newtype GetConnectionsResponse _
derive instance repGenericGetConnectionsResponse :: Generic GetConnectionsResponse _
instance showGetConnectionsResponse :: Show GetConnectionsResponse where
  show = genericShow
instance decodeGetConnectionsResponse :: Decode GetConnectionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetConnectionsResponse :: Encode GetConnectionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCrawlerMetricsRequest = GetCrawlerMetricsRequest 
  { "CrawlerNameList" :: NullOrUndefined.NullOrUndefined (CrawlerNameList)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetCrawlerMetricsRequest :: Newtype GetCrawlerMetricsRequest _
derive instance repGenericGetCrawlerMetricsRequest :: Generic GetCrawlerMetricsRequest _
instance showGetCrawlerMetricsRequest :: Show GetCrawlerMetricsRequest where
  show = genericShow
instance decodeGetCrawlerMetricsRequest :: Decode GetCrawlerMetricsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCrawlerMetricsRequest :: Encode GetCrawlerMetricsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCrawlerMetricsResponse = GetCrawlerMetricsResponse 
  { "CrawlerMetricsList" :: NullOrUndefined.NullOrUndefined (CrawlerMetricsList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetCrawlerMetricsResponse :: Newtype GetCrawlerMetricsResponse _
derive instance repGenericGetCrawlerMetricsResponse :: Generic GetCrawlerMetricsResponse _
instance showGetCrawlerMetricsResponse :: Show GetCrawlerMetricsResponse where
  show = genericShow
instance decodeGetCrawlerMetricsResponse :: Decode GetCrawlerMetricsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCrawlerMetricsResponse :: Encode GetCrawlerMetricsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCrawlerRequest = GetCrawlerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeGetCrawlerRequest :: Newtype GetCrawlerRequest _
derive instance repGenericGetCrawlerRequest :: Generic GetCrawlerRequest _
instance showGetCrawlerRequest :: Show GetCrawlerRequest where
  show = genericShow
instance decodeGetCrawlerRequest :: Decode GetCrawlerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCrawlerRequest :: Encode GetCrawlerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCrawlerResponse = GetCrawlerResponse 
  { "Crawler" :: NullOrUndefined.NullOrUndefined (Crawler)
  }
derive instance newtypeGetCrawlerResponse :: Newtype GetCrawlerResponse _
derive instance repGenericGetCrawlerResponse :: Generic GetCrawlerResponse _
instance showGetCrawlerResponse :: Show GetCrawlerResponse where
  show = genericShow
instance decodeGetCrawlerResponse :: Decode GetCrawlerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCrawlerResponse :: Encode GetCrawlerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCrawlersRequest = GetCrawlersRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetCrawlersRequest :: Newtype GetCrawlersRequest _
derive instance repGenericGetCrawlersRequest :: Generic GetCrawlersRequest _
instance showGetCrawlersRequest :: Show GetCrawlersRequest where
  show = genericShow
instance decodeGetCrawlersRequest :: Decode GetCrawlersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCrawlersRequest :: Encode GetCrawlersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCrawlersResponse = GetCrawlersResponse 
  { "Crawlers" :: NullOrUndefined.NullOrUndefined (CrawlerList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetCrawlersResponse :: Newtype GetCrawlersResponse _
derive instance repGenericGetCrawlersResponse :: Generic GetCrawlersResponse _
instance showGetCrawlersResponse :: Show GetCrawlersResponse where
  show = genericShow
instance decodeGetCrawlersResponse :: Decode GetCrawlersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCrawlersResponse :: Encode GetCrawlersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDatabaseRequest = GetDatabaseRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  }
derive instance newtypeGetDatabaseRequest :: Newtype GetDatabaseRequest _
derive instance repGenericGetDatabaseRequest :: Generic GetDatabaseRequest _
instance showGetDatabaseRequest :: Show GetDatabaseRequest where
  show = genericShow
instance decodeGetDatabaseRequest :: Decode GetDatabaseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDatabaseRequest :: Encode GetDatabaseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDatabaseResponse = GetDatabaseResponse 
  { "Database" :: NullOrUndefined.NullOrUndefined (Database)
  }
derive instance newtypeGetDatabaseResponse :: Newtype GetDatabaseResponse _
derive instance repGenericGetDatabaseResponse :: Generic GetDatabaseResponse _
instance showGetDatabaseResponse :: Show GetDatabaseResponse where
  show = genericShow
instance decodeGetDatabaseResponse :: Decode GetDatabaseResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDatabaseResponse :: Encode GetDatabaseResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDatabasesRequest = GetDatabasesRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetDatabasesRequest :: Newtype GetDatabasesRequest _
derive instance repGenericGetDatabasesRequest :: Generic GetDatabasesRequest _
instance showGetDatabasesRequest :: Show GetDatabasesRequest where
  show = genericShow
instance decodeGetDatabasesRequest :: Decode GetDatabasesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDatabasesRequest :: Encode GetDatabasesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDatabasesResponse = GetDatabasesResponse 
  { "DatabaseList" :: (DatabaseList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetDatabasesResponse :: Newtype GetDatabasesResponse _
derive instance repGenericGetDatabasesResponse :: Generic GetDatabasesResponse _
instance showGetDatabasesResponse :: Show GetDatabasesResponse where
  show = genericShow
instance decodeGetDatabasesResponse :: Decode GetDatabasesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDatabasesResponse :: Encode GetDatabasesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDataflowGraphRequest = GetDataflowGraphRequest 
  { "PythonScript" :: NullOrUndefined.NullOrUndefined (PythonScript)
  }
derive instance newtypeGetDataflowGraphRequest :: Newtype GetDataflowGraphRequest _
derive instance repGenericGetDataflowGraphRequest :: Generic GetDataflowGraphRequest _
instance showGetDataflowGraphRequest :: Show GetDataflowGraphRequest where
  show = genericShow
instance decodeGetDataflowGraphRequest :: Decode GetDataflowGraphRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDataflowGraphRequest :: Encode GetDataflowGraphRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDataflowGraphResponse = GetDataflowGraphResponse 
  { "DagNodes" :: NullOrUndefined.NullOrUndefined (DagNodes)
  , "DagEdges" :: NullOrUndefined.NullOrUndefined (DagEdges)
  }
derive instance newtypeGetDataflowGraphResponse :: Newtype GetDataflowGraphResponse _
derive instance repGenericGetDataflowGraphResponse :: Generic GetDataflowGraphResponse _
instance showGetDataflowGraphResponse :: Show GetDataflowGraphResponse where
  show = genericShow
instance decodeGetDataflowGraphResponse :: Decode GetDataflowGraphResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDataflowGraphResponse :: Encode GetDataflowGraphResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDevEndpointRequest = GetDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  }
derive instance newtypeGetDevEndpointRequest :: Newtype GetDevEndpointRequest _
derive instance repGenericGetDevEndpointRequest :: Generic GetDevEndpointRequest _
instance showGetDevEndpointRequest :: Show GetDevEndpointRequest where
  show = genericShow
instance decodeGetDevEndpointRequest :: Decode GetDevEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDevEndpointRequest :: Encode GetDevEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDevEndpointResponse = GetDevEndpointResponse 
  { "DevEndpoint" :: NullOrUndefined.NullOrUndefined (DevEndpoint)
  }
derive instance newtypeGetDevEndpointResponse :: Newtype GetDevEndpointResponse _
derive instance repGenericGetDevEndpointResponse :: Generic GetDevEndpointResponse _
instance showGetDevEndpointResponse :: Show GetDevEndpointResponse where
  show = genericShow
instance decodeGetDevEndpointResponse :: Decode GetDevEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDevEndpointResponse :: Encode GetDevEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDevEndpointsRequest = GetDevEndpointsRequest 
  { "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeGetDevEndpointsRequest :: Newtype GetDevEndpointsRequest _
derive instance repGenericGetDevEndpointsRequest :: Generic GetDevEndpointsRequest _
instance showGetDevEndpointsRequest :: Show GetDevEndpointsRequest where
  show = genericShow
instance decodeGetDevEndpointsRequest :: Decode GetDevEndpointsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDevEndpointsRequest :: Encode GetDevEndpointsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDevEndpointsResponse = GetDevEndpointsResponse 
  { "DevEndpoints" :: NullOrUndefined.NullOrUndefined (DevEndpointList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeGetDevEndpointsResponse :: Newtype GetDevEndpointsResponse _
derive instance repGenericGetDevEndpointsResponse :: Generic GetDevEndpointsResponse _
instance showGetDevEndpointsResponse :: Show GetDevEndpointsResponse where
  show = genericShow
instance decodeGetDevEndpointsResponse :: Decode GetDevEndpointsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDevEndpointsResponse :: Encode GetDevEndpointsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobRequest = GetJobRequest 
  { "JobName" :: (NameString)
  }
derive instance newtypeGetJobRequest :: Newtype GetJobRequest _
derive instance repGenericGetJobRequest :: Generic GetJobRequest _
instance showGetJobRequest :: Show GetJobRequest where
  show = genericShow
instance decodeGetJobRequest :: Decode GetJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobRequest :: Encode GetJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobResponse = GetJobResponse 
  { "Job" :: NullOrUndefined.NullOrUndefined (Job)
  }
derive instance newtypeGetJobResponse :: Newtype GetJobResponse _
derive instance repGenericGetJobResponse :: Generic GetJobResponse _
instance showGetJobResponse :: Show GetJobResponse where
  show = genericShow
instance decodeGetJobResponse :: Decode GetJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobResponse :: Encode GetJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobRunRequest = GetJobRunRequest 
  { "JobName" :: (NameString)
  , "RunId" :: (IdString)
  , "PredecessorsIncluded" :: NullOrUndefined.NullOrUndefined (BooleanValue)
  }
derive instance newtypeGetJobRunRequest :: Newtype GetJobRunRequest _
derive instance repGenericGetJobRunRequest :: Generic GetJobRunRequest _
instance showGetJobRunRequest :: Show GetJobRunRequest where
  show = genericShow
instance decodeGetJobRunRequest :: Decode GetJobRunRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobRunRequest :: Encode GetJobRunRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobRunResponse = GetJobRunResponse 
  { "JobRun" :: NullOrUndefined.NullOrUndefined (JobRun)
  }
derive instance newtypeGetJobRunResponse :: Newtype GetJobRunResponse _
derive instance repGenericGetJobRunResponse :: Generic GetJobRunResponse _
instance showGetJobRunResponse :: Show GetJobRunResponse where
  show = genericShow
instance decodeGetJobRunResponse :: Decode GetJobRunResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobRunResponse :: Encode GetJobRunResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobRunsRequest = GetJobRunsRequest 
  { "JobName" :: (NameString)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetJobRunsRequest :: Newtype GetJobRunsRequest _
derive instance repGenericGetJobRunsRequest :: Generic GetJobRunsRequest _
instance showGetJobRunsRequest :: Show GetJobRunsRequest where
  show = genericShow
instance decodeGetJobRunsRequest :: Decode GetJobRunsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobRunsRequest :: Encode GetJobRunsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobRunsResponse = GetJobRunsResponse 
  { "JobRuns" :: NullOrUndefined.NullOrUndefined (JobRunList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeGetJobRunsResponse :: Newtype GetJobRunsResponse _
derive instance repGenericGetJobRunsResponse :: Generic GetJobRunsResponse _
instance showGetJobRunsResponse :: Show GetJobRunsResponse where
  show = genericShow
instance decodeGetJobRunsResponse :: Decode GetJobRunsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobRunsResponse :: Encode GetJobRunsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobsRequest = GetJobsRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetJobsRequest :: Newtype GetJobsRequest _
derive instance repGenericGetJobsRequest :: Generic GetJobsRequest _
instance showGetJobsRequest :: Show GetJobsRequest where
  show = genericShow
instance decodeGetJobsRequest :: Decode GetJobsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobsRequest :: Encode GetJobsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetJobsResponse = GetJobsResponse 
  { "Jobs" :: NullOrUndefined.NullOrUndefined (JobList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeGetJobsResponse :: Newtype GetJobsResponse _
derive instance repGenericGetJobsResponse :: Generic GetJobsResponse _
instance showGetJobsResponse :: Show GetJobsResponse where
  show = genericShow
instance decodeGetJobsResponse :: Decode GetJobsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobsResponse :: Encode GetJobsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMappingRequest = GetMappingRequest 
  { "Source" :: (CatalogEntry)
  , "Sinks" :: NullOrUndefined.NullOrUndefined (CatalogEntries)
  , "Location" :: NullOrUndefined.NullOrUndefined (Location)
  }
derive instance newtypeGetMappingRequest :: Newtype GetMappingRequest _
derive instance repGenericGetMappingRequest :: Generic GetMappingRequest _
instance showGetMappingRequest :: Show GetMappingRequest where
  show = genericShow
instance decodeGetMappingRequest :: Decode GetMappingRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMappingRequest :: Encode GetMappingRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetMappingResponse = GetMappingResponse 
  { "Mapping" :: (MappingList)
  }
derive instance newtypeGetMappingResponse :: Newtype GetMappingResponse _
derive instance repGenericGetMappingResponse :: Generic GetMappingResponse _
instance showGetMappingResponse :: Show GetMappingResponse where
  show = genericShow
instance decodeGetMappingResponse :: Decode GetMappingResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetMappingResponse :: Encode GetMappingResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPartitionRequest = GetPartitionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValues" :: (ValueStringList)
  }
derive instance newtypeGetPartitionRequest :: Newtype GetPartitionRequest _
derive instance repGenericGetPartitionRequest :: Generic GetPartitionRequest _
instance showGetPartitionRequest :: Show GetPartitionRequest where
  show = genericShow
instance decodeGetPartitionRequest :: Decode GetPartitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPartitionRequest :: Encode GetPartitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPartitionResponse = GetPartitionResponse 
  { "Partition" :: NullOrUndefined.NullOrUndefined (Partition)
  }
derive instance newtypeGetPartitionResponse :: Newtype GetPartitionResponse _
derive instance repGenericGetPartitionResponse :: Generic GetPartitionResponse _
instance showGetPartitionResponse :: Show GetPartitionResponse where
  show = genericShow
instance decodeGetPartitionResponse :: Decode GetPartitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPartitionResponse :: Encode GetPartitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPartitionsRequest = GetPartitionsRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "Expression" :: NullOrUndefined.NullOrUndefined (PredicateString)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "Segment" :: NullOrUndefined.NullOrUndefined (Segment)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetPartitionsRequest :: Newtype GetPartitionsRequest _
derive instance repGenericGetPartitionsRequest :: Generic GetPartitionsRequest _
instance showGetPartitionsRequest :: Show GetPartitionsRequest where
  show = genericShow
instance decodeGetPartitionsRequest :: Decode GetPartitionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPartitionsRequest :: Encode GetPartitionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPartitionsResponse = GetPartitionsResponse 
  { "Partitions" :: NullOrUndefined.NullOrUndefined (PartitionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetPartitionsResponse :: Newtype GetPartitionsResponse _
derive instance repGenericGetPartitionsResponse :: Generic GetPartitionsResponse _
instance showGetPartitionsResponse :: Show GetPartitionsResponse where
  show = genericShow
instance decodeGetPartitionsResponse :: Decode GetPartitionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPartitionsResponse :: Encode GetPartitionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPlanRequest = GetPlanRequest 
  { "Mapping" :: (MappingList)
  , "Source" :: (CatalogEntry)
  , "Sinks" :: NullOrUndefined.NullOrUndefined (CatalogEntries)
  , "Location" :: NullOrUndefined.NullOrUndefined (Location)
  , "Language" :: NullOrUndefined.NullOrUndefined (Language)
  }
derive instance newtypeGetPlanRequest :: Newtype GetPlanRequest _
derive instance repGenericGetPlanRequest :: Generic GetPlanRequest _
instance showGetPlanRequest :: Show GetPlanRequest where
  show = genericShow
instance decodeGetPlanRequest :: Decode GetPlanRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPlanRequest :: Encode GetPlanRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPlanResponse = GetPlanResponse 
  { "PythonScript" :: NullOrUndefined.NullOrUndefined (PythonScript)
  , "ScalaCode" :: NullOrUndefined.NullOrUndefined (ScalaCode)
  }
derive instance newtypeGetPlanResponse :: Newtype GetPlanResponse _
derive instance repGenericGetPlanResponse :: Generic GetPlanResponse _
instance showGetPlanResponse :: Show GetPlanResponse where
  show = genericShow
instance decodeGetPlanResponse :: Decode GetPlanResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPlanResponse :: Encode GetPlanResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTableRequest = GetTableRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Name" :: (NameString)
  }
derive instance newtypeGetTableRequest :: Newtype GetTableRequest _
derive instance repGenericGetTableRequest :: Generic GetTableRequest _
instance showGetTableRequest :: Show GetTableRequest where
  show = genericShow
instance decodeGetTableRequest :: Decode GetTableRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTableRequest :: Encode GetTableRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTableResponse = GetTableResponse 
  { "Table" :: NullOrUndefined.NullOrUndefined (Table)
  }
derive instance newtypeGetTableResponse :: Newtype GetTableResponse _
derive instance repGenericGetTableResponse :: Generic GetTableResponse _
instance showGetTableResponse :: Show GetTableResponse where
  show = genericShow
instance decodeGetTableResponse :: Decode GetTableResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTableResponse :: Encode GetTableResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTableVersionRequest = GetTableVersionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (VersionString)
  }
derive instance newtypeGetTableVersionRequest :: Newtype GetTableVersionRequest _
derive instance repGenericGetTableVersionRequest :: Generic GetTableVersionRequest _
instance showGetTableVersionRequest :: Show GetTableVersionRequest where
  show = genericShow
instance decodeGetTableVersionRequest :: Decode GetTableVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTableVersionRequest :: Encode GetTableVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTableVersionResponse = GetTableVersionResponse 
  { "TableVersion" :: NullOrUndefined.NullOrUndefined (TableVersion)
  }
derive instance newtypeGetTableVersionResponse :: Newtype GetTableVersionResponse _
derive instance repGenericGetTableVersionResponse :: Generic GetTableVersionResponse _
instance showGetTableVersionResponse :: Show GetTableVersionResponse where
  show = genericShow
instance decodeGetTableVersionResponse :: Decode GetTableVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTableVersionResponse :: Encode GetTableVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTableVersionsList = GetTableVersionsList (Array TableVersion)
derive instance newtypeGetTableVersionsList :: Newtype GetTableVersionsList _
derive instance repGenericGetTableVersionsList :: Generic GetTableVersionsList _
instance showGetTableVersionsList :: Show GetTableVersionsList where
  show = genericShow
instance decodeGetTableVersionsList :: Decode GetTableVersionsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTableVersionsList :: Encode GetTableVersionsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTableVersionsRequest = GetTableVersionsRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetTableVersionsRequest :: Newtype GetTableVersionsRequest _
derive instance repGenericGetTableVersionsRequest :: Generic GetTableVersionsRequest _
instance showGetTableVersionsRequest :: Show GetTableVersionsRequest where
  show = genericShow
instance decodeGetTableVersionsRequest :: Decode GetTableVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTableVersionsRequest :: Encode GetTableVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTableVersionsResponse = GetTableVersionsResponse 
  { "TableVersions" :: NullOrUndefined.NullOrUndefined (GetTableVersionsList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetTableVersionsResponse :: Newtype GetTableVersionsResponse _
derive instance repGenericGetTableVersionsResponse :: Generic GetTableVersionsResponse _
instance showGetTableVersionsResponse :: Show GetTableVersionsResponse where
  show = genericShow
instance decodeGetTableVersionsResponse :: Decode GetTableVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTableVersionsResponse :: Encode GetTableVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTablesRequest = GetTablesRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Expression" :: NullOrUndefined.NullOrUndefined (FilterString)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetTablesRequest :: Newtype GetTablesRequest _
derive instance repGenericGetTablesRequest :: Generic GetTablesRequest _
instance showGetTablesRequest :: Show GetTablesRequest where
  show = genericShow
instance decodeGetTablesRequest :: Decode GetTablesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTablesRequest :: Encode GetTablesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTablesResponse = GetTablesResponse 
  { "TableList" :: NullOrUndefined.NullOrUndefined (TableList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetTablesResponse :: Newtype GetTablesResponse _
derive instance repGenericGetTablesResponse :: Generic GetTablesResponse _
instance showGetTablesResponse :: Show GetTablesResponse where
  show = genericShow
instance decodeGetTablesResponse :: Decode GetTablesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTablesResponse :: Encode GetTablesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTriggerRequest = GetTriggerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeGetTriggerRequest :: Newtype GetTriggerRequest _
derive instance repGenericGetTriggerRequest :: Generic GetTriggerRequest _
instance showGetTriggerRequest :: Show GetTriggerRequest where
  show = genericShow
instance decodeGetTriggerRequest :: Decode GetTriggerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTriggerRequest :: Encode GetTriggerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTriggerResponse = GetTriggerResponse 
  { "Trigger" :: NullOrUndefined.NullOrUndefined (Trigger)
  }
derive instance newtypeGetTriggerResponse :: Newtype GetTriggerResponse _
derive instance repGenericGetTriggerResponse :: Generic GetTriggerResponse _
instance showGetTriggerResponse :: Show GetTriggerResponse where
  show = genericShow
instance decodeGetTriggerResponse :: Decode GetTriggerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTriggerResponse :: Encode GetTriggerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTriggersRequest = GetTriggersRequest 
  { "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "DependentJobName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetTriggersRequest :: Newtype GetTriggersRequest _
derive instance repGenericGetTriggersRequest :: Generic GetTriggersRequest _
instance showGetTriggersRequest :: Show GetTriggersRequest where
  show = genericShow
instance decodeGetTriggersRequest :: Decode GetTriggersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTriggersRequest :: Encode GetTriggersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetTriggersResponse = GetTriggersResponse 
  { "Triggers" :: NullOrUndefined.NullOrUndefined (TriggerList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (GenericString)
  }
derive instance newtypeGetTriggersResponse :: Newtype GetTriggersResponse _
derive instance repGenericGetTriggersResponse :: Generic GetTriggersResponse _
instance showGetTriggersResponse :: Show GetTriggersResponse where
  show = genericShow
instance decodeGetTriggersResponse :: Decode GetTriggersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetTriggersResponse :: Encode GetTriggersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUserDefinedFunctionRequest = GetUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  }
derive instance newtypeGetUserDefinedFunctionRequest :: Newtype GetUserDefinedFunctionRequest _
derive instance repGenericGetUserDefinedFunctionRequest :: Generic GetUserDefinedFunctionRequest _
instance showGetUserDefinedFunctionRequest :: Show GetUserDefinedFunctionRequest where
  show = genericShow
instance decodeGetUserDefinedFunctionRequest :: Decode GetUserDefinedFunctionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserDefinedFunctionRequest :: Encode GetUserDefinedFunctionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUserDefinedFunctionResponse = GetUserDefinedFunctionResponse 
  { "UserDefinedFunction" :: NullOrUndefined.NullOrUndefined (UserDefinedFunction)
  }
derive instance newtypeGetUserDefinedFunctionResponse :: Newtype GetUserDefinedFunctionResponse _
derive instance repGenericGetUserDefinedFunctionResponse :: Generic GetUserDefinedFunctionResponse _
instance showGetUserDefinedFunctionResponse :: Show GetUserDefinedFunctionResponse where
  show = genericShow
instance decodeGetUserDefinedFunctionResponse :: Decode GetUserDefinedFunctionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserDefinedFunctionResponse :: Encode GetUserDefinedFunctionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUserDefinedFunctionsRequest = GetUserDefinedFunctionsRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "Pattern" :: (NameString)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined.NullOrUndefined (PageSize)
  }
derive instance newtypeGetUserDefinedFunctionsRequest :: Newtype GetUserDefinedFunctionsRequest _
derive instance repGenericGetUserDefinedFunctionsRequest :: Generic GetUserDefinedFunctionsRequest _
instance showGetUserDefinedFunctionsRequest :: Show GetUserDefinedFunctionsRequest where
  show = genericShow
instance decodeGetUserDefinedFunctionsRequest :: Decode GetUserDefinedFunctionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserDefinedFunctionsRequest :: Encode GetUserDefinedFunctionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetUserDefinedFunctionsResponse = GetUserDefinedFunctionsResponse 
  { "UserDefinedFunctions" :: NullOrUndefined.NullOrUndefined (UserDefinedFunctionList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (Token)
  }
derive instance newtypeGetUserDefinedFunctionsResponse :: Newtype GetUserDefinedFunctionsResponse _
derive instance repGenericGetUserDefinedFunctionsResponse :: Generic GetUserDefinedFunctionsResponse _
instance showGetUserDefinedFunctionsResponse :: Show GetUserDefinedFunctionsResponse where
  show = genericShow
instance decodeGetUserDefinedFunctionsResponse :: Decode GetUserDefinedFunctionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetUserDefinedFunctionsResponse :: Encode GetUserDefinedFunctionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A classifier that uses <code>grok</code> patterns.</p>
newtype GrokClassifier = GrokClassifier 
  { "Name" :: (NameString)
  , "Classification" :: (Classification)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version" :: NullOrUndefined.NullOrUndefined (VersionId)
  , "GrokPattern" :: (GrokPattern)
  , "CustomPatterns" :: NullOrUndefined.NullOrUndefined (CustomPatterns)
  }
derive instance newtypeGrokClassifier :: Newtype GrokClassifier _
derive instance repGenericGrokClassifier :: Generic GrokClassifier _
instance showGrokClassifier :: Show GrokClassifier where
  show = genericShow
instance decodeGrokClassifier :: Decode GrokClassifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrokClassifier :: Encode GrokClassifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GrokPattern = GrokPattern String
derive instance newtypeGrokPattern :: Newtype GrokPattern _
derive instance repGenericGrokPattern :: Generic GrokPattern _
instance showGrokPattern :: Show GrokPattern where
  show = genericShow
instance decodeGrokPattern :: Decode GrokPattern where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGrokPattern :: Encode GrokPattern where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdString = IdString String
derive instance newtypeIdString :: Newtype IdString _
derive instance repGenericIdString :: Generic IdString _
instance showIdString :: Show IdString where
  show = genericShow
instance decodeIdString :: Decode IdString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdString :: Encode IdString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The same unique identifier was associated with two different records.</p>
newtype IdempotentParameterMismatchException = IdempotentParameterMismatchException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeIdempotentParameterMismatchException :: Newtype IdempotentParameterMismatchException _
derive instance repGenericIdempotentParameterMismatchException :: Generic IdempotentParameterMismatchException _
instance showIdempotentParameterMismatchException :: Show IdempotentParameterMismatchException where
  show = genericShow
instance decodeIdempotentParameterMismatchException :: Decode IdempotentParameterMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdempotentParameterMismatchException :: Encode IdempotentParameterMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportCatalogToGlueRequest = ImportCatalogToGlueRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  }
derive instance newtypeImportCatalogToGlueRequest :: Newtype ImportCatalogToGlueRequest _
derive instance repGenericImportCatalogToGlueRequest :: Generic ImportCatalogToGlueRequest _
instance showImportCatalogToGlueRequest :: Show ImportCatalogToGlueRequest where
  show = genericShow
instance decodeImportCatalogToGlueRequest :: Decode ImportCatalogToGlueRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportCatalogToGlueRequest :: Encode ImportCatalogToGlueRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImportCatalogToGlueResponse = ImportCatalogToGlueResponse Types.NoArguments
derive instance newtypeImportCatalogToGlueResponse :: Newtype ImportCatalogToGlueResponse _
derive instance repGenericImportCatalogToGlueResponse :: Generic ImportCatalogToGlueResponse _
instance showImportCatalogToGlueResponse :: Show ImportCatalogToGlueResponse where
  show = genericShow
instance decodeImportCatalogToGlueResponse :: Decode ImportCatalogToGlueResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImportCatalogToGlueResponse :: Encode ImportCatalogToGlueResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntegerFlag = IntegerFlag Int
derive instance newtypeIntegerFlag :: Newtype IntegerFlag _
derive instance repGenericIntegerFlag :: Generic IntegerFlag _
instance showIntegerFlag :: Show IntegerFlag where
  show = genericShow
instance decodeIntegerFlag :: Decode IntegerFlag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegerFlag :: Encode IntegerFlag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntegerValue = IntegerValue Int
derive instance newtypeIntegerValue :: Newtype IntegerValue _
derive instance repGenericIntegerValue :: Generic IntegerValue _
instance showIntegerValue :: Show IntegerValue where
  show = genericShow
instance decodeIntegerValue :: Decode IntegerValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntegerValue :: Encode IntegerValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An internal service error occurred.</p>
newtype InternalServiceException = InternalServiceException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeInternalServiceException :: Newtype InternalServiceException _
derive instance repGenericInternalServiceException :: Generic InternalServiceException _
instance showInternalServiceException :: Show InternalServiceException where
  show = genericShow
instance decodeInternalServiceException :: Decode InternalServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServiceException :: Encode InternalServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input provided was not valid.</p>
newtype InvalidInputException = InvalidInputException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _
derive instance repGenericInvalidInputException :: Generic InvalidInputException _
instance showInvalidInputException :: Show InvalidInputException where
  show = genericShow
instance decodeInvalidInputException :: Decode InvalidInputException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidInputException :: Encode InvalidInputException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a JDBC data store to crawl.</p>
newtype JdbcTarget = JdbcTarget 
  { "ConnectionName" :: NullOrUndefined.NullOrUndefined (ConnectionName)
  , "Path" :: NullOrUndefined.NullOrUndefined (Path)
  , "Exclusions" :: NullOrUndefined.NullOrUndefined (PathList)
  }
derive instance newtypeJdbcTarget :: Newtype JdbcTarget _
derive instance repGenericJdbcTarget :: Generic JdbcTarget _
instance showJdbcTarget :: Show JdbcTarget where
  show = genericShow
instance decodeJdbcTarget :: Decode JdbcTarget where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJdbcTarget :: Encode JdbcTarget where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JdbcTargetList = JdbcTargetList (Array JdbcTarget)
derive instance newtypeJdbcTargetList :: Newtype JdbcTargetList _
derive instance repGenericJdbcTargetList :: Generic JdbcTargetList _
instance showJdbcTargetList :: Show JdbcTargetList where
  show = genericShow
instance decodeJdbcTargetList :: Decode JdbcTargetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJdbcTargetList :: Encode JdbcTargetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a job.</p>
newtype Job = Job 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (UriString)
  , "Role" :: NullOrUndefined.NullOrUndefined (RoleString)
  , "CreatedOn" :: NullOrUndefined.NullOrUndefined (TimestampValue)
  , "LastModifiedOn" :: NullOrUndefined.NullOrUndefined (TimestampValue)
  , "ExecutionProperty" :: NullOrUndefined.NullOrUndefined (ExecutionProperty)
  , "Command" :: NullOrUndefined.NullOrUndefined (JobCommand)
  , "DefaultArguments" :: NullOrUndefined.NullOrUndefined (GenericMap)
  , "Connections" :: NullOrUndefined.NullOrUndefined (ConnectionsList)
  , "MaxRetries" :: NullOrUndefined.NullOrUndefined (MaxRetries)
  , "AllocatedCapacity" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  }
derive instance newtypeJob :: Newtype Job _
derive instance repGenericJob :: Generic Job _
instance showJob :: Show Job where
  show = genericShow
instance decodeJob :: Decode Job where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJob :: Encode Job where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a point which a job can resume processing.</p>
newtype JobBookmarkEntry = JobBookmarkEntry 
  { "JobName" :: NullOrUndefined.NullOrUndefined (JobName)
  , "Version" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  , "Run" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  , "Attempt" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  , "JobBookmark" :: NullOrUndefined.NullOrUndefined (JsonValue)
  }
derive instance newtypeJobBookmarkEntry :: Newtype JobBookmarkEntry _
derive instance repGenericJobBookmarkEntry :: Generic JobBookmarkEntry _
instance showJobBookmarkEntry :: Show JobBookmarkEntry where
  show = genericShow
instance decodeJobBookmarkEntry :: Decode JobBookmarkEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobBookmarkEntry :: Encode JobBookmarkEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies code that executes a job.</p>
newtype JobCommand = JobCommand 
  { "Name" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "ScriptLocation" :: NullOrUndefined.NullOrUndefined (ScriptLocationString)
  }
derive instance newtypeJobCommand :: Newtype JobCommand _
derive instance repGenericJobCommand :: Generic JobCommand _
instance showJobCommand :: Show JobCommand where
  show = genericShow
instance decodeJobCommand :: Decode JobCommand where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobCommand :: Encode JobCommand where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobList = JobList (Array Job)
derive instance newtypeJobList :: Newtype JobList _
derive instance repGenericJobList :: Generic JobList _
instance showJobList :: Show JobList where
  show = genericShow
instance decodeJobList :: Decode JobList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobList :: Encode JobList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobName = JobName String
derive instance newtypeJobName :: Newtype JobName _
derive instance repGenericJobName :: Generic JobName _
instance showJobName :: Show JobName where
  show = genericShow
instance decodeJobName :: Decode JobName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobName :: Encode JobName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a job run.</p>
newtype JobRun = JobRun 
  { "Id" :: NullOrUndefined.NullOrUndefined (IdString)
  , "Attempt" :: NullOrUndefined.NullOrUndefined (AttemptCount)
  , "PreviousRunId" :: NullOrUndefined.NullOrUndefined (IdString)
  , "TriggerName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "JobName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "StartedOn" :: NullOrUndefined.NullOrUndefined (TimestampValue)
  , "LastModifiedOn" :: NullOrUndefined.NullOrUndefined (TimestampValue)
  , "CompletedOn" :: NullOrUndefined.NullOrUndefined (TimestampValue)
  , "JobRunState" :: NullOrUndefined.NullOrUndefined (JobRunState)
  , "Arguments" :: NullOrUndefined.NullOrUndefined (GenericMap)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (ErrorString)
  , "PredecessorRuns" :: NullOrUndefined.NullOrUndefined (PredecessorList)
  , "AllocatedCapacity" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  }
derive instance newtypeJobRun :: Newtype JobRun _
derive instance repGenericJobRun :: Generic JobRun _
instance showJobRun :: Show JobRun where
  show = genericShow
instance decodeJobRun :: Decode JobRun where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobRun :: Encode JobRun where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobRunList = JobRunList (Array JobRun)
derive instance newtypeJobRunList :: Newtype JobRunList _
derive instance repGenericJobRunList :: Generic JobRunList _
instance showJobRunList :: Show JobRunList where
  show = genericShow
instance decodeJobRunList :: Decode JobRunList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobRunList :: Encode JobRunList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobRunState = JobRunState String
derive instance newtypeJobRunState :: Newtype JobRunState _
derive instance repGenericJobRunState :: Generic JobRunState _
instance showJobRunState :: Show JobRunState where
  show = genericShow
instance decodeJobRunState :: Decode JobRunState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobRunState :: Encode JobRunState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies information used to update an existing job. Note that the previous job definition will be completely overwritten by this information.</p>
newtype JobUpdate = JobUpdate 
  { "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "LogUri" :: NullOrUndefined.NullOrUndefined (UriString)
  , "Role" :: NullOrUndefined.NullOrUndefined (RoleString)
  , "ExecutionProperty" :: NullOrUndefined.NullOrUndefined (ExecutionProperty)
  , "Command" :: NullOrUndefined.NullOrUndefined (JobCommand)
  , "DefaultArguments" :: NullOrUndefined.NullOrUndefined (GenericMap)
  , "Connections" :: NullOrUndefined.NullOrUndefined (ConnectionsList)
  , "MaxRetries" :: NullOrUndefined.NullOrUndefined (MaxRetries)
  , "AllocatedCapacity" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  }
derive instance newtypeJobUpdate :: Newtype JobUpdate _
derive instance repGenericJobUpdate :: Generic JobUpdate _
instance showJobUpdate :: Show JobUpdate where
  show = genericShow
instance decodeJobUpdate :: Decode JobUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobUpdate :: Encode JobUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A classifier for <code>JSON</code> content.</p>
newtype JsonClassifier = JsonClassifier 
  { "Name" :: (NameString)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version" :: NullOrUndefined.NullOrUndefined (VersionId)
  , "JsonPath" :: (JsonPath)
  }
derive instance newtypeJsonClassifier :: Newtype JsonClassifier _
derive instance repGenericJsonClassifier :: Generic JsonClassifier _
instance showJsonClassifier :: Show JsonClassifier where
  show = genericShow
instance decodeJsonClassifier :: Decode JsonClassifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJsonClassifier :: Encode JsonClassifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JsonPath = JsonPath String
derive instance newtypeJsonPath :: Newtype JsonPath _
derive instance repGenericJsonPath :: Generic JsonPath _
instance showJsonPath :: Show JsonPath where
  show = genericShow
instance decodeJsonPath :: Decode JsonPath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJsonPath :: Encode JsonPath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JsonValue = JsonValue String
derive instance newtypeJsonValue :: Newtype JsonValue _
derive instance repGenericJsonValue :: Generic JsonValue _
instance showJsonValue :: Show JsonValue where
  show = genericShow
instance decodeJsonValue :: Decode JsonValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJsonValue :: Encode JsonValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyString = KeyString String
derive instance newtypeKeyString :: Newtype KeyString _
derive instance repGenericKeyString :: Generic KeyString _
instance showKeyString :: Show KeyString where
  show = genericShow
instance decodeKeyString :: Decode KeyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyString :: Encode KeyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Language = Language String
derive instance newtypeLanguage :: Newtype Language _
derive instance repGenericLanguage :: Generic Language _
instance showLanguage :: Show Language where
  show = genericShow
instance decodeLanguage :: Decode Language where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLanguage :: Encode Language where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Status and error information about the most recent crawl.</p>
newtype LastCrawlInfo = LastCrawlInfo 
  { "Status" :: NullOrUndefined.NullOrUndefined (LastCrawlStatus)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "LogGroup" :: NullOrUndefined.NullOrUndefined (LogGroup)
  , "LogStream" :: NullOrUndefined.NullOrUndefined (LogStream)
  , "MessagePrefix" :: NullOrUndefined.NullOrUndefined (MessagePrefix)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeLastCrawlInfo :: Newtype LastCrawlInfo _
derive instance repGenericLastCrawlInfo :: Generic LastCrawlInfo _
instance showLastCrawlInfo :: Show LastCrawlInfo where
  show = genericShow
instance decodeLastCrawlInfo :: Decode LastCrawlInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastCrawlInfo :: Encode LastCrawlInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastCrawlStatus = LastCrawlStatus String
derive instance newtypeLastCrawlStatus :: Newtype LastCrawlStatus _
derive instance repGenericLastCrawlStatus :: Generic LastCrawlStatus _
instance showLastCrawlStatus :: Show LastCrawlStatus where
  show = genericShow
instance decodeLastCrawlStatus :: Decode LastCrawlStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastCrawlStatus :: Encode LastCrawlStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The location of resources.</p>
newtype Location = Location 
  { "Jdbc" :: NullOrUndefined.NullOrUndefined (CodeGenNodeArgs)
  , "S3" :: NullOrUndefined.NullOrUndefined (CodeGenNodeArgs)
  }
derive instance newtypeLocation :: Newtype Location _
derive instance repGenericLocation :: Generic Location _
instance showLocation :: Show Location where
  show = genericShow
instance decodeLocation :: Decode Location where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocation :: Encode Location where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocationMap = LocationMap (StrMap.StrMap ColumnValuesString)
derive instance newtypeLocationMap :: Newtype LocationMap _
derive instance repGenericLocationMap :: Generic LocationMap _
instance showLocationMap :: Show LocationMap where
  show = genericShow
instance decodeLocationMap :: Decode LocationMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationMap :: Encode LocationMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocationString = LocationString String
derive instance newtypeLocationString :: Newtype LocationString _
derive instance repGenericLocationString :: Generic LocationString _
instance showLocationString :: Show LocationString where
  show = genericShow
instance decodeLocationString :: Decode LocationString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocationString :: Encode LocationString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogGroup = LogGroup String
derive instance newtypeLogGroup :: Newtype LogGroup _
derive instance repGenericLogGroup :: Generic LogGroup _
instance showLogGroup :: Show LogGroup where
  show = genericShow
instance decodeLogGroup :: Decode LogGroup where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogGroup :: Encode LogGroup where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogStream = LogStream String
derive instance newtypeLogStream :: Newtype LogStream _
derive instance repGenericLogStream :: Generic LogStream _
instance showLogStream :: Show LogStream where
  show = genericShow
instance decodeLogStream :: Decode LogStream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogStream :: Encode LogStream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Logical = Logical String
derive instance newtypeLogical :: Newtype Logical _
derive instance repGenericLogical :: Generic Logical _
instance showLogical :: Show Logical where
  show = genericShow
instance decodeLogical :: Decode Logical where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogical :: Encode Logical where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogicalOperator = LogicalOperator String
derive instance newtypeLogicalOperator :: Newtype LogicalOperator _
derive instance repGenericLogicalOperator :: Generic LogicalOperator _
instance showLogicalOperator :: Show LogicalOperator where
  show = genericShow
instance decodeLogicalOperator :: Decode LogicalOperator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogicalOperator :: Encode LogicalOperator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a mapping.</p>
newtype MappingEntry = MappingEntry 
  { "SourceTable" :: NullOrUndefined.NullOrUndefined (TableName)
  , "SourcePath" :: NullOrUndefined.NullOrUndefined (SchemaPathString)
  , "SourceType" :: NullOrUndefined.NullOrUndefined (FieldType)
  , "TargetTable" :: NullOrUndefined.NullOrUndefined (TableName)
  , "TargetPath" :: NullOrUndefined.NullOrUndefined (SchemaPathString)
  , "TargetType" :: NullOrUndefined.NullOrUndefined (FieldType)
  }
derive instance newtypeMappingEntry :: Newtype MappingEntry _
derive instance repGenericMappingEntry :: Generic MappingEntry _
instance showMappingEntry :: Show MappingEntry where
  show = genericShow
instance decodeMappingEntry :: Decode MappingEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMappingEntry :: Encode MappingEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MappingList = MappingList (Array MappingEntry)
derive instance newtypeMappingList :: Newtype MappingList _
derive instance repGenericMappingList :: Generic MappingList _
instance showMappingList :: Show MappingList where
  show = genericShow
instance decodeMappingList :: Decode MappingList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMappingList :: Encode MappingList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MatchCriteria = MatchCriteria (Array NameString)
derive instance newtypeMatchCriteria :: Newtype MatchCriteria _
derive instance repGenericMatchCriteria :: Generic MatchCriteria _
instance showMatchCriteria :: Show MatchCriteria where
  show = genericShow
instance decodeMatchCriteria :: Decode MatchCriteria where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMatchCriteria :: Encode MatchCriteria where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxConcurrentRuns = MaxConcurrentRuns Int
derive instance newtypeMaxConcurrentRuns :: Newtype MaxConcurrentRuns _
derive instance repGenericMaxConcurrentRuns :: Generic MaxConcurrentRuns _
instance showMaxConcurrentRuns :: Show MaxConcurrentRuns where
  show = genericShow
instance decodeMaxConcurrentRuns :: Decode MaxConcurrentRuns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxConcurrentRuns :: Encode MaxConcurrentRuns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxRetries = MaxRetries Int
derive instance newtypeMaxRetries :: Newtype MaxRetries _
derive instance repGenericMaxRetries :: Generic MaxRetries _
instance showMaxRetries :: Show MaxRetries where
  show = genericShow
instance decodeMaxRetries :: Decode MaxRetries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxRetries :: Encode MaxRetries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessagePrefix = MessagePrefix String
derive instance newtypeMessagePrefix :: Newtype MessagePrefix _
derive instance repGenericMessagePrefix :: Generic MessagePrefix _
instance showMessagePrefix :: Show MessagePrefix where
  show = genericShow
instance decodeMessagePrefix :: Decode MessagePrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessagePrefix :: Encode MessagePrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageString = MessageString String
derive instance newtypeMessageString :: Newtype MessageString _
derive instance repGenericMessageString :: Generic MessageString _
instance showMessageString :: Show MessageString where
  show = genericShow
instance decodeMessageString :: Decode MessageString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageString :: Encode MessageString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MillisecondsCount = MillisecondsCount Number
derive instance newtypeMillisecondsCount :: Newtype MillisecondsCount _
derive instance repGenericMillisecondsCount :: Generic MillisecondsCount _
instance showMillisecondsCount :: Show MillisecondsCount where
  show = genericShow
instance decodeMillisecondsCount :: Decode MillisecondsCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMillisecondsCount :: Encode MillisecondsCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NameString = NameString String
derive instance newtypeNameString :: Newtype NameString _
derive instance repGenericNameString :: Generic NameString _
instance showNameString :: Show NameString where
  show = genericShow
instance decodeNameString :: Decode NameString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNameString :: Encode NameString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NameStringList = NameStringList (Array NameString)
derive instance newtypeNameStringList :: Newtype NameStringList _
derive instance repGenericNameStringList :: Generic NameStringList _
instance showNameStringList :: Show NameStringList where
  show = genericShow
instance decodeNameStringList :: Decode NameStringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNameStringList :: Encode NameStringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There is no applicable schedule.</p>
newtype NoScheduleException = NoScheduleException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeNoScheduleException :: Newtype NoScheduleException _
derive instance repGenericNoScheduleException :: Generic NoScheduleException _
instance showNoScheduleException :: Show NoScheduleException where
  show = genericShow
instance decodeNoScheduleException :: Decode NoScheduleException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoScheduleException :: Encode NoScheduleException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonNegativeDouble = NonNegativeDouble Number
derive instance newtypeNonNegativeDouble :: Newtype NonNegativeDouble _
derive instance repGenericNonNegativeDouble :: Generic NonNegativeDouble _
instance showNonNegativeDouble :: Show NonNegativeDouble where
  show = genericShow
instance decodeNonNegativeDouble :: Decode NonNegativeDouble where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonNegativeDouble :: Encode NonNegativeDouble where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonNegativeInteger = NonNegativeInteger Int
derive instance newtypeNonNegativeInteger :: Newtype NonNegativeInteger _
derive instance repGenericNonNegativeInteger :: Generic NonNegativeInteger _
instance showNonNegativeInteger :: Show NonNegativeInteger where
  show = genericShow
instance decodeNonNegativeInteger :: Decode NonNegativeInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonNegativeInteger :: Encode NonNegativeInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation timed out.</p>
newtype OperationTimeoutException = OperationTimeoutException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeOperationTimeoutException :: Newtype OperationTimeoutException _
derive instance repGenericOperationTimeoutException :: Generic OperationTimeoutException _
instance showOperationTimeoutException :: Show OperationTimeoutException where
  show = genericShow
instance decodeOperationTimeoutException :: Decode OperationTimeoutException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationTimeoutException :: Encode OperationTimeoutException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the sort order of a sorted column.</p>
newtype Order = Order 
  { "Column" :: (NameString)
  , "SortOrder" :: (IntegerFlag)
  }
derive instance newtypeOrder :: Newtype Order _
derive instance repGenericOrder :: Generic Order _
instance showOrder :: Show Order where
  show = genericShow
instance decodeOrder :: Decode Order where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrder :: Encode Order where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderList = OrderList (Array Order)
derive instance newtypeOrderList :: Newtype OrderList _
derive instance repGenericOrderList :: Generic OrderList _
instance showOrderList :: Show OrderList where
  show = genericShow
instance decodeOrderList :: Decode OrderList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderList :: Encode OrderList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _
derive instance repGenericPageSize :: Generic PageSize _
instance showPageSize :: Show PageSize where
  show = genericShow
instance decodePageSize :: Decode PageSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageSize :: Encode PageSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersMap = ParametersMap (StrMap.StrMap ParametersMapValue)
derive instance newtypeParametersMap :: Newtype ParametersMap _
derive instance repGenericParametersMap :: Generic ParametersMap _
instance showParametersMap :: Show ParametersMap where
  show = genericShow
instance decodeParametersMap :: Decode ParametersMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersMap :: Encode ParametersMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ParametersMapValue = ParametersMapValue String
derive instance newtypeParametersMapValue :: Newtype ParametersMapValue _
derive instance repGenericParametersMapValue :: Generic ParametersMapValue _
instance showParametersMapValue :: Show ParametersMapValue where
  show = genericShow
instance decodeParametersMapValue :: Decode ParametersMapValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParametersMapValue :: Encode ParametersMapValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a slice of table data.</p>
newtype Partition = Partition 
  { "Values" :: NullOrUndefined.NullOrUndefined (ValueStringList)
  , "DatabaseName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "TableName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastAccessTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "StorageDescriptor" :: NullOrUndefined.NullOrUndefined (StorageDescriptor)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "LastAnalyzedTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypePartition :: Newtype Partition _
derive instance repGenericPartition :: Generic Partition _
instance showPartition :: Show Partition where
  show = genericShow
instance decodePartition :: Decode Partition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartition :: Encode Partition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a partition error.</p>
newtype PartitionError = PartitionError 
  { "PartitionValues" :: NullOrUndefined.NullOrUndefined (ValueStringList)
  , "ErrorDetail" :: NullOrUndefined.NullOrUndefined (ErrorDetail)
  }
derive instance newtypePartitionError :: Newtype PartitionError _
derive instance repGenericPartitionError :: Generic PartitionError _
instance showPartitionError :: Show PartitionError where
  show = genericShow
instance decodePartitionError :: Decode PartitionError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionError :: Encode PartitionError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartitionErrors = PartitionErrors (Array PartitionError)
derive instance newtypePartitionErrors :: Newtype PartitionErrors _
derive instance repGenericPartitionErrors :: Generic PartitionErrors _
instance showPartitionErrors :: Show PartitionErrors where
  show = genericShow
instance decodePartitionErrors :: Decode PartitionErrors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionErrors :: Encode PartitionErrors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The structure used to create and update a partion.</p>
newtype PartitionInput = PartitionInput 
  { "Values" :: NullOrUndefined.NullOrUndefined (ValueStringList)
  , "LastAccessTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "StorageDescriptor" :: NullOrUndefined.NullOrUndefined (StorageDescriptor)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "LastAnalyzedTime" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypePartitionInput :: Newtype PartitionInput _
derive instance repGenericPartitionInput :: Generic PartitionInput _
instance showPartitionInput :: Show PartitionInput where
  show = genericShow
instance decodePartitionInput :: Decode PartitionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionInput :: Encode PartitionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartitionInputList = PartitionInputList (Array PartitionInput)
derive instance newtypePartitionInputList :: Newtype PartitionInputList _
derive instance repGenericPartitionInputList :: Generic PartitionInputList _
instance showPartitionInputList :: Show PartitionInputList where
  show = genericShow
instance decodePartitionInputList :: Decode PartitionInputList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionInputList :: Encode PartitionInputList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PartitionList = PartitionList (Array Partition)
derive instance newtypePartitionList :: Newtype PartitionList _
derive instance repGenericPartitionList :: Generic PartitionList _
instance showPartitionList :: Show PartitionList where
  show = genericShow
instance decodePartitionList :: Decode PartitionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionList :: Encode PartitionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a list of values defining partitions.</p>
newtype PartitionValueList = PartitionValueList 
  { "Values" :: (ValueStringList)
  }
derive instance newtypePartitionValueList :: Newtype PartitionValueList _
derive instance repGenericPartitionValueList :: Generic PartitionValueList _
instance showPartitionValueList :: Show PartitionValueList where
  show = genericShow
instance decodePartitionValueList :: Decode PartitionValueList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePartitionValueList :: Encode PartitionValueList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Path = Path String
derive instance newtypePath :: Newtype Path _
derive instance repGenericPath :: Generic Path _
instance showPath :: Show Path where
  show = genericShow
instance decodePath :: Decode Path where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePath :: Encode Path where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PathList = PathList (Array Path)
derive instance newtypePathList :: Newtype PathList _
derive instance repGenericPathList :: Generic PathList _
instance showPathList :: Show PathList where
  show = genericShow
instance decodePathList :: Decode PathList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePathList :: Encode PathList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies the physical requirements for a connection.</p>
newtype PhysicalConnectionRequirements = PhysicalConnectionRequirements 
  { "SubnetId" :: NullOrUndefined.NullOrUndefined (NameString)
  , "SecurityGroupIdList" :: NullOrUndefined.NullOrUndefined (SecurityGroupIdList)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypePhysicalConnectionRequirements :: Newtype PhysicalConnectionRequirements _
derive instance repGenericPhysicalConnectionRequirements :: Generic PhysicalConnectionRequirements _
instance showPhysicalConnectionRequirements :: Show PhysicalConnectionRequirements where
  show = genericShow
instance decodePhysicalConnectionRequirements :: Decode PhysicalConnectionRequirements where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePhysicalConnectionRequirements :: Encode PhysicalConnectionRequirements where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A job run that was used in the predicate of a conditional trigger that triggered this job run.</p>
newtype Predecessor = Predecessor 
  { "JobName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "RunId" :: NullOrUndefined.NullOrUndefined (IdString)
  }
derive instance newtypePredecessor :: Newtype Predecessor _
derive instance repGenericPredecessor :: Generic Predecessor _
instance showPredecessor :: Show Predecessor where
  show = genericShow
instance decodePredecessor :: Decode Predecessor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredecessor :: Encode Predecessor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PredecessorList = PredecessorList (Array Predecessor)
derive instance newtypePredecessorList :: Newtype PredecessorList _
derive instance repGenericPredecessorList :: Generic PredecessorList _
instance showPredecessorList :: Show PredecessorList where
  show = genericShow
instance decodePredecessorList :: Decode PredecessorList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredecessorList :: Encode PredecessorList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines the predicate of the trigger, which determines when it fires.</p>
newtype Predicate = Predicate 
  { "Logical" :: NullOrUndefined.NullOrUndefined (Logical)
  , "Conditions" :: NullOrUndefined.NullOrUndefined (ConditionList)
  }
derive instance newtypePredicate :: Newtype Predicate _
derive instance repGenericPredicate :: Generic Predicate _
instance showPredicate :: Show Predicate where
  show = genericShow
instance decodePredicate :: Decode Predicate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredicate :: Encode Predicate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PredicateString = PredicateString String
derive instance newtypePredicateString :: Newtype PredicateString _
derive instance repGenericPredicateString :: Generic PredicateString _
instance showPredicateString :: Show PredicateString where
  show = genericShow
instance decodePredicateString :: Decode PredicateString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePredicateString :: Encode PredicateString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PrincipalType = PrincipalType String
derive instance newtypePrincipalType :: Newtype PrincipalType _
derive instance repGenericPrincipalType :: Generic PrincipalType _
instance showPrincipalType :: Show PrincipalType where
  show = genericShow
instance decodePrincipalType :: Decode PrincipalType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipalType :: Encode PrincipalType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PythonScript = PythonScript String
derive instance newtypePythonScript :: Newtype PythonScript _
derive instance repGenericPythonScript :: Generic PythonScript _
instance showPythonScript :: Show PythonScript where
  show = genericShow
instance decodePythonScript :: Decode PythonScript where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePythonScript :: Encode PythonScript where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResetJobBookmarkRequest = ResetJobBookmarkRequest 
  { "JobName" :: (JobName)
  }
derive instance newtypeResetJobBookmarkRequest :: Newtype ResetJobBookmarkRequest _
derive instance repGenericResetJobBookmarkRequest :: Generic ResetJobBookmarkRequest _
instance showResetJobBookmarkRequest :: Show ResetJobBookmarkRequest where
  show = genericShow
instance decodeResetJobBookmarkRequest :: Decode ResetJobBookmarkRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetJobBookmarkRequest :: Encode ResetJobBookmarkRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResetJobBookmarkResponse = ResetJobBookmarkResponse 
  { "JobBookmarkEntry" :: NullOrUndefined.NullOrUndefined (JobBookmarkEntry)
  }
derive instance newtypeResetJobBookmarkResponse :: Newtype ResetJobBookmarkResponse _
derive instance repGenericResetJobBookmarkResponse :: Generic ResetJobBookmarkResponse _
instance showResetJobBookmarkResponse :: Show ResetJobBookmarkResponse where
  show = genericShow
instance decodeResetJobBookmarkResponse :: Decode ResetJobBookmarkResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResetJobBookmarkResponse :: Encode ResetJobBookmarkResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A resource numerical limit was exceeded.</p>
newtype ResourceNumberLimitExceededException = ResourceNumberLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeResourceNumberLimitExceededException :: Newtype ResourceNumberLimitExceededException _
derive instance repGenericResourceNumberLimitExceededException :: Generic ResourceNumberLimitExceededException _
instance showResourceNumberLimitExceededException :: Show ResourceNumberLimitExceededException where
  show = genericShow
instance decodeResourceNumberLimitExceededException :: Decode ResourceNumberLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNumberLimitExceededException :: Encode ResourceNumberLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _
derive instance repGenericResourceType :: Generic ResourceType _
instance showResourceType :: Show ResourceType where
  show = genericShow
instance decodeResourceType :: Decode ResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceType :: Encode ResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>URIs for function resources.</p>
newtype ResourceUri = ResourceUri 
  { "ResourceType" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "Uri" :: NullOrUndefined.NullOrUndefined (URI)
  }
derive instance newtypeResourceUri :: Newtype ResourceUri _
derive instance repGenericResourceUri :: Generic ResourceUri _
instance showResourceUri :: Show ResourceUri where
  show = genericShow
instance decodeResourceUri :: Decode ResourceUri where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceUri :: Encode ResourceUri where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceUriList = ResourceUriList (Array ResourceUri)
derive instance newtypeResourceUriList :: Newtype ResourceUriList _
derive instance repGenericResourceUriList :: Generic ResourceUriList _
instance showResourceUriList :: Show ResourceUriList where
  show = genericShow
instance decodeResourceUriList :: Decode ResourceUriList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceUriList :: Encode ResourceUriList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Role = Role String
derive instance newtypeRole :: Newtype Role _
derive instance repGenericRole :: Generic Role _
instance showRole :: Show Role where
  show = genericShow
instance decodeRole :: Decode Role where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRole :: Encode Role where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _
derive instance repGenericRoleArn :: Generic RoleArn _
instance showRoleArn :: Show RoleArn where
  show = genericShow
instance decodeRoleArn :: Decode RoleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleArn :: Encode RoleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleString = RoleString String
derive instance newtypeRoleString :: Newtype RoleString _
derive instance repGenericRoleString :: Generic RoleString _
instance showRoleString :: Show RoleString where
  show = genericShow
instance decodeRoleString :: Decode RoleString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleString :: Encode RoleString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RowTag = RowTag String
derive instance newtypeRowTag :: Newtype RowTag _
derive instance repGenericRowTag :: Generic RowTag _
instance showRowTag :: Show RowTag where
  show = genericShow
instance decodeRowTag :: Decode RowTag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRowTag :: Encode RowTag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a data store in Amazon S3.</p>
newtype S3Target = S3Target 
  { "Path" :: NullOrUndefined.NullOrUndefined (Path)
  , "Exclusions" :: NullOrUndefined.NullOrUndefined (PathList)
  }
derive instance newtypeS3Target :: Newtype S3Target _
derive instance repGenericS3Target :: Generic S3Target _
instance showS3Target :: Show S3Target where
  show = genericShow
instance decodeS3Target :: Decode S3Target where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3Target :: Encode S3Target where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3TargetList = S3TargetList (Array S3Target)
derive instance newtypeS3TargetList :: Newtype S3TargetList _
derive instance repGenericS3TargetList :: Generic S3TargetList _
instance showS3TargetList :: Show S3TargetList where
  show = genericShow
instance decodeS3TargetList :: Decode S3TargetList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3TargetList :: Encode S3TargetList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScalaCode = ScalaCode String
derive instance newtypeScalaCode :: Newtype ScalaCode _
derive instance repGenericScalaCode :: Generic ScalaCode _
instance showScalaCode :: Show ScalaCode where
  show = genericShow
instance decodeScalaCode :: Decode ScalaCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScalaCode :: Encode ScalaCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A scheduling object using a <code>cron</code> statement to schedule an event.</p>
newtype Schedule = Schedule 
  { "ScheduleExpression" :: NullOrUndefined.NullOrUndefined (CronExpression)
  , "State" :: NullOrUndefined.NullOrUndefined (ScheduleState)
  }
derive instance newtypeSchedule :: Newtype Schedule _
derive instance repGenericSchedule :: Generic Schedule _
instance showSchedule :: Show Schedule where
  show = genericShow
instance decodeSchedule :: Decode Schedule where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchedule :: Encode Schedule where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScheduleState = ScheduleState String
derive instance newtypeScheduleState :: Newtype ScheduleState _
derive instance repGenericScheduleState :: Generic ScheduleState _
instance showScheduleState :: Show ScheduleState where
  show = genericShow
instance decodeScheduleState :: Decode ScheduleState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScheduleState :: Encode ScheduleState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified scheduler is not running.</p>
newtype SchedulerNotRunningException = SchedulerNotRunningException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeSchedulerNotRunningException :: Newtype SchedulerNotRunningException _
derive instance repGenericSchedulerNotRunningException :: Generic SchedulerNotRunningException _
instance showSchedulerNotRunningException :: Show SchedulerNotRunningException where
  show = genericShow
instance decodeSchedulerNotRunningException :: Decode SchedulerNotRunningException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchedulerNotRunningException :: Encode SchedulerNotRunningException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified scheduler is already running.</p>
newtype SchedulerRunningException = SchedulerRunningException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeSchedulerRunningException :: Newtype SchedulerRunningException _
derive instance repGenericSchedulerRunningException :: Generic SchedulerRunningException _
instance showSchedulerRunningException :: Show SchedulerRunningException where
  show = genericShow
instance decodeSchedulerRunningException :: Decode SchedulerRunningException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchedulerRunningException :: Encode SchedulerRunningException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified scheduler is transitioning.</p>
newtype SchedulerTransitioningException = SchedulerTransitioningException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeSchedulerTransitioningException :: Newtype SchedulerTransitioningException _
derive instance repGenericSchedulerTransitioningException :: Generic SchedulerTransitioningException _
instance showSchedulerTransitioningException :: Show SchedulerTransitioningException where
  show = genericShow
instance decodeSchedulerTransitioningException :: Decode SchedulerTransitioningException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchedulerTransitioningException :: Encode SchedulerTransitioningException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Crawler policy for update and deletion behavior.</p>
newtype SchemaChangePolicy = SchemaChangePolicy 
  { "UpdateBehavior" :: NullOrUndefined.NullOrUndefined (UpdateBehavior)
  , "DeleteBehavior" :: NullOrUndefined.NullOrUndefined (DeleteBehavior)
  }
derive instance newtypeSchemaChangePolicy :: Newtype SchemaChangePolicy _
derive instance repGenericSchemaChangePolicy :: Generic SchemaChangePolicy _
instance showSchemaChangePolicy :: Show SchemaChangePolicy where
  show = genericShow
instance decodeSchemaChangePolicy :: Decode SchemaChangePolicy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaChangePolicy :: Encode SchemaChangePolicy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaPathString = SchemaPathString String
derive instance newtypeSchemaPathString :: Newtype SchemaPathString _
derive instance repGenericSchemaPathString :: Generic SchemaPathString _
instance showSchemaPathString :: Show SchemaPathString where
  show = genericShow
instance decodeSchemaPathString :: Decode SchemaPathString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaPathString :: Encode SchemaPathString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ScriptLocationString = ScriptLocationString String
derive instance newtypeScriptLocationString :: Newtype ScriptLocationString _
derive instance repGenericScriptLocationString :: Generic ScriptLocationString _
instance showScriptLocationString :: Show ScriptLocationString where
  show = genericShow
instance decodeScriptLocationString :: Decode ScriptLocationString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeScriptLocationString :: Encode ScriptLocationString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroupIdList = SecurityGroupIdList (Array NameString)
derive instance newtypeSecurityGroupIdList :: Newtype SecurityGroupIdList _
derive instance repGenericSecurityGroupIdList :: Generic SecurityGroupIdList _
instance showSecurityGroupIdList :: Show SecurityGroupIdList where
  show = genericShow
instance decodeSecurityGroupIdList :: Decode SecurityGroupIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupIdList :: Encode SecurityGroupIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Defines a non-overlapping region of a table's partitions, allowing multiple requests to be executed in parallel.</p>
newtype Segment = Segment 
  { "SegmentNumber" :: (NonNegativeInteger)
  , "TotalSegments" :: (TotalSegmentsInteger)
  }
derive instance newtypeSegment :: Newtype Segment _
derive instance repGenericSegment :: Generic Segment _
instance showSegment :: Show Segment where
  show = genericShow
instance decodeSegment :: Decode Segment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSegment :: Encode Segment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a serialization/deserialization program (SerDe) which serves as an extractor and loader.</p>
newtype SerDeInfo = SerDeInfo 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  , "SerializationLibrary" :: NullOrUndefined.NullOrUndefined (NameString)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  }
derive instance newtypeSerDeInfo :: Newtype SerDeInfo _
derive instance repGenericSerDeInfo :: Generic SerDeInfo _
instance showSerDeInfo :: Show SerDeInfo where
  show = genericShow
instance decodeSerDeInfo :: Decode SerDeInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSerDeInfo :: Encode SerDeInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies skewed values in a table. Skewed are ones that occur with very high frequency.</p>
newtype SkewedInfo = SkewedInfo 
  { "SkewedColumnNames" :: NullOrUndefined.NullOrUndefined (NameStringList)
  , "SkewedColumnValues" :: NullOrUndefined.NullOrUndefined (ColumnValueStringList)
  , "SkewedColumnValueLocationMaps" :: NullOrUndefined.NullOrUndefined (LocationMap)
  }
derive instance newtypeSkewedInfo :: Newtype SkewedInfo _
derive instance repGenericSkewedInfo :: Generic SkewedInfo _
instance showSkewedInfo :: Show SkewedInfo where
  show = genericShow
instance decodeSkewedInfo :: Decode SkewedInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSkewedInfo :: Encode SkewedInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartCrawlerRequest = StartCrawlerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeStartCrawlerRequest :: Newtype StartCrawlerRequest _
derive instance repGenericStartCrawlerRequest :: Generic StartCrawlerRequest _
instance showStartCrawlerRequest :: Show StartCrawlerRequest where
  show = genericShow
instance decodeStartCrawlerRequest :: Decode StartCrawlerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartCrawlerRequest :: Encode StartCrawlerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartCrawlerResponse = StartCrawlerResponse Types.NoArguments
derive instance newtypeStartCrawlerResponse :: Newtype StartCrawlerResponse _
derive instance repGenericStartCrawlerResponse :: Generic StartCrawlerResponse _
instance showStartCrawlerResponse :: Show StartCrawlerResponse where
  show = genericShow
instance decodeStartCrawlerResponse :: Decode StartCrawlerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartCrawlerResponse :: Encode StartCrawlerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartCrawlerScheduleRequest = StartCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  }
derive instance newtypeStartCrawlerScheduleRequest :: Newtype StartCrawlerScheduleRequest _
derive instance repGenericStartCrawlerScheduleRequest :: Generic StartCrawlerScheduleRequest _
instance showStartCrawlerScheduleRequest :: Show StartCrawlerScheduleRequest where
  show = genericShow
instance decodeStartCrawlerScheduleRequest :: Decode StartCrawlerScheduleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartCrawlerScheduleRequest :: Encode StartCrawlerScheduleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartCrawlerScheduleResponse = StartCrawlerScheduleResponse Types.NoArguments
derive instance newtypeStartCrawlerScheduleResponse :: Newtype StartCrawlerScheduleResponse _
derive instance repGenericStartCrawlerScheduleResponse :: Generic StartCrawlerScheduleResponse _
instance showStartCrawlerScheduleResponse :: Show StartCrawlerScheduleResponse where
  show = genericShow
instance decodeStartCrawlerScheduleResponse :: Decode StartCrawlerScheduleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartCrawlerScheduleResponse :: Encode StartCrawlerScheduleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartJobRunRequest = StartJobRunRequest 
  { "JobName" :: (NameString)
  , "JobRunId" :: NullOrUndefined.NullOrUndefined (IdString)
  , "Arguments" :: NullOrUndefined.NullOrUndefined (GenericMap)
  , "AllocatedCapacity" :: NullOrUndefined.NullOrUndefined (IntegerValue)
  }
derive instance newtypeStartJobRunRequest :: Newtype StartJobRunRequest _
derive instance repGenericStartJobRunRequest :: Generic StartJobRunRequest _
instance showStartJobRunRequest :: Show StartJobRunRequest where
  show = genericShow
instance decodeStartJobRunRequest :: Decode StartJobRunRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartJobRunRequest :: Encode StartJobRunRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartJobRunResponse = StartJobRunResponse 
  { "JobRunId" :: NullOrUndefined.NullOrUndefined (IdString)
  }
derive instance newtypeStartJobRunResponse :: Newtype StartJobRunResponse _
derive instance repGenericStartJobRunResponse :: Generic StartJobRunResponse _
instance showStartJobRunResponse :: Show StartJobRunResponse where
  show = genericShow
instance decodeStartJobRunResponse :: Decode StartJobRunResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartJobRunResponse :: Encode StartJobRunResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartTriggerRequest = StartTriggerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeStartTriggerRequest :: Newtype StartTriggerRequest _
derive instance repGenericStartTriggerRequest :: Generic StartTriggerRequest _
instance showStartTriggerRequest :: Show StartTriggerRequest where
  show = genericShow
instance decodeStartTriggerRequest :: Decode StartTriggerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartTriggerRequest :: Encode StartTriggerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartTriggerResponse = StartTriggerResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeStartTriggerResponse :: Newtype StartTriggerResponse _
derive instance repGenericStartTriggerResponse :: Generic StartTriggerResponse _
instance showStartTriggerResponse :: Show StartTriggerResponse where
  show = genericShow
instance decodeStartTriggerResponse :: Decode StartTriggerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartTriggerResponse :: Encode StartTriggerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopCrawlerRequest = StopCrawlerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeStopCrawlerRequest :: Newtype StopCrawlerRequest _
derive instance repGenericStopCrawlerRequest :: Generic StopCrawlerRequest _
instance showStopCrawlerRequest :: Show StopCrawlerRequest where
  show = genericShow
instance decodeStopCrawlerRequest :: Decode StopCrawlerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopCrawlerRequest :: Encode StopCrawlerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopCrawlerResponse = StopCrawlerResponse Types.NoArguments
derive instance newtypeStopCrawlerResponse :: Newtype StopCrawlerResponse _
derive instance repGenericStopCrawlerResponse :: Generic StopCrawlerResponse _
instance showStopCrawlerResponse :: Show StopCrawlerResponse where
  show = genericShow
instance decodeStopCrawlerResponse :: Decode StopCrawlerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopCrawlerResponse :: Encode StopCrawlerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopCrawlerScheduleRequest = StopCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  }
derive instance newtypeStopCrawlerScheduleRequest :: Newtype StopCrawlerScheduleRequest _
derive instance repGenericStopCrawlerScheduleRequest :: Generic StopCrawlerScheduleRequest _
instance showStopCrawlerScheduleRequest :: Show StopCrawlerScheduleRequest where
  show = genericShow
instance decodeStopCrawlerScheduleRequest :: Decode StopCrawlerScheduleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopCrawlerScheduleRequest :: Encode StopCrawlerScheduleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopCrawlerScheduleResponse = StopCrawlerScheduleResponse Types.NoArguments
derive instance newtypeStopCrawlerScheduleResponse :: Newtype StopCrawlerScheduleResponse _
derive instance repGenericStopCrawlerScheduleResponse :: Generic StopCrawlerScheduleResponse _
instance showStopCrawlerScheduleResponse :: Show StopCrawlerScheduleResponse where
  show = genericShow
instance decodeStopCrawlerScheduleResponse :: Decode StopCrawlerScheduleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopCrawlerScheduleResponse :: Encode StopCrawlerScheduleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopTriggerRequest = StopTriggerRequest 
  { "Name" :: (NameString)
  }
derive instance newtypeStopTriggerRequest :: Newtype StopTriggerRequest _
derive instance repGenericStopTriggerRequest :: Generic StopTriggerRequest _
instance showStopTriggerRequest :: Show StopTriggerRequest where
  show = genericShow
instance decodeStopTriggerRequest :: Decode StopTriggerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopTriggerRequest :: Encode StopTriggerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopTriggerResponse = StopTriggerResponse 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeStopTriggerResponse :: Newtype StopTriggerResponse _
derive instance repGenericStopTriggerResponse :: Generic StopTriggerResponse _
instance showStopTriggerResponse :: Show StopTriggerResponse where
  show = genericShow
instance decodeStopTriggerResponse :: Decode StopTriggerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopTriggerResponse :: Encode StopTriggerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the physical storage of table data.</p>
newtype StorageDescriptor = StorageDescriptor 
  { "Columns" :: NullOrUndefined.NullOrUndefined (ColumnList)
  , "Location" :: NullOrUndefined.NullOrUndefined (LocationString)
  , "InputFormat" :: NullOrUndefined.NullOrUndefined (FormatString)
  , "OutputFormat" :: NullOrUndefined.NullOrUndefined (FormatString)
  , "Compressed" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "NumberOfBuckets" :: NullOrUndefined.NullOrUndefined (Int)
  , "SerdeInfo" :: NullOrUndefined.NullOrUndefined (SerDeInfo)
  , "BucketColumns" :: NullOrUndefined.NullOrUndefined (NameStringList)
  , "SortColumns" :: NullOrUndefined.NullOrUndefined (OrderList)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "SkewedInfo" :: NullOrUndefined.NullOrUndefined (SkewedInfo)
  , "StoredAsSubDirectories" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeStorageDescriptor :: Newtype StorageDescriptor _
derive instance repGenericStorageDescriptor :: Generic StorageDescriptor _
instance showStorageDescriptor :: Show StorageDescriptor where
  show = genericShow
instance decodeStorageDescriptor :: Decode StorageDescriptor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageDescriptor :: Encode StorageDescriptor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringList = StringList (Array GenericString)
derive instance newtypeStringList :: Newtype StringList _
derive instance repGenericStringList :: Generic StringList _
instance showStringList :: Show StringList where
  show = genericShow
instance decodeStringList :: Decode StringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringList :: Encode StringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a collection of related data organized in columns and rows.</p>
newtype Table = Table 
  { "Name" :: (NameString)
  , "DatabaseName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "Owner" :: NullOrUndefined.NullOrUndefined (NameString)
  , "CreateTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "UpdateTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastAccessTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastAnalyzedTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Retention" :: NullOrUndefined.NullOrUndefined (NonNegativeInteger)
  , "StorageDescriptor" :: NullOrUndefined.NullOrUndefined (StorageDescriptor)
  , "PartitionKeys" :: NullOrUndefined.NullOrUndefined (ColumnList)
  , "ViewOriginalText" :: NullOrUndefined.NullOrUndefined (ViewTextString)
  , "ViewExpandedText" :: NullOrUndefined.NullOrUndefined (ViewTextString)
  , "TableType" :: NullOrUndefined.NullOrUndefined (TableTypeString)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  , "CreatedBy" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeTable :: Newtype Table _
derive instance repGenericTable :: Generic Table _
instance showTable :: Show Table where
  show = genericShow
instance decodeTable :: Decode Table where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTable :: Encode Table where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error record for table operations.</p>
newtype TableError = TableError 
  { "TableName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "ErrorDetail" :: NullOrUndefined.NullOrUndefined (ErrorDetail)
  }
derive instance newtypeTableError :: Newtype TableError _
derive instance repGenericTableError :: Generic TableError _
instance showTableError :: Show TableError where
  show = genericShow
instance decodeTableError :: Decode TableError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableError :: Encode TableError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TableErrors = TableErrors (Array TableError)
derive instance newtypeTableErrors :: Newtype TableErrors _
derive instance repGenericTableErrors :: Generic TableErrors _
instance showTableErrors :: Show TableErrors where
  show = genericShow
instance decodeTableErrors :: Decode TableErrors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableErrors :: Encode TableErrors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Structure used to create or update the table.</p>
newtype TableInput = TableInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "Owner" :: NullOrUndefined.NullOrUndefined (NameString)
  , "LastAccessTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastAnalyzedTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "Retention" :: NullOrUndefined.NullOrUndefined (NonNegativeInteger)
  , "StorageDescriptor" :: NullOrUndefined.NullOrUndefined (StorageDescriptor)
  , "PartitionKeys" :: NullOrUndefined.NullOrUndefined (ColumnList)
  , "ViewOriginalText" :: NullOrUndefined.NullOrUndefined (ViewTextString)
  , "ViewExpandedText" :: NullOrUndefined.NullOrUndefined (ViewTextString)
  , "TableType" :: NullOrUndefined.NullOrUndefined (TableTypeString)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ParametersMap)
  }
derive instance newtypeTableInput :: Newtype TableInput _
derive instance repGenericTableInput :: Generic TableInput _
instance showTableInput :: Show TableInput where
  show = genericShow
instance decodeTableInput :: Decode TableInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableInput :: Encode TableInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TableList = TableList (Array Table)
derive instance newtypeTableList :: Newtype TableList _
derive instance repGenericTableList :: Generic TableList _
instance showTableList :: Show TableList where
  show = genericShow
instance decodeTableList :: Decode TableList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableList :: Encode TableList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TableName = TableName String
derive instance newtypeTableName :: Newtype TableName _
derive instance repGenericTableName :: Generic TableName _
instance showTableName :: Show TableName where
  show = genericShow
instance decodeTableName :: Decode TableName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableName :: Encode TableName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TablePrefix = TablePrefix String
derive instance newtypeTablePrefix :: Newtype TablePrefix _
derive instance repGenericTablePrefix :: Generic TablePrefix _
instance showTablePrefix :: Show TablePrefix where
  show = genericShow
instance decodeTablePrefix :: Decode TablePrefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTablePrefix :: Encode TablePrefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TableTypeString = TableTypeString String
derive instance newtypeTableTypeString :: Newtype TableTypeString _
derive instance repGenericTableTypeString :: Generic TableTypeString _
instance showTableTypeString :: Show TableTypeString where
  show = genericShow
instance decodeTableTypeString :: Decode TableTypeString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableTypeString :: Encode TableTypeString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a version of a table.</p>
newtype TableVersion = TableVersion 
  { "Table" :: NullOrUndefined.NullOrUndefined (Table)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (VersionString)
  }
derive instance newtypeTableVersion :: Newtype TableVersion _
derive instance repGenericTableVersion :: Generic TableVersion _
instance showTableVersion :: Show TableVersion where
  show = genericShow
instance decodeTableVersion :: Decode TableVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableVersion :: Encode TableVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error record for table-version operations.</p>
newtype TableVersionError = TableVersionError 
  { "TableName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (VersionString)
  , "ErrorDetail" :: NullOrUndefined.NullOrUndefined (ErrorDetail)
  }
derive instance newtypeTableVersionError :: Newtype TableVersionError _
derive instance repGenericTableVersionError :: Generic TableVersionError _
instance showTableVersionError :: Show TableVersionError where
  show = genericShow
instance decodeTableVersionError :: Decode TableVersionError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableVersionError :: Encode TableVersionError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TableVersionErrors = TableVersionErrors (Array TableVersionError)
derive instance newtypeTableVersionErrors :: Newtype TableVersionErrors _
derive instance repGenericTableVersionErrors :: Generic TableVersionErrors _
instance showTableVersionErrors :: Show TableVersionErrors where
  show = genericShow
instance decodeTableVersionErrors :: Decode TableVersionErrors where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTableVersionErrors :: Encode TableVersionErrors where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimestampValue = TimestampValue Number
derive instance newtypeTimestampValue :: Newtype TimestampValue _
derive instance repGenericTimestampValue :: Generic TimestampValue _
instance showTimestampValue :: Show TimestampValue where
  show = genericShow
instance decodeTimestampValue :: Decode TimestampValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimestampValue :: Encode TimestampValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Token = Token String
derive instance newtypeToken :: Newtype Token _
derive instance repGenericToken :: Generic Token _
instance showToken :: Show Token where
  show = genericShow
instance decodeToken :: Decode Token where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeToken :: Encode Token where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TotalSegmentsInteger = TotalSegmentsInteger Int
derive instance newtypeTotalSegmentsInteger :: Newtype TotalSegmentsInteger _
derive instance repGenericTotalSegmentsInteger :: Generic TotalSegmentsInteger _
instance showTotalSegmentsInteger :: Show TotalSegmentsInteger where
  show = genericShow
instance decodeTotalSegmentsInteger :: Decode TotalSegmentsInteger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTotalSegmentsInteger :: Encode TotalSegmentsInteger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a specific trigger.</p>
newtype Trigger = Trigger 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  , "Id" :: NullOrUndefined.NullOrUndefined (IdString)
  , "Type" :: NullOrUndefined.NullOrUndefined (TriggerType)
  , "State" :: NullOrUndefined.NullOrUndefined (TriggerState)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "Actions" :: NullOrUndefined.NullOrUndefined (ActionList)
  , "Predicate" :: NullOrUndefined.NullOrUndefined (Predicate)
  }
derive instance newtypeTrigger :: Newtype Trigger _
derive instance repGenericTrigger :: Generic Trigger _
instance showTrigger :: Show Trigger where
  show = genericShow
instance decodeTrigger :: Decode Trigger where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrigger :: Encode Trigger where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TriggerList = TriggerList (Array Trigger)
derive instance newtypeTriggerList :: Newtype TriggerList _
derive instance repGenericTriggerList :: Generic TriggerList _
instance showTriggerList :: Show TriggerList where
  show = genericShow
instance decodeTriggerList :: Decode TriggerList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTriggerList :: Encode TriggerList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TriggerState = TriggerState String
derive instance newtypeTriggerState :: Newtype TriggerState _
derive instance repGenericTriggerState :: Generic TriggerState _
instance showTriggerState :: Show TriggerState where
  show = genericShow
instance decodeTriggerState :: Decode TriggerState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTriggerState :: Encode TriggerState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TriggerType = TriggerType String
derive instance newtypeTriggerType :: Newtype TriggerType _
derive instance repGenericTriggerType :: Generic TriggerType _
instance showTriggerType :: Show TriggerType where
  show = genericShow
instance decodeTriggerType :: Decode TriggerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTriggerType :: Encode TriggerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure used to provide information used to update a trigger. This object will update the the previous trigger definition by overwriting it completely.</p>
newtype TriggerUpdate = TriggerUpdate 
  { "Name" :: NullOrUndefined.NullOrUndefined (NameString)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionString)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "Actions" :: NullOrUndefined.NullOrUndefined (ActionList)
  , "Predicate" :: NullOrUndefined.NullOrUndefined (Predicate)
  }
derive instance newtypeTriggerUpdate :: Newtype TriggerUpdate _
derive instance repGenericTriggerUpdate :: Generic TriggerUpdate _
instance showTriggerUpdate :: Show TriggerUpdate where
  show = genericShow
instance decodeTriggerUpdate :: Decode TriggerUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTriggerUpdate :: Encode TriggerUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype URI = URI String
derive instance newtypeURI :: Newtype URI _
derive instance repGenericURI :: Generic URI _
instance showURI :: Show URI where
  show = genericShow
instance decodeURI :: Decode URI where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeURI :: Encode URI where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateBehavior = UpdateBehavior String
derive instance newtypeUpdateBehavior :: Newtype UpdateBehavior _
derive instance repGenericUpdateBehavior :: Generic UpdateBehavior _
instance showUpdateBehavior :: Show UpdateBehavior where
  show = genericShow
instance decodeUpdateBehavior :: Decode UpdateBehavior where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateBehavior :: Encode UpdateBehavior where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateClassifierRequest = UpdateClassifierRequest 
  { "GrokClassifier" :: NullOrUndefined.NullOrUndefined (UpdateGrokClassifierRequest)
  , "XMLClassifier" :: NullOrUndefined.NullOrUndefined (UpdateXMLClassifierRequest)
  , "JsonClassifier" :: NullOrUndefined.NullOrUndefined (UpdateJsonClassifierRequest)
  }
derive instance newtypeUpdateClassifierRequest :: Newtype UpdateClassifierRequest _
derive instance repGenericUpdateClassifierRequest :: Generic UpdateClassifierRequest _
instance showUpdateClassifierRequest :: Show UpdateClassifierRequest where
  show = genericShow
instance decodeUpdateClassifierRequest :: Decode UpdateClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateClassifierRequest :: Encode UpdateClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateClassifierResponse = UpdateClassifierResponse Types.NoArguments
derive instance newtypeUpdateClassifierResponse :: Newtype UpdateClassifierResponse _
derive instance repGenericUpdateClassifierResponse :: Generic UpdateClassifierResponse _
instance showUpdateClassifierResponse :: Show UpdateClassifierResponse where
  show = genericShow
instance decodeUpdateClassifierResponse :: Decode UpdateClassifierResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateClassifierResponse :: Encode UpdateClassifierResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateConnectionRequest = UpdateConnectionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  , "ConnectionInput" :: (ConnectionInput)
  }
derive instance newtypeUpdateConnectionRequest :: Newtype UpdateConnectionRequest _
derive instance repGenericUpdateConnectionRequest :: Generic UpdateConnectionRequest _
instance showUpdateConnectionRequest :: Show UpdateConnectionRequest where
  show = genericShow
instance decodeUpdateConnectionRequest :: Decode UpdateConnectionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConnectionRequest :: Encode UpdateConnectionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateConnectionResponse = UpdateConnectionResponse Types.NoArguments
derive instance newtypeUpdateConnectionResponse :: Newtype UpdateConnectionResponse _
derive instance repGenericUpdateConnectionResponse :: Generic UpdateConnectionResponse _
instance showUpdateConnectionResponse :: Show UpdateConnectionResponse where
  show = genericShow
instance decodeUpdateConnectionResponse :: Decode UpdateConnectionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConnectionResponse :: Encode UpdateConnectionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCrawlerRequest = UpdateCrawlerRequest 
  { "Name" :: (NameString)
  , "Role" :: NullOrUndefined.NullOrUndefined (Role)
  , "DatabaseName" :: NullOrUndefined.NullOrUndefined (DatabaseName)
  , "Description" :: NullOrUndefined.NullOrUndefined (DescriptionStringRemovable)
  , "Targets" :: NullOrUndefined.NullOrUndefined (CrawlerTargets)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (CronExpression)
  , "Classifiers" :: NullOrUndefined.NullOrUndefined (ClassifierNameList)
  , "TablePrefix" :: NullOrUndefined.NullOrUndefined (TablePrefix)
  , "SchemaChangePolicy" :: NullOrUndefined.NullOrUndefined (SchemaChangePolicy)
  , "Configuration" :: NullOrUndefined.NullOrUndefined (CrawlerConfiguration)
  }
derive instance newtypeUpdateCrawlerRequest :: Newtype UpdateCrawlerRequest _
derive instance repGenericUpdateCrawlerRequest :: Generic UpdateCrawlerRequest _
instance showUpdateCrawlerRequest :: Show UpdateCrawlerRequest where
  show = genericShow
instance decodeUpdateCrawlerRequest :: Decode UpdateCrawlerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCrawlerRequest :: Encode UpdateCrawlerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCrawlerResponse = UpdateCrawlerResponse Types.NoArguments
derive instance newtypeUpdateCrawlerResponse :: Newtype UpdateCrawlerResponse _
derive instance repGenericUpdateCrawlerResponse :: Generic UpdateCrawlerResponse _
instance showUpdateCrawlerResponse :: Show UpdateCrawlerResponse where
  show = genericShow
instance decodeUpdateCrawlerResponse :: Decode UpdateCrawlerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCrawlerResponse :: Encode UpdateCrawlerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCrawlerScheduleRequest = UpdateCrawlerScheduleRequest 
  { "CrawlerName" :: (NameString)
  , "Schedule" :: NullOrUndefined.NullOrUndefined (CronExpression)
  }
derive instance newtypeUpdateCrawlerScheduleRequest :: Newtype UpdateCrawlerScheduleRequest _
derive instance repGenericUpdateCrawlerScheduleRequest :: Generic UpdateCrawlerScheduleRequest _
instance showUpdateCrawlerScheduleRequest :: Show UpdateCrawlerScheduleRequest where
  show = genericShow
instance decodeUpdateCrawlerScheduleRequest :: Decode UpdateCrawlerScheduleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCrawlerScheduleRequest :: Encode UpdateCrawlerScheduleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateCrawlerScheduleResponse = UpdateCrawlerScheduleResponse Types.NoArguments
derive instance newtypeUpdateCrawlerScheduleResponse :: Newtype UpdateCrawlerScheduleResponse _
derive instance repGenericUpdateCrawlerScheduleResponse :: Generic UpdateCrawlerScheduleResponse _
instance showUpdateCrawlerScheduleResponse :: Show UpdateCrawlerScheduleResponse where
  show = genericShow
instance decodeUpdateCrawlerScheduleResponse :: Decode UpdateCrawlerScheduleResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateCrawlerScheduleResponse :: Encode UpdateCrawlerScheduleResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDatabaseRequest = UpdateDatabaseRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "Name" :: (NameString)
  , "DatabaseInput" :: (DatabaseInput)
  }
derive instance newtypeUpdateDatabaseRequest :: Newtype UpdateDatabaseRequest _
derive instance repGenericUpdateDatabaseRequest :: Generic UpdateDatabaseRequest _
instance showUpdateDatabaseRequest :: Show UpdateDatabaseRequest where
  show = genericShow
instance decodeUpdateDatabaseRequest :: Decode UpdateDatabaseRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDatabaseRequest :: Encode UpdateDatabaseRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDatabaseResponse = UpdateDatabaseResponse Types.NoArguments
derive instance newtypeUpdateDatabaseResponse :: Newtype UpdateDatabaseResponse _
derive instance repGenericUpdateDatabaseResponse :: Generic UpdateDatabaseResponse _
instance showUpdateDatabaseResponse :: Show UpdateDatabaseResponse where
  show = genericShow
instance decodeUpdateDatabaseResponse :: Decode UpdateDatabaseResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDatabaseResponse :: Encode UpdateDatabaseResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDevEndpointRequest = UpdateDevEndpointRequest 
  { "EndpointName" :: (GenericString)
  , "PublicKey" :: NullOrUndefined.NullOrUndefined (GenericString)
  , "CustomLibraries" :: NullOrUndefined.NullOrUndefined (DevEndpointCustomLibraries)
  , "UpdateEtlLibraries" :: NullOrUndefined.NullOrUndefined (BooleanValue)
  }
derive instance newtypeUpdateDevEndpointRequest :: Newtype UpdateDevEndpointRequest _
derive instance repGenericUpdateDevEndpointRequest :: Generic UpdateDevEndpointRequest _
instance showUpdateDevEndpointRequest :: Show UpdateDevEndpointRequest where
  show = genericShow
instance decodeUpdateDevEndpointRequest :: Decode UpdateDevEndpointRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDevEndpointRequest :: Encode UpdateDevEndpointRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDevEndpointResponse = UpdateDevEndpointResponse Types.NoArguments
derive instance newtypeUpdateDevEndpointResponse :: Newtype UpdateDevEndpointResponse _
derive instance repGenericUpdateDevEndpointResponse :: Generic UpdateDevEndpointResponse _
instance showUpdateDevEndpointResponse :: Show UpdateDevEndpointResponse where
  show = genericShow
instance decodeUpdateDevEndpointResponse :: Decode UpdateDevEndpointResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDevEndpointResponse :: Encode UpdateDevEndpointResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a grok classifier to update when passed to <code>UpdateClassifier</code>.</p>
newtype UpdateGrokClassifierRequest = UpdateGrokClassifierRequest 
  { "Name" :: (NameString)
  , "Classification" :: NullOrUndefined.NullOrUndefined (Classification)
  , "GrokPattern" :: NullOrUndefined.NullOrUndefined (GrokPattern)
  , "CustomPatterns" :: NullOrUndefined.NullOrUndefined (CustomPatterns)
  }
derive instance newtypeUpdateGrokClassifierRequest :: Newtype UpdateGrokClassifierRequest _
derive instance repGenericUpdateGrokClassifierRequest :: Generic UpdateGrokClassifierRequest _
instance showUpdateGrokClassifierRequest :: Show UpdateGrokClassifierRequest where
  show = genericShow
instance decodeUpdateGrokClassifierRequest :: Decode UpdateGrokClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateGrokClassifierRequest :: Encode UpdateGrokClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateJobRequest = UpdateJobRequest 
  { "JobName" :: (NameString)
  , "JobUpdate" :: (JobUpdate)
  }
derive instance newtypeUpdateJobRequest :: Newtype UpdateJobRequest _
derive instance repGenericUpdateJobRequest :: Generic UpdateJobRequest _
instance showUpdateJobRequest :: Show UpdateJobRequest where
  show = genericShow
instance decodeUpdateJobRequest :: Decode UpdateJobRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobRequest :: Encode UpdateJobRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateJobResponse = UpdateJobResponse 
  { "JobName" :: NullOrUndefined.NullOrUndefined (NameString)
  }
derive instance newtypeUpdateJobResponse :: Newtype UpdateJobResponse _
derive instance repGenericUpdateJobResponse :: Generic UpdateJobResponse _
instance showUpdateJobResponse :: Show UpdateJobResponse where
  show = genericShow
instance decodeUpdateJobResponse :: Decode UpdateJobResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobResponse :: Encode UpdateJobResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies a JSON classifier to be updated.</p>
newtype UpdateJsonClassifierRequest = UpdateJsonClassifierRequest 
  { "Name" :: (NameString)
  , "JsonPath" :: NullOrUndefined.NullOrUndefined (JsonPath)
  }
derive instance newtypeUpdateJsonClassifierRequest :: Newtype UpdateJsonClassifierRequest _
derive instance repGenericUpdateJsonClassifierRequest :: Generic UpdateJsonClassifierRequest _
instance showUpdateJsonClassifierRequest :: Show UpdateJsonClassifierRequest where
  show = genericShow
instance decodeUpdateJsonClassifierRequest :: Decode UpdateJsonClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJsonClassifierRequest :: Encode UpdateJsonClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePartitionRequest = UpdatePartitionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableName" :: (NameString)
  , "PartitionValueList" :: (BoundedPartitionValueList)
  , "PartitionInput" :: (PartitionInput)
  }
derive instance newtypeUpdatePartitionRequest :: Newtype UpdatePartitionRequest _
derive instance repGenericUpdatePartitionRequest :: Generic UpdatePartitionRequest _
instance showUpdatePartitionRequest :: Show UpdatePartitionRequest where
  show = genericShow
instance decodeUpdatePartitionRequest :: Decode UpdatePartitionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePartitionRequest :: Encode UpdatePartitionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdatePartitionResponse = UpdatePartitionResponse Types.NoArguments
derive instance newtypeUpdatePartitionResponse :: Newtype UpdatePartitionResponse _
derive instance repGenericUpdatePartitionResponse :: Generic UpdatePartitionResponse _
instance showUpdatePartitionResponse :: Show UpdatePartitionResponse where
  show = genericShow
instance decodeUpdatePartitionResponse :: Decode UpdatePartitionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePartitionResponse :: Encode UpdatePartitionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTableRequest = UpdateTableRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "TableInput" :: (TableInput)
  , "SkipArchive" :: NullOrUndefined.NullOrUndefined (BooleanNullable)
  }
derive instance newtypeUpdateTableRequest :: Newtype UpdateTableRequest _
derive instance repGenericUpdateTableRequest :: Generic UpdateTableRequest _
instance showUpdateTableRequest :: Show UpdateTableRequest where
  show = genericShow
instance decodeUpdateTableRequest :: Decode UpdateTableRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTableRequest :: Encode UpdateTableRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTableResponse = UpdateTableResponse Types.NoArguments
derive instance newtypeUpdateTableResponse :: Newtype UpdateTableResponse _
derive instance repGenericUpdateTableResponse :: Generic UpdateTableResponse _
instance showUpdateTableResponse :: Show UpdateTableResponse where
  show = genericShow
instance decodeUpdateTableResponse :: Decode UpdateTableResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTableResponse :: Encode UpdateTableResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTriggerRequest = UpdateTriggerRequest 
  { "Name" :: (NameString)
  , "TriggerUpdate" :: (TriggerUpdate)
  }
derive instance newtypeUpdateTriggerRequest :: Newtype UpdateTriggerRequest _
derive instance repGenericUpdateTriggerRequest :: Generic UpdateTriggerRequest _
instance showUpdateTriggerRequest :: Show UpdateTriggerRequest where
  show = genericShow
instance decodeUpdateTriggerRequest :: Decode UpdateTriggerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTriggerRequest :: Encode UpdateTriggerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateTriggerResponse = UpdateTriggerResponse 
  { "Trigger" :: NullOrUndefined.NullOrUndefined (Trigger)
  }
derive instance newtypeUpdateTriggerResponse :: Newtype UpdateTriggerResponse _
derive instance repGenericUpdateTriggerResponse :: Generic UpdateTriggerResponse _
instance showUpdateTriggerResponse :: Show UpdateTriggerResponse where
  show = genericShow
instance decodeUpdateTriggerResponse :: Decode UpdateTriggerResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateTriggerResponse :: Encode UpdateTriggerResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateUserDefinedFunctionRequest = UpdateUserDefinedFunctionRequest 
  { "CatalogId" :: NullOrUndefined.NullOrUndefined (CatalogIdString)
  , "DatabaseName" :: (NameString)
  , "FunctionName" :: (NameString)
  , "FunctionInput" :: (UserDefinedFunctionInput)
  }
derive instance newtypeUpdateUserDefinedFunctionRequest :: Newtype UpdateUserDefinedFunctionRequest _
derive instance repGenericUpdateUserDefinedFunctionRequest :: Generic UpdateUserDefinedFunctionRequest _
instance showUpdateUserDefinedFunctionRequest :: Show UpdateUserDefinedFunctionRequest where
  show = genericShow
instance decodeUpdateUserDefinedFunctionRequest :: Decode UpdateUserDefinedFunctionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserDefinedFunctionRequest :: Encode UpdateUserDefinedFunctionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateUserDefinedFunctionResponse = UpdateUserDefinedFunctionResponse Types.NoArguments
derive instance newtypeUpdateUserDefinedFunctionResponse :: Newtype UpdateUserDefinedFunctionResponse _
derive instance repGenericUpdateUserDefinedFunctionResponse :: Generic UpdateUserDefinedFunctionResponse _
instance showUpdateUserDefinedFunctionResponse :: Show UpdateUserDefinedFunctionResponse where
  show = genericShow
instance decodeUpdateUserDefinedFunctionResponse :: Decode UpdateUserDefinedFunctionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserDefinedFunctionResponse :: Encode UpdateUserDefinedFunctionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Specifies an XML classifier to be updated.</p>
newtype UpdateXMLClassifierRequest = UpdateXMLClassifierRequest 
  { "Name" :: (NameString)
  , "Classification" :: NullOrUndefined.NullOrUndefined (Classification)
  , "RowTag" :: NullOrUndefined.NullOrUndefined (RowTag)
  }
derive instance newtypeUpdateXMLClassifierRequest :: Newtype UpdateXMLClassifierRequest _
derive instance repGenericUpdateXMLClassifierRequest :: Generic UpdateXMLClassifierRequest _
instance showUpdateXMLClassifierRequest :: Show UpdateXMLClassifierRequest where
  show = genericShow
instance decodeUpdateXMLClassifierRequest :: Decode UpdateXMLClassifierRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateXMLClassifierRequest :: Encode UpdateXMLClassifierRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UriString = UriString String
derive instance newtypeUriString :: Newtype UriString _
derive instance repGenericUriString :: Generic UriString _
instance showUriString :: Show UriString where
  show = genericShow
instance decodeUriString :: Decode UriString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUriString :: Encode UriString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the equivalent of a Hive user-defined function (<code>UDF</code>) definition.</p>
newtype UserDefinedFunction = UserDefinedFunction 
  { "FunctionName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "ClassName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "OwnerName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "OwnerType" :: NullOrUndefined.NullOrUndefined (PrincipalType)
  , "CreateTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "ResourceUris" :: NullOrUndefined.NullOrUndefined (ResourceUriList)
  }
derive instance newtypeUserDefinedFunction :: Newtype UserDefinedFunction _
derive instance repGenericUserDefinedFunction :: Generic UserDefinedFunction _
instance showUserDefinedFunction :: Show UserDefinedFunction where
  show = genericShow
instance decodeUserDefinedFunction :: Decode UserDefinedFunction where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserDefinedFunction :: Encode UserDefinedFunction where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A structure used to create or updata a user-defined function.</p>
newtype UserDefinedFunctionInput = UserDefinedFunctionInput 
  { "FunctionName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "ClassName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "OwnerName" :: NullOrUndefined.NullOrUndefined (NameString)
  , "OwnerType" :: NullOrUndefined.NullOrUndefined (PrincipalType)
  , "ResourceUris" :: NullOrUndefined.NullOrUndefined (ResourceUriList)
  }
derive instance newtypeUserDefinedFunctionInput :: Newtype UserDefinedFunctionInput _
derive instance repGenericUserDefinedFunctionInput :: Generic UserDefinedFunctionInput _
instance showUserDefinedFunctionInput :: Show UserDefinedFunctionInput where
  show = genericShow
instance decodeUserDefinedFunctionInput :: Decode UserDefinedFunctionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserDefinedFunctionInput :: Encode UserDefinedFunctionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserDefinedFunctionList = UserDefinedFunctionList (Array UserDefinedFunction)
derive instance newtypeUserDefinedFunctionList :: Newtype UserDefinedFunctionList _
derive instance repGenericUserDefinedFunctionList :: Generic UserDefinedFunctionList _
instance showUserDefinedFunctionList :: Show UserDefinedFunctionList where
  show = genericShow
instance decodeUserDefinedFunctionList :: Decode UserDefinedFunctionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserDefinedFunctionList :: Encode UserDefinedFunctionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A value could not be validated.</p>
newtype ValidationException = ValidationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeValidationException :: Newtype ValidationException _
derive instance repGenericValidationException :: Generic ValidationException _
instance showValidationException :: Show ValidationException where
  show = genericShow
instance decodeValidationException :: Decode ValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationException :: Encode ValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValueString = ValueString String
derive instance newtypeValueString :: Newtype ValueString _
derive instance repGenericValueString :: Generic ValueString _
instance showValueString :: Show ValueString where
  show = genericShow
instance decodeValueString :: Decode ValueString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValueString :: Encode ValueString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValueStringList = ValueStringList (Array ValueString)
derive instance newtypeValueStringList :: Newtype ValueStringList _
derive instance repGenericValueStringList :: Generic ValueStringList _
instance showValueStringList :: Show ValueStringList where
  show = genericShow
instance decodeValueStringList :: Decode ValueStringList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValueStringList :: Encode ValueStringList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VersionId = VersionId Number
derive instance newtypeVersionId :: Newtype VersionId _
derive instance repGenericVersionId :: Generic VersionId _
instance showVersionId :: Show VersionId where
  show = genericShow
instance decodeVersionId :: Decode VersionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionId :: Encode VersionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There was a version conflict.</p>
newtype VersionMismatchException = VersionMismatchException 
  { "Message" :: NullOrUndefined.NullOrUndefined (MessageString)
  }
derive instance newtypeVersionMismatchException :: Newtype VersionMismatchException _
derive instance repGenericVersionMismatchException :: Generic VersionMismatchException _
instance showVersionMismatchException :: Show VersionMismatchException where
  show = genericShow
instance decodeVersionMismatchException :: Decode VersionMismatchException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionMismatchException :: Encode VersionMismatchException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VersionString = VersionString String
derive instance newtypeVersionString :: Newtype VersionString _
derive instance repGenericVersionString :: Generic VersionString _
instance showVersionString :: Show VersionString where
  show = genericShow
instance decodeVersionString :: Decode VersionString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionString :: Encode VersionString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ViewTextString = ViewTextString String
derive instance newtypeViewTextString :: Newtype ViewTextString _
derive instance repGenericViewTextString :: Generic ViewTextString _
instance showViewTextString :: Show ViewTextString where
  show = genericShow
instance decodeViewTextString :: Decode ViewTextString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeViewTextString :: Encode ViewTextString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A classifier for <code>XML</code> content.</p>
newtype XMLClassifier = XMLClassifier 
  { "Name" :: (NameString)
  , "Classification" :: (Classification)
  , "CreationTime" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdated" :: NullOrUndefined.NullOrUndefined (Number)
  , "Version" :: NullOrUndefined.NullOrUndefined (VersionId)
  , "RowTag" :: NullOrUndefined.NullOrUndefined (RowTag)
  }
derive instance newtypeXMLClassifier :: Newtype XMLClassifier _
derive instance repGenericXMLClassifier :: Generic XMLClassifier _
instance showXMLClassifier :: Show XMLClassifier where
  show = genericShow
instance decodeXMLClassifier :: Decode XMLClassifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeXMLClassifier :: Encode XMLClassifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
