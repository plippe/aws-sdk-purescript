

-- | <p>Amazon Athena is an interactive query service that lets you use standard SQL to analyze data directly in Amazon S3. You can point Athena at your data in Amazon S3 and run ad-hoc queries and get results in seconds. Athena is serverless, so there is no infrastructure to set up or manage. You pay only for the queries you run. Athena scales automatically—executing queries in parallel—so results are fast, even with large datasets and complex queries. For more information, see <a href="http://docs.aws.amazon.com/athena/latest/ug/what-is.html">What is Amazon Athena</a> in the <i>Amazon Athena User Guide</i>.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>
module AWS.Athena where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Athena" :: String


-- | <p>Returns the details of a single named query or a list of up to 50 queries, which you provide as an array of query ID strings. Use <a>ListNamedQueries</a> to get the list of named query IDs. If information could not be retrieved for a submitted query ID, information about the query ID submitted is listed under <a>UnprocessedNamedQueryId</a>. Named queries are different from executed queries. Use <a>BatchGetQueryExecution</a> to get details about each unique query execution, and <a>ListQueryExecutions</a> to get a list of query execution IDs.</p>
batchGetNamedQuery :: forall eff. BatchGetNamedQueryInput -> Aff (err :: AWS.RequestError | eff) BatchGetNamedQueryOutput
batchGetNamedQuery = AWS.request serviceName "BatchGetNamedQuery" 


-- | <p>Returns the details of a single query execution or a list of up to 50 query executions, which you provide as an array of query execution ID strings. To get a list of query execution IDs, use <a>ListQueryExecutions</a>. Query executions are different from named (saved) queries. Use <a>BatchGetNamedQuery</a> to get details about named queries.</p>
batchGetQueryExecution :: forall eff. BatchGetQueryExecutionInput -> Aff (err :: AWS.RequestError | eff) BatchGetQueryExecutionOutput
batchGetQueryExecution = AWS.request serviceName "BatchGetQueryExecution" 


-- | <p>Creates a named query.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>
createNamedQuery :: forall eff. CreateNamedQueryInput -> Aff (err :: AWS.RequestError | eff) CreateNamedQueryOutput
createNamedQuery = AWS.request serviceName "CreateNamedQuery" 


-- | <p>Deletes a named query.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>
deleteNamedQuery :: forall eff. DeleteNamedQueryInput -> Aff (err :: AWS.RequestError | eff) DeleteNamedQueryOutput
deleteNamedQuery = AWS.request serviceName "DeleteNamedQuery" 


-- | <p>Returns information about a single query.</p>
getNamedQuery :: forall eff. GetNamedQueryInput -> Aff (err :: AWS.RequestError | eff) GetNamedQueryOutput
getNamedQuery = AWS.request serviceName "GetNamedQuery" 


-- | <p>Returns information about a single execution of a query. Each time a query executes, information about the query execution is saved with a unique ID.</p>
getQueryExecution :: forall eff. GetQueryExecutionInput -> Aff (err :: AWS.RequestError | eff) GetQueryExecutionOutput
getQueryExecution = AWS.request serviceName "GetQueryExecution" 


-- | <p>Returns the results of a single query execution specified by <code>QueryExecutionId</code>. This request does not execute the query but returns results. Use <a>StartQueryExecution</a> to run a query.</p>
getQueryResults :: forall eff. GetQueryResultsInput -> Aff (err :: AWS.RequestError | eff) GetQueryResultsOutput
getQueryResults = AWS.request serviceName "GetQueryResults" 


-- | <p>Provides a list of all available query IDs.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>
listNamedQueries :: forall eff. ListNamedQueriesInput -> Aff (err :: AWS.RequestError | eff) ListNamedQueriesOutput
listNamedQueries = AWS.request serviceName "ListNamedQueries" 


-- | <p>Provides a list of all available query execution IDs.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>
listQueryExecutions :: forall eff. ListQueryExecutionsInput -> Aff (err :: AWS.RequestError | eff) ListQueryExecutionsOutput
listQueryExecutions = AWS.request serviceName "ListQueryExecutions" 


-- | <p>Runs (executes) the SQL query statements contained in the <code>Query</code> string.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>
startQueryExecution :: forall eff. StartQueryExecutionInput -> Aff (err :: AWS.RequestError | eff) StartQueryExecutionOutput
startQueryExecution = AWS.request serviceName "StartQueryExecution" 


-- | <p>Stops a query execution.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>
stopQueryExecution :: forall eff. StopQueryExecutionInput -> Aff (err :: AWS.RequestError | eff) StopQueryExecutionOutput
stopQueryExecution = AWS.request serviceName "StopQueryExecution" 


newtype BatchGetNamedQueryInput = BatchGetNamedQueryInput 
  { "NamedQueryIds" :: (NamedQueryIdList)
  }


newtype BatchGetNamedQueryOutput = BatchGetNamedQueryOutput 
  { "NamedQueries" :: NullOrUndefined (NamedQueryList)
  , "UnprocessedNamedQueryIds" :: NullOrUndefined (UnprocessedNamedQueryIdList)
  }


newtype BatchGetQueryExecutionInput = BatchGetQueryExecutionInput 
  { "QueryExecutionIds" :: (QueryExecutionIdList)
  }


newtype BatchGetQueryExecutionOutput = BatchGetQueryExecutionOutput 
  { "QueryExecutions" :: NullOrUndefined (QueryExecutionList)
  , "UnprocessedQueryExecutionIds" :: NullOrUndefined (UnprocessedQueryExecutionIdList)
  }


-- | <p>Information about the columns in a query execution result.</p>
newtype ColumnInfo = ColumnInfo 
  { "CatalogName" :: NullOrUndefined (String)
  , "SchemaName" :: NullOrUndefined (String)
  , "TableName" :: NullOrUndefined (String)
  , "Name" :: (String)
  , "Label" :: NullOrUndefined (String)
  , "Type" :: (String)
  , "Precision" :: NullOrUndefined (Int)
  , "Scale" :: NullOrUndefined (Int)
  , "Nullable" :: NullOrUndefined (ColumnNullable)
  , "CaseSensitive" :: NullOrUndefined (Boolean)
  }


newtype ColumnInfoList = ColumnInfoList (Array ColumnInfo)


newtype ColumnNullable = ColumnNullable String


newtype CreateNamedQueryInput = CreateNamedQueryInput 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Database" :: (DatabaseString)
  , "QueryString" :: (QueryString)
  , "ClientRequestToken" :: NullOrUndefined (IdempotencyToken)
  }


newtype CreateNamedQueryOutput = CreateNamedQueryOutput 
  { "NamedQueryId" :: NullOrUndefined (NamedQueryId)
  }


newtype DatabaseString = DatabaseString String


newtype Date = Date Number


-- | <p>A piece of data (a field in the table).</p>
newtype Datum = Datum 
  { "VarCharValue" :: NullOrUndefined (DatumString')
  }


newtype DeleteNamedQueryInput = DeleteNamedQueryInput 
  { "NamedQueryId" :: (NamedQueryId)
  }


newtype DeleteNamedQueryOutput = DeleteNamedQueryOutput 
  { 
  }


newtype DescriptionString = DescriptionString String


-- | <p>If query results are encrypted in Amazon S3, indicates the Amazon S3 encryption option used.</p>
newtype EncryptionConfiguration = EncryptionConfiguration 
  { "EncryptionOption" :: (EncryptionOption)
  , "KmsKey" :: NullOrUndefined (String)
  }


newtype EncryptionOption = EncryptionOption String


newtype ErrorCode = ErrorCode String


newtype ErrorMessage = ErrorMessage String


newtype GetNamedQueryInput = GetNamedQueryInput 
  { "NamedQueryId" :: (NamedQueryId)
  }


newtype GetNamedQueryOutput = GetNamedQueryOutput 
  { "NamedQuery" :: NullOrUndefined (NamedQuery)
  }


newtype GetQueryExecutionInput = GetQueryExecutionInput 
  { "QueryExecutionId" :: (QueryExecutionId)
  }


newtype GetQueryExecutionOutput = GetQueryExecutionOutput 
  { "QueryExecution" :: NullOrUndefined (QueryExecution)
  }


newtype GetQueryResultsInput = GetQueryResultsInput 
  { "QueryExecutionId" :: (QueryExecutionId)
  , "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxQueryResults)
  }


newtype GetQueryResultsOutput = GetQueryResultsOutput 
  { "ResultSet" :: NullOrUndefined (ResultSet)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype IdempotencyToken = IdempotencyToken String


-- | <p>Indicates a platform issue, which may be due to a transient condition or outage.</p>
newtype InternalServerException = InternalServerException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Indicates that something is wrong with the input to the request. For example, a required parameter may be missing or out of range.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "AthenaErrorCode" :: NullOrUndefined (ErrorCode)
  , "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ListNamedQueriesInput = ListNamedQueriesInput 
  { "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxNamedQueriesCount)
  }


newtype ListNamedQueriesOutput = ListNamedQueriesOutput 
  { "NamedQueryIds" :: NullOrUndefined (NamedQueryIdList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype ListQueryExecutionsInput = ListQueryExecutionsInput 
  { "NextToken" :: NullOrUndefined (Token)
  , "MaxResults" :: NullOrUndefined (MaxQueryExecutionsCount)
  }


newtype ListQueryExecutionsOutput = ListQueryExecutionsOutput 
  { "QueryExecutionIds" :: NullOrUndefined (QueryExecutionIdList)
  , "NextToken" :: NullOrUndefined (Token)
  }


newtype MaxNamedQueriesCount = MaxNamedQueriesCount Int


newtype MaxQueryExecutionsCount = MaxQueryExecutionsCount Int


newtype MaxQueryResults = MaxQueryResults Int


newtype NameString = NameString String


-- | <p>A query, where <code>QueryString</code> is the SQL query statements that comprise the query.</p>
newtype NamedQuery = NamedQuery 
  { "Name" :: (NameString)
  , "Description" :: NullOrUndefined (DescriptionString)
  , "Database" :: (DatabaseString)
  , "QueryString" :: (QueryString)
  , "NamedQueryId" :: NullOrUndefined (NamedQueryId)
  }


newtype NamedQueryId = NamedQueryId String


newtype NamedQueryIdList = NamedQueryIdList (Array NamedQueryId)


newtype NamedQueryList = NamedQueryList (Array NamedQuery)


-- | <p>Information about a single instance of a query execution.</p>
newtype QueryExecution = QueryExecution 
  { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId)
  , "Query" :: NullOrUndefined (QueryString)
  , "ResultConfiguration" :: NullOrUndefined (ResultConfiguration)
  , "QueryExecutionContext" :: NullOrUndefined (QueryExecutionContext)
  , "Status" :: NullOrUndefined (QueryExecutionStatus)
  , "Statistics" :: NullOrUndefined (QueryExecutionStatistics)
  }


-- | <p>The database in which the query execution occurs.</p>
newtype QueryExecutionContext = QueryExecutionContext 
  { "Database" :: NullOrUndefined (DatabaseString)
  }


newtype QueryExecutionId = QueryExecutionId String


newtype QueryExecutionIdList = QueryExecutionIdList (Array QueryExecutionId)


newtype QueryExecutionList = QueryExecutionList (Array QueryExecution)


newtype QueryExecutionState = QueryExecutionState String


-- | <p>The amount of data scanned during the query execution and the amount of time that it took to execute.</p>
newtype QueryExecutionStatistics = QueryExecutionStatistics 
  { "EngineExecutionTimeInMillis" :: NullOrUndefined (Number)
  , "DataScannedInBytes" :: NullOrUndefined (Number)
  }


-- | <p>The completion date, current state, submission time, and state change reason (if applicable) for the query execution.</p>
newtype QueryExecutionStatus = QueryExecutionStatus 
  { "State" :: NullOrUndefined (QueryExecutionState)
  , "StateChangeReason" :: NullOrUndefined (String)
  , "SubmissionDateTime" :: NullOrUndefined (Date)
  , "CompletionDateTime" :: NullOrUndefined (Date)
  }


newtype QueryString = QueryString String


-- | <p>The location in Amazon S3 where query results are stored and the encryption option, if any, used for query results.</p>
newtype ResultConfiguration = ResultConfiguration 
  { "OutputLocation" :: (String)
  , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration)
  }


-- | <p>The metadata and rows that comprise a query result set. The metadata describes the column structure and data types.</p>
newtype ResultSet = ResultSet 
  { "Rows" :: NullOrUndefined (RowList)
  , "ResultSetMetadata" :: NullOrUndefined (ResultSetMetadata)
  }


-- | <p>The metadata that describes the column structure and data types of a table of query results.</p>
newtype ResultSetMetadata = ResultSetMetadata 
  { "ColumnInfo" :: NullOrUndefined (ColumnInfoList)
  }


-- | <p>The rows that comprise a query result table.</p>
newtype Row = Row 
  { "Data" :: NullOrUndefined (DatumList')
  }


newtype RowList = RowList (Array Row)


newtype StartQueryExecutionInput = StartQueryExecutionInput 
  { "QueryString" :: (QueryString)
  , "ClientRequestToken" :: NullOrUndefined (IdempotencyToken)
  , "QueryExecutionContext" :: NullOrUndefined (QueryExecutionContext)
  , "ResultConfiguration" :: (ResultConfiguration)
  }


newtype StartQueryExecutionOutput = StartQueryExecutionOutput 
  { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId)
  }


newtype StopQueryExecutionInput = StopQueryExecutionInput 
  { "QueryExecutionId" :: (QueryExecutionId)
  }


newtype StopQueryExecutionOutput = StopQueryExecutionOutput 
  { 
  }


newtype ThrottleReason = ThrottleReason String


newtype Token = Token String


-- | <p>Indicates that the request was throttled.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  , "Reason" :: NullOrUndefined (ThrottleReason)
  }


-- | <p>Information about a named query ID that could not be processed.</p>
newtype UnprocessedNamedQueryId = UnprocessedNamedQueryId 
  { "NamedQueryId" :: NullOrUndefined (NamedQueryId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }


newtype UnprocessedNamedQueryIdList = UnprocessedNamedQueryIdList (Array UnprocessedNamedQueryId)


-- | <p>Describes a query execution that failed to process.</p>
newtype UnprocessedQueryExecutionId = UnprocessedQueryExecutionId 
  { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }


newtype UnprocessedQueryExecutionIdList = UnprocessedQueryExecutionIdList (Array UnprocessedQueryExecutionId)


newtype DatumList' = DatumList' (Array Datum)


newtype DatumString' = DatumString' String
