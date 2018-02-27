## Module AWS.Athena

<p>Amazon Athena is an interactive query service that lets you use standard SQL to analyze data directly in Amazon S3. You can point Athena at your data in Amazon S3 and run ad-hoc queries and get results in seconds. Athena is serverless, so there is no infrastructure to set up or manage. You pay only for the queries you run. Athena scales automatically—executing queries in parallel—so results are fast, even with large datasets and complex queries. For more information, see <a href="http://docs.aws.amazon.com/athena/latest/ug/what-is.html">What is Amazon Athena</a> in the <i>Amazon Athena User Guide</i>.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchGetNamedQuery`

``` purescript
batchGetNamedQuery :: forall eff. BatchGetNamedQueryInput -> Aff (err :: RequestError | eff) BatchGetNamedQueryOutput
```

<p>Returns the details of a single named query or a list of up to 50 queries, which you provide as an array of query ID strings. Use <a>ListNamedQueries</a> to get the list of named query IDs. If information could not be retrieved for a submitted query ID, information about the query ID submitted is listed under <a>UnprocessedNamedQueryId</a>. Named queries are different from executed queries. Use <a>BatchGetQueryExecution</a> to get details about each unique query execution, and <a>ListQueryExecutions</a> to get a list of query execution IDs.</p>

#### `batchGetQueryExecution`

``` purescript
batchGetQueryExecution :: forall eff. BatchGetQueryExecutionInput -> Aff (err :: RequestError | eff) BatchGetQueryExecutionOutput
```

<p>Returns the details of a single query execution or a list of up to 50 query executions, which you provide as an array of query execution ID strings. To get a list of query execution IDs, use <a>ListQueryExecutions</a>. Query executions are different from named (saved) queries. Use <a>BatchGetNamedQuery</a> to get details about named queries.</p>

#### `createNamedQuery`

``` purescript
createNamedQuery :: forall eff. CreateNamedQueryInput -> Aff (err :: RequestError | eff) CreateNamedQueryOutput
```

<p>Creates a named query.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>

#### `deleteNamedQuery`

``` purescript
deleteNamedQuery :: forall eff. DeleteNamedQueryInput -> Aff (err :: RequestError | eff) DeleteNamedQueryOutput
```

<p>Deletes a named query.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>

#### `getNamedQuery`

``` purescript
getNamedQuery :: forall eff. GetNamedQueryInput -> Aff (err :: RequestError | eff) GetNamedQueryOutput
```

<p>Returns information about a single query.</p>

#### `getQueryExecution`

``` purescript
getQueryExecution :: forall eff. GetQueryExecutionInput -> Aff (err :: RequestError | eff) GetQueryExecutionOutput
```

<p>Returns information about a single execution of a query. Each time a query executes, information about the query execution is saved with a unique ID.</p>

#### `getQueryResults`

``` purescript
getQueryResults :: forall eff. GetQueryResultsInput -> Aff (err :: RequestError | eff) GetQueryResultsOutput
```

<p>Returns the results of a single query execution specified by <code>QueryExecutionId</code>. This request does not execute the query but returns results. Use <a>StartQueryExecution</a> to run a query.</p>

#### `listNamedQueries`

``` purescript
listNamedQueries :: forall eff. ListNamedQueriesInput -> Aff (err :: RequestError | eff) ListNamedQueriesOutput
```

<p>Provides a list of all available query IDs.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>

#### `listQueryExecutions`

``` purescript
listQueryExecutions :: forall eff. ListQueryExecutionsInput -> Aff (err :: RequestError | eff) ListQueryExecutionsOutput
```

<p>Provides a list of all available query execution IDs.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>

#### `startQueryExecution`

``` purescript
startQueryExecution :: forall eff. StartQueryExecutionInput -> Aff (err :: RequestError | eff) StartQueryExecutionOutput
```

<p>Runs (executes) the SQL query statements contained in the <code>Query</code> string.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>

#### `stopQueryExecution`

``` purescript
stopQueryExecution :: forall eff. StopQueryExecutionInput -> Aff (err :: RequestError | eff) StopQueryExecutionOutput
```

<p>Stops a query execution.</p> <p>For code samples using the AWS SDK for Java, see <a href="http://docs.aws.amazon.com/athena/latest/ug/code-samples.html">Examples and Code Samples</a> in the <i>Amazon Athena User Guide</i>.</p>

#### `BatchGetNamedQueryInput`

``` purescript
newtype BatchGetNamedQueryInput
  = BatchGetNamedQueryInput { "NamedQueryIds" :: NamedQueryIdList }
```

##### Instances
``` purescript
Newtype BatchGetNamedQueryInput _
```

#### `BatchGetNamedQueryOutput`

``` purescript
newtype BatchGetNamedQueryOutput
  = BatchGetNamedQueryOutput { "NamedQueries" :: NullOrUndefined (NamedQueryList), "UnprocessedNamedQueryIds" :: NullOrUndefined (UnprocessedNamedQueryIdList) }
```

##### Instances
``` purescript
Newtype BatchGetNamedQueryOutput _
```

#### `BatchGetQueryExecutionInput`

``` purescript
newtype BatchGetQueryExecutionInput
  = BatchGetQueryExecutionInput { "QueryExecutionIds" :: QueryExecutionIdList }
```

##### Instances
``` purescript
Newtype BatchGetQueryExecutionInput _
```

#### `BatchGetQueryExecutionOutput`

``` purescript
newtype BatchGetQueryExecutionOutput
  = BatchGetQueryExecutionOutput { "QueryExecutions" :: NullOrUndefined (QueryExecutionList), "UnprocessedQueryExecutionIds" :: NullOrUndefined (UnprocessedQueryExecutionIdList) }
```

##### Instances
``` purescript
Newtype BatchGetQueryExecutionOutput _
```

#### `ColumnInfo`

``` purescript
newtype ColumnInfo
  = ColumnInfo { "CatalogName" :: NullOrUndefined (String), "SchemaName" :: NullOrUndefined (String), "TableName" :: NullOrUndefined (String), "Name" :: String, "Label" :: NullOrUndefined (String), "Type" :: String, "Precision" :: NullOrUndefined (Int), "Scale" :: NullOrUndefined (Int), "Nullable" :: NullOrUndefined (ColumnNullable), "CaseSensitive" :: NullOrUndefined (Boolean) }
```

<p>Information about the columns in a query execution result.</p>

##### Instances
``` purescript
Newtype ColumnInfo _
```

#### `ColumnInfoList`

``` purescript
newtype ColumnInfoList
  = ColumnInfoList (Array ColumnInfo)
```

##### Instances
``` purescript
Newtype ColumnInfoList _
```

#### `ColumnNullable`

``` purescript
newtype ColumnNullable
  = ColumnNullable String
```

##### Instances
``` purescript
Newtype ColumnNullable _
```

#### `CreateNamedQueryInput`

``` purescript
newtype CreateNamedQueryInput
  = CreateNamedQueryInput { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "Database" :: DatabaseString, "QueryString" :: QueryString, "ClientRequestToken" :: NullOrUndefined (IdempotencyToken) }
```

##### Instances
``` purescript
Newtype CreateNamedQueryInput _
```

#### `CreateNamedQueryOutput`

``` purescript
newtype CreateNamedQueryOutput
  = CreateNamedQueryOutput { "NamedQueryId" :: NullOrUndefined (NamedQueryId) }
```

##### Instances
``` purescript
Newtype CreateNamedQueryOutput _
```

#### `DatabaseString`

``` purescript
newtype DatabaseString
  = DatabaseString String
```

##### Instances
``` purescript
Newtype DatabaseString _
```

#### `Date`

``` purescript
newtype Date
  = Date Number
```

##### Instances
``` purescript
Newtype Date _
```

#### `Datum`

``` purescript
newtype Datum
  = Datum { "VarCharValue" :: NullOrUndefined (DatumString') }
```

<p>A piece of data (a field in the table).</p>

##### Instances
``` purescript
Newtype Datum _
```

#### `DeleteNamedQueryInput`

``` purescript
newtype DeleteNamedQueryInput
  = DeleteNamedQueryInput { "NamedQueryId" :: NamedQueryId }
```

##### Instances
``` purescript
Newtype DeleteNamedQueryInput _
```

#### `DeleteNamedQueryOutput`

``` purescript
newtype DeleteNamedQueryOutput
  = DeleteNamedQueryOutput {  }
```

##### Instances
``` purescript
Newtype DeleteNamedQueryOutput _
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

#### `EncryptionConfiguration`

``` purescript
newtype EncryptionConfiguration
  = EncryptionConfiguration { "EncryptionOption" :: EncryptionOption, "KmsKey" :: NullOrUndefined (String) }
```

<p>If query results are encrypted in Amazon S3, indicates the Amazon S3 encryption option used.</p>

##### Instances
``` purescript
Newtype EncryptionConfiguration _
```

#### `EncryptionOption`

``` purescript
newtype EncryptionOption
  = EncryptionOption String
```

##### Instances
``` purescript
Newtype EncryptionOption _
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

##### Instances
``` purescript
Newtype ErrorCode _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `GetNamedQueryInput`

``` purescript
newtype GetNamedQueryInput
  = GetNamedQueryInput { "NamedQueryId" :: NamedQueryId }
```

##### Instances
``` purescript
Newtype GetNamedQueryInput _
```

#### `GetNamedQueryOutput`

``` purescript
newtype GetNamedQueryOutput
  = GetNamedQueryOutput { "NamedQuery" :: NullOrUndefined (NamedQuery) }
```

##### Instances
``` purescript
Newtype GetNamedQueryOutput _
```

#### `GetQueryExecutionInput`

``` purescript
newtype GetQueryExecutionInput
  = GetQueryExecutionInput { "QueryExecutionId" :: QueryExecutionId }
```

##### Instances
``` purescript
Newtype GetQueryExecutionInput _
```

#### `GetQueryExecutionOutput`

``` purescript
newtype GetQueryExecutionOutput
  = GetQueryExecutionOutput { "QueryExecution" :: NullOrUndefined (QueryExecution) }
```

##### Instances
``` purescript
Newtype GetQueryExecutionOutput _
```

#### `GetQueryResultsInput`

``` purescript
newtype GetQueryResultsInput
  = GetQueryResultsInput { "QueryExecutionId" :: QueryExecutionId, "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxQueryResults) }
```

##### Instances
``` purescript
Newtype GetQueryResultsInput _
```

#### `GetQueryResultsOutput`

``` purescript
newtype GetQueryResultsOutput
  = GetQueryResultsOutput { "ResultSet" :: NullOrUndefined (ResultSet), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype GetQueryResultsOutput _
```

#### `IdempotencyToken`

``` purescript
newtype IdempotencyToken
  = IdempotencyToken String
```

##### Instances
``` purescript
Newtype IdempotencyToken _
```

#### `InternalServerException`

``` purescript
newtype InternalServerException
  = InternalServerException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Indicates a platform issue, which may be due to a transient condition or outage.</p>

##### Instances
``` purescript
Newtype InternalServerException _
```

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "AthenaErrorCode" :: NullOrUndefined (ErrorCode), "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Indicates that something is wrong with the input to the request. For example, a required parameter may be missing or out of range.</p>

##### Instances
``` purescript
Newtype InvalidRequestException _
```

#### `ListNamedQueriesInput`

``` purescript
newtype ListNamedQueriesInput
  = ListNamedQueriesInput { "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxNamedQueriesCount) }
```

##### Instances
``` purescript
Newtype ListNamedQueriesInput _
```

#### `ListNamedQueriesOutput`

``` purescript
newtype ListNamedQueriesOutput
  = ListNamedQueriesOutput { "NamedQueryIds" :: NullOrUndefined (NamedQueryIdList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype ListNamedQueriesOutput _
```

#### `ListQueryExecutionsInput`

``` purescript
newtype ListQueryExecutionsInput
  = ListQueryExecutionsInput { "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxQueryExecutionsCount) }
```

##### Instances
``` purescript
Newtype ListQueryExecutionsInput _
```

#### `ListQueryExecutionsOutput`

``` purescript
newtype ListQueryExecutionsOutput
  = ListQueryExecutionsOutput { "QueryExecutionIds" :: NullOrUndefined (QueryExecutionIdList), "NextToken" :: NullOrUndefined (Token) }
```

##### Instances
``` purescript
Newtype ListQueryExecutionsOutput _
```

#### `MaxNamedQueriesCount`

``` purescript
newtype MaxNamedQueriesCount
  = MaxNamedQueriesCount Int
```

##### Instances
``` purescript
Newtype MaxNamedQueriesCount _
```

#### `MaxQueryExecutionsCount`

``` purescript
newtype MaxQueryExecutionsCount
  = MaxQueryExecutionsCount Int
```

##### Instances
``` purescript
Newtype MaxQueryExecutionsCount _
```

#### `MaxQueryResults`

``` purescript
newtype MaxQueryResults
  = MaxQueryResults Int
```

##### Instances
``` purescript
Newtype MaxQueryResults _
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

#### `NamedQuery`

``` purescript
newtype NamedQuery
  = NamedQuery { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "Database" :: DatabaseString, "QueryString" :: QueryString, "NamedQueryId" :: NullOrUndefined (NamedQueryId) }
```

<p>A query, where <code>QueryString</code> is the SQL query statements that comprise the query.</p>

##### Instances
``` purescript
Newtype NamedQuery _
```

#### `NamedQueryId`

``` purescript
newtype NamedQueryId
  = NamedQueryId String
```

##### Instances
``` purescript
Newtype NamedQueryId _
```

#### `NamedQueryIdList`

``` purescript
newtype NamedQueryIdList
  = NamedQueryIdList (Array NamedQueryId)
```

##### Instances
``` purescript
Newtype NamedQueryIdList _
```

#### `NamedQueryList`

``` purescript
newtype NamedQueryList
  = NamedQueryList (Array NamedQuery)
```

##### Instances
``` purescript
Newtype NamedQueryList _
```

#### `QueryExecution`

``` purescript
newtype QueryExecution
  = QueryExecution { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId), "Query" :: NullOrUndefined (QueryString), "ResultConfiguration" :: NullOrUndefined (ResultConfiguration), "QueryExecutionContext" :: NullOrUndefined (QueryExecutionContext), "Status" :: NullOrUndefined (QueryExecutionStatus), "Statistics" :: NullOrUndefined (QueryExecutionStatistics) }
```

<p>Information about a single instance of a query execution.</p>

##### Instances
``` purescript
Newtype QueryExecution _
```

#### `QueryExecutionContext`

``` purescript
newtype QueryExecutionContext
  = QueryExecutionContext { "Database" :: NullOrUndefined (DatabaseString) }
```

<p>The database in which the query execution occurs.</p>

##### Instances
``` purescript
Newtype QueryExecutionContext _
```

#### `QueryExecutionId`

``` purescript
newtype QueryExecutionId
  = QueryExecutionId String
```

##### Instances
``` purescript
Newtype QueryExecutionId _
```

#### `QueryExecutionIdList`

``` purescript
newtype QueryExecutionIdList
  = QueryExecutionIdList (Array QueryExecutionId)
```

##### Instances
``` purescript
Newtype QueryExecutionIdList _
```

#### `QueryExecutionList`

``` purescript
newtype QueryExecutionList
  = QueryExecutionList (Array QueryExecution)
```

##### Instances
``` purescript
Newtype QueryExecutionList _
```

#### `QueryExecutionState`

``` purescript
newtype QueryExecutionState
  = QueryExecutionState String
```

##### Instances
``` purescript
Newtype QueryExecutionState _
```

#### `QueryExecutionStatistics`

``` purescript
newtype QueryExecutionStatistics
  = QueryExecutionStatistics { "EngineExecutionTimeInMillis" :: NullOrUndefined (Number), "DataScannedInBytes" :: NullOrUndefined (Number) }
```

<p>The amount of data scanned during the query execution and the amount of time that it took to execute.</p>

##### Instances
``` purescript
Newtype QueryExecutionStatistics _
```

#### `QueryExecutionStatus`

``` purescript
newtype QueryExecutionStatus
  = QueryExecutionStatus { "State" :: NullOrUndefined (QueryExecutionState), "StateChangeReason" :: NullOrUndefined (String), "SubmissionDateTime" :: NullOrUndefined (Date), "CompletionDateTime" :: NullOrUndefined (Date) }
```

<p>The completion date, current state, submission time, and state change reason (if applicable) for the query execution.</p>

##### Instances
``` purescript
Newtype QueryExecutionStatus _
```

#### `QueryString`

``` purescript
newtype QueryString
  = QueryString String
```

##### Instances
``` purescript
Newtype QueryString _
```

#### `ResultConfiguration`

``` purescript
newtype ResultConfiguration
  = ResultConfiguration { "OutputLocation" :: String, "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration) }
```

<p>The location in Amazon S3 where query results are stored and the encryption option, if any, used for query results.</p>

##### Instances
``` purescript
Newtype ResultConfiguration _
```

#### `ResultSet`

``` purescript
newtype ResultSet
  = ResultSet { "Rows" :: NullOrUndefined (RowList), "ResultSetMetadata" :: NullOrUndefined (ResultSetMetadata) }
```

<p>The metadata and rows that comprise a query result set. The metadata describes the column structure and data types.</p>

##### Instances
``` purescript
Newtype ResultSet _
```

#### `ResultSetMetadata`

``` purescript
newtype ResultSetMetadata
  = ResultSetMetadata { "ColumnInfo" :: NullOrUndefined (ColumnInfoList) }
```

<p>The metadata that describes the column structure and data types of a table of query results.</p>

##### Instances
``` purescript
Newtype ResultSetMetadata _
```

#### `Row`

``` purescript
newtype Row
  = Row { "Data" :: NullOrUndefined (DatumList') }
```

<p>The rows that comprise a query result table.</p>

##### Instances
``` purescript
Newtype Row _
```

#### `RowList`

``` purescript
newtype RowList
  = RowList (Array Row)
```

##### Instances
``` purescript
Newtype RowList _
```

#### `StartQueryExecutionInput`

``` purescript
newtype StartQueryExecutionInput
  = StartQueryExecutionInput { "QueryString" :: QueryString, "ClientRequestToken" :: NullOrUndefined (IdempotencyToken), "QueryExecutionContext" :: NullOrUndefined (QueryExecutionContext), "ResultConfiguration" :: ResultConfiguration }
```

##### Instances
``` purescript
Newtype StartQueryExecutionInput _
```

#### `StartQueryExecutionOutput`

``` purescript
newtype StartQueryExecutionOutput
  = StartQueryExecutionOutput { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId) }
```

##### Instances
``` purescript
Newtype StartQueryExecutionOutput _
```

#### `StopQueryExecutionInput`

``` purescript
newtype StopQueryExecutionInput
  = StopQueryExecutionInput { "QueryExecutionId" :: QueryExecutionId }
```

##### Instances
``` purescript
Newtype StopQueryExecutionInput _
```

#### `StopQueryExecutionOutput`

``` purescript
newtype StopQueryExecutionOutput
  = StopQueryExecutionOutput {  }
```

##### Instances
``` purescript
Newtype StopQueryExecutionOutput _
```

#### `ThrottleReason`

``` purescript
newtype ThrottleReason
  = ThrottleReason String
```

##### Instances
``` purescript
Newtype ThrottleReason _
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

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (ErrorMessage), "Reason" :: NullOrUndefined (ThrottleReason) }
```

<p>Indicates that the request was throttled.</p>

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `UnprocessedNamedQueryId`

``` purescript
newtype UnprocessedNamedQueryId
  = UnprocessedNamedQueryId { "NamedQueryId" :: NullOrUndefined (NamedQueryId), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Information about a named query ID that could not be processed.</p>

##### Instances
``` purescript
Newtype UnprocessedNamedQueryId _
```

#### `UnprocessedNamedQueryIdList`

``` purescript
newtype UnprocessedNamedQueryIdList
  = UnprocessedNamedQueryIdList (Array UnprocessedNamedQueryId)
```

##### Instances
``` purescript
Newtype UnprocessedNamedQueryIdList _
```

#### `UnprocessedQueryExecutionId`

``` purescript
newtype UnprocessedQueryExecutionId
  = UnprocessedQueryExecutionId { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Describes a query execution that failed to process.</p>

##### Instances
``` purescript
Newtype UnprocessedQueryExecutionId _
```

#### `UnprocessedQueryExecutionIdList`

``` purescript
newtype UnprocessedQueryExecutionIdList
  = UnprocessedQueryExecutionIdList (Array UnprocessedQueryExecutionId)
```

##### Instances
``` purescript
Newtype UnprocessedQueryExecutionIdList _
```

#### `DatumList'`

``` purescript
newtype DatumList'
  = DatumList' (Array Datum)
```

##### Instances
``` purescript
Newtype DatumList' _
```

#### `DatumString'`

``` purescript
newtype DatumString'
  = DatumString' String
```

##### Instances
``` purescript
Newtype DatumString' _
```


