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

#### `BatchGetNamedQueryOutput`

``` purescript
newtype BatchGetNamedQueryOutput
  = BatchGetNamedQueryOutput { "NamedQueries" :: NullOrUndefined (NamedQueryList), "UnprocessedNamedQueryIds" :: NullOrUndefined (UnprocessedNamedQueryIdList) }
```

#### `BatchGetQueryExecutionInput`

``` purescript
newtype BatchGetQueryExecutionInput
  = BatchGetQueryExecutionInput { "QueryExecutionIds" :: QueryExecutionIdList }
```

#### `BatchGetQueryExecutionOutput`

``` purescript
newtype BatchGetQueryExecutionOutput
  = BatchGetQueryExecutionOutput { "QueryExecutions" :: NullOrUndefined (QueryExecutionList), "UnprocessedQueryExecutionIds" :: NullOrUndefined (UnprocessedQueryExecutionIdList) }
```

#### `ColumnInfo`

``` purescript
newtype ColumnInfo
  = ColumnInfo { "CatalogName" :: NullOrUndefined (String), "SchemaName" :: NullOrUndefined (String), "TableName" :: NullOrUndefined (String), "Name" :: String, "Label" :: NullOrUndefined (String), "Type" :: String, "Precision" :: NullOrUndefined (Int), "Scale" :: NullOrUndefined (Int), "Nullable" :: NullOrUndefined (ColumnNullable), "CaseSensitive" :: NullOrUndefined (Boolean) }
```

<p>Information about the columns in a query execution result.</p>

#### `ColumnInfoList`

``` purescript
newtype ColumnInfoList
  = ColumnInfoList (Array ColumnInfo)
```

#### `ColumnNullable`

``` purescript
newtype ColumnNullable
  = ColumnNullable String
```

#### `CreateNamedQueryInput`

``` purescript
newtype CreateNamedQueryInput
  = CreateNamedQueryInput { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "Database" :: DatabaseString, "QueryString" :: QueryString, "ClientRequestToken" :: NullOrUndefined (IdempotencyToken) }
```

#### `CreateNamedQueryOutput`

``` purescript
newtype CreateNamedQueryOutput
  = CreateNamedQueryOutput { "NamedQueryId" :: NullOrUndefined (NamedQueryId) }
```

#### `DatabaseString`

``` purescript
newtype DatabaseString
  = DatabaseString String
```

#### `Date`

``` purescript
newtype Date
  = Date Number
```

#### `Datum`

``` purescript
newtype Datum
  = Datum { "VarCharValue" :: NullOrUndefined (DatumString') }
```

<p>A piece of data (a field in the table).</p>

#### `DeleteNamedQueryInput`

``` purescript
newtype DeleteNamedQueryInput
  = DeleteNamedQueryInput { "NamedQueryId" :: NamedQueryId }
```

#### `DeleteNamedQueryOutput`

``` purescript
newtype DeleteNamedQueryOutput
  = DeleteNamedQueryOutput {  }
```

#### `DescriptionString`

``` purescript
newtype DescriptionString
  = DescriptionString String
```

#### `EncryptionConfiguration`

``` purescript
newtype EncryptionConfiguration
  = EncryptionConfiguration { "EncryptionOption" :: EncryptionOption, "KmsKey" :: NullOrUndefined (String) }
```

<p>If query results are encrypted in Amazon S3, indicates the Amazon S3 encryption option used.</p>

#### `EncryptionOption`

``` purescript
newtype EncryptionOption
  = EncryptionOption String
```

#### `ErrorCode`

``` purescript
newtype ErrorCode
  = ErrorCode String
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `GetNamedQueryInput`

``` purescript
newtype GetNamedQueryInput
  = GetNamedQueryInput { "NamedQueryId" :: NamedQueryId }
```

#### `GetNamedQueryOutput`

``` purescript
newtype GetNamedQueryOutput
  = GetNamedQueryOutput { "NamedQuery" :: NullOrUndefined (NamedQuery) }
```

#### `GetQueryExecutionInput`

``` purescript
newtype GetQueryExecutionInput
  = GetQueryExecutionInput { "QueryExecutionId" :: QueryExecutionId }
```

#### `GetQueryExecutionOutput`

``` purescript
newtype GetQueryExecutionOutput
  = GetQueryExecutionOutput { "QueryExecution" :: NullOrUndefined (QueryExecution) }
```

#### `GetQueryResultsInput`

``` purescript
newtype GetQueryResultsInput
  = GetQueryResultsInput { "QueryExecutionId" :: QueryExecutionId, "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxQueryResults) }
```

#### `GetQueryResultsOutput`

``` purescript
newtype GetQueryResultsOutput
  = GetQueryResultsOutput { "ResultSet" :: NullOrUndefined (ResultSet), "NextToken" :: NullOrUndefined (Token) }
```

#### `IdempotencyToken`

``` purescript
newtype IdempotencyToken
  = IdempotencyToken String
```

#### `InternalServerException`

``` purescript
newtype InternalServerException
  = InternalServerException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Indicates a platform issue, which may be due to a transient condition or outage.</p>

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "AthenaErrorCode" :: NullOrUndefined (ErrorCode), "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Indicates that something is wrong with the input to the request. For example, a required parameter may be missing or out of range.</p>

#### `ListNamedQueriesInput`

``` purescript
newtype ListNamedQueriesInput
  = ListNamedQueriesInput { "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxNamedQueriesCount) }
```

#### `ListNamedQueriesOutput`

``` purescript
newtype ListNamedQueriesOutput
  = ListNamedQueriesOutput { "NamedQueryIds" :: NullOrUndefined (NamedQueryIdList), "NextToken" :: NullOrUndefined (Token) }
```

#### `ListQueryExecutionsInput`

``` purescript
newtype ListQueryExecutionsInput
  = ListQueryExecutionsInput { "NextToken" :: NullOrUndefined (Token), "MaxResults" :: NullOrUndefined (MaxQueryExecutionsCount) }
```

#### `ListQueryExecutionsOutput`

``` purescript
newtype ListQueryExecutionsOutput
  = ListQueryExecutionsOutput { "QueryExecutionIds" :: NullOrUndefined (QueryExecutionIdList), "NextToken" :: NullOrUndefined (Token) }
```

#### `MaxNamedQueriesCount`

``` purescript
newtype MaxNamedQueriesCount
  = MaxNamedQueriesCount Int
```

#### `MaxQueryExecutionsCount`

``` purescript
newtype MaxQueryExecutionsCount
  = MaxQueryExecutionsCount Int
```

#### `MaxQueryResults`

``` purescript
newtype MaxQueryResults
  = MaxQueryResults Int
```

#### `NameString`

``` purescript
newtype NameString
  = NameString String
```

#### `NamedQuery`

``` purescript
newtype NamedQuery
  = NamedQuery { "Name" :: NameString, "Description" :: NullOrUndefined (DescriptionString), "Database" :: DatabaseString, "QueryString" :: QueryString, "NamedQueryId" :: NullOrUndefined (NamedQueryId) }
```

<p>A query, where <code>QueryString</code> is the SQL query statements that comprise the query.</p>

#### `NamedQueryId`

``` purescript
newtype NamedQueryId
  = NamedQueryId String
```

#### `NamedQueryIdList`

``` purescript
newtype NamedQueryIdList
  = NamedQueryIdList (Array NamedQueryId)
```

#### `NamedQueryList`

``` purescript
newtype NamedQueryList
  = NamedQueryList (Array NamedQuery)
```

#### `QueryExecution`

``` purescript
newtype QueryExecution
  = QueryExecution { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId), "Query" :: NullOrUndefined (QueryString), "ResultConfiguration" :: NullOrUndefined (ResultConfiguration), "QueryExecutionContext" :: NullOrUndefined (QueryExecutionContext), "Status" :: NullOrUndefined (QueryExecutionStatus), "Statistics" :: NullOrUndefined (QueryExecutionStatistics) }
```

<p>Information about a single instance of a query execution.</p>

#### `QueryExecutionContext`

``` purescript
newtype QueryExecutionContext
  = QueryExecutionContext { "Database" :: NullOrUndefined (DatabaseString) }
```

<p>The database in which the query execution occurs.</p>

#### `QueryExecutionId`

``` purescript
newtype QueryExecutionId
  = QueryExecutionId String
```

#### `QueryExecutionIdList`

``` purescript
newtype QueryExecutionIdList
  = QueryExecutionIdList (Array QueryExecutionId)
```

#### `QueryExecutionList`

``` purescript
newtype QueryExecutionList
  = QueryExecutionList (Array QueryExecution)
```

#### `QueryExecutionState`

``` purescript
newtype QueryExecutionState
  = QueryExecutionState String
```

#### `QueryExecutionStatistics`

``` purescript
newtype QueryExecutionStatistics
  = QueryExecutionStatistics { "EngineExecutionTimeInMillis" :: NullOrUndefined (Number), "DataScannedInBytes" :: NullOrUndefined (Number) }
```

<p>The amount of data scanned during the query execution and the amount of time that it took to execute.</p>

#### `QueryExecutionStatus`

``` purescript
newtype QueryExecutionStatus
  = QueryExecutionStatus { "State" :: NullOrUndefined (QueryExecutionState), "StateChangeReason" :: NullOrUndefined (String), "SubmissionDateTime" :: NullOrUndefined (Date), "CompletionDateTime" :: NullOrUndefined (Date) }
```

<p>The completion date, current state, submission time, and state change reason (if applicable) for the query execution.</p>

#### `QueryString`

``` purescript
newtype QueryString
  = QueryString String
```

#### `ResultConfiguration`

``` purescript
newtype ResultConfiguration
  = ResultConfiguration { "OutputLocation" :: String, "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration) }
```

<p>The location in Amazon S3 where query results are stored and the encryption option, if any, used for query results.</p>

#### `ResultSet`

``` purescript
newtype ResultSet
  = ResultSet { "Rows" :: NullOrUndefined (RowList), "ResultSetMetadata" :: NullOrUndefined (ResultSetMetadata) }
```

<p>The metadata and rows that comprise a query result set. The metadata describes the column structure and data types.</p>

#### `ResultSetMetadata`

``` purescript
newtype ResultSetMetadata
  = ResultSetMetadata { "ColumnInfo" :: NullOrUndefined (ColumnInfoList) }
```

<p>The metadata that describes the column structure and data types of a table of query results.</p>

#### `Row`

``` purescript
newtype Row
  = Row { "Data" :: NullOrUndefined (DatumList') }
```

<p>The rows that comprise a query result table.</p>

#### `RowList`

``` purescript
newtype RowList
  = RowList (Array Row)
```

#### `StartQueryExecutionInput`

``` purescript
newtype StartQueryExecutionInput
  = StartQueryExecutionInput { "QueryString" :: QueryString, "ClientRequestToken" :: NullOrUndefined (IdempotencyToken), "QueryExecutionContext" :: NullOrUndefined (QueryExecutionContext), "ResultConfiguration" :: ResultConfiguration }
```

#### `StartQueryExecutionOutput`

``` purescript
newtype StartQueryExecutionOutput
  = StartQueryExecutionOutput { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId) }
```

#### `StopQueryExecutionInput`

``` purescript
newtype StopQueryExecutionInput
  = StopQueryExecutionInput { "QueryExecutionId" :: QueryExecutionId }
```

#### `StopQueryExecutionOutput`

``` purescript
newtype StopQueryExecutionOutput
  = StopQueryExecutionOutput {  }
```

#### `ThrottleReason`

``` purescript
newtype ThrottleReason
  = ThrottleReason String
```

#### `Token`

``` purescript
newtype Token
  = Token String
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (ErrorMessage), "Reason" :: NullOrUndefined (ThrottleReason) }
```

<p>Indicates that the request was throttled.</p>

#### `UnprocessedNamedQueryId`

``` purescript
newtype UnprocessedNamedQueryId
  = UnprocessedNamedQueryId { "NamedQueryId" :: NullOrUndefined (NamedQueryId), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Information about a named query ID that could not be processed.</p>

#### `UnprocessedNamedQueryIdList`

``` purescript
newtype UnprocessedNamedQueryIdList
  = UnprocessedNamedQueryIdList (Array UnprocessedNamedQueryId)
```

#### `UnprocessedQueryExecutionId`

``` purescript
newtype UnprocessedQueryExecutionId
  = UnprocessedQueryExecutionId { "QueryExecutionId" :: NullOrUndefined (QueryExecutionId), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Describes a query execution that failed to process.</p>

#### `UnprocessedQueryExecutionIdList`

``` purescript
newtype UnprocessedQueryExecutionIdList
  = UnprocessedQueryExecutionIdList (Array UnprocessedQueryExecutionId)
```

#### `DatumList'`

``` purescript
newtype DatumList'
  = DatumList' (Array Datum)
```

#### `DatumString'`

``` purescript
newtype DatumString'
  = DatumString' String
```


