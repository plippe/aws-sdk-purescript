## Module AWS.DynamoDB

<p>Amazon DynamoDB is a fast, highly scalable, highly available, cost-effective non-relational database service.</p> <p>Amazon DynamoDB removes traditional scalability limitations on data storage while maintaining low latency and predictable performance.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchGetItem`

``` purescript
batchGetItem :: forall eff. BatchGetItemInput -> Aff (err :: RequestError | eff) BatchGetItemOutput
```

<p>Retrieves the attributes for multiple items from multiple tables using their primary keys.</p> <p>The maximum number of item attributes that can be retrieved for a single operation is 100. Also, the number of items retrieved is constrained by a 1 MB the size limit. If the response size limit is exceeded or a partial result is returned due to an internal processing failure, Amazon DynamoDB returns an <code>UnprocessedKeys</code> value so you can retry the operation starting with the next item to get.</p> <p>Amazon DynamoDB automatically adjusts the number of items returned per page to enforce this limit. For example, even if you ask to retrieve 100 items, but each individual item is 50k in size, the system returns 20 items and an appropriate <code>UnprocessedKeys</code> value so you can get the next page of results. If necessary, your application needs its own logic to assemble the pages of results into one set.</p>

#### `batchWriteItem`

``` purescript
batchWriteItem :: forall eff. BatchWriteItemInput -> Aff (err :: RequestError | eff) BatchWriteItemOutput
```

<p>Allows to execute a batch of Put and/or Delete Requests for many tables in a single call. A total of 25 requests are allowed.</p> <p>There are no transaction guarantees provided by this API. It does not allow conditional puts nor does it support return values.</p>

#### `createTable`

``` purescript
createTable :: forall eff. CreateTableInput -> Aff (err :: RequestError | eff) CreateTableOutput
```

<p>Adds a new table to your account.</p> <p>The table name must be unique among those associated with the AWS Account issuing the request, and the AWS Region that receives the request (e.g. <code>us-east-1</code>).</p> <p>The <code>CreateTable</code> operation triggers an asynchronous workflow to begin creating the table. Amazon DynamoDB immediately returns the state of the table (<code>CREATING</code>) until the table is in the <code>ACTIVE</code> state. Once the table is in the <code>ACTIVE</code> state, you can perform data plane operations.</p>

#### `deleteItem`

``` purescript
deleteItem :: forall eff. DeleteItemInput -> Aff (err :: RequestError | eff) DeleteItemOutput
```

<p>Deletes a single item in a table by primary key.</p> <p>You can perform a conditional delete operation that deletes the item if it exists, or if it has an expected attribute value.</p>

#### `deleteTable`

``` purescript
deleteTable :: forall eff. DeleteTableInput -> Aff (err :: RequestError | eff) DeleteTableOutput
```

<p>Deletes a table and all of its items.</p> <p>If the table is in the <code>ACTIVE</code> state, you can delete it. If a table is in <code>CREATING</code> or <code>UPDATING</code> states then Amazon DynamoDB returns a <code>ResourceInUseException</code>. If the specified table does not exist, Amazon DynamoDB returns a <code>ResourceNotFoundException</code>.</p>

#### `describeTable`

``` purescript
describeTable :: forall eff. DescribeTableInput -> Aff (err :: RequestError | eff) DescribeTableOutput
```

<p>Retrieves information about the table, including the current status of the table, the primary key schema and when the table was created.</p> <p>If the table does not exist, Amazon DynamoDB returns a <code>ResourceNotFoundException</code>.</p>

#### `getItem`

``` purescript
getItem :: forall eff. GetItemInput -> Aff (err :: RequestError | eff) GetItemOutput
```

<p>Retrieves a set of Attributes for an item that matches the primary key.</p> <p>The <code>GetItem</code> operation provides an eventually-consistent read by default. If eventually-consistent reads are not acceptable for your application, use <code>ConsistentRead</code>. Although this operation might take longer than a standard read, it always returns the last updated value.</p>

#### `listTables`

``` purescript
listTables :: forall eff. ListTablesInput -> Aff (err :: RequestError | eff) ListTablesOutput
```

<p>Retrieves a paginated list of table names created by the AWS Account of the caller in the AWS Region (e.g. <code>us-east-1</code>).</p>

#### `putItem`

``` purescript
putItem :: forall eff. PutItemInput -> Aff (err :: RequestError | eff) PutItemOutput
```

<p>Creates a new item, or replaces an old item with a new item (including all the attributes).</p> <p>If an item already exists in the specified table with the same primary key, the new item completely replaces the existing item. You can perform a conditional put (insert a new item if one with the specified primary key doesn't exist), or replace an existing item if it has certain attribute values.</p>

#### `query`

``` purescript
query :: forall eff. QueryInput -> Aff (err :: RequestError | eff) QueryOutput
```

<p>Gets the values of one or more items and its attributes by primary key (composite primary key, only).</p> <p>Narrow the scope of the query using comparison operators on the <code>RangeKeyValue</code> of the composite key. Use the <code>ScanIndexForward</code> parameter to get results in forward or reverse order by range key.</p>

#### `scan`

``` purescript
scan :: forall eff. ScanInput -> Aff (err :: RequestError | eff) ScanOutput
```

<p>Retrieves one or more items and its attributes by performing a full scan of a table.</p> <p>Provide a <code>ScanFilter</code> to get more specific results.</p>

#### `updateItem`

``` purescript
updateItem :: forall eff. UpdateItemInput -> Aff (err :: RequestError | eff) UpdateItemOutput
```

<p>Edits an existing item's attributes.</p> <p>You can perform a conditional update (insert a new attribute name-value pair if it doesn't exist, or replace an existing name-value pair if it has certain expected attribute values).</p>

#### `updateTable`

``` purescript
updateTable :: forall eff. UpdateTableInput -> Aff (err :: RequestError | eff) UpdateTableOutput
```

<p>Updates the provisioned throughput for the given table.</p> <p>Setting the throughput for a table helps you manage performance and is part of the Provisioned Throughput feature of Amazon DynamoDB.</p>

#### `AttributeAction`

``` purescript
newtype AttributeAction
  = AttributeAction String
```

<p>The type of action for an item update operation. Only use the add action for numbers or sets; the specified value is added to the existing value. If a set of values is specified, the values are added to the existing set. Adds the specified attribute. If the attribute exists, it is replaced by the new value. If no value is specified, this removes the attribute and its value. If a set of values is specified, then the values in the specified set are removed from the old set.</p>

##### Instances
``` purescript
Newtype AttributeAction _
```

#### `AttributeMap`

``` purescript
newtype AttributeMap
  = AttributeMap (Map AttributeName AttributeValue)
```

##### Instances
``` purescript
Newtype AttributeMap _
```

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

##### Instances
``` purescript
Newtype AttributeName _
```

#### `AttributeNameList`

``` purescript
newtype AttributeNameList
  = AttributeNameList (Array AttributeName)
```

<p>List of <code>Attribute</code> names. If attribute names are not specified then all attributes will be returned. If some attributes are not found, they will not appear in the result.</p>

##### Instances
``` purescript
Newtype AttributeNameList _
```

#### `AttributeUpdates`

``` purescript
newtype AttributeUpdates
  = AttributeUpdates (Map AttributeName AttributeValueUpdate)
```

<p>Map of attribute name to the new value and action for the update. The attribute names specify the attributes to modify, and cannot contain any primary key attributes.</p>

##### Instances
``` purescript
Newtype AttributeUpdates _
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue { "S" :: NullOrUndefined (StringAttributeValue), "N" :: NullOrUndefined (NumberAttributeValue), "B" :: NullOrUndefined (BinaryAttributeValue), "SS" :: NullOrUndefined (StringSetAttributeValue), "NS" :: NullOrUndefined (NumberSetAttributeValue), "BS" :: NullOrUndefined (BinarySetAttributeValue) }
```

<p>AttributeValue can be <code>String</code>, <code>Number</code>, <code>Binary</code>, <code>StringSet</code>, <code>NumberSet</code>, <code>BinarySet</code>.</p>

##### Instances
``` purescript
Newtype AttributeValue _
```

#### `AttributeValueList`

``` purescript
newtype AttributeValueList
  = AttributeValueList (Array AttributeValue)
```

<p>A list of attribute values to be used with a comparison operator for a scan or query operation. For comparisons that require more than one value, such as a <code>BETWEEN</code> comparison, the AttributeValueList contains two attribute values and the comparison operator.</p>

##### Instances
``` purescript
Newtype AttributeValueList _
```

#### `AttributeValueUpdate`

``` purescript
newtype AttributeValueUpdate
  = AttributeValueUpdate { "Value" :: NullOrUndefined (AttributeValue), "Action" :: NullOrUndefined (AttributeAction) }
```

<p>Specifies the attribute to update and how to perform the update. Possible values: <code>PUT</code> (default), <code>ADD</code> or <code>DELETE</code>.</p>

##### Instances
``` purescript
Newtype AttributeValueUpdate _
```

#### `BatchGetItemInput`

``` purescript
newtype BatchGetItemInput
  = BatchGetItemInput { "RequestItems" :: BatchGetRequestMap }
```

##### Instances
``` purescript
Newtype BatchGetItemInput _
```

#### `BatchGetItemOutput`

``` purescript
newtype BatchGetItemOutput
  = BatchGetItemOutput { "Responses" :: NullOrUndefined (BatchGetResponseMap), "UnprocessedKeys" :: NullOrUndefined (BatchGetRequestMap) }
```

##### Instances
``` purescript
Newtype BatchGetItemOutput _
```

#### `BatchGetRequestMap`

``` purescript
newtype BatchGetRequestMap
  = BatchGetRequestMap (Map TableName KeysAndAttributes)
```

<p>A map of the table name and corresponding items to get by primary key. While requesting items, each table name can be invoked only once per operation.</p>

##### Instances
``` purescript
Newtype BatchGetRequestMap _
```

#### `BatchGetResponseMap`

``` purescript
newtype BatchGetResponseMap
  = BatchGetResponseMap (Map TableName BatchResponse)
```

<p>Table names and the respective item attributes from the tables.</p>

##### Instances
``` purescript
Newtype BatchGetResponseMap _
```

#### `BatchResponse`

``` purescript
newtype BatchResponse
  = BatchResponse { "Items" :: NullOrUndefined (ItemList), "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits) }
```

<p>The item attributes from a response in a specific table, along with the read resources consumed on the table during the request.</p>

##### Instances
``` purescript
Newtype BatchResponse _
```

#### `BatchWriteItemInput`

``` purescript
newtype BatchWriteItemInput
  = BatchWriteItemInput { "RequestItems" :: BatchWriteItemRequestMap }
```

##### Instances
``` purescript
Newtype BatchWriteItemInput _
```

#### `BatchWriteItemOutput`

``` purescript
newtype BatchWriteItemOutput
  = BatchWriteItemOutput { "Responses" :: NullOrUndefined (BatchWriteResponseMap), "UnprocessedItems" :: NullOrUndefined (BatchWriteItemRequestMap) }
```

<p>A container for <code>BatchWriteItem</code> response</p>

##### Instances
``` purescript
Newtype BatchWriteItemOutput _
```

#### `BatchWriteItemRequestMap`

``` purescript
newtype BatchWriteItemRequestMap
  = BatchWriteItemRequestMap (Map TableName WriteRequests)
```

<p>A map of table name to list-of-write-requests.</p> <p>Key: The table name corresponding to the list of requests</p> <p>Value: Essentially a list of request items. Each request item could contain either a <code>PutRequest</code> or <code>DeleteRequest</code>. Never both.</p>

##### Instances
``` purescript
Newtype BatchWriteItemRequestMap _
```

#### `BatchWriteResponse`

``` purescript
newtype BatchWriteResponse
  = BatchWriteResponse { "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits) }
```

##### Instances
``` purescript
Newtype BatchWriteResponse _
```

#### `BatchWriteResponseMap`

``` purescript
newtype BatchWriteResponseMap
  = BatchWriteResponseMap (Map TableName BatchWriteResponse)
```

##### Instances
``` purescript
Newtype BatchWriteResponseMap _
```

#### `BinaryAttributeValue`

``` purescript
newtype BinaryAttributeValue
  = BinaryAttributeValue String
```

##### Instances
``` purescript
Newtype BinaryAttributeValue _
```

#### `BinarySetAttributeValue`

``` purescript
newtype BinarySetAttributeValue
  = BinarySetAttributeValue (Array BinaryAttributeValue)
```

##### Instances
``` purescript
Newtype BinarySetAttributeValue _
```

#### `BooleanObject`

``` purescript
newtype BooleanObject
  = BooleanObject Boolean
```

##### Instances
``` purescript
Newtype BooleanObject _
```

#### `ComparisonOperator`

``` purescript
newtype ComparisonOperator
  = ComparisonOperator String
```

<p>A comparison operator is an enumeration of several operations:</p> <ul> <li><code>EQ</code> for <em>equal</em>.</li> <li><code>NE</code> for <em>not equal</em>.</li> <li><code>IN</code> checks for exact matches.</li> <li><code>LE</code> for <em>less than or equal to</em>.</li> <li><code>LT</code> for <em>less than</em>.</li> <li><code>GE</code> for <em>greater than or equal to</em>.</li> <li><code>GT</code> for <em>greater than</em>.</li> <li><code>BETWEEN</code> for <em>between</em>.</li> <li><code>NOT_NULL</code> for <em>exists</em>.</li> <li><code>NULL</code> for <em>not exists</em>.</li> <li><code>CONTAINS</code> for substring or value in a set.</li> <li><code>NOT_CONTAINS</code> for absence of a substring or absence of a value in a set.</li> <li><code>BEGINS_WITH</code> for a substring prefix.</li> </ul> <p>Scan operations support all available comparison operators.</p> <p>Query operations support a subset of the available comparison operators: EQ, LE, LT, GE, GT, BETWEEN, and BEGINS_WITH.</p>

##### Instances
``` purescript
Newtype ComparisonOperator _
```

#### `Condition`

``` purescript
newtype Condition
  = Condition { "AttributeValueList" :: NullOrUndefined (AttributeValueList), "ComparisonOperator" :: ComparisonOperator }
```

##### Instances
``` purescript
Newtype Condition _
```

#### `ConditionalCheckFailedException`

``` purescript
newtype ConditionalCheckFailedException
  = ConditionalCheckFailedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This exception is thrown when an expected value does not match what was found in the system.</p>

##### Instances
``` purescript
Newtype ConditionalCheckFailedException _
```

#### `ConsistentRead`

``` purescript
newtype ConsistentRead
  = ConsistentRead Boolean
```

<p>If set to <code>true</code>, then a consistent read is issued. Otherwise eventually-consistent is used.</p>

##### Instances
``` purescript
Newtype ConsistentRead _
```

#### `ConsumedCapacityUnits`

``` purescript
newtype ConsumedCapacityUnits
  = ConsumedCapacityUnits Number
```

<p>The number of Capacity Units of the provisioned throughput of the table consumed during the operation. <code>GetItem</code>, <code>BatchGetItem</code>, <code>BatchWriteItem</code>, <code>Query</code>, and <code>Scan</code> operations consume <code>ReadCapacityUnits</code>, while <code>PutItem</code>, <code>UpdateItem</code>, and <code>DeleteItem</code> operations consume <code>WriteCapacityUnits</code>.</p>

##### Instances
``` purescript
Newtype ConsumedCapacityUnits _
```

#### `CreateTableInput`

``` purescript
newtype CreateTableInput
  = CreateTableInput { "TableName" :: TableName, "KeySchema" :: KeySchema, "ProvisionedThroughput" :: ProvisionedThroughput }
```

##### Instances
``` purescript
Newtype CreateTableInput _
```

#### `CreateTableOutput`

``` purescript
newtype CreateTableOutput
  = CreateTableOutput { "TableDescription" :: NullOrUndefined (TableDescription) }
```

##### Instances
``` purescript
Newtype CreateTableOutput _
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

#### `DeleteItemInput`

``` purescript
newtype DeleteItemInput
  = DeleteItemInput { "TableName" :: TableName, "Key" :: Key, "Expected" :: NullOrUndefined (ExpectedAttributeMap), "ReturnValues" :: NullOrUndefined (ReturnValue) }
```

##### Instances
``` purescript
Newtype DeleteItemInput _
```

#### `DeleteItemOutput`

``` purescript
newtype DeleteItemOutput
  = DeleteItemOutput { "Attributes" :: NullOrUndefined (AttributeMap), "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits) }
```

##### Instances
``` purescript
Newtype DeleteItemOutput _
```

#### `DeleteRequest`

``` purescript
newtype DeleteRequest
  = DeleteRequest { "Key" :: Key }
```

<p>A container for a Delete BatchWrite request</p>

##### Instances
``` purescript
Newtype DeleteRequest _
```

#### `DeleteTableInput`

``` purescript
newtype DeleteTableInput
  = DeleteTableInput { "TableName" :: TableName }
```

##### Instances
``` purescript
Newtype DeleteTableInput _
```

#### `DeleteTableOutput`

``` purescript
newtype DeleteTableOutput
  = DeleteTableOutput { "TableDescription" :: NullOrUndefined (TableDescription) }
```

##### Instances
``` purescript
Newtype DeleteTableOutput _
```

#### `DescribeTableInput`

``` purescript
newtype DescribeTableInput
  = DescribeTableInput { "TableName" :: TableName }
```

##### Instances
``` purescript
Newtype DescribeTableInput _
```

#### `DescribeTableOutput`

``` purescript
newtype DescribeTableOutput
  = DescribeTableOutput { "Table" :: NullOrUndefined (TableDescription) }
```

##### Instances
``` purescript
Newtype DescribeTableOutput _
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

#### `ExpectedAttributeMap`

``` purescript
newtype ExpectedAttributeMap
  = ExpectedAttributeMap (Map AttributeName ExpectedAttributeValue)
```

<p>Designates an attribute for a conditional modification. The <code>Expected</code> parameter allows you to provide an attribute name, and whether or not Amazon DynamoDB should check to see if the attribute has a particular value before modifying it.</p>

##### Instances
``` purescript
Newtype ExpectedAttributeMap _
```

#### `ExpectedAttributeValue`

``` purescript
newtype ExpectedAttributeValue
  = ExpectedAttributeValue { "Value" :: NullOrUndefined (AttributeValue), "Exists" :: NullOrUndefined (BooleanObject) }
```

<p>Allows you to provide an attribute name, and whether or not Amazon DynamoDB should check to see if the attribute value already exists; or if the attribute value exists and has a particular value before changing it.</p>

##### Instances
``` purescript
Newtype ExpectedAttributeValue _
```

#### `FilterConditionMap`

``` purescript
newtype FilterConditionMap
  = FilterConditionMap (Map String Condition)
```

##### Instances
``` purescript
Newtype FilterConditionMap _
```

#### `GetItemInput`

``` purescript
newtype GetItemInput
  = GetItemInput { "TableName" :: TableName, "Key" :: Key, "AttributesToGet" :: NullOrUndefined (AttributeNameList), "ConsistentRead" :: NullOrUndefined (ConsistentRead) }
```

##### Instances
``` purescript
Newtype GetItemInput _
```

#### `GetItemOutput`

``` purescript
newtype GetItemOutput
  = GetItemOutput { "Item" :: NullOrUndefined (AttributeMap), "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits) }
```

##### Instances
``` purescript
Newtype GetItemOutput _
```

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This exception is thrown when the service has a problem when trying to process the request.</p>

##### Instances
``` purescript
Newtype InternalServerError _
```

#### `ItemList`

``` purescript
newtype ItemList
  = ItemList (Array AttributeMap)
```

##### Instances
``` purescript
Newtype ItemList _
```

#### `Key`

``` purescript
newtype Key
  = Key { "HashKeyElement" :: AttributeValue, "RangeKeyElement" :: NullOrUndefined (AttributeValue) }
```

<p>The primary key that uniquely identifies each item in a table. A primary key can be a one attribute (hash) primary key or a two attribute (hash-and-range) primary key.</p>

##### Instances
``` purescript
Newtype Key _
```

#### `KeyList`

``` purescript
newtype KeyList
  = KeyList (Array Key)
```

##### Instances
``` purescript
Newtype KeyList _
```

#### `KeySchema`

``` purescript
newtype KeySchema
  = KeySchema { "HashKeyElement" :: KeySchemaElement, "RangeKeyElement" :: NullOrUndefined (KeySchemaElement) }
```

<p>The KeySchema identifies the primary key as a one attribute primary key (hash) or a composite two attribute (hash-and-range) primary key. Single attribute primary keys have one index value: a <code>HashKeyElement</code>. A composite hash-and-range primary key contains two attribute values: a <code>HashKeyElement</code> and a <code>RangeKeyElement</code>.</p>

##### Instances
``` purescript
Newtype KeySchema _
```

#### `KeySchemaAttributeName`

``` purescript
newtype KeySchemaAttributeName
  = KeySchemaAttributeName String
```

##### Instances
``` purescript
Newtype KeySchemaAttributeName _
```

#### `KeySchemaElement`

``` purescript
newtype KeySchemaElement
  = KeySchemaElement { "AttributeName" :: KeySchemaAttributeName, "AttributeType" :: ScalarAttributeType }
```

<p><code>KeySchemaElement</code> is the primary key (hash or hash-and-range) structure for the table.</p>

##### Instances
``` purescript
Newtype KeySchemaElement _
```

#### `KeysAndAttributes`

``` purescript
newtype KeysAndAttributes
  = KeysAndAttributes { "Keys" :: KeyList, "AttributesToGet" :: NullOrUndefined (AttributeNameList), "ConsistentRead" :: NullOrUndefined (ConsistentRead) }
```

##### Instances
``` purescript
Newtype KeysAndAttributes _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This exception is thrown when the subscriber exceeded the limits on the number of objects or operations.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListTablesInput`

``` purescript
newtype ListTablesInput
  = ListTablesInput { "ExclusiveStartTableName" :: NullOrUndefined (TableName), "Limit" :: NullOrUndefined (ListTablesInputLimit) }
```

##### Instances
``` purescript
Newtype ListTablesInput _
```

#### `ListTablesInputLimit`

``` purescript
newtype ListTablesInputLimit
  = ListTablesInputLimit Int
```

<p>A number of maximum table names to return.</p>

##### Instances
``` purescript
Newtype ListTablesInputLimit _
```

#### `ListTablesOutput`

``` purescript
newtype ListTablesOutput
  = ListTablesOutput { "TableNames" :: NullOrUndefined (TableNameList), "LastEvaluatedTableName" :: NullOrUndefined (TableName) }
```

##### Instances
``` purescript
Newtype ListTablesOutput _
```

#### `NumberAttributeValue`

``` purescript
newtype NumberAttributeValue
  = NumberAttributeValue String
```

##### Instances
``` purescript
Newtype NumberAttributeValue _
```

#### `NumberSetAttributeValue`

``` purescript
newtype NumberSetAttributeValue
  = NumberSetAttributeValue (Array NumberAttributeValue)
```

##### Instances
``` purescript
Newtype NumberSetAttributeValue _
```

#### `PositiveIntegerObject`

``` purescript
newtype PositiveIntegerObject
  = PositiveIntegerObject Int
```

##### Instances
``` purescript
Newtype PositiveIntegerObject _
```

#### `PositiveLongObject`

``` purescript
newtype PositiveLongObject
  = PositiveLongObject Number
```

##### Instances
``` purescript
Newtype PositiveLongObject _
```

#### `ProvisionedThroughput`

``` purescript
newtype ProvisionedThroughput
  = ProvisionedThroughput { "ReadCapacityUnits" :: PositiveLongObject, "WriteCapacityUnits" :: PositiveLongObject }
```

<p>Provisioned throughput reserves the required read and write resources for your table in terms of <code>ReadCapacityUnits</code> and <code>WriteCapacityUnits</code>. Values for provisioned throughput depend upon your expected read/write rates, item size, and consistency. Provide the expected number of read and write operations, assuming an item size of 1k and strictly consistent reads. For 2k item size, double the value. For 3k, triple the value, etc. Eventually-consistent reads consume half the resources of strictly consistent reads.</p>

##### Instances
``` purescript
Newtype ProvisionedThroughput _
```

#### `ProvisionedThroughputDescription`

``` purescript
newtype ProvisionedThroughputDescription
  = ProvisionedThroughputDescription { "LastIncreaseDateTime" :: NullOrUndefined (Date), "LastDecreaseDateTime" :: NullOrUndefined (Date), "NumberOfDecreasesToday" :: NullOrUndefined (PositiveLongObject), "ReadCapacityUnits" :: NullOrUndefined (PositiveLongObject), "WriteCapacityUnits" :: NullOrUndefined (PositiveLongObject) }
```

##### Instances
``` purescript
Newtype ProvisionedThroughputDescription _
```

#### `ProvisionedThroughputExceededException`

``` purescript
newtype ProvisionedThroughputExceededException
  = ProvisionedThroughputExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This exception is thrown when the level of provisioned throughput defined for the table is exceeded.</p>

##### Instances
``` purescript
Newtype ProvisionedThroughputExceededException _
```

#### `PutItemInput`

``` purescript
newtype PutItemInput
  = PutItemInput { "TableName" :: TableName, "Item" :: PutItemInputAttributeMap, "Expected" :: NullOrUndefined (ExpectedAttributeMap), "ReturnValues" :: NullOrUndefined (ReturnValue) }
```

##### Instances
``` purescript
Newtype PutItemInput _
```

#### `PutItemInputAttributeMap`

``` purescript
newtype PutItemInputAttributeMap
  = PutItemInputAttributeMap (Map AttributeName AttributeValue)
```

<p>A map of the attributes for the item, and must include the primary key values that define the item. Other attribute name-value pairs can be provided for the item.</p>

##### Instances
``` purescript
Newtype PutItemInputAttributeMap _
```

#### `PutItemOutput`

``` purescript
newtype PutItemOutput
  = PutItemOutput { "Attributes" :: NullOrUndefined (AttributeMap), "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits) }
```

##### Instances
``` purescript
Newtype PutItemOutput _
```

#### `PutRequest`

``` purescript
newtype PutRequest
  = PutRequest { "Item" :: PutItemInputAttributeMap }
```

<p>A container for a Put BatchWrite request</p>

##### Instances
``` purescript
Newtype PutRequest _
```

#### `QueryInput`

``` purescript
newtype QueryInput
  = QueryInput { "TableName" :: TableName, "AttributesToGet" :: NullOrUndefined (AttributeNameList), "Limit" :: NullOrUndefined (PositiveIntegerObject), "ConsistentRead" :: NullOrUndefined (ConsistentRead), "Count" :: NullOrUndefined (BooleanObject), "HashKeyValue" :: AttributeValue, "RangeKeyCondition" :: NullOrUndefined (Condition), "ScanIndexForward" :: NullOrUndefined (BooleanObject), "ExclusiveStartKey" :: NullOrUndefined (Key) }
```

##### Instances
``` purescript
Newtype QueryInput _
```

#### `QueryOutput`

``` purescript
newtype QueryOutput
  = QueryOutput { "Items" :: NullOrUndefined (ItemList), "Count" :: NullOrUndefined (Int), "LastEvaluatedKey" :: NullOrUndefined (Key), "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits) }
```

##### Instances
``` purescript
Newtype QueryOutput _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This exception is thrown when the resource which is being attempted to be changed is in use.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>This exception is thrown when the resource which is being attempted to be changed is in use.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ReturnValue`

``` purescript
newtype ReturnValue
  = ReturnValue String
```

<p>Use this parameter if you want to get the attribute name-value pairs before or after they are modified. For <code>PUT</code> operations, the possible parameter values are <code>NONE</code> (default) or <code>ALL_OLD</code>. For update operations, the possible parameter values are <code>NONE</code> (default) or <code>ALL_OLD</code>, <code>UPDATED_OLD</code>, <code>ALL_NEW</code> or <code>UPDATED_NEW</code>.</p> <ul> <li><code>NONE</code>: Nothing is returned.</li> <li><code>ALL_OLD</code>: Returns the attributes of the item as they were before the operation.</li> <li><code>UPDATED_OLD</code>: Returns the values of the updated attributes, only, as they were before the operation.</li> <li><code>ALL_NEW</code>: Returns all the attributes and their new values after the operation.</li> <li><code>UPDATED_NEW</code>: Returns the values of the updated attributes, only, as they are after the operation.</li> </ul>

##### Instances
``` purescript
Newtype ReturnValue _
```

#### `ScalarAttributeType`

``` purescript
newtype ScalarAttributeType
  = ScalarAttributeType String
```

##### Instances
``` purescript
Newtype ScalarAttributeType _
```

#### `ScanInput`

``` purescript
newtype ScanInput
  = ScanInput { "TableName" :: TableName, "AttributesToGet" :: NullOrUndefined (AttributeNameList), "Limit" :: NullOrUndefined (PositiveIntegerObject), "Count" :: NullOrUndefined (BooleanObject), "ScanFilter" :: NullOrUndefined (FilterConditionMap), "ExclusiveStartKey" :: NullOrUndefined (Key) }
```

##### Instances
``` purescript
Newtype ScanInput _
```

#### `ScanOutput`

``` purescript
newtype ScanOutput
  = ScanOutput { "Items" :: NullOrUndefined (ItemList), "Count" :: NullOrUndefined (Int), "ScannedCount" :: NullOrUndefined (Int), "LastEvaluatedKey" :: NullOrUndefined (Key), "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits) }
```

##### Instances
``` purescript
Newtype ScanOutput _
```

#### `StringAttributeValue`

``` purescript
newtype StringAttributeValue
  = StringAttributeValue String
```

##### Instances
``` purescript
Newtype StringAttributeValue _
```

#### `StringSetAttributeValue`

``` purescript
newtype StringSetAttributeValue
  = StringSetAttributeValue (Array StringAttributeValue)
```

##### Instances
``` purescript
Newtype StringSetAttributeValue _
```

#### `TableDescription`

``` purescript
newtype TableDescription
  = TableDescription { "TableName" :: NullOrUndefined (TableName), "KeySchema" :: NullOrUndefined (KeySchema), "TableStatus" :: NullOrUndefined (TableStatus), "CreationDateTime" :: NullOrUndefined (Date), "ProvisionedThroughput" :: NullOrUndefined (ProvisionedThroughputDescription), "TableSizeBytes" :: NullOrUndefined (Number), "ItemCount" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype TableDescription _
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

#### `TableNameList`

``` purescript
newtype TableNameList
  = TableNameList (Array TableName)
```

##### Instances
``` purescript
Newtype TableNameList _
```

#### `TableStatus`

``` purescript
newtype TableStatus
  = TableStatus String
```

##### Instances
``` purescript
Newtype TableStatus _
```

#### `UpdateItemInput`

``` purescript
newtype UpdateItemInput
  = UpdateItemInput { "TableName" :: TableName, "Key" :: Key, "AttributeUpdates" :: AttributeUpdates, "Expected" :: NullOrUndefined (ExpectedAttributeMap), "ReturnValues" :: NullOrUndefined (ReturnValue) }
```

##### Instances
``` purescript
Newtype UpdateItemInput _
```

#### `UpdateItemOutput`

``` purescript
newtype UpdateItemOutput
  = UpdateItemOutput { "Attributes" :: NullOrUndefined (AttributeMap), "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits) }
```

##### Instances
``` purescript
Newtype UpdateItemOutput _
```

#### `UpdateTableInput`

``` purescript
newtype UpdateTableInput
  = UpdateTableInput { "TableName" :: TableName, "ProvisionedThroughput" :: ProvisionedThroughput }
```

##### Instances
``` purescript
Newtype UpdateTableInput _
```

#### `UpdateTableOutput`

``` purescript
newtype UpdateTableOutput
  = UpdateTableOutput { "TableDescription" :: NullOrUndefined (TableDescription) }
```

##### Instances
``` purescript
Newtype UpdateTableOutput _
```

#### `WriteRequest`

``` purescript
newtype WriteRequest
  = WriteRequest { "PutRequest" :: NullOrUndefined (PutRequest), "DeleteRequest" :: NullOrUndefined (DeleteRequest) }
```

<p>This structure is a Union of PutRequest and DeleteRequest. It can contain exactly one of <code>PutRequest</code> or <code>DeleteRequest</code>. Never Both. This is enforced in the code.</p>

##### Instances
``` purescript
Newtype WriteRequest _
```

#### `WriteRequests`

``` purescript
newtype WriteRequests
  = WriteRequests (Array WriteRequest)
```

##### Instances
``` purescript
Newtype WriteRequests _
```


