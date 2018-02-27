

-- | <p>Amazon DynamoDB is a fast, highly scalable, highly available, cost-effective non-relational database service.</p> <p>Amazon DynamoDB removes traditional scalability limitations on data storage while maintaining low latency and predictable performance.</p>
module AWS.DynamoDB where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "DynamoDB" :: String


-- | <p>Retrieves the attributes for multiple items from multiple tables using their primary keys.</p> <p>The maximum number of item attributes that can be retrieved for a single operation is 100. Also, the number of items retrieved is constrained by a 1 MB the size limit. If the response size limit is exceeded or a partial result is returned due to an internal processing failure, Amazon DynamoDB returns an <code>UnprocessedKeys</code> value so you can retry the operation starting with the next item to get.</p> <p>Amazon DynamoDB automatically adjusts the number of items returned per page to enforce this limit. For example, even if you ask to retrieve 100 items, but each individual item is 50k in size, the system returns 20 items and an appropriate <code>UnprocessedKeys</code> value so you can get the next page of results. If necessary, your application needs its own logic to assemble the pages of results into one set.</p>
batchGetItem :: forall eff. BatchGetItemInput -> Aff (err :: AWS.RequestError | eff) BatchGetItemOutput
batchGetItem = AWS.request serviceName "BatchGetItem" 


-- | <p>Allows to execute a batch of Put and/or Delete Requests for many tables in a single call. A total of 25 requests are allowed.</p> <p>There are no transaction guarantees provided by this API. It does not allow conditional puts nor does it support return values.</p>
batchWriteItem :: forall eff. BatchWriteItemInput -> Aff (err :: AWS.RequestError | eff) BatchWriteItemOutput
batchWriteItem = AWS.request serviceName "BatchWriteItem" 


-- | <p>Adds a new table to your account.</p> <p>The table name must be unique among those associated with the AWS Account issuing the request, and the AWS Region that receives the request (e.g. <code>us-east-1</code>).</p> <p>The <code>CreateTable</code> operation triggers an asynchronous workflow to begin creating the table. Amazon DynamoDB immediately returns the state of the table (<code>CREATING</code>) until the table is in the <code>ACTIVE</code> state. Once the table is in the <code>ACTIVE</code> state, you can perform data plane operations.</p>
createTable :: forall eff. CreateTableInput -> Aff (err :: AWS.RequestError | eff) CreateTableOutput
createTable = AWS.request serviceName "CreateTable" 


-- | <p>Deletes a single item in a table by primary key.</p> <p>You can perform a conditional delete operation that deletes the item if it exists, or if it has an expected attribute value.</p>
deleteItem :: forall eff. DeleteItemInput -> Aff (err :: AWS.RequestError | eff) DeleteItemOutput
deleteItem = AWS.request serviceName "DeleteItem" 


-- | <p>Deletes a table and all of its items.</p> <p>If the table is in the <code>ACTIVE</code> state, you can delete it. If a table is in <code>CREATING</code> or <code>UPDATING</code> states then Amazon DynamoDB returns a <code>ResourceInUseException</code>. If the specified table does not exist, Amazon DynamoDB returns a <code>ResourceNotFoundException</code>.</p>
deleteTable :: forall eff. DeleteTableInput -> Aff (err :: AWS.RequestError | eff) DeleteTableOutput
deleteTable = AWS.request serviceName "DeleteTable" 


-- | <p>Retrieves information about the table, including the current status of the table, the primary key schema and when the table was created.</p> <p>If the table does not exist, Amazon DynamoDB returns a <code>ResourceNotFoundException</code>.</p>
describeTable :: forall eff. DescribeTableInput -> Aff (err :: AWS.RequestError | eff) DescribeTableOutput
describeTable = AWS.request serviceName "DescribeTable" 


-- | <p>Retrieves a set of Attributes for an item that matches the primary key.</p> <p>The <code>GetItem</code> operation provides an eventually-consistent read by default. If eventually-consistent reads are not acceptable for your application, use <code>ConsistentRead</code>. Although this operation might take longer than a standard read, it always returns the last updated value.</p>
getItem :: forall eff. GetItemInput -> Aff (err :: AWS.RequestError | eff) GetItemOutput
getItem = AWS.request serviceName "GetItem" 


-- | <p>Retrieves a paginated list of table names created by the AWS Account of the caller in the AWS Region (e.g. <code>us-east-1</code>).</p>
listTables :: forall eff. ListTablesInput -> Aff (err :: AWS.RequestError | eff) ListTablesOutput
listTables = AWS.request serviceName "ListTables" 


-- | <p>Creates a new item, or replaces an old item with a new item (including all the attributes).</p> <p>If an item already exists in the specified table with the same primary key, the new item completely replaces the existing item. You can perform a conditional put (insert a new item if one with the specified primary key doesn't exist), or replace an existing item if it has certain attribute values.</p>
putItem :: forall eff. PutItemInput -> Aff (err :: AWS.RequestError | eff) PutItemOutput
putItem = AWS.request serviceName "PutItem" 


-- | <p>Gets the values of one or more items and its attributes by primary key (composite primary key, only).</p> <p>Narrow the scope of the query using comparison operators on the <code>RangeKeyValue</code> of the composite key. Use the <code>ScanIndexForward</code> parameter to get results in forward or reverse order by range key.</p>
query :: forall eff. QueryInput -> Aff (err :: AWS.RequestError | eff) QueryOutput
query = AWS.request serviceName "Query" 


-- | <p>Retrieves one or more items and its attributes by performing a full scan of a table.</p> <p>Provide a <code>ScanFilter</code> to get more specific results.</p>
scan :: forall eff. ScanInput -> Aff (err :: AWS.RequestError | eff) ScanOutput
scan = AWS.request serviceName "Scan" 


-- | <p>Edits an existing item's attributes.</p> <p>You can perform a conditional update (insert a new attribute name-value pair if it doesn't exist, or replace an existing name-value pair if it has certain expected attribute values).</p>
updateItem :: forall eff. UpdateItemInput -> Aff (err :: AWS.RequestError | eff) UpdateItemOutput
updateItem = AWS.request serviceName "UpdateItem" 


-- | <p>Updates the provisioned throughput for the given table.</p> <p>Setting the throughput for a table helps you manage performance and is part of the Provisioned Throughput feature of Amazon DynamoDB.</p>
updateTable :: forall eff. UpdateTableInput -> Aff (err :: AWS.RequestError | eff) UpdateTableOutput
updateTable = AWS.request serviceName "UpdateTable" 


-- | <p>The type of action for an item update operation. Only use the add action for numbers or sets; the specified value is added to the existing value. If a set of values is specified, the values are added to the existing set. Adds the specified attribute. If the attribute exists, it is replaced by the new value. If no value is specified, this removes the attribute and its value. If a set of values is specified, then the values in the specified set are removed from the old set.</p>
newtype AttributeAction = AttributeAction String
derive instance newtypeAttributeAction :: Newtype AttributeAction _


newtype AttributeMap = AttributeMap (Map AttributeName AttributeValue)
derive instance newtypeAttributeMap :: Newtype AttributeMap _


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _


-- | <p>List of <code>Attribute</code> names. If attribute names are not specified then all attributes will be returned. If some attributes are not found, they will not appear in the result.</p>
newtype AttributeNameList = AttributeNameList (Array AttributeName)
derive instance newtypeAttributeNameList :: Newtype AttributeNameList _


-- | <p>Map of attribute name to the new value and action for the update. The attribute names specify the attributes to modify, and cannot contain any primary key attributes.</p>
newtype AttributeUpdates = AttributeUpdates (Map AttributeName AttributeValueUpdate)
derive instance newtypeAttributeUpdates :: Newtype AttributeUpdates _


-- | <p>AttributeValue can be <code>String</code>, <code>Number</code>, <code>Binary</code>, <code>StringSet</code>, <code>NumberSet</code>, <code>BinarySet</code>.</p>
newtype AttributeValue = AttributeValue 
  { "S" :: NullOrUndefined (StringAttributeValue)
  , "N" :: NullOrUndefined (NumberAttributeValue)
  , "B" :: NullOrUndefined (BinaryAttributeValue)
  , "SS" :: NullOrUndefined (StringSetAttributeValue)
  , "NS" :: NullOrUndefined (NumberSetAttributeValue)
  , "BS" :: NullOrUndefined (BinarySetAttributeValue)
  }
derive instance newtypeAttributeValue :: Newtype AttributeValue _


-- | <p>A list of attribute values to be used with a comparison operator for a scan or query operation. For comparisons that require more than one value, such as a <code>BETWEEN</code> comparison, the AttributeValueList contains two attribute values and the comparison operator.</p>
newtype AttributeValueList = AttributeValueList (Array AttributeValue)
derive instance newtypeAttributeValueList :: Newtype AttributeValueList _


-- | <p>Specifies the attribute to update and how to perform the update. Possible values: <code>PUT</code> (default), <code>ADD</code> or <code>DELETE</code>.</p>
newtype AttributeValueUpdate = AttributeValueUpdate 
  { "Value" :: NullOrUndefined (AttributeValue)
  , "Action" :: NullOrUndefined (AttributeAction)
  }
derive instance newtypeAttributeValueUpdate :: Newtype AttributeValueUpdate _


newtype BatchGetItemInput = BatchGetItemInput 
  { "RequestItems" :: (BatchGetRequestMap)
  }
derive instance newtypeBatchGetItemInput :: Newtype BatchGetItemInput _


newtype BatchGetItemOutput = BatchGetItemOutput 
  { "Responses" :: NullOrUndefined (BatchGetResponseMap)
  , "UnprocessedKeys" :: NullOrUndefined (BatchGetRequestMap)
  }
derive instance newtypeBatchGetItemOutput :: Newtype BatchGetItemOutput _


-- | <p>A map of the table name and corresponding items to get by primary key. While requesting items, each table name can be invoked only once per operation.</p>
newtype BatchGetRequestMap = BatchGetRequestMap (Map TableName KeysAndAttributes)
derive instance newtypeBatchGetRequestMap :: Newtype BatchGetRequestMap _


-- | <p>Table names and the respective item attributes from the tables.</p>
newtype BatchGetResponseMap = BatchGetResponseMap (Map TableName BatchResponse)
derive instance newtypeBatchGetResponseMap :: Newtype BatchGetResponseMap _


-- | <p>The item attributes from a response in a specific table, along with the read resources consumed on the table during the request.</p>
newtype BatchResponse = BatchResponse 
  { "Items" :: NullOrUndefined (ItemList)
  , "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits)
  }
derive instance newtypeBatchResponse :: Newtype BatchResponse _


newtype BatchWriteItemInput = BatchWriteItemInput 
  { "RequestItems" :: (BatchWriteItemRequestMap)
  }
derive instance newtypeBatchWriteItemInput :: Newtype BatchWriteItemInput _


-- | <p>A container for <code>BatchWriteItem</code> response</p>
newtype BatchWriteItemOutput = BatchWriteItemOutput 
  { "Responses" :: NullOrUndefined (BatchWriteResponseMap)
  , "UnprocessedItems" :: NullOrUndefined (BatchWriteItemRequestMap)
  }
derive instance newtypeBatchWriteItemOutput :: Newtype BatchWriteItemOutput _


-- | <p>A map of table name to list-of-write-requests.</p> <p>Key: The table name corresponding to the list of requests</p> <p>Value: Essentially a list of request items. Each request item could contain either a <code>PutRequest</code> or <code>DeleteRequest</code>. Never both.</p>
newtype BatchWriteItemRequestMap = BatchWriteItemRequestMap (Map TableName WriteRequests)
derive instance newtypeBatchWriteItemRequestMap :: Newtype BatchWriteItemRequestMap _


newtype BatchWriteResponse = BatchWriteResponse 
  { "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits)
  }
derive instance newtypeBatchWriteResponse :: Newtype BatchWriteResponse _


newtype BatchWriteResponseMap = BatchWriteResponseMap (Map TableName BatchWriteResponse)
derive instance newtypeBatchWriteResponseMap :: Newtype BatchWriteResponseMap _


newtype BinaryAttributeValue = BinaryAttributeValue String
derive instance newtypeBinaryAttributeValue :: Newtype BinaryAttributeValue _


newtype BinarySetAttributeValue = BinarySetAttributeValue (Array BinaryAttributeValue)
derive instance newtypeBinarySetAttributeValue :: Newtype BinarySetAttributeValue _


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _


-- | <p>A comparison operator is an enumeration of several operations:</p> <ul> <li><code>EQ</code> for <em>equal</em>.</li> <li><code>NE</code> for <em>not equal</em>.</li> <li><code>IN</code> checks for exact matches.</li> <li><code>LE</code> for <em>less than or equal to</em>.</li> <li><code>LT</code> for <em>less than</em>.</li> <li><code>GE</code> for <em>greater than or equal to</em>.</li> <li><code>GT</code> for <em>greater than</em>.</li> <li><code>BETWEEN</code> for <em>between</em>.</li> <li><code>NOT_NULL</code> for <em>exists</em>.</li> <li><code>NULL</code> for <em>not exists</em>.</li> <li><code>CONTAINS</code> for substring or value in a set.</li> <li><code>NOT_CONTAINS</code> for absence of a substring or absence of a value in a set.</li> <li><code>BEGINS_WITH</code> for a substring prefix.</li> </ul> <p>Scan operations support all available comparison operators.</p> <p>Query operations support a subset of the available comparison operators: EQ, LE, LT, GE, GT, BETWEEN, and BEGINS_WITH.</p>
newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _


newtype Condition = Condition 
  { "AttributeValueList" :: NullOrUndefined (AttributeValueList)
  , "ComparisonOperator" :: (ComparisonOperator)
  }
derive instance newtypeCondition :: Newtype Condition _


-- | <p>This exception is thrown when an expected value does not match what was found in the system.</p>
newtype ConditionalCheckFailedException = ConditionalCheckFailedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConditionalCheckFailedException :: Newtype ConditionalCheckFailedException _


-- | <p>If set to <code>true</code>, then a consistent read is issued. Otherwise eventually-consistent is used.</p>
newtype ConsistentRead = ConsistentRead Boolean
derive instance newtypeConsistentRead :: Newtype ConsistentRead _


-- | <p>The number of Capacity Units of the provisioned throughput of the table consumed during the operation. <code>GetItem</code>, <code>BatchGetItem</code>, <code>BatchWriteItem</code>, <code>Query</code>, and <code>Scan</code> operations consume <code>ReadCapacityUnits</code>, while <code>PutItem</code>, <code>UpdateItem</code>, and <code>DeleteItem</code> operations consume <code>WriteCapacityUnits</code>.</p>
newtype ConsumedCapacityUnits = ConsumedCapacityUnits Number
derive instance newtypeConsumedCapacityUnits :: Newtype ConsumedCapacityUnits _


newtype CreateTableInput = CreateTableInput 
  { "TableName" :: (TableName)
  , "KeySchema" :: (KeySchema)
  , "ProvisionedThroughput" :: (ProvisionedThroughput)
  }
derive instance newtypeCreateTableInput :: Newtype CreateTableInput _


newtype CreateTableOutput = CreateTableOutput 
  { "TableDescription" :: NullOrUndefined (TableDescription)
  }
derive instance newtypeCreateTableOutput :: Newtype CreateTableOutput _


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _


newtype DeleteItemInput = DeleteItemInput 
  { "TableName" :: (TableName)
  , "Key" :: (Key)
  , "Expected" :: NullOrUndefined (ExpectedAttributeMap)
  , "ReturnValues" :: NullOrUndefined (ReturnValue)
  }
derive instance newtypeDeleteItemInput :: Newtype DeleteItemInput _


newtype DeleteItemOutput = DeleteItemOutput 
  { "Attributes" :: NullOrUndefined (AttributeMap)
  , "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits)
  }
derive instance newtypeDeleteItemOutput :: Newtype DeleteItemOutput _


-- | <p>A container for a Delete BatchWrite request</p>
newtype DeleteRequest = DeleteRequest 
  { "Key" :: (Key)
  }
derive instance newtypeDeleteRequest :: Newtype DeleteRequest _


newtype DeleteTableInput = DeleteTableInput 
  { "TableName" :: (TableName)
  }
derive instance newtypeDeleteTableInput :: Newtype DeleteTableInput _


newtype DeleteTableOutput = DeleteTableOutput 
  { "TableDescription" :: NullOrUndefined (TableDescription)
  }
derive instance newtypeDeleteTableOutput :: Newtype DeleteTableOutput _


newtype DescribeTableInput = DescribeTableInput 
  { "TableName" :: (TableName)
  }
derive instance newtypeDescribeTableInput :: Newtype DescribeTableInput _


newtype DescribeTableOutput = DescribeTableOutput 
  { "Table" :: NullOrUndefined (TableDescription)
  }
derive instance newtypeDescribeTableOutput :: Newtype DescribeTableOutput _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>Designates an attribute for a conditional modification. The <code>Expected</code> parameter allows you to provide an attribute name, and whether or not Amazon DynamoDB should check to see if the attribute has a particular value before modifying it.</p>
newtype ExpectedAttributeMap = ExpectedAttributeMap (Map AttributeName ExpectedAttributeValue)
derive instance newtypeExpectedAttributeMap :: Newtype ExpectedAttributeMap _


-- | <p>Allows you to provide an attribute name, and whether or not Amazon DynamoDB should check to see if the attribute value already exists; or if the attribute value exists and has a particular value before changing it.</p>
newtype ExpectedAttributeValue = ExpectedAttributeValue 
  { "Value" :: NullOrUndefined (AttributeValue)
  , "Exists" :: NullOrUndefined (BooleanObject)
  }
derive instance newtypeExpectedAttributeValue :: Newtype ExpectedAttributeValue _


newtype FilterConditionMap = FilterConditionMap (Map String Condition)
derive instance newtypeFilterConditionMap :: Newtype FilterConditionMap _


newtype GetItemInput = GetItemInput 
  { "TableName" :: (TableName)
  , "Key" :: (Key)
  , "AttributesToGet" :: NullOrUndefined (AttributeNameList)
  , "ConsistentRead" :: NullOrUndefined (ConsistentRead)
  }
derive instance newtypeGetItemInput :: Newtype GetItemInput _


newtype GetItemOutput = GetItemOutput 
  { "Item" :: NullOrUndefined (AttributeMap)
  , "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits)
  }
derive instance newtypeGetItemOutput :: Newtype GetItemOutput _


-- | <p>This exception is thrown when the service has a problem when trying to process the request.</p>
newtype InternalServerError = InternalServerError 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _


newtype ItemList = ItemList (Array AttributeMap)
derive instance newtypeItemList :: Newtype ItemList _


-- | <p>The primary key that uniquely identifies each item in a table. A primary key can be a one attribute (hash) primary key or a two attribute (hash-and-range) primary key.</p>
newtype Key = Key 
  { "HashKeyElement" :: (AttributeValue)
  , "RangeKeyElement" :: NullOrUndefined (AttributeValue)
  }
derive instance newtypeKey :: Newtype Key _


newtype KeyList = KeyList (Array Key)
derive instance newtypeKeyList :: Newtype KeyList _


-- | <p>The KeySchema identifies the primary key as a one attribute primary key (hash) or a composite two attribute (hash-and-range) primary key. Single attribute primary keys have one index value: a <code>HashKeyElement</code>. A composite hash-and-range primary key contains two attribute values: a <code>HashKeyElement</code> and a <code>RangeKeyElement</code>.</p>
newtype KeySchema = KeySchema 
  { "HashKeyElement" :: (KeySchemaElement)
  , "RangeKeyElement" :: NullOrUndefined (KeySchemaElement)
  }
derive instance newtypeKeySchema :: Newtype KeySchema _


newtype KeySchemaAttributeName = KeySchemaAttributeName String
derive instance newtypeKeySchemaAttributeName :: Newtype KeySchemaAttributeName _


-- | <p><code>KeySchemaElement</code> is the primary key (hash or hash-and-range) structure for the table.</p>
newtype KeySchemaElement = KeySchemaElement 
  { "AttributeName" :: (KeySchemaAttributeName)
  , "AttributeType" :: (ScalarAttributeType)
  }
derive instance newtypeKeySchemaElement :: Newtype KeySchemaElement _


newtype KeysAndAttributes = KeysAndAttributes 
  { "Keys" :: (KeyList)
  , "AttributesToGet" :: NullOrUndefined (AttributeNameList)
  , "ConsistentRead" :: NullOrUndefined (ConsistentRead)
  }
derive instance newtypeKeysAndAttributes :: Newtype KeysAndAttributes _


-- | <p>This exception is thrown when the subscriber exceeded the limits on the number of objects or operations.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListTablesInput = ListTablesInput 
  { "ExclusiveStartTableName" :: NullOrUndefined (TableName)
  , "Limit" :: NullOrUndefined (ListTablesInputLimit)
  }
derive instance newtypeListTablesInput :: Newtype ListTablesInput _


-- | <p>A number of maximum table names to return.</p>
newtype ListTablesInputLimit = ListTablesInputLimit Int
derive instance newtypeListTablesInputLimit :: Newtype ListTablesInputLimit _


newtype ListTablesOutput = ListTablesOutput 
  { "TableNames" :: NullOrUndefined (TableNameList)
  , "LastEvaluatedTableName" :: NullOrUndefined (TableName)
  }
derive instance newtypeListTablesOutput :: Newtype ListTablesOutput _


newtype NumberAttributeValue = NumberAttributeValue String
derive instance newtypeNumberAttributeValue :: Newtype NumberAttributeValue _


newtype NumberSetAttributeValue = NumberSetAttributeValue (Array NumberAttributeValue)
derive instance newtypeNumberSetAttributeValue :: Newtype NumberSetAttributeValue _


newtype PositiveIntegerObject = PositiveIntegerObject Int
derive instance newtypePositiveIntegerObject :: Newtype PositiveIntegerObject _


newtype PositiveLongObject = PositiveLongObject Number
derive instance newtypePositiveLongObject :: Newtype PositiveLongObject _


-- | <p>Provisioned throughput reserves the required read and write resources for your table in terms of <code>ReadCapacityUnits</code> and <code>WriteCapacityUnits</code>. Values for provisioned throughput depend upon your expected read/write rates, item size, and consistency. Provide the expected number of read and write operations, assuming an item size of 1k and strictly consistent reads. For 2k item size, double the value. For 3k, triple the value, etc. Eventually-consistent reads consume half the resources of strictly consistent reads.</p>
newtype ProvisionedThroughput = ProvisionedThroughput 
  { "ReadCapacityUnits" :: (PositiveLongObject)
  , "WriteCapacityUnits" :: (PositiveLongObject)
  }
derive instance newtypeProvisionedThroughput :: Newtype ProvisionedThroughput _


newtype ProvisionedThroughputDescription = ProvisionedThroughputDescription 
  { "LastIncreaseDateTime" :: NullOrUndefined (Date)
  , "LastDecreaseDateTime" :: NullOrUndefined (Date)
  , "NumberOfDecreasesToday" :: NullOrUndefined (PositiveLongObject)
  , "ReadCapacityUnits" :: NullOrUndefined (PositiveLongObject)
  , "WriteCapacityUnits" :: NullOrUndefined (PositiveLongObject)
  }
derive instance newtypeProvisionedThroughputDescription :: Newtype ProvisionedThroughputDescription _


-- | <p>This exception is thrown when the level of provisioned throughput defined for the table is exceeded.</p>
newtype ProvisionedThroughputExceededException = ProvisionedThroughputExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeProvisionedThroughputExceededException :: Newtype ProvisionedThroughputExceededException _


newtype PutItemInput = PutItemInput 
  { "TableName" :: (TableName)
  , "Item" :: (PutItemInputAttributeMap)
  , "Expected" :: NullOrUndefined (ExpectedAttributeMap)
  , "ReturnValues" :: NullOrUndefined (ReturnValue)
  }
derive instance newtypePutItemInput :: Newtype PutItemInput _


-- | <p>A map of the attributes for the item, and must include the primary key values that define the item. Other attribute name-value pairs can be provided for the item.</p>
newtype PutItemInputAttributeMap = PutItemInputAttributeMap (Map AttributeName AttributeValue)
derive instance newtypePutItemInputAttributeMap :: Newtype PutItemInputAttributeMap _


newtype PutItemOutput = PutItemOutput 
  { "Attributes" :: NullOrUndefined (AttributeMap)
  , "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits)
  }
derive instance newtypePutItemOutput :: Newtype PutItemOutput _


-- | <p>A container for a Put BatchWrite request</p>
newtype PutRequest = PutRequest 
  { "Item" :: (PutItemInputAttributeMap)
  }
derive instance newtypePutRequest :: Newtype PutRequest _


newtype QueryInput = QueryInput 
  { "TableName" :: (TableName)
  , "AttributesToGet" :: NullOrUndefined (AttributeNameList)
  , "Limit" :: NullOrUndefined (PositiveIntegerObject)
  , "ConsistentRead" :: NullOrUndefined (ConsistentRead)
  , "Count" :: NullOrUndefined (BooleanObject)
  , "HashKeyValue" :: (AttributeValue)
  , "RangeKeyCondition" :: NullOrUndefined (Condition)
  , "ScanIndexForward" :: NullOrUndefined (BooleanObject)
  , "ExclusiveStartKey" :: NullOrUndefined (Key)
  }
derive instance newtypeQueryInput :: Newtype QueryInput _


newtype QueryOutput = QueryOutput 
  { "Items" :: NullOrUndefined (ItemList)
  , "Count" :: NullOrUndefined (Int)
  , "LastEvaluatedKey" :: NullOrUndefined (Key)
  , "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits)
  }
derive instance newtypeQueryOutput :: Newtype QueryOutput _


-- | <p>This exception is thrown when the resource which is being attempted to be changed is in use.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>This exception is thrown when the resource which is being attempted to be changed is in use.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>Use this parameter if you want to get the attribute name-value pairs before or after they are modified. For <code>PUT</code> operations, the possible parameter values are <code>NONE</code> (default) or <code>ALL_OLD</code>. For update operations, the possible parameter values are <code>NONE</code> (default) or <code>ALL_OLD</code>, <code>UPDATED_OLD</code>, <code>ALL_NEW</code> or <code>UPDATED_NEW</code>.</p> <ul> <li><code>NONE</code>: Nothing is returned.</li> <li><code>ALL_OLD</code>: Returns the attributes of the item as they were before the operation.</li> <li><code>UPDATED_OLD</code>: Returns the values of the updated attributes, only, as they were before the operation.</li> <li><code>ALL_NEW</code>: Returns all the attributes and their new values after the operation.</li> <li><code>UPDATED_NEW</code>: Returns the values of the updated attributes, only, as they are after the operation.</li> </ul>
newtype ReturnValue = ReturnValue String
derive instance newtypeReturnValue :: Newtype ReturnValue _


newtype ScalarAttributeType = ScalarAttributeType String
derive instance newtypeScalarAttributeType :: Newtype ScalarAttributeType _


newtype ScanInput = ScanInput 
  { "TableName" :: (TableName)
  , "AttributesToGet" :: NullOrUndefined (AttributeNameList)
  , "Limit" :: NullOrUndefined (PositiveIntegerObject)
  , "Count" :: NullOrUndefined (BooleanObject)
  , "ScanFilter" :: NullOrUndefined (FilterConditionMap)
  , "ExclusiveStartKey" :: NullOrUndefined (Key)
  }
derive instance newtypeScanInput :: Newtype ScanInput _


newtype ScanOutput = ScanOutput 
  { "Items" :: NullOrUndefined (ItemList)
  , "Count" :: NullOrUndefined (Int)
  , "ScannedCount" :: NullOrUndefined (Int)
  , "LastEvaluatedKey" :: NullOrUndefined (Key)
  , "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits)
  }
derive instance newtypeScanOutput :: Newtype ScanOutput _


newtype StringAttributeValue = StringAttributeValue String
derive instance newtypeStringAttributeValue :: Newtype StringAttributeValue _


newtype StringSetAttributeValue = StringSetAttributeValue (Array StringAttributeValue)
derive instance newtypeStringSetAttributeValue :: Newtype StringSetAttributeValue _


newtype TableDescription = TableDescription 
  { "TableName" :: NullOrUndefined (TableName)
  , "KeySchema" :: NullOrUndefined (KeySchema)
  , "TableStatus" :: NullOrUndefined (TableStatus)
  , "CreationDateTime" :: NullOrUndefined (Date)
  , "ProvisionedThroughput" :: NullOrUndefined (ProvisionedThroughputDescription)
  , "TableSizeBytes" :: NullOrUndefined (Number)
  , "ItemCount" :: NullOrUndefined (Number)
  }
derive instance newtypeTableDescription :: Newtype TableDescription _


newtype TableName = TableName String
derive instance newtypeTableName :: Newtype TableName _


newtype TableNameList = TableNameList (Array TableName)
derive instance newtypeTableNameList :: Newtype TableNameList _


newtype TableStatus = TableStatus String
derive instance newtypeTableStatus :: Newtype TableStatus _


newtype UpdateItemInput = UpdateItemInput 
  { "TableName" :: (TableName)
  , "Key" :: (Key)
  , "AttributeUpdates" :: (AttributeUpdates)
  , "Expected" :: NullOrUndefined (ExpectedAttributeMap)
  , "ReturnValues" :: NullOrUndefined (ReturnValue)
  }
derive instance newtypeUpdateItemInput :: Newtype UpdateItemInput _


newtype UpdateItemOutput = UpdateItemOutput 
  { "Attributes" :: NullOrUndefined (AttributeMap)
  , "ConsumedCapacityUnits" :: NullOrUndefined (ConsumedCapacityUnits)
  }
derive instance newtypeUpdateItemOutput :: Newtype UpdateItemOutput _


newtype UpdateTableInput = UpdateTableInput 
  { "TableName" :: (TableName)
  , "ProvisionedThroughput" :: (ProvisionedThroughput)
  }
derive instance newtypeUpdateTableInput :: Newtype UpdateTableInput _


newtype UpdateTableOutput = UpdateTableOutput 
  { "TableDescription" :: NullOrUndefined (TableDescription)
  }
derive instance newtypeUpdateTableOutput :: Newtype UpdateTableOutput _


-- | <p>This structure is a Union of PutRequest and DeleteRequest. It can contain exactly one of <code>PutRequest</code> or <code>DeleteRequest</code>. Never Both. This is enforced in the code.</p>
newtype WriteRequest = WriteRequest 
  { "PutRequest" :: NullOrUndefined (PutRequest)
  , "DeleteRequest" :: NullOrUndefined (DeleteRequest)
  }
derive instance newtypeWriteRequest :: Newtype WriteRequest _


newtype WriteRequests = WriteRequests (Array WriteRequest)
derive instance newtypeWriteRequests :: Newtype WriteRequests _
