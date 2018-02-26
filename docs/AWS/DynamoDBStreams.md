## Module AWS.DynamoDBStreams

<fullname>Amazon DynamoDB</fullname> <p>Amazon DynamoDB Streams provides API actions for accessing streams and processing stream records. To learn more about application development with Streams, see <a href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html">Capturing Table Activity with DynamoDB Streams</a> in the Amazon DynamoDB Developer Guide.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `describeStream`

``` purescript
describeStream :: forall eff. DescribeStreamInput -> Aff (err :: RequestError | eff) DescribeStreamOutput
```

<p>Returns information about a stream, including the current status of the stream, its Amazon Resource Name (ARN), the composition of its shards, and its corresponding DynamoDB table.</p> <note> <p>You can call <code>DescribeStream</code> at a maximum rate of 10 times per second.</p> </note> <p>Each shard in the stream has a <code>SequenceNumberRange</code> associated with it. If the <code>SequenceNumberRange</code> has a <code>StartingSequenceNumber</code> but no <code>EndingSequenceNumber</code>, then the shard is still open (able to receive more stream records). If both <code>StartingSequenceNumber</code> and <code>EndingSequenceNumber</code> are present, then that shard is closed and can no longer receive more data.</p>

#### `getRecords`

``` purescript
getRecords :: forall eff. GetRecordsInput -> Aff (err :: RequestError | eff) GetRecordsOutput
```

<p>Retrieves the stream records from a given shard.</p> <p>Specify a shard iterator using the <code>ShardIterator</code> parameter. The shard iterator specifies the position in the shard from which you want to start reading stream records sequentially. If there are no stream records available in the portion of the shard that the iterator points to, <code>GetRecords</code> returns an empty list. Note that it might take multiple calls to get to a portion of the shard that contains stream records.</p> <note> <p> <code>GetRecords</code> can retrieve a maximum of 1 MB of data or 1000 stream records, whichever comes first.</p> </note>

#### `getShardIterator`

``` purescript
getShardIterator :: forall eff. GetShardIteratorInput -> Aff (err :: RequestError | eff) GetShardIteratorOutput
```

<p>Returns a shard iterator. A shard iterator provides information about how to retrieve the stream records from within a shard. Use the shard iterator in a subsequent <code>GetRecords</code> request to read the stream records from the shard.</p> <note> <p>A shard iterator expires 15 minutes after it is returned to the requester.</p> </note>

#### `listStreams`

``` purescript
listStreams :: forall eff. ListStreamsInput -> Aff (err :: RequestError | eff) ListStreamsOutput
```

<p>Returns an array of stream ARNs associated with the current account and endpoint. If the <code>TableName</code> parameter is present, then <code>ListStreams</code> will return only the streams ARNs for that table.</p> <note> <p>You can call <code>ListStreams</code> at a maximum rate of 5 times per second.</p> </note>

#### `AttributeMap`

``` purescript
newtype AttributeMap
  = AttributeMap (Map AttributeName AttributeValue)
```

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue { "S" :: NullOrUndefined (StringAttributeValue), "N" :: NullOrUndefined (NumberAttributeValue), "B" :: NullOrUndefined (BinaryAttributeValue), "SS" :: NullOrUndefined (StringSetAttributeValue), "NS" :: NullOrUndefined (NumberSetAttributeValue), "BS" :: NullOrUndefined (BinarySetAttributeValue), "M" :: NullOrUndefined (MapAttributeValue), "L" :: NullOrUndefined (ListAttributeValue), "NULL" :: NullOrUndefined (NullAttributeValue), "BOOL" :: NullOrUndefined (BooleanAttributeValue) }
```

<p>Represents the data for an attribute. You can set one, and only one, of the elements.</p> <p>Each attribute in an item is a name-value pair. An attribute can be single-valued or multi-valued set. For example, a book item can have title and authors attributes. Each book has one title but can have many authors. The multi-valued attribute is a set; duplicate values are not allowed.</p>

#### `BinaryAttributeValue`

``` purescript
newtype BinaryAttributeValue
  = BinaryAttributeValue String
```

#### `BinarySetAttributeValue`

``` purescript
newtype BinarySetAttributeValue
  = BinarySetAttributeValue (Array BinaryAttributeValue)
```

#### `BooleanAttributeValue`

``` purescript
newtype BooleanAttributeValue
  = BooleanAttributeValue Boolean
```

#### `Date`

``` purescript
newtype Date
  = Date Number
```

#### `DescribeStreamInput`

``` purescript
newtype DescribeStreamInput
  = DescribeStreamInput { "StreamArn" :: StreamArn, "Limit" :: NullOrUndefined (PositiveIntegerObject), "ExclusiveStartShardId" :: NullOrUndefined (ShardId) }
```

<p>Represents the input of a <code>DescribeStream</code> operation.</p>

#### `DescribeStreamOutput`

``` purescript
newtype DescribeStreamOutput
  = DescribeStreamOutput { "StreamDescription" :: NullOrUndefined (StreamDescription) }
```

<p>Represents the output of a <code>DescribeStream</code> operation.</p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `ExpiredIteratorException`

``` purescript
newtype ExpiredIteratorException
  = ExpiredIteratorException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The shard iterator has expired and can no longer be used to retrieve stream records. A shard iterator expires 15 minutes after it is retrieved using the <code>GetShardIterator</code> action.</p>

#### `GetRecordsInput`

``` purescript
newtype GetRecordsInput
  = GetRecordsInput { "ShardIterator" :: ShardIterator, "Limit" :: NullOrUndefined (PositiveIntegerObject) }
```

<p>Represents the input of a <code>GetRecords</code> operation.</p>

#### `GetRecordsOutput`

``` purescript
newtype GetRecordsOutput
  = GetRecordsOutput { "Records" :: NullOrUndefined (RecordList), "NextShardIterator" :: NullOrUndefined (ShardIterator) }
```

<p>Represents the output of a <code>GetRecords</code> operation.</p>

#### `GetShardIteratorInput`

``` purescript
newtype GetShardIteratorInput
  = GetShardIteratorInput { "StreamArn" :: StreamArn, "ShardId" :: ShardId, "ShardIteratorType" :: ShardIteratorType, "SequenceNumber" :: NullOrUndefined (SequenceNumber) }
```

<p>Represents the input of a <code>GetShardIterator</code> operation.</p>

#### `GetShardIteratorOutput`

``` purescript
newtype GetShardIteratorOutput
  = GetShardIteratorOutput { "ShardIterator" :: NullOrUndefined (ShardIterator) }
```

<p>Represents the output of a <code>GetShardIterator</code> operation.</p>

#### `Identity`

``` purescript
newtype Identity
  = Identity { "PrincipalId" :: NullOrUndefined (String), "Type" :: NullOrUndefined (String) }
```

<p>Contains details about the type of identity that made the request.</p>

#### `InternalServerError`

``` purescript
newtype InternalServerError
  = InternalServerError { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>An error occurred on the server side.</p>

#### `KeySchema`

``` purescript
newtype KeySchema
  = KeySchema (Array KeySchemaElement)
```

#### `KeySchemaAttributeName`

``` purescript
newtype KeySchemaAttributeName
  = KeySchemaAttributeName String
```

#### `KeySchemaElement`

``` purescript
newtype KeySchemaElement
  = KeySchemaElement { "AttributeName" :: KeySchemaAttributeName, "KeyType" :: KeyType }
```

<p>Represents <i>a single element</i> of a key schema. A key schema specifies the attributes that make up the primary key of a table, or the key attributes of an index.</p> <p>A <code>KeySchemaElement</code> represents exactly one attribute of the primary key. For example, a simple primary key (partition key) would be represented by one <code>KeySchemaElement</code>. A composite primary key (partition key and sort key) would require one <code>KeySchemaElement</code> for the partition key, and another <code>KeySchemaElement</code> for the sort key.</p> <note> <p>The partition key of an item is also known as its <i>hash attribute</i>. The term "hash attribute" derives from DynamoDB's usage of an internal hash function to evenly distribute data items across partitions, based on their partition key values.</p> <p>The sort key of an item is also known as its <i>range attribute</i>. The term "range attribute" derives from the way DynamoDB stores items with the same partition key physically close together, in sorted order by the sort key value.</p> </note>

#### `KeyType`

``` purescript
newtype KeyType
  = KeyType String
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Your request rate is too high. The AWS SDKs for DynamoDB automatically retry requests that receive this exception. Your request is eventually successful, unless your retry queue is too large to finish. Reduce the frequency of requests and use exponential backoff. For more information, go to <a href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#APIRetries">Error Retries and Exponential Backoff</a> in the <i>Amazon DynamoDB Developer Guide</i>.</p>

#### `ListAttributeValue`

``` purescript
newtype ListAttributeValue
  = ListAttributeValue (Array AttributeValue)
```

#### `ListStreamsInput`

``` purescript
newtype ListStreamsInput
  = ListStreamsInput { "TableName" :: NullOrUndefined (TableName), "Limit" :: NullOrUndefined (PositiveIntegerObject), "ExclusiveStartStreamArn" :: NullOrUndefined (StreamArn) }
```

<p>Represents the input of a <code>ListStreams</code> operation.</p>

#### `ListStreamsOutput`

``` purescript
newtype ListStreamsOutput
  = ListStreamsOutput { "Streams" :: NullOrUndefined (StreamList), "LastEvaluatedStreamArn" :: NullOrUndefined (StreamArn) }
```

<p>Represents the output of a <code>ListStreams</code> operation.</p>

#### `MapAttributeValue`

``` purescript
newtype MapAttributeValue
  = MapAttributeValue (Map AttributeName AttributeValue)
```

#### `NullAttributeValue`

``` purescript
newtype NullAttributeValue
  = NullAttributeValue Boolean
```

#### `NumberAttributeValue`

``` purescript
newtype NumberAttributeValue
  = NumberAttributeValue String
```

#### `NumberSetAttributeValue`

``` purescript
newtype NumberSetAttributeValue
  = NumberSetAttributeValue (Array NumberAttributeValue)
```

#### `OperationType`

``` purescript
newtype OperationType
  = OperationType String
```

#### `PositiveIntegerObject`

``` purescript
newtype PositiveIntegerObject
  = PositiveIntegerObject Int
```

#### `PositiveLongObject`

``` purescript
newtype PositiveLongObject
  = PositiveLongObject Number
```

#### `Record''`

``` purescript
newtype Record''
  = Record'' { "EventID'" :: NullOrUndefined (String), "EventName'" :: NullOrUndefined (OperationType), "EventVersion'" :: NullOrUndefined (String), "EventSource'" :: NullOrUndefined (String), "AwsRegion'" :: NullOrUndefined (String), "Dynamodb'" :: NullOrUndefined (StreamRecord), "UserIdentity'" :: NullOrUndefined (Identity) }
```

<p>A description of a unique event within a stream.</p>

#### `RecordList`

``` purescript
newtype RecordList
  = RecordList (Array Record'')
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The operation tried to access a nonexistent stream.</p>

#### `SequenceNumber`

``` purescript
newtype SequenceNumber
  = SequenceNumber String
```

#### `SequenceNumberRange`

``` purescript
newtype SequenceNumberRange
  = SequenceNumberRange { "StartingSequenceNumber" :: NullOrUndefined (SequenceNumber), "EndingSequenceNumber" :: NullOrUndefined (SequenceNumber) }
```

<p>The beginning and ending sequence numbers for the stream records contained within a shard.</p>

#### `Shard`

``` purescript
newtype Shard
  = Shard { "ShardId" :: NullOrUndefined (ShardId), "SequenceNumberRange" :: NullOrUndefined (SequenceNumberRange), "ParentShardId" :: NullOrUndefined (ShardId) }
```

<p>A uniquely identified group of stream records within a stream.</p>

#### `ShardDescriptionList`

``` purescript
newtype ShardDescriptionList
  = ShardDescriptionList (Array Shard)
```

#### `ShardId`

``` purescript
newtype ShardId
  = ShardId String
```

#### `ShardIterator`

``` purescript
newtype ShardIterator
  = ShardIterator String
```

#### `ShardIteratorType`

``` purescript
newtype ShardIteratorType
  = ShardIteratorType String
```

#### `Stream`

``` purescript
newtype Stream
  = Stream { "StreamArn" :: NullOrUndefined (StreamArn), "TableName" :: NullOrUndefined (TableName), "StreamLabel" :: NullOrUndefined (String) }
```

<p>Represents all of the data describing a particular stream.</p>

#### `StreamArn`

``` purescript
newtype StreamArn
  = StreamArn String
```

#### `StreamDescription`

``` purescript
newtype StreamDescription
  = StreamDescription { "StreamArn" :: NullOrUndefined (StreamArn), "StreamLabel" :: NullOrUndefined (String), "StreamStatus" :: NullOrUndefined (StreamStatus), "StreamViewType" :: NullOrUndefined (StreamViewType), "CreationRequestDateTime" :: NullOrUndefined (Date), "TableName" :: NullOrUndefined (TableName), "KeySchema" :: NullOrUndefined (KeySchema), "Shards" :: NullOrUndefined (ShardDescriptionList), "LastEvaluatedShardId" :: NullOrUndefined (ShardId) }
```

<p>Represents all of the data describing a particular stream.</p>

#### `StreamList`

``` purescript
newtype StreamList
  = StreamList (Array Stream)
```

#### `StreamRecord`

``` purescript
newtype StreamRecord
  = StreamRecord { "ApproximateCreationDateTime" :: NullOrUndefined (Date), "Keys" :: NullOrUndefined (AttributeMap), "NewImage" :: NullOrUndefined (AttributeMap), "OldImage" :: NullOrUndefined (AttributeMap), "SequenceNumber" :: NullOrUndefined (SequenceNumber), "SizeBytes" :: NullOrUndefined (PositiveLongObject), "StreamViewType" :: NullOrUndefined (StreamViewType) }
```

<p>A description of a single data modification that was performed on an item in a DynamoDB table.</p>

#### `StreamStatus`

``` purescript
newtype StreamStatus
  = StreamStatus String
```

#### `StreamViewType`

``` purescript
newtype StreamViewType
  = StreamViewType String
```

#### `StringAttributeValue`

``` purescript
newtype StringAttributeValue
  = StringAttributeValue String
```

#### `StringSetAttributeValue`

``` purescript
newtype StringSetAttributeValue
  = StringSetAttributeValue (Array StringAttributeValue)
```

#### `TableName`

``` purescript
newtype TableName
  = TableName String
```

#### `TrimmedDataAccessException`

``` purescript
newtype TrimmedDataAccessException
  = TrimmedDataAccessException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The operation attempted to read past the oldest stream record in a shard.</p> <p>In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream. You might receive a TrimmedDataAccessException if:</p> <ul> <li><p>You request a shard iterator with a sequence number older than the trim point (24 hours).</p> </li> <li><p>You obtain a shard iterator, but before you use the iterator in a <code>GetRecords</code> request, a stream record in the shard exceeds the 24 hour period and is trimmed. This causes the iterator to access a record that no longer exists.</p> </li> </ul>


