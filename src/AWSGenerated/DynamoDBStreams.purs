

-- | <fullname>Amazon DynamoDB</fullname> <p>Amazon DynamoDB Streams provides API actions for accessing streams and processing stream records. To learn more about application development with Streams, see <a href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html">Capturing Table Activity with DynamoDB Streams</a> in the Amazon DynamoDB Developer Guide.</p>
module AWS.DynamoDBStreams where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "DynamoDBStreams" :: String


-- | <p>Returns information about a stream, including the current status of the stream, its Amazon Resource Name (ARN), the composition of its shards, and its corresponding DynamoDB table.</p> <note> <p>You can call <code>DescribeStream</code> at a maximum rate of 10 times per second.</p> </note> <p>Each shard in the stream has a <code>SequenceNumberRange</code> associated with it. If the <code>SequenceNumberRange</code> has a <code>StartingSequenceNumber</code> but no <code>EndingSequenceNumber</code>, then the shard is still open (able to receive more stream records). If both <code>StartingSequenceNumber</code> and <code>EndingSequenceNumber</code> are present, then that shard is closed and can no longer receive more data.</p>
describeStream :: forall eff. DescribeStreamInput -> Aff (err :: AWS.RequestError | eff) DescribeStreamOutput
describeStream = AWS.request serviceName "DescribeStream" 


-- | <p>Retrieves the stream records from a given shard.</p> <p>Specify a shard iterator using the <code>ShardIterator</code> parameter. The shard iterator specifies the position in the shard from which you want to start reading stream records sequentially. If there are no stream records available in the portion of the shard that the iterator points to, <code>GetRecords</code> returns an empty list. Note that it might take multiple calls to get to a portion of the shard that contains stream records.</p> <note> <p> <code>GetRecords</code> can retrieve a maximum of 1 MB of data or 1000 stream records, whichever comes first.</p> </note>
getRecords :: forall eff. GetRecordsInput -> Aff (err :: AWS.RequestError | eff) GetRecordsOutput
getRecords = AWS.request serviceName "GetRecords" 


-- | <p>Returns a shard iterator. A shard iterator provides information about how to retrieve the stream records from within a shard. Use the shard iterator in a subsequent <code>GetRecords</code> request to read the stream records from the shard.</p> <note> <p>A shard iterator expires 15 minutes after it is returned to the requester.</p> </note>
getShardIterator :: forall eff. GetShardIteratorInput -> Aff (err :: AWS.RequestError | eff) GetShardIteratorOutput
getShardIterator = AWS.request serviceName "GetShardIterator" 


-- | <p>Returns an array of stream ARNs associated with the current account and endpoint. If the <code>TableName</code> parameter is present, then <code>ListStreams</code> will return only the streams ARNs for that table.</p> <note> <p>You can call <code>ListStreams</code> at a maximum rate of 5 times per second.</p> </note>
listStreams :: forall eff. ListStreamsInput -> Aff (err :: AWS.RequestError | eff) ListStreamsOutput
listStreams = AWS.request serviceName "ListStreams" 


newtype AttributeMap = AttributeMap (Map AttributeName AttributeValue)


newtype AttributeName = AttributeName String


-- | <p>Represents the data for an attribute. You can set one, and only one, of the elements.</p> <p>Each attribute in an item is a name-value pair. An attribute can be single-valued or multi-valued set. For example, a book item can have title and authors attributes. Each book has one title but can have many authors. The multi-valued attribute is a set; duplicate values are not allowed.</p>
newtype AttributeValue = AttributeValue 
  { "S" :: NullOrUndefined (StringAttributeValue)
  , "N" :: NullOrUndefined (NumberAttributeValue)
  , "B" :: NullOrUndefined (BinaryAttributeValue)
  , "SS" :: NullOrUndefined (StringSetAttributeValue)
  , "NS" :: NullOrUndefined (NumberSetAttributeValue)
  , "BS" :: NullOrUndefined (BinarySetAttributeValue)
  , "M" :: NullOrUndefined (MapAttributeValue)
  , "L" :: NullOrUndefined (ListAttributeValue)
  , "NULL" :: NullOrUndefined (NullAttributeValue)
  , "BOOL" :: NullOrUndefined (BooleanAttributeValue)
  }


newtype BinaryAttributeValue = BinaryAttributeValue String


newtype BinarySetAttributeValue = BinarySetAttributeValue (Array BinaryAttributeValue)


newtype BooleanAttributeValue = BooleanAttributeValue Boolean


newtype Date = Date Number


-- | <p>Represents the input of a <code>DescribeStream</code> operation.</p>
newtype DescribeStreamInput = DescribeStreamInput 
  { "StreamArn" :: (StreamArn)
  , "Limit" :: NullOrUndefined (PositiveIntegerObject)
  , "ExclusiveStartShardId" :: NullOrUndefined (ShardId)
  }


-- | <p>Represents the output of a <code>DescribeStream</code> operation.</p>
newtype DescribeStreamOutput = DescribeStreamOutput 
  { "StreamDescription" :: NullOrUndefined (StreamDescription)
  }


newtype ErrorMessage = ErrorMessage String


-- | <p>The shard iterator has expired and can no longer be used to retrieve stream records. A shard iterator expires 15 minutes after it is retrieved using the <code>GetShardIterator</code> action.</p>
newtype ExpiredIteratorException = ExpiredIteratorException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Represents the input of a <code>GetRecords</code> operation.</p>
newtype GetRecordsInput = GetRecordsInput 
  { "ShardIterator" :: (ShardIterator)
  , "Limit" :: NullOrUndefined (PositiveIntegerObject)
  }


-- | <p>Represents the output of a <code>GetRecords</code> operation.</p>
newtype GetRecordsOutput = GetRecordsOutput 
  { "Records" :: NullOrUndefined (RecordList)
  , "NextShardIterator" :: NullOrUndefined (ShardIterator)
  }


-- | <p>Represents the input of a <code>GetShardIterator</code> operation.</p>
newtype GetShardIteratorInput = GetShardIteratorInput 
  { "StreamArn" :: (StreamArn)
  , "ShardId" :: (ShardId)
  , "ShardIteratorType" :: (ShardIteratorType)
  , "SequenceNumber" :: NullOrUndefined (SequenceNumber)
  }


-- | <p>Represents the output of a <code>GetShardIterator</code> operation.</p>
newtype GetShardIteratorOutput = GetShardIteratorOutput 
  { "ShardIterator" :: NullOrUndefined (ShardIterator)
  }


-- | <p>Contains details about the type of identity that made the request.</p>
newtype Identity = Identity 
  { "PrincipalId" :: NullOrUndefined (String)
  , "Type" :: NullOrUndefined (String)
  }


-- | <p>An error occurred on the server side.</p>
newtype InternalServerError = InternalServerError 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype KeySchema = KeySchema (Array KeySchemaElement)


newtype KeySchemaAttributeName = KeySchemaAttributeName String


-- | <p>Represents <i>a single element</i> of a key schema. A key schema specifies the attributes that make up the primary key of a table, or the key attributes of an index.</p> <p>A <code>KeySchemaElement</code> represents exactly one attribute of the primary key. For example, a simple primary key (partition key) would be represented by one <code>KeySchemaElement</code>. A composite primary key (partition key and sort key) would require one <code>KeySchemaElement</code> for the partition key, and another <code>KeySchemaElement</code> for the sort key.</p> <note> <p>The partition key of an item is also known as its <i>hash attribute</i>. The term "hash attribute" derives from DynamoDB's usage of an internal hash function to evenly distribute data items across partitions, based on their partition key values.</p> <p>The sort key of an item is also known as its <i>range attribute</i>. The term "range attribute" derives from the way DynamoDB stores items with the same partition key physically close together, in sorted order by the sort key value.</p> </note>
newtype KeySchemaElement = KeySchemaElement 
  { "AttributeName" :: (KeySchemaAttributeName)
  , "KeyType" :: (KeyType)
  }


newtype KeyType = KeyType String


-- | <p>Your request rate is too high. The AWS SDKs for DynamoDB automatically retry requests that receive this exception. Your request is eventually successful, unless your retry queue is too large to finish. Reduce the frequency of requests and use exponential backoff. For more information, go to <a href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#APIRetries">Error Retries and Exponential Backoff</a> in the <i>Amazon DynamoDB Developer Guide</i>.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype ListAttributeValue = ListAttributeValue (Array AttributeValue)


-- | <p>Represents the input of a <code>ListStreams</code> operation.</p>
newtype ListStreamsInput = ListStreamsInput 
  { "TableName" :: NullOrUndefined (TableName)
  , "Limit" :: NullOrUndefined (PositiveIntegerObject)
  , "ExclusiveStartStreamArn" :: NullOrUndefined (StreamArn)
  }


-- | <p>Represents the output of a <code>ListStreams</code> operation.</p>
newtype ListStreamsOutput = ListStreamsOutput 
  { "Streams" :: NullOrUndefined (StreamList)
  , "LastEvaluatedStreamArn" :: NullOrUndefined (StreamArn)
  }


newtype MapAttributeValue = MapAttributeValue (Map AttributeName AttributeValue)


newtype NullAttributeValue = NullAttributeValue Boolean


newtype NumberAttributeValue = NumberAttributeValue String


newtype NumberSetAttributeValue = NumberSetAttributeValue (Array NumberAttributeValue)


newtype OperationType = OperationType String


newtype PositiveIntegerObject = PositiveIntegerObject Int


newtype PositiveLongObject = PositiveLongObject Number


-- | <p>A description of a unique event within a stream.</p>
newtype Record'' = Record'' 
  { "EventID'" :: NullOrUndefined (String)
  , "EventName'" :: NullOrUndefined (OperationType)
  , "EventVersion'" :: NullOrUndefined (String)
  , "EventSource'" :: NullOrUndefined (String)
  , "AwsRegion'" :: NullOrUndefined (String)
  , "Dynamodb'" :: NullOrUndefined (StreamRecord)
  , "UserIdentity'" :: NullOrUndefined (Identity)
  }


newtype RecordList = RecordList (Array Record'')


-- | <p>The operation tried to access a nonexistent stream.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype SequenceNumber = SequenceNumber String


-- | <p>The beginning and ending sequence numbers for the stream records contained within a shard.</p>
newtype SequenceNumberRange = SequenceNumberRange 
  { "StartingSequenceNumber" :: NullOrUndefined (SequenceNumber)
  , "EndingSequenceNumber" :: NullOrUndefined (SequenceNumber)
  }


-- | <p>A uniquely identified group of stream records within a stream.</p>
newtype Shard = Shard 
  { "ShardId" :: NullOrUndefined (ShardId)
  , "SequenceNumberRange" :: NullOrUndefined (SequenceNumberRange)
  , "ParentShardId" :: NullOrUndefined (ShardId)
  }


newtype ShardDescriptionList = ShardDescriptionList (Array Shard)


newtype ShardId = ShardId String


newtype ShardIterator = ShardIterator String


newtype ShardIteratorType = ShardIteratorType String


-- | <p>Represents all of the data describing a particular stream.</p>
newtype Stream = Stream 
  { "StreamArn" :: NullOrUndefined (StreamArn)
  , "TableName" :: NullOrUndefined (TableName)
  , "StreamLabel" :: NullOrUndefined (String)
  }


newtype StreamArn = StreamArn String


-- | <p>Represents all of the data describing a particular stream.</p>
newtype StreamDescription = StreamDescription 
  { "StreamArn" :: NullOrUndefined (StreamArn)
  , "StreamLabel" :: NullOrUndefined (String)
  , "StreamStatus" :: NullOrUndefined (StreamStatus)
  , "StreamViewType" :: NullOrUndefined (StreamViewType)
  , "CreationRequestDateTime" :: NullOrUndefined (Date)
  , "TableName" :: NullOrUndefined (TableName)
  , "KeySchema" :: NullOrUndefined (KeySchema)
  , "Shards" :: NullOrUndefined (ShardDescriptionList)
  , "LastEvaluatedShardId" :: NullOrUndefined (ShardId)
  }


newtype StreamList = StreamList (Array Stream)


-- | <p>A description of a single data modification that was performed on an item in a DynamoDB table.</p>
newtype StreamRecord = StreamRecord 
  { "ApproximateCreationDateTime" :: NullOrUndefined (Date)
  , "Keys" :: NullOrUndefined (AttributeMap)
  , "NewImage" :: NullOrUndefined (AttributeMap)
  , "OldImage" :: NullOrUndefined (AttributeMap)
  , "SequenceNumber" :: NullOrUndefined (SequenceNumber)
  , "SizeBytes" :: NullOrUndefined (PositiveLongObject)
  , "StreamViewType" :: NullOrUndefined (StreamViewType)
  }


newtype StreamStatus = StreamStatus String


newtype StreamViewType = StreamViewType String


newtype StringAttributeValue = StringAttributeValue String


newtype StringSetAttributeValue = StringSetAttributeValue (Array StringAttributeValue)


newtype TableName = TableName String


-- | <p>The operation attempted to read past the oldest stream record in a shard.</p> <p>In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream. You might receive a TrimmedDataAccessException if:</p> <ul> <li><p>You request a shard iterator with a sequence number older than the trim point (24 hours).</p> </li> <li><p>You obtain a shard iterator, but before you use the iterator in a <code>GetRecords</code> request, a stream record in the shard exceeds the 24 hour period and is trimmed. This causes the iterator to access a record that no longer exists.</p> </li> </ul>
newtype TrimmedDataAccessException = TrimmedDataAccessException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
