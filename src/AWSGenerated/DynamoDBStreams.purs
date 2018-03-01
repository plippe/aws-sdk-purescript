

-- | <fullname>Amazon DynamoDB</fullname> <p>Amazon DynamoDB Streams provides API actions for accessing streams and processing stream records. To learn more about application development with Streams, see <a href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Streams.html">Capturing Table Activity with DynamoDB Streams</a> in the Amazon DynamoDB Developer Guide.</p>
module AWS.DynamoDBStreams where

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

serviceName = "DynamoDBStreams" :: String


-- | <p>Returns information about a stream, including the current status of the stream, its Amazon Resource Name (ARN), the composition of its shards, and its corresponding DynamoDB table.</p> <note> <p>You can call <code>DescribeStream</code> at a maximum rate of 10 times per second.</p> </note> <p>Each shard in the stream has a <code>SequenceNumberRange</code> associated with it. If the <code>SequenceNumberRange</code> has a <code>StartingSequenceNumber</code> but no <code>EndingSequenceNumber</code>, then the shard is still open (able to receive more stream records). If both <code>StartingSequenceNumber</code> and <code>EndingSequenceNumber</code> are present, then that shard is closed and can no longer receive more data.</p>
describeStream :: forall eff. DescribeStreamInput -> Aff (exception :: EXCEPTION | eff) DescribeStreamOutput
describeStream = Request.request serviceName "describeStream" 


-- | <p>Retrieves the stream records from a given shard.</p> <p>Specify a shard iterator using the <code>ShardIterator</code> parameter. The shard iterator specifies the position in the shard from which you want to start reading stream records sequentially. If there are no stream records available in the portion of the shard that the iterator points to, <code>GetRecords</code> returns an empty list. Note that it might take multiple calls to get to a portion of the shard that contains stream records.</p> <note> <p> <code>GetRecords</code> can retrieve a maximum of 1 MB of data or 1000 stream records, whichever comes first.</p> </note>
getRecords :: forall eff. GetRecordsInput -> Aff (exception :: EXCEPTION | eff) GetRecordsOutput
getRecords = Request.request serviceName "getRecords" 


-- | <p>Returns a shard iterator. A shard iterator provides information about how to retrieve the stream records from within a shard. Use the shard iterator in a subsequent <code>GetRecords</code> request to read the stream records from the shard.</p> <note> <p>A shard iterator expires 15 minutes after it is returned to the requester.</p> </note>
getShardIterator :: forall eff. GetShardIteratorInput -> Aff (exception :: EXCEPTION | eff) GetShardIteratorOutput
getShardIterator = Request.request serviceName "getShardIterator" 


-- | <p>Returns an array of stream ARNs associated with the current account and endpoint. If the <code>TableName</code> parameter is present, then <code>ListStreams</code> will return only the streams ARNs for that table.</p> <note> <p>You can call <code>ListStreams</code> at a maximum rate of 5 times per second.</p> </note>
listStreams :: forall eff. ListStreamsInput -> Aff (exception :: EXCEPTION | eff) ListStreamsOutput
listStreams = Request.request serviceName "listStreams" 


newtype AttributeMap = AttributeMap (StrMap.StrMap AttributeValue)
derive instance newtypeAttributeMap :: Newtype AttributeMap _
derive instance repGenericAttributeMap :: Generic AttributeMap _
instance showAttributeMap :: Show AttributeMap where
  show = genericShow
instance decodeAttributeMap :: Decode AttributeMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeMap :: Encode AttributeMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _
derive instance repGenericAttributeName :: Generic AttributeName _
instance showAttributeName :: Show AttributeName where
  show = genericShow
instance decodeAttributeName :: Decode AttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeName :: Encode AttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the data for an attribute. You can set one, and only one, of the elements.</p> <p>Each attribute in an item is a name-value pair. An attribute can be single-valued or multi-valued set. For example, a book item can have title and authors attributes. Each book has one title but can have many authors. The multi-valued attribute is a set; duplicate values are not allowed.</p>
newtype AttributeValue = AttributeValue 
  { "S" :: NullOrUndefined.NullOrUndefined (StringAttributeValue)
  , "N" :: NullOrUndefined.NullOrUndefined (NumberAttributeValue)
  , "B" :: NullOrUndefined.NullOrUndefined (BinaryAttributeValue)
  , "SS" :: NullOrUndefined.NullOrUndefined (StringSetAttributeValue)
  , "NS" :: NullOrUndefined.NullOrUndefined (NumberSetAttributeValue)
  , "BS" :: NullOrUndefined.NullOrUndefined (BinarySetAttributeValue)
  , "NULL" :: NullOrUndefined.NullOrUndefined (NullAttributeValue)
  , "BOOL" :: NullOrUndefined.NullOrUndefined (BooleanAttributeValue)
  }
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BinaryAttributeValue = BinaryAttributeValue String
derive instance newtypeBinaryAttributeValue :: Newtype BinaryAttributeValue _
derive instance repGenericBinaryAttributeValue :: Generic BinaryAttributeValue _
instance showBinaryAttributeValue :: Show BinaryAttributeValue where
  show = genericShow
instance decodeBinaryAttributeValue :: Decode BinaryAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBinaryAttributeValue :: Encode BinaryAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BinarySetAttributeValue = BinarySetAttributeValue (Array BinaryAttributeValue)
derive instance newtypeBinarySetAttributeValue :: Newtype BinarySetAttributeValue _
derive instance repGenericBinarySetAttributeValue :: Generic BinarySetAttributeValue _
instance showBinarySetAttributeValue :: Show BinarySetAttributeValue where
  show = genericShow
instance decodeBinarySetAttributeValue :: Decode BinarySetAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBinarySetAttributeValue :: Encode BinarySetAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanAttributeValue = BooleanAttributeValue Boolean
derive instance newtypeBooleanAttributeValue :: Newtype BooleanAttributeValue _
derive instance repGenericBooleanAttributeValue :: Generic BooleanAttributeValue _
instance showBooleanAttributeValue :: Show BooleanAttributeValue where
  show = genericShow
instance decodeBooleanAttributeValue :: Decode BooleanAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanAttributeValue :: Encode BooleanAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>DescribeStream</code> operation.</p>
newtype DescribeStreamInput = DescribeStreamInput 
  { "StreamArn" :: (StreamArn)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntegerObject)
  , "ExclusiveStartShardId" :: NullOrUndefined.NullOrUndefined (ShardId)
  }
derive instance newtypeDescribeStreamInput :: Newtype DescribeStreamInput _
derive instance repGenericDescribeStreamInput :: Generic DescribeStreamInput _
instance showDescribeStreamInput :: Show DescribeStreamInput where
  show = genericShow
instance decodeDescribeStreamInput :: Decode DescribeStreamInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStreamInput :: Encode DescribeStreamInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>DescribeStream</code> operation.</p>
newtype DescribeStreamOutput = DescribeStreamOutput 
  { "StreamDescription" :: NullOrUndefined.NullOrUndefined (StreamDescription)
  }
derive instance newtypeDescribeStreamOutput :: Newtype DescribeStreamOutput _
derive instance repGenericDescribeStreamOutput :: Generic DescribeStreamOutput _
instance showDescribeStreamOutput :: Show DescribeStreamOutput where
  show = genericShow
instance decodeDescribeStreamOutput :: Decode DescribeStreamOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStreamOutput :: Encode DescribeStreamOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The shard iterator has expired and can no longer be used to retrieve stream records. A shard iterator expires 15 minutes after it is retrieved using the <code>GetShardIterator</code> action.</p>
newtype ExpiredIteratorException = ExpiredIteratorException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExpiredIteratorException :: Newtype ExpiredIteratorException _
derive instance repGenericExpiredIteratorException :: Generic ExpiredIteratorException _
instance showExpiredIteratorException :: Show ExpiredIteratorException where
  show = genericShow
instance decodeExpiredIteratorException :: Decode ExpiredIteratorException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpiredIteratorException :: Encode ExpiredIteratorException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>GetRecords</code> operation.</p>
newtype GetRecordsInput = GetRecordsInput 
  { "ShardIterator" :: (ShardIterator)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntegerObject)
  }
derive instance newtypeGetRecordsInput :: Newtype GetRecordsInput _
derive instance repGenericGetRecordsInput :: Generic GetRecordsInput _
instance showGetRecordsInput :: Show GetRecordsInput where
  show = genericShow
instance decodeGetRecordsInput :: Decode GetRecordsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRecordsInput :: Encode GetRecordsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>GetRecords</code> operation.</p>
newtype GetRecordsOutput = GetRecordsOutput 
  { "Records" :: NullOrUndefined.NullOrUndefined (RecordList)
  , "NextShardIterator" :: NullOrUndefined.NullOrUndefined (ShardIterator)
  }
derive instance newtypeGetRecordsOutput :: Newtype GetRecordsOutput _
derive instance repGenericGetRecordsOutput :: Generic GetRecordsOutput _
instance showGetRecordsOutput :: Show GetRecordsOutput where
  show = genericShow
instance decodeGetRecordsOutput :: Decode GetRecordsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetRecordsOutput :: Encode GetRecordsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>GetShardIterator</code> operation.</p>
newtype GetShardIteratorInput = GetShardIteratorInput 
  { "StreamArn" :: (StreamArn)
  , "ShardId" :: (ShardId)
  , "ShardIteratorType" :: (ShardIteratorType)
  , "SequenceNumber" :: NullOrUndefined.NullOrUndefined (SequenceNumber)
  }
derive instance newtypeGetShardIteratorInput :: Newtype GetShardIteratorInput _
derive instance repGenericGetShardIteratorInput :: Generic GetShardIteratorInput _
instance showGetShardIteratorInput :: Show GetShardIteratorInput where
  show = genericShow
instance decodeGetShardIteratorInput :: Decode GetShardIteratorInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetShardIteratorInput :: Encode GetShardIteratorInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>GetShardIterator</code> operation.</p>
newtype GetShardIteratorOutput = GetShardIteratorOutput 
  { "ShardIterator" :: NullOrUndefined.NullOrUndefined (ShardIterator)
  }
derive instance newtypeGetShardIteratorOutput :: Newtype GetShardIteratorOutput _
derive instance repGenericGetShardIteratorOutput :: Generic GetShardIteratorOutput _
instance showGetShardIteratorOutput :: Show GetShardIteratorOutput where
  show = genericShow
instance decodeGetShardIteratorOutput :: Decode GetShardIteratorOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetShardIteratorOutput :: Encode GetShardIteratorOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about the type of identity that made the request.</p>
newtype Identity = Identity 
  { "PrincipalId" :: NullOrUndefined.NullOrUndefined (String)
  , "Type" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeIdentity :: Newtype Identity _
derive instance repGenericIdentity :: Generic Identity _
instance showIdentity :: Show Identity where
  show = genericShow
instance decodeIdentity :: Decode Identity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentity :: Encode Identity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An error occurred on the server side.</p>
newtype InternalServerError = InternalServerError 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalServerError :: Newtype InternalServerError _
derive instance repGenericInternalServerError :: Generic InternalServerError _
instance showInternalServerError :: Show InternalServerError where
  show = genericShow
instance decodeInternalServerError :: Decode InternalServerError where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalServerError :: Encode InternalServerError where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeySchema = KeySchema (Array KeySchemaElement)
derive instance newtypeKeySchema :: Newtype KeySchema _
derive instance repGenericKeySchema :: Generic KeySchema _
instance showKeySchema :: Show KeySchema where
  show = genericShow
instance decodeKeySchema :: Decode KeySchema where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeySchema :: Encode KeySchema where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeySchemaAttributeName = KeySchemaAttributeName String
derive instance newtypeKeySchemaAttributeName :: Newtype KeySchemaAttributeName _
derive instance repGenericKeySchemaAttributeName :: Generic KeySchemaAttributeName _
instance showKeySchemaAttributeName :: Show KeySchemaAttributeName where
  show = genericShow
instance decodeKeySchemaAttributeName :: Decode KeySchemaAttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeySchemaAttributeName :: Encode KeySchemaAttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents <i>a single element</i> of a key schema. A key schema specifies the attributes that make up the primary key of a table, or the key attributes of an index.</p> <p>A <code>KeySchemaElement</code> represents exactly one attribute of the primary key. For example, a simple primary key (partition key) would be represented by one <code>KeySchemaElement</code>. A composite primary key (partition key and sort key) would require one <code>KeySchemaElement</code> for the partition key, and another <code>KeySchemaElement</code> for the sort key.</p> <note> <p>The partition key of an item is also known as its <i>hash attribute</i>. The term "hash attribute" derives from DynamoDB's usage of an internal hash function to evenly distribute data items across partitions, based on their partition key values.</p> <p>The sort key of an item is also known as its <i>range attribute</i>. The term "range attribute" derives from the way DynamoDB stores items with the same partition key physically close together, in sorted order by the sort key value.</p> </note>
newtype KeySchemaElement = KeySchemaElement 
  { "AttributeName" :: (KeySchemaAttributeName)
  , "KeyType" :: (KeyType)
  }
derive instance newtypeKeySchemaElement :: Newtype KeySchemaElement _
derive instance repGenericKeySchemaElement :: Generic KeySchemaElement _
instance showKeySchemaElement :: Show KeySchemaElement where
  show = genericShow
instance decodeKeySchemaElement :: Decode KeySchemaElement where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeySchemaElement :: Encode KeySchemaElement where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyType = KeyType String
derive instance newtypeKeyType :: Newtype KeyType _
derive instance repGenericKeyType :: Generic KeyType _
instance showKeyType :: Show KeyType where
  show = genericShow
instance decodeKeyType :: Decode KeyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyType :: Encode KeyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your request rate is too high. The AWS SDKs for DynamoDB automatically retry requests that receive this exception. Your request is eventually successful, unless your retry queue is too large to finish. Reduce the frequency of requests and use exponential backoff. For more information, go to <a href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/ErrorHandling.html#APIRetries">Error Retries and Exponential Backoff</a> in the <i>Amazon DynamoDB Developer Guide</i>.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListAttributeValue = ListAttributeValue (Array AttributeValue)
derive instance newtypeListAttributeValue :: Newtype ListAttributeValue _
derive instance repGenericListAttributeValue :: Generic ListAttributeValue _
instance showListAttributeValue :: Show ListAttributeValue where
  show = genericShow
instance decodeListAttributeValue :: Decode ListAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListAttributeValue :: Encode ListAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a <code>ListStreams</code> operation.</p>
newtype ListStreamsInput = ListStreamsInput 
  { "TableName" :: NullOrUndefined.NullOrUndefined (TableName)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntegerObject)
  , "ExclusiveStartStreamArn" :: NullOrUndefined.NullOrUndefined (StreamArn)
  }
derive instance newtypeListStreamsInput :: Newtype ListStreamsInput _
derive instance repGenericListStreamsInput :: Generic ListStreamsInput _
instance showListStreamsInput :: Show ListStreamsInput where
  show = genericShow
instance decodeListStreamsInput :: Decode ListStreamsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStreamsInput :: Encode ListStreamsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a <code>ListStreams</code> operation.</p>
newtype ListStreamsOutput = ListStreamsOutput 
  { "Streams" :: NullOrUndefined.NullOrUndefined (StreamList)
  , "LastEvaluatedStreamArn" :: NullOrUndefined.NullOrUndefined (StreamArn)
  }
derive instance newtypeListStreamsOutput :: Newtype ListStreamsOutput _
derive instance repGenericListStreamsOutput :: Generic ListStreamsOutput _
instance showListStreamsOutput :: Show ListStreamsOutput where
  show = genericShow
instance decodeListStreamsOutput :: Decode ListStreamsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStreamsOutput :: Encode ListStreamsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MapAttributeValue = MapAttributeValue (StrMap.StrMap AttributeValue)
derive instance newtypeMapAttributeValue :: Newtype MapAttributeValue _
derive instance repGenericMapAttributeValue :: Generic MapAttributeValue _
instance showMapAttributeValue :: Show MapAttributeValue where
  show = genericShow
instance decodeMapAttributeValue :: Decode MapAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMapAttributeValue :: Encode MapAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NullAttributeValue = NullAttributeValue Boolean
derive instance newtypeNullAttributeValue :: Newtype NullAttributeValue _
derive instance repGenericNullAttributeValue :: Generic NullAttributeValue _
instance showNullAttributeValue :: Show NullAttributeValue where
  show = genericShow
instance decodeNullAttributeValue :: Decode NullAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNullAttributeValue :: Encode NullAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NumberAttributeValue = NumberAttributeValue String
derive instance newtypeNumberAttributeValue :: Newtype NumberAttributeValue _
derive instance repGenericNumberAttributeValue :: Generic NumberAttributeValue _
instance showNumberAttributeValue :: Show NumberAttributeValue where
  show = genericShow
instance decodeNumberAttributeValue :: Decode NumberAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumberAttributeValue :: Encode NumberAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NumberSetAttributeValue = NumberSetAttributeValue (Array NumberAttributeValue)
derive instance newtypeNumberSetAttributeValue :: Newtype NumberSetAttributeValue _
derive instance repGenericNumberSetAttributeValue :: Generic NumberSetAttributeValue _
instance showNumberSetAttributeValue :: Show NumberSetAttributeValue where
  show = genericShow
instance decodeNumberSetAttributeValue :: Decode NumberSetAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNumberSetAttributeValue :: Encode NumberSetAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OperationType = OperationType String
derive instance newtypeOperationType :: Newtype OperationType _
derive instance repGenericOperationType :: Generic OperationType _
instance showOperationType :: Show OperationType where
  show = genericShow
instance decodeOperationType :: Decode OperationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationType :: Encode OperationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PositiveIntegerObject = PositiveIntegerObject Int
derive instance newtypePositiveIntegerObject :: Newtype PositiveIntegerObject _
derive instance repGenericPositiveIntegerObject :: Generic PositiveIntegerObject _
instance showPositiveIntegerObject :: Show PositiveIntegerObject where
  show = genericShow
instance decodePositiveIntegerObject :: Decode PositiveIntegerObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePositiveIntegerObject :: Encode PositiveIntegerObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PositiveLongObject = PositiveLongObject Number
derive instance newtypePositiveLongObject :: Newtype PositiveLongObject _
derive instance repGenericPositiveLongObject :: Generic PositiveLongObject _
instance showPositiveLongObject :: Show PositiveLongObject where
  show = genericShow
instance decodePositiveLongObject :: Decode PositiveLongObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePositiveLongObject :: Encode PositiveLongObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A description of a unique event within a stream.</p>
newtype Record'' = Record'' 
  { "EventID'" :: NullOrUndefined.NullOrUndefined (String)
  , "EventName'" :: NullOrUndefined.NullOrUndefined (OperationType)
  , "EventVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "EventSource'" :: NullOrUndefined.NullOrUndefined (String)
  , "AwsRegion'" :: NullOrUndefined.NullOrUndefined (String)
  , "Dynamodb'" :: NullOrUndefined.NullOrUndefined (StreamRecord)
  , "UserIdentity'" :: NullOrUndefined.NullOrUndefined (Identity)
  }
derive instance newtypeRecord'' :: Newtype Record'' _
derive instance repGenericRecord'' :: Generic Record'' _
instance showRecord'' :: Show Record'' where
  show = genericShow
instance decodeRecord'' :: Decode Record'' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecord'' :: Encode Record'' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RecordList = RecordList (Array Record'')
derive instance newtypeRecordList :: Newtype RecordList _
derive instance repGenericRecordList :: Generic RecordList _
instance showRecordList :: Show RecordList where
  show = genericShow
instance decodeRecordList :: Decode RecordList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecordList :: Encode RecordList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation tried to access a nonexistent stream.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SequenceNumber = SequenceNumber String
derive instance newtypeSequenceNumber :: Newtype SequenceNumber _
derive instance repGenericSequenceNumber :: Generic SequenceNumber _
instance showSequenceNumber :: Show SequenceNumber where
  show = genericShow
instance decodeSequenceNumber :: Decode SequenceNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSequenceNumber :: Encode SequenceNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The beginning and ending sequence numbers for the stream records contained within a shard.</p>
newtype SequenceNumberRange = SequenceNumberRange 
  { "StartingSequenceNumber" :: NullOrUndefined.NullOrUndefined (SequenceNumber)
  , "EndingSequenceNumber" :: NullOrUndefined.NullOrUndefined (SequenceNumber)
  }
derive instance newtypeSequenceNumberRange :: Newtype SequenceNumberRange _
derive instance repGenericSequenceNumberRange :: Generic SequenceNumberRange _
instance showSequenceNumberRange :: Show SequenceNumberRange where
  show = genericShow
instance decodeSequenceNumberRange :: Decode SequenceNumberRange where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSequenceNumberRange :: Encode SequenceNumberRange where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A uniquely identified group of stream records within a stream.</p>
newtype Shard = Shard 
  { "ShardId" :: NullOrUndefined.NullOrUndefined (ShardId)
  , "SequenceNumberRange" :: NullOrUndefined.NullOrUndefined (SequenceNumberRange)
  , "ParentShardId" :: NullOrUndefined.NullOrUndefined (ShardId)
  }
derive instance newtypeShard :: Newtype Shard _
derive instance repGenericShard :: Generic Shard _
instance showShard :: Show Shard where
  show = genericShow
instance decodeShard :: Decode Shard where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShard :: Encode Shard where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ShardDescriptionList = ShardDescriptionList (Array Shard)
derive instance newtypeShardDescriptionList :: Newtype ShardDescriptionList _
derive instance repGenericShardDescriptionList :: Generic ShardDescriptionList _
instance showShardDescriptionList :: Show ShardDescriptionList where
  show = genericShow
instance decodeShardDescriptionList :: Decode ShardDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShardDescriptionList :: Encode ShardDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ShardId = ShardId String
derive instance newtypeShardId :: Newtype ShardId _
derive instance repGenericShardId :: Generic ShardId _
instance showShardId :: Show ShardId where
  show = genericShow
instance decodeShardId :: Decode ShardId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShardId :: Encode ShardId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ShardIterator = ShardIterator String
derive instance newtypeShardIterator :: Newtype ShardIterator _
derive instance repGenericShardIterator :: Generic ShardIterator _
instance showShardIterator :: Show ShardIterator where
  show = genericShow
instance decodeShardIterator :: Decode ShardIterator where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShardIterator :: Encode ShardIterator where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ShardIteratorType = ShardIteratorType String
derive instance newtypeShardIteratorType :: Newtype ShardIteratorType _
derive instance repGenericShardIteratorType :: Generic ShardIteratorType _
instance showShardIteratorType :: Show ShardIteratorType where
  show = genericShow
instance decodeShardIteratorType :: Decode ShardIteratorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShardIteratorType :: Encode ShardIteratorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents all of the data describing a particular stream.</p>
newtype Stream = Stream 
  { "StreamArn" :: NullOrUndefined.NullOrUndefined (StreamArn)
  , "TableName" :: NullOrUndefined.NullOrUndefined (TableName)
  , "StreamLabel" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeStream :: Newtype Stream _
derive instance repGenericStream :: Generic Stream _
instance showStream :: Show Stream where
  show = genericShow
instance decodeStream :: Decode Stream where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStream :: Encode Stream where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamArn = StreamArn String
derive instance newtypeStreamArn :: Newtype StreamArn _
derive instance repGenericStreamArn :: Generic StreamArn _
instance showStreamArn :: Show StreamArn where
  show = genericShow
instance decodeStreamArn :: Decode StreamArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamArn :: Encode StreamArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents all of the data describing a particular stream.</p>
newtype StreamDescription = StreamDescription 
  { "StreamArn" :: NullOrUndefined.NullOrUndefined (StreamArn)
  , "StreamLabel" :: NullOrUndefined.NullOrUndefined (String)
  , "StreamStatus" :: NullOrUndefined.NullOrUndefined (StreamStatus)
  , "StreamViewType" :: NullOrUndefined.NullOrUndefined (StreamViewType)
  , "CreationRequestDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "TableName" :: NullOrUndefined.NullOrUndefined (TableName)
  , "KeySchema" :: NullOrUndefined.NullOrUndefined (KeySchema)
  , "Shards" :: NullOrUndefined.NullOrUndefined (ShardDescriptionList)
  , "LastEvaluatedShardId" :: NullOrUndefined.NullOrUndefined (ShardId)
  }
derive instance newtypeStreamDescription :: Newtype StreamDescription _
derive instance repGenericStreamDescription :: Generic StreamDescription _
instance showStreamDescription :: Show StreamDescription where
  show = genericShow
instance decodeStreamDescription :: Decode StreamDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamDescription :: Encode StreamDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamList = StreamList (Array Stream)
derive instance newtypeStreamList :: Newtype StreamList _
derive instance repGenericStreamList :: Generic StreamList _
instance showStreamList :: Show StreamList where
  show = genericShow
instance decodeStreamList :: Decode StreamList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamList :: Encode StreamList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A description of a single data modification that was performed on an item in a DynamoDB table.</p>
newtype StreamRecord = StreamRecord 
  { "ApproximateCreationDateTime" :: NullOrUndefined.NullOrUndefined (Date)
  , "Keys" :: NullOrUndefined.NullOrUndefined (AttributeMap)
  , "NewImage" :: NullOrUndefined.NullOrUndefined (AttributeMap)
  , "OldImage" :: NullOrUndefined.NullOrUndefined (AttributeMap)
  , "SequenceNumber" :: NullOrUndefined.NullOrUndefined (SequenceNumber)
  , "SizeBytes" :: NullOrUndefined.NullOrUndefined (PositiveLongObject)
  , "StreamViewType" :: NullOrUndefined.NullOrUndefined (StreamViewType)
  }
derive instance newtypeStreamRecord :: Newtype StreamRecord _
derive instance repGenericStreamRecord :: Generic StreamRecord _
instance showStreamRecord :: Show StreamRecord where
  show = genericShow
instance decodeStreamRecord :: Decode StreamRecord where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamRecord :: Encode StreamRecord where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamStatus = StreamStatus String
derive instance newtypeStreamStatus :: Newtype StreamStatus _
derive instance repGenericStreamStatus :: Generic StreamStatus _
instance showStreamStatus :: Show StreamStatus where
  show = genericShow
instance decodeStreamStatus :: Decode StreamStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamStatus :: Encode StreamStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StreamViewType = StreamViewType String
derive instance newtypeStreamViewType :: Newtype StreamViewType _
derive instance repGenericStreamViewType :: Generic StreamViewType _
instance showStreamViewType :: Show StreamViewType where
  show = genericShow
instance decodeStreamViewType :: Decode StreamViewType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStreamViewType :: Encode StreamViewType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringAttributeValue = StringAttributeValue String
derive instance newtypeStringAttributeValue :: Newtype StringAttributeValue _
derive instance repGenericStringAttributeValue :: Generic StringAttributeValue _
instance showStringAttributeValue :: Show StringAttributeValue where
  show = genericShow
instance decodeStringAttributeValue :: Decode StringAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringAttributeValue :: Encode StringAttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StringSetAttributeValue = StringSetAttributeValue (Array StringAttributeValue)
derive instance newtypeStringSetAttributeValue :: Newtype StringSetAttributeValue _
derive instance repGenericStringSetAttributeValue :: Generic StringSetAttributeValue _
instance showStringSetAttributeValue :: Show StringSetAttributeValue where
  show = genericShow
instance decodeStringSetAttributeValue :: Decode StringSetAttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStringSetAttributeValue :: Encode StringSetAttributeValue where
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


-- | <p>The operation attempted to read past the oldest stream record in a shard.</p> <p>In DynamoDB Streams, there is a 24 hour limit on data retention. Stream records whose age exceeds this limit are subject to removal (trimming) from the stream. You might receive a TrimmedDataAccessException if:</p> <ul> <li><p>You request a shard iterator with a sequence number older than the trim point (24 hours).</p> </li> <li><p>You obtain a shard iterator, but before you use the iterator in a <code>GetRecords</code> request, a stream record in the shard exceeds the 24 hour period and is trimmed. This causes the iterator to access a record that no longer exists.</p> </li> </ul>
newtype TrimmedDataAccessException = TrimmedDataAccessException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTrimmedDataAccessException :: Newtype TrimmedDataAccessException _
derive instance repGenericTrimmedDataAccessException :: Generic TrimmedDataAccessException _
instance showTrimmedDataAccessException :: Show TrimmedDataAccessException where
  show = genericShow
instance decodeTrimmedDataAccessException :: Decode TrimmedDataAccessException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrimmedDataAccessException :: Encode TrimmedDataAccessException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
