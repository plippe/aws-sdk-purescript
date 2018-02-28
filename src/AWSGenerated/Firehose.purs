

-- | <fullname>Amazon Kinesis Firehose API Reference</fullname> <p>Amazon Kinesis Firehose is a fully managed service that delivers real-time streaming data to destinations such as Amazon Simple Storage Service (Amazon S3), Amazon Elasticsearch Service (Amazon ES), and Amazon Redshift.</p>
module AWS.Firehose where

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

serviceName = "Firehose" :: String


-- | <p>Creates a delivery stream.</p> <p>By default, you can create up to 20 delivery streams per region.</p> <p>This is an asynchronous operation that immediately returns. The initial status of the delivery stream is <code>CREATING</code>. After the delivery stream is created, its status is <code>ACTIVE</code> and it now accepts data. Attempts to send data to a delivery stream that is not in the <code>ACTIVE</code> state cause an exception. To check the state of a delivery stream, use <a>DescribeDeliveryStream</a>.</p> <p>A Kinesis Firehose delivery stream can be configured to receive records directly from providers using <a>PutRecord</a> or <a>PutRecordBatch</a>, or it can be configured to use an existing Kinesis stream as its source. To specify a Kinesis stream as input, set the <code>DeliveryStreamType</code> parameter to <code>KinesisStreamAsSource</code>, and provide the Kinesis stream ARN and role ARN in the <code>KinesisStreamSourceConfiguration</code> parameter.</p> <p>A delivery stream is configured with a single destination: Amazon S3, Amazon ES, or Amazon Redshift. You must specify only one of the following destination configuration parameters: <b>ExtendedS3DestinationConfiguration</b>, <b>S3DestinationConfiguration</b>, <b>ElasticsearchDestinationConfiguration</b>, or <b>RedshiftDestinationConfiguration</b>.</p> <p>When you specify <b>S3DestinationConfiguration</b>, you can also provide the following optional values: <b>BufferingHints</b>, <b>EncryptionConfiguration</b>, and <b>CompressionFormat</b>. By default, if no <b>BufferingHints</b> value is provided, Kinesis Firehose buffers data up to 5 MB or for 5 minutes, whichever condition is satisfied first. Note that <b>BufferingHints</b> is a hint, so there are some cases where the service cannot adhere to these conditions strictly; for example, record boundaries are such that the size is a little over or under the configured buffering size. By default, no encryption is performed. We strongly recommend that you enable encryption to ensure secure data storage in Amazon S3.</p> <p>A few notes about Amazon Redshift as a destination:</p> <ul> <li> <p>An Amazon Redshift destination requires an S3 bucket as intermediate location, as Kinesis Firehose first delivers data to S3 and then uses <code>COPY</code> syntax to load data into an Amazon Redshift table. This is specified in the <b>RedshiftDestinationConfiguration.S3Configuration</b> parameter.</p> </li> <li> <p>The compression formats <code>SNAPPY</code> or <code>ZIP</code> cannot be specified in <b>RedshiftDestinationConfiguration.S3Configuration</b> because the Amazon Redshift <code>COPY</code> operation that reads from the S3 bucket doesn't support these compression formats.</p> </li> <li> <p>We strongly recommend that you use the user name and password you provide exclusively with Kinesis Firehose, and that the permissions for the account are restricted for Amazon Redshift <code>INSERT</code> permissions.</p> </li> </ul> <p>Kinesis Firehose assumes the IAM role that is configured as part of the destination. The role should allow the Kinesis Firehose principal to assume the role, and the role should have permissions that allow the service to deliver the data. For more information, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3">Amazon S3 Bucket Access</a> in the <i>Amazon Kinesis Firehose Developer Guide</i>.</p>
createDeliveryStream :: forall eff. CreateDeliveryStreamInput -> Aff (exception :: EXCEPTION | eff) CreateDeliveryStreamOutput
createDeliveryStream = Request.request serviceName "createDeliveryStream" 


-- | <p>Deletes a delivery stream and its data.</p> <p>You can delete a delivery stream only if it is in <code>ACTIVE</code> or <code>DELETING</code> state, and not in the <code>CREATING</code> state. While the deletion request is in process, the delivery stream is in the <code>DELETING</code> state.</p> <p>To check the state of a delivery stream, use <a>DescribeDeliveryStream</a>.</p> <p>While the delivery stream is <code>DELETING</code> state, the service may continue to accept the records, but the service doesn't make any guarantees with respect to delivering the data. Therefore, as a best practice, you should first stop any applications that are sending records before deleting a delivery stream.</p>
deleteDeliveryStream :: forall eff. DeleteDeliveryStreamInput -> Aff (exception :: EXCEPTION | eff) DeleteDeliveryStreamOutput
deleteDeliveryStream = Request.request serviceName "deleteDeliveryStream" 


-- | <p>Describes the specified delivery stream and gets the status. For example, after your delivery stream is created, call <a>DescribeDeliveryStream</a> to see if the delivery stream is <code>ACTIVE</code> and therefore ready for data to be sent to it.</p>
describeDeliveryStream :: forall eff. DescribeDeliveryStreamInput -> Aff (exception :: EXCEPTION | eff) DescribeDeliveryStreamOutput
describeDeliveryStream = Request.request serviceName "describeDeliveryStream" 


-- | <p>Lists your delivery streams.</p> <p>The number of delivery streams might be too large to return using a single call to <a>ListDeliveryStreams</a>. You can limit the number of delivery streams returned, using the <b>Limit</b> parameter. To determine whether there are more delivery streams to list, check the value of <b>HasMoreDeliveryStreams</b> in the output. If there are more delivery streams to list, you can request them by specifying the name of the last delivery stream returned in the call in the <b>ExclusiveStartDeliveryStreamName</b> parameter of a subsequent call.</p>
listDeliveryStreams :: forall eff. ListDeliveryStreamsInput -> Aff (exception :: EXCEPTION | eff) ListDeliveryStreamsOutput
listDeliveryStreams = Request.request serviceName "listDeliveryStreams" 


-- | <p>Writes a single data record into an Amazon Kinesis Firehose delivery stream. To write multiple data records into a delivery stream, use <a>PutRecordBatch</a>. Applications using these operations are referred to as producers.</p> <p>By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. Note that if you use <a>PutRecord</a> and <a>PutRecordBatch</a>, the limits are an aggregate across these two operations for each delivery stream. For more information about limits and how to request an increase, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>. </p> <p>You must specify the name of the delivery stream and the data record when using <a>PutRecord</a>. The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data, for example, a segment from a log file, geographic location data, website clickstream data, and so on.</p> <p>Kinesis Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (<code>\n</code>) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.</p> <p>The <a>PutRecord</a> operation returns a <b>RecordId</b>, which is a unique string assigned to each record. Producer applications can use this ID for purposes such as auditability and investigation.</p> <p>If the <a>PutRecord</a> operation throws a <b>ServiceUnavailableException</b>, back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream. </p> <p>Data records sent to Kinesis Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.</p>
putRecord :: forall eff. PutRecordInput -> Aff (exception :: EXCEPTION | eff) PutRecordOutput
putRecord = Request.request serviceName "putRecord" 


-- | <p>Writes multiple data records into a delivery stream in a single call, which can achieve higher throughput per producer than when writing single records. To write single data records into a delivery stream, use <a>PutRecord</a>. Applications using these operations are referred to as producers.</p> <p>By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. If you use <a>PutRecord</a> and <a>PutRecordBatch</a>, the limits are an aggregate across these two operations for each delivery stream. For more information about limits, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>.</p> <p>Each <a>PutRecordBatch</a> request supports up to 500 records. Each record in the request can be as large as 1,000 KB (before 64-bit encoding), up to a limit of 4 MB for the entire request. These limits cannot be changed.</p> <p>You must specify the name of the delivery stream and the data record when using <a>PutRecord</a>. The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data. For example, it could be a segment from a log file, geographic location data, web site clickstream data, and so on.</p> <p>Kinesis Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (<code>\n</code>) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.</p> <p>The <a>PutRecordBatch</a> response includes a count of failed records, <b>FailedPutCount</b>, and an array of responses, <b>RequestResponses</b>. Each entry in the <b>RequestResponses</b> array provides additional information about the processed record. It directly correlates with a record in the request array using the same ordering, from the top to the bottom. The response array always includes the same number of records as the request array. <b>RequestResponses</b> includes both successfully and unsuccessfully processed records. Kinesis Firehose attempts to process all records in each <a>PutRecordBatch</a> request. A single record failure does not stop the processing of subsequent records.</p> <p>A successfully processed record includes a <b>RecordId</b> value, which is unique for the record. An unsuccessfully processed record includes <b>ErrorCode</b> and <b>ErrorMessage</b> values. <b>ErrorCode</b> reflects the type of error, and is one of the following values: <code>ServiceUnavailable</code> or <code>InternalFailure</code>. <b>ErrorMessage</b> provides more detailed information about the error.</p> <p>If there is an internal server error or a timeout, the write might have completed or it might have failed. If <b>FailedPutCount</b> is greater than 0, retry the request, resending only those records that might have failed processing. This minimizes the possible duplicate records and also reduces the total bytes sent (and corresponding charges). We recommend that you handle any duplicates at the destination.</p> <p>If <a>PutRecordBatch</a> throws <b>ServiceUnavailableException</b>, back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream.</p> <p>Data records sent to Kinesis Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.</p>
putRecordBatch :: forall eff. PutRecordBatchInput -> Aff (exception :: EXCEPTION | eff) PutRecordBatchOutput
putRecordBatch = Request.request serviceName "putRecordBatch" 


-- | <p>Updates the specified destination of the specified delivery stream.</p> <p>You can use this operation to change the destination type (for example, to replace the Amazon S3 destination with Amazon Redshift) or change the parameters associated with a destination (for example, to change the bucket name of the Amazon S3 destination). The update might not occur immediately. The target delivery stream remains active while the configurations are updated, so data writes to the delivery stream can continue during this process. The updated configurations are usually effective within a few minutes.</p> <p>Note that switching between Amazon ES and other services is not supported. For an Amazon ES destination, you can only update to another Amazon ES destination.</p> <p>If the destination type is the same, Kinesis Firehose merges the configuration parameters specified with the destination configuration that already exists on the delivery stream. If any of the parameters are not specified in the call, the existing values are retained. For example, in the Amazon S3 destination, if <a>EncryptionConfiguration</a> is not specified, then the existing <a>EncryptionConfiguration</a> is maintained on the destination.</p> <p>If the destination type is not the same, for example, changing the destination from Amazon S3 to Amazon Redshift, Kinesis Firehose does not merge any parameters. In this case, all parameters must be specified.</p> <p>Kinesis Firehose uses <b>CurrentDeliveryStreamVersionId</b> to avoid race conditions and conflicting merges. This is a required field, and the service updates the configuration only if the existing configuration has a version ID that matches. After the update is applied successfully, the version ID is updated, and can be retrieved using <a>DescribeDeliveryStream</a>. Use the new version ID to set <b>CurrentDeliveryStreamVersionId</b> in the next call.</p>
updateDestination :: forall eff. UpdateDestinationInput -> Aff (exception :: EXCEPTION | eff) UpdateDestinationOutput
updateDestination = Request.request serviceName "updateDestination" 


newtype AWSKMSKeyARN = AWSKMSKeyARN String
derive instance newtypeAWSKMSKeyARN :: Newtype AWSKMSKeyARN _
derive instance repGenericAWSKMSKeyARN :: Generic AWSKMSKeyARN _
instance showAWSKMSKeyARN :: Show AWSKMSKeyARN where
  show = genericShow
instance decodeAWSKMSKeyARN :: Decode AWSKMSKeyARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAWSKMSKeyARN :: Encode AWSKMSKeyARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _
derive instance repGenericBooleanObject :: Generic BooleanObject _
instance showBooleanObject :: Show BooleanObject where
  show = genericShow
instance decodeBooleanObject :: Decode BooleanObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanObject :: Encode BooleanObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BucketARN = BucketARN String
derive instance newtypeBucketARN :: Newtype BucketARN _
derive instance repGenericBucketARN :: Generic BucketARN _
instance showBucketARN :: Show BucketARN where
  show = genericShow
instance decodeBucketARN :: Decode BucketARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBucketARN :: Encode BucketARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes hints for the buffering to perform before delivering data to the destination. Please note that these options are treated as hints, and therefore Kinesis Firehose may choose to use different values when it is optimal.</p>
newtype BufferingHints = BufferingHints 
  { "SizeInMBs" :: NullOrUndefined.NullOrUndefined (SizeInMBs)
  , "IntervalInSeconds" :: NullOrUndefined.NullOrUndefined (IntervalInSeconds)
  }
derive instance newtypeBufferingHints :: Newtype BufferingHints _
derive instance repGenericBufferingHints :: Generic BufferingHints _
instance showBufferingHints :: Show BufferingHints where
  show = genericShow
instance decodeBufferingHints :: Decode BufferingHints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBufferingHints :: Encode BufferingHints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the Amazon CloudWatch logging options for your delivery stream.</p>
newtype CloudWatchLoggingOptions = CloudWatchLoggingOptions 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "LogGroupName" :: NullOrUndefined.NullOrUndefined (LogGroupName)
  , "LogStreamName" :: NullOrUndefined.NullOrUndefined (LogStreamName)
  }
derive instance newtypeCloudWatchLoggingOptions :: Newtype CloudWatchLoggingOptions _
derive instance repGenericCloudWatchLoggingOptions :: Generic CloudWatchLoggingOptions _
instance showCloudWatchLoggingOptions :: Show CloudWatchLoggingOptions where
  show = genericShow
instance decodeCloudWatchLoggingOptions :: Decode CloudWatchLoggingOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudWatchLoggingOptions :: Encode CloudWatchLoggingOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClusterJDBCURL = ClusterJDBCURL String
derive instance newtypeClusterJDBCURL :: Newtype ClusterJDBCURL _
derive instance repGenericClusterJDBCURL :: Generic ClusterJDBCURL _
instance showClusterJDBCURL :: Show ClusterJDBCURL where
  show = genericShow
instance decodeClusterJDBCURL :: Decode ClusterJDBCURL where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClusterJDBCURL :: Encode ClusterJDBCURL where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CompressionFormat = CompressionFormat String
derive instance newtypeCompressionFormat :: Newtype CompressionFormat _
derive instance repGenericCompressionFormat :: Generic CompressionFormat _
instance showCompressionFormat :: Show CompressionFormat where
  show = genericShow
instance decodeCompressionFormat :: Decode CompressionFormat where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompressionFormat :: Encode CompressionFormat where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Another modification has already happened. Fetch <b>VersionId</b> again and use it to update the destination.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a <code>COPY</code> command for Amazon Redshift.</p>
newtype CopyCommand = CopyCommand 
  { "DataTableName" :: (DataTableName)
  , "DataTableColumns" :: NullOrUndefined.NullOrUndefined (DataTableColumns)
  , "CopyOptions" :: NullOrUndefined.NullOrUndefined (CopyOptions)
  }
derive instance newtypeCopyCommand :: Newtype CopyCommand _
derive instance repGenericCopyCommand :: Generic CopyCommand _
instance showCopyCommand :: Show CopyCommand where
  show = genericShow
instance decodeCopyCommand :: Decode CopyCommand where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyCommand :: Encode CopyCommand where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CopyOptions = CopyOptions String
derive instance newtypeCopyOptions :: Newtype CopyOptions _
derive instance repGenericCopyOptions :: Generic CopyOptions _
instance showCopyOptions :: Show CopyOptions where
  show = genericShow
instance decodeCopyOptions :: Decode CopyOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCopyOptions :: Encode CopyOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDeliveryStreamInput = CreateDeliveryStreamInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "DeliveryStreamType" :: NullOrUndefined.NullOrUndefined (DeliveryStreamType)
  , "KinesisStreamSourceConfiguration" :: NullOrUndefined.NullOrUndefined (KinesisStreamSourceConfiguration)
  , "S3DestinationConfiguration" :: NullOrUndefined.NullOrUndefined (S3DestinationConfiguration)
  , "ExtendedS3DestinationConfiguration" :: NullOrUndefined.NullOrUndefined (ExtendedS3DestinationConfiguration)
  , "RedshiftDestinationConfiguration" :: NullOrUndefined.NullOrUndefined (RedshiftDestinationConfiguration)
  , "ElasticsearchDestinationConfiguration" :: NullOrUndefined.NullOrUndefined (ElasticsearchDestinationConfiguration)
  , "SplunkDestinationConfiguration" :: NullOrUndefined.NullOrUndefined (SplunkDestinationConfiguration)
  }
derive instance newtypeCreateDeliveryStreamInput :: Newtype CreateDeliveryStreamInput _
derive instance repGenericCreateDeliveryStreamInput :: Generic CreateDeliveryStreamInput _
instance showCreateDeliveryStreamInput :: Show CreateDeliveryStreamInput where
  show = genericShow
instance decodeCreateDeliveryStreamInput :: Decode CreateDeliveryStreamInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeliveryStreamInput :: Encode CreateDeliveryStreamInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateDeliveryStreamOutput = CreateDeliveryStreamOutput 
  { "DeliveryStreamARN" :: NullOrUndefined.NullOrUndefined (DeliveryStreamARN)
  }
derive instance newtypeCreateDeliveryStreamOutput :: Newtype CreateDeliveryStreamOutput _
derive instance repGenericCreateDeliveryStreamOutput :: Generic CreateDeliveryStreamOutput _
instance showCreateDeliveryStreamOutput :: Show CreateDeliveryStreamOutput where
  show = genericShow
instance decodeCreateDeliveryStreamOutput :: Decode CreateDeliveryStreamOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDeliveryStreamOutput :: Encode CreateDeliveryStreamOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Data = Data String
derive instance newtypeData :: Newtype Data _
derive instance repGenericData :: Generic Data _
instance showData :: Show Data where
  show = genericShow
instance decodeData :: Decode Data where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeData :: Encode Data where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataTableColumns = DataTableColumns String
derive instance newtypeDataTableColumns :: Newtype DataTableColumns _
derive instance repGenericDataTableColumns :: Generic DataTableColumns _
instance showDataTableColumns :: Show DataTableColumns where
  show = genericShow
instance decodeDataTableColumns :: Decode DataTableColumns where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataTableColumns :: Encode DataTableColumns where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DataTableName = DataTableName String
derive instance newtypeDataTableName :: Newtype DataTableName _
derive instance repGenericDataTableName :: Generic DataTableName _
instance showDataTableName :: Show DataTableName where
  show = genericShow
instance decodeDataTableName :: Decode DataTableName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDataTableName :: Encode DataTableName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDeliveryStreamInput = DeleteDeliveryStreamInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  }
derive instance newtypeDeleteDeliveryStreamInput :: Newtype DeleteDeliveryStreamInput _
derive instance repGenericDeleteDeliveryStreamInput :: Generic DeleteDeliveryStreamInput _
instance showDeleteDeliveryStreamInput :: Show DeleteDeliveryStreamInput where
  show = genericShow
instance decodeDeleteDeliveryStreamInput :: Decode DeleteDeliveryStreamInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDeliveryStreamInput :: Encode DeleteDeliveryStreamInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDeliveryStreamOutput = DeleteDeliveryStreamOutput Types.NoArguments
derive instance newtypeDeleteDeliveryStreamOutput :: Newtype DeleteDeliveryStreamOutput _
derive instance repGenericDeleteDeliveryStreamOutput :: Generic DeleteDeliveryStreamOutput _
instance showDeleteDeliveryStreamOutput :: Show DeleteDeliveryStreamOutput where
  show = genericShow
instance decodeDeleteDeliveryStreamOutput :: Decode DeleteDeliveryStreamOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDeliveryStreamOutput :: Encode DeleteDeliveryStreamOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStartTimestamp = DeliveryStartTimestamp Number
derive instance newtypeDeliveryStartTimestamp :: Newtype DeliveryStartTimestamp _
derive instance repGenericDeliveryStartTimestamp :: Generic DeliveryStartTimestamp _
instance showDeliveryStartTimestamp :: Show DeliveryStartTimestamp where
  show = genericShow
instance decodeDeliveryStartTimestamp :: Decode DeliveryStartTimestamp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStartTimestamp :: Encode DeliveryStartTimestamp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStreamARN = DeliveryStreamARN String
derive instance newtypeDeliveryStreamARN :: Newtype DeliveryStreamARN _
derive instance repGenericDeliveryStreamARN :: Generic DeliveryStreamARN _
instance showDeliveryStreamARN :: Show DeliveryStreamARN where
  show = genericShow
instance decodeDeliveryStreamARN :: Decode DeliveryStreamARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStreamARN :: Encode DeliveryStreamARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a delivery stream.</p>
newtype DeliveryStreamDescription = DeliveryStreamDescription 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "DeliveryStreamARN" :: (DeliveryStreamARN)
  , "DeliveryStreamStatus" :: (DeliveryStreamStatus)
  , "DeliveryStreamType" :: (DeliveryStreamType)
  , "VersionId" :: (DeliveryStreamVersionId)
  , "CreateTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdateTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "Source" :: NullOrUndefined.NullOrUndefined (SourceDescription)
  , "Destinations" :: (DestinationDescriptionList)
  , "HasMoreDestinations" :: (BooleanObject)
  }
derive instance newtypeDeliveryStreamDescription :: Newtype DeliveryStreamDescription _
derive instance repGenericDeliveryStreamDescription :: Generic DeliveryStreamDescription _
instance showDeliveryStreamDescription :: Show DeliveryStreamDescription where
  show = genericShow
instance decodeDeliveryStreamDescription :: Decode DeliveryStreamDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStreamDescription :: Encode DeliveryStreamDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStreamName = DeliveryStreamName String
derive instance newtypeDeliveryStreamName :: Newtype DeliveryStreamName _
derive instance repGenericDeliveryStreamName :: Generic DeliveryStreamName _
instance showDeliveryStreamName :: Show DeliveryStreamName where
  show = genericShow
instance decodeDeliveryStreamName :: Decode DeliveryStreamName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStreamName :: Encode DeliveryStreamName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStreamNameList = DeliveryStreamNameList (Array DeliveryStreamName)
derive instance newtypeDeliveryStreamNameList :: Newtype DeliveryStreamNameList _
derive instance repGenericDeliveryStreamNameList :: Generic DeliveryStreamNameList _
instance showDeliveryStreamNameList :: Show DeliveryStreamNameList where
  show = genericShow
instance decodeDeliveryStreamNameList :: Decode DeliveryStreamNameList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStreamNameList :: Encode DeliveryStreamNameList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStreamStatus = DeliveryStreamStatus String
derive instance newtypeDeliveryStreamStatus :: Newtype DeliveryStreamStatus _
derive instance repGenericDeliveryStreamStatus :: Generic DeliveryStreamStatus _
instance showDeliveryStreamStatus :: Show DeliveryStreamStatus where
  show = genericShow
instance decodeDeliveryStreamStatus :: Decode DeliveryStreamStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStreamStatus :: Encode DeliveryStreamStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStreamType = DeliveryStreamType String
derive instance newtypeDeliveryStreamType :: Newtype DeliveryStreamType _
derive instance repGenericDeliveryStreamType :: Generic DeliveryStreamType _
instance showDeliveryStreamType :: Show DeliveryStreamType where
  show = genericShow
instance decodeDeliveryStreamType :: Decode DeliveryStreamType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStreamType :: Encode DeliveryStreamType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeliveryStreamVersionId = DeliveryStreamVersionId String
derive instance newtypeDeliveryStreamVersionId :: Newtype DeliveryStreamVersionId _
derive instance repGenericDeliveryStreamVersionId :: Generic DeliveryStreamVersionId _
instance showDeliveryStreamVersionId :: Show DeliveryStreamVersionId where
  show = genericShow
instance decodeDeliveryStreamVersionId :: Decode DeliveryStreamVersionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeliveryStreamVersionId :: Encode DeliveryStreamVersionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDeliveryStreamInput = DescribeDeliveryStreamInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Limit" :: NullOrUndefined.NullOrUndefined (DescribeDeliveryStreamInputLimit)
  , "ExclusiveStartDestinationId" :: NullOrUndefined.NullOrUndefined (DestinationId)
  }
derive instance newtypeDescribeDeliveryStreamInput :: Newtype DescribeDeliveryStreamInput _
derive instance repGenericDescribeDeliveryStreamInput :: Generic DescribeDeliveryStreamInput _
instance showDescribeDeliveryStreamInput :: Show DescribeDeliveryStreamInput where
  show = genericShow
instance decodeDescribeDeliveryStreamInput :: Decode DescribeDeliveryStreamInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDeliveryStreamInput :: Encode DescribeDeliveryStreamInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDeliveryStreamInputLimit = DescribeDeliveryStreamInputLimit Int
derive instance newtypeDescribeDeliveryStreamInputLimit :: Newtype DescribeDeliveryStreamInputLimit _
derive instance repGenericDescribeDeliveryStreamInputLimit :: Generic DescribeDeliveryStreamInputLimit _
instance showDescribeDeliveryStreamInputLimit :: Show DescribeDeliveryStreamInputLimit where
  show = genericShow
instance decodeDescribeDeliveryStreamInputLimit :: Decode DescribeDeliveryStreamInputLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDeliveryStreamInputLimit :: Encode DescribeDeliveryStreamInputLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDeliveryStreamOutput = DescribeDeliveryStreamOutput 
  { "DeliveryStreamDescription" :: (DeliveryStreamDescription)
  }
derive instance newtypeDescribeDeliveryStreamOutput :: Newtype DescribeDeliveryStreamOutput _
derive instance repGenericDescribeDeliveryStreamOutput :: Generic DescribeDeliveryStreamOutput _
instance showDescribeDeliveryStreamOutput :: Show DescribeDeliveryStreamOutput where
  show = genericShow
instance decodeDescribeDeliveryStreamOutput :: Decode DescribeDeliveryStreamOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDeliveryStreamOutput :: Encode DescribeDeliveryStreamOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the destination for a delivery stream.</p>
newtype DestinationDescription = DestinationDescription 
  { "DestinationId" :: (DestinationId)
  , "S3DestinationDescription" :: NullOrUndefined.NullOrUndefined (S3DestinationDescription)
  , "ExtendedS3DestinationDescription" :: NullOrUndefined.NullOrUndefined (ExtendedS3DestinationDescription)
  , "RedshiftDestinationDescription" :: NullOrUndefined.NullOrUndefined (RedshiftDestinationDescription)
  , "ElasticsearchDestinationDescription" :: NullOrUndefined.NullOrUndefined (ElasticsearchDestinationDescription)
  , "SplunkDestinationDescription" :: NullOrUndefined.NullOrUndefined (SplunkDestinationDescription)
  }
derive instance newtypeDestinationDescription :: Newtype DestinationDescription _
derive instance repGenericDestinationDescription :: Generic DestinationDescription _
instance showDestinationDescription :: Show DestinationDescription where
  show = genericShow
instance decodeDestinationDescription :: Decode DestinationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinationDescription :: Encode DestinationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DestinationDescriptionList = DestinationDescriptionList (Array DestinationDescription)
derive instance newtypeDestinationDescriptionList :: Newtype DestinationDescriptionList _
derive instance repGenericDestinationDescriptionList :: Generic DestinationDescriptionList _
instance showDestinationDescriptionList :: Show DestinationDescriptionList where
  show = genericShow
instance decodeDestinationDescriptionList :: Decode DestinationDescriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinationDescriptionList :: Encode DestinationDescriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DestinationId = DestinationId String
derive instance newtypeDestinationId :: Newtype DestinationId _
derive instance repGenericDestinationId :: Generic DestinationId _
instance showDestinationId :: Show DestinationId where
  show = genericShow
instance decodeDestinationId :: Decode DestinationId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDestinationId :: Encode DestinationId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the buffering to perform before delivering data to the Amazon ES destination.</p>
newtype ElasticsearchBufferingHints = ElasticsearchBufferingHints 
  { "IntervalInSeconds" :: NullOrUndefined.NullOrUndefined (ElasticsearchBufferingIntervalInSeconds)
  , "SizeInMBs" :: NullOrUndefined.NullOrUndefined (ElasticsearchBufferingSizeInMBs)
  }
derive instance newtypeElasticsearchBufferingHints :: Newtype ElasticsearchBufferingHints _
derive instance repGenericElasticsearchBufferingHints :: Generic ElasticsearchBufferingHints _
instance showElasticsearchBufferingHints :: Show ElasticsearchBufferingHints where
  show = genericShow
instance decodeElasticsearchBufferingHints :: Decode ElasticsearchBufferingHints where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchBufferingHints :: Encode ElasticsearchBufferingHints where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchBufferingIntervalInSeconds = ElasticsearchBufferingIntervalInSeconds Int
derive instance newtypeElasticsearchBufferingIntervalInSeconds :: Newtype ElasticsearchBufferingIntervalInSeconds _
derive instance repGenericElasticsearchBufferingIntervalInSeconds :: Generic ElasticsearchBufferingIntervalInSeconds _
instance showElasticsearchBufferingIntervalInSeconds :: Show ElasticsearchBufferingIntervalInSeconds where
  show = genericShow
instance decodeElasticsearchBufferingIntervalInSeconds :: Decode ElasticsearchBufferingIntervalInSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchBufferingIntervalInSeconds :: Encode ElasticsearchBufferingIntervalInSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchBufferingSizeInMBs = ElasticsearchBufferingSizeInMBs Int
derive instance newtypeElasticsearchBufferingSizeInMBs :: Newtype ElasticsearchBufferingSizeInMBs _
derive instance repGenericElasticsearchBufferingSizeInMBs :: Generic ElasticsearchBufferingSizeInMBs _
instance showElasticsearchBufferingSizeInMBs :: Show ElasticsearchBufferingSizeInMBs where
  show = genericShow
instance decodeElasticsearchBufferingSizeInMBs :: Decode ElasticsearchBufferingSizeInMBs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchBufferingSizeInMBs :: Encode ElasticsearchBufferingSizeInMBs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the configuration of a destination in Amazon ES.</p>
newtype ElasticsearchDestinationConfiguration = ElasticsearchDestinationConfiguration 
  { "RoleARN" :: (RoleARN)
  , "DomainARN" :: (ElasticsearchDomainARN)
  , "IndexName" :: (ElasticsearchIndexName)
  , "TypeName" :: (ElasticsearchTypeName)
  , "IndexRotationPeriod" :: NullOrUndefined.NullOrUndefined (ElasticsearchIndexRotationPeriod)
  , "BufferingHints" :: NullOrUndefined.NullOrUndefined (ElasticsearchBufferingHints)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (ElasticsearchRetryOptions)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (ElasticsearchS3BackupMode)
  , "S3Configuration" :: (S3DestinationConfiguration)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeElasticsearchDestinationConfiguration :: Newtype ElasticsearchDestinationConfiguration _
derive instance repGenericElasticsearchDestinationConfiguration :: Generic ElasticsearchDestinationConfiguration _
instance showElasticsearchDestinationConfiguration :: Show ElasticsearchDestinationConfiguration where
  show = genericShow
instance decodeElasticsearchDestinationConfiguration :: Decode ElasticsearchDestinationConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchDestinationConfiguration :: Encode ElasticsearchDestinationConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The destination description in Amazon ES.</p>
newtype ElasticsearchDestinationDescription = ElasticsearchDestinationDescription 
  { "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "DomainARN" :: NullOrUndefined.NullOrUndefined (ElasticsearchDomainARN)
  , "IndexName" :: NullOrUndefined.NullOrUndefined (ElasticsearchIndexName)
  , "TypeName" :: NullOrUndefined.NullOrUndefined (ElasticsearchTypeName)
  , "IndexRotationPeriod" :: NullOrUndefined.NullOrUndefined (ElasticsearchIndexRotationPeriod)
  , "BufferingHints" :: NullOrUndefined.NullOrUndefined (ElasticsearchBufferingHints)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (ElasticsearchRetryOptions)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (ElasticsearchS3BackupMode)
  , "S3DestinationDescription" :: NullOrUndefined.NullOrUndefined (S3DestinationDescription)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeElasticsearchDestinationDescription :: Newtype ElasticsearchDestinationDescription _
derive instance repGenericElasticsearchDestinationDescription :: Generic ElasticsearchDestinationDescription _
instance showElasticsearchDestinationDescription :: Show ElasticsearchDestinationDescription where
  show = genericShow
instance decodeElasticsearchDestinationDescription :: Decode ElasticsearchDestinationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchDestinationDescription :: Encode ElasticsearchDestinationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an update for a destination in Amazon ES.</p>
newtype ElasticsearchDestinationUpdate = ElasticsearchDestinationUpdate 
  { "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "DomainARN" :: NullOrUndefined.NullOrUndefined (ElasticsearchDomainARN)
  , "IndexName" :: NullOrUndefined.NullOrUndefined (ElasticsearchIndexName)
  , "TypeName" :: NullOrUndefined.NullOrUndefined (ElasticsearchTypeName)
  , "IndexRotationPeriod" :: NullOrUndefined.NullOrUndefined (ElasticsearchIndexRotationPeriod)
  , "BufferingHints" :: NullOrUndefined.NullOrUndefined (ElasticsearchBufferingHints)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (ElasticsearchRetryOptions)
  , "S3Update" :: NullOrUndefined.NullOrUndefined (S3DestinationUpdate)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeElasticsearchDestinationUpdate :: Newtype ElasticsearchDestinationUpdate _
derive instance repGenericElasticsearchDestinationUpdate :: Generic ElasticsearchDestinationUpdate _
instance showElasticsearchDestinationUpdate :: Show ElasticsearchDestinationUpdate where
  show = genericShow
instance decodeElasticsearchDestinationUpdate :: Decode ElasticsearchDestinationUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchDestinationUpdate :: Encode ElasticsearchDestinationUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchDomainARN = ElasticsearchDomainARN String
derive instance newtypeElasticsearchDomainARN :: Newtype ElasticsearchDomainARN _
derive instance repGenericElasticsearchDomainARN :: Generic ElasticsearchDomainARN _
instance showElasticsearchDomainARN :: Show ElasticsearchDomainARN where
  show = genericShow
instance decodeElasticsearchDomainARN :: Decode ElasticsearchDomainARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchDomainARN :: Encode ElasticsearchDomainARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchIndexName = ElasticsearchIndexName String
derive instance newtypeElasticsearchIndexName :: Newtype ElasticsearchIndexName _
derive instance repGenericElasticsearchIndexName :: Generic ElasticsearchIndexName _
instance showElasticsearchIndexName :: Show ElasticsearchIndexName where
  show = genericShow
instance decodeElasticsearchIndexName :: Decode ElasticsearchIndexName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchIndexName :: Encode ElasticsearchIndexName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchIndexRotationPeriod = ElasticsearchIndexRotationPeriod String
derive instance newtypeElasticsearchIndexRotationPeriod :: Newtype ElasticsearchIndexRotationPeriod _
derive instance repGenericElasticsearchIndexRotationPeriod :: Generic ElasticsearchIndexRotationPeriod _
instance showElasticsearchIndexRotationPeriod :: Show ElasticsearchIndexRotationPeriod where
  show = genericShow
instance decodeElasticsearchIndexRotationPeriod :: Decode ElasticsearchIndexRotationPeriod where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchIndexRotationPeriod :: Encode ElasticsearchIndexRotationPeriod where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchRetryDurationInSeconds = ElasticsearchRetryDurationInSeconds Int
derive instance newtypeElasticsearchRetryDurationInSeconds :: Newtype ElasticsearchRetryDurationInSeconds _
derive instance repGenericElasticsearchRetryDurationInSeconds :: Generic ElasticsearchRetryDurationInSeconds _
instance showElasticsearchRetryDurationInSeconds :: Show ElasticsearchRetryDurationInSeconds where
  show = genericShow
instance decodeElasticsearchRetryDurationInSeconds :: Decode ElasticsearchRetryDurationInSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchRetryDurationInSeconds :: Encode ElasticsearchRetryDurationInSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Amazon ES.</p>
newtype ElasticsearchRetryOptions = ElasticsearchRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined.NullOrUndefined (ElasticsearchRetryDurationInSeconds)
  }
derive instance newtypeElasticsearchRetryOptions :: Newtype ElasticsearchRetryOptions _
derive instance repGenericElasticsearchRetryOptions :: Generic ElasticsearchRetryOptions _
instance showElasticsearchRetryOptions :: Show ElasticsearchRetryOptions where
  show = genericShow
instance decodeElasticsearchRetryOptions :: Decode ElasticsearchRetryOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchRetryOptions :: Encode ElasticsearchRetryOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchS3BackupMode = ElasticsearchS3BackupMode String
derive instance newtypeElasticsearchS3BackupMode :: Newtype ElasticsearchS3BackupMode _
derive instance repGenericElasticsearchS3BackupMode :: Generic ElasticsearchS3BackupMode _
instance showElasticsearchS3BackupMode :: Show ElasticsearchS3BackupMode where
  show = genericShow
instance decodeElasticsearchS3BackupMode :: Decode ElasticsearchS3BackupMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchS3BackupMode :: Encode ElasticsearchS3BackupMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ElasticsearchTypeName = ElasticsearchTypeName String
derive instance newtypeElasticsearchTypeName :: Newtype ElasticsearchTypeName _
derive instance repGenericElasticsearchTypeName :: Generic ElasticsearchTypeName _
instance showElasticsearchTypeName :: Show ElasticsearchTypeName where
  show = genericShow
instance decodeElasticsearchTypeName :: Decode ElasticsearchTypeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeElasticsearchTypeName :: Encode ElasticsearchTypeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the encryption for a destination in Amazon S3.</p>
newtype EncryptionConfiguration = EncryptionConfiguration 
  { "NoEncryptionConfig" :: NullOrUndefined.NullOrUndefined (NoEncryptionConfig)
  , "KMSEncryptionConfig" :: NullOrUndefined.NullOrUndefined (KMSEncryptionConfig)
  }
derive instance newtypeEncryptionConfiguration :: Newtype EncryptionConfiguration _
derive instance repGenericEncryptionConfiguration :: Generic EncryptionConfiguration _
instance showEncryptionConfiguration :: Show EncryptionConfiguration where
  show = genericShow
instance decodeEncryptionConfiguration :: Decode EncryptionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionConfiguration :: Encode EncryptionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _
derive instance repGenericErrorCode :: Generic ErrorCode _
instance showErrorCode :: Show ErrorCode where
  show = genericShow
instance decodeErrorCode :: Decode ErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorCode :: Encode ErrorCode where
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


-- | <p>Describes the configuration of a destination in Amazon S3.</p>
newtype ExtendedS3DestinationConfiguration = ExtendedS3DestinationConfiguration 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "BufferingHints" :: NullOrUndefined.NullOrUndefined (BufferingHints)
  , "CompressionFormat" :: NullOrUndefined.NullOrUndefined (CompressionFormat)
  , "EncryptionConfiguration" :: NullOrUndefined.NullOrUndefined (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (S3BackupMode)
  , "S3BackupConfiguration" :: NullOrUndefined.NullOrUndefined (S3DestinationConfiguration)
  }
derive instance newtypeExtendedS3DestinationConfiguration :: Newtype ExtendedS3DestinationConfiguration _
derive instance repGenericExtendedS3DestinationConfiguration :: Generic ExtendedS3DestinationConfiguration _
instance showExtendedS3DestinationConfiguration :: Show ExtendedS3DestinationConfiguration where
  show = genericShow
instance decodeExtendedS3DestinationConfiguration :: Decode ExtendedS3DestinationConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedS3DestinationConfiguration :: Encode ExtendedS3DestinationConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a destination in Amazon S3.</p>
newtype ExtendedS3DestinationDescription = ExtendedS3DestinationDescription 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "BufferingHints" :: (BufferingHints)
  , "CompressionFormat" :: (CompressionFormat)
  , "EncryptionConfiguration" :: (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (S3BackupMode)
  , "S3BackupDescription" :: NullOrUndefined.NullOrUndefined (S3DestinationDescription)
  }
derive instance newtypeExtendedS3DestinationDescription :: Newtype ExtendedS3DestinationDescription _
derive instance repGenericExtendedS3DestinationDescription :: Generic ExtendedS3DestinationDescription _
instance showExtendedS3DestinationDescription :: Show ExtendedS3DestinationDescription where
  show = genericShow
instance decodeExtendedS3DestinationDescription :: Decode ExtendedS3DestinationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedS3DestinationDescription :: Encode ExtendedS3DestinationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an update for a destination in Amazon S3.</p>
newtype ExtendedS3DestinationUpdate = ExtendedS3DestinationUpdate 
  { "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "BucketARN" :: NullOrUndefined.NullOrUndefined (BucketARN)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "BufferingHints" :: NullOrUndefined.NullOrUndefined (BufferingHints)
  , "CompressionFormat" :: NullOrUndefined.NullOrUndefined (CompressionFormat)
  , "EncryptionConfiguration" :: NullOrUndefined.NullOrUndefined (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (S3BackupMode)
  , "S3BackupUpdate" :: NullOrUndefined.NullOrUndefined (S3DestinationUpdate)
  }
derive instance newtypeExtendedS3DestinationUpdate :: Newtype ExtendedS3DestinationUpdate _
derive instance repGenericExtendedS3DestinationUpdate :: Generic ExtendedS3DestinationUpdate _
instance showExtendedS3DestinationUpdate :: Show ExtendedS3DestinationUpdate where
  show = genericShow
instance decodeExtendedS3DestinationUpdate :: Decode ExtendedS3DestinationUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExtendedS3DestinationUpdate :: Encode ExtendedS3DestinationUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HECAcknowledgmentTimeoutInSeconds = HECAcknowledgmentTimeoutInSeconds Int
derive instance newtypeHECAcknowledgmentTimeoutInSeconds :: Newtype HECAcknowledgmentTimeoutInSeconds _
derive instance repGenericHECAcknowledgmentTimeoutInSeconds :: Generic HECAcknowledgmentTimeoutInSeconds _
instance showHECAcknowledgmentTimeoutInSeconds :: Show HECAcknowledgmentTimeoutInSeconds where
  show = genericShow
instance decodeHECAcknowledgmentTimeoutInSeconds :: Decode HECAcknowledgmentTimeoutInSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHECAcknowledgmentTimeoutInSeconds :: Encode HECAcknowledgmentTimeoutInSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HECEndpoint = HECEndpoint String
derive instance newtypeHECEndpoint :: Newtype HECEndpoint _
derive instance repGenericHECEndpoint :: Generic HECEndpoint _
instance showHECEndpoint :: Show HECEndpoint where
  show = genericShow
instance decodeHECEndpoint :: Decode HECEndpoint where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHECEndpoint :: Encode HECEndpoint where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HECEndpointType = HECEndpointType String
derive instance newtypeHECEndpointType :: Newtype HECEndpointType _
derive instance repGenericHECEndpointType :: Generic HECEndpointType _
instance showHECEndpointType :: Show HECEndpointType where
  show = genericShow
instance decodeHECEndpointType :: Decode HECEndpointType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHECEndpointType :: Encode HECEndpointType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HECToken = HECToken String
derive instance newtypeHECToken :: Newtype HECToken _
derive instance repGenericHECToken :: Generic HECToken _
instance showHECToken :: Show HECToken where
  show = genericShow
instance decodeHECToken :: Decode HECToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHECToken :: Encode HECToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IntervalInSeconds = IntervalInSeconds Int
derive instance newtypeIntervalInSeconds :: Newtype IntervalInSeconds _
derive instance repGenericIntervalInSeconds :: Generic IntervalInSeconds _
instance showIntervalInSeconds :: Show IntervalInSeconds where
  show = genericShow
instance decodeIntervalInSeconds :: Decode IntervalInSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIntervalInSeconds :: Encode IntervalInSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified input parameter has a value that is not valid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _
derive instance repGenericInvalidArgumentException :: Generic InvalidArgumentException _
instance showInvalidArgumentException :: Show InvalidArgumentException where
  show = genericShow
instance decodeInvalidArgumentException :: Decode InvalidArgumentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArgumentException :: Encode InvalidArgumentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an encryption key for a destination in Amazon S3.</p>
newtype KMSEncryptionConfig = KMSEncryptionConfig 
  { "AWSKMSKeyARN" :: (AWSKMSKeyARN)
  }
derive instance newtypeKMSEncryptionConfig :: Newtype KMSEncryptionConfig _
derive instance repGenericKMSEncryptionConfig :: Generic KMSEncryptionConfig _
instance showKMSEncryptionConfig :: Show KMSEncryptionConfig where
  show = genericShow
instance decodeKMSEncryptionConfig :: Decode KMSEncryptionConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKMSEncryptionConfig :: Encode KMSEncryptionConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KinesisStreamARN = KinesisStreamARN String
derive instance newtypeKinesisStreamARN :: Newtype KinesisStreamARN _
derive instance repGenericKinesisStreamARN :: Generic KinesisStreamARN _
instance showKinesisStreamARN :: Show KinesisStreamARN where
  show = genericShow
instance decodeKinesisStreamARN :: Decode KinesisStreamARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamARN :: Encode KinesisStreamARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The stream and role ARNs for a Kinesis stream used as the source for a delivery stream.</p>
newtype KinesisStreamSourceConfiguration = KinesisStreamSourceConfiguration 
  { "KinesisStreamARN" :: (KinesisStreamARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisStreamSourceConfiguration :: Newtype KinesisStreamSourceConfiguration _
derive instance repGenericKinesisStreamSourceConfiguration :: Generic KinesisStreamSourceConfiguration _
instance showKinesisStreamSourceConfiguration :: Show KinesisStreamSourceConfiguration where
  show = genericShow
instance decodeKinesisStreamSourceConfiguration :: Decode KinesisStreamSourceConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamSourceConfiguration :: Encode KinesisStreamSourceConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details about a Kinesis stream used as the source for a Kinesis Firehose delivery stream.</p>
newtype KinesisStreamSourceDescription = KinesisStreamSourceDescription 
  { "KinesisStreamARN" :: NullOrUndefined.NullOrUndefined (KinesisStreamARN)
  , "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "DeliveryStartTimestamp" :: NullOrUndefined.NullOrUndefined (DeliveryStartTimestamp)
  }
derive instance newtypeKinesisStreamSourceDescription :: Newtype KinesisStreamSourceDescription _
derive instance repGenericKinesisStreamSourceDescription :: Generic KinesisStreamSourceDescription _
instance showKinesisStreamSourceDescription :: Show KinesisStreamSourceDescription where
  show = genericShow
instance decodeKinesisStreamSourceDescription :: Decode KinesisStreamSourceDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKinesisStreamSourceDescription :: Encode KinesisStreamSourceDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You have already reached the limit for a requested resource.</p>
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


newtype ListDeliveryStreamsInput = ListDeliveryStreamsInput 
  { "Limit" :: NullOrUndefined.NullOrUndefined (ListDeliveryStreamsInputLimit)
  , "DeliveryStreamType" :: NullOrUndefined.NullOrUndefined (DeliveryStreamType)
  , "ExclusiveStartDeliveryStreamName" :: NullOrUndefined.NullOrUndefined (DeliveryStreamName)
  }
derive instance newtypeListDeliveryStreamsInput :: Newtype ListDeliveryStreamsInput _
derive instance repGenericListDeliveryStreamsInput :: Generic ListDeliveryStreamsInput _
instance showListDeliveryStreamsInput :: Show ListDeliveryStreamsInput where
  show = genericShow
instance decodeListDeliveryStreamsInput :: Decode ListDeliveryStreamsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeliveryStreamsInput :: Encode ListDeliveryStreamsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDeliveryStreamsInputLimit = ListDeliveryStreamsInputLimit Int
derive instance newtypeListDeliveryStreamsInputLimit :: Newtype ListDeliveryStreamsInputLimit _
derive instance repGenericListDeliveryStreamsInputLimit :: Generic ListDeliveryStreamsInputLimit _
instance showListDeliveryStreamsInputLimit :: Show ListDeliveryStreamsInputLimit where
  show = genericShow
instance decodeListDeliveryStreamsInputLimit :: Decode ListDeliveryStreamsInputLimit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeliveryStreamsInputLimit :: Encode ListDeliveryStreamsInputLimit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListDeliveryStreamsOutput = ListDeliveryStreamsOutput 
  { "DeliveryStreamNames" :: (DeliveryStreamNameList)
  , "HasMoreDeliveryStreams" :: (BooleanObject)
  }
derive instance newtypeListDeliveryStreamsOutput :: Newtype ListDeliveryStreamsOutput _
derive instance repGenericListDeliveryStreamsOutput :: Generic ListDeliveryStreamsOutput _
instance showListDeliveryStreamsOutput :: Show ListDeliveryStreamsOutput where
  show = genericShow
instance decodeListDeliveryStreamsOutput :: Decode ListDeliveryStreamsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListDeliveryStreamsOutput :: Encode ListDeliveryStreamsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogGroupName = LogGroupName String
derive instance newtypeLogGroupName :: Newtype LogGroupName _
derive instance repGenericLogGroupName :: Generic LogGroupName _
instance showLogGroupName :: Show LogGroupName where
  show = genericShow
instance decodeLogGroupName :: Decode LogGroupName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogGroupName :: Encode LogGroupName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LogStreamName = LogStreamName String
derive instance newtypeLogStreamName :: Newtype LogStreamName _
derive instance repGenericLogStreamName :: Generic LogStreamName _
instance showLogStreamName :: Show LogStreamName where
  show = genericShow
instance decodeLogStreamName :: Decode LogStreamName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogStreamName :: Encode LogStreamName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NoEncryptionConfig = NoEncryptionConfig String
derive instance newtypeNoEncryptionConfig :: Newtype NoEncryptionConfig _
derive instance repGenericNoEncryptionConfig :: Generic NoEncryptionConfig _
instance showNoEncryptionConfig :: Show NoEncryptionConfig where
  show = genericShow
instance decodeNoEncryptionConfig :: Decode NoEncryptionConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNoEncryptionConfig :: Encode NoEncryptionConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonNegativeIntegerObject = NonNegativeIntegerObject Int
derive instance newtypeNonNegativeIntegerObject :: Newtype NonNegativeIntegerObject _
derive instance repGenericNonNegativeIntegerObject :: Generic NonNegativeIntegerObject _
instance showNonNegativeIntegerObject :: Show NonNegativeIntegerObject where
  show = genericShow
instance decodeNonNegativeIntegerObject :: Decode NonNegativeIntegerObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonNegativeIntegerObject :: Encode NonNegativeIntegerObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Password = Password String
derive instance newtypePassword :: Newtype Password _
derive instance repGenericPassword :: Generic Password _
instance showPassword :: Show Password where
  show = genericShow
instance decodePassword :: Decode Password where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePassword :: Encode Password where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Prefix = Prefix String
derive instance newtypePrefix :: Newtype Prefix _
derive instance repGenericPrefix :: Generic Prefix _
instance showPrefix :: Show Prefix where
  show = genericShow
instance decodePrefix :: Decode Prefix where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrefix :: Encode Prefix where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a data processing configuration.</p>
newtype ProcessingConfiguration = ProcessingConfiguration 
  { "Enabled" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "Processors" :: NullOrUndefined.NullOrUndefined (ProcessorList)
  }
derive instance newtypeProcessingConfiguration :: Newtype ProcessingConfiguration _
derive instance repGenericProcessingConfiguration :: Generic ProcessingConfiguration _
instance showProcessingConfiguration :: Show ProcessingConfiguration where
  show = genericShow
instance decodeProcessingConfiguration :: Decode ProcessingConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessingConfiguration :: Encode ProcessingConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a data processor.</p>
newtype Processor = Processor 
  { "Type" :: (ProcessorType)
  , "Parameters" :: NullOrUndefined.NullOrUndefined (ProcessorParameterList)
  }
derive instance newtypeProcessor :: Newtype Processor _
derive instance repGenericProcessor :: Generic Processor _
instance showProcessor :: Show Processor where
  show = genericShow
instance decodeProcessor :: Decode Processor where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessor :: Encode Processor where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessorList = ProcessorList (Array Processor)
derive instance newtypeProcessorList :: Newtype ProcessorList _
derive instance repGenericProcessorList :: Generic ProcessorList _
instance showProcessorList :: Show ProcessorList where
  show = genericShow
instance decodeProcessorList :: Decode ProcessorList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessorList :: Encode ProcessorList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the processor parameter.</p>
newtype ProcessorParameter = ProcessorParameter 
  { "ParameterName" :: (ProcessorParameterName)
  , "ParameterValue" :: (ProcessorParameterValue)
  }
derive instance newtypeProcessorParameter :: Newtype ProcessorParameter _
derive instance repGenericProcessorParameter :: Generic ProcessorParameter _
instance showProcessorParameter :: Show ProcessorParameter where
  show = genericShow
instance decodeProcessorParameter :: Decode ProcessorParameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessorParameter :: Encode ProcessorParameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessorParameterList = ProcessorParameterList (Array ProcessorParameter)
derive instance newtypeProcessorParameterList :: Newtype ProcessorParameterList _
derive instance repGenericProcessorParameterList :: Generic ProcessorParameterList _
instance showProcessorParameterList :: Show ProcessorParameterList where
  show = genericShow
instance decodeProcessorParameterList :: Decode ProcessorParameterList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessorParameterList :: Encode ProcessorParameterList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessorParameterName = ProcessorParameterName String
derive instance newtypeProcessorParameterName :: Newtype ProcessorParameterName _
derive instance repGenericProcessorParameterName :: Generic ProcessorParameterName _
instance showProcessorParameterName :: Show ProcessorParameterName where
  show = genericShow
instance decodeProcessorParameterName :: Decode ProcessorParameterName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessorParameterName :: Encode ProcessorParameterName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessorParameterValue = ProcessorParameterValue String
derive instance newtypeProcessorParameterValue :: Newtype ProcessorParameterValue _
derive instance repGenericProcessorParameterValue :: Generic ProcessorParameterValue _
instance showProcessorParameterValue :: Show ProcessorParameterValue where
  show = genericShow
instance decodeProcessorParameterValue :: Decode ProcessorParameterValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessorParameterValue :: Encode ProcessorParameterValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProcessorType = ProcessorType String
derive instance newtypeProcessorType :: Newtype ProcessorType _
derive instance repGenericProcessorType :: Generic ProcessorType _
instance showProcessorType :: Show ProcessorType where
  show = genericShow
instance decodeProcessorType :: Decode ProcessorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProcessorType :: Encode ProcessorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRecordBatchInput = PutRecordBatchInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Records" :: (PutRecordBatchRequestEntryList)
  }
derive instance newtypePutRecordBatchInput :: Newtype PutRecordBatchInput _
derive instance repGenericPutRecordBatchInput :: Generic PutRecordBatchInput _
instance showPutRecordBatchInput :: Show PutRecordBatchInput where
  show = genericShow
instance decodePutRecordBatchInput :: Decode PutRecordBatchInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRecordBatchInput :: Encode PutRecordBatchInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRecordBatchOutput = PutRecordBatchOutput 
  { "FailedPutCount" :: (NonNegativeIntegerObject)
  , "RequestResponses" :: (PutRecordBatchResponseEntryList)
  }
derive instance newtypePutRecordBatchOutput :: Newtype PutRecordBatchOutput _
derive instance repGenericPutRecordBatchOutput :: Generic PutRecordBatchOutput _
instance showPutRecordBatchOutput :: Show PutRecordBatchOutput where
  show = genericShow
instance decodePutRecordBatchOutput :: Decode PutRecordBatchOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRecordBatchOutput :: Encode PutRecordBatchOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRecordBatchRequestEntryList = PutRecordBatchRequestEntryList (Array Record'')
derive instance newtypePutRecordBatchRequestEntryList :: Newtype PutRecordBatchRequestEntryList _
derive instance repGenericPutRecordBatchRequestEntryList :: Generic PutRecordBatchRequestEntryList _
instance showPutRecordBatchRequestEntryList :: Show PutRecordBatchRequestEntryList where
  show = genericShow
instance decodePutRecordBatchRequestEntryList :: Decode PutRecordBatchRequestEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRecordBatchRequestEntryList :: Encode PutRecordBatchRequestEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the result for an individual record from a <a>PutRecordBatch</a> request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.</p>
newtype PutRecordBatchResponseEntry = PutRecordBatchResponseEntry 
  { "RecordId" :: NullOrUndefined.NullOrUndefined (PutResponseRecordId)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypePutRecordBatchResponseEntry :: Newtype PutRecordBatchResponseEntry _
derive instance repGenericPutRecordBatchResponseEntry :: Generic PutRecordBatchResponseEntry _
instance showPutRecordBatchResponseEntry :: Show PutRecordBatchResponseEntry where
  show = genericShow
instance decodePutRecordBatchResponseEntry :: Decode PutRecordBatchResponseEntry where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRecordBatchResponseEntry :: Encode PutRecordBatchResponseEntry where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRecordBatchResponseEntryList = PutRecordBatchResponseEntryList (Array PutRecordBatchResponseEntry)
derive instance newtypePutRecordBatchResponseEntryList :: Newtype PutRecordBatchResponseEntryList _
derive instance repGenericPutRecordBatchResponseEntryList :: Generic PutRecordBatchResponseEntryList _
instance showPutRecordBatchResponseEntryList :: Show PutRecordBatchResponseEntryList where
  show = genericShow
instance decodePutRecordBatchResponseEntryList :: Decode PutRecordBatchResponseEntryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRecordBatchResponseEntryList :: Encode PutRecordBatchResponseEntryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRecordInput = PutRecordInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Record''" :: (Record'')
  }
derive instance newtypePutRecordInput :: Newtype PutRecordInput _
derive instance repGenericPutRecordInput :: Generic PutRecordInput _
instance showPutRecordInput :: Show PutRecordInput where
  show = genericShow
instance decodePutRecordInput :: Decode PutRecordInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRecordInput :: Encode PutRecordInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutRecordOutput = PutRecordOutput 
  { "RecordId" :: (PutResponseRecordId)
  }
derive instance newtypePutRecordOutput :: Newtype PutRecordOutput _
derive instance repGenericPutRecordOutput :: Generic PutRecordOutput _
instance showPutRecordOutput :: Show PutRecordOutput where
  show = genericShow
instance decodePutRecordOutput :: Decode PutRecordOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutRecordOutput :: Encode PutRecordOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PutResponseRecordId = PutResponseRecordId String
derive instance newtypePutResponseRecordId :: Newtype PutResponseRecordId _
derive instance repGenericPutResponseRecordId :: Generic PutResponseRecordId _
instance showPutResponseRecordId :: Show PutResponseRecordId where
  show = genericShow
instance decodePutResponseRecordId :: Decode PutResponseRecordId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutResponseRecordId :: Encode PutResponseRecordId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The unit of data in a delivery stream.</p>
newtype Record'' = Record'' 
  { "Data" :: (Data)
  }
derive instance newtypeRecord'' :: Newtype Record'' _
derive instance repGenericRecord'' :: Generic Record'' _
instance showRecord'' :: Show Record'' where
  show = genericShow
instance decodeRecord'' :: Decode Record'' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRecord'' :: Encode Record'' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the configuration of a destination in Amazon Redshift.</p>
newtype RedshiftDestinationConfiguration = RedshiftDestinationConfiguration 
  { "RoleARN" :: (RoleARN)
  , "ClusterJDBCURL" :: (ClusterJDBCURL)
  , "CopyCommand" :: (CopyCommand)
  , "Username" :: (Username)
  , "Password" :: (Password)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (RedshiftRetryOptions)
  , "S3Configuration" :: (S3DestinationConfiguration)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (RedshiftS3BackupMode)
  , "S3BackupConfiguration" :: NullOrUndefined.NullOrUndefined (S3DestinationConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeRedshiftDestinationConfiguration :: Newtype RedshiftDestinationConfiguration _
derive instance repGenericRedshiftDestinationConfiguration :: Generic RedshiftDestinationConfiguration _
instance showRedshiftDestinationConfiguration :: Show RedshiftDestinationConfiguration where
  show = genericShow
instance decodeRedshiftDestinationConfiguration :: Decode RedshiftDestinationConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDestinationConfiguration :: Encode RedshiftDestinationConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a destination in Amazon Redshift.</p>
newtype RedshiftDestinationDescription = RedshiftDestinationDescription 
  { "RoleARN" :: (RoleARN)
  , "ClusterJDBCURL" :: (ClusterJDBCURL)
  , "CopyCommand" :: (CopyCommand)
  , "Username" :: (Username)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (RedshiftRetryOptions)
  , "S3DestinationDescription" :: (S3DestinationDescription)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (RedshiftS3BackupMode)
  , "S3BackupDescription" :: NullOrUndefined.NullOrUndefined (S3DestinationDescription)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeRedshiftDestinationDescription :: Newtype RedshiftDestinationDescription _
derive instance repGenericRedshiftDestinationDescription :: Generic RedshiftDestinationDescription _
instance showRedshiftDestinationDescription :: Show RedshiftDestinationDescription where
  show = genericShow
instance decodeRedshiftDestinationDescription :: Decode RedshiftDestinationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDestinationDescription :: Encode RedshiftDestinationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an update for a destination in Amazon Redshift.</p>
newtype RedshiftDestinationUpdate = RedshiftDestinationUpdate 
  { "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "ClusterJDBCURL" :: NullOrUndefined.NullOrUndefined (ClusterJDBCURL)
  , "CopyCommand" :: NullOrUndefined.NullOrUndefined (CopyCommand)
  , "Username" :: NullOrUndefined.NullOrUndefined (Username)
  , "Password" :: NullOrUndefined.NullOrUndefined (Password)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (RedshiftRetryOptions)
  , "S3Update" :: NullOrUndefined.NullOrUndefined (S3DestinationUpdate)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (RedshiftS3BackupMode)
  , "S3BackupUpdate" :: NullOrUndefined.NullOrUndefined (S3DestinationUpdate)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeRedshiftDestinationUpdate :: Newtype RedshiftDestinationUpdate _
derive instance repGenericRedshiftDestinationUpdate :: Generic RedshiftDestinationUpdate _
instance showRedshiftDestinationUpdate :: Show RedshiftDestinationUpdate where
  show = genericShow
instance decodeRedshiftDestinationUpdate :: Decode RedshiftDestinationUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftDestinationUpdate :: Encode RedshiftDestinationUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RedshiftRetryDurationInSeconds = RedshiftRetryDurationInSeconds Int
derive instance newtypeRedshiftRetryDurationInSeconds :: Newtype RedshiftRetryDurationInSeconds _
derive instance repGenericRedshiftRetryDurationInSeconds :: Generic RedshiftRetryDurationInSeconds _
instance showRedshiftRetryDurationInSeconds :: Show RedshiftRetryDurationInSeconds where
  show = genericShow
instance decodeRedshiftRetryDurationInSeconds :: Decode RedshiftRetryDurationInSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftRetryDurationInSeconds :: Encode RedshiftRetryDurationInSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Amazon Redshift.</p>
newtype RedshiftRetryOptions = RedshiftRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined.NullOrUndefined (RedshiftRetryDurationInSeconds)
  }
derive instance newtypeRedshiftRetryOptions :: Newtype RedshiftRetryOptions _
derive instance repGenericRedshiftRetryOptions :: Generic RedshiftRetryOptions _
instance showRedshiftRetryOptions :: Show RedshiftRetryOptions where
  show = genericShow
instance decodeRedshiftRetryOptions :: Decode RedshiftRetryOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftRetryOptions :: Encode RedshiftRetryOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RedshiftS3BackupMode = RedshiftS3BackupMode String
derive instance newtypeRedshiftS3BackupMode :: Newtype RedshiftS3BackupMode _
derive instance repGenericRedshiftS3BackupMode :: Generic RedshiftS3BackupMode _
instance showRedshiftS3BackupMode :: Show RedshiftS3BackupMode where
  show = genericShow
instance decodeRedshiftS3BackupMode :: Decode RedshiftS3BackupMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRedshiftS3BackupMode :: Encode RedshiftS3BackupMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource is already in use and not available for this operation.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _
derive instance repGenericResourceInUseException :: Generic ResourceInUseException _
instance showResourceInUseException :: Show ResourceInUseException where
  show = genericShow
instance decodeResourceInUseException :: Decode ResourceInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceInUseException :: Encode ResourceInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified resource could not be found.</p>
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


newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _
derive instance repGenericRoleARN :: Generic RoleARN _
instance showRoleARN :: Show RoleARN where
  show = genericShow
instance decodeRoleARN :: Decode RoleARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleARN :: Encode RoleARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3BackupMode = S3BackupMode String
derive instance newtypeS3BackupMode :: Newtype S3BackupMode _
derive instance repGenericS3BackupMode :: Generic S3BackupMode _
instance showS3BackupMode :: Show S3BackupMode where
  show = genericShow
instance decodeS3BackupMode :: Decode S3BackupMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3BackupMode :: Encode S3BackupMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the configuration of a destination in Amazon S3.</p>
newtype S3DestinationConfiguration = S3DestinationConfiguration 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "BufferingHints" :: NullOrUndefined.NullOrUndefined (BufferingHints)
  , "CompressionFormat" :: NullOrUndefined.NullOrUndefined (CompressionFormat)
  , "EncryptionConfiguration" :: NullOrUndefined.NullOrUndefined (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeS3DestinationConfiguration :: Newtype S3DestinationConfiguration _
derive instance repGenericS3DestinationConfiguration :: Generic S3DestinationConfiguration _
instance showS3DestinationConfiguration :: Show S3DestinationConfiguration where
  show = genericShow
instance decodeS3DestinationConfiguration :: Decode S3DestinationConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3DestinationConfiguration :: Encode S3DestinationConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a destination in Amazon S3.</p>
newtype S3DestinationDescription = S3DestinationDescription 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "BufferingHints" :: (BufferingHints)
  , "CompressionFormat" :: (CompressionFormat)
  , "EncryptionConfiguration" :: (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeS3DestinationDescription :: Newtype S3DestinationDescription _
derive instance repGenericS3DestinationDescription :: Generic S3DestinationDescription _
instance showS3DestinationDescription :: Show S3DestinationDescription where
  show = genericShow
instance decodeS3DestinationDescription :: Decode S3DestinationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3DestinationDescription :: Encode S3DestinationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an update for a destination in Amazon S3.</p>
newtype S3DestinationUpdate = S3DestinationUpdate 
  { "RoleARN" :: NullOrUndefined.NullOrUndefined (RoleARN)
  , "BucketARN" :: NullOrUndefined.NullOrUndefined (BucketARN)
  , "Prefix" :: NullOrUndefined.NullOrUndefined (Prefix)
  , "BufferingHints" :: NullOrUndefined.NullOrUndefined (BufferingHints)
  , "CompressionFormat" :: NullOrUndefined.NullOrUndefined (CompressionFormat)
  , "EncryptionConfiguration" :: NullOrUndefined.NullOrUndefined (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeS3DestinationUpdate :: Newtype S3DestinationUpdate _
derive instance repGenericS3DestinationUpdate :: Generic S3DestinationUpdate _
instance showS3DestinationUpdate :: Show S3DestinationUpdate where
  show = genericShow
instance decodeS3DestinationUpdate :: Decode S3DestinationUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3DestinationUpdate :: Encode S3DestinationUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service is unavailable, back off and retry the operation. If you continue to see the exception, throughput limits for the delivery stream may have been exceeded. For more information about limits and how to request an increase, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SizeInMBs = SizeInMBs Int
derive instance newtypeSizeInMBs :: Newtype SizeInMBs _
derive instance repGenericSizeInMBs :: Generic SizeInMBs _
instance showSizeInMBs :: Show SizeInMBs where
  show = genericShow
instance decodeSizeInMBs :: Decode SizeInMBs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeInMBs :: Encode SizeInMBs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Details about a Kinesis stream used as the source for a Kinesis Firehose delivery stream.</p>
newtype SourceDescription = SourceDescription 
  { "KinesisStreamSourceDescription" :: NullOrUndefined.NullOrUndefined (KinesisStreamSourceDescription)
  }
derive instance newtypeSourceDescription :: Newtype SourceDescription _
derive instance repGenericSourceDescription :: Generic SourceDescription _
instance showSourceDescription :: Show SourceDescription where
  show = genericShow
instance decodeSourceDescription :: Decode SourceDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceDescription :: Encode SourceDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the configuration of a destination in Splunk.</p>
newtype SplunkDestinationConfiguration = SplunkDestinationConfiguration 
  { "HECEndpoint" :: (HECEndpoint)
  , "HECEndpointType" :: (HECEndpointType)
  , "HECToken" :: (HECToken)
  , "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined.NullOrUndefined (HECAcknowledgmentTimeoutInSeconds)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (SplunkRetryOptions)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (SplunkS3BackupMode)
  , "S3Configuration" :: (S3DestinationConfiguration)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeSplunkDestinationConfiguration :: Newtype SplunkDestinationConfiguration _
derive instance repGenericSplunkDestinationConfiguration :: Generic SplunkDestinationConfiguration _
instance showSplunkDestinationConfiguration :: Show SplunkDestinationConfiguration where
  show = genericShow
instance decodeSplunkDestinationConfiguration :: Decode SplunkDestinationConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSplunkDestinationConfiguration :: Encode SplunkDestinationConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a destination in Splunk.</p>
newtype SplunkDestinationDescription = SplunkDestinationDescription 
  { "HECEndpoint" :: NullOrUndefined.NullOrUndefined (HECEndpoint)
  , "HECEndpointType" :: NullOrUndefined.NullOrUndefined (HECEndpointType)
  , "HECToken" :: NullOrUndefined.NullOrUndefined (HECToken)
  , "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined.NullOrUndefined (HECAcknowledgmentTimeoutInSeconds)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (SplunkRetryOptions)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (SplunkS3BackupMode)
  , "S3DestinationDescription" :: NullOrUndefined.NullOrUndefined (S3DestinationDescription)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeSplunkDestinationDescription :: Newtype SplunkDestinationDescription _
derive instance repGenericSplunkDestinationDescription :: Generic SplunkDestinationDescription _
instance showSplunkDestinationDescription :: Show SplunkDestinationDescription where
  show = genericShow
instance decodeSplunkDestinationDescription :: Decode SplunkDestinationDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSplunkDestinationDescription :: Encode SplunkDestinationDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes an update for a destination in Splunk.</p>
newtype SplunkDestinationUpdate = SplunkDestinationUpdate 
  { "HECEndpoint" :: NullOrUndefined.NullOrUndefined (HECEndpoint)
  , "HECEndpointType" :: NullOrUndefined.NullOrUndefined (HECEndpointType)
  , "HECToken" :: NullOrUndefined.NullOrUndefined (HECToken)
  , "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined.NullOrUndefined (HECAcknowledgmentTimeoutInSeconds)
  , "RetryOptions" :: NullOrUndefined.NullOrUndefined (SplunkRetryOptions)
  , "S3BackupMode" :: NullOrUndefined.NullOrUndefined (SplunkS3BackupMode)
  , "S3Update" :: NullOrUndefined.NullOrUndefined (S3DestinationUpdate)
  , "ProcessingConfiguration" :: NullOrUndefined.NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined.NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeSplunkDestinationUpdate :: Newtype SplunkDestinationUpdate _
derive instance repGenericSplunkDestinationUpdate :: Generic SplunkDestinationUpdate _
instance showSplunkDestinationUpdate :: Show SplunkDestinationUpdate where
  show = genericShow
instance decodeSplunkDestinationUpdate :: Decode SplunkDestinationUpdate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSplunkDestinationUpdate :: Encode SplunkDestinationUpdate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SplunkRetryDurationInSeconds = SplunkRetryDurationInSeconds Int
derive instance newtypeSplunkRetryDurationInSeconds :: Newtype SplunkRetryDurationInSeconds _
derive instance repGenericSplunkRetryDurationInSeconds :: Generic SplunkRetryDurationInSeconds _
instance showSplunkRetryDurationInSeconds :: Show SplunkRetryDurationInSeconds where
  show = genericShow
instance decodeSplunkRetryDurationInSeconds :: Decode SplunkRetryDurationInSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSplunkRetryDurationInSeconds :: Encode SplunkRetryDurationInSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Splunk or if it doesn't receive an acknowledgment from Splunk.</p>
newtype SplunkRetryOptions = SplunkRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined.NullOrUndefined (SplunkRetryDurationInSeconds)
  }
derive instance newtypeSplunkRetryOptions :: Newtype SplunkRetryOptions _
derive instance repGenericSplunkRetryOptions :: Generic SplunkRetryOptions _
instance showSplunkRetryOptions :: Show SplunkRetryOptions where
  show = genericShow
instance decodeSplunkRetryOptions :: Decode SplunkRetryOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSplunkRetryOptions :: Encode SplunkRetryOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SplunkS3BackupMode = SplunkS3BackupMode String
derive instance newtypeSplunkS3BackupMode :: Newtype SplunkS3BackupMode _
derive instance repGenericSplunkS3BackupMode :: Generic SplunkS3BackupMode _
instance showSplunkS3BackupMode :: Show SplunkS3BackupMode where
  show = genericShow
instance decodeSplunkS3BackupMode :: Decode SplunkS3BackupMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSplunkS3BackupMode :: Encode SplunkS3BackupMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDestinationInput = UpdateDestinationInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "CurrentDeliveryStreamVersionId" :: (DeliveryStreamVersionId)
  , "DestinationId" :: (DestinationId)
  , "S3DestinationUpdate" :: NullOrUndefined.NullOrUndefined (S3DestinationUpdate)
  , "ExtendedS3DestinationUpdate" :: NullOrUndefined.NullOrUndefined (ExtendedS3DestinationUpdate)
  , "RedshiftDestinationUpdate" :: NullOrUndefined.NullOrUndefined (RedshiftDestinationUpdate)
  , "ElasticsearchDestinationUpdate" :: NullOrUndefined.NullOrUndefined (ElasticsearchDestinationUpdate)
  , "SplunkDestinationUpdate" :: NullOrUndefined.NullOrUndefined (SplunkDestinationUpdate)
  }
derive instance newtypeUpdateDestinationInput :: Newtype UpdateDestinationInput _
derive instance repGenericUpdateDestinationInput :: Generic UpdateDestinationInput _
instance showUpdateDestinationInput :: Show UpdateDestinationInput where
  show = genericShow
instance decodeUpdateDestinationInput :: Decode UpdateDestinationInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDestinationInput :: Encode UpdateDestinationInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDestinationOutput = UpdateDestinationOutput Types.NoArguments
derive instance newtypeUpdateDestinationOutput :: Newtype UpdateDestinationOutput _
derive instance repGenericUpdateDestinationOutput :: Generic UpdateDestinationOutput _
instance showUpdateDestinationOutput :: Show UpdateDestinationOutput where
  show = genericShow
instance decodeUpdateDestinationOutput :: Decode UpdateDestinationOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDestinationOutput :: Encode UpdateDestinationOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Username = Username String
derive instance newtypeUsername :: Newtype Username _
derive instance repGenericUsername :: Generic Username _
instance showUsername :: Show Username where
  show = genericShow
instance decodeUsername :: Decode Username where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsername :: Encode Username where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
