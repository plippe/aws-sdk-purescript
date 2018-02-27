

-- | <fullname>Amazon Kinesis Firehose API Reference</fullname> <p>Amazon Kinesis Firehose is a fully managed service that delivers real-time streaming data to destinations such as Amazon Simple Storage Service (Amazon S3), Amazon Elasticsearch Service (Amazon ES), and Amazon Redshift.</p>
module AWS.Firehose where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Firehose" :: String


-- | <p>Creates a delivery stream.</p> <p>By default, you can create up to 20 delivery streams per region.</p> <p>This is an asynchronous operation that immediately returns. The initial status of the delivery stream is <code>CREATING</code>. After the delivery stream is created, its status is <code>ACTIVE</code> and it now accepts data. Attempts to send data to a delivery stream that is not in the <code>ACTIVE</code> state cause an exception. To check the state of a delivery stream, use <a>DescribeDeliveryStream</a>.</p> <p>A Kinesis Firehose delivery stream can be configured to receive records directly from providers using <a>PutRecord</a> or <a>PutRecordBatch</a>, or it can be configured to use an existing Kinesis stream as its source. To specify a Kinesis stream as input, set the <code>DeliveryStreamType</code> parameter to <code>KinesisStreamAsSource</code>, and provide the Kinesis stream ARN and role ARN in the <code>KinesisStreamSourceConfiguration</code> parameter.</p> <p>A delivery stream is configured with a single destination: Amazon S3, Amazon ES, or Amazon Redshift. You must specify only one of the following destination configuration parameters: <b>ExtendedS3DestinationConfiguration</b>, <b>S3DestinationConfiguration</b>, <b>ElasticsearchDestinationConfiguration</b>, or <b>RedshiftDestinationConfiguration</b>.</p> <p>When you specify <b>S3DestinationConfiguration</b>, you can also provide the following optional values: <b>BufferingHints</b>, <b>EncryptionConfiguration</b>, and <b>CompressionFormat</b>. By default, if no <b>BufferingHints</b> value is provided, Kinesis Firehose buffers data up to 5 MB or for 5 minutes, whichever condition is satisfied first. Note that <b>BufferingHints</b> is a hint, so there are some cases where the service cannot adhere to these conditions strictly; for example, record boundaries are such that the size is a little over or under the configured buffering size. By default, no encryption is performed. We strongly recommend that you enable encryption to ensure secure data storage in Amazon S3.</p> <p>A few notes about Amazon Redshift as a destination:</p> <ul> <li> <p>An Amazon Redshift destination requires an S3 bucket as intermediate location, as Kinesis Firehose first delivers data to S3 and then uses <code>COPY</code> syntax to load data into an Amazon Redshift table. This is specified in the <b>RedshiftDestinationConfiguration.S3Configuration</b> parameter.</p> </li> <li> <p>The compression formats <code>SNAPPY</code> or <code>ZIP</code> cannot be specified in <b>RedshiftDestinationConfiguration.S3Configuration</b> because the Amazon Redshift <code>COPY</code> operation that reads from the S3 bucket doesn't support these compression formats.</p> </li> <li> <p>We strongly recommend that you use the user name and password you provide exclusively with Kinesis Firehose, and that the permissions for the account are restricted for Amazon Redshift <code>INSERT</code> permissions.</p> </li> </ul> <p>Kinesis Firehose assumes the IAM role that is configured as part of the destination. The role should allow the Kinesis Firehose principal to assume the role, and the role should have permissions that allow the service to deliver the data. For more information, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3">Amazon S3 Bucket Access</a> in the <i>Amazon Kinesis Firehose Developer Guide</i>.</p>
createDeliveryStream :: forall eff. CreateDeliveryStreamInput -> Aff (err :: AWS.RequestError | eff) CreateDeliveryStreamOutput
createDeliveryStream = AWS.request serviceName "createDeliveryStream" 


-- | <p>Deletes a delivery stream and its data.</p> <p>You can delete a delivery stream only if it is in <code>ACTIVE</code> or <code>DELETING</code> state, and not in the <code>CREATING</code> state. While the deletion request is in process, the delivery stream is in the <code>DELETING</code> state.</p> <p>To check the state of a delivery stream, use <a>DescribeDeliveryStream</a>.</p> <p>While the delivery stream is <code>DELETING</code> state, the service may continue to accept the records, but the service doesn't make any guarantees with respect to delivering the data. Therefore, as a best practice, you should first stop any applications that are sending records before deleting a delivery stream.</p>
deleteDeliveryStream :: forall eff. DeleteDeliveryStreamInput -> Aff (err :: AWS.RequestError | eff) DeleteDeliveryStreamOutput
deleteDeliveryStream = AWS.request serviceName "deleteDeliveryStream" 


-- | <p>Describes the specified delivery stream and gets the status. For example, after your delivery stream is created, call <a>DescribeDeliveryStream</a> to see if the delivery stream is <code>ACTIVE</code> and therefore ready for data to be sent to it.</p>
describeDeliveryStream :: forall eff. DescribeDeliveryStreamInput -> Aff (err :: AWS.RequestError | eff) DescribeDeliveryStreamOutput
describeDeliveryStream = AWS.request serviceName "describeDeliveryStream" 


-- | <p>Lists your delivery streams.</p> <p>The number of delivery streams might be too large to return using a single call to <a>ListDeliveryStreams</a>. You can limit the number of delivery streams returned, using the <b>Limit</b> parameter. To determine whether there are more delivery streams to list, check the value of <b>HasMoreDeliveryStreams</b> in the output. If there are more delivery streams to list, you can request them by specifying the name of the last delivery stream returned in the call in the <b>ExclusiveStartDeliveryStreamName</b> parameter of a subsequent call.</p>
listDeliveryStreams :: forall eff. ListDeliveryStreamsInput -> Aff (err :: AWS.RequestError | eff) ListDeliveryStreamsOutput
listDeliveryStreams = AWS.request serviceName "listDeliveryStreams" 


-- | <p>Writes a single data record into an Amazon Kinesis Firehose delivery stream. To write multiple data records into a delivery stream, use <a>PutRecordBatch</a>. Applications using these operations are referred to as producers.</p> <p>By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. Note that if you use <a>PutRecord</a> and <a>PutRecordBatch</a>, the limits are an aggregate across these two operations for each delivery stream. For more information about limits and how to request an increase, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>. </p> <p>You must specify the name of the delivery stream and the data record when using <a>PutRecord</a>. The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data, for example, a segment from a log file, geographic location data, website clickstream data, and so on.</p> <p>Kinesis Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (<code>\n</code>) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.</p> <p>The <a>PutRecord</a> operation returns a <b>RecordId</b>, which is a unique string assigned to each record. Producer applications can use this ID for purposes such as auditability and investigation.</p> <p>If the <a>PutRecord</a> operation throws a <b>ServiceUnavailableException</b>, back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream. </p> <p>Data records sent to Kinesis Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.</p>
putRecord :: forall eff. PutRecordInput -> Aff (err :: AWS.RequestError | eff) PutRecordOutput
putRecord = AWS.request serviceName "putRecord" 


-- | <p>Writes multiple data records into a delivery stream in a single call, which can achieve higher throughput per producer than when writing single records. To write single data records into a delivery stream, use <a>PutRecord</a>. Applications using these operations are referred to as producers.</p> <p>By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. If you use <a>PutRecord</a> and <a>PutRecordBatch</a>, the limits are an aggregate across these two operations for each delivery stream. For more information about limits, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>.</p> <p>Each <a>PutRecordBatch</a> request supports up to 500 records. Each record in the request can be as large as 1,000 KB (before 64-bit encoding), up to a limit of 4 MB for the entire request. These limits cannot be changed.</p> <p>You must specify the name of the delivery stream and the data record when using <a>PutRecord</a>. The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data. For example, it could be a segment from a log file, geographic location data, web site clickstream data, and so on.</p> <p>Kinesis Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (<code>\n</code>) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.</p> <p>The <a>PutRecordBatch</a> response includes a count of failed records, <b>FailedPutCount</b>, and an array of responses, <b>RequestResponses</b>. Each entry in the <b>RequestResponses</b> array provides additional information about the processed record. It directly correlates with a record in the request array using the same ordering, from the top to the bottom. The response array always includes the same number of records as the request array. <b>RequestResponses</b> includes both successfully and unsuccessfully processed records. Kinesis Firehose attempts to process all records in each <a>PutRecordBatch</a> request. A single record failure does not stop the processing of subsequent records.</p> <p>A successfully processed record includes a <b>RecordId</b> value, which is unique for the record. An unsuccessfully processed record includes <b>ErrorCode</b> and <b>ErrorMessage</b> values. <b>ErrorCode</b> reflects the type of error, and is one of the following values: <code>ServiceUnavailable</code> or <code>InternalFailure</code>. <b>ErrorMessage</b> provides more detailed information about the error.</p> <p>If there is an internal server error or a timeout, the write might have completed or it might have failed. If <b>FailedPutCount</b> is greater than 0, retry the request, resending only those records that might have failed processing. This minimizes the possible duplicate records and also reduces the total bytes sent (and corresponding charges). We recommend that you handle any duplicates at the destination.</p> <p>If <a>PutRecordBatch</a> throws <b>ServiceUnavailableException</b>, back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream.</p> <p>Data records sent to Kinesis Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.</p>
putRecordBatch :: forall eff. PutRecordBatchInput -> Aff (err :: AWS.RequestError | eff) PutRecordBatchOutput
putRecordBatch = AWS.request serviceName "putRecordBatch" 


-- | <p>Updates the specified destination of the specified delivery stream.</p> <p>You can use this operation to change the destination type (for example, to replace the Amazon S3 destination with Amazon Redshift) or change the parameters associated with a destination (for example, to change the bucket name of the Amazon S3 destination). The update might not occur immediately. The target delivery stream remains active while the configurations are updated, so data writes to the delivery stream can continue during this process. The updated configurations are usually effective within a few minutes.</p> <p>Note that switching between Amazon ES and other services is not supported. For an Amazon ES destination, you can only update to another Amazon ES destination.</p> <p>If the destination type is the same, Kinesis Firehose merges the configuration parameters specified with the destination configuration that already exists on the delivery stream. If any of the parameters are not specified in the call, the existing values are retained. For example, in the Amazon S3 destination, if <a>EncryptionConfiguration</a> is not specified, then the existing <a>EncryptionConfiguration</a> is maintained on the destination.</p> <p>If the destination type is not the same, for example, changing the destination from Amazon S3 to Amazon Redshift, Kinesis Firehose does not merge any parameters. In this case, all parameters must be specified.</p> <p>Kinesis Firehose uses <b>CurrentDeliveryStreamVersionId</b> to avoid race conditions and conflicting merges. This is a required field, and the service updates the configuration only if the existing configuration has a version ID that matches. After the update is applied successfully, the version ID is updated, and can be retrieved using <a>DescribeDeliveryStream</a>. Use the new version ID to set <b>CurrentDeliveryStreamVersionId</b> in the next call.</p>
updateDestination :: forall eff. UpdateDestinationInput -> Aff (err :: AWS.RequestError | eff) UpdateDestinationOutput
updateDestination = AWS.request serviceName "updateDestination" 


newtype AWSKMSKeyARN = AWSKMSKeyARN String
derive instance newtypeAWSKMSKeyARN :: Newtype AWSKMSKeyARN _


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _


newtype BucketARN = BucketARN String
derive instance newtypeBucketARN :: Newtype BucketARN _


-- | <p>Describes hints for the buffering to perform before delivering data to the destination. Please note that these options are treated as hints, and therefore Kinesis Firehose may choose to use different values when it is optimal.</p>
newtype BufferingHints = BufferingHints 
  { "SizeInMBs" :: NullOrUndefined (SizeInMBs)
  , "IntervalInSeconds" :: NullOrUndefined (IntervalInSeconds)
  }
derive instance newtypeBufferingHints :: Newtype BufferingHints _


-- | <p>Describes the Amazon CloudWatch logging options for your delivery stream.</p>
newtype CloudWatchLoggingOptions = CloudWatchLoggingOptions 
  { "Enabled" :: NullOrUndefined (BooleanObject)
  , "LogGroupName" :: NullOrUndefined (LogGroupName)
  , "LogStreamName" :: NullOrUndefined (LogStreamName)
  }
derive instance newtypeCloudWatchLoggingOptions :: Newtype CloudWatchLoggingOptions _


newtype ClusterJDBCURL = ClusterJDBCURL String
derive instance newtypeClusterJDBCURL :: Newtype ClusterJDBCURL _


newtype CompressionFormat = CompressionFormat String
derive instance newtypeCompressionFormat :: Newtype CompressionFormat _


-- | <p>Another modification has already happened. Fetch <b>VersionId</b> again and use it to update the destination.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


-- | <p>Describes a <code>COPY</code> command for Amazon Redshift.</p>
newtype CopyCommand = CopyCommand 
  { "DataTableName" :: (DataTableName)
  , "DataTableColumns" :: NullOrUndefined (DataTableColumns)
  , "CopyOptions" :: NullOrUndefined (CopyOptions)
  }
derive instance newtypeCopyCommand :: Newtype CopyCommand _


newtype CopyOptions = CopyOptions String
derive instance newtypeCopyOptions :: Newtype CopyOptions _


newtype CreateDeliveryStreamInput = CreateDeliveryStreamInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "DeliveryStreamType" :: NullOrUndefined (DeliveryStreamType)
  , "KinesisStreamSourceConfiguration" :: NullOrUndefined (KinesisStreamSourceConfiguration)
  , "S3DestinationConfiguration" :: NullOrUndefined (S3DestinationConfiguration)
  , "ExtendedS3DestinationConfiguration" :: NullOrUndefined (ExtendedS3DestinationConfiguration)
  , "RedshiftDestinationConfiguration" :: NullOrUndefined (RedshiftDestinationConfiguration)
  , "ElasticsearchDestinationConfiguration" :: NullOrUndefined (ElasticsearchDestinationConfiguration)
  , "SplunkDestinationConfiguration" :: NullOrUndefined (SplunkDestinationConfiguration)
  }
derive instance newtypeCreateDeliveryStreamInput :: Newtype CreateDeliveryStreamInput _


newtype CreateDeliveryStreamOutput = CreateDeliveryStreamOutput 
  { "DeliveryStreamARN" :: NullOrUndefined (DeliveryStreamARN)
  }
derive instance newtypeCreateDeliveryStreamOutput :: Newtype CreateDeliveryStreamOutput _


newtype Data = Data String
derive instance newtypeData :: Newtype Data _


newtype DataTableColumns = DataTableColumns String
derive instance newtypeDataTableColumns :: Newtype DataTableColumns _


newtype DataTableName = DataTableName String
derive instance newtypeDataTableName :: Newtype DataTableName _


newtype DeleteDeliveryStreamInput = DeleteDeliveryStreamInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  }
derive instance newtypeDeleteDeliveryStreamInput :: Newtype DeleteDeliveryStreamInput _


newtype DeleteDeliveryStreamOutput = DeleteDeliveryStreamOutput 
  { 
  }
derive instance newtypeDeleteDeliveryStreamOutput :: Newtype DeleteDeliveryStreamOutput _


newtype DeliveryStartTimestamp = DeliveryStartTimestamp Number
derive instance newtypeDeliveryStartTimestamp :: Newtype DeliveryStartTimestamp _


newtype DeliveryStreamARN = DeliveryStreamARN String
derive instance newtypeDeliveryStreamARN :: Newtype DeliveryStreamARN _


-- | <p>Contains information about a delivery stream.</p>
newtype DeliveryStreamDescription = DeliveryStreamDescription 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "DeliveryStreamARN" :: (DeliveryStreamARN)
  , "DeliveryStreamStatus" :: (DeliveryStreamStatus)
  , "DeliveryStreamType" :: (DeliveryStreamType)
  , "VersionId" :: (DeliveryStreamVersionId)
  , "CreateTimestamp" :: NullOrUndefined (Number)
  , "LastUpdateTimestamp" :: NullOrUndefined (Number)
  , "Source" :: NullOrUndefined (SourceDescription)
  , "Destinations" :: (DestinationDescriptionList)
  , "HasMoreDestinations" :: (BooleanObject)
  }
derive instance newtypeDeliveryStreamDescription :: Newtype DeliveryStreamDescription _


newtype DeliveryStreamName = DeliveryStreamName String
derive instance newtypeDeliveryStreamName :: Newtype DeliveryStreamName _


newtype DeliveryStreamNameList = DeliveryStreamNameList (Array DeliveryStreamName)
derive instance newtypeDeliveryStreamNameList :: Newtype DeliveryStreamNameList _


newtype DeliveryStreamStatus = DeliveryStreamStatus String
derive instance newtypeDeliveryStreamStatus :: Newtype DeliveryStreamStatus _


newtype DeliveryStreamType = DeliveryStreamType String
derive instance newtypeDeliveryStreamType :: Newtype DeliveryStreamType _


newtype DeliveryStreamVersionId = DeliveryStreamVersionId String
derive instance newtypeDeliveryStreamVersionId :: Newtype DeliveryStreamVersionId _


newtype DescribeDeliveryStreamInput = DescribeDeliveryStreamInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Limit" :: NullOrUndefined (DescribeDeliveryStreamInputLimit)
  , "ExclusiveStartDestinationId" :: NullOrUndefined (DestinationId)
  }
derive instance newtypeDescribeDeliveryStreamInput :: Newtype DescribeDeliveryStreamInput _


newtype DescribeDeliveryStreamInputLimit = DescribeDeliveryStreamInputLimit Int
derive instance newtypeDescribeDeliveryStreamInputLimit :: Newtype DescribeDeliveryStreamInputLimit _


newtype DescribeDeliveryStreamOutput = DescribeDeliveryStreamOutput 
  { "DeliveryStreamDescription" :: (DeliveryStreamDescription)
  }
derive instance newtypeDescribeDeliveryStreamOutput :: Newtype DescribeDeliveryStreamOutput _


-- | <p>Describes the destination for a delivery stream.</p>
newtype DestinationDescription = DestinationDescription 
  { "DestinationId" :: (DestinationId)
  , "S3DestinationDescription" :: NullOrUndefined (S3DestinationDescription)
  , "ExtendedS3DestinationDescription" :: NullOrUndefined (ExtendedS3DestinationDescription)
  , "RedshiftDestinationDescription" :: NullOrUndefined (RedshiftDestinationDescription)
  , "ElasticsearchDestinationDescription" :: NullOrUndefined (ElasticsearchDestinationDescription)
  , "SplunkDestinationDescription" :: NullOrUndefined (SplunkDestinationDescription)
  }
derive instance newtypeDestinationDescription :: Newtype DestinationDescription _


newtype DestinationDescriptionList = DestinationDescriptionList (Array DestinationDescription)
derive instance newtypeDestinationDescriptionList :: Newtype DestinationDescriptionList _


newtype DestinationId = DestinationId String
derive instance newtypeDestinationId :: Newtype DestinationId _


-- | <p>Describes the buffering to perform before delivering data to the Amazon ES destination.</p>
newtype ElasticsearchBufferingHints = ElasticsearchBufferingHints 
  { "IntervalInSeconds" :: NullOrUndefined (ElasticsearchBufferingIntervalInSeconds)
  , "SizeInMBs" :: NullOrUndefined (ElasticsearchBufferingSizeInMBs)
  }
derive instance newtypeElasticsearchBufferingHints :: Newtype ElasticsearchBufferingHints _


newtype ElasticsearchBufferingIntervalInSeconds = ElasticsearchBufferingIntervalInSeconds Int
derive instance newtypeElasticsearchBufferingIntervalInSeconds :: Newtype ElasticsearchBufferingIntervalInSeconds _


newtype ElasticsearchBufferingSizeInMBs = ElasticsearchBufferingSizeInMBs Int
derive instance newtypeElasticsearchBufferingSizeInMBs :: Newtype ElasticsearchBufferingSizeInMBs _


-- | <p>Describes the configuration of a destination in Amazon ES.</p>
newtype ElasticsearchDestinationConfiguration = ElasticsearchDestinationConfiguration 
  { "RoleARN" :: (RoleARN)
  , "DomainARN" :: (ElasticsearchDomainARN)
  , "IndexName" :: (ElasticsearchIndexName)
  , "TypeName" :: (ElasticsearchTypeName)
  , "IndexRotationPeriod" :: NullOrUndefined (ElasticsearchIndexRotationPeriod)
  , "BufferingHints" :: NullOrUndefined (ElasticsearchBufferingHints)
  , "RetryOptions" :: NullOrUndefined (ElasticsearchRetryOptions)
  , "S3BackupMode" :: NullOrUndefined (ElasticsearchS3BackupMode)
  , "S3Configuration" :: (S3DestinationConfiguration)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeElasticsearchDestinationConfiguration :: Newtype ElasticsearchDestinationConfiguration _


-- | <p>The destination description in Amazon ES.</p>
newtype ElasticsearchDestinationDescription = ElasticsearchDestinationDescription 
  { "RoleARN" :: NullOrUndefined (RoleARN)
  , "DomainARN" :: NullOrUndefined (ElasticsearchDomainARN)
  , "IndexName" :: NullOrUndefined (ElasticsearchIndexName)
  , "TypeName" :: NullOrUndefined (ElasticsearchTypeName)
  , "IndexRotationPeriod" :: NullOrUndefined (ElasticsearchIndexRotationPeriod)
  , "BufferingHints" :: NullOrUndefined (ElasticsearchBufferingHints)
  , "RetryOptions" :: NullOrUndefined (ElasticsearchRetryOptions)
  , "S3BackupMode" :: NullOrUndefined (ElasticsearchS3BackupMode)
  , "S3DestinationDescription" :: NullOrUndefined (S3DestinationDescription)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeElasticsearchDestinationDescription :: Newtype ElasticsearchDestinationDescription _


-- | <p>Describes an update for a destination in Amazon ES.</p>
newtype ElasticsearchDestinationUpdate = ElasticsearchDestinationUpdate 
  { "RoleARN" :: NullOrUndefined (RoleARN)
  , "DomainARN" :: NullOrUndefined (ElasticsearchDomainARN)
  , "IndexName" :: NullOrUndefined (ElasticsearchIndexName)
  , "TypeName" :: NullOrUndefined (ElasticsearchTypeName)
  , "IndexRotationPeriod" :: NullOrUndefined (ElasticsearchIndexRotationPeriod)
  , "BufferingHints" :: NullOrUndefined (ElasticsearchBufferingHints)
  , "RetryOptions" :: NullOrUndefined (ElasticsearchRetryOptions)
  , "S3Update" :: NullOrUndefined (S3DestinationUpdate)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeElasticsearchDestinationUpdate :: Newtype ElasticsearchDestinationUpdate _


newtype ElasticsearchDomainARN = ElasticsearchDomainARN String
derive instance newtypeElasticsearchDomainARN :: Newtype ElasticsearchDomainARN _


newtype ElasticsearchIndexName = ElasticsearchIndexName String
derive instance newtypeElasticsearchIndexName :: Newtype ElasticsearchIndexName _


newtype ElasticsearchIndexRotationPeriod = ElasticsearchIndexRotationPeriod String
derive instance newtypeElasticsearchIndexRotationPeriod :: Newtype ElasticsearchIndexRotationPeriod _


newtype ElasticsearchRetryDurationInSeconds = ElasticsearchRetryDurationInSeconds Int
derive instance newtypeElasticsearchRetryDurationInSeconds :: Newtype ElasticsearchRetryDurationInSeconds _


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Amazon ES.</p>
newtype ElasticsearchRetryOptions = ElasticsearchRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined (ElasticsearchRetryDurationInSeconds)
  }
derive instance newtypeElasticsearchRetryOptions :: Newtype ElasticsearchRetryOptions _


newtype ElasticsearchS3BackupMode = ElasticsearchS3BackupMode String
derive instance newtypeElasticsearchS3BackupMode :: Newtype ElasticsearchS3BackupMode _


newtype ElasticsearchTypeName = ElasticsearchTypeName String
derive instance newtypeElasticsearchTypeName :: Newtype ElasticsearchTypeName _


-- | <p>Describes the encryption for a destination in Amazon S3.</p>
newtype EncryptionConfiguration = EncryptionConfiguration 
  { "NoEncryptionConfig" :: NullOrUndefined (NoEncryptionConfig)
  , "KMSEncryptionConfig" :: NullOrUndefined (KMSEncryptionConfig)
  }
derive instance newtypeEncryptionConfiguration :: Newtype EncryptionConfiguration _


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>Describes the configuration of a destination in Amazon S3.</p>
newtype ExtendedS3DestinationConfiguration = ExtendedS3DestinationConfiguration 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "BufferingHints" :: NullOrUndefined (BufferingHints)
  , "CompressionFormat" :: NullOrUndefined (CompressionFormat)
  , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined (S3BackupMode)
  , "S3BackupConfiguration" :: NullOrUndefined (S3DestinationConfiguration)
  }
derive instance newtypeExtendedS3DestinationConfiguration :: Newtype ExtendedS3DestinationConfiguration _


-- | <p>Describes a destination in Amazon S3.</p>
newtype ExtendedS3DestinationDescription = ExtendedS3DestinationDescription 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "BufferingHints" :: (BufferingHints)
  , "CompressionFormat" :: (CompressionFormat)
  , "EncryptionConfiguration" :: (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined (S3BackupMode)
  , "S3BackupDescription" :: NullOrUndefined (S3DestinationDescription)
  }
derive instance newtypeExtendedS3DestinationDescription :: Newtype ExtendedS3DestinationDescription _


-- | <p>Describes an update for a destination in Amazon S3.</p>
newtype ExtendedS3DestinationUpdate = ExtendedS3DestinationUpdate 
  { "RoleARN" :: NullOrUndefined (RoleARN)
  , "BucketARN" :: NullOrUndefined (BucketARN)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "BufferingHints" :: NullOrUndefined (BufferingHints)
  , "CompressionFormat" :: NullOrUndefined (CompressionFormat)
  , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined (S3BackupMode)
  , "S3BackupUpdate" :: NullOrUndefined (S3DestinationUpdate)
  }
derive instance newtypeExtendedS3DestinationUpdate :: Newtype ExtendedS3DestinationUpdate _


newtype HECAcknowledgmentTimeoutInSeconds = HECAcknowledgmentTimeoutInSeconds Int
derive instance newtypeHECAcknowledgmentTimeoutInSeconds :: Newtype HECAcknowledgmentTimeoutInSeconds _


newtype HECEndpoint = HECEndpoint String
derive instance newtypeHECEndpoint :: Newtype HECEndpoint _


newtype HECEndpointType = HECEndpointType String
derive instance newtypeHECEndpointType :: Newtype HECEndpointType _


newtype HECToken = HECToken String
derive instance newtypeHECToken :: Newtype HECToken _


newtype IntervalInSeconds = IntervalInSeconds Int
derive instance newtypeIntervalInSeconds :: Newtype IntervalInSeconds _


-- | <p>The specified input parameter has a value that is not valid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _


-- | <p>Describes an encryption key for a destination in Amazon S3.</p>
newtype KMSEncryptionConfig = KMSEncryptionConfig 
  { "AWSKMSKeyARN" :: (AWSKMSKeyARN)
  }
derive instance newtypeKMSEncryptionConfig :: Newtype KMSEncryptionConfig _


newtype KinesisStreamARN = KinesisStreamARN String
derive instance newtypeKinesisStreamARN :: Newtype KinesisStreamARN _


-- | <p>The stream and role ARNs for a Kinesis stream used as the source for a delivery stream.</p>
newtype KinesisStreamSourceConfiguration = KinesisStreamSourceConfiguration 
  { "KinesisStreamARN" :: (KinesisStreamARN)
  , "RoleARN" :: (RoleARN)
  }
derive instance newtypeKinesisStreamSourceConfiguration :: Newtype KinesisStreamSourceConfiguration _


-- | <p>Details about a Kinesis stream used as the source for a Kinesis Firehose delivery stream.</p>
newtype KinesisStreamSourceDescription = KinesisStreamSourceDescription 
  { "KinesisStreamARN" :: NullOrUndefined (KinesisStreamARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "DeliveryStartTimestamp" :: NullOrUndefined (DeliveryStartTimestamp)
  }
derive instance newtypeKinesisStreamSourceDescription :: Newtype KinesisStreamSourceDescription _


-- | <p>You have already reached the limit for a requested resource.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListDeliveryStreamsInput = ListDeliveryStreamsInput 
  { "Limit" :: NullOrUndefined (ListDeliveryStreamsInputLimit)
  , "DeliveryStreamType" :: NullOrUndefined (DeliveryStreamType)
  , "ExclusiveStartDeliveryStreamName" :: NullOrUndefined (DeliveryStreamName)
  }
derive instance newtypeListDeliveryStreamsInput :: Newtype ListDeliveryStreamsInput _


newtype ListDeliveryStreamsInputLimit = ListDeliveryStreamsInputLimit Int
derive instance newtypeListDeliveryStreamsInputLimit :: Newtype ListDeliveryStreamsInputLimit _


newtype ListDeliveryStreamsOutput = ListDeliveryStreamsOutput 
  { "DeliveryStreamNames" :: (DeliveryStreamNameList)
  , "HasMoreDeliveryStreams" :: (BooleanObject)
  }
derive instance newtypeListDeliveryStreamsOutput :: Newtype ListDeliveryStreamsOutput _


newtype LogGroupName = LogGroupName String
derive instance newtypeLogGroupName :: Newtype LogGroupName _


newtype LogStreamName = LogStreamName String
derive instance newtypeLogStreamName :: Newtype LogStreamName _


newtype NoEncryptionConfig = NoEncryptionConfig String
derive instance newtypeNoEncryptionConfig :: Newtype NoEncryptionConfig _


newtype NonNegativeIntegerObject = NonNegativeIntegerObject Int
derive instance newtypeNonNegativeIntegerObject :: Newtype NonNegativeIntegerObject _


newtype Password = Password String
derive instance newtypePassword :: Newtype Password _


newtype Prefix = Prefix String
derive instance newtypePrefix :: Newtype Prefix _


-- | <p>Describes a data processing configuration.</p>
newtype ProcessingConfiguration = ProcessingConfiguration 
  { "Enabled" :: NullOrUndefined (BooleanObject)
  , "Processors" :: NullOrUndefined (ProcessorList)
  }
derive instance newtypeProcessingConfiguration :: Newtype ProcessingConfiguration _


-- | <p>Describes a data processor.</p>
newtype Processor = Processor 
  { "Type" :: (ProcessorType)
  , "Parameters" :: NullOrUndefined (ProcessorParameterList)
  }
derive instance newtypeProcessor :: Newtype Processor _


newtype ProcessorList = ProcessorList (Array Processor)
derive instance newtypeProcessorList :: Newtype ProcessorList _


-- | <p>Describes the processor parameter.</p>
newtype ProcessorParameter = ProcessorParameter 
  { "ParameterName" :: (ProcessorParameterName)
  , "ParameterValue" :: (ProcessorParameterValue)
  }
derive instance newtypeProcessorParameter :: Newtype ProcessorParameter _


newtype ProcessorParameterList = ProcessorParameterList (Array ProcessorParameter)
derive instance newtypeProcessorParameterList :: Newtype ProcessorParameterList _


newtype ProcessorParameterName = ProcessorParameterName String
derive instance newtypeProcessorParameterName :: Newtype ProcessorParameterName _


newtype ProcessorParameterValue = ProcessorParameterValue String
derive instance newtypeProcessorParameterValue :: Newtype ProcessorParameterValue _


newtype ProcessorType = ProcessorType String
derive instance newtypeProcessorType :: Newtype ProcessorType _


newtype PutRecordBatchInput = PutRecordBatchInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Records" :: (PutRecordBatchRequestEntryList)
  }
derive instance newtypePutRecordBatchInput :: Newtype PutRecordBatchInput _


newtype PutRecordBatchOutput = PutRecordBatchOutput 
  { "FailedPutCount" :: (NonNegativeIntegerObject)
  , "RequestResponses" :: (PutRecordBatchResponseEntryList)
  }
derive instance newtypePutRecordBatchOutput :: Newtype PutRecordBatchOutput _


newtype PutRecordBatchRequestEntryList = PutRecordBatchRequestEntryList (Array Record'')
derive instance newtypePutRecordBatchRequestEntryList :: Newtype PutRecordBatchRequestEntryList _


-- | <p>Contains the result for an individual record from a <a>PutRecordBatch</a> request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.</p>
newtype PutRecordBatchResponseEntry = PutRecordBatchResponseEntry 
  { "RecordId" :: NullOrUndefined (PutResponseRecordId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePutRecordBatchResponseEntry :: Newtype PutRecordBatchResponseEntry _


newtype PutRecordBatchResponseEntryList = PutRecordBatchResponseEntryList (Array PutRecordBatchResponseEntry)
derive instance newtypePutRecordBatchResponseEntryList :: Newtype PutRecordBatchResponseEntryList _


newtype PutRecordInput = PutRecordInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Record''" :: (Record'')
  }
derive instance newtypePutRecordInput :: Newtype PutRecordInput _


newtype PutRecordOutput = PutRecordOutput 
  { "RecordId" :: (PutResponseRecordId)
  }
derive instance newtypePutRecordOutput :: Newtype PutRecordOutput _


newtype PutResponseRecordId = PutResponseRecordId String
derive instance newtypePutResponseRecordId :: Newtype PutResponseRecordId _


-- | <p>The unit of data in a delivery stream.</p>
newtype Record'' = Record'' 
  { "Data" :: (Data)
  }
derive instance newtypeRecord'' :: Newtype Record'' _


-- | <p>Describes the configuration of a destination in Amazon Redshift.</p>
newtype RedshiftDestinationConfiguration = RedshiftDestinationConfiguration 
  { "RoleARN" :: (RoleARN)
  , "ClusterJDBCURL" :: (ClusterJDBCURL)
  , "CopyCommand" :: (CopyCommand)
  , "Username" :: (Username)
  , "Password" :: (Password)
  , "RetryOptions" :: NullOrUndefined (RedshiftRetryOptions)
  , "S3Configuration" :: (S3DestinationConfiguration)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined (RedshiftS3BackupMode)
  , "S3BackupConfiguration" :: NullOrUndefined (S3DestinationConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeRedshiftDestinationConfiguration :: Newtype RedshiftDestinationConfiguration _


-- | <p>Describes a destination in Amazon Redshift.</p>
newtype RedshiftDestinationDescription = RedshiftDestinationDescription 
  { "RoleARN" :: (RoleARN)
  , "ClusterJDBCURL" :: (ClusterJDBCURL)
  , "CopyCommand" :: (CopyCommand)
  , "Username" :: (Username)
  , "RetryOptions" :: NullOrUndefined (RedshiftRetryOptions)
  , "S3DestinationDescription" :: (S3DestinationDescription)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined (RedshiftS3BackupMode)
  , "S3BackupDescription" :: NullOrUndefined (S3DestinationDescription)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeRedshiftDestinationDescription :: Newtype RedshiftDestinationDescription _


-- | <p>Describes an update for a destination in Amazon Redshift.</p>
newtype RedshiftDestinationUpdate = RedshiftDestinationUpdate 
  { "RoleARN" :: NullOrUndefined (RoleARN)
  , "ClusterJDBCURL" :: NullOrUndefined (ClusterJDBCURL)
  , "CopyCommand" :: NullOrUndefined (CopyCommand)
  , "Username" :: NullOrUndefined (Username)
  , "Password" :: NullOrUndefined (Password)
  , "RetryOptions" :: NullOrUndefined (RedshiftRetryOptions)
  , "S3Update" :: NullOrUndefined (S3DestinationUpdate)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "S3BackupMode" :: NullOrUndefined (RedshiftS3BackupMode)
  , "S3BackupUpdate" :: NullOrUndefined (S3DestinationUpdate)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeRedshiftDestinationUpdate :: Newtype RedshiftDestinationUpdate _


newtype RedshiftRetryDurationInSeconds = RedshiftRetryDurationInSeconds Int
derive instance newtypeRedshiftRetryDurationInSeconds :: Newtype RedshiftRetryDurationInSeconds _


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Amazon Redshift.</p>
newtype RedshiftRetryOptions = RedshiftRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined (RedshiftRetryDurationInSeconds)
  }
derive instance newtypeRedshiftRetryOptions :: Newtype RedshiftRetryOptions _


newtype RedshiftS3BackupMode = RedshiftS3BackupMode String
derive instance newtypeRedshiftS3BackupMode :: Newtype RedshiftS3BackupMode _


-- | <p>The resource is already in use and not available for this operation.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>The specified resource could not be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype RoleARN = RoleARN String
derive instance newtypeRoleARN :: Newtype RoleARN _


newtype S3BackupMode = S3BackupMode String
derive instance newtypeS3BackupMode :: Newtype S3BackupMode _


-- | <p>Describes the configuration of a destination in Amazon S3.</p>
newtype S3DestinationConfiguration = S3DestinationConfiguration 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "BufferingHints" :: NullOrUndefined (BufferingHints)
  , "CompressionFormat" :: NullOrUndefined (CompressionFormat)
  , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeS3DestinationConfiguration :: Newtype S3DestinationConfiguration _


-- | <p>Describes a destination in Amazon S3.</p>
newtype S3DestinationDescription = S3DestinationDescription 
  { "RoleARN" :: (RoleARN)
  , "BucketARN" :: (BucketARN)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "BufferingHints" :: (BufferingHints)
  , "CompressionFormat" :: (CompressionFormat)
  , "EncryptionConfiguration" :: (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeS3DestinationDescription :: Newtype S3DestinationDescription _


-- | <p>Describes an update for a destination in Amazon S3.</p>
newtype S3DestinationUpdate = S3DestinationUpdate 
  { "RoleARN" :: NullOrUndefined (RoleARN)
  , "BucketARN" :: NullOrUndefined (BucketARN)
  , "Prefix" :: NullOrUndefined (Prefix)
  , "BufferingHints" :: NullOrUndefined (BufferingHints)
  , "CompressionFormat" :: NullOrUndefined (CompressionFormat)
  , "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeS3DestinationUpdate :: Newtype S3DestinationUpdate _


-- | <p>The service is unavailable, back off and retry the operation. If you continue to see the exception, throughput limits for the delivery stream may have been exceeded. For more information about limits and how to request an increase, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


newtype SizeInMBs = SizeInMBs Int
derive instance newtypeSizeInMBs :: Newtype SizeInMBs _


-- | <p>Details about a Kinesis stream used as the source for a Kinesis Firehose delivery stream.</p>
newtype SourceDescription = SourceDescription 
  { "KinesisStreamSourceDescription" :: NullOrUndefined (KinesisStreamSourceDescription)
  }
derive instance newtypeSourceDescription :: Newtype SourceDescription _


-- | <p>Describes the configuration of a destination in Splunk.</p>
newtype SplunkDestinationConfiguration = SplunkDestinationConfiguration 
  { "HECEndpoint" :: (HECEndpoint)
  , "HECEndpointType" :: (HECEndpointType)
  , "HECToken" :: (HECToken)
  , "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined (HECAcknowledgmentTimeoutInSeconds)
  , "RetryOptions" :: NullOrUndefined (SplunkRetryOptions)
  , "S3BackupMode" :: NullOrUndefined (SplunkS3BackupMode)
  , "S3Configuration" :: (S3DestinationConfiguration)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeSplunkDestinationConfiguration :: Newtype SplunkDestinationConfiguration _


-- | <p>Describes a destination in Splunk.</p>
newtype SplunkDestinationDescription = SplunkDestinationDescription 
  { "HECEndpoint" :: NullOrUndefined (HECEndpoint)
  , "HECEndpointType" :: NullOrUndefined (HECEndpointType)
  , "HECToken" :: NullOrUndefined (HECToken)
  , "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined (HECAcknowledgmentTimeoutInSeconds)
  , "RetryOptions" :: NullOrUndefined (SplunkRetryOptions)
  , "S3BackupMode" :: NullOrUndefined (SplunkS3BackupMode)
  , "S3DestinationDescription" :: NullOrUndefined (S3DestinationDescription)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeSplunkDestinationDescription :: Newtype SplunkDestinationDescription _


-- | <p>Describes an update for a destination in Splunk.</p>
newtype SplunkDestinationUpdate = SplunkDestinationUpdate 
  { "HECEndpoint" :: NullOrUndefined (HECEndpoint)
  , "HECEndpointType" :: NullOrUndefined (HECEndpointType)
  , "HECToken" :: NullOrUndefined (HECToken)
  , "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined (HECAcknowledgmentTimeoutInSeconds)
  , "RetryOptions" :: NullOrUndefined (SplunkRetryOptions)
  , "S3BackupMode" :: NullOrUndefined (SplunkS3BackupMode)
  , "S3Update" :: NullOrUndefined (S3DestinationUpdate)
  , "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration)
  , "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions)
  }
derive instance newtypeSplunkDestinationUpdate :: Newtype SplunkDestinationUpdate _


newtype SplunkRetryDurationInSeconds = SplunkRetryDurationInSeconds Int
derive instance newtypeSplunkRetryDurationInSeconds :: Newtype SplunkRetryDurationInSeconds _


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Splunk or if it doesn't receive an acknowledgment from Splunk.</p>
newtype SplunkRetryOptions = SplunkRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined (SplunkRetryDurationInSeconds)
  }
derive instance newtypeSplunkRetryOptions :: Newtype SplunkRetryOptions _


newtype SplunkS3BackupMode = SplunkS3BackupMode String
derive instance newtypeSplunkS3BackupMode :: Newtype SplunkS3BackupMode _


newtype UpdateDestinationInput = UpdateDestinationInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "CurrentDeliveryStreamVersionId" :: (DeliveryStreamVersionId)
  , "DestinationId" :: (DestinationId)
  , "S3DestinationUpdate" :: NullOrUndefined (S3DestinationUpdate)
  , "ExtendedS3DestinationUpdate" :: NullOrUndefined (ExtendedS3DestinationUpdate)
  , "RedshiftDestinationUpdate" :: NullOrUndefined (RedshiftDestinationUpdate)
  , "ElasticsearchDestinationUpdate" :: NullOrUndefined (ElasticsearchDestinationUpdate)
  , "SplunkDestinationUpdate" :: NullOrUndefined (SplunkDestinationUpdate)
  }
derive instance newtypeUpdateDestinationInput :: Newtype UpdateDestinationInput _


newtype UpdateDestinationOutput = UpdateDestinationOutput 
  { 
  }
derive instance newtypeUpdateDestinationOutput :: Newtype UpdateDestinationOutput _


newtype Username = Username String
derive instance newtypeUsername :: Newtype Username _
