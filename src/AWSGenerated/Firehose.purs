

-- | <fullname>Amazon Kinesis Firehose API Reference</fullname> <p>Amazon Kinesis Firehose is a fully managed service that delivers real-time streaming data to destinations such as Amazon Simple Storage Service (Amazon S3), Amazon Elasticsearch Service (Amazon ES), and Amazon Redshift.</p>
module AWS.Firehose where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Firehose" :: String


-- | <p>Creates a delivery stream.</p> <p>By default, you can create up to 20 delivery streams per region.</p> <p>This is an asynchronous operation that immediately returns. The initial status of the delivery stream is <code>CREATING</code>. After the delivery stream is created, its status is <code>ACTIVE</code> and it now accepts data. Attempts to send data to a delivery stream that is not in the <code>ACTIVE</code> state cause an exception. To check the state of a delivery stream, use <a>DescribeDeliveryStream</a>.</p> <p>A Kinesis Firehose delivery stream can be configured to receive records directly from providers using <a>PutRecord</a> or <a>PutRecordBatch</a>, or it can be configured to use an existing Kinesis stream as its source. To specify a Kinesis stream as input, set the <code>DeliveryStreamType</code> parameter to <code>KinesisStreamAsSource</code>, and provide the Kinesis stream ARN and role ARN in the <code>KinesisStreamSourceConfiguration</code> parameter.</p> <p>A delivery stream is configured with a single destination: Amazon S3, Amazon ES, or Amazon Redshift. You must specify only one of the following destination configuration parameters: <b>ExtendedS3DestinationConfiguration</b>, <b>S3DestinationConfiguration</b>, <b>ElasticsearchDestinationConfiguration</b>, or <b>RedshiftDestinationConfiguration</b>.</p> <p>When you specify <b>S3DestinationConfiguration</b>, you can also provide the following optional values: <b>BufferingHints</b>, <b>EncryptionConfiguration</b>, and <b>CompressionFormat</b>. By default, if no <b>BufferingHints</b> value is provided, Kinesis Firehose buffers data up to 5 MB or for 5 minutes, whichever condition is satisfied first. Note that <b>BufferingHints</b> is a hint, so there are some cases where the service cannot adhere to these conditions strictly; for example, record boundaries are such that the size is a little over or under the configured buffering size. By default, no encryption is performed. We strongly recommend that you enable encryption to ensure secure data storage in Amazon S3.</p> <p>A few notes about Amazon Redshift as a destination:</p> <ul> <li> <p>An Amazon Redshift destination requires an S3 bucket as intermediate location, as Kinesis Firehose first delivers data to S3 and then uses <code>COPY</code> syntax to load data into an Amazon Redshift table. This is specified in the <b>RedshiftDestinationConfiguration.S3Configuration</b> parameter.</p> </li> <li> <p>The compression formats <code>SNAPPY</code> or <code>ZIP</code> cannot be specified in <b>RedshiftDestinationConfiguration.S3Configuration</b> because the Amazon Redshift <code>COPY</code> operation that reads from the S3 bucket doesn't support these compression formats.</p> </li> <li> <p>We strongly recommend that you use the user name and password you provide exclusively with Kinesis Firehose, and that the permissions for the account are restricted for Amazon Redshift <code>INSERT</code> permissions.</p> </li> </ul> <p>Kinesis Firehose assumes the IAM role that is configured as part of the destination. The role should allow the Kinesis Firehose principal to assume the role, and the role should have permissions that allow the service to deliver the data. For more information, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3">Amazon S3 Bucket Access</a> in the <i>Amazon Kinesis Firehose Developer Guide</i>.</p>
createDeliveryStream :: forall eff. CreateDeliveryStreamInput -> Aff (err :: AWS.RequestError | eff) CreateDeliveryStreamOutput
createDeliveryStream = AWS.request serviceName "CreateDeliveryStream" 


-- | <p>Deletes a delivery stream and its data.</p> <p>You can delete a delivery stream only if it is in <code>ACTIVE</code> or <code>DELETING</code> state, and not in the <code>CREATING</code> state. While the deletion request is in process, the delivery stream is in the <code>DELETING</code> state.</p> <p>To check the state of a delivery stream, use <a>DescribeDeliveryStream</a>.</p> <p>While the delivery stream is <code>DELETING</code> state, the service may continue to accept the records, but the service doesn't make any guarantees with respect to delivering the data. Therefore, as a best practice, you should first stop any applications that are sending records before deleting a delivery stream.</p>
deleteDeliveryStream :: forall eff. DeleteDeliveryStreamInput -> Aff (err :: AWS.RequestError | eff) DeleteDeliveryStreamOutput
deleteDeliveryStream = AWS.request serviceName "DeleteDeliveryStream" 


-- | <p>Describes the specified delivery stream and gets the status. For example, after your delivery stream is created, call <a>DescribeDeliveryStream</a> to see if the delivery stream is <code>ACTIVE</code> and therefore ready for data to be sent to it.</p>
describeDeliveryStream :: forall eff. DescribeDeliveryStreamInput -> Aff (err :: AWS.RequestError | eff) DescribeDeliveryStreamOutput
describeDeliveryStream = AWS.request serviceName "DescribeDeliveryStream" 


-- | <p>Lists your delivery streams.</p> <p>The number of delivery streams might be too large to return using a single call to <a>ListDeliveryStreams</a>. You can limit the number of delivery streams returned, using the <b>Limit</b> parameter. To determine whether there are more delivery streams to list, check the value of <b>HasMoreDeliveryStreams</b> in the output. If there are more delivery streams to list, you can request them by specifying the name of the last delivery stream returned in the call in the <b>ExclusiveStartDeliveryStreamName</b> parameter of a subsequent call.</p>
listDeliveryStreams :: forall eff. ListDeliveryStreamsInput -> Aff (err :: AWS.RequestError | eff) ListDeliveryStreamsOutput
listDeliveryStreams = AWS.request serviceName "ListDeliveryStreams" 


-- | <p>Writes a single data record into an Amazon Kinesis Firehose delivery stream. To write multiple data records into a delivery stream, use <a>PutRecordBatch</a>. Applications using these operations are referred to as producers.</p> <p>By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. Note that if you use <a>PutRecord</a> and <a>PutRecordBatch</a>, the limits are an aggregate across these two operations for each delivery stream. For more information about limits and how to request an increase, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>. </p> <p>You must specify the name of the delivery stream and the data record when using <a>PutRecord</a>. The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data, for example, a segment from a log file, geographic location data, website clickstream data, and so on.</p> <p>Kinesis Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (<code>\n</code>) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.</p> <p>The <a>PutRecord</a> operation returns a <b>RecordId</b>, which is a unique string assigned to each record. Producer applications can use this ID for purposes such as auditability and investigation.</p> <p>If the <a>PutRecord</a> operation throws a <b>ServiceUnavailableException</b>, back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream. </p> <p>Data records sent to Kinesis Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.</p>
putRecord :: forall eff. PutRecordInput -> Aff (err :: AWS.RequestError | eff) PutRecordOutput
putRecord = AWS.request serviceName "PutRecord" 


-- | <p>Writes multiple data records into a delivery stream in a single call, which can achieve higher throughput per producer than when writing single records. To write single data records into a delivery stream, use <a>PutRecord</a>. Applications using these operations are referred to as producers.</p> <p>By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. If you use <a>PutRecord</a> and <a>PutRecordBatch</a>, the limits are an aggregate across these two operations for each delivery stream. For more information about limits, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>.</p> <p>Each <a>PutRecordBatch</a> request supports up to 500 records. Each record in the request can be as large as 1,000 KB (before 64-bit encoding), up to a limit of 4 MB for the entire request. These limits cannot be changed.</p> <p>You must specify the name of the delivery stream and the data record when using <a>PutRecord</a>. The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data. For example, it could be a segment from a log file, geographic location data, web site clickstream data, and so on.</p> <p>Kinesis Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (<code>\n</code>) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.</p> <p>The <a>PutRecordBatch</a> response includes a count of failed records, <b>FailedPutCount</b>, and an array of responses, <b>RequestResponses</b>. Each entry in the <b>RequestResponses</b> array provides additional information about the processed record. It directly correlates with a record in the request array using the same ordering, from the top to the bottom. The response array always includes the same number of records as the request array. <b>RequestResponses</b> includes both successfully and unsuccessfully processed records. Kinesis Firehose attempts to process all records in each <a>PutRecordBatch</a> request. A single record failure does not stop the processing of subsequent records.</p> <p>A successfully processed record includes a <b>RecordId</b> value, which is unique for the record. An unsuccessfully processed record includes <b>ErrorCode</b> and <b>ErrorMessage</b> values. <b>ErrorCode</b> reflects the type of error, and is one of the following values: <code>ServiceUnavailable</code> or <code>InternalFailure</code>. <b>ErrorMessage</b> provides more detailed information about the error.</p> <p>If there is an internal server error or a timeout, the write might have completed or it might have failed. If <b>FailedPutCount</b> is greater than 0, retry the request, resending only those records that might have failed processing. This minimizes the possible duplicate records and also reduces the total bytes sent (and corresponding charges). We recommend that you handle any duplicates at the destination.</p> <p>If <a>PutRecordBatch</a> throws <b>ServiceUnavailableException</b>, back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream.</p> <p>Data records sent to Kinesis Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.</p>
putRecordBatch :: forall eff. PutRecordBatchInput -> Aff (err :: AWS.RequestError | eff) PutRecordBatchOutput
putRecordBatch = AWS.request serviceName "PutRecordBatch" 


-- | <p>Updates the specified destination of the specified delivery stream.</p> <p>You can use this operation to change the destination type (for example, to replace the Amazon S3 destination with Amazon Redshift) or change the parameters associated with a destination (for example, to change the bucket name of the Amazon S3 destination). The update might not occur immediately. The target delivery stream remains active while the configurations are updated, so data writes to the delivery stream can continue during this process. The updated configurations are usually effective within a few minutes.</p> <p>Note that switching between Amazon ES and other services is not supported. For an Amazon ES destination, you can only update to another Amazon ES destination.</p> <p>If the destination type is the same, Kinesis Firehose merges the configuration parameters specified with the destination configuration that already exists on the delivery stream. If any of the parameters are not specified in the call, the existing values are retained. For example, in the Amazon S3 destination, if <a>EncryptionConfiguration</a> is not specified, then the existing <a>EncryptionConfiguration</a> is maintained on the destination.</p> <p>If the destination type is not the same, for example, changing the destination from Amazon S3 to Amazon Redshift, Kinesis Firehose does not merge any parameters. In this case, all parameters must be specified.</p> <p>Kinesis Firehose uses <b>CurrentDeliveryStreamVersionId</b> to avoid race conditions and conflicting merges. This is a required field, and the service updates the configuration only if the existing configuration has a version ID that matches. After the update is applied successfully, the version ID is updated, and can be retrieved using <a>DescribeDeliveryStream</a>. Use the new version ID to set <b>CurrentDeliveryStreamVersionId</b> in the next call.</p>
updateDestination :: forall eff. UpdateDestinationInput -> Aff (err :: AWS.RequestError | eff) UpdateDestinationOutput
updateDestination = AWS.request serviceName "UpdateDestination" 


newtype AWSKMSKeyARN = AWSKMSKeyARN String


newtype BooleanObject = BooleanObject Boolean


newtype BucketARN = BucketARN String


-- | <p>Describes hints for the buffering to perform before delivering data to the destination. Please note that these options are treated as hints, and therefore Kinesis Firehose may choose to use different values when it is optimal.</p>
newtype BufferingHints = BufferingHints 
  { "SizeInMBs" :: NullOrUndefined (SizeInMBs)
  , "IntervalInSeconds" :: NullOrUndefined (IntervalInSeconds)
  }


-- | <p>Describes the Amazon CloudWatch logging options for your delivery stream.</p>
newtype CloudWatchLoggingOptions = CloudWatchLoggingOptions 
  { "Enabled" :: NullOrUndefined (BooleanObject)
  , "LogGroupName" :: NullOrUndefined (LogGroupName)
  , "LogStreamName" :: NullOrUndefined (LogStreamName)
  }


newtype ClusterJDBCURL = ClusterJDBCURL String


newtype CompressionFormat = CompressionFormat String


-- | <p>Another modification has already happened. Fetch <b>VersionId</b> again and use it to update the destination.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Describes a <code>COPY</code> command for Amazon Redshift.</p>
newtype CopyCommand = CopyCommand 
  { "DataTableName" :: (DataTableName)
  , "DataTableColumns" :: NullOrUndefined (DataTableColumns)
  , "CopyOptions" :: NullOrUndefined (CopyOptions)
  }


newtype CopyOptions = CopyOptions String


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


newtype CreateDeliveryStreamOutput = CreateDeliveryStreamOutput 
  { "DeliveryStreamARN" :: NullOrUndefined (DeliveryStreamARN)
  }


newtype Data = Data String


newtype DataTableColumns = DataTableColumns String


newtype DataTableName = DataTableName String


newtype DeleteDeliveryStreamInput = DeleteDeliveryStreamInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  }


newtype DeleteDeliveryStreamOutput = DeleteDeliveryStreamOutput 
  { 
  }


newtype DeliveryStartTimestamp = DeliveryStartTimestamp Number


newtype DeliveryStreamARN = DeliveryStreamARN String


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


newtype DeliveryStreamName = DeliveryStreamName String


newtype DeliveryStreamNameList = DeliveryStreamNameList (Array DeliveryStreamName)


newtype DeliveryStreamStatus = DeliveryStreamStatus String


newtype DeliveryStreamType = DeliveryStreamType String


newtype DeliveryStreamVersionId = DeliveryStreamVersionId String


newtype DescribeDeliveryStreamInput = DescribeDeliveryStreamInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Limit" :: NullOrUndefined (DescribeDeliveryStreamInputLimit)
  , "ExclusiveStartDestinationId" :: NullOrUndefined (DestinationId)
  }


newtype DescribeDeliveryStreamInputLimit = DescribeDeliveryStreamInputLimit Int


newtype DescribeDeliveryStreamOutput = DescribeDeliveryStreamOutput 
  { "DeliveryStreamDescription" :: (DeliveryStreamDescription)
  }


-- | <p>Describes the destination for a delivery stream.</p>
newtype DestinationDescription = DestinationDescription 
  { "DestinationId" :: (DestinationId)
  , "S3DestinationDescription" :: NullOrUndefined (S3DestinationDescription)
  , "ExtendedS3DestinationDescription" :: NullOrUndefined (ExtendedS3DestinationDescription)
  , "RedshiftDestinationDescription" :: NullOrUndefined (RedshiftDestinationDescription)
  , "ElasticsearchDestinationDescription" :: NullOrUndefined (ElasticsearchDestinationDescription)
  , "SplunkDestinationDescription" :: NullOrUndefined (SplunkDestinationDescription)
  }


newtype DestinationDescriptionList = DestinationDescriptionList (Array DestinationDescription)


newtype DestinationId = DestinationId String


-- | <p>Describes the buffering to perform before delivering data to the Amazon ES destination.</p>
newtype ElasticsearchBufferingHints = ElasticsearchBufferingHints 
  { "IntervalInSeconds" :: NullOrUndefined (ElasticsearchBufferingIntervalInSeconds)
  , "SizeInMBs" :: NullOrUndefined (ElasticsearchBufferingSizeInMBs)
  }


newtype ElasticsearchBufferingIntervalInSeconds = ElasticsearchBufferingIntervalInSeconds Int


newtype ElasticsearchBufferingSizeInMBs = ElasticsearchBufferingSizeInMBs Int


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


newtype ElasticsearchDomainARN = ElasticsearchDomainARN String


newtype ElasticsearchIndexName = ElasticsearchIndexName String


newtype ElasticsearchIndexRotationPeriod = ElasticsearchIndexRotationPeriod String


newtype ElasticsearchRetryDurationInSeconds = ElasticsearchRetryDurationInSeconds Int


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Amazon ES.</p>
newtype ElasticsearchRetryOptions = ElasticsearchRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined (ElasticsearchRetryDurationInSeconds)
  }


newtype ElasticsearchS3BackupMode = ElasticsearchS3BackupMode String


newtype ElasticsearchTypeName = ElasticsearchTypeName String


-- | <p>Describes the encryption for a destination in Amazon S3.</p>
newtype EncryptionConfiguration = EncryptionConfiguration 
  { "NoEncryptionConfig" :: NullOrUndefined (NoEncryptionConfig)
  , "KMSEncryptionConfig" :: NullOrUndefined (KMSEncryptionConfig)
  }


newtype ErrorCode = ErrorCode String


newtype ErrorMessage = ErrorMessage String


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


newtype HECAcknowledgmentTimeoutInSeconds = HECAcknowledgmentTimeoutInSeconds Int


newtype HECEndpoint = HECEndpoint String


newtype HECEndpointType = HECEndpointType String


newtype HECToken = HECToken String


newtype IntervalInSeconds = IntervalInSeconds Int


-- | <p>The specified input parameter has a value that is not valid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Describes an encryption key for a destination in Amazon S3.</p>
newtype KMSEncryptionConfig = KMSEncryptionConfig 
  { "AWSKMSKeyARN" :: (AWSKMSKeyARN)
  }


newtype KinesisStreamARN = KinesisStreamARN String


-- | <p>The stream and role ARNs for a Kinesis stream used as the source for a delivery stream.</p>
newtype KinesisStreamSourceConfiguration = KinesisStreamSourceConfiguration 
  { "KinesisStreamARN" :: (KinesisStreamARN)
  , "RoleARN" :: (RoleARN)
  }


-- | <p>Details about a Kinesis stream used as the source for a Kinesis Firehose delivery stream.</p>
newtype KinesisStreamSourceDescription = KinesisStreamSourceDescription 
  { "KinesisStreamARN" :: NullOrUndefined (KinesisStreamARN)
  , "RoleARN" :: NullOrUndefined (RoleARN)
  , "DeliveryStartTimestamp" :: NullOrUndefined (DeliveryStartTimestamp)
  }


-- | <p>You have already reached the limit for a requested resource.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype ListDeliveryStreamsInput = ListDeliveryStreamsInput 
  { "Limit" :: NullOrUndefined (ListDeliveryStreamsInputLimit)
  , "DeliveryStreamType" :: NullOrUndefined (DeliveryStreamType)
  , "ExclusiveStartDeliveryStreamName" :: NullOrUndefined (DeliveryStreamName)
  }


newtype ListDeliveryStreamsInputLimit = ListDeliveryStreamsInputLimit Int


newtype ListDeliveryStreamsOutput = ListDeliveryStreamsOutput 
  { "DeliveryStreamNames" :: (DeliveryStreamNameList)
  , "HasMoreDeliveryStreams" :: (BooleanObject)
  }


newtype LogGroupName = LogGroupName String


newtype LogStreamName = LogStreamName String


newtype NoEncryptionConfig = NoEncryptionConfig String


newtype NonNegativeIntegerObject = NonNegativeIntegerObject Int


newtype Password = Password String


newtype Prefix = Prefix String


-- | <p>Describes a data processing configuration.</p>
newtype ProcessingConfiguration = ProcessingConfiguration 
  { "Enabled" :: NullOrUndefined (BooleanObject)
  , "Processors" :: NullOrUndefined (ProcessorList)
  }


-- | <p>Describes a data processor.</p>
newtype Processor = Processor 
  { "Type" :: (ProcessorType)
  , "Parameters" :: NullOrUndefined (ProcessorParameterList)
  }


newtype ProcessorList = ProcessorList (Array Processor)


-- | <p>Describes the processor parameter.</p>
newtype ProcessorParameter = ProcessorParameter 
  { "ParameterName" :: (ProcessorParameterName)
  , "ParameterValue" :: (ProcessorParameterValue)
  }


newtype ProcessorParameterList = ProcessorParameterList (Array ProcessorParameter)


newtype ProcessorParameterName = ProcessorParameterName String


newtype ProcessorParameterValue = ProcessorParameterValue String


newtype ProcessorType = ProcessorType String


newtype PutRecordBatchInput = PutRecordBatchInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Records" :: (PutRecordBatchRequestEntryList)
  }


newtype PutRecordBatchOutput = PutRecordBatchOutput 
  { "FailedPutCount" :: (NonNegativeIntegerObject)
  , "RequestResponses" :: (PutRecordBatchResponseEntryList)
  }


newtype PutRecordBatchRequestEntryList = PutRecordBatchRequestEntryList (Array Record'')


-- | <p>Contains the result for an individual record from a <a>PutRecordBatch</a> request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.</p>
newtype PutRecordBatchResponseEntry = PutRecordBatchResponseEntry 
  { "RecordId" :: NullOrUndefined (PutResponseRecordId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }


newtype PutRecordBatchResponseEntryList = PutRecordBatchResponseEntryList (Array PutRecordBatchResponseEntry)


newtype PutRecordInput = PutRecordInput 
  { "DeliveryStreamName" :: (DeliveryStreamName)
  , "Record''" :: (Record'')
  }


newtype PutRecordOutput = PutRecordOutput 
  { "RecordId" :: (PutResponseRecordId)
  }


newtype PutResponseRecordId = PutResponseRecordId String


-- | <p>The unit of data in a delivery stream.</p>
newtype Record'' = Record'' 
  { "Data" :: (Data)
  }


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


newtype RedshiftRetryDurationInSeconds = RedshiftRetryDurationInSeconds Int


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Amazon Redshift.</p>
newtype RedshiftRetryOptions = RedshiftRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined (RedshiftRetryDurationInSeconds)
  }


newtype RedshiftS3BackupMode = RedshiftS3BackupMode String


-- | <p>The resource is already in use and not available for this operation.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified resource could not be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype RoleARN = RoleARN String


newtype S3BackupMode = S3BackupMode String


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


-- | <p>The service is unavailable, back off and retry the operation. If you continue to see the exception, throughput limits for the delivery stream may have been exceeded. For more information about limits and how to request an increase, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype SizeInMBs = SizeInMBs Int


-- | <p>Details about a Kinesis stream used as the source for a Kinesis Firehose delivery stream.</p>
newtype SourceDescription = SourceDescription 
  { "KinesisStreamSourceDescription" :: NullOrUndefined (KinesisStreamSourceDescription)
  }


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


newtype SplunkRetryDurationInSeconds = SplunkRetryDurationInSeconds Int


-- | <p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Splunk or if it doesn't receive an acknowledgment from Splunk.</p>
newtype SplunkRetryOptions = SplunkRetryOptions 
  { "DurationInSeconds" :: NullOrUndefined (SplunkRetryDurationInSeconds)
  }


newtype SplunkS3BackupMode = SplunkS3BackupMode String


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


newtype UpdateDestinationOutput = UpdateDestinationOutput 
  { 
  }


newtype Username = Username String
