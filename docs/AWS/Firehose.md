## Module AWS.Firehose

<fullname>Amazon Kinesis Firehose API Reference</fullname> <p>Amazon Kinesis Firehose is a fully managed service that delivers real-time streaming data to destinations such as Amazon Simple Storage Service (Amazon S3), Amazon Elasticsearch Service (Amazon ES), and Amazon Redshift.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createDeliveryStream`

``` purescript
createDeliveryStream :: forall eff. CreateDeliveryStreamInput -> Aff (err :: RequestError | eff) CreateDeliveryStreamOutput
```

<p>Creates a delivery stream.</p> <p>By default, you can create up to 20 delivery streams per region.</p> <p>This is an asynchronous operation that immediately returns. The initial status of the delivery stream is <code>CREATING</code>. After the delivery stream is created, its status is <code>ACTIVE</code> and it now accepts data. Attempts to send data to a delivery stream that is not in the <code>ACTIVE</code> state cause an exception. To check the state of a delivery stream, use <a>DescribeDeliveryStream</a>.</p> <p>A Kinesis Firehose delivery stream can be configured to receive records directly from providers using <a>PutRecord</a> or <a>PutRecordBatch</a>, or it can be configured to use an existing Kinesis stream as its source. To specify a Kinesis stream as input, set the <code>DeliveryStreamType</code> parameter to <code>KinesisStreamAsSource</code>, and provide the Kinesis stream ARN and role ARN in the <code>KinesisStreamSourceConfiguration</code> parameter.</p> <p>A delivery stream is configured with a single destination: Amazon S3, Amazon ES, or Amazon Redshift. You must specify only one of the following destination configuration parameters: <b>ExtendedS3DestinationConfiguration</b>, <b>S3DestinationConfiguration</b>, <b>ElasticsearchDestinationConfiguration</b>, or <b>RedshiftDestinationConfiguration</b>.</p> <p>When you specify <b>S3DestinationConfiguration</b>, you can also provide the following optional values: <b>BufferingHints</b>, <b>EncryptionConfiguration</b>, and <b>CompressionFormat</b>. By default, if no <b>BufferingHints</b> value is provided, Kinesis Firehose buffers data up to 5 MB or for 5 minutes, whichever condition is satisfied first. Note that <b>BufferingHints</b> is a hint, so there are some cases where the service cannot adhere to these conditions strictly; for example, record boundaries are such that the size is a little over or under the configured buffering size. By default, no encryption is performed. We strongly recommend that you enable encryption to ensure secure data storage in Amazon S3.</p> <p>A few notes about Amazon Redshift as a destination:</p> <ul> <li> <p>An Amazon Redshift destination requires an S3 bucket as intermediate location, as Kinesis Firehose first delivers data to S3 and then uses <code>COPY</code> syntax to load data into an Amazon Redshift table. This is specified in the <b>RedshiftDestinationConfiguration.S3Configuration</b> parameter.</p> </li> <li> <p>The compression formats <code>SNAPPY</code> or <code>ZIP</code> cannot be specified in <b>RedshiftDestinationConfiguration.S3Configuration</b> because the Amazon Redshift <code>COPY</code> operation that reads from the S3 bucket doesn't support these compression formats.</p> </li> <li> <p>We strongly recommend that you use the user name and password you provide exclusively with Kinesis Firehose, and that the permissions for the account are restricted for Amazon Redshift <code>INSERT</code> permissions.</p> </li> </ul> <p>Kinesis Firehose assumes the IAM role that is configured as part of the destination. The role should allow the Kinesis Firehose principal to assume the role, and the role should have permissions that allow the service to deliver the data. For more information, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/controlling-access.html#using-iam-s3">Amazon S3 Bucket Access</a> in the <i>Amazon Kinesis Firehose Developer Guide</i>.</p>

#### `deleteDeliveryStream`

``` purescript
deleteDeliveryStream :: forall eff. DeleteDeliveryStreamInput -> Aff (err :: RequestError | eff) DeleteDeliveryStreamOutput
```

<p>Deletes a delivery stream and its data.</p> <p>You can delete a delivery stream only if it is in <code>ACTIVE</code> or <code>DELETING</code> state, and not in the <code>CREATING</code> state. While the deletion request is in process, the delivery stream is in the <code>DELETING</code> state.</p> <p>To check the state of a delivery stream, use <a>DescribeDeliveryStream</a>.</p> <p>While the delivery stream is <code>DELETING</code> state, the service may continue to accept the records, but the service doesn't make any guarantees with respect to delivering the data. Therefore, as a best practice, you should first stop any applications that are sending records before deleting a delivery stream.</p>

#### `describeDeliveryStream`

``` purescript
describeDeliveryStream :: forall eff. DescribeDeliveryStreamInput -> Aff (err :: RequestError | eff) DescribeDeliveryStreamOutput
```

<p>Describes the specified delivery stream and gets the status. For example, after your delivery stream is created, call <a>DescribeDeliveryStream</a> to see if the delivery stream is <code>ACTIVE</code> and therefore ready for data to be sent to it.</p>

#### `listDeliveryStreams`

``` purescript
listDeliveryStreams :: forall eff. ListDeliveryStreamsInput -> Aff (err :: RequestError | eff) ListDeliveryStreamsOutput
```

<p>Lists your delivery streams.</p> <p>The number of delivery streams might be too large to return using a single call to <a>ListDeliveryStreams</a>. You can limit the number of delivery streams returned, using the <b>Limit</b> parameter. To determine whether there are more delivery streams to list, check the value of <b>HasMoreDeliveryStreams</b> in the output. If there are more delivery streams to list, you can request them by specifying the name of the last delivery stream returned in the call in the <b>ExclusiveStartDeliveryStreamName</b> parameter of a subsequent call.</p>

#### `putRecord`

``` purescript
putRecord :: forall eff. PutRecordInput -> Aff (err :: RequestError | eff) PutRecordOutput
```

<p>Writes a single data record into an Amazon Kinesis Firehose delivery stream. To write multiple data records into a delivery stream, use <a>PutRecordBatch</a>. Applications using these operations are referred to as producers.</p> <p>By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. Note that if you use <a>PutRecord</a> and <a>PutRecordBatch</a>, the limits are an aggregate across these two operations for each delivery stream. For more information about limits and how to request an increase, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>. </p> <p>You must specify the name of the delivery stream and the data record when using <a>PutRecord</a>. The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data, for example, a segment from a log file, geographic location data, website clickstream data, and so on.</p> <p>Kinesis Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (<code>\n</code>) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.</p> <p>The <a>PutRecord</a> operation returns a <b>RecordId</b>, which is a unique string assigned to each record. Producer applications can use this ID for purposes such as auditability and investigation.</p> <p>If the <a>PutRecord</a> operation throws a <b>ServiceUnavailableException</b>, back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream. </p> <p>Data records sent to Kinesis Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.</p>

#### `putRecordBatch`

``` purescript
putRecordBatch :: forall eff. PutRecordBatchInput -> Aff (err :: RequestError | eff) PutRecordBatchOutput
```

<p>Writes multiple data records into a delivery stream in a single call, which can achieve higher throughput per producer than when writing single records. To write single data records into a delivery stream, use <a>PutRecord</a>. Applications using these operations are referred to as producers.</p> <p>By default, each delivery stream can take in up to 2,000 transactions per second, 5,000 records per second, or 5 MB per second. If you use <a>PutRecord</a> and <a>PutRecordBatch</a>, the limits are an aggregate across these two operations for each delivery stream. For more information about limits, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>.</p> <p>Each <a>PutRecordBatch</a> request supports up to 500 records. Each record in the request can be as large as 1,000 KB (before 64-bit encoding), up to a limit of 4 MB for the entire request. These limits cannot be changed.</p> <p>You must specify the name of the delivery stream and the data record when using <a>PutRecord</a>. The data record consists of a data blob that can be up to 1,000 KB in size, and any kind of data. For example, it could be a segment from a log file, geographic location data, web site clickstream data, and so on.</p> <p>Kinesis Firehose buffers records before delivering them to the destination. To disambiguate the data blobs at the destination, a common solution is to use delimiters in the data, such as a newline (<code>\n</code>) or some other character unique within the data. This allows the consumer application to parse individual data items when reading the data from the destination.</p> <p>The <a>PutRecordBatch</a> response includes a count of failed records, <b>FailedPutCount</b>, and an array of responses, <b>RequestResponses</b>. Each entry in the <b>RequestResponses</b> array provides additional information about the processed record. It directly correlates with a record in the request array using the same ordering, from the top to the bottom. The response array always includes the same number of records as the request array. <b>RequestResponses</b> includes both successfully and unsuccessfully processed records. Kinesis Firehose attempts to process all records in each <a>PutRecordBatch</a> request. A single record failure does not stop the processing of subsequent records.</p> <p>A successfully processed record includes a <b>RecordId</b> value, which is unique for the record. An unsuccessfully processed record includes <b>ErrorCode</b> and <b>ErrorMessage</b> values. <b>ErrorCode</b> reflects the type of error, and is one of the following values: <code>ServiceUnavailable</code> or <code>InternalFailure</code>. <b>ErrorMessage</b> provides more detailed information about the error.</p> <p>If there is an internal server error or a timeout, the write might have completed or it might have failed. If <b>FailedPutCount</b> is greater than 0, retry the request, resending only those records that might have failed processing. This minimizes the possible duplicate records and also reduces the total bytes sent (and corresponding charges). We recommend that you handle any duplicates at the destination.</p> <p>If <a>PutRecordBatch</a> throws <b>ServiceUnavailableException</b>, back off and retry. If the exception persists, it is possible that the throughput limits have been exceeded for the delivery stream.</p> <p>Data records sent to Kinesis Firehose are stored for 24 hours from the time they are added to a delivery stream as it attempts to send the records to the destination. If the destination is unreachable for more than 24 hours, the data is no longer available.</p>

#### `updateDestination`

``` purescript
updateDestination :: forall eff. UpdateDestinationInput -> Aff (err :: RequestError | eff) UpdateDestinationOutput
```

<p>Updates the specified destination of the specified delivery stream.</p> <p>You can use this operation to change the destination type (for example, to replace the Amazon S3 destination with Amazon Redshift) or change the parameters associated with a destination (for example, to change the bucket name of the Amazon S3 destination). The update might not occur immediately. The target delivery stream remains active while the configurations are updated, so data writes to the delivery stream can continue during this process. The updated configurations are usually effective within a few minutes.</p> <p>Note that switching between Amazon ES and other services is not supported. For an Amazon ES destination, you can only update to another Amazon ES destination.</p> <p>If the destination type is the same, Kinesis Firehose merges the configuration parameters specified with the destination configuration that already exists on the delivery stream. If any of the parameters are not specified in the call, the existing values are retained. For example, in the Amazon S3 destination, if <a>EncryptionConfiguration</a> is not specified, then the existing <a>EncryptionConfiguration</a> is maintained on the destination.</p> <p>If the destination type is not the same, for example, changing the destination from Amazon S3 to Amazon Redshift, Kinesis Firehose does not merge any parameters. In this case, all parameters must be specified.</p> <p>Kinesis Firehose uses <b>CurrentDeliveryStreamVersionId</b> to avoid race conditions and conflicting merges. This is a required field, and the service updates the configuration only if the existing configuration has a version ID that matches. After the update is applied successfully, the version ID is updated, and can be retrieved using <a>DescribeDeliveryStream</a>. Use the new version ID to set <b>CurrentDeliveryStreamVersionId</b> in the next call.</p>

#### `AWSKMSKeyARN`

``` purescript
newtype AWSKMSKeyARN
  = AWSKMSKeyARN String
```

##### Instances
``` purescript
Newtype AWSKMSKeyARN _
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

#### `BucketARN`

``` purescript
newtype BucketARN
  = BucketARN String
```

##### Instances
``` purescript
Newtype BucketARN _
```

#### `BufferingHints`

``` purescript
newtype BufferingHints
  = BufferingHints { "SizeInMBs" :: NullOrUndefined (SizeInMBs), "IntervalInSeconds" :: NullOrUndefined (IntervalInSeconds) }
```

<p>Describes hints for the buffering to perform before delivering data to the destination. Please note that these options are treated as hints, and therefore Kinesis Firehose may choose to use different values when it is optimal.</p>

##### Instances
``` purescript
Newtype BufferingHints _
```

#### `CloudWatchLoggingOptions`

``` purescript
newtype CloudWatchLoggingOptions
  = CloudWatchLoggingOptions { "Enabled" :: NullOrUndefined (BooleanObject), "LogGroupName" :: NullOrUndefined (LogGroupName), "LogStreamName" :: NullOrUndefined (LogStreamName) }
```

<p>Describes the Amazon CloudWatch logging options for your delivery stream.</p>

##### Instances
``` purescript
Newtype CloudWatchLoggingOptions _
```

#### `ClusterJDBCURL`

``` purescript
newtype ClusterJDBCURL
  = ClusterJDBCURL String
```

##### Instances
``` purescript
Newtype ClusterJDBCURL _
```

#### `CompressionFormat`

``` purescript
newtype CompressionFormat
  = CompressionFormat String
```

##### Instances
``` purescript
Newtype CompressionFormat _
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Another modification has already happened. Fetch <b>VersionId</b> again and use it to update the destination.</p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `CopyCommand`

``` purescript
newtype CopyCommand
  = CopyCommand { "DataTableName" :: DataTableName, "DataTableColumns" :: NullOrUndefined (DataTableColumns), "CopyOptions" :: NullOrUndefined (CopyOptions) }
```

<p>Describes a <code>COPY</code> command for Amazon Redshift.</p>

##### Instances
``` purescript
Newtype CopyCommand _
```

#### `CopyOptions`

``` purescript
newtype CopyOptions
  = CopyOptions String
```

##### Instances
``` purescript
Newtype CopyOptions _
```

#### `CreateDeliveryStreamInput`

``` purescript
newtype CreateDeliveryStreamInput
  = CreateDeliveryStreamInput { "DeliveryStreamName" :: DeliveryStreamName, "DeliveryStreamType" :: NullOrUndefined (DeliveryStreamType), "KinesisStreamSourceConfiguration" :: NullOrUndefined (KinesisStreamSourceConfiguration), "S3DestinationConfiguration" :: NullOrUndefined (S3DestinationConfiguration), "ExtendedS3DestinationConfiguration" :: NullOrUndefined (ExtendedS3DestinationConfiguration), "RedshiftDestinationConfiguration" :: NullOrUndefined (RedshiftDestinationConfiguration), "ElasticsearchDestinationConfiguration" :: NullOrUndefined (ElasticsearchDestinationConfiguration), "SplunkDestinationConfiguration" :: NullOrUndefined (SplunkDestinationConfiguration) }
```

##### Instances
``` purescript
Newtype CreateDeliveryStreamInput _
```

#### `CreateDeliveryStreamOutput`

``` purescript
newtype CreateDeliveryStreamOutput
  = CreateDeliveryStreamOutput { "DeliveryStreamARN" :: NullOrUndefined (DeliveryStreamARN) }
```

##### Instances
``` purescript
Newtype CreateDeliveryStreamOutput _
```

#### `Data`

``` purescript
newtype Data
  = Data String
```

##### Instances
``` purescript
Newtype Data _
```

#### `DataTableColumns`

``` purescript
newtype DataTableColumns
  = DataTableColumns String
```

##### Instances
``` purescript
Newtype DataTableColumns _
```

#### `DataTableName`

``` purescript
newtype DataTableName
  = DataTableName String
```

##### Instances
``` purescript
Newtype DataTableName _
```

#### `DeleteDeliveryStreamInput`

``` purescript
newtype DeleteDeliveryStreamInput
  = DeleteDeliveryStreamInput { "DeliveryStreamName" :: DeliveryStreamName }
```

##### Instances
``` purescript
Newtype DeleteDeliveryStreamInput _
```

#### `DeleteDeliveryStreamOutput`

``` purescript
newtype DeleteDeliveryStreamOutput
  = DeleteDeliveryStreamOutput {  }
```

##### Instances
``` purescript
Newtype DeleteDeliveryStreamOutput _
```

#### `DeliveryStartTimestamp`

``` purescript
newtype DeliveryStartTimestamp
  = DeliveryStartTimestamp Number
```

##### Instances
``` purescript
Newtype DeliveryStartTimestamp _
```

#### `DeliveryStreamARN`

``` purescript
newtype DeliveryStreamARN
  = DeliveryStreamARN String
```

##### Instances
``` purescript
Newtype DeliveryStreamARN _
```

#### `DeliveryStreamDescription`

``` purescript
newtype DeliveryStreamDescription
  = DeliveryStreamDescription { "DeliveryStreamName" :: DeliveryStreamName, "DeliveryStreamARN" :: DeliveryStreamARN, "DeliveryStreamStatus" :: DeliveryStreamStatus, "DeliveryStreamType" :: DeliveryStreamType, "VersionId" :: DeliveryStreamVersionId, "CreateTimestamp" :: NullOrUndefined (Number), "LastUpdateTimestamp" :: NullOrUndefined (Number), "Source" :: NullOrUndefined (SourceDescription), "Destinations" :: DestinationDescriptionList, "HasMoreDestinations" :: BooleanObject }
```

<p>Contains information about a delivery stream.</p>

##### Instances
``` purescript
Newtype DeliveryStreamDescription _
```

#### `DeliveryStreamName`

``` purescript
newtype DeliveryStreamName
  = DeliveryStreamName String
```

##### Instances
``` purescript
Newtype DeliveryStreamName _
```

#### `DeliveryStreamNameList`

``` purescript
newtype DeliveryStreamNameList
  = DeliveryStreamNameList (Array DeliveryStreamName)
```

##### Instances
``` purescript
Newtype DeliveryStreamNameList _
```

#### `DeliveryStreamStatus`

``` purescript
newtype DeliveryStreamStatus
  = DeliveryStreamStatus String
```

##### Instances
``` purescript
Newtype DeliveryStreamStatus _
```

#### `DeliveryStreamType`

``` purescript
newtype DeliveryStreamType
  = DeliveryStreamType String
```

##### Instances
``` purescript
Newtype DeliveryStreamType _
```

#### `DeliveryStreamVersionId`

``` purescript
newtype DeliveryStreamVersionId
  = DeliveryStreamVersionId String
```

##### Instances
``` purescript
Newtype DeliveryStreamVersionId _
```

#### `DescribeDeliveryStreamInput`

``` purescript
newtype DescribeDeliveryStreamInput
  = DescribeDeliveryStreamInput { "DeliveryStreamName" :: DeliveryStreamName, "Limit" :: NullOrUndefined (DescribeDeliveryStreamInputLimit), "ExclusiveStartDestinationId" :: NullOrUndefined (DestinationId) }
```

##### Instances
``` purescript
Newtype DescribeDeliveryStreamInput _
```

#### `DescribeDeliveryStreamInputLimit`

``` purescript
newtype DescribeDeliveryStreamInputLimit
  = DescribeDeliveryStreamInputLimit Int
```

##### Instances
``` purescript
Newtype DescribeDeliveryStreamInputLimit _
```

#### `DescribeDeliveryStreamOutput`

``` purescript
newtype DescribeDeliveryStreamOutput
  = DescribeDeliveryStreamOutput { "DeliveryStreamDescription" :: DeliveryStreamDescription }
```

##### Instances
``` purescript
Newtype DescribeDeliveryStreamOutput _
```

#### `DestinationDescription`

``` purescript
newtype DestinationDescription
  = DestinationDescription { "DestinationId" :: DestinationId, "S3DestinationDescription" :: NullOrUndefined (S3DestinationDescription), "ExtendedS3DestinationDescription" :: NullOrUndefined (ExtendedS3DestinationDescription), "RedshiftDestinationDescription" :: NullOrUndefined (RedshiftDestinationDescription), "ElasticsearchDestinationDescription" :: NullOrUndefined (ElasticsearchDestinationDescription), "SplunkDestinationDescription" :: NullOrUndefined (SplunkDestinationDescription) }
```

<p>Describes the destination for a delivery stream.</p>

##### Instances
``` purescript
Newtype DestinationDescription _
```

#### `DestinationDescriptionList`

``` purescript
newtype DestinationDescriptionList
  = DestinationDescriptionList (Array DestinationDescription)
```

##### Instances
``` purescript
Newtype DestinationDescriptionList _
```

#### `DestinationId`

``` purescript
newtype DestinationId
  = DestinationId String
```

##### Instances
``` purescript
Newtype DestinationId _
```

#### `ElasticsearchBufferingHints`

``` purescript
newtype ElasticsearchBufferingHints
  = ElasticsearchBufferingHints { "IntervalInSeconds" :: NullOrUndefined (ElasticsearchBufferingIntervalInSeconds), "SizeInMBs" :: NullOrUndefined (ElasticsearchBufferingSizeInMBs) }
```

<p>Describes the buffering to perform before delivering data to the Amazon ES destination.</p>

##### Instances
``` purescript
Newtype ElasticsearchBufferingHints _
```

#### `ElasticsearchBufferingIntervalInSeconds`

``` purescript
newtype ElasticsearchBufferingIntervalInSeconds
  = ElasticsearchBufferingIntervalInSeconds Int
```

##### Instances
``` purescript
Newtype ElasticsearchBufferingIntervalInSeconds _
```

#### `ElasticsearchBufferingSizeInMBs`

``` purescript
newtype ElasticsearchBufferingSizeInMBs
  = ElasticsearchBufferingSizeInMBs Int
```

##### Instances
``` purescript
Newtype ElasticsearchBufferingSizeInMBs _
```

#### `ElasticsearchDestinationConfiguration`

``` purescript
newtype ElasticsearchDestinationConfiguration
  = ElasticsearchDestinationConfiguration { "RoleARN" :: RoleARN, "DomainARN" :: ElasticsearchDomainARN, "IndexName" :: ElasticsearchIndexName, "TypeName" :: ElasticsearchTypeName, "IndexRotationPeriod" :: NullOrUndefined (ElasticsearchIndexRotationPeriod), "BufferingHints" :: NullOrUndefined (ElasticsearchBufferingHints), "RetryOptions" :: NullOrUndefined (ElasticsearchRetryOptions), "S3BackupMode" :: NullOrUndefined (ElasticsearchS3BackupMode), "S3Configuration" :: S3DestinationConfiguration, "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes the configuration of a destination in Amazon ES.</p>

##### Instances
``` purescript
Newtype ElasticsearchDestinationConfiguration _
```

#### `ElasticsearchDestinationDescription`

``` purescript
newtype ElasticsearchDestinationDescription
  = ElasticsearchDestinationDescription { "RoleARN" :: NullOrUndefined (RoleARN), "DomainARN" :: NullOrUndefined (ElasticsearchDomainARN), "IndexName" :: NullOrUndefined (ElasticsearchIndexName), "TypeName" :: NullOrUndefined (ElasticsearchTypeName), "IndexRotationPeriod" :: NullOrUndefined (ElasticsearchIndexRotationPeriod), "BufferingHints" :: NullOrUndefined (ElasticsearchBufferingHints), "RetryOptions" :: NullOrUndefined (ElasticsearchRetryOptions), "S3BackupMode" :: NullOrUndefined (ElasticsearchS3BackupMode), "S3DestinationDescription" :: NullOrUndefined (S3DestinationDescription), "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>The destination description in Amazon ES.</p>

##### Instances
``` purescript
Newtype ElasticsearchDestinationDescription _
```

#### `ElasticsearchDestinationUpdate`

``` purescript
newtype ElasticsearchDestinationUpdate
  = ElasticsearchDestinationUpdate { "RoleARN" :: NullOrUndefined (RoleARN), "DomainARN" :: NullOrUndefined (ElasticsearchDomainARN), "IndexName" :: NullOrUndefined (ElasticsearchIndexName), "TypeName" :: NullOrUndefined (ElasticsearchTypeName), "IndexRotationPeriod" :: NullOrUndefined (ElasticsearchIndexRotationPeriod), "BufferingHints" :: NullOrUndefined (ElasticsearchBufferingHints), "RetryOptions" :: NullOrUndefined (ElasticsearchRetryOptions), "S3Update" :: NullOrUndefined (S3DestinationUpdate), "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes an update for a destination in Amazon ES.</p>

##### Instances
``` purescript
Newtype ElasticsearchDestinationUpdate _
```

#### `ElasticsearchDomainARN`

``` purescript
newtype ElasticsearchDomainARN
  = ElasticsearchDomainARN String
```

##### Instances
``` purescript
Newtype ElasticsearchDomainARN _
```

#### `ElasticsearchIndexName`

``` purescript
newtype ElasticsearchIndexName
  = ElasticsearchIndexName String
```

##### Instances
``` purescript
Newtype ElasticsearchIndexName _
```

#### `ElasticsearchIndexRotationPeriod`

``` purescript
newtype ElasticsearchIndexRotationPeriod
  = ElasticsearchIndexRotationPeriod String
```

##### Instances
``` purescript
Newtype ElasticsearchIndexRotationPeriod _
```

#### `ElasticsearchRetryDurationInSeconds`

``` purescript
newtype ElasticsearchRetryDurationInSeconds
  = ElasticsearchRetryDurationInSeconds Int
```

##### Instances
``` purescript
Newtype ElasticsearchRetryDurationInSeconds _
```

#### `ElasticsearchRetryOptions`

``` purescript
newtype ElasticsearchRetryOptions
  = ElasticsearchRetryOptions { "DurationInSeconds" :: NullOrUndefined (ElasticsearchRetryDurationInSeconds) }
```

<p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Amazon ES.</p>

##### Instances
``` purescript
Newtype ElasticsearchRetryOptions _
```

#### `ElasticsearchS3BackupMode`

``` purescript
newtype ElasticsearchS3BackupMode
  = ElasticsearchS3BackupMode String
```

##### Instances
``` purescript
Newtype ElasticsearchS3BackupMode _
```

#### `ElasticsearchTypeName`

``` purescript
newtype ElasticsearchTypeName
  = ElasticsearchTypeName String
```

##### Instances
``` purescript
Newtype ElasticsearchTypeName _
```

#### `EncryptionConfiguration`

``` purescript
newtype EncryptionConfiguration
  = EncryptionConfiguration { "NoEncryptionConfig" :: NullOrUndefined (NoEncryptionConfig), "KMSEncryptionConfig" :: NullOrUndefined (KMSEncryptionConfig) }
```

<p>Describes the encryption for a destination in Amazon S3.</p>

##### Instances
``` purescript
Newtype EncryptionConfiguration _
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

#### `ExtendedS3DestinationConfiguration`

``` purescript
newtype ExtendedS3DestinationConfiguration
  = ExtendedS3DestinationConfiguration { "RoleARN" :: RoleARN, "BucketARN" :: BucketARN, "Prefix" :: NullOrUndefined (Prefix), "BufferingHints" :: NullOrUndefined (BufferingHints), "CompressionFormat" :: NullOrUndefined (CompressionFormat), "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions), "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "S3BackupMode" :: NullOrUndefined (S3BackupMode), "S3BackupConfiguration" :: NullOrUndefined (S3DestinationConfiguration) }
```

<p>Describes the configuration of a destination in Amazon S3.</p>

##### Instances
``` purescript
Newtype ExtendedS3DestinationConfiguration _
```

#### `ExtendedS3DestinationDescription`

``` purescript
newtype ExtendedS3DestinationDescription
  = ExtendedS3DestinationDescription { "RoleARN" :: RoleARN, "BucketARN" :: BucketARN, "Prefix" :: NullOrUndefined (Prefix), "BufferingHints" :: BufferingHints, "CompressionFormat" :: CompressionFormat, "EncryptionConfiguration" :: EncryptionConfiguration, "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions), "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "S3BackupMode" :: NullOrUndefined (S3BackupMode), "S3BackupDescription" :: NullOrUndefined (S3DestinationDescription) }
```

<p>Describes a destination in Amazon S3.</p>

##### Instances
``` purescript
Newtype ExtendedS3DestinationDescription _
```

#### `ExtendedS3DestinationUpdate`

``` purescript
newtype ExtendedS3DestinationUpdate
  = ExtendedS3DestinationUpdate { "RoleARN" :: NullOrUndefined (RoleARN), "BucketARN" :: NullOrUndefined (BucketARN), "Prefix" :: NullOrUndefined (Prefix), "BufferingHints" :: NullOrUndefined (BufferingHints), "CompressionFormat" :: NullOrUndefined (CompressionFormat), "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions), "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "S3BackupMode" :: NullOrUndefined (S3BackupMode), "S3BackupUpdate" :: NullOrUndefined (S3DestinationUpdate) }
```

<p>Describes an update for a destination in Amazon S3.</p>

##### Instances
``` purescript
Newtype ExtendedS3DestinationUpdate _
```

#### `HECAcknowledgmentTimeoutInSeconds`

``` purescript
newtype HECAcknowledgmentTimeoutInSeconds
  = HECAcknowledgmentTimeoutInSeconds Int
```

##### Instances
``` purescript
Newtype HECAcknowledgmentTimeoutInSeconds _
```

#### `HECEndpoint`

``` purescript
newtype HECEndpoint
  = HECEndpoint String
```

##### Instances
``` purescript
Newtype HECEndpoint _
```

#### `HECEndpointType`

``` purescript
newtype HECEndpointType
  = HECEndpointType String
```

##### Instances
``` purescript
Newtype HECEndpointType _
```

#### `HECToken`

``` purescript
newtype HECToken
  = HECToken String
```

##### Instances
``` purescript
Newtype HECToken _
```

#### `IntervalInSeconds`

``` purescript
newtype IntervalInSeconds
  = IntervalInSeconds Int
```

##### Instances
``` purescript
Newtype IntervalInSeconds _
```

#### `InvalidArgumentException`

``` purescript
newtype InvalidArgumentException
  = InvalidArgumentException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified input parameter has a value that is not valid.</p>

##### Instances
``` purescript
Newtype InvalidArgumentException _
```

#### `KMSEncryptionConfig`

``` purescript
newtype KMSEncryptionConfig
  = KMSEncryptionConfig { "AWSKMSKeyARN" :: AWSKMSKeyARN }
```

<p>Describes an encryption key for a destination in Amazon S3.</p>

##### Instances
``` purescript
Newtype KMSEncryptionConfig _
```

#### `KinesisStreamARN`

``` purescript
newtype KinesisStreamARN
  = KinesisStreamARN String
```

##### Instances
``` purescript
Newtype KinesisStreamARN _
```

#### `KinesisStreamSourceConfiguration`

``` purescript
newtype KinesisStreamSourceConfiguration
  = KinesisStreamSourceConfiguration { "KinesisStreamARN" :: KinesisStreamARN, "RoleARN" :: RoleARN }
```

<p>The stream and role ARNs for a Kinesis stream used as the source for a delivery stream.</p>

##### Instances
``` purescript
Newtype KinesisStreamSourceConfiguration _
```

#### `KinesisStreamSourceDescription`

``` purescript
newtype KinesisStreamSourceDescription
  = KinesisStreamSourceDescription { "KinesisStreamARN" :: NullOrUndefined (KinesisStreamARN), "RoleARN" :: NullOrUndefined (RoleARN), "DeliveryStartTimestamp" :: NullOrUndefined (DeliveryStartTimestamp) }
```

<p>Details about a Kinesis stream used as the source for a Kinesis Firehose delivery stream.</p>

##### Instances
``` purescript
Newtype KinesisStreamSourceDescription _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>You have already reached the limit for a requested resource.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListDeliveryStreamsInput`

``` purescript
newtype ListDeliveryStreamsInput
  = ListDeliveryStreamsInput { "Limit" :: NullOrUndefined (ListDeliveryStreamsInputLimit), "DeliveryStreamType" :: NullOrUndefined (DeliveryStreamType), "ExclusiveStartDeliveryStreamName" :: NullOrUndefined (DeliveryStreamName) }
```

##### Instances
``` purescript
Newtype ListDeliveryStreamsInput _
```

#### `ListDeliveryStreamsInputLimit`

``` purescript
newtype ListDeliveryStreamsInputLimit
  = ListDeliveryStreamsInputLimit Int
```

##### Instances
``` purescript
Newtype ListDeliveryStreamsInputLimit _
```

#### `ListDeliveryStreamsOutput`

``` purescript
newtype ListDeliveryStreamsOutput
  = ListDeliveryStreamsOutput { "DeliveryStreamNames" :: DeliveryStreamNameList, "HasMoreDeliveryStreams" :: BooleanObject }
```

##### Instances
``` purescript
Newtype ListDeliveryStreamsOutput _
```

#### `LogGroupName`

``` purescript
newtype LogGroupName
  = LogGroupName String
```

##### Instances
``` purescript
Newtype LogGroupName _
```

#### `LogStreamName`

``` purescript
newtype LogStreamName
  = LogStreamName String
```

##### Instances
``` purescript
Newtype LogStreamName _
```

#### `NoEncryptionConfig`

``` purescript
newtype NoEncryptionConfig
  = NoEncryptionConfig String
```

##### Instances
``` purescript
Newtype NoEncryptionConfig _
```

#### `NonNegativeIntegerObject`

``` purescript
newtype NonNegativeIntegerObject
  = NonNegativeIntegerObject Int
```

##### Instances
``` purescript
Newtype NonNegativeIntegerObject _
```

#### `Password`

``` purescript
newtype Password
  = Password String
```

##### Instances
``` purescript
Newtype Password _
```

#### `Prefix`

``` purescript
newtype Prefix
  = Prefix String
```

##### Instances
``` purescript
Newtype Prefix _
```

#### `ProcessingConfiguration`

``` purescript
newtype ProcessingConfiguration
  = ProcessingConfiguration { "Enabled" :: NullOrUndefined (BooleanObject), "Processors" :: NullOrUndefined (ProcessorList) }
```

<p>Describes a data processing configuration.</p>

##### Instances
``` purescript
Newtype ProcessingConfiguration _
```

#### `Processor`

``` purescript
newtype Processor
  = Processor { "Type" :: ProcessorType, "Parameters" :: NullOrUndefined (ProcessorParameterList) }
```

<p>Describes a data processor.</p>

##### Instances
``` purescript
Newtype Processor _
```

#### `ProcessorList`

``` purescript
newtype ProcessorList
  = ProcessorList (Array Processor)
```

##### Instances
``` purescript
Newtype ProcessorList _
```

#### `ProcessorParameter`

``` purescript
newtype ProcessorParameter
  = ProcessorParameter { "ParameterName" :: ProcessorParameterName, "ParameterValue" :: ProcessorParameterValue }
```

<p>Describes the processor parameter.</p>

##### Instances
``` purescript
Newtype ProcessorParameter _
```

#### `ProcessorParameterList`

``` purescript
newtype ProcessorParameterList
  = ProcessorParameterList (Array ProcessorParameter)
```

##### Instances
``` purescript
Newtype ProcessorParameterList _
```

#### `ProcessorParameterName`

``` purescript
newtype ProcessorParameterName
  = ProcessorParameterName String
```

##### Instances
``` purescript
Newtype ProcessorParameterName _
```

#### `ProcessorParameterValue`

``` purescript
newtype ProcessorParameterValue
  = ProcessorParameterValue String
```

##### Instances
``` purescript
Newtype ProcessorParameterValue _
```

#### `ProcessorType`

``` purescript
newtype ProcessorType
  = ProcessorType String
```

##### Instances
``` purescript
Newtype ProcessorType _
```

#### `PutRecordBatchInput`

``` purescript
newtype PutRecordBatchInput
  = PutRecordBatchInput { "DeliveryStreamName" :: DeliveryStreamName, "Records" :: PutRecordBatchRequestEntryList }
```

##### Instances
``` purescript
Newtype PutRecordBatchInput _
```

#### `PutRecordBatchOutput`

``` purescript
newtype PutRecordBatchOutput
  = PutRecordBatchOutput { "FailedPutCount" :: NonNegativeIntegerObject, "RequestResponses" :: PutRecordBatchResponseEntryList }
```

##### Instances
``` purescript
Newtype PutRecordBatchOutput _
```

#### `PutRecordBatchRequestEntryList`

``` purescript
newtype PutRecordBatchRequestEntryList
  = PutRecordBatchRequestEntryList (Array Record'')
```

##### Instances
``` purescript
Newtype PutRecordBatchRequestEntryList _
```

#### `PutRecordBatchResponseEntry`

``` purescript
newtype PutRecordBatchResponseEntry
  = PutRecordBatchResponseEntry { "RecordId" :: NullOrUndefined (PutResponseRecordId), "ErrorCode" :: NullOrUndefined (ErrorCode), "ErrorMessage" :: NullOrUndefined (ErrorMessage) }
```

<p>Contains the result for an individual record from a <a>PutRecordBatch</a> request. If the record is successfully added to your delivery stream, it receives a record ID. If the record fails to be added to your delivery stream, the result includes an error code and an error message.</p>

##### Instances
``` purescript
Newtype PutRecordBatchResponseEntry _
```

#### `PutRecordBatchResponseEntryList`

``` purescript
newtype PutRecordBatchResponseEntryList
  = PutRecordBatchResponseEntryList (Array PutRecordBatchResponseEntry)
```

##### Instances
``` purescript
Newtype PutRecordBatchResponseEntryList _
```

#### `PutRecordInput`

``` purescript
newtype PutRecordInput
  = PutRecordInput { "DeliveryStreamName" :: DeliveryStreamName, "Record''" :: Record'' }
```

##### Instances
``` purescript
Newtype PutRecordInput _
```

#### `PutRecordOutput`

``` purescript
newtype PutRecordOutput
  = PutRecordOutput { "RecordId" :: PutResponseRecordId }
```

##### Instances
``` purescript
Newtype PutRecordOutput _
```

#### `PutResponseRecordId`

``` purescript
newtype PutResponseRecordId
  = PutResponseRecordId String
```

##### Instances
``` purescript
Newtype PutResponseRecordId _
```

#### `Record''`

``` purescript
newtype Record''
  = Record'' { "Data" :: Data }
```

<p>The unit of data in a delivery stream.</p>

##### Instances
``` purescript
Newtype Record'' _
```

#### `RedshiftDestinationConfiguration`

``` purescript
newtype RedshiftDestinationConfiguration
  = RedshiftDestinationConfiguration { "RoleARN" :: RoleARN, "ClusterJDBCURL" :: ClusterJDBCURL, "CopyCommand" :: CopyCommand, "Username" :: Username, "Password" :: Password, "RetryOptions" :: NullOrUndefined (RedshiftRetryOptions), "S3Configuration" :: S3DestinationConfiguration, "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "S3BackupMode" :: NullOrUndefined (RedshiftS3BackupMode), "S3BackupConfiguration" :: NullOrUndefined (S3DestinationConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes the configuration of a destination in Amazon Redshift.</p>

##### Instances
``` purescript
Newtype RedshiftDestinationConfiguration _
```

#### `RedshiftDestinationDescription`

``` purescript
newtype RedshiftDestinationDescription
  = RedshiftDestinationDescription { "RoleARN" :: RoleARN, "ClusterJDBCURL" :: ClusterJDBCURL, "CopyCommand" :: CopyCommand, "Username" :: Username, "RetryOptions" :: NullOrUndefined (RedshiftRetryOptions), "S3DestinationDescription" :: S3DestinationDescription, "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "S3BackupMode" :: NullOrUndefined (RedshiftS3BackupMode), "S3BackupDescription" :: NullOrUndefined (S3DestinationDescription), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes a destination in Amazon Redshift.</p>

##### Instances
``` purescript
Newtype RedshiftDestinationDescription _
```

#### `RedshiftDestinationUpdate`

``` purescript
newtype RedshiftDestinationUpdate
  = RedshiftDestinationUpdate { "RoleARN" :: NullOrUndefined (RoleARN), "ClusterJDBCURL" :: NullOrUndefined (ClusterJDBCURL), "CopyCommand" :: NullOrUndefined (CopyCommand), "Username" :: NullOrUndefined (Username), "Password" :: NullOrUndefined (Password), "RetryOptions" :: NullOrUndefined (RedshiftRetryOptions), "S3Update" :: NullOrUndefined (S3DestinationUpdate), "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "S3BackupMode" :: NullOrUndefined (RedshiftS3BackupMode), "S3BackupUpdate" :: NullOrUndefined (S3DestinationUpdate), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes an update for a destination in Amazon Redshift.</p>

##### Instances
``` purescript
Newtype RedshiftDestinationUpdate _
```

#### `RedshiftRetryDurationInSeconds`

``` purescript
newtype RedshiftRetryDurationInSeconds
  = RedshiftRetryDurationInSeconds Int
```

##### Instances
``` purescript
Newtype RedshiftRetryDurationInSeconds _
```

#### `RedshiftRetryOptions`

``` purescript
newtype RedshiftRetryOptions
  = RedshiftRetryOptions { "DurationInSeconds" :: NullOrUndefined (RedshiftRetryDurationInSeconds) }
```

<p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Amazon Redshift.</p>

##### Instances
``` purescript
Newtype RedshiftRetryOptions _
```

#### `RedshiftS3BackupMode`

``` purescript
newtype RedshiftS3BackupMode
  = RedshiftS3BackupMode String
```

##### Instances
``` purescript
Newtype RedshiftS3BackupMode _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The resource is already in use and not available for this operation.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified resource could not be found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `RoleARN`

``` purescript
newtype RoleARN
  = RoleARN String
```

##### Instances
``` purescript
Newtype RoleARN _
```

#### `S3BackupMode`

``` purescript
newtype S3BackupMode
  = S3BackupMode String
```

##### Instances
``` purescript
Newtype S3BackupMode _
```

#### `S3DestinationConfiguration`

``` purescript
newtype S3DestinationConfiguration
  = S3DestinationConfiguration { "RoleARN" :: RoleARN, "BucketARN" :: BucketARN, "Prefix" :: NullOrUndefined (Prefix), "BufferingHints" :: NullOrUndefined (BufferingHints), "CompressionFormat" :: NullOrUndefined (CompressionFormat), "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes the configuration of a destination in Amazon S3.</p>

##### Instances
``` purescript
Newtype S3DestinationConfiguration _
```

#### `S3DestinationDescription`

``` purescript
newtype S3DestinationDescription
  = S3DestinationDescription { "RoleARN" :: RoleARN, "BucketARN" :: BucketARN, "Prefix" :: NullOrUndefined (Prefix), "BufferingHints" :: BufferingHints, "CompressionFormat" :: CompressionFormat, "EncryptionConfiguration" :: EncryptionConfiguration, "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes a destination in Amazon S3.</p>

##### Instances
``` purescript
Newtype S3DestinationDescription _
```

#### `S3DestinationUpdate`

``` purescript
newtype S3DestinationUpdate
  = S3DestinationUpdate { "RoleARN" :: NullOrUndefined (RoleARN), "BucketARN" :: NullOrUndefined (BucketARN), "Prefix" :: NullOrUndefined (Prefix), "BufferingHints" :: NullOrUndefined (BufferingHints), "CompressionFormat" :: NullOrUndefined (CompressionFormat), "EncryptionConfiguration" :: NullOrUndefined (EncryptionConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes an update for a destination in Amazon S3.</p>

##### Instances
``` purescript
Newtype S3DestinationUpdate _
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The service is unavailable, back off and retry the operation. If you continue to see the exception, throughput limits for the delivery stream may have been exceeded. For more information about limits and how to request an increase, see <a href="http://docs.aws.amazon.com/firehose/latest/dev/limits.html">Amazon Kinesis Firehose Limits</a>.</p>

##### Instances
``` purescript
Newtype ServiceUnavailableException _
```

#### `SizeInMBs`

``` purescript
newtype SizeInMBs
  = SizeInMBs Int
```

##### Instances
``` purescript
Newtype SizeInMBs _
```

#### `SourceDescription`

``` purescript
newtype SourceDescription
  = SourceDescription { "KinesisStreamSourceDescription" :: NullOrUndefined (KinesisStreamSourceDescription) }
```

<p>Details about a Kinesis stream used as the source for a Kinesis Firehose delivery stream.</p>

##### Instances
``` purescript
Newtype SourceDescription _
```

#### `SplunkDestinationConfiguration`

``` purescript
newtype SplunkDestinationConfiguration
  = SplunkDestinationConfiguration { "HECEndpoint" :: HECEndpoint, "HECEndpointType" :: HECEndpointType, "HECToken" :: HECToken, "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined (HECAcknowledgmentTimeoutInSeconds), "RetryOptions" :: NullOrUndefined (SplunkRetryOptions), "S3BackupMode" :: NullOrUndefined (SplunkS3BackupMode), "S3Configuration" :: S3DestinationConfiguration, "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes the configuration of a destination in Splunk.</p>

##### Instances
``` purescript
Newtype SplunkDestinationConfiguration _
```

#### `SplunkDestinationDescription`

``` purescript
newtype SplunkDestinationDescription
  = SplunkDestinationDescription { "HECEndpoint" :: NullOrUndefined (HECEndpoint), "HECEndpointType" :: NullOrUndefined (HECEndpointType), "HECToken" :: NullOrUndefined (HECToken), "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined (HECAcknowledgmentTimeoutInSeconds), "RetryOptions" :: NullOrUndefined (SplunkRetryOptions), "S3BackupMode" :: NullOrUndefined (SplunkS3BackupMode), "S3DestinationDescription" :: NullOrUndefined (S3DestinationDescription), "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes a destination in Splunk.</p>

##### Instances
``` purescript
Newtype SplunkDestinationDescription _
```

#### `SplunkDestinationUpdate`

``` purescript
newtype SplunkDestinationUpdate
  = SplunkDestinationUpdate { "HECEndpoint" :: NullOrUndefined (HECEndpoint), "HECEndpointType" :: NullOrUndefined (HECEndpointType), "HECToken" :: NullOrUndefined (HECToken), "HECAcknowledgmentTimeoutInSeconds" :: NullOrUndefined (HECAcknowledgmentTimeoutInSeconds), "RetryOptions" :: NullOrUndefined (SplunkRetryOptions), "S3BackupMode" :: NullOrUndefined (SplunkS3BackupMode), "S3Update" :: NullOrUndefined (S3DestinationUpdate), "ProcessingConfiguration" :: NullOrUndefined (ProcessingConfiguration), "CloudWatchLoggingOptions" :: NullOrUndefined (CloudWatchLoggingOptions) }
```

<p>Describes an update for a destination in Splunk.</p>

##### Instances
``` purescript
Newtype SplunkDestinationUpdate _
```

#### `SplunkRetryDurationInSeconds`

``` purescript
newtype SplunkRetryDurationInSeconds
  = SplunkRetryDurationInSeconds Int
```

##### Instances
``` purescript
Newtype SplunkRetryDurationInSeconds _
```

#### `SplunkRetryOptions`

``` purescript
newtype SplunkRetryOptions
  = SplunkRetryOptions { "DurationInSeconds" :: NullOrUndefined (SplunkRetryDurationInSeconds) }
```

<p>Configures retry behavior in case Kinesis Firehose is unable to deliver documents to Splunk or if it doesn't receive an acknowledgment from Splunk.</p>

##### Instances
``` purescript
Newtype SplunkRetryOptions _
```

#### `SplunkS3BackupMode`

``` purescript
newtype SplunkS3BackupMode
  = SplunkS3BackupMode String
```

##### Instances
``` purescript
Newtype SplunkS3BackupMode _
```

#### `UpdateDestinationInput`

``` purescript
newtype UpdateDestinationInput
  = UpdateDestinationInput { "DeliveryStreamName" :: DeliveryStreamName, "CurrentDeliveryStreamVersionId" :: DeliveryStreamVersionId, "DestinationId" :: DestinationId, "S3DestinationUpdate" :: NullOrUndefined (S3DestinationUpdate), "ExtendedS3DestinationUpdate" :: NullOrUndefined (ExtendedS3DestinationUpdate), "RedshiftDestinationUpdate" :: NullOrUndefined (RedshiftDestinationUpdate), "ElasticsearchDestinationUpdate" :: NullOrUndefined (ElasticsearchDestinationUpdate), "SplunkDestinationUpdate" :: NullOrUndefined (SplunkDestinationUpdate) }
```

##### Instances
``` purescript
Newtype UpdateDestinationInput _
```

#### `UpdateDestinationOutput`

``` purescript
newtype UpdateDestinationOutput
  = UpdateDestinationOutput {  }
```

##### Instances
``` purescript
Newtype UpdateDestinationOutput _
```

#### `Username`

``` purescript
newtype Username
  = Username String
```

##### Instances
``` purescript
Newtype Username _
```


