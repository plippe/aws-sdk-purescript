

-- | <fullname>Amazon Kinesis Data Streams Service API Reference</fullname> <p>Amazon Kinesis Data Streams is a managed service that scales elastically for real-time processing of streaming big data.</p>
module AWS.Kinesis where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Kinesis" :: String


-- | <p>Adds or updates tags for the specified Kinesis data stream. Each stream can have up to 10 tags.</p> <p>If tags have already been assigned to the stream, <code>AddTagsToStream</code> overwrites any existing tags that correspond to the specified tag keys.</p> <p> <a>AddTagsToStream</a> has a limit of five transactions per second per account.</p>
addTagsToStream :: forall eff. AddTagsToStreamInput -> Aff (err :: AWS.RequestError | eff) Unit
addTagsToStream = AWS.request serviceName "AddTagsToStream" 


-- | <p>Creates a Kinesis data stream. A stream captures and transports data records that are continuously emitted from different data sources or <i>producers</i>. Scale-out within a stream is explicitly supported by means of shards, which are uniquely identified groups of data records in a stream.</p> <p>You specify and control the number of shards that a stream is composed of. Each shard can support reads up to five transactions per second, up to a maximum data read total of 2 MB per second. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MB per second. If the amount of data input increases or decreases, you can add or remove shards.</p> <p>The stream name identifies the stream. The name is scoped to the AWS account used by the application. It is also scoped by AWS Region. That is, two streams in two different accounts can have the same name, and two streams in the same account, but in two different Regions, can have the same name.</p> <p> <code>CreateStream</code> is an asynchronous operation. Upon receiving a <code>CreateStream</code> request, Kinesis Data Streams immediately returns and sets the stream status to <code>CREATING</code>. After the stream is created, Kinesis Data Streams sets the stream status to <code>ACTIVE</code>. You should perform read and write operations only on an <code>ACTIVE</code> stream. </p> <p>You receive a <code>LimitExceededException</code> when making a <code>CreateStream</code> request when you try to do one of the following:</p> <ul> <li> <p>Have more than five streams in the <code>CREATING</code> state at any point in time.</p> </li> <li> <p>Create more shards than are authorized for your account.</p> </li> </ul> <p>For the default shard limit for an AWS account, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html">Amazon Kinesis Data Streams Limits</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>. To increase this limit, <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html">contact AWS Support</a>.</p> <p>You can use <code>DescribeStream</code> to check the stream status, which is returned in <code>StreamStatus</code>.</p> <p> <a>CreateStream</a> has a limit of five transactions per second per account.</p>
createStream :: forall eff. CreateStreamInput -> Aff (err :: AWS.RequestError | eff) Unit
createStream = AWS.request serviceName "CreateStream" 


-- | <p>Decreases the Kinesis data stream's retention period, which is the length of time data records are accessible after they are added to the stream. The minimum value of a stream's retention period is 24 hours.</p> <p>This operation may result in lost data. For example, if the stream's retention period is 48 hours and is decreased to 24 hours, any data already in the stream that is older than 24 hours is inaccessible.</p>
decreaseStreamRetentionPeriod :: forall eff. DecreaseStreamRetentionPeriodInput -> Aff (err :: AWS.RequestError | eff) Unit
decreaseStreamRetentionPeriod = AWS.request serviceName "DecreaseStreamRetentionPeriod" 


-- | <p>Deletes a Kinesis data stream and all its shards and data. You must shut down any applications that are operating on the stream before you delete the stream. If an application attempts to operate on a deleted stream, it receives the exception <code>ResourceNotFoundException</code>.</p> <p>If the stream is in the <code>ACTIVE</code> state, you can delete it. After a <code>DeleteStream</code> request, the specified stream is in the <code>DELETING</code> state until Kinesis Data Streams completes the deletion.</p> <p> <b>Note:</b> Kinesis Data Streams might continue to accept data read and write operations, such as <a>PutRecord</a>, <a>PutRecords</a>, and <a>GetRecords</a>, on a stream in the <code>DELETING</code> state until the stream deletion is complete.</p> <p>When you delete a stream, any shards in that stream are also deleted, and any tags are dissociated from the stream.</p> <p>You can use the <a>DescribeStream</a> operation to check the state of the stream, which is returned in <code>StreamStatus</code>.</p> <p> <a>DeleteStream</a> has a limit of five transactions per second per account.</p>
deleteStream :: forall eff. DeleteStreamInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteStream = AWS.request serviceName "DeleteStream" 


-- | <p>Describes the shard limits and usage for the account.</p> <p>If you update your account limits, the old limits might be returned for a few minutes.</p> <p>This operation has a limit of one transaction per second per account.</p>
describeLimits :: forall eff. DescribeLimitsInput -> Aff (err :: AWS.RequestError | eff) DescribeLimitsOutput
describeLimits = AWS.request serviceName "DescribeLimits" 


-- | <p>Describes the specified Kinesis data stream.</p> <p>The information returned includes the stream name, Amazon Resource Name (ARN), creation time, enhanced metric configuration, and shard map. The shard map is an array of shard objects. For each shard object, there is the hash key and sequence number ranges that the shard spans, and the IDs of any earlier shards that played in a role in creating the shard. Every record ingested in the stream is identified by a sequence number, which is assigned when the record is put into the stream.</p> <p>You can limit the number of shards returned by each call. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-retrieve-shards.html">Retrieving Shards from a Stream</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p>There are no guarantees about the chronological order shards returned. To process shards in chronological order, use the ID of the parent shard to track the lineage to the oldest shard.</p> <p>This operation has a limit of 10 transactions per second per account.</p>
describeStream :: forall eff. DescribeStreamInput -> Aff (err :: AWS.RequestError | eff) DescribeStreamOutput
describeStream = AWS.request serviceName "DescribeStream" 


-- | <p>Provides a summarized description of the specified Kinesis data stream without the shard list.</p> <p>The information returned includes the stream name, Amazon Resource Name (ARN), status, record retention period, approximate creation time, monitoring, encryption details, and open shard count. </p>
describeStreamSummary :: forall eff. DescribeStreamSummaryInput -> Aff (err :: AWS.RequestError | eff) DescribeStreamSummaryOutput
describeStreamSummary = AWS.request serviceName "DescribeStreamSummary" 


-- | <p>Disables enhanced monitoring.</p>
disableEnhancedMonitoring :: forall eff. DisableEnhancedMonitoringInput -> Aff (err :: AWS.RequestError | eff) EnhancedMonitoringOutput
disableEnhancedMonitoring = AWS.request serviceName "DisableEnhancedMonitoring" 


-- | <p>Enables enhanced Kinesis data stream monitoring for shard-level metrics.</p>
enableEnhancedMonitoring :: forall eff. EnableEnhancedMonitoringInput -> Aff (err :: AWS.RequestError | eff) EnhancedMonitoringOutput
enableEnhancedMonitoring = AWS.request serviceName "EnableEnhancedMonitoring" 


-- | <p>Gets data records from a Kinesis data stream's shard.</p> <p>Specify a shard iterator using the <code>ShardIterator</code> parameter. The shard iterator specifies the position in the shard from which you want to start reading data records sequentially. If there are no records available in the portion of the shard that the iterator points to, <a>GetRecords</a> returns an empty list. It might take multiple calls to get to a portion of the shard that contains records.</p> <p>You can scale by provisioning multiple shards per stream while considering service limits (for more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html">Amazon Kinesis Data Streams Limits</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>). Your application should have one thread per shard, each reading continuously from its stream. To read from a stream continually, call <a>GetRecords</a> in a loop. Use <a>GetShardIterator</a> to get the shard iterator to specify in the first <a>GetRecords</a> call. <a>GetRecords</a> returns a new shard iterator in <code>NextShardIterator</code>. Specify the shard iterator returned in <code>NextShardIterator</code> in subsequent calls to <a>GetRecords</a>. If the shard has been closed, the shard iterator can't return more data and <a>GetRecords</a> returns <code>null</code> in <code>NextShardIterator</code>. You can terminate the loop when the shard is closed, or when the shard iterator reaches the record with the sequence number or other attribute that marks it as the last record to process.</p> <p>Each data record can be up to 1 MB in size, and each shard can read up to 2 MB per second. You can ensure that your calls don't exceed the maximum supported size or throughput by using the <code>Limit</code> parameter to specify the maximum number of records that <a>GetRecords</a> can return. Consider your average record size when determining this limit.</p> <p>The size of the data returned by <a>GetRecords</a> varies depending on the utilization of the shard. The maximum size of data that <a>GetRecords</a> can return is 10 MB. If a call returns this amount of data, subsequent calls made within the next five seconds throw <code>ProvisionedThroughputExceededException</code>. If there is insufficient provisioned throughput on the stream, subsequent calls made within the next one second throw <code>ProvisionedThroughputExceededException</code>. <a>GetRecords</a> won't return any data when it throws an exception. For this reason, we recommend that you wait one second between calls to <a>GetRecords</a>; however, it's possible that the application will get exceptions for longer than 1 second.</p> <p>To detect whether the application is falling behind in processing, you can use the <code>MillisBehindLatest</code> response attribute. You can also monitor the stream using CloudWatch metrics and other mechanisms (see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/monitoring.html">Monitoring</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>).</p> <p>Each Amazon Kinesis record includes a value, <code>ApproximateArrivalTimestamp</code>, that is set when a stream successfully receives and stores a record. This is commonly referred to as a server-side time stamp, whereas a client-side time stamp is set when a data producer creates or sends the record to a stream (a data producer is any data source putting data records into a stream, for example with <a>PutRecords</a>). The time stamp has millisecond precision. There are no guarantees about the time stamp accuracy, or that the time stamp is always increasing. For example, records in a shard or across a stream might have time stamps that are out of order.</p>
getRecords :: forall eff. GetRecordsInput -> Aff (err :: AWS.RequestError | eff) GetRecordsOutput
getRecords = AWS.request serviceName "GetRecords" 


-- | <p>Gets an Amazon Kinesis shard iterator. A shard iterator expires five minutes after it is returned to the requester.</p> <p>A shard iterator specifies the shard position from which to start reading data records sequentially. The position is specified using the sequence number of a data record in a shard. A sequence number is the identifier associated with every record ingested in the stream, and is assigned when a record is put into the stream. Each stream has one or more shards.</p> <p>You must specify the shard iterator type. For example, you can set the <code>ShardIteratorType</code> parameter to read exactly from the position denoted by a specific sequence number by using the <code>AT_SEQUENCE_NUMBER</code> shard iterator type. Alternatively, the parameter can read right after the sequence number by using the <code>AFTER_SEQUENCE_NUMBER</code> shard iterator type, using sequence numbers returned by earlier calls to <a>PutRecord</a>, <a>PutRecords</a>, <a>GetRecords</a>, or <a>DescribeStream</a>. In the request, you can specify the shard iterator type <code>AT_TIMESTAMP</code> to read records from an arbitrary point in time, <code>TRIM_HORIZON</code> to cause <code>ShardIterator</code> to point to the last untrimmed record in the shard in the system (the oldest data record in the shard), or <code>LATEST</code> so that you always read the most recent data in the shard. </p> <p>When you read repeatedly from a stream, use a <a>GetShardIterator</a> request to get the first shard iterator for use in your first <a>GetRecords</a> request and for subsequent reads use the shard iterator returned by the <a>GetRecords</a> request in <code>NextShardIterator</code>. A new shard iterator is returned by every <a>GetRecords</a> request in <code>NextShardIterator</code>, which you use in the <code>ShardIterator</code> parameter of the next <a>GetRecords</a> request. </p> <p>If a <a>GetShardIterator</a> request is made too often, you receive a <code>ProvisionedThroughputExceededException</code>. For more information about throughput limits, see <a>GetRecords</a>, and <a href="http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html">Streams Limits</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p>If the shard is closed, <a>GetShardIterator</a> returns a valid iterator for the last sequence number of the shard. A shard can be closed as a result of using <a>SplitShard</a> or <a>MergeShards</a>.</p> <p> <a>GetShardIterator</a> has a limit of five transactions per second per account per open shard.</p>
getShardIterator :: forall eff. GetShardIteratorInput -> Aff (err :: AWS.RequestError | eff) GetShardIteratorOutput
getShardIterator = AWS.request serviceName "GetShardIterator" 


-- | <p>Increases the Kinesis data stream's retention period, which is the length of time data records are accessible after they are added to the stream. The maximum value of a stream's retention period is 168 hours (7 days).</p> <p>If you choose a longer stream retention period, this operation increases the time period during which records that have not yet expired are accessible. However, it does not make previous, expired data (older than the stream's previous retention period) accessible after the operation has been called. For example, if a stream's retention period is set to 24 hours and is increased to 168 hours, any data that is older than 24 hours remains inaccessible to consumer applications.</p>
increaseStreamRetentionPeriod :: forall eff. IncreaseStreamRetentionPeriodInput -> Aff (err :: AWS.RequestError | eff) Unit
increaseStreamRetentionPeriod = AWS.request serviceName "IncreaseStreamRetentionPeriod" 


-- | <p>Lists the shards in a stream and provides information about each shard.</p> <important> <p>This API is a new operation that is used by the Amazon Kinesis Client Library (KCL). If you have a fine-grained IAM policy that only allows specific operations, you must update your policy to allow calls to this API. For more information, see <a href="https://docs.aws.amazon.com/streams/latest/dev/controlling-access.html">Controlling Access to Amazon Kinesis Data Streams Resources Using IAM</a>.</p> </important>
listShards :: forall eff. ListShardsInput -> Aff (err :: AWS.RequestError | eff) ListShardsOutput
listShards = AWS.request serviceName "ListShards" 


-- | <p>Lists your Kinesis data streams.</p> <p>The number of streams may be too large to return from a single call to <code>ListStreams</code>. You can limit the number of returned streams using the <code>Limit</code> parameter. If you do not specify a value for the <code>Limit</code> parameter, Kinesis Data Streams uses the default limit, which is currently 10.</p> <p>You can detect if there are more streams available to list by using the <code>HasMoreStreams</code> flag from the returned output. If there are more streams available, you can request more streams by using the name of the last stream returned by the <code>ListStreams</code> request in the <code>ExclusiveStartStreamName</code> parameter in a subsequent request to <code>ListStreams</code>. The group of stream names returned by the subsequent request is then added to the list. You can continue this process until all the stream names have been collected in the list. </p> <p> <a>ListStreams</a> has a limit of five transactions per second per account.</p>
listStreams :: forall eff. ListStreamsInput -> Aff (err :: AWS.RequestError | eff) ListStreamsOutput
listStreams = AWS.request serviceName "ListStreams" 


-- | <p>Lists the tags for the specified Kinesis data stream. This operation has a limit of five transactions per second per account.</p>
listTagsForStream :: forall eff. ListTagsForStreamInput -> Aff (err :: AWS.RequestError | eff) ListTagsForStreamOutput
listTagsForStream = AWS.request serviceName "ListTagsForStream" 


-- | <p>Merges two adjacent shards in a Kinesis data stream and combines them into a single shard to reduce the stream's capacity to ingest and transport data. Two shards are considered adjacent if the union of the hash key ranges for the two shards form a contiguous set with no gaps. For example, if you have two shards, one with a hash key range of 276...381 and the other with a hash key range of 382...454, then you could merge these two shards into a single shard that would have a hash key range of 276...454. After the merge, the single child shard receives data for all hash key values covered by the two parent shards.</p> <p> <code>MergeShards</code> is called when there is a need to reduce the overall capacity of a stream because of excess capacity that is not being used. You must specify the shard to be merged and the adjacent shard for a stream. For more information about merging shards, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-resharding-merge.html">Merge Two Shards</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p>If the stream is in the <code>ACTIVE</code> state, you can call <code>MergeShards</code>. If a stream is in the <code>CREATING</code>, <code>UPDATING</code>, or <code>DELETING</code> state, <code>MergeShards</code> returns a <code>ResourceInUseException</code>. If the specified stream does not exist, <code>MergeShards</code> returns a <code>ResourceNotFoundException</code>. </p> <p>You can use <a>DescribeStream</a> to check the state of the stream, which is returned in <code>StreamStatus</code>.</p> <p> <code>MergeShards</code> is an asynchronous operation. Upon receiving a <code>MergeShards</code> request, Amazon Kinesis Data Streams immediately returns a response and sets the <code>StreamStatus</code> to <code>UPDATING</code>. After the operation is completed, Kinesis Data Streams sets the <code>StreamStatus</code> to <code>ACTIVE</code>. Read and write operations continue to work while the stream is in the <code>UPDATING</code> state. </p> <p>You use <a>DescribeStream</a> to determine the shard IDs that are specified in the <code>MergeShards</code> request. </p> <p>If you try to operate on too many streams in parallel using <a>CreateStream</a>, <a>DeleteStream</a>, <code>MergeShards</code>, or <a>SplitShard</a>, you receive a <code>LimitExceededException</code>. </p> <p> <code>MergeShards</code> has a limit of five transactions per second per account.</p>
mergeShards :: forall eff. MergeShardsInput -> Aff (err :: AWS.RequestError | eff) Unit
mergeShards = AWS.request serviceName "MergeShards" 


-- | <p>Writes a single data record into an Amazon Kinesis data stream. Call <code>PutRecord</code> to send data into the stream for real-time ingestion and subsequent processing, one record at a time. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MB per second.</p> <p>You must specify the name of the stream that captures, stores, and transports the data; a partition key; and the data blob itself.</p> <p>The data blob can be any type of data; for example, a segment from a log file, geographic/location data, website clickstream data, and so on.</p> <p>The partition key is used by Kinesis Data Streams to distribute data across shards. Kinesis Data Streams segregates the data records that belong to a stream into multiple shards, using the partition key associated with each data record to determine the shard to which a given data record belongs.</p> <p>Partition keys are Unicode strings, with a maximum length limit of 256 characters for each key. An MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards using the hash key ranges of the shards. You can override hashing the partition key to determine the shard by explicitly specifying a hash value using the <code>ExplicitHashKey</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream">Adding Data to a Stream</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p> <code>PutRecord</code> returns the shard ID of where the data record was placed and the sequence number that was assigned to the data record.</p> <p>Sequence numbers increase over time and are specific to a shard within a stream, not across all shards within a stream. To guarantee strictly increasing ordering, write serially to a shard and use the <code>SequenceNumberForOrdering</code> parameter. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream">Adding Data to a Stream</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p>If a <code>PutRecord</code> request cannot be processed because of insufficient provisioned throughput on the shard involved in the request, <code>PutRecord</code> throws <code>ProvisionedThroughputExceededException</code>. </p> <p>By default, data records are accessible for 24 hours from the time that they are added to a stream. You can use <a>IncreaseStreamRetentionPeriod</a> or <a>DecreaseStreamRetentionPeriod</a> to modify this retention period.</p>
putRecord :: forall eff. PutRecordInput -> Aff (err :: AWS.RequestError | eff) PutRecordOutput
putRecord = AWS.request serviceName "PutRecord" 


-- | <p>Writes multiple data records into a Kinesis data stream in a single call (also referred to as a <code>PutRecords</code> request). Use this operation to send data into the stream for data ingestion and processing. </p> <p>Each <code>PutRecords</code> request can support up to 500 records. Each record in the request can be as large as 1 MB, up to a limit of 5 MB for the entire request, including partition keys. Each shard can support writes up to 1,000 records per second, up to a maximum data write total of 1 MB per second.</p> <p>You must specify the name of the stream that captures, stores, and transports the data; and an array of request <code>Records</code>, with each record in the array requiring a partition key and data blob. The record size limit applies to the total size of the partition key and data blob.</p> <p>The data blob can be any type of data; for example, a segment from a log file, geographic/location data, website clickstream data, and so on.</p> <p>The partition key is used by Kinesis Data Streams as input to a hash function that maps the partition key and associated data to a specific shard. An MD5 hash function is used to map partition keys to 128-bit integer values and to map associated data records to shards. As a result of this hashing mechanism, all data records with the same partition key map to the same shard within the stream. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-add-data-to-stream">Adding Data to a Stream</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p>Each record in the <code>Records</code> array may include an optional parameter, <code>ExplicitHashKey</code>, which overrides the partition key to shard mapping. This parameter allows a data producer to determine explicitly the shard where the record is stored. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/developing-producers-with-sdk.html#kinesis-using-sdk-java-putrecords">Adding Multiple Records with PutRecords</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p>The <code>PutRecords</code> response includes an array of response <code>Records</code>. Each record in the response array directly correlates with a record in the request array using natural ordering, from the top to the bottom of the request and response. The response <code>Records</code> array always includes the same number of records as the request array.</p> <p>The response <code>Records</code> array includes both successfully and unsuccessfully processed records. Kinesis Data Streams attempts to process all records in each <code>PutRecords</code> request. A single record failure does not stop the processing of subsequent records.</p> <p>A successfully processed record includes <code>ShardId</code> and <code>SequenceNumber</code> values. The <code>ShardId</code> parameter identifies the shard in the stream where the record is stored. The <code>SequenceNumber</code> parameter is an identifier assigned to the put record, unique to all records in the stream.</p> <p>An unsuccessfully processed record includes <code>ErrorCode</code> and <code>ErrorMessage</code> values. <code>ErrorCode</code> reflects the type of error and can be one of the following values: <code>ProvisionedThroughputExceededException</code> or <code>InternalFailure</code>. <code>ErrorMessage</code> provides more detailed information about the <code>ProvisionedThroughputExceededException</code> exception including the account ID, stream name, and shard ID of the record that was throttled. For more information about partially successful responses, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-add-data-to-stream.html#kinesis-using-sdk-java-putrecords">Adding Multiple Records with PutRecords</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p>By default, data records are accessible for 24 hours from the time that they are added to a stream. You can use <a>IncreaseStreamRetentionPeriod</a> or <a>DecreaseStreamRetentionPeriod</a> to modify this retention period.</p>
putRecords :: forall eff. PutRecordsInput -> Aff (err :: AWS.RequestError | eff) PutRecordsOutput
putRecords = AWS.request serviceName "PutRecords" 


-- | <p>Removes tags from the specified Kinesis data stream. Removed tags are deleted and cannot be recovered after this operation successfully completes.</p> <p>If you specify a tag that does not exist, it is ignored.</p> <p> <a>RemoveTagsFromStream</a> has a limit of five transactions per second per account.</p>
removeTagsFromStream :: forall eff. RemoveTagsFromStreamInput -> Aff (err :: AWS.RequestError | eff) Unit
removeTagsFromStream = AWS.request serviceName "RemoveTagsFromStream" 


-- | <p>Splits a shard into two new shards in the Kinesis data stream, to increase the stream's capacity to ingest and transport data. <code>SplitShard</code> is called when there is a need to increase the overall capacity of a stream because of an expected increase in the volume of data records being ingested. </p> <p>You can also use <code>SplitShard</code> when a shard appears to be approaching its maximum utilization; for example, the producers sending data into the specific shard are suddenly sending more than previously anticipated. You can also call <code>SplitShard</code> to increase stream capacity, so that more Kinesis Data Streams applications can simultaneously read data from the stream for real-time processing. </p> <p>You must specify the shard to be split and the new hash key, which is the position in the shard where the shard gets split in two. In many cases, the new hash key might be the average of the beginning and ending hash key, but it can be any hash key value in the range being mapped into the shard. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/kinesis-using-sdk-java-resharding-split.html">Split a Shard</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>.</p> <p>You can use <a>DescribeStream</a> to determine the shard ID and hash key values for the <code>ShardToSplit</code> and <code>NewStartingHashKey</code> parameters that are specified in the <code>SplitShard</code> request.</p> <p> <code>SplitShard</code> is an asynchronous operation. Upon receiving a <code>SplitShard</code> request, Kinesis Data Streams immediately returns a response and sets the stream status to <code>UPDATING</code>. After the operation is completed, Kinesis Data Streams sets the stream status to <code>ACTIVE</code>. Read and write operations continue to work while the stream is in the <code>UPDATING</code> state. </p> <p>You can use <code>DescribeStream</code> to check the status of the stream, which is returned in <code>StreamStatus</code>. If the stream is in the <code>ACTIVE</code> state, you can call <code>SplitShard</code>. If a stream is in <code>CREATING</code> or <code>UPDATING</code> or <code>DELETING</code> states, <code>DescribeStream</code> returns a <code>ResourceInUseException</code>.</p> <p>If the specified stream does not exist, <code>DescribeStream</code> returns a <code>ResourceNotFoundException</code>. If you try to create more shards than are authorized for your account, you receive a <code>LimitExceededException</code>. </p> <p>For the default shard limit for an AWS account, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html">Streams Limits</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>. To increase this limit, <a href="http://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html">contact AWS Support</a>.</p> <p>If you try to operate on too many streams simultaneously using <a>CreateStream</a>, <a>DeleteStream</a>, <a>MergeShards</a>, and/or <a>SplitShard</a>, you receive a <code>LimitExceededException</code>. </p> <p> <code>SplitShard</code> has a limit of five transactions per second per account.</p>
splitShard :: forall eff. SplitShardInput -> Aff (err :: AWS.RequestError | eff) Unit
splitShard = AWS.request serviceName "SplitShard" 


-- | <p>Enables or updates server-side encryption using an AWS KMS key for a specified stream. </p> <p>Starting encryption is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to <code>UPDATING</code>. After the update is complete, Kinesis Data Streams sets the status of the stream back to <code>ACTIVE</code>. Updating or applying encryption normally takes a few seconds to complete, but it can take minutes. You can continue to read and write data to your stream while its status is <code>UPDATING</code>. Once the status of the stream is <code>ACTIVE</code>, encryption begins for records written to the stream. </p> <p>API Limits: You can successfully apply a new AWS KMS key for server-side encryption 25 times in a rolling 24-hour period.</p> <p>Note: It can take up to five seconds after the stream is in an <code>ACTIVE</code> status before all records written to the stream are encrypted. After you enable encryption, you can verify that encryption is applied by inspecting the API response from <code>PutRecord</code> or <code>PutRecords</code>.</p>
startStreamEncryption :: forall eff. StartStreamEncryptionInput -> Aff (err :: AWS.RequestError | eff) Unit
startStreamEncryption = AWS.request serviceName "StartStreamEncryption" 


-- | <p>Disables server-side encryption for a specified stream. </p> <p>Stopping encryption is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to <code>UPDATING</code>. After the update is complete, Kinesis Data Streams sets the status of the stream back to <code>ACTIVE</code>. Stopping encryption normally takes a few seconds to complete, but it can take minutes. You can continue to read and write data to your stream while its status is <code>UPDATING</code>. Once the status of the stream is <code>ACTIVE</code>, records written to the stream are no longer encrypted by Kinesis Data Streams. </p> <p>API Limits: You can successfully disable server-side encryption 25 times in a rolling 24-hour period. </p> <p>Note: It can take up to five seconds after the stream is in an <code>ACTIVE</code> status before all records written to the stream are no longer subject to encryption. After you disabled encryption, you can verify that encryption is not applied by inspecting the API response from <code>PutRecord</code> or <code>PutRecords</code>.</p>
stopStreamEncryption :: forall eff. StopStreamEncryptionInput -> Aff (err :: AWS.RequestError | eff) Unit
stopStreamEncryption = AWS.request serviceName "StopStreamEncryption" 


-- | <p>Updates the shard count of the specified stream to the specified number of shards.</p> <p>Updating the shard count is an asynchronous operation. Upon receiving the request, Kinesis Data Streams returns immediately and sets the status of the stream to <code>UPDATING</code>. After the update is complete, Kinesis Data Streams sets the status of the stream back to <code>ACTIVE</code>. Depending on the size of the stream, the scaling action could take a few minutes to complete. You can continue to read and write data to your stream while its status is <code>UPDATING</code>.</p> <p>To update the shard count, Kinesis Data Streams performs splits or merges on individual shards. This can cause short-lived shards to be created, in addition to the final shards. We recommend that you double or halve the shard count, as this results in the fewest number of splits or merges.</p> <p>This operation has the following limits. You cannot do the following:</p> <ul> <li> <p>Scale more than twice per rolling 24-hour period per stream</p> </li> <li> <p>Scale up to more than double your current shard count for a stream</p> </li> <li> <p>Scale down below half your current shard count for a stream</p> </li> <li> <p>Scale up to more than 500 shards in a stream</p> </li> <li> <p>Scale a stream with more than 500 shards down unless the result is less than 500 shards</p> </li> <li> <p>Scale up to more than the shard limit for your account</p> </li> </ul> <p>For the default limits for an AWS account, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html">Streams Limits</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>. To request an increase in the call rate limit, the shard limit for this API, or your overall shard limit, use the <a href="https://console.aws.amazon.com/support/v1#/case/create?issueType=service-limit-increase&amp;limitType=service-code-kinesis">limits form</a>.</p>
updateShardCount :: forall eff. UpdateShardCountInput -> Aff (err :: AWS.RequestError | eff) UpdateShardCountOutput
updateShardCount = AWS.request serviceName "UpdateShardCount" 


-- | <p>Represents the input for <code>AddTagsToStream</code>.</p>
newtype AddTagsToStreamInput = AddTagsToStreamInput 
  { "StreamName" :: (StreamName)
  , "Tags" :: (TagMap)
  }
derive instance newtypeAddTagsToStreamInput :: Newtype AddTagsToStreamInput _


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _


-- | <p>Represents the input for <code>CreateStream</code>.</p>
newtype CreateStreamInput = CreateStreamInput 
  { "StreamName" :: (StreamName)
  , "ShardCount" :: (PositiveIntegerObject)
  }
derive instance newtypeCreateStreamInput :: Newtype CreateStreamInput _


newtype Data = Data String
derive instance newtypeData :: Newtype Data _


-- | <p>Represents the input for <a>DecreaseStreamRetentionPeriod</a>.</p>
newtype DecreaseStreamRetentionPeriodInput = DecreaseStreamRetentionPeriodInput 
  { "StreamName" :: (StreamName)
  , "RetentionPeriodHours" :: (RetentionPeriodHours)
  }
derive instance newtypeDecreaseStreamRetentionPeriodInput :: Newtype DecreaseStreamRetentionPeriodInput _


-- | <p>Represents the input for <a>DeleteStream</a>.</p>
newtype DeleteStreamInput = DeleteStreamInput 
  { "StreamName" :: (StreamName)
  }
derive instance newtypeDeleteStreamInput :: Newtype DeleteStreamInput _


newtype DescribeLimitsInput = DescribeLimitsInput 
  { 
  }
derive instance newtypeDescribeLimitsInput :: Newtype DescribeLimitsInput _


newtype DescribeLimitsOutput = DescribeLimitsOutput 
  { "ShardLimit" :: (ShardCountObject)
  , "OpenShardCount" :: (ShardCountObject)
  }
derive instance newtypeDescribeLimitsOutput :: Newtype DescribeLimitsOutput _


-- | <p>Represents the input for <code>DescribeStream</code>.</p>
newtype DescribeStreamInput = DescribeStreamInput 
  { "StreamName" :: (StreamName)
  , "Limit" :: NullOrUndefined (DescribeStreamInputLimit)
  , "ExclusiveStartShardId" :: NullOrUndefined (ShardId)
  }
derive instance newtypeDescribeStreamInput :: Newtype DescribeStreamInput _


newtype DescribeStreamInputLimit = DescribeStreamInputLimit Int
derive instance newtypeDescribeStreamInputLimit :: Newtype DescribeStreamInputLimit _


-- | <p>Represents the output for <code>DescribeStream</code>.</p>
newtype DescribeStreamOutput = DescribeStreamOutput 
  { "StreamDescription" :: (StreamDescription)
  }
derive instance newtypeDescribeStreamOutput :: Newtype DescribeStreamOutput _


newtype DescribeStreamSummaryInput = DescribeStreamSummaryInput 
  { "StreamName" :: (StreamName)
  }
derive instance newtypeDescribeStreamSummaryInput :: Newtype DescribeStreamSummaryInput _


newtype DescribeStreamSummaryOutput = DescribeStreamSummaryOutput 
  { "StreamDescriptionSummary" :: (StreamDescriptionSummary)
  }
derive instance newtypeDescribeStreamSummaryOutput :: Newtype DescribeStreamSummaryOutput _


-- | <p>Represents the input for <a>DisableEnhancedMonitoring</a>.</p>
newtype DisableEnhancedMonitoringInput = DisableEnhancedMonitoringInput 
  { "StreamName" :: (StreamName)
  , "ShardLevelMetrics" :: (MetricsNameList)
  }
derive instance newtypeDisableEnhancedMonitoringInput :: Newtype DisableEnhancedMonitoringInput _


-- | <p>Represents the input for <a>EnableEnhancedMonitoring</a>.</p>
newtype EnableEnhancedMonitoringInput = EnableEnhancedMonitoringInput 
  { "StreamName" :: (StreamName)
  , "ShardLevelMetrics" :: (MetricsNameList)
  }
derive instance newtypeEnableEnhancedMonitoringInput :: Newtype EnableEnhancedMonitoringInput _


newtype EncryptionType = EncryptionType String
derive instance newtypeEncryptionType :: Newtype EncryptionType _


-- | <p>Represents enhanced metrics types.</p>
newtype EnhancedMetrics = EnhancedMetrics 
  { "ShardLevelMetrics" :: NullOrUndefined (MetricsNameList)
  }
derive instance newtypeEnhancedMetrics :: Newtype EnhancedMetrics _


newtype EnhancedMonitoringList = EnhancedMonitoringList (Array EnhancedMetrics)
derive instance newtypeEnhancedMonitoringList :: Newtype EnhancedMonitoringList _


-- | <p>Represents the output for <a>EnableEnhancedMonitoring</a> and <a>DisableEnhancedMonitoring</a>.</p>
newtype EnhancedMonitoringOutput = EnhancedMonitoringOutput 
  { "StreamName" :: NullOrUndefined (StreamName)
  , "CurrentShardLevelMetrics" :: NullOrUndefined (MetricsNameList)
  , "DesiredShardLevelMetrics" :: NullOrUndefined (MetricsNameList)
  }
derive instance newtypeEnhancedMonitoringOutput :: Newtype EnhancedMonitoringOutput _


newtype ErrorCode = ErrorCode String
derive instance newtypeErrorCode :: Newtype ErrorCode _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p>The provided iterator exceeds the maximum age allowed.</p>
newtype ExpiredIteratorException = ExpiredIteratorException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExpiredIteratorException :: Newtype ExpiredIteratorException _


-- | <p>The pagination token passed to the <code>ListShards</code> operation is expired. For more information, see <a>ListShardsInput$NextToken</a>.</p>
newtype ExpiredNextTokenException = ExpiredNextTokenException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExpiredNextTokenException :: Newtype ExpiredNextTokenException _


-- | <p>Represents the input for <a>GetRecords</a>.</p>
newtype GetRecordsInput = GetRecordsInput 
  { "ShardIterator" :: (ShardIterator)
  , "Limit" :: NullOrUndefined (GetRecordsInputLimit)
  }
derive instance newtypeGetRecordsInput :: Newtype GetRecordsInput _


newtype GetRecordsInputLimit = GetRecordsInputLimit Int
derive instance newtypeGetRecordsInputLimit :: Newtype GetRecordsInputLimit _


-- | <p>Represents the output for <a>GetRecords</a>.</p>
newtype GetRecordsOutput = GetRecordsOutput 
  { "Records" :: (RecordList)
  , "NextShardIterator" :: NullOrUndefined (ShardIterator)
  , "MillisBehindLatest" :: NullOrUndefined (MillisBehindLatest)
  }
derive instance newtypeGetRecordsOutput :: Newtype GetRecordsOutput _


-- | <p>Represents the input for <code>GetShardIterator</code>.</p>
newtype GetShardIteratorInput = GetShardIteratorInput 
  { "StreamName" :: (StreamName)
  , "ShardId" :: (ShardId)
  , "ShardIteratorType" :: (ShardIteratorType)
  , "StartingSequenceNumber" :: NullOrUndefined (SequenceNumber)
  , "Number" :: NullOrUndefined (Number)
  }
derive instance newtypeGetShardIteratorInput :: Newtype GetShardIteratorInput _


-- | <p>Represents the output for <code>GetShardIterator</code>.</p>
newtype GetShardIteratorOutput = GetShardIteratorOutput 
  { "ShardIterator" :: NullOrUndefined (ShardIterator)
  }
derive instance newtypeGetShardIteratorOutput :: Newtype GetShardIteratorOutput _


newtype HashKey = HashKey String
derive instance newtypeHashKey :: Newtype HashKey _


-- | <p>The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.</p>
newtype HashKeyRange = HashKeyRange 
  { "StartingHashKey" :: (HashKey)
  , "EndingHashKey" :: (HashKey)
  }
derive instance newtypeHashKeyRange :: Newtype HashKeyRange _


-- | <p>Represents the input for <a>IncreaseStreamRetentionPeriod</a>.</p>
newtype IncreaseStreamRetentionPeriodInput = IncreaseStreamRetentionPeriodInput 
  { "StreamName" :: (StreamName)
  , "RetentionPeriodHours" :: (RetentionPeriodHours)
  }
derive instance newtypeIncreaseStreamRetentionPeriodInput :: Newtype IncreaseStreamRetentionPeriodInput _


-- | <p>A specified parameter exceeds its restrictions, is not supported, or can't be used. For more information, see the returned message.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _


-- | <p>The ciphertext references a key that doesn't exist or that you don't have access to.</p>
newtype KMSAccessDeniedException = KMSAccessDeniedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeKMSAccessDeniedException :: Newtype KMSAccessDeniedException _


-- | <p>The request was rejected because the specified customer master key (CMK) isn't enabled.</p>
newtype KMSDisabledException = KMSDisabledException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeKMSDisabledException :: Newtype KMSDisabledException _


-- | <p>The request was rejected because the state of the specified resource isn't valid for this request. For more information, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/key-state.html">How Key State Affects Use of a Customer Master Key</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
newtype KMSInvalidStateException = KMSInvalidStateException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeKMSInvalidStateException :: Newtype KMSInvalidStateException _


-- | <p>The request was rejected because the specified entity or resource can't be found.</p>
newtype KMSNotFoundException = KMSNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeKMSNotFoundException :: Newtype KMSNotFoundException _


-- | <p>The AWS access key ID needs a subscription for the service.</p>
newtype KMSOptInRequired = KMSOptInRequired 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeKMSOptInRequired :: Newtype KMSOptInRequired _


-- | <p>The request was denied due to request throttling. For more information about throttling, see <a href="http://docs.aws.amazon.com/kms/latest/developerguide/limits.html#requests-per-second">Limits</a> in the <i>AWS Key Management Service Developer Guide</i>.</p>
newtype KMSThrottlingException = KMSThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeKMSThrottlingException :: Newtype KMSThrottlingException _


newtype KeyId = KeyId String
derive instance newtypeKeyId :: Newtype KeyId _


-- | <p>The requested resource exceeds the maximum number allowed, or the number of concurrent stream requests exceeds the maximum number allowed. </p>
newtype LimitExceededException = LimitExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListShardsInput = ListShardsInput 
  { "StreamName" :: NullOrUndefined (StreamName)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "ExclusiveStartShardId" :: NullOrUndefined (ShardId)
  , "MaxResults" :: NullOrUndefined (ListShardsInputLimit)
  , "StreamCreationTimestamp" :: NullOrUndefined (Number)
  }
derive instance newtypeListShardsInput :: Newtype ListShardsInput _


newtype ListShardsInputLimit = ListShardsInputLimit Int
derive instance newtypeListShardsInputLimit :: Newtype ListShardsInputLimit _


newtype ListShardsOutput = ListShardsOutput 
  { "Shards" :: NullOrUndefined (ShardList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListShardsOutput :: Newtype ListShardsOutput _


-- | <p>Represents the input for <code>ListStreams</code>.</p>
newtype ListStreamsInput = ListStreamsInput 
  { "Limit" :: NullOrUndefined (ListStreamsInputLimit)
  , "ExclusiveStartStreamName" :: NullOrUndefined (StreamName)
  }
derive instance newtypeListStreamsInput :: Newtype ListStreamsInput _


newtype ListStreamsInputLimit = ListStreamsInputLimit Int
derive instance newtypeListStreamsInputLimit :: Newtype ListStreamsInputLimit _


-- | <p>Represents the output for <code>ListStreams</code>.</p>
newtype ListStreamsOutput = ListStreamsOutput 
  { "StreamNames" :: (StreamNameList)
  , "HasMoreStreams" :: (BooleanObject)
  }
derive instance newtypeListStreamsOutput :: Newtype ListStreamsOutput _


-- | <p>Represents the input for <code>ListTagsForStream</code>.</p>
newtype ListTagsForStreamInput = ListTagsForStreamInput 
  { "StreamName" :: (StreamName)
  , "ExclusiveStartTagKey" :: NullOrUndefined (TagKey)
  , "Limit" :: NullOrUndefined (ListTagsForStreamInputLimit)
  }
derive instance newtypeListTagsForStreamInput :: Newtype ListTagsForStreamInput _


newtype ListTagsForStreamInputLimit = ListTagsForStreamInputLimit Int
derive instance newtypeListTagsForStreamInputLimit :: Newtype ListTagsForStreamInputLimit _


-- | <p>Represents the output for <code>ListTagsForStream</code>.</p>
newtype ListTagsForStreamOutput = ListTagsForStreamOutput 
  { "Tags" :: (TagList)
  , "HasMoreTags" :: (BooleanObject)
  }
derive instance newtypeListTagsForStreamOutput :: Newtype ListTagsForStreamOutput _


-- | <p>Represents the input for <code>MergeShards</code>.</p>
newtype MergeShardsInput = MergeShardsInput 
  { "StreamName" :: (StreamName)
  , "ShardToMerge" :: (ShardId)
  , "AdjacentShardToMerge" :: (ShardId)
  }
derive instance newtypeMergeShardsInput :: Newtype MergeShardsInput _


newtype MetricsName = MetricsName String
derive instance newtypeMetricsName :: Newtype MetricsName _


newtype MetricsNameList = MetricsNameList (Array MetricsName)
derive instance newtypeMetricsNameList :: Newtype MetricsNameList _


newtype MillisBehindLatest = MillisBehindLatest Number
derive instance newtypeMillisBehindLatest :: Newtype MillisBehindLatest _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype PartitionKey = PartitionKey String
derive instance newtypePartitionKey :: Newtype PartitionKey _


newtype PositiveIntegerObject = PositiveIntegerObject Int
derive instance newtypePositiveIntegerObject :: Newtype PositiveIntegerObject _


-- | <p>The request rate for the stream is too high, or the requested data is too large for the available throughput. Reduce the frequency or size of your requests. For more information, see <a href="http://docs.aws.amazon.com/kinesis/latest/dev/service-sizes-and-limits.html">Streams Limits</a> in the <i>Amazon Kinesis Data Streams Developer Guide</i>, and <a href="http://docs.aws.amazon.com/general/latest/gr/api-retries.html">Error Retries and Exponential Backoff in AWS</a> in the <i>AWS General Reference</i>.</p>
newtype ProvisionedThroughputExceededException = ProvisionedThroughputExceededException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeProvisionedThroughputExceededException :: Newtype ProvisionedThroughputExceededException _


-- | <p>Represents the input for <code>PutRecord</code>.</p>
newtype PutRecordInput = PutRecordInput 
  { "StreamName" :: (StreamName)
  , "Data" :: (Data)
  , "PartitionKey" :: (PartitionKey)
  , "ExplicitHashKey" :: NullOrUndefined (HashKey)
  , "SequenceNumberForOrdering" :: NullOrUndefined (SequenceNumber)
  }
derive instance newtypePutRecordInput :: Newtype PutRecordInput _


-- | <p>Represents the output for <code>PutRecord</code>.</p>
newtype PutRecordOutput = PutRecordOutput 
  { "ShardId" :: (ShardId)
  , "SequenceNumber" :: (SequenceNumber)
  , "EncryptionType" :: NullOrUndefined (EncryptionType)
  }
derive instance newtypePutRecordOutput :: Newtype PutRecordOutput _


-- | <p>A <code>PutRecords</code> request.</p>
newtype PutRecordsInput = PutRecordsInput 
  { "Records" :: (PutRecordsRequestEntryList)
  , "StreamName" :: (StreamName)
  }
derive instance newtypePutRecordsInput :: Newtype PutRecordsInput _


-- | <p> <code>PutRecords</code> results.</p>
newtype PutRecordsOutput = PutRecordsOutput 
  { "FailedRecordCount" :: NullOrUndefined (PositiveIntegerObject)
  , "Records" :: (PutRecordsResultEntryList)
  , "EncryptionType" :: NullOrUndefined (EncryptionType)
  }
derive instance newtypePutRecordsOutput :: Newtype PutRecordsOutput _


-- | <p>Represents the output for <code>PutRecords</code>.</p>
newtype PutRecordsRequestEntry = PutRecordsRequestEntry 
  { "Data" :: (Data)
  , "ExplicitHashKey" :: NullOrUndefined (HashKey)
  , "PartitionKey" :: (PartitionKey)
  }
derive instance newtypePutRecordsRequestEntry :: Newtype PutRecordsRequestEntry _


newtype PutRecordsRequestEntryList = PutRecordsRequestEntryList (Array PutRecordsRequestEntry)
derive instance newtypePutRecordsRequestEntryList :: Newtype PutRecordsRequestEntryList _


-- | <p>Represents the result of an individual record from a <code>PutRecords</code> request. A record that is successfully added to a stream includes <code>SequenceNumber</code> and <code>ShardId</code> in the result. A record that fails to be added to the stream includes <code>ErrorCode</code> and <code>ErrorMessage</code> in the result.</p>
newtype PutRecordsResultEntry = PutRecordsResultEntry 
  { "SequenceNumber" :: NullOrUndefined (SequenceNumber)
  , "ShardId" :: NullOrUndefined (ShardId)
  , "ErrorCode" :: NullOrUndefined (ErrorCode)
  , "ErrorMessage" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypePutRecordsResultEntry :: Newtype PutRecordsResultEntry _


newtype PutRecordsResultEntryList = PutRecordsResultEntryList (Array PutRecordsResultEntry)
derive instance newtypePutRecordsResultEntryList :: Newtype PutRecordsResultEntryList _


-- | <p>The unit of data of the Kinesis data stream, which is composed of a sequence number, a partition key, and a data blob.</p>
newtype Record'' = Record'' 
  { "SequenceNumber" :: (SequenceNumber)
  , "ApproximateArrivalTimestamp" :: NullOrUndefined (Number)
  , "Data" :: (Data)
  , "PartitionKey" :: (PartitionKey)
  , "EncryptionType" :: NullOrUndefined (EncryptionType)
  }
derive instance newtypeRecord'' :: Newtype Record'' _


newtype RecordList = RecordList (Array Record'')
derive instance newtypeRecordList :: Newtype RecordList _


-- | <p>Represents the input for <code>RemoveTagsFromStream</code>.</p>
newtype RemoveTagsFromStreamInput = RemoveTagsFromStreamInput 
  { "StreamName" :: (StreamName)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeRemoveTagsFromStreamInput :: Newtype RemoveTagsFromStreamInput _


-- | <p>The resource is not available for this operation. For successful operation, the resource must be in the <code>ACTIVE</code> state.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>The requested resource could not be found. The stream might not be specified correctly.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype RetentionPeriodHours = RetentionPeriodHours Int
derive instance newtypeRetentionPeriodHours :: Newtype RetentionPeriodHours _


newtype ScalingType = ScalingType String
derive instance newtypeScalingType :: Newtype ScalingType _


newtype SequenceNumber = SequenceNumber String
derive instance newtypeSequenceNumber :: Newtype SequenceNumber _


-- | <p>The range of possible sequence numbers for the shard.</p>
newtype SequenceNumberRange = SequenceNumberRange 
  { "StartingSequenceNumber" :: (SequenceNumber)
  , "EndingSequenceNumber" :: NullOrUndefined (SequenceNumber)
  }
derive instance newtypeSequenceNumberRange :: Newtype SequenceNumberRange _


-- | <p>A uniquely identified group of data records in a Kinesis data stream.</p>
newtype Shard = Shard 
  { "ShardId" :: (ShardId)
  , "ParentShardId" :: NullOrUndefined (ShardId)
  , "AdjacentParentShardId" :: NullOrUndefined (ShardId)
  , "HashKeyRange" :: (HashKeyRange)
  , "SequenceNumberRange" :: (SequenceNumberRange)
  }
derive instance newtypeShard :: Newtype Shard _


newtype ShardCountObject = ShardCountObject Int
derive instance newtypeShardCountObject :: Newtype ShardCountObject _


newtype ShardId = ShardId String
derive instance newtypeShardId :: Newtype ShardId _


newtype ShardIterator = ShardIterator String
derive instance newtypeShardIterator :: Newtype ShardIterator _


newtype ShardIteratorType = ShardIteratorType String
derive instance newtypeShardIteratorType :: Newtype ShardIteratorType _


newtype ShardList = ShardList (Array Shard)
derive instance newtypeShardList :: Newtype ShardList _


-- | <p>Represents the input for <code>SplitShard</code>.</p>
newtype SplitShardInput = SplitShardInput 
  { "StreamName" :: (StreamName)
  , "ShardToSplit" :: (ShardId)
  , "NewStartingHashKey" :: (HashKey)
  }
derive instance newtypeSplitShardInput :: Newtype SplitShardInput _


newtype StartStreamEncryptionInput = StartStreamEncryptionInput 
  { "StreamName" :: (StreamName)
  , "EncryptionType" :: (EncryptionType)
  , "KeyId" :: (KeyId)
  }
derive instance newtypeStartStreamEncryptionInput :: Newtype StartStreamEncryptionInput _


newtype StopStreamEncryptionInput = StopStreamEncryptionInput 
  { "StreamName" :: (StreamName)
  , "EncryptionType" :: (EncryptionType)
  , "KeyId" :: (KeyId)
  }
derive instance newtypeStopStreamEncryptionInput :: Newtype StopStreamEncryptionInput _


newtype StreamARN = StreamARN String
derive instance newtypeStreamARN :: Newtype StreamARN _


-- | <p>Represents the output for <a>DescribeStream</a>.</p>
newtype StreamDescription = StreamDescription 
  { "StreamName" :: (StreamName)
  , "StreamARN" :: (StreamARN)
  , "StreamStatus" :: (StreamStatus)
  , "Shards" :: (ShardList)
  , "HasMoreShards" :: (BooleanObject)
  , "RetentionPeriodHours" :: (RetentionPeriodHours)
  , "StreamCreationTimestamp" :: (Number)
  , "EnhancedMonitoring" :: (EnhancedMonitoringList)
  , "EncryptionType" :: NullOrUndefined (EncryptionType)
  , "KeyId" :: NullOrUndefined (KeyId)
  }
derive instance newtypeStreamDescription :: Newtype StreamDescription _


-- | <p>Represents the output for <a>DescribeStreamSummary</a> </p>
newtype StreamDescriptionSummary = StreamDescriptionSummary 
  { "StreamName" :: (StreamName)
  , "StreamARN" :: (StreamARN)
  , "StreamStatus" :: (StreamStatus)
  , "RetentionPeriodHours" :: (PositiveIntegerObject)
  , "StreamCreationTimestamp" :: (Number)
  , "EnhancedMonitoring" :: (EnhancedMonitoringList)
  , "EncryptionType" :: NullOrUndefined (EncryptionType)
  , "KeyId" :: NullOrUndefined (KeyId)
  , "OpenShardCount" :: (ShardCountObject)
  }
derive instance newtypeStreamDescriptionSummary :: Newtype StreamDescriptionSummary _


newtype StreamName = StreamName String
derive instance newtypeStreamName :: Newtype StreamName _


newtype StreamNameList = StreamNameList (Array StreamName)
derive instance newtypeStreamNameList :: Newtype StreamNameList _


newtype StreamStatus = StreamStatus String
derive instance newtypeStreamStatus :: Newtype StreamStatus _


-- | <p>Metadata assigned to the stream, consisting of a key-value pair.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagMap = TagMap (Map TagKey TagValue)
derive instance newtypeTagMap :: Newtype TagMap _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype UpdateShardCountInput = UpdateShardCountInput 
  { "StreamName" :: (StreamName)
  , "TargetShardCount" :: (PositiveIntegerObject)
  , "ScalingType" :: (ScalingType)
  }
derive instance newtypeUpdateShardCountInput :: Newtype UpdateShardCountInput _


newtype UpdateShardCountOutput = UpdateShardCountOutput 
  { "StreamName" :: NullOrUndefined (StreamName)
  , "CurrentShardCount" :: NullOrUndefined (PositiveIntegerObject)
  , "TargetShardCount" :: NullOrUndefined (PositiveIntegerObject)
  }
derive instance newtypeUpdateShardCountOutput :: Newtype UpdateShardCountOutput _
