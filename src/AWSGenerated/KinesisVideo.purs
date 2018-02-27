

-- | <p/>
module AWS.KinesisVideo where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "KinesisVideo" :: String


-- | <p>Creates a new Kinesis video stream. </p> <p>When you create a new stream, Kinesis Video Streams assigns it a version number. When you change the stream's metadata, Kinesis Video Streams updates the version. </p> <p> <code>CreateStream</code> is an asynchronous operation.</p> <p>For information about how the service works, see <a href="http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/how-it-works.html">How it Works</a>. </p> <p>You must have permissions for the <code>KinesisVideo:CreateStream</code> action.</p>
createStream :: forall eff. CreateStreamInput -> Aff (err :: AWS.RequestError | eff) CreateStreamOutput
createStream = AWS.request serviceName "CreateStream" 


-- | <p>Deletes a Kinesis video stream and the data contained in the stream. </p> <p>This method marks the stream for deletion, and makes the data in the stream inaccessible immediately.</p> <p> </p> <p> To ensure that you have the latest version of the stream before deleting it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the <code>DescribeStream</code> API. </p> <p>This operation requires permission for the <code>KinesisVideo:DeleteStream</code> action.</p>
deleteStream :: forall eff. DeleteStreamInput -> Aff (err :: AWS.RequestError | eff) DeleteStreamOutput
deleteStream = AWS.request serviceName "DeleteStream" 


-- | <p>Returns the most current information about the specified stream. You must specify either the <code>StreamName</code> or the <code>StreamARN</code>. </p>
describeStream :: forall eff. DescribeStreamInput -> Aff (err :: AWS.RequestError | eff) DescribeStreamOutput
describeStream = AWS.request serviceName "DescribeStream" 


-- | <p>Gets an endpoint for a specified stream for either reading or writing. Use this endpoint in your application to read from the specified stream (using the <code>GetMedia</code> or <code>GetMediaForFragmentList</code> operations) or write to it (using the <code>PutMedia</code> operation). </p> <note> <p>The returned endpoint does not have the API name appended. The client needs to add the API name to the returned endpoint.</p> </note> <p>In the request, specify the stream either by <code>StreamName</code> or <code>StreamARN</code>.</p>
getDataEndpoint :: forall eff. GetDataEndpointInput -> Aff (err :: AWS.RequestError | eff) GetDataEndpointOutput
getDataEndpoint = AWS.request serviceName "GetDataEndpoint" 


-- | <p>Returns an array of <code>StreamInfo</code> objects. Each object describes a stream. To retrieve only streams that satisfy a specific condition, you can specify a <code>StreamNameCondition</code>. </p>
listStreams :: forall eff. ListStreamsInput -> Aff (err :: AWS.RequestError | eff) ListStreamsOutput
listStreams = AWS.request serviceName "ListStreams" 


-- | <p>Returns a list of tags associated with the specified stream.</p> <p>In the request, you must specify either the <code>StreamName</code> or the <code>StreamARN</code>. </p>
listTagsForStream :: forall eff. ListTagsForStreamInput -> Aff (err :: AWS.RequestError | eff) ListTagsForStreamOutput
listTagsForStream = AWS.request serviceName "ListTagsForStream" 


-- | <p>Adds one or more tags to a stream. A <i>tag</i> is a key-value pair (the value is optional) that you can define and assign to AWS resources. If you specify a tag that already exists, the tag value is replaced with the value that you specify in the request. For more information, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>. </p> <p>You must provide either the <code>StreamName</code> or the <code>StreamARN</code>.</p> <p>This operation requires permission for the <code>KinesisVideo:TagStream</code> action.</p> <p>Kinesis video streams support up to 50 tags.</p>
tagStream :: forall eff. TagStreamInput -> Aff (err :: AWS.RequestError | eff) TagStreamOutput
tagStream = AWS.request serviceName "TagStream" 


-- | <p>Removes one or more tags from a stream. In the request, specify only a tag key or keys; don't specify the value. If you specify a tag key that does not exist, it's ignored.</p> <p>In the request, you must provide the <code>StreamName</code> or <code>StreamARN</code>.</p>
untagStream :: forall eff. UntagStreamInput -> Aff (err :: AWS.RequestError | eff) UntagStreamOutput
untagStream = AWS.request serviceName "UntagStream" 


-- | <p> Increases or decreases the stream's data retention period by the value that you specify. To indicate whether you want to increase or decrease the data retention period, specify the <code>Operation</code> parameter in the request body. In the request, you must specify either the <code>StreamName</code> or the <code>StreamARN</code>. </p> <note> <p>The retention period that you specify replaces the current value.</p> </note> <p>This operation requires permission for the <code>KinesisVideo:UpdateDataRetention</code> action.</p> <p>Changing the data retention period affects the data in the stream as follows:</p> <ul> <li> <p>If the data retention period is increased, existing data is retained for the new retention period. For example, if the data retention period is increased from one hour to seven hours, all existing data is retained for seven hours.</p> </li> <li> <p>If the data retention period is decreased, existing data is retained for the new retention period. For example, if the data retention period is decreased from seven hours to one hour, all existing data is retained for one hour, and any data older than one hour is deleted immediately.</p> </li> </ul>
updateDataRetention :: forall eff. UpdateDataRetentionInput -> Aff (err :: AWS.RequestError | eff) UpdateDataRetentionOutput
updateDataRetention = AWS.request serviceName "UpdateDataRetention" 


-- | <p>Updates stream metadata, such as the device name and media type.</p> <p>You must provide the stream name or the Amazon Resource Name (ARN) of the stream.</p> <p>To make sure that you have the latest version of the stream before updating it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the <code>DescribeStream</code> API. </p> <p> <code>UpdateStream</code> is an asynchronous operation, and takes time to complete.</p>
updateStream :: forall eff. UpdateStreamInput -> Aff (err :: AWS.RequestError | eff) UpdateStreamOutput
updateStream = AWS.request serviceName "UpdateStream" 


newtype APIName = APIName String
derive instance newtypeAPIName :: Newtype APIName _


-- | <p>The number of streams created for the account is too high.</p>
newtype AccountStreamLimitExceededException = AccountStreamLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAccountStreamLimitExceededException :: Newtype AccountStreamLimitExceededException _


-- | <p>Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.</p>
newtype ClientLimitExceededException = ClientLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeClientLimitExceededException :: Newtype ClientLimitExceededException _


newtype ComparisonOperator = ComparisonOperator String
derive instance newtypeComparisonOperator :: Newtype ComparisonOperator _


newtype CreateStreamInput = CreateStreamInput 
  { "DeviceName" :: NullOrUndefined (DeviceName)
  , "StreamName" :: (StreamName)
  , "MediaType" :: NullOrUndefined (MediaType)
  , "KmsKeyId" :: NullOrUndefined (KmsKeyId)
  , "DataRetentionInHours" :: NullOrUndefined (DataRetentionInHours)
  }
derive instance newtypeCreateStreamInput :: Newtype CreateStreamInput _


newtype CreateStreamOutput = CreateStreamOutput 
  { "StreamARN" :: NullOrUndefined (ResourceARN)
  }
derive instance newtypeCreateStreamOutput :: Newtype CreateStreamOutput _


newtype DataEndpoint = DataEndpoint String
derive instance newtypeDataEndpoint :: Newtype DataEndpoint _


newtype DataRetentionChangeInHours = DataRetentionChangeInHours Int
derive instance newtypeDataRetentionChangeInHours :: Newtype DataRetentionChangeInHours _


newtype DataRetentionInHours = DataRetentionInHours Int
derive instance newtypeDataRetentionInHours :: Newtype DataRetentionInHours _


newtype DeleteStreamInput = DeleteStreamInput 
  { "StreamARN" :: (ResourceARN)
  , "CurrentVersion" :: NullOrUndefined (Version)
  }
derive instance newtypeDeleteStreamInput :: Newtype DeleteStreamInput _


newtype DeleteStreamOutput = DeleteStreamOutput 
  { 
  }
derive instance newtypeDeleteStreamOutput :: Newtype DeleteStreamOutput _


newtype DescribeStreamInput = DescribeStreamInput 
  { "StreamName" :: NullOrUndefined (StreamName)
  , "StreamARN" :: NullOrUndefined (ResourceARN)
  }
derive instance newtypeDescribeStreamInput :: Newtype DescribeStreamInput _


newtype DescribeStreamOutput = DescribeStreamOutput 
  { "StreamInfo" :: NullOrUndefined (StreamInfo)
  }
derive instance newtypeDescribeStreamOutput :: Newtype DescribeStreamOutput _


newtype DeviceName = DeviceName String
derive instance newtypeDeviceName :: Newtype DeviceName _


-- | <p>Not implemented. </p>
newtype DeviceStreamLimitExceededException = DeviceStreamLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeDeviceStreamLimitExceededException :: Newtype DeviceStreamLimitExceededException _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype GetDataEndpointInput = GetDataEndpointInput 
  { "StreamName" :: NullOrUndefined (StreamName)
  , "StreamARN" :: NullOrUndefined (ResourceARN)
  , "APIName" :: (APIName)
  }
derive instance newtypeGetDataEndpointInput :: Newtype GetDataEndpointInput _


newtype GetDataEndpointOutput = GetDataEndpointOutput 
  { "DataEndpoint" :: NullOrUndefined (DataEndpoint)
  }
derive instance newtypeGetDataEndpointOutput :: Newtype GetDataEndpointOutput _


-- | <p>The value for this input parameter is invalid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _


-- | <p>Not implemented.</p>
newtype InvalidDeviceException = InvalidDeviceException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidDeviceException :: Newtype InvalidDeviceException _


-- | <p>The format of the <code>StreamARN</code> is invalid.</p>
newtype InvalidResourceFormatException = InvalidResourceFormatException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidResourceFormatException :: Newtype InvalidResourceFormatException _


newtype KmsKeyId = KmsKeyId String
derive instance newtypeKmsKeyId :: Newtype KmsKeyId _


newtype ListStreamsInput = ListStreamsInput 
  { "MaxResults" :: NullOrUndefined (ListStreamsInputLimit)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "StreamNameCondition" :: NullOrUndefined (StreamNameCondition)
  }
derive instance newtypeListStreamsInput :: Newtype ListStreamsInput _


newtype ListStreamsInputLimit = ListStreamsInputLimit Int
derive instance newtypeListStreamsInputLimit :: Newtype ListStreamsInputLimit _


newtype ListStreamsOutput = ListStreamsOutput 
  { "StreamInfoList" :: NullOrUndefined (StreamInfoList)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListStreamsOutput :: Newtype ListStreamsOutput _


newtype ListTagsForStreamInput = ListTagsForStreamInput 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "StreamARN" :: NullOrUndefined (ResourceARN)
  , "StreamName" :: NullOrUndefined (StreamName)
  }
derive instance newtypeListTagsForStreamInput :: Newtype ListTagsForStreamInput _


newtype ListTagsForStreamOutput = ListTagsForStreamOutput 
  { "NextToken" :: NullOrUndefined (NextToken)
  , "Tags" :: NullOrUndefined (ResourceTags)
  }
derive instance newtypeListTagsForStreamOutput :: Newtype ListTagsForStreamOutput _


newtype MediaType = MediaType String
derive instance newtypeMediaType :: Newtype MediaType _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>The caller is not authorized to perform this operation.</p>
newtype NotAuthorizedException = NotAuthorizedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotAuthorizedException :: Newtype NotAuthorizedException _


newtype ResourceARN = ResourceARN String
derive instance newtypeResourceARN :: Newtype ResourceARN _


-- | <p>The stream is currently not available for this operation.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>Amazon Kinesis Video Streams can't find the stream that you specified.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype ResourceTags = ResourceTags (Map TagKey TagValue)
derive instance newtypeResourceTags :: Newtype ResourceTags _


newtype Status = Status String
derive instance newtypeStatus :: Newtype Status _


-- | <p>An object describing a Kinesis video stream.</p>
newtype StreamInfo = StreamInfo 
  { "DeviceName" :: NullOrUndefined (DeviceName)
  , "StreamName" :: NullOrUndefined (StreamName)
  , "StreamARN" :: NullOrUndefined (ResourceARN)
  , "MediaType" :: NullOrUndefined (MediaType)
  , "KmsKeyId" :: NullOrUndefined (KmsKeyId)
  , "Version" :: NullOrUndefined (Version)
  , "Status" :: NullOrUndefined (Status)
  , "CreationTime" :: NullOrUndefined (Number)
  , "DataRetentionInHours" :: NullOrUndefined (DataRetentionInHours)
  }
derive instance newtypeStreamInfo :: Newtype StreamInfo _


newtype StreamInfoList = StreamInfoList (Array StreamInfo)
derive instance newtypeStreamInfoList :: Newtype StreamInfoList _


newtype StreamName = StreamName String
derive instance newtypeStreamName :: Newtype StreamName _


-- | <p>Specifies the condition that streams must satisfy to be returned when you list streams (see the <code>ListStreams</code> API). A condition has a comparison operation and a value. Currently, you can specify only the <code>BEGINS_WITH</code> operator, which finds streams whose names start with a given prefix. </p>
newtype StreamNameCondition = StreamNameCondition 
  { "ComparisonOperator" :: NullOrUndefined (ComparisonOperator)
  , "ComparisonValue" :: NullOrUndefined (StreamName)
  }
derive instance newtypeStreamNameCondition :: Newtype StreamNameCondition _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagStreamInput = TagStreamInput 
  { "StreamARN" :: NullOrUndefined (ResourceARN)
  , "StreamName" :: NullOrUndefined (StreamName)
  , "Tags" :: (ResourceTags)
  }
derive instance newtypeTagStreamInput :: Newtype TagStreamInput _


newtype TagStreamOutput = TagStreamOutput 
  { 
  }
derive instance newtypeTagStreamOutput :: Newtype TagStreamOutput _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


-- | <p>You have exceeded the limit of tags that you can associate with the resource. Kinesis video streams support up to 50 tags. </p>
newtype TagsPerResourceExceededLimitException = TagsPerResourceExceededLimitException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTagsPerResourceExceededLimitException :: Newtype TagsPerResourceExceededLimitException _


newtype UntagStreamInput = UntagStreamInput 
  { "StreamARN" :: NullOrUndefined (ResourceARN)
  , "StreamName" :: NullOrUndefined (StreamName)
  , "TagKeyList" :: (TagKeyList)
  }
derive instance newtypeUntagStreamInput :: Newtype UntagStreamInput _


newtype UntagStreamOutput = UntagStreamOutput 
  { 
  }
derive instance newtypeUntagStreamOutput :: Newtype UntagStreamOutput _


newtype UpdateDataRetentionInput = UpdateDataRetentionInput 
  { "StreamName" :: NullOrUndefined (StreamName)
  , "StreamARN" :: NullOrUndefined (ResourceARN)
  , "CurrentVersion" :: (Version)
  , "Operation" :: (UpdateDataRetentionOperation)
  , "DataRetentionChangeInHours" :: (DataRetentionChangeInHours)
  }
derive instance newtypeUpdateDataRetentionInput :: Newtype UpdateDataRetentionInput _


newtype UpdateDataRetentionOperation = UpdateDataRetentionOperation String
derive instance newtypeUpdateDataRetentionOperation :: Newtype UpdateDataRetentionOperation _


newtype UpdateDataRetentionOutput = UpdateDataRetentionOutput 
  { 
  }
derive instance newtypeUpdateDataRetentionOutput :: Newtype UpdateDataRetentionOutput _


newtype UpdateStreamInput = UpdateStreamInput 
  { "StreamName" :: NullOrUndefined (StreamName)
  , "StreamARN" :: NullOrUndefined (ResourceARN)
  , "CurrentVersion" :: (Version)
  , "DeviceName" :: NullOrUndefined (DeviceName)
  , "MediaType" :: NullOrUndefined (MediaType)
  }
derive instance newtypeUpdateStreamInput :: Newtype UpdateStreamInput _


newtype UpdateStreamOutput = UpdateStreamOutput 
  { 
  }
derive instance newtypeUpdateStreamOutput :: Newtype UpdateStreamOutput _


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _


-- | <p>The stream version that you specified is not the latest version. To get the latest version, use the <a href="http://docs.aws.amazon.com/kinesisvideo/latest/dg/API_DescribeStream.html">DescribeStream</a> API.</p>
newtype VersionMismatchException = VersionMismatchException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeVersionMismatchException :: Newtype VersionMismatchException _
