## Module AWS.KinesisVideo

<p/>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createStream`

``` purescript
createStream :: forall eff. CreateStreamInput -> Aff (err :: RequestError | eff) CreateStreamOutput
```

<p>Creates a new Kinesis video stream. </p> <p>When you create a new stream, Kinesis Video Streams assigns it a version number. When you change the stream's metadata, Kinesis Video Streams updates the version. </p> <p> <code>CreateStream</code> is an asynchronous operation.</p> <p>For information about how the service works, see <a href="http://docs.aws.amazon.com/kinesisvideostreams/latest/dg/how-it-works.html">How it Works</a>. </p> <p>You must have permissions for the <code>KinesisVideo:CreateStream</code> action.</p>

#### `deleteStream`

``` purescript
deleteStream :: forall eff. DeleteStreamInput -> Aff (err :: RequestError | eff) DeleteStreamOutput
```

<p>Deletes a Kinesis video stream and the data contained in the stream. </p> <p>This method marks the stream for deletion, and makes the data in the stream inaccessible immediately.</p> <p> </p> <p> To ensure that you have the latest version of the stream before deleting it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the <code>DescribeStream</code> API. </p> <p>This operation requires permission for the <code>KinesisVideo:DeleteStream</code> action.</p>

#### `describeStream`

``` purescript
describeStream :: forall eff. DescribeStreamInput -> Aff (err :: RequestError | eff) DescribeStreamOutput
```

<p>Returns the most current information about the specified stream. You must specify either the <code>StreamName</code> or the <code>StreamARN</code>. </p>

#### `getDataEndpoint`

``` purescript
getDataEndpoint :: forall eff. GetDataEndpointInput -> Aff (err :: RequestError | eff) GetDataEndpointOutput
```

<p>Gets an endpoint for a specified stream for either reading or writing. Use this endpoint in your application to read from the specified stream (using the <code>GetMedia</code> or <code>GetMediaForFragmentList</code> operations) or write to it (using the <code>PutMedia</code> operation). </p> <note> <p>The returned endpoint does not have the API name appended. The client needs to add the API name to the returned endpoint.</p> </note> <p>In the request, specify the stream either by <code>StreamName</code> or <code>StreamARN</code>.</p>

#### `listStreams`

``` purescript
listStreams :: forall eff. ListStreamsInput -> Aff (err :: RequestError | eff) ListStreamsOutput
```

<p>Returns an array of <code>StreamInfo</code> objects. Each object describes a stream. To retrieve only streams that satisfy a specific condition, you can specify a <code>StreamNameCondition</code>. </p>

#### `listTagsForStream`

``` purescript
listTagsForStream :: forall eff. ListTagsForStreamInput -> Aff (err :: RequestError | eff) ListTagsForStreamOutput
```

<p>Returns a list of tags associated with the specified stream.</p> <p>In the request, you must specify either the <code>StreamName</code> or the <code>StreamARN</code>. </p>

#### `tagStream`

``` purescript
tagStream :: forall eff. TagStreamInput -> Aff (err :: RequestError | eff) TagStreamOutput
```

<p>Adds one or more tags to a stream. A <i>tag</i> is a key-value pair (the value is optional) that you can define and assign to AWS resources. If you specify a tag that already exists, the tag value is replaced with the value that you specify in the request. For more information, see <a href="http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html">Using Cost Allocation Tags</a> in the <i>AWS Billing and Cost Management User Guide</i>. </p> <p>You must provide either the <code>StreamName</code> or the <code>StreamARN</code>.</p> <p>This operation requires permission for the <code>KinesisVideo:TagStream</code> action.</p> <p>Kinesis video streams support up to 50 tags.</p>

#### `untagStream`

``` purescript
untagStream :: forall eff. UntagStreamInput -> Aff (err :: RequestError | eff) UntagStreamOutput
```

<p>Removes one or more tags from a stream. In the request, specify only a tag key or keys; don't specify the value. If you specify a tag key that does not exist, it's ignored.</p> <p>In the request, you must provide the <code>StreamName</code> or <code>StreamARN</code>.</p>

#### `updateDataRetention`

``` purescript
updateDataRetention :: forall eff. UpdateDataRetentionInput -> Aff (err :: RequestError | eff) UpdateDataRetentionOutput
```

<p> Increases or decreases the stream's data retention period by the value that you specify. To indicate whether you want to increase or decrease the data retention period, specify the <code>Operation</code> parameter in the request body. In the request, you must specify either the <code>StreamName</code> or the <code>StreamARN</code>. </p> <note> <p>The retention period that you specify replaces the current value.</p> </note> <p>This operation requires permission for the <code>KinesisVideo:UpdateDataRetention</code> action.</p> <p>Changing the data retention period affects the data in the stream as follows:</p> <ul> <li> <p>If the data retention period is increased, existing data is retained for the new retention period. For example, if the data retention period is increased from one hour to seven hours, all existing data is retained for seven hours.</p> </li> <li> <p>If the data retention period is decreased, existing data is retained for the new retention period. For example, if the data retention period is decreased from seven hours to one hour, all existing data is retained for one hour, and any data older than one hour is deleted immediately.</p> </li> </ul>

#### `updateStream`

``` purescript
updateStream :: forall eff. UpdateStreamInput -> Aff (err :: RequestError | eff) UpdateStreamOutput
```

<p>Updates stream metadata, such as the device name and media type.</p> <p>You must provide the stream name or the Amazon Resource Name (ARN) of the stream.</p> <p>To make sure that you have the latest version of the stream before updating it, you can specify the stream version. Kinesis Video Streams assigns a version to each stream. When you update a stream, Kinesis Video Streams assigns a new version number. To get the latest stream version, use the <code>DescribeStream</code> API. </p> <p> <code>UpdateStream</code> is an asynchronous operation, and takes time to complete.</p>

#### `APIName`

``` purescript
newtype APIName
  = APIName String
```

#### `AccountStreamLimitExceededException`

``` purescript
newtype AccountStreamLimitExceededException
  = AccountStreamLimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The number of streams created for the account is too high.</p>

#### `ClientLimitExceededException`

``` purescript
newtype ClientLimitExceededException
  = ClientLimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.</p>

#### `ComparisonOperator`

``` purescript
newtype ComparisonOperator
  = ComparisonOperator String
```

#### `CreateStreamInput`

``` purescript
newtype CreateStreamInput
  = CreateStreamInput { "DeviceName" :: NullOrUndefined (DeviceName), "StreamName" :: StreamName, "MediaType" :: NullOrUndefined (MediaType), "KmsKeyId" :: NullOrUndefined (KmsKeyId), "DataRetentionInHours" :: NullOrUndefined (DataRetentionInHours) }
```

#### `CreateStreamOutput`

``` purescript
newtype CreateStreamOutput
  = CreateStreamOutput { "StreamARN" :: NullOrUndefined (ResourceARN) }
```

#### `DataEndpoint`

``` purescript
newtype DataEndpoint
  = DataEndpoint String
```

#### `DataRetentionChangeInHours`

``` purescript
newtype DataRetentionChangeInHours
  = DataRetentionChangeInHours Int
```

#### `DataRetentionInHours`

``` purescript
newtype DataRetentionInHours
  = DataRetentionInHours Int
```

#### `DeleteStreamInput`

``` purescript
newtype DeleteStreamInput
  = DeleteStreamInput { "StreamARN" :: ResourceARN, "CurrentVersion" :: NullOrUndefined (Version) }
```

#### `DeleteStreamOutput`

``` purescript
newtype DeleteStreamOutput
  = DeleteStreamOutput {  }
```

#### `DescribeStreamInput`

``` purescript
newtype DescribeStreamInput
  = DescribeStreamInput { "StreamName" :: NullOrUndefined (StreamName), "StreamARN" :: NullOrUndefined (ResourceARN) }
```

#### `DescribeStreamOutput`

``` purescript
newtype DescribeStreamOutput
  = DescribeStreamOutput { "StreamInfo" :: NullOrUndefined (StreamInfo) }
```

#### `DeviceName`

``` purescript
newtype DeviceName
  = DeviceName String
```

#### `DeviceStreamLimitExceededException`

``` purescript
newtype DeviceStreamLimitExceededException
  = DeviceStreamLimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Not implemented. </p>

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `GetDataEndpointInput`

``` purescript
newtype GetDataEndpointInput
  = GetDataEndpointInput { "StreamName" :: NullOrUndefined (StreamName), "StreamARN" :: NullOrUndefined (ResourceARN), "APIName" :: APIName }
```

#### `GetDataEndpointOutput`

``` purescript
newtype GetDataEndpointOutput
  = GetDataEndpointOutput { "DataEndpoint" :: NullOrUndefined (DataEndpoint) }
```

#### `InvalidArgumentException`

``` purescript
newtype InvalidArgumentException
  = InvalidArgumentException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The value for this input parameter is invalid.</p>

#### `InvalidDeviceException`

``` purescript
newtype InvalidDeviceException
  = InvalidDeviceException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Not implemented.</p>

#### `InvalidResourceFormatException`

``` purescript
newtype InvalidResourceFormatException
  = InvalidResourceFormatException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The format of the <code>StreamARN</code> is invalid.</p>

#### `KmsKeyId`

``` purescript
newtype KmsKeyId
  = KmsKeyId String
```

#### `ListStreamsInput`

``` purescript
newtype ListStreamsInput
  = ListStreamsInput { "MaxResults" :: NullOrUndefined (ListStreamsInputLimit), "NextToken" :: NullOrUndefined (NextToken), "StreamNameCondition" :: NullOrUndefined (StreamNameCondition) }
```

#### `ListStreamsInputLimit`

``` purescript
newtype ListStreamsInputLimit
  = ListStreamsInputLimit Int
```

#### `ListStreamsOutput`

``` purescript
newtype ListStreamsOutput
  = ListStreamsOutput { "StreamInfoList" :: NullOrUndefined (StreamInfoList), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListTagsForStreamInput`

``` purescript
newtype ListTagsForStreamInput
  = ListTagsForStreamInput { "NextToken" :: NullOrUndefined (NextToken), "StreamARN" :: NullOrUndefined (ResourceARN), "StreamName" :: NullOrUndefined (StreamName) }
```

#### `ListTagsForStreamOutput`

``` purescript
newtype ListTagsForStreamOutput
  = ListTagsForStreamOutput { "NextToken" :: NullOrUndefined (NextToken), "Tags" :: NullOrUndefined (ResourceTags) }
```

#### `MediaType`

``` purescript
newtype MediaType
  = MediaType String
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `NotAuthorizedException`

``` purescript
newtype NotAuthorizedException
  = NotAuthorizedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The caller is not authorized to perform this operation.</p>

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The stream is currently not available for this operation.</p>

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Amazon Kinesis Video Streams can't find the stream that you specified.</p>

#### `ResourceTags`

``` purescript
newtype ResourceTags
  = ResourceTags (Map TagKey TagValue)
```

#### `Status`

``` purescript
newtype Status
  = Status String
```

#### `StreamInfo`

``` purescript
newtype StreamInfo
  = StreamInfo { "DeviceName" :: NullOrUndefined (DeviceName), "StreamName" :: NullOrUndefined (StreamName), "StreamARN" :: NullOrUndefined (ResourceARN), "MediaType" :: NullOrUndefined (MediaType), "KmsKeyId" :: NullOrUndefined (KmsKeyId), "Version" :: NullOrUndefined (Version), "Status" :: NullOrUndefined (Status), "CreationTime" :: NullOrUndefined (Number), "DataRetentionInHours" :: NullOrUndefined (DataRetentionInHours) }
```

<p>An object describing a Kinesis video stream.</p>

#### `StreamInfoList`

``` purescript
newtype StreamInfoList
  = StreamInfoList (Array StreamInfo)
```

#### `StreamName`

``` purescript
newtype StreamName
  = StreamName String
```

#### `StreamNameCondition`

``` purescript
newtype StreamNameCondition
  = StreamNameCondition { "ComparisonOperator" :: NullOrUndefined (ComparisonOperator), "ComparisonValue" :: NullOrUndefined (StreamName) }
```

<p>Specifies the condition that streams must satisfy to be returned when you list streams (see the <code>ListStreams</code> API). A condition has a comparison operation and a value. Currently, you can specify only the <code>BEGINS_WITH</code> operator, which finds streams whose names start with a given prefix. </p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

#### `TagStreamInput`

``` purescript
newtype TagStreamInput
  = TagStreamInput { "StreamARN" :: NullOrUndefined (ResourceARN), "StreamName" :: NullOrUndefined (StreamName), "Tags" :: ResourceTags }
```

#### `TagStreamOutput`

``` purescript
newtype TagStreamOutput
  = TagStreamOutput {  }
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `TagsPerResourceExceededLimitException`

``` purescript
newtype TagsPerResourceExceededLimitException
  = TagsPerResourceExceededLimitException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>You have exceeded the limit of tags that you can associate with the resource. Kinesis video streams support up to 50 tags. </p>

#### `UntagStreamInput`

``` purescript
newtype UntagStreamInput
  = UntagStreamInput { "StreamARN" :: NullOrUndefined (ResourceARN), "StreamName" :: NullOrUndefined (StreamName), "TagKeyList" :: TagKeyList }
```

#### `UntagStreamOutput`

``` purescript
newtype UntagStreamOutput
  = UntagStreamOutput {  }
```

#### `UpdateDataRetentionInput`

``` purescript
newtype UpdateDataRetentionInput
  = UpdateDataRetentionInput { "StreamName" :: NullOrUndefined (StreamName), "StreamARN" :: NullOrUndefined (ResourceARN), "CurrentVersion" :: Version, "Operation" :: UpdateDataRetentionOperation, "DataRetentionChangeInHours" :: DataRetentionChangeInHours }
```

#### `UpdateDataRetentionOperation`

``` purescript
newtype UpdateDataRetentionOperation
  = UpdateDataRetentionOperation String
```

#### `UpdateDataRetentionOutput`

``` purescript
newtype UpdateDataRetentionOutput
  = UpdateDataRetentionOutput {  }
```

#### `UpdateStreamInput`

``` purescript
newtype UpdateStreamInput
  = UpdateStreamInput { "StreamName" :: NullOrUndefined (StreamName), "StreamARN" :: NullOrUndefined (ResourceARN), "CurrentVersion" :: Version, "DeviceName" :: NullOrUndefined (DeviceName), "MediaType" :: NullOrUndefined (MediaType) }
```

#### `UpdateStreamOutput`

``` purescript
newtype UpdateStreamOutput
  = UpdateStreamOutput {  }
```

#### `Version`

``` purescript
newtype Version
  = Version String
```

#### `VersionMismatchException`

``` purescript
newtype VersionMismatchException
  = VersionMismatchException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The stream version that you specified is not the latest version. To get the latest version, use the <a href="http://docs.aws.amazon.com/kinesisvideo/latest/dg/API_DescribeStream.html">DescribeStream</a> API.</p>


