## Module AWS.KinesisVideoMedia

<p/>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `getMedia`

``` purescript
getMedia :: forall eff. GetMediaInput -> Aff (err :: RequestError | eff) GetMediaOutput
```

<p> Use this API to retrieve media content from a Kinesis video stream. In the request, you identify stream name or stream Amazon Resource Name (ARN), and the starting chunk. Kinesis Video Streams then returns a stream of chunks in order by fragment number.</p> <note> <p> You must first call the <code>GetDataEndpoint</code> API to get an endpoint to which you can then send the <code>GetMedia</code> requests. </p> </note> <p>When you put media data (fragments) on a stream, Kinesis Video Streams stores each incoming fragment and related metadata in what is called a "chunk." For more information, see . The <code>GetMedia</code> API returns a stream of these chunks starting from the chunk that you specify in the request. </p> <p>The following limits apply when using the <code>GetMedia</code> API:</p> <ul> <li> <p>A client can call <code>GetMedia</code> up to five times per second per stream. </p> </li> <li> <p>Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a <code>GetMedia</code> session. </p> </li> </ul>

#### `ClientLimitExceededException`

``` purescript
newtype ClientLimitExceededException
  = ClientLimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client calls. Try making the call later.</p>

##### Instances
``` purescript
Newtype ClientLimitExceededException _
```

#### `ConnectionLimitExceededException`

``` purescript
newtype ConnectionLimitExceededException
  = ConnectionLimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Kinesis Video Streams has throttled the request because you have exceeded the limit of allowed client connections.</p>

##### Instances
``` purescript
Newtype ConnectionLimitExceededException _
```

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

##### Instances
``` purescript
Newtype ContentType _
```

#### `ContinuationToken`

``` purescript
newtype ContinuationToken
  = ContinuationToken String
```

##### Instances
``` purescript
Newtype ContinuationToken _
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

#### `FragmentNumberString`

``` purescript
newtype FragmentNumberString
  = FragmentNumberString String
```

##### Instances
``` purescript
Newtype FragmentNumberString _
```

#### `GetMediaInput`

``` purescript
newtype GetMediaInput
  = GetMediaInput { "StreamName" :: NullOrUndefined (StreamName), "StreamARN" :: NullOrUndefined (ResourceARN), "StartSelector" :: StartSelector }
```

##### Instances
``` purescript
Newtype GetMediaInput _
```

#### `GetMediaOutput`

``` purescript
newtype GetMediaOutput
  = GetMediaOutput { "ContentType" :: NullOrUndefined (ContentType), "Payload" :: NullOrUndefined (Payload) }
```

##### Instances
``` purescript
Newtype GetMediaOutput _
```

#### `InvalidArgumentException`

``` purescript
newtype InvalidArgumentException
  = InvalidArgumentException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The value for this input parameter is invalid.</p>

##### Instances
``` purescript
Newtype InvalidArgumentException _
```

#### `InvalidEndpointException`

``` purescript
newtype InvalidEndpointException
  = InvalidEndpointException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p> Status Code: 400, Caller used wrong endpoint to write data to a stream. On receiving such an exception, the user must call <code>GetDataEndpoint</code> with <code>AccessMode</code> set to "READ" and use the endpoint Kinesis Video returns in the next <code>GetMedia</code> call. </p>

##### Instances
``` purescript
Newtype InvalidEndpointException _
```

#### `NotAuthorizedException`

``` purescript
newtype NotAuthorizedException
  = NotAuthorizedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Status Code: 403, The caller is not authorized to perform an operation on the given stream, or the token has expired.</p>

##### Instances
``` purescript
Newtype NotAuthorizedException _
```

#### `Payload`

``` purescript
newtype Payload
  = Payload String
```

##### Instances
``` purescript
Newtype Payload _
```

#### `ResourceARN`

``` purescript
newtype ResourceARN
  = ResourceARN String
```

##### Instances
``` purescript
Newtype ResourceARN _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Status Code: 404, The stream with the given name does not exist.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `StartSelector`

``` purescript
newtype StartSelector
  = StartSelector { "StartSelectorType" :: StartSelectorType, "AfterFragmentNumber" :: NullOrUndefined (FragmentNumberString), "StartTimestamp" :: NullOrUndefined (Number), "ContinuationToken" :: NullOrUndefined (ContinuationToken) }
```

<p>Identifies the chunk on the Kinesis video stream where you want the <code>GetMedia</code> API to start returning media data. You have the following options to identify the starting chunk: </p> <ul> <li> <p>Choose the latest (or oldest) chunk.</p> </li> <li> <p>Identify a specific chunk. You can identify a specific chunk either by providing a fragment number or time stamp (server or producer). </p> </li> <li> <p>Each chunk's metadata includes a continuation token as a Matroska (MKV) tag (<code>AWS_KINESISVIDEO_CONTINUATION_TOKEN</code>). If your previous <code>GetMedia</code> request terminated, you can use this tag value in your next <code>GetMedia</code> request. The API then starts returning chunks starting where the last API ended.</p> </li> </ul>

##### Instances
``` purescript
Newtype StartSelector _
```

#### `StartSelectorType`

``` purescript
newtype StartSelectorType
  = StartSelectorType String
```

##### Instances
``` purescript
Newtype StartSelectorType _
```

#### `StreamName`

``` purescript
newtype StreamName
  = StreamName String
```

##### Instances
``` purescript
Newtype StreamName _
```


