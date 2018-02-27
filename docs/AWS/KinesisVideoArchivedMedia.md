## Module AWS.KinesisVideoArchivedMedia

<p/>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `getMediaForFragmentList`

``` purescript
getMediaForFragmentList :: forall eff. GetMediaForFragmentListInput -> Aff (err :: RequestError | eff) GetMediaForFragmentListOutput
```

<p>Gets media for a list of fragments (specified by fragment number) from the archived data in a Kinesis video stream.</p> <note> <p>This operation is only available for the AWS SDK for Java. It is not supported in AWS SDKs for other languages.</p> </note> <p>The following limits apply when using the <code>GetMediaForFragmentList</code> API:</p> <ul> <li> <p>A client can call <code>GetMediaForFragmentList</code> up to five times per second per stream. </p> </li> <li> <p>Kinesis Video Streams sends media data at a rate of up to 25 megabytes per second (or 200 megabits per second) during a <code>GetMediaForFragmentList</code> session. </p> </li> </ul>

#### `listFragments`

``` purescript
listFragments :: forall eff. ListFragmentsInput -> Aff (err :: RequestError | eff) ListFragmentsOutput
```

<p>Returns a list of <a>Fragment</a> objects from the specified stream and start location within the archived data.</p>

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

#### `ContentType`

``` purescript
newtype ContentType
  = ContentType String
```

##### Instances
``` purescript
Newtype ContentType _
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

#### `Fragment`

``` purescript
newtype Fragment
  = Fragment { "FragmentNumber" :: NullOrUndefined (String), "FragmentSizeInBytes" :: NullOrUndefined (Number), "ProducerTimestamp" :: NullOrUndefined (Number), "ServerTimestamp" :: NullOrUndefined (Number), "FragmentLengthInMilliseconds" :: NullOrUndefined (Number) }
```

<p>Represents a segment of video or other time-delimited data.</p>

##### Instances
``` purescript
Newtype Fragment _
```

#### `FragmentList`

``` purescript
newtype FragmentList
  = FragmentList (Array Fragment)
```

##### Instances
``` purescript
Newtype FragmentList _
```

#### `FragmentNumberList`

``` purescript
newtype FragmentNumberList
  = FragmentNumberList (Array FragmentNumberString)
```

##### Instances
``` purescript
Newtype FragmentNumberList _
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

#### `FragmentSelector`

``` purescript
newtype FragmentSelector
  = FragmentSelector { "FragmentSelectorType" :: FragmentSelectorType, "TimestampRange" :: TimestampRange }
```

<p>Describes the time stamp range and time stamp origin of a range of fragments.</p>

##### Instances
``` purescript
Newtype FragmentSelector _
```

#### `FragmentSelectorType`

``` purescript
newtype FragmentSelectorType
  = FragmentSelectorType String
```

##### Instances
``` purescript
Newtype FragmentSelectorType _
```

#### `GetMediaForFragmentListInput`

``` purescript
newtype GetMediaForFragmentListInput
  = GetMediaForFragmentListInput { "StreamName" :: StreamName, "Fragments" :: FragmentNumberList }
```

##### Instances
``` purescript
Newtype GetMediaForFragmentListInput _
```

#### `GetMediaForFragmentListOutput`

``` purescript
newtype GetMediaForFragmentListOutput
  = GetMediaForFragmentListOutput { "ContentType" :: NullOrUndefined (ContentType), "Payload" :: NullOrUndefined (Payload) }
```

##### Instances
``` purescript
Newtype GetMediaForFragmentListOutput _
```

#### `InvalidArgumentException`

``` purescript
newtype InvalidArgumentException
  = InvalidArgumentException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>A specified parameter exceeds its restrictions, is not supported, or can't be used.</p>

##### Instances
``` purescript
Newtype InvalidArgumentException _
```

#### `ListFragmentsInput`

``` purescript
newtype ListFragmentsInput
  = ListFragmentsInput { "StreamName" :: StreamName, "MaxResults" :: NullOrUndefined (PageLimit), "NextToken" :: NullOrUndefined (String), "FragmentSelector" :: NullOrUndefined (FragmentSelector) }
```

##### Instances
``` purescript
Newtype ListFragmentsInput _
```

#### `ListFragmentsOutput`

``` purescript
newtype ListFragmentsOutput
  = ListFragmentsOutput { "Fragments" :: NullOrUndefined (FragmentList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListFragmentsOutput _
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

#### `PageLimit`

``` purescript
newtype PageLimit
  = PageLimit Number
```

##### Instances
``` purescript
Newtype PageLimit _
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

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Kinesis Video Streams can't find the stream that you specified.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
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

#### `TimestampRange`

``` purescript
newtype TimestampRange
  = TimestampRange { "StartTimestamp" :: Number, "EndTimestamp" :: Number }
```

<p>The range of time stamps for which to return fragments.</p>

##### Instances
``` purescript
Newtype TimestampRange _
```


