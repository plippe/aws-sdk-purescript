## Module AWS.TranscribeService

<p>Operations and objects for transcribing speech to text.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `getTranscriptionJob`

``` purescript
getTranscriptionJob :: forall eff. GetTranscriptionJobRequest -> Aff (err :: RequestError | eff) GetTranscriptionJobResponse
```

<p>Returns information about a transcription job. To see the status of the job, check the <code>Status</code> field. If the status is <code>COMPLETE</code>, the job is finished and you can find the results at the location specified in the <code>TranscriptionFileUri</code> field.</p>

#### `listTranscriptionJobs`

``` purescript
listTranscriptionJobs :: forall eff. ListTranscriptionJobsRequest -> Aff (err :: RequestError | eff) ListTranscriptionJobsResponse
```

<p>Lists transcription jobs with the specified status.</p>

#### `startTranscriptionJob`

``` purescript
startTranscriptionJob :: forall eff. StartTranscriptionJobRequest -> Aff (err :: RequestError | eff) StartTranscriptionJobResponse
```

<p>Starts an asynchronous job to transcribe speech to text.</p>

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message" :: NullOrUndefined (FailureReason) }
```

<p>There is a problem with one of the input fields. Check the S3 bucket name, make sure that the job name is not a duplicate, and confirm that you are using the correct file format. Then resend your request.</p>

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `ConflictException`

``` purescript
newtype ConflictException
  = ConflictException { "Message" :: NullOrUndefined (String) }
```

<p>The <code>JobName</code> field is a duplicate of a previously entered job name. Resend your request with a different name.</p>

##### Instances
``` purescript
Newtype ConflictException _
```

#### `DateTime`

``` purescript
newtype DateTime
  = DateTime Number
```

##### Instances
``` purescript
Newtype DateTime _
```

#### `FailureReason`

``` purescript
newtype FailureReason
  = FailureReason String
```

##### Instances
``` purescript
Newtype FailureReason _
```

#### `GetTranscriptionJobRequest`

``` purescript
newtype GetTranscriptionJobRequest
  = GetTranscriptionJobRequest { "TranscriptionJobName" :: TranscriptionJobName }
```

##### Instances
``` purescript
Newtype GetTranscriptionJobRequest _
```

#### `GetTranscriptionJobResponse`

``` purescript
newtype GetTranscriptionJobResponse
  = GetTranscriptionJobResponse { "TranscriptionJob" :: NullOrUndefined (TranscriptionJob) }
```

##### Instances
``` purescript
Newtype GetTranscriptionJobResponse _
```

#### `InternalFailureException`

``` purescript
newtype InternalFailureException
  = InternalFailureException { "Message" :: NullOrUndefined (String) }
```

<p>There was an internal error. Check the error message and try your request again.</p>

##### Instances
``` purescript
Newtype InternalFailureException _
```

#### `LanguageCode`

``` purescript
newtype LanguageCode
  = LanguageCode String
```

##### Instances
``` purescript
Newtype LanguageCode _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>Either you have sent too many requests or your input file is longer than 2 hours. Wait before you resend your request, or use a smaller file and resend the request.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListTranscriptionJobsRequest`

``` purescript
newtype ListTranscriptionJobsRequest
  = ListTranscriptionJobsRequest { "Status" :: TranscriptionJobStatus, "NextToken" :: NullOrUndefined (NextToken), "MaxResults" :: NullOrUndefined (MaxResults) }
```

##### Instances
``` purescript
Newtype ListTranscriptionJobsRequest _
```

#### `ListTranscriptionJobsResponse`

``` purescript
newtype ListTranscriptionJobsResponse
  = ListTranscriptionJobsResponse { "Status" :: NullOrUndefined (TranscriptionJobStatus), "NextToken" :: NullOrUndefined (NextToken), "TranscriptionJobSummaries" :: NullOrUndefined (TranscriptionJobSummaries) }
```

##### Instances
``` purescript
Newtype ListTranscriptionJobsResponse _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `Media`

``` purescript
newtype Media
  = Media { "MediaFileUri" :: NullOrUndefined (Uri) }
```

<p>Describes the input media file in a transcription request.</p>

##### Instances
``` purescript
Newtype Media _
```

#### `MediaFormat`

``` purescript
newtype MediaFormat
  = MediaFormat String
```

##### Instances
``` purescript
Newtype MediaFormat _
```

#### `MediaSampleRateHertz`

``` purescript
newtype MediaSampleRateHertz
  = MediaSampleRateHertz Int
```

##### Instances
``` purescript
Newtype MediaSampleRateHertz _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>We can't find the requested job. Check the job name and try your request again.</p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `StartTranscriptionJobRequest`

``` purescript
newtype StartTranscriptionJobRequest
  = StartTranscriptionJobRequest { "TranscriptionJobName" :: TranscriptionJobName, "LanguageCode" :: LanguageCode, "MediaSampleRateHertz" :: NullOrUndefined (MediaSampleRateHertz), "MediaFormat" :: MediaFormat, "Media" :: Media }
```

##### Instances
``` purescript
Newtype StartTranscriptionJobRequest _
```

#### `StartTranscriptionJobResponse`

``` purescript
newtype StartTranscriptionJobResponse
  = StartTranscriptionJobResponse { "TranscriptionJob" :: NullOrUndefined (TranscriptionJob) }
```

##### Instances
``` purescript
Newtype StartTranscriptionJobResponse _
```

#### `Transcript`

``` purescript
newtype Transcript
  = Transcript { "TranscriptFileUri" :: NullOrUndefined (Uri) }
```

<p>Describes the output of a transcription job.</p>

##### Instances
``` purescript
Newtype Transcript _
```

#### `TranscriptionJob`

``` purescript
newtype TranscriptionJob
  = TranscriptionJob { "TranscriptionJobName" :: NullOrUndefined (TranscriptionJobName), "TranscriptionJobStatus" :: NullOrUndefined (TranscriptionJobStatus), "LanguageCode" :: NullOrUndefined (LanguageCode), "MediaSampleRateHertz" :: NullOrUndefined (MediaSampleRateHertz), "MediaFormat" :: NullOrUndefined (MediaFormat), "Media" :: NullOrUndefined (Media), "Transcript" :: NullOrUndefined (Transcript), "CreationTime" :: NullOrUndefined (DateTime), "CompletionTime" :: NullOrUndefined (DateTime), "FailureReason" :: NullOrUndefined (FailureReason) }
```

<p>Describes an asynchronous transcription job that was created with the <code>StartTranscriptionJob</code> operation.</p>

##### Instances
``` purescript
Newtype TranscriptionJob _
```

#### `TranscriptionJobName`

``` purescript
newtype TranscriptionJobName
  = TranscriptionJobName String
```

##### Instances
``` purescript
Newtype TranscriptionJobName _
```

#### `TranscriptionJobStatus`

``` purescript
newtype TranscriptionJobStatus
  = TranscriptionJobStatus String
```

##### Instances
``` purescript
Newtype TranscriptionJobStatus _
```

#### `TranscriptionJobSummaries`

``` purescript
newtype TranscriptionJobSummaries
  = TranscriptionJobSummaries (Array TranscriptionJobSummary)
```

##### Instances
``` purescript
Newtype TranscriptionJobSummaries _
```

#### `TranscriptionJobSummary`

``` purescript
newtype TranscriptionJobSummary
  = TranscriptionJobSummary { "TranscriptionJobName" :: NullOrUndefined (TranscriptionJobName), "CreationTime" :: NullOrUndefined (DateTime), "CompletionTime" :: NullOrUndefined (DateTime), "LanguageCode" :: NullOrUndefined (LanguageCode), "TranscriptionJobStatus" :: NullOrUndefined (TranscriptionJobStatus), "FailureReason" :: NullOrUndefined (FailureReason) }
```

<p>Provides a summary of information about a transcription job.</p>

##### Instances
``` purescript
Newtype TranscriptionJobSummary _
```

#### `Uri`

``` purescript
newtype Uri
  = Uri String
```

##### Instances
``` purescript
Newtype Uri _
```


