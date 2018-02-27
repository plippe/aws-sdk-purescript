## Module AWS.Comprehend

<p>Amazon Comprehend is an AWS service for gaining insight into the content of documents. Use these actions to determine the topics contained in your documents, the topics they discuss, the predominant sentiment expressed in them, the predominant language used, and more.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchDetectDominantLanguage`

``` purescript
batchDetectDominantLanguage :: forall eff. BatchDetectDominantLanguageRequest -> Aff (err :: RequestError | eff) BatchDetectDominantLanguageResponse
```

<p>Determines the dominant language of the input text for a batch of documents. For a list of languages that Amazon Comprehend can detect, see <a href="http://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html">Amazon Comprehend Supported Languages</a>. </p>

#### `batchDetectEntities`

``` purescript
batchDetectEntities :: forall eff. BatchDetectEntitiesRequest -> Aff (err :: RequestError | eff) BatchDetectEntitiesResponse
```

<p>Inspects the text of a batch of documents and returns information about them. For more information about entities, see <a>how-entities</a> </p>

#### `batchDetectKeyPhrases`

``` purescript
batchDetectKeyPhrases :: forall eff. BatchDetectKeyPhrasesRequest -> Aff (err :: RequestError | eff) BatchDetectKeyPhrasesResponse
```

<p>Detects the key noun phrases found in a batch of documents.</p>

#### `batchDetectSentiment`

``` purescript
batchDetectSentiment :: forall eff. BatchDetectSentimentRequest -> Aff (err :: RequestError | eff) BatchDetectSentimentResponse
```

<p>Inspects a batch of documents and returns an inference of the prevailing sentiment, <code>POSITIVE</code>, <code>NEUTRAL</code>, <code>MIXED</code>, or <code>NEGATIVE</code>, in each one.</p>

#### `describeTopicsDetectionJob`

``` purescript
describeTopicsDetectionJob :: forall eff. DescribeTopicsDetectionJobRequest -> Aff (err :: RequestError | eff) DescribeTopicsDetectionJobResponse
```

<p>Gets the properties associated with a topic detection job. Use this operation to get the status of a detection job.</p>

#### `detectDominantLanguage`

``` purescript
detectDominantLanguage :: forall eff. DetectDominantLanguageRequest -> Aff (err :: RequestError | eff) DetectDominantLanguageResponse
```

<p>Determines the dominant language of the input text. For a list of languages that Amazon Comprehend can detect, see <a href="http://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html">Amazon Comprehend Supported Languages</a>. </p>

#### `detectEntities`

``` purescript
detectEntities :: forall eff. DetectEntitiesRequest -> Aff (err :: RequestError | eff) DetectEntitiesResponse
```

<p>Inspects text for entities, and returns information about them. For more information, about entities, see <a>how-entities</a>. </p>

#### `detectKeyPhrases`

``` purescript
detectKeyPhrases :: forall eff. DetectKeyPhrasesRequest -> Aff (err :: RequestError | eff) DetectKeyPhrasesResponse
```

<p>Detects the key noun phrases found in the text. </p>

#### `detectSentiment`

``` purescript
detectSentiment :: forall eff. DetectSentimentRequest -> Aff (err :: RequestError | eff) DetectSentimentResponse
```

<p>Inspects text and returns an inference of the prevailing sentiment (<code>POSITIVE</code>, <code>NEUTRAL</code>, <code>MIXED</code>, or <code>NEGATIVE</code>). </p>

#### `listTopicsDetectionJobs`

``` purescript
listTopicsDetectionJobs :: forall eff. ListTopicsDetectionJobsRequest -> Aff (err :: RequestError | eff) ListTopicsDetectionJobsResponse
```

<p>Gets a list of the topic detection jobs that you have submitted.</p>

#### `startTopicsDetectionJob`

``` purescript
startTopicsDetectionJob :: forall eff. StartTopicsDetectionJobRequest -> Aff (err :: RequestError | eff) StartTopicsDetectionJobResponse
```

<p>Starts an asynchronous topic detection job. Use the <code>DescribeTopicDetectionJob</code> operation to track the status of a job.</p>

#### `AnyLengthString`

``` purescript
newtype AnyLengthString
  = AnyLengthString String
```

##### Instances
``` purescript
Newtype AnyLengthString _
```

#### `BatchDetectDominantLanguageItemResult`

``` purescript
newtype BatchDetectDominantLanguageItemResult
  = BatchDetectDominantLanguageItemResult { "Index" :: NullOrUndefined (Int), "Languages" :: NullOrUndefined (ListOfDominantLanguages) }
```

<p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>

##### Instances
``` purescript
Newtype BatchDetectDominantLanguageItemResult _
```

#### `BatchDetectDominantLanguageRequest`

``` purescript
newtype BatchDetectDominantLanguageRequest
  = BatchDetectDominantLanguageRequest { "TextList" :: StringList }
```

##### Instances
``` purescript
Newtype BatchDetectDominantLanguageRequest _
```

#### `BatchDetectDominantLanguageResponse`

``` purescript
newtype BatchDetectDominantLanguageResponse
  = BatchDetectDominantLanguageResponse { "ResultList" :: ListOfDetectDominantLanguageResult, "ErrorList" :: BatchItemErrorList }
```

##### Instances
``` purescript
Newtype BatchDetectDominantLanguageResponse _
```

#### `BatchDetectEntitiesItemResult`

``` purescript
newtype BatchDetectEntitiesItemResult
  = BatchDetectEntitiesItemResult { "Index" :: NullOrUndefined (Int), "Entities" :: NullOrUndefined (ListOfEntities) }
```

<p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>

##### Instances
``` purescript
Newtype BatchDetectEntitiesItemResult _
```

#### `BatchDetectEntitiesRequest`

``` purescript
newtype BatchDetectEntitiesRequest
  = BatchDetectEntitiesRequest { "TextList" :: StringList, "LanguageCode" :: String }
```

##### Instances
``` purescript
Newtype BatchDetectEntitiesRequest _
```

#### `BatchDetectEntitiesResponse`

``` purescript
newtype BatchDetectEntitiesResponse
  = BatchDetectEntitiesResponse { "ResultList" :: ListOfDetectEntitiesResult, "ErrorList" :: BatchItemErrorList }
```

##### Instances
``` purescript
Newtype BatchDetectEntitiesResponse _
```

#### `BatchDetectKeyPhrasesItemResult`

``` purescript
newtype BatchDetectKeyPhrasesItemResult
  = BatchDetectKeyPhrasesItemResult { "Index" :: NullOrUndefined (Int), "KeyPhrases" :: NullOrUndefined (ListOfKeyPhrases) }
```

<p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>

##### Instances
``` purescript
Newtype BatchDetectKeyPhrasesItemResult _
```

#### `BatchDetectKeyPhrasesRequest`

``` purescript
newtype BatchDetectKeyPhrasesRequest
  = BatchDetectKeyPhrasesRequest { "TextList" :: StringList, "LanguageCode" :: String }
```

##### Instances
``` purescript
Newtype BatchDetectKeyPhrasesRequest _
```

#### `BatchDetectKeyPhrasesResponse`

``` purescript
newtype BatchDetectKeyPhrasesResponse
  = BatchDetectKeyPhrasesResponse { "ResultList" :: ListOfDetectKeyPhrasesResult, "ErrorList" :: BatchItemErrorList }
```

##### Instances
``` purescript
Newtype BatchDetectKeyPhrasesResponse _
```

#### `BatchDetectSentimentItemResult`

``` purescript
newtype BatchDetectSentimentItemResult
  = BatchDetectSentimentItemResult { "Index" :: NullOrUndefined (Int), "Sentiment" :: NullOrUndefined (SentimentType), "SentimentScore" :: NullOrUndefined (SentimentScore) }
```

<p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>

##### Instances
``` purescript
Newtype BatchDetectSentimentItemResult _
```

#### `BatchDetectSentimentRequest`

``` purescript
newtype BatchDetectSentimentRequest
  = BatchDetectSentimentRequest { "TextList" :: StringList, "LanguageCode" :: String }
```

##### Instances
``` purescript
Newtype BatchDetectSentimentRequest _
```

#### `BatchDetectSentimentResponse`

``` purescript
newtype BatchDetectSentimentResponse
  = BatchDetectSentimentResponse { "ResultList" :: ListOfDetectSentimentResult, "ErrorList" :: BatchItemErrorList }
```

##### Instances
``` purescript
Newtype BatchDetectSentimentResponse _
```

#### `BatchItemError`

``` purescript
newtype BatchItemError
  = BatchItemError { "Index" :: NullOrUndefined (Int), "ErrorCode" :: NullOrUndefined (String), "ErrorMessage" :: NullOrUndefined (String) }
```

<p>Describes an error that occurred while processing a document in a batch. The operation returns on <code>BatchItemError</code> object for each document that contained an error.</p>

##### Instances
``` purescript
Newtype BatchItemError _
```

#### `BatchItemErrorList`

``` purescript
newtype BatchItemErrorList
  = BatchItemErrorList (Array BatchItemError)
```

##### Instances
``` purescript
Newtype BatchItemErrorList _
```

#### `BatchSizeLimitExceededException`

``` purescript
newtype BatchSizeLimitExceededException
  = BatchSizeLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The number of documents in the request exceeds the limit of 25. Try your request again with fewer documents.</p>

##### Instances
``` purescript
Newtype BatchSizeLimitExceededException _
```

#### `ClientRequestTokenString`

``` purescript
newtype ClientRequestTokenString
  = ClientRequestTokenString String
```

##### Instances
``` purescript
Newtype ClientRequestTokenString _
```

#### `DescribeTopicsDetectionJobRequest`

``` purescript
newtype DescribeTopicsDetectionJobRequest
  = DescribeTopicsDetectionJobRequest { "JobId" :: JobId }
```

##### Instances
``` purescript
Newtype DescribeTopicsDetectionJobRequest _
```

#### `DescribeTopicsDetectionJobResponse`

``` purescript
newtype DescribeTopicsDetectionJobResponse
  = DescribeTopicsDetectionJobResponse { "TopicsDetectionJobProperties" :: NullOrUndefined (TopicsDetectionJobProperties) }
```

##### Instances
``` purescript
Newtype DescribeTopicsDetectionJobResponse _
```

#### `DetectDominantLanguageRequest`

``` purescript
newtype DetectDominantLanguageRequest
  = DetectDominantLanguageRequest { "Text" :: String }
```

##### Instances
``` purescript
Newtype DetectDominantLanguageRequest _
```

#### `DetectDominantLanguageResponse`

``` purescript
newtype DetectDominantLanguageResponse
  = DetectDominantLanguageResponse { "Languages" :: NullOrUndefined (ListOfDominantLanguages) }
```

##### Instances
``` purescript
Newtype DetectDominantLanguageResponse _
```

#### `DetectEntitiesRequest`

``` purescript
newtype DetectEntitiesRequest
  = DetectEntitiesRequest { "Text" :: String, "LanguageCode" :: LanguageCode }
```

##### Instances
``` purescript
Newtype DetectEntitiesRequest _
```

#### `DetectEntitiesResponse`

``` purescript
newtype DetectEntitiesResponse
  = DetectEntitiesResponse { "Entities" :: NullOrUndefined (ListOfEntities) }
```

##### Instances
``` purescript
Newtype DetectEntitiesResponse _
```

#### `DetectKeyPhrasesRequest`

``` purescript
newtype DetectKeyPhrasesRequest
  = DetectKeyPhrasesRequest { "Text" :: String, "LanguageCode" :: LanguageCode }
```

##### Instances
``` purescript
Newtype DetectKeyPhrasesRequest _
```

#### `DetectKeyPhrasesResponse`

``` purescript
newtype DetectKeyPhrasesResponse
  = DetectKeyPhrasesResponse { "KeyPhrases" :: NullOrUndefined (ListOfKeyPhrases) }
```

##### Instances
``` purescript
Newtype DetectKeyPhrasesResponse _
```

#### `DetectSentimentRequest`

``` purescript
newtype DetectSentimentRequest
  = DetectSentimentRequest { "Text" :: String, "LanguageCode" :: LanguageCode }
```

##### Instances
``` purescript
Newtype DetectSentimentRequest _
```

#### `DetectSentimentResponse`

``` purescript
newtype DetectSentimentResponse
  = DetectSentimentResponse { "Sentiment" :: NullOrUndefined (SentimentType), "SentimentScore" :: NullOrUndefined (SentimentScore) }
```

##### Instances
``` purescript
Newtype DetectSentimentResponse _
```

#### `DominantLanguage`

``` purescript
newtype DominantLanguage
  = DominantLanguage { "LanguageCode" :: NullOrUndefined (String), "Score" :: NullOrUndefined (Number) }
```

<p>Returns the code for the dominant language in the input text and the level of confidence that Amazon Comprehend has in the accuracy of the detection.</p>

##### Instances
``` purescript
Newtype DominantLanguage _
```

#### `Entity`

``` purescript
newtype Entity
  = Entity { "Score" :: NullOrUndefined (Number), "Type" :: NullOrUndefined (EntityType), "Text" :: NullOrUndefined (String), "BeginOffset" :: NullOrUndefined (Int), "EndOffset" :: NullOrUndefined (Int) }
```

<p>Provides information about an entity. </p> <p> </p>

##### Instances
``` purescript
Newtype Entity _
```

#### `EntityType`

``` purescript
newtype EntityType
  = EntityType String
```

##### Instances
``` purescript
Newtype EntityType _
```

#### `IamRoleArn`

``` purescript
newtype IamRoleArn
  = IamRoleArn String
```

##### Instances
``` purescript
Newtype IamRoleArn _
```

#### `InputDataConfig`

``` purescript
newtype InputDataConfig
  = InputDataConfig { "S3Uri" :: S3Uri, "InputFormat" :: NullOrUndefined (InputFormat) }
```

<p>The input properties for a topic detection job.</p>

##### Instances
``` purescript
Newtype InputDataConfig _
```

#### `InputFormat`

``` purescript
newtype InputFormat
  = InputFormat String
```

##### Instances
``` purescript
Newtype InputFormat _
```

#### `InternalServerException`

``` purescript
newtype InternalServerException
  = InternalServerException { "Message" :: NullOrUndefined (String) }
```

<p>An internal server error occurred. Retry your request.</p>

##### Instances
``` purescript
Newtype InternalServerException _
```

#### `InvalidFilterException`

``` purescript
newtype InvalidFilterException
  = InvalidFilterException { "Message" :: NullOrUndefined (String) }
```

<p>The filter specified for the <code>ListTopicDetectionJobs</code> operation is invalid. Specify a different filter.</p>

##### Instances
``` purescript
Newtype InvalidFilterException _
```

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message" :: NullOrUndefined (String) }
```

<p>The request is invalid.</p>

##### Instances
``` purescript
Newtype InvalidRequestException _
```

#### `JobId`

``` purescript
newtype JobId
  = JobId String
```

##### Instances
``` purescript
Newtype JobId _
```

#### `JobName`

``` purescript
newtype JobName
  = JobName String
```

##### Instances
``` purescript
Newtype JobName _
```

#### `JobNotFoundException`

``` purescript
newtype JobNotFoundException
  = JobNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>The specified job was not found. Check the job ID and try again.</p>

##### Instances
``` purescript
Newtype JobNotFoundException _
```

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

##### Instances
``` purescript
Newtype JobStatus _
```

#### `KeyPhrase`

``` purescript
newtype KeyPhrase
  = KeyPhrase { "Score" :: NullOrUndefined (Number), "Text" :: NullOrUndefined (String), "BeginOffset" :: NullOrUndefined (Int), "EndOffset" :: NullOrUndefined (Int) }
```

<p>Describes a key noun phrase.</p>

##### Instances
``` purescript
Newtype KeyPhrase _
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

#### `ListOfDetectDominantLanguageResult`

``` purescript
newtype ListOfDetectDominantLanguageResult
  = ListOfDetectDominantLanguageResult (Array BatchDetectDominantLanguageItemResult)
```

##### Instances
``` purescript
Newtype ListOfDetectDominantLanguageResult _
```

#### `ListOfDetectEntitiesResult`

``` purescript
newtype ListOfDetectEntitiesResult
  = ListOfDetectEntitiesResult (Array BatchDetectEntitiesItemResult)
```

##### Instances
``` purescript
Newtype ListOfDetectEntitiesResult _
```

#### `ListOfDetectKeyPhrasesResult`

``` purescript
newtype ListOfDetectKeyPhrasesResult
  = ListOfDetectKeyPhrasesResult (Array BatchDetectKeyPhrasesItemResult)
```

##### Instances
``` purescript
Newtype ListOfDetectKeyPhrasesResult _
```

#### `ListOfDetectSentimentResult`

``` purescript
newtype ListOfDetectSentimentResult
  = ListOfDetectSentimentResult (Array BatchDetectSentimentItemResult)
```

##### Instances
``` purescript
Newtype ListOfDetectSentimentResult _
```

#### `ListOfDominantLanguages`

``` purescript
newtype ListOfDominantLanguages
  = ListOfDominantLanguages (Array DominantLanguage)
```

##### Instances
``` purescript
Newtype ListOfDominantLanguages _
```

#### `ListOfEntities`

``` purescript
newtype ListOfEntities
  = ListOfEntities (Array Entity)
```

##### Instances
``` purescript
Newtype ListOfEntities _
```

#### `ListOfKeyPhrases`

``` purescript
newtype ListOfKeyPhrases
  = ListOfKeyPhrases (Array KeyPhrase)
```

##### Instances
``` purescript
Newtype ListOfKeyPhrases _
```

#### `ListTopicsDetectionJobsRequest`

``` purescript
newtype ListTopicsDetectionJobsRequest
  = ListTopicsDetectionJobsRequest { "Filter" :: NullOrUndefined (TopicsDetectionJobFilter), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (MaxResultsInteger) }
```

##### Instances
``` purescript
Newtype ListTopicsDetectionJobsRequest _
```

#### `ListTopicsDetectionJobsResponse`

``` purescript
newtype ListTopicsDetectionJobsResponse
  = ListTopicsDetectionJobsResponse { "TopicsDetectionJobPropertiesList" :: NullOrUndefined (TopicsDetectionJobPropertiesList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListTopicsDetectionJobsResponse _
```

#### `MaxResultsInteger`

``` purescript
newtype MaxResultsInteger
  = MaxResultsInteger Int
```

##### Instances
``` purescript
Newtype MaxResultsInteger _
```

#### `NumberOfTopicsInteger`

``` purescript
newtype NumberOfTopicsInteger
  = NumberOfTopicsInteger Int
```

##### Instances
``` purescript
Newtype NumberOfTopicsInteger _
```

#### `OutputDataConfig`

``` purescript
newtype OutputDataConfig
  = OutputDataConfig { "S3Uri" :: S3Uri }
```

<p>Provides configuration parameters for the output of topic detection jobs.</p> <p/>

##### Instances
``` purescript
Newtype OutputDataConfig _
```

#### `S3Uri`

``` purescript
newtype S3Uri
  = S3Uri String
```

##### Instances
``` purescript
Newtype S3Uri _
```

#### `SentimentScore`

``` purescript
newtype SentimentScore
  = SentimentScore { "Positive" :: NullOrUndefined (Number), "Negative" :: NullOrUndefined (Number), "Neutral" :: NullOrUndefined (Number), "Mixed" :: NullOrUndefined (Number) }
```

<p>Describes the level of confidence that Amazon Comprehend has in the accuracy of its detection of sentiments.</p>

##### Instances
``` purescript
Newtype SentimentScore _
```

#### `SentimentType`

``` purescript
newtype SentimentType
  = SentimentType String
```

##### Instances
``` purescript
Newtype SentimentType _
```

#### `StartTopicsDetectionJobRequest`

``` purescript
newtype StartTopicsDetectionJobRequest
  = StartTopicsDetectionJobRequest { "InputDataConfig" :: InputDataConfig, "OutputDataConfig" :: OutputDataConfig, "DataAccessRoleArn" :: IamRoleArn, "JobName" :: NullOrUndefined (JobName), "NumberOfTopics" :: NullOrUndefined (NumberOfTopicsInteger), "ClientRequestToken" :: NullOrUndefined (ClientRequestTokenString) }
```

##### Instances
``` purescript
Newtype StartTopicsDetectionJobRequest _
```

#### `StartTopicsDetectionJobResponse`

``` purescript
newtype StartTopicsDetectionJobResponse
  = StartTopicsDetectionJobResponse { "JobId" :: NullOrUndefined (JobId), "JobStatus" :: NullOrUndefined (JobStatus) }
```

##### Instances
``` purescript
Newtype StartTopicsDetectionJobResponse _
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

##### Instances
``` purescript
Newtype StringList _
```

#### `TextSizeLimitExceededException`

``` purescript
newtype TextSizeLimitExceededException
  = TextSizeLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The size of the input text exceeds the limit. Use a smaller document.</p>

##### Instances
``` purescript
Newtype TextSizeLimitExceededException _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String) }
```

<p>The number of requests exceeds the limit. Resubmit your request later.</p>

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `TopicsDetectionJobFilter`

``` purescript
newtype TopicsDetectionJobFilter
  = TopicsDetectionJobFilter { "JobName" :: NullOrUndefined (JobName), "JobStatus" :: NullOrUndefined (JobStatus), "SubmitTimeBefore" :: NullOrUndefined (Number), "SubmitTimeAfter" :: NullOrUndefined (Number) }
```

<p>Provides information for filtering topic detection jobs. For more information, see .</p>

##### Instances
``` purescript
Newtype TopicsDetectionJobFilter _
```

#### `TopicsDetectionJobProperties`

``` purescript
newtype TopicsDetectionJobProperties
  = TopicsDetectionJobProperties { "JobId" :: NullOrUndefined (JobId), "JobName" :: NullOrUndefined (JobName), "JobStatus" :: NullOrUndefined (JobStatus), "Message" :: NullOrUndefined (AnyLengthString), "SubmitTime" :: NullOrUndefined (Number), "EndTime" :: NullOrUndefined (Number), "InputDataConfig" :: NullOrUndefined (InputDataConfig), "OutputDataConfig" :: NullOrUndefined (OutputDataConfig), "NumberOfTopics" :: NullOrUndefined (Int) }
```

<p>Provides information about a topic detection job.</p>

##### Instances
``` purescript
Newtype TopicsDetectionJobProperties _
```

#### `TopicsDetectionJobPropertiesList`

``` purescript
newtype TopicsDetectionJobPropertiesList
  = TopicsDetectionJobPropertiesList (Array TopicsDetectionJobProperties)
```

##### Instances
``` purescript
Newtype TopicsDetectionJobPropertiesList _
```

#### `UnsupportedLanguageException`

``` purescript
newtype UnsupportedLanguageException
  = UnsupportedLanguageException { "Message" :: NullOrUndefined (String) }
```

<p>Amazon Comprehend can't process the language of the input text. For all APIs except <code>DetectDominantLanguage</code>, Amazon Comprehend accepts only English or Spanish text. For the <code>DetectDominantLanguage</code> API, Amazon Comprehend detects 100 languages. For a list of languages, see <a>how-languages</a> </p>

##### Instances
``` purescript
Newtype UnsupportedLanguageException _
```


