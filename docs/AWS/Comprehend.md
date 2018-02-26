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

#### `BatchDetectDominantLanguageItemResult`

``` purescript
newtype BatchDetectDominantLanguageItemResult
  = BatchDetectDominantLanguageItemResult { "Index" :: NullOrUndefined (Int), "Languages" :: NullOrUndefined (ListOfDominantLanguages) }
```

<p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>

#### `BatchDetectDominantLanguageRequest`

``` purescript
newtype BatchDetectDominantLanguageRequest
  = BatchDetectDominantLanguageRequest { "TextList" :: StringList }
```

#### `BatchDetectDominantLanguageResponse`

``` purescript
newtype BatchDetectDominantLanguageResponse
  = BatchDetectDominantLanguageResponse { "ResultList" :: ListOfDetectDominantLanguageResult, "ErrorList" :: BatchItemErrorList }
```

#### `BatchDetectEntitiesItemResult`

``` purescript
newtype BatchDetectEntitiesItemResult
  = BatchDetectEntitiesItemResult { "Index" :: NullOrUndefined (Int), "Entities" :: NullOrUndefined (ListOfEntities) }
```

<p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>

#### `BatchDetectEntitiesRequest`

``` purescript
newtype BatchDetectEntitiesRequest
  = BatchDetectEntitiesRequest { "TextList" :: StringList, "LanguageCode" :: String }
```

#### `BatchDetectEntitiesResponse`

``` purescript
newtype BatchDetectEntitiesResponse
  = BatchDetectEntitiesResponse { "ResultList" :: ListOfDetectEntitiesResult, "ErrorList" :: BatchItemErrorList }
```

#### `BatchDetectKeyPhrasesItemResult`

``` purescript
newtype BatchDetectKeyPhrasesItemResult
  = BatchDetectKeyPhrasesItemResult { "Index" :: NullOrUndefined (Int), "KeyPhrases" :: NullOrUndefined (ListOfKeyPhrases) }
```

<p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>

#### `BatchDetectKeyPhrasesRequest`

``` purescript
newtype BatchDetectKeyPhrasesRequest
  = BatchDetectKeyPhrasesRequest { "TextList" :: StringList, "LanguageCode" :: String }
```

#### `BatchDetectKeyPhrasesResponse`

``` purescript
newtype BatchDetectKeyPhrasesResponse
  = BatchDetectKeyPhrasesResponse { "ResultList" :: ListOfDetectKeyPhrasesResult, "ErrorList" :: BatchItemErrorList }
```

#### `BatchDetectSentimentItemResult`

``` purescript
newtype BatchDetectSentimentItemResult
  = BatchDetectSentimentItemResult { "Index" :: NullOrUndefined (Int), "Sentiment" :: NullOrUndefined (SentimentType), "SentimentScore" :: NullOrUndefined (SentimentScore) }
```

<p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>

#### `BatchDetectSentimentRequest`

``` purescript
newtype BatchDetectSentimentRequest
  = BatchDetectSentimentRequest { "TextList" :: StringList, "LanguageCode" :: String }
```

#### `BatchDetectSentimentResponse`

``` purescript
newtype BatchDetectSentimentResponse
  = BatchDetectSentimentResponse { "ResultList" :: ListOfDetectSentimentResult, "ErrorList" :: BatchItemErrorList }
```

#### `BatchItemError`

``` purescript
newtype BatchItemError
  = BatchItemError { "Index" :: NullOrUndefined (Int), "ErrorCode" :: NullOrUndefined (String), "ErrorMessage" :: NullOrUndefined (String) }
```

<p>Describes an error that occurred while processing a document in a batch. The operation returns on <code>BatchItemError</code> object for each document that contained an error.</p>

#### `BatchItemErrorList`

``` purescript
newtype BatchItemErrorList
  = BatchItemErrorList (Array BatchItemError)
```

#### `BatchSizeLimitExceededException`

``` purescript
newtype BatchSizeLimitExceededException
  = BatchSizeLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The number of documents in the request exceeds the limit of 25. Try your request again with fewer documents.</p>

#### `ClientRequestTokenString`

``` purescript
newtype ClientRequestTokenString
  = ClientRequestTokenString String
```

#### `DescribeTopicsDetectionJobRequest`

``` purescript
newtype DescribeTopicsDetectionJobRequest
  = DescribeTopicsDetectionJobRequest { "JobId" :: JobId }
```

#### `DescribeTopicsDetectionJobResponse`

``` purescript
newtype DescribeTopicsDetectionJobResponse
  = DescribeTopicsDetectionJobResponse { "TopicsDetectionJobProperties" :: NullOrUndefined (TopicsDetectionJobProperties) }
```

#### `DetectDominantLanguageRequest`

``` purescript
newtype DetectDominantLanguageRequest
  = DetectDominantLanguageRequest { "Text" :: String }
```

#### `DetectDominantLanguageResponse`

``` purescript
newtype DetectDominantLanguageResponse
  = DetectDominantLanguageResponse { "Languages" :: NullOrUndefined (ListOfDominantLanguages) }
```

#### `DetectEntitiesRequest`

``` purescript
newtype DetectEntitiesRequest
  = DetectEntitiesRequest { "Text" :: String, "LanguageCode" :: LanguageCode }
```

#### `DetectEntitiesResponse`

``` purescript
newtype DetectEntitiesResponse
  = DetectEntitiesResponse { "Entities" :: NullOrUndefined (ListOfEntities) }
```

#### `DetectKeyPhrasesRequest`

``` purescript
newtype DetectKeyPhrasesRequest
  = DetectKeyPhrasesRequest { "Text" :: String, "LanguageCode" :: LanguageCode }
```

#### `DetectKeyPhrasesResponse`

``` purescript
newtype DetectKeyPhrasesResponse
  = DetectKeyPhrasesResponse { "KeyPhrases" :: NullOrUndefined (ListOfKeyPhrases) }
```

#### `DetectSentimentRequest`

``` purescript
newtype DetectSentimentRequest
  = DetectSentimentRequest { "Text" :: String, "LanguageCode" :: LanguageCode }
```

#### `DetectSentimentResponse`

``` purescript
newtype DetectSentimentResponse
  = DetectSentimentResponse { "Sentiment" :: NullOrUndefined (SentimentType), "SentimentScore" :: NullOrUndefined (SentimentScore) }
```

#### `DominantLanguage`

``` purescript
newtype DominantLanguage
  = DominantLanguage { "LanguageCode" :: NullOrUndefined (String), "Score" :: NullOrUndefined (Number) }
```

<p>Returns the code for the dominant language in the input text and the level of confidence that Amazon Comprehend has in the accuracy of the detection.</p>

#### `Entity`

``` purescript
newtype Entity
  = Entity { "Score" :: NullOrUndefined (Number), "Type" :: NullOrUndefined (EntityType), "Text" :: NullOrUndefined (String), "BeginOffset" :: NullOrUndefined (Int), "EndOffset" :: NullOrUndefined (Int) }
```

<p>Provides information about an entity. </p> <p> </p>

#### `EntityType`

``` purescript
newtype EntityType
  = EntityType String
```

#### `IamRoleArn`

``` purescript
newtype IamRoleArn
  = IamRoleArn String
```

#### `InputDataConfig`

``` purescript
newtype InputDataConfig
  = InputDataConfig { "S3Uri" :: S3Uri, "InputFormat" :: NullOrUndefined (InputFormat) }
```

<p>The input properties for a topic detection job.</p>

#### `InputFormat`

``` purescript
newtype InputFormat
  = InputFormat String
```

#### `InternalServerException`

``` purescript
newtype InternalServerException
  = InternalServerException { "Message" :: NullOrUndefined (String) }
```

<p>An internal server error occurred. Retry your request.</p>

#### `InvalidFilterException`

``` purescript
newtype InvalidFilterException
  = InvalidFilterException { "Message" :: NullOrUndefined (String) }
```

<p>The filter specified for the <code>ListTopicDetectionJobs</code> operation is invalid. Specify a different filter.</p>

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message" :: NullOrUndefined (String) }
```

<p>The request is invalid.</p>

#### `JobId`

``` purescript
newtype JobId
  = JobId String
```

#### `JobName`

``` purescript
newtype JobName
  = JobName String
```

#### `JobNotFoundException`

``` purescript
newtype JobNotFoundException
  = JobNotFoundException { "Message" :: NullOrUndefined (String) }
```

<p>The specified job was not found. Check the job ID and try again.</p>

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

#### `KeyPhrase`

``` purescript
newtype KeyPhrase
  = KeyPhrase { "Score" :: NullOrUndefined (Number), "Text" :: NullOrUndefined (String), "BeginOffset" :: NullOrUndefined (Int), "EndOffset" :: NullOrUndefined (Int) }
```

<p>Describes a key noun phrase.</p>

#### `LanguageCode`

``` purescript
newtype LanguageCode
  = LanguageCode String
```

#### `ListOfDetectDominantLanguageResult`

``` purescript
newtype ListOfDetectDominantLanguageResult
  = ListOfDetectDominantLanguageResult (Array BatchDetectDominantLanguageItemResult)
```

#### `ListOfDetectEntitiesResult`

``` purescript
newtype ListOfDetectEntitiesResult
  = ListOfDetectEntitiesResult (Array BatchDetectEntitiesItemResult)
```

#### `ListOfDetectKeyPhrasesResult`

``` purescript
newtype ListOfDetectKeyPhrasesResult
  = ListOfDetectKeyPhrasesResult (Array BatchDetectKeyPhrasesItemResult)
```

#### `ListOfDetectSentimentResult`

``` purescript
newtype ListOfDetectSentimentResult
  = ListOfDetectSentimentResult (Array BatchDetectSentimentItemResult)
```

#### `ListOfDominantLanguages`

``` purescript
newtype ListOfDominantLanguages
  = ListOfDominantLanguages (Array DominantLanguage)
```

#### `ListOfEntities`

``` purescript
newtype ListOfEntities
  = ListOfEntities (Array Entity)
```

#### `ListOfKeyPhrases`

``` purescript
newtype ListOfKeyPhrases
  = ListOfKeyPhrases (Array KeyPhrase)
```

#### `ListTopicsDetectionJobsRequest`

``` purescript
newtype ListTopicsDetectionJobsRequest
  = ListTopicsDetectionJobsRequest { "Filter" :: NullOrUndefined (TopicsDetectionJobFilter), "NextToken" :: NullOrUndefined (String), "MaxResults" :: NullOrUndefined (MaxResultsInteger) }
```

#### `ListTopicsDetectionJobsResponse`

``` purescript
newtype ListTopicsDetectionJobsResponse
  = ListTopicsDetectionJobsResponse { "TopicsDetectionJobPropertiesList" :: NullOrUndefined (TopicsDetectionJobPropertiesList), "NextToken" :: NullOrUndefined (String) }
```

#### `MaxResultsInteger`

``` purescript
newtype MaxResultsInteger
  = MaxResultsInteger Int
```

#### `NumberOfTopicsInteger`

``` purescript
newtype NumberOfTopicsInteger
  = NumberOfTopicsInteger Int
```

#### `OutputDataConfig`

``` purescript
newtype OutputDataConfig
  = OutputDataConfig { "S3Uri" :: S3Uri }
```

<p>Provides configuration parameters for the output of topic detection jobs.</p> <p/>

#### `S3Uri`

``` purescript
newtype S3Uri
  = S3Uri String
```

#### `SentimentScore`

``` purescript
newtype SentimentScore
  = SentimentScore { "Positive" :: NullOrUndefined (Number), "Negative" :: NullOrUndefined (Number), "Neutral" :: NullOrUndefined (Number), "Mixed" :: NullOrUndefined (Number) }
```

<p>Describes the level of confidence that Amazon Comprehend has in the accuracy of its detection of sentiments.</p>

#### `SentimentType`

``` purescript
newtype SentimentType
  = SentimentType String
```

#### `StartTopicsDetectionJobRequest`

``` purescript
newtype StartTopicsDetectionJobRequest
  = StartTopicsDetectionJobRequest { "InputDataConfig" :: InputDataConfig, "OutputDataConfig" :: OutputDataConfig, "DataAccessRoleArn" :: IamRoleArn, "JobName" :: NullOrUndefined (JobName), "NumberOfTopics" :: NullOrUndefined (NumberOfTopicsInteger), "ClientRequestToken" :: NullOrUndefined (ClientRequestTokenString) }
```

#### `StartTopicsDetectionJobResponse`

``` purescript
newtype StartTopicsDetectionJobResponse
  = StartTopicsDetectionJobResponse { "JobId" :: NullOrUndefined (JobId), "JobStatus" :: NullOrUndefined (JobStatus) }
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

#### `TextSizeLimitExceededException`

``` purescript
newtype TextSizeLimitExceededException
  = TextSizeLimitExceededException { "Message" :: NullOrUndefined (String) }
```

<p>The size of the input text exceeds the limit. Use a smaller document.</p>

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "Message" :: NullOrUndefined (String) }
```

<p>The number of requests exceeds the limit. Resubmit your request later.</p>

#### `TopicsDetectionJobFilter`

``` purescript
newtype TopicsDetectionJobFilter
  = TopicsDetectionJobFilter { "JobName" :: NullOrUndefined (JobName), "JobStatus" :: NullOrUndefined (JobStatus), "SubmitTimeBefore" :: NullOrUndefined (Number), "SubmitTimeAfter" :: NullOrUndefined (Number) }
```

<p>Provides information for filtering topic detection jobs. For more information, see .</p>

#### `TopicsDetectionJobProperties`

``` purescript
newtype TopicsDetectionJobProperties
  = TopicsDetectionJobProperties { "JobId" :: NullOrUndefined (JobId), "JobName" :: NullOrUndefined (JobName), "JobStatus" :: NullOrUndefined (JobStatus), "Message" :: NullOrUndefined (AnyLengthString), "SubmitTime" :: NullOrUndefined (Number), "EndTime" :: NullOrUndefined (Number), "InputDataConfig" :: NullOrUndefined (InputDataConfig), "OutputDataConfig" :: NullOrUndefined (OutputDataConfig), "NumberOfTopics" :: NullOrUndefined (Int) }
```

<p>Provides information about a topic detection job.</p>

#### `TopicsDetectionJobPropertiesList`

``` purescript
newtype TopicsDetectionJobPropertiesList
  = TopicsDetectionJobPropertiesList (Array TopicsDetectionJobProperties)
```

#### `UnsupportedLanguageException`

``` purescript
newtype UnsupportedLanguageException
  = UnsupportedLanguageException { "Message" :: NullOrUndefined (String) }
```

<p>Amazon Comprehend can't process the language of the input text. For all APIs except <code>DetectDominantLanguage</code>, Amazon Comprehend accepts only English or Spanish text. For the <code>DetectDominantLanguage</code> API, Amazon Comprehend detects 100 languages. For a list of languages, see <a>how-languages</a> </p>


