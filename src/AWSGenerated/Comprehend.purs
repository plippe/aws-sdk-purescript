

-- | <p>Amazon Comprehend is an AWS service for gaining insight into the content of documents. Use these actions to determine the topics contained in your documents, the topics they discuss, the predominant sentiment expressed in them, the predominant language used, and more.</p>
module AWS.Comprehend where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Comprehend" :: String


-- | <p>Determines the dominant language of the input text for a batch of documents. For a list of languages that Amazon Comprehend can detect, see <a href="http://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html">Amazon Comprehend Supported Languages</a>. </p>
batchDetectDominantLanguage :: forall eff. BatchDetectDominantLanguageRequest -> Aff (err :: AWS.RequestError | eff) BatchDetectDominantLanguageResponse
batchDetectDominantLanguage = AWS.request serviceName "batchDetectDominantLanguage" 


-- | <p>Inspects the text of a batch of documents and returns information about them. For more information about entities, see <a>how-entities</a> </p>
batchDetectEntities :: forall eff. BatchDetectEntitiesRequest -> Aff (err :: AWS.RequestError | eff) BatchDetectEntitiesResponse
batchDetectEntities = AWS.request serviceName "batchDetectEntities" 


-- | <p>Detects the key noun phrases found in a batch of documents.</p>
batchDetectKeyPhrases :: forall eff. BatchDetectKeyPhrasesRequest -> Aff (err :: AWS.RequestError | eff) BatchDetectKeyPhrasesResponse
batchDetectKeyPhrases = AWS.request serviceName "batchDetectKeyPhrases" 


-- | <p>Inspects a batch of documents and returns an inference of the prevailing sentiment, <code>POSITIVE</code>, <code>NEUTRAL</code>, <code>MIXED</code>, or <code>NEGATIVE</code>, in each one.</p>
batchDetectSentiment :: forall eff. BatchDetectSentimentRequest -> Aff (err :: AWS.RequestError | eff) BatchDetectSentimentResponse
batchDetectSentiment = AWS.request serviceName "batchDetectSentiment" 


-- | <p>Gets the properties associated with a topic detection job. Use this operation to get the status of a detection job.</p>
describeTopicsDetectionJob :: forall eff. DescribeTopicsDetectionJobRequest -> Aff (err :: AWS.RequestError | eff) DescribeTopicsDetectionJobResponse
describeTopicsDetectionJob = AWS.request serviceName "describeTopicsDetectionJob" 


-- | <p>Determines the dominant language of the input text. For a list of languages that Amazon Comprehend can detect, see <a href="http://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html">Amazon Comprehend Supported Languages</a>. </p>
detectDominantLanguage :: forall eff. DetectDominantLanguageRequest -> Aff (err :: AWS.RequestError | eff) DetectDominantLanguageResponse
detectDominantLanguage = AWS.request serviceName "detectDominantLanguage" 


-- | <p>Inspects text for entities, and returns information about them. For more information, about entities, see <a>how-entities</a>. </p>
detectEntities :: forall eff. DetectEntitiesRequest -> Aff (err :: AWS.RequestError | eff) DetectEntitiesResponse
detectEntities = AWS.request serviceName "detectEntities" 


-- | <p>Detects the key noun phrases found in the text. </p>
detectKeyPhrases :: forall eff. DetectKeyPhrasesRequest -> Aff (err :: AWS.RequestError | eff) DetectKeyPhrasesResponse
detectKeyPhrases = AWS.request serviceName "detectKeyPhrases" 


-- | <p>Inspects text and returns an inference of the prevailing sentiment (<code>POSITIVE</code>, <code>NEUTRAL</code>, <code>MIXED</code>, or <code>NEGATIVE</code>). </p>
detectSentiment :: forall eff. DetectSentimentRequest -> Aff (err :: AWS.RequestError | eff) DetectSentimentResponse
detectSentiment = AWS.request serviceName "detectSentiment" 


-- | <p>Gets a list of the topic detection jobs that you have submitted.</p>
listTopicsDetectionJobs :: forall eff. ListTopicsDetectionJobsRequest -> Aff (err :: AWS.RequestError | eff) ListTopicsDetectionJobsResponse
listTopicsDetectionJobs = AWS.request serviceName "listTopicsDetectionJobs" 


-- | <p>Starts an asynchronous topic detection job. Use the <code>DescribeTopicDetectionJob</code> operation to track the status of a job.</p>
startTopicsDetectionJob :: forall eff. StartTopicsDetectionJobRequest -> Aff (err :: AWS.RequestError | eff) StartTopicsDetectionJobResponse
startTopicsDetectionJob = AWS.request serviceName "startTopicsDetectionJob" 


newtype AnyLengthString = AnyLengthString String
derive instance newtypeAnyLengthString :: Newtype AnyLengthString _


-- | <p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>
newtype BatchDetectDominantLanguageItemResult = BatchDetectDominantLanguageItemResult 
  { "Index" :: NullOrUndefined (Int)
  , "Languages" :: NullOrUndefined (ListOfDominantLanguages)
  }
derive instance newtypeBatchDetectDominantLanguageItemResult :: Newtype BatchDetectDominantLanguageItemResult _


newtype BatchDetectDominantLanguageRequest = BatchDetectDominantLanguageRequest 
  { "TextList" :: (StringList)
  }
derive instance newtypeBatchDetectDominantLanguageRequest :: Newtype BatchDetectDominantLanguageRequest _


newtype BatchDetectDominantLanguageResponse = BatchDetectDominantLanguageResponse 
  { "ResultList" :: (ListOfDetectDominantLanguageResult)
  , "ErrorList" :: (BatchItemErrorList)
  }
derive instance newtypeBatchDetectDominantLanguageResponse :: Newtype BatchDetectDominantLanguageResponse _


-- | <p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>
newtype BatchDetectEntitiesItemResult = BatchDetectEntitiesItemResult 
  { "Index" :: NullOrUndefined (Int)
  , "Entities" :: NullOrUndefined (ListOfEntities)
  }
derive instance newtypeBatchDetectEntitiesItemResult :: Newtype BatchDetectEntitiesItemResult _


newtype BatchDetectEntitiesRequest = BatchDetectEntitiesRequest 
  { "TextList" :: (StringList)
  , "LanguageCode" :: (String)
  }
derive instance newtypeBatchDetectEntitiesRequest :: Newtype BatchDetectEntitiesRequest _


newtype BatchDetectEntitiesResponse = BatchDetectEntitiesResponse 
  { "ResultList" :: (ListOfDetectEntitiesResult)
  , "ErrorList" :: (BatchItemErrorList)
  }
derive instance newtypeBatchDetectEntitiesResponse :: Newtype BatchDetectEntitiesResponse _


-- | <p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>
newtype BatchDetectKeyPhrasesItemResult = BatchDetectKeyPhrasesItemResult 
  { "Index" :: NullOrUndefined (Int)
  , "KeyPhrases" :: NullOrUndefined (ListOfKeyPhrases)
  }
derive instance newtypeBatchDetectKeyPhrasesItemResult :: Newtype BatchDetectKeyPhrasesItemResult _


newtype BatchDetectKeyPhrasesRequest = BatchDetectKeyPhrasesRequest 
  { "TextList" :: (StringList)
  , "LanguageCode" :: (String)
  }
derive instance newtypeBatchDetectKeyPhrasesRequest :: Newtype BatchDetectKeyPhrasesRequest _


newtype BatchDetectKeyPhrasesResponse = BatchDetectKeyPhrasesResponse 
  { "ResultList" :: (ListOfDetectKeyPhrasesResult)
  , "ErrorList" :: (BatchItemErrorList)
  }
derive instance newtypeBatchDetectKeyPhrasesResponse :: Newtype BatchDetectKeyPhrasesResponse _


-- | <p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>
newtype BatchDetectSentimentItemResult = BatchDetectSentimentItemResult 
  { "Index" :: NullOrUndefined (Int)
  , "Sentiment" :: NullOrUndefined (SentimentType)
  , "SentimentScore" :: NullOrUndefined (SentimentScore)
  }
derive instance newtypeBatchDetectSentimentItemResult :: Newtype BatchDetectSentimentItemResult _


newtype BatchDetectSentimentRequest = BatchDetectSentimentRequest 
  { "TextList" :: (StringList)
  , "LanguageCode" :: (String)
  }
derive instance newtypeBatchDetectSentimentRequest :: Newtype BatchDetectSentimentRequest _


newtype BatchDetectSentimentResponse = BatchDetectSentimentResponse 
  { "ResultList" :: (ListOfDetectSentimentResult)
  , "ErrorList" :: (BatchItemErrorList)
  }
derive instance newtypeBatchDetectSentimentResponse :: Newtype BatchDetectSentimentResponse _


-- | <p>Describes an error that occurred while processing a document in a batch. The operation returns on <code>BatchItemError</code> object for each document that contained an error.</p>
newtype BatchItemError = BatchItemError 
  { "Index" :: NullOrUndefined (Int)
  , "ErrorCode" :: NullOrUndefined (String)
  , "ErrorMessage" :: NullOrUndefined (String)
  }
derive instance newtypeBatchItemError :: Newtype BatchItemError _


newtype BatchItemErrorList = BatchItemErrorList (Array BatchItemError)
derive instance newtypeBatchItemErrorList :: Newtype BatchItemErrorList _


-- | <p>The number of documents in the request exceeds the limit of 25. Try your request again with fewer documents.</p>
newtype BatchSizeLimitExceededException = BatchSizeLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeBatchSizeLimitExceededException :: Newtype BatchSizeLimitExceededException _


newtype ClientRequestTokenString = ClientRequestTokenString String
derive instance newtypeClientRequestTokenString :: Newtype ClientRequestTokenString _


newtype DescribeTopicsDetectionJobRequest = DescribeTopicsDetectionJobRequest 
  { "JobId" :: (JobId)
  }
derive instance newtypeDescribeTopicsDetectionJobRequest :: Newtype DescribeTopicsDetectionJobRequest _


newtype DescribeTopicsDetectionJobResponse = DescribeTopicsDetectionJobResponse 
  { "TopicsDetectionJobProperties" :: NullOrUndefined (TopicsDetectionJobProperties)
  }
derive instance newtypeDescribeTopicsDetectionJobResponse :: Newtype DescribeTopicsDetectionJobResponse _


newtype DetectDominantLanguageRequest = DetectDominantLanguageRequest 
  { "Text" :: (String)
  }
derive instance newtypeDetectDominantLanguageRequest :: Newtype DetectDominantLanguageRequest _


newtype DetectDominantLanguageResponse = DetectDominantLanguageResponse 
  { "Languages" :: NullOrUndefined (ListOfDominantLanguages)
  }
derive instance newtypeDetectDominantLanguageResponse :: Newtype DetectDominantLanguageResponse _


newtype DetectEntitiesRequest = DetectEntitiesRequest 
  { "Text" :: (String)
  , "LanguageCode" :: (LanguageCode)
  }
derive instance newtypeDetectEntitiesRequest :: Newtype DetectEntitiesRequest _


newtype DetectEntitiesResponse = DetectEntitiesResponse 
  { "Entities" :: NullOrUndefined (ListOfEntities)
  }
derive instance newtypeDetectEntitiesResponse :: Newtype DetectEntitiesResponse _


newtype DetectKeyPhrasesRequest = DetectKeyPhrasesRequest 
  { "Text" :: (String)
  , "LanguageCode" :: (LanguageCode)
  }
derive instance newtypeDetectKeyPhrasesRequest :: Newtype DetectKeyPhrasesRequest _


newtype DetectKeyPhrasesResponse = DetectKeyPhrasesResponse 
  { "KeyPhrases" :: NullOrUndefined (ListOfKeyPhrases)
  }
derive instance newtypeDetectKeyPhrasesResponse :: Newtype DetectKeyPhrasesResponse _


newtype DetectSentimentRequest = DetectSentimentRequest 
  { "Text" :: (String)
  , "LanguageCode" :: (LanguageCode)
  }
derive instance newtypeDetectSentimentRequest :: Newtype DetectSentimentRequest _


newtype DetectSentimentResponse = DetectSentimentResponse 
  { "Sentiment" :: NullOrUndefined (SentimentType)
  , "SentimentScore" :: NullOrUndefined (SentimentScore)
  }
derive instance newtypeDetectSentimentResponse :: Newtype DetectSentimentResponse _


-- | <p>Returns the code for the dominant language in the input text and the level of confidence that Amazon Comprehend has in the accuracy of the detection.</p>
newtype DominantLanguage = DominantLanguage 
  { "LanguageCode" :: NullOrUndefined (String)
  , "Score" :: NullOrUndefined (Number)
  }
derive instance newtypeDominantLanguage :: Newtype DominantLanguage _


-- | <p>Provides information about an entity. </p> <p> </p>
newtype Entity = Entity 
  { "Score" :: NullOrUndefined (Number)
  , "Type" :: NullOrUndefined (EntityType)
  , "Text" :: NullOrUndefined (String)
  , "BeginOffset" :: NullOrUndefined (Int)
  , "EndOffset" :: NullOrUndefined (Int)
  }
derive instance newtypeEntity :: Newtype Entity _


newtype EntityType = EntityType String
derive instance newtypeEntityType :: Newtype EntityType _


newtype IamRoleArn = IamRoleArn String
derive instance newtypeIamRoleArn :: Newtype IamRoleArn _


-- | <p>The input properties for a topic detection job.</p>
newtype InputDataConfig = InputDataConfig 
  { "S3Uri" :: (S3Uri)
  , "InputFormat" :: NullOrUndefined (InputFormat)
  }
derive instance newtypeInputDataConfig :: Newtype InputDataConfig _


newtype InputFormat = InputFormat String
derive instance newtypeInputFormat :: Newtype InputFormat _


-- | <p>An internal server error occurred. Retry your request.</p>
newtype InternalServerException = InternalServerException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInternalServerException :: Newtype InternalServerException _


-- | <p>The filter specified for the <code>ListTopicDetectionJobs</code> operation is invalid. Specify a different filter.</p>
newtype InvalidFilterException = InvalidFilterException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidFilterException :: Newtype InvalidFilterException _


-- | <p>The request is invalid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _


newtype JobName = JobName String
derive instance newtypeJobName :: Newtype JobName _


-- | <p>The specified job was not found. Check the job ID and try again.</p>
newtype JobNotFoundException = JobNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeJobNotFoundException :: Newtype JobNotFoundException _


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _


-- | <p>Describes a key noun phrase.</p>
newtype KeyPhrase = KeyPhrase 
  { "Score" :: NullOrUndefined (Number)
  , "Text" :: NullOrUndefined (String)
  , "BeginOffset" :: NullOrUndefined (Int)
  , "EndOffset" :: NullOrUndefined (Int)
  }
derive instance newtypeKeyPhrase :: Newtype KeyPhrase _


newtype LanguageCode = LanguageCode String
derive instance newtypeLanguageCode :: Newtype LanguageCode _


newtype ListOfDetectDominantLanguageResult = ListOfDetectDominantLanguageResult (Array BatchDetectDominantLanguageItemResult)
derive instance newtypeListOfDetectDominantLanguageResult :: Newtype ListOfDetectDominantLanguageResult _


newtype ListOfDetectEntitiesResult = ListOfDetectEntitiesResult (Array BatchDetectEntitiesItemResult)
derive instance newtypeListOfDetectEntitiesResult :: Newtype ListOfDetectEntitiesResult _


newtype ListOfDetectKeyPhrasesResult = ListOfDetectKeyPhrasesResult (Array BatchDetectKeyPhrasesItemResult)
derive instance newtypeListOfDetectKeyPhrasesResult :: Newtype ListOfDetectKeyPhrasesResult _


newtype ListOfDetectSentimentResult = ListOfDetectSentimentResult (Array BatchDetectSentimentItemResult)
derive instance newtypeListOfDetectSentimentResult :: Newtype ListOfDetectSentimentResult _


newtype ListOfDominantLanguages = ListOfDominantLanguages (Array DominantLanguage)
derive instance newtypeListOfDominantLanguages :: Newtype ListOfDominantLanguages _


newtype ListOfEntities = ListOfEntities (Array Entity)
derive instance newtypeListOfEntities :: Newtype ListOfEntities _


newtype ListOfKeyPhrases = ListOfKeyPhrases (Array KeyPhrase)
derive instance newtypeListOfKeyPhrases :: Newtype ListOfKeyPhrases _


newtype ListTopicsDetectionJobsRequest = ListTopicsDetectionJobsRequest 
  { "Filter" :: NullOrUndefined (TopicsDetectionJobFilter)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (MaxResultsInteger)
  }
derive instance newtypeListTopicsDetectionJobsRequest :: Newtype ListTopicsDetectionJobsRequest _


newtype ListTopicsDetectionJobsResponse = ListTopicsDetectionJobsResponse 
  { "TopicsDetectionJobPropertiesList" :: NullOrUndefined (TopicsDetectionJobPropertiesList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListTopicsDetectionJobsResponse :: Newtype ListTopicsDetectionJobsResponse _


newtype MaxResultsInteger = MaxResultsInteger Int
derive instance newtypeMaxResultsInteger :: Newtype MaxResultsInteger _


newtype NumberOfTopicsInteger = NumberOfTopicsInteger Int
derive instance newtypeNumberOfTopicsInteger :: Newtype NumberOfTopicsInteger _


-- | <p>Provides configuration parameters for the output of topic detection jobs.</p> <p/>
newtype OutputDataConfig = OutputDataConfig 
  { "S3Uri" :: (S3Uri)
  }
derive instance newtypeOutputDataConfig :: Newtype OutputDataConfig _


newtype S3Uri = S3Uri String
derive instance newtypeS3Uri :: Newtype S3Uri _


-- | <p>Describes the level of confidence that Amazon Comprehend has in the accuracy of its detection of sentiments.</p>
newtype SentimentScore = SentimentScore 
  { "Positive" :: NullOrUndefined (Number)
  , "Negative" :: NullOrUndefined (Number)
  , "Neutral" :: NullOrUndefined (Number)
  , "Mixed" :: NullOrUndefined (Number)
  }
derive instance newtypeSentimentScore :: Newtype SentimentScore _


newtype SentimentType = SentimentType String
derive instance newtypeSentimentType :: Newtype SentimentType _


newtype StartTopicsDetectionJobRequest = StartTopicsDetectionJobRequest 
  { "InputDataConfig" :: (InputDataConfig)
  , "OutputDataConfig" :: (OutputDataConfig)
  , "DataAccessRoleArn" :: (IamRoleArn)
  , "JobName" :: NullOrUndefined (JobName)
  , "NumberOfTopics" :: NullOrUndefined (NumberOfTopicsInteger)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestTokenString)
  }
derive instance newtypeStartTopicsDetectionJobRequest :: Newtype StartTopicsDetectionJobRequest _


newtype StartTopicsDetectionJobResponse = StartTopicsDetectionJobResponse 
  { "JobId" :: NullOrUndefined (JobId)
  , "JobStatus" :: NullOrUndefined (JobStatus)
  }
derive instance newtypeStartTopicsDetectionJobResponse :: Newtype StartTopicsDetectionJobResponse _


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _


-- | <p>The size of the input text exceeds the limit. Use a smaller document.</p>
newtype TextSizeLimitExceededException = TextSizeLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeTextSizeLimitExceededException :: Newtype TextSizeLimitExceededException _


-- | <p>The number of requests exceeds the limit. Resubmit your request later.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


-- | <p>Provides information for filtering topic detection jobs. For more information, see .</p>
newtype TopicsDetectionJobFilter = TopicsDetectionJobFilter 
  { "JobName" :: NullOrUndefined (JobName)
  , "JobStatus" :: NullOrUndefined (JobStatus)
  , "SubmitTimeBefore" :: NullOrUndefined (Number)
  , "SubmitTimeAfter" :: NullOrUndefined (Number)
  }
derive instance newtypeTopicsDetectionJobFilter :: Newtype TopicsDetectionJobFilter _


-- | <p>Provides information about a topic detection job.</p>
newtype TopicsDetectionJobProperties = TopicsDetectionJobProperties 
  { "JobId" :: NullOrUndefined (JobId)
  , "JobName" :: NullOrUndefined (JobName)
  , "JobStatus" :: NullOrUndefined (JobStatus)
  , "Message" :: NullOrUndefined (AnyLengthString)
  , "SubmitTime" :: NullOrUndefined (Number)
  , "EndTime" :: NullOrUndefined (Number)
  , "InputDataConfig" :: NullOrUndefined (InputDataConfig)
  , "OutputDataConfig" :: NullOrUndefined (OutputDataConfig)
  , "NumberOfTopics" :: NullOrUndefined (Int)
  }
derive instance newtypeTopicsDetectionJobProperties :: Newtype TopicsDetectionJobProperties _


newtype TopicsDetectionJobPropertiesList = TopicsDetectionJobPropertiesList (Array TopicsDetectionJobProperties)
derive instance newtypeTopicsDetectionJobPropertiesList :: Newtype TopicsDetectionJobPropertiesList _


-- | <p>Amazon Comprehend can't process the language of the input text. For all APIs except <code>DetectDominantLanguage</code>, Amazon Comprehend accepts only English or Spanish text. For the <code>DetectDominantLanguage</code> API, Amazon Comprehend detects 100 languages. For a list of languages, see <a>how-languages</a> </p>
newtype UnsupportedLanguageException = UnsupportedLanguageException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeUnsupportedLanguageException :: Newtype UnsupportedLanguageException _
