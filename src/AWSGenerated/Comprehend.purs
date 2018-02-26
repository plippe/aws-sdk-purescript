

-- | <p>Amazon Comprehend is an AWS service for gaining insight into the content of documents. Use these actions to determine the topics contained in your documents, the topics they discuss, the predominant sentiment expressed in them, the predominant language used, and more.</p>
module AWS.Comprehend where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Comprehend" :: String


-- | <p>Determines the dominant language of the input text for a batch of documents. For a list of languages that Amazon Comprehend can detect, see <a href="http://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html">Amazon Comprehend Supported Languages</a>. </p>
batchDetectDominantLanguage :: forall eff. BatchDetectDominantLanguageRequest -> Aff (err :: AWS.RequestError | eff) BatchDetectDominantLanguageResponse
batchDetectDominantLanguage = AWS.request serviceName "BatchDetectDominantLanguage" 


-- | <p>Inspects the text of a batch of documents and returns information about them. For more information about entities, see <a>how-entities</a> </p>
batchDetectEntities :: forall eff. BatchDetectEntitiesRequest -> Aff (err :: AWS.RequestError | eff) BatchDetectEntitiesResponse
batchDetectEntities = AWS.request serviceName "BatchDetectEntities" 


-- | <p>Detects the key noun phrases found in a batch of documents.</p>
batchDetectKeyPhrases :: forall eff. BatchDetectKeyPhrasesRequest -> Aff (err :: AWS.RequestError | eff) BatchDetectKeyPhrasesResponse
batchDetectKeyPhrases = AWS.request serviceName "BatchDetectKeyPhrases" 


-- | <p>Inspects a batch of documents and returns an inference of the prevailing sentiment, <code>POSITIVE</code>, <code>NEUTRAL</code>, <code>MIXED</code>, or <code>NEGATIVE</code>, in each one.</p>
batchDetectSentiment :: forall eff. BatchDetectSentimentRequest -> Aff (err :: AWS.RequestError | eff) BatchDetectSentimentResponse
batchDetectSentiment = AWS.request serviceName "BatchDetectSentiment" 


-- | <p>Gets the properties associated with a topic detection job. Use this operation to get the status of a detection job.</p>
describeTopicsDetectionJob :: forall eff. DescribeTopicsDetectionJobRequest -> Aff (err :: AWS.RequestError | eff) DescribeTopicsDetectionJobResponse
describeTopicsDetectionJob = AWS.request serviceName "DescribeTopicsDetectionJob" 


-- | <p>Determines the dominant language of the input text. For a list of languages that Amazon Comprehend can detect, see <a href="http://docs.aws.amazon.com/comprehend/latest/dg/how-languages.html">Amazon Comprehend Supported Languages</a>. </p>
detectDominantLanguage :: forall eff. DetectDominantLanguageRequest -> Aff (err :: AWS.RequestError | eff) DetectDominantLanguageResponse
detectDominantLanguage = AWS.request serviceName "DetectDominantLanguage" 


-- | <p>Inspects text for entities, and returns information about them. For more information, about entities, see <a>how-entities</a>. </p>
detectEntities :: forall eff. DetectEntitiesRequest -> Aff (err :: AWS.RequestError | eff) DetectEntitiesResponse
detectEntities = AWS.request serviceName "DetectEntities" 


-- | <p>Detects the key noun phrases found in the text. </p>
detectKeyPhrases :: forall eff. DetectKeyPhrasesRequest -> Aff (err :: AWS.RequestError | eff) DetectKeyPhrasesResponse
detectKeyPhrases = AWS.request serviceName "DetectKeyPhrases" 


-- | <p>Inspects text and returns an inference of the prevailing sentiment (<code>POSITIVE</code>, <code>NEUTRAL</code>, <code>MIXED</code>, or <code>NEGATIVE</code>). </p>
detectSentiment :: forall eff. DetectSentimentRequest -> Aff (err :: AWS.RequestError | eff) DetectSentimentResponse
detectSentiment = AWS.request serviceName "DetectSentiment" 


-- | <p>Gets a list of the topic detection jobs that you have submitted.</p>
listTopicsDetectionJobs :: forall eff. ListTopicsDetectionJobsRequest -> Aff (err :: AWS.RequestError | eff) ListTopicsDetectionJobsResponse
listTopicsDetectionJobs = AWS.request serviceName "ListTopicsDetectionJobs" 


-- | <p>Starts an asynchronous topic detection job. Use the <code>DescribeTopicDetectionJob</code> operation to track the status of a job.</p>
startTopicsDetectionJob :: forall eff. StartTopicsDetectionJobRequest -> Aff (err :: AWS.RequestError | eff) StartTopicsDetectionJobResponse
startTopicsDetectionJob = AWS.request serviceName "StartTopicsDetectionJob" 


newtype AnyLengthString = AnyLengthString String


-- | <p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>
newtype BatchDetectDominantLanguageItemResult = BatchDetectDominantLanguageItemResult 
  { "Index" :: NullOrUndefined (Int)
  , "Languages" :: NullOrUndefined (ListOfDominantLanguages)
  }


newtype BatchDetectDominantLanguageRequest = BatchDetectDominantLanguageRequest 
  { "TextList" :: (StringList)
  }


newtype BatchDetectDominantLanguageResponse = BatchDetectDominantLanguageResponse 
  { "ResultList" :: (ListOfDetectDominantLanguageResult)
  , "ErrorList" :: (BatchItemErrorList)
  }


-- | <p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>
newtype BatchDetectEntitiesItemResult = BatchDetectEntitiesItemResult 
  { "Index" :: NullOrUndefined (Int)
  , "Entities" :: NullOrUndefined (ListOfEntities)
  }


newtype BatchDetectEntitiesRequest = BatchDetectEntitiesRequest 
  { "TextList" :: (StringList)
  , "LanguageCode" :: (String)
  }


newtype BatchDetectEntitiesResponse = BatchDetectEntitiesResponse 
  { "ResultList" :: (ListOfDetectEntitiesResult)
  , "ErrorList" :: (BatchItemErrorList)
  }


-- | <p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>
newtype BatchDetectKeyPhrasesItemResult = BatchDetectKeyPhrasesItemResult 
  { "Index" :: NullOrUndefined (Int)
  , "KeyPhrases" :: NullOrUndefined (ListOfKeyPhrases)
  }


newtype BatchDetectKeyPhrasesRequest = BatchDetectKeyPhrasesRequest 
  { "TextList" :: (StringList)
  , "LanguageCode" :: (String)
  }


newtype BatchDetectKeyPhrasesResponse = BatchDetectKeyPhrasesResponse 
  { "ResultList" :: (ListOfDetectKeyPhrasesResult)
  , "ErrorList" :: (BatchItemErrorList)
  }


-- | <p>The result of calling the operation. The operation returns one object for each document that is successfully processed by the operation.</p>
newtype BatchDetectSentimentItemResult = BatchDetectSentimentItemResult 
  { "Index" :: NullOrUndefined (Int)
  , "Sentiment" :: NullOrUndefined (SentimentType)
  , "SentimentScore" :: NullOrUndefined (SentimentScore)
  }


newtype BatchDetectSentimentRequest = BatchDetectSentimentRequest 
  { "TextList" :: (StringList)
  , "LanguageCode" :: (String)
  }


newtype BatchDetectSentimentResponse = BatchDetectSentimentResponse 
  { "ResultList" :: (ListOfDetectSentimentResult)
  , "ErrorList" :: (BatchItemErrorList)
  }


-- | <p>Describes an error that occurred while processing a document in a batch. The operation returns on <code>BatchItemError</code> object for each document that contained an error.</p>
newtype BatchItemError = BatchItemError 
  { "Index" :: NullOrUndefined (Int)
  , "ErrorCode" :: NullOrUndefined (String)
  , "ErrorMessage" :: NullOrUndefined (String)
  }


newtype BatchItemErrorList = BatchItemErrorList (Array BatchItemError)


-- | <p>The number of documents in the request exceeds the limit of 25. Try your request again with fewer documents.</p>
newtype BatchSizeLimitExceededException = BatchSizeLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


newtype ClientRequestTokenString = ClientRequestTokenString String


newtype DescribeTopicsDetectionJobRequest = DescribeTopicsDetectionJobRequest 
  { "JobId" :: (JobId)
  }


newtype DescribeTopicsDetectionJobResponse = DescribeTopicsDetectionJobResponse 
  { "TopicsDetectionJobProperties" :: NullOrUndefined (TopicsDetectionJobProperties)
  }


newtype DetectDominantLanguageRequest = DetectDominantLanguageRequest 
  { "Text" :: (String)
  }


newtype DetectDominantLanguageResponse = DetectDominantLanguageResponse 
  { "Languages" :: NullOrUndefined (ListOfDominantLanguages)
  }


newtype DetectEntitiesRequest = DetectEntitiesRequest 
  { "Text" :: (String)
  , "LanguageCode" :: (LanguageCode)
  }


newtype DetectEntitiesResponse = DetectEntitiesResponse 
  { "Entities" :: NullOrUndefined (ListOfEntities)
  }


newtype DetectKeyPhrasesRequest = DetectKeyPhrasesRequest 
  { "Text" :: (String)
  , "LanguageCode" :: (LanguageCode)
  }


newtype DetectKeyPhrasesResponse = DetectKeyPhrasesResponse 
  { "KeyPhrases" :: NullOrUndefined (ListOfKeyPhrases)
  }


newtype DetectSentimentRequest = DetectSentimentRequest 
  { "Text" :: (String)
  , "LanguageCode" :: (LanguageCode)
  }


newtype DetectSentimentResponse = DetectSentimentResponse 
  { "Sentiment" :: NullOrUndefined (SentimentType)
  , "SentimentScore" :: NullOrUndefined (SentimentScore)
  }


-- | <p>Returns the code for the dominant language in the input text and the level of confidence that Amazon Comprehend has in the accuracy of the detection.</p>
newtype DominantLanguage = DominantLanguage 
  { "LanguageCode" :: NullOrUndefined (String)
  , "Score" :: NullOrUndefined (Number)
  }


-- | <p>Provides information about an entity. </p> <p> </p>
newtype Entity = Entity 
  { "Score" :: NullOrUndefined (Number)
  , "Type" :: NullOrUndefined (EntityType)
  , "Text" :: NullOrUndefined (String)
  , "BeginOffset" :: NullOrUndefined (Int)
  , "EndOffset" :: NullOrUndefined (Int)
  }


newtype EntityType = EntityType String


newtype IamRoleArn = IamRoleArn String


-- | <p>The input properties for a topic detection job.</p>
newtype InputDataConfig = InputDataConfig 
  { "S3Uri" :: (S3Uri)
  , "InputFormat" :: NullOrUndefined (InputFormat)
  }


newtype InputFormat = InputFormat String


-- | <p>An internal server error occurred. Retry your request.</p>
newtype InternalServerException = InternalServerException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The filter specified for the <code>ListTopicDetectionJobs</code> operation is invalid. Specify a different filter.</p>
newtype InvalidFilterException = InvalidFilterException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The request is invalid.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message" :: NullOrUndefined (String)
  }


newtype JobId = JobId String


newtype JobName = JobName String


-- | <p>The specified job was not found. Check the job ID and try again.</p>
newtype JobNotFoundException = JobNotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


newtype JobStatus = JobStatus String


-- | <p>Describes a key noun phrase.</p>
newtype KeyPhrase = KeyPhrase 
  { "Score" :: NullOrUndefined (Number)
  , "Text" :: NullOrUndefined (String)
  , "BeginOffset" :: NullOrUndefined (Int)
  , "EndOffset" :: NullOrUndefined (Int)
  }


newtype LanguageCode = LanguageCode String


newtype ListOfDetectDominantLanguageResult = ListOfDetectDominantLanguageResult (Array BatchDetectDominantLanguageItemResult)


newtype ListOfDetectEntitiesResult = ListOfDetectEntitiesResult (Array BatchDetectEntitiesItemResult)


newtype ListOfDetectKeyPhrasesResult = ListOfDetectKeyPhrasesResult (Array BatchDetectKeyPhrasesItemResult)


newtype ListOfDetectSentimentResult = ListOfDetectSentimentResult (Array BatchDetectSentimentItemResult)


newtype ListOfDominantLanguages = ListOfDominantLanguages (Array DominantLanguage)


newtype ListOfEntities = ListOfEntities (Array Entity)


newtype ListOfKeyPhrases = ListOfKeyPhrases (Array KeyPhrase)


newtype ListTopicsDetectionJobsRequest = ListTopicsDetectionJobsRequest 
  { "Filter" :: NullOrUndefined (TopicsDetectionJobFilter)
  , "NextToken" :: NullOrUndefined (String)
  , "MaxResults" :: NullOrUndefined (MaxResultsInteger)
  }


newtype ListTopicsDetectionJobsResponse = ListTopicsDetectionJobsResponse 
  { "TopicsDetectionJobPropertiesList" :: NullOrUndefined (TopicsDetectionJobPropertiesList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype MaxResultsInteger = MaxResultsInteger Int


newtype NumberOfTopicsInteger = NumberOfTopicsInteger Int


-- | <p>Provides configuration parameters for the output of topic detection jobs.</p> <p/>
newtype OutputDataConfig = OutputDataConfig 
  { "S3Uri" :: (S3Uri)
  }


newtype S3Uri = S3Uri String


-- | <p>Describes the level of confidence that Amazon Comprehend has in the accuracy of its detection of sentiments.</p>
newtype SentimentScore = SentimentScore 
  { "Positive" :: NullOrUndefined (Number)
  , "Negative" :: NullOrUndefined (Number)
  , "Neutral" :: NullOrUndefined (Number)
  , "Mixed" :: NullOrUndefined (Number)
  }


newtype SentimentType = SentimentType String


newtype StartTopicsDetectionJobRequest = StartTopicsDetectionJobRequest 
  { "InputDataConfig" :: (InputDataConfig)
  , "OutputDataConfig" :: (OutputDataConfig)
  , "DataAccessRoleArn" :: (IamRoleArn)
  , "JobName" :: NullOrUndefined (JobName)
  , "NumberOfTopics" :: NullOrUndefined (NumberOfTopicsInteger)
  , "ClientRequestToken" :: NullOrUndefined (ClientRequestTokenString)
  }


newtype StartTopicsDetectionJobResponse = StartTopicsDetectionJobResponse 
  { "JobId" :: NullOrUndefined (JobId)
  , "JobStatus" :: NullOrUndefined (JobStatus)
  }


newtype StringList = StringList (Array String)


-- | <p>The size of the input text exceeds the limit. Use a smaller document.</p>
newtype TextSizeLimitExceededException = TextSizeLimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>The number of requests exceeds the limit. Resubmit your request later.</p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "Message" :: NullOrUndefined (String)
  }


-- | <p>Provides information for filtering topic detection jobs. For more information, see .</p>
newtype TopicsDetectionJobFilter = TopicsDetectionJobFilter 
  { "JobName" :: NullOrUndefined (JobName)
  , "JobStatus" :: NullOrUndefined (JobStatus)
  , "SubmitTimeBefore" :: NullOrUndefined (Number)
  , "SubmitTimeAfter" :: NullOrUndefined (Number)
  }


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


newtype TopicsDetectionJobPropertiesList = TopicsDetectionJobPropertiesList (Array TopicsDetectionJobProperties)


-- | <p>Amazon Comprehend can't process the language of the input text. For all APIs except <code>DetectDominantLanguage</code>, Amazon Comprehend accepts only English or Spanish text. For the <code>DetectDominantLanguage</code> API, Amazon Comprehend detects 100 languages. For a list of languages, see <a>how-languages</a> </p>
newtype UnsupportedLanguageException = UnsupportedLanguageException 
  { "Message" :: NullOrUndefined (String)
  }
