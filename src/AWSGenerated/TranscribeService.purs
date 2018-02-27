

-- | <p>Operations and objects for transcribing speech to text.</p>
module AWS.TranscribeService where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "TranscribeService" :: String


-- | <p>Returns information about a transcription job. To see the status of the job, check the <code>Status</code> field. If the status is <code>COMPLETE</code>, the job is finished and you can find the results at the location specified in the <code>TranscriptionFileUri</code> field.</p>
getTranscriptionJob :: forall eff. GetTranscriptionJobRequest -> Aff (err :: AWS.RequestError | eff) GetTranscriptionJobResponse
getTranscriptionJob = AWS.request serviceName "getTranscriptionJob" 


-- | <p>Lists transcription jobs with the specified status.</p>
listTranscriptionJobs :: forall eff. ListTranscriptionJobsRequest -> Aff (err :: AWS.RequestError | eff) ListTranscriptionJobsResponse
listTranscriptionJobs = AWS.request serviceName "listTranscriptionJobs" 


-- | <p>Starts an asynchronous job to transcribe speech to text.</p>
startTranscriptionJob :: forall eff. StartTranscriptionJobRequest -> Aff (err :: AWS.RequestError | eff) StartTranscriptionJobResponse
startTranscriptionJob = AWS.request serviceName "startTranscriptionJob" 


-- | <p>There is a problem with one of the input fields. Check the S3 bucket name, make sure that the job name is not a duplicate, and confirm that you are using the correct file format. Then resend your request.</p>
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (FailureReason)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | <p>The <code>JobName</code> field is a duplicate of a previously entered job name. Resend your request with a different name.</p>
newtype ConflictException = ConflictException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeConflictException :: Newtype ConflictException _


newtype DateTime = DateTime Number
derive instance newtypeDateTime :: Newtype DateTime _


newtype FailureReason = FailureReason String
derive instance newtypeFailureReason :: Newtype FailureReason _


newtype GetTranscriptionJobRequest = GetTranscriptionJobRequest 
  { "TranscriptionJobName" :: (TranscriptionJobName)
  }
derive instance newtypeGetTranscriptionJobRequest :: Newtype GetTranscriptionJobRequest _


newtype GetTranscriptionJobResponse = GetTranscriptionJobResponse 
  { "TranscriptionJob" :: NullOrUndefined (TranscriptionJob)
  }
derive instance newtypeGetTranscriptionJobResponse :: Newtype GetTranscriptionJobResponse _


-- | <p>There was an internal error. Check the error message and try your request again.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _


newtype LanguageCode = LanguageCode String
derive instance newtypeLanguageCode :: Newtype LanguageCode _


-- | <p>Either you have sent too many requests or your input file is longer than 2 hours. Wait before you resend your request, or use a smaller file and resend the request.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListTranscriptionJobsRequest = ListTranscriptionJobsRequest 
  { "Status" :: (TranscriptionJobStatus)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }
derive instance newtypeListTranscriptionJobsRequest :: Newtype ListTranscriptionJobsRequest _


newtype ListTranscriptionJobsResponse = ListTranscriptionJobsResponse 
  { "Status" :: NullOrUndefined (TranscriptionJobStatus)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TranscriptionJobSummaries" :: NullOrUndefined (TranscriptionJobSummaries)
  }
derive instance newtypeListTranscriptionJobsResponse :: Newtype ListTranscriptionJobsResponse _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p>Describes the input media file in a transcription request.</p>
newtype Media = Media 
  { "MediaFileUri" :: NullOrUndefined (Uri)
  }
derive instance newtypeMedia :: Newtype Media _


newtype MediaFormat = MediaFormat String
derive instance newtypeMediaFormat :: Newtype MediaFormat _


newtype MediaSampleRateHertz = MediaSampleRateHertz Int
derive instance newtypeMediaSampleRateHertz :: Newtype MediaSampleRateHertz _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p>We can't find the requested job. Check the job name and try your request again.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


newtype StartTranscriptionJobRequest = StartTranscriptionJobRequest 
  { "TranscriptionJobName" :: (TranscriptionJobName)
  , "LanguageCode" :: (LanguageCode)
  , "MediaSampleRateHertz" :: NullOrUndefined (MediaSampleRateHertz)
  , "MediaFormat" :: (MediaFormat)
  , "Media" :: (Media)
  }
derive instance newtypeStartTranscriptionJobRequest :: Newtype StartTranscriptionJobRequest _


newtype StartTranscriptionJobResponse = StartTranscriptionJobResponse 
  { "TranscriptionJob" :: NullOrUndefined (TranscriptionJob)
  }
derive instance newtypeStartTranscriptionJobResponse :: Newtype StartTranscriptionJobResponse _


-- | <p>Describes the output of a transcription job.</p>
newtype Transcript = Transcript 
  { "TranscriptFileUri" :: NullOrUndefined (Uri)
  }
derive instance newtypeTranscript :: Newtype Transcript _


-- | <p>Describes an asynchronous transcription job that was created with the <code>StartTranscriptionJob</code> operation.</p>
newtype TranscriptionJob = TranscriptionJob 
  { "TranscriptionJobName" :: NullOrUndefined (TranscriptionJobName)
  , "TranscriptionJobStatus" :: NullOrUndefined (TranscriptionJobStatus)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "MediaSampleRateHertz" :: NullOrUndefined (MediaSampleRateHertz)
  , "MediaFormat" :: NullOrUndefined (MediaFormat)
  , "Media" :: NullOrUndefined (Media)
  , "Transcript" :: NullOrUndefined (Transcript)
  , "CreationTime" :: NullOrUndefined (DateTime)
  , "CompletionTime" :: NullOrUndefined (DateTime)
  , "FailureReason" :: NullOrUndefined (FailureReason)
  }
derive instance newtypeTranscriptionJob :: Newtype TranscriptionJob _


newtype TranscriptionJobName = TranscriptionJobName String
derive instance newtypeTranscriptionJobName :: Newtype TranscriptionJobName _


newtype TranscriptionJobStatus = TranscriptionJobStatus String
derive instance newtypeTranscriptionJobStatus :: Newtype TranscriptionJobStatus _


newtype TranscriptionJobSummaries = TranscriptionJobSummaries (Array TranscriptionJobSummary)
derive instance newtypeTranscriptionJobSummaries :: Newtype TranscriptionJobSummaries _


-- | <p>Provides a summary of information about a transcription job.</p>
newtype TranscriptionJobSummary = TranscriptionJobSummary 
  { "TranscriptionJobName" :: NullOrUndefined (TranscriptionJobName)
  , "CreationTime" :: NullOrUndefined (DateTime)
  , "CompletionTime" :: NullOrUndefined (DateTime)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "TranscriptionJobStatus" :: NullOrUndefined (TranscriptionJobStatus)
  , "FailureReason" :: NullOrUndefined (FailureReason)
  }
derive instance newtypeTranscriptionJobSummary :: Newtype TranscriptionJobSummary _


newtype Uri = Uri String
derive instance newtypeUri :: Newtype Uri _
