

-- | <p>Operations and objects for transcribing speech to text.</p>
module AWS.TranscribeService where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "TranscribeService" :: String


-- | <p>Returns information about a transcription job. To see the status of the job, check the <code>Status</code> field. If the status is <code>COMPLETE</code>, the job is finished and you can find the results at the location specified in the <code>TranscriptionFileUri</code> field.</p>
getTranscriptionJob :: forall eff. GetTranscriptionJobRequest -> Aff (err :: AWS.RequestError | eff) GetTranscriptionJobResponse
getTranscriptionJob = AWS.request serviceName "GetTranscriptionJob" 


-- | <p>Lists transcription jobs with the specified status.</p>
listTranscriptionJobs :: forall eff. ListTranscriptionJobsRequest -> Aff (err :: AWS.RequestError | eff) ListTranscriptionJobsResponse
listTranscriptionJobs = AWS.request serviceName "ListTranscriptionJobs" 


-- | <p>Starts an asynchronous job to transcribe speech to text.</p>
startTranscriptionJob :: forall eff. StartTranscriptionJobRequest -> Aff (err :: AWS.RequestError | eff) StartTranscriptionJobResponse
startTranscriptionJob = AWS.request serviceName "StartTranscriptionJob" 


-- | <p>There is a problem with one of the input fields. Check the S3 bucket name, make sure that the job name is not a duplicate, and confirm that you are using the correct file format. Then resend your request.</p>
newtype BadRequestException = BadRequestException 
  { "Message" :: NullOrUndefined (FailureReason)
  }


-- | <p>The <code>JobName</code> field is a duplicate of a previously entered job name. Resend your request with a different name.</p>
newtype ConflictException = ConflictException 
  { "Message" :: NullOrUndefined (String)
  }


newtype DateTime = DateTime Number


newtype FailureReason = FailureReason String


newtype GetTranscriptionJobRequest = GetTranscriptionJobRequest 
  { "TranscriptionJobName" :: (TranscriptionJobName)
  }


newtype GetTranscriptionJobResponse = GetTranscriptionJobResponse 
  { "TranscriptionJob" :: NullOrUndefined (TranscriptionJob)
  }


-- | <p>There was an internal error. Check the error message and try your request again.</p>
newtype InternalFailureException = InternalFailureException 
  { "Message" :: NullOrUndefined (String)
  }


newtype LanguageCode = LanguageCode String


-- | <p>Either you have sent too many requests or your input file is longer than 2 hours. Wait before you resend your request, or use a smaller file and resend the request.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (String)
  }


newtype ListTranscriptionJobsRequest = ListTranscriptionJobsRequest 
  { "Status" :: (TranscriptionJobStatus)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "MaxResults" :: NullOrUndefined (MaxResults)
  }


newtype ListTranscriptionJobsResponse = ListTranscriptionJobsResponse 
  { "Status" :: NullOrUndefined (TranscriptionJobStatus)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "TranscriptionJobSummaries" :: NullOrUndefined (TranscriptionJobSummaries)
  }


newtype MaxResults = MaxResults Int


-- | <p>Describes the input media file in a transcription request.</p>
newtype Media = Media 
  { "MediaFileUri" :: NullOrUndefined (Uri)
  }


newtype MediaFormat = MediaFormat String


newtype MediaSampleRateHertz = MediaSampleRateHertz Int


newtype NextToken = NextToken String


-- | <p>We can't find the requested job. Check the job name and try your request again.</p>
newtype NotFoundException = NotFoundException 
  { "Message" :: NullOrUndefined (String)
  }


newtype StartTranscriptionJobRequest = StartTranscriptionJobRequest 
  { "TranscriptionJobName" :: (TranscriptionJobName)
  , "LanguageCode" :: (LanguageCode)
  , "MediaSampleRateHertz" :: NullOrUndefined (MediaSampleRateHertz)
  , "MediaFormat" :: (MediaFormat)
  , "Media" :: (Media)
  }


newtype StartTranscriptionJobResponse = StartTranscriptionJobResponse 
  { "TranscriptionJob" :: NullOrUndefined (TranscriptionJob)
  }


-- | <p>Describes the output of a transcription job.</p>
newtype Transcript = Transcript 
  { "TranscriptFileUri" :: NullOrUndefined (Uri)
  }


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


newtype TranscriptionJobName = TranscriptionJobName String


newtype TranscriptionJobStatus = TranscriptionJobStatus String


newtype TranscriptionJobSummaries = TranscriptionJobSummaries (Array TranscriptionJobSummary)


-- | <p>Provides a summary of information about a transcription job.</p>
newtype TranscriptionJobSummary = TranscriptionJobSummary 
  { "TranscriptionJobName" :: NullOrUndefined (TranscriptionJobName)
  , "CreationTime" :: NullOrUndefined (DateTime)
  , "CompletionTime" :: NullOrUndefined (DateTime)
  , "LanguageCode" :: NullOrUndefined (LanguageCode)
  , "TranscriptionJobStatus" :: NullOrUndefined (TranscriptionJobStatus)
  , "FailureReason" :: NullOrUndefined (FailureReason)
  }


newtype Uri = Uri String
