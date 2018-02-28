

-- | <p>AWS IoT Jobs is a service that allows you to define a set of jobs — remote operations that are sent to and executed on one or more devices connected to AWS IoT. For example, you can define a job that instructs a set of devices to download and install application or firmware updates, reboot, rotate certificates, or perform remote troubleshooting operations.</p> <p> To create a job, you make a job document which is a description of the remote operations to be performed, and you specify a list of targets that should perform the operations. The targets can be individual things, thing groups or both.</p> <p> AWS IoT Jobs sends a message to inform the targets that a job is available. The target starts the execution of the job by downloading the job document, performing the operations it specifies, and reporting its progress to AWS IoT. The Jobs service provides commands to track the progress of a job on a specific target and for all the targets of the job</p>
module AWS.IoTJobsDataPlane where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foreign as Foreign
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined as NullOrUndefined
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.StrMap as StrMap

import AWS.Request as Request
import AWS.Request.Types as Types

serviceName = "IoTJobsDataPlane" :: String


-- | <p>Gets details of a job execution.</p>
describeJobExecution :: forall eff. DescribeJobExecutionRequest -> Aff (exception :: EXCEPTION | eff) DescribeJobExecutionResponse
describeJobExecution = Request.request serviceName "describeJobExecution" 


-- | <p>Gets the list of all jobs for a thing that are not in a terminal status.</p>
getPendingJobExecutions :: forall eff. GetPendingJobExecutionsRequest -> Aff (exception :: EXCEPTION | eff) GetPendingJobExecutionsResponse
getPendingJobExecutions = Request.request serviceName "getPendingJobExecutions" 


-- | <p>Gets and starts the next pending (status IN_PROGRESS or QUEUED) job execution for a thing.</p>
startNextPendingJobExecution :: forall eff. StartNextPendingJobExecutionRequest -> Aff (exception :: EXCEPTION | eff) StartNextPendingJobExecutionResponse
startNextPendingJobExecution = Request.request serviceName "startNextPendingJobExecution" 


-- | <p>Updates the status of a job execution.</p>
updateJobExecution :: forall eff. UpdateJobExecutionRequest -> Aff (exception :: EXCEPTION | eff) UpdateJobExecutionResponse
updateJobExecution = Request.request serviceName "updateJobExecution" 


-- | <p>The certificate is invalid.</p>
newtype CertificateValidationException = CertificateValidationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCertificateValidationException :: Newtype CertificateValidationException _
derive instance repGenericCertificateValidationException :: Generic CertificateValidationException _
instance showCertificateValidationException :: Show CertificateValidationException where
  show = genericShow
instance decodeCertificateValidationException :: Decode CertificateValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCertificateValidationException :: Encode CertificateValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobExecutionJobId = DescribeJobExecutionJobId String
derive instance newtypeDescribeJobExecutionJobId :: Newtype DescribeJobExecutionJobId _
derive instance repGenericDescribeJobExecutionJobId :: Generic DescribeJobExecutionJobId _
instance showDescribeJobExecutionJobId :: Show DescribeJobExecutionJobId where
  show = genericShow
instance decodeDescribeJobExecutionJobId :: Decode DescribeJobExecutionJobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobExecutionJobId :: Encode DescribeJobExecutionJobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobExecutionRequest = DescribeJobExecutionRequest 
  { "JobId'" :: (DescribeJobExecutionJobId)
  , "ThingName'" :: (ThingName)
  , "IncludeJobDocument'" :: NullOrUndefined.NullOrUndefined (IncludeJobDocument)
  , "ExecutionNumber'" :: NullOrUndefined.NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeDescribeJobExecutionRequest :: Newtype DescribeJobExecutionRequest _
derive instance repGenericDescribeJobExecutionRequest :: Generic DescribeJobExecutionRequest _
instance showDescribeJobExecutionRequest :: Show DescribeJobExecutionRequest where
  show = genericShow
instance decodeDescribeJobExecutionRequest :: Decode DescribeJobExecutionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobExecutionRequest :: Encode DescribeJobExecutionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeJobExecutionResponse = DescribeJobExecutionResponse 
  { "Execution'" :: NullOrUndefined.NullOrUndefined (JobExecution)
  }
derive instance newtypeDescribeJobExecutionResponse :: Newtype DescribeJobExecutionResponse _
derive instance repGenericDescribeJobExecutionResponse :: Generic DescribeJobExecutionResponse _
instance showDescribeJobExecutionResponse :: Show DescribeJobExecutionResponse where
  show = genericShow
instance decodeDescribeJobExecutionResponse :: Decode DescribeJobExecutionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeJobExecutionResponse :: Encode DescribeJobExecutionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetailsKey = DetailsKey String
derive instance newtypeDetailsKey :: Newtype DetailsKey _
derive instance repGenericDetailsKey :: Generic DetailsKey _
instance showDetailsKey :: Show DetailsKey where
  show = genericShow
instance decodeDetailsKey :: Decode DetailsKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsKey :: Encode DetailsKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetailsMap = DetailsMap (StrMap.StrMap DetailsValue)
derive instance newtypeDetailsMap :: Newtype DetailsMap _
derive instance repGenericDetailsMap :: Generic DetailsMap _
instance showDetailsMap :: Show DetailsMap where
  show = genericShow
instance decodeDetailsMap :: Decode DetailsMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsMap :: Encode DetailsMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DetailsValue = DetailsValue String
derive instance newtypeDetailsValue :: Newtype DetailsValue _
derive instance repGenericDetailsValue :: Generic DetailsValue _
instance showDetailsValue :: Show DetailsValue where
  show = genericShow
instance decodeDetailsValue :: Decode DetailsValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDetailsValue :: Encode DetailsValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionNumber = ExecutionNumber Number
derive instance newtypeExecutionNumber :: Newtype ExecutionNumber _
derive instance repGenericExecutionNumber :: Generic ExecutionNumber _
instance showExecutionNumber :: Show ExecutionNumber where
  show = genericShow
instance decodeExecutionNumber :: Decode ExecutionNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionNumber :: Encode ExecutionNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExpectedVersion = ExpectedVersion Number
derive instance newtypeExpectedVersion :: Newtype ExpectedVersion _
derive instance repGenericExpectedVersion :: Generic ExpectedVersion _
instance showExpectedVersion :: Show ExpectedVersion where
  show = genericShow
instance decodeExpectedVersion :: Decode ExpectedVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExpectedVersion :: Encode ExpectedVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPendingJobExecutionsRequest = GetPendingJobExecutionsRequest 
  { "ThingName'" :: (ThingName)
  }
derive instance newtypeGetPendingJobExecutionsRequest :: Newtype GetPendingJobExecutionsRequest _
derive instance repGenericGetPendingJobExecutionsRequest :: Generic GetPendingJobExecutionsRequest _
instance showGetPendingJobExecutionsRequest :: Show GetPendingJobExecutionsRequest where
  show = genericShow
instance decodeGetPendingJobExecutionsRequest :: Decode GetPendingJobExecutionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPendingJobExecutionsRequest :: Encode GetPendingJobExecutionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetPendingJobExecutionsResponse = GetPendingJobExecutionsResponse 
  { "InProgressJobs'" :: NullOrUndefined.NullOrUndefined (JobExecutionSummaryList)
  , "QueuedJobs'" :: NullOrUndefined.NullOrUndefined (JobExecutionSummaryList)
  }
derive instance newtypeGetPendingJobExecutionsResponse :: Newtype GetPendingJobExecutionsResponse _
derive instance repGenericGetPendingJobExecutionsResponse :: Generic GetPendingJobExecutionsResponse _
instance showGetPendingJobExecutionsResponse :: Show GetPendingJobExecutionsResponse where
  show = genericShow
instance decodeGetPendingJobExecutionsResponse :: Decode GetPendingJobExecutionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPendingJobExecutionsResponse :: Encode GetPendingJobExecutionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IncludeExecutionState = IncludeExecutionState Boolean
derive instance newtypeIncludeExecutionState :: Newtype IncludeExecutionState _
derive instance repGenericIncludeExecutionState :: Generic IncludeExecutionState _
instance showIncludeExecutionState :: Show IncludeExecutionState where
  show = genericShow
instance decodeIncludeExecutionState :: Decode IncludeExecutionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncludeExecutionState :: Encode IncludeExecutionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IncludeJobDocument = IncludeJobDocument Boolean
derive instance newtypeIncludeJobDocument :: Newtype IncludeJobDocument _
derive instance repGenericIncludeJobDocument :: Generic IncludeJobDocument _
instance showIncludeJobDocument :: Show IncludeJobDocument where
  show = genericShow
instance decodeIncludeJobDocument :: Decode IncludeJobDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIncludeJobDocument :: Encode IncludeJobDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The contents of the request were invalid. For example, this code is returned when an UpdateJobExecution request contains invalid status details. The message contains details about the error.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _
derive instance repGenericInvalidRequestException :: Generic InvalidRequestException _
instance showInvalidRequestException :: Show InvalidRequestException where
  show = genericShow
instance decodeInvalidRequestException :: Decode InvalidRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidRequestException :: Encode InvalidRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An update attempted to change the job execution to a state that is invalid because of the job execution's current state (for example, an attempt to change a request in state SUCCESS to state IN_PROGRESS). In this case, the body of the error message also contains the executionState field.</p>
newtype InvalidStateTransitionException = InvalidStateTransitionException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidStateTransitionException :: Newtype InvalidStateTransitionException _
derive instance repGenericInvalidStateTransitionException :: Generic InvalidStateTransitionException _
instance showInvalidStateTransitionException :: Show InvalidStateTransitionException where
  show = genericShow
instance decodeInvalidStateTransitionException :: Decode InvalidStateTransitionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidStateTransitionException :: Encode InvalidStateTransitionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobDocument = JobDocument String
derive instance newtypeJobDocument :: Newtype JobDocument _
derive instance repGenericJobDocument :: Generic JobDocument _
instance showJobDocument :: Show JobDocument where
  show = genericShow
instance decodeJobDocument :: Decode JobDocument where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDocument :: Encode JobDocument where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains data about a job execution.</p>
newtype JobExecution = JobExecution 
  { "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "ThingName'" :: NullOrUndefined.NullOrUndefined (ThingName)
  , "Status'" :: NullOrUndefined.NullOrUndefined (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined.NullOrUndefined (DetailsMap)
  , "QueuedAt'" :: NullOrUndefined.NullOrUndefined (QueuedAt)
  , "StartedAt'" :: NullOrUndefined.NullOrUndefined (StartedAt)
  , "LastUpdatedAt'" :: NullOrUndefined.NullOrUndefined (LastUpdatedAt)
  , "VersionNumber'" :: NullOrUndefined.NullOrUndefined (VersionNumber)
  , "ExecutionNumber'" :: NullOrUndefined.NullOrUndefined (ExecutionNumber)
  , "JobDocument'" :: NullOrUndefined.NullOrUndefined (JobDocument)
  }
derive instance newtypeJobExecution :: Newtype JobExecution _
derive instance repGenericJobExecution :: Generic JobExecution _
instance showJobExecution :: Show JobExecution where
  show = genericShow
instance decodeJobExecution :: Decode JobExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecution :: Encode JobExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains data about the state of a job execution.</p>
newtype JobExecutionState = JobExecutionState 
  { "Status'" :: NullOrUndefined.NullOrUndefined (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined.NullOrUndefined (DetailsMap)
  , "VersionNumber'" :: NullOrUndefined.NullOrUndefined (VersionNumber)
  }
derive instance newtypeJobExecutionState :: Newtype JobExecutionState _
derive instance repGenericJobExecutionState :: Generic JobExecutionState _
instance showJobExecutionState :: Show JobExecutionState where
  show = genericShow
instance decodeJobExecutionState :: Decode JobExecutionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionState :: Encode JobExecutionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobExecutionStatus = JobExecutionStatus String
derive instance newtypeJobExecutionStatus :: Newtype JobExecutionStatus _
derive instance repGenericJobExecutionStatus :: Generic JobExecutionStatus _
instance showJobExecutionStatus :: Show JobExecutionStatus where
  show = genericShow
instance decodeJobExecutionStatus :: Decode JobExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionStatus :: Encode JobExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains a subset of information about a job execution.</p>
newtype JobExecutionSummary = JobExecutionSummary 
  { "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "QueuedAt'" :: NullOrUndefined.NullOrUndefined (QueuedAt)
  , "StartedAt'" :: NullOrUndefined.NullOrUndefined (StartedAt)
  , "LastUpdatedAt'" :: NullOrUndefined.NullOrUndefined (LastUpdatedAt)
  , "VersionNumber'" :: NullOrUndefined.NullOrUndefined (VersionNumber)
  , "ExecutionNumber'" :: NullOrUndefined.NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeJobExecutionSummary :: Newtype JobExecutionSummary _
derive instance repGenericJobExecutionSummary :: Generic JobExecutionSummary _
instance showJobExecutionSummary :: Show JobExecutionSummary where
  show = genericShow
instance decodeJobExecutionSummary :: Decode JobExecutionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionSummary :: Encode JobExecutionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobExecutionSummaryList = JobExecutionSummaryList (Array JobExecutionSummary)
derive instance newtypeJobExecutionSummaryList :: Newtype JobExecutionSummaryList _
derive instance repGenericJobExecutionSummaryList :: Generic JobExecutionSummaryList _
instance showJobExecutionSummaryList :: Show JobExecutionSummaryList where
  show = genericShow
instance decodeJobExecutionSummaryList :: Decode JobExecutionSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobExecutionSummaryList :: Encode JobExecutionSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _
derive instance repGenericJobId :: Generic JobId _
instance showJobId :: Show JobId where
  show = genericShow
instance decodeJobId :: Decode JobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobId :: Encode JobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastUpdatedAt = LastUpdatedAt Number
derive instance newtypeLastUpdatedAt :: Newtype LastUpdatedAt _
derive instance repGenericLastUpdatedAt :: Generic LastUpdatedAt _
instance showLastUpdatedAt :: Show LastUpdatedAt where
  show = genericShow
instance decodeLastUpdatedAt :: Decode LastUpdatedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastUpdatedAt :: Encode LastUpdatedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueuedAt = QueuedAt Number
derive instance newtypeQueuedAt :: Newtype QueuedAt _
derive instance repGenericQueuedAt :: Generic QueuedAt _
instance showQueuedAt :: Show QueuedAt where
  show = genericShow
instance decodeQueuedAt :: Decode QueuedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueuedAt :: Encode QueuedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The service is temporarily unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartNextPendingJobExecutionRequest = StartNextPendingJobExecutionRequest 
  { "ThingName'" :: (ThingName)
  , "StatusDetails'" :: NullOrUndefined.NullOrUndefined (DetailsMap)
  }
derive instance newtypeStartNextPendingJobExecutionRequest :: Newtype StartNextPendingJobExecutionRequest _
derive instance repGenericStartNextPendingJobExecutionRequest :: Generic StartNextPendingJobExecutionRequest _
instance showStartNextPendingJobExecutionRequest :: Show StartNextPendingJobExecutionRequest where
  show = genericShow
instance decodeStartNextPendingJobExecutionRequest :: Decode StartNextPendingJobExecutionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartNextPendingJobExecutionRequest :: Encode StartNextPendingJobExecutionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartNextPendingJobExecutionResponse = StartNextPendingJobExecutionResponse 
  { "Execution'" :: NullOrUndefined.NullOrUndefined (JobExecution)
  }
derive instance newtypeStartNextPendingJobExecutionResponse :: Newtype StartNextPendingJobExecutionResponse _
derive instance repGenericStartNextPendingJobExecutionResponse :: Generic StartNextPendingJobExecutionResponse _
instance showStartNextPendingJobExecutionResponse :: Show StartNextPendingJobExecutionResponse where
  show = genericShow
instance decodeStartNextPendingJobExecutionResponse :: Decode StartNextPendingJobExecutionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartNextPendingJobExecutionResponse :: Encode StartNextPendingJobExecutionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartedAt = StartedAt Number
derive instance newtypeStartedAt :: Newtype StartedAt _
derive instance repGenericStartedAt :: Generic StartedAt _
instance showStartedAt :: Show StartedAt where
  show = genericShow
instance decodeStartedAt :: Decode StartedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartedAt :: Encode StartedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The job is in a terminal state.</p>
newtype TerminalStateException = TerminalStateException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTerminalStateException :: Newtype TerminalStateException _
derive instance repGenericTerminalStateException :: Generic TerminalStateException _
instance showTerminalStateException :: Show TerminalStateException where
  show = genericShow
instance decodeTerminalStateException :: Decode TerminalStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminalStateException :: Encode TerminalStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThingName = ThingName String
derive instance newtypeThingName :: Newtype ThingName _
derive instance repGenericThingName :: Generic ThingName _
instance showThingName :: Show ThingName where
  show = genericShow
instance decodeThingName :: Decode ThingName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThingName :: Encode ThingName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The rate exceeds the limit.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage')
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _
derive instance repGenericThrottlingException :: Generic ThrottlingException _
instance showThrottlingException :: Show ThrottlingException where
  show = genericShow
instance decodeThrottlingException :: Decode ThrottlingException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThrottlingException :: Encode ThrottlingException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateJobExecutionRequest = UpdateJobExecutionRequest 
  { "JobId'" :: (JobId)
  , "ThingName'" :: (ThingName)
  , "Status'" :: (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined.NullOrUndefined (DetailsMap)
  , "ExpectedVersion'" :: NullOrUndefined.NullOrUndefined (ExpectedVersion)
  , "IncludeJobExecutionState'" :: NullOrUndefined.NullOrUndefined (IncludeExecutionState)
  , "IncludeJobDocument'" :: NullOrUndefined.NullOrUndefined (IncludeJobDocument)
  , "ExecutionNumber'" :: NullOrUndefined.NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeUpdateJobExecutionRequest :: Newtype UpdateJobExecutionRequest _
derive instance repGenericUpdateJobExecutionRequest :: Generic UpdateJobExecutionRequest _
instance showUpdateJobExecutionRequest :: Show UpdateJobExecutionRequest where
  show = genericShow
instance decodeUpdateJobExecutionRequest :: Decode UpdateJobExecutionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobExecutionRequest :: Encode UpdateJobExecutionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateJobExecutionResponse = UpdateJobExecutionResponse 
  { "ExecutionState'" :: NullOrUndefined.NullOrUndefined (JobExecutionState)
  , "JobDocument'" :: NullOrUndefined.NullOrUndefined (JobDocument)
  }
derive instance newtypeUpdateJobExecutionResponse :: Newtype UpdateJobExecutionResponse _
derive instance repGenericUpdateJobExecutionResponse :: Generic UpdateJobExecutionResponse _
instance showUpdateJobExecutionResponse :: Show UpdateJobExecutionResponse where
  show = genericShow
instance decodeUpdateJobExecutionResponse :: Decode UpdateJobExecutionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateJobExecutionResponse :: Encode UpdateJobExecutionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VersionNumber = VersionNumber Number
derive instance newtypeVersionNumber :: Newtype VersionNumber _
derive instance repGenericVersionNumber :: Generic VersionNumber _
instance showVersionNumber :: Show VersionNumber where
  show = genericShow
instance decodeVersionNumber :: Decode VersionNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersionNumber :: Encode VersionNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
derive instance repGenericErrorMessage' :: Generic ErrorMessage' _
instance showErrorMessage' :: Show ErrorMessage' where
  show = genericShow
instance decodeErrorMessage' :: Decode ErrorMessage' where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage' :: Encode ErrorMessage' where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
