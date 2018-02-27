

-- | <p>AWS IoT Jobs is a service that allows you to define a set of jobs — remote operations that are sent to and executed on one or more devices connected to AWS IoT. For example, you can define a job that instructs a set of devices to download and install application or firmware updates, reboot, rotate certificates, or perform remote troubleshooting operations.</p> <p> To create a job, you make a job document which is a description of the remote operations to be performed, and you specify a list of targets that should perform the operations. The targets can be individual things, thing groups or both.</p> <p> AWS IoT Jobs sends a message to inform the targets that a job is available. The target starts the execution of the job by downloading the job document, performing the operations it specifies, and reporting its progress to AWS IoT. The Jobs service provides commands to track the progress of a job on a specific target and for all the targets of the job</p>
module AWS.IoTJobsDataPlane where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "IoTJobsDataPlane" :: String


-- | <p>Gets details of a job execution.</p>
describeJobExecution :: forall eff. DescribeJobExecutionRequest -> Aff (err :: AWS.RequestError | eff) DescribeJobExecutionResponse
describeJobExecution = AWS.request serviceName "DescribeJobExecution" 


-- | <p>Gets the list of all jobs for a thing that are not in a terminal status.</p>
getPendingJobExecutions :: forall eff. GetPendingJobExecutionsRequest -> Aff (err :: AWS.RequestError | eff) GetPendingJobExecutionsResponse
getPendingJobExecutions = AWS.request serviceName "GetPendingJobExecutions" 


-- | <p>Gets and starts the next pending (status IN_PROGRESS or QUEUED) job execution for a thing.</p>
startNextPendingJobExecution :: forall eff. StartNextPendingJobExecutionRequest -> Aff (err :: AWS.RequestError | eff) StartNextPendingJobExecutionResponse
startNextPendingJobExecution = AWS.request serviceName "StartNextPendingJobExecution" 


-- | <p>Updates the status of a job execution.</p>
updateJobExecution :: forall eff. UpdateJobExecutionRequest -> Aff (err :: AWS.RequestError | eff) UpdateJobExecutionResponse
updateJobExecution = AWS.request serviceName "UpdateJobExecution" 


-- | <p>The certificate is invalid.</p>
newtype CertificateValidationException = CertificateValidationException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeCertificateValidationException :: Newtype CertificateValidationException _


newtype DescribeJobExecutionJobId = DescribeJobExecutionJobId String
derive instance newtypeDescribeJobExecutionJobId :: Newtype DescribeJobExecutionJobId _


newtype DescribeJobExecutionRequest = DescribeJobExecutionRequest 
  { "JobId'" :: (DescribeJobExecutionJobId)
  , "ThingName'" :: (ThingName)
  , "IncludeJobDocument'" :: NullOrUndefined (IncludeJobDocument)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeDescribeJobExecutionRequest :: Newtype DescribeJobExecutionRequest _


newtype DescribeJobExecutionResponse = DescribeJobExecutionResponse 
  { "Execution'" :: NullOrUndefined (JobExecution)
  }
derive instance newtypeDescribeJobExecutionResponse :: Newtype DescribeJobExecutionResponse _


newtype DetailsKey = DetailsKey String
derive instance newtypeDetailsKey :: Newtype DetailsKey _


newtype DetailsMap = DetailsMap (Map DetailsKey DetailsValue)
derive instance newtypeDetailsMap :: Newtype DetailsMap _


newtype DetailsValue = DetailsValue String
derive instance newtypeDetailsValue :: Newtype DetailsValue _


newtype ExecutionNumber = ExecutionNumber Number
derive instance newtypeExecutionNumber :: Newtype ExecutionNumber _


newtype ExpectedVersion = ExpectedVersion Number
derive instance newtypeExpectedVersion :: Newtype ExpectedVersion _


newtype GetPendingJobExecutionsRequest = GetPendingJobExecutionsRequest 
  { "ThingName'" :: (ThingName)
  }
derive instance newtypeGetPendingJobExecutionsRequest :: Newtype GetPendingJobExecutionsRequest _


newtype GetPendingJobExecutionsResponse = GetPendingJobExecutionsResponse 
  { "InProgressJobs'" :: NullOrUndefined (JobExecutionSummaryList)
  , "QueuedJobs'" :: NullOrUndefined (JobExecutionSummaryList)
  }
derive instance newtypeGetPendingJobExecutionsResponse :: Newtype GetPendingJobExecutionsResponse _


newtype IncludeExecutionState = IncludeExecutionState Boolean
derive instance newtypeIncludeExecutionState :: Newtype IncludeExecutionState _


newtype IncludeJobDocument = IncludeJobDocument Boolean
derive instance newtypeIncludeJobDocument :: Newtype IncludeJobDocument _


-- | <p>The contents of the request were invalid. For example, this code is returned when an UpdateJobExecution request contains invalid status details. The message contains details about the error.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


-- | <p>An update attempted to change the job execution to a state that is invalid because of the job execution's current state (for example, an attempt to change a request in state SUCCESS to state IN_PROGRESS). In this case, the body of the error message also contains the executionState field.</p>
newtype InvalidStateTransitionException = InvalidStateTransitionException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidStateTransitionException :: Newtype InvalidStateTransitionException _


newtype JobDocument = JobDocument String
derive instance newtypeJobDocument :: Newtype JobDocument _


-- | <p>Contains data about a job execution.</p>
newtype JobExecution = JobExecution 
  { "JobId'" :: NullOrUndefined (JobId)
  , "ThingName'" :: NullOrUndefined (ThingName)
  , "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined (DetailsMap)
  , "QueuedAt'" :: NullOrUndefined (QueuedAt)
  , "StartedAt'" :: NullOrUndefined (StartedAt)
  , "LastUpdatedAt'" :: NullOrUndefined (LastUpdatedAt)
  , "VersionNumber'" :: NullOrUndefined (VersionNumber)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  , "JobDocument'" :: NullOrUndefined (JobDocument)
  }
derive instance newtypeJobExecution :: Newtype JobExecution _


-- | <p>Contains data about the state of a job execution.</p>
newtype JobExecutionState = JobExecutionState 
  { "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined (DetailsMap)
  , "VersionNumber'" :: NullOrUndefined (VersionNumber)
  }
derive instance newtypeJobExecutionState :: Newtype JobExecutionState _


newtype JobExecutionStatus = JobExecutionStatus String
derive instance newtypeJobExecutionStatus :: Newtype JobExecutionStatus _


-- | <p>Contains a subset of information about a job execution.</p>
newtype JobExecutionSummary = JobExecutionSummary 
  { "JobId'" :: NullOrUndefined (JobId)
  , "QueuedAt'" :: NullOrUndefined (QueuedAt)
  , "StartedAt'" :: NullOrUndefined (StartedAt)
  , "LastUpdatedAt'" :: NullOrUndefined (LastUpdatedAt)
  , "VersionNumber'" :: NullOrUndefined (VersionNumber)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeJobExecutionSummary :: Newtype JobExecutionSummary _


newtype JobExecutionSummaryList = JobExecutionSummaryList (Array JobExecutionSummary)
derive instance newtypeJobExecutionSummaryList :: Newtype JobExecutionSummaryList _


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _


newtype LastUpdatedAt = LastUpdatedAt Number
derive instance newtypeLastUpdatedAt :: Newtype LastUpdatedAt _


newtype QueuedAt = QueuedAt Number
derive instance newtypeQueuedAt :: Newtype QueuedAt _


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The service is temporarily unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


newtype StartNextPendingJobExecutionRequest = StartNextPendingJobExecutionRequest 
  { "ThingName'" :: (ThingName)
  , "StatusDetails'" :: NullOrUndefined (DetailsMap)
  }
derive instance newtypeStartNextPendingJobExecutionRequest :: Newtype StartNextPendingJobExecutionRequest _


newtype StartNextPendingJobExecutionResponse = StartNextPendingJobExecutionResponse 
  { "Execution'" :: NullOrUndefined (JobExecution)
  }
derive instance newtypeStartNextPendingJobExecutionResponse :: Newtype StartNextPendingJobExecutionResponse _


newtype StartedAt = StartedAt Number
derive instance newtypeStartedAt :: Newtype StartedAt _


-- | <p>The job is in a terminal state.</p>
newtype TerminalStateException = TerminalStateException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTerminalStateException :: Newtype TerminalStateException _


newtype ThingName = ThingName String
derive instance newtypeThingName :: Newtype ThingName _


-- | <p>The rate exceeds the limit.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeThrottlingException :: Newtype ThrottlingException _


newtype UpdateJobExecutionRequest = UpdateJobExecutionRequest 
  { "JobId'" :: (JobId)
  , "ThingName'" :: (ThingName)
  , "Status'" :: (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined (DetailsMap)
  , "ExpectedVersion'" :: NullOrUndefined (ExpectedVersion)
  , "IncludeJobExecutionState'" :: NullOrUndefined (IncludeExecutionState)
  , "IncludeJobDocument'" :: NullOrUndefined (IncludeJobDocument)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }
derive instance newtypeUpdateJobExecutionRequest :: Newtype UpdateJobExecutionRequest _


newtype UpdateJobExecutionResponse = UpdateJobExecutionResponse 
  { "ExecutionState'" :: NullOrUndefined (JobExecutionState)
  , "JobDocument'" :: NullOrUndefined (JobDocument)
  }
derive instance newtypeUpdateJobExecutionResponse :: Newtype UpdateJobExecutionResponse _


newtype VersionNumber = VersionNumber Number
derive instance newtypeVersionNumber :: Newtype VersionNumber _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _
