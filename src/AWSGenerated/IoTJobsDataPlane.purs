

-- | <p>AWS IoT Jobs is a service that allows you to define a set of jobs — remote operations that are sent to and executed on one or more devices connected to AWS IoT. For example, you can define a job that instructs a set of devices to download and install application or firmware updates, reboot, rotate certificates, or perform remote troubleshooting operations.</p> <p> To create a job, you make a job document which is a description of the remote operations to be performed, and you specify a list of targets that should perform the operations. The targets can be individual things, thing groups or both.</p> <p> AWS IoT Jobs sends a message to inform the targets that a job is available. The target starts the execution of the job by downloading the job document, performing the operations it specifies, and reporting its progress to AWS IoT. The Jobs service provides commands to track the progress of a job on a specific target and for all the targets of the job</p>
module AWS.IoTJobsDataPlane where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
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


newtype DescribeJobExecutionJobId = DescribeJobExecutionJobId String


newtype DescribeJobExecutionRequest = DescribeJobExecutionRequest 
  { "JobId'" :: (DescribeJobExecutionJobId)
  , "ThingName'" :: (ThingName)
  , "IncludeJobDocument'" :: NullOrUndefined (IncludeJobDocument)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }


newtype DescribeJobExecutionResponse = DescribeJobExecutionResponse 
  { "Execution'" :: NullOrUndefined (JobExecution)
  }


newtype DetailsKey = DetailsKey String


newtype DetailsMap = DetailsMap (Map DetailsKey DetailsValue)


newtype DetailsValue = DetailsValue String


newtype ExecutionNumber = ExecutionNumber Number


newtype ExpectedVersion = ExpectedVersion Number


newtype GetPendingJobExecutionsRequest = GetPendingJobExecutionsRequest 
  { "ThingName'" :: (ThingName)
  }


newtype GetPendingJobExecutionsResponse = GetPendingJobExecutionsResponse 
  { "InProgressJobs'" :: NullOrUndefined (JobExecutionSummaryList)
  , "QueuedJobs'" :: NullOrUndefined (JobExecutionSummaryList)
  }


newtype IncludeExecutionState = IncludeExecutionState Boolean


newtype IncludeJobDocument = IncludeJobDocument Boolean


-- | <p>The contents of the request were invalid. For example, this code is returned when an UpdateJobExecution request contains invalid status details. The message contains details about the error.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>An update attempted to change the job execution to a state that is invalid because of the job execution's current state (for example, an attempt to change a request in state SUCCESS to state IN_PROGRESS). In this case, the body of the error message also contains the executionState field.</p>
newtype InvalidStateTransitionException = InvalidStateTransitionException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype JobDocument = JobDocument String


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


-- | <p>Contains data about the state of a job execution.</p>
newtype JobExecutionState = JobExecutionState 
  { "Status'" :: NullOrUndefined (JobExecutionStatus)
  , "StatusDetails'" :: NullOrUndefined (DetailsMap)
  , "VersionNumber'" :: NullOrUndefined (VersionNumber)
  }


newtype JobExecutionStatus = JobExecutionStatus String


-- | <p>Contains a subset of information about a job execution.</p>
newtype JobExecutionSummary = JobExecutionSummary 
  { "JobId'" :: NullOrUndefined (JobId)
  , "QueuedAt'" :: NullOrUndefined (QueuedAt)
  , "StartedAt'" :: NullOrUndefined (StartedAt)
  , "LastUpdatedAt'" :: NullOrUndefined (LastUpdatedAt)
  , "VersionNumber'" :: NullOrUndefined (VersionNumber)
  , "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber)
  }


newtype JobExecutionSummaryList = JobExecutionSummaryList (Array JobExecutionSummary)


newtype JobId = JobId String


newtype LastUpdatedAt = LastUpdatedAt Number


newtype QueuedAt = QueuedAt Number


-- | <p>The specified resource does not exist.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


-- | <p>The service is temporarily unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype StartNextPendingJobExecutionRequest = StartNextPendingJobExecutionRequest 
  { "ThingName'" :: (ThingName)
  , "StatusDetails'" :: NullOrUndefined (DetailsMap)
  }


newtype StartNextPendingJobExecutionResponse = StartNextPendingJobExecutionResponse 
  { "Execution'" :: NullOrUndefined (JobExecution)
  }


newtype StartedAt = StartedAt Number


-- | <p>The job is in a terminal state.</p>
newtype TerminalStateException = TerminalStateException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


newtype ThingName = ThingName String


-- | <p>The rate exceeds the limit.</p>
newtype ThrottlingException = ThrottlingException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }


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


newtype UpdateJobExecutionResponse = UpdateJobExecutionResponse 
  { "ExecutionState'" :: NullOrUndefined (JobExecutionState)
  , "JobDocument'" :: NullOrUndefined (JobDocument)
  }


newtype VersionNumber = VersionNumber Number


newtype ErrorMessage' = ErrorMessage' String
