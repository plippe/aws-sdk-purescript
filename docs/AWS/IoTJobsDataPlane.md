## Module AWS.IoTJobsDataPlane

<p>AWS IoT Jobs is a service that allows you to define a set of jobs — remote operations that are sent to and executed on one or more devices connected to AWS IoT. For example, you can define a job that instructs a set of devices to download and install application or firmware updates, reboot, rotate certificates, or perform remote troubleshooting operations.</p> <p> To create a job, you make a job document which is a description of the remote operations to be performed, and you specify a list of targets that should perform the operations. The targets can be individual things, thing groups or both.</p> <p> AWS IoT Jobs sends a message to inform the targets that a job is available. The target starts the execution of the job by downloading the job document, performing the operations it specifies, and reporting its progress to AWS IoT. The Jobs service provides commands to track the progress of a job on a specific target and for all the targets of the job</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `describeJobExecution`

``` purescript
describeJobExecution :: forall eff. DescribeJobExecutionRequest -> Aff (err :: RequestError | eff) DescribeJobExecutionResponse
```

<p>Gets details of a job execution.</p>

#### `getPendingJobExecutions`

``` purescript
getPendingJobExecutions :: forall eff. GetPendingJobExecutionsRequest -> Aff (err :: RequestError | eff) GetPendingJobExecutionsResponse
```

<p>Gets the list of all jobs for a thing that are not in a terminal status.</p>

#### `startNextPendingJobExecution`

``` purescript
startNextPendingJobExecution :: forall eff. StartNextPendingJobExecutionRequest -> Aff (err :: RequestError | eff) StartNextPendingJobExecutionResponse
```

<p>Gets and starts the next pending (status IN_PROGRESS or QUEUED) job execution for a thing.</p>

#### `updateJobExecution`

``` purescript
updateJobExecution :: forall eff. UpdateJobExecutionRequest -> Aff (err :: RequestError | eff) UpdateJobExecutionResponse
```

<p>Updates the status of a job execution.</p>

#### `CertificateValidationException`

``` purescript
newtype CertificateValidationException
  = CertificateValidationException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The certificate is invalid.</p>

#### `DescribeJobExecutionJobId`

``` purescript
newtype DescribeJobExecutionJobId
  = DescribeJobExecutionJobId String
```

#### `DescribeJobExecutionRequest`

``` purescript
newtype DescribeJobExecutionRequest
  = DescribeJobExecutionRequest { "JobId'" :: DescribeJobExecutionJobId, "ThingName'" :: ThingName, "IncludeJobDocument'" :: NullOrUndefined (IncludeJobDocument), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

#### `DescribeJobExecutionResponse`

``` purescript
newtype DescribeJobExecutionResponse
  = DescribeJobExecutionResponse { "Execution'" :: NullOrUndefined (JobExecution) }
```

#### `DetailsKey`

``` purescript
newtype DetailsKey
  = DetailsKey String
```

#### `DetailsMap`

``` purescript
newtype DetailsMap
  = DetailsMap (Map DetailsKey DetailsValue)
```

#### `DetailsValue`

``` purescript
newtype DetailsValue
  = DetailsValue String
```

#### `ExecutionNumber`

``` purescript
newtype ExecutionNumber
  = ExecutionNumber Number
```

#### `ExpectedVersion`

``` purescript
newtype ExpectedVersion
  = ExpectedVersion Number
```

#### `GetPendingJobExecutionsRequest`

``` purescript
newtype GetPendingJobExecutionsRequest
  = GetPendingJobExecutionsRequest { "ThingName'" :: ThingName }
```

#### `GetPendingJobExecutionsResponse`

``` purescript
newtype GetPendingJobExecutionsResponse
  = GetPendingJobExecutionsResponse { "InProgressJobs'" :: NullOrUndefined (JobExecutionSummaryList), "QueuedJobs'" :: NullOrUndefined (JobExecutionSummaryList) }
```

#### `IncludeExecutionState`

``` purescript
newtype IncludeExecutionState
  = IncludeExecutionState Boolean
```

#### `IncludeJobDocument`

``` purescript
newtype IncludeJobDocument
  = IncludeJobDocument Boolean
```

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The contents of the request were invalid. For example, this code is returned when an UpdateJobExecution request contains invalid status details. The message contains details about the error.</p>

#### `InvalidStateTransitionException`

``` purescript
newtype InvalidStateTransitionException
  = InvalidStateTransitionException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An update attempted to change the job execution to a state that is invalid because of the job execution's current state (for example, an attempt to change a request in state SUCCESS to state IN_PROGRESS). In this case, the body of the error message also contains the executionState field.</p>

#### `JobDocument`

``` purescript
newtype JobDocument
  = JobDocument String
```

#### `JobExecution`

``` purescript
newtype JobExecution
  = JobExecution { "JobId'" :: NullOrUndefined (JobId), "ThingName'" :: NullOrUndefined (ThingName), "Status'" :: NullOrUndefined (JobExecutionStatus), "StatusDetails'" :: NullOrUndefined (DetailsMap), "QueuedAt'" :: NullOrUndefined (QueuedAt), "StartedAt'" :: NullOrUndefined (StartedAt), "LastUpdatedAt'" :: NullOrUndefined (LastUpdatedAt), "VersionNumber'" :: NullOrUndefined (VersionNumber), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber), "JobDocument'" :: NullOrUndefined (JobDocument) }
```

<p>Contains data about a job execution.</p>

#### `JobExecutionState`

``` purescript
newtype JobExecutionState
  = JobExecutionState { "Status'" :: NullOrUndefined (JobExecutionStatus), "StatusDetails'" :: NullOrUndefined (DetailsMap), "VersionNumber'" :: NullOrUndefined (VersionNumber) }
```

<p>Contains data about the state of a job execution.</p>

#### `JobExecutionStatus`

``` purescript
newtype JobExecutionStatus
  = JobExecutionStatus String
```

#### `JobExecutionSummary`

``` purescript
newtype JobExecutionSummary
  = JobExecutionSummary { "JobId'" :: NullOrUndefined (JobId), "QueuedAt'" :: NullOrUndefined (QueuedAt), "StartedAt'" :: NullOrUndefined (StartedAt), "LastUpdatedAt'" :: NullOrUndefined (LastUpdatedAt), "VersionNumber'" :: NullOrUndefined (VersionNumber), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

<p>Contains a subset of information about a job execution.</p>

#### `JobExecutionSummaryList`

``` purescript
newtype JobExecutionSummaryList
  = JobExecutionSummaryList (Array JobExecutionSummary)
```

#### `JobId`

``` purescript
newtype JobId
  = JobId String
```

#### `LastUpdatedAt`

``` purescript
newtype LastUpdatedAt
  = LastUpdatedAt Number
```

#### `QueuedAt`

``` purescript
newtype QueuedAt
  = QueuedAt Number
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The specified resource does not exist.</p>

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The service is temporarily unavailable.</p>

#### `StartNextPendingJobExecutionRequest`

``` purescript
newtype StartNextPendingJobExecutionRequest
  = StartNextPendingJobExecutionRequest { "ThingName'" :: ThingName, "StatusDetails'" :: NullOrUndefined (DetailsMap) }
```

#### `StartNextPendingJobExecutionResponse`

``` purescript
newtype StartNextPendingJobExecutionResponse
  = StartNextPendingJobExecutionResponse { "Execution'" :: NullOrUndefined (JobExecution) }
```

#### `StartedAt`

``` purescript
newtype StartedAt
  = StartedAt Number
```

#### `TerminalStateException`

``` purescript
newtype TerminalStateException
  = TerminalStateException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The job is in a terminal state.</p>

#### `ThingName`

``` purescript
newtype ThingName
  = ThingName String
```

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The rate exceeds the limit.</p>

#### `UpdateJobExecutionRequest`

``` purescript
newtype UpdateJobExecutionRequest
  = UpdateJobExecutionRequest { "JobId'" :: JobId, "ThingName'" :: ThingName, "Status'" :: JobExecutionStatus, "StatusDetails'" :: NullOrUndefined (DetailsMap), "ExpectedVersion'" :: NullOrUndefined (ExpectedVersion), "IncludeJobExecutionState'" :: NullOrUndefined (IncludeExecutionState), "IncludeJobDocument'" :: NullOrUndefined (IncludeJobDocument), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

#### `UpdateJobExecutionResponse`

``` purescript
newtype UpdateJobExecutionResponse
  = UpdateJobExecutionResponse { "ExecutionState'" :: NullOrUndefined (JobExecutionState), "JobDocument'" :: NullOrUndefined (JobDocument) }
```

#### `VersionNumber`

``` purescript
newtype VersionNumber
  = VersionNumber Number
```

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```


