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

##### Instances
``` purescript
Newtype CertificateValidationException _
```

#### `DescribeJobExecutionJobId`

``` purescript
newtype DescribeJobExecutionJobId
  = DescribeJobExecutionJobId String
```

##### Instances
``` purescript
Newtype DescribeJobExecutionJobId _
```

#### `DescribeJobExecutionRequest`

``` purescript
newtype DescribeJobExecutionRequest
  = DescribeJobExecutionRequest { "JobId'" :: DescribeJobExecutionJobId, "ThingName'" :: ThingName, "IncludeJobDocument'" :: NullOrUndefined (IncludeJobDocument), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

##### Instances
``` purescript
Newtype DescribeJobExecutionRequest _
```

#### `DescribeJobExecutionResponse`

``` purescript
newtype DescribeJobExecutionResponse
  = DescribeJobExecutionResponse { "Execution'" :: NullOrUndefined (JobExecution) }
```

##### Instances
``` purescript
Newtype DescribeJobExecutionResponse _
```

#### `DetailsKey`

``` purescript
newtype DetailsKey
  = DetailsKey String
```

##### Instances
``` purescript
Newtype DetailsKey _
```

#### `DetailsMap`

``` purescript
newtype DetailsMap
  = DetailsMap (Map DetailsKey DetailsValue)
```

##### Instances
``` purescript
Newtype DetailsMap _
```

#### `DetailsValue`

``` purescript
newtype DetailsValue
  = DetailsValue String
```

##### Instances
``` purescript
Newtype DetailsValue _
```

#### `ExecutionNumber`

``` purescript
newtype ExecutionNumber
  = ExecutionNumber Number
```

##### Instances
``` purescript
Newtype ExecutionNumber _
```

#### `ExpectedVersion`

``` purescript
newtype ExpectedVersion
  = ExpectedVersion Number
```

##### Instances
``` purescript
Newtype ExpectedVersion _
```

#### `GetPendingJobExecutionsRequest`

``` purescript
newtype GetPendingJobExecutionsRequest
  = GetPendingJobExecutionsRequest { "ThingName'" :: ThingName }
```

##### Instances
``` purescript
Newtype GetPendingJobExecutionsRequest _
```

#### `GetPendingJobExecutionsResponse`

``` purescript
newtype GetPendingJobExecutionsResponse
  = GetPendingJobExecutionsResponse { "InProgressJobs'" :: NullOrUndefined (JobExecutionSummaryList), "QueuedJobs'" :: NullOrUndefined (JobExecutionSummaryList) }
```

##### Instances
``` purescript
Newtype GetPendingJobExecutionsResponse _
```

#### `IncludeExecutionState`

``` purescript
newtype IncludeExecutionState
  = IncludeExecutionState Boolean
```

##### Instances
``` purescript
Newtype IncludeExecutionState _
```

#### `IncludeJobDocument`

``` purescript
newtype IncludeJobDocument
  = IncludeJobDocument Boolean
```

##### Instances
``` purescript
Newtype IncludeJobDocument _
```

#### `InvalidRequestException`

``` purescript
newtype InvalidRequestException
  = InvalidRequestException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The contents of the request were invalid. For example, this code is returned when an UpdateJobExecution request contains invalid status details. The message contains details about the error.</p>

##### Instances
``` purescript
Newtype InvalidRequestException _
```

#### `InvalidStateTransitionException`

``` purescript
newtype InvalidStateTransitionException
  = InvalidStateTransitionException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>An update attempted to change the job execution to a state that is invalid because of the job execution's current state (for example, an attempt to change a request in state SUCCESS to state IN_PROGRESS). In this case, the body of the error message also contains the executionState field.</p>

##### Instances
``` purescript
Newtype InvalidStateTransitionException _
```

#### `JobDocument`

``` purescript
newtype JobDocument
  = JobDocument String
```

##### Instances
``` purescript
Newtype JobDocument _
```

#### `JobExecution`

``` purescript
newtype JobExecution
  = JobExecution { "JobId'" :: NullOrUndefined (JobId), "ThingName'" :: NullOrUndefined (ThingName), "Status'" :: NullOrUndefined (JobExecutionStatus), "StatusDetails'" :: NullOrUndefined (DetailsMap), "QueuedAt'" :: NullOrUndefined (QueuedAt), "StartedAt'" :: NullOrUndefined (StartedAt), "LastUpdatedAt'" :: NullOrUndefined (LastUpdatedAt), "VersionNumber'" :: NullOrUndefined (VersionNumber), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber), "JobDocument'" :: NullOrUndefined (JobDocument) }
```

<p>Contains data about a job execution.</p>

##### Instances
``` purescript
Newtype JobExecution _
```

#### `JobExecutionState`

``` purescript
newtype JobExecutionState
  = JobExecutionState { "Status'" :: NullOrUndefined (JobExecutionStatus), "StatusDetails'" :: NullOrUndefined (DetailsMap), "VersionNumber'" :: NullOrUndefined (VersionNumber) }
```

<p>Contains data about the state of a job execution.</p>

##### Instances
``` purescript
Newtype JobExecutionState _
```

#### `JobExecutionStatus`

``` purescript
newtype JobExecutionStatus
  = JobExecutionStatus String
```

##### Instances
``` purescript
Newtype JobExecutionStatus _
```

#### `JobExecutionSummary`

``` purescript
newtype JobExecutionSummary
  = JobExecutionSummary { "JobId'" :: NullOrUndefined (JobId), "QueuedAt'" :: NullOrUndefined (QueuedAt), "StartedAt'" :: NullOrUndefined (StartedAt), "LastUpdatedAt'" :: NullOrUndefined (LastUpdatedAt), "VersionNumber'" :: NullOrUndefined (VersionNumber), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

<p>Contains a subset of information about a job execution.</p>

##### Instances
``` purescript
Newtype JobExecutionSummary _
```

#### `JobExecutionSummaryList`

``` purescript
newtype JobExecutionSummaryList
  = JobExecutionSummaryList (Array JobExecutionSummary)
```

##### Instances
``` purescript
Newtype JobExecutionSummaryList _
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

#### `LastUpdatedAt`

``` purescript
newtype LastUpdatedAt
  = LastUpdatedAt Number
```

##### Instances
``` purescript
Newtype LastUpdatedAt _
```

#### `QueuedAt`

``` purescript
newtype QueuedAt
  = QueuedAt Number
```

##### Instances
``` purescript
Newtype QueuedAt _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The specified resource does not exist.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The service is temporarily unavailable.</p>

##### Instances
``` purescript
Newtype ServiceUnavailableException _
```

#### `StartNextPendingJobExecutionRequest`

``` purescript
newtype StartNextPendingJobExecutionRequest
  = StartNextPendingJobExecutionRequest { "ThingName'" :: ThingName, "StatusDetails'" :: NullOrUndefined (DetailsMap) }
```

##### Instances
``` purescript
Newtype StartNextPendingJobExecutionRequest _
```

#### `StartNextPendingJobExecutionResponse`

``` purescript
newtype StartNextPendingJobExecutionResponse
  = StartNextPendingJobExecutionResponse { "Execution'" :: NullOrUndefined (JobExecution) }
```

##### Instances
``` purescript
Newtype StartNextPendingJobExecutionResponse _
```

#### `StartedAt`

``` purescript
newtype StartedAt
  = StartedAt Number
```

##### Instances
``` purescript
Newtype StartedAt _
```

#### `TerminalStateException`

``` purescript
newtype TerminalStateException
  = TerminalStateException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The job is in a terminal state.</p>

##### Instances
``` purescript
Newtype TerminalStateException _
```

#### `ThingName`

``` purescript
newtype ThingName
  = ThingName String
```

##### Instances
``` purescript
Newtype ThingName _
```

#### `ThrottlingException`

``` purescript
newtype ThrottlingException
  = ThrottlingException { "Message'" :: NullOrUndefined (ErrorMessage') }
```

<p>The rate exceeds the limit.</p>

##### Instances
``` purescript
Newtype ThrottlingException _
```

#### `UpdateJobExecutionRequest`

``` purescript
newtype UpdateJobExecutionRequest
  = UpdateJobExecutionRequest { "JobId'" :: JobId, "ThingName'" :: ThingName, "Status'" :: JobExecutionStatus, "StatusDetails'" :: NullOrUndefined (DetailsMap), "ExpectedVersion'" :: NullOrUndefined (ExpectedVersion), "IncludeJobExecutionState'" :: NullOrUndefined (IncludeExecutionState), "IncludeJobDocument'" :: NullOrUndefined (IncludeJobDocument), "ExecutionNumber'" :: NullOrUndefined (ExecutionNumber) }
```

##### Instances
``` purescript
Newtype UpdateJobExecutionRequest _
```

#### `UpdateJobExecutionResponse`

``` purescript
newtype UpdateJobExecutionResponse
  = UpdateJobExecutionResponse { "ExecutionState'" :: NullOrUndefined (JobExecutionState), "JobDocument'" :: NullOrUndefined (JobDocument) }
```

##### Instances
``` purescript
Newtype UpdateJobExecutionResponse _
```

#### `VersionNumber`

``` purescript
newtype VersionNumber
  = VersionNumber Number
```

##### Instances
``` purescript
Newtype VersionNumber _
```

#### `ErrorMessage'`

``` purescript
newtype ErrorMessage'
  = ErrorMessage' String
```

##### Instances
``` purescript
Newtype ErrorMessage' _
```


