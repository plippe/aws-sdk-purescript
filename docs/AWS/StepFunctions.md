## Module AWS.StepFunctions

<fullname>AWS Step Functions</fullname> <p>AWS Step Functions is a service that lets you coordinate the components of distributed applications and microservices using visual workflows.</p> <p>You can use Step Functions to build applications from individual components, each of which performs a discrete function, or <i>task</i>, allowing you to scale and change applications quickly. Step Functions provides a console that helps visualize the components of your application as a series of steps. Step Functions automatically triggers and tracks each step, and retries steps when there are errors, so your application executes predictably and in the right order every time. Step Functions logs the state of each step, so you can quickly diagnose and debug any issues.</p> <p>Step Functions manages operations and underlying infrastructure to ensure your application is available at any scale. You can run tasks on AWS, your own servers, or any system that has access to AWS. You can access and use Step Functions using the console, the AWS SDKs, or an HTTP API. For more information about Step Functions, see the <i> <a href="http://docs.aws.amazon.com/step-functions/latest/dg/welcome.html">AWS Step Functions Developer Guide</a> </i>.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createActivity`

``` purescript
createActivity :: forall eff. CreateActivityInput -> Aff (err :: RequestError | eff) CreateActivityOutput
```

<p>Creates an activity. An activity is a task which you write in any programming language and host on any machine which has access to AWS Step Functions. Activities must poll Step Functions using the <code>GetActivityTask</code> API action and respond using <code>SendTask*</code> API actions. This function lets Step Functions know the existence of your activity and returns an identifier for use in a state machine and when polling from the activity.</p>

#### `createStateMachine`

``` purescript
createStateMachine :: forall eff. CreateStateMachineInput -> Aff (err :: RequestError | eff) CreateStateMachineOutput
```

<p>Creates a state machine. A state machine consists of a collection of states that can do work (<code>Task</code> states), determine to which states to transition next (<code>Choice</code> states), stop an execution with an error (<code>Fail</code> states), and so on. State machines are specified using a JSON-based, structured language.</p>

#### `deleteActivity`

``` purescript
deleteActivity :: forall eff. DeleteActivityInput -> Aff (err :: RequestError | eff) DeleteActivityOutput
```

<p>Deletes an activity.</p>

#### `deleteStateMachine`

``` purescript
deleteStateMachine :: forall eff. DeleteStateMachineInput -> Aff (err :: RequestError | eff) DeleteStateMachineOutput
```

<p>Deletes a state machine. This is an asynchronous operation: It sets the state machine's status to <code>DELETING</code> and begins the deletion process. Each state machine execution is deleted the next time it makes a state transition.</p> <note> <p>The state machine itself is deleted after all executions are completed or deleted.</p> </note>

#### `describeActivity`

``` purescript
describeActivity :: forall eff. DescribeActivityInput -> Aff (err :: RequestError | eff) DescribeActivityOutput
```

<p>Describes an activity.</p>

#### `describeExecution`

``` purescript
describeExecution :: forall eff. DescribeExecutionInput -> Aff (err :: RequestError | eff) DescribeExecutionOutput
```

<p>Describes an execution.</p>

#### `describeStateMachine`

``` purescript
describeStateMachine :: forall eff. DescribeStateMachineInput -> Aff (err :: RequestError | eff) DescribeStateMachineOutput
```

<p>Describes a state machine.</p>

#### `describeStateMachineForExecution`

``` purescript
describeStateMachineForExecution :: forall eff. DescribeStateMachineForExecutionInput -> Aff (err :: RequestError | eff) DescribeStateMachineForExecutionOutput
```

<p>Describes the state machine associated with a specific execution.</p>

#### `getActivityTask`

``` purescript
getActivityTask :: forall eff. GetActivityTaskInput -> Aff (err :: RequestError | eff) GetActivityTaskOutput
```

<p>Used by workers to retrieve a task (with the specified activity ARN) which has been scheduled for execution by a running state machine. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available (i.e. an execution of a task of this type is needed.) The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns a <code>taskToken</code> with a null string.</p> <important> <p>Workers should set their client side socket timeout to at least 65 seconds (5 seconds higher than the maximum time the service may hold the poll request).</p> </important>

#### `getExecutionHistory`

``` purescript
getExecutionHistory :: forall eff. GetExecutionHistoryInput -> Aff (err :: RequestError | eff) GetExecutionHistoryOutput
```

<p>Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the <code>timeStamp</code> of the events. Use the <code>reverseOrder</code> parameter to get the latest events first.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>

#### `listActivities`

``` purescript
listActivities :: forall eff. ListActivitiesInput -> Aff (err :: RequestError | eff) ListActivitiesOutput
```

<p>Lists the existing activities.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>

#### `listExecutions`

``` purescript
listExecutions :: forall eff. ListExecutionsInput -> Aff (err :: RequestError | eff) ListExecutionsOutput
```

<p>Lists the executions of a state machine that meet the filtering criteria.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>

#### `listStateMachines`

``` purescript
listStateMachines :: forall eff. ListStateMachinesInput -> Aff (err :: RequestError | eff) ListStateMachinesOutput
```

<p>Lists the existing state machines.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>

#### `sendTaskFailure`

``` purescript
sendTaskFailure :: forall eff. SendTaskFailureInput -> Aff (err :: RequestError | eff) SendTaskFailureOutput
```

<p>Used by workers to report that the task identified by the <code>taskToken</code> failed.</p>

#### `sendTaskHeartbeat`

``` purescript
sendTaskHeartbeat :: forall eff. SendTaskHeartbeatInput -> Aff (err :: RequestError | eff) SendTaskHeartbeatOutput
```

<p>Used by workers to report to the service that the task represented by the specified <code>taskToken</code> is still making progress. This action resets the <code>Heartbeat</code> clock. The <code>Heartbeat</code> threshold is specified in the state machine's Amazon States Language definition. This action does not in itself create an event in the execution history. However, if the task times out, the execution history contains an <code>ActivityTimedOut</code> event.</p> <note> <p>The <code>Timeout</code> of a task, defined in the state machine's Amazon States Language definition, is its maximum allowed duration, regardless of the number of <a>SendTaskHeartbeat</a> requests received.</p> </note> <note> <p>This operation is only useful for long-lived tasks to report the liveliness of the task.</p> </note>

#### `sendTaskSuccess`

``` purescript
sendTaskSuccess :: forall eff. SendTaskSuccessInput -> Aff (err :: RequestError | eff) SendTaskSuccessOutput
```

<p>Used by workers to report that the task identified by the <code>taskToken</code> completed successfully.</p>

#### `startExecution`

``` purescript
startExecution :: forall eff. StartExecutionInput -> Aff (err :: RequestError | eff) StartExecutionOutput
```

<p>Starts a state machine execution.</p>

#### `stopExecution`

``` purescript
stopExecution :: forall eff. StopExecutionInput -> Aff (err :: RequestError | eff) StopExecutionOutput
```

<p>Stops an execution.</p>

#### `updateStateMachine`

``` purescript
updateStateMachine :: forall eff. UpdateStateMachineInput -> Aff (err :: RequestError | eff) UpdateStateMachineOutput
```

<p>Updates an existing state machine by modifying its <code>definition</code> and/or <code>roleArn</code>. Running executions will continue to use the previous <code>definition</code> and <code>roleArn</code>.</p> <note> <p>All <code>StartExecution</code> calls within a few seconds will use the updated <code>definition</code> and <code>roleArn</code>. Executions started immediately after calling <code>UpdateStateMachine</code> may use the previous state machine <code>definition</code> and <code>roleArn</code>. You must include at least one of <code>definition</code> or <code>roleArn</code> or you will receive a <code>MissingRequiredParameter</code> error.</p> </note>

#### `ActivityDoesNotExist`

``` purescript
newtype ActivityDoesNotExist
  = ActivityDoesNotExist { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified activity does not exist.</p>

#### `ActivityFailedEventDetails`

``` purescript
newtype ActivityFailedEventDetails
  = ActivityFailedEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about an activity which failed during an execution.</p>

#### `ActivityLimitExceeded`

``` purescript
newtype ActivityLimitExceeded
  = ActivityLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The maximum number of activities has been reached. Existing activities must be deleted before a new activity can be created.</p>

#### `ActivityList`

``` purescript
newtype ActivityList
  = ActivityList (Array ActivityListItem)
```

#### `ActivityListItem`

``` purescript
newtype ActivityListItem
  = ActivityListItem { "ActivityArn'" :: Arn, "Name'" :: Name, "CreationDate'" :: Number }
```

<p>Contains details about an activity.</p>

#### `ActivityScheduleFailedEventDetails`

``` purescript
newtype ActivityScheduleFailedEventDetails
  = ActivityScheduleFailedEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about an activity schedule failure which occurred during an execution.</p>

#### `ActivityScheduledEventDetails`

``` purescript
newtype ActivityScheduledEventDetails
  = ActivityScheduledEventDetails { "Resource'" :: Arn, "Input'" :: NullOrUndefined (Data), "TimeoutInSeconds'" :: NullOrUndefined (TimeoutInSeconds), "HeartbeatInSeconds'" :: NullOrUndefined (TimeoutInSeconds) }
```

<p>Contains details about an activity scheduled during an execution.</p>

#### `ActivityStartedEventDetails`

``` purescript
newtype ActivityStartedEventDetails
  = ActivityStartedEventDetails { "WorkerName'" :: NullOrUndefined (Identity) }
```

<p>Contains details about the start of an activity during an execution.</p>

#### `ActivitySucceededEventDetails`

``` purescript
newtype ActivitySucceededEventDetails
  = ActivitySucceededEventDetails { "Output'" :: NullOrUndefined (Data) }
```

<p>Contains details about an activity which successfully terminated during an execution.</p>

#### `ActivityTimedOutEventDetails`

``` purescript
newtype ActivityTimedOutEventDetails
  = ActivityTimedOutEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about an activity timeout which occurred during an execution.</p>

#### `ActivityWorkerLimitExceeded`

``` purescript
newtype ActivityWorkerLimitExceeded
  = ActivityWorkerLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The maximum number of workers concurrently polling for activity tasks has been reached.</p>

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

#### `Cause`

``` purescript
newtype Cause
  = Cause String
```

#### `CreateActivityInput`

``` purescript
newtype CreateActivityInput
  = CreateActivityInput { "Name'" :: Name }
```

#### `CreateActivityOutput`

``` purescript
newtype CreateActivityOutput
  = CreateActivityOutput { "ActivityArn'" :: Arn, "CreationDate'" :: Number }
```

#### `CreateStateMachineInput`

``` purescript
newtype CreateStateMachineInput
  = CreateStateMachineInput { "Name'" :: Name, "Definition'" :: Definition, "RoleArn'" :: Arn }
```

#### `CreateStateMachineOutput`

``` purescript
newtype CreateStateMachineOutput
  = CreateStateMachineOutput { "StateMachineArn'" :: Arn, "CreationDate'" :: Number }
```

#### `Data`

``` purescript
newtype Data
  = Data String
```

#### `Definition`

``` purescript
newtype Definition
  = Definition String
```

#### `DeleteActivityInput`

``` purescript
newtype DeleteActivityInput
  = DeleteActivityInput { "ActivityArn'" :: Arn }
```

#### `DeleteActivityOutput`

``` purescript
newtype DeleteActivityOutput
  = DeleteActivityOutput {  }
```

#### `DeleteStateMachineInput`

``` purescript
newtype DeleteStateMachineInput
  = DeleteStateMachineInput { "StateMachineArn'" :: Arn }
```

#### `DeleteStateMachineOutput`

``` purescript
newtype DeleteStateMachineOutput
  = DeleteStateMachineOutput {  }
```

#### `DescribeActivityInput`

``` purescript
newtype DescribeActivityInput
  = DescribeActivityInput { "ActivityArn'" :: Arn }
```

#### `DescribeActivityOutput`

``` purescript
newtype DescribeActivityOutput
  = DescribeActivityOutput { "ActivityArn'" :: Arn, "Name'" :: Name, "CreationDate'" :: Number }
```

#### `DescribeExecutionInput`

``` purescript
newtype DescribeExecutionInput
  = DescribeExecutionInput { "ExecutionArn'" :: Arn }
```

#### `DescribeExecutionOutput`

``` purescript
newtype DescribeExecutionOutput
  = DescribeExecutionOutput { "ExecutionArn'" :: Arn, "StateMachineArn'" :: Arn, "Name'" :: NullOrUndefined (Name), "Status'" :: ExecutionStatus, "StartDate'" :: Number, "StopDate'" :: NullOrUndefined (Number), "Input'" :: Data, "Output'" :: NullOrUndefined (Data) }
```

#### `DescribeStateMachineForExecutionInput`

``` purescript
newtype DescribeStateMachineForExecutionInput
  = DescribeStateMachineForExecutionInput { "ExecutionArn'" :: Arn }
```

#### `DescribeStateMachineForExecutionOutput`

``` purescript
newtype DescribeStateMachineForExecutionOutput
  = DescribeStateMachineForExecutionOutput { "StateMachineArn'" :: Arn, "Name'" :: Name, "Definition'" :: Definition, "RoleArn'" :: Arn, "UpdateDate'" :: Number }
```

#### `DescribeStateMachineInput`

``` purescript
newtype DescribeStateMachineInput
  = DescribeStateMachineInput { "StateMachineArn'" :: Arn }
```

#### `DescribeStateMachineOutput`

``` purescript
newtype DescribeStateMachineOutput
  = DescribeStateMachineOutput { "StateMachineArn'" :: Arn, "Name'" :: Name, "Status'" :: NullOrUndefined (StateMachineStatus), "Definition'" :: Definition, "RoleArn'" :: Arn, "CreationDate'" :: Number }
```

#### `Error`

``` purescript
newtype Error
  = Error String
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

#### `EventId`

``` purescript
newtype EventId
  = EventId Number
```

#### `ExecutionAbortedEventDetails`

``` purescript
newtype ExecutionAbortedEventDetails
  = ExecutionAbortedEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about an abort of an execution.</p>

#### `ExecutionAlreadyExists`

``` purescript
newtype ExecutionAlreadyExists
  = ExecutionAlreadyExists { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The execution has the same <code>name</code> as another execution (but a different <code>input</code>).</p> <note> <p>Executions with the same <code>name</code> and <code>input</code> are considered idempotent.</p> </note>

#### `ExecutionDoesNotExist`

``` purescript
newtype ExecutionDoesNotExist
  = ExecutionDoesNotExist { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified execution does not exist.</p>

#### `ExecutionFailedEventDetails`

``` purescript
newtype ExecutionFailedEventDetails
  = ExecutionFailedEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about an execution failure event.</p>

#### `ExecutionLimitExceeded`

``` purescript
newtype ExecutionLimitExceeded
  = ExecutionLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The maximum number of running executions has been reached. Running executions must end or be stopped before a new execution can be started.</p>

#### `ExecutionList`

``` purescript
newtype ExecutionList
  = ExecutionList (Array ExecutionListItem)
```

#### `ExecutionListItem`

``` purescript
newtype ExecutionListItem
  = ExecutionListItem { "ExecutionArn'" :: Arn, "StateMachineArn'" :: Arn, "Name'" :: Name, "Status'" :: ExecutionStatus, "StartDate'" :: Number, "StopDate'" :: NullOrUndefined (Number) }
```

<p>Contains details about an execution.</p>

#### `ExecutionStartedEventDetails`

``` purescript
newtype ExecutionStartedEventDetails
  = ExecutionStartedEventDetails { "Input'" :: NullOrUndefined (Data), "RoleArn'" :: NullOrUndefined (Arn) }
```

<p>Contains details about the start of the execution.</p>

#### `ExecutionStatus`

``` purescript
newtype ExecutionStatus
  = ExecutionStatus String
```

#### `ExecutionSucceededEventDetails`

``` purescript
newtype ExecutionSucceededEventDetails
  = ExecutionSucceededEventDetails { "Output'" :: NullOrUndefined (Data) }
```

<p>Contains details about the successful termination of the execution.</p>

#### `ExecutionTimedOutEventDetails`

``` purescript
newtype ExecutionTimedOutEventDetails
  = ExecutionTimedOutEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about the execution timeout which occurred during the execution.</p>

#### `GetActivityTaskInput`

``` purescript
newtype GetActivityTaskInput
  = GetActivityTaskInput { "ActivityArn'" :: Arn, "WorkerName'" :: NullOrUndefined (Name) }
```

#### `GetActivityTaskOutput`

``` purescript
newtype GetActivityTaskOutput
  = GetActivityTaskOutput { "TaskToken'" :: NullOrUndefined (TaskToken), "Input'" :: NullOrUndefined (Data) }
```

#### `GetExecutionHistoryInput`

``` purescript
newtype GetExecutionHistoryInput
  = GetExecutionHistoryInput { "ExecutionArn'" :: Arn, "MaxResults'" :: NullOrUndefined (PageSize), "ReverseOrder'" :: NullOrUndefined (ReverseOrder), "NextToken'" :: NullOrUndefined (PageToken) }
```

#### `GetExecutionHistoryOutput`

``` purescript
newtype GetExecutionHistoryOutput
  = GetExecutionHistoryOutput { "Events'" :: HistoryEventList, "NextToken'" :: NullOrUndefined (PageToken) }
```

#### `HistoryEvent`

``` purescript
newtype HistoryEvent
  = HistoryEvent { "Number" :: Number, "Type'" :: HistoryEventType, "Id'" :: EventId, "PreviousEventId'" :: NullOrUndefined (EventId), "ActivityFailedEventDetails'" :: NullOrUndefined (ActivityFailedEventDetails), "ActivityScheduleFailedEventDetails'" :: NullOrUndefined (ActivityScheduleFailedEventDetails), "ActivityScheduledEventDetails'" :: NullOrUndefined (ActivityScheduledEventDetails), "ActivityStartedEventDetails'" :: NullOrUndefined (ActivityStartedEventDetails), "ActivitySucceededEventDetails'" :: NullOrUndefined (ActivitySucceededEventDetails), "ActivityTimedOutEventDetails'" :: NullOrUndefined (ActivityTimedOutEventDetails), "ExecutionFailedEventDetails'" :: NullOrUndefined (ExecutionFailedEventDetails), "ExecutionStartedEventDetails'" :: NullOrUndefined (ExecutionStartedEventDetails), "ExecutionSucceededEventDetails'" :: NullOrUndefined (ExecutionSucceededEventDetails), "ExecutionAbortedEventDetails'" :: NullOrUndefined (ExecutionAbortedEventDetails), "ExecutionTimedOutEventDetails'" :: NullOrUndefined (ExecutionTimedOutEventDetails), "LambdaFunctionFailedEventDetails'" :: NullOrUndefined (LambdaFunctionFailedEventDetails), "LambdaFunctionScheduleFailedEventDetails'" :: NullOrUndefined (LambdaFunctionScheduleFailedEventDetails), "LambdaFunctionScheduledEventDetails'" :: NullOrUndefined (LambdaFunctionScheduledEventDetails), "LambdaFunctionStartFailedEventDetails'" :: NullOrUndefined (LambdaFunctionStartFailedEventDetails), "LambdaFunctionSucceededEventDetails'" :: NullOrUndefined (LambdaFunctionSucceededEventDetails), "LambdaFunctionTimedOutEventDetails'" :: NullOrUndefined (LambdaFunctionTimedOutEventDetails), "StateEnteredEventDetails'" :: NullOrUndefined (StateEnteredEventDetails), "StateExitedEventDetails'" :: NullOrUndefined (StateExitedEventDetails) }
```

<p>Contains details about the events of an execution.</p>

#### `HistoryEventList`

``` purescript
newtype HistoryEventList
  = HistoryEventList (Array HistoryEvent)
```

<p>Contains details about the events which occurred during an execution.</p>

#### `HistoryEventType`

``` purescript
newtype HistoryEventType
  = HistoryEventType String
```

#### `Identity`

``` purescript
newtype Identity
  = Identity String
```

#### `InvalidArn`

``` purescript
newtype InvalidArn
  = InvalidArn { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The provided Amazon Resource Name (ARN) is invalid.</p>

#### `InvalidDefinition`

``` purescript
newtype InvalidDefinition
  = InvalidDefinition { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The provided Amazon States Language definition is invalid.</p>

#### `InvalidExecutionInput`

``` purescript
newtype InvalidExecutionInput
  = InvalidExecutionInput { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The provided JSON input data is invalid.</p>

#### `InvalidName`

``` purescript
newtype InvalidName
  = InvalidName { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The provided name is invalid.</p>

#### `InvalidOutput`

``` purescript
newtype InvalidOutput
  = InvalidOutput { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The provided JSON output data is invalid.</p>

#### `InvalidToken`

``` purescript
newtype InvalidToken
  = InvalidToken { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The provided token is invalid.</p>

#### `LambdaFunctionFailedEventDetails`

``` purescript
newtype LambdaFunctionFailedEventDetails
  = LambdaFunctionFailedEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about a lambda function which failed during an execution.</p>

#### `LambdaFunctionScheduleFailedEventDetails`

``` purescript
newtype LambdaFunctionScheduleFailedEventDetails
  = LambdaFunctionScheduleFailedEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about a failed lambda function schedule event which occurred during an execution.</p>

#### `LambdaFunctionScheduledEventDetails`

``` purescript
newtype LambdaFunctionScheduledEventDetails
  = LambdaFunctionScheduledEventDetails { "Resource'" :: Arn, "Input'" :: NullOrUndefined (Data), "TimeoutInSeconds'" :: NullOrUndefined (TimeoutInSeconds) }
```

<p>Contains details about a lambda function scheduled during an execution.</p>

#### `LambdaFunctionStartFailedEventDetails`

``` purescript
newtype LambdaFunctionStartFailedEventDetails
  = LambdaFunctionStartFailedEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about a lambda function which failed to start during an execution.</p>

#### `LambdaFunctionSucceededEventDetails`

``` purescript
newtype LambdaFunctionSucceededEventDetails
  = LambdaFunctionSucceededEventDetails { "Output'" :: NullOrUndefined (Data) }
```

<p>Contains details about a lambda function which successfully terminated during an execution.</p>

#### `LambdaFunctionTimedOutEventDetails`

``` purescript
newtype LambdaFunctionTimedOutEventDetails
  = LambdaFunctionTimedOutEventDetails { "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

<p>Contains details about a lambda function timeout which occurred during an execution.</p>

#### `ListActivitiesInput`

``` purescript
newtype ListActivitiesInput
  = ListActivitiesInput { "MaxResults'" :: NullOrUndefined (PageSize), "NextToken'" :: NullOrUndefined (PageToken) }
```

#### `ListActivitiesOutput`

``` purescript
newtype ListActivitiesOutput
  = ListActivitiesOutput { "Activities'" :: ActivityList, "NextToken'" :: NullOrUndefined (PageToken) }
```

#### `ListExecutionsInput`

``` purescript
newtype ListExecutionsInput
  = ListExecutionsInput { "StateMachineArn'" :: Arn, "StatusFilter'" :: NullOrUndefined (ExecutionStatus), "MaxResults'" :: NullOrUndefined (PageSize), "NextToken'" :: NullOrUndefined (PageToken) }
```

#### `ListExecutionsOutput`

``` purescript
newtype ListExecutionsOutput
  = ListExecutionsOutput { "Executions'" :: ExecutionList, "NextToken'" :: NullOrUndefined (PageToken) }
```

#### `ListStateMachinesInput`

``` purescript
newtype ListStateMachinesInput
  = ListStateMachinesInput { "MaxResults'" :: NullOrUndefined (PageSize), "NextToken'" :: NullOrUndefined (PageToken) }
```

#### `ListStateMachinesOutput`

``` purescript
newtype ListStateMachinesOutput
  = ListStateMachinesOutput { "StateMachines'" :: StateMachineList, "NextToken'" :: NullOrUndefined (PageToken) }
```

#### `MissingRequiredParameter`

``` purescript
newtype MissingRequiredParameter
  = MissingRequiredParameter { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>Request is missing a required parameter. This error occurs if both <code>definition</code> and <code>roleArn</code> are not specified.</p>

#### `Name`

``` purescript
newtype Name
  = Name String
```

#### `PageSize`

``` purescript
newtype PageSize
  = PageSize Int
```

#### `PageToken`

``` purescript
newtype PageToken
  = PageToken String
```

#### `ReverseOrder`

``` purescript
newtype ReverseOrder
  = ReverseOrder Boolean
```

#### `SendTaskFailureInput`

``` purescript
newtype SendTaskFailureInput
  = SendTaskFailureInput { "TaskToken'" :: TaskToken, "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

#### `SendTaskFailureOutput`

``` purescript
newtype SendTaskFailureOutput
  = SendTaskFailureOutput {  }
```

#### `SendTaskHeartbeatInput`

``` purescript
newtype SendTaskHeartbeatInput
  = SendTaskHeartbeatInput { "TaskToken'" :: TaskToken }
```

#### `SendTaskHeartbeatOutput`

``` purescript
newtype SendTaskHeartbeatOutput
  = SendTaskHeartbeatOutput {  }
```

#### `SendTaskSuccessInput`

``` purescript
newtype SendTaskSuccessInput
  = SendTaskSuccessInput { "TaskToken'" :: TaskToken, "Output'" :: Data }
```

#### `SendTaskSuccessOutput`

``` purescript
newtype SendTaskSuccessOutput
  = SendTaskSuccessOutput {  }
```

#### `StartExecutionInput`

``` purescript
newtype StartExecutionInput
  = StartExecutionInput { "StateMachineArn'" :: Arn, "Name'" :: NullOrUndefined (Name), "Input'" :: NullOrUndefined (Data) }
```

#### `StartExecutionOutput`

``` purescript
newtype StartExecutionOutput
  = StartExecutionOutput { "ExecutionArn'" :: Arn, "StartDate'" :: Number }
```

#### `StateEnteredEventDetails`

``` purescript
newtype StateEnteredEventDetails
  = StateEnteredEventDetails { "Name'" :: Name, "Input'" :: NullOrUndefined (Data) }
```

<p>Contains details about a state entered during an execution.</p>

#### `StateExitedEventDetails`

``` purescript
newtype StateExitedEventDetails
  = StateExitedEventDetails { "Name'" :: Name, "Output'" :: NullOrUndefined (Data) }
```

<p>Contains details about an exit from a state during an execution.</p>

#### `StateMachineAlreadyExists`

``` purescript
newtype StateMachineAlreadyExists
  = StateMachineAlreadyExists { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>A state machine with the same name but a different definition or role ARN already exists.</p>

#### `StateMachineDeleting`

``` purescript
newtype StateMachineDeleting
  = StateMachineDeleting { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified state machine is being deleted.</p>

#### `StateMachineDoesNotExist`

``` purescript
newtype StateMachineDoesNotExist
  = StateMachineDoesNotExist { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified state machine does not exist.</p>

#### `StateMachineLimitExceeded`

``` purescript
newtype StateMachineLimitExceeded
  = StateMachineLimitExceeded { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p>The maximum number of state machines has been reached. Existing state machines must be deleted before a new state machine can be created.</p>

#### `StateMachineList`

``` purescript
newtype StateMachineList
  = StateMachineList (Array StateMachineListItem)
```

#### `StateMachineListItem`

``` purescript
newtype StateMachineListItem
  = StateMachineListItem { "StateMachineArn'" :: Arn, "Name'" :: Name, "CreationDate'" :: Number }
```

<p>Contains details about the state machine.</p>

#### `StateMachineStatus`

``` purescript
newtype StateMachineStatus
  = StateMachineStatus String
```

#### `StopExecutionInput`

``` purescript
newtype StopExecutionInput
  = StopExecutionInput { "ExecutionArn'" :: Arn, "Error'" :: NullOrUndefined (Error), "Cause'" :: NullOrUndefined (Cause) }
```

#### `StopExecutionOutput`

``` purescript
newtype StopExecutionOutput
  = StopExecutionOutput { "StopDate'" :: Number }
```

#### `TaskDoesNotExist`

``` purescript
newtype TaskDoesNotExist
  = TaskDoesNotExist { "Message'" :: NullOrUndefined (ErrorMessage) }
```

#### `TaskTimedOut`

``` purescript
newtype TaskTimedOut
  = TaskTimedOut { "Message'" :: NullOrUndefined (ErrorMessage) }
```

#### `TaskToken`

``` purescript
newtype TaskToken
  = TaskToken String
```

#### `TimeoutInSeconds`

``` purescript
newtype TimeoutInSeconds
  = TimeoutInSeconds Number
```

#### `UpdateStateMachineInput`

``` purescript
newtype UpdateStateMachineInput
  = UpdateStateMachineInput { "StateMachineArn'" :: Arn, "Definition'" :: NullOrUndefined (Definition), "RoleArn'" :: NullOrUndefined (Arn) }
```

#### `UpdateStateMachineOutput`

``` purescript
newtype UpdateStateMachineOutput
  = UpdateStateMachineOutput { "UpdateDate'" :: Number }
```


