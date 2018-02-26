

-- | <fullname>AWS Step Functions</fullname> <p>AWS Step Functions is a service that lets you coordinate the components of distributed applications and microservices using visual workflows.</p> <p>You can use Step Functions to build applications from individual components, each of which performs a discrete function, or <i>task</i>, allowing you to scale and change applications quickly. Step Functions provides a console that helps visualize the components of your application as a series of steps. Step Functions automatically triggers and tracks each step, and retries steps when there are errors, so your application executes predictably and in the right order every time. Step Functions logs the state of each step, so you can quickly diagnose and debug any issues.</p> <p>Step Functions manages operations and underlying infrastructure to ensure your application is available at any scale. You can run tasks on AWS, your own servers, or any system that has access to AWS. You can access and use Step Functions using the console, the AWS SDKs, or an HTTP API. For more information about Step Functions, see the <i> <a href="http://docs.aws.amazon.com/step-functions/latest/dg/welcome.html">AWS Step Functions Developer Guide</a> </i>.</p>
module AWS.StepFunctions where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "StepFunctions" :: String


-- | <p>Creates an activity. An activity is a task which you write in any programming language and host on any machine which has access to AWS Step Functions. Activities must poll Step Functions using the <code>GetActivityTask</code> API action and respond using <code>SendTask*</code> API actions. This function lets Step Functions know the existence of your activity and returns an identifier for use in a state machine and when polling from the activity.</p>
createActivity :: forall eff. CreateActivityInput -> Aff (err :: AWS.RequestError | eff) CreateActivityOutput
createActivity = AWS.request serviceName "CreateActivity" 


-- | <p>Creates a state machine. A state machine consists of a collection of states that can do work (<code>Task</code> states), determine to which states to transition next (<code>Choice</code> states), stop an execution with an error (<code>Fail</code> states), and so on. State machines are specified using a JSON-based, structured language.</p>
createStateMachine :: forall eff. CreateStateMachineInput -> Aff (err :: AWS.RequestError | eff) CreateStateMachineOutput
createStateMachine = AWS.request serviceName "CreateStateMachine" 


-- | <p>Deletes an activity.</p>
deleteActivity :: forall eff. DeleteActivityInput -> Aff (err :: AWS.RequestError | eff) DeleteActivityOutput
deleteActivity = AWS.request serviceName "DeleteActivity" 


-- | <p>Deletes a state machine. This is an asynchronous operation: It sets the state machine's status to <code>DELETING</code> and begins the deletion process. Each state machine execution is deleted the next time it makes a state transition.</p> <note> <p>The state machine itself is deleted after all executions are completed or deleted.</p> </note>
deleteStateMachine :: forall eff. DeleteStateMachineInput -> Aff (err :: AWS.RequestError | eff) DeleteStateMachineOutput
deleteStateMachine = AWS.request serviceName "DeleteStateMachine" 


-- | <p>Describes an activity.</p>
describeActivity :: forall eff. DescribeActivityInput -> Aff (err :: AWS.RequestError | eff) DescribeActivityOutput
describeActivity = AWS.request serviceName "DescribeActivity" 


-- | <p>Describes an execution.</p>
describeExecution :: forall eff. DescribeExecutionInput -> Aff (err :: AWS.RequestError | eff) DescribeExecutionOutput
describeExecution = AWS.request serviceName "DescribeExecution" 


-- | <p>Describes a state machine.</p>
describeStateMachine :: forall eff. DescribeStateMachineInput -> Aff (err :: AWS.RequestError | eff) DescribeStateMachineOutput
describeStateMachine = AWS.request serviceName "DescribeStateMachine" 


-- | <p>Describes the state machine associated with a specific execution.</p>
describeStateMachineForExecution :: forall eff. DescribeStateMachineForExecutionInput -> Aff (err :: AWS.RequestError | eff) DescribeStateMachineForExecutionOutput
describeStateMachineForExecution = AWS.request serviceName "DescribeStateMachineForExecution" 


-- | <p>Used by workers to retrieve a task (with the specified activity ARN) which has been scheduled for execution by a running state machine. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available (i.e. an execution of a task of this type is needed.) The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns a <code>taskToken</code> with a null string.</p> <important> <p>Workers should set their client side socket timeout to at least 65 seconds (5 seconds higher than the maximum time the service may hold the poll request).</p> </important>
getActivityTask :: forall eff. GetActivityTaskInput -> Aff (err :: AWS.RequestError | eff) GetActivityTaskOutput
getActivityTask = AWS.request serviceName "GetActivityTask" 


-- | <p>Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the <code>timeStamp</code> of the events. Use the <code>reverseOrder</code> parameter to get the latest events first.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
getExecutionHistory :: forall eff. GetExecutionHistoryInput -> Aff (err :: AWS.RequestError | eff) GetExecutionHistoryOutput
getExecutionHistory = AWS.request serviceName "GetExecutionHistory" 


-- | <p>Lists the existing activities.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listActivities :: forall eff. ListActivitiesInput -> Aff (err :: AWS.RequestError | eff) ListActivitiesOutput
listActivities = AWS.request serviceName "ListActivities" 


-- | <p>Lists the executions of a state machine that meet the filtering criteria.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listExecutions :: forall eff. ListExecutionsInput -> Aff (err :: AWS.RequestError | eff) ListExecutionsOutput
listExecutions = AWS.request serviceName "ListExecutions" 


-- | <p>Lists the existing state machines.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listStateMachines :: forall eff. ListStateMachinesInput -> Aff (err :: AWS.RequestError | eff) ListStateMachinesOutput
listStateMachines = AWS.request serviceName "ListStateMachines" 


-- | <p>Used by workers to report that the task identified by the <code>taskToken</code> failed.</p>
sendTaskFailure :: forall eff. SendTaskFailureInput -> Aff (err :: AWS.RequestError | eff) SendTaskFailureOutput
sendTaskFailure = AWS.request serviceName "SendTaskFailure" 


-- | <p>Used by workers to report to the service that the task represented by the specified <code>taskToken</code> is still making progress. This action resets the <code>Heartbeat</code> clock. The <code>Heartbeat</code> threshold is specified in the state machine's Amazon States Language definition. This action does not in itself create an event in the execution history. However, if the task times out, the execution history contains an <code>ActivityTimedOut</code> event.</p> <note> <p>The <code>Timeout</code> of a task, defined in the state machine's Amazon States Language definition, is its maximum allowed duration, regardless of the number of <a>SendTaskHeartbeat</a> requests received.</p> </note> <note> <p>This operation is only useful for long-lived tasks to report the liveliness of the task.</p> </note>
sendTaskHeartbeat :: forall eff. SendTaskHeartbeatInput -> Aff (err :: AWS.RequestError | eff) SendTaskHeartbeatOutput
sendTaskHeartbeat = AWS.request serviceName "SendTaskHeartbeat" 


-- | <p>Used by workers to report that the task identified by the <code>taskToken</code> completed successfully.</p>
sendTaskSuccess :: forall eff. SendTaskSuccessInput -> Aff (err :: AWS.RequestError | eff) SendTaskSuccessOutput
sendTaskSuccess = AWS.request serviceName "SendTaskSuccess" 


-- | <p>Starts a state machine execution.</p>
startExecution :: forall eff. StartExecutionInput -> Aff (err :: AWS.RequestError | eff) StartExecutionOutput
startExecution = AWS.request serviceName "StartExecution" 


-- | <p>Stops an execution.</p>
stopExecution :: forall eff. StopExecutionInput -> Aff (err :: AWS.RequestError | eff) StopExecutionOutput
stopExecution = AWS.request serviceName "StopExecution" 


-- | <p>Updates an existing state machine by modifying its <code>definition</code> and/or <code>roleArn</code>. Running executions will continue to use the previous <code>definition</code> and <code>roleArn</code>.</p> <note> <p>All <code>StartExecution</code> calls within a few seconds will use the updated <code>definition</code> and <code>roleArn</code>. Executions started immediately after calling <code>UpdateStateMachine</code> may use the previous state machine <code>definition</code> and <code>roleArn</code>. You must include at least one of <code>definition</code> or <code>roleArn</code> or you will receive a <code>MissingRequiredParameter</code> error.</p> </note>
updateStateMachine :: forall eff. UpdateStateMachineInput -> Aff (err :: AWS.RequestError | eff) UpdateStateMachineOutput
updateStateMachine = AWS.request serviceName "UpdateStateMachine" 


-- | <p>The specified activity does not exist.</p>
newtype ActivityDoesNotExist = ActivityDoesNotExist 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Contains details about an activity which failed during an execution.</p>
newtype ActivityFailedEventDetails = ActivityFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


-- | <p>The maximum number of activities has been reached. Existing activities must be deleted before a new activity can be created.</p>
newtype ActivityLimitExceeded = ActivityLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype ActivityList = ActivityList (Array ActivityListItem)


-- | <p>Contains details about an activity.</p>
newtype ActivityListItem = ActivityListItem 
  { "ActivityArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }


-- | <p>Contains details about an activity schedule failure which occurred during an execution.</p>
newtype ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


-- | <p>Contains details about an activity scheduled during an execution.</p>
newtype ActivityScheduledEventDetails = ActivityScheduledEventDetails 
  { "Resource'" :: (Arn)
  , "Input'" :: NullOrUndefined (Data)
  , "TimeoutInSeconds'" :: NullOrUndefined (TimeoutInSeconds)
  , "HeartbeatInSeconds'" :: NullOrUndefined (TimeoutInSeconds)
  }


-- | <p>Contains details about the start of an activity during an execution.</p>
newtype ActivityStartedEventDetails = ActivityStartedEventDetails 
  { "WorkerName'" :: NullOrUndefined (Identity)
  }


-- | <p>Contains details about an activity which successfully terminated during an execution.</p>
newtype ActivitySucceededEventDetails = ActivitySucceededEventDetails 
  { "Output'" :: NullOrUndefined (Data)
  }


-- | <p>Contains details about an activity timeout which occurred during an execution.</p>
newtype ActivityTimedOutEventDetails = ActivityTimedOutEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


-- | <p>The maximum number of workers concurrently polling for activity tasks has been reached.</p>
newtype ActivityWorkerLimitExceeded = ActivityWorkerLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype Arn = Arn String


newtype Cause = Cause String


newtype CreateActivityInput = CreateActivityInput 
  { "Name'" :: (Name)
  }


newtype CreateActivityOutput = CreateActivityOutput 
  { "ActivityArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }


newtype CreateStateMachineInput = CreateStateMachineInput 
  { "Name'" :: (Name)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  }


newtype CreateStateMachineOutput = CreateStateMachineOutput 
  { "StateMachineArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }


newtype Data = Data String


newtype Definition = Definition String


newtype DeleteActivityInput = DeleteActivityInput 
  { "ActivityArn'" :: (Arn)
  }


newtype DeleteActivityOutput = DeleteActivityOutput 
  { 
  }


newtype DeleteStateMachineInput = DeleteStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  }


newtype DeleteStateMachineOutput = DeleteStateMachineOutput 
  { 
  }


newtype DescribeActivityInput = DescribeActivityInput 
  { "ActivityArn'" :: (Arn)
  }


newtype DescribeActivityOutput = DescribeActivityOutput 
  { "ActivityArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }


newtype DescribeExecutionInput = DescribeExecutionInput 
  { "ExecutionArn'" :: (Arn)
  }


newtype DescribeExecutionOutput = DescribeExecutionOutput 
  { "ExecutionArn'" :: (Arn)
  , "StateMachineArn'" :: (Arn)
  , "Name'" :: NullOrUndefined (Name)
  , "Status'" :: (ExecutionStatus)
  , "StartDate'" :: (Number)
  , "StopDate'" :: NullOrUndefined (Number)
  , "Input'" :: (Data)
  , "Output'" :: NullOrUndefined (Data)
  }


newtype DescribeStateMachineForExecutionInput = DescribeStateMachineForExecutionInput 
  { "ExecutionArn'" :: (Arn)
  }


newtype DescribeStateMachineForExecutionOutput = DescribeStateMachineForExecutionOutput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  , "UpdateDate'" :: (Number)
  }


newtype DescribeStateMachineInput = DescribeStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  }


newtype DescribeStateMachineOutput = DescribeStateMachineOutput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Status'" :: NullOrUndefined (StateMachineStatus)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }


newtype Error = Error String


newtype ErrorMessage = ErrorMessage String


newtype EventId = EventId Number


-- | <p>Contains details about an abort of an execution.</p>
newtype ExecutionAbortedEventDetails = ExecutionAbortedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


-- | <p>The execution has the same <code>name</code> as another execution (but a different <code>input</code>).</p> <note> <p>Executions with the same <code>name</code> and <code>input</code> are considered idempotent.</p> </note>
newtype ExecutionAlreadyExists = ExecutionAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified execution does not exist.</p>
newtype ExecutionDoesNotExist = ExecutionDoesNotExist 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Contains details about an execution failure event.</p>
newtype ExecutionFailedEventDetails = ExecutionFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


-- | <p>The maximum number of running executions has been reached. Running executions must end or be stopped before a new execution can be started.</p>
newtype ExecutionLimitExceeded = ExecutionLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype ExecutionList = ExecutionList (Array ExecutionListItem)


-- | <p>Contains details about an execution.</p>
newtype ExecutionListItem = ExecutionListItem 
  { "ExecutionArn'" :: (Arn)
  , "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Status'" :: (ExecutionStatus)
  , "StartDate'" :: (Number)
  , "StopDate'" :: NullOrUndefined (Number)
  }


-- | <p>Contains details about the start of the execution.</p>
newtype ExecutionStartedEventDetails = ExecutionStartedEventDetails 
  { "Input'" :: NullOrUndefined (Data)
  , "RoleArn'" :: NullOrUndefined (Arn)
  }


newtype ExecutionStatus = ExecutionStatus String


-- | <p>Contains details about the successful termination of the execution.</p>
newtype ExecutionSucceededEventDetails = ExecutionSucceededEventDetails 
  { "Output'" :: NullOrUndefined (Data)
  }


-- | <p>Contains details about the execution timeout which occurred during the execution.</p>
newtype ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


newtype GetActivityTaskInput = GetActivityTaskInput 
  { "ActivityArn'" :: (Arn)
  , "WorkerName'" :: NullOrUndefined (Name)
  }


newtype GetActivityTaskOutput = GetActivityTaskOutput 
  { "TaskToken'" :: NullOrUndefined (TaskToken)
  , "Input'" :: NullOrUndefined (Data)
  }


newtype GetExecutionHistoryInput = GetExecutionHistoryInput 
  { "ExecutionArn'" :: (Arn)
  , "MaxResults'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }


newtype GetExecutionHistoryOutput = GetExecutionHistoryOutput 
  { "Events'" :: (HistoryEventList)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }


-- | <p>Contains details about the events of an execution.</p>
newtype HistoryEvent = HistoryEvent 
  { "Number" :: (Number)
  , "Type'" :: (HistoryEventType)
  , "Id'" :: (EventId)
  , "PreviousEventId'" :: NullOrUndefined (EventId)
  , "ActivityFailedEventDetails'" :: NullOrUndefined (ActivityFailedEventDetails)
  , "ActivityScheduleFailedEventDetails'" :: NullOrUndefined (ActivityScheduleFailedEventDetails)
  , "ActivityScheduledEventDetails'" :: NullOrUndefined (ActivityScheduledEventDetails)
  , "ActivityStartedEventDetails'" :: NullOrUndefined (ActivityStartedEventDetails)
  , "ActivitySucceededEventDetails'" :: NullOrUndefined (ActivitySucceededEventDetails)
  , "ActivityTimedOutEventDetails'" :: NullOrUndefined (ActivityTimedOutEventDetails)
  , "ExecutionFailedEventDetails'" :: NullOrUndefined (ExecutionFailedEventDetails)
  , "ExecutionStartedEventDetails'" :: NullOrUndefined (ExecutionStartedEventDetails)
  , "ExecutionSucceededEventDetails'" :: NullOrUndefined (ExecutionSucceededEventDetails)
  , "ExecutionAbortedEventDetails'" :: NullOrUndefined (ExecutionAbortedEventDetails)
  , "ExecutionTimedOutEventDetails'" :: NullOrUndefined (ExecutionTimedOutEventDetails)
  , "LambdaFunctionFailedEventDetails'" :: NullOrUndefined (LambdaFunctionFailedEventDetails)
  , "LambdaFunctionScheduleFailedEventDetails'" :: NullOrUndefined (LambdaFunctionScheduleFailedEventDetails)
  , "LambdaFunctionScheduledEventDetails'" :: NullOrUndefined (LambdaFunctionScheduledEventDetails)
  , "LambdaFunctionStartFailedEventDetails'" :: NullOrUndefined (LambdaFunctionStartFailedEventDetails)
  , "LambdaFunctionSucceededEventDetails'" :: NullOrUndefined (LambdaFunctionSucceededEventDetails)
  , "LambdaFunctionTimedOutEventDetails'" :: NullOrUndefined (LambdaFunctionTimedOutEventDetails)
  , "StateEnteredEventDetails'" :: NullOrUndefined (StateEnteredEventDetails)
  , "StateExitedEventDetails'" :: NullOrUndefined (StateExitedEventDetails)
  }


-- | <p>Contains details about the events which occurred during an execution.</p>
newtype HistoryEventList = HistoryEventList (Array HistoryEvent)


newtype HistoryEventType = HistoryEventType String


newtype Identity = Identity String


-- | <p>The provided Amazon Resource Name (ARN) is invalid.</p>
newtype InvalidArn = InvalidArn 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The provided Amazon States Language definition is invalid.</p>
newtype InvalidDefinition = InvalidDefinition 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The provided JSON input data is invalid.</p>
newtype InvalidExecutionInput = InvalidExecutionInput 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The provided name is invalid.</p>
newtype InvalidName = InvalidName 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The provided JSON output data is invalid.</p>
newtype InvalidOutput = InvalidOutput 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The provided token is invalid.</p>
newtype InvalidToken = InvalidToken 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Contains details about a lambda function which failed during an execution.</p>
newtype LambdaFunctionFailedEventDetails = LambdaFunctionFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


-- | <p>Contains details about a failed lambda function schedule event which occurred during an execution.</p>
newtype LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


-- | <p>Contains details about a lambda function scheduled during an execution.</p>
newtype LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails 
  { "Resource'" :: (Arn)
  , "Input'" :: NullOrUndefined (Data)
  , "TimeoutInSeconds'" :: NullOrUndefined (TimeoutInSeconds)
  }


-- | <p>Contains details about a lambda function which failed to start during an execution.</p>
newtype LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


-- | <p>Contains details about a lambda function which successfully terminated during an execution.</p>
newtype LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails 
  { "Output'" :: NullOrUndefined (Data)
  }


-- | <p>Contains details about a lambda function timeout which occurred during an execution.</p>
newtype LambdaFunctionTimedOutEventDetails = LambdaFunctionTimedOutEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


newtype ListActivitiesInput = ListActivitiesInput 
  { "MaxResults'" :: NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }


newtype ListActivitiesOutput = ListActivitiesOutput 
  { "Activities'" :: (ActivityList)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }


newtype ListExecutionsInput = ListExecutionsInput 
  { "StateMachineArn'" :: (Arn)
  , "StatusFilter'" :: NullOrUndefined (ExecutionStatus)
  , "MaxResults'" :: NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }


newtype ListExecutionsOutput = ListExecutionsOutput 
  { "Executions'" :: (ExecutionList)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }


newtype ListStateMachinesInput = ListStateMachinesInput 
  { "MaxResults'" :: NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }


newtype ListStateMachinesOutput = ListStateMachinesOutput 
  { "StateMachines'" :: (StateMachineList)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }


-- | <p>Request is missing a required parameter. This error occurs if both <code>definition</code> and <code>roleArn</code> are not specified.</p>
newtype MissingRequiredParameter = MissingRequiredParameter 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype Name = Name String


newtype PageSize = PageSize Int


newtype PageToken = PageToken String


newtype ReverseOrder = ReverseOrder Boolean


newtype SendTaskFailureInput = SendTaskFailureInput 
  { "TaskToken'" :: (TaskToken)
  , "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


newtype SendTaskFailureOutput = SendTaskFailureOutput 
  { 
  }


newtype SendTaskHeartbeatInput = SendTaskHeartbeatInput 
  { "TaskToken'" :: (TaskToken)
  }


newtype SendTaskHeartbeatOutput = SendTaskHeartbeatOutput 
  { 
  }


newtype SendTaskSuccessInput = SendTaskSuccessInput 
  { "TaskToken'" :: (TaskToken)
  , "Output'" :: (Data)
  }


newtype SendTaskSuccessOutput = SendTaskSuccessOutput 
  { 
  }


newtype StartExecutionInput = StartExecutionInput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: NullOrUndefined (Name)
  , "Input'" :: NullOrUndefined (Data)
  }


newtype StartExecutionOutput = StartExecutionOutput 
  { "ExecutionArn'" :: (Arn)
  , "StartDate'" :: (Number)
  }


-- | <p>Contains details about a state entered during an execution.</p>
newtype StateEnteredEventDetails = StateEnteredEventDetails 
  { "Name'" :: (Name)
  , "Input'" :: NullOrUndefined (Data)
  }


-- | <p>Contains details about an exit from a state during an execution.</p>
newtype StateExitedEventDetails = StateExitedEventDetails 
  { "Name'" :: (Name)
  , "Output'" :: NullOrUndefined (Data)
  }


-- | <p>A state machine with the same name but a different definition or role ARN already exists.</p>
newtype StateMachineAlreadyExists = StateMachineAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified state machine is being deleted.</p>
newtype StateMachineDeleting = StateMachineDeleting 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified state machine does not exist.</p>
newtype StateMachineDoesNotExist = StateMachineDoesNotExist 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The maximum number of state machines has been reached. Existing state machines must be deleted before a new state machine can be created.</p>
newtype StateMachineLimitExceeded = StateMachineLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype StateMachineList = StateMachineList (Array StateMachineListItem)


-- | <p>Contains details about the state machine.</p>
newtype StateMachineListItem = StateMachineListItem 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }


newtype StateMachineStatus = StateMachineStatus String


newtype StopExecutionInput = StopExecutionInput 
  { "ExecutionArn'" :: (Arn)
  , "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }


newtype StopExecutionOutput = StopExecutionOutput 
  { "StopDate'" :: (Number)
  }


newtype TaskDoesNotExist = TaskDoesNotExist 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype TaskTimedOut = TaskTimedOut 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }


newtype TaskToken = TaskToken String


newtype TimeoutInSeconds = TimeoutInSeconds Number


newtype UpdateStateMachineInput = UpdateStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  , "Definition'" :: NullOrUndefined (Definition)
  , "RoleArn'" :: NullOrUndefined (Arn)
  }


newtype UpdateStateMachineOutput = UpdateStateMachineOutput 
  { "UpdateDate'" :: (Number)
  }
