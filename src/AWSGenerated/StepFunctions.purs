

-- | <fullname>AWS Step Functions</fullname> <p>AWS Step Functions is a service that lets you coordinate the components of distributed applications and microservices using visual workflows.</p> <p>You can use Step Functions to build applications from individual components, each of which performs a discrete function, or <i>task</i>, allowing you to scale and change applications quickly. Step Functions provides a console that helps visualize the components of your application as a series of steps. Step Functions automatically triggers and tracks each step, and retries steps when there are errors, so your application executes predictably and in the right order every time. Step Functions logs the state of each step, so you can quickly diagnose and debug any issues.</p> <p>Step Functions manages operations and underlying infrastructure to ensure your application is available at any scale. You can run tasks on AWS, your own servers, or any system that has access to AWS. You can access and use Step Functions using the console, the AWS SDKs, or an HTTP API. For more information about Step Functions, see the <i> <a href="http://docs.aws.amazon.com/step-functions/latest/dg/welcome.html">AWS Step Functions Developer Guide</a> </i>.</p>
module AWS.StepFunctions where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "StepFunctions" :: String


-- | <p>Creates an activity. An activity is a task which you write in any programming language and host on any machine which has access to AWS Step Functions. Activities must poll Step Functions using the <code>GetActivityTask</code> API action and respond using <code>SendTask*</code> API actions. This function lets Step Functions know the existence of your activity and returns an identifier for use in a state machine and when polling from the activity.</p>
createActivity :: forall eff. CreateActivityInput -> Aff (err :: AWS.RequestError | eff) CreateActivityOutput
createActivity = AWS.request serviceName "createActivity" 


-- | <p>Creates a state machine. A state machine consists of a collection of states that can do work (<code>Task</code> states), determine to which states to transition next (<code>Choice</code> states), stop an execution with an error (<code>Fail</code> states), and so on. State machines are specified using a JSON-based, structured language.</p>
createStateMachine :: forall eff. CreateStateMachineInput -> Aff (err :: AWS.RequestError | eff) CreateStateMachineOutput
createStateMachine = AWS.request serviceName "createStateMachine" 


-- | <p>Deletes an activity.</p>
deleteActivity :: forall eff. DeleteActivityInput -> Aff (err :: AWS.RequestError | eff) DeleteActivityOutput
deleteActivity = AWS.request serviceName "deleteActivity" 


-- | <p>Deletes a state machine. This is an asynchronous operation: It sets the state machine's status to <code>DELETING</code> and begins the deletion process. Each state machine execution is deleted the next time it makes a state transition.</p> <note> <p>The state machine itself is deleted after all executions are completed or deleted.</p> </note>
deleteStateMachine :: forall eff. DeleteStateMachineInput -> Aff (err :: AWS.RequestError | eff) DeleteStateMachineOutput
deleteStateMachine = AWS.request serviceName "deleteStateMachine" 


-- | <p>Describes an activity.</p>
describeActivity :: forall eff. DescribeActivityInput -> Aff (err :: AWS.RequestError | eff) DescribeActivityOutput
describeActivity = AWS.request serviceName "describeActivity" 


-- | <p>Describes an execution.</p>
describeExecution :: forall eff. DescribeExecutionInput -> Aff (err :: AWS.RequestError | eff) DescribeExecutionOutput
describeExecution = AWS.request serviceName "describeExecution" 


-- | <p>Describes a state machine.</p>
describeStateMachine :: forall eff. DescribeStateMachineInput -> Aff (err :: AWS.RequestError | eff) DescribeStateMachineOutput
describeStateMachine = AWS.request serviceName "describeStateMachine" 


-- | <p>Describes the state machine associated with a specific execution.</p>
describeStateMachineForExecution :: forall eff. DescribeStateMachineForExecutionInput -> Aff (err :: AWS.RequestError | eff) DescribeStateMachineForExecutionOutput
describeStateMachineForExecution = AWS.request serviceName "describeStateMachineForExecution" 


-- | <p>Used by workers to retrieve a task (with the specified activity ARN) which has been scheduled for execution by a running state machine. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available (i.e. an execution of a task of this type is needed.) The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns a <code>taskToken</code> with a null string.</p> <important> <p>Workers should set their client side socket timeout to at least 65 seconds (5 seconds higher than the maximum time the service may hold the poll request).</p> </important>
getActivityTask :: forall eff. GetActivityTaskInput -> Aff (err :: AWS.RequestError | eff) GetActivityTaskOutput
getActivityTask = AWS.request serviceName "getActivityTask" 


-- | <p>Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the <code>timeStamp</code> of the events. Use the <code>reverseOrder</code> parameter to get the latest events first.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
getExecutionHistory :: forall eff. GetExecutionHistoryInput -> Aff (err :: AWS.RequestError | eff) GetExecutionHistoryOutput
getExecutionHistory = AWS.request serviceName "getExecutionHistory" 


-- | <p>Lists the existing activities.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listActivities :: forall eff. ListActivitiesInput -> Aff (err :: AWS.RequestError | eff) ListActivitiesOutput
listActivities = AWS.request serviceName "listActivities" 


-- | <p>Lists the executions of a state machine that meet the filtering criteria.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listExecutions :: forall eff. ListExecutionsInput -> Aff (err :: AWS.RequestError | eff) ListExecutionsOutput
listExecutions = AWS.request serviceName "listExecutions" 


-- | <p>Lists the existing state machines.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listStateMachines :: forall eff. ListStateMachinesInput -> Aff (err :: AWS.RequestError | eff) ListStateMachinesOutput
listStateMachines = AWS.request serviceName "listStateMachines" 


-- | <p>Used by workers to report that the task identified by the <code>taskToken</code> failed.</p>
sendTaskFailure :: forall eff. SendTaskFailureInput -> Aff (err :: AWS.RequestError | eff) SendTaskFailureOutput
sendTaskFailure = AWS.request serviceName "sendTaskFailure" 


-- | <p>Used by workers to report to the service that the task represented by the specified <code>taskToken</code> is still making progress. This action resets the <code>Heartbeat</code> clock. The <code>Heartbeat</code> threshold is specified in the state machine's Amazon States Language definition. This action does not in itself create an event in the execution history. However, if the task times out, the execution history contains an <code>ActivityTimedOut</code> event.</p> <note> <p>The <code>Timeout</code> of a task, defined in the state machine's Amazon States Language definition, is its maximum allowed duration, regardless of the number of <a>SendTaskHeartbeat</a> requests received.</p> </note> <note> <p>This operation is only useful for long-lived tasks to report the liveliness of the task.</p> </note>
sendTaskHeartbeat :: forall eff. SendTaskHeartbeatInput -> Aff (err :: AWS.RequestError | eff) SendTaskHeartbeatOutput
sendTaskHeartbeat = AWS.request serviceName "sendTaskHeartbeat" 


-- | <p>Used by workers to report that the task identified by the <code>taskToken</code> completed successfully.</p>
sendTaskSuccess :: forall eff. SendTaskSuccessInput -> Aff (err :: AWS.RequestError | eff) SendTaskSuccessOutput
sendTaskSuccess = AWS.request serviceName "sendTaskSuccess" 


-- | <p>Starts a state machine execution.</p>
startExecution :: forall eff. StartExecutionInput -> Aff (err :: AWS.RequestError | eff) StartExecutionOutput
startExecution = AWS.request serviceName "startExecution" 


-- | <p>Stops an execution.</p>
stopExecution :: forall eff. StopExecutionInput -> Aff (err :: AWS.RequestError | eff) StopExecutionOutput
stopExecution = AWS.request serviceName "stopExecution" 


-- | <p>Updates an existing state machine by modifying its <code>definition</code> and/or <code>roleArn</code>. Running executions will continue to use the previous <code>definition</code> and <code>roleArn</code>.</p> <note> <p>All <code>StartExecution</code> calls within a few seconds will use the updated <code>definition</code> and <code>roleArn</code>. Executions started immediately after calling <code>UpdateStateMachine</code> may use the previous state machine <code>definition</code> and <code>roleArn</code>. You must include at least one of <code>definition</code> or <code>roleArn</code> or you will receive a <code>MissingRequiredParameter</code> error.</p> </note>
updateStateMachine :: forall eff. UpdateStateMachineInput -> Aff (err :: AWS.RequestError | eff) UpdateStateMachineOutput
updateStateMachine = AWS.request serviceName "updateStateMachine" 


-- | <p>The specified activity does not exist.</p>
newtype ActivityDoesNotExist = ActivityDoesNotExist 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeActivityDoesNotExist :: Newtype ActivityDoesNotExist _


-- | <p>Contains details about an activity which failed during an execution.</p>
newtype ActivityFailedEventDetails = ActivityFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeActivityFailedEventDetails :: Newtype ActivityFailedEventDetails _


-- | <p>The maximum number of activities has been reached. Existing activities must be deleted before a new activity can be created.</p>
newtype ActivityLimitExceeded = ActivityLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeActivityLimitExceeded :: Newtype ActivityLimitExceeded _


newtype ActivityList = ActivityList (Array ActivityListItem)
derive instance newtypeActivityList :: Newtype ActivityList _


-- | <p>Contains details about an activity.</p>
newtype ActivityListItem = ActivityListItem 
  { "ActivityArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeActivityListItem :: Newtype ActivityListItem _


-- | <p>Contains details about an activity schedule failure which occurred during an execution.</p>
newtype ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeActivityScheduleFailedEventDetails :: Newtype ActivityScheduleFailedEventDetails _


-- | <p>Contains details about an activity scheduled during an execution.</p>
newtype ActivityScheduledEventDetails = ActivityScheduledEventDetails 
  { "Resource'" :: (Arn)
  , "Input'" :: NullOrUndefined (Data)
  , "TimeoutInSeconds'" :: NullOrUndefined (TimeoutInSeconds)
  , "HeartbeatInSeconds'" :: NullOrUndefined (TimeoutInSeconds)
  }
derive instance newtypeActivityScheduledEventDetails :: Newtype ActivityScheduledEventDetails _


-- | <p>Contains details about the start of an activity during an execution.</p>
newtype ActivityStartedEventDetails = ActivityStartedEventDetails 
  { "WorkerName'" :: NullOrUndefined (Identity)
  }
derive instance newtypeActivityStartedEventDetails :: Newtype ActivityStartedEventDetails _


-- | <p>Contains details about an activity which successfully terminated during an execution.</p>
newtype ActivitySucceededEventDetails = ActivitySucceededEventDetails 
  { "Output'" :: NullOrUndefined (Data)
  }
derive instance newtypeActivitySucceededEventDetails :: Newtype ActivitySucceededEventDetails _


-- | <p>Contains details about an activity timeout which occurred during an execution.</p>
newtype ActivityTimedOutEventDetails = ActivityTimedOutEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeActivityTimedOutEventDetails :: Newtype ActivityTimedOutEventDetails _


-- | <p>The maximum number of workers concurrently polling for activity tasks has been reached.</p>
newtype ActivityWorkerLimitExceeded = ActivityWorkerLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeActivityWorkerLimitExceeded :: Newtype ActivityWorkerLimitExceeded _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


newtype Cause = Cause String
derive instance newtypeCause :: Newtype Cause _


newtype CreateActivityInput = CreateActivityInput 
  { "Name'" :: (Name)
  }
derive instance newtypeCreateActivityInput :: Newtype CreateActivityInput _


newtype CreateActivityOutput = CreateActivityOutput 
  { "ActivityArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeCreateActivityOutput :: Newtype CreateActivityOutput _


newtype CreateStateMachineInput = CreateStateMachineInput 
  { "Name'" :: (Name)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  }
derive instance newtypeCreateStateMachineInput :: Newtype CreateStateMachineInput _


newtype CreateStateMachineOutput = CreateStateMachineOutput 
  { "StateMachineArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeCreateStateMachineOutput :: Newtype CreateStateMachineOutput _


newtype Data = Data String
derive instance newtypeData :: Newtype Data _


newtype Definition = Definition String
derive instance newtypeDefinition :: Newtype Definition _


newtype DeleteActivityInput = DeleteActivityInput 
  { "ActivityArn'" :: (Arn)
  }
derive instance newtypeDeleteActivityInput :: Newtype DeleteActivityInput _


newtype DeleteActivityOutput = DeleteActivityOutput 
  { 
  }
derive instance newtypeDeleteActivityOutput :: Newtype DeleteActivityOutput _


newtype DeleteStateMachineInput = DeleteStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  }
derive instance newtypeDeleteStateMachineInput :: Newtype DeleteStateMachineInput _


newtype DeleteStateMachineOutput = DeleteStateMachineOutput 
  { 
  }
derive instance newtypeDeleteStateMachineOutput :: Newtype DeleteStateMachineOutput _


newtype DescribeActivityInput = DescribeActivityInput 
  { "ActivityArn'" :: (Arn)
  }
derive instance newtypeDescribeActivityInput :: Newtype DescribeActivityInput _


newtype DescribeActivityOutput = DescribeActivityOutput 
  { "ActivityArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeDescribeActivityOutput :: Newtype DescribeActivityOutput _


newtype DescribeExecutionInput = DescribeExecutionInput 
  { "ExecutionArn'" :: (Arn)
  }
derive instance newtypeDescribeExecutionInput :: Newtype DescribeExecutionInput _


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
derive instance newtypeDescribeExecutionOutput :: Newtype DescribeExecutionOutput _


newtype DescribeStateMachineForExecutionInput = DescribeStateMachineForExecutionInput 
  { "ExecutionArn'" :: (Arn)
  }
derive instance newtypeDescribeStateMachineForExecutionInput :: Newtype DescribeStateMachineForExecutionInput _


newtype DescribeStateMachineForExecutionOutput = DescribeStateMachineForExecutionOutput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  , "UpdateDate'" :: (Number)
  }
derive instance newtypeDescribeStateMachineForExecutionOutput :: Newtype DescribeStateMachineForExecutionOutput _


newtype DescribeStateMachineInput = DescribeStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  }
derive instance newtypeDescribeStateMachineInput :: Newtype DescribeStateMachineInput _


newtype DescribeStateMachineOutput = DescribeStateMachineOutput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Status'" :: NullOrUndefined (StateMachineStatus)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeDescribeStateMachineOutput :: Newtype DescribeStateMachineOutput _


newtype Error = Error String
derive instance newtypeError :: Newtype Error _


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype EventId = EventId Number
derive instance newtypeEventId :: Newtype EventId _


-- | <p>Contains details about an abort of an execution.</p>
newtype ExecutionAbortedEventDetails = ExecutionAbortedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeExecutionAbortedEventDetails :: Newtype ExecutionAbortedEventDetails _


-- | <p>The execution has the same <code>name</code> as another execution (but a different <code>input</code>).</p> <note> <p>Executions with the same <code>name</code> and <code>input</code> are considered idempotent.</p> </note>
newtype ExecutionAlreadyExists = ExecutionAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExecutionAlreadyExists :: Newtype ExecutionAlreadyExists _


-- | <p>The specified execution does not exist.</p>
newtype ExecutionDoesNotExist = ExecutionDoesNotExist 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExecutionDoesNotExist :: Newtype ExecutionDoesNotExist _


-- | <p>Contains details about an execution failure event.</p>
newtype ExecutionFailedEventDetails = ExecutionFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeExecutionFailedEventDetails :: Newtype ExecutionFailedEventDetails _


-- | <p>The maximum number of running executions has been reached. Running executions must end or be stopped before a new execution can be started.</p>
newtype ExecutionLimitExceeded = ExecutionLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExecutionLimitExceeded :: Newtype ExecutionLimitExceeded _


newtype ExecutionList = ExecutionList (Array ExecutionListItem)
derive instance newtypeExecutionList :: Newtype ExecutionList _


-- | <p>Contains details about an execution.</p>
newtype ExecutionListItem = ExecutionListItem 
  { "ExecutionArn'" :: (Arn)
  , "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Status'" :: (ExecutionStatus)
  , "StartDate'" :: (Number)
  , "StopDate'" :: NullOrUndefined (Number)
  }
derive instance newtypeExecutionListItem :: Newtype ExecutionListItem _


-- | <p>Contains details about the start of the execution.</p>
newtype ExecutionStartedEventDetails = ExecutionStartedEventDetails 
  { "Input'" :: NullOrUndefined (Data)
  , "RoleArn'" :: NullOrUndefined (Arn)
  }
derive instance newtypeExecutionStartedEventDetails :: Newtype ExecutionStartedEventDetails _


newtype ExecutionStatus = ExecutionStatus String
derive instance newtypeExecutionStatus :: Newtype ExecutionStatus _


-- | <p>Contains details about the successful termination of the execution.</p>
newtype ExecutionSucceededEventDetails = ExecutionSucceededEventDetails 
  { "Output'" :: NullOrUndefined (Data)
  }
derive instance newtypeExecutionSucceededEventDetails :: Newtype ExecutionSucceededEventDetails _


-- | <p>Contains details about the execution timeout which occurred during the execution.</p>
newtype ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeExecutionTimedOutEventDetails :: Newtype ExecutionTimedOutEventDetails _


newtype GetActivityTaskInput = GetActivityTaskInput 
  { "ActivityArn'" :: (Arn)
  , "WorkerName'" :: NullOrUndefined (Name)
  }
derive instance newtypeGetActivityTaskInput :: Newtype GetActivityTaskInput _


newtype GetActivityTaskOutput = GetActivityTaskOutput 
  { "TaskToken'" :: NullOrUndefined (TaskToken)
  , "Input'" :: NullOrUndefined (Data)
  }
derive instance newtypeGetActivityTaskOutput :: Newtype GetActivityTaskOutput _


newtype GetExecutionHistoryInput = GetExecutionHistoryInput 
  { "ExecutionArn'" :: (Arn)
  , "MaxResults'" :: NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined (ReverseOrder)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeGetExecutionHistoryInput :: Newtype GetExecutionHistoryInput _


newtype GetExecutionHistoryOutput = GetExecutionHistoryOutput 
  { "Events'" :: (HistoryEventList)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeGetExecutionHistoryOutput :: Newtype GetExecutionHistoryOutput _


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
derive instance newtypeHistoryEvent :: Newtype HistoryEvent _


-- | <p>Contains details about the events which occurred during an execution.</p>
newtype HistoryEventList = HistoryEventList (Array HistoryEvent)
derive instance newtypeHistoryEventList :: Newtype HistoryEventList _


newtype HistoryEventType = HistoryEventType String
derive instance newtypeHistoryEventType :: Newtype HistoryEventType _


newtype Identity = Identity String
derive instance newtypeIdentity :: Newtype Identity _


-- | <p>The provided Amazon Resource Name (ARN) is invalid.</p>
newtype InvalidArn = InvalidArn 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArn :: Newtype InvalidArn _


-- | <p>The provided Amazon States Language definition is invalid.</p>
newtype InvalidDefinition = InvalidDefinition 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidDefinition :: Newtype InvalidDefinition _


-- | <p>The provided JSON input data is invalid.</p>
newtype InvalidExecutionInput = InvalidExecutionInput 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidExecutionInput :: Newtype InvalidExecutionInput _


-- | <p>The provided name is invalid.</p>
newtype InvalidName = InvalidName 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidName :: Newtype InvalidName _


-- | <p>The provided JSON output data is invalid.</p>
newtype InvalidOutput = InvalidOutput 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidOutput :: Newtype InvalidOutput _


-- | <p>The provided token is invalid.</p>
newtype InvalidToken = InvalidToken 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidToken :: Newtype InvalidToken _


-- | <p>Contains details about a lambda function which failed during an execution.</p>
newtype LambdaFunctionFailedEventDetails = LambdaFunctionFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeLambdaFunctionFailedEventDetails :: Newtype LambdaFunctionFailedEventDetails _


-- | <p>Contains details about a failed lambda function schedule event which occurred during an execution.</p>
newtype LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeLambdaFunctionScheduleFailedEventDetails :: Newtype LambdaFunctionScheduleFailedEventDetails _


-- | <p>Contains details about a lambda function scheduled during an execution.</p>
newtype LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails 
  { "Resource'" :: (Arn)
  , "Input'" :: NullOrUndefined (Data)
  , "TimeoutInSeconds'" :: NullOrUndefined (TimeoutInSeconds)
  }
derive instance newtypeLambdaFunctionScheduledEventDetails :: Newtype LambdaFunctionScheduledEventDetails _


-- | <p>Contains details about a lambda function which failed to start during an execution.</p>
newtype LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeLambdaFunctionStartFailedEventDetails :: Newtype LambdaFunctionStartFailedEventDetails _


-- | <p>Contains details about a lambda function which successfully terminated during an execution.</p>
newtype LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails 
  { "Output'" :: NullOrUndefined (Data)
  }
derive instance newtypeLambdaFunctionSucceededEventDetails :: Newtype LambdaFunctionSucceededEventDetails _


-- | <p>Contains details about a lambda function timeout which occurred during an execution.</p>
newtype LambdaFunctionTimedOutEventDetails = LambdaFunctionTimedOutEventDetails 
  { "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeLambdaFunctionTimedOutEventDetails :: Newtype LambdaFunctionTimedOutEventDetails _


newtype ListActivitiesInput = ListActivitiesInput 
  { "MaxResults'" :: NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListActivitiesInput :: Newtype ListActivitiesInput _


newtype ListActivitiesOutput = ListActivitiesOutput 
  { "Activities'" :: (ActivityList)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListActivitiesOutput :: Newtype ListActivitiesOutput _


newtype ListExecutionsInput = ListExecutionsInput 
  { "StateMachineArn'" :: (Arn)
  , "StatusFilter'" :: NullOrUndefined (ExecutionStatus)
  , "MaxResults'" :: NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListExecutionsInput :: Newtype ListExecutionsInput _


newtype ListExecutionsOutput = ListExecutionsOutput 
  { "Executions'" :: (ExecutionList)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListExecutionsOutput :: Newtype ListExecutionsOutput _


newtype ListStateMachinesInput = ListStateMachinesInput 
  { "MaxResults'" :: NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListStateMachinesInput :: Newtype ListStateMachinesInput _


newtype ListStateMachinesOutput = ListStateMachinesOutput 
  { "StateMachines'" :: (StateMachineList)
  , "NextToken'" :: NullOrUndefined (PageToken)
  }
derive instance newtypeListStateMachinesOutput :: Newtype ListStateMachinesOutput _


-- | <p>Request is missing a required parameter. This error occurs if both <code>definition</code> and <code>roleArn</code> are not specified.</p>
newtype MissingRequiredParameter = MissingRequiredParameter 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMissingRequiredParameter :: Newtype MissingRequiredParameter _


newtype Name = Name String
derive instance newtypeName :: Newtype Name _


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _


newtype PageToken = PageToken String
derive instance newtypePageToken :: Newtype PageToken _


newtype ReverseOrder = ReverseOrder Boolean
derive instance newtypeReverseOrder :: Newtype ReverseOrder _


newtype SendTaskFailureInput = SendTaskFailureInput 
  { "TaskToken'" :: (TaskToken)
  , "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeSendTaskFailureInput :: Newtype SendTaskFailureInput _


newtype SendTaskFailureOutput = SendTaskFailureOutput 
  { 
  }
derive instance newtypeSendTaskFailureOutput :: Newtype SendTaskFailureOutput _


newtype SendTaskHeartbeatInput = SendTaskHeartbeatInput 
  { "TaskToken'" :: (TaskToken)
  }
derive instance newtypeSendTaskHeartbeatInput :: Newtype SendTaskHeartbeatInput _


newtype SendTaskHeartbeatOutput = SendTaskHeartbeatOutput 
  { 
  }
derive instance newtypeSendTaskHeartbeatOutput :: Newtype SendTaskHeartbeatOutput _


newtype SendTaskSuccessInput = SendTaskSuccessInput 
  { "TaskToken'" :: (TaskToken)
  , "Output'" :: (Data)
  }
derive instance newtypeSendTaskSuccessInput :: Newtype SendTaskSuccessInput _


newtype SendTaskSuccessOutput = SendTaskSuccessOutput 
  { 
  }
derive instance newtypeSendTaskSuccessOutput :: Newtype SendTaskSuccessOutput _


newtype StartExecutionInput = StartExecutionInput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: NullOrUndefined (Name)
  , "Input'" :: NullOrUndefined (Data)
  }
derive instance newtypeStartExecutionInput :: Newtype StartExecutionInput _


newtype StartExecutionOutput = StartExecutionOutput 
  { "ExecutionArn'" :: (Arn)
  , "StartDate'" :: (Number)
  }
derive instance newtypeStartExecutionOutput :: Newtype StartExecutionOutput _


-- | <p>Contains details about a state entered during an execution.</p>
newtype StateEnteredEventDetails = StateEnteredEventDetails 
  { "Name'" :: (Name)
  , "Input'" :: NullOrUndefined (Data)
  }
derive instance newtypeStateEnteredEventDetails :: Newtype StateEnteredEventDetails _


-- | <p>Contains details about an exit from a state during an execution.</p>
newtype StateExitedEventDetails = StateExitedEventDetails 
  { "Name'" :: (Name)
  , "Output'" :: NullOrUndefined (Data)
  }
derive instance newtypeStateExitedEventDetails :: Newtype StateExitedEventDetails _


-- | <p>A state machine with the same name but a different definition or role ARN already exists.</p>
newtype StateMachineAlreadyExists = StateMachineAlreadyExists 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeStateMachineAlreadyExists :: Newtype StateMachineAlreadyExists _


-- | <p>The specified state machine is being deleted.</p>
newtype StateMachineDeleting = StateMachineDeleting 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeStateMachineDeleting :: Newtype StateMachineDeleting _


-- | <p>The specified state machine does not exist.</p>
newtype StateMachineDoesNotExist = StateMachineDoesNotExist 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeStateMachineDoesNotExist :: Newtype StateMachineDoesNotExist _


-- | <p>The maximum number of state machines has been reached. Existing state machines must be deleted before a new state machine can be created.</p>
newtype StateMachineLimitExceeded = StateMachineLimitExceeded 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeStateMachineLimitExceeded :: Newtype StateMachineLimitExceeded _


newtype StateMachineList = StateMachineList (Array StateMachineListItem)
derive instance newtypeStateMachineList :: Newtype StateMachineList _


-- | <p>Contains details about the state machine.</p>
newtype StateMachineListItem = StateMachineListItem 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeStateMachineListItem :: Newtype StateMachineListItem _


newtype StateMachineStatus = StateMachineStatus String
derive instance newtypeStateMachineStatus :: Newtype StateMachineStatus _


newtype StopExecutionInput = StopExecutionInput 
  { "ExecutionArn'" :: (Arn)
  , "Error'" :: NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined (Cause)
  }
derive instance newtypeStopExecutionInput :: Newtype StopExecutionInput _


newtype StopExecutionOutput = StopExecutionOutput 
  { "StopDate'" :: (Number)
  }
derive instance newtypeStopExecutionOutput :: Newtype StopExecutionOutput _


newtype TaskDoesNotExist = TaskDoesNotExist 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTaskDoesNotExist :: Newtype TaskDoesNotExist _


newtype TaskTimedOut = TaskTimedOut 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTaskTimedOut :: Newtype TaskTimedOut _


newtype TaskToken = TaskToken String
derive instance newtypeTaskToken :: Newtype TaskToken _


newtype TimeoutInSeconds = TimeoutInSeconds Number
derive instance newtypeTimeoutInSeconds :: Newtype TimeoutInSeconds _


newtype UpdateStateMachineInput = UpdateStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  , "Definition'" :: NullOrUndefined (Definition)
  , "RoleArn'" :: NullOrUndefined (Arn)
  }
derive instance newtypeUpdateStateMachineInput :: Newtype UpdateStateMachineInput _


newtype UpdateStateMachineOutput = UpdateStateMachineOutput 
  { "UpdateDate'" :: (Number)
  }
derive instance newtypeUpdateStateMachineOutput :: Newtype UpdateStateMachineOutput _
