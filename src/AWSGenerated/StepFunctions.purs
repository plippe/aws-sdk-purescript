

-- | <fullname>AWS Step Functions</fullname> <p>AWS Step Functions is a service that lets you coordinate the components of distributed applications and microservices using visual workflows.</p> <p>You can use Step Functions to build applications from individual components, each of which performs a discrete function, or <i>task</i>, allowing you to scale and change applications quickly. Step Functions provides a console that helps visualize the components of your application as a series of steps. Step Functions automatically triggers and tracks each step, and retries steps when there are errors, so your application executes predictably and in the right order every time. Step Functions logs the state of each step, so you can quickly diagnose and debug any issues.</p> <p>Step Functions manages operations and underlying infrastructure to ensure your application is available at any scale. You can run tasks on AWS, your own servers, or any system that has access to AWS. You can access and use Step Functions using the console, the AWS SDKs, or an HTTP API. For more information about Step Functions, see the <i> <a href="http://docs.aws.amazon.com/step-functions/latest/dg/welcome.html">AWS Step Functions Developer Guide</a> </i>.</p>
module AWS.StepFunctions where

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

serviceName = "StepFunctions" :: String


-- | <p>Creates an activity. An activity is a task which you write in any programming language and host on any machine which has access to AWS Step Functions. Activities must poll Step Functions using the <code>GetActivityTask</code> API action and respond using <code>SendTask*</code> API actions. This function lets Step Functions know the existence of your activity and returns an identifier for use in a state machine and when polling from the activity.</p>
createActivity :: forall eff. CreateActivityInput -> Aff (exception :: EXCEPTION | eff) CreateActivityOutput
createActivity = Request.request serviceName "createActivity" 


-- | <p>Creates a state machine. A state machine consists of a collection of states that can do work (<code>Task</code> states), determine to which states to transition next (<code>Choice</code> states), stop an execution with an error (<code>Fail</code> states), and so on. State machines are specified using a JSON-based, structured language.</p>
createStateMachine :: forall eff. CreateStateMachineInput -> Aff (exception :: EXCEPTION | eff) CreateStateMachineOutput
createStateMachine = Request.request serviceName "createStateMachine" 


-- | <p>Deletes an activity.</p>
deleteActivity :: forall eff. DeleteActivityInput -> Aff (exception :: EXCEPTION | eff) DeleteActivityOutput
deleteActivity = Request.request serviceName "deleteActivity" 


-- | <p>Deletes a state machine. This is an asynchronous operation: It sets the state machine's status to <code>DELETING</code> and begins the deletion process. Each state machine execution is deleted the next time it makes a state transition.</p> <note> <p>The state machine itself is deleted after all executions are completed or deleted.</p> </note>
deleteStateMachine :: forall eff. DeleteStateMachineInput -> Aff (exception :: EXCEPTION | eff) DeleteStateMachineOutput
deleteStateMachine = Request.request serviceName "deleteStateMachine" 


-- | <p>Describes an activity.</p>
describeActivity :: forall eff. DescribeActivityInput -> Aff (exception :: EXCEPTION | eff) DescribeActivityOutput
describeActivity = Request.request serviceName "describeActivity" 


-- | <p>Describes an execution.</p>
describeExecution :: forall eff. DescribeExecutionInput -> Aff (exception :: EXCEPTION | eff) DescribeExecutionOutput
describeExecution = Request.request serviceName "describeExecution" 


-- | <p>Describes a state machine.</p>
describeStateMachine :: forall eff. DescribeStateMachineInput -> Aff (exception :: EXCEPTION | eff) DescribeStateMachineOutput
describeStateMachine = Request.request serviceName "describeStateMachine" 


-- | <p>Describes the state machine associated with a specific execution.</p>
describeStateMachineForExecution :: forall eff. DescribeStateMachineForExecutionInput -> Aff (exception :: EXCEPTION | eff) DescribeStateMachineForExecutionOutput
describeStateMachineForExecution = Request.request serviceName "describeStateMachineForExecution" 


-- | <p>Used by workers to retrieve a task (with the specified activity ARN) which has been scheduled for execution by a running state machine. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available (i.e. an execution of a task of this type is needed.) The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns a <code>taskToken</code> with a null string.</p> <important> <p>Workers should set their client side socket timeout to at least 65 seconds (5 seconds higher than the maximum time the service may hold the poll request).</p> </important>
getActivityTask :: forall eff. GetActivityTaskInput -> Aff (exception :: EXCEPTION | eff) GetActivityTaskOutput
getActivityTask = Request.request serviceName "getActivityTask" 


-- | <p>Returns the history of the specified execution as a list of events. By default, the results are returned in ascending order of the <code>timeStamp</code> of the events. Use the <code>reverseOrder</code> parameter to get the latest events first.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
getExecutionHistory :: forall eff. GetExecutionHistoryInput -> Aff (exception :: EXCEPTION | eff) GetExecutionHistoryOutput
getExecutionHistory = Request.request serviceName "getExecutionHistory" 


-- | <p>Lists the existing activities.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listActivities :: forall eff. ListActivitiesInput -> Aff (exception :: EXCEPTION | eff) ListActivitiesOutput
listActivities = Request.request serviceName "listActivities" 


-- | <p>Lists the executions of a state machine that meet the filtering criteria.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listExecutions :: forall eff. ListExecutionsInput -> Aff (exception :: EXCEPTION | eff) ListExecutionsOutput
listExecutions = Request.request serviceName "listExecutions" 


-- | <p>Lists the existing state machines.</p> <p>If a <code>nextToken</code> is returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in <code>nextToken</code>. Keep all other arguments unchanged.</p>
listStateMachines :: forall eff. ListStateMachinesInput -> Aff (exception :: EXCEPTION | eff) ListStateMachinesOutput
listStateMachines = Request.request serviceName "listStateMachines" 


-- | <p>Used by workers to report that the task identified by the <code>taskToken</code> failed.</p>
sendTaskFailure :: forall eff. SendTaskFailureInput -> Aff (exception :: EXCEPTION | eff) SendTaskFailureOutput
sendTaskFailure = Request.request serviceName "sendTaskFailure" 


-- | <p>Used by workers to report to the service that the task represented by the specified <code>taskToken</code> is still making progress. This action resets the <code>Heartbeat</code> clock. The <code>Heartbeat</code> threshold is specified in the state machine's Amazon States Language definition. This action does not in itself create an event in the execution history. However, if the task times out, the execution history contains an <code>ActivityTimedOut</code> event.</p> <note> <p>The <code>Timeout</code> of a task, defined in the state machine's Amazon States Language definition, is its maximum allowed duration, regardless of the number of <a>SendTaskHeartbeat</a> requests received.</p> </note> <note> <p>This operation is only useful for long-lived tasks to report the liveliness of the task.</p> </note>
sendTaskHeartbeat :: forall eff. SendTaskHeartbeatInput -> Aff (exception :: EXCEPTION | eff) SendTaskHeartbeatOutput
sendTaskHeartbeat = Request.request serviceName "sendTaskHeartbeat" 


-- | <p>Used by workers to report that the task identified by the <code>taskToken</code> completed successfully.</p>
sendTaskSuccess :: forall eff. SendTaskSuccessInput -> Aff (exception :: EXCEPTION | eff) SendTaskSuccessOutput
sendTaskSuccess = Request.request serviceName "sendTaskSuccess" 


-- | <p>Starts a state machine execution.</p>
startExecution :: forall eff. StartExecutionInput -> Aff (exception :: EXCEPTION | eff) StartExecutionOutput
startExecution = Request.request serviceName "startExecution" 


-- | <p>Stops an execution.</p>
stopExecution :: forall eff. StopExecutionInput -> Aff (exception :: EXCEPTION | eff) StopExecutionOutput
stopExecution = Request.request serviceName "stopExecution" 


-- | <p>Updates an existing state machine by modifying its <code>definition</code> and/or <code>roleArn</code>. Running executions will continue to use the previous <code>definition</code> and <code>roleArn</code>.</p> <note> <p>All <code>StartExecution</code> calls within a few seconds will use the updated <code>definition</code> and <code>roleArn</code>. Executions started immediately after calling <code>UpdateStateMachine</code> may use the previous state machine <code>definition</code> and <code>roleArn</code>. You must include at least one of <code>definition</code> or <code>roleArn</code> or you will receive a <code>MissingRequiredParameter</code> error.</p> </note>
updateStateMachine :: forall eff. UpdateStateMachineInput -> Aff (exception :: EXCEPTION | eff) UpdateStateMachineOutput
updateStateMachine = Request.request serviceName "updateStateMachine" 


-- | <p>The specified activity does not exist.</p>
newtype ActivityDoesNotExist = ActivityDoesNotExist 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeActivityDoesNotExist :: Newtype ActivityDoesNotExist _
derive instance repGenericActivityDoesNotExist :: Generic ActivityDoesNotExist _
instance showActivityDoesNotExist :: Show ActivityDoesNotExist where
  show = genericShow
instance decodeActivityDoesNotExist :: Decode ActivityDoesNotExist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityDoesNotExist :: Encode ActivityDoesNotExist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an activity which failed during an execution.</p>
newtype ActivityFailedEventDetails = ActivityFailedEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeActivityFailedEventDetails :: Newtype ActivityFailedEventDetails _
derive instance repGenericActivityFailedEventDetails :: Generic ActivityFailedEventDetails _
instance showActivityFailedEventDetails :: Show ActivityFailedEventDetails where
  show = genericShow
instance decodeActivityFailedEventDetails :: Decode ActivityFailedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityFailedEventDetails :: Encode ActivityFailedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum number of activities has been reached. Existing activities must be deleted before a new activity can be created.</p>
newtype ActivityLimitExceeded = ActivityLimitExceeded 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeActivityLimitExceeded :: Newtype ActivityLimitExceeded _
derive instance repGenericActivityLimitExceeded :: Generic ActivityLimitExceeded _
instance showActivityLimitExceeded :: Show ActivityLimitExceeded where
  show = genericShow
instance decodeActivityLimitExceeded :: Decode ActivityLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityLimitExceeded :: Encode ActivityLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivityList = ActivityList (Array ActivityListItem)
derive instance newtypeActivityList :: Newtype ActivityList _
derive instance repGenericActivityList :: Generic ActivityList _
instance showActivityList :: Show ActivityList where
  show = genericShow
instance decodeActivityList :: Decode ActivityList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityList :: Encode ActivityList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an activity.</p>
newtype ActivityListItem = ActivityListItem 
  { "ActivityArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeActivityListItem :: Newtype ActivityListItem _
derive instance repGenericActivityListItem :: Generic ActivityListItem _
instance showActivityListItem :: Show ActivityListItem where
  show = genericShow
instance decodeActivityListItem :: Decode ActivityListItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityListItem :: Encode ActivityListItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an activity schedule failure which occurred during an execution.</p>
newtype ActivityScheduleFailedEventDetails = ActivityScheduleFailedEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeActivityScheduleFailedEventDetails :: Newtype ActivityScheduleFailedEventDetails _
derive instance repGenericActivityScheduleFailedEventDetails :: Generic ActivityScheduleFailedEventDetails _
instance showActivityScheduleFailedEventDetails :: Show ActivityScheduleFailedEventDetails where
  show = genericShow
instance decodeActivityScheduleFailedEventDetails :: Decode ActivityScheduleFailedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityScheduleFailedEventDetails :: Encode ActivityScheduleFailedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an activity scheduled during an execution.</p>
newtype ActivityScheduledEventDetails = ActivityScheduledEventDetails 
  { "Resource'" :: (Arn)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "TimeoutInSeconds'" :: NullOrUndefined.NullOrUndefined (TimeoutInSeconds)
  , "HeartbeatInSeconds'" :: NullOrUndefined.NullOrUndefined (TimeoutInSeconds)
  }
derive instance newtypeActivityScheduledEventDetails :: Newtype ActivityScheduledEventDetails _
derive instance repGenericActivityScheduledEventDetails :: Generic ActivityScheduledEventDetails _
instance showActivityScheduledEventDetails :: Show ActivityScheduledEventDetails where
  show = genericShow
instance decodeActivityScheduledEventDetails :: Decode ActivityScheduledEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityScheduledEventDetails :: Encode ActivityScheduledEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about the start of an activity during an execution.</p>
newtype ActivityStartedEventDetails = ActivityStartedEventDetails 
  { "WorkerName'" :: NullOrUndefined.NullOrUndefined (Identity)
  }
derive instance newtypeActivityStartedEventDetails :: Newtype ActivityStartedEventDetails _
derive instance repGenericActivityStartedEventDetails :: Generic ActivityStartedEventDetails _
instance showActivityStartedEventDetails :: Show ActivityStartedEventDetails where
  show = genericShow
instance decodeActivityStartedEventDetails :: Decode ActivityStartedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityStartedEventDetails :: Encode ActivityStartedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an activity which successfully terminated during an execution.</p>
newtype ActivitySucceededEventDetails = ActivitySucceededEventDetails 
  { "Output'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeActivitySucceededEventDetails :: Newtype ActivitySucceededEventDetails _
derive instance repGenericActivitySucceededEventDetails :: Generic ActivitySucceededEventDetails _
instance showActivitySucceededEventDetails :: Show ActivitySucceededEventDetails where
  show = genericShow
instance decodeActivitySucceededEventDetails :: Decode ActivitySucceededEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivitySucceededEventDetails :: Encode ActivitySucceededEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an activity timeout which occurred during an execution.</p>
newtype ActivityTimedOutEventDetails = ActivityTimedOutEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeActivityTimedOutEventDetails :: Newtype ActivityTimedOutEventDetails _
derive instance repGenericActivityTimedOutEventDetails :: Generic ActivityTimedOutEventDetails _
instance showActivityTimedOutEventDetails :: Show ActivityTimedOutEventDetails where
  show = genericShow
instance decodeActivityTimedOutEventDetails :: Decode ActivityTimedOutEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityTimedOutEventDetails :: Encode ActivityTimedOutEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum number of workers concurrently polling for activity tasks has been reached.</p>
newtype ActivityWorkerLimitExceeded = ActivityWorkerLimitExceeded 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeActivityWorkerLimitExceeded :: Newtype ActivityWorkerLimitExceeded _
derive instance repGenericActivityWorkerLimitExceeded :: Generic ActivityWorkerLimitExceeded _
instance showActivityWorkerLimitExceeded :: Show ActivityWorkerLimitExceeded where
  show = genericShow
instance decodeActivityWorkerLimitExceeded :: Decode ActivityWorkerLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityWorkerLimitExceeded :: Encode ActivityWorkerLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _
derive instance repGenericArn :: Generic Arn _
instance showArn :: Show Arn where
  show = genericShow
instance decodeArn :: Decode Arn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArn :: Encode Arn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Cause = Cause String
derive instance newtypeCause :: Newtype Cause _
derive instance repGenericCause :: Generic Cause _
instance showCause :: Show Cause where
  show = genericShow
instance decodeCause :: Decode Cause where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCause :: Encode Cause where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateActivityInput = CreateActivityInput 
  { "Name'" :: (Name)
  }
derive instance newtypeCreateActivityInput :: Newtype CreateActivityInput _
derive instance repGenericCreateActivityInput :: Generic CreateActivityInput _
instance showCreateActivityInput :: Show CreateActivityInput where
  show = genericShow
instance decodeCreateActivityInput :: Decode CreateActivityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateActivityInput :: Encode CreateActivityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateActivityOutput = CreateActivityOutput 
  { "ActivityArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeCreateActivityOutput :: Newtype CreateActivityOutput _
derive instance repGenericCreateActivityOutput :: Generic CreateActivityOutput _
instance showCreateActivityOutput :: Show CreateActivityOutput where
  show = genericShow
instance decodeCreateActivityOutput :: Decode CreateActivityOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateActivityOutput :: Encode CreateActivityOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStateMachineInput = CreateStateMachineInput 
  { "Name'" :: (Name)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  }
derive instance newtypeCreateStateMachineInput :: Newtype CreateStateMachineInput _
derive instance repGenericCreateStateMachineInput :: Generic CreateStateMachineInput _
instance showCreateStateMachineInput :: Show CreateStateMachineInput where
  show = genericShow
instance decodeCreateStateMachineInput :: Decode CreateStateMachineInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStateMachineInput :: Encode CreateStateMachineInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateStateMachineOutput = CreateStateMachineOutput 
  { "StateMachineArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeCreateStateMachineOutput :: Newtype CreateStateMachineOutput _
derive instance repGenericCreateStateMachineOutput :: Generic CreateStateMachineOutput _
instance showCreateStateMachineOutput :: Show CreateStateMachineOutput where
  show = genericShow
instance decodeCreateStateMachineOutput :: Decode CreateStateMachineOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateStateMachineOutput :: Encode CreateStateMachineOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Data = Data String
derive instance newtypeData :: Newtype Data _
derive instance repGenericData :: Generic Data _
instance showData :: Show Data where
  show = genericShow
instance decodeData :: Decode Data where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeData :: Encode Data where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Definition = Definition String
derive instance newtypeDefinition :: Newtype Definition _
derive instance repGenericDefinition :: Generic Definition _
instance showDefinition :: Show Definition where
  show = genericShow
instance decodeDefinition :: Decode Definition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefinition :: Encode Definition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteActivityInput = DeleteActivityInput 
  { "ActivityArn'" :: (Arn)
  }
derive instance newtypeDeleteActivityInput :: Newtype DeleteActivityInput _
derive instance repGenericDeleteActivityInput :: Generic DeleteActivityInput _
instance showDeleteActivityInput :: Show DeleteActivityInput where
  show = genericShow
instance decodeDeleteActivityInput :: Decode DeleteActivityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteActivityInput :: Encode DeleteActivityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteActivityOutput = DeleteActivityOutput Types.NoArguments
derive instance newtypeDeleteActivityOutput :: Newtype DeleteActivityOutput _
derive instance repGenericDeleteActivityOutput :: Generic DeleteActivityOutput _
instance showDeleteActivityOutput :: Show DeleteActivityOutput where
  show = genericShow
instance decodeDeleteActivityOutput :: Decode DeleteActivityOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteActivityOutput :: Encode DeleteActivityOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStateMachineInput = DeleteStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  }
derive instance newtypeDeleteStateMachineInput :: Newtype DeleteStateMachineInput _
derive instance repGenericDeleteStateMachineInput :: Generic DeleteStateMachineInput _
instance showDeleteStateMachineInput :: Show DeleteStateMachineInput where
  show = genericShow
instance decodeDeleteStateMachineInput :: Decode DeleteStateMachineInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStateMachineInput :: Encode DeleteStateMachineInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteStateMachineOutput = DeleteStateMachineOutput Types.NoArguments
derive instance newtypeDeleteStateMachineOutput :: Newtype DeleteStateMachineOutput _
derive instance repGenericDeleteStateMachineOutput :: Generic DeleteStateMachineOutput _
instance showDeleteStateMachineOutput :: Show DeleteStateMachineOutput where
  show = genericShow
instance decodeDeleteStateMachineOutput :: Decode DeleteStateMachineOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteStateMachineOutput :: Encode DeleteStateMachineOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivityInput = DescribeActivityInput 
  { "ActivityArn'" :: (Arn)
  }
derive instance newtypeDescribeActivityInput :: Newtype DescribeActivityInput _
derive instance repGenericDescribeActivityInput :: Generic DescribeActivityInput _
instance showDescribeActivityInput :: Show DescribeActivityInput where
  show = genericShow
instance decodeDescribeActivityInput :: Decode DescribeActivityInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivityInput :: Encode DescribeActivityInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivityOutput = DescribeActivityOutput 
  { "ActivityArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeDescribeActivityOutput :: Newtype DescribeActivityOutput _
derive instance repGenericDescribeActivityOutput :: Generic DescribeActivityOutput _
instance showDescribeActivityOutput :: Show DescribeActivityOutput where
  show = genericShow
instance decodeDescribeActivityOutput :: Decode DescribeActivityOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivityOutput :: Encode DescribeActivityOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeExecutionInput = DescribeExecutionInput 
  { "ExecutionArn'" :: (Arn)
  }
derive instance newtypeDescribeExecutionInput :: Newtype DescribeExecutionInput _
derive instance repGenericDescribeExecutionInput :: Generic DescribeExecutionInput _
instance showDescribeExecutionInput :: Show DescribeExecutionInput where
  show = genericShow
instance decodeDescribeExecutionInput :: Decode DescribeExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeExecutionInput :: Encode DescribeExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeExecutionOutput = DescribeExecutionOutput 
  { "ExecutionArn'" :: (Arn)
  , "StateMachineArn'" :: (Arn)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Status'" :: (ExecutionStatus)
  , "StartDate'" :: (Number)
  , "StopDate'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Input'" :: (Data)
  , "Output'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeDescribeExecutionOutput :: Newtype DescribeExecutionOutput _
derive instance repGenericDescribeExecutionOutput :: Generic DescribeExecutionOutput _
instance showDescribeExecutionOutput :: Show DescribeExecutionOutput where
  show = genericShow
instance decodeDescribeExecutionOutput :: Decode DescribeExecutionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeExecutionOutput :: Encode DescribeExecutionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStateMachineForExecutionInput = DescribeStateMachineForExecutionInput 
  { "ExecutionArn'" :: (Arn)
  }
derive instance newtypeDescribeStateMachineForExecutionInput :: Newtype DescribeStateMachineForExecutionInput _
derive instance repGenericDescribeStateMachineForExecutionInput :: Generic DescribeStateMachineForExecutionInput _
instance showDescribeStateMachineForExecutionInput :: Show DescribeStateMachineForExecutionInput where
  show = genericShow
instance decodeDescribeStateMachineForExecutionInput :: Decode DescribeStateMachineForExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStateMachineForExecutionInput :: Encode DescribeStateMachineForExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStateMachineForExecutionOutput = DescribeStateMachineForExecutionOutput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  , "UpdateDate'" :: (Number)
  }
derive instance newtypeDescribeStateMachineForExecutionOutput :: Newtype DescribeStateMachineForExecutionOutput _
derive instance repGenericDescribeStateMachineForExecutionOutput :: Generic DescribeStateMachineForExecutionOutput _
instance showDescribeStateMachineForExecutionOutput :: Show DescribeStateMachineForExecutionOutput where
  show = genericShow
instance decodeDescribeStateMachineForExecutionOutput :: Decode DescribeStateMachineForExecutionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStateMachineForExecutionOutput :: Encode DescribeStateMachineForExecutionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStateMachineInput = DescribeStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  }
derive instance newtypeDescribeStateMachineInput :: Newtype DescribeStateMachineInput _
derive instance repGenericDescribeStateMachineInput :: Generic DescribeStateMachineInput _
instance showDescribeStateMachineInput :: Show DescribeStateMachineInput where
  show = genericShow
instance decodeDescribeStateMachineInput :: Decode DescribeStateMachineInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStateMachineInput :: Encode DescribeStateMachineInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeStateMachineOutput = DescribeStateMachineOutput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Status'" :: NullOrUndefined.NullOrUndefined (StateMachineStatus)
  , "Definition'" :: (Definition)
  , "RoleArn'" :: (Arn)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeDescribeStateMachineOutput :: Newtype DescribeStateMachineOutput _
derive instance repGenericDescribeStateMachineOutput :: Generic DescribeStateMachineOutput _
instance showDescribeStateMachineOutput :: Show DescribeStateMachineOutput where
  show = genericShow
instance decodeDescribeStateMachineOutput :: Decode DescribeStateMachineOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeStateMachineOutput :: Encode DescribeStateMachineOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Error = Error String
derive instance newtypeError :: Newtype Error _
derive instance repGenericError :: Generic Error _
instance showError :: Show Error where
  show = genericShow
instance decodeError :: Decode Error where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeError :: Encode Error where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventId = EventId Number
derive instance newtypeEventId :: Newtype EventId _
derive instance repGenericEventId :: Generic EventId _
instance showEventId :: Show EventId where
  show = genericShow
instance decodeEventId :: Decode EventId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventId :: Encode EventId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an abort of an execution.</p>
newtype ExecutionAbortedEventDetails = ExecutionAbortedEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeExecutionAbortedEventDetails :: Newtype ExecutionAbortedEventDetails _
derive instance repGenericExecutionAbortedEventDetails :: Generic ExecutionAbortedEventDetails _
instance showExecutionAbortedEventDetails :: Show ExecutionAbortedEventDetails where
  show = genericShow
instance decodeExecutionAbortedEventDetails :: Decode ExecutionAbortedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionAbortedEventDetails :: Encode ExecutionAbortedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The execution has the same <code>name</code> as another execution (but a different <code>input</code>).</p> <note> <p>Executions with the same <code>name</code> and <code>input</code> are considered idempotent.</p> </note>
newtype ExecutionAlreadyExists = ExecutionAlreadyExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExecutionAlreadyExists :: Newtype ExecutionAlreadyExists _
derive instance repGenericExecutionAlreadyExists :: Generic ExecutionAlreadyExists _
instance showExecutionAlreadyExists :: Show ExecutionAlreadyExists where
  show = genericShow
instance decodeExecutionAlreadyExists :: Decode ExecutionAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionAlreadyExists :: Encode ExecutionAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified execution does not exist.</p>
newtype ExecutionDoesNotExist = ExecutionDoesNotExist 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExecutionDoesNotExist :: Newtype ExecutionDoesNotExist _
derive instance repGenericExecutionDoesNotExist :: Generic ExecutionDoesNotExist _
instance showExecutionDoesNotExist :: Show ExecutionDoesNotExist where
  show = genericShow
instance decodeExecutionDoesNotExist :: Decode ExecutionDoesNotExist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionDoesNotExist :: Encode ExecutionDoesNotExist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an execution failure event.</p>
newtype ExecutionFailedEventDetails = ExecutionFailedEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeExecutionFailedEventDetails :: Newtype ExecutionFailedEventDetails _
derive instance repGenericExecutionFailedEventDetails :: Generic ExecutionFailedEventDetails _
instance showExecutionFailedEventDetails :: Show ExecutionFailedEventDetails where
  show = genericShow
instance decodeExecutionFailedEventDetails :: Decode ExecutionFailedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionFailedEventDetails :: Encode ExecutionFailedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum number of running executions has been reached. Running executions must end or be stopped before a new execution can be started.</p>
newtype ExecutionLimitExceeded = ExecutionLimitExceeded 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeExecutionLimitExceeded :: Newtype ExecutionLimitExceeded _
derive instance repGenericExecutionLimitExceeded :: Generic ExecutionLimitExceeded _
instance showExecutionLimitExceeded :: Show ExecutionLimitExceeded where
  show = genericShow
instance decodeExecutionLimitExceeded :: Decode ExecutionLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionLimitExceeded :: Encode ExecutionLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionList = ExecutionList (Array ExecutionListItem)
derive instance newtypeExecutionList :: Newtype ExecutionList _
derive instance repGenericExecutionList :: Generic ExecutionList _
instance showExecutionList :: Show ExecutionList where
  show = genericShow
instance decodeExecutionList :: Decode ExecutionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionList :: Encode ExecutionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an execution.</p>
newtype ExecutionListItem = ExecutionListItem 
  { "ExecutionArn'" :: (Arn)
  , "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "Status'" :: (ExecutionStatus)
  , "StartDate'" :: (Number)
  , "StopDate'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeExecutionListItem :: Newtype ExecutionListItem _
derive instance repGenericExecutionListItem :: Generic ExecutionListItem _
instance showExecutionListItem :: Show ExecutionListItem where
  show = genericShow
instance decodeExecutionListItem :: Decode ExecutionListItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionListItem :: Encode ExecutionListItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about the start of the execution.</p>
newtype ExecutionStartedEventDetails = ExecutionStartedEventDetails 
  { "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeExecutionStartedEventDetails :: Newtype ExecutionStartedEventDetails _
derive instance repGenericExecutionStartedEventDetails :: Generic ExecutionStartedEventDetails _
instance showExecutionStartedEventDetails :: Show ExecutionStartedEventDetails where
  show = genericShow
instance decodeExecutionStartedEventDetails :: Decode ExecutionStartedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionStartedEventDetails :: Encode ExecutionStartedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionStatus = ExecutionStatus String
derive instance newtypeExecutionStatus :: Newtype ExecutionStatus _
derive instance repGenericExecutionStatus :: Generic ExecutionStatus _
instance showExecutionStatus :: Show ExecutionStatus where
  show = genericShow
instance decodeExecutionStatus :: Decode ExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionStatus :: Encode ExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about the successful termination of the execution.</p>
newtype ExecutionSucceededEventDetails = ExecutionSucceededEventDetails 
  { "Output'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeExecutionSucceededEventDetails :: Newtype ExecutionSucceededEventDetails _
derive instance repGenericExecutionSucceededEventDetails :: Generic ExecutionSucceededEventDetails _
instance showExecutionSucceededEventDetails :: Show ExecutionSucceededEventDetails where
  show = genericShow
instance decodeExecutionSucceededEventDetails :: Decode ExecutionSucceededEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionSucceededEventDetails :: Encode ExecutionSucceededEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about the execution timeout which occurred during the execution.</p>
newtype ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeExecutionTimedOutEventDetails :: Newtype ExecutionTimedOutEventDetails _
derive instance repGenericExecutionTimedOutEventDetails :: Generic ExecutionTimedOutEventDetails _
instance showExecutionTimedOutEventDetails :: Show ExecutionTimedOutEventDetails where
  show = genericShow
instance decodeExecutionTimedOutEventDetails :: Decode ExecutionTimedOutEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionTimedOutEventDetails :: Encode ExecutionTimedOutEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetActivityTaskInput = GetActivityTaskInput 
  { "ActivityArn'" :: (Arn)
  , "WorkerName'" :: NullOrUndefined.NullOrUndefined (Name)
  }
derive instance newtypeGetActivityTaskInput :: Newtype GetActivityTaskInput _
derive instance repGenericGetActivityTaskInput :: Generic GetActivityTaskInput _
instance showGetActivityTaskInput :: Show GetActivityTaskInput where
  show = genericShow
instance decodeGetActivityTaskInput :: Decode GetActivityTaskInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetActivityTaskInput :: Encode GetActivityTaskInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetActivityTaskOutput = GetActivityTaskOutput 
  { "TaskToken'" :: NullOrUndefined.NullOrUndefined (TaskToken)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeGetActivityTaskOutput :: Newtype GetActivityTaskOutput _
derive instance repGenericGetActivityTaskOutput :: Generic GetActivityTaskOutput _
instance showGetActivityTaskOutput :: Show GetActivityTaskOutput where
  show = genericShow
instance decodeGetActivityTaskOutput :: Decode GetActivityTaskOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetActivityTaskOutput :: Encode GetActivityTaskOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetExecutionHistoryInput = GetExecutionHistoryInput 
  { "ExecutionArn'" :: (Arn)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "ReverseOrder'" :: NullOrUndefined.NullOrUndefined (ReverseOrder)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeGetExecutionHistoryInput :: Newtype GetExecutionHistoryInput _
derive instance repGenericGetExecutionHistoryInput :: Generic GetExecutionHistoryInput _
instance showGetExecutionHistoryInput :: Show GetExecutionHistoryInput where
  show = genericShow
instance decodeGetExecutionHistoryInput :: Decode GetExecutionHistoryInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetExecutionHistoryInput :: Encode GetExecutionHistoryInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetExecutionHistoryOutput = GetExecutionHistoryOutput 
  { "Events'" :: (HistoryEventList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeGetExecutionHistoryOutput :: Newtype GetExecutionHistoryOutput _
derive instance repGenericGetExecutionHistoryOutput :: Generic GetExecutionHistoryOutput _
instance showGetExecutionHistoryOutput :: Show GetExecutionHistoryOutput where
  show = genericShow
instance decodeGetExecutionHistoryOutput :: Decode GetExecutionHistoryOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetExecutionHistoryOutput :: Encode GetExecutionHistoryOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about the events of an execution.</p>
newtype HistoryEvent = HistoryEvent 
  { "Number" :: (Number)
  , "Type'" :: (HistoryEventType)
  , "Id'" :: (EventId)
  , "PreviousEventId'" :: NullOrUndefined.NullOrUndefined (EventId)
  , "ActivityFailedEventDetails'" :: NullOrUndefined.NullOrUndefined (ActivityFailedEventDetails)
  , "ActivityScheduleFailedEventDetails'" :: NullOrUndefined.NullOrUndefined (ActivityScheduleFailedEventDetails)
  , "ActivityScheduledEventDetails'" :: NullOrUndefined.NullOrUndefined (ActivityScheduledEventDetails)
  , "ActivityStartedEventDetails'" :: NullOrUndefined.NullOrUndefined (ActivityStartedEventDetails)
  , "ActivitySucceededEventDetails'" :: NullOrUndefined.NullOrUndefined (ActivitySucceededEventDetails)
  , "ActivityTimedOutEventDetails'" :: NullOrUndefined.NullOrUndefined (ActivityTimedOutEventDetails)
  , "ExecutionFailedEventDetails'" :: NullOrUndefined.NullOrUndefined (ExecutionFailedEventDetails)
  , "ExecutionStartedEventDetails'" :: NullOrUndefined.NullOrUndefined (ExecutionStartedEventDetails)
  , "ExecutionSucceededEventDetails'" :: NullOrUndefined.NullOrUndefined (ExecutionSucceededEventDetails)
  , "ExecutionAbortedEventDetails'" :: NullOrUndefined.NullOrUndefined (ExecutionAbortedEventDetails)
  , "ExecutionTimedOutEventDetails'" :: NullOrUndefined.NullOrUndefined (ExecutionTimedOutEventDetails)
  , "LambdaFunctionFailedEventDetails'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionFailedEventDetails)
  , "LambdaFunctionScheduleFailedEventDetails'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionScheduleFailedEventDetails)
  , "LambdaFunctionScheduledEventDetails'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionScheduledEventDetails)
  , "LambdaFunctionStartFailedEventDetails'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionStartFailedEventDetails)
  , "LambdaFunctionSucceededEventDetails'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionSucceededEventDetails)
  , "LambdaFunctionTimedOutEventDetails'" :: NullOrUndefined.NullOrUndefined (LambdaFunctionTimedOutEventDetails)
  , "StateEnteredEventDetails'" :: NullOrUndefined.NullOrUndefined (StateEnteredEventDetails)
  , "StateExitedEventDetails'" :: NullOrUndefined.NullOrUndefined (StateExitedEventDetails)
  }
derive instance newtypeHistoryEvent :: Newtype HistoryEvent _
derive instance repGenericHistoryEvent :: Generic HistoryEvent _
instance showHistoryEvent :: Show HistoryEvent where
  show = genericShow
instance decodeHistoryEvent :: Decode HistoryEvent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistoryEvent :: Encode HistoryEvent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about the events which occurred during an execution.</p>
newtype HistoryEventList = HistoryEventList (Array HistoryEvent)
derive instance newtypeHistoryEventList :: Newtype HistoryEventList _
derive instance repGenericHistoryEventList :: Generic HistoryEventList _
instance showHistoryEventList :: Show HistoryEventList where
  show = genericShow
instance decodeHistoryEventList :: Decode HistoryEventList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistoryEventList :: Encode HistoryEventList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HistoryEventType = HistoryEventType String
derive instance newtypeHistoryEventType :: Newtype HistoryEventType _
derive instance repGenericHistoryEventType :: Generic HistoryEventType _
instance showHistoryEventType :: Show HistoryEventType where
  show = genericShow
instance decodeHistoryEventType :: Decode HistoryEventType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHistoryEventType :: Encode HistoryEventType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Identity = Identity String
derive instance newtypeIdentity :: Newtype Identity _
derive instance repGenericIdentity :: Generic Identity _
instance showIdentity :: Show Identity where
  show = genericShow
instance decodeIdentity :: Decode Identity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdentity :: Encode Identity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The provided Amazon Resource Name (ARN) is invalid.</p>
newtype InvalidArn = InvalidArn 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidArn :: Newtype InvalidArn _
derive instance repGenericInvalidArn :: Generic InvalidArn _
instance showInvalidArn :: Show InvalidArn where
  show = genericShow
instance decodeInvalidArn :: Decode InvalidArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArn :: Encode InvalidArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The provided Amazon States Language definition is invalid.</p>
newtype InvalidDefinition = InvalidDefinition 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidDefinition :: Newtype InvalidDefinition _
derive instance repGenericInvalidDefinition :: Generic InvalidDefinition _
instance showInvalidDefinition :: Show InvalidDefinition where
  show = genericShow
instance decodeInvalidDefinition :: Decode InvalidDefinition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidDefinition :: Encode InvalidDefinition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The provided JSON input data is invalid.</p>
newtype InvalidExecutionInput = InvalidExecutionInput 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidExecutionInput :: Newtype InvalidExecutionInput _
derive instance repGenericInvalidExecutionInput :: Generic InvalidExecutionInput _
instance showInvalidExecutionInput :: Show InvalidExecutionInput where
  show = genericShow
instance decodeInvalidExecutionInput :: Decode InvalidExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidExecutionInput :: Encode InvalidExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The provided name is invalid.</p>
newtype InvalidName = InvalidName 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidName :: Newtype InvalidName _
derive instance repGenericInvalidName :: Generic InvalidName _
instance showInvalidName :: Show InvalidName where
  show = genericShow
instance decodeInvalidName :: Decode InvalidName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidName :: Encode InvalidName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The provided JSON output data is invalid.</p>
newtype InvalidOutput = InvalidOutput 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidOutput :: Newtype InvalidOutput _
derive instance repGenericInvalidOutput :: Generic InvalidOutput _
instance showInvalidOutput :: Show InvalidOutput where
  show = genericShow
instance decodeInvalidOutput :: Decode InvalidOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOutput :: Encode InvalidOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The provided token is invalid.</p>
newtype InvalidToken = InvalidToken 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidToken :: Newtype InvalidToken _
derive instance repGenericInvalidToken :: Generic InvalidToken _
instance showInvalidToken :: Show InvalidToken where
  show = genericShow
instance decodeInvalidToken :: Decode InvalidToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidToken :: Encode InvalidToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a lambda function which failed during an execution.</p>
newtype LambdaFunctionFailedEventDetails = LambdaFunctionFailedEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeLambdaFunctionFailedEventDetails :: Newtype LambdaFunctionFailedEventDetails _
derive instance repGenericLambdaFunctionFailedEventDetails :: Generic LambdaFunctionFailedEventDetails _
instance showLambdaFunctionFailedEventDetails :: Show LambdaFunctionFailedEventDetails where
  show = genericShow
instance decodeLambdaFunctionFailedEventDetails :: Decode LambdaFunctionFailedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionFailedEventDetails :: Encode LambdaFunctionFailedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a failed lambda function schedule event which occurred during an execution.</p>
newtype LambdaFunctionScheduleFailedEventDetails = LambdaFunctionScheduleFailedEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeLambdaFunctionScheduleFailedEventDetails :: Newtype LambdaFunctionScheduleFailedEventDetails _
derive instance repGenericLambdaFunctionScheduleFailedEventDetails :: Generic LambdaFunctionScheduleFailedEventDetails _
instance showLambdaFunctionScheduleFailedEventDetails :: Show LambdaFunctionScheduleFailedEventDetails where
  show = genericShow
instance decodeLambdaFunctionScheduleFailedEventDetails :: Decode LambdaFunctionScheduleFailedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionScheduleFailedEventDetails :: Encode LambdaFunctionScheduleFailedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a lambda function scheduled during an execution.</p>
newtype LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails 
  { "Resource'" :: (Arn)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  , "TimeoutInSeconds'" :: NullOrUndefined.NullOrUndefined (TimeoutInSeconds)
  }
derive instance newtypeLambdaFunctionScheduledEventDetails :: Newtype LambdaFunctionScheduledEventDetails _
derive instance repGenericLambdaFunctionScheduledEventDetails :: Generic LambdaFunctionScheduledEventDetails _
instance showLambdaFunctionScheduledEventDetails :: Show LambdaFunctionScheduledEventDetails where
  show = genericShow
instance decodeLambdaFunctionScheduledEventDetails :: Decode LambdaFunctionScheduledEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionScheduledEventDetails :: Encode LambdaFunctionScheduledEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a lambda function which failed to start during an execution.</p>
newtype LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeLambdaFunctionStartFailedEventDetails :: Newtype LambdaFunctionStartFailedEventDetails _
derive instance repGenericLambdaFunctionStartFailedEventDetails :: Generic LambdaFunctionStartFailedEventDetails _
instance showLambdaFunctionStartFailedEventDetails :: Show LambdaFunctionStartFailedEventDetails where
  show = genericShow
instance decodeLambdaFunctionStartFailedEventDetails :: Decode LambdaFunctionStartFailedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionStartFailedEventDetails :: Encode LambdaFunctionStartFailedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a lambda function which successfully terminated during an execution.</p>
newtype LambdaFunctionSucceededEventDetails = LambdaFunctionSucceededEventDetails 
  { "Output'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeLambdaFunctionSucceededEventDetails :: Newtype LambdaFunctionSucceededEventDetails _
derive instance repGenericLambdaFunctionSucceededEventDetails :: Generic LambdaFunctionSucceededEventDetails _
instance showLambdaFunctionSucceededEventDetails :: Show LambdaFunctionSucceededEventDetails where
  show = genericShow
instance decodeLambdaFunctionSucceededEventDetails :: Decode LambdaFunctionSucceededEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionSucceededEventDetails :: Encode LambdaFunctionSucceededEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a lambda function timeout which occurred during an execution.</p>
newtype LambdaFunctionTimedOutEventDetails = LambdaFunctionTimedOutEventDetails 
  { "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeLambdaFunctionTimedOutEventDetails :: Newtype LambdaFunctionTimedOutEventDetails _
derive instance repGenericLambdaFunctionTimedOutEventDetails :: Generic LambdaFunctionTimedOutEventDetails _
instance showLambdaFunctionTimedOutEventDetails :: Show LambdaFunctionTimedOutEventDetails where
  show = genericShow
instance decodeLambdaFunctionTimedOutEventDetails :: Decode LambdaFunctionTimedOutEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLambdaFunctionTimedOutEventDetails :: Encode LambdaFunctionTimedOutEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListActivitiesInput = ListActivitiesInput 
  { "MaxResults'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeListActivitiesInput :: Newtype ListActivitiesInput _
derive instance repGenericListActivitiesInput :: Generic ListActivitiesInput _
instance showListActivitiesInput :: Show ListActivitiesInput where
  show = genericShow
instance decodeListActivitiesInput :: Decode ListActivitiesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListActivitiesInput :: Encode ListActivitiesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListActivitiesOutput = ListActivitiesOutput 
  { "Activities'" :: (ActivityList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeListActivitiesOutput :: Newtype ListActivitiesOutput _
derive instance repGenericListActivitiesOutput :: Generic ListActivitiesOutput _
instance showListActivitiesOutput :: Show ListActivitiesOutput where
  show = genericShow
instance decodeListActivitiesOutput :: Decode ListActivitiesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListActivitiesOutput :: Encode ListActivitiesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListExecutionsInput = ListExecutionsInput 
  { "StateMachineArn'" :: (Arn)
  , "StatusFilter'" :: NullOrUndefined.NullOrUndefined (ExecutionStatus)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeListExecutionsInput :: Newtype ListExecutionsInput _
derive instance repGenericListExecutionsInput :: Generic ListExecutionsInput _
instance showListExecutionsInput :: Show ListExecutionsInput where
  show = genericShow
instance decodeListExecutionsInput :: Decode ListExecutionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListExecutionsInput :: Encode ListExecutionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListExecutionsOutput = ListExecutionsOutput 
  { "Executions'" :: (ExecutionList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeListExecutionsOutput :: Newtype ListExecutionsOutput _
derive instance repGenericListExecutionsOutput :: Generic ListExecutionsOutput _
instance showListExecutionsOutput :: Show ListExecutionsOutput where
  show = genericShow
instance decodeListExecutionsOutput :: Decode ListExecutionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListExecutionsOutput :: Encode ListExecutionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStateMachinesInput = ListStateMachinesInput 
  { "MaxResults'" :: NullOrUndefined.NullOrUndefined (PageSize)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeListStateMachinesInput :: Newtype ListStateMachinesInput _
derive instance repGenericListStateMachinesInput :: Generic ListStateMachinesInput _
instance showListStateMachinesInput :: Show ListStateMachinesInput where
  show = genericShow
instance decodeListStateMachinesInput :: Decode ListStateMachinesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStateMachinesInput :: Encode ListStateMachinesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListStateMachinesOutput = ListStateMachinesOutput 
  { "StateMachines'" :: (StateMachineList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (PageToken)
  }
derive instance newtypeListStateMachinesOutput :: Newtype ListStateMachinesOutput _
derive instance repGenericListStateMachinesOutput :: Generic ListStateMachinesOutput _
instance showListStateMachinesOutput :: Show ListStateMachinesOutput where
  show = genericShow
instance decodeListStateMachinesOutput :: Decode ListStateMachinesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListStateMachinesOutput :: Encode ListStateMachinesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Request is missing a required parameter. This error occurs if both <code>definition</code> and <code>roleArn</code> are not specified.</p>
newtype MissingRequiredParameter = MissingRequiredParameter 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeMissingRequiredParameter :: Newtype MissingRequiredParameter _
derive instance repGenericMissingRequiredParameter :: Generic MissingRequiredParameter _
instance showMissingRequiredParameter :: Show MissingRequiredParameter where
  show = genericShow
instance decodeMissingRequiredParameter :: Decode MissingRequiredParameter where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMissingRequiredParameter :: Encode MissingRequiredParameter where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Name = Name String
derive instance newtypeName :: Newtype Name _
derive instance repGenericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow
instance decodeName :: Decode Name where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeName :: Encode Name where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageSize = PageSize Int
derive instance newtypePageSize :: Newtype PageSize _
derive instance repGenericPageSize :: Generic PageSize _
instance showPageSize :: Show PageSize where
  show = genericShow
instance decodePageSize :: Decode PageSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageSize :: Encode PageSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageToken = PageToken String
derive instance newtypePageToken :: Newtype PageToken _
derive instance repGenericPageToken :: Generic PageToken _
instance showPageToken :: Show PageToken where
  show = genericShow
instance decodePageToken :: Decode PageToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageToken :: Encode PageToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReverseOrder = ReverseOrder Boolean
derive instance newtypeReverseOrder :: Newtype ReverseOrder _
derive instance repGenericReverseOrder :: Generic ReverseOrder _
instance showReverseOrder :: Show ReverseOrder where
  show = genericShow
instance decodeReverseOrder :: Decode ReverseOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReverseOrder :: Encode ReverseOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendTaskFailureInput = SendTaskFailureInput 
  { "TaskToken'" :: (TaskToken)
  , "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeSendTaskFailureInput :: Newtype SendTaskFailureInput _
derive instance repGenericSendTaskFailureInput :: Generic SendTaskFailureInput _
instance showSendTaskFailureInput :: Show SendTaskFailureInput where
  show = genericShow
instance decodeSendTaskFailureInput :: Decode SendTaskFailureInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendTaskFailureInput :: Encode SendTaskFailureInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendTaskFailureOutput = SendTaskFailureOutput Types.NoArguments
derive instance newtypeSendTaskFailureOutput :: Newtype SendTaskFailureOutput _
derive instance repGenericSendTaskFailureOutput :: Generic SendTaskFailureOutput _
instance showSendTaskFailureOutput :: Show SendTaskFailureOutput where
  show = genericShow
instance decodeSendTaskFailureOutput :: Decode SendTaskFailureOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendTaskFailureOutput :: Encode SendTaskFailureOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendTaskHeartbeatInput = SendTaskHeartbeatInput 
  { "TaskToken'" :: (TaskToken)
  }
derive instance newtypeSendTaskHeartbeatInput :: Newtype SendTaskHeartbeatInput _
derive instance repGenericSendTaskHeartbeatInput :: Generic SendTaskHeartbeatInput _
instance showSendTaskHeartbeatInput :: Show SendTaskHeartbeatInput where
  show = genericShow
instance decodeSendTaskHeartbeatInput :: Decode SendTaskHeartbeatInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendTaskHeartbeatInput :: Encode SendTaskHeartbeatInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendTaskHeartbeatOutput = SendTaskHeartbeatOutput Types.NoArguments
derive instance newtypeSendTaskHeartbeatOutput :: Newtype SendTaskHeartbeatOutput _
derive instance repGenericSendTaskHeartbeatOutput :: Generic SendTaskHeartbeatOutput _
instance showSendTaskHeartbeatOutput :: Show SendTaskHeartbeatOutput where
  show = genericShow
instance decodeSendTaskHeartbeatOutput :: Decode SendTaskHeartbeatOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendTaskHeartbeatOutput :: Encode SendTaskHeartbeatOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendTaskSuccessInput = SendTaskSuccessInput 
  { "TaskToken'" :: (TaskToken)
  , "Output'" :: (Data)
  }
derive instance newtypeSendTaskSuccessInput :: Newtype SendTaskSuccessInput _
derive instance repGenericSendTaskSuccessInput :: Generic SendTaskSuccessInput _
instance showSendTaskSuccessInput :: Show SendTaskSuccessInput where
  show = genericShow
instance decodeSendTaskSuccessInput :: Decode SendTaskSuccessInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendTaskSuccessInput :: Encode SendTaskSuccessInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SendTaskSuccessOutput = SendTaskSuccessOutput Types.NoArguments
derive instance newtypeSendTaskSuccessOutput :: Newtype SendTaskSuccessOutput _
derive instance repGenericSendTaskSuccessOutput :: Generic SendTaskSuccessOutput _
instance showSendTaskSuccessOutput :: Show SendTaskSuccessOutput where
  show = genericShow
instance decodeSendTaskSuccessOutput :: Decode SendTaskSuccessOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSendTaskSuccessOutput :: Encode SendTaskSuccessOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartExecutionInput = StartExecutionInput 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: NullOrUndefined.NullOrUndefined (Name)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeStartExecutionInput :: Newtype StartExecutionInput _
derive instance repGenericStartExecutionInput :: Generic StartExecutionInput _
instance showStartExecutionInput :: Show StartExecutionInput where
  show = genericShow
instance decodeStartExecutionInput :: Decode StartExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartExecutionInput :: Encode StartExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartExecutionOutput = StartExecutionOutput 
  { "ExecutionArn'" :: (Arn)
  , "StartDate'" :: (Number)
  }
derive instance newtypeStartExecutionOutput :: Newtype StartExecutionOutput _
derive instance repGenericStartExecutionOutput :: Generic StartExecutionOutput _
instance showStartExecutionOutput :: Show StartExecutionOutput where
  show = genericShow
instance decodeStartExecutionOutput :: Decode StartExecutionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartExecutionOutput :: Encode StartExecutionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about a state entered during an execution.</p>
newtype StateEnteredEventDetails = StateEnteredEventDetails 
  { "Name'" :: (Name)
  , "Input'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeStateEnteredEventDetails :: Newtype StateEnteredEventDetails _
derive instance repGenericStateEnteredEventDetails :: Generic StateEnteredEventDetails _
instance showStateEnteredEventDetails :: Show StateEnteredEventDetails where
  show = genericShow
instance decodeStateEnteredEventDetails :: Decode StateEnteredEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateEnteredEventDetails :: Encode StateEnteredEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about an exit from a state during an execution.</p>
newtype StateExitedEventDetails = StateExitedEventDetails 
  { "Name'" :: (Name)
  , "Output'" :: NullOrUndefined.NullOrUndefined (Data)
  }
derive instance newtypeStateExitedEventDetails :: Newtype StateExitedEventDetails _
derive instance repGenericStateExitedEventDetails :: Generic StateExitedEventDetails _
instance showStateExitedEventDetails :: Show StateExitedEventDetails where
  show = genericShow
instance decodeStateExitedEventDetails :: Decode StateExitedEventDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateExitedEventDetails :: Encode StateExitedEventDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A state machine with the same name but a different definition or role ARN already exists.</p>
newtype StateMachineAlreadyExists = StateMachineAlreadyExists 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeStateMachineAlreadyExists :: Newtype StateMachineAlreadyExists _
derive instance repGenericStateMachineAlreadyExists :: Generic StateMachineAlreadyExists _
instance showStateMachineAlreadyExists :: Show StateMachineAlreadyExists where
  show = genericShow
instance decodeStateMachineAlreadyExists :: Decode StateMachineAlreadyExists where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateMachineAlreadyExists :: Encode StateMachineAlreadyExists where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified state machine is being deleted.</p>
newtype StateMachineDeleting = StateMachineDeleting 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeStateMachineDeleting :: Newtype StateMachineDeleting _
derive instance repGenericStateMachineDeleting :: Generic StateMachineDeleting _
instance showStateMachineDeleting :: Show StateMachineDeleting where
  show = genericShow
instance decodeStateMachineDeleting :: Decode StateMachineDeleting where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateMachineDeleting :: Encode StateMachineDeleting where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified state machine does not exist.</p>
newtype StateMachineDoesNotExist = StateMachineDoesNotExist 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeStateMachineDoesNotExist :: Newtype StateMachineDoesNotExist _
derive instance repGenericStateMachineDoesNotExist :: Generic StateMachineDoesNotExist _
instance showStateMachineDoesNotExist :: Show StateMachineDoesNotExist where
  show = genericShow
instance decodeStateMachineDoesNotExist :: Decode StateMachineDoesNotExist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateMachineDoesNotExist :: Encode StateMachineDoesNotExist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum number of state machines has been reached. Existing state machines must be deleted before a new state machine can be created.</p>
newtype StateMachineLimitExceeded = StateMachineLimitExceeded 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeStateMachineLimitExceeded :: Newtype StateMachineLimitExceeded _
derive instance repGenericStateMachineLimitExceeded :: Generic StateMachineLimitExceeded _
instance showStateMachineLimitExceeded :: Show StateMachineLimitExceeded where
  show = genericShow
instance decodeStateMachineLimitExceeded :: Decode StateMachineLimitExceeded where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateMachineLimitExceeded :: Encode StateMachineLimitExceeded where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateMachineList = StateMachineList (Array StateMachineListItem)
derive instance newtypeStateMachineList :: Newtype StateMachineList _
derive instance repGenericStateMachineList :: Generic StateMachineList _
instance showStateMachineList :: Show StateMachineList where
  show = genericShow
instance decodeStateMachineList :: Decode StateMachineList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateMachineList :: Encode StateMachineList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains details about the state machine.</p>
newtype StateMachineListItem = StateMachineListItem 
  { "StateMachineArn'" :: (Arn)
  , "Name'" :: (Name)
  , "CreationDate'" :: (Number)
  }
derive instance newtypeStateMachineListItem :: Newtype StateMachineListItem _
derive instance repGenericStateMachineListItem :: Generic StateMachineListItem _
instance showStateMachineListItem :: Show StateMachineListItem where
  show = genericShow
instance decodeStateMachineListItem :: Decode StateMachineListItem where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateMachineListItem :: Encode StateMachineListItem where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateMachineStatus = StateMachineStatus String
derive instance newtypeStateMachineStatus :: Newtype StateMachineStatus _
derive instance repGenericStateMachineStatus :: Generic StateMachineStatus _
instance showStateMachineStatus :: Show StateMachineStatus where
  show = genericShow
instance decodeStateMachineStatus :: Decode StateMachineStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateMachineStatus :: Encode StateMachineStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopExecutionInput = StopExecutionInput 
  { "ExecutionArn'" :: (Arn)
  , "Error'" :: NullOrUndefined.NullOrUndefined (Error)
  , "Cause'" :: NullOrUndefined.NullOrUndefined (Cause)
  }
derive instance newtypeStopExecutionInput :: Newtype StopExecutionInput _
derive instance repGenericStopExecutionInput :: Generic StopExecutionInput _
instance showStopExecutionInput :: Show StopExecutionInput where
  show = genericShow
instance decodeStopExecutionInput :: Decode StopExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopExecutionInput :: Encode StopExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopExecutionOutput = StopExecutionOutput 
  { "StopDate'" :: (Number)
  }
derive instance newtypeStopExecutionOutput :: Newtype StopExecutionOutput _
derive instance repGenericStopExecutionOutput :: Generic StopExecutionOutput _
instance showStopExecutionOutput :: Show StopExecutionOutput where
  show = genericShow
instance decodeStopExecutionOutput :: Decode StopExecutionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopExecutionOutput :: Encode StopExecutionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskDoesNotExist = TaskDoesNotExist 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTaskDoesNotExist :: Newtype TaskDoesNotExist _
derive instance repGenericTaskDoesNotExist :: Generic TaskDoesNotExist _
instance showTaskDoesNotExist :: Show TaskDoesNotExist where
  show = genericShow
instance decodeTaskDoesNotExist :: Decode TaskDoesNotExist where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskDoesNotExist :: Encode TaskDoesNotExist where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskTimedOut = TaskTimedOut 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTaskTimedOut :: Newtype TaskTimedOut _
derive instance repGenericTaskTimedOut :: Generic TaskTimedOut _
instance showTaskTimedOut :: Show TaskTimedOut where
  show = genericShow
instance decodeTaskTimedOut :: Decode TaskTimedOut where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskTimedOut :: Encode TaskTimedOut where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TaskToken = TaskToken String
derive instance newtypeTaskToken :: Newtype TaskToken _
derive instance repGenericTaskToken :: Generic TaskToken _
instance showTaskToken :: Show TaskToken where
  show = genericShow
instance decodeTaskToken :: Decode TaskToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTaskToken :: Encode TaskToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeoutInSeconds = TimeoutInSeconds Number
derive instance newtypeTimeoutInSeconds :: Newtype TimeoutInSeconds _
derive instance repGenericTimeoutInSeconds :: Generic TimeoutInSeconds _
instance showTimeoutInSeconds :: Show TimeoutInSeconds where
  show = genericShow
instance decodeTimeoutInSeconds :: Decode TimeoutInSeconds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeoutInSeconds :: Encode TimeoutInSeconds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStateMachineInput = UpdateStateMachineInput 
  { "StateMachineArn'" :: (Arn)
  , "Definition'" :: NullOrUndefined.NullOrUndefined (Definition)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (Arn)
  }
derive instance newtypeUpdateStateMachineInput :: Newtype UpdateStateMachineInput _
derive instance repGenericUpdateStateMachineInput :: Generic UpdateStateMachineInput _
instance showUpdateStateMachineInput :: Show UpdateStateMachineInput where
  show = genericShow
instance decodeUpdateStateMachineInput :: Decode UpdateStateMachineInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStateMachineInput :: Encode UpdateStateMachineInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateStateMachineOutput = UpdateStateMachineOutput 
  { "UpdateDate'" :: (Number)
  }
derive instance newtypeUpdateStateMachineOutput :: Newtype UpdateStateMachineOutput _
derive instance repGenericUpdateStateMachineOutput :: Generic UpdateStateMachineOutput _
instance showUpdateStateMachineOutput :: Show UpdateStateMachineOutput where
  show = genericShow
instance decodeUpdateStateMachineOutput :: Decode UpdateStateMachineOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateStateMachineOutput :: Encode UpdateStateMachineOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
