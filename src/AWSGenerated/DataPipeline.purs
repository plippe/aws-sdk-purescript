

-- | <p>AWS Data Pipeline configures and manages a data-driven workflow called a pipeline. AWS Data Pipeline handles the details of scheduling and ensuring that data dependencies are met so that your application can focus on processing the data.</p> <p>AWS Data Pipeline provides a JAR implementation of a task runner called AWS Data Pipeline Task Runner. AWS Data Pipeline Task Runner provides logic for common data management scenarios, such as performing database queries and running data analysis using Amazon Elastic MapReduce (Amazon EMR). You can use AWS Data Pipeline Task Runner as your task runner, or you can write your own task runner to provide custom data management.</p> <p>AWS Data Pipeline implements two main sets of functionality. Use the first set to create a pipeline and define data sources, schedules, dependencies, and the transforms to be performed on the data. Use the second set in your task runner application to receive the next task ready for processing. The logic for performing the task, such as querying the data, running data analysis, or converting the data from one format to another, is contained within the task runner. The task runner performs the task assigned to it by the web service, reporting progress to the web service as it does so. When the task is done, the task runner reports the final success or failure of the task to the web service.</p>
module AWS.DataPipeline where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "DataPipeline" :: String


-- | <p>Validates the specified pipeline and starts processing pipeline tasks. If the pipeline does not pass validation, activation fails.</p> <p>If you need to pause the pipeline to investigate an issue with a component, such as a data source or script, call <a>DeactivatePipeline</a>.</p> <p>To activate a finished pipeline, modify the end date for the pipeline and then activate it.</p>
activatePipeline :: forall eff. ActivatePipelineInput -> Aff (err :: AWS.RequestError | eff) ActivatePipelineOutput
activatePipeline = AWS.request serviceName "activatePipeline" 


-- | <p>Adds or modifies tags for the specified pipeline.</p>
addTags :: forall eff. AddTagsInput -> Aff (err :: AWS.RequestError | eff) AddTagsOutput
addTags = AWS.request serviceName "addTags" 


-- | <p>Creates a new, empty pipeline. Use <a>PutPipelineDefinition</a> to populate the pipeline.</p>
createPipeline :: forall eff. CreatePipelineInput -> Aff (err :: AWS.RequestError | eff) CreatePipelineOutput
createPipeline = AWS.request serviceName "createPipeline" 


-- | <p>Deactivates the specified running pipeline. The pipeline is set to the <code>DEACTIVATING</code> state until the deactivation process completes.</p> <p>To resume a deactivated pipeline, use <a>ActivatePipeline</a>. By default, the pipeline resumes from the last completed execution. Optionally, you can specify the date and time to resume the pipeline.</p>
deactivatePipeline :: forall eff. DeactivatePipelineInput -> Aff (err :: AWS.RequestError | eff) DeactivatePipelineOutput
deactivatePipeline = AWS.request serviceName "deactivatePipeline" 


-- | <p>Deletes a pipeline, its pipeline definition, and its run history. AWS Data Pipeline attempts to cancel instances associated with the pipeline that are currently being processed by task runners.</p> <p>Deleting a pipeline cannot be undone. You cannot query or restore a deleted pipeline. To temporarily pause a pipeline instead of deleting it, call <a>SetStatus</a> with the status set to <code>PAUSE</code> on individual components. Components that are paused by <a>SetStatus</a> can be resumed.</p>
deletePipeline :: forall eff. DeletePipelineInput -> Aff (err :: AWS.RequestError | eff) Unit
deletePipeline = AWS.request serviceName "deletePipeline" 


-- | <p>Gets the object definitions for a set of objects associated with the pipeline. Object definitions are composed of a set of fields that define the properties of the object.</p>
describeObjects :: forall eff. DescribeObjectsInput -> Aff (err :: AWS.RequestError | eff) DescribeObjectsOutput
describeObjects = AWS.request serviceName "describeObjects" 


-- | <p>Retrieves metadata about one or more pipelines. The information retrieved includes the name of the pipeline, the pipeline identifier, its current state, and the user account that owns the pipeline. Using account credentials, you can retrieve metadata about pipelines that you or your IAM users have created. If you are using an IAM user account, you can retrieve metadata about only those pipelines for which you have read permissions.</p> <p>To retrieve the full pipeline definition instead of metadata about the pipeline, call <a>GetPipelineDefinition</a>.</p>
describePipelines :: forall eff. DescribePipelinesInput -> Aff (err :: AWS.RequestError | eff) DescribePipelinesOutput
describePipelines = AWS.request serviceName "describePipelines" 


-- | <p>Task runners call <code>EvaluateExpression</code> to evaluate a string in the context of the specified object. For example, a task runner can evaluate SQL queries stored in Amazon S3.</p>
evaluateExpression :: forall eff. EvaluateExpressionInput -> Aff (err :: AWS.RequestError | eff) EvaluateExpressionOutput
evaluateExpression = AWS.request serviceName "evaluateExpression" 


-- | <p>Gets the definition of the specified pipeline. You can call <code>GetPipelineDefinition</code> to retrieve the pipeline definition that you provided using <a>PutPipelineDefinition</a>.</p>
getPipelineDefinition :: forall eff. GetPipelineDefinitionInput -> Aff (err :: AWS.RequestError | eff) GetPipelineDefinitionOutput
getPipelineDefinition = AWS.request serviceName "getPipelineDefinition" 


-- | <p>Lists the pipeline identifiers for all active pipelines that you have permission to access.</p>
listPipelines :: forall eff. ListPipelinesInput -> Aff (err :: AWS.RequestError | eff) ListPipelinesOutput
listPipelines = AWS.request serviceName "listPipelines" 


-- | <p>Task runners call <code>PollForTask</code> to receive a task to perform from AWS Data Pipeline. The task runner specifies which tasks it can perform by setting a value for the <code>workerGroup</code> parameter. The task returned can come from any of the pipelines that match the <code>workerGroup</code> value passed in by the task runner and that was launched using the IAM user credentials specified by the task runner.</p> <p>If tasks are ready in the work queue, <code>PollForTask</code> returns a response immediately. If no tasks are available in the queue, <code>PollForTask</code> uses long-polling and holds on to a poll connection for up to a 90 seconds, during which time the first newly scheduled task is handed to the task runner. To accomodate this, set the socket timeout in your task runner to 90 seconds. The task runner should not call <code>PollForTask</code> again on the same <code>workerGroup</code> until it receives a response, and this can take up to 90 seconds. </p>
pollForTask :: forall eff. PollForTaskInput -> Aff (err :: AWS.RequestError | eff) PollForTaskOutput
pollForTask = AWS.request serviceName "pollForTask" 


-- | <p>Adds tasks, schedules, and preconditions to the specified pipeline. You can use <code>PutPipelineDefinition</code> to populate a new pipeline.</p> <p> <code>PutPipelineDefinition</code> also validates the configuration as it adds it to the pipeline. Changes to the pipeline are saved unless one of the following three validation errors exists in the pipeline. </p> <ol> <li>An object is missing a name or identifier field.</li> <li>A string or reference field is empty.</li> <li>The number of objects in the pipeline exceeds the maximum allowed objects.</li> <li>The pipeline is in a FINISHED state.</li> </ol> <p> Pipeline object definitions are passed to the <code>PutPipelineDefinition</code> action and returned by the <a>GetPipelineDefinition</a> action. </p>
putPipelineDefinition :: forall eff. PutPipelineDefinitionInput -> Aff (err :: AWS.RequestError | eff) PutPipelineDefinitionOutput
putPipelineDefinition = AWS.request serviceName "putPipelineDefinition" 


-- | <p>Queries the specified pipeline for the names of objects that match the specified set of conditions.</p>
queryObjects :: forall eff. QueryObjectsInput -> Aff (err :: AWS.RequestError | eff) QueryObjectsOutput
queryObjects = AWS.request serviceName "queryObjects" 


-- | <p>Removes existing tags from the specified pipeline.</p>
removeTags :: forall eff. RemoveTagsInput -> Aff (err :: AWS.RequestError | eff) RemoveTagsOutput
removeTags = AWS.request serviceName "removeTags" 


-- | <p>Task runners call <code>ReportTaskProgress</code> when assigned a task to acknowledge that it has the task. If the web service does not receive this acknowledgement within 2 minutes, it assigns the task in a subsequent <a>PollForTask</a> call. After this initial acknowledgement, the task runner only needs to report progress every 15 minutes to maintain its ownership of the task. You can change this reporting time from 15 minutes by specifying a <code>reportProgressTimeout</code> field in your pipeline.</p> <p>If a task runner does not report its status after 5 minutes, AWS Data Pipeline assumes that the task runner is unable to process the task and reassigns the task in a subsequent response to <a>PollForTask</a>. Task runners should call <code>ReportTaskProgress</code> every 60 seconds.</p>
reportTaskProgress :: forall eff. ReportTaskProgressInput -> Aff (err :: AWS.RequestError | eff) ReportTaskProgressOutput
reportTaskProgress = AWS.request serviceName "reportTaskProgress" 


-- | <p>Task runners call <code>ReportTaskRunnerHeartbeat</code> every 15 minutes to indicate that they are operational. If the AWS Data Pipeline Task Runner is launched on a resource managed by AWS Data Pipeline, the web service can use this call to detect when the task runner application has failed and restart a new instance.</p>
reportTaskRunnerHeartbeat :: forall eff. ReportTaskRunnerHeartbeatInput -> Aff (err :: AWS.RequestError | eff) ReportTaskRunnerHeartbeatOutput
reportTaskRunnerHeartbeat = AWS.request serviceName "reportTaskRunnerHeartbeat" 


-- | <p>Requests that the status of the specified physical or logical pipeline objects be updated in the specified pipeline. This update might not occur immediately, but is eventually consistent. The status that can be set depends on the type of object (for example, DataNode or Activity). You cannot perform this operation on <code>FINISHED</code> pipelines and attempting to do so returns <code>InvalidRequestException</code>.</p>
setStatus :: forall eff. SetStatusInput -> Aff (err :: AWS.RequestError | eff) Unit
setStatus = AWS.request serviceName "setStatus" 


-- | <p>Task runners call <code>SetTaskStatus</code> to notify AWS Data Pipeline that a task is completed and provide information about the final status. A task runner makes this call regardless of whether the task was sucessful. A task runner does not need to call <code>SetTaskStatus</code> for tasks that are canceled by the web service during a call to <a>ReportTaskProgress</a>.</p>
setTaskStatus :: forall eff. SetTaskStatusInput -> Aff (err :: AWS.RequestError | eff) SetTaskStatusOutput
setTaskStatus = AWS.request serviceName "setTaskStatus" 


-- | <p>Validates the specified pipeline definition to ensure that it is well formed and can be run without error.</p>
validatePipelineDefinition :: forall eff. ValidatePipelineDefinitionInput -> Aff (err :: AWS.RequestError | eff) ValidatePipelineDefinitionOutput
validatePipelineDefinition = AWS.request serviceName "validatePipelineDefinition" 


-- | <p>Contains the parameters for ActivatePipeline.</p>
newtype ActivatePipelineInput = ActivatePipelineInput 
  { "PipelineId'" :: (Id')
  , "ParameterValues'" :: NullOrUndefined (ParameterValueList)
  , "StartTimestamp'" :: NullOrUndefined (Number)
  }
derive instance newtypeActivatePipelineInput :: Newtype ActivatePipelineInput _


-- | <p>Contains the output of ActivatePipeline.</p>
newtype ActivatePipelineOutput = ActivatePipelineOutput 
  { 
  }
derive instance newtypeActivatePipelineOutput :: Newtype ActivatePipelineOutput _


-- | <p>Contains the parameters for AddTags.</p>
newtype AddTagsInput = AddTagsInput 
  { "PipelineId'" :: (Id')
  , "Tags'" :: (TagList')
  }
derive instance newtypeAddTagsInput :: Newtype AddTagsInput _


-- | <p>Contains the output of AddTags.</p>
newtype AddTagsOutput = AddTagsOutput 
  { 
  }
derive instance newtypeAddTagsOutput :: Newtype AddTagsOutput _


-- | <p>Contains the parameters for CreatePipeline.</p>
newtype CreatePipelineInput = CreatePipelineInput 
  { "Name'" :: (Id')
  , "UniqueId'" :: (Id')
  , "Description'" :: NullOrUndefined (String)
  , "Tags'" :: NullOrUndefined (TagList')
  }
derive instance newtypeCreatePipelineInput :: Newtype CreatePipelineInput _


-- | <p>Contains the output of CreatePipeline.</p>
newtype CreatePipelineOutput = CreatePipelineOutput 
  { "PipelineId'" :: (Id')
  }
derive instance newtypeCreatePipelineOutput :: Newtype CreatePipelineOutput _


-- | <p>Contains the parameters for DeactivatePipeline.</p>
newtype DeactivatePipelineInput = DeactivatePipelineInput 
  { "PipelineId'" :: (Id')
  , "CancelActive'" :: NullOrUndefined (CancelActive')
  }
derive instance newtypeDeactivatePipelineInput :: Newtype DeactivatePipelineInput _


-- | <p>Contains the output of DeactivatePipeline.</p>
newtype DeactivatePipelineOutput = DeactivatePipelineOutput 
  { 
  }
derive instance newtypeDeactivatePipelineOutput :: Newtype DeactivatePipelineOutput _


-- | <p>Contains the parameters for DeletePipeline.</p>
newtype DeletePipelineInput = DeletePipelineInput 
  { "PipelineId'" :: (Id')
  }
derive instance newtypeDeletePipelineInput :: Newtype DeletePipelineInput _


-- | <p>Contains the parameters for DescribeObjects.</p>
newtype DescribeObjectsInput = DescribeObjectsInput 
  { "PipelineId'" :: (Id')
  , "ObjectIds'" :: (IdList')
  , "EvaluateExpressions'" :: NullOrUndefined (Boolean)
  , "Marker'" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeObjectsInput :: Newtype DescribeObjectsInput _


-- | <p>Contains the output of DescribeObjects.</p>
newtype DescribeObjectsOutput = DescribeObjectsOutput 
  { "PipelineObjects'" :: (PipelineObjectList)
  , "Marker'" :: NullOrUndefined (String)
  , "HasMoreResults'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDescribeObjectsOutput :: Newtype DescribeObjectsOutput _


-- | <p>Contains the parameters for DescribePipelines.</p>
newtype DescribePipelinesInput = DescribePipelinesInput 
  { "PipelineIds'" :: (IdList')
  }
derive instance newtypeDescribePipelinesInput :: Newtype DescribePipelinesInput _


-- | <p>Contains the output of DescribePipelines.</p>
newtype DescribePipelinesOutput = DescribePipelinesOutput 
  { "PipelineDescriptionList'" :: (PipelineDescriptionList)
  }
derive instance newtypeDescribePipelinesOutput :: Newtype DescribePipelinesOutput _


-- | <p>Contains the parameters for EvaluateExpression.</p>
newtype EvaluateExpressionInput = EvaluateExpressionInput 
  { "PipelineId'" :: (Id')
  , "ObjectId'" :: (Id')
  , "Expression'" :: (LongString')
  }
derive instance newtypeEvaluateExpressionInput :: Newtype EvaluateExpressionInput _


-- | <p>Contains the output of EvaluateExpression.</p>
newtype EvaluateExpressionOutput = EvaluateExpressionOutput 
  { "EvaluatedExpression'" :: (LongString')
  }
derive instance newtypeEvaluateExpressionOutput :: Newtype EvaluateExpressionOutput _


-- | <p>A key-value pair that describes a property of a pipeline object. The value is specified as either a string value (<code>StringValue</code>) or a reference to another object (<code>RefValue</code>) but not as both.</p>
newtype Field = Field 
  { "Key'" :: (FieldNameString')
  , "StringValue'" :: NullOrUndefined (FieldStringValue')
  , "RefValue'" :: NullOrUndefined (FieldNameString')
  }
derive instance newtypeField :: Newtype Field _


-- | <p>Contains the parameters for GetPipelineDefinition.</p>
newtype GetPipelineDefinitionInput = GetPipelineDefinitionInput 
  { "PipelineId'" :: (Id')
  , "Version'" :: NullOrUndefined (String)
  }
derive instance newtypeGetPipelineDefinitionInput :: Newtype GetPipelineDefinitionInput _


-- | <p>Contains the output of GetPipelineDefinition.</p>
newtype GetPipelineDefinitionOutput = GetPipelineDefinitionOutput 
  { "PipelineObjects'" :: NullOrUndefined (PipelineObjectList)
  , "ParameterObjects'" :: NullOrUndefined (ParameterObjectList)
  , "ParameterValues'" :: NullOrUndefined (ParameterValueList)
  }
derive instance newtypeGetPipelineDefinitionOutput :: Newtype GetPipelineDefinitionOutput _


-- | <p><p>Identity information for the EC2 instance that is hosting the task runner. You can get this value by calling a metadata URI from the EC2 instance. For more information, see <a href="http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html">Instance Metadata</a> in the <i>Amazon Elastic Compute Cloud User Guide.</i> Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.</p></p>
newtype InstanceIdentity = InstanceIdentity 
  { "Document'" :: NullOrUndefined (String)
  , "Signature'" :: NullOrUndefined (String)
  }
derive instance newtypeInstanceIdentity :: Newtype InstanceIdentity _


-- | <p>An internal service error occurred.</p>
newtype InternalServiceError = InternalServiceError 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInternalServiceError :: Newtype InternalServiceError _


-- | <p>The request was not valid. Verify that your request was properly formatted, that the signature was generated with the correct credentials, and that you haven't exceeded any of the service limits for your account.</p>
newtype InvalidRequestException = InvalidRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeInvalidRequestException :: Newtype InvalidRequestException _


-- | <p>Contains the parameters for ListPipelines.</p>
newtype ListPipelinesInput = ListPipelinesInput 
  { "Marker'" :: NullOrUndefined (String)
  }
derive instance newtypeListPipelinesInput :: Newtype ListPipelinesInput _


-- | <p>Contains the output of ListPipelines.</p>
newtype ListPipelinesOutput = ListPipelinesOutput 
  { "PipelineIdList'" :: (PipelineList')
  , "Marker'" :: NullOrUndefined (String)
  , "HasMoreResults'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeListPipelinesOutput :: Newtype ListPipelinesOutput _


-- | <p>Contains a logical operation for comparing the value of a field with a specified value.</p>
newtype Operator = Operator 
  { "Type'" :: NullOrUndefined (OperatorType)
  , "Values'" :: NullOrUndefined (StringList')
  }
derive instance newtypeOperator :: Newtype Operator _


newtype OperatorType = OperatorType String
derive instance newtypeOperatorType :: Newtype OperatorType _


-- | <p>The attributes allowed or specified with a parameter object.</p>
newtype ParameterAttribute = ParameterAttribute 
  { "Key'" :: (AttributeNameString')
  , "StringValue'" :: (AttributeValueString')
  }
derive instance newtypeParameterAttribute :: Newtype ParameterAttribute _


newtype ParameterAttributeList = ParameterAttributeList (Array ParameterAttribute)
derive instance newtypeParameterAttributeList :: Newtype ParameterAttributeList _


-- | <p>Contains information about a parameter object.</p>
newtype ParameterObject = ParameterObject 
  { "Id'" :: (FieldNameString')
  , "Attributes'" :: (ParameterAttributeList)
  }
derive instance newtypeParameterObject :: Newtype ParameterObject _


newtype ParameterObjectList = ParameterObjectList (Array ParameterObject)
derive instance newtypeParameterObjectList :: Newtype ParameterObjectList _


-- | <p>A value or list of parameter values. </p>
newtype ParameterValue = ParameterValue 
  { "Id'" :: (FieldNameString')
  , "StringValue'" :: (FieldStringValue')
  }
derive instance newtypeParameterValue :: Newtype ParameterValue _


newtype ParameterValueList = ParameterValueList (Array ParameterValue)
derive instance newtypeParameterValueList :: Newtype ParameterValueList _


-- | <p>The specified pipeline has been deleted.</p>
newtype PipelineDeletedException = PipelineDeletedException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypePipelineDeletedException :: Newtype PipelineDeletedException _


-- | <p>Contains pipeline metadata.</p>
newtype PipelineDescription = PipelineDescription 
  { "PipelineId'" :: (Id')
  , "Name'" :: (Id')
  , "Fields'" :: (FieldList')
  , "Description'" :: NullOrUndefined (String)
  , "Tags'" :: NullOrUndefined (TagList')
  }
derive instance newtypePipelineDescription :: Newtype PipelineDescription _


newtype PipelineDescriptionList = PipelineDescriptionList (Array PipelineDescription)
derive instance newtypePipelineDescriptionList :: Newtype PipelineDescriptionList _


-- | <p>Contains the name and identifier of a pipeline.</p>
newtype PipelineIdName = PipelineIdName 
  { "Id'" :: NullOrUndefined (Id')
  , "Name'" :: NullOrUndefined (Id')
  }
derive instance newtypePipelineIdName :: Newtype PipelineIdName _


-- | <p>The specified pipeline was not found. Verify that you used the correct user and account identifiers.</p>
newtype PipelineNotFoundException = PipelineNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypePipelineNotFoundException :: Newtype PipelineNotFoundException _


-- | <p>Contains information about a pipeline object. This can be a logical, physical, or physical attempt pipeline object. The complete set of components of a pipeline defines the pipeline.</p>
newtype PipelineObject = PipelineObject 
  { "Id'" :: (Id')
  , "Name'" :: (Id')
  , "Fields'" :: (FieldList')
  }
derive instance newtypePipelineObject :: Newtype PipelineObject _


newtype PipelineObjectList = PipelineObjectList (Array PipelineObject)
derive instance newtypePipelineObjectList :: Newtype PipelineObjectList _


newtype PipelineObjectMap = PipelineObjectMap (Map Id' PipelineObject)
derive instance newtypePipelineObjectMap :: Newtype PipelineObjectMap _


-- | <p>Contains the parameters for PollForTask.</p>
newtype PollForTaskInput = PollForTaskInput 
  { "WorkerGroup'" :: (String)
  , "Hostname'" :: NullOrUndefined (Id')
  , "InstanceIdentity'" :: NullOrUndefined (InstanceIdentity)
  }
derive instance newtypePollForTaskInput :: Newtype PollForTaskInput _


-- | <p>Contains the output of PollForTask.</p>
newtype PollForTaskOutput = PollForTaskOutput 
  { "TaskObject'" :: NullOrUndefined (TaskObject)
  }
derive instance newtypePollForTaskOutput :: Newtype PollForTaskOutput _


-- | <p>Contains the parameters for PutPipelineDefinition.</p>
newtype PutPipelineDefinitionInput = PutPipelineDefinitionInput 
  { "PipelineId'" :: (Id')
  , "PipelineObjects'" :: (PipelineObjectList)
  , "ParameterObjects'" :: NullOrUndefined (ParameterObjectList)
  , "ParameterValues'" :: NullOrUndefined (ParameterValueList)
  }
derive instance newtypePutPipelineDefinitionInput :: Newtype PutPipelineDefinitionInput _


-- | <p>Contains the output of PutPipelineDefinition.</p>
newtype PutPipelineDefinitionOutput = PutPipelineDefinitionOutput 
  { "ValidationErrors'" :: NullOrUndefined (ValidationErrors)
  , "ValidationWarnings'" :: NullOrUndefined (ValidationWarnings)
  , "Errored'" :: (Boolean)
  }
derive instance newtypePutPipelineDefinitionOutput :: Newtype PutPipelineDefinitionOutput _


-- | <p>Defines the query to run against an object.</p>
newtype Query = Query 
  { "Selectors'" :: NullOrUndefined (SelectorList)
  }
derive instance newtypeQuery :: Newtype Query _


-- | <p>Contains the parameters for QueryObjects.</p>
newtype QueryObjectsInput = QueryObjectsInput 
  { "PipelineId'" :: (Id')
  , "Query'" :: NullOrUndefined (Query)
  , "Sphere'" :: (String)
  , "Marker'" :: NullOrUndefined (String)
  , "Limit'" :: NullOrUndefined (Int)
  }
derive instance newtypeQueryObjectsInput :: Newtype QueryObjectsInput _


-- | <p>Contains the output of QueryObjects.</p>
newtype QueryObjectsOutput = QueryObjectsOutput 
  { "Ids'" :: NullOrUndefined (IdList')
  , "Marker'" :: NullOrUndefined (String)
  , "HasMoreResults'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeQueryObjectsOutput :: Newtype QueryObjectsOutput _


-- | <p>Contains the parameters for RemoveTags.</p>
newtype RemoveTagsInput = RemoveTagsInput 
  { "PipelineId'" :: (Id')
  , "TagKeys'" :: (StringList')
  }
derive instance newtypeRemoveTagsInput :: Newtype RemoveTagsInput _


-- | <p>Contains the output of RemoveTags.</p>
newtype RemoveTagsOutput = RemoveTagsOutput 
  { 
  }
derive instance newtypeRemoveTagsOutput :: Newtype RemoveTagsOutput _


-- | <p>Contains the parameters for ReportTaskProgress.</p>
newtype ReportTaskProgressInput = ReportTaskProgressInput 
  { "TaskId'" :: (TaskId')
  , "Fields'" :: NullOrUndefined (FieldList')
  }
derive instance newtypeReportTaskProgressInput :: Newtype ReportTaskProgressInput _


-- | <p>Contains the output of ReportTaskProgress.</p>
newtype ReportTaskProgressOutput = ReportTaskProgressOutput 
  { "Canceled'" :: (Boolean)
  }
derive instance newtypeReportTaskProgressOutput :: Newtype ReportTaskProgressOutput _


-- | <p>Contains the parameters for ReportTaskRunnerHeartbeat.</p>
newtype ReportTaskRunnerHeartbeatInput = ReportTaskRunnerHeartbeatInput 
  { "TaskrunnerId'" :: (Id')
  , "WorkerGroup'" :: NullOrUndefined (String)
  , "Hostname'" :: NullOrUndefined (Id')
  }
derive instance newtypeReportTaskRunnerHeartbeatInput :: Newtype ReportTaskRunnerHeartbeatInput _


-- | <p>Contains the output of ReportTaskRunnerHeartbeat.</p>
newtype ReportTaskRunnerHeartbeatOutput = ReportTaskRunnerHeartbeatOutput 
  { "Terminate'" :: (Boolean)
  }
derive instance newtypeReportTaskRunnerHeartbeatOutput :: Newtype ReportTaskRunnerHeartbeatOutput _


-- | <p>A comparision that is used to determine whether a query should return this object.</p>
newtype Selector = Selector 
  { "FieldName'" :: NullOrUndefined (String)
  , "Operator'" :: NullOrUndefined (Operator)
  }
derive instance newtypeSelector :: Newtype Selector _


-- | <p>The list of Selectors that define queries on individual fields.</p>
newtype SelectorList = SelectorList (Array Selector)
derive instance newtypeSelectorList :: Newtype SelectorList _


-- | <p>Contains the parameters for SetStatus.</p>
newtype SetStatusInput = SetStatusInput 
  { "PipelineId'" :: (Id')
  , "ObjectIds'" :: (IdList')
  , "Status'" :: (String)
  }
derive instance newtypeSetStatusInput :: Newtype SetStatusInput _


-- | <p>Contains the parameters for SetTaskStatus.</p>
newtype SetTaskStatusInput = SetTaskStatusInput 
  { "TaskId'" :: (TaskId')
  , "TaskStatus'" :: (TaskStatus)
  , "ErrorId'" :: NullOrUndefined (String)
  , "ErrorMessage'" :: NullOrUndefined (ErrorMessage')
  , "ErrorStackTrace'" :: NullOrUndefined (String)
  }
derive instance newtypeSetTaskStatusInput :: Newtype SetTaskStatusInput _


-- | <p>Contains the output of SetTaskStatus.</p>
newtype SetTaskStatusOutput = SetTaskStatusOutput 
  { 
  }
derive instance newtypeSetTaskStatusOutput :: Newtype SetTaskStatusOutput _


-- | <p>Tags are key/value pairs defined by a user and associated with a pipeline to control access. AWS Data Pipeline allows you to associate ten tags per pipeline. For more information, see <a href="http://docs.aws.amazon.com/datapipeline/latest/DeveloperGuide/dp-control-access.html">Controlling User Access to Pipelines</a> in the <i>AWS Data Pipeline Developer Guide</i>.</p>
newtype Tag = Tag 
  { "Key'" :: (TagKey')
  , "Value'" :: (TagValue')
  }
derive instance newtypeTag :: Newtype Tag _


-- | <p>The specified task was not found. </p>
newtype TaskNotFoundException = TaskNotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage')
  }
derive instance newtypeTaskNotFoundException :: Newtype TaskNotFoundException _


-- | <p>Contains information about a pipeline task that is assigned to a task runner.</p>
newtype TaskObject = TaskObject 
  { "TaskId'" :: NullOrUndefined (TaskId')
  , "PipelineId'" :: NullOrUndefined (Id')
  , "AttemptId'" :: NullOrUndefined (Id')
  , "Objects'" :: NullOrUndefined (PipelineObjectMap)
  }
derive instance newtypeTaskObject :: Newtype TaskObject _


newtype TaskStatus = TaskStatus String
derive instance newtypeTaskStatus :: Newtype TaskStatus _


-- | <p>Contains the parameters for ValidatePipelineDefinition.</p>
newtype ValidatePipelineDefinitionInput = ValidatePipelineDefinitionInput 
  { "PipelineId'" :: (Id')
  , "PipelineObjects'" :: (PipelineObjectList)
  , "ParameterObjects'" :: NullOrUndefined (ParameterObjectList)
  , "ParameterValues'" :: NullOrUndefined (ParameterValueList)
  }
derive instance newtypeValidatePipelineDefinitionInput :: Newtype ValidatePipelineDefinitionInput _


-- | <p>Contains the output of ValidatePipelineDefinition.</p>
newtype ValidatePipelineDefinitionOutput = ValidatePipelineDefinitionOutput 
  { "ValidationErrors'" :: NullOrUndefined (ValidationErrors)
  , "ValidationWarnings'" :: NullOrUndefined (ValidationWarnings)
  , "Errored'" :: (Boolean)
  }
derive instance newtypeValidatePipelineDefinitionOutput :: Newtype ValidatePipelineDefinitionOutput _


-- | <p>Defines a validation error. Validation errors prevent pipeline activation. The set of validation errors that can be returned are defined by AWS Data Pipeline.</p>
newtype ValidationError = ValidationError 
  { "Id'" :: NullOrUndefined (Id')
  , "Errors'" :: NullOrUndefined (ValidationMessages')
  }
derive instance newtypeValidationError :: Newtype ValidationError _


newtype ValidationErrors = ValidationErrors (Array ValidationError)
derive instance newtypeValidationErrors :: Newtype ValidationErrors _


-- | <p>Defines a validation warning. Validation warnings do not prevent pipeline activation. The set of validation warnings that can be returned are defined by AWS Data Pipeline.</p>
newtype ValidationWarning = ValidationWarning 
  { "Id'" :: NullOrUndefined (Id')
  , "Warnings'" :: NullOrUndefined (ValidationMessages')
  }
derive instance newtypeValidationWarning :: Newtype ValidationWarning _


newtype ValidationWarnings = ValidationWarnings (Array ValidationWarning)
derive instance newtypeValidationWarnings :: Newtype ValidationWarnings _


newtype AttributeNameString' = AttributeNameString' String
derive instance newtypeAttributeNameString' :: Newtype AttributeNameString' _


newtype AttributeValueString' = AttributeValueString' String
derive instance newtypeAttributeValueString' :: Newtype AttributeValueString' _


newtype CancelActive' = CancelActive' Boolean
derive instance newtypeCancelActive' :: Newtype CancelActive' _


newtype ErrorMessage' = ErrorMessage' String
derive instance newtypeErrorMessage' :: Newtype ErrorMessage' _


newtype FieldList' = FieldList' (Array Field)
derive instance newtypeFieldList' :: Newtype FieldList' _


newtype FieldNameString' = FieldNameString' String
derive instance newtypeFieldNameString' :: Newtype FieldNameString' _


newtype FieldStringValue' = FieldStringValue' String
derive instance newtypeFieldStringValue' :: Newtype FieldStringValue' _


newtype Id' = Id' String
derive instance newtypeId' :: Newtype Id' _


newtype IdList' = IdList' (Array Id')
derive instance newtypeIdList' :: Newtype IdList' _


newtype LongString' = LongString' String
derive instance newtypeLongString' :: Newtype LongString' _


newtype PipelineList' = PipelineList' (Array PipelineIdName)
derive instance newtypePipelineList' :: Newtype PipelineList' _


newtype StringList' = StringList' (Array String)
derive instance newtypeStringList' :: Newtype StringList' _


newtype TagKey' = TagKey' String
derive instance newtypeTagKey' :: Newtype TagKey' _


newtype TagList' = TagList' (Array Tag)
derive instance newtypeTagList' :: Newtype TagList' _


newtype TagValue' = TagValue' String
derive instance newtypeTagValue' :: Newtype TagValue' _


newtype TaskId' = TaskId' String
derive instance newtypeTaskId' :: Newtype TaskId' _


newtype ValidationMessage' = ValidationMessage' String
derive instance newtypeValidationMessage' :: Newtype ValidationMessage' _


newtype ValidationMessages' = ValidationMessages' (Array ValidationMessage')
derive instance newtypeValidationMessages' :: Newtype ValidationMessages' _
