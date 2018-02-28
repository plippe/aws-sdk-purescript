

-- | <fullname>AWS CodePipeline</fullname> <p> <b>Overview</b> </p> <p>This is the AWS CodePipeline API Reference. This guide provides descriptions of the actions and data types for AWS CodePipeline. Some functionality for your pipeline is only configurable through the API. For additional information, see the <a href="http://docs.aws.amazon.com/codepipeline/latest/userguide/welcome.html">AWS CodePipeline User Guide</a>.</p> <p>You can use the AWS CodePipeline API to work with pipelines, stages, actions, gates, and transitions, as described below.</p> <p> <i>Pipelines</i> are models of automated release processes. Each pipeline is uniquely named, and consists of actions, gates, and stages. </p> <p>You can work with pipelines by calling:</p> <ul> <li> <p> <a>CreatePipeline</a>, which creates a uniquely-named pipeline.</p> </li> <li> <p> <a>DeletePipeline</a>, which deletes the specified pipeline.</p> </li> <li> <p> <a>GetPipeline</a>, which returns information about the pipeline structure and pipeline metadata, including the pipeline Amazon Resource Name (ARN).</p> </li> <li> <p> <a>GetPipelineExecution</a>, which returns information about a specific execution of a pipeline.</p> </li> <li> <p> <a>GetPipelineState</a>, which returns information about the current state of the stages and actions of a pipeline.</p> </li> <li> <p> <a>ListPipelines</a>, which gets a summary of all of the pipelines associated with your account.</p> </li> <li> <p> <a>ListPipelineExecutions</a>, which gets a summary of the most recent executions for a pipeline.</p> </li> <li> <p> <a>StartPipelineExecution</a>, which runs the the most recent revision of an artifact through the pipeline.</p> </li> <li> <p> <a>UpdatePipeline</a>, which updates a pipeline with edits or changes to the structure of the pipeline.</p> </li> </ul> <p>Pipelines include <i>stages</i>, which are logical groupings of gates and actions. Each stage contains one or more actions that must complete before the next stage begins. A stage will result in success or failure. If a stage fails, then the pipeline stops at that stage and will remain stopped until either a new version of an artifact appears in the source location, or a user takes action to re-run the most recent artifact through the pipeline. You can call <a>GetPipelineState</a>, which displays the status of a pipeline, including the status of stages in the pipeline, or <a>GetPipeline</a>, which returns the entire structure of the pipeline, including the stages of that pipeline. For more information about the structure of stages and actions, also refer to the <a href="http://docs.aws.amazon.com/codepipeline/latest/userguide/pipeline-structure.html">AWS CodePipeline Pipeline Structure Reference</a>.</p> <p>Pipeline stages include <i>actions</i>, which are categorized into categories such as source or build actions performed within a stage of a pipeline. For example, you can use a source action to import artifacts into a pipeline from a source such as Amazon S3. Like stages, you do not work with actions directly in most cases, but you do define and interact with actions when working with pipeline operations such as <a>CreatePipeline</a> and <a>GetPipelineState</a>. </p> <p>Pipelines also include <i>transitions</i>, which allow the transition of artifacts from one stage to the next in a pipeline after the actions in one stage complete.</p> <p>You can work with transitions by calling:</p> <ul> <li> <p> <a>DisableStageTransition</a>, which prevents artifacts from transitioning to the next stage in a pipeline.</p> </li> <li> <p> <a>EnableStageTransition</a>, which enables transition of artifacts between stages in a pipeline. </p> </li> </ul> <p> <b>Using the API to integrate with AWS CodePipeline</b> </p> <p>For third-party integrators or developers who want to create their own integrations with AWS CodePipeline, the expected sequence varies from the standard API user. In order to integrate with AWS CodePipeline, developers will need to work with the following items:</p> <p> <b>Jobs</b>, which are instances of an action. For example, a job for a source action might import a revision of an artifact from a source. </p> <p>You can work with jobs by calling:</p> <ul> <li> <p> <a>AcknowledgeJob</a>, which confirms whether a job worker has received the specified job,</p> </li> <li> <p> <a>GetJobDetails</a>, which returns the details of a job,</p> </li> <li> <p> <a>PollForJobs</a>, which determines whether there are any jobs to act upon, </p> </li> <li> <p> <a>PutJobFailureResult</a>, which provides details of a job failure, and</p> </li> <li> <p> <a>PutJobSuccessResult</a>, which provides details of a job success.</p> </li> </ul> <p> <b>Third party jobs</b>, which are instances of an action created by a partner action and integrated into AWS CodePipeline. Partner actions are created by members of the AWS Partner Network.</p> <p>You can work with third party jobs by calling:</p> <ul> <li> <p> <a>AcknowledgeThirdPartyJob</a>, which confirms whether a job worker has received the specified job,</p> </li> <li> <p> <a>GetThirdPartyJobDetails</a>, which requests the details of a job for a partner action,</p> </li> <li> <p> <a>PollForThirdPartyJobs</a>, which determines whether there are any jobs to act upon, </p> </li> <li> <p> <a>PutThirdPartyJobFailureResult</a>, which provides details of a job failure, and</p> </li> <li> <p> <a>PutThirdPartyJobSuccessResult</a>, which provides details of a job success.</p> </li> </ul>
module AWS.CodePipeline where

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

serviceName = "CodePipeline" :: String


-- | <p>Returns information about a specified job and whether that job has been received by the job worker. Only used for custom actions.</p>
acknowledgeJob :: forall eff. AcknowledgeJobInput -> Aff (exception :: EXCEPTION | eff) AcknowledgeJobOutput
acknowledgeJob = Request.request serviceName "acknowledgeJob" 


-- | <p>Confirms a job worker has received the specified job. Only used for partner actions.</p>
acknowledgeThirdPartyJob :: forall eff. AcknowledgeThirdPartyJobInput -> Aff (exception :: EXCEPTION | eff) AcknowledgeThirdPartyJobOutput
acknowledgeThirdPartyJob = Request.request serviceName "acknowledgeThirdPartyJob" 


-- | <p>Creates a new custom action that can be used in all pipelines associated with the AWS account. Only used for custom actions.</p>
createCustomActionType :: forall eff. CreateCustomActionTypeInput -> Aff (exception :: EXCEPTION | eff) CreateCustomActionTypeOutput
createCustomActionType = Request.request serviceName "createCustomActionType" 


-- | <p>Creates a pipeline.</p>
createPipeline :: forall eff. CreatePipelineInput -> Aff (exception :: EXCEPTION | eff) CreatePipelineOutput
createPipeline = Request.request serviceName "createPipeline" 


-- | <p>Marks a custom action as deleted. PollForJobs for the custom action will fail after the action is marked for deletion. Only used for custom actions.</p> <important> <p>You cannot recreate a custom action after it has been deleted unless you increase the version number of the action.</p> </important>
deleteCustomActionType :: forall eff. DeleteCustomActionTypeInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteCustomActionType = Request.request serviceName "deleteCustomActionType" 


-- | <p>Deletes the specified pipeline.</p>
deletePipeline :: forall eff. DeletePipelineInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deletePipeline = Request.request serviceName "deletePipeline" 


-- | <p>Prevents artifacts in a pipeline from transitioning to the next stage in the pipeline.</p>
disableStageTransition :: forall eff. DisableStageTransitionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
disableStageTransition = Request.request serviceName "disableStageTransition" 


-- | <p>Enables artifacts in a pipeline to transition to a stage in a pipeline.</p>
enableStageTransition :: forall eff. EnableStageTransitionInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
enableStageTransition = Request.request serviceName "enableStageTransition" 


-- | <p>Returns information about a job. Only used for custom actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
getJobDetails :: forall eff. GetJobDetailsInput -> Aff (exception :: EXCEPTION | eff) GetJobDetailsOutput
getJobDetails = Request.request serviceName "getJobDetails" 


-- | <p>Returns the metadata, structure, stages, and actions of a pipeline. Can be used to return the entire structure of a pipeline in JSON format, which can then be modified and used to update the pipeline structure with <a>UpdatePipeline</a>.</p>
getPipeline :: forall eff. GetPipelineInput -> Aff (exception :: EXCEPTION | eff) GetPipelineOutput
getPipeline = Request.request serviceName "getPipeline" 


-- | <p>Returns information about an execution of a pipeline, including details about artifacts, the pipeline execution ID, and the name, version, and status of the pipeline.</p>
getPipelineExecution :: forall eff. GetPipelineExecutionInput -> Aff (exception :: EXCEPTION | eff) GetPipelineExecutionOutput
getPipelineExecution = Request.request serviceName "getPipelineExecution" 


-- | <p>Returns information about the state of a pipeline, including the stages and actions.</p>
getPipelineState :: forall eff. GetPipelineStateInput -> Aff (exception :: EXCEPTION | eff) GetPipelineStateOutput
getPipelineState = Request.request serviceName "getPipelineState" 


-- | <p>Requests the details of a job for a third party action. Only used for partner actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
getThirdPartyJobDetails :: forall eff. GetThirdPartyJobDetailsInput -> Aff (exception :: EXCEPTION | eff) GetThirdPartyJobDetailsOutput
getThirdPartyJobDetails = Request.request serviceName "getThirdPartyJobDetails" 


-- | <p>Gets a summary of all AWS CodePipeline action types associated with your account.</p>
listActionTypes :: forall eff. ListActionTypesInput -> Aff (exception :: EXCEPTION | eff) ListActionTypesOutput
listActionTypes = Request.request serviceName "listActionTypes" 


-- | <p>Gets a summary of the most recent executions for a pipeline.</p>
listPipelineExecutions :: forall eff. ListPipelineExecutionsInput -> Aff (exception :: EXCEPTION | eff) ListPipelineExecutionsOutput
listPipelineExecutions = Request.request serviceName "listPipelineExecutions" 


-- | <p>Gets a summary of all of the pipelines associated with your account.</p>
listPipelines :: forall eff. ListPipelinesInput -> Aff (exception :: EXCEPTION | eff) ListPipelinesOutput
listPipelines = Request.request serviceName "listPipelines" 


-- | <p>Returns information about any jobs for AWS CodePipeline to act upon.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
pollForJobs :: forall eff. PollForJobsInput -> Aff (exception :: EXCEPTION | eff) PollForJobsOutput
pollForJobs = Request.request serviceName "pollForJobs" 


-- | <p>Determines whether there are any third party jobs for a job worker to act on. Only used for partner actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts.</p> </important>
pollForThirdPartyJobs :: forall eff. PollForThirdPartyJobsInput -> Aff (exception :: EXCEPTION | eff) PollForThirdPartyJobsOutput
pollForThirdPartyJobs = Request.request serviceName "pollForThirdPartyJobs" 


-- | <p>Provides information to AWS CodePipeline about new revisions to a source.</p>
putActionRevision :: forall eff. PutActionRevisionInput -> Aff (exception :: EXCEPTION | eff) PutActionRevisionOutput
putActionRevision = Request.request serviceName "putActionRevision" 


-- | <p>Provides the response to a manual approval request to AWS CodePipeline. Valid responses include Approved and Rejected.</p>
putApprovalResult :: forall eff. PutApprovalResultInput -> Aff (exception :: EXCEPTION | eff) PutApprovalResultOutput
putApprovalResult = Request.request serviceName "putApprovalResult" 


-- | <p>Represents the failure of a job as returned to the pipeline by a job worker. Only used for custom actions.</p>
putJobFailureResult :: forall eff. PutJobFailureResultInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putJobFailureResult = Request.request serviceName "putJobFailureResult" 


-- | <p>Represents the success of a job as returned to the pipeline by a job worker. Only used for custom actions.</p>
putJobSuccessResult :: forall eff. PutJobSuccessResultInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putJobSuccessResult = Request.request serviceName "putJobSuccessResult" 


-- | <p>Represents the failure of a third party job as returned to the pipeline by a job worker. Only used for partner actions.</p>
putThirdPartyJobFailureResult :: forall eff. PutThirdPartyJobFailureResultInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putThirdPartyJobFailureResult = Request.request serviceName "putThirdPartyJobFailureResult" 


-- | <p>Represents the success of a third party job as returned to the pipeline by a job worker. Only used for partner actions.</p>
putThirdPartyJobSuccessResult :: forall eff. PutThirdPartyJobSuccessResultInput -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
putThirdPartyJobSuccessResult = Request.request serviceName "putThirdPartyJobSuccessResult" 


-- | <p>Resumes the pipeline execution by retrying the last failed actions in a stage.</p>
retryStageExecution :: forall eff. RetryStageExecutionInput -> Aff (exception :: EXCEPTION | eff) RetryStageExecutionOutput
retryStageExecution = Request.request serviceName "retryStageExecution" 


-- | <p>Starts the specified pipeline. Specifically, it begins processing the latest commit to the source location specified as part of the pipeline.</p>
startPipelineExecution :: forall eff. StartPipelineExecutionInput -> Aff (exception :: EXCEPTION | eff) StartPipelineExecutionOutput
startPipelineExecution = Request.request serviceName "startPipelineExecution" 


-- | <p>Updates a specified pipeline with edits or changes to its structure. Use a JSON file with the pipeline structure in conjunction with UpdatePipeline to provide the full structure of the pipeline. Updating the pipeline increases the version number of the pipeline by 1.</p>
updatePipeline :: forall eff. UpdatePipelineInput -> Aff (exception :: EXCEPTION | eff) UpdatePipelineOutput
updatePipeline = Request.request serviceName "updatePipeline" 


-- | <p>Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the Amazon S3 bucket used to store artifact for the pipeline in AWS CodePipeline.</p>
newtype AWSSessionCredentials = AWSSessionCredentials 
  { "AccessKeyId'" :: (AccessKeyId)
  , "SecretAccessKey'" :: (SecretAccessKey)
  , "SessionToken'" :: (SessionToken)
  }
derive instance newtypeAWSSessionCredentials :: Newtype AWSSessionCredentials _
derive instance repGenericAWSSessionCredentials :: Generic AWSSessionCredentials _
instance showAWSSessionCredentials :: Show AWSSessionCredentials where
  show = genericShow
instance decodeAWSSessionCredentials :: Decode AWSSessionCredentials where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAWSSessionCredentials :: Encode AWSSessionCredentials where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccessKeyId = AccessKeyId String
derive instance newtypeAccessKeyId :: Newtype AccessKeyId _
derive instance repGenericAccessKeyId :: Generic AccessKeyId _
instance showAccessKeyId :: Show AccessKeyId where
  show = genericShow
instance decodeAccessKeyId :: Decode AccessKeyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessKeyId :: Encode AccessKeyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _
derive instance repGenericAccountId :: Generic AccountId _
instance showAccountId :: Show AccountId where
  show = genericShow
instance decodeAccountId :: Decode AccountId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountId :: Encode AccountId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an AcknowledgeJob action.</p>
newtype AcknowledgeJobInput = AcknowledgeJobInput 
  { "JobId'" :: (JobId)
  , "Nonce'" :: (Nonce)
  }
derive instance newtypeAcknowledgeJobInput :: Newtype AcknowledgeJobInput _
derive instance repGenericAcknowledgeJobInput :: Generic AcknowledgeJobInput _
instance showAcknowledgeJobInput :: Show AcknowledgeJobInput where
  show = genericShow
instance decodeAcknowledgeJobInput :: Decode AcknowledgeJobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcknowledgeJobInput :: Encode AcknowledgeJobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an AcknowledgeJob action.</p>
newtype AcknowledgeJobOutput = AcknowledgeJobOutput 
  { "Status'" :: NullOrUndefined.NullOrUndefined (JobStatus)
  }
derive instance newtypeAcknowledgeJobOutput :: Newtype AcknowledgeJobOutput _
derive instance repGenericAcknowledgeJobOutput :: Generic AcknowledgeJobOutput _
instance showAcknowledgeJobOutput :: Show AcknowledgeJobOutput where
  show = genericShow
instance decodeAcknowledgeJobOutput :: Decode AcknowledgeJobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcknowledgeJobOutput :: Encode AcknowledgeJobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an AcknowledgeThirdPartyJob action.</p>
newtype AcknowledgeThirdPartyJobInput = AcknowledgeThirdPartyJobInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "Nonce'" :: (Nonce)
  , "ClientToken'" :: (ClientToken)
  }
derive instance newtypeAcknowledgeThirdPartyJobInput :: Newtype AcknowledgeThirdPartyJobInput _
derive instance repGenericAcknowledgeThirdPartyJobInput :: Generic AcknowledgeThirdPartyJobInput _
instance showAcknowledgeThirdPartyJobInput :: Show AcknowledgeThirdPartyJobInput where
  show = genericShow
instance decodeAcknowledgeThirdPartyJobInput :: Decode AcknowledgeThirdPartyJobInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcknowledgeThirdPartyJobInput :: Encode AcknowledgeThirdPartyJobInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an AcknowledgeThirdPartyJob action.</p>
newtype AcknowledgeThirdPartyJobOutput = AcknowledgeThirdPartyJobOutput 
  { "Status'" :: NullOrUndefined.NullOrUndefined (JobStatus)
  }
derive instance newtypeAcknowledgeThirdPartyJobOutput :: Newtype AcknowledgeThirdPartyJobOutput _
derive instance repGenericAcknowledgeThirdPartyJobOutput :: Generic AcknowledgeThirdPartyJobOutput _
instance showAcknowledgeThirdPartyJobOutput :: Show AcknowledgeThirdPartyJobOutput where
  show = genericShow
instance decodeAcknowledgeThirdPartyJobOutput :: Decode AcknowledgeThirdPartyJobOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAcknowledgeThirdPartyJobOutput :: Encode AcknowledgeThirdPartyJobOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionCategory = ActionCategory String
derive instance newtypeActionCategory :: Newtype ActionCategory _
derive instance repGenericActionCategory :: Generic ActionCategory _
instance showActionCategory :: Show ActionCategory where
  show = genericShow
instance decodeActionCategory :: Decode ActionCategory where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionCategory :: Encode ActionCategory where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an action configuration.</p>
newtype ActionConfiguration = ActionConfiguration 
  { "Configuration'" :: NullOrUndefined.NullOrUndefined (ActionConfigurationMap)
  }
derive instance newtypeActionConfiguration :: Newtype ActionConfiguration _
derive instance repGenericActionConfiguration :: Generic ActionConfiguration _
instance showActionConfiguration :: Show ActionConfiguration where
  show = genericShow
instance decodeActionConfiguration :: Decode ActionConfiguration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionConfiguration :: Encode ActionConfiguration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionConfigurationKey = ActionConfigurationKey String
derive instance newtypeActionConfigurationKey :: Newtype ActionConfigurationKey _
derive instance repGenericActionConfigurationKey :: Generic ActionConfigurationKey _
instance showActionConfigurationKey :: Show ActionConfigurationKey where
  show = genericShow
instance decodeActionConfigurationKey :: Decode ActionConfigurationKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionConfigurationKey :: Encode ActionConfigurationKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionConfigurationMap = ActionConfigurationMap (StrMap.StrMap ActionConfigurationValue)
derive instance newtypeActionConfigurationMap :: Newtype ActionConfigurationMap _
derive instance repGenericActionConfigurationMap :: Generic ActionConfigurationMap _
instance showActionConfigurationMap :: Show ActionConfigurationMap where
  show = genericShow
instance decodeActionConfigurationMap :: Decode ActionConfigurationMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionConfigurationMap :: Encode ActionConfigurationMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an action configuration property.</p>
newtype ActionConfigurationProperty = ActionConfigurationProperty 
  { "Name'" :: (ActionConfigurationKey)
  , "Required'" :: (Boolean)
  , "Key'" :: (Boolean)
  , "Secret'" :: (Boolean)
  , "Queryable'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Description'" :: NullOrUndefined.NullOrUndefined (Description)
  , "Type'" :: NullOrUndefined.NullOrUndefined (ActionConfigurationPropertyType)
  }
derive instance newtypeActionConfigurationProperty :: Newtype ActionConfigurationProperty _
derive instance repGenericActionConfigurationProperty :: Generic ActionConfigurationProperty _
instance showActionConfigurationProperty :: Show ActionConfigurationProperty where
  show = genericShow
instance decodeActionConfigurationProperty :: Decode ActionConfigurationProperty where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionConfigurationProperty :: Encode ActionConfigurationProperty where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionConfigurationPropertyList = ActionConfigurationPropertyList (Array ActionConfigurationProperty)
derive instance newtypeActionConfigurationPropertyList :: Newtype ActionConfigurationPropertyList _
derive instance repGenericActionConfigurationPropertyList :: Generic ActionConfigurationPropertyList _
instance showActionConfigurationPropertyList :: Show ActionConfigurationPropertyList where
  show = genericShow
instance decodeActionConfigurationPropertyList :: Decode ActionConfigurationPropertyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionConfigurationPropertyList :: Encode ActionConfigurationPropertyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionConfigurationPropertyType = ActionConfigurationPropertyType String
derive instance newtypeActionConfigurationPropertyType :: Newtype ActionConfigurationPropertyType _
derive instance repGenericActionConfigurationPropertyType :: Generic ActionConfigurationPropertyType _
instance showActionConfigurationPropertyType :: Show ActionConfigurationPropertyType where
  show = genericShow
instance decodeActionConfigurationPropertyType :: Decode ActionConfigurationPropertyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionConfigurationPropertyType :: Encode ActionConfigurationPropertyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionConfigurationQueryableValue = ActionConfigurationQueryableValue String
derive instance newtypeActionConfigurationQueryableValue :: Newtype ActionConfigurationQueryableValue _
derive instance repGenericActionConfigurationQueryableValue :: Generic ActionConfigurationQueryableValue _
instance showActionConfigurationQueryableValue :: Show ActionConfigurationQueryableValue where
  show = genericShow
instance decodeActionConfigurationQueryableValue :: Decode ActionConfigurationQueryableValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionConfigurationQueryableValue :: Encode ActionConfigurationQueryableValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionConfigurationValue = ActionConfigurationValue String
derive instance newtypeActionConfigurationValue :: Newtype ActionConfigurationValue _
derive instance repGenericActionConfigurationValue :: Generic ActionConfigurationValue _
instance showActionConfigurationValue :: Show ActionConfigurationValue where
  show = genericShow
instance decodeActionConfigurationValue :: Decode ActionConfigurationValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionConfigurationValue :: Encode ActionConfigurationValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the context of an action within the stage of a pipeline to a job worker.</p>
newtype ActionContext = ActionContext 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ActionName)
  }
derive instance newtypeActionContext :: Newtype ActionContext _
derive instance repGenericActionContext :: Generic ActionContext _
instance showActionContext :: Show ActionContext where
  show = genericShow
instance decodeActionContext :: Decode ActionContext where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionContext :: Encode ActionContext where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an action declaration.</p>
newtype ActionDeclaration = ActionDeclaration 
  { "Name'" :: (ActionName)
  , "ActionTypeId'" :: (ActionTypeId)
  , "RunOrder'" :: NullOrUndefined.NullOrUndefined (ActionRunOrder)
  , "Configuration'" :: NullOrUndefined.NullOrUndefined (ActionConfigurationMap)
  , "OutputArtifacts'" :: NullOrUndefined.NullOrUndefined (OutputArtifactList)
  , "InputArtifacts'" :: NullOrUndefined.NullOrUndefined (InputArtifactList)
  , "RoleArn'" :: NullOrUndefined.NullOrUndefined (RoleArn)
  }
derive instance newtypeActionDeclaration :: Newtype ActionDeclaration _
derive instance repGenericActionDeclaration :: Generic ActionDeclaration _
instance showActionDeclaration :: Show ActionDeclaration where
  show = genericShow
instance decodeActionDeclaration :: Decode ActionDeclaration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionDeclaration :: Encode ActionDeclaration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the run of an action.</p>
newtype ActionExecution = ActionExecution 
  { "Status'" :: NullOrUndefined.NullOrUndefined (ActionExecutionStatus)
  , "Summary'" :: NullOrUndefined.NullOrUndefined (ExecutionSummary)
  , "LastStatusChange'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Token'" :: NullOrUndefined.NullOrUndefined (ActionExecutionToken)
  , "LastUpdatedBy'" :: NullOrUndefined.NullOrUndefined (LastUpdatedBy)
  , "ExternalExecutionId'" :: NullOrUndefined.NullOrUndefined (ExecutionId)
  , "ExternalExecutionUrl'" :: NullOrUndefined.NullOrUndefined (Url)
  , "PercentComplete'" :: NullOrUndefined.NullOrUndefined (Percentage)
  , "ErrorDetails'" :: NullOrUndefined.NullOrUndefined (ErrorDetails)
  }
derive instance newtypeActionExecution :: Newtype ActionExecution _
derive instance repGenericActionExecution :: Generic ActionExecution _
instance showActionExecution :: Show ActionExecution where
  show = genericShow
instance decodeActionExecution :: Decode ActionExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionExecution :: Encode ActionExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionExecutionStatus = ActionExecutionStatus String
derive instance newtypeActionExecutionStatus :: Newtype ActionExecutionStatus _
derive instance repGenericActionExecutionStatus :: Generic ActionExecutionStatus _
instance showActionExecutionStatus :: Show ActionExecutionStatus where
  show = genericShow
instance decodeActionExecutionStatus :: Decode ActionExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionExecutionStatus :: Encode ActionExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionExecutionToken = ActionExecutionToken String
derive instance newtypeActionExecutionToken :: Newtype ActionExecutionToken _
derive instance repGenericActionExecutionToken :: Generic ActionExecutionToken _
instance showActionExecutionToken :: Show ActionExecutionToken where
  show = genericShow
instance decodeActionExecutionToken :: Decode ActionExecutionToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionExecutionToken :: Encode ActionExecutionToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionName = ActionName String
derive instance newtypeActionName :: Newtype ActionName _
derive instance repGenericActionName :: Generic ActionName _
instance showActionName :: Show ActionName where
  show = genericShow
instance decodeActionName :: Decode ActionName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionName :: Encode ActionName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified action cannot be found.</p>
newtype ActionNotFoundException = ActionNotFoundException Types.NoArguments
derive instance newtypeActionNotFoundException :: Newtype ActionNotFoundException _
derive instance repGenericActionNotFoundException :: Generic ActionNotFoundException _
instance showActionNotFoundException :: Show ActionNotFoundException where
  show = genericShow
instance decodeActionNotFoundException :: Decode ActionNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionNotFoundException :: Encode ActionNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionOwner = ActionOwner String
derive instance newtypeActionOwner :: Newtype ActionOwner _
derive instance repGenericActionOwner :: Generic ActionOwner _
instance showActionOwner :: Show ActionOwner where
  show = genericShow
instance decodeActionOwner :: Decode ActionOwner where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionOwner :: Encode ActionOwner where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionProvider = ActionProvider String
derive instance newtypeActionProvider :: Newtype ActionProvider _
derive instance repGenericActionProvider :: Generic ActionProvider _
instance showActionProvider :: Show ActionProvider where
  show = genericShow
instance decodeActionProvider :: Decode ActionProvider where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionProvider :: Encode ActionProvider where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the version (or revision) of an action.</p>
newtype ActionRevision = ActionRevision 
  { "RevisionId'" :: (Revision)
  , "RevisionChangeId'" :: (RevisionChangeIdentifier)
  , "Created'" :: (Number)
  }
derive instance newtypeActionRevision :: Newtype ActionRevision _
derive instance repGenericActionRevision :: Generic ActionRevision _
instance showActionRevision :: Show ActionRevision where
  show = genericShow
instance decodeActionRevision :: Decode ActionRevision where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionRevision :: Encode ActionRevision where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionRunOrder = ActionRunOrder Int
derive instance newtypeActionRunOrder :: Newtype ActionRunOrder _
derive instance repGenericActionRunOrder :: Generic ActionRunOrder _
instance showActionRunOrder :: Show ActionRunOrder where
  show = genericShow
instance decodeActionRunOrder :: Decode ActionRunOrder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionRunOrder :: Encode ActionRunOrder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the state of an action.</p>
newtype ActionState = ActionState 
  { "ActionName'" :: NullOrUndefined.NullOrUndefined (ActionName)
  , "CurrentRevision'" :: NullOrUndefined.NullOrUndefined (ActionRevision)
  , "LatestExecution'" :: NullOrUndefined.NullOrUndefined (ActionExecution)
  , "EntityUrl'" :: NullOrUndefined.NullOrUndefined (Url)
  , "RevisionUrl'" :: NullOrUndefined.NullOrUndefined (Url)
  }
derive instance newtypeActionState :: Newtype ActionState _
derive instance repGenericActionState :: Generic ActionState _
instance showActionState :: Show ActionState where
  show = genericShow
instance decodeActionState :: Decode ActionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionState :: Encode ActionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionStateList = ActionStateList (Array ActionState)
derive instance newtypeActionStateList :: Newtype ActionStateList _
derive instance repGenericActionStateList :: Generic ActionStateList _
instance showActionStateList :: Show ActionStateList where
  show = genericShow
instance decodeActionStateList :: Decode ActionStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionStateList :: Encode ActionStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about the details of an action type.</p>
newtype ActionType = ActionType 
  { "Id'" :: (ActionTypeId)
  , "Settings'" :: NullOrUndefined.NullOrUndefined (ActionTypeSettings)
  , "ActionConfigurationProperties'" :: NullOrUndefined.NullOrUndefined (ActionConfigurationPropertyList)
  , "InputArtifactDetails'" :: (ArtifactDetails)
  , "OutputArtifactDetails'" :: (ArtifactDetails)
  }
derive instance newtypeActionType :: Newtype ActionType _
derive instance repGenericActionType :: Generic ActionType _
instance showActionType :: Show ActionType where
  show = genericShow
instance decodeActionType :: Decode ActionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionType :: Encode ActionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an action type.</p>
newtype ActionTypeId = ActionTypeId 
  { "Category'" :: (ActionCategory)
  , "Owner'" :: (ActionOwner)
  , "Provider'" :: (ActionProvider)
  , "Version'" :: (Version)
  }
derive instance newtypeActionTypeId :: Newtype ActionTypeId _
derive instance repGenericActionTypeId :: Generic ActionTypeId _
instance showActionTypeId :: Show ActionTypeId where
  show = genericShow
instance decodeActionTypeId :: Decode ActionTypeId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionTypeId :: Encode ActionTypeId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActionTypeList = ActionTypeList (Array ActionType)
derive instance newtypeActionTypeList :: Newtype ActionTypeList _
derive instance repGenericActionTypeList :: Generic ActionTypeList _
instance showActionTypeList :: Show ActionTypeList where
  show = genericShow
instance decodeActionTypeList :: Decode ActionTypeList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionTypeList :: Encode ActionTypeList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified action type cannot be found.</p>
newtype ActionTypeNotFoundException = ActionTypeNotFoundException Types.NoArguments
derive instance newtypeActionTypeNotFoundException :: Newtype ActionTypeNotFoundException _
derive instance repGenericActionTypeNotFoundException :: Generic ActionTypeNotFoundException _
instance showActionTypeNotFoundException :: Show ActionTypeNotFoundException where
  show = genericShow
instance decodeActionTypeNotFoundException :: Decode ActionTypeNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionTypeNotFoundException :: Encode ActionTypeNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about the settings for an action type.</p>
newtype ActionTypeSettings = ActionTypeSettings 
  { "ThirdPartyConfigurationUrl'" :: NullOrUndefined.NullOrUndefined (Url)
  , "EntityUrlTemplate'" :: NullOrUndefined.NullOrUndefined (UrlTemplate)
  , "ExecutionUrlTemplate'" :: NullOrUndefined.NullOrUndefined (UrlTemplate)
  , "RevisionUrlTemplate'" :: NullOrUndefined.NullOrUndefined (UrlTemplate)
  }
derive instance newtypeActionTypeSettings :: Newtype ActionTypeSettings _
derive instance repGenericActionTypeSettings :: Generic ActionTypeSettings _
instance showActionTypeSettings :: Show ActionTypeSettings where
  show = genericShow
instance decodeActionTypeSettings :: Decode ActionTypeSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActionTypeSettings :: Encode ActionTypeSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The approval action has already been approved or rejected.</p>
newtype ApprovalAlreadyCompletedException = ApprovalAlreadyCompletedException Types.NoArguments
derive instance newtypeApprovalAlreadyCompletedException :: Newtype ApprovalAlreadyCompletedException _
derive instance repGenericApprovalAlreadyCompletedException :: Generic ApprovalAlreadyCompletedException _
instance showApprovalAlreadyCompletedException :: Show ApprovalAlreadyCompletedException where
  show = genericShow
instance decodeApprovalAlreadyCompletedException :: Decode ApprovalAlreadyCompletedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApprovalAlreadyCompletedException :: Encode ApprovalAlreadyCompletedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the result of an approval request.</p>
newtype ApprovalResult = ApprovalResult 
  { "Summary'" :: (ApprovalSummary)
  , "Status'" :: (ApprovalStatus)
  }
derive instance newtypeApprovalResult :: Newtype ApprovalResult _
derive instance repGenericApprovalResult :: Generic ApprovalResult _
instance showApprovalResult :: Show ApprovalResult where
  show = genericShow
instance decodeApprovalResult :: Decode ApprovalResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApprovalResult :: Encode ApprovalResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApprovalStatus = ApprovalStatus String
derive instance newtypeApprovalStatus :: Newtype ApprovalStatus _
derive instance repGenericApprovalStatus :: Generic ApprovalStatus _
instance showApprovalStatus :: Show ApprovalStatus where
  show = genericShow
instance decodeApprovalStatus :: Decode ApprovalStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApprovalStatus :: Encode ApprovalStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApprovalSummary = ApprovalSummary String
derive instance newtypeApprovalSummary :: Newtype ApprovalSummary _
derive instance repGenericApprovalSummary :: Generic ApprovalSummary _
instance showApprovalSummary :: Show ApprovalSummary where
  show = genericShow
instance decodeApprovalSummary :: Decode ApprovalSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApprovalSummary :: Encode ApprovalSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ApprovalToken = ApprovalToken String
derive instance newtypeApprovalToken :: Newtype ApprovalToken _
derive instance repGenericApprovalToken :: Generic ApprovalToken _
instance showApprovalToken :: Show ApprovalToken where
  show = genericShow
instance decodeApprovalToken :: Decode ApprovalToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeApprovalToken :: Encode ApprovalToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an artifact that will be worked upon by actions in the pipeline.</p>
newtype Artifact = Artifact 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ArtifactName)
  , "Revision'" :: NullOrUndefined.NullOrUndefined (Revision)
  , "Location'" :: NullOrUndefined.NullOrUndefined (ArtifactLocation)
  }
derive instance newtypeArtifact :: Newtype Artifact _
derive instance repGenericArtifact :: Generic Artifact _
instance showArtifact :: Show Artifact where
  show = genericShow
instance decodeArtifact :: Decode Artifact where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifact :: Encode Artifact where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns information about the details of an artifact.</p>
newtype ArtifactDetails = ArtifactDetails 
  { "MinimumCount'" :: (MinimumArtifactCount)
  , "MaximumCount'" :: (MaximumArtifactCount)
  }
derive instance newtypeArtifactDetails :: Newtype ArtifactDetails _
derive instance repGenericArtifactDetails :: Generic ArtifactDetails _
instance showArtifactDetails :: Show ArtifactDetails where
  show = genericShow
instance decodeArtifactDetails :: Decode ArtifactDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactDetails :: Encode ArtifactDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactList = ArtifactList (Array Artifact)
derive instance newtypeArtifactList :: Newtype ArtifactList _
derive instance repGenericArtifactList :: Generic ArtifactList _
instance showArtifactList :: Show ArtifactList where
  show = genericShow
instance decodeArtifactList :: Decode ArtifactList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactList :: Encode ArtifactList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the location of an artifact.</p>
newtype ArtifactLocation = ArtifactLocation 
  { "Type'" :: NullOrUndefined.NullOrUndefined (ArtifactLocationType)
  , "S3Location'" :: NullOrUndefined.NullOrUndefined (S3ArtifactLocation)
  }
derive instance newtypeArtifactLocation :: Newtype ArtifactLocation _
derive instance repGenericArtifactLocation :: Generic ArtifactLocation _
instance showArtifactLocation :: Show ArtifactLocation where
  show = genericShow
instance decodeArtifactLocation :: Decode ArtifactLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactLocation :: Encode ArtifactLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactLocationType = ArtifactLocationType String
derive instance newtypeArtifactLocationType :: Newtype ArtifactLocationType _
derive instance repGenericArtifactLocationType :: Generic ArtifactLocationType _
instance showArtifactLocationType :: Show ArtifactLocationType where
  show = genericShow
instance decodeArtifactLocationType :: Decode ArtifactLocationType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactLocationType :: Encode ArtifactLocationType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactName = ArtifactName String
derive instance newtypeArtifactName :: Newtype ArtifactName _
derive instance repGenericArtifactName :: Generic ArtifactName _
instance showArtifactName :: Show ArtifactName where
  show = genericShow
instance decodeArtifactName :: Decode ArtifactName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactName :: Encode ArtifactName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents revision details of an artifact. </p>
newtype ArtifactRevision = ArtifactRevision 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ArtifactName)
  , "RevisionId'" :: NullOrUndefined.NullOrUndefined (Revision)
  , "RevisionChangeIdentifier'" :: NullOrUndefined.NullOrUndefined (RevisionChangeIdentifier)
  , "RevisionSummary'" :: NullOrUndefined.NullOrUndefined (RevisionSummary)
  , "Created'" :: NullOrUndefined.NullOrUndefined (Number)
  , "RevisionUrl'" :: NullOrUndefined.NullOrUndefined (Url)
  }
derive instance newtypeArtifactRevision :: Newtype ArtifactRevision _
derive instance repGenericArtifactRevision :: Generic ArtifactRevision _
instance showArtifactRevision :: Show ArtifactRevision where
  show = genericShow
instance decodeArtifactRevision :: Decode ArtifactRevision where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactRevision :: Encode ArtifactRevision where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactRevisionList = ArtifactRevisionList (Array ArtifactRevision)
derive instance newtypeArtifactRevisionList :: Newtype ArtifactRevisionList _
derive instance repGenericArtifactRevisionList :: Generic ArtifactRevisionList _
instance showArtifactRevisionList :: Show ArtifactRevisionList where
  show = genericShow
instance decodeArtifactRevisionList :: Decode ArtifactRevisionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactRevisionList :: Encode ArtifactRevisionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The Amazon S3 bucket where artifacts are stored for the pipeline.</p>
newtype ArtifactStore = ArtifactStore 
  { "Type'" :: (ArtifactStoreType)
  , "Location'" :: (ArtifactStoreLocation)
  , "EncryptionKey'" :: NullOrUndefined.NullOrUndefined (EncryptionKey)
  }
derive instance newtypeArtifactStore :: Newtype ArtifactStore _
derive instance repGenericArtifactStore :: Generic ArtifactStore _
instance showArtifactStore :: Show ArtifactStore where
  show = genericShow
instance decodeArtifactStore :: Decode ArtifactStore where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactStore :: Encode ArtifactStore where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactStoreLocation = ArtifactStoreLocation String
derive instance newtypeArtifactStoreLocation :: Newtype ArtifactStoreLocation _
derive instance repGenericArtifactStoreLocation :: Generic ArtifactStoreLocation _
instance showArtifactStoreLocation :: Show ArtifactStoreLocation where
  show = genericShow
instance decodeArtifactStoreLocation :: Decode ArtifactStoreLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactStoreLocation :: Encode ArtifactStoreLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactStoreType = ArtifactStoreType String
derive instance newtypeArtifactStoreType :: Newtype ArtifactStoreType _
derive instance repGenericArtifactStoreType :: Generic ArtifactStoreType _
instance showArtifactStoreType :: Show ArtifactStoreType where
  show = genericShow
instance decodeArtifactStoreType :: Decode ArtifactStoreType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactStoreType :: Encode ArtifactStoreType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Reserved for future use.</p>
newtype BlockerDeclaration = BlockerDeclaration 
  { "Name'" :: (BlockerName)
  , "Type'" :: (BlockerType)
  }
derive instance newtypeBlockerDeclaration :: Newtype BlockerDeclaration _
derive instance repGenericBlockerDeclaration :: Generic BlockerDeclaration _
instance showBlockerDeclaration :: Show BlockerDeclaration where
  show = genericShow
instance decodeBlockerDeclaration :: Decode BlockerDeclaration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlockerDeclaration :: Encode BlockerDeclaration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BlockerName = BlockerName String
derive instance newtypeBlockerName :: Newtype BlockerName _
derive instance repGenericBlockerName :: Generic BlockerName _
instance showBlockerName :: Show BlockerName where
  show = genericShow
instance decodeBlockerName :: Decode BlockerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlockerName :: Encode BlockerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BlockerType = BlockerType String
derive instance newtypeBlockerType :: Newtype BlockerType _
derive instance repGenericBlockerType :: Generic BlockerType _
instance showBlockerType :: Show BlockerType where
  show = genericShow
instance decodeBlockerType :: Decode BlockerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBlockerType :: Encode BlockerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientId = ClientId String
derive instance newtypeClientId :: Newtype ClientId _
derive instance repGenericClientId :: Generic ClientId _
instance showClientId :: Show ClientId where
  show = genericShow
instance decodeClientId :: Decode ClientId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientId :: Encode ClientId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _
derive instance repGenericClientToken :: Generic ClientToken _
instance showClientToken :: Show ClientToken where
  show = genericShow
instance decodeClientToken :: Decode ClientToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientToken :: Encode ClientToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Code = Code String
derive instance newtypeCode :: Newtype Code _
derive instance repGenericCode :: Generic Code _
instance showCode :: Show Code where
  show = genericShow
instance decodeCode :: Decode Code where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCode :: Encode Code where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ContinuationToken = ContinuationToken String
derive instance newtypeContinuationToken :: Newtype ContinuationToken _
derive instance repGenericContinuationToken :: Generic ContinuationToken _
instance showContinuationToken :: Show ContinuationToken where
  show = genericShow
instance decodeContinuationToken :: Decode ContinuationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContinuationToken :: Encode ContinuationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a CreateCustomActionType operation.</p>
newtype CreateCustomActionTypeInput = CreateCustomActionTypeInput 
  { "Category'" :: (ActionCategory)
  , "Provider'" :: (ActionProvider)
  , "Version'" :: (Version)
  , "Settings'" :: NullOrUndefined.NullOrUndefined (ActionTypeSettings)
  , "ConfigurationProperties'" :: NullOrUndefined.NullOrUndefined (ActionConfigurationPropertyList)
  , "InputArtifactDetails'" :: (ArtifactDetails)
  , "OutputArtifactDetails'" :: (ArtifactDetails)
  }
derive instance newtypeCreateCustomActionTypeInput :: Newtype CreateCustomActionTypeInput _
derive instance repGenericCreateCustomActionTypeInput :: Generic CreateCustomActionTypeInput _
instance showCreateCustomActionTypeInput :: Show CreateCustomActionTypeInput where
  show = genericShow
instance decodeCreateCustomActionTypeInput :: Decode CreateCustomActionTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCustomActionTypeInput :: Encode CreateCustomActionTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a CreateCustomActionType operation.</p>
newtype CreateCustomActionTypeOutput = CreateCustomActionTypeOutput 
  { "ActionType'" :: (ActionType)
  }
derive instance newtypeCreateCustomActionTypeOutput :: Newtype CreateCustomActionTypeOutput _
derive instance repGenericCreateCustomActionTypeOutput :: Generic CreateCustomActionTypeOutput _
instance showCreateCustomActionTypeOutput :: Show CreateCustomActionTypeOutput where
  show = genericShow
instance decodeCreateCustomActionTypeOutput :: Decode CreateCustomActionTypeOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCustomActionTypeOutput :: Encode CreateCustomActionTypeOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a CreatePipeline action.</p>
newtype CreatePipelineInput = CreatePipelineInput 
  { "Pipeline'" :: (PipelineDeclaration)
  }
derive instance newtypeCreatePipelineInput :: Newtype CreatePipelineInput _
derive instance repGenericCreatePipelineInput :: Generic CreatePipelineInput _
instance showCreatePipelineInput :: Show CreatePipelineInput where
  show = genericShow
instance decodeCreatePipelineInput :: Decode CreatePipelineInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePipelineInput :: Encode CreatePipelineInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a CreatePipeline action.</p>
newtype CreatePipelineOutput = CreatePipelineOutput 
  { "Pipeline'" :: NullOrUndefined.NullOrUndefined (PipelineDeclaration)
  }
derive instance newtypeCreatePipelineOutput :: Newtype CreatePipelineOutput _
derive instance repGenericCreatePipelineOutput :: Generic CreatePipelineOutput _
instance showCreatePipelineOutput :: Show CreatePipelineOutput where
  show = genericShow
instance decodeCreatePipelineOutput :: Decode CreatePipelineOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatePipelineOutput :: Encode CreatePipelineOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about a current revision.</p>
newtype CurrentRevision = CurrentRevision 
  { "Revision'" :: (Revision)
  , "ChangeIdentifier'" :: (RevisionChangeIdentifier)
  , "Created'" :: NullOrUndefined.NullOrUndefined (Time)
  , "RevisionSummary'" :: NullOrUndefined.NullOrUndefined (RevisionSummary)
  }
derive instance newtypeCurrentRevision :: Newtype CurrentRevision _
derive instance repGenericCurrentRevision :: Generic CurrentRevision _
instance showCurrentRevision :: Show CurrentRevision where
  show = genericShow
instance decodeCurrentRevision :: Decode CurrentRevision where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCurrentRevision :: Encode CurrentRevision where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a DeleteCustomActionType operation. The custom action will be marked as deleted.</p>
newtype DeleteCustomActionTypeInput = DeleteCustomActionTypeInput 
  { "Category'" :: (ActionCategory)
  , "Provider'" :: (ActionProvider)
  , "Version'" :: (Version)
  }
derive instance newtypeDeleteCustomActionTypeInput :: Newtype DeleteCustomActionTypeInput _
derive instance repGenericDeleteCustomActionTypeInput :: Generic DeleteCustomActionTypeInput _
instance showDeleteCustomActionTypeInput :: Show DeleteCustomActionTypeInput where
  show = genericShow
instance decodeDeleteCustomActionTypeInput :: Decode DeleteCustomActionTypeInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCustomActionTypeInput :: Encode DeleteCustomActionTypeInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a DeletePipeline action.</p>
newtype DeletePipelineInput = DeletePipelineInput 
  { "Name'" :: (PipelineName)
  }
derive instance newtypeDeletePipelineInput :: Newtype DeletePipelineInput _
derive instance repGenericDeletePipelineInput :: Generic DeletePipelineInput _
instance showDeletePipelineInput :: Show DeletePipelineInput where
  show = genericShow
instance decodeDeletePipelineInput :: Decode DeletePipelineInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeletePipelineInput :: Encode DeletePipelineInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a DisableStageTransition action.</p>
newtype DisableStageTransitionInput = DisableStageTransitionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "TransitionType'" :: (StageTransitionType)
  , "Reason'" :: (DisabledReason)
  }
derive instance newtypeDisableStageTransitionInput :: Newtype DisableStageTransitionInput _
derive instance repGenericDisableStageTransitionInput :: Generic DisableStageTransitionInput _
instance showDisableStageTransitionInput :: Show DisableStageTransitionInput where
  show = genericShow
instance decodeDisableStageTransitionInput :: Decode DisableStageTransitionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableStageTransitionInput :: Encode DisableStageTransitionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DisabledReason = DisabledReason String
derive instance newtypeDisabledReason :: Newtype DisabledReason _
derive instance repGenericDisabledReason :: Generic DisabledReason _
instance showDisabledReason :: Show DisabledReason where
  show = genericShow
instance decodeDisabledReason :: Decode DisabledReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisabledReason :: Encode DisabledReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an EnableStageTransition action.</p>
newtype EnableStageTransitionInput = EnableStageTransitionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "TransitionType'" :: (StageTransitionType)
  }
derive instance newtypeEnableStageTransitionInput :: Newtype EnableStageTransitionInput _
derive instance repGenericEnableStageTransitionInput :: Generic EnableStageTransitionInput _
instance showEnableStageTransitionInput :: Show EnableStageTransitionInput where
  show = genericShow
instance decodeEnableStageTransitionInput :: Decode EnableStageTransitionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableStageTransitionInput :: Encode EnableStageTransitionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Enabled = Enabled Boolean
derive instance newtypeEnabled :: Newtype Enabled _
derive instance repGenericEnabled :: Generic Enabled _
instance showEnabled :: Show Enabled where
  show = genericShow
instance decodeEnabled :: Decode Enabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnabled :: Encode Enabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.</p>
newtype EncryptionKey = EncryptionKey 
  { "Id'" :: (EncryptionKeyId)
  , "Type'" :: (EncryptionKeyType)
  }
derive instance newtypeEncryptionKey :: Newtype EncryptionKey _
derive instance repGenericEncryptionKey :: Generic EncryptionKey _
instance showEncryptionKey :: Show EncryptionKey where
  show = genericShow
instance decodeEncryptionKey :: Decode EncryptionKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionKey :: Encode EncryptionKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptionKeyId = EncryptionKeyId String
derive instance newtypeEncryptionKeyId :: Newtype EncryptionKeyId _
derive instance repGenericEncryptionKeyId :: Generic EncryptionKeyId _
instance showEncryptionKeyId :: Show EncryptionKeyId where
  show = genericShow
instance decodeEncryptionKeyId :: Decode EncryptionKeyId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionKeyId :: Encode EncryptionKeyId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EncryptionKeyType = EncryptionKeyType String
derive instance newtypeEncryptionKeyType :: Newtype EncryptionKeyType _
derive instance repGenericEncryptionKeyType :: Generic EncryptionKeyType _
instance showEncryptionKeyType :: Show EncryptionKeyType where
  show = genericShow
instance decodeEncryptionKeyType :: Decode EncryptionKeyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEncryptionKeyType :: Encode EncryptionKeyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an error in AWS CodePipeline.</p>
newtype ErrorDetails = ErrorDetails 
  { "Code'" :: NullOrUndefined.NullOrUndefined (Code)
  , "Message'" :: NullOrUndefined.NullOrUndefined (Message)
  }
derive instance newtypeErrorDetails :: Newtype ErrorDetails _
derive instance repGenericErrorDetails :: Generic ErrorDetails _
instance showErrorDetails :: Show ErrorDetails where
  show = genericShow
instance decodeErrorDetails :: Decode ErrorDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorDetails :: Encode ErrorDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The details of the actions taken and results produced on an artifact as it passes through stages in the pipeline.</p>
newtype ExecutionDetails = ExecutionDetails 
  { "Summary'" :: NullOrUndefined.NullOrUndefined (ExecutionSummary)
  , "ExternalExecutionId'" :: NullOrUndefined.NullOrUndefined (ExecutionId)
  , "PercentComplete'" :: NullOrUndefined.NullOrUndefined (Percentage)
  }
derive instance newtypeExecutionDetails :: Newtype ExecutionDetails _
derive instance repGenericExecutionDetails :: Generic ExecutionDetails _
instance showExecutionDetails :: Show ExecutionDetails where
  show = genericShow
instance decodeExecutionDetails :: Decode ExecutionDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionDetails :: Encode ExecutionDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionId = ExecutionId String
derive instance newtypeExecutionId :: Newtype ExecutionId _
derive instance repGenericExecutionId :: Generic ExecutionId _
instance showExecutionId :: Show ExecutionId where
  show = genericShow
instance decodeExecutionId :: Decode ExecutionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionId :: Encode ExecutionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExecutionSummary = ExecutionSummary String
derive instance newtypeExecutionSummary :: Newtype ExecutionSummary _
derive instance repGenericExecutionSummary :: Generic ExecutionSummary _
instance showExecutionSummary :: Show ExecutionSummary where
  show = genericShow
instance decodeExecutionSummary :: Decode ExecutionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExecutionSummary :: Encode ExecutionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about failure details.</p>
newtype FailureDetails = FailureDetails 
  { "Type'" :: (FailureType)
  , "Message'" :: (Message)
  , "ExternalExecutionId'" :: NullOrUndefined.NullOrUndefined (ExecutionId)
  }
derive instance newtypeFailureDetails :: Newtype FailureDetails _
derive instance repGenericFailureDetails :: Generic FailureDetails _
instance showFailureDetails :: Show FailureDetails where
  show = genericShow
instance decodeFailureDetails :: Decode FailureDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureDetails :: Encode FailureDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailureType = FailureType String
derive instance newtypeFailureType :: Newtype FailureType _
derive instance repGenericFailureType :: Generic FailureType _
instance showFailureType :: Show FailureType where
  show = genericShow
instance decodeFailureType :: Decode FailureType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailureType :: Encode FailureType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a GetJobDetails action.</p>
newtype GetJobDetailsInput = GetJobDetailsInput 
  { "JobId'" :: (JobId)
  }
derive instance newtypeGetJobDetailsInput :: Newtype GetJobDetailsInput _
derive instance repGenericGetJobDetailsInput :: Generic GetJobDetailsInput _
instance showGetJobDetailsInput :: Show GetJobDetailsInput where
  show = genericShow
instance decodeGetJobDetailsInput :: Decode GetJobDetailsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobDetailsInput :: Encode GetJobDetailsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a GetJobDetails action.</p>
newtype GetJobDetailsOutput = GetJobDetailsOutput 
  { "JobDetails'" :: NullOrUndefined.NullOrUndefined (JobDetails)
  }
derive instance newtypeGetJobDetailsOutput :: Newtype GetJobDetailsOutput _
derive instance repGenericGetJobDetailsOutput :: Generic GetJobDetailsOutput _
instance showGetJobDetailsOutput :: Show GetJobDetailsOutput where
  show = genericShow
instance decodeGetJobDetailsOutput :: Decode GetJobDetailsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetJobDetailsOutput :: Encode GetJobDetailsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a GetPipelineExecution action.</p>
newtype GetPipelineExecutionInput = GetPipelineExecutionInput 
  { "PipelineName'" :: (PipelineName)
  , "PipelineExecutionId'" :: (PipelineExecutionId)
  }
derive instance newtypeGetPipelineExecutionInput :: Newtype GetPipelineExecutionInput _
derive instance repGenericGetPipelineExecutionInput :: Generic GetPipelineExecutionInput _
instance showGetPipelineExecutionInput :: Show GetPipelineExecutionInput where
  show = genericShow
instance decodeGetPipelineExecutionInput :: Decode GetPipelineExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPipelineExecutionInput :: Encode GetPipelineExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a GetPipelineExecution action.</p>
newtype GetPipelineExecutionOutput = GetPipelineExecutionOutput 
  { "PipelineExecution'" :: NullOrUndefined.NullOrUndefined (PipelineExecution)
  }
derive instance newtypeGetPipelineExecutionOutput :: Newtype GetPipelineExecutionOutput _
derive instance repGenericGetPipelineExecutionOutput :: Generic GetPipelineExecutionOutput _
instance showGetPipelineExecutionOutput :: Show GetPipelineExecutionOutput where
  show = genericShow
instance decodeGetPipelineExecutionOutput :: Decode GetPipelineExecutionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPipelineExecutionOutput :: Encode GetPipelineExecutionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a GetPipeline action.</p>
newtype GetPipelineInput = GetPipelineInput 
  { "Name'" :: (PipelineName)
  , "Version'" :: NullOrUndefined.NullOrUndefined (PipelineVersion)
  }
derive instance newtypeGetPipelineInput :: Newtype GetPipelineInput _
derive instance repGenericGetPipelineInput :: Generic GetPipelineInput _
instance showGetPipelineInput :: Show GetPipelineInput where
  show = genericShow
instance decodeGetPipelineInput :: Decode GetPipelineInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPipelineInput :: Encode GetPipelineInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a GetPipeline action.</p>
newtype GetPipelineOutput = GetPipelineOutput 
  { "Pipeline'" :: NullOrUndefined.NullOrUndefined (PipelineDeclaration)
  , "Metadata'" :: NullOrUndefined.NullOrUndefined (PipelineMetadata)
  }
derive instance newtypeGetPipelineOutput :: Newtype GetPipelineOutput _
derive instance repGenericGetPipelineOutput :: Generic GetPipelineOutput _
instance showGetPipelineOutput :: Show GetPipelineOutput where
  show = genericShow
instance decodeGetPipelineOutput :: Decode GetPipelineOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPipelineOutput :: Encode GetPipelineOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a GetPipelineState action.</p>
newtype GetPipelineStateInput = GetPipelineStateInput 
  { "Name'" :: (PipelineName)
  }
derive instance newtypeGetPipelineStateInput :: Newtype GetPipelineStateInput _
derive instance repGenericGetPipelineStateInput :: Generic GetPipelineStateInput _
instance showGetPipelineStateInput :: Show GetPipelineStateInput where
  show = genericShow
instance decodeGetPipelineStateInput :: Decode GetPipelineStateInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPipelineStateInput :: Encode GetPipelineStateInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a GetPipelineState action.</p>
newtype GetPipelineStateOutput = GetPipelineStateOutput 
  { "PipelineName'" :: NullOrUndefined.NullOrUndefined (PipelineName)
  , "PipelineVersion'" :: NullOrUndefined.NullOrUndefined (PipelineVersion)
  , "StageStates'" :: NullOrUndefined.NullOrUndefined (StageStateList)
  , "Created'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeGetPipelineStateOutput :: Newtype GetPipelineStateOutput _
derive instance repGenericGetPipelineStateOutput :: Generic GetPipelineStateOutput _
instance showGetPipelineStateOutput :: Show GetPipelineStateOutput where
  show = genericShow
instance decodeGetPipelineStateOutput :: Decode GetPipelineStateOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetPipelineStateOutput :: Encode GetPipelineStateOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a GetThirdPartyJobDetails action.</p>
newtype GetThirdPartyJobDetailsInput = GetThirdPartyJobDetailsInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  }
derive instance newtypeGetThirdPartyJobDetailsInput :: Newtype GetThirdPartyJobDetailsInput _
derive instance repGenericGetThirdPartyJobDetailsInput :: Generic GetThirdPartyJobDetailsInput _
instance showGetThirdPartyJobDetailsInput :: Show GetThirdPartyJobDetailsInput where
  show = genericShow
instance decodeGetThirdPartyJobDetailsInput :: Decode GetThirdPartyJobDetailsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetThirdPartyJobDetailsInput :: Encode GetThirdPartyJobDetailsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a GetThirdPartyJobDetails action.</p>
newtype GetThirdPartyJobDetailsOutput = GetThirdPartyJobDetailsOutput 
  { "JobDetails'" :: NullOrUndefined.NullOrUndefined (ThirdPartyJobDetails)
  }
derive instance newtypeGetThirdPartyJobDetailsOutput :: Newtype GetThirdPartyJobDetailsOutput _
derive instance repGenericGetThirdPartyJobDetailsOutput :: Generic GetThirdPartyJobDetailsOutput _
instance showGetThirdPartyJobDetailsOutput :: Show GetThirdPartyJobDetailsOutput where
  show = genericShow
instance decodeGetThirdPartyJobDetailsOutput :: Decode GetThirdPartyJobDetailsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetThirdPartyJobDetailsOutput :: Encode GetThirdPartyJobDetailsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an artifact to be worked on, such as a test or build artifact.</p>
newtype InputArtifact = InputArtifact 
  { "Name'" :: (ArtifactName)
  }
derive instance newtypeInputArtifact :: Newtype InputArtifact _
derive instance repGenericInputArtifact :: Generic InputArtifact _
instance showInputArtifact :: Show InputArtifact where
  show = genericShow
instance decodeInputArtifact :: Decode InputArtifact where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputArtifact :: Encode InputArtifact where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InputArtifactList = InputArtifactList (Array InputArtifact)
derive instance newtypeInputArtifactList :: Newtype InputArtifactList _
derive instance repGenericInputArtifactList :: Generic InputArtifactList _
instance showInputArtifactList :: Show InputArtifactList where
  show = genericShow
instance decodeInputArtifactList :: Decode InputArtifactList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInputArtifactList :: Encode InputArtifactList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified action declaration was specified in an invalid format.</p>
newtype InvalidActionDeclarationException = InvalidActionDeclarationException Types.NoArguments
derive instance newtypeInvalidActionDeclarationException :: Newtype InvalidActionDeclarationException _
derive instance repGenericInvalidActionDeclarationException :: Generic InvalidActionDeclarationException _
instance showInvalidActionDeclarationException :: Show InvalidActionDeclarationException where
  show = genericShow
instance decodeInvalidActionDeclarationException :: Decode InvalidActionDeclarationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidActionDeclarationException :: Encode InvalidActionDeclarationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The approval request already received a response or has expired.</p>
newtype InvalidApprovalTokenException = InvalidApprovalTokenException Types.NoArguments
derive instance newtypeInvalidApprovalTokenException :: Newtype InvalidApprovalTokenException _
derive instance repGenericInvalidApprovalTokenException :: Generic InvalidApprovalTokenException _
instance showInvalidApprovalTokenException :: Show InvalidApprovalTokenException where
  show = genericShow
instance decodeInvalidApprovalTokenException :: Decode InvalidApprovalTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidApprovalTokenException :: Encode InvalidApprovalTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Reserved for future use.</p>
newtype InvalidBlockerDeclarationException = InvalidBlockerDeclarationException Types.NoArguments
derive instance newtypeInvalidBlockerDeclarationException :: Newtype InvalidBlockerDeclarationException _
derive instance repGenericInvalidBlockerDeclarationException :: Generic InvalidBlockerDeclarationException _
instance showInvalidBlockerDeclarationException :: Show InvalidBlockerDeclarationException where
  show = genericShow
instance decodeInvalidBlockerDeclarationException :: Decode InvalidBlockerDeclarationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidBlockerDeclarationException :: Encode InvalidBlockerDeclarationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The client token was specified in an invalid format</p>
newtype InvalidClientTokenException = InvalidClientTokenException Types.NoArguments
derive instance newtypeInvalidClientTokenException :: Newtype InvalidClientTokenException _
derive instance repGenericInvalidClientTokenException :: Generic InvalidClientTokenException _
instance showInvalidClientTokenException :: Show InvalidClientTokenException where
  show = genericShow
instance decodeInvalidClientTokenException :: Decode InvalidClientTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidClientTokenException :: Encode InvalidClientTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified job was specified in an invalid format or cannot be found.</p>
newtype InvalidJobException = InvalidJobException Types.NoArguments
derive instance newtypeInvalidJobException :: Newtype InvalidJobException _
derive instance repGenericInvalidJobException :: Generic InvalidJobException _
instance showInvalidJobException :: Show InvalidJobException where
  show = genericShow
instance decodeInvalidJobException :: Decode InvalidJobException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidJobException :: Encode InvalidJobException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified job state was specified in an invalid format.</p>
newtype InvalidJobStateException = InvalidJobStateException Types.NoArguments
derive instance newtypeInvalidJobStateException :: Newtype InvalidJobStateException _
derive instance repGenericInvalidJobStateException :: Generic InvalidJobStateException _
instance showInvalidJobStateException :: Show InvalidJobStateException where
  show = genericShow
instance decodeInvalidJobStateException :: Decode InvalidJobStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidJobStateException :: Encode InvalidJobStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The next token was specified in an invalid format. Make sure that the next token you provided is the token returned by a previous call.</p>
newtype InvalidNextTokenException = InvalidNextTokenException Types.NoArguments
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified nonce was specified in an invalid format.</p>
newtype InvalidNonceException = InvalidNonceException Types.NoArguments
derive instance newtypeInvalidNonceException :: Newtype InvalidNonceException _
derive instance repGenericInvalidNonceException :: Generic InvalidNonceException _
instance showInvalidNonceException :: Show InvalidNonceException where
  show = genericShow
instance decodeInvalidNonceException :: Decode InvalidNonceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNonceException :: Encode InvalidNonceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified stage declaration was specified in an invalid format.</p>
newtype InvalidStageDeclarationException = InvalidStageDeclarationException Types.NoArguments
derive instance newtypeInvalidStageDeclarationException :: Newtype InvalidStageDeclarationException _
derive instance repGenericInvalidStageDeclarationException :: Generic InvalidStageDeclarationException _
instance showInvalidStageDeclarationException :: Show InvalidStageDeclarationException where
  show = genericShow
instance decodeInvalidStageDeclarationException :: Decode InvalidStageDeclarationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidStageDeclarationException :: Encode InvalidStageDeclarationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified structure was specified in an invalid format.</p>
newtype InvalidStructureException = InvalidStructureException Types.NoArguments
derive instance newtypeInvalidStructureException :: Newtype InvalidStructureException _
derive instance repGenericInvalidStructureException :: Generic InvalidStructureException _
instance showInvalidStructureException :: Show InvalidStructureException where
  show = genericShow
instance decodeInvalidStructureException :: Decode InvalidStructureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidStructureException :: Encode InvalidStructureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about a job.</p>
newtype Job = Job 
  { "Id'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "Data'" :: NullOrUndefined.NullOrUndefined (JobData)
  , "Nonce'" :: NullOrUndefined.NullOrUndefined (Nonce)
  , "AccountId'" :: NullOrUndefined.NullOrUndefined (AccountId)
  }
derive instance newtypeJob :: Newtype Job _
derive instance repGenericJob :: Generic Job _
instance showJob :: Show Job where
  show = genericShow
instance decodeJob :: Decode Job where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJob :: Encode Job where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents additional information about a job required for a job worker to complete the job.</p>
newtype JobData = JobData 
  { "ActionTypeId'" :: NullOrUndefined.NullOrUndefined (ActionTypeId)
  , "ActionConfiguration'" :: NullOrUndefined.NullOrUndefined (ActionConfiguration)
  , "PipelineContext'" :: NullOrUndefined.NullOrUndefined (PipelineContext)
  , "InputArtifacts'" :: NullOrUndefined.NullOrUndefined (ArtifactList)
  , "OutputArtifacts'" :: NullOrUndefined.NullOrUndefined (ArtifactList)
  , "ArtifactCredentials'" :: NullOrUndefined.NullOrUndefined (AWSSessionCredentials)
  , "ContinuationToken'" :: NullOrUndefined.NullOrUndefined (ContinuationToken)
  , "EncryptionKey'" :: NullOrUndefined.NullOrUndefined (EncryptionKey)
  }
derive instance newtypeJobData :: Newtype JobData _
derive instance repGenericJobData :: Generic JobData _
instance showJobData :: Show JobData where
  show = genericShow
instance decodeJobData :: Decode JobData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobData :: Encode JobData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the details of a job.</p>
newtype JobDetails = JobDetails 
  { "Id'" :: NullOrUndefined.NullOrUndefined (JobId)
  , "Data'" :: NullOrUndefined.NullOrUndefined (JobData)
  , "AccountId'" :: NullOrUndefined.NullOrUndefined (AccountId)
  }
derive instance newtypeJobDetails :: Newtype JobDetails _
derive instance repGenericJobDetails :: Generic JobDetails _
instance showJobDetails :: Show JobDetails where
  show = genericShow
instance decodeJobDetails :: Decode JobDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobDetails :: Encode JobDetails where
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


newtype JobList = JobList (Array Job)
derive instance newtypeJobList :: Newtype JobList _
derive instance repGenericJobList :: Generic JobList _
instance showJobList :: Show JobList where
  show = genericShow
instance decodeJobList :: Decode JobList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobList :: Encode JobList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified job was specified in an invalid format or cannot be found.</p>
newtype JobNotFoundException = JobNotFoundException Types.NoArguments
derive instance newtypeJobNotFoundException :: Newtype JobNotFoundException _
derive instance repGenericJobNotFoundException :: Generic JobNotFoundException _
instance showJobNotFoundException :: Show JobNotFoundException where
  show = genericShow
instance decodeJobNotFoundException :: Decode JobNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobNotFoundException :: Encode JobNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _
derive instance repGenericJobStatus :: Generic JobStatus _
instance showJobStatus :: Show JobStatus where
  show = genericShow
instance decodeJobStatus :: Decode JobStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeJobStatus :: Encode JobStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastChangedAt = LastChangedAt Number
derive instance newtypeLastChangedAt :: Newtype LastChangedAt _
derive instance repGenericLastChangedAt :: Generic LastChangedAt _
instance showLastChangedAt :: Show LastChangedAt where
  show = genericShow
instance decodeLastChangedAt :: Decode LastChangedAt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastChangedAt :: Encode LastChangedAt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastChangedBy = LastChangedBy String
derive instance newtypeLastChangedBy :: Newtype LastChangedBy _
derive instance repGenericLastChangedBy :: Generic LastChangedBy _
instance showLastChangedBy :: Show LastChangedBy where
  show = genericShow
instance decodeLastChangedBy :: Decode LastChangedBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastChangedBy :: Encode LastChangedBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastUpdatedBy = LastUpdatedBy String
derive instance newtypeLastUpdatedBy :: Newtype LastUpdatedBy _
derive instance repGenericLastUpdatedBy :: Generic LastUpdatedBy _
instance showLastUpdatedBy :: Show LastUpdatedBy where
  show = genericShow
instance decodeLastUpdatedBy :: Decode LastUpdatedBy where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastUpdatedBy :: Encode LastUpdatedBy where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The number of pipelines associated with the AWS account has exceeded the limit allowed for the account.</p>
newtype LimitExceededException = LimitExceededException Types.NoArguments
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a ListActionTypes action.</p>
newtype ListActionTypesInput = ListActionTypesInput 
  { "ActionOwnerFilter'" :: NullOrUndefined.NullOrUndefined (ActionOwner)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListActionTypesInput :: Newtype ListActionTypesInput _
derive instance repGenericListActionTypesInput :: Generic ListActionTypesInput _
instance showListActionTypesInput :: Show ListActionTypesInput where
  show = genericShow
instance decodeListActionTypesInput :: Decode ListActionTypesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListActionTypesInput :: Encode ListActionTypesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a ListActionTypes action.</p>
newtype ListActionTypesOutput = ListActionTypesOutput 
  { "ActionTypes'" :: (ActionTypeList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListActionTypesOutput :: Newtype ListActionTypesOutput _
derive instance repGenericListActionTypesOutput :: Generic ListActionTypesOutput _
instance showListActionTypesOutput :: Show ListActionTypesOutput where
  show = genericShow
instance decodeListActionTypesOutput :: Decode ListActionTypesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListActionTypesOutput :: Encode ListActionTypesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a ListPipelineExecutions action.</p>
newtype ListPipelineExecutionsInput = ListPipelineExecutionsInput 
  { "PipelineName'" :: (PipelineName)
  , "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListPipelineExecutionsInput :: Newtype ListPipelineExecutionsInput _
derive instance repGenericListPipelineExecutionsInput :: Generic ListPipelineExecutionsInput _
instance showListPipelineExecutionsInput :: Show ListPipelineExecutionsInput where
  show = genericShow
instance decodeListPipelineExecutionsInput :: Decode ListPipelineExecutionsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPipelineExecutionsInput :: Encode ListPipelineExecutionsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a ListPipelineExecutions action.</p>
newtype ListPipelineExecutionsOutput = ListPipelineExecutionsOutput 
  { "PipelineExecutionSummaries'" :: NullOrUndefined.NullOrUndefined (PipelineExecutionSummaryList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListPipelineExecutionsOutput :: Newtype ListPipelineExecutionsOutput _
derive instance repGenericListPipelineExecutionsOutput :: Generic ListPipelineExecutionsOutput _
instance showListPipelineExecutionsOutput :: Show ListPipelineExecutionsOutput where
  show = genericShow
instance decodeListPipelineExecutionsOutput :: Decode ListPipelineExecutionsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPipelineExecutionsOutput :: Encode ListPipelineExecutionsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a ListPipelines action.</p>
newtype ListPipelinesInput = ListPipelinesInput 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListPipelinesInput :: Newtype ListPipelinesInput _
derive instance repGenericListPipelinesInput :: Generic ListPipelinesInput _
instance showListPipelinesInput :: Show ListPipelinesInput where
  show = genericShow
instance decodeListPipelinesInput :: Decode ListPipelinesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPipelinesInput :: Encode ListPipelinesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a ListPipelines action.</p>
newtype ListPipelinesOutput = ListPipelinesOutput 
  { "Pipelines'" :: NullOrUndefined.NullOrUndefined (PipelineList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListPipelinesOutput :: Newtype ListPipelinesOutput _
derive instance repGenericListPipelinesOutput :: Generic ListPipelinesOutput _
instance showListPipelinesOutput :: Show ListPipelinesOutput where
  show = genericShow
instance decodeListPipelinesOutput :: Decode ListPipelinesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListPipelinesOutput :: Encode ListPipelinesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxBatchSize = MaxBatchSize Int
derive instance newtypeMaxBatchSize :: Newtype MaxBatchSize _
derive instance repGenericMaxBatchSize :: Generic MaxBatchSize _
instance showMaxBatchSize :: Show MaxBatchSize where
  show = genericShow
instance decodeMaxBatchSize :: Decode MaxBatchSize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxBatchSize :: Encode MaxBatchSize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MaximumArtifactCount = MaximumArtifactCount Int
derive instance newtypeMaximumArtifactCount :: Newtype MaximumArtifactCount _
derive instance repGenericMaximumArtifactCount :: Generic MaximumArtifactCount _
instance showMaximumArtifactCount :: Show MaximumArtifactCount where
  show = genericShow
instance decodeMaximumArtifactCount :: Decode MaximumArtifactCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaximumArtifactCount :: Encode MaximumArtifactCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _
derive instance repGenericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow
instance decodeMessage :: Decode Message where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessage :: Encode Message where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MinimumArtifactCount = MinimumArtifactCount Int
derive instance newtypeMinimumArtifactCount :: Newtype MinimumArtifactCount _
derive instance repGenericMinimumArtifactCount :: Generic MinimumArtifactCount _
instance showMinimumArtifactCount :: Show MinimumArtifactCount where
  show = genericShow
instance decodeMinimumArtifactCount :: Decode MinimumArtifactCount where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMinimumArtifactCount :: Encode MinimumArtifactCount where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Nonce = Nonce String
derive instance newtypeNonce :: Newtype Nonce _
derive instance repGenericNonce :: Generic Nonce _
instance showNonce :: Show Nonce where
  show = genericShow
instance decodeNonce :: Decode Nonce where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonce :: Encode Nonce where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The stage has failed in a later run of the pipeline and the pipelineExecutionId associated with the request is out of date.</p>
newtype NotLatestPipelineExecutionException = NotLatestPipelineExecutionException Types.NoArguments
derive instance newtypeNotLatestPipelineExecutionException :: Newtype NotLatestPipelineExecutionException _
derive instance repGenericNotLatestPipelineExecutionException :: Generic NotLatestPipelineExecutionException _
instance showNotLatestPipelineExecutionException :: Show NotLatestPipelineExecutionException where
  show = genericShow
instance decodeNotLatestPipelineExecutionException :: Decode NotLatestPipelineExecutionException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotLatestPipelineExecutionException :: Encode NotLatestPipelineExecutionException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the output of an action.</p>
newtype OutputArtifact = OutputArtifact 
  { "Name'" :: (ArtifactName)
  }
derive instance newtypeOutputArtifact :: Newtype OutputArtifact _
derive instance repGenericOutputArtifact :: Generic OutputArtifact _
instance showOutputArtifact :: Show OutputArtifact where
  show = genericShow
instance decodeOutputArtifact :: Decode OutputArtifact where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputArtifact :: Encode OutputArtifact where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OutputArtifactList = OutputArtifactList (Array OutputArtifact)
derive instance newtypeOutputArtifactList :: Newtype OutputArtifactList _
derive instance repGenericOutputArtifactList :: Generic OutputArtifactList _
instance showOutputArtifactList :: Show OutputArtifactList where
  show = genericShow
instance decodeOutputArtifactList :: Decode OutputArtifactList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOutputArtifactList :: Encode OutputArtifactList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Percentage = Percentage Int
derive instance newtypePercentage :: Newtype Percentage _
derive instance repGenericPercentage :: Generic Percentage _
instance showPercentage :: Show Percentage where
  show = genericShow
instance decodePercentage :: Decode Percentage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePercentage :: Encode Percentage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineArn = PipelineArn String
derive instance newtypePipelineArn :: Newtype PipelineArn _
derive instance repGenericPipelineArn :: Generic PipelineArn _
instance showPipelineArn :: Show PipelineArn where
  show = genericShow
instance decodePipelineArn :: Decode PipelineArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineArn :: Encode PipelineArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about a pipeline to a job worker.</p>
newtype PipelineContext = PipelineContext 
  { "PipelineName'" :: NullOrUndefined.NullOrUndefined (PipelineName)
  , "Stage'" :: NullOrUndefined.NullOrUndefined (StageContext)
  , "Action'" :: NullOrUndefined.NullOrUndefined (ActionContext)
  }
derive instance newtypePipelineContext :: Newtype PipelineContext _
derive instance repGenericPipelineContext :: Generic PipelineContext _
instance showPipelineContext :: Show PipelineContext where
  show = genericShow
instance decodePipelineContext :: Decode PipelineContext where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineContext :: Encode PipelineContext where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the structure of actions and stages to be performed in the pipeline.</p>
newtype PipelineDeclaration = PipelineDeclaration 
  { "Name'" :: (PipelineName)
  , "RoleArn'" :: (RoleArn)
  , "ArtifactStore'" :: (ArtifactStore)
  , "Stages'" :: (PipelineStageDeclarationList)
  , "Version'" :: NullOrUndefined.NullOrUndefined (PipelineVersion)
  }
derive instance newtypePipelineDeclaration :: Newtype PipelineDeclaration _
derive instance repGenericPipelineDeclaration :: Generic PipelineDeclaration _
instance showPipelineDeclaration :: Show PipelineDeclaration where
  show = genericShow
instance decodePipelineDeclaration :: Decode PipelineDeclaration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineDeclaration :: Encode PipelineDeclaration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about an execution of a pipeline.</p>
newtype PipelineExecution = PipelineExecution 
  { "PipelineName'" :: NullOrUndefined.NullOrUndefined (PipelineName)
  , "PipelineVersion'" :: NullOrUndefined.NullOrUndefined (PipelineVersion)
  , "PipelineExecutionId'" :: NullOrUndefined.NullOrUndefined (PipelineExecutionId)
  , "Status'" :: NullOrUndefined.NullOrUndefined (PipelineExecutionStatus)
  , "ArtifactRevisions'" :: NullOrUndefined.NullOrUndefined (ArtifactRevisionList)
  }
derive instance newtypePipelineExecution :: Newtype PipelineExecution _
derive instance repGenericPipelineExecution :: Generic PipelineExecution _
instance showPipelineExecution :: Show PipelineExecution where
  show = genericShow
instance decodePipelineExecution :: Decode PipelineExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineExecution :: Encode PipelineExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineExecutionId = PipelineExecutionId String
derive instance newtypePipelineExecutionId :: Newtype PipelineExecutionId _
derive instance repGenericPipelineExecutionId :: Generic PipelineExecutionId _
instance showPipelineExecutionId :: Show PipelineExecutionId where
  show = genericShow
instance decodePipelineExecutionId :: Decode PipelineExecutionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineExecutionId :: Encode PipelineExecutionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pipeline execution was specified in an invalid format or cannot be found, or an execution ID does not belong to the specified pipeline. </p>
newtype PipelineExecutionNotFoundException = PipelineExecutionNotFoundException Types.NoArguments
derive instance newtypePipelineExecutionNotFoundException :: Newtype PipelineExecutionNotFoundException _
derive instance repGenericPipelineExecutionNotFoundException :: Generic PipelineExecutionNotFoundException _
instance showPipelineExecutionNotFoundException :: Show PipelineExecutionNotFoundException where
  show = genericShow
instance decodePipelineExecutionNotFoundException :: Decode PipelineExecutionNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineExecutionNotFoundException :: Encode PipelineExecutionNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineExecutionStatus = PipelineExecutionStatus String
derive instance newtypePipelineExecutionStatus :: Newtype PipelineExecutionStatus _
derive instance repGenericPipelineExecutionStatus :: Generic PipelineExecutionStatus _
instance showPipelineExecutionStatus :: Show PipelineExecutionStatus where
  show = genericShow
instance decodePipelineExecutionStatus :: Decode PipelineExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineExecutionStatus :: Encode PipelineExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Summary information about a pipeline execution.</p>
newtype PipelineExecutionSummary = PipelineExecutionSummary 
  { "PipelineExecutionId'" :: NullOrUndefined.NullOrUndefined (PipelineExecutionId)
  , "Status'" :: NullOrUndefined.NullOrUndefined (PipelineExecutionStatus)
  , "StartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastUpdateTime'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypePipelineExecutionSummary :: Newtype PipelineExecutionSummary _
derive instance repGenericPipelineExecutionSummary :: Generic PipelineExecutionSummary _
instance showPipelineExecutionSummary :: Show PipelineExecutionSummary where
  show = genericShow
instance decodePipelineExecutionSummary :: Decode PipelineExecutionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineExecutionSummary :: Encode PipelineExecutionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineExecutionSummaryList = PipelineExecutionSummaryList (Array PipelineExecutionSummary)
derive instance newtypePipelineExecutionSummaryList :: Newtype PipelineExecutionSummaryList _
derive instance repGenericPipelineExecutionSummaryList :: Generic PipelineExecutionSummaryList _
instance showPipelineExecutionSummaryList :: Show PipelineExecutionSummaryList where
  show = genericShow
instance decodePipelineExecutionSummaryList :: Decode PipelineExecutionSummaryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineExecutionSummaryList :: Encode PipelineExecutionSummaryList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineList = PipelineList (Array PipelineSummary)
derive instance newtypePipelineList :: Newtype PipelineList _
derive instance repGenericPipelineList :: Generic PipelineList _
instance showPipelineList :: Show PipelineList where
  show = genericShow
instance decodePipelineList :: Decode PipelineList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineList :: Encode PipelineList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a pipeline.</p>
newtype PipelineMetadata = PipelineMetadata 
  { "PipelineArn'" :: NullOrUndefined.NullOrUndefined (PipelineArn)
  , "Created'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypePipelineMetadata :: Newtype PipelineMetadata _
derive instance repGenericPipelineMetadata :: Generic PipelineMetadata _
instance showPipelineMetadata :: Show PipelineMetadata where
  show = genericShow
instance decodePipelineMetadata :: Decode PipelineMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineMetadata :: Encode PipelineMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineName = PipelineName String
derive instance newtypePipelineName :: Newtype PipelineName _
derive instance repGenericPipelineName :: Generic PipelineName _
instance showPipelineName :: Show PipelineName where
  show = genericShow
instance decodePipelineName :: Decode PipelineName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineName :: Encode PipelineName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified pipeline name is already in use.</p>
newtype PipelineNameInUseException = PipelineNameInUseException Types.NoArguments
derive instance newtypePipelineNameInUseException :: Newtype PipelineNameInUseException _
derive instance repGenericPipelineNameInUseException :: Generic PipelineNameInUseException _
instance showPipelineNameInUseException :: Show PipelineNameInUseException where
  show = genericShow
instance decodePipelineNameInUseException :: Decode PipelineNameInUseException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineNameInUseException :: Encode PipelineNameInUseException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified pipeline was specified in an invalid format or cannot be found.</p>
newtype PipelineNotFoundException = PipelineNotFoundException Types.NoArguments
derive instance newtypePipelineNotFoundException :: Newtype PipelineNotFoundException _
derive instance repGenericPipelineNotFoundException :: Generic PipelineNotFoundException _
instance showPipelineNotFoundException :: Show PipelineNotFoundException where
  show = genericShow
instance decodePipelineNotFoundException :: Decode PipelineNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineNotFoundException :: Encode PipelineNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineStageDeclarationList = PipelineStageDeclarationList (Array StageDeclaration)
derive instance newtypePipelineStageDeclarationList :: Newtype PipelineStageDeclarationList _
derive instance repGenericPipelineStageDeclarationList :: Generic PipelineStageDeclarationList _
instance showPipelineStageDeclarationList :: Show PipelineStageDeclarationList where
  show = genericShow
instance decodePipelineStageDeclarationList :: Decode PipelineStageDeclarationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineStageDeclarationList :: Encode PipelineStageDeclarationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Returns a summary of a pipeline.</p>
newtype PipelineSummary = PipelineSummary 
  { "Name'" :: NullOrUndefined.NullOrUndefined (PipelineName)
  , "Version'" :: NullOrUndefined.NullOrUndefined (PipelineVersion)
  , "Created'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypePipelineSummary :: Newtype PipelineSummary _
derive instance repGenericPipelineSummary :: Generic PipelineSummary _
instance showPipelineSummary :: Show PipelineSummary where
  show = genericShow
instance decodePipelineSummary :: Decode PipelineSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineSummary :: Encode PipelineSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PipelineVersion = PipelineVersion Int
derive instance newtypePipelineVersion :: Newtype PipelineVersion _
derive instance repGenericPipelineVersion :: Generic PipelineVersion _
instance showPipelineVersion :: Show PipelineVersion where
  show = genericShow
instance decodePipelineVersion :: Decode PipelineVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineVersion :: Encode PipelineVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified pipeline version was specified in an invalid format or cannot be found.</p>
newtype PipelineVersionNotFoundException = PipelineVersionNotFoundException Types.NoArguments
derive instance newtypePipelineVersionNotFoundException :: Newtype PipelineVersionNotFoundException _
derive instance repGenericPipelineVersionNotFoundException :: Generic PipelineVersionNotFoundException _
instance showPipelineVersionNotFoundException :: Show PipelineVersionNotFoundException where
  show = genericShow
instance decodePipelineVersionNotFoundException :: Decode PipelineVersionNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePipelineVersionNotFoundException :: Encode PipelineVersionNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a PollForJobs action.</p>
newtype PollForJobsInput = PollForJobsInput 
  { "ActionTypeId'" :: (ActionTypeId)
  , "MaxBatchSize'" :: NullOrUndefined.NullOrUndefined (MaxBatchSize)
  , "QueryParam'" :: NullOrUndefined.NullOrUndefined (QueryParamMap)
  }
derive instance newtypePollForJobsInput :: Newtype PollForJobsInput _
derive instance repGenericPollForJobsInput :: Generic PollForJobsInput _
instance showPollForJobsInput :: Show PollForJobsInput where
  show = genericShow
instance decodePollForJobsInput :: Decode PollForJobsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePollForJobsInput :: Encode PollForJobsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a PollForJobs action.</p>
newtype PollForJobsOutput = PollForJobsOutput 
  { "Jobs'" :: NullOrUndefined.NullOrUndefined (JobList)
  }
derive instance newtypePollForJobsOutput :: Newtype PollForJobsOutput _
derive instance repGenericPollForJobsOutput :: Generic PollForJobsOutput _
instance showPollForJobsOutput :: Show PollForJobsOutput where
  show = genericShow
instance decodePollForJobsOutput :: Decode PollForJobsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePollForJobsOutput :: Encode PollForJobsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a PollForThirdPartyJobs action.</p>
newtype PollForThirdPartyJobsInput = PollForThirdPartyJobsInput 
  { "ActionTypeId'" :: (ActionTypeId)
  , "MaxBatchSize'" :: NullOrUndefined.NullOrUndefined (MaxBatchSize)
  }
derive instance newtypePollForThirdPartyJobsInput :: Newtype PollForThirdPartyJobsInput _
derive instance repGenericPollForThirdPartyJobsInput :: Generic PollForThirdPartyJobsInput _
instance showPollForThirdPartyJobsInput :: Show PollForThirdPartyJobsInput where
  show = genericShow
instance decodePollForThirdPartyJobsInput :: Decode PollForThirdPartyJobsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePollForThirdPartyJobsInput :: Encode PollForThirdPartyJobsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a PollForThirdPartyJobs action.</p>
newtype PollForThirdPartyJobsOutput = PollForThirdPartyJobsOutput 
  { "Jobs'" :: NullOrUndefined.NullOrUndefined (ThirdPartyJobList)
  }
derive instance newtypePollForThirdPartyJobsOutput :: Newtype PollForThirdPartyJobsOutput _
derive instance repGenericPollForThirdPartyJobsOutput :: Generic PollForThirdPartyJobsOutput _
instance showPollForThirdPartyJobsOutput :: Show PollForThirdPartyJobsOutput where
  show = genericShow
instance decodePollForThirdPartyJobsOutput :: Decode PollForThirdPartyJobsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePollForThirdPartyJobsOutput :: Encode PollForThirdPartyJobsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a PutActionRevision action.</p>
newtype PutActionRevisionInput = PutActionRevisionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "ActionName'" :: (ActionName)
  , "ActionRevision'" :: (ActionRevision)
  }
derive instance newtypePutActionRevisionInput :: Newtype PutActionRevisionInput _
derive instance repGenericPutActionRevisionInput :: Generic PutActionRevisionInput _
instance showPutActionRevisionInput :: Show PutActionRevisionInput where
  show = genericShow
instance decodePutActionRevisionInput :: Decode PutActionRevisionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutActionRevisionInput :: Encode PutActionRevisionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a PutActionRevision action.</p>
newtype PutActionRevisionOutput = PutActionRevisionOutput 
  { "NewRevision'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "PipelineExecutionId'" :: NullOrUndefined.NullOrUndefined (PipelineExecutionId)
  }
derive instance newtypePutActionRevisionOutput :: Newtype PutActionRevisionOutput _
derive instance repGenericPutActionRevisionOutput :: Generic PutActionRevisionOutput _
instance showPutActionRevisionOutput :: Show PutActionRevisionOutput where
  show = genericShow
instance decodePutActionRevisionOutput :: Decode PutActionRevisionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutActionRevisionOutput :: Encode PutActionRevisionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a PutApprovalResult action.</p>
newtype PutApprovalResultInput = PutApprovalResultInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "ActionName'" :: (ActionName)
  , "Result'" :: (ApprovalResult)
  , "Token'" :: (ApprovalToken)
  }
derive instance newtypePutApprovalResultInput :: Newtype PutApprovalResultInput _
derive instance repGenericPutApprovalResultInput :: Generic PutApprovalResultInput _
instance showPutApprovalResultInput :: Show PutApprovalResultInput where
  show = genericShow
instance decodePutApprovalResultInput :: Decode PutApprovalResultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutApprovalResultInput :: Encode PutApprovalResultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a PutApprovalResult action.</p>
newtype PutApprovalResultOutput = PutApprovalResultOutput 
  { "ApprovedAt'" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypePutApprovalResultOutput :: Newtype PutApprovalResultOutput _
derive instance repGenericPutApprovalResultOutput :: Generic PutApprovalResultOutput _
instance showPutApprovalResultOutput :: Show PutApprovalResultOutput where
  show = genericShow
instance decodePutApprovalResultOutput :: Decode PutApprovalResultOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutApprovalResultOutput :: Encode PutApprovalResultOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a PutJobFailureResult action.</p>
newtype PutJobFailureResultInput = PutJobFailureResultInput 
  { "JobId'" :: (JobId)
  , "FailureDetails'" :: (FailureDetails)
  }
derive instance newtypePutJobFailureResultInput :: Newtype PutJobFailureResultInput _
derive instance repGenericPutJobFailureResultInput :: Generic PutJobFailureResultInput _
instance showPutJobFailureResultInput :: Show PutJobFailureResultInput where
  show = genericShow
instance decodePutJobFailureResultInput :: Decode PutJobFailureResultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutJobFailureResultInput :: Encode PutJobFailureResultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a PutJobSuccessResult action.</p>
newtype PutJobSuccessResultInput = PutJobSuccessResultInput 
  { "JobId'" :: (JobId)
  , "CurrentRevision'" :: NullOrUndefined.NullOrUndefined (CurrentRevision)
  , "ContinuationToken'" :: NullOrUndefined.NullOrUndefined (ContinuationToken)
  , "ExecutionDetails'" :: NullOrUndefined.NullOrUndefined (ExecutionDetails)
  }
derive instance newtypePutJobSuccessResultInput :: Newtype PutJobSuccessResultInput _
derive instance repGenericPutJobSuccessResultInput :: Generic PutJobSuccessResultInput _
instance showPutJobSuccessResultInput :: Show PutJobSuccessResultInput where
  show = genericShow
instance decodePutJobSuccessResultInput :: Decode PutJobSuccessResultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutJobSuccessResultInput :: Encode PutJobSuccessResultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a PutThirdPartyJobFailureResult action.</p>
newtype PutThirdPartyJobFailureResultInput = PutThirdPartyJobFailureResultInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  , "FailureDetails'" :: (FailureDetails)
  }
derive instance newtypePutThirdPartyJobFailureResultInput :: Newtype PutThirdPartyJobFailureResultInput _
derive instance repGenericPutThirdPartyJobFailureResultInput :: Generic PutThirdPartyJobFailureResultInput _
instance showPutThirdPartyJobFailureResultInput :: Show PutThirdPartyJobFailureResultInput where
  show = genericShow
instance decodePutThirdPartyJobFailureResultInput :: Decode PutThirdPartyJobFailureResultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutThirdPartyJobFailureResultInput :: Encode PutThirdPartyJobFailureResultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a PutThirdPartyJobSuccessResult action.</p>
newtype PutThirdPartyJobSuccessResultInput = PutThirdPartyJobSuccessResultInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  , "CurrentRevision'" :: NullOrUndefined.NullOrUndefined (CurrentRevision)
  , "ContinuationToken'" :: NullOrUndefined.NullOrUndefined (ContinuationToken)
  , "ExecutionDetails'" :: NullOrUndefined.NullOrUndefined (ExecutionDetails)
  }
derive instance newtypePutThirdPartyJobSuccessResultInput :: Newtype PutThirdPartyJobSuccessResultInput _
derive instance repGenericPutThirdPartyJobSuccessResultInput :: Generic PutThirdPartyJobSuccessResultInput _
instance showPutThirdPartyJobSuccessResultInput :: Show PutThirdPartyJobSuccessResultInput where
  show = genericShow
instance decodePutThirdPartyJobSuccessResultInput :: Decode PutThirdPartyJobSuccessResultInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePutThirdPartyJobSuccessResultInput :: Encode PutThirdPartyJobSuccessResultInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype QueryParamMap = QueryParamMap (StrMap.StrMap ActionConfigurationQueryableValue)
derive instance newtypeQueryParamMap :: Newtype QueryParamMap _
derive instance repGenericQueryParamMap :: Generic QueryParamMap _
instance showQueryParamMap :: Show QueryParamMap where
  show = genericShow
instance decodeQueryParamMap :: Decode QueryParamMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeQueryParamMap :: Encode QueryParamMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a RetryStageExecution action.</p>
newtype RetryStageExecutionInput = RetryStageExecutionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "PipelineExecutionId'" :: (PipelineExecutionId)
  , "RetryMode'" :: (StageRetryMode)
  }
derive instance newtypeRetryStageExecutionInput :: Newtype RetryStageExecutionInput _
derive instance repGenericRetryStageExecutionInput :: Generic RetryStageExecutionInput _
instance showRetryStageExecutionInput :: Show RetryStageExecutionInput where
  show = genericShow
instance decodeRetryStageExecutionInput :: Decode RetryStageExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetryStageExecutionInput :: Encode RetryStageExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a RetryStageExecution action.</p>
newtype RetryStageExecutionOutput = RetryStageExecutionOutput 
  { "PipelineExecutionId'" :: NullOrUndefined.NullOrUndefined (PipelineExecutionId)
  }
derive instance newtypeRetryStageExecutionOutput :: Newtype RetryStageExecutionOutput _
derive instance repGenericRetryStageExecutionOutput :: Generic RetryStageExecutionOutput _
instance showRetryStageExecutionOutput :: Show RetryStageExecutionOutput where
  show = genericShow
instance decodeRetryStageExecutionOutput :: Decode RetryStageExecutionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRetryStageExecutionOutput :: Encode RetryStageExecutionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Revision = Revision String
derive instance newtypeRevision :: Newtype Revision _
derive instance repGenericRevision :: Generic Revision _
instance showRevision :: Show Revision where
  show = genericShow
instance decodeRevision :: Decode Revision where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevision :: Encode Revision where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevisionChangeIdentifier = RevisionChangeIdentifier String
derive instance newtypeRevisionChangeIdentifier :: Newtype RevisionChangeIdentifier _
derive instance repGenericRevisionChangeIdentifier :: Generic RevisionChangeIdentifier _
instance showRevisionChangeIdentifier :: Show RevisionChangeIdentifier where
  show = genericShow
instance decodeRevisionChangeIdentifier :: Decode RevisionChangeIdentifier where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevisionChangeIdentifier :: Encode RevisionChangeIdentifier where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RevisionSummary = RevisionSummary String
derive instance newtypeRevisionSummary :: Newtype RevisionSummary _
derive instance repGenericRevisionSummary :: Generic RevisionSummary _
instance showRevisionSummary :: Show RevisionSummary where
  show = genericShow
instance decodeRevisionSummary :: Decode RevisionSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRevisionSummary :: Encode RevisionSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _
derive instance repGenericRoleArn :: Generic RoleArn _
instance showRoleArn :: Show RoleArn where
  show = genericShow
instance decodeRoleArn :: Decode RoleArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleArn :: Encode RoleArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The location of the Amazon S3 bucket that contains a revision.</p>
newtype S3ArtifactLocation = S3ArtifactLocation 
  { "BucketName'" :: (S3BucketName)
  , "ObjectKey'" :: (S3ObjectKey)
  }
derive instance newtypeS3ArtifactLocation :: Newtype S3ArtifactLocation _
derive instance repGenericS3ArtifactLocation :: Generic S3ArtifactLocation _
instance showS3ArtifactLocation :: Show S3ArtifactLocation where
  show = genericShow
instance decodeS3ArtifactLocation :: Decode S3ArtifactLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3ArtifactLocation :: Encode S3ArtifactLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3BucketName = S3BucketName String
derive instance newtypeS3BucketName :: Newtype S3BucketName _
derive instance repGenericS3BucketName :: Generic S3BucketName _
instance showS3BucketName :: Show S3BucketName where
  show = genericShow
instance decodeS3BucketName :: Decode S3BucketName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3BucketName :: Encode S3BucketName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype S3ObjectKey = S3ObjectKey String
derive instance newtypeS3ObjectKey :: Newtype S3ObjectKey _
derive instance repGenericS3ObjectKey :: Generic S3ObjectKey _
instance showS3ObjectKey :: Show S3ObjectKey where
  show = genericShow
instance decodeS3ObjectKey :: Decode S3ObjectKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeS3ObjectKey :: Encode S3ObjectKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecretAccessKey = SecretAccessKey String
derive instance newtypeSecretAccessKey :: Newtype SecretAccessKey _
derive instance repGenericSecretAccessKey :: Generic SecretAccessKey _
instance showSecretAccessKey :: Show SecretAccessKey where
  show = genericShow
instance decodeSecretAccessKey :: Decode SecretAccessKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecretAccessKey :: Encode SecretAccessKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SessionToken = SessionToken String
derive instance newtypeSessionToken :: Newtype SessionToken _
derive instance repGenericSessionToken :: Generic SessionToken _
instance showSessionToken :: Show SessionToken where
  show = genericShow
instance decodeSessionToken :: Decode SessionToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSessionToken :: Encode SessionToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageActionDeclarationList = StageActionDeclarationList (Array ActionDeclaration)
derive instance newtypeStageActionDeclarationList :: Newtype StageActionDeclarationList _
derive instance repGenericStageActionDeclarationList :: Generic StageActionDeclarationList _
instance showStageActionDeclarationList :: Show StageActionDeclarationList where
  show = genericShow
instance decodeStageActionDeclarationList :: Decode StageActionDeclarationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageActionDeclarationList :: Encode StageActionDeclarationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageBlockerDeclarationList = StageBlockerDeclarationList (Array BlockerDeclaration)
derive instance newtypeStageBlockerDeclarationList :: Newtype StageBlockerDeclarationList _
derive instance repGenericStageBlockerDeclarationList :: Generic StageBlockerDeclarationList _
instance showStageBlockerDeclarationList :: Show StageBlockerDeclarationList where
  show = genericShow
instance decodeStageBlockerDeclarationList :: Decode StageBlockerDeclarationList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageBlockerDeclarationList :: Encode StageBlockerDeclarationList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about a stage to a job worker.</p>
newtype StageContext = StageContext 
  { "Name'" :: NullOrUndefined.NullOrUndefined (StageName)
  }
derive instance newtypeStageContext :: Newtype StageContext _
derive instance repGenericStageContext :: Generic StageContext _
instance showStageContext :: Show StageContext where
  show = genericShow
instance decodeStageContext :: Decode StageContext where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageContext :: Encode StageContext where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about a stage and its definition.</p>
newtype StageDeclaration = StageDeclaration 
  { "Name'" :: (StageName)
  , "Blockers'" :: NullOrUndefined.NullOrUndefined (StageBlockerDeclarationList)
  , "Actions'" :: (StageActionDeclarationList)
  }
derive instance newtypeStageDeclaration :: Newtype StageDeclaration _
derive instance repGenericStageDeclaration :: Generic StageDeclaration _
instance showStageDeclaration :: Show StageDeclaration where
  show = genericShow
instance decodeStageDeclaration :: Decode StageDeclaration where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageDeclaration :: Encode StageDeclaration where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the run of a stage.</p>
newtype StageExecution = StageExecution 
  { "PipelineExecutionId'" :: (PipelineExecutionId)
  , "Status'" :: (StageExecutionStatus)
  }
derive instance newtypeStageExecution :: Newtype StageExecution _
derive instance repGenericStageExecution :: Generic StageExecution _
instance showStageExecution :: Show StageExecution where
  show = genericShow
instance decodeStageExecution :: Decode StageExecution where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageExecution :: Encode StageExecution where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageExecutionStatus = StageExecutionStatus String
derive instance newtypeStageExecutionStatus :: Newtype StageExecutionStatus _
derive instance repGenericStageExecutionStatus :: Generic StageExecutionStatus _
instance showStageExecutionStatus :: Show StageExecutionStatus where
  show = genericShow
instance decodeStageExecutionStatus :: Decode StageExecutionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageExecutionStatus :: Encode StageExecutionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageName = StageName String
derive instance newtypeStageName :: Newtype StageName _
derive instance repGenericStageName :: Generic StageName _
instance showStageName :: Show StageName where
  show = genericShow
instance decodeStageName :: Decode StageName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageName :: Encode StageName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified stage was specified in an invalid format or cannot be found.</p>
newtype StageNotFoundException = StageNotFoundException Types.NoArguments
derive instance newtypeStageNotFoundException :: Newtype StageNotFoundException _
derive instance repGenericStageNotFoundException :: Generic StageNotFoundException _
instance showStageNotFoundException :: Show StageNotFoundException where
  show = genericShow
instance decodeStageNotFoundException :: Decode StageNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageNotFoundException :: Encode StageNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified stage can't be retried because the pipeline structure or stage state changed after the stage was not completed; the stage contains no failed actions; one or more actions are still in progress; or another retry attempt is already in progress. </p>
newtype StageNotRetryableException = StageNotRetryableException Types.NoArguments
derive instance newtypeStageNotRetryableException :: Newtype StageNotRetryableException _
derive instance repGenericStageNotRetryableException :: Generic StageNotRetryableException _
instance showStageNotRetryableException :: Show StageNotRetryableException where
  show = genericShow
instance decodeStageNotRetryableException :: Decode StageNotRetryableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageNotRetryableException :: Encode StageNotRetryableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageRetryMode = StageRetryMode String
derive instance newtypeStageRetryMode :: Newtype StageRetryMode _
derive instance repGenericStageRetryMode :: Generic StageRetryMode _
instance showStageRetryMode :: Show StageRetryMode where
  show = genericShow
instance decodeStageRetryMode :: Decode StageRetryMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageRetryMode :: Encode StageRetryMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the state of the stage.</p>
newtype StageState = StageState 
  { "StageName'" :: NullOrUndefined.NullOrUndefined (StageName)
  , "InboundTransitionState'" :: NullOrUndefined.NullOrUndefined (TransitionState)
  , "ActionStates'" :: NullOrUndefined.NullOrUndefined (ActionStateList)
  , "LatestExecution'" :: NullOrUndefined.NullOrUndefined (StageExecution)
  }
derive instance newtypeStageState :: Newtype StageState _
derive instance repGenericStageState :: Generic StageState _
instance showStageState :: Show StageState where
  show = genericShow
instance decodeStageState :: Decode StageState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageState :: Encode StageState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageStateList = StageStateList (Array StageState)
derive instance newtypeStageStateList :: Newtype StageStateList _
derive instance repGenericStageStateList :: Generic StageStateList _
instance showStageStateList :: Show StageStateList where
  show = genericShow
instance decodeStageStateList :: Decode StageStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageStateList :: Encode StageStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageTransitionType = StageTransitionType String
derive instance newtypeStageTransitionType :: Newtype StageTransitionType _
derive instance repGenericStageTransitionType :: Generic StageTransitionType _
instance showStageTransitionType :: Show StageTransitionType where
  show = genericShow
instance decodeStageTransitionType :: Decode StageTransitionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageTransitionType :: Encode StageTransitionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of a StartPipelineExecution action.</p>
newtype StartPipelineExecutionInput = StartPipelineExecutionInput 
  { "Name'" :: (PipelineName)
  }
derive instance newtypeStartPipelineExecutionInput :: Newtype StartPipelineExecutionInput _
derive instance repGenericStartPipelineExecutionInput :: Generic StartPipelineExecutionInput _
instance showStartPipelineExecutionInput :: Show StartPipelineExecutionInput where
  show = genericShow
instance decodeStartPipelineExecutionInput :: Decode StartPipelineExecutionInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartPipelineExecutionInput :: Encode StartPipelineExecutionInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of a StartPipelineExecution action.</p>
newtype StartPipelineExecutionOutput = StartPipelineExecutionOutput 
  { "PipelineExecutionId'" :: NullOrUndefined.NullOrUndefined (PipelineExecutionId)
  }
derive instance newtypeStartPipelineExecutionOutput :: Newtype StartPipelineExecutionOutput _
derive instance repGenericStartPipelineExecutionOutput :: Generic StartPipelineExecutionOutput _
instance showStartPipelineExecutionOutput :: Show StartPipelineExecutionOutput where
  show = genericShow
instance decodeStartPipelineExecutionOutput :: Decode StartPipelineExecutionOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartPipelineExecutionOutput :: Encode StartPipelineExecutionOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A response to a PollForThirdPartyJobs request returned by AWS CodePipeline when there is a job to be worked upon by a partner action.</p>
newtype ThirdPartyJob = ThirdPartyJob 
  { "ClientId'" :: NullOrUndefined.NullOrUndefined (ClientId)
  , "JobId'" :: NullOrUndefined.NullOrUndefined (JobId)
  }
derive instance newtypeThirdPartyJob :: Newtype ThirdPartyJob _
derive instance repGenericThirdPartyJob :: Generic ThirdPartyJob _
instance showThirdPartyJob :: Show ThirdPartyJob where
  show = genericShow
instance decodeThirdPartyJob :: Decode ThirdPartyJob where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThirdPartyJob :: Encode ThirdPartyJob where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the job data for a partner action.</p>
newtype ThirdPartyJobData = ThirdPartyJobData 
  { "ActionTypeId'" :: NullOrUndefined.NullOrUndefined (ActionTypeId)
  , "ActionConfiguration'" :: NullOrUndefined.NullOrUndefined (ActionConfiguration)
  , "PipelineContext'" :: NullOrUndefined.NullOrUndefined (PipelineContext)
  , "InputArtifacts'" :: NullOrUndefined.NullOrUndefined (ArtifactList)
  , "OutputArtifacts'" :: NullOrUndefined.NullOrUndefined (ArtifactList)
  , "ArtifactCredentials'" :: NullOrUndefined.NullOrUndefined (AWSSessionCredentials)
  , "ContinuationToken'" :: NullOrUndefined.NullOrUndefined (ContinuationToken)
  , "EncryptionKey'" :: NullOrUndefined.NullOrUndefined (EncryptionKey)
  }
derive instance newtypeThirdPartyJobData :: Newtype ThirdPartyJobData _
derive instance repGenericThirdPartyJobData :: Generic ThirdPartyJobData _
instance showThirdPartyJobData :: Show ThirdPartyJobData where
  show = genericShow
instance decodeThirdPartyJobData :: Decode ThirdPartyJobData where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThirdPartyJobData :: Encode ThirdPartyJobData where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The details of a job sent in response to a GetThirdPartyJobDetails request.</p>
newtype ThirdPartyJobDetails = ThirdPartyJobDetails 
  { "Id'" :: NullOrUndefined.NullOrUndefined (ThirdPartyJobId)
  , "Data'" :: NullOrUndefined.NullOrUndefined (ThirdPartyJobData)
  , "Nonce'" :: NullOrUndefined.NullOrUndefined (Nonce)
  }
derive instance newtypeThirdPartyJobDetails :: Newtype ThirdPartyJobDetails _
derive instance repGenericThirdPartyJobDetails :: Generic ThirdPartyJobDetails _
instance showThirdPartyJobDetails :: Show ThirdPartyJobDetails where
  show = genericShow
instance decodeThirdPartyJobDetails :: Decode ThirdPartyJobDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThirdPartyJobDetails :: Encode ThirdPartyJobDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThirdPartyJobId = ThirdPartyJobId String
derive instance newtypeThirdPartyJobId :: Newtype ThirdPartyJobId _
derive instance repGenericThirdPartyJobId :: Generic ThirdPartyJobId _
instance showThirdPartyJobId :: Show ThirdPartyJobId where
  show = genericShow
instance decodeThirdPartyJobId :: Decode ThirdPartyJobId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThirdPartyJobId :: Encode ThirdPartyJobId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ThirdPartyJobList = ThirdPartyJobList (Array ThirdPartyJob)
derive instance newtypeThirdPartyJobList :: Newtype ThirdPartyJobList _
derive instance repGenericThirdPartyJobList :: Generic ThirdPartyJobList _
instance showThirdPartyJobList :: Show ThirdPartyJobList where
  show = genericShow
instance decodeThirdPartyJobList :: Decode ThirdPartyJobList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeThirdPartyJobList :: Encode ThirdPartyJobList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Time = Time Number
derive instance newtypeTime :: Newtype Time _
derive instance repGenericTime :: Generic Time _
instance showTime :: Show Time where
  show = genericShow
instance decodeTime :: Decode Time where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTime :: Encode Time where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents information about the state of transitions between one stage and another stage.</p>
newtype TransitionState = TransitionState 
  { "Enabled'" :: NullOrUndefined.NullOrUndefined (Enabled)
  , "LastChangedBy'" :: NullOrUndefined.NullOrUndefined (LastChangedBy)
  , "LastChangedAt'" :: NullOrUndefined.NullOrUndefined (LastChangedAt)
  , "DisabledReason'" :: NullOrUndefined.NullOrUndefined (DisabledReason)
  }
derive instance newtypeTransitionState :: Newtype TransitionState _
derive instance repGenericTransitionState :: Generic TransitionState _
instance showTransitionState :: Show TransitionState where
  show = genericShow
instance decodeTransitionState :: Decode TransitionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTransitionState :: Encode TransitionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the input of an UpdatePipeline action.</p>
newtype UpdatePipelineInput = UpdatePipelineInput 
  { "Pipeline'" :: (PipelineDeclaration)
  }
derive instance newtypeUpdatePipelineInput :: Newtype UpdatePipelineInput _
derive instance repGenericUpdatePipelineInput :: Generic UpdatePipelineInput _
instance showUpdatePipelineInput :: Show UpdatePipelineInput where
  show = genericShow
instance decodeUpdatePipelineInput :: Decode UpdatePipelineInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePipelineInput :: Encode UpdatePipelineInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents the output of an UpdatePipeline action.</p>
newtype UpdatePipelineOutput = UpdatePipelineOutput 
  { "Pipeline'" :: NullOrUndefined.NullOrUndefined (PipelineDeclaration)
  }
derive instance newtypeUpdatePipelineOutput :: Newtype UpdatePipelineOutput _
derive instance repGenericUpdatePipelineOutput :: Generic UpdatePipelineOutput _
instance showUpdatePipelineOutput :: Show UpdatePipelineOutput where
  show = genericShow
instance decodeUpdatePipelineOutput :: Decode UpdatePipelineOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdatePipelineOutput :: Encode UpdatePipelineOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _
derive instance repGenericUrl :: Generic Url _
instance showUrl :: Show Url where
  show = genericShow
instance decodeUrl :: Decode Url where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUrl :: Encode Url where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UrlTemplate = UrlTemplate String
derive instance newtypeUrlTemplate :: Newtype UrlTemplate _
derive instance repGenericUrlTemplate :: Generic UrlTemplate _
instance showUrlTemplate :: Show UrlTemplate where
  show = genericShow
instance decodeUrlTemplate :: Decode UrlTemplate where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUrlTemplate :: Encode UrlTemplate where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The validation was specified in an invalid format.</p>
newtype ValidationException = ValidationException Types.NoArguments
derive instance newtypeValidationException :: Newtype ValidationException _
derive instance repGenericValidationException :: Generic ValidationException _
instance showValidationException :: Show ValidationException where
  show = genericShow
instance decodeValidationException :: Decode ValidationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValidationException :: Encode ValidationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
derive instance repGenericVersion :: Generic Version _
instance showVersion :: Show Version where
  show = genericShow
instance decodeVersion :: Decode Version where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVersion :: Encode Version where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
