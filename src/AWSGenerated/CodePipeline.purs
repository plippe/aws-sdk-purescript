

-- | <fullname>AWS CodePipeline</fullname> <p> <b>Overview</b> </p> <p>This is the AWS CodePipeline API Reference. This guide provides descriptions of the actions and data types for AWS CodePipeline. Some functionality for your pipeline is only configurable through the API. For additional information, see the <a href="http://docs.aws.amazon.com/codepipeline/latest/userguide/welcome.html">AWS CodePipeline User Guide</a>.</p> <p>You can use the AWS CodePipeline API to work with pipelines, stages, actions, gates, and transitions, as described below.</p> <p> <i>Pipelines</i> are models of automated release processes. Each pipeline is uniquely named, and consists of actions, gates, and stages. </p> <p>You can work with pipelines by calling:</p> <ul> <li> <p> <a>CreatePipeline</a>, which creates a uniquely-named pipeline.</p> </li> <li> <p> <a>DeletePipeline</a>, which deletes the specified pipeline.</p> </li> <li> <p> <a>GetPipeline</a>, which returns information about the pipeline structure and pipeline metadata, including the pipeline Amazon Resource Name (ARN).</p> </li> <li> <p> <a>GetPipelineExecution</a>, which returns information about a specific execution of a pipeline.</p> </li> <li> <p> <a>GetPipelineState</a>, which returns information about the current state of the stages and actions of a pipeline.</p> </li> <li> <p> <a>ListPipelines</a>, which gets a summary of all of the pipelines associated with your account.</p> </li> <li> <p> <a>ListPipelineExecutions</a>, which gets a summary of the most recent executions for a pipeline.</p> </li> <li> <p> <a>StartPipelineExecution</a>, which runs the the most recent revision of an artifact through the pipeline.</p> </li> <li> <p> <a>UpdatePipeline</a>, which updates a pipeline with edits or changes to the structure of the pipeline.</p> </li> </ul> <p>Pipelines include <i>stages</i>, which are logical groupings of gates and actions. Each stage contains one or more actions that must complete before the next stage begins. A stage will result in success or failure. If a stage fails, then the pipeline stops at that stage and will remain stopped until either a new version of an artifact appears in the source location, or a user takes action to re-run the most recent artifact through the pipeline. You can call <a>GetPipelineState</a>, which displays the status of a pipeline, including the status of stages in the pipeline, or <a>GetPipeline</a>, which returns the entire structure of the pipeline, including the stages of that pipeline. For more information about the structure of stages and actions, also refer to the <a href="http://docs.aws.amazon.com/codepipeline/latest/userguide/pipeline-structure.html">AWS CodePipeline Pipeline Structure Reference</a>.</p> <p>Pipeline stages include <i>actions</i>, which are categorized into categories such as source or build actions performed within a stage of a pipeline. For example, you can use a source action to import artifacts into a pipeline from a source such as Amazon S3. Like stages, you do not work with actions directly in most cases, but you do define and interact with actions when working with pipeline operations such as <a>CreatePipeline</a> and <a>GetPipelineState</a>. </p> <p>Pipelines also include <i>transitions</i>, which allow the transition of artifacts from one stage to the next in a pipeline after the actions in one stage complete.</p> <p>You can work with transitions by calling:</p> <ul> <li> <p> <a>DisableStageTransition</a>, which prevents artifacts from transitioning to the next stage in a pipeline.</p> </li> <li> <p> <a>EnableStageTransition</a>, which enables transition of artifacts between stages in a pipeline. </p> </li> </ul> <p> <b>Using the API to integrate with AWS CodePipeline</b> </p> <p>For third-party integrators or developers who want to create their own integrations with AWS CodePipeline, the expected sequence varies from the standard API user. In order to integrate with AWS CodePipeline, developers will need to work with the following items:</p> <p> <b>Jobs</b>, which are instances of an action. For example, a job for a source action might import a revision of an artifact from a source. </p> <p>You can work with jobs by calling:</p> <ul> <li> <p> <a>AcknowledgeJob</a>, which confirms whether a job worker has received the specified job,</p> </li> <li> <p> <a>GetJobDetails</a>, which returns the details of a job,</p> </li> <li> <p> <a>PollForJobs</a>, which determines whether there are any jobs to act upon, </p> </li> <li> <p> <a>PutJobFailureResult</a>, which provides details of a job failure, and</p> </li> <li> <p> <a>PutJobSuccessResult</a>, which provides details of a job success.</p> </li> </ul> <p> <b>Third party jobs</b>, which are instances of an action created by a partner action and integrated into AWS CodePipeline. Partner actions are created by members of the AWS Partner Network.</p> <p>You can work with third party jobs by calling:</p> <ul> <li> <p> <a>AcknowledgeThirdPartyJob</a>, which confirms whether a job worker has received the specified job,</p> </li> <li> <p> <a>GetThirdPartyJobDetails</a>, which requests the details of a job for a partner action,</p> </li> <li> <p> <a>PollForThirdPartyJobs</a>, which determines whether there are any jobs to act upon, </p> </li> <li> <p> <a>PutThirdPartyJobFailureResult</a>, which provides details of a job failure, and</p> </li> <li> <p> <a>PutThirdPartyJobSuccessResult</a>, which provides details of a job success.</p> </li> </ul>
module AWS.CodePipeline where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CodePipeline" :: String


-- | <p>Returns information about a specified job and whether that job has been received by the job worker. Only used for custom actions.</p>
acknowledgeJob :: forall eff. AcknowledgeJobInput -> Aff (err :: AWS.RequestError | eff) AcknowledgeJobOutput
acknowledgeJob = AWS.request serviceName "acknowledgeJob" 


-- | <p>Confirms a job worker has received the specified job. Only used for partner actions.</p>
acknowledgeThirdPartyJob :: forall eff. AcknowledgeThirdPartyJobInput -> Aff (err :: AWS.RequestError | eff) AcknowledgeThirdPartyJobOutput
acknowledgeThirdPartyJob = AWS.request serviceName "acknowledgeThirdPartyJob" 


-- | <p>Creates a new custom action that can be used in all pipelines associated with the AWS account. Only used for custom actions.</p>
createCustomActionType :: forall eff. CreateCustomActionTypeInput -> Aff (err :: AWS.RequestError | eff) CreateCustomActionTypeOutput
createCustomActionType = AWS.request serviceName "createCustomActionType" 


-- | <p>Creates a pipeline.</p>
createPipeline :: forall eff. CreatePipelineInput -> Aff (err :: AWS.RequestError | eff) CreatePipelineOutput
createPipeline = AWS.request serviceName "createPipeline" 


-- | <p>Marks a custom action as deleted. PollForJobs for the custom action will fail after the action is marked for deletion. Only used for custom actions.</p> <important> <p>You cannot recreate a custom action after it has been deleted unless you increase the version number of the action.</p> </important>
deleteCustomActionType :: forall eff. DeleteCustomActionTypeInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteCustomActionType = AWS.request serviceName "deleteCustomActionType" 


-- | <p>Deletes the specified pipeline.</p>
deletePipeline :: forall eff. DeletePipelineInput -> Aff (err :: AWS.RequestError | eff) Unit
deletePipeline = AWS.request serviceName "deletePipeline" 


-- | <p>Prevents artifacts in a pipeline from transitioning to the next stage in the pipeline.</p>
disableStageTransition :: forall eff. DisableStageTransitionInput -> Aff (err :: AWS.RequestError | eff) Unit
disableStageTransition = AWS.request serviceName "disableStageTransition" 


-- | <p>Enables artifacts in a pipeline to transition to a stage in a pipeline.</p>
enableStageTransition :: forall eff. EnableStageTransitionInput -> Aff (err :: AWS.RequestError | eff) Unit
enableStageTransition = AWS.request serviceName "enableStageTransition" 


-- | <p>Returns information about a job. Only used for custom actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
getJobDetails :: forall eff. GetJobDetailsInput -> Aff (err :: AWS.RequestError | eff) GetJobDetailsOutput
getJobDetails = AWS.request serviceName "getJobDetails" 


-- | <p>Returns the metadata, structure, stages, and actions of a pipeline. Can be used to return the entire structure of a pipeline in JSON format, which can then be modified and used to update the pipeline structure with <a>UpdatePipeline</a>.</p>
getPipeline :: forall eff. GetPipelineInput -> Aff (err :: AWS.RequestError | eff) GetPipelineOutput
getPipeline = AWS.request serviceName "getPipeline" 


-- | <p>Returns information about an execution of a pipeline, including details about artifacts, the pipeline execution ID, and the name, version, and status of the pipeline.</p>
getPipelineExecution :: forall eff. GetPipelineExecutionInput -> Aff (err :: AWS.RequestError | eff) GetPipelineExecutionOutput
getPipelineExecution = AWS.request serviceName "getPipelineExecution" 


-- | <p>Returns information about the state of a pipeline, including the stages and actions.</p>
getPipelineState :: forall eff. GetPipelineStateInput -> Aff (err :: AWS.RequestError | eff) GetPipelineStateOutput
getPipelineState = AWS.request serviceName "getPipelineState" 


-- | <p>Requests the details of a job for a third party action. Only used for partner actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
getThirdPartyJobDetails :: forall eff. GetThirdPartyJobDetailsInput -> Aff (err :: AWS.RequestError | eff) GetThirdPartyJobDetailsOutput
getThirdPartyJobDetails = AWS.request serviceName "getThirdPartyJobDetails" 


-- | <p>Gets a summary of all AWS CodePipeline action types associated with your account.</p>
listActionTypes :: forall eff. ListActionTypesInput -> Aff (err :: AWS.RequestError | eff) ListActionTypesOutput
listActionTypes = AWS.request serviceName "listActionTypes" 


-- | <p>Gets a summary of the most recent executions for a pipeline.</p>
listPipelineExecutions :: forall eff. ListPipelineExecutionsInput -> Aff (err :: AWS.RequestError | eff) ListPipelineExecutionsOutput
listPipelineExecutions = AWS.request serviceName "listPipelineExecutions" 


-- | <p>Gets a summary of all of the pipelines associated with your account.</p>
listPipelines :: forall eff. ListPipelinesInput -> Aff (err :: AWS.RequestError | eff) ListPipelinesOutput
listPipelines = AWS.request serviceName "listPipelines" 


-- | <p>Returns information about any jobs for AWS CodePipeline to act upon.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
pollForJobs :: forall eff. PollForJobsInput -> Aff (err :: AWS.RequestError | eff) PollForJobsOutput
pollForJobs = AWS.request serviceName "pollForJobs" 


-- | <p>Determines whether there are any third party jobs for a job worker to act on. Only used for partner actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts.</p> </important>
pollForThirdPartyJobs :: forall eff. PollForThirdPartyJobsInput -> Aff (err :: AWS.RequestError | eff) PollForThirdPartyJobsOutput
pollForThirdPartyJobs = AWS.request serviceName "pollForThirdPartyJobs" 


-- | <p>Provides information to AWS CodePipeline about new revisions to a source.</p>
putActionRevision :: forall eff. PutActionRevisionInput -> Aff (err :: AWS.RequestError | eff) PutActionRevisionOutput
putActionRevision = AWS.request serviceName "putActionRevision" 


-- | <p>Provides the response to a manual approval request to AWS CodePipeline. Valid responses include Approved and Rejected.</p>
putApprovalResult :: forall eff. PutApprovalResultInput -> Aff (err :: AWS.RequestError | eff) PutApprovalResultOutput
putApprovalResult = AWS.request serviceName "putApprovalResult" 


-- | <p>Represents the failure of a job as returned to the pipeline by a job worker. Only used for custom actions.</p>
putJobFailureResult :: forall eff. PutJobFailureResultInput -> Aff (err :: AWS.RequestError | eff) Unit
putJobFailureResult = AWS.request serviceName "putJobFailureResult" 


-- | <p>Represents the success of a job as returned to the pipeline by a job worker. Only used for custom actions.</p>
putJobSuccessResult :: forall eff. PutJobSuccessResultInput -> Aff (err :: AWS.RequestError | eff) Unit
putJobSuccessResult = AWS.request serviceName "putJobSuccessResult" 


-- | <p>Represents the failure of a third party job as returned to the pipeline by a job worker. Only used for partner actions.</p>
putThirdPartyJobFailureResult :: forall eff. PutThirdPartyJobFailureResultInput -> Aff (err :: AWS.RequestError | eff) Unit
putThirdPartyJobFailureResult = AWS.request serviceName "putThirdPartyJobFailureResult" 


-- | <p>Represents the success of a third party job as returned to the pipeline by a job worker. Only used for partner actions.</p>
putThirdPartyJobSuccessResult :: forall eff. PutThirdPartyJobSuccessResultInput -> Aff (err :: AWS.RequestError | eff) Unit
putThirdPartyJobSuccessResult = AWS.request serviceName "putThirdPartyJobSuccessResult" 


-- | <p>Resumes the pipeline execution by retrying the last failed actions in a stage.</p>
retryStageExecution :: forall eff. RetryStageExecutionInput -> Aff (err :: AWS.RequestError | eff) RetryStageExecutionOutput
retryStageExecution = AWS.request serviceName "retryStageExecution" 


-- | <p>Starts the specified pipeline. Specifically, it begins processing the latest commit to the source location specified as part of the pipeline.</p>
startPipelineExecution :: forall eff. StartPipelineExecutionInput -> Aff (err :: AWS.RequestError | eff) StartPipelineExecutionOutput
startPipelineExecution = AWS.request serviceName "startPipelineExecution" 


-- | <p>Updates a specified pipeline with edits or changes to its structure. Use a JSON file with the pipeline structure in conjunction with UpdatePipeline to provide the full structure of the pipeline. Updating the pipeline increases the version number of the pipeline by 1.</p>
updatePipeline :: forall eff. UpdatePipelineInput -> Aff (err :: AWS.RequestError | eff) UpdatePipelineOutput
updatePipeline = AWS.request serviceName "updatePipeline" 


-- | <p>Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the Amazon S3 bucket used to store artifact for the pipeline in AWS CodePipeline.</p>
newtype AWSSessionCredentials = AWSSessionCredentials 
  { "AccessKeyId'" :: (AccessKeyId)
  , "SecretAccessKey'" :: (SecretAccessKey)
  , "SessionToken'" :: (SessionToken)
  }
derive instance newtypeAWSSessionCredentials :: Newtype AWSSessionCredentials _


newtype AccessKeyId = AccessKeyId String
derive instance newtypeAccessKeyId :: Newtype AccessKeyId _


newtype AccountId = AccountId String
derive instance newtypeAccountId :: Newtype AccountId _


-- | <p>Represents the input of an AcknowledgeJob action.</p>
newtype AcknowledgeJobInput = AcknowledgeJobInput 
  { "JobId'" :: (JobId)
  , "Nonce'" :: (Nonce)
  }
derive instance newtypeAcknowledgeJobInput :: Newtype AcknowledgeJobInput _


-- | <p>Represents the output of an AcknowledgeJob action.</p>
newtype AcknowledgeJobOutput = AcknowledgeJobOutput 
  { "Status'" :: NullOrUndefined (JobStatus)
  }
derive instance newtypeAcknowledgeJobOutput :: Newtype AcknowledgeJobOutput _


-- | <p>Represents the input of an AcknowledgeThirdPartyJob action.</p>
newtype AcknowledgeThirdPartyJobInput = AcknowledgeThirdPartyJobInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "Nonce'" :: (Nonce)
  , "ClientToken'" :: (ClientToken)
  }
derive instance newtypeAcknowledgeThirdPartyJobInput :: Newtype AcknowledgeThirdPartyJobInput _


-- | <p>Represents the output of an AcknowledgeThirdPartyJob action.</p>
newtype AcknowledgeThirdPartyJobOutput = AcknowledgeThirdPartyJobOutput 
  { "Status'" :: NullOrUndefined (JobStatus)
  }
derive instance newtypeAcknowledgeThirdPartyJobOutput :: Newtype AcknowledgeThirdPartyJobOutput _


newtype ActionCategory = ActionCategory String
derive instance newtypeActionCategory :: Newtype ActionCategory _


-- | <p>Represents information about an action configuration.</p>
newtype ActionConfiguration = ActionConfiguration 
  { "Configuration'" :: NullOrUndefined (ActionConfigurationMap)
  }
derive instance newtypeActionConfiguration :: Newtype ActionConfiguration _


newtype ActionConfigurationKey = ActionConfigurationKey String
derive instance newtypeActionConfigurationKey :: Newtype ActionConfigurationKey _


newtype ActionConfigurationMap = ActionConfigurationMap (Map ActionConfigurationKey ActionConfigurationValue)
derive instance newtypeActionConfigurationMap :: Newtype ActionConfigurationMap _


-- | <p>Represents information about an action configuration property.</p>
newtype ActionConfigurationProperty = ActionConfigurationProperty 
  { "Name'" :: (ActionConfigurationKey)
  , "Required'" :: (Boolean)
  , "Key'" :: (Boolean)
  , "Secret'" :: (Boolean)
  , "Queryable'" :: NullOrUndefined (Boolean)
  , "Description'" :: NullOrUndefined (Description)
  , "Type'" :: NullOrUndefined (ActionConfigurationPropertyType)
  }
derive instance newtypeActionConfigurationProperty :: Newtype ActionConfigurationProperty _


newtype ActionConfigurationPropertyList = ActionConfigurationPropertyList (Array ActionConfigurationProperty)
derive instance newtypeActionConfigurationPropertyList :: Newtype ActionConfigurationPropertyList _


newtype ActionConfigurationPropertyType = ActionConfigurationPropertyType String
derive instance newtypeActionConfigurationPropertyType :: Newtype ActionConfigurationPropertyType _


newtype ActionConfigurationQueryableValue = ActionConfigurationQueryableValue String
derive instance newtypeActionConfigurationQueryableValue :: Newtype ActionConfigurationQueryableValue _


newtype ActionConfigurationValue = ActionConfigurationValue String
derive instance newtypeActionConfigurationValue :: Newtype ActionConfigurationValue _


-- | <p>Represents the context of an action within the stage of a pipeline to a job worker.</p>
newtype ActionContext = ActionContext 
  { "Name'" :: NullOrUndefined (ActionName)
  }
derive instance newtypeActionContext :: Newtype ActionContext _


-- | <p>Represents information about an action declaration.</p>
newtype ActionDeclaration = ActionDeclaration 
  { "Name'" :: (ActionName)
  , "ActionTypeId'" :: (ActionTypeId)
  , "RunOrder'" :: NullOrUndefined (ActionRunOrder)
  , "Configuration'" :: NullOrUndefined (ActionConfigurationMap)
  , "OutputArtifacts'" :: NullOrUndefined (OutputArtifactList)
  , "InputArtifacts'" :: NullOrUndefined (InputArtifactList)
  , "RoleArn'" :: NullOrUndefined (RoleArn)
  }
derive instance newtypeActionDeclaration :: Newtype ActionDeclaration _


-- | <p>Represents information about the run of an action.</p>
newtype ActionExecution = ActionExecution 
  { "Status'" :: NullOrUndefined (ActionExecutionStatus)
  , "Summary'" :: NullOrUndefined (ExecutionSummary)
  , "LastStatusChange'" :: NullOrUndefined (Number)
  , "Token'" :: NullOrUndefined (ActionExecutionToken)
  , "LastUpdatedBy'" :: NullOrUndefined (LastUpdatedBy)
  , "ExternalExecutionId'" :: NullOrUndefined (ExecutionId)
  , "ExternalExecutionUrl'" :: NullOrUndefined (Url)
  , "PercentComplete'" :: NullOrUndefined (Percentage)
  , "ErrorDetails'" :: NullOrUndefined (ErrorDetails)
  }
derive instance newtypeActionExecution :: Newtype ActionExecution _


newtype ActionExecutionStatus = ActionExecutionStatus String
derive instance newtypeActionExecutionStatus :: Newtype ActionExecutionStatus _


newtype ActionExecutionToken = ActionExecutionToken String
derive instance newtypeActionExecutionToken :: Newtype ActionExecutionToken _


newtype ActionName = ActionName String
derive instance newtypeActionName :: Newtype ActionName _


-- | <p>The specified action cannot be found.</p>
newtype ActionNotFoundException = ActionNotFoundException 
  { 
  }
derive instance newtypeActionNotFoundException :: Newtype ActionNotFoundException _


newtype ActionOwner = ActionOwner String
derive instance newtypeActionOwner :: Newtype ActionOwner _


newtype ActionProvider = ActionProvider String
derive instance newtypeActionProvider :: Newtype ActionProvider _


-- | <p>Represents information about the version (or revision) of an action.</p>
newtype ActionRevision = ActionRevision 
  { "RevisionId'" :: (Revision)
  , "RevisionChangeId'" :: (RevisionChangeIdentifier)
  , "Created'" :: (Number)
  }
derive instance newtypeActionRevision :: Newtype ActionRevision _


newtype ActionRunOrder = ActionRunOrder Int
derive instance newtypeActionRunOrder :: Newtype ActionRunOrder _


-- | <p>Represents information about the state of an action.</p>
newtype ActionState = ActionState 
  { "ActionName'" :: NullOrUndefined (ActionName)
  , "CurrentRevision'" :: NullOrUndefined (ActionRevision)
  , "LatestExecution'" :: NullOrUndefined (ActionExecution)
  , "EntityUrl'" :: NullOrUndefined (Url)
  , "RevisionUrl'" :: NullOrUndefined (Url)
  }
derive instance newtypeActionState :: Newtype ActionState _


newtype ActionStateList = ActionStateList (Array ActionState)
derive instance newtypeActionStateList :: Newtype ActionStateList _


-- | <p>Returns information about the details of an action type.</p>
newtype ActionType = ActionType 
  { "Id'" :: (ActionTypeId)
  , "Settings'" :: NullOrUndefined (ActionTypeSettings)
  , "ActionConfigurationProperties'" :: NullOrUndefined (ActionConfigurationPropertyList)
  , "InputArtifactDetails'" :: (ArtifactDetails)
  , "OutputArtifactDetails'" :: (ArtifactDetails)
  }
derive instance newtypeActionType :: Newtype ActionType _


-- | <p>Represents information about an action type.</p>
newtype ActionTypeId = ActionTypeId 
  { "Category'" :: (ActionCategory)
  , "Owner'" :: (ActionOwner)
  , "Provider'" :: (ActionProvider)
  , "Version'" :: (Version)
  }
derive instance newtypeActionTypeId :: Newtype ActionTypeId _


newtype ActionTypeList = ActionTypeList (Array ActionType)
derive instance newtypeActionTypeList :: Newtype ActionTypeList _


-- | <p>The specified action type cannot be found.</p>
newtype ActionTypeNotFoundException = ActionTypeNotFoundException 
  { 
  }
derive instance newtypeActionTypeNotFoundException :: Newtype ActionTypeNotFoundException _


-- | <p>Returns information about the settings for an action type.</p>
newtype ActionTypeSettings = ActionTypeSettings 
  { "ThirdPartyConfigurationUrl'" :: NullOrUndefined (Url)
  , "EntityUrlTemplate'" :: NullOrUndefined (UrlTemplate)
  , "ExecutionUrlTemplate'" :: NullOrUndefined (UrlTemplate)
  , "RevisionUrlTemplate'" :: NullOrUndefined (UrlTemplate)
  }
derive instance newtypeActionTypeSettings :: Newtype ActionTypeSettings _


-- | <p>The approval action has already been approved or rejected.</p>
newtype ApprovalAlreadyCompletedException = ApprovalAlreadyCompletedException 
  { 
  }
derive instance newtypeApprovalAlreadyCompletedException :: Newtype ApprovalAlreadyCompletedException _


-- | <p>Represents information about the result of an approval request.</p>
newtype ApprovalResult = ApprovalResult 
  { "Summary'" :: (ApprovalSummary)
  , "Status'" :: (ApprovalStatus)
  }
derive instance newtypeApprovalResult :: Newtype ApprovalResult _


newtype ApprovalStatus = ApprovalStatus String
derive instance newtypeApprovalStatus :: Newtype ApprovalStatus _


newtype ApprovalSummary = ApprovalSummary String
derive instance newtypeApprovalSummary :: Newtype ApprovalSummary _


newtype ApprovalToken = ApprovalToken String
derive instance newtypeApprovalToken :: Newtype ApprovalToken _


-- | <p>Represents information about an artifact that will be worked upon by actions in the pipeline.</p>
newtype Artifact = Artifact 
  { "Name'" :: NullOrUndefined (ArtifactName)
  , "Revision'" :: NullOrUndefined (Revision)
  , "Location'" :: NullOrUndefined (ArtifactLocation)
  }
derive instance newtypeArtifact :: Newtype Artifact _


-- | <p>Returns information about the details of an artifact.</p>
newtype ArtifactDetails = ArtifactDetails 
  { "MinimumCount'" :: (MinimumArtifactCount)
  , "MaximumCount'" :: (MaximumArtifactCount)
  }
derive instance newtypeArtifactDetails :: Newtype ArtifactDetails _


newtype ArtifactList = ArtifactList (Array Artifact)
derive instance newtypeArtifactList :: Newtype ArtifactList _


-- | <p>Represents information about the location of an artifact.</p>
newtype ArtifactLocation = ArtifactLocation 
  { "Type'" :: NullOrUndefined (ArtifactLocationType)
  , "S3Location'" :: NullOrUndefined (S3ArtifactLocation)
  }
derive instance newtypeArtifactLocation :: Newtype ArtifactLocation _


newtype ArtifactLocationType = ArtifactLocationType String
derive instance newtypeArtifactLocationType :: Newtype ArtifactLocationType _


newtype ArtifactName = ArtifactName String
derive instance newtypeArtifactName :: Newtype ArtifactName _


-- | <p>Represents revision details of an artifact. </p>
newtype ArtifactRevision = ArtifactRevision 
  { "Name'" :: NullOrUndefined (ArtifactName)
  , "RevisionId'" :: NullOrUndefined (Revision)
  , "RevisionChangeIdentifier'" :: NullOrUndefined (RevisionChangeIdentifier)
  , "RevisionSummary'" :: NullOrUndefined (RevisionSummary)
  , "Created'" :: NullOrUndefined (Number)
  , "RevisionUrl'" :: NullOrUndefined (Url)
  }
derive instance newtypeArtifactRevision :: Newtype ArtifactRevision _


newtype ArtifactRevisionList = ArtifactRevisionList (Array ArtifactRevision)
derive instance newtypeArtifactRevisionList :: Newtype ArtifactRevisionList _


-- | <p>The Amazon S3 bucket where artifacts are stored for the pipeline.</p>
newtype ArtifactStore = ArtifactStore 
  { "Type'" :: (ArtifactStoreType)
  , "Location'" :: (ArtifactStoreLocation)
  , "EncryptionKey'" :: NullOrUndefined (EncryptionKey)
  }
derive instance newtypeArtifactStore :: Newtype ArtifactStore _


newtype ArtifactStoreLocation = ArtifactStoreLocation String
derive instance newtypeArtifactStoreLocation :: Newtype ArtifactStoreLocation _


newtype ArtifactStoreType = ArtifactStoreType String
derive instance newtypeArtifactStoreType :: Newtype ArtifactStoreType _


-- | <p>Reserved for future use.</p>
newtype BlockerDeclaration = BlockerDeclaration 
  { "Name'" :: (BlockerName)
  , "Type'" :: (BlockerType)
  }
derive instance newtypeBlockerDeclaration :: Newtype BlockerDeclaration _


newtype BlockerName = BlockerName String
derive instance newtypeBlockerName :: Newtype BlockerName _


newtype BlockerType = BlockerType String
derive instance newtypeBlockerType :: Newtype BlockerType _


newtype ClientId = ClientId String
derive instance newtypeClientId :: Newtype ClientId _


newtype ClientToken = ClientToken String
derive instance newtypeClientToken :: Newtype ClientToken _


newtype Code = Code String
derive instance newtypeCode :: Newtype Code _


newtype ContinuationToken = ContinuationToken String
derive instance newtypeContinuationToken :: Newtype ContinuationToken _


-- | <p>Represents the input of a CreateCustomActionType operation.</p>
newtype CreateCustomActionTypeInput = CreateCustomActionTypeInput 
  { "Category'" :: (ActionCategory)
  , "Provider'" :: (ActionProvider)
  , "Version'" :: (Version)
  , "Settings'" :: NullOrUndefined (ActionTypeSettings)
  , "ConfigurationProperties'" :: NullOrUndefined (ActionConfigurationPropertyList)
  , "InputArtifactDetails'" :: (ArtifactDetails)
  , "OutputArtifactDetails'" :: (ArtifactDetails)
  }
derive instance newtypeCreateCustomActionTypeInput :: Newtype CreateCustomActionTypeInput _


-- | <p>Represents the output of a CreateCustomActionType operation.</p>
newtype CreateCustomActionTypeOutput = CreateCustomActionTypeOutput 
  { "ActionType'" :: (ActionType)
  }
derive instance newtypeCreateCustomActionTypeOutput :: Newtype CreateCustomActionTypeOutput _


-- | <p>Represents the input of a CreatePipeline action.</p>
newtype CreatePipelineInput = CreatePipelineInput 
  { "Pipeline'" :: (PipelineDeclaration)
  }
derive instance newtypeCreatePipelineInput :: Newtype CreatePipelineInput _


-- | <p>Represents the output of a CreatePipeline action.</p>
newtype CreatePipelineOutput = CreatePipelineOutput 
  { "Pipeline'" :: NullOrUndefined (PipelineDeclaration)
  }
derive instance newtypeCreatePipelineOutput :: Newtype CreatePipelineOutput _


-- | <p>Represents information about a current revision.</p>
newtype CurrentRevision = CurrentRevision 
  { "Revision'" :: (Revision)
  , "ChangeIdentifier'" :: (RevisionChangeIdentifier)
  , "Created'" :: NullOrUndefined (Time)
  , "RevisionSummary'" :: NullOrUndefined (RevisionSummary)
  }
derive instance newtypeCurrentRevision :: Newtype CurrentRevision _


-- | <p>Represents the input of a DeleteCustomActionType operation. The custom action will be marked as deleted.</p>
newtype DeleteCustomActionTypeInput = DeleteCustomActionTypeInput 
  { "Category'" :: (ActionCategory)
  , "Provider'" :: (ActionProvider)
  , "Version'" :: (Version)
  }
derive instance newtypeDeleteCustomActionTypeInput :: Newtype DeleteCustomActionTypeInput _


-- | <p>Represents the input of a DeletePipeline action.</p>
newtype DeletePipelineInput = DeletePipelineInput 
  { "Name'" :: (PipelineName)
  }
derive instance newtypeDeletePipelineInput :: Newtype DeletePipelineInput _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


-- | <p>Represents the input of a DisableStageTransition action.</p>
newtype DisableStageTransitionInput = DisableStageTransitionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "TransitionType'" :: (StageTransitionType)
  , "Reason'" :: (DisabledReason)
  }
derive instance newtypeDisableStageTransitionInput :: Newtype DisableStageTransitionInput _


newtype DisabledReason = DisabledReason String
derive instance newtypeDisabledReason :: Newtype DisabledReason _


-- | <p>Represents the input of an EnableStageTransition action.</p>
newtype EnableStageTransitionInput = EnableStageTransitionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "TransitionType'" :: (StageTransitionType)
  }
derive instance newtypeEnableStageTransitionInput :: Newtype EnableStageTransitionInput _


newtype Enabled = Enabled Boolean
derive instance newtypeEnabled :: Newtype Enabled _


-- | <p>Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.</p>
newtype EncryptionKey = EncryptionKey 
  { "Id'" :: (EncryptionKeyId)
  , "Type'" :: (EncryptionKeyType)
  }
derive instance newtypeEncryptionKey :: Newtype EncryptionKey _


newtype EncryptionKeyId = EncryptionKeyId String
derive instance newtypeEncryptionKeyId :: Newtype EncryptionKeyId _


newtype EncryptionKeyType = EncryptionKeyType String
derive instance newtypeEncryptionKeyType :: Newtype EncryptionKeyType _


-- | <p>Represents information about an error in AWS CodePipeline.</p>
newtype ErrorDetails = ErrorDetails 
  { "Code'" :: NullOrUndefined (Code)
  , "Message'" :: NullOrUndefined (Message)
  }
derive instance newtypeErrorDetails :: Newtype ErrorDetails _


-- | <p>The details of the actions taken and results produced on an artifact as it passes through stages in the pipeline.</p>
newtype ExecutionDetails = ExecutionDetails 
  { "Summary'" :: NullOrUndefined (ExecutionSummary)
  , "ExternalExecutionId'" :: NullOrUndefined (ExecutionId)
  , "PercentComplete'" :: NullOrUndefined (Percentage)
  }
derive instance newtypeExecutionDetails :: Newtype ExecutionDetails _


newtype ExecutionId = ExecutionId String
derive instance newtypeExecutionId :: Newtype ExecutionId _


newtype ExecutionSummary = ExecutionSummary String
derive instance newtypeExecutionSummary :: Newtype ExecutionSummary _


-- | <p>Represents information about failure details.</p>
newtype FailureDetails = FailureDetails 
  { "Type'" :: (FailureType)
  , "Message'" :: (Message)
  , "ExternalExecutionId'" :: NullOrUndefined (ExecutionId)
  }
derive instance newtypeFailureDetails :: Newtype FailureDetails _


newtype FailureType = FailureType String
derive instance newtypeFailureType :: Newtype FailureType _


-- | <p>Represents the input of a GetJobDetails action.</p>
newtype GetJobDetailsInput = GetJobDetailsInput 
  { "JobId'" :: (JobId)
  }
derive instance newtypeGetJobDetailsInput :: Newtype GetJobDetailsInput _


-- | <p>Represents the output of a GetJobDetails action.</p>
newtype GetJobDetailsOutput = GetJobDetailsOutput 
  { "JobDetails'" :: NullOrUndefined (JobDetails)
  }
derive instance newtypeGetJobDetailsOutput :: Newtype GetJobDetailsOutput _


-- | <p>Represents the input of a GetPipelineExecution action.</p>
newtype GetPipelineExecutionInput = GetPipelineExecutionInput 
  { "PipelineName'" :: (PipelineName)
  , "PipelineExecutionId'" :: (PipelineExecutionId)
  }
derive instance newtypeGetPipelineExecutionInput :: Newtype GetPipelineExecutionInput _


-- | <p>Represents the output of a GetPipelineExecution action.</p>
newtype GetPipelineExecutionOutput = GetPipelineExecutionOutput 
  { "PipelineExecution'" :: NullOrUndefined (PipelineExecution)
  }
derive instance newtypeGetPipelineExecutionOutput :: Newtype GetPipelineExecutionOutput _


-- | <p>Represents the input of a GetPipeline action.</p>
newtype GetPipelineInput = GetPipelineInput 
  { "Name'" :: (PipelineName)
  , "Version'" :: NullOrUndefined (PipelineVersion)
  }
derive instance newtypeGetPipelineInput :: Newtype GetPipelineInput _


-- | <p>Represents the output of a GetPipeline action.</p>
newtype GetPipelineOutput = GetPipelineOutput 
  { "Pipeline'" :: NullOrUndefined (PipelineDeclaration)
  , "Metadata'" :: NullOrUndefined (PipelineMetadata)
  }
derive instance newtypeGetPipelineOutput :: Newtype GetPipelineOutput _


-- | <p>Represents the input of a GetPipelineState action.</p>
newtype GetPipelineStateInput = GetPipelineStateInput 
  { "Name'" :: (PipelineName)
  }
derive instance newtypeGetPipelineStateInput :: Newtype GetPipelineStateInput _


-- | <p>Represents the output of a GetPipelineState action.</p>
newtype GetPipelineStateOutput = GetPipelineStateOutput 
  { "PipelineName'" :: NullOrUndefined (PipelineName)
  , "PipelineVersion'" :: NullOrUndefined (PipelineVersion)
  , "StageStates'" :: NullOrUndefined (StageStateList)
  , "Created'" :: NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined (Number)
  }
derive instance newtypeGetPipelineStateOutput :: Newtype GetPipelineStateOutput _


-- | <p>Represents the input of a GetThirdPartyJobDetails action.</p>
newtype GetThirdPartyJobDetailsInput = GetThirdPartyJobDetailsInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  }
derive instance newtypeGetThirdPartyJobDetailsInput :: Newtype GetThirdPartyJobDetailsInput _


-- | <p>Represents the output of a GetThirdPartyJobDetails action.</p>
newtype GetThirdPartyJobDetailsOutput = GetThirdPartyJobDetailsOutput 
  { "JobDetails'" :: NullOrUndefined (ThirdPartyJobDetails)
  }
derive instance newtypeGetThirdPartyJobDetailsOutput :: Newtype GetThirdPartyJobDetailsOutput _


-- | <p>Represents information about an artifact to be worked on, such as a test or build artifact.</p>
newtype InputArtifact = InputArtifact 
  { "Name'" :: (ArtifactName)
  }
derive instance newtypeInputArtifact :: Newtype InputArtifact _


newtype InputArtifactList = InputArtifactList (Array InputArtifact)
derive instance newtypeInputArtifactList :: Newtype InputArtifactList _


-- | <p>The specified action declaration was specified in an invalid format.</p>
newtype InvalidActionDeclarationException = InvalidActionDeclarationException 
  { 
  }
derive instance newtypeInvalidActionDeclarationException :: Newtype InvalidActionDeclarationException _


-- | <p>The approval request already received a response or has expired.</p>
newtype InvalidApprovalTokenException = InvalidApprovalTokenException 
  { 
  }
derive instance newtypeInvalidApprovalTokenException :: Newtype InvalidApprovalTokenException _


-- | <p>Reserved for future use.</p>
newtype InvalidBlockerDeclarationException = InvalidBlockerDeclarationException 
  { 
  }
derive instance newtypeInvalidBlockerDeclarationException :: Newtype InvalidBlockerDeclarationException _


-- | <p>The client token was specified in an invalid format</p>
newtype InvalidClientTokenException = InvalidClientTokenException 
  { 
  }
derive instance newtypeInvalidClientTokenException :: Newtype InvalidClientTokenException _


-- | <p>The specified job was specified in an invalid format or cannot be found.</p>
newtype InvalidJobException = InvalidJobException 
  { 
  }
derive instance newtypeInvalidJobException :: Newtype InvalidJobException _


-- | <p>The specified job state was specified in an invalid format.</p>
newtype InvalidJobStateException = InvalidJobStateException 
  { 
  }
derive instance newtypeInvalidJobStateException :: Newtype InvalidJobStateException _


-- | <p>The next token was specified in an invalid format. Make sure that the next token you provided is the token returned by a previous call.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { 
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>The specified nonce was specified in an invalid format.</p>
newtype InvalidNonceException = InvalidNonceException 
  { 
  }
derive instance newtypeInvalidNonceException :: Newtype InvalidNonceException _


-- | <p>The specified stage declaration was specified in an invalid format.</p>
newtype InvalidStageDeclarationException = InvalidStageDeclarationException 
  { 
  }
derive instance newtypeInvalidStageDeclarationException :: Newtype InvalidStageDeclarationException _


-- | <p>The specified structure was specified in an invalid format.</p>
newtype InvalidStructureException = InvalidStructureException 
  { 
  }
derive instance newtypeInvalidStructureException :: Newtype InvalidStructureException _


-- | <p>Represents information about a job.</p>
newtype Job = Job 
  { "Id'" :: NullOrUndefined (JobId)
  , "Data'" :: NullOrUndefined (JobData)
  , "Nonce'" :: NullOrUndefined (Nonce)
  , "AccountId'" :: NullOrUndefined (AccountId)
  }
derive instance newtypeJob :: Newtype Job _


-- | <p>Represents additional information about a job required for a job worker to complete the job.</p>
newtype JobData = JobData 
  { "ActionTypeId'" :: NullOrUndefined (ActionTypeId)
  , "ActionConfiguration'" :: NullOrUndefined (ActionConfiguration)
  , "PipelineContext'" :: NullOrUndefined (PipelineContext)
  , "InputArtifacts'" :: NullOrUndefined (ArtifactList)
  , "OutputArtifacts'" :: NullOrUndefined (ArtifactList)
  , "ArtifactCredentials'" :: NullOrUndefined (AWSSessionCredentials)
  , "ContinuationToken'" :: NullOrUndefined (ContinuationToken)
  , "EncryptionKey'" :: NullOrUndefined (EncryptionKey)
  }
derive instance newtypeJobData :: Newtype JobData _


-- | <p>Represents information about the details of a job.</p>
newtype JobDetails = JobDetails 
  { "Id'" :: NullOrUndefined (JobId)
  , "Data'" :: NullOrUndefined (JobData)
  , "AccountId'" :: NullOrUndefined (AccountId)
  }
derive instance newtypeJobDetails :: Newtype JobDetails _


newtype JobId = JobId String
derive instance newtypeJobId :: Newtype JobId _


newtype JobList = JobList (Array Job)
derive instance newtypeJobList :: Newtype JobList _


-- | <p>The specified job was specified in an invalid format or cannot be found.</p>
newtype JobNotFoundException = JobNotFoundException 
  { 
  }
derive instance newtypeJobNotFoundException :: Newtype JobNotFoundException _


newtype JobStatus = JobStatus String
derive instance newtypeJobStatus :: Newtype JobStatus _


newtype LastChangedAt = LastChangedAt Number
derive instance newtypeLastChangedAt :: Newtype LastChangedAt _


newtype LastChangedBy = LastChangedBy String
derive instance newtypeLastChangedBy :: Newtype LastChangedBy _


newtype LastUpdatedBy = LastUpdatedBy String
derive instance newtypeLastUpdatedBy :: Newtype LastUpdatedBy _


-- | <p>The number of pipelines associated with the AWS account has exceeded the limit allowed for the account.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p>Represents the input of a ListActionTypes action.</p>
newtype ListActionTypesInput = ListActionTypesInput 
  { "ActionOwnerFilter'" :: NullOrUndefined (ActionOwner)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListActionTypesInput :: Newtype ListActionTypesInput _


-- | <p>Represents the output of a ListActionTypes action.</p>
newtype ListActionTypesOutput = ListActionTypesOutput 
  { "ActionTypes'" :: (ActionTypeList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListActionTypesOutput :: Newtype ListActionTypesOutput _


-- | <p>Represents the input of a ListPipelineExecutions action.</p>
newtype ListPipelineExecutionsInput = ListPipelineExecutionsInput 
  { "PipelineName'" :: (PipelineName)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPipelineExecutionsInput :: Newtype ListPipelineExecutionsInput _


-- | <p>Represents the output of a ListPipelineExecutions action.</p>
newtype ListPipelineExecutionsOutput = ListPipelineExecutionsOutput 
  { "PipelineExecutionSummaries'" :: NullOrUndefined (PipelineExecutionSummaryList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPipelineExecutionsOutput :: Newtype ListPipelineExecutionsOutput _


-- | <p>Represents the input of a ListPipelines action.</p>
newtype ListPipelinesInput = ListPipelinesInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPipelinesInput :: Newtype ListPipelinesInput _


-- | <p>Represents the output of a ListPipelines action.</p>
newtype ListPipelinesOutput = ListPipelinesOutput 
  { "Pipelines'" :: NullOrUndefined (PipelineList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListPipelinesOutput :: Newtype ListPipelinesOutput _


newtype MaxBatchSize = MaxBatchSize Int
derive instance newtypeMaxBatchSize :: Newtype MaxBatchSize _


newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


newtype MaximumArtifactCount = MaximumArtifactCount Int
derive instance newtypeMaximumArtifactCount :: Newtype MaximumArtifactCount _


newtype Message = Message String
derive instance newtypeMessage :: Newtype Message _


newtype MinimumArtifactCount = MinimumArtifactCount Int
derive instance newtypeMinimumArtifactCount :: Newtype MinimumArtifactCount _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype Nonce = Nonce String
derive instance newtypeNonce :: Newtype Nonce _


-- | <p>The stage has failed in a later run of the pipeline and the pipelineExecutionId associated with the request is out of date.</p>
newtype NotLatestPipelineExecutionException = NotLatestPipelineExecutionException 
  { 
  }
derive instance newtypeNotLatestPipelineExecutionException :: Newtype NotLatestPipelineExecutionException _


-- | <p>Represents information about the output of an action.</p>
newtype OutputArtifact = OutputArtifact 
  { "Name'" :: (ArtifactName)
  }
derive instance newtypeOutputArtifact :: Newtype OutputArtifact _


newtype OutputArtifactList = OutputArtifactList (Array OutputArtifact)
derive instance newtypeOutputArtifactList :: Newtype OutputArtifactList _


newtype Percentage = Percentage Int
derive instance newtypePercentage :: Newtype Percentage _


newtype PipelineArn = PipelineArn String
derive instance newtypePipelineArn :: Newtype PipelineArn _


-- | <p>Represents information about a pipeline to a job worker.</p>
newtype PipelineContext = PipelineContext 
  { "PipelineName'" :: NullOrUndefined (PipelineName)
  , "Stage'" :: NullOrUndefined (StageContext)
  , "Action'" :: NullOrUndefined (ActionContext)
  }
derive instance newtypePipelineContext :: Newtype PipelineContext _


-- | <p>Represents the structure of actions and stages to be performed in the pipeline.</p>
newtype PipelineDeclaration = PipelineDeclaration 
  { "Name'" :: (PipelineName)
  , "RoleArn'" :: (RoleArn)
  , "ArtifactStore'" :: (ArtifactStore)
  , "Stages'" :: (PipelineStageDeclarationList)
  , "Version'" :: NullOrUndefined (PipelineVersion)
  }
derive instance newtypePipelineDeclaration :: Newtype PipelineDeclaration _


-- | <p>Represents information about an execution of a pipeline.</p>
newtype PipelineExecution = PipelineExecution 
  { "PipelineName'" :: NullOrUndefined (PipelineName)
  , "PipelineVersion'" :: NullOrUndefined (PipelineVersion)
  , "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  , "Status'" :: NullOrUndefined (PipelineExecutionStatus)
  , "ArtifactRevisions'" :: NullOrUndefined (ArtifactRevisionList)
  }
derive instance newtypePipelineExecution :: Newtype PipelineExecution _


newtype PipelineExecutionId = PipelineExecutionId String
derive instance newtypePipelineExecutionId :: Newtype PipelineExecutionId _


-- | <p>The pipeline execution was specified in an invalid format or cannot be found, or an execution ID does not belong to the specified pipeline. </p>
newtype PipelineExecutionNotFoundException = PipelineExecutionNotFoundException 
  { 
  }
derive instance newtypePipelineExecutionNotFoundException :: Newtype PipelineExecutionNotFoundException _


newtype PipelineExecutionStatus = PipelineExecutionStatus String
derive instance newtypePipelineExecutionStatus :: Newtype PipelineExecutionStatus _


-- | <p>Summary information about a pipeline execution.</p>
newtype PipelineExecutionSummary = PipelineExecutionSummary 
  { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  , "Status'" :: NullOrUndefined (PipelineExecutionStatus)
  , "StartTime'" :: NullOrUndefined (Number)
  , "LastUpdateTime'" :: NullOrUndefined (Number)
  }
derive instance newtypePipelineExecutionSummary :: Newtype PipelineExecutionSummary _


newtype PipelineExecutionSummaryList = PipelineExecutionSummaryList (Array PipelineExecutionSummary)
derive instance newtypePipelineExecutionSummaryList :: Newtype PipelineExecutionSummaryList _


newtype PipelineList = PipelineList (Array PipelineSummary)
derive instance newtypePipelineList :: Newtype PipelineList _


-- | <p>Information about a pipeline.</p>
newtype PipelineMetadata = PipelineMetadata 
  { "PipelineArn'" :: NullOrUndefined (PipelineArn)
  , "Created'" :: NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined (Number)
  }
derive instance newtypePipelineMetadata :: Newtype PipelineMetadata _


newtype PipelineName = PipelineName String
derive instance newtypePipelineName :: Newtype PipelineName _


-- | <p>The specified pipeline name is already in use.</p>
newtype PipelineNameInUseException = PipelineNameInUseException 
  { 
  }
derive instance newtypePipelineNameInUseException :: Newtype PipelineNameInUseException _


-- | <p>The specified pipeline was specified in an invalid format or cannot be found.</p>
newtype PipelineNotFoundException = PipelineNotFoundException 
  { 
  }
derive instance newtypePipelineNotFoundException :: Newtype PipelineNotFoundException _


newtype PipelineStageDeclarationList = PipelineStageDeclarationList (Array StageDeclaration)
derive instance newtypePipelineStageDeclarationList :: Newtype PipelineStageDeclarationList _


-- | <p>Returns a summary of a pipeline.</p>
newtype PipelineSummary = PipelineSummary 
  { "Name'" :: NullOrUndefined (PipelineName)
  , "Version'" :: NullOrUndefined (PipelineVersion)
  , "Created'" :: NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined (Number)
  }
derive instance newtypePipelineSummary :: Newtype PipelineSummary _


newtype PipelineVersion = PipelineVersion Int
derive instance newtypePipelineVersion :: Newtype PipelineVersion _


-- | <p>The specified pipeline version was specified in an invalid format or cannot be found.</p>
newtype PipelineVersionNotFoundException = PipelineVersionNotFoundException 
  { 
  }
derive instance newtypePipelineVersionNotFoundException :: Newtype PipelineVersionNotFoundException _


-- | <p>Represents the input of a PollForJobs action.</p>
newtype PollForJobsInput = PollForJobsInput 
  { "ActionTypeId'" :: (ActionTypeId)
  , "MaxBatchSize'" :: NullOrUndefined (MaxBatchSize)
  , "QueryParam'" :: NullOrUndefined (QueryParamMap)
  }
derive instance newtypePollForJobsInput :: Newtype PollForJobsInput _


-- | <p>Represents the output of a PollForJobs action.</p>
newtype PollForJobsOutput = PollForJobsOutput 
  { "Jobs'" :: NullOrUndefined (JobList)
  }
derive instance newtypePollForJobsOutput :: Newtype PollForJobsOutput _


-- | <p>Represents the input of a PollForThirdPartyJobs action.</p>
newtype PollForThirdPartyJobsInput = PollForThirdPartyJobsInput 
  { "ActionTypeId'" :: (ActionTypeId)
  , "MaxBatchSize'" :: NullOrUndefined (MaxBatchSize)
  }
derive instance newtypePollForThirdPartyJobsInput :: Newtype PollForThirdPartyJobsInput _


-- | <p>Represents the output of a PollForThirdPartyJobs action.</p>
newtype PollForThirdPartyJobsOutput = PollForThirdPartyJobsOutput 
  { "Jobs'" :: NullOrUndefined (ThirdPartyJobList)
  }
derive instance newtypePollForThirdPartyJobsOutput :: Newtype PollForThirdPartyJobsOutput _


-- | <p>Represents the input of a PutActionRevision action.</p>
newtype PutActionRevisionInput = PutActionRevisionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "ActionName'" :: (ActionName)
  , "ActionRevision'" :: (ActionRevision)
  }
derive instance newtypePutActionRevisionInput :: Newtype PutActionRevisionInput _


-- | <p>Represents the output of a PutActionRevision action.</p>
newtype PutActionRevisionOutput = PutActionRevisionOutput 
  { "NewRevision'" :: NullOrUndefined (Boolean)
  , "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  }
derive instance newtypePutActionRevisionOutput :: Newtype PutActionRevisionOutput _


-- | <p>Represents the input of a PutApprovalResult action.</p>
newtype PutApprovalResultInput = PutApprovalResultInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "ActionName'" :: (ActionName)
  , "Result'" :: (ApprovalResult)
  , "Token'" :: (ApprovalToken)
  }
derive instance newtypePutApprovalResultInput :: Newtype PutApprovalResultInput _


-- | <p>Represents the output of a PutApprovalResult action.</p>
newtype PutApprovalResultOutput = PutApprovalResultOutput 
  { "ApprovedAt'" :: NullOrUndefined (Number)
  }
derive instance newtypePutApprovalResultOutput :: Newtype PutApprovalResultOutput _


-- | <p>Represents the input of a PutJobFailureResult action.</p>
newtype PutJobFailureResultInput = PutJobFailureResultInput 
  { "JobId'" :: (JobId)
  , "FailureDetails'" :: (FailureDetails)
  }
derive instance newtypePutJobFailureResultInput :: Newtype PutJobFailureResultInput _


-- | <p>Represents the input of a PutJobSuccessResult action.</p>
newtype PutJobSuccessResultInput = PutJobSuccessResultInput 
  { "JobId'" :: (JobId)
  , "CurrentRevision'" :: NullOrUndefined (CurrentRevision)
  , "ContinuationToken'" :: NullOrUndefined (ContinuationToken)
  , "ExecutionDetails'" :: NullOrUndefined (ExecutionDetails)
  }
derive instance newtypePutJobSuccessResultInput :: Newtype PutJobSuccessResultInput _


-- | <p>Represents the input of a PutThirdPartyJobFailureResult action.</p>
newtype PutThirdPartyJobFailureResultInput = PutThirdPartyJobFailureResultInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  , "FailureDetails'" :: (FailureDetails)
  }
derive instance newtypePutThirdPartyJobFailureResultInput :: Newtype PutThirdPartyJobFailureResultInput _


-- | <p>Represents the input of a PutThirdPartyJobSuccessResult action.</p>
newtype PutThirdPartyJobSuccessResultInput = PutThirdPartyJobSuccessResultInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  , "CurrentRevision'" :: NullOrUndefined (CurrentRevision)
  , "ContinuationToken'" :: NullOrUndefined (ContinuationToken)
  , "ExecutionDetails'" :: NullOrUndefined (ExecutionDetails)
  }
derive instance newtypePutThirdPartyJobSuccessResultInput :: Newtype PutThirdPartyJobSuccessResultInput _


newtype QueryParamMap = QueryParamMap (Map ActionConfigurationKey ActionConfigurationQueryableValue)
derive instance newtypeQueryParamMap :: Newtype QueryParamMap _


-- | <p>Represents the input of a RetryStageExecution action.</p>
newtype RetryStageExecutionInput = RetryStageExecutionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "PipelineExecutionId'" :: (PipelineExecutionId)
  , "RetryMode'" :: (StageRetryMode)
  }
derive instance newtypeRetryStageExecutionInput :: Newtype RetryStageExecutionInput _


-- | <p>Represents the output of a RetryStageExecution action.</p>
newtype RetryStageExecutionOutput = RetryStageExecutionOutput 
  { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  }
derive instance newtypeRetryStageExecutionOutput :: Newtype RetryStageExecutionOutput _


newtype Revision = Revision String
derive instance newtypeRevision :: Newtype Revision _


newtype RevisionChangeIdentifier = RevisionChangeIdentifier String
derive instance newtypeRevisionChangeIdentifier :: Newtype RevisionChangeIdentifier _


newtype RevisionSummary = RevisionSummary String
derive instance newtypeRevisionSummary :: Newtype RevisionSummary _


newtype RoleArn = RoleArn String
derive instance newtypeRoleArn :: Newtype RoleArn _


-- | <p>The location of the Amazon S3 bucket that contains a revision.</p>
newtype S3ArtifactLocation = S3ArtifactLocation 
  { "BucketName'" :: (S3BucketName)
  , "ObjectKey'" :: (S3ObjectKey)
  }
derive instance newtypeS3ArtifactLocation :: Newtype S3ArtifactLocation _


newtype S3BucketName = S3BucketName String
derive instance newtypeS3BucketName :: Newtype S3BucketName _


newtype S3ObjectKey = S3ObjectKey String
derive instance newtypeS3ObjectKey :: Newtype S3ObjectKey _


newtype SecretAccessKey = SecretAccessKey String
derive instance newtypeSecretAccessKey :: Newtype SecretAccessKey _


newtype SessionToken = SessionToken String
derive instance newtypeSessionToken :: Newtype SessionToken _


newtype StageActionDeclarationList = StageActionDeclarationList (Array ActionDeclaration)
derive instance newtypeStageActionDeclarationList :: Newtype StageActionDeclarationList _


newtype StageBlockerDeclarationList = StageBlockerDeclarationList (Array BlockerDeclaration)
derive instance newtypeStageBlockerDeclarationList :: Newtype StageBlockerDeclarationList _


-- | <p>Represents information about a stage to a job worker.</p>
newtype StageContext = StageContext 
  { "Name'" :: NullOrUndefined (StageName)
  }
derive instance newtypeStageContext :: Newtype StageContext _


-- | <p>Represents information about a stage and its definition.</p>
newtype StageDeclaration = StageDeclaration 
  { "Name'" :: (StageName)
  , "Blockers'" :: NullOrUndefined (StageBlockerDeclarationList)
  , "Actions'" :: (StageActionDeclarationList)
  }
derive instance newtypeStageDeclaration :: Newtype StageDeclaration _


-- | <p>Represents information about the run of a stage.</p>
newtype StageExecution = StageExecution 
  { "PipelineExecutionId'" :: (PipelineExecutionId)
  , "Status'" :: (StageExecutionStatus)
  }
derive instance newtypeStageExecution :: Newtype StageExecution _


newtype StageExecutionStatus = StageExecutionStatus String
derive instance newtypeStageExecutionStatus :: Newtype StageExecutionStatus _


newtype StageName = StageName String
derive instance newtypeStageName :: Newtype StageName _


-- | <p>The specified stage was specified in an invalid format or cannot be found.</p>
newtype StageNotFoundException = StageNotFoundException 
  { 
  }
derive instance newtypeStageNotFoundException :: Newtype StageNotFoundException _


-- | <p>The specified stage can't be retried because the pipeline structure or stage state changed after the stage was not completed; the stage contains no failed actions; one or more actions are still in progress; or another retry attempt is already in progress. </p>
newtype StageNotRetryableException = StageNotRetryableException 
  { 
  }
derive instance newtypeStageNotRetryableException :: Newtype StageNotRetryableException _


newtype StageRetryMode = StageRetryMode String
derive instance newtypeStageRetryMode :: Newtype StageRetryMode _


-- | <p>Represents information about the state of the stage.</p>
newtype StageState = StageState 
  { "StageName'" :: NullOrUndefined (StageName)
  , "InboundTransitionState'" :: NullOrUndefined (TransitionState)
  , "ActionStates'" :: NullOrUndefined (ActionStateList)
  , "LatestExecution'" :: NullOrUndefined (StageExecution)
  }
derive instance newtypeStageState :: Newtype StageState _


newtype StageStateList = StageStateList (Array StageState)
derive instance newtypeStageStateList :: Newtype StageStateList _


newtype StageTransitionType = StageTransitionType String
derive instance newtypeStageTransitionType :: Newtype StageTransitionType _


-- | <p>Represents the input of a StartPipelineExecution action.</p>
newtype StartPipelineExecutionInput = StartPipelineExecutionInput 
  { "Name'" :: (PipelineName)
  }
derive instance newtypeStartPipelineExecutionInput :: Newtype StartPipelineExecutionInput _


-- | <p>Represents the output of a StartPipelineExecution action.</p>
newtype StartPipelineExecutionOutput = StartPipelineExecutionOutput 
  { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  }
derive instance newtypeStartPipelineExecutionOutput :: Newtype StartPipelineExecutionOutput _


-- | <p>A response to a PollForThirdPartyJobs request returned by AWS CodePipeline when there is a job to be worked upon by a partner action.</p>
newtype ThirdPartyJob = ThirdPartyJob 
  { "ClientId'" :: NullOrUndefined (ClientId)
  , "JobId'" :: NullOrUndefined (JobId)
  }
derive instance newtypeThirdPartyJob :: Newtype ThirdPartyJob _


-- | <p>Represents information about the job data for a partner action.</p>
newtype ThirdPartyJobData = ThirdPartyJobData 
  { "ActionTypeId'" :: NullOrUndefined (ActionTypeId)
  , "ActionConfiguration'" :: NullOrUndefined (ActionConfiguration)
  , "PipelineContext'" :: NullOrUndefined (PipelineContext)
  , "InputArtifacts'" :: NullOrUndefined (ArtifactList)
  , "OutputArtifacts'" :: NullOrUndefined (ArtifactList)
  , "ArtifactCredentials'" :: NullOrUndefined (AWSSessionCredentials)
  , "ContinuationToken'" :: NullOrUndefined (ContinuationToken)
  , "EncryptionKey'" :: NullOrUndefined (EncryptionKey)
  }
derive instance newtypeThirdPartyJobData :: Newtype ThirdPartyJobData _


-- | <p>The details of a job sent in response to a GetThirdPartyJobDetails request.</p>
newtype ThirdPartyJobDetails = ThirdPartyJobDetails 
  { "Id'" :: NullOrUndefined (ThirdPartyJobId)
  , "Data'" :: NullOrUndefined (ThirdPartyJobData)
  , "Nonce'" :: NullOrUndefined (Nonce)
  }
derive instance newtypeThirdPartyJobDetails :: Newtype ThirdPartyJobDetails _


newtype ThirdPartyJobId = ThirdPartyJobId String
derive instance newtypeThirdPartyJobId :: Newtype ThirdPartyJobId _


newtype ThirdPartyJobList = ThirdPartyJobList (Array ThirdPartyJob)
derive instance newtypeThirdPartyJobList :: Newtype ThirdPartyJobList _


newtype Time = Time Number
derive instance newtypeTime :: Newtype Time _


-- | <p>Represents information about the state of transitions between one stage and another stage.</p>
newtype TransitionState = TransitionState 
  { "Enabled'" :: NullOrUndefined (Enabled)
  , "LastChangedBy'" :: NullOrUndefined (LastChangedBy)
  , "LastChangedAt'" :: NullOrUndefined (LastChangedAt)
  , "DisabledReason'" :: NullOrUndefined (DisabledReason)
  }
derive instance newtypeTransitionState :: Newtype TransitionState _


-- | <p>Represents the input of an UpdatePipeline action.</p>
newtype UpdatePipelineInput = UpdatePipelineInput 
  { "Pipeline'" :: (PipelineDeclaration)
  }
derive instance newtypeUpdatePipelineInput :: Newtype UpdatePipelineInput _


-- | <p>Represents the output of an UpdatePipeline action.</p>
newtype UpdatePipelineOutput = UpdatePipelineOutput 
  { "Pipeline'" :: NullOrUndefined (PipelineDeclaration)
  }
derive instance newtypeUpdatePipelineOutput :: Newtype UpdatePipelineOutput _


newtype Url = Url String
derive instance newtypeUrl :: Newtype Url _


newtype UrlTemplate = UrlTemplate String
derive instance newtypeUrlTemplate :: Newtype UrlTemplate _


-- | <p>The validation was specified in an invalid format.</p>
newtype ValidationException = ValidationException 
  { 
  }
derive instance newtypeValidationException :: Newtype ValidationException _


newtype Version = Version String
derive instance newtypeVersion :: Newtype Version _
