

-- | <fullname>AWS CodePipeline</fullname> <p> <b>Overview</b> </p> <p>This is the AWS CodePipeline API Reference. This guide provides descriptions of the actions and data types for AWS CodePipeline. Some functionality for your pipeline is only configurable through the API. For additional information, see the <a href="http://docs.aws.amazon.com/codepipeline/latest/userguide/welcome.html">AWS CodePipeline User Guide</a>.</p> <p>You can use the AWS CodePipeline API to work with pipelines, stages, actions, gates, and transitions, as described below.</p> <p> <i>Pipelines</i> are models of automated release processes. Each pipeline is uniquely named, and consists of actions, gates, and stages. </p> <p>You can work with pipelines by calling:</p> <ul> <li> <p> <a>CreatePipeline</a>, which creates a uniquely-named pipeline.</p> </li> <li> <p> <a>DeletePipeline</a>, which deletes the specified pipeline.</p> </li> <li> <p> <a>GetPipeline</a>, which returns information about the pipeline structure and pipeline metadata, including the pipeline Amazon Resource Name (ARN).</p> </li> <li> <p> <a>GetPipelineExecution</a>, which returns information about a specific execution of a pipeline.</p> </li> <li> <p> <a>GetPipelineState</a>, which returns information about the current state of the stages and actions of a pipeline.</p> </li> <li> <p> <a>ListPipelines</a>, which gets a summary of all of the pipelines associated with your account.</p> </li> <li> <p> <a>ListPipelineExecutions</a>, which gets a summary of the most recent executions for a pipeline.</p> </li> <li> <p> <a>StartPipelineExecution</a>, which runs the the most recent revision of an artifact through the pipeline.</p> </li> <li> <p> <a>UpdatePipeline</a>, which updates a pipeline with edits or changes to the structure of the pipeline.</p> </li> </ul> <p>Pipelines include <i>stages</i>, which are logical groupings of gates and actions. Each stage contains one or more actions that must complete before the next stage begins. A stage will result in success or failure. If a stage fails, then the pipeline stops at that stage and will remain stopped until either a new version of an artifact appears in the source location, or a user takes action to re-run the most recent artifact through the pipeline. You can call <a>GetPipelineState</a>, which displays the status of a pipeline, including the status of stages in the pipeline, or <a>GetPipeline</a>, which returns the entire structure of the pipeline, including the stages of that pipeline. For more information about the structure of stages and actions, also refer to the <a href="http://docs.aws.amazon.com/codepipeline/latest/userguide/pipeline-structure.html">AWS CodePipeline Pipeline Structure Reference</a>.</p> <p>Pipeline stages include <i>actions</i>, which are categorized into categories such as source or build actions performed within a stage of a pipeline. For example, you can use a source action to import artifacts into a pipeline from a source such as Amazon S3. Like stages, you do not work with actions directly in most cases, but you do define and interact with actions when working with pipeline operations such as <a>CreatePipeline</a> and <a>GetPipelineState</a>. </p> <p>Pipelines also include <i>transitions</i>, which allow the transition of artifacts from one stage to the next in a pipeline after the actions in one stage complete.</p> <p>You can work with transitions by calling:</p> <ul> <li> <p> <a>DisableStageTransition</a>, which prevents artifacts from transitioning to the next stage in a pipeline.</p> </li> <li> <p> <a>EnableStageTransition</a>, which enables transition of artifacts between stages in a pipeline. </p> </li> </ul> <p> <b>Using the API to integrate with AWS CodePipeline</b> </p> <p>For third-party integrators or developers who want to create their own integrations with AWS CodePipeline, the expected sequence varies from the standard API user. In order to integrate with AWS CodePipeline, developers will need to work with the following items:</p> <p> <b>Jobs</b>, which are instances of an action. For example, a job for a source action might import a revision of an artifact from a source. </p> <p>You can work with jobs by calling:</p> <ul> <li> <p> <a>AcknowledgeJob</a>, which confirms whether a job worker has received the specified job,</p> </li> <li> <p> <a>GetJobDetails</a>, which returns the details of a job,</p> </li> <li> <p> <a>PollForJobs</a>, which determines whether there are any jobs to act upon, </p> </li> <li> <p> <a>PutJobFailureResult</a>, which provides details of a job failure, and</p> </li> <li> <p> <a>PutJobSuccessResult</a>, which provides details of a job success.</p> </li> </ul> <p> <b>Third party jobs</b>, which are instances of an action created by a partner action and integrated into AWS CodePipeline. Partner actions are created by members of the AWS Partner Network.</p> <p>You can work with third party jobs by calling:</p> <ul> <li> <p> <a>AcknowledgeThirdPartyJob</a>, which confirms whether a job worker has received the specified job,</p> </li> <li> <p> <a>GetThirdPartyJobDetails</a>, which requests the details of a job for a partner action,</p> </li> <li> <p> <a>PollForThirdPartyJobs</a>, which determines whether there are any jobs to act upon, </p> </li> <li> <p> <a>PutThirdPartyJobFailureResult</a>, which provides details of a job failure, and</p> </li> <li> <p> <a>PutThirdPartyJobSuccessResult</a>, which provides details of a job success.</p> </li> </ul>
module AWS.CodePipeline where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CodePipeline" :: String


-- | <p>Returns information about a specified job and whether that job has been received by the job worker. Only used for custom actions.</p>
acknowledgeJob :: forall eff. AcknowledgeJobInput -> Aff (err :: AWS.RequestError | eff) AcknowledgeJobOutput
acknowledgeJob = AWS.request serviceName "AcknowledgeJob" 


-- | <p>Confirms a job worker has received the specified job. Only used for partner actions.</p>
acknowledgeThirdPartyJob :: forall eff. AcknowledgeThirdPartyJobInput -> Aff (err :: AWS.RequestError | eff) AcknowledgeThirdPartyJobOutput
acknowledgeThirdPartyJob = AWS.request serviceName "AcknowledgeThirdPartyJob" 


-- | <p>Creates a new custom action that can be used in all pipelines associated with the AWS account. Only used for custom actions.</p>
createCustomActionType :: forall eff. CreateCustomActionTypeInput -> Aff (err :: AWS.RequestError | eff) CreateCustomActionTypeOutput
createCustomActionType = AWS.request serviceName "CreateCustomActionType" 


-- | <p>Creates a pipeline.</p>
createPipeline :: forall eff. CreatePipelineInput -> Aff (err :: AWS.RequestError | eff) CreatePipelineOutput
createPipeline = AWS.request serviceName "CreatePipeline" 


-- | <p>Marks a custom action as deleted. PollForJobs for the custom action will fail after the action is marked for deletion. Only used for custom actions.</p> <important> <p>You cannot recreate a custom action after it has been deleted unless you increase the version number of the action.</p> </important>
deleteCustomActionType :: forall eff. DeleteCustomActionTypeInput -> Aff (err :: AWS.RequestError | eff) Unit
deleteCustomActionType = AWS.request serviceName "DeleteCustomActionType" 


-- | <p>Deletes the specified pipeline.</p>
deletePipeline :: forall eff. DeletePipelineInput -> Aff (err :: AWS.RequestError | eff) Unit
deletePipeline = AWS.request serviceName "DeletePipeline" 


-- | <p>Prevents artifacts in a pipeline from transitioning to the next stage in the pipeline.</p>
disableStageTransition :: forall eff. DisableStageTransitionInput -> Aff (err :: AWS.RequestError | eff) Unit
disableStageTransition = AWS.request serviceName "DisableStageTransition" 


-- | <p>Enables artifacts in a pipeline to transition to a stage in a pipeline.</p>
enableStageTransition :: forall eff. EnableStageTransitionInput -> Aff (err :: AWS.RequestError | eff) Unit
enableStageTransition = AWS.request serviceName "EnableStageTransition" 


-- | <p>Returns information about a job. Only used for custom actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
getJobDetails :: forall eff. GetJobDetailsInput -> Aff (err :: AWS.RequestError | eff) GetJobDetailsOutput
getJobDetails = AWS.request serviceName "GetJobDetails" 


-- | <p>Returns the metadata, structure, stages, and actions of a pipeline. Can be used to return the entire structure of a pipeline in JSON format, which can then be modified and used to update the pipeline structure with <a>UpdatePipeline</a>.</p>
getPipeline :: forall eff. GetPipelineInput -> Aff (err :: AWS.RequestError | eff) GetPipelineOutput
getPipeline = AWS.request serviceName "GetPipeline" 


-- | <p>Returns information about an execution of a pipeline, including details about artifacts, the pipeline execution ID, and the name, version, and status of the pipeline.</p>
getPipelineExecution :: forall eff. GetPipelineExecutionInput -> Aff (err :: AWS.RequestError | eff) GetPipelineExecutionOutput
getPipelineExecution = AWS.request serviceName "GetPipelineExecution" 


-- | <p>Returns information about the state of a pipeline, including the stages and actions.</p>
getPipelineState :: forall eff. GetPipelineStateInput -> Aff (err :: AWS.RequestError | eff) GetPipelineStateOutput
getPipelineState = AWS.request serviceName "GetPipelineState" 


-- | <p>Requests the details of a job for a third party action. Only used for partner actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
getThirdPartyJobDetails :: forall eff. GetThirdPartyJobDetailsInput -> Aff (err :: AWS.RequestError | eff) GetThirdPartyJobDetailsOutput
getThirdPartyJobDetails = AWS.request serviceName "GetThirdPartyJobDetails" 


-- | <p>Gets a summary of all AWS CodePipeline action types associated with your account.</p>
listActionTypes :: forall eff. ListActionTypesInput -> Aff (err :: AWS.RequestError | eff) ListActionTypesOutput
listActionTypes = AWS.request serviceName "ListActionTypes" 


-- | <p>Gets a summary of the most recent executions for a pipeline.</p>
listPipelineExecutions :: forall eff. ListPipelineExecutionsInput -> Aff (err :: AWS.RequestError | eff) ListPipelineExecutionsOutput
listPipelineExecutions = AWS.request serviceName "ListPipelineExecutions" 


-- | <p>Gets a summary of all of the pipelines associated with your account.</p>
listPipelines :: forall eff. ListPipelinesInput -> Aff (err :: AWS.RequestError | eff) ListPipelinesOutput
listPipelines = AWS.request serviceName "ListPipelines" 


-- | <p>Returns information about any jobs for AWS CodePipeline to act upon.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>
pollForJobs :: forall eff. PollForJobsInput -> Aff (err :: AWS.RequestError | eff) PollForJobsOutput
pollForJobs = AWS.request serviceName "PollForJobs" 


-- | <p>Determines whether there are any third party jobs for a job worker to act on. Only used for partner actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts.</p> </important>
pollForThirdPartyJobs :: forall eff. PollForThirdPartyJobsInput -> Aff (err :: AWS.RequestError | eff) PollForThirdPartyJobsOutput
pollForThirdPartyJobs = AWS.request serviceName "PollForThirdPartyJobs" 


-- | <p>Provides information to AWS CodePipeline about new revisions to a source.</p>
putActionRevision :: forall eff. PutActionRevisionInput -> Aff (err :: AWS.RequestError | eff) PutActionRevisionOutput
putActionRevision = AWS.request serviceName "PutActionRevision" 


-- | <p>Provides the response to a manual approval request to AWS CodePipeline. Valid responses include Approved and Rejected.</p>
putApprovalResult :: forall eff. PutApprovalResultInput -> Aff (err :: AWS.RequestError | eff) PutApprovalResultOutput
putApprovalResult = AWS.request serviceName "PutApprovalResult" 


-- | <p>Represents the failure of a job as returned to the pipeline by a job worker. Only used for custom actions.</p>
putJobFailureResult :: forall eff. PutJobFailureResultInput -> Aff (err :: AWS.RequestError | eff) Unit
putJobFailureResult = AWS.request serviceName "PutJobFailureResult" 


-- | <p>Represents the success of a job as returned to the pipeline by a job worker. Only used for custom actions.</p>
putJobSuccessResult :: forall eff. PutJobSuccessResultInput -> Aff (err :: AWS.RequestError | eff) Unit
putJobSuccessResult = AWS.request serviceName "PutJobSuccessResult" 


-- | <p>Represents the failure of a third party job as returned to the pipeline by a job worker. Only used for partner actions.</p>
putThirdPartyJobFailureResult :: forall eff. PutThirdPartyJobFailureResultInput -> Aff (err :: AWS.RequestError | eff) Unit
putThirdPartyJobFailureResult = AWS.request serviceName "PutThirdPartyJobFailureResult" 


-- | <p>Represents the success of a third party job as returned to the pipeline by a job worker. Only used for partner actions.</p>
putThirdPartyJobSuccessResult :: forall eff. PutThirdPartyJobSuccessResultInput -> Aff (err :: AWS.RequestError | eff) Unit
putThirdPartyJobSuccessResult = AWS.request serviceName "PutThirdPartyJobSuccessResult" 


-- | <p>Resumes the pipeline execution by retrying the last failed actions in a stage.</p>
retryStageExecution :: forall eff. RetryStageExecutionInput -> Aff (err :: AWS.RequestError | eff) RetryStageExecutionOutput
retryStageExecution = AWS.request serviceName "RetryStageExecution" 


-- | <p>Starts the specified pipeline. Specifically, it begins processing the latest commit to the source location specified as part of the pipeline.</p>
startPipelineExecution :: forall eff. StartPipelineExecutionInput -> Aff (err :: AWS.RequestError | eff) StartPipelineExecutionOutput
startPipelineExecution = AWS.request serviceName "StartPipelineExecution" 


-- | <p>Updates a specified pipeline with edits or changes to its structure. Use a JSON file with the pipeline structure in conjunction with UpdatePipeline to provide the full structure of the pipeline. Updating the pipeline increases the version number of the pipeline by 1.</p>
updatePipeline :: forall eff. UpdatePipelineInput -> Aff (err :: AWS.RequestError | eff) UpdatePipelineOutput
updatePipeline = AWS.request serviceName "UpdatePipeline" 


-- | <p>Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the Amazon S3 bucket used to store artifact for the pipeline in AWS CodePipeline.</p>
newtype AWSSessionCredentials = AWSSessionCredentials 
  { "AccessKeyId'" :: (AccessKeyId)
  , "SecretAccessKey'" :: (SecretAccessKey)
  , "SessionToken'" :: (SessionToken)
  }


newtype AccessKeyId = AccessKeyId String


newtype AccountId = AccountId String


-- | <p>Represents the input of an AcknowledgeJob action.</p>
newtype AcknowledgeJobInput = AcknowledgeJobInput 
  { "JobId'" :: (JobId)
  , "Nonce'" :: (Nonce)
  }


-- | <p>Represents the output of an AcknowledgeJob action.</p>
newtype AcknowledgeJobOutput = AcknowledgeJobOutput 
  { "Status'" :: NullOrUndefined (JobStatus)
  }


-- | <p>Represents the input of an AcknowledgeThirdPartyJob action.</p>
newtype AcknowledgeThirdPartyJobInput = AcknowledgeThirdPartyJobInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "Nonce'" :: (Nonce)
  , "ClientToken'" :: (ClientToken)
  }


-- | <p>Represents the output of an AcknowledgeThirdPartyJob action.</p>
newtype AcknowledgeThirdPartyJobOutput = AcknowledgeThirdPartyJobOutput 
  { "Status'" :: NullOrUndefined (JobStatus)
  }


newtype ActionCategory = ActionCategory String


-- | <p>Represents information about an action configuration.</p>
newtype ActionConfiguration = ActionConfiguration 
  { "Configuration'" :: NullOrUndefined (ActionConfigurationMap)
  }


newtype ActionConfigurationKey = ActionConfigurationKey String


newtype ActionConfigurationMap = ActionConfigurationMap (Map ActionConfigurationKey ActionConfigurationValue)


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


newtype ActionConfigurationPropertyList = ActionConfigurationPropertyList (Array ActionConfigurationProperty)


newtype ActionConfigurationPropertyType = ActionConfigurationPropertyType String


newtype ActionConfigurationQueryableValue = ActionConfigurationQueryableValue String


newtype ActionConfigurationValue = ActionConfigurationValue String


-- | <p>Represents the context of an action within the stage of a pipeline to a job worker.</p>
newtype ActionContext = ActionContext 
  { "Name'" :: NullOrUndefined (ActionName)
  }


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


newtype ActionExecutionStatus = ActionExecutionStatus String


newtype ActionExecutionToken = ActionExecutionToken String


newtype ActionName = ActionName String


-- | <p>The specified action cannot be found.</p>
newtype ActionNotFoundException = ActionNotFoundException 
  { 
  }


newtype ActionOwner = ActionOwner String


newtype ActionProvider = ActionProvider String


-- | <p>Represents information about the version (or revision) of an action.</p>
newtype ActionRevision = ActionRevision 
  { "RevisionId'" :: (Revision)
  , "RevisionChangeId'" :: (RevisionChangeIdentifier)
  , "Created'" :: (Number)
  }


newtype ActionRunOrder = ActionRunOrder Int


-- | <p>Represents information about the state of an action.</p>
newtype ActionState = ActionState 
  { "ActionName'" :: NullOrUndefined (ActionName)
  , "CurrentRevision'" :: NullOrUndefined (ActionRevision)
  , "LatestExecution'" :: NullOrUndefined (ActionExecution)
  , "EntityUrl'" :: NullOrUndefined (Url)
  , "RevisionUrl'" :: NullOrUndefined (Url)
  }


newtype ActionStateList = ActionStateList (Array ActionState)


-- | <p>Returns information about the details of an action type.</p>
newtype ActionType = ActionType 
  { "Id'" :: (ActionTypeId)
  , "Settings'" :: NullOrUndefined (ActionTypeSettings)
  , "ActionConfigurationProperties'" :: NullOrUndefined (ActionConfigurationPropertyList)
  , "InputArtifactDetails'" :: (ArtifactDetails)
  , "OutputArtifactDetails'" :: (ArtifactDetails)
  }


-- | <p>Represents information about an action type.</p>
newtype ActionTypeId = ActionTypeId 
  { "Category'" :: (ActionCategory)
  , "Owner'" :: (ActionOwner)
  , "Provider'" :: (ActionProvider)
  , "Version'" :: (Version)
  }


newtype ActionTypeList = ActionTypeList (Array ActionType)


-- | <p>The specified action type cannot be found.</p>
newtype ActionTypeNotFoundException = ActionTypeNotFoundException 
  { 
  }


-- | <p>Returns information about the settings for an action type.</p>
newtype ActionTypeSettings = ActionTypeSettings 
  { "ThirdPartyConfigurationUrl'" :: NullOrUndefined (Url)
  , "EntityUrlTemplate'" :: NullOrUndefined (UrlTemplate)
  , "ExecutionUrlTemplate'" :: NullOrUndefined (UrlTemplate)
  , "RevisionUrlTemplate'" :: NullOrUndefined (UrlTemplate)
  }


-- | <p>The approval action has already been approved or rejected.</p>
newtype ApprovalAlreadyCompletedException = ApprovalAlreadyCompletedException 
  { 
  }


-- | <p>Represents information about the result of an approval request.</p>
newtype ApprovalResult = ApprovalResult 
  { "Summary'" :: (ApprovalSummary)
  , "Status'" :: (ApprovalStatus)
  }


newtype ApprovalStatus = ApprovalStatus String


newtype ApprovalSummary = ApprovalSummary String


newtype ApprovalToken = ApprovalToken String


-- | <p>Represents information about an artifact that will be worked upon by actions in the pipeline.</p>
newtype Artifact = Artifact 
  { "Name'" :: NullOrUndefined (ArtifactName)
  , "Revision'" :: NullOrUndefined (Revision)
  , "Location'" :: NullOrUndefined (ArtifactLocation)
  }


-- | <p>Returns information about the details of an artifact.</p>
newtype ArtifactDetails = ArtifactDetails 
  { "MinimumCount'" :: (MinimumArtifactCount)
  , "MaximumCount'" :: (MaximumArtifactCount)
  }


newtype ArtifactList = ArtifactList (Array Artifact)


-- | <p>Represents information about the location of an artifact.</p>
newtype ArtifactLocation = ArtifactLocation 
  { "Type'" :: NullOrUndefined (ArtifactLocationType)
  , "S3Location'" :: NullOrUndefined (S3ArtifactLocation)
  }


newtype ArtifactLocationType = ArtifactLocationType String


newtype ArtifactName = ArtifactName String


-- | <p>Represents revision details of an artifact. </p>
newtype ArtifactRevision = ArtifactRevision 
  { "Name'" :: NullOrUndefined (ArtifactName)
  , "RevisionId'" :: NullOrUndefined (Revision)
  , "RevisionChangeIdentifier'" :: NullOrUndefined (RevisionChangeIdentifier)
  , "RevisionSummary'" :: NullOrUndefined (RevisionSummary)
  , "Created'" :: NullOrUndefined (Number)
  , "RevisionUrl'" :: NullOrUndefined (Url)
  }


newtype ArtifactRevisionList = ArtifactRevisionList (Array ArtifactRevision)


-- | <p>The Amazon S3 bucket where artifacts are stored for the pipeline.</p>
newtype ArtifactStore = ArtifactStore 
  { "Type'" :: (ArtifactStoreType)
  , "Location'" :: (ArtifactStoreLocation)
  , "EncryptionKey'" :: NullOrUndefined (EncryptionKey)
  }


newtype ArtifactStoreLocation = ArtifactStoreLocation String


newtype ArtifactStoreType = ArtifactStoreType String


-- | <p>Reserved for future use.</p>
newtype BlockerDeclaration = BlockerDeclaration 
  { "Name'" :: (BlockerName)
  , "Type'" :: (BlockerType)
  }


newtype BlockerName = BlockerName String


newtype BlockerType = BlockerType String


newtype ClientId = ClientId String


newtype ClientToken = ClientToken String


newtype Code = Code String


newtype ContinuationToken = ContinuationToken String


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


-- | <p>Represents the output of a CreateCustomActionType operation.</p>
newtype CreateCustomActionTypeOutput = CreateCustomActionTypeOutput 
  { "ActionType'" :: (ActionType)
  }


-- | <p>Represents the input of a CreatePipeline action.</p>
newtype CreatePipelineInput = CreatePipelineInput 
  { "Pipeline'" :: (PipelineDeclaration)
  }


-- | <p>Represents the output of a CreatePipeline action.</p>
newtype CreatePipelineOutput = CreatePipelineOutput 
  { "Pipeline'" :: NullOrUndefined (PipelineDeclaration)
  }


-- | <p>Represents information about a current revision.</p>
newtype CurrentRevision = CurrentRevision 
  { "Revision'" :: (Revision)
  , "ChangeIdentifier'" :: (RevisionChangeIdentifier)
  , "Created'" :: NullOrUndefined (Time)
  , "RevisionSummary'" :: NullOrUndefined (RevisionSummary)
  }


-- | <p>Represents the input of a DeleteCustomActionType operation. The custom action will be marked as deleted.</p>
newtype DeleteCustomActionTypeInput = DeleteCustomActionTypeInput 
  { "Category'" :: (ActionCategory)
  , "Provider'" :: (ActionProvider)
  , "Version'" :: (Version)
  }


-- | <p>Represents the input of a DeletePipeline action.</p>
newtype DeletePipelineInput = DeletePipelineInput 
  { "Name'" :: (PipelineName)
  }


newtype Description = Description String


-- | <p>Represents the input of a DisableStageTransition action.</p>
newtype DisableStageTransitionInput = DisableStageTransitionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "TransitionType'" :: (StageTransitionType)
  , "Reason'" :: (DisabledReason)
  }


newtype DisabledReason = DisabledReason String


-- | <p>Represents the input of an EnableStageTransition action.</p>
newtype EnableStageTransitionInput = EnableStageTransitionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "TransitionType'" :: (StageTransitionType)
  }


newtype Enabled = Enabled Boolean


-- | <p>Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.</p>
newtype EncryptionKey = EncryptionKey 
  { "Id'" :: (EncryptionKeyId)
  , "Type'" :: (EncryptionKeyType)
  }


newtype EncryptionKeyId = EncryptionKeyId String


newtype EncryptionKeyType = EncryptionKeyType String


-- | <p>Represents information about an error in AWS CodePipeline.</p>
newtype ErrorDetails = ErrorDetails 
  { "Code'" :: NullOrUndefined (Code)
  , "Message'" :: NullOrUndefined (Message)
  }


-- | <p>The details of the actions taken and results produced on an artifact as it passes through stages in the pipeline.</p>
newtype ExecutionDetails = ExecutionDetails 
  { "Summary'" :: NullOrUndefined (ExecutionSummary)
  , "ExternalExecutionId'" :: NullOrUndefined (ExecutionId)
  , "PercentComplete'" :: NullOrUndefined (Percentage)
  }


newtype ExecutionId = ExecutionId String


newtype ExecutionSummary = ExecutionSummary String


-- | <p>Represents information about failure details.</p>
newtype FailureDetails = FailureDetails 
  { "Type'" :: (FailureType)
  , "Message'" :: (Message)
  , "ExternalExecutionId'" :: NullOrUndefined (ExecutionId)
  }


newtype FailureType = FailureType String


-- | <p>Represents the input of a GetJobDetails action.</p>
newtype GetJobDetailsInput = GetJobDetailsInput 
  { "JobId'" :: (JobId)
  }


-- | <p>Represents the output of a GetJobDetails action.</p>
newtype GetJobDetailsOutput = GetJobDetailsOutput 
  { "JobDetails'" :: NullOrUndefined (JobDetails)
  }


-- | <p>Represents the input of a GetPipelineExecution action.</p>
newtype GetPipelineExecutionInput = GetPipelineExecutionInput 
  { "PipelineName'" :: (PipelineName)
  , "PipelineExecutionId'" :: (PipelineExecutionId)
  }


-- | <p>Represents the output of a GetPipelineExecution action.</p>
newtype GetPipelineExecutionOutput = GetPipelineExecutionOutput 
  { "PipelineExecution'" :: NullOrUndefined (PipelineExecution)
  }


-- | <p>Represents the input of a GetPipeline action.</p>
newtype GetPipelineInput = GetPipelineInput 
  { "Name'" :: (PipelineName)
  , "Version'" :: NullOrUndefined (PipelineVersion)
  }


-- | <p>Represents the output of a GetPipeline action.</p>
newtype GetPipelineOutput = GetPipelineOutput 
  { "Pipeline'" :: NullOrUndefined (PipelineDeclaration)
  , "Metadata'" :: NullOrUndefined (PipelineMetadata)
  }


-- | <p>Represents the input of a GetPipelineState action.</p>
newtype GetPipelineStateInput = GetPipelineStateInput 
  { "Name'" :: (PipelineName)
  }


-- | <p>Represents the output of a GetPipelineState action.</p>
newtype GetPipelineStateOutput = GetPipelineStateOutput 
  { "PipelineName'" :: NullOrUndefined (PipelineName)
  , "PipelineVersion'" :: NullOrUndefined (PipelineVersion)
  , "StageStates'" :: NullOrUndefined (StageStateList)
  , "Created'" :: NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined (Number)
  }


-- | <p>Represents the input of a GetThirdPartyJobDetails action.</p>
newtype GetThirdPartyJobDetailsInput = GetThirdPartyJobDetailsInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  }


-- | <p>Represents the output of a GetThirdPartyJobDetails action.</p>
newtype GetThirdPartyJobDetailsOutput = GetThirdPartyJobDetailsOutput 
  { "JobDetails'" :: NullOrUndefined (ThirdPartyJobDetails)
  }


-- | <p>Represents information about an artifact to be worked on, such as a test or build artifact.</p>
newtype InputArtifact = InputArtifact 
  { "Name'" :: (ArtifactName)
  }


newtype InputArtifactList = InputArtifactList (Array InputArtifact)


-- | <p>The specified action declaration was specified in an invalid format.</p>
newtype InvalidActionDeclarationException = InvalidActionDeclarationException 
  { 
  }


-- | <p>The approval request already received a response or has expired.</p>
newtype InvalidApprovalTokenException = InvalidApprovalTokenException 
  { 
  }


-- | <p>Reserved for future use.</p>
newtype InvalidBlockerDeclarationException = InvalidBlockerDeclarationException 
  { 
  }


-- | <p>The client token was specified in an invalid format</p>
newtype InvalidClientTokenException = InvalidClientTokenException 
  { 
  }


-- | <p>The specified job was specified in an invalid format or cannot be found.</p>
newtype InvalidJobException = InvalidJobException 
  { 
  }


-- | <p>The specified job state was specified in an invalid format.</p>
newtype InvalidJobStateException = InvalidJobStateException 
  { 
  }


-- | <p>The next token was specified in an invalid format. Make sure that the next token you provided is the token returned by a previous call.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { 
  }


-- | <p>The specified nonce was specified in an invalid format.</p>
newtype InvalidNonceException = InvalidNonceException 
  { 
  }


-- | <p>The specified stage declaration was specified in an invalid format.</p>
newtype InvalidStageDeclarationException = InvalidStageDeclarationException 
  { 
  }


-- | <p>The specified structure was specified in an invalid format.</p>
newtype InvalidStructureException = InvalidStructureException 
  { 
  }


-- | <p>Represents information about a job.</p>
newtype Job = Job 
  { "Id'" :: NullOrUndefined (JobId)
  , "Data'" :: NullOrUndefined (JobData)
  , "Nonce'" :: NullOrUndefined (Nonce)
  , "AccountId'" :: NullOrUndefined (AccountId)
  }


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


-- | <p>Represents information about the details of a job.</p>
newtype JobDetails = JobDetails 
  { "Id'" :: NullOrUndefined (JobId)
  , "Data'" :: NullOrUndefined (JobData)
  , "AccountId'" :: NullOrUndefined (AccountId)
  }


newtype JobId = JobId String


newtype JobList = JobList (Array Job)


-- | <p>The specified job was specified in an invalid format or cannot be found.</p>
newtype JobNotFoundException = JobNotFoundException 
  { 
  }


newtype JobStatus = JobStatus String


newtype LastChangedAt = LastChangedAt Number


newtype LastChangedBy = LastChangedBy String


newtype LastUpdatedBy = LastUpdatedBy String


-- | <p>The number of pipelines associated with the AWS account has exceeded the limit allowed for the account.</p>
newtype LimitExceededException = LimitExceededException 
  { 
  }


-- | <p>Represents the input of a ListActionTypes action.</p>
newtype ListActionTypesInput = ListActionTypesInput 
  { "ActionOwnerFilter'" :: NullOrUndefined (ActionOwner)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a ListActionTypes action.</p>
newtype ListActionTypesOutput = ListActionTypesOutput 
  { "ActionTypes'" :: (ActionTypeList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListPipelineExecutions action.</p>
newtype ListPipelineExecutionsInput = ListPipelineExecutionsInput 
  { "PipelineName'" :: (PipelineName)
  , "MaxResults'" :: NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a ListPipelineExecutions action.</p>
newtype ListPipelineExecutionsOutput = ListPipelineExecutionsOutput 
  { "PipelineExecutionSummaries'" :: NullOrUndefined (PipelineExecutionSummaryList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the input of a ListPipelines action.</p>
newtype ListPipelinesInput = ListPipelinesInput 
  { "NextToken'" :: NullOrUndefined (NextToken)
  }


-- | <p>Represents the output of a ListPipelines action.</p>
newtype ListPipelinesOutput = ListPipelinesOutput 
  { "Pipelines'" :: NullOrUndefined (PipelineList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }


newtype MaxBatchSize = MaxBatchSize Int


newtype MaxResults = MaxResults Int


newtype MaximumArtifactCount = MaximumArtifactCount Int


newtype Message = Message String


newtype MinimumArtifactCount = MinimumArtifactCount Int


newtype NextToken = NextToken String


newtype Nonce = Nonce String


-- | <p>The stage has failed in a later run of the pipeline and the pipelineExecutionId associated with the request is out of date.</p>
newtype NotLatestPipelineExecutionException = NotLatestPipelineExecutionException 
  { 
  }


-- | <p>Represents information about the output of an action.</p>
newtype OutputArtifact = OutputArtifact 
  { "Name'" :: (ArtifactName)
  }


newtype OutputArtifactList = OutputArtifactList (Array OutputArtifact)


newtype Percentage = Percentage Int


newtype PipelineArn = PipelineArn String


-- | <p>Represents information about a pipeline to a job worker.</p>
newtype PipelineContext = PipelineContext 
  { "PipelineName'" :: NullOrUndefined (PipelineName)
  , "Stage'" :: NullOrUndefined (StageContext)
  , "Action'" :: NullOrUndefined (ActionContext)
  }


-- | <p>Represents the structure of actions and stages to be performed in the pipeline.</p>
newtype PipelineDeclaration = PipelineDeclaration 
  { "Name'" :: (PipelineName)
  , "RoleArn'" :: (RoleArn)
  , "ArtifactStore'" :: (ArtifactStore)
  , "Stages'" :: (PipelineStageDeclarationList)
  , "Version'" :: NullOrUndefined (PipelineVersion)
  }


-- | <p>Represents information about an execution of a pipeline.</p>
newtype PipelineExecution = PipelineExecution 
  { "PipelineName'" :: NullOrUndefined (PipelineName)
  , "PipelineVersion'" :: NullOrUndefined (PipelineVersion)
  , "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  , "Status'" :: NullOrUndefined (PipelineExecutionStatus)
  , "ArtifactRevisions'" :: NullOrUndefined (ArtifactRevisionList)
  }


newtype PipelineExecutionId = PipelineExecutionId String


-- | <p>The pipeline execution was specified in an invalid format or cannot be found, or an execution ID does not belong to the specified pipeline. </p>
newtype PipelineExecutionNotFoundException = PipelineExecutionNotFoundException 
  { 
  }


newtype PipelineExecutionStatus = PipelineExecutionStatus String


-- | <p>Summary information about a pipeline execution.</p>
newtype PipelineExecutionSummary = PipelineExecutionSummary 
  { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  , "Status'" :: NullOrUndefined (PipelineExecutionStatus)
  , "StartTime'" :: NullOrUndefined (Number)
  , "LastUpdateTime'" :: NullOrUndefined (Number)
  }


newtype PipelineExecutionSummaryList = PipelineExecutionSummaryList (Array PipelineExecutionSummary)


newtype PipelineList = PipelineList (Array PipelineSummary)


-- | <p>Information about a pipeline.</p>
newtype PipelineMetadata = PipelineMetadata 
  { "PipelineArn'" :: NullOrUndefined (PipelineArn)
  , "Created'" :: NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined (Number)
  }


newtype PipelineName = PipelineName String


-- | <p>The specified pipeline name is already in use.</p>
newtype PipelineNameInUseException = PipelineNameInUseException 
  { 
  }


-- | <p>The specified pipeline was specified in an invalid format or cannot be found.</p>
newtype PipelineNotFoundException = PipelineNotFoundException 
  { 
  }


newtype PipelineStageDeclarationList = PipelineStageDeclarationList (Array StageDeclaration)


-- | <p>Returns a summary of a pipeline.</p>
newtype PipelineSummary = PipelineSummary 
  { "Name'" :: NullOrUndefined (PipelineName)
  , "Version'" :: NullOrUndefined (PipelineVersion)
  , "Created'" :: NullOrUndefined (Number)
  , "Updated'" :: NullOrUndefined (Number)
  }


newtype PipelineVersion = PipelineVersion Int


-- | <p>The specified pipeline version was specified in an invalid format or cannot be found.</p>
newtype PipelineVersionNotFoundException = PipelineVersionNotFoundException 
  { 
  }


-- | <p>Represents the input of a PollForJobs action.</p>
newtype PollForJobsInput = PollForJobsInput 
  { "ActionTypeId'" :: (ActionTypeId)
  , "MaxBatchSize'" :: NullOrUndefined (MaxBatchSize)
  , "QueryParam'" :: NullOrUndefined (QueryParamMap)
  }


-- | <p>Represents the output of a PollForJobs action.</p>
newtype PollForJobsOutput = PollForJobsOutput 
  { "Jobs'" :: NullOrUndefined (JobList)
  }


-- | <p>Represents the input of a PollForThirdPartyJobs action.</p>
newtype PollForThirdPartyJobsInput = PollForThirdPartyJobsInput 
  { "ActionTypeId'" :: (ActionTypeId)
  , "MaxBatchSize'" :: NullOrUndefined (MaxBatchSize)
  }


-- | <p>Represents the output of a PollForThirdPartyJobs action.</p>
newtype PollForThirdPartyJobsOutput = PollForThirdPartyJobsOutput 
  { "Jobs'" :: NullOrUndefined (ThirdPartyJobList)
  }


-- | <p>Represents the input of a PutActionRevision action.</p>
newtype PutActionRevisionInput = PutActionRevisionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "ActionName'" :: (ActionName)
  , "ActionRevision'" :: (ActionRevision)
  }


-- | <p>Represents the output of a PutActionRevision action.</p>
newtype PutActionRevisionOutput = PutActionRevisionOutput 
  { "NewRevision'" :: NullOrUndefined (Boolean)
  , "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  }


-- | <p>Represents the input of a PutApprovalResult action.</p>
newtype PutApprovalResultInput = PutApprovalResultInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "ActionName'" :: (ActionName)
  , "Result'" :: (ApprovalResult)
  , "Token'" :: (ApprovalToken)
  }


-- | <p>Represents the output of a PutApprovalResult action.</p>
newtype PutApprovalResultOutput = PutApprovalResultOutput 
  { "ApprovedAt'" :: NullOrUndefined (Number)
  }


-- | <p>Represents the input of a PutJobFailureResult action.</p>
newtype PutJobFailureResultInput = PutJobFailureResultInput 
  { "JobId'" :: (JobId)
  , "FailureDetails'" :: (FailureDetails)
  }


-- | <p>Represents the input of a PutJobSuccessResult action.</p>
newtype PutJobSuccessResultInput = PutJobSuccessResultInput 
  { "JobId'" :: (JobId)
  , "CurrentRevision'" :: NullOrUndefined (CurrentRevision)
  , "ContinuationToken'" :: NullOrUndefined (ContinuationToken)
  , "ExecutionDetails'" :: NullOrUndefined (ExecutionDetails)
  }


-- | <p>Represents the input of a PutThirdPartyJobFailureResult action.</p>
newtype PutThirdPartyJobFailureResultInput = PutThirdPartyJobFailureResultInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  , "FailureDetails'" :: (FailureDetails)
  }


-- | <p>Represents the input of a PutThirdPartyJobSuccessResult action.</p>
newtype PutThirdPartyJobSuccessResultInput = PutThirdPartyJobSuccessResultInput 
  { "JobId'" :: (ThirdPartyJobId)
  , "ClientToken'" :: (ClientToken)
  , "CurrentRevision'" :: NullOrUndefined (CurrentRevision)
  , "ContinuationToken'" :: NullOrUndefined (ContinuationToken)
  , "ExecutionDetails'" :: NullOrUndefined (ExecutionDetails)
  }


newtype QueryParamMap = QueryParamMap (Map ActionConfigurationKey ActionConfigurationQueryableValue)


-- | <p>Represents the input of a RetryStageExecution action.</p>
newtype RetryStageExecutionInput = RetryStageExecutionInput 
  { "PipelineName'" :: (PipelineName)
  , "StageName'" :: (StageName)
  , "PipelineExecutionId'" :: (PipelineExecutionId)
  , "RetryMode'" :: (StageRetryMode)
  }


-- | <p>Represents the output of a RetryStageExecution action.</p>
newtype RetryStageExecutionOutput = RetryStageExecutionOutput 
  { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  }


newtype Revision = Revision String


newtype RevisionChangeIdentifier = RevisionChangeIdentifier String


newtype RevisionSummary = RevisionSummary String


newtype RoleArn = RoleArn String


-- | <p>The location of the Amazon S3 bucket that contains a revision.</p>
newtype S3ArtifactLocation = S3ArtifactLocation 
  { "BucketName'" :: (S3BucketName)
  , "ObjectKey'" :: (S3ObjectKey)
  }


newtype S3BucketName = S3BucketName String


newtype S3ObjectKey = S3ObjectKey String


newtype SecretAccessKey = SecretAccessKey String


newtype SessionToken = SessionToken String


newtype StageActionDeclarationList = StageActionDeclarationList (Array ActionDeclaration)


newtype StageBlockerDeclarationList = StageBlockerDeclarationList (Array BlockerDeclaration)


-- | <p>Represents information about a stage to a job worker.</p>
newtype StageContext = StageContext 
  { "Name'" :: NullOrUndefined (StageName)
  }


-- | <p>Represents information about a stage and its definition.</p>
newtype StageDeclaration = StageDeclaration 
  { "Name'" :: (StageName)
  , "Blockers'" :: NullOrUndefined (StageBlockerDeclarationList)
  , "Actions'" :: (StageActionDeclarationList)
  }


-- | <p>Represents information about the run of a stage.</p>
newtype StageExecution = StageExecution 
  { "PipelineExecutionId'" :: (PipelineExecutionId)
  , "Status'" :: (StageExecutionStatus)
  }


newtype StageExecutionStatus = StageExecutionStatus String


newtype StageName = StageName String


-- | <p>The specified stage was specified in an invalid format or cannot be found.</p>
newtype StageNotFoundException = StageNotFoundException 
  { 
  }


-- | <p>The specified stage can't be retried because the pipeline structure or stage state changed after the stage was not completed; the stage contains no failed actions; one or more actions are still in progress; or another retry attempt is already in progress. </p>
newtype StageNotRetryableException = StageNotRetryableException 
  { 
  }


newtype StageRetryMode = StageRetryMode String


-- | <p>Represents information about the state of the stage.</p>
newtype StageState = StageState 
  { "StageName'" :: NullOrUndefined (StageName)
  , "InboundTransitionState'" :: NullOrUndefined (TransitionState)
  , "ActionStates'" :: NullOrUndefined (ActionStateList)
  , "LatestExecution'" :: NullOrUndefined (StageExecution)
  }


newtype StageStateList = StageStateList (Array StageState)


newtype StageTransitionType = StageTransitionType String


-- | <p>Represents the input of a StartPipelineExecution action.</p>
newtype StartPipelineExecutionInput = StartPipelineExecutionInput 
  { "Name'" :: (PipelineName)
  }


-- | <p>Represents the output of a StartPipelineExecution action.</p>
newtype StartPipelineExecutionOutput = StartPipelineExecutionOutput 
  { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId)
  }


-- | <p>A response to a PollForThirdPartyJobs request returned by AWS CodePipeline when there is a job to be worked upon by a partner action.</p>
newtype ThirdPartyJob = ThirdPartyJob 
  { "ClientId'" :: NullOrUndefined (ClientId)
  , "JobId'" :: NullOrUndefined (JobId)
  }


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


-- | <p>The details of a job sent in response to a GetThirdPartyJobDetails request.</p>
newtype ThirdPartyJobDetails = ThirdPartyJobDetails 
  { "Id'" :: NullOrUndefined (ThirdPartyJobId)
  , "Data'" :: NullOrUndefined (ThirdPartyJobData)
  , "Nonce'" :: NullOrUndefined (Nonce)
  }


newtype ThirdPartyJobId = ThirdPartyJobId String


newtype ThirdPartyJobList = ThirdPartyJobList (Array ThirdPartyJob)


newtype Time = Time Number


-- | <p>Represents information about the state of transitions between one stage and another stage.</p>
newtype TransitionState = TransitionState 
  { "Enabled'" :: NullOrUndefined (Enabled)
  , "LastChangedBy'" :: NullOrUndefined (LastChangedBy)
  , "LastChangedAt'" :: NullOrUndefined (LastChangedAt)
  , "DisabledReason'" :: NullOrUndefined (DisabledReason)
  }


-- | <p>Represents the input of an UpdatePipeline action.</p>
newtype UpdatePipelineInput = UpdatePipelineInput 
  { "Pipeline'" :: (PipelineDeclaration)
  }


-- | <p>Represents the output of an UpdatePipeline action.</p>
newtype UpdatePipelineOutput = UpdatePipelineOutput 
  { "Pipeline'" :: NullOrUndefined (PipelineDeclaration)
  }


newtype Url = Url String


newtype UrlTemplate = UrlTemplate String


-- | <p>The validation was specified in an invalid format.</p>
newtype ValidationException = ValidationException 
  { 
  }


newtype Version = Version String
