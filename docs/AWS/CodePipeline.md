## Module AWS.CodePipeline

<fullname>AWS CodePipeline</fullname> <p> <b>Overview</b> </p> <p>This is the AWS CodePipeline API Reference. This guide provides descriptions of the actions and data types for AWS CodePipeline. Some functionality for your pipeline is only configurable through the API. For additional information, see the <a href="http://docs.aws.amazon.com/codepipeline/latest/userguide/welcome.html">AWS CodePipeline User Guide</a>.</p> <p>You can use the AWS CodePipeline API to work with pipelines, stages, actions, gates, and transitions, as described below.</p> <p> <i>Pipelines</i> are models of automated release processes. Each pipeline is uniquely named, and consists of actions, gates, and stages. </p> <p>You can work with pipelines by calling:</p> <ul> <li> <p> <a>CreatePipeline</a>, which creates a uniquely-named pipeline.</p> </li> <li> <p> <a>DeletePipeline</a>, which deletes the specified pipeline.</p> </li> <li> <p> <a>GetPipeline</a>, which returns information about the pipeline structure and pipeline metadata, including the pipeline Amazon Resource Name (ARN).</p> </li> <li> <p> <a>GetPipelineExecution</a>, which returns information about a specific execution of a pipeline.</p> </li> <li> <p> <a>GetPipelineState</a>, which returns information about the current state of the stages and actions of a pipeline.</p> </li> <li> <p> <a>ListPipelines</a>, which gets a summary of all of the pipelines associated with your account.</p> </li> <li> <p> <a>ListPipelineExecutions</a>, which gets a summary of the most recent executions for a pipeline.</p> </li> <li> <p> <a>StartPipelineExecution</a>, which runs the the most recent revision of an artifact through the pipeline.</p> </li> <li> <p> <a>UpdatePipeline</a>, which updates a pipeline with edits or changes to the structure of the pipeline.</p> </li> </ul> <p>Pipelines include <i>stages</i>, which are logical groupings of gates and actions. Each stage contains one or more actions that must complete before the next stage begins. A stage will result in success or failure. If a stage fails, then the pipeline stops at that stage and will remain stopped until either a new version of an artifact appears in the source location, or a user takes action to re-run the most recent artifact through the pipeline. You can call <a>GetPipelineState</a>, which displays the status of a pipeline, including the status of stages in the pipeline, or <a>GetPipeline</a>, which returns the entire structure of the pipeline, including the stages of that pipeline. For more information about the structure of stages and actions, also refer to the <a href="http://docs.aws.amazon.com/codepipeline/latest/userguide/pipeline-structure.html">AWS CodePipeline Pipeline Structure Reference</a>.</p> <p>Pipeline stages include <i>actions</i>, which are categorized into categories such as source or build actions performed within a stage of a pipeline. For example, you can use a source action to import artifacts into a pipeline from a source such as Amazon S3. Like stages, you do not work with actions directly in most cases, but you do define and interact with actions when working with pipeline operations such as <a>CreatePipeline</a> and <a>GetPipelineState</a>. </p> <p>Pipelines also include <i>transitions</i>, which allow the transition of artifacts from one stage to the next in a pipeline after the actions in one stage complete.</p> <p>You can work with transitions by calling:</p> <ul> <li> <p> <a>DisableStageTransition</a>, which prevents artifacts from transitioning to the next stage in a pipeline.</p> </li> <li> <p> <a>EnableStageTransition</a>, which enables transition of artifacts between stages in a pipeline. </p> </li> </ul> <p> <b>Using the API to integrate with AWS CodePipeline</b> </p> <p>For third-party integrators or developers who want to create their own integrations with AWS CodePipeline, the expected sequence varies from the standard API user. In order to integrate with AWS CodePipeline, developers will need to work with the following items:</p> <p> <b>Jobs</b>, which are instances of an action. For example, a job for a source action might import a revision of an artifact from a source. </p> <p>You can work with jobs by calling:</p> <ul> <li> <p> <a>AcknowledgeJob</a>, which confirms whether a job worker has received the specified job,</p> </li> <li> <p> <a>GetJobDetails</a>, which returns the details of a job,</p> </li> <li> <p> <a>PollForJobs</a>, which determines whether there are any jobs to act upon, </p> </li> <li> <p> <a>PutJobFailureResult</a>, which provides details of a job failure, and</p> </li> <li> <p> <a>PutJobSuccessResult</a>, which provides details of a job success.</p> </li> </ul> <p> <b>Third party jobs</b>, which are instances of an action created by a partner action and integrated into AWS CodePipeline. Partner actions are created by members of the AWS Partner Network.</p> <p>You can work with third party jobs by calling:</p> <ul> <li> <p> <a>AcknowledgeThirdPartyJob</a>, which confirms whether a job worker has received the specified job,</p> </li> <li> <p> <a>GetThirdPartyJobDetails</a>, which requests the details of a job for a partner action,</p> </li> <li> <p> <a>PollForThirdPartyJobs</a>, which determines whether there are any jobs to act upon, </p> </li> <li> <p> <a>PutThirdPartyJobFailureResult</a>, which provides details of a job failure, and</p> </li> <li> <p> <a>PutThirdPartyJobSuccessResult</a>, which provides details of a job success.</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `acknowledgeJob`

``` purescript
acknowledgeJob :: forall eff. AcknowledgeJobInput -> Aff (err :: RequestError | eff) AcknowledgeJobOutput
```

<p>Returns information about a specified job and whether that job has been received by the job worker. Only used for custom actions.</p>

#### `acknowledgeThirdPartyJob`

``` purescript
acknowledgeThirdPartyJob :: forall eff. AcknowledgeThirdPartyJobInput -> Aff (err :: RequestError | eff) AcknowledgeThirdPartyJobOutput
```

<p>Confirms a job worker has received the specified job. Only used for partner actions.</p>

#### `createCustomActionType`

``` purescript
createCustomActionType :: forall eff. CreateCustomActionTypeInput -> Aff (err :: RequestError | eff) CreateCustomActionTypeOutput
```

<p>Creates a new custom action that can be used in all pipelines associated with the AWS account. Only used for custom actions.</p>

#### `createPipeline`

``` purescript
createPipeline :: forall eff. CreatePipelineInput -> Aff (err :: RequestError | eff) CreatePipelineOutput
```

<p>Creates a pipeline.</p>

#### `deleteCustomActionType`

``` purescript
deleteCustomActionType :: forall eff. DeleteCustomActionTypeInput -> Aff (err :: RequestError | eff) Unit
```

<p>Marks a custom action as deleted. PollForJobs for the custom action will fail after the action is marked for deletion. Only used for custom actions.</p> <important> <p>You cannot recreate a custom action after it has been deleted unless you increase the version number of the action.</p> </important>

#### `deletePipeline`

``` purescript
deletePipeline :: forall eff. DeletePipelineInput -> Aff (err :: RequestError | eff) Unit
```

<p>Deletes the specified pipeline.</p>

#### `disableStageTransition`

``` purescript
disableStageTransition :: forall eff. DisableStageTransitionInput -> Aff (err :: RequestError | eff) Unit
```

<p>Prevents artifacts in a pipeline from transitioning to the next stage in the pipeline.</p>

#### `enableStageTransition`

``` purescript
enableStageTransition :: forall eff. EnableStageTransitionInput -> Aff (err :: RequestError | eff) Unit
```

<p>Enables artifacts in a pipeline to transition to a stage in a pipeline.</p>

#### `getJobDetails`

``` purescript
getJobDetails :: forall eff. GetJobDetailsInput -> Aff (err :: RequestError | eff) GetJobDetailsOutput
```

<p>Returns information about a job. Only used for custom actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>

#### `getPipeline`

``` purescript
getPipeline :: forall eff. GetPipelineInput -> Aff (err :: RequestError | eff) GetPipelineOutput
```

<p>Returns the metadata, structure, stages, and actions of a pipeline. Can be used to return the entire structure of a pipeline in JSON format, which can then be modified and used to update the pipeline structure with <a>UpdatePipeline</a>.</p>

#### `getPipelineExecution`

``` purescript
getPipelineExecution :: forall eff. GetPipelineExecutionInput -> Aff (err :: RequestError | eff) GetPipelineExecutionOutput
```

<p>Returns information about an execution of a pipeline, including details about artifacts, the pipeline execution ID, and the name, version, and status of the pipeline.</p>

#### `getPipelineState`

``` purescript
getPipelineState :: forall eff. GetPipelineStateInput -> Aff (err :: RequestError | eff) GetPipelineStateOutput
```

<p>Returns information about the state of a pipeline, including the stages and actions.</p>

#### `getThirdPartyJobDetails`

``` purescript
getThirdPartyJobDetails :: forall eff. GetThirdPartyJobDetailsInput -> Aff (err :: RequestError | eff) GetThirdPartyJobDetailsOutput
```

<p>Requests the details of a job for a third party action. Only used for partner actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>

#### `listActionTypes`

``` purescript
listActionTypes :: forall eff. ListActionTypesInput -> Aff (err :: RequestError | eff) ListActionTypesOutput
```

<p>Gets a summary of all AWS CodePipeline action types associated with your account.</p>

#### `listPipelineExecutions`

``` purescript
listPipelineExecutions :: forall eff. ListPipelineExecutionsInput -> Aff (err :: RequestError | eff) ListPipelineExecutionsOutput
```

<p>Gets a summary of the most recent executions for a pipeline.</p>

#### `listPipelines`

``` purescript
listPipelines :: forall eff. ListPipelinesInput -> Aff (err :: RequestError | eff) ListPipelinesOutput
```

<p>Gets a summary of all of the pipelines associated with your account.</p>

#### `pollForJobs`

``` purescript
pollForJobs :: forall eff. PollForJobsInput -> Aff (err :: RequestError | eff) PollForJobsOutput
```

<p>Returns information about any jobs for AWS CodePipeline to act upon.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts. Additionally, this API returns any secret values defined for the action.</p> </important>

#### `pollForThirdPartyJobs`

``` purescript
pollForThirdPartyJobs :: forall eff. PollForThirdPartyJobsInput -> Aff (err :: RequestError | eff) PollForThirdPartyJobsOutput
```

<p>Determines whether there are any third party jobs for a job worker to act on. Only used for partner actions.</p> <important> <p>When this API is called, AWS CodePipeline returns temporary credentials for the Amazon S3 bucket used to store artifacts for the pipeline, if the action requires access to that Amazon S3 bucket for input or output artifacts.</p> </important>

#### `putActionRevision`

``` purescript
putActionRevision :: forall eff. PutActionRevisionInput -> Aff (err :: RequestError | eff) PutActionRevisionOutput
```

<p>Provides information to AWS CodePipeline about new revisions to a source.</p>

#### `putApprovalResult`

``` purescript
putApprovalResult :: forall eff. PutApprovalResultInput -> Aff (err :: RequestError | eff) PutApprovalResultOutput
```

<p>Provides the response to a manual approval request to AWS CodePipeline. Valid responses include Approved and Rejected.</p>

#### `putJobFailureResult`

``` purescript
putJobFailureResult :: forall eff. PutJobFailureResultInput -> Aff (err :: RequestError | eff) Unit
```

<p>Represents the failure of a job as returned to the pipeline by a job worker. Only used for custom actions.</p>

#### `putJobSuccessResult`

``` purescript
putJobSuccessResult :: forall eff. PutJobSuccessResultInput -> Aff (err :: RequestError | eff) Unit
```

<p>Represents the success of a job as returned to the pipeline by a job worker. Only used for custom actions.</p>

#### `putThirdPartyJobFailureResult`

``` purescript
putThirdPartyJobFailureResult :: forall eff. PutThirdPartyJobFailureResultInput -> Aff (err :: RequestError | eff) Unit
```

<p>Represents the failure of a third party job as returned to the pipeline by a job worker. Only used for partner actions.</p>

#### `putThirdPartyJobSuccessResult`

``` purescript
putThirdPartyJobSuccessResult :: forall eff. PutThirdPartyJobSuccessResultInput -> Aff (err :: RequestError | eff) Unit
```

<p>Represents the success of a third party job as returned to the pipeline by a job worker. Only used for partner actions.</p>

#### `retryStageExecution`

``` purescript
retryStageExecution :: forall eff. RetryStageExecutionInput -> Aff (err :: RequestError | eff) RetryStageExecutionOutput
```

<p>Resumes the pipeline execution by retrying the last failed actions in a stage.</p>

#### `startPipelineExecution`

``` purescript
startPipelineExecution :: forall eff. StartPipelineExecutionInput -> Aff (err :: RequestError | eff) StartPipelineExecutionOutput
```

<p>Starts the specified pipeline. Specifically, it begins processing the latest commit to the source location specified as part of the pipeline.</p>

#### `updatePipeline`

``` purescript
updatePipeline :: forall eff. UpdatePipelineInput -> Aff (err :: RequestError | eff) UpdatePipelineOutput
```

<p>Updates a specified pipeline with edits or changes to its structure. Use a JSON file with the pipeline structure in conjunction with UpdatePipeline to provide the full structure of the pipeline. Updating the pipeline increases the version number of the pipeline by 1.</p>

#### `AWSSessionCredentials`

``` purescript
newtype AWSSessionCredentials
  = AWSSessionCredentials { "AccessKeyId'" :: AccessKeyId, "SecretAccessKey'" :: SecretAccessKey, "SessionToken'" :: SessionToken }
```

<p>Represents an AWS session credentials object. These credentials are temporary credentials that are issued by AWS Secure Token Service (STS). They can be used to access input and output artifacts in the Amazon S3 bucket used to store artifact for the pipeline in AWS CodePipeline.</p>

#### `AccessKeyId`

``` purescript
newtype AccessKeyId
  = AccessKeyId String
```

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

#### `AcknowledgeJobInput`

``` purescript
newtype AcknowledgeJobInput
  = AcknowledgeJobInput { "JobId'" :: JobId, "Nonce'" :: Nonce }
```

<p>Represents the input of an AcknowledgeJob action.</p>

#### `AcknowledgeJobOutput`

``` purescript
newtype AcknowledgeJobOutput
  = AcknowledgeJobOutput { "Status'" :: NullOrUndefined (JobStatus) }
```

<p>Represents the output of an AcknowledgeJob action.</p>

#### `AcknowledgeThirdPartyJobInput`

``` purescript
newtype AcknowledgeThirdPartyJobInput
  = AcknowledgeThirdPartyJobInput { "JobId'" :: ThirdPartyJobId, "Nonce'" :: Nonce, "ClientToken'" :: ClientToken }
```

<p>Represents the input of an AcknowledgeThirdPartyJob action.</p>

#### `AcknowledgeThirdPartyJobOutput`

``` purescript
newtype AcknowledgeThirdPartyJobOutput
  = AcknowledgeThirdPartyJobOutput { "Status'" :: NullOrUndefined (JobStatus) }
```

<p>Represents the output of an AcknowledgeThirdPartyJob action.</p>

#### `ActionCategory`

``` purescript
newtype ActionCategory
  = ActionCategory String
```

#### `ActionConfiguration`

``` purescript
newtype ActionConfiguration
  = ActionConfiguration { "Configuration'" :: NullOrUndefined (ActionConfigurationMap) }
```

<p>Represents information about an action configuration.</p>

#### `ActionConfigurationKey`

``` purescript
newtype ActionConfigurationKey
  = ActionConfigurationKey String
```

#### `ActionConfigurationMap`

``` purescript
newtype ActionConfigurationMap
  = ActionConfigurationMap (Map ActionConfigurationKey ActionConfigurationValue)
```

#### `ActionConfigurationProperty`

``` purescript
newtype ActionConfigurationProperty
  = ActionConfigurationProperty { "Name'" :: ActionConfigurationKey, "Required'" :: Boolean, "Key'" :: Boolean, "Secret'" :: Boolean, "Queryable'" :: NullOrUndefined (Boolean), "Description'" :: NullOrUndefined (Description), "Type'" :: NullOrUndefined (ActionConfigurationPropertyType) }
```

<p>Represents information about an action configuration property.</p>

#### `ActionConfigurationPropertyList`

``` purescript
newtype ActionConfigurationPropertyList
  = ActionConfigurationPropertyList (Array ActionConfigurationProperty)
```

#### `ActionConfigurationPropertyType`

``` purescript
newtype ActionConfigurationPropertyType
  = ActionConfigurationPropertyType String
```

#### `ActionConfigurationQueryableValue`

``` purescript
newtype ActionConfigurationQueryableValue
  = ActionConfigurationQueryableValue String
```

#### `ActionConfigurationValue`

``` purescript
newtype ActionConfigurationValue
  = ActionConfigurationValue String
```

#### `ActionContext`

``` purescript
newtype ActionContext
  = ActionContext { "Name'" :: NullOrUndefined (ActionName) }
```

<p>Represents the context of an action within the stage of a pipeline to a job worker.</p>

#### `ActionDeclaration`

``` purescript
newtype ActionDeclaration
  = ActionDeclaration { "Name'" :: ActionName, "ActionTypeId'" :: ActionTypeId, "RunOrder'" :: NullOrUndefined (ActionRunOrder), "Configuration'" :: NullOrUndefined (ActionConfigurationMap), "OutputArtifacts'" :: NullOrUndefined (OutputArtifactList), "InputArtifacts'" :: NullOrUndefined (InputArtifactList), "RoleArn'" :: NullOrUndefined (RoleArn) }
```

<p>Represents information about an action declaration.</p>

#### `ActionExecution`

``` purescript
newtype ActionExecution
  = ActionExecution { "Status'" :: NullOrUndefined (ActionExecutionStatus), "Summary'" :: NullOrUndefined (ExecutionSummary), "LastStatusChange'" :: NullOrUndefined (Number), "Token'" :: NullOrUndefined (ActionExecutionToken), "LastUpdatedBy'" :: NullOrUndefined (LastUpdatedBy), "ExternalExecutionId'" :: NullOrUndefined (ExecutionId), "ExternalExecutionUrl'" :: NullOrUndefined (Url), "PercentComplete'" :: NullOrUndefined (Percentage), "ErrorDetails'" :: NullOrUndefined (ErrorDetails) }
```

<p>Represents information about the run of an action.</p>

#### `ActionExecutionStatus`

``` purescript
newtype ActionExecutionStatus
  = ActionExecutionStatus String
```

#### `ActionExecutionToken`

``` purescript
newtype ActionExecutionToken
  = ActionExecutionToken String
```

#### `ActionName`

``` purescript
newtype ActionName
  = ActionName String
```

#### `ActionNotFoundException`

``` purescript
newtype ActionNotFoundException
  = ActionNotFoundException {  }
```

<p>The specified action cannot be found.</p>

#### `ActionOwner`

``` purescript
newtype ActionOwner
  = ActionOwner String
```

#### `ActionProvider`

``` purescript
newtype ActionProvider
  = ActionProvider String
```

#### `ActionRevision`

``` purescript
newtype ActionRevision
  = ActionRevision { "RevisionId'" :: Revision, "RevisionChangeId'" :: RevisionChangeIdentifier, "Created'" :: Number }
```

<p>Represents information about the version (or revision) of an action.</p>

#### `ActionRunOrder`

``` purescript
newtype ActionRunOrder
  = ActionRunOrder Int
```

#### `ActionState`

``` purescript
newtype ActionState
  = ActionState { "ActionName'" :: NullOrUndefined (ActionName), "CurrentRevision'" :: NullOrUndefined (ActionRevision), "LatestExecution'" :: NullOrUndefined (ActionExecution), "EntityUrl'" :: NullOrUndefined (Url), "RevisionUrl'" :: NullOrUndefined (Url) }
```

<p>Represents information about the state of an action.</p>

#### `ActionStateList`

``` purescript
newtype ActionStateList
  = ActionStateList (Array ActionState)
```

#### `ActionType`

``` purescript
newtype ActionType
  = ActionType { "Id'" :: ActionTypeId, "Settings'" :: NullOrUndefined (ActionTypeSettings), "ActionConfigurationProperties'" :: NullOrUndefined (ActionConfigurationPropertyList), "InputArtifactDetails'" :: ArtifactDetails, "OutputArtifactDetails'" :: ArtifactDetails }
```

<p>Returns information about the details of an action type.</p>

#### `ActionTypeId`

``` purescript
newtype ActionTypeId
  = ActionTypeId { "Category'" :: ActionCategory, "Owner'" :: ActionOwner, "Provider'" :: ActionProvider, "Version'" :: Version }
```

<p>Represents information about an action type.</p>

#### `ActionTypeList`

``` purescript
newtype ActionTypeList
  = ActionTypeList (Array ActionType)
```

#### `ActionTypeNotFoundException`

``` purescript
newtype ActionTypeNotFoundException
  = ActionTypeNotFoundException {  }
```

<p>The specified action type cannot be found.</p>

#### `ActionTypeSettings`

``` purescript
newtype ActionTypeSettings
  = ActionTypeSettings { "ThirdPartyConfigurationUrl'" :: NullOrUndefined (Url), "EntityUrlTemplate'" :: NullOrUndefined (UrlTemplate), "ExecutionUrlTemplate'" :: NullOrUndefined (UrlTemplate), "RevisionUrlTemplate'" :: NullOrUndefined (UrlTemplate) }
```

<p>Returns information about the settings for an action type.</p>

#### `ApprovalAlreadyCompletedException`

``` purescript
newtype ApprovalAlreadyCompletedException
  = ApprovalAlreadyCompletedException {  }
```

<p>The approval action has already been approved or rejected.</p>

#### `ApprovalResult`

``` purescript
newtype ApprovalResult
  = ApprovalResult { "Summary'" :: ApprovalSummary, "Status'" :: ApprovalStatus }
```

<p>Represents information about the result of an approval request.</p>

#### `ApprovalStatus`

``` purescript
newtype ApprovalStatus
  = ApprovalStatus String
```

#### `ApprovalSummary`

``` purescript
newtype ApprovalSummary
  = ApprovalSummary String
```

#### `ApprovalToken`

``` purescript
newtype ApprovalToken
  = ApprovalToken String
```

#### `Artifact`

``` purescript
newtype Artifact
  = Artifact { "Name'" :: NullOrUndefined (ArtifactName), "Revision'" :: NullOrUndefined (Revision), "Location'" :: NullOrUndefined (ArtifactLocation) }
```

<p>Represents information about an artifact that will be worked upon by actions in the pipeline.</p>

#### `ArtifactDetails`

``` purescript
newtype ArtifactDetails
  = ArtifactDetails { "MinimumCount'" :: MinimumArtifactCount, "MaximumCount'" :: MaximumArtifactCount }
```

<p>Returns information about the details of an artifact.</p>

#### `ArtifactList`

``` purescript
newtype ArtifactList
  = ArtifactList (Array Artifact)
```

#### `ArtifactLocation`

``` purescript
newtype ArtifactLocation
  = ArtifactLocation { "Type'" :: NullOrUndefined (ArtifactLocationType), "S3Location'" :: NullOrUndefined (S3ArtifactLocation) }
```

<p>Represents information about the location of an artifact.</p>

#### `ArtifactLocationType`

``` purescript
newtype ArtifactLocationType
  = ArtifactLocationType String
```

#### `ArtifactName`

``` purescript
newtype ArtifactName
  = ArtifactName String
```

#### `ArtifactRevision`

``` purescript
newtype ArtifactRevision
  = ArtifactRevision { "Name'" :: NullOrUndefined (ArtifactName), "RevisionId'" :: NullOrUndefined (Revision), "RevisionChangeIdentifier'" :: NullOrUndefined (RevisionChangeIdentifier), "RevisionSummary'" :: NullOrUndefined (RevisionSummary), "Created'" :: NullOrUndefined (Number), "RevisionUrl'" :: NullOrUndefined (Url) }
```

<p>Represents revision details of an artifact. </p>

#### `ArtifactRevisionList`

``` purescript
newtype ArtifactRevisionList
  = ArtifactRevisionList (Array ArtifactRevision)
```

#### `ArtifactStore`

``` purescript
newtype ArtifactStore
  = ArtifactStore { "Type'" :: ArtifactStoreType, "Location'" :: ArtifactStoreLocation, "EncryptionKey'" :: NullOrUndefined (EncryptionKey) }
```

<p>The Amazon S3 bucket where artifacts are stored for the pipeline.</p>

#### `ArtifactStoreLocation`

``` purescript
newtype ArtifactStoreLocation
  = ArtifactStoreLocation String
```

#### `ArtifactStoreType`

``` purescript
newtype ArtifactStoreType
  = ArtifactStoreType String
```

#### `BlockerDeclaration`

``` purescript
newtype BlockerDeclaration
  = BlockerDeclaration { "Name'" :: BlockerName, "Type'" :: BlockerType }
```

<p>Reserved for future use.</p>

#### `BlockerName`

``` purescript
newtype BlockerName
  = BlockerName String
```

#### `BlockerType`

``` purescript
newtype BlockerType
  = BlockerType String
```

#### `ClientId`

``` purescript
newtype ClientId
  = ClientId String
```

#### `ClientToken`

``` purescript
newtype ClientToken
  = ClientToken String
```

#### `Code`

``` purescript
newtype Code
  = Code String
```

#### `ContinuationToken`

``` purescript
newtype ContinuationToken
  = ContinuationToken String
```

#### `CreateCustomActionTypeInput`

``` purescript
newtype CreateCustomActionTypeInput
  = CreateCustomActionTypeInput { "Category'" :: ActionCategory, "Provider'" :: ActionProvider, "Version'" :: Version, "Settings'" :: NullOrUndefined (ActionTypeSettings), "ConfigurationProperties'" :: NullOrUndefined (ActionConfigurationPropertyList), "InputArtifactDetails'" :: ArtifactDetails, "OutputArtifactDetails'" :: ArtifactDetails }
```

<p>Represents the input of a CreateCustomActionType operation.</p>

#### `CreateCustomActionTypeOutput`

``` purescript
newtype CreateCustomActionTypeOutput
  = CreateCustomActionTypeOutput { "ActionType'" :: ActionType }
```

<p>Represents the output of a CreateCustomActionType operation.</p>

#### `CreatePipelineInput`

``` purescript
newtype CreatePipelineInput
  = CreatePipelineInput { "Pipeline'" :: PipelineDeclaration }
```

<p>Represents the input of a CreatePipeline action.</p>

#### `CreatePipelineOutput`

``` purescript
newtype CreatePipelineOutput
  = CreatePipelineOutput { "Pipeline'" :: NullOrUndefined (PipelineDeclaration) }
```

<p>Represents the output of a CreatePipeline action.</p>

#### `CurrentRevision`

``` purescript
newtype CurrentRevision
  = CurrentRevision { "Revision'" :: Revision, "ChangeIdentifier'" :: RevisionChangeIdentifier, "Created'" :: NullOrUndefined (Time), "RevisionSummary'" :: NullOrUndefined (RevisionSummary) }
```

<p>Represents information about a current revision.</p>

#### `DeleteCustomActionTypeInput`

``` purescript
newtype DeleteCustomActionTypeInput
  = DeleteCustomActionTypeInput { "Category'" :: ActionCategory, "Provider'" :: ActionProvider, "Version'" :: Version }
```

<p>Represents the input of a DeleteCustomActionType operation. The custom action will be marked as deleted.</p>

#### `DeletePipelineInput`

``` purescript
newtype DeletePipelineInput
  = DeletePipelineInput { "Name'" :: PipelineName }
```

<p>Represents the input of a DeletePipeline action.</p>

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `DisableStageTransitionInput`

``` purescript
newtype DisableStageTransitionInput
  = DisableStageTransitionInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "TransitionType'" :: StageTransitionType, "Reason'" :: DisabledReason }
```

<p>Represents the input of a DisableStageTransition action.</p>

#### `DisabledReason`

``` purescript
newtype DisabledReason
  = DisabledReason String
```

#### `EnableStageTransitionInput`

``` purescript
newtype EnableStageTransitionInput
  = EnableStageTransitionInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "TransitionType'" :: StageTransitionType }
```

<p>Represents the input of an EnableStageTransition action.</p>

#### `Enabled`

``` purescript
newtype Enabled
  = Enabled Boolean
```

#### `EncryptionKey`

``` purescript
newtype EncryptionKey
  = EncryptionKey { "Id'" :: EncryptionKeyId, "Type'" :: EncryptionKeyType }
```

<p>Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.</p>

#### `EncryptionKeyId`

``` purescript
newtype EncryptionKeyId
  = EncryptionKeyId String
```

#### `EncryptionKeyType`

``` purescript
newtype EncryptionKeyType
  = EncryptionKeyType String
```

#### `ErrorDetails`

``` purescript
newtype ErrorDetails
  = ErrorDetails { "Code'" :: NullOrUndefined (Code), "Message'" :: NullOrUndefined (Message) }
```

<p>Represents information about an error in AWS CodePipeline.</p>

#### `ExecutionDetails`

``` purescript
newtype ExecutionDetails
  = ExecutionDetails { "Summary'" :: NullOrUndefined (ExecutionSummary), "ExternalExecutionId'" :: NullOrUndefined (ExecutionId), "PercentComplete'" :: NullOrUndefined (Percentage) }
```

<p>The details of the actions taken and results produced on an artifact as it passes through stages in the pipeline.</p>

#### `ExecutionId`

``` purescript
newtype ExecutionId
  = ExecutionId String
```

#### `ExecutionSummary`

``` purescript
newtype ExecutionSummary
  = ExecutionSummary String
```

#### `FailureDetails`

``` purescript
newtype FailureDetails
  = FailureDetails { "Type'" :: FailureType, "Message'" :: Message, "ExternalExecutionId'" :: NullOrUndefined (ExecutionId) }
```

<p>Represents information about failure details.</p>

#### `FailureType`

``` purescript
newtype FailureType
  = FailureType String
```

#### `GetJobDetailsInput`

``` purescript
newtype GetJobDetailsInput
  = GetJobDetailsInput { "JobId'" :: JobId }
```

<p>Represents the input of a GetJobDetails action.</p>

#### `GetJobDetailsOutput`

``` purescript
newtype GetJobDetailsOutput
  = GetJobDetailsOutput { "JobDetails'" :: NullOrUndefined (JobDetails) }
```

<p>Represents the output of a GetJobDetails action.</p>

#### `GetPipelineExecutionInput`

``` purescript
newtype GetPipelineExecutionInput
  = GetPipelineExecutionInput { "PipelineName'" :: PipelineName, "PipelineExecutionId'" :: PipelineExecutionId }
```

<p>Represents the input of a GetPipelineExecution action.</p>

#### `GetPipelineExecutionOutput`

``` purescript
newtype GetPipelineExecutionOutput
  = GetPipelineExecutionOutput { "PipelineExecution'" :: NullOrUndefined (PipelineExecution) }
```

<p>Represents the output of a GetPipelineExecution action.</p>

#### `GetPipelineInput`

``` purescript
newtype GetPipelineInput
  = GetPipelineInput { "Name'" :: PipelineName, "Version'" :: NullOrUndefined (PipelineVersion) }
```

<p>Represents the input of a GetPipeline action.</p>

#### `GetPipelineOutput`

``` purescript
newtype GetPipelineOutput
  = GetPipelineOutput { "Pipeline'" :: NullOrUndefined (PipelineDeclaration), "Metadata'" :: NullOrUndefined (PipelineMetadata) }
```

<p>Represents the output of a GetPipeline action.</p>

#### `GetPipelineStateInput`

``` purescript
newtype GetPipelineStateInput
  = GetPipelineStateInput { "Name'" :: PipelineName }
```

<p>Represents the input of a GetPipelineState action.</p>

#### `GetPipelineStateOutput`

``` purescript
newtype GetPipelineStateOutput
  = GetPipelineStateOutput { "PipelineName'" :: NullOrUndefined (PipelineName), "PipelineVersion'" :: NullOrUndefined (PipelineVersion), "StageStates'" :: NullOrUndefined (StageStateList), "Created'" :: NullOrUndefined (Number), "Updated'" :: NullOrUndefined (Number) }
```

<p>Represents the output of a GetPipelineState action.</p>

#### `GetThirdPartyJobDetailsInput`

``` purescript
newtype GetThirdPartyJobDetailsInput
  = GetThirdPartyJobDetailsInput { "JobId'" :: ThirdPartyJobId, "ClientToken'" :: ClientToken }
```

<p>Represents the input of a GetThirdPartyJobDetails action.</p>

#### `GetThirdPartyJobDetailsOutput`

``` purescript
newtype GetThirdPartyJobDetailsOutput
  = GetThirdPartyJobDetailsOutput { "JobDetails'" :: NullOrUndefined (ThirdPartyJobDetails) }
```

<p>Represents the output of a GetThirdPartyJobDetails action.</p>

#### `InputArtifact`

``` purescript
newtype InputArtifact
  = InputArtifact { "Name'" :: ArtifactName }
```

<p>Represents information about an artifact to be worked on, such as a test or build artifact.</p>

#### `InputArtifactList`

``` purescript
newtype InputArtifactList
  = InputArtifactList (Array InputArtifact)
```

#### `InvalidActionDeclarationException`

``` purescript
newtype InvalidActionDeclarationException
  = InvalidActionDeclarationException {  }
```

<p>The specified action declaration was specified in an invalid format.</p>

#### `InvalidApprovalTokenException`

``` purescript
newtype InvalidApprovalTokenException
  = InvalidApprovalTokenException {  }
```

<p>The approval request already received a response or has expired.</p>

#### `InvalidBlockerDeclarationException`

``` purescript
newtype InvalidBlockerDeclarationException
  = InvalidBlockerDeclarationException {  }
```

<p>Reserved for future use.</p>

#### `InvalidClientTokenException`

``` purescript
newtype InvalidClientTokenException
  = InvalidClientTokenException {  }
```

<p>The client token was specified in an invalid format</p>

#### `InvalidJobException`

``` purescript
newtype InvalidJobException
  = InvalidJobException {  }
```

<p>The specified job was specified in an invalid format or cannot be found.</p>

#### `InvalidJobStateException`

``` purescript
newtype InvalidJobStateException
  = InvalidJobStateException {  }
```

<p>The specified job state was specified in an invalid format.</p>

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException {  }
```

<p>The next token was specified in an invalid format. Make sure that the next token you provided is the token returned by a previous call.</p>

#### `InvalidNonceException`

``` purescript
newtype InvalidNonceException
  = InvalidNonceException {  }
```

<p>The specified nonce was specified in an invalid format.</p>

#### `InvalidStageDeclarationException`

``` purescript
newtype InvalidStageDeclarationException
  = InvalidStageDeclarationException {  }
```

<p>The specified stage declaration was specified in an invalid format.</p>

#### `InvalidStructureException`

``` purescript
newtype InvalidStructureException
  = InvalidStructureException {  }
```

<p>The specified structure was specified in an invalid format.</p>

#### `Job`

``` purescript
newtype Job
  = Job { "Id'" :: NullOrUndefined (JobId), "Data'" :: NullOrUndefined (JobData), "Nonce'" :: NullOrUndefined (Nonce), "AccountId'" :: NullOrUndefined (AccountId) }
```

<p>Represents information about a job.</p>

#### `JobData`

``` purescript
newtype JobData
  = JobData { "ActionTypeId'" :: NullOrUndefined (ActionTypeId), "ActionConfiguration'" :: NullOrUndefined (ActionConfiguration), "PipelineContext'" :: NullOrUndefined (PipelineContext), "InputArtifacts'" :: NullOrUndefined (ArtifactList), "OutputArtifacts'" :: NullOrUndefined (ArtifactList), "ArtifactCredentials'" :: NullOrUndefined (AWSSessionCredentials), "ContinuationToken'" :: NullOrUndefined (ContinuationToken), "EncryptionKey'" :: NullOrUndefined (EncryptionKey) }
```

<p>Represents additional information about a job required for a job worker to complete the job.</p>

#### `JobDetails`

``` purescript
newtype JobDetails
  = JobDetails { "Id'" :: NullOrUndefined (JobId), "Data'" :: NullOrUndefined (JobData), "AccountId'" :: NullOrUndefined (AccountId) }
```

<p>Represents information about the details of a job.</p>

#### `JobId`

``` purescript
newtype JobId
  = JobId String
```

#### `JobList`

``` purescript
newtype JobList
  = JobList (Array Job)
```

#### `JobNotFoundException`

``` purescript
newtype JobNotFoundException
  = JobNotFoundException {  }
```

<p>The specified job was specified in an invalid format or cannot be found.</p>

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

#### `LastChangedAt`

``` purescript
newtype LastChangedAt
  = LastChangedAt Number
```

#### `LastChangedBy`

``` purescript
newtype LastChangedBy
  = LastChangedBy String
```

#### `LastUpdatedBy`

``` purescript
newtype LastUpdatedBy
  = LastUpdatedBy String
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>The number of pipelines associated with the AWS account has exceeded the limit allowed for the account.</p>

#### `ListActionTypesInput`

``` purescript
newtype ListActionTypesInput
  = ListActionTypesInput { "ActionOwnerFilter'" :: NullOrUndefined (ActionOwner), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListActionTypes action.</p>

#### `ListActionTypesOutput`

``` purescript
newtype ListActionTypesOutput
  = ListActionTypesOutput { "ActionTypes'" :: ActionTypeList, "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListActionTypes action.</p>

#### `ListPipelineExecutionsInput`

``` purescript
newtype ListPipelineExecutionsInput
  = ListPipelineExecutionsInput { "PipelineName'" :: PipelineName, "MaxResults'" :: NullOrUndefined (MaxResults), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListPipelineExecutions action.</p>

#### `ListPipelineExecutionsOutput`

``` purescript
newtype ListPipelineExecutionsOutput
  = ListPipelineExecutionsOutput { "PipelineExecutionSummaries'" :: NullOrUndefined (PipelineExecutionSummaryList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListPipelineExecutions action.</p>

#### `ListPipelinesInput`

``` purescript
newtype ListPipelinesInput
  = ListPipelinesInput { "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListPipelines action.</p>

#### `ListPipelinesOutput`

``` purescript
newtype ListPipelinesOutput
  = ListPipelinesOutput { "Pipelines'" :: NullOrUndefined (PipelineList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListPipelines action.</p>

#### `MaxBatchSize`

``` purescript
newtype MaxBatchSize
  = MaxBatchSize Int
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

#### `MaximumArtifactCount`

``` purescript
newtype MaximumArtifactCount
  = MaximumArtifactCount Int
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

#### `MinimumArtifactCount`

``` purescript
newtype MinimumArtifactCount
  = MinimumArtifactCount Int
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `Nonce`

``` purescript
newtype Nonce
  = Nonce String
```

#### `NotLatestPipelineExecutionException`

``` purescript
newtype NotLatestPipelineExecutionException
  = NotLatestPipelineExecutionException {  }
```

<p>The stage has failed in a later run of the pipeline and the pipelineExecutionId associated with the request is out of date.</p>

#### `OutputArtifact`

``` purescript
newtype OutputArtifact
  = OutputArtifact { "Name'" :: ArtifactName }
```

<p>Represents information about the output of an action.</p>

#### `OutputArtifactList`

``` purescript
newtype OutputArtifactList
  = OutputArtifactList (Array OutputArtifact)
```

#### `Percentage`

``` purescript
newtype Percentage
  = Percentage Int
```

#### `PipelineArn`

``` purescript
newtype PipelineArn
  = PipelineArn String
```

#### `PipelineContext`

``` purescript
newtype PipelineContext
  = PipelineContext { "PipelineName'" :: NullOrUndefined (PipelineName), "Stage'" :: NullOrUndefined (StageContext), "Action'" :: NullOrUndefined (ActionContext) }
```

<p>Represents information about a pipeline to a job worker.</p>

#### `PipelineDeclaration`

``` purescript
newtype PipelineDeclaration
  = PipelineDeclaration { "Name'" :: PipelineName, "RoleArn'" :: RoleArn, "ArtifactStore'" :: ArtifactStore, "Stages'" :: PipelineStageDeclarationList, "Version'" :: NullOrUndefined (PipelineVersion) }
```

<p>Represents the structure of actions and stages to be performed in the pipeline.</p>

#### `PipelineExecution`

``` purescript
newtype PipelineExecution
  = PipelineExecution { "PipelineName'" :: NullOrUndefined (PipelineName), "PipelineVersion'" :: NullOrUndefined (PipelineVersion), "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId), "Status'" :: NullOrUndefined (PipelineExecutionStatus), "ArtifactRevisions'" :: NullOrUndefined (ArtifactRevisionList) }
```

<p>Represents information about an execution of a pipeline.</p>

#### `PipelineExecutionId`

``` purescript
newtype PipelineExecutionId
  = PipelineExecutionId String
```

#### `PipelineExecutionNotFoundException`

``` purescript
newtype PipelineExecutionNotFoundException
  = PipelineExecutionNotFoundException {  }
```

<p>The pipeline execution was specified in an invalid format or cannot be found, or an execution ID does not belong to the specified pipeline. </p>

#### `PipelineExecutionStatus`

``` purescript
newtype PipelineExecutionStatus
  = PipelineExecutionStatus String
```

#### `PipelineExecutionSummary`

``` purescript
newtype PipelineExecutionSummary
  = PipelineExecutionSummary { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId), "Status'" :: NullOrUndefined (PipelineExecutionStatus), "StartTime'" :: NullOrUndefined (Number), "LastUpdateTime'" :: NullOrUndefined (Number) }
```

<p>Summary information about a pipeline execution.</p>

#### `PipelineExecutionSummaryList`

``` purescript
newtype PipelineExecutionSummaryList
  = PipelineExecutionSummaryList (Array PipelineExecutionSummary)
```

#### `PipelineList`

``` purescript
newtype PipelineList
  = PipelineList (Array PipelineSummary)
```

#### `PipelineMetadata`

``` purescript
newtype PipelineMetadata
  = PipelineMetadata { "PipelineArn'" :: NullOrUndefined (PipelineArn), "Created'" :: NullOrUndefined (Number), "Updated'" :: NullOrUndefined (Number) }
```

<p>Information about a pipeline.</p>

#### `PipelineName`

``` purescript
newtype PipelineName
  = PipelineName String
```

#### `PipelineNameInUseException`

``` purescript
newtype PipelineNameInUseException
  = PipelineNameInUseException {  }
```

<p>The specified pipeline name is already in use.</p>

#### `PipelineNotFoundException`

``` purescript
newtype PipelineNotFoundException
  = PipelineNotFoundException {  }
```

<p>The specified pipeline was specified in an invalid format or cannot be found.</p>

#### `PipelineStageDeclarationList`

``` purescript
newtype PipelineStageDeclarationList
  = PipelineStageDeclarationList (Array StageDeclaration)
```

#### `PipelineSummary`

``` purescript
newtype PipelineSummary
  = PipelineSummary { "Name'" :: NullOrUndefined (PipelineName), "Version'" :: NullOrUndefined (PipelineVersion), "Created'" :: NullOrUndefined (Number), "Updated'" :: NullOrUndefined (Number) }
```

<p>Returns a summary of a pipeline.</p>

#### `PipelineVersion`

``` purescript
newtype PipelineVersion
  = PipelineVersion Int
```

#### `PipelineVersionNotFoundException`

``` purescript
newtype PipelineVersionNotFoundException
  = PipelineVersionNotFoundException {  }
```

<p>The specified pipeline version was specified in an invalid format or cannot be found.</p>

#### `PollForJobsInput`

``` purescript
newtype PollForJobsInput
  = PollForJobsInput { "ActionTypeId'" :: ActionTypeId, "MaxBatchSize'" :: NullOrUndefined (MaxBatchSize), "QueryParam'" :: NullOrUndefined (QueryParamMap) }
```

<p>Represents the input of a PollForJobs action.</p>

#### `PollForJobsOutput`

``` purescript
newtype PollForJobsOutput
  = PollForJobsOutput { "Jobs'" :: NullOrUndefined (JobList) }
```

<p>Represents the output of a PollForJobs action.</p>

#### `PollForThirdPartyJobsInput`

``` purescript
newtype PollForThirdPartyJobsInput
  = PollForThirdPartyJobsInput { "ActionTypeId'" :: ActionTypeId, "MaxBatchSize'" :: NullOrUndefined (MaxBatchSize) }
```

<p>Represents the input of a PollForThirdPartyJobs action.</p>

#### `PollForThirdPartyJobsOutput`

``` purescript
newtype PollForThirdPartyJobsOutput
  = PollForThirdPartyJobsOutput { "Jobs'" :: NullOrUndefined (ThirdPartyJobList) }
```

<p>Represents the output of a PollForThirdPartyJobs action.</p>

#### `PutActionRevisionInput`

``` purescript
newtype PutActionRevisionInput
  = PutActionRevisionInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "ActionName'" :: ActionName, "ActionRevision'" :: ActionRevision }
```

<p>Represents the input of a PutActionRevision action.</p>

#### `PutActionRevisionOutput`

``` purescript
newtype PutActionRevisionOutput
  = PutActionRevisionOutput { "NewRevision'" :: NullOrUndefined (Boolean), "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId) }
```

<p>Represents the output of a PutActionRevision action.</p>

#### `PutApprovalResultInput`

``` purescript
newtype PutApprovalResultInput
  = PutApprovalResultInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "ActionName'" :: ActionName, "Result'" :: ApprovalResult, "Token'" :: ApprovalToken }
```

<p>Represents the input of a PutApprovalResult action.</p>

#### `PutApprovalResultOutput`

``` purescript
newtype PutApprovalResultOutput
  = PutApprovalResultOutput { "ApprovedAt'" :: NullOrUndefined (Number) }
```

<p>Represents the output of a PutApprovalResult action.</p>

#### `PutJobFailureResultInput`

``` purescript
newtype PutJobFailureResultInput
  = PutJobFailureResultInput { "JobId'" :: JobId, "FailureDetails'" :: FailureDetails }
```

<p>Represents the input of a PutJobFailureResult action.</p>

#### `PutJobSuccessResultInput`

``` purescript
newtype PutJobSuccessResultInput
  = PutJobSuccessResultInput { "JobId'" :: JobId, "CurrentRevision'" :: NullOrUndefined (CurrentRevision), "ContinuationToken'" :: NullOrUndefined (ContinuationToken), "ExecutionDetails'" :: NullOrUndefined (ExecutionDetails) }
```

<p>Represents the input of a PutJobSuccessResult action.</p>

#### `PutThirdPartyJobFailureResultInput`

``` purescript
newtype PutThirdPartyJobFailureResultInput
  = PutThirdPartyJobFailureResultInput { "JobId'" :: ThirdPartyJobId, "ClientToken'" :: ClientToken, "FailureDetails'" :: FailureDetails }
```

<p>Represents the input of a PutThirdPartyJobFailureResult action.</p>

#### `PutThirdPartyJobSuccessResultInput`

``` purescript
newtype PutThirdPartyJobSuccessResultInput
  = PutThirdPartyJobSuccessResultInput { "JobId'" :: ThirdPartyJobId, "ClientToken'" :: ClientToken, "CurrentRevision'" :: NullOrUndefined (CurrentRevision), "ContinuationToken'" :: NullOrUndefined (ContinuationToken), "ExecutionDetails'" :: NullOrUndefined (ExecutionDetails) }
```

<p>Represents the input of a PutThirdPartyJobSuccessResult action.</p>

#### `QueryParamMap`

``` purescript
newtype QueryParamMap
  = QueryParamMap (Map ActionConfigurationKey ActionConfigurationQueryableValue)
```

#### `RetryStageExecutionInput`

``` purescript
newtype RetryStageExecutionInput
  = RetryStageExecutionInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "PipelineExecutionId'" :: PipelineExecutionId, "RetryMode'" :: StageRetryMode }
```

<p>Represents the input of a RetryStageExecution action.</p>

#### `RetryStageExecutionOutput`

``` purescript
newtype RetryStageExecutionOutput
  = RetryStageExecutionOutput { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId) }
```

<p>Represents the output of a RetryStageExecution action.</p>

#### `Revision`

``` purescript
newtype Revision
  = Revision String
```

#### `RevisionChangeIdentifier`

``` purescript
newtype RevisionChangeIdentifier
  = RevisionChangeIdentifier String
```

#### `RevisionSummary`

``` purescript
newtype RevisionSummary
  = RevisionSummary String
```

#### `RoleArn`

``` purescript
newtype RoleArn
  = RoleArn String
```

#### `S3ArtifactLocation`

``` purescript
newtype S3ArtifactLocation
  = S3ArtifactLocation { "BucketName'" :: S3BucketName, "ObjectKey'" :: S3ObjectKey }
```

<p>The location of the Amazon S3 bucket that contains a revision.</p>

#### `S3BucketName`

``` purescript
newtype S3BucketName
  = S3BucketName String
```

#### `S3ObjectKey`

``` purescript
newtype S3ObjectKey
  = S3ObjectKey String
```

#### `SecretAccessKey`

``` purescript
newtype SecretAccessKey
  = SecretAccessKey String
```

#### `SessionToken`

``` purescript
newtype SessionToken
  = SessionToken String
```

#### `StageActionDeclarationList`

``` purescript
newtype StageActionDeclarationList
  = StageActionDeclarationList (Array ActionDeclaration)
```

#### `StageBlockerDeclarationList`

``` purescript
newtype StageBlockerDeclarationList
  = StageBlockerDeclarationList (Array BlockerDeclaration)
```

#### `StageContext`

``` purescript
newtype StageContext
  = StageContext { "Name'" :: NullOrUndefined (StageName) }
```

<p>Represents information about a stage to a job worker.</p>

#### `StageDeclaration`

``` purescript
newtype StageDeclaration
  = StageDeclaration { "Name'" :: StageName, "Blockers'" :: NullOrUndefined (StageBlockerDeclarationList), "Actions'" :: StageActionDeclarationList }
```

<p>Represents information about a stage and its definition.</p>

#### `StageExecution`

``` purescript
newtype StageExecution
  = StageExecution { "PipelineExecutionId'" :: PipelineExecutionId, "Status'" :: StageExecutionStatus }
```

<p>Represents information about the run of a stage.</p>

#### `StageExecutionStatus`

``` purescript
newtype StageExecutionStatus
  = StageExecutionStatus String
```

#### `StageName`

``` purescript
newtype StageName
  = StageName String
```

#### `StageNotFoundException`

``` purescript
newtype StageNotFoundException
  = StageNotFoundException {  }
```

<p>The specified stage was specified in an invalid format or cannot be found.</p>

#### `StageNotRetryableException`

``` purescript
newtype StageNotRetryableException
  = StageNotRetryableException {  }
```

<p>The specified stage can't be retried because the pipeline structure or stage state changed after the stage was not completed; the stage contains no failed actions; one or more actions are still in progress; or another retry attempt is already in progress. </p>

#### `StageRetryMode`

``` purescript
newtype StageRetryMode
  = StageRetryMode String
```

#### `StageState`

``` purescript
newtype StageState
  = StageState { "StageName'" :: NullOrUndefined (StageName), "InboundTransitionState'" :: NullOrUndefined (TransitionState), "ActionStates'" :: NullOrUndefined (ActionStateList), "LatestExecution'" :: NullOrUndefined (StageExecution) }
```

<p>Represents information about the state of the stage.</p>

#### `StageStateList`

``` purescript
newtype StageStateList
  = StageStateList (Array StageState)
```

#### `StageTransitionType`

``` purescript
newtype StageTransitionType
  = StageTransitionType String
```

#### `StartPipelineExecutionInput`

``` purescript
newtype StartPipelineExecutionInput
  = StartPipelineExecutionInput { "Name'" :: PipelineName }
```

<p>Represents the input of a StartPipelineExecution action.</p>

#### `StartPipelineExecutionOutput`

``` purescript
newtype StartPipelineExecutionOutput
  = StartPipelineExecutionOutput { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId) }
```

<p>Represents the output of a StartPipelineExecution action.</p>

#### `ThirdPartyJob`

``` purescript
newtype ThirdPartyJob
  = ThirdPartyJob { "ClientId'" :: NullOrUndefined (ClientId), "JobId'" :: NullOrUndefined (JobId) }
```

<p>A response to a PollForThirdPartyJobs request returned by AWS CodePipeline when there is a job to be worked upon by a partner action.</p>

#### `ThirdPartyJobData`

``` purescript
newtype ThirdPartyJobData
  = ThirdPartyJobData { "ActionTypeId'" :: NullOrUndefined (ActionTypeId), "ActionConfiguration'" :: NullOrUndefined (ActionConfiguration), "PipelineContext'" :: NullOrUndefined (PipelineContext), "InputArtifacts'" :: NullOrUndefined (ArtifactList), "OutputArtifacts'" :: NullOrUndefined (ArtifactList), "ArtifactCredentials'" :: NullOrUndefined (AWSSessionCredentials), "ContinuationToken'" :: NullOrUndefined (ContinuationToken), "EncryptionKey'" :: NullOrUndefined (EncryptionKey) }
```

<p>Represents information about the job data for a partner action.</p>

#### `ThirdPartyJobDetails`

``` purescript
newtype ThirdPartyJobDetails
  = ThirdPartyJobDetails { "Id'" :: NullOrUndefined (ThirdPartyJobId), "Data'" :: NullOrUndefined (ThirdPartyJobData), "Nonce'" :: NullOrUndefined (Nonce) }
```

<p>The details of a job sent in response to a GetThirdPartyJobDetails request.</p>

#### `ThirdPartyJobId`

``` purescript
newtype ThirdPartyJobId
  = ThirdPartyJobId String
```

#### `ThirdPartyJobList`

``` purescript
newtype ThirdPartyJobList
  = ThirdPartyJobList (Array ThirdPartyJob)
```

#### `Time`

``` purescript
newtype Time
  = Time Number
```

#### `TransitionState`

``` purescript
newtype TransitionState
  = TransitionState { "Enabled'" :: NullOrUndefined (Enabled), "LastChangedBy'" :: NullOrUndefined (LastChangedBy), "LastChangedAt'" :: NullOrUndefined (LastChangedAt), "DisabledReason'" :: NullOrUndefined (DisabledReason) }
```

<p>Represents information about the state of transitions between one stage and another stage.</p>

#### `UpdatePipelineInput`

``` purescript
newtype UpdatePipelineInput
  = UpdatePipelineInput { "Pipeline'" :: PipelineDeclaration }
```

<p>Represents the input of an UpdatePipeline action.</p>

#### `UpdatePipelineOutput`

``` purescript
newtype UpdatePipelineOutput
  = UpdatePipelineOutput { "Pipeline'" :: NullOrUndefined (PipelineDeclaration) }
```

<p>Represents the output of an UpdatePipeline action.</p>

#### `Url`

``` purescript
newtype Url
  = Url String
```

#### `UrlTemplate`

``` purescript
newtype UrlTemplate
  = UrlTemplate String
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException {  }
```

<p>The validation was specified in an invalid format.</p>

#### `Version`

``` purescript
newtype Version
  = Version String
```


