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

##### Instances
``` purescript
Newtype AWSSessionCredentials _
```

#### `AccessKeyId`

``` purescript
newtype AccessKeyId
  = AccessKeyId String
```

##### Instances
``` purescript
Newtype AccessKeyId _
```

#### `AccountId`

``` purescript
newtype AccountId
  = AccountId String
```

##### Instances
``` purescript
Newtype AccountId _
```

#### `AcknowledgeJobInput`

``` purescript
newtype AcknowledgeJobInput
  = AcknowledgeJobInput { "JobId'" :: JobId, "Nonce'" :: Nonce }
```

<p>Represents the input of an AcknowledgeJob action.</p>

##### Instances
``` purescript
Newtype AcknowledgeJobInput _
```

#### `AcknowledgeJobOutput`

``` purescript
newtype AcknowledgeJobOutput
  = AcknowledgeJobOutput { "Status'" :: NullOrUndefined (JobStatus) }
```

<p>Represents the output of an AcknowledgeJob action.</p>

##### Instances
``` purescript
Newtype AcknowledgeJobOutput _
```

#### `AcknowledgeThirdPartyJobInput`

``` purescript
newtype AcknowledgeThirdPartyJobInput
  = AcknowledgeThirdPartyJobInput { "JobId'" :: ThirdPartyJobId, "Nonce'" :: Nonce, "ClientToken'" :: ClientToken }
```

<p>Represents the input of an AcknowledgeThirdPartyJob action.</p>

##### Instances
``` purescript
Newtype AcknowledgeThirdPartyJobInput _
```

#### `AcknowledgeThirdPartyJobOutput`

``` purescript
newtype AcknowledgeThirdPartyJobOutput
  = AcknowledgeThirdPartyJobOutput { "Status'" :: NullOrUndefined (JobStatus) }
```

<p>Represents the output of an AcknowledgeThirdPartyJob action.</p>

##### Instances
``` purescript
Newtype AcknowledgeThirdPartyJobOutput _
```

#### `ActionCategory`

``` purescript
newtype ActionCategory
  = ActionCategory String
```

##### Instances
``` purescript
Newtype ActionCategory _
```

#### `ActionConfiguration`

``` purescript
newtype ActionConfiguration
  = ActionConfiguration { "Configuration'" :: NullOrUndefined (ActionConfigurationMap) }
```

<p>Represents information about an action configuration.</p>

##### Instances
``` purescript
Newtype ActionConfiguration _
```

#### `ActionConfigurationKey`

``` purescript
newtype ActionConfigurationKey
  = ActionConfigurationKey String
```

##### Instances
``` purescript
Newtype ActionConfigurationKey _
```

#### `ActionConfigurationMap`

``` purescript
newtype ActionConfigurationMap
  = ActionConfigurationMap (Map ActionConfigurationKey ActionConfigurationValue)
```

##### Instances
``` purescript
Newtype ActionConfigurationMap _
```

#### `ActionConfigurationProperty`

``` purescript
newtype ActionConfigurationProperty
  = ActionConfigurationProperty { "Name'" :: ActionConfigurationKey, "Required'" :: Boolean, "Key'" :: Boolean, "Secret'" :: Boolean, "Queryable'" :: NullOrUndefined (Boolean), "Description'" :: NullOrUndefined (Description), "Type'" :: NullOrUndefined (ActionConfigurationPropertyType) }
```

<p>Represents information about an action configuration property.</p>

##### Instances
``` purescript
Newtype ActionConfigurationProperty _
```

#### `ActionConfigurationPropertyList`

``` purescript
newtype ActionConfigurationPropertyList
  = ActionConfigurationPropertyList (Array ActionConfigurationProperty)
```

##### Instances
``` purescript
Newtype ActionConfigurationPropertyList _
```

#### `ActionConfigurationPropertyType`

``` purescript
newtype ActionConfigurationPropertyType
  = ActionConfigurationPropertyType String
```

##### Instances
``` purescript
Newtype ActionConfigurationPropertyType _
```

#### `ActionConfigurationQueryableValue`

``` purescript
newtype ActionConfigurationQueryableValue
  = ActionConfigurationQueryableValue String
```

##### Instances
``` purescript
Newtype ActionConfigurationQueryableValue _
```

#### `ActionConfigurationValue`

``` purescript
newtype ActionConfigurationValue
  = ActionConfigurationValue String
```

##### Instances
``` purescript
Newtype ActionConfigurationValue _
```

#### `ActionContext`

``` purescript
newtype ActionContext
  = ActionContext { "Name'" :: NullOrUndefined (ActionName) }
```

<p>Represents the context of an action within the stage of a pipeline to a job worker.</p>

##### Instances
``` purescript
Newtype ActionContext _
```

#### `ActionDeclaration`

``` purescript
newtype ActionDeclaration
  = ActionDeclaration { "Name'" :: ActionName, "ActionTypeId'" :: ActionTypeId, "RunOrder'" :: NullOrUndefined (ActionRunOrder), "Configuration'" :: NullOrUndefined (ActionConfigurationMap), "OutputArtifacts'" :: NullOrUndefined (OutputArtifactList), "InputArtifacts'" :: NullOrUndefined (InputArtifactList), "RoleArn'" :: NullOrUndefined (RoleArn) }
```

<p>Represents information about an action declaration.</p>

##### Instances
``` purescript
Newtype ActionDeclaration _
```

#### `ActionExecution`

``` purescript
newtype ActionExecution
  = ActionExecution { "Status'" :: NullOrUndefined (ActionExecutionStatus), "Summary'" :: NullOrUndefined (ExecutionSummary), "LastStatusChange'" :: NullOrUndefined (Number), "Token'" :: NullOrUndefined (ActionExecutionToken), "LastUpdatedBy'" :: NullOrUndefined (LastUpdatedBy), "ExternalExecutionId'" :: NullOrUndefined (ExecutionId), "ExternalExecutionUrl'" :: NullOrUndefined (Url), "PercentComplete'" :: NullOrUndefined (Percentage), "ErrorDetails'" :: NullOrUndefined (ErrorDetails) }
```

<p>Represents information about the run of an action.</p>

##### Instances
``` purescript
Newtype ActionExecution _
```

#### `ActionExecutionStatus`

``` purescript
newtype ActionExecutionStatus
  = ActionExecutionStatus String
```

##### Instances
``` purescript
Newtype ActionExecutionStatus _
```

#### `ActionExecutionToken`

``` purescript
newtype ActionExecutionToken
  = ActionExecutionToken String
```

##### Instances
``` purescript
Newtype ActionExecutionToken _
```

#### `ActionName`

``` purescript
newtype ActionName
  = ActionName String
```

##### Instances
``` purescript
Newtype ActionName _
```

#### `ActionNotFoundException`

``` purescript
newtype ActionNotFoundException
  = ActionNotFoundException {  }
```

<p>The specified action cannot be found.</p>

##### Instances
``` purescript
Newtype ActionNotFoundException _
```

#### `ActionOwner`

``` purescript
newtype ActionOwner
  = ActionOwner String
```

##### Instances
``` purescript
Newtype ActionOwner _
```

#### `ActionProvider`

``` purescript
newtype ActionProvider
  = ActionProvider String
```

##### Instances
``` purescript
Newtype ActionProvider _
```

#### `ActionRevision`

``` purescript
newtype ActionRevision
  = ActionRevision { "RevisionId'" :: Revision, "RevisionChangeId'" :: RevisionChangeIdentifier, "Created'" :: Number }
```

<p>Represents information about the version (or revision) of an action.</p>

##### Instances
``` purescript
Newtype ActionRevision _
```

#### `ActionRunOrder`

``` purescript
newtype ActionRunOrder
  = ActionRunOrder Int
```

##### Instances
``` purescript
Newtype ActionRunOrder _
```

#### `ActionState`

``` purescript
newtype ActionState
  = ActionState { "ActionName'" :: NullOrUndefined (ActionName), "CurrentRevision'" :: NullOrUndefined (ActionRevision), "LatestExecution'" :: NullOrUndefined (ActionExecution), "EntityUrl'" :: NullOrUndefined (Url), "RevisionUrl'" :: NullOrUndefined (Url) }
```

<p>Represents information about the state of an action.</p>

##### Instances
``` purescript
Newtype ActionState _
```

#### `ActionStateList`

``` purescript
newtype ActionStateList
  = ActionStateList (Array ActionState)
```

##### Instances
``` purescript
Newtype ActionStateList _
```

#### `ActionType`

``` purescript
newtype ActionType
  = ActionType { "Id'" :: ActionTypeId, "Settings'" :: NullOrUndefined (ActionTypeSettings), "ActionConfigurationProperties'" :: NullOrUndefined (ActionConfigurationPropertyList), "InputArtifactDetails'" :: ArtifactDetails, "OutputArtifactDetails'" :: ArtifactDetails }
```

<p>Returns information about the details of an action type.</p>

##### Instances
``` purescript
Newtype ActionType _
```

#### `ActionTypeId`

``` purescript
newtype ActionTypeId
  = ActionTypeId { "Category'" :: ActionCategory, "Owner'" :: ActionOwner, "Provider'" :: ActionProvider, "Version'" :: Version }
```

<p>Represents information about an action type.</p>

##### Instances
``` purescript
Newtype ActionTypeId _
```

#### `ActionTypeList`

``` purescript
newtype ActionTypeList
  = ActionTypeList (Array ActionType)
```

##### Instances
``` purescript
Newtype ActionTypeList _
```

#### `ActionTypeNotFoundException`

``` purescript
newtype ActionTypeNotFoundException
  = ActionTypeNotFoundException {  }
```

<p>The specified action type cannot be found.</p>

##### Instances
``` purescript
Newtype ActionTypeNotFoundException _
```

#### `ActionTypeSettings`

``` purescript
newtype ActionTypeSettings
  = ActionTypeSettings { "ThirdPartyConfigurationUrl'" :: NullOrUndefined (Url), "EntityUrlTemplate'" :: NullOrUndefined (UrlTemplate), "ExecutionUrlTemplate'" :: NullOrUndefined (UrlTemplate), "RevisionUrlTemplate'" :: NullOrUndefined (UrlTemplate) }
```

<p>Returns information about the settings for an action type.</p>

##### Instances
``` purescript
Newtype ActionTypeSettings _
```

#### `ApprovalAlreadyCompletedException`

``` purescript
newtype ApprovalAlreadyCompletedException
  = ApprovalAlreadyCompletedException {  }
```

<p>The approval action has already been approved or rejected.</p>

##### Instances
``` purescript
Newtype ApprovalAlreadyCompletedException _
```

#### `ApprovalResult`

``` purescript
newtype ApprovalResult
  = ApprovalResult { "Summary'" :: ApprovalSummary, "Status'" :: ApprovalStatus }
```

<p>Represents information about the result of an approval request.</p>

##### Instances
``` purescript
Newtype ApprovalResult _
```

#### `ApprovalStatus`

``` purescript
newtype ApprovalStatus
  = ApprovalStatus String
```

##### Instances
``` purescript
Newtype ApprovalStatus _
```

#### `ApprovalSummary`

``` purescript
newtype ApprovalSummary
  = ApprovalSummary String
```

##### Instances
``` purescript
Newtype ApprovalSummary _
```

#### `ApprovalToken`

``` purescript
newtype ApprovalToken
  = ApprovalToken String
```

##### Instances
``` purescript
Newtype ApprovalToken _
```

#### `Artifact`

``` purescript
newtype Artifact
  = Artifact { "Name'" :: NullOrUndefined (ArtifactName), "Revision'" :: NullOrUndefined (Revision), "Location'" :: NullOrUndefined (ArtifactLocation) }
```

<p>Represents information about an artifact that will be worked upon by actions in the pipeline.</p>

##### Instances
``` purescript
Newtype Artifact _
```

#### `ArtifactDetails`

``` purescript
newtype ArtifactDetails
  = ArtifactDetails { "MinimumCount'" :: MinimumArtifactCount, "MaximumCount'" :: MaximumArtifactCount }
```

<p>Returns information about the details of an artifact.</p>

##### Instances
``` purescript
Newtype ArtifactDetails _
```

#### `ArtifactList`

``` purescript
newtype ArtifactList
  = ArtifactList (Array Artifact)
```

##### Instances
``` purescript
Newtype ArtifactList _
```

#### `ArtifactLocation`

``` purescript
newtype ArtifactLocation
  = ArtifactLocation { "Type'" :: NullOrUndefined (ArtifactLocationType), "S3Location'" :: NullOrUndefined (S3ArtifactLocation) }
```

<p>Represents information about the location of an artifact.</p>

##### Instances
``` purescript
Newtype ArtifactLocation _
```

#### `ArtifactLocationType`

``` purescript
newtype ArtifactLocationType
  = ArtifactLocationType String
```

##### Instances
``` purescript
Newtype ArtifactLocationType _
```

#### `ArtifactName`

``` purescript
newtype ArtifactName
  = ArtifactName String
```

##### Instances
``` purescript
Newtype ArtifactName _
```

#### `ArtifactRevision`

``` purescript
newtype ArtifactRevision
  = ArtifactRevision { "Name'" :: NullOrUndefined (ArtifactName), "RevisionId'" :: NullOrUndefined (Revision), "RevisionChangeIdentifier'" :: NullOrUndefined (RevisionChangeIdentifier), "RevisionSummary'" :: NullOrUndefined (RevisionSummary), "Created'" :: NullOrUndefined (Number), "RevisionUrl'" :: NullOrUndefined (Url) }
```

<p>Represents revision details of an artifact. </p>

##### Instances
``` purescript
Newtype ArtifactRevision _
```

#### `ArtifactRevisionList`

``` purescript
newtype ArtifactRevisionList
  = ArtifactRevisionList (Array ArtifactRevision)
```

##### Instances
``` purescript
Newtype ArtifactRevisionList _
```

#### `ArtifactStore`

``` purescript
newtype ArtifactStore
  = ArtifactStore { "Type'" :: ArtifactStoreType, "Location'" :: ArtifactStoreLocation, "EncryptionKey'" :: NullOrUndefined (EncryptionKey) }
```

<p>The Amazon S3 bucket where artifacts are stored for the pipeline.</p>

##### Instances
``` purescript
Newtype ArtifactStore _
```

#### `ArtifactStoreLocation`

``` purescript
newtype ArtifactStoreLocation
  = ArtifactStoreLocation String
```

##### Instances
``` purescript
Newtype ArtifactStoreLocation _
```

#### `ArtifactStoreType`

``` purescript
newtype ArtifactStoreType
  = ArtifactStoreType String
```

##### Instances
``` purescript
Newtype ArtifactStoreType _
```

#### `BlockerDeclaration`

``` purescript
newtype BlockerDeclaration
  = BlockerDeclaration { "Name'" :: BlockerName, "Type'" :: BlockerType }
```

<p>Reserved for future use.</p>

##### Instances
``` purescript
Newtype BlockerDeclaration _
```

#### `BlockerName`

``` purescript
newtype BlockerName
  = BlockerName String
```

##### Instances
``` purescript
Newtype BlockerName _
```

#### `BlockerType`

``` purescript
newtype BlockerType
  = BlockerType String
```

##### Instances
``` purescript
Newtype BlockerType _
```

#### `ClientId`

``` purescript
newtype ClientId
  = ClientId String
```

##### Instances
``` purescript
Newtype ClientId _
```

#### `ClientToken`

``` purescript
newtype ClientToken
  = ClientToken String
```

##### Instances
``` purescript
Newtype ClientToken _
```

#### `Code`

``` purescript
newtype Code
  = Code String
```

##### Instances
``` purescript
Newtype Code _
```

#### `ContinuationToken`

``` purescript
newtype ContinuationToken
  = ContinuationToken String
```

##### Instances
``` purescript
Newtype ContinuationToken _
```

#### `CreateCustomActionTypeInput`

``` purescript
newtype CreateCustomActionTypeInput
  = CreateCustomActionTypeInput { "Category'" :: ActionCategory, "Provider'" :: ActionProvider, "Version'" :: Version, "Settings'" :: NullOrUndefined (ActionTypeSettings), "ConfigurationProperties'" :: NullOrUndefined (ActionConfigurationPropertyList), "InputArtifactDetails'" :: ArtifactDetails, "OutputArtifactDetails'" :: ArtifactDetails }
```

<p>Represents the input of a CreateCustomActionType operation.</p>

##### Instances
``` purescript
Newtype CreateCustomActionTypeInput _
```

#### `CreateCustomActionTypeOutput`

``` purescript
newtype CreateCustomActionTypeOutput
  = CreateCustomActionTypeOutput { "ActionType'" :: ActionType }
```

<p>Represents the output of a CreateCustomActionType operation.</p>

##### Instances
``` purescript
Newtype CreateCustomActionTypeOutput _
```

#### `CreatePipelineInput`

``` purescript
newtype CreatePipelineInput
  = CreatePipelineInput { "Pipeline'" :: PipelineDeclaration }
```

<p>Represents the input of a CreatePipeline action.</p>

##### Instances
``` purescript
Newtype CreatePipelineInput _
```

#### `CreatePipelineOutput`

``` purescript
newtype CreatePipelineOutput
  = CreatePipelineOutput { "Pipeline'" :: NullOrUndefined (PipelineDeclaration) }
```

<p>Represents the output of a CreatePipeline action.</p>

##### Instances
``` purescript
Newtype CreatePipelineOutput _
```

#### `CurrentRevision`

``` purescript
newtype CurrentRevision
  = CurrentRevision { "Revision'" :: Revision, "ChangeIdentifier'" :: RevisionChangeIdentifier, "Created'" :: NullOrUndefined (Time), "RevisionSummary'" :: NullOrUndefined (RevisionSummary) }
```

<p>Represents information about a current revision.</p>

##### Instances
``` purescript
Newtype CurrentRevision _
```

#### `DeleteCustomActionTypeInput`

``` purescript
newtype DeleteCustomActionTypeInput
  = DeleteCustomActionTypeInput { "Category'" :: ActionCategory, "Provider'" :: ActionProvider, "Version'" :: Version }
```

<p>Represents the input of a DeleteCustomActionType operation. The custom action will be marked as deleted.</p>

##### Instances
``` purescript
Newtype DeleteCustomActionTypeInput _
```

#### `DeletePipelineInput`

``` purescript
newtype DeletePipelineInput
  = DeletePipelineInput { "Name'" :: PipelineName }
```

<p>Represents the input of a DeletePipeline action.</p>

##### Instances
``` purescript
Newtype DeletePipelineInput _
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

##### Instances
``` purescript
Newtype Description _
```

#### `DisableStageTransitionInput`

``` purescript
newtype DisableStageTransitionInput
  = DisableStageTransitionInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "TransitionType'" :: StageTransitionType, "Reason'" :: DisabledReason }
```

<p>Represents the input of a DisableStageTransition action.</p>

##### Instances
``` purescript
Newtype DisableStageTransitionInput _
```

#### `DisabledReason`

``` purescript
newtype DisabledReason
  = DisabledReason String
```

##### Instances
``` purescript
Newtype DisabledReason _
```

#### `EnableStageTransitionInput`

``` purescript
newtype EnableStageTransitionInput
  = EnableStageTransitionInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "TransitionType'" :: StageTransitionType }
```

<p>Represents the input of an EnableStageTransition action.</p>

##### Instances
``` purescript
Newtype EnableStageTransitionInput _
```

#### `Enabled`

``` purescript
newtype Enabled
  = Enabled Boolean
```

##### Instances
``` purescript
Newtype Enabled _
```

#### `EncryptionKey`

``` purescript
newtype EncryptionKey
  = EncryptionKey { "Id'" :: EncryptionKeyId, "Type'" :: EncryptionKeyType }
```

<p>Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.</p>

##### Instances
``` purescript
Newtype EncryptionKey _
```

#### `EncryptionKeyId`

``` purescript
newtype EncryptionKeyId
  = EncryptionKeyId String
```

##### Instances
``` purescript
Newtype EncryptionKeyId _
```

#### `EncryptionKeyType`

``` purescript
newtype EncryptionKeyType
  = EncryptionKeyType String
```

##### Instances
``` purescript
Newtype EncryptionKeyType _
```

#### `ErrorDetails`

``` purescript
newtype ErrorDetails
  = ErrorDetails { "Code'" :: NullOrUndefined (Code), "Message'" :: NullOrUndefined (Message) }
```

<p>Represents information about an error in AWS CodePipeline.</p>

##### Instances
``` purescript
Newtype ErrorDetails _
```

#### `ExecutionDetails`

``` purescript
newtype ExecutionDetails
  = ExecutionDetails { "Summary'" :: NullOrUndefined (ExecutionSummary), "ExternalExecutionId'" :: NullOrUndefined (ExecutionId), "PercentComplete'" :: NullOrUndefined (Percentage) }
```

<p>The details of the actions taken and results produced on an artifact as it passes through stages in the pipeline.</p>

##### Instances
``` purescript
Newtype ExecutionDetails _
```

#### `ExecutionId`

``` purescript
newtype ExecutionId
  = ExecutionId String
```

##### Instances
``` purescript
Newtype ExecutionId _
```

#### `ExecutionSummary`

``` purescript
newtype ExecutionSummary
  = ExecutionSummary String
```

##### Instances
``` purescript
Newtype ExecutionSummary _
```

#### `FailureDetails`

``` purescript
newtype FailureDetails
  = FailureDetails { "Type'" :: FailureType, "Message'" :: Message, "ExternalExecutionId'" :: NullOrUndefined (ExecutionId) }
```

<p>Represents information about failure details.</p>

##### Instances
``` purescript
Newtype FailureDetails _
```

#### `FailureType`

``` purescript
newtype FailureType
  = FailureType String
```

##### Instances
``` purescript
Newtype FailureType _
```

#### `GetJobDetailsInput`

``` purescript
newtype GetJobDetailsInput
  = GetJobDetailsInput { "JobId'" :: JobId }
```

<p>Represents the input of a GetJobDetails action.</p>

##### Instances
``` purescript
Newtype GetJobDetailsInput _
```

#### `GetJobDetailsOutput`

``` purescript
newtype GetJobDetailsOutput
  = GetJobDetailsOutput { "JobDetails'" :: NullOrUndefined (JobDetails) }
```

<p>Represents the output of a GetJobDetails action.</p>

##### Instances
``` purescript
Newtype GetJobDetailsOutput _
```

#### `GetPipelineExecutionInput`

``` purescript
newtype GetPipelineExecutionInput
  = GetPipelineExecutionInput { "PipelineName'" :: PipelineName, "PipelineExecutionId'" :: PipelineExecutionId }
```

<p>Represents the input of a GetPipelineExecution action.</p>

##### Instances
``` purescript
Newtype GetPipelineExecutionInput _
```

#### `GetPipelineExecutionOutput`

``` purescript
newtype GetPipelineExecutionOutput
  = GetPipelineExecutionOutput { "PipelineExecution'" :: NullOrUndefined (PipelineExecution) }
```

<p>Represents the output of a GetPipelineExecution action.</p>

##### Instances
``` purescript
Newtype GetPipelineExecutionOutput _
```

#### `GetPipelineInput`

``` purescript
newtype GetPipelineInput
  = GetPipelineInput { "Name'" :: PipelineName, "Version'" :: NullOrUndefined (PipelineVersion) }
```

<p>Represents the input of a GetPipeline action.</p>

##### Instances
``` purescript
Newtype GetPipelineInput _
```

#### `GetPipelineOutput`

``` purescript
newtype GetPipelineOutput
  = GetPipelineOutput { "Pipeline'" :: NullOrUndefined (PipelineDeclaration), "Metadata'" :: NullOrUndefined (PipelineMetadata) }
```

<p>Represents the output of a GetPipeline action.</p>

##### Instances
``` purescript
Newtype GetPipelineOutput _
```

#### `GetPipelineStateInput`

``` purescript
newtype GetPipelineStateInput
  = GetPipelineStateInput { "Name'" :: PipelineName }
```

<p>Represents the input of a GetPipelineState action.</p>

##### Instances
``` purescript
Newtype GetPipelineStateInput _
```

#### `GetPipelineStateOutput`

``` purescript
newtype GetPipelineStateOutput
  = GetPipelineStateOutput { "PipelineName'" :: NullOrUndefined (PipelineName), "PipelineVersion'" :: NullOrUndefined (PipelineVersion), "StageStates'" :: NullOrUndefined (StageStateList), "Created'" :: NullOrUndefined (Number), "Updated'" :: NullOrUndefined (Number) }
```

<p>Represents the output of a GetPipelineState action.</p>

##### Instances
``` purescript
Newtype GetPipelineStateOutput _
```

#### `GetThirdPartyJobDetailsInput`

``` purescript
newtype GetThirdPartyJobDetailsInput
  = GetThirdPartyJobDetailsInput { "JobId'" :: ThirdPartyJobId, "ClientToken'" :: ClientToken }
```

<p>Represents the input of a GetThirdPartyJobDetails action.</p>

##### Instances
``` purescript
Newtype GetThirdPartyJobDetailsInput _
```

#### `GetThirdPartyJobDetailsOutput`

``` purescript
newtype GetThirdPartyJobDetailsOutput
  = GetThirdPartyJobDetailsOutput { "JobDetails'" :: NullOrUndefined (ThirdPartyJobDetails) }
```

<p>Represents the output of a GetThirdPartyJobDetails action.</p>

##### Instances
``` purescript
Newtype GetThirdPartyJobDetailsOutput _
```

#### `InputArtifact`

``` purescript
newtype InputArtifact
  = InputArtifact { "Name'" :: ArtifactName }
```

<p>Represents information about an artifact to be worked on, such as a test or build artifact.</p>

##### Instances
``` purescript
Newtype InputArtifact _
```

#### `InputArtifactList`

``` purescript
newtype InputArtifactList
  = InputArtifactList (Array InputArtifact)
```

##### Instances
``` purescript
Newtype InputArtifactList _
```

#### `InvalidActionDeclarationException`

``` purescript
newtype InvalidActionDeclarationException
  = InvalidActionDeclarationException {  }
```

<p>The specified action declaration was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidActionDeclarationException _
```

#### `InvalidApprovalTokenException`

``` purescript
newtype InvalidApprovalTokenException
  = InvalidApprovalTokenException {  }
```

<p>The approval request already received a response or has expired.</p>

##### Instances
``` purescript
Newtype InvalidApprovalTokenException _
```

#### `InvalidBlockerDeclarationException`

``` purescript
newtype InvalidBlockerDeclarationException
  = InvalidBlockerDeclarationException {  }
```

<p>Reserved for future use.</p>

##### Instances
``` purescript
Newtype InvalidBlockerDeclarationException _
```

#### `InvalidClientTokenException`

``` purescript
newtype InvalidClientTokenException
  = InvalidClientTokenException {  }
```

<p>The client token was specified in an invalid format</p>

##### Instances
``` purescript
Newtype InvalidClientTokenException _
```

#### `InvalidJobException`

``` purescript
newtype InvalidJobException
  = InvalidJobException {  }
```

<p>The specified job was specified in an invalid format or cannot be found.</p>

##### Instances
``` purescript
Newtype InvalidJobException _
```

#### `InvalidJobStateException`

``` purescript
newtype InvalidJobStateException
  = InvalidJobStateException {  }
```

<p>The specified job state was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidJobStateException _
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException {  }
```

<p>The next token was specified in an invalid format. Make sure that the next token you provided is the token returned by a previous call.</p>

##### Instances
``` purescript
Newtype InvalidNextTokenException _
```

#### `InvalidNonceException`

``` purescript
newtype InvalidNonceException
  = InvalidNonceException {  }
```

<p>The specified nonce was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidNonceException _
```

#### `InvalidStageDeclarationException`

``` purescript
newtype InvalidStageDeclarationException
  = InvalidStageDeclarationException {  }
```

<p>The specified stage declaration was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidStageDeclarationException _
```

#### `InvalidStructureException`

``` purescript
newtype InvalidStructureException
  = InvalidStructureException {  }
```

<p>The specified structure was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype InvalidStructureException _
```

#### `Job`

``` purescript
newtype Job
  = Job { "Id'" :: NullOrUndefined (JobId), "Data'" :: NullOrUndefined (JobData), "Nonce'" :: NullOrUndefined (Nonce), "AccountId'" :: NullOrUndefined (AccountId) }
```

<p>Represents information about a job.</p>

##### Instances
``` purescript
Newtype Job _
```

#### `JobData`

``` purescript
newtype JobData
  = JobData { "ActionTypeId'" :: NullOrUndefined (ActionTypeId), "ActionConfiguration'" :: NullOrUndefined (ActionConfiguration), "PipelineContext'" :: NullOrUndefined (PipelineContext), "InputArtifacts'" :: NullOrUndefined (ArtifactList), "OutputArtifacts'" :: NullOrUndefined (ArtifactList), "ArtifactCredentials'" :: NullOrUndefined (AWSSessionCredentials), "ContinuationToken'" :: NullOrUndefined (ContinuationToken), "EncryptionKey'" :: NullOrUndefined (EncryptionKey) }
```

<p>Represents additional information about a job required for a job worker to complete the job.</p>

##### Instances
``` purescript
Newtype JobData _
```

#### `JobDetails`

``` purescript
newtype JobDetails
  = JobDetails { "Id'" :: NullOrUndefined (JobId), "Data'" :: NullOrUndefined (JobData), "AccountId'" :: NullOrUndefined (AccountId) }
```

<p>Represents information about the details of a job.</p>

##### Instances
``` purescript
Newtype JobDetails _
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

#### `JobList`

``` purescript
newtype JobList
  = JobList (Array Job)
```

##### Instances
``` purescript
Newtype JobList _
```

#### `JobNotFoundException`

``` purescript
newtype JobNotFoundException
  = JobNotFoundException {  }
```

<p>The specified job was specified in an invalid format or cannot be found.</p>

##### Instances
``` purescript
Newtype JobNotFoundException _
```

#### `JobStatus`

``` purescript
newtype JobStatus
  = JobStatus String
```

##### Instances
``` purescript
Newtype JobStatus _
```

#### `LastChangedAt`

``` purescript
newtype LastChangedAt
  = LastChangedAt Number
```

##### Instances
``` purescript
Newtype LastChangedAt _
```

#### `LastChangedBy`

``` purescript
newtype LastChangedBy
  = LastChangedBy String
```

##### Instances
``` purescript
Newtype LastChangedBy _
```

#### `LastUpdatedBy`

``` purescript
newtype LastUpdatedBy
  = LastUpdatedBy String
```

##### Instances
``` purescript
Newtype LastUpdatedBy _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException {  }
```

<p>The number of pipelines associated with the AWS account has exceeded the limit allowed for the account.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListActionTypesInput`

``` purescript
newtype ListActionTypesInput
  = ListActionTypesInput { "ActionOwnerFilter'" :: NullOrUndefined (ActionOwner), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListActionTypes action.</p>

##### Instances
``` purescript
Newtype ListActionTypesInput _
```

#### `ListActionTypesOutput`

``` purescript
newtype ListActionTypesOutput
  = ListActionTypesOutput { "ActionTypes'" :: ActionTypeList, "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListActionTypes action.</p>

##### Instances
``` purescript
Newtype ListActionTypesOutput _
```

#### `ListPipelineExecutionsInput`

``` purescript
newtype ListPipelineExecutionsInput
  = ListPipelineExecutionsInput { "PipelineName'" :: PipelineName, "MaxResults'" :: NullOrUndefined (MaxResults), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListPipelineExecutions action.</p>

##### Instances
``` purescript
Newtype ListPipelineExecutionsInput _
```

#### `ListPipelineExecutionsOutput`

``` purescript
newtype ListPipelineExecutionsOutput
  = ListPipelineExecutionsOutput { "PipelineExecutionSummaries'" :: NullOrUndefined (PipelineExecutionSummaryList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListPipelineExecutions action.</p>

##### Instances
``` purescript
Newtype ListPipelineExecutionsOutput _
```

#### `ListPipelinesInput`

``` purescript
newtype ListPipelinesInput
  = ListPipelinesInput { "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the input of a ListPipelines action.</p>

##### Instances
``` purescript
Newtype ListPipelinesInput _
```

#### `ListPipelinesOutput`

``` purescript
newtype ListPipelinesOutput
  = ListPipelinesOutput { "Pipelines'" :: NullOrUndefined (PipelineList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p>Represents the output of a ListPipelines action.</p>

##### Instances
``` purescript
Newtype ListPipelinesOutput _
```

#### `MaxBatchSize`

``` purescript
newtype MaxBatchSize
  = MaxBatchSize Int
```

##### Instances
``` purescript
Newtype MaxBatchSize _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

##### Instances
``` purescript
Newtype MaxResults _
```

#### `MaximumArtifactCount`

``` purescript
newtype MaximumArtifactCount
  = MaximumArtifactCount Int
```

##### Instances
``` purescript
Newtype MaximumArtifactCount _
```

#### `Message`

``` purescript
newtype Message
  = Message String
```

##### Instances
``` purescript
Newtype Message _
```

#### `MinimumArtifactCount`

``` purescript
newtype MinimumArtifactCount
  = MinimumArtifactCount Int
```

##### Instances
``` purescript
Newtype MinimumArtifactCount _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `Nonce`

``` purescript
newtype Nonce
  = Nonce String
```

##### Instances
``` purescript
Newtype Nonce _
```

#### `NotLatestPipelineExecutionException`

``` purescript
newtype NotLatestPipelineExecutionException
  = NotLatestPipelineExecutionException {  }
```

<p>The stage has failed in a later run of the pipeline and the pipelineExecutionId associated with the request is out of date.</p>

##### Instances
``` purescript
Newtype NotLatestPipelineExecutionException _
```

#### `OutputArtifact`

``` purescript
newtype OutputArtifact
  = OutputArtifact { "Name'" :: ArtifactName }
```

<p>Represents information about the output of an action.</p>

##### Instances
``` purescript
Newtype OutputArtifact _
```

#### `OutputArtifactList`

``` purescript
newtype OutputArtifactList
  = OutputArtifactList (Array OutputArtifact)
```

##### Instances
``` purescript
Newtype OutputArtifactList _
```

#### `Percentage`

``` purescript
newtype Percentage
  = Percentage Int
```

##### Instances
``` purescript
Newtype Percentage _
```

#### `PipelineArn`

``` purescript
newtype PipelineArn
  = PipelineArn String
```

##### Instances
``` purescript
Newtype PipelineArn _
```

#### `PipelineContext`

``` purescript
newtype PipelineContext
  = PipelineContext { "PipelineName'" :: NullOrUndefined (PipelineName), "Stage'" :: NullOrUndefined (StageContext), "Action'" :: NullOrUndefined (ActionContext) }
```

<p>Represents information about a pipeline to a job worker.</p>

##### Instances
``` purescript
Newtype PipelineContext _
```

#### `PipelineDeclaration`

``` purescript
newtype PipelineDeclaration
  = PipelineDeclaration { "Name'" :: PipelineName, "RoleArn'" :: RoleArn, "ArtifactStore'" :: ArtifactStore, "Stages'" :: PipelineStageDeclarationList, "Version'" :: NullOrUndefined (PipelineVersion) }
```

<p>Represents the structure of actions and stages to be performed in the pipeline.</p>

##### Instances
``` purescript
Newtype PipelineDeclaration _
```

#### `PipelineExecution`

``` purescript
newtype PipelineExecution
  = PipelineExecution { "PipelineName'" :: NullOrUndefined (PipelineName), "PipelineVersion'" :: NullOrUndefined (PipelineVersion), "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId), "Status'" :: NullOrUndefined (PipelineExecutionStatus), "ArtifactRevisions'" :: NullOrUndefined (ArtifactRevisionList) }
```

<p>Represents information about an execution of a pipeline.</p>

##### Instances
``` purescript
Newtype PipelineExecution _
```

#### `PipelineExecutionId`

``` purescript
newtype PipelineExecutionId
  = PipelineExecutionId String
```

##### Instances
``` purescript
Newtype PipelineExecutionId _
```

#### `PipelineExecutionNotFoundException`

``` purescript
newtype PipelineExecutionNotFoundException
  = PipelineExecutionNotFoundException {  }
```

<p>The pipeline execution was specified in an invalid format or cannot be found, or an execution ID does not belong to the specified pipeline. </p>

##### Instances
``` purescript
Newtype PipelineExecutionNotFoundException _
```

#### `PipelineExecutionStatus`

``` purescript
newtype PipelineExecutionStatus
  = PipelineExecutionStatus String
```

##### Instances
``` purescript
Newtype PipelineExecutionStatus _
```

#### `PipelineExecutionSummary`

``` purescript
newtype PipelineExecutionSummary
  = PipelineExecutionSummary { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId), "Status'" :: NullOrUndefined (PipelineExecutionStatus), "StartTime'" :: NullOrUndefined (Number), "LastUpdateTime'" :: NullOrUndefined (Number) }
```

<p>Summary information about a pipeline execution.</p>

##### Instances
``` purescript
Newtype PipelineExecutionSummary _
```

#### `PipelineExecutionSummaryList`

``` purescript
newtype PipelineExecutionSummaryList
  = PipelineExecutionSummaryList (Array PipelineExecutionSummary)
```

##### Instances
``` purescript
Newtype PipelineExecutionSummaryList _
```

#### `PipelineList`

``` purescript
newtype PipelineList
  = PipelineList (Array PipelineSummary)
```

##### Instances
``` purescript
Newtype PipelineList _
```

#### `PipelineMetadata`

``` purescript
newtype PipelineMetadata
  = PipelineMetadata { "PipelineArn'" :: NullOrUndefined (PipelineArn), "Created'" :: NullOrUndefined (Number), "Updated'" :: NullOrUndefined (Number) }
```

<p>Information about a pipeline.</p>

##### Instances
``` purescript
Newtype PipelineMetadata _
```

#### `PipelineName`

``` purescript
newtype PipelineName
  = PipelineName String
```

##### Instances
``` purescript
Newtype PipelineName _
```

#### `PipelineNameInUseException`

``` purescript
newtype PipelineNameInUseException
  = PipelineNameInUseException {  }
```

<p>The specified pipeline name is already in use.</p>

##### Instances
``` purescript
Newtype PipelineNameInUseException _
```

#### `PipelineNotFoundException`

``` purescript
newtype PipelineNotFoundException
  = PipelineNotFoundException {  }
```

<p>The specified pipeline was specified in an invalid format or cannot be found.</p>

##### Instances
``` purescript
Newtype PipelineNotFoundException _
```

#### `PipelineStageDeclarationList`

``` purescript
newtype PipelineStageDeclarationList
  = PipelineStageDeclarationList (Array StageDeclaration)
```

##### Instances
``` purescript
Newtype PipelineStageDeclarationList _
```

#### `PipelineSummary`

``` purescript
newtype PipelineSummary
  = PipelineSummary { "Name'" :: NullOrUndefined (PipelineName), "Version'" :: NullOrUndefined (PipelineVersion), "Created'" :: NullOrUndefined (Number), "Updated'" :: NullOrUndefined (Number) }
```

<p>Returns a summary of a pipeline.</p>

##### Instances
``` purescript
Newtype PipelineSummary _
```

#### `PipelineVersion`

``` purescript
newtype PipelineVersion
  = PipelineVersion Int
```

##### Instances
``` purescript
Newtype PipelineVersion _
```

#### `PipelineVersionNotFoundException`

``` purescript
newtype PipelineVersionNotFoundException
  = PipelineVersionNotFoundException {  }
```

<p>The specified pipeline version was specified in an invalid format or cannot be found.</p>

##### Instances
``` purescript
Newtype PipelineVersionNotFoundException _
```

#### `PollForJobsInput`

``` purescript
newtype PollForJobsInput
  = PollForJobsInput { "ActionTypeId'" :: ActionTypeId, "MaxBatchSize'" :: NullOrUndefined (MaxBatchSize), "QueryParam'" :: NullOrUndefined (QueryParamMap) }
```

<p>Represents the input of a PollForJobs action.</p>

##### Instances
``` purescript
Newtype PollForJobsInput _
```

#### `PollForJobsOutput`

``` purescript
newtype PollForJobsOutput
  = PollForJobsOutput { "Jobs'" :: NullOrUndefined (JobList) }
```

<p>Represents the output of a PollForJobs action.</p>

##### Instances
``` purescript
Newtype PollForJobsOutput _
```

#### `PollForThirdPartyJobsInput`

``` purescript
newtype PollForThirdPartyJobsInput
  = PollForThirdPartyJobsInput { "ActionTypeId'" :: ActionTypeId, "MaxBatchSize'" :: NullOrUndefined (MaxBatchSize) }
```

<p>Represents the input of a PollForThirdPartyJobs action.</p>

##### Instances
``` purescript
Newtype PollForThirdPartyJobsInput _
```

#### `PollForThirdPartyJobsOutput`

``` purescript
newtype PollForThirdPartyJobsOutput
  = PollForThirdPartyJobsOutput { "Jobs'" :: NullOrUndefined (ThirdPartyJobList) }
```

<p>Represents the output of a PollForThirdPartyJobs action.</p>

##### Instances
``` purescript
Newtype PollForThirdPartyJobsOutput _
```

#### `PutActionRevisionInput`

``` purescript
newtype PutActionRevisionInput
  = PutActionRevisionInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "ActionName'" :: ActionName, "ActionRevision'" :: ActionRevision }
```

<p>Represents the input of a PutActionRevision action.</p>

##### Instances
``` purescript
Newtype PutActionRevisionInput _
```

#### `PutActionRevisionOutput`

``` purescript
newtype PutActionRevisionOutput
  = PutActionRevisionOutput { "NewRevision'" :: NullOrUndefined (Boolean), "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId) }
```

<p>Represents the output of a PutActionRevision action.</p>

##### Instances
``` purescript
Newtype PutActionRevisionOutput _
```

#### `PutApprovalResultInput`

``` purescript
newtype PutApprovalResultInput
  = PutApprovalResultInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "ActionName'" :: ActionName, "Result'" :: ApprovalResult, "Token'" :: ApprovalToken }
```

<p>Represents the input of a PutApprovalResult action.</p>

##### Instances
``` purescript
Newtype PutApprovalResultInput _
```

#### `PutApprovalResultOutput`

``` purescript
newtype PutApprovalResultOutput
  = PutApprovalResultOutput { "ApprovedAt'" :: NullOrUndefined (Number) }
```

<p>Represents the output of a PutApprovalResult action.</p>

##### Instances
``` purescript
Newtype PutApprovalResultOutput _
```

#### `PutJobFailureResultInput`

``` purescript
newtype PutJobFailureResultInput
  = PutJobFailureResultInput { "JobId'" :: JobId, "FailureDetails'" :: FailureDetails }
```

<p>Represents the input of a PutJobFailureResult action.</p>

##### Instances
``` purescript
Newtype PutJobFailureResultInput _
```

#### `PutJobSuccessResultInput`

``` purescript
newtype PutJobSuccessResultInput
  = PutJobSuccessResultInput { "JobId'" :: JobId, "CurrentRevision'" :: NullOrUndefined (CurrentRevision), "ContinuationToken'" :: NullOrUndefined (ContinuationToken), "ExecutionDetails'" :: NullOrUndefined (ExecutionDetails) }
```

<p>Represents the input of a PutJobSuccessResult action.</p>

##### Instances
``` purescript
Newtype PutJobSuccessResultInput _
```

#### `PutThirdPartyJobFailureResultInput`

``` purescript
newtype PutThirdPartyJobFailureResultInput
  = PutThirdPartyJobFailureResultInput { "JobId'" :: ThirdPartyJobId, "ClientToken'" :: ClientToken, "FailureDetails'" :: FailureDetails }
```

<p>Represents the input of a PutThirdPartyJobFailureResult action.</p>

##### Instances
``` purescript
Newtype PutThirdPartyJobFailureResultInput _
```

#### `PutThirdPartyJobSuccessResultInput`

``` purescript
newtype PutThirdPartyJobSuccessResultInput
  = PutThirdPartyJobSuccessResultInput { "JobId'" :: ThirdPartyJobId, "ClientToken'" :: ClientToken, "CurrentRevision'" :: NullOrUndefined (CurrentRevision), "ContinuationToken'" :: NullOrUndefined (ContinuationToken), "ExecutionDetails'" :: NullOrUndefined (ExecutionDetails) }
```

<p>Represents the input of a PutThirdPartyJobSuccessResult action.</p>

##### Instances
``` purescript
Newtype PutThirdPartyJobSuccessResultInput _
```

#### `QueryParamMap`

``` purescript
newtype QueryParamMap
  = QueryParamMap (Map ActionConfigurationKey ActionConfigurationQueryableValue)
```

##### Instances
``` purescript
Newtype QueryParamMap _
```

#### `RetryStageExecutionInput`

``` purescript
newtype RetryStageExecutionInput
  = RetryStageExecutionInput { "PipelineName'" :: PipelineName, "StageName'" :: StageName, "PipelineExecutionId'" :: PipelineExecutionId, "RetryMode'" :: StageRetryMode }
```

<p>Represents the input of a RetryStageExecution action.</p>

##### Instances
``` purescript
Newtype RetryStageExecutionInput _
```

#### `RetryStageExecutionOutput`

``` purescript
newtype RetryStageExecutionOutput
  = RetryStageExecutionOutput { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId) }
```

<p>Represents the output of a RetryStageExecution action.</p>

##### Instances
``` purescript
Newtype RetryStageExecutionOutput _
```

#### `Revision`

``` purescript
newtype Revision
  = Revision String
```

##### Instances
``` purescript
Newtype Revision _
```

#### `RevisionChangeIdentifier`

``` purescript
newtype RevisionChangeIdentifier
  = RevisionChangeIdentifier String
```

##### Instances
``` purescript
Newtype RevisionChangeIdentifier _
```

#### `RevisionSummary`

``` purescript
newtype RevisionSummary
  = RevisionSummary String
```

##### Instances
``` purescript
Newtype RevisionSummary _
```

#### `RoleArn`

``` purescript
newtype RoleArn
  = RoleArn String
```

##### Instances
``` purescript
Newtype RoleArn _
```

#### `S3ArtifactLocation`

``` purescript
newtype S3ArtifactLocation
  = S3ArtifactLocation { "BucketName'" :: S3BucketName, "ObjectKey'" :: S3ObjectKey }
```

<p>The location of the Amazon S3 bucket that contains a revision.</p>

##### Instances
``` purescript
Newtype S3ArtifactLocation _
```

#### `S3BucketName`

``` purescript
newtype S3BucketName
  = S3BucketName String
```

##### Instances
``` purescript
Newtype S3BucketName _
```

#### `S3ObjectKey`

``` purescript
newtype S3ObjectKey
  = S3ObjectKey String
```

##### Instances
``` purescript
Newtype S3ObjectKey _
```

#### `SecretAccessKey`

``` purescript
newtype SecretAccessKey
  = SecretAccessKey String
```

##### Instances
``` purescript
Newtype SecretAccessKey _
```

#### `SessionToken`

``` purescript
newtype SessionToken
  = SessionToken String
```

##### Instances
``` purescript
Newtype SessionToken _
```

#### `StageActionDeclarationList`

``` purescript
newtype StageActionDeclarationList
  = StageActionDeclarationList (Array ActionDeclaration)
```

##### Instances
``` purescript
Newtype StageActionDeclarationList _
```

#### `StageBlockerDeclarationList`

``` purescript
newtype StageBlockerDeclarationList
  = StageBlockerDeclarationList (Array BlockerDeclaration)
```

##### Instances
``` purescript
Newtype StageBlockerDeclarationList _
```

#### `StageContext`

``` purescript
newtype StageContext
  = StageContext { "Name'" :: NullOrUndefined (StageName) }
```

<p>Represents information about a stage to a job worker.</p>

##### Instances
``` purescript
Newtype StageContext _
```

#### `StageDeclaration`

``` purescript
newtype StageDeclaration
  = StageDeclaration { "Name'" :: StageName, "Blockers'" :: NullOrUndefined (StageBlockerDeclarationList), "Actions'" :: StageActionDeclarationList }
```

<p>Represents information about a stage and its definition.</p>

##### Instances
``` purescript
Newtype StageDeclaration _
```

#### `StageExecution`

``` purescript
newtype StageExecution
  = StageExecution { "PipelineExecutionId'" :: PipelineExecutionId, "Status'" :: StageExecutionStatus }
```

<p>Represents information about the run of a stage.</p>

##### Instances
``` purescript
Newtype StageExecution _
```

#### `StageExecutionStatus`

``` purescript
newtype StageExecutionStatus
  = StageExecutionStatus String
```

##### Instances
``` purescript
Newtype StageExecutionStatus _
```

#### `StageName`

``` purescript
newtype StageName
  = StageName String
```

##### Instances
``` purescript
Newtype StageName _
```

#### `StageNotFoundException`

``` purescript
newtype StageNotFoundException
  = StageNotFoundException {  }
```

<p>The specified stage was specified in an invalid format or cannot be found.</p>

##### Instances
``` purescript
Newtype StageNotFoundException _
```

#### `StageNotRetryableException`

``` purescript
newtype StageNotRetryableException
  = StageNotRetryableException {  }
```

<p>The specified stage can't be retried because the pipeline structure or stage state changed after the stage was not completed; the stage contains no failed actions; one or more actions are still in progress; or another retry attempt is already in progress. </p>

##### Instances
``` purescript
Newtype StageNotRetryableException _
```

#### `StageRetryMode`

``` purescript
newtype StageRetryMode
  = StageRetryMode String
```

##### Instances
``` purescript
Newtype StageRetryMode _
```

#### `StageState`

``` purescript
newtype StageState
  = StageState { "StageName'" :: NullOrUndefined (StageName), "InboundTransitionState'" :: NullOrUndefined (TransitionState), "ActionStates'" :: NullOrUndefined (ActionStateList), "LatestExecution'" :: NullOrUndefined (StageExecution) }
```

<p>Represents information about the state of the stage.</p>

##### Instances
``` purescript
Newtype StageState _
```

#### `StageStateList`

``` purescript
newtype StageStateList
  = StageStateList (Array StageState)
```

##### Instances
``` purescript
Newtype StageStateList _
```

#### `StageTransitionType`

``` purescript
newtype StageTransitionType
  = StageTransitionType String
```

##### Instances
``` purescript
Newtype StageTransitionType _
```

#### `StartPipelineExecutionInput`

``` purescript
newtype StartPipelineExecutionInput
  = StartPipelineExecutionInput { "Name'" :: PipelineName }
```

<p>Represents the input of a StartPipelineExecution action.</p>

##### Instances
``` purescript
Newtype StartPipelineExecutionInput _
```

#### `StartPipelineExecutionOutput`

``` purescript
newtype StartPipelineExecutionOutput
  = StartPipelineExecutionOutput { "PipelineExecutionId'" :: NullOrUndefined (PipelineExecutionId) }
```

<p>Represents the output of a StartPipelineExecution action.</p>

##### Instances
``` purescript
Newtype StartPipelineExecutionOutput _
```

#### `ThirdPartyJob`

``` purescript
newtype ThirdPartyJob
  = ThirdPartyJob { "ClientId'" :: NullOrUndefined (ClientId), "JobId'" :: NullOrUndefined (JobId) }
```

<p>A response to a PollForThirdPartyJobs request returned by AWS CodePipeline when there is a job to be worked upon by a partner action.</p>

##### Instances
``` purescript
Newtype ThirdPartyJob _
```

#### `ThirdPartyJobData`

``` purescript
newtype ThirdPartyJobData
  = ThirdPartyJobData { "ActionTypeId'" :: NullOrUndefined (ActionTypeId), "ActionConfiguration'" :: NullOrUndefined (ActionConfiguration), "PipelineContext'" :: NullOrUndefined (PipelineContext), "InputArtifacts'" :: NullOrUndefined (ArtifactList), "OutputArtifacts'" :: NullOrUndefined (ArtifactList), "ArtifactCredentials'" :: NullOrUndefined (AWSSessionCredentials), "ContinuationToken'" :: NullOrUndefined (ContinuationToken), "EncryptionKey'" :: NullOrUndefined (EncryptionKey) }
```

<p>Represents information about the job data for a partner action.</p>

##### Instances
``` purescript
Newtype ThirdPartyJobData _
```

#### `ThirdPartyJobDetails`

``` purescript
newtype ThirdPartyJobDetails
  = ThirdPartyJobDetails { "Id'" :: NullOrUndefined (ThirdPartyJobId), "Data'" :: NullOrUndefined (ThirdPartyJobData), "Nonce'" :: NullOrUndefined (Nonce) }
```

<p>The details of a job sent in response to a GetThirdPartyJobDetails request.</p>

##### Instances
``` purescript
Newtype ThirdPartyJobDetails _
```

#### `ThirdPartyJobId`

``` purescript
newtype ThirdPartyJobId
  = ThirdPartyJobId String
```

##### Instances
``` purescript
Newtype ThirdPartyJobId _
```

#### `ThirdPartyJobList`

``` purescript
newtype ThirdPartyJobList
  = ThirdPartyJobList (Array ThirdPartyJob)
```

##### Instances
``` purescript
Newtype ThirdPartyJobList _
```

#### `Time`

``` purescript
newtype Time
  = Time Number
```

##### Instances
``` purescript
Newtype Time _
```

#### `TransitionState`

``` purescript
newtype TransitionState
  = TransitionState { "Enabled'" :: NullOrUndefined (Enabled), "LastChangedBy'" :: NullOrUndefined (LastChangedBy), "LastChangedAt'" :: NullOrUndefined (LastChangedAt), "DisabledReason'" :: NullOrUndefined (DisabledReason) }
```

<p>Represents information about the state of transitions between one stage and another stage.</p>

##### Instances
``` purescript
Newtype TransitionState _
```

#### `UpdatePipelineInput`

``` purescript
newtype UpdatePipelineInput
  = UpdatePipelineInput { "Pipeline'" :: PipelineDeclaration }
```

<p>Represents the input of an UpdatePipeline action.</p>

##### Instances
``` purescript
Newtype UpdatePipelineInput _
```

#### `UpdatePipelineOutput`

``` purescript
newtype UpdatePipelineOutput
  = UpdatePipelineOutput { "Pipeline'" :: NullOrUndefined (PipelineDeclaration) }
```

<p>Represents the output of an UpdatePipeline action.</p>

##### Instances
``` purescript
Newtype UpdatePipelineOutput _
```

#### `Url`

``` purescript
newtype Url
  = Url String
```

##### Instances
``` purescript
Newtype Url _
```

#### `UrlTemplate`

``` purescript
newtype UrlTemplate
  = UrlTemplate String
```

##### Instances
``` purescript
Newtype UrlTemplate _
```

#### `ValidationException`

``` purescript
newtype ValidationException
  = ValidationException {  }
```

<p>The validation was specified in an invalid format.</p>

##### Instances
``` purescript
Newtype ValidationException _
```

#### `Version`

``` purescript
newtype Version
  = Version String
```

##### Instances
``` purescript
Newtype Version _
```


