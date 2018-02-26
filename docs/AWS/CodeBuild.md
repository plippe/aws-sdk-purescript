## Module AWS.CodeBuild

<fullname>AWS CodeBuild</fullname> <p>AWS CodeBuild is a fully managed build service in the cloud. AWS CodeBuild compiles your source code, runs unit tests, and produces artifacts that are ready to deploy. AWS CodeBuild eliminates the need to provision, manage, and scale your own build servers. It provides prepackaged build environments for the most popular programming languages and build tools, such as Apache Maven, Gradle, and more. You can also fully customize build environments in AWS CodeBuild to use your own build tools. AWS CodeBuild scales automatically to meet peak build requests, and you pay only for the build time you consume. For more information about AWS CodeBuild, see the <i>AWS CodeBuild User Guide</i>.</p> <p>AWS CodeBuild supports these operations:</p> <ul> <li> <p> <code>BatchDeleteBuilds</code>: Deletes one or more builds.</p> </li> <li> <p> <code>BatchGetProjects</code>: Gets information about one or more build projects. A <i>build project</i> defines how AWS CodeBuild will run a build. This includes information such as where to get the source code to build, the build environment to use, the build commands to run, and where to store the build output. A <i>build environment</i> represents a combination of operating system, programming language runtime, and tools that AWS CodeBuild will use to run a build. Also, you can add tags to build projects to help manage your resources and costs.</p> </li> <li> <p> <code>CreateProject</code>: Creates a build project.</p> </li> <li> <p> <code>CreateWebhook</code>: For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, enables AWS CodeBuild to begin automatically rebuilding the source code every time a code change is pushed to the repository.</p> </li> <li> <p> <code>DeleteProject</code>: Deletes a build project.</p> </li> <li> <p> <code>DeleteWebhook</code>: For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, stops AWS CodeBuild from automatically rebuilding the source code every time a code change is pushed to the repository.</p> </li> <li> <p> <code>ListProjects</code>: Gets a list of build project names, with each build project name representing a single build project.</p> </li> <li> <p> <code>UpdateProject</code>: Changes the settings of an existing build project.</p> </li> <li> <p> <code>BatchGetBuilds</code>: Gets information about one or more builds.</p> </li> <li> <p> <code>ListBuilds</code>: Gets a list of build IDs, with each build ID representing a single build.</p> </li> <li> <p> <code>ListBuildsForProject</code>: Gets a list of build IDs for the specified build project, with each build ID representing a single build.</p> </li> <li> <p> <code>StartBuild</code>: Starts running a build.</p> </li> <li> <p> <code>StopBuild</code>: Attempts to stop running a build.</p> </li> <li> <p> <code>ListCuratedEnvironmentImages</code>: Gets information about Docker images that are managed by AWS CodeBuild.</p> </li> </ul>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `batchDeleteBuilds`

``` purescript
batchDeleteBuilds :: forall eff. BatchDeleteBuildsInput -> Aff (err :: RequestError | eff) BatchDeleteBuildsOutput
```

<p>Deletes one or more builds.</p>

#### `batchGetBuilds`

``` purescript
batchGetBuilds :: forall eff. BatchGetBuildsInput -> Aff (err :: RequestError | eff) BatchGetBuildsOutput
```

<p>Gets information about builds.</p>

#### `batchGetProjects`

``` purescript
batchGetProjects :: forall eff. BatchGetProjectsInput -> Aff (err :: RequestError | eff) BatchGetProjectsOutput
```

<p>Gets information about build projects.</p>

#### `createProject`

``` purescript
createProject :: forall eff. CreateProjectInput -> Aff (err :: RequestError | eff) CreateProjectOutput
```

<p>Creates a build project.</p>

#### `createWebhook`

``` purescript
createWebhook :: forall eff. CreateWebhookInput -> Aff (err :: RequestError | eff) CreateWebhookOutput
```

<p>For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, enables AWS CodeBuild to begin automatically rebuilding the source code every time a code change is pushed to the repository.</p> <important> <p>If you enable webhooks for an AWS CodeBuild project, and the project is used as a build step in AWS CodePipeline, then two identical builds will be created for each commit. One build is triggered through webhooks, and one through AWS CodePipeline. Because billing is on a per-build basis, you will be billed for both builds. Therefore, if you are using AWS CodePipeline, we recommend that you disable webhooks in CodeBuild. In the AWS CodeBuild console, clear the Webhook box. For more information, see step 9 in <a href="http://docs.aws.amazon.com/codebuild/latest/userguide/change-project.html#change-project-console">Change a Build Project's Settings</a>.</p> </important>

#### `deleteProject`

``` purescript
deleteProject :: forall eff. DeleteProjectInput -> Aff (err :: RequestError | eff) DeleteProjectOutput
```

<p>Deletes a build project.</p>

#### `deleteWebhook`

``` purescript
deleteWebhook :: forall eff. DeleteWebhookInput -> Aff (err :: RequestError | eff) DeleteWebhookOutput
```

<p>For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, stops AWS CodeBuild from automatically rebuilding the source code every time a code change is pushed to the repository.</p>

#### `invalidateProjectCache`

``` purescript
invalidateProjectCache :: forall eff. InvalidateProjectCacheInput -> Aff (err :: RequestError | eff) InvalidateProjectCacheOutput
```

<p>Resets the cache for a project.</p>

#### `listBuilds`

``` purescript
listBuilds :: forall eff. ListBuildsInput -> Aff (err :: RequestError | eff) ListBuildsOutput
```

<p>Gets a list of build IDs, with each build ID representing a single build.</p>

#### `listBuildsForProject`

``` purescript
listBuildsForProject :: forall eff. ListBuildsForProjectInput -> Aff (err :: RequestError | eff) ListBuildsForProjectOutput
```

<p>Gets a list of build IDs for the specified build project, with each build ID representing a single build.</p>

#### `listCuratedEnvironmentImages`

``` purescript
listCuratedEnvironmentImages :: forall eff. ListCuratedEnvironmentImagesInput -> Aff (err :: RequestError | eff) ListCuratedEnvironmentImagesOutput
```

<p>Gets information about Docker images that are managed by AWS CodeBuild.</p>

#### `listProjects`

``` purescript
listProjects :: forall eff. ListProjectsInput -> Aff (err :: RequestError | eff) ListProjectsOutput
```

<p>Gets a list of build project names, with each build project name representing a single build project.</p>

#### `startBuild`

``` purescript
startBuild :: forall eff. StartBuildInput -> Aff (err :: RequestError | eff) StartBuildOutput
```

<p>Starts running a build.</p>

#### `stopBuild`

``` purescript
stopBuild :: forall eff. StopBuildInput -> Aff (err :: RequestError | eff) StopBuildOutput
```

<p>Attempts to stop running a build.</p>

#### `updateProject`

``` purescript
updateProject :: forall eff. UpdateProjectInput -> Aff (err :: RequestError | eff) UpdateProjectOutput
```

<p>Changes the settings of a build project.</p>

#### `AccountLimitExceededException`

``` purescript
newtype AccountLimitExceededException
  = AccountLimitExceededException {  }
```

<p>An AWS service limit was exceeded for the calling AWS account.</p>

#### `ArtifactNamespace`

``` purescript
newtype ArtifactNamespace
  = ArtifactNamespace String
```

#### `ArtifactPackaging`

``` purescript
newtype ArtifactPackaging
  = ArtifactPackaging String
```

#### `ArtifactsType`

``` purescript
newtype ArtifactsType
  = ArtifactsType String
```

#### `BatchDeleteBuildsInput`

``` purescript
newtype BatchDeleteBuildsInput
  = BatchDeleteBuildsInput { "Ids'" :: BuildIds }
```

#### `BatchDeleteBuildsOutput`

``` purescript
newtype BatchDeleteBuildsOutput
  = BatchDeleteBuildsOutput { "BuildsDeleted'" :: NullOrUndefined (BuildIds), "BuildsNotDeleted'" :: NullOrUndefined (BuildsNotDeleted) }
```

#### `BatchGetBuildsInput`

``` purescript
newtype BatchGetBuildsInput
  = BatchGetBuildsInput { "Ids'" :: BuildIds }
```

#### `BatchGetBuildsOutput`

``` purescript
newtype BatchGetBuildsOutput
  = BatchGetBuildsOutput { "Builds'" :: NullOrUndefined (Builds), "BuildsNotFound'" :: NullOrUndefined (BuildIds) }
```

#### `BatchGetProjectsInput`

``` purescript
newtype BatchGetProjectsInput
  = BatchGetProjectsInput { "Names'" :: ProjectNames }
```

#### `BatchGetProjectsOutput`

``` purescript
newtype BatchGetProjectsOutput
  = BatchGetProjectsOutput { "Projects'" :: NullOrUndefined (Projects), "ProjectsNotFound'" :: NullOrUndefined (ProjectNames) }
```

#### `Build`

``` purescript
newtype Build
  = Build { "Id'" :: NullOrUndefined (NonEmptyString), "Arn'" :: NullOrUndefined (NonEmptyString), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "CurrentPhase'" :: NullOrUndefined (String), "BuildStatus'" :: NullOrUndefined (StatusType), "SourceVersion'" :: NullOrUndefined (NonEmptyString), "ProjectName'" :: NullOrUndefined (NonEmptyString), "Phases'" :: NullOrUndefined (BuildPhases), "Source'" :: NullOrUndefined (ProjectSource), "Artifacts'" :: NullOrUndefined (BuildArtifacts), "Cache'" :: NullOrUndefined (ProjectCache), "Environment'" :: NullOrUndefined (ProjectEnvironment), "Logs'" :: NullOrUndefined (LogsLocation), "TimeoutInMinutes'" :: NullOrUndefined (WrapperInt), "BuildComplete'" :: NullOrUndefined (Boolean), "Initiator'" :: NullOrUndefined (String), "VpcConfig'" :: NullOrUndefined (VpcConfig), "NetworkInterface'" :: NullOrUndefined (NetworkInterface) }
```

<p>Information about a build.</p>

#### `BuildArtifacts`

``` purescript
newtype BuildArtifacts
  = BuildArtifacts { "Location'" :: NullOrUndefined (String), "Sha256sum'" :: NullOrUndefined (String), "Md5sum'" :: NullOrUndefined (String) }
```

<p>Information about build output artifacts.</p>

#### `BuildIds`

``` purescript
newtype BuildIds
  = BuildIds (Array NonEmptyString)
```

#### `BuildNotDeleted`

``` purescript
newtype BuildNotDeleted
  = BuildNotDeleted { "Id'" :: NullOrUndefined (NonEmptyString), "StatusCode'" :: NullOrUndefined (String) }
```

<p>Information about a build that could not be successfully deleted.</p>

#### `BuildPhase`

``` purescript
newtype BuildPhase
  = BuildPhase { "PhaseType'" :: NullOrUndefined (BuildPhaseType), "PhaseStatus'" :: NullOrUndefined (StatusType), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "DurationInSeconds'" :: NullOrUndefined (WrapperLong), "Contexts'" :: NullOrUndefined (PhaseContexts) }
```

<p>Information about a stage for a build.</p>

#### `BuildPhaseType`

``` purescript
newtype BuildPhaseType
  = BuildPhaseType String
```

#### `BuildPhases`

``` purescript
newtype BuildPhases
  = BuildPhases (Array BuildPhase)
```

#### `Builds`

``` purescript
newtype Builds
  = Builds (Array Build)
```

#### `BuildsNotDeleted`

``` purescript
newtype BuildsNotDeleted
  = BuildsNotDeleted (Array BuildNotDeleted)
```

#### `CacheType`

``` purescript
newtype CacheType
  = CacheType String
```

#### `ComputeType`

``` purescript
newtype ComputeType
  = ComputeType String
```

#### `CreateProjectInput`

``` purescript
newtype CreateProjectInput
  = CreateProjectInput { "Name'" :: ProjectName, "Description'" :: NullOrUndefined (ProjectDescription), "Source'" :: ProjectSource, "Artifacts'" :: ProjectArtifacts, "Cache'" :: NullOrUndefined (ProjectCache), "Environment'" :: ProjectEnvironment, "ServiceRole'" :: NullOrUndefined (NonEmptyString), "TimeoutInMinutes'" :: NullOrUndefined (TimeOut), "EncryptionKey'" :: NullOrUndefined (NonEmptyString), "Tags'" :: NullOrUndefined (TagList), "VpcConfig'" :: NullOrUndefined (VpcConfig), "BadgeEnabled'" :: NullOrUndefined (WrapperBoolean) }
```

#### `CreateProjectOutput`

``` purescript
newtype CreateProjectOutput
  = CreateProjectOutput { "Project'" :: NullOrUndefined (Project) }
```

#### `CreateWebhookInput`

``` purescript
newtype CreateWebhookInput
  = CreateWebhookInput { "ProjectName'" :: ProjectName }
```

#### `CreateWebhookOutput`

``` purescript
newtype CreateWebhookOutput
  = CreateWebhookOutput { "Webhook'" :: NullOrUndefined (Webhook) }
```

#### `DeleteProjectInput`

``` purescript
newtype DeleteProjectInput
  = DeleteProjectInput { "Name'" :: NonEmptyString }
```

#### `DeleteProjectOutput`

``` purescript
newtype DeleteProjectOutput
  = DeleteProjectOutput {  }
```

#### `DeleteWebhookInput`

``` purescript
newtype DeleteWebhookInput
  = DeleteWebhookInput { "ProjectName'" :: ProjectName }
```

#### `DeleteWebhookOutput`

``` purescript
newtype DeleteWebhookOutput
  = DeleteWebhookOutput {  }
```

#### `EnvironmentImage`

``` purescript
newtype EnvironmentImage
  = EnvironmentImage { "Name'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "Versions'" :: NullOrUndefined (ImageVersions) }
```

<p>Information about a Docker image that is managed by AWS CodeBuild.</p>

#### `EnvironmentImages`

``` purescript
newtype EnvironmentImages
  = EnvironmentImages (Array EnvironmentImage)
```

#### `EnvironmentLanguage`

``` purescript
newtype EnvironmentLanguage
  = EnvironmentLanguage { "Language'" :: NullOrUndefined (LanguageType), "Images'" :: NullOrUndefined (EnvironmentImages) }
```

<p>A set of Docker images that are related by programming language and are managed by AWS CodeBuild.</p>

#### `EnvironmentLanguages`

``` purescript
newtype EnvironmentLanguages
  = EnvironmentLanguages (Array EnvironmentLanguage)
```

#### `EnvironmentPlatform`

``` purescript
newtype EnvironmentPlatform
  = EnvironmentPlatform { "Platform'" :: NullOrUndefined (PlatformType), "Languages'" :: NullOrUndefined (EnvironmentLanguages) }
```

<p>A set of Docker images that are related by platform and are managed by AWS CodeBuild.</p>

#### `EnvironmentPlatforms`

``` purescript
newtype EnvironmentPlatforms
  = EnvironmentPlatforms (Array EnvironmentPlatform)
```

#### `EnvironmentType`

``` purescript
newtype EnvironmentType
  = EnvironmentType String
```

#### `EnvironmentVariable`

``` purescript
newtype EnvironmentVariable
  = EnvironmentVariable { "Name'" :: NonEmptyString, "Value'" :: String, "Type'" :: NullOrUndefined (EnvironmentVariableType) }
```

<p>Information about an environment variable for a build project or a build.</p>

#### `EnvironmentVariableType`

``` purescript
newtype EnvironmentVariableType
  = EnvironmentVariableType String
```

#### `EnvironmentVariables`

``` purescript
newtype EnvironmentVariables
  = EnvironmentVariables (Array EnvironmentVariable)
```

#### `GitCloneDepth`

``` purescript
newtype GitCloneDepth
  = GitCloneDepth Int
```

#### `ImageVersions`

``` purescript
newtype ImageVersions
  = ImageVersions (Array String)
```

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException {  }
```

<p>The input value that was provided is not valid.</p>

#### `InvalidateProjectCacheInput`

``` purescript
newtype InvalidateProjectCacheInput
  = InvalidateProjectCacheInput { "ProjectName'" :: NonEmptyString }
```

#### `InvalidateProjectCacheOutput`

``` purescript
newtype InvalidateProjectCacheOutput
  = InvalidateProjectCacheOutput {  }
```

#### `KeyInput`

``` purescript
newtype KeyInput
  = KeyInput String
```

#### `LanguageType`

``` purescript
newtype LanguageType
  = LanguageType String
```

#### `ListBuildsForProjectInput`

``` purescript
newtype ListBuildsForProjectInput
  = ListBuildsForProjectInput { "ProjectName'" :: NonEmptyString, "SortOrder'" :: NullOrUndefined (SortOrderType), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListBuildsForProjectOutput`

``` purescript
newtype ListBuildsForProjectOutput
  = ListBuildsForProjectOutput { "Ids'" :: NullOrUndefined (BuildIds), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListBuildsInput`

``` purescript
newtype ListBuildsInput
  = ListBuildsInput { "SortOrder'" :: NullOrUndefined (SortOrderType), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListBuildsOutput`

``` purescript
newtype ListBuildsOutput
  = ListBuildsOutput { "Ids'" :: NullOrUndefined (BuildIds), "NextToken'" :: NullOrUndefined (String) }
```

#### `ListCuratedEnvironmentImagesInput`

``` purescript
newtype ListCuratedEnvironmentImagesInput
  = ListCuratedEnvironmentImagesInput {  }
```

#### `ListCuratedEnvironmentImagesOutput`

``` purescript
newtype ListCuratedEnvironmentImagesOutput
  = ListCuratedEnvironmentImagesOutput { "Platforms'" :: NullOrUndefined (EnvironmentPlatforms) }
```

#### `ListProjectsInput`

``` purescript
newtype ListProjectsInput
  = ListProjectsInput { "SortBy'" :: NullOrUndefined (ProjectSortByType), "SortOrder'" :: NullOrUndefined (SortOrderType), "NextToken'" :: NullOrUndefined (NonEmptyString) }
```

#### `ListProjectsOutput`

``` purescript
newtype ListProjectsOutput
  = ListProjectsOutput { "NextToken'" :: NullOrUndefined (String), "Projects'" :: NullOrUndefined (ProjectNames) }
```

#### `LogsLocation`

``` purescript
newtype LogsLocation
  = LogsLocation { "GroupName'" :: NullOrUndefined (String), "StreamName'" :: NullOrUndefined (String), "DeepLink'" :: NullOrUndefined (String) }
```

<p>Information about build logs in Amazon CloudWatch Logs.</p>

#### `NetworkInterface`

``` purescript
newtype NetworkInterface
  = NetworkInterface { "SubnetId'" :: NullOrUndefined (NonEmptyString), "NetworkInterfaceId'" :: NullOrUndefined (NonEmptyString) }
```

<p>Describes a network interface.</p>

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

#### `OAuthProviderException`

``` purescript
newtype OAuthProviderException
  = OAuthProviderException {  }
```

<p>There was a problem with the underlying OAuth provider.</p>

#### `PhaseContext`

``` purescript
newtype PhaseContext
  = PhaseContext { "StatusCode'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String) }
```

<p>Additional information about a build phase that has an error. You can use this information to help troubleshoot a failed build.</p>

#### `PhaseContexts`

``` purescript
newtype PhaseContexts
  = PhaseContexts (Array PhaseContext)
```

#### `PlatformType`

``` purescript
newtype PlatformType
  = PlatformType String
```

#### `Project`

``` purescript
newtype Project
  = Project { "Name'" :: NullOrUndefined (ProjectName), "Arn'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (ProjectDescription), "Source'" :: NullOrUndefined (ProjectSource), "Artifacts'" :: NullOrUndefined (ProjectArtifacts), "Cache'" :: NullOrUndefined (ProjectCache), "Environment'" :: NullOrUndefined (ProjectEnvironment), "ServiceRole'" :: NullOrUndefined (NonEmptyString), "TimeoutInMinutes'" :: NullOrUndefined (TimeOut), "EncryptionKey'" :: NullOrUndefined (NonEmptyString), "Tags'" :: NullOrUndefined (TagList), "Created'" :: NullOrUndefined (Number), "LastModified'" :: NullOrUndefined (Number), "Webhook'" :: NullOrUndefined (Webhook), "VpcConfig'" :: NullOrUndefined (VpcConfig), "Badge'" :: NullOrUndefined (ProjectBadge) }
```

<p>Information about a build project.</p>

#### `ProjectArtifacts`

``` purescript
newtype ProjectArtifacts
  = ProjectArtifacts { "Type'" :: ArtifactsType, "Location'" :: NullOrUndefined (String), "Path'" :: NullOrUndefined (String), "NamespaceType'" :: NullOrUndefined (ArtifactNamespace), "Name'" :: NullOrUndefined (String), "Packaging'" :: NullOrUndefined (ArtifactPackaging) }
```

<p>Information about the build output artifacts for the build project.</p>

#### `ProjectBadge`

``` purescript
newtype ProjectBadge
  = ProjectBadge { "BadgeEnabled'" :: NullOrUndefined (Boolean), "BadgeRequestUrl'" :: NullOrUndefined (String) }
```

<p>Information about the build badge for the build project.</p>

#### `ProjectCache`

``` purescript
newtype ProjectCache
  = ProjectCache { "Type'" :: CacheType, "Location'" :: NullOrUndefined (String) }
```

<p>Information about the cache for the build project.</p>

#### `ProjectDescription`

``` purescript
newtype ProjectDescription
  = ProjectDescription String
```

#### `ProjectEnvironment`

``` purescript
newtype ProjectEnvironment
  = ProjectEnvironment { "Type'" :: EnvironmentType, "Image'" :: NonEmptyString, "ComputeType'" :: ComputeType, "EnvironmentVariables'" :: NullOrUndefined (EnvironmentVariables), "PrivilegedMode'" :: NullOrUndefined (WrapperBoolean), "Certificate'" :: NullOrUndefined (String) }
```

<p>Information about the build environment of the build project.</p>

#### `ProjectName`

``` purescript
newtype ProjectName
  = ProjectName String
```

#### `ProjectNames`

``` purescript
newtype ProjectNames
  = ProjectNames (Array NonEmptyString)
```

#### `ProjectSortByType`

``` purescript
newtype ProjectSortByType
  = ProjectSortByType String
```

#### `ProjectSource`

``` purescript
newtype ProjectSource
  = ProjectSource { "Type'" :: SourceType, "Location'" :: NullOrUndefined (String), "GitCloneDepth'" :: NullOrUndefined (GitCloneDepth), "Buildspec'" :: NullOrUndefined (String), "Auth'" :: NullOrUndefined (SourceAuth), "InsecureSsl'" :: NullOrUndefined (WrapperBoolean) }
```

<p>Information about the build input source code for the build project.</p>

#### `Projects`

``` purescript
newtype Projects
  = Projects (Array Project)
```

#### `ResourceAlreadyExistsException`

``` purescript
newtype ResourceAlreadyExistsException
  = ResourceAlreadyExistsException {  }
```

<p>The specified AWS resource cannot be created, because an AWS resource with the same settings already exists.</p>

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>The specified AWS resource cannot be found.</p>

#### `SecurityGroupIds`

``` purescript
newtype SecurityGroupIds
  = SecurityGroupIds (Array NonEmptyString)
```

#### `SortOrderType`

``` purescript
newtype SortOrderType
  = SortOrderType String
```

#### `SourceAuth`

``` purescript
newtype SourceAuth
  = SourceAuth { "Type'" :: SourceAuthType, "Resource'" :: NullOrUndefined (String) }
```

<p>Information about the authorization settings for AWS CodeBuild to access the source code to be built.</p> <p>This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly (unless the build project's source <code>type</code> value is <code>BITBUCKET</code> or <code>GITHUB</code>).</p>

#### `SourceAuthType`

``` purescript
newtype SourceAuthType
  = SourceAuthType String
```

#### `SourceType`

``` purescript
newtype SourceType
  = SourceType String
```

#### `StartBuildInput`

``` purescript
newtype StartBuildInput
  = StartBuildInput { "ProjectName'" :: NonEmptyString, "SourceVersion'" :: NullOrUndefined (String), "ArtifactsOverride'" :: NullOrUndefined (ProjectArtifacts), "EnvironmentVariablesOverride'" :: NullOrUndefined (EnvironmentVariables), "GitCloneDepthOverride'" :: NullOrUndefined (GitCloneDepth), "BuildspecOverride'" :: NullOrUndefined (String), "TimeoutInMinutesOverride'" :: NullOrUndefined (TimeOut) }
```

#### `StartBuildOutput`

``` purescript
newtype StartBuildOutput
  = StartBuildOutput { "Build'" :: NullOrUndefined (Build) }
```

#### `StatusType`

``` purescript
newtype StatusType
  = StatusType String
```

#### `StopBuildInput`

``` purescript
newtype StopBuildInput
  = StopBuildInput { "Id'" :: NonEmptyString }
```

#### `StopBuildOutput`

``` purescript
newtype StopBuildOutput
  = StopBuildOutput { "Build'" :: NullOrUndefined (Build) }
```

#### `Subnets`

``` purescript
newtype Subnets
  = Subnets (Array NonEmptyString)
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key'" :: NullOrUndefined (KeyInput), "Value'" :: NullOrUndefined (ValueInput) }
```

<p>A tag, consisting of a key and a value.</p> <p>This tag is available for use by AWS services that support tags in AWS CodeBuild.</p>

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TimeOut`

``` purescript
newtype TimeOut
  = TimeOut Int
```

#### `UpdateProjectInput`

``` purescript
newtype UpdateProjectInput
  = UpdateProjectInput { "Name'" :: NonEmptyString, "Description'" :: NullOrUndefined (ProjectDescription), "Source'" :: NullOrUndefined (ProjectSource), "Artifacts'" :: NullOrUndefined (ProjectArtifacts), "Cache'" :: NullOrUndefined (ProjectCache), "Environment'" :: NullOrUndefined (ProjectEnvironment), "ServiceRole'" :: NullOrUndefined (NonEmptyString), "TimeoutInMinutes'" :: NullOrUndefined (TimeOut), "EncryptionKey'" :: NullOrUndefined (NonEmptyString), "Tags'" :: NullOrUndefined (TagList), "VpcConfig'" :: NullOrUndefined (VpcConfig), "BadgeEnabled'" :: NullOrUndefined (WrapperBoolean) }
```

#### `UpdateProjectOutput`

``` purescript
newtype UpdateProjectOutput
  = UpdateProjectOutput { "Project'" :: NullOrUndefined (Project) }
```

#### `ValueInput`

``` purescript
newtype ValueInput
  = ValueInput String
```

#### `VpcConfig`

``` purescript
newtype VpcConfig
  = VpcConfig { "VpcId'" :: NullOrUndefined (NonEmptyString), "Subnets'" :: NullOrUndefined (Subnets), "SecurityGroupIds'" :: NullOrUndefined (SecurityGroupIds) }
```

<p>Information about the VPC configuration that AWS CodeBuild will access.</p>

#### `Webhook`

``` purescript
newtype Webhook
  = Webhook { "Url'" :: NullOrUndefined (NonEmptyString), "PayloadUrl'" :: NullOrUndefined (NonEmptyString), "Secret'" :: NullOrUndefined (NonEmptyString) }
```

<p>Information about a webhook in GitHub that connects repository events to a build project in AWS CodeBuild.</p>

#### `WrapperBoolean`

``` purescript
newtype WrapperBoolean
  = WrapperBoolean Boolean
```

#### `WrapperInt`

``` purescript
newtype WrapperInt
  = WrapperInt Int
```

#### `WrapperLong`

``` purescript
newtype WrapperLong
  = WrapperLong Number
```


