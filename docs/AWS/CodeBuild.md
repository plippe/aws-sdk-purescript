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

##### Instances
``` purescript
Newtype AccountLimitExceededException _
```

#### `ArtifactNamespace`

``` purescript
newtype ArtifactNamespace
  = ArtifactNamespace String
```

##### Instances
``` purescript
Newtype ArtifactNamespace _
```

#### `ArtifactPackaging`

``` purescript
newtype ArtifactPackaging
  = ArtifactPackaging String
```

##### Instances
``` purescript
Newtype ArtifactPackaging _
```

#### `ArtifactsType`

``` purescript
newtype ArtifactsType
  = ArtifactsType String
```

##### Instances
``` purescript
Newtype ArtifactsType _
```

#### `BatchDeleteBuildsInput`

``` purescript
newtype BatchDeleteBuildsInput
  = BatchDeleteBuildsInput { "Ids'" :: BuildIds }
```

##### Instances
``` purescript
Newtype BatchDeleteBuildsInput _
```

#### `BatchDeleteBuildsOutput`

``` purescript
newtype BatchDeleteBuildsOutput
  = BatchDeleteBuildsOutput { "BuildsDeleted'" :: NullOrUndefined (BuildIds), "BuildsNotDeleted'" :: NullOrUndefined (BuildsNotDeleted) }
```

##### Instances
``` purescript
Newtype BatchDeleteBuildsOutput _
```

#### `BatchGetBuildsInput`

``` purescript
newtype BatchGetBuildsInput
  = BatchGetBuildsInput { "Ids'" :: BuildIds }
```

##### Instances
``` purescript
Newtype BatchGetBuildsInput _
```

#### `BatchGetBuildsOutput`

``` purescript
newtype BatchGetBuildsOutput
  = BatchGetBuildsOutput { "Builds'" :: NullOrUndefined (Builds), "BuildsNotFound'" :: NullOrUndefined (BuildIds) }
```

##### Instances
``` purescript
Newtype BatchGetBuildsOutput _
```

#### `BatchGetProjectsInput`

``` purescript
newtype BatchGetProjectsInput
  = BatchGetProjectsInput { "Names'" :: ProjectNames }
```

##### Instances
``` purescript
Newtype BatchGetProjectsInput _
```

#### `BatchGetProjectsOutput`

``` purescript
newtype BatchGetProjectsOutput
  = BatchGetProjectsOutput { "Projects'" :: NullOrUndefined (Projects), "ProjectsNotFound'" :: NullOrUndefined (ProjectNames) }
```

##### Instances
``` purescript
Newtype BatchGetProjectsOutput _
```

#### `Build`

``` purescript
newtype Build
  = Build { "Id'" :: NullOrUndefined (NonEmptyString), "Arn'" :: NullOrUndefined (NonEmptyString), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "CurrentPhase'" :: NullOrUndefined (String), "BuildStatus'" :: NullOrUndefined (StatusType), "SourceVersion'" :: NullOrUndefined (NonEmptyString), "ProjectName'" :: NullOrUndefined (NonEmptyString), "Phases'" :: NullOrUndefined (BuildPhases), "Source'" :: NullOrUndefined (ProjectSource), "Artifacts'" :: NullOrUndefined (BuildArtifacts), "Cache'" :: NullOrUndefined (ProjectCache), "Environment'" :: NullOrUndefined (ProjectEnvironment), "Logs'" :: NullOrUndefined (LogsLocation), "TimeoutInMinutes'" :: NullOrUndefined (WrapperInt), "BuildComplete'" :: NullOrUndefined (Boolean), "Initiator'" :: NullOrUndefined (String), "VpcConfig'" :: NullOrUndefined (VpcConfig), "NetworkInterface'" :: NullOrUndefined (NetworkInterface) }
```

<p>Information about a build.</p>

##### Instances
``` purescript
Newtype Build _
```

#### `BuildArtifacts`

``` purescript
newtype BuildArtifacts
  = BuildArtifacts { "Location'" :: NullOrUndefined (String), "Sha256sum'" :: NullOrUndefined (String), "Md5sum'" :: NullOrUndefined (String) }
```

<p>Information about build output artifacts.</p>

##### Instances
``` purescript
Newtype BuildArtifacts _
```

#### `BuildIds`

``` purescript
newtype BuildIds
  = BuildIds (Array NonEmptyString)
```

##### Instances
``` purescript
Newtype BuildIds _
```

#### `BuildNotDeleted`

``` purescript
newtype BuildNotDeleted
  = BuildNotDeleted { "Id'" :: NullOrUndefined (NonEmptyString), "StatusCode'" :: NullOrUndefined (String) }
```

<p>Information about a build that could not be successfully deleted.</p>

##### Instances
``` purescript
Newtype BuildNotDeleted _
```

#### `BuildPhase`

``` purescript
newtype BuildPhase
  = BuildPhase { "PhaseType'" :: NullOrUndefined (BuildPhaseType), "PhaseStatus'" :: NullOrUndefined (StatusType), "StartTime'" :: NullOrUndefined (Number), "EndTime'" :: NullOrUndefined (Number), "DurationInSeconds'" :: NullOrUndefined (WrapperLong), "Contexts'" :: NullOrUndefined (PhaseContexts) }
```

<p>Information about a stage for a build.</p>

##### Instances
``` purescript
Newtype BuildPhase _
```

#### `BuildPhaseType`

``` purescript
newtype BuildPhaseType
  = BuildPhaseType String
```

##### Instances
``` purescript
Newtype BuildPhaseType _
```

#### `BuildPhases`

``` purescript
newtype BuildPhases
  = BuildPhases (Array BuildPhase)
```

##### Instances
``` purescript
Newtype BuildPhases _
```

#### `Builds`

``` purescript
newtype Builds
  = Builds (Array Build)
```

##### Instances
``` purescript
Newtype Builds _
```

#### `BuildsNotDeleted`

``` purescript
newtype BuildsNotDeleted
  = BuildsNotDeleted (Array BuildNotDeleted)
```

##### Instances
``` purescript
Newtype BuildsNotDeleted _
```

#### `CacheType`

``` purescript
newtype CacheType
  = CacheType String
```

##### Instances
``` purescript
Newtype CacheType _
```

#### `ComputeType`

``` purescript
newtype ComputeType
  = ComputeType String
```

##### Instances
``` purescript
Newtype ComputeType _
```

#### `CreateProjectInput`

``` purescript
newtype CreateProjectInput
  = CreateProjectInput { "Name'" :: ProjectName, "Description'" :: NullOrUndefined (ProjectDescription), "Source'" :: ProjectSource, "Artifacts'" :: ProjectArtifacts, "Cache'" :: NullOrUndefined (ProjectCache), "Environment'" :: ProjectEnvironment, "ServiceRole'" :: NullOrUndefined (NonEmptyString), "TimeoutInMinutes'" :: NullOrUndefined (TimeOut), "EncryptionKey'" :: NullOrUndefined (NonEmptyString), "Tags'" :: NullOrUndefined (TagList), "VpcConfig'" :: NullOrUndefined (VpcConfig), "BadgeEnabled'" :: NullOrUndefined (WrapperBoolean) }
```

##### Instances
``` purescript
Newtype CreateProjectInput _
```

#### `CreateProjectOutput`

``` purescript
newtype CreateProjectOutput
  = CreateProjectOutput { "Project'" :: NullOrUndefined (Project) }
```

##### Instances
``` purescript
Newtype CreateProjectOutput _
```

#### `CreateWebhookInput`

``` purescript
newtype CreateWebhookInput
  = CreateWebhookInput { "ProjectName'" :: ProjectName }
```

##### Instances
``` purescript
Newtype CreateWebhookInput _
```

#### `CreateWebhookOutput`

``` purescript
newtype CreateWebhookOutput
  = CreateWebhookOutput { "Webhook'" :: NullOrUndefined (Webhook) }
```

##### Instances
``` purescript
Newtype CreateWebhookOutput _
```

#### `DeleteProjectInput`

``` purescript
newtype DeleteProjectInput
  = DeleteProjectInput { "Name'" :: NonEmptyString }
```

##### Instances
``` purescript
Newtype DeleteProjectInput _
```

#### `DeleteProjectOutput`

``` purescript
newtype DeleteProjectOutput
  = DeleteProjectOutput {  }
```

##### Instances
``` purescript
Newtype DeleteProjectOutput _
```

#### `DeleteWebhookInput`

``` purescript
newtype DeleteWebhookInput
  = DeleteWebhookInput { "ProjectName'" :: ProjectName }
```

##### Instances
``` purescript
Newtype DeleteWebhookInput _
```

#### `DeleteWebhookOutput`

``` purescript
newtype DeleteWebhookOutput
  = DeleteWebhookOutput {  }
```

##### Instances
``` purescript
Newtype DeleteWebhookOutput _
```

#### `EnvironmentImage`

``` purescript
newtype EnvironmentImage
  = EnvironmentImage { "Name'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (String), "Versions'" :: NullOrUndefined (ImageVersions) }
```

<p>Information about a Docker image that is managed by AWS CodeBuild.</p>

##### Instances
``` purescript
Newtype EnvironmentImage _
```

#### `EnvironmentImages`

``` purescript
newtype EnvironmentImages
  = EnvironmentImages (Array EnvironmentImage)
```

##### Instances
``` purescript
Newtype EnvironmentImages _
```

#### `EnvironmentLanguage`

``` purescript
newtype EnvironmentLanguage
  = EnvironmentLanguage { "Language'" :: NullOrUndefined (LanguageType), "Images'" :: NullOrUndefined (EnvironmentImages) }
```

<p>A set of Docker images that are related by programming language and are managed by AWS CodeBuild.</p>

##### Instances
``` purescript
Newtype EnvironmentLanguage _
```

#### `EnvironmentLanguages`

``` purescript
newtype EnvironmentLanguages
  = EnvironmentLanguages (Array EnvironmentLanguage)
```

##### Instances
``` purescript
Newtype EnvironmentLanguages _
```

#### `EnvironmentPlatform`

``` purescript
newtype EnvironmentPlatform
  = EnvironmentPlatform { "Platform'" :: NullOrUndefined (PlatformType), "Languages'" :: NullOrUndefined (EnvironmentLanguages) }
```

<p>A set of Docker images that are related by platform and are managed by AWS CodeBuild.</p>

##### Instances
``` purescript
Newtype EnvironmentPlatform _
```

#### `EnvironmentPlatforms`

``` purescript
newtype EnvironmentPlatforms
  = EnvironmentPlatforms (Array EnvironmentPlatform)
```

##### Instances
``` purescript
Newtype EnvironmentPlatforms _
```

#### `EnvironmentType`

``` purescript
newtype EnvironmentType
  = EnvironmentType String
```

##### Instances
``` purescript
Newtype EnvironmentType _
```

#### `EnvironmentVariable`

``` purescript
newtype EnvironmentVariable
  = EnvironmentVariable { "Name'" :: NonEmptyString, "Value'" :: String, "Type'" :: NullOrUndefined (EnvironmentVariableType) }
```

<p>Information about an environment variable for a build project or a build.</p>

##### Instances
``` purescript
Newtype EnvironmentVariable _
```

#### `EnvironmentVariableType`

``` purescript
newtype EnvironmentVariableType
  = EnvironmentVariableType String
```

##### Instances
``` purescript
Newtype EnvironmentVariableType _
```

#### `EnvironmentVariables`

``` purescript
newtype EnvironmentVariables
  = EnvironmentVariables (Array EnvironmentVariable)
```

##### Instances
``` purescript
Newtype EnvironmentVariables _
```

#### `GitCloneDepth`

``` purescript
newtype GitCloneDepth
  = GitCloneDepth Int
```

##### Instances
``` purescript
Newtype GitCloneDepth _
```

#### `ImageVersions`

``` purescript
newtype ImageVersions
  = ImageVersions (Array String)
```

##### Instances
``` purescript
Newtype ImageVersions _
```

#### `InvalidInputException`

``` purescript
newtype InvalidInputException
  = InvalidInputException {  }
```

<p>The input value that was provided is not valid.</p>

##### Instances
``` purescript
Newtype InvalidInputException _
```

#### `InvalidateProjectCacheInput`

``` purescript
newtype InvalidateProjectCacheInput
  = InvalidateProjectCacheInput { "ProjectName'" :: NonEmptyString }
```

##### Instances
``` purescript
Newtype InvalidateProjectCacheInput _
```

#### `InvalidateProjectCacheOutput`

``` purescript
newtype InvalidateProjectCacheOutput
  = InvalidateProjectCacheOutput {  }
```

##### Instances
``` purescript
Newtype InvalidateProjectCacheOutput _
```

#### `KeyInput`

``` purescript
newtype KeyInput
  = KeyInput String
```

##### Instances
``` purescript
Newtype KeyInput _
```

#### `LanguageType`

``` purescript
newtype LanguageType
  = LanguageType String
```

##### Instances
``` purescript
Newtype LanguageType _
```

#### `ListBuildsForProjectInput`

``` purescript
newtype ListBuildsForProjectInput
  = ListBuildsForProjectInput { "ProjectName'" :: NonEmptyString, "SortOrder'" :: NullOrUndefined (SortOrderType), "NextToken'" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListBuildsForProjectInput _
```

#### `ListBuildsForProjectOutput`

``` purescript
newtype ListBuildsForProjectOutput
  = ListBuildsForProjectOutput { "Ids'" :: NullOrUndefined (BuildIds), "NextToken'" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListBuildsForProjectOutput _
```

#### `ListBuildsInput`

``` purescript
newtype ListBuildsInput
  = ListBuildsInput { "SortOrder'" :: NullOrUndefined (SortOrderType), "NextToken'" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListBuildsInput _
```

#### `ListBuildsOutput`

``` purescript
newtype ListBuildsOutput
  = ListBuildsOutput { "Ids'" :: NullOrUndefined (BuildIds), "NextToken'" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListBuildsOutput _
```

#### `ListCuratedEnvironmentImagesInput`

``` purescript
newtype ListCuratedEnvironmentImagesInput
  = ListCuratedEnvironmentImagesInput {  }
```

##### Instances
``` purescript
Newtype ListCuratedEnvironmentImagesInput _
```

#### `ListCuratedEnvironmentImagesOutput`

``` purescript
newtype ListCuratedEnvironmentImagesOutput
  = ListCuratedEnvironmentImagesOutput { "Platforms'" :: NullOrUndefined (EnvironmentPlatforms) }
```

##### Instances
``` purescript
Newtype ListCuratedEnvironmentImagesOutput _
```

#### `ListProjectsInput`

``` purescript
newtype ListProjectsInput
  = ListProjectsInput { "SortBy'" :: NullOrUndefined (ProjectSortByType), "SortOrder'" :: NullOrUndefined (SortOrderType), "NextToken'" :: NullOrUndefined (NonEmptyString) }
```

##### Instances
``` purescript
Newtype ListProjectsInput _
```

#### `ListProjectsOutput`

``` purescript
newtype ListProjectsOutput
  = ListProjectsOutput { "NextToken'" :: NullOrUndefined (String), "Projects'" :: NullOrUndefined (ProjectNames) }
```

##### Instances
``` purescript
Newtype ListProjectsOutput _
```

#### `LogsLocation`

``` purescript
newtype LogsLocation
  = LogsLocation { "GroupName'" :: NullOrUndefined (String), "StreamName'" :: NullOrUndefined (String), "DeepLink'" :: NullOrUndefined (String) }
```

<p>Information about build logs in Amazon CloudWatch Logs.</p>

##### Instances
``` purescript
Newtype LogsLocation _
```

#### `NetworkInterface`

``` purescript
newtype NetworkInterface
  = NetworkInterface { "SubnetId'" :: NullOrUndefined (NonEmptyString), "NetworkInterfaceId'" :: NullOrUndefined (NonEmptyString) }
```

<p>Describes a network interface.</p>

##### Instances
``` purescript
Newtype NetworkInterface _
```

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

##### Instances
``` purescript
Newtype NonEmptyString _
```

#### `OAuthProviderException`

``` purescript
newtype OAuthProviderException
  = OAuthProviderException {  }
```

<p>There was a problem with the underlying OAuth provider.</p>

##### Instances
``` purescript
Newtype OAuthProviderException _
```

#### `PhaseContext`

``` purescript
newtype PhaseContext
  = PhaseContext { "StatusCode'" :: NullOrUndefined (String), "Message'" :: NullOrUndefined (String) }
```

<p>Additional information about a build phase that has an error. You can use this information to help troubleshoot a failed build.</p>

##### Instances
``` purescript
Newtype PhaseContext _
```

#### `PhaseContexts`

``` purescript
newtype PhaseContexts
  = PhaseContexts (Array PhaseContext)
```

##### Instances
``` purescript
Newtype PhaseContexts _
```

#### `PlatformType`

``` purescript
newtype PlatformType
  = PlatformType String
```

##### Instances
``` purescript
Newtype PlatformType _
```

#### `Project`

``` purescript
newtype Project
  = Project { "Name'" :: NullOrUndefined (ProjectName), "Arn'" :: NullOrUndefined (String), "Description'" :: NullOrUndefined (ProjectDescription), "Source'" :: NullOrUndefined (ProjectSource), "Artifacts'" :: NullOrUndefined (ProjectArtifacts), "Cache'" :: NullOrUndefined (ProjectCache), "Environment'" :: NullOrUndefined (ProjectEnvironment), "ServiceRole'" :: NullOrUndefined (NonEmptyString), "TimeoutInMinutes'" :: NullOrUndefined (TimeOut), "EncryptionKey'" :: NullOrUndefined (NonEmptyString), "Tags'" :: NullOrUndefined (TagList), "Created'" :: NullOrUndefined (Number), "LastModified'" :: NullOrUndefined (Number), "Webhook'" :: NullOrUndefined (Webhook), "VpcConfig'" :: NullOrUndefined (VpcConfig), "Badge'" :: NullOrUndefined (ProjectBadge) }
```

<p>Information about a build project.</p>

##### Instances
``` purescript
Newtype Project _
```

#### `ProjectArtifacts`

``` purescript
newtype ProjectArtifacts
  = ProjectArtifacts { "Type'" :: ArtifactsType, "Location'" :: NullOrUndefined (String), "Path'" :: NullOrUndefined (String), "NamespaceType'" :: NullOrUndefined (ArtifactNamespace), "Name'" :: NullOrUndefined (String), "Packaging'" :: NullOrUndefined (ArtifactPackaging) }
```

<p>Information about the build output artifacts for the build project.</p>

##### Instances
``` purescript
Newtype ProjectArtifacts _
```

#### `ProjectBadge`

``` purescript
newtype ProjectBadge
  = ProjectBadge { "BadgeEnabled'" :: NullOrUndefined (Boolean), "BadgeRequestUrl'" :: NullOrUndefined (String) }
```

<p>Information about the build badge for the build project.</p>

##### Instances
``` purescript
Newtype ProjectBadge _
```

#### `ProjectCache`

``` purescript
newtype ProjectCache
  = ProjectCache { "Type'" :: CacheType, "Location'" :: NullOrUndefined (String) }
```

<p>Information about the cache for the build project.</p>

##### Instances
``` purescript
Newtype ProjectCache _
```

#### `ProjectDescription`

``` purescript
newtype ProjectDescription
  = ProjectDescription String
```

##### Instances
``` purescript
Newtype ProjectDescription _
```

#### `ProjectEnvironment`

``` purescript
newtype ProjectEnvironment
  = ProjectEnvironment { "Type'" :: EnvironmentType, "Image'" :: NonEmptyString, "ComputeType'" :: ComputeType, "EnvironmentVariables'" :: NullOrUndefined (EnvironmentVariables), "PrivilegedMode'" :: NullOrUndefined (WrapperBoolean), "Certificate'" :: NullOrUndefined (String) }
```

<p>Information about the build environment of the build project.</p>

##### Instances
``` purescript
Newtype ProjectEnvironment _
```

#### `ProjectName`

``` purescript
newtype ProjectName
  = ProjectName String
```

##### Instances
``` purescript
Newtype ProjectName _
```

#### `ProjectNames`

``` purescript
newtype ProjectNames
  = ProjectNames (Array NonEmptyString)
```

##### Instances
``` purescript
Newtype ProjectNames _
```

#### `ProjectSortByType`

``` purescript
newtype ProjectSortByType
  = ProjectSortByType String
```

##### Instances
``` purescript
Newtype ProjectSortByType _
```

#### `ProjectSource`

``` purescript
newtype ProjectSource
  = ProjectSource { "Type'" :: SourceType, "Location'" :: NullOrUndefined (String), "GitCloneDepth'" :: NullOrUndefined (GitCloneDepth), "Buildspec'" :: NullOrUndefined (String), "Auth'" :: NullOrUndefined (SourceAuth), "InsecureSsl'" :: NullOrUndefined (WrapperBoolean) }
```

<p>Information about the build input source code for the build project.</p>

##### Instances
``` purescript
Newtype ProjectSource _
```

#### `Projects`

``` purescript
newtype Projects
  = Projects (Array Project)
```

##### Instances
``` purescript
Newtype Projects _
```

#### `ResourceAlreadyExistsException`

``` purescript
newtype ResourceAlreadyExistsException
  = ResourceAlreadyExistsException {  }
```

<p>The specified AWS resource cannot be created, because an AWS resource with the same settings already exists.</p>

##### Instances
``` purescript
Newtype ResourceAlreadyExistsException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException {  }
```

<p>The specified AWS resource cannot be found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `SecurityGroupIds`

``` purescript
newtype SecurityGroupIds
  = SecurityGroupIds (Array NonEmptyString)
```

##### Instances
``` purescript
Newtype SecurityGroupIds _
```

#### `SortOrderType`

``` purescript
newtype SortOrderType
  = SortOrderType String
```

##### Instances
``` purescript
Newtype SortOrderType _
```

#### `SourceAuth`

``` purescript
newtype SourceAuth
  = SourceAuth { "Type'" :: SourceAuthType, "Resource'" :: NullOrUndefined (String) }
```

<p>Information about the authorization settings for AWS CodeBuild to access the source code to be built.</p> <p>This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly (unless the build project's source <code>type</code> value is <code>BITBUCKET</code> or <code>GITHUB</code>).</p>

##### Instances
``` purescript
Newtype SourceAuth _
```

#### `SourceAuthType`

``` purescript
newtype SourceAuthType
  = SourceAuthType String
```

##### Instances
``` purescript
Newtype SourceAuthType _
```

#### `SourceType`

``` purescript
newtype SourceType
  = SourceType String
```

##### Instances
``` purescript
Newtype SourceType _
```

#### `StartBuildInput`

``` purescript
newtype StartBuildInput
  = StartBuildInput { "ProjectName'" :: NonEmptyString, "SourceVersion'" :: NullOrUndefined (String), "ArtifactsOverride'" :: NullOrUndefined (ProjectArtifacts), "EnvironmentVariablesOverride'" :: NullOrUndefined (EnvironmentVariables), "GitCloneDepthOverride'" :: NullOrUndefined (GitCloneDepth), "BuildspecOverride'" :: NullOrUndefined (String), "TimeoutInMinutesOverride'" :: NullOrUndefined (TimeOut) }
```

##### Instances
``` purescript
Newtype StartBuildInput _
```

#### `StartBuildOutput`

``` purescript
newtype StartBuildOutput
  = StartBuildOutput { "Build'" :: NullOrUndefined (Build) }
```

##### Instances
``` purescript
Newtype StartBuildOutput _
```

#### `StatusType`

``` purescript
newtype StatusType
  = StatusType String
```

##### Instances
``` purescript
Newtype StatusType _
```

#### `StopBuildInput`

``` purescript
newtype StopBuildInput
  = StopBuildInput { "Id'" :: NonEmptyString }
```

##### Instances
``` purescript
Newtype StopBuildInput _
```

#### `StopBuildOutput`

``` purescript
newtype StopBuildOutput
  = StopBuildOutput { "Build'" :: NullOrUndefined (Build) }
```

##### Instances
``` purescript
Newtype StopBuildOutput _
```

#### `Subnets`

``` purescript
newtype Subnets
  = Subnets (Array NonEmptyString)
```

##### Instances
``` purescript
Newtype Subnets _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key'" :: NullOrUndefined (KeyInput), "Value'" :: NullOrUndefined (ValueInput) }
```

<p>A tag, consisting of a key and a value.</p> <p>This tag is available for use by AWS services that support tags in AWS CodeBuild.</p>

##### Instances
``` purescript
Newtype Tag _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
```

#### `TimeOut`

``` purescript
newtype TimeOut
  = TimeOut Int
```

##### Instances
``` purescript
Newtype TimeOut _
```

#### `UpdateProjectInput`

``` purescript
newtype UpdateProjectInput
  = UpdateProjectInput { "Name'" :: NonEmptyString, "Description'" :: NullOrUndefined (ProjectDescription), "Source'" :: NullOrUndefined (ProjectSource), "Artifacts'" :: NullOrUndefined (ProjectArtifacts), "Cache'" :: NullOrUndefined (ProjectCache), "Environment'" :: NullOrUndefined (ProjectEnvironment), "ServiceRole'" :: NullOrUndefined (NonEmptyString), "TimeoutInMinutes'" :: NullOrUndefined (TimeOut), "EncryptionKey'" :: NullOrUndefined (NonEmptyString), "Tags'" :: NullOrUndefined (TagList), "VpcConfig'" :: NullOrUndefined (VpcConfig), "BadgeEnabled'" :: NullOrUndefined (WrapperBoolean) }
```

##### Instances
``` purescript
Newtype UpdateProjectInput _
```

#### `UpdateProjectOutput`

``` purescript
newtype UpdateProjectOutput
  = UpdateProjectOutput { "Project'" :: NullOrUndefined (Project) }
```

##### Instances
``` purescript
Newtype UpdateProjectOutput _
```

#### `ValueInput`

``` purescript
newtype ValueInput
  = ValueInput String
```

##### Instances
``` purescript
Newtype ValueInput _
```

#### `VpcConfig`

``` purescript
newtype VpcConfig
  = VpcConfig { "VpcId'" :: NullOrUndefined (NonEmptyString), "Subnets'" :: NullOrUndefined (Subnets), "SecurityGroupIds'" :: NullOrUndefined (SecurityGroupIds) }
```

<p>Information about the VPC configuration that AWS CodeBuild will access.</p>

##### Instances
``` purescript
Newtype VpcConfig _
```

#### `Webhook`

``` purescript
newtype Webhook
  = Webhook { "Url'" :: NullOrUndefined (NonEmptyString), "PayloadUrl'" :: NullOrUndefined (NonEmptyString), "Secret'" :: NullOrUndefined (NonEmptyString) }
```

<p>Information about a webhook in GitHub that connects repository events to a build project in AWS CodeBuild.</p>

##### Instances
``` purescript
Newtype Webhook _
```

#### `WrapperBoolean`

``` purescript
newtype WrapperBoolean
  = WrapperBoolean Boolean
```

##### Instances
``` purescript
Newtype WrapperBoolean _
```

#### `WrapperInt`

``` purescript
newtype WrapperInt
  = WrapperInt Int
```

##### Instances
``` purescript
Newtype WrapperInt _
```

#### `WrapperLong`

``` purescript
newtype WrapperLong
  = WrapperLong Number
```

##### Instances
``` purescript
Newtype WrapperLong _
```


