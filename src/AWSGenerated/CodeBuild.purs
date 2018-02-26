

-- | <fullname>AWS CodeBuild</fullname> <p>AWS CodeBuild is a fully managed build service in the cloud. AWS CodeBuild compiles your source code, runs unit tests, and produces artifacts that are ready to deploy. AWS CodeBuild eliminates the need to provision, manage, and scale your own build servers. It provides prepackaged build environments for the most popular programming languages and build tools, such as Apache Maven, Gradle, and more. You can also fully customize build environments in AWS CodeBuild to use your own build tools. AWS CodeBuild scales automatically to meet peak build requests, and you pay only for the build time you consume. For more information about AWS CodeBuild, see the <i>AWS CodeBuild User Guide</i>.</p> <p>AWS CodeBuild supports these operations:</p> <ul> <li> <p> <code>BatchDeleteBuilds</code>: Deletes one or more builds.</p> </li> <li> <p> <code>BatchGetProjects</code>: Gets information about one or more build projects. A <i>build project</i> defines how AWS CodeBuild will run a build. This includes information such as where to get the source code to build, the build environment to use, the build commands to run, and where to store the build output. A <i>build environment</i> represents a combination of operating system, programming language runtime, and tools that AWS CodeBuild will use to run a build. Also, you can add tags to build projects to help manage your resources and costs.</p> </li> <li> <p> <code>CreateProject</code>: Creates a build project.</p> </li> <li> <p> <code>CreateWebhook</code>: For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, enables AWS CodeBuild to begin automatically rebuilding the source code every time a code change is pushed to the repository.</p> </li> <li> <p> <code>DeleteProject</code>: Deletes a build project.</p> </li> <li> <p> <code>DeleteWebhook</code>: For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, stops AWS CodeBuild from automatically rebuilding the source code every time a code change is pushed to the repository.</p> </li> <li> <p> <code>ListProjects</code>: Gets a list of build project names, with each build project name representing a single build project.</p> </li> <li> <p> <code>UpdateProject</code>: Changes the settings of an existing build project.</p> </li> <li> <p> <code>BatchGetBuilds</code>: Gets information about one or more builds.</p> </li> <li> <p> <code>ListBuilds</code>: Gets a list of build IDs, with each build ID representing a single build.</p> </li> <li> <p> <code>ListBuildsForProject</code>: Gets a list of build IDs for the specified build project, with each build ID representing a single build.</p> </li> <li> <p> <code>StartBuild</code>: Starts running a build.</p> </li> <li> <p> <code>StopBuild</code>: Attempts to stop running a build.</p> </li> <li> <p> <code>ListCuratedEnvironmentImages</code>: Gets information about Docker images that are managed by AWS CodeBuild.</p> </li> </ul>
module AWS.CodeBuild where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "CodeBuild" :: String


-- | <p>Deletes one or more builds.</p>
batchDeleteBuilds :: forall eff. BatchDeleteBuildsInput -> Aff (err :: AWS.RequestError | eff) BatchDeleteBuildsOutput
batchDeleteBuilds = AWS.request serviceName "BatchDeleteBuilds" 


-- | <p>Gets information about builds.</p>
batchGetBuilds :: forall eff. BatchGetBuildsInput -> Aff (err :: AWS.RequestError | eff) BatchGetBuildsOutput
batchGetBuilds = AWS.request serviceName "BatchGetBuilds" 


-- | <p>Gets information about build projects.</p>
batchGetProjects :: forall eff. BatchGetProjectsInput -> Aff (err :: AWS.RequestError | eff) BatchGetProjectsOutput
batchGetProjects = AWS.request serviceName "BatchGetProjects" 


-- | <p>Creates a build project.</p>
createProject :: forall eff. CreateProjectInput -> Aff (err :: AWS.RequestError | eff) CreateProjectOutput
createProject = AWS.request serviceName "CreateProject" 


-- | <p>For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, enables AWS CodeBuild to begin automatically rebuilding the source code every time a code change is pushed to the repository.</p> <important> <p>If you enable webhooks for an AWS CodeBuild project, and the project is used as a build step in AWS CodePipeline, then two identical builds will be created for each commit. One build is triggered through webhooks, and one through AWS CodePipeline. Because billing is on a per-build basis, you will be billed for both builds. Therefore, if you are using AWS CodePipeline, we recommend that you disable webhooks in CodeBuild. In the AWS CodeBuild console, clear the Webhook box. For more information, see step 9 in <a href="http://docs.aws.amazon.com/codebuild/latest/userguide/change-project.html#change-project-console">Change a Build Project's Settings</a>.</p> </important>
createWebhook :: forall eff. CreateWebhookInput -> Aff (err :: AWS.RequestError | eff) CreateWebhookOutput
createWebhook = AWS.request serviceName "CreateWebhook" 


-- | <p>Deletes a build project.</p>
deleteProject :: forall eff. DeleteProjectInput -> Aff (err :: AWS.RequestError | eff) DeleteProjectOutput
deleteProject = AWS.request serviceName "DeleteProject" 


-- | <p>For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, stops AWS CodeBuild from automatically rebuilding the source code every time a code change is pushed to the repository.</p>
deleteWebhook :: forall eff. DeleteWebhookInput -> Aff (err :: AWS.RequestError | eff) DeleteWebhookOutput
deleteWebhook = AWS.request serviceName "DeleteWebhook" 


-- | <p>Resets the cache for a project.</p>
invalidateProjectCache :: forall eff. InvalidateProjectCacheInput -> Aff (err :: AWS.RequestError | eff) InvalidateProjectCacheOutput
invalidateProjectCache = AWS.request serviceName "InvalidateProjectCache" 


-- | <p>Gets a list of build IDs, with each build ID representing a single build.</p>
listBuilds :: forall eff. ListBuildsInput -> Aff (err :: AWS.RequestError | eff) ListBuildsOutput
listBuilds = AWS.request serviceName "ListBuilds" 


-- | <p>Gets a list of build IDs for the specified build project, with each build ID representing a single build.</p>
listBuildsForProject :: forall eff. ListBuildsForProjectInput -> Aff (err :: AWS.RequestError | eff) ListBuildsForProjectOutput
listBuildsForProject = AWS.request serviceName "ListBuildsForProject" 


-- | <p>Gets information about Docker images that are managed by AWS CodeBuild.</p>
listCuratedEnvironmentImages :: forall eff. ListCuratedEnvironmentImagesInput -> Aff (err :: AWS.RequestError | eff) ListCuratedEnvironmentImagesOutput
listCuratedEnvironmentImages = AWS.request serviceName "ListCuratedEnvironmentImages" 


-- | <p>Gets a list of build project names, with each build project name representing a single build project.</p>
listProjects :: forall eff. ListProjectsInput -> Aff (err :: AWS.RequestError | eff) ListProjectsOutput
listProjects = AWS.request serviceName "ListProjects" 


-- | <p>Starts running a build.</p>
startBuild :: forall eff. StartBuildInput -> Aff (err :: AWS.RequestError | eff) StartBuildOutput
startBuild = AWS.request serviceName "StartBuild" 


-- | <p>Attempts to stop running a build.</p>
stopBuild :: forall eff. StopBuildInput -> Aff (err :: AWS.RequestError | eff) StopBuildOutput
stopBuild = AWS.request serviceName "StopBuild" 


-- | <p>Changes the settings of a build project.</p>
updateProject :: forall eff. UpdateProjectInput -> Aff (err :: AWS.RequestError | eff) UpdateProjectOutput
updateProject = AWS.request serviceName "UpdateProject" 


-- | <p>An AWS service limit was exceeded for the calling AWS account.</p>
newtype AccountLimitExceededException = AccountLimitExceededException 
  { 
  }


newtype ArtifactNamespace = ArtifactNamespace String


newtype ArtifactPackaging = ArtifactPackaging String


newtype ArtifactsType = ArtifactsType String


newtype BatchDeleteBuildsInput = BatchDeleteBuildsInput 
  { "Ids'" :: (BuildIds)
  }


newtype BatchDeleteBuildsOutput = BatchDeleteBuildsOutput 
  { "BuildsDeleted'" :: NullOrUndefined (BuildIds)
  , "BuildsNotDeleted'" :: NullOrUndefined (BuildsNotDeleted)
  }


newtype BatchGetBuildsInput = BatchGetBuildsInput 
  { "Ids'" :: (BuildIds)
  }


newtype BatchGetBuildsOutput = BatchGetBuildsOutput 
  { "Builds'" :: NullOrUndefined (Builds)
  , "BuildsNotFound'" :: NullOrUndefined (BuildIds)
  }


newtype BatchGetProjectsInput = BatchGetProjectsInput 
  { "Names'" :: (ProjectNames)
  }


newtype BatchGetProjectsOutput = BatchGetProjectsOutput 
  { "Projects'" :: NullOrUndefined (Projects)
  , "ProjectsNotFound'" :: NullOrUndefined (ProjectNames)
  }


-- | <p>Information about a build.</p>
newtype Build = Build 
  { "Id'" :: NullOrUndefined (NonEmptyString)
  , "Arn'" :: NullOrUndefined (NonEmptyString)
  , "StartTime'" :: NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined (Number)
  , "CurrentPhase'" :: NullOrUndefined (String)
  , "BuildStatus'" :: NullOrUndefined (StatusType)
  , "SourceVersion'" :: NullOrUndefined (NonEmptyString)
  , "ProjectName'" :: NullOrUndefined (NonEmptyString)
  , "Phases'" :: NullOrUndefined (BuildPhases)
  , "Source'" :: NullOrUndefined (ProjectSource)
  , "Artifacts'" :: NullOrUndefined (BuildArtifacts)
  , "Cache'" :: NullOrUndefined (ProjectCache)
  , "Environment'" :: NullOrUndefined (ProjectEnvironment)
  , "Logs'" :: NullOrUndefined (LogsLocation)
  , "TimeoutInMinutes'" :: NullOrUndefined (WrapperInt)
  , "BuildComplete'" :: NullOrUndefined (Boolean)
  , "Initiator'" :: NullOrUndefined (String)
  , "VpcConfig'" :: NullOrUndefined (VpcConfig)
  , "NetworkInterface'" :: NullOrUndefined (NetworkInterface)
  }


-- | <p>Information about build output artifacts.</p>
newtype BuildArtifacts = BuildArtifacts 
  { "Location'" :: NullOrUndefined (String)
  , "Sha256sum'" :: NullOrUndefined (String)
  , "Md5sum'" :: NullOrUndefined (String)
  }


newtype BuildIds = BuildIds (Array NonEmptyString)


-- | <p>Information about a build that could not be successfully deleted.</p>
newtype BuildNotDeleted = BuildNotDeleted 
  { "Id'" :: NullOrUndefined (NonEmptyString)
  , "StatusCode'" :: NullOrUndefined (String)
  }


-- | <p>Information about a stage for a build.</p>
newtype BuildPhase = BuildPhase 
  { "PhaseType'" :: NullOrUndefined (BuildPhaseType)
  , "PhaseStatus'" :: NullOrUndefined (StatusType)
  , "StartTime'" :: NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined (Number)
  , "DurationInSeconds'" :: NullOrUndefined (WrapperLong)
  , "Contexts'" :: NullOrUndefined (PhaseContexts)
  }


newtype BuildPhaseType = BuildPhaseType String


newtype BuildPhases = BuildPhases (Array BuildPhase)


newtype Builds = Builds (Array Build)


newtype BuildsNotDeleted = BuildsNotDeleted (Array BuildNotDeleted)


newtype CacheType = CacheType String


newtype ComputeType = ComputeType String


newtype CreateProjectInput = CreateProjectInput 
  { "Name'" :: (ProjectName)
  , "Description'" :: NullOrUndefined (ProjectDescription)
  , "Source'" :: (ProjectSource)
  , "Artifacts'" :: (ProjectArtifacts)
  , "Cache'" :: NullOrUndefined (ProjectCache)
  , "Environment'" :: (ProjectEnvironment)
  , "ServiceRole'" :: NullOrUndefined (NonEmptyString)
  , "TimeoutInMinutes'" :: NullOrUndefined (TimeOut)
  , "EncryptionKey'" :: NullOrUndefined (NonEmptyString)
  , "Tags'" :: NullOrUndefined (TagList)
  , "VpcConfig'" :: NullOrUndefined (VpcConfig)
  , "BadgeEnabled'" :: NullOrUndefined (WrapperBoolean)
  }


newtype CreateProjectOutput = CreateProjectOutput 
  { "Project'" :: NullOrUndefined (Project)
  }


newtype CreateWebhookInput = CreateWebhookInput 
  { "ProjectName'" :: (ProjectName)
  }


newtype CreateWebhookOutput = CreateWebhookOutput 
  { "Webhook'" :: NullOrUndefined (Webhook)
  }


newtype DeleteProjectInput = DeleteProjectInput 
  { "Name'" :: (NonEmptyString)
  }


newtype DeleteProjectOutput = DeleteProjectOutput 
  { 
  }


newtype DeleteWebhookInput = DeleteWebhookInput 
  { "ProjectName'" :: (ProjectName)
  }


newtype DeleteWebhookOutput = DeleteWebhookOutput 
  { 
  }


-- | <p>Information about a Docker image that is managed by AWS CodeBuild.</p>
newtype EnvironmentImage = EnvironmentImage 
  { "Name'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (String)
  , "Versions'" :: NullOrUndefined (ImageVersions)
  }


newtype EnvironmentImages = EnvironmentImages (Array EnvironmentImage)


-- | <p>A set of Docker images that are related by programming language and are managed by AWS CodeBuild.</p>
newtype EnvironmentLanguage = EnvironmentLanguage 
  { "Language'" :: NullOrUndefined (LanguageType)
  , "Images'" :: NullOrUndefined (EnvironmentImages)
  }


newtype EnvironmentLanguages = EnvironmentLanguages (Array EnvironmentLanguage)


-- | <p>A set of Docker images that are related by platform and are managed by AWS CodeBuild.</p>
newtype EnvironmentPlatform = EnvironmentPlatform 
  { "Platform'" :: NullOrUndefined (PlatformType)
  , "Languages'" :: NullOrUndefined (EnvironmentLanguages)
  }


newtype EnvironmentPlatforms = EnvironmentPlatforms (Array EnvironmentPlatform)


newtype EnvironmentType = EnvironmentType String


-- | <p>Information about an environment variable for a build project or a build.</p>
newtype EnvironmentVariable = EnvironmentVariable 
  { "Name'" :: (NonEmptyString)
  , "Value'" :: (String)
  , "Type'" :: NullOrUndefined (EnvironmentVariableType)
  }


newtype EnvironmentVariableType = EnvironmentVariableType String


newtype EnvironmentVariables = EnvironmentVariables (Array EnvironmentVariable)


newtype GitCloneDepth = GitCloneDepth Int


newtype ImageVersions = ImageVersions (Array String)


-- | <p>The input value that was provided is not valid.</p>
newtype InvalidInputException = InvalidInputException 
  { 
  }


newtype InvalidateProjectCacheInput = InvalidateProjectCacheInput 
  { "ProjectName'" :: (NonEmptyString)
  }


newtype InvalidateProjectCacheOutput = InvalidateProjectCacheOutput 
  { 
  }


newtype KeyInput = KeyInput String


newtype LanguageType = LanguageType String


newtype ListBuildsForProjectInput = ListBuildsForProjectInput 
  { "ProjectName'" :: (NonEmptyString)
  , "SortOrder'" :: NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype ListBuildsForProjectOutput = ListBuildsForProjectOutput 
  { "Ids'" :: NullOrUndefined (BuildIds)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype ListBuildsInput = ListBuildsInput 
  { "SortOrder'" :: NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype ListBuildsOutput = ListBuildsOutput 
  { "Ids'" :: NullOrUndefined (BuildIds)
  , "NextToken'" :: NullOrUndefined (String)
  }


newtype ListCuratedEnvironmentImagesInput = ListCuratedEnvironmentImagesInput 
  { 
  }


newtype ListCuratedEnvironmentImagesOutput = ListCuratedEnvironmentImagesOutput 
  { "Platforms'" :: NullOrUndefined (EnvironmentPlatforms)
  }


newtype ListProjectsInput = ListProjectsInput 
  { "SortBy'" :: NullOrUndefined (ProjectSortByType)
  , "SortOrder'" :: NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined (NonEmptyString)
  }


newtype ListProjectsOutput = ListProjectsOutput 
  { "NextToken'" :: NullOrUndefined (String)
  , "Projects'" :: NullOrUndefined (ProjectNames)
  }


-- | <p>Information about build logs in Amazon CloudWatch Logs.</p>
newtype LogsLocation = LogsLocation 
  { "GroupName'" :: NullOrUndefined (String)
  , "StreamName'" :: NullOrUndefined (String)
  , "DeepLink'" :: NullOrUndefined (String)
  }


-- | <p>Describes a network interface.</p>
newtype NetworkInterface = NetworkInterface 
  { "SubnetId'" :: NullOrUndefined (NonEmptyString)
  , "NetworkInterfaceId'" :: NullOrUndefined (NonEmptyString)
  }


newtype NonEmptyString = NonEmptyString String


-- | <p>There was a problem with the underlying OAuth provider.</p>
newtype OAuthProviderException = OAuthProviderException 
  { 
  }


-- | <p>Additional information about a build phase that has an error. You can use this information to help troubleshoot a failed build.</p>
newtype PhaseContext = PhaseContext 
  { "StatusCode'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }


newtype PhaseContexts = PhaseContexts (Array PhaseContext)


newtype PlatformType = PlatformType String


-- | <p>Information about a build project.</p>
newtype Project = Project 
  { "Name'" :: NullOrUndefined (ProjectName)
  , "Arn'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (ProjectDescription)
  , "Source'" :: NullOrUndefined (ProjectSource)
  , "Artifacts'" :: NullOrUndefined (ProjectArtifacts)
  , "Cache'" :: NullOrUndefined (ProjectCache)
  , "Environment'" :: NullOrUndefined (ProjectEnvironment)
  , "ServiceRole'" :: NullOrUndefined (NonEmptyString)
  , "TimeoutInMinutes'" :: NullOrUndefined (TimeOut)
  , "EncryptionKey'" :: NullOrUndefined (NonEmptyString)
  , "Tags'" :: NullOrUndefined (TagList)
  , "Created'" :: NullOrUndefined (Number)
  , "LastModified'" :: NullOrUndefined (Number)
  , "Webhook'" :: NullOrUndefined (Webhook)
  , "VpcConfig'" :: NullOrUndefined (VpcConfig)
  , "Badge'" :: NullOrUndefined (ProjectBadge)
  }


-- | <p>Information about the build output artifacts for the build project.</p>
newtype ProjectArtifacts = ProjectArtifacts 
  { "Type'" :: (ArtifactsType)
  , "Location'" :: NullOrUndefined (String)
  , "Path'" :: NullOrUndefined (String)
  , "NamespaceType'" :: NullOrUndefined (ArtifactNamespace)
  , "Name'" :: NullOrUndefined (String)
  , "Packaging'" :: NullOrUndefined (ArtifactPackaging)
  }


-- | <p>Information about the build badge for the build project.</p>
newtype ProjectBadge = ProjectBadge 
  { "BadgeEnabled'" :: NullOrUndefined (Boolean)
  , "BadgeRequestUrl'" :: NullOrUndefined (String)
  }


-- | <p>Information about the cache for the build project.</p>
newtype ProjectCache = ProjectCache 
  { "Type'" :: (CacheType)
  , "Location'" :: NullOrUndefined (String)
  }


newtype ProjectDescription = ProjectDescription String


-- | <p>Information about the build environment of the build project.</p>
newtype ProjectEnvironment = ProjectEnvironment 
  { "Type'" :: (EnvironmentType)
  , "Image'" :: (NonEmptyString)
  , "ComputeType'" :: (ComputeType)
  , "EnvironmentVariables'" :: NullOrUndefined (EnvironmentVariables)
  , "PrivilegedMode'" :: NullOrUndefined (WrapperBoolean)
  , "Certificate'" :: NullOrUndefined (String)
  }


newtype ProjectName = ProjectName String


newtype ProjectNames = ProjectNames (Array NonEmptyString)


newtype ProjectSortByType = ProjectSortByType String


-- | <p>Information about the build input source code for the build project.</p>
newtype ProjectSource = ProjectSource 
  { "Type'" :: (SourceType)
  , "Location'" :: NullOrUndefined (String)
  , "GitCloneDepth'" :: NullOrUndefined (GitCloneDepth)
  , "Buildspec'" :: NullOrUndefined (String)
  , "Auth'" :: NullOrUndefined (SourceAuth)
  , "InsecureSsl'" :: NullOrUndefined (WrapperBoolean)
  }


newtype Projects = Projects (Array Project)


-- | <p>The specified AWS resource cannot be created, because an AWS resource with the same settings already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { 
  }


-- | <p>The specified AWS resource cannot be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }


newtype SecurityGroupIds = SecurityGroupIds (Array NonEmptyString)


newtype SortOrderType = SortOrderType String


-- | <p>Information about the authorization settings for AWS CodeBuild to access the source code to be built.</p> <p>This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly (unless the build project's source <code>type</code> value is <code>BITBUCKET</code> or <code>GITHUB</code>).</p>
newtype SourceAuth = SourceAuth 
  { "Type'" :: (SourceAuthType)
  , "Resource'" :: NullOrUndefined (String)
  }


newtype SourceAuthType = SourceAuthType String


newtype SourceType = SourceType String


newtype StartBuildInput = StartBuildInput 
  { "ProjectName'" :: (NonEmptyString)
  , "SourceVersion'" :: NullOrUndefined (String)
  , "ArtifactsOverride'" :: NullOrUndefined (ProjectArtifacts)
  , "EnvironmentVariablesOverride'" :: NullOrUndefined (EnvironmentVariables)
  , "GitCloneDepthOverride'" :: NullOrUndefined (GitCloneDepth)
  , "BuildspecOverride'" :: NullOrUndefined (String)
  , "TimeoutInMinutesOverride'" :: NullOrUndefined (TimeOut)
  }


newtype StartBuildOutput = StartBuildOutput 
  { "Build'" :: NullOrUndefined (Build)
  }


newtype StatusType = StatusType String


newtype StopBuildInput = StopBuildInput 
  { "Id'" :: (NonEmptyString)
  }


newtype StopBuildOutput = StopBuildOutput 
  { "Build'" :: NullOrUndefined (Build)
  }


newtype Subnets = Subnets (Array NonEmptyString)


-- | <p>A tag, consisting of a key and a value.</p> <p>This tag is available for use by AWS services that support tags in AWS CodeBuild.</p>
newtype Tag = Tag 
  { "Key'" :: NullOrUndefined (KeyInput)
  , "Value'" :: NullOrUndefined (ValueInput)
  }


newtype TagList = TagList (Array Tag)


newtype TimeOut = TimeOut Int


newtype UpdateProjectInput = UpdateProjectInput 
  { "Name'" :: (NonEmptyString)
  , "Description'" :: NullOrUndefined (ProjectDescription)
  , "Source'" :: NullOrUndefined (ProjectSource)
  , "Artifacts'" :: NullOrUndefined (ProjectArtifacts)
  , "Cache'" :: NullOrUndefined (ProjectCache)
  , "Environment'" :: NullOrUndefined (ProjectEnvironment)
  , "ServiceRole'" :: NullOrUndefined (NonEmptyString)
  , "TimeoutInMinutes'" :: NullOrUndefined (TimeOut)
  , "EncryptionKey'" :: NullOrUndefined (NonEmptyString)
  , "Tags'" :: NullOrUndefined (TagList)
  , "VpcConfig'" :: NullOrUndefined (VpcConfig)
  , "BadgeEnabled'" :: NullOrUndefined (WrapperBoolean)
  }


newtype UpdateProjectOutput = UpdateProjectOutput 
  { "Project'" :: NullOrUndefined (Project)
  }


newtype ValueInput = ValueInput String


-- | <p>Information about the VPC configuration that AWS CodeBuild will access.</p>
newtype VpcConfig = VpcConfig 
  { "VpcId'" :: NullOrUndefined (NonEmptyString)
  , "Subnets'" :: NullOrUndefined (Subnets)
  , "SecurityGroupIds'" :: NullOrUndefined (SecurityGroupIds)
  }


-- | <p>Information about a webhook in GitHub that connects repository events to a build project in AWS CodeBuild.</p>
newtype Webhook = Webhook 
  { "Url'" :: NullOrUndefined (NonEmptyString)
  , "PayloadUrl'" :: NullOrUndefined (NonEmptyString)
  , "Secret'" :: NullOrUndefined (NonEmptyString)
  }


newtype WrapperBoolean = WrapperBoolean Boolean


newtype WrapperInt = WrapperInt Int


newtype WrapperLong = WrapperLong Number
