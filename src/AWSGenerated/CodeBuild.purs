

-- | <fullname>AWS CodeBuild</fullname> <p>AWS CodeBuild is a fully managed build service in the cloud. AWS CodeBuild compiles your source code, runs unit tests, and produces artifacts that are ready to deploy. AWS CodeBuild eliminates the need to provision, manage, and scale your own build servers. It provides prepackaged build environments for the most popular programming languages and build tools, such as Apache Maven, Gradle, and more. You can also fully customize build environments in AWS CodeBuild to use your own build tools. AWS CodeBuild scales automatically to meet peak build requests, and you pay only for the build time you consume. For more information about AWS CodeBuild, see the <i>AWS CodeBuild User Guide</i>.</p> <p>AWS CodeBuild supports these operations:</p> <ul> <li> <p> <code>BatchDeleteBuilds</code>: Deletes one or more builds.</p> </li> <li> <p> <code>BatchGetProjects</code>: Gets information about one or more build projects. A <i>build project</i> defines how AWS CodeBuild will run a build. This includes information such as where to get the source code to build, the build environment to use, the build commands to run, and where to store the build output. A <i>build environment</i> represents a combination of operating system, programming language runtime, and tools that AWS CodeBuild will use to run a build. Also, you can add tags to build projects to help manage your resources and costs.</p> </li> <li> <p> <code>CreateProject</code>: Creates a build project.</p> </li> <li> <p> <code>CreateWebhook</code>: For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, enables AWS CodeBuild to begin automatically rebuilding the source code every time a code change is pushed to the repository.</p> </li> <li> <p> <code>DeleteProject</code>: Deletes a build project.</p> </li> <li> <p> <code>DeleteWebhook</code>: For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, stops AWS CodeBuild from automatically rebuilding the source code every time a code change is pushed to the repository.</p> </li> <li> <p> <code>ListProjects</code>: Gets a list of build project names, with each build project name representing a single build project.</p> </li> <li> <p> <code>UpdateProject</code>: Changes the settings of an existing build project.</p> </li> <li> <p> <code>BatchGetBuilds</code>: Gets information about one or more builds.</p> </li> <li> <p> <code>ListBuilds</code>: Gets a list of build IDs, with each build ID representing a single build.</p> </li> <li> <p> <code>ListBuildsForProject</code>: Gets a list of build IDs for the specified build project, with each build ID representing a single build.</p> </li> <li> <p> <code>StartBuild</code>: Starts running a build.</p> </li> <li> <p> <code>StopBuild</code>: Attempts to stop running a build.</p> </li> <li> <p> <code>ListCuratedEnvironmentImages</code>: Gets information about Docker images that are managed by AWS CodeBuild.</p> </li> </ul>
module AWS.CodeBuild where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeAccountLimitExceededException :: Newtype AccountLimitExceededException _


newtype ArtifactNamespace = ArtifactNamespace String
derive instance newtypeArtifactNamespace :: Newtype ArtifactNamespace _


newtype ArtifactPackaging = ArtifactPackaging String
derive instance newtypeArtifactPackaging :: Newtype ArtifactPackaging _


newtype ArtifactsType = ArtifactsType String
derive instance newtypeArtifactsType :: Newtype ArtifactsType _


newtype BatchDeleteBuildsInput = BatchDeleteBuildsInput 
  { "Ids'" :: (BuildIds)
  }
derive instance newtypeBatchDeleteBuildsInput :: Newtype BatchDeleteBuildsInput _


newtype BatchDeleteBuildsOutput = BatchDeleteBuildsOutput 
  { "BuildsDeleted'" :: NullOrUndefined (BuildIds)
  , "BuildsNotDeleted'" :: NullOrUndefined (BuildsNotDeleted)
  }
derive instance newtypeBatchDeleteBuildsOutput :: Newtype BatchDeleteBuildsOutput _


newtype BatchGetBuildsInput = BatchGetBuildsInput 
  { "Ids'" :: (BuildIds)
  }
derive instance newtypeBatchGetBuildsInput :: Newtype BatchGetBuildsInput _


newtype BatchGetBuildsOutput = BatchGetBuildsOutput 
  { "Builds'" :: NullOrUndefined (Builds)
  , "BuildsNotFound'" :: NullOrUndefined (BuildIds)
  }
derive instance newtypeBatchGetBuildsOutput :: Newtype BatchGetBuildsOutput _


newtype BatchGetProjectsInput = BatchGetProjectsInput 
  { "Names'" :: (ProjectNames)
  }
derive instance newtypeBatchGetProjectsInput :: Newtype BatchGetProjectsInput _


newtype BatchGetProjectsOutput = BatchGetProjectsOutput 
  { "Projects'" :: NullOrUndefined (Projects)
  , "ProjectsNotFound'" :: NullOrUndefined (ProjectNames)
  }
derive instance newtypeBatchGetProjectsOutput :: Newtype BatchGetProjectsOutput _


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
derive instance newtypeBuild :: Newtype Build _


-- | <p>Information about build output artifacts.</p>
newtype BuildArtifacts = BuildArtifacts 
  { "Location'" :: NullOrUndefined (String)
  , "Sha256sum'" :: NullOrUndefined (String)
  , "Md5sum'" :: NullOrUndefined (String)
  }
derive instance newtypeBuildArtifacts :: Newtype BuildArtifacts _


newtype BuildIds = BuildIds (Array NonEmptyString)
derive instance newtypeBuildIds :: Newtype BuildIds _


-- | <p>Information about a build that could not be successfully deleted.</p>
newtype BuildNotDeleted = BuildNotDeleted 
  { "Id'" :: NullOrUndefined (NonEmptyString)
  , "StatusCode'" :: NullOrUndefined (String)
  }
derive instance newtypeBuildNotDeleted :: Newtype BuildNotDeleted _


-- | <p>Information about a stage for a build.</p>
newtype BuildPhase = BuildPhase 
  { "PhaseType'" :: NullOrUndefined (BuildPhaseType)
  , "PhaseStatus'" :: NullOrUndefined (StatusType)
  , "StartTime'" :: NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined (Number)
  , "DurationInSeconds'" :: NullOrUndefined (WrapperLong)
  , "Contexts'" :: NullOrUndefined (PhaseContexts)
  }
derive instance newtypeBuildPhase :: Newtype BuildPhase _


newtype BuildPhaseType = BuildPhaseType String
derive instance newtypeBuildPhaseType :: Newtype BuildPhaseType _


newtype BuildPhases = BuildPhases (Array BuildPhase)
derive instance newtypeBuildPhases :: Newtype BuildPhases _


newtype Builds = Builds (Array Build)
derive instance newtypeBuilds :: Newtype Builds _


newtype BuildsNotDeleted = BuildsNotDeleted (Array BuildNotDeleted)
derive instance newtypeBuildsNotDeleted :: Newtype BuildsNotDeleted _


newtype CacheType = CacheType String
derive instance newtypeCacheType :: Newtype CacheType _


newtype ComputeType = ComputeType String
derive instance newtypeComputeType :: Newtype ComputeType _


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
derive instance newtypeCreateProjectInput :: Newtype CreateProjectInput _


newtype CreateProjectOutput = CreateProjectOutput 
  { "Project'" :: NullOrUndefined (Project)
  }
derive instance newtypeCreateProjectOutput :: Newtype CreateProjectOutput _


newtype CreateWebhookInput = CreateWebhookInput 
  { "ProjectName'" :: (ProjectName)
  }
derive instance newtypeCreateWebhookInput :: Newtype CreateWebhookInput _


newtype CreateWebhookOutput = CreateWebhookOutput 
  { "Webhook'" :: NullOrUndefined (Webhook)
  }
derive instance newtypeCreateWebhookOutput :: Newtype CreateWebhookOutput _


newtype DeleteProjectInput = DeleteProjectInput 
  { "Name'" :: (NonEmptyString)
  }
derive instance newtypeDeleteProjectInput :: Newtype DeleteProjectInput _


newtype DeleteProjectOutput = DeleteProjectOutput 
  { 
  }
derive instance newtypeDeleteProjectOutput :: Newtype DeleteProjectOutput _


newtype DeleteWebhookInput = DeleteWebhookInput 
  { "ProjectName'" :: (ProjectName)
  }
derive instance newtypeDeleteWebhookInput :: Newtype DeleteWebhookInput _


newtype DeleteWebhookOutput = DeleteWebhookOutput 
  { 
  }
derive instance newtypeDeleteWebhookOutput :: Newtype DeleteWebhookOutput _


-- | <p>Information about a Docker image that is managed by AWS CodeBuild.</p>
newtype EnvironmentImage = EnvironmentImage 
  { "Name'" :: NullOrUndefined (String)
  , "Description'" :: NullOrUndefined (String)
  , "Versions'" :: NullOrUndefined (ImageVersions)
  }
derive instance newtypeEnvironmentImage :: Newtype EnvironmentImage _


newtype EnvironmentImages = EnvironmentImages (Array EnvironmentImage)
derive instance newtypeEnvironmentImages :: Newtype EnvironmentImages _


-- | <p>A set of Docker images that are related by programming language and are managed by AWS CodeBuild.</p>
newtype EnvironmentLanguage = EnvironmentLanguage 
  { "Language'" :: NullOrUndefined (LanguageType)
  , "Images'" :: NullOrUndefined (EnvironmentImages)
  }
derive instance newtypeEnvironmentLanguage :: Newtype EnvironmentLanguage _


newtype EnvironmentLanguages = EnvironmentLanguages (Array EnvironmentLanguage)
derive instance newtypeEnvironmentLanguages :: Newtype EnvironmentLanguages _


-- | <p>A set of Docker images that are related by platform and are managed by AWS CodeBuild.</p>
newtype EnvironmentPlatform = EnvironmentPlatform 
  { "Platform'" :: NullOrUndefined (PlatformType)
  , "Languages'" :: NullOrUndefined (EnvironmentLanguages)
  }
derive instance newtypeEnvironmentPlatform :: Newtype EnvironmentPlatform _


newtype EnvironmentPlatforms = EnvironmentPlatforms (Array EnvironmentPlatform)
derive instance newtypeEnvironmentPlatforms :: Newtype EnvironmentPlatforms _


newtype EnvironmentType = EnvironmentType String
derive instance newtypeEnvironmentType :: Newtype EnvironmentType _


-- | <p>Information about an environment variable for a build project or a build.</p>
newtype EnvironmentVariable = EnvironmentVariable 
  { "Name'" :: (NonEmptyString)
  , "Value'" :: (String)
  , "Type'" :: NullOrUndefined (EnvironmentVariableType)
  }
derive instance newtypeEnvironmentVariable :: Newtype EnvironmentVariable _


newtype EnvironmentVariableType = EnvironmentVariableType String
derive instance newtypeEnvironmentVariableType :: Newtype EnvironmentVariableType _


newtype EnvironmentVariables = EnvironmentVariables (Array EnvironmentVariable)
derive instance newtypeEnvironmentVariables :: Newtype EnvironmentVariables _


newtype GitCloneDepth = GitCloneDepth Int
derive instance newtypeGitCloneDepth :: Newtype GitCloneDepth _


newtype ImageVersions = ImageVersions (Array String)
derive instance newtypeImageVersions :: Newtype ImageVersions _


-- | <p>The input value that was provided is not valid.</p>
newtype InvalidInputException = InvalidInputException 
  { 
  }
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _


newtype InvalidateProjectCacheInput = InvalidateProjectCacheInput 
  { "ProjectName'" :: (NonEmptyString)
  }
derive instance newtypeInvalidateProjectCacheInput :: Newtype InvalidateProjectCacheInput _


newtype InvalidateProjectCacheOutput = InvalidateProjectCacheOutput 
  { 
  }
derive instance newtypeInvalidateProjectCacheOutput :: Newtype InvalidateProjectCacheOutput _


newtype KeyInput = KeyInput String
derive instance newtypeKeyInput :: Newtype KeyInput _


newtype LanguageType = LanguageType String
derive instance newtypeLanguageType :: Newtype LanguageType _


newtype ListBuildsForProjectInput = ListBuildsForProjectInput 
  { "ProjectName'" :: (NonEmptyString)
  , "SortOrder'" :: NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListBuildsForProjectInput :: Newtype ListBuildsForProjectInput _


newtype ListBuildsForProjectOutput = ListBuildsForProjectOutput 
  { "Ids'" :: NullOrUndefined (BuildIds)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListBuildsForProjectOutput :: Newtype ListBuildsForProjectOutput _


newtype ListBuildsInput = ListBuildsInput 
  { "SortOrder'" :: NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListBuildsInput :: Newtype ListBuildsInput _


newtype ListBuildsOutput = ListBuildsOutput 
  { "Ids'" :: NullOrUndefined (BuildIds)
  , "NextToken'" :: NullOrUndefined (String)
  }
derive instance newtypeListBuildsOutput :: Newtype ListBuildsOutput _


newtype ListCuratedEnvironmentImagesInput = ListCuratedEnvironmentImagesInput 
  { 
  }
derive instance newtypeListCuratedEnvironmentImagesInput :: Newtype ListCuratedEnvironmentImagesInput _


newtype ListCuratedEnvironmentImagesOutput = ListCuratedEnvironmentImagesOutput 
  { "Platforms'" :: NullOrUndefined (EnvironmentPlatforms)
  }
derive instance newtypeListCuratedEnvironmentImagesOutput :: Newtype ListCuratedEnvironmentImagesOutput _


newtype ListProjectsInput = ListProjectsInput 
  { "SortBy'" :: NullOrUndefined (ProjectSortByType)
  , "SortOrder'" :: NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeListProjectsInput :: Newtype ListProjectsInput _


newtype ListProjectsOutput = ListProjectsOutput 
  { "NextToken'" :: NullOrUndefined (String)
  , "Projects'" :: NullOrUndefined (ProjectNames)
  }
derive instance newtypeListProjectsOutput :: Newtype ListProjectsOutput _


-- | <p>Information about build logs in Amazon CloudWatch Logs.</p>
newtype LogsLocation = LogsLocation 
  { "GroupName'" :: NullOrUndefined (String)
  , "StreamName'" :: NullOrUndefined (String)
  , "DeepLink'" :: NullOrUndefined (String)
  }
derive instance newtypeLogsLocation :: Newtype LogsLocation _


-- | <p>Describes a network interface.</p>
newtype NetworkInterface = NetworkInterface 
  { "SubnetId'" :: NullOrUndefined (NonEmptyString)
  , "NetworkInterfaceId'" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeNetworkInterface :: Newtype NetworkInterface _


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _


-- | <p>There was a problem with the underlying OAuth provider.</p>
newtype OAuthProviderException = OAuthProviderException 
  { 
  }
derive instance newtypeOAuthProviderException :: Newtype OAuthProviderException _


-- | <p>Additional information about a build phase that has an error. You can use this information to help troubleshoot a failed build.</p>
newtype PhaseContext = PhaseContext 
  { "StatusCode'" :: NullOrUndefined (String)
  , "Message'" :: NullOrUndefined (String)
  }
derive instance newtypePhaseContext :: Newtype PhaseContext _


newtype PhaseContexts = PhaseContexts (Array PhaseContext)
derive instance newtypePhaseContexts :: Newtype PhaseContexts _


newtype PlatformType = PlatformType String
derive instance newtypePlatformType :: Newtype PlatformType _


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
derive instance newtypeProject :: Newtype Project _


-- | <p>Information about the build output artifacts for the build project.</p>
newtype ProjectArtifacts = ProjectArtifacts 
  { "Type'" :: (ArtifactsType)
  , "Location'" :: NullOrUndefined (String)
  , "Path'" :: NullOrUndefined (String)
  , "NamespaceType'" :: NullOrUndefined (ArtifactNamespace)
  , "Name'" :: NullOrUndefined (String)
  , "Packaging'" :: NullOrUndefined (ArtifactPackaging)
  }
derive instance newtypeProjectArtifacts :: Newtype ProjectArtifacts _


-- | <p>Information about the build badge for the build project.</p>
newtype ProjectBadge = ProjectBadge 
  { "BadgeEnabled'" :: NullOrUndefined (Boolean)
  , "BadgeRequestUrl'" :: NullOrUndefined (String)
  }
derive instance newtypeProjectBadge :: Newtype ProjectBadge _


-- | <p>Information about the cache for the build project.</p>
newtype ProjectCache = ProjectCache 
  { "Type'" :: (CacheType)
  , "Location'" :: NullOrUndefined (String)
  }
derive instance newtypeProjectCache :: Newtype ProjectCache _


newtype ProjectDescription = ProjectDescription String
derive instance newtypeProjectDescription :: Newtype ProjectDescription _


-- | <p>Information about the build environment of the build project.</p>
newtype ProjectEnvironment = ProjectEnvironment 
  { "Type'" :: (EnvironmentType)
  , "Image'" :: (NonEmptyString)
  , "ComputeType'" :: (ComputeType)
  , "EnvironmentVariables'" :: NullOrUndefined (EnvironmentVariables)
  , "PrivilegedMode'" :: NullOrUndefined (WrapperBoolean)
  , "Certificate'" :: NullOrUndefined (String)
  }
derive instance newtypeProjectEnvironment :: Newtype ProjectEnvironment _


newtype ProjectName = ProjectName String
derive instance newtypeProjectName :: Newtype ProjectName _


newtype ProjectNames = ProjectNames (Array NonEmptyString)
derive instance newtypeProjectNames :: Newtype ProjectNames _


newtype ProjectSortByType = ProjectSortByType String
derive instance newtypeProjectSortByType :: Newtype ProjectSortByType _


-- | <p>Information about the build input source code for the build project.</p>
newtype ProjectSource = ProjectSource 
  { "Type'" :: (SourceType)
  , "Location'" :: NullOrUndefined (String)
  , "GitCloneDepth'" :: NullOrUndefined (GitCloneDepth)
  , "Buildspec'" :: NullOrUndefined (String)
  , "Auth'" :: NullOrUndefined (SourceAuth)
  , "InsecureSsl'" :: NullOrUndefined (WrapperBoolean)
  }
derive instance newtypeProjectSource :: Newtype ProjectSource _


newtype Projects = Projects (Array Project)
derive instance newtypeProjects :: Newtype Projects _


-- | <p>The specified AWS resource cannot be created, because an AWS resource with the same settings already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { 
  }
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _


-- | <p>The specified AWS resource cannot be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { 
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


newtype SecurityGroupIds = SecurityGroupIds (Array NonEmptyString)
derive instance newtypeSecurityGroupIds :: Newtype SecurityGroupIds _


newtype SortOrderType = SortOrderType String
derive instance newtypeSortOrderType :: Newtype SortOrderType _


-- | <p>Information about the authorization settings for AWS CodeBuild to access the source code to be built.</p> <p>This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly (unless the build project's source <code>type</code> value is <code>BITBUCKET</code> or <code>GITHUB</code>).</p>
newtype SourceAuth = SourceAuth 
  { "Type'" :: (SourceAuthType)
  , "Resource'" :: NullOrUndefined (String)
  }
derive instance newtypeSourceAuth :: Newtype SourceAuth _


newtype SourceAuthType = SourceAuthType String
derive instance newtypeSourceAuthType :: Newtype SourceAuthType _


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _


newtype StartBuildInput = StartBuildInput 
  { "ProjectName'" :: (NonEmptyString)
  , "SourceVersion'" :: NullOrUndefined (String)
  , "ArtifactsOverride'" :: NullOrUndefined (ProjectArtifacts)
  , "EnvironmentVariablesOverride'" :: NullOrUndefined (EnvironmentVariables)
  , "GitCloneDepthOverride'" :: NullOrUndefined (GitCloneDepth)
  , "BuildspecOverride'" :: NullOrUndefined (String)
  , "TimeoutInMinutesOverride'" :: NullOrUndefined (TimeOut)
  }
derive instance newtypeStartBuildInput :: Newtype StartBuildInput _


newtype StartBuildOutput = StartBuildOutput 
  { "Build'" :: NullOrUndefined (Build)
  }
derive instance newtypeStartBuildOutput :: Newtype StartBuildOutput _


newtype StatusType = StatusType String
derive instance newtypeStatusType :: Newtype StatusType _


newtype StopBuildInput = StopBuildInput 
  { "Id'" :: (NonEmptyString)
  }
derive instance newtypeStopBuildInput :: Newtype StopBuildInput _


newtype StopBuildOutput = StopBuildOutput 
  { "Build'" :: NullOrUndefined (Build)
  }
derive instance newtypeStopBuildOutput :: Newtype StopBuildOutput _


newtype Subnets = Subnets (Array NonEmptyString)
derive instance newtypeSubnets :: Newtype Subnets _


-- | <p>A tag, consisting of a key and a value.</p> <p>This tag is available for use by AWS services that support tags in AWS CodeBuild.</p>
newtype Tag = Tag 
  { "Key'" :: NullOrUndefined (KeyInput)
  , "Value'" :: NullOrUndefined (ValueInput)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TimeOut = TimeOut Int
derive instance newtypeTimeOut :: Newtype TimeOut _


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
derive instance newtypeUpdateProjectInput :: Newtype UpdateProjectInput _


newtype UpdateProjectOutput = UpdateProjectOutput 
  { "Project'" :: NullOrUndefined (Project)
  }
derive instance newtypeUpdateProjectOutput :: Newtype UpdateProjectOutput _


newtype ValueInput = ValueInput String
derive instance newtypeValueInput :: Newtype ValueInput _


-- | <p>Information about the VPC configuration that AWS CodeBuild will access.</p>
newtype VpcConfig = VpcConfig 
  { "VpcId'" :: NullOrUndefined (NonEmptyString)
  , "Subnets'" :: NullOrUndefined (Subnets)
  , "SecurityGroupIds'" :: NullOrUndefined (SecurityGroupIds)
  }
derive instance newtypeVpcConfig :: Newtype VpcConfig _


-- | <p>Information about a webhook in GitHub that connects repository events to a build project in AWS CodeBuild.</p>
newtype Webhook = Webhook 
  { "Url'" :: NullOrUndefined (NonEmptyString)
  , "PayloadUrl'" :: NullOrUndefined (NonEmptyString)
  , "Secret'" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeWebhook :: Newtype Webhook _


newtype WrapperBoolean = WrapperBoolean Boolean
derive instance newtypeWrapperBoolean :: Newtype WrapperBoolean _


newtype WrapperInt = WrapperInt Int
derive instance newtypeWrapperInt :: Newtype WrapperInt _


newtype WrapperLong = WrapperLong Number
derive instance newtypeWrapperLong :: Newtype WrapperLong _
