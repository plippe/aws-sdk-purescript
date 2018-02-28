

-- | <fullname>AWS CodeBuild</fullname> <p>AWS CodeBuild is a fully managed build service in the cloud. AWS CodeBuild compiles your source code, runs unit tests, and produces artifacts that are ready to deploy. AWS CodeBuild eliminates the need to provision, manage, and scale your own build servers. It provides prepackaged build environments for the most popular programming languages and build tools, such as Apache Maven, Gradle, and more. You can also fully customize build environments in AWS CodeBuild to use your own build tools. AWS CodeBuild scales automatically to meet peak build requests, and you pay only for the build time you consume. For more information about AWS CodeBuild, see the <i>AWS CodeBuild User Guide</i>.</p> <p>AWS CodeBuild supports these operations:</p> <ul> <li> <p> <code>BatchDeleteBuilds</code>: Deletes one or more builds.</p> </li> <li> <p> <code>BatchGetProjects</code>: Gets information about one or more build projects. A <i>build project</i> defines how AWS CodeBuild will run a build. This includes information such as where to get the source code to build, the build environment to use, the build commands to run, and where to store the build output. A <i>build environment</i> represents a combination of operating system, programming language runtime, and tools that AWS CodeBuild will use to run a build. Also, you can add tags to build projects to help manage your resources and costs.</p> </li> <li> <p> <code>CreateProject</code>: Creates a build project.</p> </li> <li> <p> <code>CreateWebhook</code>: For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, enables AWS CodeBuild to begin automatically rebuilding the source code every time a code change is pushed to the repository.</p> </li> <li> <p> <code>DeleteProject</code>: Deletes a build project.</p> </li> <li> <p> <code>DeleteWebhook</code>: For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, stops AWS CodeBuild from automatically rebuilding the source code every time a code change is pushed to the repository.</p> </li> <li> <p> <code>ListProjects</code>: Gets a list of build project names, with each build project name representing a single build project.</p> </li> <li> <p> <code>UpdateProject</code>: Changes the settings of an existing build project.</p> </li> <li> <p> <code>BatchGetBuilds</code>: Gets information about one or more builds.</p> </li> <li> <p> <code>ListBuilds</code>: Gets a list of build IDs, with each build ID representing a single build.</p> </li> <li> <p> <code>ListBuildsForProject</code>: Gets a list of build IDs for the specified build project, with each build ID representing a single build.</p> </li> <li> <p> <code>StartBuild</code>: Starts running a build.</p> </li> <li> <p> <code>StopBuild</code>: Attempts to stop running a build.</p> </li> <li> <p> <code>ListCuratedEnvironmentImages</code>: Gets information about Docker images that are managed by AWS CodeBuild.</p> </li> </ul>
module AWS.CodeBuild where

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

serviceName = "CodeBuild" :: String


-- | <p>Deletes one or more builds.</p>
batchDeleteBuilds :: forall eff. BatchDeleteBuildsInput -> Aff (exception :: EXCEPTION | eff) BatchDeleteBuildsOutput
batchDeleteBuilds = Request.request serviceName "batchDeleteBuilds" 


-- | <p>Gets information about builds.</p>
batchGetBuilds :: forall eff. BatchGetBuildsInput -> Aff (exception :: EXCEPTION | eff) BatchGetBuildsOutput
batchGetBuilds = Request.request serviceName "batchGetBuilds" 


-- | <p>Gets information about build projects.</p>
batchGetProjects :: forall eff. BatchGetProjectsInput -> Aff (exception :: EXCEPTION | eff) BatchGetProjectsOutput
batchGetProjects = Request.request serviceName "batchGetProjects" 


-- | <p>Creates a build project.</p>
createProject :: forall eff. CreateProjectInput -> Aff (exception :: EXCEPTION | eff) CreateProjectOutput
createProject = Request.request serviceName "createProject" 


-- | <p>For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, enables AWS CodeBuild to begin automatically rebuilding the source code every time a code change is pushed to the repository.</p> <important> <p>If you enable webhooks for an AWS CodeBuild project, and the project is used as a build step in AWS CodePipeline, then two identical builds will be created for each commit. One build is triggered through webhooks, and one through AWS CodePipeline. Because billing is on a per-build basis, you will be billed for both builds. Therefore, if you are using AWS CodePipeline, we recommend that you disable webhooks in CodeBuild. In the AWS CodeBuild console, clear the Webhook box. For more information, see step 9 in <a href="http://docs.aws.amazon.com/codebuild/latest/userguide/change-project.html#change-project-console">Change a Build Project's Settings</a>.</p> </important>
createWebhook :: forall eff. CreateWebhookInput -> Aff (exception :: EXCEPTION | eff) CreateWebhookOutput
createWebhook = Request.request serviceName "createWebhook" 


-- | <p>Deletes a build project.</p>
deleteProject :: forall eff. DeleteProjectInput -> Aff (exception :: EXCEPTION | eff) DeleteProjectOutput
deleteProject = Request.request serviceName "deleteProject" 


-- | <p>For an existing AWS CodeBuild build project that has its source code stored in a GitHub repository, stops AWS CodeBuild from automatically rebuilding the source code every time a code change is pushed to the repository.</p>
deleteWebhook :: forall eff. DeleteWebhookInput -> Aff (exception :: EXCEPTION | eff) DeleteWebhookOutput
deleteWebhook = Request.request serviceName "deleteWebhook" 


-- | <p>Resets the cache for a project.</p>
invalidateProjectCache :: forall eff. InvalidateProjectCacheInput -> Aff (exception :: EXCEPTION | eff) InvalidateProjectCacheOutput
invalidateProjectCache = Request.request serviceName "invalidateProjectCache" 


-- | <p>Gets a list of build IDs, with each build ID representing a single build.</p>
listBuilds :: forall eff. ListBuildsInput -> Aff (exception :: EXCEPTION | eff) ListBuildsOutput
listBuilds = Request.request serviceName "listBuilds" 


-- | <p>Gets a list of build IDs for the specified build project, with each build ID representing a single build.</p>
listBuildsForProject :: forall eff. ListBuildsForProjectInput -> Aff (exception :: EXCEPTION | eff) ListBuildsForProjectOutput
listBuildsForProject = Request.request serviceName "listBuildsForProject" 


-- | <p>Gets information about Docker images that are managed by AWS CodeBuild.</p>
listCuratedEnvironmentImages :: forall eff. ListCuratedEnvironmentImagesInput -> Aff (exception :: EXCEPTION | eff) ListCuratedEnvironmentImagesOutput
listCuratedEnvironmentImages = Request.request serviceName "listCuratedEnvironmentImages" 


-- | <p>Gets a list of build project names, with each build project name representing a single build project.</p>
listProjects :: forall eff. ListProjectsInput -> Aff (exception :: EXCEPTION | eff) ListProjectsOutput
listProjects = Request.request serviceName "listProjects" 


-- | <p>Starts running a build.</p>
startBuild :: forall eff. StartBuildInput -> Aff (exception :: EXCEPTION | eff) StartBuildOutput
startBuild = Request.request serviceName "startBuild" 


-- | <p>Attempts to stop running a build.</p>
stopBuild :: forall eff. StopBuildInput -> Aff (exception :: EXCEPTION | eff) StopBuildOutput
stopBuild = Request.request serviceName "stopBuild" 


-- | <p>Changes the settings of a build project.</p>
updateProject :: forall eff. UpdateProjectInput -> Aff (exception :: EXCEPTION | eff) UpdateProjectOutput
updateProject = Request.request serviceName "updateProject" 


-- | <p>An AWS service limit was exceeded for the calling AWS account.</p>
newtype AccountLimitExceededException = AccountLimitExceededException Types.NoArguments
derive instance newtypeAccountLimitExceededException :: Newtype AccountLimitExceededException _
derive instance repGenericAccountLimitExceededException :: Generic AccountLimitExceededException _
instance showAccountLimitExceededException :: Show AccountLimitExceededException where
  show = genericShow
instance decodeAccountLimitExceededException :: Decode AccountLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountLimitExceededException :: Encode AccountLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactNamespace = ArtifactNamespace String
derive instance newtypeArtifactNamespace :: Newtype ArtifactNamespace _
derive instance repGenericArtifactNamespace :: Generic ArtifactNamespace _
instance showArtifactNamespace :: Show ArtifactNamespace where
  show = genericShow
instance decodeArtifactNamespace :: Decode ArtifactNamespace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactNamespace :: Encode ArtifactNamespace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactPackaging = ArtifactPackaging String
derive instance newtypeArtifactPackaging :: Newtype ArtifactPackaging _
derive instance repGenericArtifactPackaging :: Generic ArtifactPackaging _
instance showArtifactPackaging :: Show ArtifactPackaging where
  show = genericShow
instance decodeArtifactPackaging :: Decode ArtifactPackaging where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactPackaging :: Encode ArtifactPackaging where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ArtifactsType = ArtifactsType String
derive instance newtypeArtifactsType :: Newtype ArtifactsType _
derive instance repGenericArtifactsType :: Generic ArtifactsType _
instance showArtifactsType :: Show ArtifactsType where
  show = genericShow
instance decodeArtifactsType :: Decode ArtifactsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeArtifactsType :: Encode ArtifactsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteBuildsInput = BatchDeleteBuildsInput 
  { "Ids'" :: (BuildIds)
  }
derive instance newtypeBatchDeleteBuildsInput :: Newtype BatchDeleteBuildsInput _
derive instance repGenericBatchDeleteBuildsInput :: Generic BatchDeleteBuildsInput _
instance showBatchDeleteBuildsInput :: Show BatchDeleteBuildsInput where
  show = genericShow
instance decodeBatchDeleteBuildsInput :: Decode BatchDeleteBuildsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteBuildsInput :: Encode BatchDeleteBuildsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchDeleteBuildsOutput = BatchDeleteBuildsOutput 
  { "BuildsDeleted'" :: NullOrUndefined.NullOrUndefined (BuildIds)
  , "BuildsNotDeleted'" :: NullOrUndefined.NullOrUndefined (BuildsNotDeleted)
  }
derive instance newtypeBatchDeleteBuildsOutput :: Newtype BatchDeleteBuildsOutput _
derive instance repGenericBatchDeleteBuildsOutput :: Generic BatchDeleteBuildsOutput _
instance showBatchDeleteBuildsOutput :: Show BatchDeleteBuildsOutput where
  show = genericShow
instance decodeBatchDeleteBuildsOutput :: Decode BatchDeleteBuildsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchDeleteBuildsOutput :: Encode BatchDeleteBuildsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetBuildsInput = BatchGetBuildsInput 
  { "Ids'" :: (BuildIds)
  }
derive instance newtypeBatchGetBuildsInput :: Newtype BatchGetBuildsInput _
derive instance repGenericBatchGetBuildsInput :: Generic BatchGetBuildsInput _
instance showBatchGetBuildsInput :: Show BatchGetBuildsInput where
  show = genericShow
instance decodeBatchGetBuildsInput :: Decode BatchGetBuildsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetBuildsInput :: Encode BatchGetBuildsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetBuildsOutput = BatchGetBuildsOutput 
  { "Builds'" :: NullOrUndefined.NullOrUndefined (Builds)
  , "BuildsNotFound'" :: NullOrUndefined.NullOrUndefined (BuildIds)
  }
derive instance newtypeBatchGetBuildsOutput :: Newtype BatchGetBuildsOutput _
derive instance repGenericBatchGetBuildsOutput :: Generic BatchGetBuildsOutput _
instance showBatchGetBuildsOutput :: Show BatchGetBuildsOutput where
  show = genericShow
instance decodeBatchGetBuildsOutput :: Decode BatchGetBuildsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetBuildsOutput :: Encode BatchGetBuildsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetProjectsInput = BatchGetProjectsInput 
  { "Names'" :: (ProjectNames)
  }
derive instance newtypeBatchGetProjectsInput :: Newtype BatchGetProjectsInput _
derive instance repGenericBatchGetProjectsInput :: Generic BatchGetProjectsInput _
instance showBatchGetProjectsInput :: Show BatchGetProjectsInput where
  show = genericShow
instance decodeBatchGetProjectsInput :: Decode BatchGetProjectsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetProjectsInput :: Encode BatchGetProjectsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BatchGetProjectsOutput = BatchGetProjectsOutput 
  { "Projects'" :: NullOrUndefined.NullOrUndefined (Projects)
  , "ProjectsNotFound'" :: NullOrUndefined.NullOrUndefined (ProjectNames)
  }
derive instance newtypeBatchGetProjectsOutput :: Newtype BatchGetProjectsOutput _
derive instance repGenericBatchGetProjectsOutput :: Generic BatchGetProjectsOutput _
instance showBatchGetProjectsOutput :: Show BatchGetProjectsOutput where
  show = genericShow
instance decodeBatchGetProjectsOutput :: Decode BatchGetProjectsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBatchGetProjectsOutput :: Encode BatchGetProjectsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a build.</p>
newtype Build = Build 
  { "Id'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "StartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "CurrentPhase'" :: NullOrUndefined.NullOrUndefined (String)
  , "BuildStatus'" :: NullOrUndefined.NullOrUndefined (StatusType)
  , "SourceVersion'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "ProjectName'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Phases'" :: NullOrUndefined.NullOrUndefined (BuildPhases)
  , "Source'" :: NullOrUndefined.NullOrUndefined (ProjectSource)
  , "Artifacts'" :: NullOrUndefined.NullOrUndefined (BuildArtifacts)
  , "Cache'" :: NullOrUndefined.NullOrUndefined (ProjectCache)
  , "Environment'" :: NullOrUndefined.NullOrUndefined (ProjectEnvironment)
  , "Logs'" :: NullOrUndefined.NullOrUndefined (LogsLocation)
  , "TimeoutInMinutes'" :: NullOrUndefined.NullOrUndefined (WrapperInt)
  , "BuildComplete'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "Initiator'" :: NullOrUndefined.NullOrUndefined (String)
  , "VpcConfig'" :: NullOrUndefined.NullOrUndefined (VpcConfig)
  , "NetworkInterface'" :: NullOrUndefined.NullOrUndefined (NetworkInterface)
  }
derive instance newtypeBuild :: Newtype Build _
derive instance repGenericBuild :: Generic Build _
instance showBuild :: Show Build where
  show = genericShow
instance decodeBuild :: Decode Build where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuild :: Encode Build where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about build output artifacts.</p>
newtype BuildArtifacts = BuildArtifacts 
  { "Location'" :: NullOrUndefined.NullOrUndefined (String)
  , "Sha256sum'" :: NullOrUndefined.NullOrUndefined (String)
  , "Md5sum'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBuildArtifacts :: Newtype BuildArtifacts _
derive instance repGenericBuildArtifacts :: Generic BuildArtifacts _
instance showBuildArtifacts :: Show BuildArtifacts where
  show = genericShow
instance decodeBuildArtifacts :: Decode BuildArtifacts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildArtifacts :: Encode BuildArtifacts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuildIds = BuildIds (Array NonEmptyString)
derive instance newtypeBuildIds :: Newtype BuildIds _
derive instance repGenericBuildIds :: Generic BuildIds _
instance showBuildIds :: Show BuildIds where
  show = genericShow
instance decodeBuildIds :: Decode BuildIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildIds :: Encode BuildIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a build that could not be successfully deleted.</p>
newtype BuildNotDeleted = BuildNotDeleted 
  { "Id'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "StatusCode'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeBuildNotDeleted :: Newtype BuildNotDeleted _
derive instance repGenericBuildNotDeleted :: Generic BuildNotDeleted _
instance showBuildNotDeleted :: Show BuildNotDeleted where
  show = genericShow
instance decodeBuildNotDeleted :: Decode BuildNotDeleted where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildNotDeleted :: Encode BuildNotDeleted where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a stage for a build.</p>
newtype BuildPhase = BuildPhase 
  { "PhaseType'" :: NullOrUndefined.NullOrUndefined (BuildPhaseType)
  , "PhaseStatus'" :: NullOrUndefined.NullOrUndefined (StatusType)
  , "StartTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "EndTime'" :: NullOrUndefined.NullOrUndefined (Number)
  , "DurationInSeconds'" :: NullOrUndefined.NullOrUndefined (WrapperLong)
  , "Contexts'" :: NullOrUndefined.NullOrUndefined (PhaseContexts)
  }
derive instance newtypeBuildPhase :: Newtype BuildPhase _
derive instance repGenericBuildPhase :: Generic BuildPhase _
instance showBuildPhase :: Show BuildPhase where
  show = genericShow
instance decodeBuildPhase :: Decode BuildPhase where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildPhase :: Encode BuildPhase where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuildPhaseType = BuildPhaseType String
derive instance newtypeBuildPhaseType :: Newtype BuildPhaseType _
derive instance repGenericBuildPhaseType :: Generic BuildPhaseType _
instance showBuildPhaseType :: Show BuildPhaseType where
  show = genericShow
instance decodeBuildPhaseType :: Decode BuildPhaseType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildPhaseType :: Encode BuildPhaseType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuildPhases = BuildPhases (Array BuildPhase)
derive instance newtypeBuildPhases :: Newtype BuildPhases _
derive instance repGenericBuildPhases :: Generic BuildPhases _
instance showBuildPhases :: Show BuildPhases where
  show = genericShow
instance decodeBuildPhases :: Decode BuildPhases where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildPhases :: Encode BuildPhases where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Builds = Builds (Array Build)
derive instance newtypeBuilds :: Newtype Builds _
derive instance repGenericBuilds :: Generic Builds _
instance showBuilds :: Show Builds where
  show = genericShow
instance decodeBuilds :: Decode Builds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuilds :: Encode Builds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BuildsNotDeleted = BuildsNotDeleted (Array BuildNotDeleted)
derive instance newtypeBuildsNotDeleted :: Newtype BuildsNotDeleted _
derive instance repGenericBuildsNotDeleted :: Generic BuildsNotDeleted _
instance showBuildsNotDeleted :: Show BuildsNotDeleted where
  show = genericShow
instance decodeBuildsNotDeleted :: Decode BuildsNotDeleted where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBuildsNotDeleted :: Encode BuildsNotDeleted where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CacheType = CacheType String
derive instance newtypeCacheType :: Newtype CacheType _
derive instance repGenericCacheType :: Generic CacheType _
instance showCacheType :: Show CacheType where
  show = genericShow
instance decodeCacheType :: Decode CacheType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCacheType :: Encode CacheType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComputeType = ComputeType String
derive instance newtypeComputeType :: Newtype ComputeType _
derive instance repGenericComputeType :: Generic ComputeType _
instance showComputeType :: Show ComputeType where
  show = genericShow
instance decodeComputeType :: Decode ComputeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeType :: Encode ComputeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateProjectInput = CreateProjectInput 
  { "Name'" :: (ProjectName)
  , "Description'" :: NullOrUndefined.NullOrUndefined (ProjectDescription)
  , "Source'" :: (ProjectSource)
  , "Artifacts'" :: (ProjectArtifacts)
  , "Cache'" :: NullOrUndefined.NullOrUndefined (ProjectCache)
  , "Environment'" :: (ProjectEnvironment)
  , "ServiceRole'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "TimeoutInMinutes'" :: NullOrUndefined.NullOrUndefined (TimeOut)
  , "EncryptionKey'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "VpcConfig'" :: NullOrUndefined.NullOrUndefined (VpcConfig)
  , "BadgeEnabled'" :: NullOrUndefined.NullOrUndefined (WrapperBoolean)
  }
derive instance newtypeCreateProjectInput :: Newtype CreateProjectInput _
derive instance repGenericCreateProjectInput :: Generic CreateProjectInput _
instance showCreateProjectInput :: Show CreateProjectInput where
  show = genericShow
instance decodeCreateProjectInput :: Decode CreateProjectInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateProjectInput :: Encode CreateProjectInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateProjectOutput = CreateProjectOutput 
  { "Project'" :: NullOrUndefined.NullOrUndefined (Project)
  }
derive instance newtypeCreateProjectOutput :: Newtype CreateProjectOutput _
derive instance repGenericCreateProjectOutput :: Generic CreateProjectOutput _
instance showCreateProjectOutput :: Show CreateProjectOutput where
  show = genericShow
instance decodeCreateProjectOutput :: Decode CreateProjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateProjectOutput :: Encode CreateProjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateWebhookInput = CreateWebhookInput 
  { "ProjectName'" :: (ProjectName)
  }
derive instance newtypeCreateWebhookInput :: Newtype CreateWebhookInput _
derive instance repGenericCreateWebhookInput :: Generic CreateWebhookInput _
instance showCreateWebhookInput :: Show CreateWebhookInput where
  show = genericShow
instance decodeCreateWebhookInput :: Decode CreateWebhookInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateWebhookInput :: Encode CreateWebhookInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateWebhookOutput = CreateWebhookOutput 
  { "Webhook'" :: NullOrUndefined.NullOrUndefined (Webhook)
  }
derive instance newtypeCreateWebhookOutput :: Newtype CreateWebhookOutput _
derive instance repGenericCreateWebhookOutput :: Generic CreateWebhookOutput _
instance showCreateWebhookOutput :: Show CreateWebhookOutput where
  show = genericShow
instance decodeCreateWebhookOutput :: Decode CreateWebhookOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateWebhookOutput :: Encode CreateWebhookOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteProjectInput = DeleteProjectInput 
  { "Name'" :: (NonEmptyString)
  }
derive instance newtypeDeleteProjectInput :: Newtype DeleteProjectInput _
derive instance repGenericDeleteProjectInput :: Generic DeleteProjectInput _
instance showDeleteProjectInput :: Show DeleteProjectInput where
  show = genericShow
instance decodeDeleteProjectInput :: Decode DeleteProjectInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteProjectInput :: Encode DeleteProjectInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteProjectOutput = DeleteProjectOutput Types.NoArguments
derive instance newtypeDeleteProjectOutput :: Newtype DeleteProjectOutput _
derive instance repGenericDeleteProjectOutput :: Generic DeleteProjectOutput _
instance showDeleteProjectOutput :: Show DeleteProjectOutput where
  show = genericShow
instance decodeDeleteProjectOutput :: Decode DeleteProjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteProjectOutput :: Encode DeleteProjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteWebhookInput = DeleteWebhookInput 
  { "ProjectName'" :: (ProjectName)
  }
derive instance newtypeDeleteWebhookInput :: Newtype DeleteWebhookInput _
derive instance repGenericDeleteWebhookInput :: Generic DeleteWebhookInput _
instance showDeleteWebhookInput :: Show DeleteWebhookInput where
  show = genericShow
instance decodeDeleteWebhookInput :: Decode DeleteWebhookInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteWebhookInput :: Encode DeleteWebhookInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteWebhookOutput = DeleteWebhookOutput Types.NoArguments
derive instance newtypeDeleteWebhookOutput :: Newtype DeleteWebhookOutput _
derive instance repGenericDeleteWebhookOutput :: Generic DeleteWebhookOutput _
instance showDeleteWebhookOutput :: Show DeleteWebhookOutput where
  show = genericShow
instance decodeDeleteWebhookOutput :: Decode DeleteWebhookOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteWebhookOutput :: Encode DeleteWebhookOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a Docker image that is managed by AWS CodeBuild.</p>
newtype EnvironmentImage = EnvironmentImage 
  { "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (String)
  , "Versions'" :: NullOrUndefined.NullOrUndefined (ImageVersions)
  }
derive instance newtypeEnvironmentImage :: Newtype EnvironmentImage _
derive instance repGenericEnvironmentImage :: Generic EnvironmentImage _
instance showEnvironmentImage :: Show EnvironmentImage where
  show = genericShow
instance decodeEnvironmentImage :: Decode EnvironmentImage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentImage :: Encode EnvironmentImage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentImages = EnvironmentImages (Array EnvironmentImage)
derive instance newtypeEnvironmentImages :: Newtype EnvironmentImages _
derive instance repGenericEnvironmentImages :: Generic EnvironmentImages _
instance showEnvironmentImages :: Show EnvironmentImages where
  show = genericShow
instance decodeEnvironmentImages :: Decode EnvironmentImages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentImages :: Encode EnvironmentImages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A set of Docker images that are related by programming language and are managed by AWS CodeBuild.</p>
newtype EnvironmentLanguage = EnvironmentLanguage 
  { "Language'" :: NullOrUndefined.NullOrUndefined (LanguageType)
  , "Images'" :: NullOrUndefined.NullOrUndefined (EnvironmentImages)
  }
derive instance newtypeEnvironmentLanguage :: Newtype EnvironmentLanguage _
derive instance repGenericEnvironmentLanguage :: Generic EnvironmentLanguage _
instance showEnvironmentLanguage :: Show EnvironmentLanguage where
  show = genericShow
instance decodeEnvironmentLanguage :: Decode EnvironmentLanguage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentLanguage :: Encode EnvironmentLanguage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentLanguages = EnvironmentLanguages (Array EnvironmentLanguage)
derive instance newtypeEnvironmentLanguages :: Newtype EnvironmentLanguages _
derive instance repGenericEnvironmentLanguages :: Generic EnvironmentLanguages _
instance showEnvironmentLanguages :: Show EnvironmentLanguages where
  show = genericShow
instance decodeEnvironmentLanguages :: Decode EnvironmentLanguages where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentLanguages :: Encode EnvironmentLanguages where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A set of Docker images that are related by platform and are managed by AWS CodeBuild.</p>
newtype EnvironmentPlatform = EnvironmentPlatform 
  { "Platform'" :: NullOrUndefined.NullOrUndefined (PlatformType)
  , "Languages'" :: NullOrUndefined.NullOrUndefined (EnvironmentLanguages)
  }
derive instance newtypeEnvironmentPlatform :: Newtype EnvironmentPlatform _
derive instance repGenericEnvironmentPlatform :: Generic EnvironmentPlatform _
instance showEnvironmentPlatform :: Show EnvironmentPlatform where
  show = genericShow
instance decodeEnvironmentPlatform :: Decode EnvironmentPlatform where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentPlatform :: Encode EnvironmentPlatform where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentPlatforms = EnvironmentPlatforms (Array EnvironmentPlatform)
derive instance newtypeEnvironmentPlatforms :: Newtype EnvironmentPlatforms _
derive instance repGenericEnvironmentPlatforms :: Generic EnvironmentPlatforms _
instance showEnvironmentPlatforms :: Show EnvironmentPlatforms where
  show = genericShow
instance decodeEnvironmentPlatforms :: Decode EnvironmentPlatforms where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentPlatforms :: Encode EnvironmentPlatforms where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentType = EnvironmentType String
derive instance newtypeEnvironmentType :: Newtype EnvironmentType _
derive instance repGenericEnvironmentType :: Generic EnvironmentType _
instance showEnvironmentType :: Show EnvironmentType where
  show = genericShow
instance decodeEnvironmentType :: Decode EnvironmentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentType :: Encode EnvironmentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about an environment variable for a build project or a build.</p>
newtype EnvironmentVariable = EnvironmentVariable 
  { "Name'" :: (NonEmptyString)
  , "Value'" :: (String)
  , "Type'" :: NullOrUndefined.NullOrUndefined (EnvironmentVariableType)
  }
derive instance newtypeEnvironmentVariable :: Newtype EnvironmentVariable _
derive instance repGenericEnvironmentVariable :: Generic EnvironmentVariable _
instance showEnvironmentVariable :: Show EnvironmentVariable where
  show = genericShow
instance decodeEnvironmentVariable :: Decode EnvironmentVariable where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentVariable :: Encode EnvironmentVariable where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentVariableType = EnvironmentVariableType String
derive instance newtypeEnvironmentVariableType :: Newtype EnvironmentVariableType _
derive instance repGenericEnvironmentVariableType :: Generic EnvironmentVariableType _
instance showEnvironmentVariableType :: Show EnvironmentVariableType where
  show = genericShow
instance decodeEnvironmentVariableType :: Decode EnvironmentVariableType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentVariableType :: Encode EnvironmentVariableType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EnvironmentVariables = EnvironmentVariables (Array EnvironmentVariable)
derive instance newtypeEnvironmentVariables :: Newtype EnvironmentVariables _
derive instance repGenericEnvironmentVariables :: Generic EnvironmentVariables _
instance showEnvironmentVariables :: Show EnvironmentVariables where
  show = genericShow
instance decodeEnvironmentVariables :: Decode EnvironmentVariables where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnvironmentVariables :: Encode EnvironmentVariables where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GitCloneDepth = GitCloneDepth Int
derive instance newtypeGitCloneDepth :: Newtype GitCloneDepth _
derive instance repGenericGitCloneDepth :: Generic GitCloneDepth _
instance showGitCloneDepth :: Show GitCloneDepth where
  show = genericShow
instance decodeGitCloneDepth :: Decode GitCloneDepth where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGitCloneDepth :: Encode GitCloneDepth where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ImageVersions = ImageVersions (Array String)
derive instance newtypeImageVersions :: Newtype ImageVersions _
derive instance repGenericImageVersions :: Generic ImageVersions _
instance showImageVersions :: Show ImageVersions where
  show = genericShow
instance decodeImageVersions :: Decode ImageVersions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeImageVersions :: Encode ImageVersions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The input value that was provided is not valid.</p>
newtype InvalidInputException = InvalidInputException Types.NoArguments
derive instance newtypeInvalidInputException :: Newtype InvalidInputException _
derive instance repGenericInvalidInputException :: Generic InvalidInputException _
instance showInvalidInputException :: Show InvalidInputException where
  show = genericShow
instance decodeInvalidInputException :: Decode InvalidInputException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidInputException :: Encode InvalidInputException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidateProjectCacheInput = InvalidateProjectCacheInput 
  { "ProjectName'" :: (NonEmptyString)
  }
derive instance newtypeInvalidateProjectCacheInput :: Newtype InvalidateProjectCacheInput _
derive instance repGenericInvalidateProjectCacheInput :: Generic InvalidateProjectCacheInput _
instance showInvalidateProjectCacheInput :: Show InvalidateProjectCacheInput where
  show = genericShow
instance decodeInvalidateProjectCacheInput :: Decode InvalidateProjectCacheInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidateProjectCacheInput :: Encode InvalidateProjectCacheInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InvalidateProjectCacheOutput = InvalidateProjectCacheOutput Types.NoArguments
derive instance newtypeInvalidateProjectCacheOutput :: Newtype InvalidateProjectCacheOutput _
derive instance repGenericInvalidateProjectCacheOutput :: Generic InvalidateProjectCacheOutput _
instance showInvalidateProjectCacheOutput :: Show InvalidateProjectCacheOutput where
  show = genericShow
instance decodeInvalidateProjectCacheOutput :: Decode InvalidateProjectCacheOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidateProjectCacheOutput :: Encode InvalidateProjectCacheOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype KeyInput = KeyInput String
derive instance newtypeKeyInput :: Newtype KeyInput _
derive instance repGenericKeyInput :: Generic KeyInput _
instance showKeyInput :: Show KeyInput where
  show = genericShow
instance decodeKeyInput :: Decode KeyInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeKeyInput :: Encode KeyInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LanguageType = LanguageType String
derive instance newtypeLanguageType :: Newtype LanguageType _
derive instance repGenericLanguageType :: Generic LanguageType _
instance showLanguageType :: Show LanguageType where
  show = genericShow
instance decodeLanguageType :: Decode LanguageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLanguageType :: Encode LanguageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBuildsForProjectInput = ListBuildsForProjectInput 
  { "ProjectName'" :: (NonEmptyString)
  , "SortOrder'" :: NullOrUndefined.NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListBuildsForProjectInput :: Newtype ListBuildsForProjectInput _
derive instance repGenericListBuildsForProjectInput :: Generic ListBuildsForProjectInput _
instance showListBuildsForProjectInput :: Show ListBuildsForProjectInput where
  show = genericShow
instance decodeListBuildsForProjectInput :: Decode ListBuildsForProjectInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBuildsForProjectInput :: Encode ListBuildsForProjectInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBuildsForProjectOutput = ListBuildsForProjectOutput 
  { "Ids'" :: NullOrUndefined.NullOrUndefined (BuildIds)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListBuildsForProjectOutput :: Newtype ListBuildsForProjectOutput _
derive instance repGenericListBuildsForProjectOutput :: Generic ListBuildsForProjectOutput _
instance showListBuildsForProjectOutput :: Show ListBuildsForProjectOutput where
  show = genericShow
instance decodeListBuildsForProjectOutput :: Decode ListBuildsForProjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBuildsForProjectOutput :: Encode ListBuildsForProjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBuildsInput = ListBuildsInput 
  { "SortOrder'" :: NullOrUndefined.NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListBuildsInput :: Newtype ListBuildsInput _
derive instance repGenericListBuildsInput :: Generic ListBuildsInput _
instance showListBuildsInput :: Show ListBuildsInput where
  show = genericShow
instance decodeListBuildsInput :: Decode ListBuildsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBuildsInput :: Encode ListBuildsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListBuildsOutput = ListBuildsOutput 
  { "Ids'" :: NullOrUndefined.NullOrUndefined (BuildIds)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeListBuildsOutput :: Newtype ListBuildsOutput _
derive instance repGenericListBuildsOutput :: Generic ListBuildsOutput _
instance showListBuildsOutput :: Show ListBuildsOutput where
  show = genericShow
instance decodeListBuildsOutput :: Decode ListBuildsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBuildsOutput :: Encode ListBuildsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCuratedEnvironmentImagesInput = ListCuratedEnvironmentImagesInput Types.NoArguments
derive instance newtypeListCuratedEnvironmentImagesInput :: Newtype ListCuratedEnvironmentImagesInput _
derive instance repGenericListCuratedEnvironmentImagesInput :: Generic ListCuratedEnvironmentImagesInput _
instance showListCuratedEnvironmentImagesInput :: Show ListCuratedEnvironmentImagesInput where
  show = genericShow
instance decodeListCuratedEnvironmentImagesInput :: Decode ListCuratedEnvironmentImagesInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCuratedEnvironmentImagesInput :: Encode ListCuratedEnvironmentImagesInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListCuratedEnvironmentImagesOutput = ListCuratedEnvironmentImagesOutput 
  { "Platforms'" :: NullOrUndefined.NullOrUndefined (EnvironmentPlatforms)
  }
derive instance newtypeListCuratedEnvironmentImagesOutput :: Newtype ListCuratedEnvironmentImagesOutput _
derive instance repGenericListCuratedEnvironmentImagesOutput :: Generic ListCuratedEnvironmentImagesOutput _
instance showListCuratedEnvironmentImagesOutput :: Show ListCuratedEnvironmentImagesOutput where
  show = genericShow
instance decodeListCuratedEnvironmentImagesOutput :: Decode ListCuratedEnvironmentImagesOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListCuratedEnvironmentImagesOutput :: Encode ListCuratedEnvironmentImagesOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListProjectsInput = ListProjectsInput 
  { "SortBy'" :: NullOrUndefined.NullOrUndefined (ProjectSortByType)
  , "SortOrder'" :: NullOrUndefined.NullOrUndefined (SortOrderType)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeListProjectsInput :: Newtype ListProjectsInput _
derive instance repGenericListProjectsInput :: Generic ListProjectsInput _
instance showListProjectsInput :: Show ListProjectsInput where
  show = genericShow
instance decodeListProjectsInput :: Decode ListProjectsInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProjectsInput :: Encode ListProjectsInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListProjectsOutput = ListProjectsOutput 
  { "NextToken'" :: NullOrUndefined.NullOrUndefined (String)
  , "Projects'" :: NullOrUndefined.NullOrUndefined (ProjectNames)
  }
derive instance newtypeListProjectsOutput :: Newtype ListProjectsOutput _
derive instance repGenericListProjectsOutput :: Generic ListProjectsOutput _
instance showListProjectsOutput :: Show ListProjectsOutput where
  show = genericShow
instance decodeListProjectsOutput :: Decode ListProjectsOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProjectsOutput :: Encode ListProjectsOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about build logs in Amazon CloudWatch Logs.</p>
newtype LogsLocation = LogsLocation 
  { "GroupName'" :: NullOrUndefined.NullOrUndefined (String)
  , "StreamName'" :: NullOrUndefined.NullOrUndefined (String)
  , "DeepLink'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeLogsLocation :: Newtype LogsLocation _
derive instance repGenericLogsLocation :: Generic LogsLocation _
instance showLogsLocation :: Show LogsLocation where
  show = genericShow
instance decodeLogsLocation :: Decode LogsLocation where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLogsLocation :: Encode LogsLocation where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a network interface.</p>
newtype NetworkInterface = NetworkInterface 
  { "SubnetId'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "NetworkInterfaceId'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeNetworkInterface :: Newtype NetworkInterface _
derive instance repGenericNetworkInterface :: Generic NetworkInterface _
instance showNetworkInterface :: Show NetworkInterface where
  show = genericShow
instance decodeNetworkInterface :: Decode NetworkInterface where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNetworkInterface :: Encode NetworkInterface where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _
derive instance repGenericNonEmptyString :: Generic NonEmptyString _
instance showNonEmptyString :: Show NonEmptyString where
  show = genericShow
instance decodeNonEmptyString :: Decode NonEmptyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonEmptyString :: Encode NonEmptyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>There was a problem with the underlying OAuth provider.</p>
newtype OAuthProviderException = OAuthProviderException Types.NoArguments
derive instance newtypeOAuthProviderException :: Newtype OAuthProviderException _
derive instance repGenericOAuthProviderException :: Generic OAuthProviderException _
instance showOAuthProviderException :: Show OAuthProviderException where
  show = genericShow
instance decodeOAuthProviderException :: Decode OAuthProviderException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOAuthProviderException :: Encode OAuthProviderException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Additional information about a build phase that has an error. You can use this information to help troubleshoot a failed build.</p>
newtype PhaseContext = PhaseContext 
  { "StatusCode'" :: NullOrUndefined.NullOrUndefined (String)
  , "Message'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypePhaseContext :: Newtype PhaseContext _
derive instance repGenericPhaseContext :: Generic PhaseContext _
instance showPhaseContext :: Show PhaseContext where
  show = genericShow
instance decodePhaseContext :: Decode PhaseContext where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePhaseContext :: Encode PhaseContext where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PhaseContexts = PhaseContexts (Array PhaseContext)
derive instance newtypePhaseContexts :: Newtype PhaseContexts _
derive instance repGenericPhaseContexts :: Generic PhaseContexts _
instance showPhaseContexts :: Show PhaseContexts where
  show = genericShow
instance decodePhaseContexts :: Decode PhaseContexts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePhaseContexts :: Encode PhaseContexts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PlatformType = PlatformType String
derive instance newtypePlatformType :: Newtype PlatformType _
derive instance repGenericPlatformType :: Generic PlatformType _
instance showPlatformType :: Show PlatformType where
  show = genericShow
instance decodePlatformType :: Decode PlatformType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatformType :: Encode PlatformType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a build project.</p>
newtype Project = Project 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ProjectName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (String)
  , "Description'" :: NullOrUndefined.NullOrUndefined (ProjectDescription)
  , "Source'" :: NullOrUndefined.NullOrUndefined (ProjectSource)
  , "Artifacts'" :: NullOrUndefined.NullOrUndefined (ProjectArtifacts)
  , "Cache'" :: NullOrUndefined.NullOrUndefined (ProjectCache)
  , "Environment'" :: NullOrUndefined.NullOrUndefined (ProjectEnvironment)
  , "ServiceRole'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "TimeoutInMinutes'" :: NullOrUndefined.NullOrUndefined (TimeOut)
  , "EncryptionKey'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "Created'" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastModified'" :: NullOrUndefined.NullOrUndefined (Number)
  , "Webhook'" :: NullOrUndefined.NullOrUndefined (Webhook)
  , "VpcConfig'" :: NullOrUndefined.NullOrUndefined (VpcConfig)
  , "Badge'" :: NullOrUndefined.NullOrUndefined (ProjectBadge)
  }
derive instance newtypeProject :: Newtype Project _
derive instance repGenericProject :: Generic Project _
instance showProject :: Show Project where
  show = genericShow
instance decodeProject :: Decode Project where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProject :: Encode Project where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the build output artifacts for the build project.</p>
newtype ProjectArtifacts = ProjectArtifacts 
  { "Type'" :: (ArtifactsType)
  , "Location'" :: NullOrUndefined.NullOrUndefined (String)
  , "Path'" :: NullOrUndefined.NullOrUndefined (String)
  , "NamespaceType'" :: NullOrUndefined.NullOrUndefined (ArtifactNamespace)
  , "Name'" :: NullOrUndefined.NullOrUndefined (String)
  , "Packaging'" :: NullOrUndefined.NullOrUndefined (ArtifactPackaging)
  }
derive instance newtypeProjectArtifacts :: Newtype ProjectArtifacts _
derive instance repGenericProjectArtifacts :: Generic ProjectArtifacts _
instance showProjectArtifacts :: Show ProjectArtifacts where
  show = genericShow
instance decodeProjectArtifacts :: Decode ProjectArtifacts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectArtifacts :: Encode ProjectArtifacts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the build badge for the build project.</p>
newtype ProjectBadge = ProjectBadge 
  { "BadgeEnabled'" :: NullOrUndefined.NullOrUndefined (Boolean)
  , "BadgeRequestUrl'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeProjectBadge :: Newtype ProjectBadge _
derive instance repGenericProjectBadge :: Generic ProjectBadge _
instance showProjectBadge :: Show ProjectBadge where
  show = genericShow
instance decodeProjectBadge :: Decode ProjectBadge where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectBadge :: Encode ProjectBadge where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the cache for the build project.</p>
newtype ProjectCache = ProjectCache 
  { "Type'" :: (CacheType)
  , "Location'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeProjectCache :: Newtype ProjectCache _
derive instance repGenericProjectCache :: Generic ProjectCache _
instance showProjectCache :: Show ProjectCache where
  show = genericShow
instance decodeProjectCache :: Decode ProjectCache where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectCache :: Encode ProjectCache where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProjectDescription = ProjectDescription String
derive instance newtypeProjectDescription :: Newtype ProjectDescription _
derive instance repGenericProjectDescription :: Generic ProjectDescription _
instance showProjectDescription :: Show ProjectDescription where
  show = genericShow
instance decodeProjectDescription :: Decode ProjectDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectDescription :: Encode ProjectDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the build environment of the build project.</p>
newtype ProjectEnvironment = ProjectEnvironment 
  { "Type'" :: (EnvironmentType)
  , "Image'" :: (NonEmptyString)
  , "ComputeType'" :: (ComputeType)
  , "EnvironmentVariables'" :: NullOrUndefined.NullOrUndefined (EnvironmentVariables)
  , "PrivilegedMode'" :: NullOrUndefined.NullOrUndefined (WrapperBoolean)
  , "Certificate'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeProjectEnvironment :: Newtype ProjectEnvironment _
derive instance repGenericProjectEnvironment :: Generic ProjectEnvironment _
instance showProjectEnvironment :: Show ProjectEnvironment where
  show = genericShow
instance decodeProjectEnvironment :: Decode ProjectEnvironment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectEnvironment :: Encode ProjectEnvironment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProjectName = ProjectName String
derive instance newtypeProjectName :: Newtype ProjectName _
derive instance repGenericProjectName :: Generic ProjectName _
instance showProjectName :: Show ProjectName where
  show = genericShow
instance decodeProjectName :: Decode ProjectName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectName :: Encode ProjectName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProjectNames = ProjectNames (Array NonEmptyString)
derive instance newtypeProjectNames :: Newtype ProjectNames _
derive instance repGenericProjectNames :: Generic ProjectNames _
instance showProjectNames :: Show ProjectNames where
  show = genericShow
instance decodeProjectNames :: Decode ProjectNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectNames :: Encode ProjectNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ProjectSortByType = ProjectSortByType String
derive instance newtypeProjectSortByType :: Newtype ProjectSortByType _
derive instance repGenericProjectSortByType :: Generic ProjectSortByType _
instance showProjectSortByType :: Show ProjectSortByType where
  show = genericShow
instance decodeProjectSortByType :: Decode ProjectSortByType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectSortByType :: Encode ProjectSortByType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the build input source code for the build project.</p>
newtype ProjectSource = ProjectSource 
  { "Type'" :: (SourceType)
  , "Location'" :: NullOrUndefined.NullOrUndefined (String)
  , "GitCloneDepth'" :: NullOrUndefined.NullOrUndefined (GitCloneDepth)
  , "Buildspec'" :: NullOrUndefined.NullOrUndefined (String)
  , "Auth'" :: NullOrUndefined.NullOrUndefined (SourceAuth)
  , "InsecureSsl'" :: NullOrUndefined.NullOrUndefined (WrapperBoolean)
  }
derive instance newtypeProjectSource :: Newtype ProjectSource _
derive instance repGenericProjectSource :: Generic ProjectSource _
instance showProjectSource :: Show ProjectSource where
  show = genericShow
instance decodeProjectSource :: Decode ProjectSource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectSource :: Encode ProjectSource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Projects = Projects (Array Project)
derive instance newtypeProjects :: Newtype Projects _
derive instance repGenericProjects :: Generic Projects _
instance showProjects :: Show Projects where
  show = genericShow
instance decodeProjects :: Decode Projects where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjects :: Encode Projects where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified AWS resource cannot be created, because an AWS resource with the same settings already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException Types.NoArguments
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _
derive instance repGenericResourceAlreadyExistsException :: Generic ResourceAlreadyExistsException _
instance showResourceAlreadyExistsException :: Show ResourceAlreadyExistsException where
  show = genericShow
instance decodeResourceAlreadyExistsException :: Decode ResourceAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAlreadyExistsException :: Encode ResourceAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified AWS resource cannot be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException Types.NoArguments
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroupIds = SecurityGroupIds (Array NonEmptyString)
derive instance newtypeSecurityGroupIds :: Newtype SecurityGroupIds _
derive instance repGenericSecurityGroupIds :: Generic SecurityGroupIds _
instance showSecurityGroupIds :: Show SecurityGroupIds where
  show = genericShow
instance decodeSecurityGroupIds :: Decode SecurityGroupIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupIds :: Encode SecurityGroupIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SortOrderType = SortOrderType String
derive instance newtypeSortOrderType :: Newtype SortOrderType _
derive instance repGenericSortOrderType :: Generic SortOrderType _
instance showSortOrderType :: Show SortOrderType where
  show = genericShow
instance decodeSortOrderType :: Decode SortOrderType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSortOrderType :: Encode SortOrderType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the authorization settings for AWS CodeBuild to access the source code to be built.</p> <p>This information is for the AWS CodeBuild console's use only. Your code should not get or set this information directly (unless the build project's source <code>type</code> value is <code>BITBUCKET</code> or <code>GITHUB</code>).</p>
newtype SourceAuth = SourceAuth 
  { "Type'" :: (SourceAuthType)
  , "Resource'" :: NullOrUndefined.NullOrUndefined (String)
  }
derive instance newtypeSourceAuth :: Newtype SourceAuth _
derive instance repGenericSourceAuth :: Generic SourceAuth _
instance showSourceAuth :: Show SourceAuth where
  show = genericShow
instance decodeSourceAuth :: Decode SourceAuth where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceAuth :: Encode SourceAuth where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceAuthType = SourceAuthType String
derive instance newtypeSourceAuthType :: Newtype SourceAuthType _
derive instance repGenericSourceAuthType :: Generic SourceAuthType _
instance showSourceAuthType :: Show SourceAuthType where
  show = genericShow
instance decodeSourceAuthType :: Decode SourceAuthType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceAuthType :: Encode SourceAuthType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SourceType = SourceType String
derive instance newtypeSourceType :: Newtype SourceType _
derive instance repGenericSourceType :: Generic SourceType _
instance showSourceType :: Show SourceType where
  show = genericShow
instance decodeSourceType :: Decode SourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSourceType :: Encode SourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartBuildInput = StartBuildInput 
  { "ProjectName'" :: (NonEmptyString)
  , "SourceVersion'" :: NullOrUndefined.NullOrUndefined (String)
  , "ArtifactsOverride'" :: NullOrUndefined.NullOrUndefined (ProjectArtifacts)
  , "EnvironmentVariablesOverride'" :: NullOrUndefined.NullOrUndefined (EnvironmentVariables)
  , "GitCloneDepthOverride'" :: NullOrUndefined.NullOrUndefined (GitCloneDepth)
  , "BuildspecOverride'" :: NullOrUndefined.NullOrUndefined (String)
  , "TimeoutInMinutesOverride'" :: NullOrUndefined.NullOrUndefined (TimeOut)
  }
derive instance newtypeStartBuildInput :: Newtype StartBuildInput _
derive instance repGenericStartBuildInput :: Generic StartBuildInput _
instance showStartBuildInput :: Show StartBuildInput where
  show = genericShow
instance decodeStartBuildInput :: Decode StartBuildInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartBuildInput :: Encode StartBuildInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartBuildOutput = StartBuildOutput 
  { "Build'" :: NullOrUndefined.NullOrUndefined (Build)
  }
derive instance newtypeStartBuildOutput :: Newtype StartBuildOutput _
derive instance repGenericStartBuildOutput :: Generic StartBuildOutput _
instance showStartBuildOutput :: Show StartBuildOutput where
  show = genericShow
instance decodeStartBuildOutput :: Decode StartBuildOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartBuildOutput :: Encode StartBuildOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StatusType = StatusType String
derive instance newtypeStatusType :: Newtype StatusType _
derive instance repGenericStatusType :: Generic StatusType _
instance showStatusType :: Show StatusType where
  show = genericShow
instance decodeStatusType :: Decode StatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStatusType :: Encode StatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopBuildInput = StopBuildInput 
  { "Id'" :: (NonEmptyString)
  }
derive instance newtypeStopBuildInput :: Newtype StopBuildInput _
derive instance repGenericStopBuildInput :: Generic StopBuildInput _
instance showStopBuildInput :: Show StopBuildInput where
  show = genericShow
instance decodeStopBuildInput :: Decode StopBuildInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopBuildInput :: Encode StopBuildInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopBuildOutput = StopBuildOutput 
  { "Build'" :: NullOrUndefined.NullOrUndefined (Build)
  }
derive instance newtypeStopBuildOutput :: Newtype StopBuildOutput _
derive instance repGenericStopBuildOutput :: Generic StopBuildOutput _
instance showStopBuildOutput :: Show StopBuildOutput where
  show = genericShow
instance decodeStopBuildOutput :: Decode StopBuildOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopBuildOutput :: Encode StopBuildOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Subnets = Subnets (Array NonEmptyString)
derive instance newtypeSubnets :: Newtype Subnets _
derive instance repGenericSubnets :: Generic Subnets _
instance showSubnets :: Show Subnets where
  show = genericShow
instance decodeSubnets :: Decode Subnets where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnets :: Encode Subnets where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A tag, consisting of a key and a value.</p> <p>This tag is available for use by AWS services that support tags in AWS CodeBuild.</p>
newtype Tag = Tag 
  { "Key'" :: NullOrUndefined.NullOrUndefined (KeyInput)
  , "Value'" :: NullOrUndefined.NullOrUndefined (ValueInput)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeOut = TimeOut Int
derive instance newtypeTimeOut :: Newtype TimeOut _
derive instance repGenericTimeOut :: Generic TimeOut _
instance showTimeOut :: Show TimeOut where
  show = genericShow
instance decodeTimeOut :: Decode TimeOut where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeOut :: Encode TimeOut where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateProjectInput = UpdateProjectInput 
  { "Name'" :: (NonEmptyString)
  , "Description'" :: NullOrUndefined.NullOrUndefined (ProjectDescription)
  , "Source'" :: NullOrUndefined.NullOrUndefined (ProjectSource)
  , "Artifacts'" :: NullOrUndefined.NullOrUndefined (ProjectArtifacts)
  , "Cache'" :: NullOrUndefined.NullOrUndefined (ProjectCache)
  , "Environment'" :: NullOrUndefined.NullOrUndefined (ProjectEnvironment)
  , "ServiceRole'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "TimeoutInMinutes'" :: NullOrUndefined.NullOrUndefined (TimeOut)
  , "EncryptionKey'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Tags'" :: NullOrUndefined.NullOrUndefined (TagList)
  , "VpcConfig'" :: NullOrUndefined.NullOrUndefined (VpcConfig)
  , "BadgeEnabled'" :: NullOrUndefined.NullOrUndefined (WrapperBoolean)
  }
derive instance newtypeUpdateProjectInput :: Newtype UpdateProjectInput _
derive instance repGenericUpdateProjectInput :: Generic UpdateProjectInput _
instance showUpdateProjectInput :: Show UpdateProjectInput where
  show = genericShow
instance decodeUpdateProjectInput :: Decode UpdateProjectInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateProjectInput :: Encode UpdateProjectInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateProjectOutput = UpdateProjectOutput 
  { "Project'" :: NullOrUndefined.NullOrUndefined (Project)
  }
derive instance newtypeUpdateProjectOutput :: Newtype UpdateProjectOutput _
derive instance repGenericUpdateProjectOutput :: Generic UpdateProjectOutput _
instance showUpdateProjectOutput :: Show UpdateProjectOutput where
  show = genericShow
instance decodeUpdateProjectOutput :: Decode UpdateProjectOutput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateProjectOutput :: Encode UpdateProjectOutput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ValueInput = ValueInput String
derive instance newtypeValueInput :: Newtype ValueInput _
derive instance repGenericValueInput :: Generic ValueInput _
instance showValueInput :: Show ValueInput where
  show = genericShow
instance decodeValueInput :: Decode ValueInput where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeValueInput :: Encode ValueInput where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the VPC configuration that AWS CodeBuild will access.</p>
newtype VpcConfig = VpcConfig 
  { "VpcId'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Subnets'" :: NullOrUndefined.NullOrUndefined (Subnets)
  , "SecurityGroupIds'" :: NullOrUndefined.NullOrUndefined (SecurityGroupIds)
  }
derive instance newtypeVpcConfig :: Newtype VpcConfig _
derive instance repGenericVpcConfig :: Generic VpcConfig _
instance showVpcConfig :: Show VpcConfig where
  show = genericShow
instance decodeVpcConfig :: Decode VpcConfig where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcConfig :: Encode VpcConfig where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a webhook in GitHub that connects repository events to a build project in AWS CodeBuild.</p>
newtype Webhook = Webhook 
  { "Url'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "PayloadUrl'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Secret'" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeWebhook :: Newtype Webhook _
derive instance repGenericWebhook :: Generic Webhook _
instance showWebhook :: Show Webhook where
  show = genericShow
instance decodeWebhook :: Decode Webhook where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWebhook :: Encode Webhook where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WrapperBoolean = WrapperBoolean Boolean
derive instance newtypeWrapperBoolean :: Newtype WrapperBoolean _
derive instance repGenericWrapperBoolean :: Generic WrapperBoolean _
instance showWrapperBoolean :: Show WrapperBoolean where
  show = genericShow
instance decodeWrapperBoolean :: Decode WrapperBoolean where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWrapperBoolean :: Encode WrapperBoolean where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WrapperInt = WrapperInt Int
derive instance newtypeWrapperInt :: Newtype WrapperInt _
derive instance repGenericWrapperInt :: Generic WrapperInt _
instance showWrapperInt :: Show WrapperInt where
  show = genericShow
instance decodeWrapperInt :: Decode WrapperInt where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWrapperInt :: Encode WrapperInt where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WrapperLong = WrapperLong Number
derive instance newtypeWrapperLong :: Newtype WrapperLong _
derive instance repGenericWrapperLong :: Generic WrapperLong _
instance showWrapperLong :: Show WrapperLong where
  show = genericShow
instance decodeWrapperLong :: Decode WrapperLong where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWrapperLong :: Encode WrapperLong where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
