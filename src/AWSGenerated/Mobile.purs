

-- | <p> AWS Mobile Service provides mobile app and website developers with capabilities required to configure AWS resources and bootstrap their developer desktop projects with the necessary SDKs, constants, tools and samples to make use of those resources. </p>
module AWS.Mobile where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "Mobile" :: String


-- | <p> Creates an AWS Mobile Hub project. </p>
createProject :: forall eff. CreateProjectRequest -> Aff (err :: AWS.RequestError | eff) CreateProjectResult
createProject = AWS.request serviceName "createProject" 


-- | <p> Delets a project in AWS Mobile Hub. </p>
deleteProject :: forall eff. DeleteProjectRequest -> Aff (err :: AWS.RequestError | eff) DeleteProjectResult
deleteProject = AWS.request serviceName "deleteProject" 


-- | <p> Get the bundle details for the requested bundle id. </p>
describeBundle :: forall eff. DescribeBundleRequest -> Aff (err :: AWS.RequestError | eff) DescribeBundleResult
describeBundle = AWS.request serviceName "describeBundle" 


-- | <p> Gets details about a project in AWS Mobile Hub. </p>
describeProject :: forall eff. DescribeProjectRequest -> Aff (err :: AWS.RequestError | eff) DescribeProjectResult
describeProject = AWS.request serviceName "describeProject" 


-- | <p> Generates customized software development kit (SDK) and or tool packages used to integrate mobile web or mobile app clients with backend AWS resources. </p>
exportBundle :: forall eff. ExportBundleRequest -> Aff (err :: AWS.RequestError | eff) ExportBundleResult
exportBundle = AWS.request serviceName "exportBundle" 


-- | <p> Exports project configuration to a snapshot which can be downloaded and shared. Note that mobile app push credentials are encrypted in exported projects, so they can only be shared successfully within the same AWS account. </p>
exportProject :: forall eff. ExportProjectRequest -> Aff (err :: AWS.RequestError | eff) ExportProjectResult
exportProject = AWS.request serviceName "exportProject" 


-- | <p> List all available bundles. </p>
listBundles :: forall eff. ListBundlesRequest -> Aff (err :: AWS.RequestError | eff) ListBundlesResult
listBundles = AWS.request serviceName "listBundles" 


-- | <p> Lists projects in AWS Mobile Hub. </p>
listProjects :: forall eff. ListProjectsRequest -> Aff (err :: AWS.RequestError | eff) ListProjectsResult
listProjects = AWS.request serviceName "listProjects" 


-- | <p> Update an existing project. </p>
updateProject :: forall eff. UpdateProjectRequest -> Aff (err :: AWS.RequestError | eff) UpdateProjectResult
updateProject = AWS.request serviceName "updateProject" 


-- | <p> Account Action is required in order to continue the request. </p>
newtype AccountActionRequiredException = AccountActionRequiredException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAccountActionRequiredException :: Newtype AccountActionRequiredException _


-- | <p> Key part of key-value attribute pairs. </p>
newtype AttributeKey = AttributeKey String
derive instance newtypeAttributeKey :: Newtype AttributeKey _


-- | <p> Value part of key-value attribute pairs. </p>
newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _


-- | <p> Key-value attribute pairs. </p>
newtype Attributes = Attributes (Map AttributeKey AttributeValue)
derive instance newtypeAttributes :: Newtype Attributes _


-- | <p> The request cannot be processed because some parameter is not valid or the project state prevents the operation from being performed. </p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _


-- | <p> Description of the download bundle. </p>
newtype BundleDescription = BundleDescription String
derive instance newtypeBundleDescription :: Newtype BundleDescription _


-- | <p> The details of the bundle. </p>
newtype BundleDetails = BundleDetails 
  { "BundleId'" :: NullOrUndefined (BundleId)
  , "Title'" :: NullOrUndefined (BundleTitle)
  , "Version'" :: NullOrUndefined (BundleVersion)
  , "Description'" :: NullOrUndefined (BundleDescription)
  , "IconUrl'" :: NullOrUndefined (IconUrl)
  , "AvailablePlatforms'" :: NullOrUndefined (Platforms)
  }
derive instance newtypeBundleDetails :: Newtype BundleDetails _


-- | <p> Unique bundle identifier. </p>
newtype BundleId = BundleId String
derive instance newtypeBundleId :: Newtype BundleId _


-- | <p> A list of bundles. </p>
newtype BundleList = BundleList (Array BundleDetails)
derive instance newtypeBundleList :: Newtype BundleList _


-- | <p> Title of the download bundle. </p>
newtype BundleTitle = BundleTitle String
derive instance newtypeBundleTitle :: Newtype BundleTitle _


-- | <p> Version of the download bundle. </p>
newtype BundleVersion = BundleVersion String
derive instance newtypeBundleVersion :: Newtype BundleVersion _


newtype ConsoleUrl = ConsoleUrl String
derive instance newtypeConsoleUrl :: Newtype ConsoleUrl _


-- | <p> Binary file data. </p>
newtype Contents = Contents String
derive instance newtypeContents :: Newtype Contents _


-- | <p> Request structure used to request a project be created. </p>
newtype CreateProjectRequest = CreateProjectRequest 
  { "Name'" :: NullOrUndefined (ProjectName)
  , "Region'" :: NullOrUndefined (ProjectRegion)
  , "Contents'" :: NullOrUndefined (Contents)
  , "SnapshotId'" :: NullOrUndefined (SnapshotId)
  }
derive instance newtypeCreateProjectRequest :: Newtype CreateProjectRequest _


-- | <p> Result structure used in response to a request to create a project. </p>
newtype CreateProjectResult = CreateProjectResult 
  { "Details'" :: NullOrUndefined (ProjectDetails)
  }
derive instance newtypeCreateProjectResult :: Newtype CreateProjectResult _


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _


-- | <p> Request structure used to request a project be deleted. </p>
newtype DeleteProjectRequest = DeleteProjectRequest 
  { "ProjectId'" :: (ProjectId)
  }
derive instance newtypeDeleteProjectRequest :: Newtype DeleteProjectRequest _


-- | <p> Result structure used in response to request to delete a project. </p>
newtype DeleteProjectResult = DeleteProjectResult 
  { "DeletedResources'" :: NullOrUndefined (Resources)
  , "OrphanedResources'" :: NullOrUndefined (Resources)
  }
derive instance newtypeDeleteProjectResult :: Newtype DeleteProjectResult _


-- | <p> Request structure to request the details of a specific bundle. </p>
newtype DescribeBundleRequest = DescribeBundleRequest 
  { "BundleId'" :: (BundleId)
  }
derive instance newtypeDescribeBundleRequest :: Newtype DescribeBundleRequest _


-- | <p> Result structure contains the details of the bundle. </p>
newtype DescribeBundleResult = DescribeBundleResult 
  { "Details'" :: NullOrUndefined (BundleDetails)
  }
derive instance newtypeDescribeBundleResult :: Newtype DescribeBundleResult _


-- | <p> Request structure used to request details about a project. </p>
newtype DescribeProjectRequest = DescribeProjectRequest 
  { "ProjectId'" :: (ProjectId)
  , "SyncFromResources'" :: NullOrUndefined (Boolean)
  }
derive instance newtypeDescribeProjectRequest :: Newtype DescribeProjectRequest _


-- | <p> Result structure used for requests of project details. </p>
newtype DescribeProjectResult = DescribeProjectResult 
  { "Details'" :: NullOrUndefined (ProjectDetails)
  }
derive instance newtypeDescribeProjectResult :: Newtype DescribeProjectResult _


-- | <p> The download Url. </p>
newtype DownloadUrl = DownloadUrl String
derive instance newtypeDownloadUrl :: Newtype DownloadUrl _


-- | <p> The Exception Error Message. </p>
newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


-- | <p> Request structure used to request generation of custom SDK and tool packages required to integrate mobile web or app clients with backed AWS resources. </p>
newtype ExportBundleRequest = ExportBundleRequest 
  { "BundleId'" :: (BundleId)
  , "ProjectId'" :: NullOrUndefined (ProjectId)
  , "Platform'" :: NullOrUndefined (Platform)
  }
derive instance newtypeExportBundleRequest :: Newtype ExportBundleRequest _


-- | <p> Result structure which contains link to download custom-generated SDK and tool packages used to integrate mobile web or app clients with backed AWS resources. </p>
newtype ExportBundleResult = ExportBundleResult 
  { "DownloadUrl'" :: NullOrUndefined (DownloadUrl)
  }
derive instance newtypeExportBundleResult :: Newtype ExportBundleResult _


-- | <p> Request structure used in requests to export project configuration details. </p>
newtype ExportProjectRequest = ExportProjectRequest 
  { "ProjectId'" :: (ProjectId)
  }
derive instance newtypeExportProjectRequest :: Newtype ExportProjectRequest _


-- | <p> Result structure used for requests to export project configuration details. </p>
newtype ExportProjectResult = ExportProjectResult 
  { "DownloadUrl'" :: NullOrUndefined (DownloadUrl)
  , "ShareUrl'" :: NullOrUndefined (ShareUrl)
  , "SnapshotId'" :: NullOrUndefined (SnapshotId)
  }
derive instance newtypeExportProjectResult :: Newtype ExportProjectResult _


-- | <p> Identifies which feature in AWS Mobile Hub is associated with this AWS resource. </p>
newtype Feature = Feature String
derive instance newtypeFeature :: Newtype Feature _


-- | <p> Icon for the download bundle. </p>
newtype IconUrl = IconUrl String
derive instance newtypeIconUrl :: Newtype IconUrl _


-- | <p> The service has encountered an unexpected error condition which prevents it from servicing the request. </p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _


-- | <p> There are too many AWS Mobile Hub projects in the account or the account has exceeded the maximum number of resources in some AWS service. You should create another sub-account using AWS Organizations or remove some resources and retry your request. </p>
newtype LimitExceededException = LimitExceededException 
  { "RetryAfterSeconds'" :: NullOrUndefined (ErrorMessage)
  , "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


-- | <p> Request structure to request all available bundles. </p>
newtype ListBundlesRequest = ListBundlesRequest 
  { "MaxResults'" :: NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListBundlesRequest :: Newtype ListBundlesRequest _


-- | <p> Result structure contains a list of all available bundles with details. </p>
newtype ListBundlesResult = ListBundlesResult 
  { "BundleList'" :: NullOrUndefined (BundleList)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListBundlesResult :: Newtype ListBundlesResult _


-- | <p> Request structure used to request projects list in AWS Mobile Hub. </p>
newtype ListProjectsRequest = ListProjectsRequest 
  { "MaxResults'" :: NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListProjectsRequest :: Newtype ListProjectsRequest _


-- | <p> Result structure used for requests to list projects in AWS Mobile Hub. </p>
newtype ListProjectsResult = ListProjectsResult 
  { "Projects'" :: NullOrUndefined (ProjectSummaries)
  , "NextToken'" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListProjectsResult :: Newtype ListProjectsResult _


-- | <p> Maximum number of records to list in a single response. </p>
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _


-- | <p> Pagination token. Set to null to start listing records from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more entries. </p>
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


-- | <p> No entity can be found with the specified identifier. </p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _


-- | <p> Developer desktop or target mobile app or website platform. </p>
newtype Platform = Platform String
derive instance newtypePlatform :: Newtype Platform _


-- | <p> Developer desktop or mobile app or website platforms. </p>
newtype Platforms = Platforms (Array Platform)
derive instance newtypePlatforms :: Newtype Platforms _


-- | <p> Detailed information about an AWS Mobile Hub project. </p>
newtype ProjectDetails = ProjectDetails 
  { "Name'" :: NullOrUndefined (ProjectName)
  , "ProjectId'" :: NullOrUndefined (ProjectId)
  , "Region'" :: NullOrUndefined (ProjectRegion)
  , "State'" :: NullOrUndefined (ProjectState)
  , "CreatedDate'" :: NullOrUndefined (Date)
  , "LastUpdatedDate'" :: NullOrUndefined (Date)
  , "ConsoleUrl'" :: NullOrUndefined (ConsoleUrl)
  , "Resources'" :: NullOrUndefined (Resources)
  }
derive instance newtypeProjectDetails :: Newtype ProjectDetails _


-- | <p> Unique project identifier. </p>
newtype ProjectId = ProjectId String
derive instance newtypeProjectId :: Newtype ProjectId _


-- | <p> Name of the project. </p>
newtype ProjectName = ProjectName String
derive instance newtypeProjectName :: Newtype ProjectName _


-- | <p> Default region to use for AWS resource creation in the AWS Mobile Hub project. </p>
newtype ProjectRegion = ProjectRegion String
derive instance newtypeProjectRegion :: Newtype ProjectRegion _


-- | <p> Synchronization state for a project. </p>
newtype ProjectState = ProjectState String
derive instance newtypeProjectState :: Newtype ProjectState _


-- | <p> List of projects. </p>
newtype ProjectSummaries = ProjectSummaries (Array ProjectSummary)
derive instance newtypeProjectSummaries :: Newtype ProjectSummaries _


-- | <p> Summary information about an AWS Mobile Hub project. </p>
newtype ProjectSummary = ProjectSummary 
  { "Name'" :: NullOrUndefined (ProjectName)
  , "ProjectId'" :: NullOrUndefined (ProjectId)
  }
derive instance newtypeProjectSummary :: Newtype ProjectSummary _


-- | <p> Information about an instance of an AWS resource associated with a project. </p>
newtype Resource = Resource 
  { "Type'" :: NullOrUndefined (ResourceType)
  , "Name'" :: NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined (ResourceArn)
  , "Feature'" :: NullOrUndefined (Feature)
  , "Attributes'" :: NullOrUndefined (Attributes)
  }
derive instance newtypeResource :: Newtype Resource _


-- | <p> AWS resource name which uniquely identifies the resource in AWS systems. </p>
newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _


-- | <p> Name of the AWS resource (e.g., for an Amazon S3 bucket this is the name of the bucket). </p>
newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _


-- | <p> Simplified name for type of AWS resource (e.g., bucket is an Amazon S3 bucket). </p>
newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


-- | <p> List of AWS resources associated with a project. </p>
newtype Resources = Resources (Array Resource)
derive instance newtypeResources :: Newtype Resources _


-- | <p> The service is temporarily unavailable. The request should be retried after some time delay. </p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "RetryAfterSeconds'" :: NullOrUndefined (ErrorMessage)
  , "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


-- | <p> URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again. </p>
newtype ShareUrl = ShareUrl String
derive instance newtypeShareUrl :: Newtype ShareUrl _


-- | <p> Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL. </p>
newtype SnapshotId = SnapshotId String
derive instance newtypeSnapshotId :: Newtype SnapshotId _


-- | <p> Too many requests have been received for this AWS account in too short a time. The request should be retried after some time delay. </p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "RetryAfterSeconds'" :: NullOrUndefined (ErrorMessage)
  , "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _


-- | <p> Credentials of the caller are insufficient to authorize the request. </p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _


-- | <p> Request structure used for requests to update project configuration. </p>
newtype UpdateProjectRequest = UpdateProjectRequest 
  { "Contents'" :: NullOrUndefined (Contents)
  , "ProjectId'" :: (ProjectId)
  }
derive instance newtypeUpdateProjectRequest :: Newtype UpdateProjectRequest _


-- | <p> Result structure used for requests to updated project configuration. </p>
newtype UpdateProjectResult = UpdateProjectResult 
  { "Details'" :: NullOrUndefined (ProjectDetails)
  }
derive instance newtypeUpdateProjectResult :: Newtype UpdateProjectResult _
