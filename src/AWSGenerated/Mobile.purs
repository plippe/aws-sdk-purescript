

-- | <p> AWS Mobile Service provides mobile app and website developers with capabilities required to configure AWS resources and bootstrap their developer desktop projects with the necessary SDKs, constants, tools and samples to make use of those resources. </p>
module AWS.Mobile where

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

serviceName = "Mobile" :: String


-- | <p> Creates an AWS Mobile Hub project. </p>
createProject :: forall eff. CreateProjectRequest -> Aff (exception :: EXCEPTION | eff) CreateProjectResult
createProject = Request.request serviceName "createProject" 


-- | <p> Delets a project in AWS Mobile Hub. </p>
deleteProject :: forall eff. DeleteProjectRequest -> Aff (exception :: EXCEPTION | eff) DeleteProjectResult
deleteProject = Request.request serviceName "deleteProject" 


-- | <p> Get the bundle details for the requested bundle id. </p>
describeBundle :: forall eff. DescribeBundleRequest -> Aff (exception :: EXCEPTION | eff) DescribeBundleResult
describeBundle = Request.request serviceName "describeBundle" 


-- | <p> Gets details about a project in AWS Mobile Hub. </p>
describeProject :: forall eff. DescribeProjectRequest -> Aff (exception :: EXCEPTION | eff) DescribeProjectResult
describeProject = Request.request serviceName "describeProject" 


-- | <p> Generates customized software development kit (SDK) and or tool packages used to integrate mobile web or mobile app clients with backend AWS resources. </p>
exportBundle :: forall eff. ExportBundleRequest -> Aff (exception :: EXCEPTION | eff) ExportBundleResult
exportBundle = Request.request serviceName "exportBundle" 


-- | <p> Exports project configuration to a snapshot which can be downloaded and shared. Note that mobile app push credentials are encrypted in exported projects, so they can only be shared successfully within the same AWS account. </p>
exportProject :: forall eff. ExportProjectRequest -> Aff (exception :: EXCEPTION | eff) ExportProjectResult
exportProject = Request.request serviceName "exportProject" 


-- | <p> List all available bundles. </p>
listBundles :: forall eff. ListBundlesRequest -> Aff (exception :: EXCEPTION | eff) ListBundlesResult
listBundles = Request.request serviceName "listBundles" 


-- | <p> Lists projects in AWS Mobile Hub. </p>
listProjects :: forall eff. ListProjectsRequest -> Aff (exception :: EXCEPTION | eff) ListProjectsResult
listProjects = Request.request serviceName "listProjects" 


-- | <p> Update an existing project. </p>
updateProject :: forall eff. UpdateProjectRequest -> Aff (exception :: EXCEPTION | eff) UpdateProjectResult
updateProject = Request.request serviceName "updateProject" 


-- | <p> Account Action is required in order to continue the request. </p>
newtype AccountActionRequiredException = AccountActionRequiredException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeAccountActionRequiredException :: Newtype AccountActionRequiredException _
derive instance repGenericAccountActionRequiredException :: Generic AccountActionRequiredException _
instance showAccountActionRequiredException :: Show AccountActionRequiredException where
  show = genericShow
instance decodeAccountActionRequiredException :: Decode AccountActionRequiredException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccountActionRequiredException :: Encode AccountActionRequiredException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Key part of key-value attribute pairs. </p>
newtype AttributeKey = AttributeKey String
derive instance newtypeAttributeKey :: Newtype AttributeKey _
derive instance repGenericAttributeKey :: Generic AttributeKey _
instance showAttributeKey :: Show AttributeKey where
  show = genericShow
instance decodeAttributeKey :: Decode AttributeKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeKey :: Encode AttributeKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Value part of key-value attribute pairs. </p>
newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Key-value attribute pairs. </p>
newtype Attributes = Attributes (StrMap.StrMap AttributeValue)
derive instance newtypeAttributes :: Newtype Attributes _
derive instance repGenericAttributes :: Generic Attributes _
instance showAttributes :: Show Attributes where
  show = genericShow
instance decodeAttributes :: Decode Attributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributes :: Encode Attributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The request cannot be processed because some parameter is not valid or the project state prevents the operation from being performed. </p>
newtype BadRequestException = BadRequestException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeBadRequestException :: Newtype BadRequestException _
derive instance repGenericBadRequestException :: Generic BadRequestException _
instance showBadRequestException :: Show BadRequestException where
  show = genericShow
instance decodeBadRequestException :: Decode BadRequestException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBadRequestException :: Encode BadRequestException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Description of the download bundle. </p>
newtype BundleDescription = BundleDescription String
derive instance newtypeBundleDescription :: Newtype BundleDescription _
derive instance repGenericBundleDescription :: Generic BundleDescription _
instance showBundleDescription :: Show BundleDescription where
  show = genericShow
instance decodeBundleDescription :: Decode BundleDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleDescription :: Encode BundleDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The details of the bundle. </p>
newtype BundleDetails = BundleDetails 
  { "BundleId'" :: NullOrUndefined.NullOrUndefined (BundleId)
  , "Title'" :: NullOrUndefined.NullOrUndefined (BundleTitle)
  , "Version'" :: NullOrUndefined.NullOrUndefined (BundleVersion)
  , "Description'" :: NullOrUndefined.NullOrUndefined (BundleDescription)
  , "IconUrl'" :: NullOrUndefined.NullOrUndefined (IconUrl)
  , "AvailablePlatforms'" :: NullOrUndefined.NullOrUndefined (Platforms)
  }
derive instance newtypeBundleDetails :: Newtype BundleDetails _
derive instance repGenericBundleDetails :: Generic BundleDetails _
instance showBundleDetails :: Show BundleDetails where
  show = genericShow
instance decodeBundleDetails :: Decode BundleDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleDetails :: Encode BundleDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Unique bundle identifier. </p>
newtype BundleId = BundleId String
derive instance newtypeBundleId :: Newtype BundleId _
derive instance repGenericBundleId :: Generic BundleId _
instance showBundleId :: Show BundleId where
  show = genericShow
instance decodeBundleId :: Decode BundleId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleId :: Encode BundleId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> A list of bundles. </p>
newtype BundleList = BundleList (Array BundleDetails)
derive instance newtypeBundleList :: Newtype BundleList _
derive instance repGenericBundleList :: Generic BundleList _
instance showBundleList :: Show BundleList where
  show = genericShow
instance decodeBundleList :: Decode BundleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleList :: Encode BundleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Title of the download bundle. </p>
newtype BundleTitle = BundleTitle String
derive instance newtypeBundleTitle :: Newtype BundleTitle _
derive instance repGenericBundleTitle :: Generic BundleTitle _
instance showBundleTitle :: Show BundleTitle where
  show = genericShow
instance decodeBundleTitle :: Decode BundleTitle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleTitle :: Encode BundleTitle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Version of the download bundle. </p>
newtype BundleVersion = BundleVersion String
derive instance newtypeBundleVersion :: Newtype BundleVersion _
derive instance repGenericBundleVersion :: Generic BundleVersion _
instance showBundleVersion :: Show BundleVersion where
  show = genericShow
instance decodeBundleVersion :: Decode BundleVersion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleVersion :: Encode BundleVersion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConsoleUrl = ConsoleUrl String
derive instance newtypeConsoleUrl :: Newtype ConsoleUrl _
derive instance repGenericConsoleUrl :: Generic ConsoleUrl _
instance showConsoleUrl :: Show ConsoleUrl where
  show = genericShow
instance decodeConsoleUrl :: Decode ConsoleUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConsoleUrl :: Encode ConsoleUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Binary file data. </p>
newtype Contents = Contents String
derive instance newtypeContents :: Newtype Contents _
derive instance repGenericContents :: Generic Contents _
instance showContents :: Show Contents where
  show = genericShow
instance decodeContents :: Decode Contents where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeContents :: Encode Contents where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure used to request a project be created. </p>
newtype CreateProjectRequest = CreateProjectRequest 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ProjectName)
  , "Region'" :: NullOrUndefined.NullOrUndefined (ProjectRegion)
  , "Contents'" :: NullOrUndefined.NullOrUndefined (Contents)
  , "SnapshotId'" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  }
derive instance newtypeCreateProjectRequest :: Newtype CreateProjectRequest _
derive instance repGenericCreateProjectRequest :: Generic CreateProjectRequest _
instance showCreateProjectRequest :: Show CreateProjectRequest where
  show = genericShow
instance decodeCreateProjectRequest :: Decode CreateProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateProjectRequest :: Encode CreateProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure used in response to a request to create a project. </p>
newtype CreateProjectResult = CreateProjectResult 
  { "Details'" :: NullOrUndefined.NullOrUndefined (ProjectDetails)
  }
derive instance newtypeCreateProjectResult :: Newtype CreateProjectResult _
derive instance repGenericCreateProjectResult :: Generic CreateProjectResult _
instance showCreateProjectResult :: Show CreateProjectResult where
  show = genericShow
instance decodeCreateProjectResult :: Decode CreateProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateProjectResult :: Encode CreateProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Date = Date Number
derive instance newtypeDate :: Newtype Date _
derive instance repGenericDate :: Generic Date _
instance showDate :: Show Date where
  show = genericShow
instance decodeDate :: Decode Date where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDate :: Encode Date where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure used to request a project be deleted. </p>
newtype DeleteProjectRequest = DeleteProjectRequest 
  { "ProjectId'" :: (ProjectId)
  }
derive instance newtypeDeleteProjectRequest :: Newtype DeleteProjectRequest _
derive instance repGenericDeleteProjectRequest :: Generic DeleteProjectRequest _
instance showDeleteProjectRequest :: Show DeleteProjectRequest where
  show = genericShow
instance decodeDeleteProjectRequest :: Decode DeleteProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteProjectRequest :: Encode DeleteProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure used in response to request to delete a project. </p>
newtype DeleteProjectResult = DeleteProjectResult 
  { "DeletedResources'" :: NullOrUndefined.NullOrUndefined (Resources)
  , "OrphanedResources'" :: NullOrUndefined.NullOrUndefined (Resources)
  }
derive instance newtypeDeleteProjectResult :: Newtype DeleteProjectResult _
derive instance repGenericDeleteProjectResult :: Generic DeleteProjectResult _
instance showDeleteProjectResult :: Show DeleteProjectResult where
  show = genericShow
instance decodeDeleteProjectResult :: Decode DeleteProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteProjectResult :: Encode DeleteProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure to request the details of a specific bundle. </p>
newtype DescribeBundleRequest = DescribeBundleRequest 
  { "BundleId'" :: (BundleId)
  }
derive instance newtypeDescribeBundleRequest :: Newtype DescribeBundleRequest _
derive instance repGenericDescribeBundleRequest :: Generic DescribeBundleRequest _
instance showDescribeBundleRequest :: Show DescribeBundleRequest where
  show = genericShow
instance decodeDescribeBundleRequest :: Decode DescribeBundleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBundleRequest :: Encode DescribeBundleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure contains the details of the bundle. </p>
newtype DescribeBundleResult = DescribeBundleResult 
  { "Details'" :: NullOrUndefined.NullOrUndefined (BundleDetails)
  }
derive instance newtypeDescribeBundleResult :: Newtype DescribeBundleResult _
derive instance repGenericDescribeBundleResult :: Generic DescribeBundleResult _
instance showDescribeBundleResult :: Show DescribeBundleResult where
  show = genericShow
instance decodeDescribeBundleResult :: Decode DescribeBundleResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeBundleResult :: Encode DescribeBundleResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure used to request details about a project. </p>
newtype DescribeProjectRequest = DescribeProjectRequest 
  { "ProjectId'" :: (ProjectId)
  , "SyncFromResources'" :: NullOrUndefined.NullOrUndefined (Boolean)
  }
derive instance newtypeDescribeProjectRequest :: Newtype DescribeProjectRequest _
derive instance repGenericDescribeProjectRequest :: Generic DescribeProjectRequest _
instance showDescribeProjectRequest :: Show DescribeProjectRequest where
  show = genericShow
instance decodeDescribeProjectRequest :: Decode DescribeProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeProjectRequest :: Encode DescribeProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure used for requests of project details. </p>
newtype DescribeProjectResult = DescribeProjectResult 
  { "Details'" :: NullOrUndefined.NullOrUndefined (ProjectDetails)
  }
derive instance newtypeDescribeProjectResult :: Newtype DescribeProjectResult _
derive instance repGenericDescribeProjectResult :: Generic DescribeProjectResult _
instance showDescribeProjectResult :: Show DescribeProjectResult where
  show = genericShow
instance decodeDescribeProjectResult :: Decode DescribeProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeProjectResult :: Encode DescribeProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The download Url. </p>
newtype DownloadUrl = DownloadUrl String
derive instance newtypeDownloadUrl :: Newtype DownloadUrl _
derive instance repGenericDownloadUrl :: Generic DownloadUrl _
instance showDownloadUrl :: Show DownloadUrl where
  show = genericShow
instance decodeDownloadUrl :: Decode DownloadUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDownloadUrl :: Encode DownloadUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The Exception Error Message. </p>
newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _
derive instance repGenericErrorMessage :: Generic ErrorMessage _
instance showErrorMessage :: Show ErrorMessage where
  show = genericShow
instance decodeErrorMessage :: Decode ErrorMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessage :: Encode ErrorMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure used to request generation of custom SDK and tool packages required to integrate mobile web or app clients with backed AWS resources. </p>
newtype ExportBundleRequest = ExportBundleRequest 
  { "BundleId'" :: (BundleId)
  , "ProjectId'" :: NullOrUndefined.NullOrUndefined (ProjectId)
  , "Platform'" :: NullOrUndefined.NullOrUndefined (Platform)
  }
derive instance newtypeExportBundleRequest :: Newtype ExportBundleRequest _
derive instance repGenericExportBundleRequest :: Generic ExportBundleRequest _
instance showExportBundleRequest :: Show ExportBundleRequest where
  show = genericShow
instance decodeExportBundleRequest :: Decode ExportBundleRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportBundleRequest :: Encode ExportBundleRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure which contains link to download custom-generated SDK and tool packages used to integrate mobile web or app clients with backed AWS resources. </p>
newtype ExportBundleResult = ExportBundleResult 
  { "DownloadUrl'" :: NullOrUndefined.NullOrUndefined (DownloadUrl)
  }
derive instance newtypeExportBundleResult :: Newtype ExportBundleResult _
derive instance repGenericExportBundleResult :: Generic ExportBundleResult _
instance showExportBundleResult :: Show ExportBundleResult where
  show = genericShow
instance decodeExportBundleResult :: Decode ExportBundleResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportBundleResult :: Encode ExportBundleResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure used in requests to export project configuration details. </p>
newtype ExportProjectRequest = ExportProjectRequest 
  { "ProjectId'" :: (ProjectId)
  }
derive instance newtypeExportProjectRequest :: Newtype ExportProjectRequest _
derive instance repGenericExportProjectRequest :: Generic ExportProjectRequest _
instance showExportProjectRequest :: Show ExportProjectRequest where
  show = genericShow
instance decodeExportProjectRequest :: Decode ExportProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportProjectRequest :: Encode ExportProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure used for requests to export project configuration details. </p>
newtype ExportProjectResult = ExportProjectResult 
  { "DownloadUrl'" :: NullOrUndefined.NullOrUndefined (DownloadUrl)
  , "ShareUrl'" :: NullOrUndefined.NullOrUndefined (ShareUrl)
  , "SnapshotId'" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  }
derive instance newtypeExportProjectResult :: Newtype ExportProjectResult _
derive instance repGenericExportProjectResult :: Generic ExportProjectResult _
instance showExportProjectResult :: Show ExportProjectResult where
  show = genericShow
instance decodeExportProjectResult :: Decode ExportProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExportProjectResult :: Encode ExportProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Identifies which feature in AWS Mobile Hub is associated with this AWS resource. </p>
newtype Feature = Feature String
derive instance newtypeFeature :: Newtype Feature _
derive instance repGenericFeature :: Generic Feature _
instance showFeature :: Show Feature where
  show = genericShow
instance decodeFeature :: Decode Feature where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFeature :: Encode Feature where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Icon for the download bundle. </p>
newtype IconUrl = IconUrl String
derive instance newtypeIconUrl :: Newtype IconUrl _
derive instance repGenericIconUrl :: Generic IconUrl _
instance showIconUrl :: Show IconUrl where
  show = genericShow
instance decodeIconUrl :: Decode IconUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIconUrl :: Encode IconUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The service has encountered an unexpected error condition which prevents it from servicing the request. </p>
newtype InternalFailureException = InternalFailureException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInternalFailureException :: Newtype InternalFailureException _
derive instance repGenericInternalFailureException :: Generic InternalFailureException _
instance showInternalFailureException :: Show InternalFailureException where
  show = genericShow
instance decodeInternalFailureException :: Decode InternalFailureException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInternalFailureException :: Encode InternalFailureException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> There are too many AWS Mobile Hub projects in the account or the account has exceeded the maximum number of resources in some AWS service. You should create another sub-account using AWS Organizations or remove some resources and retry your request. </p>
newtype LimitExceededException = LimitExceededException 
  { "RetryAfterSeconds'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure to request all available bundles. </p>
newtype ListBundlesRequest = ListBundlesRequest 
  { "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListBundlesRequest :: Newtype ListBundlesRequest _
derive instance repGenericListBundlesRequest :: Generic ListBundlesRequest _
instance showListBundlesRequest :: Show ListBundlesRequest where
  show = genericShow
instance decodeListBundlesRequest :: Decode ListBundlesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBundlesRequest :: Encode ListBundlesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure contains a list of all available bundles with details. </p>
newtype ListBundlesResult = ListBundlesResult 
  { "BundleList'" :: NullOrUndefined.NullOrUndefined (BundleList)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListBundlesResult :: Newtype ListBundlesResult _
derive instance repGenericListBundlesResult :: Generic ListBundlesResult _
instance showListBundlesResult :: Show ListBundlesResult where
  show = genericShow
instance decodeListBundlesResult :: Decode ListBundlesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListBundlesResult :: Encode ListBundlesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure used to request projects list in AWS Mobile Hub. </p>
newtype ListProjectsRequest = ListProjectsRequest 
  { "MaxResults'" :: NullOrUndefined.NullOrUndefined (MaxResults)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListProjectsRequest :: Newtype ListProjectsRequest _
derive instance repGenericListProjectsRequest :: Generic ListProjectsRequest _
instance showListProjectsRequest :: Show ListProjectsRequest where
  show = genericShow
instance decodeListProjectsRequest :: Decode ListProjectsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProjectsRequest :: Encode ListProjectsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure used for requests to list projects in AWS Mobile Hub. </p>
newtype ListProjectsResult = ListProjectsResult 
  { "Projects'" :: NullOrUndefined.NullOrUndefined (ProjectSummaries)
  , "NextToken'" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListProjectsResult :: Newtype ListProjectsResult _
derive instance repGenericListProjectsResult :: Generic ListProjectsResult _
instance showListProjectsResult :: Show ListProjectsResult where
  show = genericShow
instance decodeListProjectsResult :: Decode ListProjectsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListProjectsResult :: Encode ListProjectsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Maximum number of records to list in a single response. </p>
newtype MaxResults = MaxResults Int
derive instance newtypeMaxResults :: Newtype MaxResults _
derive instance repGenericMaxResults :: Generic MaxResults _
instance showMaxResults :: Show MaxResults where
  show = genericShow
instance decodeMaxResults :: Decode MaxResults where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMaxResults :: Encode MaxResults where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Pagination token. Set to null to start listing records from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more entries. </p>
newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> No entity can be found with the specified identifier. </p>
newtype NotFoundException = NotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeNotFoundException :: Newtype NotFoundException _
derive instance repGenericNotFoundException :: Generic NotFoundException _
instance showNotFoundException :: Show NotFoundException where
  show = genericShow
instance decodeNotFoundException :: Decode NotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotFoundException :: Encode NotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Developer desktop or target mobile app or website platform. </p>
newtype Platform = Platform String
derive instance newtypePlatform :: Newtype Platform _
derive instance repGenericPlatform :: Generic Platform _
instance showPlatform :: Show Platform where
  show = genericShow
instance decodePlatform :: Decode Platform where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatform :: Encode Platform where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Developer desktop or mobile app or website platforms. </p>
newtype Platforms = Platforms (Array Platform)
derive instance newtypePlatforms :: Newtype Platforms _
derive instance repGenericPlatforms :: Generic Platforms _
instance showPlatforms :: Show Platforms where
  show = genericShow
instance decodePlatforms :: Decode Platforms where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePlatforms :: Encode Platforms where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Detailed information about an AWS Mobile Hub project. </p>
newtype ProjectDetails = ProjectDetails 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ProjectName)
  , "ProjectId'" :: NullOrUndefined.NullOrUndefined (ProjectId)
  , "Region'" :: NullOrUndefined.NullOrUndefined (ProjectRegion)
  , "State'" :: NullOrUndefined.NullOrUndefined (ProjectState)
  , "CreatedDate'" :: NullOrUndefined.NullOrUndefined (Date)
  , "LastUpdatedDate'" :: NullOrUndefined.NullOrUndefined (Date)
  , "ConsoleUrl'" :: NullOrUndefined.NullOrUndefined (ConsoleUrl)
  , "Resources'" :: NullOrUndefined.NullOrUndefined (Resources)
  }
derive instance newtypeProjectDetails :: Newtype ProjectDetails _
derive instance repGenericProjectDetails :: Generic ProjectDetails _
instance showProjectDetails :: Show ProjectDetails where
  show = genericShow
instance decodeProjectDetails :: Decode ProjectDetails where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectDetails :: Encode ProjectDetails where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Unique project identifier. </p>
newtype ProjectId = ProjectId String
derive instance newtypeProjectId :: Newtype ProjectId _
derive instance repGenericProjectId :: Generic ProjectId _
instance showProjectId :: Show ProjectId where
  show = genericShow
instance decodeProjectId :: Decode ProjectId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectId :: Encode ProjectId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Name of the project. </p>
newtype ProjectName = ProjectName String
derive instance newtypeProjectName :: Newtype ProjectName _
derive instance repGenericProjectName :: Generic ProjectName _
instance showProjectName :: Show ProjectName where
  show = genericShow
instance decodeProjectName :: Decode ProjectName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectName :: Encode ProjectName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Default region to use for AWS resource creation in the AWS Mobile Hub project. </p>
newtype ProjectRegion = ProjectRegion String
derive instance newtypeProjectRegion :: Newtype ProjectRegion _
derive instance repGenericProjectRegion :: Generic ProjectRegion _
instance showProjectRegion :: Show ProjectRegion where
  show = genericShow
instance decodeProjectRegion :: Decode ProjectRegion where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectRegion :: Encode ProjectRegion where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Synchronization state for a project. </p>
newtype ProjectState = ProjectState String
derive instance newtypeProjectState :: Newtype ProjectState _
derive instance repGenericProjectState :: Generic ProjectState _
instance showProjectState :: Show ProjectState where
  show = genericShow
instance decodeProjectState :: Decode ProjectState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectState :: Encode ProjectState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> List of projects. </p>
newtype ProjectSummaries = ProjectSummaries (Array ProjectSummary)
derive instance newtypeProjectSummaries :: Newtype ProjectSummaries _
derive instance repGenericProjectSummaries :: Generic ProjectSummaries _
instance showProjectSummaries :: Show ProjectSummaries where
  show = genericShow
instance decodeProjectSummaries :: Decode ProjectSummaries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectSummaries :: Encode ProjectSummaries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Summary information about an AWS Mobile Hub project. </p>
newtype ProjectSummary = ProjectSummary 
  { "Name'" :: NullOrUndefined.NullOrUndefined (ProjectName)
  , "ProjectId'" :: NullOrUndefined.NullOrUndefined (ProjectId)
  }
derive instance newtypeProjectSummary :: Newtype ProjectSummary _
derive instance repGenericProjectSummary :: Generic ProjectSummary _
instance showProjectSummary :: Show ProjectSummary where
  show = genericShow
instance decodeProjectSummary :: Decode ProjectSummary where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProjectSummary :: Encode ProjectSummary where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Information about an instance of an AWS resource associated with a project. </p>
newtype Resource = Resource 
  { "Type'" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "Name'" :: NullOrUndefined.NullOrUndefined (ResourceName)
  , "Arn'" :: NullOrUndefined.NullOrUndefined (ResourceArn)
  , "Feature'" :: NullOrUndefined.NullOrUndefined (Feature)
  , "Attributes'" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypeResource :: Newtype Resource _
derive instance repGenericResource :: Generic Resource _
instance showResource :: Show Resource where
  show = genericShow
instance decodeResource :: Decode Resource where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResource :: Encode Resource where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> AWS resource name which uniquely identifies the resource in AWS systems. </p>
newtype ResourceArn = ResourceArn String
derive instance newtypeResourceArn :: Newtype ResourceArn _
derive instance repGenericResourceArn :: Generic ResourceArn _
instance showResourceArn :: Show ResourceArn where
  show = genericShow
instance decodeResourceArn :: Decode ResourceArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceArn :: Encode ResourceArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Name of the AWS resource (e.g., for an Amazon S3 bucket this is the name of the bucket). </p>
newtype ResourceName = ResourceName String
derive instance newtypeResourceName :: Newtype ResourceName _
derive instance repGenericResourceName :: Generic ResourceName _
instance showResourceName :: Show ResourceName where
  show = genericShow
instance decodeResourceName :: Decode ResourceName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceName :: Encode ResourceName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Simplified name for type of AWS resource (e.g., bucket is an Amazon S3 bucket). </p>
newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _
derive instance repGenericResourceType :: Generic ResourceType _
instance showResourceType :: Show ResourceType where
  show = genericShow
instance decodeResourceType :: Decode ResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceType :: Encode ResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> List of AWS resources associated with a project. </p>
newtype Resources = Resources (Array Resource)
derive instance newtypeResources :: Newtype Resources _
derive instance repGenericResources :: Generic Resources _
instance showResources :: Show Resources where
  show = genericShow
instance decodeResources :: Decode Resources where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResources :: Encode Resources where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> The service is temporarily unavailable. The request should be retried after some time delay. </p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "RetryAfterSeconds'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again. </p>
newtype ShareUrl = ShareUrl String
derive instance newtypeShareUrl :: Newtype ShareUrl _
derive instance repGenericShareUrl :: Generic ShareUrl _
instance showShareUrl :: Show ShareUrl where
  show = genericShow
instance decodeShareUrl :: Decode ShareUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShareUrl :: Encode ShareUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL. </p>
newtype SnapshotId = SnapshotId String
derive instance newtypeSnapshotId :: Newtype SnapshotId _
derive instance repGenericSnapshotId :: Generic SnapshotId _
instance showSnapshotId :: Show SnapshotId where
  show = genericShow
instance decodeSnapshotId :: Decode SnapshotId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotId :: Encode SnapshotId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Too many requests have been received for this AWS account in too short a time. The request should be retried after some time delay. </p>
newtype TooManyRequestsException = TooManyRequestsException 
  { "RetryAfterSeconds'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  , "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeTooManyRequestsException :: Newtype TooManyRequestsException _
derive instance repGenericTooManyRequestsException :: Generic TooManyRequestsException _
instance showTooManyRequestsException :: Show TooManyRequestsException where
  show = genericShow
instance decodeTooManyRequestsException :: Decode TooManyRequestsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyRequestsException :: Encode TooManyRequestsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Credentials of the caller are insufficient to authorize the request. </p>
newtype UnauthorizedException = UnauthorizedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ErrorMessage)
  }
derive instance newtypeUnauthorizedException :: Newtype UnauthorizedException _
derive instance repGenericUnauthorizedException :: Generic UnauthorizedException _
instance showUnauthorizedException :: Show UnauthorizedException where
  show = genericShow
instance decodeUnauthorizedException :: Decode UnauthorizedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedException :: Encode UnauthorizedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Request structure used for requests to update project configuration. </p>
newtype UpdateProjectRequest = UpdateProjectRequest 
  { "Contents'" :: NullOrUndefined.NullOrUndefined (Contents)
  , "ProjectId'" :: (ProjectId)
  }
derive instance newtypeUpdateProjectRequest :: Newtype UpdateProjectRequest _
derive instance repGenericUpdateProjectRequest :: Generic UpdateProjectRequest _
instance showUpdateProjectRequest :: Show UpdateProjectRequest where
  show = genericShow
instance decodeUpdateProjectRequest :: Decode UpdateProjectRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateProjectRequest :: Encode UpdateProjectRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p> Result structure used for requests to updated project configuration. </p>
newtype UpdateProjectResult = UpdateProjectResult 
  { "Details'" :: NullOrUndefined.NullOrUndefined (ProjectDetails)
  }
derive instance newtypeUpdateProjectResult :: Newtype UpdateProjectResult _
derive instance repGenericUpdateProjectResult :: Generic UpdateProjectResult _
instance showUpdateProjectResult :: Show UpdateProjectResult where
  show = genericShow
instance decodeUpdateProjectResult :: Decode UpdateProjectResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateProjectResult :: Encode UpdateProjectResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
