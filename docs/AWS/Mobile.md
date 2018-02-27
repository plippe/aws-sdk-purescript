## Module AWS.Mobile

<p> AWS Mobile Service provides mobile app and website developers with capabilities required to configure AWS resources and bootstrap their developer desktop projects with the necessary SDKs, constants, tools and samples to make use of those resources. </p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createProject`

``` purescript
createProject :: forall eff. CreateProjectRequest -> Aff (err :: RequestError | eff) CreateProjectResult
```

<p> Creates an AWS Mobile Hub project. </p>

#### `deleteProject`

``` purescript
deleteProject :: forall eff. DeleteProjectRequest -> Aff (err :: RequestError | eff) DeleteProjectResult
```

<p> Delets a project in AWS Mobile Hub. </p>

#### `describeBundle`

``` purescript
describeBundle :: forall eff. DescribeBundleRequest -> Aff (err :: RequestError | eff) DescribeBundleResult
```

<p> Get the bundle details for the requested bundle id. </p>

#### `describeProject`

``` purescript
describeProject :: forall eff. DescribeProjectRequest -> Aff (err :: RequestError | eff) DescribeProjectResult
```

<p> Gets details about a project in AWS Mobile Hub. </p>

#### `exportBundle`

``` purescript
exportBundle :: forall eff. ExportBundleRequest -> Aff (err :: RequestError | eff) ExportBundleResult
```

<p> Generates customized software development kit (SDK) and or tool packages used to integrate mobile web or mobile app clients with backend AWS resources. </p>

#### `exportProject`

``` purescript
exportProject :: forall eff. ExportProjectRequest -> Aff (err :: RequestError | eff) ExportProjectResult
```

<p> Exports project configuration to a snapshot which can be downloaded and shared. Note that mobile app push credentials are encrypted in exported projects, so they can only be shared successfully within the same AWS account. </p>

#### `listBundles`

``` purescript
listBundles :: forall eff. ListBundlesRequest -> Aff (err :: RequestError | eff) ListBundlesResult
```

<p> List all available bundles. </p>

#### `listProjects`

``` purescript
listProjects :: forall eff. ListProjectsRequest -> Aff (err :: RequestError | eff) ListProjectsResult
```

<p> Lists projects in AWS Mobile Hub. </p>

#### `updateProject`

``` purescript
updateProject :: forall eff. UpdateProjectRequest -> Aff (err :: RequestError | eff) UpdateProjectResult
```

<p> Update an existing project. </p>

#### `AccountActionRequiredException`

``` purescript
newtype AccountActionRequiredException
  = AccountActionRequiredException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> Account Action is required in order to continue the request. </p>

##### Instances
``` purescript
Newtype AccountActionRequiredException _
```

#### `AttributeKey`

``` purescript
newtype AttributeKey
  = AttributeKey String
```

<p> Key part of key-value attribute pairs. </p>

##### Instances
``` purescript
Newtype AttributeKey _
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

<p> Value part of key-value attribute pairs. </p>

##### Instances
``` purescript
Newtype AttributeValue _
```

#### `Attributes`

``` purescript
newtype Attributes
  = Attributes (Map AttributeKey AttributeValue)
```

<p> Key-value attribute pairs. </p>

##### Instances
``` purescript
Newtype Attributes _
```

#### `BadRequestException`

``` purescript
newtype BadRequestException
  = BadRequestException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> The request cannot be processed because some parameter is not valid or the project state prevents the operation from being performed. </p>

##### Instances
``` purescript
Newtype BadRequestException _
```

#### `BundleDescription`

``` purescript
newtype BundleDescription
  = BundleDescription String
```

<p> Description of the download bundle. </p>

##### Instances
``` purescript
Newtype BundleDescription _
```

#### `BundleDetails`

``` purescript
newtype BundleDetails
  = BundleDetails { "BundleId'" :: NullOrUndefined (BundleId), "Title'" :: NullOrUndefined (BundleTitle), "Version'" :: NullOrUndefined (BundleVersion), "Description'" :: NullOrUndefined (BundleDescription), "IconUrl'" :: NullOrUndefined (IconUrl), "AvailablePlatforms'" :: NullOrUndefined (Platforms) }
```

<p> The details of the bundle. </p>

##### Instances
``` purescript
Newtype BundleDetails _
```

#### `BundleId`

``` purescript
newtype BundleId
  = BundleId String
```

<p> Unique bundle identifier. </p>

##### Instances
``` purescript
Newtype BundleId _
```

#### `BundleList`

``` purescript
newtype BundleList
  = BundleList (Array BundleDetails)
```

<p> A list of bundles. </p>

##### Instances
``` purescript
Newtype BundleList _
```

#### `BundleTitle`

``` purescript
newtype BundleTitle
  = BundleTitle String
```

<p> Title of the download bundle. </p>

##### Instances
``` purescript
Newtype BundleTitle _
```

#### `BundleVersion`

``` purescript
newtype BundleVersion
  = BundleVersion String
```

<p> Version of the download bundle. </p>

##### Instances
``` purescript
Newtype BundleVersion _
```

#### `ConsoleUrl`

``` purescript
newtype ConsoleUrl
  = ConsoleUrl String
```

##### Instances
``` purescript
Newtype ConsoleUrl _
```

#### `Contents`

``` purescript
newtype Contents
  = Contents String
```

<p> Binary file data. </p>

##### Instances
``` purescript
Newtype Contents _
```

#### `CreateProjectRequest`

``` purescript
newtype CreateProjectRequest
  = CreateProjectRequest { "Name'" :: NullOrUndefined (ProjectName), "Region'" :: NullOrUndefined (ProjectRegion), "Contents'" :: NullOrUndefined (Contents), "SnapshotId'" :: NullOrUndefined (SnapshotId) }
```

<p> Request structure used to request a project be created. </p>

##### Instances
``` purescript
Newtype CreateProjectRequest _
```

#### `CreateProjectResult`

``` purescript
newtype CreateProjectResult
  = CreateProjectResult { "Details'" :: NullOrUndefined (ProjectDetails) }
```

<p> Result structure used in response to a request to create a project. </p>

##### Instances
``` purescript
Newtype CreateProjectResult _
```

#### `Date`

``` purescript
newtype Date
  = Date Number
```

##### Instances
``` purescript
Newtype Date _
```

#### `DeleteProjectRequest`

``` purescript
newtype DeleteProjectRequest
  = DeleteProjectRequest { "ProjectId'" :: ProjectId }
```

<p> Request structure used to request a project be deleted. </p>

##### Instances
``` purescript
Newtype DeleteProjectRequest _
```

#### `DeleteProjectResult`

``` purescript
newtype DeleteProjectResult
  = DeleteProjectResult { "DeletedResources'" :: NullOrUndefined (Resources), "OrphanedResources'" :: NullOrUndefined (Resources) }
```

<p> Result structure used in response to request to delete a project. </p>

##### Instances
``` purescript
Newtype DeleteProjectResult _
```

#### `DescribeBundleRequest`

``` purescript
newtype DescribeBundleRequest
  = DescribeBundleRequest { "BundleId'" :: BundleId }
```

<p> Request structure to request the details of a specific bundle. </p>

##### Instances
``` purescript
Newtype DescribeBundleRequest _
```

#### `DescribeBundleResult`

``` purescript
newtype DescribeBundleResult
  = DescribeBundleResult { "Details'" :: NullOrUndefined (BundleDetails) }
```

<p> Result structure contains the details of the bundle. </p>

##### Instances
``` purescript
Newtype DescribeBundleResult _
```

#### `DescribeProjectRequest`

``` purescript
newtype DescribeProjectRequest
  = DescribeProjectRequest { "ProjectId'" :: ProjectId, "SyncFromResources'" :: NullOrUndefined (Boolean) }
```

<p> Request structure used to request details about a project. </p>

##### Instances
``` purescript
Newtype DescribeProjectRequest _
```

#### `DescribeProjectResult`

``` purescript
newtype DescribeProjectResult
  = DescribeProjectResult { "Details'" :: NullOrUndefined (ProjectDetails) }
```

<p> Result structure used for requests of project details. </p>

##### Instances
``` purescript
Newtype DescribeProjectResult _
```

#### `DownloadUrl`

``` purescript
newtype DownloadUrl
  = DownloadUrl String
```

<p> The download Url. </p>

##### Instances
``` purescript
Newtype DownloadUrl _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

<p> The Exception Error Message. </p>

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `ExportBundleRequest`

``` purescript
newtype ExportBundleRequest
  = ExportBundleRequest { "BundleId'" :: BundleId, "ProjectId'" :: NullOrUndefined (ProjectId), "Platform'" :: NullOrUndefined (Platform) }
```

<p> Request structure used to request generation of custom SDK and tool packages required to integrate mobile web or app clients with backed AWS resources. </p>

##### Instances
``` purescript
Newtype ExportBundleRequest _
```

#### `ExportBundleResult`

``` purescript
newtype ExportBundleResult
  = ExportBundleResult { "DownloadUrl'" :: NullOrUndefined (DownloadUrl) }
```

<p> Result structure which contains link to download custom-generated SDK and tool packages used to integrate mobile web or app clients with backed AWS resources. </p>

##### Instances
``` purescript
Newtype ExportBundleResult _
```

#### `ExportProjectRequest`

``` purescript
newtype ExportProjectRequest
  = ExportProjectRequest { "ProjectId'" :: ProjectId }
```

<p> Request structure used in requests to export project configuration details. </p>

##### Instances
``` purescript
Newtype ExportProjectRequest _
```

#### `ExportProjectResult`

``` purescript
newtype ExportProjectResult
  = ExportProjectResult { "DownloadUrl'" :: NullOrUndefined (DownloadUrl), "ShareUrl'" :: NullOrUndefined (ShareUrl), "SnapshotId'" :: NullOrUndefined (SnapshotId) }
```

<p> Result structure used for requests to export project configuration details. </p>

##### Instances
``` purescript
Newtype ExportProjectResult _
```

#### `Feature`

``` purescript
newtype Feature
  = Feature String
```

<p> Identifies which feature in AWS Mobile Hub is associated with this AWS resource. </p>

##### Instances
``` purescript
Newtype Feature _
```

#### `IconUrl`

``` purescript
newtype IconUrl
  = IconUrl String
```

<p> Icon for the download bundle. </p>

##### Instances
``` purescript
Newtype IconUrl _
```

#### `InternalFailureException`

``` purescript
newtype InternalFailureException
  = InternalFailureException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> The service has encountered an unexpected error condition which prevents it from servicing the request. </p>

##### Instances
``` purescript
Newtype InternalFailureException _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "RetryAfterSeconds'" :: NullOrUndefined (ErrorMessage), "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> There are too many AWS Mobile Hub projects in the account or the account has exceeded the maximum number of resources in some AWS service. You should create another sub-account using AWS Organizations or remove some resources and retry your request. </p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListBundlesRequest`

``` purescript
newtype ListBundlesRequest
  = ListBundlesRequest { "MaxResults'" :: NullOrUndefined (MaxResults), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p> Request structure to request all available bundles. </p>

##### Instances
``` purescript
Newtype ListBundlesRequest _
```

#### `ListBundlesResult`

``` purescript
newtype ListBundlesResult
  = ListBundlesResult { "BundleList'" :: NullOrUndefined (BundleList), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p> Result structure contains a list of all available bundles with details. </p>

##### Instances
``` purescript
Newtype ListBundlesResult _
```

#### `ListProjectsRequest`

``` purescript
newtype ListProjectsRequest
  = ListProjectsRequest { "MaxResults'" :: NullOrUndefined (MaxResults), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p> Request structure used to request projects list in AWS Mobile Hub. </p>

##### Instances
``` purescript
Newtype ListProjectsRequest _
```

#### `ListProjectsResult`

``` purescript
newtype ListProjectsResult
  = ListProjectsResult { "Projects'" :: NullOrUndefined (ProjectSummaries), "NextToken'" :: NullOrUndefined (NextToken) }
```

<p> Result structure used for requests to list projects in AWS Mobile Hub. </p>

##### Instances
``` purescript
Newtype ListProjectsResult _
```

#### `MaxResults`

``` purescript
newtype MaxResults
  = MaxResults Int
```

<p> Maximum number of records to list in a single response. </p>

##### Instances
``` purescript
Newtype MaxResults _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

<p> Pagination token. Set to null to start listing records from start. If non-null pagination token is returned in a result, then pass its value in here in another request to list more entries. </p>

##### Instances
``` purescript
Newtype NextToken _
```

#### `NotFoundException`

``` purescript
newtype NotFoundException
  = NotFoundException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> No entity can be found with the specified identifier. </p>

##### Instances
``` purescript
Newtype NotFoundException _
```

#### `Platform`

``` purescript
newtype Platform
  = Platform String
```

<p> Developer desktop or target mobile app or website platform. </p>

##### Instances
``` purescript
Newtype Platform _
```

#### `Platforms`

``` purescript
newtype Platforms
  = Platforms (Array Platform)
```

<p> Developer desktop or mobile app or website platforms. </p>

##### Instances
``` purescript
Newtype Platforms _
```

#### `ProjectDetails`

``` purescript
newtype ProjectDetails
  = ProjectDetails { "Name'" :: NullOrUndefined (ProjectName), "ProjectId'" :: NullOrUndefined (ProjectId), "Region'" :: NullOrUndefined (ProjectRegion), "State'" :: NullOrUndefined (ProjectState), "CreatedDate'" :: NullOrUndefined (Date), "LastUpdatedDate'" :: NullOrUndefined (Date), "ConsoleUrl'" :: NullOrUndefined (ConsoleUrl), "Resources'" :: NullOrUndefined (Resources) }
```

<p> Detailed information about an AWS Mobile Hub project. </p>

##### Instances
``` purescript
Newtype ProjectDetails _
```

#### `ProjectId`

``` purescript
newtype ProjectId
  = ProjectId String
```

<p> Unique project identifier. </p>

##### Instances
``` purescript
Newtype ProjectId _
```

#### `ProjectName`

``` purescript
newtype ProjectName
  = ProjectName String
```

<p> Name of the project. </p>

##### Instances
``` purescript
Newtype ProjectName _
```

#### `ProjectRegion`

``` purescript
newtype ProjectRegion
  = ProjectRegion String
```

<p> Default region to use for AWS resource creation in the AWS Mobile Hub project. </p>

##### Instances
``` purescript
Newtype ProjectRegion _
```

#### `ProjectState`

``` purescript
newtype ProjectState
  = ProjectState String
```

<p> Synchronization state for a project. </p>

##### Instances
``` purescript
Newtype ProjectState _
```

#### `ProjectSummaries`

``` purescript
newtype ProjectSummaries
  = ProjectSummaries (Array ProjectSummary)
```

<p> List of projects. </p>

##### Instances
``` purescript
Newtype ProjectSummaries _
```

#### `ProjectSummary`

``` purescript
newtype ProjectSummary
  = ProjectSummary { "Name'" :: NullOrUndefined (ProjectName), "ProjectId'" :: NullOrUndefined (ProjectId) }
```

<p> Summary information about an AWS Mobile Hub project. </p>

##### Instances
``` purescript
Newtype ProjectSummary _
```

#### `Resource`

``` purescript
newtype Resource
  = Resource { "Type'" :: NullOrUndefined (ResourceType), "Name'" :: NullOrUndefined (ResourceName), "Arn'" :: NullOrUndefined (ResourceArn), "Feature'" :: NullOrUndefined (Feature), "Attributes'" :: NullOrUndefined (Attributes) }
```

<p> Information about an instance of an AWS resource associated with a project. </p>

##### Instances
``` purescript
Newtype Resource _
```

#### `ResourceArn`

``` purescript
newtype ResourceArn
  = ResourceArn String
```

<p> AWS resource name which uniquely identifies the resource in AWS systems. </p>

##### Instances
``` purescript
Newtype ResourceArn _
```

#### `ResourceName`

``` purescript
newtype ResourceName
  = ResourceName String
```

<p> Name of the AWS resource (e.g., for an Amazon S3 bucket this is the name of the bucket). </p>

##### Instances
``` purescript
Newtype ResourceName _
```

#### `ResourceType`

``` purescript
newtype ResourceType
  = ResourceType String
```

<p> Simplified name for type of AWS resource (e.g., bucket is an Amazon S3 bucket). </p>

##### Instances
``` purescript
Newtype ResourceType _
```

#### `Resources`

``` purescript
newtype Resources
  = Resources (Array Resource)
```

<p> List of AWS resources associated with a project. </p>

##### Instances
``` purescript
Newtype Resources _
```

#### `ServiceUnavailableException`

``` purescript
newtype ServiceUnavailableException
  = ServiceUnavailableException { "RetryAfterSeconds'" :: NullOrUndefined (ErrorMessage), "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> The service is temporarily unavailable. The request should be retried after some time delay. </p>

##### Instances
``` purescript
Newtype ServiceUnavailableException _
```

#### `ShareUrl`

``` purescript
newtype ShareUrl
  = ShareUrl String
```

<p> URL which can be shared to allow other AWS users to create their own project in AWS Mobile Hub with the same configuration as the specified project. This URL pertains to a snapshot in time of the project configuration that is created when this API is called. If you want to share additional changes to your project configuration, then you will need to create and share a new snapshot by calling this method again. </p>

##### Instances
``` purescript
Newtype ShareUrl _
```

#### `SnapshotId`

``` purescript
newtype SnapshotId
  = SnapshotId String
```

<p> Unique identifier for the exported snapshot of the project configuration. This snapshot identifier is included in the share URL. </p>

##### Instances
``` purescript
Newtype SnapshotId _
```

#### `TooManyRequestsException`

``` purescript
newtype TooManyRequestsException
  = TooManyRequestsException { "RetryAfterSeconds'" :: NullOrUndefined (ErrorMessage), "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> Too many requests have been received for this AWS account in too short a time. The request should be retried after some time delay. </p>

##### Instances
``` purescript
Newtype TooManyRequestsException _
```

#### `UnauthorizedException`

``` purescript
newtype UnauthorizedException
  = UnauthorizedException { "Message'" :: NullOrUndefined (ErrorMessage) }
```

<p> Credentials of the caller are insufficient to authorize the request. </p>

##### Instances
``` purescript
Newtype UnauthorizedException _
```

#### `UpdateProjectRequest`

``` purescript
newtype UpdateProjectRequest
  = UpdateProjectRequest { "Contents'" :: NullOrUndefined (Contents), "ProjectId'" :: ProjectId }
```

<p> Request structure used for requests to update project configuration. </p>

##### Instances
``` purescript
Newtype UpdateProjectRequest _
```

#### `UpdateProjectResult`

``` purescript
newtype UpdateProjectResult
  = UpdateProjectResult { "Details'" :: NullOrUndefined (ProjectDetails) }
```

<p> Result structure used for requests to updated project configuration. </p>

##### Instances
``` purescript
Newtype UpdateProjectResult _
```


