## Module AWS.AppStream

<fullname>Amazon AppStream 2.0</fullname> <p>You can use Amazon AppStream 2.0 to stream desktop applications to any device running a web browser, without rewriting them.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `associateFleet`

``` purescript
associateFleet :: forall eff. AssociateFleetRequest -> Aff (err :: RequestError | eff) AssociateFleetResult
```

<p>Associates the specified fleet with the specified stack.</p>

#### `copyImage`

``` purescript
copyImage :: forall eff. CopyImageRequest -> Aff (err :: RequestError | eff) CopyImageResponse
```

<p>Copies the image within the same region or to a new region within the same AWS account. Note that any tags you added to the image will not be copied.</p>

#### `createDirectoryConfig`

``` purescript
createDirectoryConfig :: forall eff. CreateDirectoryConfigRequest -> Aff (err :: RequestError | eff) CreateDirectoryConfigResult
```

<p>Creates a directory configuration.</p>

#### `createFleet`

``` purescript
createFleet :: forall eff. CreateFleetRequest -> Aff (err :: RequestError | eff) CreateFleetResult
```

<p>Creates a fleet.</p>

#### `createImageBuilder`

``` purescript
createImageBuilder :: forall eff. CreateImageBuilderRequest -> Aff (err :: RequestError | eff) CreateImageBuilderResult
```

<p>Creates an image builder.</p> <p>The initial state of the builder is <code>PENDING</code>. When it is ready, the state is <code>RUNNING</code>.</p>

#### `createImageBuilderStreamingURL`

``` purescript
createImageBuilderStreamingURL :: forall eff. CreateImageBuilderStreamingURLRequest -> Aff (err :: RequestError | eff) CreateImageBuilderStreamingURLResult
```

<p>Creates a URL to start an image builder streaming session.</p>

#### `createStack`

``` purescript
createStack :: forall eff. CreateStackRequest -> Aff (err :: RequestError | eff) CreateStackResult
```

<p>Creates a stack.</p>

#### `createStreamingURL`

``` purescript
createStreamingURL :: forall eff. CreateStreamingURLRequest -> Aff (err :: RequestError | eff) CreateStreamingURLResult
```

<p>Creates a URL to start a streaming session for the specified user.</p>

#### `deleteDirectoryConfig`

``` purescript
deleteDirectoryConfig :: forall eff. DeleteDirectoryConfigRequest -> Aff (err :: RequestError | eff) DeleteDirectoryConfigResult
```

<p>Deletes the specified directory configuration.</p>

#### `deleteFleet`

``` purescript
deleteFleet :: forall eff. DeleteFleetRequest -> Aff (err :: RequestError | eff) DeleteFleetResult
```

<p>Deletes the specified fleet.</p>

#### `deleteImage`

``` purescript
deleteImage :: forall eff. DeleteImageRequest -> Aff (err :: RequestError | eff) DeleteImageResult
```

<p>Deletes the specified image. You cannot delete an image that is currently in use. After you delete an image, you cannot provision new capacity using the image.</p>

#### `deleteImageBuilder`

``` purescript
deleteImageBuilder :: forall eff. DeleteImageBuilderRequest -> Aff (err :: RequestError | eff) DeleteImageBuilderResult
```

<p>Deletes the specified image builder and releases the capacity.</p>

#### `deleteStack`

``` purescript
deleteStack :: forall eff. DeleteStackRequest -> Aff (err :: RequestError | eff) DeleteStackResult
```

<p>Deletes the specified stack. After this operation completes, the environment can no longer be activated and any reservations made for the stack are released.</p>

#### `describeDirectoryConfigs`

``` purescript
describeDirectoryConfigs :: forall eff. DescribeDirectoryConfigsRequest -> Aff (err :: RequestError | eff) DescribeDirectoryConfigsResult
```

<p>Describes the specified directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response. </p>

#### `describeFleets`

``` purescript
describeFleets :: forall eff. DescribeFleetsRequest -> Aff (err :: RequestError | eff) DescribeFleetsResult
```

<p>Describes the specified fleets or all fleets in the account.</p>

#### `describeImageBuilders`

``` purescript
describeImageBuilders :: forall eff. DescribeImageBuildersRequest -> Aff (err :: RequestError | eff) DescribeImageBuildersResult
```

<p>Describes the specified image builders or all image builders in the account.</p>

#### `describeImages`

``` purescript
describeImages :: forall eff. DescribeImagesRequest -> Aff (err :: RequestError | eff) DescribeImagesResult
```

<p>Describes the specified images or all images in the account.</p>

#### `describeSessions`

``` purescript
describeSessions :: forall eff. DescribeSessionsRequest -> Aff (err :: RequestError | eff) DescribeSessionsResult
```

<p>Describes the streaming sessions for the specified stack and fleet. If a user ID is provided, only the streaming sessions for only that user are returned. If an authentication type is not provided, the default is to authenticate users using a streaming URL.</p>

#### `describeStacks`

``` purescript
describeStacks :: forall eff. DescribeStacksRequest -> Aff (err :: RequestError | eff) DescribeStacksResult
```

<p>Describes the specified stacks or all stacks in the account.</p>

#### `disassociateFleet`

``` purescript
disassociateFleet :: forall eff. DisassociateFleetRequest -> Aff (err :: RequestError | eff) DisassociateFleetResult
```

<p>Disassociates the specified fleet from the specified stack.</p>

#### `expireSession`

``` purescript
expireSession :: forall eff. ExpireSessionRequest -> Aff (err :: RequestError | eff) ExpireSessionResult
```

<p>Stops the specified streaming session.</p>

#### `listAssociatedFleets`

``` purescript
listAssociatedFleets :: forall eff. ListAssociatedFleetsRequest -> Aff (err :: RequestError | eff) ListAssociatedFleetsResult
```

<p>Lists the fleets associated with the specified stack.</p>

#### `listAssociatedStacks`

``` purescript
listAssociatedStacks :: forall eff. ListAssociatedStacksRequest -> Aff (err :: RequestError | eff) ListAssociatedStacksResult
```

<p>Lists the stacks associated with the specified fleet.</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: RequestError | eff) ListTagsForResourceResponse
```

<p>Lists the tags for the specified AppStream 2.0 resource. You can tag AppStream 2.0 image builders, images, fleets, and stacks.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>

#### `startFleet`

``` purescript
startFleet :: forall eff. StartFleetRequest -> Aff (err :: RequestError | eff) StartFleetResult
```

<p>Starts the specified fleet.</p>

#### `startImageBuilder`

``` purescript
startImageBuilder :: forall eff. StartImageBuilderRequest -> Aff (err :: RequestError | eff) StartImageBuilderResult
```

<p>Starts the specified image builder.</p>

#### `stopFleet`

``` purescript
stopFleet :: forall eff. StopFleetRequest -> Aff (err :: RequestError | eff) StopFleetResult
```

<p>Stops the specified fleet.</p>

#### `stopImageBuilder`

``` purescript
stopImageBuilder :: forall eff. StopImageBuilderRequest -> Aff (err :: RequestError | eff) StopImageBuilderResult
```

<p>Stops the specified image builder.</p>

#### `tagResource`

``` purescript
tagResource :: forall eff. TagResourceRequest -> Aff (err :: RequestError | eff) TagResourceResponse
```

<p>Adds or overwrites one or more tags for the specified AppStream 2.0 resource. You can tag AppStream 2.0 image builders, images, fleets, and stacks.</p> <p>Each tag consists of a key and an optional value. If a resource already has a tag with the same key, this operation updates its value.</p> <p>To list the current tags for your resources, use <a>ListTagsForResource</a>. To disassociate tags from your resources, use <a>UntagResource</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>

#### `untagResource`

``` purescript
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: RequestError | eff) UntagResourceResponse
```

<p>Disassociates the specified tags from the specified AppStream 2.0 resource.</p> <p>To list the current tags for your resources, use <a>ListTagsForResource</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>

#### `updateDirectoryConfig`

``` purescript
updateDirectoryConfig :: forall eff. UpdateDirectoryConfigRequest -> Aff (err :: RequestError | eff) UpdateDirectoryConfigResult
```

<p>Updates the specified directory configuration.</p>

#### `updateFleet`

``` purescript
updateFleet :: forall eff. UpdateFleetRequest -> Aff (err :: RequestError | eff) UpdateFleetResult
```

<p>Updates the specified fleet.</p> <p>If the fleet is in the <code>STOPPED</code> state, you can update any attribute except the fleet name. If the fleet is in the <code>RUNNING</code> state, you can update the <code>DisplayName</code> and <code>ComputeCapacity</code> attributes. If the fleet is in the <code>STARTING</code> or <code>STOPPING</code> state, you can't update it.</p>

#### `updateStack`

``` purescript
updateStack :: forall eff. UpdateStackRequest -> Aff (err :: RequestError | eff) UpdateStackResult
```

<p>Updates the specified stack.</p>

#### `AccountName`

``` purescript
newtype AccountName
  = AccountName String
```

##### Instances
``` purescript
Newtype AccountName _
```

#### `AccountPassword`

``` purescript
newtype AccountPassword
  = AccountPassword String
```

##### Instances
``` purescript
Newtype AccountPassword _
```

#### `Application`

``` purescript
newtype Application
  = Application { "Name" :: NullOrUndefined (String), "DisplayName" :: NullOrUndefined (String), "IconURL" :: NullOrUndefined (String), "LaunchPath" :: NullOrUndefined (String), "LaunchParameters" :: NullOrUndefined (String), "Enabled" :: NullOrUndefined (Boolean), "Metadata" :: NullOrUndefined (Metadata) }
```

<p>Describes an application in the application catalog.</p>

##### Instances
``` purescript
Newtype Application _
```

#### `Applications`

``` purescript
newtype Applications
  = Applications (Array Application)
```

##### Instances
``` purescript
Newtype Applications _
```

#### `AppstreamAgentVersion`

``` purescript
newtype AppstreamAgentVersion
  = AppstreamAgentVersion String
```

##### Instances
``` purescript
Newtype AppstreamAgentVersion _
```

#### `Arn`

``` purescript
newtype Arn
  = Arn String
```

##### Instances
``` purescript
Newtype Arn _
```

#### `AssociateFleetRequest`

``` purescript
newtype AssociateFleetRequest
  = AssociateFleetRequest { "FleetName" :: String, "StackName" :: String }
```

##### Instances
``` purescript
Newtype AssociateFleetRequest _
```

#### `AssociateFleetResult`

``` purescript
newtype AssociateFleetResult
  = AssociateFleetResult {  }
```

##### Instances
``` purescript
Newtype AssociateFleetResult _
```

#### `AuthenticationType`

``` purescript
newtype AuthenticationType
  = AuthenticationType String
```

##### Instances
``` purescript
Newtype AuthenticationType _
```

#### `BooleanObject`

``` purescript
newtype BooleanObject
  = BooleanObject Boolean
```

##### Instances
``` purescript
Newtype BooleanObject _
```

#### `ComputeCapacity`

``` purescript
newtype ComputeCapacity
  = ComputeCapacity { "DesiredInstances" :: Int }
```

<p>Describes the capacity for a fleet.</p>

##### Instances
``` purescript
Newtype ComputeCapacity _
```

#### `ComputeCapacityStatus`

``` purescript
newtype ComputeCapacityStatus
  = ComputeCapacityStatus { "Desired" :: Int, "Running" :: NullOrUndefined (Int), "InUse" :: NullOrUndefined (Int), "Available" :: NullOrUndefined (Int) }
```

<p>Describes the capacity status for a fleet.</p>

##### Instances
``` purescript
Newtype ComputeCapacityStatus _
```

#### `ConcurrentModificationException`

``` purescript
newtype ConcurrentModificationException
  = ConcurrentModificationException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>An API error occurred. Wait a few minutes and try again.</p>

##### Instances
``` purescript
Newtype ConcurrentModificationException _
```

#### `CopyImageRequest`

``` purescript
newtype CopyImageRequest
  = CopyImageRequest { "SourceImageName" :: Name, "DestinationImageName" :: Name, "DestinationRegion" :: RegionName, "DestinationImageDescription" :: NullOrUndefined (Description) }
```

##### Instances
``` purescript
Newtype CopyImageRequest _
```

#### `CopyImageResponse`

``` purescript
newtype CopyImageResponse
  = CopyImageResponse { "DestinationImageName" :: NullOrUndefined (Name) }
```

##### Instances
``` purescript
Newtype CopyImageResponse _
```

#### `CreateDirectoryConfigRequest`

``` purescript
newtype CreateDirectoryConfigRequest
  = CreateDirectoryConfigRequest { "DirectoryName" :: DirectoryName, "OrganizationalUnitDistinguishedNames" :: OrganizationalUnitDistinguishedNamesList, "ServiceAccountCredentials" :: ServiceAccountCredentials }
```

##### Instances
``` purescript
Newtype CreateDirectoryConfigRequest _
```

#### `CreateDirectoryConfigResult`

``` purescript
newtype CreateDirectoryConfigResult
  = CreateDirectoryConfigResult { "DirectoryConfig" :: NullOrUndefined (DirectoryConfig) }
```

##### Instances
``` purescript
Newtype CreateDirectoryConfigResult _
```

#### `CreateFleetRequest`

``` purescript
newtype CreateFleetRequest
  = CreateFleetRequest { "Name" :: Name, "ImageName" :: String, "InstanceType" :: String, "FleetType" :: NullOrUndefined (FleetType), "ComputeCapacity" :: ComputeCapacity, "VpcConfig" :: NullOrUndefined (VpcConfig), "MaxUserDurationInSeconds" :: NullOrUndefined (Int), "DisconnectTimeoutInSeconds" :: NullOrUndefined (Int), "Description" :: NullOrUndefined (Description), "DisplayName" :: NullOrUndefined (DisplayName), "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject), "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo) }
```

##### Instances
``` purescript
Newtype CreateFleetRequest _
```

#### `CreateFleetResult`

``` purescript
newtype CreateFleetResult
  = CreateFleetResult { "Fleet" :: NullOrUndefined (Fleet) }
```

##### Instances
``` purescript
Newtype CreateFleetResult _
```

#### `CreateImageBuilderRequest`

``` purescript
newtype CreateImageBuilderRequest
  = CreateImageBuilderRequest { "Name" :: Name, "ImageName" :: String, "InstanceType" :: String, "Description" :: NullOrUndefined (Description), "DisplayName" :: NullOrUndefined (DisplayName), "VpcConfig" :: NullOrUndefined (VpcConfig), "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject), "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo), "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion) }
```

##### Instances
``` purescript
Newtype CreateImageBuilderRequest _
```

#### `CreateImageBuilderResult`

``` purescript
newtype CreateImageBuilderResult
  = CreateImageBuilderResult { "ImageBuilder" :: NullOrUndefined (ImageBuilder) }
```

##### Instances
``` purescript
Newtype CreateImageBuilderResult _
```

#### `CreateImageBuilderStreamingURLRequest`

``` purescript
newtype CreateImageBuilderStreamingURLRequest
  = CreateImageBuilderStreamingURLRequest { "Name" :: String, "Validity" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype CreateImageBuilderStreamingURLRequest _
```

#### `CreateImageBuilderStreamingURLResult`

``` purescript
newtype CreateImageBuilderStreamingURLResult
  = CreateImageBuilderStreamingURLResult { "StreamingURL" :: NullOrUndefined (String), "Expires" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype CreateImageBuilderStreamingURLResult _
```

#### `CreateStackRequest`

``` purescript
newtype CreateStackRequest
  = CreateStackRequest { "Name" :: String, "Description" :: NullOrUndefined (Description), "DisplayName" :: NullOrUndefined (DisplayName), "StorageConnectors" :: NullOrUndefined (StorageConnectorList), "RedirectURL" :: NullOrUndefined (RedirectURL) }
```

##### Instances
``` purescript
Newtype CreateStackRequest _
```

#### `CreateStackResult`

``` purescript
newtype CreateStackResult
  = CreateStackResult { "Stack" :: NullOrUndefined (Stack) }
```

##### Instances
``` purescript
Newtype CreateStackResult _
```

#### `CreateStreamingURLRequest`

``` purescript
newtype CreateStreamingURLRequest
  = CreateStreamingURLRequest { "StackName" :: String, "FleetName" :: String, "UserId" :: StreamingUrlUserId, "ApplicationId" :: NullOrUndefined (String), "Validity" :: NullOrUndefined (Number), "SessionContext" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype CreateStreamingURLRequest _
```

#### `CreateStreamingURLResult`

``` purescript
newtype CreateStreamingURLResult
  = CreateStreamingURLResult { "StreamingURL" :: NullOrUndefined (String), "Expires" :: NullOrUndefined (Number) }
```

##### Instances
``` purescript
Newtype CreateStreamingURLResult _
```

#### `DeleteDirectoryConfigRequest`

``` purescript
newtype DeleteDirectoryConfigRequest
  = DeleteDirectoryConfigRequest { "DirectoryName" :: DirectoryName }
```

##### Instances
``` purescript
Newtype DeleteDirectoryConfigRequest _
```

#### `DeleteDirectoryConfigResult`

``` purescript
newtype DeleteDirectoryConfigResult
  = DeleteDirectoryConfigResult {  }
```

##### Instances
``` purescript
Newtype DeleteDirectoryConfigResult _
```

#### `DeleteFleetRequest`

``` purescript
newtype DeleteFleetRequest
  = DeleteFleetRequest { "Name" :: String }
```

##### Instances
``` purescript
Newtype DeleteFleetRequest _
```

#### `DeleteFleetResult`

``` purescript
newtype DeleteFleetResult
  = DeleteFleetResult {  }
```

##### Instances
``` purescript
Newtype DeleteFleetResult _
```

#### `DeleteImageBuilderRequest`

``` purescript
newtype DeleteImageBuilderRequest
  = DeleteImageBuilderRequest { "Name" :: Name }
```

##### Instances
``` purescript
Newtype DeleteImageBuilderRequest _
```

#### `DeleteImageBuilderResult`

``` purescript
newtype DeleteImageBuilderResult
  = DeleteImageBuilderResult { "ImageBuilder" :: NullOrUndefined (ImageBuilder) }
```

##### Instances
``` purescript
Newtype DeleteImageBuilderResult _
```

#### `DeleteImageRequest`

``` purescript
newtype DeleteImageRequest
  = DeleteImageRequest { "Name" :: Name }
```

##### Instances
``` purescript
Newtype DeleteImageRequest _
```

#### `DeleteImageResult`

``` purescript
newtype DeleteImageResult
  = DeleteImageResult { "Image" :: NullOrUndefined (Image) }
```

##### Instances
``` purescript
Newtype DeleteImageResult _
```

#### `DeleteStackRequest`

``` purescript
newtype DeleteStackRequest
  = DeleteStackRequest { "Name" :: String }
```

##### Instances
``` purescript
Newtype DeleteStackRequest _
```

#### `DeleteStackResult`

``` purescript
newtype DeleteStackResult
  = DeleteStackResult {  }
```

##### Instances
``` purescript
Newtype DeleteStackResult _
```

#### `DescribeDirectoryConfigsRequest`

``` purescript
newtype DescribeDirectoryConfigsRequest
  = DescribeDirectoryConfigsRequest { "DirectoryNames" :: NullOrUndefined (DirectoryNameList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeDirectoryConfigsRequest _
```

#### `DescribeDirectoryConfigsResult`

``` purescript
newtype DescribeDirectoryConfigsResult
  = DescribeDirectoryConfigsResult { "DirectoryConfigs" :: NullOrUndefined (DirectoryConfigList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeDirectoryConfigsResult _
```

#### `DescribeFleetsRequest`

``` purescript
newtype DescribeFleetsRequest
  = DescribeFleetsRequest { "Names" :: NullOrUndefined (StringList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeFleetsRequest _
```

#### `DescribeFleetsResult`

``` purescript
newtype DescribeFleetsResult
  = DescribeFleetsResult { "Fleets" :: NullOrUndefined (FleetList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeFleetsResult _
```

#### `DescribeImageBuildersRequest`

``` purescript
newtype DescribeImageBuildersRequest
  = DescribeImageBuildersRequest { "Names" :: NullOrUndefined (StringList), "MaxResults" :: NullOrUndefined (Int), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeImageBuildersRequest _
```

#### `DescribeImageBuildersResult`

``` purescript
newtype DescribeImageBuildersResult
  = DescribeImageBuildersResult { "ImageBuilders" :: NullOrUndefined (ImageBuilderList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeImageBuildersResult _
```

#### `DescribeImagesRequest`

``` purescript
newtype DescribeImagesRequest
  = DescribeImagesRequest { "Names" :: NullOrUndefined (StringList) }
```

##### Instances
``` purescript
Newtype DescribeImagesRequest _
```

#### `DescribeImagesResult`

``` purescript
newtype DescribeImagesResult
  = DescribeImagesResult { "Images" :: NullOrUndefined (ImageList) }
```

##### Instances
``` purescript
Newtype DescribeImagesResult _
```

#### `DescribeSessionsRequest`

``` purescript
newtype DescribeSessionsRequest
  = DescribeSessionsRequest { "StackName" :: String, "FleetName" :: String, "UserId" :: NullOrUndefined (UserId), "NextToken" :: NullOrUndefined (String), "Limit" :: NullOrUndefined (Int), "AuthenticationType" :: NullOrUndefined (AuthenticationType) }
```

##### Instances
``` purescript
Newtype DescribeSessionsRequest _
```

#### `DescribeSessionsResult`

``` purescript
newtype DescribeSessionsResult
  = DescribeSessionsResult { "Sessions" :: NullOrUndefined (SessionList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeSessionsResult _
```

#### `DescribeStacksRequest`

``` purescript
newtype DescribeStacksRequest
  = DescribeStacksRequest { "Names" :: NullOrUndefined (StringList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeStacksRequest _
```

#### `DescribeStacksResult`

``` purescript
newtype DescribeStacksResult
  = DescribeStacksResult { "Stacks" :: NullOrUndefined (StackList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype DescribeStacksResult _
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

#### `DirectoryConfig`

``` purescript
newtype DirectoryConfig
  = DirectoryConfig { "DirectoryName" :: DirectoryName, "OrganizationalUnitDistinguishedNames" :: NullOrUndefined (OrganizationalUnitDistinguishedNamesList), "ServiceAccountCredentials" :: NullOrUndefined (ServiceAccountCredentials), "CreatedTime" :: NullOrUndefined (Number) }
```

<p>Configuration information for the directory used to join domains.</p>

##### Instances
``` purescript
Newtype DirectoryConfig _
```

#### `DirectoryConfigList`

``` purescript
newtype DirectoryConfigList
  = DirectoryConfigList (Array DirectoryConfig)
```

##### Instances
``` purescript
Newtype DirectoryConfigList _
```

#### `DirectoryName`

``` purescript
newtype DirectoryName
  = DirectoryName String
```

##### Instances
``` purescript
Newtype DirectoryName _
```

#### `DirectoryNameList`

``` purescript
newtype DirectoryNameList
  = DirectoryNameList (Array DirectoryName)
```

##### Instances
``` purescript
Newtype DirectoryNameList _
```

#### `DisassociateFleetRequest`

``` purescript
newtype DisassociateFleetRequest
  = DisassociateFleetRequest { "FleetName" :: String, "StackName" :: String }
```

##### Instances
``` purescript
Newtype DisassociateFleetRequest _
```

#### `DisassociateFleetResult`

``` purescript
newtype DisassociateFleetResult
  = DisassociateFleetResult {  }
```

##### Instances
``` purescript
Newtype DisassociateFleetResult _
```

#### `DisplayName`

``` purescript
newtype DisplayName
  = DisplayName String
```

##### Instances
``` purescript
Newtype DisplayName _
```

#### `DomainJoinInfo`

``` purescript
newtype DomainJoinInfo
  = DomainJoinInfo { "DirectoryName" :: NullOrUndefined (DirectoryName), "OrganizationalUnitDistinguishedName" :: NullOrUndefined (OrganizationalUnitDistinguishedName) }
```

<p>Contains the information needed to join a Microsoft Active Directory domain.</p>

##### Instances
``` purescript
Newtype DomainJoinInfo _
```

#### `ErrorMessage`

``` purescript
newtype ErrorMessage
  = ErrorMessage String
```

<p>The error message in the exception.</p>

##### Instances
``` purescript
Newtype ErrorMessage _
```

#### `ExpireSessionRequest`

``` purescript
newtype ExpireSessionRequest
  = ExpireSessionRequest { "SessionId" :: String }
```

##### Instances
``` purescript
Newtype ExpireSessionRequest _
```

#### `ExpireSessionResult`

``` purescript
newtype ExpireSessionResult
  = ExpireSessionResult {  }
```

##### Instances
``` purescript
Newtype ExpireSessionResult _
```

#### `Fleet`

``` purescript
newtype Fleet
  = Fleet { "Arn" :: Arn, "Name" :: String, "DisplayName" :: NullOrUndefined (String), "Description" :: NullOrUndefined (String), "ImageName" :: String, "InstanceType" :: String, "FleetType" :: NullOrUndefined (FleetType), "ComputeCapacityStatus" :: ComputeCapacityStatus, "MaxUserDurationInSeconds" :: NullOrUndefined (Int), "DisconnectTimeoutInSeconds" :: NullOrUndefined (Int), "State" :: FleetState, "VpcConfig" :: NullOrUndefined (VpcConfig), "CreatedTime" :: NullOrUndefined (Number), "FleetErrors" :: NullOrUndefined (FleetErrors), "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject), "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo) }
```

<p>Contains the parameters for a fleet.</p>

##### Instances
``` purescript
Newtype Fleet _
```

#### `FleetAttribute`

``` purescript
newtype FleetAttribute
  = FleetAttribute String
```

<p>The fleet attribute.</p>

##### Instances
``` purescript
Newtype FleetAttribute _
```

#### `FleetAttributes`

``` purescript
newtype FleetAttributes
  = FleetAttributes (Array FleetAttribute)
```

<p>The fleet attributes.</p>

##### Instances
``` purescript
Newtype FleetAttributes _
```

#### `FleetError`

``` purescript
newtype FleetError
  = FleetError { "ErrorCode" :: NullOrUndefined (FleetErrorCode), "ErrorMessage" :: NullOrUndefined (String) }
```

<p>Describes a fleet error.</p>

##### Instances
``` purescript
Newtype FleetError _
```

#### `FleetErrorCode`

``` purescript
newtype FleetErrorCode
  = FleetErrorCode String
```

##### Instances
``` purescript
Newtype FleetErrorCode _
```

#### `FleetErrors`

``` purescript
newtype FleetErrors
  = FleetErrors (Array FleetError)
```

##### Instances
``` purescript
Newtype FleetErrors _
```

#### `FleetList`

``` purescript
newtype FleetList
  = FleetList (Array Fleet)
```

<p>The fleets.</p>

##### Instances
``` purescript
Newtype FleetList _
```

#### `FleetState`

``` purescript
newtype FleetState
  = FleetState String
```

##### Instances
``` purescript
Newtype FleetState _
```

#### `FleetType`

``` purescript
newtype FleetType
  = FleetType String
```

##### Instances
``` purescript
Newtype FleetType _
```

#### `Image`

``` purescript
newtype Image
  = Image { "Name" :: String, "Arn" :: NullOrUndefined (Arn), "BaseImageArn" :: NullOrUndefined (Arn), "DisplayName" :: NullOrUndefined (String), "State" :: NullOrUndefined (ImageState), "Visibility" :: NullOrUndefined (VisibilityType), "ImageBuilderSupported" :: NullOrUndefined (Boolean), "Platform" :: NullOrUndefined (PlatformType), "Description" :: NullOrUndefined (String), "StateChangeReason" :: NullOrUndefined (ImageStateChangeReason), "Applications" :: NullOrUndefined (Applications), "CreatedTime" :: NullOrUndefined (Number), "PublicBaseImageReleasedDate" :: NullOrUndefined (Number), "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion) }
```

<p>Describes an image.</p>

##### Instances
``` purescript
Newtype Image _
```

#### `ImageBuilder`

``` purescript
newtype ImageBuilder
  = ImageBuilder { "Name" :: String, "Arn" :: NullOrUndefined (Arn), "ImageArn" :: NullOrUndefined (Arn), "Description" :: NullOrUndefined (String), "DisplayName" :: NullOrUndefined (String), "VpcConfig" :: NullOrUndefined (VpcConfig), "InstanceType" :: NullOrUndefined (String), "Platform" :: NullOrUndefined (PlatformType), "State" :: NullOrUndefined (ImageBuilderState), "StateChangeReason" :: NullOrUndefined (ImageBuilderStateChangeReason), "CreatedTime" :: NullOrUndefined (Number), "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject), "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo), "ImageBuilderErrors" :: NullOrUndefined (ResourceErrors), "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion) }
```

<p>Describes a streaming instance used for editing an image. New images are created from a snapshot through an image builder.</p>

##### Instances
``` purescript
Newtype ImageBuilder _
```

#### `ImageBuilderList`

``` purescript
newtype ImageBuilderList
  = ImageBuilderList (Array ImageBuilder)
```

##### Instances
``` purescript
Newtype ImageBuilderList _
```

#### `ImageBuilderState`

``` purescript
newtype ImageBuilderState
  = ImageBuilderState String
```

##### Instances
``` purescript
Newtype ImageBuilderState _
```

#### `ImageBuilderStateChangeReason`

``` purescript
newtype ImageBuilderStateChangeReason
  = ImageBuilderStateChangeReason { "Code" :: NullOrUndefined (ImageBuilderStateChangeReasonCode), "Message" :: NullOrUndefined (String) }
```

<p>Describes the reason why the last image builder state change occurred.</p>

##### Instances
``` purescript
Newtype ImageBuilderStateChangeReason _
```

#### `ImageBuilderStateChangeReasonCode`

``` purescript
newtype ImageBuilderStateChangeReasonCode
  = ImageBuilderStateChangeReasonCode String
```

##### Instances
``` purescript
Newtype ImageBuilderStateChangeReasonCode _
```

#### `ImageList`

``` purescript
newtype ImageList
  = ImageList (Array Image)
```

##### Instances
``` purescript
Newtype ImageList _
```

#### `ImageState`

``` purescript
newtype ImageState
  = ImageState String
```

##### Instances
``` purescript
Newtype ImageState _
```

#### `ImageStateChangeReason`

``` purescript
newtype ImageStateChangeReason
  = ImageStateChangeReason { "Code" :: NullOrUndefined (ImageStateChangeReasonCode), "Message" :: NullOrUndefined (String) }
```

<p>Describes the reason why the last image state change occurred.</p>

##### Instances
``` purescript
Newtype ImageStateChangeReason _
```

#### `ImageStateChangeReasonCode`

``` purescript
newtype ImageStateChangeReasonCode
  = ImageStateChangeReasonCode String
```

##### Instances
``` purescript
Newtype ImageStateChangeReasonCode _
```

#### `IncompatibleImageException`

``` purescript
newtype IncompatibleImageException
  = IncompatibleImageException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The image does not support storage connectors.</p>

##### Instances
``` purescript
Newtype IncompatibleImageException _
```

#### `InvalidParameterCombinationException`

``` purescript
newtype InvalidParameterCombinationException
  = InvalidParameterCombinationException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>Indicates an incorrect combination of parameters, or a missing parameter.</p>

##### Instances
``` purescript
Newtype InvalidParameterCombinationException _
```

#### `InvalidRoleException`

``` purescript
newtype InvalidRoleException
  = InvalidRoleException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified role is invalid.</p>

##### Instances
``` purescript
Newtype InvalidRoleException _
```

#### `LimitExceededException`

``` purescript
newtype LimitExceededException
  = LimitExceededException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The requested limit exceeds the permitted limit for an account.</p>

##### Instances
``` purescript
Newtype LimitExceededException _
```

#### `ListAssociatedFleetsRequest`

``` purescript
newtype ListAssociatedFleetsRequest
  = ListAssociatedFleetsRequest { "StackName" :: String, "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListAssociatedFleetsRequest _
```

#### `ListAssociatedFleetsResult`

``` purescript
newtype ListAssociatedFleetsResult
  = ListAssociatedFleetsResult { "Names" :: NullOrUndefined (StringList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListAssociatedFleetsResult _
```

#### `ListAssociatedStacksRequest`

``` purescript
newtype ListAssociatedStacksRequest
  = ListAssociatedStacksRequest { "FleetName" :: String, "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListAssociatedStacksRequest _
```

#### `ListAssociatedStacksResult`

``` purescript
newtype ListAssociatedStacksResult
  = ListAssociatedStacksResult { "Names" :: NullOrUndefined (StringList), "NextToken" :: NullOrUndefined (String) }
```

##### Instances
``` purescript
Newtype ListAssociatedStacksResult _
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceArn" :: Arn }
```

##### Instances
``` purescript
Newtype ListTagsForResourceRequest _
```

#### `ListTagsForResourceResponse`

``` purescript
newtype ListTagsForResourceResponse
  = ListTagsForResourceResponse { "Tags" :: NullOrUndefined (Tags) }
```

##### Instances
``` purescript
Newtype ListTagsForResourceResponse _
```

#### `Metadata`

``` purescript
newtype Metadata
  = Metadata (Map String String)
```

##### Instances
``` purescript
Newtype Metadata _
```

#### `Name`

``` purescript
newtype Name
  = Name String
```

##### Instances
``` purescript
Newtype Name _
```

#### `OperationNotPermittedException`

``` purescript
newtype OperationNotPermittedException
  = OperationNotPermittedException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The attempted operation is not permitted.</p>

##### Instances
``` purescript
Newtype OperationNotPermittedException _
```

#### `OrganizationalUnitDistinguishedName`

``` purescript
newtype OrganizationalUnitDistinguishedName
  = OrganizationalUnitDistinguishedName String
```

##### Instances
``` purescript
Newtype OrganizationalUnitDistinguishedName _
```

#### `OrganizationalUnitDistinguishedNamesList`

``` purescript
newtype OrganizationalUnitDistinguishedNamesList
  = OrganizationalUnitDistinguishedNamesList (Array OrganizationalUnitDistinguishedName)
```

##### Instances
``` purescript
Newtype OrganizationalUnitDistinguishedNamesList _
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

#### `RedirectURL`

``` purescript
newtype RedirectURL
  = RedirectURL String
```

##### Instances
``` purescript
Newtype RedirectURL _
```

#### `RegionName`

``` purescript
newtype RegionName
  = RegionName String
```

##### Instances
``` purescript
Newtype RegionName _
```

#### `ResourceAlreadyExistsException`

``` purescript
newtype ResourceAlreadyExistsException
  = ResourceAlreadyExistsException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified resource already exists.</p>

##### Instances
``` purescript
Newtype ResourceAlreadyExistsException _
```

#### `ResourceError`

``` purescript
newtype ResourceError
  = ResourceError { "ErrorCode" :: NullOrUndefined (FleetErrorCode), "ErrorMessage" :: NullOrUndefined (String), "ErrorTimestamp" :: NullOrUndefined (Number) }
```

<p>Describes a resource error.</p>

##### Instances
``` purescript
Newtype ResourceError _
```

#### `ResourceErrors`

``` purescript
newtype ResourceErrors
  = ResourceErrors (Array ResourceError)
```

##### Instances
``` purescript
Newtype ResourceErrors _
```

#### `ResourceIdentifier`

``` purescript
newtype ResourceIdentifier
  = ResourceIdentifier String
```

<p>The ARN of the resource.</p>

##### Instances
``` purescript
Newtype ResourceIdentifier _
```

#### `ResourceInUseException`

``` purescript
newtype ResourceInUseException
  = ResourceInUseException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified resource is in use.</p>

##### Instances
``` purescript
Newtype ResourceInUseException _
```

#### `ResourceNotAvailableException`

``` purescript
newtype ResourceNotAvailableException
  = ResourceNotAvailableException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified resource exists and is not in use, but isn't available.</p>

##### Instances
``` purescript
Newtype ResourceNotAvailableException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message" :: NullOrUndefined (ErrorMessage) }
```

<p>The specified resource was not found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `SecurityGroupIdList`

``` purescript
newtype SecurityGroupIdList
  = SecurityGroupIdList (Array String)
```

<p>The security group IDs.</p>

##### Instances
``` purescript
Newtype SecurityGroupIdList _
```

#### `ServiceAccountCredentials`

``` purescript
newtype ServiceAccountCredentials
  = ServiceAccountCredentials { "AccountName" :: AccountName, "AccountPassword" :: AccountPassword }
```

<p>Describes the credentials for the service account used by the streaming instance to connect to the directory.</p>

##### Instances
``` purescript
Newtype ServiceAccountCredentials _
```

#### `Session`

``` purescript
newtype Session
  = Session { "Id" :: String, "UserId" :: UserId, "StackName" :: String, "FleetName" :: String, "State" :: SessionState, "AuthenticationType" :: NullOrUndefined (AuthenticationType) }
```

<p>Describes a streaming session.</p>

##### Instances
``` purescript
Newtype Session _
```

#### `SessionList`

``` purescript
newtype SessionList
  = SessionList (Array Session)
```

<p>List of sessions.</p>

##### Instances
``` purescript
Newtype SessionList _
```

#### `SessionState`

``` purescript
newtype SessionState
  = SessionState String
```

<p>Possible values for the state of a streaming session.</p>

##### Instances
``` purescript
Newtype SessionState _
```

#### `Stack`

``` purescript
newtype Stack
  = Stack { "Arn" :: NullOrUndefined (Arn), "Name" :: String, "Description" :: NullOrUndefined (String), "DisplayName" :: NullOrUndefined (String), "CreatedTime" :: NullOrUndefined (Number), "StorageConnectors" :: NullOrUndefined (StorageConnectorList), "RedirectURL" :: NullOrUndefined (RedirectURL), "StackErrors" :: NullOrUndefined (StackErrors) }
```

<p>Describes a stack.</p>

##### Instances
``` purescript
Newtype Stack _
```

#### `StackAttribute`

``` purescript
newtype StackAttribute
  = StackAttribute String
```

##### Instances
``` purescript
Newtype StackAttribute _
```

#### `StackAttributes`

``` purescript
newtype StackAttributes
  = StackAttributes (Array StackAttribute)
```

##### Instances
``` purescript
Newtype StackAttributes _
```

#### `StackError`

``` purescript
newtype StackError
  = StackError { "ErrorCode" :: NullOrUndefined (StackErrorCode), "ErrorMessage" :: NullOrUndefined (String) }
```

<p>Describes a stack error.</p>

##### Instances
``` purescript
Newtype StackError _
```

#### `StackErrorCode`

``` purescript
newtype StackErrorCode
  = StackErrorCode String
```

##### Instances
``` purescript
Newtype StackErrorCode _
```

#### `StackErrors`

``` purescript
newtype StackErrors
  = StackErrors (Array StackError)
```

<p>The stack errors.</p>

##### Instances
``` purescript
Newtype StackErrors _
```

#### `StackList`

``` purescript
newtype StackList
  = StackList (Array Stack)
```

<p>The stacks.</p>

##### Instances
``` purescript
Newtype StackList _
```

#### `StartFleetRequest`

``` purescript
newtype StartFleetRequest
  = StartFleetRequest { "Name" :: String }
```

##### Instances
``` purescript
Newtype StartFleetRequest _
```

#### `StartFleetResult`

``` purescript
newtype StartFleetResult
  = StartFleetResult {  }
```

##### Instances
``` purescript
Newtype StartFleetResult _
```

#### `StartImageBuilderRequest`

``` purescript
newtype StartImageBuilderRequest
  = StartImageBuilderRequest { "Name" :: String, "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion) }
```

##### Instances
``` purescript
Newtype StartImageBuilderRequest _
```

#### `StartImageBuilderResult`

``` purescript
newtype StartImageBuilderResult
  = StartImageBuilderResult { "ImageBuilder" :: NullOrUndefined (ImageBuilder) }
```

##### Instances
``` purescript
Newtype StartImageBuilderResult _
```

#### `StopFleetRequest`

``` purescript
newtype StopFleetRequest
  = StopFleetRequest { "Name" :: String }
```

##### Instances
``` purescript
Newtype StopFleetRequest _
```

#### `StopFleetResult`

``` purescript
newtype StopFleetResult
  = StopFleetResult {  }
```

##### Instances
``` purescript
Newtype StopFleetResult _
```

#### `StopImageBuilderRequest`

``` purescript
newtype StopImageBuilderRequest
  = StopImageBuilderRequest { "Name" :: String }
```

##### Instances
``` purescript
Newtype StopImageBuilderRequest _
```

#### `StopImageBuilderResult`

``` purescript
newtype StopImageBuilderResult
  = StopImageBuilderResult { "ImageBuilder" :: NullOrUndefined (ImageBuilder) }
```

##### Instances
``` purescript
Newtype StopImageBuilderResult _
```

#### `StorageConnector`

``` purescript
newtype StorageConnector
  = StorageConnector { "ConnectorType" :: StorageConnectorType, "ResourceIdentifier" :: NullOrUndefined (ResourceIdentifier) }
```

<p>Describes a storage connector.</p>

##### Instances
``` purescript
Newtype StorageConnector _
```

#### `StorageConnectorList`

``` purescript
newtype StorageConnectorList
  = StorageConnectorList (Array StorageConnector)
```

<p>The storage connectors.</p>

##### Instances
``` purescript
Newtype StorageConnectorList _
```

#### `StorageConnectorType`

``` purescript
newtype StorageConnectorType
  = StorageConnectorType String
```

<p>The type of storage connector.</p>

##### Instances
``` purescript
Newtype StorageConnectorType _
```

#### `StreamingUrlUserId`

``` purescript
newtype StreamingUrlUserId
  = StreamingUrlUserId String
```

##### Instances
``` purescript
Newtype StreamingUrlUserId _
```

#### `StringList`

``` purescript
newtype StringList
  = StringList (Array String)
```

##### Instances
``` purescript
Newtype StringList _
```

#### `SubnetIdList`

``` purescript
newtype SubnetIdList
  = SubnetIdList (Array String)
```

<p>The subnet IDs.</p>

##### Instances
``` purescript
Newtype SubnetIdList _
```

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

##### Instances
``` purescript
Newtype TagKey _
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeyList _
```

#### `TagResourceRequest`

``` purescript
newtype TagResourceRequest
  = TagResourceRequest { "ResourceArn" :: Arn, "Tags" :: Tags }
```

##### Instances
``` purescript
Newtype TagResourceRequest _
```

#### `TagResourceResponse`

``` purescript
newtype TagResourceResponse
  = TagResourceResponse {  }
```

##### Instances
``` purescript
Newtype TagResourceResponse _
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

##### Instances
``` purescript
Newtype TagValue _
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Map TagKey TagValue)
```

##### Instances
``` purescript
Newtype Tags _
```

#### `UntagResourceRequest`

``` purescript
newtype UntagResourceRequest
  = UntagResourceRequest { "ResourceArn" :: Arn, "TagKeys" :: TagKeyList }
```

##### Instances
``` purescript
Newtype UntagResourceRequest _
```

#### `UntagResourceResponse`

``` purescript
newtype UntagResourceResponse
  = UntagResourceResponse {  }
```

##### Instances
``` purescript
Newtype UntagResourceResponse _
```

#### `UpdateDirectoryConfigRequest`

``` purescript
newtype UpdateDirectoryConfigRequest
  = UpdateDirectoryConfigRequest { "DirectoryName" :: DirectoryName, "OrganizationalUnitDistinguishedNames" :: NullOrUndefined (OrganizationalUnitDistinguishedNamesList), "ServiceAccountCredentials" :: NullOrUndefined (ServiceAccountCredentials) }
```

##### Instances
``` purescript
Newtype UpdateDirectoryConfigRequest _
```

#### `UpdateDirectoryConfigResult`

``` purescript
newtype UpdateDirectoryConfigResult
  = UpdateDirectoryConfigResult { "DirectoryConfig" :: NullOrUndefined (DirectoryConfig) }
```

##### Instances
``` purescript
Newtype UpdateDirectoryConfigResult _
```

#### `UpdateFleetRequest`

``` purescript
newtype UpdateFleetRequest
  = UpdateFleetRequest { "ImageName" :: NullOrUndefined (String), "Name" :: String, "InstanceType" :: NullOrUndefined (String), "ComputeCapacity" :: NullOrUndefined (ComputeCapacity), "VpcConfig" :: NullOrUndefined (VpcConfig), "MaxUserDurationInSeconds" :: NullOrUndefined (Int), "DisconnectTimeoutInSeconds" :: NullOrUndefined (Int), "DeleteVpcConfig" :: NullOrUndefined (Boolean), "Description" :: NullOrUndefined (Description), "DisplayName" :: NullOrUndefined (DisplayName), "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject), "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo), "AttributesToDelete" :: NullOrUndefined (FleetAttributes) }
```

##### Instances
``` purescript
Newtype UpdateFleetRequest _
```

#### `UpdateFleetResult`

``` purescript
newtype UpdateFleetResult
  = UpdateFleetResult { "Fleet" :: NullOrUndefined (Fleet) }
```

##### Instances
``` purescript
Newtype UpdateFleetResult _
```

#### `UpdateStackRequest`

``` purescript
newtype UpdateStackRequest
  = UpdateStackRequest { "DisplayName" :: NullOrUndefined (DisplayName), "Description" :: NullOrUndefined (Description), "Name" :: String, "StorageConnectors" :: NullOrUndefined (StorageConnectorList), "DeleteStorageConnectors" :: NullOrUndefined (Boolean), "RedirectURL" :: NullOrUndefined (RedirectURL), "AttributesToDelete" :: NullOrUndefined (StackAttributes) }
```

##### Instances
``` purescript
Newtype UpdateStackRequest _
```

#### `UpdateStackResult`

``` purescript
newtype UpdateStackResult
  = UpdateStackResult { "Stack" :: NullOrUndefined (Stack) }
```

##### Instances
``` purescript
Newtype UpdateStackResult _
```

#### `UserId`

``` purescript
newtype UserId
  = UserId String
```

##### Instances
``` purescript
Newtype UserId _
```

#### `VisibilityType`

``` purescript
newtype VisibilityType
  = VisibilityType String
```

##### Instances
``` purescript
Newtype VisibilityType _
```

#### `VpcConfig`

``` purescript
newtype VpcConfig
  = VpcConfig { "SubnetIds" :: NullOrUndefined (SubnetIdList), "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdList) }
```

<p>Describes VPC configuration information.</p>

##### Instances
``` purescript
Newtype VpcConfig _
```


