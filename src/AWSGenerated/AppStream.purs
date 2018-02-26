

-- | <fullname>Amazon AppStream 2.0</fullname> <p>You can use Amazon AppStream 2.0 to stream desktop applications to any device running a web browser, without rewriting them.</p>
module AWS.AppStream where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AppStream" :: String


-- | <p>Associates the specified fleet with the specified stack.</p>
associateFleet :: forall eff. AssociateFleetRequest -> Aff (err :: AWS.RequestError | eff) AssociateFleetResult
associateFleet = AWS.request serviceName "AssociateFleet" 


-- | <p>Copies the image within the same region or to a new region within the same AWS account. Note that any tags you added to the image will not be copied.</p>
copyImage :: forall eff. CopyImageRequest -> Aff (err :: AWS.RequestError | eff) CopyImageResponse
copyImage = AWS.request serviceName "CopyImage" 


-- | <p>Creates a directory configuration.</p>
createDirectoryConfig :: forall eff. CreateDirectoryConfigRequest -> Aff (err :: AWS.RequestError | eff) CreateDirectoryConfigResult
createDirectoryConfig = AWS.request serviceName "CreateDirectoryConfig" 


-- | <p>Creates a fleet.</p>
createFleet :: forall eff. CreateFleetRequest -> Aff (err :: AWS.RequestError | eff) CreateFleetResult
createFleet = AWS.request serviceName "CreateFleet" 


-- | <p>Creates an image builder.</p> <p>The initial state of the builder is <code>PENDING</code>. When it is ready, the state is <code>RUNNING</code>.</p>
createImageBuilder :: forall eff. CreateImageBuilderRequest -> Aff (err :: AWS.RequestError | eff) CreateImageBuilderResult
createImageBuilder = AWS.request serviceName "CreateImageBuilder" 


-- | <p>Creates a URL to start an image builder streaming session.</p>
createImageBuilderStreamingURL :: forall eff. CreateImageBuilderStreamingURLRequest -> Aff (err :: AWS.RequestError | eff) CreateImageBuilderStreamingURLResult
createImageBuilderStreamingURL = AWS.request serviceName "CreateImageBuilderStreamingURL" 


-- | <p>Creates a stack.</p>
createStack :: forall eff. CreateStackRequest -> Aff (err :: AWS.RequestError | eff) CreateStackResult
createStack = AWS.request serviceName "CreateStack" 


-- | <p>Creates a URL to start a streaming session for the specified user.</p>
createStreamingURL :: forall eff. CreateStreamingURLRequest -> Aff (err :: AWS.RequestError | eff) CreateStreamingURLResult
createStreamingURL = AWS.request serviceName "CreateStreamingURL" 


-- | <p>Deletes the specified directory configuration.</p>
deleteDirectoryConfig :: forall eff. DeleteDirectoryConfigRequest -> Aff (err :: AWS.RequestError | eff) DeleteDirectoryConfigResult
deleteDirectoryConfig = AWS.request serviceName "DeleteDirectoryConfig" 


-- | <p>Deletes the specified fleet.</p>
deleteFleet :: forall eff. DeleteFleetRequest -> Aff (err :: AWS.RequestError | eff) DeleteFleetResult
deleteFleet = AWS.request serviceName "DeleteFleet" 


-- | <p>Deletes the specified image. You cannot delete an image that is currently in use. After you delete an image, you cannot provision new capacity using the image.</p>
deleteImage :: forall eff. DeleteImageRequest -> Aff (err :: AWS.RequestError | eff) DeleteImageResult
deleteImage = AWS.request serviceName "DeleteImage" 


-- | <p>Deletes the specified image builder and releases the capacity.</p>
deleteImageBuilder :: forall eff. DeleteImageBuilderRequest -> Aff (err :: AWS.RequestError | eff) DeleteImageBuilderResult
deleteImageBuilder = AWS.request serviceName "DeleteImageBuilder" 


-- | <p>Deletes the specified stack. After this operation completes, the environment can no longer be activated and any reservations made for the stack are released.</p>
deleteStack :: forall eff. DeleteStackRequest -> Aff (err :: AWS.RequestError | eff) DeleteStackResult
deleteStack = AWS.request serviceName "DeleteStack" 


-- | <p>Describes the specified directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response. </p>
describeDirectoryConfigs :: forall eff. DescribeDirectoryConfigsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDirectoryConfigsResult
describeDirectoryConfigs = AWS.request serviceName "DescribeDirectoryConfigs" 


-- | <p>Describes the specified fleets or all fleets in the account.</p>
describeFleets :: forall eff. DescribeFleetsRequest -> Aff (err :: AWS.RequestError | eff) DescribeFleetsResult
describeFleets = AWS.request serviceName "DescribeFleets" 


-- | <p>Describes the specified image builders or all image builders in the account.</p>
describeImageBuilders :: forall eff. DescribeImageBuildersRequest -> Aff (err :: AWS.RequestError | eff) DescribeImageBuildersResult
describeImageBuilders = AWS.request serviceName "DescribeImageBuilders" 


-- | <p>Describes the specified images or all images in the account.</p>
describeImages :: forall eff. DescribeImagesRequest -> Aff (err :: AWS.RequestError | eff) DescribeImagesResult
describeImages = AWS.request serviceName "DescribeImages" 


-- | <p>Describes the streaming sessions for the specified stack and fleet. If a user ID is provided, only the streaming sessions for only that user are returned. If an authentication type is not provided, the default is to authenticate users using a streaming URL.</p>
describeSessions :: forall eff. DescribeSessionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeSessionsResult
describeSessions = AWS.request serviceName "DescribeSessions" 


-- | <p>Describes the specified stacks or all stacks in the account.</p>
describeStacks :: forall eff. DescribeStacksRequest -> Aff (err :: AWS.RequestError | eff) DescribeStacksResult
describeStacks = AWS.request serviceName "DescribeStacks" 


-- | <p>Disassociates the specified fleet from the specified stack.</p>
disassociateFleet :: forall eff. DisassociateFleetRequest -> Aff (err :: AWS.RequestError | eff) DisassociateFleetResult
disassociateFleet = AWS.request serviceName "DisassociateFleet" 


-- | <p>Stops the specified streaming session.</p>
expireSession :: forall eff. ExpireSessionRequest -> Aff (err :: AWS.RequestError | eff) ExpireSessionResult
expireSession = AWS.request serviceName "ExpireSession" 


-- | <p>Lists the fleets associated with the specified stack.</p>
listAssociatedFleets :: forall eff. ListAssociatedFleetsRequest -> Aff (err :: AWS.RequestError | eff) ListAssociatedFleetsResult
listAssociatedFleets = AWS.request serviceName "ListAssociatedFleets" 


-- | <p>Lists the stacks associated with the specified fleet.</p>
listAssociatedStacks :: forall eff. ListAssociatedStacksRequest -> Aff (err :: AWS.RequestError | eff) ListAssociatedStacksResult
listAssociatedStacks = AWS.request serviceName "ListAssociatedStacks" 


-- | <p>Lists the tags for the specified AppStream 2.0 resource. You can tag AppStream 2.0 image builders, images, fleets, and stacks.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResponse
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>Starts the specified fleet.</p>
startFleet :: forall eff. StartFleetRequest -> Aff (err :: AWS.RequestError | eff) StartFleetResult
startFleet = AWS.request serviceName "StartFleet" 


-- | <p>Starts the specified image builder.</p>
startImageBuilder :: forall eff. StartImageBuilderRequest -> Aff (err :: AWS.RequestError | eff) StartImageBuilderResult
startImageBuilder = AWS.request serviceName "StartImageBuilder" 


-- | <p>Stops the specified fleet.</p>
stopFleet :: forall eff. StopFleetRequest -> Aff (err :: AWS.RequestError | eff) StopFleetResult
stopFleet = AWS.request serviceName "StopFleet" 


-- | <p>Stops the specified image builder.</p>
stopImageBuilder :: forall eff. StopImageBuilderRequest -> Aff (err :: AWS.RequestError | eff) StopImageBuilderResult
stopImageBuilder = AWS.request serviceName "StopImageBuilder" 


-- | <p>Adds or overwrites one or more tags for the specified AppStream 2.0 resource. You can tag AppStream 2.0 image builders, images, fleets, and stacks.</p> <p>Each tag consists of a key and an optional value. If a resource already has a tag with the same key, this operation updates its value.</p> <p>To list the current tags for your resources, use <a>ListTagsForResource</a>. To disassociate tags from your resources, use <a>UntagResource</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) TagResourceResponse
tagResource = AWS.request serviceName "TagResource" 


-- | <p>Disassociates the specified tags from the specified AppStream 2.0 resource.</p> <p>To list the current tags for your resources, use <a>ListTagsForResource</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) UntagResourceResponse
untagResource = AWS.request serviceName "UntagResource" 


-- | <p>Updates the specified directory configuration.</p>
updateDirectoryConfig :: forall eff. UpdateDirectoryConfigRequest -> Aff (err :: AWS.RequestError | eff) UpdateDirectoryConfigResult
updateDirectoryConfig = AWS.request serviceName "UpdateDirectoryConfig" 


-- | <p>Updates the specified fleet.</p> <p>If the fleet is in the <code>STOPPED</code> state, you can update any attribute except the fleet name. If the fleet is in the <code>RUNNING</code> state, you can update the <code>DisplayName</code> and <code>ComputeCapacity</code> attributes. If the fleet is in the <code>STARTING</code> or <code>STOPPING</code> state, you can't update it.</p>
updateFleet :: forall eff. UpdateFleetRequest -> Aff (err :: AWS.RequestError | eff) UpdateFleetResult
updateFleet = AWS.request serviceName "UpdateFleet" 


-- | <p>Updates the specified stack.</p>
updateStack :: forall eff. UpdateStackRequest -> Aff (err :: AWS.RequestError | eff) UpdateStackResult
updateStack = AWS.request serviceName "UpdateStack" 


newtype AccountName = AccountName String


newtype AccountPassword = AccountPassword String


-- | <p>Describes an application in the application catalog.</p>
newtype Application = Application 
  { "Name" :: NullOrUndefined (String)
  , "DisplayName" :: NullOrUndefined (String)
  , "IconURL" :: NullOrUndefined (String)
  , "LaunchPath" :: NullOrUndefined (String)
  , "LaunchParameters" :: NullOrUndefined (String)
  , "Enabled" :: NullOrUndefined (Boolean)
  , "Metadata" :: NullOrUndefined (Metadata)
  }


newtype Applications = Applications (Array Application)


newtype AppstreamAgentVersion = AppstreamAgentVersion String


newtype Arn = Arn String


newtype AssociateFleetRequest = AssociateFleetRequest 
  { "FleetName" :: (String)
  , "StackName" :: (String)
  }


newtype AssociateFleetResult = AssociateFleetResult 
  { 
  }


newtype AuthenticationType = AuthenticationType String


newtype BooleanObject = BooleanObject Boolean


-- | <p>Describes the capacity for a fleet.</p>
newtype ComputeCapacity = ComputeCapacity 
  { "DesiredInstances" :: (Int)
  }


-- | <p>Describes the capacity status for a fleet.</p>
newtype ComputeCapacityStatus = ComputeCapacityStatus 
  { "Desired" :: (Int)
  , "Running" :: NullOrUndefined (Int)
  , "InUse" :: NullOrUndefined (Int)
  , "Available" :: NullOrUndefined (Int)
  }


-- | <p>An API error occurred. Wait a few minutes and try again.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype CopyImageRequest = CopyImageRequest 
  { "SourceImageName" :: (Name)
  , "DestinationImageName" :: (Name)
  , "DestinationRegion" :: (RegionName)
  , "DestinationImageDescription" :: NullOrUndefined (Description)
  }


newtype CopyImageResponse = CopyImageResponse 
  { "DestinationImageName" :: NullOrUndefined (Name)
  }


newtype CreateDirectoryConfigRequest = CreateDirectoryConfigRequest 
  { "DirectoryName" :: (DirectoryName)
  , "OrganizationalUnitDistinguishedNames" :: (OrganizationalUnitDistinguishedNamesList)
  , "ServiceAccountCredentials" :: (ServiceAccountCredentials)
  }


newtype CreateDirectoryConfigResult = CreateDirectoryConfigResult 
  { "DirectoryConfig" :: NullOrUndefined (DirectoryConfig)
  }


newtype CreateFleetRequest = CreateFleetRequest 
  { "Name" :: (Name)
  , "ImageName" :: (String)
  , "InstanceType" :: (String)
  , "FleetType" :: NullOrUndefined (FleetType)
  , "ComputeCapacity" :: (ComputeCapacity)
  , "VpcConfig" :: NullOrUndefined (VpcConfig)
  , "MaxUserDurationInSeconds" :: NullOrUndefined (Int)
  , "DisconnectTimeoutInSeconds" :: NullOrUndefined (Int)
  , "Description" :: NullOrUndefined (Description)
  , "DisplayName" :: NullOrUndefined (DisplayName)
  , "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject)
  , "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo)
  }


newtype CreateFleetResult = CreateFleetResult 
  { "Fleet" :: NullOrUndefined (Fleet)
  }


newtype CreateImageBuilderRequest = CreateImageBuilderRequest 
  { "Name" :: (Name)
  , "ImageName" :: (String)
  , "InstanceType" :: (String)
  , "Description" :: NullOrUndefined (Description)
  , "DisplayName" :: NullOrUndefined (DisplayName)
  , "VpcConfig" :: NullOrUndefined (VpcConfig)
  , "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject)
  , "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo)
  , "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion)
  }


newtype CreateImageBuilderResult = CreateImageBuilderResult 
  { "ImageBuilder" :: NullOrUndefined (ImageBuilder)
  }


newtype CreateImageBuilderStreamingURLRequest = CreateImageBuilderStreamingURLRequest 
  { "Name" :: (String)
  , "Validity" :: NullOrUndefined (Number)
  }


newtype CreateImageBuilderStreamingURLResult = CreateImageBuilderStreamingURLResult 
  { "StreamingURL" :: NullOrUndefined (String)
  , "Expires" :: NullOrUndefined (Number)
  }


newtype CreateStackRequest = CreateStackRequest 
  { "Name" :: (String)
  , "Description" :: NullOrUndefined (Description)
  , "DisplayName" :: NullOrUndefined (DisplayName)
  , "StorageConnectors" :: NullOrUndefined (StorageConnectorList)
  , "RedirectURL" :: NullOrUndefined (RedirectURL)
  }


newtype CreateStackResult = CreateStackResult 
  { "Stack" :: NullOrUndefined (Stack)
  }


newtype CreateStreamingURLRequest = CreateStreamingURLRequest 
  { "StackName" :: (String)
  , "FleetName" :: (String)
  , "UserId" :: (StreamingUrlUserId)
  , "ApplicationId" :: NullOrUndefined (String)
  , "Validity" :: NullOrUndefined (Number)
  , "SessionContext" :: NullOrUndefined (String)
  }


newtype CreateStreamingURLResult = CreateStreamingURLResult 
  { "StreamingURL" :: NullOrUndefined (String)
  , "Expires" :: NullOrUndefined (Number)
  }


newtype DeleteDirectoryConfigRequest = DeleteDirectoryConfigRequest 
  { "DirectoryName" :: (DirectoryName)
  }


newtype DeleteDirectoryConfigResult = DeleteDirectoryConfigResult 
  { 
  }


newtype DeleteFleetRequest = DeleteFleetRequest 
  { "Name" :: (String)
  }


newtype DeleteFleetResult = DeleteFleetResult 
  { 
  }


newtype DeleteImageBuilderRequest = DeleteImageBuilderRequest 
  { "Name" :: (Name)
  }


newtype DeleteImageBuilderResult = DeleteImageBuilderResult 
  { "ImageBuilder" :: NullOrUndefined (ImageBuilder)
  }


newtype DeleteImageRequest = DeleteImageRequest 
  { "Name" :: (Name)
  }


newtype DeleteImageResult = DeleteImageResult 
  { "Image" :: NullOrUndefined (Image)
  }


newtype DeleteStackRequest = DeleteStackRequest 
  { "Name" :: (String)
  }


newtype DeleteStackResult = DeleteStackResult 
  { 
  }


newtype DescribeDirectoryConfigsRequest = DescribeDirectoryConfigsRequest 
  { "DirectoryNames" :: NullOrUndefined (DirectoryNameList)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeDirectoryConfigsResult = DescribeDirectoryConfigsResult 
  { "DirectoryConfigs" :: NullOrUndefined (DirectoryConfigList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeFleetsRequest = DescribeFleetsRequest 
  { "Names" :: NullOrUndefined (StringList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeFleetsResult = DescribeFleetsResult 
  { "Fleets" :: NullOrUndefined (FleetList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeImageBuildersRequest = DescribeImageBuildersRequest 
  { "Names" :: NullOrUndefined (StringList)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeImageBuildersResult = DescribeImageBuildersResult 
  { "ImageBuilders" :: NullOrUndefined (ImageBuilderList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeImagesRequest = DescribeImagesRequest 
  { "Names" :: NullOrUndefined (StringList)
  }


newtype DescribeImagesResult = DescribeImagesResult 
  { "Images" :: NullOrUndefined (ImageList)
  }


newtype DescribeSessionsRequest = DescribeSessionsRequest 
  { "StackName" :: (String)
  , "FleetName" :: (String)
  , "UserId" :: NullOrUndefined (UserId)
  , "NextToken" :: NullOrUndefined (String)
  , "Limit" :: NullOrUndefined (Int)
  , "AuthenticationType" :: NullOrUndefined (AuthenticationType)
  }


newtype DescribeSessionsResult = DescribeSessionsResult 
  { "Sessions" :: NullOrUndefined (SessionList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeStacksRequest = DescribeStacksRequest 
  { "Names" :: NullOrUndefined (StringList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype DescribeStacksResult = DescribeStacksResult 
  { "Stacks" :: NullOrUndefined (StackList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype Description = Description String


-- | <p>Configuration information for the directory used to join domains.</p>
newtype DirectoryConfig = DirectoryConfig 
  { "DirectoryName" :: (DirectoryName)
  , "OrganizationalUnitDistinguishedNames" :: NullOrUndefined (OrganizationalUnitDistinguishedNamesList)
  , "ServiceAccountCredentials" :: NullOrUndefined (ServiceAccountCredentials)
  , "CreatedTime" :: NullOrUndefined (Number)
  }


newtype DirectoryConfigList = DirectoryConfigList (Array DirectoryConfig)


newtype DirectoryName = DirectoryName String


newtype DirectoryNameList = DirectoryNameList (Array DirectoryName)


newtype DisassociateFleetRequest = DisassociateFleetRequest 
  { "FleetName" :: (String)
  , "StackName" :: (String)
  }


newtype DisassociateFleetResult = DisassociateFleetResult 
  { 
  }


newtype DisplayName = DisplayName String


-- | <p>Contains the information needed to join a Microsoft Active Directory domain.</p>
newtype DomainJoinInfo = DomainJoinInfo 
  { "DirectoryName" :: NullOrUndefined (DirectoryName)
  , "OrganizationalUnitDistinguishedName" :: NullOrUndefined (OrganizationalUnitDistinguishedName)
  }


-- | <p>The error message in the exception.</p>
newtype ErrorMessage = ErrorMessage String


newtype ExpireSessionRequest = ExpireSessionRequest 
  { "SessionId" :: (String)
  }


newtype ExpireSessionResult = ExpireSessionResult 
  { 
  }


-- | <p>Contains the parameters for a fleet.</p>
newtype Fleet = Fleet 
  { "Arn" :: (Arn)
  , "Name" :: (String)
  , "DisplayName" :: NullOrUndefined (String)
  , "Description" :: NullOrUndefined (String)
  , "ImageName" :: (String)
  , "InstanceType" :: (String)
  , "FleetType" :: NullOrUndefined (FleetType)
  , "ComputeCapacityStatus" :: (ComputeCapacityStatus)
  , "MaxUserDurationInSeconds" :: NullOrUndefined (Int)
  , "DisconnectTimeoutInSeconds" :: NullOrUndefined (Int)
  , "State" :: (FleetState)
  , "VpcConfig" :: NullOrUndefined (VpcConfig)
  , "CreatedTime" :: NullOrUndefined (Number)
  , "FleetErrors" :: NullOrUndefined (FleetErrors)
  , "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject)
  , "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo)
  }


-- | <p>The fleet attribute.</p>
newtype FleetAttribute = FleetAttribute String


-- | <p>The fleet attributes.</p>
newtype FleetAttributes = FleetAttributes (Array FleetAttribute)


-- | <p>Describes a fleet error.</p>
newtype FleetError = FleetError 
  { "ErrorCode" :: NullOrUndefined (FleetErrorCode)
  , "ErrorMessage" :: NullOrUndefined (String)
  }


newtype FleetErrorCode = FleetErrorCode String


newtype FleetErrors = FleetErrors (Array FleetError)


-- | <p>The fleets.</p>
newtype FleetList = FleetList (Array Fleet)


newtype FleetState = FleetState String


newtype FleetType = FleetType String


-- | <p>Describes an image.</p>
newtype Image = Image 
  { "Name" :: (String)
  , "Arn" :: NullOrUndefined (Arn)
  , "BaseImageArn" :: NullOrUndefined (Arn)
  , "DisplayName" :: NullOrUndefined (String)
  , "State" :: NullOrUndefined (ImageState)
  , "Visibility" :: NullOrUndefined (VisibilityType)
  , "ImageBuilderSupported" :: NullOrUndefined (Boolean)
  , "Platform" :: NullOrUndefined (PlatformType)
  , "Description" :: NullOrUndefined (String)
  , "StateChangeReason" :: NullOrUndefined (ImageStateChangeReason)
  , "Applications" :: NullOrUndefined (Applications)
  , "CreatedTime" :: NullOrUndefined (Number)
  , "PublicBaseImageReleasedDate" :: NullOrUndefined (Number)
  , "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion)
  }


-- | <p>Describes a streaming instance used for editing an image. New images are created from a snapshot through an image builder.</p>
newtype ImageBuilder = ImageBuilder 
  { "Name" :: (String)
  , "Arn" :: NullOrUndefined (Arn)
  , "ImageArn" :: NullOrUndefined (Arn)
  , "Description" :: NullOrUndefined (String)
  , "DisplayName" :: NullOrUndefined (String)
  , "VpcConfig" :: NullOrUndefined (VpcConfig)
  , "InstanceType" :: NullOrUndefined (String)
  , "Platform" :: NullOrUndefined (PlatformType)
  , "State" :: NullOrUndefined (ImageBuilderState)
  , "StateChangeReason" :: NullOrUndefined (ImageBuilderStateChangeReason)
  , "CreatedTime" :: NullOrUndefined (Number)
  , "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject)
  , "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo)
  , "ImageBuilderErrors" :: NullOrUndefined (ResourceErrors)
  , "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion)
  }


newtype ImageBuilderList = ImageBuilderList (Array ImageBuilder)


newtype ImageBuilderState = ImageBuilderState String


-- | <p>Describes the reason why the last image builder state change occurred.</p>
newtype ImageBuilderStateChangeReason = ImageBuilderStateChangeReason 
  { "Code" :: NullOrUndefined (ImageBuilderStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }


newtype ImageBuilderStateChangeReasonCode = ImageBuilderStateChangeReasonCode String


newtype ImageList = ImageList (Array Image)


newtype ImageState = ImageState String


-- | <p>Describes the reason why the last image state change occurred.</p>
newtype ImageStateChangeReason = ImageStateChangeReason 
  { "Code" :: NullOrUndefined (ImageStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }


newtype ImageStateChangeReasonCode = ImageStateChangeReasonCode String


-- | <p>The image does not support storage connectors.</p>
newtype IncompatibleImageException = IncompatibleImageException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Indicates an incorrect combination of parameters, or a missing parameter.</p>
newtype InvalidParameterCombinationException = InvalidParameterCombinationException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified role is invalid.</p>
newtype InvalidRoleException = InvalidRoleException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The requested limit exceeds the permitted limit for an account.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype ListAssociatedFleetsRequest = ListAssociatedFleetsRequest 
  { "StackName" :: (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListAssociatedFleetsResult = ListAssociatedFleetsResult 
  { "Names" :: NullOrUndefined (StringList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListAssociatedStacksRequest = ListAssociatedStacksRequest 
  { "FleetName" :: (String)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListAssociatedStacksResult = ListAssociatedStacksResult 
  { "Names" :: NullOrUndefined (StringList)
  , "NextToken" :: NullOrUndefined (String)
  }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceArn" :: (Arn)
  }


newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "Tags" :: NullOrUndefined (Tags)
  }


newtype Metadata = Metadata (Map String String)


newtype Name = Name String


-- | <p>The attempted operation is not permitted.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


newtype OrganizationalUnitDistinguishedName = OrganizationalUnitDistinguishedName String


newtype OrganizationalUnitDistinguishedNamesList = OrganizationalUnitDistinguishedNamesList (Array OrganizationalUnitDistinguishedName)


newtype PlatformType = PlatformType String


newtype RedirectURL = RedirectURL String


newtype RegionName = RegionName String


-- | <p>The specified resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>Describes a resource error.</p>
newtype ResourceError = ResourceError 
  { "ErrorCode" :: NullOrUndefined (FleetErrorCode)
  , "ErrorMessage" :: NullOrUndefined (String)
  , "ErrorTimestamp" :: NullOrUndefined (Number)
  }


newtype ResourceErrors = ResourceErrors (Array ResourceError)


-- | <p>The ARN of the resource.</p>
newtype ResourceIdentifier = ResourceIdentifier String


-- | <p>The specified resource is in use.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified resource exists and is not in use, but isn't available.</p>
newtype ResourceNotAvailableException = ResourceNotAvailableException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The specified resource was not found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }


-- | <p>The security group IDs.</p>
newtype SecurityGroupIdList = SecurityGroupIdList (Array String)


-- | <p>Describes the credentials for the service account used by the streaming instance to connect to the directory.</p>
newtype ServiceAccountCredentials = ServiceAccountCredentials 
  { "AccountName" :: (AccountName)
  , "AccountPassword" :: (AccountPassword)
  }


-- | <p>Describes a streaming session.</p>
newtype Session = Session 
  { "Id" :: (String)
  , "UserId" :: (UserId)
  , "StackName" :: (String)
  , "FleetName" :: (String)
  , "State" :: (SessionState)
  , "AuthenticationType" :: NullOrUndefined (AuthenticationType)
  }


-- | <p>List of sessions.</p>
newtype SessionList = SessionList (Array Session)


-- | <p>Possible values for the state of a streaming session.</p>
newtype SessionState = SessionState String


-- | <p>Describes a stack.</p>
newtype Stack = Stack 
  { "Arn" :: NullOrUndefined (Arn)
  , "Name" :: (String)
  , "Description" :: NullOrUndefined (String)
  , "DisplayName" :: NullOrUndefined (String)
  , "CreatedTime" :: NullOrUndefined (Number)
  , "StorageConnectors" :: NullOrUndefined (StorageConnectorList)
  , "RedirectURL" :: NullOrUndefined (RedirectURL)
  , "StackErrors" :: NullOrUndefined (StackErrors)
  }


newtype StackAttribute = StackAttribute String


newtype StackAttributes = StackAttributes (Array StackAttribute)


-- | <p>Describes a stack error.</p>
newtype StackError = StackError 
  { "ErrorCode" :: NullOrUndefined (StackErrorCode)
  , "ErrorMessage" :: NullOrUndefined (String)
  }


newtype StackErrorCode = StackErrorCode String


-- | <p>The stack errors.</p>
newtype StackErrors = StackErrors (Array StackError)


-- | <p>The stacks.</p>
newtype StackList = StackList (Array Stack)


newtype StartFleetRequest = StartFleetRequest 
  { "Name" :: (String)
  }


newtype StartFleetResult = StartFleetResult 
  { 
  }


newtype StartImageBuilderRequest = StartImageBuilderRequest 
  { "Name" :: (String)
  , "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion)
  }


newtype StartImageBuilderResult = StartImageBuilderResult 
  { "ImageBuilder" :: NullOrUndefined (ImageBuilder)
  }


newtype StopFleetRequest = StopFleetRequest 
  { "Name" :: (String)
  }


newtype StopFleetResult = StopFleetResult 
  { 
  }


newtype StopImageBuilderRequest = StopImageBuilderRequest 
  { "Name" :: (String)
  }


newtype StopImageBuilderResult = StopImageBuilderResult 
  { "ImageBuilder" :: NullOrUndefined (ImageBuilder)
  }


-- | <p>Describes a storage connector.</p>
newtype StorageConnector = StorageConnector 
  { "ConnectorType" :: (StorageConnectorType)
  , "ResourceIdentifier" :: NullOrUndefined (ResourceIdentifier)
  }


-- | <p>The storage connectors.</p>
newtype StorageConnectorList = StorageConnectorList (Array StorageConnector)


-- | <p>The type of storage connector.</p>
newtype StorageConnectorType = StorageConnectorType String


newtype StreamingUrlUserId = StreamingUrlUserId String


newtype StringList = StringList (Array String)


-- | <p>The subnet IDs.</p>
newtype SubnetIdList = SubnetIdList (Array String)


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array TagKey)


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "Tags" :: (Tags)
  }


newtype TagResourceResponse = TagResourceResponse 
  { 
  }


newtype TagValue = TagValue String


newtype Tags = Tags (Map TagKey TagValue)


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "TagKeys" :: (TagKeyList)
  }


newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }


newtype UpdateDirectoryConfigRequest = UpdateDirectoryConfigRequest 
  { "DirectoryName" :: (DirectoryName)
  , "OrganizationalUnitDistinguishedNames" :: NullOrUndefined (OrganizationalUnitDistinguishedNamesList)
  , "ServiceAccountCredentials" :: NullOrUndefined (ServiceAccountCredentials)
  }


newtype UpdateDirectoryConfigResult = UpdateDirectoryConfigResult 
  { "DirectoryConfig" :: NullOrUndefined (DirectoryConfig)
  }


newtype UpdateFleetRequest = UpdateFleetRequest 
  { "ImageName" :: NullOrUndefined (String)
  , "Name" :: (String)
  , "InstanceType" :: NullOrUndefined (String)
  , "ComputeCapacity" :: NullOrUndefined (ComputeCapacity)
  , "VpcConfig" :: NullOrUndefined (VpcConfig)
  , "MaxUserDurationInSeconds" :: NullOrUndefined (Int)
  , "DisconnectTimeoutInSeconds" :: NullOrUndefined (Int)
  , "DeleteVpcConfig" :: NullOrUndefined (Boolean)
  , "Description" :: NullOrUndefined (Description)
  , "DisplayName" :: NullOrUndefined (DisplayName)
  , "EnableDefaultInternetAccess" :: NullOrUndefined (BooleanObject)
  , "DomainJoinInfo" :: NullOrUndefined (DomainJoinInfo)
  , "AttributesToDelete" :: NullOrUndefined (FleetAttributes)
  }


newtype UpdateFleetResult = UpdateFleetResult 
  { "Fleet" :: NullOrUndefined (Fleet)
  }


newtype UpdateStackRequest = UpdateStackRequest 
  { "DisplayName" :: NullOrUndefined (DisplayName)
  , "Description" :: NullOrUndefined (Description)
  , "Name" :: (String)
  , "StorageConnectors" :: NullOrUndefined (StorageConnectorList)
  , "DeleteStorageConnectors" :: NullOrUndefined (Boolean)
  , "RedirectURL" :: NullOrUndefined (RedirectURL)
  , "AttributesToDelete" :: NullOrUndefined (StackAttributes)
  }


newtype UpdateStackResult = UpdateStackResult 
  { "Stack" :: NullOrUndefined (Stack)
  }


newtype UserId = UserId String


newtype VisibilityType = VisibilityType String


-- | <p>Describes VPC configuration information.</p>
newtype VpcConfig = VpcConfig 
  { "SubnetIds" :: NullOrUndefined (SubnetIdList)
  , "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdList)
  }
