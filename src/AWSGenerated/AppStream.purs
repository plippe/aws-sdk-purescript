

-- | <fullname>Amazon AppStream 2.0</fullname> <p>You can use Amazon AppStream 2.0 to stream desktop applications to any device running a web browser, without rewriting them.</p>
module AWS.AppStream where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "AppStream" :: String


-- | <p>Associates the specified fleet with the specified stack.</p>
associateFleet :: forall eff. AssociateFleetRequest -> Aff (err :: AWS.RequestError | eff) AssociateFleetResult
associateFleet = AWS.request serviceName "associateFleet" 


-- | <p>Copies the image within the same region or to a new region within the same AWS account. Note that any tags you added to the image will not be copied.</p>
copyImage :: forall eff. CopyImageRequest -> Aff (err :: AWS.RequestError | eff) CopyImageResponse
copyImage = AWS.request serviceName "copyImage" 


-- | <p>Creates a directory configuration.</p>
createDirectoryConfig :: forall eff. CreateDirectoryConfigRequest -> Aff (err :: AWS.RequestError | eff) CreateDirectoryConfigResult
createDirectoryConfig = AWS.request serviceName "createDirectoryConfig" 


-- | <p>Creates a fleet.</p>
createFleet :: forall eff. CreateFleetRequest -> Aff (err :: AWS.RequestError | eff) CreateFleetResult
createFleet = AWS.request serviceName "createFleet" 


-- | <p>Creates an image builder.</p> <p>The initial state of the builder is <code>PENDING</code>. When it is ready, the state is <code>RUNNING</code>.</p>
createImageBuilder :: forall eff. CreateImageBuilderRequest -> Aff (err :: AWS.RequestError | eff) CreateImageBuilderResult
createImageBuilder = AWS.request serviceName "createImageBuilder" 


-- | <p>Creates a URL to start an image builder streaming session.</p>
createImageBuilderStreamingURL :: forall eff. CreateImageBuilderStreamingURLRequest -> Aff (err :: AWS.RequestError | eff) CreateImageBuilderStreamingURLResult
createImageBuilderStreamingURL = AWS.request serviceName "createImageBuilderStreamingURL" 


-- | <p>Creates a stack.</p>
createStack :: forall eff. CreateStackRequest -> Aff (err :: AWS.RequestError | eff) CreateStackResult
createStack = AWS.request serviceName "createStack" 


-- | <p>Creates a URL to start a streaming session for the specified user.</p>
createStreamingURL :: forall eff. CreateStreamingURLRequest -> Aff (err :: AWS.RequestError | eff) CreateStreamingURLResult
createStreamingURL = AWS.request serviceName "createStreamingURL" 


-- | <p>Deletes the specified directory configuration.</p>
deleteDirectoryConfig :: forall eff. DeleteDirectoryConfigRequest -> Aff (err :: AWS.RequestError | eff) DeleteDirectoryConfigResult
deleteDirectoryConfig = AWS.request serviceName "deleteDirectoryConfig" 


-- | <p>Deletes the specified fleet.</p>
deleteFleet :: forall eff. DeleteFleetRequest -> Aff (err :: AWS.RequestError | eff) DeleteFleetResult
deleteFleet = AWS.request serviceName "deleteFleet" 


-- | <p>Deletes the specified image. You cannot delete an image that is currently in use. After you delete an image, you cannot provision new capacity using the image.</p>
deleteImage :: forall eff. DeleteImageRequest -> Aff (err :: AWS.RequestError | eff) DeleteImageResult
deleteImage = AWS.request serviceName "deleteImage" 


-- | <p>Deletes the specified image builder and releases the capacity.</p>
deleteImageBuilder :: forall eff. DeleteImageBuilderRequest -> Aff (err :: AWS.RequestError | eff) DeleteImageBuilderResult
deleteImageBuilder = AWS.request serviceName "deleteImageBuilder" 


-- | <p>Deletes the specified stack. After this operation completes, the environment can no longer be activated and any reservations made for the stack are released.</p>
deleteStack :: forall eff. DeleteStackRequest -> Aff (err :: AWS.RequestError | eff) DeleteStackResult
deleteStack = AWS.request serviceName "deleteStack" 


-- | <p>Describes the specified directory configurations. Note that although the response syntax in this topic includes the account password, this password is not returned in the actual response. </p>
describeDirectoryConfigs :: forall eff. DescribeDirectoryConfigsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDirectoryConfigsResult
describeDirectoryConfigs = AWS.request serviceName "describeDirectoryConfigs" 


-- | <p>Describes the specified fleets or all fleets in the account.</p>
describeFleets :: forall eff. DescribeFleetsRequest -> Aff (err :: AWS.RequestError | eff) DescribeFleetsResult
describeFleets = AWS.request serviceName "describeFleets" 


-- | <p>Describes the specified image builders or all image builders in the account.</p>
describeImageBuilders :: forall eff. DescribeImageBuildersRequest -> Aff (err :: AWS.RequestError | eff) DescribeImageBuildersResult
describeImageBuilders = AWS.request serviceName "describeImageBuilders" 


-- | <p>Describes the specified images or all images in the account.</p>
describeImages :: forall eff. DescribeImagesRequest -> Aff (err :: AWS.RequestError | eff) DescribeImagesResult
describeImages = AWS.request serviceName "describeImages" 


-- | <p>Describes the streaming sessions for the specified stack and fleet. If a user ID is provided, only the streaming sessions for only that user are returned. If an authentication type is not provided, the default is to authenticate users using a streaming URL.</p>
describeSessions :: forall eff. DescribeSessionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeSessionsResult
describeSessions = AWS.request serviceName "describeSessions" 


-- | <p>Describes the specified stacks or all stacks in the account.</p>
describeStacks :: forall eff. DescribeStacksRequest -> Aff (err :: AWS.RequestError | eff) DescribeStacksResult
describeStacks = AWS.request serviceName "describeStacks" 


-- | <p>Disassociates the specified fleet from the specified stack.</p>
disassociateFleet :: forall eff. DisassociateFleetRequest -> Aff (err :: AWS.RequestError | eff) DisassociateFleetResult
disassociateFleet = AWS.request serviceName "disassociateFleet" 


-- | <p>Stops the specified streaming session.</p>
expireSession :: forall eff. ExpireSessionRequest -> Aff (err :: AWS.RequestError | eff) ExpireSessionResult
expireSession = AWS.request serviceName "expireSession" 


-- | <p>Lists the fleets associated with the specified stack.</p>
listAssociatedFleets :: forall eff. ListAssociatedFleetsRequest -> Aff (err :: AWS.RequestError | eff) ListAssociatedFleetsResult
listAssociatedFleets = AWS.request serviceName "listAssociatedFleets" 


-- | <p>Lists the stacks associated with the specified fleet.</p>
listAssociatedStacks :: forall eff. ListAssociatedStacksRequest -> Aff (err :: AWS.RequestError | eff) ListAssociatedStacksResult
listAssociatedStacks = AWS.request serviceName "listAssociatedStacks" 


-- | <p>Lists the tags for the specified AppStream 2.0 resource. You can tag AppStream 2.0 image builders, images, fleets, and stacks.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResponse
listTagsForResource = AWS.request serviceName "listTagsForResource" 


-- | <p>Starts the specified fleet.</p>
startFleet :: forall eff. StartFleetRequest -> Aff (err :: AWS.RequestError | eff) StartFleetResult
startFleet = AWS.request serviceName "startFleet" 


-- | <p>Starts the specified image builder.</p>
startImageBuilder :: forall eff. StartImageBuilderRequest -> Aff (err :: AWS.RequestError | eff) StartImageBuilderResult
startImageBuilder = AWS.request serviceName "startImageBuilder" 


-- | <p>Stops the specified fleet.</p>
stopFleet :: forall eff. StopFleetRequest -> Aff (err :: AWS.RequestError | eff) StopFleetResult
stopFleet = AWS.request serviceName "stopFleet" 


-- | <p>Stops the specified image builder.</p>
stopImageBuilder :: forall eff. StopImageBuilderRequest -> Aff (err :: AWS.RequestError | eff) StopImageBuilderResult
stopImageBuilder = AWS.request serviceName "stopImageBuilder" 


-- | <p>Adds or overwrites one or more tags for the specified AppStream 2.0 resource. You can tag AppStream 2.0 image builders, images, fleets, and stacks.</p> <p>Each tag consists of a key and an optional value. If a resource already has a tag with the same key, this operation updates its value.</p> <p>To list the current tags for your resources, use <a>ListTagsForResource</a>. To disassociate tags from your resources, use <a>UntagResource</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>
tagResource :: forall eff. TagResourceRequest -> Aff (err :: AWS.RequestError | eff) TagResourceResponse
tagResource = AWS.request serviceName "tagResource" 


-- | <p>Disassociates the specified tags from the specified AppStream 2.0 resource.</p> <p>To list the current tags for your resources, use <a>ListTagsForResource</a>.</p> <p>For more information about tags, see <a href="http://docs.aws.amazon.com/appstream2/latest/developerguide/tagging-basic.html">Tagging Your Resources</a> in the <i>Amazon AppStream 2.0 Developer Guide</i>.</p>
untagResource :: forall eff. UntagResourceRequest -> Aff (err :: AWS.RequestError | eff) UntagResourceResponse
untagResource = AWS.request serviceName "untagResource" 


-- | <p>Updates the specified directory configuration.</p>
updateDirectoryConfig :: forall eff. UpdateDirectoryConfigRequest -> Aff (err :: AWS.RequestError | eff) UpdateDirectoryConfigResult
updateDirectoryConfig = AWS.request serviceName "updateDirectoryConfig" 


-- | <p>Updates the specified fleet.</p> <p>If the fleet is in the <code>STOPPED</code> state, you can update any attribute except the fleet name. If the fleet is in the <code>RUNNING</code> state, you can update the <code>DisplayName</code> and <code>ComputeCapacity</code> attributes. If the fleet is in the <code>STARTING</code> or <code>STOPPING</code> state, you can't update it.</p>
updateFleet :: forall eff. UpdateFleetRequest -> Aff (err :: AWS.RequestError | eff) UpdateFleetResult
updateFleet = AWS.request serviceName "updateFleet" 


-- | <p>Updates the specified stack.</p>
updateStack :: forall eff. UpdateStackRequest -> Aff (err :: AWS.RequestError | eff) UpdateStackResult
updateStack = AWS.request serviceName "updateStack" 


newtype AccountName = AccountName String
derive instance newtypeAccountName :: Newtype AccountName _


newtype AccountPassword = AccountPassword String
derive instance newtypeAccountPassword :: Newtype AccountPassword _


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
derive instance newtypeApplication :: Newtype Application _


newtype Applications = Applications (Array Application)
derive instance newtypeApplications :: Newtype Applications _


newtype AppstreamAgentVersion = AppstreamAgentVersion String
derive instance newtypeAppstreamAgentVersion :: Newtype AppstreamAgentVersion _


newtype Arn = Arn String
derive instance newtypeArn :: Newtype Arn _


newtype AssociateFleetRequest = AssociateFleetRequest 
  { "FleetName" :: (String)
  , "StackName" :: (String)
  }
derive instance newtypeAssociateFleetRequest :: Newtype AssociateFleetRequest _


newtype AssociateFleetResult = AssociateFleetResult 
  { 
  }
derive instance newtypeAssociateFleetResult :: Newtype AssociateFleetResult _


newtype AuthenticationType = AuthenticationType String
derive instance newtypeAuthenticationType :: Newtype AuthenticationType _


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _


-- | <p>Describes the capacity for a fleet.</p>
newtype ComputeCapacity = ComputeCapacity 
  { "DesiredInstances" :: (Int)
  }
derive instance newtypeComputeCapacity :: Newtype ComputeCapacity _


-- | <p>Describes the capacity status for a fleet.</p>
newtype ComputeCapacityStatus = ComputeCapacityStatus 
  { "Desired" :: (Int)
  , "Running" :: NullOrUndefined (Int)
  , "InUse" :: NullOrUndefined (Int)
  , "Available" :: NullOrUndefined (Int)
  }
derive instance newtypeComputeCapacityStatus :: Newtype ComputeCapacityStatus _


-- | <p>An API error occurred. Wait a few minutes and try again.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


newtype CopyImageRequest = CopyImageRequest 
  { "SourceImageName" :: (Name)
  , "DestinationImageName" :: (Name)
  , "DestinationRegion" :: (RegionName)
  , "DestinationImageDescription" :: NullOrUndefined (Description)
  }
derive instance newtypeCopyImageRequest :: Newtype CopyImageRequest _


newtype CopyImageResponse = CopyImageResponse 
  { "DestinationImageName" :: NullOrUndefined (Name)
  }
derive instance newtypeCopyImageResponse :: Newtype CopyImageResponse _


newtype CreateDirectoryConfigRequest = CreateDirectoryConfigRequest 
  { "DirectoryName" :: (DirectoryName)
  , "OrganizationalUnitDistinguishedNames" :: (OrganizationalUnitDistinguishedNamesList)
  , "ServiceAccountCredentials" :: (ServiceAccountCredentials)
  }
derive instance newtypeCreateDirectoryConfigRequest :: Newtype CreateDirectoryConfigRequest _


newtype CreateDirectoryConfigResult = CreateDirectoryConfigResult 
  { "DirectoryConfig" :: NullOrUndefined (DirectoryConfig)
  }
derive instance newtypeCreateDirectoryConfigResult :: Newtype CreateDirectoryConfigResult _


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
derive instance newtypeCreateFleetRequest :: Newtype CreateFleetRequest _


newtype CreateFleetResult = CreateFleetResult 
  { "Fleet" :: NullOrUndefined (Fleet)
  }
derive instance newtypeCreateFleetResult :: Newtype CreateFleetResult _


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
derive instance newtypeCreateImageBuilderRequest :: Newtype CreateImageBuilderRequest _


newtype CreateImageBuilderResult = CreateImageBuilderResult 
  { "ImageBuilder" :: NullOrUndefined (ImageBuilder)
  }
derive instance newtypeCreateImageBuilderResult :: Newtype CreateImageBuilderResult _


newtype CreateImageBuilderStreamingURLRequest = CreateImageBuilderStreamingURLRequest 
  { "Name" :: (String)
  , "Validity" :: NullOrUndefined (Number)
  }
derive instance newtypeCreateImageBuilderStreamingURLRequest :: Newtype CreateImageBuilderStreamingURLRequest _


newtype CreateImageBuilderStreamingURLResult = CreateImageBuilderStreamingURLResult 
  { "StreamingURL" :: NullOrUndefined (String)
  , "Expires" :: NullOrUndefined (Number)
  }
derive instance newtypeCreateImageBuilderStreamingURLResult :: Newtype CreateImageBuilderStreamingURLResult _


newtype CreateStackRequest = CreateStackRequest 
  { "Name" :: (String)
  , "Description" :: NullOrUndefined (Description)
  , "DisplayName" :: NullOrUndefined (DisplayName)
  , "StorageConnectors" :: NullOrUndefined (StorageConnectorList)
  , "RedirectURL" :: NullOrUndefined (RedirectURL)
  }
derive instance newtypeCreateStackRequest :: Newtype CreateStackRequest _


newtype CreateStackResult = CreateStackResult 
  { "Stack" :: NullOrUndefined (Stack)
  }
derive instance newtypeCreateStackResult :: Newtype CreateStackResult _


newtype CreateStreamingURLRequest = CreateStreamingURLRequest 
  { "StackName" :: (String)
  , "FleetName" :: (String)
  , "UserId" :: (StreamingUrlUserId)
  , "ApplicationId" :: NullOrUndefined (String)
  , "Validity" :: NullOrUndefined (Number)
  , "SessionContext" :: NullOrUndefined (String)
  }
derive instance newtypeCreateStreamingURLRequest :: Newtype CreateStreamingURLRequest _


newtype CreateStreamingURLResult = CreateStreamingURLResult 
  { "StreamingURL" :: NullOrUndefined (String)
  , "Expires" :: NullOrUndefined (Number)
  }
derive instance newtypeCreateStreamingURLResult :: Newtype CreateStreamingURLResult _


newtype DeleteDirectoryConfigRequest = DeleteDirectoryConfigRequest 
  { "DirectoryName" :: (DirectoryName)
  }
derive instance newtypeDeleteDirectoryConfigRequest :: Newtype DeleteDirectoryConfigRequest _


newtype DeleteDirectoryConfigResult = DeleteDirectoryConfigResult 
  { 
  }
derive instance newtypeDeleteDirectoryConfigResult :: Newtype DeleteDirectoryConfigResult _


newtype DeleteFleetRequest = DeleteFleetRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeleteFleetRequest :: Newtype DeleteFleetRequest _


newtype DeleteFleetResult = DeleteFleetResult 
  { 
  }
derive instance newtypeDeleteFleetResult :: Newtype DeleteFleetResult _


newtype DeleteImageBuilderRequest = DeleteImageBuilderRequest 
  { "Name" :: (Name)
  }
derive instance newtypeDeleteImageBuilderRequest :: Newtype DeleteImageBuilderRequest _


newtype DeleteImageBuilderResult = DeleteImageBuilderResult 
  { "ImageBuilder" :: NullOrUndefined (ImageBuilder)
  }
derive instance newtypeDeleteImageBuilderResult :: Newtype DeleteImageBuilderResult _


newtype DeleteImageRequest = DeleteImageRequest 
  { "Name" :: (Name)
  }
derive instance newtypeDeleteImageRequest :: Newtype DeleteImageRequest _


newtype DeleteImageResult = DeleteImageResult 
  { "Image" :: NullOrUndefined (Image)
  }
derive instance newtypeDeleteImageResult :: Newtype DeleteImageResult _


newtype DeleteStackRequest = DeleteStackRequest 
  { "Name" :: (String)
  }
derive instance newtypeDeleteStackRequest :: Newtype DeleteStackRequest _


newtype DeleteStackResult = DeleteStackResult 
  { 
  }
derive instance newtypeDeleteStackResult :: Newtype DeleteStackResult _


newtype DescribeDirectoryConfigsRequest = DescribeDirectoryConfigsRequest 
  { "DirectoryNames" :: NullOrUndefined (DirectoryNameList)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDirectoryConfigsRequest :: Newtype DescribeDirectoryConfigsRequest _


newtype DescribeDirectoryConfigsResult = DescribeDirectoryConfigsResult 
  { "DirectoryConfigs" :: NullOrUndefined (DirectoryConfigList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeDirectoryConfigsResult :: Newtype DescribeDirectoryConfigsResult _


newtype DescribeFleetsRequest = DescribeFleetsRequest 
  { "Names" :: NullOrUndefined (StringList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeFleetsRequest :: Newtype DescribeFleetsRequest _


newtype DescribeFleetsResult = DescribeFleetsResult 
  { "Fleets" :: NullOrUndefined (FleetList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeFleetsResult :: Newtype DescribeFleetsResult _


newtype DescribeImageBuildersRequest = DescribeImageBuildersRequest 
  { "Names" :: NullOrUndefined (StringList)
  , "MaxResults" :: NullOrUndefined (Int)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeImageBuildersRequest :: Newtype DescribeImageBuildersRequest _


newtype DescribeImageBuildersResult = DescribeImageBuildersResult 
  { "ImageBuilders" :: NullOrUndefined (ImageBuilderList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeImageBuildersResult :: Newtype DescribeImageBuildersResult _


newtype DescribeImagesRequest = DescribeImagesRequest 
  { "Names" :: NullOrUndefined (StringList)
  }
derive instance newtypeDescribeImagesRequest :: Newtype DescribeImagesRequest _


newtype DescribeImagesResult = DescribeImagesResult 
  { "Images" :: NullOrUndefined (ImageList)
  }
derive instance newtypeDescribeImagesResult :: Newtype DescribeImagesResult _


newtype DescribeSessionsRequest = DescribeSessionsRequest 
  { "StackName" :: (String)
  , "FleetName" :: (String)
  , "UserId" :: NullOrUndefined (UserId)
  , "NextToken" :: NullOrUndefined (String)
  , "Limit" :: NullOrUndefined (Int)
  , "AuthenticationType" :: NullOrUndefined (AuthenticationType)
  }
derive instance newtypeDescribeSessionsRequest :: Newtype DescribeSessionsRequest _


newtype DescribeSessionsResult = DescribeSessionsResult 
  { "Sessions" :: NullOrUndefined (SessionList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeSessionsResult :: Newtype DescribeSessionsResult _


newtype DescribeStacksRequest = DescribeStacksRequest 
  { "Names" :: NullOrUndefined (StringList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeStacksRequest :: Newtype DescribeStacksRequest _


newtype DescribeStacksResult = DescribeStacksResult 
  { "Stacks" :: NullOrUndefined (StackList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeDescribeStacksResult :: Newtype DescribeStacksResult _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


-- | <p>Configuration information for the directory used to join domains.</p>
newtype DirectoryConfig = DirectoryConfig 
  { "DirectoryName" :: (DirectoryName)
  , "OrganizationalUnitDistinguishedNames" :: NullOrUndefined (OrganizationalUnitDistinguishedNamesList)
  , "ServiceAccountCredentials" :: NullOrUndefined (ServiceAccountCredentials)
  , "CreatedTime" :: NullOrUndefined (Number)
  }
derive instance newtypeDirectoryConfig :: Newtype DirectoryConfig _


newtype DirectoryConfigList = DirectoryConfigList (Array DirectoryConfig)
derive instance newtypeDirectoryConfigList :: Newtype DirectoryConfigList _


newtype DirectoryName = DirectoryName String
derive instance newtypeDirectoryName :: Newtype DirectoryName _


newtype DirectoryNameList = DirectoryNameList (Array DirectoryName)
derive instance newtypeDirectoryNameList :: Newtype DirectoryNameList _


newtype DisassociateFleetRequest = DisassociateFleetRequest 
  { "FleetName" :: (String)
  , "StackName" :: (String)
  }
derive instance newtypeDisassociateFleetRequest :: Newtype DisassociateFleetRequest _


newtype DisassociateFleetResult = DisassociateFleetResult 
  { 
  }
derive instance newtypeDisassociateFleetResult :: Newtype DisassociateFleetResult _


newtype DisplayName = DisplayName String
derive instance newtypeDisplayName :: Newtype DisplayName _


-- | <p>Contains the information needed to join a Microsoft Active Directory domain.</p>
newtype DomainJoinInfo = DomainJoinInfo 
  { "DirectoryName" :: NullOrUndefined (DirectoryName)
  , "OrganizationalUnitDistinguishedName" :: NullOrUndefined (OrganizationalUnitDistinguishedName)
  }
derive instance newtypeDomainJoinInfo :: Newtype DomainJoinInfo _


-- | <p>The error message in the exception.</p>
newtype ErrorMessage = ErrorMessage String
derive instance newtypeErrorMessage :: Newtype ErrorMessage _


newtype ExpireSessionRequest = ExpireSessionRequest 
  { "SessionId" :: (String)
  }
derive instance newtypeExpireSessionRequest :: Newtype ExpireSessionRequest _


newtype ExpireSessionResult = ExpireSessionResult 
  { 
  }
derive instance newtypeExpireSessionResult :: Newtype ExpireSessionResult _


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
derive instance newtypeFleet :: Newtype Fleet _


-- | <p>The fleet attribute.</p>
newtype FleetAttribute = FleetAttribute String
derive instance newtypeFleetAttribute :: Newtype FleetAttribute _


-- | <p>The fleet attributes.</p>
newtype FleetAttributes = FleetAttributes (Array FleetAttribute)
derive instance newtypeFleetAttributes :: Newtype FleetAttributes _


-- | <p>Describes a fleet error.</p>
newtype FleetError = FleetError 
  { "ErrorCode" :: NullOrUndefined (FleetErrorCode)
  , "ErrorMessage" :: NullOrUndefined (String)
  }
derive instance newtypeFleetError :: Newtype FleetError _


newtype FleetErrorCode = FleetErrorCode String
derive instance newtypeFleetErrorCode :: Newtype FleetErrorCode _


newtype FleetErrors = FleetErrors (Array FleetError)
derive instance newtypeFleetErrors :: Newtype FleetErrors _


-- | <p>The fleets.</p>
newtype FleetList = FleetList (Array Fleet)
derive instance newtypeFleetList :: Newtype FleetList _


newtype FleetState = FleetState String
derive instance newtypeFleetState :: Newtype FleetState _


newtype FleetType = FleetType String
derive instance newtypeFleetType :: Newtype FleetType _


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
derive instance newtypeImage :: Newtype Image _


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
derive instance newtypeImageBuilder :: Newtype ImageBuilder _


newtype ImageBuilderList = ImageBuilderList (Array ImageBuilder)
derive instance newtypeImageBuilderList :: Newtype ImageBuilderList _


newtype ImageBuilderState = ImageBuilderState String
derive instance newtypeImageBuilderState :: Newtype ImageBuilderState _


-- | <p>Describes the reason why the last image builder state change occurred.</p>
newtype ImageBuilderStateChangeReason = ImageBuilderStateChangeReason 
  { "Code" :: NullOrUndefined (ImageBuilderStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeImageBuilderStateChangeReason :: Newtype ImageBuilderStateChangeReason _


newtype ImageBuilderStateChangeReasonCode = ImageBuilderStateChangeReasonCode String
derive instance newtypeImageBuilderStateChangeReasonCode :: Newtype ImageBuilderStateChangeReasonCode _


newtype ImageList = ImageList (Array Image)
derive instance newtypeImageList :: Newtype ImageList _


newtype ImageState = ImageState String
derive instance newtypeImageState :: Newtype ImageState _


-- | <p>Describes the reason why the last image state change occurred.</p>
newtype ImageStateChangeReason = ImageStateChangeReason 
  { "Code" :: NullOrUndefined (ImageStateChangeReasonCode)
  , "Message" :: NullOrUndefined (String)
  }
derive instance newtypeImageStateChangeReason :: Newtype ImageStateChangeReason _


newtype ImageStateChangeReasonCode = ImageStateChangeReasonCode String
derive instance newtypeImageStateChangeReasonCode :: Newtype ImageStateChangeReasonCode _


-- | <p>The image does not support storage connectors.</p>
newtype IncompatibleImageException = IncompatibleImageException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeIncompatibleImageException :: Newtype IncompatibleImageException _


-- | <p>Indicates an incorrect combination of parameters, or a missing parameter.</p>
newtype InvalidParameterCombinationException = InvalidParameterCombinationException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidParameterCombinationException :: Newtype InvalidParameterCombinationException _


-- | <p>The specified role is invalid.</p>
newtype InvalidRoleException = InvalidRoleException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeInvalidRoleException :: Newtype InvalidRoleException _


-- | <p>The requested limit exceeds the permitted limit for an account.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype ListAssociatedFleetsRequest = ListAssociatedFleetsRequest 
  { "StackName" :: (String)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListAssociatedFleetsRequest :: Newtype ListAssociatedFleetsRequest _


newtype ListAssociatedFleetsResult = ListAssociatedFleetsResult 
  { "Names" :: NullOrUndefined (StringList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListAssociatedFleetsResult :: Newtype ListAssociatedFleetsResult _


newtype ListAssociatedStacksRequest = ListAssociatedStacksRequest 
  { "FleetName" :: (String)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListAssociatedStacksRequest :: Newtype ListAssociatedStacksRequest _


newtype ListAssociatedStacksResult = ListAssociatedStacksResult 
  { "Names" :: NullOrUndefined (StringList)
  , "NextToken" :: NullOrUndefined (String)
  }
derive instance newtypeListAssociatedStacksResult :: Newtype ListAssociatedStacksResult _


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceArn" :: (Arn)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _


newtype ListTagsForResourceResponse = ListTagsForResourceResponse 
  { "Tags" :: NullOrUndefined (Tags)
  }
derive instance newtypeListTagsForResourceResponse :: Newtype ListTagsForResourceResponse _


newtype Metadata = Metadata (Map String String)
derive instance newtypeMetadata :: Newtype Metadata _


newtype Name = Name String
derive instance newtypeName :: Newtype Name _


-- | <p>The attempted operation is not permitted.</p>
newtype OperationNotPermittedException = OperationNotPermittedException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeOperationNotPermittedException :: Newtype OperationNotPermittedException _


newtype OrganizationalUnitDistinguishedName = OrganizationalUnitDistinguishedName String
derive instance newtypeOrganizationalUnitDistinguishedName :: Newtype OrganizationalUnitDistinguishedName _


newtype OrganizationalUnitDistinguishedNamesList = OrganizationalUnitDistinguishedNamesList (Array OrganizationalUnitDistinguishedName)
derive instance newtypeOrganizationalUnitDistinguishedNamesList :: Newtype OrganizationalUnitDistinguishedNamesList _


newtype PlatformType = PlatformType String
derive instance newtypePlatformType :: Newtype PlatformType _


newtype RedirectURL = RedirectURL String
derive instance newtypeRedirectURL :: Newtype RedirectURL _


newtype RegionName = RegionName String
derive instance newtypeRegionName :: Newtype RegionName _


-- | <p>The specified resource already exists.</p>
newtype ResourceAlreadyExistsException = ResourceAlreadyExistsException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceAlreadyExistsException :: Newtype ResourceAlreadyExistsException _


-- | <p>Describes a resource error.</p>
newtype ResourceError = ResourceError 
  { "ErrorCode" :: NullOrUndefined (FleetErrorCode)
  , "ErrorMessage" :: NullOrUndefined (String)
  , "ErrorTimestamp" :: NullOrUndefined (Number)
  }
derive instance newtypeResourceError :: Newtype ResourceError _


newtype ResourceErrors = ResourceErrors (Array ResourceError)
derive instance newtypeResourceErrors :: Newtype ResourceErrors _


-- | <p>The ARN of the resource.</p>
newtype ResourceIdentifier = ResourceIdentifier String
derive instance newtypeResourceIdentifier :: Newtype ResourceIdentifier _


-- | <p>The specified resource is in use.</p>
newtype ResourceInUseException = ResourceInUseException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceInUseException :: Newtype ResourceInUseException _


-- | <p>The specified resource exists and is not in use, but isn't available.</p>
newtype ResourceNotAvailableException = ResourceNotAvailableException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotAvailableException :: Newtype ResourceNotAvailableException _


-- | <p>The specified resource was not found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message" :: NullOrUndefined (ErrorMessage)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The security group IDs.</p>
newtype SecurityGroupIdList = SecurityGroupIdList (Array String)
derive instance newtypeSecurityGroupIdList :: Newtype SecurityGroupIdList _


-- | <p>Describes the credentials for the service account used by the streaming instance to connect to the directory.</p>
newtype ServiceAccountCredentials = ServiceAccountCredentials 
  { "AccountName" :: (AccountName)
  , "AccountPassword" :: (AccountPassword)
  }
derive instance newtypeServiceAccountCredentials :: Newtype ServiceAccountCredentials _


-- | <p>Describes a streaming session.</p>
newtype Session = Session 
  { "Id" :: (String)
  , "UserId" :: (UserId)
  , "StackName" :: (String)
  , "FleetName" :: (String)
  , "State" :: (SessionState)
  , "AuthenticationType" :: NullOrUndefined (AuthenticationType)
  }
derive instance newtypeSession :: Newtype Session _


-- | <p>List of sessions.</p>
newtype SessionList = SessionList (Array Session)
derive instance newtypeSessionList :: Newtype SessionList _


-- | <p>Possible values for the state of a streaming session.</p>
newtype SessionState = SessionState String
derive instance newtypeSessionState :: Newtype SessionState _


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
derive instance newtypeStack :: Newtype Stack _


newtype StackAttribute = StackAttribute String
derive instance newtypeStackAttribute :: Newtype StackAttribute _


newtype StackAttributes = StackAttributes (Array StackAttribute)
derive instance newtypeStackAttributes :: Newtype StackAttributes _


-- | <p>Describes a stack error.</p>
newtype StackError = StackError 
  { "ErrorCode" :: NullOrUndefined (StackErrorCode)
  , "ErrorMessage" :: NullOrUndefined (String)
  }
derive instance newtypeStackError :: Newtype StackError _


newtype StackErrorCode = StackErrorCode String
derive instance newtypeStackErrorCode :: Newtype StackErrorCode _


-- | <p>The stack errors.</p>
newtype StackErrors = StackErrors (Array StackError)
derive instance newtypeStackErrors :: Newtype StackErrors _


-- | <p>The stacks.</p>
newtype StackList = StackList (Array Stack)
derive instance newtypeStackList :: Newtype StackList _


newtype StartFleetRequest = StartFleetRequest 
  { "Name" :: (String)
  }
derive instance newtypeStartFleetRequest :: Newtype StartFleetRequest _


newtype StartFleetResult = StartFleetResult 
  { 
  }
derive instance newtypeStartFleetResult :: Newtype StartFleetResult _


newtype StartImageBuilderRequest = StartImageBuilderRequest 
  { "Name" :: (String)
  , "AppstreamAgentVersion" :: NullOrUndefined (AppstreamAgentVersion)
  }
derive instance newtypeStartImageBuilderRequest :: Newtype StartImageBuilderRequest _


newtype StartImageBuilderResult = StartImageBuilderResult 
  { "ImageBuilder" :: NullOrUndefined (ImageBuilder)
  }
derive instance newtypeStartImageBuilderResult :: Newtype StartImageBuilderResult _


newtype StopFleetRequest = StopFleetRequest 
  { "Name" :: (String)
  }
derive instance newtypeStopFleetRequest :: Newtype StopFleetRequest _


newtype StopFleetResult = StopFleetResult 
  { 
  }
derive instance newtypeStopFleetResult :: Newtype StopFleetResult _


newtype StopImageBuilderRequest = StopImageBuilderRequest 
  { "Name" :: (String)
  }
derive instance newtypeStopImageBuilderRequest :: Newtype StopImageBuilderRequest _


newtype StopImageBuilderResult = StopImageBuilderResult 
  { "ImageBuilder" :: NullOrUndefined (ImageBuilder)
  }
derive instance newtypeStopImageBuilderResult :: Newtype StopImageBuilderResult _


-- | <p>Describes a storage connector.</p>
newtype StorageConnector = StorageConnector 
  { "ConnectorType" :: (StorageConnectorType)
  , "ResourceIdentifier" :: NullOrUndefined (ResourceIdentifier)
  }
derive instance newtypeStorageConnector :: Newtype StorageConnector _


-- | <p>The storage connectors.</p>
newtype StorageConnectorList = StorageConnectorList (Array StorageConnector)
derive instance newtypeStorageConnectorList :: Newtype StorageConnectorList _


-- | <p>The type of storage connector.</p>
newtype StorageConnectorType = StorageConnectorType String
derive instance newtypeStorageConnectorType :: Newtype StorageConnectorType _


newtype StreamingUrlUserId = StreamingUrlUserId String
derive instance newtypeStreamingUrlUserId :: Newtype StreamingUrlUserId _


newtype StringList = StringList (Array String)
derive instance newtypeStringList :: Newtype StringList _


-- | <p>The subnet IDs.</p>
newtype SubnetIdList = SubnetIdList (Array String)
derive instance newtypeSubnetIdList :: Newtype SubnetIdList _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array TagKey)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagResourceRequest = TagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "Tags" :: (Tags)
  }
derive instance newtypeTagResourceRequest :: Newtype TagResourceRequest _


newtype TagResourceResponse = TagResourceResponse 
  { 
  }
derive instance newtypeTagResourceResponse :: Newtype TagResourceResponse _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Map TagKey TagValue)
derive instance newtypeTags :: Newtype Tags _


newtype UntagResourceRequest = UntagResourceRequest 
  { "ResourceArn" :: (Arn)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeUntagResourceRequest :: Newtype UntagResourceRequest _


newtype UntagResourceResponse = UntagResourceResponse 
  { 
  }
derive instance newtypeUntagResourceResponse :: Newtype UntagResourceResponse _


newtype UpdateDirectoryConfigRequest = UpdateDirectoryConfigRequest 
  { "DirectoryName" :: (DirectoryName)
  , "OrganizationalUnitDistinguishedNames" :: NullOrUndefined (OrganizationalUnitDistinguishedNamesList)
  , "ServiceAccountCredentials" :: NullOrUndefined (ServiceAccountCredentials)
  }
derive instance newtypeUpdateDirectoryConfigRequest :: Newtype UpdateDirectoryConfigRequest _


newtype UpdateDirectoryConfigResult = UpdateDirectoryConfigResult 
  { "DirectoryConfig" :: NullOrUndefined (DirectoryConfig)
  }
derive instance newtypeUpdateDirectoryConfigResult :: Newtype UpdateDirectoryConfigResult _


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
derive instance newtypeUpdateFleetRequest :: Newtype UpdateFleetRequest _


newtype UpdateFleetResult = UpdateFleetResult 
  { "Fleet" :: NullOrUndefined (Fleet)
  }
derive instance newtypeUpdateFleetResult :: Newtype UpdateFleetResult _


newtype UpdateStackRequest = UpdateStackRequest 
  { "DisplayName" :: NullOrUndefined (DisplayName)
  , "Description" :: NullOrUndefined (Description)
  , "Name" :: (String)
  , "StorageConnectors" :: NullOrUndefined (StorageConnectorList)
  , "DeleteStorageConnectors" :: NullOrUndefined (Boolean)
  , "RedirectURL" :: NullOrUndefined (RedirectURL)
  , "AttributesToDelete" :: NullOrUndefined (StackAttributes)
  }
derive instance newtypeUpdateStackRequest :: Newtype UpdateStackRequest _


newtype UpdateStackResult = UpdateStackResult 
  { "Stack" :: NullOrUndefined (Stack)
  }
derive instance newtypeUpdateStackResult :: Newtype UpdateStackResult _


newtype UserId = UserId String
derive instance newtypeUserId :: Newtype UserId _


newtype VisibilityType = VisibilityType String
derive instance newtypeVisibilityType :: Newtype VisibilityType _


-- | <p>Describes VPC configuration information.</p>
newtype VpcConfig = VpcConfig 
  { "SubnetIds" :: NullOrUndefined (SubnetIdList)
  , "SecurityGroupIds" :: NullOrUndefined (SecurityGroupIdList)
  }
derive instance newtypeVpcConfig :: Newtype VpcConfig _
