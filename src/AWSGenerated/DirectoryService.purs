

-- | <fullname>AWS Directory Service</fullname> <p>AWS Directory Service is a web service that makes it easy for you to setup and run directories in the AWS cloud, or connect your AWS resources with an existing on-premises Microsoft Active Directory. This guide provides detailed information about AWS Directory Service operations, data types, parameters, and errors. For information about AWS Directory Services features, see <a href="https://aws.amazon.com/directoryservice/">AWS Directory Service</a> and the <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html">AWS Directory Service Administration Guide</a>.</p> <note> <p>AWS provides SDKs that consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .Net, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to AWS Directory Service and other AWS services. For more information about the AWS SDKs, including how to download and install them, see <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> </note>
module AWS.DirectoryService where

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

serviceName = "DirectoryService" :: String


-- | <p>If the DNS server for your on-premises domain uses a publicly addressable IP address, you must add a CIDR address block to correctly route traffic to and from your Microsoft AD on Amazon Web Services. <i>AddIpRoutes</i> adds this address block. You can also use <i>AddIpRoutes</i> to facilitate routing traffic that uses public IP ranges from your Microsoft AD on AWS to a peer VPC. </p> <p>Before you call <i>AddIpRoutes</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>AddIpRoutes</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
addIpRoutes :: forall eff. AddIpRoutesRequest -> Aff (exception :: EXCEPTION | eff) AddIpRoutesResult
addIpRoutes = Request.request serviceName "addIpRoutes" 


-- | <p>Adds or overwrites one or more tags for the specified directory. Each directory can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique to each resource.</p>
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (exception :: EXCEPTION | eff) AddTagsToResourceResult
addTagsToResource = Request.request serviceName "addTagsToResource" 


-- | <p>Cancels an in-progress schema extension to a Microsoft AD directory. Once a schema extension has started replicating to all domain controllers, the task can no longer be canceled. A schema extension can be canceled during any of the following states; <code>Initializing</code>, <code>CreatingSnapshot</code>, and <code>UpdatingSchema</code>.</p>
cancelSchemaExtension :: forall eff. CancelSchemaExtensionRequest -> Aff (exception :: EXCEPTION | eff) CancelSchemaExtensionResult
cancelSchemaExtension = Request.request serviceName "cancelSchemaExtension" 


-- | <p>Creates an AD Connector to connect to an on-premises directory.</p> <p>Before you call <i>ConnectDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>ConnectDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
connectDirectory :: forall eff. ConnectDirectoryRequest -> Aff (exception :: EXCEPTION | eff) ConnectDirectoryResult
connectDirectory = Request.request serviceName "connectDirectory" 


-- | <p>Creates an alias for a directory and assigns the alias to the directory. The alias is used to construct the access URL for the directory, such as <code>http://&lt;alias&gt;.awsapps.com</code>.</p> <important> <p>After an alias has been created, it cannot be deleted or reused, so this operation should only be used when absolutely necessary.</p> </important>
createAlias :: forall eff. CreateAliasRequest -> Aff (exception :: EXCEPTION | eff) CreateAliasResult
createAlias = Request.request serviceName "createAlias" 


-- | <p>Creates a computer account in the specified directory, and joins the computer to the directory.</p>
createComputer :: forall eff. CreateComputerRequest -> Aff (exception :: EXCEPTION | eff) CreateComputerResult
createComputer = Request.request serviceName "createComputer" 


-- | <p>Creates a conditional forwarder associated with your AWS directory. Conditional forwarders are required in order to set up a trust relationship with another domain. The conditional forwarder points to the trusted domain.</p>
createConditionalForwarder :: forall eff. CreateConditionalForwarderRequest -> Aff (exception :: EXCEPTION | eff) CreateConditionalForwarderResult
createConditionalForwarder = Request.request serviceName "createConditionalForwarder" 


-- | <p>Creates a Simple AD directory.</p> <p>Before you call <i>CreateDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>CreateDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
createDirectory :: forall eff. CreateDirectoryRequest -> Aff (exception :: EXCEPTION | eff) CreateDirectoryResult
createDirectory = Request.request serviceName "createDirectory" 


-- | <p>Creates a Microsoft AD in the AWS cloud.</p> <p>Before you call <i>CreateMicrosoftAD</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>CreateMicrosoftAD</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
createMicrosoftAD :: forall eff. CreateMicrosoftADRequest -> Aff (exception :: EXCEPTION | eff) CreateMicrosoftADResult
createMicrosoftAD = Request.request serviceName "createMicrosoftAD" 


-- | <p>Creates a snapshot of a Simple AD or Microsoft AD directory in the AWS cloud.</p> <note> <p>You cannot take snapshots of AD Connector directories.</p> </note>
createSnapshot :: forall eff. CreateSnapshotRequest -> Aff (exception :: EXCEPTION | eff) CreateSnapshotResult
createSnapshot = Request.request serviceName "createSnapshot" 


-- | <p>AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your Microsoft AD in the AWS cloud, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.</p> <p>This action initiates the creation of the AWS side of a trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>
createTrust :: forall eff. CreateTrustRequest -> Aff (exception :: EXCEPTION | eff) CreateTrustResult
createTrust = Request.request serviceName "createTrust" 


-- | <p>Deletes a conditional forwarder that has been set up for your AWS directory.</p>
deleteConditionalForwarder :: forall eff. DeleteConditionalForwarderRequest -> Aff (exception :: EXCEPTION | eff) DeleteConditionalForwarderResult
deleteConditionalForwarder = Request.request serviceName "deleteConditionalForwarder" 


-- | <p>Deletes an AWS Directory Service directory.</p> <p>Before you call <i>DeleteDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>DeleteDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
deleteDirectory :: forall eff. DeleteDirectoryRequest -> Aff (exception :: EXCEPTION | eff) DeleteDirectoryResult
deleteDirectory = Request.request serviceName "deleteDirectory" 


-- | <p>Deletes a directory snapshot.</p>
deleteSnapshot :: forall eff. DeleteSnapshotRequest -> Aff (exception :: EXCEPTION | eff) DeleteSnapshotResult
deleteSnapshot = Request.request serviceName "deleteSnapshot" 


-- | <p>Deletes an existing trust relationship between your Microsoft AD in the AWS cloud and an external domain.</p>
deleteTrust :: forall eff. DeleteTrustRequest -> Aff (exception :: EXCEPTION | eff) DeleteTrustResult
deleteTrust = Request.request serviceName "deleteTrust" 


-- | <p>Removes the specified directory as a publisher to the specified SNS topic.</p>
deregisterEventTopic :: forall eff. DeregisterEventTopicRequest -> Aff (exception :: EXCEPTION | eff) DeregisterEventTopicResult
deregisterEventTopic = Request.request serviceName "deregisterEventTopic" 


-- | <p>Obtains information about the conditional forwarders for this account.</p> <p>If no input parameters are provided for RemoteDomainNames, this request describes all conditional forwarders for the specified directory ID.</p>
describeConditionalForwarders :: forall eff. DescribeConditionalForwardersRequest -> Aff (exception :: EXCEPTION | eff) DescribeConditionalForwardersResult
describeConditionalForwarders = Request.request serviceName "describeConditionalForwarders" 


-- | <p>Obtains information about the directories that belong to this account.</p> <p>You can retrieve information about specific directories by passing the directory identifiers in the <i>DirectoryIds</i> parameter. Otherwise, all directories that belong to the current account are returned.</p> <p>This operation supports pagination with the use of the <i>NextToken</i> request and response parameters. If more results are available, the <i>DescribeDirectoriesResult.NextToken</i> member contains a token that you pass in the next call to <a>DescribeDirectories</a> to retrieve the next set of items.</p> <p>You can also specify a maximum number of return results with the <i>Limit</i> parameter.</p>
describeDirectories :: forall eff. DescribeDirectoriesRequest -> Aff (exception :: EXCEPTION | eff) DescribeDirectoriesResult
describeDirectories = Request.request serviceName "describeDirectories" 


-- | <p>Provides information about any domain controllers in your directory.</p>
describeDomainControllers :: forall eff. DescribeDomainControllersRequest -> Aff (exception :: EXCEPTION | eff) DescribeDomainControllersResult
describeDomainControllers = Request.request serviceName "describeDomainControllers" 


-- | <p>Obtains information about which SNS topics receive status messages from the specified directory.</p> <p>If no input parameters are provided, such as DirectoryId or TopicName, this request describes all of the associations in the account.</p>
describeEventTopics :: forall eff. DescribeEventTopicsRequest -> Aff (exception :: EXCEPTION | eff) DescribeEventTopicsResult
describeEventTopics = Request.request serviceName "describeEventTopics" 


-- | <p>Obtains information about the directory snapshots that belong to this account.</p> <p>This operation supports pagination with the use of the <i>NextToken</i> request and response parameters. If more results are available, the <i>DescribeSnapshots.NextToken</i> member contains a token that you pass in the next call to <a>DescribeSnapshots</a> to retrieve the next set of items.</p> <p>You can also specify a maximum number of return results with the <i>Limit</i> parameter.</p>
describeSnapshots :: forall eff. DescribeSnapshotsRequest -> Aff (exception :: EXCEPTION | eff) DescribeSnapshotsResult
describeSnapshots = Request.request serviceName "describeSnapshots" 


-- | <p>Obtains information about the trust relationships for this account.</p> <p>If no input parameters are provided, such as DirectoryId or TrustIds, this request describes all the trust relationships belonging to the account.</p>
describeTrusts :: forall eff. DescribeTrustsRequest -> Aff (exception :: EXCEPTION | eff) DescribeTrustsResult
describeTrusts = Request.request serviceName "describeTrusts" 


-- | <p>Disables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector directory.</p>
disableRadius :: forall eff. DisableRadiusRequest -> Aff (exception :: EXCEPTION | eff) DisableRadiusResult
disableRadius = Request.request serviceName "disableRadius" 


-- | <p>Disables single-sign on for a directory.</p>
disableSso :: forall eff. DisableSsoRequest -> Aff (exception :: EXCEPTION | eff) DisableSsoResult
disableSso = Request.request serviceName "disableSso" 


-- | <p>Enables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector directory.</p>
enableRadius :: forall eff. EnableRadiusRequest -> Aff (exception :: EXCEPTION | eff) EnableRadiusResult
enableRadius = Request.request serviceName "enableRadius" 


-- | <p>Enables single sign-on for a directory.</p>
enableSso :: forall eff. EnableSsoRequest -> Aff (exception :: EXCEPTION | eff) EnableSsoResult
enableSso = Request.request serviceName "enableSso" 


-- | <p>Obtains directory limit information for the current region.</p>
getDirectoryLimits :: forall eff. GetDirectoryLimitsRequest -> Aff (exception :: EXCEPTION | eff) GetDirectoryLimitsResult
getDirectoryLimits = Request.request serviceName "getDirectoryLimits" 


-- | <p>Obtains the manual snapshot limits for a directory.</p>
getSnapshotLimits :: forall eff. GetSnapshotLimitsRequest -> Aff (exception :: EXCEPTION | eff) GetSnapshotLimitsResult
getSnapshotLimits = Request.request serviceName "getSnapshotLimits" 


-- | <p>Lists the address blocks that you have added to a directory.</p>
listIpRoutes :: forall eff. ListIpRoutesRequest -> Aff (exception :: EXCEPTION | eff) ListIpRoutesResult
listIpRoutes = Request.request serviceName "listIpRoutes" 


-- | <p>Lists all schema extensions applied to a Microsoft AD Directory.</p>
listSchemaExtensions :: forall eff. ListSchemaExtensionsRequest -> Aff (exception :: EXCEPTION | eff) ListSchemaExtensionsResult
listSchemaExtensions = Request.request serviceName "listSchemaExtensions" 


-- | <p>Lists all tags on a directory.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (exception :: EXCEPTION | eff) ListTagsForResourceResult
listTagsForResource = Request.request serviceName "listTagsForResource" 


-- | <p>Associates a directory with an SNS topic. This establishes the directory as a publisher to the specified SNS topic. You can then receive email or text (SMS) messages when the status of your directory changes. You get notified if your directory goes from an Active status to an Impaired or Inoperable status. You also receive a notification when the directory returns to an Active status.</p>
registerEventTopic :: forall eff. RegisterEventTopicRequest -> Aff (exception :: EXCEPTION | eff) RegisterEventTopicResult
registerEventTopic = Request.request serviceName "registerEventTopic" 


-- | <p>Removes IP address blocks from a directory.</p>
removeIpRoutes :: forall eff. RemoveIpRoutesRequest -> Aff (exception :: EXCEPTION | eff) RemoveIpRoutesResult
removeIpRoutes = Request.request serviceName "removeIpRoutes" 


-- | <p>Removes tags from a directory.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (exception :: EXCEPTION | eff) RemoveTagsFromResourceResult
removeTagsFromResource = Request.request serviceName "removeTagsFromResource" 


-- | <p>Restores a directory using an existing directory snapshot.</p> <p>When you restore a directory from a snapshot, any changes made to the directory after the snapshot date are overwritten.</p> <p>This action returns as soon as the restore operation is initiated. You can monitor the progress of the restore operation by calling the <a>DescribeDirectories</a> operation with the directory identifier. When the <b>DirectoryDescription.Stage</b> value changes to <code>Active</code>, the restore operation is complete.</p>
restoreFromSnapshot :: forall eff. RestoreFromSnapshotRequest -> Aff (exception :: EXCEPTION | eff) RestoreFromSnapshotResult
restoreFromSnapshot = Request.request serviceName "restoreFromSnapshot" 


-- | <p>Applies a schema extension to a Microsoft AD directory.</p>
startSchemaExtension :: forall eff. StartSchemaExtensionRequest -> Aff (exception :: EXCEPTION | eff) StartSchemaExtensionResult
startSchemaExtension = Request.request serviceName "startSchemaExtension" 


-- | <p>Updates a conditional forwarder that has been set up for your AWS directory.</p>
updateConditionalForwarder :: forall eff. UpdateConditionalForwarderRequest -> Aff (exception :: EXCEPTION | eff) UpdateConditionalForwarderResult
updateConditionalForwarder = Request.request serviceName "updateConditionalForwarder" 


-- | <p>Adds or removes domain controllers to or from the directory. Based on the difference between current value and new value (provided through this API call), domain controllers will be added or removed. It may take up to 45 minutes for any new domain controllers to become fully active once the requested number of domain controllers is updated. During this time, you cannot make another update request.</p>
updateNumberOfDomainControllers :: forall eff. UpdateNumberOfDomainControllersRequest -> Aff (exception :: EXCEPTION | eff) UpdateNumberOfDomainControllersResult
updateNumberOfDomainControllers = Request.request serviceName "updateNumberOfDomainControllers" 


-- | <p>Updates the Remote Authentication Dial In User Service (RADIUS) server information for an AD Connector directory.</p>
updateRadius :: forall eff. UpdateRadiusRequest -> Aff (exception :: EXCEPTION | eff) UpdateRadiusResult
updateRadius = Request.request serviceName "updateRadius" 


-- | <p>AWS Directory Service for Microsoft Active Directory allows you to configure and verify trust relationships.</p> <p>This action verifies a trust relationship between your Microsoft AD in the AWS cloud and an external domain.</p>
verifyTrust :: forall eff. VerifyTrustRequest -> Aff (exception :: EXCEPTION | eff) VerifyTrustResult
verifyTrust = Request.request serviceName "verifyTrust" 


newtype AccessUrl = AccessUrl String
derive instance newtypeAccessUrl :: Newtype AccessUrl _
derive instance repGenericAccessUrl :: Generic AccessUrl _
instance showAccessUrl :: Show AccessUrl where
  show = genericShow
instance decodeAccessUrl :: Decode AccessUrl where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessUrl :: Encode AccessUrl where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddIpRoutesRequest = AddIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "IpRoutes" :: (IpRoutes)
  , "UpdateSecurityGroupForDirectoryControllers" :: NullOrUndefined.NullOrUndefined (UpdateSecurityGroupForDirectoryControllers)
  }
derive instance newtypeAddIpRoutesRequest :: Newtype AddIpRoutesRequest _
derive instance repGenericAddIpRoutesRequest :: Generic AddIpRoutesRequest _
instance showAddIpRoutesRequest :: Show AddIpRoutesRequest where
  show = genericShow
instance decodeAddIpRoutesRequest :: Decode AddIpRoutesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddIpRoutesRequest :: Encode AddIpRoutesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddIpRoutesResult = AddIpRoutesResult Types.NoArguments
derive instance newtypeAddIpRoutesResult :: Newtype AddIpRoutesResult _
derive instance repGenericAddIpRoutesResult :: Generic AddIpRoutesResult _
instance showAddIpRoutesResult :: Show AddIpRoutesResult where
  show = genericShow
instance decodeAddIpRoutesResult :: Decode AddIpRoutesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddIpRoutesResult :: Encode AddIpRoutesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "Tags" :: (Tags)
  }
derive instance newtypeAddTagsToResourceRequest :: Newtype AddTagsToResourceRequest _
derive instance repGenericAddTagsToResourceRequest :: Generic AddTagsToResourceRequest _
instance showAddTagsToResourceRequest :: Show AddTagsToResourceRequest where
  show = genericShow
instance decodeAddTagsToResourceRequest :: Decode AddTagsToResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceRequest :: Encode AddTagsToResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddTagsToResourceResult = AddTagsToResourceResult Types.NoArguments
derive instance newtypeAddTagsToResourceResult :: Newtype AddTagsToResourceResult _
derive instance repGenericAddTagsToResourceResult :: Generic AddTagsToResourceResult _
instance showAddTagsToResourceResult :: Show AddTagsToResourceResult where
  show = genericShow
instance decodeAddTagsToResourceResult :: Decode AddTagsToResourceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddTagsToResourceResult :: Encode AddTagsToResourceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddedDateTime = AddedDateTime Number
derive instance newtypeAddedDateTime :: Newtype AddedDateTime _
derive instance repGenericAddedDateTime :: Generic AddedDateTime _
instance showAddedDateTime :: Show AddedDateTime where
  show = genericShow
instance decodeAddedDateTime :: Decode AddedDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddedDateTime :: Encode AddedDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AliasName = AliasName String
derive instance newtypeAliasName :: Newtype AliasName _
derive instance repGenericAliasName :: Generic AliasName _
instance showAliasName :: Show AliasName where
  show = genericShow
instance decodeAliasName :: Decode AliasName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAliasName :: Encode AliasName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Represents a named directory attribute.</p>
newtype Attribute = Attribute 
  { "Name" :: NullOrUndefined.NullOrUndefined (AttributeName)
  , "Value" :: NullOrUndefined.NullOrUndefined (AttributeValue)
  }
derive instance newtypeAttribute :: Newtype Attribute _
derive instance repGenericAttribute :: Generic Attribute _
instance showAttribute :: Show Attribute where
  show = genericShow
instance decodeAttribute :: Decode Attribute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttribute :: Encode Attribute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _
derive instance repGenericAttributeName :: Generic AttributeName _
instance showAttributeName :: Show AttributeName where
  show = genericShow
instance decodeAttributeName :: Decode AttributeName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeName :: Encode AttributeName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _
derive instance repGenericAttributeValue :: Generic AttributeValue _
instance showAttributeValue :: Show AttributeValue where
  show = genericShow
instance decodeAttributeValue :: Decode AttributeValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributeValue :: Encode AttributeValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Attributes = Attributes (Array Attribute)
derive instance newtypeAttributes :: Newtype Attributes _
derive instance repGenericAttributes :: Generic Attributes _
instance showAttributes :: Show Attributes where
  show = genericShow
instance decodeAttributes :: Decode Attributes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAttributes :: Encode Attributes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An authentication error occurred.</p>
newtype AuthenticationFailedException = AuthenticationFailedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeAuthenticationFailedException :: Newtype AuthenticationFailedException _
derive instance repGenericAuthenticationFailedException :: Generic AuthenticationFailedException _
instance showAuthenticationFailedException :: Show AuthenticationFailedException where
  show = genericShow
instance decodeAuthenticationFailedException :: Decode AuthenticationFailedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthenticationFailedException :: Encode AuthenticationFailedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZone = AvailabilityZone String
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _
derive instance repGenericAvailabilityZone :: Generic AvailabilityZone _
instance showAvailabilityZone :: Show AvailabilityZone where
  show = genericShow
instance decodeAvailabilityZone :: Decode AvailabilityZone where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZone :: Encode AvailabilityZone where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AvailabilityZones = AvailabilityZones (Array AvailabilityZone)
derive instance newtypeAvailabilityZones :: Newtype AvailabilityZones _
derive instance repGenericAvailabilityZones :: Generic AvailabilityZones _
instance showAvailabilityZones :: Show AvailabilityZones where
  show = genericShow
instance decodeAvailabilityZones :: Decode AvailabilityZones where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAvailabilityZones :: Encode AvailabilityZones where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelSchemaExtensionRequest = CancelSchemaExtensionRequest 
  { "DirectoryId" :: (DirectoryId)
  , "SchemaExtensionId" :: (SchemaExtensionId)
  }
derive instance newtypeCancelSchemaExtensionRequest :: Newtype CancelSchemaExtensionRequest _
derive instance repGenericCancelSchemaExtensionRequest :: Generic CancelSchemaExtensionRequest _
instance showCancelSchemaExtensionRequest :: Show CancelSchemaExtensionRequest where
  show = genericShow
instance decodeCancelSchemaExtensionRequest :: Decode CancelSchemaExtensionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelSchemaExtensionRequest :: Encode CancelSchemaExtensionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CancelSchemaExtensionResult = CancelSchemaExtensionResult Types.NoArguments
derive instance newtypeCancelSchemaExtensionResult :: Newtype CancelSchemaExtensionResult _
derive instance repGenericCancelSchemaExtensionResult :: Generic CancelSchemaExtensionResult _
instance showCancelSchemaExtensionResult :: Show CancelSchemaExtensionResult where
  show = genericShow
instance decodeCancelSchemaExtensionResult :: Decode CancelSchemaExtensionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCancelSchemaExtensionResult :: Encode CancelSchemaExtensionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CidrIp = CidrIp String
derive instance newtypeCidrIp :: Newtype CidrIp _
derive instance repGenericCidrIp :: Generic CidrIp _
instance showCidrIp :: Show CidrIp where
  show = genericShow
instance decodeCidrIp :: Decode CidrIp where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCidrIp :: Encode CidrIp where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CidrIps = CidrIps (Array CidrIp)
derive instance newtypeCidrIps :: Newtype CidrIps _
derive instance repGenericCidrIps :: Generic CidrIps _
instance showCidrIps :: Show CidrIps where
  show = genericShow
instance decodeCidrIps :: Decode CidrIps where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCidrIps :: Encode CidrIps where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A client exception has occurred.</p>
newtype ClientException = ClientException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeClientException :: Newtype ClientException _
derive instance repGenericClientException :: Generic ClientException _
instance showClientException :: Show ClientException where
  show = genericShow
instance decodeClientException :: Decode ClientException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeClientException :: Encode ClientException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CloudOnlyDirectoriesLimitReached = CloudOnlyDirectoriesLimitReached Boolean
derive instance newtypeCloudOnlyDirectoriesLimitReached :: Newtype CloudOnlyDirectoriesLimitReached _
derive instance repGenericCloudOnlyDirectoriesLimitReached :: Generic CloudOnlyDirectoriesLimitReached _
instance showCloudOnlyDirectoriesLimitReached :: Show CloudOnlyDirectoriesLimitReached where
  show = genericShow
instance decodeCloudOnlyDirectoriesLimitReached :: Decode CloudOnlyDirectoriesLimitReached where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCloudOnlyDirectoriesLimitReached :: Encode CloudOnlyDirectoriesLimitReached where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a computer account in a directory.</p>
newtype Computer = Computer 
  { "ComputerId" :: NullOrUndefined.NullOrUndefined (SID)
  , "ComputerName" :: NullOrUndefined.NullOrUndefined (ComputerName)
  , "ComputerAttributes" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypeComputer :: Newtype Computer _
derive instance repGenericComputer :: Generic Computer _
instance showComputer :: Show Computer where
  show = genericShow
instance decodeComputer :: Decode Computer where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputer :: Encode Computer where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComputerName = ComputerName String
derive instance newtypeComputerName :: Newtype ComputerName _
derive instance repGenericComputerName :: Generic ComputerName _
instance showComputerName :: Show ComputerName where
  show = genericShow
instance decodeComputerName :: Decode ComputerName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputerName :: Encode ComputerName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ComputerPassword = ComputerPassword String
derive instance newtypeComputerPassword :: Newtype ComputerPassword _
derive instance repGenericComputerPassword :: Generic ComputerPassword _
instance showComputerPassword :: Show ComputerPassword where
  show = genericShow
instance decodeComputerPassword :: Decode ComputerPassword where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputerPassword :: Encode ComputerPassword where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>
newtype ConditionalForwarder = ConditionalForwarder 
  { "RemoteDomainName" :: NullOrUndefined.NullOrUndefined (RemoteDomainName)
  , "DnsIpAddrs" :: NullOrUndefined.NullOrUndefined (DnsIpAddrs)
  , "ReplicationScope" :: NullOrUndefined.NullOrUndefined (ReplicationScope)
  }
derive instance newtypeConditionalForwarder :: Newtype ConditionalForwarder _
derive instance repGenericConditionalForwarder :: Generic ConditionalForwarder _
instance showConditionalForwarder :: Show ConditionalForwarder where
  show = genericShow
instance decodeConditionalForwarder :: Decode ConditionalForwarder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConditionalForwarder :: Encode ConditionalForwarder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConditionalForwarders = ConditionalForwarders (Array ConditionalForwarder)
derive instance newtypeConditionalForwarders :: Newtype ConditionalForwarders _
derive instance repGenericConditionalForwarders :: Generic ConditionalForwarders _
instance showConditionalForwarders :: Show ConditionalForwarders where
  show = genericShow
instance decodeConditionalForwarders :: Decode ConditionalForwarders where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConditionalForwarders :: Encode ConditionalForwarders where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>ConnectDirectory</a> operation.</p>
newtype ConnectDirectoryRequest = ConnectDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined.NullOrUndefined (DirectoryShortName)
  , "Password" :: (ConnectPassword)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Size" :: (DirectorySize)
  , "ConnectSettings" :: (DirectoryConnectSettings)
  }
derive instance newtypeConnectDirectoryRequest :: Newtype ConnectDirectoryRequest _
derive instance repGenericConnectDirectoryRequest :: Generic ConnectDirectoryRequest _
instance showConnectDirectoryRequest :: Show ConnectDirectoryRequest where
  show = genericShow
instance decodeConnectDirectoryRequest :: Decode ConnectDirectoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectDirectoryRequest :: Encode ConnectDirectoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>ConnectDirectory</a> operation.</p>
newtype ConnectDirectoryResult = ConnectDirectoryResult 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  }
derive instance newtypeConnectDirectoryResult :: Newtype ConnectDirectoryResult _
derive instance repGenericConnectDirectoryResult :: Generic ConnectDirectoryResult _
instance showConnectDirectoryResult :: Show ConnectDirectoryResult where
  show = genericShow
instance decodeConnectDirectoryResult :: Decode ConnectDirectoryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectDirectoryResult :: Encode ConnectDirectoryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectPassword = ConnectPassword String
derive instance newtypeConnectPassword :: Newtype ConnectPassword _
derive instance repGenericConnectPassword :: Generic ConnectPassword _
instance showConnectPassword :: Show ConnectPassword where
  show = genericShow
instance decodeConnectPassword :: Decode ConnectPassword where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectPassword :: Encode ConnectPassword where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ConnectedDirectoriesLimitReached = ConnectedDirectoriesLimitReached Boolean
derive instance newtypeConnectedDirectoriesLimitReached :: Newtype ConnectedDirectoriesLimitReached _
derive instance repGenericConnectedDirectoriesLimitReached :: Generic ConnectedDirectoriesLimitReached _
instance showConnectedDirectoriesLimitReached :: Show ConnectedDirectoriesLimitReached where
  show = genericShow
instance decodeConnectedDirectoriesLimitReached :: Decode ConnectedDirectoriesLimitReached where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectedDirectoriesLimitReached :: Encode ConnectedDirectoriesLimitReached where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>CreateAlias</a> operation.</p>
newtype CreateAliasRequest = CreateAliasRequest 
  { "DirectoryId" :: (DirectoryId)
  , "Alias" :: (AliasName)
  }
derive instance newtypeCreateAliasRequest :: Newtype CreateAliasRequest _
derive instance repGenericCreateAliasRequest :: Generic CreateAliasRequest _
instance showCreateAliasRequest :: Show CreateAliasRequest where
  show = genericShow
instance decodeCreateAliasRequest :: Decode CreateAliasRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAliasRequest :: Encode CreateAliasRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>CreateAlias</a> operation.</p>
newtype CreateAliasResult = CreateAliasResult 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "Alias" :: NullOrUndefined.NullOrUndefined (AliasName)
  }
derive instance newtypeCreateAliasResult :: Newtype CreateAliasResult _
derive instance repGenericCreateAliasResult :: Generic CreateAliasResult _
instance showCreateAliasResult :: Show CreateAliasResult where
  show = genericShow
instance decodeCreateAliasResult :: Decode CreateAliasResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateAliasResult :: Encode CreateAliasResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>CreateComputer</a> operation.</p>
newtype CreateComputerRequest = CreateComputerRequest 
  { "DirectoryId" :: (DirectoryId)
  , "ComputerName" :: (ComputerName)
  , "Password" :: (ComputerPassword)
  , "OrganizationalUnitDistinguishedName" :: NullOrUndefined.NullOrUndefined (OrganizationalUnitDN)
  , "ComputerAttributes" :: NullOrUndefined.NullOrUndefined (Attributes)
  }
derive instance newtypeCreateComputerRequest :: Newtype CreateComputerRequest _
derive instance repGenericCreateComputerRequest :: Generic CreateComputerRequest _
instance showCreateComputerRequest :: Show CreateComputerRequest where
  show = genericShow
instance decodeCreateComputerRequest :: Decode CreateComputerRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateComputerRequest :: Encode CreateComputerRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results for the <a>CreateComputer</a> operation.</p>
newtype CreateComputerResult = CreateComputerResult 
  { "Computer" :: NullOrUndefined.NullOrUndefined (Computer)
  }
derive instance newtypeCreateComputerResult :: Newtype CreateComputerResult _
derive instance repGenericCreateComputerResult :: Generic CreateComputerResult _
instance showCreateComputerResult :: Show CreateComputerResult where
  show = genericShow
instance decodeCreateComputerResult :: Decode CreateComputerResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateComputerResult :: Encode CreateComputerResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Initiates the creation of a conditional forwarder for your AWS Directory Service for Microsoft Active Directory. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>
newtype CreateConditionalForwarderRequest = CreateConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "DnsIpAddrs" :: (DnsIpAddrs)
  }
derive instance newtypeCreateConditionalForwarderRequest :: Newtype CreateConditionalForwarderRequest _
derive instance repGenericCreateConditionalForwarderRequest :: Generic CreateConditionalForwarderRequest _
instance showCreateConditionalForwarderRequest :: Show CreateConditionalForwarderRequest where
  show = genericShow
instance decodeCreateConditionalForwarderRequest :: Decode CreateConditionalForwarderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConditionalForwarderRequest :: Encode CreateConditionalForwarderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a CreateConditinalForwarder request.</p>
newtype CreateConditionalForwarderResult = CreateConditionalForwarderResult Types.NoArguments
derive instance newtypeCreateConditionalForwarderResult :: Newtype CreateConditionalForwarderResult _
derive instance repGenericCreateConditionalForwarderResult :: Generic CreateConditionalForwarderResult _
instance showCreateConditionalForwarderResult :: Show CreateConditionalForwarderResult where
  show = genericShow
instance decodeCreateConditionalForwarderResult :: Decode CreateConditionalForwarderResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateConditionalForwarderResult :: Encode CreateConditionalForwarderResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>CreateDirectory</a> operation. </p>
newtype CreateDirectoryRequest = CreateDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined.NullOrUndefined (DirectoryShortName)
  , "Password" :: (Password)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "Size" :: (DirectorySize)
  , "VpcSettings" :: NullOrUndefined.NullOrUndefined (DirectoryVpcSettings)
  }
derive instance newtypeCreateDirectoryRequest :: Newtype CreateDirectoryRequest _
derive instance repGenericCreateDirectoryRequest :: Generic CreateDirectoryRequest _
instance showCreateDirectoryRequest :: Show CreateDirectoryRequest where
  show = genericShow
instance decodeCreateDirectoryRequest :: Decode CreateDirectoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDirectoryRequest :: Encode CreateDirectoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>CreateDirectory</a> operation.</p>
newtype CreateDirectoryResult = CreateDirectoryResult 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  }
derive instance newtypeCreateDirectoryResult :: Newtype CreateDirectoryResult _
derive instance repGenericCreateDirectoryResult :: Generic CreateDirectoryResult _
instance showCreateDirectoryResult :: Show CreateDirectoryResult where
  show = genericShow
instance decodeCreateDirectoryResult :: Decode CreateDirectoryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateDirectoryResult :: Encode CreateDirectoryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Creates a Microsoft AD in the AWS cloud.</p>
newtype CreateMicrosoftADRequest = CreateMicrosoftADRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined.NullOrUndefined (DirectoryShortName)
  , "Password" :: (Password)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "VpcSettings" :: (DirectoryVpcSettings)
  , "Edition" :: NullOrUndefined.NullOrUndefined (DirectoryEdition)
  }
derive instance newtypeCreateMicrosoftADRequest :: Newtype CreateMicrosoftADRequest _
derive instance repGenericCreateMicrosoftADRequest :: Generic CreateMicrosoftADRequest _
instance showCreateMicrosoftADRequest :: Show CreateMicrosoftADRequest where
  show = genericShow
instance decodeCreateMicrosoftADRequest :: Decode CreateMicrosoftADRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMicrosoftADRequest :: Encode CreateMicrosoftADRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result of a CreateMicrosoftAD request.</p>
newtype CreateMicrosoftADResult = CreateMicrosoftADResult 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  }
derive instance newtypeCreateMicrosoftADResult :: Newtype CreateMicrosoftADResult _
derive instance repGenericCreateMicrosoftADResult :: Generic CreateMicrosoftADResult _
instance showCreateMicrosoftADResult :: Show CreateMicrosoftADResult where
  show = genericShow
instance decodeCreateMicrosoftADResult :: Decode CreateMicrosoftADResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateMicrosoftADResult :: Encode CreateMicrosoftADResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateSnapshotBeforeSchemaExtension = CreateSnapshotBeforeSchemaExtension Boolean
derive instance newtypeCreateSnapshotBeforeSchemaExtension :: Newtype CreateSnapshotBeforeSchemaExtension _
derive instance repGenericCreateSnapshotBeforeSchemaExtension :: Generic CreateSnapshotBeforeSchemaExtension _
instance showCreateSnapshotBeforeSchemaExtension :: Show CreateSnapshotBeforeSchemaExtension where
  show = genericShow
instance decodeCreateSnapshotBeforeSchemaExtension :: Decode CreateSnapshotBeforeSchemaExtension where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotBeforeSchemaExtension :: Encode CreateSnapshotBeforeSchemaExtension where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>CreateSnapshot</a> operation.</p>
newtype CreateSnapshotRequest = CreateSnapshotRequest 
  { "DirectoryId" :: (DirectoryId)
  , "Name" :: NullOrUndefined.NullOrUndefined (SnapshotName)
  }
derive instance newtypeCreateSnapshotRequest :: Newtype CreateSnapshotRequest _
derive instance repGenericCreateSnapshotRequest :: Generic CreateSnapshotRequest _
instance showCreateSnapshotRequest :: Show CreateSnapshotRequest where
  show = genericShow
instance decodeCreateSnapshotRequest :: Decode CreateSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotRequest :: Encode CreateSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>CreateSnapshot</a> operation.</p>
newtype CreateSnapshotResult = CreateSnapshotResult 
  { "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  }
derive instance newtypeCreateSnapshotResult :: Newtype CreateSnapshotResult _
derive instance repGenericCreateSnapshotResult :: Generic CreateSnapshotResult _
instance showCreateSnapshotResult :: Show CreateSnapshotResult where
  show = genericShow
instance decodeCreateSnapshotResult :: Decode CreateSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateSnapshotResult :: Encode CreateSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your Microsoft AD in the AWS cloud, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.</p> <p>This action initiates the creation of the AWS side of a trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>
newtype CreateTrustRequest = CreateTrustRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "TrustPassword" :: (TrustPassword)
  , "TrustDirection" :: (TrustDirection)
  , "TrustType" :: NullOrUndefined.NullOrUndefined (TrustType)
  , "ConditionalForwarderIpAddrs" :: NullOrUndefined.NullOrUndefined (DnsIpAddrs)
  }
derive instance newtypeCreateTrustRequest :: Newtype CreateTrustRequest _
derive instance repGenericCreateTrustRequest :: Generic CreateTrustRequest _
instance showCreateTrustRequest :: Show CreateTrustRequest where
  show = genericShow
instance decodeCreateTrustRequest :: Decode CreateTrustRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrustRequest :: Encode CreateTrustRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a CreateTrust request.</p>
newtype CreateTrustResult = CreateTrustResult 
  { "TrustId" :: NullOrUndefined.NullOrUndefined (TrustId)
  }
derive instance newtypeCreateTrustResult :: Newtype CreateTrustResult _
derive instance repGenericCreateTrustResult :: Generic CreateTrustResult _
instance showCreateTrustResult :: Show CreateTrustResult where
  show = genericShow
instance decodeCreateTrustResult :: Decode CreateTrustResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTrustResult :: Encode CreateTrustResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreatedDateTime = CreatedDateTime Number
derive instance newtypeCreatedDateTime :: Newtype CreatedDateTime _
derive instance repGenericCreatedDateTime :: Generic CreatedDateTime _
instance showCreatedDateTime :: Show CreatedDateTime where
  show = genericShow
instance decodeCreatedDateTime :: Decode CreatedDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreatedDateTime :: Encode CreatedDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteAssociatedConditionalForwarder = DeleteAssociatedConditionalForwarder Boolean
derive instance newtypeDeleteAssociatedConditionalForwarder :: Newtype DeleteAssociatedConditionalForwarder _
derive instance repGenericDeleteAssociatedConditionalForwarder :: Generic DeleteAssociatedConditionalForwarder _
instance showDeleteAssociatedConditionalForwarder :: Show DeleteAssociatedConditionalForwarder where
  show = genericShow
instance decodeDeleteAssociatedConditionalForwarder :: Decode DeleteAssociatedConditionalForwarder where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteAssociatedConditionalForwarder :: Encode DeleteAssociatedConditionalForwarder where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Deletes a conditional forwarder.</p>
newtype DeleteConditionalForwarderRequest = DeleteConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  }
derive instance newtypeDeleteConditionalForwarderRequest :: Newtype DeleteConditionalForwarderRequest _
derive instance repGenericDeleteConditionalForwarderRequest :: Generic DeleteConditionalForwarderRequest _
instance showDeleteConditionalForwarderRequest :: Show DeleteConditionalForwarderRequest where
  show = genericShow
instance decodeDeleteConditionalForwarderRequest :: Decode DeleteConditionalForwarderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConditionalForwarderRequest :: Encode DeleteConditionalForwarderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a DeleteConditionalForwarder request.</p>
newtype DeleteConditionalForwarderResult = DeleteConditionalForwarderResult Types.NoArguments
derive instance newtypeDeleteConditionalForwarderResult :: Newtype DeleteConditionalForwarderResult _
derive instance repGenericDeleteConditionalForwarderResult :: Generic DeleteConditionalForwarderResult _
instance showDeleteConditionalForwarderResult :: Show DeleteConditionalForwarderResult where
  show = genericShow
instance decodeDeleteConditionalForwarderResult :: Decode DeleteConditionalForwarderResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteConditionalForwarderResult :: Encode DeleteConditionalForwarderResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DeleteDirectory</a> operation.</p>
newtype DeleteDirectoryRequest = DeleteDirectoryRequest 
  { "DirectoryId" :: (DirectoryId)
  }
derive instance newtypeDeleteDirectoryRequest :: Newtype DeleteDirectoryRequest _
derive instance repGenericDeleteDirectoryRequest :: Generic DeleteDirectoryRequest _
instance showDeleteDirectoryRequest :: Show DeleteDirectoryRequest where
  show = genericShow
instance decodeDeleteDirectoryRequest :: Decode DeleteDirectoryRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDirectoryRequest :: Encode DeleteDirectoryRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>DeleteDirectory</a> operation.</p>
newtype DeleteDirectoryResult = DeleteDirectoryResult 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  }
derive instance newtypeDeleteDirectoryResult :: Newtype DeleteDirectoryResult _
derive instance repGenericDeleteDirectoryResult :: Generic DeleteDirectoryResult _
instance showDeleteDirectoryResult :: Show DeleteDirectoryResult where
  show = genericShow
instance decodeDeleteDirectoryResult :: Decode DeleteDirectoryResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDirectoryResult :: Encode DeleteDirectoryResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DeleteSnapshot</a> operation.</p>
newtype DeleteSnapshotRequest = DeleteSnapshotRequest 
  { "SnapshotId" :: (SnapshotId)
  }
derive instance newtypeDeleteSnapshotRequest :: Newtype DeleteSnapshotRequest _
derive instance repGenericDeleteSnapshotRequest :: Generic DeleteSnapshotRequest _
instance showDeleteSnapshotRequest :: Show DeleteSnapshotRequest where
  show = genericShow
instance decodeDeleteSnapshotRequest :: Decode DeleteSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSnapshotRequest :: Encode DeleteSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>DeleteSnapshot</a> operation.</p>
newtype DeleteSnapshotResult = DeleteSnapshotResult 
  { "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  }
derive instance newtypeDeleteSnapshotResult :: Newtype DeleteSnapshotResult _
derive instance repGenericDeleteSnapshotResult :: Generic DeleteSnapshotResult _
instance showDeleteSnapshotResult :: Show DeleteSnapshotResult where
  show = genericShow
instance decodeDeleteSnapshotResult :: Decode DeleteSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteSnapshotResult :: Encode DeleteSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Deletes the local side of an existing trust relationship between the Microsoft AD in the AWS cloud and the external domain.</p>
newtype DeleteTrustRequest = DeleteTrustRequest 
  { "TrustId" :: (TrustId)
  , "DeleteAssociatedConditionalForwarder" :: NullOrUndefined.NullOrUndefined (DeleteAssociatedConditionalForwarder)
  }
derive instance newtypeDeleteTrustRequest :: Newtype DeleteTrustRequest _
derive instance repGenericDeleteTrustRequest :: Generic DeleteTrustRequest _
instance showDeleteTrustRequest :: Show DeleteTrustRequest where
  show = genericShow
instance decodeDeleteTrustRequest :: Decode DeleteTrustRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTrustRequest :: Encode DeleteTrustRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a DeleteTrust request.</p>
newtype DeleteTrustResult = DeleteTrustResult 
  { "TrustId" :: NullOrUndefined.NullOrUndefined (TrustId)
  }
derive instance newtypeDeleteTrustResult :: Newtype DeleteTrustResult _
derive instance repGenericDeleteTrustResult :: Generic DeleteTrustResult _
instance showDeleteTrustResult :: Show DeleteTrustResult where
  show = genericShow
instance decodeDeleteTrustResult :: Decode DeleteTrustResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTrustResult :: Encode DeleteTrustResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Removes the specified directory as a publisher to the specified SNS topic.</p>
newtype DeregisterEventTopicRequest = DeregisterEventTopicRequest 
  { "DirectoryId" :: (DirectoryId)
  , "TopicName" :: (TopicName)
  }
derive instance newtypeDeregisterEventTopicRequest :: Newtype DeregisterEventTopicRequest _
derive instance repGenericDeregisterEventTopicRequest :: Generic DeregisterEventTopicRequest _
instance showDeregisterEventTopicRequest :: Show DeregisterEventTopicRequest where
  show = genericShow
instance decodeDeregisterEventTopicRequest :: Decode DeregisterEventTopicRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterEventTopicRequest :: Encode DeregisterEventTopicRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a DeregisterEventTopic request.</p>
newtype DeregisterEventTopicResult = DeregisterEventTopicResult Types.NoArguments
derive instance newtypeDeregisterEventTopicResult :: Newtype DeregisterEventTopicResult _
derive instance repGenericDeregisterEventTopicResult :: Generic DeregisterEventTopicResult _
instance showDeregisterEventTopicResult :: Show DeregisterEventTopicResult where
  show = genericShow
instance decodeDeregisterEventTopicResult :: Decode DeregisterEventTopicResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeregisterEventTopicResult :: Encode DeregisterEventTopicResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a conditional forwarder.</p>
newtype DescribeConditionalForwardersRequest = DescribeConditionalForwardersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainNames" :: NullOrUndefined.NullOrUndefined (RemoteDomainNames)
  }
derive instance newtypeDescribeConditionalForwardersRequest :: Newtype DescribeConditionalForwardersRequest _
derive instance repGenericDescribeConditionalForwardersRequest :: Generic DescribeConditionalForwardersRequest _
instance showDescribeConditionalForwardersRequest :: Show DescribeConditionalForwardersRequest where
  show = genericShow
instance decodeDescribeConditionalForwardersRequest :: Decode DescribeConditionalForwardersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConditionalForwardersRequest :: Encode DescribeConditionalForwardersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a DescribeConditionalForwarder request.</p>
newtype DescribeConditionalForwardersResult = DescribeConditionalForwardersResult 
  { "ConditionalForwarders" :: NullOrUndefined.NullOrUndefined (ConditionalForwarders)
  }
derive instance newtypeDescribeConditionalForwardersResult :: Newtype DescribeConditionalForwardersResult _
derive instance repGenericDescribeConditionalForwardersResult :: Generic DescribeConditionalForwardersResult _
instance showDescribeConditionalForwardersResult :: Show DescribeConditionalForwardersResult where
  show = genericShow
instance decodeDescribeConditionalForwardersResult :: Decode DescribeConditionalForwardersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeConditionalForwardersResult :: Encode DescribeConditionalForwardersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DescribeDirectories</a> operation.</p>
newtype DescribeDirectoriesRequest = DescribeDirectoriesRequest 
  { "DirectoryIds" :: NullOrUndefined.NullOrUndefined (DirectoryIds)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  }
derive instance newtypeDescribeDirectoriesRequest :: Newtype DescribeDirectoriesRequest _
derive instance repGenericDescribeDirectoriesRequest :: Generic DescribeDirectoriesRequest _
instance showDescribeDirectoriesRequest :: Show DescribeDirectoriesRequest where
  show = genericShow
instance decodeDescribeDirectoriesRequest :: Decode DescribeDirectoriesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDirectoriesRequest :: Encode DescribeDirectoriesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>DescribeDirectories</a> operation.</p>
newtype DescribeDirectoriesResult = DescribeDirectoriesResult 
  { "DirectoryDescriptions" :: NullOrUndefined.NullOrUndefined (DirectoryDescriptions)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeDirectoriesResult :: Newtype DescribeDirectoriesResult _
derive instance repGenericDescribeDirectoriesResult :: Generic DescribeDirectoriesResult _
instance showDescribeDirectoriesResult :: Show DescribeDirectoriesResult where
  show = genericShow
instance decodeDescribeDirectoriesResult :: Decode DescribeDirectoriesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDirectoriesResult :: Encode DescribeDirectoriesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDomainControllersRequest = DescribeDomainControllersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "DomainControllerIds" :: NullOrUndefined.NullOrUndefined (DomainControllerIds)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  }
derive instance newtypeDescribeDomainControllersRequest :: Newtype DescribeDomainControllersRequest _
derive instance repGenericDescribeDomainControllersRequest :: Generic DescribeDomainControllersRequest _
instance showDescribeDomainControllersRequest :: Show DescribeDomainControllersRequest where
  show = genericShow
instance decodeDescribeDomainControllersRequest :: Decode DescribeDomainControllersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDomainControllersRequest :: Encode DescribeDomainControllersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDomainControllersResult = DescribeDomainControllersResult 
  { "DomainControllers" :: NullOrUndefined.NullOrUndefined (DomainControllers)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeDomainControllersResult :: Newtype DescribeDomainControllersResult _
derive instance repGenericDescribeDomainControllersResult :: Generic DescribeDomainControllersResult _
instance showDescribeDomainControllersResult :: Show DescribeDomainControllersResult where
  show = genericShow
instance decodeDescribeDomainControllersResult :: Decode DescribeDomainControllersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDomainControllersResult :: Encode DescribeDomainControllersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes event topics.</p>
newtype DescribeEventTopicsRequest = DescribeEventTopicsRequest 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "TopicNames" :: NullOrUndefined.NullOrUndefined (TopicNames)
  }
derive instance newtypeDescribeEventTopicsRequest :: Newtype DescribeEventTopicsRequest _
derive instance repGenericDescribeEventTopicsRequest :: Generic DescribeEventTopicsRequest _
instance showDescribeEventTopicsRequest :: Show DescribeEventTopicsRequest where
  show = genericShow
instance decodeDescribeEventTopicsRequest :: Decode DescribeEventTopicsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventTopicsRequest :: Encode DescribeEventTopicsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a DescribeEventTopic request.</p>
newtype DescribeEventTopicsResult = DescribeEventTopicsResult 
  { "EventTopics" :: NullOrUndefined.NullOrUndefined (EventTopics)
  }
derive instance newtypeDescribeEventTopicsResult :: Newtype DescribeEventTopicsResult _
derive instance repGenericDescribeEventTopicsResult :: Generic DescribeEventTopicsResult _
instance showDescribeEventTopicsResult :: Show DescribeEventTopicsResult where
  show = genericShow
instance decodeDescribeEventTopicsResult :: Decode DescribeEventTopicsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeEventTopicsResult :: Encode DescribeEventTopicsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DescribeSnapshots</a> operation.</p>
newtype DescribeSnapshotsRequest = DescribeSnapshotsRequest 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "SnapshotIds" :: NullOrUndefined.NullOrUndefined (SnapshotIds)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  }
derive instance newtypeDescribeSnapshotsRequest :: Newtype DescribeSnapshotsRequest _
derive instance repGenericDescribeSnapshotsRequest :: Generic DescribeSnapshotsRequest _
instance showDescribeSnapshotsRequest :: Show DescribeSnapshotsRequest where
  show = genericShow
instance decodeDescribeSnapshotsRequest :: Decode DescribeSnapshotsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSnapshotsRequest :: Encode DescribeSnapshotsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>DescribeSnapshots</a> operation.</p>
newtype DescribeSnapshotsResult = DescribeSnapshotsResult 
  { "Snapshots" :: NullOrUndefined.NullOrUndefined (Snapshots)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeSnapshotsResult :: Newtype DescribeSnapshotsResult _
derive instance repGenericDescribeSnapshotsResult :: Generic DescribeSnapshotsResult _
instance showDescribeSnapshotsResult :: Show DescribeSnapshotsResult where
  show = genericShow
instance decodeDescribeSnapshotsResult :: Decode DescribeSnapshotsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeSnapshotsResult :: Encode DescribeSnapshotsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the trust relationships for a particular Microsoft AD in the AWS cloud. If no input parameters are are provided, such as directory ID or trust ID, this request describes all the trust relationships.</p>
newtype DescribeTrustsRequest = DescribeTrustsRequest 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "TrustIds" :: NullOrUndefined.NullOrUndefined (TrustIds)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  }
derive instance newtypeDescribeTrustsRequest :: Newtype DescribeTrustsRequest _
derive instance repGenericDescribeTrustsRequest :: Generic DescribeTrustsRequest _
instance showDescribeTrustsRequest :: Show DescribeTrustsRequest where
  show = genericShow
instance decodeDescribeTrustsRequest :: Decode DescribeTrustsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTrustsRequest :: Encode DescribeTrustsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a DescribeTrust request.</p>
newtype DescribeTrustsResult = DescribeTrustsResult 
  { "Trusts" :: NullOrUndefined.NullOrUndefined (Trusts)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeTrustsResult :: Newtype DescribeTrustsResult _
derive instance repGenericDescribeTrustsResult :: Generic DescribeTrustsResult _
instance showDescribeTrustsResult :: Show DescribeTrustsResult where
  show = genericShow
instance decodeDescribeTrustsResult :: Decode DescribeTrustsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTrustsResult :: Encode DescribeTrustsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _
derive instance repGenericDescription :: Generic Description _
instance showDescription :: Show Description where
  show = genericShow
instance decodeDescription :: Decode Description where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescription :: Encode Description where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DesiredNumberOfDomainControllers = DesiredNumberOfDomainControllers Int
derive instance newtypeDesiredNumberOfDomainControllers :: Newtype DesiredNumberOfDomainControllers _
derive instance repGenericDesiredNumberOfDomainControllers :: Generic DesiredNumberOfDomainControllers _
instance showDesiredNumberOfDomainControllers :: Show DesiredNumberOfDomainControllers where
  show = genericShow
instance decodeDesiredNumberOfDomainControllers :: Decode DesiredNumberOfDomainControllers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDesiredNumberOfDomainControllers :: Encode DesiredNumberOfDomainControllers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information for the <a>ConnectDirectory</a> operation when an AD Connector directory is being created.</p>
newtype DirectoryConnectSettings = DirectoryConnectSettings 
  { "VpcId" :: (VpcId)
  , "SubnetIds" :: (SubnetIds)
  , "CustomerDnsIps" :: (DnsIpAddrs)
  , "CustomerUserName" :: (UserName)
  }
derive instance newtypeDirectoryConnectSettings :: Newtype DirectoryConnectSettings _
derive instance repGenericDirectoryConnectSettings :: Generic DirectoryConnectSettings _
instance showDirectoryConnectSettings :: Show DirectoryConnectSettings where
  show = genericShow
instance decodeDirectoryConnectSettings :: Decode DirectoryConnectSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryConnectSettings :: Encode DirectoryConnectSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about an AD Connector directory.</p>
newtype DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription 
  { "VpcId" :: NullOrUndefined.NullOrUndefined (VpcId)
  , "SubnetIds" :: NullOrUndefined.NullOrUndefined (SubnetIds)
  , "CustomerUserName" :: NullOrUndefined.NullOrUndefined (UserName)
  , "SecurityGroupId" :: NullOrUndefined.NullOrUndefined (SecurityGroupId)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZones)
  , "ConnectIps" :: NullOrUndefined.NullOrUndefined (IpAddrs)
  }
derive instance newtypeDirectoryConnectSettingsDescription :: Newtype DirectoryConnectSettingsDescription _
derive instance repGenericDirectoryConnectSettingsDescription :: Generic DirectoryConnectSettingsDescription _
instance showDirectoryConnectSettingsDescription :: Show DirectoryConnectSettingsDescription where
  show = genericShow
instance decodeDirectoryConnectSettingsDescription :: Decode DirectoryConnectSettingsDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryConnectSettingsDescription :: Encode DirectoryConnectSettingsDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about an AWS Directory Service directory.</p>
newtype DirectoryDescription = DirectoryDescription 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "Name" :: NullOrUndefined.NullOrUndefined (DirectoryName)
  , "ShortName" :: NullOrUndefined.NullOrUndefined (DirectoryShortName)
  , "Size" :: NullOrUndefined.NullOrUndefined (DirectorySize)
  , "Edition" :: NullOrUndefined.NullOrUndefined (DirectoryEdition)
  , "Alias" :: NullOrUndefined.NullOrUndefined (AliasName)
  , "AccessUrl" :: NullOrUndefined.NullOrUndefined (AccessUrl)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "DnsIpAddrs" :: NullOrUndefined.NullOrUndefined (DnsIpAddrs)
  , "Stage" :: NullOrUndefined.NullOrUndefined (DirectoryStage)
  , "LaunchTime" :: NullOrUndefined.NullOrUndefined (LaunchTime)
  , "StageLastUpdatedDateTime" :: NullOrUndefined.NullOrUndefined (LastUpdatedDateTime)
  , "Type" :: NullOrUndefined.NullOrUndefined (DirectoryType)
  , "VpcSettings" :: NullOrUndefined.NullOrUndefined (DirectoryVpcSettingsDescription)
  , "ConnectSettings" :: NullOrUndefined.NullOrUndefined (DirectoryConnectSettingsDescription)
  , "RadiusSettings" :: NullOrUndefined.NullOrUndefined (RadiusSettings)
  , "RadiusStatus" :: NullOrUndefined.NullOrUndefined (RadiusStatus)
  , "StageReason" :: NullOrUndefined.NullOrUndefined (StageReason)
  , "SsoEnabled" :: NullOrUndefined.NullOrUndefined (SsoEnabled)
  , "DesiredNumberOfDomainControllers" :: NullOrUndefined.NullOrUndefined (DesiredNumberOfDomainControllers)
  }
derive instance newtypeDirectoryDescription :: Newtype DirectoryDescription _
derive instance repGenericDirectoryDescription :: Generic DirectoryDescription _
instance showDirectoryDescription :: Show DirectoryDescription where
  show = genericShow
instance decodeDirectoryDescription :: Decode DirectoryDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryDescription :: Encode DirectoryDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of directory descriptions.</p>
newtype DirectoryDescriptions = DirectoryDescriptions (Array DirectoryDescription)
derive instance newtypeDirectoryDescriptions :: Newtype DirectoryDescriptions _
derive instance repGenericDirectoryDescriptions :: Generic DirectoryDescriptions _
instance showDirectoryDescriptions :: Show DirectoryDescriptions where
  show = genericShow
instance decodeDirectoryDescriptions :: Decode DirectoryDescriptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryDescriptions :: Encode DirectoryDescriptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryEdition = DirectoryEdition String
derive instance newtypeDirectoryEdition :: Newtype DirectoryEdition _
derive instance repGenericDirectoryEdition :: Generic DirectoryEdition _
instance showDirectoryEdition :: Show DirectoryEdition where
  show = genericShow
instance decodeDirectoryEdition :: Decode DirectoryEdition where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryEdition :: Encode DirectoryEdition where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryId = DirectoryId String
derive instance newtypeDirectoryId :: Newtype DirectoryId _
derive instance repGenericDirectoryId :: Generic DirectoryId _
instance showDirectoryId :: Show DirectoryId where
  show = genericShow
instance decodeDirectoryId :: Decode DirectoryId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryId :: Encode DirectoryId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of directory identifiers.</p>
newtype DirectoryIds = DirectoryIds (Array DirectoryId)
derive instance newtypeDirectoryIds :: Newtype DirectoryIds _
derive instance repGenericDirectoryIds :: Generic DirectoryIds _
instance showDirectoryIds :: Show DirectoryIds where
  show = genericShow
instance decodeDirectoryIds :: Decode DirectoryIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryIds :: Encode DirectoryIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum number of directories in the region has been reached. You can use the <a>GetDirectoryLimits</a> operation to determine your directory limits in the region.</p>
newtype DirectoryLimitExceededException = DirectoryLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeDirectoryLimitExceededException :: Newtype DirectoryLimitExceededException _
derive instance repGenericDirectoryLimitExceededException :: Generic DirectoryLimitExceededException _
instance showDirectoryLimitExceededException :: Show DirectoryLimitExceededException where
  show = genericShow
instance decodeDirectoryLimitExceededException :: Decode DirectoryLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryLimitExceededException :: Encode DirectoryLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains directory limit information for a region.</p>
newtype DirectoryLimits = DirectoryLimits 
  { "CloudOnlyDirectoriesLimit" :: NullOrUndefined.NullOrUndefined (Limit)
  , "CloudOnlyDirectoriesCurrentCount" :: NullOrUndefined.NullOrUndefined (Limit)
  , "CloudOnlyDirectoriesLimitReached" :: NullOrUndefined.NullOrUndefined (CloudOnlyDirectoriesLimitReached)
  , "CloudOnlyMicrosoftADLimit" :: NullOrUndefined.NullOrUndefined (Limit)
  , "CloudOnlyMicrosoftADCurrentCount" :: NullOrUndefined.NullOrUndefined (Limit)
  , "CloudOnlyMicrosoftADLimitReached" :: NullOrUndefined.NullOrUndefined (CloudOnlyDirectoriesLimitReached)
  , "ConnectedDirectoriesLimit" :: NullOrUndefined.NullOrUndefined (Limit)
  , "ConnectedDirectoriesCurrentCount" :: NullOrUndefined.NullOrUndefined (Limit)
  , "ConnectedDirectoriesLimitReached" :: NullOrUndefined.NullOrUndefined (ConnectedDirectoriesLimitReached)
  }
derive instance newtypeDirectoryLimits :: Newtype DirectoryLimits _
derive instance repGenericDirectoryLimits :: Generic DirectoryLimits _
instance showDirectoryLimits :: Show DirectoryLimits where
  show = genericShow
instance decodeDirectoryLimits :: Decode DirectoryLimits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryLimits :: Encode DirectoryLimits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryName = DirectoryName String
derive instance newtypeDirectoryName :: Newtype DirectoryName _
derive instance repGenericDirectoryName :: Generic DirectoryName _
instance showDirectoryName :: Show DirectoryName where
  show = genericShow
instance decodeDirectoryName :: Decode DirectoryName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryName :: Encode DirectoryName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryShortName = DirectoryShortName String
derive instance newtypeDirectoryShortName :: Newtype DirectoryShortName _
derive instance repGenericDirectoryShortName :: Generic DirectoryShortName _
instance showDirectoryShortName :: Show DirectoryShortName where
  show = genericShow
instance decodeDirectoryShortName :: Decode DirectoryShortName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryShortName :: Encode DirectoryShortName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectorySize = DirectorySize String
derive instance newtypeDirectorySize :: Newtype DirectorySize _
derive instance repGenericDirectorySize :: Generic DirectorySize _
instance showDirectorySize :: Show DirectorySize where
  show = genericShow
instance decodeDirectorySize :: Decode DirectorySize where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectorySize :: Encode DirectorySize where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryStage = DirectoryStage String
derive instance newtypeDirectoryStage :: Newtype DirectoryStage _
derive instance repGenericDirectoryStage :: Generic DirectoryStage _
instance showDirectoryStage :: Show DirectoryStage where
  show = genericShow
instance decodeDirectoryStage :: Decode DirectoryStage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryStage :: Encode DirectoryStage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryType = DirectoryType String
derive instance newtypeDirectoryType :: Newtype DirectoryType _
derive instance repGenericDirectoryType :: Generic DirectoryType _
instance showDirectoryType :: Show DirectoryType where
  show = genericShow
instance decodeDirectoryType :: Decode DirectoryType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryType :: Encode DirectoryType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified directory is unavailable or could not be found.</p>
newtype DirectoryUnavailableException = DirectoryUnavailableException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeDirectoryUnavailableException :: Newtype DirectoryUnavailableException _
derive instance repGenericDirectoryUnavailableException :: Generic DirectoryUnavailableException _
instance showDirectoryUnavailableException :: Show DirectoryUnavailableException where
  show = genericShow
instance decodeDirectoryUnavailableException :: Decode DirectoryUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryUnavailableException :: Encode DirectoryUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains VPC information for the <a>CreateDirectory</a> or <a>CreateMicrosoftAD</a> operation.</p>
newtype DirectoryVpcSettings = DirectoryVpcSettings 
  { "VpcId" :: (VpcId)
  , "SubnetIds" :: (SubnetIds)
  }
derive instance newtypeDirectoryVpcSettings :: Newtype DirectoryVpcSettings _
derive instance repGenericDirectoryVpcSettings :: Generic DirectoryVpcSettings _
instance showDirectoryVpcSettings :: Show DirectoryVpcSettings where
  show = genericShow
instance decodeDirectoryVpcSettings :: Decode DirectoryVpcSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryVpcSettings :: Encode DirectoryVpcSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the directory.</p>
newtype DirectoryVpcSettingsDescription = DirectoryVpcSettingsDescription 
  { "VpcId" :: NullOrUndefined.NullOrUndefined (VpcId)
  , "SubnetIds" :: NullOrUndefined.NullOrUndefined (SubnetIds)
  , "SecurityGroupId" :: NullOrUndefined.NullOrUndefined (SecurityGroupId)
  , "AvailabilityZones" :: NullOrUndefined.NullOrUndefined (AvailabilityZones)
  }
derive instance newtypeDirectoryVpcSettingsDescription :: Newtype DirectoryVpcSettingsDescription _
derive instance repGenericDirectoryVpcSettingsDescription :: Generic DirectoryVpcSettingsDescription _
instance showDirectoryVpcSettingsDescription :: Show DirectoryVpcSettingsDescription where
  show = genericShow
instance decodeDirectoryVpcSettingsDescription :: Decode DirectoryVpcSettingsDescription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryVpcSettingsDescription :: Encode DirectoryVpcSettingsDescription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DisableRadius</a> operation.</p>
newtype DisableRadiusRequest = DisableRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  }
derive instance newtypeDisableRadiusRequest :: Newtype DisableRadiusRequest _
derive instance repGenericDisableRadiusRequest :: Generic DisableRadiusRequest _
instance showDisableRadiusRequest :: Show DisableRadiusRequest where
  show = genericShow
instance decodeDisableRadiusRequest :: Decode DisableRadiusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableRadiusRequest :: Encode DisableRadiusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>DisableRadius</a> operation.</p>
newtype DisableRadiusResult = DisableRadiusResult Types.NoArguments
derive instance newtypeDisableRadiusResult :: Newtype DisableRadiusResult _
derive instance repGenericDisableRadiusResult :: Generic DisableRadiusResult _
instance showDisableRadiusResult :: Show DisableRadiusResult where
  show = genericShow
instance decodeDisableRadiusResult :: Decode DisableRadiusResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableRadiusResult :: Encode DisableRadiusResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>DisableSso</a> operation.</p>
newtype DisableSsoRequest = DisableSsoRequest 
  { "DirectoryId" :: (DirectoryId)
  , "UserName" :: NullOrUndefined.NullOrUndefined (UserName)
  , "Password" :: NullOrUndefined.NullOrUndefined (ConnectPassword)
  }
derive instance newtypeDisableSsoRequest :: Newtype DisableSsoRequest _
derive instance repGenericDisableSsoRequest :: Generic DisableSsoRequest _
instance showDisableSsoRequest :: Show DisableSsoRequest where
  show = genericShow
instance decodeDisableSsoRequest :: Decode DisableSsoRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableSsoRequest :: Encode DisableSsoRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>DisableSso</a> operation.</p>
newtype DisableSsoResult = DisableSsoResult Types.NoArguments
derive instance newtypeDisableSsoResult :: Newtype DisableSsoResult _
derive instance repGenericDisableSsoResult :: Generic DisableSsoResult _
instance showDisableSsoResult :: Show DisableSsoResult where
  show = genericShow
instance decodeDisableSsoResult :: Decode DisableSsoResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDisableSsoResult :: Encode DisableSsoResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DnsIpAddrs = DnsIpAddrs (Array IpAddr)
derive instance newtypeDnsIpAddrs :: Newtype DnsIpAddrs _
derive instance repGenericDnsIpAddrs :: Generic DnsIpAddrs _
instance showDnsIpAddrs :: Show DnsIpAddrs where
  show = genericShow
instance decodeDnsIpAddrs :: Decode DnsIpAddrs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDnsIpAddrs :: Encode DnsIpAddrs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about the domain controllers for a specified directory.</p>
newtype DomainController = DomainController 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "DomainControllerId" :: NullOrUndefined.NullOrUndefined (DomainControllerId)
  , "DnsIpAddr" :: NullOrUndefined.NullOrUndefined (IpAddr)
  , "VpcId" :: NullOrUndefined.NullOrUndefined (VpcId)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (SubnetId)
  , "AvailabilityZone" :: NullOrUndefined.NullOrUndefined (AvailabilityZone)
  , "Status" :: NullOrUndefined.NullOrUndefined (DomainControllerStatus)
  , "StatusReason" :: NullOrUndefined.NullOrUndefined (DomainControllerStatusReason)
  , "LaunchTime" :: NullOrUndefined.NullOrUndefined (LaunchTime)
  , "StatusLastUpdatedDateTime" :: NullOrUndefined.NullOrUndefined (LastUpdatedDateTime)
  }
derive instance newtypeDomainController :: Newtype DomainController _
derive instance repGenericDomainController :: Generic DomainController _
instance showDomainController :: Show DomainController where
  show = genericShow
instance decodeDomainController :: Decode DomainController where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainController :: Encode DomainController where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainControllerId = DomainControllerId String
derive instance newtypeDomainControllerId :: Newtype DomainControllerId _
derive instance repGenericDomainControllerId :: Generic DomainControllerId _
instance showDomainControllerId :: Show DomainControllerId where
  show = genericShow
instance decodeDomainControllerId :: Decode DomainControllerId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainControllerId :: Encode DomainControllerId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainControllerIds = DomainControllerIds (Array DomainControllerId)
derive instance newtypeDomainControllerIds :: Newtype DomainControllerIds _
derive instance repGenericDomainControllerIds :: Generic DomainControllerIds _
instance showDomainControllerIds :: Show DomainControllerIds where
  show = genericShow
instance decodeDomainControllerIds :: Decode DomainControllerIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainControllerIds :: Encode DomainControllerIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum allowed number of domain controllers per directory was exceeded. The default limit per directory is 20 domain controllers.</p>
newtype DomainControllerLimitExceededException = DomainControllerLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeDomainControllerLimitExceededException :: Newtype DomainControllerLimitExceededException _
derive instance repGenericDomainControllerLimitExceededException :: Generic DomainControllerLimitExceededException _
instance showDomainControllerLimitExceededException :: Show DomainControllerLimitExceededException where
  show = genericShow
instance decodeDomainControllerLimitExceededException :: Decode DomainControllerLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainControllerLimitExceededException :: Encode DomainControllerLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainControllerStatus = DomainControllerStatus String
derive instance newtypeDomainControllerStatus :: Newtype DomainControllerStatus _
derive instance repGenericDomainControllerStatus :: Generic DomainControllerStatus _
instance showDomainControllerStatus :: Show DomainControllerStatus where
  show = genericShow
instance decodeDomainControllerStatus :: Decode DomainControllerStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainControllerStatus :: Encode DomainControllerStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainControllerStatusReason = DomainControllerStatusReason String
derive instance newtypeDomainControllerStatusReason :: Newtype DomainControllerStatusReason _
derive instance repGenericDomainControllerStatusReason :: Generic DomainControllerStatusReason _
instance showDomainControllerStatusReason :: Show DomainControllerStatusReason where
  show = genericShow
instance decodeDomainControllerStatusReason :: Decode DomainControllerStatusReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainControllerStatusReason :: Encode DomainControllerStatusReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DomainControllers = DomainControllers (Array DomainController)
derive instance newtypeDomainControllers :: Newtype DomainControllers _
derive instance repGenericDomainControllers :: Generic DomainControllers _
instance showDomainControllers :: Show DomainControllers where
  show = genericShow
instance decodeDomainControllers :: Decode DomainControllers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDomainControllers :: Encode DomainControllers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>EnableRadius</a> operation.</p>
newtype EnableRadiusRequest = EnableRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RadiusSettings" :: (RadiusSettings)
  }
derive instance newtypeEnableRadiusRequest :: Newtype EnableRadiusRequest _
derive instance repGenericEnableRadiusRequest :: Generic EnableRadiusRequest _
instance showEnableRadiusRequest :: Show EnableRadiusRequest where
  show = genericShow
instance decodeEnableRadiusRequest :: Decode EnableRadiusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableRadiusRequest :: Encode EnableRadiusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>EnableRadius</a> operation.</p>
newtype EnableRadiusResult = EnableRadiusResult Types.NoArguments
derive instance newtypeEnableRadiusResult :: Newtype EnableRadiusResult _
derive instance repGenericEnableRadiusResult :: Generic EnableRadiusResult _
instance showEnableRadiusResult :: Show EnableRadiusResult where
  show = genericShow
instance decodeEnableRadiusResult :: Decode EnableRadiusResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableRadiusResult :: Encode EnableRadiusResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>EnableSso</a> operation.</p>
newtype EnableSsoRequest = EnableSsoRequest 
  { "DirectoryId" :: (DirectoryId)
  , "UserName" :: NullOrUndefined.NullOrUndefined (UserName)
  , "Password" :: NullOrUndefined.NullOrUndefined (ConnectPassword)
  }
derive instance newtypeEnableSsoRequest :: Newtype EnableSsoRequest _
derive instance repGenericEnableSsoRequest :: Generic EnableSsoRequest _
instance showEnableSsoRequest :: Show EnableSsoRequest where
  show = genericShow
instance decodeEnableSsoRequest :: Decode EnableSsoRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableSsoRequest :: Encode EnableSsoRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>EnableSso</a> operation.</p>
newtype EnableSsoResult = EnableSsoResult Types.NoArguments
derive instance newtypeEnableSsoResult :: Newtype EnableSsoResult _
derive instance repGenericEnableSsoResult :: Generic EnableSsoResult _
instance showEnableSsoResult :: Show EnableSsoResult where
  show = genericShow
instance decodeEnableSsoResult :: Decode EnableSsoResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEnableSsoResult :: Encode EnableSsoResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EndDateTime = EndDateTime Number
derive instance newtypeEndDateTime :: Newtype EndDateTime _
derive instance repGenericEndDateTime :: Generic EndDateTime _
instance showEndDateTime :: Show EndDateTime where
  show = genericShow
instance decodeEndDateTime :: Decode EndDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEndDateTime :: Encode EndDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified entity already exists.</p>
newtype EntityAlreadyExistsException = EntityAlreadyExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeEntityAlreadyExistsException :: Newtype EntityAlreadyExistsException _
derive instance repGenericEntityAlreadyExistsException :: Generic EntityAlreadyExistsException _
instance showEntityAlreadyExistsException :: Show EntityAlreadyExistsException where
  show = genericShow
instance decodeEntityAlreadyExistsException :: Decode EntityAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityAlreadyExistsException :: Encode EntityAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified entity could not be found.</p>
newtype EntityDoesNotExistException = EntityDoesNotExistException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeEntityDoesNotExistException :: Newtype EntityDoesNotExistException _
derive instance repGenericEntityDoesNotExistException :: Generic EntityDoesNotExistException _
instance showEntityDoesNotExistException :: Show EntityDoesNotExistException where
  show = genericShow
instance decodeEntityDoesNotExistException :: Decode EntityDoesNotExistException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityDoesNotExistException :: Encode EntityDoesNotExistException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about SNS topic and AWS Directory Service directory associations.</p>
newtype EventTopic = EventTopic 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "TopicName" :: NullOrUndefined.NullOrUndefined (TopicName)
  , "TopicArn" :: NullOrUndefined.NullOrUndefined (TopicArn)
  , "CreatedDateTime" :: NullOrUndefined.NullOrUndefined (CreatedDateTime)
  , "Status" :: NullOrUndefined.NullOrUndefined (TopicStatus)
  }
derive instance newtypeEventTopic :: Newtype EventTopic _
derive instance repGenericEventTopic :: Generic EventTopic _
instance showEventTopic :: Show EventTopic where
  show = genericShow
instance decodeEventTopic :: Decode EventTopic where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTopic :: Encode EventTopic where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EventTopics = EventTopics (Array EventTopic)
derive instance newtypeEventTopics :: Newtype EventTopics _
derive instance repGenericEventTopics :: Generic EventTopics _
instance showEventTopics :: Show EventTopics where
  show = genericShow
instance decodeEventTopics :: Decode EventTopics where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEventTopics :: Encode EventTopics where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The descriptive message for the exception.</p>
newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _
derive instance repGenericExceptionMessage :: Generic ExceptionMessage _
instance showExceptionMessage :: Show ExceptionMessage where
  show = genericShow
instance decodeExceptionMessage :: Decode ExceptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExceptionMessage :: Encode ExceptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>GetDirectoryLimits</a> operation.</p>
newtype GetDirectoryLimitsRequest = GetDirectoryLimitsRequest Types.NoArguments
derive instance newtypeGetDirectoryLimitsRequest :: Newtype GetDirectoryLimitsRequest _
derive instance repGenericGetDirectoryLimitsRequest :: Generic GetDirectoryLimitsRequest _
instance showGetDirectoryLimitsRequest :: Show GetDirectoryLimitsRequest where
  show = genericShow
instance decodeGetDirectoryLimitsRequest :: Decode GetDirectoryLimitsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDirectoryLimitsRequest :: Encode GetDirectoryLimitsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>GetDirectoryLimits</a> operation.</p>
newtype GetDirectoryLimitsResult = GetDirectoryLimitsResult 
  { "DirectoryLimits" :: NullOrUndefined.NullOrUndefined (DirectoryLimits)
  }
derive instance newtypeGetDirectoryLimitsResult :: Newtype GetDirectoryLimitsResult _
derive instance repGenericGetDirectoryLimitsResult :: Generic GetDirectoryLimitsResult _
instance showGetDirectoryLimitsResult :: Show GetDirectoryLimitsResult where
  show = genericShow
instance decodeGetDirectoryLimitsResult :: Decode GetDirectoryLimitsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDirectoryLimitsResult :: Encode GetDirectoryLimitsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>GetSnapshotLimits</a> operation.</p>
newtype GetSnapshotLimitsRequest = GetSnapshotLimitsRequest 
  { "DirectoryId" :: (DirectoryId)
  }
derive instance newtypeGetSnapshotLimitsRequest :: Newtype GetSnapshotLimitsRequest _
derive instance repGenericGetSnapshotLimitsRequest :: Generic GetSnapshotLimitsRequest _
instance showGetSnapshotLimitsRequest :: Show GetSnapshotLimitsRequest where
  show = genericShow
instance decodeGetSnapshotLimitsRequest :: Decode GetSnapshotLimitsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSnapshotLimitsRequest :: Encode GetSnapshotLimitsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>GetSnapshotLimits</a> operation.</p>
newtype GetSnapshotLimitsResult = GetSnapshotLimitsResult 
  { "SnapshotLimits" :: NullOrUndefined.NullOrUndefined (SnapshotLimits)
  }
derive instance newtypeGetSnapshotLimitsResult :: Newtype GetSnapshotLimitsResult _
derive instance repGenericGetSnapshotLimitsResult :: Generic GetSnapshotLimitsResult _
instance showGetSnapshotLimitsResult :: Show GetSnapshotLimitsResult where
  show = genericShow
instance decodeGetSnapshotLimitsResult :: Decode GetSnapshotLimitsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetSnapshotLimitsResult :: Encode GetSnapshotLimitsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The account does not have sufficient permission to perform the operation.</p>
newtype InsufficientPermissionsException = InsufficientPermissionsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeInsufficientPermissionsException :: Newtype InsufficientPermissionsException _
derive instance repGenericInsufficientPermissionsException :: Generic InsufficientPermissionsException _
instance showInsufficientPermissionsException :: Show InsufficientPermissionsException where
  show = genericShow
instance decodeInsufficientPermissionsException :: Decode InsufficientPermissionsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInsufficientPermissionsException :: Encode InsufficientPermissionsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The <i>NextToken</i> value is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _
derive instance repGenericInvalidNextTokenException :: Generic InvalidNextTokenException _
instance showInvalidNextTokenException :: Show InvalidNextTokenException where
  show = genericShow
instance decodeInvalidNextTokenException :: Decode InvalidNextTokenException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidNextTokenException :: Encode InvalidNextTokenException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more parameters are not valid.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _
derive instance repGenericInvalidParameterException :: Generic InvalidParameterException _
instance showInvalidParameterException :: Show InvalidParameterException where
  show = genericShow
instance decodeInvalidParameterException :: Decode InvalidParameterException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterException :: Encode InvalidParameterException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddr = IpAddr String
derive instance newtypeIpAddr :: Newtype IpAddr _
derive instance repGenericIpAddr :: Generic IpAddr _
instance showIpAddr :: Show IpAddr where
  show = genericShow
instance decodeIpAddr :: Decode IpAddr where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddr :: Encode IpAddr where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddrs = IpAddrs (Array IpAddr)
derive instance newtypeIpAddrs :: Newtype IpAddrs _
derive instance repGenericIpAddrs :: Generic IpAddrs _
instance showIpAddrs :: Show IpAddrs where
  show = genericShow
instance decodeIpAddrs :: Decode IpAddrs where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddrs :: Encode IpAddrs where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>IP address block. This is often the address block of the DNS server used for your on-premises domain. </p>
newtype IpRoute = IpRoute 
  { "CidrIp" :: NullOrUndefined.NullOrUndefined (CidrIp)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeIpRoute :: Newtype IpRoute _
derive instance repGenericIpRoute :: Generic IpRoute _
instance showIpRoute :: Show IpRoute where
  show = genericShow
instance decodeIpRoute :: Decode IpRoute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpRoute :: Encode IpRoute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about one or more IP address blocks.</p>
newtype IpRouteInfo = IpRouteInfo 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "CidrIp" :: NullOrUndefined.NullOrUndefined (CidrIp)
  , "IpRouteStatusMsg" :: NullOrUndefined.NullOrUndefined (IpRouteStatusMsg)
  , "AddedDateTime" :: NullOrUndefined.NullOrUndefined (AddedDateTime)
  , "IpRouteStatusReason" :: NullOrUndefined.NullOrUndefined (IpRouteStatusReason)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeIpRouteInfo :: Newtype IpRouteInfo _
derive instance repGenericIpRouteInfo :: Generic IpRouteInfo _
instance showIpRouteInfo :: Show IpRouteInfo where
  show = genericShow
instance decodeIpRouteInfo :: Decode IpRouteInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpRouteInfo :: Encode IpRouteInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum allowed number of IP addresses was exceeded. The default limit is 100 IP address blocks.</p>
newtype IpRouteLimitExceededException = IpRouteLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeIpRouteLimitExceededException :: Newtype IpRouteLimitExceededException _
derive instance repGenericIpRouteLimitExceededException :: Generic IpRouteLimitExceededException _
instance showIpRouteLimitExceededException :: Show IpRouteLimitExceededException where
  show = genericShow
instance decodeIpRouteLimitExceededException :: Decode IpRouteLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpRouteLimitExceededException :: Encode IpRouteLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpRouteStatusMsg = IpRouteStatusMsg String
derive instance newtypeIpRouteStatusMsg :: Newtype IpRouteStatusMsg _
derive instance repGenericIpRouteStatusMsg :: Generic IpRouteStatusMsg _
instance showIpRouteStatusMsg :: Show IpRouteStatusMsg where
  show = genericShow
instance decodeIpRouteStatusMsg :: Decode IpRouteStatusMsg where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpRouteStatusMsg :: Encode IpRouteStatusMsg where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpRouteStatusReason = IpRouteStatusReason String
derive instance newtypeIpRouteStatusReason :: Newtype IpRouteStatusReason _
derive instance repGenericIpRouteStatusReason :: Generic IpRouteStatusReason _
instance showIpRouteStatusReason :: Show IpRouteStatusReason where
  show = genericShow
instance decodeIpRouteStatusReason :: Decode IpRouteStatusReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpRouteStatusReason :: Encode IpRouteStatusReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpRoutes = IpRoutes (Array IpRoute)
derive instance newtypeIpRoutes :: Newtype IpRoutes _
derive instance repGenericIpRoutes :: Generic IpRoutes _
instance showIpRoutes :: Show IpRoutes where
  show = genericShow
instance decodeIpRoutes :: Decode IpRoutes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpRoutes :: Encode IpRoutes where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpRoutesInfo = IpRoutesInfo (Array IpRouteInfo)
derive instance newtypeIpRoutesInfo :: Newtype IpRoutesInfo _
derive instance repGenericIpRoutesInfo :: Generic IpRoutesInfo _
instance showIpRoutesInfo :: Show IpRoutesInfo where
  show = genericShow
instance decodeIpRoutesInfo :: Decode IpRoutesInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpRoutesInfo :: Encode IpRoutesInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LastUpdatedDateTime = LastUpdatedDateTime Number
derive instance newtypeLastUpdatedDateTime :: Newtype LastUpdatedDateTime _
derive instance repGenericLastUpdatedDateTime :: Generic LastUpdatedDateTime _
instance showLastUpdatedDateTime :: Show LastUpdatedDateTime where
  show = genericShow
instance decodeLastUpdatedDateTime :: Decode LastUpdatedDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLastUpdatedDateTime :: Encode LastUpdatedDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LaunchTime = LaunchTime Number
derive instance newtypeLaunchTime :: Newtype LaunchTime _
derive instance repGenericLaunchTime :: Generic LaunchTime _
instance showLaunchTime :: Show LaunchTime where
  show = genericShow
instance decodeLaunchTime :: Decode LaunchTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLaunchTime :: Encode LaunchTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LdifContent = LdifContent String
derive instance newtypeLdifContent :: Newtype LdifContent _
derive instance repGenericLdifContent :: Generic LdifContent _
instance showLdifContent :: Show LdifContent where
  show = genericShow
instance decodeLdifContent :: Decode LdifContent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLdifContent :: Encode LdifContent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Limit = Limit Int
derive instance newtypeLimit :: Newtype Limit _
derive instance repGenericLimit :: Generic Limit _
instance showLimit :: Show Limit where
  show = genericShow
instance decodeLimit :: Decode Limit where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimit :: Encode Limit where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIpRoutesRequest = ListIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  }
derive instance newtypeListIpRoutesRequest :: Newtype ListIpRoutesRequest _
derive instance repGenericListIpRoutesRequest :: Generic ListIpRoutesRequest _
instance showListIpRoutesRequest :: Show ListIpRoutesRequest where
  show = genericShow
instance decodeListIpRoutesRequest :: Decode ListIpRoutesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIpRoutesRequest :: Encode ListIpRoutesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListIpRoutesResult = ListIpRoutesResult 
  { "IpRoutesInfo" :: NullOrUndefined.NullOrUndefined (IpRoutesInfo)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListIpRoutesResult :: Newtype ListIpRoutesResult _
derive instance repGenericListIpRoutesResult :: Generic ListIpRoutesResult _
instance showListIpRoutesResult :: Show ListIpRoutesResult where
  show = genericShow
instance decodeListIpRoutesResult :: Decode ListIpRoutesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListIpRoutesResult :: Encode ListIpRoutesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSchemaExtensionsRequest = ListSchemaExtensionsRequest 
  { "DirectoryId" :: (DirectoryId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  }
derive instance newtypeListSchemaExtensionsRequest :: Newtype ListSchemaExtensionsRequest _
derive instance repGenericListSchemaExtensionsRequest :: Generic ListSchemaExtensionsRequest _
instance showListSchemaExtensionsRequest :: Show ListSchemaExtensionsRequest where
  show = genericShow
instance decodeListSchemaExtensionsRequest :: Decode ListSchemaExtensionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSchemaExtensionsRequest :: Encode ListSchemaExtensionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListSchemaExtensionsResult = ListSchemaExtensionsResult 
  { "SchemaExtensionsInfo" :: NullOrUndefined.NullOrUndefined (SchemaExtensionsInfo)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListSchemaExtensionsResult :: Newtype ListSchemaExtensionsResult _
derive instance repGenericListSchemaExtensionsResult :: Generic ListSchemaExtensionsResult _
instance showListSchemaExtensionsResult :: Show ListSchemaExtensionsResult where
  show = genericShow
instance decodeListSchemaExtensionsResult :: Decode ListSchemaExtensionsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListSchemaExtensionsResult :: Encode ListSchemaExtensionsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _
derive instance repGenericListTagsForResourceRequest :: Generic ListTagsForResourceRequest _
instance showListTagsForResourceRequest :: Show ListTagsForResourceRequest where
  show = genericShow
instance decodeListTagsForResourceRequest :: Decode ListTagsForResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceRequest :: Encode ListTagsForResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ListTagsForResourceResult = ListTagsForResourceResult 
  { "Tags" :: NullOrUndefined.NullOrUndefined (Tags)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsForResourceResult :: Newtype ListTagsForResourceResult _
derive instance repGenericListTagsForResourceResult :: Generic ListTagsForResourceResult _
instance showListTagsForResourceResult :: Show ListTagsForResourceResult where
  show = genericShow
instance decodeListTagsForResourceResult :: Decode ListTagsForResourceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeListTagsForResourceResult :: Encode ListTagsForResourceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ManualSnapshotsLimitReached = ManualSnapshotsLimitReached Boolean
derive instance newtypeManualSnapshotsLimitReached :: Newtype ManualSnapshotsLimitReached _
derive instance repGenericManualSnapshotsLimitReached :: Generic ManualSnapshotsLimitReached _
instance showManualSnapshotsLimitReached :: Show ManualSnapshotsLimitReached where
  show = genericShow
instance decodeManualSnapshotsLimitReached :: Decode ManualSnapshotsLimitReached where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeManualSnapshotsLimitReached :: Encode ManualSnapshotsLimitReached where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _
derive instance repGenericNextToken :: Generic NextToken _
instance showNextToken :: Show NextToken where
  show = genericShow
instance decodeNextToken :: Decode NextToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNextToken :: Encode NextToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrganizationalUnitDN = OrganizationalUnitDN String
derive instance newtypeOrganizationalUnitDN :: Newtype OrganizationalUnitDN _
derive instance repGenericOrganizationalUnitDN :: Generic OrganizationalUnitDN _
instance showOrganizationalUnitDN :: Show OrganizationalUnitDN where
  show = genericShow
instance decodeOrganizationalUnitDN :: Decode OrganizationalUnitDN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganizationalUnitDN :: Encode OrganizationalUnitDN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Password = Password String
derive instance newtypePassword :: Newtype Password _
derive instance repGenericPassword :: Generic Password _
instance showPassword :: Show Password where
  show = genericShow
instance decodePassword :: Decode Password where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePassword :: Encode Password where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PortNumber = PortNumber Int
derive instance newtypePortNumber :: Newtype PortNumber _
derive instance repGenericPortNumber :: Generic PortNumber _
instance showPortNumber :: Show PortNumber where
  show = genericShow
instance decodePortNumber :: Decode PortNumber where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePortNumber :: Encode PortNumber where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RadiusAuthenticationProtocol = RadiusAuthenticationProtocol String
derive instance newtypeRadiusAuthenticationProtocol :: Newtype RadiusAuthenticationProtocol _
derive instance repGenericRadiusAuthenticationProtocol :: Generic RadiusAuthenticationProtocol _
instance showRadiusAuthenticationProtocol :: Show RadiusAuthenticationProtocol where
  show = genericShow
instance decodeRadiusAuthenticationProtocol :: Decode RadiusAuthenticationProtocol where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRadiusAuthenticationProtocol :: Encode RadiusAuthenticationProtocol where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RadiusDisplayLabel = RadiusDisplayLabel String
derive instance newtypeRadiusDisplayLabel :: Newtype RadiusDisplayLabel _
derive instance repGenericRadiusDisplayLabel :: Generic RadiusDisplayLabel _
instance showRadiusDisplayLabel :: Show RadiusDisplayLabel where
  show = genericShow
instance decodeRadiusDisplayLabel :: Decode RadiusDisplayLabel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRadiusDisplayLabel :: Encode RadiusDisplayLabel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RadiusRetries = RadiusRetries Int
derive instance newtypeRadiusRetries :: Newtype RadiusRetries _
derive instance repGenericRadiusRetries :: Generic RadiusRetries _
instance showRadiusRetries :: Show RadiusRetries where
  show = genericShow
instance decodeRadiusRetries :: Decode RadiusRetries where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRadiusRetries :: Encode RadiusRetries where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about a Remote Authentication Dial In User Service (RADIUS) server.</p>
newtype RadiusSettings = RadiusSettings 
  { "RadiusServers" :: NullOrUndefined.NullOrUndefined (Servers)
  , "RadiusPort" :: NullOrUndefined.NullOrUndefined (PortNumber)
  , "RadiusTimeout" :: NullOrUndefined.NullOrUndefined (RadiusTimeout)
  , "RadiusRetries" :: NullOrUndefined.NullOrUndefined (RadiusRetries)
  , "SharedSecret" :: NullOrUndefined.NullOrUndefined (RadiusSharedSecret)
  , "AuthenticationProtocol" :: NullOrUndefined.NullOrUndefined (RadiusAuthenticationProtocol)
  , "DisplayLabel" :: NullOrUndefined.NullOrUndefined (RadiusDisplayLabel)
  , "UseSameUsername" :: NullOrUndefined.NullOrUndefined (UseSameUsername)
  }
derive instance newtypeRadiusSettings :: Newtype RadiusSettings _
derive instance repGenericRadiusSettings :: Generic RadiusSettings _
instance showRadiusSettings :: Show RadiusSettings where
  show = genericShow
instance decodeRadiusSettings :: Decode RadiusSettings where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRadiusSettings :: Encode RadiusSettings where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RadiusSharedSecret = RadiusSharedSecret String
derive instance newtypeRadiusSharedSecret :: Newtype RadiusSharedSecret _
derive instance repGenericRadiusSharedSecret :: Generic RadiusSharedSecret _
instance showRadiusSharedSecret :: Show RadiusSharedSecret where
  show = genericShow
instance decodeRadiusSharedSecret :: Decode RadiusSharedSecret where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRadiusSharedSecret :: Encode RadiusSharedSecret where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RadiusStatus = RadiusStatus String
derive instance newtypeRadiusStatus :: Newtype RadiusStatus _
derive instance repGenericRadiusStatus :: Generic RadiusStatus _
instance showRadiusStatus :: Show RadiusStatus where
  show = genericShow
instance decodeRadiusStatus :: Decode RadiusStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRadiusStatus :: Encode RadiusStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RadiusTimeout = RadiusTimeout Int
derive instance newtypeRadiusTimeout :: Newtype RadiusTimeout _
derive instance repGenericRadiusTimeout :: Generic RadiusTimeout _
instance showRadiusTimeout :: Show RadiusTimeout where
  show = genericShow
instance decodeRadiusTimeout :: Decode RadiusTimeout where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRadiusTimeout :: Encode RadiusTimeout where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Registers a new event topic.</p>
newtype RegisterEventTopicRequest = RegisterEventTopicRequest 
  { "DirectoryId" :: (DirectoryId)
  , "TopicName" :: (TopicName)
  }
derive instance newtypeRegisterEventTopicRequest :: Newtype RegisterEventTopicRequest _
derive instance repGenericRegisterEventTopicRequest :: Generic RegisterEventTopicRequest _
instance showRegisterEventTopicRequest :: Show RegisterEventTopicRequest where
  show = genericShow
instance decodeRegisterEventTopicRequest :: Decode RegisterEventTopicRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterEventTopicRequest :: Encode RegisterEventTopicRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of a RegisterEventTopic request.</p>
newtype RegisterEventTopicResult = RegisterEventTopicResult Types.NoArguments
derive instance newtypeRegisterEventTopicResult :: Newtype RegisterEventTopicResult _
derive instance repGenericRegisterEventTopicResult :: Generic RegisterEventTopicResult _
instance showRegisterEventTopicResult :: Show RegisterEventTopicResult where
  show = genericShow
instance decodeRegisterEventTopicResult :: Decode RegisterEventTopicResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegisterEventTopicResult :: Encode RegisterEventTopicResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoteDomainName = RemoteDomainName String
derive instance newtypeRemoteDomainName :: Newtype RemoteDomainName _
derive instance repGenericRemoteDomainName :: Generic RemoteDomainName _
instance showRemoteDomainName :: Show RemoteDomainName where
  show = genericShow
instance decodeRemoteDomainName :: Decode RemoteDomainName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoteDomainName :: Encode RemoteDomainName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoteDomainNames = RemoteDomainNames (Array RemoteDomainName)
derive instance newtypeRemoteDomainNames :: Newtype RemoteDomainNames _
derive instance repGenericRemoteDomainNames :: Generic RemoteDomainNames _
instance showRemoteDomainNames :: Show RemoteDomainNames where
  show = genericShow
instance decodeRemoteDomainNames :: Decode RemoteDomainNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoteDomainNames :: Encode RemoteDomainNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveIpRoutesRequest = RemoveIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "CidrIps" :: (CidrIps)
  }
derive instance newtypeRemoveIpRoutesRequest :: Newtype RemoveIpRoutesRequest _
derive instance repGenericRemoveIpRoutesRequest :: Generic RemoveIpRoutesRequest _
instance showRemoveIpRoutesRequest :: Show RemoveIpRoutesRequest where
  show = genericShow
instance decodeRemoveIpRoutesRequest :: Decode RemoveIpRoutesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveIpRoutesRequest :: Encode RemoveIpRoutesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveIpRoutesResult = RemoveIpRoutesResult Types.NoArguments
derive instance newtypeRemoveIpRoutesResult :: Newtype RemoveIpRoutesResult _
derive instance repGenericRemoveIpRoutesResult :: Generic RemoveIpRoutesResult _
instance showRemoveIpRoutesResult :: Show RemoveIpRoutesResult where
  show = genericShow
instance decodeRemoveIpRoutesResult :: Decode RemoveIpRoutesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveIpRoutesResult :: Encode RemoveIpRoutesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeRemoveTagsFromResourceRequest :: Newtype RemoveTagsFromResourceRequest _
derive instance repGenericRemoveTagsFromResourceRequest :: Generic RemoveTagsFromResourceRequest _
instance showRemoveTagsFromResourceRequest :: Show RemoveTagsFromResourceRequest where
  show = genericShow
instance decodeRemoveTagsFromResourceRequest :: Decode RemoveTagsFromResourceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceRequest :: Encode RemoveTagsFromResourceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveTagsFromResourceResult = RemoveTagsFromResourceResult Types.NoArguments
derive instance newtypeRemoveTagsFromResourceResult :: Newtype RemoveTagsFromResourceResult _
derive instance repGenericRemoveTagsFromResourceResult :: Generic RemoveTagsFromResourceResult _
instance showRemoveTagsFromResourceResult :: Show RemoveTagsFromResourceResult where
  show = genericShow
instance decodeRemoveTagsFromResourceResult :: Decode RemoveTagsFromResourceResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveTagsFromResourceResult :: Encode RemoveTagsFromResourceResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ReplicationScope = ReplicationScope String
derive instance newtypeReplicationScope :: Newtype ReplicationScope _
derive instance repGenericReplicationScope :: Generic ReplicationScope _
instance showReplicationScope :: Show ReplicationScope where
  show = genericShow
instance decodeReplicationScope :: Decode ReplicationScope where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeReplicationScope :: Encode ReplicationScope where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The AWS request identifier.</p>
newtype RequestId = RequestId String
derive instance newtypeRequestId :: Newtype RequestId _
derive instance repGenericRequestId :: Generic RequestId _
instance showRequestId :: Show RequestId where
  show = genericShow
instance decodeRequestId :: Decode RequestId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRequestId :: Encode RequestId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _
derive instance repGenericResourceId :: Generic ResourceId _
instance showResourceId :: Show ResourceId where
  show = genericShow
instance decodeResourceId :: Decode ResourceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceId :: Encode ResourceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An object representing the inputs for the <a>RestoreFromSnapshot</a> operation.</p>
newtype RestoreFromSnapshotRequest = RestoreFromSnapshotRequest 
  { "SnapshotId" :: (SnapshotId)
  }
derive instance newtypeRestoreFromSnapshotRequest :: Newtype RestoreFromSnapshotRequest _
derive instance repGenericRestoreFromSnapshotRequest :: Generic RestoreFromSnapshotRequest _
instance showRestoreFromSnapshotRequest :: Show RestoreFromSnapshotRequest where
  show = genericShow
instance decodeRestoreFromSnapshotRequest :: Decode RestoreFromSnapshotRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreFromSnapshotRequest :: Encode RestoreFromSnapshotRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>RestoreFromSnapshot</a> operation.</p>
newtype RestoreFromSnapshotResult = RestoreFromSnapshotResult Types.NoArguments
derive instance newtypeRestoreFromSnapshotResult :: Newtype RestoreFromSnapshotResult _
derive instance repGenericRestoreFromSnapshotResult :: Generic RestoreFromSnapshotResult _
instance showRestoreFromSnapshotResult :: Show RestoreFromSnapshotResult where
  show = genericShow
instance decodeRestoreFromSnapshotResult :: Decode RestoreFromSnapshotResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRestoreFromSnapshotResult :: Encode RestoreFromSnapshotResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SID = SID String
derive instance newtypeSID :: Newtype SID _
derive instance repGenericSID :: Generic SID _
instance showSID :: Show SID where
  show = genericShow
instance decodeSID :: Decode SID where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSID :: Encode SID where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaExtensionId = SchemaExtensionId String
derive instance newtypeSchemaExtensionId :: Newtype SchemaExtensionId _
derive instance repGenericSchemaExtensionId :: Generic SchemaExtensionId _
instance showSchemaExtensionId :: Show SchemaExtensionId where
  show = genericShow
instance decodeSchemaExtensionId :: Decode SchemaExtensionId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaExtensionId :: Encode SchemaExtensionId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a schema extension.</p>
newtype SchemaExtensionInfo = SchemaExtensionInfo 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "SchemaExtensionId" :: NullOrUndefined.NullOrUndefined (SchemaExtensionId)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "SchemaExtensionStatus" :: NullOrUndefined.NullOrUndefined (SchemaExtensionStatus)
  , "SchemaExtensionStatusReason" :: NullOrUndefined.NullOrUndefined (SchemaExtensionStatusReason)
  , "StartDateTime" :: NullOrUndefined.NullOrUndefined (StartDateTime)
  , "EndDateTime" :: NullOrUndefined.NullOrUndefined (EndDateTime)
  }
derive instance newtypeSchemaExtensionInfo :: Newtype SchemaExtensionInfo _
derive instance repGenericSchemaExtensionInfo :: Generic SchemaExtensionInfo _
instance showSchemaExtensionInfo :: Show SchemaExtensionInfo where
  show = genericShow
instance decodeSchemaExtensionInfo :: Decode SchemaExtensionInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaExtensionInfo :: Encode SchemaExtensionInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaExtensionStatus = SchemaExtensionStatus String
derive instance newtypeSchemaExtensionStatus :: Newtype SchemaExtensionStatus _
derive instance repGenericSchemaExtensionStatus :: Generic SchemaExtensionStatus _
instance showSchemaExtensionStatus :: Show SchemaExtensionStatus where
  show = genericShow
instance decodeSchemaExtensionStatus :: Decode SchemaExtensionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaExtensionStatus :: Encode SchemaExtensionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaExtensionStatusReason = SchemaExtensionStatusReason String
derive instance newtypeSchemaExtensionStatusReason :: Newtype SchemaExtensionStatusReason _
derive instance repGenericSchemaExtensionStatusReason :: Generic SchemaExtensionStatusReason _
instance showSchemaExtensionStatusReason :: Show SchemaExtensionStatusReason where
  show = genericShow
instance decodeSchemaExtensionStatusReason :: Decode SchemaExtensionStatusReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaExtensionStatusReason :: Encode SchemaExtensionStatusReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SchemaExtensionsInfo = SchemaExtensionsInfo (Array SchemaExtensionInfo)
derive instance newtypeSchemaExtensionsInfo :: Newtype SchemaExtensionsInfo _
derive instance repGenericSchemaExtensionsInfo :: Generic SchemaExtensionsInfo _
instance showSchemaExtensionsInfo :: Show SchemaExtensionsInfo where
  show = genericShow
instance decodeSchemaExtensionsInfo :: Decode SchemaExtensionsInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSchemaExtensionsInfo :: Encode SchemaExtensionsInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SecurityGroupId = SecurityGroupId String
derive instance newtypeSecurityGroupId :: Newtype SecurityGroupId _
derive instance repGenericSecurityGroupId :: Generic SecurityGroupId _
instance showSecurityGroupId :: Show SecurityGroupId where
  show = genericShow
instance decodeSecurityGroupId :: Decode SecurityGroupId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSecurityGroupId :: Encode SecurityGroupId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Server = Server String
derive instance newtypeServer :: Newtype Server _
derive instance repGenericServer :: Generic Server _
instance showServer :: Show Server where
  show = genericShow
instance decodeServer :: Decode Server where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServer :: Encode Server where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Servers = Servers (Array Server)
derive instance newtypeServers :: Newtype Servers _
derive instance repGenericServers :: Generic Servers _
instance showServers :: Show Servers where
  show = genericShow
instance decodeServers :: Decode Servers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServers :: Encode Servers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>An exception has occurred in AWS Directory Service.</p>
newtype ServiceException = ServiceException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeServiceException :: Newtype ServiceException _
derive instance repGenericServiceException :: Generic ServiceException _
instance showServiceException :: Show ServiceException where
  show = genericShow
instance decodeServiceException :: Decode ServiceException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceException :: Encode ServiceException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a directory snapshot.</p>
newtype Snapshot = Snapshot 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "SnapshotId" :: NullOrUndefined.NullOrUndefined (SnapshotId)
  , "Type" :: NullOrUndefined.NullOrUndefined (SnapshotType)
  , "Name" :: NullOrUndefined.NullOrUndefined (SnapshotName)
  , "Status" :: NullOrUndefined.NullOrUndefined (SnapshotStatus)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (StartTime)
  }
derive instance newtypeSnapshot :: Newtype Snapshot _
derive instance repGenericSnapshot :: Generic Snapshot _
instance showSnapshot :: Show Snapshot where
  show = genericShow
instance decodeSnapshot :: Decode Snapshot where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshot :: Encode Snapshot where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotId = SnapshotId String
derive instance newtypeSnapshotId :: Newtype SnapshotId _
derive instance repGenericSnapshotId :: Generic SnapshotId _
instance showSnapshotId :: Show SnapshotId where
  show = genericShow
instance decodeSnapshotId :: Decode SnapshotId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotId :: Encode SnapshotId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of directory snapshot identifiers.</p>
newtype SnapshotIds = SnapshotIds (Array SnapshotId)
derive instance newtypeSnapshotIds :: Newtype SnapshotIds _
derive instance repGenericSnapshotIds :: Generic SnapshotIds _
instance showSnapshotIds :: Show SnapshotIds where
  show = genericShow
instance decodeSnapshotIds :: Decode SnapshotIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotIds :: Encode SnapshotIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum number of manual snapshots for the directory has been reached. You can use the <a>GetSnapshotLimits</a> operation to determine the snapshot limits for a directory.</p>
newtype SnapshotLimitExceededException = SnapshotLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeSnapshotLimitExceededException :: Newtype SnapshotLimitExceededException _
derive instance repGenericSnapshotLimitExceededException :: Generic SnapshotLimitExceededException _
instance showSnapshotLimitExceededException :: Show SnapshotLimitExceededException where
  show = genericShow
instance decodeSnapshotLimitExceededException :: Decode SnapshotLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotLimitExceededException :: Encode SnapshotLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains manual snapshot limit information for a directory.</p>
newtype SnapshotLimits = SnapshotLimits 
  { "ManualSnapshotsLimit" :: NullOrUndefined.NullOrUndefined (Limit)
  , "ManualSnapshotsCurrentCount" :: NullOrUndefined.NullOrUndefined (Limit)
  , "ManualSnapshotsLimitReached" :: NullOrUndefined.NullOrUndefined (ManualSnapshotsLimitReached)
  }
derive instance newtypeSnapshotLimits :: Newtype SnapshotLimits _
derive instance repGenericSnapshotLimits :: Generic SnapshotLimits _
instance showSnapshotLimits :: Show SnapshotLimits where
  show = genericShow
instance decodeSnapshotLimits :: Decode SnapshotLimits where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotLimits :: Encode SnapshotLimits where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotName = SnapshotName String
derive instance newtypeSnapshotName :: Newtype SnapshotName _
derive instance repGenericSnapshotName :: Generic SnapshotName _
instance showSnapshotName :: Show SnapshotName where
  show = genericShow
instance decodeSnapshotName :: Decode SnapshotName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotName :: Encode SnapshotName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotStatus = SnapshotStatus String
derive instance newtypeSnapshotStatus :: Newtype SnapshotStatus _
derive instance repGenericSnapshotStatus :: Generic SnapshotStatus _
instance showSnapshotStatus :: Show SnapshotStatus where
  show = genericShow
instance decodeSnapshotStatus :: Decode SnapshotStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotStatus :: Encode SnapshotStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SnapshotType = SnapshotType String
derive instance newtypeSnapshotType :: Newtype SnapshotType _
derive instance repGenericSnapshotType :: Generic SnapshotType _
instance showSnapshotType :: Show SnapshotType where
  show = genericShow
instance decodeSnapshotType :: Decode SnapshotType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshotType :: Encode SnapshotType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>A list of descriptions of directory snapshots.</p>
newtype Snapshots = Snapshots (Array Snapshot)
derive instance newtypeSnapshots :: Newtype Snapshots _
derive instance repGenericSnapshots :: Generic Snapshots _
instance showSnapshots :: Show Snapshots where
  show = genericShow
instance decodeSnapshots :: Decode Snapshots where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSnapshots :: Encode Snapshots where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SsoEnabled = SsoEnabled Boolean
derive instance newtypeSsoEnabled :: Newtype SsoEnabled _
derive instance repGenericSsoEnabled :: Generic SsoEnabled _
instance showSsoEnabled :: Show SsoEnabled where
  show = genericShow
instance decodeSsoEnabled :: Decode SsoEnabled where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSsoEnabled :: Encode SsoEnabled where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StageReason = StageReason String
derive instance newtypeStageReason :: Newtype StageReason _
derive instance repGenericStageReason :: Generic StageReason _
instance showStageReason :: Show StageReason where
  show = genericShow
instance decodeStageReason :: Decode StageReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStageReason :: Encode StageReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartDateTime = StartDateTime Number
derive instance newtypeStartDateTime :: Newtype StartDateTime _
derive instance repGenericStartDateTime :: Generic StartDateTime _
instance showStartDateTime :: Show StartDateTime where
  show = genericShow
instance decodeStartDateTime :: Decode StartDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartDateTime :: Encode StartDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartSchemaExtensionRequest = StartSchemaExtensionRequest 
  { "DirectoryId" :: (DirectoryId)
  , "CreateSnapshotBeforeSchemaExtension" :: (CreateSnapshotBeforeSchemaExtension)
  , "LdifContent" :: (LdifContent)
  , "Description" :: (Description)
  }
derive instance newtypeStartSchemaExtensionRequest :: Newtype StartSchemaExtensionRequest _
derive instance repGenericStartSchemaExtensionRequest :: Generic StartSchemaExtensionRequest _
instance showStartSchemaExtensionRequest :: Show StartSchemaExtensionRequest where
  show = genericShow
instance decodeStartSchemaExtensionRequest :: Decode StartSchemaExtensionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartSchemaExtensionRequest :: Encode StartSchemaExtensionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartSchemaExtensionResult = StartSchemaExtensionResult 
  { "SchemaExtensionId" :: NullOrUndefined.NullOrUndefined (SchemaExtensionId)
  }
derive instance newtypeStartSchemaExtensionResult :: Newtype StartSchemaExtensionResult _
derive instance repGenericStartSchemaExtensionResult :: Generic StartSchemaExtensionResult _
instance showStartSchemaExtensionResult :: Show StartSchemaExtensionResult where
  show = genericShow
instance decodeStartSchemaExtensionResult :: Decode StartSchemaExtensionResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartSchemaExtensionResult :: Encode StartSchemaExtensionResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartTime = StartTime Number
derive instance newtypeStartTime :: Newtype StartTime _
derive instance repGenericStartTime :: Generic StartTime _
instance showStartTime :: Show StartTime where
  show = genericShow
instance decodeStartTime :: Decode StartTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartTime :: Encode StartTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StateLastUpdatedDateTime = StateLastUpdatedDateTime Number
derive instance newtypeStateLastUpdatedDateTime :: Newtype StateLastUpdatedDateTime _
derive instance repGenericStateLastUpdatedDateTime :: Generic StateLastUpdatedDateTime _
instance showStateLastUpdatedDateTime :: Show StateLastUpdatedDateTime where
  show = genericShow
instance decodeStateLastUpdatedDateTime :: Decode StateLastUpdatedDateTime where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStateLastUpdatedDateTime :: Encode StateLastUpdatedDateTime where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _
derive instance repGenericSubnetId :: Generic SubnetId _
instance showSubnetId :: Show SubnetId where
  show = genericShow
instance decodeSubnetId :: Decode SubnetId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetId :: Encode SubnetId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubnetIds = SubnetIds (Array SubnetId)
derive instance newtypeSubnetIds :: Newtype SubnetIds _
derive instance repGenericSubnetIds :: Generic SubnetIds _
instance showSubnetIds :: Show SubnetIds where
  show = genericShow
instance decodeSubnetIds :: Decode SubnetIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubnetIds :: Encode SubnetIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Metadata assigned to a directory consisting of a key-value pair.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _
derive instance repGenericTag :: Generic Tag _
instance showTag :: Show Tag where
  show = genericShow
instance decodeTag :: Decode Tag where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTag :: Encode Tag where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _
derive instance repGenericTagKey :: Generic TagKey _
instance showTagKey :: Show TagKey where
  show = genericShow
instance decodeTagKey :: Decode TagKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKey :: Encode TagKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _
derive instance repGenericTagKeys :: Generic TagKeys _
instance showTagKeys :: Show TagKeys where
  show = genericShow
instance decodeTagKeys :: Decode TagKeys where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeys :: Encode TagKeys where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum allowed number of tags was exceeded.</p>
newtype TagLimitExceededException = TagLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeTagLimitExceededException :: Newtype TagLimitExceededException _
derive instance repGenericTagLimitExceededException :: Generic TagLimitExceededException _
instance showTagLimitExceededException :: Show TagLimitExceededException where
  show = genericShow
instance decodeTagLimitExceededException :: Decode TagLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagLimitExceededException :: Encode TagLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _
derive instance repGenericTagValue :: Generic TagValue _
instance showTagValue :: Show TagValue where
  show = genericShow
instance decodeTagValue :: Decode TagValue where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagValue :: Encode TagValue where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _
derive instance repGenericTags :: Generic Tags _
instance showTags :: Show Tags where
  show = genericShow
instance decodeTags :: Decode Tags where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTags :: Encode Tags where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicArn = TopicArn String
derive instance newtypeTopicArn :: Newtype TopicArn _
derive instance repGenericTopicArn :: Generic TopicArn _
instance showTopicArn :: Show TopicArn where
  show = genericShow
instance decodeTopicArn :: Decode TopicArn where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicArn :: Encode TopicArn where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicName = TopicName String
derive instance newtypeTopicName :: Newtype TopicName _
derive instance repGenericTopicName :: Generic TopicName _
instance showTopicName :: Show TopicName where
  show = genericShow
instance decodeTopicName :: Decode TopicName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicName :: Encode TopicName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicNames = TopicNames (Array TopicName)
derive instance newtypeTopicNames :: Newtype TopicNames _
derive instance repGenericTopicNames :: Generic TopicNames _
instance showTopicNames :: Show TopicNames where
  show = genericShow
instance decodeTopicNames :: Decode TopicNames where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicNames :: Encode TopicNames where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TopicStatus = TopicStatus String
derive instance newtypeTopicStatus :: Newtype TopicStatus _
derive instance repGenericTopicStatus :: Generic TopicStatus _
instance showTopicStatus :: Show TopicStatus where
  show = genericShow
instance decodeTopicStatus :: Decode TopicStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTopicStatus :: Encode TopicStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a trust relationship between an Microsoft AD in the AWS cloud and an external domain.</p>
newtype Trust = Trust 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "TrustId" :: NullOrUndefined.NullOrUndefined (TrustId)
  , "RemoteDomainName" :: NullOrUndefined.NullOrUndefined (RemoteDomainName)
  , "TrustType" :: NullOrUndefined.NullOrUndefined (TrustType)
  , "TrustDirection" :: NullOrUndefined.NullOrUndefined (TrustDirection)
  , "TrustState" :: NullOrUndefined.NullOrUndefined (TrustState)
  , "CreatedDateTime" :: NullOrUndefined.NullOrUndefined (CreatedDateTime)
  , "LastUpdatedDateTime" :: NullOrUndefined.NullOrUndefined (LastUpdatedDateTime)
  , "StateLastUpdatedDateTime" :: NullOrUndefined.NullOrUndefined (StateLastUpdatedDateTime)
  , "TrustStateReason" :: NullOrUndefined.NullOrUndefined (TrustStateReason)
  }
derive instance newtypeTrust :: Newtype Trust _
derive instance repGenericTrust :: Generic Trust _
instance showTrust :: Show Trust where
  show = genericShow
instance decodeTrust :: Decode Trust where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrust :: Encode Trust where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrustDirection = TrustDirection String
derive instance newtypeTrustDirection :: Newtype TrustDirection _
derive instance repGenericTrustDirection :: Generic TrustDirection _
instance showTrustDirection :: Show TrustDirection where
  show = genericShow
instance decodeTrustDirection :: Decode TrustDirection where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustDirection :: Encode TrustDirection where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrustId = TrustId String
derive instance newtypeTrustId :: Newtype TrustId _
derive instance repGenericTrustId :: Generic TrustId _
instance showTrustId :: Show TrustId where
  show = genericShow
instance decodeTrustId :: Decode TrustId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustId :: Encode TrustId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrustIds = TrustIds (Array TrustId)
derive instance newtypeTrustIds :: Newtype TrustIds _
derive instance repGenericTrustIds :: Generic TrustIds _
instance showTrustIds :: Show TrustIds where
  show = genericShow
instance decodeTrustIds :: Decode TrustIds where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustIds :: Encode TrustIds where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrustPassword = TrustPassword String
derive instance newtypeTrustPassword :: Newtype TrustPassword _
derive instance repGenericTrustPassword :: Generic TrustPassword _
instance showTrustPassword :: Show TrustPassword where
  show = genericShow
instance decodeTrustPassword :: Decode TrustPassword where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustPassword :: Encode TrustPassword where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrustState = TrustState String
derive instance newtypeTrustState :: Newtype TrustState _
derive instance repGenericTrustState :: Generic TrustState _
instance showTrustState :: Show TrustState where
  show = genericShow
instance decodeTrustState :: Decode TrustState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustState :: Encode TrustState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrustStateReason = TrustStateReason String
derive instance newtypeTrustStateReason :: Newtype TrustStateReason _
derive instance repGenericTrustStateReason :: Generic TrustStateReason _
instance showTrustStateReason :: Show TrustStateReason where
  show = genericShow
instance decodeTrustStateReason :: Decode TrustStateReason where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustStateReason :: Encode TrustStateReason where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TrustType = TrustType String
derive instance newtypeTrustType :: Newtype TrustType _
derive instance repGenericTrustType :: Generic TrustType _
instance showTrustType :: Show TrustType where
  show = genericShow
instance decodeTrustType :: Decode TrustType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrustType :: Encode TrustType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Trusts = Trusts (Array Trust)
derive instance newtypeTrusts :: Newtype Trusts _
derive instance repGenericTrusts :: Generic Trusts _
instance showTrusts :: Show Trusts where
  show = genericShow
instance decodeTrusts :: Decode Trusts where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTrusts :: Encode Trusts where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation is not supported.</p>
newtype UnsupportedOperationException = UnsupportedOperationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined.NullOrUndefined (RequestId)
  }
derive instance newtypeUnsupportedOperationException :: Newtype UnsupportedOperationException _
derive instance repGenericUnsupportedOperationException :: Generic UnsupportedOperationException _
instance showUnsupportedOperationException :: Show UnsupportedOperationException where
  show = genericShow
instance decodeUnsupportedOperationException :: Decode UnsupportedOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedOperationException :: Encode UnsupportedOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Updates a conditional forwarder.</p>
newtype UpdateConditionalForwarderRequest = UpdateConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "DnsIpAddrs" :: (DnsIpAddrs)
  }
derive instance newtypeUpdateConditionalForwarderRequest :: Newtype UpdateConditionalForwarderRequest _
derive instance repGenericUpdateConditionalForwarderRequest :: Generic UpdateConditionalForwarderRequest _
instance showUpdateConditionalForwarderRequest :: Show UpdateConditionalForwarderRequest where
  show = genericShow
instance decodeUpdateConditionalForwarderRequest :: Decode UpdateConditionalForwarderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConditionalForwarderRequest :: Encode UpdateConditionalForwarderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The result of an UpdateConditionalForwarder request.</p>
newtype UpdateConditionalForwarderResult = UpdateConditionalForwarderResult Types.NoArguments
derive instance newtypeUpdateConditionalForwarderResult :: Newtype UpdateConditionalForwarderResult _
derive instance repGenericUpdateConditionalForwarderResult :: Generic UpdateConditionalForwarderResult _
instance showUpdateConditionalForwarderResult :: Show UpdateConditionalForwarderResult where
  show = genericShow
instance decodeUpdateConditionalForwarderResult :: Decode UpdateConditionalForwarderResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateConditionalForwarderResult :: Encode UpdateConditionalForwarderResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateNumberOfDomainControllersRequest = UpdateNumberOfDomainControllersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "DesiredNumber" :: (DesiredNumberOfDomainControllers)
  }
derive instance newtypeUpdateNumberOfDomainControllersRequest :: Newtype UpdateNumberOfDomainControllersRequest _
derive instance repGenericUpdateNumberOfDomainControllersRequest :: Generic UpdateNumberOfDomainControllersRequest _
instance showUpdateNumberOfDomainControllersRequest :: Show UpdateNumberOfDomainControllersRequest where
  show = genericShow
instance decodeUpdateNumberOfDomainControllersRequest :: Decode UpdateNumberOfDomainControllersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateNumberOfDomainControllersRequest :: Encode UpdateNumberOfDomainControllersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateNumberOfDomainControllersResult = UpdateNumberOfDomainControllersResult Types.NoArguments
derive instance newtypeUpdateNumberOfDomainControllersResult :: Newtype UpdateNumberOfDomainControllersResult _
derive instance repGenericUpdateNumberOfDomainControllersResult :: Generic UpdateNumberOfDomainControllersResult _
instance showUpdateNumberOfDomainControllersResult :: Show UpdateNumberOfDomainControllersResult where
  show = genericShow
instance decodeUpdateNumberOfDomainControllersResult :: Decode UpdateNumberOfDomainControllersResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateNumberOfDomainControllersResult :: Encode UpdateNumberOfDomainControllersResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the inputs for the <a>UpdateRadius</a> operation.</p>
newtype UpdateRadiusRequest = UpdateRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RadiusSettings" :: (RadiusSettings)
  }
derive instance newtypeUpdateRadiusRequest :: Newtype UpdateRadiusRequest _
derive instance repGenericUpdateRadiusRequest :: Generic UpdateRadiusRequest _
instance showUpdateRadiusRequest :: Show UpdateRadiusRequest where
  show = genericShow
instance decodeUpdateRadiusRequest :: Decode UpdateRadiusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRadiusRequest :: Encode UpdateRadiusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains the results of the <a>UpdateRadius</a> operation.</p>
newtype UpdateRadiusResult = UpdateRadiusResult Types.NoArguments
derive instance newtypeUpdateRadiusResult :: Newtype UpdateRadiusResult _
derive instance repGenericUpdateRadiusResult :: Generic UpdateRadiusResult _
instance showUpdateRadiusResult :: Show UpdateRadiusResult where
  show = genericShow
instance decodeUpdateRadiusResult :: Decode UpdateRadiusResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateRadiusResult :: Encode UpdateRadiusResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateSecurityGroupForDirectoryControllers = UpdateSecurityGroupForDirectoryControllers Boolean
derive instance newtypeUpdateSecurityGroupForDirectoryControllers :: Newtype UpdateSecurityGroupForDirectoryControllers _
derive instance repGenericUpdateSecurityGroupForDirectoryControllers :: Generic UpdateSecurityGroupForDirectoryControllers _
instance showUpdateSecurityGroupForDirectoryControllers :: Show UpdateSecurityGroupForDirectoryControllers where
  show = genericShow
instance decodeUpdateSecurityGroupForDirectoryControllers :: Decode UpdateSecurityGroupForDirectoryControllers where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateSecurityGroupForDirectoryControllers :: Encode UpdateSecurityGroupForDirectoryControllers where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UseSameUsername = UseSameUsername Boolean
derive instance newtypeUseSameUsername :: Newtype UseSameUsername _
derive instance repGenericUseSameUsername :: Generic UseSameUsername _
instance showUseSameUsername :: Show UseSameUsername where
  show = genericShow
instance decodeUseSameUsername :: Decode UseSameUsername where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUseSameUsername :: Encode UseSameUsername where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserName = UserName String
derive instance newtypeUserName :: Newtype UserName _
derive instance repGenericUserName :: Generic UserName _
instance showUserName :: Show UserName where
  show = genericShow
instance decodeUserName :: Decode UserName where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserName :: Encode UserName where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Initiates the verification of an existing trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>
newtype VerifyTrustRequest = VerifyTrustRequest 
  { "TrustId" :: (TrustId)
  }
derive instance newtypeVerifyTrustRequest :: Newtype VerifyTrustRequest _
derive instance repGenericVerifyTrustRequest :: Generic VerifyTrustRequest _
instance showVerifyTrustRequest :: Show VerifyTrustRequest where
  show = genericShow
instance decodeVerifyTrustRequest :: Decode VerifyTrustRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyTrustRequest :: Encode VerifyTrustRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Result of a VerifyTrust request.</p>
newtype VerifyTrustResult = VerifyTrustResult 
  { "TrustId" :: NullOrUndefined.NullOrUndefined (TrustId)
  }
derive instance newtypeVerifyTrustResult :: Newtype VerifyTrustResult _
derive instance repGenericVerifyTrustResult :: Generic VerifyTrustResult _
instance showVerifyTrustResult :: Show VerifyTrustResult where
  show = genericShow
instance decodeVerifyTrustResult :: Decode VerifyTrustResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVerifyTrustResult :: Encode VerifyTrustResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VpcId = VpcId String
derive instance newtypeVpcId :: Newtype VpcId _
derive instance repGenericVpcId :: Generic VpcId _
instance showVpcId :: Show VpcId where
  show = genericShow
instance decodeVpcId :: Decode VpcId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVpcId :: Encode VpcId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
