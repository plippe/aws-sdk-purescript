

-- | <fullname>AWS Directory Service</fullname> <p>AWS Directory Service is a web service that makes it easy for you to setup and run directories in the AWS cloud, or connect your AWS resources with an existing on-premises Microsoft Active Directory. This guide provides detailed information about AWS Directory Service operations, data types, parameters, and errors. For information about AWS Directory Services features, see <a href="https://aws.amazon.com/directoryservice/">AWS Directory Service</a> and the <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html">AWS Directory Service Administration Guide</a>.</p> <note> <p>AWS provides SDKs that consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .Net, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to AWS Directory Service and other AWS services. For more information about the AWS SDKs, including how to download and install them, see <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> </note>
module AWS.DirectoryService where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "DirectoryService" :: String


-- | <p>If the DNS server for your on-premises domain uses a publicly addressable IP address, you must add a CIDR address block to correctly route traffic to and from your Microsoft AD on Amazon Web Services. <i>AddIpRoutes</i> adds this address block. You can also use <i>AddIpRoutes</i> to facilitate routing traffic that uses public IP ranges from your Microsoft AD on AWS to a peer VPC. </p> <p>Before you call <i>AddIpRoutes</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>AddIpRoutes</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
addIpRoutes :: forall eff. AddIpRoutesRequest -> Aff (err :: AWS.RequestError | eff) AddIpRoutesResult
addIpRoutes = AWS.request serviceName "AddIpRoutes" 


-- | <p>Adds or overwrites one or more tags for the specified directory. Each directory can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique to each resource.</p>
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (err :: AWS.RequestError | eff) AddTagsToResourceResult
addTagsToResource = AWS.request serviceName "AddTagsToResource" 


-- | <p>Cancels an in-progress schema extension to a Microsoft AD directory. Once a schema extension has started replicating to all domain controllers, the task can no longer be canceled. A schema extension can be canceled during any of the following states; <code>Initializing</code>, <code>CreatingSnapshot</code>, and <code>UpdatingSchema</code>.</p>
cancelSchemaExtension :: forall eff. CancelSchemaExtensionRequest -> Aff (err :: AWS.RequestError | eff) CancelSchemaExtensionResult
cancelSchemaExtension = AWS.request serviceName "CancelSchemaExtension" 


-- | <p>Creates an AD Connector to connect to an on-premises directory.</p> <p>Before you call <i>ConnectDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>ConnectDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
connectDirectory :: forall eff. ConnectDirectoryRequest -> Aff (err :: AWS.RequestError | eff) ConnectDirectoryResult
connectDirectory = AWS.request serviceName "ConnectDirectory" 


-- | <p>Creates an alias for a directory and assigns the alias to the directory. The alias is used to construct the access URL for the directory, such as <code>http://&lt;alias&gt;.awsapps.com</code>.</p> <important> <p>After an alias has been created, it cannot be deleted or reused, so this operation should only be used when absolutely necessary.</p> </important>
createAlias :: forall eff. CreateAliasRequest -> Aff (err :: AWS.RequestError | eff) CreateAliasResult
createAlias = AWS.request serviceName "CreateAlias" 


-- | <p>Creates a computer account in the specified directory, and joins the computer to the directory.</p>
createComputer :: forall eff. CreateComputerRequest -> Aff (err :: AWS.RequestError | eff) CreateComputerResult
createComputer = AWS.request serviceName "CreateComputer" 


-- | <p>Creates a conditional forwarder associated with your AWS directory. Conditional forwarders are required in order to set up a trust relationship with another domain. The conditional forwarder points to the trusted domain.</p>
createConditionalForwarder :: forall eff. CreateConditionalForwarderRequest -> Aff (err :: AWS.RequestError | eff) CreateConditionalForwarderResult
createConditionalForwarder = AWS.request serviceName "CreateConditionalForwarder" 


-- | <p>Creates a Simple AD directory.</p> <p>Before you call <i>CreateDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>CreateDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
createDirectory :: forall eff. CreateDirectoryRequest -> Aff (err :: AWS.RequestError | eff) CreateDirectoryResult
createDirectory = AWS.request serviceName "CreateDirectory" 


-- | <p>Creates a Microsoft AD in the AWS cloud.</p> <p>Before you call <i>CreateMicrosoftAD</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>CreateMicrosoftAD</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
createMicrosoftAD :: forall eff. CreateMicrosoftADRequest -> Aff (err :: AWS.RequestError | eff) CreateMicrosoftADResult
createMicrosoftAD = AWS.request serviceName "CreateMicrosoftAD" 


-- | <p>Creates a snapshot of a Simple AD or Microsoft AD directory in the AWS cloud.</p> <note> <p>You cannot take snapshots of AD Connector directories.</p> </note>
createSnapshot :: forall eff. CreateSnapshotRequest -> Aff (err :: AWS.RequestError | eff) CreateSnapshotResult
createSnapshot = AWS.request serviceName "CreateSnapshot" 


-- | <p>AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your Microsoft AD in the AWS cloud, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.</p> <p>This action initiates the creation of the AWS side of a trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>
createTrust :: forall eff. CreateTrustRequest -> Aff (err :: AWS.RequestError | eff) CreateTrustResult
createTrust = AWS.request serviceName "CreateTrust" 


-- | <p>Deletes a conditional forwarder that has been set up for your AWS directory.</p>
deleteConditionalForwarder :: forall eff. DeleteConditionalForwarderRequest -> Aff (err :: AWS.RequestError | eff) DeleteConditionalForwarderResult
deleteConditionalForwarder = AWS.request serviceName "DeleteConditionalForwarder" 


-- | <p>Deletes an AWS Directory Service directory.</p> <p>Before you call <i>DeleteDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>DeleteDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>
deleteDirectory :: forall eff. DeleteDirectoryRequest -> Aff (err :: AWS.RequestError | eff) DeleteDirectoryResult
deleteDirectory = AWS.request serviceName "DeleteDirectory" 


-- | <p>Deletes a directory snapshot.</p>
deleteSnapshot :: forall eff. DeleteSnapshotRequest -> Aff (err :: AWS.RequestError | eff) DeleteSnapshotResult
deleteSnapshot = AWS.request serviceName "DeleteSnapshot" 


-- | <p>Deletes an existing trust relationship between your Microsoft AD in the AWS cloud and an external domain.</p>
deleteTrust :: forall eff. DeleteTrustRequest -> Aff (err :: AWS.RequestError | eff) DeleteTrustResult
deleteTrust = AWS.request serviceName "DeleteTrust" 


-- | <p>Removes the specified directory as a publisher to the specified SNS topic.</p>
deregisterEventTopic :: forall eff. DeregisterEventTopicRequest -> Aff (err :: AWS.RequestError | eff) DeregisterEventTopicResult
deregisterEventTopic = AWS.request serviceName "DeregisterEventTopic" 


-- | <p>Obtains information about the conditional forwarders for this account.</p> <p>If no input parameters are provided for RemoteDomainNames, this request describes all conditional forwarders for the specified directory ID.</p>
describeConditionalForwarders :: forall eff. DescribeConditionalForwardersRequest -> Aff (err :: AWS.RequestError | eff) DescribeConditionalForwardersResult
describeConditionalForwarders = AWS.request serviceName "DescribeConditionalForwarders" 


-- | <p>Obtains information about the directories that belong to this account.</p> <p>You can retrieve information about specific directories by passing the directory identifiers in the <i>DirectoryIds</i> parameter. Otherwise, all directories that belong to the current account are returned.</p> <p>This operation supports pagination with the use of the <i>NextToken</i> request and response parameters. If more results are available, the <i>DescribeDirectoriesResult.NextToken</i> member contains a token that you pass in the next call to <a>DescribeDirectories</a> to retrieve the next set of items.</p> <p>You can also specify a maximum number of return results with the <i>Limit</i> parameter.</p>
describeDirectories :: forall eff. DescribeDirectoriesRequest -> Aff (err :: AWS.RequestError | eff) DescribeDirectoriesResult
describeDirectories = AWS.request serviceName "DescribeDirectories" 


-- | <p>Provides information about any domain controllers in your directory.</p>
describeDomainControllers :: forall eff. DescribeDomainControllersRequest -> Aff (err :: AWS.RequestError | eff) DescribeDomainControllersResult
describeDomainControllers = AWS.request serviceName "DescribeDomainControllers" 


-- | <p>Obtains information about which SNS topics receive status messages from the specified directory.</p> <p>If no input parameters are provided, such as DirectoryId or TopicName, this request describes all of the associations in the account.</p>
describeEventTopics :: forall eff. DescribeEventTopicsRequest -> Aff (err :: AWS.RequestError | eff) DescribeEventTopicsResult
describeEventTopics = AWS.request serviceName "DescribeEventTopics" 


-- | <p>Obtains information about the directory snapshots that belong to this account.</p> <p>This operation supports pagination with the use of the <i>NextToken</i> request and response parameters. If more results are available, the <i>DescribeSnapshots.NextToken</i> member contains a token that you pass in the next call to <a>DescribeSnapshots</a> to retrieve the next set of items.</p> <p>You can also specify a maximum number of return results with the <i>Limit</i> parameter.</p>
describeSnapshots :: forall eff. DescribeSnapshotsRequest -> Aff (err :: AWS.RequestError | eff) DescribeSnapshotsResult
describeSnapshots = AWS.request serviceName "DescribeSnapshots" 


-- | <p>Obtains information about the trust relationships for this account.</p> <p>If no input parameters are provided, such as DirectoryId or TrustIds, this request describes all the trust relationships belonging to the account.</p>
describeTrusts :: forall eff. DescribeTrustsRequest -> Aff (err :: AWS.RequestError | eff) DescribeTrustsResult
describeTrusts = AWS.request serviceName "DescribeTrusts" 


-- | <p>Disables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector directory.</p>
disableRadius :: forall eff. DisableRadiusRequest -> Aff (err :: AWS.RequestError | eff) DisableRadiusResult
disableRadius = AWS.request serviceName "DisableRadius" 


-- | <p>Disables single-sign on for a directory.</p>
disableSso :: forall eff. DisableSsoRequest -> Aff (err :: AWS.RequestError | eff) DisableSsoResult
disableSso = AWS.request serviceName "DisableSso" 


-- | <p>Enables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector directory.</p>
enableRadius :: forall eff. EnableRadiusRequest -> Aff (err :: AWS.RequestError | eff) EnableRadiusResult
enableRadius = AWS.request serviceName "EnableRadius" 


-- | <p>Enables single sign-on for a directory.</p>
enableSso :: forall eff. EnableSsoRequest -> Aff (err :: AWS.RequestError | eff) EnableSsoResult
enableSso = AWS.request serviceName "EnableSso" 


-- | <p>Obtains directory limit information for the current region.</p>
getDirectoryLimits :: forall eff. GetDirectoryLimitsRequest -> Aff (err :: AWS.RequestError | eff) GetDirectoryLimitsResult
getDirectoryLimits = AWS.request serviceName "GetDirectoryLimits" 


-- | <p>Obtains the manual snapshot limits for a directory.</p>
getSnapshotLimits :: forall eff. GetSnapshotLimitsRequest -> Aff (err :: AWS.RequestError | eff) GetSnapshotLimitsResult
getSnapshotLimits = AWS.request serviceName "GetSnapshotLimits" 


-- | <p>Lists the address blocks that you have added to a directory.</p>
listIpRoutes :: forall eff. ListIpRoutesRequest -> Aff (err :: AWS.RequestError | eff) ListIpRoutesResult
listIpRoutes = AWS.request serviceName "ListIpRoutes" 


-- | <p>Lists all schema extensions applied to a Microsoft AD Directory.</p>
listSchemaExtensions :: forall eff. ListSchemaExtensionsRequest -> Aff (err :: AWS.RequestError | eff) ListSchemaExtensionsResult
listSchemaExtensions = AWS.request serviceName "ListSchemaExtensions" 


-- | <p>Lists all tags on a directory.</p>
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: AWS.RequestError | eff) ListTagsForResourceResult
listTagsForResource = AWS.request serviceName "ListTagsForResource" 


-- | <p>Associates a directory with an SNS topic. This establishes the directory as a publisher to the specified SNS topic. You can then receive email or text (SMS) messages when the status of your directory changes. You get notified if your directory goes from an Active status to an Impaired or Inoperable status. You also receive a notification when the directory returns to an Active status.</p>
registerEventTopic :: forall eff. RegisterEventTopicRequest -> Aff (err :: AWS.RequestError | eff) RegisterEventTopicResult
registerEventTopic = AWS.request serviceName "RegisterEventTopic" 


-- | <p>Removes IP address blocks from a directory.</p>
removeIpRoutes :: forall eff. RemoveIpRoutesRequest -> Aff (err :: AWS.RequestError | eff) RemoveIpRoutesResult
removeIpRoutes = AWS.request serviceName "RemoveIpRoutes" 


-- | <p>Removes tags from a directory.</p>
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (err :: AWS.RequestError | eff) RemoveTagsFromResourceResult
removeTagsFromResource = AWS.request serviceName "RemoveTagsFromResource" 


-- | <p>Restores a directory using an existing directory snapshot.</p> <p>When you restore a directory from a snapshot, any changes made to the directory after the snapshot date are overwritten.</p> <p>This action returns as soon as the restore operation is initiated. You can monitor the progress of the restore operation by calling the <a>DescribeDirectories</a> operation with the directory identifier. When the <b>DirectoryDescription.Stage</b> value changes to <code>Active</code>, the restore operation is complete.</p>
restoreFromSnapshot :: forall eff. RestoreFromSnapshotRequest -> Aff (err :: AWS.RequestError | eff) RestoreFromSnapshotResult
restoreFromSnapshot = AWS.request serviceName "RestoreFromSnapshot" 


-- | <p>Applies a schema extension to a Microsoft AD directory.</p>
startSchemaExtension :: forall eff. StartSchemaExtensionRequest -> Aff (err :: AWS.RequestError | eff) StartSchemaExtensionResult
startSchemaExtension = AWS.request serviceName "StartSchemaExtension" 


-- | <p>Updates a conditional forwarder that has been set up for your AWS directory.</p>
updateConditionalForwarder :: forall eff. UpdateConditionalForwarderRequest -> Aff (err :: AWS.RequestError | eff) UpdateConditionalForwarderResult
updateConditionalForwarder = AWS.request serviceName "UpdateConditionalForwarder" 


-- | <p>Adds or removes domain controllers to or from the directory. Based on the difference between current value and new value (provided through this API call), domain controllers will be added or removed. It may take up to 45 minutes for any new domain controllers to become fully active once the requested number of domain controllers is updated. During this time, you cannot make another update request.</p>
updateNumberOfDomainControllers :: forall eff. UpdateNumberOfDomainControllersRequest -> Aff (err :: AWS.RequestError | eff) UpdateNumberOfDomainControllersResult
updateNumberOfDomainControllers = AWS.request serviceName "UpdateNumberOfDomainControllers" 


-- | <p>Updates the Remote Authentication Dial In User Service (RADIUS) server information for an AD Connector directory.</p>
updateRadius :: forall eff. UpdateRadiusRequest -> Aff (err :: AWS.RequestError | eff) UpdateRadiusResult
updateRadius = AWS.request serviceName "UpdateRadius" 


-- | <p>AWS Directory Service for Microsoft Active Directory allows you to configure and verify trust relationships.</p> <p>This action verifies a trust relationship between your Microsoft AD in the AWS cloud and an external domain.</p>
verifyTrust :: forall eff. VerifyTrustRequest -> Aff (err :: AWS.RequestError | eff) VerifyTrustResult
verifyTrust = AWS.request serviceName "VerifyTrust" 


newtype AccessUrl = AccessUrl String
derive instance newtypeAccessUrl :: Newtype AccessUrl _


newtype AddIpRoutesRequest = AddIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "IpRoutes" :: (IpRoutes)
  , "UpdateSecurityGroupForDirectoryControllers" :: NullOrUndefined (UpdateSecurityGroupForDirectoryControllers)
  }
derive instance newtypeAddIpRoutesRequest :: Newtype AddIpRoutesRequest _


newtype AddIpRoutesResult = AddIpRoutesResult 
  { 
  }
derive instance newtypeAddIpRoutesResult :: Newtype AddIpRoutesResult _


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "Tags" :: (Tags)
  }
derive instance newtypeAddTagsToResourceRequest :: Newtype AddTagsToResourceRequest _


newtype AddTagsToResourceResult = AddTagsToResourceResult 
  { 
  }
derive instance newtypeAddTagsToResourceResult :: Newtype AddTagsToResourceResult _


newtype AddedDateTime = AddedDateTime Number
derive instance newtypeAddedDateTime :: Newtype AddedDateTime _


newtype AliasName = AliasName String
derive instance newtypeAliasName :: Newtype AliasName _


-- | <p>Represents a named directory attribute.</p>
newtype Attribute = Attribute 
  { "Name" :: NullOrUndefined (AttributeName)
  , "Value" :: NullOrUndefined (AttributeValue)
  }
derive instance newtypeAttribute :: Newtype Attribute _


newtype AttributeName = AttributeName String
derive instance newtypeAttributeName :: Newtype AttributeName _


newtype AttributeValue = AttributeValue String
derive instance newtypeAttributeValue :: Newtype AttributeValue _


newtype Attributes = Attributes (Array Attribute)
derive instance newtypeAttributes :: Newtype Attributes _


-- | <p>An authentication error occurred.</p>
newtype AuthenticationFailedException = AuthenticationFailedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeAuthenticationFailedException :: Newtype AuthenticationFailedException _


newtype AvailabilityZone = AvailabilityZone String
derive instance newtypeAvailabilityZone :: Newtype AvailabilityZone _


newtype AvailabilityZones = AvailabilityZones (Array AvailabilityZone)
derive instance newtypeAvailabilityZones :: Newtype AvailabilityZones _


newtype CancelSchemaExtensionRequest = CancelSchemaExtensionRequest 
  { "DirectoryId" :: (DirectoryId)
  , "SchemaExtensionId" :: (SchemaExtensionId)
  }
derive instance newtypeCancelSchemaExtensionRequest :: Newtype CancelSchemaExtensionRequest _


newtype CancelSchemaExtensionResult = CancelSchemaExtensionResult 
  { 
  }
derive instance newtypeCancelSchemaExtensionResult :: Newtype CancelSchemaExtensionResult _


newtype CidrIp = CidrIp String
derive instance newtypeCidrIp :: Newtype CidrIp _


newtype CidrIps = CidrIps (Array CidrIp)
derive instance newtypeCidrIps :: Newtype CidrIps _


-- | <p>A client exception has occurred.</p>
newtype ClientException = ClientException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeClientException :: Newtype ClientException _


newtype CloudOnlyDirectoriesLimitReached = CloudOnlyDirectoriesLimitReached Boolean
derive instance newtypeCloudOnlyDirectoriesLimitReached :: Newtype CloudOnlyDirectoriesLimitReached _


-- | <p>Contains information about a computer account in a directory.</p>
newtype Computer = Computer 
  { "ComputerId" :: NullOrUndefined (SID)
  , "ComputerName" :: NullOrUndefined (ComputerName)
  , "ComputerAttributes" :: NullOrUndefined (Attributes)
  }
derive instance newtypeComputer :: Newtype Computer _


newtype ComputerName = ComputerName String
derive instance newtypeComputerName :: Newtype ComputerName _


newtype ComputerPassword = ComputerPassword String
derive instance newtypeComputerPassword :: Newtype ComputerPassword _


-- | <p>Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>
newtype ConditionalForwarder = ConditionalForwarder 
  { "RemoteDomainName" :: NullOrUndefined (RemoteDomainName)
  , "DnsIpAddrs" :: NullOrUndefined (DnsIpAddrs)
  , "ReplicationScope" :: NullOrUndefined (ReplicationScope)
  }
derive instance newtypeConditionalForwarder :: Newtype ConditionalForwarder _


newtype ConditionalForwarders = ConditionalForwarders (Array ConditionalForwarder)
derive instance newtypeConditionalForwarders :: Newtype ConditionalForwarders _


-- | <p>Contains the inputs for the <a>ConnectDirectory</a> operation.</p>
newtype ConnectDirectoryRequest = ConnectDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined (DirectoryShortName)
  , "Password" :: (ConnectPassword)
  , "Description" :: NullOrUndefined (Description)
  , "Size" :: (DirectorySize)
  , "ConnectSettings" :: (DirectoryConnectSettings)
  }
derive instance newtypeConnectDirectoryRequest :: Newtype ConnectDirectoryRequest _


-- | <p>Contains the results of the <a>ConnectDirectory</a> operation.</p>
newtype ConnectDirectoryResult = ConnectDirectoryResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  }
derive instance newtypeConnectDirectoryResult :: Newtype ConnectDirectoryResult _


newtype ConnectPassword = ConnectPassword String
derive instance newtypeConnectPassword :: Newtype ConnectPassword _


newtype ConnectedDirectoriesLimitReached = ConnectedDirectoriesLimitReached Boolean
derive instance newtypeConnectedDirectoriesLimitReached :: Newtype ConnectedDirectoriesLimitReached _


-- | <p>Contains the inputs for the <a>CreateAlias</a> operation.</p>
newtype CreateAliasRequest = CreateAliasRequest 
  { "DirectoryId" :: (DirectoryId)
  , "Alias" :: (AliasName)
  }
derive instance newtypeCreateAliasRequest :: Newtype CreateAliasRequest _


-- | <p>Contains the results of the <a>CreateAlias</a> operation.</p>
newtype CreateAliasResult = CreateAliasResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "Alias" :: NullOrUndefined (AliasName)
  }
derive instance newtypeCreateAliasResult :: Newtype CreateAliasResult _


-- | <p>Contains the inputs for the <a>CreateComputer</a> operation.</p>
newtype CreateComputerRequest = CreateComputerRequest 
  { "DirectoryId" :: (DirectoryId)
  , "ComputerName" :: (ComputerName)
  , "Password" :: (ComputerPassword)
  , "OrganizationalUnitDistinguishedName" :: NullOrUndefined (OrganizationalUnitDN)
  , "ComputerAttributes" :: NullOrUndefined (Attributes)
  }
derive instance newtypeCreateComputerRequest :: Newtype CreateComputerRequest _


-- | <p>Contains the results for the <a>CreateComputer</a> operation.</p>
newtype CreateComputerResult = CreateComputerResult 
  { "Computer" :: NullOrUndefined (Computer)
  }
derive instance newtypeCreateComputerResult :: Newtype CreateComputerResult _


-- | <p>Initiates the creation of a conditional forwarder for your AWS Directory Service for Microsoft Active Directory. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>
newtype CreateConditionalForwarderRequest = CreateConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "DnsIpAddrs" :: (DnsIpAddrs)
  }
derive instance newtypeCreateConditionalForwarderRequest :: Newtype CreateConditionalForwarderRequest _


-- | <p>The result of a CreateConditinalForwarder request.</p>
newtype CreateConditionalForwarderResult = CreateConditionalForwarderResult 
  { 
  }
derive instance newtypeCreateConditionalForwarderResult :: Newtype CreateConditionalForwarderResult _


-- | <p>Contains the inputs for the <a>CreateDirectory</a> operation. </p>
newtype CreateDirectoryRequest = CreateDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined (DirectoryShortName)
  , "Password" :: (Password)
  , "Description" :: NullOrUndefined (Description)
  , "Size" :: (DirectorySize)
  , "VpcSettings" :: NullOrUndefined (DirectoryVpcSettings)
  }
derive instance newtypeCreateDirectoryRequest :: Newtype CreateDirectoryRequest _


-- | <p>Contains the results of the <a>CreateDirectory</a> operation.</p>
newtype CreateDirectoryResult = CreateDirectoryResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  }
derive instance newtypeCreateDirectoryResult :: Newtype CreateDirectoryResult _


-- | <p>Creates a Microsoft AD in the AWS cloud.</p>
newtype CreateMicrosoftADRequest = CreateMicrosoftADRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined (DirectoryShortName)
  , "Password" :: (Password)
  , "Description" :: NullOrUndefined (Description)
  , "VpcSettings" :: (DirectoryVpcSettings)
  , "Edition" :: NullOrUndefined (DirectoryEdition)
  }
derive instance newtypeCreateMicrosoftADRequest :: Newtype CreateMicrosoftADRequest _


-- | <p>Result of a CreateMicrosoftAD request.</p>
newtype CreateMicrosoftADResult = CreateMicrosoftADResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  }
derive instance newtypeCreateMicrosoftADResult :: Newtype CreateMicrosoftADResult _


newtype CreateSnapshotBeforeSchemaExtension = CreateSnapshotBeforeSchemaExtension Boolean
derive instance newtypeCreateSnapshotBeforeSchemaExtension :: Newtype CreateSnapshotBeforeSchemaExtension _


-- | <p>Contains the inputs for the <a>CreateSnapshot</a> operation.</p>
newtype CreateSnapshotRequest = CreateSnapshotRequest 
  { "DirectoryId" :: (DirectoryId)
  , "Name" :: NullOrUndefined (SnapshotName)
  }
derive instance newtypeCreateSnapshotRequest :: Newtype CreateSnapshotRequest _


-- | <p>Contains the results of the <a>CreateSnapshot</a> operation.</p>
newtype CreateSnapshotResult = CreateSnapshotResult 
  { "SnapshotId" :: NullOrUndefined (SnapshotId)
  }
derive instance newtypeCreateSnapshotResult :: Newtype CreateSnapshotResult _


-- | <p>AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your Microsoft AD in the AWS cloud, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.</p> <p>This action initiates the creation of the AWS side of a trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>
newtype CreateTrustRequest = CreateTrustRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "TrustPassword" :: (TrustPassword)
  , "TrustDirection" :: (TrustDirection)
  , "TrustType" :: NullOrUndefined (TrustType)
  , "ConditionalForwarderIpAddrs" :: NullOrUndefined (DnsIpAddrs)
  }
derive instance newtypeCreateTrustRequest :: Newtype CreateTrustRequest _


-- | <p>The result of a CreateTrust request.</p>
newtype CreateTrustResult = CreateTrustResult 
  { "TrustId" :: NullOrUndefined (TrustId)
  }
derive instance newtypeCreateTrustResult :: Newtype CreateTrustResult _


newtype CreatedDateTime = CreatedDateTime Number
derive instance newtypeCreatedDateTime :: Newtype CreatedDateTime _


newtype DeleteAssociatedConditionalForwarder = DeleteAssociatedConditionalForwarder Boolean
derive instance newtypeDeleteAssociatedConditionalForwarder :: Newtype DeleteAssociatedConditionalForwarder _


-- | <p>Deletes a conditional forwarder.</p>
newtype DeleteConditionalForwarderRequest = DeleteConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  }
derive instance newtypeDeleteConditionalForwarderRequest :: Newtype DeleteConditionalForwarderRequest _


-- | <p>The result of a DeleteConditionalForwarder request.</p>
newtype DeleteConditionalForwarderResult = DeleteConditionalForwarderResult 
  { 
  }
derive instance newtypeDeleteConditionalForwarderResult :: Newtype DeleteConditionalForwarderResult _


-- | <p>Contains the inputs for the <a>DeleteDirectory</a> operation.</p>
newtype DeleteDirectoryRequest = DeleteDirectoryRequest 
  { "DirectoryId" :: (DirectoryId)
  }
derive instance newtypeDeleteDirectoryRequest :: Newtype DeleteDirectoryRequest _


-- | <p>Contains the results of the <a>DeleteDirectory</a> operation.</p>
newtype DeleteDirectoryResult = DeleteDirectoryResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  }
derive instance newtypeDeleteDirectoryResult :: Newtype DeleteDirectoryResult _


-- | <p>Contains the inputs for the <a>DeleteSnapshot</a> operation.</p>
newtype DeleteSnapshotRequest = DeleteSnapshotRequest 
  { "SnapshotId" :: (SnapshotId)
  }
derive instance newtypeDeleteSnapshotRequest :: Newtype DeleteSnapshotRequest _


-- | <p>Contains the results of the <a>DeleteSnapshot</a> operation.</p>
newtype DeleteSnapshotResult = DeleteSnapshotResult 
  { "SnapshotId" :: NullOrUndefined (SnapshotId)
  }
derive instance newtypeDeleteSnapshotResult :: Newtype DeleteSnapshotResult _


-- | <p>Deletes the local side of an existing trust relationship between the Microsoft AD in the AWS cloud and the external domain.</p>
newtype DeleteTrustRequest = DeleteTrustRequest 
  { "TrustId" :: (TrustId)
  , "DeleteAssociatedConditionalForwarder" :: NullOrUndefined (DeleteAssociatedConditionalForwarder)
  }
derive instance newtypeDeleteTrustRequest :: Newtype DeleteTrustRequest _


-- | <p>The result of a DeleteTrust request.</p>
newtype DeleteTrustResult = DeleteTrustResult 
  { "TrustId" :: NullOrUndefined (TrustId)
  }
derive instance newtypeDeleteTrustResult :: Newtype DeleteTrustResult _


-- | <p>Removes the specified directory as a publisher to the specified SNS topic.</p>
newtype DeregisterEventTopicRequest = DeregisterEventTopicRequest 
  { "DirectoryId" :: (DirectoryId)
  , "TopicName" :: (TopicName)
  }
derive instance newtypeDeregisterEventTopicRequest :: Newtype DeregisterEventTopicRequest _


-- | <p>The result of a DeregisterEventTopic request.</p>
newtype DeregisterEventTopicResult = DeregisterEventTopicResult 
  { 
  }
derive instance newtypeDeregisterEventTopicResult :: Newtype DeregisterEventTopicResult _


-- | <p>Describes a conditional forwarder.</p>
newtype DescribeConditionalForwardersRequest = DescribeConditionalForwardersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainNames" :: NullOrUndefined (RemoteDomainNames)
  }
derive instance newtypeDescribeConditionalForwardersRequest :: Newtype DescribeConditionalForwardersRequest _


-- | <p>The result of a DescribeConditionalForwarder request.</p>
newtype DescribeConditionalForwardersResult = DescribeConditionalForwardersResult 
  { "ConditionalForwarders" :: NullOrUndefined (ConditionalForwarders)
  }
derive instance newtypeDescribeConditionalForwardersResult :: Newtype DescribeConditionalForwardersResult _


-- | <p>Contains the inputs for the <a>DescribeDirectories</a> operation.</p>
newtype DescribeDirectoriesRequest = DescribeDirectoriesRequest 
  { "DirectoryIds" :: NullOrUndefined (DirectoryIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }
derive instance newtypeDescribeDirectoriesRequest :: Newtype DescribeDirectoriesRequest _


-- | <p>Contains the results of the <a>DescribeDirectories</a> operation.</p>
newtype DescribeDirectoriesResult = DescribeDirectoriesResult 
  { "DirectoryDescriptions" :: NullOrUndefined (DirectoryDescriptions)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeDirectoriesResult :: Newtype DescribeDirectoriesResult _


newtype DescribeDomainControllersRequest = DescribeDomainControllersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "DomainControllerIds" :: NullOrUndefined (DomainControllerIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }
derive instance newtypeDescribeDomainControllersRequest :: Newtype DescribeDomainControllersRequest _


newtype DescribeDomainControllersResult = DescribeDomainControllersResult 
  { "DomainControllers" :: NullOrUndefined (DomainControllers)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeDomainControllersResult :: Newtype DescribeDomainControllersResult _


-- | <p>Describes event topics.</p>
newtype DescribeEventTopicsRequest = DescribeEventTopicsRequest 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "TopicNames" :: NullOrUndefined (TopicNames)
  }
derive instance newtypeDescribeEventTopicsRequest :: Newtype DescribeEventTopicsRequest _


-- | <p>The result of a DescribeEventTopic request.</p>
newtype DescribeEventTopicsResult = DescribeEventTopicsResult 
  { "EventTopics" :: NullOrUndefined (EventTopics)
  }
derive instance newtypeDescribeEventTopicsResult :: Newtype DescribeEventTopicsResult _


-- | <p>Contains the inputs for the <a>DescribeSnapshots</a> operation.</p>
newtype DescribeSnapshotsRequest = DescribeSnapshotsRequest 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "SnapshotIds" :: NullOrUndefined (SnapshotIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }
derive instance newtypeDescribeSnapshotsRequest :: Newtype DescribeSnapshotsRequest _


-- | <p>Contains the results of the <a>DescribeSnapshots</a> operation.</p>
newtype DescribeSnapshotsResult = DescribeSnapshotsResult 
  { "Snapshots" :: NullOrUndefined (Snapshots)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeSnapshotsResult :: Newtype DescribeSnapshotsResult _


-- | <p>Describes the trust relationships for a particular Microsoft AD in the AWS cloud. If no input parameters are are provided, such as directory ID or trust ID, this request describes all the trust relationships.</p>
newtype DescribeTrustsRequest = DescribeTrustsRequest 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "TrustIds" :: NullOrUndefined (TrustIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }
derive instance newtypeDescribeTrustsRequest :: Newtype DescribeTrustsRequest _


-- | <p>The result of a DescribeTrust request.</p>
newtype DescribeTrustsResult = DescribeTrustsResult 
  { "Trusts" :: NullOrUndefined (Trusts)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeDescribeTrustsResult :: Newtype DescribeTrustsResult _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


newtype DesiredNumberOfDomainControllers = DesiredNumberOfDomainControllers Int
derive instance newtypeDesiredNumberOfDomainControllers :: Newtype DesiredNumberOfDomainControllers _


-- | <p>Contains information for the <a>ConnectDirectory</a> operation when an AD Connector directory is being created.</p>
newtype DirectoryConnectSettings = DirectoryConnectSettings 
  { "VpcId" :: (VpcId)
  , "SubnetIds" :: (SubnetIds)
  , "CustomerDnsIps" :: (DnsIpAddrs)
  , "CustomerUserName" :: (UserName)
  }
derive instance newtypeDirectoryConnectSettings :: Newtype DirectoryConnectSettings _


-- | <p>Contains information about an AD Connector directory.</p>
newtype DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription 
  { "VpcId" :: NullOrUndefined (VpcId)
  , "SubnetIds" :: NullOrUndefined (SubnetIds)
  , "CustomerUserName" :: NullOrUndefined (UserName)
  , "SecurityGroupId" :: NullOrUndefined (SecurityGroupId)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  , "ConnectIps" :: NullOrUndefined (IpAddrs)
  }
derive instance newtypeDirectoryConnectSettingsDescription :: Newtype DirectoryConnectSettingsDescription _


-- | <p>Contains information about an AWS Directory Service directory.</p>
newtype DirectoryDescription = DirectoryDescription 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "Name" :: NullOrUndefined (DirectoryName)
  , "ShortName" :: NullOrUndefined (DirectoryShortName)
  , "Size" :: NullOrUndefined (DirectorySize)
  , "Edition" :: NullOrUndefined (DirectoryEdition)
  , "Alias" :: NullOrUndefined (AliasName)
  , "AccessUrl" :: NullOrUndefined (AccessUrl)
  , "Description" :: NullOrUndefined (Description)
  , "DnsIpAddrs" :: NullOrUndefined (DnsIpAddrs)
  , "Stage" :: NullOrUndefined (DirectoryStage)
  , "LaunchTime" :: NullOrUndefined (LaunchTime)
  , "StageLastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime)
  , "Type" :: NullOrUndefined (DirectoryType)
  , "VpcSettings" :: NullOrUndefined (DirectoryVpcSettingsDescription)
  , "ConnectSettings" :: NullOrUndefined (DirectoryConnectSettingsDescription)
  , "RadiusSettings" :: NullOrUndefined (RadiusSettings)
  , "RadiusStatus" :: NullOrUndefined (RadiusStatus)
  , "StageReason" :: NullOrUndefined (StageReason)
  , "SsoEnabled" :: NullOrUndefined (SsoEnabled)
  , "DesiredNumberOfDomainControllers" :: NullOrUndefined (DesiredNumberOfDomainControllers)
  }
derive instance newtypeDirectoryDescription :: Newtype DirectoryDescription _


-- | <p>A list of directory descriptions.</p>
newtype DirectoryDescriptions = DirectoryDescriptions (Array DirectoryDescription)
derive instance newtypeDirectoryDescriptions :: Newtype DirectoryDescriptions _


newtype DirectoryEdition = DirectoryEdition String
derive instance newtypeDirectoryEdition :: Newtype DirectoryEdition _


newtype DirectoryId = DirectoryId String
derive instance newtypeDirectoryId :: Newtype DirectoryId _


-- | <p>A list of directory identifiers.</p>
newtype DirectoryIds = DirectoryIds (Array DirectoryId)
derive instance newtypeDirectoryIds :: Newtype DirectoryIds _


-- | <p>The maximum number of directories in the region has been reached. You can use the <a>GetDirectoryLimits</a> operation to determine your directory limits in the region.</p>
newtype DirectoryLimitExceededException = DirectoryLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeDirectoryLimitExceededException :: Newtype DirectoryLimitExceededException _


-- | <p>Contains directory limit information for a region.</p>
newtype DirectoryLimits = DirectoryLimits 
  { "CloudOnlyDirectoriesLimit" :: NullOrUndefined (Limit)
  , "CloudOnlyDirectoriesCurrentCount" :: NullOrUndefined (Limit)
  , "CloudOnlyDirectoriesLimitReached" :: NullOrUndefined (CloudOnlyDirectoriesLimitReached)
  , "CloudOnlyMicrosoftADLimit" :: NullOrUndefined (Limit)
  , "CloudOnlyMicrosoftADCurrentCount" :: NullOrUndefined (Limit)
  , "CloudOnlyMicrosoftADLimitReached" :: NullOrUndefined (CloudOnlyDirectoriesLimitReached)
  , "ConnectedDirectoriesLimit" :: NullOrUndefined (Limit)
  , "ConnectedDirectoriesCurrentCount" :: NullOrUndefined (Limit)
  , "ConnectedDirectoriesLimitReached" :: NullOrUndefined (ConnectedDirectoriesLimitReached)
  }
derive instance newtypeDirectoryLimits :: Newtype DirectoryLimits _


newtype DirectoryName = DirectoryName String
derive instance newtypeDirectoryName :: Newtype DirectoryName _


newtype DirectoryShortName = DirectoryShortName String
derive instance newtypeDirectoryShortName :: Newtype DirectoryShortName _


newtype DirectorySize = DirectorySize String
derive instance newtypeDirectorySize :: Newtype DirectorySize _


newtype DirectoryStage = DirectoryStage String
derive instance newtypeDirectoryStage :: Newtype DirectoryStage _


newtype DirectoryType = DirectoryType String
derive instance newtypeDirectoryType :: Newtype DirectoryType _


-- | <p>The specified directory is unavailable or could not be found.</p>
newtype DirectoryUnavailableException = DirectoryUnavailableException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeDirectoryUnavailableException :: Newtype DirectoryUnavailableException _


-- | <p>Contains VPC information for the <a>CreateDirectory</a> or <a>CreateMicrosoftAD</a> operation.</p>
newtype DirectoryVpcSettings = DirectoryVpcSettings 
  { "VpcId" :: (VpcId)
  , "SubnetIds" :: (SubnetIds)
  }
derive instance newtypeDirectoryVpcSettings :: Newtype DirectoryVpcSettings _


-- | <p>Contains information about the directory.</p>
newtype DirectoryVpcSettingsDescription = DirectoryVpcSettingsDescription 
  { "VpcId" :: NullOrUndefined (VpcId)
  , "SubnetIds" :: NullOrUndefined (SubnetIds)
  , "SecurityGroupId" :: NullOrUndefined (SecurityGroupId)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  }
derive instance newtypeDirectoryVpcSettingsDescription :: Newtype DirectoryVpcSettingsDescription _


-- | <p>Contains the inputs for the <a>DisableRadius</a> operation.</p>
newtype DisableRadiusRequest = DisableRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  }
derive instance newtypeDisableRadiusRequest :: Newtype DisableRadiusRequest _


-- | <p>Contains the results of the <a>DisableRadius</a> operation.</p>
newtype DisableRadiusResult = DisableRadiusResult 
  { 
  }
derive instance newtypeDisableRadiusResult :: Newtype DisableRadiusResult _


-- | <p>Contains the inputs for the <a>DisableSso</a> operation.</p>
newtype DisableSsoRequest = DisableSsoRequest 
  { "DirectoryId" :: (DirectoryId)
  , "UserName" :: NullOrUndefined (UserName)
  , "Password" :: NullOrUndefined (ConnectPassword)
  }
derive instance newtypeDisableSsoRequest :: Newtype DisableSsoRequest _


-- | <p>Contains the results of the <a>DisableSso</a> operation.</p>
newtype DisableSsoResult = DisableSsoResult 
  { 
  }
derive instance newtypeDisableSsoResult :: Newtype DisableSsoResult _


newtype DnsIpAddrs = DnsIpAddrs (Array IpAddr)
derive instance newtypeDnsIpAddrs :: Newtype DnsIpAddrs _


-- | <p>Contains information about the domain controllers for a specified directory.</p>
newtype DomainController = DomainController 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "DomainControllerId" :: NullOrUndefined (DomainControllerId)
  , "DnsIpAddr" :: NullOrUndefined (IpAddr)
  , "VpcId" :: NullOrUndefined (VpcId)
  , "SubnetId" :: NullOrUndefined (SubnetId)
  , "AvailabilityZone" :: NullOrUndefined (AvailabilityZone)
  , "Status" :: NullOrUndefined (DomainControllerStatus)
  , "StatusReason" :: NullOrUndefined (DomainControllerStatusReason)
  , "LaunchTime" :: NullOrUndefined (LaunchTime)
  , "StatusLastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime)
  }
derive instance newtypeDomainController :: Newtype DomainController _


newtype DomainControllerId = DomainControllerId String
derive instance newtypeDomainControllerId :: Newtype DomainControllerId _


newtype DomainControllerIds = DomainControllerIds (Array DomainControllerId)
derive instance newtypeDomainControllerIds :: Newtype DomainControllerIds _


-- | <p>The maximum allowed number of domain controllers per directory was exceeded. The default limit per directory is 20 domain controllers.</p>
newtype DomainControllerLimitExceededException = DomainControllerLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeDomainControllerLimitExceededException :: Newtype DomainControllerLimitExceededException _


newtype DomainControllerStatus = DomainControllerStatus String
derive instance newtypeDomainControllerStatus :: Newtype DomainControllerStatus _


newtype DomainControllerStatusReason = DomainControllerStatusReason String
derive instance newtypeDomainControllerStatusReason :: Newtype DomainControllerStatusReason _


newtype DomainControllers = DomainControllers (Array DomainController)
derive instance newtypeDomainControllers :: Newtype DomainControllers _


-- | <p>Contains the inputs for the <a>EnableRadius</a> operation.</p>
newtype EnableRadiusRequest = EnableRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RadiusSettings" :: (RadiusSettings)
  }
derive instance newtypeEnableRadiusRequest :: Newtype EnableRadiusRequest _


-- | <p>Contains the results of the <a>EnableRadius</a> operation.</p>
newtype EnableRadiusResult = EnableRadiusResult 
  { 
  }
derive instance newtypeEnableRadiusResult :: Newtype EnableRadiusResult _


-- | <p>Contains the inputs for the <a>EnableSso</a> operation.</p>
newtype EnableSsoRequest = EnableSsoRequest 
  { "DirectoryId" :: (DirectoryId)
  , "UserName" :: NullOrUndefined (UserName)
  , "Password" :: NullOrUndefined (ConnectPassword)
  }
derive instance newtypeEnableSsoRequest :: Newtype EnableSsoRequest _


-- | <p>Contains the results of the <a>EnableSso</a> operation.</p>
newtype EnableSsoResult = EnableSsoResult 
  { 
  }
derive instance newtypeEnableSsoResult :: Newtype EnableSsoResult _


newtype EndDateTime = EndDateTime Number
derive instance newtypeEndDateTime :: Newtype EndDateTime _


-- | <p>The specified entity already exists.</p>
newtype EntityAlreadyExistsException = EntityAlreadyExistsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeEntityAlreadyExistsException :: Newtype EntityAlreadyExistsException _


-- | <p>The specified entity could not be found.</p>
newtype EntityDoesNotExistException = EntityDoesNotExistException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeEntityDoesNotExistException :: Newtype EntityDoesNotExistException _


-- | <p>Information about SNS topic and AWS Directory Service directory associations.</p>
newtype EventTopic = EventTopic 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "TopicName" :: NullOrUndefined (TopicName)
  , "TopicArn" :: NullOrUndefined (TopicArn)
  , "CreatedDateTime" :: NullOrUndefined (CreatedDateTime)
  , "Status" :: NullOrUndefined (TopicStatus)
  }
derive instance newtypeEventTopic :: Newtype EventTopic _


newtype EventTopics = EventTopics (Array EventTopic)
derive instance newtypeEventTopics :: Newtype EventTopics _


-- | <p>The descriptive message for the exception.</p>
newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _


-- | <p>Contains the inputs for the <a>GetDirectoryLimits</a> operation.</p>
newtype GetDirectoryLimitsRequest = GetDirectoryLimitsRequest 
  { 
  }
derive instance newtypeGetDirectoryLimitsRequest :: Newtype GetDirectoryLimitsRequest _


-- | <p>Contains the results of the <a>GetDirectoryLimits</a> operation.</p>
newtype GetDirectoryLimitsResult = GetDirectoryLimitsResult 
  { "DirectoryLimits" :: NullOrUndefined (DirectoryLimits)
  }
derive instance newtypeGetDirectoryLimitsResult :: Newtype GetDirectoryLimitsResult _


-- | <p>Contains the inputs for the <a>GetSnapshotLimits</a> operation.</p>
newtype GetSnapshotLimitsRequest = GetSnapshotLimitsRequest 
  { "DirectoryId" :: (DirectoryId)
  }
derive instance newtypeGetSnapshotLimitsRequest :: Newtype GetSnapshotLimitsRequest _


-- | <p>Contains the results of the <a>GetSnapshotLimits</a> operation.</p>
newtype GetSnapshotLimitsResult = GetSnapshotLimitsResult 
  { "SnapshotLimits" :: NullOrUndefined (SnapshotLimits)
  }
derive instance newtypeGetSnapshotLimitsResult :: Newtype GetSnapshotLimitsResult _


-- | <p>The account does not have sufficient permission to perform the operation.</p>
newtype InsufficientPermissionsException = InsufficientPermissionsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeInsufficientPermissionsException :: Newtype InsufficientPermissionsException _


-- | <p>The <i>NextToken</i> value is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeInvalidNextTokenException :: Newtype InvalidNextTokenException _


-- | <p>One or more parameters are not valid.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeInvalidParameterException :: Newtype InvalidParameterException _


newtype IpAddr = IpAddr String
derive instance newtypeIpAddr :: Newtype IpAddr _


newtype IpAddrs = IpAddrs (Array IpAddr)
derive instance newtypeIpAddrs :: Newtype IpAddrs _


-- | <p>IP address block. This is often the address block of the DNS server used for your on-premises domain. </p>
newtype IpRoute = IpRoute 
  { "CidrIp" :: NullOrUndefined (CidrIp)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeIpRoute :: Newtype IpRoute _


-- | <p>Information about one or more IP address blocks.</p>
newtype IpRouteInfo = IpRouteInfo 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "CidrIp" :: NullOrUndefined (CidrIp)
  , "IpRouteStatusMsg" :: NullOrUndefined (IpRouteStatusMsg)
  , "AddedDateTime" :: NullOrUndefined (AddedDateTime)
  , "IpRouteStatusReason" :: NullOrUndefined (IpRouteStatusReason)
  , "Description" :: NullOrUndefined (Description)
  }
derive instance newtypeIpRouteInfo :: Newtype IpRouteInfo _


-- | <p>The maximum allowed number of IP addresses was exceeded. The default limit is 100 IP address blocks.</p>
newtype IpRouteLimitExceededException = IpRouteLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeIpRouteLimitExceededException :: Newtype IpRouteLimitExceededException _


newtype IpRouteStatusMsg = IpRouteStatusMsg String
derive instance newtypeIpRouteStatusMsg :: Newtype IpRouteStatusMsg _


newtype IpRouteStatusReason = IpRouteStatusReason String
derive instance newtypeIpRouteStatusReason :: Newtype IpRouteStatusReason _


newtype IpRoutes = IpRoutes (Array IpRoute)
derive instance newtypeIpRoutes :: Newtype IpRoutes _


newtype IpRoutesInfo = IpRoutesInfo (Array IpRouteInfo)
derive instance newtypeIpRoutesInfo :: Newtype IpRoutesInfo _


newtype LastUpdatedDateTime = LastUpdatedDateTime Number
derive instance newtypeLastUpdatedDateTime :: Newtype LastUpdatedDateTime _


newtype LaunchTime = LaunchTime Number
derive instance newtypeLaunchTime :: Newtype LaunchTime _


newtype LdifContent = LdifContent String
derive instance newtypeLdifContent :: Newtype LdifContent _


newtype Limit = Limit Int
derive instance newtypeLimit :: Newtype Limit _


newtype ListIpRoutesRequest = ListIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }
derive instance newtypeListIpRoutesRequest :: Newtype ListIpRoutesRequest _


newtype ListIpRoutesResult = ListIpRoutesResult 
  { "IpRoutesInfo" :: NullOrUndefined (IpRoutesInfo)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListIpRoutesResult :: Newtype ListIpRoutesResult _


newtype ListSchemaExtensionsRequest = ListSchemaExtensionsRequest 
  { "DirectoryId" :: (DirectoryId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }
derive instance newtypeListSchemaExtensionsRequest :: Newtype ListSchemaExtensionsRequest _


newtype ListSchemaExtensionsResult = ListSchemaExtensionsResult 
  { "SchemaExtensionsInfo" :: NullOrUndefined (SchemaExtensionsInfo)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListSchemaExtensionsResult :: Newtype ListSchemaExtensionsResult _


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }
derive instance newtypeListTagsForResourceRequest :: Newtype ListTagsForResourceRequest _


newtype ListTagsForResourceResult = ListTagsForResourceResult 
  { "Tags" :: NullOrUndefined (Tags)
  , "NextToken" :: NullOrUndefined (NextToken)
  }
derive instance newtypeListTagsForResourceResult :: Newtype ListTagsForResourceResult _


newtype ManualSnapshotsLimitReached = ManualSnapshotsLimitReached Boolean
derive instance newtypeManualSnapshotsLimitReached :: Newtype ManualSnapshotsLimitReached _


newtype NextToken = NextToken String
derive instance newtypeNextToken :: Newtype NextToken _


newtype OrganizationalUnitDN = OrganizationalUnitDN String
derive instance newtypeOrganizationalUnitDN :: Newtype OrganizationalUnitDN _


newtype Password = Password String
derive instance newtypePassword :: Newtype Password _


newtype PortNumber = PortNumber Int
derive instance newtypePortNumber :: Newtype PortNumber _


newtype RadiusAuthenticationProtocol = RadiusAuthenticationProtocol String
derive instance newtypeRadiusAuthenticationProtocol :: Newtype RadiusAuthenticationProtocol _


newtype RadiusDisplayLabel = RadiusDisplayLabel String
derive instance newtypeRadiusDisplayLabel :: Newtype RadiusDisplayLabel _


newtype RadiusRetries = RadiusRetries Int
derive instance newtypeRadiusRetries :: Newtype RadiusRetries _


-- | <p>Contains information about a Remote Authentication Dial In User Service (RADIUS) server.</p>
newtype RadiusSettings = RadiusSettings 
  { "RadiusServers" :: NullOrUndefined (Servers)
  , "RadiusPort" :: NullOrUndefined (PortNumber)
  , "RadiusTimeout" :: NullOrUndefined (RadiusTimeout)
  , "RadiusRetries" :: NullOrUndefined (RadiusRetries)
  , "SharedSecret" :: NullOrUndefined (RadiusSharedSecret)
  , "AuthenticationProtocol" :: NullOrUndefined (RadiusAuthenticationProtocol)
  , "DisplayLabel" :: NullOrUndefined (RadiusDisplayLabel)
  , "UseSameUsername" :: NullOrUndefined (UseSameUsername)
  }
derive instance newtypeRadiusSettings :: Newtype RadiusSettings _


newtype RadiusSharedSecret = RadiusSharedSecret String
derive instance newtypeRadiusSharedSecret :: Newtype RadiusSharedSecret _


newtype RadiusStatus = RadiusStatus String
derive instance newtypeRadiusStatus :: Newtype RadiusStatus _


newtype RadiusTimeout = RadiusTimeout Int
derive instance newtypeRadiusTimeout :: Newtype RadiusTimeout _


-- | <p>Registers a new event topic.</p>
newtype RegisterEventTopicRequest = RegisterEventTopicRequest 
  { "DirectoryId" :: (DirectoryId)
  , "TopicName" :: (TopicName)
  }
derive instance newtypeRegisterEventTopicRequest :: Newtype RegisterEventTopicRequest _


-- | <p>The result of a RegisterEventTopic request.</p>
newtype RegisterEventTopicResult = RegisterEventTopicResult 
  { 
  }
derive instance newtypeRegisterEventTopicResult :: Newtype RegisterEventTopicResult _


newtype RemoteDomainName = RemoteDomainName String
derive instance newtypeRemoteDomainName :: Newtype RemoteDomainName _


newtype RemoteDomainNames = RemoteDomainNames (Array RemoteDomainName)
derive instance newtypeRemoteDomainNames :: Newtype RemoteDomainNames _


newtype RemoveIpRoutesRequest = RemoveIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "CidrIps" :: (CidrIps)
  }
derive instance newtypeRemoveIpRoutesRequest :: Newtype RemoveIpRoutesRequest _


newtype RemoveIpRoutesResult = RemoveIpRoutesResult 
  { 
  }
derive instance newtypeRemoveIpRoutesResult :: Newtype RemoveIpRoutesResult _


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "TagKeys" :: (TagKeys)
  }
derive instance newtypeRemoveTagsFromResourceRequest :: Newtype RemoveTagsFromResourceRequest _


newtype RemoveTagsFromResourceResult = RemoveTagsFromResourceResult 
  { 
  }
derive instance newtypeRemoveTagsFromResourceResult :: Newtype RemoveTagsFromResourceResult _


newtype ReplicationScope = ReplicationScope String
derive instance newtypeReplicationScope :: Newtype ReplicationScope _


-- | <p>The AWS request identifier.</p>
newtype RequestId = RequestId String
derive instance newtypeRequestId :: Newtype RequestId _


newtype ResourceId = ResourceId String
derive instance newtypeResourceId :: Newtype ResourceId _


-- | <p>An object representing the inputs for the <a>RestoreFromSnapshot</a> operation.</p>
newtype RestoreFromSnapshotRequest = RestoreFromSnapshotRequest 
  { "SnapshotId" :: (SnapshotId)
  }
derive instance newtypeRestoreFromSnapshotRequest :: Newtype RestoreFromSnapshotRequest _


-- | <p>Contains the results of the <a>RestoreFromSnapshot</a> operation.</p>
newtype RestoreFromSnapshotResult = RestoreFromSnapshotResult 
  { 
  }
derive instance newtypeRestoreFromSnapshotResult :: Newtype RestoreFromSnapshotResult _


newtype SID = SID String
derive instance newtypeSID :: Newtype SID _


newtype SchemaExtensionId = SchemaExtensionId String
derive instance newtypeSchemaExtensionId :: Newtype SchemaExtensionId _


-- | <p>Information about a schema extension.</p>
newtype SchemaExtensionInfo = SchemaExtensionInfo 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "SchemaExtensionId" :: NullOrUndefined (SchemaExtensionId)
  , "Description" :: NullOrUndefined (Description)
  , "SchemaExtensionStatus" :: NullOrUndefined (SchemaExtensionStatus)
  , "SchemaExtensionStatusReason" :: NullOrUndefined (SchemaExtensionStatusReason)
  , "StartDateTime" :: NullOrUndefined (StartDateTime)
  , "EndDateTime" :: NullOrUndefined (EndDateTime)
  }
derive instance newtypeSchemaExtensionInfo :: Newtype SchemaExtensionInfo _


newtype SchemaExtensionStatus = SchemaExtensionStatus String
derive instance newtypeSchemaExtensionStatus :: Newtype SchemaExtensionStatus _


newtype SchemaExtensionStatusReason = SchemaExtensionStatusReason String
derive instance newtypeSchemaExtensionStatusReason :: Newtype SchemaExtensionStatusReason _


newtype SchemaExtensionsInfo = SchemaExtensionsInfo (Array SchemaExtensionInfo)
derive instance newtypeSchemaExtensionsInfo :: Newtype SchemaExtensionsInfo _


newtype SecurityGroupId = SecurityGroupId String
derive instance newtypeSecurityGroupId :: Newtype SecurityGroupId _


newtype Server = Server String
derive instance newtypeServer :: Newtype Server _


newtype Servers = Servers (Array Server)
derive instance newtypeServers :: Newtype Servers _


-- | <p>An exception has occurred in AWS Directory Service.</p>
newtype ServiceException = ServiceException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeServiceException :: Newtype ServiceException _


-- | <p>Describes a directory snapshot.</p>
newtype Snapshot = Snapshot 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "SnapshotId" :: NullOrUndefined (SnapshotId)
  , "Type" :: NullOrUndefined (SnapshotType)
  , "Name" :: NullOrUndefined (SnapshotName)
  , "Status" :: NullOrUndefined (SnapshotStatus)
  , "StartTime" :: NullOrUndefined (StartTime)
  }
derive instance newtypeSnapshot :: Newtype Snapshot _


newtype SnapshotId = SnapshotId String
derive instance newtypeSnapshotId :: Newtype SnapshotId _


-- | <p>A list of directory snapshot identifiers.</p>
newtype SnapshotIds = SnapshotIds (Array SnapshotId)
derive instance newtypeSnapshotIds :: Newtype SnapshotIds _


-- | <p>The maximum number of manual snapshots for the directory has been reached. You can use the <a>GetSnapshotLimits</a> operation to determine the snapshot limits for a directory.</p>
newtype SnapshotLimitExceededException = SnapshotLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeSnapshotLimitExceededException :: Newtype SnapshotLimitExceededException _


-- | <p>Contains manual snapshot limit information for a directory.</p>
newtype SnapshotLimits = SnapshotLimits 
  { "ManualSnapshotsLimit" :: NullOrUndefined (Limit)
  , "ManualSnapshotsCurrentCount" :: NullOrUndefined (Limit)
  , "ManualSnapshotsLimitReached" :: NullOrUndefined (ManualSnapshotsLimitReached)
  }
derive instance newtypeSnapshotLimits :: Newtype SnapshotLimits _


newtype SnapshotName = SnapshotName String
derive instance newtypeSnapshotName :: Newtype SnapshotName _


newtype SnapshotStatus = SnapshotStatus String
derive instance newtypeSnapshotStatus :: Newtype SnapshotStatus _


newtype SnapshotType = SnapshotType String
derive instance newtypeSnapshotType :: Newtype SnapshotType _


-- | <p>A list of descriptions of directory snapshots.</p>
newtype Snapshots = Snapshots (Array Snapshot)
derive instance newtypeSnapshots :: Newtype Snapshots _


newtype SsoEnabled = SsoEnabled Boolean
derive instance newtypeSsoEnabled :: Newtype SsoEnabled _


newtype StageReason = StageReason String
derive instance newtypeStageReason :: Newtype StageReason _


newtype StartDateTime = StartDateTime Number
derive instance newtypeStartDateTime :: Newtype StartDateTime _


newtype StartSchemaExtensionRequest = StartSchemaExtensionRequest 
  { "DirectoryId" :: (DirectoryId)
  , "CreateSnapshotBeforeSchemaExtension" :: (CreateSnapshotBeforeSchemaExtension)
  , "LdifContent" :: (LdifContent)
  , "Description" :: (Description)
  }
derive instance newtypeStartSchemaExtensionRequest :: Newtype StartSchemaExtensionRequest _


newtype StartSchemaExtensionResult = StartSchemaExtensionResult 
  { "SchemaExtensionId" :: NullOrUndefined (SchemaExtensionId)
  }
derive instance newtypeStartSchemaExtensionResult :: Newtype StartSchemaExtensionResult _


newtype StartTime = StartTime Number
derive instance newtypeStartTime :: Newtype StartTime _


newtype StateLastUpdatedDateTime = StateLastUpdatedDateTime Number
derive instance newtypeStateLastUpdatedDateTime :: Newtype StateLastUpdatedDateTime _


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _


newtype SubnetIds = SubnetIds (Array SubnetId)
derive instance newtypeSubnetIds :: Newtype SubnetIds _


-- | <p>Metadata assigned to a directory consisting of a key-value pair.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeys = TagKeys (Array TagKey)
derive instance newtypeTagKeys :: Newtype TagKeys _


-- | <p>The maximum allowed number of tags was exceeded.</p>
newtype TagLimitExceededException = TagLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeTagLimitExceededException :: Newtype TagLimitExceededException _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


newtype Tags = Tags (Array Tag)
derive instance newtypeTags :: Newtype Tags _


newtype TopicArn = TopicArn String
derive instance newtypeTopicArn :: Newtype TopicArn _


newtype TopicName = TopicName String
derive instance newtypeTopicName :: Newtype TopicName _


newtype TopicNames = TopicNames (Array TopicName)
derive instance newtypeTopicNames :: Newtype TopicNames _


newtype TopicStatus = TopicStatus String
derive instance newtypeTopicStatus :: Newtype TopicStatus _


-- | <p>Describes a trust relationship between an Microsoft AD in the AWS cloud and an external domain.</p>
newtype Trust = Trust 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "TrustId" :: NullOrUndefined (TrustId)
  , "RemoteDomainName" :: NullOrUndefined (RemoteDomainName)
  , "TrustType" :: NullOrUndefined (TrustType)
  , "TrustDirection" :: NullOrUndefined (TrustDirection)
  , "TrustState" :: NullOrUndefined (TrustState)
  , "CreatedDateTime" :: NullOrUndefined (CreatedDateTime)
  , "LastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime)
  , "StateLastUpdatedDateTime" :: NullOrUndefined (StateLastUpdatedDateTime)
  , "TrustStateReason" :: NullOrUndefined (TrustStateReason)
  }
derive instance newtypeTrust :: Newtype Trust _


newtype TrustDirection = TrustDirection String
derive instance newtypeTrustDirection :: Newtype TrustDirection _


newtype TrustId = TrustId String
derive instance newtypeTrustId :: Newtype TrustId _


newtype TrustIds = TrustIds (Array TrustId)
derive instance newtypeTrustIds :: Newtype TrustIds _


newtype TrustPassword = TrustPassword String
derive instance newtypeTrustPassword :: Newtype TrustPassword _


newtype TrustState = TrustState String
derive instance newtypeTrustState :: Newtype TrustState _


newtype TrustStateReason = TrustStateReason String
derive instance newtypeTrustStateReason :: Newtype TrustStateReason _


newtype TrustType = TrustType String
derive instance newtypeTrustType :: Newtype TrustType _


newtype Trusts = Trusts (Array Trust)
derive instance newtypeTrusts :: Newtype Trusts _


-- | <p>The operation is not supported.</p>
newtype UnsupportedOperationException = UnsupportedOperationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }
derive instance newtypeUnsupportedOperationException :: Newtype UnsupportedOperationException _


-- | <p>Updates a conditional forwarder.</p>
newtype UpdateConditionalForwarderRequest = UpdateConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "DnsIpAddrs" :: (DnsIpAddrs)
  }
derive instance newtypeUpdateConditionalForwarderRequest :: Newtype UpdateConditionalForwarderRequest _


-- | <p>The result of an UpdateConditionalForwarder request.</p>
newtype UpdateConditionalForwarderResult = UpdateConditionalForwarderResult 
  { 
  }
derive instance newtypeUpdateConditionalForwarderResult :: Newtype UpdateConditionalForwarderResult _


newtype UpdateNumberOfDomainControllersRequest = UpdateNumberOfDomainControllersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "DesiredNumber" :: (DesiredNumberOfDomainControllers)
  }
derive instance newtypeUpdateNumberOfDomainControllersRequest :: Newtype UpdateNumberOfDomainControllersRequest _


newtype UpdateNumberOfDomainControllersResult = UpdateNumberOfDomainControllersResult 
  { 
  }
derive instance newtypeUpdateNumberOfDomainControllersResult :: Newtype UpdateNumberOfDomainControllersResult _


-- | <p>Contains the inputs for the <a>UpdateRadius</a> operation.</p>
newtype UpdateRadiusRequest = UpdateRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RadiusSettings" :: (RadiusSettings)
  }
derive instance newtypeUpdateRadiusRequest :: Newtype UpdateRadiusRequest _


-- | <p>Contains the results of the <a>UpdateRadius</a> operation.</p>
newtype UpdateRadiusResult = UpdateRadiusResult 
  { 
  }
derive instance newtypeUpdateRadiusResult :: Newtype UpdateRadiusResult _


newtype UpdateSecurityGroupForDirectoryControllers = UpdateSecurityGroupForDirectoryControllers Boolean
derive instance newtypeUpdateSecurityGroupForDirectoryControllers :: Newtype UpdateSecurityGroupForDirectoryControllers _


newtype UseSameUsername = UseSameUsername Boolean
derive instance newtypeUseSameUsername :: Newtype UseSameUsername _


newtype UserName = UserName String
derive instance newtypeUserName :: Newtype UserName _


-- | <p>Initiates the verification of an existing trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>
newtype VerifyTrustRequest = VerifyTrustRequest 
  { "TrustId" :: (TrustId)
  }
derive instance newtypeVerifyTrustRequest :: Newtype VerifyTrustRequest _


-- | <p>Result of a VerifyTrust request.</p>
newtype VerifyTrustResult = VerifyTrustResult 
  { "TrustId" :: NullOrUndefined (TrustId)
  }
derive instance newtypeVerifyTrustResult :: Newtype VerifyTrustResult _


newtype VpcId = VpcId String
derive instance newtypeVpcId :: Newtype VpcId _
