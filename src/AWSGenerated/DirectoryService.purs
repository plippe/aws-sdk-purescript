

-- | <fullname>AWS Directory Service</fullname> <p>AWS Directory Service is a web service that makes it easy for you to setup and run directories in the AWS cloud, or connect your AWS resources with an existing on-premises Microsoft Active Directory. This guide provides detailed information about AWS Directory Service operations, data types, parameters, and errors. For information about AWS Directory Services features, see <a href="https://aws.amazon.com/directoryservice/">AWS Directory Service</a> and the <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html">AWS Directory Service Administration Guide</a>.</p> <note> <p>AWS provides SDKs that consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .Net, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to AWS Directory Service and other AWS services. For more information about the AWS SDKs, including how to download and install them, see <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> </note>
module AWS.DirectoryService where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
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


newtype AddIpRoutesRequest = AddIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "IpRoutes" :: (IpRoutes)
  , "UpdateSecurityGroupForDirectoryControllers" :: NullOrUndefined (UpdateSecurityGroupForDirectoryControllers)
  }


newtype AddIpRoutesResult = AddIpRoutesResult 
  { 
  }


newtype AddTagsToResourceRequest = AddTagsToResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "Tags" :: (Tags)
  }


newtype AddTagsToResourceResult = AddTagsToResourceResult 
  { 
  }


newtype AddedDateTime = AddedDateTime Number


newtype AliasName = AliasName String


-- | <p>Represents a named directory attribute.</p>
newtype Attribute = Attribute 
  { "Name" :: NullOrUndefined (AttributeName)
  , "Value" :: NullOrUndefined (AttributeValue)
  }


newtype AttributeName = AttributeName String


newtype AttributeValue = AttributeValue String


newtype Attributes = Attributes (Array Attribute)


-- | <p>An authentication error occurred.</p>
newtype AuthenticationFailedException = AuthenticationFailedException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


newtype AvailabilityZone = AvailabilityZone String


newtype AvailabilityZones = AvailabilityZones (Array AvailabilityZone)


newtype CancelSchemaExtensionRequest = CancelSchemaExtensionRequest 
  { "DirectoryId" :: (DirectoryId)
  , "SchemaExtensionId" :: (SchemaExtensionId)
  }


newtype CancelSchemaExtensionResult = CancelSchemaExtensionResult 
  { 
  }


newtype CidrIp = CidrIp String


newtype CidrIps = CidrIps (Array CidrIp)


-- | <p>A client exception has occurred.</p>
newtype ClientException = ClientException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


newtype CloudOnlyDirectoriesLimitReached = CloudOnlyDirectoriesLimitReached Boolean


-- | <p>Contains information about a computer account in a directory.</p>
newtype Computer = Computer 
  { "ComputerId" :: NullOrUndefined (SID)
  , "ComputerName" :: NullOrUndefined (ComputerName)
  , "ComputerAttributes" :: NullOrUndefined (Attributes)
  }


newtype ComputerName = ComputerName String


newtype ComputerPassword = ComputerPassword String


-- | <p>Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>
newtype ConditionalForwarder = ConditionalForwarder 
  { "RemoteDomainName" :: NullOrUndefined (RemoteDomainName)
  , "DnsIpAddrs" :: NullOrUndefined (DnsIpAddrs)
  , "ReplicationScope" :: NullOrUndefined (ReplicationScope)
  }


newtype ConditionalForwarders = ConditionalForwarders (Array ConditionalForwarder)


-- | <p>Contains the inputs for the <a>ConnectDirectory</a> operation.</p>
newtype ConnectDirectoryRequest = ConnectDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined (DirectoryShortName)
  , "Password" :: (ConnectPassword)
  , "Description" :: NullOrUndefined (Description)
  , "Size" :: (DirectorySize)
  , "ConnectSettings" :: (DirectoryConnectSettings)
  }


-- | <p>Contains the results of the <a>ConnectDirectory</a> operation.</p>
newtype ConnectDirectoryResult = ConnectDirectoryResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  }


newtype ConnectPassword = ConnectPassword String


newtype ConnectedDirectoriesLimitReached = ConnectedDirectoriesLimitReached Boolean


-- | <p>Contains the inputs for the <a>CreateAlias</a> operation.</p>
newtype CreateAliasRequest = CreateAliasRequest 
  { "DirectoryId" :: (DirectoryId)
  , "Alias" :: (AliasName)
  }


-- | <p>Contains the results of the <a>CreateAlias</a> operation.</p>
newtype CreateAliasResult = CreateAliasResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "Alias" :: NullOrUndefined (AliasName)
  }


-- | <p>Contains the inputs for the <a>CreateComputer</a> operation.</p>
newtype CreateComputerRequest = CreateComputerRequest 
  { "DirectoryId" :: (DirectoryId)
  , "ComputerName" :: (ComputerName)
  , "Password" :: (ComputerPassword)
  , "OrganizationalUnitDistinguishedName" :: NullOrUndefined (OrganizationalUnitDN)
  , "ComputerAttributes" :: NullOrUndefined (Attributes)
  }


-- | <p>Contains the results for the <a>CreateComputer</a> operation.</p>
newtype CreateComputerResult = CreateComputerResult 
  { "Computer" :: NullOrUndefined (Computer)
  }


-- | <p>Initiates the creation of a conditional forwarder for your AWS Directory Service for Microsoft Active Directory. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>
newtype CreateConditionalForwarderRequest = CreateConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "DnsIpAddrs" :: (DnsIpAddrs)
  }


-- | <p>The result of a CreateConditinalForwarder request.</p>
newtype CreateConditionalForwarderResult = CreateConditionalForwarderResult 
  { 
  }


-- | <p>Contains the inputs for the <a>CreateDirectory</a> operation. </p>
newtype CreateDirectoryRequest = CreateDirectoryRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined (DirectoryShortName)
  , "Password" :: (Password)
  , "Description" :: NullOrUndefined (Description)
  , "Size" :: (DirectorySize)
  , "VpcSettings" :: NullOrUndefined (DirectoryVpcSettings)
  }


-- | <p>Contains the results of the <a>CreateDirectory</a> operation.</p>
newtype CreateDirectoryResult = CreateDirectoryResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  }


-- | <p>Creates a Microsoft AD in the AWS cloud.</p>
newtype CreateMicrosoftADRequest = CreateMicrosoftADRequest 
  { "Name" :: (DirectoryName)
  , "ShortName" :: NullOrUndefined (DirectoryShortName)
  , "Password" :: (Password)
  , "Description" :: NullOrUndefined (Description)
  , "VpcSettings" :: (DirectoryVpcSettings)
  , "Edition" :: NullOrUndefined (DirectoryEdition)
  }


-- | <p>Result of a CreateMicrosoftAD request.</p>
newtype CreateMicrosoftADResult = CreateMicrosoftADResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  }


newtype CreateSnapshotBeforeSchemaExtension = CreateSnapshotBeforeSchemaExtension Boolean


-- | <p>Contains the inputs for the <a>CreateSnapshot</a> operation.</p>
newtype CreateSnapshotRequest = CreateSnapshotRequest 
  { "DirectoryId" :: (DirectoryId)
  , "Name" :: NullOrUndefined (SnapshotName)
  }


-- | <p>Contains the results of the <a>CreateSnapshot</a> operation.</p>
newtype CreateSnapshotResult = CreateSnapshotResult 
  { "SnapshotId" :: NullOrUndefined (SnapshotId)
  }


-- | <p>AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your Microsoft AD in the AWS cloud, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.</p> <p>This action initiates the creation of the AWS side of a trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>
newtype CreateTrustRequest = CreateTrustRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "TrustPassword" :: (TrustPassword)
  , "TrustDirection" :: (TrustDirection)
  , "TrustType" :: NullOrUndefined (TrustType)
  , "ConditionalForwarderIpAddrs" :: NullOrUndefined (DnsIpAddrs)
  }


-- | <p>The result of a CreateTrust request.</p>
newtype CreateTrustResult = CreateTrustResult 
  { "TrustId" :: NullOrUndefined (TrustId)
  }


newtype CreatedDateTime = CreatedDateTime Number


newtype DeleteAssociatedConditionalForwarder = DeleteAssociatedConditionalForwarder Boolean


-- | <p>Deletes a conditional forwarder.</p>
newtype DeleteConditionalForwarderRequest = DeleteConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  }


-- | <p>The result of a DeleteConditionalForwarder request.</p>
newtype DeleteConditionalForwarderResult = DeleteConditionalForwarderResult 
  { 
  }


-- | <p>Contains the inputs for the <a>DeleteDirectory</a> operation.</p>
newtype DeleteDirectoryRequest = DeleteDirectoryRequest 
  { "DirectoryId" :: (DirectoryId)
  }


-- | <p>Contains the results of the <a>DeleteDirectory</a> operation.</p>
newtype DeleteDirectoryResult = DeleteDirectoryResult 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  }


-- | <p>Contains the inputs for the <a>DeleteSnapshot</a> operation.</p>
newtype DeleteSnapshotRequest = DeleteSnapshotRequest 
  { "SnapshotId" :: (SnapshotId)
  }


-- | <p>Contains the results of the <a>DeleteSnapshot</a> operation.</p>
newtype DeleteSnapshotResult = DeleteSnapshotResult 
  { "SnapshotId" :: NullOrUndefined (SnapshotId)
  }


-- | <p>Deletes the local side of an existing trust relationship between the Microsoft AD in the AWS cloud and the external domain.</p>
newtype DeleteTrustRequest = DeleteTrustRequest 
  { "TrustId" :: (TrustId)
  , "DeleteAssociatedConditionalForwarder" :: NullOrUndefined (DeleteAssociatedConditionalForwarder)
  }


-- | <p>The result of a DeleteTrust request.</p>
newtype DeleteTrustResult = DeleteTrustResult 
  { "TrustId" :: NullOrUndefined (TrustId)
  }


-- | <p>Removes the specified directory as a publisher to the specified SNS topic.</p>
newtype DeregisterEventTopicRequest = DeregisterEventTopicRequest 
  { "DirectoryId" :: (DirectoryId)
  , "TopicName" :: (TopicName)
  }


-- | <p>The result of a DeregisterEventTopic request.</p>
newtype DeregisterEventTopicResult = DeregisterEventTopicResult 
  { 
  }


-- | <p>Describes a conditional forwarder.</p>
newtype DescribeConditionalForwardersRequest = DescribeConditionalForwardersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainNames" :: NullOrUndefined (RemoteDomainNames)
  }


-- | <p>The result of a DescribeConditionalForwarder request.</p>
newtype DescribeConditionalForwardersResult = DescribeConditionalForwardersResult 
  { "ConditionalForwarders" :: NullOrUndefined (ConditionalForwarders)
  }


-- | <p>Contains the inputs for the <a>DescribeDirectories</a> operation.</p>
newtype DescribeDirectoriesRequest = DescribeDirectoriesRequest 
  { "DirectoryIds" :: NullOrUndefined (DirectoryIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }


-- | <p>Contains the results of the <a>DescribeDirectories</a> operation.</p>
newtype DescribeDirectoriesResult = DescribeDirectoriesResult 
  { "DirectoryDescriptions" :: NullOrUndefined (DirectoryDescriptions)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype DescribeDomainControllersRequest = DescribeDomainControllersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "DomainControllerIds" :: NullOrUndefined (DomainControllerIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }


newtype DescribeDomainControllersResult = DescribeDomainControllersResult 
  { "DomainControllers" :: NullOrUndefined (DomainControllers)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Describes event topics.</p>
newtype DescribeEventTopicsRequest = DescribeEventTopicsRequest 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "TopicNames" :: NullOrUndefined (TopicNames)
  }


-- | <p>The result of a DescribeEventTopic request.</p>
newtype DescribeEventTopicsResult = DescribeEventTopicsResult 
  { "EventTopics" :: NullOrUndefined (EventTopics)
  }


-- | <p>Contains the inputs for the <a>DescribeSnapshots</a> operation.</p>
newtype DescribeSnapshotsRequest = DescribeSnapshotsRequest 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "SnapshotIds" :: NullOrUndefined (SnapshotIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }


-- | <p>Contains the results of the <a>DescribeSnapshots</a> operation.</p>
newtype DescribeSnapshotsResult = DescribeSnapshotsResult 
  { "Snapshots" :: NullOrUndefined (Snapshots)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


-- | <p>Describes the trust relationships for a particular Microsoft AD in the AWS cloud. If no input parameters are are provided, such as directory ID or trust ID, this request describes all the trust relationships.</p>
newtype DescribeTrustsRequest = DescribeTrustsRequest 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "TrustIds" :: NullOrUndefined (TrustIds)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }


-- | <p>The result of a DescribeTrust request.</p>
newtype DescribeTrustsResult = DescribeTrustsResult 
  { "Trusts" :: NullOrUndefined (Trusts)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype Description = Description String


newtype DesiredNumberOfDomainControllers = DesiredNumberOfDomainControllers Int


-- | <p>Contains information for the <a>ConnectDirectory</a> operation when an AD Connector directory is being created.</p>
newtype DirectoryConnectSettings = DirectoryConnectSettings 
  { "VpcId" :: (VpcId)
  , "SubnetIds" :: (SubnetIds)
  , "CustomerDnsIps" :: (DnsIpAddrs)
  , "CustomerUserName" :: (UserName)
  }


-- | <p>Contains information about an AD Connector directory.</p>
newtype DirectoryConnectSettingsDescription = DirectoryConnectSettingsDescription 
  { "VpcId" :: NullOrUndefined (VpcId)
  , "SubnetIds" :: NullOrUndefined (SubnetIds)
  , "CustomerUserName" :: NullOrUndefined (UserName)
  , "SecurityGroupId" :: NullOrUndefined (SecurityGroupId)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  , "ConnectIps" :: NullOrUndefined (IpAddrs)
  }


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


-- | <p>A list of directory descriptions.</p>
newtype DirectoryDescriptions = DirectoryDescriptions (Array DirectoryDescription)


newtype DirectoryEdition = DirectoryEdition String


newtype DirectoryId = DirectoryId String


-- | <p>A list of directory identifiers.</p>
newtype DirectoryIds = DirectoryIds (Array DirectoryId)


-- | <p>The maximum number of directories in the region has been reached. You can use the <a>GetDirectoryLimits</a> operation to determine your directory limits in the region.</p>
newtype DirectoryLimitExceededException = DirectoryLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


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


newtype DirectoryName = DirectoryName String


newtype DirectoryShortName = DirectoryShortName String


newtype DirectorySize = DirectorySize String


newtype DirectoryStage = DirectoryStage String


newtype DirectoryType = DirectoryType String


-- | <p>The specified directory is unavailable or could not be found.</p>
newtype DirectoryUnavailableException = DirectoryUnavailableException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


-- | <p>Contains VPC information for the <a>CreateDirectory</a> or <a>CreateMicrosoftAD</a> operation.</p>
newtype DirectoryVpcSettings = DirectoryVpcSettings 
  { "VpcId" :: (VpcId)
  , "SubnetIds" :: (SubnetIds)
  }


-- | <p>Contains information about the directory.</p>
newtype DirectoryVpcSettingsDescription = DirectoryVpcSettingsDescription 
  { "VpcId" :: NullOrUndefined (VpcId)
  , "SubnetIds" :: NullOrUndefined (SubnetIds)
  , "SecurityGroupId" :: NullOrUndefined (SecurityGroupId)
  , "AvailabilityZones" :: NullOrUndefined (AvailabilityZones)
  }


-- | <p>Contains the inputs for the <a>DisableRadius</a> operation.</p>
newtype DisableRadiusRequest = DisableRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  }


-- | <p>Contains the results of the <a>DisableRadius</a> operation.</p>
newtype DisableRadiusResult = DisableRadiusResult 
  { 
  }


-- | <p>Contains the inputs for the <a>DisableSso</a> operation.</p>
newtype DisableSsoRequest = DisableSsoRequest 
  { "DirectoryId" :: (DirectoryId)
  , "UserName" :: NullOrUndefined (UserName)
  , "Password" :: NullOrUndefined (ConnectPassword)
  }


-- | <p>Contains the results of the <a>DisableSso</a> operation.</p>
newtype DisableSsoResult = DisableSsoResult 
  { 
  }


newtype DnsIpAddrs = DnsIpAddrs (Array IpAddr)


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


newtype DomainControllerId = DomainControllerId String


newtype DomainControllerIds = DomainControllerIds (Array DomainControllerId)


-- | <p>The maximum allowed number of domain controllers per directory was exceeded. The default limit per directory is 20 domain controllers.</p>
newtype DomainControllerLimitExceededException = DomainControllerLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


newtype DomainControllerStatus = DomainControllerStatus String


newtype DomainControllerStatusReason = DomainControllerStatusReason String


newtype DomainControllers = DomainControllers (Array DomainController)


-- | <p>Contains the inputs for the <a>EnableRadius</a> operation.</p>
newtype EnableRadiusRequest = EnableRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RadiusSettings" :: (RadiusSettings)
  }


-- | <p>Contains the results of the <a>EnableRadius</a> operation.</p>
newtype EnableRadiusResult = EnableRadiusResult 
  { 
  }


-- | <p>Contains the inputs for the <a>EnableSso</a> operation.</p>
newtype EnableSsoRequest = EnableSsoRequest 
  { "DirectoryId" :: (DirectoryId)
  , "UserName" :: NullOrUndefined (UserName)
  , "Password" :: NullOrUndefined (ConnectPassword)
  }


-- | <p>Contains the results of the <a>EnableSso</a> operation.</p>
newtype EnableSsoResult = EnableSsoResult 
  { 
  }


newtype EndDateTime = EndDateTime Number


-- | <p>The specified entity already exists.</p>
newtype EntityAlreadyExistsException = EntityAlreadyExistsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


-- | <p>The specified entity could not be found.</p>
newtype EntityDoesNotExistException = EntityDoesNotExistException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


-- | <p>Information about SNS topic and AWS Directory Service directory associations.</p>
newtype EventTopic = EventTopic 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "TopicName" :: NullOrUndefined (TopicName)
  , "TopicArn" :: NullOrUndefined (TopicArn)
  , "CreatedDateTime" :: NullOrUndefined (CreatedDateTime)
  , "Status" :: NullOrUndefined (TopicStatus)
  }


newtype EventTopics = EventTopics (Array EventTopic)


-- | <p>The descriptive message for the exception.</p>
newtype ExceptionMessage = ExceptionMessage String


-- | <p>Contains the inputs for the <a>GetDirectoryLimits</a> operation.</p>
newtype GetDirectoryLimitsRequest = GetDirectoryLimitsRequest 
  { 
  }


-- | <p>Contains the results of the <a>GetDirectoryLimits</a> operation.</p>
newtype GetDirectoryLimitsResult = GetDirectoryLimitsResult 
  { "DirectoryLimits" :: NullOrUndefined (DirectoryLimits)
  }


-- | <p>Contains the inputs for the <a>GetSnapshotLimits</a> operation.</p>
newtype GetSnapshotLimitsRequest = GetSnapshotLimitsRequest 
  { "DirectoryId" :: (DirectoryId)
  }


-- | <p>Contains the results of the <a>GetSnapshotLimits</a> operation.</p>
newtype GetSnapshotLimitsResult = GetSnapshotLimitsResult 
  { "SnapshotLimits" :: NullOrUndefined (SnapshotLimits)
  }


-- | <p>The account does not have sufficient permission to perform the operation.</p>
newtype InsufficientPermissionsException = InsufficientPermissionsException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


-- | <p>The <i>NextToken</i> value is not valid.</p>
newtype InvalidNextTokenException = InvalidNextTokenException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


-- | <p>One or more parameters are not valid.</p>
newtype InvalidParameterException = InvalidParameterException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


newtype IpAddr = IpAddr String


newtype IpAddrs = IpAddrs (Array IpAddr)


-- | <p>IP address block. This is often the address block of the DNS server used for your on-premises domain. </p>
newtype IpRoute = IpRoute 
  { "CidrIp" :: NullOrUndefined (CidrIp)
  , "Description" :: NullOrUndefined (Description)
  }


-- | <p>Information about one or more IP address blocks.</p>
newtype IpRouteInfo = IpRouteInfo 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "CidrIp" :: NullOrUndefined (CidrIp)
  , "IpRouteStatusMsg" :: NullOrUndefined (IpRouteStatusMsg)
  , "AddedDateTime" :: NullOrUndefined (AddedDateTime)
  , "IpRouteStatusReason" :: NullOrUndefined (IpRouteStatusReason)
  , "Description" :: NullOrUndefined (Description)
  }


-- | <p>The maximum allowed number of IP addresses was exceeded. The default limit is 100 IP address blocks.</p>
newtype IpRouteLimitExceededException = IpRouteLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


newtype IpRouteStatusMsg = IpRouteStatusMsg String


newtype IpRouteStatusReason = IpRouteStatusReason String


newtype IpRoutes = IpRoutes (Array IpRoute)


newtype IpRoutesInfo = IpRoutesInfo (Array IpRouteInfo)


newtype LastUpdatedDateTime = LastUpdatedDateTime Number


newtype LaunchTime = LaunchTime Number


newtype LdifContent = LdifContent String


newtype Limit = Limit Int


newtype ListIpRoutesRequest = ListIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }


newtype ListIpRoutesResult = ListIpRoutesResult 
  { "IpRoutesInfo" :: NullOrUndefined (IpRoutesInfo)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListSchemaExtensionsRequest = ListSchemaExtensionsRequest 
  { "DirectoryId" :: (DirectoryId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }


newtype ListSchemaExtensionsResult = ListSchemaExtensionsResult 
  { "SchemaExtensionsInfo" :: NullOrUndefined (SchemaExtensionsInfo)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ListTagsForResourceRequest = ListTagsForResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "NextToken" :: NullOrUndefined (NextToken)
  , "Limit" :: NullOrUndefined (Limit)
  }


newtype ListTagsForResourceResult = ListTagsForResourceResult 
  { "Tags" :: NullOrUndefined (Tags)
  , "NextToken" :: NullOrUndefined (NextToken)
  }


newtype ManualSnapshotsLimitReached = ManualSnapshotsLimitReached Boolean


newtype NextToken = NextToken String


newtype OrganizationalUnitDN = OrganizationalUnitDN String


newtype Password = Password String


newtype PortNumber = PortNumber Int


newtype RadiusAuthenticationProtocol = RadiusAuthenticationProtocol String


newtype RadiusDisplayLabel = RadiusDisplayLabel String


newtype RadiusRetries = RadiusRetries Int


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


newtype RadiusSharedSecret = RadiusSharedSecret String


newtype RadiusStatus = RadiusStatus String


newtype RadiusTimeout = RadiusTimeout Int


-- | <p>Registers a new event topic.</p>
newtype RegisterEventTopicRequest = RegisterEventTopicRequest 
  { "DirectoryId" :: (DirectoryId)
  , "TopicName" :: (TopicName)
  }


-- | <p>The result of a RegisterEventTopic request.</p>
newtype RegisterEventTopicResult = RegisterEventTopicResult 
  { 
  }


newtype RemoteDomainName = RemoteDomainName String


newtype RemoteDomainNames = RemoteDomainNames (Array RemoteDomainName)


newtype RemoveIpRoutesRequest = RemoveIpRoutesRequest 
  { "DirectoryId" :: (DirectoryId)
  , "CidrIps" :: (CidrIps)
  }


newtype RemoveIpRoutesResult = RemoveIpRoutesResult 
  { 
  }


newtype RemoveTagsFromResourceRequest = RemoveTagsFromResourceRequest 
  { "ResourceId" :: (ResourceId)
  , "TagKeys" :: (TagKeys)
  }


newtype RemoveTagsFromResourceResult = RemoveTagsFromResourceResult 
  { 
  }


newtype ReplicationScope = ReplicationScope String


-- | <p>The AWS request identifier.</p>
newtype RequestId = RequestId String


newtype ResourceId = ResourceId String


-- | <p>An object representing the inputs for the <a>RestoreFromSnapshot</a> operation.</p>
newtype RestoreFromSnapshotRequest = RestoreFromSnapshotRequest 
  { "SnapshotId" :: (SnapshotId)
  }


-- | <p>Contains the results of the <a>RestoreFromSnapshot</a> operation.</p>
newtype RestoreFromSnapshotResult = RestoreFromSnapshotResult 
  { 
  }


newtype SID = SID String


newtype SchemaExtensionId = SchemaExtensionId String


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


newtype SchemaExtensionStatus = SchemaExtensionStatus String


newtype SchemaExtensionStatusReason = SchemaExtensionStatusReason String


newtype SchemaExtensionsInfo = SchemaExtensionsInfo (Array SchemaExtensionInfo)


newtype SecurityGroupId = SecurityGroupId String


newtype Server = Server String


newtype Servers = Servers (Array Server)


-- | <p>An exception has occurred in AWS Directory Service.</p>
newtype ServiceException = ServiceException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


-- | <p>Describes a directory snapshot.</p>
newtype Snapshot = Snapshot 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "SnapshotId" :: NullOrUndefined (SnapshotId)
  , "Type" :: NullOrUndefined (SnapshotType)
  , "Name" :: NullOrUndefined (SnapshotName)
  , "Status" :: NullOrUndefined (SnapshotStatus)
  , "StartTime" :: NullOrUndefined (StartTime)
  }


newtype SnapshotId = SnapshotId String


-- | <p>A list of directory snapshot identifiers.</p>
newtype SnapshotIds = SnapshotIds (Array SnapshotId)


-- | <p>The maximum number of manual snapshots for the directory has been reached. You can use the <a>GetSnapshotLimits</a> operation to determine the snapshot limits for a directory.</p>
newtype SnapshotLimitExceededException = SnapshotLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


-- | <p>Contains manual snapshot limit information for a directory.</p>
newtype SnapshotLimits = SnapshotLimits 
  { "ManualSnapshotsLimit" :: NullOrUndefined (Limit)
  , "ManualSnapshotsCurrentCount" :: NullOrUndefined (Limit)
  , "ManualSnapshotsLimitReached" :: NullOrUndefined (ManualSnapshotsLimitReached)
  }


newtype SnapshotName = SnapshotName String


newtype SnapshotStatus = SnapshotStatus String


newtype SnapshotType = SnapshotType String


-- | <p>A list of descriptions of directory snapshots.</p>
newtype Snapshots = Snapshots (Array Snapshot)


newtype SsoEnabled = SsoEnabled Boolean


newtype StageReason = StageReason String


newtype StartDateTime = StartDateTime Number


newtype StartSchemaExtensionRequest = StartSchemaExtensionRequest 
  { "DirectoryId" :: (DirectoryId)
  , "CreateSnapshotBeforeSchemaExtension" :: (CreateSnapshotBeforeSchemaExtension)
  , "LdifContent" :: (LdifContent)
  , "Description" :: (Description)
  }


newtype StartSchemaExtensionResult = StartSchemaExtensionResult 
  { "SchemaExtensionId" :: NullOrUndefined (SchemaExtensionId)
  }


newtype StartTime = StartTime Number


newtype StateLastUpdatedDateTime = StateLastUpdatedDateTime Number


newtype SubnetId = SubnetId String


newtype SubnetIds = SubnetIds (Array SubnetId)


-- | <p>Metadata assigned to a directory consisting of a key-value pair.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeys = TagKeys (Array TagKey)


-- | <p>The maximum allowed number of tags was exceeded.</p>
newtype TagLimitExceededException = TagLimitExceededException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


newtype TagValue = TagValue String


newtype Tags = Tags (Array Tag)


newtype TopicArn = TopicArn String


newtype TopicName = TopicName String


newtype TopicNames = TopicNames (Array TopicName)


newtype TopicStatus = TopicStatus String


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


newtype TrustDirection = TrustDirection String


newtype TrustId = TrustId String


newtype TrustIds = TrustIds (Array TrustId)


newtype TrustPassword = TrustPassword String


newtype TrustState = TrustState String


newtype TrustStateReason = TrustStateReason String


newtype TrustType = TrustType String


newtype Trusts = Trusts (Array Trust)


-- | <p>The operation is not supported.</p>
newtype UnsupportedOperationException = UnsupportedOperationException 
  { "Message" :: NullOrUndefined (ExceptionMessage)
  , "RequestId" :: NullOrUndefined (RequestId)
  }


-- | <p>Updates a conditional forwarder.</p>
newtype UpdateConditionalForwarderRequest = UpdateConditionalForwarderRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RemoteDomainName" :: (RemoteDomainName)
  , "DnsIpAddrs" :: (DnsIpAddrs)
  }


-- | <p>The result of an UpdateConditionalForwarder request.</p>
newtype UpdateConditionalForwarderResult = UpdateConditionalForwarderResult 
  { 
  }


newtype UpdateNumberOfDomainControllersRequest = UpdateNumberOfDomainControllersRequest 
  { "DirectoryId" :: (DirectoryId)
  , "DesiredNumber" :: (DesiredNumberOfDomainControllers)
  }


newtype UpdateNumberOfDomainControllersResult = UpdateNumberOfDomainControllersResult 
  { 
  }


-- | <p>Contains the inputs for the <a>UpdateRadius</a> operation.</p>
newtype UpdateRadiusRequest = UpdateRadiusRequest 
  { "DirectoryId" :: (DirectoryId)
  , "RadiusSettings" :: (RadiusSettings)
  }


-- | <p>Contains the results of the <a>UpdateRadius</a> operation.</p>
newtype UpdateRadiusResult = UpdateRadiusResult 
  { 
  }


newtype UpdateSecurityGroupForDirectoryControllers = UpdateSecurityGroupForDirectoryControllers Boolean


newtype UseSameUsername = UseSameUsername Boolean


newtype UserName = UserName String


-- | <p>Initiates the verification of an existing trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>
newtype VerifyTrustRequest = VerifyTrustRequest 
  { "TrustId" :: (TrustId)
  }


-- | <p>Result of a VerifyTrust request.</p>
newtype VerifyTrustResult = VerifyTrustResult 
  { "TrustId" :: NullOrUndefined (TrustId)
  }


newtype VpcId = VpcId String
