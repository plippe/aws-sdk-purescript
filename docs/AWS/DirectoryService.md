## Module AWS.DirectoryService

<fullname>AWS Directory Service</fullname> <p>AWS Directory Service is a web service that makes it easy for you to setup and run directories in the AWS cloud, or connect your AWS resources with an existing on-premises Microsoft Active Directory. This guide provides detailed information about AWS Directory Service operations, data types, parameters, and errors. For information about AWS Directory Services features, see <a href="https://aws.amazon.com/directoryservice/">AWS Directory Service</a> and the <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/what_is.html">AWS Directory Service Administration Guide</a>.</p> <note> <p>AWS provides SDKs that consist of libraries and sample code for various programming languages and platforms (Java, Ruby, .Net, iOS, Android, etc.). The SDKs provide a convenient way to create programmatic access to AWS Directory Service and other AWS services. For more information about the AWS SDKs, including how to download and install them, see <a href="http://aws.amazon.com/tools/">Tools for Amazon Web Services</a>.</p> </note>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `addIpRoutes`

``` purescript
addIpRoutes :: forall eff. AddIpRoutesRequest -> Aff (err :: RequestError | eff) AddIpRoutesResult
```

<p>If the DNS server for your on-premises domain uses a publicly addressable IP address, you must add a CIDR address block to correctly route traffic to and from your Microsoft AD on Amazon Web Services. <i>AddIpRoutes</i> adds this address block. You can also use <i>AddIpRoutes</i> to facilitate routing traffic that uses public IP ranges from your Microsoft AD on AWS to a peer VPC. </p> <p>Before you call <i>AddIpRoutes</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>AddIpRoutes</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>

#### `addTagsToResource`

``` purescript
addTagsToResource :: forall eff. AddTagsToResourceRequest -> Aff (err :: RequestError | eff) AddTagsToResourceResult
```

<p>Adds or overwrites one or more tags for the specified directory. Each directory can have a maximum of 50 tags. Each tag consists of a key and optional value. Tag keys must be unique to each resource.</p>

#### `cancelSchemaExtension`

``` purescript
cancelSchemaExtension :: forall eff. CancelSchemaExtensionRequest -> Aff (err :: RequestError | eff) CancelSchemaExtensionResult
```

<p>Cancels an in-progress schema extension to a Microsoft AD directory. Once a schema extension has started replicating to all domain controllers, the task can no longer be canceled. A schema extension can be canceled during any of the following states; <code>Initializing</code>, <code>CreatingSnapshot</code>, and <code>UpdatingSchema</code>.</p>

#### `connectDirectory`

``` purescript
connectDirectory :: forall eff. ConnectDirectoryRequest -> Aff (err :: RequestError | eff) ConnectDirectoryResult
```

<p>Creates an AD Connector to connect to an on-premises directory.</p> <p>Before you call <i>ConnectDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>ConnectDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>

#### `createAlias`

``` purescript
createAlias :: forall eff. CreateAliasRequest -> Aff (err :: RequestError | eff) CreateAliasResult
```

<p>Creates an alias for a directory and assigns the alias to the directory. The alias is used to construct the access URL for the directory, such as <code>http://&lt;alias&gt;.awsapps.com</code>.</p> <important> <p>After an alias has been created, it cannot be deleted or reused, so this operation should only be used when absolutely necessary.</p> </important>

#### `createComputer`

``` purescript
createComputer :: forall eff. CreateComputerRequest -> Aff (err :: RequestError | eff) CreateComputerResult
```

<p>Creates a computer account in the specified directory, and joins the computer to the directory.</p>

#### `createConditionalForwarder`

``` purescript
createConditionalForwarder :: forall eff. CreateConditionalForwarderRequest -> Aff (err :: RequestError | eff) CreateConditionalForwarderResult
```

<p>Creates a conditional forwarder associated with your AWS directory. Conditional forwarders are required in order to set up a trust relationship with another domain. The conditional forwarder points to the trusted domain.</p>

#### `createDirectory`

``` purescript
createDirectory :: forall eff. CreateDirectoryRequest -> Aff (err :: RequestError | eff) CreateDirectoryResult
```

<p>Creates a Simple AD directory.</p> <p>Before you call <i>CreateDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>CreateDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>

#### `createMicrosoftAD`

``` purescript
createMicrosoftAD :: forall eff. CreateMicrosoftADRequest -> Aff (err :: RequestError | eff) CreateMicrosoftADResult
```

<p>Creates a Microsoft AD in the AWS cloud.</p> <p>Before you call <i>CreateMicrosoftAD</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>CreateMicrosoftAD</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>

#### `createSnapshot`

``` purescript
createSnapshot :: forall eff. CreateSnapshotRequest -> Aff (err :: RequestError | eff) CreateSnapshotResult
```

<p>Creates a snapshot of a Simple AD or Microsoft AD directory in the AWS cloud.</p> <note> <p>You cannot take snapshots of AD Connector directories.</p> </note>

#### `createTrust`

``` purescript
createTrust :: forall eff. CreateTrustRequest -> Aff (err :: RequestError | eff) CreateTrustResult
```

<p>AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your Microsoft AD in the AWS cloud, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.</p> <p>This action initiates the creation of the AWS side of a trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>

#### `deleteConditionalForwarder`

``` purescript
deleteConditionalForwarder :: forall eff. DeleteConditionalForwarderRequest -> Aff (err :: RequestError | eff) DeleteConditionalForwarderResult
```

<p>Deletes a conditional forwarder that has been set up for your AWS directory.</p>

#### `deleteDirectory`

``` purescript
deleteDirectory :: forall eff. DeleteDirectoryRequest -> Aff (err :: RequestError | eff) DeleteDirectoryResult
```

<p>Deletes an AWS Directory Service directory.</p> <p>Before you call <i>DeleteDirectory</i>, ensure that all of the required permissions have been explicitly granted through a policy. For details about what permissions are required to run the <i>DeleteDirectory</i> operation, see <a href="http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html">AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference</a>.</p>

#### `deleteSnapshot`

``` purescript
deleteSnapshot :: forall eff. DeleteSnapshotRequest -> Aff (err :: RequestError | eff) DeleteSnapshotResult
```

<p>Deletes a directory snapshot.</p>

#### `deleteTrust`

``` purescript
deleteTrust :: forall eff. DeleteTrustRequest -> Aff (err :: RequestError | eff) DeleteTrustResult
```

<p>Deletes an existing trust relationship between your Microsoft AD in the AWS cloud and an external domain.</p>

#### `deregisterEventTopic`

``` purescript
deregisterEventTopic :: forall eff. DeregisterEventTopicRequest -> Aff (err :: RequestError | eff) DeregisterEventTopicResult
```

<p>Removes the specified directory as a publisher to the specified SNS topic.</p>

#### `describeConditionalForwarders`

``` purescript
describeConditionalForwarders :: forall eff. DescribeConditionalForwardersRequest -> Aff (err :: RequestError | eff) DescribeConditionalForwardersResult
```

<p>Obtains information about the conditional forwarders for this account.</p> <p>If no input parameters are provided for RemoteDomainNames, this request describes all conditional forwarders for the specified directory ID.</p>

#### `describeDirectories`

``` purescript
describeDirectories :: forall eff. DescribeDirectoriesRequest -> Aff (err :: RequestError | eff) DescribeDirectoriesResult
```

<p>Obtains information about the directories that belong to this account.</p> <p>You can retrieve information about specific directories by passing the directory identifiers in the <i>DirectoryIds</i> parameter. Otherwise, all directories that belong to the current account are returned.</p> <p>This operation supports pagination with the use of the <i>NextToken</i> request and response parameters. If more results are available, the <i>DescribeDirectoriesResult.NextToken</i> member contains a token that you pass in the next call to <a>DescribeDirectories</a> to retrieve the next set of items.</p> <p>You can also specify a maximum number of return results with the <i>Limit</i> parameter.</p>

#### `describeDomainControllers`

``` purescript
describeDomainControllers :: forall eff. DescribeDomainControllersRequest -> Aff (err :: RequestError | eff) DescribeDomainControllersResult
```

<p>Provides information about any domain controllers in your directory.</p>

#### `describeEventTopics`

``` purescript
describeEventTopics :: forall eff. DescribeEventTopicsRequest -> Aff (err :: RequestError | eff) DescribeEventTopicsResult
```

<p>Obtains information about which SNS topics receive status messages from the specified directory.</p> <p>If no input parameters are provided, such as DirectoryId or TopicName, this request describes all of the associations in the account.</p>

#### `describeSnapshots`

``` purescript
describeSnapshots :: forall eff. DescribeSnapshotsRequest -> Aff (err :: RequestError | eff) DescribeSnapshotsResult
```

<p>Obtains information about the directory snapshots that belong to this account.</p> <p>This operation supports pagination with the use of the <i>NextToken</i> request and response parameters. If more results are available, the <i>DescribeSnapshots.NextToken</i> member contains a token that you pass in the next call to <a>DescribeSnapshots</a> to retrieve the next set of items.</p> <p>You can also specify a maximum number of return results with the <i>Limit</i> parameter.</p>

#### `describeTrusts`

``` purescript
describeTrusts :: forall eff. DescribeTrustsRequest -> Aff (err :: RequestError | eff) DescribeTrustsResult
```

<p>Obtains information about the trust relationships for this account.</p> <p>If no input parameters are provided, such as DirectoryId or TrustIds, this request describes all the trust relationships belonging to the account.</p>

#### `disableRadius`

``` purescript
disableRadius :: forall eff. DisableRadiusRequest -> Aff (err :: RequestError | eff) DisableRadiusResult
```

<p>Disables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector directory.</p>

#### `disableSso`

``` purescript
disableSso :: forall eff. DisableSsoRequest -> Aff (err :: RequestError | eff) DisableSsoResult
```

<p>Disables single-sign on for a directory.</p>

#### `enableRadius`

``` purescript
enableRadius :: forall eff. EnableRadiusRequest -> Aff (err :: RequestError | eff) EnableRadiusResult
```

<p>Enables multi-factor authentication (MFA) with the Remote Authentication Dial In User Service (RADIUS) server for an AD Connector directory.</p>

#### `enableSso`

``` purescript
enableSso :: forall eff. EnableSsoRequest -> Aff (err :: RequestError | eff) EnableSsoResult
```

<p>Enables single sign-on for a directory.</p>

#### `getDirectoryLimits`

``` purescript
getDirectoryLimits :: forall eff. GetDirectoryLimitsRequest -> Aff (err :: RequestError | eff) GetDirectoryLimitsResult
```

<p>Obtains directory limit information for the current region.</p>

#### `getSnapshotLimits`

``` purescript
getSnapshotLimits :: forall eff. GetSnapshotLimitsRequest -> Aff (err :: RequestError | eff) GetSnapshotLimitsResult
```

<p>Obtains the manual snapshot limits for a directory.</p>

#### `listIpRoutes`

``` purescript
listIpRoutes :: forall eff. ListIpRoutesRequest -> Aff (err :: RequestError | eff) ListIpRoutesResult
```

<p>Lists the address blocks that you have added to a directory.</p>

#### `listSchemaExtensions`

``` purescript
listSchemaExtensions :: forall eff. ListSchemaExtensionsRequest -> Aff (err :: RequestError | eff) ListSchemaExtensionsResult
```

<p>Lists all schema extensions applied to a Microsoft AD Directory.</p>

#### `listTagsForResource`

``` purescript
listTagsForResource :: forall eff. ListTagsForResourceRequest -> Aff (err :: RequestError | eff) ListTagsForResourceResult
```

<p>Lists all tags on a directory.</p>

#### `registerEventTopic`

``` purescript
registerEventTopic :: forall eff. RegisterEventTopicRequest -> Aff (err :: RequestError | eff) RegisterEventTopicResult
```

<p>Associates a directory with an SNS topic. This establishes the directory as a publisher to the specified SNS topic. You can then receive email or text (SMS) messages when the status of your directory changes. You get notified if your directory goes from an Active status to an Impaired or Inoperable status. You also receive a notification when the directory returns to an Active status.</p>

#### `removeIpRoutes`

``` purescript
removeIpRoutes :: forall eff. RemoveIpRoutesRequest -> Aff (err :: RequestError | eff) RemoveIpRoutesResult
```

<p>Removes IP address blocks from a directory.</p>

#### `removeTagsFromResource`

``` purescript
removeTagsFromResource :: forall eff. RemoveTagsFromResourceRequest -> Aff (err :: RequestError | eff) RemoveTagsFromResourceResult
```

<p>Removes tags from a directory.</p>

#### `restoreFromSnapshot`

``` purescript
restoreFromSnapshot :: forall eff. RestoreFromSnapshotRequest -> Aff (err :: RequestError | eff) RestoreFromSnapshotResult
```

<p>Restores a directory using an existing directory snapshot.</p> <p>When you restore a directory from a snapshot, any changes made to the directory after the snapshot date are overwritten.</p> <p>This action returns as soon as the restore operation is initiated. You can monitor the progress of the restore operation by calling the <a>DescribeDirectories</a> operation with the directory identifier. When the <b>DirectoryDescription.Stage</b> value changes to <code>Active</code>, the restore operation is complete.</p>

#### `startSchemaExtension`

``` purescript
startSchemaExtension :: forall eff. StartSchemaExtensionRequest -> Aff (err :: RequestError | eff) StartSchemaExtensionResult
```

<p>Applies a schema extension to a Microsoft AD directory.</p>

#### `updateConditionalForwarder`

``` purescript
updateConditionalForwarder :: forall eff. UpdateConditionalForwarderRequest -> Aff (err :: RequestError | eff) UpdateConditionalForwarderResult
```

<p>Updates a conditional forwarder that has been set up for your AWS directory.</p>

#### `updateNumberOfDomainControllers`

``` purescript
updateNumberOfDomainControllers :: forall eff. UpdateNumberOfDomainControllersRequest -> Aff (err :: RequestError | eff) UpdateNumberOfDomainControllersResult
```

<p>Adds or removes domain controllers to or from the directory. Based on the difference between current value and new value (provided through this API call), domain controllers will be added or removed. It may take up to 45 minutes for any new domain controllers to become fully active once the requested number of domain controllers is updated. During this time, you cannot make another update request.</p>

#### `updateRadius`

``` purescript
updateRadius :: forall eff. UpdateRadiusRequest -> Aff (err :: RequestError | eff) UpdateRadiusResult
```

<p>Updates the Remote Authentication Dial In User Service (RADIUS) server information for an AD Connector directory.</p>

#### `verifyTrust`

``` purescript
verifyTrust :: forall eff. VerifyTrustRequest -> Aff (err :: RequestError | eff) VerifyTrustResult
```

<p>AWS Directory Service for Microsoft Active Directory allows you to configure and verify trust relationships.</p> <p>This action verifies a trust relationship between your Microsoft AD in the AWS cloud and an external domain.</p>

#### `AccessUrl`

``` purescript
newtype AccessUrl
  = AccessUrl String
```

#### `AddIpRoutesRequest`

``` purescript
newtype AddIpRoutesRequest
  = AddIpRoutesRequest { "DirectoryId" :: DirectoryId, "IpRoutes" :: IpRoutes, "UpdateSecurityGroupForDirectoryControllers" :: NullOrUndefined (UpdateSecurityGroupForDirectoryControllers) }
```

#### `AddIpRoutesResult`

``` purescript
newtype AddIpRoutesResult
  = AddIpRoutesResult {  }
```

#### `AddTagsToResourceRequest`

``` purescript
newtype AddTagsToResourceRequest
  = AddTagsToResourceRequest { "ResourceId" :: ResourceId, "Tags" :: Tags }
```

#### `AddTagsToResourceResult`

``` purescript
newtype AddTagsToResourceResult
  = AddTagsToResourceResult {  }
```

#### `AddedDateTime`

``` purescript
newtype AddedDateTime
  = AddedDateTime Number
```

#### `AliasName`

``` purescript
newtype AliasName
  = AliasName String
```

#### `Attribute`

``` purescript
newtype Attribute
  = Attribute { "Name" :: NullOrUndefined (AttributeName), "Value" :: NullOrUndefined (AttributeValue) }
```

<p>Represents a named directory attribute.</p>

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

#### `Attributes`

``` purescript
newtype Attributes
  = Attributes (Array Attribute)
```

#### `AuthenticationFailedException`

``` purescript
newtype AuthenticationFailedException
  = AuthenticationFailedException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>An authentication error occurred.</p>

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone String
```

#### `AvailabilityZones`

``` purescript
newtype AvailabilityZones
  = AvailabilityZones (Array AvailabilityZone)
```

#### `CancelSchemaExtensionRequest`

``` purescript
newtype CancelSchemaExtensionRequest
  = CancelSchemaExtensionRequest { "DirectoryId" :: DirectoryId, "SchemaExtensionId" :: SchemaExtensionId }
```

#### `CancelSchemaExtensionResult`

``` purescript
newtype CancelSchemaExtensionResult
  = CancelSchemaExtensionResult {  }
```

#### `CidrIp`

``` purescript
newtype CidrIp
  = CidrIp String
```

#### `CidrIps`

``` purescript
newtype CidrIps
  = CidrIps (Array CidrIp)
```

#### `ClientException`

``` purescript
newtype ClientException
  = ClientException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>A client exception has occurred.</p>

#### `CloudOnlyDirectoriesLimitReached`

``` purescript
newtype CloudOnlyDirectoriesLimitReached
  = CloudOnlyDirectoriesLimitReached Boolean
```

#### `Computer`

``` purescript
newtype Computer
  = Computer { "ComputerId" :: NullOrUndefined (SID), "ComputerName" :: NullOrUndefined (ComputerName), "ComputerAttributes" :: NullOrUndefined (Attributes) }
```

<p>Contains information about a computer account in a directory.</p>

#### `ComputerName`

``` purescript
newtype ComputerName
  = ComputerName String
```

#### `ComputerPassword`

``` purescript
newtype ComputerPassword
  = ComputerPassword String
```

#### `ConditionalForwarder`

``` purescript
newtype ConditionalForwarder
  = ConditionalForwarder { "RemoteDomainName" :: NullOrUndefined (RemoteDomainName), "DnsIpAddrs" :: NullOrUndefined (DnsIpAddrs), "ReplicationScope" :: NullOrUndefined (ReplicationScope) }
```

<p>Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>

#### `ConditionalForwarders`

``` purescript
newtype ConditionalForwarders
  = ConditionalForwarders (Array ConditionalForwarder)
```

#### `ConnectDirectoryRequest`

``` purescript
newtype ConnectDirectoryRequest
  = ConnectDirectoryRequest { "Name" :: DirectoryName, "ShortName" :: NullOrUndefined (DirectoryShortName), "Password" :: ConnectPassword, "Description" :: NullOrUndefined (Description), "Size" :: DirectorySize, "ConnectSettings" :: DirectoryConnectSettings }
```

<p>Contains the inputs for the <a>ConnectDirectory</a> operation.</p>

#### `ConnectDirectoryResult`

``` purescript
newtype ConnectDirectoryResult
  = ConnectDirectoryResult { "DirectoryId" :: NullOrUndefined (DirectoryId) }
```

<p>Contains the results of the <a>ConnectDirectory</a> operation.</p>

#### `ConnectPassword`

``` purescript
newtype ConnectPassword
  = ConnectPassword String
```

#### `ConnectedDirectoriesLimitReached`

``` purescript
newtype ConnectedDirectoriesLimitReached
  = ConnectedDirectoriesLimitReached Boolean
```

#### `CreateAliasRequest`

``` purescript
newtype CreateAliasRequest
  = CreateAliasRequest { "DirectoryId" :: DirectoryId, "Alias" :: AliasName }
```

<p>Contains the inputs for the <a>CreateAlias</a> operation.</p>

#### `CreateAliasResult`

``` purescript
newtype CreateAliasResult
  = CreateAliasResult { "DirectoryId" :: NullOrUndefined (DirectoryId), "Alias" :: NullOrUndefined (AliasName) }
```

<p>Contains the results of the <a>CreateAlias</a> operation.</p>

#### `CreateComputerRequest`

``` purescript
newtype CreateComputerRequest
  = CreateComputerRequest { "DirectoryId" :: DirectoryId, "ComputerName" :: ComputerName, "Password" :: ComputerPassword, "OrganizationalUnitDistinguishedName" :: NullOrUndefined (OrganizationalUnitDN), "ComputerAttributes" :: NullOrUndefined (Attributes) }
```

<p>Contains the inputs for the <a>CreateComputer</a> operation.</p>

#### `CreateComputerResult`

``` purescript
newtype CreateComputerResult
  = CreateComputerResult { "Computer" :: NullOrUndefined (Computer) }
```

<p>Contains the results for the <a>CreateComputer</a> operation.</p>

#### `CreateConditionalForwarderRequest`

``` purescript
newtype CreateConditionalForwarderRequest
  = CreateConditionalForwarderRequest { "DirectoryId" :: DirectoryId, "RemoteDomainName" :: RemoteDomainName, "DnsIpAddrs" :: DnsIpAddrs }
```

<p>Initiates the creation of a conditional forwarder for your AWS Directory Service for Microsoft Active Directory. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>

#### `CreateConditionalForwarderResult`

``` purescript
newtype CreateConditionalForwarderResult
  = CreateConditionalForwarderResult {  }
```

<p>The result of a CreateConditinalForwarder request.</p>

#### `CreateDirectoryRequest`

``` purescript
newtype CreateDirectoryRequest
  = CreateDirectoryRequest { "Name" :: DirectoryName, "ShortName" :: NullOrUndefined (DirectoryShortName), "Password" :: Password, "Description" :: NullOrUndefined (Description), "Size" :: DirectorySize, "VpcSettings" :: NullOrUndefined (DirectoryVpcSettings) }
```

<p>Contains the inputs for the <a>CreateDirectory</a> operation. </p>

#### `CreateDirectoryResult`

``` purescript
newtype CreateDirectoryResult
  = CreateDirectoryResult { "DirectoryId" :: NullOrUndefined (DirectoryId) }
```

<p>Contains the results of the <a>CreateDirectory</a> operation.</p>

#### `CreateMicrosoftADRequest`

``` purescript
newtype CreateMicrosoftADRequest
  = CreateMicrosoftADRequest { "Name" :: DirectoryName, "ShortName" :: NullOrUndefined (DirectoryShortName), "Password" :: Password, "Description" :: NullOrUndefined (Description), "VpcSettings" :: DirectoryVpcSettings, "Edition" :: NullOrUndefined (DirectoryEdition) }
```

<p>Creates a Microsoft AD in the AWS cloud.</p>

#### `CreateMicrosoftADResult`

``` purescript
newtype CreateMicrosoftADResult
  = CreateMicrosoftADResult { "DirectoryId" :: NullOrUndefined (DirectoryId) }
```

<p>Result of a CreateMicrosoftAD request.</p>

#### `CreateSnapshotBeforeSchemaExtension`

``` purescript
newtype CreateSnapshotBeforeSchemaExtension
  = CreateSnapshotBeforeSchemaExtension Boolean
```

#### `CreateSnapshotRequest`

``` purescript
newtype CreateSnapshotRequest
  = CreateSnapshotRequest { "DirectoryId" :: DirectoryId, "Name" :: NullOrUndefined (SnapshotName) }
```

<p>Contains the inputs for the <a>CreateSnapshot</a> operation.</p>

#### `CreateSnapshotResult`

``` purescript
newtype CreateSnapshotResult
  = CreateSnapshotResult { "SnapshotId" :: NullOrUndefined (SnapshotId) }
```

<p>Contains the results of the <a>CreateSnapshot</a> operation.</p>

#### `CreateTrustRequest`

``` purescript
newtype CreateTrustRequest
  = CreateTrustRequest { "DirectoryId" :: DirectoryId, "RemoteDomainName" :: RemoteDomainName, "TrustPassword" :: TrustPassword, "TrustDirection" :: TrustDirection, "TrustType" :: NullOrUndefined (TrustType), "ConditionalForwarderIpAddrs" :: NullOrUndefined (DnsIpAddrs) }
```

<p>AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your Microsoft AD in the AWS cloud, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.</p> <p>This action initiates the creation of the AWS side of a trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>

#### `CreateTrustResult`

``` purescript
newtype CreateTrustResult
  = CreateTrustResult { "TrustId" :: NullOrUndefined (TrustId) }
```

<p>The result of a CreateTrust request.</p>

#### `CreatedDateTime`

``` purescript
newtype CreatedDateTime
  = CreatedDateTime Number
```

#### `DeleteAssociatedConditionalForwarder`

``` purescript
newtype DeleteAssociatedConditionalForwarder
  = DeleteAssociatedConditionalForwarder Boolean
```

#### `DeleteConditionalForwarderRequest`

``` purescript
newtype DeleteConditionalForwarderRequest
  = DeleteConditionalForwarderRequest { "DirectoryId" :: DirectoryId, "RemoteDomainName" :: RemoteDomainName }
```

<p>Deletes a conditional forwarder.</p>

#### `DeleteConditionalForwarderResult`

``` purescript
newtype DeleteConditionalForwarderResult
  = DeleteConditionalForwarderResult {  }
```

<p>The result of a DeleteConditionalForwarder request.</p>

#### `DeleteDirectoryRequest`

``` purescript
newtype DeleteDirectoryRequest
  = DeleteDirectoryRequest { "DirectoryId" :: DirectoryId }
```

<p>Contains the inputs for the <a>DeleteDirectory</a> operation.</p>

#### `DeleteDirectoryResult`

``` purescript
newtype DeleteDirectoryResult
  = DeleteDirectoryResult { "DirectoryId" :: NullOrUndefined (DirectoryId) }
```

<p>Contains the results of the <a>DeleteDirectory</a> operation.</p>

#### `DeleteSnapshotRequest`

``` purescript
newtype DeleteSnapshotRequest
  = DeleteSnapshotRequest { "SnapshotId" :: SnapshotId }
```

<p>Contains the inputs for the <a>DeleteSnapshot</a> operation.</p>

#### `DeleteSnapshotResult`

``` purescript
newtype DeleteSnapshotResult
  = DeleteSnapshotResult { "SnapshotId" :: NullOrUndefined (SnapshotId) }
```

<p>Contains the results of the <a>DeleteSnapshot</a> operation.</p>

#### `DeleteTrustRequest`

``` purescript
newtype DeleteTrustRequest
  = DeleteTrustRequest { "TrustId" :: TrustId, "DeleteAssociatedConditionalForwarder" :: NullOrUndefined (DeleteAssociatedConditionalForwarder) }
```

<p>Deletes the local side of an existing trust relationship between the Microsoft AD in the AWS cloud and the external domain.</p>

#### `DeleteTrustResult`

``` purescript
newtype DeleteTrustResult
  = DeleteTrustResult { "TrustId" :: NullOrUndefined (TrustId) }
```

<p>The result of a DeleteTrust request.</p>

#### `DeregisterEventTopicRequest`

``` purescript
newtype DeregisterEventTopicRequest
  = DeregisterEventTopicRequest { "DirectoryId" :: DirectoryId, "TopicName" :: TopicName }
```

<p>Removes the specified directory as a publisher to the specified SNS topic.</p>

#### `DeregisterEventTopicResult`

``` purescript
newtype DeregisterEventTopicResult
  = DeregisterEventTopicResult {  }
```

<p>The result of a DeregisterEventTopic request.</p>

#### `DescribeConditionalForwardersRequest`

``` purescript
newtype DescribeConditionalForwardersRequest
  = DescribeConditionalForwardersRequest { "DirectoryId" :: DirectoryId, "RemoteDomainNames" :: NullOrUndefined (RemoteDomainNames) }
```

<p>Describes a conditional forwarder.</p>

#### `DescribeConditionalForwardersResult`

``` purescript
newtype DescribeConditionalForwardersResult
  = DescribeConditionalForwardersResult { "ConditionalForwarders" :: NullOrUndefined (ConditionalForwarders) }
```

<p>The result of a DescribeConditionalForwarder request.</p>

#### `DescribeDirectoriesRequest`

``` purescript
newtype DescribeDirectoriesRequest
  = DescribeDirectoriesRequest { "DirectoryIds" :: NullOrUndefined (DirectoryIds), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

<p>Contains the inputs for the <a>DescribeDirectories</a> operation.</p>

#### `DescribeDirectoriesResult`

``` purescript
newtype DescribeDirectoriesResult
  = DescribeDirectoriesResult { "DirectoryDescriptions" :: NullOrUndefined (DirectoryDescriptions), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Contains the results of the <a>DescribeDirectories</a> operation.</p>

#### `DescribeDomainControllersRequest`

``` purescript
newtype DescribeDomainControllersRequest
  = DescribeDomainControllersRequest { "DirectoryId" :: DirectoryId, "DomainControllerIds" :: NullOrUndefined (DomainControllerIds), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

#### `DescribeDomainControllersResult`

``` purescript
newtype DescribeDomainControllersResult
  = DescribeDomainControllersResult { "DomainControllers" :: NullOrUndefined (DomainControllers), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `DescribeEventTopicsRequest`

``` purescript
newtype DescribeEventTopicsRequest
  = DescribeEventTopicsRequest { "DirectoryId" :: NullOrUndefined (DirectoryId), "TopicNames" :: NullOrUndefined (TopicNames) }
```

<p>Describes event topics.</p>

#### `DescribeEventTopicsResult`

``` purescript
newtype DescribeEventTopicsResult
  = DescribeEventTopicsResult { "EventTopics" :: NullOrUndefined (EventTopics) }
```

<p>The result of a DescribeEventTopic request.</p>

#### `DescribeSnapshotsRequest`

``` purescript
newtype DescribeSnapshotsRequest
  = DescribeSnapshotsRequest { "DirectoryId" :: NullOrUndefined (DirectoryId), "SnapshotIds" :: NullOrUndefined (SnapshotIds), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

<p>Contains the inputs for the <a>DescribeSnapshots</a> operation.</p>

#### `DescribeSnapshotsResult`

``` purescript
newtype DescribeSnapshotsResult
  = DescribeSnapshotsResult { "Snapshots" :: NullOrUndefined (Snapshots), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Contains the results of the <a>DescribeSnapshots</a> operation.</p>

#### `DescribeTrustsRequest`

``` purescript
newtype DescribeTrustsRequest
  = DescribeTrustsRequest { "DirectoryId" :: NullOrUndefined (DirectoryId), "TrustIds" :: NullOrUndefined (TrustIds), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

<p>Describes the trust relationships for a particular Microsoft AD in the AWS cloud. If no input parameters are are provided, such as directory ID or trust ID, this request describes all the trust relationships.</p>

#### `DescribeTrustsResult`

``` purescript
newtype DescribeTrustsResult
  = DescribeTrustsResult { "Trusts" :: NullOrUndefined (Trusts), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The result of a DescribeTrust request.</p>

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `DesiredNumberOfDomainControllers`

``` purescript
newtype DesiredNumberOfDomainControllers
  = DesiredNumberOfDomainControllers Int
```

#### `DirectoryConnectSettings`

``` purescript
newtype DirectoryConnectSettings
  = DirectoryConnectSettings { "VpcId" :: VpcId, "SubnetIds" :: SubnetIds, "CustomerDnsIps" :: DnsIpAddrs, "CustomerUserName" :: UserName }
```

<p>Contains information for the <a>ConnectDirectory</a> operation when an AD Connector directory is being created.</p>

#### `DirectoryConnectSettingsDescription`

``` purescript
newtype DirectoryConnectSettingsDescription
  = DirectoryConnectSettingsDescription { "VpcId" :: NullOrUndefined (VpcId), "SubnetIds" :: NullOrUndefined (SubnetIds), "CustomerUserName" :: NullOrUndefined (UserName), "SecurityGroupId" :: NullOrUndefined (SecurityGroupId), "AvailabilityZones" :: NullOrUndefined (AvailabilityZones), "ConnectIps" :: NullOrUndefined (IpAddrs) }
```

<p>Contains information about an AD Connector directory.</p>

#### `DirectoryDescription`

``` purescript
newtype DirectoryDescription
  = DirectoryDescription { "DirectoryId" :: NullOrUndefined (DirectoryId), "Name" :: NullOrUndefined (DirectoryName), "ShortName" :: NullOrUndefined (DirectoryShortName), "Size" :: NullOrUndefined (DirectorySize), "Edition" :: NullOrUndefined (DirectoryEdition), "Alias" :: NullOrUndefined (AliasName), "AccessUrl" :: NullOrUndefined (AccessUrl), "Description" :: NullOrUndefined (Description), "DnsIpAddrs" :: NullOrUndefined (DnsIpAddrs), "Stage" :: NullOrUndefined (DirectoryStage), "LaunchTime" :: NullOrUndefined (LaunchTime), "StageLastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime), "Type" :: NullOrUndefined (DirectoryType), "VpcSettings" :: NullOrUndefined (DirectoryVpcSettingsDescription), "ConnectSettings" :: NullOrUndefined (DirectoryConnectSettingsDescription), "RadiusSettings" :: NullOrUndefined (RadiusSettings), "RadiusStatus" :: NullOrUndefined (RadiusStatus), "StageReason" :: NullOrUndefined (StageReason), "SsoEnabled" :: NullOrUndefined (SsoEnabled), "DesiredNumberOfDomainControllers" :: NullOrUndefined (DesiredNumberOfDomainControllers) }
```

<p>Contains information about an AWS Directory Service directory.</p>

#### `DirectoryDescriptions`

``` purescript
newtype DirectoryDescriptions
  = DirectoryDescriptions (Array DirectoryDescription)
```

<p>A list of directory descriptions.</p>

#### `DirectoryEdition`

``` purescript
newtype DirectoryEdition
  = DirectoryEdition String
```

#### `DirectoryId`

``` purescript
newtype DirectoryId
  = DirectoryId String
```

#### `DirectoryIds`

``` purescript
newtype DirectoryIds
  = DirectoryIds (Array DirectoryId)
```

<p>A list of directory identifiers.</p>

#### `DirectoryLimitExceededException`

``` purescript
newtype DirectoryLimitExceededException
  = DirectoryLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum number of directories in the region has been reached. You can use the <a>GetDirectoryLimits</a> operation to determine your directory limits in the region.</p>

#### `DirectoryLimits`

``` purescript
newtype DirectoryLimits
  = DirectoryLimits { "CloudOnlyDirectoriesLimit" :: NullOrUndefined (Limit), "CloudOnlyDirectoriesCurrentCount" :: NullOrUndefined (Limit), "CloudOnlyDirectoriesLimitReached" :: NullOrUndefined (CloudOnlyDirectoriesLimitReached), "CloudOnlyMicrosoftADLimit" :: NullOrUndefined (Limit), "CloudOnlyMicrosoftADCurrentCount" :: NullOrUndefined (Limit), "CloudOnlyMicrosoftADLimitReached" :: NullOrUndefined (CloudOnlyDirectoriesLimitReached), "ConnectedDirectoriesLimit" :: NullOrUndefined (Limit), "ConnectedDirectoriesCurrentCount" :: NullOrUndefined (Limit), "ConnectedDirectoriesLimitReached" :: NullOrUndefined (ConnectedDirectoriesLimitReached) }
```

<p>Contains directory limit information for a region.</p>

#### `DirectoryName`

``` purescript
newtype DirectoryName
  = DirectoryName String
```

#### `DirectoryShortName`

``` purescript
newtype DirectoryShortName
  = DirectoryShortName String
```

#### `DirectorySize`

``` purescript
newtype DirectorySize
  = DirectorySize String
```

#### `DirectoryStage`

``` purescript
newtype DirectoryStage
  = DirectoryStage String
```

#### `DirectoryType`

``` purescript
newtype DirectoryType
  = DirectoryType String
```

#### `DirectoryUnavailableException`

``` purescript
newtype DirectoryUnavailableException
  = DirectoryUnavailableException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The specified directory is unavailable or could not be found.</p>

#### `DirectoryVpcSettings`

``` purescript
newtype DirectoryVpcSettings
  = DirectoryVpcSettings { "VpcId" :: VpcId, "SubnetIds" :: SubnetIds }
```

<p>Contains VPC information for the <a>CreateDirectory</a> or <a>CreateMicrosoftAD</a> operation.</p>

#### `DirectoryVpcSettingsDescription`

``` purescript
newtype DirectoryVpcSettingsDescription
  = DirectoryVpcSettingsDescription { "VpcId" :: NullOrUndefined (VpcId), "SubnetIds" :: NullOrUndefined (SubnetIds), "SecurityGroupId" :: NullOrUndefined (SecurityGroupId), "AvailabilityZones" :: NullOrUndefined (AvailabilityZones) }
```

<p>Contains information about the directory.</p>

#### `DisableRadiusRequest`

``` purescript
newtype DisableRadiusRequest
  = DisableRadiusRequest { "DirectoryId" :: DirectoryId }
```

<p>Contains the inputs for the <a>DisableRadius</a> operation.</p>

#### `DisableRadiusResult`

``` purescript
newtype DisableRadiusResult
  = DisableRadiusResult {  }
```

<p>Contains the results of the <a>DisableRadius</a> operation.</p>

#### `DisableSsoRequest`

``` purescript
newtype DisableSsoRequest
  = DisableSsoRequest { "DirectoryId" :: DirectoryId, "UserName" :: NullOrUndefined (UserName), "Password" :: NullOrUndefined (ConnectPassword) }
```

<p>Contains the inputs for the <a>DisableSso</a> operation.</p>

#### `DisableSsoResult`

``` purescript
newtype DisableSsoResult
  = DisableSsoResult {  }
```

<p>Contains the results of the <a>DisableSso</a> operation.</p>

#### `DnsIpAddrs`

``` purescript
newtype DnsIpAddrs
  = DnsIpAddrs (Array IpAddr)
```

#### `DomainController`

``` purescript
newtype DomainController
  = DomainController { "DirectoryId" :: NullOrUndefined (DirectoryId), "DomainControllerId" :: NullOrUndefined (DomainControllerId), "DnsIpAddr" :: NullOrUndefined (IpAddr), "VpcId" :: NullOrUndefined (VpcId), "SubnetId" :: NullOrUndefined (SubnetId), "AvailabilityZone" :: NullOrUndefined (AvailabilityZone), "Status" :: NullOrUndefined (DomainControllerStatus), "StatusReason" :: NullOrUndefined (DomainControllerStatusReason), "LaunchTime" :: NullOrUndefined (LaunchTime), "StatusLastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime) }
```

<p>Contains information about the domain controllers for a specified directory.</p>

#### `DomainControllerId`

``` purescript
newtype DomainControllerId
  = DomainControllerId String
```

#### `DomainControllerIds`

``` purescript
newtype DomainControllerIds
  = DomainControllerIds (Array DomainControllerId)
```

#### `DomainControllerLimitExceededException`

``` purescript
newtype DomainControllerLimitExceededException
  = DomainControllerLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum allowed number of domain controllers per directory was exceeded. The default limit per directory is 20 domain controllers.</p>

#### `DomainControllerStatus`

``` purescript
newtype DomainControllerStatus
  = DomainControllerStatus String
```

#### `DomainControllerStatusReason`

``` purescript
newtype DomainControllerStatusReason
  = DomainControllerStatusReason String
```

#### `DomainControllers`

``` purescript
newtype DomainControllers
  = DomainControllers (Array DomainController)
```

#### `EnableRadiusRequest`

``` purescript
newtype EnableRadiusRequest
  = EnableRadiusRequest { "DirectoryId" :: DirectoryId, "RadiusSettings" :: RadiusSettings }
```

<p>Contains the inputs for the <a>EnableRadius</a> operation.</p>

#### `EnableRadiusResult`

``` purescript
newtype EnableRadiusResult
  = EnableRadiusResult {  }
```

<p>Contains the results of the <a>EnableRadius</a> operation.</p>

#### `EnableSsoRequest`

``` purescript
newtype EnableSsoRequest
  = EnableSsoRequest { "DirectoryId" :: DirectoryId, "UserName" :: NullOrUndefined (UserName), "Password" :: NullOrUndefined (ConnectPassword) }
```

<p>Contains the inputs for the <a>EnableSso</a> operation.</p>

#### `EnableSsoResult`

``` purescript
newtype EnableSsoResult
  = EnableSsoResult {  }
```

<p>Contains the results of the <a>EnableSso</a> operation.</p>

#### `EndDateTime`

``` purescript
newtype EndDateTime
  = EndDateTime Number
```

#### `EntityAlreadyExistsException`

``` purescript
newtype EntityAlreadyExistsException
  = EntityAlreadyExistsException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The specified entity already exists.</p>

#### `EntityDoesNotExistException`

``` purescript
newtype EntityDoesNotExistException
  = EntityDoesNotExistException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The specified entity could not be found.</p>

#### `EventTopic`

``` purescript
newtype EventTopic
  = EventTopic { "DirectoryId" :: NullOrUndefined (DirectoryId), "TopicName" :: NullOrUndefined (TopicName), "TopicArn" :: NullOrUndefined (TopicArn), "CreatedDateTime" :: NullOrUndefined (CreatedDateTime), "Status" :: NullOrUndefined (TopicStatus) }
```

<p>Information about SNS topic and AWS Directory Service directory associations.</p>

#### `EventTopics`

``` purescript
newtype EventTopics
  = EventTopics (Array EventTopic)
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

<p>The descriptive message for the exception.</p>

#### `GetDirectoryLimitsRequest`

``` purescript
newtype GetDirectoryLimitsRequest
  = GetDirectoryLimitsRequest {  }
```

<p>Contains the inputs for the <a>GetDirectoryLimits</a> operation.</p>

#### `GetDirectoryLimitsResult`

``` purescript
newtype GetDirectoryLimitsResult
  = GetDirectoryLimitsResult { "DirectoryLimits" :: NullOrUndefined (DirectoryLimits) }
```

<p>Contains the results of the <a>GetDirectoryLimits</a> operation.</p>

#### `GetSnapshotLimitsRequest`

``` purescript
newtype GetSnapshotLimitsRequest
  = GetSnapshotLimitsRequest { "DirectoryId" :: DirectoryId }
```

<p>Contains the inputs for the <a>GetSnapshotLimits</a> operation.</p>

#### `GetSnapshotLimitsResult`

``` purescript
newtype GetSnapshotLimitsResult
  = GetSnapshotLimitsResult { "SnapshotLimits" :: NullOrUndefined (SnapshotLimits) }
```

<p>Contains the results of the <a>GetSnapshotLimits</a> operation.</p>

#### `InsufficientPermissionsException`

``` purescript
newtype InsufficientPermissionsException
  = InsufficientPermissionsException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The account does not have sufficient permission to perform the operation.</p>

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The <i>NextToken</i> value is not valid.</p>

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>One or more parameters are not valid.</p>

#### `IpAddr`

``` purescript
newtype IpAddr
  = IpAddr String
```

#### `IpAddrs`

``` purescript
newtype IpAddrs
  = IpAddrs (Array IpAddr)
```

#### `IpRoute`

``` purescript
newtype IpRoute
  = IpRoute { "CidrIp" :: NullOrUndefined (CidrIp), "Description" :: NullOrUndefined (Description) }
```

<p>IP address block. This is often the address block of the DNS server used for your on-premises domain. </p>

#### `IpRouteInfo`

``` purescript
newtype IpRouteInfo
  = IpRouteInfo { "DirectoryId" :: NullOrUndefined (DirectoryId), "CidrIp" :: NullOrUndefined (CidrIp), "IpRouteStatusMsg" :: NullOrUndefined (IpRouteStatusMsg), "AddedDateTime" :: NullOrUndefined (AddedDateTime), "IpRouteStatusReason" :: NullOrUndefined (IpRouteStatusReason), "Description" :: NullOrUndefined (Description) }
```

<p>Information about one or more IP address blocks.</p>

#### `IpRouteLimitExceededException`

``` purescript
newtype IpRouteLimitExceededException
  = IpRouteLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum allowed number of IP addresses was exceeded. The default limit is 100 IP address blocks.</p>

#### `IpRouteStatusMsg`

``` purescript
newtype IpRouteStatusMsg
  = IpRouteStatusMsg String
```

#### `IpRouteStatusReason`

``` purescript
newtype IpRouteStatusReason
  = IpRouteStatusReason String
```

#### `IpRoutes`

``` purescript
newtype IpRoutes
  = IpRoutes (Array IpRoute)
```

#### `IpRoutesInfo`

``` purescript
newtype IpRoutesInfo
  = IpRoutesInfo (Array IpRouteInfo)
```

#### `LastUpdatedDateTime`

``` purescript
newtype LastUpdatedDateTime
  = LastUpdatedDateTime Number
```

#### `LaunchTime`

``` purescript
newtype LaunchTime
  = LaunchTime Number
```

#### `LdifContent`

``` purescript
newtype LdifContent
  = LdifContent String
```

#### `Limit`

``` purescript
newtype Limit
  = Limit Int
```

#### `ListIpRoutesRequest`

``` purescript
newtype ListIpRoutesRequest
  = ListIpRoutesRequest { "DirectoryId" :: DirectoryId, "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

#### `ListIpRoutesResult`

``` purescript
newtype ListIpRoutesResult
  = ListIpRoutesResult { "IpRoutesInfo" :: NullOrUndefined (IpRoutesInfo), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListSchemaExtensionsRequest`

``` purescript
newtype ListSchemaExtensionsRequest
  = ListSchemaExtensionsRequest { "DirectoryId" :: DirectoryId, "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

#### `ListSchemaExtensionsResult`

``` purescript
newtype ListSchemaExtensionsResult
  = ListSchemaExtensionsResult { "SchemaExtensionsInfo" :: NullOrUndefined (SchemaExtensionsInfo), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceId" :: ResourceId, "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

#### `ListTagsForResourceResult`

``` purescript
newtype ListTagsForResourceResult
  = ListTagsForResourceResult { "Tags" :: NullOrUndefined (Tags), "NextToken" :: NullOrUndefined (NextToken) }
```

#### `ManualSnapshotsLimitReached`

``` purescript
newtype ManualSnapshotsLimitReached
  = ManualSnapshotsLimitReached Boolean
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

#### `OrganizationalUnitDN`

``` purescript
newtype OrganizationalUnitDN
  = OrganizationalUnitDN String
```

#### `Password`

``` purescript
newtype Password
  = Password String
```

#### `PortNumber`

``` purescript
newtype PortNumber
  = PortNumber Int
```

#### `RadiusAuthenticationProtocol`

``` purescript
newtype RadiusAuthenticationProtocol
  = RadiusAuthenticationProtocol String
```

#### `RadiusDisplayLabel`

``` purescript
newtype RadiusDisplayLabel
  = RadiusDisplayLabel String
```

#### `RadiusRetries`

``` purescript
newtype RadiusRetries
  = RadiusRetries Int
```

#### `RadiusSettings`

``` purescript
newtype RadiusSettings
  = RadiusSettings { "RadiusServers" :: NullOrUndefined (Servers), "RadiusPort" :: NullOrUndefined (PortNumber), "RadiusTimeout" :: NullOrUndefined (RadiusTimeout), "RadiusRetries" :: NullOrUndefined (RadiusRetries), "SharedSecret" :: NullOrUndefined (RadiusSharedSecret), "AuthenticationProtocol" :: NullOrUndefined (RadiusAuthenticationProtocol), "DisplayLabel" :: NullOrUndefined (RadiusDisplayLabel), "UseSameUsername" :: NullOrUndefined (UseSameUsername) }
```

<p>Contains information about a Remote Authentication Dial In User Service (RADIUS) server.</p>

#### `RadiusSharedSecret`

``` purescript
newtype RadiusSharedSecret
  = RadiusSharedSecret String
```

#### `RadiusStatus`

``` purescript
newtype RadiusStatus
  = RadiusStatus String
```

#### `RadiusTimeout`

``` purescript
newtype RadiusTimeout
  = RadiusTimeout Int
```

#### `RegisterEventTopicRequest`

``` purescript
newtype RegisterEventTopicRequest
  = RegisterEventTopicRequest { "DirectoryId" :: DirectoryId, "TopicName" :: TopicName }
```

<p>Registers a new event topic.</p>

#### `RegisterEventTopicResult`

``` purescript
newtype RegisterEventTopicResult
  = RegisterEventTopicResult {  }
```

<p>The result of a RegisterEventTopic request.</p>

#### `RemoteDomainName`

``` purescript
newtype RemoteDomainName
  = RemoteDomainName String
```

#### `RemoteDomainNames`

``` purescript
newtype RemoteDomainNames
  = RemoteDomainNames (Array RemoteDomainName)
```

#### `RemoveIpRoutesRequest`

``` purescript
newtype RemoveIpRoutesRequest
  = RemoveIpRoutesRequest { "DirectoryId" :: DirectoryId, "CidrIps" :: CidrIps }
```

#### `RemoveIpRoutesResult`

``` purescript
newtype RemoveIpRoutesResult
  = RemoveIpRoutesResult {  }
```

#### `RemoveTagsFromResourceRequest`

``` purescript
newtype RemoveTagsFromResourceRequest
  = RemoveTagsFromResourceRequest { "ResourceId" :: ResourceId, "TagKeys" :: TagKeys }
```

#### `RemoveTagsFromResourceResult`

``` purescript
newtype RemoveTagsFromResourceResult
  = RemoveTagsFromResourceResult {  }
```

#### `ReplicationScope`

``` purescript
newtype ReplicationScope
  = ReplicationScope String
```

#### `RequestId`

``` purescript
newtype RequestId
  = RequestId String
```

<p>The AWS request identifier.</p>

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

#### `RestoreFromSnapshotRequest`

``` purescript
newtype RestoreFromSnapshotRequest
  = RestoreFromSnapshotRequest { "SnapshotId" :: SnapshotId }
```

<p>An object representing the inputs for the <a>RestoreFromSnapshot</a> operation.</p>

#### `RestoreFromSnapshotResult`

``` purescript
newtype RestoreFromSnapshotResult
  = RestoreFromSnapshotResult {  }
```

<p>Contains the results of the <a>RestoreFromSnapshot</a> operation.</p>

#### `SID`

``` purescript
newtype SID
  = SID String
```

#### `SchemaExtensionId`

``` purescript
newtype SchemaExtensionId
  = SchemaExtensionId String
```

#### `SchemaExtensionInfo`

``` purescript
newtype SchemaExtensionInfo
  = SchemaExtensionInfo { "DirectoryId" :: NullOrUndefined (DirectoryId), "SchemaExtensionId" :: NullOrUndefined (SchemaExtensionId), "Description" :: NullOrUndefined (Description), "SchemaExtensionStatus" :: NullOrUndefined (SchemaExtensionStatus), "SchemaExtensionStatusReason" :: NullOrUndefined (SchemaExtensionStatusReason), "StartDateTime" :: NullOrUndefined (StartDateTime), "EndDateTime" :: NullOrUndefined (EndDateTime) }
```

<p>Information about a schema extension.</p>

#### `SchemaExtensionStatus`

``` purescript
newtype SchemaExtensionStatus
  = SchemaExtensionStatus String
```

#### `SchemaExtensionStatusReason`

``` purescript
newtype SchemaExtensionStatusReason
  = SchemaExtensionStatusReason String
```

#### `SchemaExtensionsInfo`

``` purescript
newtype SchemaExtensionsInfo
  = SchemaExtensionsInfo (Array SchemaExtensionInfo)
```

#### `SecurityGroupId`

``` purescript
newtype SecurityGroupId
  = SecurityGroupId String
```

#### `Server`

``` purescript
newtype Server
  = Server String
```

#### `Servers`

``` purescript
newtype Servers
  = Servers (Array Server)
```

#### `ServiceException`

``` purescript
newtype ServiceException
  = ServiceException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>An exception has occurred in AWS Directory Service.</p>

#### `Snapshot`

``` purescript
newtype Snapshot
  = Snapshot { "DirectoryId" :: NullOrUndefined (DirectoryId), "SnapshotId" :: NullOrUndefined (SnapshotId), "Type" :: NullOrUndefined (SnapshotType), "Name" :: NullOrUndefined (SnapshotName), "Status" :: NullOrUndefined (SnapshotStatus), "StartTime" :: NullOrUndefined (StartTime) }
```

<p>Describes a directory snapshot.</p>

#### `SnapshotId`

``` purescript
newtype SnapshotId
  = SnapshotId String
```

#### `SnapshotIds`

``` purescript
newtype SnapshotIds
  = SnapshotIds (Array SnapshotId)
```

<p>A list of directory snapshot identifiers.</p>

#### `SnapshotLimitExceededException`

``` purescript
newtype SnapshotLimitExceededException
  = SnapshotLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum number of manual snapshots for the directory has been reached. You can use the <a>GetSnapshotLimits</a> operation to determine the snapshot limits for a directory.</p>

#### `SnapshotLimits`

``` purescript
newtype SnapshotLimits
  = SnapshotLimits { "ManualSnapshotsLimit" :: NullOrUndefined (Limit), "ManualSnapshotsCurrentCount" :: NullOrUndefined (Limit), "ManualSnapshotsLimitReached" :: NullOrUndefined (ManualSnapshotsLimitReached) }
```

<p>Contains manual snapshot limit information for a directory.</p>

#### `SnapshotName`

``` purescript
newtype SnapshotName
  = SnapshotName String
```

#### `SnapshotStatus`

``` purescript
newtype SnapshotStatus
  = SnapshotStatus String
```

#### `SnapshotType`

``` purescript
newtype SnapshotType
  = SnapshotType String
```

#### `Snapshots`

``` purescript
newtype Snapshots
  = Snapshots (Array Snapshot)
```

<p>A list of descriptions of directory snapshots.</p>

#### `SsoEnabled`

``` purescript
newtype SsoEnabled
  = SsoEnabled Boolean
```

#### `StageReason`

``` purescript
newtype StageReason
  = StageReason String
```

#### `StartDateTime`

``` purescript
newtype StartDateTime
  = StartDateTime Number
```

#### `StartSchemaExtensionRequest`

``` purescript
newtype StartSchemaExtensionRequest
  = StartSchemaExtensionRequest { "DirectoryId" :: DirectoryId, "CreateSnapshotBeforeSchemaExtension" :: CreateSnapshotBeforeSchemaExtension, "LdifContent" :: LdifContent, "Description" :: Description }
```

#### `StartSchemaExtensionResult`

``` purescript
newtype StartSchemaExtensionResult
  = StartSchemaExtensionResult { "SchemaExtensionId" :: NullOrUndefined (SchemaExtensionId) }
```

#### `StartTime`

``` purescript
newtype StartTime
  = StartTime Number
```

#### `StateLastUpdatedDateTime`

``` purescript
newtype StateLastUpdatedDateTime
  = StateLastUpdatedDateTime Number
```

#### `SubnetId`

``` purescript
newtype SubnetId
  = SubnetId String
```

#### `SubnetIds`

``` purescript
newtype SubnetIds
  = SubnetIds (Array SubnetId)
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>Metadata assigned to a directory consisting of a key-value pair.</p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys (Array TagKey)
```

#### `TagLimitExceededException`

``` purescript
newtype TagLimitExceededException
  = TagLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum allowed number of tags was exceeded.</p>

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `Tags`

``` purescript
newtype Tags
  = Tags (Array Tag)
```

#### `TopicArn`

``` purescript
newtype TopicArn
  = TopicArn String
```

#### `TopicName`

``` purescript
newtype TopicName
  = TopicName String
```

#### `TopicNames`

``` purescript
newtype TopicNames
  = TopicNames (Array TopicName)
```

#### `TopicStatus`

``` purescript
newtype TopicStatus
  = TopicStatus String
```

#### `Trust`

``` purescript
newtype Trust
  = Trust { "DirectoryId" :: NullOrUndefined (DirectoryId), "TrustId" :: NullOrUndefined (TrustId), "RemoteDomainName" :: NullOrUndefined (RemoteDomainName), "TrustType" :: NullOrUndefined (TrustType), "TrustDirection" :: NullOrUndefined (TrustDirection), "TrustState" :: NullOrUndefined (TrustState), "CreatedDateTime" :: NullOrUndefined (CreatedDateTime), "LastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime), "StateLastUpdatedDateTime" :: NullOrUndefined (StateLastUpdatedDateTime), "TrustStateReason" :: NullOrUndefined (TrustStateReason) }
```

<p>Describes a trust relationship between an Microsoft AD in the AWS cloud and an external domain.</p>

#### `TrustDirection`

``` purescript
newtype TrustDirection
  = TrustDirection String
```

#### `TrustId`

``` purescript
newtype TrustId
  = TrustId String
```

#### `TrustIds`

``` purescript
newtype TrustIds
  = TrustIds (Array TrustId)
```

#### `TrustPassword`

``` purescript
newtype TrustPassword
  = TrustPassword String
```

#### `TrustState`

``` purescript
newtype TrustState
  = TrustState String
```

#### `TrustStateReason`

``` purescript
newtype TrustStateReason
  = TrustStateReason String
```

#### `TrustType`

``` purescript
newtype TrustType
  = TrustType String
```

#### `Trusts`

``` purescript
newtype Trusts
  = Trusts (Array Trust)
```

#### `UnsupportedOperationException`

``` purescript
newtype UnsupportedOperationException
  = UnsupportedOperationException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The operation is not supported.</p>

#### `UpdateConditionalForwarderRequest`

``` purescript
newtype UpdateConditionalForwarderRequest
  = UpdateConditionalForwarderRequest { "DirectoryId" :: DirectoryId, "RemoteDomainName" :: RemoteDomainName, "DnsIpAddrs" :: DnsIpAddrs }
```

<p>Updates a conditional forwarder.</p>

#### `UpdateConditionalForwarderResult`

``` purescript
newtype UpdateConditionalForwarderResult
  = UpdateConditionalForwarderResult {  }
```

<p>The result of an UpdateConditionalForwarder request.</p>

#### `UpdateNumberOfDomainControllersRequest`

``` purescript
newtype UpdateNumberOfDomainControllersRequest
  = UpdateNumberOfDomainControllersRequest { "DirectoryId" :: DirectoryId, "DesiredNumber" :: DesiredNumberOfDomainControllers }
```

#### `UpdateNumberOfDomainControllersResult`

``` purescript
newtype UpdateNumberOfDomainControllersResult
  = UpdateNumberOfDomainControllersResult {  }
```

#### `UpdateRadiusRequest`

``` purescript
newtype UpdateRadiusRequest
  = UpdateRadiusRequest { "DirectoryId" :: DirectoryId, "RadiusSettings" :: RadiusSettings }
```

<p>Contains the inputs for the <a>UpdateRadius</a> operation.</p>

#### `UpdateRadiusResult`

``` purescript
newtype UpdateRadiusResult
  = UpdateRadiusResult {  }
```

<p>Contains the results of the <a>UpdateRadius</a> operation.</p>

#### `UpdateSecurityGroupForDirectoryControllers`

``` purescript
newtype UpdateSecurityGroupForDirectoryControllers
  = UpdateSecurityGroupForDirectoryControllers Boolean
```

#### `UseSameUsername`

``` purescript
newtype UseSameUsername
  = UseSameUsername Boolean
```

#### `UserName`

``` purescript
newtype UserName
  = UserName String
```

#### `VerifyTrustRequest`

``` purescript
newtype VerifyTrustRequest
  = VerifyTrustRequest { "TrustId" :: TrustId }
```

<p>Initiates the verification of an existing trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>

#### `VerifyTrustResult`

``` purescript
newtype VerifyTrustResult
  = VerifyTrustResult { "TrustId" :: NullOrUndefined (TrustId) }
```

<p>Result of a VerifyTrust request.</p>

#### `VpcId`

``` purescript
newtype VpcId
  = VpcId String
```


