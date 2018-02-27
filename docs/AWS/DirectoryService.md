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

##### Instances
``` purescript
Newtype AccessUrl _
```

#### `AddIpRoutesRequest`

``` purescript
newtype AddIpRoutesRequest
  = AddIpRoutesRequest { "DirectoryId" :: DirectoryId, "IpRoutes" :: IpRoutes, "UpdateSecurityGroupForDirectoryControllers" :: NullOrUndefined (UpdateSecurityGroupForDirectoryControllers) }
```

##### Instances
``` purescript
Newtype AddIpRoutesRequest _
```

#### `AddIpRoutesResult`

``` purescript
newtype AddIpRoutesResult
  = AddIpRoutesResult {  }
```

##### Instances
``` purescript
Newtype AddIpRoutesResult _
```

#### `AddTagsToResourceRequest`

``` purescript
newtype AddTagsToResourceRequest
  = AddTagsToResourceRequest { "ResourceId" :: ResourceId, "Tags" :: Tags }
```

##### Instances
``` purescript
Newtype AddTagsToResourceRequest _
```

#### `AddTagsToResourceResult`

``` purescript
newtype AddTagsToResourceResult
  = AddTagsToResourceResult {  }
```

##### Instances
``` purescript
Newtype AddTagsToResourceResult _
```

#### `AddedDateTime`

``` purescript
newtype AddedDateTime
  = AddedDateTime Number
```

##### Instances
``` purescript
Newtype AddedDateTime _
```

#### `AliasName`

``` purescript
newtype AliasName
  = AliasName String
```

##### Instances
``` purescript
Newtype AliasName _
```

#### `Attribute`

``` purescript
newtype Attribute
  = Attribute { "Name" :: NullOrUndefined (AttributeName), "Value" :: NullOrUndefined (AttributeValue) }
```

<p>Represents a named directory attribute.</p>

##### Instances
``` purescript
Newtype Attribute _
```

#### `AttributeName`

``` purescript
newtype AttributeName
  = AttributeName String
```

##### Instances
``` purescript
Newtype AttributeName _
```

#### `AttributeValue`

``` purescript
newtype AttributeValue
  = AttributeValue String
```

##### Instances
``` purescript
Newtype AttributeValue _
```

#### `Attributes`

``` purescript
newtype Attributes
  = Attributes (Array Attribute)
```

##### Instances
``` purescript
Newtype Attributes _
```

#### `AuthenticationFailedException`

``` purescript
newtype AuthenticationFailedException
  = AuthenticationFailedException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>An authentication error occurred.</p>

##### Instances
``` purescript
Newtype AuthenticationFailedException _
```

#### `AvailabilityZone`

``` purescript
newtype AvailabilityZone
  = AvailabilityZone String
```

##### Instances
``` purescript
Newtype AvailabilityZone _
```

#### `AvailabilityZones`

``` purescript
newtype AvailabilityZones
  = AvailabilityZones (Array AvailabilityZone)
```

##### Instances
``` purescript
Newtype AvailabilityZones _
```

#### `CancelSchemaExtensionRequest`

``` purescript
newtype CancelSchemaExtensionRequest
  = CancelSchemaExtensionRequest { "DirectoryId" :: DirectoryId, "SchemaExtensionId" :: SchemaExtensionId }
```

##### Instances
``` purescript
Newtype CancelSchemaExtensionRequest _
```

#### `CancelSchemaExtensionResult`

``` purescript
newtype CancelSchemaExtensionResult
  = CancelSchemaExtensionResult {  }
```

##### Instances
``` purescript
Newtype CancelSchemaExtensionResult _
```

#### `CidrIp`

``` purescript
newtype CidrIp
  = CidrIp String
```

##### Instances
``` purescript
Newtype CidrIp _
```

#### `CidrIps`

``` purescript
newtype CidrIps
  = CidrIps (Array CidrIp)
```

##### Instances
``` purescript
Newtype CidrIps _
```

#### `ClientException`

``` purescript
newtype ClientException
  = ClientException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>A client exception has occurred.</p>

##### Instances
``` purescript
Newtype ClientException _
```

#### `CloudOnlyDirectoriesLimitReached`

``` purescript
newtype CloudOnlyDirectoriesLimitReached
  = CloudOnlyDirectoriesLimitReached Boolean
```

##### Instances
``` purescript
Newtype CloudOnlyDirectoriesLimitReached _
```

#### `Computer`

``` purescript
newtype Computer
  = Computer { "ComputerId" :: NullOrUndefined (SID), "ComputerName" :: NullOrUndefined (ComputerName), "ComputerAttributes" :: NullOrUndefined (Attributes) }
```

<p>Contains information about a computer account in a directory.</p>

##### Instances
``` purescript
Newtype Computer _
```

#### `ComputerName`

``` purescript
newtype ComputerName
  = ComputerName String
```

##### Instances
``` purescript
Newtype ComputerName _
```

#### `ComputerPassword`

``` purescript
newtype ComputerPassword
  = ComputerPassword String
```

##### Instances
``` purescript
Newtype ComputerPassword _
```

#### `ConditionalForwarder`

``` purescript
newtype ConditionalForwarder
  = ConditionalForwarder { "RemoteDomainName" :: NullOrUndefined (RemoteDomainName), "DnsIpAddrs" :: NullOrUndefined (DnsIpAddrs), "ReplicationScope" :: NullOrUndefined (ReplicationScope) }
```

<p>Points to a remote domain with which you are setting up a trust relationship. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>

##### Instances
``` purescript
Newtype ConditionalForwarder _
```

#### `ConditionalForwarders`

``` purescript
newtype ConditionalForwarders
  = ConditionalForwarders (Array ConditionalForwarder)
```

##### Instances
``` purescript
Newtype ConditionalForwarders _
```

#### `ConnectDirectoryRequest`

``` purescript
newtype ConnectDirectoryRequest
  = ConnectDirectoryRequest { "Name" :: DirectoryName, "ShortName" :: NullOrUndefined (DirectoryShortName), "Password" :: ConnectPassword, "Description" :: NullOrUndefined (Description), "Size" :: DirectorySize, "ConnectSettings" :: DirectoryConnectSettings }
```

<p>Contains the inputs for the <a>ConnectDirectory</a> operation.</p>

##### Instances
``` purescript
Newtype ConnectDirectoryRequest _
```

#### `ConnectDirectoryResult`

``` purescript
newtype ConnectDirectoryResult
  = ConnectDirectoryResult { "DirectoryId" :: NullOrUndefined (DirectoryId) }
```

<p>Contains the results of the <a>ConnectDirectory</a> operation.</p>

##### Instances
``` purescript
Newtype ConnectDirectoryResult _
```

#### `ConnectPassword`

``` purescript
newtype ConnectPassword
  = ConnectPassword String
```

##### Instances
``` purescript
Newtype ConnectPassword _
```

#### `ConnectedDirectoriesLimitReached`

``` purescript
newtype ConnectedDirectoriesLimitReached
  = ConnectedDirectoriesLimitReached Boolean
```

##### Instances
``` purescript
Newtype ConnectedDirectoriesLimitReached _
```

#### `CreateAliasRequest`

``` purescript
newtype CreateAliasRequest
  = CreateAliasRequest { "DirectoryId" :: DirectoryId, "Alias" :: AliasName }
```

<p>Contains the inputs for the <a>CreateAlias</a> operation.</p>

##### Instances
``` purescript
Newtype CreateAliasRequest _
```

#### `CreateAliasResult`

``` purescript
newtype CreateAliasResult
  = CreateAliasResult { "DirectoryId" :: NullOrUndefined (DirectoryId), "Alias" :: NullOrUndefined (AliasName) }
```

<p>Contains the results of the <a>CreateAlias</a> operation.</p>

##### Instances
``` purescript
Newtype CreateAliasResult _
```

#### `CreateComputerRequest`

``` purescript
newtype CreateComputerRequest
  = CreateComputerRequest { "DirectoryId" :: DirectoryId, "ComputerName" :: ComputerName, "Password" :: ComputerPassword, "OrganizationalUnitDistinguishedName" :: NullOrUndefined (OrganizationalUnitDN), "ComputerAttributes" :: NullOrUndefined (Attributes) }
```

<p>Contains the inputs for the <a>CreateComputer</a> operation.</p>

##### Instances
``` purescript
Newtype CreateComputerRequest _
```

#### `CreateComputerResult`

``` purescript
newtype CreateComputerResult
  = CreateComputerResult { "Computer" :: NullOrUndefined (Computer) }
```

<p>Contains the results for the <a>CreateComputer</a> operation.</p>

##### Instances
``` purescript
Newtype CreateComputerResult _
```

#### `CreateConditionalForwarderRequest`

``` purescript
newtype CreateConditionalForwarderRequest
  = CreateConditionalForwarderRequest { "DirectoryId" :: DirectoryId, "RemoteDomainName" :: RemoteDomainName, "DnsIpAddrs" :: DnsIpAddrs }
```

<p>Initiates the creation of a conditional forwarder for your AWS Directory Service for Microsoft Active Directory. Conditional forwarders are required in order to set up a trust relationship with another domain.</p>

##### Instances
``` purescript
Newtype CreateConditionalForwarderRequest _
```

#### `CreateConditionalForwarderResult`

``` purescript
newtype CreateConditionalForwarderResult
  = CreateConditionalForwarderResult {  }
```

<p>The result of a CreateConditinalForwarder request.</p>

##### Instances
``` purescript
Newtype CreateConditionalForwarderResult _
```

#### `CreateDirectoryRequest`

``` purescript
newtype CreateDirectoryRequest
  = CreateDirectoryRequest { "Name" :: DirectoryName, "ShortName" :: NullOrUndefined (DirectoryShortName), "Password" :: Password, "Description" :: NullOrUndefined (Description), "Size" :: DirectorySize, "VpcSettings" :: NullOrUndefined (DirectoryVpcSettings) }
```

<p>Contains the inputs for the <a>CreateDirectory</a> operation. </p>

##### Instances
``` purescript
Newtype CreateDirectoryRequest _
```

#### `CreateDirectoryResult`

``` purescript
newtype CreateDirectoryResult
  = CreateDirectoryResult { "DirectoryId" :: NullOrUndefined (DirectoryId) }
```

<p>Contains the results of the <a>CreateDirectory</a> operation.</p>

##### Instances
``` purescript
Newtype CreateDirectoryResult _
```

#### `CreateMicrosoftADRequest`

``` purescript
newtype CreateMicrosoftADRequest
  = CreateMicrosoftADRequest { "Name" :: DirectoryName, "ShortName" :: NullOrUndefined (DirectoryShortName), "Password" :: Password, "Description" :: NullOrUndefined (Description), "VpcSettings" :: DirectoryVpcSettings, "Edition" :: NullOrUndefined (DirectoryEdition) }
```

<p>Creates a Microsoft AD in the AWS cloud.</p>

##### Instances
``` purescript
Newtype CreateMicrosoftADRequest _
```

#### `CreateMicrosoftADResult`

``` purescript
newtype CreateMicrosoftADResult
  = CreateMicrosoftADResult { "DirectoryId" :: NullOrUndefined (DirectoryId) }
```

<p>Result of a CreateMicrosoftAD request.</p>

##### Instances
``` purescript
Newtype CreateMicrosoftADResult _
```

#### `CreateSnapshotBeforeSchemaExtension`

``` purescript
newtype CreateSnapshotBeforeSchemaExtension
  = CreateSnapshotBeforeSchemaExtension Boolean
```

##### Instances
``` purescript
Newtype CreateSnapshotBeforeSchemaExtension _
```

#### `CreateSnapshotRequest`

``` purescript
newtype CreateSnapshotRequest
  = CreateSnapshotRequest { "DirectoryId" :: DirectoryId, "Name" :: NullOrUndefined (SnapshotName) }
```

<p>Contains the inputs for the <a>CreateSnapshot</a> operation.</p>

##### Instances
``` purescript
Newtype CreateSnapshotRequest _
```

#### `CreateSnapshotResult`

``` purescript
newtype CreateSnapshotResult
  = CreateSnapshotResult { "SnapshotId" :: NullOrUndefined (SnapshotId) }
```

<p>Contains the results of the <a>CreateSnapshot</a> operation.</p>

##### Instances
``` purescript
Newtype CreateSnapshotResult _
```

#### `CreateTrustRequest`

``` purescript
newtype CreateTrustRequest
  = CreateTrustRequest { "DirectoryId" :: DirectoryId, "RemoteDomainName" :: RemoteDomainName, "TrustPassword" :: TrustPassword, "TrustDirection" :: TrustDirection, "TrustType" :: NullOrUndefined (TrustType), "ConditionalForwarderIpAddrs" :: NullOrUndefined (DnsIpAddrs) }
```

<p>AWS Directory Service for Microsoft Active Directory allows you to configure trust relationships. For example, you can establish a trust between your Microsoft AD in the AWS cloud, and your existing on-premises Microsoft Active Directory. This would allow you to provide users and groups access to resources in either domain, with a single set of credentials.</p> <p>This action initiates the creation of the AWS side of a trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>

##### Instances
``` purescript
Newtype CreateTrustRequest _
```

#### `CreateTrustResult`

``` purescript
newtype CreateTrustResult
  = CreateTrustResult { "TrustId" :: NullOrUndefined (TrustId) }
```

<p>The result of a CreateTrust request.</p>

##### Instances
``` purescript
Newtype CreateTrustResult _
```

#### `CreatedDateTime`

``` purescript
newtype CreatedDateTime
  = CreatedDateTime Number
```

##### Instances
``` purescript
Newtype CreatedDateTime _
```

#### `DeleteAssociatedConditionalForwarder`

``` purescript
newtype DeleteAssociatedConditionalForwarder
  = DeleteAssociatedConditionalForwarder Boolean
```

##### Instances
``` purescript
Newtype DeleteAssociatedConditionalForwarder _
```

#### `DeleteConditionalForwarderRequest`

``` purescript
newtype DeleteConditionalForwarderRequest
  = DeleteConditionalForwarderRequest { "DirectoryId" :: DirectoryId, "RemoteDomainName" :: RemoteDomainName }
```

<p>Deletes a conditional forwarder.</p>

##### Instances
``` purescript
Newtype DeleteConditionalForwarderRequest _
```

#### `DeleteConditionalForwarderResult`

``` purescript
newtype DeleteConditionalForwarderResult
  = DeleteConditionalForwarderResult {  }
```

<p>The result of a DeleteConditionalForwarder request.</p>

##### Instances
``` purescript
Newtype DeleteConditionalForwarderResult _
```

#### `DeleteDirectoryRequest`

``` purescript
newtype DeleteDirectoryRequest
  = DeleteDirectoryRequest { "DirectoryId" :: DirectoryId }
```

<p>Contains the inputs for the <a>DeleteDirectory</a> operation.</p>

##### Instances
``` purescript
Newtype DeleteDirectoryRequest _
```

#### `DeleteDirectoryResult`

``` purescript
newtype DeleteDirectoryResult
  = DeleteDirectoryResult { "DirectoryId" :: NullOrUndefined (DirectoryId) }
```

<p>Contains the results of the <a>DeleteDirectory</a> operation.</p>

##### Instances
``` purescript
Newtype DeleteDirectoryResult _
```

#### `DeleteSnapshotRequest`

``` purescript
newtype DeleteSnapshotRequest
  = DeleteSnapshotRequest { "SnapshotId" :: SnapshotId }
```

<p>Contains the inputs for the <a>DeleteSnapshot</a> operation.</p>

##### Instances
``` purescript
Newtype DeleteSnapshotRequest _
```

#### `DeleteSnapshotResult`

``` purescript
newtype DeleteSnapshotResult
  = DeleteSnapshotResult { "SnapshotId" :: NullOrUndefined (SnapshotId) }
```

<p>Contains the results of the <a>DeleteSnapshot</a> operation.</p>

##### Instances
``` purescript
Newtype DeleteSnapshotResult _
```

#### `DeleteTrustRequest`

``` purescript
newtype DeleteTrustRequest
  = DeleteTrustRequest { "TrustId" :: TrustId, "DeleteAssociatedConditionalForwarder" :: NullOrUndefined (DeleteAssociatedConditionalForwarder) }
```

<p>Deletes the local side of an existing trust relationship between the Microsoft AD in the AWS cloud and the external domain.</p>

##### Instances
``` purescript
Newtype DeleteTrustRequest _
```

#### `DeleteTrustResult`

``` purescript
newtype DeleteTrustResult
  = DeleteTrustResult { "TrustId" :: NullOrUndefined (TrustId) }
```

<p>The result of a DeleteTrust request.</p>

##### Instances
``` purescript
Newtype DeleteTrustResult _
```

#### `DeregisterEventTopicRequest`

``` purescript
newtype DeregisterEventTopicRequest
  = DeregisterEventTopicRequest { "DirectoryId" :: DirectoryId, "TopicName" :: TopicName }
```

<p>Removes the specified directory as a publisher to the specified SNS topic.</p>

##### Instances
``` purescript
Newtype DeregisterEventTopicRequest _
```

#### `DeregisterEventTopicResult`

``` purescript
newtype DeregisterEventTopicResult
  = DeregisterEventTopicResult {  }
```

<p>The result of a DeregisterEventTopic request.</p>

##### Instances
``` purescript
Newtype DeregisterEventTopicResult _
```

#### `DescribeConditionalForwardersRequest`

``` purescript
newtype DescribeConditionalForwardersRequest
  = DescribeConditionalForwardersRequest { "DirectoryId" :: DirectoryId, "RemoteDomainNames" :: NullOrUndefined (RemoteDomainNames) }
```

<p>Describes a conditional forwarder.</p>

##### Instances
``` purescript
Newtype DescribeConditionalForwardersRequest _
```

#### `DescribeConditionalForwardersResult`

``` purescript
newtype DescribeConditionalForwardersResult
  = DescribeConditionalForwardersResult { "ConditionalForwarders" :: NullOrUndefined (ConditionalForwarders) }
```

<p>The result of a DescribeConditionalForwarder request.</p>

##### Instances
``` purescript
Newtype DescribeConditionalForwardersResult _
```

#### `DescribeDirectoriesRequest`

``` purescript
newtype DescribeDirectoriesRequest
  = DescribeDirectoriesRequest { "DirectoryIds" :: NullOrUndefined (DirectoryIds), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

<p>Contains the inputs for the <a>DescribeDirectories</a> operation.</p>

##### Instances
``` purescript
Newtype DescribeDirectoriesRequest _
```

#### `DescribeDirectoriesResult`

``` purescript
newtype DescribeDirectoriesResult
  = DescribeDirectoriesResult { "DirectoryDescriptions" :: NullOrUndefined (DirectoryDescriptions), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Contains the results of the <a>DescribeDirectories</a> operation.</p>

##### Instances
``` purescript
Newtype DescribeDirectoriesResult _
```

#### `DescribeDomainControllersRequest`

``` purescript
newtype DescribeDomainControllersRequest
  = DescribeDomainControllersRequest { "DirectoryId" :: DirectoryId, "DomainControllerIds" :: NullOrUndefined (DomainControllerIds), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

##### Instances
``` purescript
Newtype DescribeDomainControllersRequest _
```

#### `DescribeDomainControllersResult`

``` purescript
newtype DescribeDomainControllersResult
  = DescribeDomainControllersResult { "DomainControllers" :: NullOrUndefined (DomainControllers), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype DescribeDomainControllersResult _
```

#### `DescribeEventTopicsRequest`

``` purescript
newtype DescribeEventTopicsRequest
  = DescribeEventTopicsRequest { "DirectoryId" :: NullOrUndefined (DirectoryId), "TopicNames" :: NullOrUndefined (TopicNames) }
```

<p>Describes event topics.</p>

##### Instances
``` purescript
Newtype DescribeEventTopicsRequest _
```

#### `DescribeEventTopicsResult`

``` purescript
newtype DescribeEventTopicsResult
  = DescribeEventTopicsResult { "EventTopics" :: NullOrUndefined (EventTopics) }
```

<p>The result of a DescribeEventTopic request.</p>

##### Instances
``` purescript
Newtype DescribeEventTopicsResult _
```

#### `DescribeSnapshotsRequest`

``` purescript
newtype DescribeSnapshotsRequest
  = DescribeSnapshotsRequest { "DirectoryId" :: NullOrUndefined (DirectoryId), "SnapshotIds" :: NullOrUndefined (SnapshotIds), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

<p>Contains the inputs for the <a>DescribeSnapshots</a> operation.</p>

##### Instances
``` purescript
Newtype DescribeSnapshotsRequest _
```

#### `DescribeSnapshotsResult`

``` purescript
newtype DescribeSnapshotsResult
  = DescribeSnapshotsResult { "Snapshots" :: NullOrUndefined (Snapshots), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>Contains the results of the <a>DescribeSnapshots</a> operation.</p>

##### Instances
``` purescript
Newtype DescribeSnapshotsResult _
```

#### `DescribeTrustsRequest`

``` purescript
newtype DescribeTrustsRequest
  = DescribeTrustsRequest { "DirectoryId" :: NullOrUndefined (DirectoryId), "TrustIds" :: NullOrUndefined (TrustIds), "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

<p>Describes the trust relationships for a particular Microsoft AD in the AWS cloud. If no input parameters are are provided, such as directory ID or trust ID, this request describes all the trust relationships.</p>

##### Instances
``` purescript
Newtype DescribeTrustsRequest _
```

#### `DescribeTrustsResult`

``` purescript
newtype DescribeTrustsResult
  = DescribeTrustsResult { "Trusts" :: NullOrUndefined (Trusts), "NextToken" :: NullOrUndefined (NextToken) }
```

<p>The result of a DescribeTrust request.</p>

##### Instances
``` purescript
Newtype DescribeTrustsResult _
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

#### `DesiredNumberOfDomainControllers`

``` purescript
newtype DesiredNumberOfDomainControllers
  = DesiredNumberOfDomainControllers Int
```

##### Instances
``` purescript
Newtype DesiredNumberOfDomainControllers _
```

#### `DirectoryConnectSettings`

``` purescript
newtype DirectoryConnectSettings
  = DirectoryConnectSettings { "VpcId" :: VpcId, "SubnetIds" :: SubnetIds, "CustomerDnsIps" :: DnsIpAddrs, "CustomerUserName" :: UserName }
```

<p>Contains information for the <a>ConnectDirectory</a> operation when an AD Connector directory is being created.</p>

##### Instances
``` purescript
Newtype DirectoryConnectSettings _
```

#### `DirectoryConnectSettingsDescription`

``` purescript
newtype DirectoryConnectSettingsDescription
  = DirectoryConnectSettingsDescription { "VpcId" :: NullOrUndefined (VpcId), "SubnetIds" :: NullOrUndefined (SubnetIds), "CustomerUserName" :: NullOrUndefined (UserName), "SecurityGroupId" :: NullOrUndefined (SecurityGroupId), "AvailabilityZones" :: NullOrUndefined (AvailabilityZones), "ConnectIps" :: NullOrUndefined (IpAddrs) }
```

<p>Contains information about an AD Connector directory.</p>

##### Instances
``` purescript
Newtype DirectoryConnectSettingsDescription _
```

#### `DirectoryDescription`

``` purescript
newtype DirectoryDescription
  = DirectoryDescription { "DirectoryId" :: NullOrUndefined (DirectoryId), "Name" :: NullOrUndefined (DirectoryName), "ShortName" :: NullOrUndefined (DirectoryShortName), "Size" :: NullOrUndefined (DirectorySize), "Edition" :: NullOrUndefined (DirectoryEdition), "Alias" :: NullOrUndefined (AliasName), "AccessUrl" :: NullOrUndefined (AccessUrl), "Description" :: NullOrUndefined (Description), "DnsIpAddrs" :: NullOrUndefined (DnsIpAddrs), "Stage" :: NullOrUndefined (DirectoryStage), "LaunchTime" :: NullOrUndefined (LaunchTime), "StageLastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime), "Type" :: NullOrUndefined (DirectoryType), "VpcSettings" :: NullOrUndefined (DirectoryVpcSettingsDescription), "ConnectSettings" :: NullOrUndefined (DirectoryConnectSettingsDescription), "RadiusSettings" :: NullOrUndefined (RadiusSettings), "RadiusStatus" :: NullOrUndefined (RadiusStatus), "StageReason" :: NullOrUndefined (StageReason), "SsoEnabled" :: NullOrUndefined (SsoEnabled), "DesiredNumberOfDomainControllers" :: NullOrUndefined (DesiredNumberOfDomainControllers) }
```

<p>Contains information about an AWS Directory Service directory.</p>

##### Instances
``` purescript
Newtype DirectoryDescription _
```

#### `DirectoryDescriptions`

``` purescript
newtype DirectoryDescriptions
  = DirectoryDescriptions (Array DirectoryDescription)
```

<p>A list of directory descriptions.</p>

##### Instances
``` purescript
Newtype DirectoryDescriptions _
```

#### `DirectoryEdition`

``` purescript
newtype DirectoryEdition
  = DirectoryEdition String
```

##### Instances
``` purescript
Newtype DirectoryEdition _
```

#### `DirectoryId`

``` purescript
newtype DirectoryId
  = DirectoryId String
```

##### Instances
``` purescript
Newtype DirectoryId _
```

#### `DirectoryIds`

``` purescript
newtype DirectoryIds
  = DirectoryIds (Array DirectoryId)
```

<p>A list of directory identifiers.</p>

##### Instances
``` purescript
Newtype DirectoryIds _
```

#### `DirectoryLimitExceededException`

``` purescript
newtype DirectoryLimitExceededException
  = DirectoryLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum number of directories in the region has been reached. You can use the <a>GetDirectoryLimits</a> operation to determine your directory limits in the region.</p>

##### Instances
``` purescript
Newtype DirectoryLimitExceededException _
```

#### `DirectoryLimits`

``` purescript
newtype DirectoryLimits
  = DirectoryLimits { "CloudOnlyDirectoriesLimit" :: NullOrUndefined (Limit), "CloudOnlyDirectoriesCurrentCount" :: NullOrUndefined (Limit), "CloudOnlyDirectoriesLimitReached" :: NullOrUndefined (CloudOnlyDirectoriesLimitReached), "CloudOnlyMicrosoftADLimit" :: NullOrUndefined (Limit), "CloudOnlyMicrosoftADCurrentCount" :: NullOrUndefined (Limit), "CloudOnlyMicrosoftADLimitReached" :: NullOrUndefined (CloudOnlyDirectoriesLimitReached), "ConnectedDirectoriesLimit" :: NullOrUndefined (Limit), "ConnectedDirectoriesCurrentCount" :: NullOrUndefined (Limit), "ConnectedDirectoriesLimitReached" :: NullOrUndefined (ConnectedDirectoriesLimitReached) }
```

<p>Contains directory limit information for a region.</p>

##### Instances
``` purescript
Newtype DirectoryLimits _
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

#### `DirectoryShortName`

``` purescript
newtype DirectoryShortName
  = DirectoryShortName String
```

##### Instances
``` purescript
Newtype DirectoryShortName _
```

#### `DirectorySize`

``` purescript
newtype DirectorySize
  = DirectorySize String
```

##### Instances
``` purescript
Newtype DirectorySize _
```

#### `DirectoryStage`

``` purescript
newtype DirectoryStage
  = DirectoryStage String
```

##### Instances
``` purescript
Newtype DirectoryStage _
```

#### `DirectoryType`

``` purescript
newtype DirectoryType
  = DirectoryType String
```

##### Instances
``` purescript
Newtype DirectoryType _
```

#### `DirectoryUnavailableException`

``` purescript
newtype DirectoryUnavailableException
  = DirectoryUnavailableException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The specified directory is unavailable or could not be found.</p>

##### Instances
``` purescript
Newtype DirectoryUnavailableException _
```

#### `DirectoryVpcSettings`

``` purescript
newtype DirectoryVpcSettings
  = DirectoryVpcSettings { "VpcId" :: VpcId, "SubnetIds" :: SubnetIds }
```

<p>Contains VPC information for the <a>CreateDirectory</a> or <a>CreateMicrosoftAD</a> operation.</p>

##### Instances
``` purescript
Newtype DirectoryVpcSettings _
```

#### `DirectoryVpcSettingsDescription`

``` purescript
newtype DirectoryVpcSettingsDescription
  = DirectoryVpcSettingsDescription { "VpcId" :: NullOrUndefined (VpcId), "SubnetIds" :: NullOrUndefined (SubnetIds), "SecurityGroupId" :: NullOrUndefined (SecurityGroupId), "AvailabilityZones" :: NullOrUndefined (AvailabilityZones) }
```

<p>Contains information about the directory.</p>

##### Instances
``` purescript
Newtype DirectoryVpcSettingsDescription _
```

#### `DisableRadiusRequest`

``` purescript
newtype DisableRadiusRequest
  = DisableRadiusRequest { "DirectoryId" :: DirectoryId }
```

<p>Contains the inputs for the <a>DisableRadius</a> operation.</p>

##### Instances
``` purescript
Newtype DisableRadiusRequest _
```

#### `DisableRadiusResult`

``` purescript
newtype DisableRadiusResult
  = DisableRadiusResult {  }
```

<p>Contains the results of the <a>DisableRadius</a> operation.</p>

##### Instances
``` purescript
Newtype DisableRadiusResult _
```

#### `DisableSsoRequest`

``` purescript
newtype DisableSsoRequest
  = DisableSsoRequest { "DirectoryId" :: DirectoryId, "UserName" :: NullOrUndefined (UserName), "Password" :: NullOrUndefined (ConnectPassword) }
```

<p>Contains the inputs for the <a>DisableSso</a> operation.</p>

##### Instances
``` purescript
Newtype DisableSsoRequest _
```

#### `DisableSsoResult`

``` purescript
newtype DisableSsoResult
  = DisableSsoResult {  }
```

<p>Contains the results of the <a>DisableSso</a> operation.</p>

##### Instances
``` purescript
Newtype DisableSsoResult _
```

#### `DnsIpAddrs`

``` purescript
newtype DnsIpAddrs
  = DnsIpAddrs (Array IpAddr)
```

##### Instances
``` purescript
Newtype DnsIpAddrs _
```

#### `DomainController`

``` purescript
newtype DomainController
  = DomainController { "DirectoryId" :: NullOrUndefined (DirectoryId), "DomainControllerId" :: NullOrUndefined (DomainControllerId), "DnsIpAddr" :: NullOrUndefined (IpAddr), "VpcId" :: NullOrUndefined (VpcId), "SubnetId" :: NullOrUndefined (SubnetId), "AvailabilityZone" :: NullOrUndefined (AvailabilityZone), "Status" :: NullOrUndefined (DomainControllerStatus), "StatusReason" :: NullOrUndefined (DomainControllerStatusReason), "LaunchTime" :: NullOrUndefined (LaunchTime), "StatusLastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime) }
```

<p>Contains information about the domain controllers for a specified directory.</p>

##### Instances
``` purescript
Newtype DomainController _
```

#### `DomainControllerId`

``` purescript
newtype DomainControllerId
  = DomainControllerId String
```

##### Instances
``` purescript
Newtype DomainControllerId _
```

#### `DomainControllerIds`

``` purescript
newtype DomainControllerIds
  = DomainControllerIds (Array DomainControllerId)
```

##### Instances
``` purescript
Newtype DomainControllerIds _
```

#### `DomainControllerLimitExceededException`

``` purescript
newtype DomainControllerLimitExceededException
  = DomainControllerLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum allowed number of domain controllers per directory was exceeded. The default limit per directory is 20 domain controllers.</p>

##### Instances
``` purescript
Newtype DomainControllerLimitExceededException _
```

#### `DomainControllerStatus`

``` purescript
newtype DomainControllerStatus
  = DomainControllerStatus String
```

##### Instances
``` purescript
Newtype DomainControllerStatus _
```

#### `DomainControllerStatusReason`

``` purescript
newtype DomainControllerStatusReason
  = DomainControllerStatusReason String
```

##### Instances
``` purescript
Newtype DomainControllerStatusReason _
```

#### `DomainControllers`

``` purescript
newtype DomainControllers
  = DomainControllers (Array DomainController)
```

##### Instances
``` purescript
Newtype DomainControllers _
```

#### `EnableRadiusRequest`

``` purescript
newtype EnableRadiusRequest
  = EnableRadiusRequest { "DirectoryId" :: DirectoryId, "RadiusSettings" :: RadiusSettings }
```

<p>Contains the inputs for the <a>EnableRadius</a> operation.</p>

##### Instances
``` purescript
Newtype EnableRadiusRequest _
```

#### `EnableRadiusResult`

``` purescript
newtype EnableRadiusResult
  = EnableRadiusResult {  }
```

<p>Contains the results of the <a>EnableRadius</a> operation.</p>

##### Instances
``` purescript
Newtype EnableRadiusResult _
```

#### `EnableSsoRequest`

``` purescript
newtype EnableSsoRequest
  = EnableSsoRequest { "DirectoryId" :: DirectoryId, "UserName" :: NullOrUndefined (UserName), "Password" :: NullOrUndefined (ConnectPassword) }
```

<p>Contains the inputs for the <a>EnableSso</a> operation.</p>

##### Instances
``` purescript
Newtype EnableSsoRequest _
```

#### `EnableSsoResult`

``` purescript
newtype EnableSsoResult
  = EnableSsoResult {  }
```

<p>Contains the results of the <a>EnableSso</a> operation.</p>

##### Instances
``` purescript
Newtype EnableSsoResult _
```

#### `EndDateTime`

``` purescript
newtype EndDateTime
  = EndDateTime Number
```

##### Instances
``` purescript
Newtype EndDateTime _
```

#### `EntityAlreadyExistsException`

``` purescript
newtype EntityAlreadyExistsException
  = EntityAlreadyExistsException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The specified entity already exists.</p>

##### Instances
``` purescript
Newtype EntityAlreadyExistsException _
```

#### `EntityDoesNotExistException`

``` purescript
newtype EntityDoesNotExistException
  = EntityDoesNotExistException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The specified entity could not be found.</p>

##### Instances
``` purescript
Newtype EntityDoesNotExistException _
```

#### `EventTopic`

``` purescript
newtype EventTopic
  = EventTopic { "DirectoryId" :: NullOrUndefined (DirectoryId), "TopicName" :: NullOrUndefined (TopicName), "TopicArn" :: NullOrUndefined (TopicArn), "CreatedDateTime" :: NullOrUndefined (CreatedDateTime), "Status" :: NullOrUndefined (TopicStatus) }
```

<p>Information about SNS topic and AWS Directory Service directory associations.</p>

##### Instances
``` purescript
Newtype EventTopic _
```

#### `EventTopics`

``` purescript
newtype EventTopics
  = EventTopics (Array EventTopic)
```

##### Instances
``` purescript
Newtype EventTopics _
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

<p>The descriptive message for the exception.</p>

##### Instances
``` purescript
Newtype ExceptionMessage _
```

#### `GetDirectoryLimitsRequest`

``` purescript
newtype GetDirectoryLimitsRequest
  = GetDirectoryLimitsRequest {  }
```

<p>Contains the inputs for the <a>GetDirectoryLimits</a> operation.</p>

##### Instances
``` purescript
Newtype GetDirectoryLimitsRequest _
```

#### `GetDirectoryLimitsResult`

``` purescript
newtype GetDirectoryLimitsResult
  = GetDirectoryLimitsResult { "DirectoryLimits" :: NullOrUndefined (DirectoryLimits) }
```

<p>Contains the results of the <a>GetDirectoryLimits</a> operation.</p>

##### Instances
``` purescript
Newtype GetDirectoryLimitsResult _
```

#### `GetSnapshotLimitsRequest`

``` purescript
newtype GetSnapshotLimitsRequest
  = GetSnapshotLimitsRequest { "DirectoryId" :: DirectoryId }
```

<p>Contains the inputs for the <a>GetSnapshotLimits</a> operation.</p>

##### Instances
``` purescript
Newtype GetSnapshotLimitsRequest _
```

#### `GetSnapshotLimitsResult`

``` purescript
newtype GetSnapshotLimitsResult
  = GetSnapshotLimitsResult { "SnapshotLimits" :: NullOrUndefined (SnapshotLimits) }
```

<p>Contains the results of the <a>GetSnapshotLimits</a> operation.</p>

##### Instances
``` purescript
Newtype GetSnapshotLimitsResult _
```

#### `InsufficientPermissionsException`

``` purescript
newtype InsufficientPermissionsException
  = InsufficientPermissionsException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The account does not have sufficient permission to perform the operation.</p>

##### Instances
``` purescript
Newtype InsufficientPermissionsException _
```

#### `InvalidNextTokenException`

``` purescript
newtype InvalidNextTokenException
  = InvalidNextTokenException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The <i>NextToken</i> value is not valid.</p>

##### Instances
``` purescript
Newtype InvalidNextTokenException _
```

#### `InvalidParameterException`

``` purescript
newtype InvalidParameterException
  = InvalidParameterException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>One or more parameters are not valid.</p>

##### Instances
``` purescript
Newtype InvalidParameterException _
```

#### `IpAddr`

``` purescript
newtype IpAddr
  = IpAddr String
```

##### Instances
``` purescript
Newtype IpAddr _
```

#### `IpAddrs`

``` purescript
newtype IpAddrs
  = IpAddrs (Array IpAddr)
```

##### Instances
``` purescript
Newtype IpAddrs _
```

#### `IpRoute`

``` purescript
newtype IpRoute
  = IpRoute { "CidrIp" :: NullOrUndefined (CidrIp), "Description" :: NullOrUndefined (Description) }
```

<p>IP address block. This is often the address block of the DNS server used for your on-premises domain. </p>

##### Instances
``` purescript
Newtype IpRoute _
```

#### `IpRouteInfo`

``` purescript
newtype IpRouteInfo
  = IpRouteInfo { "DirectoryId" :: NullOrUndefined (DirectoryId), "CidrIp" :: NullOrUndefined (CidrIp), "IpRouteStatusMsg" :: NullOrUndefined (IpRouteStatusMsg), "AddedDateTime" :: NullOrUndefined (AddedDateTime), "IpRouteStatusReason" :: NullOrUndefined (IpRouteStatusReason), "Description" :: NullOrUndefined (Description) }
```

<p>Information about one or more IP address blocks.</p>

##### Instances
``` purescript
Newtype IpRouteInfo _
```

#### `IpRouteLimitExceededException`

``` purescript
newtype IpRouteLimitExceededException
  = IpRouteLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum allowed number of IP addresses was exceeded. The default limit is 100 IP address blocks.</p>

##### Instances
``` purescript
Newtype IpRouteLimitExceededException _
```

#### `IpRouteStatusMsg`

``` purescript
newtype IpRouteStatusMsg
  = IpRouteStatusMsg String
```

##### Instances
``` purescript
Newtype IpRouteStatusMsg _
```

#### `IpRouteStatusReason`

``` purescript
newtype IpRouteStatusReason
  = IpRouteStatusReason String
```

##### Instances
``` purescript
Newtype IpRouteStatusReason _
```

#### `IpRoutes`

``` purescript
newtype IpRoutes
  = IpRoutes (Array IpRoute)
```

##### Instances
``` purescript
Newtype IpRoutes _
```

#### `IpRoutesInfo`

``` purescript
newtype IpRoutesInfo
  = IpRoutesInfo (Array IpRouteInfo)
```

##### Instances
``` purescript
Newtype IpRoutesInfo _
```

#### `LastUpdatedDateTime`

``` purescript
newtype LastUpdatedDateTime
  = LastUpdatedDateTime Number
```

##### Instances
``` purescript
Newtype LastUpdatedDateTime _
```

#### `LaunchTime`

``` purescript
newtype LaunchTime
  = LaunchTime Number
```

##### Instances
``` purescript
Newtype LaunchTime _
```

#### `LdifContent`

``` purescript
newtype LdifContent
  = LdifContent String
```

##### Instances
``` purescript
Newtype LdifContent _
```

#### `Limit`

``` purescript
newtype Limit
  = Limit Int
```

##### Instances
``` purescript
Newtype Limit _
```

#### `ListIpRoutesRequest`

``` purescript
newtype ListIpRoutesRequest
  = ListIpRoutesRequest { "DirectoryId" :: DirectoryId, "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

##### Instances
``` purescript
Newtype ListIpRoutesRequest _
```

#### `ListIpRoutesResult`

``` purescript
newtype ListIpRoutesResult
  = ListIpRoutesResult { "IpRoutesInfo" :: NullOrUndefined (IpRoutesInfo), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListIpRoutesResult _
```

#### `ListSchemaExtensionsRequest`

``` purescript
newtype ListSchemaExtensionsRequest
  = ListSchemaExtensionsRequest { "DirectoryId" :: DirectoryId, "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

##### Instances
``` purescript
Newtype ListSchemaExtensionsRequest _
```

#### `ListSchemaExtensionsResult`

``` purescript
newtype ListSchemaExtensionsResult
  = ListSchemaExtensionsResult { "SchemaExtensionsInfo" :: NullOrUndefined (SchemaExtensionsInfo), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListSchemaExtensionsResult _
```

#### `ListTagsForResourceRequest`

``` purescript
newtype ListTagsForResourceRequest
  = ListTagsForResourceRequest { "ResourceId" :: ResourceId, "NextToken" :: NullOrUndefined (NextToken), "Limit" :: NullOrUndefined (Limit) }
```

##### Instances
``` purescript
Newtype ListTagsForResourceRequest _
```

#### `ListTagsForResourceResult`

``` purescript
newtype ListTagsForResourceResult
  = ListTagsForResourceResult { "Tags" :: NullOrUndefined (Tags), "NextToken" :: NullOrUndefined (NextToken) }
```

##### Instances
``` purescript
Newtype ListTagsForResourceResult _
```

#### `ManualSnapshotsLimitReached`

``` purescript
newtype ManualSnapshotsLimitReached
  = ManualSnapshotsLimitReached Boolean
```

##### Instances
``` purescript
Newtype ManualSnapshotsLimitReached _
```

#### `NextToken`

``` purescript
newtype NextToken
  = NextToken String
```

##### Instances
``` purescript
Newtype NextToken _
```

#### `OrganizationalUnitDN`

``` purescript
newtype OrganizationalUnitDN
  = OrganizationalUnitDN String
```

##### Instances
``` purescript
Newtype OrganizationalUnitDN _
```

#### `Password`

``` purescript
newtype Password
  = Password String
```

##### Instances
``` purescript
Newtype Password _
```

#### `PortNumber`

``` purescript
newtype PortNumber
  = PortNumber Int
```

##### Instances
``` purescript
Newtype PortNumber _
```

#### `RadiusAuthenticationProtocol`

``` purescript
newtype RadiusAuthenticationProtocol
  = RadiusAuthenticationProtocol String
```

##### Instances
``` purescript
Newtype RadiusAuthenticationProtocol _
```

#### `RadiusDisplayLabel`

``` purescript
newtype RadiusDisplayLabel
  = RadiusDisplayLabel String
```

##### Instances
``` purescript
Newtype RadiusDisplayLabel _
```

#### `RadiusRetries`

``` purescript
newtype RadiusRetries
  = RadiusRetries Int
```

##### Instances
``` purescript
Newtype RadiusRetries _
```

#### `RadiusSettings`

``` purescript
newtype RadiusSettings
  = RadiusSettings { "RadiusServers" :: NullOrUndefined (Servers), "RadiusPort" :: NullOrUndefined (PortNumber), "RadiusTimeout" :: NullOrUndefined (RadiusTimeout), "RadiusRetries" :: NullOrUndefined (RadiusRetries), "SharedSecret" :: NullOrUndefined (RadiusSharedSecret), "AuthenticationProtocol" :: NullOrUndefined (RadiusAuthenticationProtocol), "DisplayLabel" :: NullOrUndefined (RadiusDisplayLabel), "UseSameUsername" :: NullOrUndefined (UseSameUsername) }
```

<p>Contains information about a Remote Authentication Dial In User Service (RADIUS) server.</p>

##### Instances
``` purescript
Newtype RadiusSettings _
```

#### `RadiusSharedSecret`

``` purescript
newtype RadiusSharedSecret
  = RadiusSharedSecret String
```

##### Instances
``` purescript
Newtype RadiusSharedSecret _
```

#### `RadiusStatus`

``` purescript
newtype RadiusStatus
  = RadiusStatus String
```

##### Instances
``` purescript
Newtype RadiusStatus _
```

#### `RadiusTimeout`

``` purescript
newtype RadiusTimeout
  = RadiusTimeout Int
```

##### Instances
``` purescript
Newtype RadiusTimeout _
```

#### `RegisterEventTopicRequest`

``` purescript
newtype RegisterEventTopicRequest
  = RegisterEventTopicRequest { "DirectoryId" :: DirectoryId, "TopicName" :: TopicName }
```

<p>Registers a new event topic.</p>

##### Instances
``` purescript
Newtype RegisterEventTopicRequest _
```

#### `RegisterEventTopicResult`

``` purescript
newtype RegisterEventTopicResult
  = RegisterEventTopicResult {  }
```

<p>The result of a RegisterEventTopic request.</p>

##### Instances
``` purescript
Newtype RegisterEventTopicResult _
```

#### `RemoteDomainName`

``` purescript
newtype RemoteDomainName
  = RemoteDomainName String
```

##### Instances
``` purescript
Newtype RemoteDomainName _
```

#### `RemoteDomainNames`

``` purescript
newtype RemoteDomainNames
  = RemoteDomainNames (Array RemoteDomainName)
```

##### Instances
``` purescript
Newtype RemoteDomainNames _
```

#### `RemoveIpRoutesRequest`

``` purescript
newtype RemoveIpRoutesRequest
  = RemoveIpRoutesRequest { "DirectoryId" :: DirectoryId, "CidrIps" :: CidrIps }
```

##### Instances
``` purescript
Newtype RemoveIpRoutesRequest _
```

#### `RemoveIpRoutesResult`

``` purescript
newtype RemoveIpRoutesResult
  = RemoveIpRoutesResult {  }
```

##### Instances
``` purescript
Newtype RemoveIpRoutesResult _
```

#### `RemoveTagsFromResourceRequest`

``` purescript
newtype RemoveTagsFromResourceRequest
  = RemoveTagsFromResourceRequest { "ResourceId" :: ResourceId, "TagKeys" :: TagKeys }
```

##### Instances
``` purescript
Newtype RemoveTagsFromResourceRequest _
```

#### `RemoveTagsFromResourceResult`

``` purescript
newtype RemoveTagsFromResourceResult
  = RemoveTagsFromResourceResult {  }
```

##### Instances
``` purescript
Newtype RemoveTagsFromResourceResult _
```

#### `ReplicationScope`

``` purescript
newtype ReplicationScope
  = ReplicationScope String
```

##### Instances
``` purescript
Newtype ReplicationScope _
```

#### `RequestId`

``` purescript
newtype RequestId
  = RequestId String
```

<p>The AWS request identifier.</p>

##### Instances
``` purescript
Newtype RequestId _
```

#### `ResourceId`

``` purescript
newtype ResourceId
  = ResourceId String
```

##### Instances
``` purescript
Newtype ResourceId _
```

#### `RestoreFromSnapshotRequest`

``` purescript
newtype RestoreFromSnapshotRequest
  = RestoreFromSnapshotRequest { "SnapshotId" :: SnapshotId }
```

<p>An object representing the inputs for the <a>RestoreFromSnapshot</a> operation.</p>

##### Instances
``` purescript
Newtype RestoreFromSnapshotRequest _
```

#### `RestoreFromSnapshotResult`

``` purescript
newtype RestoreFromSnapshotResult
  = RestoreFromSnapshotResult {  }
```

<p>Contains the results of the <a>RestoreFromSnapshot</a> operation.</p>

##### Instances
``` purescript
Newtype RestoreFromSnapshotResult _
```

#### `SID`

``` purescript
newtype SID
  = SID String
```

##### Instances
``` purescript
Newtype SID _
```

#### `SchemaExtensionId`

``` purescript
newtype SchemaExtensionId
  = SchemaExtensionId String
```

##### Instances
``` purescript
Newtype SchemaExtensionId _
```

#### `SchemaExtensionInfo`

``` purescript
newtype SchemaExtensionInfo
  = SchemaExtensionInfo { "DirectoryId" :: NullOrUndefined (DirectoryId), "SchemaExtensionId" :: NullOrUndefined (SchemaExtensionId), "Description" :: NullOrUndefined (Description), "SchemaExtensionStatus" :: NullOrUndefined (SchemaExtensionStatus), "SchemaExtensionStatusReason" :: NullOrUndefined (SchemaExtensionStatusReason), "StartDateTime" :: NullOrUndefined (StartDateTime), "EndDateTime" :: NullOrUndefined (EndDateTime) }
```

<p>Information about a schema extension.</p>

##### Instances
``` purescript
Newtype SchemaExtensionInfo _
```

#### `SchemaExtensionStatus`

``` purescript
newtype SchemaExtensionStatus
  = SchemaExtensionStatus String
```

##### Instances
``` purescript
Newtype SchemaExtensionStatus _
```

#### `SchemaExtensionStatusReason`

``` purescript
newtype SchemaExtensionStatusReason
  = SchemaExtensionStatusReason String
```

##### Instances
``` purescript
Newtype SchemaExtensionStatusReason _
```

#### `SchemaExtensionsInfo`

``` purescript
newtype SchemaExtensionsInfo
  = SchemaExtensionsInfo (Array SchemaExtensionInfo)
```

##### Instances
``` purescript
Newtype SchemaExtensionsInfo _
```

#### `SecurityGroupId`

``` purescript
newtype SecurityGroupId
  = SecurityGroupId String
```

##### Instances
``` purescript
Newtype SecurityGroupId _
```

#### `Server`

``` purescript
newtype Server
  = Server String
```

##### Instances
``` purescript
Newtype Server _
```

#### `Servers`

``` purescript
newtype Servers
  = Servers (Array Server)
```

##### Instances
``` purescript
Newtype Servers _
```

#### `ServiceException`

``` purescript
newtype ServiceException
  = ServiceException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>An exception has occurred in AWS Directory Service.</p>

##### Instances
``` purescript
Newtype ServiceException _
```

#### `Snapshot`

``` purescript
newtype Snapshot
  = Snapshot { "DirectoryId" :: NullOrUndefined (DirectoryId), "SnapshotId" :: NullOrUndefined (SnapshotId), "Type" :: NullOrUndefined (SnapshotType), "Name" :: NullOrUndefined (SnapshotName), "Status" :: NullOrUndefined (SnapshotStatus), "StartTime" :: NullOrUndefined (StartTime) }
```

<p>Describes a directory snapshot.</p>

##### Instances
``` purescript
Newtype Snapshot _
```

#### `SnapshotId`

``` purescript
newtype SnapshotId
  = SnapshotId String
```

##### Instances
``` purescript
Newtype SnapshotId _
```

#### `SnapshotIds`

``` purescript
newtype SnapshotIds
  = SnapshotIds (Array SnapshotId)
```

<p>A list of directory snapshot identifiers.</p>

##### Instances
``` purescript
Newtype SnapshotIds _
```

#### `SnapshotLimitExceededException`

``` purescript
newtype SnapshotLimitExceededException
  = SnapshotLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum number of manual snapshots for the directory has been reached. You can use the <a>GetSnapshotLimits</a> operation to determine the snapshot limits for a directory.</p>

##### Instances
``` purescript
Newtype SnapshotLimitExceededException _
```

#### `SnapshotLimits`

``` purescript
newtype SnapshotLimits
  = SnapshotLimits { "ManualSnapshotsLimit" :: NullOrUndefined (Limit), "ManualSnapshotsCurrentCount" :: NullOrUndefined (Limit), "ManualSnapshotsLimitReached" :: NullOrUndefined (ManualSnapshotsLimitReached) }
```

<p>Contains manual snapshot limit information for a directory.</p>

##### Instances
``` purescript
Newtype SnapshotLimits _
```

#### `SnapshotName`

``` purescript
newtype SnapshotName
  = SnapshotName String
```

##### Instances
``` purescript
Newtype SnapshotName _
```

#### `SnapshotStatus`

``` purescript
newtype SnapshotStatus
  = SnapshotStatus String
```

##### Instances
``` purescript
Newtype SnapshotStatus _
```

#### `SnapshotType`

``` purescript
newtype SnapshotType
  = SnapshotType String
```

##### Instances
``` purescript
Newtype SnapshotType _
```

#### `Snapshots`

``` purescript
newtype Snapshots
  = Snapshots (Array Snapshot)
```

<p>A list of descriptions of directory snapshots.</p>

##### Instances
``` purescript
Newtype Snapshots _
```

#### `SsoEnabled`

``` purescript
newtype SsoEnabled
  = SsoEnabled Boolean
```

##### Instances
``` purescript
Newtype SsoEnabled _
```

#### `StageReason`

``` purescript
newtype StageReason
  = StageReason String
```

##### Instances
``` purescript
Newtype StageReason _
```

#### `StartDateTime`

``` purescript
newtype StartDateTime
  = StartDateTime Number
```

##### Instances
``` purescript
Newtype StartDateTime _
```

#### `StartSchemaExtensionRequest`

``` purescript
newtype StartSchemaExtensionRequest
  = StartSchemaExtensionRequest { "DirectoryId" :: DirectoryId, "CreateSnapshotBeforeSchemaExtension" :: CreateSnapshotBeforeSchemaExtension, "LdifContent" :: LdifContent, "Description" :: Description }
```

##### Instances
``` purescript
Newtype StartSchemaExtensionRequest _
```

#### `StartSchemaExtensionResult`

``` purescript
newtype StartSchemaExtensionResult
  = StartSchemaExtensionResult { "SchemaExtensionId" :: NullOrUndefined (SchemaExtensionId) }
```

##### Instances
``` purescript
Newtype StartSchemaExtensionResult _
```

#### `StartTime`

``` purescript
newtype StartTime
  = StartTime Number
```

##### Instances
``` purescript
Newtype StartTime _
```

#### `StateLastUpdatedDateTime`

``` purescript
newtype StateLastUpdatedDateTime
  = StateLastUpdatedDateTime Number
```

##### Instances
``` purescript
Newtype StateLastUpdatedDateTime _
```

#### `SubnetId`

``` purescript
newtype SubnetId
  = SubnetId String
```

##### Instances
``` purescript
Newtype SubnetId _
```

#### `SubnetIds`

``` purescript
newtype SubnetIds
  = SubnetIds (Array SubnetId)
```

##### Instances
``` purescript
Newtype SubnetIds _
```

#### `Tag`

``` purescript
newtype Tag
  = Tag { "Key" :: TagKey, "Value" :: TagValue }
```

<p>Metadata assigned to a directory consisting of a key-value pair.</p>

##### Instances
``` purescript
Newtype Tag _
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

#### `TagKeys`

``` purescript
newtype TagKeys
  = TagKeys (Array TagKey)
```

##### Instances
``` purescript
Newtype TagKeys _
```

#### `TagLimitExceededException`

``` purescript
newtype TagLimitExceededException
  = TagLimitExceededException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The maximum allowed number of tags was exceeded.</p>

##### Instances
``` purescript
Newtype TagLimitExceededException _
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
  = Tags (Array Tag)
```

##### Instances
``` purescript
Newtype Tags _
```

#### `TopicArn`

``` purescript
newtype TopicArn
  = TopicArn String
```

##### Instances
``` purescript
Newtype TopicArn _
```

#### `TopicName`

``` purescript
newtype TopicName
  = TopicName String
```

##### Instances
``` purescript
Newtype TopicName _
```

#### `TopicNames`

``` purescript
newtype TopicNames
  = TopicNames (Array TopicName)
```

##### Instances
``` purescript
Newtype TopicNames _
```

#### `TopicStatus`

``` purescript
newtype TopicStatus
  = TopicStatus String
```

##### Instances
``` purescript
Newtype TopicStatus _
```

#### `Trust`

``` purescript
newtype Trust
  = Trust { "DirectoryId" :: NullOrUndefined (DirectoryId), "TrustId" :: NullOrUndefined (TrustId), "RemoteDomainName" :: NullOrUndefined (RemoteDomainName), "TrustType" :: NullOrUndefined (TrustType), "TrustDirection" :: NullOrUndefined (TrustDirection), "TrustState" :: NullOrUndefined (TrustState), "CreatedDateTime" :: NullOrUndefined (CreatedDateTime), "LastUpdatedDateTime" :: NullOrUndefined (LastUpdatedDateTime), "StateLastUpdatedDateTime" :: NullOrUndefined (StateLastUpdatedDateTime), "TrustStateReason" :: NullOrUndefined (TrustStateReason) }
```

<p>Describes a trust relationship between an Microsoft AD in the AWS cloud and an external domain.</p>

##### Instances
``` purescript
Newtype Trust _
```

#### `TrustDirection`

``` purescript
newtype TrustDirection
  = TrustDirection String
```

##### Instances
``` purescript
Newtype TrustDirection _
```

#### `TrustId`

``` purescript
newtype TrustId
  = TrustId String
```

##### Instances
``` purescript
Newtype TrustId _
```

#### `TrustIds`

``` purescript
newtype TrustIds
  = TrustIds (Array TrustId)
```

##### Instances
``` purescript
Newtype TrustIds _
```

#### `TrustPassword`

``` purescript
newtype TrustPassword
  = TrustPassword String
```

##### Instances
``` purescript
Newtype TrustPassword _
```

#### `TrustState`

``` purescript
newtype TrustState
  = TrustState String
```

##### Instances
``` purescript
Newtype TrustState _
```

#### `TrustStateReason`

``` purescript
newtype TrustStateReason
  = TrustStateReason String
```

##### Instances
``` purescript
Newtype TrustStateReason _
```

#### `TrustType`

``` purescript
newtype TrustType
  = TrustType String
```

##### Instances
``` purescript
Newtype TrustType _
```

#### `Trusts`

``` purescript
newtype Trusts
  = Trusts (Array Trust)
```

##### Instances
``` purescript
Newtype Trusts _
```

#### `UnsupportedOperationException`

``` purescript
newtype UnsupportedOperationException
  = UnsupportedOperationException { "Message" :: NullOrUndefined (ExceptionMessage), "RequestId" :: NullOrUndefined (RequestId) }
```

<p>The operation is not supported.</p>

##### Instances
``` purescript
Newtype UnsupportedOperationException _
```

#### `UpdateConditionalForwarderRequest`

``` purescript
newtype UpdateConditionalForwarderRequest
  = UpdateConditionalForwarderRequest { "DirectoryId" :: DirectoryId, "RemoteDomainName" :: RemoteDomainName, "DnsIpAddrs" :: DnsIpAddrs }
```

<p>Updates a conditional forwarder.</p>

##### Instances
``` purescript
Newtype UpdateConditionalForwarderRequest _
```

#### `UpdateConditionalForwarderResult`

``` purescript
newtype UpdateConditionalForwarderResult
  = UpdateConditionalForwarderResult {  }
```

<p>The result of an UpdateConditionalForwarder request.</p>

##### Instances
``` purescript
Newtype UpdateConditionalForwarderResult _
```

#### `UpdateNumberOfDomainControllersRequest`

``` purescript
newtype UpdateNumberOfDomainControllersRequest
  = UpdateNumberOfDomainControllersRequest { "DirectoryId" :: DirectoryId, "DesiredNumber" :: DesiredNumberOfDomainControllers }
```

##### Instances
``` purescript
Newtype UpdateNumberOfDomainControllersRequest _
```

#### `UpdateNumberOfDomainControllersResult`

``` purescript
newtype UpdateNumberOfDomainControllersResult
  = UpdateNumberOfDomainControllersResult {  }
```

##### Instances
``` purescript
Newtype UpdateNumberOfDomainControllersResult _
```

#### `UpdateRadiusRequest`

``` purescript
newtype UpdateRadiusRequest
  = UpdateRadiusRequest { "DirectoryId" :: DirectoryId, "RadiusSettings" :: RadiusSettings }
```

<p>Contains the inputs for the <a>UpdateRadius</a> operation.</p>

##### Instances
``` purescript
Newtype UpdateRadiusRequest _
```

#### `UpdateRadiusResult`

``` purescript
newtype UpdateRadiusResult
  = UpdateRadiusResult {  }
```

<p>Contains the results of the <a>UpdateRadius</a> operation.</p>

##### Instances
``` purescript
Newtype UpdateRadiusResult _
```

#### `UpdateSecurityGroupForDirectoryControllers`

``` purescript
newtype UpdateSecurityGroupForDirectoryControllers
  = UpdateSecurityGroupForDirectoryControllers Boolean
```

##### Instances
``` purescript
Newtype UpdateSecurityGroupForDirectoryControllers _
```

#### `UseSameUsername`

``` purescript
newtype UseSameUsername
  = UseSameUsername Boolean
```

##### Instances
``` purescript
Newtype UseSameUsername _
```

#### `UserName`

``` purescript
newtype UserName
  = UserName String
```

##### Instances
``` purescript
Newtype UserName _
```

#### `VerifyTrustRequest`

``` purescript
newtype VerifyTrustRequest
  = VerifyTrustRequest { "TrustId" :: TrustId }
```

<p>Initiates the verification of an existing trust relationship between a Microsoft AD in the AWS cloud and an external domain.</p>

##### Instances
``` purescript
Newtype VerifyTrustRequest _
```

#### `VerifyTrustResult`

``` purescript
newtype VerifyTrustResult
  = VerifyTrustResult { "TrustId" :: NullOrUndefined (TrustId) }
```

<p>Result of a VerifyTrust request.</p>

##### Instances
``` purescript
Newtype VerifyTrustResult _
```

#### `VpcId`

``` purescript
newtype VpcId
  = VpcId String
```

##### Instances
``` purescript
Newtype VpcId _
```


