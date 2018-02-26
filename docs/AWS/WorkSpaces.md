## Module AWS.WorkSpaces

<fullname>Amazon WorkSpaces Service</fullname> <p>Amazon WorkSpaces enables you to provision virtual, cloud-based Microsoft Windows desktops for your users.</p>

#### `serviceName`

``` purescript
serviceName :: String
```

#### `createTags`

``` purescript
createTags :: forall eff. CreateTagsRequest -> Aff (err :: RequestError | eff) CreateTagsResult
```

<p>Creates tags for the specified WorkSpace.</p>

#### `createWorkspaces`

``` purescript
createWorkspaces :: forall eff. CreateWorkspacesRequest -> Aff (err :: RequestError | eff) CreateWorkspacesResult
```

<p>Creates one or more WorkSpaces.</p> <p>This operation is asynchronous and returns before the WorkSpaces are created.</p>

#### `deleteTags`

``` purescript
deleteTags :: forall eff. DeleteTagsRequest -> Aff (err :: RequestError | eff) DeleteTagsResult
```

<p>Deletes the specified tags from a WorkSpace.</p>

#### `describeTags`

``` purescript
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: RequestError | eff) DescribeTagsResult
```

<p>Describes the tags for the specified WorkSpace.</p>

#### `describeWorkspaceBundles`

``` purescript
describeWorkspaceBundles :: forall eff. DescribeWorkspaceBundlesRequest -> Aff (err :: RequestError | eff) DescribeWorkspaceBundlesResult
```

<p>Describes the available WorkSpace bundles.</p> <p>You can filter the results using either bundle ID or owner, but not both.</p>

#### `describeWorkspaceDirectories`

``` purescript
describeWorkspaceDirectories :: forall eff. DescribeWorkspaceDirectoriesRequest -> Aff (err :: RequestError | eff) DescribeWorkspaceDirectoriesResult
```

<p>Describes the available AWS Directory Service directories that are registered with Amazon WorkSpaces.</p>

#### `describeWorkspaces`

``` purescript
describeWorkspaces :: forall eff. DescribeWorkspacesRequest -> Aff (err :: RequestError | eff) DescribeWorkspacesResult
```

<p>Describes the specified WorkSpaces.</p> <p>You can filter the results using bundle ID, directory ID, or owner, but you can specify only one filter at a time.</p>

#### `describeWorkspacesConnectionStatus`

``` purescript
describeWorkspacesConnectionStatus :: forall eff. DescribeWorkspacesConnectionStatusRequest -> Aff (err :: RequestError | eff) DescribeWorkspacesConnectionStatusResult
```

<p>Describes the connection status of the specified WorkSpaces.</p>

#### `modifyWorkspaceProperties`

``` purescript
modifyWorkspaceProperties :: forall eff. ModifyWorkspacePropertiesRequest -> Aff (err :: RequestError | eff) ModifyWorkspacePropertiesResult
```

<p>Modifies the specified WorkSpace properties.</p>

#### `rebootWorkspaces`

``` purescript
rebootWorkspaces :: forall eff. RebootWorkspacesRequest -> Aff (err :: RequestError | eff) RebootWorkspacesResult
```

<p>Reboots the specified WorkSpaces.</p> <p>You cannot reboot a WorkSpace unless its state is <code>AVAILABLE</code>, <code>IMPAIRED</code>, or <code>INOPERABLE</code>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have rebooted.</p>

#### `rebuildWorkspaces`

``` purescript
rebuildWorkspaces :: forall eff. RebuildWorkspacesRequest -> Aff (err :: RequestError | eff) RebuildWorkspacesResult
```

<p>Rebuilds the specified WorkSpaces.</p> <p>You cannot rebuild a WorkSpace unless its state is <code>AVAILABLE</code> or <code>ERROR</code>.</p> <p>Rebuilding a WorkSpace is a potentially destructive action that can result in the loss of data. For more information, see <a href="http://docs.aws.amazon.com/workspaces/latest/adminguide/reset-workspace.html">Rebuild a WorkSpace</a>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have been completely rebuilt.</p>

#### `startWorkspaces`

``` purescript
startWorkspaces :: forall eff. StartWorkspacesRequest -> Aff (err :: RequestError | eff) StartWorkspacesResult
```

<p>Starts the specified WorkSpaces.</p> <p>You cannot start a WorkSpace unless it has a running mode of <code>AutoStop</code> and a state of <code>STOPPED</code>.</p>

#### `stopWorkspaces`

``` purescript
stopWorkspaces :: forall eff. StopWorkspacesRequest -> Aff (err :: RequestError | eff) StopWorkspacesResult
```

<p> Stops the specified WorkSpaces.</p> <p>You cannot stop a WorkSpace unless it has a running mode of <code>AutoStop</code> and a state of <code>AVAILABLE</code>, <code>IMPAIRED</code>, <code>UNHEALTHY</code>, or <code>ERROR</code>.</p>

#### `terminateWorkspaces`

``` purescript
terminateWorkspaces :: forall eff. TerminateWorkspacesRequest -> Aff (err :: RequestError | eff) TerminateWorkspacesResult
```

<p>Terminates the specified WorkSpaces.</p> <p>Terminating a WorkSpace is a permanent action and cannot be undone. The user's data is destroyed. If you need to archive any user data, contact Amazon Web Services before terminating the WorkSpace.</p> <p>You can terminate a WorkSpace that is in any state except <code>SUSPENDED</code>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have been completely terminated.</p>

#### `ARN`

``` purescript
newtype ARN
  = ARN String
```

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The user is not authorized to access a resource.</p>

#### `Alias`

``` purescript
newtype Alias
  = Alias String
```

#### `BooleanObject`

``` purescript
newtype BooleanObject
  = BooleanObject Boolean
```

#### `BundleId`

``` purescript
newtype BundleId
  = BundleId String
```

#### `BundleIdList`

``` purescript
newtype BundleIdList
  = BundleIdList (Array BundleId)
```

#### `BundleList`

``` purescript
newtype BundleList
  = BundleList (Array WorkspaceBundle)
```

#### `BundleOwner`

``` purescript
newtype BundleOwner
  = BundleOwner String
```

#### `Compute`

``` purescript
newtype Compute
  = Compute String
```

#### `ComputeType`

``` purescript
newtype ComputeType
  = ComputeType { "Name" :: NullOrUndefined (Compute) }
```

<p>Information about the compute type.</p>

#### `ComputerName`

``` purescript
newtype ComputerName
  = ComputerName String
```

#### `ConnectionState`

``` purescript
newtype ConnectionState
  = ConnectionState String
```

#### `CreateTagsRequest`

``` purescript
newtype CreateTagsRequest
  = CreateTagsRequest { "ResourceId" :: NonEmptyString, "Tags" :: TagList }
```

#### `CreateTagsResult`

``` purescript
newtype CreateTagsResult
  = CreateTagsResult {  }
```

#### `CreateWorkspacesRequest`

``` purescript
newtype CreateWorkspacesRequest
  = CreateWorkspacesRequest { "Workspaces" :: WorkspaceRequestList }
```

#### `CreateWorkspacesResult`

``` purescript
newtype CreateWorkspacesResult
  = CreateWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedCreateWorkspaceRequests), "PendingRequests" :: NullOrUndefined (WorkspaceList) }
```

#### `DefaultOu`

``` purescript
newtype DefaultOu
  = DefaultOu String
```

#### `DefaultWorkspaceCreationProperties`

``` purescript
newtype DefaultWorkspaceCreationProperties
  = DefaultWorkspaceCreationProperties { "EnableWorkDocs" :: NullOrUndefined (BooleanObject), "EnableInternetAccess" :: NullOrUndefined (BooleanObject), "DefaultOu" :: NullOrUndefined (DefaultOu), "CustomSecurityGroupId" :: NullOrUndefined (SecurityGroupId), "UserEnabledAsLocalAdministrator" :: NullOrUndefined (BooleanObject) }
```

<p>Information about defaults used to create a WorkSpace.</p>

#### `DeleteTagsRequest`

``` purescript
newtype DeleteTagsRequest
  = DeleteTagsRequest { "ResourceId" :: NonEmptyString, "TagKeys" :: TagKeyList }
```

#### `DeleteTagsResult`

``` purescript
newtype DeleteTagsResult
  = DeleteTagsResult {  }
```

#### `DescribeTagsRequest`

``` purescript
newtype DescribeTagsRequest
  = DescribeTagsRequest { "ResourceId" :: NonEmptyString }
```

#### `DescribeTagsResult`

``` purescript
newtype DescribeTagsResult
  = DescribeTagsResult { "TagList" :: NullOrUndefined (TagList) }
```

#### `DescribeWorkspaceBundlesRequest`

``` purescript
newtype DescribeWorkspaceBundlesRequest
  = DescribeWorkspaceBundlesRequest { "BundleIds" :: NullOrUndefined (BundleIdList), "Owner" :: NullOrUndefined (BundleOwner), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `DescribeWorkspaceBundlesResult`

``` purescript
newtype DescribeWorkspaceBundlesResult
  = DescribeWorkspaceBundlesResult { "Bundles" :: NullOrUndefined (BundleList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `DescribeWorkspaceDirectoriesRequest`

``` purescript
newtype DescribeWorkspaceDirectoriesRequest
  = DescribeWorkspaceDirectoriesRequest { "DirectoryIds" :: NullOrUndefined (DirectoryIdList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `DescribeWorkspaceDirectoriesResult`

``` purescript
newtype DescribeWorkspaceDirectoriesResult
  = DescribeWorkspaceDirectoriesResult { "Directories" :: NullOrUndefined (DirectoryList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `DescribeWorkspacesConnectionStatusRequest`

``` purescript
newtype DescribeWorkspacesConnectionStatusRequest
  = DescribeWorkspacesConnectionStatusRequest { "WorkspaceIds" :: NullOrUndefined (WorkspaceIdList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `DescribeWorkspacesConnectionStatusResult`

``` purescript
newtype DescribeWorkspacesConnectionStatusResult
  = DescribeWorkspacesConnectionStatusResult { "WorkspacesConnectionStatus" :: NullOrUndefined (WorkspaceConnectionStatusList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `DescribeWorkspacesRequest`

``` purescript
newtype DescribeWorkspacesRequest
  = DescribeWorkspacesRequest { "WorkspaceIds" :: NullOrUndefined (WorkspaceIdList), "DirectoryId" :: NullOrUndefined (DirectoryId), "UserName" :: NullOrUndefined (UserName), "BundleId" :: NullOrUndefined (BundleId), "Limit" :: NullOrUndefined (Limit), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `DescribeWorkspacesResult`

``` purescript
newtype DescribeWorkspacesResult
  = DescribeWorkspacesResult { "Workspaces" :: NullOrUndefined (WorkspaceList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

#### `Description`

``` purescript
newtype Description
  = Description String
```

#### `DirectoryId`

``` purescript
newtype DirectoryId
  = DirectoryId String
```

#### `DirectoryIdList`

``` purescript
newtype DirectoryIdList
  = DirectoryIdList (Array DirectoryId)
```

#### `DirectoryList`

``` purescript
newtype DirectoryList
  = DirectoryList (Array WorkspaceDirectory)
```

#### `DirectoryName`

``` purescript
newtype DirectoryName
  = DirectoryName String
```

#### `DnsIpAddresses`

``` purescript
newtype DnsIpAddresses
  = DnsIpAddresses (Array IpAddress)
```

#### `ErrorType`

``` purescript
newtype ErrorType
  = ErrorType String
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

#### `FailedCreateWorkspaceRequest`

``` purescript
newtype FailedCreateWorkspaceRequest
  = FailedCreateWorkspaceRequest { "WorkspaceRequest" :: NullOrUndefined (WorkspaceRequest), "ErrorCode" :: NullOrUndefined (ErrorType), "ErrorMessage" :: NullOrUndefined (Description) }
```

<p>Information about a WorkSpace that could not be created.</p>

#### `FailedCreateWorkspaceRequests`

``` purescript
newtype FailedCreateWorkspaceRequests
  = FailedCreateWorkspaceRequests (Array FailedCreateWorkspaceRequest)
```

#### `FailedRebootWorkspaceRequests`

``` purescript
newtype FailedRebootWorkspaceRequests
  = FailedRebootWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

#### `FailedRebuildWorkspaceRequests`

``` purescript
newtype FailedRebuildWorkspaceRequests
  = FailedRebuildWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

#### `FailedStartWorkspaceRequests`

``` purescript
newtype FailedStartWorkspaceRequests
  = FailedStartWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

#### `FailedStopWorkspaceRequests`

``` purescript
newtype FailedStopWorkspaceRequests
  = FailedStopWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

#### `FailedTerminateWorkspaceRequests`

``` purescript
newtype FailedTerminateWorkspaceRequests
  = FailedTerminateWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

#### `FailedWorkspaceChangeRequest`

``` purescript
newtype FailedWorkspaceChangeRequest
  = FailedWorkspaceChangeRequest { "WorkspaceId" :: NullOrUndefined (WorkspaceId), "ErrorCode" :: NullOrUndefined (ErrorType), "ErrorMessage" :: NullOrUndefined (Description) }
```

<p>Information about a WorkSpace that could not be rebooted (<a>RebootWorkspaces</a>), rebuilt (<a>RebuildWorkspaces</a>), terminated (<a>TerminateWorkspaces</a>), started (<a>StartWorkspaces</a>), or stopped (<a>StopWorkspaces</a>).</p>

#### `InvalidParameterValuesException`

``` purescript
newtype InvalidParameterValuesException
  = InvalidParameterValuesException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>One or more parameter values are not valid.</p>

#### `InvalidResourceStateException`

``` purescript
newtype InvalidResourceStateException
  = InvalidResourceStateException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The state of the WorkSpace is not valid for this operation.</p>

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

#### `Limit`

``` purescript
newtype Limit
  = Limit Int
```

#### `ModificationResourceEnum`

``` purescript
newtype ModificationResourceEnum
  = ModificationResourceEnum String
```

#### `ModificationState`

``` purescript
newtype ModificationState
  = ModificationState { "Resource" :: NullOrUndefined (ModificationResourceEnum), "State" :: NullOrUndefined (ModificationStateEnum) }
```

<p>Information about a WorkSpace modification.</p>

#### `ModificationStateEnum`

``` purescript
newtype ModificationStateEnum
  = ModificationStateEnum String
```

#### `ModificationStateList`

``` purescript
newtype ModificationStateList
  = ModificationStateList (Array ModificationState)
```

#### `ModifyWorkspacePropertiesRequest`

``` purescript
newtype ModifyWorkspacePropertiesRequest
  = ModifyWorkspacePropertiesRequest { "WorkspaceId" :: WorkspaceId, "WorkspaceProperties" :: WorkspaceProperties }
```

#### `ModifyWorkspacePropertiesResult`

``` purescript
newtype ModifyWorkspacePropertiesResult
  = ModifyWorkspacePropertiesResult {  }
```

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

#### `OperationInProgressException`

``` purescript
newtype OperationInProgressException
  = OperationInProgressException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The properties of this WorkSpace are currently being modified. Try again in a moment.</p>

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

#### `RebootRequest`

``` purescript
newtype RebootRequest
  = RebootRequest { "WorkspaceId" :: WorkspaceId }
```

<p>Information used to reboot a WorkSpace.</p>

#### `RebootWorkspaceRequests`

``` purescript
newtype RebootWorkspaceRequests
  = RebootWorkspaceRequests (Array RebootRequest)
```

#### `RebootWorkspacesRequest`

``` purescript
newtype RebootWorkspacesRequest
  = RebootWorkspacesRequest { "RebootWorkspaceRequests" :: RebootWorkspaceRequests }
```

#### `RebootWorkspacesResult`

``` purescript
newtype RebootWorkspacesResult
  = RebootWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedRebootWorkspaceRequests) }
```

#### `RebuildRequest`

``` purescript
newtype RebuildRequest
  = RebuildRequest { "WorkspaceId" :: WorkspaceId }
```

<p>Information used to rebuild a WorkSpace.</p>

#### `RebuildWorkspaceRequests`

``` purescript
newtype RebuildWorkspaceRequests
  = RebuildWorkspaceRequests (Array RebuildRequest)
```

#### `RebuildWorkspacesRequest`

``` purescript
newtype RebuildWorkspacesRequest
  = RebuildWorkspacesRequest { "RebuildWorkspaceRequests" :: RebuildWorkspaceRequests }
```

#### `RebuildWorkspacesResult`

``` purescript
newtype RebuildWorkspacesResult
  = RebuildWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedRebuildWorkspaceRequests) }
```

#### `RegistrationCode`

``` purescript
newtype RegistrationCode
  = RegistrationCode String
```

#### `ResourceLimitExceededException`

``` purescript
newtype ResourceLimitExceededException
  = ResourceLimitExceededException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>Your resource limits have been exceeded.</p>

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage), "ResourceId" :: NullOrUndefined (NonEmptyString) }
```

<p>The resource could not be found.</p>

#### `ResourceUnavailableException`

``` purescript
newtype ResourceUnavailableException
  = ResourceUnavailableException { "Message'" :: NullOrUndefined (ExceptionMessage), "ResourceId" :: NullOrUndefined (NonEmptyString) }
```

<p>The specified resource is not available.</p>

#### `RootStorage`

``` purescript
newtype RootStorage
  = RootStorage { "Capacity" :: NullOrUndefined (NonEmptyString) }
```

<p>Information about the root volume for a WorkSpace bundle.</p>

#### `RootVolumeSizeGib`

``` purescript
newtype RootVolumeSizeGib
  = RootVolumeSizeGib Int
```

#### `RunningMode`

``` purescript
newtype RunningMode
  = RunningMode String
```

#### `RunningModeAutoStopTimeoutInMinutes`

``` purescript
newtype RunningModeAutoStopTimeoutInMinutes
  = RunningModeAutoStopTimeoutInMinutes Int
```

#### `SecurityGroupId`

``` purescript
newtype SecurityGroupId
  = SecurityGroupId String
```

#### `StartRequest`

``` purescript
newtype StartRequest
  = StartRequest { "WorkspaceId" :: NullOrUndefined (WorkspaceId) }
```

<p>Information used to start a WorkSpace.</p>

#### `StartWorkspaceRequests`

``` purescript
newtype StartWorkspaceRequests
  = StartWorkspaceRequests (Array StartRequest)
```

#### `StartWorkspacesRequest`

``` purescript
newtype StartWorkspacesRequest
  = StartWorkspacesRequest { "StartWorkspaceRequests" :: StartWorkspaceRequests }
```

#### `StartWorkspacesResult`

``` purescript
newtype StartWorkspacesResult
  = StartWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedStartWorkspaceRequests) }
```

#### `StopRequest`

``` purescript
newtype StopRequest
  = StopRequest { "WorkspaceId" :: NullOrUndefined (WorkspaceId) }
```

<p>Information used to stop a WorkSpace.</p>

#### `StopWorkspaceRequests`

``` purescript
newtype StopWorkspaceRequests
  = StopWorkspaceRequests (Array StopRequest)
```

#### `StopWorkspacesRequest`

``` purescript
newtype StopWorkspacesRequest
  = StopWorkspacesRequest { "StopWorkspaceRequests" :: StopWorkspaceRequests }
```

#### `StopWorkspacesResult`

``` purescript
newtype StopWorkspacesResult
  = StopWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedStopWorkspaceRequests) }
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
  = Tag { "Key" :: TagKey, "Value" :: NullOrUndefined (TagValue) }
```

<p>Information about a tag.</p>

#### `TagKey`

``` purescript
newtype TagKey
  = TagKey String
```

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array NonEmptyString)
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

#### `TagValue`

``` purescript
newtype TagValue
  = TagValue String
```

#### `TerminateRequest`

``` purescript
newtype TerminateRequest
  = TerminateRequest { "WorkspaceId" :: WorkspaceId }
```

<p>Information used to terminate a WorkSpace.</p>

#### `TerminateWorkspaceRequests`

``` purescript
newtype TerminateWorkspaceRequests
  = TerminateWorkspaceRequests (Array TerminateRequest)
```

#### `TerminateWorkspacesRequest`

``` purescript
newtype TerminateWorkspacesRequest
  = TerminateWorkspacesRequest { "TerminateWorkspaceRequests" :: TerminateWorkspaceRequests }
```

#### `TerminateWorkspacesResult`

``` purescript
newtype TerminateWorkspacesResult
  = TerminateWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedTerminateWorkspaceRequests) }
```

#### `UnsupportedWorkspaceConfigurationException`

``` purescript
newtype UnsupportedWorkspaceConfigurationException
  = UnsupportedWorkspaceConfigurationException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The configuration of this WorkSpace is not supported for this operation. For more information, see the <a href="http://docs.aws.amazon.com/workspaces/latest/adminguide/">Amazon WorkSpaces Administration Guide</a>. </p>

#### `UserName`

``` purescript
newtype UserName
  = UserName String
```

#### `UserStorage`

``` purescript
newtype UserStorage
  = UserStorage { "Capacity" :: NullOrUndefined (NonEmptyString) }
```

<p>Information about the user storage for a WorkSpace bundle.</p>

#### `UserVolumeSizeGib`

``` purescript
newtype UserVolumeSizeGib
  = UserVolumeSizeGib Int
```

#### `VolumeEncryptionKey`

``` purescript
newtype VolumeEncryptionKey
  = VolumeEncryptionKey String
```

#### `Workspace`

``` purescript
newtype Workspace
  = Workspace { "WorkspaceId" :: NullOrUndefined (WorkspaceId), "DirectoryId" :: NullOrUndefined (DirectoryId), "UserName" :: NullOrUndefined (UserName), "IpAddress" :: NullOrUndefined (IpAddress), "State" :: NullOrUndefined (WorkspaceState), "BundleId" :: NullOrUndefined (BundleId), "SubnetId" :: NullOrUndefined (SubnetId), "ErrorMessage" :: NullOrUndefined (Description), "ErrorCode" :: NullOrUndefined (WorkspaceErrorCode), "ComputerName" :: NullOrUndefined (ComputerName), "VolumeEncryptionKey" :: NullOrUndefined (VolumeEncryptionKey), "UserVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject), "RootVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject), "WorkspaceProperties" :: NullOrUndefined (WorkspaceProperties), "ModificationStates" :: NullOrUndefined (ModificationStateList) }
```

<p>Information about a WorkSpace.</p>

#### `WorkspaceBundle`

``` purescript
newtype WorkspaceBundle
  = WorkspaceBundle { "BundleId" :: NullOrUndefined (BundleId), "Name" :: NullOrUndefined (NonEmptyString), "Owner" :: NullOrUndefined (BundleOwner), "Description" :: NullOrUndefined (Description), "RootStorage" :: NullOrUndefined (RootStorage), "UserStorage" :: NullOrUndefined (UserStorage), "ComputeType" :: NullOrUndefined (ComputeType) }
```

<p>Information about a WorkSpace bundle.</p>

#### `WorkspaceConnectionStatus`

``` purescript
newtype WorkspaceConnectionStatus
  = WorkspaceConnectionStatus { "WorkspaceId" :: NullOrUndefined (WorkspaceId), "ConnectionState" :: NullOrUndefined (ConnectionState), "ConnectionStateCheckTimestamp" :: NullOrUndefined (Number), "LastKnownUserConnectionTimestamp" :: NullOrUndefined (Number) }
```

<p>Describes the connection status of a WorkSpace.</p>

#### `WorkspaceConnectionStatusList`

``` purescript
newtype WorkspaceConnectionStatusList
  = WorkspaceConnectionStatusList (Array WorkspaceConnectionStatus)
```

#### `WorkspaceDirectory`

``` purescript
newtype WorkspaceDirectory
  = WorkspaceDirectory { "DirectoryId" :: NullOrUndefined (DirectoryId), "Alias" :: NullOrUndefined (Alias), "DirectoryName" :: NullOrUndefined (DirectoryName), "RegistrationCode" :: NullOrUndefined (RegistrationCode), "SubnetIds" :: NullOrUndefined (SubnetIds), "DnsIpAddresses" :: NullOrUndefined (DnsIpAddresses), "CustomerUserName" :: NullOrUndefined (UserName), "IamRoleId" :: NullOrUndefined (ARN), "DirectoryType" :: NullOrUndefined (WorkspaceDirectoryType), "WorkspaceSecurityGroupId" :: NullOrUndefined (SecurityGroupId), "State" :: NullOrUndefined (WorkspaceDirectoryState), "WorkspaceCreationProperties" :: NullOrUndefined (DefaultWorkspaceCreationProperties) }
```

<p>Contains information about an AWS Directory Service directory for use with Amazon WorkSpaces.</p>

#### `WorkspaceDirectoryState`

``` purescript
newtype WorkspaceDirectoryState
  = WorkspaceDirectoryState String
```

#### `WorkspaceDirectoryType`

``` purescript
newtype WorkspaceDirectoryType
  = WorkspaceDirectoryType String
```

#### `WorkspaceErrorCode`

``` purescript
newtype WorkspaceErrorCode
  = WorkspaceErrorCode String
```

#### `WorkspaceId`

``` purescript
newtype WorkspaceId
  = WorkspaceId String
```

#### `WorkspaceIdList`

``` purescript
newtype WorkspaceIdList
  = WorkspaceIdList (Array WorkspaceId)
```

#### `WorkspaceList`

``` purescript
newtype WorkspaceList
  = WorkspaceList (Array Workspace)
```

#### `WorkspaceProperties`

``` purescript
newtype WorkspaceProperties
  = WorkspaceProperties { "RunningMode" :: NullOrUndefined (RunningMode), "RunningModeAutoStopTimeoutInMinutes" :: NullOrUndefined (RunningModeAutoStopTimeoutInMinutes), "RootVolumeSizeGib" :: NullOrUndefined (RootVolumeSizeGib), "UserVolumeSizeGib" :: NullOrUndefined (UserVolumeSizeGib), "ComputeTypeName" :: NullOrUndefined (Compute) }
```

<p>Information about a WorkSpace.</p>

#### `WorkspaceRequest`

``` purescript
newtype WorkspaceRequest
  = WorkspaceRequest { "DirectoryId" :: DirectoryId, "UserName" :: UserName, "BundleId" :: BundleId, "VolumeEncryptionKey" :: NullOrUndefined (VolumeEncryptionKey), "UserVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject), "RootVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject), "WorkspaceProperties" :: NullOrUndefined (WorkspaceProperties), "Tags" :: NullOrUndefined (TagList) }
```

<p>Information used to create a WorkSpace.</p>

#### `WorkspaceRequestList`

``` purescript
newtype WorkspaceRequestList
  = WorkspaceRequestList (Array WorkspaceRequest)
```

#### `WorkspaceState`

``` purescript
newtype WorkspaceState
  = WorkspaceState String
```


