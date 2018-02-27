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

##### Instances
``` purescript
Newtype ARN _
```

#### `AccessDeniedException`

``` purescript
newtype AccessDeniedException
  = AccessDeniedException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The user is not authorized to access a resource.</p>

##### Instances
``` purescript
Newtype AccessDeniedException _
```

#### `Alias`

``` purescript
newtype Alias
  = Alias String
```

##### Instances
``` purescript
Newtype Alias _
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

#### `BundleId`

``` purescript
newtype BundleId
  = BundleId String
```

##### Instances
``` purescript
Newtype BundleId _
```

#### `BundleIdList`

``` purescript
newtype BundleIdList
  = BundleIdList (Array BundleId)
```

##### Instances
``` purescript
Newtype BundleIdList _
```

#### `BundleList`

``` purescript
newtype BundleList
  = BundleList (Array WorkspaceBundle)
```

##### Instances
``` purescript
Newtype BundleList _
```

#### `BundleOwner`

``` purescript
newtype BundleOwner
  = BundleOwner String
```

##### Instances
``` purescript
Newtype BundleOwner _
```

#### `Compute`

``` purescript
newtype Compute
  = Compute String
```

##### Instances
``` purescript
Newtype Compute _
```

#### `ComputeType`

``` purescript
newtype ComputeType
  = ComputeType { "Name" :: NullOrUndefined (Compute) }
```

<p>Information about the compute type.</p>

##### Instances
``` purescript
Newtype ComputeType _
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

#### `ConnectionState`

``` purescript
newtype ConnectionState
  = ConnectionState String
```

##### Instances
``` purescript
Newtype ConnectionState _
```

#### `CreateTagsRequest`

``` purescript
newtype CreateTagsRequest
  = CreateTagsRequest { "ResourceId" :: NonEmptyString, "Tags" :: TagList }
```

##### Instances
``` purescript
Newtype CreateTagsRequest _
```

#### `CreateTagsResult`

``` purescript
newtype CreateTagsResult
  = CreateTagsResult {  }
```

##### Instances
``` purescript
Newtype CreateTagsResult _
```

#### `CreateWorkspacesRequest`

``` purescript
newtype CreateWorkspacesRequest
  = CreateWorkspacesRequest { "Workspaces" :: WorkspaceRequestList }
```

##### Instances
``` purescript
Newtype CreateWorkspacesRequest _
```

#### `CreateWorkspacesResult`

``` purescript
newtype CreateWorkspacesResult
  = CreateWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedCreateWorkspaceRequests), "PendingRequests" :: NullOrUndefined (WorkspaceList) }
```

##### Instances
``` purescript
Newtype CreateWorkspacesResult _
```

#### `DefaultOu`

``` purescript
newtype DefaultOu
  = DefaultOu String
```

##### Instances
``` purescript
Newtype DefaultOu _
```

#### `DefaultWorkspaceCreationProperties`

``` purescript
newtype DefaultWorkspaceCreationProperties
  = DefaultWorkspaceCreationProperties { "EnableWorkDocs" :: NullOrUndefined (BooleanObject), "EnableInternetAccess" :: NullOrUndefined (BooleanObject), "DefaultOu" :: NullOrUndefined (DefaultOu), "CustomSecurityGroupId" :: NullOrUndefined (SecurityGroupId), "UserEnabledAsLocalAdministrator" :: NullOrUndefined (BooleanObject) }
```

<p>Information about defaults used to create a WorkSpace.</p>

##### Instances
``` purescript
Newtype DefaultWorkspaceCreationProperties _
```

#### `DeleteTagsRequest`

``` purescript
newtype DeleteTagsRequest
  = DeleteTagsRequest { "ResourceId" :: NonEmptyString, "TagKeys" :: TagKeyList }
```

##### Instances
``` purescript
Newtype DeleteTagsRequest _
```

#### `DeleteTagsResult`

``` purescript
newtype DeleteTagsResult
  = DeleteTagsResult {  }
```

##### Instances
``` purescript
Newtype DeleteTagsResult _
```

#### `DescribeTagsRequest`

``` purescript
newtype DescribeTagsRequest
  = DescribeTagsRequest { "ResourceId" :: NonEmptyString }
```

##### Instances
``` purescript
Newtype DescribeTagsRequest _
```

#### `DescribeTagsResult`

``` purescript
newtype DescribeTagsResult
  = DescribeTagsResult { "TagList" :: NullOrUndefined (TagList) }
```

##### Instances
``` purescript
Newtype DescribeTagsResult _
```

#### `DescribeWorkspaceBundlesRequest`

``` purescript
newtype DescribeWorkspaceBundlesRequest
  = DescribeWorkspaceBundlesRequest { "BundleIds" :: NullOrUndefined (BundleIdList), "Owner" :: NullOrUndefined (BundleOwner), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype DescribeWorkspaceBundlesRequest _
```

#### `DescribeWorkspaceBundlesResult`

``` purescript
newtype DescribeWorkspaceBundlesResult
  = DescribeWorkspaceBundlesResult { "Bundles" :: NullOrUndefined (BundleList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype DescribeWorkspaceBundlesResult _
```

#### `DescribeWorkspaceDirectoriesRequest`

``` purescript
newtype DescribeWorkspaceDirectoriesRequest
  = DescribeWorkspaceDirectoriesRequest { "DirectoryIds" :: NullOrUndefined (DirectoryIdList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype DescribeWorkspaceDirectoriesRequest _
```

#### `DescribeWorkspaceDirectoriesResult`

``` purescript
newtype DescribeWorkspaceDirectoriesResult
  = DescribeWorkspaceDirectoriesResult { "Directories" :: NullOrUndefined (DirectoryList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype DescribeWorkspaceDirectoriesResult _
```

#### `DescribeWorkspacesConnectionStatusRequest`

``` purescript
newtype DescribeWorkspacesConnectionStatusRequest
  = DescribeWorkspacesConnectionStatusRequest { "WorkspaceIds" :: NullOrUndefined (WorkspaceIdList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype DescribeWorkspacesConnectionStatusRequest _
```

#### `DescribeWorkspacesConnectionStatusResult`

``` purescript
newtype DescribeWorkspacesConnectionStatusResult
  = DescribeWorkspacesConnectionStatusResult { "WorkspacesConnectionStatus" :: NullOrUndefined (WorkspaceConnectionStatusList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype DescribeWorkspacesConnectionStatusResult _
```

#### `DescribeWorkspacesRequest`

``` purescript
newtype DescribeWorkspacesRequest
  = DescribeWorkspacesRequest { "WorkspaceIds" :: NullOrUndefined (WorkspaceIdList), "DirectoryId" :: NullOrUndefined (DirectoryId), "UserName" :: NullOrUndefined (UserName), "BundleId" :: NullOrUndefined (BundleId), "Limit" :: NullOrUndefined (Limit), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype DescribeWorkspacesRequest _
```

#### `DescribeWorkspacesResult`

``` purescript
newtype DescribeWorkspacesResult
  = DescribeWorkspacesResult { "Workspaces" :: NullOrUndefined (WorkspaceList), "NextToken" :: NullOrUndefined (PaginationToken) }
```

##### Instances
``` purescript
Newtype DescribeWorkspacesResult _
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

#### `DirectoryId`

``` purescript
newtype DirectoryId
  = DirectoryId String
```

##### Instances
``` purescript
Newtype DirectoryId _
```

#### `DirectoryIdList`

``` purescript
newtype DirectoryIdList
  = DirectoryIdList (Array DirectoryId)
```

##### Instances
``` purescript
Newtype DirectoryIdList _
```

#### `DirectoryList`

``` purescript
newtype DirectoryList
  = DirectoryList (Array WorkspaceDirectory)
```

##### Instances
``` purescript
Newtype DirectoryList _
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

#### `DnsIpAddresses`

``` purescript
newtype DnsIpAddresses
  = DnsIpAddresses (Array IpAddress)
```

##### Instances
``` purescript
Newtype DnsIpAddresses _
```

#### `ErrorType`

``` purescript
newtype ErrorType
  = ErrorType String
```

##### Instances
``` purescript
Newtype ErrorType _
```

#### `ExceptionMessage`

``` purescript
newtype ExceptionMessage
  = ExceptionMessage String
```

##### Instances
``` purescript
Newtype ExceptionMessage _
```

#### `FailedCreateWorkspaceRequest`

``` purescript
newtype FailedCreateWorkspaceRequest
  = FailedCreateWorkspaceRequest { "WorkspaceRequest" :: NullOrUndefined (WorkspaceRequest), "ErrorCode" :: NullOrUndefined (ErrorType), "ErrorMessage" :: NullOrUndefined (Description) }
```

<p>Information about a WorkSpace that could not be created.</p>

##### Instances
``` purescript
Newtype FailedCreateWorkspaceRequest _
```

#### `FailedCreateWorkspaceRequests`

``` purescript
newtype FailedCreateWorkspaceRequests
  = FailedCreateWorkspaceRequests (Array FailedCreateWorkspaceRequest)
```

##### Instances
``` purescript
Newtype FailedCreateWorkspaceRequests _
```

#### `FailedRebootWorkspaceRequests`

``` purescript
newtype FailedRebootWorkspaceRequests
  = FailedRebootWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

##### Instances
``` purescript
Newtype FailedRebootWorkspaceRequests _
```

#### `FailedRebuildWorkspaceRequests`

``` purescript
newtype FailedRebuildWorkspaceRequests
  = FailedRebuildWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

##### Instances
``` purescript
Newtype FailedRebuildWorkspaceRequests _
```

#### `FailedStartWorkspaceRequests`

``` purescript
newtype FailedStartWorkspaceRequests
  = FailedStartWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

##### Instances
``` purescript
Newtype FailedStartWorkspaceRequests _
```

#### `FailedStopWorkspaceRequests`

``` purescript
newtype FailedStopWorkspaceRequests
  = FailedStopWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

##### Instances
``` purescript
Newtype FailedStopWorkspaceRequests _
```

#### `FailedTerminateWorkspaceRequests`

``` purescript
newtype FailedTerminateWorkspaceRequests
  = FailedTerminateWorkspaceRequests (Array FailedWorkspaceChangeRequest)
```

##### Instances
``` purescript
Newtype FailedTerminateWorkspaceRequests _
```

#### `FailedWorkspaceChangeRequest`

``` purescript
newtype FailedWorkspaceChangeRequest
  = FailedWorkspaceChangeRequest { "WorkspaceId" :: NullOrUndefined (WorkspaceId), "ErrorCode" :: NullOrUndefined (ErrorType), "ErrorMessage" :: NullOrUndefined (Description) }
```

<p>Information about a WorkSpace that could not be rebooted (<a>RebootWorkspaces</a>), rebuilt (<a>RebuildWorkspaces</a>), terminated (<a>TerminateWorkspaces</a>), started (<a>StartWorkspaces</a>), or stopped (<a>StopWorkspaces</a>).</p>

##### Instances
``` purescript
Newtype FailedWorkspaceChangeRequest _
```

#### `InvalidParameterValuesException`

``` purescript
newtype InvalidParameterValuesException
  = InvalidParameterValuesException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>One or more parameter values are not valid.</p>

##### Instances
``` purescript
Newtype InvalidParameterValuesException _
```

#### `InvalidResourceStateException`

``` purescript
newtype InvalidResourceStateException
  = InvalidResourceStateException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The state of the WorkSpace is not valid for this operation.</p>

##### Instances
``` purescript
Newtype InvalidResourceStateException _
```

#### `IpAddress`

``` purescript
newtype IpAddress
  = IpAddress String
```

##### Instances
``` purescript
Newtype IpAddress _
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

#### `ModificationResourceEnum`

``` purescript
newtype ModificationResourceEnum
  = ModificationResourceEnum String
```

##### Instances
``` purescript
Newtype ModificationResourceEnum _
```

#### `ModificationState`

``` purescript
newtype ModificationState
  = ModificationState { "Resource" :: NullOrUndefined (ModificationResourceEnum), "State" :: NullOrUndefined (ModificationStateEnum) }
```

<p>Information about a WorkSpace modification.</p>

##### Instances
``` purescript
Newtype ModificationState _
```

#### `ModificationStateEnum`

``` purescript
newtype ModificationStateEnum
  = ModificationStateEnum String
```

##### Instances
``` purescript
Newtype ModificationStateEnum _
```

#### `ModificationStateList`

``` purescript
newtype ModificationStateList
  = ModificationStateList (Array ModificationState)
```

##### Instances
``` purescript
Newtype ModificationStateList _
```

#### `ModifyWorkspacePropertiesRequest`

``` purescript
newtype ModifyWorkspacePropertiesRequest
  = ModifyWorkspacePropertiesRequest { "WorkspaceId" :: WorkspaceId, "WorkspaceProperties" :: WorkspaceProperties }
```

##### Instances
``` purescript
Newtype ModifyWorkspacePropertiesRequest _
```

#### `ModifyWorkspacePropertiesResult`

``` purescript
newtype ModifyWorkspacePropertiesResult
  = ModifyWorkspacePropertiesResult {  }
```

##### Instances
``` purescript
Newtype ModifyWorkspacePropertiesResult _
```

#### `NonEmptyString`

``` purescript
newtype NonEmptyString
  = NonEmptyString String
```

##### Instances
``` purescript
Newtype NonEmptyString _
```

#### `OperationInProgressException`

``` purescript
newtype OperationInProgressException
  = OperationInProgressException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The properties of this WorkSpace are currently being modified. Try again in a moment.</p>

##### Instances
``` purescript
Newtype OperationInProgressException _
```

#### `PaginationToken`

``` purescript
newtype PaginationToken
  = PaginationToken String
```

##### Instances
``` purescript
Newtype PaginationToken _
```

#### `RebootRequest`

``` purescript
newtype RebootRequest
  = RebootRequest { "WorkspaceId" :: WorkspaceId }
```

<p>Information used to reboot a WorkSpace.</p>

##### Instances
``` purescript
Newtype RebootRequest _
```

#### `RebootWorkspaceRequests`

``` purescript
newtype RebootWorkspaceRequests
  = RebootWorkspaceRequests (Array RebootRequest)
```

##### Instances
``` purescript
Newtype RebootWorkspaceRequests _
```

#### `RebootWorkspacesRequest`

``` purescript
newtype RebootWorkspacesRequest
  = RebootWorkspacesRequest { "RebootWorkspaceRequests" :: RebootWorkspaceRequests }
```

##### Instances
``` purescript
Newtype RebootWorkspacesRequest _
```

#### `RebootWorkspacesResult`

``` purescript
newtype RebootWorkspacesResult
  = RebootWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedRebootWorkspaceRequests) }
```

##### Instances
``` purescript
Newtype RebootWorkspacesResult _
```

#### `RebuildRequest`

``` purescript
newtype RebuildRequest
  = RebuildRequest { "WorkspaceId" :: WorkspaceId }
```

<p>Information used to rebuild a WorkSpace.</p>

##### Instances
``` purescript
Newtype RebuildRequest _
```

#### `RebuildWorkspaceRequests`

``` purescript
newtype RebuildWorkspaceRequests
  = RebuildWorkspaceRequests (Array RebuildRequest)
```

##### Instances
``` purescript
Newtype RebuildWorkspaceRequests _
```

#### `RebuildWorkspacesRequest`

``` purescript
newtype RebuildWorkspacesRequest
  = RebuildWorkspacesRequest { "RebuildWorkspaceRequests" :: RebuildWorkspaceRequests }
```

##### Instances
``` purescript
Newtype RebuildWorkspacesRequest _
```

#### `RebuildWorkspacesResult`

``` purescript
newtype RebuildWorkspacesResult
  = RebuildWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedRebuildWorkspaceRequests) }
```

##### Instances
``` purescript
Newtype RebuildWorkspacesResult _
```

#### `RegistrationCode`

``` purescript
newtype RegistrationCode
  = RegistrationCode String
```

##### Instances
``` purescript
Newtype RegistrationCode _
```

#### `ResourceLimitExceededException`

``` purescript
newtype ResourceLimitExceededException
  = ResourceLimitExceededException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>Your resource limits have been exceeded.</p>

##### Instances
``` purescript
Newtype ResourceLimitExceededException _
```

#### `ResourceNotFoundException`

``` purescript
newtype ResourceNotFoundException
  = ResourceNotFoundException { "Message'" :: NullOrUndefined (ExceptionMessage), "ResourceId" :: NullOrUndefined (NonEmptyString) }
```

<p>The resource could not be found.</p>

##### Instances
``` purescript
Newtype ResourceNotFoundException _
```

#### `ResourceUnavailableException`

``` purescript
newtype ResourceUnavailableException
  = ResourceUnavailableException { "Message'" :: NullOrUndefined (ExceptionMessage), "ResourceId" :: NullOrUndefined (NonEmptyString) }
```

<p>The specified resource is not available.</p>

##### Instances
``` purescript
Newtype ResourceUnavailableException _
```

#### `RootStorage`

``` purescript
newtype RootStorage
  = RootStorage { "Capacity" :: NullOrUndefined (NonEmptyString) }
```

<p>Information about the root volume for a WorkSpace bundle.</p>

##### Instances
``` purescript
Newtype RootStorage _
```

#### `RootVolumeSizeGib`

``` purescript
newtype RootVolumeSizeGib
  = RootVolumeSizeGib Int
```

##### Instances
``` purescript
Newtype RootVolumeSizeGib _
```

#### `RunningMode`

``` purescript
newtype RunningMode
  = RunningMode String
```

##### Instances
``` purescript
Newtype RunningMode _
```

#### `RunningModeAutoStopTimeoutInMinutes`

``` purescript
newtype RunningModeAutoStopTimeoutInMinutes
  = RunningModeAutoStopTimeoutInMinutes Int
```

##### Instances
``` purescript
Newtype RunningModeAutoStopTimeoutInMinutes _
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

#### `StartRequest`

``` purescript
newtype StartRequest
  = StartRequest { "WorkspaceId" :: NullOrUndefined (WorkspaceId) }
```

<p>Information used to start a WorkSpace.</p>

##### Instances
``` purescript
Newtype StartRequest _
```

#### `StartWorkspaceRequests`

``` purescript
newtype StartWorkspaceRequests
  = StartWorkspaceRequests (Array StartRequest)
```

##### Instances
``` purescript
Newtype StartWorkspaceRequests _
```

#### `StartWorkspacesRequest`

``` purescript
newtype StartWorkspacesRequest
  = StartWorkspacesRequest { "StartWorkspaceRequests" :: StartWorkspaceRequests }
```

##### Instances
``` purescript
Newtype StartWorkspacesRequest _
```

#### `StartWorkspacesResult`

``` purescript
newtype StartWorkspacesResult
  = StartWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedStartWorkspaceRequests) }
```

##### Instances
``` purescript
Newtype StartWorkspacesResult _
```

#### `StopRequest`

``` purescript
newtype StopRequest
  = StopRequest { "WorkspaceId" :: NullOrUndefined (WorkspaceId) }
```

<p>Information used to stop a WorkSpace.</p>

##### Instances
``` purescript
Newtype StopRequest _
```

#### `StopWorkspaceRequests`

``` purescript
newtype StopWorkspaceRequests
  = StopWorkspaceRequests (Array StopRequest)
```

##### Instances
``` purescript
Newtype StopWorkspaceRequests _
```

#### `StopWorkspacesRequest`

``` purescript
newtype StopWorkspacesRequest
  = StopWorkspacesRequest { "StopWorkspaceRequests" :: StopWorkspaceRequests }
```

##### Instances
``` purescript
Newtype StopWorkspacesRequest _
```

#### `StopWorkspacesResult`

``` purescript
newtype StopWorkspacesResult
  = StopWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedStopWorkspaceRequests) }
```

##### Instances
``` purescript
Newtype StopWorkspacesResult _
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
  = Tag { "Key" :: TagKey, "Value" :: NullOrUndefined (TagValue) }
```

<p>Information about a tag.</p>

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

#### `TagKeyList`

``` purescript
newtype TagKeyList
  = TagKeyList (Array NonEmptyString)
```

##### Instances
``` purescript
Newtype TagKeyList _
```

#### `TagList`

``` purescript
newtype TagList
  = TagList (Array Tag)
```

##### Instances
``` purescript
Newtype TagList _
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

#### `TerminateRequest`

``` purescript
newtype TerminateRequest
  = TerminateRequest { "WorkspaceId" :: WorkspaceId }
```

<p>Information used to terminate a WorkSpace.</p>

##### Instances
``` purescript
Newtype TerminateRequest _
```

#### `TerminateWorkspaceRequests`

``` purescript
newtype TerminateWorkspaceRequests
  = TerminateWorkspaceRequests (Array TerminateRequest)
```

##### Instances
``` purescript
Newtype TerminateWorkspaceRequests _
```

#### `TerminateWorkspacesRequest`

``` purescript
newtype TerminateWorkspacesRequest
  = TerminateWorkspacesRequest { "TerminateWorkspaceRequests" :: TerminateWorkspaceRequests }
```

##### Instances
``` purescript
Newtype TerminateWorkspacesRequest _
```

#### `TerminateWorkspacesResult`

``` purescript
newtype TerminateWorkspacesResult
  = TerminateWorkspacesResult { "FailedRequests" :: NullOrUndefined (FailedTerminateWorkspaceRequests) }
```

##### Instances
``` purescript
Newtype TerminateWorkspacesResult _
```

#### `UnsupportedWorkspaceConfigurationException`

``` purescript
newtype UnsupportedWorkspaceConfigurationException
  = UnsupportedWorkspaceConfigurationException { "Message'" :: NullOrUndefined (ExceptionMessage) }
```

<p>The configuration of this WorkSpace is not supported for this operation. For more information, see the <a href="http://docs.aws.amazon.com/workspaces/latest/adminguide/">Amazon WorkSpaces Administration Guide</a>. </p>

##### Instances
``` purescript
Newtype UnsupportedWorkspaceConfigurationException _
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

#### `UserStorage`

``` purescript
newtype UserStorage
  = UserStorage { "Capacity" :: NullOrUndefined (NonEmptyString) }
```

<p>Information about the user storage for a WorkSpace bundle.</p>

##### Instances
``` purescript
Newtype UserStorage _
```

#### `UserVolumeSizeGib`

``` purescript
newtype UserVolumeSizeGib
  = UserVolumeSizeGib Int
```

##### Instances
``` purescript
Newtype UserVolumeSizeGib _
```

#### `VolumeEncryptionKey`

``` purescript
newtype VolumeEncryptionKey
  = VolumeEncryptionKey String
```

##### Instances
``` purescript
Newtype VolumeEncryptionKey _
```

#### `Workspace`

``` purescript
newtype Workspace
  = Workspace { "WorkspaceId" :: NullOrUndefined (WorkspaceId), "DirectoryId" :: NullOrUndefined (DirectoryId), "UserName" :: NullOrUndefined (UserName), "IpAddress" :: NullOrUndefined (IpAddress), "State" :: NullOrUndefined (WorkspaceState), "BundleId" :: NullOrUndefined (BundleId), "SubnetId" :: NullOrUndefined (SubnetId), "ErrorMessage" :: NullOrUndefined (Description), "ErrorCode" :: NullOrUndefined (WorkspaceErrorCode), "ComputerName" :: NullOrUndefined (ComputerName), "VolumeEncryptionKey" :: NullOrUndefined (VolumeEncryptionKey), "UserVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject), "RootVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject), "WorkspaceProperties" :: NullOrUndefined (WorkspaceProperties), "ModificationStates" :: NullOrUndefined (ModificationStateList) }
```

<p>Information about a WorkSpace.</p>

##### Instances
``` purescript
Newtype Workspace _
```

#### `WorkspaceBundle`

``` purescript
newtype WorkspaceBundle
  = WorkspaceBundle { "BundleId" :: NullOrUndefined (BundleId), "Name" :: NullOrUndefined (NonEmptyString), "Owner" :: NullOrUndefined (BundleOwner), "Description" :: NullOrUndefined (Description), "RootStorage" :: NullOrUndefined (RootStorage), "UserStorage" :: NullOrUndefined (UserStorage), "ComputeType" :: NullOrUndefined (ComputeType) }
```

<p>Information about a WorkSpace bundle.</p>

##### Instances
``` purescript
Newtype WorkspaceBundle _
```

#### `WorkspaceConnectionStatus`

``` purescript
newtype WorkspaceConnectionStatus
  = WorkspaceConnectionStatus { "WorkspaceId" :: NullOrUndefined (WorkspaceId), "ConnectionState" :: NullOrUndefined (ConnectionState), "ConnectionStateCheckTimestamp" :: NullOrUndefined (Number), "LastKnownUserConnectionTimestamp" :: NullOrUndefined (Number) }
```

<p>Describes the connection status of a WorkSpace.</p>

##### Instances
``` purescript
Newtype WorkspaceConnectionStatus _
```

#### `WorkspaceConnectionStatusList`

``` purescript
newtype WorkspaceConnectionStatusList
  = WorkspaceConnectionStatusList (Array WorkspaceConnectionStatus)
```

##### Instances
``` purescript
Newtype WorkspaceConnectionStatusList _
```

#### `WorkspaceDirectory`

``` purescript
newtype WorkspaceDirectory
  = WorkspaceDirectory { "DirectoryId" :: NullOrUndefined (DirectoryId), "Alias" :: NullOrUndefined (Alias), "DirectoryName" :: NullOrUndefined (DirectoryName), "RegistrationCode" :: NullOrUndefined (RegistrationCode), "SubnetIds" :: NullOrUndefined (SubnetIds), "DnsIpAddresses" :: NullOrUndefined (DnsIpAddresses), "CustomerUserName" :: NullOrUndefined (UserName), "IamRoleId" :: NullOrUndefined (ARN), "DirectoryType" :: NullOrUndefined (WorkspaceDirectoryType), "WorkspaceSecurityGroupId" :: NullOrUndefined (SecurityGroupId), "State" :: NullOrUndefined (WorkspaceDirectoryState), "WorkspaceCreationProperties" :: NullOrUndefined (DefaultWorkspaceCreationProperties) }
```

<p>Contains information about an AWS Directory Service directory for use with Amazon WorkSpaces.</p>

##### Instances
``` purescript
Newtype WorkspaceDirectory _
```

#### `WorkspaceDirectoryState`

``` purescript
newtype WorkspaceDirectoryState
  = WorkspaceDirectoryState String
```

##### Instances
``` purescript
Newtype WorkspaceDirectoryState _
```

#### `WorkspaceDirectoryType`

``` purescript
newtype WorkspaceDirectoryType
  = WorkspaceDirectoryType String
```

##### Instances
``` purescript
Newtype WorkspaceDirectoryType _
```

#### `WorkspaceErrorCode`

``` purescript
newtype WorkspaceErrorCode
  = WorkspaceErrorCode String
```

##### Instances
``` purescript
Newtype WorkspaceErrorCode _
```

#### `WorkspaceId`

``` purescript
newtype WorkspaceId
  = WorkspaceId String
```

##### Instances
``` purescript
Newtype WorkspaceId _
```

#### `WorkspaceIdList`

``` purescript
newtype WorkspaceIdList
  = WorkspaceIdList (Array WorkspaceId)
```

##### Instances
``` purescript
Newtype WorkspaceIdList _
```

#### `WorkspaceList`

``` purescript
newtype WorkspaceList
  = WorkspaceList (Array Workspace)
```

##### Instances
``` purescript
Newtype WorkspaceList _
```

#### `WorkspaceProperties`

``` purescript
newtype WorkspaceProperties
  = WorkspaceProperties { "RunningMode" :: NullOrUndefined (RunningMode), "RunningModeAutoStopTimeoutInMinutes" :: NullOrUndefined (RunningModeAutoStopTimeoutInMinutes), "RootVolumeSizeGib" :: NullOrUndefined (RootVolumeSizeGib), "UserVolumeSizeGib" :: NullOrUndefined (UserVolumeSizeGib), "ComputeTypeName" :: NullOrUndefined (Compute) }
```

<p>Information about a WorkSpace.</p>

##### Instances
``` purescript
Newtype WorkspaceProperties _
```

#### `WorkspaceRequest`

``` purescript
newtype WorkspaceRequest
  = WorkspaceRequest { "DirectoryId" :: DirectoryId, "UserName" :: UserName, "BundleId" :: BundleId, "VolumeEncryptionKey" :: NullOrUndefined (VolumeEncryptionKey), "UserVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject), "RootVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject), "WorkspaceProperties" :: NullOrUndefined (WorkspaceProperties), "Tags" :: NullOrUndefined (TagList) }
```

<p>Information used to create a WorkSpace.</p>

##### Instances
``` purescript
Newtype WorkspaceRequest _
```

#### `WorkspaceRequestList`

``` purescript
newtype WorkspaceRequestList
  = WorkspaceRequestList (Array WorkspaceRequest)
```

##### Instances
``` purescript
Newtype WorkspaceRequestList _
```

#### `WorkspaceState`

``` purescript
newtype WorkspaceState
  = WorkspaceState String
```

##### Instances
``` purescript
Newtype WorkspaceState _
```


