

-- | <fullname>Amazon WorkSpaces Service</fullname> <p>Amazon WorkSpaces enables you to provision virtual, cloud-based Microsoft Windows desktops for your users.</p>
module AWS.WorkSpaces where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "WorkSpaces" :: String


-- | <p>Creates tags for the specified WorkSpace.</p>
createTags :: forall eff. CreateTagsRequest -> Aff (err :: AWS.RequestError | eff) CreateTagsResult
createTags = AWS.request serviceName "CreateTags" 


-- | <p>Creates one or more WorkSpaces.</p> <p>This operation is asynchronous and returns before the WorkSpaces are created.</p>
createWorkspaces :: forall eff. CreateWorkspacesRequest -> Aff (err :: AWS.RequestError | eff) CreateWorkspacesResult
createWorkspaces = AWS.request serviceName "CreateWorkspaces" 


-- | <p>Deletes the specified tags from a WorkSpace.</p>
deleteTags :: forall eff. DeleteTagsRequest -> Aff (err :: AWS.RequestError | eff) DeleteTagsResult
deleteTags = AWS.request serviceName "DeleteTags" 


-- | <p>Describes the tags for the specified WorkSpace.</p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (err :: AWS.RequestError | eff) DescribeTagsResult
describeTags = AWS.request serviceName "DescribeTags" 


-- | <p>Describes the available WorkSpace bundles.</p> <p>You can filter the results using either bundle ID or owner, but not both.</p>
describeWorkspaceBundles :: forall eff. DescribeWorkspaceBundlesRequest -> Aff (err :: AWS.RequestError | eff) DescribeWorkspaceBundlesResult
describeWorkspaceBundles = AWS.request serviceName "DescribeWorkspaceBundles" 


-- | <p>Describes the available AWS Directory Service directories that are registered with Amazon WorkSpaces.</p>
describeWorkspaceDirectories :: forall eff. DescribeWorkspaceDirectoriesRequest -> Aff (err :: AWS.RequestError | eff) DescribeWorkspaceDirectoriesResult
describeWorkspaceDirectories = AWS.request serviceName "DescribeWorkspaceDirectories" 


-- | <p>Describes the specified WorkSpaces.</p> <p>You can filter the results using bundle ID, directory ID, or owner, but you can specify only one filter at a time.</p>
describeWorkspaces :: forall eff. DescribeWorkspacesRequest -> Aff (err :: AWS.RequestError | eff) DescribeWorkspacesResult
describeWorkspaces = AWS.request serviceName "DescribeWorkspaces" 


-- | <p>Describes the connection status of the specified WorkSpaces.</p>
describeWorkspacesConnectionStatus :: forall eff. DescribeWorkspacesConnectionStatusRequest -> Aff (err :: AWS.RequestError | eff) DescribeWorkspacesConnectionStatusResult
describeWorkspacesConnectionStatus = AWS.request serviceName "DescribeWorkspacesConnectionStatus" 


-- | <p>Modifies the specified WorkSpace properties.</p>
modifyWorkspaceProperties :: forall eff. ModifyWorkspacePropertiesRequest -> Aff (err :: AWS.RequestError | eff) ModifyWorkspacePropertiesResult
modifyWorkspaceProperties = AWS.request serviceName "ModifyWorkspaceProperties" 


-- | <p>Reboots the specified WorkSpaces.</p> <p>You cannot reboot a WorkSpace unless its state is <code>AVAILABLE</code>, <code>IMPAIRED</code>, or <code>INOPERABLE</code>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have rebooted.</p>
rebootWorkspaces :: forall eff. RebootWorkspacesRequest -> Aff (err :: AWS.RequestError | eff) RebootWorkspacesResult
rebootWorkspaces = AWS.request serviceName "RebootWorkspaces" 


-- | <p>Rebuilds the specified WorkSpaces.</p> <p>You cannot rebuild a WorkSpace unless its state is <code>AVAILABLE</code> or <code>ERROR</code>.</p> <p>Rebuilding a WorkSpace is a potentially destructive action that can result in the loss of data. For more information, see <a href="http://docs.aws.amazon.com/workspaces/latest/adminguide/reset-workspace.html">Rebuild a WorkSpace</a>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have been completely rebuilt.</p>
rebuildWorkspaces :: forall eff. RebuildWorkspacesRequest -> Aff (err :: AWS.RequestError | eff) RebuildWorkspacesResult
rebuildWorkspaces = AWS.request serviceName "RebuildWorkspaces" 


-- | <p>Starts the specified WorkSpaces.</p> <p>You cannot start a WorkSpace unless it has a running mode of <code>AutoStop</code> and a state of <code>STOPPED</code>.</p>
startWorkspaces :: forall eff. StartWorkspacesRequest -> Aff (err :: AWS.RequestError | eff) StartWorkspacesResult
startWorkspaces = AWS.request serviceName "StartWorkspaces" 


-- | <p> Stops the specified WorkSpaces.</p> <p>You cannot stop a WorkSpace unless it has a running mode of <code>AutoStop</code> and a state of <code>AVAILABLE</code>, <code>IMPAIRED</code>, <code>UNHEALTHY</code>, or <code>ERROR</code>.</p>
stopWorkspaces :: forall eff. StopWorkspacesRequest -> Aff (err :: AWS.RequestError | eff) StopWorkspacesResult
stopWorkspaces = AWS.request serviceName "StopWorkspaces" 


-- | <p>Terminates the specified WorkSpaces.</p> <p>Terminating a WorkSpace is a permanent action and cannot be undone. The user's data is destroyed. If you need to archive any user data, contact Amazon Web Services before terminating the WorkSpace.</p> <p>You can terminate a WorkSpace that is in any state except <code>SUSPENDED</code>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have been completely terminated.</p>
terminateWorkspaces :: forall eff. TerminateWorkspacesRequest -> Aff (err :: AWS.RequestError | eff) TerminateWorkspacesResult
terminateWorkspaces = AWS.request serviceName "TerminateWorkspaces" 


newtype ARN = ARN String


-- | <p>The user is not authorized to access a resource.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }


newtype Alias = Alias String


newtype BooleanObject = BooleanObject Boolean


newtype BundleId = BundleId String


newtype BundleIdList = BundleIdList (Array BundleId)


newtype BundleList = BundleList (Array WorkspaceBundle)


newtype BundleOwner = BundleOwner String


newtype Compute = Compute String


-- | <p>Information about the compute type.</p>
newtype ComputeType = ComputeType 
  { "Name" :: NullOrUndefined (Compute)
  }


newtype ComputerName = ComputerName String


newtype ConnectionState = ConnectionState String


newtype CreateTagsRequest = CreateTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  , "Tags" :: (TagList)
  }


newtype CreateTagsResult = CreateTagsResult 
  { 
  }


newtype CreateWorkspacesRequest = CreateWorkspacesRequest 
  { "Workspaces" :: (WorkspaceRequestList)
  }


newtype CreateWorkspacesResult = CreateWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedCreateWorkspaceRequests)
  , "PendingRequests" :: NullOrUndefined (WorkspaceList)
  }


newtype DefaultOu = DefaultOu String


-- | <p>Information about defaults used to create a WorkSpace.</p>
newtype DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties 
  { "EnableWorkDocs" :: NullOrUndefined (BooleanObject)
  , "EnableInternetAccess" :: NullOrUndefined (BooleanObject)
  , "DefaultOu" :: NullOrUndefined (DefaultOu)
  , "CustomSecurityGroupId" :: NullOrUndefined (SecurityGroupId)
  , "UserEnabledAsLocalAdministrator" :: NullOrUndefined (BooleanObject)
  }


newtype DeleteTagsRequest = DeleteTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  , "TagKeys" :: (TagKeyList)
  }


newtype DeleteTagsResult = DeleteTagsResult 
  { 
  }


newtype DescribeTagsRequest = DescribeTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  }


newtype DescribeTagsResult = DescribeTagsResult 
  { "TagList" :: NullOrUndefined (TagList)
  }


newtype DescribeWorkspaceBundlesRequest = DescribeWorkspaceBundlesRequest 
  { "BundleIds" :: NullOrUndefined (BundleIdList)
  , "Owner" :: NullOrUndefined (BundleOwner)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype DescribeWorkspaceBundlesResult = DescribeWorkspaceBundlesResult 
  { "Bundles" :: NullOrUndefined (BundleList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype DescribeWorkspaceDirectoriesRequest = DescribeWorkspaceDirectoriesRequest 
  { "DirectoryIds" :: NullOrUndefined (DirectoryIdList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype DescribeWorkspaceDirectoriesResult = DescribeWorkspaceDirectoriesResult 
  { "Directories" :: NullOrUndefined (DirectoryList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype DescribeWorkspacesConnectionStatusRequest = DescribeWorkspacesConnectionStatusRequest 
  { "WorkspaceIds" :: NullOrUndefined (WorkspaceIdList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype DescribeWorkspacesConnectionStatusResult = DescribeWorkspacesConnectionStatusResult 
  { "WorkspacesConnectionStatus" :: NullOrUndefined (WorkspaceConnectionStatusList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype DescribeWorkspacesRequest = DescribeWorkspacesRequest 
  { "WorkspaceIds" :: NullOrUndefined (WorkspaceIdList)
  , "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "UserName" :: NullOrUndefined (UserName)
  , "BundleId" :: NullOrUndefined (BundleId)
  , "Limit" :: NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype DescribeWorkspacesResult = DescribeWorkspacesResult 
  { "Workspaces" :: NullOrUndefined (WorkspaceList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }


newtype Description = Description String


newtype DirectoryId = DirectoryId String


newtype DirectoryIdList = DirectoryIdList (Array DirectoryId)


newtype DirectoryList = DirectoryList (Array WorkspaceDirectory)


newtype DirectoryName = DirectoryName String


newtype DnsIpAddresses = DnsIpAddresses (Array IpAddress)


newtype ErrorType = ErrorType String


newtype ExceptionMessage = ExceptionMessage String


-- | <p>Information about a WorkSpace that could not be created.</p>
newtype FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest 
  { "WorkspaceRequest" :: NullOrUndefined (WorkspaceRequest)
  , "ErrorCode" :: NullOrUndefined (ErrorType)
  , "ErrorMessage" :: NullOrUndefined (Description)
  }


newtype FailedCreateWorkspaceRequests = FailedCreateWorkspaceRequests (Array FailedCreateWorkspaceRequest)


newtype FailedRebootWorkspaceRequests = FailedRebootWorkspaceRequests (Array FailedWorkspaceChangeRequest)


newtype FailedRebuildWorkspaceRequests = FailedRebuildWorkspaceRequests (Array FailedWorkspaceChangeRequest)


newtype FailedStartWorkspaceRequests = FailedStartWorkspaceRequests (Array FailedWorkspaceChangeRequest)


newtype FailedStopWorkspaceRequests = FailedStopWorkspaceRequests (Array FailedWorkspaceChangeRequest)


newtype FailedTerminateWorkspaceRequests = FailedTerminateWorkspaceRequests (Array FailedWorkspaceChangeRequest)


-- | <p>Information about a WorkSpace that could not be rebooted (<a>RebootWorkspaces</a>), rebuilt (<a>RebuildWorkspaces</a>), terminated (<a>TerminateWorkspaces</a>), started (<a>StartWorkspaces</a>), or stopped (<a>StopWorkspaces</a>).</p>
newtype FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  , "ErrorCode" :: NullOrUndefined (ErrorType)
  , "ErrorMessage" :: NullOrUndefined (Description)
  }


-- | <p>One or more parameter values are not valid.</p>
newtype InvalidParameterValuesException = InvalidParameterValuesException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>The state of the WorkSpace is not valid for this operation.</p>
newtype InvalidResourceStateException = InvalidResourceStateException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }


newtype IpAddress = IpAddress String


newtype Limit = Limit Int


newtype ModificationResourceEnum = ModificationResourceEnum String


-- | <p>Information about a WorkSpace modification.</p>
newtype ModificationState = ModificationState 
  { "Resource" :: NullOrUndefined (ModificationResourceEnum)
  , "State" :: NullOrUndefined (ModificationStateEnum)
  }


newtype ModificationStateEnum = ModificationStateEnum String


newtype ModificationStateList = ModificationStateList (Array ModificationState)


newtype ModifyWorkspacePropertiesRequest = ModifyWorkspacePropertiesRequest 
  { "WorkspaceId" :: (WorkspaceId)
  , "WorkspaceProperties" :: (WorkspaceProperties)
  }


newtype ModifyWorkspacePropertiesResult = ModifyWorkspacePropertiesResult 
  { 
  }


newtype NonEmptyString = NonEmptyString String


-- | <p>The properties of this WorkSpace are currently being modified. Try again in a moment.</p>
newtype OperationInProgressException = OperationInProgressException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }


newtype PaginationToken = PaginationToken String


-- | <p>Information used to reboot a WorkSpace.</p>
newtype RebootRequest = RebootRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }


newtype RebootWorkspaceRequests = RebootWorkspaceRequests (Array RebootRequest)


newtype RebootWorkspacesRequest = RebootWorkspacesRequest 
  { "RebootWorkspaceRequests" :: (RebootWorkspaceRequests)
  }


newtype RebootWorkspacesResult = RebootWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedRebootWorkspaceRequests)
  }


-- | <p>Information used to rebuild a WorkSpace.</p>
newtype RebuildRequest = RebuildRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }


newtype RebuildWorkspaceRequests = RebuildWorkspaceRequests (Array RebuildRequest)


newtype RebuildWorkspacesRequest = RebuildWorkspacesRequest 
  { "RebuildWorkspaceRequests" :: (RebuildWorkspaceRequests)
  }


newtype RebuildWorkspacesResult = RebuildWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedRebuildWorkspaceRequests)
  }


newtype RegistrationCode = RegistrationCode String


-- | <p>Your resource limits have been exceeded.</p>
newtype ResourceLimitExceededException = ResourceLimitExceededException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }


-- | <p>The resource could not be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  , "ResourceId" :: NullOrUndefined (NonEmptyString)
  }


-- | <p>The specified resource is not available.</p>
newtype ResourceUnavailableException = ResourceUnavailableException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  , "ResourceId" :: NullOrUndefined (NonEmptyString)
  }


-- | <p>Information about the root volume for a WorkSpace bundle.</p>
newtype RootStorage = RootStorage 
  { "Capacity" :: NullOrUndefined (NonEmptyString)
  }


newtype RootVolumeSizeGib = RootVolumeSizeGib Int


newtype RunningMode = RunningMode String


newtype RunningModeAutoStopTimeoutInMinutes = RunningModeAutoStopTimeoutInMinutes Int


newtype SecurityGroupId = SecurityGroupId String


-- | <p>Information used to start a WorkSpace.</p>
newtype StartRequest = StartRequest 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  }


newtype StartWorkspaceRequests = StartWorkspaceRequests (Array StartRequest)


newtype StartWorkspacesRequest = StartWorkspacesRequest 
  { "StartWorkspaceRequests" :: (StartWorkspaceRequests)
  }


newtype StartWorkspacesResult = StartWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedStartWorkspaceRequests)
  }


-- | <p>Information used to stop a WorkSpace.</p>
newtype StopRequest = StopRequest 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  }


newtype StopWorkspaceRequests = StopWorkspaceRequests (Array StopRequest)


newtype StopWorkspacesRequest = StopWorkspacesRequest 
  { "StopWorkspaceRequests" :: (StopWorkspaceRequests)
  }


newtype StopWorkspacesResult = StopWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedStopWorkspaceRequests)
  }


newtype SubnetId = SubnetId String


newtype SubnetIds = SubnetIds (Array SubnetId)


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }


newtype TagKey = TagKey String


newtype TagKeyList = TagKeyList (Array NonEmptyString)


newtype TagList = TagList (Array Tag)


newtype TagValue = TagValue String


-- | <p>Information used to terminate a WorkSpace.</p>
newtype TerminateRequest = TerminateRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }


newtype TerminateWorkspaceRequests = TerminateWorkspaceRequests (Array TerminateRequest)


newtype TerminateWorkspacesRequest = TerminateWorkspacesRequest 
  { "TerminateWorkspaceRequests" :: (TerminateWorkspaceRequests)
  }


newtype TerminateWorkspacesResult = TerminateWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedTerminateWorkspaceRequests)
  }


-- | <p>The configuration of this WorkSpace is not supported for this operation. For more information, see the <a href="http://docs.aws.amazon.com/workspaces/latest/adminguide/">Amazon WorkSpaces Administration Guide</a>. </p>
newtype UnsupportedWorkspaceConfigurationException = UnsupportedWorkspaceConfigurationException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }


newtype UserName = UserName String


-- | <p>Information about the user storage for a WorkSpace bundle.</p>
newtype UserStorage = UserStorage 
  { "Capacity" :: NullOrUndefined (NonEmptyString)
  }


newtype UserVolumeSizeGib = UserVolumeSizeGib Int


newtype VolumeEncryptionKey = VolumeEncryptionKey String


-- | <p>Information about a WorkSpace.</p>
newtype Workspace = Workspace 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  , "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "UserName" :: NullOrUndefined (UserName)
  , "IpAddress" :: NullOrUndefined (IpAddress)
  , "State" :: NullOrUndefined (WorkspaceState)
  , "BundleId" :: NullOrUndefined (BundleId)
  , "SubnetId" :: NullOrUndefined (SubnetId)
  , "ErrorMessage" :: NullOrUndefined (Description)
  , "ErrorCode" :: NullOrUndefined (WorkspaceErrorCode)
  , "ComputerName" :: NullOrUndefined (ComputerName)
  , "VolumeEncryptionKey" :: NullOrUndefined (VolumeEncryptionKey)
  , "UserVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject)
  , "RootVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject)
  , "WorkspaceProperties" :: NullOrUndefined (WorkspaceProperties)
  , "ModificationStates" :: NullOrUndefined (ModificationStateList)
  }


-- | <p>Information about a WorkSpace bundle.</p>
newtype WorkspaceBundle = WorkspaceBundle 
  { "BundleId" :: NullOrUndefined (BundleId)
  , "Name" :: NullOrUndefined (NonEmptyString)
  , "Owner" :: NullOrUndefined (BundleOwner)
  , "Description" :: NullOrUndefined (Description)
  , "RootStorage" :: NullOrUndefined (RootStorage)
  , "UserStorage" :: NullOrUndefined (UserStorage)
  , "ComputeType" :: NullOrUndefined (ComputeType)
  }


-- | <p>Describes the connection status of a WorkSpace.</p>
newtype WorkspaceConnectionStatus = WorkspaceConnectionStatus 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  , "ConnectionState" :: NullOrUndefined (ConnectionState)
  , "ConnectionStateCheckTimestamp" :: NullOrUndefined (Number)
  , "LastKnownUserConnectionTimestamp" :: NullOrUndefined (Number)
  }


newtype WorkspaceConnectionStatusList = WorkspaceConnectionStatusList (Array WorkspaceConnectionStatus)


-- | <p>Contains information about an AWS Directory Service directory for use with Amazon WorkSpaces.</p>
newtype WorkspaceDirectory = WorkspaceDirectory 
  { "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "Alias" :: NullOrUndefined (Alias)
  , "DirectoryName" :: NullOrUndefined (DirectoryName)
  , "RegistrationCode" :: NullOrUndefined (RegistrationCode)
  , "SubnetIds" :: NullOrUndefined (SubnetIds)
  , "DnsIpAddresses" :: NullOrUndefined (DnsIpAddresses)
  , "CustomerUserName" :: NullOrUndefined (UserName)
  , "IamRoleId" :: NullOrUndefined (ARN)
  , "DirectoryType" :: NullOrUndefined (WorkspaceDirectoryType)
  , "WorkspaceSecurityGroupId" :: NullOrUndefined (SecurityGroupId)
  , "State" :: NullOrUndefined (WorkspaceDirectoryState)
  , "WorkspaceCreationProperties" :: NullOrUndefined (DefaultWorkspaceCreationProperties)
  }


newtype WorkspaceDirectoryState = WorkspaceDirectoryState String


newtype WorkspaceDirectoryType = WorkspaceDirectoryType String


newtype WorkspaceErrorCode = WorkspaceErrorCode String


newtype WorkspaceId = WorkspaceId String


newtype WorkspaceIdList = WorkspaceIdList (Array WorkspaceId)


newtype WorkspaceList = WorkspaceList (Array Workspace)


-- | <p>Information about a WorkSpace.</p>
newtype WorkspaceProperties = WorkspaceProperties 
  { "RunningMode" :: NullOrUndefined (RunningMode)
  , "RunningModeAutoStopTimeoutInMinutes" :: NullOrUndefined (RunningModeAutoStopTimeoutInMinutes)
  , "RootVolumeSizeGib" :: NullOrUndefined (RootVolumeSizeGib)
  , "UserVolumeSizeGib" :: NullOrUndefined (UserVolumeSizeGib)
  , "ComputeTypeName" :: NullOrUndefined (Compute)
  }


-- | <p>Information used to create a WorkSpace.</p>
newtype WorkspaceRequest = WorkspaceRequest 
  { "DirectoryId" :: (DirectoryId)
  , "UserName" :: (UserName)
  , "BundleId" :: (BundleId)
  , "VolumeEncryptionKey" :: NullOrUndefined (VolumeEncryptionKey)
  , "UserVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject)
  , "RootVolumeEncryptionEnabled" :: NullOrUndefined (BooleanObject)
  , "WorkspaceProperties" :: NullOrUndefined (WorkspaceProperties)
  , "Tags" :: NullOrUndefined (TagList)
  }


newtype WorkspaceRequestList = WorkspaceRequestList (Array WorkspaceRequest)


newtype WorkspaceState = WorkspaceState String
