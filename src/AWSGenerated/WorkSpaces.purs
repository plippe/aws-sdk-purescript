

-- | <fullname>Amazon WorkSpaces Service</fullname> <p>Amazon WorkSpaces enables you to provision virtual, cloud-based Microsoft Windows desktops for your users.</p>
module AWS.WorkSpaces where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
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
derive instance newtypeARN :: Newtype ARN _


-- | <p>The user is not authorized to access a resource.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _


newtype Alias = Alias String
derive instance newtypeAlias :: Newtype Alias _


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _


newtype BundleId = BundleId String
derive instance newtypeBundleId :: Newtype BundleId _


newtype BundleIdList = BundleIdList (Array BundleId)
derive instance newtypeBundleIdList :: Newtype BundleIdList _


newtype BundleList = BundleList (Array WorkspaceBundle)
derive instance newtypeBundleList :: Newtype BundleList _


newtype BundleOwner = BundleOwner String
derive instance newtypeBundleOwner :: Newtype BundleOwner _


newtype Compute = Compute String
derive instance newtypeCompute :: Newtype Compute _


-- | <p>Information about the compute type.</p>
newtype ComputeType = ComputeType 
  { "Name" :: NullOrUndefined (Compute)
  }
derive instance newtypeComputeType :: Newtype ComputeType _


newtype ComputerName = ComputerName String
derive instance newtypeComputerName :: Newtype ComputerName _


newtype ConnectionState = ConnectionState String
derive instance newtypeConnectionState :: Newtype ConnectionState _


newtype CreateTagsRequest = CreateTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  , "Tags" :: (TagList)
  }
derive instance newtypeCreateTagsRequest :: Newtype CreateTagsRequest _


newtype CreateTagsResult = CreateTagsResult 
  { 
  }
derive instance newtypeCreateTagsResult :: Newtype CreateTagsResult _


newtype CreateWorkspacesRequest = CreateWorkspacesRequest 
  { "Workspaces" :: (WorkspaceRequestList)
  }
derive instance newtypeCreateWorkspacesRequest :: Newtype CreateWorkspacesRequest _


newtype CreateWorkspacesResult = CreateWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedCreateWorkspaceRequests)
  , "PendingRequests" :: NullOrUndefined (WorkspaceList)
  }
derive instance newtypeCreateWorkspacesResult :: Newtype CreateWorkspacesResult _


newtype DefaultOu = DefaultOu String
derive instance newtypeDefaultOu :: Newtype DefaultOu _


-- | <p>Information about defaults used to create a WorkSpace.</p>
newtype DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties 
  { "EnableWorkDocs" :: NullOrUndefined (BooleanObject)
  , "EnableInternetAccess" :: NullOrUndefined (BooleanObject)
  , "DefaultOu" :: NullOrUndefined (DefaultOu)
  , "CustomSecurityGroupId" :: NullOrUndefined (SecurityGroupId)
  , "UserEnabledAsLocalAdministrator" :: NullOrUndefined (BooleanObject)
  }
derive instance newtypeDefaultWorkspaceCreationProperties :: Newtype DefaultWorkspaceCreationProperties _


newtype DeleteTagsRequest = DeleteTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeDeleteTagsRequest :: Newtype DeleteTagsRequest _


newtype DeleteTagsResult = DeleteTagsResult 
  { 
  }
derive instance newtypeDeleteTagsResult :: Newtype DeleteTagsResult _


newtype DescribeTagsRequest = DescribeTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  }
derive instance newtypeDescribeTagsRequest :: Newtype DescribeTagsRequest _


newtype DescribeTagsResult = DescribeTagsResult 
  { "TagList" :: NullOrUndefined (TagList)
  }
derive instance newtypeDescribeTagsResult :: Newtype DescribeTagsResult _


newtype DescribeWorkspaceBundlesRequest = DescribeWorkspaceBundlesRequest 
  { "BundleIds" :: NullOrUndefined (BundleIdList)
  , "Owner" :: NullOrUndefined (BundleOwner)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspaceBundlesRequest :: Newtype DescribeWorkspaceBundlesRequest _


newtype DescribeWorkspaceBundlesResult = DescribeWorkspaceBundlesResult 
  { "Bundles" :: NullOrUndefined (BundleList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspaceBundlesResult :: Newtype DescribeWorkspaceBundlesResult _


newtype DescribeWorkspaceDirectoriesRequest = DescribeWorkspaceDirectoriesRequest 
  { "DirectoryIds" :: NullOrUndefined (DirectoryIdList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspaceDirectoriesRequest :: Newtype DescribeWorkspaceDirectoriesRequest _


newtype DescribeWorkspaceDirectoriesResult = DescribeWorkspaceDirectoriesResult 
  { "Directories" :: NullOrUndefined (DirectoryList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspaceDirectoriesResult :: Newtype DescribeWorkspaceDirectoriesResult _


newtype DescribeWorkspacesConnectionStatusRequest = DescribeWorkspacesConnectionStatusRequest 
  { "WorkspaceIds" :: NullOrUndefined (WorkspaceIdList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspacesConnectionStatusRequest :: Newtype DescribeWorkspacesConnectionStatusRequest _


newtype DescribeWorkspacesConnectionStatusResult = DescribeWorkspacesConnectionStatusResult 
  { "WorkspacesConnectionStatus" :: NullOrUndefined (WorkspaceConnectionStatusList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspacesConnectionStatusResult :: Newtype DescribeWorkspacesConnectionStatusResult _


newtype DescribeWorkspacesRequest = DescribeWorkspacesRequest 
  { "WorkspaceIds" :: NullOrUndefined (WorkspaceIdList)
  , "DirectoryId" :: NullOrUndefined (DirectoryId)
  , "UserName" :: NullOrUndefined (UserName)
  , "BundleId" :: NullOrUndefined (BundleId)
  , "Limit" :: NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspacesRequest :: Newtype DescribeWorkspacesRequest _


newtype DescribeWorkspacesResult = DescribeWorkspacesResult 
  { "Workspaces" :: NullOrUndefined (WorkspaceList)
  , "NextToken" :: NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspacesResult :: Newtype DescribeWorkspacesResult _


newtype Description = Description String
derive instance newtypeDescription :: Newtype Description _


newtype DirectoryId = DirectoryId String
derive instance newtypeDirectoryId :: Newtype DirectoryId _


newtype DirectoryIdList = DirectoryIdList (Array DirectoryId)
derive instance newtypeDirectoryIdList :: Newtype DirectoryIdList _


newtype DirectoryList = DirectoryList (Array WorkspaceDirectory)
derive instance newtypeDirectoryList :: Newtype DirectoryList _


newtype DirectoryName = DirectoryName String
derive instance newtypeDirectoryName :: Newtype DirectoryName _


newtype DnsIpAddresses = DnsIpAddresses (Array IpAddress)
derive instance newtypeDnsIpAddresses :: Newtype DnsIpAddresses _


newtype ErrorType = ErrorType String
derive instance newtypeErrorType :: Newtype ErrorType _


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _


-- | <p>Information about a WorkSpace that could not be created.</p>
newtype FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest 
  { "WorkspaceRequest" :: NullOrUndefined (WorkspaceRequest)
  , "ErrorCode" :: NullOrUndefined (ErrorType)
  , "ErrorMessage" :: NullOrUndefined (Description)
  }
derive instance newtypeFailedCreateWorkspaceRequest :: Newtype FailedCreateWorkspaceRequest _


newtype FailedCreateWorkspaceRequests = FailedCreateWorkspaceRequests (Array FailedCreateWorkspaceRequest)
derive instance newtypeFailedCreateWorkspaceRequests :: Newtype FailedCreateWorkspaceRequests _


newtype FailedRebootWorkspaceRequests = FailedRebootWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedRebootWorkspaceRequests :: Newtype FailedRebootWorkspaceRequests _


newtype FailedRebuildWorkspaceRequests = FailedRebuildWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedRebuildWorkspaceRequests :: Newtype FailedRebuildWorkspaceRequests _


newtype FailedStartWorkspaceRequests = FailedStartWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedStartWorkspaceRequests :: Newtype FailedStartWorkspaceRequests _


newtype FailedStopWorkspaceRequests = FailedStopWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedStopWorkspaceRequests :: Newtype FailedStopWorkspaceRequests _


newtype FailedTerminateWorkspaceRequests = FailedTerminateWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedTerminateWorkspaceRequests :: Newtype FailedTerminateWorkspaceRequests _


-- | <p>Information about a WorkSpace that could not be rebooted (<a>RebootWorkspaces</a>), rebuilt (<a>RebuildWorkspaces</a>), terminated (<a>TerminateWorkspaces</a>), started (<a>StartWorkspaces</a>), or stopped (<a>StopWorkspaces</a>).</p>
newtype FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  , "ErrorCode" :: NullOrUndefined (ErrorType)
  , "ErrorMessage" :: NullOrUndefined (Description)
  }
derive instance newtypeFailedWorkspaceChangeRequest :: Newtype FailedWorkspaceChangeRequest _


-- | <p>One or more parameter values are not valid.</p>
newtype InvalidParameterValuesException = InvalidParameterValuesException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidParameterValuesException :: Newtype InvalidParameterValuesException _


-- | <p>The state of the WorkSpace is not valid for this operation.</p>
newtype InvalidResourceStateException = InvalidResourceStateException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidResourceStateException :: Newtype InvalidResourceStateException _


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _


newtype Limit = Limit Int
derive instance newtypeLimit :: Newtype Limit _


newtype ModificationResourceEnum = ModificationResourceEnum String
derive instance newtypeModificationResourceEnum :: Newtype ModificationResourceEnum _


-- | <p>Information about a WorkSpace modification.</p>
newtype ModificationState = ModificationState 
  { "Resource" :: NullOrUndefined (ModificationResourceEnum)
  , "State" :: NullOrUndefined (ModificationStateEnum)
  }
derive instance newtypeModificationState :: Newtype ModificationState _


newtype ModificationStateEnum = ModificationStateEnum String
derive instance newtypeModificationStateEnum :: Newtype ModificationStateEnum _


newtype ModificationStateList = ModificationStateList (Array ModificationState)
derive instance newtypeModificationStateList :: Newtype ModificationStateList _


newtype ModifyWorkspacePropertiesRequest = ModifyWorkspacePropertiesRequest 
  { "WorkspaceId" :: (WorkspaceId)
  , "WorkspaceProperties" :: (WorkspaceProperties)
  }
derive instance newtypeModifyWorkspacePropertiesRequest :: Newtype ModifyWorkspacePropertiesRequest _


newtype ModifyWorkspacePropertiesResult = ModifyWorkspacePropertiesResult 
  { 
  }
derive instance newtypeModifyWorkspacePropertiesResult :: Newtype ModifyWorkspacePropertiesResult _


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _


-- | <p>The properties of this WorkSpace are currently being modified. Try again in a moment.</p>
newtype OperationInProgressException = OperationInProgressException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeOperationInProgressException :: Newtype OperationInProgressException _


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _


-- | <p>Information used to reboot a WorkSpace.</p>
newtype RebootRequest = RebootRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }
derive instance newtypeRebootRequest :: Newtype RebootRequest _


newtype RebootWorkspaceRequests = RebootWorkspaceRequests (Array RebootRequest)
derive instance newtypeRebootWorkspaceRequests :: Newtype RebootWorkspaceRequests _


newtype RebootWorkspacesRequest = RebootWorkspacesRequest 
  { "RebootWorkspaceRequests" :: (RebootWorkspaceRequests)
  }
derive instance newtypeRebootWorkspacesRequest :: Newtype RebootWorkspacesRequest _


newtype RebootWorkspacesResult = RebootWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedRebootWorkspaceRequests)
  }
derive instance newtypeRebootWorkspacesResult :: Newtype RebootWorkspacesResult _


-- | <p>Information used to rebuild a WorkSpace.</p>
newtype RebuildRequest = RebuildRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }
derive instance newtypeRebuildRequest :: Newtype RebuildRequest _


newtype RebuildWorkspaceRequests = RebuildWorkspaceRequests (Array RebuildRequest)
derive instance newtypeRebuildWorkspaceRequests :: Newtype RebuildWorkspaceRequests _


newtype RebuildWorkspacesRequest = RebuildWorkspacesRequest 
  { "RebuildWorkspaceRequests" :: (RebuildWorkspaceRequests)
  }
derive instance newtypeRebuildWorkspacesRequest :: Newtype RebuildWorkspacesRequest _


newtype RebuildWorkspacesResult = RebuildWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedRebuildWorkspaceRequests)
  }
derive instance newtypeRebuildWorkspacesResult :: Newtype RebuildWorkspacesResult _


newtype RegistrationCode = RegistrationCode String
derive instance newtypeRegistrationCode :: Newtype RegistrationCode _


-- | <p>Your resource limits have been exceeded.</p>
newtype ResourceLimitExceededException = ResourceLimitExceededException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeResourceLimitExceededException :: Newtype ResourceLimitExceededException _


-- | <p>The resource could not be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  , "ResourceId" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _


-- | <p>The specified resource is not available.</p>
newtype ResourceUnavailableException = ResourceUnavailableException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  , "ResourceId" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeResourceUnavailableException :: Newtype ResourceUnavailableException _


-- | <p>Information about the root volume for a WorkSpace bundle.</p>
newtype RootStorage = RootStorage 
  { "Capacity" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeRootStorage :: Newtype RootStorage _


newtype RootVolumeSizeGib = RootVolumeSizeGib Int
derive instance newtypeRootVolumeSizeGib :: Newtype RootVolumeSizeGib _


newtype RunningMode = RunningMode String
derive instance newtypeRunningMode :: Newtype RunningMode _


newtype RunningModeAutoStopTimeoutInMinutes = RunningModeAutoStopTimeoutInMinutes Int
derive instance newtypeRunningModeAutoStopTimeoutInMinutes :: Newtype RunningModeAutoStopTimeoutInMinutes _


newtype SecurityGroupId = SecurityGroupId String
derive instance newtypeSecurityGroupId :: Newtype SecurityGroupId _


-- | <p>Information used to start a WorkSpace.</p>
newtype StartRequest = StartRequest 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  }
derive instance newtypeStartRequest :: Newtype StartRequest _


newtype StartWorkspaceRequests = StartWorkspaceRequests (Array StartRequest)
derive instance newtypeStartWorkspaceRequests :: Newtype StartWorkspaceRequests _


newtype StartWorkspacesRequest = StartWorkspacesRequest 
  { "StartWorkspaceRequests" :: (StartWorkspaceRequests)
  }
derive instance newtypeStartWorkspacesRequest :: Newtype StartWorkspacesRequest _


newtype StartWorkspacesResult = StartWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedStartWorkspaceRequests)
  }
derive instance newtypeStartWorkspacesResult :: Newtype StartWorkspacesResult _


-- | <p>Information used to stop a WorkSpace.</p>
newtype StopRequest = StopRequest 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  }
derive instance newtypeStopRequest :: Newtype StopRequest _


newtype StopWorkspaceRequests = StopWorkspaceRequests (Array StopRequest)
derive instance newtypeStopWorkspaceRequests :: Newtype StopWorkspaceRequests _


newtype StopWorkspacesRequest = StopWorkspacesRequest 
  { "StopWorkspaceRequests" :: (StopWorkspaceRequests)
  }
derive instance newtypeStopWorkspacesRequest :: Newtype StopWorkspacesRequest _


newtype StopWorkspacesResult = StopWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedStopWorkspaceRequests)
  }
derive instance newtypeStopWorkspacesResult :: Newtype StopWorkspacesResult _


newtype SubnetId = SubnetId String
derive instance newtypeSubnetId :: Newtype SubnetId _


newtype SubnetIds = SubnetIds (Array SubnetId)
derive instance newtypeSubnetIds :: Newtype SubnetIds _


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined (TagValue)
  }
derive instance newtypeTag :: Newtype Tag _


newtype TagKey = TagKey String
derive instance newtypeTagKey :: Newtype TagKey _


newtype TagKeyList = TagKeyList (Array NonEmptyString)
derive instance newtypeTagKeyList :: Newtype TagKeyList _


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _


newtype TagValue = TagValue String
derive instance newtypeTagValue :: Newtype TagValue _


-- | <p>Information used to terminate a WorkSpace.</p>
newtype TerminateRequest = TerminateRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }
derive instance newtypeTerminateRequest :: Newtype TerminateRequest _


newtype TerminateWorkspaceRequests = TerminateWorkspaceRequests (Array TerminateRequest)
derive instance newtypeTerminateWorkspaceRequests :: Newtype TerminateWorkspaceRequests _


newtype TerminateWorkspacesRequest = TerminateWorkspacesRequest 
  { "TerminateWorkspaceRequests" :: (TerminateWorkspaceRequests)
  }
derive instance newtypeTerminateWorkspacesRequest :: Newtype TerminateWorkspacesRequest _


newtype TerminateWorkspacesResult = TerminateWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined (FailedTerminateWorkspaceRequests)
  }
derive instance newtypeTerminateWorkspacesResult :: Newtype TerminateWorkspacesResult _


-- | <p>The configuration of this WorkSpace is not supported for this operation. For more information, see the <a href="http://docs.aws.amazon.com/workspaces/latest/adminguide/">Amazon WorkSpaces Administration Guide</a>. </p>
newtype UnsupportedWorkspaceConfigurationException = UnsupportedWorkspaceConfigurationException 
  { "Message'" :: NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeUnsupportedWorkspaceConfigurationException :: Newtype UnsupportedWorkspaceConfigurationException _


newtype UserName = UserName String
derive instance newtypeUserName :: Newtype UserName _


-- | <p>Information about the user storage for a WorkSpace bundle.</p>
newtype UserStorage = UserStorage 
  { "Capacity" :: NullOrUndefined (NonEmptyString)
  }
derive instance newtypeUserStorage :: Newtype UserStorage _


newtype UserVolumeSizeGib = UserVolumeSizeGib Int
derive instance newtypeUserVolumeSizeGib :: Newtype UserVolumeSizeGib _


newtype VolumeEncryptionKey = VolumeEncryptionKey String
derive instance newtypeVolumeEncryptionKey :: Newtype VolumeEncryptionKey _


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
derive instance newtypeWorkspace :: Newtype Workspace _


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
derive instance newtypeWorkspaceBundle :: Newtype WorkspaceBundle _


-- | <p>Describes the connection status of a WorkSpace.</p>
newtype WorkspaceConnectionStatus = WorkspaceConnectionStatus 
  { "WorkspaceId" :: NullOrUndefined (WorkspaceId)
  , "ConnectionState" :: NullOrUndefined (ConnectionState)
  , "ConnectionStateCheckTimestamp" :: NullOrUndefined (Number)
  , "LastKnownUserConnectionTimestamp" :: NullOrUndefined (Number)
  }
derive instance newtypeWorkspaceConnectionStatus :: Newtype WorkspaceConnectionStatus _


newtype WorkspaceConnectionStatusList = WorkspaceConnectionStatusList (Array WorkspaceConnectionStatus)
derive instance newtypeWorkspaceConnectionStatusList :: Newtype WorkspaceConnectionStatusList _


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
derive instance newtypeWorkspaceDirectory :: Newtype WorkspaceDirectory _


newtype WorkspaceDirectoryState = WorkspaceDirectoryState String
derive instance newtypeWorkspaceDirectoryState :: Newtype WorkspaceDirectoryState _


newtype WorkspaceDirectoryType = WorkspaceDirectoryType String
derive instance newtypeWorkspaceDirectoryType :: Newtype WorkspaceDirectoryType _


newtype WorkspaceErrorCode = WorkspaceErrorCode String
derive instance newtypeWorkspaceErrorCode :: Newtype WorkspaceErrorCode _


newtype WorkspaceId = WorkspaceId String
derive instance newtypeWorkspaceId :: Newtype WorkspaceId _


newtype WorkspaceIdList = WorkspaceIdList (Array WorkspaceId)
derive instance newtypeWorkspaceIdList :: Newtype WorkspaceIdList _


newtype WorkspaceList = WorkspaceList (Array Workspace)
derive instance newtypeWorkspaceList :: Newtype WorkspaceList _


-- | <p>Information about a WorkSpace.</p>
newtype WorkspaceProperties = WorkspaceProperties 
  { "RunningMode" :: NullOrUndefined (RunningMode)
  , "RunningModeAutoStopTimeoutInMinutes" :: NullOrUndefined (RunningModeAutoStopTimeoutInMinutes)
  , "RootVolumeSizeGib" :: NullOrUndefined (RootVolumeSizeGib)
  , "UserVolumeSizeGib" :: NullOrUndefined (UserVolumeSizeGib)
  , "ComputeTypeName" :: NullOrUndefined (Compute)
  }
derive instance newtypeWorkspaceProperties :: Newtype WorkspaceProperties _


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
derive instance newtypeWorkspaceRequest :: Newtype WorkspaceRequest _


newtype WorkspaceRequestList = WorkspaceRequestList (Array WorkspaceRequest)
derive instance newtypeWorkspaceRequestList :: Newtype WorkspaceRequestList _


newtype WorkspaceState = WorkspaceState String
derive instance newtypeWorkspaceState :: Newtype WorkspaceState _
