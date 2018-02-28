

-- | <fullname>Amazon WorkSpaces Service</fullname> <p>Amazon WorkSpaces enables you to provision virtual, cloud-based Microsoft Windows desktops for your users.</p>
module AWS.WorkSpaces where

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

serviceName = "WorkSpaces" :: String


-- | <p>Creates tags for the specified WorkSpace.</p>
createTags :: forall eff. CreateTagsRequest -> Aff (exception :: EXCEPTION | eff) CreateTagsResult
createTags = Request.request serviceName "createTags" 


-- | <p>Creates one or more WorkSpaces.</p> <p>This operation is asynchronous and returns before the WorkSpaces are created.</p>
createWorkspaces :: forall eff. CreateWorkspacesRequest -> Aff (exception :: EXCEPTION | eff) CreateWorkspacesResult
createWorkspaces = Request.request serviceName "createWorkspaces" 


-- | <p>Deletes the specified tags from a WorkSpace.</p>
deleteTags :: forall eff. DeleteTagsRequest -> Aff (exception :: EXCEPTION | eff) DeleteTagsResult
deleteTags = Request.request serviceName "deleteTags" 


-- | <p>Describes the tags for the specified WorkSpace.</p>
describeTags :: forall eff. DescribeTagsRequest -> Aff (exception :: EXCEPTION | eff) DescribeTagsResult
describeTags = Request.request serviceName "describeTags" 


-- | <p>Describes the available WorkSpace bundles.</p> <p>You can filter the results using either bundle ID or owner, but not both.</p>
describeWorkspaceBundles :: forall eff. DescribeWorkspaceBundlesRequest -> Aff (exception :: EXCEPTION | eff) DescribeWorkspaceBundlesResult
describeWorkspaceBundles = Request.request serviceName "describeWorkspaceBundles" 


-- | <p>Describes the available AWS Directory Service directories that are registered with Amazon WorkSpaces.</p>
describeWorkspaceDirectories :: forall eff. DescribeWorkspaceDirectoriesRequest -> Aff (exception :: EXCEPTION | eff) DescribeWorkspaceDirectoriesResult
describeWorkspaceDirectories = Request.request serviceName "describeWorkspaceDirectories" 


-- | <p>Describes the specified WorkSpaces.</p> <p>You can filter the results using bundle ID, directory ID, or owner, but you can specify only one filter at a time.</p>
describeWorkspaces :: forall eff. DescribeWorkspacesRequest -> Aff (exception :: EXCEPTION | eff) DescribeWorkspacesResult
describeWorkspaces = Request.request serviceName "describeWorkspaces" 


-- | <p>Describes the connection status of the specified WorkSpaces.</p>
describeWorkspacesConnectionStatus :: forall eff. DescribeWorkspacesConnectionStatusRequest -> Aff (exception :: EXCEPTION | eff) DescribeWorkspacesConnectionStatusResult
describeWorkspacesConnectionStatus = Request.request serviceName "describeWorkspacesConnectionStatus" 


-- | <p>Modifies the specified WorkSpace properties.</p>
modifyWorkspaceProperties :: forall eff. ModifyWorkspacePropertiesRequest -> Aff (exception :: EXCEPTION | eff) ModifyWorkspacePropertiesResult
modifyWorkspaceProperties = Request.request serviceName "modifyWorkspaceProperties" 


-- | <p>Reboots the specified WorkSpaces.</p> <p>You cannot reboot a WorkSpace unless its state is <code>AVAILABLE</code>, <code>IMPAIRED</code>, or <code>INOPERABLE</code>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have rebooted.</p>
rebootWorkspaces :: forall eff. RebootWorkspacesRequest -> Aff (exception :: EXCEPTION | eff) RebootWorkspacesResult
rebootWorkspaces = Request.request serviceName "rebootWorkspaces" 


-- | <p>Rebuilds the specified WorkSpaces.</p> <p>You cannot rebuild a WorkSpace unless its state is <code>AVAILABLE</code> or <code>ERROR</code>.</p> <p>Rebuilding a WorkSpace is a potentially destructive action that can result in the loss of data. For more information, see <a href="http://docs.aws.amazon.com/workspaces/latest/adminguide/reset-workspace.html">Rebuild a WorkSpace</a>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have been completely rebuilt.</p>
rebuildWorkspaces :: forall eff. RebuildWorkspacesRequest -> Aff (exception :: EXCEPTION | eff) RebuildWorkspacesResult
rebuildWorkspaces = Request.request serviceName "rebuildWorkspaces" 


-- | <p>Starts the specified WorkSpaces.</p> <p>You cannot start a WorkSpace unless it has a running mode of <code>AutoStop</code> and a state of <code>STOPPED</code>.</p>
startWorkspaces :: forall eff. StartWorkspacesRequest -> Aff (exception :: EXCEPTION | eff) StartWorkspacesResult
startWorkspaces = Request.request serviceName "startWorkspaces" 


-- | <p> Stops the specified WorkSpaces.</p> <p>You cannot stop a WorkSpace unless it has a running mode of <code>AutoStop</code> and a state of <code>AVAILABLE</code>, <code>IMPAIRED</code>, <code>UNHEALTHY</code>, or <code>ERROR</code>.</p>
stopWorkspaces :: forall eff. StopWorkspacesRequest -> Aff (exception :: EXCEPTION | eff) StopWorkspacesResult
stopWorkspaces = Request.request serviceName "stopWorkspaces" 


-- | <p>Terminates the specified WorkSpaces.</p> <p>Terminating a WorkSpace is a permanent action and cannot be undone. The user's data is destroyed. If you need to archive any user data, contact Amazon Web Services before terminating the WorkSpace.</p> <p>You can terminate a WorkSpace that is in any state except <code>SUSPENDED</code>.</p> <p>This operation is asynchronous and returns before the WorkSpaces have been completely terminated.</p>
terminateWorkspaces :: forall eff. TerminateWorkspacesRequest -> Aff (exception :: EXCEPTION | eff) TerminateWorkspacesResult
terminateWorkspaces = Request.request serviceName "terminateWorkspaces" 


newtype ARN = ARN String
derive instance newtypeARN :: Newtype ARN _
derive instance repGenericARN :: Generic ARN _
instance showARN :: Show ARN where
  show = genericShow
instance decodeARN :: Decode ARN where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeARN :: Encode ARN where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user is not authorized to access a resource.</p>
newtype AccessDeniedException = AccessDeniedException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeAccessDeniedException :: Newtype AccessDeniedException _
derive instance repGenericAccessDeniedException :: Generic AccessDeniedException _
instance showAccessDeniedException :: Show AccessDeniedException where
  show = genericShow
instance decodeAccessDeniedException :: Decode AccessDeniedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAccessDeniedException :: Encode AccessDeniedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Alias = Alias String
derive instance newtypeAlias :: Newtype Alias _
derive instance repGenericAlias :: Generic Alias _
instance showAlias :: Show Alias where
  show = genericShow
instance decodeAlias :: Decode Alias where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAlias :: Encode Alias where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanObject = BooleanObject Boolean
derive instance newtypeBooleanObject :: Newtype BooleanObject _
derive instance repGenericBooleanObject :: Generic BooleanObject _
instance showBooleanObject :: Show BooleanObject where
  show = genericShow
instance decodeBooleanObject :: Decode BooleanObject where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanObject :: Encode BooleanObject where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BundleId = BundleId String
derive instance newtypeBundleId :: Newtype BundleId _
derive instance repGenericBundleId :: Generic BundleId _
instance showBundleId :: Show BundleId where
  show = genericShow
instance decodeBundleId :: Decode BundleId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleId :: Encode BundleId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BundleIdList = BundleIdList (Array BundleId)
derive instance newtypeBundleIdList :: Newtype BundleIdList _
derive instance repGenericBundleIdList :: Generic BundleIdList _
instance showBundleIdList :: Show BundleIdList where
  show = genericShow
instance decodeBundleIdList :: Decode BundleIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleIdList :: Encode BundleIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BundleList = BundleList (Array WorkspaceBundle)
derive instance newtypeBundleList :: Newtype BundleList _
derive instance repGenericBundleList :: Generic BundleList _
instance showBundleList :: Show BundleList where
  show = genericShow
instance decodeBundleList :: Decode BundleList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleList :: Encode BundleList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BundleOwner = BundleOwner String
derive instance newtypeBundleOwner :: Newtype BundleOwner _
derive instance repGenericBundleOwner :: Generic BundleOwner _
instance showBundleOwner :: Show BundleOwner where
  show = genericShow
instance decodeBundleOwner :: Decode BundleOwner where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBundleOwner :: Encode BundleOwner where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype Compute = Compute String
derive instance newtypeCompute :: Newtype Compute _
derive instance repGenericCompute :: Generic Compute _
instance showCompute :: Show Compute where
  show = genericShow
instance decodeCompute :: Decode Compute where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCompute :: Encode Compute where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the compute type.</p>
newtype ComputeType = ComputeType 
  { "Name" :: NullOrUndefined.NullOrUndefined (Compute)
  }
derive instance newtypeComputeType :: Newtype ComputeType _
derive instance repGenericComputeType :: Generic ComputeType _
instance showComputeType :: Show ComputeType where
  show = genericShow
instance decodeComputeType :: Decode ComputeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComputeType :: Encode ComputeType where
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


newtype ConnectionState = ConnectionState String
derive instance newtypeConnectionState :: Newtype ConnectionState _
derive instance repGenericConnectionState :: Generic ConnectionState _
instance showConnectionState :: Show ConnectionState where
  show = genericShow
instance decodeConnectionState :: Decode ConnectionState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConnectionState :: Encode ConnectionState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTagsRequest = CreateTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  , "Tags" :: (TagList)
  }
derive instance newtypeCreateTagsRequest :: Newtype CreateTagsRequest _
derive instance repGenericCreateTagsRequest :: Generic CreateTagsRequest _
instance showCreateTagsRequest :: Show CreateTagsRequest where
  show = genericShow
instance decodeCreateTagsRequest :: Decode CreateTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTagsRequest :: Encode CreateTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateTagsResult = CreateTagsResult Types.NoArguments
derive instance newtypeCreateTagsResult :: Newtype CreateTagsResult _
derive instance repGenericCreateTagsResult :: Generic CreateTagsResult _
instance showCreateTagsResult :: Show CreateTagsResult where
  show = genericShow
instance decodeCreateTagsResult :: Decode CreateTagsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateTagsResult :: Encode CreateTagsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateWorkspacesRequest = CreateWorkspacesRequest 
  { "Workspaces" :: (WorkspaceRequestList)
  }
derive instance newtypeCreateWorkspacesRequest :: Newtype CreateWorkspacesRequest _
derive instance repGenericCreateWorkspacesRequest :: Generic CreateWorkspacesRequest _
instance showCreateWorkspacesRequest :: Show CreateWorkspacesRequest where
  show = genericShow
instance decodeCreateWorkspacesRequest :: Decode CreateWorkspacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateWorkspacesRequest :: Encode CreateWorkspacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateWorkspacesResult = CreateWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined.NullOrUndefined (FailedCreateWorkspaceRequests)
  , "PendingRequests" :: NullOrUndefined.NullOrUndefined (WorkspaceList)
  }
derive instance newtypeCreateWorkspacesResult :: Newtype CreateWorkspacesResult _
derive instance repGenericCreateWorkspacesResult :: Generic CreateWorkspacesResult _
instance showCreateWorkspacesResult :: Show CreateWorkspacesResult where
  show = genericShow
instance decodeCreateWorkspacesResult :: Decode CreateWorkspacesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateWorkspacesResult :: Encode CreateWorkspacesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DefaultOu = DefaultOu String
derive instance newtypeDefaultOu :: Newtype DefaultOu _
derive instance repGenericDefaultOu :: Generic DefaultOu _
instance showDefaultOu :: Show DefaultOu where
  show = genericShow
instance decodeDefaultOu :: Decode DefaultOu where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultOu :: Encode DefaultOu where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about defaults used to create a WorkSpace.</p>
newtype DefaultWorkspaceCreationProperties = DefaultWorkspaceCreationProperties 
  { "EnableWorkDocs" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "EnableInternetAccess" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "DefaultOu" :: NullOrUndefined.NullOrUndefined (DefaultOu)
  , "CustomSecurityGroupId" :: NullOrUndefined.NullOrUndefined (SecurityGroupId)
  , "UserEnabledAsLocalAdministrator" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  }
derive instance newtypeDefaultWorkspaceCreationProperties :: Newtype DefaultWorkspaceCreationProperties _
derive instance repGenericDefaultWorkspaceCreationProperties :: Generic DefaultWorkspaceCreationProperties _
instance showDefaultWorkspaceCreationProperties :: Show DefaultWorkspaceCreationProperties where
  show = genericShow
instance decodeDefaultWorkspaceCreationProperties :: Decode DefaultWorkspaceCreationProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDefaultWorkspaceCreationProperties :: Encode DefaultWorkspaceCreationProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTagsRequest = DeleteTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  , "TagKeys" :: (TagKeyList)
  }
derive instance newtypeDeleteTagsRequest :: Newtype DeleteTagsRequest _
derive instance repGenericDeleteTagsRequest :: Generic DeleteTagsRequest _
instance showDeleteTagsRequest :: Show DeleteTagsRequest where
  show = genericShow
instance decodeDeleteTagsRequest :: Decode DeleteTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTagsRequest :: Encode DeleteTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteTagsResult = DeleteTagsResult Types.NoArguments
derive instance newtypeDeleteTagsResult :: Newtype DeleteTagsResult _
derive instance repGenericDeleteTagsResult :: Generic DeleteTagsResult _
instance showDeleteTagsResult :: Show DeleteTagsResult where
  show = genericShow
instance decodeDeleteTagsResult :: Decode DeleteTagsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteTagsResult :: Encode DeleteTagsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTagsRequest = DescribeTagsRequest 
  { "ResourceId" :: (NonEmptyString)
  }
derive instance newtypeDescribeTagsRequest :: Newtype DescribeTagsRequest _
derive instance repGenericDescribeTagsRequest :: Generic DescribeTagsRequest _
instance showDescribeTagsRequest :: Show DescribeTagsRequest where
  show = genericShow
instance decodeDescribeTagsRequest :: Decode DescribeTagsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsRequest :: Encode DescribeTagsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeTagsResult = DescribeTagsResult 
  { "TagList" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeDescribeTagsResult :: Newtype DescribeTagsResult _
derive instance repGenericDescribeTagsResult :: Generic DescribeTagsResult _
instance showDescribeTagsResult :: Show DescribeTagsResult where
  show = genericShow
instance decodeDescribeTagsResult :: Decode DescribeTagsResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeTagsResult :: Encode DescribeTagsResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkspaceBundlesRequest = DescribeWorkspaceBundlesRequest 
  { "BundleIds" :: NullOrUndefined.NullOrUndefined (BundleIdList)
  , "Owner" :: NullOrUndefined.NullOrUndefined (BundleOwner)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspaceBundlesRequest :: Newtype DescribeWorkspaceBundlesRequest _
derive instance repGenericDescribeWorkspaceBundlesRequest :: Generic DescribeWorkspaceBundlesRequest _
instance showDescribeWorkspaceBundlesRequest :: Show DescribeWorkspaceBundlesRequest where
  show = genericShow
instance decodeDescribeWorkspaceBundlesRequest :: Decode DescribeWorkspaceBundlesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkspaceBundlesRequest :: Encode DescribeWorkspaceBundlesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkspaceBundlesResult = DescribeWorkspaceBundlesResult 
  { "Bundles" :: NullOrUndefined.NullOrUndefined (BundleList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspaceBundlesResult :: Newtype DescribeWorkspaceBundlesResult _
derive instance repGenericDescribeWorkspaceBundlesResult :: Generic DescribeWorkspaceBundlesResult _
instance showDescribeWorkspaceBundlesResult :: Show DescribeWorkspaceBundlesResult where
  show = genericShow
instance decodeDescribeWorkspaceBundlesResult :: Decode DescribeWorkspaceBundlesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkspaceBundlesResult :: Encode DescribeWorkspaceBundlesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkspaceDirectoriesRequest = DescribeWorkspaceDirectoriesRequest 
  { "DirectoryIds" :: NullOrUndefined.NullOrUndefined (DirectoryIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspaceDirectoriesRequest :: Newtype DescribeWorkspaceDirectoriesRequest _
derive instance repGenericDescribeWorkspaceDirectoriesRequest :: Generic DescribeWorkspaceDirectoriesRequest _
instance showDescribeWorkspaceDirectoriesRequest :: Show DescribeWorkspaceDirectoriesRequest where
  show = genericShow
instance decodeDescribeWorkspaceDirectoriesRequest :: Decode DescribeWorkspaceDirectoriesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkspaceDirectoriesRequest :: Encode DescribeWorkspaceDirectoriesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkspaceDirectoriesResult = DescribeWorkspaceDirectoriesResult 
  { "Directories" :: NullOrUndefined.NullOrUndefined (DirectoryList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspaceDirectoriesResult :: Newtype DescribeWorkspaceDirectoriesResult _
derive instance repGenericDescribeWorkspaceDirectoriesResult :: Generic DescribeWorkspaceDirectoriesResult _
instance showDescribeWorkspaceDirectoriesResult :: Show DescribeWorkspaceDirectoriesResult where
  show = genericShow
instance decodeDescribeWorkspaceDirectoriesResult :: Decode DescribeWorkspaceDirectoriesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkspaceDirectoriesResult :: Encode DescribeWorkspaceDirectoriesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkspacesConnectionStatusRequest = DescribeWorkspacesConnectionStatusRequest 
  { "WorkspaceIds" :: NullOrUndefined.NullOrUndefined (WorkspaceIdList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspacesConnectionStatusRequest :: Newtype DescribeWorkspacesConnectionStatusRequest _
derive instance repGenericDescribeWorkspacesConnectionStatusRequest :: Generic DescribeWorkspacesConnectionStatusRequest _
instance showDescribeWorkspacesConnectionStatusRequest :: Show DescribeWorkspacesConnectionStatusRequest where
  show = genericShow
instance decodeDescribeWorkspacesConnectionStatusRequest :: Decode DescribeWorkspacesConnectionStatusRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkspacesConnectionStatusRequest :: Encode DescribeWorkspacesConnectionStatusRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkspacesConnectionStatusResult = DescribeWorkspacesConnectionStatusResult 
  { "WorkspacesConnectionStatus" :: NullOrUndefined.NullOrUndefined (WorkspaceConnectionStatusList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspacesConnectionStatusResult :: Newtype DescribeWorkspacesConnectionStatusResult _
derive instance repGenericDescribeWorkspacesConnectionStatusResult :: Generic DescribeWorkspacesConnectionStatusResult _
instance showDescribeWorkspacesConnectionStatusResult :: Show DescribeWorkspacesConnectionStatusResult where
  show = genericShow
instance decodeDescribeWorkspacesConnectionStatusResult :: Decode DescribeWorkspacesConnectionStatusResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkspacesConnectionStatusResult :: Encode DescribeWorkspacesConnectionStatusResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkspacesRequest = DescribeWorkspacesRequest 
  { "WorkspaceIds" :: NullOrUndefined.NullOrUndefined (WorkspaceIdList)
  , "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "UserName" :: NullOrUndefined.NullOrUndefined (UserName)
  , "BundleId" :: NullOrUndefined.NullOrUndefined (BundleId)
  , "Limit" :: NullOrUndefined.NullOrUndefined (Limit)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspacesRequest :: Newtype DescribeWorkspacesRequest _
derive instance repGenericDescribeWorkspacesRequest :: Generic DescribeWorkspacesRequest _
instance showDescribeWorkspacesRequest :: Show DescribeWorkspacesRequest where
  show = genericShow
instance decodeDescribeWorkspacesRequest :: Decode DescribeWorkspacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkspacesRequest :: Encode DescribeWorkspacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeWorkspacesResult = DescribeWorkspacesResult 
  { "Workspaces" :: NullOrUndefined.NullOrUndefined (WorkspaceList)
  , "NextToken" :: NullOrUndefined.NullOrUndefined (PaginationToken)
  }
derive instance newtypeDescribeWorkspacesResult :: Newtype DescribeWorkspacesResult _
derive instance repGenericDescribeWorkspacesResult :: Generic DescribeWorkspacesResult _
instance showDescribeWorkspacesResult :: Show DescribeWorkspacesResult where
  show = genericShow
instance decodeDescribeWorkspacesResult :: Decode DescribeWorkspacesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeWorkspacesResult :: Encode DescribeWorkspacesResult where
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


newtype DirectoryId = DirectoryId String
derive instance newtypeDirectoryId :: Newtype DirectoryId _
derive instance repGenericDirectoryId :: Generic DirectoryId _
instance showDirectoryId :: Show DirectoryId where
  show = genericShow
instance decodeDirectoryId :: Decode DirectoryId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryId :: Encode DirectoryId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryIdList = DirectoryIdList (Array DirectoryId)
derive instance newtypeDirectoryIdList :: Newtype DirectoryIdList _
derive instance repGenericDirectoryIdList :: Generic DirectoryIdList _
instance showDirectoryIdList :: Show DirectoryIdList where
  show = genericShow
instance decodeDirectoryIdList :: Decode DirectoryIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryIdList :: Encode DirectoryIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DirectoryList = DirectoryList (Array WorkspaceDirectory)
derive instance newtypeDirectoryList :: Newtype DirectoryList _
derive instance repGenericDirectoryList :: Generic DirectoryList _
instance showDirectoryList :: Show DirectoryList where
  show = genericShow
instance decodeDirectoryList :: Decode DirectoryList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDirectoryList :: Encode DirectoryList where
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


newtype DnsIpAddresses = DnsIpAddresses (Array IpAddress)
derive instance newtypeDnsIpAddresses :: Newtype DnsIpAddresses _
derive instance repGenericDnsIpAddresses :: Generic DnsIpAddresses _
instance showDnsIpAddresses :: Show DnsIpAddresses where
  show = genericShow
instance decodeDnsIpAddresses :: Decode DnsIpAddresses where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDnsIpAddresses :: Encode DnsIpAddresses where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorType = ErrorType String
derive instance newtypeErrorType :: Newtype ErrorType _
derive instance repGenericErrorType :: Generic ErrorType _
instance showErrorType :: Show ErrorType where
  show = genericShow
instance decodeErrorType :: Decode ErrorType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorType :: Encode ErrorType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ExceptionMessage = ExceptionMessage String
derive instance newtypeExceptionMessage :: Newtype ExceptionMessage _
derive instance repGenericExceptionMessage :: Generic ExceptionMessage _
instance showExceptionMessage :: Show ExceptionMessage where
  show = genericShow
instance decodeExceptionMessage :: Decode ExceptionMessage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeExceptionMessage :: Encode ExceptionMessage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a WorkSpace that could not be created.</p>
newtype FailedCreateWorkspaceRequest = FailedCreateWorkspaceRequest 
  { "WorkspaceRequest" :: NullOrUndefined.NullOrUndefined (WorkspaceRequest)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorType)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeFailedCreateWorkspaceRequest :: Newtype FailedCreateWorkspaceRequest _
derive instance repGenericFailedCreateWorkspaceRequest :: Generic FailedCreateWorkspaceRequest _
instance showFailedCreateWorkspaceRequest :: Show FailedCreateWorkspaceRequest where
  show = genericShow
instance decodeFailedCreateWorkspaceRequest :: Decode FailedCreateWorkspaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedCreateWorkspaceRequest :: Encode FailedCreateWorkspaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedCreateWorkspaceRequests = FailedCreateWorkspaceRequests (Array FailedCreateWorkspaceRequest)
derive instance newtypeFailedCreateWorkspaceRequests :: Newtype FailedCreateWorkspaceRequests _
derive instance repGenericFailedCreateWorkspaceRequests :: Generic FailedCreateWorkspaceRequests _
instance showFailedCreateWorkspaceRequests :: Show FailedCreateWorkspaceRequests where
  show = genericShow
instance decodeFailedCreateWorkspaceRequests :: Decode FailedCreateWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedCreateWorkspaceRequests :: Encode FailedCreateWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedRebootWorkspaceRequests = FailedRebootWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedRebootWorkspaceRequests :: Newtype FailedRebootWorkspaceRequests _
derive instance repGenericFailedRebootWorkspaceRequests :: Generic FailedRebootWorkspaceRequests _
instance showFailedRebootWorkspaceRequests :: Show FailedRebootWorkspaceRequests where
  show = genericShow
instance decodeFailedRebootWorkspaceRequests :: Decode FailedRebootWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedRebootWorkspaceRequests :: Encode FailedRebootWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedRebuildWorkspaceRequests = FailedRebuildWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedRebuildWorkspaceRequests :: Newtype FailedRebuildWorkspaceRequests _
derive instance repGenericFailedRebuildWorkspaceRequests :: Generic FailedRebuildWorkspaceRequests _
instance showFailedRebuildWorkspaceRequests :: Show FailedRebuildWorkspaceRequests where
  show = genericShow
instance decodeFailedRebuildWorkspaceRequests :: Decode FailedRebuildWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedRebuildWorkspaceRequests :: Encode FailedRebuildWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedStartWorkspaceRequests = FailedStartWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedStartWorkspaceRequests :: Newtype FailedStartWorkspaceRequests _
derive instance repGenericFailedStartWorkspaceRequests :: Generic FailedStartWorkspaceRequests _
instance showFailedStartWorkspaceRequests :: Show FailedStartWorkspaceRequests where
  show = genericShow
instance decodeFailedStartWorkspaceRequests :: Decode FailedStartWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedStartWorkspaceRequests :: Encode FailedStartWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedStopWorkspaceRequests = FailedStopWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedStopWorkspaceRequests :: Newtype FailedStopWorkspaceRequests _
derive instance repGenericFailedStopWorkspaceRequests :: Generic FailedStopWorkspaceRequests _
instance showFailedStopWorkspaceRequests :: Show FailedStopWorkspaceRequests where
  show = genericShow
instance decodeFailedStopWorkspaceRequests :: Decode FailedStopWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedStopWorkspaceRequests :: Encode FailedStopWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FailedTerminateWorkspaceRequests = FailedTerminateWorkspaceRequests (Array FailedWorkspaceChangeRequest)
derive instance newtypeFailedTerminateWorkspaceRequests :: Newtype FailedTerminateWorkspaceRequests _
derive instance repGenericFailedTerminateWorkspaceRequests :: Generic FailedTerminateWorkspaceRequests _
instance showFailedTerminateWorkspaceRequests :: Show FailedTerminateWorkspaceRequests where
  show = genericShow
instance decodeFailedTerminateWorkspaceRequests :: Decode FailedTerminateWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedTerminateWorkspaceRequests :: Encode FailedTerminateWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a WorkSpace that could not be rebooted (<a>RebootWorkspaces</a>), rebuilt (<a>RebuildWorkspaces</a>), terminated (<a>TerminateWorkspaces</a>), started (<a>StartWorkspaces</a>), or stopped (<a>StopWorkspaces</a>).</p>
newtype FailedWorkspaceChangeRequest = FailedWorkspaceChangeRequest 
  { "WorkspaceId" :: NullOrUndefined.NullOrUndefined (WorkspaceId)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (ErrorType)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (Description)
  }
derive instance newtypeFailedWorkspaceChangeRequest :: Newtype FailedWorkspaceChangeRequest _
derive instance repGenericFailedWorkspaceChangeRequest :: Generic FailedWorkspaceChangeRequest _
instance showFailedWorkspaceChangeRequest :: Show FailedWorkspaceChangeRequest where
  show = genericShow
instance decodeFailedWorkspaceChangeRequest :: Decode FailedWorkspaceChangeRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedWorkspaceChangeRequest :: Encode FailedWorkspaceChangeRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more parameter values are not valid.</p>
newtype InvalidParameterValuesException = InvalidParameterValuesException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidParameterValuesException :: Newtype InvalidParameterValuesException _
derive instance repGenericInvalidParameterValuesException :: Generic InvalidParameterValuesException _
instance showInvalidParameterValuesException :: Show InvalidParameterValuesException where
  show = genericShow
instance decodeInvalidParameterValuesException :: Decode InvalidParameterValuesException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidParameterValuesException :: Encode InvalidParameterValuesException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The state of the WorkSpace is not valid for this operation.</p>
newtype InvalidResourceStateException = InvalidResourceStateException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeInvalidResourceStateException :: Newtype InvalidResourceStateException _
derive instance repGenericInvalidResourceStateException :: Generic InvalidResourceStateException _
instance showInvalidResourceStateException :: Show InvalidResourceStateException where
  show = genericShow
instance decodeInvalidResourceStateException :: Decode InvalidResourceStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidResourceStateException :: Encode InvalidResourceStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IpAddress = IpAddress String
derive instance newtypeIpAddress :: Newtype IpAddress _
derive instance repGenericIpAddress :: Generic IpAddress _
instance showIpAddress :: Show IpAddress where
  show = genericShow
instance decodeIpAddress :: Decode IpAddress where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIpAddress :: Encode IpAddress where
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


newtype ModificationResourceEnum = ModificationResourceEnum String
derive instance newtypeModificationResourceEnum :: Newtype ModificationResourceEnum _
derive instance repGenericModificationResourceEnum :: Generic ModificationResourceEnum _
instance showModificationResourceEnum :: Show ModificationResourceEnum where
  show = genericShow
instance decodeModificationResourceEnum :: Decode ModificationResourceEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModificationResourceEnum :: Encode ModificationResourceEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a WorkSpace modification.</p>
newtype ModificationState = ModificationState 
  { "Resource" :: NullOrUndefined.NullOrUndefined (ModificationResourceEnum)
  , "State" :: NullOrUndefined.NullOrUndefined (ModificationStateEnum)
  }
derive instance newtypeModificationState :: Newtype ModificationState _
derive instance repGenericModificationState :: Generic ModificationState _
instance showModificationState :: Show ModificationState where
  show = genericShow
instance decodeModificationState :: Decode ModificationState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModificationState :: Encode ModificationState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModificationStateEnum = ModificationStateEnum String
derive instance newtypeModificationStateEnum :: Newtype ModificationStateEnum _
derive instance repGenericModificationStateEnum :: Generic ModificationStateEnum _
instance showModificationStateEnum :: Show ModificationStateEnum where
  show = genericShow
instance decodeModificationStateEnum :: Decode ModificationStateEnum where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModificationStateEnum :: Encode ModificationStateEnum where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModificationStateList = ModificationStateList (Array ModificationState)
derive instance newtypeModificationStateList :: Newtype ModificationStateList _
derive instance repGenericModificationStateList :: Generic ModificationStateList _
instance showModificationStateList :: Show ModificationStateList where
  show = genericShow
instance decodeModificationStateList :: Decode ModificationStateList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModificationStateList :: Encode ModificationStateList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyWorkspacePropertiesRequest = ModifyWorkspacePropertiesRequest 
  { "WorkspaceId" :: (WorkspaceId)
  , "WorkspaceProperties" :: (WorkspaceProperties)
  }
derive instance newtypeModifyWorkspacePropertiesRequest :: Newtype ModifyWorkspacePropertiesRequest _
derive instance repGenericModifyWorkspacePropertiesRequest :: Generic ModifyWorkspacePropertiesRequest _
instance showModifyWorkspacePropertiesRequest :: Show ModifyWorkspacePropertiesRequest where
  show = genericShow
instance decodeModifyWorkspacePropertiesRequest :: Decode ModifyWorkspacePropertiesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyWorkspacePropertiesRequest :: Encode ModifyWorkspacePropertiesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ModifyWorkspacePropertiesResult = ModifyWorkspacePropertiesResult Types.NoArguments
derive instance newtypeModifyWorkspacePropertiesResult :: Newtype ModifyWorkspacePropertiesResult _
derive instance repGenericModifyWorkspacePropertiesResult :: Generic ModifyWorkspacePropertiesResult _
instance showModifyWorkspacePropertiesResult :: Show ModifyWorkspacePropertiesResult where
  show = genericShow
instance decodeModifyWorkspacePropertiesResult :: Decode ModifyWorkspacePropertiesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeModifyWorkspacePropertiesResult :: Encode ModifyWorkspacePropertiesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString :: Newtype NonEmptyString _
derive instance repGenericNonEmptyString :: Generic NonEmptyString _
instance showNonEmptyString :: Show NonEmptyString where
  show = genericShow
instance decodeNonEmptyString :: Decode NonEmptyString where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNonEmptyString :: Encode NonEmptyString where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The properties of this WorkSpace are currently being modified. Try again in a moment.</p>
newtype OperationInProgressException = OperationInProgressException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeOperationInProgressException :: Newtype OperationInProgressException _
derive instance repGenericOperationInProgressException :: Generic OperationInProgressException _
instance showOperationInProgressException :: Show OperationInProgressException where
  show = genericShow
instance decodeOperationInProgressException :: Decode OperationInProgressException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOperationInProgressException :: Encode OperationInProgressException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PaginationToken = PaginationToken String
derive instance newtypePaginationToken :: Newtype PaginationToken _
derive instance repGenericPaginationToken :: Generic PaginationToken _
instance showPaginationToken :: Show PaginationToken where
  show = genericShow
instance decodePaginationToken :: Decode PaginationToken where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePaginationToken :: Encode PaginationToken where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information used to reboot a WorkSpace.</p>
newtype RebootRequest = RebootRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }
derive instance newtypeRebootRequest :: Newtype RebootRequest _
derive instance repGenericRebootRequest :: Generic RebootRequest _
instance showRebootRequest :: Show RebootRequest where
  show = genericShow
instance decodeRebootRequest :: Decode RebootRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootRequest :: Encode RebootRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootWorkspaceRequests = RebootWorkspaceRequests (Array RebootRequest)
derive instance newtypeRebootWorkspaceRequests :: Newtype RebootWorkspaceRequests _
derive instance repGenericRebootWorkspaceRequests :: Generic RebootWorkspaceRequests _
instance showRebootWorkspaceRequests :: Show RebootWorkspaceRequests where
  show = genericShow
instance decodeRebootWorkspaceRequests :: Decode RebootWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootWorkspaceRequests :: Encode RebootWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootWorkspacesRequest = RebootWorkspacesRequest 
  { "RebootWorkspaceRequests" :: (RebootWorkspaceRequests)
  }
derive instance newtypeRebootWorkspacesRequest :: Newtype RebootWorkspacesRequest _
derive instance repGenericRebootWorkspacesRequest :: Generic RebootWorkspacesRequest _
instance showRebootWorkspacesRequest :: Show RebootWorkspacesRequest where
  show = genericShow
instance decodeRebootWorkspacesRequest :: Decode RebootWorkspacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootWorkspacesRequest :: Encode RebootWorkspacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebootWorkspacesResult = RebootWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined.NullOrUndefined (FailedRebootWorkspaceRequests)
  }
derive instance newtypeRebootWorkspacesResult :: Newtype RebootWorkspacesResult _
derive instance repGenericRebootWorkspacesResult :: Generic RebootWorkspacesResult _
instance showRebootWorkspacesResult :: Show RebootWorkspacesResult where
  show = genericShow
instance decodeRebootWorkspacesResult :: Decode RebootWorkspacesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebootWorkspacesResult :: Encode RebootWorkspacesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information used to rebuild a WorkSpace.</p>
newtype RebuildRequest = RebuildRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }
derive instance newtypeRebuildRequest :: Newtype RebuildRequest _
derive instance repGenericRebuildRequest :: Generic RebuildRequest _
instance showRebuildRequest :: Show RebuildRequest where
  show = genericShow
instance decodeRebuildRequest :: Decode RebuildRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebuildRequest :: Encode RebuildRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebuildWorkspaceRequests = RebuildWorkspaceRequests (Array RebuildRequest)
derive instance newtypeRebuildWorkspaceRequests :: Newtype RebuildWorkspaceRequests _
derive instance repGenericRebuildWorkspaceRequests :: Generic RebuildWorkspaceRequests _
instance showRebuildWorkspaceRequests :: Show RebuildWorkspaceRequests where
  show = genericShow
instance decodeRebuildWorkspaceRequests :: Decode RebuildWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebuildWorkspaceRequests :: Encode RebuildWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebuildWorkspacesRequest = RebuildWorkspacesRequest 
  { "RebuildWorkspaceRequests" :: (RebuildWorkspaceRequests)
  }
derive instance newtypeRebuildWorkspacesRequest :: Newtype RebuildWorkspacesRequest _
derive instance repGenericRebuildWorkspacesRequest :: Generic RebuildWorkspacesRequest _
instance showRebuildWorkspacesRequest :: Show RebuildWorkspacesRequest where
  show = genericShow
instance decodeRebuildWorkspacesRequest :: Decode RebuildWorkspacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebuildWorkspacesRequest :: Encode RebuildWorkspacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RebuildWorkspacesResult = RebuildWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined.NullOrUndefined (FailedRebuildWorkspaceRequests)
  }
derive instance newtypeRebuildWorkspacesResult :: Newtype RebuildWorkspacesResult _
derive instance repGenericRebuildWorkspacesResult :: Generic RebuildWorkspacesResult _
instance showRebuildWorkspacesResult :: Show RebuildWorkspacesResult where
  show = genericShow
instance decodeRebuildWorkspacesResult :: Decode RebuildWorkspacesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRebuildWorkspacesResult :: Encode RebuildWorkspacesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RegistrationCode = RegistrationCode String
derive instance newtypeRegistrationCode :: Newtype RegistrationCode _
derive instance repGenericRegistrationCode :: Generic RegistrationCode _
instance showRegistrationCode :: Show RegistrationCode where
  show = genericShow
instance decodeRegistrationCode :: Decode RegistrationCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRegistrationCode :: Encode RegistrationCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Your resource limits have been exceeded.</p>
newtype ResourceLimitExceededException = ResourceLimitExceededException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeResourceLimitExceededException :: Newtype ResourceLimitExceededException _
derive instance repGenericResourceLimitExceededException :: Generic ResourceLimitExceededException _
instance showResourceLimitExceededException :: Show ResourceLimitExceededException where
  show = genericShow
instance decodeResourceLimitExceededException :: Decode ResourceLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceLimitExceededException :: Encode ResourceLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource could not be found.</p>
newtype ResourceNotFoundException = ResourceNotFoundException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeResourceNotFoundException :: Newtype ResourceNotFoundException _
derive instance repGenericResourceNotFoundException :: Generic ResourceNotFoundException _
instance showResourceNotFoundException :: Show ResourceNotFoundException where
  show = genericShow
instance decodeResourceNotFoundException :: Decode ResourceNotFoundException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNotFoundException :: Encode ResourceNotFoundException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified resource is not available.</p>
newtype ResourceUnavailableException = ResourceUnavailableException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  , "ResourceId" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeResourceUnavailableException :: Newtype ResourceUnavailableException _
derive instance repGenericResourceUnavailableException :: Generic ResourceUnavailableException _
instance showResourceUnavailableException :: Show ResourceUnavailableException where
  show = genericShow
instance decodeResourceUnavailableException :: Decode ResourceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceUnavailableException :: Encode ResourceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about the root volume for a WorkSpace bundle.</p>
newtype RootStorage = RootStorage 
  { "Capacity" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeRootStorage :: Newtype RootStorage _
derive instance repGenericRootStorage :: Generic RootStorage _
instance showRootStorage :: Show RootStorage where
  show = genericShow
instance decodeRootStorage :: Decode RootStorage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRootStorage :: Encode RootStorage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RootVolumeSizeGib = RootVolumeSizeGib Int
derive instance newtypeRootVolumeSizeGib :: Newtype RootVolumeSizeGib _
derive instance repGenericRootVolumeSizeGib :: Generic RootVolumeSizeGib _
instance showRootVolumeSizeGib :: Show RootVolumeSizeGib where
  show = genericShow
instance decodeRootVolumeSizeGib :: Decode RootVolumeSizeGib where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRootVolumeSizeGib :: Encode RootVolumeSizeGib where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RunningMode = RunningMode String
derive instance newtypeRunningMode :: Newtype RunningMode _
derive instance repGenericRunningMode :: Generic RunningMode _
instance showRunningMode :: Show RunningMode where
  show = genericShow
instance decodeRunningMode :: Decode RunningMode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunningMode :: Encode RunningMode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RunningModeAutoStopTimeoutInMinutes = RunningModeAutoStopTimeoutInMinutes Int
derive instance newtypeRunningModeAutoStopTimeoutInMinutes :: Newtype RunningModeAutoStopTimeoutInMinutes _
derive instance repGenericRunningModeAutoStopTimeoutInMinutes :: Generic RunningModeAutoStopTimeoutInMinutes _
instance showRunningModeAutoStopTimeoutInMinutes :: Show RunningModeAutoStopTimeoutInMinutes where
  show = genericShow
instance decodeRunningModeAutoStopTimeoutInMinutes :: Decode RunningModeAutoStopTimeoutInMinutes where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRunningModeAutoStopTimeoutInMinutes :: Encode RunningModeAutoStopTimeoutInMinutes where
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


-- | <p>Information used to start a WorkSpace.</p>
newtype StartRequest = StartRequest 
  { "WorkspaceId" :: NullOrUndefined.NullOrUndefined (WorkspaceId)
  }
derive instance newtypeStartRequest :: Newtype StartRequest _
derive instance repGenericStartRequest :: Generic StartRequest _
instance showStartRequest :: Show StartRequest where
  show = genericShow
instance decodeStartRequest :: Decode StartRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartRequest :: Encode StartRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartWorkspaceRequests = StartWorkspaceRequests (Array StartRequest)
derive instance newtypeStartWorkspaceRequests :: Newtype StartWorkspaceRequests _
derive instance repGenericStartWorkspaceRequests :: Generic StartWorkspaceRequests _
instance showStartWorkspaceRequests :: Show StartWorkspaceRequests where
  show = genericShow
instance decodeStartWorkspaceRequests :: Decode StartWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartWorkspaceRequests :: Encode StartWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartWorkspacesRequest = StartWorkspacesRequest 
  { "StartWorkspaceRequests" :: (StartWorkspaceRequests)
  }
derive instance newtypeStartWorkspacesRequest :: Newtype StartWorkspacesRequest _
derive instance repGenericStartWorkspacesRequest :: Generic StartWorkspacesRequest _
instance showStartWorkspacesRequest :: Show StartWorkspacesRequest where
  show = genericShow
instance decodeStartWorkspacesRequest :: Decode StartWorkspacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartWorkspacesRequest :: Encode StartWorkspacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StartWorkspacesResult = StartWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined.NullOrUndefined (FailedStartWorkspaceRequests)
  }
derive instance newtypeStartWorkspacesResult :: Newtype StartWorkspacesResult _
derive instance repGenericStartWorkspacesResult :: Generic StartWorkspacesResult _
instance showStartWorkspacesResult :: Show StartWorkspacesResult where
  show = genericShow
instance decodeStartWorkspacesResult :: Decode StartWorkspacesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStartWorkspacesResult :: Encode StartWorkspacesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information used to stop a WorkSpace.</p>
newtype StopRequest = StopRequest 
  { "WorkspaceId" :: NullOrUndefined.NullOrUndefined (WorkspaceId)
  }
derive instance newtypeStopRequest :: Newtype StopRequest _
derive instance repGenericStopRequest :: Generic StopRequest _
instance showStopRequest :: Show StopRequest where
  show = genericShow
instance decodeStopRequest :: Decode StopRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopRequest :: Encode StopRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopWorkspaceRequests = StopWorkspaceRequests (Array StopRequest)
derive instance newtypeStopWorkspaceRequests :: Newtype StopWorkspaceRequests _
derive instance repGenericStopWorkspaceRequests :: Generic StopWorkspaceRequests _
instance showStopWorkspaceRequests :: Show StopWorkspaceRequests where
  show = genericShow
instance decodeStopWorkspaceRequests :: Decode StopWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopWorkspaceRequests :: Encode StopWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopWorkspacesRequest = StopWorkspacesRequest 
  { "StopWorkspaceRequests" :: (StopWorkspaceRequests)
  }
derive instance newtypeStopWorkspacesRequest :: Newtype StopWorkspacesRequest _
derive instance repGenericStopWorkspacesRequest :: Generic StopWorkspacesRequest _
instance showStopWorkspacesRequest :: Show StopWorkspacesRequest where
  show = genericShow
instance decodeStopWorkspacesRequest :: Decode StopWorkspacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopWorkspacesRequest :: Encode StopWorkspacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StopWorkspacesResult = StopWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined.NullOrUndefined (FailedStopWorkspaceRequests)
  }
derive instance newtypeStopWorkspacesResult :: Newtype StopWorkspacesResult _
derive instance repGenericStopWorkspacesResult :: Generic StopWorkspacesResult _
instance showStopWorkspacesResult :: Show StopWorkspacesResult where
  show = genericShow
instance decodeStopWorkspacesResult :: Decode StopWorkspacesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStopWorkspacesResult :: Encode StopWorkspacesResult where
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


-- | <p>Information about a tag.</p>
newtype Tag = Tag 
  { "Key" :: (TagKey)
  , "Value" :: NullOrUndefined.NullOrUndefined (TagValue)
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


newtype TagKeyList = TagKeyList (Array NonEmptyString)
derive instance newtypeTagKeyList :: Newtype TagKeyList _
derive instance repGenericTagKeyList :: Generic TagKeyList _
instance showTagKeyList :: Show TagKeyList where
  show = genericShow
instance decodeTagKeyList :: Decode TagKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagKeyList :: Encode TagKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TagList = TagList (Array Tag)
derive instance newtypeTagList :: Newtype TagList _
derive instance repGenericTagList :: Generic TagList _
instance showTagList :: Show TagList where
  show = genericShow
instance decodeTagList :: Decode TagList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTagList :: Encode TagList where
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


-- | <p>Information used to terminate a WorkSpace.</p>
newtype TerminateRequest = TerminateRequest 
  { "WorkspaceId" :: (WorkspaceId)
  }
derive instance newtypeTerminateRequest :: Newtype TerminateRequest _
derive instance repGenericTerminateRequest :: Generic TerminateRequest _
instance showTerminateRequest :: Show TerminateRequest where
  show = genericShow
instance decodeTerminateRequest :: Decode TerminateRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateRequest :: Encode TerminateRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateWorkspaceRequests = TerminateWorkspaceRequests (Array TerminateRequest)
derive instance newtypeTerminateWorkspaceRequests :: Newtype TerminateWorkspaceRequests _
derive instance repGenericTerminateWorkspaceRequests :: Generic TerminateWorkspaceRequests _
instance showTerminateWorkspaceRequests :: Show TerminateWorkspaceRequests where
  show = genericShow
instance decodeTerminateWorkspaceRequests :: Decode TerminateWorkspaceRequests where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateWorkspaceRequests :: Encode TerminateWorkspaceRequests where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateWorkspacesRequest = TerminateWorkspacesRequest 
  { "TerminateWorkspaceRequests" :: (TerminateWorkspaceRequests)
  }
derive instance newtypeTerminateWorkspacesRequest :: Newtype TerminateWorkspacesRequest _
derive instance repGenericTerminateWorkspacesRequest :: Generic TerminateWorkspacesRequest _
instance showTerminateWorkspacesRequest :: Show TerminateWorkspacesRequest where
  show = genericShow
instance decodeTerminateWorkspacesRequest :: Decode TerminateWorkspacesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateWorkspacesRequest :: Encode TerminateWorkspacesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TerminateWorkspacesResult = TerminateWorkspacesResult 
  { "FailedRequests" :: NullOrUndefined.NullOrUndefined (FailedTerminateWorkspaceRequests)
  }
derive instance newtypeTerminateWorkspacesResult :: Newtype TerminateWorkspacesResult _
derive instance repGenericTerminateWorkspacesResult :: Generic TerminateWorkspacesResult _
instance showTerminateWorkspacesResult :: Show TerminateWorkspacesResult where
  show = genericShow
instance decodeTerminateWorkspacesResult :: Decode TerminateWorkspacesResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTerminateWorkspacesResult :: Encode TerminateWorkspacesResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The configuration of this WorkSpace is not supported for this operation. For more information, see the <a href="http://docs.aws.amazon.com/workspaces/latest/adminguide/">Amazon WorkSpaces Administration Guide</a>. </p>
newtype UnsupportedWorkspaceConfigurationException = UnsupportedWorkspaceConfigurationException 
  { "Message'" :: NullOrUndefined.NullOrUndefined (ExceptionMessage)
  }
derive instance newtypeUnsupportedWorkspaceConfigurationException :: Newtype UnsupportedWorkspaceConfigurationException _
derive instance repGenericUnsupportedWorkspaceConfigurationException :: Generic UnsupportedWorkspaceConfigurationException _
instance showUnsupportedWorkspaceConfigurationException :: Show UnsupportedWorkspaceConfigurationException where
  show = genericShow
instance decodeUnsupportedWorkspaceConfigurationException :: Decode UnsupportedWorkspaceConfigurationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnsupportedWorkspaceConfigurationException :: Encode UnsupportedWorkspaceConfigurationException where
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


-- | <p>Information about the user storage for a WorkSpace bundle.</p>
newtype UserStorage = UserStorage 
  { "Capacity" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  }
derive instance newtypeUserStorage :: Newtype UserStorage _
derive instance repGenericUserStorage :: Generic UserStorage _
instance showUserStorage :: Show UserStorage where
  show = genericShow
instance decodeUserStorage :: Decode UserStorage where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserStorage :: Encode UserStorage where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserVolumeSizeGib = UserVolumeSizeGib Int
derive instance newtypeUserVolumeSizeGib :: Newtype UserVolumeSizeGib _
derive instance repGenericUserVolumeSizeGib :: Generic UserVolumeSizeGib _
instance showUserVolumeSizeGib :: Show UserVolumeSizeGib where
  show = genericShow
instance decodeUserVolumeSizeGib :: Decode UserVolumeSizeGib where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserVolumeSizeGib :: Encode UserVolumeSizeGib where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype VolumeEncryptionKey = VolumeEncryptionKey String
derive instance newtypeVolumeEncryptionKey :: Newtype VolumeEncryptionKey _
derive instance repGenericVolumeEncryptionKey :: Generic VolumeEncryptionKey _
instance showVolumeEncryptionKey :: Show VolumeEncryptionKey where
  show = genericShow
instance decodeVolumeEncryptionKey :: Decode VolumeEncryptionKey where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeVolumeEncryptionKey :: Encode VolumeEncryptionKey where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a WorkSpace.</p>
newtype Workspace = Workspace 
  { "WorkspaceId" :: NullOrUndefined.NullOrUndefined (WorkspaceId)
  , "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "UserName" :: NullOrUndefined.NullOrUndefined (UserName)
  , "IpAddress" :: NullOrUndefined.NullOrUndefined (IpAddress)
  , "State" :: NullOrUndefined.NullOrUndefined (WorkspaceState)
  , "BundleId" :: NullOrUndefined.NullOrUndefined (BundleId)
  , "SubnetId" :: NullOrUndefined.NullOrUndefined (SubnetId)
  , "ErrorMessage" :: NullOrUndefined.NullOrUndefined (Description)
  , "ErrorCode" :: NullOrUndefined.NullOrUndefined (WorkspaceErrorCode)
  , "ComputerName" :: NullOrUndefined.NullOrUndefined (ComputerName)
  , "VolumeEncryptionKey" :: NullOrUndefined.NullOrUndefined (VolumeEncryptionKey)
  , "UserVolumeEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "RootVolumeEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "WorkspaceProperties" :: NullOrUndefined.NullOrUndefined (WorkspaceProperties)
  , "ModificationStates" :: NullOrUndefined.NullOrUndefined (ModificationStateList)
  }
derive instance newtypeWorkspace :: Newtype Workspace _
derive instance repGenericWorkspace :: Generic Workspace _
instance showWorkspace :: Show Workspace where
  show = genericShow
instance decodeWorkspace :: Decode Workspace where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspace :: Encode Workspace where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a WorkSpace bundle.</p>
newtype WorkspaceBundle = WorkspaceBundle 
  { "BundleId" :: NullOrUndefined.NullOrUndefined (BundleId)
  , "Name" :: NullOrUndefined.NullOrUndefined (NonEmptyString)
  , "Owner" :: NullOrUndefined.NullOrUndefined (BundleOwner)
  , "Description" :: NullOrUndefined.NullOrUndefined (Description)
  , "RootStorage" :: NullOrUndefined.NullOrUndefined (RootStorage)
  , "UserStorage" :: NullOrUndefined.NullOrUndefined (UserStorage)
  , "ComputeType" :: NullOrUndefined.NullOrUndefined (ComputeType)
  }
derive instance newtypeWorkspaceBundle :: Newtype WorkspaceBundle _
derive instance repGenericWorkspaceBundle :: Generic WorkspaceBundle _
instance showWorkspaceBundle :: Show WorkspaceBundle where
  show = genericShow
instance decodeWorkspaceBundle :: Decode WorkspaceBundle where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceBundle :: Encode WorkspaceBundle where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the connection status of a WorkSpace.</p>
newtype WorkspaceConnectionStatus = WorkspaceConnectionStatus 
  { "WorkspaceId" :: NullOrUndefined.NullOrUndefined (WorkspaceId)
  , "ConnectionState" :: NullOrUndefined.NullOrUndefined (ConnectionState)
  , "ConnectionStateCheckTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  , "LastKnownUserConnectionTimestamp" :: NullOrUndefined.NullOrUndefined (Number)
  }
derive instance newtypeWorkspaceConnectionStatus :: Newtype WorkspaceConnectionStatus _
derive instance repGenericWorkspaceConnectionStatus :: Generic WorkspaceConnectionStatus _
instance showWorkspaceConnectionStatus :: Show WorkspaceConnectionStatus where
  show = genericShow
instance decodeWorkspaceConnectionStatus :: Decode WorkspaceConnectionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceConnectionStatus :: Encode WorkspaceConnectionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceConnectionStatusList = WorkspaceConnectionStatusList (Array WorkspaceConnectionStatus)
derive instance newtypeWorkspaceConnectionStatusList :: Newtype WorkspaceConnectionStatusList _
derive instance repGenericWorkspaceConnectionStatusList :: Generic WorkspaceConnectionStatusList _
instance showWorkspaceConnectionStatusList :: Show WorkspaceConnectionStatusList where
  show = genericShow
instance decodeWorkspaceConnectionStatusList :: Decode WorkspaceConnectionStatusList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceConnectionStatusList :: Encode WorkspaceConnectionStatusList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Contains information about an AWS Directory Service directory for use with Amazon WorkSpaces.</p>
newtype WorkspaceDirectory = WorkspaceDirectory 
  { "DirectoryId" :: NullOrUndefined.NullOrUndefined (DirectoryId)
  , "Alias" :: NullOrUndefined.NullOrUndefined (Alias)
  , "DirectoryName" :: NullOrUndefined.NullOrUndefined (DirectoryName)
  , "RegistrationCode" :: NullOrUndefined.NullOrUndefined (RegistrationCode)
  , "SubnetIds" :: NullOrUndefined.NullOrUndefined (SubnetIds)
  , "DnsIpAddresses" :: NullOrUndefined.NullOrUndefined (DnsIpAddresses)
  , "CustomerUserName" :: NullOrUndefined.NullOrUndefined (UserName)
  , "IamRoleId" :: NullOrUndefined.NullOrUndefined (ARN)
  , "DirectoryType" :: NullOrUndefined.NullOrUndefined (WorkspaceDirectoryType)
  , "WorkspaceSecurityGroupId" :: NullOrUndefined.NullOrUndefined (SecurityGroupId)
  , "State" :: NullOrUndefined.NullOrUndefined (WorkspaceDirectoryState)
  , "WorkspaceCreationProperties" :: NullOrUndefined.NullOrUndefined (DefaultWorkspaceCreationProperties)
  }
derive instance newtypeWorkspaceDirectory :: Newtype WorkspaceDirectory _
derive instance repGenericWorkspaceDirectory :: Generic WorkspaceDirectory _
instance showWorkspaceDirectory :: Show WorkspaceDirectory where
  show = genericShow
instance decodeWorkspaceDirectory :: Decode WorkspaceDirectory where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceDirectory :: Encode WorkspaceDirectory where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceDirectoryState = WorkspaceDirectoryState String
derive instance newtypeWorkspaceDirectoryState :: Newtype WorkspaceDirectoryState _
derive instance repGenericWorkspaceDirectoryState :: Generic WorkspaceDirectoryState _
instance showWorkspaceDirectoryState :: Show WorkspaceDirectoryState where
  show = genericShow
instance decodeWorkspaceDirectoryState :: Decode WorkspaceDirectoryState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceDirectoryState :: Encode WorkspaceDirectoryState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceDirectoryType = WorkspaceDirectoryType String
derive instance newtypeWorkspaceDirectoryType :: Newtype WorkspaceDirectoryType _
derive instance repGenericWorkspaceDirectoryType :: Generic WorkspaceDirectoryType _
instance showWorkspaceDirectoryType :: Show WorkspaceDirectoryType where
  show = genericShow
instance decodeWorkspaceDirectoryType :: Decode WorkspaceDirectoryType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceDirectoryType :: Encode WorkspaceDirectoryType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceErrorCode = WorkspaceErrorCode String
derive instance newtypeWorkspaceErrorCode :: Newtype WorkspaceErrorCode _
derive instance repGenericWorkspaceErrorCode :: Generic WorkspaceErrorCode _
instance showWorkspaceErrorCode :: Show WorkspaceErrorCode where
  show = genericShow
instance decodeWorkspaceErrorCode :: Decode WorkspaceErrorCode where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceErrorCode :: Encode WorkspaceErrorCode where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceId = WorkspaceId String
derive instance newtypeWorkspaceId :: Newtype WorkspaceId _
derive instance repGenericWorkspaceId :: Generic WorkspaceId _
instance showWorkspaceId :: Show WorkspaceId where
  show = genericShow
instance decodeWorkspaceId :: Decode WorkspaceId where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceId :: Encode WorkspaceId where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceIdList = WorkspaceIdList (Array WorkspaceId)
derive instance newtypeWorkspaceIdList :: Newtype WorkspaceIdList _
derive instance repGenericWorkspaceIdList :: Generic WorkspaceIdList _
instance showWorkspaceIdList :: Show WorkspaceIdList where
  show = genericShow
instance decodeWorkspaceIdList :: Decode WorkspaceIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceIdList :: Encode WorkspaceIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceList = WorkspaceList (Array Workspace)
derive instance newtypeWorkspaceList :: Newtype WorkspaceList _
derive instance repGenericWorkspaceList :: Generic WorkspaceList _
instance showWorkspaceList :: Show WorkspaceList where
  show = genericShow
instance decodeWorkspaceList :: Decode WorkspaceList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceList :: Encode WorkspaceList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information about a WorkSpace.</p>
newtype WorkspaceProperties = WorkspaceProperties 
  { "RunningMode" :: NullOrUndefined.NullOrUndefined (RunningMode)
  , "RunningModeAutoStopTimeoutInMinutes" :: NullOrUndefined.NullOrUndefined (RunningModeAutoStopTimeoutInMinutes)
  , "RootVolumeSizeGib" :: NullOrUndefined.NullOrUndefined (RootVolumeSizeGib)
  , "UserVolumeSizeGib" :: NullOrUndefined.NullOrUndefined (UserVolumeSizeGib)
  , "ComputeTypeName" :: NullOrUndefined.NullOrUndefined (Compute)
  }
derive instance newtypeWorkspaceProperties :: Newtype WorkspaceProperties _
derive instance repGenericWorkspaceProperties :: Generic WorkspaceProperties _
instance showWorkspaceProperties :: Show WorkspaceProperties where
  show = genericShow
instance decodeWorkspaceProperties :: Decode WorkspaceProperties where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceProperties :: Encode WorkspaceProperties where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Information used to create a WorkSpace.</p>
newtype WorkspaceRequest = WorkspaceRequest 
  { "DirectoryId" :: (DirectoryId)
  , "UserName" :: (UserName)
  , "BundleId" :: (BundleId)
  , "VolumeEncryptionKey" :: NullOrUndefined.NullOrUndefined (VolumeEncryptionKey)
  , "UserVolumeEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "RootVolumeEncryptionEnabled" :: NullOrUndefined.NullOrUndefined (BooleanObject)
  , "WorkspaceProperties" :: NullOrUndefined.NullOrUndefined (WorkspaceProperties)
  , "Tags" :: NullOrUndefined.NullOrUndefined (TagList)
  }
derive instance newtypeWorkspaceRequest :: Newtype WorkspaceRequest _
derive instance repGenericWorkspaceRequest :: Generic WorkspaceRequest _
instance showWorkspaceRequest :: Show WorkspaceRequest where
  show = genericShow
instance decodeWorkspaceRequest :: Decode WorkspaceRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceRequest :: Encode WorkspaceRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceRequestList = WorkspaceRequestList (Array WorkspaceRequest)
derive instance newtypeWorkspaceRequestList :: Newtype WorkspaceRequestList _
derive instance repGenericWorkspaceRequestList :: Generic WorkspaceRequestList _
instance showWorkspaceRequestList :: Show WorkspaceRequestList where
  show = genericShow
instance decodeWorkspaceRequestList :: Decode WorkspaceRequestList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceRequestList :: Encode WorkspaceRequestList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype WorkspaceState = WorkspaceState String
derive instance newtypeWorkspaceState :: Newtype WorkspaceState _
derive instance repGenericWorkspaceState :: Generic WorkspaceState _
instance showWorkspaceState :: Show WorkspaceState where
  show = genericShow
instance decodeWorkspaceState :: Decode WorkspaceState where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeWorkspaceState :: Encode WorkspaceState where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
