

-- | <p>The WorkDocs API is designed for the following use cases:</p> <ul> <li> <p>File Migration: File migration applications are supported for users who want to migrate their files from an on-premises or off-premises file system or service. Users can insert files into a user directory structure, as well as allow for basic metadata changes, such as modifications to the permissions of files.</p> </li> <li> <p>Security: Support security applications are supported for users who have additional security needs, such as antivirus or data loss prevention. The API actions, along with AWS CloudTrail, allow these applications to detect when changes occur in Amazon WorkDocs. Then, the application can take the necessary actions and replace the target file. If the target file violates the policy, the application can also choose to email the user.</p> </li> <li> <p>eDiscovery/Analytics: General administrative applications are supported, such as eDiscovery and analytics. These applications can choose to mimic or record the actions in an Amazon WorkDocs site, along with AWS CloudTrail, to replicate data for eDiscovery, backup, or analytical applications.</p> </li> </ul> <p>All Amazon WorkDocs API actions are Amazon authenticated and certificate-signed. They not only require the use of the AWS SDK, but also allow for the exclusive use of IAM users and roles to help facilitate access, trust, and permission policies. By creating a role and allowing an IAM user to access the Amazon WorkDocs site, the IAM user gains full administrative visibility into the entire Amazon WorkDocs site (or as set in the IAM policy). This includes, but is not limited to, the ability to modify file permissions and upload any file to any user. This allows developers to perform the three use cases above, as well as give users the ability to grant access on a selective basis using the IAM model.</p>
module AWS.WorkDocs where

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

serviceName = "WorkDocs" :: String


-- | <p>Aborts the upload of the specified document version that was previously initiated by <a>InitiateDocumentVersionUpload</a>. The client should make this call only when it no longer intends to upload the document version, or fails to do so.</p>
abortDocumentVersionUpload :: forall eff. AbortDocumentVersionUploadRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
abortDocumentVersionUpload = Request.request serviceName "abortDocumentVersionUpload" 


-- | <p>Activates the specified user. Only active users can access Amazon WorkDocs.</p>
activateUser :: forall eff. ActivateUserRequest -> Aff (exception :: EXCEPTION | eff) ActivateUserResponse
activateUser = Request.request serviceName "activateUser" 


-- | <p>Creates a set of permissions for the specified folder or document. The resource permissions are overwritten if the principals already have different permissions.</p>
addResourcePermissions :: forall eff. AddResourcePermissionsRequest -> Aff (exception :: EXCEPTION | eff) AddResourcePermissionsResponse
addResourcePermissions = Request.request serviceName "addResourcePermissions" 


-- | <p>Adds a new comment to the specified document version.</p>
createComment :: forall eff. CreateCommentRequest -> Aff (exception :: EXCEPTION | eff) CreateCommentResponse
createComment = Request.request serviceName "createComment" 


-- | <p>Adds one or more custom properties to the specified resource (a folder, document, or version).</p>
createCustomMetadata :: forall eff. CreateCustomMetadataRequest -> Aff (exception :: EXCEPTION | eff) CreateCustomMetadataResponse
createCustomMetadata = Request.request serviceName "createCustomMetadata" 


-- | <p>Creates a folder with the specified name and parent folder.</p>
createFolder :: forall eff. CreateFolderRequest -> Aff (exception :: EXCEPTION | eff) CreateFolderResponse
createFolder = Request.request serviceName "createFolder" 


-- | <p>Adds the specified list of labels to the given resource (a document or folder)</p>
createLabels :: forall eff. CreateLabelsRequest -> Aff (exception :: EXCEPTION | eff) CreateLabelsResponse
createLabels = Request.request serviceName "createLabels" 


-- | <p>Configure WorkDocs to use Amazon SNS notifications.</p> <p>The endpoint receives a confirmation message, and must confirm the subscription. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SendMessageToHttp.html#SendMessageToHttp.confirm">Confirm the Subscription</a> in the <i>Amazon Simple Notification Service Developer Guide</i>.</p>
createNotificationSubscription :: forall eff. CreateNotificationSubscriptionRequest -> Aff (exception :: EXCEPTION | eff) CreateNotificationSubscriptionResponse
createNotificationSubscription = Request.request serviceName "createNotificationSubscription" 


-- | <p>Creates a user in a Simple AD or Microsoft AD directory. The status of a newly created user is "ACTIVE". New users can access Amazon WorkDocs.</p>
createUser :: forall eff. CreateUserRequest -> Aff (exception :: EXCEPTION | eff) CreateUserResponse
createUser = Request.request serviceName "createUser" 


-- | <p>Deactivates the specified user, which revokes the user's access to Amazon WorkDocs.</p>
deactivateUser :: forall eff. DeactivateUserRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deactivateUser = Request.request serviceName "deactivateUser" 


-- | <p>Deletes the specified comment from the document version.</p>
deleteComment :: forall eff. DeleteCommentRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteComment = Request.request serviceName "deleteComment" 


-- | <p>Deletes custom metadata from the specified resource.</p>
deleteCustomMetadata :: forall eff. DeleteCustomMetadataRequest -> Aff (exception :: EXCEPTION | eff) DeleteCustomMetadataResponse
deleteCustomMetadata = Request.request serviceName "deleteCustomMetadata" 


-- | <p>Permanently deletes the specified document and its associated metadata.</p>
deleteDocument :: forall eff. DeleteDocumentRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteDocument = Request.request serviceName "deleteDocument" 


-- | <p>Permanently deletes the specified folder and its contents.</p>
deleteFolder :: forall eff. DeleteFolderRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteFolder = Request.request serviceName "deleteFolder" 


-- | <p>Deletes the contents of the specified folder.</p>
deleteFolderContents :: forall eff. DeleteFolderContentsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteFolderContents = Request.request serviceName "deleteFolderContents" 


-- | <p>Deletes the specified list of labels from a resource.</p>
deleteLabels :: forall eff. DeleteLabelsRequest -> Aff (exception :: EXCEPTION | eff) DeleteLabelsResponse
deleteLabels = Request.request serviceName "deleteLabels" 


-- | <p>Deletes the specified subscription from the specified organization.</p>
deleteNotificationSubscription :: forall eff. DeleteNotificationSubscriptionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteNotificationSubscription = Request.request serviceName "deleteNotificationSubscription" 


-- | <p>Deletes the specified user from a Simple AD or Microsoft AD directory.</p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
deleteUser = Request.request serviceName "deleteUser" 


-- | <p>Describes the user activities in a specified time period.</p>
describeActivities :: forall eff. DescribeActivitiesRequest -> Aff (exception :: EXCEPTION | eff) DescribeActivitiesResponse
describeActivities = Request.request serviceName "describeActivities" 


-- | <p>List all the comments for the specified document version.</p>
describeComments :: forall eff. DescribeCommentsRequest -> Aff (exception :: EXCEPTION | eff) DescribeCommentsResponse
describeComments = Request.request serviceName "describeComments" 


-- | <p>Retrieves the document versions for the specified document.</p> <p>By default, only active versions are returned.</p>
describeDocumentVersions :: forall eff. DescribeDocumentVersionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeDocumentVersionsResponse
describeDocumentVersions = Request.request serviceName "describeDocumentVersions" 


-- | <p>Describes the contents of the specified folder, including its documents and subfolders.</p> <p>By default, Amazon WorkDocs returns the first 100 active document and folder metadata items. If there are more results, the response includes a marker that you can use to request the next set of results. You can also request initialized documents.</p>
describeFolderContents :: forall eff. DescribeFolderContentsRequest -> Aff (exception :: EXCEPTION | eff) DescribeFolderContentsResponse
describeFolderContents = Request.request serviceName "describeFolderContents" 


-- | <p>Describes the groups specified by query.</p>
describeGroups :: forall eff. DescribeGroupsRequest -> Aff (exception :: EXCEPTION | eff) DescribeGroupsResponse
describeGroups = Request.request serviceName "describeGroups" 


-- | <p>Lists the specified notification subscriptions.</p>
describeNotificationSubscriptions :: forall eff. DescribeNotificationSubscriptionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeNotificationSubscriptionsResponse
describeNotificationSubscriptions = Request.request serviceName "describeNotificationSubscriptions" 


-- | <p>Describes the permissions of a specified resource.</p>
describeResourcePermissions :: forall eff. DescribeResourcePermissionsRequest -> Aff (exception :: EXCEPTION | eff) DescribeResourcePermissionsResponse
describeResourcePermissions = Request.request serviceName "describeResourcePermissions" 


-- | <p>Describes the current user's special folders; the <code>RootFolder</code> and the <code>RecycleBin</code>. <code>RootFolder</code> is the root of user's files and folders and <code>RecycleBin</code> is the root of recycled items. This is not a valid action for SigV4 (administrative API) clients.</p>
describeRootFolders :: forall eff. DescribeRootFoldersRequest -> Aff (exception :: EXCEPTION | eff) DescribeRootFoldersResponse
describeRootFolders = Request.request serviceName "describeRootFolders" 


-- | <p>Describes the specified users. You can describe all users or filter the results (for example, by status or organization).</p> <p>By default, Amazon WorkDocs returns the first 24 active or pending users. If there are more results, the response includes a marker that you can use to request the next set of results.</p>
describeUsers :: forall eff. DescribeUsersRequest -> Aff (exception :: EXCEPTION | eff) DescribeUsersResponse
describeUsers = Request.request serviceName "describeUsers" 


-- | <p>Retrieves details of the current user for whom the authentication token was generated. This is not a valid action for SigV4 (administrative API) clients.</p>
getCurrentUser :: forall eff. GetCurrentUserRequest -> Aff (exception :: EXCEPTION | eff) GetCurrentUserResponse
getCurrentUser = Request.request serviceName "getCurrentUser" 


-- | <p>Retrieves details of a document.</p>
getDocument :: forall eff. GetDocumentRequest -> Aff (exception :: EXCEPTION | eff) GetDocumentResponse
getDocument = Request.request serviceName "getDocument" 


-- | <p>Retrieves the path information (the hierarchy from the root folder) for the requested document.</p> <p>By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested document and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the names of the parent folders.</p>
getDocumentPath :: forall eff. GetDocumentPathRequest -> Aff (exception :: EXCEPTION | eff) GetDocumentPathResponse
getDocumentPath = Request.request serviceName "getDocumentPath" 


-- | <p>Retrieves version metadata for the specified document.</p>
getDocumentVersion :: forall eff. GetDocumentVersionRequest -> Aff (exception :: EXCEPTION | eff) GetDocumentVersionResponse
getDocumentVersion = Request.request serviceName "getDocumentVersion" 


-- | <p>Retrieves the metadata of the specified folder.</p>
getFolder :: forall eff. GetFolderRequest -> Aff (exception :: EXCEPTION | eff) GetFolderResponse
getFolder = Request.request serviceName "getFolder" 


-- | <p>Retrieves the path information (the hierarchy from the root folder) for the specified folder.</p> <p>By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested folder and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the parent folder names.</p>
getFolderPath :: forall eff. GetFolderPathRequest -> Aff (exception :: EXCEPTION | eff) GetFolderPathResponse
getFolderPath = Request.request serviceName "getFolderPath" 


-- | <p>Creates a new document object and version object.</p> <p>The client specifies the parent folder ID and name of the document to upload. The ID is optionally specified when creating a new version of an existing document. This is the first step to upload a document. Next, upload the document to the URL returned from the call, and then call <a>UpdateDocumentVersion</a>.</p> <p>To cancel the document upload, call <a>AbortDocumentVersionUpload</a>.</p>
initiateDocumentVersionUpload :: forall eff. InitiateDocumentVersionUploadRequest -> Aff (exception :: EXCEPTION | eff) InitiateDocumentVersionUploadResponse
initiateDocumentVersionUpload = Request.request serviceName "initiateDocumentVersionUpload" 


-- | <p>Removes all the permissions from the specified resource.</p>
removeAllResourcePermissions :: forall eff. RemoveAllResourcePermissionsRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removeAllResourcePermissions = Request.request serviceName "removeAllResourcePermissions" 


-- | <p>Removes the permission for the specified principal from the specified resource.</p>
removeResourcePermission :: forall eff. RemoveResourcePermissionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
removeResourcePermission = Request.request serviceName "removeResourcePermission" 


-- | <p>Updates the specified attributes of a document. The user must have access to both the document and its parent folder, if applicable.</p>
updateDocument :: forall eff. UpdateDocumentRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateDocument = Request.request serviceName "updateDocument" 


-- | <p>Changes the status of the document version to ACTIVE. </p> <p>Amazon WorkDocs also sets its document container to ACTIVE. This is the last step in a document upload, after the client uploads the document to an S3-presigned URL returned by <a>InitiateDocumentVersionUpload</a>. </p>
updateDocumentVersion :: forall eff. UpdateDocumentVersionRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateDocumentVersion = Request.request serviceName "updateDocumentVersion" 


-- | <p>Updates the specified attributes of the specified folder. The user must have access to both the folder and its parent folder, if applicable.</p>
updateFolder :: forall eff. UpdateFolderRequest -> Aff (exception :: EXCEPTION | eff) Types.NoOutput
updateFolder = Request.request serviceName "updateFolder" 


-- | <p>Updates the specified attributes of the specified user, and grants or revokes administrative privileges to the Amazon WorkDocs site.</p>
updateUser :: forall eff. UpdateUserRequest -> Aff (exception :: EXCEPTION | eff) UpdateUserResponse
updateUser = Request.request serviceName "updateUser" 


newtype AbortDocumentVersionUploadRequest = AbortDocumentVersionUploadRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  }
derive instance newtypeAbortDocumentVersionUploadRequest :: Newtype AbortDocumentVersionUploadRequest _
derive instance repGenericAbortDocumentVersionUploadRequest :: Generic AbortDocumentVersionUploadRequest _
instance showAbortDocumentVersionUploadRequest :: Show AbortDocumentVersionUploadRequest where
  show = genericShow
instance decodeAbortDocumentVersionUploadRequest :: Decode AbortDocumentVersionUploadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAbortDocumentVersionUploadRequest :: Encode AbortDocumentVersionUploadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivateUserRequest = ActivateUserRequest 
  { "UserId" :: (IdType)
  , "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  }
derive instance newtypeActivateUserRequest :: Newtype ActivateUserRequest _
derive instance repGenericActivateUserRequest :: Generic ActivateUserRequest _
instance showActivateUserRequest :: Show ActivateUserRequest where
  show = genericShow
instance decodeActivateUserRequest :: Decode ActivateUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivateUserRequest :: Encode ActivateUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivateUserResponse = ActivateUserResponse 
  { "User" :: NullOrUndefined.NullOrUndefined (User)
  }
derive instance newtypeActivateUserResponse :: Newtype ActivateUserResponse _
derive instance repGenericActivateUserResponse :: Generic ActivateUserResponse _
instance showActivateUserResponse :: Show ActivateUserResponse where
  show = genericShow
instance decodeActivateUserResponse :: Decode ActivateUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivateUserResponse :: Encode ActivateUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the activity information.</p>
newtype Activity = Activity 
  { "Type" :: NullOrUndefined.NullOrUndefined (ActivityType)
  , "TimeStamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "OrganizationId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Initiator" :: NullOrUndefined.NullOrUndefined (UserMetadata)
  , "Participants" :: NullOrUndefined.NullOrUndefined (Participants)
  , "ResourceMetadata" :: NullOrUndefined.NullOrUndefined (ResourceMetadata)
  , "OriginalParent" :: NullOrUndefined.NullOrUndefined (ResourceMetadata)
  , "CommentMetadata" :: NullOrUndefined.NullOrUndefined (CommentMetadata)
  }
derive instance newtypeActivity :: Newtype Activity _
derive instance repGenericActivity :: Generic Activity _
instance showActivity :: Show Activity where
  show = genericShow
instance decodeActivity :: Decode Activity where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivity :: Encode Activity where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ActivityType = ActivityType String
derive instance newtypeActivityType :: Newtype ActivityType _
derive instance repGenericActivityType :: Generic ActivityType _
instance showActivityType :: Show ActivityType where
  show = genericShow
instance decodeActivityType :: Decode ActivityType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeActivityType :: Encode ActivityType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddResourcePermissionsRequest = AddResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "Principals" :: (SharePrincipalList)
  , "NotificationOptions" :: NullOrUndefined.NullOrUndefined (NotificationOptions)
  }
derive instance newtypeAddResourcePermissionsRequest :: Newtype AddResourcePermissionsRequest _
derive instance repGenericAddResourcePermissionsRequest :: Generic AddResourcePermissionsRequest _
instance showAddResourcePermissionsRequest :: Show AddResourcePermissionsRequest where
  show = genericShow
instance decodeAddResourcePermissionsRequest :: Decode AddResourcePermissionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddResourcePermissionsRequest :: Encode AddResourcePermissionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AddResourcePermissionsResponse = AddResourcePermissionsResponse 
  { "ShareResults" :: NullOrUndefined.NullOrUndefined (ShareResultsList)
  }
derive instance newtypeAddResourcePermissionsResponse :: Newtype AddResourcePermissionsResponse _
derive instance repGenericAddResourcePermissionsResponse :: Generic AddResourcePermissionsResponse _
instance showAddResourcePermissionsResponse :: Show AddResourcePermissionsResponse where
  show = genericShow
instance decodeAddResourcePermissionsResponse :: Decode AddResourcePermissionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAddResourcePermissionsResponse :: Encode AddResourcePermissionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype AuthenticationHeaderType = AuthenticationHeaderType String
derive instance newtypeAuthenticationHeaderType :: Newtype AuthenticationHeaderType _
derive instance repGenericAuthenticationHeaderType :: Generic AuthenticationHeaderType _
instance showAuthenticationHeaderType :: Show AuthenticationHeaderType where
  show = genericShow
instance decodeAuthenticationHeaderType :: Decode AuthenticationHeaderType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeAuthenticationHeaderType :: Encode AuthenticationHeaderType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanEnumType = BooleanEnumType String
derive instance newtypeBooleanEnumType :: Newtype BooleanEnumType _
derive instance repGenericBooleanEnumType :: Generic BooleanEnumType _
instance showBooleanEnumType :: Show BooleanEnumType where
  show = genericShow
instance decodeBooleanEnumType :: Decode BooleanEnumType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanEnumType :: Encode BooleanEnumType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype BooleanType = BooleanType Boolean
derive instance newtypeBooleanType :: Newtype BooleanType _
derive instance repGenericBooleanType :: Generic BooleanType _
instance showBooleanType :: Show BooleanType where
  show = genericShow
instance decodeBooleanType :: Decode BooleanType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeBooleanType :: Encode BooleanType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a comment.</p>
newtype Comment = Comment 
  { "CommentId" :: (CommentIdType)
  , "ParentId" :: NullOrUndefined.NullOrUndefined (CommentIdType)
  , "ThreadId" :: NullOrUndefined.NullOrUndefined (CommentIdType)
  , "Text" :: NullOrUndefined.NullOrUndefined (CommentTextType)
  , "Contributor" :: NullOrUndefined.NullOrUndefined (User)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "Status" :: NullOrUndefined.NullOrUndefined (CommentStatusType)
  , "Visibility" :: NullOrUndefined.NullOrUndefined (CommentVisibilityType)
  , "RecipientId" :: NullOrUndefined.NullOrUndefined (IdType)
  }
derive instance newtypeComment :: Newtype Comment _
derive instance repGenericComment :: Generic Comment _
instance showComment :: Show Comment where
  show = genericShow
instance decodeComment :: Decode Comment where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeComment :: Encode Comment where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentIdType = CommentIdType String
derive instance newtypeCommentIdType :: Newtype CommentIdType _
derive instance repGenericCommentIdType :: Generic CommentIdType _
instance showCommentIdType :: Show CommentIdType where
  show = genericShow
instance decodeCommentIdType :: Decode CommentIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentIdType :: Encode CommentIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentList = CommentList (Array Comment)
derive instance newtypeCommentList :: Newtype CommentList _
derive instance repGenericCommentList :: Generic CommentList _
instance showCommentList :: Show CommentList where
  show = genericShow
instance decodeCommentList :: Decode CommentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentList :: Encode CommentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the metadata of a comment.</p>
newtype CommentMetadata = CommentMetadata 
  { "CommentId" :: NullOrUndefined.NullOrUndefined (CommentIdType)
  , "Contributor" :: NullOrUndefined.NullOrUndefined (User)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "CommentStatus" :: NullOrUndefined.NullOrUndefined (CommentStatusType)
  , "RecipientId" :: NullOrUndefined.NullOrUndefined (IdType)
  }
derive instance newtypeCommentMetadata :: Newtype CommentMetadata _
derive instance repGenericCommentMetadata :: Generic CommentMetadata _
instance showCommentMetadata :: Show CommentMetadata where
  show = genericShow
instance decodeCommentMetadata :: Decode CommentMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentMetadata :: Encode CommentMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentStatusType = CommentStatusType String
derive instance newtypeCommentStatusType :: Newtype CommentStatusType _
derive instance repGenericCommentStatusType :: Generic CommentStatusType _
instance showCommentStatusType :: Show CommentStatusType where
  show = genericShow
instance decodeCommentStatusType :: Decode CommentStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentStatusType :: Encode CommentStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentTextType = CommentTextType String
derive instance newtypeCommentTextType :: Newtype CommentTextType _
derive instance repGenericCommentTextType :: Generic CommentTextType _
instance showCommentTextType :: Show CommentTextType where
  show = genericShow
instance decodeCommentTextType :: Decode CommentTextType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentTextType :: Encode CommentTextType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CommentVisibilityType = CommentVisibilityType String
derive instance newtypeCommentVisibilityType :: Newtype CommentVisibilityType _
derive instance repGenericCommentVisibilityType :: Generic CommentVisibilityType _
instance showCommentVisibilityType :: Show CommentVisibilityType where
  show = genericShow
instance decodeCommentVisibilityType :: Decode CommentVisibilityType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCommentVisibilityType :: Encode CommentVisibilityType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource hierarchy is changing.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _
derive instance repGenericConcurrentModificationException :: Generic ConcurrentModificationException _
instance showConcurrentModificationException :: Show ConcurrentModificationException where
  show = genericShow
instance decodeConcurrentModificationException :: Decode ConcurrentModificationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeConcurrentModificationException :: Encode ConcurrentModificationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCommentRequest = CreateCommentRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "ParentId" :: NullOrUndefined.NullOrUndefined (CommentIdType)
  , "ThreadId" :: NullOrUndefined.NullOrUndefined (CommentIdType)
  , "Text" :: (CommentTextType)
  , "Visibility" :: NullOrUndefined.NullOrUndefined (CommentVisibilityType)
  , "NotifyCollaborators" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeCreateCommentRequest :: Newtype CreateCommentRequest _
derive instance repGenericCreateCommentRequest :: Generic CreateCommentRequest _
instance showCreateCommentRequest :: Show CreateCommentRequest where
  show = genericShow
instance decodeCreateCommentRequest :: Decode CreateCommentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCommentRequest :: Encode CreateCommentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCommentResponse = CreateCommentResponse 
  { "Comment" :: NullOrUndefined.NullOrUndefined (Comment)
  }
derive instance newtypeCreateCommentResponse :: Newtype CreateCommentResponse _
derive instance repGenericCreateCommentResponse :: Generic CreateCommentResponse _
instance showCreateCommentResponse :: Show CreateCommentResponse where
  show = genericShow
instance decodeCreateCommentResponse :: Decode CreateCommentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCommentResponse :: Encode CreateCommentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCustomMetadataRequest = CreateCustomMetadataRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (DocumentVersionIdType)
  , "CustomMetadata" :: (CustomMetadataMap)
  }
derive instance newtypeCreateCustomMetadataRequest :: Newtype CreateCustomMetadataRequest _
derive instance repGenericCreateCustomMetadataRequest :: Generic CreateCustomMetadataRequest _
instance showCreateCustomMetadataRequest :: Show CreateCustomMetadataRequest where
  show = genericShow
instance decodeCreateCustomMetadataRequest :: Decode CreateCustomMetadataRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCustomMetadataRequest :: Encode CreateCustomMetadataRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateCustomMetadataResponse = CreateCustomMetadataResponse Types.NoArguments
derive instance newtypeCreateCustomMetadataResponse :: Newtype CreateCustomMetadataResponse _
derive instance repGenericCreateCustomMetadataResponse :: Generic CreateCustomMetadataResponse _
instance showCreateCustomMetadataResponse :: Show CreateCustomMetadataResponse where
  show = genericShow
instance decodeCreateCustomMetadataResponse :: Decode CreateCustomMetadataResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateCustomMetadataResponse :: Encode CreateCustomMetadataResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateFolderRequest = CreateFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: (ResourceIdType)
  }
derive instance newtypeCreateFolderRequest :: Newtype CreateFolderRequest _
derive instance repGenericCreateFolderRequest :: Generic CreateFolderRequest _
instance showCreateFolderRequest :: Show CreateFolderRequest where
  show = genericShow
instance decodeCreateFolderRequest :: Decode CreateFolderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFolderRequest :: Encode CreateFolderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateFolderResponse = CreateFolderResponse 
  { "Metadata" :: NullOrUndefined.NullOrUndefined (FolderMetadata)
  }
derive instance newtypeCreateFolderResponse :: Newtype CreateFolderResponse _
derive instance repGenericCreateFolderResponse :: Generic CreateFolderResponse _
instance showCreateFolderResponse :: Show CreateFolderResponse where
  show = genericShow
instance decodeCreateFolderResponse :: Decode CreateFolderResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateFolderResponse :: Encode CreateFolderResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLabelsRequest = CreateLabelsRequest 
  { "ResourceId" :: (ResourceIdType)
  , "Labels" :: (SharedLabels)
  , "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  }
derive instance newtypeCreateLabelsRequest :: Newtype CreateLabelsRequest _
derive instance repGenericCreateLabelsRequest :: Generic CreateLabelsRequest _
instance showCreateLabelsRequest :: Show CreateLabelsRequest where
  show = genericShow
instance decodeCreateLabelsRequest :: Decode CreateLabelsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLabelsRequest :: Encode CreateLabelsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateLabelsResponse = CreateLabelsResponse Types.NoArguments
derive instance newtypeCreateLabelsResponse :: Newtype CreateLabelsResponse _
derive instance repGenericCreateLabelsResponse :: Generic CreateLabelsResponse _
instance showCreateLabelsResponse :: Show CreateLabelsResponse where
  show = genericShow
instance decodeCreateLabelsResponse :: Decode CreateLabelsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateLabelsResponse :: Encode CreateLabelsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateNotificationSubscriptionRequest = CreateNotificationSubscriptionRequest 
  { "OrganizationId" :: (IdType)
  , "Endpoint" :: (SubscriptionEndPointType)
  , "Protocol" :: (SubscriptionProtocolType)
  , "SubscriptionType" :: (SubscriptionType)
  }
derive instance newtypeCreateNotificationSubscriptionRequest :: Newtype CreateNotificationSubscriptionRequest _
derive instance repGenericCreateNotificationSubscriptionRequest :: Generic CreateNotificationSubscriptionRequest _
instance showCreateNotificationSubscriptionRequest :: Show CreateNotificationSubscriptionRequest where
  show = genericShow
instance decodeCreateNotificationSubscriptionRequest :: Decode CreateNotificationSubscriptionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateNotificationSubscriptionRequest :: Encode CreateNotificationSubscriptionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateNotificationSubscriptionResponse = CreateNotificationSubscriptionResponse 
  { "Subscription" :: NullOrUndefined.NullOrUndefined (Subscription)
  }
derive instance newtypeCreateNotificationSubscriptionResponse :: Newtype CreateNotificationSubscriptionResponse _
derive instance repGenericCreateNotificationSubscriptionResponse :: Generic CreateNotificationSubscriptionResponse _
instance showCreateNotificationSubscriptionResponse :: Show CreateNotificationSubscriptionResponse where
  show = genericShow
instance decodeCreateNotificationSubscriptionResponse :: Decode CreateNotificationSubscriptionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateNotificationSubscriptionResponse :: Encode CreateNotificationSubscriptionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateUserRequest = CreateUserRequest 
  { "OrganizationId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Username" :: (UsernameType)
  , "EmailAddress" :: NullOrUndefined.NullOrUndefined (EmailAddressType)
  , "GivenName" :: (UserAttributeValueType)
  , "Surname" :: (UserAttributeValueType)
  , "Password" :: (PasswordType)
  , "TimeZoneId" :: NullOrUndefined.NullOrUndefined (TimeZoneIdType)
  , "StorageRule" :: NullOrUndefined.NullOrUndefined (StorageRuleType)
  , "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  }
derive instance newtypeCreateUserRequest :: Newtype CreateUserRequest _
derive instance repGenericCreateUserRequest :: Generic CreateUserRequest _
instance showCreateUserRequest :: Show CreateUserRequest where
  show = genericShow
instance decodeCreateUserRequest :: Decode CreateUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserRequest :: Encode CreateUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CreateUserResponse = CreateUserResponse 
  { "User" :: NullOrUndefined.NullOrUndefined (User)
  }
derive instance newtypeCreateUserResponse :: Newtype CreateUserResponse _
derive instance repGenericCreateUserResponse :: Generic CreateUserResponse _
instance showCreateUserResponse :: Show CreateUserResponse where
  show = genericShow
instance decodeCreateUserResponse :: Decode CreateUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCreateUserResponse :: Encode CreateUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomMetadataKeyList = CustomMetadataKeyList (Array CustomMetadataKeyType)
derive instance newtypeCustomMetadataKeyList :: Newtype CustomMetadataKeyList _
derive instance repGenericCustomMetadataKeyList :: Generic CustomMetadataKeyList _
instance showCustomMetadataKeyList :: Show CustomMetadataKeyList where
  show = genericShow
instance decodeCustomMetadataKeyList :: Decode CustomMetadataKeyList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomMetadataKeyList :: Encode CustomMetadataKeyList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomMetadataKeyType = CustomMetadataKeyType String
derive instance newtypeCustomMetadataKeyType :: Newtype CustomMetadataKeyType _
derive instance repGenericCustomMetadataKeyType :: Generic CustomMetadataKeyType _
instance showCustomMetadataKeyType :: Show CustomMetadataKeyType where
  show = genericShow
instance decodeCustomMetadataKeyType :: Decode CustomMetadataKeyType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomMetadataKeyType :: Encode CustomMetadataKeyType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The limit has been reached on the number of custom properties for the specified resource.</p>
newtype CustomMetadataLimitExceededException = CustomMetadataLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeCustomMetadataLimitExceededException :: Newtype CustomMetadataLimitExceededException _
derive instance repGenericCustomMetadataLimitExceededException :: Generic CustomMetadataLimitExceededException _
instance showCustomMetadataLimitExceededException :: Show CustomMetadataLimitExceededException where
  show = genericShow
instance decodeCustomMetadataLimitExceededException :: Decode CustomMetadataLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomMetadataLimitExceededException :: Encode CustomMetadataLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomMetadataMap = CustomMetadataMap (StrMap.StrMap CustomMetadataValueType)
derive instance newtypeCustomMetadataMap :: Newtype CustomMetadataMap _
derive instance repGenericCustomMetadataMap :: Generic CustomMetadataMap _
instance showCustomMetadataMap :: Show CustomMetadataMap where
  show = genericShow
instance decodeCustomMetadataMap :: Decode CustomMetadataMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomMetadataMap :: Encode CustomMetadataMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype CustomMetadataValueType = CustomMetadataValueType String
derive instance newtypeCustomMetadataValueType :: Newtype CustomMetadataValueType _
derive instance repGenericCustomMetadataValueType :: Generic CustomMetadataValueType _
instance showCustomMetadataValueType :: Show CustomMetadataValueType where
  show = genericShow
instance decodeCustomMetadataValueType :: Decode CustomMetadataValueType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeCustomMetadataValueType :: Encode CustomMetadataValueType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeactivateUserRequest = DeactivateUserRequest 
  { "UserId" :: (IdType)
  , "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  }
derive instance newtypeDeactivateUserRequest :: Newtype DeactivateUserRequest _
derive instance repGenericDeactivateUserRequest :: Generic DeactivateUserRequest _
instance showDeactivateUserRequest :: Show DeactivateUserRequest where
  show = genericShow
instance decodeDeactivateUserRequest :: Decode DeactivateUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeactivateUserRequest :: Encode DeactivateUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The last user in the organization is being deactivated.</p>
newtype DeactivatingLastSystemUserException = DeactivatingLastSystemUserException Types.NoArguments
derive instance newtypeDeactivatingLastSystemUserException :: Newtype DeactivatingLastSystemUserException _
derive instance repGenericDeactivatingLastSystemUserException :: Generic DeactivatingLastSystemUserException _
instance showDeactivatingLastSystemUserException :: Show DeactivatingLastSystemUserException where
  show = genericShow
instance decodeDeactivatingLastSystemUserException :: Decode DeactivatingLastSystemUserException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeactivatingLastSystemUserException :: Encode DeactivatingLastSystemUserException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCommentRequest = DeleteCommentRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "CommentId" :: (CommentIdType)
  }
derive instance newtypeDeleteCommentRequest :: Newtype DeleteCommentRequest _
derive instance repGenericDeleteCommentRequest :: Generic DeleteCommentRequest _
instance showDeleteCommentRequest :: Show DeleteCommentRequest where
  show = genericShow
instance decodeDeleteCommentRequest :: Decode DeleteCommentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCommentRequest :: Encode DeleteCommentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCustomMetadataRequest = DeleteCustomMetadataRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (DocumentVersionIdType)
  , "Keys" :: NullOrUndefined.NullOrUndefined (CustomMetadataKeyList)
  , "DeleteAll" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeDeleteCustomMetadataRequest :: Newtype DeleteCustomMetadataRequest _
derive instance repGenericDeleteCustomMetadataRequest :: Generic DeleteCustomMetadataRequest _
instance showDeleteCustomMetadataRequest :: Show DeleteCustomMetadataRequest where
  show = genericShow
instance decodeDeleteCustomMetadataRequest :: Decode DeleteCustomMetadataRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCustomMetadataRequest :: Encode DeleteCustomMetadataRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteCustomMetadataResponse = DeleteCustomMetadataResponse Types.NoArguments
derive instance newtypeDeleteCustomMetadataResponse :: Newtype DeleteCustomMetadataResponse _
derive instance repGenericDeleteCustomMetadataResponse :: Generic DeleteCustomMetadataResponse _
instance showDeleteCustomMetadataResponse :: Show DeleteCustomMetadataResponse where
  show = genericShow
instance decodeDeleteCustomMetadataResponse :: Decode DeleteCustomMetadataResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteCustomMetadataResponse :: Encode DeleteCustomMetadataResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteDocumentRequest = DeleteDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  }
derive instance newtypeDeleteDocumentRequest :: Newtype DeleteDocumentRequest _
derive instance repGenericDeleteDocumentRequest :: Generic DeleteDocumentRequest _
instance showDeleteDocumentRequest :: Show DeleteDocumentRequest where
  show = genericShow
instance decodeDeleteDocumentRequest :: Decode DeleteDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteDocumentRequest :: Encode DeleteDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteFolderContentsRequest = DeleteFolderContentsRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  }
derive instance newtypeDeleteFolderContentsRequest :: Newtype DeleteFolderContentsRequest _
derive instance repGenericDeleteFolderContentsRequest :: Generic DeleteFolderContentsRequest _
instance showDeleteFolderContentsRequest :: Show DeleteFolderContentsRequest where
  show = genericShow
instance decodeDeleteFolderContentsRequest :: Decode DeleteFolderContentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFolderContentsRequest :: Encode DeleteFolderContentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteFolderRequest = DeleteFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  }
derive instance newtypeDeleteFolderRequest :: Newtype DeleteFolderRequest _
derive instance repGenericDeleteFolderRequest :: Generic DeleteFolderRequest _
instance showDeleteFolderRequest :: Show DeleteFolderRequest where
  show = genericShow
instance decodeDeleteFolderRequest :: Decode DeleteFolderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteFolderRequest :: Encode DeleteFolderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLabelsRequest = DeleteLabelsRequest 
  { "ResourceId" :: (ResourceIdType)
  , "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "Labels" :: NullOrUndefined.NullOrUndefined (SharedLabels)
  , "DeleteAll" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeDeleteLabelsRequest :: Newtype DeleteLabelsRequest _
derive instance repGenericDeleteLabelsRequest :: Generic DeleteLabelsRequest _
instance showDeleteLabelsRequest :: Show DeleteLabelsRequest where
  show = genericShow
instance decodeDeleteLabelsRequest :: Decode DeleteLabelsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLabelsRequest :: Encode DeleteLabelsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteLabelsResponse = DeleteLabelsResponse Types.NoArguments
derive instance newtypeDeleteLabelsResponse :: Newtype DeleteLabelsResponse _
derive instance repGenericDeleteLabelsResponse :: Generic DeleteLabelsResponse _
instance showDeleteLabelsResponse :: Show DeleteLabelsResponse where
  show = genericShow
instance decodeDeleteLabelsResponse :: Decode DeleteLabelsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteLabelsResponse :: Encode DeleteLabelsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteNotificationSubscriptionRequest = DeleteNotificationSubscriptionRequest 
  { "SubscriptionId" :: (IdType)
  , "OrganizationId" :: (IdType)
  }
derive instance newtypeDeleteNotificationSubscriptionRequest :: Newtype DeleteNotificationSubscriptionRequest _
derive instance repGenericDeleteNotificationSubscriptionRequest :: Generic DeleteNotificationSubscriptionRequest _
instance showDeleteNotificationSubscriptionRequest :: Show DeleteNotificationSubscriptionRequest where
  show = genericShow
instance decodeDeleteNotificationSubscriptionRequest :: Decode DeleteNotificationSubscriptionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteNotificationSubscriptionRequest :: Encode DeleteNotificationSubscriptionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DeleteUserRequest = DeleteUserRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "UserId" :: (IdType)
  }
derive instance newtypeDeleteUserRequest :: Newtype DeleteUserRequest _
derive instance repGenericDeleteUserRequest :: Generic DeleteUserRequest _
instance showDeleteUserRequest :: Show DeleteUserRequest where
  show = genericShow
instance decodeDeleteUserRequest :: Decode DeleteUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDeleteUserRequest :: Encode DeleteUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivitiesRequest = DescribeActivitiesRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "StartTime" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "EndTime" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "OrganizationId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "UserId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeActivitiesRequest :: Newtype DescribeActivitiesRequest _
derive instance repGenericDescribeActivitiesRequest :: Generic DescribeActivitiesRequest _
instance showDescribeActivitiesRequest :: Show DescribeActivitiesRequest where
  show = genericShow
instance decodeDescribeActivitiesRequest :: Decode DescribeActivitiesRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivitiesRequest :: Encode DescribeActivitiesRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeActivitiesResponse = DescribeActivitiesResponse 
  { "UserActivities" :: NullOrUndefined.NullOrUndefined (UserActivities)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeActivitiesResponse :: Newtype DescribeActivitiesResponse _
derive instance repGenericDescribeActivitiesResponse :: Generic DescribeActivitiesResponse _
instance showDescribeActivitiesResponse :: Show DescribeActivitiesResponse where
  show = genericShow
instance decodeDescribeActivitiesResponse :: Decode DescribeActivitiesResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeActivitiesResponse :: Encode DescribeActivitiesResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeCommentsRequest = DescribeCommentsRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeCommentsRequest :: Newtype DescribeCommentsRequest _
derive instance repGenericDescribeCommentsRequest :: Generic DescribeCommentsRequest _
instance showDescribeCommentsRequest :: Show DescribeCommentsRequest where
  show = genericShow
instance decodeDescribeCommentsRequest :: Decode DescribeCommentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCommentsRequest :: Encode DescribeCommentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeCommentsResponse = DescribeCommentsResponse 
  { "Comments" :: NullOrUndefined.NullOrUndefined (CommentList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeCommentsResponse :: Newtype DescribeCommentsResponse _
derive instance repGenericDescribeCommentsResponse :: Generic DescribeCommentsResponse _
instance showDescribeCommentsResponse :: Show DescribeCommentsResponse where
  show = genericShow
instance decodeDescribeCommentsResponse :: Decode DescribeCommentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeCommentsResponse :: Encode DescribeCommentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDocumentVersionsRequest = DescribeDocumentVersionsRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Include" :: NullOrUndefined.NullOrUndefined (FieldNamesType)
  , "Fields" :: NullOrUndefined.NullOrUndefined (FieldNamesType)
  }
derive instance newtypeDescribeDocumentVersionsRequest :: Newtype DescribeDocumentVersionsRequest _
derive instance repGenericDescribeDocumentVersionsRequest :: Generic DescribeDocumentVersionsRequest _
instance showDescribeDocumentVersionsRequest :: Show DescribeDocumentVersionsRequest where
  show = genericShow
instance decodeDescribeDocumentVersionsRequest :: Decode DescribeDocumentVersionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDocumentVersionsRequest :: Encode DescribeDocumentVersionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeDocumentVersionsResponse = DescribeDocumentVersionsResponse 
  { "DocumentVersions" :: NullOrUndefined.NullOrUndefined (DocumentVersionMetadataList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeDocumentVersionsResponse :: Newtype DescribeDocumentVersionsResponse _
derive instance repGenericDescribeDocumentVersionsResponse :: Generic DescribeDocumentVersionsResponse _
instance showDescribeDocumentVersionsResponse :: Show DescribeDocumentVersionsResponse where
  show = genericShow
instance decodeDescribeDocumentVersionsResponse :: Decode DescribeDocumentVersionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeDocumentVersionsResponse :: Encode DescribeDocumentVersionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeFolderContentsRequest = DescribeFolderContentsRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  , "Sort" :: NullOrUndefined.NullOrUndefined (ResourceSortType)
  , "Order" :: NullOrUndefined.NullOrUndefined (OrderType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  , "Type" :: NullOrUndefined.NullOrUndefined (FolderContentType)
  , "Include" :: NullOrUndefined.NullOrUndefined (FieldNamesType)
  }
derive instance newtypeDescribeFolderContentsRequest :: Newtype DescribeFolderContentsRequest _
derive instance repGenericDescribeFolderContentsRequest :: Generic DescribeFolderContentsRequest _
instance showDescribeFolderContentsRequest :: Show DescribeFolderContentsRequest where
  show = genericShow
instance decodeDescribeFolderContentsRequest :: Decode DescribeFolderContentsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFolderContentsRequest :: Encode DescribeFolderContentsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeFolderContentsResponse = DescribeFolderContentsResponse 
  { "Folders" :: NullOrUndefined.NullOrUndefined (FolderMetadataList)
  , "Documents" :: NullOrUndefined.NullOrUndefined (DocumentMetadataList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeFolderContentsResponse :: Newtype DescribeFolderContentsResponse _
derive instance repGenericDescribeFolderContentsResponse :: Generic DescribeFolderContentsResponse _
instance showDescribeFolderContentsResponse :: Show DescribeFolderContentsResponse where
  show = genericShow
instance decodeDescribeFolderContentsResponse :: Decode DescribeFolderContentsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeFolderContentsResponse :: Encode DescribeFolderContentsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeGroupsRequest = DescribeGroupsRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "SearchQuery" :: (SearchQueryType)
  , "OrganizationId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (PositiveIntegerType)
  }
derive instance newtypeDescribeGroupsRequest :: Newtype DescribeGroupsRequest _
derive instance repGenericDescribeGroupsRequest :: Generic DescribeGroupsRequest _
instance showDescribeGroupsRequest :: Show DescribeGroupsRequest where
  show = genericShow
instance decodeDescribeGroupsRequest :: Decode DescribeGroupsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGroupsRequest :: Encode DescribeGroupsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeGroupsResponse = DescribeGroupsResponse 
  { "Groups" :: NullOrUndefined.NullOrUndefined (GroupMetadataList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeGroupsResponse :: Newtype DescribeGroupsResponse _
derive instance repGenericDescribeGroupsResponse :: Generic DescribeGroupsResponse _
instance showDescribeGroupsResponse :: Show DescribeGroupsResponse where
  show = genericShow
instance decodeDescribeGroupsResponse :: Decode DescribeGroupsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeGroupsResponse :: Encode DescribeGroupsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeNotificationSubscriptionsRequest = DescribeNotificationSubscriptionsRequest 
  { "OrganizationId" :: (IdType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  }
derive instance newtypeDescribeNotificationSubscriptionsRequest :: Newtype DescribeNotificationSubscriptionsRequest _
derive instance repGenericDescribeNotificationSubscriptionsRequest :: Generic DescribeNotificationSubscriptionsRequest _
instance showDescribeNotificationSubscriptionsRequest :: Show DescribeNotificationSubscriptionsRequest where
  show = genericShow
instance decodeDescribeNotificationSubscriptionsRequest :: Decode DescribeNotificationSubscriptionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeNotificationSubscriptionsRequest :: Encode DescribeNotificationSubscriptionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeNotificationSubscriptionsResponse = DescribeNotificationSubscriptionsResponse 
  { "Subscriptions" :: NullOrUndefined.NullOrUndefined (SubscriptionList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeNotificationSubscriptionsResponse :: Newtype DescribeNotificationSubscriptionsResponse _
derive instance repGenericDescribeNotificationSubscriptionsResponse :: Generic DescribeNotificationSubscriptionsResponse _
instance showDescribeNotificationSubscriptionsResponse :: Show DescribeNotificationSubscriptionsResponse where
  show = genericShow
instance decodeDescribeNotificationSubscriptionsResponse :: Decode DescribeNotificationSubscriptionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeNotificationSubscriptionsResponse :: Encode DescribeNotificationSubscriptionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeResourcePermissionsRequest = DescribeResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "PrincipalId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeResourcePermissionsRequest :: Newtype DescribeResourcePermissionsRequest _
derive instance repGenericDescribeResourcePermissionsRequest :: Generic DescribeResourcePermissionsRequest _
instance showDescribeResourcePermissionsRequest :: Show DescribeResourcePermissionsRequest where
  show = genericShow
instance decodeDescribeResourcePermissionsRequest :: Decode DescribeResourcePermissionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResourcePermissionsRequest :: Encode DescribeResourcePermissionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeResourcePermissionsResponse = DescribeResourcePermissionsResponse 
  { "Principals" :: NullOrUndefined.NullOrUndefined (PrincipalList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeResourcePermissionsResponse :: Newtype DescribeResourcePermissionsResponse _
derive instance repGenericDescribeResourcePermissionsResponse :: Generic DescribeResourcePermissionsResponse _
instance showDescribeResourcePermissionsResponse :: Show DescribeResourcePermissionsResponse where
  show = genericShow
instance decodeDescribeResourcePermissionsResponse :: Decode DescribeResourcePermissionsResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeResourcePermissionsResponse :: Encode DescribeResourcePermissionsResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRootFoldersRequest = DescribeRootFoldersRequest 
  { "AuthenticationToken" :: (AuthenticationHeaderType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeRootFoldersRequest :: Newtype DescribeRootFoldersRequest _
derive instance repGenericDescribeRootFoldersRequest :: Generic DescribeRootFoldersRequest _
instance showDescribeRootFoldersRequest :: Show DescribeRootFoldersRequest where
  show = genericShow
instance decodeDescribeRootFoldersRequest :: Decode DescribeRootFoldersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRootFoldersRequest :: Encode DescribeRootFoldersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeRootFoldersResponse = DescribeRootFoldersResponse 
  { "Folders" :: NullOrUndefined.NullOrUndefined (FolderMetadataList)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeRootFoldersResponse :: Newtype DescribeRootFoldersResponse _
derive instance repGenericDescribeRootFoldersResponse :: Generic DescribeRootFoldersResponse _
instance showDescribeRootFoldersResponse :: Show DescribeRootFoldersResponse where
  show = genericShow
instance decodeDescribeRootFoldersResponse :: Decode DescribeRootFoldersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeRootFoldersResponse :: Encode DescribeRootFoldersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeUsersRequest = DescribeUsersRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "OrganizationId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "UserIds" :: NullOrUndefined.NullOrUndefined (UserIdsType)
  , "Query" :: NullOrUndefined.NullOrUndefined (SearchQueryType)
  , "Include" :: NullOrUndefined.NullOrUndefined (UserFilterType)
  , "Order" :: NullOrUndefined.NullOrUndefined (OrderType)
  , "Sort" :: NullOrUndefined.NullOrUndefined (UserSortType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Fields" :: NullOrUndefined.NullOrUndefined (FieldNamesType)
  }
derive instance newtypeDescribeUsersRequest :: Newtype DescribeUsersRequest _
derive instance repGenericDescribeUsersRequest :: Generic DescribeUsersRequest _
instance showDescribeUsersRequest :: Show DescribeUsersRequest where
  show = genericShow
instance decodeDescribeUsersRequest :: Decode DescribeUsersRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUsersRequest :: Encode DescribeUsersRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DescribeUsersResponse = DescribeUsersResponse 
  { "Users" :: NullOrUndefined.NullOrUndefined (OrganizationUserList)
  , "TotalNumberOfUsers" :: NullOrUndefined.NullOrUndefined (SizeType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeUsersResponse :: Newtype DescribeUsersResponse _
derive instance repGenericDescribeUsersResponse :: Generic DescribeUsersResponse _
instance showDescribeUsersResponse :: Show DescribeUsersResponse where
  show = genericShow
instance decodeDescribeUsersResponse :: Decode DescribeUsersResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDescribeUsersResponse :: Encode DescribeUsersResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentContentType = DocumentContentType String
derive instance newtypeDocumentContentType :: Newtype DocumentContentType _
derive instance repGenericDocumentContentType :: Generic DocumentContentType _
instance showDocumentContentType :: Show DocumentContentType where
  show = genericShow
instance decodeDocumentContentType :: Decode DocumentContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentContentType :: Encode DocumentContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when the document is locked for comments and user tries to create or delete a comment on that document.</p>
newtype DocumentLockedForCommentsException = DocumentLockedForCommentsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeDocumentLockedForCommentsException :: Newtype DocumentLockedForCommentsException _
derive instance repGenericDocumentLockedForCommentsException :: Generic DocumentLockedForCommentsException _
instance showDocumentLockedForCommentsException :: Show DocumentLockedForCommentsException where
  show = genericShow
instance decodeDocumentLockedForCommentsException :: Decode DocumentLockedForCommentsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentLockedForCommentsException :: Encode DocumentLockedForCommentsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the document.</p>
newtype DocumentMetadata = DocumentMetadata 
  { "Id" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "CreatorId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "ParentFolderId" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "LatestVersionMetadata" :: NullOrUndefined.NullOrUndefined (DocumentVersionMetadata)
  , "ResourceState" :: NullOrUndefined.NullOrUndefined (ResourceStateType)
  , "Labels" :: NullOrUndefined.NullOrUndefined (SharedLabels)
  }
derive instance newtypeDocumentMetadata :: Newtype DocumentMetadata _
derive instance repGenericDocumentMetadata :: Generic DocumentMetadata _
instance showDocumentMetadata :: Show DocumentMetadata where
  show = genericShow
instance decodeDocumentMetadata :: Decode DocumentMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentMetadata :: Encode DocumentMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentMetadataList = DocumentMetadataList (Array DocumentMetadata)
derive instance newtypeDocumentMetadataList :: Newtype DocumentMetadataList _
derive instance repGenericDocumentMetadataList :: Generic DocumentMetadataList _
instance showDocumentMetadataList :: Show DocumentMetadataList where
  show = genericShow
instance decodeDocumentMetadataList :: Decode DocumentMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentMetadataList :: Encode DocumentMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentSourceType = DocumentSourceType String
derive instance newtypeDocumentSourceType :: Newtype DocumentSourceType _
derive instance repGenericDocumentSourceType :: Generic DocumentSourceType _
instance showDocumentSourceType :: Show DocumentSourceType where
  show = genericShow
instance decodeDocumentSourceType :: Decode DocumentSourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentSourceType :: Encode DocumentSourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentSourceUrlMap = DocumentSourceUrlMap (StrMap.StrMap UrlType)
derive instance newtypeDocumentSourceUrlMap :: Newtype DocumentSourceUrlMap _
derive instance repGenericDocumentSourceUrlMap :: Generic DocumentSourceUrlMap _
instance showDocumentSourceUrlMap :: Show DocumentSourceUrlMap where
  show = genericShow
instance decodeDocumentSourceUrlMap :: Decode DocumentSourceUrlMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentSourceUrlMap :: Encode DocumentSourceUrlMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentStatusType = DocumentStatusType String
derive instance newtypeDocumentStatusType :: Newtype DocumentStatusType _
derive instance repGenericDocumentStatusType :: Generic DocumentStatusType _
instance showDocumentStatusType :: Show DocumentStatusType where
  show = genericShow
instance decodeDocumentStatusType :: Decode DocumentStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentStatusType :: Encode DocumentStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentThumbnailType = DocumentThumbnailType String
derive instance newtypeDocumentThumbnailType :: Newtype DocumentThumbnailType _
derive instance repGenericDocumentThumbnailType :: Generic DocumentThumbnailType _
instance showDocumentThumbnailType :: Show DocumentThumbnailType where
  show = genericShow
instance decodeDocumentThumbnailType :: Decode DocumentThumbnailType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentThumbnailType :: Encode DocumentThumbnailType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentThumbnailUrlMap = DocumentThumbnailUrlMap (StrMap.StrMap UrlType)
derive instance newtypeDocumentThumbnailUrlMap :: Newtype DocumentThumbnailUrlMap _
derive instance repGenericDocumentThumbnailUrlMap :: Generic DocumentThumbnailUrlMap _
instance showDocumentThumbnailUrlMap :: Show DocumentThumbnailUrlMap where
  show = genericShow
instance decodeDocumentThumbnailUrlMap :: Decode DocumentThumbnailUrlMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentThumbnailUrlMap :: Encode DocumentThumbnailUrlMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentVersionIdType = DocumentVersionIdType String
derive instance newtypeDocumentVersionIdType :: Newtype DocumentVersionIdType _
derive instance repGenericDocumentVersionIdType :: Generic DocumentVersionIdType _
instance showDocumentVersionIdType :: Show DocumentVersionIdType where
  show = genericShow
instance decodeDocumentVersionIdType :: Decode DocumentVersionIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersionIdType :: Encode DocumentVersionIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a version of a document.</p>
newtype DocumentVersionMetadata = DocumentVersionMetadata 
  { "Id" :: NullOrUndefined.NullOrUndefined (DocumentVersionIdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  , "ContentType" :: NullOrUndefined.NullOrUndefined (DocumentContentType)
  , "Size" :: NullOrUndefined.NullOrUndefined (SizeType)
  , "Signature" :: NullOrUndefined.NullOrUndefined (HashType)
  , "Status" :: NullOrUndefined.NullOrUndefined (DocumentStatusType)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ContentCreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ContentModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "CreatorId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Thumbnail" :: NullOrUndefined.NullOrUndefined (DocumentThumbnailUrlMap)
  , "Source" :: NullOrUndefined.NullOrUndefined (DocumentSourceUrlMap)
  }
derive instance newtypeDocumentVersionMetadata :: Newtype DocumentVersionMetadata _
derive instance repGenericDocumentVersionMetadata :: Generic DocumentVersionMetadata _
instance showDocumentVersionMetadata :: Show DocumentVersionMetadata where
  show = genericShow
instance decodeDocumentVersionMetadata :: Decode DocumentVersionMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersionMetadata :: Encode DocumentVersionMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentVersionMetadataList = DocumentVersionMetadataList (Array DocumentVersionMetadata)
derive instance newtypeDocumentVersionMetadataList :: Newtype DocumentVersionMetadataList _
derive instance repGenericDocumentVersionMetadataList :: Generic DocumentVersionMetadataList _
instance showDocumentVersionMetadataList :: Show DocumentVersionMetadataList where
  show = genericShow
instance decodeDocumentVersionMetadataList :: Decode DocumentVersionMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersionMetadataList :: Encode DocumentVersionMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype DocumentVersionStatus = DocumentVersionStatus String
derive instance newtypeDocumentVersionStatus :: Newtype DocumentVersionStatus _
derive instance repGenericDocumentVersionStatus :: Generic DocumentVersionStatus _
instance showDocumentVersionStatus :: Show DocumentVersionStatus where
  show = genericShow
instance decodeDocumentVersionStatus :: Decode DocumentVersionStatus where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDocumentVersionStatus :: Encode DocumentVersionStatus where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>This exception is thrown when a valid checkout ID is not presented on document version upload calls for a document that has been checked out from Web client.</p>
newtype DraftUploadOutOfSyncException = DraftUploadOutOfSyncException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeDraftUploadOutOfSyncException :: Newtype DraftUploadOutOfSyncException _
derive instance repGenericDraftUploadOutOfSyncException :: Generic DraftUploadOutOfSyncException _
instance showDraftUploadOutOfSyncException :: Show DraftUploadOutOfSyncException where
  show = genericShow
instance decodeDraftUploadOutOfSyncException :: Decode DraftUploadOutOfSyncException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeDraftUploadOutOfSyncException :: Encode DraftUploadOutOfSyncException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EmailAddressType = EmailAddressType String
derive instance newtypeEmailAddressType :: Newtype EmailAddressType _
derive instance repGenericEmailAddressType :: Generic EmailAddressType _
instance showEmailAddressType :: Show EmailAddressType where
  show = genericShow
instance decodeEmailAddressType :: Decode EmailAddressType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEmailAddressType :: Encode EmailAddressType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource already exists.</p>
newtype EntityAlreadyExistsException = EntityAlreadyExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeEntityAlreadyExistsException :: Newtype EntityAlreadyExistsException _
derive instance repGenericEntityAlreadyExistsException :: Generic EntityAlreadyExistsException _
instance showEntityAlreadyExistsException :: Show EntityAlreadyExistsException where
  show = genericShow
instance decodeEntityAlreadyExistsException :: Decode EntityAlreadyExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityAlreadyExistsException :: Encode EntityAlreadyExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype EntityIdList = EntityIdList (Array IdType)
derive instance newtypeEntityIdList :: Newtype EntityIdList _
derive instance repGenericEntityIdList :: Generic EntityIdList _
instance showEntityIdList :: Show EntityIdList where
  show = genericShow
instance decodeEntityIdList :: Decode EntityIdList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityIdList :: Encode EntityIdList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource does not exist.</p>
newtype EntityNotExistsException = EntityNotExistsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  , "EntityIds" :: NullOrUndefined.NullOrUndefined (EntityIdList)
  }
derive instance newtypeEntityNotExistsException :: Newtype EntityNotExistsException _
derive instance repGenericEntityNotExistsException :: Generic EntityNotExistsException _
instance showEntityNotExistsException :: Show EntityNotExistsException where
  show = genericShow
instance decodeEntityNotExistsException :: Decode EntityNotExistsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeEntityNotExistsException :: Encode EntityNotExistsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ErrorMessageType = ErrorMessageType String
derive instance newtypeErrorMessageType :: Newtype ErrorMessageType _
derive instance repGenericErrorMessageType :: Generic ErrorMessageType _
instance showErrorMessageType :: Show ErrorMessageType where
  show = genericShow
instance decodeErrorMessageType :: Decode ErrorMessageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeErrorMessageType :: Encode ErrorMessageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The AWS Directory Service cannot reach an on-premises instance. Or a dependency under the control of the organization is failing, such as a connected Active Directory.</p>
newtype FailedDependencyException = FailedDependencyException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeFailedDependencyException :: Newtype FailedDependencyException _
derive instance repGenericFailedDependencyException :: Generic FailedDependencyException _
instance showFailedDependencyException :: Show FailedDependencyException where
  show = genericShow
instance decodeFailedDependencyException :: Decode FailedDependencyException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFailedDependencyException :: Encode FailedDependencyException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FieldNamesType = FieldNamesType String
derive instance newtypeFieldNamesType :: Newtype FieldNamesType _
derive instance repGenericFieldNamesType :: Generic FieldNamesType _
instance showFieldNamesType :: Show FieldNamesType where
  show = genericShow
instance decodeFieldNamesType :: Decode FieldNamesType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFieldNamesType :: Encode FieldNamesType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FolderContentType = FolderContentType String
derive instance newtypeFolderContentType :: Newtype FolderContentType _
derive instance repGenericFolderContentType :: Generic FolderContentType _
instance showFolderContentType :: Show FolderContentType where
  show = genericShow
instance decodeFolderContentType :: Decode FolderContentType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFolderContentType :: Encode FolderContentType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a folder.</p>
newtype FolderMetadata = FolderMetadata 
  { "Id" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  , "CreatorId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "ParentFolderId" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ResourceState" :: NullOrUndefined.NullOrUndefined (ResourceStateType)
  , "Signature" :: NullOrUndefined.NullOrUndefined (HashType)
  , "Labels" :: NullOrUndefined.NullOrUndefined (SharedLabels)
  , "Size" :: NullOrUndefined.NullOrUndefined (SizeType)
  , "LatestVersionSize" :: NullOrUndefined.NullOrUndefined (SizeType)
  }
derive instance newtypeFolderMetadata :: Newtype FolderMetadata _
derive instance repGenericFolderMetadata :: Generic FolderMetadata _
instance showFolderMetadata :: Show FolderMetadata where
  show = genericShow
instance decodeFolderMetadata :: Decode FolderMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFolderMetadata :: Encode FolderMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype FolderMetadataList = FolderMetadataList (Array FolderMetadata)
derive instance newtypeFolderMetadataList :: Newtype FolderMetadataList _
derive instance repGenericFolderMetadataList :: Generic FolderMetadataList _
instance showFolderMetadataList :: Show FolderMetadataList where
  show = genericShow
instance decodeFolderMetadataList :: Decode FolderMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeFolderMetadataList :: Encode FolderMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCurrentUserRequest = GetCurrentUserRequest 
  { "AuthenticationToken" :: (AuthenticationHeaderType)
  }
derive instance newtypeGetCurrentUserRequest :: Newtype GetCurrentUserRequest _
derive instance repGenericGetCurrentUserRequest :: Generic GetCurrentUserRequest _
instance showGetCurrentUserRequest :: Show GetCurrentUserRequest where
  show = genericShow
instance decodeGetCurrentUserRequest :: Decode GetCurrentUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCurrentUserRequest :: Encode GetCurrentUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetCurrentUserResponse = GetCurrentUserResponse 
  { "User" :: NullOrUndefined.NullOrUndefined (User)
  }
derive instance newtypeGetCurrentUserResponse :: Newtype GetCurrentUserResponse _
derive instance repGenericGetCurrentUserResponse :: Generic GetCurrentUserResponse _
instance showGetCurrentUserResponse :: Show GetCurrentUserResponse where
  show = genericShow
instance decodeGetCurrentUserResponse :: Decode GetCurrentUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetCurrentUserResponse :: Encode GetCurrentUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDocumentPathRequest = GetDocumentPathRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (IdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Fields" :: NullOrUndefined.NullOrUndefined (FieldNamesType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeGetDocumentPathRequest :: Newtype GetDocumentPathRequest _
derive instance repGenericGetDocumentPathRequest :: Generic GetDocumentPathRequest _
instance showGetDocumentPathRequest :: Show GetDocumentPathRequest where
  show = genericShow
instance decodeGetDocumentPathRequest :: Decode GetDocumentPathRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentPathRequest :: Encode GetDocumentPathRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDocumentPathResponse = GetDocumentPathResponse 
  { "Path" :: NullOrUndefined.NullOrUndefined (ResourcePath)
  }
derive instance newtypeGetDocumentPathResponse :: Newtype GetDocumentPathResponse _
derive instance repGenericGetDocumentPathResponse :: Generic GetDocumentPathResponse _
instance showGetDocumentPathResponse :: Show GetDocumentPathResponse where
  show = genericShow
instance decodeGetDocumentPathResponse :: Decode GetDocumentPathResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentPathResponse :: Encode GetDocumentPathResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDocumentRequest = GetDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "IncludeCustomMetadata" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeGetDocumentRequest :: Newtype GetDocumentRequest _
derive instance repGenericGetDocumentRequest :: Generic GetDocumentRequest _
instance showGetDocumentRequest :: Show GetDocumentRequest where
  show = genericShow
instance decodeGetDocumentRequest :: Decode GetDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentRequest :: Encode GetDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDocumentResponse = GetDocumentResponse 
  { "Metadata" :: NullOrUndefined.NullOrUndefined (DocumentMetadata)
  , "CustomMetadata" :: NullOrUndefined.NullOrUndefined (CustomMetadataMap)
  }
derive instance newtypeGetDocumentResponse :: Newtype GetDocumentResponse _
derive instance repGenericGetDocumentResponse :: Generic GetDocumentResponse _
instance showGetDocumentResponse :: Show GetDocumentResponse where
  show = genericShow
instance decodeGetDocumentResponse :: Decode GetDocumentResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentResponse :: Encode GetDocumentResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDocumentVersionRequest = GetDocumentVersionRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "Fields" :: NullOrUndefined.NullOrUndefined (FieldNamesType)
  , "IncludeCustomMetadata" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeGetDocumentVersionRequest :: Newtype GetDocumentVersionRequest _
derive instance repGenericGetDocumentVersionRequest :: Generic GetDocumentVersionRequest _
instance showGetDocumentVersionRequest :: Show GetDocumentVersionRequest where
  show = genericShow
instance decodeGetDocumentVersionRequest :: Decode GetDocumentVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentVersionRequest :: Encode GetDocumentVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetDocumentVersionResponse = GetDocumentVersionResponse 
  { "Metadata" :: NullOrUndefined.NullOrUndefined (DocumentVersionMetadata)
  , "CustomMetadata" :: NullOrUndefined.NullOrUndefined (CustomMetadataMap)
  }
derive instance newtypeGetDocumentVersionResponse :: Newtype GetDocumentVersionResponse _
derive instance repGenericGetDocumentVersionResponse :: Generic GetDocumentVersionResponse _
instance showGetDocumentVersionResponse :: Show GetDocumentVersionResponse where
  show = genericShow
instance decodeGetDocumentVersionResponse :: Decode GetDocumentVersionResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetDocumentVersionResponse :: Encode GetDocumentVersionResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFolderPathRequest = GetFolderPathRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (IdType)
  , "Limit" :: NullOrUndefined.NullOrUndefined (LimitType)
  , "Fields" :: NullOrUndefined.NullOrUndefined (FieldNamesType)
  , "Marker" :: NullOrUndefined.NullOrUndefined (PageMarkerType)
  }
derive instance newtypeGetFolderPathRequest :: Newtype GetFolderPathRequest _
derive instance repGenericGetFolderPathRequest :: Generic GetFolderPathRequest _
instance showGetFolderPathRequest :: Show GetFolderPathRequest where
  show = genericShow
instance decodeGetFolderPathRequest :: Decode GetFolderPathRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFolderPathRequest :: Encode GetFolderPathRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFolderPathResponse = GetFolderPathResponse 
  { "Path" :: NullOrUndefined.NullOrUndefined (ResourcePath)
  }
derive instance newtypeGetFolderPathResponse :: Newtype GetFolderPathResponse _
derive instance repGenericGetFolderPathResponse :: Generic GetFolderPathResponse _
instance showGetFolderPathResponse :: Show GetFolderPathResponse where
  show = genericShow
instance decodeGetFolderPathResponse :: Decode GetFolderPathResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFolderPathResponse :: Encode GetFolderPathResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFolderRequest = GetFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  , "IncludeCustomMetadata" :: NullOrUndefined.NullOrUndefined (BooleanType)
  }
derive instance newtypeGetFolderRequest :: Newtype GetFolderRequest _
derive instance repGenericGetFolderRequest :: Generic GetFolderRequest _
instance showGetFolderRequest :: Show GetFolderRequest where
  show = genericShow
instance decodeGetFolderRequest :: Decode GetFolderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFolderRequest :: Encode GetFolderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GetFolderResponse = GetFolderResponse 
  { "Metadata" :: NullOrUndefined.NullOrUndefined (FolderMetadata)
  , "CustomMetadata" :: NullOrUndefined.NullOrUndefined (CustomMetadataMap)
  }
derive instance newtypeGetFolderResponse :: Newtype GetFolderResponse _
derive instance repGenericGetFolderResponse :: Generic GetFolderResponse _
instance showGetFolderResponse :: Show GetFolderResponse where
  show = genericShow
instance decodeGetFolderResponse :: Decode GetFolderResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGetFolderResponse :: Encode GetFolderResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the metadata of a user group.</p>
newtype GroupMetadata = GroupMetadata 
  { "Id" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (GroupNameType)
  }
derive instance newtypeGroupMetadata :: Newtype GroupMetadata _
derive instance repGenericGroupMetadata :: Generic GroupMetadata _
instance showGroupMetadata :: Show GroupMetadata where
  show = genericShow
instance decodeGroupMetadata :: Decode GroupMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupMetadata :: Encode GroupMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupMetadataList = GroupMetadataList (Array GroupMetadata)
derive instance newtypeGroupMetadataList :: Newtype GroupMetadataList _
derive instance repGenericGroupMetadataList :: Generic GroupMetadataList _
instance showGroupMetadataList :: Show GroupMetadataList where
  show = genericShow
instance decodeGroupMetadataList :: Decode GroupMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupMetadataList :: Encode GroupMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype GroupNameType = GroupNameType String
derive instance newtypeGroupNameType :: Newtype GroupNameType _
derive instance repGenericGroupNameType :: Generic GroupNameType _
instance showGroupNameType :: Show GroupNameType where
  show = genericShow
instance decodeGroupNameType :: Decode GroupNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeGroupNameType :: Encode GroupNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HashType = HashType String
derive instance newtypeHashType :: Newtype HashType _
derive instance repGenericHashType :: Generic HashType _
instance showHashType :: Show HashType where
  show = genericShow
instance decodeHashType :: Decode HashType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHashType :: Encode HashType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HeaderNameType = HeaderNameType String
derive instance newtypeHeaderNameType :: Newtype HeaderNameType _
derive instance repGenericHeaderNameType :: Generic HeaderNameType _
instance showHeaderNameType :: Show HeaderNameType where
  show = genericShow
instance decodeHeaderNameType :: Decode HeaderNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeaderNameType :: Encode HeaderNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype HeaderValueType = HeaderValueType String
derive instance newtypeHeaderValueType :: Newtype HeaderValueType _
derive instance repGenericHeaderValueType :: Generic HeaderValueType _
instance showHeaderValueType :: Show HeaderValueType where
  show = genericShow
instance decodeHeaderValueType :: Decode HeaderValueType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeHeaderValueType :: Encode HeaderValueType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype IdType = IdType String
derive instance newtypeIdType :: Newtype IdType _
derive instance repGenericIdType :: Generic IdType _
instance showIdType :: Show IdType where
  show = genericShow
instance decodeIdType :: Decode IdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIdType :: Encode IdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The user is undergoing transfer of ownership.</p>
newtype IllegalUserStateException = IllegalUserStateException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeIllegalUserStateException :: Newtype IllegalUserStateException _
derive instance repGenericIllegalUserStateException :: Generic IllegalUserStateException _
instance showIllegalUserStateException :: Show IllegalUserStateException where
  show = genericShow
instance decodeIllegalUserStateException :: Decode IllegalUserStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeIllegalUserStateException :: Encode IllegalUserStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InitiateDocumentVersionUploadRequest = InitiateDocumentVersionUploadRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "Id" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  , "ContentCreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ContentModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ContentType" :: NullOrUndefined.NullOrUndefined (DocumentContentType)
  , "DocumentSizeInBytes" :: NullOrUndefined.NullOrUndefined (SizeType)
  , "ParentFolderId" :: (ResourceIdType)
  }
derive instance newtypeInitiateDocumentVersionUploadRequest :: Newtype InitiateDocumentVersionUploadRequest _
derive instance repGenericInitiateDocumentVersionUploadRequest :: Generic InitiateDocumentVersionUploadRequest _
instance showInitiateDocumentVersionUploadRequest :: Show InitiateDocumentVersionUploadRequest where
  show = genericShow
instance decodeInitiateDocumentVersionUploadRequest :: Decode InitiateDocumentVersionUploadRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateDocumentVersionUploadRequest :: Encode InitiateDocumentVersionUploadRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype InitiateDocumentVersionUploadResponse = InitiateDocumentVersionUploadResponse 
  { "Metadata" :: NullOrUndefined.NullOrUndefined (DocumentMetadata)
  , "UploadMetadata" :: NullOrUndefined.NullOrUndefined (UploadMetadata)
  }
derive instance newtypeInitiateDocumentVersionUploadResponse :: Newtype InitiateDocumentVersionUploadResponse _
derive instance repGenericInitiateDocumentVersionUploadResponse :: Generic InitiateDocumentVersionUploadResponse _
instance showInitiateDocumentVersionUploadResponse :: Show InitiateDocumentVersionUploadResponse where
  show = genericShow
instance decodeInitiateDocumentVersionUploadResponse :: Decode InitiateDocumentVersionUploadResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInitiateDocumentVersionUploadResponse :: Encode InitiateDocumentVersionUploadResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The pagination marker or limit fields are not valid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _
derive instance repGenericInvalidArgumentException :: Generic InvalidArgumentException _
instance showInvalidArgumentException :: Show InvalidArgumentException where
  show = genericShow
instance decodeInvalidArgumentException :: Decode InvalidArgumentException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidArgumentException :: Encode InvalidArgumentException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation is invalid.</p>
newtype InvalidOperationException = InvalidOperationException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidOperationException :: Newtype InvalidOperationException _
derive instance repGenericInvalidOperationException :: Generic InvalidOperationException _
instance showInvalidOperationException :: Show InvalidOperationException where
  show = genericShow
instance decodeInvalidOperationException :: Decode InvalidOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidOperationException :: Encode InvalidOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The password is invalid.</p>
newtype InvalidPasswordException = InvalidPasswordException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidPasswordException :: Newtype InvalidPasswordException _
derive instance repGenericInvalidPasswordException :: Generic InvalidPasswordException _
instance showInvalidPasswordException :: Show InvalidPasswordException where
  show = genericShow
instance decodeInvalidPasswordException :: Decode InvalidPasswordException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeInvalidPasswordException :: Encode InvalidPasswordException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The maximum of 100,000 folders under the parent folder has been exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _
derive instance repGenericLimitExceededException :: Generic LimitExceededException _
instance showLimitExceededException :: Show LimitExceededException where
  show = genericShow
instance decodeLimitExceededException :: Decode LimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitExceededException :: Encode LimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LimitType = LimitType Int
derive instance newtypeLimitType :: Newtype LimitType _
derive instance repGenericLimitType :: Generic LimitType _
instance showLimitType :: Show LimitType where
  show = genericShow
instance decodeLimitType :: Decode LimitType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLimitType :: Encode LimitType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype LocaleType = LocaleType String
derive instance newtypeLocaleType :: Newtype LocaleType _
derive instance repGenericLocaleType :: Generic LocaleType _
instance showLocaleType :: Show LocaleType where
  show = genericShow
instance decodeLocaleType :: Decode LocaleType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeLocaleType :: Encode LocaleType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MarkerType = MarkerType String
derive instance newtypeMarkerType :: Newtype MarkerType _
derive instance repGenericMarkerType :: Generic MarkerType _
instance showMarkerType :: Show MarkerType where
  show = genericShow
instance decodeMarkerType :: Decode MarkerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMarkerType :: Encode MarkerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype MessageType = MessageType String
derive instance newtypeMessageType :: Newtype MessageType _
derive instance repGenericMessageType :: Generic MessageType _
instance showMessageType :: Show MessageType where
  show = genericShow
instance decodeMessageType :: Decode MessageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeMessageType :: Encode MessageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Set of options which defines notification preferences of given action.</p>
newtype NotificationOptions = NotificationOptions 
  { "SendEmail" :: NullOrUndefined.NullOrUndefined (BooleanType)
  , "EmailMessage" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeNotificationOptions :: Newtype NotificationOptions _
derive instance repGenericNotificationOptions :: Generic NotificationOptions _
instance showNotificationOptions :: Show NotificationOptions where
  show = genericShow
instance decodeNotificationOptions :: Decode NotificationOptions where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeNotificationOptions :: Encode NotificationOptions where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrderType = OrderType String
derive instance newtypeOrderType :: Newtype OrderType _
derive instance repGenericOrderType :: Generic OrderType _
instance showOrderType :: Show OrderType where
  show = genericShow
instance decodeOrderType :: Decode OrderType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrderType :: Encode OrderType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype OrganizationUserList = OrganizationUserList (Array User)
derive instance newtypeOrganizationUserList :: Newtype OrganizationUserList _
derive instance repGenericOrganizationUserList :: Generic OrganizationUserList _
instance showOrganizationUserList :: Show OrganizationUserList where
  show = genericShow
instance decodeOrganizationUserList :: Decode OrganizationUserList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeOrganizationUserList :: Encode OrganizationUserList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PageMarkerType = PageMarkerType String
derive instance newtypePageMarkerType :: Newtype PageMarkerType _
derive instance repGenericPageMarkerType :: Generic PageMarkerType _
instance showPageMarkerType :: Show PageMarkerType where
  show = genericShow
instance decodePageMarkerType :: Decode PageMarkerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePageMarkerType :: Encode PageMarkerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the users or user groups.</p>
newtype Participants = Participants 
  { "Users" :: NullOrUndefined.NullOrUndefined (UserMetadataList)
  , "Groups" :: NullOrUndefined.NullOrUndefined (GroupMetadataList)
  }
derive instance newtypeParticipants :: Newtype Participants _
derive instance repGenericParticipants :: Generic Participants _
instance showParticipants :: Show Participants where
  show = genericShow
instance decodeParticipants :: Decode Participants where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeParticipants :: Encode Participants where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PasswordType = PasswordType String
derive instance newtypePasswordType :: Newtype PasswordType _
derive instance repGenericPasswordType :: Generic PasswordType _
instance showPasswordType :: Show PasswordType where
  show = genericShow
instance decodePasswordType :: Decode PasswordType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePasswordType :: Encode PasswordType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the permissions.</p>
newtype PermissionInfo = PermissionInfo 
  { "Role" :: NullOrUndefined.NullOrUndefined (RoleType)
  , "Type" :: NullOrUndefined.NullOrUndefined (RolePermissionType)
  }
derive instance newtypePermissionInfo :: Newtype PermissionInfo _
derive instance repGenericPermissionInfo :: Generic PermissionInfo _
instance showPermissionInfo :: Show PermissionInfo where
  show = genericShow
instance decodePermissionInfo :: Decode PermissionInfo where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePermissionInfo :: Encode PermissionInfo where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PermissionInfoList = PermissionInfoList (Array PermissionInfo)
derive instance newtypePermissionInfoList :: Newtype PermissionInfoList _
derive instance repGenericPermissionInfoList :: Generic PermissionInfoList _
instance showPermissionInfoList :: Show PermissionInfoList where
  show = genericShow
instance decodePermissionInfoList :: Decode PermissionInfoList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePermissionInfoList :: Encode PermissionInfoList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PositiveIntegerType = PositiveIntegerType Int
derive instance newtypePositiveIntegerType :: Newtype PositiveIntegerType _
derive instance repGenericPositiveIntegerType :: Generic PositiveIntegerType _
instance showPositiveIntegerType :: Show PositiveIntegerType where
  show = genericShow
instance decodePositiveIntegerType :: Decode PositiveIntegerType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePositiveIntegerType :: Encode PositiveIntegerType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PositiveSizeType = PositiveSizeType Number
derive instance newtypePositiveSizeType :: Newtype PositiveSizeType _
derive instance repGenericPositiveSizeType :: Generic PositiveSizeType _
instance showPositiveSizeType :: Show PositiveSizeType where
  show = genericShow
instance decodePositiveSizeType :: Decode PositiveSizeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePositiveSizeType :: Encode PositiveSizeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a resource.</p>
newtype Principal = Principal 
  { "Id" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Type" :: NullOrUndefined.NullOrUndefined (PrincipalType)
  , "Roles" :: NullOrUndefined.NullOrUndefined (PermissionInfoList)
  }
derive instance newtypePrincipal :: Newtype Principal _
derive instance repGenericPrincipal :: Generic Principal _
instance showPrincipal :: Show Principal where
  show = genericShow
instance decodePrincipal :: Decode Principal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipal :: Encode Principal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PrincipalList = PrincipalList (Array Principal)
derive instance newtypePrincipalList :: Newtype PrincipalList _
derive instance repGenericPrincipalList :: Generic PrincipalList _
instance showPrincipalList :: Show PrincipalList where
  show = genericShow
instance decodePrincipalList :: Decode PrincipalList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipalList :: Encode PrincipalList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype PrincipalType = PrincipalType String
derive instance newtypePrincipalType :: Newtype PrincipalType _
derive instance repGenericPrincipalType :: Generic PrincipalType _
instance showPrincipalType :: Show PrincipalType where
  show = genericShow
instance decodePrincipalType :: Decode PrincipalType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodePrincipalType :: Encode PrincipalType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The specified document version is not in the INITIALIZED state.</p>
newtype ProhibitedStateException = ProhibitedStateException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeProhibitedStateException :: Newtype ProhibitedStateException _
derive instance repGenericProhibitedStateException :: Generic ProhibitedStateException _
instance showProhibitedStateException :: Show ProhibitedStateException where
  show = genericShow
instance decodeProhibitedStateException :: Decode ProhibitedStateException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeProhibitedStateException :: Encode ProhibitedStateException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveAllResourcePermissionsRequest = RemoveAllResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  }
derive instance newtypeRemoveAllResourcePermissionsRequest :: Newtype RemoveAllResourcePermissionsRequest _
derive instance repGenericRemoveAllResourcePermissionsRequest :: Generic RemoveAllResourcePermissionsRequest _
instance showRemoveAllResourcePermissionsRequest :: Show RemoveAllResourcePermissionsRequest where
  show = genericShow
instance decodeRemoveAllResourcePermissionsRequest :: Decode RemoveAllResourcePermissionsRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveAllResourcePermissionsRequest :: Encode RemoveAllResourcePermissionsRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RemoveResourcePermissionRequest = RemoveResourcePermissionRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "PrincipalId" :: (IdType)
  , "PrincipalType" :: NullOrUndefined.NullOrUndefined (PrincipalType)
  }
derive instance newtypeRemoveResourcePermissionRequest :: Newtype RemoveResourcePermissionRequest _
derive instance repGenericRemoveResourcePermissionRequest :: Generic RemoveResourcePermissionRequest _
instance showRemoveResourcePermissionRequest :: Show RemoveResourcePermissionRequest where
  show = genericShow
instance decodeRemoveResourcePermissionRequest :: Decode RemoveResourcePermissionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRemoveResourcePermissionRequest :: Encode RemoveResourcePermissionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The resource is already checked out.</p>
newtype ResourceAlreadyCheckedOutException = ResourceAlreadyCheckedOutException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeResourceAlreadyCheckedOutException :: Newtype ResourceAlreadyCheckedOutException _
derive instance repGenericResourceAlreadyCheckedOutException :: Generic ResourceAlreadyCheckedOutException _
instance showResourceAlreadyCheckedOutException :: Show ResourceAlreadyCheckedOutException where
  show = genericShow
instance decodeResourceAlreadyCheckedOutException :: Decode ResourceAlreadyCheckedOutException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceAlreadyCheckedOutException :: Encode ResourceAlreadyCheckedOutException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceIdType = ResourceIdType String
derive instance newtypeResourceIdType :: Newtype ResourceIdType _
derive instance repGenericResourceIdType :: Generic ResourceIdType _
instance showResourceIdType :: Show ResourceIdType where
  show = genericShow
instance decodeResourceIdType :: Decode ResourceIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceIdType :: Encode ResourceIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the metadata of a resource.</p>
newtype ResourceMetadata = ResourceMetadata 
  { "Type" :: NullOrUndefined.NullOrUndefined (ResourceType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  , "OriginalName" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  , "Id" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "VersionId" :: NullOrUndefined.NullOrUndefined (DocumentVersionIdType)
  , "Owner" :: NullOrUndefined.NullOrUndefined (UserMetadata)
  , "ParentId" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  }
derive instance newtypeResourceMetadata :: Newtype ResourceMetadata _
derive instance repGenericResourceMetadata :: Generic ResourceMetadata _
instance showResourceMetadata :: Show ResourceMetadata where
  show = genericShow
instance decodeResourceMetadata :: Decode ResourceMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceMetadata :: Encode ResourceMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceNameType = ResourceNameType String
derive instance newtypeResourceNameType :: Newtype ResourceNameType _
derive instance repGenericResourceNameType :: Generic ResourceNameType _
instance showResourceNameType :: Show ResourceNameType where
  show = genericShow
instance decodeResourceNameType :: Decode ResourceNameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceNameType :: Encode ResourceNameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the path information of a resource.</p>
newtype ResourcePath = ResourcePath 
  { "Components" :: NullOrUndefined.NullOrUndefined (ResourcePathComponentList)
  }
derive instance newtypeResourcePath :: Newtype ResourcePath _
derive instance repGenericResourcePath :: Generic ResourcePath _
instance showResourcePath :: Show ResourcePath where
  show = genericShow
instance decodeResourcePath :: Decode ResourcePath where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourcePath :: Encode ResourcePath where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the resource path.</p>
newtype ResourcePathComponent = ResourcePathComponent 
  { "Id" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  }
derive instance newtypeResourcePathComponent :: Newtype ResourcePathComponent _
derive instance repGenericResourcePathComponent :: Generic ResourcePathComponent _
instance showResourcePathComponent :: Show ResourcePathComponent where
  show = genericShow
instance decodeResourcePathComponent :: Decode ResourcePathComponent where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourcePathComponent :: Encode ResourcePathComponent where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourcePathComponentList = ResourcePathComponentList (Array ResourcePathComponent)
derive instance newtypeResourcePathComponentList :: Newtype ResourcePathComponentList _
derive instance repGenericResourcePathComponentList :: Generic ResourcePathComponentList _
instance showResourcePathComponentList :: Show ResourcePathComponentList where
  show = genericShow
instance decodeResourcePathComponentList :: Decode ResourcePathComponentList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourcePathComponentList :: Encode ResourcePathComponentList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceSortType = ResourceSortType String
derive instance newtypeResourceSortType :: Newtype ResourceSortType _
derive instance repGenericResourceSortType :: Generic ResourceSortType _
instance showResourceSortType :: Show ResourceSortType where
  show = genericShow
instance decodeResourceSortType :: Decode ResourceSortType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceSortType :: Encode ResourceSortType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceStateType = ResourceStateType String
derive instance newtypeResourceStateType :: Newtype ResourceStateType _
derive instance repGenericResourceStateType :: Generic ResourceStateType _
instance showResourceStateType :: Show ResourceStateType where
  show = genericShow
instance decodeResourceStateType :: Decode ResourceStateType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceStateType :: Encode ResourceStateType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _
derive instance repGenericResourceType :: Generic ResourceType _
instance showResourceType :: Show ResourceType where
  show = genericShow
instance decodeResourceType :: Decode ResourceType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeResourceType :: Encode ResourceType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RolePermissionType = RolePermissionType String
derive instance newtypeRolePermissionType :: Newtype RolePermissionType _
derive instance repGenericRolePermissionType :: Generic RolePermissionType _
instance showRolePermissionType :: Show RolePermissionType where
  show = genericShow
instance decodeRolePermissionType :: Decode RolePermissionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRolePermissionType :: Encode RolePermissionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype RoleType = RoleType String
derive instance newtypeRoleType :: Newtype RoleType _
derive instance repGenericRoleType :: Generic RoleType _
instance showRoleType :: Show RoleType where
  show = genericShow
instance decodeRoleType :: Decode RoleType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeRoleType :: Encode RoleType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SearchQueryType = SearchQueryType String
derive instance newtypeSearchQueryType :: Newtype SearchQueryType _
derive instance repGenericSearchQueryType :: Generic SearchQueryType _
instance showSearchQueryType :: Show SearchQueryType where
  show = genericShow
instance decodeSearchQueryType :: Decode SearchQueryType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSearchQueryType :: Encode SearchQueryType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>One or more of the dependencies is unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _
derive instance repGenericServiceUnavailableException :: Generic ServiceUnavailableException _
instance showServiceUnavailableException :: Show ServiceUnavailableException where
  show = genericShow
instance decodeServiceUnavailableException :: Decode ServiceUnavailableException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeServiceUnavailableException :: Encode ServiceUnavailableException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the recipient type and ID, if available.</p>
newtype SharePrincipal = SharePrincipal 
  { "Id" :: (IdType)
  , "Type" :: (PrincipalType)
  , "Role" :: (RoleType)
  }
derive instance newtypeSharePrincipal :: Newtype SharePrincipal _
derive instance repGenericSharePrincipal :: Generic SharePrincipal _
instance showSharePrincipal :: Show SharePrincipal where
  show = genericShow
instance decodeSharePrincipal :: Decode SharePrincipal where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSharePrincipal :: Encode SharePrincipal where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SharePrincipalList = SharePrincipalList (Array SharePrincipal)
derive instance newtypeSharePrincipalList :: Newtype SharePrincipalList _
derive instance repGenericSharePrincipalList :: Generic SharePrincipalList _
instance showSharePrincipalList :: Show SharePrincipalList where
  show = genericShow
instance decodeSharePrincipalList :: Decode SharePrincipalList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSharePrincipalList :: Encode SharePrincipalList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the share results of a resource.</p>
newtype ShareResult = ShareResult 
  { "PrincipalId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Role" :: NullOrUndefined.NullOrUndefined (RoleType)
  , "Status" :: NullOrUndefined.NullOrUndefined (ShareStatusType)
  , "ShareId" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "StatusMessage" :: NullOrUndefined.NullOrUndefined (MessageType)
  }
derive instance newtypeShareResult :: Newtype ShareResult _
derive instance repGenericShareResult :: Generic ShareResult _
instance showShareResult :: Show ShareResult where
  show = genericShow
instance decodeShareResult :: Decode ShareResult where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShareResult :: Encode ShareResult where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ShareResultsList = ShareResultsList (Array ShareResult)
derive instance newtypeShareResultsList :: Newtype ShareResultsList _
derive instance repGenericShareResultsList :: Generic ShareResultsList _
instance showShareResultsList :: Show ShareResultsList where
  show = genericShow
instance decodeShareResultsList :: Decode ShareResultsList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShareResultsList :: Encode ShareResultsList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype ShareStatusType = ShareStatusType String
derive instance newtypeShareStatusType :: Newtype ShareStatusType _
derive instance repGenericShareStatusType :: Generic ShareStatusType _
instance showShareStatusType :: Show ShareStatusType where
  show = genericShow
instance decodeShareStatusType :: Decode ShareStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeShareStatusType :: Encode ShareStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SharedLabel = SharedLabel String
derive instance newtypeSharedLabel :: Newtype SharedLabel _
derive instance repGenericSharedLabel :: Generic SharedLabel _
instance showSharedLabel :: Show SharedLabel where
  show = genericShow
instance decodeSharedLabel :: Decode SharedLabel where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSharedLabel :: Encode SharedLabel where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SharedLabels = SharedLabels (Array SharedLabel)
derive instance newtypeSharedLabels :: Newtype SharedLabels _
derive instance repGenericSharedLabels :: Generic SharedLabels _
instance showSharedLabels :: Show SharedLabels where
  show = genericShow
instance decodeSharedLabels :: Decode SharedLabels where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSharedLabels :: Encode SharedLabels where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SignedHeaderMap = SignedHeaderMap (StrMap.StrMap HeaderValueType)
derive instance newtypeSignedHeaderMap :: Newtype SignedHeaderMap _
derive instance repGenericSignedHeaderMap :: Generic SignedHeaderMap _
instance showSignedHeaderMap :: Show SignedHeaderMap where
  show = genericShow
instance decodeSignedHeaderMap :: Decode SignedHeaderMap where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSignedHeaderMap :: Encode SignedHeaderMap where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SizeType = SizeType Number
derive instance newtypeSizeType :: Newtype SizeType _
derive instance repGenericSizeType :: Generic SizeType _
instance showSizeType :: Show SizeType where
  show = genericShow
instance decodeSizeType :: Decode SizeType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSizeType :: Encode SizeType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The storage limit has been exceeded.</p>
newtype StorageLimitExceededException = StorageLimitExceededException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeStorageLimitExceededException :: Newtype StorageLimitExceededException _
derive instance repGenericStorageLimitExceededException :: Generic StorageLimitExceededException _
instance showStorageLimitExceededException :: Show StorageLimitExceededException where
  show = genericShow
instance decodeStorageLimitExceededException :: Decode StorageLimitExceededException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageLimitExceededException :: Encode StorageLimitExceededException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The storage limit will be exceeded.</p>
newtype StorageLimitWillExceedException = StorageLimitWillExceedException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeStorageLimitWillExceedException :: Newtype StorageLimitWillExceedException _
derive instance repGenericStorageLimitWillExceedException :: Generic StorageLimitWillExceedException _
instance showStorageLimitWillExceedException :: Show StorageLimitWillExceedException where
  show = genericShow
instance decodeStorageLimitWillExceedException :: Decode StorageLimitWillExceedException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageLimitWillExceedException :: Encode StorageLimitWillExceedException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the storage for a user.</p>
newtype StorageRuleType = StorageRuleType 
  { "StorageAllocatedInBytes" :: NullOrUndefined.NullOrUndefined (PositiveSizeType)
  , "StorageType" :: NullOrUndefined.NullOrUndefined (StorageType)
  }
derive instance newtypeStorageRuleType :: Newtype StorageRuleType _
derive instance repGenericStorageRuleType :: Generic StorageRuleType _
instance showStorageRuleType :: Show StorageRuleType where
  show = genericShow
instance decodeStorageRuleType :: Decode StorageRuleType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageRuleType :: Encode StorageRuleType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype StorageType = StorageType String
derive instance newtypeStorageType :: Newtype StorageType _
derive instance repGenericStorageType :: Generic StorageType _
instance showStorageType :: Show StorageType where
  show = genericShow
instance decodeStorageType :: Decode StorageType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeStorageType :: Encode StorageType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a subscription.</p>
newtype Subscription = Subscription 
  { "SubscriptionId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "EndPoint" :: NullOrUndefined.NullOrUndefined (SubscriptionEndPointType)
  , "Protocol" :: NullOrUndefined.NullOrUndefined (SubscriptionProtocolType)
  }
derive instance newtypeSubscription :: Newtype Subscription _
derive instance repGenericSubscription :: Generic Subscription _
instance showSubscription :: Show Subscription where
  show = genericShow
instance decodeSubscription :: Decode Subscription where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscription :: Encode Subscription where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscriptionEndPointType = SubscriptionEndPointType String
derive instance newtypeSubscriptionEndPointType :: Newtype SubscriptionEndPointType _
derive instance repGenericSubscriptionEndPointType :: Generic SubscriptionEndPointType _
instance showSubscriptionEndPointType :: Show SubscriptionEndPointType where
  show = genericShow
instance decodeSubscriptionEndPointType :: Decode SubscriptionEndPointType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionEndPointType :: Encode SubscriptionEndPointType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscriptionList = SubscriptionList (Array Subscription)
derive instance newtypeSubscriptionList :: Newtype SubscriptionList _
derive instance repGenericSubscriptionList :: Generic SubscriptionList _
instance showSubscriptionList :: Show SubscriptionList where
  show = genericShow
instance decodeSubscriptionList :: Decode SubscriptionList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionList :: Encode SubscriptionList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscriptionProtocolType = SubscriptionProtocolType String
derive instance newtypeSubscriptionProtocolType :: Newtype SubscriptionProtocolType _
derive instance repGenericSubscriptionProtocolType :: Generic SubscriptionProtocolType _
instance showSubscriptionProtocolType :: Show SubscriptionProtocolType where
  show = genericShow
instance decodeSubscriptionProtocolType :: Decode SubscriptionProtocolType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionProtocolType :: Encode SubscriptionProtocolType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype SubscriptionType = SubscriptionType String
derive instance newtypeSubscriptionType :: Newtype SubscriptionType _
derive instance repGenericSubscriptionType :: Generic SubscriptionType _
instance showSubscriptionType :: Show SubscriptionType where
  show = genericShow
instance decodeSubscriptionType :: Decode SubscriptionType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeSubscriptionType :: Encode SubscriptionType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimeZoneIdType = TimeZoneIdType String
derive instance newtypeTimeZoneIdType :: Newtype TimeZoneIdType _
derive instance repGenericTimeZoneIdType :: Generic TimeZoneIdType _
instance showTimeZoneIdType :: Show TimeZoneIdType where
  show = genericShow
instance decodeTimeZoneIdType :: Decode TimeZoneIdType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimeZoneIdType :: Encode TimeZoneIdType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype TimestampType = TimestampType Number
derive instance newtypeTimestampType :: Newtype TimestampType _
derive instance repGenericTimestampType :: Generic TimestampType _
instance showTimestampType :: Show TimestampType where
  show = genericShow
instance decodeTimestampType :: Decode TimestampType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTimestampType :: Encode TimestampType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The limit has been reached on the number of labels for the specified resource.</p>
newtype TooManyLabelsException = TooManyLabelsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeTooManyLabelsException :: Newtype TooManyLabelsException _
derive instance repGenericTooManyLabelsException :: Generic TooManyLabelsException _
instance showTooManyLabelsException :: Show TooManyLabelsException where
  show = genericShow
instance decodeTooManyLabelsException :: Decode TooManyLabelsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManyLabelsException :: Encode TooManyLabelsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>You've reached the limit on the number of subscriptions for the WorkDocs instance.</p>
newtype TooManySubscriptionsException = TooManySubscriptionsException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeTooManySubscriptionsException :: Newtype TooManySubscriptionsException _
derive instance repGenericTooManySubscriptionsException :: Generic TooManySubscriptionsException _
instance showTooManySubscriptionsException :: Show TooManySubscriptionsException where
  show = genericShow
instance decodeTooManySubscriptionsException :: Decode TooManySubscriptionsException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeTooManySubscriptionsException :: Encode TooManySubscriptionsException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The operation is not permitted.</p>
newtype UnauthorizedOperationException = UnauthorizedOperationException Types.NoArguments
derive instance newtypeUnauthorizedOperationException :: Newtype UnauthorizedOperationException _
derive instance repGenericUnauthorizedOperationException :: Generic UnauthorizedOperationException _
instance showUnauthorizedOperationException :: Show UnauthorizedOperationException where
  show = genericShow
instance decodeUnauthorizedOperationException :: Decode UnauthorizedOperationException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedOperationException :: Encode UnauthorizedOperationException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>The caller does not have access to perform the action on the resource.</p>
newtype UnauthorizedResourceAccessException = UnauthorizedResourceAccessException 
  { "Message" :: NullOrUndefined.NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeUnauthorizedResourceAccessException :: Newtype UnauthorizedResourceAccessException _
derive instance repGenericUnauthorizedResourceAccessException :: Generic UnauthorizedResourceAccessException _
instance showUnauthorizedResourceAccessException :: Show UnauthorizedResourceAccessException where
  show = genericShow
instance decodeUnauthorizedResourceAccessException :: Decode UnauthorizedResourceAccessException where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUnauthorizedResourceAccessException :: Encode UnauthorizedResourceAccessException where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDocumentRequest = UpdateDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "ResourceState" :: NullOrUndefined.NullOrUndefined (ResourceStateType)
  }
derive instance newtypeUpdateDocumentRequest :: Newtype UpdateDocumentRequest _
derive instance repGenericUpdateDocumentRequest :: Generic UpdateDocumentRequest _
instance showUpdateDocumentRequest :: Show UpdateDocumentRequest where
  show = genericShow
instance decodeUpdateDocumentRequest :: Decode UpdateDocumentRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDocumentRequest :: Encode UpdateDocumentRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateDocumentVersionRequest = UpdateDocumentVersionRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "VersionStatus" :: NullOrUndefined.NullOrUndefined (DocumentVersionStatus)
  }
derive instance newtypeUpdateDocumentVersionRequest :: Newtype UpdateDocumentVersionRequest _
derive instance repGenericUpdateDocumentVersionRequest :: Generic UpdateDocumentVersionRequest _
instance showUpdateDocumentVersionRequest :: Show UpdateDocumentVersionRequest where
  show = genericShow
instance decodeUpdateDocumentVersionRequest :: Decode UpdateDocumentVersionRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateDocumentVersionRequest :: Encode UpdateDocumentVersionRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateFolderRequest = UpdateFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  , "Name" :: NullOrUndefined.NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "ResourceState" :: NullOrUndefined.NullOrUndefined (ResourceStateType)
  }
derive instance newtypeUpdateFolderRequest :: Newtype UpdateFolderRequest _
derive instance repGenericUpdateFolderRequest :: Generic UpdateFolderRequest _
instance showUpdateFolderRequest :: Show UpdateFolderRequest where
  show = genericShow
instance decodeUpdateFolderRequest :: Decode UpdateFolderRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateFolderRequest :: Encode UpdateFolderRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateUserRequest = UpdateUserRequest 
  { "AuthenticationToken" :: NullOrUndefined.NullOrUndefined (AuthenticationHeaderType)
  , "UserId" :: (IdType)
  , "GivenName" :: NullOrUndefined.NullOrUndefined (UserAttributeValueType)
  , "Surname" :: NullOrUndefined.NullOrUndefined (UserAttributeValueType)
  , "Type" :: NullOrUndefined.NullOrUndefined (UserType)
  , "StorageRule" :: NullOrUndefined.NullOrUndefined (StorageRuleType)
  , "TimeZoneId" :: NullOrUndefined.NullOrUndefined (TimeZoneIdType)
  , "Locale" :: NullOrUndefined.NullOrUndefined (LocaleType)
  , "GrantPoweruserPrivileges" :: NullOrUndefined.NullOrUndefined (BooleanEnumType)
  }
derive instance newtypeUpdateUserRequest :: Newtype UpdateUserRequest _
derive instance repGenericUpdateUserRequest :: Generic UpdateUserRequest _
instance showUpdateUserRequest :: Show UpdateUserRequest where
  show = genericShow
instance decodeUpdateUserRequest :: Decode UpdateUserRequest where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserRequest :: Encode UpdateUserRequest where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UpdateUserResponse = UpdateUserResponse 
  { "User" :: NullOrUndefined.NullOrUndefined (User)
  }
derive instance newtypeUpdateUserResponse :: Newtype UpdateUserResponse _
derive instance repGenericUpdateUserResponse :: Generic UpdateUserResponse _
instance showUpdateUserResponse :: Show UpdateUserResponse where
  show = genericShow
instance decodeUpdateUserResponse :: Decode UpdateUserResponse where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUpdateUserResponse :: Encode UpdateUserResponse where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the upload.</p>
newtype UploadMetadata = UploadMetadata 
  { "UploadUrl" :: NullOrUndefined.NullOrUndefined (UrlType)
  , "SignedHeaders" :: NullOrUndefined.NullOrUndefined (SignedHeaderMap)
  }
derive instance newtypeUploadMetadata :: Newtype UploadMetadata _
derive instance repGenericUploadMetadata :: Generic UploadMetadata _
instance showUploadMetadata :: Show UploadMetadata where
  show = genericShow
instance decodeUploadMetadata :: Decode UploadMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUploadMetadata :: Encode UploadMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UrlType = UrlType String
derive instance newtypeUrlType :: Newtype UrlType _
derive instance repGenericUrlType :: Generic UrlType _
instance showUrlType :: Show UrlType where
  show = genericShow
instance decodeUrlType :: Decode UrlType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUrlType :: Encode UrlType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes a user.</p>
newtype User = User 
  { "Id" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Username" :: NullOrUndefined.NullOrUndefined (UsernameType)
  , "EmailAddress" :: NullOrUndefined.NullOrUndefined (EmailAddressType)
  , "GivenName" :: NullOrUndefined.NullOrUndefined (UserAttributeValueType)
  , "Surname" :: NullOrUndefined.NullOrUndefined (UserAttributeValueType)
  , "OrganizationId" :: NullOrUndefined.NullOrUndefined (IdType)
  , "RootFolderId" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "RecycleBinFolderId" :: NullOrUndefined.NullOrUndefined (ResourceIdType)
  , "Status" :: NullOrUndefined.NullOrUndefined (UserStatusType)
  , "Type" :: NullOrUndefined.NullOrUndefined (UserType)
  , "CreatedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "ModifiedTimestamp" :: NullOrUndefined.NullOrUndefined (TimestampType)
  , "TimeZoneId" :: NullOrUndefined.NullOrUndefined (TimeZoneIdType)
  , "Locale" :: NullOrUndefined.NullOrUndefined (LocaleType)
  , "Storage" :: NullOrUndefined.NullOrUndefined (UserStorageMetadata)
  }
derive instance newtypeUser :: Newtype User _
derive instance repGenericUser :: Generic User _
instance showUser :: Show User where
  show = genericShow
instance decodeUser :: Decode User where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUser :: Encode User where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserActivities = UserActivities (Array Activity)
derive instance newtypeUserActivities :: Newtype UserActivities _
derive instance repGenericUserActivities :: Generic UserActivities _
instance showUserActivities :: Show UserActivities where
  show = genericShow
instance decodeUserActivities :: Decode UserActivities where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserActivities :: Encode UserActivities where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserAttributeValueType = UserAttributeValueType String
derive instance newtypeUserAttributeValueType :: Newtype UserAttributeValueType _
derive instance repGenericUserAttributeValueType :: Generic UserAttributeValueType _
instance showUserAttributeValueType :: Show UserAttributeValueType where
  show = genericShow
instance decodeUserAttributeValueType :: Decode UserAttributeValueType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserAttributeValueType :: Encode UserAttributeValueType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserFilterType = UserFilterType String
derive instance newtypeUserFilterType :: Newtype UserFilterType _
derive instance repGenericUserFilterType :: Generic UserFilterType _
instance showUserFilterType :: Show UserFilterType where
  show = genericShow
instance decodeUserFilterType :: Decode UserFilterType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserFilterType :: Encode UserFilterType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserIdsType = UserIdsType String
derive instance newtypeUserIdsType :: Newtype UserIdsType _
derive instance repGenericUserIdsType :: Generic UserIdsType _
instance showUserIdsType :: Show UserIdsType where
  show = genericShow
instance decodeUserIdsType :: Decode UserIdsType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserIdsType :: Encode UserIdsType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the metadata of the user.</p>
newtype UserMetadata = UserMetadata 
  { "Id" :: NullOrUndefined.NullOrUndefined (IdType)
  , "Username" :: NullOrUndefined.NullOrUndefined (UsernameType)
  , "GivenName" :: NullOrUndefined.NullOrUndefined (UserAttributeValueType)
  , "Surname" :: NullOrUndefined.NullOrUndefined (UserAttributeValueType)
  , "EmailAddress" :: NullOrUndefined.NullOrUndefined (EmailAddressType)
  }
derive instance newtypeUserMetadata :: Newtype UserMetadata _
derive instance repGenericUserMetadata :: Generic UserMetadata _
instance showUserMetadata :: Show UserMetadata where
  show = genericShow
instance decodeUserMetadata :: Decode UserMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserMetadata :: Encode UserMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserMetadataList = UserMetadataList (Array UserMetadata)
derive instance newtypeUserMetadataList :: Newtype UserMetadataList _
derive instance repGenericUserMetadataList :: Generic UserMetadataList _
instance showUserMetadataList :: Show UserMetadataList where
  show = genericShow
instance decodeUserMetadataList :: Decode UserMetadataList where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserMetadataList :: Encode UserMetadataList where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserSortType = UserSortType String
derive instance newtypeUserSortType :: Newtype UserSortType _
derive instance repGenericUserSortType :: Generic UserSortType _
instance showUserSortType :: Show UserSortType where
  show = genericShow
instance decodeUserSortType :: Decode UserSortType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserSortType :: Encode UserSortType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserStatusType = UserStatusType String
derive instance newtypeUserStatusType :: Newtype UserStatusType _
derive instance repGenericUserStatusType :: Generic UserStatusType _
instance showUserStatusType :: Show UserStatusType where
  show = genericShow
instance decodeUserStatusType :: Decode UserStatusType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserStatusType :: Encode UserStatusType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


-- | <p>Describes the storage for a user.</p>
newtype UserStorageMetadata = UserStorageMetadata 
  { "StorageUtilizedInBytes" :: NullOrUndefined.NullOrUndefined (SizeType)
  , "StorageRule" :: NullOrUndefined.NullOrUndefined (StorageRuleType)
  }
derive instance newtypeUserStorageMetadata :: Newtype UserStorageMetadata _
derive instance repGenericUserStorageMetadata :: Generic UserStorageMetadata _
instance showUserStorageMetadata :: Show UserStorageMetadata where
  show = genericShow
instance decodeUserStorageMetadata :: Decode UserStorageMetadata where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserStorageMetadata :: Encode UserStorageMetadata where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UserType = UserType String
derive instance newtypeUserType :: Newtype UserType _
derive instance repGenericUserType :: Generic UserType _
instance showUserType :: Show UserType where
  show = genericShow
instance decodeUserType :: Decode UserType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUserType :: Encode UserType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }


newtype UsernameType = UsernameType String
derive instance newtypeUsernameType :: Newtype UsernameType _
derive instance repGenericUsernameType :: Generic UsernameType _
instance showUsernameType :: Show UsernameType where
  show = genericShow
instance decodeUsernameType :: Decode UsernameType where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
instance encodeUsernameType :: Encode UsernameType where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }
