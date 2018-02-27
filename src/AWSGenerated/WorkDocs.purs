

-- | <p>The WorkDocs API is designed for the following use cases:</p> <ul> <li> <p>File Migration: File migration applications are supported for users who want to migrate their files from an on-premises or off-premises file system or service. Users can insert files into a user directory structure, as well as allow for basic metadata changes, such as modifications to the permissions of files.</p> </li> <li> <p>Security: Support security applications are supported for users who have additional security needs, such as antivirus or data loss prevention. The API actions, along with AWS CloudTrail, allow these applications to detect when changes occur in Amazon WorkDocs. Then, the application can take the necessary actions and replace the target file. If the target file violates the policy, the application can also choose to email the user.</p> </li> <li> <p>eDiscovery/Analytics: General administrative applications are supported, such as eDiscovery and analytics. These applications can choose to mimic or record the actions in an Amazon WorkDocs site, along with AWS CloudTrail, to replicate data for eDiscovery, backup, or analytical applications.</p> </li> </ul> <p>All Amazon WorkDocs API actions are Amazon authenticated and certificate-signed. They not only require the use of the AWS SDK, but also allow for the exclusive use of IAM users and roles to help facilitate access, trust, and permission policies. By creating a role and allowing an IAM user to access the Amazon WorkDocs site, the IAM user gains full administrative visibility into the entire Amazon WorkDocs site (or as set in the IAM policy). This includes, but is not limited to, the ability to modify file permissions and upload any file to any user. This allows developers to perform the three use cases above, as well as give users the ability to grant access on a selective basis using the IAM model.</p>
module AWS.WorkDocs where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "WorkDocs" :: String


-- | <p>Aborts the upload of the specified document version that was previously initiated by <a>InitiateDocumentVersionUpload</a>. The client should make this call only when it no longer intends to upload the document version, or fails to do so.</p>
abortDocumentVersionUpload :: forall eff. AbortDocumentVersionUploadRequest -> Aff (err :: AWS.RequestError | eff) Unit
abortDocumentVersionUpload = AWS.request serviceName "abortDocumentVersionUpload" 


-- | <p>Activates the specified user. Only active users can access Amazon WorkDocs.</p>
activateUser :: forall eff. ActivateUserRequest -> Aff (err :: AWS.RequestError | eff) ActivateUserResponse
activateUser = AWS.request serviceName "activateUser" 


-- | <p>Creates a set of permissions for the specified folder or document. The resource permissions are overwritten if the principals already have different permissions.</p>
addResourcePermissions :: forall eff. AddResourcePermissionsRequest -> Aff (err :: AWS.RequestError | eff) AddResourcePermissionsResponse
addResourcePermissions = AWS.request serviceName "addResourcePermissions" 


-- | <p>Adds a new comment to the specified document version.</p>
createComment :: forall eff. CreateCommentRequest -> Aff (err :: AWS.RequestError | eff) CreateCommentResponse
createComment = AWS.request serviceName "createComment" 


-- | <p>Adds one or more custom properties to the specified resource (a folder, document, or version).</p>
createCustomMetadata :: forall eff. CreateCustomMetadataRequest -> Aff (err :: AWS.RequestError | eff) CreateCustomMetadataResponse
createCustomMetadata = AWS.request serviceName "createCustomMetadata" 


-- | <p>Creates a folder with the specified name and parent folder.</p>
createFolder :: forall eff. CreateFolderRequest -> Aff (err :: AWS.RequestError | eff) CreateFolderResponse
createFolder = AWS.request serviceName "createFolder" 


-- | <p>Adds the specified list of labels to the given resource (a document or folder)</p>
createLabels :: forall eff. CreateLabelsRequest -> Aff (err :: AWS.RequestError | eff) CreateLabelsResponse
createLabels = AWS.request serviceName "createLabels" 


-- | <p>Configure WorkDocs to use Amazon SNS notifications.</p> <p>The endpoint receives a confirmation message, and must confirm the subscription. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SendMessageToHttp.html#SendMessageToHttp.confirm">Confirm the Subscription</a> in the <i>Amazon Simple Notification Service Developer Guide</i>.</p>
createNotificationSubscription :: forall eff. CreateNotificationSubscriptionRequest -> Aff (err :: AWS.RequestError | eff) CreateNotificationSubscriptionResponse
createNotificationSubscription = AWS.request serviceName "createNotificationSubscription" 


-- | <p>Creates a user in a Simple AD or Microsoft AD directory. The status of a newly created user is "ACTIVE". New users can access Amazon WorkDocs.</p>
createUser :: forall eff. CreateUserRequest -> Aff (err :: AWS.RequestError | eff) CreateUserResponse
createUser = AWS.request serviceName "createUser" 


-- | <p>Deactivates the specified user, which revokes the user's access to Amazon WorkDocs.</p>
deactivateUser :: forall eff. DeactivateUserRequest -> Aff (err :: AWS.RequestError | eff) Unit
deactivateUser = AWS.request serviceName "deactivateUser" 


-- | <p>Deletes the specified comment from the document version.</p>
deleteComment :: forall eff. DeleteCommentRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteComment = AWS.request serviceName "deleteComment" 


-- | <p>Deletes custom metadata from the specified resource.</p>
deleteCustomMetadata :: forall eff. DeleteCustomMetadataRequest -> Aff (err :: AWS.RequestError | eff) DeleteCustomMetadataResponse
deleteCustomMetadata = AWS.request serviceName "deleteCustomMetadata" 


-- | <p>Permanently deletes the specified document and its associated metadata.</p>
deleteDocument :: forall eff. DeleteDocumentRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteDocument = AWS.request serviceName "deleteDocument" 


-- | <p>Permanently deletes the specified folder and its contents.</p>
deleteFolder :: forall eff. DeleteFolderRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteFolder = AWS.request serviceName "deleteFolder" 


-- | <p>Deletes the contents of the specified folder.</p>
deleteFolderContents :: forall eff. DeleteFolderContentsRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteFolderContents = AWS.request serviceName "deleteFolderContents" 


-- | <p>Deletes the specified list of labels from a resource.</p>
deleteLabels :: forall eff. DeleteLabelsRequest -> Aff (err :: AWS.RequestError | eff) DeleteLabelsResponse
deleteLabels = AWS.request serviceName "deleteLabels" 


-- | <p>Deletes the specified subscription from the specified organization.</p>
deleteNotificationSubscription :: forall eff. DeleteNotificationSubscriptionRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteNotificationSubscription = AWS.request serviceName "deleteNotificationSubscription" 


-- | <p>Deletes the specified user from a Simple AD or Microsoft AD directory.</p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteUser = AWS.request serviceName "deleteUser" 


-- | <p>Describes the user activities in a specified time period.</p>
describeActivities :: forall eff. DescribeActivitiesRequest -> Aff (err :: AWS.RequestError | eff) DescribeActivitiesResponse
describeActivities = AWS.request serviceName "describeActivities" 


-- | <p>List all the comments for the specified document version.</p>
describeComments :: forall eff. DescribeCommentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeCommentsResponse
describeComments = AWS.request serviceName "describeComments" 


-- | <p>Retrieves the document versions for the specified document.</p> <p>By default, only active versions are returned.</p>
describeDocumentVersions :: forall eff. DescribeDocumentVersionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDocumentVersionsResponse
describeDocumentVersions = AWS.request serviceName "describeDocumentVersions" 


-- | <p>Describes the contents of the specified folder, including its documents and subfolders.</p> <p>By default, Amazon WorkDocs returns the first 100 active document and folder metadata items. If there are more results, the response includes a marker that you can use to request the next set of results. You can also request initialized documents.</p>
describeFolderContents :: forall eff. DescribeFolderContentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeFolderContentsResponse
describeFolderContents = AWS.request serviceName "describeFolderContents" 


-- | <p>Describes the groups specified by query.</p>
describeGroups :: forall eff. DescribeGroupsRequest -> Aff (err :: AWS.RequestError | eff) DescribeGroupsResponse
describeGroups = AWS.request serviceName "describeGroups" 


-- | <p>Lists the specified notification subscriptions.</p>
describeNotificationSubscriptions :: forall eff. DescribeNotificationSubscriptionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeNotificationSubscriptionsResponse
describeNotificationSubscriptions = AWS.request serviceName "describeNotificationSubscriptions" 


-- | <p>Describes the permissions of a specified resource.</p>
describeResourcePermissions :: forall eff. DescribeResourcePermissionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeResourcePermissionsResponse
describeResourcePermissions = AWS.request serviceName "describeResourcePermissions" 


-- | <p>Describes the current user's special folders; the <code>RootFolder</code> and the <code>RecycleBin</code>. <code>RootFolder</code> is the root of user's files and folders and <code>RecycleBin</code> is the root of recycled items. This is not a valid action for SigV4 (administrative API) clients.</p>
describeRootFolders :: forall eff. DescribeRootFoldersRequest -> Aff (err :: AWS.RequestError | eff) DescribeRootFoldersResponse
describeRootFolders = AWS.request serviceName "describeRootFolders" 


-- | <p>Describes the specified users. You can describe all users or filter the results (for example, by status or organization).</p> <p>By default, Amazon WorkDocs returns the first 24 active or pending users. If there are more results, the response includes a marker that you can use to request the next set of results.</p>
describeUsers :: forall eff. DescribeUsersRequest -> Aff (err :: AWS.RequestError | eff) DescribeUsersResponse
describeUsers = AWS.request serviceName "describeUsers" 


-- | <p>Retrieves details of the current user for whom the authentication token was generated. This is not a valid action for SigV4 (administrative API) clients.</p>
getCurrentUser :: forall eff. GetCurrentUserRequest -> Aff (err :: AWS.RequestError | eff) GetCurrentUserResponse
getCurrentUser = AWS.request serviceName "getCurrentUser" 


-- | <p>Retrieves details of a document.</p>
getDocument :: forall eff. GetDocumentRequest -> Aff (err :: AWS.RequestError | eff) GetDocumentResponse
getDocument = AWS.request serviceName "getDocument" 


-- | <p>Retrieves the path information (the hierarchy from the root folder) for the requested document.</p> <p>By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested document and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the names of the parent folders.</p>
getDocumentPath :: forall eff. GetDocumentPathRequest -> Aff (err :: AWS.RequestError | eff) GetDocumentPathResponse
getDocumentPath = AWS.request serviceName "getDocumentPath" 


-- | <p>Retrieves version metadata for the specified document.</p>
getDocumentVersion :: forall eff. GetDocumentVersionRequest -> Aff (err :: AWS.RequestError | eff) GetDocumentVersionResponse
getDocumentVersion = AWS.request serviceName "getDocumentVersion" 


-- | <p>Retrieves the metadata of the specified folder.</p>
getFolder :: forall eff. GetFolderRequest -> Aff (err :: AWS.RequestError | eff) GetFolderResponse
getFolder = AWS.request serviceName "getFolder" 


-- | <p>Retrieves the path information (the hierarchy from the root folder) for the specified folder.</p> <p>By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested folder and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the parent folder names.</p>
getFolderPath :: forall eff. GetFolderPathRequest -> Aff (err :: AWS.RequestError | eff) GetFolderPathResponse
getFolderPath = AWS.request serviceName "getFolderPath" 


-- | <p>Creates a new document object and version object.</p> <p>The client specifies the parent folder ID and name of the document to upload. The ID is optionally specified when creating a new version of an existing document. This is the first step to upload a document. Next, upload the document to the URL returned from the call, and then call <a>UpdateDocumentVersion</a>.</p> <p>To cancel the document upload, call <a>AbortDocumentVersionUpload</a>.</p>
initiateDocumentVersionUpload :: forall eff. InitiateDocumentVersionUploadRequest -> Aff (err :: AWS.RequestError | eff) InitiateDocumentVersionUploadResponse
initiateDocumentVersionUpload = AWS.request serviceName "initiateDocumentVersionUpload" 


-- | <p>Removes all the permissions from the specified resource.</p>
removeAllResourcePermissions :: forall eff. RemoveAllResourcePermissionsRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeAllResourcePermissions = AWS.request serviceName "removeAllResourcePermissions" 


-- | <p>Removes the permission for the specified principal from the specified resource.</p>
removeResourcePermission :: forall eff. RemoveResourcePermissionRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeResourcePermission = AWS.request serviceName "removeResourcePermission" 


-- | <p>Updates the specified attributes of a document. The user must have access to both the document and its parent folder, if applicable.</p>
updateDocument :: forall eff. UpdateDocumentRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateDocument = AWS.request serviceName "updateDocument" 


-- | <p>Changes the status of the document version to ACTIVE. </p> <p>Amazon WorkDocs also sets its document container to ACTIVE. This is the last step in a document upload, after the client uploads the document to an S3-presigned URL returned by <a>InitiateDocumentVersionUpload</a>. </p>
updateDocumentVersion :: forall eff. UpdateDocumentVersionRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateDocumentVersion = AWS.request serviceName "updateDocumentVersion" 


-- | <p>Updates the specified attributes of the specified folder. The user must have access to both the folder and its parent folder, if applicable.</p>
updateFolder :: forall eff. UpdateFolderRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateFolder = AWS.request serviceName "updateFolder" 


-- | <p>Updates the specified attributes of the specified user, and grants or revokes administrative privileges to the Amazon WorkDocs site.</p>
updateUser :: forall eff. UpdateUserRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserResponse
updateUser = AWS.request serviceName "updateUser" 


newtype AbortDocumentVersionUploadRequest = AbortDocumentVersionUploadRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  }
derive instance newtypeAbortDocumentVersionUploadRequest :: Newtype AbortDocumentVersionUploadRequest _


newtype ActivateUserRequest = ActivateUserRequest 
  { "UserId" :: (IdType)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  }
derive instance newtypeActivateUserRequest :: Newtype ActivateUserRequest _


newtype ActivateUserResponse = ActivateUserResponse 
  { "User" :: NullOrUndefined (User)
  }
derive instance newtypeActivateUserResponse :: Newtype ActivateUserResponse _


-- | <p>Describes the activity information.</p>
newtype Activity = Activity 
  { "Type" :: NullOrUndefined (ActivityType)
  , "TimeStamp" :: NullOrUndefined (TimestampType)
  , "OrganizationId" :: NullOrUndefined (IdType)
  , "Initiator" :: NullOrUndefined (UserMetadata)
  , "Participants" :: NullOrUndefined (Participants)
  , "ResourceMetadata" :: NullOrUndefined (ResourceMetadata)
  , "OriginalParent" :: NullOrUndefined (ResourceMetadata)
  , "CommentMetadata" :: NullOrUndefined (CommentMetadata)
  }
derive instance newtypeActivity :: Newtype Activity _


newtype ActivityType = ActivityType String
derive instance newtypeActivityType :: Newtype ActivityType _


newtype AddResourcePermissionsRequest = AddResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "Principals" :: (SharePrincipalList)
  , "NotificationOptions" :: NullOrUndefined (NotificationOptions)
  }
derive instance newtypeAddResourcePermissionsRequest :: Newtype AddResourcePermissionsRequest _


newtype AddResourcePermissionsResponse = AddResourcePermissionsResponse 
  { "ShareResults" :: NullOrUndefined (ShareResultsList)
  }
derive instance newtypeAddResourcePermissionsResponse :: Newtype AddResourcePermissionsResponse _


newtype AuthenticationHeaderType = AuthenticationHeaderType String
derive instance newtypeAuthenticationHeaderType :: Newtype AuthenticationHeaderType _


newtype BooleanEnumType = BooleanEnumType String
derive instance newtypeBooleanEnumType :: Newtype BooleanEnumType _


newtype BooleanType = BooleanType Boolean
derive instance newtypeBooleanType :: Newtype BooleanType _


-- | <p>Describes a comment.</p>
newtype Comment = Comment 
  { "CommentId" :: (CommentIdType)
  , "ParentId" :: NullOrUndefined (CommentIdType)
  , "ThreadId" :: NullOrUndefined (CommentIdType)
  , "Text" :: NullOrUndefined (CommentTextType)
  , "Contributor" :: NullOrUndefined (User)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "Status" :: NullOrUndefined (CommentStatusType)
  , "Visibility" :: NullOrUndefined (CommentVisibilityType)
  , "RecipientId" :: NullOrUndefined (IdType)
  }
derive instance newtypeComment :: Newtype Comment _


newtype CommentIdType = CommentIdType String
derive instance newtypeCommentIdType :: Newtype CommentIdType _


newtype CommentList = CommentList (Array Comment)
derive instance newtypeCommentList :: Newtype CommentList _


-- | <p>Describes the metadata of a comment.</p>
newtype CommentMetadata = CommentMetadata 
  { "CommentId" :: NullOrUndefined (CommentIdType)
  , "Contributor" :: NullOrUndefined (User)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "CommentStatus" :: NullOrUndefined (CommentStatusType)
  , "RecipientId" :: NullOrUndefined (IdType)
  }
derive instance newtypeCommentMetadata :: Newtype CommentMetadata _


newtype CommentStatusType = CommentStatusType String
derive instance newtypeCommentStatusType :: Newtype CommentStatusType _


newtype CommentTextType = CommentTextType String
derive instance newtypeCommentTextType :: Newtype CommentTextType _


newtype CommentVisibilityType = CommentVisibilityType String
derive instance newtypeCommentVisibilityType :: Newtype CommentVisibilityType _


-- | <p>The resource hierarchy is changing.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeConcurrentModificationException :: Newtype ConcurrentModificationException _


newtype CreateCommentRequest = CreateCommentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "ParentId" :: NullOrUndefined (CommentIdType)
  , "ThreadId" :: NullOrUndefined (CommentIdType)
  , "Text" :: (CommentTextType)
  , "Visibility" :: NullOrUndefined (CommentVisibilityType)
  , "NotifyCollaborators" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeCreateCommentRequest :: Newtype CreateCommentRequest _


newtype CreateCommentResponse = CreateCommentResponse 
  { "Comment" :: NullOrUndefined (Comment)
  }
derive instance newtypeCreateCommentResponse :: Newtype CreateCommentResponse _


newtype CreateCustomMetadataRequest = CreateCustomMetadataRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "VersionId" :: NullOrUndefined (DocumentVersionIdType)
  , "CustomMetadata" :: (CustomMetadataMap)
  }
derive instance newtypeCreateCustomMetadataRequest :: Newtype CreateCustomMetadataRequest _


newtype CreateCustomMetadataResponse = CreateCustomMetadataResponse 
  { 
  }
derive instance newtypeCreateCustomMetadataResponse :: Newtype CreateCustomMetadataResponse _


newtype CreateFolderRequest = CreateFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: (ResourceIdType)
  }
derive instance newtypeCreateFolderRequest :: Newtype CreateFolderRequest _


newtype CreateFolderResponse = CreateFolderResponse 
  { "Metadata" :: NullOrUndefined (FolderMetadata)
  }
derive instance newtypeCreateFolderResponse :: Newtype CreateFolderResponse _


newtype CreateLabelsRequest = CreateLabelsRequest 
  { "ResourceId" :: (ResourceIdType)
  , "Labels" :: (SharedLabels)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  }
derive instance newtypeCreateLabelsRequest :: Newtype CreateLabelsRequest _


newtype CreateLabelsResponse = CreateLabelsResponse 
  { 
  }
derive instance newtypeCreateLabelsResponse :: Newtype CreateLabelsResponse _


newtype CreateNotificationSubscriptionRequest = CreateNotificationSubscriptionRequest 
  { "OrganizationId" :: (IdType)
  , "Endpoint" :: (SubscriptionEndPointType)
  , "Protocol" :: (SubscriptionProtocolType)
  , "SubscriptionType" :: (SubscriptionType)
  }
derive instance newtypeCreateNotificationSubscriptionRequest :: Newtype CreateNotificationSubscriptionRequest _


newtype CreateNotificationSubscriptionResponse = CreateNotificationSubscriptionResponse 
  { "Subscription" :: NullOrUndefined (Subscription)
  }
derive instance newtypeCreateNotificationSubscriptionResponse :: Newtype CreateNotificationSubscriptionResponse _


newtype CreateUserRequest = CreateUserRequest 
  { "OrganizationId" :: NullOrUndefined (IdType)
  , "Username" :: (UsernameType)
  , "EmailAddress" :: NullOrUndefined (EmailAddressType)
  , "GivenName" :: (UserAttributeValueType)
  , "Surname" :: (UserAttributeValueType)
  , "Password" :: (PasswordType)
  , "TimeZoneId" :: NullOrUndefined (TimeZoneIdType)
  , "StorageRule" :: NullOrUndefined (StorageRuleType)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  }
derive instance newtypeCreateUserRequest :: Newtype CreateUserRequest _


newtype CreateUserResponse = CreateUserResponse 
  { "User" :: NullOrUndefined (User)
  }
derive instance newtypeCreateUserResponse :: Newtype CreateUserResponse _


newtype CustomMetadataKeyList = CustomMetadataKeyList (Array CustomMetadataKeyType)
derive instance newtypeCustomMetadataKeyList :: Newtype CustomMetadataKeyList _


newtype CustomMetadataKeyType = CustomMetadataKeyType String
derive instance newtypeCustomMetadataKeyType :: Newtype CustomMetadataKeyType _


-- | <p>The limit has been reached on the number of custom properties for the specified resource.</p>
newtype CustomMetadataLimitExceededException = CustomMetadataLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeCustomMetadataLimitExceededException :: Newtype CustomMetadataLimitExceededException _


newtype CustomMetadataMap = CustomMetadataMap (Map CustomMetadataKeyType CustomMetadataValueType)
derive instance newtypeCustomMetadataMap :: Newtype CustomMetadataMap _


newtype CustomMetadataValueType = CustomMetadataValueType String
derive instance newtypeCustomMetadataValueType :: Newtype CustomMetadataValueType _


newtype DeactivateUserRequest = DeactivateUserRequest 
  { "UserId" :: (IdType)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  }
derive instance newtypeDeactivateUserRequest :: Newtype DeactivateUserRequest _


-- | <p>The last user in the organization is being deactivated.</p>
newtype DeactivatingLastSystemUserException = DeactivatingLastSystemUserException 
  { 
  }
derive instance newtypeDeactivatingLastSystemUserException :: Newtype DeactivatingLastSystemUserException _


newtype DeleteCommentRequest = DeleteCommentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "CommentId" :: (CommentIdType)
  }
derive instance newtypeDeleteCommentRequest :: Newtype DeleteCommentRequest _


newtype DeleteCustomMetadataRequest = DeleteCustomMetadataRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "VersionId" :: NullOrUndefined (DocumentVersionIdType)
  , "Keys" :: NullOrUndefined (CustomMetadataKeyList)
  , "DeleteAll" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeDeleteCustomMetadataRequest :: Newtype DeleteCustomMetadataRequest _


newtype DeleteCustomMetadataResponse = DeleteCustomMetadataResponse 
  { 
  }
derive instance newtypeDeleteCustomMetadataResponse :: Newtype DeleteCustomMetadataResponse _


newtype DeleteDocumentRequest = DeleteDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  }
derive instance newtypeDeleteDocumentRequest :: Newtype DeleteDocumentRequest _


newtype DeleteFolderContentsRequest = DeleteFolderContentsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  }
derive instance newtypeDeleteFolderContentsRequest :: Newtype DeleteFolderContentsRequest _


newtype DeleteFolderRequest = DeleteFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  }
derive instance newtypeDeleteFolderRequest :: Newtype DeleteFolderRequest _


newtype DeleteLabelsRequest = DeleteLabelsRequest 
  { "ResourceId" :: (ResourceIdType)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "Labels" :: NullOrUndefined (SharedLabels)
  , "DeleteAll" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeDeleteLabelsRequest :: Newtype DeleteLabelsRequest _


newtype DeleteLabelsResponse = DeleteLabelsResponse 
  { 
  }
derive instance newtypeDeleteLabelsResponse :: Newtype DeleteLabelsResponse _


newtype DeleteNotificationSubscriptionRequest = DeleteNotificationSubscriptionRequest 
  { "SubscriptionId" :: (IdType)
  , "OrganizationId" :: (IdType)
  }
derive instance newtypeDeleteNotificationSubscriptionRequest :: Newtype DeleteNotificationSubscriptionRequest _


newtype DeleteUserRequest = DeleteUserRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "UserId" :: (IdType)
  }
derive instance newtypeDeleteUserRequest :: Newtype DeleteUserRequest _


newtype DescribeActivitiesRequest = DescribeActivitiesRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "StartTime" :: NullOrUndefined (TimestampType)
  , "EndTime" :: NullOrUndefined (TimestampType)
  , "OrganizationId" :: NullOrUndefined (IdType)
  , "UserId" :: NullOrUndefined (IdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeActivitiesRequest :: Newtype DescribeActivitiesRequest _


newtype DescribeActivitiesResponse = DescribeActivitiesResponse 
  { "UserActivities" :: NullOrUndefined (UserActivities)
  , "Marker" :: NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeActivitiesResponse :: Newtype DescribeActivitiesResponse _


newtype DescribeCommentsRequest = DescribeCommentsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeCommentsRequest :: Newtype DescribeCommentsRequest _


newtype DescribeCommentsResponse = DescribeCommentsResponse 
  { "Comments" :: NullOrUndefined (CommentList)
  , "Marker" :: NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeCommentsResponse :: Newtype DescribeCommentsResponse _


newtype DescribeDocumentVersionsRequest = DescribeDocumentVersionsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Include" :: NullOrUndefined (FieldNamesType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  }
derive instance newtypeDescribeDocumentVersionsRequest :: Newtype DescribeDocumentVersionsRequest _


newtype DescribeDocumentVersionsResponse = DescribeDocumentVersionsResponse 
  { "DocumentVersions" :: NullOrUndefined (DocumentVersionMetadataList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeDocumentVersionsResponse :: Newtype DescribeDocumentVersionsResponse _


newtype DescribeFolderContentsRequest = DescribeFolderContentsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  , "Sort" :: NullOrUndefined (ResourceSortType)
  , "Order" :: NullOrUndefined (OrderType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  , "Type" :: NullOrUndefined (FolderContentType)
  , "Include" :: NullOrUndefined (FieldNamesType)
  }
derive instance newtypeDescribeFolderContentsRequest :: Newtype DescribeFolderContentsRequest _


newtype DescribeFolderContentsResponse = DescribeFolderContentsResponse 
  { "Folders" :: NullOrUndefined (FolderMetadataList)
  , "Documents" :: NullOrUndefined (DocumentMetadataList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeFolderContentsResponse :: Newtype DescribeFolderContentsResponse _


newtype DescribeGroupsRequest = DescribeGroupsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "SearchQuery" :: (SearchQueryType)
  , "OrganizationId" :: NullOrUndefined (IdType)
  , "Marker" :: NullOrUndefined (MarkerType)
  , "Limit" :: NullOrUndefined (PositiveIntegerType)
  }
derive instance newtypeDescribeGroupsRequest :: Newtype DescribeGroupsRequest _


newtype DescribeGroupsResponse = DescribeGroupsResponse 
  { "Groups" :: NullOrUndefined (GroupMetadataList)
  , "Marker" :: NullOrUndefined (MarkerType)
  }
derive instance newtypeDescribeGroupsResponse :: Newtype DescribeGroupsResponse _


newtype DescribeNotificationSubscriptionsRequest = DescribeNotificationSubscriptionsRequest 
  { "OrganizationId" :: (IdType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  , "Limit" :: NullOrUndefined (LimitType)
  }
derive instance newtypeDescribeNotificationSubscriptionsRequest :: Newtype DescribeNotificationSubscriptionsRequest _


newtype DescribeNotificationSubscriptionsResponse = DescribeNotificationSubscriptionsResponse 
  { "Subscriptions" :: NullOrUndefined (SubscriptionList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeNotificationSubscriptionsResponse :: Newtype DescribeNotificationSubscriptionsResponse _


newtype DescribeResourcePermissionsRequest = DescribeResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "PrincipalId" :: NullOrUndefined (IdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeResourcePermissionsRequest :: Newtype DescribeResourcePermissionsRequest _


newtype DescribeResourcePermissionsResponse = DescribeResourcePermissionsResponse 
  { "Principals" :: NullOrUndefined (PrincipalList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeResourcePermissionsResponse :: Newtype DescribeResourcePermissionsResponse _


newtype DescribeRootFoldersRequest = DescribeRootFoldersRequest 
  { "AuthenticationToken" :: (AuthenticationHeaderType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeRootFoldersRequest :: Newtype DescribeRootFoldersRequest _


newtype DescribeRootFoldersResponse = DescribeRootFoldersResponse 
  { "Folders" :: NullOrUndefined (FolderMetadataList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeRootFoldersResponse :: Newtype DescribeRootFoldersResponse _


newtype DescribeUsersRequest = DescribeUsersRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "OrganizationId" :: NullOrUndefined (IdType)
  , "UserIds" :: NullOrUndefined (UserIdsType)
  , "Query" :: NullOrUndefined (SearchQueryType)
  , "Include" :: NullOrUndefined (UserFilterType)
  , "Order" :: NullOrUndefined (OrderType)
  , "Sort" :: NullOrUndefined (UserSortType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  }
derive instance newtypeDescribeUsersRequest :: Newtype DescribeUsersRequest _


newtype DescribeUsersResponse = DescribeUsersResponse 
  { "Users" :: NullOrUndefined (OrganizationUserList)
  , "TotalNumberOfUsers" :: NullOrUndefined (SizeType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeDescribeUsersResponse :: Newtype DescribeUsersResponse _


newtype DocumentContentType = DocumentContentType String
derive instance newtypeDocumentContentType :: Newtype DocumentContentType _


-- | <p>This exception is thrown when the document is locked for comments and user tries to create or delete a comment on that document.</p>
newtype DocumentLockedForCommentsException = DocumentLockedForCommentsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeDocumentLockedForCommentsException :: Newtype DocumentLockedForCommentsException _


-- | <p>Describes the document.</p>
newtype DocumentMetadata = DocumentMetadata 
  { "Id" :: NullOrUndefined (ResourceIdType)
  , "CreatorId" :: NullOrUndefined (IdType)
  , "ParentFolderId" :: NullOrUndefined (ResourceIdType)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "ModifiedTimestamp" :: NullOrUndefined (TimestampType)
  , "LatestVersionMetadata" :: NullOrUndefined (DocumentVersionMetadata)
  , "ResourceState" :: NullOrUndefined (ResourceStateType)
  , "Labels" :: NullOrUndefined (SharedLabels)
  }
derive instance newtypeDocumentMetadata :: Newtype DocumentMetadata _


newtype DocumentMetadataList = DocumentMetadataList (Array DocumentMetadata)
derive instance newtypeDocumentMetadataList :: Newtype DocumentMetadataList _


newtype DocumentSourceType = DocumentSourceType String
derive instance newtypeDocumentSourceType :: Newtype DocumentSourceType _


newtype DocumentSourceUrlMap = DocumentSourceUrlMap (Map DocumentSourceType UrlType)
derive instance newtypeDocumentSourceUrlMap :: Newtype DocumentSourceUrlMap _


newtype DocumentStatusType = DocumentStatusType String
derive instance newtypeDocumentStatusType :: Newtype DocumentStatusType _


newtype DocumentThumbnailType = DocumentThumbnailType String
derive instance newtypeDocumentThumbnailType :: Newtype DocumentThumbnailType _


newtype DocumentThumbnailUrlMap = DocumentThumbnailUrlMap (Map DocumentThumbnailType UrlType)
derive instance newtypeDocumentThumbnailUrlMap :: Newtype DocumentThumbnailUrlMap _


newtype DocumentVersionIdType = DocumentVersionIdType String
derive instance newtypeDocumentVersionIdType :: Newtype DocumentVersionIdType _


-- | <p>Describes a version of a document.</p>
newtype DocumentVersionMetadata = DocumentVersionMetadata 
  { "Id" :: NullOrUndefined (DocumentVersionIdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "ContentType" :: NullOrUndefined (DocumentContentType)
  , "Size" :: NullOrUndefined (SizeType)
  , "Signature" :: NullOrUndefined (HashType)
  , "Status" :: NullOrUndefined (DocumentStatusType)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "ModifiedTimestamp" :: NullOrUndefined (TimestampType)
  , "ContentCreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "ContentModifiedTimestamp" :: NullOrUndefined (TimestampType)
  , "CreatorId" :: NullOrUndefined (IdType)
  , "Thumbnail" :: NullOrUndefined (DocumentThumbnailUrlMap)
  , "Source" :: NullOrUndefined (DocumentSourceUrlMap)
  }
derive instance newtypeDocumentVersionMetadata :: Newtype DocumentVersionMetadata _


newtype DocumentVersionMetadataList = DocumentVersionMetadataList (Array DocumentVersionMetadata)
derive instance newtypeDocumentVersionMetadataList :: Newtype DocumentVersionMetadataList _


newtype DocumentVersionStatus = DocumentVersionStatus String
derive instance newtypeDocumentVersionStatus :: Newtype DocumentVersionStatus _


-- | <p>This exception is thrown when a valid checkout ID is not presented on document version upload calls for a document that has been checked out from Web client.</p>
newtype DraftUploadOutOfSyncException = DraftUploadOutOfSyncException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeDraftUploadOutOfSyncException :: Newtype DraftUploadOutOfSyncException _


newtype EmailAddressType = EmailAddressType String
derive instance newtypeEmailAddressType :: Newtype EmailAddressType _


-- | <p>The resource already exists.</p>
newtype EntityAlreadyExistsException = EntityAlreadyExistsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeEntityAlreadyExistsException :: Newtype EntityAlreadyExistsException _


newtype EntityIdList = EntityIdList (Array IdType)
derive instance newtypeEntityIdList :: Newtype EntityIdList _


-- | <p>The resource does not exist.</p>
newtype EntityNotExistsException = EntityNotExistsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  , "EntityIds" :: NullOrUndefined (EntityIdList)
  }
derive instance newtypeEntityNotExistsException :: Newtype EntityNotExistsException _


newtype ErrorMessageType = ErrorMessageType String
derive instance newtypeErrorMessageType :: Newtype ErrorMessageType _


-- | <p>The AWS Directory Service cannot reach an on-premises instance. Or a dependency under the control of the organization is failing, such as a connected Active Directory.</p>
newtype FailedDependencyException = FailedDependencyException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeFailedDependencyException :: Newtype FailedDependencyException _


newtype FieldNamesType = FieldNamesType String
derive instance newtypeFieldNamesType :: Newtype FieldNamesType _


newtype FolderContentType = FolderContentType String
derive instance newtypeFolderContentType :: Newtype FolderContentType _


-- | <p>Describes a folder.</p>
newtype FolderMetadata = FolderMetadata 
  { "Id" :: NullOrUndefined (ResourceIdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "CreatorId" :: NullOrUndefined (IdType)
  , "ParentFolderId" :: NullOrUndefined (ResourceIdType)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "ModifiedTimestamp" :: NullOrUndefined (TimestampType)
  , "ResourceState" :: NullOrUndefined (ResourceStateType)
  , "Signature" :: NullOrUndefined (HashType)
  , "Labels" :: NullOrUndefined (SharedLabels)
  , "Size" :: NullOrUndefined (SizeType)
  , "LatestVersionSize" :: NullOrUndefined (SizeType)
  }
derive instance newtypeFolderMetadata :: Newtype FolderMetadata _


newtype FolderMetadataList = FolderMetadataList (Array FolderMetadata)
derive instance newtypeFolderMetadataList :: Newtype FolderMetadataList _


newtype GetCurrentUserRequest = GetCurrentUserRequest 
  { "AuthenticationToken" :: (AuthenticationHeaderType)
  }
derive instance newtypeGetCurrentUserRequest :: Newtype GetCurrentUserRequest _


newtype GetCurrentUserResponse = GetCurrentUserResponse 
  { "User" :: NullOrUndefined (User)
  }
derive instance newtypeGetCurrentUserResponse :: Newtype GetCurrentUserResponse _


newtype GetDocumentPathRequest = GetDocumentPathRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (IdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeGetDocumentPathRequest :: Newtype GetDocumentPathRequest _


newtype GetDocumentPathResponse = GetDocumentPathResponse 
  { "Path" :: NullOrUndefined (ResourcePath)
  }
derive instance newtypeGetDocumentPathResponse :: Newtype GetDocumentPathResponse _


newtype GetDocumentRequest = GetDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "IncludeCustomMetadata" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeGetDocumentRequest :: Newtype GetDocumentRequest _


newtype GetDocumentResponse = GetDocumentResponse 
  { "Metadata" :: NullOrUndefined (DocumentMetadata)
  , "CustomMetadata" :: NullOrUndefined (CustomMetadataMap)
  }
derive instance newtypeGetDocumentResponse :: Newtype GetDocumentResponse _


newtype GetDocumentVersionRequest = GetDocumentVersionRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  , "IncludeCustomMetadata" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeGetDocumentVersionRequest :: Newtype GetDocumentVersionRequest _


newtype GetDocumentVersionResponse = GetDocumentVersionResponse 
  { "Metadata" :: NullOrUndefined (DocumentVersionMetadata)
  , "CustomMetadata" :: NullOrUndefined (CustomMetadataMap)
  }
derive instance newtypeGetDocumentVersionResponse :: Newtype GetDocumentVersionResponse _


newtype GetFolderPathRequest = GetFolderPathRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (IdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }
derive instance newtypeGetFolderPathRequest :: Newtype GetFolderPathRequest _


newtype GetFolderPathResponse = GetFolderPathResponse 
  { "Path" :: NullOrUndefined (ResourcePath)
  }
derive instance newtypeGetFolderPathResponse :: Newtype GetFolderPathResponse _


newtype GetFolderRequest = GetFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  , "IncludeCustomMetadata" :: NullOrUndefined (BooleanType)
  }
derive instance newtypeGetFolderRequest :: Newtype GetFolderRequest _


newtype GetFolderResponse = GetFolderResponse 
  { "Metadata" :: NullOrUndefined (FolderMetadata)
  , "CustomMetadata" :: NullOrUndefined (CustomMetadataMap)
  }
derive instance newtypeGetFolderResponse :: Newtype GetFolderResponse _


-- | <p>Describes the metadata of a user group.</p>
newtype GroupMetadata = GroupMetadata 
  { "Id" :: NullOrUndefined (IdType)
  , "Name" :: NullOrUndefined (GroupNameType)
  }
derive instance newtypeGroupMetadata :: Newtype GroupMetadata _


newtype GroupMetadataList = GroupMetadataList (Array GroupMetadata)
derive instance newtypeGroupMetadataList :: Newtype GroupMetadataList _


newtype GroupNameType = GroupNameType String
derive instance newtypeGroupNameType :: Newtype GroupNameType _


newtype HashType = HashType String
derive instance newtypeHashType :: Newtype HashType _


newtype HeaderNameType = HeaderNameType String
derive instance newtypeHeaderNameType :: Newtype HeaderNameType _


newtype HeaderValueType = HeaderValueType String
derive instance newtypeHeaderValueType :: Newtype HeaderValueType _


newtype IdType = IdType String
derive instance newtypeIdType :: Newtype IdType _


-- | <p>The user is undergoing transfer of ownership.</p>
newtype IllegalUserStateException = IllegalUserStateException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeIllegalUserStateException :: Newtype IllegalUserStateException _


newtype InitiateDocumentVersionUploadRequest = InitiateDocumentVersionUploadRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "Id" :: NullOrUndefined (ResourceIdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "ContentCreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "ContentModifiedTimestamp" :: NullOrUndefined (TimestampType)
  , "ContentType" :: NullOrUndefined (DocumentContentType)
  , "DocumentSizeInBytes" :: NullOrUndefined (SizeType)
  , "ParentFolderId" :: (ResourceIdType)
  }
derive instance newtypeInitiateDocumentVersionUploadRequest :: Newtype InitiateDocumentVersionUploadRequest _


newtype InitiateDocumentVersionUploadResponse = InitiateDocumentVersionUploadResponse 
  { "Metadata" :: NullOrUndefined (DocumentMetadata)
  , "UploadMetadata" :: NullOrUndefined (UploadMetadata)
  }
derive instance newtypeInitiateDocumentVersionUploadResponse :: Newtype InitiateDocumentVersionUploadResponse _


-- | <p>The pagination marker or limit fields are not valid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidArgumentException :: Newtype InvalidArgumentException _


-- | <p>The operation is invalid.</p>
newtype InvalidOperationException = InvalidOperationException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidOperationException :: Newtype InvalidOperationException _


-- | <p>The password is invalid.</p>
newtype InvalidPasswordException = InvalidPasswordException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeInvalidPasswordException :: Newtype InvalidPasswordException _


-- | <p>The maximum of 100,000 folders under the parent folder has been exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeLimitExceededException :: Newtype LimitExceededException _


newtype LimitType = LimitType Int
derive instance newtypeLimitType :: Newtype LimitType _


newtype LocaleType = LocaleType String
derive instance newtypeLocaleType :: Newtype LocaleType _


newtype MarkerType = MarkerType String
derive instance newtypeMarkerType :: Newtype MarkerType _


newtype MessageType = MessageType String
derive instance newtypeMessageType :: Newtype MessageType _


-- | <p>Set of options which defines notification preferences of given action.</p>
newtype NotificationOptions = NotificationOptions 
  { "SendEmail" :: NullOrUndefined (BooleanType)
  , "EmailMessage" :: NullOrUndefined (MessageType)
  }
derive instance newtypeNotificationOptions :: Newtype NotificationOptions _


newtype OrderType = OrderType String
derive instance newtypeOrderType :: Newtype OrderType _


newtype OrganizationUserList = OrganizationUserList (Array User)
derive instance newtypeOrganizationUserList :: Newtype OrganizationUserList _


newtype PageMarkerType = PageMarkerType String
derive instance newtypePageMarkerType :: Newtype PageMarkerType _


-- | <p>Describes the users or user groups.</p>
newtype Participants = Participants 
  { "Users" :: NullOrUndefined (UserMetadataList)
  , "Groups" :: NullOrUndefined (GroupMetadataList)
  }
derive instance newtypeParticipants :: Newtype Participants _


newtype PasswordType = PasswordType String
derive instance newtypePasswordType :: Newtype PasswordType _


-- | <p>Describes the permissions.</p>
newtype PermissionInfo = PermissionInfo 
  { "Role" :: NullOrUndefined (RoleType)
  , "Type" :: NullOrUndefined (RolePermissionType)
  }
derive instance newtypePermissionInfo :: Newtype PermissionInfo _


newtype PermissionInfoList = PermissionInfoList (Array PermissionInfo)
derive instance newtypePermissionInfoList :: Newtype PermissionInfoList _


newtype PositiveIntegerType = PositiveIntegerType Int
derive instance newtypePositiveIntegerType :: Newtype PositiveIntegerType _


newtype PositiveSizeType = PositiveSizeType Number
derive instance newtypePositiveSizeType :: Newtype PositiveSizeType _


-- | <p>Describes a resource.</p>
newtype Principal = Principal 
  { "Id" :: NullOrUndefined (IdType)
  , "Type" :: NullOrUndefined (PrincipalType)
  , "Roles" :: NullOrUndefined (PermissionInfoList)
  }
derive instance newtypePrincipal :: Newtype Principal _


newtype PrincipalList = PrincipalList (Array Principal)
derive instance newtypePrincipalList :: Newtype PrincipalList _


newtype PrincipalType = PrincipalType String
derive instance newtypePrincipalType :: Newtype PrincipalType _


-- | <p>The specified document version is not in the INITIALIZED state.</p>
newtype ProhibitedStateException = ProhibitedStateException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeProhibitedStateException :: Newtype ProhibitedStateException _


newtype RemoveAllResourcePermissionsRequest = RemoveAllResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  }
derive instance newtypeRemoveAllResourcePermissionsRequest :: Newtype RemoveAllResourcePermissionsRequest _


newtype RemoveResourcePermissionRequest = RemoveResourcePermissionRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "PrincipalId" :: (IdType)
  , "PrincipalType" :: NullOrUndefined (PrincipalType)
  }
derive instance newtypeRemoveResourcePermissionRequest :: Newtype RemoveResourcePermissionRequest _


-- | <p>The resource is already checked out.</p>
newtype ResourceAlreadyCheckedOutException = ResourceAlreadyCheckedOutException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeResourceAlreadyCheckedOutException :: Newtype ResourceAlreadyCheckedOutException _


newtype ResourceIdType = ResourceIdType String
derive instance newtypeResourceIdType :: Newtype ResourceIdType _


-- | <p>Describes the metadata of a resource.</p>
newtype ResourceMetadata = ResourceMetadata 
  { "Type" :: NullOrUndefined (ResourceType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "OriginalName" :: NullOrUndefined (ResourceNameType)
  , "Id" :: NullOrUndefined (ResourceIdType)
  , "VersionId" :: NullOrUndefined (DocumentVersionIdType)
  , "Owner" :: NullOrUndefined (UserMetadata)
  , "ParentId" :: NullOrUndefined (ResourceIdType)
  }
derive instance newtypeResourceMetadata :: Newtype ResourceMetadata _


newtype ResourceNameType = ResourceNameType String
derive instance newtypeResourceNameType :: Newtype ResourceNameType _


-- | <p>Describes the path information of a resource.</p>
newtype ResourcePath = ResourcePath 
  { "Components" :: NullOrUndefined (ResourcePathComponentList)
  }
derive instance newtypeResourcePath :: Newtype ResourcePath _


-- | <p>Describes the resource path.</p>
newtype ResourcePathComponent = ResourcePathComponent 
  { "Id" :: NullOrUndefined (IdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  }
derive instance newtypeResourcePathComponent :: Newtype ResourcePathComponent _


newtype ResourcePathComponentList = ResourcePathComponentList (Array ResourcePathComponent)
derive instance newtypeResourcePathComponentList :: Newtype ResourcePathComponentList _


newtype ResourceSortType = ResourceSortType String
derive instance newtypeResourceSortType :: Newtype ResourceSortType _


newtype ResourceStateType = ResourceStateType String
derive instance newtypeResourceStateType :: Newtype ResourceStateType _


newtype ResourceType = ResourceType String
derive instance newtypeResourceType :: Newtype ResourceType _


newtype RolePermissionType = RolePermissionType String
derive instance newtypeRolePermissionType :: Newtype RolePermissionType _


newtype RoleType = RoleType String
derive instance newtypeRoleType :: Newtype RoleType _


newtype SearchQueryType = SearchQueryType String
derive instance newtypeSearchQueryType :: Newtype SearchQueryType _


-- | <p>One or more of the dependencies is unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeServiceUnavailableException :: Newtype ServiceUnavailableException _


-- | <p>Describes the recipient type and ID, if available.</p>
newtype SharePrincipal = SharePrincipal 
  { "Id" :: (IdType)
  , "Type" :: (PrincipalType)
  , "Role" :: (RoleType)
  }
derive instance newtypeSharePrincipal :: Newtype SharePrincipal _


newtype SharePrincipalList = SharePrincipalList (Array SharePrincipal)
derive instance newtypeSharePrincipalList :: Newtype SharePrincipalList _


-- | <p>Describes the share results of a resource.</p>
newtype ShareResult = ShareResult 
  { "PrincipalId" :: NullOrUndefined (IdType)
  , "Role" :: NullOrUndefined (RoleType)
  , "Status" :: NullOrUndefined (ShareStatusType)
  , "ShareId" :: NullOrUndefined (ResourceIdType)
  , "StatusMessage" :: NullOrUndefined (MessageType)
  }
derive instance newtypeShareResult :: Newtype ShareResult _


newtype ShareResultsList = ShareResultsList (Array ShareResult)
derive instance newtypeShareResultsList :: Newtype ShareResultsList _


newtype ShareStatusType = ShareStatusType String
derive instance newtypeShareStatusType :: Newtype ShareStatusType _


newtype SharedLabel = SharedLabel String
derive instance newtypeSharedLabel :: Newtype SharedLabel _


newtype SharedLabels = SharedLabels (Array SharedLabel)
derive instance newtypeSharedLabels :: Newtype SharedLabels _


newtype SignedHeaderMap = SignedHeaderMap (Map HeaderNameType HeaderValueType)
derive instance newtypeSignedHeaderMap :: Newtype SignedHeaderMap _


newtype SizeType = SizeType Number
derive instance newtypeSizeType :: Newtype SizeType _


-- | <p>The storage limit has been exceeded.</p>
newtype StorageLimitExceededException = StorageLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeStorageLimitExceededException :: Newtype StorageLimitExceededException _


-- | <p>The storage limit will be exceeded.</p>
newtype StorageLimitWillExceedException = StorageLimitWillExceedException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeStorageLimitWillExceedException :: Newtype StorageLimitWillExceedException _


-- | <p>Describes the storage for a user.</p>
newtype StorageRuleType = StorageRuleType 
  { "StorageAllocatedInBytes" :: NullOrUndefined (PositiveSizeType)
  , "StorageType" :: NullOrUndefined (StorageType)
  }
derive instance newtypeStorageRuleType :: Newtype StorageRuleType _


newtype StorageType = StorageType String
derive instance newtypeStorageType :: Newtype StorageType _


-- | <p>Describes a subscription.</p>
newtype Subscription = Subscription 
  { "SubscriptionId" :: NullOrUndefined (IdType)
  , "EndPoint" :: NullOrUndefined (SubscriptionEndPointType)
  , "Protocol" :: NullOrUndefined (SubscriptionProtocolType)
  }
derive instance newtypeSubscription :: Newtype Subscription _


newtype SubscriptionEndPointType = SubscriptionEndPointType String
derive instance newtypeSubscriptionEndPointType :: Newtype SubscriptionEndPointType _


newtype SubscriptionList = SubscriptionList (Array Subscription)
derive instance newtypeSubscriptionList :: Newtype SubscriptionList _


newtype SubscriptionProtocolType = SubscriptionProtocolType String
derive instance newtypeSubscriptionProtocolType :: Newtype SubscriptionProtocolType _


newtype SubscriptionType = SubscriptionType String
derive instance newtypeSubscriptionType :: Newtype SubscriptionType _


newtype TimeZoneIdType = TimeZoneIdType String
derive instance newtypeTimeZoneIdType :: Newtype TimeZoneIdType _


newtype TimestampType = TimestampType Number
derive instance newtypeTimestampType :: Newtype TimestampType _


-- | <p>The limit has been reached on the number of labels for the specified resource.</p>
newtype TooManyLabelsException = TooManyLabelsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeTooManyLabelsException :: Newtype TooManyLabelsException _


-- | <p>You've reached the limit on the number of subscriptions for the WorkDocs instance.</p>
newtype TooManySubscriptionsException = TooManySubscriptionsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeTooManySubscriptionsException :: Newtype TooManySubscriptionsException _


-- | <p>The operation is not permitted.</p>
newtype UnauthorizedOperationException = UnauthorizedOperationException 
  { 
  }
derive instance newtypeUnauthorizedOperationException :: Newtype UnauthorizedOperationException _


-- | <p>The caller does not have access to perform the action on the resource.</p>
newtype UnauthorizedResourceAccessException = UnauthorizedResourceAccessException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }
derive instance newtypeUnauthorizedResourceAccessException :: Newtype UnauthorizedResourceAccessException _


newtype UpdateDocumentRequest = UpdateDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: NullOrUndefined (ResourceIdType)
  , "ResourceState" :: NullOrUndefined (ResourceStateType)
  }
derive instance newtypeUpdateDocumentRequest :: Newtype UpdateDocumentRequest _


newtype UpdateDocumentVersionRequest = UpdateDocumentVersionRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "VersionStatus" :: NullOrUndefined (DocumentVersionStatus)
  }
derive instance newtypeUpdateDocumentVersionRequest :: Newtype UpdateDocumentVersionRequest _


newtype UpdateFolderRequest = UpdateFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: NullOrUndefined (ResourceIdType)
  , "ResourceState" :: NullOrUndefined (ResourceStateType)
  }
derive instance newtypeUpdateFolderRequest :: Newtype UpdateFolderRequest _


newtype UpdateUserRequest = UpdateUserRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "UserId" :: (IdType)
  , "GivenName" :: NullOrUndefined (UserAttributeValueType)
  , "Surname" :: NullOrUndefined (UserAttributeValueType)
  , "Type" :: NullOrUndefined (UserType)
  , "StorageRule" :: NullOrUndefined (StorageRuleType)
  , "TimeZoneId" :: NullOrUndefined (TimeZoneIdType)
  , "Locale" :: NullOrUndefined (LocaleType)
  , "GrantPoweruserPrivileges" :: NullOrUndefined (BooleanEnumType)
  }
derive instance newtypeUpdateUserRequest :: Newtype UpdateUserRequest _


newtype UpdateUserResponse = UpdateUserResponse 
  { "User" :: NullOrUndefined (User)
  }
derive instance newtypeUpdateUserResponse :: Newtype UpdateUserResponse _


-- | <p>Describes the upload.</p>
newtype UploadMetadata = UploadMetadata 
  { "UploadUrl" :: NullOrUndefined (UrlType)
  , "SignedHeaders" :: NullOrUndefined (SignedHeaderMap)
  }
derive instance newtypeUploadMetadata :: Newtype UploadMetadata _


newtype UrlType = UrlType String
derive instance newtypeUrlType :: Newtype UrlType _


-- | <p>Describes a user.</p>
newtype User = User 
  { "Id" :: NullOrUndefined (IdType)
  , "Username" :: NullOrUndefined (UsernameType)
  , "EmailAddress" :: NullOrUndefined (EmailAddressType)
  , "GivenName" :: NullOrUndefined (UserAttributeValueType)
  , "Surname" :: NullOrUndefined (UserAttributeValueType)
  , "OrganizationId" :: NullOrUndefined (IdType)
  , "RootFolderId" :: NullOrUndefined (ResourceIdType)
  , "RecycleBinFolderId" :: NullOrUndefined (ResourceIdType)
  , "Status" :: NullOrUndefined (UserStatusType)
  , "Type" :: NullOrUndefined (UserType)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "ModifiedTimestamp" :: NullOrUndefined (TimestampType)
  , "TimeZoneId" :: NullOrUndefined (TimeZoneIdType)
  , "Locale" :: NullOrUndefined (LocaleType)
  , "Storage" :: NullOrUndefined (UserStorageMetadata)
  }
derive instance newtypeUser :: Newtype User _


newtype UserActivities = UserActivities (Array Activity)
derive instance newtypeUserActivities :: Newtype UserActivities _


newtype UserAttributeValueType = UserAttributeValueType String
derive instance newtypeUserAttributeValueType :: Newtype UserAttributeValueType _


newtype UserFilterType = UserFilterType String
derive instance newtypeUserFilterType :: Newtype UserFilterType _


newtype UserIdsType = UserIdsType String
derive instance newtypeUserIdsType :: Newtype UserIdsType _


-- | <p>Describes the metadata of the user.</p>
newtype UserMetadata = UserMetadata 
  { "Id" :: NullOrUndefined (IdType)
  , "Username" :: NullOrUndefined (UsernameType)
  , "GivenName" :: NullOrUndefined (UserAttributeValueType)
  , "Surname" :: NullOrUndefined (UserAttributeValueType)
  , "EmailAddress" :: NullOrUndefined (EmailAddressType)
  }
derive instance newtypeUserMetadata :: Newtype UserMetadata _


newtype UserMetadataList = UserMetadataList (Array UserMetadata)
derive instance newtypeUserMetadataList :: Newtype UserMetadataList _


newtype UserSortType = UserSortType String
derive instance newtypeUserSortType :: Newtype UserSortType _


newtype UserStatusType = UserStatusType String
derive instance newtypeUserStatusType :: Newtype UserStatusType _


-- | <p>Describes the storage for a user.</p>
newtype UserStorageMetadata = UserStorageMetadata 
  { "StorageUtilizedInBytes" :: NullOrUndefined (SizeType)
  , "StorageRule" :: NullOrUndefined (StorageRuleType)
  }
derive instance newtypeUserStorageMetadata :: Newtype UserStorageMetadata _


newtype UserType = UserType String
derive instance newtypeUserType :: Newtype UserType _


newtype UsernameType = UsernameType String
derive instance newtypeUsernameType :: Newtype UsernameType _
