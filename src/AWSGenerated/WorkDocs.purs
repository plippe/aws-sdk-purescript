

-- | <p>The WorkDocs API is designed for the following use cases:</p> <ul> <li> <p>File Migration: File migration applications are supported for users who want to migrate their files from an on-premises or off-premises file system or service. Users can insert files into a user directory structure, as well as allow for basic metadata changes, such as modifications to the permissions of files.</p> </li> <li> <p>Security: Support security applications are supported for users who have additional security needs, such as antivirus or data loss prevention. The API actions, along with AWS CloudTrail, allow these applications to detect when changes occur in Amazon WorkDocs. Then, the application can take the necessary actions and replace the target file. If the target file violates the policy, the application can also choose to email the user.</p> </li> <li> <p>eDiscovery/Analytics: General administrative applications are supported, such as eDiscovery and analytics. These applications can choose to mimic or record the actions in an Amazon WorkDocs site, along with AWS CloudTrail, to replicate data for eDiscovery, backup, or analytical applications.</p> </li> </ul> <p>All Amazon WorkDocs API actions are Amazon authenticated and certificate-signed. They not only require the use of the AWS SDK, but also allow for the exclusive use of IAM users and roles to help facilitate access, trust, and permission policies. By creating a role and allowing an IAM user to access the Amazon WorkDocs site, the IAM user gains full administrative visibility into the entire Amazon WorkDocs site (or as set in the IAM policy). This includes, but is not limited to, the ability to modify file permissions and upload any file to any user. This allows developers to perform the three use cases above, as well as give users the ability to grant access on a selective basis using the IAM model.</p>
module AWS.WorkDocs where

import Control.Monad.Aff (Aff)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Map (Map)
import Data.Unit (Unit, unit)

import AWS.Request as AWS

serviceName = "WorkDocs" :: String


-- | <p>Aborts the upload of the specified document version that was previously initiated by <a>InitiateDocumentVersionUpload</a>. The client should make this call only when it no longer intends to upload the document version, or fails to do so.</p>
abortDocumentVersionUpload :: forall eff. AbortDocumentVersionUploadRequest -> Aff (err :: AWS.RequestError | eff) Unit
abortDocumentVersionUpload = AWS.request serviceName "AbortDocumentVersionUpload" 


-- | <p>Activates the specified user. Only active users can access Amazon WorkDocs.</p>
activateUser :: forall eff. ActivateUserRequest -> Aff (err :: AWS.RequestError | eff) ActivateUserResponse
activateUser = AWS.request serviceName "ActivateUser" 


-- | <p>Creates a set of permissions for the specified folder or document. The resource permissions are overwritten if the principals already have different permissions.</p>
addResourcePermissions :: forall eff. AddResourcePermissionsRequest -> Aff (err :: AWS.RequestError | eff) AddResourcePermissionsResponse
addResourcePermissions = AWS.request serviceName "AddResourcePermissions" 


-- | <p>Adds a new comment to the specified document version.</p>
createComment :: forall eff. CreateCommentRequest -> Aff (err :: AWS.RequestError | eff) CreateCommentResponse
createComment = AWS.request serviceName "CreateComment" 


-- | <p>Adds one or more custom properties to the specified resource (a folder, document, or version).</p>
createCustomMetadata :: forall eff. CreateCustomMetadataRequest -> Aff (err :: AWS.RequestError | eff) CreateCustomMetadataResponse
createCustomMetadata = AWS.request serviceName "CreateCustomMetadata" 


-- | <p>Creates a folder with the specified name and parent folder.</p>
createFolder :: forall eff. CreateFolderRequest -> Aff (err :: AWS.RequestError | eff) CreateFolderResponse
createFolder = AWS.request serviceName "CreateFolder" 


-- | <p>Adds the specified list of labels to the given resource (a document or folder)</p>
createLabels :: forall eff. CreateLabelsRequest -> Aff (err :: AWS.RequestError | eff) CreateLabelsResponse
createLabels = AWS.request serviceName "CreateLabels" 


-- | <p>Configure WorkDocs to use Amazon SNS notifications.</p> <p>The endpoint receives a confirmation message, and must confirm the subscription. For more information, see <a href="http://docs.aws.amazon.com/sns/latest/dg/SendMessageToHttp.html#SendMessageToHttp.confirm">Confirm the Subscription</a> in the <i>Amazon Simple Notification Service Developer Guide</i>.</p>
createNotificationSubscription :: forall eff. CreateNotificationSubscriptionRequest -> Aff (err :: AWS.RequestError | eff) CreateNotificationSubscriptionResponse
createNotificationSubscription = AWS.request serviceName "CreateNotificationSubscription" 


-- | <p>Creates a user in a Simple AD or Microsoft AD directory. The status of a newly created user is "ACTIVE". New users can access Amazon WorkDocs.</p>
createUser :: forall eff. CreateUserRequest -> Aff (err :: AWS.RequestError | eff) CreateUserResponse
createUser = AWS.request serviceName "CreateUser" 


-- | <p>Deactivates the specified user, which revokes the user's access to Amazon WorkDocs.</p>
deactivateUser :: forall eff. DeactivateUserRequest -> Aff (err :: AWS.RequestError | eff) Unit
deactivateUser = AWS.request serviceName "DeactivateUser" 


-- | <p>Deletes the specified comment from the document version.</p>
deleteComment :: forall eff. DeleteCommentRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteComment = AWS.request serviceName "DeleteComment" 


-- | <p>Deletes custom metadata from the specified resource.</p>
deleteCustomMetadata :: forall eff. DeleteCustomMetadataRequest -> Aff (err :: AWS.RequestError | eff) DeleteCustomMetadataResponse
deleteCustomMetadata = AWS.request serviceName "DeleteCustomMetadata" 


-- | <p>Permanently deletes the specified document and its associated metadata.</p>
deleteDocument :: forall eff. DeleteDocumentRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteDocument = AWS.request serviceName "DeleteDocument" 


-- | <p>Permanently deletes the specified folder and its contents.</p>
deleteFolder :: forall eff. DeleteFolderRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteFolder = AWS.request serviceName "DeleteFolder" 


-- | <p>Deletes the contents of the specified folder.</p>
deleteFolderContents :: forall eff. DeleteFolderContentsRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteFolderContents = AWS.request serviceName "DeleteFolderContents" 


-- | <p>Deletes the specified list of labels from a resource.</p>
deleteLabels :: forall eff. DeleteLabelsRequest -> Aff (err :: AWS.RequestError | eff) DeleteLabelsResponse
deleteLabels = AWS.request serviceName "DeleteLabels" 


-- | <p>Deletes the specified subscription from the specified organization.</p>
deleteNotificationSubscription :: forall eff. DeleteNotificationSubscriptionRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteNotificationSubscription = AWS.request serviceName "DeleteNotificationSubscription" 


-- | <p>Deletes the specified user from a Simple AD or Microsoft AD directory.</p>
deleteUser :: forall eff. DeleteUserRequest -> Aff (err :: AWS.RequestError | eff) Unit
deleteUser = AWS.request serviceName "DeleteUser" 


-- | <p>Describes the user activities in a specified time period.</p>
describeActivities :: forall eff. DescribeActivitiesRequest -> Aff (err :: AWS.RequestError | eff) DescribeActivitiesResponse
describeActivities = AWS.request serviceName "DescribeActivities" 


-- | <p>List all the comments for the specified document version.</p>
describeComments :: forall eff. DescribeCommentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeCommentsResponse
describeComments = AWS.request serviceName "DescribeComments" 


-- | <p>Retrieves the document versions for the specified document.</p> <p>By default, only active versions are returned.</p>
describeDocumentVersions :: forall eff. DescribeDocumentVersionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeDocumentVersionsResponse
describeDocumentVersions = AWS.request serviceName "DescribeDocumentVersions" 


-- | <p>Describes the contents of the specified folder, including its documents and subfolders.</p> <p>By default, Amazon WorkDocs returns the first 100 active document and folder metadata items. If there are more results, the response includes a marker that you can use to request the next set of results. You can also request initialized documents.</p>
describeFolderContents :: forall eff. DescribeFolderContentsRequest -> Aff (err :: AWS.RequestError | eff) DescribeFolderContentsResponse
describeFolderContents = AWS.request serviceName "DescribeFolderContents" 


-- | <p>Describes the groups specified by query.</p>
describeGroups :: forall eff. DescribeGroupsRequest -> Aff (err :: AWS.RequestError | eff) DescribeGroupsResponse
describeGroups = AWS.request serviceName "DescribeGroups" 


-- | <p>Lists the specified notification subscriptions.</p>
describeNotificationSubscriptions :: forall eff. DescribeNotificationSubscriptionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeNotificationSubscriptionsResponse
describeNotificationSubscriptions = AWS.request serviceName "DescribeNotificationSubscriptions" 


-- | <p>Describes the permissions of a specified resource.</p>
describeResourcePermissions :: forall eff. DescribeResourcePermissionsRequest -> Aff (err :: AWS.RequestError | eff) DescribeResourcePermissionsResponse
describeResourcePermissions = AWS.request serviceName "DescribeResourcePermissions" 


-- | <p>Describes the current user's special folders; the <code>RootFolder</code> and the <code>RecycleBin</code>. <code>RootFolder</code> is the root of user's files and folders and <code>RecycleBin</code> is the root of recycled items. This is not a valid action for SigV4 (administrative API) clients.</p>
describeRootFolders :: forall eff. DescribeRootFoldersRequest -> Aff (err :: AWS.RequestError | eff) DescribeRootFoldersResponse
describeRootFolders = AWS.request serviceName "DescribeRootFolders" 


-- | <p>Describes the specified users. You can describe all users or filter the results (for example, by status or organization).</p> <p>By default, Amazon WorkDocs returns the first 24 active or pending users. If there are more results, the response includes a marker that you can use to request the next set of results.</p>
describeUsers :: forall eff. DescribeUsersRequest -> Aff (err :: AWS.RequestError | eff) DescribeUsersResponse
describeUsers = AWS.request serviceName "DescribeUsers" 


-- | <p>Retrieves details of the current user for whom the authentication token was generated. This is not a valid action for SigV4 (administrative API) clients.</p>
getCurrentUser :: forall eff. GetCurrentUserRequest -> Aff (err :: AWS.RequestError | eff) GetCurrentUserResponse
getCurrentUser = AWS.request serviceName "GetCurrentUser" 


-- | <p>Retrieves details of a document.</p>
getDocument :: forall eff. GetDocumentRequest -> Aff (err :: AWS.RequestError | eff) GetDocumentResponse
getDocument = AWS.request serviceName "GetDocument" 


-- | <p>Retrieves the path information (the hierarchy from the root folder) for the requested document.</p> <p>By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested document and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the names of the parent folders.</p>
getDocumentPath :: forall eff. GetDocumentPathRequest -> Aff (err :: AWS.RequestError | eff) GetDocumentPathResponse
getDocumentPath = AWS.request serviceName "GetDocumentPath" 


-- | <p>Retrieves version metadata for the specified document.</p>
getDocumentVersion :: forall eff. GetDocumentVersionRequest -> Aff (err :: AWS.RequestError | eff) GetDocumentVersionResponse
getDocumentVersion = AWS.request serviceName "GetDocumentVersion" 


-- | <p>Retrieves the metadata of the specified folder.</p>
getFolder :: forall eff. GetFolderRequest -> Aff (err :: AWS.RequestError | eff) GetFolderResponse
getFolder = AWS.request serviceName "GetFolder" 


-- | <p>Retrieves the path information (the hierarchy from the root folder) for the specified folder.</p> <p>By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested folder and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the parent folder names.</p>
getFolderPath :: forall eff. GetFolderPathRequest -> Aff (err :: AWS.RequestError | eff) GetFolderPathResponse
getFolderPath = AWS.request serviceName "GetFolderPath" 


-- | <p>Creates a new document object and version object.</p> <p>The client specifies the parent folder ID and name of the document to upload. The ID is optionally specified when creating a new version of an existing document. This is the first step to upload a document. Next, upload the document to the URL returned from the call, and then call <a>UpdateDocumentVersion</a>.</p> <p>To cancel the document upload, call <a>AbortDocumentVersionUpload</a>.</p>
initiateDocumentVersionUpload :: forall eff. InitiateDocumentVersionUploadRequest -> Aff (err :: AWS.RequestError | eff) InitiateDocumentVersionUploadResponse
initiateDocumentVersionUpload = AWS.request serviceName "InitiateDocumentVersionUpload" 


-- | <p>Removes all the permissions from the specified resource.</p>
removeAllResourcePermissions :: forall eff. RemoveAllResourcePermissionsRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeAllResourcePermissions = AWS.request serviceName "RemoveAllResourcePermissions" 


-- | <p>Removes the permission for the specified principal from the specified resource.</p>
removeResourcePermission :: forall eff. RemoveResourcePermissionRequest -> Aff (err :: AWS.RequestError | eff) Unit
removeResourcePermission = AWS.request serviceName "RemoveResourcePermission" 


-- | <p>Updates the specified attributes of a document. The user must have access to both the document and its parent folder, if applicable.</p>
updateDocument :: forall eff. UpdateDocumentRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateDocument = AWS.request serviceName "UpdateDocument" 


-- | <p>Changes the status of the document version to ACTIVE. </p> <p>Amazon WorkDocs also sets its document container to ACTIVE. This is the last step in a document upload, after the client uploads the document to an S3-presigned URL returned by <a>InitiateDocumentVersionUpload</a>. </p>
updateDocumentVersion :: forall eff. UpdateDocumentVersionRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateDocumentVersion = AWS.request serviceName "UpdateDocumentVersion" 


-- | <p>Updates the specified attributes of the specified folder. The user must have access to both the folder and its parent folder, if applicable.</p>
updateFolder :: forall eff. UpdateFolderRequest -> Aff (err :: AWS.RequestError | eff) Unit
updateFolder = AWS.request serviceName "UpdateFolder" 


-- | <p>Updates the specified attributes of the specified user, and grants or revokes administrative privileges to the Amazon WorkDocs site.</p>
updateUser :: forall eff. UpdateUserRequest -> Aff (err :: AWS.RequestError | eff) UpdateUserResponse
updateUser = AWS.request serviceName "UpdateUser" 


newtype AbortDocumentVersionUploadRequest = AbortDocumentVersionUploadRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  }


newtype ActivateUserRequest = ActivateUserRequest 
  { "UserId" :: (IdType)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  }


newtype ActivateUserResponse = ActivateUserResponse 
  { "User" :: NullOrUndefined (User)
  }


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


newtype ActivityType = ActivityType String


newtype AddResourcePermissionsRequest = AddResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "Principals" :: (SharePrincipalList)
  , "NotificationOptions" :: NullOrUndefined (NotificationOptions)
  }


newtype AddResourcePermissionsResponse = AddResourcePermissionsResponse 
  { "ShareResults" :: NullOrUndefined (ShareResultsList)
  }


newtype AuthenticationHeaderType = AuthenticationHeaderType String


newtype BooleanEnumType = BooleanEnumType String


newtype BooleanType = BooleanType Boolean


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


newtype CommentIdType = CommentIdType String


newtype CommentList = CommentList (Array Comment)


-- | <p>Describes the metadata of a comment.</p>
newtype CommentMetadata = CommentMetadata 
  { "CommentId" :: NullOrUndefined (CommentIdType)
  , "Contributor" :: NullOrUndefined (User)
  , "CreatedTimestamp" :: NullOrUndefined (TimestampType)
  , "CommentStatus" :: NullOrUndefined (CommentStatusType)
  , "RecipientId" :: NullOrUndefined (IdType)
  }


newtype CommentStatusType = CommentStatusType String


newtype CommentTextType = CommentTextType String


newtype CommentVisibilityType = CommentVisibilityType String


-- | <p>The resource hierarchy is changing.</p>
newtype ConcurrentModificationException = ConcurrentModificationException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


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


newtype CreateCommentResponse = CreateCommentResponse 
  { "Comment" :: NullOrUndefined (Comment)
  }


newtype CreateCustomMetadataRequest = CreateCustomMetadataRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "VersionId" :: NullOrUndefined (DocumentVersionIdType)
  , "CustomMetadata" :: (CustomMetadataMap)
  }


newtype CreateCustomMetadataResponse = CreateCustomMetadataResponse 
  { 
  }


newtype CreateFolderRequest = CreateFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: (ResourceIdType)
  }


newtype CreateFolderResponse = CreateFolderResponse 
  { "Metadata" :: NullOrUndefined (FolderMetadata)
  }


newtype CreateLabelsRequest = CreateLabelsRequest 
  { "ResourceId" :: (ResourceIdType)
  , "Labels" :: (SharedLabels)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  }


newtype CreateLabelsResponse = CreateLabelsResponse 
  { 
  }


newtype CreateNotificationSubscriptionRequest = CreateNotificationSubscriptionRequest 
  { "OrganizationId" :: (IdType)
  , "Endpoint" :: (SubscriptionEndPointType)
  , "Protocol" :: (SubscriptionProtocolType)
  , "SubscriptionType" :: (SubscriptionType)
  }


newtype CreateNotificationSubscriptionResponse = CreateNotificationSubscriptionResponse 
  { "Subscription" :: NullOrUndefined (Subscription)
  }


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


newtype CreateUserResponse = CreateUserResponse 
  { "User" :: NullOrUndefined (User)
  }


newtype CustomMetadataKeyList = CustomMetadataKeyList (Array CustomMetadataKeyType)


newtype CustomMetadataKeyType = CustomMetadataKeyType String


-- | <p>The limit has been reached on the number of custom properties for the specified resource.</p>
newtype CustomMetadataLimitExceededException = CustomMetadataLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


newtype CustomMetadataMap = CustomMetadataMap (Map CustomMetadataKeyType CustomMetadataValueType)


newtype CustomMetadataValueType = CustomMetadataValueType String


newtype DeactivateUserRequest = DeactivateUserRequest 
  { "UserId" :: (IdType)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  }


-- | <p>The last user in the organization is being deactivated.</p>
newtype DeactivatingLastSystemUserException = DeactivatingLastSystemUserException 
  { 
  }


newtype DeleteCommentRequest = DeleteCommentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "CommentId" :: (CommentIdType)
  }


newtype DeleteCustomMetadataRequest = DeleteCustomMetadataRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "VersionId" :: NullOrUndefined (DocumentVersionIdType)
  , "Keys" :: NullOrUndefined (CustomMetadataKeyList)
  , "DeleteAll" :: NullOrUndefined (BooleanType)
  }


newtype DeleteCustomMetadataResponse = DeleteCustomMetadataResponse 
  { 
  }


newtype DeleteDocumentRequest = DeleteDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  }


newtype DeleteFolderContentsRequest = DeleteFolderContentsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  }


newtype DeleteFolderRequest = DeleteFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  }


newtype DeleteLabelsRequest = DeleteLabelsRequest 
  { "ResourceId" :: (ResourceIdType)
  , "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "Labels" :: NullOrUndefined (SharedLabels)
  , "DeleteAll" :: NullOrUndefined (BooleanType)
  }


newtype DeleteLabelsResponse = DeleteLabelsResponse 
  { 
  }


newtype DeleteNotificationSubscriptionRequest = DeleteNotificationSubscriptionRequest 
  { "SubscriptionId" :: (IdType)
  , "OrganizationId" :: (IdType)
  }


newtype DeleteUserRequest = DeleteUserRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "UserId" :: (IdType)
  }


newtype DescribeActivitiesRequest = DescribeActivitiesRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "StartTime" :: NullOrUndefined (TimestampType)
  , "EndTime" :: NullOrUndefined (TimestampType)
  , "OrganizationId" :: NullOrUndefined (IdType)
  , "UserId" :: NullOrUndefined (IdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (MarkerType)
  }


newtype DescribeActivitiesResponse = DescribeActivitiesResponse 
  { "UserActivities" :: NullOrUndefined (UserActivities)
  , "Marker" :: NullOrUndefined (MarkerType)
  }


newtype DescribeCommentsRequest = DescribeCommentsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (MarkerType)
  }


newtype DescribeCommentsResponse = DescribeCommentsResponse 
  { "Comments" :: NullOrUndefined (CommentList)
  , "Marker" :: NullOrUndefined (MarkerType)
  }


newtype DescribeDocumentVersionsRequest = DescribeDocumentVersionsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Include" :: NullOrUndefined (FieldNamesType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  }


newtype DescribeDocumentVersionsResponse = DescribeDocumentVersionsResponse 
  { "DocumentVersions" :: NullOrUndefined (DocumentVersionMetadataList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


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


newtype DescribeFolderContentsResponse = DescribeFolderContentsResponse 
  { "Folders" :: NullOrUndefined (FolderMetadataList)
  , "Documents" :: NullOrUndefined (DocumentMetadataList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


newtype DescribeGroupsRequest = DescribeGroupsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "SearchQuery" :: (SearchQueryType)
  , "OrganizationId" :: NullOrUndefined (IdType)
  , "Marker" :: NullOrUndefined (MarkerType)
  , "Limit" :: NullOrUndefined (PositiveIntegerType)
  }


newtype DescribeGroupsResponse = DescribeGroupsResponse 
  { "Groups" :: NullOrUndefined (GroupMetadataList)
  , "Marker" :: NullOrUndefined (MarkerType)
  }


newtype DescribeNotificationSubscriptionsRequest = DescribeNotificationSubscriptionsRequest 
  { "OrganizationId" :: (IdType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  , "Limit" :: NullOrUndefined (LimitType)
  }


newtype DescribeNotificationSubscriptionsResponse = DescribeNotificationSubscriptionsResponse 
  { "Subscriptions" :: NullOrUndefined (SubscriptionList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


newtype DescribeResourcePermissionsRequest = DescribeResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "PrincipalId" :: NullOrUndefined (IdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


newtype DescribeResourcePermissionsResponse = DescribeResourcePermissionsResponse 
  { "Principals" :: NullOrUndefined (PrincipalList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


newtype DescribeRootFoldersRequest = DescribeRootFoldersRequest 
  { "AuthenticationToken" :: (AuthenticationHeaderType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


newtype DescribeRootFoldersResponse = DescribeRootFoldersResponse 
  { "Folders" :: NullOrUndefined (FolderMetadataList)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


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


newtype DescribeUsersResponse = DescribeUsersResponse 
  { "Users" :: NullOrUndefined (OrganizationUserList)
  , "TotalNumberOfUsers" :: NullOrUndefined (SizeType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


newtype DocumentContentType = DocumentContentType String


-- | <p>This exception is thrown when the document is locked for comments and user tries to create or delete a comment on that document.</p>
newtype DocumentLockedForCommentsException = DocumentLockedForCommentsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


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


newtype DocumentMetadataList = DocumentMetadataList (Array DocumentMetadata)


newtype DocumentSourceType = DocumentSourceType String


newtype DocumentSourceUrlMap = DocumentSourceUrlMap (Map DocumentSourceType UrlType)


newtype DocumentStatusType = DocumentStatusType String


newtype DocumentThumbnailType = DocumentThumbnailType String


newtype DocumentThumbnailUrlMap = DocumentThumbnailUrlMap (Map DocumentThumbnailType UrlType)


newtype DocumentVersionIdType = DocumentVersionIdType String


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


newtype DocumentVersionMetadataList = DocumentVersionMetadataList (Array DocumentVersionMetadata)


newtype DocumentVersionStatus = DocumentVersionStatus String


-- | <p>This exception is thrown when a valid checkout ID is not presented on document version upload calls for a document that has been checked out from Web client.</p>
newtype DraftUploadOutOfSyncException = DraftUploadOutOfSyncException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


newtype EmailAddressType = EmailAddressType String


-- | <p>The resource already exists.</p>
newtype EntityAlreadyExistsException = EntityAlreadyExistsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


newtype EntityIdList = EntityIdList (Array IdType)


-- | <p>The resource does not exist.</p>
newtype EntityNotExistsException = EntityNotExistsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  , "EntityIds" :: NullOrUndefined (EntityIdList)
  }


newtype ErrorMessageType = ErrorMessageType String


-- | <p>The AWS Directory Service cannot reach an on-premises instance. Or a dependency under the control of the organization is failing, such as a connected Active Directory.</p>
newtype FailedDependencyException = FailedDependencyException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


newtype FieldNamesType = FieldNamesType String


newtype FolderContentType = FolderContentType String


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


newtype FolderMetadataList = FolderMetadataList (Array FolderMetadata)


newtype GetCurrentUserRequest = GetCurrentUserRequest 
  { "AuthenticationToken" :: (AuthenticationHeaderType)
  }


newtype GetCurrentUserResponse = GetCurrentUserResponse 
  { "User" :: NullOrUndefined (User)
  }


newtype GetDocumentPathRequest = GetDocumentPathRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (IdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


newtype GetDocumentPathResponse = GetDocumentPathResponse 
  { "Path" :: NullOrUndefined (ResourcePath)
  }


newtype GetDocumentRequest = GetDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "IncludeCustomMetadata" :: NullOrUndefined (BooleanType)
  }


newtype GetDocumentResponse = GetDocumentResponse 
  { "Metadata" :: NullOrUndefined (DocumentMetadata)
  , "CustomMetadata" :: NullOrUndefined (CustomMetadataMap)
  }


newtype GetDocumentVersionRequest = GetDocumentVersionRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  , "IncludeCustomMetadata" :: NullOrUndefined (BooleanType)
  }


newtype GetDocumentVersionResponse = GetDocumentVersionResponse 
  { "Metadata" :: NullOrUndefined (DocumentVersionMetadata)
  , "CustomMetadata" :: NullOrUndefined (CustomMetadataMap)
  }


newtype GetFolderPathRequest = GetFolderPathRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (IdType)
  , "Limit" :: NullOrUndefined (LimitType)
  , "Fields" :: NullOrUndefined (FieldNamesType)
  , "Marker" :: NullOrUndefined (PageMarkerType)
  }


newtype GetFolderPathResponse = GetFolderPathResponse 
  { "Path" :: NullOrUndefined (ResourcePath)
  }


newtype GetFolderRequest = GetFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  , "IncludeCustomMetadata" :: NullOrUndefined (BooleanType)
  }


newtype GetFolderResponse = GetFolderResponse 
  { "Metadata" :: NullOrUndefined (FolderMetadata)
  , "CustomMetadata" :: NullOrUndefined (CustomMetadataMap)
  }


-- | <p>Describes the metadata of a user group.</p>
newtype GroupMetadata = GroupMetadata 
  { "Id" :: NullOrUndefined (IdType)
  , "Name" :: NullOrUndefined (GroupNameType)
  }


newtype GroupMetadataList = GroupMetadataList (Array GroupMetadata)


newtype GroupNameType = GroupNameType String


newtype HashType = HashType String


newtype HeaderNameType = HeaderNameType String


newtype HeaderValueType = HeaderValueType String


newtype IdType = IdType String


-- | <p>The user is undergoing transfer of ownership.</p>
newtype IllegalUserStateException = IllegalUserStateException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


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


newtype InitiateDocumentVersionUploadResponse = InitiateDocumentVersionUploadResponse 
  { "Metadata" :: NullOrUndefined (DocumentMetadata)
  , "UploadMetadata" :: NullOrUndefined (UploadMetadata)
  }


-- | <p>The pagination marker or limit fields are not valid.</p>
newtype InvalidArgumentException = InvalidArgumentException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


-- | <p>The operation is invalid.</p>
newtype InvalidOperationException = InvalidOperationException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


-- | <p>The password is invalid.</p>
newtype InvalidPasswordException = InvalidPasswordException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


-- | <p>The maximum of 100,000 folders under the parent folder has been exceeded.</p>
newtype LimitExceededException = LimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


newtype LimitType = LimitType Int


newtype LocaleType = LocaleType String


newtype MarkerType = MarkerType String


newtype MessageType = MessageType String


-- | <p>Set of options which defines notification preferences of given action.</p>
newtype NotificationOptions = NotificationOptions 
  { "SendEmail" :: NullOrUndefined (BooleanType)
  , "EmailMessage" :: NullOrUndefined (MessageType)
  }


newtype OrderType = OrderType String


newtype OrganizationUserList = OrganizationUserList (Array User)


newtype PageMarkerType = PageMarkerType String


-- | <p>Describes the users or user groups.</p>
newtype Participants = Participants 
  { "Users" :: NullOrUndefined (UserMetadataList)
  , "Groups" :: NullOrUndefined (GroupMetadataList)
  }


newtype PasswordType = PasswordType String


-- | <p>Describes the permissions.</p>
newtype PermissionInfo = PermissionInfo 
  { "Role" :: NullOrUndefined (RoleType)
  , "Type" :: NullOrUndefined (RolePermissionType)
  }


newtype PermissionInfoList = PermissionInfoList (Array PermissionInfo)


newtype PositiveIntegerType = PositiveIntegerType Int


newtype PositiveSizeType = PositiveSizeType Number


-- | <p>Describes a resource.</p>
newtype Principal = Principal 
  { "Id" :: NullOrUndefined (IdType)
  , "Type" :: NullOrUndefined (PrincipalType)
  , "Roles" :: NullOrUndefined (PermissionInfoList)
  }


newtype PrincipalList = PrincipalList (Array Principal)


newtype PrincipalType = PrincipalType String


-- | <p>The specified document version is not in the INITIALIZED state.</p>
newtype ProhibitedStateException = ProhibitedStateException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


newtype RemoveAllResourcePermissionsRequest = RemoveAllResourcePermissionsRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  }


newtype RemoveResourcePermissionRequest = RemoveResourcePermissionRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "ResourceId" :: (ResourceIdType)
  , "PrincipalId" :: (IdType)
  , "PrincipalType" :: NullOrUndefined (PrincipalType)
  }


-- | <p>The resource is already checked out.</p>
newtype ResourceAlreadyCheckedOutException = ResourceAlreadyCheckedOutException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


newtype ResourceIdType = ResourceIdType String


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


newtype ResourceNameType = ResourceNameType String


-- | <p>Describes the path information of a resource.</p>
newtype ResourcePath = ResourcePath 
  { "Components" :: NullOrUndefined (ResourcePathComponentList)
  }


-- | <p>Describes the resource path.</p>
newtype ResourcePathComponent = ResourcePathComponent 
  { "Id" :: NullOrUndefined (IdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  }


newtype ResourcePathComponentList = ResourcePathComponentList (Array ResourcePathComponent)


newtype ResourceSortType = ResourceSortType String


newtype ResourceStateType = ResourceStateType String


newtype ResourceType = ResourceType String


newtype RolePermissionType = RolePermissionType String


newtype RoleType = RoleType String


newtype SearchQueryType = SearchQueryType String


-- | <p>One or more of the dependencies is unavailable.</p>
newtype ServiceUnavailableException = ServiceUnavailableException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


-- | <p>Describes the recipient type and ID, if available.</p>
newtype SharePrincipal = SharePrincipal 
  { "Id" :: (IdType)
  , "Type" :: (PrincipalType)
  , "Role" :: (RoleType)
  }


newtype SharePrincipalList = SharePrincipalList (Array SharePrincipal)


-- | <p>Describes the share results of a resource.</p>
newtype ShareResult = ShareResult 
  { "PrincipalId" :: NullOrUndefined (IdType)
  , "Role" :: NullOrUndefined (RoleType)
  , "Status" :: NullOrUndefined (ShareStatusType)
  , "ShareId" :: NullOrUndefined (ResourceIdType)
  , "StatusMessage" :: NullOrUndefined (MessageType)
  }


newtype ShareResultsList = ShareResultsList (Array ShareResult)


newtype ShareStatusType = ShareStatusType String


newtype SharedLabel = SharedLabel String


newtype SharedLabels = SharedLabels (Array SharedLabel)


newtype SignedHeaderMap = SignedHeaderMap (Map HeaderNameType HeaderValueType)


newtype SizeType = SizeType Number


-- | <p>The storage limit has been exceeded.</p>
newtype StorageLimitExceededException = StorageLimitExceededException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


-- | <p>The storage limit will be exceeded.</p>
newtype StorageLimitWillExceedException = StorageLimitWillExceedException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


-- | <p>Describes the storage for a user.</p>
newtype StorageRuleType = StorageRuleType 
  { "StorageAllocatedInBytes" :: NullOrUndefined (PositiveSizeType)
  , "StorageType" :: NullOrUndefined (StorageType)
  }


newtype StorageType = StorageType String


-- | <p>Describes a subscription.</p>
newtype Subscription = Subscription 
  { "SubscriptionId" :: NullOrUndefined (IdType)
  , "EndPoint" :: NullOrUndefined (SubscriptionEndPointType)
  , "Protocol" :: NullOrUndefined (SubscriptionProtocolType)
  }


newtype SubscriptionEndPointType = SubscriptionEndPointType String


newtype SubscriptionList = SubscriptionList (Array Subscription)


newtype SubscriptionProtocolType = SubscriptionProtocolType String


newtype SubscriptionType = SubscriptionType String


newtype TimeZoneIdType = TimeZoneIdType String


newtype TimestampType = TimestampType Number


-- | <p>The limit has been reached on the number of labels for the specified resource.</p>
newtype TooManyLabelsException = TooManyLabelsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


-- | <p>You've reached the limit on the number of subscriptions for the WorkDocs instance.</p>
newtype TooManySubscriptionsException = TooManySubscriptionsException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


-- | <p>The operation is not permitted.</p>
newtype UnauthorizedOperationException = UnauthorizedOperationException 
  { 
  }


-- | <p>The caller does not have access to perform the action on the resource.</p>
newtype UnauthorizedResourceAccessException = UnauthorizedResourceAccessException 
  { "Message" :: NullOrUndefined (ErrorMessageType)
  }


newtype UpdateDocumentRequest = UpdateDocumentRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: NullOrUndefined (ResourceIdType)
  , "ResourceState" :: NullOrUndefined (ResourceStateType)
  }


newtype UpdateDocumentVersionRequest = UpdateDocumentVersionRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "DocumentId" :: (ResourceIdType)
  , "VersionId" :: (DocumentVersionIdType)
  , "VersionStatus" :: NullOrUndefined (DocumentVersionStatus)
  }


newtype UpdateFolderRequest = UpdateFolderRequest 
  { "AuthenticationToken" :: NullOrUndefined (AuthenticationHeaderType)
  , "FolderId" :: (ResourceIdType)
  , "Name" :: NullOrUndefined (ResourceNameType)
  , "ParentFolderId" :: NullOrUndefined (ResourceIdType)
  , "ResourceState" :: NullOrUndefined (ResourceStateType)
  }


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


newtype UpdateUserResponse = UpdateUserResponse 
  { "User" :: NullOrUndefined (User)
  }


-- | <p>Describes the upload.</p>
newtype UploadMetadata = UploadMetadata 
  { "UploadUrl" :: NullOrUndefined (UrlType)
  , "SignedHeaders" :: NullOrUndefined (SignedHeaderMap)
  }


newtype UrlType = UrlType String


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


newtype UserActivities = UserActivities (Array Activity)


newtype UserAttributeValueType = UserAttributeValueType String


newtype UserFilterType = UserFilterType String


newtype UserIdsType = UserIdsType String


-- | <p>Describes the metadata of the user.</p>
newtype UserMetadata = UserMetadata 
  { "Id" :: NullOrUndefined (IdType)
  , "Username" :: NullOrUndefined (UsernameType)
  , "GivenName" :: NullOrUndefined (UserAttributeValueType)
  , "Surname" :: NullOrUndefined (UserAttributeValueType)
  , "EmailAddress" :: NullOrUndefined (EmailAddressType)
  }


newtype UserMetadataList = UserMetadataList (Array UserMetadata)


newtype UserSortType = UserSortType String


newtype UserStatusType = UserStatusType String


-- | <p>Describes the storage for a user.</p>
newtype UserStorageMetadata = UserStorageMetadata 
  { "StorageUtilizedInBytes" :: NullOrUndefined (SizeType)
  , "StorageRule" :: NullOrUndefined (StorageRuleType)
  }


newtype UserType = UserType String


newtype UsernameType = UsernameType String
